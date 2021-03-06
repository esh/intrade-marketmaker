(ns marketmaker
	(:require [tradeservice]))

(defn get-open-orders [contract-id]
	(filter
		#(and
			(= contract-id (get @% :contract-id)) 
			(some #{(get @% :state)} ['New 'Open]))
		(vals @tradeservice/*orders*)))

(defn outstanding [side open-orders]
	(apply + (map
		#(get @% :qty)
		(filter #(= side (get @% :side)) open-orders))))

(defn maintain-orders [contract-id qty skew spread sides]
	(let [tob-bid (get (first (get @(tradeservice/get-quote contract-id) :bids)) :price)
			  tob-offer (get (first (get @(tradeservice/get-quote contract-id) :offers)) :price)
				open-orders (get-open-orders contract-id)
				outstanding-bids (outstanding 'Buy open-orders)
			  outstanding-offers (outstanding 'Sell open-orders)]
		(doall (pmap
			#(let [order @%
						 side (get order :side)
						 price (get order :price)]
				(when
					(or (nil? tob-bid)
							(nil? tob-offer)
							(not (some #{side} sides))
							(and
								(= 'Buy side)
								(or
									(> outstanding-bids qty) 
									(not (=
										price
										(min
											(+ tob-bid (- spread) skew)
											(- tob-offer tradeservice/*tick-size*))))))
							(and
								(= 'Sell side)
								(or
									(> outstanding-offers qty)
									(not (=
										price
										(max
											(+ tob-offer spread skew)
											(+ tob-bid tradeservice/*tick-size*)))))))
					(tradeservice/cancel-order %)))
			open-orders)))
	(let [tob-bid (get (first (get @(tradeservice/get-quote contract-id) :bids)) :price)
			  tob-offer (get (first (get @(tradeservice/get-quote contract-id) :offers)) :price)
			  open-orders (get-open-orders contract-id)
			  outstanding-bids (outstanding 'Buy open-orders)
			  outstanding-offers (outstanding 'Sell open-orders)]
		(doall (filter #(not (nil? %)) (list
			(if (and
						(some #{'Buy} sides)
						(< outstanding-bids qty)
						(not (nil? tob-bid))
						(not (nil? tob-offer)))
					(tradeservice/send-order
						:contract-id contract-id
						:side 'Buy
						:price (min (+ tob-bid (- spread) skew) (- tob-offer tradeservice/*tick-size*)) 
						:qty (- qty outstanding-bids)
						:tif 'GFS))
			(if (and
						(some #{'Sell} sides)
						(< outstanding-offers qty)
						(not (nil? tob-bid))
						(not (nil? tob-offer)))
					(tradeservice/send-order
						:contract-id contract-id
						:side 'Sell
						:price (max (+ tob-offer spread skew) (+ tob-bid tradeservice/*tick-size*)) 
						:qty (- qty outstanding-offers)
						:tif 'GFS)))))))
