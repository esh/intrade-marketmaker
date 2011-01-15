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

(defn maintain-orders [contract-id bids offers qty skew spread sides]
	(let [open-orders (get-open-orders contract-id)
				outstanding-bids (outstanding 'Buy open-orders)
			  outstanding-offers (outstanding 'Sell open-orders)]
		(doall (pmap
			#(let [order @%
						 side (get order :side)
						 price (get order :price)]
				(when
					(or (= 0 (count bids))
							(= 0 (count offers))
							(not (some #{side} sides))
							(and (= 'Buy side)
								(or
									(> outstanding-bids qty) 
									(not (= (get (first bids) :price)))))
							(and (= 'Sell side)
								(or
									(> outstanding-offers qty)
									(not (= (get (first offers) :price))))))
					(tradeservice/cancel-order %)))
			open-orders))
	))
