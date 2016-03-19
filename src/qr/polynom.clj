(ns qr.polynom
	(:require [qr.galua-field :as galua]))

;; Полиномы представляют собой вектора с коэффициентами.
;; Степень для коэффициента получается

(defn multiple-polynom-by-num
	[polynom mult]
	(map #(galua/galua-multiply % mult) polynom))

(defn multiple-polynom-by-x
	[polynom mult base]
	(concat (multiple-polynom-by-num polynom mult) (repeat base 0)))

(defn merge-polynoms
	[p1 p2]
	(if (> (count p2) (count p1))
		(merge-polynoms p2 p1)
		(let [dif (- (count p1) (count p2))
			  alt-p2 (concat (repeat dif 0) p2)]
			  (map #(bit-xor %1 %2) p1 alt-p2))))

(defn multiple-polynoms
	[[head & other] polynom]
	(let [polynom-exponent (count other)]
		(if (not (> polynom-exponent 0))
			(multiple-polynom-by-num polynom head)
			(let [prev-polynom (multiple-polynoms other polynom)
				  prev-polynom-exponent (dec (count prev-polynom))
				  new-polynom (multiple-polynom-by-x polynom head prev-polynom-exponent)]
				(merge-polynoms prev-polynom new-polynom)))))

(defn generator-polynom
	[step]
	(if (not (= step 1))
		(let [prev-step (dec step)]
			(multiple-polynoms (generator-polynom prev-step) [1 (galua/galua-pow 2 prev-step)]))
		[1 1]))

(defn normalize-polynom
	[polynom normalizer]
	(let [f (first polynom)
		  mult (galua/galua-division normalizer f)]
		  (multiple-polynom-by-num polynom mult)))

(defn trim-polynom
	[polynom]
	(drop-while #(= % 0) polynom))
