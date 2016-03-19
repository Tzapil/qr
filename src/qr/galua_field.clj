(ns qr.galua-field
	(:require [qr.helpers :as helpers]))

;; Defines algebra in GF(256)
;; All addition and subtraction performed by XOR


(def natural-seq (iterate inc 0))

(defn- galue-base
	[number]
	(if (> number 255)
		(bit-xor number 285)
		number))

(defn galua-pow
	[base exponent]
	(if (not (= exponent 0))
		(galue-base (* (galua-pow base (dec exponent)) base))
		1))

(defn- galua-pow-seq
	([] (galua-pow-seq 0))
	([exponent] (cons (galua-pow 2 exponent) (lazy-seq (galua-pow-seq (inc exponent))))))

(defn galua-log
	[num exponent]
	(some #(and (= (galua-pow exponent %) num) %) natural-seq))

(defn galua-multiply
	[n1 n2]
	(if (not (or (= n1 0) (= n2 0)))
		(galua-pow 2 (mod (+ (galua-log n1 2) (galua-log n2 2)) 255))
		0))

(defn galua-division
	[n1 n2]
	(some #(and (= (galua-multiply n2 %) n1) %) natural-seq))