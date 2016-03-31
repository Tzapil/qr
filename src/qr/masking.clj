(ns qr.masking
	(:require 
		[qr.matrix :as matrix]
		[qr.helpers :as helpers]))

(def version 1)
(def error-correction-level 3)
(def size (+ (* (- version 1) 4) 21))

(def ^:dynamic *penalty-value*)
(def ^:dynamic *penalty-pattern*)

(def ^:dynamic *mask*)

(defn- floor
	[number]
	(int (Math/floor number)))

(def mask-predicates [
		{ :num 0 :predicate (fn [x y] (= 0 (mod (+ x y) 2))) }						  					  ;;0
		{ :num 1 :predicate (fn [x y] (= 0 (mod x 2))) }												  ;;1
		{ :num 2 :predicate (fn [x y] (= 0 (mod y 3))) }												  ;;2
		{ :num 3 :predicate (fn [x y] (= 0 (mod (+ x y) 3))) }											  ;;3
		{ :num 4 :predicate (fn [x y] (= 0 (mod (+ (floor (/ x 2)) (floor (/ y 3))) 2))) }				  ;;4
		{ :num 5 :predicate (fn [x y] (= 0 (let [mult (* x y)] (+ (mod mult 2) (mod mult 3)))))	}		  ;;5
		{ :num 6 :predicate (fn [x y] (= 0 (let [mult (* x y)] (mod (+ (mod mult 2) (mod mult 3)) 2)))) } ;;6
		{ :num 7 :predicate (fn [x y] (= 0 (mod (+ (mod (+ x y) 2) (mod (* x y) 3)) 2))) }				  ;;7
	])

(def information
		["011010101011111"
		"011000001101000"
		"011111100110001"
		"011101000000110"
		"010010010110100"
		"010000110000011"
		"010111011011010"
		"010101111101101"])

(defn mask-pixel
	[value]
	(mod (inc value) 2))

(defn penalty-pattern
	[pattern penalty-size field]
	(matrix/walk-matrix 
		(fn [result x y]
			(if-let [equal (matrix/walk-matrix 
								(fn [result px py]
									(and result (= (matrix/get-pixel px py pattern) (matrix/get-pixel (+ x px) (+ y py) field)))) 
								true
								pattern)]
					(+ result penalty-size)
					result)) 
		0 
		field))

(defn same-colour-pixels
	[pixel & others]
		(if (not (= (count others) 0))
			(let [result (same-colour-pixels others)
				  prev (first others)]
				  (if (= pixel prev)
				  		(inc result)
				  		1))
			1))

(defn first-penaly-row
	[row]
	(if (>= (count row) 5)
		(let [pixels-line (same-colour-pixels row)
			  result (first-penaly-row (drop pixels-line row))]
			(+ result (if (>= pixels-line 5) (+ 3 (- pixels-line 5)) 0)))
		0))

(defn reverse-field
	[field]
	(let [size (count (first field))]
		(vec (map (fn [column] (vec (map #(% column) field))) (take size (iterate inc 0))))))

(defn first-penalty
	[field]
	(let [row-penalty (reduce #(+ %1 (first-penaly-row %2)) 0 field)
		  columns-penalty (reduce #(+ %1 (first-penaly-row %2)) 0 (reverse-field field))]
		  (+ row-penalty columns-penalty)))

(defn second-penalty
	[field]
	(let [patterns [
		[[0 0] 
		 [0 0]] 
		[[1 1] 
		 [1 1]]]]
		 (reduce #(+ %1 (penalty-pattern %2 3 field)) 0 patterns)))

(defn third-penalty
	[field]
	(let [patterns [ [ [0 0 0 0 1 0 1 1 1 0 1] ] [ [1 0 1 1 1 0 1 0 0 0 0] ] ] 
		  horizontal (reduce #(+ %1 (penalty-pattern %2 40 field)) 0 patterns)
		  reversed (reverse-field field)
		  vertical (reduce #(+ %1 (penalty-pattern %2 40 reversed)) 0 patterns)]
		 (+ horizontal vertical)))

(defn fourth-penalty
	[field]
	(let [size (* size size)
		  black (reduce (fn [accum row] (reduce #(+ %1 %2) accum row)) 0 field)
		  percent (* (/ black size) 100)
		  low-value (- percent (mod percent 5))
		  hi-value (+ low-value 5)
		  abs-lv (/ (Math/abs (- 50 low-value)) 5)
		  abs-hv (/ (Math/abs (- 50 hi-value)) 5)]
		 (* (min abs-lv abs-hv) 10)))

(defn calc-penalty
	[field]
	(let [penalties [first-penalty second-penalty third-penalty]]
		(reduce #(+ %1 (%2 field)) 0 penalties)))

(defn put-mask
	[predicate field]
	(matrix/walk-matrix (fn [result x y] 
		(let [pixel (matrix/get-pixel x y result)]
			(if (and (< pixel 2) (predicate x y))
				(matrix/set-pixel x y (mask-pixel pixel) result)
				result)))
	field field))

(defn mask
	[field]
	(let [penalties (map #(let [masked (put-mask (:predicate %) field)
								clear-field (matrix/remove-reserved masked)
								penalty (calc-penalty clear-field)] {:field clear-field :penalty penalty :mask %}) mask-predicates)
		 						minimum (reduce (fn [minimum current] (if (> (:penalty minimum) (:penalty current)) current minimum)) {:penalty 32000} penalties)
		 						info (nth information (get-in minimum [:mask :num]))]
		(println info)
		(->>
			:field
			minimum
			(matrix/draw-pattern 0 8 [(vec (map #(helpers/byn-to-num (str %)) (reverse (drop 9 info))))])
			(matrix/draw-pattern 8 8 [(vec (map #(helpers/byn-to-num (str %)) (reverse (take 2 (drop 7 info)))))])
			(matrix/draw-pattern 8 0 (vec (map #(conj [] (helpers/byn-to-num (str %))) (take 6 info))))
			(matrix/set-pixel 8 7 (helpers/byn-to-num (str (nth information 6))))
			(matrix/draw-pattern (- size 8) 8 [(vec (map #(helpers/byn-to-num (str %)) (reverse (take 8 info))))])
			(matrix/draw-pattern 8 (- size 7) (vec (map #(conj [] (helpers/byn-to-num (str %))) (drop 8 info))))
			)))