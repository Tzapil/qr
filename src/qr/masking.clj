(ns qr.masking
	(:require [qr.matrix :as matrix]))

(def version 1)
(def error-correction-level 3)
(def size (+ (* (- version 1) 4) 21))

(defn- floor
	[number]
	(int (Math/floor number)))

(def mask-predicates [
		(fn [x y] (mod (+ x y) 2))												;;0
		(fn [x y] (mod x 2))													;;1
		(fn [x y] (mod y 3))													;;2
		(fn [x y] (mod (+ x y) 3))												;;3
		(fn [x y] (mod (+ (floor (/ x 2)) (floor (/ y 3))) 2))					;;4
		(fn [x y] (let [mult (* x y)] (+ (mod mult 2) (mod mult 3))))			;;5
		(fn [x y] (let [mult (* x y)] (mod (+ (mod mult 2) (mod mult 3)) 2)))	;;6
		(fn [x y] (mod (+ (mod (+ x y) 2) (mod (* x y) 3)) 2))					;;7
	])

(defn mask-pixel
	[value]
	(mod (inc value) 2))

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
		[(map (fn [column] (map #(% column) field)) (take size (iterate inc 0)))]))

(defn first-penaly
	[field]
	(let [row-penalty (reduce #(+ %1 (first-penaly-row %2)) 0 field)
		  columns-penalty (reduce #(+ %1 (first-penaly-row %2)) 0 (reverse-field field))]
		  (+ row-penalty columns-penalty)))

(defn second-penalty-xy
	[x y field]
	(let [steps [[0 0] [1 0] [0 1] [1 1]]
		  pixels (map #(matrix/get-pixel (+ x (% 0)) (+ y (% 1)) field) steps)]
		(if (apply = pixels)
			3
			0)))

(defn second-penalty-row
	[y field]
	(loop [x 0
		   penalty 0]
		   (if (< x size)
		   		(recur (inc x) (+ penalty (second-penalty-xy x y field)))
		   		penalty)))

(defn second-penalty
	[field]
	(loop [y 0
		   penalty 0]
		   (if (< y size)
		   		(recur (inc y) (+ penalty (second-penalty-row y field)))
		   		penalty)))

(defn penalty-pattern-point
	[pattern value x y field]
		)

(defn penalty-pattern-row
	[pattern value y field]
	(loop [x 0
		   penalty 0]
		   (if (< x size)
		   		(recur (inc x) (+ penalty (penalty-pattern-point pattern value x y field)))
		   		penalty)))

(defn penalty-pattern
	[pattern value field]
	(loop [y 0
		   penalty 0]
		   (if (< y size)
		   		(recur (inc y) (+ penalty (penalty-pattern-row pattern value y field)))
		   		penalty)))