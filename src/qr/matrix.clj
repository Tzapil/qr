(ns qr.matrix)

(def version 1)
(def size (+ (* (- version 1) 4) 21))

(def finding-pattern
	[[1 1 1 1 1 1 1]
	 [1 0 0 0 0 0 1]
	 [1 0 1 1 1 0 1]
	 [1 0 1 1 1 0 1]
	 [1 0 1 1 1 0 1]
	 [1 0 0 0 0 0 1]
	 [1 1 1 1 1 1 1]])

 (defn build-template
 	[size]
 	(apply vector (repeat size (apply vector (repeat size 9)))))

(defn set-pixel
	[field x y value]
	(assoc-in field [y x] value)) 

(defn draw-pattern-flat
	[field line x y]
	(let [size-x (count line)]
		(loop [iterator-x 0
			   new-field field]
			   (if (< iterator-x size-x)
					(recur (inc iterator-x) (set-pixel new-field (+ x iterator-x) y (get line iterator-x)))
					new-field))))

(defn draw-pattern
	[field pattern x y]
	(let [size-y (count pattern)]
		(loop [iterator-y 0
			   new-field field]
			(if (< iterator-y size-y)
				(recur (inc iterator-y) (draw-pattern-flat new-field (get pattern iterator-y) x (+ y iterator-y)))
				new-field))))

(defn draw-line-x
	[field x y length value]
	(draw-pattern field [(apply vector (repeat length value))] x y))

(defn draw-line-y
	[field x y length value]
	(draw-pattern field (apply vector (repeat length [value])) x y))

(defn add-left-top-fp
	[field]
	(draw-line-y (draw-line-x (draw-pattern field finding-pattern 0 0) 0 7 8 0) 7 0 8 0))

(defn add-right-top-fp
	[field]
	(let [x-pos (- size 7)]
		(draw-line-y (draw-line-x (draw-pattern field finding-pattern x-pos 0) (dec x-pos) 7 8 0) (dec x-pos) 0 8 0)))

(defn add-left-bottom-fp
	[field]
	(let [y-pos (- size 7)]
		(draw-line-y (draw-line-x (draw-pattern field finding-pattern 0 y-pos) 0 (dec y-pos) 8 0) 7 (dec y-pos) 8 0)))

(defn add-fiding-patterns
	[field]
	(add-left-bottom-fp (add-left-top-fp (add-right-top-fp field))))