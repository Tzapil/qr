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
 	(vec (repeat size (vec (repeat size 9)))))

(defn set-pixel
	[x y value field]
	(assoc-in field [y x] value)) 

(defn draw-pattern-flat
	[x y line field]
	(let [size-x (count line)]
		(loop [iterator-x 0
			   new-field field]
			   (if (< iterator-x size-x)
					(recur (inc iterator-x) (set-pixel (+ x iterator-x) y (get line iterator-x) new-field))
					new-field))))

(defn draw-pattern
	[x y pattern field]
	(let [size-y (count pattern)]
		(loop [iterator-y 0
			   new-field field]
			(if (< iterator-y size-y)
				(recur (inc iterator-y) (draw-pattern-flat x (+ y iterator-y) (get pattern iterator-y) new-field))
				new-field))))

(defn draw-line-x
	[x y length value field]
	(draw-pattern x y [(vec (repeat length value))] field))

(defn draw-line-y
	[x y length value field]
	(draw-pattern x y (vec (repeat length [value])) field))

(defn add-left-top-fp
	[field]
	(->> field
		(draw-pattern 0 0 finding-pattern)
		(draw-line-x 0 7 8 0)
		(draw-line-y 7 0 8 0 )))

(defn add-right-top-fp
	[field]
	(let [x-pos (- size 7)]
		(->> field
			(draw-pattern x-pos 0 finding-pattern)
			(draw-line-x (dec x-pos) 7 8 0)
			(draw-line-y (dec x-pos) 0 8 0))))

(defn add-left-bottom-fp
	[field]
	(let [y-pos (- size 7)]
		(->> field
			(draw-pattern 0 y-pos finding-pattern)
			(draw-line-x 0 (dec y-pos) 8 0)
			(draw-line-y 7 (dec y-pos) 8 0))))

(defn- counter []  
  (let [tick (atom 0)]
    #(swap! tick (fn [n] (mod (inc n) 2)))))

(defn add-timing-patterns
	[field]
	(let [t1 (counter)
		t2 (counter)
		length (- size 16)]
		(->> field
			(draw-pattern 8 7 [(vec (repeatedly length t1))])
			(draw-pattern 7 8 (vec (repeatedly length #(vector (t2))))))))


(defn add-fiding-patterns
	[field]
	(-> field
		add-right-top-fp
		add-left-top-fp
		add-left-bottom-fp))