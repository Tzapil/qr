(ns qr.matrix)

(def version 1)
(def error-correction-level 3)
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

(defn get-pixel
	[x y field]
	(get-in field [y x])) 

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
			(draw-pattern 8 6 [(vec (repeatedly length t1))])
			(draw-pattern 6 8 (vec (repeatedly length #(vector (t2))))))))


(defn add-black-mark
	[field]
	(set-pixel 8 (- size 8) 1 field))

(defn add-fiding-patterns
	[field]
	(-> field
		add-right-top-fp
		add-left-top-fp
		add-left-bottom-fp))

(defn add-alignment-patterns
	[field]
	field)

(defn reserve-information-area
	[field]
	(->> field
		(draw-pattern (- size 8) 8 [(vec (repeat 8 5))])	;; top-right corner
		(draw-pattern 8 (- size 7) (vec (repeat 7 [5])))	;; bottom-left corner
		(draw-pattern 0 8 [(vec (repeat 6 5))])				;; top-left-horizontal
		(draw-pattern 8 0 (vec (repeat 6 [5])))				;; top-left-vertical
		(draw-pattern 7 7 [[0 5]							;; top-left-corner
						   [5 5]])))

(def empty-pixel 9)

(defn- is-empty-pixel
	[x y field]
	(= (get-pixel x y field) empty-pixel))

(defn- count-empty-pixels
	[x y steps field]
	(let [steps-count (count steps)]
		(loop [curx-x x
			   curx-y y
			   counter 0
			   result 0]
			   (if (and (< curx-y size) (>= curx-y 0))
			   		(let [current-step (steps (mod counter 2))
			   			  next-x (+ curx-x (current-step 0))
			   			  next-y (+ curx-y (current-step 1))]
			   			  (recur next-x next-y (inc counter) (if (is-empty-pixel curx-x curx-y field) (inc result) result)))
			   		result))))

(defn add-data-step
	[x y data steps field]
	(let [steps-count (count steps)]
		(loop [curx-x x
			   curx-y y
			   counter 0
			   [pixel & others] data
			   new-field field]
			   ;;(println "SMALL: " curx-x ", " curx-y)
			   (if (and (< curx-y size) (>= curx-y 0))
			   		(let [cur-pixel (get-pixel curx-x curx-y new-field)
			   			current-step (steps counter)
			   			next-x (+ curx-x (current-step 0))
			   			next-y (+ curx-y (current-step 1))
			   			next-step (mod (inc counter) steps-count)]
			   			(if (= cur-pixel empty-pixel)
			   				(recur next-x next-y next-step others (set-pixel curx-x curx-y pixel new-field))
			   				(recur next-x next-y next-step (into [pixel] others) new-field)))
			   		new-field)
			   )))

(defn add-data-to-field
	[data field]
	(let [last-point (- size 1)
		  directions [{
				:steps [[-1 0] [1 -1]]
				:y last-point 
		  	} 
		  	{
			 	:steps [[-1 0] [1 1]]
			 	:y 0
		 	}]]
		(loop [curx-x last-point
			   rest-data data
			   counter 0
			   new-field field]
			   (if (> (count rest-data) 0)
			   		(let [direction (directions counter)
			   		 	  steps (:steps direction)
			   		 	  curx-y (:y direction)
			   		 	  next-x (- curx-x 2)
			   		 	  next-counter (mod (inc counter) 2)
			   		 	  empty-pixels (count-empty-pixels curx-x curx-y steps new-field)
			   		 	  w-data (take empty-pixels rest-data)
			   		 	  r-data (drop empty-pixels rest-data)]
			   		 	  	(println "BIG: " curx-x)
			   		 	  	(println "EMPTY_PIX: " empty-pixels)
			   		 	  	(println "W_DATA: " w-data)
			   				(recur next-x r-data next-counter (add-data-step curx-x curx-y w-data steps new-field)))
			   		new-field))))