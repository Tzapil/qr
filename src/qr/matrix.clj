(ns qr.matrix)

(defn equal
 	[m1 m2]
	(every? true? (map #(= %1 %2) m1 m2)))

(defn set-value
    [x y value m]
    (assoc-in m [y x] value)) 

(defn get-value
    [x y m]
    (get-in m [y x])) 

(defn draw-pattern-flat
    [x y line m]
    (let [size-x (count line)]
        (loop [iterator-x 0
               new-m m]
               (if (< iterator-x size-x)
                    (recur (inc iterator-x) (set-value (+ x iterator-x) y (get line iterator-x) new-m))
                    new-m))))

(defn draw-pattern
    [x y pattern m]
    (let [size-y (count pattern)]
        (loop [iterator-y 0
               new-m m]
            (if (< iterator-y size-y)
                (recur (inc iterator-y) (draw-pattern-flat x (+ y iterator-y) (get pattern iterator-y) new-m))
                new-m))))

(defn draw-line-x
    [x y length value m]
    (draw-pattern x y [(vec (repeat length value))] m))

(defn draw-line-y
    [x y length value m]
    (draw-pattern x y (vec (repeat length [value])) m))

(defn walk-row
    [function accum y m]
    (let [row (m y)
          size (count row)]
        (loop [x 0
               result accum]
            (if (< x size)
                (recur (inc x) (function result x y))
                result))))

(defn walk
    [function accum m]
    (let [size (count m)]
        (loop [y 0
               result accum]
            (if (< y size)
                (recur (inc y) (walk-row function result y m))
                result))))

(defn print-matrix
    [m]
    (vec (map #(or (println %) %) m)))