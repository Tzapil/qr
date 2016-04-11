(ns qr.field
    (:require [qr.matrix :as matrix]))

(def version 1)
(def error-correction-level 3)
(def size (+ (* (- version 1) 4) 21))

(def finding-pattern
    [[3 3 3 3 3 3 3]
     [3 2 2 2 2 2 3]
     [3 2 3 3 3 2 3]
     [3 2 3 3 3 2 3]
     [3 2 3 3 3 2 3]
     [3 2 2 2 2 2 3]
     [3 3 3 3 3 3 3]])

(defn build-template
    [size]
    (vec (repeat size (vec (repeat size 9)))))

(def empty-pixel 9)

(defn- is-empty-pixel
    [x y field]
    (= (matrix/get-value x y field) empty-pixel))

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

(defn add-left-top-fp
    [field]
    (->> field
        (matrix/draw-pattern 0 0 finding-pattern)
        (matrix/draw-line-x 0 7 8 2)
        (matrix/draw-line-y 7 0 8 2)))

(defn add-right-top-fp
    [field]
    (let [x-pos (- size 7)]
        (->> field
            (matrix/draw-pattern x-pos 0 finding-pattern)
            (matrix/draw-line-x (dec x-pos) 7 8 2)
            (matrix/draw-line-y (dec x-pos) 0 8 2))))

(defn add-left-bottom-fp
    [field]
    (let [y-pos (- size 7)]
        (->> field
            (matrix/draw-pattern 0 y-pos finding-pattern)
            (matrix/draw-line-x 0 (dec y-pos) 8 2)
            (matrix/draw-line-y 7 (dec y-pos) 8 2))))

(defn- counter []  
  (let [tick (atom 0)]
    #(swap! tick (fn [n] (+ (mod (inc n) 2) 2)))))

(defn add-timing-patterns
    [field]
    (let [t1 (counter)
        t2 (counter)
        length (- size 16)]
        (->> field
            (matrix/draw-pattern 8 6 [(vec (repeatedly length t1))])
            (matrix/draw-pattern 6 8 (vec (repeatedly length #(vector (t2))))))))

(defn add-black-mark
    [field]
    (matrix/set-value 8 (- size 8) 3 field))

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
        (matrix/draw-pattern (- size 8) 8 [(vec (repeat 8 2))])    ;; top-right corner
        (matrix/draw-pattern 8 (- size 7) (vec (repeat 7 [2])))    ;; bottom-left corner
        (matrix/draw-pattern 0 8 [(vec (repeat 6 2))])                ;; top-left-horizontal
        (matrix/draw-pattern 8 0 (vec (repeat 6 [2])))                ;; top-left-vertical
        (matrix/draw-pattern 7 7 [[2 2]                            ;; top-left-corner
                                  [2 2]])))

(defn remove-reserved
    [field]
    (vec (map (fn [row] (vec (map #(mod % 2) row))) field)))

(defn add-data-step
    [x y data steps field]
    (let [steps-count (count steps)]
        (loop [curx-x x
               curx-y y
               counter 0
               [pixel & others] data
               new-field field]
               (if (and (< curx-y size) (>= curx-y 0))
                       (let [cur-pixel (matrix/get-value curx-x curx-y new-field)
                           current-step (steps counter)
                           next-x (+ curx-x (current-step 0))
                           next-y (+ curx-y (current-step 1))
                           next-step (mod (inc counter) steps-count)]
                           (if (= cur-pixel empty-pixel)
                               (recur next-x next-y next-step others (matrix/set-value curx-x curx-y pixel new-field))
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
                               (recur next-x r-data next-counter (add-data-step curx-x curx-y w-data steps new-field)))
                       new-field))))