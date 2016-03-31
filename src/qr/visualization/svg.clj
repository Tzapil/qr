(ns qr.visualization.svg)

(def colours {0 "white"
			  1 "black"
			  9 "gray"
			  2 "blue"
			  3 "red"})

(def width 10)
(def height 10)

(defn point
	[x y w h value]
	(str "<rect fill=\"" (colours value) "\" x=\"" x "px\" y=\"" y "px\" width=\"" w "px\" height=\"" h "px\"/>\n"))

(defn draw-pattern-flat
	[line y]
	(let [size-x (count line)
		  yh (* y height)]
		(loop [iterator-x 0
			   pixels []]
			   (if (< iterator-x size-x)
					(recur (inc iterator-x) (conj pixels (point (* iterator-x width) yh width height (line iterator-x))))
					pixels))))

(defn draw-field
	[field]
	(let [size-y (count field)]
		(loop [iterator-y 0
			   pixels []]
			(if (< iterator-y size-y)
				(recur (inc iterator-y) (concat pixels (draw-pattern-flat (get field iterator-y) iterator-y)))
				pixels))))

(defn xml
  "svg 'template', which also flips the coordinate system"
  [qr-field]
  (str "<svg height=\"210px\" width=\"210px\">"
       (apply str (draw-field qr-field))
       "</svg>"))