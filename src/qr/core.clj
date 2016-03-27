(ns qr.core
  (:require [qr.encode.core :as encode]
  			[qr.galua-field :as galua]
  			[qr.polynom :as polynom]
  			[qr.encode.correction :as correction]
  			[qr.matrix :as matrix]
  			[qr.visualization.svg :as svg]
  			[qr.helpers :as helpers])
  (:gen-class))

(defn enc [string]
	(correction/RS-correction (encode/encode string)))

(defn find-patt
	[data]
  (->>
    (matrix/build-template 21)
    matrix/add-fiding-patterns
    matrix/add-timing-patterns
    matrix/add-alignment-patterns
    matrix/add-black-mark
    matrix/reserve-information-area
    (matrix/add-data-to-field (map #(helpers/byn-to-num (str %)) data))))

(defn svg-paint
	[data]
	(let [filename "qr_code.html"]
		(spit filename (svg/xml (find-patt data)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [encounted (enc "hello world")]
  		(println (count encounted))
  		(println encounted)
  		(svg-paint encounted)))
