(ns qr.core
  (:require [qr.encode.core :as encode]
  			[qr.galua-field :as galua]
  			[qr.polynom :as polynom]
  			[qr.encode.correction :as correction]
  			[qr.field :as matrix]
  			[qr.visualization.svg :as svg]
  			[qr.helpers :as helpers]
        [qr.masking :as masking])
  (:gen-class))

(defn enc [string]
	(let [encoded (encode/encode string)]
    (println "WO CORRECTION")
    (println encoded)
    (println (count encoded))
    (correction/RS-correction encoded)))

(defn find-patt
	[data]
  (->>
    (matrix/build-template 21)
    matrix/add-fiding-patterns
    matrix/add-timing-patterns
    matrix/add-alignment-patterns
    matrix/add-black-mark
    matrix/reserve-information-area
    (matrix/add-data-to-field (map #(helpers/byn-to-num (str %)) data))
    masking/mask))

(defn svg-paint
	[data]
	(let [filename "qr_code.html"]
		(spit filename (svg/draw (find-patt data)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [encounted (enc "HELLO")]
  		(println (count encounted))
  		(println encounted)
  		(svg-paint encounted)))
