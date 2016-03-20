(ns qr.core
  (:require [qr.encode.core :as encode]
  			[qr.galua-field :as galua]
  			[qr.polynom :as polynom]
  			[qr.encode.correction :as correction]
  			[qr.matrix :as matrix]
  			[qr.visualization.svg :as svg])
  (:gen-class))

(defn enc [string]
	(correction/RS-correction (encode/encode string)))

(defn find-patt
	[]
	(matrix/add-fiding-patterns (matrix/build-template 21)))

(defn svg-paint
	[]
	(let [filename "qr_code.html"]
		(spit filename (svg/xml (find-patt)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (svg-paint)
  (println (enc "hello world")))
