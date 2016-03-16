(ns qr.core
  (:require [qr.encode :as encode])
  (:gen-class))

(defn enc [string]
	(encode/encode string))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
