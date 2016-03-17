(ns qr.encode.core
	(:require 
		[qr.helpers :as helpers]
		[qr.mode :as mode]
		[qr.encode.alphanumeric :as alphanumeric]))

(def maximum-capacity (* 13 8))	;; 104

(def pads ["11101100" "00010001"])

(defn multiple-length-by
	[string multiplier]
	(let [missing-zeros (- multiplier (mod (count string) multiplier))]
		(helpers/add-zeros-back string missing-zeros)))

(defn mode-indicator
	[mode]
	(get-in mode/modes [mode :prefix]))

(defn char-count-length
	[mode version]
	(get-in mode/modes [mode :length]))

(defn char-count-indicator
	[in-string mode version]
	(helpers/lead-zeros (helpers/num-to-byn (count in-string)) (char-count-length mode version)))

(defn encode-raw
	[in-string]
	(str (mode-indicator :alphanumeric) 
		(char-count-indicator in-string :alphanumeric 1)
		(alphanumeric/encode in-string)))

(defn add-pads
	([raw-string]
		(add-pads raw-string 0))
	([raw-string counter]
		(if (< (count raw-string) maximum-capacity)
			(add-pads (str raw-string (get pads counter)) (mod (inc counter) 2))
			raw-string)))

(defn encode
	[in-string]
	(let [raw-string (encode-raw in-string)
		  missing-zeros (- maximum-capacity (count raw-string))]
		(add-pads (multiple-length-by (helpers/add-zeros-back raw-string (min missing-zeros 4)) 8))))