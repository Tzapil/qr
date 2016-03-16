(ns qr.encode
	(:require 
		[qr.helpers :as helpers]
		[qr.mode :as mode]))

(def alphanumeric-first-char-multiplier 45)
(def alphanumeric-char-length 11)
(def alphanumeric-last-char-length 6)

(def alphanumeric-table {
	\0 0
	\1 1
	\2 2
	\3 3
	\4 4
	\5 5
	\6 6
	\7 7
	\8 8
	\9 9
	\A 10
	\B 11
	\C 12
	\D 13
	\E 14
	\F 15
	\G 16
	\H 17
	\I 18
	\J 19
	\K 20
	\L 21
	\M 22
	\N 23
	\O 24
	\P 25
	\Q 26
	\R 27
	\S 28
	\T 29
	\U 30
	\V 31
	\W 32
	\X 33
	\Y 34
	\Z 35
	\space 36 
	\$ 37
	\% 38
	\* 39
	\+ 40
	\- 41
	\. 42
	\/ 43
	\: 44
	})

(def maximum-capacity (* 17 8))

(defn encode-alphanumeric-part 
	([char1]
		(helpers/lead-zeros (Integer/toString char1 2) alphanumeric-last-char-length))
	([char1 char2]
		(let [result-number (+ char2 (* char1 alphanumeric-first-char-multiplier))]
			(helpers/lead-zeros (Integer/toString result-number 2) alphanumeric-char-length))))

(defn encode-alphanumeric
	[in-string]
	(let [string (clojure.string/upper-case in-string)]
		(map (fn [pair] 
				(apply encode-alphanumeric-part  (map #(get alphanumeric-table %) pair)))
		(partition-all 2 string))))


(defn encode-body
	[in-string]
	(str (get-in mode/modes [:alphanumeric :prefix]) 
		(helpers/lead-zeros (Integer/toString (count in-string) 2) (get-in mode/modes [:alphanumeric :length])) 
		(apply str (encode-alphanumeric in-string))))

(defn encode
	[in-string]
	(let [out-string (encode-body in-string)
		  missing-zeros (- maximum-capacity (count out-string))]
		(helpers/add-zeros-back out-string (min missing-zeros 4))))

(defn multiple-length-by
	[string multiplier]
	(let [missing-zeros (- multiplier (mod (count string) multiplier))]
		(helpers/add-zeros-back out-string missing-zeros)))