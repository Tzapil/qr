(ns qr.encode.alphanumeric
	(:require [qr.helpers :as helpers]))

(def first-char-multiplier 45)
(def char-length 11)
(def last-char-length 6)

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

(defn encode-part 
	([chr]
		(helpers/lead-zeros (Integer/toString chr 2) last-char-length))
	([chr1 chr2]
		(let [result-number (+ chr2 (* chr1 first-char-multiplier))]
			(helpers/lead-zeros (Integer/toString result-number 2) char-length))))

(defn encode
	[in-string]
	(let [string (clojure.string/upper-case in-string)]
		(apply str 
			(map (fn [pair] 
				(apply encode-part   (map #(get alphanumeric-table %) pair)))
			(partition-all 2 string)))))
