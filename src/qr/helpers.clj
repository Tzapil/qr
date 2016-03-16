(ns qr.helpers)

(defn add-zeros-back
	[string count]
	(str string (apply str (repeat count "0"))))

(defn add-zeros-lead
	[string count]
	(str (apply str (repeat count "0")) string))

(defn missing-zeros
	[string require-length]
	(- require-length (count string)))

(defn lead-zeros
	[string require-length]
	(let [zeros-count (missing-zeros string require-length)]
		(add-zeros-lead string zeros-count)))

(defn back-zeros
	[string require-length]
	(let [zeros-count (- require-length (count string))]
		(add-zeros-back string zeros-count)))