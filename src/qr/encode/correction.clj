(ns qr.encode.correction)

(def groups [{
		:blocks 1
		:codewords 13
	}])

(defn break-codewords [string count]
	(map #(apply str %) (partition 8 (take (* count 8) string))))

(defn break-blocks [string count codewords]
	(map #(break-codewords (apply str %) codewords) (partition (* codewords 8) string)))

(defn break-groups [string [{:blocks blocks-count :codewords codewords-count} & others]]
	(let [size (* blocks-count codewords-count 8)
		  group (break-blocks (take size string) blocks-count break-codewords)]
		(conj group (break-groups (apply str (drop size string)) others))))