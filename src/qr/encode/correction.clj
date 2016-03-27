(ns qr.encode.correction
	(:require 
		[qr.polynom :as polynom]
		[qr.helpers :as helpers]))

(def groups [{
		:blocks 1
		:codewords 13
	}])

(def correction-codewords 13)
(def data-codewords 13)
(def reminder-bits 0)

(defn break-codewords [string]
	(map #(helpers/byn-to-num (apply str %)) (partition 8 string)))

(defn break-blocks [string codewords]
	(map #(break-codewords %) (partition (* codewords 8) string)))

(defn break-groups [string [{:keys [blocks codewords]} & others]]
	(let [size (* blocks codewords 8)
		  group (break-blocks (take size string) codewords)]
		(conj (if (> (count others) 0) 
				(break-groups (drop size string) others) 
				[]) 
			group)))

(defn message-polynom
	[block]
	(map helpers/byn-to-num block))

(defn xor-polynoms
	[message generator]
	(polynom/trim-polynom (map #(bit-xor %1 %2) message generator)))

(defn division-polynoms
	[message generator]
	(let [normalized (polynom/normalize-polynom generator (first message))]
		(xor-polynoms message normalized)))

(defn division
	[message-polynom generator-polynom]
	(loop [iteration 0
		   message message-polynom
		   generator generator-polynom]
		(if (< iteration data-codewords)
			(let [xored-polynom (division-polynoms message generator)]
				(recur (inc iteration) xored-polynom (take (- (count generator) 1) generator)))
			message)))

(defn correction-block
	[block]
	(let [message (polynom/multiple-polynom-by-x block 1 correction-codewords)
		  generator (polynom/multiple-polynom-by-x (polynom/generator-polynom correction-codewords) 1 data-codewords)]
		  (division message generator)))

(defn correction-groups
	[groups]
	(map (fn [blocks] (map #(correction-block %) blocks)) groups))

(defn interleave-groups
	[groups]
	(let [blocks (reduce #(concat %1 %2) [] groups)]
		(loop [counter 0
		       result []]
		   	(let [elements (map (fn [block] (nth block counter nil)) blocks)
		   		  clear-elements (remove nil? elements)]
		   		(if (> (count clear-elements) 0)
		   			(recur (inc counter) (concat result clear-elements))
		   			result)))))

(defn convert-to-binary
	[numbers]
	(apply str (map #(helpers/lead-zeros (helpers/num-to-byn %) 8) numbers)))

(defn add-reminder-bits
	[string]
	(str string (apply str (repeat reminder-bits "0"))))

(defn RS-correction
	[string]
	(let [groups (break-groups string groups)
		  correction-codewords (correction-groups groups)
		  raw-seq (concat (interleave-groups groups) (interleave-groups correction-codewords))]
		  (-> raw-seq
		  	convert-to-binary
		  	add-reminder-bits)))

;;(defn Rid-Solomon-correction-group
;;	[[group & others]]
;;	(let [corrected ])
;;	(if (> (count others) 0)
;;		(Rid-Solomon-correction others)))