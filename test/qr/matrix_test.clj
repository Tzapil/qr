(ns qr.matrix-test
  (:require [clojure.test :refer :all]
            [qr.matrix :refer :all]))

(deftest bt-test
  (testing "Build Template"
    (doseq [size [21 42 56 34 33 45 100]]
        (let [template (build-template size)
              y-size (count template)
              x-sizes (map #(count %) template)]
            (is (= y-size size))
            (is (not (some #(not (= size %)) x-sizes)))
            (is (not (some (fn [row] (some #(not (= 9 %)) row)) template)))))))

(deftest gp-test
  (testing "Get Pixel From Field"
    (let [field [[1  2  3  4  5  6]
                 [7  8  9  10 11 12]
                 [0  0  0  0  0  0]
                 [3  3  3  4  3  5]]
          test-data [{:x 0 :y 0 :v 1}
                       {:x 1 :y 0 :v 2}
                       {:x 2 :y 0 :v 3}
                       {:x 3 :y 0 :v 4}
                       {:x 4 :y 0 :v 5}
                       {:x 5 :y 0 :v 6}
                       {:x 0 :y 1 :v 7}
                       {:x 5 :y 1 :v 12}
                       {:x 5 :y 3 :v 5}
                       {:x 3 :y 3 :v 4}
                       {:x 20 :y 20 :v nil}
                       {:x -1 :y 3 :v nil}]]
        (doseq [t test-data]
            (let [{x :x y :y v :v} t]
                (is (= v (get-pixel x y field))))))))

(deftest sp-test
  (testing "Set Pixel From Field"
    (let [field [[0 0 0 0 0 0]
                 [0 0 0 0 0 0]]
          test-data [{:x 1 :y 1 :v 1}
                       {:x 3 :y 0 :v 3}
                       {:x 2 :y 1 :v 5}]]
        (doseq [t test-data]
            (let [{x :x y :y v :v} t
                  new-field (set-pixel x y v field)]
                (is (= v (get-pixel x y new-field))))))))

(deftest eq-test
	(testing "Equal Function"
		(let [tests [{
				:f1 [[0 0 0 0 0]]
				:f2 [[0 0 0 0 0]]
				:r true
			} {
				:f1 [[0 1 2 3 4]
				     [5 6 7 8 9]
				     [0 1 2 3 4]
				     [5 6 7 8 9]]	
			    :f2 [[0 1 2 3 4]
				     [5 6 7 8 9]
				     [0 1 2 3 4]
				     [5 6 7 8 9]]
			    :r true	
			} {
				:f1 [[0 1 2 7 4]	;; error
				     [5 6 7 8 9]
				     [0 1 2 3 4]
				     [5 6 7 8 9]]	
			    :f2 [[0 1 2 3 4]
				     [5 6 7 8 9]
				     [0 1 2 3 4]
				     [5 6 7 8 9]]	
			    :r false
			}]]
			(doseq [{f1 :f1 f2 :f2 r :r} tests]
				(is (= r (equal f1 f2)))))))

(deftest dlx-test
    (testing "Draw X Line"
        (let [field [[0 0 0 0 0 0]
                     [0 0 0 0 0 0]
                     [0 0 0 0 0 0]]
              rfield [[0 5 5 5 5 0]
                      [5 0 0 0 0 0]
                      [0 0 5 5 5 0]]
              test-data [{:x 1 :y 1 :l 0 :v 5}
              			 {:x 1 :y 0 :l 4 :v 5}
                         {:x 0 :y 1 :l 1 :v 5}
                         {:x 2 :y 2 :l 3 :v 5}]]
            (let [tfield (reduce (fn [nfield {x :x y :y l :l v :v}] (draw-line-x x y l v nfield)) field test-data)]
            	(is (equal rfield tfield))))))

(def dly-test
    (testing "Draw Y Line"
        (let [field [[0 0 0 0 0 0]
                     [0 0 0 0 0 0]
                     [0 0 0 0 0 0]
                     [0 0 0 0 0 0]
                     [0 0 0 0 0 0]]
              rfield [[0 5 5 5 5 0]
                      [5 0 0 0 0 0]
                      [0 0 5 5 5 0]]
              test-data [{:x 1 :y 1 :l 0 :v 5}
              			 {:x 1 :y 0 :l 4 :v 5}
                         {:x 0 :y 1 :l 1 :v 5}
                         {:x 2 :y 2 :l 3 :v 5}]]
            (let [tfield (reduce (fn [nfield {x :x y :y l :l v :v}] (draw-line-x x y l v nfield)) field test-data)]
            	(is (equal rfield tfield))))))