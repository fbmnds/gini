(ns gini-test
  (:require [clojure.test :refer :all]
            [gini :refer :all]))


(defmacro pure-time
  "Like clojure.core/time, returns the time as a value
   instead of a string."
  [expr]
  `(let [start# (. System (nanoTime))]
     (do
       (prn ~expr)
       (/ (double (- (. System (nanoTime)) start#)) 1000000.0))))


(deftest cum-fn-test
  (testing "testing cum-fn"
    (is (= (cum-fn (repeat 10 1) +)
           '(1 2 3 4 5 6 7 8 9 10)))
    (is (= (last (cum-fn (range 1 11) *))
           (* 1 2 3 4 5 6 7 8 9 10)))
    (is (= (cum-fn [[0 1] [2 9]] (fn [x y] [(+ (x 0) (y 0)) (+ (x 1) (y 1))]))
           [[0 1] [2 10]]))
    (is (= (cum-fn [[0 1] [2 9] [-2 -10]] (fn [x y] [(+ (x 0) (y 0)) (+ (x 1) (y 1))]))
           [[0 1] [2 10] [0 0]]))
    (let [t100 (pure-time (last (cum-fn (range 100) +)))
          t1000 (pure-time (last (cum-fn (range 1000) +)))
          t10000 (pure-time (last (cum-fn (range 10000) +)))
          t100000 (pure-time (last (cum-fn (range 100000) +)))]
      (is (and (< (/ t100000 t10000) 13)
               (< (/ t10000 t1000) 13)
               (< (/ t1000 t100) 13))))
    ))

; testing private fn:
; @#'some-ns/some-private-var
; https://groups.google.com/d/msg/clojure/mJqplAdt3TY/q2Ur5j0OmTcJ

(deftest set-xy-test
  (testing "testing set-xy"
    (let [x [0.0224 0.0276 0.0402 0.0498 0.06 0.09 0.11 0.15 0.19 0.26]
          y [0 1 1 1 1 1 1 1 1 10]]
      (is (= (@#'gini/set-xy x)
             '([0.0224 1] [0.0276 1] [0.0402 1] [0.0498 1] [0.06 1]
                 [0.09 1] [0.11 1] [0.15 1] [0.19 1] [0.26 1])))
      (is (= (@#'gini/set-xy x y))
          '([0.0224 0] [0.0276 1] [0.0402 1] [0.0498 1] [0.06 1]
              [0.09 1] [0.11 1] [0.15 1] [0.19 1] [0.26 10])))
    ))



(defn- round-zz [zz n]
  (map #(map (fn [x] (with-precision n (float x))) %) zz))

(defn- format-zz [zz n]
  (map #(map (fn [x] (format "%.2f" (float x))) %) zz))

(deftest x-y-test
  (testing "testing set-xy"
    (let [xy-1 '([0.0224 1] [0.0276 1] [0.0402 1] [0.0498 1] [0.06 1]
                 [0.09 1] [0.11 1] [0.15 1] [0.19 1] [0.26 1])
          xy-2 '([0.0224 0] [0.0276 1] [0.0402 1] [0.0498 1] [0.06 1]
                 [0.09 1] [0.11 1] [0.15 1] [0.19 1] [0.26 10])]
      (is (= (format-zz (@#'gini/x-y xy-1) 2)
             (format-zz '((0.0 26.0 45.0 60.0 71.0 80.0 86.0 90.98 95.0 97.76 100.0)
               (0.0 10.0 20.0 30.0 40.0 50.0 60.0 70.0 80.0 90.0 100.0)) 2)))
      (is (= (format-zz (@#'gini/x-y xy-2) 2)
             (format-zz '((0.0 26.0 45.0 60.0 71.0 80.0 86.0 90.98 95.0 97.76 100.0)
               (0.0 55.555557 61.11111 66.666664 72.22222 77.77778 83.333336 88.888885 94.44444 100.0 100.0)) 2))))
    ))
