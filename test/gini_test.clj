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
                   [0.09 1] [0.11 1] [0.15 1] [0.19 1] [0.26 10])
          xy-3 [[0.0224 0] [0.0276 1] [0.0402 2] [0.0498 3] [0.06 7]
                [0.09 6] [0.11 5] [0.15 4] [0.19 9] [0.4 10]]]
      (is (= (format-zz (@#'gini/x-y xy-1) 2)
             '(("0,00" "26,00" "45,00" "60,00" "71,00" "80,00" "86,00" "90,98" "95,00" "97,76" "100,00")
               ("0,00" "10,00" "20,00" "30,00" "40,00" "50,00" "60,00" "70,00" "80,00" "90,00" "100,00"))))
      (is (= (format-zz (@#'gini/x-y xy-2) 2)
             '(("0,00" "26,00" "45,00" "60,00" "71,00" "80,00" "86,00" "90,98" "95,00" "97,76" "100,00")
               ("0,00" "55,56" "61,11" "66,67" "72,22" "77,78" "83,33" "88,89" "94,44" "100,00" "100,00"))))
      (is (= (format-zz (@#'gini/x-y xy-3) 2)
             '(("0,00" "35,09" "51,75" "64,91" "74,56" "82,46" "87,72" "92,09" "95,61" "98,04" "100,00")
               ("0,00" "21,28" "40,43" "48,94" "59,57" "72,34" "87,23" "93,62" "97,87" "100,00" "100,00"))))
      (is (= (format-zz (@#'gini/x-y xy-3 :order-pos second) 2)
             '(("0,00" "35,09" "51,75" "57,02" "64,91" "74,56" "87,72" "92,09" "95,61" "98,04" "100,00")
               ("0,00" "21,28" "40,43" "55,32" "68,09" "78,72" "87,23" "93,62" "97,87" "100,00" "100,00"))))
      (is (= (format-zz (@#'gini/x-y xy-3 :order < :order-pos second) 2)
             '(("0,00" "1,96" "4,39" "7,91" "12,28" "25,44" "35,09" "42,98" "48,25" "64,91" "100,00")
               ("0,00" "0,00" "2,13" "6,38" "12,77" "21,28" "31,91" "44,68" "59,57" "78,72" "100,00"))))

      ;; test with larger input:
      ;; (def xy-4 (map vector
      ;;             (vec (take 100000 (repeatedly #(rand-int 42))))
      ;;             (vec (take 100000 (repeatedly #(rand-int 42))))))

      )
    ))
