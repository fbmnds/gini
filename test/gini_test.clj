(ns gini-test
  (:use midje.sweet
        (incanter core charts))
  (:require [gini :refer :all]))

;; save settings
;;
(def print-length *print-length*)
(set! *print-length* 5)
;;
;; set consistent en_US number formats for number/string conversions
;;
(def en_US (java.util.Locale. "en" "US"))
(def locale (java.util.Locale/getDefault))
(if-not (= locale en_US)
  (java.util.Locale/setDefault en_US))



;; depends on en_US for format consistency
;;
(defn- format-x [x n]
  (read-string (format (clojure.string/join ["%." (str n) "f"]) (java.math.BigDecimal. x))))

(defn- format-zz [zz n]
  (map #(map (fn [x] (format-x x n)) %) zz))

;; TODO: understand with-precision
;;
(defn- round-zz [zz n]
  (map #(map (fn [x] (with-precision n x)) %) zz))



(defmacro pure-time
  "Like clojure.core/time, returns the time as a value
   instead of a string./Leon Grapenthin."
  [expr]
  `(let [start# (. System (nanoTime))]
     (do
       (prn ~expr)
       (/ (double (- (. System (nanoTime)) start#)) 1000000.0))))


(def tn [100 1000 10000 100000])
(def theta 12)

(defn- perf? [tn]
  (let [[t100 t1000 t10000 t100000] tn
        res (and (< (/ t100000 t10000) theta)
                 (< (/ t10000 t1000) theta)
                 (< (/ t1000 t100) theta))]
    (if-not res
      (println "\n- observed times:\n  ---------------\n "
               (map #(format-x % 4) tn))
      true)))



;; testing private fn:
;; @#'some-ns/some-private-var
;; https://groups.google.com/d/msg/clojure/mJqplAdt3TY/q2Ur5j0OmTcJ



(defn- cum-fn-perf [n]
  (pure-time (take 5 (cum-fn (range n) +))))

(fact "cum-fn"
      (cum-fn (repeat 10 1) +) => '(1 2 3 4 5 6 7 8 9 10)
      (last (cum-fn (range 1 11) *)) => (* 1 2 3 4 5 6 7 8 9 10)
      (cum-fn [[0 1] [2 9]]
              (fn [x y] [(+ (x 0) (y 0)) (+ (x 1) (y 1))]))
      => [[0 1] [2 10]]
      (cum-fn [[0 1] [2 9] [-2 -10]]
              (fn [x y] [(+ (x 0) (y 0)) (+ (x 1) (y 1))]))
      => [[0 1] [2 10] [0 0]]
      (perf? (map cum-fn-perf tn)) => truthy)



(defn- set-xy-perf [n]
  (pure-time (take 5
              (@#'gini/set-xy (vec (take n (repeatedly #(rand-int 42))))
                              (vec (take n (repeatedly #(rand-int 42))))))))

(facts "set-xy"
       (let [x [0.0224 0.0276 0.0402 0.0498 0.06 0.09 0.11 0.15 0.19 0.26]
             y [0 1 1 1 1 1 1 1 1 10]]
         (fact (@#'gini/set-xy x)
               => '([0.0224 1] [0.0276 1] [0.0402 1] [0.0498 1] [0.06 1]
                      [0.09 1] [0.11 1] [0.15 1] [0.19 1] [0.26 1]))
         (fact (@#'gini/set-xy x y)
               => '([0.0224 0] [0.0276 1] [0.0402 1] [0.0498 1] [0.06 1]
                      [0.09 1] [0.11 1] [0.15 1] [0.19 1] [0.26 10])))
       (fact (perf? (map set-xy-perf tn)) => truthy))



(def test-set-xy (@#'gini/set-xy (vec (take 100000 (repeatedly #(rand-int 42))))
                                 (vec (take 100000 (repeatedly #(rand-int 42))))))

(defn- split-xy-perf [n]
  (pure-time (take 5
              (@#'gini/split-xy (take n test-set-xy)))))

(fact "split-xy"
      (perf? (map split-xy-perf tn)) => truthy)



(def test-cum+-xy
  (vec (@#'gini/cum-fn (sort-by first > test-set-xy) @#'gini/vec+)))

(defn- norm-xy-perf [n]
  (pure-time (format-zz
              (take 5 (@#'gini/norm-xy (take n test-cum+-xy))) 4)))

(fact "norm-xy"
      (perf? (map norm-xy-perf tn)) => truthy)



;; gini-xy tested implicitely via gini-x-y
;;
(defn- gini-x-y-perf [n]
  (pure-time (format-zz (take 2 (@#'gini/gini-x-y (take n test-set-xy))) 4)))

(facts "gini-x-y / gini-xy"
       (let [xy-1 '([0.0224 1] [0.0276 1] [0.0402 1] [0.0498 1] [0.06 1]
                      [0.09 1] [0.11 1] [0.15 1] [0.19 1] [0.26 1])
             xy-2 '([0.0224 0] [0.0276 1] [0.0402 1] [0.0498 1] [0.06 1]
                      [0.09 1] [0.11 1] [0.15 1] [0.19 1] [0.26 10])
             xy-3 [[0.0224 0] [0.0276 1] [0.0402 2] [0.0498 3] [0.06 7]
                   [0.09 6] [0.11 5] [0.15 4] [0.19 9] [0.4 10]]]
         (fact (format-zz (@#'gini/gini-x-y-100 xy-1) 2)
               => '((0.00 26.00 45.00 60.00 71.00
                     80.00 86.00 90.98 95.00 97.76 100.00)
                    (0.00 10.00 20.00 30.00 40.00
                     50.00 60.00 70.00 80.00 90.00 100.00)))
         (fact (format-zz (@#'gini/gini-x-y-100 xy-2) 2)
               => '((0.00 26.00 45.00 60.00 71.00
                     80.00 86.00 90.98 95.00 97.76 100.00)
                    (0.00 55.56 61.11 66.67 72.22
                     77.78 83.33 88.89 94.44 100.00 100.00)))
         (fact (format-zz (@#'gini/gini-x-y-100 xy-3) 2)
               => '((0.00 35.09 51.75 64.91 74.56
                     82.46 87.72 92.09 95.61 98.04 100.00)
                    (0.00 21.28 40.43 48.94 59.57
                     72.34 87.23 93.62 97.87 100.00 100.00)))
         (fact (format-zz (@#'gini/gini-x-y-100 xy-3 :order-pos second) 2)
               => '((0.00 35.09 51.75 57.02 64.91
                     74.56 87.72 92.09 95.61 98.04 100.00)
                    (0.00 21.28 40.43 55.32 68.09
                     78.72 87.23 93.62 97.87 100.00 100.00)))
         (fact (format-zz (@#'gini/gini-x-y-100 xy-3 :order < :order-pos second) 2)
               => '((0.00 1.96 4.39 7.91 12.28
                     25.44 35.09 42.98 48.25 64.91 100.00)
                    (0.00 0.00 2.13 6.38 12.77
                     21.28 31.91 44.68 59.57 78.72 100.00))))
       (fact (perf? (map gini-x-y-perf tn)) => truthy))


;; https://en.wikipedia.org/wiki/Gini_coefficient
;; Gini coefficients of representative income distributions
;;
(def x (take 1000 (iterate inc 1)))
(def y0 (map #(Math/pow % 0.3333) x))
(def y1 (map #(Math/sqrt %) x))
(def max-x (reduce max x))
(def y2 (map #(+ % (* max-x 0.1)) x))
(def y3 (map #(+ % (* max-x 0.05)) x))
(def y4 x)
(def y5 (map #(Math/pow % 2) x))
(def y6 (map #(Math/pow % 3) x))

(defn- <eps? [x y eps]
  (< (/ (Math/abs (- x y)) (+ x y)) eps))

(facts "gini-coeff"
       (fact (gini-coeff (repeat 10 1)) => 0.0)
       (fact (<eps? (gini-coeff y0) 0.143 0.02) => truthy)
       (fact (format-x (gini-coeff y1) 3) => 0.2)
       (fact (<eps? (gini-coeff y2) 0.273 0.02) => truthy)
       (fact (<eps? (gini-coeff y3) 0.302 0.02) => truthy)
       (fact (format-x (gini-coeff y4) 3) => 0.333)
       (fact (format-x (gini-coeff y5) 3) => 0.5)
       (fact (format-x (gini-coeff y6) 3) => 0.6)
       )



(save (lorenz-curve y0) "lc-y0.png")
(save (lorenz-curve y1) "lc-y1.png")
(save (lorenz-curve y2) "lc-y2.png")
(save (lorenz-curve y3) "lc-y3.png")
(save (lorenz-curve y4) "lc-y4.png")
(save (lorenz-curve y5) "lc-y5.png")
(save (lorenz-curve y6) "lc-y6.png")




;; restore settings
;;
(set! *print-length* print-length)
;;
(if-not (= locale en_US)
  (java.util.Locale/setDefault locale))
