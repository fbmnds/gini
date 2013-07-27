(ns gini
  (:use (incanter core charts)))

; (use '(incanter core charts))
; (require 'clojure.inspector)


;; in O(n) on lazy-seqs
;;
(defn cum-fn
  ([s cfn]
     (cond (empty? s) nil
           (empty? (rest s)) (list (first s))
           :else (lazy-seq (cons (first s) (cum-fn (first s) (rest s) cfn)))))
  ([x s cfn]
     (cond (empty? s) nil
           (empty? (rest s)) (list (cfn x (first s)))
           :else (lazy-seq (cons (cfn x (first s)) (cum-fn (cfn x (first s)) (rest s) cfn))))))



(defn- set-xy
  ([x]
     (set-xy x (repeat (inc (count x)) 1)))
  ([x y]
     (map vector x y)))


(defn- split-xy [xy]
  [(vec (map first xy)) (vec (map second xy))])


(defn- vec+ [x y]
  [(+ (x 0) (y 0)) (+ (x 1) (y 1))])


(defn- norm-xy [cum+-xy]
  (let [max-cum+-xy (last cum+-xy)]
    (map (fn [[x y]] [(* (/ x (max-cum+-xy 0)) 100.0)
                     (* (/ y (max-cum+-xy 1)) 100.0)]) cum+-xy)))


(defn- x-y [xy & {:keys [order order-pos]
                  :or {order >
                       order-pos first}}]
  (let [cum+-xy (vec (cum-fn (sort-by order-pos order xy) vec+))]
    (split-xy (conj (norm-xy cum+-xy) [0 0]))))



(defn lorenz-curve
  ([xy & {:keys [order order-pos title x-label y-label legend]
          :or {order >
               order-pos first
               title "Lorenz Curve"
               x-label "% of cumulated x-observations"
               y-label "% of cumulated y-observations"
               legend true}}]
     (cond
      (number? (first xy))
      (lorenz-curve (set-xy xy)
                    :order order
                    :order-pos order-pos
                    :title title
                    :x-label "% of cumulated observations"
                    :y-label "% of population"
                    :legend legend)

      (vector? (first xy))
      (let [[x y] (x-y xy)]
        (doto
            (xy-plot x y
                     :title title
                     :x-label x-label
                     :y-label y-label
                     :legend legend)
          (add-lines y y)
          view)))

     :else
     (throw (IllegalArgumentException.
             "Input xy must be seq of numbers or of number tuples"))))
