(ns gini
  (:use (incanter core charts)))

; (use '(incanter core charts))
; (require 'clojure.inspector)


;; re-invention of clojure.core/reductions
;; (ref´ed by A. Stoddard)
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



(defn set-xy
  ([x]
     (set-xy x (repeat (count x) 1)))
  ([x y]
     (map vector x y)))


(defn- split-xy [xy]
  [(vec (map first xy)) (vec (map second xy))])


(defn- vec+ [x y]
  [(+ (x 0) (y 0)) (+ (x 1) (y 1))])

;; xy must be sorted by >
;; use norm-xy ONLY after cum-fn!
;;
(defn- norm-xy [xy]
  (let [max-xy (last xy)]
    (map (fn [[x y]] [(* 1.0 (/ x (max-xy 0)))
                     (* 1.0 (/ y (max-xy 1)))]) xy)))


(defn gini-xy [xy & {:keys [order order-pos]
                     :or {order >
                          order-pos first}}]
  (conj (norm-xy (reductions vec+ (sort-by order-pos order xy))) [0 0]))


(defn gini-x-y [xy & {:keys [order order-pos]
                  :or {order >
                       order-pos first}}]
  (split-xy (gini-xy xy :order order :order-pos order-pos)))


(defn gini-x-y-100 [xy & {:keys [order order-pos]
                  :or {order >
                       order-pos first}}]
  (map #(map (partial * 100.0) %)
       (gini-x-y xy :order order :order-pos order-pos)))


(defn- x0 [z] (first (first z)))
(defn- x1 [z] (second (first z)))
(defn- y0 [z] (first (second z)))
(defn- y1 [z] (second (second z)))
(defn- s [z] (* (- (x1 z) (x0 z)) (+ (y1 z) (y0 z))))

(defn gini-coeff
  ([xy]
     (cond
      (number? (first xy)) (gini-coeff xy (repeat (count xy) 1))
      (vector? (first xy))
      (let [v (map (partial partition 2 1) (gini-x-y xy))]
        (- 1.0 (reduce + (map s (set-xy (first v) (second v))))))
      :else
      (throw (IllegalArgumentException.
              "Input xy must be seq of numbers or of pairs of numbers"))))
  ([x y] (gini-coeff (set-xy x y))))



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
      (let [[x y] (gini-x-y xy)]
        (doto
            (xy-plot x y
                     :title title
                     :x-label x-label
                     :y-label y-label
                     :legend legend)
          (add-lines y y)
          view))

     :else
     (throw (IllegalArgumentException.
             "Input xy must be seq of numbers or of pairs of numbers")))))
