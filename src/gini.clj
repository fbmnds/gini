(ns gini)


(defmacro pure-time
  "Like clojure.core/time, returns the time as a value
   instead of a string."
  [expr]
  `(let [start# (. System (nanoTime))]
     (do
       (prn ~expr)
       (/ (double (- (. System (nanoTime)) start#)) 1000000.0))))



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



;; approx. in O(n)
;;
(defn cum-fn-finite [v cfn]
  (let [s (seq v)
        f (vector (first s))
        r (rest s)]
    (cond
     (empty? s) nil
     (empty? r) f
     :else (loop [last-of-head (last f)
                  head f
                  tail r]
             (if (empty? tail)
               head
               (let [l (cfn last-of-head (first tail))]
                 (recur l
                        (conj head l)
                        (next tail))))))))



;; approx. in O(n)
;;
(defn cum-sum-finite [v]
  (let [s (seq v)
        f (vector (first s))
        r (rest s)]
    (cond
     (empty? s) nil
     (empty? r) f
     :else (loop [last-of-head (last f)
                  head f
                  tail r]
             (if (empty? tail)
               head
               (let [l (+ last-of-head (first tail))]
                 (recur l
                        (conj head l)
                        (next tail))))))))


;; worse than O(n)
;;
(defn cum-sum-finite [v]
  (let [s (seq v)
        f (vector (first s))
        r (rest s)]
    (cond
     (empty? s) nil
     (empty? r) f
     :else (loop [head f
                  tail r]
             (if (empty? tail)
               head
               (recur (conj head (+ (last head) (first tail)))
                      (next tail)))))))



;; worse than O(n)
;;
(defn cum-sum-finite [v]
  (let [s (seq v)
        f (vector (first s))
        r (rest s)]
    (cond
     (empty? s) nil
     (empty? r) f
     :else (reduce (fn [v x] (conj v (+ (last v) x))) f (vec r)))))




(defn cum-sum-finite [v]
  (reduce (fn [v x] (conj v (+ (last v) x))) (vector (first v)) (rest v)))

(comment

user> (cum-sum-finite [1 2 3 4 5 6])
[1 3 6 10 15 21]
user> (time (last (cum-sum-finite (vec (range 10)))))
"Elapsed time: 1.057789 msecs"
45
user> (time (last (cum-sum-finite (vec (range 100)))))
"Elapsed time: 2.468889 msecs"
4950
user> (time (last (cum-sum-finite (vec (range 1000)))))
"Elapsed time: 92.410092 msecs"
499500
user> (time (last (cum-sum-finite (vec (range 10000)))))
"Elapsed time: 4942.767294 msecs"
49995000
user> (time (last (cum-sum-finite (vec (range 100000)))))
"Elapsed time: 571564.274941 msecs"
4999950000

  )




;; does not work: recur is not in tail position
;;
(defn cum-sum
  [s]
  (if (= 1 (count s)) s
      (let [seq (seq s)
            head (first seq)
            tail (rest seq)
            rec-cum-sum
            (fn rec-cum-sum
              [acc s]
              (if (= 1 (count s)) (list (+ acc (first s)))
                  (let [head (first s)
                        tail (rest s)
                        acc (+ acc head)]
                    (lazy-seq (cons acc (recur acc tail))))))]
        (lazy-seq (cons head (rec-cum-sum head tail))))))



;; inefficient, as without recur
;;
(defn cum-sum
  [s]
  (if (= 1 (count s)) s
      (let [seq (seq s)
            head (first seq)
            tail (rest seq)
            rec-cum-sum
            (fn rec-cum-sum
              [acc s]
              (if (= 1 (count s)) (list (+ acc (first s)))
                  (let [head (first s)
                        tail (rest s)
                        acc (+ acc head)]
                    (lazy-seq (cons acc (rec-cum-sum acc tail))))))]
        (lazy-seq (cons head (rec-cum-sum head tail))))))



(use '(incanter core charts))

(declare lorenz-curve)

(defn lorenz-curve-x
  [x & {:keys [order title x-label y-label legend]
        :or {order >
             title "Lorenz Curve"
             x-label "% of cumulated observations"
             y-label "% of population"
             legend true}}]
  (if (= order >)
    (lorenz-curve x (repeat (count x) 1)
                  :order >
                  :title title
                  :x-label x-label
                  :y-label y-label
                  :legend legend)
    (lorenz-curve (repeat (count x) 1) x
                  :order <
                  :title title
                  :x-label y-label
                  :y-label x-label
                  :legend legend)))


(defn lorenz-curve
  ([x y & {:keys [order title x-label y-label legend]
           :or {order >
                title "Lorenz Curve"
                x-label "% of cumulated x-observations"
                y-label "% of cumulated y-observations"
                legend true}}]
     (let [cum+-x (cum-fn (sort order x) +)
           x (conj (map #(/ % (last cum+-x)) cum+-x) 0)  ; could throw Div0-Exception
           cum+-y (cum-fn (sort order y) +)
           y (conj (map #(/ % (last cum+-y)) cum+-y) 0)] ; could throw Div0-Exception
       (doto
           (xy-plot x y
                    :title title
                    :x-label x-label
                    :y-label y-label
                    :legend legend)
         (add-lines y y)
         view))))


(defn lorenz-curve
  [obs & {:keys [order title x-label y-label legend]
          :or {order >
               title "Lorenz Curve"
               x-label "% of cumulated observations"
               y-label "% of population"
               legend true}}]
  (cond
   (< (count obs) 2) nil
   :else
   (let [obs (sort order obs)
         count-obs (count obs)
         y (conj (map #(/ % count-obs) (range 1 (inc count-obs))) 0)
         cum+-obs (cum-fn obs +)
         x (conj (map #(/ % (last cum+-obs)) cum+-obs) 0)] ; could throw Div0-Exception
     (doto
         (xy-plot x y
                  :title title
                  :x-label x-label
                  :y-label y-label
                  :legend legend)
       (add-lines y y)
       view))))



(defn test-parms
  [x & y {:keys [order title x-label y-label legend]
          :or {order >
               title "Lorenz Curve"
               x-label "% of cumulated observations"
               y-label "% of population"
               legend true}}]
  (cond (symbol? y) (print "x :keys")
        :else (print "x y :keys")))
