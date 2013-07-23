(ns gini)


;; approx. in O(n)
;;
(defn cum-fn-finite [v cfn]
  (let [s (seq v)
        f (vector (first s))
        r (rest s)]
    (cond
     (empty? s) nil
     (empty? r) f
     :else (loop [last-of-head (* 1N (last f))
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



(defn gini-plot
  [obs]
  (cond
   (not (seq? obs)) nil
   (< (count obs) 2) nil
   :else
   (let [count-of-obs (count obs)
         cum-count-of-obs (range 1 (inc count-of-obs))
         sum-of-obs (reduce + obs)
         obs (sort > obs)
         cum-sum-of-obs (cum-sum obs)])))
