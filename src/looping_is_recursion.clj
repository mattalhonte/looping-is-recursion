(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [my-current my-seq]
                 (if (empty? my-seq)
                   my-current
                   (recur (first my-seq) (rest my-seq))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (if (not (== (count seq1) (count seq2)))
    false
    (let [helper (fn [seq-a seq-b]
                   (if (not (= (first seq-a) (first seq-b)))
                     false
                     (if (and (empty? seq-a) (empty? seq-b))
                       true
                        (recur (rest seq-a) (rest seq-b)))))]
      (helper seq1 seq2))))

(defn find-first-index [pred a-seq]
  (loop [acc a-seq
         n 0]
    (if (empty? acc)
      nil
      (if (pred (first acc))
        n
        (recur (rest acc) (inc n))))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [acc 0
           n 0
           my-seq a-seq]
      (if (empty? my-seq)
        (/ acc n)
        (recur (+ (first my-seq) acc) (inc n) (rest my-seq))))))

(defn parity [a-seq]
    (loop [my-seq a-seq
           my-set #{}]
      (if (empty? my-seq)
        my-set
      (if (contains? my-set (first my-seq))
        (recur (rest my-seq) (disj my-set (first my-seq)))
        (recur (rest my-seq) (conj my-set (first my-seq)))))))

(defn fast-fibo [n]
  (if (zero? n)
    0
  (loop [my-count 0
         fib 1
         fib-last 0]
    (if (= my-count n)
      fib-last
      (recur (inc my-count) (+ fib fib-last) fib)))))

(defn cut-at-repetition [a-seq]
  (loop [my-seq a-seq
         new-seq []
         new-set #{}]
    (if (empty? my-seq)
      new-seq
      (if (contains? new-set (first my-seq))
        new-seq
        (recur (rest my-seq) (conj new-seq (first my-seq)) (conj new-set (first my-seq)))))))

