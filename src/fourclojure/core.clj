(ns fourclojure.core)

                                        ; Problem 79 - Triangle Minimal Path
(defn fx
  ([t] (fx t 0))
  ([t i]
     (cond
      (seq t) (+ ((first t) i)
                 (min (fx (rest t) i)
                      (fx (rest t) (inc i))))
      :else 0)))

(= 7 (fx '([1] [2 4] [5 1 4] [2 3 4 5])))

(= 20 (fx '([3] [2 4] [1 9 3] [9 9 2 4] [4 6 6 7 8] [5 7 3 5 1 4])))

                                        ; Problem 80 - Perfect Numbers

(defn __ [x] (= x (reduce + (filter #(= 0 (rem x %)) (range 1 x)))))

(= (__ 6) true)

(= (__ 7) false)

(= (__ 496) true)

(= (__ 500) false)

(= (__ 8128) true)

                                        ; Problem 81 - Set Intersection

(def __ (comp set keep))

(= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})

(= (__ #{0 1 2} #{3 4 5}) #{})

(= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})

                                        ; Problem 82 - Word Chains

(defn __
  ([xs] (some #(__ % (disj xs)) xs))
  ([x xs] (cond (seq xs) ()
                :else true)))

(defn levenshein
  ([a b] (levenshein (seq a) (seq b) 0))
  ([a b d]
     (cond (or (empty? a) (empty? b)) (+ d (count a) (count b))
           (= (first a) (first b)) (recur (rest a) (rest b) d)
           :else (min (levenshein a (rest b) (inc d))
                      (levenshein (rest a) b (inc d))
                      (if (and (= (first a) (first (rest b)))
                               (= (first b) (first (rest a))))
                        (levenshein (rest (rest a)) (rest (rest b)) (inc d))
                        (levenshein (rest a) (rest b) (inc d)))))))

(= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))

(= false (__ #{"cot" "hot" "bat" "fat"}))

(= false (__ #{"to" "top" "stop" "tops" "toss"}))

(= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))

(= true (__ #{"share" "hares" "shares" "hare" "are"}))

(= false (__ #{"share" "hares" "hare" "are"}))

                                        ; Problem 83 - A Half-Truth
(def __ not=)

(= false (__ false false))

(= true (__ true false))

(= false (__ true))

(= true (__ false true false))

(= false (__ true true true))

(= true (__ true true true false))

                                        ; Problem 84 - Transitive Closure

(def __ #(let [s (into % (for [[a b] % [c d] % :when (= b c)] [a d]))]
  (if (= s %) s (recur s))))

(let [divides #{[8 4] [9 3] [4 2] [27 9]}]
  (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))

(let [more-legs
      #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
  (= (__ more-legs)
     #{["cat" "man"] ["cat" "snake"] ["man" "snake"] ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))

(let [progeny
      #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
  (= (__ progeny)
     #{["father" "son"] ["father" "grandson"] ["uncle" "cousin"] ["son" "grandson"]}))

                                        ; Problem 85 - Power Set

(def __ (fn fx [s]
          (reduce clojure.set/union #{s} (map #(fx (disj s %)) s))))

(def __ (fn fx [s]
          (if (empty? s) #{#{}}
              (clojure.set/union (fx (next s))
                                 (map #(conj % (first s)) (fx (next s)))))))

(def __ (fn [s]
          (reduce (fn [s k] (into s (map #(conj % k) s))) #{#{}} s)))

(= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})

(= (__ #{}) #{#{}})

(= (__ #{1 2 3})
   #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})

(= (count (__ (into #{} (range 10)))) 1024)

                                        ; Problem 86 - Happy Numbers

(def __ (fn [x]
  (loop [x x xs #{}]
    (cond (xs x) false
          (= x 1) true
          :else (recur
                 (reduce + (map (comp #(* % %) read-string str) (str x)))
                             (conj xs x))))))

(= (__ 7) true)

(= (__ 986543210) true)

(= (__ 2) false)

(= (__ 3) false)

                                        ; Problem 87 - Doesn't Exist

                                        ; Problem 88 - Symmetric Difference

(def __ (partial reduce #((if (% %2) disj conj) % %2)))

(= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})

(= (__ #{:a :b :c} #{}) #{:a :b :c})

(= (__ #{} #{4 5 6}) #{4 5 6})

(= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})

                                        ; Problem 89 - Graph Tour

                                        ; - You can start at any node.
                                        ; - You must visit each edge exactly once.
                                        ; - All edges are undirected.

(= true (__ [[:a :b]]))

(= false (__ [[:a :a] [:b :b]]))

(= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
              [:a :d] [:b :d] [:c :d]]))

(= true (__ [[1 2] [2 3] [3 4] [4 1]]))

(= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
             [:b :e] [:a :d] [:b :d] [:c :e]
             [:d :e] [:c :f] [:d :f]]))

(= false (__ [[1 2] [2 3] [2 4] [2 5]]))
