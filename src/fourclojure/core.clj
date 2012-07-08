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
