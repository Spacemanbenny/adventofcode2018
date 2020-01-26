(ns adventofcode2018.aoc18.day2
  (:require [clojure.math.combinatorics :as combo]))

(defn hasNs [n id]
  (some
    #(= % n)
    (map
      val
      (frequencies id))))

(defn part1 [ids]
 (*
    (count
      (filter #(hasNs 3 %) ids))
    (count
      (filter #(hasNs 2 %) ids))
  )
)

(defn part1Solution []
  (part1
     (clojure.string/split-lines
       (slurp "day2input.txt")
       )
    ))

(defn charEquals [ch1 ch2]
  (and (= ch1 ch2) ch1))

(defn commonstring [s1 s2]
  (apply str (remove #(= % false) (map charEquals s1 s2)))
)

(defn doPairs [pairs]
  (map #(commonstring (first %) (second %)) pairs)
)

(defn part2 [ids]
  (let [pairs (combo/combinations ids 2)]
    (apply max-key count (doPairs pairs))
  ))


(defn part2Solution []
  (part2
    (clojure.string/split-lines
      (slurp "day2input.txt")
      )
    ))

(assert ( = 12 (part1  ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])))
(assert (= nil (hasNs 3 "abcdef")))
(assert (not= nil (hasNs 3 "bababc")))
(assert (= nil (hasNs 3 "abbcde")))
(assert (not= nil (hasNs 3 "abcccd")))
(assert (= nil (hasNs 3 "aabcdd")))
(assert (= nil (hasNs 3 "abcdee")))
(assert (not= nil (hasNs 3 "ababab")))

(println (part1Solution))
(assert (= 8398 (part1Solution)))

(assert (= "abc" (commonstring "abc" "abc")))
(assert (= "ab" (commonstring "abc" "abd")))

(assert (= "fgij" (part2 ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])))

(println (part2Solution))
(assert (= "hhvsdkatysmiqjxunezgwcdpr" (part2Solution)))

