(ns adventofcode2018.aoc18.day1)

(defn part1 [freqChanges]
    (reduce + freqChanges)
)

(defn part2 [freqChanges]
  (loop [freqChanges (cycle freqChanges) currentValue 0 cumulatives #{0}]
    (let [nextValue (+ currentValue (first freqChanges))]
      (if (contains? cumulatives nextValue)
        nextValue
        (recur (rest freqChanges) nextValue (conj cumulatives nextValue))
        )
      )
    )
  )

(defn partNSolution [partN]
  (partN
    (map read-string
    (clojure.string/split-lines
      (slurp "day1input.txt")
    ))
))

(assert  (= 3 (part1 [1 1 1])))
(assert (= 0 (part1 [1 1 -2])))
(assert (= -6 (part1 [-1 -2 -3])))
(println (partNSolution part1))

(assert (= 2 (part2 [1 -2 3 1])))
(assert (= 0 (part2 [1 -1])))
(assert (= 10 (part2 [3 3 4 -2 -4])))
(assert (= 5 (part2 [-6 3 8 5 -6])))
(assert (= 14 (part2 [7 7 -2 -7 -4])))

(println (partNSolution part2))
(assert (= 448 (partNSolution part2 )))
