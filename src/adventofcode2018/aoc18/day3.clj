(ns adventofcode2018.aoc18.day3
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as ppr])
  (:use clojure.test))

(defn getNum [splitLine index]
     (read-string (nth splitLine index)))

(defn allCoordsInRectangle [line]
  (let [splitLine (str/split line #"[,:x ]")
        x1 (getNum splitLine 2) x2 (+ x1 (getNum splitLine 5))
        y1 (getNum splitLine 3) y2 (+ y1 (getNum splitLine 6))]

    (for [y (range y1 y2) x (range x1 x2)]
      [x y])
    )
  )

(defn allRectangles [lines]
  (apply concat
    (map allCoordsInRectangle lines)
    )
  )

(defn addToSeen [seen element]
  (update seen element inc)
  )

(defn convertCoordsToMap [coords]
  (apply hash-map (interleave coords (repeat 0)))
  )

(defn countClaims [lines]
    (loop [coords (allRectangles lines)
           seen (convertCoordsToMap coords)]

      (if (empty? coords)
        seen
        (recur (drop 1 coords)
               (addToSeen seen (first coords)))
        )
    )
  )

(defn overlappingClaims [lines]
  (into {} (filter #( > (second %) 1) (countClaims lines)))
  )

(defn countOverlappingClaims [lines]
  (count (overlappingClaims lines)))

(defn part1 []
  (countOverlappingClaims
    (clojure.string/split-lines
      (slurp "day3input.txt")
      )
    )
  )

(is (= '([1 3] [2 3] [3 3] [4 3]
         [1 4] [2 4] [3 4] [4 4]
         [1 5] [2 5] [3 5] [4 5]
         [1 6] [2 6] [3 6] [4 6]
         ) (allCoordsInRectangle "#1 @ 1,3: 4x4")))

(is (= '([1 3] [2 3] [3 3] [4 3]
             [1 4] [2 4] [3 4] [4 4]
             [1 5] [2 5] [3 5] [4 5]
             [1 6] [2 6] [3 6] [4 6]
             [2 1] [3 1] [2 2] [3 2]
             ) (allRectangles ["#1 @ 1,3: 4x4" "#1 @ 2,1: 2x2"])))

(is (= (countClaims ["#1 @ 1,3: 4x4" "#1 @ 4,4: 2x2"])
       {[4 3] 1, [2 3] 1, [2 5] 1, [3 3] 1, [5 4] 1, [3 4] 1, [4 6] 1, [1 4] 1, [1 3] 1, [1 5] 1, [5 5] 1, [2 4] 1, [3 6] 1, [4 5] 2, [1 6] 1, [4 4] 2, [2 6] 1, [3 5] 1}))

(is (= (overlappingClaims ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])
       {[3 3] 2 [4 3] 2 [3 4]2 [4 4] 2}))

(is (= (countOverlappingClaims ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])
       4))
(is (= (part1) 120408))
