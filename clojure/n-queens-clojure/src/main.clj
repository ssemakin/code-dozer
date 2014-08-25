
; Porting n-queens problem from Scala (Programming Scala, 2nd edition) to Clojure.
; (for) construction was deliberately avoided.

(defn in-check [q1 q2]
  (or
    (= (q1 0) (q2 0))
    (= (q1 1) (q2 1))
    (= (Math/abs (- (q1 0) (q2 0))) (Math/abs (- (q1 1) (q2 1))))
    )
  )

(defn safe? [queen queens]
  (every? #(not (in-check queen %)) queens))

(defn find-queens [n]
  (defn placeQueens [k]
    (if (= k 0) [[]]
      (loop [queens (placeQueens (dec k)), result []]
        (if (first queens)
          (let [columns (map #(eval [k %]) (range 1 (inc n)))
                safe-queens (filter #(safe? % (first queens)) columns)
                all-queens (map #(cons % (first queens)) safe-queens)]
            (recur (rest queens) (concat all-queens result)))
          result
          )
        )
      )
    )
  (placeQueens n)
  )

(defn print-solutions [solutions]
  (if (empty? solutions)
    (println "\tSorry, none.")
    (loop [s solutions]
      (when (first s)
        (println (first s))
        (recur (rest s)))
      )
    )
  )

;(println (safe? [1 2] [[1 1]]))
(println "This is the solutions found:")
(print-solutions (find-queens 4))
