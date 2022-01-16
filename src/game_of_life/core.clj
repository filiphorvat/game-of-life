(ns game-of-life.core
  (:gen-class))
(require '[com.climate.claypoole :as cp])

(defn random-chance
  [chance]
  (if (< (rand-int 100) chance)
    1
    0))

(defn rand-vec
  [n chance]
  (vec (take n (repeatedly #(random-chance chance))))
  )

(defn rand-vec-2d
  [n chance]
  (vec (take n (repeatedly #(rand-vec n chance))))
  )

(def indexes [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn normalize
  [num n]
  (condp = num
    -1 (- n 1)
    n 0
    num))

(defn get-relative-indexes
  [i j n]
  (for [[x y] indexes]
    [ (normalize (+ x i) n) (normalize (+ y j) n)]))

(defn get-neighbours
  [matrix i j]
  (for
    [[x y] (get-relative-indexes i j (count matrix))]
    (get-in matrix [x y])
    ))

(defn neighbour-count
  [neighbours]
  (reduce + neighbours))

(defn print-matrix
  [matrix]
  (doseq [row matrix] (println row)))

(defn next-cell-state
  [current-state neighbour-count]
  (case current-state
    0 (case neighbour-count
        3 1
        0)
    1 (case neighbour-count
        2 1
        3 1
        0))
  )

(defn next-row-state
  [matrix i]
  (vec
    (map-indexed
      (fn [j cell]
        (next-cell-state
          cell
          (neighbour-count
            (get-neighbours matrix i j))))
      (nth matrix i))))


(defn next-state
  [matrix]
  (vec
    (for [i (take (count matrix) (iterate inc 0))]
      (next-row-state matrix i))))

(defn next-state2
  [matrix]
  (vec
    (cp/pfor
      (cp/threadpool 12)
      [i (take (count matrix) (iterate inc 0))]
      (next-row-state matrix i))))

(defn -main
  [& args]
)


(def matrix (rand-vec-2d 2000 40))
(time
  (dotimes [_ 5]
    (def matrix (next-state matrix))))

(time
  (dotimes [_ 5]
    (def matrix (next-state2 matrix))))
(println "Done")

(shutdown-agents)