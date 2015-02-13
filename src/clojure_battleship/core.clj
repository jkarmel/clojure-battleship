(ns clojure-battleship.core)

(defn create-board [height width]
  (repeat height (repeat width nil)))

(defn print-board [board]
  (doall (map println board)))

(defn board-map [f board]
  (map-indexed
    (fn [y row]
      (map-indexed (fn [x cell] (f {:x x :y y} cell)) row)) board))

(defn ship-on-space-down [size {ship-x :x ship-y :y} {board-x :x board-y :y}]
  (and
    (= ship-x board-x)
    (< (- board-y ship-y) size)
    (>= board-y ship-y)))

(defn ship-on-space-right [size {ship-x :x ship-y :y} {board-x :x board-y :y}]
  (and
    (= ship-y board-y)
    (< (- board-x ship-x) size)
    (>= board-x ship-x)))

(defn ships [board]
  (set (filter number? (flatten board))))

(defn next-ship-num [board]
  (if (empty? (ships board))
    1
    (inc (apply max (seq (ships board))))))

(defn place-ship [size ship-start-coords down-or-right board]
  (let [ship-on-space (if (= down-or-right "down")
                        ship-on-space-down
                        ship-on-space-right)]
    (board-map
      (fn [board-coords cell]
        (if (ship-on-space size ship-start-coords board-coords)
          (next-ship-num board)
          cell))
      board)))

(defn mark-hit [coords board]
  (board-map
    (fn [board-coords cell] (if (= coords board-coords) 'x cell))
    board))

(defn fire [coords board]
  (let [new-board (mark-hit coords board)
        count-ship-spaces (fn [board] (->> board flatten (filter number?) count))]
    {
     :board new-board
     :hit  (not (= (count-ship-spaces board) (count-ship-spaces new-board)))
     :sunk (not (= (count (ships board)) (count (ships new-board))))
     :win  (zero? (count (ships new-board)))
    }))
