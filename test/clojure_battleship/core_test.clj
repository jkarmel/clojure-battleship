(ns clojure-battleship.core-test
  (:require [clojure.test :refer :all]
            [clojure-battleship.core :refer :all]))


(deftest test-create-board
  (let [board (create-board 3 2)]
    (do
      (testing "create-board"
        (is (= (count board) 3))
        (is (= (count (first board)) 2)))

      (testing "board-map"
        (is (=
             (board-map (fn [coords cell] [(coords :x) (coords :y)]) board)
             [
              [[0 0] [1 0]]
              [[0 1] [1 1]]
              [[0 2] [1 2]]
              ])))

      (testing "ship-on-space-down"
        (is (not (ship-on-space-down 3 {:x 1 :y 1} {:x 1 :y 0} )))
        (is (ship-on-space-down 3 {:x 1 :y 1} {:x 1 :y 1} ))
        (is (ship-on-space-down 3 {:x 1 :y 1} {:x 1 :y 3} ))
        (is (not (ship-on-space-down 3 {:x 1 :y 1} {:x 1 :y 4})))
        (is (not (ship-on-space-down 3 {:x 2 :y 1} {:x 1 :y 1})))

      (testing "the game"
        (let [board-with-ship (place-ship 2 {:x 1 :y 1} "down" board)
              board-with-hit (mark-hit {:x 1 :y 1} board-with-ship)]
          (testing "place ship"
            (is (= board-with-ship [
                                [nil nil]
                                [nil   1]
                                [nil   1]
                                ]))
            (is (= (place-ship 2 {:x 0 :y 0} "right" board-with-ship) [
                                                               [2 2]
                                                               [nil 1]
                                                               [nil 1]
                                                               ])))
          (testing "fire"
            (is ((fire {:x 1 :y 1} board-with-ship) :hit))
            (is (not ((fire {:x 1 :y 1} board-with-ship) :sunk)))
            (is ((fire {:x 1 :y 2} board-with-hit) :sunk))
            (is ((fire {:x 1 :y 2} board-with-hit) :win)))))))))
