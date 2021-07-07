(ns tictactoe
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

;; Game pieces
(def ^:const blank -5)
(def ^:const X 1)
(def ^:const O 2)

(def blank-board [[blank blank blank]
                  [blank blank blank]
                  [blank blank blank]])

(def game (atom {:board blank-board
                 :next-to-play X}))

(defn blank? [val]
  (= blank val))

(defn isX? [val]
  (= X val))

(defn isO? [val]
  (= O val))

(defn to-str [x]
  (case x
    -5 " "
    1 "X"
    2 "O"))

(defn l-diagsum [m]
  (let [size (count m)
        idxs (range size)
        values (map #(-> (nth m %) (nth %)) idxs)
        sum (reduce + values)]
    sum))

(defn r-diagsum [m]
  (let [size (count m)
        idxs (range size)]
    (loop [values []
           o-idx (first idxs)
           in-idx (last idxs)]
      (if (= (count values) (count m))
        (reduce + values)
        (do
          (def value (-> (nth m o-idx) (nth in-idx)))
          (recur (conj values value) (+ o-idx 1) (- in-idx 1)))))))

(defn sumvec [x]
  (reduce + x))

(defn transpose [m]
  (apply mapv vector m))

(defn rowsums [m]
  (mapv sumvec m))

(defn colsums [m]
  (mapv sumvec (transpose m)))

(defn and-the-winner-is [m]
  (let [sums (concat (rowsums m) (colsums m) [(l-diagsum m)] [(r-diagsum m)])
        Xwin (some #(= 3 %) sums)
        Owin (some #(= 6 %) sums)
        draw (= nil (some #(= blank %) (flatten m)))]
    (cond
      Xwin X
      Owin O
      draw "draw"
      :else false)))

(defn build-row [row]
  (apply str (to-str (first row)) "|" (to-str (nth row 1)) "|" (to-str (last row))))

(defn divider []
  (str "-+-+-"))

(defn draw-board [board]
  (let [row1 (build-row (nth board 0))
        row2 (build-row (nth board 1))
        row3 (build-row (nth board 2))
        out [row1 (divider) row2 (divider) row3]]
    (println (str/join "\n" out))))

(defn unflatten [x]
  [(subvec x 0 3)
   (subvec x 3 6)
   (subvec x 6 9)])

(defn update-board! [x pos]
  (let [v (vec (flatten (@game :board)))
        z (assoc v (- pos 1) x)]
    (swap! game assoc :board (unflatten z))))

(defn in-range? [pos]
  (and (> pos 0) (< pos 10)))

(defn pos-blank? [pos]
  (let [v (flatten (@game :board))
        x (nth v (- pos 1))
        blank? (= -5 x)]
    blank?))

(defn get-valid-input []
  (loop [x (read-string (read-line))]
    (cond
      (not (in-range? x)) (do
                            (println "You must pick a value 1-9:")
                            (recur (read-string (read-line))))
      (not (pos-blank? x)) (do
                       (println "This position is already filled. Try again:")
                       (recur (read-string (read-line))))
      :else x)))

(defn X-get-move []
  (println "X's move:")
  (get-valid-input))

(defn O-get-move []
  (println "O's move:")
  (get-valid-input))

(defn O-get-computer-move []
  (rand-nth (range 1 10)))

(defn get-next-to-play []
  (@game :next-to-play))

(defn set-next-to-play! [x]
  (swap! game assoc :next-to-play x))

(defn clear-screen []
  (print (str (char 27) "[2J")) ; clears screen
  (print (str (char 27) "[;H")) ; put cursor in upper left
  )

(defn -main []
  (clear-screen)
  (println "Get ready to play!")
  (while (= false (and-the-winner-is (@game :board)))
    (draw-board (@game :board))
    (case (get-next-to-play)
      1 (do
          (update-board! X (X-get-move))
          (set-next-to-play! O))
      2 (do
          (update-board! O (O-get-computer-move))
          (set-next-to-play! X)))
      (clear-screen))
  (case (and-the-winner-is (@game :board))
    1 (println "X wins!")
    2 (println "O wins!")
    "draw" (println "It's a draw!"))
  (draw-board (@game :board)))
