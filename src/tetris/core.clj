(ns tetris.core
  (:require [clojure.string :as s]))

;; -------------------------------------------------------------------------------------------
;; ----------------------------------------- GLOBALS -----------------------------------------
;; -------------------------------------------------------------------------------------------

;; The player's score.
(def SCORE (atom 0))
(def CLEARED-LINES (atom 0))

;; Our playfield.
(def MATRIX (atom []))

;; The active tetris piece that the player moves.
;; Possible rotations: CENTER, ROTATE1, ROTATE2, ROTATE3
(def ACTIVE-PIECE (atom {:id "" :rotation :CENTER :row 0 :col 0 :graphics []}))

;; All possible tetris pieces and their spawning locations.
(def START-POSITIONS
  {"I" [0 3]
   "O" [0 4]
   "Z" [0 3]
   "S" [0 3]
   "J" [0 3]
   "L" [0 3]
   "T" [0 3]})

;; All the glorious graphics in our game:
(def EMPTY-LINE ["." "." "." "." "." "." "." "." "." "."])
(def PIECES-NORMAL
  {"I" [["." "." "." "."]
        ["c" "c" "c" "c"]
        ["." "." "." "."]
        ["." "." "." "."]]
   "O" [["y" "y"]
        ["y" "y"]]
   "Z" [["r" "r" "."]
        ["." "r" "r"]
        ["." "." "."]]
   "S" [["." "g" "g"]
        ["g" "g" "."]
        ["." "." "."]]
   "J" [["b" "." "."]
        ["b" "b" "b"]
        ["." "." "."]]
   "L" [["." "." "o"]
        ["o" "o" "o"]
        ["." "." "."]]
   "T" [["." "m" "."]
        ["m" "m" "m"]
        ["." "." "."]]})

(def PIECES-ROTATED1
  {"I" [["." "." "c" "."]
        ["." "." "c" "."]
        ["." "." "c" "."]
        ["." "." "c" "."]]
   "O" [["y" "y"]
        ["y" "y"]]
   "Z" [["." "." "r"]
        ["." "r" "r"]
        ["." "r" "."]]
   "S" [["." "g" "."]
        ["." "g" "g"]
        ["." "." "g"]]
   "J" [["." "b" "b"]
        ["." "b" "."]
        ["." "b" "."]]
   "L" [["." "o" "."]
        ["." "o" "."]
        ["." "o" "o"]]
   "T" [["." "m" "."]
        ["." "m" "m"]
        ["." "m" "."]]})

(def PIECES-ROTATED2
  {"I" [["." "." "." "."]
        ["." "." "." "."]
        ["c" "c" "c" "c"]
        ["." "." "." "."]]
   "O" [["y" "y"]
        ["y" "y"]]
   "Z" [["." "." "."]
        ["r" "r" "."]
        ["." "r" "r"]]
   "S" [["." "." "."]
        ["." "g" "g"]
        ["g" "g" "."]]
   "J" [["." "." "."]
        ["b" "b" "b"]
        ["." "." "b"]]
   "L" [["." "." "."]
        ["o" "o" "o"]
        ["o" "." "."]]
   "T" [["." "." "."]
        ["m" "m" "m"]
        ["." "m" "."]]})

(def PIECES-ROTATED3
  {"I" [["." "c" "." "."]
        ["." "c" "." "."]
        ["." "c" "." "."]
        ["." "c" "." "."]]
   "O" [["y" "y"]
         ["y" "y"]]
   "Z" [["." "r" "."]
        ["r" "r" "."]
        ["r" "." "."]]
   "S" [["g" "." "."]
        ["g" "g" "."]
        ["." "g" "."]]
   "J" [["." "b" "."]
        ["." "b" "."]
        ["b" "b" "."]]
   "L" [["o" "o" "."]
        ["." "o" "."]
        ["." "o" "."]]
   "T" [["." "m" "."]
        ["m" "m" "."]
        ["." "m" "."]]})


;; -------------------------------------------------------------------------------------------
;; ------------------------------------ Create playfield -------------------------------------
;; -------------------------------------------------------------------------------------------

(defn get-empty-matrix []
  (into [] (take 22 (repeat EMPTY-LINE))))

(defn print-matrix! [matrix]
  (flush)
  (if (empty? matrix) (recur (get-empty-matrix))
    (println (s/join "\n" (map #(s/join " " %) matrix)))))

(defn read-matrix! []
  (into []
        (take 22 (repeatedly #(s/split (read-line) #" ")))))

(defn clear-matrix! []
  (swap! MATRIX (fn [m] (get-empty-matrix))))

;; -------------------------------------------------------------------------------------------
;; ------------------------------------ Draw tetris piece ------------------------------------
;; -------------------------------------------------------------------------------------------

(defn get-graphics
  [piece-id rotation]
  (cond
    (= :CENTER rotation) (get PIECES-NORMAL piece-id)
    (= :ROTATE1 rotation) (get PIECES-ROTATED1 piece-id)
    (= :ROTATE2 rotation) (get PIECES-ROTATED2 piece-id)
    (= :ROTATE3 rotation) (get PIECES-ROTATED3 piece-id)))

(defn set-active-piece!
  ([id] (set-active-piece! id :CENTER (START-POSITIONS id)))
  ([id [row col]] (set-active-piece! id (:rotation @ACTIVE-PIECE) [row col]))
  ([id rotation [row col]]
   (swap!
     ACTIVE-PIECE
     (fn [p] {:id id
              :rotation rotation
              :row row
              :col col
              :graphics (get-graphics id rotation)}))))

(defn clean-rows
  "Removes from the piece any rows that contain only empty spaces."
  [piece-graphics]
  (let [top-empty-rows (take-while (fn [row] (every? #(= "." %) row)) piece-graphics)
        full-rows (filter (fn [row] (some #(not= "." %) row)) piece-graphics)]
    (into [] (concat top-empty-rows full-rows))))

(defn flip-row
  "Selects a single row from a piece graphics from rows to cols representation."
  [piece-graphics col]
  (into [] (map #(nth % col) piece-graphics)))

(defn flip-all-rows
  [piece-graphics]
  (vec
    (for [i (range (count (first piece-graphics)))
          :let [col (flip-row piece-graphics i)]]
      col)))

;; NOTE: Calling flip-all-rows twice, basically flips the rows/cols representation back into it's original form.
(defn clean-cols
  "Removes from the piece any cols that contain only empty spaces."
  [piece-graphics]
  (flip-all-rows
    (filter
      (fn [row] (some #(not= "." %) row))
      (flip-all-rows piece-graphics))))

(defn clean-piece
  "Removes any empty rows and cols from a piece."
  [piece-graphics]
  (clean-cols (clean-rows piece-graphics)))

(defn insert-piece-row [piece-row matrix-row position]
  (let [row-size (count matrix-row)
        end-position (+ position (count piece-row))
        leading-space (take-while #(= "." %) piece-row)
        trailing-space (drop-while #(not= "." %) (drop-while #(= "." %) piece-row))
        before-piece (subvec matrix-row 0 (+ position (count leading-space)))
        piece (filter #(not= "." %) piece-row)
        after-piece (subvec matrix-row (- end-position (count trailing-space)))]
    (into []
          (concat
            before-piece piece after-piece))))

(defn upcase-matrix [matrix]
  (mapv #(mapv s/upper-case %) matrix))

(defn downcase-matrix [matrix]
  (mapv #(mapv s/lower-case %) matrix))

;; Throws an IndexOutOfBoundsException, if the row/col are outside the matrix.
;; NOTE: (map #(map s/upper-case %) piece) - Need the double map, since the piece is represented as a vector of vectors.
(defn insert-piece [piece matrix row col]
  (let [piece (clean-piece piece)
        num-piece-rows (count piece)
        rows-before-piece (subvec matrix 0 row)
        rows-for-piece (subvec matrix row (+ row num-piece-rows))
        rows-after-piece (subvec matrix (+ num-piece-rows row))
        piece-upcased (upcase-matrix piece)
        piece-inserted (map #(insert-piece-row %1 %2 col) piece-upcased rows-for-piece)]
    (into [] (concat rows-before-piece piece-inserted rows-after-piece))))

;; -------------------------------------------------------------------------------------------
;; ------------------------------------ Collision handling -----------------------------------
;; -------------------------------------------------------------------------------------------

(defn get-bottom-collisions
  "Detects all bottom collision on a single piece row."
  [piece-row move-row]
  (map #(and (not= "." %1) (not= "." %2)) piece-row move-row))

(defn get-side-collision
  "Returns true if there is a side collision for a single row."
  [piece-row move-row piece-col move-col]
  (and (not= "." (get piece-row piece-col)) (not= "." (get move-row move-col))))

(defn get-side-collisions
  "Returns a seq of true/false for every side collision of the current piece at the coords the player is trying to move."
  [piece-rows move-rows piece-col move-col]
  (map #(get-side-collision %1 %2 piece-col move-col) piece-rows move-rows))

(defn count-collisions
  [collision-vector]
  (count (filter #(.contains % true) collision-vector)))

(defn detect-collisions
  "Returns true if the active tetris piece will collide with anything in the matrix at the given coordinates."
  [move-row move-col rotation-graphics]
  (let [piece-height (count (clean-piece rotation-graphics))
        piece-row (:row @ACTIVE-PIECE)
        piece-col (:col @ACTIVE-PIECE)
        portrait (insert-piece rotation-graphics (get-empty-matrix) piece-row piece-col)
        piece-slice (subvec portrait piece-row (+ piece-row piece-height))
        move-slice (subvec @MATRIX move-row (+ move-row piece-height))
        bottom-collisions (map #(get-bottom-collisions %1 %2) piece-slice move-slice)]
    (cond
      (> (count-collisions bottom-collisions) 0) :bottom-collison
      (> (count-collisions [(get-side-collisions piece-slice move-slice piece-col move-col)]) 0) :side-collison
      :else :no-collision)))

;; -------------------------------------------------------------------------------------------
;; ------------------------------------ Rotation handling ------------------------------------
;; -------------------------------------------------------------------------------------------

(defn check-bounds
  "Takes the row and col of where the player is trying to move and checks if they are within the matrix."
  [move-x move-y piece-graphics matrix]
  (let [piece-cleaned (clean-piece piece-graphics)
        piece-width (count (last (sort-by count piece-cleaned)))
        piece-height (count piece-cleaned)
        y-limit (- (count EMPTY-LINE) piece-width)
        x-limit (- (count matrix) piece-height)]
    (cond
      (< move-y 0) :side-collison
      (> move-y y-limit) :side-collison
      (> move-x x-limit) :bottom-collison
      :else :in-bounds)))

(defn rotate-piece! [rotation]
  (let [current-id (:id @ACTIVE-PIECE)
        current-row (:row @ACTIVE-PIECE)
        current-col (:col @ACTIVE-PIECE)
        new-rotation (get-graphics current-id rotation)]
    (if (and (= :in-bounds (check-bounds current-row current-col new-rotation @MATRIX))
             (= :no-collision (detect-collisions current-row current-col new-rotation)))
      (swap!
        ACTIVE-PIECE
        (fn [p]
          {:id current-id
           :rotation rotation
           :row current-row
           :col current-col
           :graphics new-rotation}))
      :cant-rotate)))

(defn rotate-left! []
  (let [current-rotation (:rotation @ACTIVE-PIECE)]
   (cond
     (= :CENTER current-rotation) (rotate-piece! :ROTATE3)
     (= :ROTATE3 current-rotation) (rotate-piece! :ROTATE2)
     (= :ROTATE2 current-rotation) (rotate-piece! :ROTATE1)
     :else (rotate-piece! :CENTER))))

(defn rotate-right! []
  (let [current-rotation (:rotation @ACTIVE-PIECE)]
    (cond
      (= :CENTER current-rotation) (rotate-piece! :ROTATE1)
      (= :ROTATE1 current-rotation) (rotate-piece! :ROTATE2)
      (= :ROTATE2 current-rotation) (rotate-piece! :ROTATE3)
      :else (rotate-piece! :CENTER))))

;; -------------------------------------------------------------------------------------------
;; ------------------------------------ Move tetris piece ------------------------------------
;; -------------------------------------------------------------------------------------------

(defn update-playfield! []
  (swap! MATRIX
         #(downcase-matrix
            (insert-piece (:graphics @ACTIVE-PIECE) % (:row @ACTIVE-PIECE) (:col @ACTIVE-PIECE)))))

(defn move-active-piece! [& {:keys [x y]
                             :or {x (:row @ACTIVE-PIECE)
                                  y (:col @ACTIVE-PIECE)}}]
  (if (= :in-bounds (check-bounds x y (:graphics @ACTIVE-PIECE) @MATRIX))
    (set-active-piece! (:id @ACTIVE-PIECE) [x y])
    :out-of-bounds))

(defn move-left!
  "Allows the player to move the current active piece to the left."
  []
  (let [new-x (:row @ACTIVE-PIECE)
        new-y (dec (:col @ACTIVE-PIECE))]
    (cond
      (not= :in-bounds (check-bounds new-x new-y (:graphics @ACTIVE-PIECE) @MATRIX)) :out-of-bouns
      (= :side-collison (detect-collisions new-x new-y (:graphics @ACTIVE-PIECE))) :cant-move-there
      :else (move-active-piece! :x new-x :y new-y))))

(defn move-right!
  "Allows the player to move the current active piece to the right."
  []
  (let [new-x (:row @ACTIVE-PIECE)
        new-y (inc (:col @ACTIVE-PIECE))]
    (cond
      (not= :in-bounds (check-bounds new-x new-y (:graphics @ACTIVE-PIECE) @MATRIX)) :out-of-bouns
      (= :side-collison (detect-collisions new-x new-y (:graphics @ACTIVE-PIECE))) :cant-move-there
      :else (move-active-piece! :x new-x :y new-y))))

(defn move-down!
  "Moves to move the current active tetris piece one step down."
  []
  (let [new-x (inc (:row @ACTIVE-PIECE))
        new-y (:col @ACTIVE-PIECE)]
    (cond
      (not= :in-bounds (check-bounds new-x new-y (:graphics @ACTIVE-PIECE) @MATRIX)) (update-playfield!)
      (= :bottom-collison (detect-collisions new-x new-y (:graphics @ACTIVE-PIECE))) (update-playfield!)
      :else (move-active-piece! :x new-x :y new-y))))

(defn hard-drop!
  "Drops the current tetris piece all the way to the bottom.
  Since this operation cannot be undone, the playfield is left permanently changed."
  []
  (dotimes [i (count @MATRIX)]
    (move-down!))
  (update-playfield!))

;; -------------------------------------------------------------------------------------------
;; ---------------------------------------- Game loop ----------------------------------------
;; -------------------------------------------------------------------------------------------

(defn get-filled-lines
  "Returns any lines with no empty spaces in them."
  [matrix]
  (filter #(not (.contains % ".")) matrix))

(defn clear-filled-lines
  "Well, if the player has filled any lines, we have to unfill them."
  [matrix]
  (let [num-cleared-lines (count (get-filled-lines @MATRIX))
        matrix-cleared (remove #(not (.contains % ".")) matrix)]
    (concat
      (take num-cleared-lines (repeat EMPTY-LINE))
      matrix-cleared)))

(defn game-over?
  "Returns true if the player has reached the top level of the matrix, thus losing the game."
  []
  (some #(not= "." %) (first @MATRIX)))

(defn step!
  "Perform the next step in the game (if the player cleared a line, count the score and stuff)"
  []
  (let [num-cleared-lines (count (get-filled-lines @MATRIX))]
    (when (> num-cleared-lines 0)
      (swap! MATRIX #(clear-filled-lines %))
      (swap! CLEARED-LINES #(+ num-cleared-lines %))
      (swap! SCORE #(+ (* 100 num-cleared-lines) %)))))

(defn clear-screen!
  "Clear the console window."
  []
  (-> (new ProcessBuilder (list "cmd" "/c" "cls")) (.inheritIO) (.start) (.waitFor)))

(defn do-command! [command-str]
  (cond
    (.contains command-str " ") (doseq [command (s/split command-str #" ")]
                                  (do-command! command))
    (= "q" command-str) (System/exit 0)
    (= "p" command-str) (print-matrix! @MATRIX)
    (= "P" command-str) (print-matrix!
                         (insert-piece (:graphics @ACTIVE-PIECE) @MATRIX (:row @ACTIVE-PIECE) (:col @ACTIVE-PIECE)))
    (= "g" command-str) (swap! MATRIX (fn [m] (read-matrix!)))
    (= "c" command-str) (clear-matrix!)
    (= "C" command-str) (clear-screen!)
    (= "?s" command-str) (println @SCORE)
    (= "?n" command-str) (println @CLEARED-LINES)
    (= ";" command-str) (print "\n")
    (= "s" command-str) (step!)
    (= "I" command-str) (set-active-piece! "I")
    (= "O" command-str) (set-active-piece! "O")
    (= "Z" command-str) (set-active-piece! "Z")
    (= "S" command-str) (set-active-piece! "S")
    (= "J" command-str) (set-active-piece! "J")
    (= "L" command-str) (set-active-piece! "L")
    (= "T" command-str) (set-active-piece! "T")
    (= "t" command-str) (print-matrix! (:graphics @ACTIVE-PIECE))
    (= ")" command-str) (rotate-right!)
    (= "(" command-str) (rotate-left!)
    (= "<" command-str) (move-left!)
    (= ">" command-str) (move-right!)
    (= "v" command-str) (move-down!)
    (= "V" command-str) (hard-drop!)
    :else (doseq [command (s/split command-str #"")] (do-command! command))))

(defn -main []
  (let [command (read-line)]
    (do-command! command))
  (recur))
