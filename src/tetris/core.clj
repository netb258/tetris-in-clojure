(ns tetris.core
  (:require [clojure.string :as s]
            [lanterna.screen :as console])
  (:gen-class))

;; -------------------------------------------------------------------------------------------
;; ----------------------------------------- GLOBALS -----------------------------------------
;; -------------------------------------------------------------------------------------------

;; The window that will hold our game.
(def DISPLAY (console/get-screen :swing {:rows 22 :cols 20}))
(def HIGH-SCORE-FILE "./score.dat")

;; The player's score.
(def SCORE (atom 0))
(def CLEARED-LINES (atom 0))

(def GAME-SPEED (atom 600))
(def MAX-SPEED 100)

;; Our playfield.
(def MATRIX (atom []))

;; The active tetris piece that the player moves.
;; Possible rotations: CENTER, ROTATE1, ROTATE2, ROTATE3
(def ACTIVE-PIECE (atom {:id "" :rotation :CENTER :row 0 :col 0 :anchored false :graphics []}))

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

(defn print-line!
  "A custom printing function for our swing console."
  [text lnum]
  (console/put-string DISPLAY 0 lnum text))

(defn clear-screen!
  "Clear the console window."
  []
  (console/redraw DISPLAY))

(defn get-empty-matrix []
  (into [] (take 22 (repeat EMPTY-LINE))))

(defn print-matrix! [matrix]
  (flush)
  (if (empty? matrix) (recur (get-empty-matrix))
    (let [lines (map #(s/join " " %) matrix)]
      (doseq [[line i] (map list lines (range (count lines)))]
        (print-line! line i)))))

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
              :anchored false
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

(defn get-collisions
  "Detects all bottom collision on a single piece row."
  [piece-row move-row]
  (map #(and (not= "." %1) (not= "." %2)) piece-row move-row))

(defn count-collisions
  [collision-vector]
  (count (filter #(.contains % true) collision-vector)))

(defn detect-collision
  "Returns true if the active tetris piece will collide with anything in the matrix at the given coordinates."
  [move-row move-col rotation-graphics]
  (let [piece-height (count (clean-piece rotation-graphics))
        portrait (insert-piece rotation-graphics (get-empty-matrix) move-row move-col)
        piece-slice (subvec portrait move-row (+ move-row piece-height))
        move-slice (subvec @MATRIX move-row (+ move-row piece-height))
        collisions (map #(get-collisions %1 %2) piece-slice move-slice)]
    (if
      (> (count-collisions collisions) 0) :collision
      :no-collision)))

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
             (= :no-collision (detect-collision current-row current-col new-rotation)))
      (swap!
        ACTIVE-PIECE
        (fn [p]
          {:id current-id
           :rotation rotation
           :row current-row
           :col current-col
           :anchored false
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
            (insert-piece (:graphics @ACTIVE-PIECE) % (:row @ACTIVE-PIECE) (:col @ACTIVE-PIECE))))
  (swap! ACTIVE-PIECE #(assoc % :anchored true)))

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
      (= :collision (detect-collision new-x new-y (:graphics @ACTIVE-PIECE))) :cant-move-there
      :else (move-active-piece! :x new-x :y new-y))))

(defn move-right!
  "Allows the player to move the current active piece to the right."
  []
  (let [new-x (:row @ACTIVE-PIECE)
        new-y (inc (:col @ACTIVE-PIECE))]
    (cond
      (not= :in-bounds (check-bounds new-x new-y (:graphics @ACTIVE-PIECE) @MATRIX)) :out-of-bouns
      (= :collision (detect-collision new-x new-y (:graphics @ACTIVE-PIECE))) :cant-move-there
      :else (move-active-piece! :x new-x :y new-y))))

(defn move-down!
  "Moves to move the current active tetris piece one step down."
  []
  (let [new-x (inc (:row @ACTIVE-PIECE))
        new-y (:col @ACTIVE-PIECE)]
    (cond
      (not= :in-bounds (check-bounds new-x new-y (:graphics @ACTIVE-PIECE) @MATRIX)) (update-playfield!)
      (= :collision (detect-collision new-x new-y (:graphics @ACTIVE-PIECE))) (update-playfield!)
      :else (move-active-piece! :x new-x :y new-y))))

;; -------------------------------------------------------------------------------------------
;; ---------------------------------------- Game loop ----------------------------------------
;; -------------------------------------------------------------------------------------------

(defn choose-new-piece! []
  (let [new-piece (first (shuffle ["I" "O" "Z" "S" "J" "L" "T"]))]
    (set-active-piece! new-piece)))

(defn get-filled-lines
  "Returns any lines with no empty spaces in them."
  [matrix]
  (filter #(not (.contains % ".")) matrix))

(defn clear-filled-lines
  "Well, if the player has filled any lines, we have to unfill them."
  [matrix]
  (let [num-cleared-lines (count (get-filled-lines @MATRIX))
        matrix-cleared (into [] (remove #(not (.contains % ".")) matrix))]
    (into []
          (concat
            (take num-cleared-lines (repeat EMPTY-LINE))
            matrix-cleared))))

(defn game-over?
  "Returns true if the player has reached the top level of the matrix, thus losing the game."
  []
  (some #(not= "." %) (first @MATRIX)))

(defn quit-game! []
  (System/exit 0))

(defn read-high-score
  "Reads the players best score from a file"
  [fname]
  (read-string (slurp fname)))

(defn save-high-score
  "Saves the players score to a file."
  [fname]
  (spit fname (with-out-str (pr @SCORE))))

(defn overwrite-high-score! []
  (let [high-score (read-high-score HIGH-SCORE-FILE)]
    (when (> @SCORE high-score)
      (save-high-score HIGH-SCORE-FILE))))

(defn game-over!
  "Shows the game over message and exits when the player presses ESC key."
  []
  (overwrite-high-score!)
  (print-line! "**** GAME OVER ****" 0)
  (print-line! (str "SCORE - " @SCORE) 1)
  (print-line! (str "HIGH SCORE - " (read-high-score HIGH-SCORE-FILE)) 2)
  (clear-screen!)
  (let [input-key (console/get-key-blocking DISPLAY)]
    (if (= :escape input-key) (quit-game!)
      (recur))))

(defn increase-difficulty!
  []
  (when (> @GAME-SPEED MAX-SPEED)
    (swap! GAME-SPEED #(- % 1))))

(defn step!
  "Perform the next step in the game (if the player cleared a line, count the score and stuff)"
  []
  (let [num-cleared-lines (count (get-filled-lines @MATRIX))]
    (when (> num-cleared-lines 0)
      (swap! MATRIX #(clear-filled-lines %))
      (swap! CLEARED-LINES #(+ num-cleared-lines %))
      (swap! SCORE #(+ (* 100 num-cleared-lines) %)))))

(defn read-input []
  (let [user-input (console/get-key DISPLAY)]
    (cond
      (= :left user-input) (move-left!)
      (= :right user-input) (move-right!)
      (= :down user-input) (move-down!)
      (= :escape user-input) (quit-game!)
      (= \z user-input) (rotate-left!)
      (= \x user-input) (rotate-right!))))

(defn game-loop []
  (when (or (= "" (:id @ACTIVE-PIECE))
            (= true (:anchored @ACTIVE-PIECE)))
    (choose-new-piece!))
  (step!)
  (clear-screen!)
  (print-matrix!
    (insert-piece
      (:graphics @ACTIVE-PIECE) @MATRIX (:row @ACTIVE-PIECE) (:col @ACTIVE-PIECE)))
  (read-input)
  (if (game-over?)
    (game-over!)
    (recur)))

(defn -main []
  (console/start DISPLAY)
  (print-line! "***** TETRIS *****" 0)
  (print-line! "PRESS ANY KEY: PLAY" 1)
  (print-line! "PRESS ESC: QUIT" 2)
  (clear-screen!)
  (console/get-key-blocking DISPLAY)
  (clear-matrix!)
  (future
    (while true
      (do
        (increase-difficulty!) ;; Increase the difficulty slowly as the game progresses.
        (Thread/sleep @GAME-SPEED)
        (move-down!))))
  (game-loop))
