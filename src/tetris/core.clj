(ns tetris.core ;; The main/core module handles the game loop.
  (:require [clojure.string :as s]
            [tetris.matrix :as m]
            [tetris.rotation :as r]
            [tetris.collision :as c]
            [tetris.move :as mv]
            [tetris.graphics :as g]
            [tetris.gui :as gui])
  (:gen-class))

;; -------------------------------------------------------------------------------------------
;; ----------------------------------------- GLOBALS -----------------------------------------
;; -------------------------------------------------------------------------------------------

;; The player's score.
(def SCORE (atom 0))
(def CLEARED-LINES (atom 0))
(def HIGH-SCORE-FILE "./score.dat")

;; Timers.
(def LAST-MOVE-TIME (atom (System/currentTimeMillis))) ;; The exact time of when the game last moved down.

;; Our playfield.
(def MATRIX (atom []))
(def MATRIX-START-ROW 4)

;; The active tetris piece that the player moves.
;; Possible rotations: CENTER, ROTATE1, ROTATE2, ROTATE3
(def ACTIVE-PIECE (atom {:id "" :rotation :CENTER :row 0 :col 0 :anchored false :graphics []}))

;; The number of pieces that the player has received so far in the game.
(def PIECE-COUNT (atom 0))

;; A lazy seq of all pieces that will flow one after the other during the game.
(def NEXT-PIECE
  (repeatedly
    #(first (shuffle ["I" "O" "Z" "S" "J" "L" "T"]))))

;; -------------------------------------------------------------------------------------------
;; ------------------------------------------ SCORE ------------------------------------------
;; -------------------------------------------------------------------------------------------

(defn read-high-score
  "Contract: string -> int
  Reads the players best score from a file"
  [fname]
  (read-string (slurp fname)))

(defn save-high-score
  "Contract: string -> int
  Saves the players score to a file."
  [fname]
  (spit fname (with-out-str (pr [@SCORE @CLEARED-LINES]))))

(defn overwrite-high-score!
  "Contract: nil -> nil"
  []
  (let [high-score (read-high-score HIGH-SCORE-FILE)]
    (when (> @SCORE (first high-score))
      (save-high-score HIGH-SCORE-FILE))))

;; -------------------------------------------------------------------------------------------
;; ---------------------------------------- Game loop ----------------------------------------
;; -------------------------------------------------------------------------------------------

(defn clear-playfield!
  "Contract: nil -> nil"
  []
  (swap! MATRIX (fn [m] (m/get-empty-matrix))))

(defn get-game-speed "Contract: nil -> int" []
  (cond
    (> @CLEARED-LINES 100) 60
    (> @CLEARED-LINES 75) 80
    (> @CLEARED-LINES 60) 120
    (> @CLEARED-LINES 40) 250
    (> @CLEARED-LINES 30) 330
    (> @CLEARED-LINES 20) 400
    (> @CLEARED-LINES 10) 500
    :else 600))

(defn choose-new-piece!
  "Contract: nil -> string"
  []
  (mv/set-active-piece! ACTIVE-PIECE (nth NEXT-PIECE @PIECE-COUNT))
  (swap! PIECE-COUNT #(inc %)))

(defn get-filled-lines
  "Contract: vector<vector> -> vector<vector>
  Returns any lines with no empty spaces in them."
  [matrix]
  (filter #(not (some #{"."} %)) matrix))

(defn clear-filled-lines
  "Contract: vector<vector> -> vector<vector>
  Well, if the player has filled any lines, we have to unfill them."
  [matrix]
  (let [num-cleared-lines (count (get-filled-lines @MATRIX))
        matrix-cleared (into [] (remove #(not (some #{"."} %)) matrix))]
    (into []
          (concat
            (take num-cleared-lines (repeat g/EMPTY-LINE))
            matrix-cleared))))

(defn game-over?
  "Contract: nil -> bool
  Returns true if the player has reached the top level of the matrix, thus losing the game."
  []
  (some #(not= "." %) (first @MATRIX)))

(defn quit-game!
  "Contract: nil -> nil"
  []
  (System/exit 0))

(defn restart-game!
  "Contract: nil -> nil
  Allows the player to start playing the game again after game-over."
  []
  (swap! SCORE (fn [a] 0))
  (swap! CLEARED-LINES (fn [a] 0))
  (clear-playfield!))

(defn step!
  "Contract: nil -> nil
  Perform the next step in the game (if the player cleared a line, count the score and stuff)"
  []
  (let [num-cleared-lines (count (get-filled-lines @MATRIX))]
    (when (> num-cleared-lines 0)
      (swap! MATRIX #(clear-filled-lines %))
      (swap! CLEARED-LINES #(+ num-cleared-lines %))
      (swap! SCORE #(+ (* 100 num-cleared-lines) %)))))

(defn pause-game!
  "Contract nil -> nil"
  []
  (gui/print-line! "*** GAME PAUSED ***" 0 false)
  (gui/print-line! "*ANY KEY: CONTINUE*" 1 false)
  (gui/print-line! (gui/right-pad (str "*SCORE: " @SCORE) 19) 2 false)
  (gui/print-line! (gui/right-pad (str "*LINES: " @CLEARED-LINES) 19) 3 false)
  (gui/clear-screen!)
  (gui/get-key-blocking))

(defn read-input
  "Contract: nil -> nil"
  []
  (let [user-input (gui/get-key)]
    (cond
      (= :left user-input) (mv/move-left! MATRIX ACTIVE-PIECE)
      (= :right user-input) (mv/move-right! MATRIX ACTIVE-PIECE)
      (= :down user-input) (mv/move-down! MATRIX ACTIVE-PIECE)
      (= :up user-input) (mv/hard-drop! MATRIX ACTIVE-PIECE)
      (= :escape user-input) (quit-game!)
      (= :enter user-input) (pause-game!)
      (= \p user-input) (pause-game!)
      (= \z user-input) (r/rotate-left! MATRIX ACTIVE-PIECE)
      (= \x user-input) (r/rotate-right! MATRIX ACTIVE-PIECE))))

(defn force-down!
  "Contract: nil -> nil
  Force the current active piece to move down on its own."
  []
  (when (>
         (- (System/currentTimeMillis) @LAST-MOVE-TIME)
         (get-game-speed))
    (swap! LAST-MOVE-TIME (fn [x] (System/currentTimeMillis)))
    (mv/move-down! MATRIX ACTIVE-PIECE)))

(defn show-next-piece!
  "Contract: nil -> nil
  Displays the next tetris piece that the player will receive."
  []
  (gui/print-line! "^^^ NEXT1 PIECE ^^^" 3 false)
  (let [next-piece-id (nth NEXT-PIECE @PIECE-COUNT)
        next-piece-graphics (g/get-graphics next-piece-id :CENTER)
        start-position (mv/START-POSITIONS next-piece-id)
        padding (into [] (take 3 (repeat g/EMPTY-LINE)))
        x (first start-position)
        y (last start-position)
        offset 0]
    (gui/print-matrix!
      (m/insert-piece
        next-piece-graphics padding x y)
      offset)))

(defn get-lowest-row
  "Contract: vector<vector> int int -> int
  Returns the lowest row that a piece can drop in the matrix."
  [piece-graphics current-row current-col]
  (let [new-x (inc current-row)
        new-y current-col]
    (cond
      (not= :in-bounds (c/check-bounds new-x new-y piece-graphics @MATRIX)) current-row
      (= :collision (c/detect-collision new-x new-y piece-graphics @MATRIX)) current-row
      :else (recur piece-graphics new-x new-y))))

(defn show-playfield!
  "Contract: nil -> nil
  Renders the playfield along with the current tetris piece and it's shadow.
  The shadow is the little preview at the bottom, that tells the player where the current tetris piece is going to land."
  []
  (let [shadow-graphics (map (fn [row] (map #(if (not= "." %) "=" %) row)) (:graphics @ACTIVE-PIECE))
        shadow-col (:col @ACTIVE-PIECE)
        shadow-row (get-lowest-row shadow-graphics (:row @ACTIVE-PIECE) shadow-col)
        playfield-with-shadow (m/insert-piece shadow-graphics @MATRIX shadow-row shadow-col)]
    (gui/print-matrix!
      (m/insert-piece
        (:graphics @ACTIVE-PIECE) playfield-with-shadow (:row @ACTIVE-PIECE) (:col @ACTIVE-PIECE))
      MATRIX-START-ROW)))

;; game-over! needs to call game-loop early, in order to restart the game.
(declare game-loop)

(defn game-over!
  "Contract: nil -> nil
  Shows the game over message and exits when the player presses ESC key."
  []
  (overwrite-high-score!)
  (gui/print-line! "**** GAME OVER ****" 0 false)
  (gui/print-line! (gui/right-pad (str "YOUR SCORE - " @SCORE) 19) 1 false)
  (gui/print-line! (gui/right-pad (str "YOUR LINES - " @CLEARED-LINES) 19) 2 false)
  (gui/print-line! (gui/right-pad (str "HIGH SCORE - " (first (read-high-score HIGH-SCORE-FILE))) 19) 3 false)
  (gui/print-line! (gui/right-pad (str "HIGH LINES - " (last (read-high-score HIGH-SCORE-FILE))) 19) 4 false)
  (gui/print-line! (gui/right-pad "ENTER: RESTART" 19) 5 false)
  (gui/print-line! (gui/right-pad "ESC: QUIT" 19) 6 false)
  (gui/clear-screen!)
  (let [input-key (gui/get-key-blocking)]
    (cond
      (= :escape input-key) (quit-game!)
      (= :enter input-key) (do (restart-game!) (game-loop))
      :else (recur))))

(defn game-loop
  "Contract: nil -> nil"
  []
  (when (or (= "" (:id @ACTIVE-PIECE))
            (= true (:anchored @ACTIVE-PIECE)))
    (choose-new-piece!))
  (step!)
  (gui/clear-screen!)
  (show-next-piece!)
  (show-playfield!)
  (read-input)
  (force-down!)
  (if (game-over?)
    (game-over!)
    (recur)))

(defn show-title-screen!
  "Contract: nil -> char"
  []
  (gui/print-line! "***** TETRIS *****" 0 false)
  (gui/print-line! "PRESS ANY KEY: PLAY" 1 false)
  (gui/print-line! "PRESS ESC: QUIT" 2 false)
  (gui/print-line! "AROW KEYS: MOVE" 3 false)
  (gui/print-line! "PRESS Z: ROTATE L" 4 false)
  (gui/print-line! "PRESS X: ROTATE R" 5 false)
  (gui/print-line! "PRESS P: PAUSE" 6 false)
  (gui/print-line! "PRESS ENTER: PAUSE" 7 false)
  (gui/clear-screen!)
  (gui/get-key-blocking))

(defn -main []
  (println "Done!") ;; Signal that we have loaded the program.
  (gui/start-gui)
  ;; Center the main window before showing the title screen.
  (gui/center-gui)
  (show-title-screen!)
  (clear-playfield!)
  (game-loop)) 
