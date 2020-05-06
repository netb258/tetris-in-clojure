(ns tetris.core ;; The main/core module handles the game loop.
  (:require [clojure.string :as s]
            [tetris.matrix :as m]
            [tetris.rotation :as r]
            [tetris.collision :as c]
            [tetris.move :as mv]
            [tetris.graphics :as g]
            [tetris.score :as score]
            [tetris.gui :as gui]
            [quil.core :as q])
  (:import java.awt.event.KeyEvent)
  (:gen-class))

;; -------------------------------------------------------------------------------------------
;; ---------------------------------------- Game State ---------------------------------------
;; -------------------------------------------------------------------------------------------

;; The player's score.
(def SCORE (atom 0))
(def CLEARED-LINES (atom 0))
(def HIGH-SCORE-FILE "./score.dat")
(def GAME-RUNNING? (atom false))

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

;; We want these keys to be repeated quickly, so we won't rely on the system repeat rate.
(def is-left-pressed? (atom false))
(def is-right-pressed? (atom false))

;; The player can move left and right in a 105ms interval.
(def player-move-interval 105)
;; The exact time when the player last moved.
(def player-last-move-time (atom (System/currentTimeMillis)))

;; -------------------------------------------------------------------------------------------
;; ---------------------------------------- Game loop ----------------------------------------
;; -------------------------------------------------------------------------------------------

(defn clear-playfield!
  "Contract: nil -> nil"
  []
  (swap! MATRIX (fn [m] (m/get-empty-matrix))))

(defn get-game-speed "Contract: nil -> int" []
  (cond
    (> @CLEARED-LINES 120) 60
    (> @CLEARED-LINES 100) 65
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

(defn step!
  "Contract: nil -> nil
  Perform the next step in the game (if the player cleared a line, count the score and stuff)"
  []
  (let [num-cleared-lines (count (get-filled-lines @MATRIX))]
    (when (> num-cleared-lines 0)
      (swap! MATRIX #(clear-filled-lines %))
      (swap! CLEARED-LINES #(+ num-cleared-lines %))
      (swap! SCORE #(+ (* 100 num-cleared-lines) %)))))

(defn force-down!
  "Contract: nil -> nil
  Force the current active piece to move down on its own."
  []
  (when (>=
         (- (System/currentTimeMillis) @LAST-MOVE-TIME)
         (get-game-speed))
    (swap! LAST-MOVE-TIME (fn [x] (System/currentTimeMillis)))
    (mv/move-down! MATRIX ACTIVE-PIECE)))

(defn force-left!
  "Contract: nil -> nil
  Force the current active piece to move left."
  []
  (when (and
          (= true @is-left-pressed?)
          (>=
            (- (System/currentTimeMillis) @player-last-move-time)
            player-move-interval))
    (swap! player-last-move-time (fn [x] (System/currentTimeMillis)))
    (mv/move-left! MATRIX ACTIVE-PIECE)))

(defn force-right!
  "Contract: nil -> nil
  Force the current active piece to move right."
  []
  (when (and
          (= true @is-right-pressed?)
          (>=
            (- (System/currentTimeMillis) @player-last-move-time)
            player-move-interval))
    (swap! player-last-move-time (fn [x] (System/currentTimeMillis)))
    (mv/move-right! MATRIX ACTIVE-PIECE)))

(defn game-loop
  "Contract: nil -> nil"
  []
  (when @GAME-RUNNING?
    (when (or (= "" (:id @ACTIVE-PIECE))
              (= true (:anchored @ACTIVE-PIECE)))
      (choose-new-piece!))
    (step!)
    (force-down!)
    (force-left!)
    (force-right!)
    (when (game-over?)
      (score/overwrite-high-score! HIGH-SCORE-FILE @SCORE @CLEARED-LINES))))

;; -------------------------------------------------------------------------------------------
;; ----------------------------------- MAIN - Display game -----------------------------------
;; -------------------------------------------------------------------------------------------

(defn key-down
  "Contract: nil -> nil"
  []
  (let [user-input (gui/get-key)]
    (cond
      (= KeyEvent/VK_LEFT user-input) (reset! is-left-pressed? true)
      (= KeyEvent/VK_RIGHT user-input) (reset! is-right-pressed? true)
      (= KeyEvent/VK_DOWN user-input) (mv/move-down! MATRIX ACTIVE-PIECE)
      (= KeyEvent/VK_UP user-input) (mv/hard-drop! MATRIX ACTIVE-PIECE)
      (= \newline user-input) (swap! GAME-RUNNING? not)
      ;; NOTE: The playfield cannot finilize while the player is rotating.
      (= \z user-input) (do (r/rotate-left! MATRIX ACTIVE-PIECE)
                            (reset! mv/update-interval (future (Thread/sleep 500) (inc 0))))
      (= \x user-input) (do (r/rotate-right! MATRIX ACTIVE-PIECE)
                            (reset! mv/update-interval (future (Thread/sleep 500) (inc 0)))))))

(defn key-up
  "Contract: nil -> nil"
  []
  (let [user-input (gui/get-key)]
    (cond
      ;; NOTE: If the player has stopped rotating, then we can update the playfield.
      (= \z user-input) (reset! mv/update-interval nil)
      (= \x user-input) (reset! mv/update-interval nil)
      (= KeyEvent/VK_LEFT user-input) (do (reset! is-left-pressed? false) (reset! mv/update-interval nil))
      (= KeyEvent/VK_RIGHT user-input) (do (reset! is-right-pressed? false) (reset! mv/update-interval nil)))))

(defn show-next-piece!
  "Contract: nil -> nil
  Displays the next tetris piece that the player will receive."
  []
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

(defn show-playfield!
  "Contract: nil -> nil
  Renders the playfield along with the current tetris piece and it's shadow.
  The shadow is the little preview at the bottom, that tells the player where the current tetris piece is going to land."
  []
  (cond
    (game-over?) ;; If the game is over, then show the GAME OVER screen.
    (gui/show-game-over-screen!
      @SCORE
      @CLEARED-LINES
      (first (score/read-high-score HIGH-SCORE-FILE))
      (last (score/read-high-score HIGH-SCORE-FILE)))
    @GAME-RUNNING? ;; If the game is currently running, then draw all the graphics.
    (do ;; Show the next piece the player will receive, also show the current piece with a preview shadow:
      (show-next-piece!)
      (let [shadow-graphics (map (fn [row] (map #(if (not= "." %) "=" %) row)) (:graphics @ACTIVE-PIECE))
            shadow-col (:col @ACTIVE-PIECE)
            shadow-row (mv/get-lowest-row @MATRIX shadow-graphics (:row @ACTIVE-PIECE) shadow-col)
            playfield-with-shadow (m/insert-piece shadow-graphics @MATRIX shadow-row shadow-col)]
        (gui/print-matrix!
          (m/insert-piece
            (:graphics @ACTIVE-PIECE) playfield-with-shadow (:row @ACTIVE-PIECE) (:col @ACTIVE-PIECE))
          MATRIX-START-ROW)))
    ;; If the game is not running and is not over, then it must be paused.
    :else (gui/show-pause-menu!
            @SCORE
            @CLEARED-LINES
            (first (score/read-high-score HIGH-SCORE-FILE))
            (last (score/read-high-score HIGH-SCORE-FILE)))))

(defn run-game! []
  (game-loop)
  (show-playfield!))

(defn -main []
  (clear-playfield!)
  (q/defsketch example
    :title "Tetris"
    :settings #(q/smooth 2)
    :setup gui/setup
    :key-pressed key-down
    :key-released key-up
    :mouse-clicked #(swap! GAME-RUNNING? not)
    :draw run-game!
    :features [:exit-on-close]
    :size [gui/WINDOW-WIDTH gui/WINDOW-HEIGHT]))
