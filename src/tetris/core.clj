(ns tetris.core ;; The main/core module handles the game loop.
  (:require [clojure.string :as s]
            [tetris.matrix :as m]
            [tetris.rotation :as r]
            [tetris.collision :as c]
            [tetris.move :as mv]
            [tetris.graphics :as g]
            [quil.core :as q])
  (:import java.awt.event.KeyEvent)
  (:gen-class))

;; -------------------------------------------------------------------------------------------
;; ----------------------------------------- GLOBALS -----------------------------------------
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
;; ------------------------------------------- GUI -------------------------------------------
;; -------------------------------------------------------------------------------------------

(def WINDOW-WIDTH 250)
(def WINDOW-HEIGHT 650)
(def SQUARE-WIDTH 25)
(def SQUARE-HEIGHT 25)
(def FPS 30)

(defn setup []
  (q/frame-rate FPS)
  (q/stroke 0)
  (q/stroke-weight 0)
  (q/background 255 255 255))

(defn get-color
  [ch]
  (cond
    (or (= \b ch) (= \B ch)) (q/fill 0 75 255)
    (or (= \r ch) (= \R ch)) (q/fill 255 17 0)
    (or (= \y ch) (= \Y ch)) (q/fill 254 226 62)
    (or (= \g ch) (= \G ch)) (q/fill 99 200 62)
    (or (= \m ch) (= \M ch)) (q/fill 219 48 130)
    (or (= \c ch) (= \C ch)) (q/fill 27 161 266)
    (or (= \o ch) (= \O ch)) (q/fill 255 129 0)
    (= \= ch) (q/fill 220 220 220)
    :else (q/fill 255 255 255)))

(defn print-line!
  [text lnum use-color]
  (doall
    (map-indexed
      (fn [idx ch]
        (get-color ch)
        (q/rect (* idx SQUARE-WIDTH) (* lnum SQUARE-HEIGHT) SQUARE-WIDTH SQUARE-HEIGHT))
      text)))

(defn print-matrix!
  [matrix offset]
  (if (empty? matrix) (recur (m/get-empty-matrix) offset)
    (let [lines (map #(clojure.string/join "" %) matrix)]
      (doseq [[line i] (map list lines (range (count lines)))]
        (print-line! line (+ i offset) true)))))

(defn get-key []
  (let [raw-key (q/raw-key)
        the-key-code (q/key-code)
        the-key-pressed (if (= processing.core.PConstants/CODED (int raw-key)) the-key-code raw-key)]
    the-key-pressed))

(defn show-pause-menu! []
  (q/background 0 0 0)
  (q/text-size 20)
  (q/text-align :center)
  (q/text "TETRIS" (/ WINDOW-WIDTH 2) 50)
  (q/text-size 15)
  (q/text-align :left)
  (q/text "MOUSE CLICK: PLAY/PAUSE" 0 90)
  (q/text "PRESS ESC: QUIT" 0 120)
  (q/text "AROW KEYS: MOVE" 0 140)
  (q/text "PRESS Z: ROTATE L" 0 160)
  (q/text "PRESS X: ROTATE R" 0 180)
  (q/text (str "*SCORE: " @SCORE) 0 230)
  (q/text (str "*LINES: " @CLEARED-LINES) 0 250))

(defn show-game-over-screen! []
  (q/background 0 0 0)
  (q/text-size 20)
  (q/text-align :center)
  (q/text "GAME OVER" (/ WINDOW-WIDTH 2) 50)
  (q/text-size 15)
  (q/text-align :left)
  (q/text (str "YOUR SCORE: " @SCORE) 0 90)
  (q/text (str "YOUR LINES: " @CLEARED-LINES) 0 120)
  (q/text (str "HIGH SCORE: " (first (read-high-score HIGH-SCORE-FILE))) 0 150)
  (q/text (str "HIGH LINES: " (last (read-high-score HIGH-SCORE-FILE))) 0 180))

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

;;(defn pause-game!
;;  "Contract nil -> nil"
;;  []
;;  (gui/print-line! "*** GAME PAUSED ***" 0 false)
;;  (gui/print-line! "*ANY KEY: CONTINUE*" 1 false)
;;  (gui/print-line! (gui/right-pad (str "*SCORE: " @SCORE) 19) 2 false)
;;  (gui/print-line! (gui/right-pad (str "*LINES: " @CLEARED-LINES) 19) 3 false)
;;  (gui/clear-screen!)
;;  (gui/get-key-blocking))

(defn read-input
  "Contract: nil -> nil"
  []
  (let [user-input (get-key)]
    (cond
      (= KeyEvent/VK_LEFT user-input) (mv/move-left! MATRIX ACTIVE-PIECE)
      (= KeyEvent/VK_RIGHT user-input) (mv/move-right! MATRIX ACTIVE-PIECE)
      (= KeyEvent/VK_DOWN user-input) (mv/move-down! MATRIX ACTIVE-PIECE)
      (= KeyEvent/VK_UP user-input) (mv/hard-drop! MATRIX ACTIVE-PIECE)
      ;; (= :escape user-input) (quit-game!)
      ;; (= :enter user-input) (pause-game!)
      ;; (= \p user-input) (pause-game!)
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
  ;;(gui/print-line! "^^^ NEXT1 PIECE ^^^" 3 false)
  (let [next-piece-id (nth NEXT-PIECE @PIECE-COUNT)
        next-piece-graphics (g/get-graphics next-piece-id :CENTER)
        start-position (mv/START-POSITIONS next-piece-id)
        padding (into [] (take 3 (repeat g/EMPTY-LINE)))
        x (first start-position)
        y (last start-position)
        offset 0]
    (print-matrix!
      (m/insert-piece
        next-piece-graphics padding x y)
      offset)))

(defn show-playfield!
  "Contract: nil -> nil
  Renders the playfield along with the current tetris piece and it's shadow.
  The shadow is the little preview at the bottom, that tells the player where the current tetris piece is going to land."
  []
  (cond
    (game-over?) (show-game-over-screen!)
    @GAME-RUNNING?
    (do
      (show-next-piece!)
      (let [shadow-graphics (map (fn [row] (map #(if (not= "." %) "=" %) row)) (:graphics @ACTIVE-PIECE))
            shadow-col (:col @ACTIVE-PIECE)
            shadow-row (mv/get-lowest-row @MATRIX shadow-graphics (:row @ACTIVE-PIECE) shadow-col)
            playfield-with-shadow (m/insert-piece shadow-graphics @MATRIX shadow-row shadow-col)]
        (print-matrix!
          (m/insert-piece
            (:graphics @ACTIVE-PIECE) playfield-with-shadow (:row @ACTIVE-PIECE) (:col @ACTIVE-PIECE))
          MATRIX-START-ROW)))
    :else (show-pause-menu!)))

(defn game-loop
  "Contract: nil -> nil"
  []
  (if @GAME-RUNNING?
    (do
      (when (or (= "" (:id @ACTIVE-PIECE))
                (= true (:anchored @ACTIVE-PIECE)))
        (choose-new-piece!))
      (step!)
      (force-down!)
      (if (game-over?)
        (overwrite-high-score!)
        (do
          (Thread/sleep 10) ;; The loop must not go too fast, or we'll waste CPU.
          (recur))))
    (do
      (Thread/sleep 10) ;; The loop must not go too fast, or we'll waste CPU.
      (recur))))

(defn -main []
  (println "Done!") ;; Signal that we have loaded the program.
  (clear-playfield!)
  (q/defsketch example
    :title "Tetris board"
    :settings #(q/smooth 2)
    :setup setup
    :key-pressed read-input
    :mouse-clicked #(swap! GAME-RUNNING? not)
    :draw show-playfield!
    :features [:exit-on-close]
    :size [WINDOW-WIDTH WINDOW-HEIGHT])
  (game-loop))
