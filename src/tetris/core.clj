(ns tetris.core
  (:require [clojure.string :as s]
            [lanterna.screen :as console]
            [lanterna.terminal :as t])
  (:import com.googlecode.lanterna.screen.Screen)
  (:gen-class))

;; -------------------------------------------------------------------------------------------
;; ----------------------------------------- GLOBALS -----------------------------------------
;; -------------------------------------------------------------------------------------------

;; The window that will hold our game.
(def WINDOW (t/get-terminal :swing {:rows 26 :cols 19 :font-size 20}))
(def DISPLAY (new Screen WINDOW))

;; The player's score.
(def SCORE (atom 0))
(def CLEARED-LINES (atom 0))
(def HIGH-SCORE-FILE "./score.dat")

;; Timers.
(def LAST-MOVE-TIME (atom (System/currentTimeMillis))) ;; The exact time of when the game last moved down.
(def REDRAW-PAUSE 20)

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

(defn get-color
  [ch]
  (cond
    (or (= \b ch) (= \B ch)) {:fg :blue}
    (or (= \r ch) (= \R ch)) {:fg :red}
    (or (= \y ch) (= \Y ch)) {:fg :yellow}
    (or (= \g ch) (= \G ch)) {:fg :green}
    (or (= \m ch) (= \M ch)) {:fg :magenta}
    (or (= \c ch) (= \C ch)) {:fg :cyan}
    (or (= \o ch) (= \O ch)) {:fg :white}
    :else {:fg :default :bg :default}))

(defn right-pad
  "RIght pad string with spaces, making it at least len long."
  [mystr len]
  (format (str "%-" len "s") mystr))

(defn print-line!
  "Contract: string int bool -> nil
  A custom printing function for our swing console.
  NOTE: Obliously it returns something, since it's a call to map,
  but the result is useless so I'm contracting it as -> nil."
  [text lnum use-color]
  (doall
    (map-indexed
      (fn [idx ch]
        (console/put-string DISPLAY idx lnum (str ch) (when use-color (get-color ch))))
      text)))

(defn clear-screen!
  "Contract: nil -> nil
  Clear the console window."
  []
  (console/redraw DISPLAY)
  (Thread/sleep REDRAW-PAUSE)) ;; We need a slight delay when redrawing or it will consume too much CPU.

(defn get-empty-matrix
  "Contract: nil -> vector<vector>"
  []
  (into [] (take 22 (repeat EMPTY-LINE))))

(defn print-matrix!
  "Contract: vector<vector> int -> nil"
  [matrix offset]
  (flush)
  (if (empty? matrix) (recur (get-empty-matrix) offset)
    (let [lines (map #(s/join " " %) matrix)]
      (doseq [[line i] (map list lines (range (count lines)))]
        (print-line! line (+ i offset) true)))))

(defn clear-matrix!
  "Contract: nil -> nil"
  []
  (swap! MATRIX (fn [m] (get-empty-matrix))))

;; -------------------------------------------------------------------------------------------
;; ------------------------------------ Draw tetris piece ------------------------------------
;; -------------------------------------------------------------------------------------------

(defn get-graphics
  "Contract: string keyword -> vector<vector>"
  [piece-id rotation]
  (cond
    (= :CENTER rotation) (get PIECES-NORMAL piece-id)
    (= :ROTATE1 rotation) (get PIECES-ROTATED1 piece-id)
    (= :ROTATE2 rotation) (get PIECES-ROTATED2 piece-id)
    (= :ROTATE3 rotation) (get PIECES-ROTATED3 piece-id)))

(defn set-active-piece!
  ([id] (set-active-piece! id :CENTER (START-POSITIONS id)))
  ([id [row col]] (set-active-piece! id (:rotation @ACTIVE-PIECE) [row col]))
  ([id rotation [row col]] ;; Contract: string keyword [int int] -> nil
   (swap!
     ACTIVE-PIECE
     (fn [p] {:id id
              :rotation rotation
              :row row
              :col col
              :anchored false
              :graphics (get-graphics id rotation)}))))

(defn clean-rows
  "Contract: vector<vector> -> vector<vector>
  Removes from the piece any rows that contain only empty spaces."
  [piece-graphics]
  (let [top-empty-rows (take-while (fn [row] (every? #(= "." %) row)) piece-graphics)
        full-rows (filter (fn [row] (some #(not= "." %) row)) piece-graphics)]
    (into [] (concat top-empty-rows full-rows))))

(defn flip-row
  "Contract: vector<vector> int -> vector<string>
  Selects a single row from a piece graphics and transforms it from rows to cols representation."
  [piece-graphics col]
  (into [] (map #(nth % col) piece-graphics)))

(defn flip-all-rows
  "Contract: vector<vector> -> vector<vector>"
  [piece-graphics]
  (vec
    (for [i (range (count (first piece-graphics)))
          :let [col (flip-row piece-graphics i)]]
      col)))

;; NOTE: Calling flip-all-rows twice, basically flips the rows/cols representation back into it's original form.
(defn clean-cols
  "Contract: vector<vector> -> vector<vector>
  Removes from the piece any cols that contain only empty spaces."
  [piece-graphics]
  (flip-all-rows
    (filter
      (fn [row] (some #(not= "." %) row))
      (flip-all-rows piece-graphics))))

(defn clean-piece
  "Contract: vector<vector> -> vector<vector>
  Removes any empty rows and cols from a piece."
  [piece-graphics]
  (clean-cols (clean-rows piece-graphics)))

(defn insert-piece-row
  "Contract: vector<string> vector<string> int -> vector<string>"
  [piece-row matrix-row position]
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

(defn upcase-matrix
  "Contract: vector<vector> -> vector<vector>"
  [matrix]
  (mapv #(mapv s/upper-case %) matrix))

(defn downcase-matrix
  "Contract: vector<vector> -> vector<vector>"
  [matrix]
  (mapv #(mapv s/lower-case %) matrix))

;; Throws an IndexOutOfBoundsException, if the row/col are outside the matrix.
;; NOTE: (map #(map s/upper-case %) piece) - Need the double map, since the piece is represented as a vector of vectors.
(defn insert-piece
  "Contract: vector<vector> vector<vector> int int -> vector<vector>"
  [piece matrix row col]
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
  "Contract: vector<string> vector<string> -> vector<bool>
  Detects collisions on a single piece row."
  [piece-row move-row]
  (map #(and (not= "." %1) (not= "." %2)) piece-row move-row))

(defn count-collisions
  "Contract: vector<bool> -> int"
  [collision-vector]
  (count (filter #(some #{true} %) collision-vector)))

(defn detect-collision
  "Contract: int int vector<vector> -> keyword
  Returns :collision if the active tetris piece will collide with anything in the matrix at the given coordinates."
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
  "Contract: int int vector<vector> vector<vector> -> keyword
  Takes the row and col of where the player is trying to move and checks if they are within the matrix."
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

;; Move left is defined a bit later, but rotate-piece! actually needs it to do some adjustments.
(declare move-left!)

(defn rotate-piece!
  "Contract: keyword -> nil
  NOTE: There is only one situation when the rotation would put us out of bounds:
  When we have moved too far to the right and we are trying to rotate out of the matrix (can't happen for left).
  We correct this situation by moving ot the left, until (not= :in-bounds) becomes false (or we :cant-move-there)."
  [rotation]
  (let [current-id (:id @ACTIVE-PIECE)
        current-row (:row @ACTIVE-PIECE)
        current-col (:col @ACTIVE-PIECE)
        new-rotation (get-graphics current-id rotation)]
    (cond
      (not= :in-bounds (check-bounds current-row current-col new-rotation @MATRIX))
      (when (not= :cant-move-there (move-left!)) (recur rotation))
      (= :no-collision (detect-collision current-row current-col new-rotation))
      (swap!
        ACTIVE-PIECE
        (fn [p]
          {:id current-id
           :rotation rotation
           :row current-row
           :col current-col
           :anchored false
           :graphics new-rotation}))
      :else :cant-rotate)))

(defn rotate-left!
  "Contract: nil -> nil"
  []
  (let [current-rotation (:rotation @ACTIVE-PIECE)]
   (cond
     (= :CENTER current-rotation) (rotate-piece! :ROTATE3)
     (= :ROTATE3 current-rotation) (rotate-piece! :ROTATE2)
     (= :ROTATE2 current-rotation) (rotate-piece! :ROTATE1)
     :else (rotate-piece! :CENTER))))

(defn rotate-right!
  "Contract: nil -> nil"
  []
  (let [current-rotation (:rotation @ACTIVE-PIECE)]
    (cond
      (= :CENTER current-rotation) (rotate-piece! :ROTATE1)
      (= :ROTATE1 current-rotation) (rotate-piece! :ROTATE2)
      (= :ROTATE2 current-rotation) (rotate-piece! :ROTATE3)
      :else (rotate-piece! :CENTER))))

;; -------------------------------------------------------------------------------------------
;; ------------------------------------ Move tetris piece ------------------------------------
;; -------------------------------------------------------------------------------------------

(defn update-playfield!
  "Contract: nil -> nil"
  []
  (swap! MATRIX
         #(downcase-matrix
            (insert-piece (:graphics @ACTIVE-PIECE) % (:row @ACTIVE-PIECE) (:col @ACTIVE-PIECE))))
  (swap! ACTIVE-PIECE #(assoc % :anchored true)))

(defn move-active-piece!
  "Contract: int int -> nil or error keyword"
  [& {:keys [x y]
      :or {x (:row @ACTIVE-PIECE)
           y (:col @ACTIVE-PIECE)}}]
  (if (= :in-bounds (check-bounds x y (:graphics @ACTIVE-PIECE) @MATRIX))
    (set-active-piece! (:id @ACTIVE-PIECE) [x y])
    :out-of-bounds))

(defn move-left!
  "Contract: nil -> nil or error keyword
  Allows the player to move the current active piece to the left."
  []
  (let [new-x (:row @ACTIVE-PIECE)
        new-y (dec (:col @ACTIVE-PIECE))]
    (cond
      (not= :in-bounds (check-bounds new-x new-y (:graphics @ACTIVE-PIECE) @MATRIX)) :out-of-bouns
      (= :collision (detect-collision new-x new-y (:graphics @ACTIVE-PIECE))) :cant-move-there
      :else (move-active-piece! :x new-x :y new-y))))

(defn move-right!
  "Contract: nil -> nil or error keyword
  Allows the player to move the current active piece to the right."
  []
  (let [new-x (:row @ACTIVE-PIECE)
        new-y (inc (:col @ACTIVE-PIECE))]
    (cond
      (not= :in-bounds (check-bounds new-x new-y (:graphics @ACTIVE-PIECE) @MATRIX)) :out-of-bouns
      (= :collision (detect-collision new-x new-y (:graphics @ACTIVE-PIECE))) :cant-move-there
      :else (move-active-piece! :x new-x :y new-y))))

(defn move-down!
  "Contract: nil -> nil or error keyword
  Moves to move the current active tetris piece one step down."
  []
  (let [new-x (inc (:row @ACTIVE-PIECE))
        new-y (:col @ACTIVE-PIECE)]
    (cond
      (not= :in-bounds (check-bounds new-x new-y (:graphics @ACTIVE-PIECE) @MATRIX)) (update-playfield!)
      (= :collision (detect-collision new-x new-y (:graphics @ACTIVE-PIECE))) (update-playfield!)
      :else (move-active-piece! :x new-x :y new-y))))

(defn hard-drop!
  "Contract: nil -> nil or error keyword
  Drop the player to the bottom of the matrix instantly."
  []
  (dotimes [i (count @MATRIX)]
    (move-down!)))

;; -------------------------------------------------------------------------------------------
;; ---------------------------------------- Game loop ----------------------------------------
;; -------------------------------------------------------------------------------------------

(defn get-game-speed "Contract: nil -> int" []
  (cond
    (> @CLEARED-LINES 50) 100
    (> @CLEARED-LINES 40) 200
    (> @CLEARED-LINES 30) 300
    (> @CLEARED-LINES 20) 400
    (> @CLEARED-LINES 10) 500
    :else 600))

(defn choose-new-piece!
  "Contract: nil -> string"
  []
  (set-active-piece! (nth NEXT-PIECE @PIECE-COUNT))
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
            (take num-cleared-lines (repeat EMPTY-LINE))
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

(defn read-high-score
  "Contract: string -> int
  Reads the players best score from a file"
  [fname]
  (read-string (slurp fname)))

(defn save-high-score
  "Contract: string -> int
  Saves the players score to a file."
  [fname]
  (spit fname (with-out-str (pr @SCORE))))

(defn overwrite-high-score!
  "Contract: nil -> nil"
  []
  (let [high-score (read-high-score HIGH-SCORE-FILE)]
    (when (> @SCORE high-score)
      (save-high-score HIGH-SCORE-FILE))))

(defn game-over!
  "Contract: nil -> nil
  Shows the game over message and exits when the player presses ESC key."
  []
  (overwrite-high-score!)
  (print-line! "**** GAME OVER ****" 0 false)
  (print-line! (str "YOUR SCORE - " @SCORE) 1 false)
  (print-line! (str "HIGH SCORE - " (read-high-score HIGH-SCORE-FILE)) 2 false)
  (clear-screen!)
  (let [input-key (console/get-key-blocking DISPLAY)]
    (if (= :escape input-key) (quit-game!)
      (recur))))

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
  (print-line! "*** GAME PAUSED ***" 0 false)
  (print-line! "*ANY KEY: CONTINUE*" 1 false)
  (print-line! (right-pad (str "*SCORE: " @SCORE) 19) 2 false)
  (clear-screen!)
  (console/get-key-blocking DISPLAY))

(defn read-input
  "Contract: nil -> nil"
  []
  (let [user-input (console/get-key DISPLAY)]
    (cond
      (= :left user-input) (move-left!)
      (= :right user-input) (move-right!)
      (= :down user-input) (move-down!)
      (= :up user-input) (hard-drop!)
      (= :escape user-input) (quit-game!)
      (= :enter user-input) (pause-game!)
      (= \p user-input) (pause-game!)
      (= \z user-input) (rotate-left!)
      (= \x user-input) (rotate-right!))))

(defn force-down!
  "Contract: nil -> nil
  Force the current active piece to move down on its own."
  []
  (when (>
         (- (System/currentTimeMillis) @LAST-MOVE-TIME)
         (get-game-speed))
    (swap! LAST-MOVE-TIME (fn [x] (System/currentTimeMillis)))
    (move-down!)))

(defn show-next-piece!
  "Contract: nil -> nil
  Displays the next tetris piece that the player will receive."
  []
  (print-line! "^^^ NEXT1 PIECE ^^^" 3 false)
  (let [next-piece-id (nth NEXT-PIECE @PIECE-COUNT)
        next-piece-graphics (get-graphics next-piece-id :CENTER)
        start-position (START-POSITIONS next-piece-id)
        padding (into [] (take 3 (repeat EMPTY-LINE)))
        x (first start-position)
        y (last start-position)
        offset 0]
    (print-matrix!
      (insert-piece
        next-piece-graphics padding x y)
      offset)))

(defn get-lowest-row
  "Contract: vector<vector> int int -> int
  Returns the lowest row that a piece can drop in the matrix."
  [piece-graphics current-row current-col]
  (let [new-x (inc current-row)
        new-y current-col]
    (cond
      (not= :in-bounds (check-bounds new-x new-y piece-graphics @MATRIX)) current-row
      (= :collision (detect-collision new-x new-y piece-graphics)) current-row
      :else (recur piece-graphics new-x new-y))))

(defn show-playfield!
  "Contract: nil -> nil
  Renders the playfield along with the current tetris piece and it's shadow.
  The shadow is the little preview at the bottom, that tells the player where the current tetris piece is going to land."
  []
  (let [shadow-graphics (map (fn [row] (map #(if (not= "." %) "v" %) row)) (:graphics @ACTIVE-PIECE))
        shadow-col (:col @ACTIVE-PIECE)
        shadow-row (get-lowest-row shadow-graphics (:row @ACTIVE-PIECE) shadow-col)
        playfield-with-shadow (insert-piece shadow-graphics @MATRIX shadow-row shadow-col)]
    (print-matrix!
      (insert-piece
        (:graphics @ACTIVE-PIECE) playfield-with-shadow (:row @ACTIVE-PIECE) (:col @ACTIVE-PIECE))
      MATRIX-START-ROW)))

(defn game-loop
  "Contract: nil -> nil"
  []
  (when (or (= "" (:id @ACTIVE-PIECE))
            (= true (:anchored @ACTIVE-PIECE)))
    (choose-new-piece!))
  (step!)
  (clear-screen!)
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
  (print-line! "***** TETRIS *****" 0 false)
  (print-line! "PRESS ANY KEY: PLAY" 1 false)
  (print-line! "PRESS ESC: QUIT" 2 false)
  (print-line! "AROW KEYS: MOVE" 3 false)
  (print-line! "PRESS Z: ROTATE L" 4 false)
  (print-line! "PRESS X: ROTATE R" 5 false)
  (print-line! "PRESS P: PAUSE" 6 false)
  (print-line! "PRESS ENTER: PAUSE" 7 false)
  (clear-screen!)
  (console/get-key-blocking DISPLAY))

(defn -main []
  (println "Done!") ;; Signal that we have loaded the program.
  (console/start DISPLAY)
  ;; Center the main window before showing the title screen.
  (-> WINDOW (.getJFrame) (.setLocationRelativeTo nil))
  (show-title-screen!)
  (clear-matrix!)
  (game-loop)) 
