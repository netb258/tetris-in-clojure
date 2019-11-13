;; Functions that draw the game.
(ns tetris.gui
  (:require [quil.core :as q]
            [tetris.matrix :as m])
  (:import java.awt.event.KeyEvent))

(def WINDOW-WIDTH 250)
(def WINDOW-HEIGHT 650)
(def SQUARE-WIDTH 25)
(def SQUARE-HEIGHT 25)
(def FPS 50)

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

(defn show-pause-menu! [score lines high-score high-lines]
  (q/background 255 255 255)
  (q/stroke 0 0 0)
  (q/fill 0 0 0)
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
  (q/text (str "*SCORE: " score) 0 230)
  (q/text (str "*LINES: " lines) 0 250)
  (q/text (str "HIGH SCORE: " high-score) 0 270)
  (q/text (str "HIGH LINES: " high-lines) 0 290))

(defn show-game-over-screen! [score lines high-score high-lines]
  (q/background 0 0 0)
  (q/stroke 255 255 255)
  (q/fill 255 255 255)
  (q/text-size 20)
  (q/text-align :center)
  (q/text "GAME OVER" (/ WINDOW-WIDTH 2) 50)
  (q/text-size 15)
  (q/text-align :left)
  (q/text (str "YOUR SCORE: " score) 0 90)
  (q/text (str "YOUR LINES: " lines) 0 120)
  (q/text (str "HIGH SCORE: " high-score) 0 150)
  (q/text (str "HIGH LINES: " high-lines) 0 180))
