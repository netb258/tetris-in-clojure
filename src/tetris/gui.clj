;; Our GUI is a command line interface with colors.
(ns tetris.gui
  (:require [lanterna.terminal :as t]
            [lanterna.screen :as console]
            [tetris.matrix :as m])
  (:import com.googlecode.lanterna.screen.Screen))

;; The window that will hold our game.
(def WINDOW (t/get-terminal :swing {:rows 26 :cols 19 :font-size 18}))
(def DISPLAY (new Screen WINDOW))
(def REDRAW-PAUSE 20)

(defn get-color
  [ch]
  (cond
    (or (= \b ch) (= \B ch)) {:fg :blue}
    (or (= \r ch) (= \R ch)) {:fg :red}
    (or (= \y ch) (= \Y ch)) {:fg :yellow :styles #{:bold}}
    (or (= \g ch) (= \G ch)) {:fg :green}
    (or (= \m ch) (= \M ch)) {:fg :magenta}
    (or (= \c ch) (= \C ch)) {:fg :cyan}
    (or (= \o ch) (= \O ch)) {:fg :yellow}
    (= \= ch) {:fg :white :styles #{:underline}}
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

(defn print-matrix!
  "Contract: vector<vector> int -> nil"
  [matrix offset]
  (flush)
  (if (empty? matrix) (recur (m/get-empty-matrix) offset)
    (let [lines (map #(clojure.string/join " " %) matrix)]
      (doseq [[line i] (map list lines (range (count lines)))]
        (print-line! line (+ i offset) true)))))

(defn get-key
  "Contract: nil -> char
  Does not block when listening for a keypress."
  []
  (console/get-key DISPLAY))

(defn get-key-blocking
  "Contract: nil -> char
  Blocks when listening for a keypress."
  []
  (console/get-key-blocking DISPLAY))

(defn start-gui
  "Contract: nil -> nil"
  []
  (console/start DISPLAY))

(defn center-gui
  "Contract: nil -> nil"
  []
  (-> WINDOW (.getJFrame) (.setLocationRelativeTo nil)))
