(ns teatrees.game
  (:require [clojure.tools.logging :as log]
            [clojure.core.async :as async]))

(def x-max 5)
(def y-max 5)
(def z-max 40)

(defrecord Game [id field pl1id pl2id])

;; TODO: Write figures templates

(defn prohibited-fields [f figure] (into #{} (map f figure)))

(defn check-dir 
  [f figure field]
  (nil? (some (prohibited-fields f figure) field)))

(defn can-move?
  [field figure dir]
  (let [xb (dec x-max)
        yb (dec y-max)]
    (case dir
      :left  (and 
               (check-dir #(assoc % :x (dec (:x %))) figure field)
               (> (:x (apply min-key :x figure)) 0))
      :up    (and
               (check-dir #(assoc % :y (inc (:y %))) figure field)
               (< (:y (apply max-key :y figure)) yb))
      :down  (and
               (check-dir #(assoc % :y (dec (:y %))) figure field)
               (> (:y (apply min-key :y figure)) 0))
      :right (and 
               (check-dir #(assoc % :x (inc (:x %))) figure field)
               (< (:x (apply max-key :x figure)) xb)))))

(defn move
  [field figure])