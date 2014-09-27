(ns teatrees.game
  (:require [clojure.tools.logging :as log]
            [clojure.core.async :as async]))

(def x-max 5)
(def y-max 5)
(def z-max 10)
(def zborder 5)

(defrecord Game [id field pl1id pl2id])

;; TODO: Write figures templates

(defn prohibited-fields [f figure] (into #{} (map f figure)))

(defn check-dir 
  [f figure field]
  (nil? (some (prohibited-fields f figure) field)))

(defn can-move?
  [dir figure field]
  (let [xb (dec x-max)
        yb (dec y-max)
        zb (dec zborder)]
    (case dir
      :left   (and 
                (check-dir #(assoc % :x (dec (:x %))) figure field)
                (> (:x (apply min-key :x figure)) 0))
      :up     (and
                (check-dir #(assoc % :y (inc (:y %))) figure field)
                (< (:y (apply max-key :y figure)) yb))
      :down   (and
                (check-dir #(assoc % :y (dec (:y %))) figure field)
                (> (:y (apply min-key :y figure)) 0))
      :right  (and 
                (check-dir #(assoc % :x (inc (:x %))) figure field)
                (< (:x (apply max-key :x figure)) xb))
      :bottom (and 
                (check-dir #(assoc % :z (inc (:z %))) figure field)
                (< (:z (apply max-key :z figure)) zb))
      :top    (and 
                (check-dir #(assoc % :z (dec (:z %))) figure field)
                (> (:z (apply max-key :z figure)) zb)))))

(defn move
  [dir figure field]
  (case dir
    :bottom (if (can-move? dir figure field)
              {:field field :figure (map #(assoc % :z (inc (:z %))) figure)}
              {:field (merge field figure) :figure nil})
    :top    (if (can-move? dir figure field)
              {:field field :figure (map #(assoc % :z (dec (:z %))) figure)}
              {:field (merge field figure) :figure nil})
    (let [fg (if (can-move? dir field figure)
                   (do
                     (log/info "Going move" dir)
                     (case dir
                      :left  (map #(assoc % :x (dec (:x %))) figure)
                      :right (map #(assoc % :x (inc (:x %))) figure)
                      :up    (map #(assoc % :y (inc (:y %))) figure)
                      :down  (map #(assoc % :y (dec (:y %))) figure)))
                   figure)]
          {:field field :figure fg})))



















