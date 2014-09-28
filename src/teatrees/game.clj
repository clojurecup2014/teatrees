(ns teatrees.game
  (:require [clojure.tools.logging :as log]
            [clojure.core.async :as async])
  (:import (java.util TimerTask Timer)))

(def x-max 10)
(def y-max 10)
(def z-max 21)
(def zborder 11)

(def available-games (ref (clojure.lang.PersistentQueue/EMPTY)))
(def current-games (ref {}))
(def global-events (async/chan))

(defn- success 
  [response] { :status :ok :body response })

(defn- failure
  ([response] (failure response :bad-request))
  ([response status] {:status status :body {:error response}}))

(defn make-uuid [] (keyword (str (java.util.UUID/randomUUID))))

(defn make-timer
  [uuid])

(defn try-join
  [name]
  (dosync
    (if (> (count @available-games) 0)
      (let [game-old (peek @available-games)
            uuid (:uuid game-old)
            game-new (assoc game-old :player2 name 
                                     :state :started
                                     :field []
                                     :player1-figure []
                                     :player2-figure []
                                     :timer (make-timer uuid default-rate))]
        (alter available-games pop)
        (alter current-games conj game-new)
        (dissoc game-new :timer))
      (let [gm { :uuid (make-uuid) 
                 :player1 name 
                 :player2 false 
                 :state :awaiting }]
        (alter available-games conj gm)
        gm))))

(defn high-scores
  []
  (success "empty"))

(defn field-state
  [uuid]
  (success "ok"))

(defn available
  []
  (success @available-games))

(defn disconnect
  [uuid playerid]
  (success "ok"))

(defn move-figure
  [uuid dir playernm]
  (if-let [ch (@game-channels uuid)]
    ()
    (failure "Game not found" :internal-server-error)))

;; Game implementation

(def square-templ { :type :square
                    :center 2
                    :fig [{:x 0, :y 0, :z 0}
                          {:x 1, :y 0, :z 0}
                          {:x 0, :y 1, :z 0}
                          {:x 1, :y 1, :z 0}]})

(def line-templ { :type :line
                  :center 2
                  :fig [{:x 0, :y 0, :z 0}
                        {:x 1, :y 0, :z 0}
                        {:x 2, :y 0, :z 0}
                        {:x 3, :y 0, :z 0}]})

(def arrow-templ { :type :arrow
                   :center 2
                   :fig [{:x 0, :y 0, :z 0}
                         {:x 1, :y 0, :z 0}
                         {:x 2, :y 0, :z 0}
                         {:x 1, :y 1, :z 0}]})

(def angle-r-templ { :type :anlge-r
                     :center 2
                     :fig [{:x 0, :y 0, :z 0}
                     {:x 1, :y 0, :z 0}
                     {:x 2, :y 0, :z 0}
                     {:x 2, :y 1, :z 0}] })

(def angle-l-templ { :type :angle-l
                     :center 2
                     :fig [{:x 0, :y 0, :z 0}
                           {:x 1, :y 0, :z 0}
                           {:x 2, :y 0, :z 0}
                           {:x 0, :y 1, :z 0}]})

(def snake-l-templ { :type :snake-l
                     :center 2
                     :fig [{:x 0, :y 0, :z 0}
                           {:x 1, :y 0, :z 0}
                           {:x 1, :y 1, :z 0}
                           {:x 2, :y 1, :z 0}]})

(def snake-r-templ { :type :snake-r
                     :center 2
                     :fig [{:x 0, :y 1, :z 0}
                           {:x 1, :y 1, :z 0}
                           {:x 1, :y 0, :z 0}
                           {:x 2, :y 0, :z 0}]})

(declare rotate)

(def figures [square-templ 
              line-templ
              arrow-templ
              angle-r-templ
              angle-l-templ
              snake-l-templ
              snake-r-templ])

(defn make-figure
  [player]
  (case player
    :player1 (let [figure (get figures (rand-int (count figures)))]
               (map #{}))
    :player2 (let [figure ((get figures (rand-int (count figures))))]
               (map #{}))))

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
    (let [fg (if (can-move? dir figure field)
                   (do
                     (log/info "Going move" dir)
                     (case dir
                      :left  (map #(assoc % :x (dec (:x %))) figure)
                      :right (map #(assoc % :x (inc (:x %))) figure)
                      :up    (map #(assoc % :y (inc (:y %))) figure)
                      :down  (map #(assoc % :y (dec (:y %))) figure)))
                   figure)]
          {:field field :figure fg})))

(defn rotate
  [figure center axis direction]
  (let [rest-axes (remove #{axis} [:x :y :z])
        rot-fn (case direction
                 :cw (fn [[x y]] [y (- x)])
                 :ccw (fn [[x y]] [(- y) x]))]
    (for [cell figure
          :let [norm-cell (merge-with - cell center)
                rot-vals (map norm-cell rest-axes)
                rotated-vals (rot-fn rot-vals)
                rotated-cell (merge norm-cell
                                    (apply hash-map (interleave rest-axes rotated-vals)))
                new-cell (merge-with + rotated-cell center)]]
      new-cell)))



(async/go-loop []
  (when-let [[type & params] (async/alts! @game-channels)]
    (case type
      :start ()
      :move ()
      :finish ())
    (recur))))]











