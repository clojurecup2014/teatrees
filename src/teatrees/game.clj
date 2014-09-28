(ns teatrees.game
  (:require [clojure.tools.logging :as log]
            [clojure.core.async :as async]))

(def x-max 10)
(def y-max 10)
(def z-max 21)
(def zborder 11)

(def available-games (ref (clojure.lang.PersistentQueue/EMPTY)))
(def current-games (ref {}))
(def game-channels (ref {}))

(defn- success 
  [response] { :status :ok :body response })

(defn- failure
  ([response] (failure response :bad-request))
  ([response status] {:status status :body {:error response}}))

(defn make-uuid [] (keyword (str (java.util.UUID/randomUUID))))

(defn try-join
  [name]
  (dosync
    (if (> (count @available-games) 0)
      (let [game-old (peek @available-games)
            game-new (assoc game-old :player2 name 
                                     :state :started)
            uuid (:uuid game-new)]
        (alter available-games pop)
        (alter current-games conj game-new)
        (alter game-channels assoc uuid (async/chan))
        game-new)
      (let [gm { :uuid (make-uuid) 
                 :player1 name 
                 :player2 false 
                 :state :awaiting }]
        (dosync
          (alter available-games conj gm)
          gm)))))

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