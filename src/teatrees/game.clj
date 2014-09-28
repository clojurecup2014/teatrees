(ns teatrees.game
  (:require [clojure.tools.logging :as log]
            [clojure.core.async :as async]
            [clj-time.core :refer [before? now ago minutes]]))

(def x-max 10)
(def y-max 10)
(def z-max 21)
(def zborder 11)

(def rate 1000)
(def keep-results (* 60 1000))

(def available-games (ref (clojure.lang.PersistentQueue/EMPTY)))
(def current-games (ref {}))

(def events (async/chan))

(defn- success 
  [response] { :status :ok :body response })

(defn- failure
  ([response] (failure response :bad-request))
  ([response status] {:status status :body {:error response}}))

(defn make-timer
  [uuid]
  (future
    (loop []
      (when-let [cg (@current-games uuid)]
        (case (cg :state)
          :finished (do
                      (Thread/sleep keep-results)
                      (dosync (alter current-games dissoc uuid)))
          :started (do
                     (async/>!! events [:move uuid "1" :bottom])
                     (async/>!! events [:move uuid "2" :top])
                     (Thread/sleep rate)
                     (recur)))))))

(defn make-uuid [] (str (java.util.UUID/randomUUID)))

(declare place-new-fig)

(defn try-join
  [name]
  (dosync
    (if (> (count @available-games) 0)
      (let [game-old (peek @available-games)
            game-new (-> game-old
                         (assoc :state :started
                                :field (map place-new-fig [1 2]))
                         (update-in [:players] conj {:name name :score 0}))
            uuid (:uuid game-new)]
        (log/info "Found room" uuid "Starting...")
        (alter available-games pop)
        (alter current-games assoc uuid game-new)
        (make-timer uuid)
        (success (assoc game-new :player-no 1)))
      (let [gm { :uuid (make-uuid)
                 :border-pos (int (/ z-max 2))
                 :players [{:name name :score 0}]
                 :field []
                 :state :awaiting }]
        (dosync
          (log/info "Created room" (gm :uuid))
          (alter available-games conj gm)
          (success (assoc (select-keys gm [:uuid :state]) :player-no 0)))))))

(defn high-scores
  []
  (success "empty"))

(defn awaiting-state
  [uuid]
  (if-let [game (@current-games uuid)]
    (success game)
    (success {:state :awaiting})))

(defn field-state
  [uuid]
  (if-let [game (@current-games uuid)]
    (success game)
    (success {:state :unknown})))

(defn available
  []
  (success @available-games))

(defn disconnect
  [uuid playerid]
  (success "ok"))

(defn move-figure
  [uuid dir player]
  (if (@current-games uuid)
    (do
      (case player)
        "1" (when-not (= dir :top)
              (async/go (async/>! events [:move uuid player :dir])))
        "2" (when-not (= dir :bottom)
              (async/go (async/>! events [:move uuid player :dir])))
      (success "ok"))
    (failure "Game not found" :internal-server-error))) 

(defn rotate-figure
  [uuid dir axis player]
  (if (@current-games uuid)
    (async/go (async/>! events [:rotate uuid dir axis player]))
    (failure "Game not found" :internal-server-error)))

;; Game implementation

(def square [{ :x 0, :y 0, :z 0 }
             { :x 0, :y 1, :z 0 } 
             { :x 1, :y 0, :z 0 }
             { :x 1, :y 1, :z 0 }])

(def line   [{ :x -1, :y 0, :z 0 }
             { :x 0, :y 0, :z 0 } 
             { :x 1, :y 0, :z 0 }
             { :x 2, :y 0, :z 0 }])

(def arrow  [{ :x -1, :y 0, :z 0 }
             { :x 0, :y 0, :z 0 } 
             { :x 0, :y 1, :z 0 }
             { :x 1, :y 0, :z 0 }])

(def angle-l [{ :x -1, :y 1, :z 0 }
              { :x -1, :y 0, :z 0 } 
              { :x 0, :y 0, :z 0 }
              { :x 1, :y 0, :z 0 }])

(def angle-r [{ :x -1, :y 0, :z 0 }
              { :x 0, :y 0, :z 0 } 
              { :x 1, :y 0, :z 0 }
              { :x 1, :y 1, :z 0 }])

(def snake-l [{ :x -1, :y 0, :z 0 }
              { :x 0, :y 0, :z 0 } 
              { :x 0, :y 1, :z 0 }
              { :x 1, :y 1, :z 0 }])

(def snake-r [{ :x -1, :y 1, :z 0 }
              { :x 0, :y 1, :z 0 } 
              { :x 0, :y 0, :z 0 }
              { :x 1, :y 0, :z 0 }])

(def figures [square line arrow angle-l angle-r snake-l angle-r])

(defn rotate* [direction axis figure center]
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

(defn can-rotate?
  [dir axis figure field zmin zmax]
  (let [center (second figure)
        prohibited (into #{} (rotate* figure center axis dir))]
    (and 
      (nil? (some prohibited field))
      (every? #(and (> % -1) (< % x-max)) (map :x prohibited))
      (every? #(and (> % -1) (< % y-max)) (map :y prohibited))
      (every? #(and (> % zmin) (< % z-max)) (map :z prohibited)))))

(defn rotate
  [direction axis figure field player]
  (let [center (second figure)
        [zmin zmax] (case player
                      "1" [-1 zborder]
                      "2" [zborder z-max])]
    (if (can-rotate? direction axis figure field zmin zmax)
      (rotate* direction axis figure center)
      figure)))

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

(defn call-rotate!
  [uuid dir axis player]
  (when-let [cg (@current-games uuid)]
    (let [pl (dec (Integer/parseInt player))
          fig ((:players cg) pl)
          field (cg :field)
          rotated (rotate dir axis fig field player)]
      (dosync
        (alter current-games assoc-in [uuid :players pl :figure] rotated)))))

(defn- calc-shift-clean
  [cell cmp line-vec zb]
  (count (filter #(cmp (cell :z) % zb) line-vec)))

(defn- shift-cell
  [cell shift op]
  (assoc cell :z (op (cell :z) shift)))

(defn- calc-and-shift
  [cell cmp line-vec zb op]
  (let [shift (calc-shift-clean cell cmp line-vec zb)]
    (shift-cell cell shift op)))

(defn cleanise-field
  [dir line-vec field zb]
  (let [[cmp op] (case dir
                  :top [> -]
                  :bottom [< +])]
    (map #(calc-and-shift % cmp line-vec zb op) field)))

(defn place-new-fig
  [player]
  (let [fig (rand-nth figures)
        shift {:x (/ x-max 2)
               :y (/ y-max 2)
               :z (if (= player 1) 0 (dec z-max))}
        moved-fig (map #(merge-with + % shift) fig)]
    moved-fig))

(defn row-is-full?
  [field row]
  (let [need-cells (* x-max y-max)
        row-cells (filter #(= row (:z %)) field)]
    (= need-cells (count row-cells))))

(defn rows-to-remove
  [field figure]
  (->> figure
       (map :z)
       set
       sort
       (filter #(row-is-full? field %))))

(defn move
  [dir figure field player]
  (let [[axis dir-fn] (case dir
                        :bottom [:z inc]
                        :top [:z dec]
                        :left [:x dec]
                        :right [:x inc]
                        :up [:y inc]
                        :down [:y dec])]
    (cond
      (can-move? dir figure field)
        (do
          (log/info "Going move" dir)
          {:field field
           :figure (map #(assoc % axis (dir-fn (axis %))) figure)})
      (or (and (= player 1) (= dir :bottom)))
        {:field (merge field figure)
         :figure nil
         :failed (not (can-move? :top figure field))}
      (or (and (= player 2) (= dir :top)))
        {:field (merge field figure)
         :figure nil
         :failed (not (can-move? :bottom figure field))}
      :else
        {:field field :figure figure})))

(defn move!
  [uuid player dir]
  (let [game (@current-games uuid)
        field (:field game)
        figure (get-in game [:players player :figure])
        {new-field :field new-fig :figure failed :failed}
          (move dir figure field player)
        no-rows (if new-fig (rows-to-remove new-field new-fig) [])
        cleansed-field (cleanise-field dir no-rows new-field (game :border-pos))
        new-fig (if new-fig
                  new-fig
                  (place-new-fig player))
        new-game (-> game
                     (assoc :field cleaned-field)
                     (assoc-in [:players player :figure] new-fig))]
    (dosync
      (alter current-games assoc uuid game))
    (when failed
      (async/go
        (async/>!! events [:finished uuid player])))))
  
(async/go-loop []
  (let [[[action & msg] _] (async/alts! [events])]
    (log/info action)
    (case action
      :move (let [[uuid player dir] msg]
              (log/info "Accepted move message." msg)
              (move! uuid player dir))
      :rotate (apply call-rotate! msg)
      :placed (log/info "Accepted placed message." msg)
      ;; :destroyed (apply shift-field* msg) ;; expects [uuid line-vec player]
      :finished (log/info "Accepted finished message." msg)))
  (recur))
