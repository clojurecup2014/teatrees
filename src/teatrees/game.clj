(ns teatrees.game
  (:require [clojure.tools.logging :as log]
            [clojure.core.async :as async]
            [clj-time.core :refer [before? now ago minutes]]))

(def x-max 7)
(def y-max 7)
(def z-max 21)
(def zborder 10)

(def rate 2000)
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
  (async/thread
    (log/info "Created timer for" uuid)
    (loop []
      (when-let [cg (@current-games uuid)]
        (Thread/sleep rate)
        (case (cg :state)
          :finished (do
                      (Thread/sleep keep-results)
                      (log/info "Cleaning game" uuid)
                      (dosync (alter current-games dissoc uuid)))
          :started (do
                     (async/>!! events [:move uuid 1 :bottom])
                     (async/>!! events [:move uuid 2 :top])
                     (recur)))))))

(defn make-uuid [] (str (java.util.UUID/randomUUID)))

(declare place-new-fig)

(defn try-join
  [name]
  (dosync
    (if (> (count @available-games) 0)
      (let [game-old (peek @available-games)
            game-new (-> game-old
                         (assoc :state :started)
                         (assoc-in [:players 0 :fig] (place-new-fig 1))
                         (update-in [:players] conj {:name name
                                                     :score 0
                                                     :fig (place-new-fig 2)}))
            uuid (:uuid game-new)]
        (log/info "Found room" uuid "Starting...")
        (alter available-games pop)
        (alter current-games assoc uuid game-new)
        (make-timer uuid)
        (success (assoc game-new :player-no 1)))
      (let [gm { :uuid (make-uuid)
                 :border-pos zborder
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
      (case player
        "1" (when-not (= dir :top)
              (async/go (async/>! events [:move uuid 1 dir])))
        "2" (when-not (= dir :bottom)
              (async/go (async/>! events [:move uuid 2 dir]))))
      (success "ok"))
    (failure "Game not found" :internal-server-error))) 

(defn rotate-figure
  [uuid axis dir player]
  (if (@current-games uuid)
    (do
      (async/go (async/>! events [:rotate uuid dir axis player]))
      (success "ok"))
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
                 1 (fn [[x y]] [y (- x)])
                 -1 (fn [[x y]] [(- y) x]))]
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
        prohibited (into #{} (rotate* dir axis figure center))]
    (and 
      (nil? (some prohibited field))
      (every? #(and (> % -1) (< % x-max)) (map :x prohibited))
      (every? #(and (> % -1) (< % y-max)) (map :y prohibited))
      (every? #(and (> % zmin) (< % zmax)) (map :z prohibited)))))

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
  [dir figure field zborder]
  (let [xb (dec x-max)
        yb (dec y-max)
        zb zborder]
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
                (< (:z (apply max-key :z figure)) (dec zb)))
      :top    (and 
                (check-dir #(assoc % :z (dec (:z %))) figure field)
                (> (:z (apply min-key :z figure)) (inc zb))))))

(defn call-rotate!
  [uuid dir axis player]
  (when-let [cg (@current-games uuid)]
    (let [pl (dec (Integer/parseInt player))
          fig (get-in cg [:players pl :fig])
          field (cg :field)
          rotated (rotate dir axis fig field player)]
      (dosync
        (alter current-games assoc-in [uuid :players pl :fig] rotated)))))

(defn cleanise-field
  [dir line-vec field zb]
  (let [[cmp op] (case dir
                  :top [> -]
                  :bottom [< +])
        line-set (set line-vec)
        rem-cnt (count line-vec)
        clean-field (remove #(line-set (:z %)) field)]
    (log/info "Cleanse called with" line-set)
    (for [{z :z :as cell} clean-field
          :let [removed-under (count (filter #(cmp z %) line-vec))]]
      (assoc cell :z (op z rem-cnt removed-under)))))

(defn place-new-fig
  [player]
  (let [fig (rand-nth figures)
        shift {:x (int (/ x-max 2))
               :y (int (/ y-max 2))
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
  (let [result (->> figure
                 (map :z)
                 set
                 sort
                 (filter #(row-is-full? field %)))]
    result))

(defn fig-at-start?
  [figure player]
  (let [[f r] (if (= player 1) [min 0] [max (dec z-max)])
        closest (apply f (map :z figure))]
    (= closest r)))

(defn move
  [dir figure field player zborder]
  (let [[axis dir-fn] (case dir
                        :bottom [:z inc]
                        :top [:z dec]
                        :left [:x dec]
                        :right [:x inc]
                        :up [:y inc]
                        :down [:y dec])]
    (cond
      (can-move? dir figure field zborder)
        (do
          (log/info "Going move" dir)
          {:field field
           :figure (map #(assoc % axis (dir-fn (axis %))) figure)})
      (and (= player 1) (= dir :bottom))
        {:field (concat field figure)
         :figure nil
         :failed (fig-at-start? figure player)}
      (and (= player 2) (= dir :top))
        {:field (concat field figure)
         :figure nil
         :failed (fig-at-start? figure player)}
      :else
        {:field field :figure figure})))

(defn fall
  [field figure player zborder]
  (let [dir (if (= player 1) :bottom :top)]
    (loop [field field
           figure figure]
      (let [{new-field :field new-fig :figure :as mv-state} (move dir figure field player zborder)]
        (if new-fig
          (recur new-field new-fig)
          {:field field :figure figure})))))

(defn move!
  [uuid player dir]
  (let [{:keys [field border-pos] :as game} (@current-games uuid)
        figure (get-in game [:players (dec player) :fig])
        {new-field :field new-fig :figure failed :failed}
          (if (= dir :fall)
            (fall field figure player border-pos)
            (move dir figure field player border-pos))
        no-rows (if new-fig [] (rows-to-remove new-field figure))
        cleanise-dir (if (= player 1) :bottom :top)
        cleansed-field (if (seq no-rows)
                         (cleanise-field cleanise-dir no-rows new-field (game :border-pos))
                         new-field)
        shift-num (* (count no-rows) (if (= player 1) 1 -1))
        new-border (+ border-pos shift-num)
        shifted-field (if (seq no-rows)
                        (map #(assoc % :z (+ (:z %) shift-num)) cleansed-field)
                        new-field)
        score (if new-fig 0 (count figure))
        line-score (* (count no-rows) 200)
        new-fig (if new-fig
                  new-fig
                  (place-new-fig player))
        new-game (-> game
                   (assoc :field cleansed-field)
                   (assoc :border-pos new-border)
                   (update-in [:players (dec player) :score] + score line-score)
                   (assoc-in [:players (dec player) :fig] new-fig))]
    (dosync
      (alter current-games assoc uuid new-game))
    (when failed
      (async/go
        (async/>! events [:finished uuid player])))))

(defn finish!
  [uuid player]
  (dosync
    (alter current-games assoc-in [uuid :state] :finished)))
  
(async/go-loop []
  (let [[[action & msg] _] (async/alts! [events])]
    (log/info action)
    (case action
      :move (let [[uuid player dir] msg]
              (move! uuid player dir))
      :rotate (apply call-rotate! msg)
      :placed (log/info "Accepted placed message." msg)
      :finished (let [[uuid player] msg]
                  (log/info "Accepted finished message." msg)
                  (finish! uuid player))))
  (recur))
