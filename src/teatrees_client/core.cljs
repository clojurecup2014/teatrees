(ns teatrees-client.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [goog.events :as events]
            [om-tools.core :refer-macros [defcomponent]]
            [om-tools.dom :as dom :include-macros true]
            [goog.Timer :as timer]
            [om.core :as om :include-macros true]
            ; [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.string :as str]
            [clojure.set :as set]
            [teatrees-client.utils :as utils :refer [edn-xhr]])
  (:import [goog History]
           [goog.history EventType]))

(enable-console-print!)

(def app-state
  (atom
    {:game-state :welcome
     :game-field-size {:x 10 :y 10 :z 31}
     :cell-size 20
     :cell-gap 2
     :game-field []
     :border-pos 15
     :uuid nil
     :player-name ""
     :player-no 0
     ; :players (vec (repeat 2 {:name ""
     ;                          :score 0
     ;                          :figure []}))
     }))

;; =================================
;; App lifecycle

(defn end-game [app data]
  (om/update! app :game-state :ended))

(defn update-received [app data timer]
  (let [{:keys [game-state game-field players]} data]
    (when (= game-state :ended)
      (.stop timer)
      (end-game app data))
    (om/update! app :game-field game-field)
    (om/transact! app :players
      #(map merge % players))))

(defn game-poll [app timer]
  (edn-xhr {:method :get
            :url "request-update"
            :data {:uuid (:uuid app)}
            :on-complete #(update-received app % timer)}))

(defn start-game [app]
  (om/update! app :game-state :running)
  (let [timer (goog.Timer. 1000)]
    (.start timer)
    (events/listen timer goog.Timer/TICK #(game-poll app timer))))

(defn wait-poll [app timer]
  (edn-xhr {:method :get
            :url "join"
            :data {:uuid (:uuid app)}
            :on-complete
              (fn [{:keys [status] :as data}]
                (when (= status :start)
                  (.stop timer)
                  (start-game app)))}))

(defn wait-for-start [app]
  (om/update! app :game-state :waiting)
  (let [timer (goog.Timer. 1000)]
    (.start timer)
    (events/listen timer goog.Timer/TICK #(wait-poll app timer))))

(defn end-game [app data]
  (om/update! app :game-state :ended))

(defn start-or-wait [app data]
  (om/update! app :uuid (:uuid data))
  (case (:status data) 
    :wait (wait-for-start app)
    :start (start-game app)))

(defn try-join-game-test [app name]
  (start-or-wait app
                 {:status :start
                  :uuid 1
                  :update {:game-field [{:id 0 :x 0 :y 0 :z 0}
                                        {:id 1 :x 0 :y 1 :z 0}
                                        {:id 2 :x 1 :y 1 :z 0}
                                        {:id 3 :x 1 :y 1 :z 1}]
                           :players [{:name "Player 1"
                                      :score 0
                                      :figure [{:id 10 :x 5 :y 5 :z 38}
                                               {:id 11 :x 5 :y 5 :z 39}
                                               {:id 12 :x 6 :y 5 :z 39}
                                               {:id 13 :x 5 :y 5 :z 37}]}]}}))

(defn try-join-game [app name]
  (edn-xhr {:method :get
            :url "join"
            :data {:name name}
            :on-complete #(start-or-wait app %)}))

;; =================================
;; Interaction

(def keycodes
  "https://code.google.com/p/closure-library/source/browse/closure/goog/events/keycodes.js?r=70795893b3841c22b412de14f9ecf8331f2e7073"
  {37 :left
   38 :up
   39 :right
   40 :down
   32 :space
   65 :A
   81 :Q
   83 :S
   87 :W
   88 :X
   90 :Z})

(defn event->msg [e]
  (if-let [k (keycodes (.-keyCode e))]
    (let [transform
          (case k
            :left {:t {:x -1}}
            :up {:t {:y 1}}
            :right {:t {:x 1}}
            :down {:t {:y -1}}
            :space {:t {:z -1}}
            :Q {:r {:x -1}}
            :W {:r {:x 1}}
            :A {:r {:y -1}}
            :S {:r {:y 1}}
            :Z {:r {:z -1}}
            :X {:r {:z 1}})]
    {:type :keydown
     :transform transform})))

(defn reg-events [ch]
  (events/listen (.-body js/document)
                 (.-KEYDOWN events/EventType)
                 #(put! ch (event->msg %))))
 
;; =================================
;; Presentation

(def view-defs
  {:front {:dims [:x :z :y] :dirs [1 -1 1]}
   :back {:dims [:x :z :y] :dirs [-1 -1 -1]}
   :left {:dims [:y :z :x] :dirs [-1 -1 1]}
   :right {:dims [:y :z :x] :dirs [1 -1 -1]}
   :top {:dims [:x :y :z] :dirs [1 -1 -1]}
   :bottom {:dims [:x :y :z] :dirs [1 1 1]}})

(defn view-visible-dims [view]
  (->> view-defs view :dims (take 2)))

(defn project-game-field
  ([field field-size view-from]
   (let [{:keys [dims dirs]} (view-from view-defs)]
     (project-game-field field field-size dims dirs)))
  ([field field-size dims dirs]
    (let [view-dims (take 2 dims)
          depth-dim (last dims)
          norm-coord (fn [value size invert]
                            (if invert (- size 1 value) value))
          dir-map (apply hash-map (interleave dims dirs))
          norm-field (for [cell field]
                       (reduce
                        (fn [cell dim]
                          (assoc cell dim (norm-coord (dim cell) (dim field-size) (-> dim dir-map neg?))))
                        cell dims))]
      (->> norm-field
           (group-by #(select-keys % view-dims))
           (map (fn [[plane grouped]]
                  (let [depth-rec (apply min-key depth-dim grouped)
                        depth-rec-conv (set/rename-keys
                                        (apply dissoc depth-rec view-dims)
                                        {depth-dim :depth})]
                    (assoc depth-rec-conv :x (plane (first view-dims))
                                          :y (plane (second view-dims))))))))))

(defn coord-converter [cell-size cell-gap]
  (fn [pos] (* pos (+ cell-size cell-gap))))

(defn calc-cell-pos [conv {:keys [x y]}]
  (map conv [x y]))

(defcomponent game-field-cell [{:keys [cell conv cell-size cell-gap]} owner]
  (render-state [_ state]
    (let [[x y] (calc-cell-pos conv cell)]
      (dom/rect {:class "cell"
                 :x x :y y :width cell-size :height cell-size}))))

(defn bg-for-view [view width-px height-px conv border cell-size cell-gap]
  (condp contains? view
    #{:front :back :left :right}
      (let [border-px (conv border)
            p2-y (+ border-px cell-size cell-gap)]
        (println {:p2-y p2-y :border-px border-px :width-px width-px :height-px height-px})
        [(dom/rect {:class "view-bg-player1"
                    :width width-px
                    :height (- border-px cell-gap)})
         (dom/rect {:class "view-bg-border"
                    :y border-px
                    :width width-px
                    :height cell-size})
         (dom/rect {:class "view-bg-player2"
                    :y p2-y
                    :width width-px
                    :height (- height-px p2-y)})])
    #{:top}
      [(dom/rect {:class "view-bg-player1"
                  :width width-px
                  :height height-px})]
    #{:bottom}
      [(dom/rect {:class "view-bg-player2"
                  :width width-px
                  :height height-px})]))

(defcomponent game-field-view [app owner {:keys [ch view] :as opts}]
  (render [_]
    (let [{:keys [game-field game-field-size cell-size cell-gap]} app
          proj (project-game-field game-field game-field-size view)
          conv (coord-converter cell-size cell-gap)
          visible-dims (view-visible-dims view)
          view-size (map game-field-size visible-dims)
          [width-px height-px] (map #(- (conv %) cell-gap) view-size)
          ]
      (dom/div {:class "col-xs-3 game-view"}
        (dom/h3 (-> view name str/capitalize))
        (dom/svg {:width width-px
                  :height height-px}
          (bg-for-view view width-px height-px conv (:border-pos app) cell-size cell-gap)
          (for [cell proj]
            (om/build game-field-cell {:cell cell
                                       :conv conv
                                       :cell-size cell-size
                                       :cell-gap cell-gap})))))))

(defcomponent game-field [app owner {:keys [ch] :as opts}]
  (render [_]
    (let [views [:top :left :front]
          {:keys [game-field game-field-size]} app]
      (dom/div {:class "container-fluid"
          :id "tetris"
          :ref "tetris"}
        (dom/div {:class "row game-field"}
          (om/build game-field-view app {:opts {:view :top}})
          (om/build game-field-view app {:opts {:view :left}})
          (om/build game-field-view app {:opts {:view :front}})
          (om/build game-field-view app {:opts {:view :bottom}}))))))

(defn calc-cell-size-and-gap [field-size window-size]
  (let [min-x (/ (:x window-size) 4 (:x field-size))
        min-y (/ (:y window-size) (:z field-size))
        min-size (min min-x min-y)
        full-size (Math/floor min-size)
        gap (Math/floor (/ full-size 10))
        cell-gap (if (zero? gap) 1 gap)
        cell-size (- full-size cell-gap)]
    [cell-size cell-gap]))

(defn window-resized [app owner]
  (let [{:keys [game-field-size]} (om/value app)
        win-size {:x (- (.-innerWidth js/window) 20)
                  :y (- (.-innerHeight js/window) 20)}
        [cell-size cell-gap] (calc-cell-size-and-gap game-field-size
                                                     win-size)]
    (om/update! app :cell-size cell-size)
    (om/update! app :cell-gap cell-gap)))

(defn monitor-window-resize [app owner]
  (events/listen js/window
                 (.-RESIZE events/EventType)
                 #(window-resized app owner)))

(defn handle-event [app msg]
  (println "msg received" msg)
  (case (:type msg)
    :try-join-game
      (try-join-game app (:name msg))))

(defcomponent footer [app owner]
  (render [_]
    (dom/div {:class "footer"}
      (dom/a {:href "https://github.com/clojurecup2014/teatrees"} "Tea Trees")
      " by Bumblebears (2014)")))

(defcomponent welcome [app owner {:keys [ch] :as opts}]
  (render [_]
    (dom/div {:class "container small-container"}
      (dom/div {:class "jumbotron"}
        (dom/h1 "Tea Trees")
        (dom/p "3d tetris")
        (dom/form {:on-submit (fn [e]
                                (.preventDefault e)
                                (put! ch {:type :try-join-game
                                          :name (.-value (om/get-node owner "username"))}))}
          (dom/div {:class "form-group"}
            (dom/label {:for "username"} "Enter your name")
            (dom/input {:class "form-control"
                        :id "username"
                        :ref "username"}))
          (dom/button {:class "btn btn-primary"} "Start!")))
      (om/build footer app))))

(defcomponent waiting [app owner]
  (render [_]
    (dom/div {:class "container small-container"}
      (dom/h1 "Tea Trees")
      (dom/p (:player-name app)
             ", we're waiting for another player to join...\n"
             "Don't run away as it won't take long."))))

(defcomponent game-ended [app owner]
  (render [_]
    (dom/div {:clas "contianer small-container"}
      (dom/h1 "Tea Trees")
      (dom/h2 "Game over")
      (let [winner (apply max-key :score (:players app))]
        (dom/p (:name winner) " won, let's congratulate him (or her)!"))
      (dom/button {:class "btn btn-primary"
                   :role "button"
                   :on-click #(try-join-game app (:player-name app))} "Play again"))))

(defcomponent tetris [app owner]
  (init-state [_]
    {:ch (chan)})
  (will-mount [_]
    (let [ch (om/get-state owner :ch)]
      (go (loop []
        (let [msg (<! ch)]
          (handle-event app msg)
          (recur))))))
  (did-mount [_]
    (monitor-window-resize app owner)
    (window-resized app owner))
  (render-state [_ {:keys [ch] :as state}]
    (condp contains? (:game-state app)
      #{:welcome} (om/build welcome app {:opts {:ch ch}})
      #{:waiting} (om/build waiting app) 
      #{:running} (om/build game-field app {:opts {:ch ch}})
      #{:ended} (om/build game-ended app {:opts {:ch ch}}))))

(om/root
  tetris
  app-state
  {:target (. js/document (getElementById "app"))})