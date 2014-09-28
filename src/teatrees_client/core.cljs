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
            [teatrees-client.utils :as utils :refer [edn-xhr]]
            [garden.color :as color :refer [hsl rgb as-hex]])
  (:import [goog History]
           [goog.history EventType]))

;; Urls:
;; POST /join - join the game
;; GET  /game/:uuid/wait - wait for the second player (poll every second)
;; GET  /game/:uuid/state - get current state (poll every second)    
;; POST /game/:uuid/move (:player-no, :move) - move figure
;; POST /game/:uuid/rotate (:player-no, :rotate) - rotate figure

(enable-console-print!)

(def app-state
  (atom
    {:game-state :welcome
     :game-field-size {:x 11 :y 11 :z 31}
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
  (let [{:keys [state game-field players border-pos]} data]
    (case state
      :ended
      (do 
        (.stop timer)
        (end-game app data))
      :started
      (do
        (om/update! app :game-field game-field)
        (om/update! app :border-pos border-pos)
        (om/transact! app :players
          #(map merge % players)))
      :unknown
      (do
        (om/update! app :game-state :unknown)))))

(defn uuid-for-url [uuid]
  (if (symbol? uuid) (name uuid) uuid))

(defn game-poll [app timer]
  (edn-xhr {:method :get
            :url (str "game/" (uuid-for-url (:uuid @app)) "/state")
            :on-complete #(update-received app % timer)}))

(defn start-game [app {:keys [players player-no] :as data}]
  (om/update! app :game-state :running)
  (om/update! app :players players)
  (let [timer (goog.Timer. 1000)]
    (.start timer)
    (events/listen timer goog.Timer/TICK #(game-poll app timer))))

(defn wait-poll [app timer]
  (edn-xhr {:method :get
            :url (str "game/" (uuid-for-url (:uuid @app)) "/await")
            :on-complete
              (fn [{:keys [state] :as data}]
                (when (= state :started)
                  (.stop timer)
                  (start-game app data)))}))

(defn wait-for-start [app]
  (om/update! app :game-state :waiting)
  (let [timer (goog.Timer. 1000)]
    (.start timer)
    (events/listen timer goog.Timer/TICK #(wait-poll app timer))))

(defn end-game [app data]
  (om/update! app :game-state :ended))

(defn start-or-wait [app data]
  (om/update! app :uuid (:uuid data))
  (om/update! app :player-no (:player-no data))
  (case (:state data) 
    :awaiting (wait-for-start app)
    :started (start-game app data)))

(defn try-join-game [app name]
  (om/update! app :player-name name)
  (edn-xhr {:method :post
            :url "join"
            :data {:name name}
            :on-complete #(start-or-wait app %)}))

(defn send-action [app transform]
  (when (and (= :running (:game-state @app))
             (:uuid @app))
    (let [[op transform-mod] (if (:rotate transform)
                               ["rotate" (:rotate transform)]
                               ["move" (set/rename-keys transform {:move :dir})])]
      (edn-xhr {:method :post
                :url (str "game/" (uuid-for-url (:uuid @app)) "/" op "/" (inc (:player-no @app)))
                :data transform-mod
                :on-complete identity}))))

;; =================================
;; Interaction

(def keycodes
  "https://code.google.com/p/closure-library/source/browse/closure/goog/events/keycodes.js?r=70795893b3841c22b412de14f9ecf8331f2e7073"
  {37 :left
   38 :up
   39 :right
   40 :down
   65 :A
   81 :Q
   83 :S
   87 :W
   88 :X
   90 :Z})

(defn event->msg [e]
  (if-let [k (keycodes (.-keyCode e))]
    (if-let [transform
             (case k
               :left {:move :left}
               :up {:move :up}
               :right {:move :right}
               :down {:move :down}
               :Q {:rotate {:axis :x :dir -1}}
               :W {:rotate {:axis :x :dir 1}}
               :A {:rotate {:axis :y :dir -1}}
               :S {:rotate {:axis :y :dir 1}}
               :Z {:rotate {:axis :z :dir -1}}
               :X {:rotate {:axis :z :dir 1}})]
      {:type :keydown
       :transform transform})))

(defn reg-events [ch]
  (events/listen (.-body js/document)
                 (.-KEYDOWN events/EventType)
                 #(when-let [m (event->msg %)] (put! ch m))))
 
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
    (let [{:keys [depth]} cell
          [x y] (calc-cell-pos conv cell)]
      (dom/rect {:class "cell"
                 :x x :y y :width cell-size :height cell-size
                 :style {:fill (as-hex (hsl (* depth 5) 50 50))}}))))

(defn bg-for-view [view width-px height-px conv border cell-size cell-gap]
  (condp contains? view
    #{:front :back :left :right}
      (let [border-px (conv border)
            p2-y (+ border-px cell-size cell-gap)]
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

(defcomponent game-field-view [app owner {:keys [view] :as opts}]
  (render [_]
    (let [{:keys [game-field game-field-size cell-size cell-gap]} app
          proj (project-game-field game-field game-field-size view)
          conv (coord-converter cell-size cell-gap)
          visible-dims (view-visible-dims view)
          view-size (map game-field-size visible-dims)
          [width-px height-px] (map #(- (conv %) cell-gap) view-size)]
      (dom/svg {:width width-px
                :height height-px}
        (bg-for-view view width-px height-px conv (:border-pos app) cell-size cell-gap)
        (for [cell proj]
          (om/build game-field-cell {:cell cell
                                     :conv conv
                                     :cell-size cell-size
                                     :cell-gap cell-gap}))
        (flatten 
          (map-indexed
            (fn [idx player]
              (let [fig (:fig player)
                    proj-fig (project-game-field fig game-field-size view)]
                (if (or (and (= view :top) (= idx 0))
                        (and (= view :bottom) (= idx 1))
                        (not (view #{:top :bottom})))
                  (for [cell proj-fig]
                    (om/build game-field-cell {:cell cell
                                               :conv conv
                                               :cell-size cell-size
                                               :cell-gap cell-gap})))))
            (:players app)))))))

(defcomponent player-info [{:keys [player player-no]} owner]
  (render [_]
    (let [{:keys [name score]} player]
      (dom/div {:class (str "player-info player-info-" (inc player-no))}
        name
        (dom/br nil)
        (str "Score: " score)))))

(def control-help
  (dom/dl
    (flatten 
      (for [[k1 k2 desc] [["Left" "Right" "Move on X axis"]
                          ["Up" "Down" "Move on Y axis"]
                          ["Q" "W" "Rotate around X axis"]
                          ["A" "S" "Rotate around Y axis"]
                          ["Z" "X" "Rotate around Z axis"]]]
        [(dom/dt
           (dom/kbd k1)
           "/"
           (dom/kbd k2))
         (dom/dd desc)]))))

(defcomponent game-field [app owner {:keys [ch] :as opts}]
  (render [_]
    (let [views [:top :left :front :bottom]
          {:keys [game-field game-field-size player-no]} app]
      (dom/div {:class "container-fluid"
                :id "tetris"
                :ref "tetris"}
        (dom/div {:class "row header-row"}
          (for [view views]
            (dom/div {:class "col-xs-3"}
              (dom/h3 (-> view name str/capitalize)))))
        (dom/div {:class "row game-field"}
          (for [view views]
            (dom/div {:class (str "col-xs-3 game-view" (when (= view :bottom) " game-view-bottom"))}
              (when (= view :bottom)
                [(om/build player-info {:player (-> app :players second) :player-no 1})
                 (when (= player-no 1) control-help)])
              (om/build game-field-view app {:opts {:view view}})
              (when (= view :top)
                [(om/build player-info {:player (-> app :players first) :player-no 0})
                 (when (= player-no 0) control-help)]))))))))

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
                  :y (- (.-innerHeight js/window) 70)}
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
      (try-join-game app (:name msg))
    :keydown
      (send-action app (:transform msg))))

(defcomponent footer [app owner]
  (render [_]
    (dom/div {:class "footer"}
      (dom/a {:href "https://github.com/clojurecup2014/teatrees"} "Tea Trees")
      " by Bumblebears (2014)")))

(defn start-game-submited [app owner e ch]
  (.preventDefault e)
  (let [username (str/trim (.-value (om/get-node owner "username")))]
    (println "username" username "count" (count username))
    (if (< (count username) 2)
      (om/set-state! owner :bad-name true)
      (do
        (om/set-state! owner :bad-name false)
        (put! ch {:type :try-join-game :name username})))))

(defcomponent welcome [app owner {:keys [ch] :as opts}]
  (init-state [_]
    {:bad-name false})
  (render-state [_ {:keys [bad-name]}]
    (dom/div {:class "container small-container"}
      (dom/div {:class "jumbotron"}
        (dom/h1 "Tea Trees")
        (dom/p "3d tetris")
        (dom/form {:on-submit #(start-game-submited app owner % ch)}
          (dom/div {:class "form-group"}
            (dom/label {:for "username"} "Enter your name")
            (dom/input {:class "form-control"
                        :id "username"
                        :ref "username"}))
          (when bad-name
            (dom/div {:class "alert alert-warning"
                      :role "alert"}
              (dom/strong "Bad name!")
              " No offence. Let's call you by name not shorter than 2 characters, ok?"))
          (dom/button {:class "btn btn-primary"} "Start!")))
      (om/build footer app))))

(defcomponent waiting [app owner]
  (render [_]
    (dom/div {:class "container small-container"}
      (dom/h1 "Tea Trees")
      (dom/p (:player-name app)
             ", we're waiting for another player to join...
             Don't run away as it won't take long."))))

(defcomponent game-ended [app owner]
  (render [_]
    (dom/div {:class "container small-container"}
      (dom/h1 "Tea Trees")
      (dom/h2 "Game over")
      (let [players (:players app)
            draw? (= (-> players first :score) (-> players second :score))
            winner (apply max-key :score (:players app))]
        (dom/p
          (str "Score: " (str/join " and " (map #(str (:name %) " - " (:score %)) players)))
          (dom/br nil)
          (if draw?
            (str "No way! It's draw! "
                 (str/join " and " (map :name players))
                 ", you were equally cool this time!")
            (str (:name winner) " won, let's congratulate him (or her)!"))))
      (dom/button {:class "btn btn-primary"
                   :role "button"
                   :on-click #(try-join-game app (:player-name @app))} "Play again"))))

(defcomponent tetris [app owner]
  (init-state [_]
    {:ch (chan)})
  (will-mount [_]
    (let [ch (om/get-state owner :ch)]
      (reg-events ch)
      (go (loop []
        (let [msg (<! ch)]
          (handle-event app msg)
          (recur))))))
  (did-mount [_]
    (monitor-window-resize app owner)
    (window-resized app owner))
  (render-state [_ {:keys [ch] :as state}]
    ; (println app)
    (case (:game-state app)
      :welcome (om/build welcome app {:opts {:ch ch}})
      :waiting (om/build waiting app) 
      :running (om/build game-field app {:opts {:ch ch}})
      :ended   (om/build game-ended app {:opts {:ch ch}}))))

(om/root
  tetris
  app-state
  {:target (. js/document (getElementById "app"))})