(ns teatrees.core
  (:require [clojure.tools.logging :as log]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]
            [ring.middleware.format :as format]
            [ring.util.response :refer [resource-response]]
            [teatrees.middleware.status-wrapper :refer :all]
            [teatrees.game :as gm]))

(defroutes game
  (GET "/available-games" [] (gm/available))
  (GET "/high-scores" [] (gm/high-scores))
  (GET "/state/:uuid" [uuid] (gm/field-state uuid))
  (GET "/status/:uuid" [uuid] ()) ; For polling
  (POST "/move" [uuid dir player-nm] (gm/move uuid dir player-nm))
  (POST "/join" [name] (gm/try-join name))

  (GET "/" [] (resource-response "index.html" {:root "public"}))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (-> (handler/site game)
      wrap-status-code
      (format/wrap-restful-format :formats [:edn])))