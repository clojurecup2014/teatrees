(ns teatrees.core
  (:require [clojure.tools.logging :as log]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]
            [ring.middleware.format :as format]
            [teatrees.middleware.key-caser :refer :all]
            [teatrees.middleware.status-wrapper :refer :all]
            [teatrees.game-master :as gm]
            [teatrees.game :as game]))

(defroutes game
  (GET "/available-games" [] (gm/available))
  (GET "/high-scores" [] (gm/high-scores))
  (GET "/field-state/:uuid" [uuid] (gm/field-state uuid))
  (POST "/join" [uuid name] (gm/join uuid name))

  (GET "/" [] (ring.util.response/redirect "/index.html"))
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
  (-> (handler/site game)
      wrap-status-code
      wrap-case-change
      (format/wrap-restful-format :formats [:json-kw :edn])))