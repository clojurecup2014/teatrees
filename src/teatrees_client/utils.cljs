(ns teatrees-client.utils
  (:require [cljs.reader :as reader]
            [goog.events :as events]
            [goog.dom :as gdom]
            [clojure.string :as str]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:import [goog.net XhrIo]
           [goog.Uri QueryData]
           [goog.structs Map]
           goog.net.EventType
           [goog.events EventType]))

(def ^:private meths
  {:get "GET"
   :put "PUT"
   :post "POST"
   :delete "DELETE"})

(defn- params-for-url [params]
  (.createFromMap QueryData
    (Map. (clj->js params))))

(defn remove-empty-entries [m]
  (->> m
       (filter (fn [[_ v]] (and v (not (-> v str str/blank?)))))
       (into {})))

(defn edn-xhr [{:keys [method url data on-complete]}]
  (let [xhr (XhrIo.)
        clean-data (remove-empty-entries data)
        ; full-url (str "test/" url ".edn"
        full-url (str url
                   (when (and clean-data (= method :get))
                     (str "?" (params-for-url clean-data))))]
    (events/listen xhr goog.net.EventType.COMPLETE
      (fn [e]
        (on-complete (reader/read-string (.getResponseText xhr)))))
    (. xhr
      (send full-url (meths method) (when data (pr-str data))
        #js {"Content-Type" "application/edn"}))))