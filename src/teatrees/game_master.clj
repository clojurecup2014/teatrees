(ns teatrees.game-master
  (:require [clojure.tools.logging :as log]
            [clojure.core.async :as async]))

(defn- success 
  [response] { :status :ok :body response })

(defn- failure
  ([response] (failure response :bad-request))
  ([response status] {:status status :body {:error response}}))

(defn available
  []
  (success "ok"))

(defn high-scores
  []
  (success "empty"))

(defn field-state
  [uuid]
  (success "ok"))

(defn join
  [uuid]
  (success "ok"))

(defn disconnect
  [uuid playerid]
  (success "ok"))