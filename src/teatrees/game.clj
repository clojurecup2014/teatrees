(ns teatrees.game
  (:require [clojure.tools.logging :as log]
            [clojure.core.async :as async]))

(def x-max 10)
(def y-max 10)
(def z-max 40)


(defrecord Game [id field pl1id pl2id])

(def ^:private game (atom { :id}))