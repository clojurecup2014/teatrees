(ns teatrees.middleware.key-caser
  (:require [org.tobereplaced.lettercase :refer [mixed-name]]
            [clojure.set :refer [rename-keys]]))

(defn change-keys-case
  [m]
  (let [kseq-old (keys m)
        kseq-new (map mixed-name kseq-old)
        kmap (zipmap kseq-old kseq-new)]
    (rename-keys m kmap)))

(defn wrap-case-change
  [handler]
  (fn
    [request]
    (let [response (handler request)
          body (:body response)]
      (if (map? body)
        (assoc response :body (change-keys-case body))
        response))))