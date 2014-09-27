(ns teatrees.middleware.status-wrapper)

(def http-status-keywords
  {
    ;; Success
    :ok 200
    :created 201
    :accepted 202
    :non-authoritative-information 203
    :no-content 204
    :reset-content 205
    :partial-content 206
    ;; Redirect
    :multiple-choises 300
    :moved-permanently 301
    :found 302
    :see-other 303
    :not-modified 304
    :use-proxy 305
    :temporary-redirect 307
    ;; Client failure
    :bad-request 400
    :unauthorized 401
    :payment-required 402
    :forbidden 403
    :not-found 404
    :method-not-allowed 405
    :not-acceptable 406
    :proxy-authentication-required 407
    :request-timeout 408
    :conflict 409
    :gone 410
    :length-required 411
    :precondition-failed 412
    :request-entity-too-large 413
    :request-uri-too-long 414
    :unsupported-media-type 415
    :requested-range-not-satisfiable 416
    :expectation-failed 417
    ;; Server failure
    :internal-server-error 500
    :not-implemented 501
    :bad-gateway 502
    :service-unabailable 503
    :gateway-timeout 504
    :http-version-not-supported 505
  })

(defn wrap-status-code
  [handler]
  (fn
    [request]
    (let [response (handler request)
          status (:status response)]
      (if (keyword? status)
        (merge response { :status (status http-status-keywords)})
        response))))