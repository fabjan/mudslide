(ns mudslide.http
  (:require [mudslide.core :as core]
            [ring.adapter.jetty :as jetty]
            [compojure.core :refer [defroutes GET PUT]]
            [compojure.route :refer [not-found]]
            [clojure.tools.logging :as log]))

(defn welcome
  [request]
  {:status 200
   :body "<h1>Hemsida</h1>"
   :headers {}})

(defn wrap-exceptions
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch AssertionError e
        {:status 400 :body "Invalid data\n"})
      (catch Exception e
        (log/warn "Unhandled exception" e)
        {:status 500 :body "Internal server error\n"}))))

(defroutes routes
  (GET "/" [] welcome)

  (PUT "/api/files/:checksum" [checksum :as req] (core/save-file checksum (:body req)))
  (GET "/api/files/:checksum" [checksum] (core/get-file-stream checksum))
  (GET "/api/files" [] (core/list-files))

  (PUT "/api/manifests/:checksum" [checksum :as req] (core/save-manifest checksum (:body req)))
  (GET "/api/manifests/:checksum" [checksum] (core/get-manifest checksum))
  (GET "/api/manifests" [] (core/list-manifests))

  ;(not-found "404 Not found")
  )

(defn start [port-number]
  (jetty/run-jetty (-> routes wrap-exceptions)
                   {:port (Integer. port-number)}))