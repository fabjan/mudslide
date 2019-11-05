(ns mudslide.command
  (:require [mudslide.core :as core]
            [mudslide.http :as http]
            [mudslide.util :as util]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.tools.logging :as log])
  (:gen-class))

(defn print-stats [prefix]
  (binding [mudslide.core/*store-prefix* prefix]
    (let [manifest-names (core/list-manifests)
          file-hashes (core/list-files)]
      (println "prefix:" prefix)
      (println "stored manifests:")
      (println (apply util/long-str manifest-names))
      (println "stored files:")
      (println (apply util/long-str file-hashes))
      )))

(defn store-string [prefix checksum data]
  (binding [mudslide.core/*store-prefix* prefix]
    (log/info "saving test file with content" (str "'" data "'"))
    (core/save-text-file checksum data)))

(defn dump-text-file [prefix hash]
  (binding [mudslide.core/*store-prefix* prefix]
    (println (core/get-text-file-content hash))))

(defn start-web-api [prefix port-number]
  (binding [mudslide.core/*store-prefix* prefix]
    (http/start port-number)))

(def command-options
  [["-d" "--prefix DIR" "Where to put the file store root directory"
    :default "/tmp/mudslide"
    :validate [#(not (str/includes? % "..")) "Must be an absolute path"]]
   ["-p" "--port PORT" "Port number"
    :default 8989
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-h" "--help"]])

(defn usage [options-summary]
  (util/long-str
   "Stores and retrieves files."
   ""
   "Usage: mudslide [options] [action ...]"
   ""
   "Options:"
   options-summary
   ""
   "Actions:"
   "  start       Starts the web api"
   "  stats       Prints information about the storage"
   "  store H STR Store the string STR with checksum H"
   "  dump HASH   Print the contents of file HASH"
   ""
   "Please refer to README.md for more information"))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args command-options)]
    (cond

      (:help options)
      (exit 0 (usage summary))

      errors ; if errors is not falsy, there are errors
      (exit 1 (apply util/long-str errors))

      (= ["stats"] arguments)
      (print-stats (:prefix options))

      (= ["start"] arguments)
      (start-web-api (:prefix options) (:port options))
      
      (and (= "store" (first arguments)) (= 2 (count arguments)))
      (store-string (:prefix options) (second arguments))

      (and (= "dump" (first arguments)) (= 2 (count arguments)))
      (dump-text-file (:prefix options) (second arguments))

      :else
      (exit 1 (usage summary)))))
