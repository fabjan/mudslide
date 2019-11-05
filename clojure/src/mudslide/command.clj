(ns mudslide.command
  (:require [mudslide.core :as core]
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

(defn store-string [prefix data]
  (binding [mudslide.core/*store-prefix* prefix]
    (log/info "saving test file with content" (str "'" data "'"))
    (core/save-text-file data)))

(defn dump-text-file [prefix hash]
  (binding [mudslide.core/*store-prefix* prefix]
    (println (core/get-text-file-content hash))))

(def command-options
  [["-d" "--prefix DIR" "Where to put the file store root directory"
    :default "/tmp/mudslide"
    :validate [#(not (str/includes? % "..")) "Must be an absolute path"]]
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
   "  stats       Prints information about the storage"
   "  store STR   Store the string STR"
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

      (and (= "store" (first arguments)) (= 2 (count arguments)))
      (store-string (:prefix options) (second arguments))

      (and (= "dump" (first arguments)) (= 2 (count arguments)))
      (dump-text-file (:prefix options) (second arguments))

      :else
      (exit 1 (usage summary)))))
