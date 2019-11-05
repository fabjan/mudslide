(ns mudslide.core
  (:require [mudslide.util :as util]
            [mudslide.manifest :as manifest]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]))

(def ^:dynamic *store-prefix* "/tmp/mudslide") ; dynamic to let the server decide

(defn- files-root []
  (io/file *store-prefix* "mudslide.clojure.store" "files"))
(defn- manifests-root []
  (io/file *store-prefix* "mudslide.clojure.store" "manifests"))
(defn- get-store-file [name]
  (io/file (files-root) name))
(defn- get-store-manifest [name]
  (io/file (manifests-root) name))

(defn name-file [data]
  (util/checksum data))
(defn name-manifest [manifest]
  (::manifest/checksum manifest))

(defn- write-file-if-not-exists
  "write data to file, iff file does not already exist"
  [file data]
  (let [name (.getAbsolutePath file)]
    (if (.exists file)
      (log/debug "ignore duplicate file write" name)
      (do
        (log/info "storing new file" name)
        (io/make-parents file)
        (with-open [o (io/output-stream file)]
          (.write o data))))))

(defn save-file
  "store the given byte array in a file named by its hash"
  [data]
  (assert (bytes? data) "I can only store byte arrays")
  (let [name (name-file data)
        file (get-store-file name)]
    (write-file-if-not-exists file data)))

(defn save-text-file
  "convenience wrapper for testing, see save-file"
  [s]
  (save-file (.getBytes s)))

(defn save-manifest
  "check that all files in the manifest exist, and store the manifest"
  [manifest-content]
  (let [manifest (manifest/parse manifest-content)
        manifest-name (name-manifest manifest)
        files (::manifest/files manifest)]
    (assert manifest-name "manifest has no checksum")
    (assert files "manifest has no files")
    (doseq [manifest-file files]
      (let [stored-filename (::manifest/filehash manifest-file)]
        (assert (.exists (get-store-file stored-filename))
                (str "manifest file missing: " (::manifest/filename manifest-file)))))
    (log/info "saving manifest" manifest-name
              "with files" (map :mudslide.manifest/filename files))
    (write-file-if-not-exists (get-store-manifest manifest-name)
                              (.getBytes manifest-content))))

(defn list-manifests
  "list all stored manifests"
  []
  (->> (manifests-root)
       (file-seq)
       (drop 1)
       (map #(.getName %))))

(defn list-files
  "list all stored files"
  []
  (->> (files-root)
       (file-seq)
       (drop 1)
       (map #(.getName %))))

(defn get-manifest
  "get the content of the manifest with the given name"
  [name]
  (-> (get-store-manifest name)
      (slurp)))

(defn get-file-stream
  "get a stream for the content of the file with the given name"
  [name]
  (-> (get-store-file name)
      (io/input-stream)))

(defn get-text-file-content
  "get the string content of the file with the given name"
  [name]
  (with-open [baos (java.io.ByteArrayOutputStream.)]
    (io/copy (get-file-stream name) baos)
    (.toString baos)))
