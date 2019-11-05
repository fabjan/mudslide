(ns mudslide.core
  (:require [mudslide.util :as util]
            [mudslide.manifest :as manifest]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log])
  (:import java.io.InputStream))

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

(defn- write-blob-if-not-exists
  "write data to file, iff file does not already exist"
  [file data]
  (let [name (.getAbsolutePath file)]
    (if (.exists file)
      (log/debug "ignore duplicate blob write" name)
      (do
        (log/info "storing new blob" name)
        (io/make-parents file)
        (with-open [o (io/output-stream file)]
          (.write o data))))))

(defn to-bytes [data]
  (cond
    (bytes? data) data
    (instance? InputStream data) (let [os (java.io.ByteArrayOutputStream.)]
                                   (io/copy data os)
                                   (.toByteArray os))))

(defn save-file
  "store the given byte array in a file named by its hash"
  [checksum data]
  (let [bytes (to-bytes data)
        name (name-file bytes)
        real-file (get-store-file name)]
    (assert (= name checksum) "incorrect file checksum")
    (write-blob-if-not-exists real-file bytes)
    checksum))

(defn save-text-file
  "convenience wrapper for testing, see save-file"
  [checksum s]
  (save-file checksum (.getBytes s)))

(defn to-string [data]
  (cond
    (string? data) data
    (instance? InputStream data) (slurp data)))

(defn save-manifest
  "check that all files in the manifest exist, and store the manifest"
  [checksum manifest-content]
  (let [manifest-text (to-string manifest-content)
        manifest (manifest/parse manifest-text)
        manifest-name (name-manifest manifest)
        files (::manifest/files manifest)]
    (assert manifest-name "manifest has no checksum")
    (assert (= manifest-name checksum) "incorrect manifest checksum")
    (assert files "manifest has no files")
    (doseq [manifest-file files]
      (let [stored-filename (::manifest/filehash manifest-file)]
        (assert (.exists (get-store-file stored-filename))
                (str "manifest file missing: " (::manifest/filename manifest-file)))))
    (log/info "saving manifest" manifest-name
              "with files" (map :mudslide.manifest/filename files))
    (write-blob-if-not-exists (get-store-manifest manifest-name)
                              (.getBytes manifest-text))
    checksum))

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
