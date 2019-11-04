(ns mudslide.util)

(defn get-digest []
  (java.security.MessageDigest/getInstance "sha1"))

(defn hex-string [bytes]
  (apply str (map #(format "%02x" (bit-and % 0xff)) bytes)))

(defn checksum [bytes]
  (-> (get-digest)
      (.digest bytes)
      (hex-string)))

(defn string-checksum [s]
  (checksum (.getBytes s)))

(defn long-str [& strings]
  (clojure.string/join "\n" strings))
