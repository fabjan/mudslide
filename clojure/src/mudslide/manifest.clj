(ns mudslide.manifest
  (:require [mudslide.util :as util]
            [clojure.string :as str]))

(defn parse-card [line digest]
  (let [[type rest] (str/split line #" " 2)]
    (cond
      (= type "Z") {::checksum rest}
      (= type "F") (let [[checksum name] (str/split rest #" " 2)]
                     (.update digest (.getBytes (str line "\n")))
                     {::filehash checksum ::filename name}))))

(defn parse
  "parses the given manifest into a map"
  [manifest]
  (let [lines (str/split-lines manifest)
        checksummer (util/get-digest)
        cards (map #(parse-card % checksummer) lines)
        [checksum-card] (filter #(contains? % ::checksum) cards)
        given-checksum (::checksum checksum-card)
        expected-checksum (util/hex-string (.digest checksummer))
        file-cards (filter #(contains? % ::filename) cards)
        manifest (conj {::files file-cards} checksum-card)]
    (assert (= expected-checksum given-checksum) "manifest checksum is invalid")
    manifest))
