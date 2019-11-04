(ns mudslide.integration-test
  (:require [clojure.test :refer [deftest testing is]]
            [mudslide.manifest :as manifest]
            [mudslide.util :as util]))

(deftest test-manifests
  (testing "can be parsed"
    (let [manifest-file
          (util/long-str "F e242ed3bffccdf271b7fbaf34ed72d089537b42f bar.txt"
                         "F f1d2d2f924e986ac86fdf7b36c94bcdf32beec15 foo.txt"
                         "Z 1fb2be2881c60c77abc59aa4b7ae5fabb5dbe96d")
          manifest (manifest/parse manifest-file)]
      (is (= "1fb2be2881c60c77abc59aa4b7ae5fabb5dbe96d" (::manifest/checksum manifest))))))

(deftest test-utils
  (testing "can smash together strings"
    (is (= "foo\nbar" (util/long-str "foo" "bar"))))
  (testing "can present pretty sha1 checksums"
    (is (= "c412b37f8c0484e6db8bce177ae88c5443b26e92" (util/string-checksum "hej")))))


(comment
  
  ; some files and a manifest to test with
  (do
    (def foo-txt "foo\n")
    (def bar-txt "bar\n")
    (def manifest (mudslide.util/long-str
                   "F e242ed3bffccdf271b7fbaf34ed72d089537b42f bar.txt"
                   "F f1d2d2f924e986ac86fdf7b36c94bcdf32beec15 foo.txt"
                   "Z 1fb2be2881c60c77abc59aa4b7ae5fabb5dbe96d")))

  (println (mudslide.util/long-str "foo.txt:" foo-txt
                              "bar.txt:" bar-txt
                              "manifest:" manifest))

  ; WARNING these commands will write to your filesystem (but only in /tmp/mudslide.testing)
  (binding [mudslide.core/*store-prefix* "/tmp/mudslide.testing"]
    (mudslide.core/save-text-file foo-txt)
    (try
      (mudslide.core/save-manifest manifest)
      (catch Throwable t (println "cannot create manifest yet" (.getMessage t))))
    (mudslide.core/save-text-file bar-txt)
    (mudslide.core/save-manifest manifest)
    (mudslide.core/list-manifests)
    (mudslide.core/get-manifest "1fb2be2881c60c77abc59aa4b7ae5fabb5dbe96d")
    (mudslide.core/get-text-file-content "f1d2d2f924e986ac86fdf7b36c94bcdf32beec15"))
  ;
  )
