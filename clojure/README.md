## Installation

You need [Clojure] to build and run this app. A version recent enough to
handle deps.edn for building, 1.10+ I think.

Run `clj -Auberjar` to create a standalone jar, to be run with `java -jar`.

Run `clj -Anative-image` to create a native binary (takes a while to build).

If you don't have a favorite Clojure editor, you can use [Nightlight] to edit
the code. Run `clj -Aide` to let the web ide download and serve itself on
localhost.

[Kaocha] can run the tests, it has a mode that watches test and source files
for changes and re-runs tests: `bin/kaocha --watch`.

[Clojure]: https://clojure.org/guides/getting_started
[Nightlight]: https://sekao.net/nightlight/
[Kaocha]: https://github.com/lambdaisland/kaocha
