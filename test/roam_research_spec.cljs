(ns roam-research-spec
  (:require
   [goog.string :as s]
   [goog.string.format]
   [cljs.test :refer [deftest is]]
   [map-diff :refer [map-commit
                     map-revert
                     map-diff]]))

(defn map-full-test
  [a b]
  (is (= b (->> (map-diff a b)
                (map-commit a))) (s/format "commit %s -> %s" a b))
  (is (= a (->> (map-diff b a)
                (map-commit b))) (s/format "commit %s -> %s" b a))
  (is (= a (->> (map-diff a b)
                (map-revert b))) (s/format "revert %s -> %s" b a))
  (is (= b (->> (map-diff b a)
                (map-revert a))) (s/format "revert %s -> %s" a b)))

(defn roam-research->clj [s]
  (js->clj (.parse js/JSON s) :keywordize-keys false))

(deftest testing-roam-research-data
  (let [data-a (roam-research->clj "{
  \":block/parents\": [{ \":db/id\": 3 }],
  \":block/string\": \"7GUIs\",
  \":create/time\": 1609151779781,
  \":create/user\": { \":db/id\": 1 },
  \":block/order\": 0,
  \":block/open\": true,
  \":edit/time\": 1609151785742,
  \":block/uid\": \"OtQdkIAKn\",
  \":db/id\": 4,
  \":block/page\": { \":db/id\": 3 },
  \":edit/user\": { \":db/id\": 1 }}")
        data-b (roam-research->clj "{
  \":block/parents\": [{ \":db/id\": 3 }],
  \":block/string\": \"[[Flexiana Framework]]\",
  \":block/refs\": [{ \":db/id\": 6 }],
  \":create/time\": 1609151785735,
  \":create/user\": { \":db/id\": 1 },
  \":block/order\": 1,
  \":block/open\": true,
  \":edit/time\": 1609188039048,
  \":block/uid\": \"Rm0BLZjrR\",
  \":edit/seen-by\": [{ \":db/id\": 41 }],
  \":db/id\": 5,
  \":block/page\": { \":db/id\": 3 },
  \":edit/user\": { \":db/id\": 1 }}")]
    (map-full-test data-a data-b)))