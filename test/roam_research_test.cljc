(ns roam-research-test
  (:require
   #?(:cljs [roam-research :as rr])
   #?(:cljs [cljs.test :refer [deftest is]]
      :clj [clojure.test :refer [deftest is]])))

(deftest translate-data
  (is (= {":block/parents" [{":db/id" 3}]}
         (rr/->clj "{\":block/parents\": [{ \":db/id\": 3 }]}")))
  (is (= {":block/uid" "OtQdkIAKn"
          ":block/page" {":db/id" 3}
          ":edit/user" {":db/id" 1}
          ":db/id" 4}
         (rr/->clj "{\":block/uid\": \"OtQdkIAKn\",
                     \":block/page\": { \":db/id\": 3 },
                     \":edit/user\": { \":db/id\": 1 },
                     \":db/id\": 4}"))))