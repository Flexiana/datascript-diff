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
                     \":db/id\": 4}")))
  (is (= [[213] [187] [233] [179] [79] [76] [224] [221] [236] [204] [212]]
         (rr/->clj "[[213], [187], [233], [179], [79], [76], [224], [221], [236], [204], [212]]"))))

(deftest complex-case-A
  (let [data (rr/->clj "{\":db/id\":262,\":block/children\":[{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"**This one is a title**\",\":create/time\":1625859838582,\":create/user\":{\":db/id\":245},\":block/order\":0,\":block/open\":true,\":edit/time\":1625859854754,\":block/uid\":\"6hhjCNnbV\",\":db/id\":263,\":block/page\":{\":db/id\":262},\":block/children\":[{\":db/id\":264},{\":db/id\":265}],\":edit/user\":{\":db/id\":245}},{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"Connect with [[Flexiana Clojure Academy]]\",\":block/refs\":[{\":db/id\":79}],\":create/time\":1626110920175,\":create/user\":{\":db/id\":245},\":block/order\":1,\":block/open\":true,\":edit/time\":1626110934621,\":block/uid\":\"nokyRsv6z\",\":db/id\":273,\":block/page\":{\":db/id\":262},\":edit/user\":{\":db/id\":245}},{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"{{[[TODO]]}} Make another complex test with nested arrays\",\":block/refs\":[{\":db/id\":276}],\":create/time\":1626110937038,\":create/user\":{\":db/id\":245},\":block/order\":2,\":block/open\":true,\":edit/time\":1626112337695,\":block/uid\":\"ROPniJBtp\",\":db/id\":274,\":block/page\":{\":db/id\":262},\":edit/user\":{\":db/id\":245}},{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"{{[[TODO]]}} Create a query with datalog to get childrens recursively\",\":block/refs\":[{\":db/id\":276}],\":create/time\":1626111227098,\":create/user\":{\":db/id\":245},\":block/order\":3,\":block/open\":true,\":edit/time\":1626111776789,\":block/uid\":\"g89JcbFKO\",\":db/id\":278,\":block/page\":{\":db/id\":262},\":edit/user\":{\":db/id\":245}},{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"{{[[TODO]]}} Create another test case where it uses maps within vectors\",\":block/refs\":[{\":db/id\":276}],\":create/time\":1626111298863,\":create/user\":{\":db/id\":245},\":block/order\":4,\":block/open\":true,\":edit/time\":1626111653970,\":block/uid\":\"oYg1daORN\",\":db/id\":281,\":block/page\":{\":db/id\":262},\":edit/user\":{\":db/id\":245}},{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"{{query: {and: [[TODO]]}}}\",\":block/refs\":[{\":db/id\":276}],\":create/time\":1626112147053,\":create/user\":{\":db/id\":245},\":block/order\":5,\":block/open\":true,\":edit/time\":1626112203499,\":block/uid\":\"zC-39mftY\",\":db/id\":282,\":block/page\":{\":db/id\":262},\":edit/user\":{\":db/id\":245}}],\":block/uid\":\"w96hZCtfv\",\":create/time\":1625859836790,\":create/user\":{\":db/id\":245},\":edit/time\":1625859836790,\":edit/user\":{\":db/id\":245},\":node/title\":\"Complex test case | A\"}")]
    (is (= (get-in data [":block/children" 0 ":block/children"])
           [{":db/id" 264} {":db/id" 265}]))
    (is (= (get-in data [":block/children" 0 ":block/string"]) "**This one is a title**"))
    (is (= (get-in data [":block/children" 0 ":create/user" ":db/id"]) 245))))
