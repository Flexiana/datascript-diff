(ns map-diff-test
  (:require
    #?(:cljs [cljs.test :refer [deftest is]]
       :clj [clojure.test :refer [deftest is format]])
    #?(:cljs [goog.string :as s])
    #?(:cljs [goog.string.format])
    [map-diff :refer [map-commit
                      map-revert
                      map-diff
                      narrowing
                      expansion]]
    #?(:cljs [roam-research :as rr])
    [seq-diff :refer [seq-diff
                      common-ordered-part
                      seq-revert
                      seq-commit]]))


(deftest step-one
  (is (= {} (expansion {} {})))
  (is (= {} (expansion {:a 1} {:a 1})))
  (is (= {[:a] {:+ 2}} (expansion {} {:a 2})))
  (is (= {[:b] {:+ 2}, [:c] {:+ 3}} (expansion {:a 1} {:b 2 :c 3})))
  (is (= {[:a] {:- 1, :+ 2}, [:b] {:+ 2}, [:c] {:+ 3}}
         (expansion {:a 1} {:a 2 :b 2 :c 3})))
  (is (= {[:a] {:+ {:a 1}}} (expansion {} {:a {:a 1}})))
  (is (= {[:a] {:+ {:a 1}}} (expansion {:b 2} {:a {:a 1}})))
  (is (= {[:a] {:- 2, :+ {:a 1}}, [:b] {:+ 3}}
         (expansion {:a 2} {:a {:a 1}
                            :b 3})))
  (is (= {[:b] {:+ 3}}
         (expansion {:a {:a 1}} {:a {:a 1}
                                 :b 3})))
  (is (= {'(:a :a) {:- 1, :+ 2}, [:b] {:+ 3}}
         (expansion {:a {:a 1}} {:a {:a 2}
                                 :b 3})))
  (is (= {[:b] [{:- 1} {:- 2} {:- 3} nil nil nil]}
         (expansion {:b [1 2 3 4 5 6]} {:b [4 5 6]}))))


(defn step-2
  [a b]
  (-> (expansion a b)
      (narrowing a b)))


(deftest step-two
  (is (= {[:a] {:- "a"}} (step-2 {:a "a"} {})))
  (is (= {[:a] {:- "c", :+ "a"}} (step-2 {:a "c"} {:a "a"})))
  (is (= {[:b] {:- 2}} (narrowing {} {:a "c" :b 2} {:a "a"})))
  (is (= {[:a] {:- {:a 1}}} (narrowing {} {:a {:a 1}} {})))
  (is (= {'(:a :a) {:- 2, :+ 1}, '(:a :b) {:- 2}} (narrowing {'(:a :a) {:- 2, :+ 1}} {:a {:a 2 :b 2}} {:a {:a 1}})))
  (is (= {[:a] {:- {:a {:b 5}}, :+ 2}} (step-2 {:a {:a {:b 5}}} {:a 2}))))


(deftest full-diff
  (is (= {} (map-diff {} {})))
  (is (= {} (map-diff {:a 1} {:a 1})))
  (is (= {[:a] {:+ 2}} (map-diff {} {:a 2})))
  (is (= {[:b] {:+ 2}, [:c] {:+ 3}, [:a] {:- 1}} (map-diff {:a 1} {:b 2 :c 3})))
  (is (= {[:a] {:- 1, :+ 2}, [:b] {:+ 2}, [:c] {:+ 3}}
         (map-diff {:a 1} {:a 2 :b 2 :c 3})))
  (is (= {[:a] {:+ {:a 1}}} (map-diff {} {:a {:a 1}})))
  (is (= {[:a] {:+ {:a 1}}, [:b] {:- 2}} (map-diff {:b 2} {:a {:a 1}})))
  (is (= {[:a] {:- 2, :+ {:a 1}}, [:b] {:+ 3}}
         (map-diff {:a 2} {:a {:a 1}
                           :b 3})))
  (is (= {'(:a :a) {:- 1, :+ 2}, [:b] {:+ 3}, [:c] {:- 3}}
         (map-diff {:a {:a 1} :c 3} {:a {:a 2} :b 3})))
  (is (= {'(:a :a) {:- 1, :+ 2}, [:b] {:+ 3}, [:d] {:- {:e 5}}}
         (map-diff {:a {:a 1 :b 2} :d {:e 5}}
                   {:a {:a 2 :b 2} :b 3})))
  (is (= {[:b] [{:- 1} {:- 2} {:- 3} nil nil nil], [:a] {:- "apple", :+ [123]}}
         (map-diff {:a "apple"
                    :b [1 2 3 4 5 6]}
                   {:b [4 5 6]
                    :a [123]}))))


(defn map-full-test
  [a b]
  (is (= b (->> (map-diff a b)
                (map-commit a))) #?(:cljs (s/format "commit %s -> %s" a b)
                                    :clj (format "commit %s -> %s" a b)))
  (is (= a (->> (map-diff b a)
                (map-commit b))) #?(:cljs (s/format "commit %s -> %s" b a)
                                    :clj (format "commit %s -> %s" b a)))
  (is (= a (->> (map-diff a b)
                (map-revert b))) #?(:cljs (s/format "revert %s -> %s" b a)
                                    :clj (format "revert %s -> %s" b a)))
  (is (= b (->> (map-diff b a)
                (map-revert a))) #?(:cljs (s/format "revert %s -> %s" a b)
                                    :clj (format "revert %s -> %s" a b))))


(deftest map-diff-commit-test
  (map-full-test {} {})
  (map-full-test {:a 1} {:a 1})
  (map-full-test {} {:a 2})
  (map-full-test {:a 1} {:b 2 :c 3})
  (map-full-test {:a 1} {:a 2 :b 2 :c 3})
  (map-full-test {} {:a {:a 1}})
  (map-full-test {:b 2} {:a {:a 1}})
  (map-full-test {:a 2} {:a {:a 1} :b 3})
  (map-full-test {:a {:a 1} :c 3} {:a {:a 1} :b 3})
  (map-full-test {:a {:a 1} :b 2 :d {:e 5}} {:a {:a 2 :b 2} :b 3})
  (map-full-test {:a {:a {:a 1 :b 2}} :b 2 :d {:e 5}} {:a {:a {:a 2} :b 2} :b 3})
  (map-full-test {:a "apple" :b [1 2 3 4 5 6]} {:b [4 5 6]})
  (map-full-test {:x [{:a 3} 2]} {:x [{:a 3} 2]})
  (map-full-test {:a [:c :b :d]} {:a [:a :b :c]})
  (map-full-test {:z [1 1 2 3 5 7 10 18 {:a "b"} 19 13 15]
                  :x {:_ 2 :a "b" :b [1 2 3]}
                  :a "b"
                  :b "c"}
                 {:z [1 "a" "b" 1 3 1 1 2 18 {:a "c"} 19 4 5 6 7 8 9 10 11 12]
                  :x {:a "d" :b [2 1 3]}
                  :a "c"
                  :b "apple"})
  (map-full-test {":a" 1} {":a" 3}))


(defn seq-diff-commit-revert-test
  [a b]
  (is (= b (->> (seq-diff a b)
                (seq-commit a))) #?(:clj (format "commit %s -> %s" a b)
                                    :cljs (s/format "commit %s -> %s" a b)))
  (is (= a (->> (seq-diff b a)
                (seq-commit b))) #?(:clj (format "commit %s -> %s" b a)
                                    :cljs (s/format "commit %s -> %s" b a)))
  (is (= a (->> (seq-diff a b)
                (seq-revert b))) #?(:clj (format "revert %s -> %s" b a)
                                    :cljs (s/format "revert %s -> %s" b a)))
  (is (= b (->> (seq-diff b a)
                (seq-revert a))) #?(:clj (format "revert %s -> %s" a b)
                                    :cljs (s/format "revert %s -> %s" a b))))


(deftest seq-test
  (is (= [:b] (common-ordered-part [:a :b :c] [:c :b :d])))
  (is (= [:b] (common-ordered-part [:a :b :c] [:b :d])))
  (is (= [:b :c] (common-ordered-part [:a :b :c] [:b :c :b :d])))
  (is (= [] (common-ordered-part [5 6 4 2 1] [7 8 9])))
  (is (= [1 4] (common-ordered-part [1 2 3 4] [1 4 5 6 7])))
  (is (= [1 4] (common-ordered-part [1 4 5 6 7] [1 2 3 4])))
  (is (= [1] (common-ordered-part [1 4 5 6 7] [7 5 1])))
  (seq-diff-commit-revert-test [:c :b :d] [:a :b :c])
  (seq-diff-commit-revert-test [1 2 3 4 5 6] [4 5 6])
  (seq-diff-commit-revert-test [1 2 3 4 5 6] [4 5 6])
  (seq-diff-commit-revert-test [:a :b :c] [:c :b :d])
  (seq-diff-commit-revert-test [1 2 [3 1]] [1 2 [3 2]]))


(deftest map-in-seq
  (seq-diff-commit-revert-test [{:a {:a 2}}] [{:a {:a 3}}])
  (seq-diff-commit-revert-test [{:a {:a 2}}] [{:a {:a 3}}])
  (seq-diff-commit-revert-test [1 2 {:a {:a 2}} 1 2] [{:a {:a 3}} 1 2])
  (seq-diff-commit-revert-test [1 2 {:a {:a 3}} 2 1 2] [1 2 {:a {:a 2}} 1 2])
  (seq-diff-commit-revert-test [{:a {:a {:a 1 :b 2}} :b 2 :d {:e 5}}] [{:a {:a {:a 2} :b 2} :b 3}])
  (seq-diff-commit-revert-test [1 1 2 3 5 7 10 18 {:a "b"} 19 13 15]
                               [1 "a" "b" 1 3 1 1 2 18 {:a "c"} 19 4 5 6 7 8 9 10 11 12])
  (is (= (first (seq-diff [{:a 2} 2] [{:a 3} 2]))
         (map-diff {:a 2} {:a 3})))
  (is (= (first (seq-diff [{:a 2} 2] [{:a 3} 2]))
         (map-diff {:a 2} {:a 3})))
  (is (= (first (seq-diff [{:a 2} 2] [{:a 3} 2]))
         (map-diff {:a 2} {:a 3})))
  (is (= [{:a 3} 2] (seq-commit [{:a 2} 2] (seq-diff [{:a 2} 2] [{:a 3} 2]))))
  (is (= (first (seq-diff [{:a 2} 2] [{:a 3} 2]))
         (map-diff {:a 2} {:a 3})))
  (is (= (first (seq-diff [{:a 2} 2] [{:a 3} 2]))
         (map-diff {:a 2} {:a 3}))))


(deftest partial-roam-research-data
  (let [data-a (rr/->clj "{\":block/parents\": [{ \":db/id\": 3 }]}")
        data-b (rr/->clj "{\":block/uid\": \"OtQdkIAKn\",
                           \":block/page\": { \":db/id\": 3 },
                           \":edit/user\": { \":db/id\": 1 },
                           \":db/id\": 4}")]
    (is (= {[":block/parents"] {:+ [{":db/id" 3}]}}
           (expansion {} data-a)))
    (is (= {[":block/uid"] {:+ "OtQdkIAKn"}
            [":block/page"] {:+ {":db/id" 3}}
            [":edit/user"] {:+ {":db/id" 1}}
            [":db/id"] {:+ 4}}
           (expansion {} data-b)))
    (map-full-test data-a data-b)))


(deftest full-roam-research-data
  (let [data-a (rr/->clj "{\":block/parents\": [{ \":db/id\": 3 }],
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
        data-b (rr/->clj "{\":block/parents\": [{ \":db/id\": 3 }],
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
                           \":edit/user\": { \":db/id\": 1 }}")
        ids-pages-in-account-a (rr/->clj "[[213], [187], [233], [179], [79], [76], [224], [221], [236],
                                           [204], [212], [215], [246], [182], [234], [259], [216], [229],
                                           [206], [218], [230], [226], [235], [173], [242], [251], [249],
                                           [2], [208], [228], [238], [209], [207], [239]]")
        ids-pages-in-account-b (rr/->clj "[[253], [244], [203], [217], [188], [185], [231], [247],
                                           [227], [3], [6], [225], [237], [191], [205], [78], [256],
                                           [164], [211], [232], [181], [180], [240], [222], [184],
                                           [210], [223], [220], [241]]")]
    (seq-diff-commit-revert-test ids-pages-in-account-a ids-pages-in-account-b)
    (map-full-test data-a data-b)))


(deftest test-nested-case-with-complex-case-A
  (let [data (rr/->clj "{\":db/id\":262,\":block/children\":[{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"**This one is a title**\",\":create/time\":1625859838582,\":create/user\":{\":db/id\":245},\":block/order\":0,\":block/open\":true,\":edit/time\":1625859854754,\":block/uid\":\"6hhjCNnbV\",\":db/id\":263,\":block/page\":{\":db/id\":262},\":block/children\":[{\":db/id\":264},{\":db/id\":265}],\":edit/user\":{\":db/id\":245}},{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"Connect with [[Flexiana Clojure Academy]]\",\":block/refs\":[{\":db/id\":79}],\":create/time\":1626110920175,\":create/user\":{\":db/id\":245},\":block/order\":1,\":block/open\":true,\":edit/time\":1626110934621,\":block/uid\":\"nokyRsv6z\",\":db/id\":273,\":block/page\":{\":db/id\":262},\":edit/user\":{\":db/id\":245}},{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"{{[[TODO]]}} Make another complex test with nested arrays\",\":block/refs\":[{\":db/id\":276}],\":create/time\":1626110937038,\":create/user\":{\":db/id\":245},\":block/order\":2,\":block/open\":true,\":edit/time\":1626112337695,\":block/uid\":\"ROPniJBtp\",\":db/id\":274,\":block/page\":{\":db/id\":262},\":edit/user\":{\":db/id\":245}},{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"{{[[TODO]]}} Create a query with datalog to get childrens recursively\",\":block/refs\":[{\":db/id\":276}],\":create/time\":1626111227098,\":create/user\":{\":db/id\":245},\":block/order\":3,\":block/open\":true,\":edit/time\":1626111776789,\":block/uid\":\"g89JcbFKO\",\":db/id\":278,\":block/page\":{\":db/id\":262},\":edit/user\":{\":db/id\":245}},{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"{{[[TODO]]}} Create another test case where it uses maps within vectors\",\":block/refs\":[{\":db/id\":276}],\":create/time\":1626111298863,\":create/user\":{\":db/id\":245},\":block/order\":4,\":block/open\":true,\":edit/time\":1626111653970,\":block/uid\":\"oYg1daORN\",\":db/id\":281,\":block/page\":{\":db/id\":262},\":edit/user\":{\":db/id\":245}},{\":block/parents\":[{\":db/id\":262}],\":block/string\":\"{{query: {and: [[TODO]]}}}\",\":block/refs\":[{\":db/id\":276}],\":create/time\":1626112147053,\":create/user\":{\":db/id\":245},\":block/order\":5,\":block/open\":true,\":edit/time\":1626112203499,\":block/uid\":\"zC-39mftY\",\":db/id\":282,\":block/page\":{\":db/id\":262},\":edit/user\":{\":db/id\":245}}],\":block/uid\":\"w96hZCtfv\",\":create/time\":1625859836790,\":create/user\":{\":db/id\":245},\":edit/time\":1625859836790,\":edit/user\":{\":db/id\":245},\":node/title\":\"Complex test case | A\"}")]
    (map-full-test (update-in data [":block/children" 0 ":block/parents" 0 ":db/id"] inc) data)
    (is (= {[":block/children"] [{[":block/parents"] [nil], [":block/children"] [nil {[":db/id"] {:- 266, :+ 265}}]} nil nil nil nil nil]}
           (map-diff (update-in data [":block/children" 0 ":block/children" 1 ":db/id"] inc) data)))
    (is (= {[":block/children"] [{[":block/parents"] [nil], [":block/children"] [{[":db/id"] {:- 263, :+ 264}} nil]} nil nil nil nil nil]}
           (map-diff (update-in data [":block/children" 0 ":block/children" 0 ":db/id"] dec) data)))))


(defn update-id
  [m]
  (assoc-in m [0 "id"] (str "F-" (get-in m [0 "id"]))))


(defn delete-children
  [m]
  (assoc-in m [0 "children"] []))


(deftest real-data-in-vector
  (let [data-a (rr/->clj "[[{\"id\":276,\"uid\":\"LhZeYTZQ0\",\"time\":1626110980956,\"user\":{\"id\":245},\"title\":\"TODO\"}],[{\"user\":{\"id\":1},\"seen-by\":[{\"id\":245}],\"time\":1609313306401,\"title\":\"Flexiana Clojure Academy\",\"children\":[{\"id\":80},{\"id\":81},{\"id\":84},{\"id\":90},{\"id\":99},{\"id\":108}],\"uid\":\"T8N9qXG_o\",\"id\":79,\"sidebar\":1}],[{\"id\":251,\"children\":[{\"id\":252}],\"uid\":\"Tb8ASGrTz\",\"time\":1625229073776,\"user\":{\"id\":1},\"title\":\"RoamResearch\"}],[{\"id\":1609151756506,\"uid\":\"12-28-2020\",\"time\":1609151756509,\"user\":{\"id\":1},\"title\":\"December 28th, 2020\"}],[{\"id\":262,\"children\":[{\"id\":263},{\"id\":273},{\"id\":274},{\"id\":278},{\"id\":281},{\"id\":282}],\"uid\":\"w96hZCtfv\",\"time\":1625859836790,\"user\":{\"id\":245},\"title\":\"Complex test case | A\"}],[{\"user\":{\"id\":1},\"seen-by\":[{\"id\":245}],\"time\":1625229086268,\"title\":\"Note-taking app\",\"children\":[{\"id\":254},{\"id\":255}],\"uid\":\"nKmtwRNgd\",\"id\":253}],[{\"id\":1626709152312,\"uid\":\"07-19-2021\",\"time\":1626709152313,\"user\":{\"id\":245},\"title\":\"July 19th, 2021\"}],[{\"user\":{\"id\":41},\"time\":1614877122576,\"title\":\"March 4th, 2021\",\"children\":[{\"id\":190}],\"id\":188,\"uid\":\"03-04-2021\"}],[{\"id\":3,\"children\":[{\"id\":4},{\"id\":5}],\"uid\":\"AYmFRYV8q\",\"time\":1609151774585,\"user\":{\"id\":1},\"title\":\"Open Source Backlog\"}],[{\"user\":{\"id\":1},\"seen-by\":[{\"id\":77}],\"time\":1609188039048,\"title\":\"Flexiana Framework\",\"children\":[{\"id\":7},{\"id\":34},{\"id\":39}],\"uid\":\"zaqiZUUyR\",\"id\":6,\"sidebar\":0}],[{\"user\":{\"id\":1},\"time\":1614937968365,\"title\":\"March 5th, 2021\",\"children\":[{\"id\":194}],\"id\":191,\"uid\":\"03-05-2021\"}],[{\"id\":256,\"uid\":\"FXHmCHZoW\",\"time\":1625229099421,\"user\":{\"id\":1},\"title\":\"Evernote\"}],[{\"id\":1626280159646,\"uid\":\"07-14-2021\",\"time\":1626280159649,\"user\":{\"id\":245},\"title\":\"July 14th, 2021\"}],[{\"id\":1626389356021,\"uid\":\"07-15-2021\",\"time\":1626389356023,\"user\":{\"id\":245},\"title\":\"July 15th, 2021\"}]]")
        data-b (map update-id data-a)
        data-c (map delete-children data-a)]
    (seq-diff-commit-revert-test data-a data-b)
    (seq-diff-commit-revert-test data-b data-c)
    (seq-diff-commit-revert-test data-c data-a)))
