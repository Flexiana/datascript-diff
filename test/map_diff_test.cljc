(ns map-diff-test
  (:require
    [clojure.test :refer :all]
    [seq-diff :refer [seq-diff
                      common-ordered-part
                      seq-revert
                      seq-commit]]
    [map-diff :refer [map-commit
                      map-diff
                      narrowing
                      expansion]]))

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

(expansion {:a {:a 2 :b 2}} {:a {:a 1}})

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
                (map-commit a))) (format "%s -> %s" a b))
  (is (= a (->> (map-diff b a)
                (map-commit b))) (format "%s -> %s" b a)))

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
                  :b "apple"}))

(defn seq-diff-commit-test
  [a b]
  (is (= b (->> (seq-diff a b)
                (seq-commit a))) (format "%s -> %s" a b))
  (is (= a (->> (seq-diff b a)
                (seq-commit b))) (format "%s -> %s" b a)))

(deftest seq-test
  (is (= [:b] (common-ordered-part [:a :b :c] [:c :b :d])))
  (is (= [:b] (common-ordered-part [:a :b :c] [:b :d])))
  (is (= [:b :c] (common-ordered-part [:a :b :c] [:b :c :b :d])))
  (is (= [] (common-ordered-part [5 6 4 2 1] [7 8 9])))
  (is (= [1 4] (common-ordered-part [1 2 3 4] [1 4 5 6 7])))
  (is (= [1 4] (common-ordered-part [1 4 5 6 7] [1 2 3 4])))
  (is (= [1] (common-ordered-part [1 4 5 6 7] [7 5 1])))
  (seq-diff-commit-test [:c :b :d] [:a :b :c])
  (seq-diff-commit-test [1 2 3 4 5 6] [4 5 6])
  (seq-diff-commit-test [1 2 3 4 5 6] [4 5 6])
  (seq-diff-commit-test [:a :b :c] [:c :b :d])
  (seq-diff-commit-test [1 2 [3 1]] [1 2 [3 2]]))

(deftest map-in-seq
  (seq-diff-commit-test [{:a {:a 2}}] [{:a {:a 3}}])
  (seq-diff-commit-test [{:a {:a 2}}] [{:a {:a 3}}])
  (seq-diff-commit-test [1 2 {:a {:a 2}} 1 2] [{:a {:a 3}} 1 2])
  (seq-diff-commit-test [1 2 {:a {:a 3}} 2 1 2] [1 2 {:a {:a 2}} 1 2])
  (seq-diff-commit-test [{:a {:a {:a 1 :b 2}} :b 2 :d {:e 5}}] [{:a {:a {:a 2} :b 2} :b 3}])
  (seq-diff-commit-test [1 1 2 3 5 7 10 18 {:a "b"} 19 13 15]
                        [1 "a" "b" 1 3 1 1 2 18 {:a "c"} 19 4 5 6 7 8 9 10 11 12])
  (is (= (first (:to-print (seq-diff [{:a 2} 2] [{:a 3} 2])))
         (:to-print (map-diff {:a 2} {:a 3}))))
  (is (= (first (:+ (seq-diff [{:a 2} 2] [{:a 3} 2])))
         (:+ (map-diff {:a 2} {:a 3}))))
  (is (= (first (:- (seq-diff [{:a 2} 2] [{:a 3} 2])))
         (:- (map-diff {:a 2} {:a 3}))))
  (is (= [{:a 3} 2] (seq-commit [{:a 2} 2] (seq-diff [{:a 2} 2] [{:a 3} 2]))))
  (is (= (first (:- (seq-diff [{:a 2} 2] [{:a 3} 2])))
         (:- (map-diff {:a 2} {:a 3}))))
  (is (= (first (:+ (seq-diff [{:a 2} 2] [{:a 3} 2])))
         (:+ (map-diff {:a 2} {:a 3})))))

(defn seq-diff-revert-test
  [a b]
  (is (= a (->> (seq-diff a b)
                (seq-revert b))) (format "%s -> %s" a b))
  (is (= b (->> (seq-diff b a)
                (seq-revert a))) (format "%s -> %s" b a)))

(seq-diff-revert-test [] [])
(seq-diff-revert-test [1] [])
(seq-diff-revert-test [] [1])
(seq-diff-revert-test [1 [1]] [])
(seq-diff-revert-test [] [1 [2]])
(seq-diff-revert-test [1 [2]] [2 [1]])

(seq-diff-revert-test [1 {:a 1}] [2 {}])

