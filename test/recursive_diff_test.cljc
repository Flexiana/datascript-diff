(ns recursive-diff-test
  (:require
    [clojure.test :refer :all]
    [recursive-diff :refer [commit
                            make
                            narrowing
                            prepare-print
                            expansion]]))

(deftest step-one
  (is (= {} (expansion {} {})))
  (is (= {} (expansion {:a 1} {:a 1})))
  (is (= {:+ {[:a] 2}} (expansion {} {:a 2})))
  (is (= {:+ {[:b] 2 [:c] 3}} (expansion {:a 1} {:b 2 :c 3})))
  (is (= {:- {[:a] 1} :+ {[:a] 2 [:b] 2 [:c] 3}}
         (expansion {:a 1} {:a 2 :b 2 :c 3})))
  (is (= {:+ {[:a] {:a 1}}} (expansion {} {:a {:a 1}})))
  (is (= {:+ {[:a] {:a 1}}} (expansion {:b 2} {:a {:a 1}})))
  (is (= {:- {[:a] 2}
          :+ {[:a] {:a 1}
              [:b] 3}}
         (expansion {:a 2} {:a {:a 1}
                            :b 3})))
  (is (= {:+ {[:b] 3}}
         (expansion {:a {:a 1}} {:a {:a 1}
                                 :b 3})))
  (is (= {:+ {[:a :a] 2
              [:b]    3}
          :- {[:a :a] 1}}
         (expansion {:a {:a 1}} {:a {:a 2}
                                 :b 3})))
  (is (= {:- {[:b] [1 2 3 4 5 6]}
          :+ {[:b] [4 5 6]}}
         (expansion {:b [1 2 3 4 5 6]} {:b [4 5 6]}))))

(deftest step-two
  (is (= {:- {[:a] "a"}} (narrowing {} {:a "a"} {})))
  (is (= {} (narrowing {} {:a "c"} {:a "a"})))
  (is (= {:- {[:b] 2}} (narrowing {} {:a "c" :b 2} {:a "a"})))
  (is (= {:- {[:a] {:a 1}}} (narrowing {} {:a {:a 1}} {})))
  (is (= {} (narrowing {} {:a {:a {:b 5}}} {:a 2}))))

(deftest full-diff
  (is (= {} (make {} {})))
  (is (= {} (make {:a 1} {:a 1})))
  (is (= {:+ {[:a] 2}} (make {} {:a 2})))
  (is (= {:+ {[:b] 2, [:c] 3}, :- {[:a] 1}} (make {:a 1} {:b 2 :c 3})))
  (is (= {:- {[:a] 1} :+ {[:a] 2 [:b] 2 [:c] 3}}
         (make {:a 1} {:a 2 :b 2 :c 3})))
  (is (= {:+ {[:a] {:a 1}}} (make {} {:a {:a 1}})))
  (is (= {:+ {[:a] {:a 1}}, :- {[:b] 2}} (make {:b 2} {:a {:a 1}})))
  (is (= {:- {[:a] 2}
          :+ {[:a] {:a 1}
              [:b] 3}}
         (make {:a 2} {:a {:a 1}
                       :b 3})))
  (is (= {:+ {[:b] 3}
          :- {[:c] 3}}
         (make {:a {:a 1} :c 3} {:a {:a 1}
                                 :b 3})))
  (is (= {:+ {[:a :a] 2
              [:b]    3}
          :- {[:a :a] 1
              [:d]    {:e 5}}}
         (make {:a {:a 1
                    :b 2}
                :d {:e 5}}
               {:a {:a 2
                    :b 2}
                :b 3})))
  (is (= {:- {[:b] [1 2 3 4 5 6]
              [:a] "apple"}
          :+ {[:b] [4 5 6]}}
         (make {:a "apple"
                :b [1 2 3 4 5 6]} {:b [4 5 6]}))))

(defn test-commit
  [a b]
  (is (= b (->> (make a b)
                (commit a))))
  (is (= a (->> (make b a)
                (commit b)))))

(deftest apply-diff
  (test-commit {} {})
  (test-commit {:a 1} {:a 1})
  (test-commit {} {:a 2})
  (test-commit {:a 1} {:b 2 :c 3})
  (test-commit {:a 1} {:a 2 :b 2 :c 3})
  (test-commit {} {:a {:a 1}})
  (test-commit {:b 2} {:a {:a 1}})
  (test-commit {:a 2} {:a {:a 1} :b 3})
  (test-commit {:a {:a 1} :c 3} {:a {:a 1} :b 3})
  (test-commit {:a {:a 1} :b 2 :d {:e 5}} {:a {:a 2 :b 2} :b 3})
  (test-commit {:a {:a {:a 1 :b 2}} :b 2 :d {:e 5}} {:a {:a {:a 2} :b 2} :b 3})
  (test-commit {:a "apple" :b [1 2 3 4 5 6]} {:b [4 5 6]}))

(deftest print-prepare
  (is (= {:a {:a {:- 1, :+ 2}, :b 2}, :d {:- {:e 5}}, :b {:+ 3}}
         (prepare-print {:a {:a 1
                             :b 2}
                         :d {:e 5}}
                        {:+ {[:a :a] 2
                             [:b]    3}
                         :- {[:a :a] 1
                             [:d]    {:e 5}}}))))


(defn subcoll? [x y]
  (let [a (set x)
        b (set y)]
    (every? a b)))

(defn get-indexes [v e]
  (keep-indexed (fn [idx v] (when (= e v) idx)) v))

(defn shorter [x y]
  (if (> (count x) (count y)) y x))


(defn same-ordered? [x y]
  (let [y-in-x (reduce concat (for [e y] (get-indexes x e)))
        x-in-y (reduce concat (for [e x] (get-indexes y e)))
        idx-y-in-x (reduce (fn [acc e]
                             (cond
                               (< e (first acc)) (concat [e] acc)
                               (> e (last acc)) (concat acc [e])
                               :else acc))
                           [(first y-in-x)] (rest y-in-x))
        idx-x-in-y (reduce (fn [acc e]
                             (cond
                               (< e (first acc)) (concat [e] acc)
                               (> e (last acc)) (concat acc [e])
                               :else acc))
                           [(first x-in-y)] (rest x-in-y))]
    (println y-in-x idx-y-in-x)
    (println x-in-y idx-x-in-y)
    (shorter (map x idx-y-in-x) (map y idx-x-in-y))))

(subcoll? '(:a :b :3) [:b])

(get-indexes [:a :a :b :a] :c)

(same-ordered? [:a :c :3 :c :b] [0 :a  :c :b :3 :c])