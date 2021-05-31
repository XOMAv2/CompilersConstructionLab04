(ns cc.lab04.utils
  (:require [clojure.set]))

(defmacro when-let*
  ([bindings & body]
   (if (seq bindings)
     `(when-let [~(first bindings) ~(second bindings)]
        (when-let* ~(drop 2 bindings) ~@body))
     `(do ~@body))))

(defn get-all-rhs
  "Получение множества всех правых частей правил грамматики."
  [{:keys [prods]}]
  (apply clojure.set/union (vals prods)))

(defn rhs-unique?
  "Проверка правых частей правил грамматики на уникальность."
  [{:keys [prods]}]
  (let [rhs-list (apply concat (vals prods))
        rhs-set (set rhs-list)]
    (= (count rhs-set) (count rhs-list))))

(defn get-all-prods
  "Получение множества всех пар правил вида [lhs rhs]."
  [{:keys [prods]}]
  (-> (for [lhs (keys prods)
            rhs (get prods lhs)]
        [lhs rhs])
      set))

(defn find-left-side
  "Поиск левой части правила по цепочке терминалов (!!!) из правой части."
  [rhs {:keys [terms] :as grammar}]
  (let [rhs (filter terms rhs)
        prods (->> (get-all-prods grammar)
                   (map (fn [[l r]]
                          [l (vec (filter terms r))])))]
    (reduce (fn [_ [l r]]
              (when (= r rhs)
                (reduced l)))
            nil prods)))

(defn find-prod-by-rhs
  "Поиск правила по цепочке терминалов (!!!) из правой части."
  [rhs {:keys [terms] :as grammar}]
  (let [rhs-terms (filter terms rhs)
        prods (get-all-prods grammar)]
    (reduce (fn [_ [l r]]
              (when-let* [r-terms (filter terms r)
                          _ (seq r-terms)
                          _ (= r-terms rhs-terms)]
                         (reduced {l r})))
            nil
            prods)))

#_(find-prod-by-rhs ["kek"] (read-string (slurp "resources/grammar.edn")))
