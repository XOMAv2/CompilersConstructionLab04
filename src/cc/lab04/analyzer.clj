(ns cc.lab04.analyzer
  (:require [cc.lab04.utils :refer [find-prod-by-rhs]]
            [com.walmartlabs.cond-let :refer [cond-let]])
  (:gen-class))

(defn g-reduce
  "Свёртка."
  [stack {:keys [precedence-table subroutines border-marker] :as grammar} & {:keys [debug?]}]
  (loop [stack stack
         chain '()]
    (cond-let
     :let [_ (when debug? (println "stack -->" stack))
           _ (when debug? (println "chain -->" chain))]

     ; Стек содерджит единственный символ - маркер границы.
     (= (vec stack) [border-marker])
     (do (when debug? (println :1))
         (into stack chain))

     :let [top (peek stack)
           bottom (peek (pop stack))
           relation (get-in precedence-table [bottom top])
           chain (conj chain top)
           _ (when debug? (println "new chain -->" chain))]

     (contains? #{">" "="} relation)
     (do (when debug? (println :2))
         (recur (pop stack) chain))

     (nil? relation)
     (do (println :3)
         (throw (Exception. "KEKEXEC")))

     ; Отношение "<".
     :let [prod (find-prod-by-rhs chain grammar)
           _ (when debug? (println "prod -->" prod))]

     (nil? prod)
     (do (when debug? (println :4))
         stack)

     :else
     (let [lhs (-> prod keys first)
           rhs (-> prod vals first)]
       (eval (get subroutines rhs))
       (do (when debug? (println :5))
           (pop stack))))))

(defn syntax-analyzer
  [tokens {:keys [precedence-table nonterms border-marker] :as grammar} & {:keys [debug?]}]
  (loop [stack (list border-marker)
         tree :success
         tokens tokens]
    (cond-let
     :let [token (first tokens)
           sp (peek stack)]

     (= sp token border-marker)
     tree
     
     ; Пропуск нетерминала.
     (some #{token} nonterms)
     (do (if debug? (println "Пропуск нетерминала." {:stack stack :tokens tokens}))
         (recur stack tree (rest tokens)))

     :let [relation (get-in precedence-table [sp token])]

     (nil? relation)
     (throw (Exception. (format "Конструкция ... %s %s ... запрещена." sp token)))
     
     ; Перенос.
     (contains? #{"<" "="} relation)
     (do (if debug? (println "Перенос." {:stack stack :tokens tokens}))
         (recur (conj stack token) tree (rest tokens)))
     
     ; Свёртка.
     :else
     (do (if debug? (println "Свёртка." {:stack stack :tokens tokens}))
         (recur (g-reduce stack grammar :debug? debug?) tree tokens)))))
