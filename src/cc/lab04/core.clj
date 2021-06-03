(ns cc.lab04.core
  (:require [clojure.string]
            [cc.lab04.analyzer :refer [syntax-analyzer]])
  (:gen-class))

(defn -main
  [grammar-path text & {:keys [debug?] :or {debug? false}}]
  (let [grammar (-> grammar-path slurp read-string)
        tokens (-> text
                   (clojure.string/trim)
                   (clojure.string/split #"\s+")
                   (conj "$"))]
    (syntax-analyzer tokens grammar :debug? debug?)))

(-main "resources/grammar.edn"
       "( not идент ) + ( - конст ) >= ( идент *       ( идент - конст ) mod конст  )   "
       :debug? false)

#_(-main "resources/grammar.edn"
       "идент * идент")

#_(-main "resources/grammar.edn"
       "идент or идент * идент"
       :debug? false)
