(ns blackjack.test1)

; Declaração de Simbolos...
(def nome "Fabricio")
(def idade  20)

; Construção de função
(defn saudadacao
  "Saudação iniciante em Clojure"
  [nome]
  (str "Bem vindo " nome))

(println (saudadacao nome))