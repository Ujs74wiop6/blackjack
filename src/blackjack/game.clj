(ns blackjack.game
  (:require [card-ascii-art.core :as card]))

; Card scheme:
; A, 2, 3, 4, 5, 6, 7, 8, 9, 10, J,  Q,  K
; 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13;

(defn new-card []
  "Generates a card number between 1 and 13"
  (inc (rand-int 13)))

; Represent a player
; Ex:
;   {:player "Fabricio"
;    :cards [3, 4]
;    :points ...}


; Função para calcular o ponto com relação as cartas
; J, Q, K = 10 (não 11, 12 e 13)
; A = (Se não passar de 12) 11, (se passar de 21) = 1

(defn QJK->10 [cards]
  (if (> cards 10) 10 cards))

(defn A->11 [card]
  (if (= card 1) 11 card))

(defn points-cards [cards]
  (let [points-without-JQK (map QJK->10 cards)
        points-with-A11 (map A->11 points-without-JQK)
        points-with-A-1 (reduce + points-without-JQK)
        points-with-A11 (reduce + points-with-A11)]
    (if (> points-with-A11 22) points-with-A-1 points-with-A11)))


; Collection of names for the Dealer
(def nomes ["Dealer-Gustavo" "Dealer-Bob" "Dealer-Charlie" "Dealer-Vitor" "Dealer-Carlos"
            "Dealer-Sophia" "Dealer-Emily" "Dealer-Liam" "Dealer-Lucas" "Dealer-Isabella"
            "Dealer-Mia" "Dealer-Emma" "Dealer-Oliver" "Dealer-João" "Dealer-Daniel"
            "Dealer-Amelia"])

; Function that will generate a random name for Dealer-player
(defn rand-name [nomes]
  "Generate a random name from the given list"
  (rand-nth nomes))

; Function that will create a Dealer-player map
(defn PC-player []
  "Function for definition player and your cards"
  (def pc-name (rand-name nomes))
  (let [card1 (new-card)
        card2 (new-card)
        cards [card1 card2]
        points (points-cards cards)]
    {:player-name pc-name
     :cards       cards
     :points      points}))

; Function that will create the player who will play, with his name
(defn player [nome-player]
  "Function for definition player and your cards"
  (let [card1 (new-card)
        card2 (new-card)
        cards [card1 card2]
        points (points-cards cards)]
    {:player-name nome-player
     :cards       cards
     :points      points}))

(defn limpa-tela []
  "Clear screen/REPL function"
  (println "\u001b[H\u001b[2J"))

;Função para gerar uma nova carta e incrementar
; com as cartas do player, assim no final também
; atualizando os pontos do jogados
(defn more-card [player]
  (let [card (new-card)
        cards (conj (:cards player) card)
        new-player (update player :cards conj card)
        points (points-cards cards)]
    (assoc new-player :points points)))

(defn player-decision-continue? [player]
  (println (:player-name player) ": Mais cartas ?")
  (= (read-line) "sim"))

(defn dealer-decision-continue? [player-points dealer]
  (let [dealer-points (:points dealer)]
    (if (> player-points 21) false (<= dealer-points player-points))))


(defn game [player fn-decision-continue?]
  (if (fn-decision-continue? player)
    (let [player-with-more-cards (more-card player)]
      (card/print-player player-with-more-cards)
      (recur player-with-more-cards fn-decision-continue?))
    player))

; se ambos passaram de 21 -> ambos perderam
; se pontos iguais -> empatou
; se player passou de 21 -> dealer ganhou
; se dealer passou de 21 -> player ganhou
; se player maior que dealer -> player ganhou
; se dealer maior que player -> dealer ganhou

(defn end-game [player dealer]
  (let [player-points (:points player)
        dealer-points (:points dealer)
        player-name (:player-name player)
        dealer-name (:player-name dealer)
        message (cond
                  (and (> player-points 21) (> dealer-points 21)) " Ambos perderam"
                  (= player-points dealer-points) " Empate"
                  (> player-points 21) (str dealer-name " Ganhou")
                  (> dealer-points 21) (str player-name " Ganhou")
                  (> player-points dealer-points) (str player-name "Ganhou")
                  (> dealer-points player-points) (str dealer-name " Ganhou"))]
    (card/print-player player)
    (card/print-player dealer)
    (print message)))


 (defn start-game [nome-player]
  "Function to start the game and show players and their points in the game"
  (limpa-tela)
   (def player-1 (player nome-player))
   (card/print-player player-1)
   (def dealer (PC-player))
   (card/print-masked-player dealer)
   (def player-after-game (game player-1 player-decision-continue?))
   (def partial-dealer-decision-continue (partial dealer-decision-continue? (:points player-after-game)))
   (def dealer-after-game (game dealer partial-dealer-decision-continue))
   (end-game player-after-game dealer-after-game))

(start-game "Guto")
