(ns ambition.ai
  (:require [ambition.model :as model]))

(defn cpu-name []
  (first (shuffle #{"Al Coholic"
                    "Oliver Klozoff"
                    "I. P. Freely"
                    "Jacques Strap"
                    "Seymour Butz"
                    "Homer Sexual"
                    "Mike Rotch"
                    ;; "I'm a stupid moron with an ugly face and a big butt and my butt smells and I like to kiss my own butt"
                    "Hugh Jass"
                    "Bea O'Problem"
                    "Amanda Hugginkiss"
                    "Ivana Tinkle"
                    "Anita Bath"
                    "Maya Buttreeks"
                    "Ura Snotball"
                    "Ollie Tabooger"})))

(defn random-ai []
  {:name "Rando"
   :ai-type :random
   :play-card (fn [app pid]
     (when-let [plays (model/valid-plays app pid)]
       (rand-nth plays)))})

(defn base-value [{:keys [rank suit]}]
  (cond
   (= rank 1) 20
   (= rank 2) 25
   :else rank))

(defn raw-trick-strength [trick-suit honor-played? {:keys [rank suit] :as card}]
  (cond
   (and trick-suit (not= suit trick-suit)) 0
   (and (not honor-played?) (= rank 2)) 1
   :else (base-value card)))

(defn trick-strength [app {:keys [rank suit] :as candidate-card}]
  (let [cards (:current-trick app)
        trick-suit (:suit (first cards))
        cards-in-suit (filter #(= (:suit %) trick-suit) cards)
        honor-played? (some model/is-honor?
                            (concat
                             (when (= trick-suit suit) [candidate-card]) ;; there WILL be
                             cards-in-suit))
        ts (partial raw-trick-strength trick-suit honor-played?)
        winning-card (last (sort-by ts cards-in-suit))]
    (cond
     (and trick-suit (not= suit trick-suit)) 0
     (or (not winning-card)
         (> (ts candidate-card) (ts winning-card))) (ts candidate-card)
     (and (= 2 rank) (not= 3 (count cards))) 1 ;; later player could still play an honor
     :else 0)))

(defn biggest-ai []
  {:name "Big Boy"
   :ai-type :big
   :play-card (fn [app pid]
                (when-let [plays (model/valid-plays app pid)]
                  (last (sort-by (juxt (partial trick-strength app)
                                       base-value)
                                 plays))))})

(defn littlest-ai []
  {:name "Little Boy"
   :ai-type :little
   :play-card (fn [app pid]
                (when-let [plays (model/valid-plays app pid)]
                  (first (sort-by (juxt (partial trick-strength app)
                                       base-value)
                                 plays))))})

(defn slamminess [app {:keys [suit rank] :as card}]
  [(trick-strength app card) ;; slammier to play something that wins the trick
   (- (base-value card)) ;; slammier to play the weakest card possible if we can't win the trick
   (case suit ;; tiebreaker when trying to decide what to throw away
     :spades 0 ;; slammier to hold spades than most other cards
     :hearts 1
     :diamonds (if (= rank 2) -5 2) ;; very slammy to hold the 2 of diamonds
     :clubs (if (= rank 13) -10 3) ;; excruciatingly slammy to hold the king of clubs
     )])

(defn plays-by-slamminess [app pid]
  (sort-by (partial slamminess app)
           (model/valid-plays app pid)))

(defn trick-winner-ai []
  {:name "Slams MacKenzie"
   :ai-type :slammer
   :play-card (fn [app pid] (last (plays-by-slamminess app pid)))})

(defn trick-loser-ai []
  {:name "Nil Boy"
   :ai-type :round-loser
   :play-card (fn [app pid] (first (plays-by-slamminess app pid)))})

(defn second-place-ai []
  {:name "Mr Second Place"
   :ai-type :second-place
   :play-card (fn [app pid]
                (when-let [plays (model/valid-plays app pid)]
                  (let [leader (last (sort-by :points (:players app)))
                        leader-points (:points leader)
                        my-points (:points (nth (:players app) pid))
                        trick-points (model/trick-value app)
                        ai (if (< (+ trick-points my-points) leader-points)
                             (trick-winner-ai)
                             (trick-loser-ai))]
                    ((:play-card ai) app pid))))})

(def ai-choices [trick-winner-ai second-place-ai trick-loser-ai])
(defn make-ai-player [index]
  (merge
   {:cards []
    :points 0
    :score 0
    :index index
    :strikes 0}
   ((nth ai-choices (mod index (count ai-choices))))))
