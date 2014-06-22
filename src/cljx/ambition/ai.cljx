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
   (not= suit trick-suit) 0
   (and (not honor-played?) (= rank 2)) 1
   :else (base-value card)))

(defn trick-strength [app {:keys [rank suit] :as candidate-card}]
  (let [cards (:current-trick app)
        trick-suit (:suit (first cards))
        cards-in-suit (filter #(= (:suit %) trick-suit) cards)
        honor-played? (some model/is-honor? cards-in-suit)
        ts (partial raw-trick-strength trick-suit honor-played?)
        winning-card (last (sort-by ts cards-in-suit))]
    (cond
     (not= suit trick-suit) 0
     (> (ts candidate-card) (ts winning-card)) (ts candidate-card)
     (and (= 2 rank) (not= 3 (count cards))) 1 ;; later player could still play an honor
     :else 0)))

(defn biggest-ai []
  {:name "Big Boy"
   :play-card (fn [app pid]
                (when-let [plays (model/valid-plays app pid)]
                  (last (sort-by (juxt (partial trick-strength app)
                                       base-value)
                                 plays))))})

(defn littlest-ai []
  {:name "Little Boy"
   :play-card (fn [app pid]
                (when-let [plays (model/valid-plays app pid)]
                  (first (sort-by (juxt (partial trick-strength app)
                                       base-value)
                                 plays))))})

(defn slammer-ai []
  {:name "Slams MacKenzie"
   :play-card (fn [app pid]
                (when-let [plays (model/valid-plays app pid)]
                  (last (sort-by (juxt (partial trick-strength app)
                                       (comp - base-value))
                                 plays))))})

(def ai-choices [slammer-ai littlest-ai littlest-ai])
(defn make-ai-player [index]
  (merge
   {:cards []
    :points 0
    :score 0
    :index index
    :strikes 0}
   ((nth ai-choices (mod index (count ai-choices))))))
