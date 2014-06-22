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

(defn biggest-ai []
  {:name "Big Boy"
   :play-card (fn [app pid]
                (when-let [plays (model/valid-plays app pid)]
                  (last (sort-by :rank plays))))})

(defn littlest-ai []
  {:name "Little Boy"
   :play-card (fn [app pid]
                (when-let [plays (model/valid-plays app pid)]
                  (first (sort-by :rank plays))))})

(defn make-ai-player [index]
  (merge
   {:cards []
    :points 0
    :score 0
    :index index
    :strikes 0}
   (nth [(random-ai)
         (biggest-ai)
         (littlest-ai)]
        (mod index 3))))
