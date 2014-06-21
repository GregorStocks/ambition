(ns ambition.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:text "HELLO"
                      :current-player-index 0
                      :user-player-index 0
                      :players [{:name "Gregor"
                                 :cards [{:suit :hearts :rank 1}]}
                                {:name "CPU 1"
                                 :cards [{:suit :hearts :rank 1}]}
                                {:name "CPU 2"
                                 :cards [{:suit :hearts :rank 1}]}
                                {:name "CPU 3"
                                 :cards [{:suit :hearts :rank 1}]}]}))

(defn render-card [card]
  (let [munged-rank (mod (- 14 (:rank card)) 13)]
    (dom/img #js {:src (str "card-images/"
                            (+ (* 4 munged-rank)
                               (case (:suit card)
                                 :clubs 1
                                 :spades 2
                                 :hearts 3
                                 :diamonds 4))
                            ".png")})))

(defn render-cardback [card-count]
  (dom/div #js {:className "cardback"}
           (str card-count "cards")))

(defn log [arg]
  (.log js/console arg))

(defn render-player [app pid classname]
  (let [player (-> app :players (get pid))]
    (apply dom/li
           #js {:className (str classname " player")}
           (conj
            (if (= pid (:user-player-index app))
              (map render-card (:cards player))
              [(render-cardback (count (:cards player)))])
            (dom/h1 #js {:className "playerName"}
                    (:name player))))))

(def suits [:hearts :diamonds :spades :clubs])
(def ranks [1 2 3 4 5 6 7 8 9 10 11 12 13])

(defn deal-cards [app & args]
  (let [cards (shuffle (mapcat (fn [rank] (map (fn [suit] {:rank rank :suit suit})
                                              suits))
                               ranks))
        cards-by-player (partition 13 cards)]
    (.log js/console (pr-str (first  (:players     (assoc app :players (map-indexed #(merge %2 {:cards (nth cards-by-player %1)})
                                     (:players app)))))))
    (assoc app :players (apply vector (map-indexed #(merge %2 {:cards (nth cards-by-player %1)})
                                     (:players app))))))

(om/root
 (fn [app owner]
   (dom/div nil
            (apply dom/ul nil
                   (map-indexed (partial render-player app)
                                ["bottom" "left" "top" "right"]))))
 app-state
 {:target (. js/document (getElementById "app"))})

(swap! app-state deal-cards)
