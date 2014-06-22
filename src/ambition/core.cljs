(ns ambition.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defn log [arg]
  (.log js/console arg))

(def app-state (atom {:text "HELLO"
                      :current-player-index 0
                      :user-player-index 0
                      :past-tricks []
                      :current-trick []
                      :players [{:name "Gregor"
                                 :cards []
                                 :points 0}
                                {:name "CPU 1"
                                 :cards []
                                 :points 0}
                                {:name "CPU 2"
                                 :cards []
                                 :points 0}
                                {:name "CPU 3"
                                 :cards []
                                 :points 0}]}))

(defn tick [app]
  (when-let [pid (:current-player-index @app)]
    (when-not (= (:user-player-index @app) pid)
      (let [cards (-> app deref :players (nth pid) :cards)
            played-suit (:suit (first (:current-trick @app)))
            valid-cards (or (seq (filter #(= (:suit %) played-suit) cards))
                            cards)
            card (rand-nth valid-cards)]
        (play-card app pid card)))))

(defn is-honor? [{:keys [rank]}]
  (or (> rank 10)
      (= rank 1)))

(defn point-value [{:keys [rank suit] :as card}]
  (let [multiplier (case suit
                     :hearts 1
                     :diamonds 1
                     :spades 2
                     :clubs 0)]
    (cond
     (and (= suit :clubs) (= rank 13)) 17
     (is-honor? card) (* multiplier 3)
     (and (= suit :hearts) (= rank 2)) 10
     :else multiplier)))

(defn end-trick! [app]
  (om/transact! app :current-player-index (constantly nil)) ;; nobody move!
  (js/setTimeout (partial score-trick! app) 2000))

(defn score-trick! [app]
  (let [trick (:current-trick @app)
        valid-suit (:suit (first trick))
        cards-on-suit (filter #(= (:suit %) valid-suit) trick)
        value (fn [{:keys [rank suit]}]
                (cond (= rank 1) 15
                      (and (= rank 2)
                           (some is-honor? cards-on-suit)) 20
                      :else rank))
        winner-pid (:player (last (sort-by value cards-on-suit)))
        card-point-value (reduce + (map point-value trick))
        point-value (+ card-point-value
                       (if (empty? (:previous-tricks @app)) 10 0))]
    (om/transact! app [:players winner-pid :points] (partial + point-value))
    (om/transact! app [:current-trick] (constantly []))
    (om/transact! app :current-player-index (constantly winner-pid))
    (js/setTimeout (partial tick app) 1000)))

(defn play-card [app pid card]
  (when (= (:current-player-index @app) pid)
    (om/transact! app [:players pid :cards] (partial filter #(not= card %)))
    (om/transact! app [:current-trick] #(conj % card))
    (om/transact! app :current-player-index #(mod (inc %)
                                                  (count (:players @app))))
    (when (= 4 (count (:current-trick @app)))
      (end-trick! app))
    (js/setTimeout (partial tick app) 1000)))

(defn card-view [app pid card]
  (let [munged-rank (mod (- 14 (:rank card)) 13)]
    (dom/img #js {:onClick #(when pid (play-card app pid card))
                  :src (str "card-images/"
                            (+ (* 4 munged-rank)
                               (case (:suit card)
                                 :clubs 1
                                 :spades 2
                                 :hearts 3
                                 :diamonds 4))
                            ".png")})))

(defn render-cardback [card-count]
  (dom/div #js {:className "cardback"}
           (str card-count " cards")))

(defn render-player [app owner pid classname]
  (let [player (-> app :players (get pid))]
    (apply dom/li
           #js {:className (str classname " player")}
           (dom/h1 #js {:className "playerName"}
                   (:name player) ": "
                   (:points player) " Points")
           (if (= pid (:user-player-index app))
             (map (partial card-view app pid) (:cards player))
             (vector (render-cardback (count (:cards player))))))))

(def suits [:hearts :diamonds :spades :clubs])
(def ranks [1 2 3 4 5 6 7 8 9 10 11 12 13])

(defn deal-cards [app & args]
  (let [cards (shuffle (mapcat (fn [rank] (map (fn [suit] {:rank rank :suit suit})
                                              suits))
                               ranks))
        cards-by-player (partition 13 cards)
        players (map-indexed (fn [pid player]
                               (let [annotate-card #(assoc % :player pid)
                                     raw-cards (nth cards-by-player pid)]
                                 (assoc player :cards
                                        (map annotate-card raw-cards))))
                             (:players app)) ]
    (assoc app :players (apply vector players))))

(om/root
 (fn [app owner]
   (dom/div nil
            (apply dom/ul nil
                   (map (partial card-view app 0) (:current-trick app)))
            (apply dom/ul nil
                   (map-indexed (partial render-player app owner)
                                ["bottom" "left" "top" "right"]))))
 app-state
 {:target (. js/document (getElementById "app"))})

(swap! app-state deal-cards)
