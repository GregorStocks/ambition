(ns ambition.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ambition.model :as model]))

(enable-console-print!)

(defn log [arg]
  (.log js/console arg))

(defn ticks [t]
  (* t 100))

(def app-state
  (atom
   (merge
    (model/deal-cards
     {:current-player-index 0
      :winning-player-index nil
      :user-player-index 0
      :past-tricks []
      :current-trick []
      :players [{:name "Gregor"
                 :index 0
                 :cards []
                 :points 0
                 :score 0
                 :strikes 0}
                {:name "CPU 1"
                 :index 1
                 :cards []
                 :points 0
                 :score 0
                 :strikes 0}
                {:name "CPU 2"
                 :index 2
                 :cards []
                 :points 0
                 :score 0
                 :strikes 0}
                {:name "CPU 3"
                 :index 3
                 :cards []
                 :points 0
                 :score 0
                 :strikes 0}]})
    {:current-player-index 0})))


(defn end-trick! [app]
  (let [trick (:current-trick @app)
        valid-suit (:suit (first trick))
        cards-on-suit (filter #(= (:suit %) valid-suit) trick)
        value (fn [{:keys [rank suit]}]
                (cond (= rank 1) 15
                      (and (= rank 2)
                           (some model/is-honor? cards-on-suit)) 20
                      :else rank))
        winner-pid (:player (last (sort-by value cards-on-suit)))
        point-value (model/trick-value @app)]
    (om/transact! app [:past-tricks] #(conj % trick))
    (om/transact! app [:current-trick] (constantly []))
    (om/transact! app :current-player-index (constantly nil)) ;; nobody move!
    (om/transact! app :winning-player-index (constantly winner-pid))
    (js/setTimeout
     (fn []
       (om/transact! app [:players winner-pid :points] (partial + point-value))
       (om/transact! app :winning-player-index (constantly nil))
       (if (= 13 (count (:past-tricks @app)))
         (do
           (om/transact! app model/end-round)
           (when-not (= :game-over (:stage @app))
             (js/setTimeout #(do
                               (om/transact! app model/deal-cards)
                               (js/setTimeout (partial tick app)
                                              (ticks 5)))
                            (ticks 10))))
         (do
           (when-not (and (= winner-pid (:user-player-index @app)) model/dont-play-for-user)
             (js/setTimeout (partial tick app) (ticks 2)))
           (om/transact! app :current-player-index (constantly winner-pid)))))
     (ticks 3))))

(defn play-card [app pid card]
  (when (and (= (:current-player-index @app) pid)
             (some (partial = card) (model/valid-plays @app pid)))
    (om/transact! app [:players pid :cards] (partial filter #(not= card %)))
    (om/transact! app [:current-trick] #(conj % card))
    (om/transact! app :current-player-index #(mod (inc %)
                                                  (count (:players @app))))
    (if (= 4 (count (:current-trick @app)))
      (end-trick! app)
      (when-not (and (= (:current-player-index @app) (:user-player-index @app)) model/dont-play-for-user)
        (js/setTimeout (partial tick app) (ticks 0.5))))))

(defn card-view [app pid card]
  (dom/div #js {:className "card"}
   (dom/img #js {:onClick #(when pid (play-card app pid card))
                 :src (str "card-images/"
                           (+ (* 4 (mod (- 14 (:rank card)) 13))
                              (case (:suit card)
                                :clubs 1
                                :spades 2
                                :hearts 3
                                :diamonds 4))
                           ".png")})
   (dom/p nil (str (model/point-value card)))))

(defn render-cardback [card-count]
  (dom/div #js {:className "cardback"}
           (str card-count " cards")))

(defn render-player [app owner pid classname]
  (let [player (-> app :players (get pid))]
    (apply dom/li
           #js {:className (str classname " player")}
           (dom/h1 #js {:className
                        (str "playerName"
                             (cond
                              (= pid (:current-player-index app))  " activePlayer"
                              (= pid (:winning-player-index app))  " winningPlayer"))}
                   (:name player) ": "
                   (:points player) " Points")
           (dom/h3 nil "Score: " (:score player))
           (dom/h3 nil "Strikes: " (:strikes player))
           (if (= pid (:user-player-index app))
             (map (partial card-view app pid) (:cards player))
             (vector (render-cardback (count (:cards player))))))))

(defn tick [app]
  (if-let [pid (:current-player-index @app)]
    (when-not (and (= (:user-player-index @app) pid)
                   model/dont-play-for-user)
      (if-let [card (rand-nth (model/valid-plays @app pid))]
        (play-card app pid card)
        (log "FUCKING WHAT")))
    (log "FUUUUUUUUUCKING CALLED TICK WRONG TIME")))

(om/root
 (fn [app owner]
   (let [points (mapcat #(map model/point-value (:cards %)) (:players app) )]
     (dom/div nil
              (when (or (:current-player-index app)
                        (seq (:past-tricks app)))
                (apply dom/ul #js {:className "trick"}
                       (dom/li nil
                               "Remaining points: " (reduce + points))
                       (when (empty? (:past-tricks app))
                         (dom/li nil "First trick: 10 points"))
                       (let [value (model/trick-value app)]
                         (dom/li nil "Current trick: " value " points"))
                       (map (partial card-view app 0) (:current-trick app))))
              (apply dom/ul nil
                     (map-indexed (partial render-player app owner)
                                  ["bottom" "left" "top" "right"])))))
 app-state
 {:target (. js/document (getElementById "app"))})
