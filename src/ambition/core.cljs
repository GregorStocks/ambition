(ns ambition.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(defn log [arg]
  (.log js/console arg))

(def app-state (atom {:current-player-index 0
                      :winning-player-index nil
                      :user-player-index 0
                      :past-tricks []
                      :current-trick []
                      :players [{:name "Gregor"
                                 :cards []
                                 :points 0
                                 :score 0
                                 :strikes 0}
                                {:name "CPU 1"
                                 :cards []
                                 :points 0
                                 :score 0
                                 :strikes 0}
                                {:name "CPU 2"
                                 :cards []
                                 :points 0
                                 :score 0
                                 :strikes 0}
                                {:name "CPU 3"
                                 :cards []
                                 :points 0
                                 :score 0
                                 :strikes 0}]}))


(defn valid-plays [app pid]
  (let [cards (-> app deref :players (nth pid) :cards)
        played-suit (or (:suit (first (:current-trick @app)))
                        (when (empty? (:past-tricks @app))
                          :diamonds))]
    (or (seq (filter #(= (:suit %) played-suit) cards))
        cards)))

(def suits [:hearts :diamonds :spades :clubs])
(def ranks [1 2 3 4 5 6 7 8 9 10 11 12 13])
(defn deal-cards [app]
  (let [cards (shuffle (mapcat (fn [rank] (map (fn [suit] {:rank rank :suit suit})
                                              suits))
                               ranks))
        cards-by-player (map (partial sort-by #(+ (if (= (:rank %) 1)
                                                    14
                                                    (:rank %))
                                                  (case (:suit %)
                                                    :clubs 0
                                                    :hearts 50
                                                    :diamonds 100
                                                    :spades 150)))
                             (partition 13 cards))
        players (map-indexed (fn [pid player]
                               (let [annotate-card #(assoc % :player pid)
                                     raw-cards (nth cards-by-player pid)]
                                 (assoc player :cards
                                        (map annotate-card raw-cards))))
                             (:players app))
        eight (first (filter #(and (= (:rank %) 8)
                                   (= (:suit %) :diamonds))
                             (mapcat :cards players)))
        starter (:player eight)]
    (assoc app
      :players (apply vector players)
      :current-player-index starter)))



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

(defn trick-value [app]
  (let [trick (:current-trick app)
        _ (log "DUHHH")
        card-point-value (reduce + (map point-value trick))
        point-value (+ card-point-value
                       (if (empty? (:past-tricks app)) 10 0))]
    point-value))

(def slam-cutoff 75)
(defn end-round! [app]
  (log "ROUND OVER")
  (log (pr-str app))
  (when-not (empty? (:past-tricks @app))
    (let [players (:players @app)
          max-points (apply max (map :points players))
          num-nils (count (filter #(= (:points %) 0) players))
          round-result #(let [points (:points %)]
                          (cond
                           (and (zero? points) (>= max-points slam-cutoff)) {:score 0 :strikes 1}
                           (and (zero? points) (= num-nils 1)) {:score 30}
                           (zero? points) {:score 15}
                           (< points 15) {:score points :strikes 1}
                           (< points max-points) {:score points}
                           (= points 120) {:score 120}
                           (>= points slam-cutoff) {:score (+ 50
                                                              (max 0 (- points 75))
                                                              (max 0 (- points 105)))}
                           :else {:score 0 :strikes 1}))]
      (om/transact! app :players (partial map #(merge-with + % (round-result %))))))

  (om/transact! app [:past-tricks] (constantly []))
  (om/transact! app :players (partial map #(assoc % :points 0)))

    (if-let [strikeouts (seq (filter #(>= (:strikes %) 4) (:players @app)))]
      (let [winner (first (sort-by #(- (:score %) (if (>= (:strikes %) 4) 10000 0))
                                   (:players @app)))]
        (log (str  "ALL HAIL KING WINNER MAN HE IS" (pr-str winner))))
      (om/transact! app deal-cards)))

(defn end-trick! [app]
  (let [trick (:current-trick @app)
        valid-suit (:suit (first trick))
        cards-on-suit (filter #(= (:suit %) valid-suit) trick)
        value (fn [{:keys [rank suit]}]
                (cond (= rank 1) 15
                      (and (= rank 2)
                           (some is-honor? cards-on-suit)) 20
                      :else rank))
        winner-pid (:player (last (sort-by value cards-on-suit)))
        point-value (trick-value @app)]
    (om/transact! app :current-player-index (constantly nil)) ;; nobody move!
    (om/transact! app :winning-player-index (constantly winner-pid))
    (js/setTimeout
     (fn []
       (om/transact! app [:players winner-pid :points] (partial + point-value))
       (om/transact! app [:current-trick] (constantly []))
       (om/transact! app [:past-tricks] #(conj % trick))
       (om/transact! app :winning-player-index (constantly nil))
       (if (= 13 (count (:past-tricks @app)))
         (end-round! app)
         (do
           (when-not (= winner-pid (:user-player-index @app)) (js/setTimeout (partial tick app) 2000))
           (om/transact! app :current-player-index (constantly winner-pid)))))
     3000)))

(defn play-card [app pid card]
  (when (and (= (:current-player-index @app) pid)
             (some (partial = card) (valid-plays app pid)))
    (om/transact! app [:players pid :cards] (partial filter #(not= card %)))
    (om/transact! app [:current-trick] #(conj % card))
    (om/transact! app :current-player-index #(mod (inc %)
                                                  (count (:players @app))))
    (if (= 4 (count (:current-trick @app)))
      (end-trick! app)
      (when-not (= (:current-player-index @app) (:user-player-index @app))
        (js/setTimeout (partial tick app) 500)))))

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
   (dom/p nil (str (point-value card)))))

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
           (if (= pid (:user-player-index app))
             (map (partial card-view app pid) (:cards player))
             (vector (render-cardback (count (:cards player))))))))

(defn tick [app]
  (log "ASSES")
  (if-let [pid (:current-player-index @app)]
    (when-not (= (:user-player-index @app) pid)
      (if-let [card (rand-nth (valid-plays app pid))]
        (play-card app pid card)
        (log "FUCKING WHAT")))
    (log "FUUUUUUUUUCKING CALLED TICK WRONG TIME")))

(defn game-view [app owner]
  (reify
    om/IInitState
    (init-state [this]
      (let [result (deal-cards app)]
        (when-not (= (:current-player-index app)
                     (:user-player-index app))
          (js/setTimeout (partial tick app) 500))
        result))
    om/IRender
    (render [this]
      (log " rendering")
      (let [points (mapcat #(map point-value (:cards %)) (:players app) )]
        (dom/div nil
                 (apply dom/ul #js {:className "trick"}
                        (dom/li nil
                                "Remaining points: " (reduce + points))
                        (when (empty? (:past-tricks app))
                          (dom/li nil "First trick: 10 points"))
                        (let [value (trick-value app)]
                          (dom/li nil "Current trick: " value " points"))
                        (map (partial card-view app 0) (:current-trick app)))
                 (apply dom/ul nil
                        (map-indexed (partial render-player app owner)
                                     ["bottom" "left" "top" "right"])))))))

(om/root
 game-view
 app-state
 {:target (. js/document (getElementById "app"))})
