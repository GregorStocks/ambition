(ns ambition.model
  (:require
   #+clj [clojure.tools.logging :as log]))

(defn log [& args]
  #+cljs (.log js/console (apply pr-str args))
  #+clj (log/info args))

(defn base-app-state [players]
  {:current-player-index 0
   :winning-player-index nil
   :user-player-index 0
   :past-tricks []
   :current-trick []
   :stage :init
   :ticks-since-update 0
   :players players})

(def suits [:hearts :diamonds :spades :clubs])
(def ranks [1 2 3 4 5 6 7 8 9 10 11 12 13])
(def dont-play-for-user
  #+clj false
  #+cljs true)
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
                                 (assoc player
                                   :cards (map annotate-card raw-cards)
                                   :points 0)))
                             (:players app))
        eight (first (filter #(and (= (:rank %) 8)
                                   (= (:suit %) :diamonds))
                             (mapcat :cards players)))
        starter (:player eight)]
    (assoc app
      :players (apply vector players)
      :current-player-index starter)))

(defn valid-plays [app pid]
  (let [cards (-> app :players (nth pid) :cards)
        played-suit (or (:suit (first (:current-trick app)))
                        (when (empty? (:past-tricks app))
                          :diamonds))]
    (or (seq (filter #(= (:suit %) played-suit) cards))
        cards)))

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
        card-point-value (reduce + (map point-value trick))
        point-value (+ card-point-value
                       (if (empty? (:past-tricks app)) 10 0))]
    point-value))

(def slam-cutoff 75)
(defn summarize-round [app]
  (when-not (empty? (:past-tricks app))
    (let [players (:players app)
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
                           :else {:score 0 :strikes 1}))
          updated (-> app
                      (update-in [:players] (comp
                                             #(apply vector %)
                                             (partial map #(merge-with + % (round-result %)))))
                      (assoc :stage :round-summary)
                      (assoc :round-results (map round-result (:players app))))]
      updated)))


(defn summarize-trick [app]
  (let [trick (:current-trick app)
        valid-suit (:suit (first trick))
        cards-on-suit (filter #(= (:suit %) valid-suit) trick)
        value (fn [{:keys [rank suit]}]
                (cond (= rank 1) 15
                      (and (= rank 2)
                           (some is-honor? cards-on-suit)) 20
                      :else rank))
        winner-pid (:player (last (sort-by value cards-on-suit)))
        point-value (trick-value app)
        result (->
                app
                (update-in [:past-tricks] conj trick)
                (dissoc :current-trick)
                (dissoc :current-player-index)
                (assoc :winning-player-index winner-pid)
                (assoc :trick-value point-value)
                (update-in [:players winner-pid :points] + point-value)
                (assoc :stage :trick-summary))]
    result))

(defn next-pid [app pid]
  (mod (inc pid)
       (count (:players app))))

(defn play-card [card app]
  (let [pid (:player card)]
    (if (some (partial = card)
                (valid-plays app pid))
      (let [result
            (-> app
                (update-in [:players pid :cards] (partial filter #(not= card %)))
                (update-in [:current-trick] conj card)
                (update-in [:current-player-index] (partial next-pid app)))]
        result)
      app)))

(defn ai-pick-card [app pid]
  (let [ai (:play-card (nth (:players app) pid))]
    (ai app pid)))

(defn summarize-game [app]
  (let [winner (last (sort-by #(- (:score %)
                                  (if (>= (:strikes %) 4) 10000 0))
                              (:players app)))]
    (assoc app
      :stage :game-summary
      :past-tricks []
      :winner (:index winner))))

(defn start-new-trick [app]
  (assoc app
    :stage :trick
    :current-trick []
    :current-player-index (:winning-player-index app)))

(defn start-new-round [app]
  (assoc (deal-cards app)
    :past-tricks []
    :current-trick []
    :stage :trick))

(defn start-new-game [app]
  (let [reset-points #(merge % {:points 0 :score 0 :strikes 0})
        result (-> app
                   (update-in [:players] #(apply vector (map reset-points %)))
                   start-new-round)]
    result))

(defn run-one-update [app]
  (let [result (case (:stage app)
                 :init (start-new-game app)
                 :trick (do
                          (if (= 4 (count (:current-trick app)))
                            (do
                              (summarize-trick app))
                            (do
                              (let [pid (:current-player-index app)
                                    card (ai-pick-card app pid)]
                                (if (or (not card)
                                        (and (= (:user-player-index app) pid)
                                             dont-play-for-user))
                                  app
                                  (play-card card app))))))
                 :trick-summary (if (= 13 (count (:past-tricks app)))
                                  (summarize-round app)
                                  (start-new-trick app))
                 :round-summary (let [strikeouts (filter #(>= (:strikes %) 4) (:players app))]
                                  (if (seq strikeouts)
                                    (summarize-game app)
                                    (start-new-round app)))
                 :game-summary (start-new-game app)
                 (do
                   (log "WTF" (pr-str app))))
        adjusted (assoc result :ticks-since-update 0)]
    adjusted))
