(ns ambition.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ambition.model :as model]
            [ambition.ai :as ai]
            #+clj [ambition.deathmatch :as deathmatch]))

#+cljs
(do
  (enable-console-print!)

  (def app-state
    (atom
     (model/base-app-state
      (cons (merge (ai/littlest-ai)
                   {:name "You"
                    :index 0
                    :cards []
                    :points 0
                    :score 0
                    :strikes 0})
            (map #(ai/make-ai-player %) (range 1 4))))))

  (defn card-view [app pid points? card]
    (dom/div
     #js {:className "card"}
     (dom/img #js {:onClick #(when (= (:current-player-index @app) pid)
                               (om/transact! app (partial model/play-card card)))
                   :src (str "card-images/"
                             (+ (* 4 (mod (- 14 (:rank card)) 13))
                                (case (:suit card)
                                  :clubs 1
                                  :spades 2
                                  :hearts 3
                                  :diamonds 4))
                             ".png")})
     (when points?
       (dom/p nil (str (model/point-value card))))))

  (defn render-cardback [card-count]
    (dom/div #js {:className "cardback"}
             (str card-count " cards")))

  (defn render-player [app owner pid classname]
    (let [player (-> app :players (get pid))]
      (dom/li
       #js {:className (str classname " player")}
       (dom/h3 #js {:className (str "playerName"
                                    (if (:current-player-index app)
                                      (when (= pid (:current-player-index app))  " activePlayer")
                                      (when (= pid (:winning-player-index app))  " winningPlayer")))}
               (:name player) ": "
               (:points player) " Points")
       (dom/h4 nil "Score: " (:score player))
       (dom/h4 nil "Strikes: " (:strikes player))
       (if (= pid (:user-player-index app))
         (apply dom/div #js {:className "player-cards"}
                (map (partial card-view app pid true) (:cards player)))
         (render-cardback (count (:cards player)))))))

  (def wait-times {:init 0
                   :trick 1
                   :trick-summary 5
                   :round-summary 20
                   :game-summary 50})
  (defn ticks->ms [ticks]
    (* ticks 500))

  (defn tick [app]
    (let [stage (:stage @app)
          ticks-since-update (:ticks-since-update @app)
          should-tick? (>= ticks-since-update (wait-times stage))]
      (if should-tick?
        (om/transact! app model/run-one-update)
        (om/transact! app :ticks-since-update inc)))
    (js/setTimeout (partial tick app) (ticks->ms 1)))

  (defn cell-for-player-card [app pid]
    (let [result     (if-let [card (first (filter #(= (:player %) pid)
                                                  (:current-trick app)))]
                       (dom/td #js {:className (str "player" pid "card")}
                               (card-view app pid false card))
                       (dom/td #js {:className (str "player" pid "card")}))]
      result))

  (defn game-content [app]
    (case (:stage app)
      :init (dom/div nil)
      :trick (dom/div nil
                      (when (or (:current-player-index app)
                                (seq (:past-tricks app)))
                        (dom/ul #js {:className "trick"}
                                (dom/li nil
                                        "Remaining points: " (reduce + (mapcat #(map model/point-value (:cards %)) (:players app) )))
                                (when (empty? (:past-tricks app))
                                  (dom/li nil "First trick: 10 points"))
                                (let [value (model/trick-value app)]
                                  (dom/li nil "Current trick: " value " points"))
                                (dom/table #js {:className "played-cards"}
                                           (dom/tr nil
                                                   (dom/td nil " ")
                                                   (cell-for-player-card app 2))
                                           (dom/tr nil
                                                   (cell-for-player-card app 1)
                                                   (dom/td nil " ")
                                                   (cell-for-player-card app 3))
                                           (dom/tr nil
                                                   (dom/td nil " ")
                                                   (cell-for-player-card app 0))))))
      :trick-summary (let [winner (nth
                                   (:players app)
                                   (:winning-player-index app))]
                       (dom/div #js {:className "trick-summary summary"}
                                (dom/h2 nil (str
                                             (:name winner)
                                             " won the trick and got "
                                             (:trick-value app)
                                             " points."))))
      :round-summary (dom/div #js {:className "round-summary summary"}
                              (apply dom/h2 nil
                                     (map-indexed
                                      #(let [result (nth (:round-results app)
                                                         %1)]
                                         (dom/h3 nil
                                                 (str (:name %2) " got a score of "
                                                      (or (:score result) 0) " and "
                                                      (or (:strikes result) 0) " strikes.")))
                                      (:players app))))
      :game-summary (let [winner (nth (:players app) (:winner app))]
                      (dom/div #js {:className "game-summary summary"}
                               (dom/h1 nil "Game summary: winner is " (:name winner) ". GOODBYE")))))

  (om/root
   (fn [app owner]
     (reify
       om/IWillMount
       (will-mount [this]
         (js/setTimeout (partial tick app) (ticks->ms 1)))
       om/IRender
       (render [this]
         (dom/div nil
                  (game-content app)
                  (dom/p #js {:className "footer"}
                         (dom/a #js {:href "https://github.com/GregorStocks/ambition"}
                                "https://github.com/GregorStocks/ambition")
                         " "
                         (dom/a #js {:href "http://ambition.techcrunks.com"}
                                "http://ambition.techcrunks.com"))
                  (apply dom/ul nil
                         (map-indexed (partial render-player app owner)
                                      ["bottom" "left" "top" "right"]))))))
   app-state
   {:target (. js/document (getElementById "app"))}))

#+clj
(defn -main [& args]
  (apply deathmatch/-main args))
