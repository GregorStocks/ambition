(ns ambition.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [ambition.model :as model]))

(enable-console-print!)

(defn ticks [t]
  (* t 100))

(def app-state
  (atom
   {:current-player-index 0
    :winning-player-index nil
    :user-player-index 0
    :past-tricks []
    :current-trick []
    :stage :init
    :ticks-since-update 0
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
               :strikes 0}]}))

(defn card-view [app pid card]
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
   (dom/p nil (str (model/point-value card)))))

(defn render-cardback [card-count]
  (dom/div #js {:className "cardback"}
           (str card-count " cards")))

(defn render-player [app owner pid classname]
  (let [player (-> app :players (get pid))]
    (apply dom/li
           #js {:className (str classname " player")}
           (dom/h1 #js {:className (str "playerName"
                                        (if (:current-player-index app)
                                          (when (= pid (:current-player-index app))  " activePlayer")
                                          (when (= pid (:winning-player-index app))  " winningPlayer")))}
                   (:name player) ": "
                   (:points player) " Points")
           (dom/h3 nil "Score: " (:score player))
           (dom/h3 nil "Strikes: " (:strikes player))
           (if (= pid (:user-player-index app))
             (map (partial card-view app pid) (:cards player))
             (vector (render-cardback (count (:cards player))))))))

(def wait-times {:init 0
                 :trick 1
                 :trick-summary 5
                 :round-summary 7
                 :game-summary 10})
(defn ticks->ms [ticks]
  (* ticks 500))

(defn run-one-update [app]
  (model/run-one-update app))

(defn tick [app]
  (let [stage (:stage @app)
        ticks-since-update (:ticks-since-update @app)
        should-tick? (>= ticks-since-update (wait-times stage))]
    (if should-tick?
      (om/transact! app model/run-one-update)
      (om/transact! app :ticks-since-update inc)))
  (js/setTimeout (partial tick app) (ticks->ms 1)))

(defn game-content [app]
  (case (:stage app)
    :init (dom/div nil)
    :trick (dom/div nil
                    (when (or (:current-player-index app)
                              (seq (:past-tricks app)))
                      (apply dom/ul #js {:className "trick"}
                             (dom/li nil
                                     "Remaining points: " (reduce + (mapcat #(map model/point-value (:cards %)) (:players app) )))
                             (when (empty? (:past-tricks app))
                               (dom/li nil "First trick: 10 points"))
                             (let [value (model/trick-value app)]
                               (dom/li nil "Current trick: " value " points"))
                             (map (partial card-view app 0) (:current-trick app)))))
    :trick-summary (let [winner (nth
                                 (:players app)
                                 (:winning-player-index app))]
                     (dom/div nil
                              (dom/h1 nil "Trick summary: " (:name winner)
                                      " wins and gets "
                                      (:trick-value app)
                                      " points. wooooo")))
    :round-summary (dom/div nil
                            (dom/h1 nil "Round summary: " (pr-str (:round-results app))))
    :game-summary (dom/div nil
                           (dom/h1 nil "Game summary: winner is " (:name ((:winner app)
                                                                         (:players app)))))))

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
                (apply dom/ul nil
                       (map-indexed (partial render-player app owner)
                                    ["bottom" "left" "top" "right"]))))))
 app-state
 {:target (. js/document (getElementById "app"))})
