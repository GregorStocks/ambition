(ns ambition.deathmatch
  (:require [ambition.model :as model]
            [ambition.ai :as ai]
            [clojure.tools.logging :as log]))

(defn run-game [ais]
  (let [players (map-indexed (fn [pid ai] (merge ai {:index pid
                                                    :cards []
                                                    :points 0
                                                    :score 0
                                                    :strikes 0}))
                             ais)]
    (loop [app (model/base-app-state players)]
      (when (:stage app)
        (if (= (:stage app) :game-summary)
          app
          (recur (model/run-one-update app)))))))

(defn summarize-results [results]
  (let [freqs (frequencies (map :winner results))]
    (map (fn [[pid wins]] (str pid ": " wins " wins.\n"))
         (sort-by first freqs))))

(defn -main [& args]
  (let [ais [(ai/biggest-ai)
             (ai/littlest-ai)
             (ai/slammer-ai)
             (ai/littlest-ai)]
        results (repeatedly 1000 (partial run-game ais))]
    (println (summarize-results results))))
