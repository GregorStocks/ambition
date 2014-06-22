(ns ambition.deathmatch
  (:require [ambition.model :as model]
            [ambition.ai :as ai]))

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
  (let [win-freqs (frequencies (map #(:ai-type (nth
                                            (:players %)
                                            (:winner %)))
                                    results))
        participate-freqs (frequencies (mapcat #(map :ai-type (:players %)) results))]
    (map (fn [[pid wins]] (str pid ": " wins " wins ("
                              (long (* 100 (/ wins (participate-freqs pid))))
                              "%)\n"))
         (sort-by first win-freqs))))

(defn infinite-seq [elems]
  (concat elems (lazy-seq (infinite-seq elems))))

(defn -main [& args]
  (let [ais [(ai/biggest-ai)
             (ai/littlest-ai)
             (ai/trick-winner-ai)
             (ai/trick-loser-ai)
             (ai/second-place-ai)
             (ai/random-ai)]
        results (map run-game (partition 4 (take 4000 (infinite-seq ais))))]
    (println (summarize-results results))))
