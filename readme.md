Running the webapp
==================
`lein cljx once && lein cljsbuild once` and open public/index.html in an browser. done.

AI deathmatch
============
Plays all the AIs against each other a thousand times or so and reports the results.

`lein cljx once && lein run`

Produces a result something like

(:big: 215 wins (32%)
 :little: 138 wins (20%)
 :random: 131 wins (19%)
 :round-loser: 132 wins (19%)
 :second-place: 192 wins (28%)
 :slammer: 192 wins (28%)
)
