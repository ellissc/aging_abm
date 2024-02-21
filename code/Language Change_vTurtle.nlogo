;; TODO: Potentially swap out a person, replace turtles -- could keep the population size the same
;; Replace in a linked fashion
;; think about what it would mean for a new agent / set of agents to be added
;; Stochastic block model? Connected caveman networks (similar to small world, then break a tie and reconnect)?
;; YA should have weaker preferences? They're embedded and are constantly getting input / reinforcement
;; Switch to an associative learning method?
;; Need different assumptions -- figure out the explanation for what is going on...
;; Ambiguous sentence as norm evolution?

extensions [nw]
globals [max-age]

;breed [turtles turtle]

turtles-own [
  state            ;; current grammar state (ranges from 0 to 1)
  orig-state       ;; each turtle's initially assigned grammar state
  spoken-state     ;; output of turtle's speech (0 or 1)
  age              ;; current age of turtle
  gamma            ;; learning rate of turtle
  cohort           ;; group of turtle (1 or 2)
]

;;;
;;; SETUP PROCEDURES
;;;

to setup
  clear-all
  ask patches [ set pcolor 8 ]
;  repeat num-turtles [ make-turtle ]

  ifelse (network-type = "small-world")
  [ nw:generate-watts-strogatz turtles links num-turtles 2 0.1 [ turtle-setup ] ]
  [ ifelse (network-type = "preferential")
    [nw:generate-preferential-attachment turtles links num-turtles min-degree [ turtle-setup ]]
    [nw:generate-random turtles links num-turtles random-link-prob [ turtle-setup ] ]
  ]



  ask links [set color black]

  initialize-groups
  distribute-grammars
;  create-network

  let init-max-age (max [age] of turtles)
  set max-age (simulation-limit + init-max-age)

  if initial-gamma-based-on-age?
  [ ask turtles [ gamma-by-age age gamma-change ] ]

  repeat 100 [ layout ]
  reset-ticks
end

to make-turtle
  create-turtles 1 [
    rt random-float 360
    fd max-pxcor
    set size 3
    set state 0.0
    set age 0

    ifelse deterministic-gamma?
    [ set gamma initial-gamma ]
    [ let temp-gamma random-normal initial-gamma gamma-sd
      ifelse temp-gamma > 0
      [ set gamma temp-gamma ]
      [ set gamma 0.01 ]
    ]
  ]
end

to turtle-setup
  rt random-float 360
    fd max-pxcor
    set size 2
    set state 0.0
    set age 0

    ifelse deterministic-gamma?
    [ set gamma initial-gamma ]
    [ let temp-gamma random-normal initial-gamma gamma-sd
      ifelse temp-gamma > 0
      [ set gamma temp-gamma ]
      [ set gamma 0.01 ]
    ]
end

to initialize-groups
  ask turtles [set cohort 1]

  ask n-of ((percent-cohort2 / 100) * num-turtles) turtles
  [ set cohort 2]

  ask turtles [ initialize-age ]

  ask turtles [ update-shape ]
end

to update-shape
  ifelse cohort = 1
  [ set shape "square" ]
  [ set shape "triangle" ]
end

to initialize-age
  ifelse deterministic-age?
  [
    ifelse (cohort = 1)
    [ set age cohort1-age ]
    [ set age cohort2-age ]
  ]

  [ ifelse (cohort = 1)
    [
      let temp-age random-normal cohort1-age age-sd
      ifelse temp-age > 0
      [ set age temp-age]
      [ set age 0]
    ]
    [
      let temp-age random-normal cohort2-age age-sd
      ifelse temp-age > 0
      [ set age temp-age]
      [ set age 0]
    ]
  ]
end

to distribute-grammars
  ask turtles [ set state 0 ]

  ifelse cohort-based-grammar
  [
    let cohort1-num count turtles with [cohort = 1]
    let cohort2-num count turtles with [cohort = 2]
    ask n-of ((C1-percent-grammar-1 / 100) * cohort1-num) turtles with [cohort = 1]
    [ set state 1.0 ]

    ask n-of ((C2-percent-grammar-1 / 100) * cohort2-num) turtles with [cohort = 2]
    [ set state 1.0 ]
  ]
  [
    ask n-of ((percent-grammar-1 / 100) * num-turtles) turtles
    [ set state 1.0 ]
  ]

  if random-init-state [
    ask turtles [set state random-float 1.0]
  ]

  ask turtles [
    set orig-state state
    set spoken-state state
    update-color
  ]
end

to-report cohort1-state
  report mean [state] of turtles with [cohort = 1]
end

to-report cohort2-state
  report mean [state] of turtles with [cohort = 2]
end

to create-network
  let partner nobody
  let first-turtle one-of turtles
  let second-turtle one-of turtles with [self != first-turtle]
  ask first-turtle [ create-link-with second-turtle [ set color white ] ]
  let new-turtle one-of turtles with [not any? link-neighbors]
  while [new-turtle != nobody] [
    set partner find-partner
    ask new-turtle [ create-link-with partner [ set color white ] ]
    layout
    set new-turtle one-of turtles with [not any? link-neighbors]
  ]
end

to update-color
  set color scale-color red state 0 1
end

to grow-older [ growth-influence ]
  set age age + 1
  if (abs(growth-influence) = 1) [ gamma-by-age age growth-influence ]
end

to gamma-by-age [ age-param growth-influence ]
  let a perseverance
  let b initial-gamma
  if (growth-influence = 1) [ set b 0.01 ]
  if (growth-influence = -1) [ set b 0.04 ]
  if (growth-influence = 0) [ set b 0.025 ]
  let constant 0.04
  let age-prop (age-param / max-age)
  let new-gamma ((constant * growth-influence *(age-prop ^ a) ) + b)
  set gamma new-gamma
end

to reset-turtles
  clear-all-plots
  ask turtles [
    set state orig-state
    update-color
  ]
  reset-ticks
end

to redistribute-grammars
  clear-all-plots
  distribute-grammars
  reset-ticks
end

to-report orig-grammar-string
  report ifelse-value orig-state = 1.0 ["1"] ["0"]
end

;;;
;;; GO PROCEDURES
;;;

to go
  if (ticks = simulation-limit) [stop]
  ask turtles [ communicate-via update-algorithm ]
  ask turtles [ update-color ]
  ask turtles [ grow-older gamma-change]
  tick
end

to communicate-via [ algorithm ]
  ifelse (algorithm = "threshold")
  [ listen-threshold ]
  [ ifelse (algorithm = "individual")
    [ listen-individual ]
    [ if (algorithm = "reward")
      [ speak
        ask link-neighbors
        [ listen [spoken-state] of myself [cohort] of myself ]
      ]
    ]
  ]
end

to listen-threshold
  let grammar-one-sum sum [state] of link-neighbors
  ifelse grammar-one-sum >= (count link-neighbors * threshold-val)
  [ set state 1 ]
  [ if not sink-state-1? [ set state 0 ] ]
end

to listen-individual
  set state [state] of one-of link-neighbors
end

to speak
  if logistic?
  [ let gain (alpha + 0.1) * 20
    let filter-val 1 / (1 + exp (- (gain * state - 1) * 5))
    ifelse random-float 1.0 <= filter-val
    [ set spoken-state 1 ]
    [ set spoken-state 0 ]
  ]
  if not logistic?
  [ let biased-val 1.5 * state
    if biased-val >= 1 [ set biased-val 1 ]
    ifelse random-float 1.0 <= biased-val
    [ set spoken-state 1 ]
    [ set spoken-state 0 ]
  ]
end

to listen [heard-state heard-cohort]
  let listen-chance 1
  if heard-cohort != cohort
  [ if random-float 1.0 >= chance-listen-to-outgroup
    [ set listen-chance 0]
  ]

  if listen-chance = 1
  [
    ifelse random-float 1.0 <= state
    [
      ifelse heard-state = 1
      [ set state state + (gamma * (1 - state)) ]
      [ set state (1 - gamma) * state ]
    ]
    [
      ifelse heard-state = 0
      [ set state state * (1 - gamma) ]
      [ set state gamma + state * (1 - gamma) ]
    ]
  ]
end

to-report find-partner
  let pick random-float sum [count link-neighbors] of (turtles with [any? link-neighbors])
  let partner nobody
  ask turtles
  [
    if partner = nobody
    [ ifelse count link-neighbors > pick
      [ set partner self]
      [ set pick pick - (count link-neighbors)]
    ]
  ]
  report partner
end

to layout
  layout-spring (turtles with [any? link-neighbors]) links 1 6 1
end

;to highlight
;  ifelse mouse-inside?
;    [ do-highlight ]
;    [ undo-highlight ]
;  display
;end
;
;to undo-highlight
;  clear-output
;  ask turtles [ update-color ]
;  ask links [ set color white ]
;end
;
;to do-highlight
;  let highlight-color blue
;  let min-d min [distancexy mouse-xcor mouse-ycor] of turtles
;  let the-turtle one-of turtles with
;  [any? link-neighbors and distancexy mouse-xcor mouse-ycor = min-d]
;  let highlighted-turtle one-of turtles with [color = highlight-color]
;  if the-turtle != nobody and the-turtle != highlighted-turtle
;  [
;    ask the-turtle
;    [ undo-highlight
;      output-print word "original grammar state: "  orig-grammar-string
;      output-print word "current grammar state: " precision state 5
;      set color highlight-color
;      ask my-links [ set color cyan - 1 ]
;      ask link-neighbors [ set color blue + 1 ]
;    ]
;  ]
;end
@#$#@#$#@
GRAPHICS-WINDOW
375
10
747
383
-1
-1
4.0
1
10
1
1
1
0
0
0
1
-45
45
-45
45
1
1
1
ticks
30.0

BUTTON
10
10
105
43
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
226
10
366
44
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
110
10
205
43
layout
layout\ndisplay
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
10
160
203
194
reset states
reset-turtles
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
10
45
205
78
num-turtles
num-turtles
0
500
360.0
10
1
NIL
HORIZONTAL

SLIDER
10
81
204
114
percent-grammar-1
percent-grammar-1
0
100
50.0
10
1
%
HORIZONTAL

CHOOSER
225
155
365
200
update-algorithm
update-algorithm
"individual" "threshold" "reward"
2

SLIDER
225
320
365
353
alpha
alpha
0
0.05
0.0
0.0050
1
NIL
HORIZONTAL

BUTTON
10
120
204
155
redistribute grammars
redistribute-grammars
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
225
205
365
238
threshold-val
threshold-val
0
1
0.3
0.05
1
NIL
HORIZONTAL

SWITCH
225
245
365
278
sink-state-1?
sink-state-1?
1
1
-1000

SWITCH
225
284
365
317
logistic?
logistic?
0
1
-1000

TEXTBOX
15
205
211
356
* threshold-val and sink state only\napply for \"individual\" and \"threshold\"\nupdating algorithms\n\n* when logistic? is off, there's a\nbuilt-in bias towards grammar 1\n\n* alpha only applies to the\n\"reward\" updating algorithm\nand when logistic? is on; \nbias in favor of grammar 1.
10
0.0
0

BUTTON
226
45
366
79
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

TEXTBOX
5
440
113
458
Additions: GAMMA\n
10
0.0
1

SLIDER
0
405
173
438
simulation-limit
simulation-limit
0
1000
500.0
100
1
NIL
HORIZONTAL

MONITOR
1235
260
1305
305
Mean state
mean [state] of turtles
3
1
11

SLIDER
0
455
173
488
initial-gamma
initial-gamma
0.01
0.04
0.01
0.01
1
NIL
HORIZONTAL

TEXTBOX
180
460
288
494
* Learning rate for reward algorithm
10
0.0
1

PLOT
1065
135
1230
255
Age distribution
NIL
NIL
0.0
1350.0
0.0
10.0
true
false
"" ""
PENS
"default" 25.0 1 -16777216 true "" "histogram [age] of turtles"

SWITCH
0
490
176
523
deterministic-gamma?
deterministic-gamma?
1
1
-1000

SLIDER
0
525
176
558
gamma-sd
gamma-sd
0
0.05
0.01
0.01
1
NIL
HORIZONTAL

PLOT
1065
10
1230
130
Gamma distribution
NIL
NIL
0.0
0.1
0.0
10.0
true
false
"" ""
PENS
"default" 0.001 1 -16777216 true "" "histogram [gamma] of turtles"

MONITOR
1235
10
1305
55
Mean g.
mean [gamma] of turtles
3
1
11

TEXTBOX
181
534
284
558
* SD for gamma if not determinstic.\n
10
0.0
1

TEXTBOX
5
375
248
400
Default settings for original parameters:\nreward, logistic, alpha = 0.000\n
10
0.0
1

TEXTBOX
180
491
295
542
* if not deterministic,\nInitial-gamma serves as mean
10
0.0
1

SLIDER
450
410
600
443
percent-cohort2
percent-cohort2
0
100
50.0
10
1
%
HORIZONTAL

TEXTBOX
462
396
612
415
Additions: AGE
10
0.0
1

SWITCH
450
445
600
478
deterministic-age?
deterministic-age?
1
1
-1000

SLIDER
495
480
600
513
cohort1-age
cohort1-age
0
700
100.0
50
1
NIL
HORIZONTAL

SLIDER
495
515
600
548
cohort2-age
cohort2-age
0
700
0.0
50
1
NIL
HORIZONTAL

SLIDER
495
550
600
583
age-sd
age-sd
0
50
40.0
10
1
NIL
HORIZONTAL

MONITOR
1235
185
1300
230
NIL
max-age
3
1
11

SWITCH
215
400
440
433
initial-gamma-based-on-age?
initial-gamma-based-on-age?
1
1
-1000

MONITOR
1235
135
1302
180
Mean age
mean [age] of turtles
3
1
11

CHOOSER
325
470
440
515
perseverance
perseverance
0.25 0.5 1 2 3
2

SLIDER
605
515
795
548
chance-listen-to-outgroup
chance-listen-to-outgroup
0.1
1
1.0
0.1
1
NIL
HORIZONTAL

SWITCH
605
410
795
443
cohort-based-grammar
cohort-based-grammar
1
1
-1000

SLIDER
605
445
795
478
C1-percent-grammar-1
C1-percent-grammar-1
0
100
50.0
10
1
%
HORIZONTAL

SLIDER
605
480
795
513
C2-percent-grammar-1
C2-percent-grammar-1
0
100
50.0
10
1
%
HORIZONTAL

PLOT
750
10
1060
200
Cohort-based states
Time
State
0.0
100.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" "plot cohort1-state"
"pen-1" 1.0 0 -13345367 true "" "plot cohort2-state"
"pen-2" 5.0 2 -5987164 true "" "if ((ticks mod 5) = 0) [ plot mean [state] of turtles ] "

PLOT
1065
260
1230
380
State distribution
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 true "" "histogram [state] of turtles"

PLOT
750
205
1060
380
State by age
Age
State
0.0
10.0
0.0
1.0
true
false
"" "clear-plot"
PENS
"default" 1.0 2 -13345367 true "" "ask turtles with [cohort = 1] [plotxy age state ]"
"pen-1" 1.0 2 -2674135 true "" "ask turtles with [cohort = 2] [plotxy age state ]"

TEXTBOX
5
360
375
378
----------------------------------------------------------
11
0.0
1

TEXTBOX
255
385
405
403
Additions: GAMMA-BY-AGE
11
0.0
1

TEXTBOX
605
395
890
421
Additions: COHORTS (1 as square, 2 as triangle)
11
0.0
1

SWITCH
800
410
920
443
delay-cohort
delay-cohort
1
1
-1000

SLIDER
800
445
920
478
delay-period
delay-period
0
200
100.0
10
1
NIL
HORIZONTAL

SWITCH
800
505
950
538
random-init-state
random-init-state
0
1
-1000

TEXTBOX
805
540
955
581
If random-init-state, it will\noverride any other starting\npercent grammar settings
11
0.0
1

SLIDER
320
435
440
468
gamma-change
gamma-change
-1
1
0.0
1
1
NIL
HORIZONTAL

TEXTBOX
300
515
495
585
* Gamma-change determines whether gamma changes with age, and the directionality\n* Perseverance determines the shape/slope
11
0.0
1

CHOOSER
980
405
1118
450
network-type
network-type
"random" "small-world" "preferential"
2

CHOOSER
980
455
1118
500
layout-type
layout-type
"spring"
0

BUTTON
225
80
365
113
Clear
clear-all ca
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
980
505
1120
538
random-link-prob
random-link-prob
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
980
545
1120
578
min-degree
min-degree
1
5
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
1125
515
1275
531
* Random network only
11
0.0
1

TEXTBOX
1125
550
1275
568
* Preferential network only
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model explores how the properties of language users and the structure of their social networks can affect the course of language change.

In this model, there are two linguistic variants in competition within the social network -- one variant generated by grammar 0 and the other generated by grammar 1. Language users interact with each other based on whom they are connected to in the network. At each iteration, each individual speaks by passing an utterance using either grammar 0 or grammar 1 to the neighbors in the network. Individuals then listen to their neighbors and change their grammars based on what they heard.

## HOW IT WORKS

The networks in this model are constructed through the process of "preferential attachment" in which individuals enter the network one by one, and prefer to connect to those language users who already have many connections. This leads to the emergence of a few "hubs", or language users who are very well connected; most other language users have very few connections.

There are three different options to control how language users listen and learn from their neighbors, listed in the UPDATE-ALGORITHM chooser. For two of these options, INDIVIDUAL and THRESHOLD, language users can only access one grammar at a time. Those that can only access grammar 1 are white in color, and those that can access only grammar 0 are black. For the third option, REWARD, each grammar is associated with a weight, which determines the language user's probability of accessing that grammar. Because there are only two grammars in competition here, the weights are represented with a single value - the weight of grammar 1. The color of the nodes reflect this probability; the larger the weight of grammar 1, the lighter the node.

- INDIVIDUAL: Language users choose one of their neighbors randomly, and adopt that neighbor's grammar.

- THRESHOLD: Language users adopt grammar 1 if some proportion of their neighbors is already using grammar 1. This proportion is set with the THRESHOLD-VAL slider. For example, if THRESHOLD-VAL is 0.30, then an individual will adopt grammar 1 if at least 30% of his neighbors have grammar 1.

- REWARD: Language users update their probability of using one grammar or the other. In this algorithm, if an individual hears an utterance from grammar 1, the individual's weight of grammar 1 is increased, and they will be more likely to use that grammar in the next iteration. Similarly, hearing an utterance from grammar 0 increases the likelihood of using grammar 0 in the next iteration.

## HOW TO USE IT

The NUM-NODES slider determines the number of nodes (or individuals) to be included in the network population. PERCENT-GRAMMAR-1 determines the proportion of these language learners who will be initialized to use grammar 1. The remaining nodes will be initialized to use grammar 0.

Press SETUP-EVERYTHING to generate a new network based on NUM-NODES and PERCENT-GRAMMAR-1.

Press GO ONCE to allow all language users to "speak" and "listen" only once, according to the algorithm in the UPDATE-ALGORITHM dropdown menu (see the above section for more about these options). Press GO for the simulation to run continuously; pressing GO again will halt the simulation.

Press LAYOUT to move the nodes around so that the structure of the network easier to see.

When the HIGHLIGHT button is pressed, rolling over a node in the network will highlight the nodes to which it is connected. Additionally, the node's initial and current grammar state will be displayed in the output area.

Press REDISTRIBUTE-GRAMMARS to reassign grammars to all language users, under the same initial condition. For example, if 20% of the nodes were initialized with grammar 1, pressing REDISTRIBUTE-GRAMMARS will assign grammar 1 to a new sample of 20% of the population.

Press RESET-STATES to reinitialize all language users to their original grammars. This allows you to run the model multiple times without generating a new network structure.

The SINK-STATE-1? switch applies only for the INDIVIDUAL and THRESHOLD updating algorithms. If on, once an individual adopts grammar 1, then he can never go back to grammar 0.

The LOGISTIC? switch applies only for the REWARD updating algorithm. If on, an individual's probability of using one of the grammars is pushed to the extremes (closer to 0% or 100%), based on the output of the logistic function. For more details, see https://en.wikipedia.org/wiki/Logistic_function.

The ALPHA slider also applies only for the REWARD updating algorithm, and only when LOGISTIC? is turned on. ALPHA represents a bias in favor of grammar 1. Probabilities are pushed to the extremes, and shifted toward selecting grammar 1. The larger the value of ALPHA, the more likely a language user will speak using grammar 1.

The plot "Mean state of language users in the network" calculates the average weight of grammar 1 for all nodes in the network, at each iteration.

## THINGS TO NOTICE

Over time, language users tend to arrive at using just one grammar all of the time. However, they may not all converge to the same grammar. It is possible for sub-groups to emerge, which may be seen as the formation of different dialects.

## THINGS TO TRY

Under what conditions is it possible to get one grammar to spread through the entire network? Try manipulating PERCENT-GRAMMAR-1, the updating algorithm, and the various other parameters. Does the number of nodes matter?

## EXTENDING THE MODEL

Whether or not two language users interact with each other is determined by the network structure. How would the model behave if language users were connected by a small-world network rather than a preferential attachment network?

In this model, only two grammars are in competition in the network. Try extending the model to allow competition between three grammars.

The updating algorithm currently has agents updating asynchronously. Currently, the grammar may spread one step or several within one tick, depending on the links. Try implementing synchronous updating.

Regardless of the updating algorithm, language users always start out using one grammar categorically (that is, with a weight of 0 or 1). Edit the model to allow some language users to be initialized to an intermediate weight (e.g., 0.5).

## NETLOGO FEATURES

Networks are represented using turtles (nodes) and links.  In NetLogo, both turtles and links are agents.

## RELATED MODELS

Preferential Attachment

## CREDITS AND REFERENCES

This model was also described in Troutman, Celina; Clark, Brady; and Goldrick, Matthew (2008) "Social networks and intraspeaker variation during periods of language change," University of Pennsylvania Working Papers in Linguistics: Vol. 14: Issue 1, Article 25.
https://repository.upenn.edu/pwpl/vol14/iss1/25/

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Troutman, C. and Wilensky, U. (2007).  NetLogo Language Change model.  http://ccl.northwestern.edu/netlogo/models/LanguageChange.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2007 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

<!-- 2007 Cite: Troutman, C. -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Varying Gamma Alpha Percent" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [state] of nodes</metric>
    <enumeratedValueSet variable="alpha">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sink-state-1?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-val">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="logistic?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;reward&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-1">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="45"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-limit">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.04"/>
      <value value="0.05"/>
      <value value="0.06"/>
      <value value="0.07"/>
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-gamma?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-sd">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma_____with-age">
      <value value="&quot;is-constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-cost">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma-based-on-age?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort1-age">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-sd">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-cohort2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort2-age">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-listen-to-outgroup">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort-based-grammar">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C1-percent-grammar-1">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C2-percent-grammar-1">
      <value value="80"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="half-split-age" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>mean [state] of nodes</metric>
    <enumeratedValueSet variable="simulation-limit">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-sd">
      <value value="0"/>
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma-based-on-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sink-state-1?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-val">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="logistic?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma_____with-age">
      <value value="&quot;increases&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-sd">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group2-age">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;reward&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group1-age">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-cost">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-1">
      <value value="20"/>
      <value value="40"/>
      <value value="45"/>
      <value value="50"/>
      <value value="60"/>
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-gamma?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-group2">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-age?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="probabilistic-gamma" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [state] of nodes</metric>
    <enumeratedValueSet variable="alpha">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sink-state-1?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-val">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="logistic?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;reward&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-1">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="45"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-limit">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.04"/>
      <value value="0.05"/>
      <value value="0.06"/>
      <value value="0.07"/>
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-gamma?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-sd">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma_____with-age">
      <value value="&quot;is-constant&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perseverance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma-based-on-age?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort1-age">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-sd">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-cohort2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort2-age">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-listen-to-outgroup">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort-based-grammar">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C1-percent-grammar-1">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C2-percent-grammar-1">
      <value value="80"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="gamma-based-on-age" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [state] of nodes</metric>
    <metric>mean [age] of nodes</metric>
    <enumeratedValueSet variable="alpha">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sink-state-1?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-val">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="logistic?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;reward&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-1">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="45"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-limit">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-gamma?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-sd">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma_____with-age">
      <value value="&quot;decreases&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perseverance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma-based-on-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-age?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort1-age">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-sd">
      <value value="30"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-cohort2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort2-age">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-listen-to-outgroup">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort-based-grammar">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C1-percent-grammar-1">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C2-percent-grammar-1">
      <value value="80"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary-gamma-perseverance" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [state] of nodes</metric>
    <enumeratedValueSet variable="alpha">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sink-state-1?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-val">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="logistic?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;reward&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-1">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="45"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-limit">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-gamma?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-sd">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma_____with-age">
      <value value="&quot;decreases&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perseverance">
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma-based-on-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort1-age">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-sd">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-cohort2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort2-age">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-listen-to-outgroup">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort-based-grammar">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C1-percent-grammar-1">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C2-percent-grammar-1">
      <value value="80"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="two-groups" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [state] of nodes</metric>
    <metric>cohort1-state</metric>
    <metric>cohort2-state</metric>
    <enumeratedValueSet variable="alpha">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sink-state-1?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-val">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="logistic?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;reward&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-1">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="45"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-limit">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-gamma?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-sd">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma_____with-age">
      <value value="&quot;decreases&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perseverance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma-based-on-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort1-age">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-sd">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-cohort2">
      <value value="10"/>
      <value value="20"/>
      <value value="35"/>
      <value value="50"/>
      <value value="65"/>
      <value value="75"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort2-age">
      <value value="50"/>
      <value value="150"/>
      <value value="300"/>
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-listen-to-outgroup">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort-based-grammar">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C1-percent-grammar-1">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C2-percent-grammar-1">
      <value value="80"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary-group-starting-percentage" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>mean [state] of nodes</metric>
    <metric>cohort1-state</metric>
    <metric>cohort2-state</metric>
    <enumeratedValueSet variable="alpha">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sink-state-1?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-val">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="logistic?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;reward&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-1">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-limit">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-gamma?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-sd">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma_____with-age">
      <value value="&quot;decreases&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perseverance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma-based-on-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort1-age">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-sd">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-cohort2">
      <value value="5"/>
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort2-age">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="chance-listen-to-outgroup">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort-based-grammar">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C1-percent-grammar-1">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C2-percent-grammar-1">
      <value value="10"/>
      <value value="20"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="group-preference" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [state] of nodes</metric>
    <metric>cohort1-state</metric>
    <metric>cohort2-state</metric>
    <enumeratedValueSet variable="alpha">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sink-state-1?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold-val">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="logistic?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="update-algorithm">
      <value value="&quot;reward&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-grammar-1">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="45"/>
      <value value="50"/>
      <value value="55"/>
      <value value="60"/>
      <value value="65"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-limit">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-gamma?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-sd">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma_____with-age">
      <value value="&quot;decreases&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perseverance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-gamma-based-on-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deterministic-age?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort1-age">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-sd">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-cohort2">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cohort2-age">
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="chance-listen-to-outgroup" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="cohort-based-grammar">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C1-percent-grammar-1">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C2-percent-grammar-1">
      <value value="80"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
