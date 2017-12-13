; NOTES:
; Instalation notes:
;      pr-utils is a custom extension that must be present in the Netlogo extensions folder in a directory of the same name.
;      A folder called "results" has to be present in the same location of the main netlogo file. This directory is used to store detailed results of each simulation.
;      Every simulation execution rewrites the content of the "results" folder

; The code blocks marked as ------ PR ------ are exectued only in the Peer review simulation
; The code blocks marked as ------ REP ----- are executed only in the Reputation simulation
; The code blocks marked as ------ PR-REP -- are executed in both simulations, Peer review and Reputation


; primary entities
breed [scientists scientist]
scientists-own[role quality productivity tmp-prod initial-reputation reputation id age]
breed [papers paper]
papers-own [quality published? inherited-reputation reputation age curr-pr-reads curr-rep-reads tot-pr-reads tot-rep-reads mistaken?]
breed [journals journal]
journals-own [editor quality-average quality]

; support-connection entities
directed-link-breed [read-prs read-pr]
directed-link-breed [read-reps read-rep]
read-reps-own [score]
breed [drafts draft]
drafts-own [quality]
breed [obsoletes obsolete]
obsoletes-own [quality published? inherited-reputation reputation age  curr-pr-reads curr-rep-reads tot-pr-reads tot-rep-reads]
undirected-link-breed [paper-to-authors paper-to-author]
undirected-link-breed [reviewers reviewer]
directed-link-breed [they-appear-in appears-in]

; NOTE: pr-utils is a custom extension that must be present in the Netlogo extensions folder in a directory of the same name.
extensions [pr-utils rnd stats csv]

globals [
  types
  version
  topics
  itm-paper
  itm-scientist
  papers-plot-following
  journals-list
  checking-zipf
  checking-exp
  scientists-correlation
  papers-correlation
]


;-----------SETUP---------------------------------------------------------------------------------------------


to setup
  ; Initializes the simulation

  clear-all
  reset-ticks
  set version "07"
  set itm-paper 10
  set itm-scientist 5
  ifelse random-seed? [
    random-seed random 1000000
    let zero pr-utils:zipfGenInit random 1000000 initial-Zipf-exponent]
  [
    random-seed 1432
    let uno pr-utils:zipfGenInit 1432 initial-Zipf-exponent
  ]
  if behaviorspace-experiment-name = "" [plot-zipf-and-exp-just-because]

  create-scientists initial-scientists [setup-scientist set shape "circle"]
  if simulationType = "PR" or  simulationType = "both" [
    setup-journals
  ]
  set papers-plot-following no-turtles
end

to plot-zipf-and-exp-just-because
  let how-many 500
  let current-type   excellence-criterion
  set excellence-criterion "exponentialrandom"
  set checking-exp  n-values how-many [log generate-quality 10]
  set excellence-criterion "zipf"
  set checking-zipf  n-values how-many [log generate-quality 10]
  set excellence-criterion current-type

  set-current-plot "comparing generate-qualities"
  set-plot-x-range 0 max sentence checking-zipf checking-exp
  set-current-plot-pen"zipf" histogram checking-zipf
  set-current-plot-pen "exp" histogram checking-exp
end

to setup-scientist
  ; Initialize a scientist

  set role "author"
  set productivity random-exponential production-mean
  set quality generate-quality
  set initial-reputation generate-quality
  set reputation initial-reputation
  set age 1
  if count journals > 0 [create-reviewer-with one-of journals]
end


;----- PR -------------------

to setup-journals
  ; Creates the journals where the scientists can publish their papers

  create-journals n-of-journals [
    set shape "triangle"
    set editor one-of scientists
    ask n-of (2 * num-reviewers) scientists [create-reviewer-with myself]
  ]
  set journals-list sort journals
  let random-sample sort n-values (n-of-journals * 20) [generate-quality]
  adapt-journal-quality random-sample
end

to adapt-journal-quality [sorted_pqualities]
  ; Stablishes the journal qualities
  ; The space of existing journal qualities is divided in n bins with the same number of papers inside (approximatively)

  let index 0;
  foreach  journals-list [
    ask ? [set quality item floor ((index + 1 / 2) * (length sorted_pqualities / n-of-journals) ) sorted_pqualities]
    set index index + 1
  ]
end

;----- REP ------------------

to-report weight-inherited-vs-eval [inherit itm-limit evals]
  ; Calculates the reputation of a paper based on the evaluations it has received and the reputation coming from its authors

  let num-eval length evals
  let eval-weight -1 ; weights evaluations vs. inherited
  ifelse num-eval = 0 [
    report inherit  ; if there are no evaluations of that paper we use inherited-reputation only
  ]
  [
    if-else num-eval < itm-limit [ ; if there are less than 'itm-paper' evaluations of that paper we use inherited-reputation and the evaluations
      set eval-weight sin((pi * num-eval) / (2 * itm-limit))
    ]
    [ ; if there are more than 'itm-paper' evaluations we use only the info from the evaluations
      set eval-weight 1
    ]
    let evalCalc mean evals; (sum evals) / (length evals)
    report (inherit * (1 - eval-weight) + evalCalc * eval-weight)
  ]
end

to-report evaluations
  let scores [score] of my-in-read-reps
  report ifelse-value (length scores > 0) [reduce sentence [score] of my-in-read-reps][[]]
end

to-report evaluators
  report in-read-rep-neighbors
end


;----- PR-REP -----------------
to go
  ; Simulation step

  generate-papers ; papers are generated as drafts.

  if (ticks mod 5 = 0 ) [save-distributions]

  if (ticks = 0 ) [set papers-plot-following (turtle-set one-of drafts)  ;
    set-current-plot "papers read per turn - red PR blue REP"
    create-temporary-plot-pen word [who] of one-of papers-plot-following "pr" set-plot-pen-color red
    create-temporary-plot-pen word [who] of one-of papers-plot-following "rep"  set-plot-pen-color blue
  ]
  if (ticks = 20)  [
    let new-followed one-of  papers with [ age = 1] ; max-one-of papers with [ age <= 2] [quality]
    set papers-plot-following turtle-set new-followed
    set-current-plot "papers read per turn - red PR blue REP"
    create-temporary-plot-pen word [who] of new-followed "pr" set-plot-pen-color red
    create-temporary-plot-pen word [who] of new-followed "rep"  set-plot-pen-color blue
  ]
  ask papers [set curr-pr-reads 0 set curr-rep-reads 0]

  if simulationType = "PR" or simulationType = "both" [
    review-drafts ; transforms drafts into papers or into unpublished things. Drafts are empty at this point, all of them changed into papers.
    ask scientists [read-and-think-pr] ; Papers are read but not evaluated.
    adapt-journal-quality sort [quality] of papers
  ]

  if simulationType = "REP" or simulationType = "both"
  [  ; this is the REP branch of the if

     ; calculate the inherited-reputation, reputation and visibility  of all papers
    if any? drafts [ask drafts [set breed papers set published? false set age 0 set mistaken? false]] ; rep only branch, everything published. In the "both" branch will not fire.
    ask papers [

      ; inherited-reputation
      set inherited-reputation mean [reputation] of paper-to-author-neighbors

      ; reputation
      set reputation weight-inherited-vs-eval inherited-reputation itm-paper evaluations
      ;set reputation ifelse-value (empty? evaluations) [inherited-reputation][mean evaluations]
    ]
    ; Papers are evaluated.
    ask scientists [read-and-think-rep]

    ; calculate the reputation of the authors
    ask scientists [
      set reputation weight-inherited-vs-eval initial-reputation itm-scientist ([reputation] of paper-to-author-neighbors with [breed = papers ])
    ]
  ]

  ; new scientists
  ask scientists [set age age + 1]
  ask n-of (scientists-leaving) scientists [set productivity 0] ; not age related because people just change field.
  create-scientists new-scientists-per-turn [setup-scientist set shape "circle"]

  ; increase the age of the paper, update paper accumulators
  ask papers [set age age + 1 set tot-rep-reads tot-rep-reads + curr-rep-reads
    set tot-pr-reads tot-pr-reads + curr-pr-reads]

  ask papers with [age > obsolescence-threshold] [set breed obsoletes]

  calculateCorrelationsScientists
  calculateCorrelationsPapers


  if behaviorspace-experiment-name = "" [do-plots]
  tick
  print word "Tick n. " ticks
end

;-----PR-------------------

to-report reading-weight-pr ;self=paper
  report ([quality] of one-of out-appears-in-neighbors) * ifelse-value (age-weight?) [age-weight age][1] ; it's the journal quality - only one appears-in
end

;-----REP-------------------

to-report reading-weight-rep ;self=paper
    let r reputation * ifelse-value (age-weight?) [age-weight age][1]
    report ifelse-value (r > 0) [r][0]
end

;------PR-REP----------------

to-report age-weight [the-age] ; this is justfied by the fact that after three years novelty wears off.
  ifelse (the-age <= 2) [report item (the-age) [0.25 0.6 1]] [ ; first three years of paper life, 0 1 and 2
    report ( 1  / exp ((the-age - 2) / 5) )] ; after five years it gets 50%
end

;-----REP-------------------

to read-and-think-rep ;self=scientist
  let not-yet-read no-turtles
  ifelse (read-cap?) [
    let already-read out-read-rep-neighbors
    set not-yet-read papers with [not member? self already-read]
  ]
  [
    set not-yet-read papers
  ]
  let num-to-read min list (productivity *  productivity-to-reads-multiplier) count not-yet-read
  let to-read rnd:weighted-n-of num-to-read not-yet-read [ reading-weight-rep ]
  ask to-read [
    let paper-evaluation evaluate-paper-forREP myself self; myself = the_evaluator, self = the_paper
    if not in-read-rep-neighbor? myself [create-read-rep-from myself [set score []]]
    ask in-read-rep-from myself [set score fput  paper-evaluation score ]
    set curr-rep-reads curr-rep-reads + 1
  ]
end

;-----PR-------------------

to read-and-think-pr ;self=scientist
  let not-yet-read no-turtles
  ifelse (read-cap?) [
    let already-read out-read-pr-neighbors
    set not-yet-read papers with [published? and not member? self already-read]
  ]
  [
    set not-yet-read papers with [published?]
  ]
  let num-to-read min list (productivity *  productivity-to-reads-multiplier) count not-yet-read
  let to-read rnd:weighted-n-of num-to-read not-yet-read [ reading-weight-pr ]
  ask to-read [
    if not in-read-pr-neighbor? myself [create-read-pr-from myself]
    set curr-pr-reads curr-pr-reads + 1
  ]
end

;-----REP-------------------

 to-report evaluate-paper-forREP [evaluator a-paper]
    report ([quality] of a-paper  + (ifelse-value(random-float 1.0 > 0.5) [-1][1] * generate-quality * generate-quality / quality))
 end

;-----PR--------------------

to-report evaluate-paper-forPR [a-paper journal-quality] ; context: the scientist who evaluate.
  report ifelse-value  ([quality] of a-paper > (journal-quality + (ifelse-value(random-float 1 > 0.5) [-1][1] * generate-quality * generate-quality  / quality) )) ["yes"]["no"]
end



;-----PR--------------------

to review-drafts
  ; Simulates the review of a paper and decides about publication or rejection

  ask drafts [
    if-else strategic-submission? [ ;one of the authors chooses the journal. Which one? He chooses a journal that is the nearest to his quality. You can assume it was a resubmission if you go for a journal of lower quality.
     let the-journal [
        choose-journal
        ] of one-of paper-to-author-neighbors
      ask the-journal [review myself]
    ][
    ask one-of journals [review myself]
    ]
  ]
end

to-report choose-journal
  ; An author chooses a journal near his or her quality

  let theJ min-one-of journals [abs (quality + ifelse-value(random-float 1 > 0.5)[-1][1] * generate-quality - [quality] of myself)]
report theJ
end


to review [a-paper] ; self=journal
  let revs n-of num-reviewers reviewer-neighbors  ; people could end out evaluating their own paper, but as they would not do it differently, it'ìs not a big issue.
  let evals [evaluate-paper-forPR a-paper [quality] of myself ] of revs
  if-else length filter  [? = "yes"] evals > length filter [? = "no"] evals [
    ask a-paper [set breed papers create-appears-in-to myself
      set age 0
      set published? true
      set mistaken? ifelse-value (quality < [quality] of myself)  [true][false]
    ] ; published, yay
  ]
  [
    ask a-paper [set breed papers set age 0 set published? false set mistaken? false
      set mistaken? ifelse-value (quality >= [quality] of myself)  [true][false]
    ] ; rejected, groan.
  ]
end

;-----PR-REP----------------

to generate-papers
  ; Generates the (draft) papers according to the total production of the active scientists.
  ; Each paper is assigned a set of co-authors and then assigned an initial quality. Here we
  ; also generate the exceptionally good papers that appear in the system from time to time.

  let production-total floor sum [productivity] of scientists
  ; First we create all the drafts with random quality
  create-drafts floor production-total / average-authors-per-paper  [
    set quality generate-quality   ; The quality of the paper starts indipendently of the quality of the authors (heck the authors aren't even yet assigned)
  ]
  if (artificial-big-paper? and (ticks mod time-artificial-big-paper) = 0) and ticks > 0 [
    ask one-of drafts [set quality 10 * max [quality] of papers
      print (word "Exceptional paper n. " who " with quality: " quality)
    ]
  ]
  if-else keep-co-workers [create-co-authors-on-network] [create-co-authors-randomly]
  ; At this point the paper has the authors assigned.
  ; Next, average the quality between paper and authors.
  ask drafts [
    let num-of-authors count paper-to-author-neighbors ;with [quality != -1]
    set quality (quality + sum [quality] of paper-to-author-neighbors) / (num-of-authors + 1)  ; In this averaging the paper - which is, the idea - counts exactly as the quality of one author.
                                                                                               ; Then authors change. They can learn something, but they can't forget something. So:
    ask paper-to-author-neighbors [ if quality < ([quality] of myself) [set quality (1 - rate-of-adaptation) * quality + rate-of-adaptation * [quality] of myself] ]
  ]
end


to create-co-authors-randomly
  ; Assigns co-authors to a paper. The co-authors are assigned randomly. A scientist can
  ; be the co-author of a paper only if he/she still has production capacity.

  ask scientists [ set tmp-prod productivity ]
  let drafts-list [ self ] of drafts with [ not any? paper-to-author-neighbors ]
  let candidates-list reduce sentence [ n-values tmp-prod [ self ] ] of scientists
  let n min (list (length drafts-list) (length candidates-list))
  set drafts-list sublist drafts-list 0 n
  (foreach (n-of n candidates-list) (drafts-list) [
    ask ?1 [
      create-paper-to-author-with ?2
      decrease-tmp-prod
    ]
  ])
  foreach drafts-list [
    let the-draft ?
    repeat (floor random-exponential average-authors-per-paper - 1) [
      let candidates scientists with [ tmp-prod > 0 ]
      if any? candidates [
        ask rnd:weighted-one-of candidates [ tmp-prod ] [
          create-paper-to-author-with the-draft
          decrease-tmp-prod
        ]
      ]
    ]
  ]
  ask drafts with [ not any? paper-to-author-neighbors ] [ die ]
end


to decrease-tmp-prod
  set tmp-prod max list 0 (tmp-prod - 1)
  if (tmp-prod < 0) [print "ERROR"]
end


to create-co-authors-on-network
  ; Assings co-authors to a paper. The co-authors are taken from the network of
  ; previous co-authors of the progressively assigned authors (with the first
  ; one assigned randomly)

  ask scientists [ set tmp-prod productivity ]
  let drafts-list [ self ] of drafts with [ not any? paper-to-author-neighbors ]

  let make-me-integer min [tmp-prod] of scientists
  let candidates-list reduce sentence [ n-values round (tmp-prod / make-me-integer) [ self ] ] of scientists

  let n min (list (length drafts-list) (length candidates-list))
  set drafts-list sublist drafts-list 0 n
  (foreach (n-of n candidates-list) (drafts-list) [
    ask ?1 [
      create-paper-to-author-with ?2
      decrease-tmp-prod
    ]
  ])
  foreach drafts-list [
    let the-draft ?
    repeat (floor random-exponential average-authors-per-paper - 1) [
      let current-authors [ paper-to-author-neighbors ] of the-draft
      let candidates (turtle-set [ coauthors ] of current-authors) with [
        tmp-prod > 0 and not member? self current-authors
      ]
      if not any? candidates [
        set candidates scientists with [ tmp-prod > 0 ]
      ]
      if any? candidates [
        ask rnd:weighted-one-of candidates [ tmp-prod ] [
          create-paper-to-author-with the-draft
          decrease-tmp-prod
        ]
      ]
    ]
  ]
  ask drafts with [ not any? paper-to-author-neighbors ] [ die ]
end


to-report coauthors
  report [ paper-to-author-neighbors ] of paper-to-author-neighbors
end


to-report generate-quality
  ; Quality generator. Generates qualities using either exponential random or a
  ; zipf distribution (as selected by the user in the user interface)

  ifelse (excellence-criterion = "exponentialrandom")[
    report random-exponential r-exp-mean + 1
  ]
  [
    ifelse (excellence-criterion = "zipf") [
      let theZ pr-utils:zipfGen
      report ifelse-value sawoff-zipf? [min (list 100 theZ)][theZ]
    ]
    [ ; must be initialized (in the setup)
      report "uh, not implemented"
    ]
  ]
end

;------------PLOTTING------------------------


to do-plots

set-current-plot "papers read per turn - red PR blue REP"

ask papers-plot-following with [breed = papers][
  set-current-plot-pen word who "pr"  plotxy ticks  curr-pr-reads
  set-current-plot-pen word who "rep"  plotxy ticks  curr-rep-reads
]
let to-replace papers-plot-following with [breed = obsoletes]
if any? to-replace [
  ask to-replace [
    ask max-one-of papers with [age <= 2] [quality] [
      set papers-plot-following (turtle-set self papers-plot-following)
      create-temporary-plot-pen word who "pr" set-plot-pen-color red
      create-temporary-plot-pen word who "rep"  set-plot-pen-color blue
    ]
  ]
  set papers-plot-following papers-plot-following with [breed != obsoletes]
]

set-current-plot "readings by paper quality (blue: rep, red:PR)"
clear-plot

if simulationType = "REP"  [ set-plot-pen-color blue  ask papers [plotxy quality count evaluators] ]
if simulationType = "PR"  [set-plot-pen-color red  ask papers with [published?] [ plotxy quality count in-read-pr-neighbors] ]
if simulationType = "both"  [

set-plot-pen-color red  ask papers  with [published?]  [ plotxy quality count in-read-pr-neighbors]
set-plot-pen-color blue ask papers   [plotxy quality count evaluators]]

set-current-plot "mistakes"
plot count papers with [mistaken? and age = 1]

set-current-plot "published, mistakes"
set-current-plot-pen "mistakes"
plot count papers with [mistaken?]
set-current-plot-pen "published"
plot count papers with [published?]

set-current-plot "readings by paper quality (blue: rep, red:PR)"
clear-plot
if simulationType = "REP"  [ set-plot-pen-color blue  ask papers [plotxy quality count evaluators] ]
if simulationType = "PR"  [set-plot-pen-color red  ask papers with [published?] [ plotxy quality count in-read-pr-neighbors] ]
if simulationType = "both"  [
  set-plot-pen-color red  ask papers  with [published?]  [ plotxy quality count in-read-pr-neighbors]
  set-plot-pen-color blue ask papers   [plotxy quality count evaluators]]

set-current-plot "Obsoletes"
clear-plot
if simulationType = "REP"  [ set-plot-pen-color blue  ask obsoletes [plotxy quality count evaluators] ]
if simulationType = "PR"  [set-plot-pen-color red  ask obsoletes with [published?] [ plotxy quality count in-read-pr-neighbors] ]
if simulationType = "both"  [
  set-plot-pen-color red  ask obsoletes  with [published?]  [ plotxy quality count in-read-pr-neighbors]
  set-plot-pen-color blue ask obsoletes   [plotxy quality count evaluators]]

set-current-plot "readings by author quality"
clear-plot
if simulationType = "REP"  [set-plot-pen-color blue  ask scientists [plotxy quality sum [count evaluators] of paper-to-author-neighbors with [breed = papers] ] ]
if simulationType = "PR"  [set-plot-pen-color red  ask scientists [plotxy quality sum [count in-read-pr-neighbors] of paper-to-author-neighbors with [breed = papers] ] ]
if simulationType = "both"  [
  set-plot-pen-color blue  ask scientists [plotxy quality sum [count evaluators] of paper-to-author-neighbors with [breed = papers] ]
  set-plot-pen-color red  ask scientists [plotxy quality sum [count in-read-pr-neighbors] of paper-to-author-neighbors with [breed = papers] ]
 ]

set-current-plot "quality distribution authors"
set-plot-x-range 0 max  [quality] of scientists + 1
histogram [quality] of scientists

set-current-plot "reputation"
set-plot-x-range 0 max  [reputation] of papers + 1
histogram [reputation] of papers

set-current-plot   "quality of papers"
set-plot-x-range 0 max  [quality] of papers + 1
    histogram [quality] of papers

set-current-plot   "quality of published papers"
set-plot-x-range 0 max  [quality] of papers + 1
   histogram [quality] of papers with [published?]


set-current-plot   "quality of papers (log)"
set-plot-x-range 0 max  [log quality 10] of papers + 1
  histogram [log quality 10] of papers

set-current-plot "quality-by-productivity"
ask scientists [plotxy productivity quality]

set-current-plot "authors-per-paper"
set-plot-x-range 0 max  [count paper-to-author-neighbors] of papers + 1
histogram [count paper-to-author-neighbors] of papers

set-current-plot "quality of journals"
set-plot-x-range 0 max  [quality] of journals + 1
let quality-j-list []
ask papers [
  let j-quality [quality] of out-appears-in-neighbors
  if length j-quality > 0 [
    set quality-j-list fput item 0 j-quality quality-j-list
  ]
]
histogram quality-j-list

end


;------------REPORTING------------------------



to-report filename
  let expname ifelse-value (behaviorspace-experiment-name = "") [""][(word behaviorspace-experiment-name "-" behaviorspace-run-number "-")]
  report  (word "./results/" expname version "-" simulationType "-" excellence-criterion "."  "-z" initial-Zipf-exponent "-e" r-exp-mean
    ".ns." initial-scientists "+" new-scientists-per-turn "-" scientists-leaving
     "-aw." age-weight? "-kc." keep-co-workers "-nr." num-reviewers "-pm." production-mean )
end

to happy-ending
  let the-name filename
  export-world (word the-name "-world.csv")
end

to save-distributions
  save-authors
  save-papers
  save-journals
end

to save-papers
  let the-breed papers
  let variables ["behaviorspace-run-number" "ticks" "who" "count evaluators" "reputation" "quality" "age"]
  let names ["runnumber" "ticks" "ID"  "reads"  "reputation" "quality" "age"]
  save-stuff-2 the-breed variables names
  end

to save-journals
  let the-breed journals
  let variables ["behaviorspace-run-number" "ticks" "who" "count in-appears-in-neighbors" "quality" ]
  let names ["runnumber" "ticks" "ID" "reads"  "quality" ]
  save-stuff-2 the-breed variables names
end

to save-authors
  let the-breed scientists
  let variables ["behaviorspace-run-number" "ticks" "who" "sum [count evaluators] of paper-to-author-neighbors with [breed = papers]" "count paper-to-author-neighbors" "reputation" "quality" "productivity" "age"]
  let names ["runnumber" "ticks" "ID" "reads" "papers" "reputation" "quality" "productivity" "age"]
 save-stuff-2 the-breed variables names
end

to save-stuff-2 [the-breed variables names]
  let the-filename (word filename "." the-breed ".csv")
  let values [ map runresult variables ] of the-breed
  file-open the-filename
  ;print values
  if not file-exists? the-filename [
    csv:to-file the-filename fput names values
  ]
  file-print csv:to-string values
file-close
end

to-report export-network-string
   let the-net ""
   ask papers [
     ask paper-to-author-neighbors [
       ask [paper-to-author-neighbors] of myself [
       set the-net (word the-net [who + 1] of myself  " " (who + 1) "\\n" )
       ]
   ]
   ]
   report the-net
end

to-report export-network-list
   let the-net []
   ask papers [
     ask paper-to-author-neighbors [
       ask [paper-to-author-neighbors] of myself [;myself being, I hope, the paper
       set the-net fput (list ( [who + 1] of myself)  (who + 1) ) the-net
       ]
   ]
   ]
   report the-net
end


to-report export-network-type
   let the-net ""
   ask papers [
     ask paper-to-author-neighbors [
       ask [paper-to-author-neighbors] of myself [;myself being the paper
       type [who + 1] of myself ; now myself is the author under consideration
       type " "
       type who + 1 ; the co-author
       print ""
       ]
   ]
   ]
end

to-report correlationMatrix [which i j]
  ifelse (ticks = 0)
  [report 0]
  [report item i  item j ifelse-value (which = "papers") [papers-correlation ] [ scientists-correlation]]
end

to-report correlationScientistQualityRep
  report correlationMatrix "scientist" 0 1
end

to-report correlationScientistQualityReadPR
  report correlationMatrix "scientist" 0 2
end

to-report correlationScientistReputationReadREP
  report correlationMatrix "scientist" 1 3
  end

to calculateCorrelationsScientists ; side effect, matrix ready
  let tbl stats:newtable
  ask Scientists [stats:add tbl (list quality reputation
      sum [count evaluators] of paper-to-author-neighbors with [breed = papers]
      sum [count in-read-pr-neighbors] of paper-to-author-neighbors with [breed = papers]  )]
  set scientists-correlation  stats:correlation tbl
end

to-report correlationPapersQualityRep
  report correlationMatrix "paper" 0 1
end

to-report correlationPapersQualityReadPR
  report correlationMatrix "paper" 0 2
end

to-report correlationPapersReputationReadREP
  report correlationMatrix "paper" 1 3
end

to calculateCorrelationsPapers ; side effect, matrix ready
  let tbl stats:newtable
  if count papers > 2 [
    ask papers [stats:add tbl (list quality reputation
      count my-in-read-prs
      count my-in-read-reps)]
  set papers-correlation  stats:correlation tbl
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
204
10
449
229
-1
-1
7.833333333333333
1
10
1
1
1
0
1
1
1
0
23
0
23
1
1
1
ticks
30.0

BUTTON
35
125
157
158
NIL
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
35
165
157
198
go
go\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1175
10
1375
160
authors-per-paper
NIL
NIL
0.0
30.0
0.0
40.0
true
true
"" ""
PENS
"authors" 1.0 1 -16777216 true "" ""

BUTTON
35
205
160
238
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
410
55
595
100
excellence-criterion
excellence-criterion
"exponentialrandom" "zipf"
1

PLOT
255
370
455
520
quality-by-productivity
NIL
NIL
0.0
10.0
0.0
150.0
true
false
"" ""
PENS
"qbp" 1.0 2 -16777216 true "" ""

PLOT
675
630
1170
780
quality of papers
NIL
NIL
0.0
300.0
0.0
10.0
true
false
"" ""
PENS
"pen-0" 1.0 1 -16777216 true "" ""

PLOT
460
370
660
520
quality distribution authors
NIL
NIL
0.0
110.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

SLIDER
410
180
595
213
average-authors-per-paper
average-authors-per-paper
0.01
10
2.275
0.01
1
NIL
HORIZONTAL

SWITCH
410
145
595
178
keep-co-workers
keep-co-workers
0
1
-1000

SLIDER
410
285
594
318
initial-Zipf-exponent
initial-Zipf-exponent
1.1
10
1.88
0.1
1
NIL
HORIZONTAL

PLOT
1175
320
1489
470
quality of papers (log)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

SLIDER
410
215
595
248
num-reviewers
num-reviewers
1
10
3
1
1
NIL
HORIZONTAL

CHOOSER
35
10
173
55
simulationType
simulationType
"PR" "REP" "both"
2

MONITOR
35
65
94
110
NIL
version
17
1
11

MONITOR
35
255
200
300
quality mean (pub)
mean [quality] of papers with [published?]
1
1
11

MONITOR
35
305
200
350
unpublished quality
mean [quality] of papers with [not published?]
1
1
11

SLIDER
410
10
595
43
new-scientists-per-turn
new-scientists-per-turn
0
100
0
1
1
NIL
HORIZONTAL

PLOT
680
165
1169
315
readings by paper quality (blue: rep, red:PR)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
675
320
1170
470
readings by author quality
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

SLIDER
410
320
595
353
production-mean
production-mean
0
20
0.07
0.001
1
NIL
HORIZONTAL

SLIDER
410
250
595
283
r-exp-mean
r-exp-mean
0
200
11.1
1
1
NIL
HORIZONTAL

SLIDER
410
110
595
143
initial-scientists
initial-scientists
0
4000
848
100
1
NIL
HORIZONTAL

MONITOR
35
355
200
400
quality mass rep
sum [quality * count evaluators] of papers + sum [quality * count evaluators] of obsoletes
0
1
11

MONITOR
35
405
200
450
quality mass pr
sum [quality * count my-in-read-prs] of papers + sum [quality * count my-in-read-prs] of obsoletes
0
1
11

PLOT
1175
165
1375
315
reputation
NIL
NIL
0.0
20.0
0.0
0.0
true
false
"" ""
PENS
"pen-0" 1.0 1 -7500403 true "" ""

SLIDER
605
45
750
78
n-of-journals
n-of-journals
1
100
36
5
1
NIL
HORIZONTAL

PLOT
970
10
1170
160
Obsoletes
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

MONITOR
35
455
200
500
rep sq q
sum [quality ^ 2 * count evaluators] of papers + sum [quality  ^ 2 * count evaluators] of obsoletes
0
1
11

MONITOR
35
505
200
550
pr sq q
sum [quality ^ 2 * count my-in-read-prs] of papers + sum [quality ^ 2 * count my-in-read-prs] of obsoletes
0
1
11

PLOT
765
10
965
160
mistakes
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
675
475
1169
625
published, mistakes
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"mistakes" 1.0 2 -16777216 true "" ""
"published" 1.0 0 -13840069 true "" ""

PLOT
255
680
660
830
papers read per turn - red PR blue REP
NIL
NIL
0.0
50.0
0.0
6.0
true
false
"" ""
PENS
"pen-0" 1.0 2 -7500403 true "" ""

SWITCH
35
660
170
693
age-weight?
age-weight?
0
1
-1000

SWITCH
36
624
169
657
sawoff-zipf?
sawoff-zipf?
1
1
-1000

SWITCH
605
80
750
113
read-cap?
read-cap?
1
1
-1000

SWITCH
35
695
200
728
strategic-submission?
strategic-submission?
0
1
-1000

PLOT
1175
475
1485
625
quality of journals
NIL
NIL
0.0
2.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

MONITOR
255
245
380
290
Papers
count papers
17
1
11

MONITOR
255
295
380
340
Published papers
count papers with [published?]
17
1
11

PLOT
675
785
1172
935
quality of published papers
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

PLOT
255
525
660
675
comparing generate-qualities
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"zipf" 1.0 0 -16777216 true "" ""
"exp" 1.0 0 -5298144 true "" ""

SLIDER
35
820
225
853
productivity-to-reads-multiplier
productivity-to-reads-multiplier
1
10
5
1
1
NIL
HORIZONTAL

SLIDER
605
10
750
43
scientists-leaving
scientists-leaving
0
50
0
1
1
NIL
HORIZONTAL

MONITOR
35
555
200
600
number of scientsts
count scientists
0
1
11

SLIDER
35
740
207
773
rate-of-adaptation
rate-of-adaptation
0
1
0.1
0.1
1
NIL
HORIZONTAL

SWITCH
35
910
189
943
random-seed?
random-seed?
0
1
-1000

SWITCH
255
860
436
893
artificial-big-paper?
artificial-big-paper?
1
1
-1000

SLIDER
255
895
460
928
time-artificial-big-paper
time-artificial-big-paper
1
100
30
1
1
NIL
HORIZONTAL

SLIDER
35
860
225
893
obsolescence-threshold
obsolescence-threshold
5
100
100
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This is a model of paper publication and consumption that allows to study the effect of two different evaluation mechanisms, peer review and reputation, on the quality of the manuscripts accessed by a scientific community.

## HOW IT WORKS

In order to compare the two filters - reputation and peer review - the model includes a common paper production mechanism and then runs two distinct sub-models for accessing papers: one driven by reputation and the other by peer review.

The simulation cycle of the model begins with paper production, generating the papers for the current time step and adding them to the papers pool. Afterwards, it applies either a simulated peer review process or a simulated reputation mechanism, both driving parallel reading processes. In the former case, scientists read papers as prioritized by journal's quality whereas, in the latter case, they read papers as prioritized by their reputation.

The two sub-models share the same ground truth for what regards paper and scientist quality. This guarantees a clean comparison, made with exactly the same papers in the system, at the cost of ignoring the feedback from reading towards paper production.

## INSTALLATION NOTES (IMPORTANT!)

* **pr-utils** is a custom extension that must be present in the Netlogo extensions folder in a directory of the same name.
* A folder called **results** has to be present in the same location of the main netlogo file. This directory is used to store detailed results of each simulation.

## HOW TO USE IT

SETUP button — sets up the model by creating the agents.

GO button — runs the model

GO-ONCE - runs one step of the simulation

## THINGS TO TRY

In order to reproduce the results presented in the paper you will find defined the different set of experiments in the BehaviorSpace. Detailed results are stored in the folder "results" that previously must be created in the directory where the application is installed (see installation notes)

## HOW TO CITE

## CREDITS AND REFERENCES

Copyright 2015 Mario Paolucci & Jordi Sabater-Mir & Francisco Grimaldo under the GPL licence. Developed at LABSS (http://labss.istc.cnr.it/), ETSE-UV, IIIA-CSIC.
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

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="v7-JAAMAS-exp" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="44"/>
    <metric>sum [quality * count my-in-read-prs] of papers</metric>
    <metric>sum [quality * count my-in-read-prs] of obsoletes</metric>
    <metric>sum [quality * count my-in-read-reps] of papers</metric>
    <metric>sum [quality * count my-in-read-reps] of obsoletes</metric>
    <metric>sum [quality ^ 2 * count my-in-read-prs] of papers</metric>
    <metric>sum [quality ^ 2 * count my-in-read-prs] of obsoletes</metric>
    <metric>sum [quality ^ 2 * count my-in-read-reps] of papers</metric>
    <metric>sum [quality ^ 2 * count my-in-read-reps] of obsoletes</metric>
    <metric>count papers</metric>
    <metric>count papers with [mistaken?]</metric>
    <metric>count papers with [published?]</metric>
    <metric>count papers with [not any? my-in-read-prs]</metric>
    <metric>count papers with [not any? my-in-read-reps]</metric>
    <metric>count papers with [published? and not any? my-in-read-prs]</metric>
    <metric>correlationScientistQualityRep</metric>
    <metric>correlationScientistQualityReadPR</metric>
    <metric>correlationScientistReputationReadREP</metric>
    <metric>correlationPapersQualityRep</metric>
    <metric>correlationPapersReputationReadREP</metric>
    <enumeratedValueSet variable="productivity-to-reads-multiplier">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="1047"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-Zipf-exponent">
      <value value="1.87"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-exp-mean">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-of-adaptation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-authors-per-paper">
      <value value="2.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-artificial-big-paper">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sawoff-zipf?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scientists-leaving">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-reviewers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="production-mean">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategic-submission?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="excellence-criterion">
      <value value="&quot;exponentialrandom&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-scientists-per-turn">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-of-journals">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obsolescence-threshold">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="artificial-big-paper?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationType">
      <value value="&quot;both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-weight?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="keep-co-workers">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="read-cap?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v7-JAAMAS-zipf" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="44"/>
    <metric>sum [quality * count my-in-read-prs] of papers</metric>
    <metric>sum [quality * count my-in-read-prs] of obsoletes</metric>
    <metric>sum [quality * count my-in-read-reps] of papers</metric>
    <metric>sum [quality * count my-in-read-reps] of obsoletes</metric>
    <metric>sum [quality ^ 2 * count my-in-read-prs] of papers</metric>
    <metric>sum [quality ^ 2 * count my-in-read-prs] of obsoletes</metric>
    <metric>sum [quality ^ 2 * count my-in-read-reps] of papers</metric>
    <metric>sum [quality ^ 2 * count my-in-read-reps] of obsoletes</metric>
    <metric>count papers</metric>
    <metric>count papers with [mistaken?]</metric>
    <metric>count papers with [published?]</metric>
    <metric>count papers with [not any? my-in-read-prs]</metric>
    <metric>count papers with [not any? my-in-read-reps]</metric>
    <metric>count papers with [published? and not any? my-in-read-prs]</metric>
    <metric>correlationScientistQualityRep</metric>
    <metric>correlationScientistQualityReadPR</metric>
    <metric>correlationScientistReputationReadREP</metric>
    <metric>correlationPapersQualityRep</metric>
    <metric>correlationPapersReputationReadREP</metric>
    <enumeratedValueSet variable="productivity-to-reads-multiplier">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-Zipf-exponent">
      <value value="1.88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-exp-mean">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-of-adaptation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-authors-per-paper">
      <value value="2.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-artificial-big-paper">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sawoff-zipf?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scientists-leaving">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-reviewers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="production-mean">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategic-submission?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="excellence-criterion">
      <value value="&quot;zipf&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-scientists-per-turn">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-of-journals">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obsolescence-threshold">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="artificial-big-paper?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationType">
      <value value="&quot;both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-weight?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="keep-co-workers">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="read-cap?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v7-PR-exp" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="44"/>
    <metric>sum [quality * count my-in-read-prs] of papers</metric>
    <metric>sum [quality * count my-in-read-prs] of obsoletes</metric>
    <metric>sum [quality * count my-in-read-reps] of papers</metric>
    <metric>sum [quality * count my-in-read-reps] of obsoletes</metric>
    <metric>sum [quality ^ 2 * count my-in-read-prs] of papers</metric>
    <metric>sum [quality ^ 2 * count my-in-read-prs] of obsoletes</metric>
    <metric>sum [quality ^ 2 * count my-in-read-reps] of papers</metric>
    <metric>sum [quality ^ 2 * count my-in-read-reps] of obsoletes</metric>
    <metric>count papers</metric>
    <metric>count papers with [mistaken?]</metric>
    <metric>count papers with [published?]</metric>
    <metric>count papers with [not any? my-in-read-prs]</metric>
    <metric>count papers with [not any? my-in-read-reps]</metric>
    <metric>count papers with [published? and not any? my-in-read-prs]</metric>
    <metric>correlationScientistQualityRep</metric>
    <metric>correlationScientistQualityReadPR</metric>
    <metric>correlationScientistReputationReadREP</metric>
    <metric>correlationPapersQualityRep</metric>
    <metric>correlationPapersReputationReadREP</metric>
    <enumeratedValueSet variable="productivity-to-reads-multiplier">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="848"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-Zipf-exponent">
      <value value="1.88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-exp-mean">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-of-adaptation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-authors-per-paper">
      <value value="2.275"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-artificial-big-paper">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sawoff-zipf?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scientists-leaving">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-reviewers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="production-mean">
      <value value="0.07"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategic-submission?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="excellence-criterion">
      <value value="&quot;exponentialrandom&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-scientists-per-turn">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-of-journals">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obsolescence-threshold">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="artificial-big-paper?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationType">
      <value value="&quot;both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-weight?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="keep-co-workers">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="read-cap?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="v7-PR-zipf" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="44"/>
    <metric>sum [quality * count my-in-read-prs] of papers</metric>
    <metric>sum [quality * count my-in-read-prs] of obsoletes</metric>
    <metric>sum [quality * count my-in-read-reps] of papers</metric>
    <metric>sum [quality * count my-in-read-reps] of obsoletes</metric>
    <metric>sum [quality ^ 2 * count my-in-read-prs] of papers</metric>
    <metric>sum [quality ^ 2 * count my-in-read-prs] of obsoletes</metric>
    <metric>sum [quality ^ 2 * count my-in-read-reps] of papers</metric>
    <metric>sum [quality ^ 2 * count my-in-read-reps] of obsoletes</metric>
    <metric>count papers</metric>
    <metric>count papers with [mistaken?]</metric>
    <metric>count papers with [published?]</metric>
    <metric>count papers with [not any? my-in-read-prs]</metric>
    <metric>count papers with [not any? my-in-read-reps]</metric>
    <metric>count papers with [published? and not any? my-in-read-prs]</metric>
    <metric>correlationScientistQualityRep</metric>
    <metric>correlationScientistQualityReadPR</metric>
    <metric>correlationScientistReputationReadREP</metric>
    <metric>correlationPapersQualityRep</metric>
    <metric>correlationPapersReputationReadREP</metric>
    <enumeratedValueSet variable="productivity-to-reads-multiplier">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-scientists">
      <value value="848"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-Zipf-exponent">
      <value value="1.88"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-exp-mean">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-of-adaptation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-authors-per-paper">
      <value value="2.275"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-artificial-big-paper">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sawoff-zipf?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scientists-leaving">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-reviewers">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="production-mean">
      <value value="0.07"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="strategic-submission?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="excellence-criterion">
      <value value="&quot;zipf&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-scientists-per-turn">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-of-journals">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obsolescence-threshold">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="artificial-big-paper?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationType">
      <value value="&quot;both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="age-weight?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="keep-co-workers">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="read-cap?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-seed?">
      <value value="true"/>
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
