+++
title = "Example:Total circles area/Racket"
description = ""
date = 2014-07-15T09:21:34Z
aliases = []
[extra]
id = 17773
[taxonomies]
categories = []
tags = []
+++


## Racket


<img src="timb.net/images/RC/Total-circles-area/TCA-all-circles.png" alt="All the circles"/><br/>
<img src="timb.net/images/RC/Total-circles-area/TCA-split-circles.png" alt="Showing the path edge from split-circles"/>

[[http://timb.net/images/RC/Total-circles-area/TCA-all-circles.png All the circles]]
[[http://timb.net/images/RC/Total-circles-area/TCA-split-circles.png Showing the path edge from split-circles]]

Below are an analytical solution in <code>TCA-analytical.rkt</code>, and a Monte-carlo solution in
<code>TCA-monte-carlo.rkt</code>. Common code (notably types) is in <code>TCA-types.rkt</code>, and
<code>TCA-task.rkt</code> (we don't need to list the circle coordinates more than once).

In order to get my head around the algorithm, I have also thrown together a <code>TCA-draw.rkt</code>
which (if I can get RC to let me upload piccies) produces the attached images (otherwise you'll have
to go to timb.net for them).

===<code>TCA-types.rkt</code>===
This solution (with the exception of <code>TCA-draw.rkt</code>) is in ''typed/racket''. This file
defines the types, and geometrical arithmetic. In fact it encroaches on the analytical solution's
magic -- but it's not so easy to separate concerns here.


```racket
#lang typed/racket
;;; TCA-types : basic geometric types, and some basic geometric operations upon them

;; Arguably even the split circles / other path operations are general enough to 
(provide (all-defined-out)) ; to external "drawing" module

(struct Vec    ((x : Real) (y : Real)))
(struct Angle2 ((a0 : Angle) (a1 : Angle))) ; Angles of the start and end points of the circle arc.
(struct Circle ((c : Vec) (r : Real)))
(struct Arc    ((c : Circle) (a2 : Angle2)))

(define-type Angle Real)
(define-type Vecs (Listof Vec))
(define-type Arcs (Listof Arc))
(define-type Angles (Listof Angle))
(define-type Circles (Listof Circle))

(define-type Geometric (U Vec Circle Arc))

(: v-cross : Vec Vec -> Real)
(: v-dot   : Vec Vec -> Real)
(: v+      : Vec Vec -> Vec)
(: v-      : Vec Vec -> Vec)
(: v-len   : Vec -> Real)
(: v-dist  : Vec Vec -> Real)
(: v-scale : Vec Real -> Vec)
(: v-norm  : Vec -> Vec)

(define/match (v-cross v0 v1)
  [((Vec a b) (Vec c d)) (- (* a d) (* b c))])

(define/match (v-dot v0 v1)
  [((Vec a b) (Vec c d)) (+ (* a c) (* b d))])

(define/match (v+ v0 v1)
  [((Vec a b) (Vec c d)) (Vec (+ a c) (+ b d))])

(define/match (v- v0 v1)
  [((Vec a b) (Vec c d)) (Vec (- a c) (- b d))])

(define (v-len v)
  (cast (sqrt (v-dot v v)) Real))

(define (v-dist a b)
  (v-len (v- a b)))

(define/match (v-scale v s)
  [((Vec x y) s) (Vec (* x s) (* y s))])

(define/match (v-norm v)
  [((and (Vec x y) (app v-len l))) (Vec (/ x l) (/ y l))])

(: v-angle : Vec -> Angle)
(: a-norm  : Angle -> Angle)

(define/match (v-angle v)
  [((Vec x y)) (atan y x)])

(define (a-norm a)
  (cond [(> a pi) (- a (* 2 pi))] [(< a (- pi)) (+ a (* 2 pi))] [else a]))

(: circle-cross : Circle -> (Circle -> Angles))
(define/match ((circle-cross C0) C1)
  [((Circle c0 r0) (Circle c1 r1))
   (define d (v-dist c0 c1))
   (cond
     [(>= d (+ r0 r1)) null]
     [(<= d (abs (- r0 r1))) null]
     [else
      (define s (/ (+ r0 r1 d) 2))
      (define a (sqrt (* s (- s d) (- s r0) (- s r1))))
      (define h (* 2 (/ a d)))
      (define dr (v- c1 c0))
      (define r0^2 (sqr r0))
      (define dr-ang (v-angle dr))
      (define ang (+ dr-ang (if (> (+ r0^2 (sqr d)) (sqr r1)) 0 pi)))
      (define da (cast (asin (/ h r0)) Real))
      
      (map a-norm (list (- ang da) (+ ang da)))])])

(: arc-point  : Circle Angle -> Vec)
(: arc-start  : Arc -> Vec)
(: arc-mid    : Arc -> Vec)
(: arc-end    : Arc -> Vec)
(: arc-centre : Arc -> Vec)
(: arc-area   : Arc -> Real)

(define/match (arc-point crc arc)
  [((Circle c r) a) (v+ c (Vec (* r (cos a)) (* r (sin a))))])

(define/match (arc-start arc)  [((Arc c (Angle2 a0 _)))  (arc-point c a0)])
(define/match (arc-mid arc)    [((Arc c (Angle2 a0 a1))) (arc-point c (/ (+ a0 a1) 2))])
(define/match (arc-end arc)    [((Arc c (Angle2 _  a1))) (arc-point c a1)])
(define/match (arc-centre arc) [((Arc (Circle c _) _)) c])

(define arc-area
  (match-lambda
    [(Arc (Circle _ r) (Angle2 a0 a1))
     (* (sqr r) (- a1 a0) 1/2)]))

(: tri-area : Vec Vec Vec -> Real)
(define (tri-area a b c) (/ (v-cross (v- b a) (v- c b)) 2))

;; Boundaries -- needed both for drawing and monte-carlo
(: x-min : Any -> Real)
(: x-max : Any -> Real)
(: y-min : Any -> Real)
(: y-max : Any -> Real)

;; values is too general for the type checker... so (argmin values l) is not possible
(: real-min : (Listof Real) -> Real)
(: real-max : (Listof Real) -> Real)

(define (real-min l)
  (foldl min +Inf.0 l))

(define (real-max l)
  (foldl max -Inf.0 l))

(define/match (x-min o)
  [((list os ...)) (real-min (map x-min os))]
  [((Vec x y)) x]
  [((Circle (Vec x y) r)) (- x r)]
  [((Arc c _)) (x-min c)]
  [(else) +Inf.0])

(define/match (x-max o)
  [((list os ...)) (real-max (map x-max os))]
  [((Vec x y)) x]
  [((Circle (Vec x y) r)) (+ x r)]
  [((Arc c _)) (x-max c)]
  [(else) -Inf.0])

(define/match (y-min o)
  [((list os ...)) (real-min (map y-min os))]
  [((Vec x y)) y]
  [((Circle (Vec x y) r)) (- y r)]
  [((Arc c _)) (y-min c)]
  [(else) +Inf.0])

(define/match (y-max o)
  [((list os ...)) (real-max (map y-max os))]
  [((Vec x y)) y]
  [((Circle (Vec x y) r)) (+ y r)]
  [((Arc c _)) (y-max c)]
  [(else) -Inf.0])
```


===<code>TCA-draw.rkt</code>===
This does not depend on any algorithms, and allows visualisation of the geometrical objects (in fact
scenarios composed of lists of said objects)


```racket
#lang racket
(require "TCA-types.rkt")
(require pict)
(provide (all-defined-out))
(define/match (draw-object dc o)
  [(dc (list o ot ...)) (draw-object dc o) (draw-object dc ot)]
  [(dc (list)) (void)]
  [(dc (Vec x y)) (send dc draw-point x y)]
  [(dc (? string? s)) (send* dc (set-pen s 0 'solid) (set-brush s 'transparent))]
  [(dc (Circle (Vec cx cy) r)) (send dc draw-ellipse (- cx r) (- cy r) (* 2 r) (* 2 r))]
  [(dc (and arc (Arc (Circle (Vec cx cy) r) (Angle2 start end))))
   (send dc draw-arc (- cx r) (- cy r) (* 2 r) (* 2 r) (- end) (- start))])

(define (draw-situation #:fill-first? (fill-first? #f) pict-w pict-h o0 . os)
  (define o* (cons o0 os))
  (define x* (x-min o*))
  (define y* (y-min o*))
  (define w* (- (x-max o*) x*))
  (define h* (- (y-max o*) y*))
  
  (dc
   (λ (dc dx dy)
     (define-values (canvas-w canvas-h) (send dc get-size))
     (define scl-x (/ canvas-w w*))
     (define scl-y (/ canvas-h h*))
     (define scl (min scl-x scl-y))
     (send* dc
       (set-initial-matrix (vector scl 0 0 (- scl) 0 0))
       (translate (- x*) (- (+ y* h*)))
       ;; outline
       (set-pen "red" 0 'solid) (set-brush "red" 'transparent)
       (draw-rectangle x* y* w* h*)
       ;; colour of tail objects
       (set-pen   "green" 0 'solid) (set-brush "green" (if fill-first? 'solid 'transparent)))     
     ;; drawn in reverse, so last in list is most background
     (for ((o (reverse os))) (draw-object dc o))     
     ;; colour of first object -- drawn last, so on top
     (send* dc (set-pen "cyan" 0 'solid) (set-brush "cyan" 'transparent))
     (draw-object dc o0))
   pict-w pict-h))

(define (show-situation #:fill-first? (f-1st? #f) #:width (w 600) #:height (h 400) o0 . os)
  (show-pict (apply draw-situation w h o0 os #:fill-first? f-1st?)))

;; Saves image of situation to file
(define (file-situation file-name #:fill-first? (f-1st? #f) #:width (w 600) #:height (h 400) o0 . os)
  (define bmp (pict->bitmap (apply draw-situation w h o0 os #:fill-first? f-1st?)))
  (send bmp save-file file-name 'png))
```


===<code>TCA-monte-carlo.rkt</code>===
* If <var><code>deeper</code></var> is ≤ 0, then this does a plain old Monte Carlo approximation.
* If <var><code>deeper</code></var> > 0, then the sample space is divided into 4 (2x2), and circles
that cannot possibly affect each subsample are discarded. This allows for more focussed "inside?"
testing.

Nothing more sophisticated is attempted. That would be verging on ''analytical''.


```racket
#lang typed/racket
(require "TCA-types.rkt")
(provide circles-area)

(: circles-area :
   Circles [#:samples Nonnegative-Integer]
   [#:bounds (U #f (List Real Real Real Real))]
   [#:deeper Nonnegative-Integer]
   -> Real)

;; Provides naive sub-sampling when deeper > 0
(define (circles-area all-cs #:samples (samples 1000) #:bounds (bounds #f) #:deeper (deeper 6))
  (when (>= deeper 3) (eprintf "[~a/~a]" deeper (length all-cs)) (flush-output))
  (define x (or (and bounds (first bounds))  (x-min all-cs)))
  (define y (or (and bounds (second bounds)) (y-min all-cs)))
  (define w (or (and bounds (third bounds))  (- (x-max all-cs) x)))
  (define h (or (and bounds (fourth bounds)) (- (y-max all-cs) y)))
  (define w/2 (/ w 2))
  (define h/2 (/ h 2))
  (define x+ (+ x w/2))
  (define y+ (+ y h/2))
  (define bounds-c (Vec x+ y+))
  (define bounds-diagonal (v-dist (Vec x y) bounds-c))
  
  ;; we won't go into the detail of "circles that intersect the area",
  ;; just circles that can't possibly overlap
  ;; (their radius is further than the diagonal of the bounds)
  ;; if we want more analysis on this, then we'll use the TCA-analytical solution. No?
  (: circle-threatens? : Circle -> Boolean)
  (define (circle-threatens? c) (< (v-dist (Circle-c c) bounds-c) (+ (Circle-r c) bounds-diagonal)))
  (define cs (filter circle-threatens? all-cs))
  (cond
    [(null? cs) 0]
    [(not (positive? deeper))
     (: in-circle? : Vec -> (Circle -> Boolean))
     (define ((in-circle? v) c) (< (v-dist v (Circle-c c)) (Circle-r c)))
     (: in-any-circle? : Vec -> Boolean)
     (define (in-any-circle? v)
       (list? (memf (in-circle? v) cs)))
     (: rand-vec : -> Vec)
     (define (rand-vec) (Vec (+ x (* w (random))) (+ y (* h (random)))))
     (define hits (for/sum : Real ((i (in-range samples)) #:when (in-any-circle? (rand-vec))) 1))
     (* hits (* w h) (/ samples))]
    [else
     (+ (circles-area cs #:samples samples #:bounds (list x y w/2 h/2) #:deeper (sub1 deeper))
        (circles-area cs #:samples samples #:bounds (list x+ y w/2 h/2) #:deeper (sub1 deeper))
        (circles-area cs #:samples samples #:bounds (list x y+ w/2 h/2) #:deeper (sub1 deeper))
        (circles-area cs #:samples samples #:bounds (list x+ y+ w/2 h/2) #:deeper (sub1 deeper)))]))
```


===<code>TCA-analytical.rkt</code>===
{{trans|Haskell}}

This provides an analytical solution. Much of the Haskell heavy lifting is rewitten in
<code>TCA-types.rkt</code>.


```racket
#lang typed/racket
(require "TCA-types.rkt")
(provide circles-area
         split-circles ;; I want to be able to draw this
         )

(: split-1-circle : (Pair Circle Angles) -> Arcs)
(define/match (split-1-circle c.angs)
  [((cons c angs))
   (for/list : Arcs ((a1 (in-list angs)) (a2 (in-list (cdr angs)))) (Arc c (Angle2 a1 a2)))])

(: split-circles : Circles -> Arcs)
(define (split-circles cs)
  (: in-circle? (Arc Circle -> Boolean))
  (define/match (in-circle? a c)
    [((and arc (Arc (? Circle? c1) _)) (and c2 (Circle cen2 r2)))
     (and (not (equal? c1 c2)) (< (v-dist (arc-mid arc) cen2) r2))])
  
  ;; If an arc that was part of one circle is inside *another* circle,
  ;; it will not be part of the zero-winding path, so reject it.
  (: not-in-any-circle? : Arc -> Boolean)
  (define/match (not-in-any-circle? arc)
    [(arc) (not (memf (curry in-circle? arc) cs))])
  
  (: f : Circle -> (Pair Circle Angles))
  (define (f c)
    (cons c (sort (list* (- pi) pi (apply append (map (circle-cross c) cs))) <)))
  
  (define c-angs (map f cs))
  
  (define arcs (apply append (map split-1-circle c-angs)))
  
  (filter not-in-any-circle? arcs))

#|
Given a list of arcs, build sets of closed paths from them. If one arc's end point is no more than
1e-4 from another's start point, they are considered connected.  Since these start/end points
resulted from intersecting circles earlier, they *should* be exactly the same, but floating point
precision may cause small differences, hence the 1e-4 error margin.  When there are genuinely
different intersections closer than this margin, the method will backfire, badly.
|#

(: arc-dist-quantum Real)
(define arc-dist-quantum 1e-12)

(: make-paths : Arcs -> (Listof Arcs))
(define (make-paths arcs)
  (: join-arcs : Arcs Arcs -> (Listof Arcs))
  (define join-arcs
    (match-lambda**
     [(a (list)) (list a)]
     [((list) (cons x xs)) (join-arcs (list x) xs)]
     [(a X)
      #:when (< (v-dist (arc-start (first a)) (arc-end (last a))) arc-dist-quantum)
      (cons a (join-arcs null X))]
     [(a (cons x xs))
      #:when (< (v-dist (arc-end (last a)) (arc-start x)) arc-dist-quantum)
      (join-arcs (append a (list x)) xs)]
     [(a (cons x xs)) (join-arcs a (append xs (list x)))]))
  (join-arcs null arcs))

;; Slice N-polygon into N-2 triangles.
(: polyline-area : Vecs -> Real)
(define (polyline-area v.vs)
  (match-define (cons v vs) v.vs)
  (for/sum : Real ((v0 : Vec (in-list vs)) (v1 : Vec (in-list (cdr vs))))
    (tri-area v v0 v1)))

(: path-area : Arcs -> Real)
(define (path-area arcs)
  (define-values (a e)
    (for/fold ((a : Real 0) (e : Vecs null))
      ((arc : Arc (in-list arcs)))
      (values (+ a (arc-area arc)) (append e (list (arc-centre arc) (arc-end arc))))))
  (+ a (polyline-area e)))

(: circles-area : Circles -> Real)
(define (circles-area cs)
  (apply + (map path-area (make-paths (split-circles cs)))))
```


===<code>TCA-task.rkt</code>===
This file does the business of showing results and whatnot.

```racket
#lang typed/racket

(require "TCA-types.rkt")

(require (prefix-in analytical: "TCA-analytical.rkt"))
(require (prefix-in monte-carlo: "TCA-monte-carlo.rkt"))

(provide (all-defined-out)) ; to external "drawing" module

(: circles Circles)
(define circles
  (list (Circle (Vec  1.6417233788  1.6121789534) 0.0848270516)
        (Circle (Vec -1.4944608174  1.2077959613) 1.1039549836)
        (Circle (Vec  0.6110294452 -0.6907087527) 0.9089162485)
        (Circle (Vec  0.3844862411  0.2923344616) 0.2375743054)
        (Circle (Vec -0.2495892950 -0.3832854473) 1.0845181219)
        (Circle (Vec  1.7813504266  1.6178237031) 0.8162655711)
        (Circle (Vec -0.1985249206 -0.8343333301) 0.0538864941)
        (Circle (Vec -1.7011985145 -0.1263820964) 0.4776976918)
        (Circle (Vec -0.4319462812  1.4104420482) 0.7886291537)
        (Circle (Vec  0.2178372997 -0.9499557344) 0.0357871187)
        (Circle (Vec -0.6294854565 -1.3078893852) 0.7653357688)
        (Circle (Vec  1.7952608455  0.6281269104) 0.2727652452)
        (Circle (Vec  1.4168575317  1.0683357171) 1.1016025378)
        (Circle (Vec  1.4637371396  0.9463877418) 1.1846214562)
        (Circle (Vec -0.5263668798  1.7315156631) 1.4428514068)
        (Circle (Vec -1.2197352481  0.9144146579) 1.0727263474)
        (Circle (Vec -0.1389358881  0.1092805780) 0.7350208828)
        (Circle (Vec  1.5293954595  0.0030278255) 1.2472867347)
        (Circle (Vec -0.5258728625  1.3782633069) 1.3495508831)
        (Circle (Vec -0.1403562064  0.2437382535) 1.3804956588)
        (Circle (Vec  0.8055826339 -0.0482092025) 0.3327165165)
        (Circle (Vec -0.6311979224  0.7184578971) 0.2491045282)
        (Circle (Vec  1.4685857879 -0.8347049536) 1.3670667538)
        (Circle (Vec -0.6855727502  1.6465021616) 1.0593087096)
        (Circle (Vec  0.0152957411  0.0638919221) 0.9771215985)))

(define exact-value 21.5650366038563989590842249288781480183977)
(: run-task : (Circles -> Real) -> Void)
(define (run-task f)
  (define a (f circles))
  (printf "circles-area (~s): ~a [error:~a]~%" f a (- a exact-value)))

(run-task analytical:circles-area)
(run-task monte-carlo:circles-area)

(define-type file-situation-drawer-t
  (->* (String (Pairof (U Geometric String) (Listof (U Geometric String))))
       (#:width Nonnegative-Integer #:height Nonnegative-Integer #:fill-first? Boolean)
       Void))

(require/typed "TCA-draw.rkt" [file-situation file-situation-drawer-t])
(require "TCA-analytical.rkt")

(file-situation "TCA-all-circles.png" (cons (car circles) (cdr circles)))
(file-situation "TCA-split-circles.png"
                (cons (car circles)
                      (append (cdr circles) (list "black") (split-circles circles))))
```


{{out}}
As well as this output, there are also two image files (whether or not you see them depends on my permissions at the moment!)

```txt
circles-area (#<procedure:circles-area>): 21.5650366038564 [error:0.0]
circles-area (#<procedure:circles-area>): 21.564722220577558 [error:-0.000314383278841035]
```

