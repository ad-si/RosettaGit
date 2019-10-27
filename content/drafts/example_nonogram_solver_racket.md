+++
title = "Example:Nonogram solver/Racket"
description = ""
date = 2014-05-31T20:35:49Z
aliases = []
[extra]
id = 17372
[taxonomies]
categories = []
tags = []
+++

<noinclude>{{Programming-example-page|Nonogram solver|language=Racket}}</noinclude>
There are a number of functional parts to this...
*parsing the files
*getting the data into a usable state
*rendering (for debug and prettiness)
*the actual solution of the Nonograms themselves
*as well as the ubiquitous test cases
So we'll have it here on its own page


```racket
#lang racket
;;; --------------------------------------------------------------------------------------------------
;;; Representaiton is done with bits: the bit patterns for a block being:
;;; -------------------------------------------------------------------------
;;; #b00 (0) - block is not in representation (also a terminator on the left)
;;; #b01 (1) - block is white
;;; #b10 (2) - block is black
;;; #b11 (3) - block is undecided
;;; None of the problems has > 32 columns, so 64-bits should be fine
;;; If we go above 64-bits, then a. we have a difficult problem
;;;                              b. racket will use bignums rather
;;;                                 than fixnums
;;;
;;; A "blocks" is an integer formed of two-bit block codes (above)
;;;
;;; A "representation" is a sequence (list) of black stretch lengths; which need to be separated by at
;;; least one white between, and optionally prefixed and suffied with whites
;;;
;;; A "candidate" is a sequence (list) of <white-length [black-length white-length]...>, specifying
;;; one instance of a "representation".
;;;
;;; A "puzzle" is a sequence (vector) of blocks
;;; -- if the puzzle is <= 32 blocks wide, this could well be an fxvector (but ignore that
;;;    possibility for now)
;;;
;;; "Options" is a sequence (list) of blocks
;;;
;;; white is often abbreviated (in variables etc. to W), black to "B"
;;; --------------------------------------------------------------------------------------------------
(module+ test (require rackunit))
(define *problems-file-name* "nonogram_problems.txt")

;;; --------------------------------------------------------------------------------------------------
;;; Parsing Input
;;; --------------------------------------------------------------------------------------------------
(define (letter-rep->number-rep c) (+ 1 (- (char->integer (char-upcase c)) (char->integer #\A))))

;; takes the letters representation, returns a list of list of numbers. The list returned is an
;; "option" - a list of ([white-width black-width] ... white-width)
(define (letters-rep->list²-rep s)
  (for/list ((b (regexp-split #rx" +" s)))
    (map letter-rep->number-rep (string->list b))))

(define (read-nonogram-description prt)
  (match (read-line prt)
    [(? eof-object?) #f]
    ["" (read-nonogram-description prt)]
    [(? string? l)
     (vector (letters-rep->list²-rep l)
             (letters-rep->list²-rep (read-line prt)))]))

(define (read-one-nonogram-from-file file-name)
  (call-with-input-file file-name read-nonogram-description))

(module+ test
  (check-equal? (map letter-rep->number-rep '(#\A #\a #\B #\C)) '(1 1 2 3))  
  (check-equal? (letters-rep->list²-rep "C BA CB BB F AE F A B")
                '([3] [2 1] [3 2] [2 2] [6] [1 5] [6] [1] [2]))
  (check-equal? (letters-rep->list²-rep "AB CA AE GA E C D C")
                '([1 2]  [3 1]  [1 5]  [7 1]  [5]  [3]  [4] [3]))
  (check-equal? (read-one-nonogram-from-file *problems-file-name*)
                #(([3] [2 1] [3 2] [2 2] [6] [1 5] [6] [1] [2])
                  ([1 2]  [3 1]  [1 5]  [7 1]  [5]  [3]  [4] [3]))))

;;; --------------------------------------------------------------------------------------------------
;;; Generate Candidates
;;; --------------------------------------------------------------------------------------------------
(define (rep->candidates n-cells blacks)
  (define (inr cells-remain bs leftmost?)
    (define bs-l (sequence-length bs))
    (define min-space-needed (- (apply + bs-l bs) 1))
    (cond
      [(null? bs) (list (list cells-remain))]
      [(> min-space-needed cells-remain) null]
      [else
       (define initial-whites-min-size (if leftmost? 0 1))
       (define intial-whites-range
         (in-range initial-whites-min-size (add1 (- cells-remain min-space-needed))))
       (for*/list ((intial-whites intial-whites-range)
                   (tl (in-list (inr (- cells-remain intial-whites (car bs)) (cdr bs) #f))))
         (list* intial-whites (car bs) tl))]))
  (inr n-cells blacks #t))

(module+ test
  (check-match
   (rep->candidates 5 '(1)) (list-no-order '(0 1 4) '(1 1 3) '(2 1 2) '(3 1 1) '(4 1 0)))
  (check-match
   (rep->candidates 5 '(1 1))
   (list-no-order '(0 1 1 1 2) '(0 1 2 1 1) '(0 1 3 1 0) '(1 1 1 1 1) '(1 1 2 1 0) '(2 1 1 1 0))))

(define (make-Ws l) (for/fold ((rv 0)) ((_ l)) (+ (* 4 rv) #b01)))
(define (make-Bs l) (* 2 (make-Ws l)))
(module+ test
  (check-eq? (make-Ws 0) #b00)
  (check-eq? (make-Bs 0) #b00)
  (check-eq? (make-Ws 1) #b01)
  (check-eq? (make-Bs 1) #b10)
  (check-eq? (make-Ws 3) #b010101)
  (check-eq? (make-Bs 3) #b101010))

(define (candidate->blocks cand)
  (define (inr cand rv)
    (match cand
      [(list (and W (app make-Ws Ws)) (and B (app make-Bs Bs)) r ...)
       (inr r (+ (arithmetic-shift rv (* 2 (+ B W))) (arithmetic-shift Ws (* 2 B)) Bs))]
      [(list (and W (app make-Ws Ws))) (+ (arithmetic-shift rv (* 2 W)) Ws)]))
  (inr cand 0))

(module+ test
  (check-eq? (candidate->blocks '(0)) 0)
  (check-eq? (candidate->blocks '(1)) #b01)
  (check-eq? (candidate->blocks '(1 1 1)) #b011001)
  (check-equal?
   (map candidate->blocks
        '((0 1 1 1 2) (0 1 2 1 1) (0 1 3 1 0) (1 1 1 1 1) (1 1 2 1 0) (2 1 1 1 0)))
   '(#b1001100101 #b1001011001 #b1001010110 #b0110011001 #b0110010110 #b0101100110)))

;; Given a (non-empty) sequence of blocks return a list of blocks which must be black, must be
;; white or have to be dertermined another way (through matching along the other axis).
(define (find-definite-blocks blocks)
  (for/fold ((known (sequence-ref blocks 0))) ((merge blocks))
    (bitwise-ior known merge)))

(module+ test
  (check-eq? (find-definite-blocks '(#b010101 #b010110 #b100110)) #b110111)
  )

;; returns the list of blocks (from options) that can be overlaid over the solution
;; this means that the following must hold false for all bits (if it holds false, we can do a zero?
;; test, which is easiser than an all-significant-bits-set? test:
;; pattern cand  
;;    0      0     F
;;    0      1     T
;;    1      0     F
;;    1      1     F
;; (cand !bitwise-impl pattern) = !(pattern | !cand) = (!pattern & cand)
(define (filter-against-partial-solution part-sltn options)
  (define not-part-sltn (bitwise-not part-sltn))
  (define (option-fits? cand) (zero? (bitwise-and cand not-part-sltn)))
  (filter option-fits? options))

(module+ test
  (check-equal?
   (filter-against-partial-solution #b011110 '(#b101010 #b010110 #b011010))
   '(#b010110 #b011010)))

;;; --------------------------------------------------------------------------------------------------
;;; Rendering -- it's pretty tough to see what's going on, when you have no pictures!
;;; --------------------------------------------------------------------------------------------------
(define ((render-puzzle knil kons-W kons-B kons-_ compose-lines) pzl)
  (define (render-blocks bs)
    (define (inr bs acc)
      (match (bitwise-and bs #b11)
        [#b00 acc]
        [#b01 (inr (arithmetic-shift bs -2) (kons-W acc))]
        [#b10 (inr (arithmetic-shift bs -2) (kons-B acc))]
        [#b11 (inr (arithmetic-shift bs -2) (kons-_ acc))]))
    (inr bs knil))
  (compose-lines (map render-blocks pzl)))

(define string-render-puzzle
  (render-puzzle ""
                 (curry string-append ".")
                 (curry string-append "#")
                 (curry string-append "?")
                 (curryr string-join "\n")))

(module+ test
  (check-equal? (string-render-puzzle '(#b101010 #b010101 #b111111)) "###\n...\n???"))

;;; We need to work on x and y blocks uniformly, so this will convert from one to t'other
;;; Rotates a sequence of blocks
(define (rotate-blocks x-blocks)
  (define x-width- (integer-length (sequence-ref x-blocks 0)))
  (define x-width (if (odd? x-width-) (add1 x-width-) x-width-))
  ;(printf "~a ~a" x-width x-blocks)
  (for/list ((block-idx (in-range x-width 0 -2)))
    (for/fold ((y-block 0))
      ((x-block x-blocks))
      (+ (arithmetic-shift y-block 2)
         (bitwise-bit-field x-block (- block-idx 2) block-idx)))))

(module+ test
  (check-equal? (rotate-blocks '(#b1110 #b0111)) '(#b1101 #b1011))
  (check-equal? (rotate-blocks '(#b0110 #b0111)) '(#b0101 #b1011)))

;;; --------------------------------------------------------------------------------------------------
;;; SOLVER (finally!):
;;; --------------------------------------------------------------------------------------------------
;;; solve the nonogram... both "solvers" return as values:
;;;   solution-changed? did the solution change -- if not we either have a solution or as good a
;;;                     solution as the program can provide
;;;   new-solution      the newly-changed solution
;;;   new-x-blocks      x-blocks that are now available as candidates
;;;   new-y-blocks      y-blocks that are now available as candidates
(define (solved? blocks) (for/and ((b blocks)) (= (sequence-length b) 1)))

(define (solve-nonogram x-rep.y-rep) ; pair of reps as read from e.g. read-nonogram-from-file
  (match-define (vector x-rep y-rep) x-rep.y-rep)
  (define width (sequence-length y-rep))
  (define height  (sequence-length x-rep))
  (define x-candidates (map (curry rep->candidates width) x-rep))
  (define y-candidates (map (curry rep->candidates height) y-rep))
  (define x-options    (for/list ((cnds x-candidates)) (map candidate->blocks cnds)))
  (define y-options    (for/list ((cnds y-candidates)) (map candidate->blocks cnds)))
  
  (define-values (solution complete?) (sub-solve-nonogram x-options y-options))
  (unless complete? (displayln "INCOMPLETE SOLUTION"))
  solution)

(define (sub-solve-nonogram x-options y-options)
  (define known-x (map find-definite-blocks x-options))
  (define known-y (map find-definite-blocks y-options))
  (cond
    [(solved? x-options) (values known-x #t)]
    [else
     (define new-y-options (map filter-against-partial-solution (rotate-blocks known-x) y-options))
     (define new-x-options (map filter-against-partial-solution (rotate-blocks known-y) x-options))
     (displayln (string-render-puzzle (map find-definite-blocks new-x-options)) (current-error-port))
     (newline (current-error-port))     
     (if (and (equal? x-options new-x-options) (equal? y-options new-y-options))
         (values known-x #f) ; oh... we can't get any further
         (sub-solve-nonogram new-x-options new-y-options))]))

;;;; TESTING
(module+ test
  (define chicken #<<EOS
.###....
##.#....
.###..##
..##..##
..######
#.#####.
######..
....#...
...##...
EOS
    )
  (check-equal?
   (string-render-puzzle (solve-nonogram (read-one-nonogram-from-file *problems-file-name*)))
   chicken))

;;; IMAGE RENDERING
(require pict racket/gui/base)

(define *cell-size* 10)
(define ((paint-cell fill-colour) dc dx dy)
  (define C (- *cell-size* 1))
  (define old-brush (send dc get-brush))
  (define old-pen (send dc get-pen))        
  (define path (new dc-path%))
  (send path rectangle 0 0 C C)
  (send* dc
    (set-brush (new brush% [color fill-colour]))
    (set-pen (new pen% [width 1] [color "black"]))
    (draw-path path dx dy)
    (set-brush old-brush)
    (set-pen old-pen)))

(define (draw-cell fill-colour)
  (dc (paint-cell fill-colour)
      *cell-size* *cell-size*))

(define ((row-append-cell colour) row-so-far)
  (hc-append (draw-cell colour)  row-so-far))

(define image-render-puzzle
  (render-puzzle
   (blank)
   (row-append-cell "white")
   (row-append-cell "black")
   (row-append-cell "yellow")
   (λ (rows) (apply vc-append rows))))

(module+ test
  ; test though visual inspection... it's a style thing, really
  (image-render-puzzle (solve-nonogram (read-one-nonogram-from-file *problems-file-name*))))

;;; MAIN
(module+ main
  (unless (directory-exists? "images") (make-directory "images"))
  
  (call-with-input-file *problems-file-name*
    (lambda (prt)
      (let loop ((idx 1) (pzl (read-nonogram-description prt)) (collage (blank)))
        (cond
          [pzl
           (define img (image-render-puzzle (solve-nonogram pzl)))
           (send (pict->bitmap img) save-file (build-path "images" (format "nonogram-~a.png" idx))
                 'png)
           (displayln (image-render-puzzle (solve-nonogram pzl)))
           (loop (add1 idx) (read-nonogram-description prt) (vl-append 2 collage img))]
          [else
           (send (pict->bitmap collage) save-file (build-path "images" (format "nonogram-all.png"))
                 'png)
           (displayln collage)])))))
```


{{out}}

I have little luck with attaching images. You may need to run the program in Racket and, if you can, attach some pictures here!

Ah... foo! Here's an external link to one I did earlier... [[http://timb.net/images/nonogram-all.png]]. Life's too short to wrestle with Rosetta Code...

After changing the:
 (displayln (image-render-puzzle (solve-nonogram pzl)))
to
 (displayln (string-render-puzzle (solve-nonogram pzl)))
at the end of the '''main''' module part, we get the following on standard out (stderr has been redirected to '''/dev/null''')



```txt
.###....
##.#....
.###..##
..##..##
..######
#.#####.
######..
....#...
...##...
..........######....
........###.#..###..
...#..###...#....###
..###.##############
...#..#............#
..#.#.##..........##
#####..##........##.
#####...#........#..
#####..###.###.###..
########.###.###.###
....###.#...........
....##.####.#.......
....#.###.###.......
..##.####...........
.###.###.#....###...
###..##.##...#.###..
##..##.##....##.##..
....##.#.#..##.#.#..
....#.##.#...####...
....#.#.##.....##...
.....##.##..########
....##.##...##..####
....#.##.##.#...#..#
###..###.#####.....#
#.#.###.#....#....##
##..###.#....###.###
.#.###.##.########..
.####.###.########..
...#.####.##.#####..
...#.####.##...##...
....####..##...#####
...#####.###...#####
...####.#..........#
..####.##...........
..###.###...........
....................#####
..##..............###..##
.##..............#####..#
##.............########..
##....#####.###########..
#.#..##....#....######...
#..##.....#.......###....
##........#.............#
.##.....######.........##
..###############....####
.....##########..########
....##.#.####.###..######
........#################
........#################
.......##################
.......#...##############
.......#.#.##############
........#####...#########
.................########
..................#######


```

