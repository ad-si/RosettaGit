+++
title = "Example:Hough transform/Racket"
description = ""
date = 2014-06-10T06:52:36Z
aliases = []
[extra]
id = 17698
[taxonomies]
categories = []
tags = []
+++

{{header|Racket}}
{{trans|C}}

This example splits the original image into 4 channels, and passes each of them
to a Racket <code>place</code>. The places are implemented in modules, which
are invoked by name; which allows for multiple implementations (and, to be
honest, multiple transforms -- but we'll stick with Hough for now).

There are two modules:
* "Hough-transform-basic.rkt", which is easy to read, but runs with Racket's type-safety on (slowing it down)
* "Hough-transform-fast.rkt", which uses "unsafe-..." functions. These have been tested before being unleashed upon an unsuspecting public

The module implementations come first, followed by the code to split, delegate
and remerge the image channels.

[[http://timb.net/images/RC/Hough-transform/180px-Pentagon-Hough.png The Transformed Image]]


===Hough-transform-basic.rkt===


```racket
#lang racket
;;; Note that types are not mentioned here at all in this package

(provide hough-transform-channel)
;; For these half-quadrants, it's better to iterate by 'y'	
(define (iterate-quadrant θ)
  [cond
    [(< θ 45) 'y]
    [(< 315 θ) 'y]
    [(and (< 135 θ) (< θ 225)) 'y]
    [else 'x]])

(define (sub-hough-transform-channel w h trg-pxls trg-h deg-start deg-end chnl-pxls)
  ;; these constants appear repeatedly in the arithmetic
  (define w/2 (/ w 2))
  (define h/2 (/ h 2))
  (for ((deg (in-range deg-start deg-end)))
    ;; all these worth pre-computing...
    (define θ (degrees->radians deg))
    (define cθ (cos θ))
    (define sθ (sin θ))
    (define i-q (iterate-quadrant deg))
    (define (chnl-byte-idx x y) (+ (* y w) x))
    (for ((ρ (in-range 0 trg-h)))      
      ;; Sum the colors of the line with equation x*cos(θ) + y*sin(θ) = ρ
      (define-values (S P)
        (match i-q
          ['y ; else x quadrant
           (for*/fold ((S 0) (P 0))
             ((y (in-range 0 h))
              (x (in-value (+ w/2 (/ (- ρ (* (- h/2 y) sθ)) cθ))))
              #:unless (or (< x 0) (>= x w))
              (x (in-value (exact-round x)))
              #:unless (= x w))
             (values (+ S (bytes-ref chnl-pxls (chnl-byte-idx x y))) (+ P 1)))]
          ['x ; else x quadrant
           (for*/fold ((S 0) (P 0))
             ((x (in-range 0 w))
              (y (in-value (- h/2 (/ (- ρ (* (- x w/2) cθ)) sθ))))
              #:unless (or (< y 0) (>= y h))
              (y (in-value (exact-round y)))
              #:unless (= y h))
             (values (+ S (bytes-ref chnl-pxls (chnl-byte-idx x y))) (+ P 1)))]))
      
      (when (> P 0)
        (define idx (+ (* ρ 360) deg))
        (bytes-set! trg-pxls idx 255) ; make it opaque (α = 255)
        (bytes-set! trg-pxls idx (quotient S P))))))

;; does exception handling, and talking back to master process, leaving sub-hough-transform-channel to
;; do the biz.
(define (hough-transform-channel ch)
;; Message from on high is...
;; (list name s-w s-h t-chan t-h 0 360 s-chan)
  (match-define (list name w h trg-pxls trg-h deg-start deg-end chnl-pxls) (place-channel-get ch))
  (place-channel-put ch (cons name 'started))
  (with-handlers
      ;; "style" says to not handle the most general exception: but we're not expecting to generate
      ;; any, so wouldn't know any better would we?
      [(exn? (λ (x) (place-channel-put ch (cons name (exn-message x)))))]
    (sub-hough-transform-channel w h trg-pxls trg-h deg-start deg-end chnl-pxls)
    (place-channel-put ch (cons name #t))))
```


===Hough-transform-fast.rkt===

Note the block:


```racket
(require racket/require)
(require (filtered-in
          (λ (name) (regexp-replace #rx"unsafe-" name ""))
          racket/unsafe/ops))
#;(require racket/flonum racket/fixnum)
```


This uses unsafe, but fast, versions of <code>fx...</code> and <code>fl...</code> functions. If (especially during development and testing), one of these functions receives a non-{fix,flo}num value, then you could lose your interpreter... even DrRacket won't save you. 'Unsafe' means 'unsafe'! So until you are confident use:


```racket
(require racket/require)
#;(require (filtered-in
          (λ (name) (regexp-replace #rx"unsafe-" name ""))
          racket/unsafe/ops))
(require racket/flonum racket/fixnum)
```


Racket type checks and raises exceptions against <code>fx...</code> and <code>fl...</code> functions. Once these are purged, you can remove your safety harness!


```racket
#lang racket

;;; This module tries to be a little smarter about types. It *should* prove to be better performing
;;; than "Hough-transform-basic.rkt"; which spends a lot of time shuffling between the integer-based
;;; image coordinates and the floatier trignonmetric coordiantes. Here, we run two sets of coordinates
;;; (and variables). When a variable has two parallel values they are suffixed with '-x', for fixnum
;;; and suffixed with '-f' for float. As far as possible, they will be generated and manipulated as
;;; closely as possible to each other.

;;; At some point (after testing) we will require the unsafe versions of the fl... functions
(require racket/require)
(require (filtered-in
          (λ (name) (regexp-replace #rx"unsafe-" name ""))
          racket/unsafe/ops))
#;(require racket/flonum racket/fixnum)

(provide hough-transform-channel)
;; For these half-quadrants, it's better to iterate by 'y'
(define (iterate-quadrant deg) ; degrees are fixnum
  [cond
    [(fx< deg 45) 'y]
    [(fx< 315 deg) 'y]
    [(and (fx< 135 deg) (fx< deg 225)) 'y]
    [else 'x]])

(define (sub-hough-transform-channel w h trg-pxls trg-h deg-start deg-end chnl-pxls)
  ;; these constants appear repeatedly in the arithmetic
  (define w-f (fx->fl w))
  (define h-f (fx->fl h))
  (define w/2 (fl/ w-f 2.))
  (define h/2 (fl/ h-f 2.))
  
  (for ((deg (in-range deg-start deg-end)))
    ;; all these worth pre-computing...
    (define i-q (iterate-quadrant deg))
    ;; the fx->fl below catches 0 (since degrees->radians can say, exactly, that pi*0/180 is 0)
    (define θ (degrees->radians (fx->fl deg)))
    (define cθ (flcos θ))
    (define sθ (flsin θ))
    (define (chnl-byte-idx x y) (fx+ (fx* y w) x))
    (for* ((ρ-x (in-range 0 trg-h)))
      (define ρ-f (fx->fl ρ-x))
      ;; Sum the colors of the line with equation x*cos(θ) + y*sin(θ) = ρ
      (define-values (S P)
        (match i-q
          ['y
           ;; in Hough-transform-basic, these two loops were for*/fold, which the racket documentation
           ;; itself confesses can be a little slow. We'll run out own named let loop to do the work
           ;; for us here
           (let inr ((S 0) (P 0) (y-f 0.))
             (cond
               [(fl>= y-f h-f) (values S P)]
               [else
                (define x-f (+ w/2 (fl/ (fl- ρ-f (fl* (fl- h/2 y-f) sθ)) cθ)))
                (define y+1 (fl+ y-f 1.))
                (cond [(or (fl< x-f 0.) (fl>= x-f w-f)) (inr S P y+1)]
                      [else
                       (define x-x (exact-round x-f))
                       (cond [(= x-x w) (inr S P y+1)]
                             [else
                              (define y-x (exact-round y-f))
                              (define idx (chnl-byte-idx x-x y-x))
                              (inr (fx+ S (bytes-ref chnl-pxls idx)) (fx+ P 1) y+1)])])]))]
          ['x ; else x quadrant
           (let inr ((S 0) (P 0) (x-f 0.))
             (cond
               [(fl>= x-f w-f) (values S P)]
               [else
                (define y-f (fl- h/2 (fl/ (fl- ρ-f (fl* (fl- x-f w/2) cθ)) sθ)))
                (define x+1 (fl+ x-f 1.))
                (cond [(or (fl< y-f 0.) (fl>= y-f h-f)) (inr S P x+1)]
                      [else
                       (define y-x (exact-round y-f))
                       (cond [(= y-x h) (inr S P x+1)]
                             [else
                              (define x-x (exact-round x-f))
                              (define idx (chnl-byte-idx x-x y-x))
                              (inr (fx+ S (bytes-ref chnl-pxls idx)) (fx+ P 1) x+1)])])]))]))
      
      (when (fx> P 0)
        (define idx (fx+ (fx* ρ-x 360) deg))
        (bytes-set! trg-pxls idx 255) ; make it opaque (α = 255)
        (bytes-set! trg-pxls idx (fxquotient S P))))))


;; does exception handling, and talking back to master process, leaving sub-hough-transform-channel to
;; do the biz.

;; Message from on high is...
;; (list name s-w s-h t-chan t-h 0 360 s-chan)
(define (hough-transform-channel ch)
  (match-define (list name w h trg-pxls trg-h deg-start deg-end chnl-pxls) (place-channel-get ch))
  (place-channel-put ch (cons name 'started))
  (with-handlers
      ;; "style" says to not handle the most general exception: but we're not expecting to generate
      ;; any, so wouldn't know any better would we?
      [(exn? (λ (x) (place-channel-put ch (cons name (exn-message x)))))]
    (sub-hough-transform-channel w h trg-pxls trg-h deg-start deg-end chnl-pxls)
    (place-channel-put ch (cons name #t))))
```


===Hough-transform.rkt===

This is the main program, delegating to the module above. Ignoring all of the the image processing for now, you should be able to use the <code>(let wait-places ...)</code> loop more generally.

Note that:
 (place-channel-put plce (list c-name s-w s-h t-chan t-h 0 360 s-chan))
in this file, is reflected by:
  (match-define (list name w h trg-pxls trg-h deg-start deg-end chnl-pxls) (place-channel-get ch))
in the submodules, which in turn is matched by:
  (define (sub-hough-transform-channel w h trg-pxls trg-h deg-start deg-end chnl-pxls) ...)


```racket
#lang racket
;;; Derived from a port of TCL (hence the colouring of the output)

;;; This seems to be quite hard work for racket's number type pyramid, since we cast from coordinates
;;; (integers) to polar coordinates (floats) and back to coordinates.
;;;
;;; The three modules experiment with three implementations.
;;; basic - the first is a transcription of the TCL module. I could understand that better than most
;;;         of the others!
;;; fast  - does manual type-casting and unleashes the unsafe-fx and unsafe-fl functionality.
;;;         But it's a bit seat of the pants!
;;;
;;; In all three cases, we separate the image into channels. This simplifies the transformation
;;; function to just one channel (and the alpha channel for aesthetics); but also allows for
;;; parallelisation, maybe even distributon with "place"s.

;;; --------------------------------------------------------------------------------------------------
;;; GLOBALS
;;; --------------------------------------------------------------------------------------------------
(define IMAGE-DEPTH 4) ; the image depth ARGB bytes in a byte array
;; the Python version works over only one channel; that's obviously three times faster than doing the
;; three colour channels... but works for the monochrome example in the task. Use this to transform
;; all channels independently.
(define USE-CHANNELS? (if #t #(#f #t #t #t) #(#f #t #f #f)))
(define CHANNEL-NAMES #(α red green blue))
(define BACKGROUND-COLOUR (shared-bytes 255 192 255 192)); A colour to show the unmodified background
;; Function to transform an original image file name to it's "houghed" name
(define (hough-output-file-name source-filename)
  (path-replace-suffix (string->path source-filename) "-Hough.png"))

(define-logger hough-transform)
(current-logger hough-transform-logger)

;;; --------------------------------------------------------------------------------------------------
;;; MAIN
;;; Whatever happens, we'll need to read a file, transform it and write a file...
;;; --------------------------------------------------------------------------------------------------

;; This module contains the necessary for bitmap handling, file handling and place-farming. So the
;; transformation functions will be handling "bytes", shared byte vectors of (A R G B ...) bytes.
(require racket/draw)

;; split the bitmap (and its bytes) into channels. As we do this, we generate some interesting data
;; which we will pass back to our caller.
(define (bitmap->channel-bytes&c bmp use-chnls?)
  (define w (send bmp get-width))  
  (define h (send bmp get-height))
  (define sz (* w h IMAGE-DEPTH))
  (define bmp-bytes (make-bytes sz))
  (define (extract-channel offset)
    (define chnl (make-shared-bytes (/ sz IMAGE-DEPTH)))
    (for ((i (in-naturals)) (b (in-bytes bmp-bytes offset sz IMAGE-DEPTH))) (bytes-set! chnl i b))
    (values offset chnl))
  (send bmp get-argb-pixels 0 0 w h bmp-bytes #f #f)
  (define-values (offsets channels)
    (for/lists (offsts chnls) ((offset (in-naturals)) (wanted? use-chnls?) #:when wanted?)
      (extract-channel offset)))
  (values w h sz offsets channels))

;; Prepare our source and destination byte arrays and bitmaps. Farm them out to channel-transforming
;; places
(define (hough-transform-image source-bitmap place-module xform-function-name)
  (define-values (s-w s-h s-sz channel-offsets s-channels)
    (bitmap->channel-bytes&c source-bitmap USE-CHANNELS?))
  (define t-h (round (/ (sqrt (+ (sqr s-w) (sqr s-h))) 2)))
  (define t-w 360) ; degrees
  (define t-sz (* t-h t-w))
  ; prepare the target channels...
  (define t-channels (for/list ((init BACKGROUND-COLOUR)) (make-shared-bytes t-sz init)))
  
  (define channel-xform-places-chs
    (for/list
        ((offset channel-offsets)
         (s-chan s-channels))
      (define c-name (vector-ref CHANNEL-NAMES offset))
      (define t-chan (list-ref t-channels    offset))
      (define plce (dynamic-place place-module xform-function-name))
      (place-channel-put plce (list c-name s-w s-h t-chan t-h 0 360 s-chan))
      (log-info "wait place start... ")
      (let ((go (place-channel-get plce))) (log-info "~a" go))
      plce))
  
  (let wait-places ((chs channel-xform-places-chs))
    (unless (null? chs)
      (log-info "Wait ~a places... " (length chs)) (flush-output)
      (define evt-wraps
        (for/list ((ch chs) (i (in-naturals)))
          (wrap-evt
           ch
           (λ (v)
             (log-info "place #~a: ~a" i v)
             (define-values (L R) (split-at chs i))             
             (wait-places (append L (cdr R)))))))
      (apply sync evt-wraps)))
  
  ;; put Humpty together again
  (define t-bytes (make-bytes (* t-sz IMAGE-DEPTH)))
  (for ((offset (in-naturals))
        (t-chan t-channels))
    (for ((t-idx (in-range t-sz))
          (b-idx (in-range offset (* t-sz IMAGE-DEPTH) IMAGE-DEPTH)))
      (bytes-set! t-bytes b-idx (bytes-ref t-chan t-idx))))
  (define target-bitmap (make-object bitmap% 360 t-h #f #t))
  (send target-bitmap set-argb-pixels 0 0 360 t-h t-bytes)
  target-bitmap)

(define (transform-image source-file-name place-module (xform-function-name 'hough-transform-channel))
  (define source-image (make-object bitmap% source-file-name))
  (define target-image (hough-transform-image source-image place-module xform-function-name))
  (send target-image save-file (hough-output-file-name source-file-name) 'png)
  target-image)

;;; --------------------------------------------------------------------------------------------------
(module+ main
  (transform-image "180px-Pentagon.png" "Hough-transform-basic.rkt")
  (transform-image "180px-Pentagon.png" "Hough-transform-fast.rkt")
  )
```

