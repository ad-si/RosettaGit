+++
title = "Execute SNUSP/Racket"
description = ""
date = 2014-06-11T17:31:04Z
aliases = []
[extra]
id = 14218
[taxonomies]
categories = []
tags = []
+++

{{implementation|SNUSP}}{{collection|RCSNUSP}}

Interpreter for Modular (and, maybe, Bloated) [[SNUSP]].

Looking at the split example, it seems I don't understand one of threads or read!

But have a go and confirm if split works as '''you'd''' expect.

I've implemented an "abstract" esoteric machine which encompasses the ability to run a BF, a Funge and SNUSP.

'''esoteric.rkt'''

```racket
#lang racket
;;;; This file defines all identifiers which are generally useful for
;;;; the kind of machine you'll find on esolangs.org (SNUSP, Funges, BFs)
(provide (struct-out pointer-2d)
         (struct-out machine)
         (struct-out m/c-thread)
         (struct-out m/c-cursor)
         
         machine-instruction
         m/c-cursor-direction-updater
         m/c-thread-update-cursor
         
         debugging? ; suggest require this with a prefix
         memory-out-of-bounds?
         memory-unbounded-longhand
         
         memory-default-value
         oob-reporter
         memory-overflow
         valid-data-value?
         
         instruction-space-padding
         
         get-memory
         set-memory
         update-memory
         
         normalise-instruction-set)

;                                                                        
;                                                                        
;    ;;;;    ;                           ;                               
;   ;    ;   ;                           ;                               
;   ;      ;;;;;   ;;;;  ;   ;   ;;;   ;;;;;  ;   ;   ;;;;   ;;;    ;;;  
;   ;;       ;     ;;  ; ;   ;  ;;  ;    ;    ;   ;   ;;  ; ;;  ;  ;   ; 
;    ;;;;    ;     ;     ;   ;  ;        ;    ;   ;   ;     ;   ;  ;     
;        ;   ;     ;     ;   ;  ;        ;    ;   ;   ;     ;;;;;   ;;;  
;        ;   ;     ;     ;   ;  ;        ;    ;   ;   ;     ;          ; 
;   ;    ;   ;     ;     ;   ;  ;;       ;    ;   ;   ;     ;   ;  ;   ; 
;    ;;;;    ;;;   ;      ;;;;   ;;;;    ;;;   ;;;;   ;      ;;;    ;;;  
;                                                                        
;                                                                        
;                                                                        

(define-struct pointer-2d (r c) #:prefab)

;; state that can be pushed onto a stack
(define-struct m/c-cursor (p2d direction) #:prefab)

;; I stands for "instruction"
;; M stands for "data" (memory)
;; stack - list of m/c-cursor (excluding current cursor)
(define-struct m/c-thread (id M-p2d csr stack) #:prefab)

(define-struct machine (data prog threads) #:prefab)

(define ((m/c-cursor-direction-updater upd-dir) csr)
  (struct-copy m/c-cursor csr
               (direction (upd-dir (m/c-cursor-direction csr)))))

(define (m/c-thread-update-cursor T csr-updater)
  (struct-copy m/c-thread T (csr (csr-updater (m/c-thread-csr T)))))

;                                                                        
;                                                                        
;   ;;;;;                                       ;                        
;   ;    ;                                      ;                        
;   ;    ;  ;;;    ;;;;   ;;;   ;;;;;   ;;;   ;;;;;   ;;;    ;;;;   ;;;  
;   ;    ; ;   ;   ;;  ; ;   ;  ; ; ;  ;;  ;    ;    ;;  ;   ;;  ; ;   ; 
;   ;;;;;      ;   ;         ;  ; ; ;  ;   ;    ;    ;   ;   ;     ;     
;   ;       ;;;;   ;      ;;;;  ; ; ;  ;;;;;    ;    ;;;;;   ;      ;;;  
;   ;      ;   ;   ;     ;   ;  ; ; ;  ;        ;    ;       ;         ; 
;   ;      ;   ;   ;     ;   ;  ; ; ;  ;   ;    ;    ;   ;   ;     ;   ; 
;   ;       ;;;;   ;      ;;;;  ; ; ;   ;;;     ;;;   ;;;    ;      ;;;  
;                                                                        
;                                                                        
;                                                                        
(define debugging? (make-parameter #f))

;;; Memory model:
;;;  memory can be bounded in all directions
;;;  bounds are specified by the following coordinates

;;; generally, min is inclusive and max is exclusive
;;; not really relevant for +/- inf.0, though
(define (memory-unbounded-longhand r c)
  (or (< r -inf.0) (>= r +inf.0) (< c -inf.0) (>= c +inf.0)))

(define memory-out-of-bounds? (make-parameter (lambda (r c) #f)))

;; defauts for memory
(define memory-default-value (make-parameter 0))
(define (oob-reporter caller rc) (error caller "out of bounds: ~s" rc))
(define memory-overflow oob-reporter)

; or throw an error
(define valid-data-value? (make-parameter (thunk* #t)))

;; best bet to ensure this is not an instruction character ... but pretty when used to
;; print the instructions space out (if needed)
(define instruction-space-padding (make-parameter #\space))

;                                            
;                                            
;   ;    ;                                   
;   ;;  ;;                                   
;   ;;  ;;  ;;;   ;;;;;   ;;;    ;;;;  ;   ; 
;   ; ;; ; ;;  ;  ; ; ;  ;   ;   ;;  ; ;   ; 
;   ; ;; ; ;   ;  ; ; ;  ;   ;   ;      ; ;  
;   ; ;; ; ;;;;;  ; ; ;  ;   ;   ;      ; ;  
;   ;    ; ;      ; ; ;  ;   ;   ;      ; ;  
;   ;    ; ;   ;  ; ; ;  ;   ;   ;      ;;   
;   ;    ;  ;;;   ; ; ;   ;;;    ;       ;   
;                                        ;   
;                                       ;    
;                                      ;;    

; INSTRUCTION AND DATA MEMORIES


(define (machine-instruction M r.c)
  (define P (machine-prog M))
  (define r (pointer-2d-r r.c))
  (define c (pointer-2d-c r.c))
  (define row-in-bounds? (< -1 r (length P)))
  (define col-in-bounds? (and row-in-bounds? (< -1 c (length (car P)))))
  (and row-in-bounds? col-in-bounds? (list-ref (list-ref (machine-prog M) r) c)))

(define get-memory
  (case-lambda
    ((D rc)
     (get-memory D (pointer-2d-r rc) (pointer-2d-c rc)))
    ((D r c)
     (if ((memory-out-of-bounds?) r c)
         ((memory-overflow) 'data r c)         
         (hash-ref D (pointer-2d r c) (memory-default-value))))))

(define set-memory
  (case-lambda
    ((D rc v)
     (set-memory D (pointer-2d-r rc) (pointer-2d-c rc) v))
    ((D r c v)
     (if ((memory-out-of-bounds?) r c)
         ((memory-overflow) 'data r c)
         (hash-set D (pointer-2d r c) v)))))

(define update-memory
  (case-lambda
    ((D rc f)
     (update-memory D (pointer-2d-r rc) (pointer-2d-c rc) f))
    ((D r c f)
     (let* ((v (get-memory D r c)) (v* (f v)))
       (when ((valid-data-value?) v*)
         (hash-set D (pointer-2d r c) v*))))))


;                                                                        
;                                                      ;                 
;                                                             ;          
;                                                             ;          
;    ;;;    ;;;;   ;;;           ;;;    ;;;   ; ;;   ;;;    ;;;;;  ;   ; 
;   ;   ;   ;;  ; ;;  ;         ;   ;  ;   ;  ;;  ;    ;      ;    ;   ; 
;   ;       ;     ;             ;          ;  ;   ;    ;      ;     ; ;  
;    ;;;    ;     ;              ;;;    ;;;;  ;   ;    ;      ;     ; ;  
;       ;   ;     ;                 ;  ;   ;  ;   ;    ;      ;     ; ;  
;   ;   ;   ;     ;;            ;   ;  ;   ;  ;   ;    ;      ;     ;;   
;    ;;;    ;      ;;;;          ;;;    ;;;;  ;   ;  ;;;;;    ;;;    ;   
;                                                                    ;   
;                                                                   ;    
;                                                                  ;;    

(define (all-list-lengths=? l) (or (null? l) (apply = (map length l))))
(define rectangular-list?
  (and/c (listof list?) (flat-contract all-list-lengths=?)))

;; returns the maximum width of l and whether it is rectangular (i.e. doesn't need padding)
(define (list-rectangular?/width L)
  (for*/fold
      ((max-l #f) (rectangular? #t))
    ((r (in-list L))
     (l (in-value (length r)))) ;; TODO: RENAME TO `len`
    (cond [(not max-l) (values l rectangular?)]
          [(= l max-l) (values l rectangular?)]
          [else (values (max l max-l) #f)])))

(define ((pad-to-width width padding) r)
  (append r (make-list (- width (length r)) padding)))  
(define (pad-right-list L width padding)
  (map (pad-to-width width padding) L))

;; turns a sequence of seqences of characters (e.g. a list of strings) into a
;; rectangular-list? of characters
(define (normalise-instruction-set I)
  (define I-chars (sequence->list (sequence-map sequence->list I)))
  (define-values (I-max-width I-rectangular?) (list-rectangular?/width I-chars))  
  (if I-rectangular?
      I-chars
      (normalise-instruction-set
       (pad-right-list I-chars I-max-width (instruction-space-padding)))))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;  ;        ;    ;     
;     ;    ;;;;;   ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;   ;  ;   ;    ;    ;   ; 
;     ;;;   ;;;    ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

[module+ test
  ;;; prepare the unit testing module
  (require rackunit)
  (require rackunit/text-ui)
  
  (define-test-suite ts:rectangular-list?
    (check-pred rectangular-list?
                (normalise-instruction-set
                 '("
### ====
"
                   "
### ====
"
                   "
### ====
"
                   "
### ====
")))
    (check-pred rectangular-list?
                (normalise-instruction-set
                 '("
### =
"
                   "
### ==
"
                   "
### ====
"
                   "======")))
    (check-pred rectangular-list?
                (parameterize ((instruction-space-padding #\X))
                  (normalise-instruction-set
                   '("
### =
"
                     "
### ==
"
                     "
### ====
"
                     "======")))))
  
  (define-test-suite ts:esoteric-machine
    ts:rectangular-list?)
  (run-tests ts:esoteric-machine)
  ]
```


'''SNUSP.rkt'''

```racket
#lang racket
(require "esoteric-machine.rkt")

(define SNUSP-logger (make-logger 'snusp (current-logger)))
(define root-logger (current-logger)) ; in case you need it
(current-logger SNUSP-logger)

;;; Snusp References:
;;;   http://esoteric.voxelperfect.net/files/snusp/doc/snusp-1.0-spec-wd1.pdf

;;; SNUSP language levels
(define snusp-modular? (make-parameter #t))
(define snusp-bloated? (make-parameter #t))

;                                                                 
;                                                                 
;    ;;;;  ;;   ; ;    ;  ;;;;  ;;;;;         ;    ;      ;   ;;; 
;   ;    ; ;;   ; ;    ; ;    ; ;    ;        ;;  ;;     ;   ;   ;
;   ;      ; ;  ; ;    ; ;      ;    ;        ;;  ;;     ;  ;     
;   ;;     ; ;  ; ;    ; ;;     ;    ;        ; ;; ;    ;   ;     
;    ;;;;  ; ;; ; ;    ;  ;;;;  ;;;;;         ; ;; ;    ;   ;     
;        ; ;  ; ; ;    ;      ; ;             ; ;; ;   ;    ;     
;        ; ;  ; ; ;    ;      ; ;             ;    ;   ;    ;     
;   ;    ; ;   ;; ;    ; ;    ; ;             ;    ;  ;      ;   ;
;    ;;;;  ;   ;;  ;;;;   ;;;;  ;             ;    ;  ;       ;;; 
;                                                    ;            
;                                                                 
;                                                      

;; finds the first $, otherwise return 0,0
(define (snusp-start-coordinates I)
  (for*/fold ((r 0) (c 0))
    (((rw rn) (in-parallel I (in-naturals)))
     ((cl cn) (in-parallel rw (in-naturals)))
     #:when (char=? cl #\$))
    #:final #t
    (values rn cn)))

(define (new-snusp-machine I)
  (define instructions (normalise-instruction-set I))
  (define-values (start-r start-c) (snusp-start-coordinates I))
  (define initial-thread
    (make-m/c-thread (gensym 'm/c-thread) (pointer-2d 0 0) (m/c-cursor (pointer-2d start-r start-c) #\>) null))
  (machine (hash) instructions (list initial-thread)))

;; Directions are as per bloated snusp -- no need to map up to : etc...
;;   :
;;  < >
;;   ;
(define directions '(#\> #\; #\< #\:))
(define (LURD d) (case d ((#\<) #\:) ((#\:) #\<) ((#\>) #\;) ((#\;) #\>))) ; \
(define (RULD d) (case d ((#\>) #\:) ((#\:) #\>) ((#\<) #\;) ((#\;) #\<))) ; /

(define (snusp-move-p2d d rc)
  (case d
    ((#\:) (struct-copy pointer-2d rc (r (sub1 (pointer-2d-r rc)))))
    ((#\;) (struct-copy pointer-2d rc (r (add1 (pointer-2d-r rc)))))
    ((#\<) (struct-copy pointer-2d rc (c (sub1 (pointer-2d-c rc)))))
    ((#\>) (struct-copy pointer-2d rc (c (add1 (pointer-2d-c rc)))))
    (else rc)))

(define (snusp-thread-fwd T)
  (match T
    ((m/c-thread _ _ (and csr (m/c-cursor p2d dir)) _)
     (define csr* (struct-copy m/c-cursor csr (p2d (snusp-move-p2d dir p2d))))
     (struct-copy m/c-thread T (csr csr*)))))

(define (snusp-machine-tick M tick-nr)
  (define (ret-fwd t) ;; returns (list T) as a convenience
    (list (snusp-thread-fwd t)))
  (define (ret-NOOP t) (values M (list (snusp-thread-fwd t))))
  
  (log-debug "machine tick #~a" tick-nr)
  
  (define (handle-modular-instruction M T M-p2d I)
    (case I
      ;;; Modular
      [(#\@) ; enter “Push the current direction and IP location on the call-stack”           
       (values
        M
        (ret-fwd
         (struct-copy
          m/c-thread T
          (stack
           (cons
            (m/c-thread-csr T)
            (m/c-thread-stack T))))))]
      
      [(#\#) ; leave “Pop direction and IP location off call-stack and advance IP one step”
       (values
        M
        (match (m/c-thread-stack T)
          ('() null)
          ((cons stack-head stack-tail)
           (ret-fwd (snusp-thread-fwd
                     (struct-copy m/c-thread T
                                  (csr stack-head)
                                  (stack stack-tail)))))))]
      
      [else
       (if (snusp-bloated?) (handle-bloated-instruction M T M-p2d I)
           (ret-NOOP T))]))
  
  (define (handle-bloated-instruction M T M-p2d I)
    (case I
      [(#\: #\;) ; memory up-down (up/down is bloated behaviour)
       (values M (ret-fwd (struct-copy m/c-thread T (M-p2d (snusp-move-p2d I M-p2d)))))]
      
      [(#\%) ; rand
       (define (random-0-to-n n) (random (add1 n)))
       (define new-data (update-memory (machine-data M) M-p2d random-0-to-n))
       (values (struct-copy machine M (data new-data)) (ret-fwd T))]
      
      [(#\&) ; split
       ;; SPLIT moves the instruction pointer of the old thread one step forward,
       ;; so it is possible to distinguish the old thread from the new
       (define new-T (struct-copy m/c-thread T (id (gensym 'm/c-thread))))
       (values M (list (snusp-thread-fwd (snusp-thread-fwd T))
                       (snusp-thread-fwd new-T)))]
      ;; if it didn't happen in bloated, it's not going to happen!
      [else (ret-NOOP T)]))
  
  ;; returns the modified machine and a list of (modified) threads to
  ;; replace T with
  (define (handle-instruction M T M-p2d I)
    (case I
      ;;; Core
      [(#\< #\>) ; memory left/right (up/down is bloated behaviour)
       (values M (ret-fwd (struct-copy m/c-thread T (M-p2d (snusp-move-p2d I M-p2d)))))]
      [(#\+ #\-) ; memory inc/dec
       (define new-data (update-memory (machine-data M) M-p2d (if (char=? I #\+) add1 sub1)))
       (values (struct-copy machine M (data new-data)) (ret-fwd T))]                
      [(#\,) ; memory read I/O
       (define received-int (char->integer (read-char)))
       (define new-data (set-memory (machine-data M) M-p2d received-int))
       (values (struct-copy machine M (data new-data)) (ret-fwd T))]
      [(#\.) ; memory write I/O
       (write-char (integer->char (get-memory (machine-data M) M-p2d)))
       (values M (ret-fwd T))]
      [(#\\) ; LURD
       (values M (ret-fwd (m/c-thread-update-cursor T (m/c-cursor-direction-updater LURD))))]
      [(#\/) ; RULD
       (values M (ret-fwd (m/c-thread-update-cursor T (m/c-cursor-direction-updater RULD))))]
      [(#\!) ; skip
       (values M (ret-fwd (snusp-thread-fwd T)))]
      [(#\?) ; skipz
       (define do-skip? (zero? (get-memory (machine-data M) M-p2d)))
       (values M (ret-fwd (if do-skip? (snusp-thread-fwd T) T)))]      
      [else
       (cond [(snusp-modular?) (handle-modular-instruction M T M-p2d I)]
             [(snusp-bloated?) (handle-bloated-instruction M T M-p2d I)]
             [else (ret-NOOP T)])]))
  
  (define (m/c-thread-turn M T)
    (log-debug "thread-turn for ~a" (m/c-thread-id T))
    
    (define I (machine-instruction M (m/c-cursor-p2d (m/c-thread-csr T))))
    (when (debugging?) (log-debug "I@~a ~a" T I))
    (cond
      [(not I) ; I-p2d is OOB
       (log-debug "thread ~a terminated" (m/c-thread-id T))
       (values M null)]
      [else (handle-instruction M T (m/c-thread-M-p2d T) I)]))
  
  (define-values (ret-M new-T last-run-thread)
    (for/fold
        ((M M) (new-threads null) (last-run-thread #f))
      ((T (machine-threads M)))
      (define-values (M* T*) (m/c-thread-turn M T))
      (values M* (append new-threads T*) T)))
  
  (values (struct-copy machine ret-M (threads new-T))
          last-run-thread))

(define (execute-snusp-machine M tick-nr remaining-ticks (last-run-thread #f))
  (define-values (new-m/c last-run-thread) (snusp-machine-tick M tick-nr))
  (match* (new-m/c last-run-thread remaining-ticks)
    [((machine _    _ '()) #f _) (memory-default-value)]           
    [(_ #f 0) (memory-default-value)]           
    [((machine data _ _) (m/c-thread _ M-p2d _ _) 0) (get-memory data M-p2d)]           
    [((machine data _ '()) (m/c-thread _ M-p2d _ _) _) (get-memory data M-p2d)]           
    [(_ _ _) (execute-snusp-machine new-m/c (add1 tick-nr) (sub1 remaining-ticks))]))

(define (execute/new-snusp-machine source #:remaining-ticks (remaining-ticks +inf.0))
  (execute-snusp-machine
   (new-snusp-machine (regexp-split #rx"\n" source))
   0 remaining-ticks))

;; good for tests and demonstrations
(define (snusp-io-string I #:in-string (in-string ""))
  (with-output-to-string
   (λ () (with-input-from-string
          in-string
          (λ () (execute/new-snusp-machine I))))))

(define (snusp-val-string I #:in-string (in-string ""))
  (parameterize ((current-output-port (open-output-string))
                 (current-input-port (open-input-string in-string))) ; string is a sink
   (execute/new-snusp-machine I)))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;  ;        ;    ;     
;     ;    ;;;;;   ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;   ;  ;   ;    ;    ;   ; 
;     ;;;   ;;;    ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

[module+ test
  ;;; prepare the unit testing module
  (require rackunit)
  (require rackunit/text-ui)
  
  (define-simple-check (check-start-coordinates r c L)
    (let-values (((start-r start-c) (snusp-start-coordinates L)))
      (check-equal? (list r c) (list start-r start-c))))
  
  (define-test-suite ts:snusp-start-coordinates
    (check-start-coordinates 0 0 '((#\. #\. #\.)
                                   (#\. #\. #\.)
                                   (#\. #\. #\.)))
    
    (check-start-coordinates 1 1 '((#\. #\. #\.)
                                   (#\. #\$ #\.)
                                   (#\. #\. #\.)))
    
    (check-start-coordinates 2 2 '((#\. #\. #\.)
                                   (#\. #\. #\.)
                                   (#\. #\. #\$)))
    (check-start-coordinates 1 1 '((#\. #\. #\.)
                                   (#\. #\$ #\.)
                                   (#\. #\. #\$))))
  
  (define-test-suite ts:degenerate-m/c
    (check-not-exn (λ () (call-with-values (λ () (execute-snusp-machine (new-snusp-machine '()) 0 +inf.0))
                                           list))
                   "null machine works")
    
    (check-not-exn (λ () (call-with-values (λ () (execute/new-snusp-machine ""))
                                           list)) "empty machine works"))
  
  (check-equal? "x" (snusp-io-string ",." #:in-string "x"))
  
  ;; programs that create 48 will print a #\0
  (define prog-48/1 #<<EOS
++++++++++ ++++++++++ ++++++++++ ++++++++++ ++++++++ .
EOS
    )
  (define prog-48/2 #<<EOS
?#?.++++++++++++++++++++++++=!\\
                              \/
EOS
    )
  
  (define prog-48/6 #<<EOS
=@\.
  \=@@@+@+++++#
EOS
    )
  (define prog-print #<<EOS
$++++++++++++\
/
### ======
/
|        /recurse\    #/?\ zero
\=print=!\@\>?!\@/<@\.!\-/
           |   \=/  \=itoa=@@@+@+++++#
           !     /+ !/+ !/+ !/+   \    mod10
           /<+> -\!?-\!?-\!?-\!?-\!
           \?!\-?!\-?!\-?!\-?!\-?/\    div10
              #  +/! +/! +/! +/! +/
EOS
    )
  
  (define prog-hw/1 #<<EOS
      /@@@@++++#               #+++@@\                #-----@@@\n
$@\H.@/e.+++++++l.l.+++o.>>++++.< .<@/w.@\o.+++r.++@\l.@\d.>+.@/.#
  \@@@@=>++++>+++++<<@+++++#       #---@@/!
### ===
/!==/
EOS
    )
  (define prog-hw/2 #<<EOS
  H e l l o ,   w o r l d !
$@\@\@\@\@\@\@\@\@\@\@\@\@\#
  | | | | | | | | | | | | |
  |!|!|!|!|!|!|!|!|!|!|!|!|@@@+@-@@@+++# 128
  @ @ @ @ @ | | @ @ @ @ @ |
  \!\!\!\!\!|!|!\!\!\!\!\!|-@@+@@@@+++# 64
  | @ @ @ @ @ @ @ @ @ @ @ @
  |!\!\!\!\!\!\!\!\!\!\!\!\@@@-@++++# 32
  | | | | | | | @ | @ | | |
  |!|!|!|!|!|!|!\!|!\!|!|!|+@+@++++# 16
  @ | @ @ @ @ | | @ | @ | |
  \!|!\!\!\!\!|!|!\!|!\!|!|@@+++# 8
  | @ @ @ @ @ | @ @ | @ @ |
  |!\!\!\!\!\!|!\!\!|!\!\!|++++# 4
  | | | | @ | | @ @ @ | | |
  |!|!|!|!\!|!|!\!\!\!|!|!|++# 2
  | @ | | @ | | @ @ | | | @
  |!\!|!|!\!|!|!\!\!|!|!|!\+# 1
  | | | | | | | | | | | | |
  \!\!\!\!\!\!\!\!\!\!\!\!\.># print and move
EOS
    )
  
  ;; Two e.gs from:
  
  ;; esolangs.org/SNUSP this is "modular":
  
  ;; “This example from the SNUSP spec shows how to use the
  ;;  call-stack to define an ECHO subroutine and call it twice:”
  (define prog-echo/modular
    #<<EOS
       /==!/======ECHO==,==.==#
       |   |
$==>==@/==@/==<==#
EOS
    )
  
  (define-test-suite ts:48-adders
    (check-eq? (char->integer #\0) 48)     
    (check-equal? (snusp-io-string prog-48/1) "0")    
    (check-equal? (snusp-val-string prog-48/1) 48)    
    (check-equal? (snusp-io-string prog-48/2) "0")
    (check-equal? (snusp-io-string prog-48/6) "0"))
  
  (define-test-suite ts:print
    (check-equal? (snusp-io-string prog-print) "12"))
  
  (define-test-suite ts:hello-world
    (check-equal? (snusp-io-string prog-hw/1) "Hello, world!\n")    
    (check-equal? (snusp-io-string prog-hw/2) "Hello, world!"))
  
  (define-test-suite ts:echo
    (check-equal? (snusp-io-string prog-echo/modular #:in-string "ab") "ab"))
  
  ;; This is bloated (but deterministic!):
  ;; “The following example uses two threads to print ! until a key is pressed:”
  ;;
  ;; We're not going to test this, since the description suggest an asynchronous
  ;; approach to reading a character... which (read-char) can't really test so
  ;; well.
  (define prog-exclaimer/bloated
    #<<EOS
                    /==.==<==\
                    |        |
     /+++++++++++==&\==>===?!/==<<==#
     \+++++++++++\  |
$==>==+++++++++++/  \==>==,==#
EOS
    )
  
  (define-test-suite ts:all
    ts:snusp-start-coordinates
    ts:degenerate-m/c
    ts:48-adders
    ts:hello-world
    ts:print
    ts:echo)
  (run-tests ts:all)  
  ]
```

