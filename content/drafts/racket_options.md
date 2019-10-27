+++
title = "Racket/Options"
description = ""
date = 2013-04-25T06:25:01Z
aliases = []
[extra]
id = 13363
[taxonomies]
categories = []
tags = []
+++

This code is a small library for dealing with options file, used in [[Update a configuration file]] and [[Read a configuration file]].  It is assumed to be in a file called "options.rkt".


```Racket

#lang racket

(provide read-options write-options define-options ENABLE)

;; holds the verbatim line, includes empty lines
(struct comment (contents))
;; can gold a string or #t for no specified value
(struct option (name [value #:mutable] [disabled? #:mutable]))

;; a convenient global parameter to hold the current options
(define current-options (make-parameter '()))

;; reads and normalizes an options file
(define (read-options file)
  (define (parse raw-line)
    (define line (string-trim raw-line))
    (match line
      [(regexp #px"^(?:(?:#.*)?)$") (comment line)] ; includes empty lines
      [(regexp #px"^(;+)?\\s*([^;\\s]+)(?:(?:\\s*=\\s*|\\s+)(\\S.*))?$"
               (list _ dis name val))
       (option (string-upcase name)
               (or (not val)
                   (let* ([val (string-split val #px"\\s*,\\s*")]
                          [val (if (null? (cdr val)) (car val) val)])
                     (or (equal? "" val) val))) ; "" same as unspecified
               (and dis #t))]
      [(regexp #px"^;") #f] ; discard these lines
      [_ (error 'read-options "invalid line in options file: ~s" line)]))
  (define (same-option? x y)
    (and (option? x) (option? y) (equal? (option-name x) (option-name y))))
  (current-options (remove-duplicates (filter-map parse (file->lines file))
                                      same-option?)))

(define (write-options file)
  (with-output-to-file file #:exists 'truncate
    (λ() (for ([o (current-options)])
           (if (comment? o)
             (displayln (comment-contents o))
             (let ([v (option-value o)])
               (when (option-disabled? o) (display "; "))
               (display (option-name o))
               (when (string? v) (printf " ~a" v))
               (newline)))))))

;; a special constant that is used with `opt-set!' to just enable an option
(define ENABLE (gensym))

;; convert any value into #t (no specific value), #f (missing/disabled),
;; a string, or a list of these; #t is also for an empty list
(define (->val val)
  (define (-> x) (if (boolean? x) x (~a x)))
  (cond [(list? val) (and (pair? val) (map -> val))]
        [(eq? ENABLE val) #t]
        [else (-> val)]))

;; returns a value, #t for no-value-specified and #f for disabled or missing
(define (opt-ref name)
  (define NAME (string-upcase (~a name)))
  (define opt (for/or ([o (current-options)])
                (and (option? o) (equal? NAME (option-name o)) o)))
  (and opt (not (option-disabled? opt)) (option-value opt)))

;; use #f to disable, #t for a no-value, or a plain string; use a
;; special ENABLE constant to enable a previously disabled value
(define (opt-set! name val)
  (define NAME (string-upcase (~a name)))
  (define opt (for/or ([o (current-options)])
                (and (option? o) (equal? NAME (option-name o)) o)))
  (define (add-option o)
    ;; a separator line, and then the new option
    (current-options `(,@(current-options) ,(comment "") ,o)))
  (cond [(not (or opt val)) (void)] ; disable nonexistent => do nothing
        [(not opt) (add-option (option NAME (->val val) (not val)))]
        [(not val) (set-option-disabled?! opt #t)] ; preserves old value if any
        [else (set-option-disabled?! opt #f)
              (unless (eq? val ENABLE) (set-option-value! opt (->val val)))]))

;; make it possible to treat options as plain variables
(define-syntax-rule (define-options name ...)
  (begin (define-syntax name
           (syntax-id-rules (set!)
             [(set! _ val) (opt-set! 'name val)]
             [(_ . xs)     ((opt-ref 'name) . xs)]
             [_            (opt-ref 'name)]))
         ...))

```

