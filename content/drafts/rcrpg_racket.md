+++
title = "RCRPG/Racket"
description = ""
date = 2015-11-09T08:19:27Z
aliases = []
[extra]
id = 19745
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
This [[Racket]] version of [[:Category:RCRPG|RCRPG]] uses standard in, out and error.
Error reports come out on the current error port, so if someone wanted to wrap it, they'd
be able to tell what's a happy response and what's not.

All the commands listed on the blog post are implemented, as well as look and unequip.

There's plenty of scope for emitting things in natural (even internationalised) language.
But I've not gone down that long and windy path. I've been studiously lazy about it. All
the items begin with consonants, so no need for a/an descriminating; and why count
"no ladders", "a ladder", "two ladders"... when you can just say "a ladder", "a ladder
and a ladder", "a ladder, a ladder and a ladder"? (Plus this way who cares if gold is
countable or not)

==Code==

```racket
#lang racket
(require racket/stxparam)

(struct R (walls contents name))

(struct P (position inventory equipment))

(struct G (rooms player dictionary over?))

(define (raise/fmt fmt . args)
  (raise (apply format args)))

(define all-dirs '("north" "east" "up" "south" "west" "down"))

(define (opposite-direction d)
  (cond [(member d all-dirs) => (λ (d-t) (fourth (append d-t all-dirs)))] [else #f]))

(define current-room-contents-generator
  (make-parameter
   (match-lambda
     [(vector x y z) (match (random 5) [0 '("ladder")] [1 '("gold")] [2 '("sledge")] [_ null])])))

(define nat-lang-join-list
  (match-lambda [(list x) x] [(list xs ... xn) (format "~a and ~a" (string-join xs ", ") xn)]))

(define (show-contents something nothing)
  (match-lambda
    [(list) nothing]
    [(list (app (curry format "a ~a") c) ...); good enough for now           
     (format "~a ~a" something (nat-lang-join-list c))]))

(define (new-room entry pos)
  (R (remove (opposite-direction entry) all-dirs) ((current-room-contents-generator) pos) #f))

(define game-player-room (match-lambda [(G rms (P p _ _) _ _) (dict-ref rms p)]))

(define game-player-room/pos (match-lambda [(G rs (P p _ _) _ _) (values (dict-ref rs p) p)]))

(define (game-set-player-room g r+)
  (define-values (r- rpos) (game-player-room/pos g))
  (struct-copy G g [rooms (dict-set (G-rooms g) rpos r+)]))

(define (make-goer dir dx dy dz (validate-room void))
  (lambda (g t0 . more-tokens)
    (match-define (and (G rms plyr _ _) (app game-player-room/pos r (vector x y z))) g)
    (when (member dir (R-walls r)) (raise/fmt "there is no passage ~a" dir))
    (validate-room r)
    (define pos+ (vector (+ x dx) (+ y dy) (+ z dz)))
    (printf "You go ~a.~%" dir)
    (struct-copy G g (player (struct-copy P plyr (position pos+)))
                 [rooms (if (dict-ref rms pos+ #f)
                            rms (dict-set rms pos+ (new-room dir pos+)))])))

;; all commands are ((game cmd-token more-tokens) -> game+) ; or raise a string error
(define (room-has-ladder? r)
  (unless (member "ladder" (R-contents r))
    (raise "there is no ladder here")))

(define (go direction)  
  (match direction
    ["north" (make-goer "north" 0 -1  0)]
    ["south" (make-goer "south" 0  1  0)]
    ["east"  (make-goer "east"  1  0  0)]
    ["west"  (make-goer "west" -1  0  0)]
    ["down"  (make-goer "down"  0  0 -1)]
    ["up"    (make-goer "up"    0  0  1 room-has-ladder?)]))

(define (command-arity-check command arity more-tokens)
  (when arity
    (unless (= (sub1 arity) (length more-tokens))
      (raise/fmt "~a needs exactly ~a words" command arity))))

(define-syntax-parameter g (λ (stx) (raise-syntax-error "g used outside of cmd")))
(define-syntax-parameter tokens (λ (stx) (raise-syntax-error "tokens used outside of cmd")))

(define-syntax (cmd stx)
  (syntax-case stx ()
    [(_ name (defs ...) match-clauses ...) #'(cmd name 1 (defs ...) match-clauses ...)]
    [(_ name expected-arity (defs ...) match-clauses ...)
     #'(define (name gm command . tkns)
         (syntax-parameterize
          ([g (syntax-id-rules () [_ gm])]
           [tokens (syntax-id-rules () [_ tkns])])
          (match-let (defs ...)
            (command-arity-check command expected-arity tokens)
            (match g match-clauses ...))))]))

(cmd alias 3 ([(list a c) tokens])
     [(G _ _ d _)
      (define o (dict-ref d c #f))
      (unless o (raise "old name was not a command"))
      (struct-copy G g [dictionary (dict-set d a o)])])

(cmd name #f ([(list name-bits ...) tokens])
     [(app game-player-room r)
      (define n (and (not (null? name-bits)) (string-join name-bits)))
      (game-set-player-room g (struct-copy R r [name n]))])

(cmd look ()
     [(app game-player-room (and r (R ws cs nm)))
      (printf "You are in ~a:~%" (or nm "a room like many others"))
      (match (set-subtract all-dirs ws)
        [(list) (printf "There are no exits.~%")]
        [(list exit) (printf "There is an exit ~a.~%" exit)]
        [(list exits ...) (printf "There are exits ~a.~%" (nat-lang-join-list exits))])
      (displayln ((show-contents "You see:" "There is nothing here.") cs))
      g])

(cmd inventory ()
     [(G _ (P _ inv eqp) _ _)
      (define full-inv (if eqp (append inv (list (format "~a (equipped)" eqp))) inv))
      (displayln ((show-contents "You have:" "You have nothing") inv))
      g])

(cmd equip ([(list item) tokens])
     [_ #:when (equal? item "gold")
        (raise/fmt "No! The task description says that ~a is useless" item)]
     [(G _ (P _ _ eqp) _ _)#:when (equal? eqp item) (raise "what's wrong with the one you've got?")]
     [(G _ (P _ (list (not (== item)) ...) _) _ _) (raise/fmt "you don't have a ~a" item)]
     [(G _ (and (P _ (list-no-order (== item) inv-remain ...) #f) p) _ _)
      (printf "You wield the ~a from your pack.~%" item)
      (define over? (equal? item "prize"))
      (struct-copy G g
                   [player (struct-copy P p [equipment item] [inventory inv-remain])]
                   [over? over?])])

(cmd unequip ()
     [(G _ (P _ _ #f) _ _) (raise "you have nothing equipped")]
     [(G _ (and p (P _ inv eqp)) _ _)
      (struct-copy G g [player (struct-copy P [inventory (cons eqp inv)] [equipment #f])])])

(cmd take 2 ([(list item) tokens])
     [(and (G _ (and p (P _ inv _)) _ _)
           (app game-player-room (and r (R _ (and cnts (list _ ..1)) _))))
      #:when (equal? item "all")
      (printf "You pick up the ~a~%" (nat-lang-join-list cnts))
      (struct-copy G (game-set-player-room g (struct-copy R r (contents null)))
                   (player (struct-copy P p (inventory (append cnts inv)))))]
     [_ #:when (equal? item "all") (raise "there is nothing in the room")]    
     [(and (G _ (and p (P _ inv _)) _ _)
           (app game-player-room (and r (R _ (list-no-order (== item) more-cnts ...) _))))
      (printf "You pick up the ~a~%" item)
      (struct-copy G (game-set-player-room g (struct-copy R r (contents more-cnts)))
                   (player (struct-copy P p (inventory (cons item inv)))))]
     [_ (raise/fmt "There is no ~a in the room" item)])

(cmd drop 2 ([(list item) tokens])
     [(G _ (P _ (list) #f) _ _) #:when (equal? item "all") (raise "you have nothing to drop")]
     [(and (G _ (and p (P _ (list inv ..1) eqp)) _ _)
           (app game-player-room (and r (R _ cnts _))))
      #:when (equal? item "all")
      (printf "You drop the ~a from you pack.~%" (nat-lang-join-list inv))
      (when eqp (printf "You drop the ~a in your hand.~%" eqp))
      (define dropped (if eqp (cons eqp inv) inv))
      (struct-copy G (game-set-player-room g (struct-copy R r [contents (append dropped cnts)]))
                   (player (struct-copy P p [inventory null] [equipment #f])))]
     
     [(and (G _ (and p (P _ _ (== item))) _ _) (app game-player-room (and r (R _ cnts _))))
      (printf "You drop you currently equipped ~a.~%" item)
      (struct-copy G (game-set-player-room g (struct-copy R r [contents (cons item cnts)]))
                   (player (struct-copy P p (equipment #f))))]
     [_ (raise/fmt "you have no ~a" item)])

(cmd attack 2 ([(list dir) tokens])
     [_ #:when (not (member dir all-dirs)) (raise/fmt "~s is not even a real direction" dir)]
     
     [(and (G _ (P _ _ "sledge") _ _)
           (app game-player-room (and r (R (list-no-order (== dir) more-walls ...) _ _))))
      (printf "thwap! you smash a hole ~a~%" dir)
      (game-set-player-room g (struct-copy R r [walls more-walls]))]
     
     [(and (G _ (P _ (list-no-order "sledge" _ ...) _) _ _) _)
      (raise "you have no sledge equipped (but you have one in your pack)")]
     
     [(and (G _ (P _ _ "sledge") _ _)
           (app game-player-room (R (list-no-order (not (== dir)) ...) _ _)))
      (raise/fmt "you swing wildly ~a... but there is already a hole there" dir)]
     
     [_ (raise "no sledge, no wall... how disappointing")])

;; -| GAME PLAY |-------------------------------------------------------------------------------------
(define g0
  (G (hash
      #(1 1 5) (R all-dirs (list "prize") "Prize Room")
      #(5 5 -5) (R all-dirs (list "sledge") "the very first room"))
     (P #(5 5 -5) null #f)
     (apply
      hash
      `(,@(append* (map (λ (n) (list n (go n))) all-dirs))
        "attack" ,attack
        "drop" ,drop
        "take" ,take
        "inventory" ,inventory
        "name" ,name
        "equip" ,equip
        "unequip" ,unequip
        "alias" ,alias
        "look" look ; look isn't a standard RCRPG command
        ))
     #f))

(define (turn g cmd)
  (match (string-split cmd)
    [(list) g]
    [(list command more ...)
     (define f (dict-ref (G-dictionary g) command
                         (λ () (raise/fmt "command ~s not found" command))))
     (apply f g command more)]))

(define (show-prompt g)
  (define ppos (P-position (G-player g)))
  (look g "look")
  (printf "~a> " ppos)
  (flush-output))

(define (play g)
  (show-prompt g)
  (define i (read-line))
  (if (eof-object? i)
      (displayln "bye!")
      (let ((g+ (with-handlers ([string? (λ (s) (eprintf "oh dear... ~a~%" s) g)])
                  (begin0 (turn g i) (newline)))))
        (if (G-over? g+) (displayln "well done! bye!") (play g+)))))

(module+ main
  (play g0))
```

