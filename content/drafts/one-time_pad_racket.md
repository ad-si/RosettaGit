+++
title = "One-time pad/Racket"
description = ""
date = 2015-03-31T07:53:32Z
aliases = []
[extra]
id = 18946
[taxonomies]
categories = []
tags = []
+++

This is a [[Racket]] implementation of the [[One-time pad]] task.

We have encryption decryption and pad file management all bundled together here.

<lang>#lang racket
(require srfi/14) ; character sets

;; Pseudo-Vigenere implementation
(define (vigenere-en/decrypt-from-alphabet ab... default-char)
  (define ab...-cs (string->char-set ab...))
  (define m (char-set-size ab...-cs))
  
  (unless (char-set-contains? ab...-cs default-char)
    (error 'en/decrypt-from-alphabet
           "default-char:~s must be member of alphabet:~s" default-char ab...))
  
  (define chr# (for/hash ((i (in-naturals)) (c ab...)) (values i c)))
  (define ord# (for/hash ((i (in-naturals)) (c ab...)) (values c i)))
  
  (define (normalise-char c)
    (cond [(char-set-contains? ab...-cs c) c]
          [(let ((C (char-upcase c))) (and (char-set-contains? ab...-cs C) C)) => values]
          [else default-char]))
  
  (define (encrypt k c)
    (hash-ref chr# (modulo (+ (hash-ref ord# k) (hash-ref ord# (normalise-char c))) m)))
  
  (define (decrypt k c)
    (hash-ref chr# (modulo (- (hash-ref ord# c) (hash-ref ord# k)) m)))
  
  (values ab... encrypt decrypt))

(define-values (AB... ENCRYPT DECRYPT)
  ;; I'm no cryptanalyst, but if (length of the alhabet mod 256 != 0), I'm concerned that there
  ;; *might* be some weakening of the pad (and it gives an excuse for a slightly larger character set)
  (vigenere-en/decrypt-from-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ_.,!/?" #\_))

;; /dev/random is good but slow. /dev/urandom is a bit faster... the racket PRNG could be too
;; predictable. (But there ain't no /dev/u?random on Windows (AFAIK)
(define (default-random-number-generator rfn)
  (define prng (λ () (random #x10000)))
  (define frng (λ () (with-input-from-file rfn (λ () (integer-bytes->integer (read-bytes 4) #f)))))
  (cond
    [(not rfn) (eprintf "WARNING: using build in PRNG~%") prng]
    [(not (file-exists? rfn)) (eprintf "WARNING: file:~s does not exist. Using build in PRNG~%" rfn)
                              prng]         
    [else frng]))

;; Writes the pad to (current-output-port). If dots? is enabled, then progress is reflected on
;; (current-error-port) -- /dev/random can be very slow!
(define (generate-otp
         n-lines #:chars/line (c/l 48) #:chars/block (c/b 6) #:alphabet (ab... AB...)
         #:meta-data (meta-data #f) #:dots? (dots? #t) #:random-file-name (rfn #f)
         #:rng (rng (default-random-number-generator rfn)))
  (define ab...-len (string-length ab...))
  (display "# One-time-pad")
  (when meta-data (printf "~%# ~s" meta-data))  
  (for* ((line n-lines)
         #:when (begin (newline) (when dots? (newline (current-error-port))))
         (chr c/l))
    (define rnd-int (rng))
    (when (zero? (modulo chr c/b)) (write-char #\space)
      (when dots? (write-char #\space (current-error-port))))
    (write-char (string-ref ab... (modulo rnd-int ab...-len)))
    (when dots? (write-char #\. (current-error-port))))
  (newline)
  (when dots? (newline (current-error-port)))
  (displayln "# End one-time-pad"))

;; Wraps the above to write to the given otp-file-name
(define (generate-pad-file
         otp-file-name n-lines #:chars/line (c/l 48) #:chars/block (c/b 6) #:alphabet (ab... AB...)
         #:meta-data (mta #f) #:dots? (dots? #t) #:exists (exists 'error) #:random-file-name (rfn #f)
         #:rng (rng (default-random-number-generator rfn)))
  (with-handlers ([exn:fail:filesystem?
                   (λ (x) (eprintf "error generating file: ~s~%" (exn-message x)) #f)])
    (with-output-to-file otp-file-name #:exists exists
      (λ () (generate-otp n-lines #:chars/line c/l #:chars/block c/b #:alphabet ab...
                          #:meta-data mta #:dots? dots? #:random-file-name rfn #:rng rng)))))

;; OTP FILE "Management" -- scratches lines for you
(define (otp-scratch-lines f-name lines-used)
  (define-values (in out) (open-input-output-file f-name #:exists 'update))
  (let loop
    ((fp (file-position in)) (line (read-line in)) (lines-used lines-used))
    (cond [(zero? lines-used) (void)]
          [(eof-object? line) (error "otp-scratch-lines: ran out of pad!")]
          [(regexp-match #px"^[#\\-]" line) (loop (file-position in) (read-line in) lines-used)]
          [else
           (define old-fp (file-position in))
           (file-position out fp)
           (write-char #\- out)
           (flush-output out)
           (file-position in old-fp)
           (loop old-fp (read-line in) (sub1 lines-used))]))
  (close-input-port in)
  (close-output-port out))

;; Produce two functions that taks a pad-file and a string
(define (make-pad-functions encrypt-fn decrypt-fn)
  (define ((en/decrypt-from-pad crypto-fn) pad-file str)
    (define (use-otp-line line-chars s e lines-used)
      (cond [(null? s) (values (list->string (reverse e)) (add1 lines-used))]
            [(null? line-chars) (sub-d/e-f-p (read-line) s e (add1 lines-used))]
            [(char=? (car line-chars) #\space) (use-otp-line (cdr line-chars) s e lines-used)]
            [else (use-otp-line
                   (cdr line-chars)
                   (cdr s)
                   (cons (crypto-fn (car line-chars) (car s)) e)
                   lines-used)]))
    
    (define (sub-d/e-f-p line s e lines-used)
      (cond [(null? s) (values (list->string (reverse e)) lines-used)]
            [(eof-object? line) (error 'de/encrypt-from-pad "ran out of pad!")]
            [(regexp-match #px"^[#\\-]" line) (sub-d/e-f-p (read-line) s e lines-used)]
            [else (use-otp-line (string->list line) s e lines-used)]))
    (with-input-from-file pad-file (λ () (sub-d/e-f-p (read-line) (string->list str) null 0))))
  
  (values (en/decrypt-from-pad encrypt-fn) (en/decrypt-from-pad encrypt-fn)))

(define-values (encrypt-from-pad decrypt-from-pad) 
  (make-pad-functions ENCRYPT DECRYPT))

;; Testing
(module+ test
  (generate-pad-file
   "test.otp" 4
   #:random-file-name "/dev/urandom" ; is faster
   #:exists 'replace)
  
  ;; pad-file as generated
  (printf "Pad file as generated:~%~a~%" (file->string "test.otp"))
  (define-values (enc enc-lines-used)
    (encrypt-from-pad "test.otp" #<<EOS
Mary had a little lamb! We've heard it all before. Mary had a little lamb, and then she had some more.
EOS
                                   ))
  (printf "Cyphertext: ~s~%" enc)
  (define-values (dec dec-lines-used) (decrypt-from-pad "test.otp" enc))
  (printf "Plaintext:  ~s~%" dec)
  (printf "Scratch: ~s lines from your pad file~%" enc-lines-used)
  (otp-scratch-lines "test.otp" enc-lines-used)
  (printf "Pad file after scratching:~%~a~%" (file->string "test.otp"))
  )
```

