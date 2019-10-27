+++
title = "99 Bottles of Beer/Lisp"
description = ""
date = 2018-08-23T12:04:42Z
aliases = []
[extra]
id = 18273
[taxonomies]
categories = []
tags = []
+++

<!-- 
=Lisp= 
-->
{{collection|99 Bottles of Beer}}
[[99 Bottles of Beer]] done in Lisp-languages

<!--
See [[99 Bottles of Beer/Lisp]]
-->

<!-- still missing:
Emacs Lisp
-->

__toc__


## ACL2


```Lisp
(defun bottles-of-beer (n)
   (if (zp n)
       nil
       (prog2$ (cw (concatenate 'string
                   "~%"
                   "~N0 bottle~#1~[~/s~] of beer on the wall,~%"
                   "~n0 bottle~#1~[~/s~] of beer.~%"
                   "Take one down, pass it around,~%"
                   "~n2 bottle~#3~[~/s~] of beer on the wall.~%")
                   n
                   (if (= n 1) 0 1)
                   (1- n)
                   (if (= n 2) 0 1))
               (bottles-of-beer (- n 1)))))
```



## Common Lisp


### Sensible solution


```lisp
(defun bottles (x)
  (loop for bottles from x downto 1
        do (format t "~a bottle~:p of beer on the wall
~:*~a bottle~:p of beer
Take one down, pass it around
~a bottle~:p of beer on the wall~2%" bottles (1- bottles))))
```

and then just call

```lisp
(bottles 99)
```



### Ridiculous


```lisp
(format t "~{~[~^~]~:*~D bottle~:P of beer on the wall~%~:*~D bottle~:P of beer~%Take one down, pass it around~%~D bottle~:P~:* of beer on the wall~2%~}"
          (loop :for n :from 99 :downto 0 :collect n))
```

The [http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm FORMAT function] is probably the most baroque (i.e. featureful almost to a fault) function in Common Lisp. 
To really drive this point home, try replacing each instance of <tt>~D</tt> 
with <tt>~R</tt>, and then with <tt>~@R</tt>. 
Yes, this is all standard and dependable (dys?)functionality.

Explanation of the format string for the uninitiated:
* <tt>~{<i>fmt</i>~}</tt> expects the next argument to be a list (which is of the integers from 99 down to 0), and executes the format string <i>fmt</i> on each element. It is essentially a map or foreach.
* <tt>~[...~]</tt> is a case/switch. It executes the <i>n</i>th clause, where <i>n</i> is taken from the next argument. Since there is only one clause here, it will be executed only when the argument is 0.
* <tt>~^</tt> will terminate formatting.
* <tt>~:*</tt> will back-up to the most-recently used argument.
* <tt>~D</tt> prints the next argument as a decimal number.
* <tt>~:P</tt> is for English plurals: it prints <tt>s</tt> if the last argument wasn't 1; it prints nothing otherwise. There's also <tt>~@P</tt> for <tt>y</tt>/<tt>ies</tt>, in case you were worried about that.
Note, by the way, how the emoticons <tt>:*~D</tt> and <tt>:P</tt> have shown up in the format string. FORMAT is so powerful, it's even self-aware about how silly it is.

### Alternate solution

Bit of a beginner in Lisp, but this seems to work: 

```lisp

(defun beer-verse (count)
  "Recurses the verses"
  (format t "~A bottle~:P of beer on the wall~%" count)
  (format t "~A bottle~:P of beer~%" count)
  (format t "Take one down, pass it round~%")
  (format t "~A bottle~A of beer on the wall~%~%"
	  (if (= count 1)
	      "No"
	      (- count 1))
	  (if (/= count 2)
	      "s"
	      ""))
  (if (> count 1)
      (beer-verse (- count 1))))
(beer-verse 99)

```

<!-- missing here:

## Emacs Lisp

-->


## NewLISP


```newlisp
(for (n 99 1) 
(println n " bottles of beer on the wall," n " bottles of beer. Take one down, pass it around. ")
(println (- n 1) "bottles of beer on the wall!"))

;;recursive
;;also shows list afterword
(define (rec bottles)
	(if (!= 0 bottles) (print "/n" bottles " bottles of beer on the wall" bottles " bottles of beer. 
\nTake one down, pass it around, " (- bottles 1) 
" bottles of beer on the wall" (rec ( - bottles 1))))(list bottles))

(rec 99)
```



## Ol


```scheme

(define nn 99)

(for-each (lambda (n)
   (let ((bottle (lambda (n) (if (eq? n 1) " bottle" " bottles")))
         (m (- n 1)))
      (print
         n (bottle n) " of beer on the wall, "
         n (bottle n) " of beer." "\n"
         "Take one down and pass it around, "
         (if (eq? m 0) "no more" m)
         (bottle m) " of beer on the wall.\n")))
   (reverse (iota nn 1)))
(print
   "No more bottles of beer on the wall, "
   "no more bottles of beer." "\n"
   "Go to the store and buy some more, "
   nn " bottles of beer on the wall.")

```



## PicoLisp


```PicoLisp
(de bottles (N)
   (case N
      (0 "No more beer")
      (1 "One bottle of beer")
      (T (cons N " bottles of beer")) ) )

(for (N 99 (gt0 N))
   (prinl (bottles N) " on the wall,")
   (prinl (bottles N) ".")
   (prinl "Take one down, pass it around,")
   (prinl (bottles (dec 'N)) " on the wall.")
   (prinl) )
```



## Shen


```Shen
(define bottles-h
  { number --> string }
  0 -> "No more beer"
  1 -> "One bottle of beer"
  N -> (make-string "~A bottles of beer" N))

(define bottles
  { number --> number }
  0 -> 0
  X -> (let Msg (bottles-h X)
         (do (output "~A on the wall~%~A~%Take one down, pass it around~%~A on the wall~%~%" Msg Msg (bottles-h (- X 1)))
             (bottles (- X 1)))))
```



## Wart


```python
def (beer n)
  when (n > 0)
    prn n " bottles of beer on the wall"
    prn n " bottles of beer"
    prn "take one down, pass it around"
    prn n-1 " bottles of beer on the wall"
    prn ""
    beer n-1
```

