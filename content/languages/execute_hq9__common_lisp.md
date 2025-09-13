+++
title = "Execute HQ9+/Common Lisp"
description = ""
date = 2010-02-06T14:28:19Z
aliases = []
[extra]
id = 4709
[taxonomies]
categories = []
tags = []
+++



The interpreter (in [Common Lisp](https://rosettacode.org/wiki/Common_Lisp)) accepts a string, treated as program text, or a pathname object, in which case the contents of the file are the program text.


```lisp
(defun slurp (filespec)
  "Return the contents of the file as a string."
  (with-output-to-string (out)
    (with-open-file (in filespec :direction :input)
      (loop for line = (read-line in nil nil)
            until (null line) do (write-line line out)))))

(defun hq9+ (input &optional (out *standard-output*))
  "Execute the hq9+ program designated by input.  If input is a
string, it is taken as the program text.  If it is a pathname, then
the program text is the content of the file.  The final value of the
accumulator is returned."
  (loop with src = (if (stringp input) input (slurp input))
        with accumulator = 0
        for c across src
        do (case c
            (#\h (write-line "Hello, world!" out))
            (#\q (write-string src out))
            (#\+ (incf accumulator))
            (#\9
             (do ((n 99 (1- n))) ((zerop n))
               (format out "~&~%~w bottle~:p of beer on the wall~%~
                            ~w bottle~:p of beer~%~
                            Take one down, pass it around~%~
                            ~:[~w bottle~:p~;No more bottles~] ~
                                           of beer on the wall~%"
                       n n (zerop (1- n)) n))
             (format out "~&~%No more bottles of beer on the wall~%~
                          No more bottles of beer on the wall~%~
                          Go to the store and buy some more~%~
                          99 bottles of beer on the wall.~%")))
        finally (return accumulator)))
```

