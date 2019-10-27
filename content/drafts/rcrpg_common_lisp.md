+++
title = "RCRPG/Common Lisp"
description = ""
date = 2009-12-30T20:43:34Z
aliases = []
[extra]
id = 4074
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
This [[Common Lisp]] version of [[RCRPG]] implements a text interface.

==Features Used==
* Global constants and variables
* Documentation strings
* Functions, anonymous and named, including <code>&optional</code>, <code>&key</code>, and <code>&rest</code> parameters and <code>&aux</code> psuedo-parameters
* Macros
* Reader macros
* Packages
* Hash tables, lists, association lists, and symbol property lists
* The use of <code>eval-when</code> to ensure that running a compiled binary is functionally identical to interpreting the source code directly
* <code>format</code>
* Various standard control structures as well as <code>iterate</code>, the nonstandard but portable replacement for <code>loop</code>
* A single implementation-defined function, <code>quit</code>
* Arbitrary runtime code execution (including <code>defitem</code> and <code>defcommand</code> forms) with the <code>rep</code> command

==Notes==
For a list of all commands, just type "?".

The implementation goes to some lengths to pretend it's a fully-featured text-adventure engine in the style of [http://en.wikipedia.org/wiki/Inform Inform]. Except for usage messages, the output is always natural-looking English prose, and commands like "go east", "take them", "equip it", "drop every piece of gold", and "drop 2 rubies" are supported, along with terser equivalents like "e" and "dr 2 uby". Fair warning that the support for pronouns and quantities without item names is a bit of a hack. In particular, "take it" and "take 3" refer to whichever items happen to be first in the list the game uses internally for storage, in which items may be ordered quite differently from how they appear in the output.

The aliasing system allows you to shadow existing command names. However, a shadowed binding is reactivated when the shadowing binding is deleted, and the game will prevent you from shadowing or deleting a command's last remaining visible binding.

You don't need to explicitly drop a ladder in order to use it; the game will do it for you when necessary. Pick-axes work just like sledges. All other items have no function.

The Prize Room is always at (1, 1, 5), as in the Perl version.

==Code==

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :iterate))
(defpackage :org.rosettacode.rcrpg
  (:use :common-lisp :iterate)
  (:shadow :room)
  (:import-from :common-lisp-user :quit))
    ; #'quit is SBCL's quit function. Replace :quit with :exit
    ; if that's what your implementation calls it.
(in-package :org.rosettacode.rcrpg)

(setf *random-state* (make-random-state t))
(setf *print-pretty* nil)

;;; --------------------------------------------------
;;; Generally useful definitions
;;; --------------------------------------------------

(defun id (x)
"Because \"identity\" is too long."
  x)

(defmacro bif (var test then &optional else &aux (g (gensym)))
"Binding if."
 `(let ((,g ,test))
   (if ,g
     (let ((,var ,g)) ,then)
     ,else)))

(defmacro rpush (x l &aux (g (gensym)))
"Pushes X onto the end of L."
  `(bif ,g ,l
    (rplacd (last ,g) (list ,x))
    (setf ,l (list ,x))))

(defun split-at (n seq)
"(split-at 3 '(:a :b :c :d :e)) => ((:a :b :c) (:d :e))"
  (list
    (subseq seq 0 n)
    (subseq seq n)))

(defun fal (&rest args &aux (cons (apply #'assoc args)))
"Forward alist lookup."
  (when cons (cdr cons)))

(defun ral (&rest args &aux (cons (apply #'rassoc args)))
"Reverse alist lookup."
  (when cons (car cons)))

(defun join-with-spaces (l)
  (format nil "~{~a~^ ~}" l))

(eval-when (:compile-toplevel :load-toplevel :execute)
; Needed so #'sconcat can be used in macros.
  (defun sconcat (&rest l)
    (apply #'concatenate 'string l)))

(defun whitespace-p (c)
  (char= c #\Space))

(defun putline (&rest l)
  (mapc #'princ l)
  (terpri))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\λ
  ; A convenient notation for unary lambdas:
  ; #λ(- x 2) => (lambda (x) (- x 2))
    (lambda (stream subchar arg)
      (declare (ignore subchar arg))
      (list 'lambda (list (read-from-string "x")) (read stream t)))))
        ; I say (read-from-string "x") rather than (intern "X") or
        ; 'x so that the macro works regardless of the current
        ; package or readtable case.

(defconstant +list-fmt+
  "~{~#[~;~a~;~a and ~a~;~a, ~a, and ~a~:;~a, ~]~}")

;;; --------------------------------------------------
;;; Points
;;; --------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
; Needed so #'point can be used in the definition of constants.
  (defstruct point
    (x 0 :type fixnum)
    (y 0 :type fixnum)
    (z 0 :type fixnum))
  
  (defun point (x y z)
    (make-point :x x :y y :z z)))

(defparameter *player-position* (point 0 0 0))

(defconstant +east+  (point  1  0  0))
(defconstant +west+  (point -1  0  0))
(defconstant +north+ (point  0  1  0))
(defconstant +south+ (point  0 -1  0))
(defconstant +up+    (point  0  0  1))
(defconstant +down+  (point  0  0 -1))
  ; Defining directions thus lets us move a point in a direction
  ; with #'point+ (defined below).

(defconstant +directions+ (list
  (cons "east"	+east+)
  (cons "west"	+west+)
  (cons "north"	+north+)
  (cons "south"	+south+)
  (cons "up"	+up+)
  (cons "down"	+down+)))

(defun read-direction (s &aux (len (length s)))
  (fal s +directions+
    :key #λ(subseq x 0 len)
    :test #'string-equal))

(defun direction-name (d)
  (ral d +directions+ :test #'equalp))

(defun point+ (p1 p2)
  (point
    (+ (point-x p1) (point-x p2))
    (+ (point-y p1) (point-y p2))
    (+ (point-z p1) (point-z p2))))

(defun point-invert (p)
  (with-slots (x y z) p
    (point (- x) (- y) (- z))))

(defmethod print-object ((p point) stream &aux
    (s (if *print-escape* "(point ~d ~d ~d)" "(~d, ~d, ~d)")))
  (with-slots (x y z) p
    (format stream s x y z)))

;;; --------------------------------------------------
;;; Items
;;; --------------------------------------------------

(defparameter *items* ()
  "A list of all defined items. Items are represented as symbols.")
(defparameter *item-frequency-counter* 0
  "The sum of all item frequencies.")
(defparameter *player-inventory* ()
  "What items the player is carrying. Multiple copies of the
  same item are represented by multiple instances of the
  appropriate symbol.")
(defparameter *player-equipped-tool* nil)

(defmacro defitem
   (symbol name frequency &key
    (singular-name (sconcat "a " name))
    (plural-name (sconcat name "s"))
    equippable
    &aux (x (gensym)))
  `(progn
    (incf *item-frequency-counter* ,frequency)
    (rpush ',symbol *items*)
    (dolist (,x
      `((:name ,,name)
        (:frequency ,*item-frequency-counter*)
        (:singular-name ,,singular-name)
        (:plural-name ,,plural-name)
        (:equippable ,,equippable)))
      (setf (get ',symbol (car ,x)) (cadr ,x)))))

(dolist (f '(name frequency singular-name plural-name equippable))
  ; Generate #'item-name, #'item-frequency, etc.   
  (eval `(defun
    ,(read-from-string (sconcat "item-" (symbol-name f)))
    (item)
    (get item ,(read-from-string (sconcat ":" (symbol-name f)))))))

(defitem ladder "ladder" 10)

(defitem sledge "sledge" 1
  :equippable t)

(defitem axe "pick-axe" 10
  :equippable t)

(defitem spatula "spatula" 5
  :equippable t)
  ; An example of an equippable item that you can't dig with.

(defitem ruby "ruby" 1
  :plural-name "rubies")

(defitem sapphire "sapphire" 5)

(defitem emerald "emerald" 10
  :singular-name "an emerald")

(defitem gold "gold" 25
  :singular-name "a piece of gold"
  :plural-name "pieces of gold")

(defun item-pretty-name (item n)
"(item-pretty-name 'ruby 2) => \"two rubies\""
  (cond
    ((= n 1)
      (item-singular-name item))
    ((<= n 20)
      (format nil "~r ~a" n (item-plural-name item)))
    (t
      (format nil "~d ~a" n (item-plural-name item)))))

(defun random-items ()
"Creates a short, possibly empty list of items."
  (iter
    (repeat (random 5))
    (for frequency-index = (random *item-frequency-counter*))
    (collect (find-if
       #λ(< frequency-index (item-frequency x))
       *items*))))

(defun list-items (l &aux (h (make-hash-table)))
"(list-items '(ruby ladder ruby)) =>
 (\"a ladder\" \"two rubies\")
The order in which distinct items are mentioned is the same
as the order in which the items were defined."
  (dolist (item l)
    (incf (gethash item h 0)))
  (iter
    (for item in *items*)
    (for n = (gethash item h 0))
    (unless (= n 0)
      (collect (item-pretty-name item n)))))

;;; --------------------------------------------------
;;; Rooms
;;; --------------------------------------------------

(defstruct room
  (name nil :type (or string null))
  (exits () :type list)
    ; A list of directions.
  (items (random-items) :type list))

(defun room-has-exit (r d)
  (when (find d (room-exits r) :test #'equalp) t))

(defparameter *map* (make-hash-table :test #'equalp)
  "A hash table mapping points to rooms.")
(defmacro room-at (point) `(gethash ,point *map*))

(defun describe-room (point &aux
    (room (room-at point))
    (items (list-items (room-items room)))
    orths)
  (format t "~%~a ~@[— ~a~]~%" point (room-name room))
  (if (null (room-exits room))
    (putline "There are no exits.")
    (destructuring-bind (east west north south up down)
        (mapcar
          #λ(when (room-has-exit room (cdr x)) (car x))
          +directions+)
      (when up (putline
          "There's a hole in the ceiling large enough for you to fit through."))
      (when down (putline
          "There's a hole in the floor large enough for you to fit through."))
      (setf orths (list east west north south))
      (case (count-if #'id orths)
        (0)
        (1 (format t "There's an exit to the ~a.~%"
          (find-if #'id orths)))
        (t (format t (sconcat "Exits lead " +list-fmt+ ".~%")
          (remove-if-not #'id orths))))))
  (when items
    (format t
      (sconcat "~%You see here " +list-fmt+ ".~%")
      items)))

;;; --------------------------------------------------
;;; Commands
;;; --------------------------------------------------

(defstruct command
  (args () :type list)
  (usages () :type list)
  (def #'id :type function))

(defparameter *commands* ()
  "An alist mapping names to command objects. Neither names
  nor commands are necessarily unique.")
(defparameter *command-objs* ()
  "All the distinct command objects, in order of definition.")

(defun visible-command-assocs ()
  (remove-duplicates
    *commands*
    :key #'car :test #'string-equal :from-end t))

(defmacro defcommand (names usages args &body body &aux
    (cmd (gensym))
    (visible-args (subseq args 0 (or
      (position '&aux args)
      (length args)))))
  `(let ((,cmd (make-command
      :args ',visible-args
      :usages (list ,@(or
        usages
        (list (format nil "~{~a~^ ~}" visible-args))))
      :def (lambda ,args (block nil ,@body)))))
    (dolist (name ',names)
      (push (cons name ,cmd) *commands*))
    (rpush ,cmd *command-objs*)))

(defcommand ("help" "?") () ()
  (format t
    "~%Available commands:~%~%~{- ~{~{~a~^ / ~}~%~{  ~a~%~}~}~^~%~}"
    (iter
      (with h = (make-hash-table))
      (for (name . c) in (visible-command-assocs))
        ; Shadowed associations aren't listed.
      (push name (gethash c h))
      (finally (return (mapcar
        #λ(let ((strs (gethash x h)))
          (list
            strs
            (mapcar
              #λ(sconcat (car strs) " " (string-downcase x))
              (command-usages x))))
        *command-objs*))))))

(defcommand ("name" "rename") () (&rest new-room-name)
  (setf (room-name (room-at *player-position*))
    (when new-room-name
      ; "name" with no arguments sets the room-name to nil, not
      ; an empty string.
      (join-with-spaces new-room-name)))
  (putline "Room renamed."))

(defcommand ("alias") () (command-name new-name)
  (bif c (fal command-name *commands* :test #'string-equal)
    (progn
      (bif shadowed-c (fal new-name *commands* :test #'string-equal)
        (when (= 1 (count shadowed-c (visible-command-assocs) :key #'cdr))
          (format t
            "But the command \"~a\" has no other usable name.~%"
            new-name)
          (return)))
      (push (cons new-name c) *commands*)
      (format t "Okay, \"~a\" now means \"~a\".~%"
        new-name command-name))
    (putline "No command with that exact name is already defined.")))

(defcommand ("delalias") () (command-name)
  (bif c (fal command-name *commands* :test #'string-equal)
    (if (= 1 (count c (visible-command-assocs) :key #'cdr))
      (putline "But that command has no other usable name.")
      (progn
        (setf *commands* (remove
          command-name *commands*
          :test #'string= :key #'car :count 1))
        (putline "Alias removed.")))
    (putline "No command with that exact name is already defined.")))

(defcommand ("look") () ()
  (describe-room *player-position*))

(defun try-move-player (dir &aux
    (r (room-at *player-position*)))
  (cond
    ((not (room-has-exit r dir))
      (putline "You can't go that way."))
    ((equalp dir +up+)
      (cond
        ((find 'ladder (room-items r))
          (format t "You climb up ~a ladder.~%"
            (if (= 1 (count 'ladder (room-items r))) "the" "a"))
          (setf *player-position* (point+ *player-position* +up+)))
        ((find 'ladder *player-inventory*)
          ; The player automatically drops and uses one ladder.
          (format t "You position ~a and climb up it.~%"
            (if (= 1 (count 'ladder *player-inventory*))
              "your ladder" "one of your ladders"))
          (setf *player-inventory* (remove 'ladder *player-inventory* :count 1))
          (push 'ladder (room-items r))
          (setf *player-position* (point+ *player-position* +up+)))
        (t
          (putline "You can't get a good grip on the sides of the hole."))))
    (t
      (when (equalp dir +down+)
        (format t "You ~a down to the room below.~%"
          (if (find 'ladder (room-items (room-at (point+ *player-position* +down+))))
            "climb" "carefully hoist yourself")))
      (setf *player-position* (point+ *player-position* dir)))))

(defcommand ("go" "move" "walk" "run" "enter" "climb") () (direction &aux
    (d (read-direction direction)))
  (if d
    (try-move-player d)
    (putline "I don't recognize that direction.")))

(dolist (d +directions+)
  (eval
    `(defcommand (,(car d) ,(elt (car d) 0)) () ()
      (try-move-player ,(cdr d)))))

(defcommand ("inventory") () ()
  (if *player-equipped-tool*
    (format t "You have ~a equipped.~%"
      (item-singular-name *player-equipped-tool*))
    (putline "You don't have anything equipped."))
  (if *player-inventory*
    (format t "You are carrying:~%~{  ~a~%~}"
      (list-items *player-inventory*))
    (putline "Your knapsack is empty.")))

(defun match-description-against-items
    (description items no-match-message &aux
    (first (car description)) (qty-requested 1) (name nil))
"Given a list of strings DESCRIPTION like one of these:
  gold
  ol
  a gold
  it
  3
  all
  3 gold
  3 pieces of g
  all gold
returns a list of two lists, namely
  (a) the appropriate items from ITEMS
  (b) ITEMS with the contents of (a) removed
or a string describing an error."
  (cond
    ((every #'digit-char-p first)
      (setf qty-requested (parse-integer first))
      (when (= 0 qty-requested)
        (return-from match-description-against-items "Um, okay.")))
    ((find first '("all" "every" "them") :test #'string=)
      (setf qty-requested nil))
    ((find first '("a" "an" "the" "it") :test #'string=)
      (setf qty-requested 1))
    (t
      (setf name (join-with-spaces description))))
  (if (and (not name) (cdr description))
    (setf name (join-with-spaces (cdr description))))
  (if name
    (let*
       ((distinct-items (iter (for i in items) (adjoining i)))
        (l (iter
          (for i in distinct-items)
          (when (or
              (search name (item-name i))
              (search name (item-singular-name i))
              (search name (item-plural-name i)))
            (collect i))))
        qty-present)
      (case (length l)
        (0 no-match-message)
        (1
          (setf qty-present (count (car l) items))
          (if qty-requested
            (when (< qty-present qty-requested)
              (return-from match-description-against-items "There aren't so many."))
            (setf qty-requested qty-present))
          (list
            (make-list qty-requested :initial-element (car l))
            (remove (car l) items :count qty-requested)))
        (t "I can't tell which item you're referring to.")))
    (if (and qty-requested (< (length items) qty-requested))
      "There aren't so many items."
      (split-at (or qty-requested (length items)) items))))

(defconstant +item-name-usage+
  "first-word-of-item-name &rest rest-of-item-name")

(defcommand ("take" "get" "grab")
    (+item-name-usage+ "quantity" "quantity &rest item-name")
    (arg1 &rest argn &aux
      (r (room-at *player-position*))
      (items (room-items r))
      temp)
  (when (null items)
    (putline "There's nothing here to take.")
    (return))
  (setf temp (match-description-against-items
    (cons arg1 argn) items "You can't see any such thing."))
  (when (atom temp)
    ; It's an error message.
    (putline temp)
    (return))
  (setf *player-inventory* (append (car temp) *player-inventory*))
  (setf (room-items r) (cadr temp))
  (cond
    ((= 1 (length (car temp)))
      (putline "Taken."))
    ((<= (length (car temp)) 20)
      (format t "You took ~r item~:p.~%" (length (car temp))))
    (t
      (format t "You took ~d item~:p.~%" (length (car temp))))))

(defcommand ("drop")
    (+item-name-usage+ "quantity" "quantity &rest item-name")
    (arg1 &rest argn &aux
      (r (room-at *player-position*))
      temp)
  (when (null *player-inventory*)
    (putline "Your knapsack is already empty.")
    (return))
  (setf temp (match-description-against-items
      (cons arg1 argn) *player-inventory* "No such thing is in your knapsack."))
  (when (atom temp)
    ; It's an error message.
    (putline temp)
    (return))
  (setf (room-items r) (append (car temp) (room-items r)))
  (setf *player-inventory* (cadr temp))
  (cond
    ((= 1 (length (car temp)))
      (putline "Dropped."))
    ((<= (length (car temp)) 20)
      (format t "You dropped ~r item~:p.~%" (length (car temp))))
    (t
      (format t "You dropped ~d item~:p.~%" (length (car temp))))))

(defcommand ("equip" "wield" "arm" "ready")
    (+item-name-usage+)
    (arg1 &rest argn &aux temp)
  (when (null *player-inventory*)
    (putline "You don't have anything to equip.")
    (return))
  (setf temp (match-description-against-items
      (cons arg1 argn) *player-inventory* "No such thing is in your knapsack."))
  (when (atom temp)
    ; It's an error message.
    (putline temp)
    (return))
  (unless (= 1 (length (car temp)))
    (putline "You can only have one tool equipped at a time.")
    (return))
  (unless (item-equippable (caar temp))
    (putline "You can't equip that.")
    (return))
  (setf *player-inventory* 
    (if *player-equipped-tool*
      ; Any previously equipped tool is returned to the player's
      ; inventory.
      (cons *player-equipped-tool* (cadr temp))
      (cadr temp)))
  (setf *player-equipped-tool* (caar temp))
  (format t "You ready the ~a for action.~%"
    (item-name *player-equipped-tool*)))

(defcommand ("unequip" "remove" "stow" "pack") () ()
; Takes no arguments, since you can only have one item equipped
; at a time.
  (if *player-equipped-tool*
    (progn
      (format t "You put away your ~a.~%"
        (item-name *player-equipped-tool*))
      (push *player-equipped-tool* *player-inventory*)
      (setf *player-equipped-tool* nil))
    (putline "You don't have anything equipped.")))

(defcommand ("hit" "strike" "dig" "attack") () (direction &aux
    (d (read-direction direction))
    (r (room-at *player-position*))
    new-p)
  (cond
    ((not d)
      (putline "Illegal direction."))
    ((not *player-equipped-tool*)
      (putline "You don't have an appropriate tool equipped."))
    ((not (find *player-equipped-tool* '(sledge axe)))
      (format t "You can't dig with ~a.~%"
        (item-singular-name *player-equipped-tool*)))
    ((room-has-exit r d)
      (putline "I'd say that one hole per surface is enough."))
    (t
      (push d (room-exits r))
      (setf new-p (point+ *player-position* d))
      (if (room-at new-p)
        (push (point-invert d) (room-exits (room-at new-p)))
        (setf (room-at new-p)
          (make-room :exits (list (point-invert d)))))
      (format t "You create a large hole in the ~a.~%"
        (cond
          ((equalp d +up+) "ceiling")
          ((equalp d +down+) "floor")
          (t (sconcat (direction-name d) "ern wall")))))))

(defcommand ("rep") () (&rest tokens &aux vs)
; Read, Eval, Print. Handy for debugging/cheating.
  (unless tokens
    (return))
  (setf vs (multiple-value-list (ignore-errors
    (eval (read-from-string (join-with-spaces tokens))))))
  (if (and (= 2 (length vs)) (not (car vs)))
    (format t "Error: ~a~%" (cadr vs))
    (putline (car vs))))

;;; --------------------------------------------------
;;; Execution
;;; --------------------------------------------------

(setf (room-at *player-position*) (make-room
  :name "Start"
  :items (list 'sledge)))
(setf (room-at (point 1 1 5)) (make-room
  :name "The Prize Room"
  :items (iter
    (repeat 3)
    (nconcing (random-items) into items)
    (finally (return (nconc
      items
      (make-list (+ 2 (random 6)) :initial-element 'ruby)
      (make-list (+ 40 (random 20)) :initial-element 'gold)))))))

(putline "Welcome to RCRPG, ANSI-Approved Parentheses Edition.")
(putline "Brought to you by the letter λ.")
(describe-room *player-position*)

(iter ; The main loop.
  (for line = (progn
    (terpri)
    (princ ">")
    (finish-output)
    (read-line *standard-input* nil)))
  (while line)
    ; The only way to get out of the loop is to send an EOF
    ; to the console.
  (unless (find-if-not #'whitespace-p line)
    (putline "Say what?")
    (next-iteration))

  ; Tokenize the input.
  (for (cmd . args) = (iter
    (for p initially 0 then end)
    (for start = (position-if-not #'whitespace-p line :start p))
    (unless start
      (return l))
    (for end = (position-if #'whitespace-p line :start start))
    (unless end
      (collect (subseq line start) into l)
      (return l))
    (collect (subseq line start end) into l)))
  
  ; Try to match CMD against known command names.
  (for cmdlen = (length cmd))
  (for matches = (iter
    ; A command name is a match if it begins with CMD. A name
    ; that's exactly CMD is preferred above all other matches.
    (for (name . nil) in (visible-command-assocs))
    (for n = (string-not-equal cmd name))
    (unless n
      (return name))
    (when (= n cmdlen)
      (collect name))))
  (unless matches
    (putline "I don't recognize that command.")
    (next-iteration))
  (when (and (listp matches) (> (length matches) 1))
    (format t
      (sconcat
        "Ambiguous abbreviation: please disambiguate among "
        +list-fmt+ ".~%")
      (mapcar #λ(sconcat "\"" x "\"") matches))
    (next-iteration))
  (for name = (if (listp matches) (car matches) matches))
  (for c = (fal name *commands*))

  ; Check if the player got the command's arity right. If not,
  ; print a usage message.
  (for optional-pos = (position '&optional (command-args c)))
  (for rest-pos = (position '&rest (command-args c)))
  (when (cond
      ((and optional-pos rest-pos)
        (< (length args) optional-pos))
      (optional-pos (or
        (< (length args) optional-pos)
        (> (length args) (length (command-args c)))))
      (rest-pos
        (< (length args) rest-pos))
      (t
        (/= (length args) (length (command-args c)))))
    (format t "Usage: ~(~{~a~%~@{       ~a~%~}~}~)"
      (mapcar #λ(sconcat name " " x) (command-usages c)))
    (next-iteration))

  ; Run the command.
  (for last-pos = *player-position*)
  (apply (command-def c) args)
  (unless (equalp last-pos *player-position*)
    (describe-room *player-position*)))

(format t "~%So long.~%")
(quit)
```


[[Category:RCRPG]]
