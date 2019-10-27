+++
title = "RCSNUSP/PicoLisp"
description = ""
date = 2014-10-10T10:47:34Z
aliases = []
[extra]
id = 18043
[taxonomies]
categories = []
tags = []
+++

'''snusp.l'''

```PicoLisp
(load "@lib/simul.l")

(de snusp (L)
   (let
      (M (length (maxi length L))
         Grid (grid M (length L)) 
         L 
            (mapcar
               '((I) (need (- M) I " "))
               L ) )
      (println 'L L)
      (mapc
         '((G L)
            (mapc '((This Val) (=: V Val)) G L) )
         Grid
         (apply mapcar (reverse (mapcar chop L)) list) )
      
      # Debug
      #(disp Grid 0 '((This) (align 3 (: V))))         
      
      (let 
         (This
            (or
               (find
                  '((I) I)
                  (mapcar
                     '((I)
                        (find 
                           '((This) (= (: V) '$))
                           I ) )
                  Grid ) )   
               (last (car Grid)) )
            Dir 'east
            S NIL
            D (list 0)
            DH 1 
            DL 1 )
         
         (loop
            (case (: V)
               (>
                  (inc 'DH)
                  (when (> DH DL)
                     (setq D (insert DH D 0))
                     (inc 'DL) ) )
               (<
                  (dec 'DH)
                  (when (< DH 1)
                     (setq D (insert DH D 0))
                     (inc 'DL)
                     (one DH) ) )
               (+ (inc (nth D DH)))
               (- (dec (nth D DH)))
               (. (prin (char (get D DH))))
               (! (setq This (Dir This)))
               (?
                  (when (=0 (get D DH))
                     (setq This (Dir This)) ) )
               ("," (set (nth D DH) (char (key))))
               (@ (push 'S (list This Dir)))
               ("#"
                  (if (car S)
                     (prog1
                        (pop 'S)
                        (setq This (car @)  Dir (cadr @))
                        (setq This (Dir This)) )
                     (off This) ) )
               (/
                  (setq Dir
                     (case Dir
                        (east 'north)
                        (north 'east)
                        (west 'south)
                        (south 'west) ) ) )
               (\\
                  (setq Dir
                     (case Dir
                        (west 'north)
                        (north 'west)
                        (east 'south)
                        (south 'east) ) ) ) )
            (setq This (Dir This))
            (NIL This) ) ) ) )
            
(when (argv)
   (snusp
      (in (car (argv)) (split (till) "^J")) ) )
      
(bye)
```


'''99.snusp'''

```txt

   /=!/
### =====
!/==+++++++++#   +9
   |  |  /=!/=====@/==@@@+@+++++# +48 (itoa)
   |  |  |  |  /==!/==@@@@=++++#  +32 (space)
   |  |  |  |  |   \==@@++\!+++++++++++++\!+++++\ 
   9  9 '9''9' space     'b'            'o'    't'
   
 $@/>@/>@/>@/>@/>
### =====@/>============@/>=
@/>++++++++++     \n  setup
   /
### =================================loop==
>\!=>\!<<<<<<<< /
   \@\@\>cr.@\< ?\<->+++++++++>->+++++++++\       |   |
     ! |     |   \===-
### ==
>=>-==BCD==!\< @\< ?/< ?/# no more beer!
     /=|=====|
### ==========================
/
     | |     \<++t.<<----a.>----k.<++++e.<_.>>++++o.-n.< e.<_.>-d.>+o.>+++w.<-n.<<_.\ 
     | |     /                                                                      /
     | |     \>---a.>n.<+++d.<_.>>++p.<---a.>>----s.s.<<<_.>>-------i.>+t.<<<_.\ 
     | |     /                                                                 /
     | |     \>a.>>--r.<++++++o.>+++u.<-n.<+++d.>>>cr.<-T<+O<--B<<<#
     | !
     \@\<<<_.>>o.-n.<<_.>>>++t.<<+++h.---e.<_.>>>+++w.<<----a.>--l.l.>>CR.<---T<+++O<+B<<<#
       |
       \9.>9.>_.>B.>O.>T.t.<---l.<+++e.>>-s.<<<_.>>+++O.<+f.<_.>----b.+++e.E.>>-R.#

```


Usage:

```txt

mike@tron:~# pil ./snusp.l - 99.snusp +
99 bottles of beer on the wall
   <a lot of strings>
00 bottles of beer on the wall

```

