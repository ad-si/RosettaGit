+++
title = "Go Fish/PicoLisp"
description = ""
date = 2012-12-06T07:00:17Z
aliases = []
[extra]
id = 8973
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}


```PicoLisp
(de *Ranks
   Ace 2 3 4 5 6 7 8 9 10 Jack Queen King )
 
(de goFish ()
   (let
      (Ocean (by '(NIL (rand)) sort (mapcan '((R) (need 4 R)) *Ranks))
         Your (cut 9 'Ocean)
         Mine (cut 9 'Ocean)
         YouHave NIL
         YouDont NIL
         YourBooks NIL
         MyBooks NIL
         Reply NIL
         Options NIL
         Request NIL )
      (loop
         (prin "Your Books: ")
         (flush)
         (println YourBooks)
         (prin "My Books:   ")
         (flush)
         (println MyBooks)
         (T (nor Your Mine Ocean)
            (let (Y (length YourBooks)  M (length MyBooks))
               (prinl
                  (cond
                     ((= Y M) "Tie game")
                     ((> Y M) "You won!")
                     (T "I won!") ) ) ) )
         (prin "You have ")
         (flush)
         (println (sort Your))
         (prinl "I have " (length Mine) " cards")
         (loop
            (prin
               (if Ocean
                  "Ask for a rank, lay down a book, or 'draw' a card: "
                  "Ask for a rank or lay down a book: " ) )
            (flush)
            (T (member (setq Reply (read)) *Ranks)
               (ifn (filter = Mine (circ Reply))
                  (prinl
                     "   I don't have any card of rank "
                     (push 'YouHave Reply) )
                  (prin "   I give you ")
                  (flush)
                  (println @)
                  (setq
                     Mine (diff Mine @)
                     Your (append @ Your)
                     YouHave (append @ YouHave)
                     YouDont (diff YouDont @) ) ) )
            (T (and Ocean (== 'draw Reply))
               (prinl "   You draw a " (push 'Your (pop 'Ocean)))
               (off YouDont) )
            (cond
               ((atom Reply)
                  (prin "   The rank must be one of ")
                  (flush)
                  (println *Ranks) )
               ((and (cdddr Reply) (member (car Reply) *Ranks) (not (cdr (uniq Reply))) (= (length Your) (length (append (diff Your Reply) Reply))))
                  (prin "   You lay down the book ")
                  (flush)
                  (println (push 'YourBooks Reply))
                  (setq
                     Your (diff Your Reply)
                     YouHave (diff YouHave Reply) ) )
               (T (prinl "   A book consists of four ranks, e.g. (7 7 7 7)")) ) )
         (cond
            ((setq Options (diff (rot Mine) YouDont))
               (setq Request
                  (car
                     (or
                        (sect
                           (filter
                              '((Opt) (= 3 (cnt = Mine (circ Opt))))
                              Options )
                           YouHave )
                        (sect Options YouHave)
                        Options ) ) )
               (loop
                  (prin "Please give me all your " Request "s (or NIL): ")
                  (flush)
                  (NIL (setq Reply (read))
                     (push 'YouDont Request)
                     (ifn Ocean
                        (prinl "   I pass")
                        (prinl "   I draw a card")
                        (push 'Mine (pop 'Ocean)) ) )
                  (T (and (pair Reply) (member Request Reply) (not (cdr (uniq Reply))) (= (length Your) (length (append (diff Your Reply) Reply))))
                     (setq
                        Your (diff Your Reply)
                        YouHave (diff YouHave Reply)
                        Mine (append Reply Mine) ) )
                  (prinl "   I expect a list of " Request "s") ) )
            (Ocean
               (prinl "   I draw a card")
               (push 'Mine (pop 'Ocean)) )
            (T (prinl "   I pass")) )
         (while (find '((R) (= 4 (cnt = Mine (circ R)))) *Ranks)
            (let B (need 4 @)
               (prin "   I lay down the book ")
               (flush)
               (println (push 'MyBooks B))
               (setq Mine (diff Mine B)) ) )
         (prinl) ) ) )
```

