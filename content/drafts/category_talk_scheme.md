+++
title = "Category talk:Scheme"
description = ""
date = 2012-11-08T11:15:23Z
aliases = []
[extra]
id = 12553
[taxonomies]
categories = []
tags = []
+++

(define program
  '((def true_0 (lambda x (lambda y x)))
    (def false_0 (lambda x (lambda y y)))
 
    (def not_0 (lambda x ((x false_0) true_0)))
    (not_0 true_0)
    (not_0 false_0)
 
    (def and_0 (lambda x (lambda y ((x y) false_0))))
    ((and_0 true_0) true_0)
    ((and_0 true_0) false_0)
    ((and_0 false_0) true_0)
    ((and_0 false_0) false_0)
 
    (def or_0 (lambda x (lambda y ((x true_0) y))))
    ((or_0 true_0) true_0)
    ((or_0 true_0) false_0)
    ((or_0 false_0) true_0)
    ((or_0 false_0) false_0)))


> (interpret program)
(lambda x (lambda y x))
(lambda x (lambda y y))

(lambda x ((x false_0) true_0))
(lambda x (lambda y y))
(lambda x (lambda y x))

(lambda x (lambda y ((x y) false_0))) 
(lambda x (lambda y x)) 
(lambda x (lambda y y)) 
(lambda x (lambda y y)) 
(lambda x (lambda y y))

(lambda x (lambda y ((x true_0) y)))
(lambda x (lambda y x))
(lambda x (lambda y x))
(lambda x (lambda y x))
(lambda x (lambda y y))
