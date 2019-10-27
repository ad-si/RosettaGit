+++
title = "Talk:Latin Squares in reduced form/Randomizing using Jacobson and Matthews’ Technique"
description = ""
date = 2019-08-12T14:59:14Z
aliases = []
[extra]
id = 22460
[taxonomies]
categories = []
tags = []
+++

==https://brainwagon.org/2016/05/17/code-for-generating-a-random-latin-square/ some good news and some bad news==
Quoting from Drizen the following is this task:

Theorem 4. Let X0 be an arbitrarily distributed order-n Latin
square that starts a Markov chain of (proper and improper) squares: to each square,
apply a move chosen uniformly at random from the permissible ±1-moves [n2(n-1)
from a proper square, 8 from an improper square]. Let Xᵜ ≡ (X1;X2;X3; : : :) be
the subsequence of proper squares we encounter; then Xᵜ is a Markov chain with a
(unique) stationary distribution that is uniform over the set of order-n Latin squares.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:57, 12 August 2019 (UTC)

### some bad news

The C code above has defined a meaning for random which is not suitable for this task, and in my view not suitable for its own purpose. It performs permissible ±1-moves in a loop DIM*DIM*DIM times, where DIM is the order of the required Latin Square. In effect it generates a Markov chain and returns X(DIM*DIM*DIM) as its Random Latin Square. If one uses this as X1 it is: not this task; and slow. To generate X5 for Latin Square order 5 the C-code generates 42*42*42*5 proper squares. The task is only asking for 750. As observed you will wait a very long time for X1 of order 256 while the C-code generates 256*256*256 proper squares.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:57, 12 August 2019 (UTC) 

### some good news
 
The C code above may be translated into this task if one removes the outer loop. I would expect part 3 and part 4 in less than 1 second.--[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 11:57, 12 August 2019 (UTC)

: Thanks Nigel.

: If I remove the outer loop (i.e. perform 1 iteration rather than DIM*DIM*DIM iterations), then parts 3 and 4 complete in about 7 seconds on my elderly Linux machine. If we assume a modern machine would be 3 or 4 times faster and rewriting the whole thing in C rather than Go would be twice as fast again, then your estimate of around 1 second looks about right.

: Googling brings up a couple of other J & M implementations, the first in Java (albeit in bits) and the other in Javascript (possibly a translation of the first):

: [http://sedici.unlp.edu.ar/bitstream/handle/10915/42155/Documento_completo.pdf?sequence=1 java]

: [https://github.com/RAMPKORV/jacobson-matthews-latin-square-js/blob/master/getRandomLatinSquare.js javascript]

: They both seem to be looping n ^ 3 times so it may be that the brainwagon.org implementation has worked from these.

: Anyway, if one iteration suffices for the purposes of this task (the results certainly look random enough), I'll change my first version and get rid of the second version which is no longer needed now and was not completely satisfactory in any case :) --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 14:58, 12 August 2019 (UTC)
