+++
title = "A+B/Turing Machine"
description = ""
date = 2015-03-03T18:22:17Z
aliases = []
[extra]
id = 18822
[taxonomies]
categories = []
tags = []
+++

The following is a solution implemented on a 20-state, 13-color turing machine.

<lang>
;Start in state A
0 * * * A

;State A- find right end of left block, detect sign
A _ * l B
A - * r AAA
A * * r *

;State B- decrement left block, detect if calculations are complete
B 0 9 l *
B 9 8 r C
B 8 7 r C
B 7 6 r C
B 6 5 r C
B 5 4 r C
B 4 3 r C
B 3 2 r C
B 2 1 r C
B 1 0 r C
B _ * r AA
B * * * AA

;States C and D- find right end of right block, check sign on right block
C _ * r D
C * * r *
D _ * l E
D - * r AAAA
D * * r *

;State E- increment right block, shifting to the right if needed
E 0 1 l F
E 1 2 l F
E 2 3 l F
E 3 4 l F
E 4 5 l F
E 5 6 l F
E 6 7 l F
E 7 8 l F
E 8 9 l F
E 9 0 l *
E _ * r SHIFT1
E M * r SHIFT1

;State F- return to right end of left block
F _ * l B
F * * l *

;States AA and BB- calculations are finished, clean up and halt
AA _ * r BB
AA * _ r *
BB M - * halt
BB * * * halt

;States SHIFT1 and SHIFT0- shift right block 1 cell to the right
SHIFT1 0 1 r SHIFT0
SHIFT0 0 0 r SHIFT0
SHIFT0 _ 0 * F 

;State AAA- Left block is negative, increment left block towards 0 and detect if finished
AAA 0 9 l *
AAA 9 8 r BBB
AAA 8 7 r BBB
AAA 7 6 r BBB
AAA 6 5 r BBB
AAA 5 4 r BBB
AAA 4 3 r BBB
AAA 3 2 r BBB
AAA 2 1 r BBB
AAA 1 0 r BBB
AAA _ * r BB
AAA * * * BB

;States BBB and CCC- Find right end of right block, check sign on right block
BBB _ * r CCC
BBB * * r *
CCC _ * l DDD
CCC - * * AAAAA
CCC * * r *

;State DDD- Decrement right block, detect if finished
DDD 9 8 l EEE
DDD 8 7 l EEE
DDD 7 6 l EEE
DDD 6 5 l EEE
DDD 5 4 l EEE
DDD 4 3 l EEE
DDD 3 2 l EEE
DDD 2 1 l EEE
DDD 1 0 l EEE
DDD 0 9 l *
DDD _ * r FFF

;State EEE- Return to right end of left block
EEE _ * l *
EEE * * * AAA

;State FFF- Cleanup
FFF _ * * GGG
FFF * _ r *

;State GGG- Return to right end of left block
GGG _ * l *
GGG * * * HHH

;State HHH- finish calculations and halt
HHH 0 1 * halt
HHH 1 2 * halt
HHH 2 3 * halt
HHH 3 4 * halt
HHH 4 5 * halt
HHH 5 6 * halt
HHH 6 7 * halt
HHH 7 8 * halt
HHH 8 9 * halt
HHH 9 0 l *

;States AAAA and BBBB- Right block is negative, respond accordingly
AAAA _ * l BBBB
AAAA * * r * 
BBBB 9 8 l F
BBBB 8 7 l F
BBBB 7 6 l F
BBBB 6 5 l F
BBBB 5 4 l F
BBBB 4 3 l F
BBBB 3 2 l F
BBBB 2 1 l F
BBBB 1 0 l F
BBBB 0 9 l *
BBBB * * * FFF

;State AAAAA- Both blocks are negative, respond accordingly
AAAAA * M r E

```

