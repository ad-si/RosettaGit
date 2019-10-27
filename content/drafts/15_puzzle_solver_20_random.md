+++
title = "15 puzzle solver/20 Random"
description = ""
date = 2017-11-15T13:03:36Z
aliases = []
[extra]
id = 21640
[taxonomies]
categories = []
tags = []
+++

The following table shows the result of solving 20 [http://www.rosettacode.org/wiki/15_Puzzle_Game#C.23 randomly generated 15 puzzles] using the C++ imperative solver.


The average number of moves to solve the puzzle is 53, max 61 min 45.


The average time to solve the puzzle is 2.9524 seconds. With one outlier at 29 seconds.



```txt
 
                         time   n  _n   
0xdab6531480c2e79f     0.040s  45   6   luurdruldlurrrddlldrrulurdlluruldldrdlururdrd
0x92fd74c6810e5a3b     0.477s  54   8   druuulddruuldllurrdldlurdrruuldldrdluldrrruulullddrdrr
0x487c50df9163bea2     0.174s  48   7   drrdllluurdruulldrdrrdllurruullddrdruuulldlddrrr
0x7ab1093458d2e6cf     0.118s  47   8   ddrurdluldruuulddrururddldrululurdruldllurrrddd
0xeb8fc5d3629017a4    29.300s  61  10   dluuurdllldrdruuldruulldddrruurddluuurdldrullulddruuldddrurrd
0x4cf92de35786b1a0     2.818s  56   7   ullurullddrdlurrurddlulururdluldrdrdlluuldrrruullldrdrdr
0xce2b170a65d489f3     0.221s  51   7   rddluululddruurdruldllddrruurdllldrruldrruluruldrdd
0x4d87a62950ebcf13     4.106s  55   9   uulddrurullddrrdruuldruuldldrdlluruurdddluulurrdrddlurd
0x45bd378c0e26af19     3.889s  56   9   rrdlurruuldrulldrulldrddluurrulldrdrdruuulldddruldlurrrd
0xf43627518b0ed9ac     0.277s  48   8   lurulldrrrulldrdllurdrrullddrurulullddrrulddrurd
0x7ad568b012ce34f9     1.603s  58   9   luldddluurddruuulddrrdllurruullldddrururulldldrurulldrdrrd
0xa90b16f7452ced38     4.240s  52  11   dlurddluulddrulddrurdruulldlururdruldddluuurddruldrd
0x4fe8a93c76d02b15     1.835s  57   8   ulddlurdruluulldrddruuulddldrruuruldllddrrululurdldrurdrd
0x97fc86d5e2b140a3     7.038s  60  10   ruullurddrrdllururullddldrulurrrullldrdrrullurdddruuuldldrdr
0x35e40ac7f96b21d8     0.027s  45   7   ddruulddruuulddrruulldrddruuldldrurrdlluurrdd
0x5f3a987e4dc2016b     0.252s  51   7   ruldrrurullurddlurdruuldrddlullurddruurulldlurddrrd
0x5872efca06914bd3     0.951s  54   7   rrulddruurddlullurdldrulurrrdlluurdlurrdldrulllurrrddd
0x36fe79ba21d50c84     0.714s  57   8   uurdrurddluurddlluldrulurrurdlulldrdrurddluuuldddrulurdrd
0x48d1b7ca230e659f     0.079s  50   4   rululdlurdlddrruuldrrulurdllurdllurdlddrurullddrrr
0x3c0ba57e6d2f8941     0.676s  56   7   dddruuuldddllurrrululdddrulldruluruldrrurdldrululddldrr

```

==Phix solutions:==
First, non-optimal - the following table shows the result of solving the same puzzles using the Phix solver, with MTM=0 and STM=0. 

Move counts are listed below as mtm (stm) [stm-optimal from elsewhere (ie above)].

The average number of moves to solve a puzzle is 38.45, max 44 min 29. 

The average time to solve a puzzle is none too shabby, but we can soon change that...

```txt

{13,10,11,6,5,3,1,4,8,0,12,2,14,7,9,15}   0.25s  33 (53) [45]  lu2r2dlur2d2l3u2r2d3l2ur2ul2drur2uld3lu3r2dldldr2
{9,2,15,13,7,4,12,6,8,1,0,14,5,10,3,11}   1.03s  38 (68) [54]  dru3l2d2r2u2l2d2r2ul3d2r3u2l3dr2u2rdl3ur3dl3drdlu2r2dl2urd2r2
{4,8,7,12,5,0,13,15,9,1,6,3,11,14,10,2}   0.28s  34 (56) [48]  dru2l2d3r3u3l2d2rdru3ld3l2urdlu3r2d2lu2rdld2ruldr2
{7,10,11,1,0,9,3,4,5,8,13,2,14,6,12,15}   3.20s  35 (55) [47]  rdr2ul3dr2u2rd2lu2l2dr2uld3lu3rd3ru3rd3l2urdruldr
{14,11,8,15,12,5,13,3,6,2,9,0,1,7,10,4}   0.92s  44 (79) [61]  u2l3d3r2u3l2d3r2u3l2d3r2u2rd2lu3rd2l2uldr2u2rdl3d2r3ul2u2rd2luruld2rdr
{4,12,15,9,2,13,14,3,5,7,8,6,11,1,10,0}   1.72s  41 (68) [56]  ul2u2ld3ru2r2ul3d3r3u3ld3lur2ul3dr2ul2dr2dlur2dlu3ldlur2drd2
{12,14,2,11,1,7,0,10,6,5,13,4,8,9,15,3}   0.14s  36 (57) [51]  luld2ru2r2d3lu2l2d2r2u3rd2l3dr2uldr2ulu2ldr2dl2u2rd2rd
{4,13,8,7,10,6,2,9,5,0,14,11,12,15,1,3}   0.30s  42 (67) [55]  u2ld2r2dlu2r2ul3d2r3dl3u3rd3rul2ur3dl2ur2ul3d2ru2rdldruldrdr
{4,5,11,13,3,7,8,12,0,14,2,6,10,15,1,9}   0.14s  38 (66) [56]  r2dl2ur3ul2uld2r3u2l3d2r3u2l3dr2d2rul2dr2ul3dr3u2l2dldrur2d
{15,4,3,6,2,7,5,1,8,11,0,14,13,9,10,12}   2.36s  36 (52) [48]  lurul2dr3ul2dr2dl3ur3dlu2l2drd2ru2l2drdru2rdlurd2
{7,10,13,5,6,8,11,0,1,2,12,14,3,4,15,9}   0.31s  42 (62) [58]  l2d2r2u2l2ur2d2l2u2rd3lu2ldr2dl2urdrulu2ldrurd3ru3ldld2rurd
{10,9,0,11,1,6,15,7,4,5,2,12,14,13,3,8}   2.02s  34 (58) [52]  l2d2r2ul2urd3ru3l2d2r3u2l2d3ru3ld2rulurdrd2l3ur2urd2
{4,15,14,8,10,9,3,12,7,6,13,0,2,11,1,5}   0.14s  43 (71) [57]  ulul2d3r2u2l2d2r2u3l2d2r2ul2d2r3u2l3d2r2u2ld2luru2ld2rur2dluruld2rd
{9,7,15,12,8,6,13,5,14,2,11,1,4,0,10,3}   0.50s  44 (70) [60]  lu2r3dl3ur3ul2drdl2u2rd3r2ul2dlu3r2d3lu3rd2ru2ld2rul2d2rurdlurd
{3,5,14,4,0,10,12,7,15,9,6,11,2,1,13,8}   0.06s  29 (45) [45]  d2ru2ld2ru3ld2r2u2l2drd2ru2ldldrur2dl2u2r2d2
{5,15,3,10,9,8,7,14,4,13,12,2,0,1,6,11}   1.88s  40 (59) [51]  rur2ul3d2r3u3ld2lu2ld2ru2ldr3dldrul2dru2rul2d2ruruld2rd
{5,8,7,2,14,15,12,10,0,6,9,1,4,11,13,3}   0.11s  40 (68) [54]  r3uld2l2ur3dl2u2ld2ru3rd3lu3r2d2l3u2r3d2lu2rd2luld2rurdl2ur2d
{3,6,15,14,7,9,11,10,2,1,13,5,0,12,8,4}   0.06s  40 (69) [57]  uruldr3ul3dr3dl3u2r2urd3lu3ld3ru3rd3lu3l2drd2lur2u2ld2ldr3
{4,8,13,1,11,7,12,10,2,3,0,14,6,5,9,15}   0.95s  39 (70) [50]  ul2d2r3ul2u2r2dl3ur3dl3ur2dl2d2ru3rd3lu2ld2r3u3l2d3rulu2rdrd2
{3,12,0,11,10,5,7,14,6,13,2,15,8,9,4,1}   0.45s  41 (66) [56]  l2d3rulur2d2ru3l3d3r3u3ld3rul2dr2u2ldlu2r2dldluruldlur2d3r

```

STM-optimal solutions are quite a bit slower.. With move counts again shown as mtm (stm) [else]:

```txt

{13,10,11,6,5,3,1,4,8,0,12,2,14,7,9,15}   2.42s  38 (45) [45]  lu2rdruldlur3d2l2uldrdr2ulurdl2dlururuldrdrd
{9,2,15,13,7,4,12,6,8,1,0,14,5,10,3,11} 196.23s  38 (54) [54]  ul2ur3dluld2lur2urdld2ru2ldlu2rd3luldr3u2l2uld2rdr2
{4,8,7,12,5,0,13,15,9,1,6,3,11,14,10,2}  46.23s  30 (48) [48]  dr2dl3u2rdru2l2drdr2dl2ur2u2l2d2rdru3l2dld2r3
{7,10,11,1,0,9,3,4,5,8,13,2,14,6,12,15}  71.92s  37 (47) [47]  d2rurdluldru3ld2rururd2ldrululurdruldl2ur3d3
{14,11,8,15,12,5,13,3,6,2,9,0,1,7,10,4} 5.5 hrs! 43 (61) [61]  dlu2l2drdru2ldru2rdlul2d3r2u2rd2lu3rdldrul2uld2ru2ld3rur2d
{4,12,15,9,2,13,14,3,5,7,8,6,11,1,10,0} 657.56s  44 (56) [56]  ul2urul2d2rdlur2urdl2ururdluldrd2ruldlu2ldr3u2l3drdrdr
{12,14,2,11,1,7,0,10,6,5,13,4,8,9,15,3}  48.39s  39 (51) [51]  luld2ruld2rur2dl2ulururdrdl2ururd2l2ururd2lul2d2r3
{4,13,8,7,10,6,2,9,5,0,14,11,12,15,1,3} 462.69s  41 (55) [55]  u2ld2rurul2d2r2dru2ldru2ldldrdl2uru2rd3lu2lur2drd2lurd
{4,5,11,13,3,7,8,12,0,14,2,6,10,15,1,9} 388.74s  40 (56) [56]  r2dl2ur3ul2uld2rururdlul2d2r2uldrdru3l3d2rdruldlur3d
{15,4,3,6,2,7,5,1,8,11,0,14,13,9,10,12}  36.92s  36 (48) [48]  lurul2dr3ul2drdl2urdr2ul2d2rurulul2d2r2uld2rurd
{7,10,13,5,6,8,11,0,1,2,12,14,3,4,15,9}  98.28s  42 (58) [58]  luld3lu2rd2ru2ldr2dl2ur2ulul2d3ruru2rdl3dru2rdluldrdr2d
{10,9,0,11,1,6,15,7,4,5,2,12,14,13,3,8} 421.03s  36 (52) [52]  l2d2rdru2l2drdrurululd3ru3ld2rurul2dr2d2l3ur2urd2
{4,15,14,8,10,9,3,12,7,6,13,0,2,11,1,5} 134.48s  44 (57) [57]  ldlurdru2lul2drd2r2ulu2ld2ldr2u2l2d2r2ululurdldrur2uld2rd
{9,7,15,12,8,6,13,5,14,2,11,1,4,0,10,3} 678.39s  43 (60) [60]  lurur2d2l2u2ldrururd2ldrulu2rd2l2u2r2dld2ru2l3urd3lu2rdrdr
{3,5,14,4,0,10,12,7,15,9,6,11,2,1,13,8}   4.78s  29 (45) [45]  d2ru2ld2ru3ld2r2u2l2drd2ru2ldldrur2dl2u2r2d2
{5,15,3,10,9,8,7,14,4,13,12,2,0,1,6,11}  60.02s  40 (51) [51]  ruldr2urul2urd2lurdru2ldrd2lul2urd2ru2rul2dlurd2r2d
{5,8,7,2,14,15,12,10,0,6,9,1,4,11,13,3}  68.48s  38 (54) [54]  r2ul2drdru2rd2lululd2rulur3dl2u2rdlur2dldrul3ur3d3
{3,6,15,14,7,9,11,10,2,1,13,5,0,12,8,4} 145.20s  43 (57) [57]  u2rdrurd2lu2rd2l3urdlu2r2urdlul2drdrurd2lu3ld3rulurdrd
{4,8,13,1,11,7,12,10,2,3,0,14,6,5,9,15}  17.14s  37 (50) [50]  rulul2druld3r2u2ldr2ulurdl2urdl2urdld2rurul2d2r3
{3,12,0,11,10,5,7,14,6,13,2,15,8,9,4,1} 278.75s  43 (56) [56]  ldld2rur2dluruldl2ur2drdlul2uruldr2urd2lu2rd2lu2ld2ldr3

```

MTM-optimal solutions take even longer to find, so much so that I only did the first!

```txt

puzzle = {13,10,11,6,5,3,1,4,8,0,12,2,14,7,9,15}
mtm-optimal solution of 30 moves found in 2 hours, 13 minutes and 18s: lu2r2d2l2u2r3dl3ur2d3l2ur3ul3drurd2ru3ld2lu2r2d3

```

