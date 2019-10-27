+++
title = "Talk:Zhang-Suen thinning algorithm"
description = ""
date = 2014-06-18T20:20:04Z
aliases = []
[extra]
id = 16509
[taxonomies]
categories = []
tags = []
+++

==Inverted axis?==
Could someone help explain why, in the example image the following
transformation occurs (in the periods separating the R and C, and after the
C):

```txt

.....    .....
.###. -> .....
.#?#.    ..#..
.....    .....

```

Surely the the cell labelled '?' will be culled at step 1:
 - It is black with 8 neighbours
 - B = 5 (2 <= 5 <= 6)
 - A = 1
 - At least one of P2 P4 P6 is white (P6 is white)
 - At least one of P4 P6 P8 is white (P6 is white)
Why isn't it whitened at step 1?

--[[User:Tim-brown|Tim-brown]] ([[User talk:Tim-brown|talk]]) 17:33, 15 October 2013 (UTC)

:I expect that there are removals of some of those surrounding cells before it gets to your '?' cell which affects the final outcome. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 18:54, 15 October 2013 (UTC)

::I'm considering what happens to this individual cell (not necessarily its neighbours). As far as I am concerned, the situation I describe above is the calculation for Step-1 of the first iteration. Nothing has changed (i.e. been removed) before this: all changes are stored, and applied `after` the analysis. So the step-1 rule should apply to cell '?'. And it should be blank (by my interpretation of the rules). But it ain't.
--[[User:Tim-brown|Tim-brown]] ([[User talk:Tim-brown|talk]]) 19:27, 15 October 2013 (UTC)

You are right the table given of P1 to P9 in the task page is actually transformed but ins such a way that the output is thinned in the same way (but with an offset possibly). I am actually calculating with:
<table border="1">
  <tr><td>P7</td><td>P6</td><td>P5</td></tr>
  <tr><td>P8</td><td><b>P1</b></td><td>P4</td></tr>
  <tr><td>P9</td><td>P2</td><td>P3</td></tr>
</table>
I've got an inverted vertical axis it seems. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 22:32, 15 October 2013 (UTC)
:Now I'm a bit more confident in posting my Racket solution! ---[[User:Tim-brown|Tim-brown]] ([[User talk:Tim-brown|talk]]) 09:50, 16 October 2013 (UTC)

Is the thinned version of the big "R.C." in the task description correct?
I have it as:

```txt
...........................................................
...........................................................
....#.##########.......................#######.............
.....##........#...................####.......#............
.....#..........#.................##.......................
.....#..........#................#.........................
.....#..........#................#.........................
.....#..........#................#.........................
.....############...............#..........................
.....#..........#...............#..........................
.....#..........#................#.........................
.....#..........#................#.........................
.....#..........#................#.........................
.....#............................##.......................
.....#.............................############............
.......................###..........................###....
...........................................................
...........................................................
```

---[[User:Tim-brown|Tim-brown]] ([[User talk:Tim-brown|talk]]) 10:05, 16 October 2013 (UTC)

: I've corrected that too Tim. Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 22:30, 18 October 2013 (UTC)


==Three Data Files==
To avoid every entry to needlessly contain the same large tables, I suggest to add to the task description three links to the tree files of the test cases. -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])
:I think if the outputs are typically in ./# form, the inputs probably should be too, rather than csv. --[[User:TimToady|TimToady]] ([[User talk:TimToady|talk]]) 15:53, 21 October 2013 (UTC)

==Mathematica's in-built algorithm==

I converted Mathematica's ones and zeros to something I could see and it seems that their in-built thinning algorithm does not give the same result as that of Zhang Suen.

I think that it is right and proper to show the output of the in-built function as I would expect that to be used, but I also asked for an implementation of the ZS algorithm as I maked it incomplete. 

If you think that is wrong then just give your argument please.

The following is just a code dump of how I converted to ascii art for comparison using Python:


```python>>>
 print('''
{
{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
{0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0}, 
{0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0}, 
{0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0}, 
{0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
{0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
{0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0}, 
{0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0}, 
{0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0}, 
{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}}'''.replace('{', '(').replace('}', ')'))

(
(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0), 
(0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0), 
(0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0), 
(0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
(0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0), 
(0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0), 
(0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0), 
(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
>>> inp = (
(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
(0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
(0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0),
(0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
(0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0),
(0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0),
(0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0),
(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
>>> print('\n'.join(''.join(('#' if x else ' ') for x in line) for line in inp))
                                
 #########       ########       
 ###   ####     ####  ####      
 ###    ###     ###    ###      
 ###   ####     ###             
 #########      ###             
 ### ####       ###    ###      
 ###  ####  ### ####  #### ###  
 ###   #### ###  ########  ###  
                                
>>> print('\n'.join(''.join(('#' if x else ' ') for x in line) for line in out))
                                
   ####            ###          
  #    #          #   ###       
  #     #        #              
  #     #        #              
   ##  #         #              
  #  ###         #              
  #     #         #    ##       
         ## ##     ####    ##   
                                
>>> 
```

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:20, 18 June 2014 (UTC)
