+++
title = "Talk:Minesweeper game"
description = ""
date = 2010-12-25T11:15:24Z
aliases = []
[extra]
id = 7719
[taxonomies]
categories = []
tags = []
+++

==Too long?==
It took several hours to write the task description and to code this tasks initial Python solution. I will probably set the sample output to scroll when the page gets too long, but other language examples may well have to be sub-pages. 

I freely admit that the task was chosen with an eye to attracting those new to programming :-)
 --[[User:Paddy3118|Paddy3118]] 10:58, 10 July 2010 (UTC)

:Sometimes the Python version exceeds the standard [http://effbot.org/pyref/sys.getrecursionlimit.htm recursion limit] (1,000) on my machine when clearing the first grid point:


```txt

There are 4 true mines of fixed position in the grid

......
......
......
......
m x y/c x y/p/r: c 2 4
Traceback (most recent call last):
  File "/Users/martin/mine.py", line 125, in <module>
    clear(x,y, showgrid, grid, mines, markedmines)
  File "/Users/martin/mine.py", line 97, in clear
    clear(xx, yy, showgrid, grid, mines, markedmines)
  File "/Users/martin/mine.py", line 97, in clear
    clear(xx, yy, showgrid, grid, mines, markedmines)
  File "/Users/martin/mine.py", line 97, in clear
    clear(xx, yy, showgrid, grid, mines, markedmines)
(repeats a zillion times...)
  File "/Users/martin/mine.py", line 86, in clear
    for xx in (x-1, x, x+1)
RuntimeError: maximum recursion depth exceeded while getting the str of an object

```


:I suppose that's a bug? Otherwise, I'm pretty impressed with the brevity and readability of the Python solution! --[[User:Morn|Morn]] 11:15, 25 December 2010 (UTC)

== Differences in behavior from classic game ==
The clearing criteria seems wrong or ill-defined.  In the classic game if you clear a square that has one or more adjacent mines no further clearing happens.  If the square has zero adjacent mines a clearing is opened to an edge of non-zero numbers.  (The small size of the board and mine probability makes it hard to properly demonstrate the clearing criteria) --[[User:Dgamey|Dgamey]] 09:55, 11 July 2010 (UTC)

: I must say that I just made up what I thought was the clearing algorithm on games I had played some time ago, and I was probably unconsciously trying to minimise complexity in an already large task. Could we go with this simpler clearing strategy, at risk of impairing the playability of the game? --[[User:Paddy3118|Paddy3118]] 11:01, 11 July 2010 (UTC)

:: The classic clearing criterion is to only auto-reveal cells if they have no mines in the neighbors at all. It's pretty trivial to do this given that you need to calculate that figure anyway for display on the cell, and you can just call the clearing function (safely) recursively. (It gets slightly tricker with a very large grid since it's possible to get into problems with recursion depth, if that's something your language implementation imposes a cap on, but that's a refinement really.) –[[User:Dkf|Donal Fellows]] 11:37, 11 July 2010 (UTC)

:: Why must it be recursive clearing?  Not all languages support recursion. Even for the large grid in a classic game 16 x 30 it wouldn't too bad.  --[[User:Dgamey|Dgamey]] 13:27, 11 July 2010 (UTC)
::: Doesn't have to be, but it ''is'' easy for (almost) everyone to implement. –[[User:Dkf|Donal Fellows]] 14:10, 11 July 2010 (UTC)
:::: There's also the loop/recursion transform. --[[User:Short Circuit|Michael Mol]] 15:50, 11 July 2010 (UTC)
:: I'd suggest making the "resursive clearing" feature optional. That enables the bulk of the task, and lets anyone sufficiently clever come along and handle the part that may be  more difficult for some languages. --[[User:Short Circuit|Michael Mol]] 15:50, 11 July 2010 (UTC)

::: Oh! I thought that we were just discussing what the clearing ''is'' rather than whether it should be left out?  don't think it would be much of a game without a clearing feature of some sort.  --[[User:Paddy3118|Paddy3118]] 16:50, 11 July 2010 (UTC)
:::: I've played implementations that didn't recursively clear, but it sounded to me like Dgamey was concerned about the task definition either being unclear, incorrect and/or limiting the use of languages which can solve the task in "spirit" (i.e. allow one to play a game recognizably like Minesweeper), but not easily solve it in its entierity. I was pondering a compromise, but I may have misinterpreted the conversation. --[[User:Short Circuit|Michael Mol]] 22:40, 11 July 2010 (UTC)
:::: I was referring to the HOW the clearing was done (i.e. recursion/iteration) rather than IF.  Yes it should be a requirement.  Just not how. --[[User:Dgamey|Dgamey]] 03:01, 12 July 2010 (UTC)

:: The grid size and mine density chosen in the task (i.e. 6x4 and from 4 to 12 mines) won't show off the recursive clearing well.  There won't be many squares with 0 adjacent bombs which is what is needed for that clearing is supposed to kick in.  The classic game was (a) 9x9 10 mines, (b) 16x16 and 40 mines, and (c) 16x30 and 99 mines.  I suggest making the values of the starting parameters a suggestion.  --[[User:Dgamey|Dgamey]] 03:01, 12 July 2010 (UTC)
::: I agree that the grid size is too small to really bring out the clearing routine, but I thought that this would have a major affect on the size of entries. Sample output would be large no matter how short the actual implementation. I like the idea of changing the explanation of clearing to a suggestion though. --[[User:Paddy3118|Paddy3118]] 04:41, 12 July 2010 (UTC)  

FWIW, the ''real'' difference between this task and the classic game is that in the classic implementation, the first square you clear is ''never'' a mine; if there is a mine there, the game moves it away. In fact, there was (is?) a cheat which lets you find out where the mines are, and you can use that to confirm it yourself. (Alas, I don't remember the details of the cheat; it was a long time ago.) The moving of a mine only ever worked for the first square you cleared though. I don't suggest that anyone implements it, but it's one of the marks of the real original; it had a lot more thought applied to it than it appeared to have at the time. –[[User:Dkf|Donal Fellows]] 23:31, 11 July 2010 (UTC)
: Interesting feature.  I believe the cheat was taken away before XP.  --[[User:Dgamey|Dgamey]] 03:01, 12 July 2010 (UTC)
:: Could well be. It relied on being able to toggle a pixel in the corner of the screen directly, which I can imagine didn't fit well with the display management model updates that happened in the NT line of kernels. –[[User:Dkf|Donal Fellows]] 08:42, 12 July 2010 (UTC)
::: With Windows, that's abstracted away with Win32 (the API set, which oddly was available on Windows 3.1 and earlier). All graphic access in Windows prior to DirectX is done was a device-neutral fashion; a program could conceivably operate the same way on a printer framebuffer as it did on a monitor.  The call sequence is the same (Get a handle to the device context, get a handle to the bitmap for the device context, lock the bitmap to get a buffer pointer, unlock bitmap, unlock the device context.), and any Win32 app can grab the NULL DC, which is assumed by the system to mean the full desktop. (Pretty easy way to test your coord-calculating logic in your code, actually.)
::: The easter egg apparently disappeared in XP SP2, which had a lot of security improvements. If I had to guess, I'd say that corner pixel violated some security policy that didn't have a basis in technical limitations, like covering part of the screen that didn't belong to the app and didn't serve a core app role. --[[User:Short Circuit|Michael Mol]] 12:34, 12 July 2010 (UTC)

60% seems too high. --[[User:Rdm|Rdm]] 17:35, 21 July 2010 (UTC)
: Agreed. Based on Dgamey's values above perhaps the task spec of 20-60% mines should be changed to 10-20%.--[[User:Tikkanz|Tikkanz]] 21:36, 21 July 2010 (UTC)
:: I suggest making the revision optional so as not to invalidate original entries. --[[User:Dgamey|Dgamey]] 01:22, 22 July 2010 (UTC)
