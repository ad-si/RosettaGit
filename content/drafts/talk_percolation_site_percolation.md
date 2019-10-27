+++
title = "Talk:Percolation/Site percolation"
description = ""
date = 2013-09-12T02:48:54Z
aliases = []
[extra]
id = 16176
[taxonomies]
categories = []
tags = []
+++

==Tcl percolation?==
Hi, it seems as if the Tcl output does not show a ''successful'' percolation through a 15x15 grid? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 16:12, 8 September 2013 (UTC)
: Yes. It's random, and around the threshold level where it can go either way. Requiring a ''successful'' percolation is lame. I'll remove that word from the task description. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 23:26, 8 September 2013 (UTC)

::Why remove that feature Donal? Seeing a successful percolation is much more involving for the viewer than not. Also the other examples have that feature, making Tcl the odd one out? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 01:27, 9 September 2013 (UTC)
::: Because it's a foolish requirement. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 09:44, 9 September 2013 (UTC)

::: I can't see the foolishness of the requirement to print a percolating example over printing an example which does not percolate. Would anyone else care to put their views as well as Donal? Thanks. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 15:38, 9 September 2013 (UTC)

I can live with your amended wording ''"Optionally depict a percolation through a cell grid graphically."'' Although I would have preferred to emphasize that if there is no through path then there is no percolation. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 02:59, 11 September 2013 (UTC)

==Granularity and tries number==
The increase of granularity and tries number in the D and C entries was just to make this task a little more interesting. Requiring a fixed number of simulations in the task description is silly. The simulation outputs are similar, just with more significant digits. Different languages as Python and D have different advantages, and the D entry is designed to be faster. If you don't allow a higher number of simulations, those speed optimizations become useless, and using D instead of Python to run a simulation becomes not much useful. One of the points in using a language as C/D to perform this task is to run a higher number of simulations compared to a Python implementation. So please allow a higher number of tries in the task description. -[[User:Bearophile|bearophile]] ([[User talk:Bearophile|talk]])

:SOunds great. Done! --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 02:48, 12 September 2013 (UTC)
