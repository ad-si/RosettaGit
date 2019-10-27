+++
title = "Talk:Sorting algorithms/Selection sort"
description = ""
date = 2018-09-21T23:25:51Z
aliases = []
[extra]
id = 21275
[taxonomies]
categories = []
tags = []
+++

==Page update of 15 January==
I reverted the change by RomilAditya as they replaced the entire page with their new solution:

<code>
-module(solution). -import(lists,[delete/2,max/1]). -compile(export_all). selection_sort([],Sort)-> Sort; selection_sort(Ar,Sort)-> M=max(Ar), Ad=delete(M,Ar), selection_sort(Ad,[M|Sort]). print_array([])->ok; print_array([H|T])-> io:format("~p~n",[H]), print_array(T).

main()-> Ans=selection_sort([1,5,7,8,4,10],[]), print_array(Ans).
</code>


Not sure what language it is... --[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 11:52, 15 January 2017 (UTC)

: The computer programming language is:     '''XPL0'''         -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:25, 21 September 2018 (UTC)
