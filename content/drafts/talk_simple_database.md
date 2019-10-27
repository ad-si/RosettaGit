+++
title = "Talk:Simple database"
description = ""
date = 2014-03-29T15:47:31Z
aliases = []
[extra]
id = 10751
[taxonomies]
categories = []
tags = []
+++

==Latest entry?==
Does latest entry mean the most recent entry made or the most recent entry date? For Erlang I have implemented ''made'', but I can change.
If it is the latter (date), what about many entries the same date? All of them or the latest made?

==Name change?==
The task seems to be about creating a database but does not use that word in the title? How about re-naming the task "Simple database". --[[User:Paddy3118|Paddy3118]] 09:57, 1 November 2011 (UTC)
: your are right. in fact the exercise from a book that inspired this task is also called simple database.--[[User:EMBee|eMBee]] 10:13, 1 November 2011 (UTC)

== command line interface ==

Presumably it does not matter if the command line interface uses a command line supplied by the application, the language or the operating system?  --[[User:Rdm|Rdm]] 11:59, 1 November 2011 (UTC)
:well, one point of the task is to tie in other tasks like the use of serialization and also reading commandline options. i think building an interface on top of a REPL is worth a separate task: [[Readline interface]] (build a simple application with a readline interface.) the functionality of this program may be taken from [[Simple database]])--[[User:EMBee|eMBee]] 06:20, 2 November 2011 (UTC)
:: In general, the way I'd do it is to write a library that implements the functionality as a collection of procedures(/classes/whatever) and then an application or two that wraps that into a true command-line interface, an interactive shell, a GUI, a webapp, etc. as necessary. (Well, I wouldn't normally bother with writing an interactive shell as my preferred language already has one, but you get the picture.) Dividing functionality from (user-facing) interface makes testing much easier too. –[[User:Dkf|Donal Fellows]] 09:05, 2 November 2011 (UTC)

== does J use a human readable database? ==

could you please describe in more detail how the J solution meets the 'human readable database' requirement? the file containing the data itself should be directly readable without additional program support. 

using a database dump would be acceptable if it is created every time the database is saved and read back in when the database is loaded.--[[User:EMBee|eMBee]] 03:46, 8 February 2012 (UTC)

:Inserted database dump.--[[User:Lambertdw|Dave]] 16:27, 9 February 2012 (UTC)
:: thanks. i attribute it to my inability to read the J code to figure that out myself--[[User:EMBee|eMBee]] 17:04, 9 February 2012 (UTC)

== The Lisp code is bugged ==

Attention is needed to the common lisp code of this task as it is not properly implemented.
: could you please explain what's wrong with it?--[[User:EMBee|eMBee]] 23:19, 11 March 2012 (UTC)

== The end of a phrase is nigh ==
I noticed that the task description says ''"... and saved to disk"''. Will that become "and saved to ssd" or "and saved to secondary storage" soon?

Personally I would like the phrase to linger long after home computers stop using rotating disk. --[[User:Paddy3118|Paddy3118]] 20:50, 6 July 2012 (UTC)

== The C code is bugged ==

The C code is bugged

Compilation with GCC version 4.8.1 (Ubuntu/Linaro 4.8.1-10ubuntu9):

'''gcc simple-database.c -o simple-database'''

 simple-database.c:188:12: error: conflicting types for ‘by_date’
 static int by_date (pdb_t *p1, pdb_t *p2) {
            ^
 simple-database.c:31:13: note: previous declaration of ‘by_date’ was here
 static sort by_date;
             ^
 simple-database.c:31:13: warning: ‘by_date’ used but never defined [enabled by default]


Compilation with clang version 3.2-7ubuntu1 (tags/RELEASE_32/final) (based on LLVM 3.2) give the same result:

'''clang simple-database.c -o simple-database'''

 simple-database.c:188:12: error: conflicting types for 'by_date'
 static int by_date (pdb_t *p1, pdb_t *p2) {
           ^
 simple-database.c:31:13: note: previous declaration is here
 static sort by_date;
            ^
 1 error generated.

:Should be fixed. The [http://rosettacode.org/mw/index.php?title=Simple_database&diff=155789&oldid=153280 last editor] forgot to change the parameter types for the function at the bottom.
:Valgrind reports an invalid read (also if I run the [http://rosettacode.org/mw/index.php?title=Simple_database&oldid=133351#C original code]) but I'm not in the mood tonight to investigate further :-). --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 21:07, 26 March 2014 (UTC)
:: I've made some changes. Now code compiles and runs without segmentation fault. Please check consistency of code style. Unfortunately that "simple DB" not usable for me, because keeps all data in memory and consumes five times more memory compared to the size of the file. --[[User:Yamiyam|Yamiyam]] ([[User talk:Yamiyam|talk]]) 15:47, 29 March 2014 (UTC)
