+++
title = "Rosetta Code:Village Pump/Suggest a programming task"
description = ""
date = 2019-07-04T14:53:23Z
aliases = []
[extra]
id = 1559
[taxonomies]
categories = []
tags = []
+++

{{#set:is contribution page=true}}[[Category:Requesting Content]]So 
you want to see a problem solved? If you're not comfortable creating the task page yourself, feel free to edit this page, and describe the problem below. 
(To edit this page, click the "edit" tab at the top.) 
You also might want to check out the [[:Category:Draft Programming Tasks|draft tasks]] to see if someone has already suggested one and partially produced the page; if so, just help them out by supplying what's needed to take the page from draft to full task.

When making a request, please place it in the [[#Unsorted|Unsorted]] section. 
Also, feel free to review the others that are already here; help sort them into the other categories, based on the category descriptions. 
(If we need another category or two, you can create them.)

=Incomplete=

==General==
* Benchmarks
** Find out the relative speeds of two or more different pieces of code, which perform the same calculation in different ways.  Each piece of code should be iterated enough that it runs for at least one second.
** Show how to run a profiler for a program written in your language.  The program that's profiled should be one of the RC tasks.  Show the commands which will show the top 10 most frequently run lines of code in the program.
* Date Calculations
** Calculate Self-describing numbers
** Calculate age given a birthdate
** Calculate number of [years|days|hours|minutes|seconds] since some date or time
** Calculate difference between two times
** Sum a time and a time interval
** Calculate day of week given a date
** Calculate day of month given a date
** Calculate day of year given a date
** Calculate [first|second|third|fourth|last] [Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday] of month
** Calculate number of [Sundays|Mondays|Tuesdays|Wednesdays|Thursdays|Fridays|Saturdays] in a given month or year
** Display a date in various formats
** Something with [http://terrancalendar.com the Terran Computational Calendar] (and contribute to [https://github.com/terrancalendar/terrancalendar their Github project])
* Show creation, element insertion, element removal and enumeration of a "set" type that enforces constraints as described [[Talk:Symmetric difference#Set_type|here]]. (An invariant analog would be nice as well.)
* The [[:Category:Object oriented|Object oriented category]] is missing a lot of the basics like [[calling a method]].
* Explicit implementation of various [[wp:Category:Software design patterns|design patterns]]
** Multiple dispatch (aka [[wp:Multimethods|multimethods]])
*** Written in such a way that languages don't necessarily have to use multi-methods if they don't have them? --[[User:Paddy3118|Paddy3118]] 19:46, 23 July 2010 (UTC)
**** I'd be in favor of that. I've written that one myself. --[[User:Short Circuit|Michael Mol]] 02:43, 24 July 2010 (UTC)
* Closures
* an encryption program
** What sort of encryption? We've already got tasks for using SSL connections (e.g., [[Client-Authenticated HTTPS Request]]) so maybe you're after message-level encryption? Perhaps a simple [[wp:Data Encryption Standard|DES]] implementation would be apt. (The advantage of that is that it is not particularly strong itself, and so doesn't run close to the legal issues in this area, but points the way towards real production encryption.) –[[User:Dkf|Donal Fellows]] 09:17, 20 January 2010 (UTC)
** See Playfair below --[[User:Brnikat|Brnikat]] ([[User talk:Brnikat|talk]]) 15:09, 10 July 2015 (UTC)
* Monads
* Coroutines/[[Generator]]s
* Sentinel value -- What values are commonly used for sentinel purposes. (This may end up beyond language comparison and delve into protocols) --[[User:Short Circuit|Short Circuit]] 18:09, 9 June 2009 (UTC)
* List all duplicated files of a given path. A duplicated is an exact copy, having the same checksum.
** Perhaps [[Null]] should be generalized to cover this? --[[User:Kevin Reid|Kevin Reid]] 21:27, 29 August 2009 (UTC)
* Applying a list of functions to a value. (As different from applying a function to a list of values.)[[User:Rahul|Rahul]] 21:23, 9 December 2008 (UTC)
::See [[First-class functions]] --[[User:Paddy3118|Paddy3118]] 15:34, 24 February 2009 (UTC)
::: Not entirely appropriate.  Nothing prevents one from having a list of function pointers in C or C++ to apply to the same pointer or reference to the value in question.  However, there appears to be significant discussion whether C is even appropriate in the task you link to. --[[User:Short Circuit|Short Circuit]] 07:52, 25 February 2009 (UTC)
* Parallelized version of [[Lucas-Lehmer test]].
* Comparison methods and operators
:Could this be added to [[Comparing two integers]]? --[[User:Mwn3d|Mwn3d]] 07:28, 21 December 2007 (MST)
* Type-variant variables in type-safe languages. (i.e. [http://msdn.microsoft.com/en-us/library/ms221258(VS.80).aspx Microsoft's VARIANT].)
: ''Type-variant'' rather suggests dynamic polymorphism, i.e., when the specific type of the value (and the value itself) depends on the type tag of the object. MS VARIANT is rather a different thing. It is a union, a container type which content depends on the value of the constraint. This is also a form polymorphism, but different, a dynamically constrained type. The type of the object does '''not''' vary. A similar case represents unbounded array, which size depends on the actual bounds (the constraint). (Granted, MS VARIANT serves the purpose of dynamic polymorphism, but that is merely because MS wished to keep it conform to non-[[object-oriented|OO]] languages.) --[[User:Dmitry-kazakov|Dmitry-kazakov]] 13:24, 9 April 2009 (UTC)

* I don't know where this should go, but it would be great to have a use comparison of regular expressions [[Special:Contributions/187.37.58.35|187.37.58.35]] 18:40, 3 November 2009 (UTC)
:Is [[Regular expression matching|this]] close? --[[User:Mwn3d|Mwn3d]] 20:22, 3 November 2009 (UTC)
:: Probably not quite what he's looking for. Different regex implementations (i.e. .Net's, Perl's, sed, Visual Studio's, etc.) have different extensions and escape sequences. It's likely he's looking for a comparison as though each variant of regex were treated as being distinct languages. ''Accessing'' the implementation is one thing, but performing various supported operations is another. --[[User:Short Circuit|Michael Mol]] 21:50, 3 November 2009 (UTC)
::: [[POSIX]] specifies two levels of regular expressions, and then there's additional levels beyond that (c.f. [[Perl]] and [[PCRE]]). There's also a lot of disagreement between different camps here over the type of automaton that should be used in the implementation. It's a mess to be honest (as extensions beyond POSIX EREs aren't standardized). Because of that, if you're going to delve deeper into REs, I advise picking one of the POSIX levels and explicitly stating that that level only be supported. –[[User:Dkf|Donal Fellows]] 23:03, 3 November 2009 (UTC)
:: yea in a long term it would be great to have something like [[User:Short Circuit|Michael Mol]] said but that's a very nice start, thanks [[Special:Contributions/187.37.58.35|187.37.58.35]] 14:39, 4 November 2009 (UTC)
::UPDATE: now I noticed this page [[Regular_expression_matching]] is there for quite a time, but I didn't found it in the 'by Task' link, that's why I posted, also searching by "Regular expression" in the search box results nothing.. [[Special:Contributions/187.37.58.35|187.37.58.35]] 16:17, 4 November 2009 (UTC)
:::That task is listed under [[:Category:Text processing|Text processing]] on the Solutions by Task page. You can also look at [[:Category:Programming Tasks]] to see a list with no sub-categories. I can set up a redirect from "Regular expressions" for now, though it seems like we need more tasks to cover REs. --[[User:Mwn3d|Mwn3d]] 16:20, 4 November 2009 (UTC)
* Simple, common requirements for 3D animation, including running an operation consistently N times per second, generate and draw basic geometric shapes like spheres, cubes and cylinders, load and display a point mesh and apply a texture to a quad.  I'll host any desired data on the server. --[[User:Short Circuit|Michael Mol]] 08:29, 13 November 2009 (UTC)

* Produce an SVG showing a cumulative distribution function - on the x axis show the number of programming tasks completed, on the y scale show the proportion of programming languages on rosetta code that have completed less than or equal to that number of tasks. Maybe also plot the cdf of some standard distribution on the graph as well. (Weibull?) --[[User:Rldrenth|Rldrenth]] 00:43, 22 December 2009 (UTC)

* An example of lexical and dynamic variable binding in various languages.  For example, in lisp the let construct and also let with variables declared as special would be useful to see this in other languages, such as Tcl and Python.  [[User:WilliamKF|WilliamKF]] 18:03, 4 January 2010 (UTC)

* [http://thedailywtf.com/Articles/Avoiding-the-Splice.aspx Avoiding the Splice]

* Modules. This basic syntax task would cover C's #include, Objective-C's #import, Python's import, Java's package/import, etc.  It would also cover symbol visibility control in compilation units (private/public, static/export, etc.).

* Zipping sequences (calculating their [[wp:Convolution (computer science)|convolution]]) and unzipping

* cat & tee: copy input to output, copy input to multiple outputs. I'm not sure of the best name for these tasks ("IO/cat" & "IO/tee"? "copy input to output"?). [[Input loop]] has some solutions that implement the first, but it doesn't explicitly say what should be done with input, and some solutions to `cat` and `tee` may not use loops (which is what's interesting about these tasks).

* We should maybe double check whether we are missing any tasks corresponding to algorithms listed at http://algo-visualizer.jasonpark.me/

* Images: Reading, modifying, and writing either a .PNG/.JPG or other common image type.  (Currently, there's bitmap and PPM stuff, but this task is for somethign more practical.)

* It would be good to have an implementation of the Nelder-Mead gradient descent algorithm. https://en.wikipedia.org/wiki/Nelder-Mead_method  This would be a good candidate for a generic driver (eg:c++ template class) accepting a function and returning the parameter(s) that map to the functions (apparent) minimum.  --[[User:Bitrat|Bitrat]] 14:10, 17 January 2019 (UTC)

==Games==
* Connect Four (or more) with variable and standard game board (6 rows, 7 columns)
*:- but watch for trade mark violations etc --[[User:Paddy3118|Paddy3118]] 17:33, 29 April 2009 (UTC)
*::O.K., the games can be called The Captain's Mistress", because that is the original, much older verson of the "Connect Four" (which was "invented" and copyrighted in 1974 by Milton Bradley, according to [[wp:Connect four|Wikipedia]].) --[[User:Borisbaran|Borisbaran]] 12:33, 3 May 2009 (UTC)
*::: Then why not dots-and-crosses (five-in-a-line): discovered a site which call it this way, but I played it when I was eight y.o., and surely it's a very old game people played with paper and a pen long before anyone thought to register a copyright or what on it; instead of different colors, we can use a cross and a dot, and this is the name I knew it: "puntini e crocette" (dots and crosses)... But I think the interesting part would be to program the A.I. opponent... --[[User:ShinTakezou|ShinTakezou]] 14:07, 5 May 2009 (UTC)
*::: Reading it better... It's Gomoku! (But a paper can be like a board bigger than 19&times;19) --[[User:ShinTakezou|ShinTakezou]] 14:10, 5 May 2009 (UTC)
* [[wp:Dots and Boxes|Dots and Boxes]] is quite a fun game to do, especially since the game has been around for ages and its strategies have been well studied. It's also quite easy to do just the GUI parts of the game, making a simple game for two players. —[[User:Dkf|Dkf]] 12:15, 18 May 2009 (UTC)
*: More possibilities are [[wp:Von Neumann cellular automaton|von Neumann]] and [[wp:Langton's loops|Langton]] cellular automata. They're not really games as such, but they're certainly interesting in similar ways. —[[User:Dkf|Donal Fellows]] 14:58, 9 August 2009 (UTC)
* Magic square: a program that could compute a magic square of any size, whether odd, doubly-even, or singly even.
::* ''magic squares of odd order'' has been implemented.   ''Magic squares of even order''   has no known algorithm and therefore will be problematic to generate. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:41, 20 March 2014 (UTC) 
*Some classic and simple games, such as ''[[wp:Hangman (game)|Hangman]]'', ''[[wp:Snake (video game)|Snake]]'', ''[[wp:Breakout (video game)|Breakout]]'', or ''[[wp:Battleship (game)|Battleship]]''. --[[User:Morn|Morn]] 17:26, 26 December 2010 (UTC)
*[[wp:Rule 90]]
* Perl6 [[chess grammar]], eventually with side effects for move validation and/or adjudication.
* 9x9 sudoku, a program that generates a sudoku box as well as has an option to automatically solve it
* Implement a pacman game in our language. Here is the Assembler-code of the original [http://cubeman.org/arcade-source/pacman.asm]
::Note that that assembler code makes deep use of very specific hardware which is not present on most systems. Just reading it for comprehension would take days of work, for someone like me. --[[User:Rdm|Rdm]] 16:02, 17 December 2012 (UTC)
::And the size of any entry is likely to be too large. --[[User:Paddy3118|Paddy3118]] 16:15, 17 December 2012 (UTC)
::::You are both right, here is a link to a java implementation of pacman [http://zetcode.com/tutorials/javagamestutorial/pacman/]
* Implement a program that generates a random [[wp:Chess960 starting position|Chess960 starting position]]--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 23:56, 18 April 2014 (UTC)
* What about the [[wp:Nim|Nim]] game and the variations found on its wiki page?
:See [[User:DanBron/Game of Nim]] for history... --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:57, 22 May 2015 (UTC)
* Annotate a crossword diagram given in the format


```txt

......#......
.#.#.#.#.#.#.
.......#.....
.#.###.#.#.#.
.....#.......
##.#.#.#.###.
...#.....#...
.###.#.#.#.##
.......#.....
.#.#.#.###.#.
.....#.......
.#.#.#.#.#.#.
......#......

```


and produce a diagram with the numbers in place and a skeleton set of clues in this format


```txt

|--------------------------------------|
|1 |  |2 |  |3 |  |##|4 |5 |  |6 |  |7 |
|  |  |  |  |  |  |##|  |  |  |  |  |  |
|--+--+--+--+--+--+--+--+--+--+--+--+--|
|  |##|  |##|  |##|8 |##|  |##|  |##|  |
|  |##|  |##|  |##|  |##|  |##|  |##|  |
|--+--+--+--+--+--+--+--+--+--+--+--+--|
|  |##|  |##|##|##|  |##|  |##|  |##|  |
|  |##|  |##|##|##|  |##|  |##|  |##|  |
|--+--+--+--+--+--+--+--+--+--+--+--+--|
|11|  |  |  |12|##|13|  |  |  |  |  |  |
|  |  |  |  |  |##|  |  |  |  |  |  |  |
|--+--+--+--+--+--+--+--+--+--+--+--+--|
|##|##|  |##|  |##|  |##|  |##|##|##|  |
|##|##|  |##|  |##|  |##|  |##|##|##|  |
|--+--+--+--+--+--+--+--+--+--+--+--+--|
|14|  |  |##|15|  |  |  |  |##|16|  |  |
|  |  |  |##|  |  |  |  |  |##|  |  |  |
|--+--+--+--+--+--+--+--+--+--+--+--+--|
|  |##|##|##|  |##|  |##|  |##|  |##|##|
|  |##|##|##|  |##|  |##|  |##|  |##|##|
|--+--+--+--+--+--+--+--+--+--+--+--+--|
|17|  |18|  |  |  |  |##|19|  |  |  |20|
|  |  |  |  |  |  |  |##|  |  |  |  |  |
|--+--+--+--+--+--+--+--+--+--+--+--+--|
|  |##|  |##|  |##|  |##|##|##|  |##|  |
|  |##|  |##|  |##|  |##|##|##|  |##|  |
|--+--+--+--+--+--+--+--+--+--+--+--+--|
|21|  |  |  |  |##|22|  |23|  |  |  |  |
|  |  |  |  |  |##|  |  |  |  |  |  |  |
|--+--+--+--+--+--+--+--+--+--+--+--+--|
|  |##|  |##|  |##|  |##|  |##|  |##|  |
|  |##|  |##|  |##|  |##|  |##|  |##|  |
|--+--+--+--+--+--+--+--+--+--+--+--+--|
|24|  |  |  |  |  |##|25|  |  |  |  |  |
|  |  |  |  |  |  |##|  |  |  |  |  |  |
|--------------------------------------|

Across
   1 (6)
   4 (6)
   9 (7)
  10 (5)
  11 (5)
  13 (7)
  14 (3)
  15 (5)
  16 (3)
  17 (7)
  19 (5)
  21 (5)
  22 (7)
  24 (6)
  25 (6)

Down
   1 (5)
   2 (7)
   3 (7)
   5 (9)
   6 (5)
   7 (7)
   8 (11)
  12 (9)
  14 (7)
  16 (7)
  18 (5)
  20 (5)
  23 (3)

```

where the bracket numbers are the lengths of the corresponding solutions.

A much harder task would be, given a set of simple definition type clues and the language used, solve the crossword.

--[[User:Brnikat|Brnikat]] ([[User talk:Brnikat|talk]]) 15:03, 10 July 2015 (UTC)


### Database / Network

* Simple DB connection and queries.
* Certificate-authenticated SSL.
* Secure Socket Layer.
* Simple command line client for [[wp:Extensible Messaging and Presence Protocol|XMPP]] (Extensible Messaging and Presence Protocol) .


###  Data structures and algorithms 

* Implement a finite state machine that verifies that a string contains a valid binary number. This is already done [https://sites.google.com/site/opensourceconstriubtions/ettl-martin-1/articles/how-to-determine-a-string-represents-a-binary-number-using-a-finite-state-machine here].
* Implement [http://de.wikipedia.org/wiki/Duff%E2%80%99s_Device Duff's Device] in your programming language.
: It seems to be specific to C/C++ and would it be best practice or just some kind of optimisation? --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:54, 20 May 2014 (UTC)
* canonical huffman code
* Include-in : test wether a list is a part of another one, ie if each element of the first list is an element of the second one
: In Emacs Lisp: 
```lisp
(defun is-include (l1 l2)
  "test if l1 is a part of l2. In other words, if each element of l1
   is an element of l2"
  (cond
   ((null l1) t)
   ((member (car l1) l2) (is-include (cdr l1) l2))
   (t nil)))
```

:: Is that intended to model a subset operation? –[[User:Dkf|Donal Fellows]] 13:52, 18 December 2012 (UTC)
* <s>[[Sorting_algorithms/Heapsort|Heapsort]]</s> [done] and maybe some other sorts from [[wp:Algorithm implementation/Sorting|Wikipedia's list of sorting algorithms]]
:: I might be interested in [[wp:Introsort]] and [[wp:Timsort]], as both are reported to be highly efficient sorts. However, they're also not the best-described sorting algorithms (e.g., no actual algorithm on the WP pages) so I'm not in a hurry to actually take them on. –[[User:Dkf|Donal Fellows]] 09:15, 2 September 2010 (UTC)

:::The Timsort algorithm description might only be its source! I suspect it may not be a good candidate for an RC task for that reason, but if someone knowledgable could break out a sub-algorithm, such as finding optimal runs of pre-sorted data then implementing that sub-algorithm might form a useful task. --[[User:Paddy3118|Paddy3118]] 10:51, 10 November 2011 (UTC) 

:::Timsort is actually very well described - the Wikipedia page now has an algorithm description, and [http://hg.python.org/cpython/file/tip/Objects/listsort.txt this page] has a detailed description from the author. [[User:Gereeter|Gereeter]] 15:51, 17 March 2013 (UTC)

:: * [[wp:strand_sort|Strand sort]]
:: * [[wp:smoothsort|Smooth sort]]
:: * [[wp:Patience sort]]
:: * [[wp:Tree sort]] - would demonstrate the use of binary search trees
:: * *TimSort http://en.wikipedia.org/wiki/Tim_sort
* [[wp:B-tree|B-Tree]]
* [[wp:R-tree|R-Tree]]
* [[wp:Red-black tree|Red-black tree]]
* [http://www.davekoelle.com/alphanum.html The Alphanum Algorithm] People sort strings with numbers differently than software does. Most sorting algorithms compare ASCII values, which produces an ordering that is inconsistent with human logic. 
: Check out [[Natural sorting]]. It may be what you're after. --[[User:Paddy3118|Paddy3118]] 17:07, 21 September 2012 (UTC)

* SAX. As well as DOM and XPath it would probably be useful to demonstrate parsing a simple XML document with SAX.
* Base64 encode/decode
* singly-linked list algorithms. singly-linked list/reversal --[[User:Rlrandallx|Rlrandallx]] 07:03, 11 June 2010 (UTC).
* doubly-linked list algorithms, both cursor- and pointer-based implementations
* Writing a parser to parse structured text into appropriate native types. (A good demonstration format to parse might be JSON, though it should be about creating a parser rather handling JSON.)
::JSON is large. [[Range expansion]] requires the parsing of structured text and is smaller. Will that do? --[[User:Paddy3118|Paddy3118]] 05:23, 23 November 2010 (UTC)
* [[wp:Burrows-Wheeler transform|Burrows-Wheeler transform]]
* [[wp:Move-to-front transform|Move-to-front transform]]
: Done as [[Move-to-front algorithm]] --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 20:21, 20 May 2014 (UTC)
* [[wp:Cartesian tree|Cartesian tree]] construction [[User:qu1j0t3]]
* [[wp:Trie|Prefix Tree]]
* [[wp:Hash table]]: It's an extremely common data structure in practice, as it has very nice performance, so it would be good to have at least one task on it (as part of the Data Structures trail). –[[User:Dkf|Donal Fellows]] 09:36, 20 January 2011 (UTC)
:: Hmm; there's [[Associative array]] (and its associated tasks). Not quite as obvious as I hoped for. Maybe we need some more tasks in this area. –[[User:Dkf|Donal Fellows]] 09:26, 21 January 2011 (UTC)
* Should we have "Stack/Usage" to go along with [[Stack]], similar to [[Queue]] and [[Queue/Usage]]? --[[Special:Contributions/76.21.41.59|76.21.41.59]] 06:26, 15 September 2011 (UTC)
* [[wp:DBSCAN|DBSCAN]] (Density-Based Spatial Clustering of Applications with Noise)
* The Java String [http://docs.oracle.com/javase/6/docs/api/java/lang/String.html#hashCode%28%29 hashCode] function (notably used by [[wp:Minecraft]] for generating world seeds). --[[User:Stuart P. Bentley|STUART]] 21:13, 8 January 2012 (UTC)
::In Lua: 
```lua
local function hashCode(str)
  local total = 0
  local n = #str
  for i=1, n do
    total = (total + str:byte(i) * 31^(n-i)) % 2^32
  end
  return total
end

```
 --[[User:Stuart P. Bentley|STUART]] 21:18, 8 January 2012 (UTC)
* B-Splines might be a good task (I'm having trouble coming up with good example B-Splines right now...).  --[[User:Rdm|Rdm]] 19:31, 24 June 2012 (UTC)
* Cannot find [http://en.wikipedia.org/wiki/Selection_algorithm Linear general selection algorithm - Median of Medians algorithm] --[[User:Zmi007|Zmi007]] ([[User talk:Zmi007|talk]]) 23:34, 9 August 2013 (UTC)
* A simple succinct data structure plus its benchmar: http://en.wikipedia.org/wiki/Succinct_data_structure#Succinct_dictionaries


### = Graph algorithms =

* [[wp:Dijkstra's algorithm|Dijkstra's algorithm]]: single-source shortest-paths problem &mdash;''[[User:Ruud Koot|Ruud]]'' 22:37, 10 March 2011 (UTC)
* [[wp:Floyd&ndash;Warshall algorithm|Floyd&ndash;Warshall algorithm]]: all-pairs shortest-paths problem &mdash;''[[User:Ruud Koot|Ruud]]'' 22:37, 10 March 2011 (UTC)
* [[wp:Ford&ndash;Fulkerson algorithm|Ford&ndash;Fulkerson algorithm]] ([[wp:Edmonds&ndash;Karp|Edmonds&ndash;Karp]] variant): maximum-flow problem &mdash;''[[User:Ruud Koot|Ruud]]'' 22:37, 10 March 2011 (UTC)
* List all (strict) [[wp:Dominator_ (graph_theory)|Dominators]] of a node in a graph 
* List all (strict) Post-dominators of a node in a graph
* [http://en.wikipedia.org/wiki/Cycle_detection Cycle detection] algorithms:
** [http://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare Floyd's cycle-finding algorithm]/"Tortoise and Hare" - (e.g. [http://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare Python], [http://stackoverflow.com/questions/2663115/how-to-detect-a-loop-in-a-linked-list Java])
** [http://en.wikipedia.org/wiki/Cycle_detection#Brent.27s_algorithm Brent's algorithm]
* Minimum mean-weight cycle cancelling algorithm: minimum-cost maximum-flow problem &mdash;''[[User:Ruud Koot|Ruud]]'' 22:37, 10 March 2011 (UTC)
* [[wp:A* search algorithm|A* search algorithm]], surprised this isn't here, I looked, if it is, [[A*]] and [[A star]] should probably redirect to it. I'm attempting to translate Wikipedia's pseudocode into Python right now. [[User:Keiji|Keiji]] 19:55, 6 April 2011 (UTC)
** Okay, [[User:Keiji/aystar.py|here's my attempt]]. I haven't tested it yet, namely because I can't think of a good way to come up with input data that would make for a good test. [[User:Keiji|Keiji]] 22:46, 6 April 2011 (UTC)
* [[wp:Depth-first search]] Done as [[Tree traversal]]. 
* [[wp:Breadth-first search]] Done as [[Tree traversal]]. --[[User:Paddy3118|Paddy3118]] 04:31, 24 November 2011 (UTC)
* [[wp:Newick format]] parser.
* [[wp:Lambda calculus|Lambda calculus]] parser and compiler.--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 10:31, 27 August 2013 (UTC)


### System calls

* Copy a directory tree recursively
** On a local filesystem
** With an option to specify files or folder to exclude
* Open a named pipe
* Communicate with a child process using pipes


### Mathematical Operations

* Task: bAckermann. The idea is to take the Ackermann function and find the sequence of all possible calls to the function sorted by number of recursions. The output would begin: 
    A(0,n) , A(1,0) , A(1,1) , A(2,0) , A(1,2) , A(1,3) , A(1,4) , A(1,5) , A(2,1) , A(1,6) , A(3,0) , A(1,7) , ...
:Conjecture: R(x+z,y)>R(x,y) and R(x,y+z)>R(x,y) for all positive integer z and all positive integer x where R is the function returning the number of recursions in the corresponding call to the Ackermann function. Can anyone prove this conjecture? -[[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 02:34, 7 May 2014 (UTC)
* Write a programm in our programming language, that showns the numerical limits of all available datatypes for numbers. The limits of datatypes could be max number, min number, bit precission, decimal precission, etc.  
* [[Convert a decimal number to fraction ]]Convert a decimal number to fraction. 
* [[Self-describing numbers ]]Calculate Self-describing numbers.
* I do not fully believe that nobody did actually wrote abut the old ADDER yet!. ADDER Basically ADDS. For a coder simply     X=X+A     THIS will add A into X every time called and whatever A is..... (Originally placed elsewhere by user: 186.109.38.217)
* [[wp:Church encoding|Church encoding]] and computation with Church numerals
* Euler's method for approximating solutions to differential equations
* Symbolic differentiation. [http://userweb.cs.utexas.edu/users/novak/asg-symdif.html] [http://www.codecodex.com/wiki/index.php?title=Derivative]
* Gauss-Algorithm
** do you mean [[wp:Gaussian elimination|Gaussian elimination]]? --[[User:ShinTakezou|ShinTakezou]] 13:12, 19 May 2010 (UTC)
* What about symbolic antiderivatives or something like that? --[[User:Alegend|Alegend]] 17:13, 2 August 2010 (UTC)
* [[wp:Shunting-yard_algorithm]]
* [[wp:Reverse Polish notation]]
* Fast Fourier transform (Cooley-Tukey) --[[User:Tarballs are good|Tarballs are good]] 22:26, 5 January 2011 (UTC)
* Bankers Rounding (also known as [http://en.wikipedia.org/wiki/Rounding#Round_half_to_even 'round-to-even']), not just for the usual suspects, such as 
** 22.5 to 22.0, or
** -567.5 to -568.0
** but also for 
** 1.55 to 1.60, and 
**-42.0000015 to -42.0000020 
** --[[User:Prino|Prino]] 23:41, 18 February 2011 (UTC)
* [http://www2.stetson.edu/~efriedma/mathmagic/0511.html This] MathMagic entry. [[User:MagiMaster|MagiMaster]] 08:44, 30 May 2011 (UTC)
* [[wp:Exponential moving average]]
* [[Unit calculator/convertor]].  A bit like what Google can do:  https://www.google.com/search?q=10.5+cm+in+inches.  With all SI units plus others (days, months for time, W.hour for energy, etc)
* Perform basic mathematical operations (+,*,/,-) for numbers represented as strings.[https://github.com/danmar/cppcheck/blob/master/lib/mathlib.h Here] is a sample implementation in C++.
* Implement the longest increasing subsequence algorithm in your programming language. [http://www.algorithmist.com/index.php/Longest_Increasing_Subsequence Here] is an implementation in C and C++.
* [http://en.wikipedia.org/wiki/Fermat_primality_test Fermat primality test]
* Implement a function in your programming language that prints numbers in [http://en.wikipedia.org/wiki/Engineering_notation Engineering_notation]. A C# implementation is already [http://stackoverflow.com/a/808295 available].
* [http://en.wikipedia.org/wiki/Arithmetic_shift Arithmetic shift] - Many languages lack operators like C's << and >>. The goal for the task would be to build the function shift(n,s) where n and s are both signed integers; n is the number to be shifted and s is the number of positions (negative for left; positive for right). So shift(5,-2) would yield the same result as 5<<2.   --[[User:Mappo|Mappo]] 19 Nov. 2014
:: See [[Bitwise operations]]. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 17:40, 19 November 2014 (UTC)
:::Looks like he wants it re-implemented even if the language already covers it. --[[User:Mwn3d|Mwn3d]] ([[User talk:Mwn3d|talk]]) 17:56, 19 November 2014 (UTC)


### Color Spaces

* Conversion from sRGB to HSL, HSV and CMYK.
:That would not be Color Space conversion. You can not convert a color space into a color model. sRGB is a color space that uses RGB color model. HSL, HSV and CMYK are color models. Maybe you mean color model conversion? However, conversion to CMYK requires information about the inks, paper and printing device used. In practice, it requires color space conversion, too. --[[User:PauliKL|PauliKL]] 15:29, 2 December 2009 (UTC)
:: Ok, how about: [[Color Model Conversion]]. Convert a value from RGB to HSL, from HSL to HSV, HSV to YUV, and from YUV to RGB. Or using some other chain order that can better take advantage of color model similarities. --[[User:Short Circuit|Michael Mol]] 13:34, 8 October 2010 (UTC)
* Generate color triad. Given a color, return the two colors which are +/- 120 degrees on the color wheel. --[[User:Short Circuit|Michael Mol]] 13:34, 8 October 2010 (UTC)
* Desaturate. Given four colors (#ffff00;, #0000ff;, and #00ffff), desaturate them. --[[User:Short Circuit|Michael Mol]] 13:34, 8 October 2010 (UTC)


### Terminal Control

* Clearing the terminal window [[Terminal Control/Clear the screen]]
* Moving the cursor to a specific position on the screen (such as row 6, column 4) [[Terminal Control/Moving the cursor to a specific location on the screen]]

* Allow data entry in a specific place on the screen
* Restict data entry fields to a specific length (for example restrict input to 5 characters, do not allow a 6th character to be entered, and restrict the cursor from moving beyond the input length [[Terminal Control/Restrict width positional input/No wrapping‎]]
* This time restrict the input field on screen to a specific length, but allow the left hand side of the field to disappear, if the length of input exceeds the on screen input length (for example the input field may allow 30 characters on screen, but if you enter 40 characters, only the last 30 are shown on screen (unless you press the backspace key). Do not let the cursor move beyond the designated on screen field length. Terminal Control/Restrict width positional input/With wrapping‎]]
* Draw a box at a specific position on the terminal, with a specific height and width. For example, draw a box at row 3, column 7, 40 characters wide, and 8 characters in height.
* Display text within the box. If the text does not fit in the box, make the text roll up and down in response to cursor movements, so that the text can be displayed.
* Allow freeform data entry into a box on the screen.
* Draw a box on the screen, preserving the contents of what was underneath. Now make the box disappear, so that the screen looks as it did, before the box was drawn.

==API-specific==
* Provide a SOAP server function
* Create a simple, interactive website using CGI (perhaps a guestbook or a "Hello, $Name" style page). Any language with access to environment variables should be able to do this.

==Library-specific==
* Create a COM client (with early binding) (particularly with GCC/MinGW) (if possible under Winelib in Linux is also interesting)
* Convert a Microsoft Word document (*.doc, *.docx) to ASCII text with your favorite programming language.
* Demonstrate how to open and read text from a Microsoft Word document, using you favorite programming language.

==Implementation-specific==
* Show how your language supports separation of interface or specification from implementation.

==Language-specific==
Add a sample Makefile to demonstrate how to build a dummy executable for several programming languages (distinguish between several platforms: Linux,Windows,etc)

[[wp:Duck typing|Duck typing]] specifically my [[wp:Talk:Duck_typing#DuckDecoy Example|DuckDecoy Example]]? --[[User:Paddy3118|Paddy3118]] 18:28, 5 May 2009 (UTC)
: The example is a bit confusing, but I think it's marking the difference between classes as true types and classes as patterns (with method invocations as message sends), yes? –[[User:Dkf|Donal Fellows]] 08:22, 3 January 2010 (UTC)
A language called LC3 is a great tool for students wishing to learn assembly and wish to see what is actually in the RAM space. It is extremely simple. I think a language page would be worth the trouble. I have a few old examples that I can add in this language. --[[User:MichaelChrisco|Michael Chrisco]] 12:35, 26 July 2010

==Project level==
Which project, RC ?  --[[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 07:20, 21 November 2014 (UTC)

If possible, find ways to break these into smaller tasks 
which can ultimately be assembled into the final project.

===Self-hosting compiler===
* Self-hosting compiler. Now it is a draft task. If this task is too large then I could break it up into smaller pieces. An example of a subtask that might be good is to translate functions expressed in language A into functions expressed in language B for languages A and B that have notions of functions. More specific would be to translate a Scheme-like syntax for functions into your language's syntax for functions. -[[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 02:43, 7 May 2014 (UTC)


### Super Simple p2p network

: Could be done with FIFOs for streams, and a constant number of clients. Needs to be more specific regarding what it does. --[[User:Short Circuit|Short Circuit]] 22:28, 6 December 2007 (MST)
:: probably means some thing like this <nowiki>http://ansuz.sooke.bc.ca/software/molester/molester</nowiki>. To make it suitable for rc the restrictions need to be spelt out - i.e. fixing the protocol and discovery mechanisms [[User:Rahul|Rahul]] 20:24, 7 October 2008 (UTC)

===A table-based native code assembler===
implement a table-based native code (macro?) assembler in various HLLs


### Stateful behavior simulation

Demonstrate discrete event simulation and stateful behaviour by simulating a simple pick-and-place or storage/retrieval system robot. The robot would be given commands to retrieve from location A and place into location B. The robot would in turn command its 4 motors in sequence in order to accomplish the task.  The amount of time that a motor runs would be dependant on the distance required to move.  A scheduler would be set up to generate callback events to the motors at proper times to indicate
their motion was complete.  The complete specification of this task would be somewhat involved. --[[User:Rldrenth|Rldrenth]] 18:09, 20 January 2009 (UTC)
:This needs to be simplified/clarified from requiring "four motors" to having three commands, "set velocity forward/back", "no-op" and "claw open/close".
:Subsequently, it can be divided into four parts:
# Call a function at a constant interval
# Create a function which reads from a primary queue containing simple commands and a secondary queue containing complex commands, processing exactly one of these commands, and then calls the state update function.
# Create a function that converts a complex command "starting from x1, pick up at x2 and drop at x3" to one of the three simple commands
# Create a function that updates the state (velocity and position) based on the currently executing command.
In the interest of simplicity, it can be assumed that the program may terminate once the secondary queue is empty, that the robot has a constant velocity either forward or backward, and that the velocity and position values at termination are irrelevant. --[[User:Short Circuit|Short Circuit]] 05:40, 19 May 2009 (UTC)


### Unit test framework

Demonstrate the use of a unit test framework for your language
: Is [[Test a function]] a suitable demonstration? –[[User:Dkf|Donal Fellows]] 11:41, 26 February 2010 (UTC)


### Simple Shell

Execute system commands, pipes, I/O redirection, command history, custom PATH variable, etc. --[[User:CheesyPuffs144|CheesyPuffs144]] 01:44, 9 August 2009 (UTC)


### Prolog interpreter

The [[wp:Prolog|Prolog's syntax]] looks incredibly simple, and I'm wondering how difficult it would be to write a prolog interpreter in various langauges. --[[User:Short Circuit|Michael Mol]] 08:21, 13 November 2009 (UTC)

Interesting and relevant suggestion, Michael. Many such projects exist for reference, including Norvig's in [http://norvig.com/paip.html "Paradigms of Artificial Intelligence Programming,"] Paul Graham's in [http://www.paulgraham.com/onlisp.html  "On Lisp,"] the interpreter that is part of [http://www.j-paine.org/dobbs/poplog.html Poplog], and [http://codespeak.net/pypy/dist/pypy/doc/prolog-interpreter.html PyPy's]. [[User:qu1j0t3]]

:Prolog's syntax seems relatively simple, when compared with some other languages, but I do not think it is "incredibly simple".  For example, it has three distinct syntaxes for function calls, three different kinds of operators (prefix, infix and postfix), operator precedence, various forms of control flow syntax, and so on... --[[User:Rdm|Rdm]] 02:16, 3 September 2010 (UTC)

:Even though Prolog's syntax is simple, there is a rather elaborate parser necessary (including redefining operators and precedence, IIRC) plus the runtime operation of Prolog is rather complicated to implement (SWI-Prolog for example is quite a large project), this would only be feasible if you define a subset of Prolog (e.g. only logical terms). --[[User:AlexLehm|AlexLehm]] 10:30, 28 October 2011 (UTC)

:Edinburgh Prolog standard has rather elaborate character definition syntax. Prolog parser is anything but trivial. [[User:WillNess|WillNess]] 08:43, 2 December 2011 (UTC)


### Compact Whitespace interpreter

Implement a [[wp:Whitespace_(programming_language)|whitespace]] interpreter such that the program used to implement it is as small as possible while still correctly implementing whitespace


### General 2D morphing

I have an idea for a task but it might require original work, possibly on research level.  It's inspired from recent research about self-assembling robots:  http://phys.org/news/2013-10-surprisingly-simple-scheme-self-assembling-robots.html

What I have in mind is a simplified 2D model for an algorithm that would allow a set of square robots to take any arbitray shape.

Here is how I may formulate the task:

:Given N randomly selected points on a possibly infinite grid, and a given arbitrary target set of N points on this very same grid, find a set of N non-colliding trajectories such that the set of points at the end of the trajectories is the same as the target.

--[[User:Grondilu|Grondilu]] ([[User talk:Grondilu|talk]]) 09:42, 7 October 2013 (UTC)

===Chat Server, Client and Gateway===
I'll get started on a demo of this and proper write up over the next couple weeks. 
There's already a [[Chat server]] task, but it doesn't really specify a protocol. 
Most implementations seem to be pretty straightforward, query the client for a nick, then announce all messages to everyone.

The chat server I'd propose is slightly more complicated, but not as complex as IRC. 
I'll work on a more clear protocol spec, but at a minimum changing of nicks and private messages, some others that will come up I'm sure (more features would be permitted, but a minimum and compatible protocol for the later projects).

The second project is a Chat Client, it would hide the protocol (similar to IRC clients) so that the user can simply type messages, pm people via something like `/pm rabuf Hey, going to the movie tonight?`. Filtering messages received from the server that the user doesn't need (like PING/PONG messages), or optionally killlists to hide messages from annoying people. This could be split into 2+ projects. A simple CLI, this would look like the result of telnetting into the current chat servers. Hides the protocol, user simply sees prompts for a name at the start, and a prompt for messages. A more complex CLI version using something like ncurses or slang to build a more featureful UI (separate panel for PMs, text input in a panel at the bottom, etc.). And a GUI client 
(ideally, each language would have one `client` core that people build these 
UI wrappers around).

The third part is what [[IRC Gateway]] is proposing now, but simplified. Connect two servers, announce messages between them or extend the protocol of the servers to allow gateways to be special. Announce messages as rabuf if the user is local, rabuf@rosettacode if they're from the other side of the gateway, etc. This is mainly to address what I (personally, nothing against the task, but it's potentially very easy or ludicrously hard) see as a problem in the IRC Gateway task. In the TCL version the code is largley provided by an existing library, in languages lacking such a library one would need to be made or found. Potentially allow PMs across the gateway `/pm rabuf@rosettacode Yo!`.

Overall, these three projects build on existing projects around UIs and networked interfaces, they *may* be a bit large, but we've got some pretty hefty tasks floating about now, and I feel these should all be manageable. The name might be changed so that it doesn't conflict with the existing and simple Chat Server task, it doesn't need to be replaced, it's a nice simple example of client/server architecture in each language. Perhaps something like "Rosetta Chat". As I said, I'll be working on this over the next couple weeks and feedback, advice, criticism, etc are all welcome.

--[[User:Rabuf|Rabuf]] ([[User talk:Rabuf|talk]]) 20:47, 5 November 2013 (UTC)

==Unsorted==
 I am trying to write the following instructions in Pharo or SmallTalk:
- a linked list with tests 
- a simple program equivalent of javadoc system that generates a mini javadoc like html file for a class passed as argument. 


Place new items here, if it's unclear where they belong.


###  Operators polymorphism 

For many OO programming languages we need simple examples of [http://en.wikipedia.org/wiki/Operator_overloading operator overloading] and (if applicable to language) examples how to create new operators.--[[User:Zmi007|Zmi007]] ([[User talk:Zmi007|talk]]) 10:31, 16 April 2015 (UTC)

Not only OO languages.  Algol68 allows operator overloading and creation of new operators.  For instance

```ALGOL68

OP FACTORIAL = (INT n) INT :
IF n < 1 THEN 1 ELSE
   INT fact:= 1;
   FOR i FROM 2 TO n DO fact TIMESAB i OD;
   fact
FI;
OP * = (CHAR c, INT n) []CHAR :
( [n]CHAR result := HEAP [n] CHAR;
  FOR i TO n DO result[i]:=c OD;
  result
);
print ((FACTORIAL 5, newline));
print (("X" * 6, newline))

```

Output:
<lang>
       +120
XXXXXX

```

[[User:Brnikat|Brnikat]] ([[User talk:Brnikat|talk]]) 11:58, 10 July 2015 (UTC)


###  Simple OCR 
 
("optical character recognition") - Find text (or numbers) in pictures,
for example comics (like Dilbert), or screenshots. 

WRT "simple", we restrict this to chars of known fonts,
e.g. by giving the program some picture(s) with known text-samples. --13:07, 13 December 2014 (UTC)

=== Page-splitter ===
A bot for splitting pages into sub-pages. 
If possible, with only a single captcha-check.

E.g. I moved some entries from [[99_Bottles_of_Beer]] to separate pages, 

to organize them into groups of languages per subpage, 

see [[:Category:99_Bottles_of_Beer]]. 
The Page-splitter should be able to automate such tasks. --[[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 07:20, 21 November 2014 (UTC)


###  Reversible random bit generator 

Implement a random bit generator that can be run forward and backward. 
When run backward it produces bits in reverse order. 
I suggest using a maximal period minimal cost linear hybrid cellular automaton utilizing rules 90 and 150. -[[User:Zelah|Zelah]] ([[User talk:Zelah|talk]]) 02:35, 7 May 2014 (UTC)

=== Duff's Device ===
Implement [http://en.wikipedia.org/wiki/Duff%27s_device Duff's Device] or a near equivalent [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 03:27, 26 April 2014 (UTC)


### Bank Routing Number Validator

All banks are assigned one or more 9-digit routing numbers (aka. transit routing number) that are self checking according to this algorithm.  
From the left, multiply each digit by a corresponding weight (3,7,1,3,7,1,3,7,1) and add all products.  
If the sum ends in zero, the number is a valid routing number. 
Try it for 121000248 - it should sum up to 60. 
Also try it on the 9-digit routing number found at the bottom of your checking account)


### Clipboard Manipulation

Display the clipboard content & Write new content to the clipboard


### Minilight
 
A minimal global illumination renderer http://www.hxa.name/minilight/


### Random RC task

Select a random task or draft task from RC, filtering out the non-task pages
**Possibly done as a variation on [[Rosetta Code/Find unimplemented tasks]]?
:See [http://irclog.perlgeek.de/rosettacode/2011-11-21#i_4737091 this IRC discussion] for more information -- [[User:Eriksiers|Erik Siers]] 05:46, 21 November 2011 (UTC)


###  Noises 

* Perlin Noise, Simplex Noise, Worley Noise implementations
http://en.wikipedia.org/wiki/Perlin_noise
http://en.wikipedia.org/wiki/Simplex_noise
http://en.wikipedia.org/wiki/Worley_noise
https://gist.github.com/304522
http://libnoise.sourceforge.net/

* Perlin Noise, Simplex Noise, Worley Noise implementations for tiling system in video games or other media


###  Thread safe circular buffer

Using mutexes or semaphores to conduct thread safe and robust read and writes from a circular buffer.


### Henderson Escher Picture Language

[http://www.ecs.soton.ac.uk/%7Eph/funcgeo.pdf PDF] as made famous by [http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-15.html#%_sec_2.2.4 SICP Section 2.2.4] [http://groups.csail.mit.edu/mac/classes/6.001/abelson-sussman-lectures/ Video, Lecture 3a].  
Here is an [http://www.frank-buss.de/lisp/functional.html implementation in Lisp] by Frank Buss. --[[User:Alanning|Alanning]] 04:00, 27 May 2011 (UTC)


### Phonecode

Given a list of words, and a telephone number, find all possible encodings of that telephone number by words. This task has already been used in a few comparative studies of language performance and productivity, for example [http://page.mi.fu-berlin.de/prechelt/Biblio/jccpprtTR.pdf]
: Thanks so much for that link! I had read the paper some time ago but I probably have the link on an old hard drive somewhere...
: You have to remember though, that the aims of RC are ''very'' different to the authors of that paper and any task would have to be written for RC. If anyone has the number to a cold-calling firm, we could find the most appropriate phrase for their number ;-) or maybe not. --[[User:Paddy3118|Paddy3118]] 08:15, 7 May 2011 (UTC)
:: Here's an easy one. The phone number to the White House [http://www.whitehouse.gov/contact switch board]: 202-456-1414. Nothing specific or political intended here; it's just a published, public number. I do like the task idea, though. --[[User:Short Circuit|Michael Mol]] 13:15, 8 May 2011 (UTC)


### Weather Routing

The weather routing problem has the following parts:
<ul>
  <li> a predicted surface wind direction and speed, at increments of longitude, latitude, and time</li>
  <li> an expected surface current direction and speed, at increments of longitude, latitude, and time</li>
  <li> 'polar data' describing maximum speed of a sailboat at points of sail for a given speed of wind over water</li>
  <li> regions for sailing (the open ocean) and not (the land, shallows, restricted areas, etc.)</li>
  <li> a starting location and time, and a destination</li>
</ul>

Given the above information and a specific path, progress along path and therefore arrival time are determined. The weather routing problem, conversely, is to determine the path which results in the earliest arrival time.
[[User:JimTheriot|JimTheriot]] ([[User talk:JimTheriot|talk]]) 00:33, 8 July 2015 (UTC)


### Project Euler problems


* I suggest implementations of various maze generation algorithms, such as Recursive Division or Prim's Algorithm. A website that already does this for Python can be found here: [http://weblog.jamisbuck.org/search?q=maze+generation http://weblog.jamisbuck.org]--[[User:Intercoder|Intercoder]] 12:57, 7 March 2011 (UTC)

* My suggestion is for a unicode interpretation task.  Specifically, specifying the encode eg utf-8, the task should provide a small file of text (or the data to programmer could be bytes of data instead). The task should be to depict (interpret) the data in a number of forms, for instance text, binary,hex.  This would highlight features of some languages for handling Unicode and gain the user related xperience with parsing strings, string-numeric conversion, etc (I did see http://rosettacode.org/wiki/Character_codes)--[[User:Billymac00|Billymac00]] 02:21, 3 January 2011 (UTC)

* Breadth-first search (BFS).  Given an n-ary tree, enumerate all the leaves of the tree in breadth-first, left-to-right order and collect them into a list.
:Isn't most of that captured in [[Tree traversal]]? --[[User:Paddy3118|Paddy3118]] 14:58, 3 November 2010 (UTC)

* Particle Swarm Optimization (PSO) implementation. A test case would be something like finding the global minimum of the banana function using PSO. An optional addition to the task would be to implement the algorithm using concurrency/parallelization techniques. The task is inherently parallel. --[[User:Treefall|Treefall]] 23:32, 30 August 2010 (UTC)

* Financial functions. E.g. Future Value, Present Value, Nominal declining balance depreciation, Straight line depreciation, Uneven internal rate of return, Weibull analysis, T-Bill Discount ... and the list goes on. Examples: [http://finance-old.bi.no/~bernt/gcc_prog/recipes/recipes/node3.html the value of time] --[[User:Axtens|Axtens]] 14:29, 20 March 2010 (UTC)
 
* Not sure where to put this, but could we prefix all the statistics-related tasks with "Statistics/"? All the average and means are bunched up together but poor old standard deviation is an outlier. --[[User:Axtens|Axtens]] 05:21, 24 February 2010 (UTC)
** There's already [[:Category:Probability and statistics]]. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 16:03, 4 April 2010 (UTC)

*Put all examples of a given language into a single page/file for easy reference, some extra comments differentiating where examples begin and start. That would be really nice, users can do a pdf print of the page:)
: The page would be too long. --[[User:Paddy3118|Paddy3118]] 15:06, 26 February 2010 (UTC)
: That's something that's been discussed on and off for a couple years. The key problem is logically dividing examples, moving them to a separate area, and using transclusion to pull them back. I created a "Example" wiki namespace specifically for the purpose. --[[User:Short Circuit|Michael Mol]] 15:59, 26 February 2010 (UTC)

* Concise code/expressiveness: Not exactly a task but: for a given language and task measure the code size(preferably excluding comments), then compare to all other languages that have completed that task. list the percentile rank of this language+task's code next to the task name.  For a given language, average all its percentile ranks and list next to language name in the main list. 

:Any measure of concision would need a metric for both code size and code clarity. Your proposed measurement of just code size would lead to code "golfing". I think it is better for the examples to be written as idiomatic examples of a language that either fulfils the entirety of a task description, or at least states what is missing. --[[User:Paddy3118|Paddy3118]] 03:25, 1 September 2010 (UTC) 
:: Agreed. While code golf is sometimes awe-inspiring, it's typically of horribly low value because it leads to things that people can't read. We're not exploring the [[wp:Kolmogorov complexity|Kolmogorov Complexity]] of a problem, we're showing how a task ''should'' be done in many languages (i.e., with good style; making code the way we'd like it to be if we encountered it). –[[User:Dkf|Donal Fellows]] 09:59, 1 September 2010 (UTC)

* Seeing as we have GCD (greatest common divisor), how about LCM (lowest common multiple), <s>HCF (highest common factor),</s> and Chebyshev function. --[[User:Axtens|Axtens]] 09:21, 17 February 2010 (UTC)
** Isn't HCF the same thing as GCD? --[[Special:Contributions/164.67.235.79|164.67.235.79]] 09:45, 17 February 2010 (UTC)
***(blush) oops, so it is. --[[User:Axtens|Axtens]] 11:56, 17 February 2010 (UTC)

* Sets: AddToSet, RemoveFromSet, IsSubset etc. VBScript would have to construct something. Other languages have it already. --[[User:Axtens|Axtens]] 07:11, 11 February 2010 (UTC) 
* non-JSON serialisation (what Tcl does, for instance, writing data out to a text file that can then be 'source'd at another time.) --[[User:Axtens|Axtens]] 02:48, 10 February 2010 (UTC)
* File creation time (there is already File modification time, which can be used for File access time with atime instead of mtime, but there's nothing in utime.h for creation time )

* [[wp:Levenshtein distance]]

* Add the other [[wp:Soundex#Soundex_variants|Soundex variants]] to the Soundex task or create separate tasks for each of them. --[[User:Axtens|Axtens]] 07:27, 4 April 2010 (UTC)

* Given [[wp:cron]] I suggest a three part task: 
*: 1. Parse a simplified crontab (no ranges or asterisks)
*: 2. Issue the commands in the simplified crontab at the appropriate times
*: 3. Handle a standard crontab with ranges and asterisks
*:I'd have created the task already but that all my code is still on paper. --[[User:Axtens|Axtens]] 14:21, 20 April 2010 (UTC)

* [[wp:Gray code]]. Specifically, construct and ''n''-bit Gray Code that satisfies the restrictions for the Beckett-Gray code, or show that no such code exists (3-bit and 4-bit happen to fall under the "can't satisfy" set, for example). A task like this is interesting for it being Gray Code, and for the matching against the Beckett-Gray code being accelerable by languages with good support for concurrency, and more easily expressed by languages which express data set problems. (All the details required for solving this problem are on the WP page) --[[User:Short Circuit|Michael Mol]] 12:34, 19 May 2010 (UTC)
: This sounds like it would be more appropriate for [http://projecteuler.net Project Euler] than Rosetta Code:  How many useful programming projects really need an implementation of a Becket-Gray search? And, how much would a typical programmer learn about translation by studying such implementations?  --[[User:Rdm|Rdm]] 14:11, 19 May 2010 (UTC)
:: I never thought of RC as about studying translation. The closest to that is studying unfamiliar languages by seeing a task implemented in a familiar one. Project Euler focuses on implement before observation, where RC is geared more towards observation, and implementation if nobody's already provided it. Apart from being an interesting curiosity (you'd be surprised how many people who actually stick around and look at code came in on curiosity pages like Ethiopian Multiplication and Quine; come for the show, stick around and contribute code), the task I described allows code langauges to exercise dataset manipulation and idiomatic examples of concurrency, the latter of which, at least, doesn't have much presence on the site. --[[User:Short Circuit|Michael Mol]] 18:31, 19 May 2010 (UTC)
::: But a Beckett Gray search has only one interesting case (5 bits wide).  The 6 bit wide case takes 15+ hours which makes testing a 6 bit solution for accuracy problematic, the 0, 1 and 2 bit cases are trivial, and the 3 and 4 bit cases are not doable (and what does it mean to "show that no such code exists"?  Is reporting that the last value in the found sequence has 3 bits set sufficient?).  [And, if my implementation is correct that 5 bit case has 16 distinct valid results -- and each of them has 120 permutations all of which are valid for a total of 1920 different, valid 5 bit beckett gray codes.] --[[User:Rdm|Rdm]] 19:57, 19 May 2010 (UTC)
:::: The 6-bit-wide case discussed on the WP page indicates that a full search of the data set takes 15 hours, but doesn't indicate the hardware, language or techniques used to find it. See [[Lucas-Lehmer test]] for another task that takes a while to run. When originally written, it specified to allow the program to run until it found M47, which wasn't even known at the time. [[User:NevilleDNZ|NevilleDNZ]] apparently has a sense of humor.

::::  There are multiple possible solutions for the 5-bit and 6-bit cases, but only one (but no necessarily any particular one)  needs to be found. Conveniently, this is one of those problems where verifying that a solution is valid is easier than finding the solution in the first place; the way I'd expect it to be written is generate gray codes/foreach gray code check for satisfying Beckett restrictions/display first satisfier found and terminate. A final, redundant validity check would be to verify that the presented sequence is a gray code, and that the presented sequence satisfies the Beckett restriction.

::::  By "show that no such code exists," I meant prove that there is no 3-bit or 4-bit Beckett-Gray code; there may gray codes, but there are no gray codes in that space that satisfy the additional constraints placed by the Beckett playwright. If the entire problem space is searched without a found solution, then either there is no solution, or the searching program is in error. in the generate->check(terminate) pipeline, if the generator runs dry without the check ever succeeding, then it's been shown that no example exists.

::::  Going back, you don't need to show ''all'' BG codes for a number of bits, but show at least one if any exist, or show none if none exist. At least, that's what I had in mind when I suggested the task. --[[User:Short Circuit|Michael Mol]] 03:20, 20 May 2010 (UTC)

* I suppose that the RPG task is to demonstrate how to implement a domain-specific language, yes? If not, then perhaps cook up a task that demonstrates one.
: [[RCRPG/Perl]] was the result of me scratching a nostalgic itch while stuck in bed sick one weekend. I didn't have a deeper goal than that. --[[User:Short Circuit|Michael Mol]] 09:33, 20 June 2010 (UTC)

* A task designed to require a large amount of processing, tuned to allow lower level languages like C to find the solution in a reasonable amount of time (like one or two minutes) and higher level languages like Python to be too much slow for the job.
::You may not get the answer you were expecting as the idiomatic Python solution might be to add ''this'' to the C solution then call the C from Python using ''that'' wrapper generator. Their are several toolsets around that use Python as a wrapper around existing C and Fortran libraries and allow the easy wrapping of C routines. --[[User:Paddy3118|Paddy3118]] 05:53, 23 November 2010 (UTC)
:::This Python solution is acceptable. The point of this task is not to keep Python out of the page, but to show how various languages face a computation-heavy task.

::See [[Ackermann function]] where the extremely compute intensive function needs bigints and other optimisations to get to larger answers. On that page, the C version might be compute intensive, but inefficient compared to the Python one.

* A task demonstrating how to write a PHP extension DLL (or .so) ... hmmm ... that can only be done in C and Delphi so far. Maybe that's too much of an ask. --[[User:Axtens|Axtens]]

* Building a suffix tree (possibly followed by a generalized suffix tree). If the tree is built in linear time, then many string problems can be solved efficiently. for example finding the longest common substring between two strings becomes linear rather than polynomial through a DP method.

* Use [[wp:Canny_edge_detector|Canny edge detection]] to find edges in an image. This could, in turn, be a pre-filter for a [[Hough transform]] which then allows extraction of coordinates (ρ,θ) of significant edge lines within the image. --[[User:Coderjoe|Coderjoe]] 10:39, 10 August 2010 (UTC)

* Well since I created the bead sort using addition, I would love to see other work being done with it. Perhaps other language implementations on this sorting algorithm. Or improvements to my original sudo code or implamentation. I should note that the sorting algorithm I created is not "true" to the exact nature of Bead sort but it does work like one would expect from such a solution. It probably should be named Using addition (as gravity) for Bead sort or something similar. --[[User:MichaelChrisco|MichaelChrisco]] 11:03, 22 August 2010 (UTC)

* I suggest to copy the program task (and its implementations) from here: http://stackoverflow.com/questions/3538156/file-i-o-in-every-programming-language-closed
: See [[File IO]]. Actually, I've been watching the analytics data closely over the last week, and that task has seen a large number of hits from a link on the relevant StackOverflow page. --[[User:Short Circuit|Michael Mol]] 18:37, 27 August 2010 (UTC)
:: It probably means that people come on this site to learn and copy code, it's positive. But the File IO Task is very simple. A similar but a bit more complex task may be added.
:::Well, the StackOverflow case looks rather unique. IIRC, they already had a code golf/chrestomathy section on their site (We've often gotten referral hits from SO in "how do I do X" or "how do I do X in Y" questions, and I've seen mention of "move it to the SO wiki" before.), and the big deal with that task involves the social structure and politics of the appropriateness of the posted SO question, its closing and its temporary deletion. Also, it's inappropriate to copy content from places like SO if that copying violates [[Rosetta Code:Copyrights]]. --[[User:Short Circuit|Michael Mol]] 03:23, 29 August 2010 (UTC)
:::I hope I havent stepped on anyones toes here but I've been trying to get the reddit community interested in the site. The more eyeballs, the more chances of good code and tasks (and donations wink wink). IO tasks could be done with blocks, bits, and implementing different database models. I dont know about the language of such a task (like implement SQL or something) but it would be interesting.--[[User:MichaelChrisco|MichaelChrisco]] 00:16, 29 August 2010 (UTC)
:::: Hey, you're not stepping on anyone's toes there. I prefer word-of-mouth to any other form of advertisement I could get. :) --[[User:Short Circuit|Michael Mol]] 03:23, 29 August 2010 (UTC)
* [[wp:Discrete cosine transform|Discrete cosine transform]]
*[http://www.arcfn.com/2010/06/using-arc-to-decode-venters-secret-dna.html decode Venter's secret DNA watermark]

*[http://code.google.com/p/webp/ WebP] container and conversion from JPEG (or whatever other format you'd like).

* One or more tasks which illustrate the technique of [[wp:Dynamic programming]], in addition to the existing [[Knapsack problem]]; preferably solving problems of broader interest. —[[User:Kevin Reid|Kevin Reid]] 19:02, 16 February 2011 (UTC)
: Try [[Longest common subsequence]] too. --[[User:Paddy3118|Paddy3118]] 22:32, 16 February 2011 (UTC)
: Here is link to an implementation of [[Longest common subsequence]] in [http://www.algorithmist.com/index.php/Longest_Increasing_Subsequence.c C] and [http://www.algorithmist.com/index.php/Longest_Increasing_Subsequence.cpp C++]. 

* Escape from Zurg: http://blog.jonhnnyweslley.net/2008/08/escape-from-zurg.html

* Execute a program written in [[Piet]]. --[[User:ThAlEdison|ThAlEdison]] 19:13, 11 March 2011 (UTC)

* Read the name of a task and the name of a programming language from standard input, and put an implementation of the task in that programming language on standard output. Ideally, N tasks and M programming languages should require O(N+M) lines of source code rather than O(N*M). --[[User:Johnicholas|Johnicholas]] 11:04, 18 March 2011 (UTC)

* In object-oriented programming, show how a subclass's constructor can explicitly pass arguments to its superclass's constructor to initialize its base object (instance of the superclass). --[[Special:Contributions/98.210.210.193|98.210.210.193]] 09:02, 29 March 2011 (UTC)

* Remove some elements from a mutable collection while iterating through it, based on some condition of the element. Related to [[Filter]], but modifying the existing collection rather than creating a new one. This is a challenging task, because removing the element you are currently iterating over, may affect the iteration itself (changes indexes / affects iterators). --[[Special:Contributions/208.80.119.67|208.80.119.67]] 19:32, 5 April 2011 (UTC)
:This is an Anti-pattern. Why code it? --[[User:Paddy3118|Paddy3118]] 19:57, 5 April 2011 (UTC)
:: Perhaps the original commenter overspecified. Make it less specific? Call it [[Set filter]]. --[[User:Short Circuit|Michael Mol]] 20:16, 5 April 2011 (UTC)
::: Some languages provide this operation, exactly as the original commenter specified. Common Lisp has ''delete-if-not'', Factor has ''filter!'', and Ruby has ''Array#select!''. Java hackers might implement this with [http://download.oracle.com/javase/6/docs/api/java/util/Iterator.html#remove() java.util.Iterator.remove()] from [http://download.oracle.com/javase/6/docs/api/ Java API]. I added this task as an option to [[Filter]], along with code for Factor and Ruby. (The page already had Common Lisp.) --[[User:Kernigh|Kernigh]] 01:55, 6 April 2011 (UTC)

* Create a program to reseed the Linux Kernel's pseudo random number generator through /dev/urandom with random bytes fetched from [http://www.random.org/] A PERL implementation can be found in the Ubuntu repositories in the package named reseed

* Show how to print a file in a portable way (for instance with GTK) --[[User:Fjfabien|François Fabien]] 21 mai 2011

* [[wp:Linear programming]], and especially the [[wp:simplex algorithm]]. [[User:Toucan|Toucan]] 16:37, 15 June 2011 (UTC)

* Cyclic Redundancy Check (CRC)

* Get screen resolution [[Special:Contributions/82.83.239.135|82.83.239.135]] 19:45, 1 October 2011 (UTC)

* Get title of window under cursor [[Special:Contributions/82.83.239.135|82.83.239.135]] 19:45, 1 October 2011 (UTC)

* Ant problem: http://ideone.com/nLN4i

* Hash-join
: The hash join algorithm is very useful in the development of a relational database systems. The task consists in implementing the following algorithm and provinduing a simple reproducible example: The records of files R and S are both hashed to the same hash file, using the same hashing function on the join attributes A of R and B of S as hash keys. A single pass through the file with fewer records (say, R) hashes its records to the hash file buckets. A single pass through the other file (S) then hashes each of its records to the appropriate bucket, where the record is combined with all matching records from R.

** the following three items could be in a category like program checking or program verification(or whatever this is called)
* Programming by contract/Design by contract
Some languages support Design by contract as part of the language (e.g. Eiffel) or via libraries or language extensions (cofoja, gcontracts, C#), I think it would be useful to have a simple (probably incorrect) implementation of a task and the contracts that exhibit the programming mistake. --[[User:AlexLehm|AlexLehm]] 09:01, 28 October 2011 (UTC)
* Assert

* Unit Tests

* Zebra Puzzle [[wp:Zebra Puzzle]]

:* Hear, hear! [[User:WillNess|WillNess]] 09:34, 2 December 2011 (UTC) ... [[Zebra puzzle|started it up as a draft task]] myself. [[User:WillNess|WillNess]] 11:49, 2 December 2011 (UTC)

* [[Trial Division Sieve]]. We now have [[Primality by trial division]] (testing one number) and [[Sieve of Eratosthenes]]. The latter has trial-division sieves in it, that really don't belong but can't properly go into the former. They should get their own home IMO. Also, [[Generating primes in given range]], whether by offset sieving or by filtering. There is [[Prime decomposition]] of a number into its factors, but there's no [[Generating divisors]] of a number, or [[Counting divisors]] of a number, or [[Euler's totient function]], etc.  [[User:WillNess|WillNess]] 09:34, 2 December 2011 (UTC)
*  matrix shape:  suppose the input is 4 the ouput will be

```txt
1  2  3  4
5  8  9  10
6  11 13 14
7  12 15 16
```

if the input is 5 then the output will be

```txt
1  2  3  4  5
6  10 11 12 13
7  14 17 18 19
8  15 20 22 23
9  16 21 24 24
```

* Implement this new little algorithm: http://www.codeproject.com/KB/recipes/Goldbach.aspx
* Write a program to draw a Golden Spiral
* Four ideas from Ruby Quiz site:
** http://www.rubyquiz.com/quiz39.html
** http://www.rubyquiz.com/quiz111.html
** http://www.rubyquiz.com/quiz117.html
** http://www.rubyquiz.com/quiz137.html
* Ternary and quaternary search (binary search is already present).

*Maximum Unchecked Problem: Place 8 queens on a chessboard so that as many squares as possible are not attacked by any queen. (Cells occupied by queens are counted as attacked.) Rouse Ball in "Mathematical Recreations and Essays" believes the answer to be 11, but was never able to prove it. [[User:Jon Rob|Jon Rob]] 14:41, 30 October 2012 (UTC)

*Farmer across river problem: man, wolf, goat, cabbage and ship, everyone knows. Make the algorithm to find solution.

* Seam Carving (or Context Aware Image Resizing) basic algorithm: http://nparashuram.com/seamcarving/  as test case use this image converted to grey scale: http://upload.wikimedia.org/wikipedia/commons/c/cb/Broadway_tower_edit.jpg

* Draw a Pythagoras Tree ( http://fsharpnews.blogspot.it/2009/05/pythagoras-tree.html  http://en.wikipedia.org/wiki/Pythagoras_tree_%28fractal%29 )

* Linear regression [[User:Wei2912|Wei2912]] ([[User talk:Wei2912|talk]]) 14:38, 6 June 2014 (UTC)


###  Playfair encryption and decryption 

This is a specific example of the encryption problem mentioned in the general section.  Playfair is a simple 25-character bigram alphabetic cipher which was still being used by the military in WW1.  The task is to use a given keyword or phrase to generate a key which is used to encrypt a plain text and then to decrypt the corresponding cypher text.  As only 25 different letters can be used, one must be chosen '''not''' to be used in the plaintext, substituting a valid character as needed to maintain unambiguity of the message.  As adjacent identical letters can not be present in the plaintext before encryption, one or more  dummy characters must be inserted between them.  The plaintext must be padded to an even number of characters before encryption.

I don't yet have a suggested plaintext which trips all the above special conditions but will produce one, along with a pass phrase, the corresponding key and the cipher text.

[[User:Brnikat|Brnikat]] ([[User talk:Brnikat|talk]]) 11:16, 10 July 2015 (UTC)

:There is already a draft task [[Playfair_cipher]] --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 05:32, 11 July 2015 (UTC)


###  ICMP Ping 
 
Send an ICMP ping to some local resource, such as the current default gateway or even just 127.0.0.1 or ::1, and display information about the response or the absence of the response.


###  Generate a Regular Expression for a Range of Integers 


Write a function that, given the integers 0 <= ''min'' < = ''max'', returns a regular expression that will match the decimal representation of any integer in the range [''min'', ''max'']. You may assume there are no thousand-separators, leading 0's, or other problematic characters in the string to be processed. You may also assume the caller of your function will prepend/append appropriate anchors (e.g., ^, $, or \b) depending on need, so don't include them. Don't use \d as an alias for [0-9] since \d can match weird unicode characters on some systems.

'''Example''':

 ''min'' = 1

 ''max'' = 31

 f(''min'', ''max'') = [1-9]|[1-2][0-9]|3[0-1] /* possible result */

 f(''min'', ''max'') = [1-9]|[12][0-9]|3[01] /* equally-valid alternate result */

'''Example''': 

 ''min'' = 91

 ''max'' = 417

 f(''min'', ''max'') = 9[1-9]|[1-3][0-9]{2}|4(0[0-9]|1[0-7]) /* possible result */

 f(''min'', ''max'') = 9[1-9]|[1-3][0-9][0-9]|4(0[0-9]|1[0-7]) /* equally-valid alternate result */


Hint: Use a recursive, divide-and-conquer approach. You will have to handle cases where max contains more digits than min.

Bonus: support ranges containing negative integers.


###  Weighted Random 

Pick a random item from a list of dozen items, where each item has different weight (rarity). If the language permits, the list should be made in a way that makes it easy to add new items without having to adjust the weights of the other items.

Gaming example: random treasure generation in roguelikes. I've found that this is extremely awkward to do in some languages, and simple in others.


###  Partitioning 

Task for "partition an integer into X primes".  For example, partition 19 into 3 primes could return  3+5+11.



-----



This request was just created today.


See the Rosetta Code task:

:*   [http://rosettacode.org/wiki/Partition_an_integer_X_into_N_primes '''Partition an integer X into N primes'''].


-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:48, 3 March 2017 (UTC)

=== Bron-Kerbosch algorithm ===
Implement any of the variants of the [[wp:Bron-Kerbosch_algorithm|Bron-Kerbosch algorithm]] for finding maximal cliques (maximal complete subgraphs) in an undirected graph.

This should likely be sorted under 1.2.2.1, Graph algorithms.

==Insufficient information==
These task suggestions do not have sufficient information to allow the creation of a task. This section will be cleaned out periodically, but these may inspire the creation of other tasks in the mean time.


### Design patterns

<div style="column-count: 3; -webkit-column-count:3; -moz-column-count:3">
* [[wp:Abstract factory pattern|Abstract factory pattern]]
* [[wp:Active object|Active object]]
* [[wp:Active record pattern|Active record pattern]]
* [[wp:Adapter pattern|Adapter pattern]]
* [[wp:Aggregate pattern|Aggregate pattern]]
* [[wp:Amelioration Patterns|Amelioration Patterns]]
* [[wp:Archetype Pattern|Archetype Pattern]]
* [[wp:Architectural pattern (computer science)|Architectural pattern (computer science)]]
* [[wp:Asynchronous method invocation|Asynchronous method invocation]]
* [[wp:Balking pattern|Balking pattern]]
* [[wp:Barton-Nackman trick|Barton-Nackman trick]]
* [[wp:Behavioral pattern|Behavioral pattern]]
* [[wp:Bridge pattern|Bridge pattern]]
* [[wp:Builder pattern|Builder pattern]]
* [[wp:Chain-of-responsibility pattern|Chain-of-responsibility pattern]]
* [[wp:Command pattern|Command pattern]]
* [[wp:Composite pattern|Composite pattern]]
* [[wp:Concurrency pattern|Concurrency pattern]]
* [[wp:Creational pattern|Creational pattern]]
* [[wp:Curiously recurring template pattern|Curiously recurring template pattern]]
* [[wp:Data transfer object|Data transfer object]]
* [[wp:Data access object|Data access object]]
* [[wp:User:Rvalimaki|User:Rvalimaki]]
* [[wp:Debugging patterns|Debugging patterns]]
* [[wp:Decorator pattern|Decorator pattern]]
* [[wp:Delegation pattern|Delegation pattern]]
* [[wp:Dependency inversion principle|Dependency inversion principle]]
* [[wp:Design Patterns (book)|Design Patterns (book)]]
* [[wp:Design pattern|Design pattern]]
* [[wp:Differentiated service|Differentiated service]]
* [[wp:Dispose pattern|Dispose pattern]]
* [[wp:Distributed design patterns|Distributed design patterns]]
* [[wp:Double dispatch|Double dispatch]]
* [[wp:Double-chance function|Double-chance function]]
* [[wp:Double-checked locking|Double-checked locking]]
* [[wp:Enterprise Integration Patterns|Enterprise Integration Patterns]]
* [[wp:Event handler|Event handler]]
* [[wp:Exception chaining|Exception chaining]]
* [[wp:Extensibility pattern|Extensibility pattern]]
* [[wp:Facade pattern|Facade pattern]]
* [[wp:Factory method pattern|Factory method pattern]]
* [[wp:Factory object|Factory object]]
* [[wp:Factory pattern|Factory pattern]]
* [[wp:Fluent interface|Fluent interface]]
* [[wp:Flyweight pattern|Flyweight pattern]]
* [[wp:Front Controller pattern|Front Controller pattern]]
* [[wp:Fundamental pattern|Fundamental pattern]]
* [[wp:Guarded suspension|Guarded suspension]]
* [[wp:Hash consing|Hash consing]]
* [[wp:Head-Body Pattern|Head-Body Pattern]]
* [[wp:Hierarchical visitor pattern|Hierarchical visitor pattern]]
* [[wp:Hollywood Principle|Hollywood Principle]]
* [[wp:Identity map|Identity map]]
* [[wp:Initialization on demand holder idiom|Initialization on demand holder idiom]]
* [[wp:Interaction design pattern|Interaction design pattern]]
* [[wp:Interceptor pattern|Interceptor pattern]]
* [[wp:Interpreter pattern|Interpreter pattern]]
* [[wp:Inversion of control|Inversion of control]]
* [[wp:Iterator pattern|Iterator pattern]]
* [[wp:Lazy Inheritance|Lazy Inheritance]]
* [[wp:Lazy initialization|Lazy initialization]]
* [[wp:Lazy loading|Lazy loading]]
* [[wp:List of Object-oriented design patterns|List of Object-oriented design patterns]]
* [[wp:Mangler Pattern|Mangler Pattern]]
* [[wp:Marker interface pattern|Marker interface pattern]]
* [[wp:Mediator pattern|Mediator pattern]]
* [[wp:Memento pattern|Memento pattern]]
* [[wp:Message broker|Message broker]]
* [[wp:Mock object|Mock object]]
* [[wp:Model 1|Model 1]]
* [[wp:Model 2|Model 2]]
* [[wp:Model View ViewModel|Model View ViewModel]]
* [[wp:Model-view-presenter|Model-view-presenter]]
* [[wp:Model-view-controller|Model-view-controller]]
* [[wp:User:Damianham/Model View Role|User:Damianham/Model View Role]]
* [[wp:Model-view-adapter|Model-view-adapter]]
* [[wp:Multiton pattern|Multiton pattern]]
* [[wp:Naked objects|Naked objects]]
* [[wp:Nianio|Nianio]]
* [[wp:Null Object pattern|Null Object pattern]]
* [[wp:Object pool|Object pool]]
* [[wp:Observer pattern|Observer pattern]]
* [[wp:Pipeline (software)|Pipeline (software)]]
* [[wp:Portland Pattern Repository|Portland Pattern Repository]]
* [[wp:Presentation-abstraction-control|Presentation-abstraction-control]]
* [[wp:Private class data pattern|Private class data pattern]]
:[[Scope modifiers]]? --[[User:Mwn3d|Mwn3d]] 19:57, 27 November 2009 (UTC)
:: Looks like a poor match to me. –[[User:Dkf|Donal Fellows]] 08:02, 3 January 2010 (UTC)
* [[wp:Process patterns|Process patterns]]
* [[wp:Producer-consumer problem| Producer-consumer Pattern]]
* [[wp:Prototype pattern|Prototype pattern]]
* [[wp:Provider model|Provider model]]
* [[wp:Proxy pattern|Proxy pattern]]
* [[wp:Reactor pattern|Reactor pattern]]
* [[wp:Read/write lock pattern|Read/write lock pattern]]
* [[wp:Row Data Gateway|Row Data Gateway]]
* [[wp:Scheduled-task pattern|Scheduled-task pattern]]
* [[wp:Scheduler pattern|Scheduler pattern]]
* [[wp:Separation of presentation and content|Separation of presentation and content]]
* [[wp:Service locator pattern|Service locator pattern]]
* [[wp:Single-serving visitor pattern|Single-serving visitor pattern]]
* [[wp:Proactor pattern|Proactor pattern]]
* [[wp:Specification pattern|Specification pattern]]
* [[wp:State pattern|State pattern]]
* [[wp:Strategy pattern|Strategy pattern]]
* [[wp:Structural pattern|Structural pattern]]
* [[wp:Table Data Gateway|Table Data Gateway]]
* [[wp:Template method pattern|Template method pattern]]
* [[wp:Thread pool pattern|Thread pool pattern]]
* [[wp:Visitor pattern|Visitor pattern]]
* [[wp:Workflow patterns|Workflow patterns]]
</div>

=Recently Completed=
If a task has been completed, move the request to this category.  Add a link to the task page, and sign (add <nowiki>--~~~~</nowiki>) to the request.  Completed tasks more than a week old should be removed from the list.

*[[Catalan numbers]] --[[User:Mwn3d|Mwn3d]] 19:27, 16 February 2011 (UTC)

* [[Langton's ant]] (draft task please review) --[[User:AlexLehm|AlexLehm]] 14:27, 30 October 2011 (UTC)

=Discuss=
* Querying devices for certain SNMP values and output reponses to .html or .txt-file.
** Used for: 
*** generating webpage where helpdesk can view the VLAN of a user-port
*** querying forwarding database of switch / ARP-table of router
**Can be extended:
*** with config file for: device list, SNMP-values to interrogate, SNMP community strings, ...
*** support for SNMPv3
*** generating history reports: what mac-address has been on this port, when has a change been made, ...
** Wanted to do this myself for a long time already using Python or Ruby, but not making a lot of progress.  Any help or suggestion would be welcome.
: This should probably be split into [[Query SNMP server]] and [[Retrieve configuration setting]] (for an application, not the SNMP server).  There are already tasks for outputting to a file. --[[User:Short Circuit|Short Circuit]] 23:43, 16 February 2008 (MST)

* Win interface... C++ calls to Fortran F90/95 Source Code ... and back...
: This should be as trivial as [[Call function in shared library]], if the Fortran code has been compiled into a shared library. (Regardless of OS) --[[User:Short Circuit|Short Circuit]] 23:43, 16 February 2008 (MST)

* JSON task(s)
** [[Serialize Data/JSON]]
** [[Deserialize Data/JSON]]
: Serialization and deserialization are extremely common programming tasks, and there are a fair number of open formats for the purpose.  [[wp:Serialization|Serialization]] and deserialization should probably get their own category under [[:Category:Solutions by Programming Task]].  Additionally, json.org has [http://www.json.org/index.html a list of existing JSON implementations], sorted by language, to refer to.  This should be a very quick thing to implement for JSON.  Other formats that should be considered are XML and binary (packed) formats. --[[User:Short Circuit|Short Circuit]] 04:38, 20 June 2009 (UTC)
* Parsing [http://tools.ietf.org/html/rfc4180 RFC 4180 compliant] CSV
** Should take into account escaping of commas, quotes and newline characters

* Dynamic Object Oriented tasks
** I was looking through some of the existing task examples 
::* [[Add a variable to a class instance at runtime]]
::* [[Break_OO_privacy]]
::* [[Respond to an unknown method call]]
::* [[Send_an_unknown_method_call]]
:: and started thinking about them in terms of proofs of concept in a larger framework of dynamically extending objects, classes, methods, etc..  It seems to me that tasks that allowed for more dynamic object manipulation like:
::* Add a variable/method to a class runtime
::* Add a method to a class instance at runtime
would be useful to that end.  I was thinking that doing these as small proof of concepts rather than trying to build some kind of big framework would be most useful.  Now since I'm not a heavy user of OO, I might go overboard here and was wondering what others might think. --[[User:Dgamey|Dgamey]] 17:38, 27 December 2011 (UTC)
* I'd like to see a set of tasks related to representation of graphs and basic algorithms for them. There is a set of tasks around bitmaps, but that's more like API. For graphs, it's different, yet justifies a common category. [[User:Avmich|Avmich]] 05:06, 10 July 2012 (UTC)

* Communicating with a child process using pipes
** I have a procedure, written in Web 68, which creates a child process and provides means of sending data to the child,
via the child's std in,
and getting data from the chid's std out. I want to extend this procedure to provide access to the child's std err output.
I'd like to submit this procedure to rosettacode.org as it is now working satisfactorily. Sian Mountbatten
<poenikatu@fastmail.co.uk>


==Logarithm, sine and cosine implementations==
I just wanted to know if we could do this, I searched the site and was suprised there wasn't an article about this yet, ofcourse the idea is not to use the standard libraries, what do you think? [[User:Rainb|Rainb]] ([[User talk:Rainb|talk]]) 13:14, 19 July 2013 (UTC)

: For the trigonometric (trig) functions, see the Rosetta Code task:   Trigonometric functions.   For the various logarithms, I agree, it would be nice to have Rosetta Code have algorithms for the various log functions (LOG (LOG10), LN, LG (LOG2), LNLN, LOGLOG, LOGP1, LOG to any base, etc.) -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:50, 20 March 2014 (UTC)
