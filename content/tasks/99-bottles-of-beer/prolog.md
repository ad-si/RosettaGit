+++
title = "Prolog"
description = ""
date = 2014-11-21T12:00:48Z
aliases = []
[extra]
id = 18272
[taxonomies]
categories = []
tags = []
+++

<!--
=Prolog=
-->
{{collection|99 Bottles of Beer}}
[[99 Bottles of Beer]] done in Prolog-languages
<!--
See [[99 Bottles of Beer/Prolog]]
-->

__toc__


## Prolog

{{works with|SWI Prolog}}

```prolog
bottles(0):-!.
bottles(X):-
    writef('%t bottles of beer on the wall \n',[X]),
    writef('%t bottles of beer\n',[X]),
    write('Take one down, pass it around\n'),
    succ(XN,X),
    writef('%t bottles of beer on the wall \n\n',[XN]),
    bottles(XN).

:- bottles(99).
```


An other version that handles plural/not plural conditions.


```prolog
line1(X):- line2(X),write(' on the wall').
line2(0):- write('no more bottles of beer').
line2(1):- write('1 bottle of beer').
line2(X):- writef('%t bottles of beer',[X]).
line3(1):- write('Take it down, pass it around').
line3(X):- write('Take one down, pass it around').
line4(X):- line1(X).

bottles(0):-!.
bottles(X):-
    succ(XN,X),
    line1(X),nl,
    line2(X),nl,
    line3(X),nl,
    line4(XN),nl,nl,
    !,
    bottles(XN).

:- bottles(99).
```



## Visual Prolog


```visual prolog

implement main
    open core, std, console

class predicates
    bottles : (integer) -> string procedure (i).

clauses
    bottles(1) = "bottle" :- !.
    bottles(_) = "bottles".

    run():-
        init(),
        foreach B = downTo(99,1) do
            write(B," ",bottles(B), " of beer on the wall,\n"),
            write(B," ",bottles(B), " of beer,\n"),
            write("Take one down, pass it around,\n"),
            write(B-1," ",bottles(B-1)," of beer on the wall.\n\n")
        end foreach,

        succeed().
end implement main

goal
    mainExe::run(main::run).

```

