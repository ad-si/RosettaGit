+++
title = "RCSNUSP/Factor"
description = ""
date = 2019-10-04T15:53:42Z
aliases = []
[extra]
id = 22574
[taxonomies]
categories = []
tags = []
+++


This is a translation of the Go entry [http://www.rosettacode.org/wiki/Execute_SNUSP/Go], and as such consists of core SNUSP, a fixed-size data store, and no bounds checking. It has been made a bit more idiomatic for Factor by splitting the implementation up into several words (functions), and uses a tuple in place of Go's lexical variables.

```factor
USING: accessors byte-arrays combinators
combinators.short-circuit fry io kernel math multiline sequences
splitting strings ;
IN: rosetta-code.snusp

STRING: sample-program
/++++!/
### =====
?\>++.>+.+++++++..+++\
\+++\ | /+>+++++++>/ /++++++++++<<.++>./
$+++/ | \+++++++++>\ \+++++.>.+++.-----\
      \==-<<<<+>+++/ /=.>.+>.--------.-/
;

: find-start ( matrix -- r c )
    [ CHAR: $ swap member? ] find [ CHAR: $ = ] find drop dup
    [ 1 + ] [ 2drop 0 0 ] if ;

! Instruction store, data store, instruction pointer row,
! instruction pointer column, data pointer, instruction
! direction
TUPLE: snusp is ds ipr ipc dp id ;

: <snusp> ( str n -- snusp )
    [ "\n" split ] [ <byte-array> ] bi* over find-start 0 0
    snusp boa ;

: step ( snusp -- )
    1 over id>> [ 2 bitand - ] [ 1 bitand zero? ] bi
    [ '[ _ + ] change-ipc ] [ '[ _ + ] change-ipr ] if drop ;

: still-running? ( snusp -- ? )
    {
        [ ipr>> 0 >= ]
        [ [ ipr>> ] [ is>> length < ] bi ]
        [ ipc>> 0 >= ]
        [ [ ipc>> ] [ ipr>> ] [ is>> nth length < ] tri ]
    } 1&& ;

: data ( snusp -- dp ds ) [ dp>> ] [ ds>> ] bi ;

CONSTANT: commands {
    { CHAR: > [ [ 1 + ] change-dp drop ] }
    { CHAR: < [ [ 1 - ] change-dp drop ] }
    { CHAR: + [ data [ 1 + ] change-nth ] }
    { CHAR: - [ data [ 1 - ] change-nth ] }
    { CHAR: . [ data nth 1string write ] }
    { CHAR: , [ read1 swap data set-nth ] }
    { CHAR: / [ [ bitnot ] change-id drop ] }
    { CHAR: \\ [ [ 1 bitxor ] change-id drop ] }
    { CHAR: ! [ step ] }
    { CHAR: ? [ dup data nth zero? [ step ] [ drop ] if ] }
    [ 2drop ]
}

: curr-instr ( snusp -- n )
    [ ipc>> ] [ ipr>> ] [ is>> ] tri nth nth ;

: execute-snusp ( snusp -- )
    [ dup still-running? ]
    [ dup dup curr-instr commands case dup step ] while drop ;

: snusp-demo ( -- ) sample-program 5 <snusp> execute-snusp ;

MAIN: snusp-demo

```
```txt

Hello World!

```

