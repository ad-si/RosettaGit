+++
title = "Forth common practice"
description = ""
date = 2007-09-17T20:45:17Z
aliases = []
[extra]
id = 2134
[taxonomies]
categories = []
tags = []
+++

The [[Forth]] language has a standardized vocabulary, but many words have become common practice since that 1994 standard.
  1 cells constant  cell
 -1 cells constant -cell
 : cell-  -cell + ;

 : -rot ( a b c -- c a b ) rot rot ;

 : bounds ( addr len -- limit addr ) over + swap ;  \ convert string/array to DO-LOOP limits

DEFER-IS for late-bound, revectorable, and forward-referenced words:
 : noop ( -- ) ;
 : defer  create ( "name" -- ) ['] noop ,   does> ( -- ) @ execute ;
 : is ( xt "name" -- ) ' >body ! ;
 
 defer lessthan
 ' < is lessthan
 2 3 lessthan .  \ -1 (true)

Alternate local variable syntax using curly braces, designed to look like a regular stack comment:
 : muldiv { a b -- a*b a/b }     \ all stuff after "--" is ignored
   a b *  a b / ;

Some counted string utilities:
 : c+! ( n caddr -- ) dup >r c@ + r> c! ;
 : append ( src len dest -- ) 2dup 2>r  count + swap move  2r> c+! ;
 : place ( src len dest -- ) 2dup 2>r  1+ swap move  2r> c! ;
 : scan ( str len char -- str' len' ) >r begin dup while over c@ r@ <> while 1 /string repeat then r> drop ;
 : skip ( str len char -- str' len' ) >r begin dup while over c@ r@ =  while 1 /string repeat then r> drop ;
 : split ( str len char -- str1 len1 str2 len2 ) >r 2dup r> scan 2swap 2 pick - ;
