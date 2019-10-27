+++
title = "Execute Brain****/Brain****"
description = ""
date = 2014-04-21T13:42:25Z
aliases = []
[extra]
id = 17546
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}
'''Dbfi''' is a [[brainfuck]] self-interpreter written by [[Daniel B. Cristofani]] [http://www.hevanet.com/cristofd/dbfi.b] and described in detail in [http://arxiv.org/abs/cs/0311032v1]. This is the shortest known brainfuck self-interpreter and possibly the shortest self-interpreter amongst all imperative languages. Note that a big part of dbfi code is devoted to ASCII decoding, comment handling, and avoiding undefined behaviour. Removing this would reduce the size to nearly half.
 >>>+[[-]>>[-]++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[
 ->>]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<
 ]<]<[[<]>[[>]>>[>>]+[<<]<[<]<+>>-]>[>]+[->>]<<<<[[<<]<[<]+<<[+>+<<-[>-->+<<-[>
 +<[>>+<<-]]]>[<+>-]<]++>>-->[>]>>[>>]]<<[>>+<[[<]<]>[[<<]<[<]+[-<+>>-[<<+>++>-
 [<->[<<+>>-]]]<[>+<-]>]>[>]>]>[>>]>>]<<[>>+>>+>>]<<[->>>>>>>>]<<[>.>>>>>>>]<<[
 >->>>>>]<<[>,>>>]<<[>+>]<<[+<<]<]

Dbfi inspired Clive Gifford to write the fastest Brainfuck self-interpreter (since dbfi is the shortest, but not the fastest) [http://eigenratios.blogspot.com/2006/12/very-fast-brainfck-self-interpreter.html]. Its compressed version is
 >>>>>+[->>++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[->>
 ]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<]>[
 -]+<<[--[[-]>>->+<<<]>>[-<<<<[>+<-]>>>>>>+<]<<]>>[-]<<>>>[<<<+>>>-]<<<]>>>+<<<
 <[<<]>>[[<+>>+<-]+<-[-[-[-[-[-[-[->->>[>>]>>[>>]<+<[<<]<<[<<]<]>[->>[>>]>>[>>]
 <,<[<<]<<[<<]]<]>[->>[>>]>>[>>]<-<[<<]<<[<<]]<]>[->>[>>]>>[>>]<.<[<<]<<[<<]]<]
 >[->>[>>]>>[>>]<<-<<[<<]<<[<<]]<]>[->>[>>]>>[>>]+[<<]<<[<<]]<]>[->>[>>]>>[>>]<
 [>+>>+<<<-]>[<+>-]>>[<<+>>[-]]+<<[>>-<<-]>>[<<+>>>>+<<-]>>[<<+>>-]<<[>>+<<-]+>
 >[<<->>-]<<<<[-<<[<<]<<[<<]<<<<<++>>]>>[-<<<<<<[<<]<<[<<]<]>]<]>[->>[>>]>>[>>]
 <[>+>>+<<<-]>[[<+>-]>>[-]+<<]>>[<<+>>>>+<<-]>>[<<+>>-]<<[>>+<<-]+>>[<<->>-]<<<
 <[-<<[<<]<<[<<]<<<<<+>>]>>[-<<<<<<[<<]<<[<<]<]>]>[<+>-]<<<<<<[>>+<<-[->>->>+[>
 >>[-<+>>+<]+<-[-[[-]>[-]<]>[-1<<<+>>>]<]>[-<<<->>>]>[-<+>]<<<<[>>+<<-]>>]<<<<<
 <]>>[-<<+[>>>[-<+>>+<]+<-[-[[-]>[-]<]>[-1<<<->>>]<]>[-<<<+>>>]>[-<+>]<<<<[<<+>
 >-]<<]]]>>>>>>>]
Below is relative comparison of time to run a simple program with a stack of two self-interpreters.
{| class="wikitable"
|-
| '''Lower SI''' || '''Upper SI''' || '''Time'''
|-
| cgbfi || cgbfi || 9.1
|-
| cgbfi || dbfi || 7.0
|-
| dbfi || cgbfi || 35.0
|-
| dbfi || dbfi || 21.3
|-
|}
Here cgbfi stands for Clive Gifford Brainfuck Interpreter. Time is given in seconds, which does not matter, because only relative values are important.

The second line shows faster result because DBFI is shorter than CGBSI, and this outweighs the speed benefit of the faster self-interpreter used in the second level in the stack of interpreters. The short program used for the comparison is
 >+>+>+>+>++<[>[<+++>-]<<]> [>+>+<<-]>[-]>.!
and the interpreter [http://mazonka.com/brainf/bff4.c bff4.c].

Content is available under [http://creativecommons.org/publicdomain/zero/1.0/ CC0 public domain dedication] via [http://esolangs.org/wiki/Dbfi the esolang wiki].
