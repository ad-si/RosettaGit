+++
title = "Number names/REXX"
description = ""
date = 2015-06-10T22:15:45Z
aliases = []
[extra]
id = 9317
[taxonomies]
categories = []
tags = []
+++

The full source of the   '''$SPELL#'''   REXX program is included here ──►   [[$SPELL.REX|$SPELL#.REX]].

The full help for the     '''$SPELL#'''   REXX program is included here ──►   [[$SPELL.HEL|$SPELL#.HEL]].

As I understand the solving of any Rosetta Code task, it's OK to use a pre-existing computer language program. 

I had first considered taking the original program and ripping out all the "good stuff" and just solve the primary task (and optional requirements), but it was taking too much time so I left the (whole) REXX program intact. 

Not included here is the   '''$H'''   REXX program which shows (among other things) various forms of help   (program documentation, program/help author, sample uses, or logic flow). 



This REXX program supports: 

* long and short scale   (American and British styles)
* ''ordinal numbers''   and   ''cardinal numbers''
* expressing the number as a (calendar)   ''year''     ('''1975''' would be '''nineteen seventy-five''')
* decimal points   (<big>'''6'''</big>   is different than   <big>'''6.'''</big>)
* decimal fractions
* numbers with leading zeroes (as well as trailing zeroes)
* leading signs   (<big>'''+'''</big> or <big>'''-'''</big>)
* exponential notation
* currency symbols
* adding commas (or whatever) to the output (including blanks)
* numbers   (American) roughly up to 3,000 digits
* numbers   (English)     roughly up to 6,000 digits
* numbers with metric suffixes, namely: 
** '''K M G T P E Z Y X W V U'''
* numbers with binary suffixes, namely:
** '''Ki Mi Gi Ti Pi Ei Zi Yi Xi Wi Vi Ui''' 
** the above suffixes can be combined:   '''44MM'''   (44 million million)
* numbers with some common suffixes such as:
** '''dozens'''
** '''gross'''
** '''greatgross'''
** '''pairs'''
** '''scores'''
** '''googols''' 
** or any suitable abbreviation of the above 
* numbers expressed as a factorial   (such as   '''100!''').
* user-definable names for:
** signs     ('''-6'''   ''could be''   expressed as   '''negatory six''') 
** decimal point     ('''2.4'''   ''could be''   expressed as   '''two boink four''')
** power   (the power of ten that the number is being raised to)
** currency symbols
** zero (the digit)
** blanks
** a value of zero
* numbers may have commas but they aren't enunciated

The program has a pretty hefty prologue which determines   (among other things):

* the operating system (and maybe the type)
* the name of the REXX program: it's filename, filetype, filemode (or path, if you will)
* the access-name used to acquire environmental variables' values
* if '''EBCDIC''' or '''ASCII''' system
* which REXX (or KEXX) is being used
* the name of the program (command) used to clear the terminal screen
* if appropriate, invokes   '''$H'''   to show the help, logic flow, sample uses, or author.

I'm still working on the REXX program to reduce the amount of names by programmatically generating any amount of number-names   (instead of having a fixed amount of names in text form).

Also note that there appears not to be a consensus on the names of some numbers:
* '''nonillion'''   is also called '''noventillion'''
* '''noven*'''        are also named '''novem*'''
* '''septen*'''     are also named '''septem*'''
* '''ses*'''              are also named '''sex*'''
* '''tre*'''                also named '''tres*'''


The   '''$SPELL#'''   REXX program makes use of   '''$T'''   REXX program which is used to display text and/or write the text to a file. 

The   '''$T'''   REXX program is included here ──►   [[$T.REX]].

The help for the   '''$T'''   REXX program is included here ──►   [[$T.HEL]].


The   '''$SPELL#'''   REXX program makes use of   '''$ERR'''   REXX program which is used to display error messages (via '''$T'''). 

The   '''$ERR'''   REXX program is included here ──►     [[$ERR.REX]].

The help for the   '''$ERR'''   REXX program is included here ──►   [[$ERR.HEL]].


Some older REXXes don't have a   '''changestr'''   BIF, so one is included here ──►   [[CHANGESTR.REX]].




==output==
Output  when using the input of:   <tt> 373 ordinal </tt>
 three hundred seventy-third

Output  when using the input of:   <tt> 373 </tt>
 three hundred seventy-three

Output  when using the input of:   <tt> +45 </tt>
 plus forty-five

Output  when using the input of:   <tt> 1989 asYear </tt>
 nineteen eighty-nine

Output  when using the input of:   <tt> -12.79 </tt>
 minus twelve point seven nine

Output  when using the input of:   <tt> $119.63 </tt>
 one hundred nineteen dollars and sixty-three cents

Output  when using the input of:   <tt> 3456T </tt>
 three quadrillion four hundred fifty-six trillion

Output  when using the input of:   <tt> 4Ki </tt>
 four thousand ninety-six

Output  when using the input of:   <tt> 6! </tt>
 seven hundred twenty

Output  when using the input of:   <tt> 10dozen </tt>
 one hundred twenty

Output  when using the input of:   <tt> 70000000000000... </tt>     (3,000 zeroes total)
 seven octononagintanongentillion
