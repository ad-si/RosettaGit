+++
title = "ALGOL 68/prelude"
description = ""
date = 2018-01-07T12:51:02Z
aliases = []
[extra]
id = 4115
[taxonomies]
categories = []
tags = []
+++

The following summarises the useful and various [[ALGOL 68]] prelude templates.  
These are used in several ALGOL 68 code samples.

Note: These are not part of the classic [http://web.comlab.ox.ac.uk/people/Jeremy.Gibbons/wg21/ UNESCO IFIP Working Group 2.1]'s standard prelude, rather they were specifically created for [[Main_Page|Rosetta Code]]'s ALGOL 68 code examples.

###  prelude/general.a68 

Used in:
[[Ternary_logic#ALGOL 68|Ternary_logic]]

{{Template:prelude/general.a68}}

=== prelude/is_prime.a68 ===
Used in:
[[Primality_by_Trial_Division#ALGOL 68|Primality_by_Trial_Division]] and
[[Trial_factoring_of_a_Mersenne_number#ALGOL 68|Trial_factoring_of_a_Mersenne_number]].

{{Template:prelude/pow_mod.a68}}

=== prelude/pow_mod.a68 ===
Used in:
[[Miller-Rabin test#ALGOL 68|Miller-Rabin test]],
[[Multiplicative order#ALGOL 68|Multiplicative order]] and
[[Trial factoring of a Mersenne number#ALGOL 68|Trial factoring of a Mersenne number]].

{{Template:prelude/pow_mod.a68}}


###  prelude/sort.a68 

Used in:
[[Multiplicative order#ALGOL 68|Multiplicative order]],
[[Sort_an_array_of_composite_structures#ALGOL_68|Sort an array of composite structures]] and
[[Sort most popular programming languages#ALGOL 68|Sort most popular programming languages]].

{{Template:prelude/sort.a68}}


###  prelude/errata.a68 

A collections of OPerators, MODES and variables that are "kind of" implied by Algol68's definition

Used in: [[Plot coordinate pairs#ALGOL 68|Plot coordinate pairs]].
* c.f. [[ALGOL_68/prelude/errata.a68]]

=== prelude/graph_2d.a68 ===
A simple tookit for producing basic 2d graphs.

Used in: [[Plot coordinate pairs#ALGOL 68|Plot coordinate pairs]].
* c.f. [[ALGOL_68/prelude/graph_2d.a68]]



###  string in string 

The Algol 68G compiler/interpreter provides a procedure string in string, similar to the standard char in string.
<br/>
Here is a version for other implementations of Algol 68:

```algol68
# A string in string procedure for use with compilers other than Algol 68G   #
# returns TRUE if s is in t, FALSE otherwise.                                #
# if pos is not NIL:                                                         #
#     if s is in t, pos is set to the starting position os s in t,           #
#     pos is left unchanged is s is not in t                                 #
PROC string in string = ( STRING s, REF INT pos, STRING t )BOOL:
     IF s = "" THEN
        # s is empty                                                         #
        IF REF INT(pos) ISNT REF INT(NIL) THEN pos := LWB t FI;
        TRUE
     ELSE
        # s and t are non-empty                                              #
        BOOL found      := FALSE;
        CHAR first char  = s[ LWB s ];
        INT  first pos  := LWB t;
        INT  end pos     = UPB t;
        INT  s length    = ( UPB s - LWB s ) + 1;
        WHILE NOT found AND first pos <= end pos DO
            found := char in string( first char, first pos, t[ first pos : @ first pos ] );
            IF NOT found THEN
                # the first character is not present                         #
                first pos := end pos + 1
            ELIF s = t[ first pos : first pos + ( s length - 1 ) ] THEN
                # found the full string s at first pos                       #
                IF REF INT(pos) ISNT REF INT(NIL) THEN pos := first pos FI
            ELSE
                # haven't got the full string s at first pos                 #
                first pos +:= 1;
                found := FALSE
            FI
        OD;
        found
     FI # string in string # ;
```



=== The classic [http://web.comlab.ox.ac.uk/people/Jeremy.Gibbons/wg21/ UNESCO IFIP Working Group 2.1]'s standard prelude contents ===
Names from the "official" standard prelude: <, <=, +, +:=, +=:, +*, &, ∧, ⌈, ↓, ⌋, ≥, ≤, =, ∨, ⊥, ÷, ÷×, ÷×:=, ÷*, ÷*:=, ÷:=, ×, ×:=, ~, ↑, *, **, *:=, ¬, -, -:=, /, /:=, /=, %, %×, %×:=, %*, %*:=, %:=, >, >=, =, ABS, AND, ARG, BIN, BITS, BOOL, BYTES, CHANNEL, CHAR, COMPL, CONJ, DIVAB, DOWN, ELEM, ENTIER, EQ, FILE, FORMAT, GE, GT, I, IM, INT, LE, LENG, LEVEL, LT, LWB, MINUSAB, MOD, MODAB, NE, NOT, ODD, OR, OVER, OVERAB, PLUSAB, PLUSTO, RE, REAL, REPR, ROUND, SEMA, SHL, SHORTEN, SHR, SIGN, STRING, TIMESAB, UP, UPB, VOID, arccos, arcsin, arctan, associate, backspace, bin possible, bits lengths, bits pack, bits shorths, bits width, blank, bytes lengths, bytes pack, bytes shorths, bytes width, chan, char in string, char number, close, compressible, cos, create, errorchar, estab possible, establish, exp, exp width, fixed, flip, float, flop, get, get bin, get possible, getf, int shorths, int width, last random, line number, ln, lock, make conv, make term, max abs char, max int, max real, newline, newpage, next random, null character, on char error, on format end, on line end, on logical file end, on page end, on physical file end, on value error, open, page number, pi, print, printf, put, put bin, put possible, putf, random, read, read bin, readf, real lengths, real shorths, real width, reidf, reidf possible, reset, reset possible, scratch, set, set char number, set possible, sin, small real, space, sqrt, stand back, stand back channel, stand in, stand in channel, stand out, stand out channel, standconv, stop, tan, whole, write, write bin, writef, L BITS, L BYTES, L COMPL, L INT, L REAL, L arccos, L arcsin, L arctan, L bits pack, L bits width, L bytes pack, L bytes width, L cos, L exp, L exp width, L int width, L last random, L ln, L max int, L max real, L next random, L pi, L random, L real width, L sin, L small real, L sqrt, L tan.

The L indicates extra precision, eg REAL, or LONG REAL or LONG LONG REAL etc, even SHORT REAL etc

These names next were intended for internal compiler use only: אBEYOND, אBFILE, אBOOK, אCOLLECTION, אCOLLITEM, אCONV, אCPATTERN, אFLEXTEXT, אFPATTERN, אFRAME, אGPATTERN, אINSERTION, אINTYPE, אNUMBER, אOUTTYPE, אPATTERN, אPICTURE, אPIECE, אPOS, אROWS, אSFRAME, אSIMPLIN, אSIMPLOUT, אSINSERT, אSTRAIGHTIN, אSTRAIGHTOUT, אTEXT, אalignment, אassociate format, אbfileprotect, אbook bounds, אchainbfile, אchar dig, אcheck pos, אcurrent pos, אdig char, אdo fpattern, אedit string, אfalse, אfile available, אfrom bin, אget char, אget good file, אget good line, אget good page, אget insertion, אget next picture, אgremlins, אidf ok, אindit string, אline ended, אlockedbfile, אlogical file ended, אmatch, אnext pos, אpage ended, אphysical file ended, אput char, אput insertion, אset bin mood, אset char mood, אset mood, אset read mood, אset write mood, אstandardize, אstaticize frames, אstaticize insertion, אstring to L int, אstring to L real, אsubfixed, אsubwhole, אto bin, אundefined, אL standardize
[[Category:ALGOL 68|*]]
