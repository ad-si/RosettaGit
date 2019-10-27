+++
title = "Talk:ALGOL 68/prelude"
description = ""
date = 2009-05-02T01:22:30Z
aliases = []
[extra]
id = 4120
[taxonomies]
categories = []
tags = []
+++

Are these part of the ALGOL standard library? Or just intended for code sharing between tasks?  (I ask because [[Haskell]]'s prelude is effectively its standard library.) Also, you could link from your Sort template to the [[Shell sort]] task, and also link this from the main [[ALGOL 68]] category. --[[User:IanOsgood|IanOsgood]] 15:10, 1 May 2009 (UTC)

:Basically there are a few routines that are very useful, and it seems a bit dirty to cut and past them each time.  In future I will use a template in these cases for the original ( eg into shell sort, as you suggested ), and use a PR READ "module.a68" PR in other places were the routine would be distracting.

:None of those routines (including ''sort'' and ''pow mod'') are part of [[ALGOL 68]]'s standard prelude, see below for a complete list. Specifically there is no ''sort'' in the list below, nor any linear algebra OPerators or routines (which is surprising).

:The people with the "Continuing responsibility ALGOL 68" ([http://web.comlab.ox.ac.uk/people/Jeremy.Gibbons/wg21/ Working Group 2.1 ]) defined only the names in the list below, and there appears to be no continuing effort to evolve any related libraries, maybe in the next 50 years something will happen. :-)  (This is not all bad, as there are several standard libraries in existence today, eg GSL, POSIX etc, that are standard across many langauges, so plugging into these - like ALGOL 68G does - really helps)

:FYI: Here is the complete list of the "standard prelude" routines/operators/files/constants etc:

:Names from the "official" standard prelude: <, <=, +, +:=, +=:, +*, &, ∧, ⌈, ↓, ⌋, ≥, ≤, =, ∨, ⊥, ÷, ÷×, ÷×:=, ÷*, ÷*:=, ÷:=, ×, ×:=, ~, ↑, *, **, *:=, ¬, -, -:=, /, /:=, /=, %, %×, %×:=, %*, %*:=, %:=, >, >=, =, ABS, AND, ARG, BIN, BITS, BOOL, BYTES, CHANNEL, CHAR, COMPL, CONJ, DIVAB, DOWN, ELEM, ENTIER, EQ, FILE, FORMAT, GE, GT, I, IM, INT, LE, LENG, LEVEL, LT, LWB, MINUSAB, MOD, MODAB, NE, NOT, ODD, OR, OVER, OVERAB, PLUSAB, PLUSTO, RE, REAL, REPR, ROUND, SEMA, SHL, SHORTEN, SHR, SIGN, STRING, TIMESAB, UP, UPB, VOID, arccos, arcsin, arctan, associate, backspace, bin possible, bits lengths, bits pack, bits shorths, bits width, blank, bytes lengths, bytes pack, bytes shorths, bytes width, chan, char in string, char number, close, compressible, cos, create, errorchar, estab possible, establish, exp, exp width, fixed, flip, float, flop, get, get bin, get possible, getf, int shorths, int width, last random, line number, ln, lock, make conv, make term, max abs char, max int, max real, newline, newpage, next random, null character, on char error, on format end, on line end, on logical file end, on page end, on physical file end, on value error, open, page number, pi, print, printf, put, put bin, put possible, putf, random, read, read bin, readf, real lengths, real shorths, real width, reidf, reidf possible, reset, reset possible, scratch, set, set char number, set possible, sin, small real, space, sqrt, stand back, stand back channel, stand in, stand in channel, stand out, stand out channel, standconv, stop, tan, whole, write, write bin, writef, L BITS, L BYTES, L COMPL, L INT, L REAL, L arccos, L arcsin, L arctan, L bits pack, L bits width, L bytes pack, L bytes width, L cos, L exp, L exp width, L int width, L last random, L ln, L max int, L max real, L next random, L pi, L random, L real width, L sin, L small real, L sqrt, L tan.

:The L indicates extra precision, eg REAL, or LONG REAL or LONG LONG REAL etc, even SHORT REAL etc

:These names next were intended for internal compiler use only: אBEYOND, אBFILE, אBOOK, אCOLLECTION, אCOLLITEM, אCONV, אCPATTERN, אFLEXTEXT, אFPATTERN, אFRAME, אGPATTERN, אINSERTION, אINTYPE, אNUMBER, אOUTTYPE, אPATTERN, אPICTURE, אPIECE, אPOS, אROWS, אSFRAME, אSIMPLIN, אSIMPLOUT, אSINSERT, אSTRAIGHTIN, אSTRAIGHTOUT, אTEXT, אalignment, אassociate format, אbfileprotect, אbook bounds, אchainbfile, אchar dig, אcheck pos, אcurrent pos, אdig char, אdo fpattern, אedit string, אfalse, אfile available, אfrom bin, אget char, אget good file, אget good line, אget good page, אget insertion, אget next picture, אgremlins, אidf ok, אindit string, אline ended, אlockedbfile, אlogical file ended, אmatch, אnext pos, אpage ended, אphysical file ended, אput char, אput insertion, אset bin mood, אset char mood, אset mood, אset read mood, אset write mood, אstandardize, אstaticize frames, אstaticize insertion, אstring to L int, אstring to L real, אsubfixed, אsubwhole, אto bin, אundefined, אL standardize

[[User:NevilleDNZ|NevilleDNZ]] 01:22, 2 May 2009 (UTC)
