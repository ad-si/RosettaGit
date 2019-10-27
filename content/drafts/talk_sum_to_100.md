+++
title = "Talk:Sum to 100"
description = ""
date = 2017-07-11T19:09:36Z
aliases = []
[extra]
id = 21264
[taxonomies]
categories = []
tags = []
+++

==A 'sum' that can be shown but not expressed ?==

Not sure that I have understood your third request. Something that can be 'shown' but not 'expressed' ? Sounds appeallingly ineffable and mysterious, but perhaps not entirely clear ...

Judging by your undo of my first suggestion, I guess that you are '''not''' simply looking for the first or lowest-valued integer than can't be expressed as a sum constructed in this way, but then I don't actually have any idea of what you '''are''' asking for. Perhaps some kind of example or concretisation might clarify ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 01:42, 1 January 2017 (UTC)

: See the REXX's output to help understand what is required.   I have changed the word   '''first'''   to   '''lowest'''   which is much clearer to what was intended   (I knew what I meant, but it was somewhat unclear in this context).   (Note that negative sums/integers are possible, these sums are just negative "versions" of the positive sums, so the requirement was worded to avoid looking for negative results.)   Since the sums are integers (as there is no way to get a non-integer sum from additions and/or subtractions of decimal digits or whole numbers).   I used the word "expressed" as it isn't necessary to have the expression "shown".   The word "shown" (to me) seems to indicate to display, not necessarily to express (in this case, mathematically).   To answer your question, yes, it is possible.   Showing something doesn't mean that it is being expressed   (in the sense that it is being understood).   I believe it boils down to word choices.   I was trying for a concise wording of the requirement without being unnecessarily wordy (or obtuse).   It's a fine art.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:04, 1 January 2017 (UTC)

: I've also added a trailing phrase to the 3<sup>rd</sup> requirement that (I think) fits in more to what you originally addressed.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:09, 1 January 2017 (UTC)

:: Would it be consistent with your meaning to replace the word 'sum' with 'integer' ? Also if you are looking for the lowest integer that is inexpressible in these terms, perhaps it would be helpful to give an example of a higher integer of this kind, and demonstrate/explain its inexpressibility within this system ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 02:16, 1 January 2017 (UTC)

::: I used the word '''sum''' because it was already used in the task's preamble and the word was also defined (in context).   Introducing another word (integer) would, I think, just belabor the effort to have another version of the '''sum''' word.   The next number   (after '''211''')   that can't be expressed within the rules is   '''219'''.   Another example of a sum that can't be expressed is   '''5791'''.   I'm not sure how to demonstrate/explain its inexpressibility other than to show (display)   ''all''   the possible mathematical expressions that '''don't''' (can't) express the   '''5791'''   sum.   This would be somewhat of a strange proof (and voluminous), but not unheard of.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:29, 1 January 2017 (UTC)

:::: FWIW my experience as a reader is that using the word 'sum' at that point certainly proved confusing at first reading, and even now feels distractingly odd. 211 is certainly a '''number''' – a member of the set of integers – but the point you are making about it seems to be precisely that it is '''not''' a sum (the result of an addition) within this system. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 02:38, 1 January 2017 (UTC)

::::: I'm not saying that '''211''' isn't an integer or a number --- just that it (the sum of 211) can't be expressed mathematically in the context of the rules for this puzzle as stated in the task's preamble.   '''211''' is a sum (in particular, the lowest sum) of the set of sums that can't be expressed within the rules.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:56, 1 January 2017 (UTC)

:::: PS on the issue of an example; an existence proof would be fine – you could just give an example of a number like 5791, say that it can't be expressed by a sum that is restricted to these permutations of digits and signs, and ask for code that finds the lowest positive integer that shares this property. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 02:46, 1 January 2017 (UTC)

::::: That seems overly wordy just to ask for the lowest positive sum that can't be expressed (within the rules).   In particular, the requirement was expressly worded to find   ''that''   sum.   However, I will add an example (i.e.:   such as 5074).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:56, 1 January 2017 (UTC)

==a discussion of the task's requirements and wording==
This (first) section of comments was moved here to the ''talk'' page from the task's requirements.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:07, 11 July 2017 (UTC)
 

Note: If you allow/disallow negative first numbers you do get different results. If you allow negative first numbers, which you should according to the specification; then you get at least two targets with the same highest number of solutions: The positive and negative versions of the same target, only the positive one is between 0 to 123456789.  The specification assumes there is only one positive target with the maximum number of solutions.  It is worth looking for multiple targets with the same number of solutions, just in case there are more.  This is especially true if you try the variants with different lists of digits. For example using the digits '987654321' and excluding a negative first number comes up with two distinct targets with the same maximum number of solutions, both of which happen to be positive. If you are playing with the asymetric version with out leading negative numbers of different lists of digits the sums with the highest number of solution may only be negative.  In this case it is worth excluding the test to show only positive totals and put up with seeing duplicate +ve and -ve solutions.

Robin Murison‎

:::::: Added signature of Robin Murison by [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:07, 11 July 2017 (UTC)

== extra credit ==
I had initially thought to add an extra credit solution for the   ''largest''   sum   (except for the obvious   '''123456789''').   I'm not sure it would be a worthwhile (optional) requirement, it could be pretty CPU intensive.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 04:21, 1 January 2017 (UTC)

: Seems fine as an additional task – I've just added it for Haskell and it doesn't seem to noticeably increase the interpreter run-time. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 00:29, 5 January 2017 (UTC)

:: For interpreted languages, it may be problematic.   Even so, I added an "extra credit" (optional) requirement.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:44, 5 January 2017 (UTC)

::: I might not be optimistic with AppleScript, but JavaScript and GHCi (the Haskell interpreter that comes with the GHC compiler) both seem quite relaxed about it when run from inside a code editor. Of course the data types and algorithm will make a difference :-) [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 01:01, 5 January 2017 (UTC)

:: For performance note that it is not necessary to generate the strings to count the solutions. There are 2 * 3 ** (base-2) possible strings. In base 2 the maximum value is 1 the minimum value -1. 0 can not be generated so 66% of values can be represented. The number of possible strings increases exponentially with the base, so to stretch this task for extra credit we could ask what is the %age in base 64, or 60 if we want to be Babylonian, or why not more. I often read that we use base 10 because we have 10 fingers, is there any archeological evidence that the Babylonians were severely deformed? --[[User:Nigel Galloway|Nigel Galloway]] ([[User talk:Nigel Galloway|talk]]) 12:54, 25 February 2017 (UTC)

==complete list of solutions for   '''9'''==
For those that are interested (or need a sanity check), the 46 solutions (for the sum of 9) are:

```txt

solution:  9  ◄───►  -1+2+3+4+5+6+7-8-9
solution:  9  ◄───►  -1+2+3-4+5-6-7+8+9
solution:  9  ◄───►  -1+2+3-4-5+6+7-8+9
solution:  9  ◄───►  -1+2+3-45+67-8-9
solution:  9  ◄───►  -1+2+34+56+7-89
solution:  9  ◄───►  -1+2-3+4+5-6+7-8+9
solution:  9  ◄───►  -1+2-3+4-5+6+7+8-9
solution:  9  ◄───►  -1+23+4+5+67-89
solution:  9  ◄───►  -1+23+4-5-6-7-8+9
solution:  9  ◄───►  -1+23-4+5-6-7+8-9
solution:  9  ◄───►  -1+23-4-5+6+7-8-9
solution:  9  ◄───►  -1-2+3+4+5+6-7-8+9
solution:  9  ◄───►  -1-2+3+4+5-6+7+8-9
solution:  9  ◄───►  -1-2+3-4-5-6+7+8+9
solution:  9  ◄───►  -1-2+3-4-56+78-9
solution:  9  ◄───►  -1-2-3+4-5+6-7+8+9
solution:  9  ◄───►  -1-2-3+45-6-7-8-9
solution:  9  ◄───►  -1-2-3-4+5+6+7-8+9
solution:  9  ◄───►  -1-2-34+56+7-8-9
solution:  9  ◄───►  -1-23+45-6-7-8+9
solution:  9  ◄───►  -12+3-45-6+78-9
solution:  9  ◄───►  -12+34+5+6-7-8-9
solution:  9  ◄───►  -12+34+56-78+9
solution:  9  ◄───►  -12-34+5+67-8-9
solution:  9  ◄───►  1+2+3+4-5-6-7+8+9
solution:  9  ◄───►  1+2+3-4+5-6+7-8+9
solution:  9  ◄───►  1+2+3-4-5+6+7+8-9
solution:  9  ◄───►  1+2-3+4+5+6-7-8+9
solution:  9  ◄───►  1+2-3+4+5-6+7+8-9
solution:  9  ◄───►  1+2-3-4-5-6+7+8+9
solution:  9  ◄───►  1+2-3-4-56+78-9
solution:  9  ◄───►  1+2-34-56+7+89
solution:  9  ◄───►  1+23+4-5-6-7+8-9
solution:  9  ◄───►  1+23-4+5-6+7-8-9
solution:  9  ◄───►  1+23-45+6+7+8+9
solution:  9  ◄───►  1-2+3+4+5+6-7+8-9
solution:  9  ◄───►  1-2+3-4-5+6-7+8+9
solution:  9  ◄───►  1-2-3+4+5-6-7+8+9
solution:  9  ◄───►  1-2-3+4-5+6+7-8+9
solution:  9  ◄───►  1-2-3-4+5+6+7+8-9
solution:  9  ◄───►  1-2-3-4-5-67+89
solution:  9  ◄───►  1-23+4+5-67+89
solution:  9  ◄───►  1-23+45-6-7+8-9
solution:  9  ◄───►  1-23-4+5+6+7+8+9
solution:  9  ◄───►  1-23-45-6-7+89
solution:  9  ◄───►  12-34-56+78+9
       46 solutions found for 9

```


==complete list of the number of solutions for zero ──► 300==
For those that are interested in the density of solutions:

```txt

       22 solutions found for   0 ──────────────────────
       43 solutions found for   1 ───────────────────────────────────────────
       18 solutions found for   2 ──────────────────
       41 solutions found for   3 ─────────────────────────────────────────
       18 solutions found for   4 ──────────────────
       40 solutions found for   5 ────────────────────────────────────────
       24 solutions found for   6 ────────────────────────
       39 solutions found for   7 ───────────────────────────────────────
       18 solutions found for   8 ──────────────────
       46 solutions found for   9 ──────────────────────────────────────────────
       17 solutions found for  10 ─────────────────
       38 solutions found for  11 ──────────────────────────────────────
       27 solutions found for  12 ───────────────────────────
       38 solutions found for  13 ──────────────────────────────────────
       24 solutions found for  14 ────────────────────────
       43 solutions found for  15 ───────────────────────────────────────────
       18 solutions found for  16 ──────────────────
       39 solutions found for  17 ───────────────────────────────────────
       23 solutions found for  18 ───────────────────────
       37 solutions found for  19 ─────────────────────────────────────
       23 solutions found for  20 ───────────────────────
       43 solutions found for  21 ───────────────────────────────────────────
       25 solutions found for  22 ─────────────────────────
       36 solutions found for  23 ────────────────────────────────────
       32 solutions found for  24 ────────────────────────────────
       36 solutions found for  25 ────────────────────────────────────
       25 solutions found for  26 ─────────────────────────
       44 solutions found for  27 ────────────────────────────────────────────
       25 solutions found for  28 ─────────────────────────
       35 solutions found for  29 ───────────────────────────────────
       34 solutions found for  30 ──────────────────────────────────
       31 solutions found for  31 ───────────────────────────────
       26 solutions found for  32 ──────────────────────────
       37 solutions found for  33 ─────────────────────────────────────
       24 solutions found for  34 ────────────────────────
       35 solutions found for  35 ───────────────────────────────────
       32 solutions found for  36 ────────────────────────────────
       32 solutions found for  37 ────────────────────────────────
       27 solutions found for  38 ───────────────────────────
       37 solutions found for  39 ─────────────────────────────────────
       31 solutions found for  40 ───────────────────────────────
       26 solutions found for  41 ──────────────────────────
       34 solutions found for  42 ──────────────────────────────────
       34 solutions found for  43 ──────────────────────────────────
       29 solutions found for  44 ─────────────────────────────
       42 solutions found for  45 ──────────────────────────────────────────
       27 solutions found for  46 ───────────────────────────
       27 solutions found for  47 ───────────────────────────
       35 solutions found for  48 ───────────────────────────────────
       26 solutions found for  49 ──────────────────────────
       28 solutions found for  50 ────────────────────────────
       35 solutions found for  51 ───────────────────────────────────
       29 solutions found for  52 ─────────────────────────────
       33 solutions found for  53 ─────────────────────────────────
       30 solutions found for  54 ──────────────────────────────
       26 solutions found for  55 ──────────────────────────
       23 solutions found for  56 ───────────────────────
       29 solutions found for  57 ─────────────────────────────
       32 solutions found for  58 ────────────────────────────────
       25 solutions found for  59 ─────────────────────────
       33 solutions found for  60 ─────────────────────────────────
       30 solutions found for  61 ──────────────────────────────
       24 solutions found for  62 ────────────────────────
       34 solutions found for  63 ──────────────────────────────────
       22 solutions found for  64 ──────────────────────
       26 solutions found for  65 ──────────────────────────
       32 solutions found for  66 ────────────────────────────────
       30 solutions found for  67 ──────────────────────────────
       25 solutions found for  68 ─────────────────────────
       29 solutions found for  69 ─────────────────────────────
       22 solutions found for  70 ──────────────────────
       25 solutions found for  71 ─────────────────────────
       26 solutions found for  72 ──────────────────────────
       24 solutions found for  73 ────────────────────────
       22 solutions found for  74 ──────────────────────
       26 solutions found for  75 ──────────────────────────
       25 solutions found for  76 ─────────────────────────
       22 solutions found for  77 ──────────────────────
       24 solutions found for  78 ────────────────────────
       25 solutions found for  79 ─────────────────────────
       19 solutions found for  80 ───────────────────
       31 solutions found for  81 ───────────────────────────────
       20 solutions found for  82 ────────────────────
       23 solutions found for  83 ───────────────────────
       25 solutions found for  84 ─────────────────────────
       21 solutions found for  85 ─────────────────────
       18 solutions found for  86 ──────────────────
       23 solutions found for  87 ───────────────────────
       17 solutions found for  88 ─────────────────
       24 solutions found for  89 ────────────────────────
       21 solutions found for  90 ─────────────────────
       22 solutions found for  91 ──────────────────────
       21 solutions found for  92 ─────────────────────
       23 solutions found for  93 ───────────────────────
       17 solutions found for  94 ─────────────────
       17 solutions found for  95 ─────────────────
       19 solutions found for  96 ───────────────────
       21 solutions found for  97 ─────────────────────
       15 solutions found for  98 ───────────────
       25 solutions found for  99 ─────────────────────────
       12 solutions found for 100 ────────────
       18 solutions found for 101 ──────────────────
       19 solutions found for 102 ───────────────────
       14 solutions found for 103 ──────────────
       14 solutions found for 104 ──────────────
       18 solutions found for 105 ──────────────────
       18 solutions found for 106 ──────────────────
       17 solutions found for 107 ─────────────────
       19 solutions found for 108 ───────────────────
       12 solutions found for 109 ────────────
       14 solutions found for 110 ──────────────
       19 solutions found for 111 ───────────────────
       13 solutions found for 112 ─────────────
       12 solutions found for 113 ────────────
       17 solutions found for 114 ─────────────────
       15 solutions found for 115 ───────────────
       14 solutions found for 116 ──────────────
       20 solutions found for 117 ────────────────────
       13 solutions found for 118 ─────────────
       11 solutions found for 119 ───────────
       18 solutions found for 120 ──────────────────
       10 solutions found for 121 ──────────
        9 solutions found for 122 ─────────
       13 solutions found for 123 ─────────────
       12 solutions found for 124 ────────────
       12 solutions found for 125 ────────────
       19 solutions found for 126 ───────────────────
        8 solutions found for 127 ────────
       12 solutions found for 128 ────────────
       12 solutions found for 129 ────────────
       13 solutions found for 130 ─────────────
        7 solutions found for 131 ───────
       15 solutions found for 132 ───────────────
       14 solutions found for 133 ──────────────
        8 solutions found for 134 ────────
       16 solutions found for 135 ────────────────
       10 solutions found for 136 ──────────
        5 solutions found for 137 ─────
       15 solutions found for 138 ───────────────
        7 solutions found for 139 ───────
       10 solutions found for 140 ──────────
       10 solutions found for 141 ──────────
       10 solutions found for 142 ──────────
        9 solutions found for 143 ─────────
       14 solutions found for 144 ──────────────
        8 solutions found for 145 ────────
        8 solutions found for 146 ────────
       11 solutions found for 147 ───────────
        9 solutions found for 148 ─────────
        6 solutions found for 149 ──────
       10 solutions found for 150 ──────────
        9 solutions found for 151 ─────────
        6 solutions found for 152 ──────
       13 solutions found for 153 ─────────────
        9 solutions found for 154 ─────────
        6 solutions found for 155 ──────
       10 solutions found for 156 ──────────
        8 solutions found for 157 ────────
        4 solutions found for 158 ────
        8 solutions found for 159 ────────
        5 solutions found for 160 ─────
        4 solutions found for 161 ────
       11 solutions found for 162 ───────────
        5 solutions found for 163 ─────
        6 solutions found for 164 ──────
        7 solutions found for 165 ───────
        4 solutions found for 166 ────
        5 solutions found for 167 ─────
        3 solutions found for 168 ───
        8 solutions found for 169 ────────
        4 solutions found for 170 ────
       10 solutions found for 171 ──────────
        6 solutions found for 172 ──────
        4 solutions found for 173 ────
        5 solutions found for 174 ─────
        3 solutions found for 175 ───
        1 solution  found for 176 ─
        6 solutions found for 177 ──────
        3 solutions found for 178 ───
        3 solutions found for 179 ───
        7 solutions found for 180 ───────
        2 solutions found for 181 ──
        4 solutions found for 182 ────
        4 solutions found for 183 ────
        3 solutions found for 184 ───
        5 solutions found for 185 ─────
        4 solutions found for 186 ────
        5 solutions found for 187 ─────
        4 solutions found for 188 ────
        6 solutions found for 189 ──────
        6 solutions found for 190 ──────
        1 solution  found for 191 ─
        4 solutions found for 192 ────
        2 solutions found for 193 ──
        1 solution  found for 194 ─
        4 solutions found for 195 ────
        1 solution  found for 196 ─
        3 solutions found for 197 ───
        7 solutions found for 198 ───────
        3 solutions found for 199 ───
        3 solutions found for 200 ───
        4 solutions found for 201 ────
        2 solutions found for 202 ──
        3 solutions found for 203 ───
        2 solutions found for 204 ──
        2 solutions found for 205 ──
        2 solutions found for 206 ──
        6 solutions found for 207 ──────
        4 solutions found for 208 ────
        1 solution  found for 209 ─
        5 solutions found for 210 ─────
       no solutions found for 211
        4 solutions found for 212 ────
        1 solution  found for 213 ─
        3 solutions found for 214 ───
        1 solution  found for 215 ─
        6 solutions found for 216 ──────
        1 solution  found for 217 ─
        3 solutions found for 218 ───
       no solutions found for 219
        3 solutions found for 220 ───
       no solutions found for 221
        4 solutions found for 222 ────
        1 solution  found for 223 ─
        4 solutions found for 224 ────
        2 solutions found for 225 ──
        5 solutions found for 226 ─────
       no solutions found for 227
        4 solutions found for 228 ────
       no solutions found for 229
        3 solutions found for 230 ───
        2 solutions found for 231 ──
        2 solutions found for 232 ──
       no solutions found for 233
        5 solutions found for 234 ─────
       no solutions found for 235
        3 solutions found for 236 ───
        1 solution  found for 237 ─
        3 solutions found for 238 ───
       no solutions found for 239
        6 solutions found for 240 ──────
       no solutions found for 241
        5 solutions found for 242 ─────
        3 solutions found for 243 ───
        6 solutions found for 244 ──────
        2 solutions found for 245 ──
        5 solutions found for 246 ─────
       no solutions found for 247
        2 solutions found for 248 ──
        3 solutions found for 249 ───
        2 solutions found for 250 ──
        2 solutions found for 251 ──
        7 solutions found for 252 ───────
        1 solution  found for 253 ─
        4 solutions found for 254 ────
        2 solutions found for 255 ──
        5 solutions found for 256 ─────
        2 solutions found for 257 ──
        7 solutions found for 258 ───────
        1 solution  found for 259 ─
        4 solutions found for 260 ────
        3 solutions found for 261 ───
        3 solutions found for 262 ───
        1 solution  found for 263 ─
        4 solutions found for 264 ────
        4 solutions found for 265 ────
        2 solutions found for 266 ──
        5 solutions found for 267 ─────
        2 solutions found for 268 ──
        2 solutions found for 269 ──
        6 solutions found for 270 ──────
        2 solutions found for 271 ──
        1 solution  found for 272 ─
        3 solutions found for 273 ───
        1 solution  found for 274 ─
        1 solution  found for 275 ─
        3 solutions found for 276 ───
       no solutions found for 277
        4 solutions found for 278 ────
        3 solutions found for 279 ───
        4 solutions found for 280 ────
        4 solutions found for 281 ────
        2 solutions found for 282 ──
        4 solutions found for 283 ────
       no solutions found for 284
        2 solutions found for 285 ──
       no solutions found for 286
       no solutions found for 287
        2 solutions found for 288 ──
        1 solution  found for 289 ─
        1 solution  found for 290 ─
        2 solutions found for 291 ──
        1 solution  found for 292 ─
        1 solution  found for 293 ─
        3 solutions found for 294 ───
        1 solution  found for 295 ─
        3 solutions found for 296 ───
        2 solutions found for 297 ──
        2 solutions found for 298 ──
        2 solutions found for 299 ──
       no solutions found for 300

sum of  9  has the maximum number of solutions:  46
sums of  211 219 221 227 229 233 235 239 241 247 277 284 286 287 300  have the minimum number of solutions:  0

```


==a discussion of the task's requirements and wording==
This (added) section of superscript comments was moved here to the   ''talk''   page from the task's requirements (the task's preamble).   I also changed the hard-to-read superscript text to be displayed as "regular" text).   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:12, 11 July 2017 (UTC)
 

:: Note: If you allow/disallow negative first numbers you do get different results. If you allow negative first numbers, which you should according to the specification; then you get at least two targets with the same highest number of solutions: The positive and negative versions of the same target, only the positive one is between 0 to 123456789.  The specification assumes there is only one positive target with the maximum number of solutions.  It is worth looking for multiple targets with the same number of solutions, just in case there are more.  This is especially true if you try the variants with different lists of digits. For example using the digits '987654321' and excluding a negative first number comes up with two distinct targets with the same maximum number of solutions, both of which happen to be positive. If you are playing with the asymetric version with out leading negative numbers of different lists of digits the sums with the highest number of solution may only be negative.  In this case it is worth excluding the test to show only positive totals and put up with seeing duplicate +ve and -ve solutions.

:: Robin Murison‎

::::::: Added signature of   ''Robin Murison''   by [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:12, 11 July 2017 (UTC)

-----

Yes, a minus sign ('''-''') is allowed before the first digit(s)   This is specifically allowed from the 2<sup>nd</sup> sentence in the task's preamble, there is no '''if''' about it.

There is only one sum (you call it a target) that has a maximum.   This wasn't assumed, it was determined by examination of all possible targets (which were non-negative sums), in part, to give programmers   ''a priori''    information.   The 2<sup>nd</sup> task requirement asks '''the''' sum which has the maximum number of solutions   (from zero to infinity --- this excludes negative sums/targets).   There is only one correct answer --- but you may assume there will be more than one solution, and if so, the wording will be changed.   The task's preamble could've been worded as asking for   '''sum(s)''';   it was a word choice that I made when composing the task's requirements. 

Note that negative sums (targets) are specifically excluded as per the task's description/requirements. 

It   ''is''   worth looking for multiple targets (and indeed, that is a requirement of this task) with the same --- or for that matter --- any number of solutions, this is   a   purpose of this task. 

The use of the digit string of '''987654321''' isn't allowed, only the digit string of '''123456789'''.

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 19:07, 11 July 2017 (UTC)
