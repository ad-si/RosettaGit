+++
title = "Talk:Align columns"
description = ""
date = 2010-11-14T13:54:11Z
aliases = []
[extra]
id = 3193
[taxonomies]
categories = []
tags = []
+++

== J solution ==

I haven't read the J solution in depth yet, but it appears more complex than may be necessary.  For example, with <code>'LEFT CENTER RIGHT'=:i.3</code> we can solve the task thus:

    _2 {:\ ": <;._2@:,~&>/'$';LF;text [ 9!:17 ,~ CENTER [ 9!:7 ' '$~11
    Given        a         text     file    of     many      lines,    where   fields  within    a    line  
     are     delineated     by       a    single 'dollar'  character,  write      a    program              
     that      aligns      each    column   of    fields       by     ensuring  that    words    in   each  
    column      are     separated    by     at     least      one      space.                               
   Further,    allow       for      each   word     in         a       column    to      be    either left  
  justified,   right    justified,   or   center justified   within     its    column.                      
                                                                                                            
Replacing <code>CENTER</code> with <code>LEFT</code> or <code>RIGHT</code> to taste. 

Now, of course, the justification selected applies to every word.  If we want to be able to treat each word (or column) independently, there are other methods.  Even for this more general specification, I'm still not convinced that the large explicit verb is required, but I'll have to review it in detail to confirm.

: I concur that your solution is adequate to the specified task. It should replace the one I posted. I wrote mine without knowing about 9!:y. Thank you for this more skillful solution, Dan. --[[User:TBH|TBH]] 22:58, 1 December 2008 (UTC)

: I've just noticed that the code proposed above does not compensate for the inconsistent input data, which lacks the delimiter at the end of some lines. (It does not seem right to me to have the example text have such an irregularity when nothing about the task suggests that data-quality testing is relevant, but, that's what we've been given.) --[[User:TBH|TBH]] 00:28, 2 December 2008 (UTC)

:: I have added a note to clarify that this aspect of the data is intentional. --[[User:Paddy3118|Paddy3118]] 03:52, 2 December 2008 (UTC)

:::  Fixed. --[[User:DanBron|DanBron]] 12:38, 2 December 2008 (UTC)

: The new solution has the column count hardcoded into it. I suspect that the task is intended to work such that the tally of columns is derived from the text, not provided by the user as part of the input. --[[User:TBH|TBH]] 22:34, 2 December 2008 (UTC)

:: Where is the column count hardcoded?  AFAIK, the number of columns wholly determined by the (maximum) number of <code>$</code>s in a line.  To be more explicit, the numeric constants are, from left to right: 
::{| class="wikitable" style="text-align: top,left; font-size: 85%; width: auto; table-layout: fixed;"
|-
| Constant
| Description
|-
| <code>'''_2''' {:\ ":</code>
| Non-overlapping (<code>'''-'''</code>) pairs (<code>'''2'''</code>) of rows of the formatted data.
|-
| <code><;.'''_2'''</code>
| Split the input, treating the last (<code>'''2'''</code>) item as the fret, and exclude (<code>'''-'''</code>) the frets.
|-
| <code>'''9'''!:'''17'''</code>
| Foreign controlling parameters (<code>'''9'''</code>), specifically controlling alignment (<code>'''17'''</code>).
|-
| <code>,~ '''CENTER'''</code>
| Parameter specifying centered alignment. (A prior version had <code>'''CENTER''' #~'''2'''</code>, which is the same, but one word longer)
|-
| <code>'''9'''!:'''7'''</code>
| Foreign controlling parameters (<code>'''9'''</code>), specifically controlling linedrawing characters (<code>'''7'''</code>).
|-
| <code>' '$~'''11'''</code>
| Parameter specifying linedrawing characters (all <code>'''11'''</code> linedrawing characters will be <code>' '</code>).
|}
::--[[User:DanBron|DanBron]] 23:32, 2 December 2008 (UTC)

::: Ouch. OK, so my testing was really inadequate, and the conclusion I'd jumped to was flat out wrong. Yes, I think that does indicate that said verb is hard to read, but I wish I hadn't piped up with my concern until I'd done enough reading to correct this misconception on my own. --[[User:TBH|TBH]] 01:53, 4 December 2008 (UTC)


###  poll: short or general? 


Given that:
#  The <code>9!:17</code> version confused even a veteran J user, and
#  using these foreigns has side effects which would need to be reverted after the desired effect is acheived, further cluttering the solution, and
#  using foreigns that control global formatting to acheive local formatting might be considered cute at best, and
#  the task calls for "''allow[ing] each word in a column to be [aligned]''", which could imply the need to align words or columns independently, then

should we scrap our short (if cute) solution, and replace it with a longer, but more general solution?  As a (possibly desirable) side-effect, a solution which eschewed the foreigns would be more comparable to others written in languages which lack a built-in formatting & alignment facility.

We could use the original solution:
 colalign=: verb define
  'L' colalign y                   NB. default to left-justify all columns
 :
  D=. '$'                          NB. the column delimiter
  afD=. ,&.> (#&D&.> @: (D&~: @: {: &.>))   NB. assure final delimiter (verb)
  T0=. afD <;._2 y                 NB. text arranged as lines
  T1=. > <;._2&.> T0               NB. as word grid
  Alignment=. x, (({:$T1)-#x) # {:x
  if. 'C' e. Alignment do. 
   Colwidths=. >./"2 Sizes=. > #&.>"0 T1
   CPad=. ('C'=Alignment)*"1 <.2%~ Sizes -~"1 Colwidths
   T2=. T1 ,~&.>"0 CPad #&.>"0 ' ' NB. as center-justify-padded grid
   else. T2=. T1 end.
  if. doesR=. 'R' e. Alignment do.
   whereR=.'R'=Alignment
   T3=. (($T2)$whereR)}(,: |.&.>) T2 NB. as right-justify-prepared grid
   else. T3=. T2 end.
  T4=. >@{."1&.> 1 <@|:;.1 |: T3   NB. as columns
  if. doesR do.
   T5=. whereR }(,: |."1&.>) T4    NB. as rt-jstfy-completed columns
   else. T5=. T4 end.
  SP=. <|:,:' '#~#T0  	NB. space-column to interleave with text columns
  >,"1&.>/ }. ,|: SP,,:T5
 )

Example use:
  colalign text         NB.  Default is left alignment.
  'C'  colalign text    NB.  Can specify a different alignment.

Or another candidate, such as:
    colAlign     =:  'L'&$: : (format@:align parse)
      format     =:  [: > (,. ' '&,.)&.>/
      align      =:  cols |."1&.>~ (<nowiki>''</nowiki>;1) <;.1 rotation
        cols     =:  <@:>"1@:|:@:] 
        rotation =:  (<.@*"1 2: %~ 'LCR'&i.)~ offset
          offset =:  (-"1 >./)@:(#&>)
      parse      =:  [: (-@:(-:{:) }. <;._2@:,~)&>/ '$'; LF ; ]

Example of use:
    colAlign text                                NB.  Default is left alignment.
    'C' colAlign text                            NB.  Can specify a different alignment for all columns,
    'RCLRCLRCLRCL' colAlign text                 NB.  or for each column independently,
    (6 12 (] {~ (?.@$ #)) 'LCR') colAlign text   NB.  or for each word independently.

Of course, we could include two solutions:  lead with the short one, finish with the general one.  But that might be overkill. 
--[[User:DanBron|DanBron]] 02:39, 3 December 2008 (UTC)

: It was only ever my intention to have all columns aligned the same way. I have added (another), note to that effect. --[[User:Paddy3118|Paddy3118]] 05:11, 3 December 2008 (UTC)

: I very much like the new code you posted above, and am of the opinion that it should be posted as the sole J solution. Your one-liner is interesting, but what seems to me a better way to handle it would be to link to a Jwiki page where you post it. The version I wrote is best set aside. The desireable features of your new version are (1) emphasis on functional composition, (2) self-documenting qualities of component verbs, and (3) focus on standard programming techniques with constraint to the core primaries. --[[User:TBH|TBH]] 01:48, 4 December 2008 (UTC)

:: Whoops, sorry I didn't see this discussion before I made my changes to the current J solution. I don't see a problem with using the foreign conjunctions. If your language has a facility that makes a job easier then why not use it! Adding a comment prevents confusion for veteran and novice users alike, and I agree that the session should be returned to its prior state. I'd be happy with Dan's proposed solution above too, although it could probably be simplified some, now that the spec has been clarified.

== Output Question ==
Given that the lines contain varying numbers of fields, when a justified line is output should the program<br \>
A) only output the justified fields that the line contains, or<br \>
B) output spaces for the columns when the line does not contain all the columns?

: Hi, assume that the output should look good visually, (in a monospaced font), and that lines will never be wrapped when displayed so ''trailing'' whitespace is irrelevant. --[[User:Paddy3118|Paddy3118]] 03:39, 2 December 2008 (UTC)

==R Solution and Row, Column headers==
Can the output be printed without them? --[[User:Paddy3118|Paddy3118]] 06:16, 6 August 2009 (UTC)
:That would be nice, but if the language really can't, they can stay. —[[User:Dkf|Donal Fellows]] 08:11, 6 August 2009 (UTC)
If the row/column headers cannot be removed then maybe a comment to say that should be added to the entry, as the task is about output formatting. --[[User:Paddy3118|Paddy3118]] 15:50, 6 August 2009 (UTC)

==Show output==
Could examples either show their output or state that their output is checked and is the same as another (named) example that shows its output. Thanks. --[[User:Paddy3118|Paddy3118]] 16:45, 7 January 2010 (UTC)

==Extra characters in Prolog solution==
I was thinking flagging the Prolog solution as being incorrect due to the presence of the extra quotes in the output. If prolog has no way around this then maybe a note could be added to ''plainly'' state that Prolog cannot print the output without adding quotes and then a flag of incorrect would not be necessary? --[[User:Paddy3118|Paddy3118]] 13:54, 14 November 2010 (UTC)
