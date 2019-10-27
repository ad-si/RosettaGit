+++
title = "Talk:Word wrap"
description = ""
date = 2018-08-17T03:25:32Z
aliases = []
[extra]
id = 11603
[taxonomies]
categories = []
tags = []
+++

== Run BASIC ==

Hmm.  I definitely think the task requirements should be changed, just not sure how yet.  Not sure I want to disallow external programs, but a solution doesn't meet my intent if the wrapped text is never returned to the main program. &mdash;[[User:Sonia|Sonia]] 20:56, 28 March 2012 (UTC)

After sleeping on it, maybe it's okay.  I did say simple, and the solution does after all, show the wrapped text pasted back into RC as output. &mdash;[[User:Sonia|Sonia]] 00:33, 30 March 2012 (UTC)

== more options ==

I think it would be nice to add some options:

: --justification-- (aligning the left AND right margins.
: --left ragged edge-- as if the text is meant to be read from right to left (also called right justification). 
: --sentences-- add extra blanks for end-of-sentences.
: --centering-- centered justification. 
: --margins-- support the use of margins.
: --indentation-- also, support negative indentations.
: --paragraphs-- whenever a blank line, or (say), when the ¶ (paragraph) symbol is detected.
: --columnar output-- support multiple (newspaper) columns (with/without a separator border). -- [[User:Gerard Schildberger|Gerard Schildberger]] 03:52, 31 March 2012 (UTC)

:: Thanks for the ideas!  Some of these I had thought of, but I wanted to leave the basic task as simple as possible, letting people either code a very simple algorithm or show an equivalent even simpler method.  Your ideas might make interesting extra credit or even separate tasks.  Of course, It's a wiki...you can make any changes you feel strongly about; I just liked the least squares metric described in the WP article and though an alternative algorithm would make interesting extra credit and that adding that would be plenty for a single task. &mdash;[[User:Sonia|Sonia]] 17:29, 1 April 2012 (UTC)

== There are two tasks relating to word wrapping==
Some languages provide an inbuilt facility for word wrap within the width of the screen (whereas wrap to a particular column width involves a bulkier overhead). We also need a wordwrap task to demonstrate the simpler scenario of wrapping to screen width. This would give us two tasks: Wordwrap/Screen Width and Wordwrap/Custom Width.

[[User:Markhobley|Markhobley]] 19:25, 5 February 2013 (UTC)

==handling long words==

This subject came up in a REXX newsgroup some time ago (regarding the formatting of text).

What does a word-wrapper program do when encountering a word longer than the working margins?

Several choices:
::* truncate the word
::* truncate the word (with footnote or some such indicator)
::* show as-is
::* hyphenate the word
::* other  

My REXX program (version 1) doesn't truncate long words, but instead, show the word in its entirety (with possible wrapping), thereby preserving the text content.   Other programs are not so kind (some even loop forever), but I suppose this situation is beyond the intent of the task. 

By the way, REXX has the feature that if you display a line wider than the terminal (or window), it will break up the line and show the full text. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:05, 20 August 2013 (UTC)

:Hi Gerard, there is also the option of using a smaller font that you get in some spreadsheets, but what I'd go for if I was reading an ebook for example, would be intelligent hyphenation - for example by not splitting mid-syllab-
le. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 04:40, 21 August 2013 (UTC)

:: Wouldn't proper (correct) hyphenation be way beyond the scope of this task (and wiki)? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 06:42, 21 August 2013 (UTC)
::: Oh yes. I just took the question as asking "in a perfect world ...". --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:24, 21 August 2013 (UTC)

== REXX timings ==

I created a file containing one line of about 1000000 characters containing words of 1 to 90 characters, randomly distributed such as

```txt

'A nnnnnnnnnnnnnn ooooooooooooooo nnnnnnnnnnnnnn cccc...'

```

Timing of the 3 versions show on Windows XP using ooRexx:

```txt

    width 10  72  1000
version 0 29  27    19 seconds 
version 1 30  28    19 seconds 
version 2 16  10     3 seconds
PL/I           1 second

```

versions 0 and 1 adapted as usual: @->a, $->d, = -> =""

<strike>
version 0 has a minor flaw: The output has a leading blank. Otherwise outputs are identical. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:55, 21 August 2013 (UTC)
</strike>
::::: Fixed the (extra) leading blank. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:01, 21 August 2013 (UTC)

: Since you didn't post a version of the program (version 2) that actually reads a file, I suspect that a factor is reading the (one million bytes) text file.   Also, console (terminal) I/O (at least on Windows/XP systems and such [using Regina]) is very unkind to timings (elapsed time), especially when causing the output to scroll.   The REXX version 2 doesn't write it's output to the terminal.   It's hard to compare apples to oranges when one program writes to the terminal, another writes to a file.   I frequently time REXX programs, and timing large amounts of data being written to the screen (even as an artifact) really effects the elapsed time (which is, I suspect, what you are measuring, not CPU time).   When displaying a million bytes of characters to a DOS window uses a fair amount of wall clock time, and the same can be said for reading a file that large.   Also, please note, this is the (Classic) REXX section, not ooRexx.   Also note that the task asks to wrap a paragraph of text, not a book.   The input file (LAWS.TXT) exceeded that by a bit, but using a million bytes of text stresses the REXXes variable accessing mechanism quite a bit, and what is being measured (besides the reading and displaying) is the accessing of the text, in this case, the WORD BIF.   If speed is what is wanted, a stemmed array could've been used instead of a flat representation (one REXX variable), but that would obfuscate somewhat the REXX program during the reading of the file.   The idea was to show how to re-format a paragraph, and for that amount of text, it wasn't worth the added complexity to make the REXX program faster.   One million bytes of text was a design consideration. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 15:49, 21 August 2013 (UTC)

:: Sorry, I forgot to mention that I adjusted all 3 versions so that they read the file (1 line) and create an output file (lineout instead of say). I could never display a million bytes on the screen in 10, 20 or what seconds :-) 200 paragraphs having 5000 characters each would be the same load. And I wanted to compare the algorithms' speed in some measurable way. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 18:02, 21 August 2013 (UTC)

::: ooRexx is the only (somewhat classic) Rexx I have! Can you please measure the timings with Regina? Here are the programs that should work on Regina as well as on ooRexx:
<lang>
/* REXX */
oid='long.txt'; 'erase' oid
s='A'
l=0
c='abcdefghijklmnopqrstuvwxyz'
c=c||translate(c)'1234567890.,-;:_!"§$%&/()=?`'||c
Say length(c)
do i=1 To 100
  c.i=substr(c,i,1)
  End
cnt.=0
Do Until l>=1000000
  r=random(1,90)
  s=s copies(c.r,r)
  l=l+r+1
  cnt.r=cnt.r+1
End
Say l
Call lineout oid,s
do r=1 To 90
  Say right(r,3) right(cnt.r,5)
  End

```


```rexx
/*REXX pgm ww0 reads a file and displays it (with word wrap to the screen). */
Call time 'R'
parse arg iFID width                   /*get optional arguments from CL.*/
oid=fn(ifid)'0.'width
a=''                                    /*nullify the text  (so far).    */
     do j=0  while lines(iFID)\==0     /*read from the file until E-O-F.*/
     a=a linein(iFID)                  /*append the file's text to  a   */
     end   /*j*/
d=''
     do k=1  for words(a); x=word(a,k) /*parse until text (a) exhausted.*/
     _=d x                             /*append it to the money and see.*/
     if length(_)>width  then do       /*words exceeded the width?      */
                              Call o d    /*display what we got so far.    */
                              _=x      /*overflow for the next line.    */
                              end
     d=_                               /*append this word to the output.*/
     end   /*k*/
if d\==''  then Call o d                  /*handle any residual words.     */
                                       /*stick a fork in it, we're done.*/
Say time('E')
Call lineout ifid
Exit
o: Return lineout(oid,arg(1))
```

   

```rexx
/*REXX pgm ww1 reads a file and displays it (with word wrap to the screen). */
Call time 'R'
parse arg iFID width justify _ .                 /*get optional CL args.*/
if iFID=''  |iFID==','   then iFID ='LAWS.TXT'   /*default input file ID*/
if width==''|width==','  then width=linesize()   /*Default? Use linesize*/
if width==0              then width=80           /*indeterminable width.*/
if right(width,1)=='%'   then do                 /*handle  %  of width. */
                              width=translate(width,,'%') /*remove the %*/
                              width=linesize() * translate(width,,"%")%100
                              end
if justify==''|justify==','  then justify='Left' /*Default?   Use  LEFT */
just=left(justify,1)                   /*only use first char of JUSTIFY.*/
just=translate(just)                   /*be able to handle mixed case.  */
if pos(just,'BCLR')==0   then call err "JUSTIFY (3rd arg) is illegal:"  justify
if _\==''                then call err "too many arguments specified."  _
if \datatype(width,'W')  then call err "WIDTH (2nd arg) isn't an integer:" width
oid=fn(ifid)'1.'width
a=''                                     /*nullify the text  (so far).    */
      do j=0  while lines(iFID)\==0    /*read from the file until E-O-F.*/
      a=a linein(iFID)                 /*append the file's text to  a   */
      end   /*j*/

if j==0   then call err  'file'  iFID  "not found."
if a=''   then call err  'file'  iFID  "is empty."
d=''
    do k=1  for words(a);  x=word(a,k) /*parse until text (a) exhausted.*/
    _=d x                              /*append it to the money and see.*/
    if length(_)>width  then call tell /*word(s) exceeded the width?    */
    d=_                                /*the new words are OK so far.   */
    end   /*k*/

call tell                              /*handle any residual words.     */
Say 1 time('E')
Call lineout ifid
exit                                   /*stick a fork in it, we're done.*/
/*----------------------------------ERR subroutine----------------------*/
err:  say;   say '***error!***';   say;   say arg(1);  say;  say;  exit 13
/*----------------------------------TELL subroutine---------------------*/
tell: if d==''  then return            /*first word may be too long.    */
w=max(width,length(d))                 /*don't truncate very long words.*/
               select
               when just=='B'  then d=justify(d,w)      /*?----both----?*/
               when just=='C'  then d= center(d,w)      /*  ?centered?  */
               when just=='L'  then d=  strip(d)        /*left ?--------*/
               when just=='R'  then d=  right(d,w)      /*------? right */
               end   /*select*/
Call o d                               /*show and tell, or write--?file?*/
_=x                                    /*handle any word overflow.      */
return                                 /*go back and keep truckin'.     */

o: Return lineout(oid,arg(1))
```



```rexx

/* REXX ww2 ***************************************************************
* 20.08.2013 Walter Pachl  "my way"
**********************************************************************/
Call time 'R'
Parse Arg iFid w
oid=fn(ifid)'2.'w
s=linein(ifid)
say length(s)
Call ow s
Say time('E')
Call lineout ifid
Exit
ow:
  Parse Arg s
  s=s' '
  Do While length(s)>w
    Do i=w+1 to 1 by -1
      If substr(s,i,1)='' Then Leave
      End
    If i=0 Then
      p=pos(' ',s)
    Else
      p=i
    Call o left(s,p)
    s=substr(s,p+1)
    End
  If s>'' Then
    Call o s
  Return
o:Return lineout(oid,arg(1))

```


:: Translated version 2 to PL/I. Since PL/I has a limit of 32767 for character strings I had to cut the input into junks of 20000 bytes and add extra reads. Output is identical to REXX. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:38, 21 August 2013 (UTC)

The last shown REXX program has a problem with classic REXX: '''fn''' is an unknown function.   Also, that REXX program only reads the first record of the file (does exactly one read) instead of doing a loop until done.   It would make more sense to exclude the time to read the file as well as bypassing the writing of the records to the file, as the I/O would be unvarying and slightly dependent on other I/O activity in the system, not to mention caching.    Whoever does the first reading pays for all the I/O, the 2nd reading would be from cache.   I would benchmark for a paragraph of text as the task says, not a million bytes.   Scale up the number of executions to make the timings meaningful.   Also, I took the liberty of breaking up the listing of the REXX programs into separate sections, perhaps it would be a good idea to label/identify them, not to mention to bring version 0 and 1 up to date. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:01, 21 August 2013 (UTC)

-----

I seemed to found a discrepancy.   For an input of:

```txt

     ────────── Computer programming laws ──────────
 The Primal Scenario  -or-  Basic Datum of Experience:
    ∙ Systems in general work poorly or not at all.
    ∙ Nothing complicated works.
    ∙ Complicated systems seldom exceed 5% efficiency.
    ∙ There is always a fly in the ointment.

```

The REXX versions 0 and 1 produce:

```txt

────────── Computer programming laws
────────── The Primal Scenario -or-
Basic Datum of Experience: ∙ Systems in
general work poorly or not at all. ∙
Nothing complicated works. ∙ Complicated
systems seldom exceed 5% efficiency. ∙
There is always a fly in the ointment.

```

The REXX version 2 (modified for my timings) produces:

```txt

      ────────── Computer programming
laws ──────────  The Primal Scenario
-or-  Basic Datum of Experience:     ∙
Systems in general work poorly or not at
all.     ∙ Nothing complicated works.
 ∙ Complicated systems seldom exceed 5%
efficiency.     ∙ There is always a fly
in the ointment.

```

It seems that the REXX version 2 isn't handling leading or imbedded blanks. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:40, 21 August 2013 (UTC)

:: correct. pls try to live without that "feature". for testing, pls replace fn(fid) with "long"--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 21:56, 21 August 2013 (UTC)

::: I don't understand.   That isn't a "feature" (failure by design?), that is a bug.   The output (the word wrapping) isn't what I expect, although it might be the design goal of the coder of the REXX version 2 program to not ignore those blanks. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:06, 21 August 2013 (UTC)

:: version 0 and 1 remove them and reduce multiple blanks to one blank. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 21:59, 21 August 2013 (UTC)

::: What about version zero ???   REXX version 0 and 1 already removes leading and multiple embedded blanks (as well as trailing blanks). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:06, 21 August 2013 (UTC)

:::: that's what I tried to say. the '1' got lost.--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 22:15, 21 August 2013 (UTC)

adding s=space(s) to ww2 should fix that!?! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 22:49, 21 August 2013 (UTC)

: Yes, it should.   The proof is in the tasting of the pudding. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:52, 21 August 2013 (UTC)

-----

This just performed with:
::* the   newer   REXX version 1   using a ''stemmed array'' instead of a ''char string''  
::* the updated REXX version 2   using   '''s=space(s)'''
::* using an appropriate values of repetitions to elongate the elapsed time
::* using modified programs to suppress the writing/display of the output
::* bypassing the timing of the reading of the input file
::* both REXX programs producing the exact same output
::* using many trials and variations (under Windows/XP)
::* using the REXX Regina 3.7 interpreter  
::* using my coal-fired steam-driven Frankenbox   (built last century)
the timings are:
:::::* REXX version 1     2.49 seconds
:::::* REXX version 2     2.45 seconds
:::::* REXX version 2     2.29 seconds   (optimized with exact comparisons)
:::::* REXX version 2     1.27 seconds   (optimized with   '''lastpos'''   BIF)
:::::* REXX version 2     1.06 seconds   (optimized with   '''parse'''   statement)
:::::* REXX version 2     1.05 seconds   (optimized by making the '''ow''' subroutine non-destructive)
:::::* REXX version 2     1.01 seconds   (optimized by making the '''ow''' subroutine in-line)
:::::* REXX version 2     0.96 seconds   (optimized the inner DO loop, eliminated an   '''if'''   statement)
The   '''lastpos'''   BIF was used to find the last blank (within a field of '''W''' characters instead of searching for the last blank character by character). 

Further optimization was done using   '''parse'''   instead of   '''substr'''   and other such thingys. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 23:39, 21 August 2013 (UTC)


I really have to stop optimizing that REXX program, I'm running out of coal. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 00:11, 22 August 2013 (UTC)

Well, I ran out of coal ... can't stoke the fires anymore. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:31, 22 August 2013 (UTC)

:: I refrained from using lastpos since the (classic?) Rexx on the host does not have it. Is version 2 that you refer to "my way" modified as noted above? Are your final versions 1 & 2 available somewhere? I had to look up vestigual (limited English) - it should have been vestigial:-) --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 06:35, 22 August 2013 (UTC)

::: Yes, the REXX version 2 (as mentioned above) is a cumulative modification of the original, that is, the 3rd optimization is the changed 2nd optimization. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:19, 22 August 2013 (UTC)

::: (about the misspelling of ''vestigial''):   I did a <strike> quite </strike> quick web check and found many hits on ''vestigual'', but I saw the answers to a question and thought that was the correct spelling.   At least, I'm not alone in misspelling that word:   Vestigial Vsetigial Vesitgial Veetigial Veatigial Vedtigial Vewtigial Vextigial Vesrigial Vesgigial Vesyigial Vestogial Vestugial Vestkgial Vestirial Vestinial Vestitial Vestihial Vestibial Vestifial Vestigoal Vestigual Vestigkal Vestigisl Vestigizl Vestigiql Vestigiap Vestigiam Vestigiak --- that word must hold some kind of record in the number of ways to misspell a word.   But I got almost all of the letters right. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:19, 22 August 2013 (UTC)

::::: There is no need to strike misspellings, just correct them.   The reason I did a strike-out for the "quite" misspelling is that the misspelling was discussed later, so I just couldn't correct it without making the comment invalid. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:22, 23 August 2013 (UTC) 

:: In my IBM time I learned that American colleagues are less spelling-conscious than we Europeans (or Austrians). It's a matter of emphasis on spelling in school. Did you do quite a web check or a quiet web check --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:36, 23 August 2013 (UTC)

::: Rosetta Code isn't the place to publish such observations.   I know a snub when I hear (or read) one.   Best to just remove such comments, even from a discussion page.   Even if it were true, its still not an unbiased opinion or possible not even a valid observation (too limited and narrow), and it might appear that it could be based on a limited sampling group (and by one person at that).   Not everybody has a spell-checker available.   Without spell-checkers, typos are more common.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:24, 17 August 2018 (UTC)
 
::: Are you sure about the   '''lastpos'''   BIF not being available in (your) host's version of REXX?    It's been around in REXX at least since 1984 (according to a VM System Product Interpreter Reference Summary), long before it was ported to MVS (or whatever it's being called now).   Which host (and release) are you using?   I didn't post any of REXX version 2 programs since you signed your name to it, and I didn't want to publish various versions of it, as it would appear that you were the author, and it didn't seem worth all the bother to include disclaimers and whatnot, and I had so many versions.   I was just fooling around and was squeezing blood from a turnip trying to get more performance out of the program.   I probably could get more performance out of it, but I got tired shoveling all that coal, and I had to add more code to handle a special case of long words. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:28, 22 August 2013 (UTC)

:::: I'd suggest to leave the header intact and add change lines such as * yyyyddmm GS this and that. But I really don't care. I put my names into my programs because I like to be known. Your programs are easily recognizable by @ and $ :-) AND your unique indentation rules! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:36, 23 August 2013 (UTC)

::::: I guess some people like to be known.   However, Rosetta Code has a policy against vanity badges and strongly discouraged, and most have been removed.     People can look at the ''history''' file and see who performed the entering of the computer program and/or the changes.   I have learned later (after I did the tuning and timings) that timings are also discouraged, especially between languages.   This whole discussion on the REXX timings should probably be deleted.   



Here is the latest revision   (with not much commenting, but better than nothing):

```rexx
/*rexx*/     parse arg ifid w            /*get required options from CL */
/*{timer}*/  parse arg ifid w times .    /*a good try is  10k ──► 100k. */
/*{timer}*/  if times==''  then times=1  /*use a default if omitted.    */
s=''
                do  while lines(ifid)\==0
                s=s linein(ifid)
                end   /*DO while*/
s=space(s)                               /*remove superfluous blanks.   */
say 'length of input string:' length(s)  /*display the length of input. */
say
call time 'Reset'                        /*reset the REXX elapsed timer.*/
/*{timer}*/     do jj=1  for times       /*the repetitions thingy.      */
                x=s' '                   /*var  X  is destroyed (below).*/

                               do  while x\==''     /*1 chunk at a time.*/
                               i=lastpos(' ',x,w+1) /*look for blank <W.*/
                               if i==0  then do     /*...no blank found.*/
                                             call o left(x,w)
                                             parse var x =(w) x
                                             end
                                        else do     /*... a blank found.*/
                                             call o left(x,i)
                                             parse var x =(i) +1 x
                                             end
                               end   /*DO while*/
/*{timer}*/     end   /*jj*/
say
say  format(time('Elapsed'),,2)  "seconds  for"  times  'times.'
call lineout ifid
exit

/*{timer}*/  o: if jj==times  then say arg(1);  return  /*show last text*/
             o:                    say arg(1);  return
```

Here is the input file:

```txt

     ────────── Computer programming laws ──────────
 The Primal Scenario  -or-  Basic Datum of Experience:
    ∙ Systems in general work poorly or not at all.
    ∙ Nothing complicated works.
    ∙ Complicated systems seldom exceed 5% efficiency.
    ∙ There is always a fly in the ointment.

```

-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:28, 22 August 2013 (UTC)

: lastpos: no I'm not sure and I have alas no longer a host (pun intended). I wonder where I missed it. Thanks for massaging my program. I shall study it later and test my 1MB file. Your input, by the way, is not exactly a "paragraph", is it? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:44, 22 August 2013 (UTC)

:: As mentioned earlier, it was bigger than a paragraph; I hated to cut it down (as the file above).   I was using a 100x200 character wide console window and I needed something with some heft to it.   Plus, with almost all of us (readers of Rosetta Code) being computer programmers of one sort or another, I thought a by-product would be some people perusing the text and reflecting on the wisdom of the laws ... if not only in a Murphy's Law sort of way. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:58, 22 August 2013 (UTC)

My results from testing your program:

```txt

with i=lastpos(' ',x,w+1) /*look for blank <W.*/
  rexx gs text.txt 72 1000000 -> 7.09 seconds  for 1000000 times.

with Do i=w+1 to 1 by -1
       If substr(x,i,1)=' ' Then Leave
     End
  rexx gs2 text.txt 72 1000000 -> 10.88 seconds  for 1000000 times.

```


: I got a 45% improvement (using Regina REXX), you got a 35% improvement (using ooRexx) --- Are my assumptions correct?   How many engines does your laptop have?   How much memory?   What other processes are running?   When I run benchmarks, the computer is running pretty much naked (as possible).   No matter what the improvement (35% or 45%), that's nothing to sneeze at. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:29, 22 August 2013 (UTC)

:: Nobody sneezes. Can't answer your questions. Will use lastpos from now on. thanks. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:36, 23 August 2013 (UTC)

Unfortunately I cannot verify a similar performance difference with my 1MB file. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:58, 22 August 2013 (UTC)

With o: Return (to avoid output to screen)

```txt

rexx gs  long.txt 72 -> 2.31 seconds for 1 times
rexx gs2 long.txt 72 -> 2.36 seconds for 1 times

```

--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 20:11, 22 August 2013 (UTC)

: With a one megabyte file, you may be measuring the effects of paging in your laptop (as for elapsed time) as well as competition/interference with other processes.   That was one reason why I used a multiplier for the '''do''' loop instead of increasing the amount of text read.   The drawback is that (the multiplier) increases the locality of reference, and I don't know enough about the Microsoft Windows paging sub-system to know how much of an effect that is. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 20:36, 22 August 2013 (UTC)

:: Let's leave it at that. I shall be using lastpos in the future. thanks. Nevertheless version 2 seems to be undoubtedly better than !?! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:40, 23 August 2013 (UTC)

::: I wouldn't agree that your version is   ''undoubtedly''   better.   I do have a few doubts.   Version one version doesn't erase existing files, it also has more options (the ''kind'' of text justifications, giving the user a choice), it has as lot more documentation (comments) to explain what is happening and why, has error checking and error messages to handle bad command line options, checks for file-not-found and file-is-empty conditions, etc.   I assume you must be using a different or unknown metric(s) for ''undoubtedly better".   Rosetta Code is not the place to crow about one's version being better than another, '''unless''' you wrote both versions and you're pointing out the value   (however one judges ''value'')   of one program entry versus another.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 03:24, 17 August 2018 (UTC)
