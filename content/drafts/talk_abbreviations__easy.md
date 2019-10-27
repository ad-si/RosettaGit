+++
title = "Talk:Abbreviations, easy"
description = ""
date = 2018-03-23T06:16:36Z
aliases = []
[extra]
id = 21603
[taxonomies]
categories = []
tags = []
+++

== Need some clarifications ==

Ok, I'm not going to waste my time coding a new Perl 6 example until the specs are clarified.

    * a command is followed by an optional number, which indicates the minimum abbreviation

So... we need to account for optional numbers? It is part of the spec. Why aren't there any tests for it? What if we had the following command table?

   Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy
   COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
   NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
   Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO
   MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT
   READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT
   RIght 4 LEft SAVE  SET SHift SI  SORT SOS STAck STATus TOP TRAnsfer 6 Type Up

(Note: the optional 4 and 6 after RIght and TRAnsfer.) Should the string  "riG rePEAT copies put mo rest types fup. 6 poweRin" return  "*error* REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT"?

It doesn't now when testing the reference REXX implementation. Is ''that'' wrong or is ''the spec''? If we ''aren't'' supposed to account for 'optional' numbers, why are they '''in''' the spec?

-----

: The specification that had the phrase   <big> "optional number" </big>   was a carry-over from a   ''cut-n-paste''   from the   ''abbreviations, simple''   Rosetta Code (draft) task that was entered previous to this task.   That phrase was removed as soon as I woke up the next afternoon.   In any case, the above point isn't moot any longer.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:54, 19 September 2017 (UTC)

:: !!???? And yet that bullet point shown above is ''still'' in the spec, '''right now'''. How is it not moot? Even after I pointed it out and questioned it? Do you know what the definition of moot is?  --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 10:18, 19 September 2017 (UTC)

::: I had deleted another phrase and I never got around to deleting the other.   I got interrupted with something critical at home and I just got back to the computer very late this morning.   The erroneous phrase has been elided.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 10:29, 19 September 2017 (UTC)

-----

    * A blank input   (or a null input)   should return a null string.

What is meant by a null string? It has different meanings in different languages and could have several meanings depending on interpretation. 

: Use whatever "null string" means in the computer language that is being used.   I didn't mean to say that a null string is a string with a null in it.   I always thought it generally meant a string of zero length.   However, if you want, I could replace that phrase with:     (or an empty string)     ----- in all three Rosetta Code related      '''ABBREVIATIONS, xxx '''     tasks.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:49, 19 September 2017 (UTC)



Consider this contrived example; all are possible valid interpretations of "return a null string" in Perl 6:

    What pet name did your uncle Harold use for his car when he was a teenager?
    
    'a null string' # The string 'a null string'; Uncle Harold was a computer programmer and did weird things for illustrative purposes.
    <nowiki>''</nowiki>              # An empty string; Uncle Harold was boring and unimaginative and didn't have pet names for anything.
    Nil             # The absence of a value (It ''could'' have one, but doesn't); Uncle Harold lived in the city, never got his licence and didn't even own a car let alone name it.
    Mu              # The Most undefined object. There is no there there; I don't even have an Uncle Harold.

: Asking a real person a question that usually requires a phrase when the answer doesn't/can't answer the question succinctly;   asking a computer program (possibly an AI program) to do the same would probably require a different syntax for the answer.   No human would answer with a "null string", they would say something that would be meaningful to the questioner.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:54, 19 September 2017 (UTC) 

:: You've never dealt with actual users, have you. (Or at least me :-) )  --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 10:18, 19 September 2017 (UTC) 

Maybe it should be a null byte?  \z, ^@, \0 or \x00? C programmers will be familiar with those. 
: To me, a null byte ('00'x) is not a null string, a null string is a string which has a length of zero (bytes or characters).   But use whatever you feel comfortable with in the computer language that you're using, or whatever is idiomatic for the computer language being used.   It doesn't really matter that much, as it is supposed to be in response to a blank line (that is being read), and a quiet response is supposed to be indicated.   --- And I didn't want to go down that rabbit hole explaining what a quiet response was (or is), or how to indicate some kind of response (or non-response) to a non-question (er, ...   a line that doesn't contain anything (nothing) that is in/or a line in the file being read).   I suppose I could've said just to ignore blank lines, except to return a null string, but that would just get into a semantic cyclic loop.   Er, ...   wait ... aren't most loops cyclic?     But use whatever definition works for you, you could even just ignore the blank line, nobody will flag it as incorrect. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:54, 19 September 2017 (UTC)

Or should it just be an empty string? And if it should, '''why not just state that?'''
: Because some people think that a blank string   ''is''   an empty string (I know I could think that way, depending upon the context).     Still others will argue, what is an empty string?   Would it be the same as a null string, a null character, a string with a blank in it, ...     But, if you want to return an empty string (with either definition, they both work for me).   That is why the phrase "a null string" was used.   I was trying to use a phrase that most people would understand.   Even if they didn't, I would not bemoan them for using whatever they thought it meant (null string, a string with blanks in it, empty string, null character, whatever was idiomatic for the computer language(s) being used.   I thought it would be clear that if a blank line was read, a blank (or equivalent) would be the answer (that is, the result from the computer program --- blank line in, blank line out ... a one-off cognitive version of GIGO).   For the system that I'm on, a blank record has a length of zero, even though I thought of that line as having a blank in it.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 02:54, 19 September 2017 (UTC)

:: Yes, but the spec doesn't specify a '''blank''' string for output. The question isn't about '''blank''' strings. I never even mentioned a '''blank''' string. The spec say return a '''null''' string. The term '''null''' can be interpreted in many different ways.... Never mind. I'm not going to repeat my whole explanation and inquiry. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 10:18, 19 September 2017 (UTC)

== Incorrect example output? ==

Why does <code>put</code> return <code>PUT</code> instead of <code>*error*</code>? One of the stipulations is "if there isn't any lowercase letters in the word in the command table,   then there isn't an abbreviation permitted." <code>PUT</code> and <code>PUTD</code> are the only commands that begin with <code>put</code> but they are both all-caps, so an abbreviation should not be permitted.

: Your statement saying " ... ''so an abbreviation should not be permitted''"   is correct.   However,   ''put''   is <u>not</u> an abbreviation, it is a match, as specified by the bullet of the specifications (rules):
::* compares equal (regardless of case) to the leading characters of the word in the command table
: ... along with the other specifications.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:14, 23 March 2018 (UTC)
