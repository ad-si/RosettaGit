+++
title = "Talk:Temperature conversion"
description = ""
date = 2016-04-09T18:34:09Z
aliases = []
[extra]
id = 15879
[taxonomies]
categories = []
tags = []
+++


### Celcius spellings


Shouldn't Celcius be Celsius ???

: It's been spelled both ways, along with other variations and alternatives (as noted in the REXX example). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:35, 13 August 2013 (UTC)

:::: I looked into REXX and see centigrade, centingrade, centesimal, Celsius, Celcius.  WHERE did you find centingrade or centesimal? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 22:01, 13 August 2013 (UTC)

::::: I don't keep track where I saw the references (I was doing some deep research on the history of old centigrade thermometers and came across the older names for it).   [''Centesimal degree'' was mentioned in the Wikipedia article '''Celsius'''.]   I just support the alternate spellings, misspelled or not.   If some number of people use the misspellings, my program accepts it in an attempt to support the (named or misspelled) temperature conversions.   This shouldn't be construed as an endorsement, nor a tacit approval of the misspelling(s). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:32, 13 August 2013 (UTC)

:::::: My sincere thanks to Grondilu who corrected the task description. I do NOT suggest that you support another misspelling you find with Google:

```txt

Celcius to Farenheit Farenheit to Celcius 
Farenheit Celsius Celsius Farenheit 
Farenheit to Celsius Celcius to Farenheit Conversion 
Celsius to Farenheit Convert Farenheit to Celcius 

```

:::::: I'd rather tell a user about apparently wrong input (as Google does: "did you mean ....?") instead of accepting it. Anyway, centigrade and Celsius are the only names for that scale that I know. Thanks for telling me about Celsius' merits. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:10, 14 August 2013 (UTC)

::::::: Google has a different purpose;   when somebody enters a word (or words) to be found, Google doesn't know (for sure) if the word is misspelled or not, as a matter of fact, it will find entries for misspelled words, and of course, things that aren't words at all.   Google's purpose is to assist in finding stuff.   The purpose of (my) temperature conversion is to convert a unit (or units) of temperature scales to another scale (actually, a set of temperature scales), and if the user enters (misspelled) celcius, I know what he meant to enter.   Google can't do that for certain, so it prompts the user and may go with the correct spelling (which is a judgement call, for it may be that the misspelled word is what the user wanted to find);   which is what my program does (goes with correct spelling).   Google, of course, allows the user to force Google to use the original word (or the correct spelled word, I suspect there is some heuristics going on), but that isn't applicable for this application (program) and would be waste of the user's time to force re-entering of the temperature scale, and without a hint, the user wouldn't necessarily know the correct spelling (unless the user knew it was a typo).   If a user specified ''celcius'', there isn't a need or reason to reject the request, and force a correction.   The conversion program isn't the grammar police, just a conversion tool.   I don't find fault with a program that rejects only but the correct spellings, I just feel it is less useful than a program that is more forgiving, and does, in effect, '''d'''o '''w'''hat '''I''' '''m'''ean (DWIM).   As I understood the task's description, it is to convert value(s) from some temperature scale(s) to another set of scales.   That it allows a user's feeble attempts at spelling is just a feature of the program.   The fact that ''celcius'' is a common misspelling, it means that many people must be using it.   Note that no misspelled words are shown in the (conversion) output except possibly for the input echo, the preferred spelling is used. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 07:40, 14 August 2013 (UTC)

::::::::: How about this then for my input of tc 10 Celsius to the modified program (Upper, as usual):

```txt

-------------------------------------------  10 Celsius
         10          Celcius
        135          Delisle
etc. 

```


::::::::: ??? I'd rather tell people about errors they make (and I KNOW by experience you are doing that as well.) For an input of 10 Fahrenheit you show -12.222 Celcius thus propagating the misspelling. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 08:24, 14 August 2013 (UTC)

:::::::::: I hadn't noticed that I had Celcius in the output, it was a typo (and one would think that I would've caught that with all these conversations). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 08:39, 14 August 2013 (UTC) 

:::::::: Should   wp:Fahrenheit|Fahrenheit be wp:Degrees Fahrenheit|Fahrenheit ? What's the point of these tags? --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:17, 14 August 2013 (UTC)

:: None of my dictionaries have celcius. I asked on my favorite English forum and got these responses:

```txt


von Walter (AT), today, 21:52  Spam?  ...

is Celcius a valid / used alternate spelling of Celsius 
 or just a misspelling in some Wikipedia articles and elsewhere?
 Thanks in advance
 Walter  



Antwort:  
 Misspelling.   #720341 
 
von Joanne (GB/AT), today, 21:57  Spam?  ...

http://www.onelook.com/?w=celcius&ls=a - the alleged entries either come up with no entry found or redirect you to celsius - apart from the wiki page for the rap album Celcius, and we all know that rappers can't spell ;-)  



Antwort:  
Wikipedia(SV): Anders_Celsius   #720343 
 
von Dracs (DE), today, 22:10  Spam?  ... 

```

:: YOY should rosettacode insist and propagate a misspelling (my view) instead of using the usual word (Celsius was the one who defined the Celsius scale) --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 20:20, 13 August 2013 (UTC)

::: No, that's not quite true.   Andres Celsius invented/created/defined   ''a''   temperature scale; it was called centigrade (and other names) for two centuries or so.   For various reasons, around 1948 the name ''centigrade'' was replaced with ''Celsius'' by some international standards.   I know I was taught to use centigrade in grade school (USA), but by the time I went to college, Celsius was in use. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:32, 13 August 2013 (UTC)

Well, if the Oxford Dictionary [http://oxforddictionaries.com/spellcheck/english/?q=celcius says it's a common misspelling] then that is good enough for me and we should correct it in the task description and urge others to use the correct spelling. (It even goes so far as to check the spelling in other languages). --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 05:20, 14 August 2013 (UTC)

: Thanks Paddy. You mean 'computer languages' :-) and you know the answer to my wp question!?! (see above) --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 05:28, 14 August 2013 (UTC)

Incorrect correction? This will never be true: if left(uU,6)=='DEGREES' --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 10:25, 14 August 2013 (UTC)

:: Yes, I changed the wrong length.   The surgery was a success, but the patient died. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:27, 14 August 2013 (UTC)

I changed several (obvious) misspellings but not all --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:48, 16 August 2013 (UTC)


### temperatures below absolute zero

Moved to a separate topic --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 06:18, 16 August 2013 (UTC)

== Absolute Zero ==

Another chain (?) of thought: Shouldn't -3K (and other out of range temperatures) be rejected  ?? 

```txt

--------------------------------------------------  -3k
       -276.15       Celcius
        564.225      Delisle    

```
--[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 09:09, 14 August 2013 (UTC)

:: In a news article [[http://www.rdmag.com/news/2013/01/temperature-below-absolute-zero|An R&amp;D Magazine Webcast]], it's possible to have temperatures below absolute zero according to physicists at the Ludwig-Maximilians University Munich and the Max Planck Institute of Quantum Optics in Garching.   An interesting article concerning (among other things) the inversion of energy due to something called the Boltzmann distribution. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 14:58, 14 August 2013 (UTC)

::: I opened the link you gave but couldn't find anything there.    
::: I did find this link though (learning something every day) http://www.livescience.com/25959-atoms-colder-than-absolute-zero.html and a sentence therein: "The temperatures we achieved are negative nanokelvin," Schneider told LiveScience. Maybe they'll never reach minus 3 K --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 20:48, 14 August 2013 (UTC)


Are there -xK temperatures?
What range (also on the upper side) should be accepted? 
Some programs check for k<0, others don't. --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 06:18, 16 August 2013 (UTC)

: Nobody seems to care :-( --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 20:34, 1 November 2013 (UTC)

:: As an aside, I choose to not check (in the REXX example) for those kinds of errors(?)   (negative kelvins) as it detracts from the conversion part of the process.   However, the presence of negative kelvins isn't for a fact, an error --- well, maybe yes, maybe no, could be, or might not be.   The more error checking that is done, the more "clutter" or chaff in the program.   In a complete working example, yes, that error check and others would be proper;   ya can never to too careful when accepting data from the command line (C.L.) or via passed arguments/parameters. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:44, 2 December 2014 (UTC)

:: Also, there appears (as I understand it) an upper bound to temperatures, probably 1 (one) Planck.   Of what little I know(?) about that temperature is, at 1º Planck, the laws of physics (or some of them?) seem to (start) breaking down.   1º Planck = 1.416833e+32 kelvins.   If that can of worms is opened, I fear for us, the hoi polloi programmers (and amateur/wanna-be/armchair scientists/physicists). -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 22:23, 2 December 2014 (UTC)

== Bonus ==

I think it would be nice to also have any-to-any conversion, 

i.e. also accept input-values in Celsius, Fahrenheit etc. 

and convert to all the other temperature-bases.  --[[User:Hajo|Hajo]] ([[User talk:Hajo|talk]]) 13:37, 2 December 2014 (UTC

: An   ''any-to-all''   is already done with the REXX example. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:31, 2 December 2014 (UTC)

: An   ''any-to-any''   has been written (in REXX) and <strike>will be</strike> has been posted to Rosetta Code <strike>soon</strike>. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 21:31, 2 December 2014 (UTC))  --updated-- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:05, 3 December 2014 (UTC)

==spelling of kelvins==

From the USMA (United States Metric Association):

The '''kelvin (K)''' temperature scale is an extension of the degree Celsius scale down to absolute zero, a hypothetical temperature characterized by a complete absence of heat energy.   Temperatures on this scale are called '''kelvins''', NOT degrees kelvin, kelvin is ''not'' capitalized, and the symbol (capital K) stands alone with no degree symbol.   [The official name was changed to "kelvin" and symbol "K" by the 13th General Conference on Weights and Measures (CGPM) in 1967.] 

In light of this, perhaps the author of this Rosetta Code task (or anybody) would like to correct the spelling and/or capitalization of the task's wording. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 01:33, 3 December 2014 (UTC)

: There seems to be a lot of disagreement about it: [http://physics.stackexchange.com/questions/114079/are-there-reasons-for-the-discrepancies-in-absolute-temp-units-kelvin-vs-kelv physics.stackexchange.com] [[User:Fwend|Fwend]] ([[User talk:Fwend|talk]]) 18:33, 9 April 2016 (UTC)
