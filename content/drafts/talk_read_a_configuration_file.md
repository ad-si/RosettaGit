+++
title = "Talk:Read a configuration file"
description = ""
date = 2013-07-27T18:12:46Z
aliases = []
[extra]
id = 9245
[taxonomies]
categories = []
tags = []
+++

== current task ==

Ok, so we currently have a task here without any implementation.  But I find it odd that we are supposed to set a value based on the contents of a comment in a config file.  In properly factored code I would imagine that either [a] you set the defaults before the config file is read, and/or [b] you explicitly declare non-false boolean values.  (I am also a bit wary about the use of the term "standard" in that config file's comment.)  --[[User:Rdm|Rdm]] 22:28, 10 February 2011 (UTC)
: [[Java]] [[wp:.properties|properties files]] are closer to a ''de facto'' "standard", especially if you ignore the property substitution feature. Failing that, there's the old [[wp:INI file|INI format]]. –[[User:Dkf|Donal Fellows]] 22:59, 10 February 2011 (UTC)

:How about modifying the task be the reading of a "simple" ini file that contains:
:
```ini

; Here we go...
[my_section]
; Blah=blah
ab=1.234
[her_section]
;ab=xyz
bc=C:\a\b\d.txt
ab=release

```

: And print the resulting information? I would think that variants of .ini files are in common use, and the current task example is odd in having two comment characters, #; one of which ';' seems to be ignored and a value returned for seedsremoved; also the file has uppercase identifiers that are returned as lowercase variables? Too much magic! --[[User:Paddy3118|Paddy3118]] 03:59, 11 February 2011 (UTC)

: The task says to ignore comments. So I wrote a Ruby example that ignores the '; SEEDSREMOVED' line and then prints 'seedsremoved = false' because it never found a 'SEEDSREMOVED' line. I suspect that the task is a draft because the author wants to decide what else to put in the config file. --[[User:Kernigh|Kernigh]] 04:41, 11 February 2011 (UTC)

Indeed. SEEDSREMOVED is commented and should be ignored. That sound right to me. There is a sample file in the task description. This task is good to go as far as I can see.

[[User:Markhobley|Markhobley]] 19:00, 24 April 2011 (UTC)

I have replied to the reason for two comment characters on a previous occasion: The semicolon can be removed by an automated configurator. A hash can never be removed by a configurator.

: Are we supposed to use the examples shown?  Most people have "corrected" the spelling of <tt> NEEDSPEELING </tt>  which means ''it needs climbing''. It's old word (1513 ce, chiefly Scottish) -- [[User:Gerard Schildberger|Gerard Schildberger]] 08:19, 29 March 2012 (UTC)

== "Standard" ==

I would like to see a statement of exactly how this configuration file format is “standard”, or the removal of the “standard” wording. Or, parsing a more “standard” (well-known) config file format such as Java properties or INI. —[[User:Kevin Reid|Kevin Reid]] 18:49, 16 February 2011 (UTC)

: Agreed. My inclination would be to use INI as being more "language neutral".--[[User:Tikkanz|Tikkanz]] 20:00, 16 February 2011 (UTC)
: It would probably be better to formulate similar as "read a config file according to your language standard, extra credit for INI, Java property and XYZ" --[[User:Jofur|&lt;Jofur&gt;]] 20:11, 16 February 2011 (UTC)
: Agreed as well. I'd also be interested in seeing if there are configuration formats which can be parsed based on run-time-consumable grammar definitions. --[[User:Short Circuit|Michael Mol]] 20:23, 16 February 2011 (UTC)

This was very similar to the .INI format, before the boxed section headers were introduced. I developed that into a brief specification entitled "Standard Configuration File Format" which I wrote back in 1991.

The reason for the two comment symbols is that the hash symbol is a permanent comment and cannot be manipulated automatically. However, a leading semicolon may be removed or placed by automatic software reconfiguration tools.

A related task to this one, which I have not yet written up is to comment out or uncomment one of the parameters.

[[User:Markhobley|Markhobley]] 00:47, 21 February 2011 (UTC)

: In that case, where is the documentation link to the formal description of what the spec is? A standard isn't a standard unless people agree on it. That's the whole of what defines a standard, and I can assure you that people only really agree on defined versions of documents. (I used to work in standardization, so I ''know'' what's involved. It's a long long slog to do it for real.) –[[User:Dkf|Donal Fellows]] 17:14, 24 April 2011 (UTC)

On orginal parser implementations, comments were completely ignored. I went for option [a] set defaults before the config file is read. Boolean values were initially set to false unless toggled to true by the parser. There was no magic to uppercase / lowercase and variable names not matching the option names. I simply converted the case of the option name to upper case, and in the match code, I simply placed the matched data into the variable that I required.

[[User:Markhobley|Markhobley]] 00:15, 28 February 2011 (UTC)

: I take it you're not dealing with configuration parameters with commas in? (So far as I can see, they're always handled as sequence separators. Significant leading and trailing space would be another issue.) –[[User:Dkf|Donal Fellows]] 17:18, 24 April 2011 (UTC)

:: Then, so much for having (people) names in the configuration file (as OTHERFAMILY indicates), commas are part of [some] names, and even more so with titles and such. -- [[User:Gerard Schildberger|Gerard Schildberger]] 09:02, 29 March 2012 (UTC)

:: Commas are ubiquitous.  

moneyFormat=$999.999.999.999.99

favMovie=It's a Mad, Mad, Mad, Mad World

saying=Other than that, Mrs. Lincoln, how was the play?

seperater=,

who=Every Tom, Dick, and Harry.

savings=$12,567.42

born=August 22, 1977

SOE (during WW II), the British abbreviated "Special Operations, Executive" as "S.O.,E."

 -- [[User:Gerard Schildberger|Gerard Schildberger]] 17:10, 29 March 2012 (UTC)

----

Commas are used to separate parameters. In implementations that I have written, the configuration parser passed the parameters to a separate parser, which handles the comma and validation of the parameters. The job of the configuration reader was to read the file, and feed the appropriate parameters to the appropriate variables.

[[User:Markhobley|Markhobley]] 19:11, 24 April 2011 (UTC)


* The authors reason for why he calls it a standard  - being created in 1991, is not enough to call it a standard.
* Doing what the source of the original implementation does is not enough to describe the format for other implementations.

The good thing about standards is that we have so many to choose from :-)

We really need a succinct description of the format to get this page moving, or swap to a similar but better documented format. --[[User:Paddy3118|Paddy3118]] 18:07, 24 April 2011 (UTC)

There is a sample file on the task description. Which part of the format is not clear from the example?

: the config line that has ;SEEDSREMOVED.  It is clearly a comment and it's supposed to be ignored <tt> {lines beginninning [sic] with a semicolon are ignored ...</tt> }, but yet, the task says that we are supposed to set the var <tt> seedsremoved </tt> to <tt> false </tt>. --[[User:Gerard Schildberger|Gerard Schildberger]] 09:00, 29 March 2012 (UTC)

: the use of (apparently) commas (,) to signify multiple parameters. For a (human being) name(a) such as -----> Harry Connick, Jr., Pliny The Elder, a Roman (23 AD - Auguest 25,79 AD), S. J. Smith, DDS <----- it would be very hard to correctly parse (as the "rules" are, it would be parsed as seven different names).  For my part, I'm going to treat multiple parameters as one, as the task said these MAY be stored in an array. -- [[User:Gerard Schildberger|Gerard Schildberger]] 09:00, 29 March 2012 (UTC) 

: what if commas are supposed to be part of the data field? -----> primes=2,3,5,7,11,13 ... <----- -- [[User:Gerard Schildberger|Gerard Schildberger]] 09:00, 29 March 2012 (UTC)

: the preserving of case for the parameter fields seems to be optional: "<tt>... sensative, but configuration parameter data is case sensative and MAY be preserved ...</tt>".  If it isn't preserved, then the data isn't case sensative. Don't ask us to scan for case sensative data, and then we MAY throw it away ... or keep it (the case, that is). -- [[User:Gerard Schildberger|Gerard Schildberger]] 09:00, 29 March 2012 (UTC)

: what happens when the parameter (name) has a character that isn't support as a valid symbol for a "variable name"? -----> Ægypt = Cairo <-----, ----> ½pint = slang for small person <-----, ----> Großman=Germany <----- most of these, of course, we can almost assume we know what they mean, but what if the meaning isn't known? -- [[User:Gerard Schildberger|Gerard Schildberger]] 09:00, 29 March 2012 (UTC)

We can always create new tasks for other formats of course.

[[User:Markhobley|Markhobley]] 19:11, 24 April 2011 (UTC)

== REXX vs ooRexx ==

Can you please tell us what did not work on which classic Rexx? (except, maybe, for the first line) --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:10, 24 July 2013 (UTC)

: The example that was moved to ooRexx had a comment:   ''This program was tested using Open Object Rexx 4.1.1.''

: The following caused problems with various Classic REXXes:

:: * The use of   '''line~left(3)'''   (Regina, PC/REXX, Personal REXX, R4 [et al]).  
:: * Some   '''stream'''   options weren't recognized (et al). 
:: * The first line raised a '''syntax''' error (PC/REXX, Personal REXX, R4).
:: * The   '''IF arg(1) = '-h', ...'''   construct of using   '''then'''   wasn't recognized (et al). 
:: * The use of   '''any'''   with the   '''signal'''   statement (et al).
:: * Received an ''incorrect call to routine''   '''value'''   ----- at this point, I stopped looking for more errors.

: I don't feel comfortable doing this level of debugging/bench-testing of a REXX program that clearly wasn't tested with any Classic REXX.   Maybe the author of the program could perform that task better. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 17:31, 24 July 2013 (UTC)
::: Thank you for telling me/us (I am NOT the author). --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 18:22, 24 July 2013 (UTC)

:::: I tested now the REXX program and I think that there is a problem with bad variable names

```rexx

   varList=varList xxx                 /*add it to the list of vARiables*/
   if value='' then value='true'       /*if no value, then use "true".  */
   if symbol(xxx)=='BAD'  then do      /*can REXX use the variable name?*/
                               Say xxx 'is a bad name'
                               badVar=badVar+1;    bad=bad xxx;    iterate
                               end

```

:::: bad names should not go into varlist!?! --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 19:47, 24 July 2013 (UTC)

::::: Yes, you're correct.   If the variable name (in the configuration file) isn't a legal REXX variable name, then it shouldn't be added to the list of variable names (varList) in the REXX program.   The REXX code has been fixed. -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 18:12, 27 July 2013 (UTC)
