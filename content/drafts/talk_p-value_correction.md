+++
title = "Talk:P-value correction"
description = ""
date = 2018-03-20T17:38:32Z
aliases = []
[extra]
id = 21660
[taxonomies]
categories = []
tags = []
+++

Would anyone be able to offer any specific corrections/additions to what I've written here?--[[User:Hailholyghost|Hailholyghost]] ([[User talk:Hailholyghost|talk]]) 14:28, 10 November 2017 (UTC)

: It would be much better if you gave the algorithm to use, as pseudo-code, as part of the task description. [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 07:52, 11 November 2017 (UTC)

== License problem ==

Like in the [[Fivenum]] task, the R code that was shown comes from the official R sources, and is covered by the GPL license. Therefore, it's not suitable for RC. It's a bit annoying, because almost all implementations are translations of this R code, or translations of translations. And the GPL is not compatible with the GFDL of this site, so all these solutions should really be removed. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 07:34, 14 March 2018 (UTC)

So far, here are the implementations which are modified versions of GPL code, hence protected by GPL:
* C: translation of R
* Kotlin: translation of C
* Perl: translation of C
* zkl: translatoin of C
* D: translation of Kotlin
* Java: translation of D
* Python: translation of Perl

I suspect the Perl 6 is a translation of Perl or C, so has the same problem, but the author gave no indication.

The only implementations that actually respect the GFDL (the license of the Rosetta Code web site) are the R and Julia implementations, which simply use a library function.

To Rosetta Code contributors: please read [[Rosetta Code:Copyrights]]. It states: ''"By submitting content to this site, a user of Rosetta Code guarantees that they have the authority to release such content under the GNU Free Documentation License, version 1.2. Content, once submitted to this site, becomes available under the GFDL (v1.2), as well as any more permissive license the contributor chooses (See below). The submitter, and the submitter alone, accepts liability for consequences arising from the unauthorized release of submitted copyrighted material."''

However, the GFDL is '''NOT''' compatible with the GPL. See for instance [[wp:GNU Free Documentation License#GPL incompatible in both directions]].

[[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 11:13, 14 March 2018 (UTC)

: Thanks for pointing out that there are licensing issues with those implementations which are direct or indirect translations of R source code and which (as the author of the Kotlin entry) had escaped me.

: Fortunately, I believe the Perl 6 entry to be an original work. It is much more concise than any of the offending entries, predated the Perl entry and looks nothing like the C code. Moreover, it makes use of features (Schwartzian transforms and the Šidák correction method) which the others lack.

: I think the problem could therefore be solved if the offending entries were eventually replaced by direct or indirect translations of the Perl 6 code. To start the ball rolling I've added a second Kotlin version (q.v.).

: The results are the same except for the Šidák method. I'm no expert in this area so may have made a mistake but, from what I've read, this is a simple 'one-step' correction method (similar to but more optimistic than Bonferonni) whereas the Perl 6 code seems to be doing something more complicated.

--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 19:09, 18 March 2018 (UTC)
:Nice if we can use the Perl 6 version, thank you! [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 20:04, 18 March 2018 (UTC)

:: For good measure, I thought I'd add a second C version(q.v.) so there's plenty now to choose from for those wishing to update their translations.
--[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 17:37, 20 March 2018 (UTC)
