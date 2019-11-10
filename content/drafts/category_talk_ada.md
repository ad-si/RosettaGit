+++
title = "Category talk:Ada"
description = ""
date = 2010-12-16T09:54:16Z
aliases = []
[extra]
id = 4623
[taxonomies]
categories = []
tags = []
+++

[[Ada]] has compilers for .NET and JVM targets, i.e. into intermediate code. So according to the [artificial and in general wrong] classification machine vs. bytecode [[Ada]] is in both categories. There also exist [[Ada]] to [[C]] compilers, which do not fall into any category. I also know at least one [[Fortran]] compiler which capable to produce either machine or intermediate interpretable code. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 20:11, 31 July 2009 (UTC)
:While many languages are starting to get side implementations that use the JVM for execution, the purpose for the execution method categories is to note the standard method of execution. Where does that rule put Ada? --[[User:Mwn3d|Mwn3d]] 20:15, 31 July 2009 (UTC)
::There is no standard execution method, the language just does not mandate any. It is was designed to be portable, so whether the target is "hard" or "soft" is irrelevant. The same program can be compiled for JVM or x86 target, with an OS or else standalone. The classification has little sense to me, because any portable language is like this. When the target is more or less universal you can always compile into it. --[[User:Dmitry-kazakov|Dmitry-kazakov]] 10:35, 1 August 2009 (UTC)
:::Lots of languages don't have standard execution methods. That's missing the point entirely. People writing programs in Ada will usually anticipate that their code will run with the performance of native execution, and there are toolchains to support this assumption of long-standing. The significant uses of Ada will also be done this way, especially if you were to encounter its use in the wild (e.g., in defense consulting). And the “execution method” field is single-valued anyway, so can only really capture the majority of what is seen in practice. Think of it this way: if every installation of an Ada program in the world were to vote right now on how it is executing, which candidate would win the nomination? —[[User:Dkf|Donal Fellows]] 06:37, 2 August 2009 (UTC)
:::: A language doesn't typically have an execution method outside of an implementation.  Details such as bytecode-oriented, native-code, JIT and interpreted are all a matter of implementation for all languages that I know of which aren't also definitions of a specific virtual machine, such as CLR, Parrot, JVM, Brainfuck and SNUSP.  Details like execution method should be moved from the language template to the implementation template.  Toolchains are typically separate from the semantics and meaning of most languages.  With Ada being a relatively rigorously standardized language, I don't see that anything not defined as part of its language standard should be indicated to be an integral part of the language.
:::: Regardless, this has highlighted a problem with the current design of the language template (Not something to get excited about, as few rollouts are flawless.); Having certain details as scalar values rather than lists leads to information loss.  The next iteration of the language template might try transcluding the list from another page.--[[User:Short Circuit|Short Circuit]] 08:50, 2 August 2009 (UTC)
::::: Yes, I agree with that. It belongs to the language implementation template. (Granted, there are languages mandating certain implementations or making some classes of implementation difficult.) --[[User:Dmitry-kazakov|Dmitry-kazakov]] 09:13, 2 August 2009 (UTC)
:::"Standard execution method" was probably the wrong phrase to use. "Standard" has a couple meanings. Maybe I should have said "most likely execution method you'd run into if you chose to program in a language". Does that make it any better? --[[User:Mwn3d|Mwn3d]] 15:29, 28 August 2009 (UTC)
== Custom highlighting ==

I just removed some custom syntax highlighting from the Ada example in [[Exponentiation operator]]. Does anyone remember anywhere else that this was done to avoid just going through and checking? --[[User:Mwn3d|Mwn3d]] 15:26, 28 August 2009 (UTC)

== web stuff ==

These Tasks would best be implemented using [http://libre.adacore.com/libre/tools/aws AWS]:
* [[Active Directory/Connect]] (in reality it's just LDAP)
* [[Active Directory/Search for a user]]
* [[HTTPS]]
* [[HTTPS/Authenticated]]
* [[HTTPS/Client-authenticated]]
* [[Rosetta Code/Count examples]]
* [[Rosetta Code/Find unimplemented tasks]]
* [[Send email]]
* [[SOAP]]
* [[Web scraping]]
* [[Yahoo! search interface]]

I don't have AWS available, but will have a look at them as soon as i have. [[User:Oenone|Oenone]] 09:54, 16 December 2010 (UTC)
