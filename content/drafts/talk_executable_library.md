+++
title = "Talk:Executable library"
description = ""
date = 2012-09-13T21:25:54Z
aliases = []
[extra]
id = 9372
[taxonomies]
categories = []
tags = []
+++

==History==
This task came from discussions [[Talk:Scripted Main|Here]]. I was nervous about just deleting all the code for that task so decided to create this, mark the other for maybe future deletion, and see how things go. --[[User:Paddy3118|Paddy3118]] 23:44, 14 March 2011 (UTC)

==We should merge Executable library's content back into Scripted main==
Executable library is perhaps a better name, but the examples on it overshadow the point of having an executable library. Leave hailstones to another article and just show people how to do Python's if __name__=="__main__": main() in multiple languages (the point of Rosetta Stone).

If you're having trouble understanding the point of Scripted main / Executable library, run these Perl programs (https://github.com/mcandre/scriptedmain/tree/master/perl) and look at their code. test.pl illustrates loading code from another Perl file. scriptedmain.pm doubles as both a library that can export code, and a self-contained executable in its own right.

Another example is ios7crypt (https://github.com/mcandre/ios7crypt), a command line tool for encrypting and decrypting Cisco router passwords. You can either call 
```sh
 $ ./ios7crypt.scm -e monkey
```
 as a self-contained shell script, or 
```chicken
(load "ios7crypt")
```
 and treat the file like a library, accessing its methods like a normal API.

--[[User:Mcandre]]

:Or not! Hi Mcandre. If there has been no other improvements to scripted mains task description then I don't see a case for the two tasks to merge. The reasons for this task appearing will still exist. This task is about what pythons thing can give you rather than the thing itself and has already given us that excellent [[Executable_library#C|C example]] (IMHO). If you maybe extract the essence of your examples and put that in the scripted main task description then we could end up with two great tasks with slightly differing emphasis? --[[User:Paddy3118|Paddy3118]] 07:13, 13 September 2012 (UTC)

:: Executable library is duplicating the content of [[Scripted main]], thus both should be merged. The difference is that [[Scripted main]] contains bite size, simple examples in dozens of languages, while [[Executable library]] contains fewer, more overblown examples. I admit that my examples may be too simplistic to demonstrate the need for an "executable library", but I believe hailstone sequences are sufficiently complex that they distract from the purpose of an "executable library". --[[User:Mcandre]]

:::You say it is a duplicate then describe the differences. What you are ignoring is that the differences are important. An executable library could as a library contain code to work with the CGI protocol and when executed work as a CGI capable web server. Given that kind of existing capability a choice of adding extra functionality to an existing RC task  alleviates some of the issues of how to have a meaningful task, that is not too complex.
:::In contrast, I have just reread scripted main and the description doesn't say that anything need be done to fulfil it! Someone needs to state in the task description just what is required to fulfil the task. You state that it has more examples. It also has issues in its talk page that are a long way from being resolved. --[[User:Paddy3118|Paddy3118]] 21:25, 13 September 2012 (UTC)

Scripted main states at the moment:
:''It is useful to be able to execute a main() function only when a program is run directly. This is a central feature in programming scripts; the feature can be called scripted main or modulino. Examples from GitHub. Sometimes getting the ScriptName is required in order to determine when to run main(). ''
You could try answering in the text:
* Run directly vs what? Explain what you mean by direct/indirect running.
* Is it central as stated?
* Tell the example writer what is required to complete the task. A task needs to tell you how to complete it, you know, "Bring me The Ark of the Covenant" rather than "There's this box and it has great power". Only one is a task (even if you do receive lots of boxes in the post for the other) 
--[[User:Paddy3118|Paddy3118]] 21:25, 13 September 2012 (UTC)

==Tcl problem==
Oh no, after all the confusion of [[Scripted main]] the Tcl example shows a design pattern that is central to the method, but doesn't follow the task description in a way that I had thought a secondary feature of the task - the two separate files. I won't make any changes just yet, (maybe the Tcl author would like to comment)? But I am aware that although I like what this sets out to do, the explanation might need further clarification. --[[User:Paddy3118|Paddy3118]] 06:13, 16 March 2011 (UTC)
: I must've been distracted when I wrote it. Corrected. –[[User:Dkf|Donal Fellows]] 11:28, 16 March 2011 (UTC)
:: Thanks Donal. --[[User:Paddy3118|Paddy3118]] 12:34, 16 March 2011 (UTC)

== executable Shared Objects? ==

Is there a way to build Shared Objects to make them valid executables? --[[User:Oenone|Oenone]] 08:41, 19 April 2011 (UTC)

:Presumably you mean [http://www.friedenhq.org/index.php?option=com_content&view=article&id=46:shared-objects&catid=34:amigaos&Itemid=56 Shared Objects] and not [http://www.flashmagazine.com/tutorials/detail/breadcrumbs_shared_objects/ Shared Objects]? --[[User:Rdm|Rdm]] 12:19, 19 April 2011 (UTC)
:: I know it's doable in the former case on Windows. I don't know enough about ELF to know if it's doable there. --[[User:Short Circuit|Michael Mol]] 12:24, 19 April 2011 (UTC)
Yes, I was talking about ELF Shared Objects. I don't know any way to make it both, a shared object and an executable. So for Unix the task is not solvable. --[[User:Oenone|Oenone]] 12:35, 19 April 2011 (UTC)
: ELF != Unix -- you just need a different file format (and, thus, a different "interpreter").  But the problem with elf is e_type where 2 is "executable file" and 3 is "shared object" http://linux.die.net/man/5/elf --[[User:Rdm|Rdm]] 13:07, 19 April 2011 (UTC)
::: There are other problems with ELF shared objects. With systems like [[OpenBSD]], an executable needs some [http://www.openbsd.org/cgi-bin/cvsweb/src/lib/csu/amd64/crt0.c?rev=1.3 special code] to initialize certain variables (like <tt>environ</tt> and <tt>__progname</tt>), to call <tt>main()</tt> and to call <tt>exit()</tt> with the return value from main. A shared object misses this special code, so you cannot start a shared object as an executable. --[[User:Kernigh|Kernigh]] 17:11, 19 April 2011 (UTC)
:::: I think I am missing something here.  Why can't an elf shared object be made to contain this code? --[[User:Rdm|Rdm]] 18:18, 19 April 2011 (UTC)
::::: I don't know know much of anything about ELF, but you mentioned e_type...I'm guessing that's a header field which identifies the role of the binary? Perhaps there's no spec for how to execute that glue code if e_type isn't 2? (It's arguable that not all of that glue code is strictly necessary, though; the task is satisfiable without referencing environment varss, so <tt>environ</tt> might not be necessary) --[[User:Short Circuit|Michael Mol]] 18:49, 19 April 2011 (UTC)
:::::: Sure, ok... I suppose my point was that, if e_type had been defined as a bitfield instead of an enum, this kind of issue could have been solved already.  Not that I can see a use for a core dump which is also a shared object and and also an executable... And I suppose ELF could be extended to support a "shared object that is also an executable", though I am not sure who could get away with defining such an extension.  (On Linux it would be whoever owns /etc/ld-linux.so but for ELF as a whole it's probably some standards body that would take 10 years before they could get around to considering the issue.)  --[[User:Rdm|Rdm]] 19:03, 19 April 2011 (UTC)

So, most compiled languages will have to be omitted, since most operating systems don't provide anything to make this possible. Using GCC you could produce relocatable object code, which can be converted into an executable or a library, but this was excluded in the task description. --[[User:Oenone|Oenone]] 11:51, 10 May 2011 (UTC)

:Most likely. It should be possible in several dynamic languages, and is heavily endorsed by Python.--[[User:Paddy3118|Paddy3118]] 18:48, 10 May 2011 (UTC)

::Alternatively: currently the task does not require the library be a single file -- it could easily be a directory or more properly a set of files within a directory.  (And if it were changed to be required to be implemented as a single flat file it might be possible to do something by wrapping the necessary pieces in a manner something like a self extracting archive, which when used normally is an executable but can be told to extract the needed library elements at compile time, with an invocation that would need to go in the makefile or msbuild file or whatever.  (Though, of course, the task could also be changed to prohibit the generation of object code at compile time, but I think most compilers generate intermediate object files as a normal part of the compilation process..)) --[[User:Rdm|Rdm]] 20:07, 10 May 2011 (UTC)

::: The ''idea'' is for the library/shared object/module/... to be what is normally thought of as such for the language, accessed in the normal way for that language. Similarly when used as an executable it should be done in the normal way an executable is run in that language and accessing the same source. I want to allow a language the ability to mess with the content of their library-cum-program, (and maybe with the options used when invoking it in either mode?). For a compiled language, running the compiler as part of 'running' the library is not normal. The only way I could currently see a compiled language being able to do this is if their is some trick which allows a shared object when run  to act like an executable, and when used as a library, e.g. left on LD_LIBRARY_PATH in Unix, to act as shared object library.

:::: It's a bit hard to say what "the normal way" would be I have used plenty of programs which have required installers because they had so many moving parts.  But, also -- in the case of compiled languages -- we are speaking of operating system distinctions here, and not language distinctions.  (For example, consider the possibility of a program using [[wp:9p|9p]] to present entirely different images for the same file, depending on context.  The protocol itself is language independent, but support for it is not operating system independent.)  --[[User:Rdm|Rdm]] 05:29, 11 May 2011 (UTC)

::: You don't normally require the presence of the compiler at run time so I would expect any solution to not need a compiler as part of the solution.

::: What might be possible - although I am at one of the many boundaries to my knowledge - is maybe to use the neccessary presence of a byte-code interpreter for languages that normally require the use of a byte-code interpreter when run.

:::Some languages are released with compilers and interpreters. If they can fashion an example that is constrained to using their interpreters only then they should state that prominently and give their solution. I think Haskel has an interpreter. Their are much lesser known interpreters for what are normally thought of as static languages, such as C. A solution for those might best be put under the actual name of the interpreter as their may be so much distance between the two. --[[User:Paddy3118|Paddy3118]] 02:06, 11 May 2011 (UTC)


==C debug help request==
Hi guys, I was impressed with the C example enough to try it out but I can't get it to compile on Ubuntu?

My efforts so far:
;<nowiki><snipped/></nowiki>
--[[User:Paddy3118|Paddy3118]] 12:22, 16 June 2011 (UTC)

: missing <tt>#i</tt> from <tt>#include</tt> ? --[[User:Oenone|Oenone]] 12:28, 16 June 2011 (UTC)
::Thanks! that was indeed the problem. --[[User:Paddy3118|Paddy3118]] 12:42, 16 June 2011 (UTC)

Now that I have the C code working for me, I am impressed yet again. --[[User:Paddy3118|Paddy3118]] 12:43, 16 June 2011 (UTC)

==Too complicated==
This whole thing is getting too complicated. The purpose of ScriptedMain is to list code in as many languages as possible that does this: [http://docs.python.org/library/__main__.html http://docs.python.org/library/__main__.html] For lack of a better name, it can be called "scripted main". It reinforces the idea that such a function runs when the script is run on its own, not when the module is imported by other code. --[[User:Mcandre]]

:Not quite. The point is more like you can have a useful library for other programs to access ''as well as'' a useful program in its own right ''from the same file''. 
:Your suggested name isn't descriptive. It is hard to put a name on something using just two words, but I think "Executable library" has the advantage of being more descriptive.
:This tasks was written to allow languages to give a concrete example of an executable library where they can focus on the executable library part of the task but using part of a more simpler task as the basis of the library, that already has many examples - the extra work would be mostly on turning it into an executable library that could be demonstrated to work, and comparable in how the language gets things to work, in both aspects of the task: as a library and as an executable.
:Too complicated? I don't think the task is overly complex for what it attempts to describe, as the task might well be foreign to the RC audience and so programmers need guidance in trying to nail things down, remember, executable libraries might be ''popular'', (such as being mentioned in official documentation), in maybe only Python. It's certainly not a popular technique in C :-)
: --[[User:Paddy3118|Paddy3118]] 02:24, 6 August 2011 (UTC)
::Ah, I get it now. The hailstone example serves two purposes: borrow from another RC article, avoiding duplication; and present a useful example of API/CLI bundling. My ScriptedMain examples do not offer a practical reason for their existence. However, I think that the hailstone sequence is a bit much for coders who just want the Executable library behavior. Maybe use Hello World instead? I see now that it's hard to strike a balance between simplicity and usefulness. Do you have more suggestions for RC-ifying my submissions? --[[User:Mcandre]]

==Pike problem==
''"to use the library as a class, save it as HailStone.pike to use it as a module, save it as Hailstone.pmod "'' is not what the task is after. Make it one file and your on! --[[User:Paddy3118|Paddy3118]] 07:34, 7 November 2011 (UTC)
: the first file IS the one file you are looking for. it is the complete library, usable as an executable. the other two files are to satisfy the second requirement to build a second executable to use the library. since usage as a class and as a module are different, this solution provides both. i have added that to the explanations. thank you for pointing out that this was not clear.
: to elaborate: 
:: every source file in Pike with any extension can be used as an executable if it has a <code>main()</code> function.
:: every source file can be used as a module if it has a <code>.pmod</code> extension.
:: every source file can always be used as a class (because it is one)
:: the code for all 3 forms may be identical.--[[User:EMBee|eMBee]] 08:24, 7 November 2011 (UTC)

:::Hi eMBee, Are you saying then that the correctly formulated file with a .pmod extension can be used ''both'' as a module and as an executable (without name change)? If so, then all is well. --[[User:Paddy3118|Paddy3118]] 08:40, 7 November 2011 (UTC)
:::: yes, without name change. when used as an executable the extension is ignored. it only matters to separate class and module use. 
:::: the .pike extension is only needed to let pike detect the class automatically, otherwise, since classes can be compiled at runtime, i can compile any file as a class manually using <code>program Hailstone_class = compile_file("hailstone.pmod");</code>.--[[User:EMBee|eMBee]] 08:55, 7 November 2011 (UTC)
:::::Ta! --[[User:Paddy3118|Paddy3118]] 09:40, 7 November 2011 (UTC)

==ADA and use of library==
It is not clear if the executable ./hailstone_test calls the compiled library ./hailstone or if it just compiles in the source to ./hailstone (which would not satisfy the task). --[[User:Paddy3118|Paddy3118]] 18:10, 29 May 2012 (UTC)
: With the proper set of compiler options, you can compile and link ''hailstone_test.adb'' and generate the executable hailstone_test '''without''' the source ''hailstone.adb'', but with the specification ''hailstone.ads''.  Which seems to satisfy the task. On the other hand, as for most compiled languages, there is something in between source code and executable. To generate either of the the executables ''hailstone'' and ''hailstone_test'' the linker must access the object code in ''hailstone.o''.

::Hi, the task would require ./hailstone to be an executable and ./hailstone_test to be another executable that does not contain the code of ./hailstone but would get theat functionality from the ./hailstone file (probably by dynamic linking). That is very hard to do in a compiled language but not impossible - [[Executable_library#C|the C example]] shows that it can be done.

::As it stands, it seems as if the ADA code doesn't do enough. --[[User:Paddy3118|Paddy3118]] 20:14, 30 May 2012 (UTC)
