+++
title = "Talk:Program name"
description = ""
date = 2011-11-02T19:13:05Z
aliases = []
[extra]
id = 10243
[taxonomies]
categories = []
tags = []
+++

== Java entry ==
The Java entry is kind of funny because it requires you to type the name of the class in order to print it. Then you might as well print it directly. It really should be using reflection. I've never used reflection in Java, but a after a glance at the [http://download.oracle.com/javase/tutorial/reflect/class/classNew.html documentation], I came up with this. [[User:Fwend|Fwend]] 03:26, 6 August 2011 (UTC)


```java
public class Test {
   public static void main(String[] args) {
       Class c = new Object(){}.getClass().getEnclosingClass();
       System.out.println(c.getName());
   }
}
```

:That doesn't work. There is no enclosing class for Object so it's an NPE when you try to get the name. I'll correct the example. --[[User:Mwn3d|Mwn3d]] 04:32, 6 August 2011 (UTC)
:: Strange. It printed 'Test' yesterday when I tried it. But maybe I made a change to the code after that. I've added a body to new Object, now it works again. Anyway, the System.getProperty thing is even better. [[User:Fwend|Fwend]] 07:54, 6 August 2011 (UTC)
:::I agree, it's pointless to hardcode the class and then ask for it as a string. Thanks Fwend and Mwn3d for correcting the example. --[[User:Mcandre]]
::::I think it's still not right. The main method can be called from another class, since it's public. You could run the class Test2 which calls <code>Test.main(args)</code> in its main method. The desired output for this task would be "Test2" since that's the "script" that was run, but that example would still print "Test". --[[User:Mwn3d|Mwn3d]] 17:58, 6 August 2011 (UTC)
::::: Then you'd be going out of your way to get the wrong result. The code should obviously be in the active main. [[User:Fwend|Fwend]] 23:15, 6 August 2011 (UTC)

==Question on the goal of the task==
Hi, Is it to:
# Give the name of the executable for compiled code
# Give the name of the file being interpreted in the case of interpreted code.
The task description seems OK for interpreters, but the C example does not fit the task. Maybe the task description needs an update? --[[User:Paddy3118|Paddy3118]] 02:43, 6 August 2011 (UTC)
:The point is to identify the program name, which may mean different things in different languages. For compiled languages, the best you can do is identify the final executable. For interpreted languages, the program is often considered to be the source code file name. --[[User:Mcandre]]

==Re-title?==
Suggest the task be renamed "Script name" which is more in line with RC style? --[[User:Paddy3118|Paddy3118]] 02:46, 6 August 2011 (UTC)
:+1 --[[User:Mwn3d|Mwn3d]] 04:32, 6 August 2011 (UTC)
:+1 Sure, I don't mind. Consistency above all. --[[User:Mcandre]]

::What about "Invocation name" or "Program name"? The task is not limited to scripts. [[User:Markhobley|Markhobley]] 21:21, 6 August 2011 (UTC)
:::Now that you mention it, "Program name" sounds better. --[[User:Mwn3d|Mwn3d]] 02:59, 7 August 2011 (UTC)
: This wiki likes "Names like this", but some other wikis like NamesLikeThis. So I am not surprised that some users create ScriptName or NamesLikeThis in this wiki. --[[User:Kernigh|Kernigh]] 01:39, 7 August 2011 (UTC)

==AWK==

The awk solution does not work for me. I pasted the code into an executable script file test.awk as follows:

 #!/usr/bin/awk
 BEGIN {
 "ls" | getline file
 close("ls")
 print "This file is " file
 }

I then run my script test.awk as follows:

./test.awk

This gives an error: awk: 1: unexpected character '.'

My awk interpreter is mawk, if that matters.

[[User:Markhobley|Markhobley]] 12:14, 6 September 2011 (UTC)

Ahhh, I fixed the hashbang:

 #!/usr/bin/awk -f

It still didn't work though, because it gives the wrong filename:

 $ ./test.awk                                  
 This file is keyring-QoRJiR

It should read "This file is test.awk".

[[User:Markhobley|Markhobley]] 12:17, 6 September 2011 (UTC)

: This solution was wrong, by reporting the first file in the current directory (as if by <code>ls | head -1</code>), which might not be the Awk script. This solution replaced a previous solution, also wrong. I deleted this solution and added <nowiki>{{omit from|AWK|...}}</nowiki>, because I believe that no solution is possible. --[[User:Kernigh|Kernigh]] 17:55, 6 September 2011 (UTC)

==Octave==

The Octave example is not wrong. Try it, it works, end of story. --[[User:Mcandre|Mcandre]]

: Not really.  OS kernels don't necessarily pass all items on shebang line to interpreter; some give the script name as the only argument, some pass all items on the shebang line as a single string, and some pass only the first item as argument (and some don't even have /usr/bin/env).  The example works ''on your system'', end of story. --[[User:Ledrug|Ledrug]] 03:23, 30 September 2011 (UTC)

:: Does <code>#!/usr/bin/env octave -qf</code> work on some system? I had assumed that someone had inserted a <code>#!/usr/bin/env octave -qf</code> without testing it. Perhaps my assumption was wrong. --[[User:Kernigh|Kernigh]] 03:35, 30 September 2011 (UTC)
::: On Linux /usr/bin/env would try to invoke interpreter with "octave -qf" as exe file, so it's not going to work here.  I don't know if some other system would behave differently.  For reference [[http://www.in-ulm.de/~mascheck/various/shebang/#blankrequired]]. --[[User:Ledrug|Ledrug]] 03:39, 30 September 2011 (UTC)

:: If it's a question of OS-dependent behavior, then {{tmpl|works with}} is probably the appropriate way to identify this. If most common platforms ''do'' provide the information, then {{tmpl|works with}} is probably unnecessary, and listing exemptions (or classes of exemptions) is probably more useful. --[[User:Short Circuit|Michael Mol]] 14:04, 30 September 2011 (UTC)
::: Judging by the reference chart I linked to above, it may work on MaxOS X 10.4 and later, some BSDs, never Linux, some SunOS, some cygwin, and native windows is kinda out of question.  I'm assuming the empty cell int that chart means "all args in one", which means 'octave -qf' does not work.  It's very messy, and unless someone can test it comprehensively, it might just be easier to leave it out.  For now it's probably safer to use inclusive 'works with' if you can verify it works on your OS. --[[User:Ledrug|Ledrug]] 14:20, 30 September 2011 (UTC)

== Common Lisp ==

while i agree that it is odd to have references to other implementations when it is stated that the solution is specific to CLisp, i think the tendency should be to make this code run on other implementations as well. --[[User:EMBee|eMBee]] 19:13, 2 November 2011 (UTC)
