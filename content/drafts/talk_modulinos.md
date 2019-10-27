+++
title = "Talk:Modulinos"
description = ""
date = 2015-04-17T17:28:29Z
aliases = []
[extra]
id = 9324
[taxonomies]
categories = []
tags = []
+++

== Needs more description ==

The task description here is missing something. Is this a task to show a main function that will be run from the command line? I've never heard the phrase "scripted main" before, so I'd like some more definition there too. The examples also seem to do lots of stuff that isn't described in the directions. --[[User:Mwn3d|Mwn3d]] 00:16, 4 March 2011 (UTC)
:Very much agree.
:The Github link in the intro text says this:
 Programs that only run main() if they're not loaded as libraries by other programs.
:Make of that what you will. Clarification '''definitely''' needed. -- [[User:Eriksiers|Erik Siers]] 07:16, 4 March 2011 (UTC)
:I write command line programs with usage info, flags, etc. I like to combine the exported API with a usable program. Some programming languages have well-known syntax; Python's if __name__=="__main__": main() illustrates this behavior. Other languages, such as Common Lisp, require a hack. I want these hacks to be more easily Googled--I had trouble finding them in the first place. --[[User:Mcandre|Mcandre]]

:: This current examples are an awful way to use Python's <tt>__name__ == "__main__"</tt> or Ruby's <tt>__FILE__ == $0</tt>. The Python code exports <tt>scriptedmain.main()</tt> and the Ruby code actually defines a global function <tt>main</tt>, which replaces my global function <tt>main</tt> if I defined one before I required the library. I would like to change the Python and Ruby examples to look like <tt>if __name__ == "main": print "It is %s" % meaning_of_life()</tt>, but I am not sure if the task allows me to do this. --[[User:Kernigh|Kernigh]] 22:40, 6 March 2011 (UTC)

::: Kernigh, requires go at the top of files, not after you've declared a main() method. If you deviate from standard programming layouts, you circumvent the language's built-in capabilities, and you're sure to get strange results. --[[User:Mcandre|Mcandre]]

==Name change?==
Reason being that ScriptedMain , even if changed to the more RC norm of "Scripted main", doesn't mean much, or as much as, say, '''"Executable library"'''. The task seems to be about having a means of allowing library code - usually imported/included into a larger program; to also function as a useful program in its own right when executed directly.

:Use whichever name you like. It's hard to search for this behavior online, so I've decided to give it an itinerant name, "Scripted main", the main() that runs only when a program is run directly, not imported by other programs. --[[User:Mcandre|Mcandre]]

The idea of an executable library (scripted main), has merit for interpreted languages, but seems contrived for compiled languages as in most cases you are further removed from both the library sources and the means to compile them, when actually executing a program using the library. Although many stand alone libraries are available in, for example, C, I have only seen stand-alone utilities that come with those libraries as separate programs with separate source files that link to the libraries at compile time. --[[User:Paddy3118|Paddy3118]] 00:39, 4 March 2011 (UTC)

There is some truth to that. But many languages offer scripting and compilation. In any case, it's helpful to know that main() is always executed (C, C++, D), optionally executed (Perl, Python, Ruby, Chicken Scheme), or requires obscure syntactical tricks to even attempt (Bash, Lua, Common Lisp). I could omit the print-current-directory, print-current-program, and print-command-line-arguments sections from the scripted main examples, since Rosetta already has snippets for those behaviors. But programmers are likely to want scripted main and these other behaviors together, and often one is needed in order for the other to be possible. E.g., Chicken Scheme has no built-in way to get the program name, and scripted main won't work in compiled mode unless it can compare its command line arguments and program name.

TL;DR: All four behaviors are useful to know and they're very hard to Google. --[[User:Mcandre|Mcandre]]

: ... Which is why I would prefer a more descriptive name that might be more likely to be searched for, like '''"Executable library"'''. --[[User:Paddy3118|Paddy3118]] 12:35, 6 March 2011 (UTC)

:: I think that "library" should be in the page name. I prefer '''"Library with optional main"''', where the optional main is what runs only when I directly run the program, not when I load the library. '''"Library with scripted main"''' would work if we want to keep the "scripted main" phrase. --[[User:Kernigh|Kernigh]] 22:40, 6 March 2011 (UTC)
::: Those aren't bad ideas, and they show that you understand why I created the article. Makes me all warm and fuzzy. --[[User:Mcandre|Mcandre]]

== Some chat in IRC ==

There was some discussion in IRC on the subject: [http://irclog.perlgeek.de/rosettacode/2011-03-04#i_3355683 http://irclog.perlgeek.de/rosettacode/2011-03-04#i_3355683]. --[[User:Short Circuit|Michael Mol]] 16:14, 4 March 2011 (UTC)

:When a compiled program is run, the compiled program is run. The compiled program cannot be loaded as a library too can it? (I mean that it isn't done normally). If not then only normally interpreted languages could have this property; things like the C example don't fit the task description. --[[User:Paddy3118|Paddy3118]] 16:33, 4 March 2011 (UTC)
:: It can; we do exactly this in one of my employer's products. I was rather surprised myself. I'd venture a guess that, at the OS level, this is exactly what's done with binary images anyway, except that the OS automatically runs a predefined entry point. (On Windows, I believe this is WinMain. I'm not sure what it is on Linux. The CRT wraps these in both cases.) The C example on the page is incorrect to that end, though. I believe the Java example is, too. --[[User:Short Circuit|Michael Mol]] 17:37, 4 March 2011 (UTC)

:I could change the task to reflect what Python does with, for example its CGIHTTPServer module, where calling it as command <code>python -m CGIHTTPServer</code> starts a web server, and loading it as a module would allow the calling program access to code for setting up a web server. --[[User:Paddy3118|Paddy3118]] 16:33, 4 March 2011 (UTC)

C code can use libraries ("shared libraries", "dynamically loaded libraries", whatever...).  It's relatively trivial to put all of the logic in a library and have a trivial program that links to that library.  That said, this is more about the build process than it is about the C code.   (But good practice usually suggests that the "library code" and the "program code" be in separate files.) --[[User:Rdm|Rdm]] 12:16, 7 March 2011 (UTC)
: Can shared libraries be executed ''without'' an extra 'trivial program' though? Python not only has that ability, but it is a documented and well used feature of the language. --[[User:Paddy3118|Paddy3118]] 17:15, 7 March 2011 (UTC)

:: I believe that that is an OS issue.  Hypothetically speaking: a shared library that defines a "main()" routine, can be treated as an executable.  However if the OS requires something else (like: let's say there is a bit you set: when this bit is set you have an executable, when this bit is cleared you have a shared library), then you need a separate file (so that that bit can be set to different values for each of the two purposes).  (With Python, the interpreter itself can serve to make this distinction -- in this case it is the "extra 'trivial program'" (or perhaps not-so-trivial program) that you need to make this work.)  --[[User:Rdm|Rdm]] 17:24, 7 March 2011 (UTC)

::: So far it seems there are a lot of examples with no clear definition of what they are about. To require the presence of a compiler for these examples in statically compiled languages is irregular, as most run-time environments for compiled languages do not have the compiler or the source present. 
:::The task seems to be a way to get RC as the second place that mentions the page title as well as [http://speely.wordpress.com/2010/11/27/writing-scripts-with-common-lisp/ one blog entry] all from the same person, without enough thought on what ideas are to come under the title. 
::: Mcandre needs to answer some more of the points put to him on the purpose of the task - maybe by answering all the questions and suggestions on this talk page). 

:::I guess I am against what looks like one persons attempt to popularize their own nebulous idea through RC. Is this a different kind of spam? --[[User:Paddy3118|Paddy3118]] 07:34, 8 March 2011 (UTC)
:::: I don't know. It could simply be a case of [https://thelure.wordpress.com/2010/12/06/tais-model/ Tai's Model], or it ''could be a design pattern without an agreed-upon name.'' I could be wrong, but I think it's likely to be the latter. The name 'scripted main' is in no way clarifying to me as to what the pattern is supposed to do. However, the [https://github.com/mcandre/scriptedmain/blob/45e1ea1521fd541681c6db1468ab0964ed8f9595/README README file] from mcandre's GitHub hosting of his initial examples describes a simple behavior, ''"scriptedmain - Programs that only run main() if they're not loaded as libraries by other programs."'' I really think the better description of this is "write a program  which does one thing when executed directly and another thing when loaded as part of a larger program." --[[User:Short Circuit|Michael Mol]] 16:21, 8 March 2011 (UTC)

::::: Yes, there is some confusion over compiled vs scripted vs interpreted vs assembled code and what qualifies as an executable. For the purposes of this article, I consider any ELF/MACH-O/DOS/WIN32 binary file, and any shebang'ed file as an executable. Strictly compiled languages (C, C++, D, Objective C) don't seem to belong in this article, because programs in those languages already have a good idea of what a main() function does and where it belongs. But, in the subset of compiled languages, some people prefer not to use separate files for API and CLI. E.g., "weak" allows this in C/C++.

:::::: I think windows COM servers would be an example of doing this kind of thing with a compiled language.  It is, of course, OS specific.  But the whole concept of "executable" is also OS specific.  Tasks which ask us for a subtle variation on an OS specific concept would typically be OS specific tasks.  --[[User:Rdm|Rdm]] 16:19, 12 May 2011 (UTC)

::::: For major scripting languages (Perl, Python, Ruby), it's often assumed that API and CLI work together. To beat a dead horse, Python's if __name__=="__main__" is a much sought-after snippet (2,030,000 results in Google at last count). Coders want to know how to do this. I think they want to do this in other languages as well.
::::: For languages in the middle (Octave, Common Lisp, Chicken Scheme, Clojure), it's plumb difficult to achieve this behavior. Some languages can only partially do "scripted main"; they either require obscure shebangs (Clojure), or worse, require special commands in the shell (LLVM). If I don't add a runtime example for a language, that means that both "python scriptedmain.py" and "./scriptedmain.py" work as expected. IRC users refuse to help me, claiming that their language's REPL is infinitely better than shell. That may be, but I like to provide standard, POSIX, CLI interfaces whenever possible. It took me hours to wrestle working snippets--other coders shouldn't have to.

::::: I want to get back on IRC and chat with you guys. You seem interested, and rightly curious. In the mean time, feel free to twitter me at @mcandre. --[[User:Mcandre|Mcandre]]

:::::: FWIW, there's no problem with OS-specific examples — that's how life is sometimes — though it's best if they're described as such. It does mean that there's an opportunity to describe how to do it on other operating systems/platforms, but that's all cool. (Re language snootiness, hang in there and fight the good fight. The world is open, not closed.) –[[User:Dkf|Donal Fellows]] 09:53, 13 May 2011 (UTC)

== Ambiguities ==

According to the page history, the C program was the first example, so it should serve to illustrate what this task is about.  However, the C implementation has:


```c
#include "scriptedmain.h"
```


This whole concept of "startup" is an OS issue, and not a language issue.  Some operating systems have various ways of starting programs, and "main" may or may not be a part of them.  (For example, using drag&drop to start a program might work differently from starting the program from a command line.)

Furthermore, different concepts of what this all means depend very much on how the code is represented.  A compiled language may be represented as a shared library, or not, where an interpreted language may be represented as neither.  But a compiled language may also be represented as source code.  And if you have source code as a set of files, you could perhaps simply ignore the file that defines main (if the language even uses that name -- some might abstract it away even if the OS requires its use).

So, anyways, I think this task needs some work.  Specifically, I think it needs some kind of illustration requirement (where the "scripted main" code gets used by some other code that has its own "main").

-- [[User:Rdm|Rdm]] 17:51, 4 March 2011 (UTC)

Test the C program yourself. As long as scriptedmain.h is missing the main() prototype, a second file test.c will not execute scriptedmain.c's main(). --[[User:Mcandre|Mcandre]]

:I see that you have updated the description of the C program, to specify that scriptedmain.h is blank, so I have removed those comments that referred to this issue.  However, since the example does not demonstrate the use of any API, I think the example is useless and unclear.  (We have to imagine that we know what you are trying to illustrate.)

:That said, my current impression is that you are asking us to illustrate putting the API in a separate file from the definition of "main".  If that is the case, many of the examples here (including the one I wrote) are incorrect. --[[User:Rdm|Rdm]] 13:49, 6 March 2011 (UTC)

:: I was wrong about using a blank header file, but one of you on #rosettacode helped me write a definitely working version using the "weak" attribute. Thanks much.

:: My examples could certainly be cleaned up or even patched up. They're currently a work in progress--see the activity at GitHub. https://github.com/mcandre/scriptedmain/commits/master

:: My hope is that once everyone understand the purpose of this article, people will contribute more and more examples until a "scripted main" can be written in almost any language, and those for which scripted main is impossible will be documented so that coders don't waste time trying. --[[User:Mcandre|Mcandre]]

== Is this something like what you want to show? ==

OK I'm still not clear on this, so I've kind of been avoiding it and hoping that everyone else can figure it out. I have a guess for what I think this is trying to show, though. I'll show it in Java:

```java5
package RC.test;
import java.util.Arrays;

public class MainClass{
    public static void main(String[] args){
        System.out.println(Arrays.toString(args));
    }
}
```



```java5
import RC.test.MainClass;
import java.util.Arrays;

public class TestClass{
    public static void main(String[] args){
        MainClass.main({"this", "is", "a", "test"});
    }
}
```

If I would run <code>java MainClass testing 1 2 3</code> it would print:

```txt
testing
1
2
3
```

If I would run <code>java TestClass</code> it would print:


```txt
this
is
a
test
```

So the main method in MainClass is callable from another class if MainClass is imported in TestClass. I'm not sure if that means Java has a scripted main or not because that phrase means nothing to me still. What do you think? --[[User:Mwn3d|Mwn3d]] 21:20, 6 March 2011 (UTC)

:Seems close; but the central idea of what the task is about needs work before anyone can judge. I propose we work first on what the task is about; next, on whethere the idea is general enough to be in RC; then on language implementations. --[[User:Paddy3118|Paddy3118]] 07:01, 7 March 2011 (UTC)

:No, your Java example seems wrong to me. The current task description says to ''"to execute a main() function only when a program is run directly"''. So call MainClass.main() only when running MainClass? Your example is wrong because it also calls MainClass.main() when running TestClass? --[[User:Kernigh|Kernigh]] 02:10, 9 March 2011 (UTC)

:Mwn3d, see the current Java example, "ScriptedMain". The comments above about "library as an executable" are on point. The desired behavior is that the ScriptedMain class runs a main method, Test imports ScriptedMain, and Java does not call ScriptedMain.main() when Test is run. --[[User:Mcandre|Mcandre]]

==What ''is'' an "Executable library"==
''(This section needs expansion and discussion to see if we have enough to create a task that is capable of being completed by enough languages.)'' --[[User:Paddy3118|Paddy3118]] 07:45, 7 March 2011 (UTC)

Since my words only carry so far, someone else wrote a short description of this behavior at [http://stackoverflow.com/questions/1973373/why-does-it-do-this-if-name-main/1973391#1973391 Stack Overflow]. --[[User:Mcandre]]

When given a task in a programming 'contest' such as to create a simple function and to find values of that function at certain points; then you would be giving your example in the form of an executable library if:

'''For an interpreted language:'''
* When the interpreter is called on the source file, the result is the generation of all the values needed for the 'contest'. Although a command-line argument specifying that the library is being directly executed is permitted, an executable library must be able to contain all the code to produce the values for the competition.
* When another source file includes the library source file as a library/module resource, the simple function can then be called, but the code in the library file for producing results specific to the competition, should ''not'' be automatically called at all.

'''For a compiled language:'''

The equivalent would be an executable shared object file (Unix) or executable DLL (Windows).
* When the .so or DLL is called/run/clicked on, the result is the generation of all the values needed for the 'contest'.
* Another executable must be able to access the same .so/DLL file as a resource from which the simple function can then be called, but the code in the .so/DLL file for producing results specific to the competition, should ''not'' be automatically called at all.


### =Examples=

An example would be the [[Hailstone sequence#Python|Python]] entry for [[Hailstone sequence]].

:
```python
def hailstone(n):
    seq = [n]
    while n>1:
        n = 3*n + 1 if n & 1 else n//2
        seq.append(n)
    return seq

if __name__ == '__main__':
    h = hailstone(27)
    assert len(h)==112 and h[:4]==[27, 82, 41, 124] and h[-4:]==[8, 4, 2, 1]
    print("Maximum length %i was found for hailstone(%i) for numbers <100,000" %
          max((len(hailstone(i)), i) for i in range(1,100000)))
```


In the case of the Python language the interpreter maintains a module level variable called __name__. If a file hailstone.py is ''imported'' (as <code>import hailstone</code>), then the __name__ variable is set to the import name of 'hailstone' and the <code>if __name__ == '__main__'</code> expression would then be false, and only the hailstone function is available to the importer.

If the same file hailstone.py is ''run'', (as maybe <code>python hailstone.py</code>; or maybe double-clicking the hailstone.py file), then the __name__ variable is set to the special name of '__main__' and the <code>if __name__ == '__main__'</code> expression would then be true causing its block of code to be executed. 

--[[User:Paddy3118|Paddy3118]] 07:45, 7 March 2011 (UTC)


: Perfect example Paddy! Is Rosetta Code's main purpose to help coders in programming contests, or are you just framing the article with that mindset for easier reading? --[[User:Mcandre|Mcandre]]

:: Hi Mcandre, I was 'framing' with an eye to maybe rewriting the task goals in a manner that was easier for more people to understand. I'm glad that we are on the same wavelength so far on the goals, but as a task description, the above has holes:
::#  It needs to specify a task for the program that is importing the library to do. Maybe: ''"Use the libraries hailstone function, in the standard manner, (or document how this use deviates from standard use of a library); to find the hailstone length returned most often for 1 <= n < 100,000"''
::#  It needs a solution in another language
::#  It would be good, but probably not essential to get a solution in another non-scripting language too.

:: '''Here is more to flesh out an initial Python solution:'''
::
```python
from collections import Counter
from executable_hailstone_library import hailstone

def hailstone_length_frequency(hrange):
    return Counter(len(hailstone(n)) for n in hrange).most_common()

if __name__ == '__main__':
    upto = 100000
    hlen, freq = hailstone_length_frequency(range(1, upto))[0]
    print("The length of hailstone sequence that is most common for\n"
          "hailstone(n) where 1<=n<%i, is %i. It occurs %i times."
          % (upto, hlen, freq))
```


::'''Sample output''':
::<code>The length of hailstone sequence that is most common for</code>
::<code>hailstone(n) where 1<=n<100000, is 72. It occurs 1467 times</code>

:: '''So, who's for the change!''' --[[User:Paddy3118|Paddy3118]] 08:15, 13 March 2011 (UTC)

::: Good stuff. My only concern with hailstone vs meaning of life is that the hailstone sequence may warrant its own article, the duplication may confuse both hailstone geeks and scripting geeks, and the meaning of life snippet is much simpler to read, write, and understand. One caveat I can think of for the meaning of life example is that it fails to demonstrate export of a useful function ("return 42" is not a useful function). The function must be useful enough that the reader understands why it's worthing coding as both a command line script and as an API, yet useless enough that it's not duplicating another article. If you're set on using the hailstone sequence, feel free to borrow code from [https://github.com/mcandre/cspace My Github] --[[User:Mcandre|Mcandre]]

:::: Hi Mcandre, the new task [[Executable library]] allows people to copy code from the pre-existing [[Hailstone sequence]] task. Check it out. --[[User:Paddy3118|Paddy3118]] 10:25, 4 April 2011 (UTC)

====On fleshing-out a rewrite====
I'll check-back over the next 12 hours and, with enough support of the points I made, move and rewrite the task.

(Thanks for the Ruby support)! --[[User:Paddy3118|Paddy3118]] 05:18, 14 March 2011 (UTC)
 
=====[[Ruby]]=====

Yes, I guess that it would be a good idea to change all the examples to hailstone libraries. Here is [[Ruby]] code. Might require Ruby 1.9.


```ruby
# hailstone.rb
module Hailstone
  def sequence(n)
    seq = [n]
    seq << (n = if n.even? then n / 2 else n * 3 + 1 end) until n == 1
    seq
  end
  module_function :sequence

  if __FILE__ == $0
    big_hs = Enumerator.new do |y|
      (1...100_000).each { |n| y << sequence(n) }
    end.max_by { |hs| hs.size }
    puts "#{big_hs[0]} has a hailstone sequence length of #{big_hs.size}."
    puts "The largest number in that sequence is #{big_hs.max}."
  end
end
```



```ruby
# hsfreq.rb
require 'hailstone'

module Hailstone
  def most_common_length(enum)
    h = Hash.new(0)
    enum.each { |n| h[sequence(n).length] += 1 }
    h.max_by { |length, count| count }
  end
  module_function :most_common_length

  if __FILE__ == $0
    last = 99_999
    length, count = most_common_length(1..last)
    puts "Given the hailstone sequences for 1 to #{last},"
    puts "the most common sequence length is #{length},"
    puts "with #{count} such sequences."
  end
end
```



```txt
$ ruby19 hailstone.rb                                                          
77031 has a hailstone sequence length of 351.
The largest number in that sequence is 21933016.
$ ruby19 -I. hsfreq.rb                                                         
Given the hailstone sequences for 1 to 99999,
the most common sequence length is 72,
with 1467 such sequences.
```


--[[User:Kernigh|Kernigh]] 04:16, 14 March 2011 (UTC)

== New goals that could possibly be integrated into this ==

Some languages (like gambas and visual basic) support two startup modes. Applications written on these platforms start with an open window that waits for events, and it is necessary to do some jiggerypokery to cause a main procedure to run instead. I was going to create a new task to demonstrate how to achieve this, and then I remembered this "scripted main". Maybe this can be reorganized to include these requirements? It is not necessarily a "scripted" main, because visual basic is compiled, so we would probably need a rename, though I have not decided on a name for this yet, I am open to ideas. [[User:Markhobley|Markhobley]] 21:58, 11 July 2011 (UTC)
:FWIW in documentation that I have written, this is usually headed "Making the application startup from a main routine". [[User:Markhobley|Markhobley]] 22:08, 11 July 2011 (UTC)

:I would vote to start a new draft task for new topics as this is marked for a slow, lingering, death :-)
:I was wondering however, in your proposed task? Is it about, for example, clicking on a script file in windows to get the script to run versus clicking on the executable to get a [[wp:REPL|REPL]] to run on Windows? --[[User:Paddy3118|Paddy3118]] 06:34, 12 July 2011 (UTC)
::No. The jiggerypokery is done at project build time, not at run time invocation, so that would be a separate task. [[User:Markhobley|Markhobley]] 07:30, 12 July 2011 (UTC)
