+++
title = "Talk:Binary strings"
description = ""
date = 2012-09-18T21:30:33Z
aliases = []
[extra]
id = 4072
[taxonomies]
categories = []
tags = []
+++

== Break out? ==

I think this task should be broken out into smaller tasks and then put into a category. [[String concatenation]] is already a task, so it would be grouped with these tasks. --[[User:Mwn3d|Mwn3d]] 18:04, 14 April 2009 (UTC)

: Maybe is a good idea... But a path similar to [[Basic bitmap storage]] should be begun. I mean, the struct String is shared among all the tasks... so we need also a task like "provide a basic storage for a (binary) string", so that next tasks can refer to it instead of replicating the struct, or linking to where it is defined since it is needed by a specific function, e.g. see "String concatenation" for struct... --[[User:ShinTakezou|ShinTakezou]] 11:51, 15 April 2009 (UTC)

: Then I've taken a look at those tasks and they do not focus on the concept of "byte strings", rather they refer to text strings. This is an ''issue'' if the text string implementation uses a terminator character, like C; and in fact the C solutions to those tasks ([[Copy a string]], [[String concatenation]], [[String length]]) work only for null-terminated string (i.e. "null" char can't be part of the string). (Of course this does not happen in every languages; but C is among those having this "problem"). I think it is enough to add some more C code to those tasks... '''Or''' maybe I gave the wrong name, should it be "Basic binary string manipulation functions"? (binary or according to Wikipedia bytestring) --[[User:ShinTakezou|ShinTakezou]] 15:23, 15 April 2009 (UTC)
::You're in a better position to figure that out than I am. I don't think I was ever really clear on byte strings and binary strings (probably because most of the string work I've done is in Java where most of the details are hidden or irrelevant). --[[User:Mwn3d|Mwn3d]] 19:58, 15 April 2009 (UTC)
::: There's nothing but a conventional distinction (but the following Java example says that after all it can be not so conventional after all...). Generally a string is just a sequence of "symbols" (bytes), even text are made of bytes of course... The distinction just stresses the fact that the bytes can be interpreted as text (according to which encoding...?) and are not generic binary data. Strings are not exactly "binary safe" in Java, but there's no terminator in use:


```java
public class binsafe {
  public static void main(String[] args) {
    System.out.print("\000\000test\001\377");
  }
}
```


::: Outputs


```txt
$ java -cp . binsafe |hexdump -C
00000000  00 00 74 65 73 74 01 c3  bf                       |..test...|
00000009
```


::: Which looks odd since the byte 255 (octal 377) is oddly UTF-8 encoded, infact


```txt
$ printf "\xc3\xbf" |iconv -f utf-8 -t latin1 |hexdump -C
00000000  ff                                                |.|
00000001
```


::: Maybe there's a method in the String class that says Java not to "interpret" the string, or maybe such a task in Java should be accomplished using a custom class innerly using byte[]. --[[User:ShinTakezou|ShinTakezou]] 21:53, 15 April 2009 (UTC)
::::Java has a String method <tt>toByteArray()</tt>, but I think the actual tasks need to be customized. --[[User:Mwn3d|Mwn3d]] 22:14, 15 April 2009 (UTC)

== Comment: PureBasic does not have real binary strings ==

I would like to comment on the given PureBasic code for binary strings.

The example code using variables with a $ postfix and string literals
enclosed in double quotes (") does use normal PureBasic strings.

Those PureBasic strings are 0 terminated.
They may not contain Chr(0)
and therefore are not binary safe.

If you want to handle "strings made of arbitrary bytes" in PureBasic,
you could use memory buffers or byte arrays,
but the code required for handling such,
is more evolved, than I am prepared to produce at this moment.

== Alternative implementation ==

"If your language of choice does have this built-in support, show a possible alternative implementation for the functions or abilities already provided by the language."

For a language that has no limitations on byte values within a byte string, reimplementing something so primitive would be madness, and thankfully, no one yet has taken it seriously.  Reimplement without using bytes?  without using arrays?  Or, use existing language features, but pointlessly wrap them in functions with more cumbersome syntax?  As worded, I can't imagine the task as anything but an exercise in obfuscation.  &mdash;[[User:Sonia|Sonia]] 00:29, 14 February 2011 (UTC)

:Different mood today.  I posted a solution.  &mdash;[[User:Sonia|Sonia]] 21:30, 18 September 2012 (UTC)
