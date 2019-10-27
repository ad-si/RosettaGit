+++
title = "Talk:XML/Output"
description = ""
date = 2013-12-09T09:05:46Z
aliases = []
[extra]
id = 3278
[taxonomies]
categories = []
tags = []
+++

You need to separate the task description fully from the example so others can follow the description alone. --[[User:Paddy3118|Paddy3118]] 19:43, 28 December 2008 (UTC)

How about extending the task by adding a 'special' name that needs escaping, such as '<None>', and ask for sample output. You would then force entries to handle escaped characters. --[[User:Paddy3118|Paddy3118]] 05:15, 8 June 2009 (UTC)
: On one level that'd be good, but on another it's not so good. (XML docs aren't supposed to contain magic flag values in text nodes; that's what using a different elements is for...) —[[User:Dkf|Donal Fellows]] 09:00, 8 June 2009 (UTC)
:: Why not simply have a name in the list which includes something with a named entity, like the German "Jürgen" ("J&amp;uuml;rgen") or the french "André" ("Andr&amp;eacute;")? --[[User:Ce|Ce]] 09:18, 8 June 2009 (UTC)
::: Because those don't ''need'' to be encoded; the data stream might be UTF-8 (which is actually the default for XML...) For the quoting to work, we'd need to force the inclusion of each of <code><>'"&amp;</code> (those are the only ones that XML requires handling, and even the quoting characters only need doing in some circumstances; it's really messy) and those characters don't look like first names. (The set of things you're supposed to quote is larger in HTML.) —[[User:Dkf|Donal Fellows]] 10:47, 8 June 2009 (UTC)
:::: Of course, the task description contains: "If attempting this task by direct string manipulation, the implementation ''must'' include code to perform entity substitution for the characters that have entities defined in the XML 1.0 specification." That is, the task says, encode whereever you ''can'', not wherever you ''must''. --[[User:Ce|Ce]] 11:54, 8 June 2009 (UTC)

== What is the point? ==

If the task requires creating some fixed output, the easiest and most effective way would be just to insert the required contents in the file. That has very little to do with real world problems.

It would make more sense if the task would be for example to read list of names from a text file and use it to create an XML file with specified structure. In that case it would make sense to to require converting special characters into entities, since such special characters might exist in the input file. In order to compare the output, you could then have an example input file. --[[User:PauliKL|PauliKL]] 11:48, 8 June 2009 (UTC)
: Instead of reading from a file, just get it from standard input (or more generally, "from the user"). That way, the task doesn't get cluttered with file system stuff unrelated to this task. --[[User:Ce|Ce]] 11:54, 8 June 2009 (UTC)

==Modification of Task==
I suggest the task be changed to adress some of the issues discussed above. I would suggest the following

----
Create a function that takes a list of character names and a list of corresponding remarks and returns an XML doccument of <code><Character></code> elements each with a name attributes and each enclosing its remarks. All <code><Character></code> elements are to be enclosed in turn, in an outer <code><CharacterRemarks></code> element. 

As an example, calling the function with the three names of: 

```txt

April
Tam O'Shanter
Emily
```

And three remarks of:

```txt

Bubbly: I'm > Tam and <= Emily
Burns: "When chapman billies leave the street ..."
Short & shrift
```

Should produce the XML (but not necessarily with the indentation):

```txt
<CharacterRemarks>
    <Character name="April">Bubbly: I'm &amp;gt; Tam and &amp;lt;= Emily</Character>
    <Character name="Tam O&amp;apos;Shanter"
            >Burns: &amp;quot;When chapman billies leave the street ...&amp;quot;</Character>
    <Character name="Emily">Short &amp;amp; shrift</Character>
</CharacterRemarks>
```


Note: the example is chosen to also show correct escaping of XML strings
Note too that although the task is written to take two lists of corresponding  data, a single mapping/hash/dicctionary of nammes to remarks is also acceptable

----


--[[User:Paddy3118|Paddy3118]] 07:14, 9 June 2009 (UTC)

:The suggested task description is not quite clear to a non-English speaking person like me. Maybe splitting it into shorter sentences would help. Anyway, I think I got the idea from the examples. But I think the example input should not contain language specific details such as the \ escape character. And not the quotes (string delimiters) either, if they are not supposed to be part of the input text. --[[User:PauliKL|PauliKL]] 09:42, 9 June 2009 (UTC)

::Hi Paul, I'll work on the description, but on the use of \: I couldn't think of a way to include a quote in a quoted string without it - I am open to sugestions though. --[[User:Paddy3118|Paddy3118]] 11:11, 9 June 2009 (UTC)
:::That's the point when I use UNICODE quotes. Thankfully they're easy to type on this keyboard. —[[User:Dkf|Donal Fellows]] 12:26, 9 June 2009 (UTC)
::Why quoted strings? Why not just enter the example input as plain text? In real world applications, the data would came from user, or from file, or from stdin, etc. Normally, the text is just plain text, with no quotes or escape characters. (Of course the test code that calls the actual function could use string constants, but that is language and implementation specific.) On the other hand, using two input lists would be quite strange way to enter data in real world. It would be more logical to enter the name and remark together, as a pair. (But I don't quite understand what this "remark" is supposed to be.) --[[User:PauliKL|PauliKL]] 15:27, 9 June 2009 (UTC)
:Hi Pauli, I've changed the example above to be without outer quotes for the input text descriptions and removed the backslash. I have chosen the simplest input format that I thought languages could cope with. The focus is to be the generation of correctly formatted XML. The remark is just a device to allow the example to include characters that should be quoted, as is the apostrophe in Tam O'Shanter. --[[User:Paddy3118|Paddy3118]] 18:08, 9 June 2009 (UTC)
::I've (re)done the Tcl implementation as a single combined list (as that's the natural way to do it with Tcl's iterators). Don't get hung up over the small stuff. Note that serializers might be smart enough to know that they don't need to quote in "Tam O'Shanter"; what's required is that documents be syntactically valid XML with the same infoset as the sample. (Matching the strings completely is harder and would be better saved for a task that specifies a specific normalization form; this is not that task.) —[[User:Dkf|Donal Fellows]] 13:00, 10 June 2009 (UTC)

==Escaping Escapes==
The output of the Python example has characters like > escaped but in a <pre tag it is viewed as the character rather than the escaped version. As all XML escapes start with an ampersand, I replaced the ampersand in the true Python output with the sequence five character sequence '&amp;amp;' and the output on page reflects the textual output of the program. --[[User:Paddy3118|Paddy3118]] 18:40, 9 June 2009 (UTC)

'''Please edit sample output so that preview output looks like program output. Unless you manually escape-the-escapes as above, you will end up seeing the character rather than the escape sequence!''' --[[User:Paddy3118|Paddy3118]] 06:10, 12 June 2009 (UTC)

== Should we underline that... ==

Should we underline that output differs slightly according to the used parser? LibXML used in C, e.g. does not escape the "'" of O'Shanter, nor " (quot); perl package XML::Mini escapes ", but not ' in O'Shanter ... and other parsers, I can see it from the output samples, do escape ' and "... --[[User:ShinTakezou|ShinTakezou]] 15:44, 14 June 2009 (UTC)
:As far as I can [http://www.xml.com/axml/testaxml.htm see], there is some flexibility as to what ''needs'' to be escaped where. --[[User:Paddy3118|Paddy3118]] 16:28, 14 June 2009 (UTC)

== Return or print? ==

The task description says "returns an XML document" but to my (untrained) eye many languages are printing the document. Should we change the description to allow printing? Or ask the languages to change?
