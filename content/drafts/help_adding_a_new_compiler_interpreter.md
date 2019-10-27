+++
title = "Help:Adding a new compiler/interpreter"
description = ""
date = 2007-11-26T01:29:00Z
aliases = []
[extra]
id = 1567
[taxonomies]
categories = []
tags = []
+++

Adding a compiler is very simple.

==Programming Example==

You should probably start by examining the existing [[:Category:Programming Tasks|programming tasks]], and finding one that you can complete with your compiler of choice.


### Creating a New Example

Then you may need to add a programming example to that task.  Find the place in the list of languages already applied where your language would fall, alphabetically.  Then enter something like this:

 <nowiki>
 
## C

 '''Compiler:''' [[gcc]] 4.0.1
 
  int main ( void ) {
       // Some code here
  }</nowiki>

You don't have to use the C programming language, you ''definitely'' don't have to use gcc, and your code example will probably be different.  You need to make sure you create a link to your language of choice, and to your compiler, interpreter, or what-have-you.  To create a link, surround the text with double square brackets.  For example, [[gcc]] would be represented as <nowiki>[[gcc]]</nowiki>.

Finally, note the importance of the extra space in front of the code.  That creates a dashed box around the code sample, and puts it in a monospace font.


### Using an Existing Example


If there's already a programming example written in a language your compiler supports, then all you need to do is ensure that the programming example works as expected in your compiler.  Once you've determined that, you need only add your compiler to the list of compilers that support that code and that programming language.

Assuming you want to add Visual C++ 2005 to the list of compilers that support a programming example written in C++...

 <nowiki>
 
## C++

 '''Compiler:''' [[g++]] 4.0.1
  int main ( void ) {
       // Some code here
  }</nowiki>

...becomes...

 <nowiki>
 
## C++

 '''Compiler:''' [[g++]] 4.0.1, [[Visual C++]] 2005
  int main ( void ) {
       // Some code here
  }</nowiki>

...and there you have it.  There's really only one thing to watch out for, here.  Do ''not'' include the compiler's version number in the link.  A compiler is a product, and compiler pages should be one-to-a-product.

==Compiler or Interpreter Page==

If a programming example which mentions the compiler already exists, creating a compiler page is extremely simple.  Just click on the link to the compiler link, and click "Create this article", on the resulting page.  Add whatever information is appropriate, and end the article with the compiler template.


### Template?


That's right.  There's a template to get compiler and interpreter pages started.  To use it, create your article, and begin it with <nowiki>{{implementation|Langauge}}</nowiki>, and replace "Language" with the language your compiler or interpreter implements.

Click Preview Page, and you can see what your page will look like.  If it still looks somewhat sparse, and you don't have anything else to add, you can add <nowiki>{{stub}}</nowiki> to the page.


Click Save Page, and you'll also see the page added to the Implementations category.

Cool, huh?

==Conclusion==

Thanks for showing an interest in adding information to Rosetta Code.  With the help of people like you, Rosetta Code will become a true programmer's resource!
