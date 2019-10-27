+++
title = "Rosetta Code:Village Pump/Ideas for 2010"
description = ""
date = 2010-11-28T17:35:25Z
aliases = []
[extra]
id = 5262
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Ideas for 2010
|summary=Various user ideas for 2010
}}
Ideas for 2010? Mention them in here. Discussions can be moved into their own pages if they get large. --[[User:Short Circuit|Michael Mol]] 06:01, 1 January 2010 (UTC)
==Michael's Ideas==
* I'd like to see more examples taking advantages of libraries accessible to them. --[[User:Short Circuit|Michael Mol]] 06:01, 1 January 2010 (UTC)
* I'd like to see more examples which use libraries show how to implement the functionality in their own code. --[[User:Short Circuit|Michael Mol]] 06:01, 1 January 2010 (UTC)
* I'd like to see the Encyclopedia pages get split into their own realm, and get more programming-relevant content that tends to escape Wikipedia for reasons of notability or origin. --[[User:Short Circuit|Michael Mol]] 06:01, 1 January 2010 (UTC)
* I want to set up the ability for people to order books based on content selection rules, but that's going to depend on further increasing the quality and organization of the site. --[[User:Short Circuit|Michael Mol]] 06:01, 1 January 2010 (UTC)
* I want to sell things T-Shirts, jackets and hats. --[[User:Short Circuit|Michael Mol]] 06:01, 1 January 2010 (UTC)
::other thing that could be sold: mousepads
:: See [[Rosetta Code:Village Pump/Sales and stuff]] --[[User:Short Circuit|Michael Mol]] 02:28, 25 January 2010 (UTC)
* I'd like to get the community even more active, without people feeling forced to do this or that. --[[User:Short Circuit|Michael Mol]] 06:01, 1 January 2010 (UTC)
* I'd like to improve communications with things like an XMPP server, XMPP MUCs and a convenient in-wiki chat box. (akin to everything2's catbox, I suppose...) --[[User:Short Circuit|Michael Mol]] 06:01, 1 January 2010 (UTC)
* I'd like to see more tasks covering more domains, such as concurrency, networking, GUIs and agents/AI. --[[User:Short Circuit|Michael Mol]] 06:01, 1 January 2010 (UTC)

==Extracted Comparisons==
I am not sure if the Media-Wiki structure would make this straightforward, but I would like to choose from one to N languages and from one to M tasks and extract an easy comparison table for the entries for those languages on just those tasks. --[[User:Paddy3118|Paddy3118]] 16:58, 2 January 2010 (UTC)

:Or be able to specify a set of languages you are interested when viewing any tasks for a particular session. 
:I'm hoping this can be supported as well. --[[User:Rldrenth|Rldrenth]] 17:29, 2 January 2010 (UTC)

: Straightforward? No.  But that hasn't stopped us in the past. I created the "Example" namespace specifically for this; Examples can then be subsequently transcluded. A few problems still remain:

:* There needs to be an edit link on the transclusion target edits the page on the Example namespace. Could be done with templates.
:* Pages within the Example namespace need to be able to be created from within a page they would be transcluded into, akin to 'create new section' behavior. May require some database foo and some other extension magic.
:* With the potential for an example to be included into multiple pages, it may be viewed under contexts with different goals, and so edited with differing intent. Examples thus need to be able to be forked, with page properties and edit history preserved. *Definitely* requires some database foo and extension magic. --[[User:Short Circuit|Michael Mol]] 18:05, 2 January 2010 (UTC)

:An unsophisticated and fragile but very easy way to do this would be to use regexes to to extract the appropriate sections from the wikitext of the appropriate pages. If anybody's interested, I could write a program that did this. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 19:10, 2 January 2010 (UTC)
::I too was thinking on similar lines. --[[User:Paddy3118|Paddy3118]] 03:57, 3 January 2010 (UTC)

I had hoped for more discussion on whether this was a good idea or not. One of my motivations was that when I am comparing languages and tasks I don't like flipping between tabs and then I am never interested in comparing all the language examples.

I would like to have this as a ditch-able feature. If it costs too much to compute, then ditch it. --[[User:Paddy3118|Paddy3118]] 03:57, 3 January 2010 (UTC)

: I've been ''wanting'' to be able to do what you're describing for a couple years. I very much like the idea, myself. --[[User:Short Circuit|Michael Mol]] 01:48, 25 January 2010 (UTC)

:That's cool. It's a similar idea to [http://tactileint.com/cw/index.php/Main_Page CrossWise]. --[[Special:Contributions/164.67.235.148|164.67.235.148]] 04:35, 25 March 2010 (UTC)

===Mock-up===
I am not sure that the RC mediaWiki Tables are as flexible as the Wikipedia ones, but I have tried to do an example below.

What I would like to do is:
#  Control the width of each column
#  have each cell contents scroll for long lines or too many lines.
#  Better delineation of cell boundaries when compared to lang/pre boxes.
#  ...

Note: [[wp:Help:Table|This]] was very helpful in formatting this table, but I am not sure if I would need to generate raw HTML or markup.

{| class="wikitable" border="4"
|-
!  
!  [[Python]]
!  [[Java]]
|-
!  [[Yuletide Holiday]]
|  bgcolor="#cef2e0" | 
```python
import datetime

def yuletide():
   sunday = 6
   days = (day.strftime('%d %b %Y') for day in (datetime.date(year, 12, 25) 
      for year in range(2008,2122)) if day.weekday() == sunday)
   print '\n'.join(days)

yuletide()
```
Output:

```txt
25 Dec 2011
25 Dec 2016
25 Dec 2022
25 Dec 2033
25 Dec 2039
25 Dec 2044
25 Dec 2050
25 Dec 2061
25 Dec 2067
25 Dec 2072
25 Dec 2078
25 Dec 2089
25 Dec 2095
25 Dec 2101
25 Dec 2107
25 Dec 2112
25 Dec 2118
```


|  bgcolor="#f2e0ce" | 
```java
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

public class Yuletide{
	public static void main(String[] args) {
		for(int i = 2008;i<=2121;i++){
			Calendar cal = new GregorianCalendar(i, Calendar.DECEMBER,
					25);
			if(cal.get(Calendar.DAY_OF_WEEK)==Calendar.SUNDAY){
				System.out.println(cal.getTime());
			}
		}
	}
}
```

Output:

```txt
Sun Dec 25 00:00:00 CST 2011
Sun Dec 25 00:00:00 CST 2016
Sun Dec 25 00:00:00 CST 2022
Sun Dec 25 00:00:00 CST 2033
Sun Dec 25 00:00:00 CST 2039
Sun Dec 25 00:00:00 CST 2044
Sun Dec 25 00:00:00 CST 2050
Sun Dec 25 00:00:00 CST 2061
Sun Dec 25 00:00:00 CST 2067
Sun Dec 25 00:00:00 CST 2072
Sun Dec 25 00:00:00 CST 2078
Sun Dec 25 00:00:00 CST 2089
Sun Dec 25 00:00:00 CST 2095
Sun Dec 25 00:00:00 CST 2101
Sun Dec 25 00:00:00 CST 2107
Sun Dec 25 00:00:00 CST 2112
Sun Dec 25 00:00:00 CST 2118
```


|-
!  [[Ethiopian Multiplication]]
|  bgcolor="#cef2e0" | 
```python
tutor = True

def halve(x):
    return x//2

def double(x):
    return x*2

def even(x):
    return not x % 2

def ethiopian(multiplier, multiplicand):
    if tutor:
        print( "Ethiopian multiplication of %i and %i" %
               (multiplier, multiplicand) )
    result = 0
    while multiplier >= 1:
        if even(multiplier):
            if tutor: print( "%4i %6i STRUCK" %
                             (multiplier, multiplicand) )
        else:
            if tutor: print( "%4i %6i KEPT" %
                             (multiplier, multiplicand) )
            result += multiplicand
        multiplier = halve(multiplier)
        multiplicand = double(multiplicand)
    if tutor: print()
    return result
```


Sample output

```txt
Python 3.1 (r31:73574, Jun 26 2009, 20:21:35) [MSC v.1500 32 bit (Intel)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> ethiopian(17, 34)
Ethiopian multiplication of 17 and 34
  17     34 KEPT
   8     68 STRUCK
   4    136 STRUCK
   2    272 STRUCK
   1    544 KEPT

578
>>> 
```


Without the tutorial code, and taking advantage of Python's lambda:


```python
halve = lambda x: x // 2
double = lambda x: x*2
even = lambda x : not x % 2
 
def ethiopian(multiplier, multiplicand):
    result = 0

    while multiplier >= 1:
        if not even(multiplier):
            result += multiplicand
        multiplier = halve(multiplier)
        multiplicand = double(multiplicand)

    return result
```


|  bgcolor="#f2e0ce" | {{works with|Java|1.5+}}

```java5
import java.util.HashMap;
import java.util.Scanner;
public class Mult {
  public static void main(String[] args){
    Scanner sc = new Scanner(System.in);
    int first = sc.nextInt();
    int second = sc.nextInt();

    HashMap <Integer, Integer> columns = new HashMap <Integer, Integer>();
    columns.put(first, second);
    do{
      first = doubleInt(first);
      second = halveInt(second);
      columns.put(first, second);
    }while(first != 1);

    int sum = 0;
    for(int firstNum:columns.keySet()){
      if(!isEven(firstNum)){
        sum += columns.get(firstNum);
      }
    }
    System.out.println(sum);
  }

  public static int doubleInt(int doubleMe){
    return doubleMe << 1; //shift left
  }

  public static int halveInt(int halveMe){
    return halveMe >>> 1; //shift right
  }

  public static boolean isEven(int num){
    return num & 1 == 0;
  }
}
```

An optimised variant using the three helper functions from the other example.

```java5
/**
 * This method will use ethiopian styled multiplication.
 * @param a Any non-negative integer.
 * @param b Any integer.
 * @result a multiplied by b
 */
public static int ethiopianMultiply(int a, int b) {
  if(a==0 || b==0) {
    return 0;
  }
  int result = 0;
  while(a>=1) {
    if(!isEven(a)) {
      result+=b;
    }
    b = doubleInt(b);
    a = halveInt(a);
  }
  return result;
}

/**
 * This method is an improved version that will use
 * ethiopian styled multiplication, but can fully
 * support negative parameters.
 * @param a Any integer.
 * @param b Any integer.
 * @result a multiplied by b
 */
public static int ethiopianMultiplyWithImprovement(int a, int b) {
  if(a==0 || b==0) {
    return 0;
  }
  if(a<0) {
    a=-a;
    b=-b;
  } else if(b>0 && a>b) {
    int tmp = a;
    a = b;
    b = tmp;
  }
  int result = 0;
  while(a>=1) {
    if(!isEven(a)) {
      result+=b;
    }
    b = doubleInt(b);
    a = halveInt(a);
  }
  return result;
}
```


|}

: If you have access to a MediaWiki install, consider putting together an extension that builds it based on categories, similar to how the MCS extension works. Opticron has some experience in this area.  As far as complexity goes, I don't know whether you'd want to build the code by hand, or transclude a template per example. --[[User:Short Circuit|Michael Mol]] 08:31, 3 January 2010 (UTC)

:: Now i know the limits of my knowledge. There is so much of your comment that I could not follow, as I don't at present delve into mediawiki, I just use it. :-)
 
:: --[[User:Paddy3118|Paddy3118]] 11:20, 3 January 2010 (UTC)
::: I'm not that great at communication, and I wrote that in a rush.  I'll have to come back and explain more clearly later. --[[User:Short Circuit|Michael Mol]] 12:17, 3 January 2010 (UTC)

==Selling T-Shirts==
Realizing that a way is needed to offset costs of supporting this site, I'm fine with this idea. You would
probably have a good deal of success selling books on various programming languages as well.  I'm wondering
if publishers like Manning, Apress, O'Reilly, or Pragmatic would have any interest. --[[User:Rldrenth|Rldrenth]] 17:25, 2 January 2010 (UTC)
:For the T-shirts it's just been a matter of having the time to set it up.  I also had the idea that the T-shirt selection could change every month, based on the language selection rankings. --[[User:Short Circuit|Michael Mol]] 18:09, 2 January 2010 (UTC)
:For books, I've been wanting to do this for a ''long'' time, and my thoughts on the subject relate to the extracted examples above. I've been trying to find the time to implement it over the last three months. --[[User:Short Circuit|Michael Mol]] 18:09, 2 January 2010 (UTC)
::So what you're looking at is a program that would construct a custom book based on the customer's selecton of languages and tasks that would contain the Rosetta Code examples. With an index and a table of contents as well, I'm sure.  That's interesting. --[[User:Rldrenth|Rldrenth]] 18:40, 2 January 2010 (UTC)
::: That's pretty much exactly what I'd like to see.  My desired approach would be to take an order for the construction of the book, push it through a self-publisher like Lulu, and then make the PDF and LaTeX versions (It ''is'' GFDL content...) available online. I'd like to avoid hosting the PDFs without a sale of some sort, though, as then I'm just asking for my bandiwdth consumption to explode. I'd hope that such a system could be useful for textbooks. --[[User:Short Circuit|Michael Mol]] 21:32, 2 January 2010 (UTC)

==More OpenGL related Tasks==
* I would like to see more OpenGL tasks. It would be important to not fall into the trap of illustrating OpenGL features, because the examples would be very close translations. Instead we could focus on how to use OpenGL to do various things with it. [[User:Blue Prawn|Blue Prawn]] 20:01, 2 January 2010 (UTC)
** For example one idea I got is to load datas that both contain geometry informations and animation informations. Resolving the task should focus on the timeline data structure. For such a task we could use a mix of X3D and Smil, here is a draft: [[OpenGL/Loading Animated Data]], don't hesitate to modify it, or delete it, it's just a draft. What do you think about it? [[User:Blue Prawn|Blue Prawn]] 20:01, 2 January 2010 (UTC)
**: I think I'd want to find a library to do all the heavy lifting for me before taking that one on. It's formidable to do from first principles! –[[User:Dkf|Donal Fellows]] 21:23, 2 January 2010 (UTC)
* Having more 3D-targeted tasks would be wonderful, though I'd like the tasks to be generalized (where possible) to allow for implementations with other APIs, such as SDL and DirectX. --[[User:Short Circuit|Michael Mol]] 21:27, 2 January 2010 (UTC)
** SDL is for windowing, using Glut or SDL won't change a lot, but why not. Probably the best place to show the windowing alternative methods would be in the base [[OpenGL]] page. Once this is done in the base page, it's not a big deal to use Glut, SDL, or another windowing interface. [[User:Blue Prawn|Blue Prawn]] 21:45, 2 January 2010 (UTC)
*** Ah.  It was my impression that SDL had inherent support for 3D primitives. --[[User:Short Circuit|Michael Mol]] 02:33, 3 January 2010 (UTC)
** About DirectX, as long as I know it's not very portable, isn't it a MS/Windows only library? This said, the draft task [[OpenGL/Loading Animated Data]] could be solved with DirectX in a very similar way, while this task is centered on the datas and the timeline. We could perhaps rename [[OpenGL/Loading Animated Data]] to [[Loading Animated 3D Data]] --[[User:Blue Prawn|Blue Prawn]] 21:45, 2 January 2010 (UTC)
*** I'd be in favor of renaming to [[Loading Animated 3D Data]].  Also, if nobody has yet done so, [[Template:Draft task]] should be added.
*** Just as C and Fortran are two programming languages that address general-purpose programming tasks, Direct3D and OpenGL are both 3D graphics APIs that address managing 3D rendering and acceleration hardware, and you might even throw in POVRay and Maya scene description code in the mix for some of the tasks. Just as comparison by language helps an observer transfer a skill set across a language boundary (as well as judge language suitability for their purposes), comparison by library helps an observer transfer a skill set involving that library's domain from one library to another. A similar set of comparisons might be drawn betweek Gtk+, Qt, wxWidgets and Win32. My intent for site organization is to maintain a maximal degree of comparative genericity.  [[:Category:Solutions_by_Library]] is in the navigation sidebar, with that goal in mind.
*** Focusing on tool portability isn't a principle component of the site; If it were, I'd have to argue against the inclusion of languages without portable implementations (meaning at least the Big Three of Windows, UNIX-like and Mac), and that runs counter to the utility of comparison by example.
*** Does that make sense, or did I just wind up rambling?--[[User:Short Circuit|Michael Mol]] 02:33, 3 January 2010 (UTC)
*** OK, so I have renamed the page [[OpenGL/Loading Animated Data]] to [[Loading Animated 3D Data]] and in the text I have made the replacement /with OpenGL/with OpenGL (or something else)/ -- [[User:Blue Prawn|Blue Prawn]] 19:39, 3 January 2010 (UTC)
* It'd be nice if there was something to show off loading a height map or something like that. My (very vague) idea is that it's very often useful to load a landscape defined as a grid of points and then render that as a net of triangles. That's the foundation for a lot of real world use of 3D engines. –[[User:Dkf|Donal Fellows]] 20:43, 3 January 2010 (UTC)

== advertisements? ==

I haven't been involved in the discussions of generating revenue, but has selling advertising been discussed?  I personally would have no problem with the addition of ads to RC, particularly if they're textual (eg Google's Adwords), or even just relevant (eg O'Reilly media).  But please, no animated advertisements (I'd just use an ad blocker, which would defeat the purpose of adding advertisements in the first place.)

--[[User:DanBron|DanBron]] 16:43, 4 January 2010 (UTC)
: I personally detest ads on websites, which is a major reason I haven't put them on.  However, I'd probably be willing to enable them for anonymous users, but turn them off when people log in.  Textual ads aren't so bad; I don't have to worry so much about someone throwing in a flash applet that covers half the page. --[[User:Short Circuit|Michael Mol]] 17:58, 4 January 2010 (UTC)

:: Well, Google Adsense has been in place for a while, but it's anyone's guess why they haven't shown anything yet. I may need to do some debugging. If it's just Google not being able to figure out what's relevant, I'll drop it, and possibly switch to Amazon (or nothing at all, if other sources work.) --[[User:Short Circuit|Michael Mol]] 01:46, 25 January 2010 (UTC)
