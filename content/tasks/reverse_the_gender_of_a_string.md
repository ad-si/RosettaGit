+++
title = "Reverse the gender of a string"
description = ""
date = 2019-10-12T09:21:55Z
aliases = []
[extra]
id = 12356
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "arturo",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "miniscript",
  "objeck",
  "perl",
  "perl_6",
  "phix",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ring",
  "scala",
  "sidef",
  "tcl",
]
+++

The task is to create a function that reverses the gender of the text of a string. The function should take one arguments being a string to undergo the sex change. The returned string should contain this initial string, with all references to gender switched.


```pseudocode
print rev_gender("She was a soul stripper. She took my heart!")
He was a soul stripper. He took my heart!
```



## Arturo


```arturo
reverseGender [str]{
	ret 		str
	entries 	#("She" "she" "Her" "her" "hers" "He" "he" "His" "his" "him")
	repls		#("He_" "he_" "His_" "his_" "his_" "She_" "she_" "Her_" "her_" "her_")

	loop $(range 0 $(size entries)-1) [i]{
		ret $(replace ret "/\b"+entries.[i]+"\b/" repls.[i])
	}

	return $(replace ret "_" "")
}

print $(reverseGender "She was a soul stripper. She took his heart!")
print $(reverseGender "He was a soul stripper. He took her heart!")
print $(reverseGender "She wants what's hers, he wants her and she wants him!")
print $(reverseGender "Her dog belongs to him but his dog is hers!")
```


```txt
He was a soul stripper. He took her heart!
She was a soul stripper. She took his heart!
He wants what's his, she wants his and he wants her!
His dog belongs to her but her dog is his!
```



## FreeBASIC

Although in principle all gender-related words in the dictionary could be swapped, I've only attempted to swap the 3rd person pronouns, possessive pronouns and possessive adjectives here. Even then, without code to understand the context, some swaps are ambiguous - for example 'her' could map to 'his' or 'him' and 'his' could map to 'her' or 'hers'.

To avoid swapping words which have already been swapped, thereby nullifying the original swap, I've appended an underscore to each replacement word and then removed all the underscores when all swaps have been made. This assumes, of course, that the text didn't include any underscores to start with.

```freebasic
' FB 1.05.0 Win64

Function isWordChar(s As String) As Boolean
  Return ("a" <= s AndAlso s <= "z") OrElse ("A" <= s AndAlso s <= "Z") OrElse("0" <= s AndAlso s <= "9") OrElse s = "_"
End Function

Function revGender(s As Const String) As String
  If s = "" Then Return ""
  Dim t As String = s
  Dim word(1 To 10) As String = {"She", "she", "Her",  "her",  "hers", "He",   "he",   "His",  "his",  "him"}
  Dim repl(1 To 10) As String = {"He_", "he_", "His_", "his_" ,"his_", "She_", "she_", "Her_", "her_", "her_"}
  Dim As Integer index, start, after
  For i As Integer = 1 To 10
    start = 1
    While start <= Len(t) - Len(word(i)) + 1
      index = Instr(start, t, word(i))
      If index = 0 Then Exit While
      after = index + Len(word(i))
      If index = 1 AndAlso after <= Len(t) AndAlso CInt(isWordChar(Mid(t, after, 1))) Then
        start = after
        Continue While
      End If
      If index > 1 AndAlso after <= Len(t) AndAlso _
        (CInt(isWordChar(Mid(t, index - 1, 1))) OrElse CInt(isWordChar(Mid(t, after, 1)))) Then
        start = after
        Continue While
      End If
      t = Left(t, index - 1) + repl(i) + Mid(t, after)
      start = index + Len(repl(i))
    Wend
  Next
  ' now remove all underscores
  For i As Integer = Len(t) To 1 Step -1
    If Mid(t, i, 1) = "_" Then
      t = Left(t, i - 1) + Mid(t, i + 1)
    End If
  Next
  Return t
End Function

Print revGender("She was a soul stripper. She took his heart!")
Print revGender("He was a soul stripper. He took her heart!")
Print revGender("She wants what's hers, he wants her and she wants him!")
Print revGender("Her dog belongs to him but his dog is hers!")
Print
Print "Press any key to quit"
Sleep
```


```txt

He was a soul stripper. He took her heart!
She was a soul stripper. She took his heart!
He wants what's his, she wants his and he wants her!
His dog belongs to her but her dog is his!

```



## Go


```go
package main

import (
    "fmt"
    "strings"
)

func reverseGender(s string) string {
    if strings.Contains(s, "She") {
        return strings.Replace(s, "She", "He", -1)
    } else if strings.Contains(s, "He") {
        return strings.Replace(s, "He", "She", -1)
    }
    return s
}

func main() {
    s := "She was a soul stripper. She took my heart!"
    t := reverseGender(s)
    fmt.Println(t)
    fmt.Println(reverseGender(t))
}
```


```txt

He was a soul stripper. He took my heart!
She was a soul stripper. She took my heart!

```



## Haskell


We can optimise the time and space complexity of this computation by careful selection of the source and target languages (not specified in the task description, although the example appears to be drawn from some kind of Anglo-Saxon dialect, which seems a bit sub-optimal for these purposes).

Sino-Tibetan dialects generally work well here. If we choose any standard transcription of Modern Standard Chinese (such as Pinyin or IPA) or more or less any written sample of pre 19c literary Chinese, we can reduce the entire computation down to a very pleasing intersection of fully optimized space and time performance with reasonably clear and succinct code:

```haskell
id
```



## J


Note that we cannot do a good job for the general case of english text using simple rules. For example, consider:

* Give her the book. It is her book.
* Give him the book. It is his book.


For this simple example, to determine whether to change ''her'' to ''him'' or ''his'' we would need a grammatical representation of the surrounding context.

So, for now, we limit ourselves to the simple case specified in the task example, and do not even do all that great of a job there, either:


```J
cheaptrick=: rplc&(;:'She He He She')
```


And, the task example:


```J
   cheaptrick 'She was a soul stripper. She took my heart!'
He was a soul stripper. He took my heart!
   cheaptrick cheaptrick 'She was a soul stripper. She took my heart!'
She was a soul stripper. She took my heart!
```



## Java

```java
public class ReallyLameTranslationOfJ {

    public static void main(String[] args) {
        String s = "She was a soul stripper. She took my heart!";
        System.out.println(cheapTrick(s));
        System.out.println(cheapTrick(cheapTrick(s)));
    }

    static String cheapTrick(String s) {
        if (s.contains("She"))
            return s.replaceAll("She", "He");
        else if(s.contains("He"))
            return s.replaceAll("He", "She");
        return s;
    }
}
```



```txt
He was a soul stripper. He took my heart!
She was a soul stripper. She took my heart!
```



## Julia

```Julia
module ReverseGender

const MARKER = "\0"

const words = "^" .* ["She", "she", "Her",  "her",  "hers", "He",   "he",   "His",  "his",  "him"] .* "\$" .|> Regex
const repls = ["He", "he", "His", "his" ,"his", "She", "she", "Her", "her", "her"] .* MARKER

function reverse(s::AbstractString)
    for (w, r) in zip(words, repls)
        s = replace(s, w => r)
    end
    return replace(s, MARKER => "")
end

end  # module ReverseGender

@show ReverseGender.reverse("She was a soul stripper. She took his heart!")
@show ReverseGender.reverse("He was a soul stripper. He took her heart!")
@show ReverseGender.reverse("She wants what's hers, he wants her and she wants him!")
@show ReverseGender.reverse("Her dog belongs to him but his dog is hers!")
```



## Kotlin

This program uses a similar approach to the FreeBASIC entry:

```scala
// version 1.0.6

fun reverseGender(s: String): String {
    var t = s
    val words = listOf("She", "she", "Her",  "her",  "hers", "He",   "he",   "His",  "his",  "him")
    val repls = listOf("He_", "he_", "His_", "his_" ,"his_", "She_", "she_", "Her_", "her_", "her_")
    for (i in 0 until words.size) {
        val r = Regex("""\b${words[i]}\b""")
        t = t.replace(r, repls[i])
    }
    return t.replace("_", "")
}

fun main(args: Array<String>) {
    println(reverseGender("She was a soul stripper. She took his heart!"))
    println(reverseGender("He was a soul stripper. He took her heart!"))
    println(reverseGender("She wants what's hers, he wants her and she wants him!"))
    println(reverseGender("Her dog belongs to him but his dog is hers!"))
}
```


```txt

He was a soul stripper. He took her heart!
She was a soul stripper. She took his heart!
He wants what's his, she wants his and he wants her!
His dog belongs to her but her dog is his!

```



## MiniScript


```MiniScript
cap = function(w)   // (capitalize a word)
    return w[0].upper + w[1:]
end function

trans = {"she":"he", "her":"his", "hers":"his"}
trans = trans + {"he":"she", "his":"her", "him":"her"}

for k in trans.indexes
    trans[cap(k)] = cap(trans[k])
end for

reverseGender = function(s)
    s = s.replace(".", " .").replace(",", " ,").replace("?", " ?").replace("!", " !")
    words = s.split
    for i in words.indexes
        if trans.hasIndex(words[i]) then words[i] = trans[words[i]]
    end for
    s = words.join
    s = s.replace(" .", ".").replace(" ,", ",").replace(" ?", "?").replace(" !", "!")
    return s
end function

test = function(s)
    print "BEFORE: " + s
    print "AFTER:  " + reverseGender(s)
    print
end function

test "She was a soul stripper. She took his heart!"
test "He was a soul stripper. He took her heart!"
test "She wants what's hers, he wants her and she wants him!"
test "Her dog belongs to him but his dog is hers!"
```


```txt
BEFORE: She was a soul stripper. She took his heart!
AFTER: He was a soul stripper. He took her heart!

BEFORE: He was a soul stripper. He took her heart!
AFTER: She was a soul stripper. She took his heart!

BEFORE: She wants what's hers, he wants her and she wants him!
AFTER: He wants what's his, she wants his and he wants her!

BEFORE: Her dog belongs to him but his dog is hers!
AFTER: His dog belongs to her but her dog is his!
```



## Objeck

```objeck
class ReallyLameTranslationOfJ {
  function : Main(args : String[]) ~ Nil {
    s := "She was a soul stripper. She took my heart!";
    CheapTrick(s)->PrintLine();
    CheapTrick(CheapTrick(s))->PrintLine();
  }

  function : CheapTrick(s : String) ~ String {
    if(s->Has("She")) {
      return s->ReplaceAll("She", "He");
    }
    else if(s->Has("He")) {
      return s->ReplaceAll("He", "She");
    };

    return s;
  }
}

```


Output:

```txt

He was a soul stripper. He took my heart!
She was a soul stripper. She took my heart!

```



## Perl

A minimal implementation, using a hash to manage swaps. But this approach breaks down if, for instance, 'him' were to replace 'me', as 'her' can't be used to map to both 'his' and 'him'.

```perl
my %swaps = (
    'she'  => 'he',
    'his'  => 'her',
);

$swaps{         $swaps{$_} } =         $_ for keys %swaps; # inverted pairs
$swaps{ ucfirst $swaps{$_} } = ucfirst $_ for keys %swaps; # title-case version

sub gender_swap {
    my($s) = @_;
    $s =~ s/\b$_\b/_$swaps{$_}/g for keys %swaps; # swap and add guard character
    $s =~ s/_//g;                                 # remove guard
    $s;
}

$original = qq{She was this soul sherpa. She took his heart! They say she's going to put me on a shelf.\n};
print $swapped  = gender_swap($original);
print $reverted = gender_swap($swapped);
```

```txt
He was this soul sherpa. He took her heart! They say he's going to put me on a shelf.
She was this soul sherpa. She took his heart! They say she's going to put me on a shelf.
```



## Perl 6

Mechanically, this task is trivial. Perl 6 provides several flexible and powerful methods to wrangle text. Linguistically, this task is impossible (and laughable). Mappings are non-regular and in many cases, non-deterministic without semantic analysis of the content and context, which is '''WAY''' beyond what anyone is going to invest in a Rosettacode task. Whatever.

For extremely limited circumstances such as this example, this should suffice. Notice case matching and replication. Handles contractions, but ignores embedded matching text.


```perl6
say S:g:ii/«'she'»/he/ given "She was a soul stripper. She took my heart! They say she's going to put me on a shelf.";
```

```txt
He was a soul stripper. He took my heart! They say he's going to put me on a shelf.
```



## Phix

Oh well, I tried...  There are a couple of mildly interesting points though:

words is a pair-list, ie "she","he" maps both ways, with first-upper-case handled too, and

replacing the words right->left means no need to fiddle with indexes when lengths differ.

```Phix
constant words = {"she","he","his","her","him","her","hers","his"}

function reverse_gender(string s)
    integer ch, wordend
    bool inword = false, wordch
    for i=length(s) to 0 by -1 do
        ch = iff(i=0?' ':s[i])
        wordch = not find(ch," .,'!\n")
        if inword then
            if not wordch then
                string this = lower(s[i+1..wordend])
                integer k = find(this,words)
                if k then
                    string rep = words[iff(mod(k,2)?k+1:k-1)]
--                  if s[i+2..wordend]=rep[2..$] then -- might be wanted here
--                    -- (either skipping completely or all upper->all upper)
                    if s[i+1]!=words[k][1] then rep[1] = upper(rep[1]) end if
                    s[i+1..wordend] = rep
                end if
                inword = false
            end if
        elsif wordch then
            inword = true
            wordend = i
        end if
    end for
    return s
end function

constant tests = {"She was a soul stripper. She took my heart!\n",
                  "Her dog belongs to him but his dog is hers!\n"}  -- ha ha!

for i=1 to length(tests) do
    string ti = tests[i],
           r  = reverse_gender(ti),
           rr = reverse_gender(r)
    puts(1,ti&r&rr&"\n")
end for
```

```txt

She was a soul stripper. She took my heart!
He was a soul stripper. He took my heart!
She was a soul stripper. She took my heart!

Her dog belongs to him but his dog is hers!
His dog belongs to her but her dog is his!
Her dog belongs to his but his dog is her!

```



## PowerShell

{{trans|J}} (Made more PowerShelly.)

```PowerShell

function Switch-Gender ([string]$String)
{
    if ($String -match "She")
    {
        $String.Replace("She", "He")
    }
    elseif ($String -match "He")
    {
        $String.Replace("He", "She")
    }
    else
    {
        $String
    }
}

Switch-Gender "She was a soul stripper. She took my heart!"
Switch-Gender (Switch-Gender "She was a soul stripper. She took my heart!")

```

```txt

He was a soul stripper. He took my heart!
She was a soul stripper. She took my heart!

```



## Python


```Python
#!/usr/bin/env python
# -*- coding: utf-8 -*- #

import re
male2female=u"""maleS femaleS, maleness femaleness,
him her, himself herself, his her, his hers, he she,
Mr Mrs, Mister Missus, Ms Mr, Master Miss, Master Mistress,
uncleS auntS, nephewS nieceS, sonS daughterS, grandsonS granddaughterS,
brotherS sisterS, man woman, men women, boyS girlS, paternal maternal,
grandfatherS grandmotherS, GodfatherS GodmotherS, GodsonS GoddaughterS,
fiancéS fiancéeS, husband wife, husbands wives, fatherS motherS,
bachelorS spinsterS, bridegroomS brideS, widowerS widowS,
KnightS DameS, Sir DameS, KingS QueenS, DukeS DuchessES, PrinceS PrincessES,
Lord Lady, Lords Ladies, MarquessES MarchionessES, EarlS CountessES, ViscountS ViscountessES,
ladS lassES, sir madam, gentleman lady, gentlemen ladies, BaronS BaronessES,
stallionS mareS, ramS eweS, coltS fillieS, billy nanny, billies nannies, bullS cowS,
godS goddessES, heroS heroineS, shirtS blouseS, undies nickers, sweat glow,
jackarooS jillarooS, gigoloS hookerS, landlord landlady, landlords landladies,
manservantS maidservantS, actorS actressES, CountS CountessES, EmperorS EmpressES,
giantS giantessES, heirS heiressES, hostS hostessES, lionS lionessES, managerS manageressES,
murdererS murderessES, priestS priestessES, poetS poetessES, shepherdS shepherdessES,
stewardS stewardessES, tigerS tigressES, waiterS waitressES,
cockS henS, dogS bitchES, drakeS henS, dogS vixenS,
tomS tibS, boarS sowS, buckS roeS, peacockS peahenS,
gander goose, ganders geese, friarS nunS, monkS nunS, Adam Eve,
Aaron Erin, Adrian Adriana, Aidrian Aidriana, Alan Alaina, Albert Alberta,
Alex Alexa, Alex Alexis, Alexander Alaxandra, Alexander Alexandra,
Alexander Alexis, Alexandra Alexander, Alexei Alexis, Alfred Alfreda,
Andrew Andrea, Andrew Andrea, Angel Angelica, Anthony Antonia,
Antoine Antoinette, Ariel Arielle, Ashleigh Ashley,
Barry Barrie, Benedict Benita, Benjamin Benjamine, Bert Bertha,
Brandon Brandi, Brendan Brenda, Briana Brian, Brian Rianne,
Caela Caesi, Caeleb Caeli, Carl Carla, Carl Carly, Carolus Caroline,
Charles Caroline, Charles Charlotte, Christian Christa, Christian Christiana,
Christian Christina, Christopher Christina, Christopher Christine,
Clarence Claire, Claude Claudia, Clement Clementine, Cory Cora,
Daniel Daniella, Daniel Danielle, David Davena, David Davida,
David Davina, Dean Deanna, Devin Devina,
Edward Edwina, Edwin Edwina, Emil Emilie, Emil Emily, Eric Erica, Erick Erica,
Erick Ericka, Ernest Ernestine, Ethan Etha, Ethan Ethel, Eugene Eugenie,
Fabian Fabia, Francesco Francesca, Frances Francesca, Francis Frances,
Francis Francine, Frederick Fredrica, Fred Freda, Fredrick Frederica,
Gabriel Gabriella, Gabriel Gabrielle, Gene Jean, George Georgia, george georgina,
George Georgina, Gerald Geraldine, Giovanni Giovanna, Glen Glenn,
Harry Harriet, Harry Harriette, Heather Heath, Henry Henrietta, Horace Horatia,
Ian Iana, Ilija Ilinka, Ivo Ivy, Ivan Ivy,
Jack Jackelyn, Jack Jackie, Jack Jaclyn, Jack Jacqueline, Jacob Jacobine,
James Jamesina, James Jamie, Jaun Jaunita, Jayda Jayden, Jesse Jessica,
Jesse Jessie, Joe Johanna, Joel Joelle, John Jean, John Joan,
John Johanna, Joleen Joseph, Jon Joane, Joseph Josephine, Joseph Josphine,
Julian Julia, Julian Juliana, Julian Julianna, Justin Justine,
Karl Karly, Kendrick Kendra, Ken Kendra, Kian Kiana, Kyle Kylie,
Laurence Laura, Laurence Lauren, Laurence Laurencia, Leigh Leigha,
Leon Leona, Louis Louise, Lucas Lucia, Lucian Lucy, Luke Lucia, Lyle Lyla,
Maria Mario, Mario Maricela, Mark Marcia, Marshall Marsha, Martin martina,
Martin Martina, Martin Martine, Max Maxine, Michael Michaela, Michael Micheala,
Michael Michelle, Mitchell Michelle, Nadir Nadira, Nicholas Nicole, Nicholas Nicki,
Nicholas Nicole, Nicky Nikki, Nicolas Nicole, Nigel Nigella, Noel Noelle,
Oen Ioena, Oliver Olivia,
Patrick Patricia, Paul Paula, Phillip Phillipa, Phillip Pippa,
Quintin Quintina,
Reginald Regina, Richard Richardine, Robert Roberta, Robert Robyn, Ronald Rhonda,
Ryan Rhian, Ryan Ryanne,
Samantha Samuel, Samuel Samantha, Samuel Sammantha, Samuel Samuela,
Sean Sian, Sean Siana, Shaun Shauna, Sheldon Shelby, Sonny Sunny,
Stephan Stephanie, Stephen Stephanie, Steven Stephanie,
Terry Carol, Terry Carrol, Theodore Theadora, Theodore Theodora,
Theodore Theordora, Thomas Thomasina, Tristan Tricia, Tristen Tricia,
Ulric Ulrika, Valentin Valentina, Victor Victoria,
William Wilhelmina, William Willa, William Willamina,
Xavier Xaviera, Yarden Yardena, Zahi Zahira, Zion Ziona"""

re_nl=re.compile(r",[ \n]*")
m2f=[ tok.split(" ") for tok in re_nl.split(male2female) ]

switch={}
words=[]


re_plural=re.compile("E*S$")
re_ES=re.compile("ES$")

def gen_pluralize(m,f):
# do plurals first
  yield re_plural.sub("",m),re_plural.sub("",f)
  yield re_ES.sub("es",m),re_ES.sub("es",f)
  yield re_plural.sub("s",m),re_plural.sub("s",f)

def gen_capitalize_pluralize(m,f):
  for m,f in gen_pluralize(m,f):
    yield m.capitalize(), f.capitalize()
    yield m,f

def gen_switch_role_capitalize_pluralize(m,f):
  for m,f in gen_capitalize_pluralize(m,f):
    yield m,f
    yield f,m

for m,f in m2f:
  for xy,xx in gen_switch_role_capitalize_pluralize(m,f):
    if xy not in switch:
      switch[xy]=xx
      words.append(xy)

words="|".join(words)

re_word=re.compile(ur"\b("+words+ur")\b")

text=u'''When a new-hatched savage running wild about his native
woodlands in a grass clout, followed by the nibbling goats, as if
he were a green sapling; even then, in Queequeg's ambitious soul,
lurked a strong desire to see something more of Christendom than
a specimen whaler or two. His father was a High Chief, a King;
his uncle a High Priest; and on the maternal side he boasted aunts
who were the wives of unconquerable warriors. There was excellent
blood in his veins-royal stuff; though sadly vitiated, I fear,
by the cannibal propensity he nourished in his untutored youth.'''


def rev_gender(text):
  text=re_word.split(text)
  return "".join([ word+switch[gen] for word,gen in zip(text[::2],text[1::2])])+text[-1]

print rev_gender(text)
```

'''Output:'''

```txt

When a new-hatched savage running wild about her native
woodlands in a grass clout, followed by the nibbling goats, as if
she were a green sapling; even then, in Queequeg's ambitious soul,
lurked a strong desire to see something more of Christendom than
a specimen whaler or two. Her mother was a High Chief, a Queen;
her aunt a High Priestess; and on the paternal side she boasted uncles
who were the husbands of unconquerable warriors. There was excellent
blood in her veins-royal stuff; though sadly vitiated, I fear,
by the cannibal propensity she nourished in her untutored youth.

```



## Racket


```racket

#lang at-exp racket

(define raw-mapping @~a{
  maleS femaleS, maleness femaleness, him her, himself herself, his her, his
  hers, he she, Mr Mrs, Mister Missus, Ms Mr, Master Miss, MasterS MistressES,
  uncleS auntS, nephewS nieceS, sonS daughterS, grandsonS granddaughterS,
  brotherS sisterS, man woman, men women, boyS girlS, paternal maternal,
  grandfatherS grandmotherS, GodfatherS GodmotherS, GodsonS GoddaughterS,
  fiancéS fiancéeS, husband wife, husbands wives, fatherS motherS, bachelorS
  spinsterS, bridegroomS brideS, widowerS widowS, KnightS DameS, Sir DameS,
  KingS QueenS, DukeS DuchessES, PrinceS PrincessES, Lord Lady, Lords Ladies,
  MarquessES MarchionessES, EarlS CountessES, ViscountS ViscountessES, ladS
  lassES, sir madam, gentleman lady, gentlemen ladies, BaronS BaronessES,
  stallionS mareS, ramS eweS, coltS fillieS, billy nanny, billies nannies,
  bullS cowS, godS goddessES, heroS heroineS, shirtS blouseS, undies nickers,
  sweat glow, jackarooS jillarooS, gigoloS hookerS, landlord landlady,
  landlords landladies, manservantS maidservantS, actorS actressES, CountS
  CountessES, EmperorS EmpressES, giantS giantessES, heirS heiressES, hostS
  hostessES, lionS lionessES, managerS manageressES, murdererS murderessES,
  priestS priestessES, poetS poetessES, shepherdS shepherdessES, stewardS
  stewardessES, tigerS tigressES, waiterS waitressES, cockS henS, dogS bitchES,
  drakeS henS, dogS vixenS, tomS tibS, boarS sowS, buckS roeS, peacockS
  peahenS, gander goose, ganders geese, friarS nunS, monkS nunS, Adam Eve,
  Aaron Erin, Adrian Adriana, Aidrian Aidriana, Alan Alaina, Albert Alberta,
  Alex Alexa, Alex Alexis, Alexander Alaxandra, Alexander Alexandra, Alexander
  Alexis, Alexandra Alexander, Alexei Alexis, Alfred Alfreda, Andrew Andrea,
  Angel Angelica, Anthony Antonia, Antoine Antoinette, Ariel Arielle, Ashleigh
  Ashley, Barry Barrie, Benedict Benita, Benjamin Benjamine, Bert Bertha,
  Brandon Brandi, Brendan Brenda, Briana Brian, Brian Rianne, Caela Caesi,
  Caeleb Caeli, Carl Carla, Carl Carly, Carolus Caroline, Charles Caroline,
  Charles Charlotte, Christian Christa, Christian Christiana, Christian
  Christina, Christopher Christina, Christopher Christine, Clarence Claire,
  Claude Claudia, Clement Clementine, Cory Cora, Daniel Daniella, Daniel
  Danielle, David Davena, David Davida, David Davina, Dean Deanna, Devin
  Devina, Edward Edwina, Edwin Edwina, Emil Emilie, Emil Emily, Eric Erica,
  Erick Erica, Erick Ericka, Ernest Ernestine, Ethan Etha, Ethan Ethel, Eugene
  Eugenie, Fabian Fabia, Francesco Francesca, Frances Francesca, Francis
  Frances, Francis Francine, Frederick Fredrica, Fred Freda, Fredrick
  Frederica, Gabriel Gabriella, Gabriel Gabrielle, Gene Jean, George Georgia,
  George Georgina, Gerald Geraldine, Giovanni Giovanna, Glen Glenn, Harry
  Harriet, Harry Harriette, Heather Heath, Henry Henrietta, Horace Horatia, Ian
  Iana, Ilija Ilinka, Ivo Ivy, Ivan Ivy, Jack Jackelyn, Jack Jackie, Jack
  Jaclyn, Jack Jacqueline, Jacob Jacobine, James Jamesina, James Jamie, Jaun
  Jaunita, Jayda Jayden, Jesse Jessica, Jesse Jessie, Joe Johanna, Joel Joelle,
  John Jean, John Joan, John Johanna, Joleen Joseph, Jon Joane, Joseph
  Josephine, Joseph Josphine, Julian Julia, Julian Juliana, Julian Julianna,
  Justin Justine, Karl Karly, Kendrick Kendra, Ken Kendra, Kian Kiana, Kyle
  Kylie, Laurence Laura, Laurence Lauren, Laurence Laurencia, Leigh Leigha,
  Leon Leona, Louis Louise, Lucas Lucia, Lucian Lucy, Luke Lucia, Lyle Lyla,
  Maria Mario, Mario Maricela, Mark Marcia, Marshall Marsha, Martin martina,
  Martin Martine, Max Maxine, Michael Michaela, Michael Micheala, Michael
  Michelle, Mitchell Michelle, Nadir Nadira, Nicholas Nicole, Nicholas Nicki,
  Nicky Nikki, Nicolas Nicole, Nigel Nigella, Noel Noelle, Oen Ioena, Oliver
  Olivia, Patrick Patricia, Paul Paula, Phillip Phillipa, Phillip Pippa,
  Quintin Quintina, Reginald Regina, Richard Richardine, Robert Roberta, Robert
  Robyn, Ronald Rhonda, Ryan Rhian, Ryan Ryanne, Samantha Samuel, Samuel
  Samantha, Samuel Sammantha, Samuel Samuela, Sean Sian, Sean Siana, Shaun
  Shauna, Sheldon Shelby, Sonny Sunny, Stephan Stephanie, Stephen Stephanie,
  Steven Stephanie, Terry Carol, Terry Carrol, Theodore Theadora, Theodore
  Theodora, Theodore Theordora, Thomas Thomasina, Tristan Tricia, Tristen
  Tricia, Ulric Ulrika, Valentin Valentina, Victor Victoria, William
  Wilhelmina, William Willa, William Willamina, Xavier Xaviera, Yarden Yardena,
  Zahi Zahira, Zion Ziona})

(define flip-map (make-hash))

(for ([m (reverse (regexp-split #px"\\s*,\\s*" raw-mapping))])
  (define p (string-split m))
  (unless (= 2 (length p)) (error "Bad raw data"))
  (define (map! x y)
    (hash-set! flip-map (string-foldcase x) (string-foldcase y))
    (hash-set! flip-map (string-upcase x) (string-upcase y))
    (hash-set! flip-map (string-titlecase x) (string-titlecase y)))
  (define (2map! x y) (map! x y) (map! y x))
  (apply 2map! p)
  (apply 2map! (map (λ(x) (regexp-replace #rx"E?S$" x "")) p)))

(define (reverse-gender str)
  (regexp-replace* #px"\\w+" str
    (λ(word) (hash-ref flip-map word word))))

(displayln (reverse-gender @~a{
She was a soul stripper. She took my heart!

When a new-hatched savage running wild about his native
woodlands in a grass clout, followed by the nibbling goats, as if
he were a green sapling; even then, in Queequeg's ambitious soul,
lurked a strong desire to see something more of Christendom than
a specimen whaler or two. His father was a High Chief, a King;
his uncle a High Priest; and on the maternal side he boasted aunts
who were the wives of unconquerable warriors. There was excellent
blood in his veins-royal stuff; though sadly vitiated, I fear,
by the cannibal propensity he nourished in his untutored youth.
}))

```

```txt

He was a soul stripper. He took my heart!

When a new-hatched savage running wild about her native
woodlands in a grass clout, followed by the nibbling goats, as if
she were a green sapling; even then, in Queequeg's ambitious soul,
lurked a strong desire to see something more of Christendom than
a specimen whaler or two. Her mother was a High Chief, a Queen;
her aunt a High Priestess; and on the paternal side she boasted uncles
who were the husbands of unconquerable warriors. There was excellent
blood in her veins-royal stuff; though sadly vitiated, I fear,
by the cannibal propensity she nourished in her untutored youth.

```



## REXX

Not much effort was put into compressing the words (as far as pluralizing and constructing the
various forms of words).   More code could be written to parse words that have a hyphen   (or minus).

The 1<sup>st</sup> letter of each word in examined and it's case (lower/upper) is preserved.

Given the large size of the table (list), it would make more sense to put the words in a separate file instead of coding them in-line (within the computer program).

```rexx
/*REXX program reverse genderizes a text string (that may contain gender-specific words)*/
parse value  linesize()-1   with   sw  @  @.  !. /*get screen width,  nullify some vars.*/
parse arg old
if old='' then old='When a new-hatched savage running wild about his native woodlands in',
                   'a grass clout, followed by the nibbling goats, as if he were a green',
                   'sapling; even then, in Queequegs ambitious soul, lurked a strong'    ,
                   'desire to see something more of Christendom than a specimen whaler'  ,
                   'or two. His father was a High Chief, a King; his uncle a High'       ,
                   'Priest; and on the maternal side he boasted aunts who were the wives',
                   'of unconquerable warriors. There was excellent blood in his'         ,
                   'veins-royal stuff; though sadly vitiated, I fear, by the cannibal'   ,
                   'propensity he nourished in his untutored youth.'

call tell old, ' old '                           /*show a nicely parsed  "old"  text.   */

@=@ "abboty abbess"
@=@ "actor actress"
@=@ "ad-boy ad-girl ad-man ad-woman ad-men ad-women"
@=@ "adboy adgirl adman adwoman admen adwomen"
@=@ "administrator administratrix"
@=@ "adonis belle"
@=@ "adulterer adultress"
@=@ "agribusinessboy agribusinessgirl"
@=@ "agribusinessman agribusinesswoman agribusinessmen agribusinesswomen"
@=@ "aidboy aidgirl aidman aidwoman aidmen aidwomen"
@=@ "airboy airgirl airman airwoman airmen airwomen"
@=@ "aircraftboy aircraftgirl aircraftman aircraftwoman aircraftmen aircraftwomen"
@=@ "aircraftsboy aircraftsgirl aircraftsman aircraftswoman aircraftsmen aircraftswomen"
@=@ "aircrewboy aircrewgirl aircrewman aircrewwoman aircrewmen aircrewwomen"
@=@ "alderboy aldergirl alderman alderwoman aldermen alderwomen"
@=@ "almsboy almsgirl"
@=@ "almsman almswoman almsmen almswomen"
@=@ "alterboy altergirl alterman alterwoman altermen alterwomen"
@=@ "alongshoreboy alongshoregirl"
@=@ "alongshoreman alongshorewoman alongshoremen alongshorewomen"
@=@ "ambassador ambassadress"
@=@ "ambulanceboy ambulancegirl ambulanceman ambulancewoman ambulancemen ambulancewomen"
@=@ "anchor anchress"
@=@ "anchorboy anchorgirl anchorman anchorwoman anchormen anchorwomen"
@=@ "apeboy apegirl apeman apewoman apemen apewomen"
@=@ "archduke archduchess"
@=@ "archer archeress"
@=@ "artilleryboy artillerygirl artilleryman artillerywoman artillerymen artillerywomen"
@=@ "artsboy artsgirl artsman artswoman artsmen artswomen"
@=@ "assboy assgirl assman asswoman assmen asswomen"
@=@ "assemblyboy assemblygirl assemblyman assemblywoman assemblymen assemblywomen"
@=@ "attackboy attackgirl attackman attackwoman attackmen attackwomen"
@=@ "author authoress"
@=@ "aviator aviatrix aviators aviatrices"
@=@ "axboy axgirl axman axwoman axmen axwomen"
@=@ "axeboy axegirl axeman axewoman axemen axewomen"
@=@ "bachelor bachelorette bachelor spinster"
@=@ "backboy backgirl backman backwoman backmen backwomen"
@=@ "backwoodsboy backwoodsgirl backwoodsman backwoodswoman backwoodsmen backwoodswomen"
@=@ "badboy badgirl badman badwoman badmen badwomen"
@=@ "bagboy baggirl bagman bagwoman bagmen bagwomen"
@=@ "baggageboy baggagegirl baggageman baggagewoman baggagemen baggagewomen"
@=@ "bail-bondsboy bail-bondsgirl"
@=@ "bail-bondsman bail-bondswoman bail-bondsmen bail-bondswomen"
@=@ "bailsboy bailsgirl bailsman bailswoman bailsmen bailswomen"
@=@ "ballerino ballerina"
@=@ "bandsboy bandsgirl bandsman bandswoman bandsmen bandswomen"
@=@ "barboy bargirl barman barwoman barmen barwomen barman barmaid"
@=@ "bargeboy bargegirl bargeman bargewoman bargemen bargewomen"
@=@ "barkeeper barkeeperess"
@=@ "baron baroness baronet baronetess"
@=@ "baseboy basegirl baseman basewoman basemen basewomen"
@=@ "bassboy bassgirl bassman basswoman bassmen basswomen"
@=@ "batboy batgirl batman batwoman batmen batwomen"
@=@ "batsboy batsgirl batsman batswoman batsmen batswomen"
@=@ "bayboy baygirl bayman baywoman baymen baywomen"
@=@ "beadsboy beadsgirl beadsman beadswoman beadsmen beadswomen"
@=@ "bedesboy bedesgirl bedesman bedeswoman bedesmen bedeswomen"
@=@ "beggarboy beggargirl beggarman beggarwoman beggarmen beggarwomen"
@=@ "bellboy bellgirl bellman bellwoman bellmen bellwomen"
@=@ "benefactor benefactress"
@=@ "billboy billgirl billman billwoman billmen billwomen"
@=@ "billy nanny billies nannies"
@=@ "billygoat nannygoat"
@=@ "binboy bingirl binman binwoman binmen binwomen"
@=@ "birdboy birdgirl birdman birdwoman birdmen birdwomen"
@=@ "blond blonde"
@=@ "bluesboy bluesgirl bluesman blueswoman bluesmen blueswomen"
@=@ "boar sow"
@=@ "boardboy boardgirl boardman boardwoman boardmen boardwomen"
@=@ "boatboy boatgirl boatman boatwoman boatmen boatwomen"
@=@ "boatsboy boatsgirl boatsman boatswoman boatsmen boatswomen"
@=@ "bogeyboy bogeygirl bogeyman bogeywoman bogeymen bogeywomen"
@=@ "bogyboy bogygirl bogyman bogywoman bogymen bogywomen"
@=@ "boilerboy boilergirl boilerman boilerwoman boilermen boilerwomen"
@=@ "bombardboy bombardgirl bombardman bombardwoman bombardmen bombardwomen"
@=@ "bondboy bondgirl bondman bondwoman bondmen bondwomen"
@=@ "bondsboy bondsgirl bondsman bondswoman bondsmen bondswomen"
@=@ "bonesboy bonesgirl bonesman boneswoman bonesmen boneswomen"
@=@ "boogeyboy boogeygirl boogeyman boogeywoman boogeymen boogeywomen"
@=@ "boogieboy boogiegirl boogieman boogiewoman boogiemen boogiewomen"
@=@ "boogyboy boogygirl boogyman boogywoman boogymen boogywomen"
@=@ "bookboy bookgirl bookman bookwoman bookmen bookwomen"
@=@ "boothboy boothgirl boothman boothwoman boothmen boothwomen"
@=@ "bordboy bordgirl bordman bordwoman bordmen bordwomen"
@=@ "bowboy bowgirl bowman bowwoman bowmen bowwomen"
@=@ "bowsboy bowsgirl bowsman bowswoman bowsmen bowswomen"
@=@ "boxboy boxgirl boxman boxwoman boxmen boxwomen"
@=@ "boy girl boydom girldom boyhood girlhood"
@=@ "boy-band girl-band"
@=@ "boy-oh-boy girl-oh-girl"
@=@ "boychildren girlchildren"
@=@ "boyfriend girlfriend"
@=@ "boyish girlish boyism girlism"
@=@ "boyish-looking girlish-looking boyishly girlishly boyishness girlishness"
@=@ "boylike girllike boylikeness girllikeness boyliker girlliker"
@=@ "boylikest girllikest boyscout girlscout boyship girlship"
@=@ "brakeboy brakegirl brakeman brakewoman brakemen brakewomen"
@=@ "breadboy breadgirl breadman breadwoman breadmen breadwomen"
@=@ "breakboy breakgirl breakman breakwoman breakmen breakwomen"
@=@ "brethern sistern"
@=@ "brickboy brickgirl brickman brickwoman brickmen brickwomen"
@=@ "bridegroom bride"
@=@ "bridesboy bridesgirl bridesman brideswoman bridesmen brideswomen"
@=@ "briefboy briefgirl briefman briefwoman briefmen briefwomen"
@=@ "brinksboy brinksgirl brinksman brinkswoman brinksmen brinkswomen"
@=@ "bro sis brother sister brotherhood sisterhood brotherly sisterly"
@=@ "brotherboy brothergirl brotherman brotherwoman brothermen brotherwomen"
@=@ "buck doe"
@=@ "bull cow bullshit cowshit"
@=@ "busboy busgirl busman buswoman busmen buswomen"
@=@ "bushboy bushgirl bushman bushwoman bushmen bushwomen"
@=@ "bushelboy bushelgirl bushelman bushelwoman bushelmen bushelwomen"
@=@ "businessboy businessgirl businessman businesswoman businessmen businesswomen"
@=@ "butcher butcheress"
@=@ "butt-boy butt-girl butt-man butt-woman butt-men butt-women"
@=@ "butterboy buttergirl butterman butterwoman buttermen butterwomen"
@=@ "buttonboy buttongirl buttonman buttonwoman buttonmen buttonwomen"
@=@ "cabboy cabgirl cabman cabwoman cabmen cabwomen"
@=@ "cakeboy cakegirl cakeman cakewoman cakemen cakewomen"
@=@ "caliph calafia caliph calipha"
@=@ "cameraboy cameragirl cameraman camerawoman cameramen camerawomen"
@=@ "candy-boy candy-girl candy-man candy-woman candy-men candy-women"
@=@ "candyboy candygirl candyman candywoman candymen candywomen"
@=@ "canoeboy canoegirl canoeman canoewoman canoemen canoewomen"
@=@ "carboy cargirl carman carwoman carmen carwomen"
@=@ "cartboy cartgirl cartman cartwoman cartmen cartwomen"
@=@ "caterer cateress"
@=@ "catfisherboy catfishergirl catfisherman catfisherwoman catfishermen catfisherwomen"
@=@ "cattleboy cattlegirl cattleman cattlewoman cattlemen cattlewomen"
@=@ "cavalryboy cavalrygirl cavalryman cavalrywoman cavalrymen cavalrywomen"
@=@ "caveboy cavegirl caveman cavewoman cavemen cavewomen"
@=@ "cellarboy cellargirl cellarman cellarwoman cellarmen cellarwomen"
@=@ "centerboy centergirl centerman centerwoman centermen centerwomen"
@=@ "centreboy centregirl centreman centrewoman centremen centrewomen"
@=@ "chainboy chaingirl chainman chainwoman chainmen chainwomen"
@=@ "chairboy chairgirl chairman chairwoman chairmen chairwomen"
@=@ "chanter chantress"
@=@ "chapboy chapgirl chapman chapwoman chapmen chapwomen"
@=@ "chapelboy chapelgirl chapelman chapelwoman chapelmen chapelwomen"
@=@ "charboy chargirl charman charwoman charmen charwomen"
@=@ "checkweighboy checkweighgirl"
@=@ "checkweighman checkweighwoman checkweighmen checkweighwomen"
@=@ "chessboy chessgirl chessman chesswoman chessmen chesswomen"
@=@ "chief chiefess"
@=@ "chinaboy chinagirl chinaman chinawoman chinamen chinawomen"
@=@ "chineseboy chinesegirl chineseman chinesewoman chinesemen chinesewomen"
@=@ "churchboy churchgirl churchman churchwoman churchmen churchwomen"
@=@ "cisboy cisgirl cisman ciswoman cismen ciswomen"
@=@ "clansboy clansgirl clansman clanswoman clansmen clanswomen"
@=@ "classboy classgirl classman classwoman classmen classwomen"
@=@ "clergyboy clergygirl clergyman clergywoman clergymen clergywomen"
@=@ "clerk clerkess"
@=@ "clubboy clubgirl clubman clubwoman clubmen clubwomen"
@=@ "coachboy coachgirl coachman coachwoman coachmen coachwomen"
@=@ "coadjutor cadutrix"
@=@ "coalboy coalgirl coalman coalwoman coalmen coalwomen"
@=@ "coastguardsboy coastguardsgirl"
@=@ "coastguardsman coastguardswoman coastguardsmen coastguardswomen"
@=@ "cock hen"
@=@ "cocksboy cocksgirl cocksman cockswoman cocksmen cockswomen"
@=@ "cogboy coggirl cogman cogwoman cogmen cogwomen"
@=@ "colorboy colorgirl colorman colorwoman colormen colorwomen"
@=@ "colourboy colourgirl colourman colourwoman colourmen colourwomen"
@=@ "colt fillie"
@=@ "commedian comedienne"
@=@ "committeeboy committeegirl committeeman committeewoman committeemen committeewomen"
@=@ "commonwealthboy commonwealthgirl"
@=@ "commonwealthman commonwealthwoman commonwealthmen commonwealthwomen"
@=@ "commonwealthsboy commonwealthsgirl"
@=@ "commonwealthsman commonwealthswoman commonwealthsmen commonwealthswomen"
@=@ "conboy congirl conman conwoman conmen conwomen"
@=@ "conductor conductress"
@=@ "confessor confessoress"
@=@ "congressboy congressgirl congressman congresswoman congressmen congresswomen"
@=@ "conquer conqueress"
@=@ "cook cookess"
@=@ "copeboy copegirl copeman copewoman copemen copewomen"
@=@ "cornerboy cornergirl cornerman cornerwoman cornermen cornerwomen"
@=@ "cornishboy cornishgirl cornishman cornishwoman cornishmen cornishwomen"
@=@ "corpsboy corpsgirl corpsman corpswoman corpsmen corpswomen"
@=@ "councilboy councilgirl councilman councilwoman councilmen councilwomen"
@=@ "count countess"
@=@ "counterboy countergirl counterman counterwoman countermen counterwomen"
@=@ "countryboy countrygirl countryman countrywoman countrymen countrywomen"
@=@ "cowboy cowgirl cowman cowwoman cowmen cowwomen"
@=@ "cracksboy cracksgirl cracksman crackswoman cracksmen crackswomen"
@=@ "craftsboy craftsgirl craftsman craftswoman craftsmen craftswomen"
@=@ "cragsboy cragsgirl cragsman cragswoman cragsmen cragswomen"
@=@ "crayfisherboy crayfishergirl"
@=@ "crayfisherman crayfisherwoman crayfishermen crayfisherwomen"
@=@ "cyberboy cybergirl cyberman cyberwoman cybermen cyberwomen"
@=@ "czar czarina"
@=@ "dad mom dada mama daddy mommy daddies mommies"
@=@ "dairyboy dairygirl dairyman dairywoman dairymen dairywomen"
@=@ "dangerboy dangergirl dangerman dangerwoman dangermen dangerwomen"
@=@ "daysboy daysgirl daysman dayswoman daysmen dayswomen"
@=@ "deacon deaconess"
@=@ "deadboy deadgirl deadman deadwoman deadmen deadwomen"
@=@ "debutant debutante"
@=@ "demesboy demesgirl demesman demeswoman demesmen demeswomen"
@=@ "demon demoness"
@=@ "deskboy deskgirl deskman deskwoman deskmen deskwomen"
@=@ "devil deviless"
@=@ "director directress"
@=@ "dirtboy dirtgirl dirtman dirtwoman dirtmen dirtwomen"
@=@ "divine divineress"
@=@ "divorce divorcee"
@=@ "doctor doctress"
@=@ "dog bitch dogs bitches"
@=@ "dominator dominatrix dominators dominatrices"
@=@ "dragon dragoness"
@=@ "drake duck"
@=@ "draftboy draftgirl draftman draftwoman drafemen drafewomen"
@=@ "draftsboy draftsgirl draftsman draftswoman draftsmen draftswomen"
@=@ "draughtsboy draughtsgirl draughtsman draughtswoman draughtsmen draughtswomen"
@=@ "drayboy draygirl drayman draywoman draymen draywomen"
@=@ "drone bee"
@=@ "dude dudette"
@=@ "duke duchess"
@=@ "dutchboy dutchgirl"
@=@ "dutchman dutchwoman dutchmen dutchwomen"
@=@ "earl countess"
@=@ "earthboy earthgirl earthman earthwoman earthmen earthwomen"
@=@ "earthsboy earthsgirl earthsman earthswoman earthsmen earthswomen"
@=@ "editor editress"
@=@ "editor editrix"
@=@ "elector electress"
@=@ "emperor empress"
@=@ "enchanter enchantress"
@=@ "englishboy englishgirl englishman englishwoman englishmen englishwomen"
@=@ "everyboy everygirl everyman everywoman everymen everywomen"
@=@ "ex-boyfriend ex-girlfriend ex-husband ex-wife ex-husbands ex-wives"
@=@ "executor executrix executor executres"
@=@ "faceboy facegirl faceman facewoman facemen facewomen"
@=@ "father mother"
@=@ "fatherfucker motherfucker fatherphocker motherphocker fatherfucker mutherfucker"
@=@ "fatherhood motherhood"
@=@ "fiance fiancee"
@=@ "fireboy firegirl fireman firewoman firemen firewomen"
@=@ "fisherboy fishergirl fisherman fisherwoman fishermen fisherwomen"
@=@ "fishboy fishgirl fishman fishwoman fishmen fishwomen"
@=@ "foeboy foegirl foeman foewoman foemen foewomen"
@=@ "foreboy foregirl foreman forewoman foremen forewomen"
@=@ "freeboy freegirl freedman freedwoman freedmen freedwomen"
@=@ "freedboy freedgirl freeman freewoman freemen freewomen"
@=@ "frenchboy frenchgirl frenchman frenchwoman frenchmen frenchwomen"
@=@ "fretboy fretgirl fretman fretwoman fretmen fretwomen"
@=@ "friar nun"
@=@ "frontboy frontgirl frontiersboy frontiersgirl"
@=@ "frontiersman frontierswoman frontiersmen frontierswomen"
@=@ "frontman frontwoman frontmen frontwomen"
@=@ "funnyboy funnygirl funnyman funnywoman funnymen funnywomen"
@=@ "gander goose ganders geese"
@=@ "gasboy gasgirl gasman gaswoman gasmen gaswomen"
@=@ "gentleboy gentlegirl gentleman gentlewoman gentlemen gentlewomen"
@=@ "giant giantess"
@=@ "gigolo hooker"
@=@ "gladiator gladiatrix"
@=@ "gleeboy gleegirl gleeman gleewoman gleemen gleewomen"
@=@ "gloveboy glovegirl"
@=@ "gloveman glovewoman glovemen glovewomen"
@=@ "god godess"
@=@ "godfather godmother godson goddaughter"
@=@ "governor governoress"
@=@ "gownboy gowngirl gownman gownwoman gownmen gownwomen"
@=@ "gownsboy gownsgirl gownsman gownswoman gownsmen gownswomen"
@=@ "gramp granny"
@=@ "granddad grandmom"
@=@ "grandfather grandmother"
@=@ "grandnephew grandniece"
@=@ "grandpa grandma"
@=@ "grandpapa grandmama"
@=@ "grandpop grandmom"
@=@ "grandson granddaughter"
@=@ "great-granddad great-grandmom"
@=@ "great-grandfather great-grandmother"
@=@ "great-grandnephew great-grandniece"
@=@ "great-grandpa great-grandma"
@=@ "great-grandpapa great-grandmama"
@=@ "great-grandpop great-grandmom"
@=@ "great-grandson great-granddaughter"
@=@ "great-granduncle great-grandaunt"
@=@ "great-granduncle great-grandauntie"
@=@ "great-great-granddad great-great-grandmom"
@=@ "great-great-grandfather great-great-grandmother"
@=@ "great-great-grandnephew great-great-grandniece"
@=@ "great-great-grandpa great-great-grandma"
@=@ "great-great-grandpapa great-great-grandmama"
@=@ "great-great-grandpop great-great-grandmom"
@=@ "great-great-grandson great-great-granddaughter"
@=@ "great-great-granduncle great-great-grandaunt"
@=@ "great-great-granduncle great-great-grandauntie"
@=@ "great-great-great-granddad great-great-great-grandmom"
@=@ "great-great-great-grandfather great-great-great-grandmother"
@=@ "great-great-great-grandnephew great-great-great-grandniece"
@=@ "great-great-great-grandpa great-great-great-grandma"
@=@ "great-great-great-grandpapa great-great-great-grandmama"
@=@ "great-great-great-grandpop great-great-great-grandmom"
@=@ "great-great-great-grandson great-great-great-granddaughter"
@=@ "great-great-great-granduncle great-great-great-grandaunt"
@=@ "great-great-great-granduncle great-great-great-grandauntie"
@=@ "great-great-great-great-granddad great-great-great-great-grandmom"
@=@ "great-great-great-great-grandfather great-great-great-great-grandmother"
@=@ "great-great-great-great-grandnephew great-great-great-great-grandniece"
@=@ "great-great-great-great-grandpa great-great-great-great-grandma"
@=@ "great-great-great-great-grandpapa great-great-great-great-grandmama"
@=@ "great-great-great-great-grandpop great-great-great-great-grandmom"
@=@ "great-great-great-great-grandson great-great-great-great-granddaughter"
@=@ "great-great-great-great-granduncle great-great-great-great-grandaunt"
@=@ "great-great-great-great-granduncle great-great-great-great-grandauntie"
@=@ "great-great-great-great-great-granddad great-great-great-great-great-grandmom"
@=@ "great-great-great-great-great-grandfather great-great-great-great-great-grandmother"
@=@ "great-great-great-great-great-grandnephew great-great-great-great-great-grandniece"
@=@ "great-great-great-great-great-grandpa great-great-great-great-great-grandma"
@=@ "great-great-great-great-great-grandpapa great-great-great-great-great-grandmama"
@=@ "great-great-great-great-great-grandpop great-great-great-great-great-grandmom"
@=@ "great-great-great-great-great-grandson great-great-great-great-great-granddaughter"
@=@ "great-great-great-great-great-granduncle great-great-great-great-great-grandaunt"
@=@ "great-great-great-great-great-granduncle great-great-great-great-great-grandauntie"
@=@ "great-great-great-great-great-uncle great-great-great-great-great-grandaunt"
@=@ "great-great-great-great-great-uncle great-great-great-great-great-grandauntie"
@=@ "great-great-great-great-uncle great-great-great-great-grandaunt"
@=@ "great-great-great-great-uncle great-great-great-great-grandauntie"
@=@ "great-great-great-uncle great-great-great-grandaunt"
@=@ "great-great-great-uncle great-great-great-grandauntie"
@=@ "great-great-uncle great-great-grandaunt"
@=@ "great-great-uncle great-great-grandauntie"
@=@ "great-uncle great-grandaunt"
@=@ "great-uncle great-grandauntie"
@=@ "gringo gringa"
@=@ "groom bride"
@=@ "groomsboy groomsgirl groomsman groomswoman groomsmen groomswomen"
@=@ "groundsboy groundsgirl groundsman groundswoman groundsmen groundswomen"
@=@ "gunboy gungirl gunman gunwoman gunmen gunwomen"
@=@ "guy gal"
@=@ "hackboy hackgirl hackman hackwoman hackmen hackwomen"
@=@ "hammerboy hammergirl hammerman hammerwoman hammermen hammerwomen"
@=@ "handcraftsboy handcraftsgirl"
@=@ "handcraftsman handcraftswoman handcraftsmen handcraftswomen"
@=@ "handi-craftsboy handi-craftsgirl"
@=@ "handi-craftsman handi-craftswoman handi-craftsmen handi-craftswomen"
@=@ "hangboy hanggirl hangman hangwoman hangmen hangwomen"
@=@ "hardboy hardgirl hardman hardwoman hardmen hardwomen"
@=@ "hatchetboy hatchetgirl hatchetman hatchetwoman hatchetmen hatchetwomen"
@=@ "he she him her himself herself his hers his her"
@=@ "he-boy he-girl he-man he-woman he-men he-women"
@=@ "headmaster headmistress"
@=@ "heir heiress"
@=@ "helboy helgirl helman helwoman helmen helwomen"
@=@ "helmsman helmswoman helmsmen helmswomen"
@=@ "herdboy herdgirl herdman herdwoman herdmen herdwoman"
@=@ "heritor heritress heritor heritrix"
@=@ "hero heroine"
@=@ "highwayboy highwaygirl highwayman highwaywoman highwaymen highwaywomen"
@=@ "hillsboy hillsgirl hillsman hillswoman hillsmen hillswomen"
@=@ "hob jill"
@=@ "horseboy horsegirl horseman horsewoman horsemen horsewomen"
@=@ "host hostess"
@=@ "hunter huntress"
@=@ "husband wife husbands wives"
@=@ "hypeboy hypegirl hypeman hypewoman hypemen hypewomen"
@=@ "iceboy icegirl iceman icewoman icemen icewomen"
@=@ "incubii sucubii incubus succubus"
@=@ "inheritor inheritress inheritor inheritrix"
@=@ "instructor instructress"
@=@ "irishboy irishgirl irishman irishwoman irishmen irishwomen"
@=@ "ironboy irongirl ironman ironwoman ironmen ironwomen"
@=@ "jackaroo jillaroo jack jill"
@=@ "jew jewess"
@=@ "jointer jointress"
@=@ "khaliph khalafia khaliph khalipha"
@=@ "king queen"
@=@ "king-hit queen-hit"
@=@ "king-of-arms queen-of-arms"
@=@ "kingcraft queencraft"
@=@ "kingcup queencup"
@=@ "kingdom queendom"
@=@ "kingdomful queendomful kingdomless queendomless kingdomship queendomship"
@=@ "kinged queened"
@=@ "kinger queener"
@=@ "kingest queenest"
@=@ "kinghead queenhead"
@=@ "kinghood queenhood"
@=@ "kinging queening"
@=@ "kingless queenless kinglessness queenlessness"
@=@ "kinglier queenlier kingliest queenliest"
@=@ "kinglihood queenlihood"
@=@ "kinglike queenlike kingliker queenliker kinglikest queenlikest"
@=@ "kingliness queenliness"
@=@ "kingling queenling kingling queenling kingly queenly"
@=@ "kingmaker queenmaker kingmaking queenmaking"
@=@ "kingpiece queenpiece"
@=@ "kingpin queenpin kingpost queenpost"
@=@ "kingsboy kingsgirl kingsman kingswoman kingsmen kingswomen"
@=@ "kingship queenship"
@=@ "kingside queenside"
@=@ "kingsize queensize"
@=@ "kingsman queensman kingsmen queensmen"
@=@ "klansboy klansgirl klansman klanswoman klansmen klanswomen"
@=@ "kinglier queenlier kingliest queenliest"
@=@ "kinglihood queenlihood"
@=@ "kinglike queenlike kingliker queenliker kinglikest queenlikest"
@=@ "kingliness queenliness"
@=@ "kingling queenling kingling queenling kingly queenly"
@=@ "kingmaker queenmaker kingmaking queenmaking"
@=@ "kingpiece queenpiece"
@=@ "kingpin queenpin kingpost queenpost"
@=@ "kingsboy kingsgirl kingsman kingswoman kingsmen kingswomen"
@=@ "kingship queenship"
@=@ "kingside queenside"
@=@ "kingsize queensize"
@=@ "kingsman queensman kingsmen queensmen"
@=@ "klansboy klansgirl klansman klanswoman klansmen klanswomen"
@=@ "knight dame"
@=@ "lad lass laddie lassie"
@=@ "landgrave landgravine"
@=@ "landlord landlady landlords handladies"
@=@ "latino latina"
@=@ "launderer laundress"
@=@ "laundryboy laundrygirl laundryman laundrywoman laundrymen laundrywomen"
@=@ "lawboy lawgirl lawman lawwoman lawmen lawwomen"
@=@ "lawyer layeress"
@=@ "layboy laygirl layman laywoman laymen laywomen"
@=@ "leatherboy leathergirl leatherman leatherwoman leathermen leatherwomen"
@=@ "legboy leggirl legman legwoman legmen legwomen"
@=@ "liegeboy liegegirl liegeman liegewoman liegemen liegewomen"
@=@ "lineboy linegirl lineman linewoman linemen linewomen"
@=@ "linesboy linesgirl linesman lineswoman linesmen lineswomen"
@=@ "linkboy linkgirl linkman linkwoman linkmen linkwomen"
@=@ "lion lioness"
@=@ "lizardboy lizardgirl lizardman lizardwoman lizardmen lizardwomen"
@=@ "lord lady lords ladies"
@=@ "madboy madgirl madman madwoman madmen madwomen"
@=@ "mailboy mailgirl mailman mailwoman mailmen mailwomen"
@=@ "male female maleness femaleness"
@=@ "man woman men women"
@=@ "man-boy girl-worman"
@=@ "man-children woman-children manchildren womanchildren"
@=@ "manager manageress"
@=@ "manhood womenhood"
@=@ "manly womanly"
@=@ "manservant maidservant"
@=@ "margrave margavine"
@=@ "marquess marquis marquise marchioness"
@=@ "masculine feminine"
@=@ "masseue masseuse"
@=@ "mastboy mastgirl"
@=@ "mastman mastwoman mastmen mastwomen"
@=@ "maybe-boy maybe-girl maybe-man maybe-woman maybe-men maybe-women"
@=@ "mayor mayoress"
@=@ "mediator mediatress mediator mediatrix mediator mediatrice"
@=@ "men-children women-children menchildren womenchildren"
@=@ "merboy mergirl merman mermaid merman merwoman mermen merwomen"
@=@ "middleboy middlegirl middleman middlewoman middlemen middlewomen"
@=@ "midshipboy midshipgirl midshipman midshipwoman midshipmen midshipwomen"
@=@ "milkboy milkgirl milkman milkwoman milkmen milkwomen"
@=@ "millionaire millionairess billionaire billionairess"
@=@ "misandry misogyny misandrist misogynist"
@=@ "moneyboy moneygirl moneyman moneywoman moneymen moneywomen"
@=@ "monk nun"
@=@ "monster monsteress"
@=@ "moor morisco"
@=@ "mr mrs mister missus mr ms mr mz master miss master mistress"
@=@ "murderer murderess"
@=@ "muscleboy musclegirl muscleman musclewoman musclemen musclewomen"
@=@ "negroe negress negro negress"
@=@ "nephew niece"
@=@ "newsboy newsgirl newsman newswoman newsmen newswomen"
@=@ "newspaperboy newspapergirl newspaperman newspaperwoman newspapermen newspaperwomen"
@=@ "no-boy no-girl no-man no-woman no-men no-women"
@=@ "nobelman noblewoman nobelmen nobelwomen"
@=@ "nurseryboy nurserygirl nurseryman nurserywoman nurserymen nurserywomen"
@=@ "orator oratress orator oratrix"
@=@ "orchardboy orchardgirl orchardman orchardwoman orchardmen orchardwomen"
@=@ "overboy overgirl overman overwoman overmen overwomen"
@=@ "pa ma papa mama pop mom poppy mommy"
@=@ "paceboy pacegirl paceman pacewoman pacemen pacewomen"
@=@ "paternal maternal patriarchal matriarchal"
@=@ "patricide matricide"
@=@ "patrolboy patrolgirl patrolman patrolwoman patrolmen patrolwomen"
@=@ "patron patroness"
@=@ "peacock peahen"
@=@ "pitboy pitgirl pitman pitwoman pitmen pitwomen"
@=@ "pitchboy pitchgirl pitchman pitchwoman pitchmen pitchwomen"
@=@ "plowman plowwoman plowmen plowwomen"
@=@ "poet poetess"
@=@ "policeboy policegirl policeman policewoman policemen policewomen"
@=@ "poultryboy poultrygirl poultryman poultrywoman poultrymen poultrywomen"
@=@ "preacher preacheress"
@=@ "priest priestess"
@=@ "prince princess"
@=@ "prior prioress"
@=@ "prophet prophetess"
@=@ "proprietor proprietress"
@=@ "protor protectress"
@=@ "ragboy raggirl ragman ragwoman ragmen ragwomen"
@=@ "railroadboy railroadgirl railroadman railroadwoman railroadmen railroadwomen"
@=@ "railwayboy railwaygirl railwayman railwaywoman railwaymen railwaywomen"
@=@ "rainboy raingirl rainman rainwoman rainmen rainwomen"
@=@ "ram ewe billy ewe"
@=@ "rastaboy rastagirl rastaman rastawoman rastamen rastawomen"
@=@ "remainder-boy remainder-girl"
@=@ "remainder-man remainder-woman remainder-men remainder-women"
@=@ "remainderboy remaindergirl remainderman remainderwoman remaindermen remainderwomen"
@=@ "repoboy repogirl repoman repowoman repomen repowomen"
@=@ "rescueboy rescuegirl rescueman rescuewoman rescuemen rescuewomen"
@=@ "ringboy ringgirl ringman ringwoman ringmen ringwomen"
@=@ "schoolmaster schoolmistress"
@=@ "scotsboy scotsgirl scotsman scotswoman scotsmen scotswomen"
@=@ "sculptor sculptress"
@=@ "seaboy seagirl seaman seawoman seamen seawomen"
@=@ "seducer seduceress"
@=@ "seductor seductress"
@=@ "seedsboy seedsgirl seedsman seedswoman seedsmen seedswomen"
@=@ "sempster sempstress"
@=@ "senor senora"
@=@ "serviceboy servicegirl serviceman servicewoman servicemen servicewomen"
@=@ "sewerboy sewergirl sewerman sewerwoman sewermen sewerwomen"
@=@ "shaboy shagirl shaman shawoman shamen shawomen"
@=@ "sheepboy sheepgirl sheepman sheepwoman sheepmen sheepwomen"
@=@ "shellfisherboy shellfishergirl"
@=@ "shellfisherman shellfisherwoman shellfishermen shellfisherwomen"
@=@ "shepherd shepherdess"
@=@ "shirt blouse"
@=@ "shopboy shopgirl shopman shopwoman shopmen shopwomen"
@=@ "showboy showgirl showman showwoman showmen showwomen"
@=@ "silkboy silkgirl silkman silkwoman silkmen silkwomen"
@=@ "singer singeress"
@=@ "sir madam sir ma'am sir damn"
@=@ "sire dam"
@=@ "snowboy snowgirl snowman snowwoman snowmen snowwomen"
@=@ "son daughter"
@=@ "songster songstress"
@=@ "sorcerer sorceress"
@=@ "spokesboy spokesgirl spokesman spokeswoman spokesmen spokeswomen"
@=@ "sportsboy sportsgirl sportsman sportswoman sportsmen sportswomen"
@=@ "stag hind"
@=@ "stallion mare"
@=@ "statesboy statesgirl statesman stateswoman statesmen stateswomen"
@=@ "steer heifer"
@=@ "steersboy steersgirl steersman steerswoman steersmen steerswomen"
@=@ "stepdad stepmom stepfather stepmother stepson stepdaughter"
@=@ "steward stewardess"
@=@ "stuntboy stuntgirl stuntman stuntwoman stuntmen stuntwomen"
@=@ "suitor suitress"
@=@ "sultan sultana"
@=@ "sweat glow"
@=@ "tailor seamstress"
@=@ "talesboy talesgirl talesman taleswoman talesmen taleswomen"
@=@ "talisboy talisgirl talisman taliswoman talismen taliswomen"
@=@ "taskmaster taskmistress"
@=@ "temptor temptress"
@=@ "terminator terminatrix"
@=@ "tiger tigress"
@=@ "toastmaster toastmistress"
@=@ "tod vixen"
@=@ "tom hen"
@=@ "townsboy townsgirl townsman townswoman townsmen townswomen"
@=@ "toyboy toygirl toyman toywoman toymen toywomen"
@=@ "tradesboy tradesgirl tradesman tradeswoman tradesmen tradeswomen"
@=@ "traitor traitress"
@=@ "trencherboy trenchergirl trencherman trencherwoman trenchermen trencherwomen"
@=@ "triggerboy triggergirl triggerman triggerwoman triggermen triggerwomen"
@=@ "tutor tutoress"
@=@ "tzar tzarina"
@=@ "uncle aunt uncle auntie"
@=@ "undies knickers"
@=@ "usher usherette"
@=@ "utilityboy utilitygirl utilityman utilitywoman utilitymen utilitywomen"
@=@ "vampire vampiress"
@=@ "victor victress"
@=@ "villian villainess"
@=@ "viscount viscountess viscount visereine"
@=@ "vixor vixen"
@=@ "votary votaress votary votress votaries votresses"
@=@ "wageboy wagegirl wageman wagewoman wagemen wagewomen"
@=@ "waiter waitress"
@=@ "warder wardess"
@=@ "warrior warrioress warlock witch"
@=@ "washerboy washergirl washerman washerwoman washermen washerwomen"
@=@ "watchboy watchgirl watchman watchwoman watchmen watchwomen"
@=@ "waterboy watergirl waterman waterwoman watermen waterwomen"
@=@ "weighboy weighgirl weighman weighwoman weighmen weighwomen"
@=@ "werewolf wifwolf"
@=@ "whaleboy whalegirl whaleman whalewoman whalemen whalewomen"
@=@ "wheelboy wheelgirl wheelman wheelwoman wheelmen wheelwomen"
@=@ "whoremonger whoremistress"
@=@ "widower widow"
@=@ "wingboy winggirl wingman wingwoman wingmen wingwomen"
@=@ "wiseboy wisegirl wiseman wisewoman wisemen wisewomen"
@=@ "wizard witch"
@=@ "workboy workgirl workman workwoman workmen workwomen"
@=@ "workingboy workinggirl workingman workingwoman workingmen workingwomen"
@=@ "yachtsboy yachtsgirl yachtsman yachtswoman yachtsmen yachtswomen"
@=@ "yardboy yardgirl yardman yardwoman yardmen yardwomen"
@=@ "yes-boy yes-girl yes-man yes-woman yes-men yes-women"
                                                 /*"first" names;  not a complete list. */
@=@ "Aaron Erin Adam Eve Adrian Adriana Aidrian Aidriana Alan Alaina Albert Alberta Alex"
@=@ "Alexa Alex Alexis Alexander Alaxandra Alexander Alexandra Alexander Alexis"
@=@ "Alexandra Alexander Alexei Alexis Alfred Alfreda Andrew Andrea Angel Angelica"
@=@ "Anthony Antonia Antoine Antoinette Ariel Arielle Ashleigh Ashley Barry Barrie"
@=@ "Benedict Benita Benjamin Benjamine Bert Bertha Brandon Brandi Brendan Brenda Brian"
@=@ "Rianne Briana Brian Caela Caesi Caeleb Caeli Carl Carla Carl Carly Carolus Caroline"
@=@ "Charles Caroline Charles Charlotte Christian Christa Christian Christiana Christian"
@=@ "Christina Christopher Christina Christopher Christine Clarence Claire Claude"
@=@ "Claudia Clement Clementine Cory Cora Daniel Daniella Daniel Danielle David Davena"
@=@ "David Davida David Davina Dean Deanna Devin Devina Edward Edwina Edwin Edwina Emil"
@=@ "Emilie Emil Emily Eric Erica Erick Erica Erick Ericka Ernest Ernestine Ethan Etha"
@=@ "Ethan Ethel Eugene Eugenie Fabian Fabia Frances Francesca Francesco Francesca"
@=@ "Francis Frances Francis Francine Fred Freda Frederick Fredrica Fredrick Frederica"
@=@ "Gabriel Gabriella Gabriel Gabrielle Gene Jean George Georgia George Georgina Gerald"
@=@ "Geraldine Gerard Gerardette Giovanni Giovanna Glen Glenn Harry Harriet Harry"
@=@ "Harriette Heather Heath Henry Henrietta Horace Horatia Ian Iana Ilija Ilinka Ivan"
@=@ "Ivy Ivo Ivy Jack Jackelyn Jack Jackie Jack Jaclyn Jack Jacqueline Jacob Jacobine"
@=@ "James Jamesina James Jamie Jaun Jaunita Jayda Jayden Jesse Jessica Jesse Jessie Joe"
@=@ "Johanna Joel Joelle John Jean John Joan John Johanna Joleen Joseph Jon Joane Joseph"
@=@ "Josephine Joseph Josphine Julian Julia Julian Juliana Julian Julianna Justin"
@=@ "Justine Karl Karly Ken Kendra Kendrick Kendra Kian Kiana Kyle Kylie Laurence Laura"
@=@ "Laurence Lauren Laurence Laurencia Leigh Leigha Leon Leona Louis Louise Lucas Lucia"
@=@ "Lucian Lucy Luke Lucia Lyle Lyla Maria Mario Mario Maricela Mark Marcia Marshall"
@=@ "Marsha Martin Martina Martin Martine Max Maxine Michael Michaela Michael Micheala"
@=@ "Michael Michelle Mitchell Michelle Nadir Nadira Nicholas Nicki Nicholas Nicole"
@=@ "Nicky Nikki Nicolas Nicole Nigel Nigella Noel Noelle Oen Ioena Oliver Olivia"
@=@ "Patrick Patricia Paul Paula Phillip Phillipa Phillip Pippa Quintin Quintina"
@=@ "Reginald Regina Richard Richardine Robert Roberta Robert Robyn Ronald Rhonda Ryan"
@=@ "Rhian Ryan Ryanne Samantha Samuel Samuel Samantha Samuel Sammantha Samuel Samuela"
@=@ "Sean Sian Sean Siana Shaun Shauna Sheldon Shelby Sonny Sunny Stephan Stephanie"
@=@ "Stephen Stephanie Steven Stephanie Terry Carol Terry Carrol Theodore Theadora"
@=@ "Theodore Theodora Theodore Theordora Thomas Thomasina Tristan Tricia Tristen Tricia"
@=@ "Ulric Ulrika Valentin Valentina Victor Victoria William Wilhelmina William Willa"
@=@ "William Willamina Xavier Xaviera Yarden Yardena Zahi Zahira Zion Ziona"

say center(" There're "      words(@)      ' words in the gender bender list. ', sw, '─')

  do j=1  to words(@)  by 2;          n=j+1
  m  =word(@,j);    f  =word(@,n);    @.m=m    ;     !.m=f    ;    @.f  =f  ;    !.f  =m
  ms =many(m)  ;    fs =many(f)  ;    @.ms=ms  ;     !.ms=fs  ;    @.fs =fs ;    !.fs =ms
  mp =proper(m);    fp =proper(f);    @.mp=mp  ;     !.mp=fp  ;    @.fp =fp ;    !.fp =mp
  mps=many(mp) ;    fps=many(fp) ;    @.mps=mps;     !.mps=fps;    @.fps=fps;    !.fps=mps
  upper  m  f  ;                      @.m=m    ;     !.m=f    ;    @.f  =f  ;    !.f  =m
  ms =many(m)  ;    fs =many(f)  ;    @.ms=ms  ;     !.ms=fs  ;    @.fs =fs ;    !.fs =ms
  end   /*j*/
                               /* [↑]  handle lower/uppercase, capitalized, and plurals.*/
new=
        do k=1  for words(old)
        new=new  bendit( word(old,k) )           /*construct a list of  "gender"  words.*/
        end   /*k*/
say
call tell new, ' new '                           /*show a nicely parsed  "new"  text.   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bendit:  parse arg x 1 ox                        /*get a word,  make a copy of original.*/
         if length(x)==1  then return ox         /*if one character, then return as is. */
         @abc= 'abcdefghijklmnopqrstuvwxyz'      /*define a lowercase (Latin) alphabet. */
         parse upper var  @abc  @abcU  pref suff /*get uppercase version,  nullify vars.*/
         @abcU=@abc || @abcU                     /*construct lower & uppercase alpahbet.*/
         _=verify(x, @abcU, 'M')                 /*see if all the "letters" are letters.*/
         if _==0  then return ox                 /*No?  Then return it as is; not a word*/
         pref=left(x, _ - 1)                     /*obtain (any, if at all) prefix.      */
         x=substr(x, _)                          /*obtain the suffix  (any, if at all). */
         xr=reverse(x)                           /*reverse the string (for testing caps)*/
         _=verify(xr, @abcU, 'M')
         if _\==0  then do;  suff=reverse( left(xr, _  - 1) )
                             xr=substr(xr, _)
                        end
         x=reverse(xr)
         if \datatype(x, 'M')  then return x     /*Not all letters? Then return original*/
         if @.x\==''  then return pref || !.x || suff                /*plurized ?       */
         if !.x\==''  then return pref || @.x || suff                /*has a gender ?   */
                           return pref ||   x || suff                /*No? Return as is.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
many:    parse arg _;  if right(_, 1)=='s'  then return _ || 'es'    /*maintain lower.  */
                       if right(_, 1)=='S'  then return _ || 'ES'    /*    "    upper.  */
                       if datatype(_,'U')   then return _'S'         /*use uppercase?   */
                                                 return _'s'         /* "  lowercase.   */
/*──────────────────────────────────────────────────────────────────────────────────────*/
proper:  arg L1 2;     parse arg 2 _2;           return L1 || _2
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell:    procedure expose sw;   parse arg z;     z=space(z);   $=
         say center( arg(2), sw, '─')
                                            do until z=='';  parse var z x z;        n=$ x
                                            if length(n)<sw  then do;  $=n;  iterate;  end
                                            say strip($)
                                            $=x
                                            end   /*until*/
         if $\=''  then say strip($)
         say
         return
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console).

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]].

```txt

───────────────────────────────────────────── old ─────────────────────────────────────────────
She was a soul stripper. She took my heart!

────────────────────── There're  2776  words in the gender bender list. ───────────────────────

───────────────────────────────────────────── new ─────────────────────────────────────────────
He was a soul stripper. He took my heart!

```

```txt

───────────────────────────────────── old ─────────────────────────────────────
When a new-hatched savage running wild about his native woodlands in a grass
clout, followed by the nibbling goats, as if he were a green sapling; even
then, in Queequegs ambitious soul, lurked a strong desire to see something
more of Christendom than a specimen whaler or two. His father was a High
Chief, a King; his uncle a High Priest; and on the maternal side he boasted
aunts who were the wives of unconquerable warriors. There was excellent blood
in his veins-royal stuff; though sadly vitiated, I fear, by the cannibal
propensity he nourished in his untutored youth.

────────────── There're  2776  words in the gender bender list. ───────────────

───────────────────────────────────── new ─────────────────────────────────────
When a new-hatched savage running wild about her native woodlands in a grass
clout, followed by the nibbling goats, as if she were a green sapling; even
then, in Queequegs ambitious soul, lurked a strong desire to see something
more of Christendom than a specimen whaler or two. Her mother was a High
Chiefess, a Queen; her auntie a High Priestess; and on the paternal side she
boasted uncles who were the husbands of unconquerable warrioresses. There was
excellent blood in her veins-royal stuff; though sadly vitiated, I fear, by
the cannibal propensity she nourished in her untutored youth.

```



## Ring


```ring

# Project : Reverse the gender of a string

revGender = list(4)
word = ["She", "she", "Her", "her", "hers", "He", "he", "His", "his", "him"]
repl = ["He", "he", "His", "his" ,"his", "She", "she", "Her", "her", "her"]

revGender[1] = "She was a soul stripper. She took his heart!"
revGender[2] = "He was a soul stripper. He took her heart!"
revGender[3] = "She wants what's hers, he wants her and she wants him!"
revGender[4] = "Her dog belongs to him but his dog is hers!"

for p=1 to 4
    gstr = ""
    see revGender[p] + " ->" + nl
    gend = repl(revGender[p])
    for nr=1 to len(gend)
        if nr = len(gend)
           gstr = gstr + gend[nr]
        else
           gstr = gstr + gend[nr] + " "
        ok
    next
    gstr = trim(gstr)
    gstr = left(gstr, len(gstr) - 2)
    if right(gstr, 1) != "!"
       gstr = gstr + "!"
    ok
    see gstr + nl + nl
next

func repl(cStr)
     cStr = words(cStr) + nl
     for n=1 to len(cStr)
         flag = 0
         for m=1 to len(word)
             if right(cStr[n],1) = ","
                cStr[n] = left(cStr[n], len(cStr[n]) - 1)
                flag = 1
             ok
             if right(cStr[n],1) = "!"
                cStr[n] = left(cStr[n], len(cStr[n]) - 1)
                flag = 2
             ok
             if cStr[n] = word[m]
                if flag = 0
                   cStr[n] = repl[m]
                ok
                if flag = 1
                   cStr[n] = repl[m] + ","
                ok
                if flag = 2
                   cStr[n] = repl[m] + "!"
                ok
                exit
             ok
         next
        next
        return cStr


func words(cStr2)
     aList = str2list(cStr2)
     for x in aList
         x2 = substr(x," ",nl)
         alist2 = str2list(x2)
     next
     return alist2

```

Output:

```txt

She was a soul stripper. She took his heart! ->
He was a soul stripper. He took her heart!

He was a soul stripper. He took her heart! ->
She was a soul stripper. She took his heart!

She wants what's hers, he wants her and she wants him! ->
He wants what's his, she wants his and he wants her!

Her dog belongs to him but his dog is hers! ->
His dog belongs to her but her dog is his!

```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/cpBaoMf/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/0dajvapgRRChRZaZgpeqnQ Scastie (remote JVM)].

```Scala
object RevGender extends App {
  val s = "She was a soul stripper. She took my heart!"
  println(cheapTrick(s))
  println(cheapTrick(cheapTrick(s)))

  def cheapTrick(s: String): String = s match {
    case _: String if s.toLowerCase.contains("she") => s.replaceAll("She", "He")
    case _: String if s.toLowerCase.contains("he")  => s.replaceAll("He", "She")
    case _: String                                  => s
  }

}
```


## Sidef


```ruby
var male2female = <<'EOD'
  maleS femaleS, maleness femaleness, him her, himself herself, his her, his
  hers, he she, Mr Mrs, Mister Missus, Ms Mr, Master Miss, MasterS MistressES,
  uncleS auntS, nephewS nieceS, sonS daughterS, grandsonS granddaughterS,
  brotherS sisterS, man woman, men women, boyS girlS, paternal maternal,
  grandfatherS grandmotherS, GodfatherS GodmotherS, GodsonS GoddaughterS,
  fiancéS fiancéeS, husband wife, husbands wives, fatherS motherS, bachelorS
  spinsterS, bridegroomS brideS, widowerS widowS, KnightS DameS, Sir DameS,
  KingS QueenS, DukeS DuchessES, PrinceS PrincessES, Lord Lady, Lords Ladies,
  MarquessES MarchionessES, EarlS CountessES, ViscountS ViscountessES, ladS
  lassES, sir madam, gentleman lady, gentlemen ladies, BaronS BaronessES,
  stallionS mareS, ramS eweS, coltS fillieS, billy nanny, billies nannies,
  bullS cowS, godS goddessES, heroS heroineS, shirtS blouseS, undies nickers,
  sweat glow, jackarooS jillarooS, gigoloS hookerS, landlord landlady,
  landlords landladies, manservantS maidservantS, actorS actressES, CountS
  CountessES, EmperorS EmpressES, giantS giantessES, heirS heiressES, hostS
  hostessES, lionS lionessES, managerS manageressES, murdererS murderessES,
  priestS priestessES, poetS poetessES, shepherdS shepherdessES, stewardS
  stewardessES, tigerS tigressES, waiterS waitressES, cockS henS, dogS bitchES,
  drakeS henS, dogS vixenS, tomS tibS, boarS sowS, buckS roeS, peacockS
  peahenS, gander goose, ganders geese, friarS nunS, monkS nunS
EOD
 
var m2f = male2female.split(/,\s*/).map { |tok| tok.words}
 
var re_plural = /E?S\z/
var re_ES = /ES\z/
 
func gen_pluralize(m, f) {
    [
        [m - re_plural, f - re_plural],
        [m.sub(re_ES, 'es'), f.sub(re_ES, 'es')],
        [m.sub(re_plural, 's'), f.sub(re_plural, 's')],
    ]
}
 
var dict = Hash()
 
for m,f in m2f {
    for x,y in gen_pluralize(m, f).map{.map{.lc}} {
        if (x ~~ dict) {
            dict{y} = x
        } else {
            dict{x, y} = (y, x)
        }
    }
}
 
var gen_re = Regex.new('\b(' + dict.keys.join('|') + ')\b', 'i')
 
func copy_case(orig, repl) {
    var a = orig.chars
    var b = repl.chars
 
    var uc = 0
    var min = [a, b].map{.len}.min
    for i in ^min {
        if (a[i] ~~ /^[[:upper:]]/) {
            b[i].uc!
            ++uc
        }
    }
 
    uc == min ? repl.uc : b.join('')
}
 
func reverse_gender(text) {
    text.gsub(gen_re, { |a| copy_case(a, dict{a.lc}) })
}
```


Example:

```ruby
say reverse_gender("She was a soul stripper. She took my heart!");
```

```txt
He was a soul stripper. He took my heart!
```



## Tcl


```tcl
# Construct the mapping variables from the source mapping
apply {{} {
    global genderMap genderRE
    # The mapping is from the Python solution, though omitting the names
    # for the sake of a bit of brevity...
    foreach {maleTerm femaleTerm} {
	maleS femaleS  maleness femaleness
	him her  himself herself  his hers  his her  he she
	Mr Mrs  Mister Missus  Ms Mr  Master Miss  Master Mistress
	uncleS auntS  nephewS nieceS  sonS daughterS  grandsonS granddaughterS
	brotherS sisterS  man woman  men women  boyS girlS  paternal maternal
	grandfatherS grandmotherS  GodfatherS GodmotherS  GodsonS GoddaughterS
	fiancéS fiancéeS  husband wife  husbands wives  fatherS motherS
	bachelorS spinsterS  bridegroomS brideS  widowerS widowS
	KnightS DameS  Sir DameS  KingS QueenS  DukeS DuchessES
	PrinceS PrincessES  Lord Lady  Lords Ladies  MarquessES MarchionessES
	EarlS CountessES  ViscountS ViscountessES  ladS lassES  sir madam
	gentleman lady  gentlemen ladies  BaronS BaronessES
	stallionS mareS  ramS eweS  coltS fillieS  billy nanny  billies nannies
	bullS cowS  godS goddessES  heroS heroineS  shirtS blouseS  undies nickers
	sweat glow  jackarooS jillarooS  gigoloS hookerS  landlord landlady
	landlords landladies  manservantS maidservantS  actorS actressES
	CountS CountessES  EmperorS EmpressES  giantS giantessES  heirS heiressES
	hostS hostessES  lionS lionessES  managerS manageressES
	murdererS murderessES  priestS priestessES  poetS poetessES
	shepherdS shepherdessES  stewardS stewardessES  tigerS tigressES
	waiterS waitressES  cockS henS  dogS bitchES  drakeS henS  dogS vixenS
	tomS tibS  boarS sowS  buckS roeS  peacockS peahenS
	gander goose  ganders geese  friarS nunS  monkS nunS
    } {
	foreach {m f} [list \
	    $maleTerm $femaleTerm \
	    [regsub {E*S$} $maleTerm ""] [regsub {E*S$} $femaleTerm ""]
	] {
	    dict set genderMap [string tolower $m] [string tolower $f]
	    dict set genderMap [string toupper $m] [string toupper $f]
	    dict set genderMap [string totitle $m] [string totitle $f]
	    dict set genderMap [string tolower $f] [string tolower $m]
	    dict set genderMap [string toupper $f] [string toupper $m]
	    dict set genderMap [string totitle $f] [string totitle $m]
	}
    }
    # Now the RE, which matches any key in the map *as a word*
    set genderRE "\\m(?:[join [dict keys $genderMap] |])\\M"
}}

proc reverseGender {string} {
    global genderRE genderMap
    # Used to disable Tcl's metacharacters for [subst]
    set safetyMap {\\ \\\\ \[ \\\[ \] \\\] $ \\$}
    subst [regsub -all $genderRE [string map $safetyMap $string] {[
	string map $genderMap &
    ]}]
}
```

Demonstrating:

```tcl
puts [reverseGender "She was a soul stripper. She took my heart!"]\n
puts [reverseGender "When a new-hatched savage running wild about his native
woodlands in a grass clout, followed by the nibbling goats, as if
he were a green sapling; even then, in Queequeg's ambitious soul,
lurked a strong desire to see something more of Christendom than
a specimen whaler or two. His father was a High Chief, a King;
his uncle a High Priest; and on the maternal side he boasted aunts
who were the wives of unconquerable warriors. There was excellent
blood in his veins-royal stuff; though sadly vitiated, I fear,
by the cannibal propensity he nourished in his untutored youth."]
```

```txt

He was a soul stripper. He took my heart!

When a new-hatched savage running wild about her native
woodlands in a grass clout, followed by the nibbling goats, as if
she were a green sapling; even then, in Queequeg's ambitious soul,
lurked a strong desire to see something more of Christendom than
a specimen whaler or two. Her mother was a High Chief, a Queen;
her aunt a High Priestess; and on the paternal side she boasted uncles
who were the husbands of unconquerable warriors. There was excellent
blood in her veins-royal stuff; though sadly vitiated, I fear,
by the cannibal propensity she nourished in her untutored youth.

```

