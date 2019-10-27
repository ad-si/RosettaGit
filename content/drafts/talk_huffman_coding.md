+++
title = "Talk:Huffman coding"
description = ""
date = 2019-05-16T17:09:52Z
aliases = []
[extra]
id = 4036
[taxonomies]
categories = []
tags = []
+++

==the Java example==
Isn't the Java example wrong? It's not even descending - I thought Huffman code needs it to be sorted as descending before making a tree.

Umm... this is all wrong. Read the Wikipedia article. According to your scheme Huffman codewords would all be of the form 111...10 or 111...1, but that is not at all the case. --[[Special:Contributions/76.167.241.45|76.167.241.45]] 03:59, 26 March 2009 (UTC)
: Yep. It is not Huffman coding. (Hmm, I do like the wikipedia description of the two queue method though ...) --[[User:Paddy3118|Paddy3118]] 06:56, 26 March 2009 (UTC)
::I did it based on what I learned in class today. If you look at the "Basic technique" section on the WP it shows codes identical to ones I used in the example so I'm pretty sure it is Huffman coding. There must be a few ways to generate them that give different actual codes with the same idea. --[[User:Mwn3d|Mwn3d]] 13:44, 26 March 2009 (UTC)
::: For the example given, the Huffman code indeed looks like this. But as a general algorithm, it's wrong. What you should do is that you "combine" the last two elements in the table, and sort them back into your list. So starting with the example,
 (A=?): 50%
 (B=?): 25%
 (C=?): 12.5%
 (D=?): 12.5%
::: you first assign the bit to the last two items (C and D), and then ''combine'' them, adding the frequencies, and ''sorting it into the right place''
 (A=?): 50%
 (B=?): 25%
 (C=?0,D=?1): 25%
::: Then you do the same again:
 (A=?): 50%
 (B=?0, C=?10, D=?11): 50%
::: And finally you get
 (A=0, B=10, C=110, D=1110): 100%
::: Thus the result is indeed as given in the example. However, assume that you start with
 (A=?): 25%
 (B=?): 25%
 (C=?): 25%
 (D=?): 25%
::: Your algorithm would still give the same result, while it's obvious that using just the standard two-bit numbering is optimal for this case. And indeed, the first step of the Huffman coding gives:
 (C=?0, D=?1): 50%
 (A=?): 25%
 (B=?): 25%
::: Note how the (C,D) case moves up, because it's probability is larger than the 25% of each of A and B. Therefore the next step combines A and B:
 (A=?0, B=?1): 50%
 (C=?0, D=?1): 50%
::: (I've made the convention that items with the same probability are sorted lexicographically; of course other conventions, like e.g. leaving the newly formed pair as low as possible, also work). Now our final combination provides:
 (A=00, B=01, C=10, D=11): 100%
::: which obviously is an optimal code for this case. --[[User:Ce|Ce]] 14:17, 26 March 2009 (UTC)
::::Huffman coding in the case where all the symbols have the same frequency is not practical, though. If all the symbols have the same frequency no thought needs ot be put into it at all and you can just use ordinary binary encoding. --[[User:Mwn3d|Mwn3d]] 15:59, 26 March 2009 (UTC)
:::::Huffman coding in the case where all the symbols have the same frequency will ''precisely'' generate the binary encoding. That's the whole point. Huffman coding always generates an optimal symbol-by-symbol coding.
:::::Anyway, a better example of Huffman coding I think would be something like the example at the top right of the Wikipedia article. The frequencies are taken from the string "this is an example of a huffman tree", and produces the following:
:::::{| class="wikitable"
!Char!!Freq!!Code
|-
|space||7/36||111
|-
|a    ||4/36||010
|-
|e    ||4/36||000
|-
|f    ||3/36||1101
|-
|h    ||2/36||1010
|-
|i    ||2/36||1000
|-
|m    ||2/36||0111
|-
|n    ||2/36||0010
|-
|s    ||2/36||1011
|-
|t    ||2/36||0110
|-
|l    ||1/36||11001
|-
|o    ||1/36||00110
|-
|p    ||1/36||10011
|-
|r    ||1/36||11000
|-
|u    ||1/36||00111
|-
|x    ||1/36||10010
|}
:::::Your coding doesn't have to be exactly the same, but it should be equivalent to the above (i.e. codeword lengths should be the same, except that symbols with equal frequencies might be switched around). --[[Special:Contributions/76.167.241.45|76.167.241.45]] 18:55, 26 March 2009 (UTC)


### By hand

I made tons of that when coded my first Static Huffman cruncher eons ago (68k assembly, maybe I still can find the codes on old 3.5 floppies); by hand I made so

```txt

A 50% ----------------\
B 25% ----------\      \
C 12.5%  \       \      \  100%
D 12.5%  / 25%   /  50% /

```


so being "up" 1 and "down" 0: A=1, B=01, C=001, D=000. From here an algo can be extracted (by word in short: group 2-by-2 less frequent leaves; so first we have 12.5% and 12.5%, then we have the new leaf at 25%, and another one at 25%, so we join these... and so on). Dynamic Huffman Encoding is a little bit harder, not too much anyway, but I have never implemented that. I like this task, with a little bit of maquillage it's ok. 

Now let's take the case all 25%


```txt

A 25 \ 50 \
B 25 /     \
C 25 \     / 100
D 25 / 50 /

```


Id est, two bit for each letter. --[[User:ShinTakezou|ShinTakezou]] 16:33, 26 March 2009 (UTC)

By the way, what is exactly wrong? The examplanation? Or the java code? --[[User:ShinTakezou|ShinTakezou]] 16:37, 26 March 2009 (UTC)


==Python==
I took a <strike>good</strike> better look at the WP article and came up with the following code, together with printouts of what it is doing:

```python
from heapq import heappush, heappop, heapify

def encode(symbol2weights, tutor= False):
    ''' Huffman encode the given dict mapping symbols to weights '''
    heap = [ [float(wt), [sym, '']] for sym, wt in symbol2weights.iteritems() ]
    heapify(heap)
    if tutor: print "ENCODING:", sorted(symbol2weights.iteritems())
    while len(heap) >1:
        lo = heappop(heap)
        hi = heappop(heap)
        if tutor: print "  COMBINING:", lo, '\n        AND:', hi
        for i in lo[1:]: i[1] = '0' + i[1]
        for i in hi[1:]: i[1] = '1' + i[1]
        lohi = [ lo[0] + hi[0] ] + lo[1:] + hi[1:]
        if tutor: print "  PRODUCING:", lohi, '\n'
        heappush(heap, lohi)
    return sorted(heappop(heap)[1:], key=lambda x: (len(x[-1]), x))

#readin = "B 25   C 2.5 D  12.5 A 5 \n"
#readin = "a .1 b .15 c .3 d .16 e .29" # Wikipedia sample
#readin = "a1 .4 a2 .35 a3 .2 a4 .05" # Wikipedia sample
readin = "A 50 B 25 C 12.5 D 12.5" # RC example
cleaned = readin.strip().split()
symbol2weights = dict((symbol, wt)
                     for symbol, wt in zip(cleaned[0::2], cleaned[1::2]) )
huff = encode(symbol2weights, True)
print "\nSYMBOL\tWEIGHT\tHUFFMAN CODE"
for h in huff:
    print "%s\t%s\t%s" % (h[0], symbol2weights[h[0]], h[1])

```


The output, for one of your examples above is:

```txt
ENCODING: [('A', '50'), ('B', '25'), ('C', '12.5'), ('D', '12.5')]
  COMBINING: [12.5, ['C', '']] 
        AND: [12.5, ['D', '']]
  PRODUCING: [25.0, ['C', '0'], ['D', '1']] 

  COMBINING: [25.0, ['B', '']] 
        AND: [25.0, ['C', '0'], ['D', '1']]
  PRODUCING: [50.0, ['B', '0'], ['C', '10'], ['D', '11']] 

  COMBINING: [50.0, ['A', '']] 
        AND: [50.0, ['B', '0'], ['C', '10'], ['D', '11']]
  PRODUCING: [100.0, ['A', '0'], ['B', '10'], ['C', '110'], ['D', '111']] 


SYMBOL	WEIGHT	HUFFMAN CODE
A	50	0
B	25	10
C	12.5	110
D	12.5	111
```


With the following lines inserted into the program:

```python
astring = "this is an example of a huffman tree"
symbol2weights = dict((ch, astring.count(ch)) for ch in set(astring))

```

I get the following codes:

```txt
SYMBOL	WEIGHT	HUFFMAN CODE
 	7	111
a	4	000
e	4	001
f	3	1101
h	2	0100
i	2	0101
m	2	0111
n	2	1000
s	2	1010
t	2	1011
l	1	01100
o	1	01101
p	1	10010
r	1	10011
u	1	11000
x	1	11001
```


- --[[User:Paddy3118|Paddy3118]] 10:14, 27 March 2009 (UTC)

: At a glance it's ok; it's enough that the codes are not ambiguous (a longer code cannot have as "prefix" a shorter one). My code generate apparently a third way... hopefully right:


```txt

  (3) 000
a (3) 110
e (3) 111
f (4) 0010
h (4) 0011
i (4) 0110
m (4) 0111
n (4) 0100
s (4) 0101
t (4) 1010
l (5) 10010
o (5) 10011
p (5) 10000
r (5) 10001
u (5) 10110
x (5) 10111

```



::Yeah, it looks right. --[[Special:Contributions/71.106.173.110|71.106.173.110]] 09:47, 27 March 2009 (UTC)

:: Pretty certain testing the Python code is not correct --[[Special:Contributions/Art-the-physicist|Art-the-physicist]] 18:09, 16 May 2019

== Category ==

Is this a Text Processing task? We use "text" just because it's easier to show things using text, but this task works with any sequence of bytes... So I've removed the Text Processing category from the task. --[[User:ShinTakezou|ShinTakezou]] 00:23, 27 March 2009 (UTC)

== The C code uses a GCC extension ==

In the C code, the <code>swap_</code> macro (defined for <code>_heap_sort</code>) uses statement expressions, which are a gcc extension. The program might therefore not compile with other compilers. --[[User:Ce|Ce]] 08:57, 27 March 2009 (UTC)

: Fixed. --[[User:ShinTakezou|ShinTakezou]] 10:22, 27 March 2009 (UTC)

== Complaint about C++ example ==

User 122.167.5.231 [http://rosettacode.org/mw/index.php?title=Huffman_coding&diff=129055&oldid=128618 added] a claim that the C++ code is incorrect. I have moved the note from the page to here:

''Important : This method does not generate the optimal Huffman tree for any given string; it suffers from a serious flaw because of the fact that elements in a c++ priority queue are ordered according to strict weak ordering. To see why, please check out [http://cs.nyu.edu/~melamed/courses/102/lectures/huffman.ppt this example]. It shows that the optimal huffman tree for the given line of text will have no code longer than 4 bits. This piece of code generates huffman codes which are 5 bits in size. Try running it with the same line of text as input and you can verify this.''

I dispute these statements. First of all, the linked PowerPoint presentation incorrectly encodes the text in their example ("Eerie eyes seen near lake.") given their own encoding they generated. It says that it takes 73 bits; however, the correct encoded string is <tt>000010110000011001110001010110101111011010111001111101011111100011001111110100100101</tt>, which is 84 bits. Secondly, nowhere in the PowerPoint does it "show that the optimal huffman tree ... will have no code longer than 4 bits". It merely shows that that particular optimal Huffman coding (one of many possible ones which are equally optimal) has no code longer than 4 bits. In fact, if you take the C++ code and run the same example string, you will get an encoding which, although it uses 5-bit codes for some characters, still encodes the string in 84 bits, so is equally optimal. Finally, the PowerPoint does not mention any "serious flaw because of the fact that elements in a c++ priority queue are ordered according to strict weak ordering", and I can't seem to make sense of this statement. --[[User:Spoon!|Spoon!]] 11:21, 26 December 2011 (UTC)

: Given that the example string has more than 16 different characters, the "4 bit" assertion is obviously wrong.  I guess the anon failed to take into account the requirement that no code can be a prefix to another code.  (And, powerpoint?  Please.  Use a portable format.) --[[User:Ledrug|Ledrug]] 18:29, 26 December 2011 (UTC)

:: The example string in their example only has 12 characters. --[[User:Spoon!|Spoon!]] 03:58, 27 December 2011 (UTC)

== task's wording ==

"Using the characters and their frequency from the string "this is an example for huffman encoding", create a program to generate a Huffman encoding for each character as a table."

I'd suggest:

create a program to generate a Huffman encoding for each character in a string as a table and show this table for the string "this is an example for huffman encoding" --[[User:Walterpachl|Walterpachl]] ([[User talk:Walterpachl|talk]]) 07:54, 26 July 2014 (UTC)
