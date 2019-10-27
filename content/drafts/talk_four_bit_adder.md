+++
title = "Talk:Four bit adder"
description = ""
date = 2012-07-09T01:05:54Z
aliases = []
[extra]
id = 7519
[taxonomies]
categories = []
tags = []
+++

== Cleanup ==

Some things that ought to be cleaned up in the description of this task:
* Provide some math notation describing the individual gates; that will be of some use to those not accustomed to thinking in schematic gate logic.
* (Maybe?) individual images per-gate. Replace mentions of specific gates with their symbol, or...
* Make gate-specific usages of the words '''AND''', '''OR''', '''NOT''', '''XOR''' and their negated counterparts consistent in terms of capitalization and style.
* My embedding job for the images isn't particularly stable, wrt coexistence with the '''pre''' blocks, but I don't have time atm to do a nice table for them. Whoever does...thank you? :)
--[[User:Short Circuit|Michael Mol]] 13:51, 15 June 2010 (UTC)

: Indeed I did the images thinking to describe the task more in a visual way instead of sticking to notation only...! ;; linked wikipedia gates page to avoid to have to do other images:D, I could add a little "lecture" about boolean notation and gates, but I suppose there must be a limit ;; more easily doable, I will do it (later);;  --[[User:ShinTakezou|ShinTakezou]] 16:48, 15 June 2010 (UTC)

:The images push down into the following code, which looks wrong.  I think that entry headers should get the {clear: both} style?  --[[User:Rdm|Rdm]] 16:52, 15 June 2010 (UTC)
:: I can try that. It's going to slow the site down some to edit that particular template, though! Just beware... --[[User:Short Circuit|Michael Mol]] 17:33, 15 June 2010 (UTC)
::: That did not seem to help, for me (I am using Chrome).  Perhaps because the style is on a <nowiki><span></nowiki>?  

::: http://www.blooberry.com/indexdot/css/properties/classify/clear.htm says that css2 says that clear:both only works on block elements.  So I made myself a local copy of the page (setting the base href so it still worked), and the images stopped overlapping the text for the C entry when I changed the raw html so it looked like this:  <nowiki><div class="mw-headline"><div style="clear:both"><a href="/wiki/Category:C" title="Category:C">C</a></div></div></nowiki>

::: That is still not great, but maybe its a starting point? --[[User:Rdm|Rdm]] 18:06, 15 June 2010 (UTC)
:::: Not likely to work well; the div would need to be made flow-inline, to avoid breaking the header, and that would in turn break clear-both again. The best solution for this particular page (at least for the moment) is to put the images in a table, instead of off to the side the way they are. I don't have time to much with it, though. I'll revert [[Template:Header]] and let someone get the table working properly. --[[User:Short Circuit|Michael Mol]] 18:10, 15 June 2010 (UTC)
::::: I put an empty div with clear:both at the bottom of the introductory section.  That should be good enough for now. --[[User:Rdm|Rdm]] 18:13, 15 June 2010 (UTC)

== Accidental reversion ==

Don't worry about the reversion of the C example. It can be brought back after the page activity has settled down a bit. I don't know why MW didn't warn Mwn3d about the intermediate change, but I'm not worried about it. --[[User:Short Circuit|Michael Mol]] 17:55, 15 June 2010 (UTC)
: Anyway restored back sather impl adding to the current (after reversion) version; it could be I need a rest --[[User:ShinTakezou|ShinTakezou]] 17:56, 15 June 2010 (UTC)
:I think it didn't warn me because it was also warning me about new messages on my talk page? Sorry anyway. It looks like it was just comments so that's not as bad. --[[User:Mwn3d|Mwn3d]] 18:07, 15 June 2010 (UTC)

== Extra long, educational version of the task (draft) ==

I was thinking about making the task description better. This draft likely is too long to be suitable; maybe also too much educational, even though still "incomplete" someway. I don't dislike it totally however. Suggestions? --[[User:ShinTakezou|ShinTakezou]] 17:21, 16 June 2010 (UTC)

'''Preface'''

It is often said that computers "understand" 1s and 0s. Indeed, the digital electronics that makes them to work is (as the word digital suggests) a two-state elettronics (we won't talk here about the need for a [[wp:Three-state logic|"third" state]] and the fact that [[wp:Resistor|resistors]], [[wp:Capacitor|capacitors]], [[wp:Inductor|inductors]] are present of course).

Seen at the lowest level (but not so low that we can see the physics behind the scenes) we can say that every digital circuit consists of elements able to do operations on 1s and 0s (we call these two possible states ''bits'' from now on). The most basic operations correspond to the one we already know from the [[wp:Boolean algebra (logic)|boolean]] [[wp:Boolean logic|algebra]] (we can interpret the two possible states also as true and false). So, the possible operations are e.g. ''not'' , ''and'', ''or'', ''exclusive or'' (''xor'')... and several others that however can be rewritten in term of the previous one.

For each of those operations we can imagine it exists a hardware component, that we call [[wp:Logic gate|logic ''gate'']] or simply ''gate'', able to perform them. So the most elemental gates are '''NOT''', '''AND''', '''OR''', '''XOR'''.

  These images (except Xor) are not ready
 {|
  |-
  ! NOT !! AND !! OR !! XOR
  |-
  | [[Image:Not.svg|64px|alt=NOT gate]]
  | [[Image:And.svg|64px|alt=AND gate]]
  | [[Image:Or.svg|64px|alt=OR gate]]
  | [[Image:Xor.svg|64px|alt=XOR gate]]
  |}

We imagine that these are "binary" gates, accepting two inputs and yielding one output, save for '''NOT''' which is an unary gate: it has an input and an output. At each input and output corresponds a [[wp:Lead (electronics)|pin]]. Commonly gates are [[wp:Integrated circuit packaging|packaged]] together into a single [[wp:Chip carrier|package]], containing e.g. [[wp:7400 series|four gates]], and they exist also [[wp:Integrated circuit|chips]] that "implement" a N-input '''OR''', '''AND'''... However these can be done using several gates in their binary "version", so we don't consider them ''elemental''.

Indeed considering the [[wp:De Morgan's laws|De Morgan laws]] and other amenities from boolean algebra, we can see that our set of gates is still not really the minimal elemental set of gates. The discussion about which gates can be considered elemental should not ignore how these gates are created in hardware using "transistors", and the fact that a single purposely chosen gate is all we need to create our circuits.

For example, the '''NAND''' gates is a gate similar to the '''AND''', but the output is inverted (''not''ed). All the other gates can be realized using this single gate! So that the minimal set of elemental gates can contain just one element, e.g. the '''NAND''', but the '''NOR''' can be used similarly (see for example [http://tams-www.informatik.uni-hamburg.de/applets/hades/webdemos/05-switched/40-cmos/nand.html this link], that shows how a CMOS '''NAND''' and '''AND''' are realized and how they work; observe that the '''NAND''' has 4 transistors while the '''AND''' needs six transistors).

But since we are interested in a certain degree of comfort, we shall build the [[wp:Circuit diagram|schematics]] for our circuits using several not-so-elemental gates. We shall consider only three gates ('''NOT''', '''OR''' and '''AND''') as "elemental".

We can easily imagine that a complex digital circuit, if explicitly written in terms of gates '''NOT''', '''OR''' and '''AND''', can become rather hard to be understood and maintained. Exactly how it happens in programming, it is comfortable to build "functional" blocks, containing other (interconnected) functional blocks, containing other functional blocks... until we reach the most elemental blocks that can be "described" only in terms of elemental gates.

More complex "blocks" can be "''analitically''" described using [[wp:Truth table|truth tables]]: for each possible input, we write the output(s). From this table it is easy to write one (or several)
[[wp:Canonical form (Boolean algebra)|analitycal expression(s)]] that then can be [[wp:Karnaugh map|simplified]] and used as model to implement the block using the usable gates. We shall use as [[wp:Boolean logic#Other notations|notation]] a + for ''or'', the product sign · for ''and'' (or nothing: A·B = AB, inheriting mathematical conventions), a bar over a symbol or a group of symbols as ''not''; the <math>\oplus</math> (being a ''xor'') will be considered as a shortcut for
<math>\overline{A}B + A\overline{B}</math>, i.e. for the ''xor'' written in terms of ''and'', ''or'' and ''not''.

One of the most simple but useful "block" we can imagine is the one able to sum two bits. The block has two input pins (let us call them A and B) and two output pins (let us call them S and C, we'll understand later why). Using the binary numeric system, we know that

* 0 + 0 = 0
* 0 + 1 = 1
* 1 + 0 = 1
* 1 + 1 = 10

The last line, in particular, can be read the following way: one summed to one gives 0, with a carry of 1. So the two inputs are the addends, while first output pin is the result of the Sum, and the second pin is the Carry. The carry is 1 only when both input are 1. If we study the truth table of the logic ''and'' (which is realized in hardware with an '''AND''' gate), we see that its "output" is true (1) only when both inputs are true (1). So we can write:

* C = AB

Let us now study the S bit we find on the output S pin of our block. It is comparable to the
[[wp:Truth table#Exclusive disjunction|truth table of the ''xor'']] and so we can say that

* S = <math>A \oplus B</math>

The expressions for C and S, given A and B, ''describe'' our ''adder''. This is called indeed [[wp:Adder (electronics)#Half adder|''half-adder'']]. Now let us first see how easily these two expressions can be "transformed" in a digital circuit schematic, using for the gates the symbols already seen above.

<!-- [[Image:Halfadder.svg|center|300px|alt=Half adder]] -->
[[Image:Halfadder.png|center|alt=Half adder]]

We can consider this as a fuctional block of its own, and later we'll be not interested anymore in its inner details: we know its function, and we'll use it as a "black box".

The limit of a ''half-adder'' is that it can't be used to sum more bits. If we go into the process of summing two binary number by hand, exactly as it happens while summing two base-N numbers, we soon find that we need to sum not only two digits of the same column, but also the [[wp:Carry (arithmetic)|carry]] from the previous column. So, to fully be able to sum two bits, being these part of numbers consisting of more than one digits, we should have a ''block'' that accepts three inputs: two for the addends and one for the carry from the previous "column".

At this point we could build our truth table for these three inputs and two outputs, find our two expressions for outputs S and C and simplify them, and use them to build the block that we can call ''full-adder''.

But we have already "half" of the function we want, and so let us use it. The ''half-adder'' is not able to "recognize" the source of its inputs. So let us connect the S-output of a half-adder (Left) to the A-input of another half-adder (Right); now the S-output of HA-R is the sum of its A and B inputs, but A is the S-output of HA-L, i.e. it is the sum of two bits. Thus, we can be easily convinced of the fact that the S-output of HA-R is the sum of three bits. We have still two C-outputs from the half-adders, that we would like to combine so that the final carry is the correct one expected from the sum ot three bits.

These two carries can be combined using an '''OR''' (we could have used also a '''XOR''', but since for us this is not elemental, we should avoid it). Explaning this is a little bit "tricky", but we can do an empirical check [[wp:Adder (electronics)#Full adder|looking at the truth table]].

<!-- [[Image:Fulladder.svg|center|300px|alt=Full adder]] -->
[[Image:Fulladder.png|center|alt=Full adder]]

Indeed, full-adders are not realized using two half-adders and an '''OR''', since there's a more cheap way of doing it. However this approach is more clear.

Since the full-adder is able to sum also a carry from a previous stage, it can be easily used to implement an adder for more than two one-bit numbers. If we want to sum two N-bits numbers, we need simply N full-adders: each full-adder sums the n-th bits from the two input "numbers", plus the carry from the sum of the previous (n-1)th bits; therefore we can call the inputs of a full-adder A<sub>n</sub>, B<sub>n</sub>, C<sub>n-1</sub> and the outputs S<sub>n</sub> and C<sub>n</sub>. Since the unity bits can't have a carry, the carry input of the "first" full-adder (the one summing the unity bits) is set to zero. Moreover, the last carry (the carry of the last full-adder, the one summing the Most Significant Bits) can be used to know if there was an overflow.


'''References'''

* Wikipedia (inline links)
* J.Millman, A.Grabel ''Microelectronics'' (McGraw-Hill)


'''Task'''

Using only '''AND''', '''OR''' and '''NOT''' gates (''simulated'' by the bitwise logical operators of your language), create the following functional block (from bottom-up)

* XOR
* Half-adder
* Full-adder
* 4-bit adder

The '''XOR''' gate, once described in terms of '''AND''', '''NOT''' and '''OR''', can be used the same way of the other elemental gate, i.e. as an existing bitwise operator.

<!-- [[Image:4bitsadder.svg|center|300px|alt=4-bit adder]] -->
[[Image:4bitsadder.png|center|alt=4-bit adder]]


As it can be seen from the image, the 4-bit adder has 8 inputs (4 per input "number") and five outputs: 4 are the result of the sum, while the last is the overflow bit.

Solutions should try to be as descriptive as possible, making as easy as possible to identify "connections" between higher-order "blocks". It is not mandatory to replicate the syntax of higher-order blocks in the "elemental" gate blocks, i.e. elemental gate operations can be performed as usual bitwise operations, or they can be "wrapped" in a ''block'' in order to expose the same syntax of higher-order blocks, at implementers' choice.

In order to test the 4-bit adder, write a test code to sum two 4-bit numbers: set the eight input bits to certain values and show the five output bits.

: It's interesting, but I see two problems.
:# Full treatment of the task subject outside of software is outside Rosetta Code's domain. A brief explanation, with a link to someone else's full treatment, would be more appropriate. Alternately, you could treat gate logic as its own language, and apply implementations for things like mathematical operators and logic, and through sufficient examples describing ''those'', provide for a full treatment via intra-site linking.
:# Where did all that text come from? --[[User:Short Circuit|Michael Mol]] 17:28, 16 June 2010 (UTC)

# Could I write that outside (e.g. on my capo-nord/soci/xmav) and provide a link then? (In case it would be useful for hardware unaware people to dive in the POV that allowed some implementers to use words like Wire, GND and Bus, catching someway the "hardware" PoV I liked for ths task)
# From my fingers. Parts of the explanation are excerpt of stuffs I started to write (in italian) for an old educational course about "informatics" (since my idea was/is that without having an idea about how computers do their things, one could find harder to understand some concepts that could "emerge" even from everyday usage by non-programmers who usually just "click" on the right place) --[[User:ShinTakezou|ShinTakezou]] 17:39, 16 June 2010 (UTC)
Forgot the most important note for the point 2: the course was never completed nor released; even if it were, the license would be a CC non-commercial sharealike (compatible to RC one I suppose), or GFDL. [[User:ShinTakezou|ShinTakezou]] 17:43, 16 June 2010 (UTC)
:# How about [[Four bit adder/Indepth]] for supplementary details? Something over in the Encyclopedia area might work, too.
:# Ah. Sorry; I was just a bit suspicious, because your English isn't usually that good, and the text showed up pretty quickly. >.> --[[User:Short Circuit|Michael Mol]] 17:44, 16 June 2010 (UTC)

# I think I will do that if it seems useful to people, otherwise is the Task part only better than the current one? (Adding tiny bits &mdash;but what exactly...&mdash; from above?)
# It is what I am doing from this morning (with pauses of course), with emacs and translate.google as dictionary to enlarge a bit my used lemmas set,... so it was not so quick! Subsequent edits show that I should use more the preview button anyway :D --[[User:ShinTakezou|ShinTakezou]] 18:11, 16 June 2010 (UTC)

: Hi Guys,
 Wouldn't this be better off in the Category:Encyclopedia (sic) section?
 P.S. my spell checker says it should be encyclopaedia with an ae). --[[User:Paddy3118|Paddy3118]] 05:00, 17 June 2010 (UTC)

:: With an “æ”? Yes, but a lot of people have problems typing such things and US dictionaries are slipping towards using a plain “e” in those locations. ''Sic transit gloria mundi.'' –[[User:Dkf|Donal Fellows]] 12:52, 18 June 2010 (UTC)

== Motivations behind the task (and point 1 (RC domain)) ==

My idea when I've created this task was to begin something I've never started but that was in my mind since eons: implementing a small microprocessor. I know they exist languages like VHDL and Verilog (that by the way are on RC too, and this task should be suitable for them!) and more, but I would like a standalone software, and not to be too much hardware-design bound; i.e., I want an emulator, that emulates the innermost hardware in a lowlevel way, but "outside" behaves like a software emulator (I doubt Bochs and similar do their emulation so "low-level")

I don't know the exact "extension" of what languages like VHDL can do, but once I tried SPICE and it did not grasp my attention as a suitable tool for my idea.

The four bit adder was a pilot test for more similar task (if this sort of tasks are suitable for RC domain, being a little bit hardware-related) (e.g. I discovered on the run that ideas in the Go implementation, according to what I can understand of Go, can "model" a latch, while C impl can't) ... --[[User:ShinTakezou|ShinTakezou]] 18:48, 16 June 2010 (UTC)
: It sounds like something on the scale of RCRPG/RCBF/RCSNUSP, and it has its own kinds of interest. I'd say go ahead and try it, but don't use [[Template:Task]] for it, at least not soon. I'd suggest you first try modeling the basic gates (including allowing XOR to be done natively), then extend to half adder, full adder, flip-flops, etc. I noticed you didn't account for the possibility of clocks and/or propagation. (I thought of doing a C++ example that had a Clock() method, for example.)
: Find a corner of the wiki and see what you come up with; there are obviously already some HW-savvy folks around here, and maybe you'll come up with something that can be incorporated/adapted/fit with the wiki in general. Also, let me know if you come up with a gate-logic implementation of BF. My step-dad was interested in doing that a while back. ;) --[[User:Short Circuit|Michael Mol]] 18:57, 16 June 2010 (UTC)

::While I can see this being an "interesting project", you may want to be careful with the specification of the individual tasks. 

::For example this task is specified asynchronously, i.e. without any concept <i>when</i> the output is supposed to be valid (every gate introduces a delay, after all). If you're really implementing hardware and your bits have real-world consequences (because they trigger further logic which in the end makes real decisions like firing jet engines and whatnot) then you can not just let your output bits flutter around randomly for a while while you're shuffling your carry bits around. 

::Which I guess is the heart of my comment: What is the usefulness of a task that "simulates hardware"? I can see how RC can have tasks about bit-operations. I can kinda see how HW simulation might be kinda appropriate (in the sense of Verilog programming for FPGAs or ASICs; that's real programming with real languages) but in that case I'm not sure how valuable it is to require rather un-idiomatic layouts in the task descriptions. For example the shown layout of the 4-bit adder is squential (=naive) and I don't think this is how anybody doing real hardware would implement it. Imagine doing math on 64-bit numbers for a moment: after the <i>last</i> input bit has settled, your output bits are undefined (i.e. can be flipping from 0 to 1 and back a random number of times) for 65 times the delay introduced by a single full adder while you're slowly moving your carry bit from stage to stage. In other words: sequential carries maximize the execution time of the overall block. If you have that much time to waste, the you're almost always better off just running something on a ready-built processor rather than implementing your own hardware. So it is vaguely unclear to me what anybody is supposed to learn from this. [[User:Sgeier|Sgeier]] 19:12, 1 September 2010 (UTC)
:::I don't know quite how this is going to fit into RC in general yet, so I'm hoping that it can remain somewhat segregated away from the regular body of tasks by avoiding usage of, e.g. [[Template:Task]] and much of the regular infrastructure until things settle down and those interested in the hardware domain get a feel for how these tasks can/cannot tie in with the rest of the wiki. I haven't had time to monitor it, maintain separation and consider how things can tie back together later. My gut tells me that simulating hardware falls along similar lines to software implementations of programming languages and virtual machines, (See RCBF, RCSNUSP, RCH9Q+), and has analogous values and roles. (For whatever reasons, those tasks can expose complex practical consequence and behavior in their relevant languages.)
:::As for task descriptions forcing unidiomatic layouts and behaviors, and making unnecessary or incorrect assumptions about the problems they're trying to describe, people with the experience to recognize these things should generally work with the people who wrote the tasks to help them expand their understanding and improve the tasks they write in order to intersect the core idea/goal of the task writer with a broad cross-section of possible solutions without unnecessarily forcing unidiomatic code. --[[User:Short Circuit|Michael Mol]] 22:58, 1 September 2010 (UTC)

The idea as whole is enough large to be in the project category. Constructive blocks however are simple task of their own (task used in the general way, not as "RC task"); having a library of these blocks should then make it simpler (!) to realized the final project. 

Indeed, as the "4-bit adder" task was done, it proved to be badly planned. The general way I set up things do not allow for all the needed things to do all kinds of circuits needed for the final project. E.g. as I did both C and Sather impl can't describe a latch at all, even expanding to allow a clock; deeper thoughts made me believe Go too could have problem, putting the program in a never-ending wait for ready signal, but I didn' analyse it further.

As already said, my aim is not to build things too much hardware-design bound. But I am realizing that maybe some "real" simulation trick must be used sooner or later (again, latch ... I'll be going to experiment on my own before saying more, but I vaguely see they can be problematic in a straightforward approach). Being curious to answer the question "is this idea not new?", I searched the net and '''of course''' I've found already done stuffs, but as real hardware design (e.g. the harp project, and YASEP), very interesting for anybody interested in.

I think the "processor" can have as instruction set BF, with the understanding that <tt>.</tt> and <tt>,</tt> can interface the cpu to any suitable device (that must be projected purposely) and likely more instructions for jump could be useful to augment performance (the <nowiki>[</nowiki> needs look-ahead and a <nowiki>[</nowiki>-stack ...). Currently my ideas about how all this can be realized are rather foggy; however I've installed some VHDL tools, going to see how this task would look with it. -[[User:ShinTakezou|ShinTakezou]] 10:36, 17 June 2010 (UTC)

:For a latch, two nor gates with their outputs cross-wired to the other's input should be sufficient?  You do need to represent their "previous state", which complicates things, but I do not think you need additional logic?  --[[User:Rdm|Rdm]] 12:20, 17 June 2010 (UTC)

:: By "definition", it can be done that way. But note that C impl is sequential, so it is not possible to use as input an output produced later; on the other hand, about Go, it seems it would wait for the input to be available... the problem could be that one input will be not available until the output is generated, and it is not generated since both gates will be awaiting for a value to be set in the connection. What does it happen if a random value (0 or 1) is set preliminarly as output for the NOR gates? Does the simulated latch becomes "bistable" as it happens in reality, or rather it will expose a wrong (to be analysed) behaviour? It is of course implementation specific and can be fixed, but current implementation and in particular my sequential approach to the "description" of the circuit does not help. --[[User:ShinTakezou|ShinTakezou]] 18:23, 17 June 2010 (UTC)

::: I was thinking of something like this: 
```c>#include <stdio.h

 
typedef char pin_t;
#define IN const pin_t *
#define OUT pin_t *
#define STATE pin_t *
#define PIN(X) pin_t _##X; pin_t *X = & _##X;
#define V(X) (*(X))
 
/* a NOT that does not soil the rest of the host of the single bit */
#define NOT(X) (~(X)&1)
 
#define NOR(X,Y) (NOT((X)|(Y)))

void latch(IN a, STATE b, STATE c, IN d)
{
  V(b)= NOR(V(a),V(c));
  V(c)= NOR(V(b),V(d));
}

int main()
{
  PIN(p0); PIN(p1); PIN(p2); PIN(p3);
  V(p0)= V(p1)= V(p2)= V(p3)= 0;
 
  int j;
  for (j= 0; j < 16; j++) {
    int bit= 4&j ?1 :0;
    V(p0)= 1&j ?bit :NOT(bit);
    V(p3)= 2&j ?bit :NOT(bit);
    if (V(p0) && V(p3)) continue; /* forbidden case */
    else if (V(p0)) printf("%d\tset\t", j);
    else if (V(p3)) printf("%d\treset\t", j);
    else printf("%d\t\t", j);
    printf("%d%d%d%d\t", V(p0),V(p1),V(p2),V(p3));
    latch(p0, p1, p2, p3);
    printf("%d%d%d%d\t", V(p0),V(p1),V(p2),V(p3));
    if (V(p2)) printf("is on\n");
    else if (V(p1)) printf("is off\n");
    else printf("is switching\n");
    printf("\t\t");
    V(p0)= 0;
    V(p3)= 0;
    printf("%d%d%d%d\t", V(p0),V(p1),V(p2),V(p3));
    latch(p0, p1, p2, p3);
    printf("%d%d%d%d\t", V(p0),V(p1),V(p2),V(p3));
    if (V(p2)) printf("is on\n");
    if (V(p1)) printf("is off\n");
  }
  return 0;
}
```
 --[[User:Rdm|Rdm]] 21:05, 17 June 2010 (UTC)
:::: P.S. note that that latch function really needs to be run twice to reach steady state.  However, because I made one gate faster than the other, this is only apparent some of the time.  --[[User:Rdm|Rdm]] 13:28, 18 June 2010 (UTC)
::::: This is interesting, I will reconsider the whole topics once healed by failing with vhdl quick-attempts with found free (as in beer and as in speech) tools... --[[User:ShinTakezou|ShinTakezou]] 17:47, 28 July 2010 (UTC)

== On Digital Simulators ==
Hi ShinTakezou, I've been using digital simulators for a number of decades now, and work for a company - Infineon Technologies, That has several microprocessor designs. You might want to look at transaction based modelling for a 'higher level' model of a processor. In the industry we might juggle several models of the same design, each with their own trade-offs.

* At the lowest level you would have a model of the masks used to selectively expose the chip. 
* Higher up, you might model the individual, interconnected transistors. 
* Higher up you might model the individual gates of design (where the magic of how the gates work is only needed by the person who creates the gate libraries).
* Higher up you have RTL; register transfer level modelling where a designer might create registers and clocks and model data transfer and modifications w.r.t. clocks - most digital design is done at this level using the Verilog and VHDL languages.
 Synthesis and then layout tools are used to translate RTL to gate and then to silicon masks.
* Higher up than RTL lie so-called behavioural modelling simulators and languages/language libraries and methodologies. 

You might want to look at [http://www.systemc.org/home/ SystemC]. It is a free library and methodology for simulating digital designs in C++. Although the library and its simulator exist in a free form, you will of course have to pay if you were wanting to, say, Synthesize your design down to gates; and commercial SystemC simulators make it so much more easy to debug and navigate your design. --[[User:Paddy3118|Paddy3118]] 18:56, 28 July 2010 (UTC)

==Logic Simulators==

A quick note on how a logic simulator provides the illusion of parallelism needed to a typical logic circuit: a technique known as a "two list simulator". The "circuit" is represented by a set of objects that represent the components, and a second set the represent the wires (connectivity). Simulation proceeds in two phases: first, we tell each component to update its outputs based on the current value of its inputs. Next, we tell each wire to propagate the output of the upstream component to the input of the downstream component(s). While doing this, we note if any values actually change -- if none do then the current timestep is done (as an optimization, only those components whose inputs have changed are updated on each iteration). The term "two list simulation" comes the need to maintain these two sets of things to update: the components, and the wires..

(ps. Small world: I, too, used to work for Infineon -- 10 years ago when it was spun off from Siemens -- these days I'm at NVIDIA
--[[User:Davewhipp|DaveWhipp]])

==C++ code size==
Is the C++ example so large because it needs to be that large to fulfil the task? I don't think the example given shows C++ in a good light due to its extreme length. Maybe it was copied from existing code used for another purpose? --[[User:Paddy3118|Paddy3118]] 20:07, 30 September 2011 (UTC)
: Meh.  The C++ code appears to be quite thorough and models the concept of physical devices pretty well.  It's wordy, but that's C++.  It actually can notice connection errors because everything exists as a device down to the input voltage sources.  Compared to it, the C example is completely worthless as a "simulation", where you might have forgotten to put a voltage on one of the input pins and the program will merrily give you garbage output without noticing a thing.  I suspect a lot of other languages also have this problem, which makes the task sort of much ado about nothing.  It's very possible the C++ code can be shortened, but if it ends up like the C code, I'd prefer the way it is now, however much I hate long winded code. --[[User:Ledrug|Ledrug]] 21:42, 30 September 2011 (UTC)
:: But the task description does not even mention voltages, nor does it ask us to build up the gates on simulated silicon (or any other such model).  Meanwhile, you can build four bit adders using optical or mechanical gates, where voltages are not a meaningful concept.  So it's not far to describe this issue as a problem with the implementations in other languages. --[[User:Rdm|Rdm]] 22:27, 30 September 2011 (UTC)
::: Look, it's not my word: the C example called the inputs "pins" and the assignment statement "V", what was I supposed to infer?  The desc does use the word "simulate", btw. I don't really care if you simulate a voltage or water pressure, the matter is task description does seem to want some reasonable simulation despite wasting a lot of effort talking about how one should use bit mask as input/output values.  The C code along with some translated code did the task without a remotely systematic way of simulating a circuit (can you verify input pins are all with valid values? how much work is it change the code to do a 128 bit adder instead of 4?), it's ironic to call this a simulation. --[[User:Ledrug|Ledrug]] 23:23, 30 September 2011 (UTC)
::::Ok, and the task does say "chip" which would also imply electronics.  But I would also draw a distinction between other languages in general (some of which have implementations where the 128 bit adder is trivial) and the C implementation in specific (which I have not studied).  Still... if the task meant for physics to be simulated, instead of logic, I imagine it would have said something about the way physics was supposed to be simulated? --[[User:Rdm|Rdm]] 01:04, 1 October 2011 (UTC)

::I suspect the C++ (not the C) code, rather than being idiomatic, is "enterprise-y" the kind of code you get when people refuse to say no or the people in charge think "I might just need this" or "if I put this in, I can't be fired for leaving it out". There are several examples already existing that could be used to show what is expected from an answer. RC rarely requires any example of that size. It is just as unreasonable to have the C++ example as it would be to add a code golf solution. Simulations are supposed to reflect an ''appropriate'' abstraction of what is simulated. --[[User:Paddy3118|Paddy3118]] 10:26, 1 October 2011 (UTC)
::: Actually, the C++ code looks very idiomatic to me. Certainly not the most efficient, but it's very clear and readable in function. (Actually, frightfully so; I've never seen C++ code that clear) Also, an opinion on the code golf reference...Code golf is generally undesirable because it reduces clarity of code, and adds perverse incentives for example writers. --[[User:Short Circuit|Michael Mol]] 11:42, 1 October 2011 (UTC)

Could someone move the C++ code to a separate sub-page at least? That huge wad of code is over 16 pages long where the others are less than two pages! It is clearly [http://oxforddictionaries.com/definition/anomalous anomalous] in size.
(P.S. it would be instructive to know a little about how the code was written. Was it written for this RC task alone? Was a lot of the code generated by an IDE? ...) --[[User:Paddy3118|Paddy3118]] 04:59, 1 October 2011 (UTC)
: Concur on subpage, and done. I think it was very probably written for Rosetta Code; the usage of a namespace named 'rosetta' is good indicator. --[[User:Short Circuit|Michael Mol]] 11:42, 1 October 2011 (UTC)

== Clojure solution ==

I added a second Clojure solution to show what I think is more idiomatic Clojure code.  Because Clojure does not have TCO (which unavailable in Java), it's generally not a good idea to write recursive code in the old Lisp style as done in the original solution ('''n-bit-adder''' function).  If possible, you should use the special form '''recur''' in a tail-call position to avoid growing the stack.  But most of the time, it's better to find a solution using '''reduce''' if you can.

The second solution shows a couple of interesting language features.  First, it uses destructuring to simplify extracting multiple values out of a vector (see my '''full-adder''' function).  Second, it uses '''reduce''' in '''nbit-adder''' which is a good thing for Clojure programmers to learn.   Also, the pre-condition (''':pre''') is a nice way to check for errors.  It can be turned off without having to change the source.

The choice of big endian or little endian is arbitrary, but I think big endian is easier to read.  That requires a little extra fiddling with how the bits are fed to the reduce function, but also shows off the '''rseq''' function, which is constant-time reverse for vectors.

== I made a latch! ==

It wasn't my original goal.  Instead my first step was a concurrent implementation a little more robust than MizardX's.  That method probably would be sufficient to build a latch, but I wanted something more resistant to blocking and deadlocking.  Anyway,

```txt

=== RUN Test-4
--- PASS: Test-4 (0.01 seconds)
        logic_test.go:17: power up
        logic_test.go:20: S: 0 R: 0 Q: 1 NQ: 0
        logic_test.go:22: pulling R = 1
        logic_test.go:25: S: 0 R: 1 Q: 0 NQ: 1
        logic_test.go:27: pulling R = 0
        logic_test.go:30: S: 0 R: 0 Q: 0 NQ: 1
        logic_test.go:32: pulling S = 1
        logic_test.go:35: S: 1 R: 0 Q: 1 NQ: 0
        logic_test.go:37: pulling S = 0
        logic_test.go:40: S: 0 R: 0 Q: 1 NQ: 0

```

The concurrency isn't very exotic, and could probably be coded pretty easily in most languages with any kind of concurrency support, but I'm not sure it's quite ready for another task.  It still runs asyncronously, with no concept of simulation time, so you have to do the hokey thing of just sleeping for a bit to let it run.  I have a vague idea of using a priority queue for simulation time.  Maybe that will be next. &mdash;[[User:Sonia|Sonia]] 01:05, 9 July 2012 (UTC)
