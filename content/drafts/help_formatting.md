+++
title = "Help:Formatting"
description = ""
date = 2019-07-04T17:13:53Z
aliases = []
[extra]
id = 2466
[taxonomies]
categories = []
tags = []
+++

Editing on a wiki site is a bit different from editing plain [[HTML]]. While HTML is allowed, there are some extra things.

==The Editor==
If you enable JavaScript on your browser, you get a neat little toolbar to help you out with wiki editing. You can use these buttons you help you learn how to do things for yourself. First, there are '''Bold''' and ''Italics'' buttons. Next, there is a button you can use to [[Main Page|link to an internal page]], and one you can use to [http://www.rosettacode.org link to an external page]. There is a button for making text into a header (like "The Editor" shown above). You can link to an image on the site with the next button:

[[Image:Fifo.gif]]

Or to another form of media with the one after that. You can show mathematical formulas in [[LaTeX]] with the next button. The next button allows you to show text without wiki formatting in case your language uses text that wiki has reserved for itself (for example, two apostrophes in a row). The next button adds your signature to things (used mostly for talk pages), and the last button adds a horizontal bar, but that gets annoying.

==Wiki Formatting on Your Own==
If you would like to break yourself away from the GUI editor options, you can click the "edit" tab on almost any page to see the wikicodes for yourself, or just type the formatting codes in yourself.

<nowiki>'''Bold text'''</nowiki>

<nowiki>''Italic text''</nowiki>

<nowiki>[[InternalLink]] or [[InternalLink|Display title]]</nowiki> to change what words show up on the page for the link.

<nowiki>[externalLink] or [externalLink Display title]</nowiki> to change what words show up on the page for the link.

<nowiki>[[wp:wikipedia link]] or [[wp:wikipedia link|Display title]]</nowiki> to link to a wikipedia page with the name that follows "wp:"

<nowiki>[[oeis:AXXXXXX]] or [[oeis:AXXXXXX|Display title]]</nowiki> where AXXXXXX is the sequence index, to link to an OEIS sequence page. Leading zeros may be omitted. A1 is the same as A000001.

<nowiki>=Big headline=</nowiki> (has horizontal bar, not a button in the JavaScript editor)

<nowiki>==Small headline==</nowiki> (has horizontal bar)

<nowiki>
### Medium headline
</nowiki> (not a button in the JavaScript editor)

<nowiki>[[Image:Example.jpg]]</nowiki>

<nowiki>[[Media:Example.ogg]]</nowiki>

<nowiki>
```c>C code example
```
</nowiki


<nowiki><math>LaTeX formula</math></nowiki>

<nowiki><nowiki>Don't use wiki formatting</nowiki></nowiki>

Your username: <nowiki>--~~~</nowiki> (not a button in the JavaScript editor)

Your signature (username, date, and time): <nowiki>--~~~~</nowiki>

Just the date and time: <nowiki>--~~~~~</nowiki> (not a button in the JavaScript editor)

Horizontal bar: <nowiki>----</nowiki>

Numbered list example (raw wiki text):

```txt
#Item 1
#Item 2
#Item 3

#Item 1
```

What it displays:
#Item 1
#Item 2
#Item 3

#Item 1

<nowiki>*Bulleted text</nowiki> (shown below, not a button in the JavaScript editor)

<nowiki>:Indented text</nowiki>. Use multiple colons to indent multiple times. (not a button in the JavaScript editor)

<nowiki><tt>Monospaced font text</tt></nowiki> usually used for talking about commands in sentences. (not a button in the JavaScript editor)

<nowiki>REDIRECT [[other page]]</nowiki> to redirect visitors of this page to the "other page." (not a button in the JavaScript editor)

<nowiki>[[Category:category page]]</nowiki> to add a page to a category. (shows a link to the category on the bottom of the page, but not in the text...not a button in the JavaScript editor)

<nowiki>[[:Category:category page]] or [[:Category:category page|Display title]]</nowiki> to insert a link to a category page into the text of a page without adding it to the category. (not a button in the JavaScript editor)

==Useful Tips, Characters, and Codes==

One way to make sure that your edits have come out just like you want is to preview them before submitting them. Clicking the "Show preview" button before you save your page will show you how the page will look with whatever content you have added, but it won't bother anyone else if it's wrong. Even after all of this it may still not look right, though. So here are a few more tips:

*To add a newline. simply insert two returns or add a <nowiki>
</nowiki>. (the HTML way will be a slightly smaller line break)
*To add certain math characters (for those with no LaTeX experience) ''with Windows'', hold "Alt" and press the following codes on your keypad (no padding with zeros):
:*26 → (Implies)
:*170 ¬ (Not)
:*224 through 234 αßΓπΣσµτΦΘΩ (Greek letters: alpha, big beta, big gamma, pi, big sigma, little sigma, mu, tau, big phi, big theta, big omega)
:*236 ∞ (Infinity)
:*241 through 243 ±≥≤ (Plus/minus, less than/equal to, greater than/equal to)
:*246 ÷ (Division)
:*247 ≈ (Approximately)
:*248 ° (Degrees)
:*251 √ (Square root. Will not extend across entire formula...parentheses suggested)
:*253 ² (Squared)
: On Linux with German keyboard layout, the following key combinations will usually work:
:* AltGr + U: →
:* AltGr + ^: ¬
:* AltGr + Shift + Q: Ω
:* AltGr + k: ĸ
:* AltGr + m: µ
:* AltGr + .: ·
:* AltGr + ;: ×
:* AltGr + :: ÷
:* AltGr + 1 through 3: ¹²³
:* AltGr + 5: ½
:* AltGr + 4: ¼
:* AltGr + Shift + 2: ⅛
:* AltGr + Shift + 5: ⅜
:* AltGr + Shift + 6: ⅝
:* AltGr + Shift + 7: ⅞
:* AltGr + Shift + 9: ±
: In any case, independent of operating system and keyboard layout, you can use the HTML entities:
:* &amp;rarr; &rarr;
:* &amp;not; &not;
:* &amp;alpha;&amp;beta;&amp;gamma;&amp;delta;&amp;epsilon;&amp;zeta;&amp;eta;&amp;theta; &alpha;&beta;&gamma;&delta;&epsilon;&zeta;&eta;&theta;
:* &amp;iota;&amp;kappa;&amp;lambda;&amp;mu;&amp;nu;&amp;xi;&amp;omicron;&amp;pi; &iota;&kappa;&lambda;&mu;&nu;&xi;&omicron;&pi;
:* &amp;rho;&amp;sigma;&amp;tau;&amp;upsilon;&amp;chi;&amp;phi;&amp;psi;&amp;omega; &rho;&sigma;&tau;&upsilon;&chi;&phi;&psi;&omega;
:* &amp;Alpha;&amp;Beta;&amp;Gamma;&amp;Delta;&amp;Epsilon;&amp;Zeta;&amp;Eta;&amp;Theta; &Alpha;&Beta;&Gamma;&Delta;&Epsilon;&Zeta;&Eta;&Theta;
:* &amp;Iota;&amp;Kappa;&amp;Lambda;&amp;Mu;&amp;Nu;&amp;Xi;&amp;Omicron;&amp;Pi; &Iota;&Kappa;&Lambda;&Mu;&Nu;&Xi;&Omicron;&Pi;
:* &amp;Rho;&amp;Sigma;&amp;Tau;&amp;Upsilon;&amp;Chi;&amp;Phi;&amp;Psi;&amp;Omega; &Rho;&Sigma;&Tau;&Upsilon;&Chi;&Phi;&Psi;&Omega;
:* &amp;infin; &infin;
:* &amp;plusmn; &plusmn;
:* &amp;le;&amp;ge;&amp;ne;&amp;asymp; &le;&ge;&ne;&asymp;
:* &amp;middot;&amp;times;&amp;divide; &middot;&times;&divide;
:* &amp;deg; &deg;
:* &amp;radic; &radic;
:* &amp;sup1;&amp;sup2;&amp;sup3; &sup1;&sup2;&sup3;
*To add superscripted text, put <nowiki><sup></sup></nowiki> around it. <nowiki><sup>example</sup></nowiki> shows <sup>example</sup>.
*To add subscripted text, put <nowiki><sub></sub></nowiki> around it. <nowiki><sub>example</sub></nowiki> shows <sub>example</sub>.
*To show your source code in a box, add a <nowiki><lang></nowiki> tag with a programming language as an argument:

```c
int main(char* argv, int argc){
   printf("Hello, World!");
}
```

(See [[Help:Syntax highlighting]] for language arguments)
*To show text with wikicode in a code box, add a space before each line (including blank lines):

''This is italic text.''
 ''This is italic text in a box.''
*To show text without wikicode in a box, add a <nowiki>
```txt
</nowiki> tag:

''This is italic text.''

```txt
''This is not italic text in a box, but it shows the wikicode for italics.''
```

*Putting two spaces after a period (as is common practice) will only show up as one space when typing plain text. It basically wastes server space.
*If you have a question about something on a page, ask! Sometimes even we are wrong about coding, so if you think you see a problem or don't understand something, click the "discussion" tab at the top of the page and tell us about it.
*See [[Help:Contribute Content]] for more help on specific types of pages.

==Guidelines (not rules)==
*Spelling and grammar count...PROOFREAD! [[Firefox]] has spell check built-in (tools → options → advanced tab → "Check my spelling as I type").
*When you add programming examples to a page, make sure you insert them in the proper alphabetical order.
:*Case-insensitive
:*Longer words come after shorter words that start with the same sequence ("bar" comes before "barber" and [[C]] comes before [[C++]])
*If you change only a small portion of a page (correcting spelling, adding punctuation, etc.), check the "This is a minor edit" box so that people can filter out typo fixes when they search (adding an entire programming example is not a minor edit).
*Try to make the most of each edit. Five edits to a page in a ten minute span crowds the [[Special:Recentchanges|recent changes]] page.
*Try to add explanation in sentences to more complex examples. Write as if someone who has never heard of your language is reading.
*Add your signature (<nowiki>--~~~~</nowiki>) to the end of messages on talk pages, and indent responses underneath previous messages:

Message 1.

:Response to message 1.

::Response to response to message 1.

:Second response to message 1.

Message 2.

:Response to message 2.
==External Links==
* [[wp:Help:Contents/Editing_Wikipedia|Editing Wikipedia]]
* [[wp:Wikipedia:Cheatsheet|Cheatsheet]]
* [[wp:Wikipedia:Tutorial|Tutorial]]
* [[wp:Wikipedia:How_to_edit_a_page|How to edit a page]] contains details about markup
* [http://meta.wikimedia.org/wiki/Help:Displaying_a_formula Help:Displaying a formula] explains TeX syntax for <nowiki><math>...</math></nowiki> tags.
