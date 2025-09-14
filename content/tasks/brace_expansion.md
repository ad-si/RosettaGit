+++
title = "Brace expansion"
description = ""
date = 2019-08-23T02:02:13Z
aliases = []
[extra]
id = 17127
[taxonomies]
categories = ["String manipulation", "task"]
tags = []
languages = [
  "11l",
  "autohotkey",
  "common_lisp",
  "cpp",
  "d",
  "elixir",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "simula",
  "tcl",
  "zkl",
]
+++

## Task

Brace expansion is a type of parameter expansion [[wp:Bash_%28Unix_shell%29#Brace_expansion|made popular by Unix shells]], where it allows users to specify multiple similar string parameters without having to type them all out. E.g. the parameter <code>enable_{audio,video}</code> would be interpreted as if both <code>enable_audio</code> and <code>enable_video</code> had been specified.

{{task heading}}

Write a function that can perform brace expansion on any input string, according to the following specification.

Demonstrate how it would be used, and that it passes the four test cases given below.

{{task heading|Specification}}

In the input string, balanced pairs of braces containing comma-separated substrings <small>(details below)</small> represent ''alternations'' that specify multiple alternatives which are to appear at that position in the output. In general, one can imagine the information conveyed by the input string as a tree of nested alternations interspersed with literal substrings, as shown in the middle part of the following diagram:

<!-- BEGINNING OF MAIN DIAGRAM -->
<table align="center" style="margin-top:0.8em; margin-bottom:1em; border:double 3px #bbb; background:#F9F9F9; color:#111; line-height:1.5em"><tr><td>
<div style="padding:0.4em 0.6em; margin:0 0 0 0.4em; background:#ddd; font-family:'DejaVu Sans Mono','Courier New',Consolas,Monaco,monospace"><nowiki>It{{em,alic}iz,erat}e{d,}</nowiki></div>
</td><td>
<div style="padding:0 0.6em; text-align:center; color:#665">''parse''
―――――▶
&zwnj;</div>
</td><td>
<table style="border-spacing:0; border-collapse:collapse; text-align:center; background:#D5B874; font-family:'DejaVu Sans Mono','Courier New',Consolas,Monaco,monospace">
  <tr>
    <td style="padding:0 0 0 0.7em;">It</td>
    <td style="padding:0.1em 0 0.1em 0;">
      <table style="border-spacing:0 0.3em;">
        <tr>
          <td style="padding:0" rowspan="2">
            <div style="font-size:155%; line-height:1.15em; position:relative; left:0.32em; color:#644709">⎧
⎨
⎩</div>
          </td>
          <td style="padding:0 0.4em; background:#E5CD94">
            <table style="border-spacing:0 0.2em; position:relative; right:0.25em">
              <tr>
                <td style="padding:0" rowspan="2">
                  <div style="font-size:100%;line-height:1em; position:relative; left:0.26em; color:#644709">⎧
⎨
⎩</div>
                </td>
                <td style="padding:0 0.4em; background:#F3E0B3">em</td>
                <td style="padding:0" rowspan="2">
                  <div style="font-size:100%; line-height:1em; position:relative; right:0.26em; color:#644709">⎫
⎬
⎭</div>
                </td>
              </tr>
              <tr><td style="padding:0 0.5em; background:#F3E0B3">alic</td></tr>
            </table>
          </td>
          <td style="padding:0 0 0 0; background:#E5CD94">
            <div style="position:relative; right:0.5em">iz</div>
          </td>
          <td style="padding:0" rowspan="2">
            <div style="font-size:155%; line-height:1.15em; position:relative; right:0.32em; color:#644709">⎫
⎬
⎭</div>
          </td>
        </tr>
        <tr><td style="padding:0; background:#E5CD94" colspan="2">erat</td></tr>
      </table>
    </td>
    <td style="padding:0">e</td>
    <td style="padding:0 0.32em 0 0.2em">
      <table style="border-spacing:0 0.2em;">
        <tr>
          <td style="padding:0" rowspan="2">
            <div style="font-size:100%;line-height:1em; position:relative; left:0.28em; color:#644709">⎧
⎨
⎩</div>
          </td>
          <td style="padding:0 0.5em; background:#E5CD94">d</td>
          <td style="padding:0" rowspan="2">
            <div style="font-size:100%;line-height:1em; position:relative; right:0.26em; color:#644709">⎫
⎬
⎭</div>
          </td>
        </tr>
        <tr><td style="padding:0; background-color:#E5CD94; background-image: linear-gradient(-45deg, #E5CD94 25%, #D5B874 25%, #D5B874 50%, #E5CD94 50%, #E5CD94 75%, #D5B874 75%, #D5B874); background-size: 0.5em 0.5em;">&zwnj;</td></tr>
      </table>
    </td>
  </tr>
</table>
</td><td>
<div style="padding:0 0.6em; text-align:center; color:#665">''expand''
―――――▶
&zwnj;</div>
</td><td style"padding:0"><div style="padding:0.5em 0.6em; margin:0.4em 0.4em 0.4em 0; background:#ddd; font-family:'DejaVu Sans Mono','Courier New',Consolas,Monaco,monospace; line-height:1.35em"><span style="padding:0.1em 0;background:#D5B874">It<span style="padding:0.1em 0;background:#F3E0B3">em</span><span style="padding:0.1em 0;background:#E5CD94">iz</span>e<span style="padding:0.1em 0;background:#E5CD94">d</span></span>
<span style="padding:0.1em 0;background:#D5B874">It<span style="padding:0.1em 0;background:#F3E0B3">em</span><span style="padding:0.1em 0;background:#E5CD94">iz</span>e</span>
<span style="padding:0.1em 0;background:#D5B874">It<span style="padding:0.1em 0;background:#F3E0B3">alic</span><span style="padding:0.1em 0;background:#E5CD94">iz</span>e<span style="padding:0.1em 0;background:#E5CD94">d</span></span>
<span style="padding:0.1em 0;background:#D5B874">It<span style="padding:0.1em 0;background:#F3E0B3">alic</span><span style="padding:0.1em 0;background:#E5CD94">iz</span>e</span>

<span style="padding:0.1em 0;background:#D5B874">It<span style="padding:0.1em 0;background:#E5CD94">erat</span>e<span style="padding:0.1em 0;background:#E5CD94">d</span></span>
<span style="padding:0.1em 0;background:#D5B874">It<span style="padding:0.1em 0;background:#E5CD94">erat</span>e</span></div>
</td></tr>
<tr style="text-align:center; color:#665">
  <td><small>input string</small></td>
  <td></td>
  <td><small>alternation tree</small></td>
  <td></td>
  <td><small>output ''(list of strings)''</small></td>
</tr></table>
<!-- END OF MAIN DIAGRAM -->

This tree can in turn be transformed into the intended list of output strings by, colloquially speaking, determining all the possible ways to walk through it from left to right while only descending into ''one'' branch of each alternation one comes across ''(see the right part of the diagram)''. When implementing it, one can of course combine the parsing and expansion into a single algorithm, but this specification discusses them separately for the sake of clarity.

'''Expansion''' of alternations can be more rigorously described by these rules:

<!-- BEGINNING OF "EXPANSION" SIDE-DIAGRAM -->
<table style="margin:0.4em; float:right; position:relative; top:-0.8em; padding:0.22em 0.5em; background:#F9F9F9; border:solid 1px #ccc; color:#111; line-height:1.2em; font-family:'DejaVu Sans Mono', 'Courier New', Consolas, Monaco;"><tr>
<td style="padding:0;">
  <table style="border-spacing:0 0.2em; background:#D5B874">
    <tr>
      <td style="padding:0 0.1em 0 0.6em; background:#E5CD94">a</td>
      <td style="padding:0; background:#E5CD94">
        <table style="border-spacing:0 0.2em">
          <tr>
            <td style="padding:0" rowspan="2">
              <div style="font-size:100%; color:#644709; line-height:1em; position:relative; left:0.27em">⎧
⎨
⎩</div>
            </td>
            <td style="padding:0 0.35em; background:#F3E0B3">2</td>
            <td style="padding:0" rowspan="2">
              <div style="font-size:100%; color:#644709; line-height:1em; position:relative; right:0.25em">⎫
⎬
⎭</div>
            </td>
          </tr>
          <tr><td style="padding:0 0.35em; background:#F3E0B3">1</td></tr>
        </table>
      </td>
      <td style="padding:0 0.06em 0 0.12em; background:#E5CD94">b</td>
      <td style="padding:0; background:#E5CD94">
        <table style="border-spacing:0 0.2em">
          <tr>
            <td style="padding:0" rowspan="3">
              <div style="font-size:115%; color:#644709; line-height:1.19em; position:relative; left:0.25em">⎧
⎨
⎩</div>
            </td>
            <td style="padding:0 0.4em; background:#F3E0B3">X</td>
            <td style="padding:0" rowspan="3">
              <div style="font-size:115%; color:#644709; line-height:1.19em; position:relative; right:0.25em">⎫
⎬
⎭</div>
            </td>
          </tr>
          <tr><td style="padding:0 0.4em; background:#F3E0B3">Y</td></tr>
          <tr><td style="padding:0 0.4em; background:#F3E0B3">X</td></tr>
        </table>
      </td>
      <td style="padding:0 0.6em 0 0.1em; background:#E5CD94">c</td>
    </tr>
  </table>
</td>
<td><div style="padding:0 0.6em;text-align:center; color:#665">&#10230;</div></td>
<td style="padding:0; background:#D5B874"><table style="border-spacing:0 0.25em;">
  <tr><td style="padding:0 0.4em; background:#E5CD94">a2bXc</td></tr>
  <tr><td style="padding:0 0.4em; background:#E5CD94">a2bYc</td></tr>
  <tr><td style="padding:0 0.4em; background:#E5CD94">a2bXc</td></tr>
  <tr><td style="padding:0 0.4em; background:#E5CD94">a1bXc</td></tr>
  <tr><td style="padding:0 0.4em; background:#E5CD94">a1bYc</td></tr>
  <tr><td style="padding:0 0.4em; background:#E5CD94">a1bXc</td></tr>
</table></td>
</tr></table>
<!-- END OF "EXPANSION" SIDE-DIAGRAM -->

<ul style="margin-bottom:0.9em">
<li>
An alternation causes the list of alternatives that will be produced by its parent branch to be increased &#119899;-fold, each copy featuring <u>one</u> of the &#119899; alternatives produced by the alternation's child branches, in turn, at that position.
</li>
<li>
This means that multiple alternations inside the same branch are cumulative  ''(i.e. the complete list of alternatives produced by a branch is the string-concatenating "[[wp:Cartesian product|Cartesian product]]" of its parts)''.
</li>
<li>
All alternatives (even duplicate and empty ones) are preserved, and they are ordered like the examples demonstrate  ''(i.e. "[[wp:Lexicographical_order|lexicographically]]" with regard to the alternations)''.
</li>
<li>
The alternatives produced by the root branch constitute the final output.
</li>
</ul>

'''Parsing''' the input string involves some additional complexity to deal with escaped characters and "incomplete" brace pairs:

<!-- BEGINNING OF "PARSING" SIDE-DIAGRAM -->
<table style="margin:0.4em; border-collapse:collapse; clear:both; float:right; position:relative; top:-0.8em; padding:0.22em 0.5em; background:#F9F9F9; border:solid 1px #ccc; color:#111; line-height:1.2em; text-align:center; font-family:'DejaVu Sans Mono','Courier New',Consolas,Monaco,monospace"><tr>
<td style="padding:0 0 0 0.6em"><div style="padding:0.35em 0.5em; margin:0; background:#ddd">a\\{\\\{b,c\,d}</div></td>
<td style="padding:0"><div style="padding:0 0.6em; color:#665">&#10230;</div></td>

<td style="padding:0.4em 0.6em 0.4em 0">
  <table style="border-spacing:0 0.2em; border-collapse:collapse; background:#D5B874"><tr>
    <td style="padding:0 0 0 0.6em">a\\</td>
    <td style="padding:0.2em 0.28em 0.2em 0.10em;">
      <table style="border-spacing:0 0.2em">
        <tr>
          <td style="padding:0" rowspan="2">
            <div style="font-size:100%; color:#644709; line-height:1em; position:relative; left:0.25em">⎧
⎨
⎩</div>
          </td>
          <td style="padding:0 0.35em; background:#E5CD94">\\\{b</td>
          <td style="padding:0" rowspan="2">
            <div style="font-size:100%; color:#644709; line-height:1em; position:relative; right:0.25em">⎫
⎬
⎭</div>
          </td>
        </tr>
        <tr><td style="padding:0 0.35em; background:#E5CD94">c\,d</td></tr>
      </table>
    </td>
  </tr></table>
</td>
</tr><tr>
  <td style="border-top:solid 1px #ccc; padding:0 0 0 0.6em"><div style="padding:0.35em 0.5em; margin:0; background:#ddd">{a,b{c{,{d}}e}f</div></td>
  <td style="border-top:solid 1px #ccc; padding:0"><div style="padding:0; color:#665">&#10230;</div></td>
  <td style="border-top:solid 1px #ccc; padding:0.4em 0.6em 0.4em 0">
  <table style="border-spacing:0 0.2em; border-collapse:collapse; background:#D5B874"><tr>
    <td style="padding:0 0 0 0.6em">{a,b{c</td>
    <td style="padding:0.2em 0.1em 0.2em 0.1em;">
      <table style="border-spacing:0 0.2em">
        <tr>
          <td style="padding:0" rowspan="2">
            <div style="font-size:100%; color:#644709; line-height:1em; position:relative; left:0.25em">⎧
⎨
⎩</div>
          </td>
          <td style="padding:0 0.35em; background-color:#E5CD94; background-image: linear-gradient(-45deg, #E5CD94 25%, #D5B874 25%, #D5B874 50%, #E5CD94 50%, #E5CD94 75%, #D5B874 75%, #D5B874); background-size: 0.5em 0.5em;">&zwnj;</td>
          <td style="padding:0" rowspan="2">
            <div style="font-size:100%; color:#644709; line-height:1em; position:relative; right:0.25em">⎫
⎬
⎭</div>
          </td>
        </tr>
        <tr><td style="padding:0 0.35em; background:#E5CD94">{d}</td></tr>
      </table>
    </td>
    <td style="padding:0 0.28em 0 0">e}f</td>
  </tr></table>
</td>
</tr></table>
<!-- END OF "PARSING" SIDE-DIAGRAM -->

<ul style="margin-bottom:0.9em">
<li>
An unescaped ''backslash'' which precedes another character, [[wp:Escape_character|escapes]] that character (to force it to be treated as literal). The backslashes are passed along to the output unchanged.
</li>
<li>
Balanced ''brace pairs'' are identified by, conceptually, going through the string from left to right and associating each unescaped closing brace that is encountered with the <u>nearest</u> still unassociated unescaped opening brace to its left (if any). Furthermore, each unescaped ''comma'' is associated with the <u>innermost</u> brace pair that contains it (if any). With that in mind:
<ul>
<li>
Each brace pair that has <u>at least one comma</u> associated with it, forms an alternation (whose branches are the brace pair's contents split at its commas). The associated brace and comma characters themselves do not become part of the output.
</li>
<li>
Brace characters from pairs without any associated comma, as well as unassociated brace and comma characters, as well as all characters that are not covered by the preceding rules, are instead treated as literals.
</li>
</ul>
</li>
</ul>

For every possible input string, your implementation should produce exactly the output which this specification mandates. Please comply with this even when it's inconvenient, to ensure that all implementations are comparable. However, none of the above should be interpreted as instructions (or even recommendations) for '''how''' to implement it. Try to come up with a solution that is idiomatic in your programming language. ''(See [[#Perl]] for a reference implementation.)''

{{task heading|Test Cases}}

{| class="wikitable" style="white-space: nowrap;"
|-
! Input
<small style="font-weight:normal">''(single string)''</small>
! Ouput
<small style="font-weight:normal">''(list/array of strings)''</small>
|- style="vertical-align:top"
|
<code>~/{Downloads,Pictures}/*.{jpg,gif,png}</code>
|
<code>~/Downloads/*.jpg</code>

<code>~/Downloads/*.gif</code>

<code>~/Downloads/*.png</code>

<code>~/Pictures/*.jpg</code>

<code>~/Pictures/*.gif</code>

<code>~/Pictures/*.png</code>

|- style="vertical-align:top"
|
<code>It{{em,alic}iz,erat}e{d,}, please.</code>
|
<code>Itemized, please.</code>

<code>Itemize, please.</code>

<code>Italicized, please.</code>

<code>Italicize, please.</code>

<code>Iterated, please.</code>

<code>Iterate, please.</code>

|- style="vertical-align:top"
|
<code>{,{,gotta have{ ,\, again\, }}more }cowbell!</code>
|
<code>cowbell!</code>

<code>more cowbell!</code>

<code>gotta have more cowbell!</code>

<code>gotta have\, again\, more cowbell!</code>

|- style="vertical-align:top"
|
<code>{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}</code>
|
<code>{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}</code>

<code>{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}</code>
|}
<hr style="clear:both; margin-bottom:1em;"/>


## 11l

{{trans|Python}}

```11l
F getitem(=s, depth = 0)
   V out = [‘’]
   L s != ‘’
      V c = String(s[0])
      I depth & (c == ‘,’ | c == ‘}’)
         R (out, s)
      I c == ‘{’
         V x = getgroup(s[1..], depth + 1)
         I !x[0].empty
            out = multiloop(out, x[0], (a, b) -> a‘’b)
            s = x[1]
            L.continue
      I c == "\\" & s.len > 1
         (s, c) = (s[1..], c‘’s[1])
      out = out.map(a -> a‘’@c)
      s = s[1..]
   R (out, s)

F getgroup(=s, depth)
   [String] out
   V comma = 0B
   L s != ‘’
      V gs = getitem(s, depth)
      s = gs[1]
      I s == ‘’
         L.break
      out [+]= gs[0]

      I s[0] == ‘}’
         I comma
            R (out, s[1..])
         R (out.map(a -> ‘{’a‘}’), s[1..])

      I s[0] == ‘,’
         (comma, s) = (1B, s[1..])
   R ([‘’] * 0, ‘’)

L(s) ‘~/{Downloads,Pictures}/*.{jpg,gif,png}
It{{em,alic}iz,erat}e{d,}, please.
{,{,gotta have{ ,\, again\, }}more }cowbell!
{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}’.split("\n")
   print(([s] [+] getitem(s)[0]).join("\n\t")"\n")
```

{{out}}

```txt

~/{Downloads,Pictures}/*.{jpg,gif,png}
        ~/Downloads/*.jpg
        ~/Downloads/*.gif
        ~/Downloads/*.png
        ~/Pictures/*.jpg
        ~/Pictures/*.gif
        ~/Pictures/*.png
...
{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
        {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
        {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```



## AutoHotkey

{{incorrect|AutoHotkey|Output for edge cases is wrong.}}


```autohotkey
; AutoHotkey runs Perl-compatible regular expressions no problem
t=~/{Downloads,Pictures}/ *.{jpg,gif,png} ; Note I added a space so the RosettaCode syntax highlighting will work. The AutoHotkey interpreter handles it no problem.
msgbox % expand(t)
t=It{{em,alic}iz,erat}e{d,}, please.
msgbox % expand(t)
t={,{,gotta have{ ,\, again\, }}more }cowbell!
msgbox % expand(t)
t={}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
msgbox % expand(t)

expand(t) {
t:=RegExReplace(t, "\\\\", "\!") ; if you want to use these character combinations literally in your string, just switch these susbstitutions to something unicode
,t:=RegExReplace(t, "\\,", "\#")
,t:=RegExReplace(t, "\\\{", "\@")
,t:=RegExReplace(t, "\\\}", "\&")
a=1
While a or b
	; expand (braces with multiple commas) which are (apparently inside something else)
	t:=RegExReplace(t, "Sxm)^(.*)  ([{,])  ([^{},]*)  \{  ([^{},]*)  ,  ([^{},]* , [^{}]*)  \}  ([^{},]*?)  ([},])  (.*)$", "$1$2$3$4$6,$3{$5}$6$7$8", a)
	; expand (braces with single commas) which are (apparently inside something else)
	,t:=RegExReplace(t, "Sxm)^(.*)  ([{,])  ([^{},]*)  \{  ([^{},]*)  ,  ([^{},]*)  \}  ([^{},]*?)  ([},])  (.*)$", "$1$2$3$4$6,$3$5$6$7$8", b)
a=1
While a or b
	; expand braces with single commas
	t:=RegExReplace(t, "Sxm)^(.*)  \{  ([^{},]*) , ([^{},]*)  \}  (.*)$", "$1$2$4`r`n$1$3$4", a)
	; expand braces with multiple commas
	,t:=RegExReplace(t, "Sxm)^(.*)  \{  ([^{},]*)  ,  ([^{},]* , [^{}]*)  \}  (.*)$", "$1$2$4`r`n$1{$3}$4", b)
t:=RegExReplace(t, "\\!", "\\")
,t:=RegExReplace(t, "\\#", "\,")
,t:=RegExReplace(t, "\\@", "\{")
,t:=RegExReplace(t, "\\&", "\}")
Return t
}
```

{{out}}

```txt
~/Downloads/*.jpg
~/Downloads/*.gif
~/Downloads/*.png
~/Pictures/*.jpg
~/Pictures/*.gif
~/Pictures/*.png

Itemized, please.
Itemize, please.
Italicized, please.
Italicize, please.
Iterated, please.
Iterate, please.

cowbell!
more cowbell!
gotta have more cowbell!
gotta have\, again\, more cowbell!

{}} some {\\edge }{ cases, here\\\}
{}} some {\\edgy }{ cases, here\\\}
```



## C++

C++11 solution:


```cpp
#include <iostream>
#include <iterator>
#include <string>
#include <utility>
#include <vector>

namespace detail {

template <typename ForwardIterator>
class tokenizer
{

	ForwardIterator _tbegin, _tend, _end;

public:

	tokenizer(ForwardIterator begin, ForwardIterator end)
		: _tbegin(begin), _tend(begin), _end(end)
	{ }

	template <typename Lambda>
	bool next(Lambda istoken)
	{
		if (_tbegin == _end) {
			return false;
		}
		_tbegin = _tend;
		for (; _tend != _end && !istoken(*_tend); ++_tend) {
			if (*_tend == '\\' && std::next(_tend) != _end) {
				++_tend;
			}
		}
		if (_tend == _tbegin) {
			_tend++;
		}
		return _tbegin != _end;
	}

	ForwardIterator begin() const { return _tbegin; }
	ForwardIterator end()   const { return _tend; }
	bool operator==(char c) { return *_tbegin == c; }

};

template <typename List>
void append_all(List & lista, const List & listb)
{
	if (listb.size() == 1) {
		for (auto & a : lista) {
			a += listb.back();
		}
	} else {
		List tmp;
		for (auto & a : lista) {
			for (auto & b : listb) {
				tmp.push_back(a + b);
			}
		}
		lista = std::move(tmp);
	}
}

template <typename String, typename List, typename Tokenizer>
List expand(Tokenizer & token)
{

	std::vector<List> alts{ { String() } };

	while (token.next([](char c) { return c == '{' || c == ',' || c == '}'; })) {

		if (token == '{') {
			append_all(alts.back(), expand<String, List>(token));
		} else if (token == ',') {
			alts.push_back({ String() });
		} else if (token == '}') {
			if (alts.size() == 1) {
				for (auto & a : alts.back()) {
					a = '{' + a + '}';
				}
				return alts.back();
			} else {
				for (std::size_t i = 1; i < alts.size(); i++) {
					alts.front().insert(alts.front().end(),
						std::make_move_iterator(std::begin(alts[i])),
						std::make_move_iterator(std::end(alts[i])));
				}
				return std::move(alts.front());
			}
		} else {
			for (auto & a : alts.back()) {
				a.append(token.begin(), token.end());
			}
		}

	}

	List result{ String{ '{' } };
	append_all(result, alts.front());
	for (std::size_t i = 1; i < alts.size(); i++) {
		for (auto & a : result) {
			a += ',';
		}
		append_all(result, alts[i]);
	}
	return result;
}

} // namespace detail

template <
	typename ForwardIterator,
	typename String = std::basic_string<
		typename std::iterator_traits<ForwardIterator>::value_type
	>,
	typename List = std::vector<String>
>
List expand(ForwardIterator begin, ForwardIterator end)
{
	detail::tokenizer<ForwardIterator> token(begin, end);
	List list{ String() };
	while (token.next([](char c) { return c == '{'; })) {
		if (token == '{') {
			detail::append_all(list, detail::expand<String, List>(token));
		} else {
			for (auto & a : list) {
				a.append(token.begin(), token.end());
			}
		}
	}
	return list;
}

template <
	typename Range,
	typename String = std::basic_string<typename Range::value_type>,
	typename List = std::vector<String>
>
List expand(const Range & range)
{
	using Iterator = typename Range::const_iterator;
	return expand<Iterator, String, List>(std::begin(range), std::end(range));
}

int main()
{

	for (std::string string : {
		R"(~/{Downloads,Pictures}/*.{jpg,gif,png})",
		R"(It{{em,alic}iz,erat}e{d,}, please.)",
		R"({,{,gotta have{ ,\, again\, }}more }cowbell!)",
		R"({}} some {\\{edge,edgy} }{ cases, here\\\})",
		R"(a{b{1,2}c)",
		R"(a{1,2}b}c)",
		R"(a{1,{2},3}b)",
		R"(a{b{1,2}c{}})",
		R"(more{ darn{ cowbell,},})",
		R"(ab{c,d\,e{f,g\h},i\,j{k,l\,m}n,o\,p}qr)",
		R"({a,{\,b}c)",
		R"(a{b,{{c}})",
		R"({a{\}b,c}d)",
		R"({a,b{{1,2}e}f)",
		R"({}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\})",
		R"({{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{)",
	}) {
		std::cout << string << '\n';
		for (auto expansion : expand(string)) {
			std::cout << "    " << expansion << '\n';
		}
		std::cout << '\n';
	}

	return 0;
}
```



## Common Lisp


```lisp
(defstruct alternation
  (alternatives nil :type list))

(defun alternatives-end-positions (string start)
  (assert (char= (char string start) #\{))
  (loop with level = 0
        with end-positions
        with escapep and commap
        for index from start below (length string)
        for c = (char string index)
        do (cond (escapep
                  (setf escapep nil))
                 ((char= c #\\)
                  (setf escapep t))
                 ((char= c #\{)
                  (incf level))
                 ((char= c #\})
                  (decf level)
                  (when (zerop level)
                    (push index end-positions)
                    (loop-finish)))
                 ((and (char= c #\,) (= level 1))
                  (setf commap t)
                  (push index end-positions)))
        finally (return (and (zerop level) commap (nreverse end-positions)))))

(defun parse-alternation (string start)
  (loop with end-positions = (alternatives-end-positions string start)
        for %start = (1+ start) then (1+ %end)
        for %end in end-positions
        collect (parse string :start %start :end %end) into alternatives
        finally (return (and alternatives
                             (values (make-alternation :alternatives alternatives) (1+ %end))))))

(defun parse (string &key (start 0) (end (length string)))
  (loop with result and escapep
        for index = start then next
        while (< index end)
        for c = (char string index)
        for next = (1+ index)
        do (cond (escapep
                  (push c result)
                  (setf escapep nil))
                 ((char= c #\\)
                  (push c result)
                  (setf escapep t))
                 ((char= c #\{)
                  (multiple-value-bind (alternation next-index)
                      (parse-alternation string index)
                    (cond (alternation
                           (push alternation result)
                           (setf next next-index))
                          (t
                           (push c result)))))
                 (t
                  (push c result)))
        finally (return (nreverse result))))

(defun traverse-alternation (alternation)
  (mapcan #'traverse (alternation-alternatives alternation)))

(defun traverse (parsed)
  (let ((results (list nil)))
    (dolist (element parsed results)
      (etypecase element
        (character
         (setf results (loop for r in results
                             collect (nconc r (list element)))))
        (alternation
         (setf results (loop for r in results
                             nconc (loop for ar in (traverse-alternation element)
                                         collect (append r ar)))))))))

(defun expand (string)
  (loop for result in (traverse (parse string))
        collect (coerce result 'string)))

(defun main ()
  (dolist (input '("~/{Downloads,Pictures}/*.{jpg,gif,png}"
                   "It{{em,alic}iz,erat}e{d,}, please."
                   "{,{,gotta have{ ,\\, again\\, }}more }cowbell!"
                   "{}} some }{,{\\\\{ edge, edge} \\,}{ cases, {here} \\\\\\\\\\}"))
    (write-line input)
    (dolist (output (expand input))
      (format t "    ~A~%" output))
    (terpri)))
```

{{out}}

```txt
~/{Downloads,Pictures}/*.{jpg,gif,png}
    ~/Downloads/*.jpg
    ~/Downloads/*.gif
    ~/Downloads/*.png
    ~/Pictures/*.jpg
    ~/Pictures/*.gif
    ~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.
    Itemized, please.
    Itemize, please.
    Italicized, please.
    Italicize, please.
    Iterated, please.
    Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!
    cowbell!
    more cowbell!
    gotta have more cowbell!
    gotta have\, again\, more cowbell!

{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
    {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
    {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```



## D

{{trans|Python}}
This code is not UTF-corrected, because it uses slicing instead of front, popFront, etc.

```d
import std.stdio, std.typecons, std.array, std.range, std.algorithm, std.string;

Nullable!(Tuple!(string[], string)) getGroup(string s, in uint depth)
pure nothrow @safe {
    string[] sout;
    auto comma = false;

    while (!s.empty) {
        // {const g, s} = getItems(s, depth);
        const r = getItems(s, depth);
        const g = r[0];
        s = r[1];

        if (s.empty)
            break;
        sout ~= g;

        if (s[0] == '}') {
            if (comma)
                return typeof(return)(tuple(sout, s[1 .. $]));
            return typeof(return)(tuple(
                sout.map!(a => '{' ~ a ~ '}').array, s[1 .. $]));
        }

        if (s[0] == ',') {
            comma = true;
            s = s[1 .. $];
        }
    }
    return typeof(return)();
}

Tuple!(string[], string) getItems(string s, in uint depth=0) pure @safe nothrow {
    auto sout = [""];

    while (!s.empty) {
        auto c = s[0 .. 1];
        if (depth && (c == "," || c == "}"))
            return tuple(sout, s);

        if (c == "{") {
            /*const*/ auto x = getGroup(s.dropOne, depth + 1);
            if (!x.isNull) {
                sout = cartesianProduct(sout, x[0])
                       .map!(ab => ab[0] ~ ab[1])
                       .array;
                s = x[1];
                continue;
            }
        }

        if (c == "\\" && s.length > 1) {
            c ~= s[1];
            s = s[1 .. $];
        }

        sout = sout.map!(a => a ~ c).array;
        s = s[1 .. $];
    }
    return tuple(sout, s);
}

void main() @safe {
    immutable testCases = r"~/{Downloads,Pictures}/*.{jpg,gif,png}
It{{em,alic}iz,erat}e{d,}, please.
{,{,gotta have{ ,\, again\, }}more }cowbell!
{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
a{b{1,2}c
a{1,2}b}c
a{1,{2},3}b
a{b{1,2}c{}}
more{ darn{ cowbell,},}
ab{c,d\,e{f,g\h},i\,j{k,l\,m}n,o\,p}qr
{a,{\,b}c
a{b,{{c}}
{a{\}b,c}d
{a,b{{1,2}e}f";

    foreach (const s; testCases.splitLines)
        writefln("%s\n%-(    %s\n%)\n", s, s.getItems[0]);
}
```

{{out}}

```txt
~/{Downloads,Pictures}/*.{jpg,gif,png}
    ~/Downloads/*.jpg
    ~/Downloads/*.gif
    ~/Downloads/*.png
    ~/Pictures/*.jpg
    ~/Pictures/*.gif
    ~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.
    Itemized, please.
    Itemize, please.
    Italicized, please.
    Italicize, please.
    Iterated, please.
    Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!
    cowbell!
    more cowbell!
    gotta have more cowbell!
    gotta have\, again\, more cowbell!

{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
    {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
    {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

a{b{1,2}c
    a{b1c
    a{b2c

a{1,2}b}c
    a1b}c
    a2b}c

a{1,{2},3}b
    a1b
    a{2}b
    a3b

a{b{1,2}c{}}
    a{b1c{}}
    a{b2c{}}

more{ darn{ cowbell,},}
    more darn cowbell
    more darn
    more

ab{c,d\,e{f,g\h},i\,j{k,l\,m}n,o\,p}qr
    abcqr
    abd\,efqr
    abd\,eg\hqr
    abi\,jknqr
    abi\,jl\,mnqr
    abo\,pqr

{a,{\,b}c
    {a,{\,b}c

a{b,{{c}}
    a{b,{{c}}

{a{\}b,c}d
    {a\}bd
    {acd

{a,b{{1,2}e}f
    {a,b{1e}f
    {a,b{2e}f

```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Brace_expansion do
  def getitem(s), do: getitem(String.codepoints(s), 0, [""])

  defp getitem([], _, out), do: {out,[]}
  defp getitem([c|_]=s, depth, out) when depth>0 and (c == "," or c == "}"), do: {out,s}
  defp getitem([c|t], depth, out) do
    x = getgroup(t, depth+1, [], false)
    if (c == "{") and x do
      {y, z} = x
      out2 = for a <- out, b <- y, do: a<>b
      getitem(z, depth, out2)
    else
      if c == "\\" and length(t) > 0 do
        c2 = c <> hd(t)
        getitem(tl(t), depth, Enum.map(out, fn a -> a <> c2 end))
      else
        getitem(t, depth, Enum.map(out, fn a -> a <> c end))
      end
    end
  end

  defp getgroup([], _, _, _), do: nil
  defp getgroup(s, depth, out, comma) do
    {g, s2} = getitem(s, depth, [""])
    if s2 == [] do
      nil
    else
      out2 = out ++ g
      case hd(s2) do
        "}" -> if comma, do: {out2, tl(s2)},
                       else: {Enum.map(out2, &"{#{&1}}"), tl(s2)}
        "," -> getgroup(tl(s2), depth, out2, true)
        _   -> getgroup(s2, depth, out2, comma)
      end
    end
  end
end

test_cases = ~S"""
~/{Downloads,Pictures}/*.{jpg,gif,png}
It{{em,alic}iz,erat}e{d,}, please.
{,{,gotta have{ ,\, again\, }}more }cowbell!
{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
""" |> String.split("\n", trim: true)

Enum.each(test_cases, fn s ->
  IO.puts s
  Brace_expansion.getitem(s)
  |> elem(0)
  |> Enum.each(fn str -> IO.puts "\t#{str}" end)
  IO.puts ""
end)
```


{{out}}

```txt

~/{Downloads,Pictures}/*.{jpg,gif,png}
        ~/Downloads/*.jpg
        ~/Downloads/*.gif
        ~/Downloads/*.png
        ~/Pictures/*.jpg
        ~/Pictures/*.gif
        ~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.
        Itemized, please.
        Itemize, please.
        Italicized, please.
        Italicize, please.
        Iterated, please.
        Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!
        cowbell!
        more cowbell!
        gotta have more cowbell!
        gotta have\, again\, more cowbell!

{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
        {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
        {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```



## Haskell

[http://www.reddit.com/r/readablecode/comments/1w6exe/p6_crosswalk_braceexpansionparses/cf229at "Here is a direct translation to Haskell using parsec"] (of [http://rosettacode.org/mw/index.php?title=Brace_expansion&oldid=175567#Perl_6 an earlier version of the Perl 6 solution]):


```haskell
{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
       (Parsec, (<|>), anyChar, between, char, many, many1, noneOf, parse,
        try)

parser :: Parsec String u [String]
parser =
  expand <$> many (try alts <|> try alt1 <|> escape <|> pure . pure <$> anyChar)
  where
    alts = concat <$> between (char '{') (char '}') (alt `sepBy2` char ',')
    alt1 =
      (: []) . ('{' :) . (++ "}") <$>
      between (char '{') (char '}') (many $ noneOf ",{}")
    alt =
      expand <$>
      many (try alts <|> try alt1 <|> escape <|> pure . pure <$> noneOf ",}")
    escape = pure <$> sequence [char '\\', anyChar]
    expand = foldr ((<*>) . fmap (++)) [""]
    p `sepBy2` sep = (:) <$> p <*> many1 (sep >> p)

showExpansion :: String -> String
showExpansion = (++) <*> ("\n-->\n" ++) . either show unlines . parse parser []

main :: IO ()
main =
  mapM_
    (putStrLn . showExpansion)
    [ "~/{Downloads,Pictures}/*.{jpg,gif,png}"
    , "It{{em,alic}iz,erat}e{d,}, please."
    , "{,{,gotta have{ ,\\, again\\, }}more }cowbell!"
    , "{}} some {\\\\{edge,edgy} }{ cases, here\\\\\\\\\\}"
    ]
```

{{out}}

```txt
~/{Downloads,Pictures}/*.{jpg,gif,png}
-->
~/Downloads/*.jpg
~/Downloads/*.gif
~/Downloads/*.png
~/Pictures/*.jpg
~/Pictures/*.gif
~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.
-->
Itemized, please.
Itemize, please.
Italicized, please.
Italicize, please.
Iterated, please.
Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!
-->
cowbell!
more cowbell!
gotta have more cowbell!
gotta have\, again\, more cowbell!

{}} some {\\{edge,edgy} }{ cases, here\\\\\}
-->
{}} some {\\edge }{ cases, here\\\\\}
{}} some {\\edgy }{ cases, here\\\\\}
```



## Go

<code>expand.go</code>:

```go
package expand

// Expander is anything that can be expanded into a slice of strings.
type Expander interface {
	Expand() []string
}

// Text is a trivial Expander that expands to a slice with just itself.
type Text string

func (t Text) Expand() []string { return []string{string(t)} }

// Alternation is an Expander that expands to the union of its
// members' expansions.
type Alternation []Expander

func (alt Alternation) Expand() []string {
	var out []string
	for _, e := range alt {
		out = append(out, e.Expand()...)
	}
	return out
}

// Sequence is an Expander that expands to the combined sequence of its
// members' expansions.
type Sequence []Expander

func (seq Sequence) Expand() []string {
	if len(seq) == 0 {
		return nil
	}
	out := seq[0].Expand()
	for _, e := range seq[1:] {
		out = combine(out, e.Expand())
	}
	return out
}

func combine(al, bl []string) []string {
	out := make([]string, 0, len(al)*len(bl))
	for _, a := range al {
		for _, b := range bl {
			out = append(out, a+b)
		}
	}
	return out
}

// Currently only single byte runes are supported for these.
const (
	escape   = '\\'
	altStart = '{'
	altEnd   = '}'
	altSep   = ','
)

type piT struct{ pos, cnt, depth int }

type Brace string

// Expand takes an input string and returns the expanded list of
// strings. The input string can contain any number of nested
// alternation specifications of the form "{A,B}" which is expanded to
// the strings "A", then "B".
//
// E.g. Expand("a{2,1}b{X,Y,X}c") returns ["a2bXc", "a2bYc", "a2bXc",
// "a1bXc", "a1bYc", "a1bXc"].
//
// Unmatched '{', ',', and '}' characters are passed through to the
// output. The special meaning of these characters can be escaped with
// '\', (which itself can be escaped).
// Escape characters are not removed, but passed through to the output.
func Expand(s string) []string   { return Brace(s).Expand() }
func (b Brace) Expand() []string { return b.Expander().Expand() }
func (b Brace) Expander() Expander {
	s := string(b)
	//log.Printf("Exapand(%#q)\n", s)
	var posInfo []piT
	var stack []int // indexes into posInfo
	removePosInfo := func(i int) {
		end := len(posInfo) - 1
		copy(posInfo[i:end], posInfo[i+1:])
		posInfo = posInfo[:end]
	}

	inEscape := false
	for i, r := range s {
		if inEscape {
			inEscape = false
			continue
		}
		switch r {
		case escape:
			inEscape = true
		case altStart:
			stack = append(stack, len(posInfo))
			posInfo = append(posInfo, piT{i, 0, len(stack)})
		case altEnd:
			if len(stack) == 0 {
				continue
			}
			si := len(stack) - 1
			pi := stack[si]
			if posInfo[pi].cnt == 0 {
				removePosInfo(pi)
				for pi < len(posInfo) {
					if posInfo[pi].depth == len(stack) {
						removePosInfo(pi)
					} else {
						pi++
					}
				}
			} else {
				posInfo = append(posInfo, piT{i, -2, len(stack)})
			}
			stack = stack[:si]
		case altSep:
			if len(stack) == 0 {
				continue
			}
			posInfo = append(posInfo, piT{i, -1, len(stack)})
			posInfo[stack[len(stack)-1]].cnt++
		}
	}
	//log.Println("stack:", stack)
	for len(stack) > 0 {
		si := len(stack) - 1
		pi := stack[si]
		depth := posInfo[pi].depth
		removePosInfo(pi)
		for pi < len(posInfo) {
			if posInfo[pi].depth == depth {
				removePosInfo(pi)
			} else {
				pi++
			}
		}
		stack = stack[:si]
	}
	return buildExp(s, 0, posInfo)
}

func buildExp(s string, off int, info []piT) Expander {
	if len(info) == 0 {
		return Text(s)
	}
	//log.Printf("buildExp(%#q, %d, %v)\n", s, off, info)
	var seq Sequence
	i := 0
	var dj, j, depth int
	for dk, piK := range info {
		k := piK.pos - off
		switch s[k] {
		case altStart:
			if depth == 0 {
				dj = dk
				j = k
				depth = piK.depth
			}
		case altEnd:
			if piK.depth != depth {
				continue
			}
			if j > i {
				seq = append(seq, Text(s[i:j]))
			}
			alt := buildAlt(s[j+1:k], depth, j+1+off, info[dj+1:dk])
			seq = append(seq, alt)
			i = k + 1
			depth = 0
		}
	}
	if j := len(s); j > i {
		seq = append(seq, Text(s[i:j]))
	}
	if len(seq) == 1 {
		return seq[0]
	}
	return seq
}

func buildAlt(s string, depth, off int, info []piT) Alternation {
	//log.Printf("buildAlt(%#q, %d, %d, %v)\n", s, depth, off, info)
	var alt Alternation
	i := 0
	var di int
	for dk, piK := range info {
		if piK.depth != depth {
			continue
		}
		if k := piK.pos - off; s[k] == altSep {
			sub := buildExp(s[i:k], i+off, info[di:dk])
			alt = append(alt, sub)
			i = k + 1
			di = dk + 1
		}
	}
	sub := buildExp(s[i:], i+off, info[di:])
	alt = append(alt, sub)
	return alt
}
```

<code>expand_test.go</code>

```go
package expand

import (
	"fmt"
	"reflect"
	"testing"
)

// These could be in expand.go but for now they're only used for debugging.
func (t Text) String() string          { return fmt.Sprintf("%#q", string(t)) }
func (alt Alternation) String() string { return fmt.Sprintf("ALT:%v", []Expander(alt)) }

// From http://rosettacode.org/wiki/Brace_expansion
var testCases = []struct {
	input    string
	expected []string
}{
	{"", []string{""}},
	{"a{2,1}b{X,Y,X}c", []string{
		"a2bXc",
		"a2bYc",
		"a2bXc",
		"a1bXc",
		"a1bYc",
		"a1bXc"}},
	{`a\\{\\\{b,c\,d}`, []string{
		`a\\\\\{b`,
		`a\\c\,d`}},
	{"{a,b{c{,{d}}e}f", []string{
		"{a,b{ce}f",
		"{a,b{c{d}e}f"}},
	{"~/{Downloads,Pictures}/*.{jpg,gif,png}", []string{
		"~/Downloads/*.jpg",
		"~/Downloads/*.gif",
		"~/Downloads/*.png",
		"~/Pictures/*.jpg",
		"~/Pictures/*.gif",
		"~/Pictures/*.png"}},
	{"It{{em,alic}iz,erat}e{d,}, please.", []string{
		"Itemized, please.",
		"Itemize, please.",
		"Italicized, please.",
		"Italicize, please.",
		"Iterated, please.",
		"Iterate, please."}},
	{`{,{,gotta have{ ,\, again\, }}more }cowbell!`, []string{
		"cowbell!",
		"more cowbell!",
		"gotta have more cowbell!",
		`gotta have\, again\, more cowbell!`}},
	{`{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}`, []string{
		`{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}`,
		`{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}`}},
}

//func ml(l []string) string { return "\n\t" + strings.Join(l, "\n\t") }
func ml(l []string) string {
	var result string
	for _, s := range l {
		result += fmt.Sprintf("\n\t%#q", s)
	}
	return result
}

func TestExpand(t *testing.T) {
	for _, d := range testCases {
		if g := Expand(d.input); !reflect.DeepEqual(g, d.expected) {
			t.Errorf("unexpected result\n Expand(%#q) gave:%v\nExpected:%v",
				d.input, ml(g), ml(d.expected))
		} else {
			// Normally Go tests aren't this verbose, but for rosettacode
			t.Logf("as expected\n Expand(%#q):%v", d.input, ml(g))
		}
	}
}

func BenchmarkExpand(b *testing.B) {
	input := testCases[5].input
	//b.Logf("Benchmarking Expand(%#q)\n", input)
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		Expand(input)
	}
}
```

{{out}}

```txt

% go test -v -bench=.
=== RUN TestExpand
--- PASS: TestExpand (0.00 seconds)
	expand_test.go:72: as expected
		 Expand(``):
			``
	expand_test.go:72: as expected
		 Expand(`a{2,1}b{X,Y,X}c`):
			`a2bXc`
			`a2bYc`
			`a2bXc`
			`a1bXc`
			`a1bYc`
			`a1bXc`
	expand_test.go:72: as expected
		 Expand(`a\\{\\\{b,c\,d}`):
			`a\\\\\{b`
			`a\\c\,d`
	expand_test.go:72: as expected
		 Expand(`{a,b{c{,{d}}e}f`):
			`{a,b{ce}f`
			`{a,b{c{d}e}f`
	expand_test.go:72: as expected
		 Expand(`~/{Downloads,Pictures}/*.{jpg,gif,png}`):
			`~/Downloads/*.jpg`
			`~/Downloads/*.gif`
			`~/Downloads/*.png`
			`~/Pictures/*.jpg`
			`~/Pictures/*.gif`
			`~/Pictures/*.png`
	expand_test.go:72: as expected
		 Expand(`It{{em,alic}iz,erat}e{d,}, please.`):
			`Itemized, please.`
			`Itemize, please.`
			`Italicized, please.`
			`Italicize, please.`
			`Iterated, please.`
			`Iterate, please.`
	expand_test.go:72: as expected
		 Expand(`{,{,gotta have{ ,\, again\, }}more }cowbell!`):
			`cowbell!`
			`more cowbell!`
			`gotta have more cowbell!`
			`gotta have\, again\, more cowbell!`
	expand_test.go:72: as expected
		 Expand(`{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}`):
			`{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}`
			`{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}`
PASS
BenchmarkExpand	  500000	      6562 ns/op	    2467 B/op	      64 allocs/op
ok  	rosetta_code/Brace_expansion	3.347s

```



## J


Implementation:


```J

NB. legit { , and } do not follow a legit backslash:
legit=: 1,_1}.4>(3;(_2[\"1".;._2]0 :0);('\';a.);0 _1 0 1)&;:&.(' '&,)
 2 1   1 1 NB. result 0 or 1: initial state
 2 2   1 2 NB. result 2 or 3: after receiving a non backslash
 1 2   1 2 NB. result 4 or 5: after receiving a backslash
)

expand=:3 :0
  Ch=. u:inv y
  M=. N=. 1+>./ Ch
  Ch=. Ch*-_1^legit y
  delim=. 'left comma right'=. u:inv '{,}'
  J=. i.K=. #Ch
  while. M=. M+1 do.
    candidates=. i.0 2
    for_check.I. comma=Ch do.
      begin=. >./I. left=check{. Ch
      end=. check+<./I. right=check}. Ch
      if. K>:end-begin do.
        candidates=. candidates,begin,end
      end.
    end.
    if. 0=#candidates do. break. end.
    'begin end'=. candidates{~(i.>./) -/"1 candidates
    ndx=. I.(begin<:J)*(end>:J)*Ch e. delim
    Ch=. M ndx} Ch
  end.
  T=. ,<Ch
  for_mark. |.N}.i.M  do.
    T=. ; mark divide each T
  end.
  u: each |each T
)

divide=:4 :0
  if. -.x e. y do. ,<y return. end.
  mask=. x=y
  prefix=. < y #~ -.+./\ mask
  suffix=. < y #~ -.+./\. mask
  options=. }:mask <;._1 y
  prefix,each options,each suffix
)
```


Examples:


```J>
expand t1
~/Downloads/*.jpg
~/Downloads/*.gif
~/Downloads/*.png
~/Pictures/*.jpg
~/Pictures/*.gif
~/Pictures/*.png
   > expand t2
Itemized, please.
Itemize, please.
Italicized, please.
Italicize, please.
Iterated, please.
Iterate, please.
   >expand t3
cowbell!
more cowbell!
gotta have more cowbell!
gotta have\, again\, more cowbell!
   >expand t4
{}} some {\\edge }{ cases, here\\\}
{}} some {\\edgy }{ cases, here\\\}
```


Explanation:

Instead of working directly with text, work with a string of numeric unicode values. Negate the numbers for characters which are "off limits" because of preceding backslashes (we will take the absolute value and convert back to unicode for the final result). Also, find a limit value larger than that of the largest in-use character.

Then, iteratively: for each relevant comma, find the location of the closest surrounding braces. From these candidates, pick a pair of braces that's the shortest distance apart. Mark those braces and their contained relevant commas by replacing their character codes with an integer larger than any previously used (all of them in the set get marked with the same number). Repeat until we cannot find any more possibilities.

Finally, for each integer that we've used to mark delimiter locations, split out each of the marked options (each with a copy of that group's prefix and suffix). (Then when all that is done, take the absolute values convert back to unicode for the final result.)


## Java

Should be able to handle all printable Unicode.

```java
public class BraceExpansion {

    public static void main(String[] args) {
        for (String s : new String[]{"It{{em,alic}iz,erat}e{d,}, please.",
            "~/{Downloads,Pictures}/*.{jpg,gif,png}",
            "{,{,gotta have{ ,\\, again\\, }}more }cowbell!",
            "{}} some }{,{\\\\{ edge, edge} \\,}{ cases, {here} \\\\\\\\\\}"}) {
            System.out.println();
            expand(s);
        }
    }

    public static void expand(String s) {
        expandR("", s, "");
    }

    private static void expandR(String pre, String s, String suf) {
        int i1 = -1, i2 = 0;
        String noEscape = s.replaceAll("([\\\\]{2}|[\\\\][,}{])", "  ");
        StringBuilder sb = null;

        outer:
        while ((i1 = noEscape.indexOf('{', i1 + 1)) != -1) {
            i2 = i1 + 1;
            sb = new StringBuilder(s);
            for (int depth = 1; i2 < s.length() && depth > 0; i2++) {
                char c = noEscape.charAt(i2);
                depth = (c == '{') ? ++depth : depth;
                depth = (c == '}') ? --depth : depth;
                if (c == ',' && depth == 1) {
                    sb.setCharAt(i2, '\u0000');
                } else if (c == '}' && depth == 0 && sb.indexOf("\u0000") != -1)
                    break outer;
            }
        }
        if (i1 == -1) {
            if (suf.length() > 0)
                expandR(pre + s, suf, "");
            else
                System.out.printf("%s%s%s%n", pre, s, suf);
        } else {
            for (String m : sb.substring(i1 + 1, i2).split("\u0000", -1))
                expandR(pre + s.substring(0, i1), m, s.substring(i2 + 1) + suf);
        }
    }
}
```



```txt
Itemized, please.
Itemize, please.
Italicized, please.
Italicize, please.
Iterated, please.
Iterate, please.

~/Downloads/*.jpg
~/Downloads/*.gif
~/Downloads/*.png
~/Pictures/*.jpg
~/Pictures/*.gif
~/Pictures/*.png

cowbell!
more cowbell!
gotta have more cowbell!
gotta have\, again\, more cowbell!

{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
```




## JavaScript



### ES5 Functional


Without importing Node.js libraries, JavaScript doesn't immediately have access to anything like Haskell's Parsec, but using a functional idiom of JavaScript, and emphasising clarity more than optimisation, we can separate out the tokenizing from the parsing, and the parsing from the generation of strings, to build a function which:
:#returns the set of expansions for each brace expression, and
:#logs a pretty-printed abstract syntax tree for each expression to the console (in a JSON format).

Each node of the parse tree consists of one of two simple functions (AND: syntagmatic concatenation, OR: flattening of paradigms) with a set of arguments, each of which may be a plain string or an AND or OR subtree. The expansions are derived by evaluating the parse tree as an expression.


```JavaScript
(function () {
  'use strict'

  // Index of any closing brace matching the opening brace at iPosn
  // with the indices of any immediately-enclosed commas
  function bracePair(tkns, iPosn, iNest, lstCommas) {
    if (iPosn >= tkns.length || iPosn < 0) return null;

    var t = tkns[iPosn],
      n = (t === '{') ? iNest + 1 : (t === '}' ? iNest - 1 : iNest),
      lst = (t === ',' && iNest === 1) ? lstCommas.concat(iPosn) : lstCommas;

    return n ? bracePair(tkns, iPosn + 1, n, lst) : {
      close: iPosn,
      commas: lst
    };
  }

  // Parse of a SYNTAGM subtree
  function andTree(dctSofar, tkns) {
    if (!tkns.length) return [dctSofar, []];

    var dctParse = dctSofar ? dctSofar : {
        fn: and,
        args: []
      },

      head = tkns[0],
      tail = head ? tkns.slice(1) : [],

      dctBrace = head === '{' ? bracePair(
        tkns, 0, 0, []
      ) : null,

      lstOR = dctBrace && dctBrace.close && dctBrace.commas.length ? (
        splitAt(dctBrace.close + 1, tkns)
      ) : null;

    return andTree({
      fn: and,
      args: dctParse.args.concat(
        lstOR ? orTree(dctParse, lstOR[0], dctBrace.commas) : head
      )
    }, lstOR ? lstOR[1] : tail);
  }

  // Parse of a PARADIGM subtree
  function orTree(dctSofar, tkns, lstCommas) {
    if (!tkns.length) return [dctSofar, []];
    var iLast = lstCommas.length;

    return {
      fn: or,
      args: splitsAt(
        lstCommas, tkns
      ).map(function (x, i) {
        var ts = x.slice(1, i === iLast ? -1 : void 0);

        return ts.length ? ts : [''];
      }).map(function (ts) {
        return ts.length > 1 ? andTree(null, ts)[0] : ts[0];
      })
    };
  }

  // List of unescaped braces and commas, and remaining strings
  function tokens(str) {
    // Filter function excludes empty splitting artefacts
    var toS = function (x) {
      return x.toString();
    };

    return str.split(/(\\\\)/).filter(toS).reduce(function (a, s) {
      return a.concat(s.charAt(0) === '\\' ? s : s.split(
        /(\\*[{,}])/
      ).filter(toS));
    }, []);
  }

  // PARSE TREE OPERATOR (1 of 2)
  // Each possible head * each possible tail
  function and(args) {
    var lng = args.length,
      head = lng ? args[0] : null,
      lstHead = "string" === typeof head ? [head] : head;

    return lng ? (
      1 < lng ? lstHead.reduce(function (a, h) {
        return a.concat(and(args.slice(1)).map(function (t) {
          return h + t;
        }));
      }, []) : lstHead
    ) : [];
  }

  // PARSE TREE OPERATOR (2 of 2)
  // Each option flattened
  function or(args) {
    return args.reduce(function (a, b) {
      return a.concat(b);
    }, []);
  }

  // One list split into two (first sublist length n)
  function splitAt(n, lst) {
    return n < lst.length + 1 ? [lst.slice(0, n), lst.slice(n)] : [lst, []];
  }

  // One list split into several (sublist lengths [n])
  function splitsAt(lstN, lst) {
    return lstN.reduceRight(function (a, x) {
      return splitAt(x, a[0]).concat(a.slice(1));
    }, [lst]);
  }

  // Value of the parse tree
  function evaluated(e) {
    return typeof e === 'string' ? e :
      e.fn(e.args.map(evaluated));
  }

  // JSON prettyprint (for parse tree, token list etc)
  function pp(e) {
    return JSON.stringify(e, function (k, v) {
      return typeof v === 'function' ? (
        '[function ' + v.name + ']'
      ) : v;
    }, 2)
  }


  // MAIN

  // s -> [s]
  function expansions(s) {
    // BRACE EXPRESSION PARSED
    var dctParse = andTree(null, tokens(s))[0];

    // ABSTRACT SYNTAX TREE LOGGED
    console.log(pp(dctParse));

    // AST EVALUATED TO LIST OF STRINGS
    return evaluated(dctParse);
  }


  // Sample expressions, double-escaped for quotation in source code.
  var lstTests = [
    '~/{Downloads,Pictures}/*.{jpg,gif,png}',
    'It{{em,alic}iz,erat}e{d,}, please.',
    '{,{,gotta have{ ,\\, again\\, }}more }cowbell!',
    '{}} some }{,{\\\\{ edge, edge} \\,}{ cases, {here} \\\\\\\\\\}'
  ];


  // 1. Return each expression with an indented list of its expansions, while
  // 2. logging each parse tree to the console.log() stream

  return lstTests.map(function (s) {
    return s + '\n\n' + expansions(s).map(function (x) {
      return '   ' + x;
    }).join('\n');
  }).join('\n\n');

})();
```


Value returned by function:


```txt
~/{Downloads,Pictures}/*.{jpg,gif,png}

   ~/Downloads/*.jpg
   ~/Downloads/*.gif
   ~/Downloads/*.png
   ~/Pictures/*.jpg
   ~/Pictures/*.gif
   ~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.

   Itemized, please.
   Itemize, please.
   Italicized, please.
   Italicize, please.
   Iterated, please.
   Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!

   cowbell!
   more cowbell!
   gotta have more cowbell!
   gotta have\, again\, more cowbell!

{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}

   {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
   {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
```


Sample of parse trees logged to the console:


```JavaScript
{
  "fn": "[function and]",
  "args": [
    "It",
    {
      "fn": "[function or]",
      "args": [
        {
          "fn": "[function and]",
          "args": [
            {
              "fn": "[function or]",
              "args": [
                "em",
                "alic"
              ]
            },
            "iz"
          ]
        },
        "erat"
      ]
    },
    "e",
    {
      "fn": "[function or]",
      "args": [
        "d",
        ""
      ]
    },
    ",",
    " please."
  ]
}
```




## Julia

{{trans|Python}}
```julia
 function getitem(s, depth=0)
    out = [""]
    while s != ""
        c = s[1]
        if depth > 0 && (c == ',' || c == '}')
            return out, s
        elseif c == '{'
            x = getgroup(s[2:end], depth+1)
            if x != ""
                out, s = [a * b for a in out, b in x[1]], x[2]
                continue
            end
        elseif c == '\\' && length(s) > 1
            s, c = s[2:end], c * s[2]
        end
        out, s = [a * c for a in out], s[2:end]
    end
    return out, s
end

function getgroup(s, depth)
    out, comma = "", false
    while s != ""
        g, s = getitem(s, depth)
        if s == ""
            break
        end
        out = vcat([out...], [g...])
        if s[1] == '}'
            if comma
                return out, s[2:end]
            end
            return ["{" * a * "}" for a in out], s[2:end]
        end
        if s[1] == ','
            comma, s = true, s[2:end]
        end
    end
    return ""
end

const teststrings = [raw"~/{Downloads,Pictures}/*.{jpg,gif,png}",
                    raw"It{{em,alic}iz,erat}e{d,}, please.",
                    raw"{,{,gotta have{ ,\, again\, }}more }cowbell!",
                    raw"{}} some }{,{\\\\{ edge, edge} \,}{ cases, {here} \\\\\\\\\}'''"]

for s in teststrings
    println("\n", s, "\n--------------------------------------------")
    for ans in getitem(s)[1]
        println(ans)
    end
end
```
 {{output}}
```txt

~/{Downloads,Pictures}/*.{jpg,gif,png}
--------------------------------------------
~/Downloads/*.jpg
~/Pictures/*.jpg
~/Downloads/*.gif
~/Pictures/*.gif
~/Downloads/*.png
~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.
--------------------------------------------
Itemized, please.
Italicized, please.
Iterated, please.
Itemize, please.
Italicize, please.
Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!
--------------------------------------------
cowbell!
more cowbell!
gotta have more cowbell!
gotta have\  again\  more cowbell!

{}} some }{,{\\\\{ edge, edge} \,}{ cases, {here} \\\\\\\\\}'''
--------------------------------------------
{}} some }{,{\\\{ edge \}}{ cases, {here} \\\\\\\\\''''
{}} some }{,{\\\{ edge \}}{ cases, {here} \\\\\\\\\''''

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

object BraceExpansion {
    fun expand(s: String) = expandR("", s, "")

    private val r = Regex("""([\\]{2}|[\\][,}{])""")

    private fun expandR(pre: String, s: String, suf: String) {
        val noEscape = s.replace(r, "  ")
        var sb = StringBuilder("")
        var i1 = noEscape.indexOf('{')
        var i2 = 0

        outer@ while (i1 != -1) {
            sb = StringBuilder(s)
            var depth = 1
            i2 = i1 + 1
            while (i2 < s.length && depth > 0) {
                val c = noEscape[i2]

                if (c == '{') depth++
                else if (c == '}') depth--

                if (c == ',' && depth == 1) sb[i2] = '\u0000'
                else if (c == '}' && depth == 0 && sb.indexOf("\u0000") != -1) break@outer
                i2++
            }
            i1 = noEscape.indexOf('{', i1 + 1)
        }
        if (i1 == -1) {
            if (suf.isNotEmpty()) expandR(pre + s, suf, "")
            else println("$pre$s$suf")
        } else {
            for (m in sb.substring(i1 + 1, i2).split('\u0000')) {
                expandR(pre + s.substring(0, i1), m, s.substring(i2 + 1) + suf)
            }
        }
    }
}

fun main(args: Array<String>) {
    val strings = arrayOf(
        """~/{Downloads,Pictures}/*.{jpg,gif,png}""",
        """It{{em,alic}iz,erat}e{d,}, please.""",
        """{,{,gotta have{ ,\, again\, }}more }cowbell!""",
        """{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}"""
    )
    for (s in strings) {
        println()
        BraceExpansion.expand(s)
    }
}
```


{{out}}

```txt

~/Downloads/*.jpg
~/Downloads/*.gif
~/Downloads/*.png
~/Pictures/*.jpg
~/Pictures/*.gif
~/Pictures/*.png

Itemized, please.
Itemize, please.
Italicized, please.
Italicize, please.
Iterated, please.
Iterate, please.

cowbell!
more cowbell!
gotta have more cowbell!
gotta have\, again\, more cowbell!

{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```



## Perl


Perl has a built-in [http://perldoc.perl.org/functions/glob.html <code>glob</code>] function which does brace expansion, but it can't be used to solve this task because it also does shell-like word splitting, wildcard expansion, and tilde expansion at the same time.
The <code>File::Glob</code> core module gives access to the more low-level [http://perldoc.perl.org/File/Glob.html#EXPORTS <code>bsd_glob</code>] function which actually supports exclusive brace expansion, however it does not comply with this task's specification when it comes to preserving backslashes and handling unbalanced brace characters.

So here is a manual solution that implements the specification precisely:


```perl
sub brace_expand {
    my $input = shift;
    my @stack = ([my $current = ['']]);

    while ($input =~ /\G ((?:[^\\{,}]++ | \\(?:.|\z))++ | . )/gx) {
        if ($1 eq '{') {
            push @stack, [$current = ['']];
        }
        elsif ($1 eq ',' && @stack > 1) {
            push @{$stack[-1]}, ($current = ['']);
        }
        elsif ($1 eq '}' && @stack > 1) {
            my $group = pop @stack;
            $current = $stack[-1][-1];

            # handle the case of brace pairs without commas:
            @{$group->[0]} = map { "{$_}" } @{$group->[0]} if @$group == 1;

            @$current = map {
                my $c = $_;
                map { map { $c . $_ } @$_ } @$group;
            } @$current;
        }
        else { $_ .= $1 for @$current; }
    }

    # handle the case of missing closing braces:
    while (@stack > 1) {
        my $right = pop @{$stack[-1]};
        my $sep;
        if (@{$stack[-1]}) { $sep = ',' }
        else               { $sep = '{'; pop @stack }
        $current = $stack[-1][-1];
        @$current = map {
            my $c = $_;
            map { $c . $sep . $_ } @$right;
        } @$current;
    }

    return @$current;
}
```


Usage demonstration:

```perl
while (my $input = <DATA>) {
    chomp($input);
    print "$input\n";
    print "    $_\n" for brace_expand($input);
    print "\n";
}

__DATA__
~/{Downloads,Pictures}/*.{jpg,gif,png}
It{{em,alic}iz,erat}e{d,}, please.
{,{,gotta have{ ,\, again\, }}more }cowbell!
{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
```

{{out}}

```txt

~/{Downloads,Pictures}/*.{jpg,gif,png}
    ~/Downloads/*.jpg
    ~/Downloads/*.gif
    ~/Downloads/*.png
    ~/Pictures/*.jpg
    ~/Pictures/*.gif
    ~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.
    Itemized, please.
    Itemize, please.
    Italicized, please.
    Italicize, please.
    Iterated, please.
    Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!
    cowbell!
    more cowbell!
    gotta have more cowbell!
    gotta have\, again\, more cowbell!

{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
    {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
    {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}


```



## Perl 6

The task description allows the taking of shortcuts, but please note that we are not taking any shortcuts here.  The solution is short because this particular problem happens to map quite naturally to the strengths of Perl 6.

First, the parsing is handled with a grammar that can backtrack in the few places this problem needs it.  The <tt>+</tt> quantifier matches one or more alternatives (we handle the case of a single alternative in the walk now), and the <tt>%</tt> modifier requires a comma between each quantified item to its left.  Note that the <tt>*</tt> quantifiers do <i>not</i> backtrack here, because the <tt>token</tt> keyword suppresses that; all the backtracking here fails over to a different alternative in an outer alternation (that is, things separated by the <tt>|</tt> character in the grammar. Most of these failovers just nibble an uninteresting character and continue.)

On the other end, we recursively walk the parse tree returning expanded sublists, and we do the cartesian concatenation of sublists at each level by use of the <tt>X~</tt> operator, which is a "cross" metaoperator used on a simple <tt>~</tt> concatenation.  As a list infix operator, <tt>X~</tt> does not care how many items are on either side, which is just what you want in this case, since some of the arguments are strings and some are lists.  Here we use a fold or reduction form in square brackets to interpose the cross-concat between each value generated by the map, which returns a mixture of lists and literal strings.  One other thing that might not be obvious: if we bind to the match variable, <tt>$/</tt>, we automatically get all the syntactic sugar for its submatches.  In this case, <tt>$0</tt> is short for <tt>$/[0]</tt>, and represents all the submatches captured by 0th set of parens in either <tt>TOP</tt> or <tt>alt</tt>.  <tt>$&lt;meta&gt;</tt> is likewise short for <tt>$/&lt;meta&gt;</tt>, and retrieves what was captured by that named submatch.

```perl6
grammar BraceExpansion {
    token TOP  { ( <meta> | . )* }
    token meta { '{' <alts> '}' | \\ .  }
    token alts { <alt>+ % ',' }
    token alt  { ( <meta> | <-[ , } ]> )* }
}

sub crosswalk($/) {
    [X~] '', $0.map: -> $/ { ([$<meta><alts><alt>.&alternatives]) or ~$/ }
}

sub alternatives($_) {
    when :not { () }
    when 1    { '{' X~ $_».&crosswalk X~ '}' }
    default   { $_».&crosswalk }
}

sub brace-expand($s) { crosswalk BraceExpansion.parse($s) }

# Testing:

sub bxtest(*@s) {
    for @s -> $s {
        say "\n$s";
        for brace-expand($s) {
            say "    ", $_;
        }
    }
}

bxtest Q:to/END/.lines;
    ~/{Downloads,Pictures}/*.{jpg,gif,png}
    It{{em,alic}iz,erat}e{d,}, please.
    {,{,gotta have{ ,\, again\, }}more }cowbell!
    {}} some {\\{edge,edgy} }{ cases, here\\\}
    a{b{1,2}c
    a{1,2}b}c
    a{1,{2},3}b
    a{b{1,2}c{}}
    more{ darn{ cowbell,},}
    ab{c,d\,e{f,g\h},i\,j{k,l\,m}n,o\,p}qr
    {a,{\,b}c
    a{b,{{c}}
    {a{\}b,c}d
    {a,b{{1,2}e}f
    END
```

{{out}}

```txt
~/{Downloads,Pictures}/*.{jpg,gif,png}
    ~/Downloads/*.jpg
    ~/Downloads/*.gif
    ~/Downloads/*.png
    ~/Pictures/*.jpg
    ~/Pictures/*.gif
    ~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.
    Itemized, please.
    Itemize, please.
    Italicized, please.
    Italicize, please.
    Iterated, please.
    Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!
    cowbell!
    more cowbell!
    gotta have more cowbell!
    gotta have\, again\, more cowbell!

{}} some {\\{edge,edgy} }{ cases, here\\\}
    {}} some {\\edge }{ cases, here\\\}
    {}} some {\\edgy }{ cases, here\\\}

a{b{1,2}c
    a{b1c
    a{b2c

a{1,2}b}c
    a1b}c
    a2b}c

a{1,{2},3}b
    a1b
    a{2}b
    a3b

a{b{1,2}c{}}
    a{b1c{}}
    a{b2c{}}

more{ darn{ cowbell,},}
    more darn cowbell
    more darn
    more

ab{c,d\,e{f,g\h},i\,j{k,l\,m}n,o\,p}qr
    abcqr
    abd\,efqr
    abd\,eg\hqr
    abi\,jknqr
    abi\,jl\,mnqr
    abo\,pqr

{a,{\,b}c
    {a,{\,b}c

a{b,{{c}}
    a{b,{{c}}

{a{\}b,c}d
    {a\}bd
    {acd

{a,b{{1,2}e}f
    {a,b{1e}f
    {a,b{2e}f
```



## Phix

Fairly straightforward recursive solution

```Phix
--
-- demo\rosetta\Brace_expansion.exw
--
### ==========================

--
function pair(sequence stems, sequence brest)
    sequence res = {}
    for i=1 to length(stems) do
        for j=1 to length(brest) do
            res = append(res,stems[i]&brest[j])
        end for
    end for
    return res
end function

function brarse(string s)
integer idx = 1
    while idx<=length(s) do
        integer ch = s[idx]
        if ch='{' then
            sequence alts = {idx}
            idx += 1
            integer l0 = idx
            bool nest = false
            integer level = 1
            while idx<=length(s) do
                switch s[idx] do
                    case '{': level += 1
                              nest = true
                    case '}': level -= 1
                              if level=0 then exit end if
                    case ',': if level=1 then
                                alts = append(alts,idx)
                              end if
                    case '\\': idx += 1
                end switch
                idx += 1
            end while
            if length(alts)>1 and level=0 then
                alts &= idx
                sequence stems = {}
                string stem = s[1..alts[1]-1]
                for i=2 to length(alts) do
                    string rest = s[alts[i-1]+1..alts[i]-1]
                    if nest then
                        sequence inners = brarse(rest)
                        for j=1 to length(inners) do
                            stems = append(stems,stem&inners[j])
                        end for
                    else
                        stems = append(stems,stem&rest)
                    end if
                end for
                return pair(stems,brarse(s[idx+1..$]))
            elsif nest then
                return pair({s[1..l0-1]},brarse(s[l0..$]))
            end if
        end if
        idx += 1
    end while
    return {s}
end function

-- (since ? and pp() add their own backslash escapes:)
procedure edump(sequence x)
    for i=1 to length(x) do
        printf(1,"%s\n",{x[i]})
    end for
end procedure

edump(brarse("~/{Downloads,Pictures}/*.{jpg,gif,png}"))
edump(brarse("It{{em,alic}iz,erat}e{d,}, please."))
edump(brarse(`{,{,gotta have{ ,\, again\, }}more }cowbell!`))
edump(brarse(`{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}`))
```

{{Out}}

```txt

~/Downloads/*.jpg
~/Downloads/*.gif
~/Downloads/*.png
~/Pictures/*.jpg
~/Pictures/*.gif
~/Pictures/*.png
Itemized, please.
Itemize, please.
Italicized, please.
Italicize, please.
Iterated, please.
Iterate, please.
cowbell!
more cowbell!
gotta have more cowbell!
gotta have\, again\, more cowbell!
{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```



## PicoLisp


```PicoLisp
(de braceExpand (Str)
   (let Lst
      (make
         (for (Lst (chop Str)  Lst)
            (case (pop 'Lst)
               ("\\" (link (pop 'Lst)))
               ("{"
                  (recur ()
                     (let L
                        (make
                           (while
                              (case (pop 'Lst)
                                 ("\\" (link (pop 'Lst)) Lst)
                                 ("{" (recurse) Lst)
                                 ("}" NIL)
                                 ("," (link 0) Lst)  # Replace commata with '0'
                                 (T (link @) Lst) ) ) )
                        (if (= "}" @)  # Was closing brace
                           (if (member 0 L)  # Has comma(ta)
                              (link (split L 0))
                              (chain (list "{") (replace L 0 ",") (list "}")))
                           (chain (list "{") (replace L 0 ",")) ) ) ) )
               (T (link @)) ) ) )
      (recur (Lst)
         (ifn (find pair Lst)
            (list (pack Lst))
            (let R (recurse (cdr Lst))
               (mapcan
                  '((A) (mapcar '((B) (pack A B)) R))
                  (if (pair (car Lst))
                     (mapcan recurse (car Lst))
                     (list (car Lst)) ) ) ) ) ) ) )
```

Test:

```PicoLisp
(test
   (quote
      "~/Downloads/*.jpg"
      "~/Downloads/*.gif"
      "~/Downloads/*.png"
      "~/Pictures/*.jpg"
      "~/Pictures/*.gif"
      "~/Pictures/*.png" )
   (braceExpand "~/{Downloads,Pictures}/*.{jpg,gif,png}") )

(test
   (quote
      "Itemized, please."
      "Itemize, please."
      "Italicized, please."
      "Italicize, please."
      "Iterated, please."
      "Iterate, please." )
   (braceExpand "It{{em,alic}iz,erat}e{d,}, please.") )

(test
   (quote
      "cowbell!"
      "more cowbell!"
      "gotta have more cowbell!"
      "gotta have\, again\, more cowbell!" )
   (braceExpand "{,{,gotta have{ ,\\, again\\, }}more }cowbell!") )

(test
   (quote
      "{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}"
      "{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}" )
   (braceExpand "{}} some }{,{\\\\{ edge, edge} \\,}{ cases, {here} \\\\\\\\\\}") )
```



## PowerShell

{{works with|PowerShell|2}}

```PowerShell

function Expand-Braces ( [string]$String )
    {
    $Escaped = $False
    $Stack = New-Object System.Collections.Stack
    $ClosedBraces = $BracesToParse = $Null

    ForEach ( $i in 0..($String.Length-1) )
        {
        Switch ( $String[$i] )
            {
            '\' {
                $Escaped = -not $Escaped
                break
                }

            '{' {
                If ( -not $Escaped ) { $Stack.Push( [pscustomobject]@{ Delimiters = @( $i ) } ) }
                }

            ',' {
                If ( -not $Escaped -and $Stack.Count ) { $Stack.Peek().Delimiters += $i }
                }

            '}' {
                If ( -not $Escaped -and $Stack.Count )
                    {
                    $Stack.Peek().Delimiters += $i
                    $ClosedBraces = $Stack.Pop()
                    If ( $ClosedBraces.Delimiters.Count -gt 2 )
                        {
                        $BracesToParse = $ClosedBraces
                        }
                    }
                }

            default { $Escaped = $False }
            }
        }

        If ( $BracesToParse )
            {
            $Start = $String.Substring( 0, $BracesToParse.Delimiters[0] )
            $End   = $String.Substring( $BracesToParse.Delimiters[-1] + 1 )

            ForEach ( $i in 0..($BracesToParse.Delimiters.Count-2) )
                {
                $Option = $String.Substring( $BracesToParse.Delimiters[$i] + 1, $BracesToParse.Delimiters[$i+1] - $BracesToParse.Delimiters[$i] - 1 )
                Expand-Braces ( $Start + $Option + $End )
                }
            }
        Else
            {
            $String
            }
    }

```


```PowerShell

$TestStrings = @(
    'It{{em,alic}iz,erat}e{d,}, please.'
    '~/{Downloads,Pictures}/*.{jpg,gif,png}'
    '{,{,gotta have{ ,\, again\, }}more }cowbell!'
    '{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}'
    )

ForEach ( $String in $TestStrings )
    {
    ''
    $String
    '------'
    Expand-Braces $String
    }

```

{{out}}

```txt

It{{em,alic}iz,erat}e{d,}, please.
------
Itemized, please.
Italicized, please.
Iterated, please.
Itemize, please.
Italicize, please.
Iterate, please.

~/{Downloads,Pictures}/*.{jpg,gif,png}
------
~/Downloads/*.jpg
~/Pictures/*.jpg
~/Downloads/*.gif
~/Pictures/*.gif
~/Downloads/*.png
~/Pictures/*.png

{,{,gotta have{ ,\, again\, }}more }cowbell!
------
cowbell!
more cowbell!
gotta have more cowbell!
gotta have\, again\, more cowbell!

{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
------
{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```



## Python


```python
def getitem(s, depth=0):
    out = [""]
    while s:
        c = s[0]
        if depth and (c == ',' or c == '}'):
            return out,s
        if c == '{':
            x = getgroup(s[1:], depth+1)
            if x:
                out,s = [a+b for a in out for b in x[0]], x[1]
                continue
        if c == '\\' and len(s) > 1:
            s, c = s[1:], c + s[1]

        out, s = [a+c for a in out], s[1:]

    return out,s

def getgroup(s, depth):
    out, comma = [], False
    while s:
        g,s = getitem(s, depth)
        if not s: break
        out += g

        if s[0] == '}':
            if comma: return out, s[1:]
            return ['{' + a + '}' for a in out], s[1:]

        if s[0] == ',':
            comma,s = True, s[1:]

    return None

# stolen cowbells from perl6 example
for s in '''~/{Downloads,Pictures}/*.{jpg,gif,png}
It{{em,alic}iz,erat}e{d,}, please.
{,{,gotta have{ ,\, again\, }}more }cowbell!
{}} some }{,{\\\\{ edge, edge} \,}{ cases, {here} \\\\\\\\\}'''.split('\n'):
    print "\n\t".join([s] + getitem(s)[0]) + "\n"
```


{{out}}

```txt

~/{Downloads,Pictures}/*.{jpg,gif,png}
        ~/Downloads/*.jpg
        ~/Downloads/*.gif
        ~/Downloads/*.png
        ~/Pictures/*.jpg
        ~/Pictures/*.gif
        ~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.
        Itemized, please.
        Itemize, please.
        Italicized, please.
        Italicize, please.
        Iterated, please.
        Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!
        cowbell!
        more cowbell!
        gotta have more cowbell!
        gotta have\, again\, more cowbell!

{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
        {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
        {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}


```



## Racket

{{trans|Python}}

```racket
#lang racket/base
(require racket/match)
(define (merge-lists as . bss)
  (match bss
    ['() as]
    [(cons b bt)
     (apply merge-lists
            (for*/list ((a (in-list as)) (b (in-list b))) (append a b))
            bt)]))

(define (get-item cs depth)
  (let loop ((out '(())) (cs cs))
    (match cs
      ['() (values out cs)]
      [`(,(or #\, #\}) . ,more) #:when (positive? depth) (values out cs)]
      [`(#\{ . ,more)
       (=> no-group-match)
       (define-values (group-out group-rem) (get-group more (add1 depth) no-group-match))
       (loop (merge-lists out group-out) group-rem)]
      [(or `(#\\ ,c ,more ...) `(,c . ,more)) (loop (merge-lists out (list (list c))) more)])))

(define (get-group cs depth no-group-match)
  (let loop ((out '()) (cs cs) (comma? #f))
    (when (null? cs) (no-group-match))
    (define-values (item-out item-rem) (get-item cs depth))
    (when (null? item-rem) (no-group-match))
    (let ((out (append out item-out)))
      (match item-rem
        [`(#\} . ,more) #:when comma? (values out more)]
        [`(#\} . ,more) (values (merge-lists '((#\{)) out '((#\}))) more)]
        [`(#\, . ,more) (loop out more #t)]
        [_ (no-group-match)]))))

(define (brace-expand s)
  (let-values (([out rem] (get-item (string->list s) 0))) (map list->string out)))

(module+ test
  (define patterns
    (list
     "~/{Downloads,Pictures}/*.{jpg,gif,png}"
     "It{{em,alic}iz,erat}e{d,}, please."
     #<<P
{,{,gotta have{ ,\, again\, }}more }cowbell!
P
     #<<P
{}} some }{,{\\\\{ edge, edge} \,}{ cases, {here} \\\\\\\\\}
P
     ))
  (for ((s (in-list patterns)) #:when (printf "expand: ~a~%" s) (x (in-list (brace-expand s))))
    (printf "\t~a~%" x)))
```

{{out}}

```txt
expand: ~/{Downloads,Pictures}/*.{jpg,gif,png}
	~/Downloads/*.jpg
	~/Downloads/*.gif
	~/Downloads/*.png
	~/Pictures/*.jpg
	~/Pictures/*.gif
	~/Pictures/*.png
expand: It{{em,alic}iz,erat}e{d,}, please.
	Itemized, please.
	Itemize, please.
	Italicized, please.
	Italicize, please.
	Iterated, please.
	Iterate, please.
expand: {,{,gotta have{ ,\, again\, }}more }cowbell!
	cowbell!
	more cowbell!
	gotta have more cowbell!
	gotta have, again, more cowbell!
expand: {}} some }{,{\\\\{ edge, edge} \,}{ cases, {here} \\\\\\\\\}
	{}} some }{,{\\ edge ,}{ cases, {here} \\\\}
	{}} some }{,{\\ edge ,}{ cases, {here} \\\\}
```



## REXX


```rexx
/*---------------------------------------------------------------------
* Brace expansion
* 26.07.2016
* s.* holds the set of strings
*--------------------------------------------------------------------*/
text.1='{,{,gotta have{ ,\, again\, }}more }cowbell!'
text.2='~/{Downloads,Pictures}/*.{jpg,gif,png}'
text.3='It{{em,alic}iz,erat}e{d,}, please. '
text.4='{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\} '
text.5='x{,a,b,c}{d,e}y'
text.6='aa{,{,11}cc}22'
text.7='{}'
Parse Arg dbg
oid='brace.xxx'; 'erase' oid
Do case=1 To 7
  Call brac text.case
  End
Return
brac:
  s.=0
  Parse Arg s
  Say ''
  Say '  's
  s.1.0=1                            /* first iteration              */
  s.1.1=s                            /* the initial string           */
  Do it=1 To 10 Until todo=0         /* Iterate until all done       */
    todo=0                           /* will be 1 if more to be done */
    Call dbg 'Iteration' it
    do di=1 To s.it.0                /* show current state           */
      Call dbg 's.'it'.'di  s.it.di
      End
    ita=it+1                         /* index for next set of strings*/
    xp=0
    do di=1 To s.it.0                /* loop over all strings        */
      Call dbg it'.'di s.it.di
      Call bra s.it.di               /* analyze current string       */
      If braces=1 Then Do            /* If brace groups were found   */
        Do bgi=1 To bgdata.0         /* loop over grace groups       */
          If end.bgi=0 Then Iterate  /* Incomplete bg (... )         */
          clist=''
          Do cj=1 To ci.bgi.0
            clist=clist ci.bgi.cj
            End
          Call dbg bgdata.bgi '->' clist
          If ccount.bgi>0 Then Do    /* comma(s) founf in bg         */
            Call expand bgi          /* expand this bg               */
            xp=1                     /* indicate that we worked      */
            Leave
            End
          End
        If xp=0 Then Do              /* nothing done                 */
          z=s.ita.0+1                /* copy string to next iteration*/
          s.ita.z=s.it.di
          End
        End
      Else Do                        /* no brace group               */
        z=s.ita.0+1                  /* copy string to next iteration*/
        s.ita.z=s
        s.ita.0=z
        End
      End
    Do dd=1 To s.ita.0               /* log current set of strings   */
      Call dbg ita dd s.ita.dd
      End
    End
  Do dd=1 To s.it.0                  /* show final set of strings    */
    Say dd s.it.dd
    End
  Return

bra:
/*---------------------------------------------------------------------
* Analyze the given string
* Input: s
* Output:
* bgdata.*  Array of data about brace groups:
*                      level start column comma positions end column
*--------------------------------------------------------------------*/
parse Arg s
Call dbg 'bra:' s
level=0
bgdata.=0
bgn=0
bgnmax=0
ccount.=0
ol=''
ci.=0
bgnl=''
braces=0
end.=0
escape=0
Do i=1 To length(s)
  c=substr(s,i,1)
  Select
    When escape Then
      escape=0
    When c='\' Then
      escape=1
    When c='{' Then Do
      level=level+1
      Call bm c
      co=level
      End
    When c='}' Then Do
      If level>0 Then Do
        co=level
        Call bm c
        level=level-1
        End
      End
    When c=',' Then Do
      co=level
      If co>0 Then Do
        ccount.bgn=ccount.bgn+1
        z=ccount.bgn
        ci.bgn.0=z
        ci.bgn.z=i
        End
      If ccount.bgn>0 Then
        braces=1
      End
    Otherwise
      co=level
    End
  ol=ol||co
  bgnl=bgnl||bgn
  End
Call dbg s
Call dbg ol
Call dbg left(copies('123456789.',10),length(s))
Call dbg bgnl
Do bgi=1 To bgdata.0
  If end.bgi=1 Then Do
    cl=''
    Do cii=1 To ci.bgi.0
      cl=cl ci.bgi.cii
      End
    Parse Var bgdata.bgi level a e
    Call dbg bgi level a cl e
    End
  End
Return

bm:
/*---------------------------------------------------------------------
* Brace Management
* for '{' create a new brace group )record level and start column
* for '}' look for corresponding bg and add end column
* Input: column and character ( '{' or '}' )
* Output: bgdata.*  level start-column [end-column]
*--------------------------------------------------------------------*/
  Parse Arg oc
  Call dbg oc i level
  If oc='{' Then Do
    z=bgdata.0+1
    bgdata.z=level i
    bgdata.0=z
    bgn=bgnmax+1
    bgnmax=bgn
    End
  Else Do
    Do bgi=bgdata.0 To 1 By -1
      If level=word(bgdata.bgi,1) Then Do
        bgdata.bgi=bgdata.bgi i
        end.bgi=1
        Leave
        End
      End
    bgn=bgn-1
    Call dbg bgdata.bgi 'bgn='bgn
    End
  Return

expand:
/*---------------------------------------------------------------------
* Expand a brace group in string s
*--------------------------------------------------------------------*/
  Parse Arg bgi
  Parse Var bgdata.bgi . start end
  clist=start clist end
  If words(clist)>0 Then Do          /* commas in brace group        */
    left=left(s,start-1)             /* part of s before the '{'     */
    rite=substr(s,end+1)             /* part of s after the '}'      */
    Do k=1 To words(clist)-1         /* Loop over comma positions    */
      a=word(clist,k)                /* start position               */
      e=word(clist,k+1)              /* end position                 */
      choice.k=substr(s,a+1,e-a-1)   /* one of the choices           */
      z=s.ita.0+1                    /* add new string to next set   */
      s.ita.z=left||choice.k||rite   /* construct new string         */
      s.ita.0=z
      todo=1                         /* maybe more to be done        */
      End
    End
  Else Do                            /* no commas                    */
    z=s.ita.0+1                      /* copy string as is to next set*/
    s.ita.z=s
    s.ita.0=z
    End
  Do zz=1 To s.ita.0
    Call dbg zz s.ita.zz
    End
  Return

dbg:                                 /* handle debug output          */
  If dbg<>'' Then                    /* argument given               */
    Say arg(1)                       /* show on screen               */
  Call lineout oid,arg(1)            /* write to file                */
  Return
```

{{out}}

```txt
J:\>rexx braces

  {,{,gotta have{ ,\, again\, }}more }cowbell!
1 cowbell!
2 more cowbell!
3 gotta have more cowbell!
4 gotta have\, again\, more cowbell!

  ~/{Downloads,Pictures}/*.{jpg,gif,png}
1 ~/Downloads/*.jpg
2 ~/Downloads/*.gif
3 ~/Downloads/*.png
4 ~/Pictures/*.jpg
5 ~/Pictures/*.gif
6 ~/Pictures/*.png

  It{{em,alic}iz,erat}e{d,}, please.
1 Itemized, please.
2 Itemize, please.
3 Italicized, please.
4 Italicize, please.
5 Iterated, please.
6 Iterate, please.

  {}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
1 {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
2 {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

  x{,a,b,c}{d,e}y
1 xdy
2 xey
3 xady
4 xaey
5 xbdy
6 xbey
7 xcdy
8 xcey

  aa{,{,11}cc}22
1 aa22
2 aacc22
3 aa11cc22

  {}
1 {}
```



## Ruby

{{trans|Python}}

```ruby
def getitem(s, depth=0)
  out = [""]
  until s.empty?
    c = s[0]
    break  if depth>0 and (c == ',' or c == '}')
    if c == '{' and x = getgroup(s[1..-1], depth+1)
      out = out.product(x[0]).map{|a,b| a+b}
      s = x[1]
    else
      s, c = s[1..-1], c + s[1]  if c == '\\' and s.size > 1
      out, s = out.map{|a| a+c}, s[1..-1]
    end
  end
  return out, s
end

def getgroup(s, depth)
  out, comma = [], false
  until s.empty?
    g, s = getitem(s, depth)
    break  if s.empty?
    out += g
    case s[0]
      when '}' then return (comma ? out : out.map{|a| "{#{a}}"}), s[1..-1]
      when ',' then comma, s = true, s[1..-1]
    end
  end
end

strs = <<'EOS'
~/{Downloads,Pictures}/*.{jpg,gif,png}
It{{em,alic}iz,erat}e{d,}, please.
{,{,gotta have{ ,\, again\, }}more }cowbell!
{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
EOS

strs.each_line do |s|
  puts s.chomp!
  puts getitem(s)[0].map{|str| "\t"+str}
  puts
end
```


{{out}}

```txt

~/{Downloads,Pictures}/*.{jpg,gif,png}
	~/Downloads/*.jpg
	~/Downloads/*.gif
	~/Downloads/*.png
	~/Pictures/*.jpg
	~/Pictures/*.gif
	~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.
	Itemized, please.
	Itemize, please.
	Italicized, please.
	Italicize, please.
	Iterated, please.
	Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!
	cowbell!
	more cowbell!
	gotta have more cowbell!
	gotta have\, again\, more cowbell!

{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
	{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
	{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```



## Rust


```rust
const OPEN_CHAR: char = '{';
const CLOSE_CHAR: char = '}';
const SEPARATOR: char = ',';
const ESCAPE: char = '\\';

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Open,
    Close,
    Separator,
    Payload(String),
    Branches(Branches),
}

impl From<char> for Token {
    fn from(ch: char) -> Token {
        match ch {
            OPEN_CHAR => Token::Open,
            CLOSE_CHAR => Token::Close,
            SEPARATOR => Token::Separator,
            _ => panic!("Non tokenizable char!"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Branches {
    tokens: Vec<Vec<Token>>,
}

impl Branches {
    fn new() -> Branches {
        Branches{
            tokens: Vec::new(),
        }
    }

    fn add_branch(&mut self, branch: Vec<Token>) {
        self.tokens.push(branch);
    }

    fn from(tokens: &Vec<Token>) -> Branches {
        let mut branches = Branches::new();
        let mut tail = tokens.clone();
        while let Some(pos) = tail.iter().position(|token| { *token == Token::Separator }) {
            let mut rest = tail.split_off(pos);
            branches.add_branch(tail);
            rest.remove(0);
            tail = rest;
        }
        branches.add_branch(tail);
        branches
    }
}

impl From<Branches> for Token {
    fn from(branches: Branches) -> Token {
        Token::Branches(branches)
    }
}

impl From<Vec<Token>> for Branches {
    fn from(tokens: Vec<Token>) -> Branches {
        Branches::from(&tokens)
    }
}

impl From<Token> for String {
    fn from(token: Token) -> String {
        match token {
            Token::Branches(_) => panic!("Cannot convert to String!"),
            Token::Payload(text) => text,
            Token::Open => OPEN_CHAR.to_string(),
            Token::Close => CLOSE_CHAR.to_string(),
            Token::Separator => SEPARATOR.to_string(),
        }
    }
}

impl From<Branches> for Vec<String> {
    fn from(branches: Branches) -> Vec<String> {
        let Branches{ tokens: token_lines } = branches;
        let mut vec: Vec<String> = Vec::new();
        let braces = { if token_lines.len() == 1 { true } else { false } };
        for tokens in token_lines {
            let mut vec_string = output(tokens);
            vec.append(&mut vec_string);
        }
        if braces {
            vec.iter()
                .map(|line| {
                    format!("{}{}{}", OPEN_CHAR, line, CLOSE_CHAR)
                }).
                collect::<Vec<String>>()
        } else {
            vec
        }
    }
}

impl From<Token> for Vec<String> {
    fn from(token: Token) -> Vec<String> {
        match token {
            Token::Branches(branches) => {
                branches.into()
            },
            _ => {
                let frag: String = token.into();
                vec![frag]
            },
        }
    }
}

fn tokenize(string: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = string.chars();
    let mut payload = String::new();
    while let Some(ch) = chars.next() {
        match ch {
            OPEN_CHAR | SEPARATOR | CLOSE_CHAR => {
                if payload.len() > 0 {
                    tokens.push(Token::Payload(payload));
                }
                payload = String::new();
                if ch == CLOSE_CHAR {
                    let pos = tokens.iter().rposition(|token| *token == Token::Open);
                    if let Some(pos) = pos {
                        let branches: Branches = {
                            let mut to_branches = tokens.split_off(pos);
                            to_branches.remove(0);
                            to_branches
                        }.into();
                        tokens.push(branches.into());
                    } else {
                        tokens.push(ch.into());
                    }
                } else {
                    tokens.push(ch.into());
                }
            },
            ESCAPE => {
                payload.push(ch);
                if let Some(next_char) = chars.next() {
                    payload.push(next_char);
                }
            },
            _ => payload.push(ch),
        }
    }
    let payload = payload.trim_end();
    if payload.len() > 0 {
        tokens.push(Token::Payload(payload.into()));
    }
    tokens
}

fn output(tokens: Vec<Token>) -> Vec<String> {
    let mut output: Vec<String> = vec![String::new()];
    for token in tokens {
        let mut aux: Vec<String> = Vec::new();
        let strings: Vec<String> = token.into();
        for root in &output {
            for string in &strings {
                aux.push({format!("{}{}", root, string)});
            }
        }
        output = aux;
    }
    output
}

fn main() {
    let mut input: String = String::new();
    std::io::stdin().read_line(&mut input).unwrap();

    let tokens: Vec<Token> = tokenize(&input);
    // println!("Tokens:\n{:#?}", tokens);

    let output = output(tokens);
    for line in &output {
        println!("{}", line);
    }
}
```



{{out}}

```txt
INPUT -> ~/{Downloads,Pictures}/*.{jpg,gif,png}
~/Downloads/*.jpg
~/Downloads/*.gif
~/Downloads/*.png
~/Pictures/*.jpg
~/Pictures/*.gif
~/Pictures/*.png

INPUT -> It{{em,alic}iz,erat}e{d,}, please.
Itemized, please.
Itemize, please.
Italicized, please.
Italicize, please.
Iterated, please.
Iterate, please.

INPUT -> {,{,gotta have{ ,\, again\, }}more }cowbell!
cowbell!
more cowbell!
gotta have more cowbell!
gotta have\, again\, more cowbell!

INPUT -> {}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```





{{trans|Python}}

```rust

fn main() {
    let input = "~/{Downloads,Pictures}/*.{jpg,gif,png}
It{{em,alic}iz,erat}e{d,}, please.
{,{,gotta have{ ,\\, again\\, }}more }cowbell!
{}} some }{,{\\\\{ edge, edge} \\,}{ cases, {here} \\\\\\\\\\}";

    for line in input.split("\n") {
        println!("{}", line);
        for line in get_item(line, 0).0 {
            println!("\t{}", line);
        }
    }
}

fn get_item(s: &str, depth: usize) -> (Vec<String>, &str) {
    let mut out = vec![String::new()];
    let mut s = s;
    while !s.is_empty() {
        let mut c = s.chars().nth(0).unwrap().to_string();
        if depth > 0 && (c == "," || c == "}") {
            return (out, s);
        }
        if c == "{" {
            let x = get_group(&s[1..], depth + 1);
            if x.is_some() {
                let (items, s_new) = x.unwrap();
                out = out
                    .iter()
                    .map(|out_item| {
                        items
                            .iter()
                            .map(|item| format!("{}{}", out_item, item))
                            .collect::<Vec<String>>()
                    })
                    .flatten()
                    .collect();
                s = s_new;
                continue;
            }
        }
        if c == "\\" && s.len() > 1 {
            s = &s[1..];
            c.push(s.chars().nth(0).unwrap());
        }
        out = out
            .iter()
            .map(|o| [o, &c[..]].concat())
            .collect::<Vec<String>>();
        s = &s[1..];
    }
    (out, s)
}

fn get_group(s: &str, depth: usize) -> Option<(Vec<String>, &str)> {
    let mut out: Vec<String> = vec![];
    let mut comma = false;
    let mut s = s;
    while !s.is_empty() {
        let (g, new_s) = get_item(s, depth);
        s = new_s;
        if s.is_empty() {
            break;
        }
        out.extend(g);
        if s.chars().nth(0).unwrap() == '}' {
            if comma {
                return Some((out, &s[1..]));
            }
            return Some((
                out.iter().map(|item| format!("{{{}}}", item)).collect(),
                &s[1..],
            ));
        }
        if s.chars().nth(0).unwrap() == ',' {
            comma = true;
            s = &s[1..];
        }
    }
    None
}

```


{{out}}

```txt

~/{Downloads,Pictures}/*.{jpg,gif,png}
	~/Downloads/*.jpg
	~/Downloads/*.gif
	~/Downloads/*.png
	~/Pictures/*.jpg
	~/Pictures/*.gif
	~/Pictures/*.png
It{{em,alic}iz,erat}e{d,}, please.
	Itemized, please.
	Itemize, please.
	Italicized, please.
	Italicize, please.
	Iterated, please.
	Iterate, please.
{,{,gotta have{ ,\, again\, }}more }cowbell!
	cowbell!
	more cowbell!
	gotta have more cowbell!
	gotta have\, again\, more cowbell!
{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
	{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
	{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```



## Scala

{{works with|Scala|2.11}}

```scala

import collection.mutable.ListBuffer
case class State(isChild: Boolean, alts: ListBuffer[String], rem: List[Char])
def expand(s: String): Seq[String] = {
  def parseGroup(s: State): State = s.rem match {
    case Nil =>  s.copy(alts = ListBuffer("{" + s.alts.mkString(",")))
    case ('{' | ',')::sp =>
      val newS = State(true, ListBuffer(""), rem = sp)
      val elem = parseElem(newS)
      elem.rem match {
        case Nil =>     elem.copy(alts = elem.alts.map(a => "{" + s.alts.map(_ + ",").mkString("") + a))
        case elemrem => parseGroup(s.copy(alts = (s.alts ++= elem.alts), rem = elem.rem))
      }
    case '}'::sp =>
      if (s.alts.isEmpty)          s.copy(alts = ListBuffer("{}"), rem = sp)
      else if(s.alts.length == 1)  s.copy(alts = ListBuffer("{"+s.alts.head + "}"), rem = sp)
      else                         s.copy(rem = sp)
    case _ => throw new Exception("parseGroup should be called only with delimitors")
  }
  def parseElem(s: State): State = s.rem match {
    case Nil =>  s
    case '{'::sp =>
      val ys = parseGroup(State(true, ListBuffer(), s.rem))
      val newAlts = for { x <- s.alts; y <- ys.alts} yield x + y
      parseElem(s.copy(alts = newAlts, rem = ys.rem))
    case (',' | '}')::_ if s.isChild => s
    case '\\'::c::sp => parseElem(s.copy(alts = s.alts.map(_ + '\\' + c), rem = sp))
    case c::sp =>       parseElem(s.copy(alts = s.alts.map(_ + c), rem = sp))
  }
  parseElem(State(false, ListBuffer(""), s.toList)).alts
}

```

Demonstrating:

```scala

println(expand("""~/{Downloads,Pictures}/*.{jpg,gif,png}""") mkString "\n")
println(expand("It{{em,alic}iz,erat}e{d,}, please.") mkString "\n")
println(expand("""{,{,gotta have{ ,\, again\, }}more }cowbell!""") mkString "\n")
println(expand("""{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}""") mkString "\n")

```

{{out}}

```txt

~/Downloads/*.jpg
~/Downloads/*.gif
~/Downloads/*.png
~/Pictures/*.jpg
~/Pictures/*.gif
~/Pictures/*.png
Itemized, please.
Itemize, please.
Italicized, please.
Italicize, please.
Iterated, please.
Iterate, please.
cowbell!
more cowbell!
gotta have more cowbell!
gotta have\, again\, more cowbell!
{}} some }{,\\ edge \,{ cases, {here} \\\\\}
{}} some }{,\\ edge \,{ cases, {here} \\\\\}

```



## Scheme


```Scheme

(define (parse-brackets str)
  ;; We parse the bracketed strings using an accumulator and a stack
  ;;
  ;; lst is the stream of tokens
  ;; acc is the accumulated list of "bits" in this branch
  ;; stk is a list of partially completed accumulators
  ;;
  (let go ((lst (string->list str))
           (acc '())
           (stk '()))
    (cond ((null? lst)
           (unless (null? stk)
             (error "parse-brackets" 'non-empty-stack))
           (comma-sep acc))
          ((eq? (car lst) #\{)
           (go (cdr lst)
               '()
               (cons acc stk)))
          ((eq? (car lst) #\})
           (when (null? stk)
             (error "parse-brackets" 'empty-stack))
           (go (cdr lst)
               (cons (comma-sep acc) (car stk))
               (cdr stk)))
          (else
           (go (cdr lst)
               (cons (car lst) acc)
               stk)))))

(define (comma-sep lst)
  ;; This function is applied to the accumulator, it does three things:
  ;; - it reverses the list
  ;; - joins characters into short strings
  ;; - splits the strings based on commas
  ;;
  (let go ((lst lst)
           (acc '())
           (rst '()))
    (if (null? lst)
        (cons (list->string acc) rst)
        (cond ((eq? #\, (car lst))
               (go (cdr lst)
                   '()
                   (cons (list->string acc) rst)))
              ((char? (car lst))
               (go (cdr lst)
                   (cons (car lst) acc)
                   rst))
              (else
               (go (cdr lst)
                   '()
                   (cons (car lst)
                         (cons (list->string acc)
                               rst))))))))

;; We use the list monad for the nondeterminism needed to expand out every possible bracket option

(define (concatenate lists)
  (apply append lists))

(define (return x)
  (list x))
(define (>>= l f)
  (concatenate (map f l)))

(define (sequence lsts)
  (if (null? lsts)
      (return '())
      (>>= (car lsts)
           (lambda (option)
             (>>= (sequence (cdr lsts))
                  (lambda (tail)
                    (return (cons option tail))))))))

(define (expand-inner tree)
  (if (string? tree)
      (return tree)
      (>>= tree
           (lambda (option)
             (expand-inner option)))))

(define (expand tree)
  (define (string-append* lst) (apply string-append lst))
  (map string-append* (sequence (map expand-inner tree))))

(define (bracket-expand str)
  (expand (parse-brackets str)))

(bracket-expand "It{{em,alic}iz,erat}e{d,}")
;; '("Ited" "Ite" "Itemed" "Iteme" "Italiced" "Italice" "Itized" "Itize" "Iterated" "Iterate")

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: expandBraces (in string: stri) is func
  local
    var boolean: escaped is FALSE;
    var integer: depth is 0;
    var array integer: bracePoints is 0 times 0;
    var array integer: bracesToParse is 0 times 0;
    var string: prefix is "";
    var string: suffix is "";
    var string: option is "";
    var integer: idx is 0;
  begin
    for key idx range stri do
      case stri[idx] of
        when {'\\'}:
          escaped := not escaped;
        when {'{'}:
          incr(depth);
          if not escaped and depth = 1 then
            bracePoints := [] (idx);
          end if;
        when {','}:
          if not escaped and depth = 1 then
            bracePoints &:= idx;
          end if;
        when {'}'}:
          if not escaped and depth = 1 and length(bracePoints) >= 2 then
            bracesToParse := bracePoints & [] (idx);
          end if;
          decr(depth);
      end case;
      if stri[idx] <> '\\' then
        escaped := FALSE;
      end if;
    end for;
    if length(bracesToParse) <> 0 then
      prefix := stri[.. pred(bracesToParse[1])];
      suffix := stri[succ(bracesToParse[length(bracesToParse)]) ..];
      for idx range 1 to pred(length(bracesToParse)) do
        option := stri[succ(bracesToParse[idx]) .. pred(bracesToParse[succ(idx)])];
        expandBraces(prefix & option & suffix);
      end for;
    else
      writeln(stri);
    end if;
  end func;

const proc: main is func
  local
    var string: stri is "";
  begin
    for stri range [] ("It{{em,alic}iz,erat}e{d,}, please.",
                       "~/{Downloads,Pictures}/*.{jpg,gif,png}",
                       "{,{,gotta have{ ,\\, again\\, }}more }cowbell!",
                       "{}} some }{,{\\\\{ edge, edge} \\,}{ cases, {here} \\\\\\\\\\}") do
      writeln;
      expandBraces(stri);
    end for;
  end func;
```


{{out}}

```txt


Itemized, please.
Italicized, please.
Iterated, please.
Itemize, please.
Italicize, please.
Iterate, please.

~/Downloads/*.jpg
~/Pictures/*.jpg
~/Downloads/*.gif
~/Pictures/*.gif
~/Downloads/*.png
~/Pictures/*.png

cowbell!
more cowbell!
gotta have more cowbell!
gotta have\, again\, more cowbell!

{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```



## Sidef

{{trans|Perl}}

```ruby
func brace_expand (input) {
    var current = ['']
    var stack = [[current]]

    loop {
        var t = input.match(
            /\G ((?:[^\\{,}]++ | \\(?:.|\z))++ | . )/gx
        )[0] \\ break

        if (t == '{') {
            stack << [current = ['']]
        }
        elsif ((t == ',') && (stack.len > 1)) {
            stack[-1] << (current = [''])
        }
        elsif ((t == '}') && (stack.len > 1)) {
            var group = stack.pop
            current   = stack[-1][-1]

            # handle the case of brace pairs without commas:
            group[0][] = group[0].map{ '{'+_+'}' }... if (group.len == 1)

            current[] = current.map { |c|
                group.map { .map { c + _ }... }...
            }...
        }
        else {
            current[] = current.map { _ + t }...
        }
    }

    # handle the case of missing closing braces:
    while (stack.len > 1) {

        var right = stack[-1].pop
        var sep = ','

        if (stack[-1].is_empty) {
            sep = '{'
            stack.pop
        }

        current = stack[-1][-1]
        current[] = current.map  { |c|
            right.map { c + sep + _ }...
        }...
    }

    return current
}

DATA.each { |line|
    say line
    brace_expand(line).each { "\t#{_}".say }
    say ''
}

__DATA__
~/{Downloads,Pictures}/*.{jpg,gif,png}
It{{em,alic}iz,erat}e{d,}, please.
{,{,gotta have{ ,\, again\, }}more }cowbell!
{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
```

{{out}}

```txt

~/{Downloads,Pictures}/*.{jpg,gif,png}
	~/Downloads/*.jpg
	~/Downloads/*.gif
	~/Downloads/*.png
	~/Pictures/*.jpg
	~/Pictures/*.gif
	~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.
	Itemized, please.
	Itemize, please.
	Italicized, please.
	Italicize, please.
	Iterated, please.
	Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!
	cowbell!
	more cowbell!
	gotta have more cowbell!
	gotta have\, again\, more cowbell!

{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
	{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
	{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```


## Simula

{{trans|Python}}

```simula
CLASS ARRAYLISTS;
BEGIN

   CLASS ITEM;;

   CLASS ITEMARRAY(N); INTEGER N;
   BEGIN
      REF(ITEM) ARRAY DATA(1:N);
   !  OUTTEXT("NEW ITEMARRAY WITH ");!OUTINT(N, 0);!OUTTEXT(" ELEMENTS");
   !  OUTIMAGE;
   END;

   CLASS ARRAYLIST;
   BEGIN

      PROCEDURE EXPAND(N); INTEGER N;
      BEGIN
         INTEGER I;
         REF(ITEMARRAY) TEMP;
      !  OUTTEXT("EXPAND TO CAPACITY ");!OUTINT(N, 0);!OUTIMAGE;
         TEMP :- NEW ITEMARRAY(N);
         FOR I := 1 STEP 1 UNTIL SIZE DO
            TEMP.DATA(I) :- ITEMS.DATA(I);
         ITEMS :- TEMP;
      END;

      PROCEDURE ADD(T); REF(ITEM) T;
      BEGIN
         IF SIZE + 1 > CAPACITY THEN
         BEGIN
            CAPACITY := 2 * CAPACITY;
            EXPAND(CAPACITY);
         END;
         SIZE := SIZE + 1;
         ITEMS.DATA(SIZE) :- T;
      !  OUTTEXT("SIZE IS ");!OUTINT(SIZE, 0);!OUTIMAGE;
      END;

      PROCEDURE REMOVE(I); INTEGER I;
      BEGIN
        INTEGER J;
        IF I < 1 OR I > SIZE THEN ERROR("REMOVE: INDEX OUT OF BOUNDS");
        FOR J := I STEP 1 UNTIL SIZE - 1 DO
          ITEMS.DATA(J) :- ITEMS.DATA(J + 1);
        ITEMS.DATA(SIZE) :- NONE;
        SIZE := SIZE - 1;
      END;

      REF(ITEM) PROCEDURE GET(I); INTEGER I;
      BEGIN
         IF I < 1 OR I > SIZE THEN ERROR("GET: INDEX OUT OF BOUNDS");
         GET :- ITEMS.DATA(I);
      END;

      INTEGER CAPACITY;
      INTEGER SIZE;
      REF(ITEMARRAY) ITEMS;

      CAPACITY := 20;
      SIZE := 0;
      EXPAND(CAPACITY);

   END;


   ITEM CLASS TEXTITEM(TXT); TEXT TXT;;

   ARRAYLIST CLASS TEXTARRAYLIST;
   BEGIN
      PROCEDURE ADD(T); TEXT T;
         THIS TEXTARRAYLIST QUA ARRAYLIST.ADD(NEW TEXTITEM(T));
      TEXT PROCEDURE GET(I); INTEGER I;
         GET :- THIS TEXTARRAYLIST QUA ARRAYLIST.GET(I) QUA TEXTITEM.TXT;
   END;


   ITEM CLASS REALITEM(X); REAL X;;

   ARRAYLIST CLASS REALARRAYLIST;
   BEGIN
      PROCEDURE ADD(X); REAL X;
         THIS REALARRAYLIST QUA ARRAYLIST.ADD(NEW REALITEM(X));
      REAL PROCEDURE GET(I); INTEGER I;
         GET := THIS REALARRAYLIST QUA ARRAYLIST.GET(I) QUA REALITEM.X;
   END;

END;
```

```simula
EXTERNAL CLASS ARRAYLISTS;
ARRAYLISTS
BEGIN


    CLASS TUPLE(TLIST,T);
        REF(TEXTARRAYLIST) TLIST;
        TEXT T;
    BEGIN
    END TUPLE;


    REF(TUPLE) PROCEDURE GETITEM(S, DEPTH);
        TEXT S;
        INTEGER DEPTH;
    BEGIN
        REF(TUPLE) RESULT;
        REF(TEXTARRAYLIST) OUT;
        OUT :- NEW TEXTARRAYLIST;
        OUT.ADD("");

    CONTINUE:
        WHILE S.LENGTH > 0 DO BEGIN
            CHARACTER C;
            TEXT CTEXT;

            S.SETPOS(1); C := S.GETCHAR; CTEXT :- BLANKS(1); CTEXT.PUTCHAR(C);
            IF DEPTH > 0 AND (C = ',' OR C = '}') THEN
                GOTO RETURN;

            IF C = '{' THEN BEGIN
                REF(TUPLE) X;

                X :- GETGROUP(S.SUB(2,S.LENGTH-1), DEPTH + 1);
                IF X =/= NONE THEN BEGIN
                    REF(TEXTARRAYLIST) OUT2;
                    INTEGER A,B;
                    ! OUT,S = [A+B FOR A IN OUT FOR B IN X[0]], X[1] ;
                    OUT2 :- NEW TEXTARRAYLIST;
                    FOR A := 1 STEP 1 UNTIL OUT.SIZE DO
                        FOR B := 1 STEP 1 UNTIL X.TLIST.SIZE DO
                            OUT2.ADD(OUT.GET(A) & X.TLIST.GET(B));
                    OUT :- OUT2;
                    S :- X.T;
                    GOTO CONTINUE;
                END;
            END;

            IF C = '\' AND S.LENGTH > 1 THEN BEGIN
                TEXT NEWCTEXT;

                NEWCTEXT :- BLANKS(CTEXT.LENGTH + 1);
                NEWCTEXT := CTEXT;
                NEWCTEXT.SETPOS(CTEXT.LENGTH+1);
                S.SETPOS(2); NEWCTEXT.PUTCHAR(S.GETCHAR);
                CTEXT :- NEWCTEXT;
                S :- S.SUB(2,S.LENGTH-1);
            END;

            BEGIN
                REF(TEXTARRAYLIST) OUT2;
                INTEGER A;
                ! OUT, S = [A+CTEXT FOR A IN OUT], S[1:] ;
                OUT2 :- NEW TEXTARRAYLIST;
                FOR A := 1 STEP 1 UNTIL OUT.SIZE DO
                    OUT2.ADD(OUT.GET(A) & CTEXT);
                OUT :- OUT2;
                S :- S.SUB(2,S.LENGTH-1);
            END;
        END;

    RETURN:
        RESULT :- NEW TUPLE(OUT,S);
        GETITEM :- RESULT;
    END GETITEM;


    REF(TUPLE) PROCEDURE GETGROUP(S, DEPTH);
        TEXT S;
        INTEGER DEPTH;
    BEGIN
        REF(TUPLE) RESULT;
        REF(TEXTARRAYLIST) OUT;
        BOOLEAN COMMA,BREAK;

        OUT :- NEW TEXTARRAYLIST;
        COMMA := FALSE;
        BREAK := FALSE;
        WHILE S.LENGTH > 0 AND NOT BREAK DO BEGIN
            REF(TUPLE) GS;
            REF(TEXTARRAYLIST) G;

            GS :- GETITEM(S, DEPTH); G :- GS.TLIST; S :- GS.T;
            IF S.LENGTH = 0 THEN BREAK := TRUE ELSE
            BEGIN
                CHARACTER S1;
                INTEGER I;

                ! out += g ;
                FOR I := 1 STEP 1 UNTIL G.SIZE DO OUT.ADD(G.GET(I));

                S.SETPOS(1); S1 := S.GETCHAR;
                IF S1 = '}' THEN BEGIN
                    IF COMMA THEN BEGIN
                        BREAK := TRUE;
                        RESULT :- NEW TUPLE(OUT, S.SUB(2,S.LENGTH-1));
                    END ELSE
                    BEGIN
                        REF(TEXTARRAYLIST) OUT2;
                        INTEGER I;

                        OUT2 :- NEW TEXTARRAYLIST;
                        FOR I := 1 STEP 1 UNTIL OUT.SIZE DO
                            OUT2.ADD("{" & OUT.GET(I) & "}");
                        BREAK := TRUE;
                        RESULT :- NEW TUPLE(OUT2, S.SUB(2,S.LENGTH-1));
                    END;
                END ELSE
                IF S1 = ',' THEN BEGIN
                    COMMA := TRUE;
                    S :- S.SUB(2,S.LENGTH-1);
                END;
            END;
        END;
        GETGROUP :- RESULT;
    END GETGROUP;


    TEXT INP;

    FOR INP :- "~/{Downloads,Pictures}/*.{jpg,gif,png}",
               "It{{em,alic}iz,erat}e{d,}, please.",
               "{,{,gotta have{ ,\, again\, }}more }cowbell!",
               "{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}" DO
    BEGIN
        REF(TUPLE) RES;
        INTEGER I;

        RES :- GETITEM(INP,0);
        OUTTEXT(INP);
        OUTIMAGE;
        FOR I := 1 STEP 1 UNTIL RES.TLIST.SIZE DO BEGIN
            OUTTEXT("        ");
            OUTTEXT(RES.TLIST.GET(I));
            OUTIMAGE;
        END;
        OUTIMAGE;
    END;

END

```

{{out}}

```txt
~/{Downloads,Pictures}/*.{jpg,gif,png}
        ~/Downloads/*.jpg
        ~/Downloads/*.gif
        ~/Downloads/*.png
        ~/Pictures/*.jpg
        ~/Pictures/*.gif
        ~/Pictures/*.png

It{{em,alic}iz,erat}e{d,}, please.
        Itemized, please.
        Itemize, please.
        Italicized, please.
        Italicize, please.
        Iterated, please.
        Iterate, please.

{,{,gotta have{ ,\, again\, }}more }cowbell!
        cowbell!
        more cowbell!
        gotta have more cowbell!
        gotta have\, again\, more cowbell!

{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
        {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
        {}} some }{,{\\ edge \,}{ cases, {here} \\\\\}


```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

proc combine {cases1 cases2 {insert ""}} {
    set result {}
    foreach c1 $cases1 {
	foreach c2 $cases2 {
	    lappend result $c1$insert$c2
	}
    }
    return $result
}
proc expand {string *expvar} {
    upvar 1 ${*expvar} expanded
    set a {}
    set result {}
    set depth 0
    foreach token [regexp -all -inline {(?:[^\\{},]|\\.)+|[\\{},]} $string] {
	switch $token {
	    "," {
		if {$depth == 0} {
		    lappend result {*}[commatize $a]
		    set a {}
		    set expanded 1
		    continue
		}
	    }
	    "\{" {incr depth  1}
	    "\}" {incr depth -1}
	}
	append a $token
    }
    lappend result {*}[commatize $a]
    return $result
}
proc commatize {string} {
    set current {{}}
    set depth 0
    foreach token [regexp -all -inline {(?:[^\\{},]|\\.)+|[\\{},]} $string] {
	switch $token {
	    "\{" {
		if {[incr depth] == 1} {
		    set collect {}
		    continue
		}
	    }
	    "\}" {
		if {[incr depth -1] == 0} {
		    set foundComma 0
		    set exp [expand $collect foundComma]
		    if {!$foundComma} {
			set exp [lmap c [commatize $collect] {set c \{$c\}}]
		    }
		    set current [combine $current $exp]
		    continue
		} elseif {$depth < 0} {
		    set depth 0
		}
	    }
	}
	if {$depth} {
	    append collect $token
	} else {
	    set current [lmap s $current {set s $s$token}]
	}
    }
    if {$depth} {
	set current [combine $current [commatize $collect] "\{"]
    }
    return $current
}
```

Demonstrating:

```tcl
foreach testcase {
    "~/{Downloads,Pictures}/*.{jpg,gif,png}"
    "It{{em,alic}iz,erat}e{d,}, please."
    "{,{,gotta have{ ,\\, again\\, }}more }cowbell!"
    "\{\}\} some \}\{,\{\\\\\{ edge, edge\} \\,\}\{ cases, \{here\} \\\\\\\\\\\}"
} {
    puts $testcase\n\t[join [commatize $testcase] \n\t]
}
```

{{out}}

```txt

~/{Downloads,Pictures}/*.{jpg,gif,png}
	~/Downloads/*.jpg
	~/Downloads/*.gif
	~/Downloads/*.png
	~/Pictures/*.jpg
	~/Pictures/*.gif
	~/Pictures/*.png
It{{em,alic}iz,erat}e{d,}, please.
	Itemized, please.
	Itemize, please.
	Italicized, please.
	Italicize, please.
	Iterated, please.
	Iterate, please.
{,{,gotta have{ ,\, again\, }}more }cowbell!
	cowbell!
	more cowbell!
	gotta have more cowbell!
	gotta have\, again\, more cowbell!
{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}
	{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}
	{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}

```



## zkl

This is a two pass algorithm (2*length(string)), one pass to find valid {} pairs, the next pass to expand them.

```zkl
fcn eyeball(code,ps=L(),brace=False){ //-->indexes of valid braces & commas
   cs:=L();
   foreach c in (code){ // start fresh or continue (if recursing)
      switch(c){
	 case("\\"){ __cWalker.next(); }
	 case(",") { if(brace) cs.append(__cWalker.n); } // maybe valid
	 case("{") {  // this is real only if there is matching } and a comma
	    n:=__cWalker.n;
	    _,cz:=self.fcn(__cWalker,ps,True);
	    if(cz){ ps.append(n,__cWalker.n); ps.extend(cz) } // valid {} pair
	 }
	 case("}"){ if(brace) return(ps,cs); }
      }
   }
   return(ps,False)
}

fcn expando(code,strings=T("")){
   reg [const] stack=List(); reg roots,cs; bs,_:=eyeball(code);
   foreach c in (code){
      if(bs.holds(__cWalker.n)){
         if     (c=="{") { stack.append(cs); cs=0; roots=strings;       }
	 else if(c==",") { stack.append(strings); strings=roots; cs+=1; }
	 else if(c=="}") { do(cs){ strings=stack.pop().extend(strings); } cs=stack.pop(); }
      }else   if(c=="\\"){
	 c="\\"+__cWalker.next();
	 strings=strings.apply('+(c));
      }
      else strings=strings.apply('+(c));
   }
   strings
}
```


```zkl
foreach bs in (T("It{{em,alic}iz,erat}e{d,}",  "~/{Downloads,Pictures}/*.{jpg,gif,png}",
   "It{{em,alic}iz,erat}e{d,}, please.",  "a{2,1}b{X,Y,X}c",  0'|a\\{\\\{b,c\,d}|,
   "{a,b{c{,{d}}e}f",  0'|{,{,gotta have{ ,\, again\, }}more }cowbell!|,
   0'|{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}|))
{
   "%s expands to\n   %s".fmt(bs,expando(bs)).println();
}
```

{{out}}

```txt

It{{em,alic}iz,erat}e{d,} expands to
   L("Itemized","Italicized","Iterated","Itemize","Italicize","Iterate")
~/{Downloads,Pictures}/*.{jpg,gif,png} expands to
   L("~/Downloads/*.jpg","~/Pictures/*.jpg","~/Downloads/*.gif","~/Pictures/*.gif","~/Downloads/*.png","~/Pictures/*.png")
It{{em,alic}iz,erat}e{d,}, please. expands to
   L("Itemized, please.","Italicized, please.","Iterated, please.","Itemize, please.","Italicize, please.","Iterate, please.")
a{2,1}b{X,Y,X}c expands to
   L("a2bXc","a1bXc","a2bYc","a1bYc","a2bXc","a1bXc")
a\\{\\\{b,c\,d} expands to
   L("a\\\\\{b","a\\c\,d")
{a,b{c{,{d}}e}f expands to
   L("{a,b{ce}f","{a,b{c{d}e}f")
{,{,gotta have{ ,\, again\, }}more }cowbell! expands to
   L("cowbell!","more cowbell!","gotta have more cowbell!","gotta have\, again\, more cowbell!")
{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\} expands to
   L("{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}","{}} some }{,{\\ edge \,}{ cases, {here} \\\\\}")

```

