+++
title = "Rosetta Code/Fix code tags"
description = ""
date = 2019-07-28T12:42:09Z
aliases = []
[extra]
id = 3320
[taxonomies]
categories = []
tags = []
+++

{{task|Text processing}}
[[Category:Rosetta Code related]]

;Task:
Fix Rosetta Code deprecated code tags, with these rules:

```txt

Change <%s> to <lang %s>
Change </%s> to
```

Change <code %s> to <lang %s>
Change </code> to
```


```



;Usage:

```txt

./convert.py < wikisource.txt > converted.txt

```






## AutoHotkey


```AutoHotkey>; usage:
 fixtags.ahk input.txt ouput.txt
FileRead, text, %1%
langs = ada,awk,autohotkey,etc
slang = /lang
slang := "<" . slang . "/>"
Loop, Parse, langs, `,
{
  tag1 = <%A_LoopField%>
  tag2 = </%A_LoopField%>
  text := RegExReplace(text, tag1, "<lang " . A_LoopField . ">")
  text := RegExReplace(text, tag2, slang)
  text := RegExReplace(text, "<code (.+?)>(.*?)</code>"
          , "<lang $1>$2" . slang)
}
FileAppend, % text, %2%

```



## D

{{trans|Python}}

```d
import std.stdio, std.regex, std.string, std.array;

immutable langs = "_div abap actionscript actionscript3 ada apache
applescript apt_sources asm asp autoit avisynth bash basic4gl bf
blitzbasic bnf boo c c_mac caddcl cadlisp cfdg cfm cil cobol cpp
cpp-qt csharp css d delphi diff dos dot eiffel email fortran
freebasic genero gettext glsl gml gnuplot groovy haskell hq9plus
html4strict idl ini inno intercal io java java5 javascript kixtart
klonec klonecpp latex lisp lolcode lotusformulas lotusscript
lscript lua m68k make matlab mirc modula3 mpasm mxml mysql nsis
objc ocaml ocaml-brief oobas oracle11 oracle8 pascal per perl php
php-brief pic16 pixelbender plsql povray powershell progress
prolog providex python qbasic rails reg robots ruby sas scala
scheme scilab sdlbasic smalltalk smarty sql tcl teraterm text
thinbasic tsql typoscript vb vbnet verilog vhdl vim visualfoxpro
visualprolog whitespace winbatch xml xorg_conf xpp z80".split;

string fixTags(string text) {
    static immutable slang = "/lang";
    static immutable code = "code";

    foreach (immutable lang; langs) {
        text = text.replace("<%s>".format(lang),
                            "<lang %s>".format(lang));
        text = text.replace("</%s>".format(lang),
                            "<%s>".format(slang));
    }

    return text.replace("<%s (.+?)>(.*?)</%s>"
                        .format(code, code).regex("g"),
                        "<lang $1>$2<%s>".format(slang));
}

void main() {
    ("lorem ipsum <c>some c code</c>dolor sit amet, <csharp>some " ~
    "csharp code</csharp> consectetur adipisicing elit, <code r>" ~
    " some r code </code>sed do eiusmod tempor incididunt")
    .fixTags
    .writeln;
}
```

{{out}}

```txt
lorem ipsum
```c>some c code</lang
dolor sit amet,
```csharp>some csharp code</lang
 consectetur adipisicing elit,
```r> some r code
```
sed do eiusmod tempor incididunt</pre



## Erlang

Commented away are 3 lines that would create a dict of the existing languages on Rosetta Code. Since the examples have 3 imaginary code tags, I replaced that code this way.

```Erlang

#! /usr/bin/env escript
-module( fix_code_tags ).
-mode( compile ).

main( _ ) ->
	File_lines = loop( io:get_line(""), [] ),
	Code_fixed_lines = fix_code_tag( binary:list_to_bin(File_lines) ),
%	true = code:add_pathz( "ebin" ),
%	ok = find_unimplemented_tasks:init(),
%	Dict = dict:from_list( [dict_tuple(X) || X <- rank_languages_by_popularity:rosettacode_languages()],
	Dict = dict:from_list( [dict_tuple(X) || X <- ["foo", "bar", "baz"]] ),
	{_Dict, All_fixed_lines} = lists:foldl( fun fix_language_tag/2, {Dict, Code_fixed_lines}, dict:fetch_keys(Dict) ),
	io:fwrite( "~s", [binary:bin_to_list(All_fixed_lines)] ).



dict_tuple( Language ) -> {binary:list_to_bin(string:to_lower(Language)), binary:list_to_bin(Language)}.

fix_code_tag( Binary ) ->
	Avoid_wiki = binary:list_to_bin( [<<"<">>, <<"lang ">>] ),
	Code_fixed_lines = binary:replace( Binary, <<"<code ">>, Avoid_wiki, [global] ),
	Avoid_wiki_again = binary:list_to_bin( [<<"</">>, <<"lang ">>] ),
	binary:replace( Code_fixed_lines, <<"</code>">>, Avoid_wiki_again, [global] ).

fix_language_tag( Language_key, {Dict, Binary} ) ->
	Language = fix_language_tag_rosettacode_language( Language_key, dict:find(Language_key, Dict) ),
	Language_start_old = binary:list_to_bin( [<<"<">>, Language, <<">">>] ),
	Language_start_new = binary:list_to_bin( [<<"<">>, <<"lang ">>, Language, <<">">>] ),
	Fixed_lines = binary:replace( Binary, Language_start_old, Language_start_new, [global] ),
	Language_stop_old = binary:list_to_bin( [<<"</">>, Language, <<">">>] ),
	Language_stop_new = binary:list_to_bin( [<<"</">>, <<"lang>">>] ),
	{Dict, binary:replace( Fixed_lines, Language_stop_old, Language_stop_new, [global] )}.

fix_language_tag_rosettacode_language( _Language_key, {ok, Language} ) -> Language;
fix_language_tag_rosettacode_language( Language_key, error ) -> Language_key.

loop( eof, Acc ) -> lists:reverse( Acc );
loop( Line, Acc ) -> loop( io:get_line(""), [Line | Acc] ).

```

{{out}}

```txt

% cat fix_code_tags
Lorem ipsum <code foo>saepe audire</code> elaboraret ne quo, id equidem
atomorum inciderint usu. <foo>In sit inermis deleniti percipit</foo>,
ius ex tale civibus omittam. <barf>Vix ut doctus cetero invenire</barf>, his eu
altera electram. Tota adhuc altera te sea, <code bar>soluta appetere ut mel</bar>.
Quo quis graecis vivendo te, <baz>posse nullam lobortis ex usu</code>. Eam volumus perpetua
constituto id, mea an omittam fierent vituperatoribus.
% fix_code_tags.escript < fix_code_tags
Lorem ipsum
```foo>saepe audire</lang
 elaboraret ne quo, id equidem
atomorum inciderint usu.
```foo>In sit inermis deleniti percipit</lang
,
ius ex tale civibus omittam. <barf>Vix ut doctus cetero invenire</barf>, his eu
altera electram. Tota adhuc altera te sea,
```bar>soluta appetere ut mel</lang
.
Quo quis graecis vivendo te,
```baz>posse nullam lobortis ex usu</lang
. Eam volumus perpetua
constituto id, mea an omittam fierent vituperatoribus.

```


=={{header|F_Sharp|F#}}==
While the ubiquitous loop over languages approach can be used, here we capture all tag variations to fix in one dotNet regex.

```fsharp
open System
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv =
    let langs = [| "foo"; "foo 2"; "bar"; "baz" |];    // An array of (pseudo) languages we handle
    let regexStringAlternationOfLanguageNames = String.Join("|", (Array.map Regex.Escape langs))
    let regexForOldLangSyntax =
        new Regex(String.Format("""
            <                   # Opening of a tag.
            (                   # Group alternation of 2 cases
                (                   # Group for alternation of 2 cases with a language name
                    (?<CloseMarker>/)   # Might be a closing tag,
                    |                   # Or
                    code\s              # an old <code ...> tag
                )?                  # End of alternation; optional
                \b(?<Lang>{0})\b    # Followed by the captured Language alternation
            |                   # Or
                (?<CloseMarker>/code)# An old </code> end tag
            )                   # End of group
            >                   # The final tag closing
            """, regexStringAlternationOfLanguageNames),
            RegexOptions.IgnorePatternWhitespace ||| RegexOptions.ExplicitCapture)

    let replaceEvaluator (m : Match) =
        if m.Groups.Item("CloseMarker").Length > 0 then "</" + "lang>"
        else "<lang " + m.Groups.Item("Lang").Value + ">"

    printfn "%s" (regexForOldLangSyntax.Replace(Console.In.ReadToEnd(), replaceEvaluator))
    0
```

Output

```txt
&gt;Rosetta.exe
<foo 2>...</foo 2>
<code bar>...</code>
^Z

```foo 2>...</lang


```bar>...
```
</pre



## Go


```go
package main

import "fmt"
import "io/ioutil"
import "log"
import "os"
import "regexp"
import "strings"

func main() {
	err := fix()
	if err != nil {
		log.Fatalln(err)
	}
}

func fix() (err error) {
	buf, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		return err
	}
	out, err := Lang(string(buf))
	if err != nil {
		return err
	}
	fmt.Println(out)
	return nil
}

func Lang(in string) (out string, err error) {
	reg := regexp.MustCompile("<[^>]+>")
	out = reg.ReplaceAllStringFunc(in, repl)
	return out, nil
}

func repl(in string) (out string) {
	if in == "</code>" {
		// Change </code> to </ lang>
		return "</"+"lang>"
	}

	// mid is the content in between '<' and '>'.
	mid := in[1 : len(in)-1]

	// thanks, random lua guy
	var langs = []string{
		"abap", "actionscript", "actionscript3", "ada", "apache", "applescript",
		"apt_sources", "asm", "asp", "autoit", "avisynth", "bash", "basic4gl",
		"bf", "blitzbasic", "bnf", "boo", "c", "caddcl", "cadlisp", "cfdg", "cfm",
		"cil", "c_mac", "cobol", "cpp", "cpp-qt", "csharp", "css", "d", "delphi",
		"diff", "_div", "dos", "dot", "eiffel", "email", "fortran", "freebasic",
		"genero", "gettext", "glsl", "gml", "gnuplot", "go", "groovy", "haskell",
		"hq9plus", "html4strict", "idl", "ini", "inno", "intercal", "io", "java",
		"java5", "javascript", "kixtart", "klonec", "klonecpp", "latex", "lisp",
		"lolcode", "lotusformulas", "lotusscript", "lscript", "lua", "m68k",
		"make", "matlab", "mirc", "modula3", "mpasm", "mxml", "mysql", "nsis",
		"objc", "ocaml", "ocaml-brief", "oobas", "oracle11", "oracle8", "pascal",
		"per", "perl", "php", "php-brief", "pic16", "pixelbender", "plsql",
		"povray", "powershell", "progress", "prolog", "providex", "python",
		"qbasic", "rails", "reg", "robots", "ruby", "sas", "scala", "scheme",
		"scilab", "sdlbasic", "smalltalk", "smarty", "sql", "tcl", "teraterm",
		"text", "thinbasic", "tsql", "typoscript", "vb", "vbnet", "verilog",
		"vhdl", "vim", "visualfoxpro", "visualprolog", "whitespace", "winbatch",
		"xml", "xorg_conf", "xpp", "z80",
	}
	for _, lang := range langs {
		if mid == lang {
			// Change <%s> to <lang %s>
			return fmt.Sprintf("<lang %s>", lang)
		}

		if strings.HasPrefix(mid, "/") {
			if mid[len("/"):] == lang {
				// Change </%s> to </ lang>
				return "</"+"lang>"
			}
		}

		if strings.HasPrefix(mid, "code ") {
			if mid[len("code "):] == lang {
				// Change <code %s> to <lang %s>
				return fmt.Sprintf("<lang %s>", lang)
			}
		}
	}

	return in
}
```



## J

'''Solution:'''

```j
require 'printf strings files'

langs=. <;._1 LF -.~ noun define   NB. replace with real lang strings
 foo bar
 baz
)
```


```txt
patterns=. noun define
<%s>|<lang %s>|
</%s>|
```
|
<code %s>|<lang %s>|
</code>|
```
|
)
```


```j
fixCodeTags=: rplc&(, <;._2;._2 &> patterns vbsprintf _5]\ 5#langs)
```


'''Example Usage:'''

```txt
   SampleText=: noun define
Lorem ipsum <code foo>saepe audire</code> elaboraret ne quo, id equidem
atomorum inciderint usu. <foo>In sit inermis deleniti percipit</foo>,
ius ex tale civibus omittam. <barf>Vix ut doctus cetero invenire</barf>, his eu
altera electram. Tota adhuc altera te sea, <code bar>soluta appetere ut mel</bar>.
Quo quis graecis vivendo te, <baz>posse nullam lobortis ex usu</code>. Eam volumus perpetua
constituto id, mea an omittam fierent vituperatoribus.
)

   fixCodeTags SampleText
Lorem ipsum
```foo>saepe audire</lang
 elaboraret ne quo, id equidem
atomorum inciderint usu.
```foo>In sit inermis deleniti percipit</lang
,
ius ex tale civibus omittam. <barf>Vix ut doctus cetero invenire</barf>, his eu
altera electram. Tota adhuc altera te sea,
```bar>soluta appetere ut mel</lang
.
Quo quis graecis vivendo te,
```baz>posse nullam lobortis ex usu</lang
. Eam volumus perpetua
constituto id, mea an omittam fierent vituperatoribus.
```

Reading/writing file:

```j
   'converted.txt' fwrite~ fixCodeTags fread 'wikisource.txt'
```



## Java


```java
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;

public class FixCodeTags
{
	public static void main(String[] args)
	{
		String sourcefile=args[0];
		String convertedfile=args[1];
		convert(sourcefile,convertedfile);
	}
		static String[] languages = {"abap", "actionscript", "actionscript3",
			"ada", "apache", "applescript", "apt_sources", "asm", "asp",
			"autoit", "avisynth", "bar", "bash", "basic4gl", "bf",
			"blitzbasic", "bnf", "boo", "c", "caddcl", "cadlisp", "cfdg",
			"cfm", "cil", "c_mac", "cobol", "cpp", "cpp-qt", "csharp", "css",
			"d", "delphi", "diff", "_div", "dos", "dot", "eiffel", "email",
			"foo", "fortran", "freebasic", "genero", "gettext", "glsl", "gml",
			"gnuplot", "go", "groovy", "haskell", "hq9plus", "html4strict",
			"idl", "ini", "inno", "intercal", "io", "java", "java5",
			"javascript", "kixtart", "klonec", "klonecpp", "latex", "lisp",
			"lolcode", "lotusformulas", "lotusscript", "lscript", "lua",
			"m68k", "make", "matlab", "mirc", "modula3", "mpasm", "mxml",
			"mysql", "nsis", "objc", "ocaml", "ocaml-brief", "oobas",
			"oracle11", "oracle8", "pascal", "per", "perl", "php", "php-brief",
			"pic16", "pixelbender", "plsql", "povray", "powershell",
			"progress", "prolog", "providex", "python", "qbasic", "rails",
			"reg", "robots", "ruby", "sas", "scala", "scheme", "scilab",
			"sdlbasic", "smalltalk", "smarty", "sql", "tcl", "teraterm",
			"text", "thinbasic", "tsql", "typoscript", "vb", "vbnet",
			"verilog", "vhdl", "vim", "visualfoxpro", "visualprolog",
			"whitespace", "winbatch", "xml", "xorg_conf", "xpp", "z80"};
	static void convert(String sourcefile,String convertedfile)
	{
		try
		{
			BufferedReader br=new BufferedReader(new FileReader(sourcefile));
			//String buffer to store contents of the file
			StringBuffer sb=new StringBuffer("");
			String line;
			while((line=br.readLine())!=null)
			{
				for(int i=0;i<languages.length;i++)
				{
					String lang=languages[i];
					line=line.replaceAll("<"+lang+">", "<lang "+lang+">");
					line=line.replaceAll("</"+lang+">", "</"+lang+">");
					line=line.replaceAll("<code "+lang+">", "<lang "+lang+">");
					line=line.replaceAll("</code>", "</"+"lang>");
				}
				sb.append(line);
			}
			br.close();

			FileWriter fw=new FileWriter(new File(convertedfile));
			//Write entire string buffer into the file
			fw.write(sb.toString());
			fw.close();
		}
		catch (Exception e)
		{
			System.out.println("Something went horribly wrong: "+e.getMessage());
		}
	}
}

```


Example:

```txt

user@ubuntu:~$ cat fix.in
Lorem ipsum <code foo>saepe audire</code> elaboraret ne quo, id equidem
atomorum inciderint usu. <foo>In sit inermis deleniti percipit</foo>,
ius ex tale civibus omittam. <barf>Vix ut doctus cetero invenire</barf>, his eu
altera electram. Tota adhuc altera te sea, <code bar>soluta appetere ut mel</bar>.
Quo quis graecis vivendo te, <baz>posse nullam lobortis ex usu</code>. Eam volumus perpetua
constituto id, mea an omittam fierent vituperatoribus.

user@ubuntu:~$ java FixCodeTags fix.in fix.out
user@ubuntu:~$ cat fix.out
Lorem ipsum
```foo>saepe audire</lang
 elaboraret ne quo, id equidem
atomorum inciderint usu.
```foo>In sit inermis deleniti percipit</foo
,
ius ex tale civibus omittam. <barf>Vix ut doctus cetero invenire</barf>, his eu
altera electram. Tota adhuc altera te sea,
```bar>soluta appetere ut mel</bar
.
Quo quis graecis vivendo te, <baz>posse nullam lobortis ex usu
```
. Eam volumus perpetua
constituto id, mea an omittam fierent vituperatoribus.


```



## JavaScript

{{works with|SpiderMonkey}}

```javascript
var langs = ['foo', 'bar', 'baz']; // real list of langs goes here
var end_tag = '</'+'lang>';

var line;
while (line = readline()) {
    line = line.replace(new RegExp('</code>', 'gi'), end_tag);
    for (var i = 0; i < langs.length; i++)
        line = line.replace(new RegExp('<(?:code )?(' + langs[i] + ')>', 'gi'), '<lang $1>')
                   .replace(new RegExp('</' + langs[i] + '>', 'gi'), end_tag);
    print(line);
}
```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}


```julia
function fixtags(text::AbstractString)
    langs = ["ada", "cpp-qt", "pascal", "lscript", "z80", "visualprolog",
            "html4strict", "cil", "objc", "asm", "progress", "teraterm", "hq9plus",
            "genero", "tsql", "email", "pic16", "tcl", "apt_sources", "io", "apache",
            "vhdl", "avisynth", "winbatch", "vbnet", "ini", "scilab", "ocaml-brief",
            "sas", "actionscript3", "qbasic", "perl", "bnf", "cobol", "powershell",
            "php", "kixtart", "visualfoxpro", "mirc", "make", "javascript", "cpp",
            "sdlbasic", "cadlisp", "php-brief", "rails", "verilog", "xml", "csharp",
            "actionscript", "nsis", "bash", "typoscript", "freebasic", "dot",
            "applescript", "haskell", "dos", "oracle8", "cfdg", "glsl", "lotusscript",
            "mpasm", "latex", "sql", "klonec", "ruby", "ocaml", "smarty", "python",
            "oracle11", "caddcl", "robots", "groovy", "smalltalk", "diff", "fortran",
            "cfm", "lua", "modula3", "vb", "autoit", "java", "text", "scala",
            "lotusformulas", "pixelbender", "reg", "_div", "whitespace", "providex",
            "asp", "css", "lolcode", "lisp", "inno", "mysql", "plsql", "matlab",
            "oobas", "vim", "delphi", "xorg_conf", "gml", "prolog", "bf", "per",
            "scheme", "mxml", "d", "basic4gl", "m68k", "gnuplot", "idl", "abap",
            "intercal", "c_mac", "thinbasic", "java5", "xpp", "boo", "klonecpp",
            "blitzbasic", "eiffel", "povray", "c", "gettext"]

    slang = "/lang"
    code  = "code"

    for l in langs
            text = replace(text, "<$l>","<lang $l>")
            text = replace(text, "</$l>", "<$slang>")
    end

    text = replace(text, Regex("(?s)<$code (.+?)>(.*?)</$code>"), "
```\\1
\\2<$slang>")
end

const txt = readstring(ARGS[1])
println(fixtags(txt))
```



## Lua


```lua

--thanks, random python guy
langs = {'ada', 'cpp-qt', 'pascal', 'lscript', 'z80', 'visualprolog',
'html4strict', 'cil', 'objc', 'asm', 'progress', 'teraterm', 'hq9plus',
'genero', 'tsql', 'email', 'pic16', 'tcl', 'apt_sources', 'io', 'apache',
'vhdl', 'avisynth', 'winbatch', 'vbnet', 'ini', 'scilab', 'ocaml-brief',
'sas', 'actionscript3', 'qbasic', 'perl', 'bnf', 'cobol', 'powershell',
'php', 'kixtart', 'visualfoxpro', 'mirc', 'make', 'javascript', 'cpp',
'sdlbasic', 'cadlisp', 'php-brief', 'rails', 'verilog', 'xml', 'csharp',
'actionscript', 'nsis', 'bash', 'typoscript', 'freebasic', 'dot',
'applescript', 'haskell', 'dos', 'oracle8', 'cfdg', 'glsl', 'lotusscript',
'mpasm', 'latex', 'sql', 'klonec', 'ruby', 'ocaml', 'smarty', 'python',
'oracle11', 'caddcl', 'robots', 'groovy', 'smalltalk', 'diff', 'fortran',
'cfm', 'lua', 'modula3', 'vb', 'autoit', 'java', 'text', 'scala',
'lotusformulas', 'pixelbender', 'reg', '_div', 'whitespace', 'providex',
'asp', 'css', 'lolcode', 'lisp', 'inno', 'mysql', 'plsql', 'matlab',
'oobas', 'vim', 'delphi', 'xorg_conf', 'gml', 'prolog', 'bf', 'per',
'scheme', 'mxml', 'd', 'basic4gl', 'm68k', 'gnuplot', 'idl', 'abap',
'intercal', 'c_mac', 'thinbasic', 'java5', 'xpp', 'boo', 'klonecpp',
'blitzbasic', 'eiffel', 'povray', 'c', 'gettext'}

for line in io.lines() do
  for i, v in ipairs(langs) do
	line = line:gsub("<" .. v .. ">", "<lang " .. v .. ">")
    line = line:gsub("<code " .. v .. ">", "<lang " .. v .. ">")
    line = line:gsub("</" .. v .. ">", "</" .. "lang>")  --the weird concatenation is to prevent the markup from breaking
    line = line:gsub("</" .. "code>", "</" .. "lang>")
  end
  print(line)
end

```


## Maple


```Maple>#Used <#/lang
 to desensitize wiki
txt := FileTools[Text][ReadFile]("C:/Users/username/Desktop/text.txt"):
#langs should be a real list of programming languages
langs := ['foo', 'bar', 'baz', 'barf'];
for lan in langs do
	txt := StringTools:-SubstituteAll(txt, cat("<", lan, ">"), cat ("<lang ", lan, ">")):
	txt := StringTools:-SubstituteAll(txt, cat("</", lan, ">"), "<#/lang>"):
	txt := StringTools:-SubstituteAll(txt, cat("<code ", lan, ">"), cat ("<lang ", lan, ">")):
	txt := StringTools:-SubstituteAll(txt, "</code>", "<#/lang>"):
end do;
print(txt);
```

{{Out|Output}}

```txt

"Lorem ipsum
```foo>saepe audire</lang
 elaboraret ne quo, id equidem
atomorum inciderint usu.
```foo>In sit inermis deleniti percipit</lang
,
ius ex tale civibus omittam.
```barf>Vix ut doctus cetero invenire</lang
, his eu
altera electram. Tota adhuc altera te sea,
```bar>soluta appetere ut mel</lang
.
Quo quis graecis vivendo te,
```baz>posse nullam lobortis ex usu</lang
. Eam volumus perpetua
constituto id, mea an omittam fierent vituperatoribus. "

```



## Mathematica


```txt
StringReplace[Import["wikisource.txt"],
{"</"~~Shortest[__]~~">"->"
```
",
("<code "|"<")~~Shortest[x__]~~">"->"<lang "~~ x~~">"}
]>>"converted.txt"

->conversion of sample text
Lorem ipsum
```foo>saepe audire</lang
elaboraret ne quo,id equidem
atomorum inciderint usu.
```foo>In sit inermis deleniti percipit</lang
,
ius ex tale civibus omittam.
```barf>Vix ut doctus cetero invenire</lang
,
his eualtera electram.Tota adhuc altera te sea,
```bar>soluta appetere ut mel</lang
.
Quo quis graecis vivendo te,
```baz>posse nullam lobortis ex usu</lang
.
Eam volumus perpetua constituto id,mea an omittam fierent vituperatoribus

```



## OCaml


```ocaml
#load "str.cma"

let langs =
  Str.split (Str.regexp " ")
    "actionscript ada algol68 amigae applescript autohotkey awk bash basic \
     befunge bf c cfm cobol cpp csharp d delphi e eiffel factor false forth \
     fortran fsharp haskell haxe j java javascript lisaac lisp logo lua m4 \
     mathematica maxscript modula3 moo objc ocaml octave oz pascal perl \
     perl6 php pike pop11 powershell prolog python qbasic r rebol ruby \
     scala scheme slate smalltalk tcl ti89b vbnet vedit"

let read_in ic =
  let buf = Buffer.create 16384
  and tmp = String.create 4096 in
  let rec aux() =
    let bytes = input ic tmp 0 4096 in
    if bytes > 0 then begin
      Buffer.add_substring buf tmp 0 bytes;
      aux()
    end
  in
  (try aux() with End_of_file -> ());
  (Buffer.contents buf)

let repl pat tpl str =
  let reg = Str.regexp_string_case_fold pat in
  let str = Str.global_replace reg tpl str in
  (str)

(* change <%s> to <lang %s> *)
let repl1 lang str =
  let pat = "<" ^ lang ^ ">"
  and tpl = "<lang " ^ lang ^ ">" in
  (repl pat tpl str)

(* change </%s> to </la\ng> *)
let repl2 lang str =
  let pat = "</" ^ lang ^ ">"
  and tpl = "</lang"^">" in
  (repl pat tpl str)

(* change <code %s> to <lang %s> *)
let repl3 lang str =
  let pat = "<code " ^ lang ^ ">"
  and tpl = "<lang " ^ lang ^ ">" in
  (repl pat tpl str)

(* change </code> to </la\ng> *)
let repl4 lang str =
  let pat = "</code>"
  and tpl = "</lang"^">" in
  (repl pat tpl str)


let () =
  print_string (
    List.fold_left (fun str lang ->
        (repl4 lang (repl3 lang (repl2 lang (repl1 lang str))))
      ) (read_in stdin) langs)
```


(in the code the strings <nowiki>
```
</nowiki> have been split in order to not confuse the wiki)

this line of code:

```ocaml
        (repl4 lang (repl3 lang (repl2 lang (repl1 lang str))))
```


could also be written like this:

```ocaml
        List.fold_right (fun repl -> repl lang) [repl1; repl2; repl3; repl4] str
```



Here we implement the <code style='color:#151080; font-weight:bold;'>read_in</code> function to read all the content from an input channel because there is no such function in the standard library, but we can use the [http://code.google.com/p/ocaml-extlib/ extLib] which provides the function <code style='color:#151080; font-weight:bold;'>Std.input_all</code> (in its module [http://ocaml-extlib.googlecode.com/svn/doc/apiref/Std.html Std]).


## Perl


```perl
my @langs = qw(ada cpp-qt pascal lscript z80 visualprolog
html4strict cil objc asm progress teraterm hq9plus genero tsql
email pic16 tcl apt_sources io apache vhdl avisynth winbatch
vbnet ini scilab ocaml-brief sas actionscript3 qbasic perl bnf
cobol powershell php kixtart visualfoxpro mirc make javascript
cpp sdlbasic cadlisp php-brief rails verilog xml csharp
actionscript nsis bash typoscript freebasic dot applescript
haskell dos oracle8 cfdg glsl lotusscript mpasm latex sql klonec
ruby ocaml smarty python oracle11 caddcl robots groovy smalltalk
diff fortran cfm lua modula3 vb autoit java text scala
lotusformulas pixelbender reg _div whitespace providex asp css
lolcode lisp inno mysql plsql matlab oobas vim delphi xorg_conf
gml prolog bf per scheme mxml d basic4gl m68k gnuplot idl abap
intercal c_mac thinbasic java5 xpp boo klonecpp blitzbasic eiffel
povray c gettext);

my $text = join "", <STDIN>;
my $slang="/lang";
for (@langs) {
    $text =~ s|<$_>|<lang $_>|g;
    $text =~ s|</$_>|<$slang>|g;
}

$text =~ s|<code (.+?)>(.*?)</code>|<lang $1>$2<$slang>|sg;

print $text;
```


## Perl 6


```perl6
my @langs = <
    abap actionscript actionscript3 ada apache applescript apt_sources
    asm asp autoit avisynth bash basic4gl bf blitzbasic bnf boo c caddcl
    cadlisp cfdg cfm cil c_mac cobol cpp cpp-qt csharp css d delphi
    diff _div dos dot eiffel email fortran freebasic genero gettext
    glsl gml gnuplot groovy haskell hq9plus html4strict idl ini inno
    intercal io java java5 javascript kixtart klonec klonecpp latex lisp
    lolcode lotusformulas lotusscript lscript lua m68k make matlab mirc
    modula3 mpasm mxml mysql nsis objc ocaml ocaml-brief oobas oracle11
    oracle8 pascal per perl php php-brief pic16 pixelbender plsql povray
    powershell progress prolog providex python qbasic rails reg robots
    ruby sas scala scheme scilab sdlbasic smalltalk smarty sql tcl teraterm
    text thinbasic tsql typoscript vb vbnet verilog vhdl vim visualfoxpro
    visualprolog whitespace winbatch xml xorg_conf xpp z80
>;

$_ = slurp;

for @langs -> $l {
    s:g:i [ '<' 'lang '?  $l '>' ] = "<lang $l>";
    s:g [ '</' $l '>' ] = '</' ~ 'lang>';
}

s:g [ '<code '(.+?) '>' (.*?) '</code>' ] = "<lang $0>{$1}</"~"lang>";

.say;
```


```perl6
use v6;

constant @langs = < abap actionscript actionscript3 ada  … >;

slurp().subst(
    rx:r{
        | '<' <( $<need-add-space>=<?> )> @langs '>'
        | '</' <( @langs )> '>'
        | '<' '/'? <( code )> [<.ws> @langs]? '>'
    },
    'lang' ~ " " x *<need-add-space>.so,
    :g,
).print
```



## Phix

{{trans|D}}

```Phix
constant ltext = `_div abap actionscript actionscript3 ada apache
applescript apt_sources asm asp autoit avisynth bash basic4gl bf
blitzbasic bnf boo c c_mac caddcl cadlisp cfdg cfm cil cobol cpp
cpp-qt csharp css d delphi diff dos dot eiffel email fortran
freebasic genero gettext glsl gml gnuplot groovy haskell hq9plus
html4strict idl ini inno intercal io java java5 javascript kixtart
klonec klonecpp latex lisp lolcode lotusformulas lotusscript
lscript lua m68k make matlab mirc modula3 mpasm mxml mysql nsis
objc ocaml ocaml-brief oobas oracle11 oracle8 pascal per perl php
php-brief pic16 pixelbender plsql povray powershell progress
prolog providex python qbasic r rails reg robots ruby sas scala
scheme scilab sdlbasic smalltalk smarty sql tcl teraterm text
thinbasic tsql typoscript vb vbnet verilog vhdl vim visualfoxpro
visualprolog whitespace winbatch xml xorg_conf xpp z80`,
         langs = split(substitute(ltext,"\n"," "))

function fix_tags(string text)
    for i=1 to length(langs) do
        string lang = langs[i],
               openl = sprintf("<%s>",{lang}),
               openc = sprintf("<code %s>",{lang}),
               lopen = sprintf("<lang %s>",{lang}),
               closl = sprintf("</%s>",{lang}),
               closc = sprintf("</%s>",{"code"}),
               lclos = sprintf("</%s>",{"lang"})
        text = substitute_all(text,{openl,openc,closl,closc},
                                   {lopen,lopen,lclos,lclos})
    end for
    return text
end function

constant test = """
lorem ipsum <c>some c code</c>dolor sit amet, <csharp>some
csharp code</csharp> consectetur adipisicing elit, <code r>
some r code </code>sed do eiusmod tempor incididunt
"""
puts(1,fix_tags(test))
```

{{out}}

```txt

lorem ipsum
```c>some c code</lang
dolor sit amet,
```c#
some
csharp code
```
 consectetur adipisicing elit,
```r

some r code
```
sed do eiusmod tempor incididunt

```




## PicoLisp


```PicoLisp
#!/usr/bin/picolisp /usr/lib/picolisp/lib.l

(let Lang '("ada" "awk" "c" "forth" "prolog" "python" "z80")
   (in NIL
      (while (echo "<")
         (let S (till ">" T)
            (cond
               ((pre? "code " S) (prin "<lang" (cddddr (chop S))))
               ((member S Lang) (prin "<lang " S))
               ((= S "/code") (prin "</lang"))
               ((and (pre? "/" S) (member (pack (cdr (chop S))) Lang))
                  (prin "</lang") )
               (T (prin "<" S)) ) ) ) ) )
(bye)
```



## PureBasic


```PureBasic
If Not OpenConsole()
  End
ElseIf CountProgramParameters() <> 2
  PrintN("Usage: "+GetFilePart(ProgramFilename())+" InFile OutFile")
  End
EndIf

Define Infile$ =ProgramParameter(), Outfile$=ProgramParameter()
If ReadFile(0,Infile$)
  NewList Out$()
  Define line$, part$, new$, pos1, pos2
  While Not Eof(0)
    line$=ReadString(0): pos2=0
    Repeat
      pos1=FindString(line$,"<",pos2)
      pos2=FindString(line$,">",pos1)
      If pos1 And pos2
        part$=Mid(line$,pos1+1,pos2-pos1-1)
        If Mid(part$,1,1)="/"
          new$="<"+"/lang>" ; Line split to avoid problem forum coding
        ElseIf Mid(part$,1,5)="code "
          new$="<lang "+Mid(part$,6)+">"
        Else
          new$="<lang "+part$+">"
        EndIf
        line$=ReplaceString(line$,"<"+part$+">",new$)
      Else
        Break
      EndIf
    ForEver
    AddElement(Out$()): Out$()=line$
  Wend
  CloseFile(0)
  If CreateFile(1, Outfile$)
    ForEach Out$()
      WriteStringN(1,Out$())
    Next
    CloseFile(1)
  EndIf
EndIf
```



## Python



```python
# coding: utf-8

import sys
import re

langs = ['ada', 'cpp-qt', 'pascal', 'lscript', 'z80', 'visualprolog',
'html4strict', 'cil', 'objc', 'asm', 'progress', 'teraterm', 'hq9plus',
'genero', 'tsql', 'email', 'pic16', 'tcl', 'apt_sources', 'io', 'apache',
'vhdl', 'avisynth', 'winbatch', 'vbnet', 'ini', 'scilab', 'ocaml-brief',
'sas', 'actionscript3', 'qbasic', 'perl', 'bnf', 'cobol', 'powershell',
'php', 'kixtart', 'visualfoxpro', 'mirc', 'make', 'javascript', 'cpp',
'sdlbasic', 'cadlisp', 'php-brief', 'rails', 'verilog', 'xml', 'csharp',
'actionscript', 'nsis', 'bash', 'typoscript', 'freebasic', 'dot',
'applescript', 'haskell', 'dos', 'oracle8', 'cfdg', 'glsl', 'lotusscript',
'mpasm', 'latex', 'sql', 'klonec', 'ruby', 'ocaml', 'smarty', 'python',
'oracle11', 'caddcl', 'robots', 'groovy', 'smalltalk', 'diff', 'fortran',
'cfm', 'lua', 'modula3', 'vb', 'autoit', 'java', 'text', 'scala',
'lotusformulas', 'pixelbender', 'reg', '_div', 'whitespace', 'providex',
'asp', 'css', 'lolcode', 'lisp', 'inno', 'mysql', 'plsql', 'matlab',
'oobas', 'vim', 'delphi', 'xorg_conf', 'gml', 'prolog', 'bf', 'per',
'scheme', 'mxml', 'd', 'basic4gl', 'm68k', 'gnuplot', 'idl', 'abap',
'intercal', 'c_mac', 'thinbasic', 'java5', 'xpp', 'boo', 'klonecpp',
'blitzbasic', 'eiffel', 'povray', 'c', 'gettext']

slang = '/lang'
code='code'

text = sys.stdin.read()

for i in langs:
    text = text.replace("<%s>" % i,"<lang %s>" % i)
    text = text.replace("</%s>" % i, "<%s>" % slang)

text = re.sub("(?s)<%s (.+?)>(.*?)</%s>"%(code,code), r"
```\1
\2<%s>" % slang, text)

sys.stdout.write(text)

```



## R

Note that the instances of ##### are to stop the wiki getting confused.  Please remove them before running the code.

```R

fixtags <- function(page)
{
   langs <- c("c", "c-sharp", "r")   # a complete list is required, obviously
   langs <- paste(langs, collapse="|")
   page <- gsub(paste("<(", langs, ")>", sep=""), "
```\\1
", page)
   page <- gsub(paste("</(", langs, ")>", sep=""), "</#####lang>", page)
   page <- gsub(paste("<code(", langs, ")>", sep=""), "
```\\1
", page)
   page <- gsub(paste("</code>", sep=""), "</#####lang>", page)
   page
}

page <- "lorem ipsum <c>some c code</c>dolor sit amet,<c-sharp>some c-sharp code</c-sharp>
consectetur adipisicing elit,<code r>some r code</code>sed do eiusmod tempor incididunt"
fixtags(page)

```



## Racket



```racket

#lang racket

(define lang-names '("X" "Y" "Z"))

(define rx
  (regexp (string-join lang-names "|"
                       #:before-first "<((/?(?:code)?)(?:( )?("
                       #:after-last "))?)>")))

(let loop () ; does all in a single scan
  (define m (regexp-match rx (current-input-port) 0 #f (current-output-port)))
  (when m
    (define-values [all pfx space lang] (apply values (cdr m)))
    (printf "<~a>"
      (cond [(not lang) (if (equal? pfx #"/code") #"/lang" all)]
            [space (if (equal? pfx #"code") (bytes-append #"lang " lang) all)]
            [(equal? pfx #"") (bytes-append #"lang " lang)]
            [(equal? pfx #"/") #"/lang"]
            [else all]))
    (loop)))

```



## REXX


```rexx
/*REXX program  fixes (changes)  depreciated  HTML  code tags  with  newer tags.        */
@="<";  old.=;        old.1 = @'%s>'         ;     new.1 = @"lang %s>"
                      old.2 = @'/%s>'        ;     new.2 = @"/lang>"
                      old.3 = @'code %s>'    ;     new.3 = @"lang %s>"
                      old.4 = @'/code>'      ;     new.4 = @"/lang>"

iFID = 'Wikisource.txt'                          /*the  Input File  IDentifier.         */
oFID = 'converted.txt'                           /*the Output   "      "                */

  do  while lines(iFID)\==0                      /*keep reading the file until finished.*/
  $=linein(iFID)                                 /*read a record from the input file.   */
                      do j=1  while old.j \== '' /*change old ──► new  until  finished. */
                      $=changestr(old.j,$,new.j) /*let REXX do the heavy lifting.       */
                      end   /*j*/
  call lineout oFID,$                            /*write re-formatted record to output. */
  end   /*while*/                                /*stick a fork in it,  we're all done. */
```

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ───►   [[CHANGESTR.REX]].





## Ruby


```ruby
# get all stdin in one string
#text = $stdin.read
# for testing, use
text = DATA.read
slash_lang = '/lang'
langs = %w(foo bar baz) # actual list of languages declared here
for lang in langs
  text.gsub!(Regexp.new("<(#{lang})>")) {"<lang #$1>"}
  text.gsub!(Regexp.new("</#{lang}>"), "<#{slash_lang}>")
end
text.gsub!(/<code (.*?)>/, '
```\1
')
text.gsub!(/<\/code>/, "<#{slash_lang}>")
print text

__END__
Lorem ipsum <code foo>saepe audire</code> elaboraret ne quo, id equidem
atomorum inciderint usu. <foo>In sit inermis deleniti percipit</foo>,
ius ex tale civibus omittam. <barf>Vix ut doctus cetero invenire</barf>, his eu
altera electram. Tota adhuc altera te sea, <code bar>soluta appetere ut mel</bar>.
Quo quis graecis vivendo te, <baz>posse nullam lobortis ex usu</code>. Eam volumus perpetua
constituto id, mea an omittam fierent vituperatoribus.
```



```txt
Lorem ipsum
```foo>saepe audire</lang
 elaboraret ne quo, id equidem
atomorum inciderint usu.
```foo>In sit inermis deleniti percipit</lang
,
ius ex tale civibus omittam. <barf>Vix ut doctus cetero invenire</barf>, his eu
altera electram. Tota adhuc altera te sea,
```bar>soluta appetere ut mel</lang
.
Quo quis graecis vivendo te,
```baz>posse nullam lobortis ex usu</lang
. Eam volumus perpetua
constituto id, mea an omittam fierent vituperatoribus.
```



## Scala


### Single pass converter

Tackles e.g. also multiple whitespaces.
{{Out}}
Experience it running in your browser at [https://scastie.scala-lang.org/5U6vqsOaTi6AU5FcqfA2lA Scastie (remote JVM)].

```Scala
object FixCodeTags extends App {
  val rx = // See for regex explanation: https://regex101.com/r/N8X4x7/3/
           // Flags ignore case, dot matching line breaks, unicode support
    s"(?is)<(?:(?:code\\s+)?(${langs.mkString("|")}))>(.+?|)<\\/(?:code|\\1)>".r

  def langs = // Real list of langs goes here
    Seq("bar", "baz", "foo", "Scala", "உயிர்/Uyir", "Müller")

  def markDown =
    """Lorem ipsum <Code foo>saepe audire</code> elaboraret ne quo, id equidem
      |atomorum inciderint usu. <foo>In sit inermis deleniti percipit</foo>, ius
      |ex tale civibus omittam. <barf>Vix ut doctus cetero invenire</barf>, his eu
      |altera electram. Tota adhuc altera te sea, <code bar>soluta appetere ut mel
      |</bar>. Quo quis graecis vivendo te, <baz>posse nullam lobortis ex usu</code>.
      |Eam volumus perpetua constituto id, mea an omittam fierent vituperatoribus.
      |Empty element: <Müller></Müller><scaLa></Scala><உயிர்/Uyir></உயிர்/Uyir>""".stripMargin

  println(rx.replaceAllIn(markDown, _ match {
    case rx(langName, langCode) => s"<lang ${langName.capitalize}>${langCode}<${"/lan"}g>"
  })) // ${"/lan"}g is the <noWiki> escape.

}
```


## Sidef


```ruby
var langs = %w(ada cpp-qt pascal lscript z80 visualprolog
html4strict cil objc asm progress teraterm hq9plus genero tsql
email pic16 tcl apt_sources io apache vhdl avisynth winbatch
vbnet ini scilab ocaml-brief sas actionscript3 qbasic perl bnf
cobol powershell php kixtart visualfoxpro mirc make javascript
cpp sdlbasic cadlisp php-brief rails verilog xml csharp
actionscript nsis bash typoscript freebasic dot applescript
haskell dos oracle8 cfdg glsl lotusscript mpasm latex sql klonec
ruby ocaml smarty python oracle11 caddcl robots groovy smalltalk
diff fortran cfm lua modula3 vb autoit java text scala
lotusformulas pixelbender reg _div whitespace providex asp css
lolcode lisp inno mysql plsql matlab oobas vim delphi xorg_conf
gml prolog bf per scheme mxml d basic4gl m68k gnuplot idl abap
intercal c_mac thinbasic java5 xpp boo klonecpp blitzbasic eiffel
povray c gettext).join('|');

var text = ARGF.slurp;

text.gsub!(Regex.new('<(' + langs + ')>'), {|s1| "<lang #{s1}>" });
text.gsub!(Regex.new('</(' + langs + ')>'), "</" + "lang>");
text.gsub!(
    Regex.new('<code\h+(' + langs + ')>(.*?)</code>', 's'),
    {|s1,s2| "<lang #{s1}>#{s2}</" + "lang>"}
);

print text;
```



## Tcl


```tcl
set langs {
    ada cpp-qt pascal lscript z80 visualprolog html4strict cil objc asm progress teraterm
    hq9plus genero tsql email pic16 tcl apt_sources io apache vhdl avisynth winbatch vbnet
    ini scilab ocaml-brief sas actionscript3 qbasic perl bnf cobol powershell php kixtart
    visualfoxpro mirc make javascript cpp sdlbasic cadlisp php-brief rails verilog xml
    csharp actionscript nsis bash typoscript freebasic dot applescript haskell dos oracle8
    cfdg glsl lotusscript mpasm latex sql klonec ruby ocaml smarty python oracle11 caddcl
    robots groovy smalltalk diff fortran cfm lua modula3 vb autoit java text scala lotusformulas
    pixelbender reg _div whitespace providex asp css lolcode lisp inno mysql plsql matlab
    oobas vim delphi xorg_conf gml prolog bf per scheme mxml d basic4gl m68k gnuplot idl
    abap intercal c_mac thinbasic java5 xpp boo klonecpp blitzbasic eiffel povray c gettext
}

set text [read stdin]
set slang /lang
foreach lang $langs {
    set text [regsub -all "<$lang>" $text "<lang $lang>"]
    set text [regsub -all "</$lang>" $text "<$slang>"]
}
set text [regsub -all "<code (.+?)>(.+?)</code>" $text "
```\\1
\\2<$slang>"]
```

Alternatively, for foreach loop may be replaced with:

```tcl
set text [regexp -all "<([join $langs |])>" $text {
```\1
}]
set text [regexp -all "</(?:[join $langs |])>" $text "<$slang>"]
```


This task does not require regular expressions at all:

```tcl>set replacements [list </code
 <$slang>]
foreach lang $langs {
    lappend replacements "<$lang>" "<lang $lang>"
    lappend replacements "</$lang>" "<$slang>"
    lappend replacements "<code $lang>" "<lang $lang>"
}
set text [string map $replacements $text]
```



## zkl


```zkl
fcn replace(data,src,dstpat){
   re,n,buf:=RegExp(src),0,Data();
   while(re.search(data,True,n)){
      matched:=re.matched;	// L(L(12,3),"c")
      data[matched[0].xplode()]=re.sub(data,dstpat,buf);  // "\1" --> "c"
      n=matched[0].sum(0);  // move past change
   }
}
data:=File.stdin.read();
foreach src,dst in (T(
     T(0'|<(\w+)>|,     0'|
```\1
|), T(0'|</(\w+)>|,"</" "lang>"),
     T(0'|<code (\w+)>|,0'|
```\1
|) )){
   replace(data,src,dst)
}
print(data.text);
```

Note: the "</" "lang>" to keep /lang the wiki from getting confused (it is string concatenation).
{{out}}

```txt

$ cat wikisource.txt
lorem ipsum <c>some c code</c>dolor sit amet, <csharp>some
csharp code</csharp> consectetur adipisicing elit, <code r>
   some r code </code>sed do eiusmod tempor incididunt

$ zkl bbb < wikisource.txt
lorem ipsum

```c
some c code
```
dolor sit amet,
```c#
some
csharp code
```
 consectetur adipisicing elit,
```r

   some r code
```
sed do eiusmod tempor incididunt

```



{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|Maxima}}
{{omit from|Openscad}}
{{omit from|TPP}}
