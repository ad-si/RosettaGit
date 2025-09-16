+++
title = "Text to HTML"
description = ""
date = 2019-01-12T10:35:40Z
aliases = []
[extra]
id = 11140
[taxonomies]
categories = []
tags = []
+++

{{Draft task|Text processing}}
When developing a Website it is occasionally necessary to handle text that is received without formatting, and present it in a pleasing manner. to achieve this the text needs to be converted to HTML.

Write a converter from plain text to HTML.

The plain text has no formatting information.

It may have centered headlines, numbered sections, paragraphs, lists, and URIs. It could even have tables.

Simple converters restrict themselves at identifying paragraphs, but i believe more can be done if the text is analyzed.

You are not requested to copy the algorithm from the existing solutions but use whatever faculties available in your language to best solve the problem.

The only requirement is to ensure that the result is valid xhtml.


## Go

This isn't very sophisticated but does a few things in a simple-minded way.

```go
package main

import (
    "fmt"
    "html"
    "regexp"
    "strings"
)

var t = `     Sample Text

This is an example of converting plain text to HTML which demonstrates extracting a title and escaping certain characters within bulleted and numbered lists.

* This is a bulleted list with a less than sign (<)

* And this is its second line with a greater than sign (>)

A 'normal' paragraph between the lists.

1. This is a numbered list with an ampersand (&)

2. "Second line" in double quotes

3. 'Third line' in single quotes

That's all folks.`

func main() {
    p := regexp.MustCompile(`\n\s*(\n\s*)+`)
    ul := regexp.MustCompile(`^\*`)
    ol := regexp.MustCompile(`^\d\.`)
    t = html.EscapeString(t) // escape <, >, &, " and '
    paras := p.Split(t, -1)

    // Assume if first character of first paragraph is white-space
    // then it's probably a document title.
    firstChar := paras[0][0]
    title := "Untitled"
    k := 0
    if firstChar == ' ' || firstChar == '\t' {
        title = strings.TrimSpace(paras[0])
        k = 1
    }
    fmt.Println("<html>")
    fmt.Printf("<head><title>%s</title></head>\n", title)
    fmt.Println("<body>")

    blist := false
    nlist := false
    for _, para := range paras[k:] {
        para2 := strings.TrimSpace(para)

        if ul.MatchString(para2) {
            if !blist {
                blist = true
                fmt.Println("<ul>")
            }
            para2 = strings.TrimSpace(para2[1:])
            fmt.Printf("  <li>%s</li>\n", para2)
            continue
        } else if blist {
            blist = false
            fmt.Println("</ul>")
        }

        if ol.MatchString(para2) {
            if !nlist {
                nlist = true
                fmt.Println("<ol>")
            }
            para2 = strings.TrimSpace(para2[2:])
            fmt.Printf("  <li>%s</li>\n", para2)
            continue
        } else if nlist {
            nlist = false
            fmt.Println("</ol>")
        }

        if !blist && !nlist {
            fmt.Printf("<p>%s</p>\n", para2)
        }
    }
    if blist {
        fmt.Println("</ul>")
    }
    if nlist {
        fmt.Println("</ol>")
    }
    fmt.Println("</body>")
    fmt.Println("</html>")
}
```


{{out}}

```html
<html

<head><title>Sample Text</title></head>
<body>
<p>This is an example of converting plain text to HTML which demonstrates extracting a title and escaping certain characters within bulleted and numbered lists.</p>
<ul>
  <li>This is a bulleted list with a less than sign (&lt;)</li>
  <li>And this is its second line with a greater than sign (&gt;)</li>
</ul>
<p>A &#39;normal&#39; paragraph between the lists.</p>
<ol>
  <li>This is a numbered list with an ampersand (&amp;)</li>
  <li>&#34;Second line&#34; in double quotes</li>
  <li>&#39;Third line&#39; in single quotes</li>
</ol>
<p>That&#39;s all folks.</p>
</body>
</html>
```



## Phix

The best thing to do here is to keep it utterly trivial.

```Phix
constant {hchars,hsubs} = columnize({{"&","&amp;"},
                                     {"<","&lt;"},
                                     {">","&gt;"},
                                     {"\"","&quot;"},
                                     {"\'","&apos;"}})

constant fmt = """
<html>
<head><title>%s</title></head>
<body>

```txt

%s

```

</body>
</html>
"""

function text_to_html_page(string title, text)
    title = substitute_all(title,hchars,hsubs)
    text = substitute_all(text,hchars,hsubs)
    return sprintf(fmt,{title,text})
--  return substitute_all(sprintf(fmt,{title,text}),hchars,hsubs)
end function

constant text = """
  This is
  a paragraph

      a block of
      code

  * A one-bullet list
    > With quoted text
    >
    >     and code
"""

puts(1,text_to_html_page("my title",text))
```

{{out}}
The last line of text_to_html() (as commented out) was used to generate the
sanitised version of the output, as needed for inclusion on this page.

```txt

&lt;html&gt;
&lt;head&gt;&lt;title&gt;my title&lt;/title&gt;&lt;/head&gt;
&lt;body&gt;
&lt;pre&gt;
  This is
  a paragraph

      a block of
      code

  * A one-bullet list
    &amp;gt; With quoted text
    &amp;gt;
    &amp;gt;     and code

&lt;/pre&gt;
&lt;/body&gt;
&lt;/html&gt;

```



## Pike

algorithm:
* split by line
* find average line length to identify centered lines
* find isolated lines to identify section headings
* find URIs
* identify section numbering
* identify bullet and numbered lists
* identify paragraphs
* identify indented lines
* if possible identify tables

to ensure valid xhtml create a nested structure:
* create an xml node
* add elements to node
* add lines to element if appropriate

this implementation is still incomplete.

```Pike
// function to calculate the average line length (not used yet below)
int linelength(array lines)
{
    array sizes = sizeof(lines[*])-({0});
    sizes = sort(sizes);

    // only consider the larger half of lines minus the top 5%
    array larger = sizes[sizeof(sizes)/2..sizeof(sizes)-sizeof(sizes)/20];

    int averagelarger = `+(@larger)/sizeof(larger);
    return averagelarger;
}

array mark_up(array lines)
{
    array markup = ({});

    // find special lines
    foreach(lines; int index; string line)
    {
        string strippedline = String.trim_whites(line);
        if (sizeof(strippedline))
        {
            string firstchar = strippedline[0..0];
            int pos = search(line, firstchar);

            if (lines[index-1]-" "-"\t" =="" && lines[index+1]-" "-"\t" =="")
                markup +=({ ({ "heading", strippedline, pos }) });
            else if (firstchar == "*")
                markup += ({ ({ "bullet", strippedline, pos }) });
            else if ( (<"0","1","2","3","4","5","6","7","8","9">)[firstchar] )
                markup += ({ ({ "number", strippedline, pos }) });
            else if (pos > 0)
                markup += ({ ({ "indent", strippedline, pos }) });
            else
                markup += ({ ({ "regular", strippedline, pos }) });
        }
        else markup += ({ ({ "empty" }) });
    }

    foreach(markup; int index; array line)
    {
        if (index > 0 && index < sizeof(markup)-1 )
        {
            if (line[0] == "regular" && markup[index-1][0] != "regular" && markup[index+1][0] != "regular")
                line[0] = "heading";
        }
    }

    //find paragraphs
    foreach(markup; int index; array line)
    {
        if (index > 0 && index < sizeof(markup)-1 )
        {
            if (line[0] == "empty" && markup[index-1][0] == "regular" && markup[index+1][0] == "regular")
                line[0] = "new paragraph";
            else if (line[0] == "empty" && markup[index-1][0] == "regular" && markup[index+1][0] != "regular")
                line[0] = "end paragraph";
            else if (line[0] == "empty" && markup[index-1][0] != "regular" && markup[index+1][0] == "regular")
                line[0] = "begin paragraph";
        }
    }
    return markup;
}

object make_tree(array markup)
{
    object root = Parser.XML.Tree.SimpleRootNode();
    object newline = Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_TEXT, "", ([]), "\n");
    array current = ({ Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_ELEMENT, "div", ([]), "") });
    root->add_child(current[-1]);

    foreach (markup; int index; array line)
    {
        switch(line[0])
        {
            case "heading":
                      current[-1]->add_child(newline);
                      object h = Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_ELEMENT, "h3", ([]), "");
                      h->add_child(Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_TEXT, "", ([]), line[1]));
                      current[-1]->add_child(h);
                      current[-1]->add_child(newline);
                  break;
            case "bullet":
            case "number":
                      if (current[-1]->get_tag_name() == "li")
                          current = Array.pop(current)[1];
                      current[-1]->add_child(newline);
                      object li = Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_ELEMENT, "li", ([]), "");
                      li->add_child(Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_TEXT, "", ([]), line[1]));
                      current[-1]->add_child(li);
                      current = Array.push(current, li);
                  break;
            case "indent":
                      if (markup[index-1][0] != "bullet" && markup[index-1][0] != "number")
                          current = Array.pop(current)[1];
                      current[-1]->add_child(Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_TEXT, "", ([]), line[1]));
                  break;
            case "new paragraph":
                      current = Array.pop(current)[1];
                      current[-1]->add_child(newline);
            case "begin paragraph":
                      object p = Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_ELEMENT, "p", ([]), "");
                      current[-1]->add_child(p);
                      current = Array.push(current, p);
                 break;
            case "end paragraph":
                      current = Array.pop(current)[1];
                      current[-1]->add_child(newline);
                 break;
            case "regular":
                      current[-1]->add_child(Parser.XML.Tree.SimpleNode(Parser.XML.Tree.XML_TEXT, "", ([]), line[1]));
            case "empty":
                  break;
        }
    }
    return root;
}
```



## Racket


This task seems like it's very under-defined, but the discussion seems to be headed towards a simple markdown specification.
I therefore do this with a small interface to [https://github.com/jgm/cmark cmark] to render [http://commonmark.org/ commonmark] text.

(Note that this is not some cooked code, it's coming from code that I'm using to render class notes, and hopefully it will be useful to have such an example here.
It certainly seems to me as a useful thing compared to some half-baked not-really-markdown-or-anything implementation.)


```racket

#lang at-exp racket

(require ffi/unsafe ffi/unsafe/define)

(define-ffi-definer defcmark (ffi-lib "libcmark"))

(define _cmark_opts
  (_bitmask '(sourcepos = 1 hardbreaks = 2 normalize = 4 smart = 8)))
(define-cpointer-type _node)
(defcmark cmark_markdown_to_html
  (_fun [bs : _bytes] [_int = (bytes-length bs)] _cmark_opts
        -> [r : _bytes] -> (begin0 (bytes->string/utf-8 r) (free r))))

(define (cmark-markdown-to-html #:options [opts '(normalize smart)] . text)
    (cmark_markdown_to_html (string->bytes/utf-8 (string-append* text)) opts))

(display @cmark-markdown-to-html{
  This is
  a paragraph

      a block of
      code

  * A one-bullet list
    > With quoted text
    >
    >     and code
})

```


{{out}}

```txt

&lt;p&gt;This is
a paragraph&lt;/p&gt;
&lt;pre&gt;&lt;code&gt;a block of
code
&lt;/code&gt;&lt;/pre&gt;
&lt;ul&gt;
&lt;li&gt;A one-bullet list
&lt;blockquote&gt;
&lt;p&gt;With quoted text&lt;/p&gt;
&lt;pre&gt;&lt;code&gt;and code
&lt;/code&gt;&lt;/pre&gt;
&lt;/blockquote&gt;
&lt;/li&gt;
&lt;/ul&gt;

```



## Tcl

This renderer doesn't do all that much. Indeed, it deliberately avoids doing all the complexity that is possible; instead it seeks to just provide the minimum that could possibly be useful to someone who is doing very simple text pages.

```tcl
package require Tcl 8.5

proc splitParagraphs {text} {
    split [regsub -all {\n\s*(\n\s*)+} [string trim $text] \u0000] "\u0000"
}
proc determineParagraph {para} {
    set para [regsub -all {\s*\n\s*} $para " "]
    switch -regexp -- $para {
	{^\s*\*+\s} {
	    return [list ul [string trimleft $para " \t*"]]
	}
	{^\s*\d+\.\s} {
	    set para [string trimleft $para " \t\n0123456789"]
	    set para [string range $para 1 end]
	    return [list ol [string trimleft $para " \t"]]
	}
	{^#+\s} {
	    return [list heading [string trimleft $para " \t#"]]
	}
    }
    return [list normal $para]
}
proc markupParagraphContent {para} {
    set para [string map {& &amp; < &lt; > &gt;} $para]
    regsub -all {_([\w&;]+)_} $para {<i>\1</i>} para
    regsub -all {\*([\w&;]+)\*} $para {<b>\1</b>} para
    regsub -all {`([\w&;]+)`} $para {<tt>\1</tt>} para
    return $para
}

proc markupText {title text} {
    set title [string map {& &amp; < &lt; > &gt;} $title]
    set result "<html>"
    append result "<head><title>" $title "</title>\n</head>"
    append result "<body>" "<h1>$title</h1>\n"
    set state normal
    foreach para [splitParagraphs $text] {
	lassign [determineParagraph $para] type para
	set para [markupParagraphContent $para]
	switch $state,$type {
	    normal,normal {append result "<p>" $para "</p>\n"}
	    normal,heading {
		append result "<h2>" $para "</h2>\n"
		set type normal
	    }
	    normal,ol {append result "<ol>" "<li>" $para "</li>\n"}
	    normal,ul {append result "<ul>" "<li>" $para "</li>\n"}

	    ul,normal {append result "</ul>" "<p>" $para "</p>\n"}
	    ul,heading {
		append result "</ul>" "<h2>" $para "</h2>\n"
		set type normal
	    }
	    ul,ol {append result "</ul>" "<ol>" "<li>" $para "</li>\n"}
	    ul,ul {append result "<li>" $para "</li>\n"}

	    ol,normal {append result "</ol>" "<p>" $para "</p>\n"}
	    ol,heading {
		append result "</ol>" "<h2>" $para "</h2>\n"
		set type normal
	    }
	    ol,ol {append result "<li>" $para "</li>\n"}
	    ol,ul {append result "</ol>" "<ul>" "<li>" $para "</li>\n"}
	}
	set state $type
    }
    if {$state ne "normal"} {
	append result "</$state>"
    }
    return [append result "</body></html>"]
}
```

Here's an example of how it would be used.

```tcl
set sample "
This is an example of how a pseudo-markdown-ish formatting scheme could
work. It's really much simpler than markdown, but does support a few things.

# Block paragraph types

* This is a bulleted list

* And this is the second item in it

1. Here's a numbered list

2. Second item

3. Third item

# Inline formatting types

The formatter can render text with _italics_, *bold* and in a `typewriter`
font. It also does the right thing with <angle brackets> and &amp;ersands,
but relies on the encoding of the characters to be conveyed separately."

puts [markupText "Sample" $sample]
```

{{out}}

```html
<html><head><title>Sample</title

</head><body><h1>Sample</h1>
<p>This is an example of how a pseudo-markdown-ish formatting scheme could work. It's really much simpler than markdown, but does support a few things.</p>
<h2>Block paragraph types</h2>
<ul><li>This is a bulleted list</li>
<li>And this is the second item in it</li>
</ul><ol><li>Here's a numbered list</li>
<li>Second item</li>
<li>Third item</li>
</ol><h2>Inline formatting types</h2>
<p>The formatter can render text with <i>italics</i>, <b>bold</b> and in a <tt>typewriter</tt> font. It also does the right thing with &lt;angle brackets&gt; and &amp;amp;ersands, but relies on the encoding of the characters to be conveyed separately.</p>
</body></html>
```

