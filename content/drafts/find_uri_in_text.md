+++
title = "Find URI in text"
description = ""
date = 2019-05-13T14:22:09Z
aliases = []
[extra]
id = 11134
[taxonomies]
categories = []
tags = []
+++

{{Draft task|Text processing}}
Write a function to search plain text for URIs or IRIs.

The function should return a list of URIs or IRIs found in the text.   

The definition of a URI is given in RFC 3986.
IRI is defined in RFC 3987.

For searching URIs in particular "Appendix C.  Delimiting a URI in Context" is noteworthy.

The abbreviation IRI isn't as well known as URI and the short description is that an IRI is just an alternate form of a URI that supports Internationalization and hence Unicode.  While many specifications support both forms this isn't universal.

Consider the following issues:
* <code>. , ; ' ? ( )</code> are legal characters in a URI, but they are often used in plain text as a delimiter.
* IRIs allow most (but not all) unicode characters.
* URIs can be something else besides http:// or https://

sample text:
 <nowiki>
 this URI contains an illegal character, parentheses and a misplaced full stop:
 http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (which is handled by http://mediawiki.org/).
 and another one just to confuse the parser: http://en.wikipedia.org/wiki/-)
 ")" is handled the wrong way by the mediawiki parser.
 ftp://domain.name/path(balanced_brackets)/foo.html
 ftp://domain.name/path(balanced_brackets)/ending.in.dot.
 ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
 leading junk ftp://domain.name/path/embedded?punct/uation.
 leading junk ftp://domain.name/dangling_close_paren)
 if you have other interesting URIs for testing, please add them here:
 </nowiki>

Regular expressions to solve the task are fine, but alternative approaches are welcome too. (otherwise, this task would degrade into 'finding and applying the best regular expression')

'''Extra Credit:''' implement the parser to match the IRI specification in RFC 3987.


## Go

{{trans|Kotlin}}
{{libheader|golang-pkg-pcre}}


The regexp package in the Go standard library is not fully compatible with PCRE and is unable to compile the regular expression used here. A third party library has therefore been used instead which is a PCRE shim for Go.

```go
package main

import (
    "fmt"
    "github.com/glenn-brown/golang-pkg-pcre/src/pkg/pcre"
)

var pattern = 
    "(*UTF)(*UCP)" +                    // Make \w unicode aware
    "[a-z][-a-z0-9+.]*:" +              // Scheme...
    "(?=[/\\w])" +                      // ... but not just the scheme
    "(?://[-\\w.@:]+)?" +               // Host
    "[-\\w.~/%!$&'()*+,;=]*" +          // Path
    "(?:\\?[-\\w.~%!$&'()*+,;=/?]*)?" + // Query
    "(?:\\#[-\\w.~%!$&'()*+,;=/?]*)?"   // Fragment

func main() {
    text := `
this URI contains an illegal character, parentheses and a misplaced full stop:
http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (which is handled by http://mediawiki.org/).
and another one just to confuse the parser: http://en.wikipedia.org/wiki/-)
")" is handled the wrong way by the mediawiki parser.
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
leading junk ftp://domain.name/path/embedded?punct/uation.
leading junk ftp://domain.name/dangling_close_paren)
if you have other interesting URIs for testing, please add them here:
http://www.example.org/foo.html#includes_fragment
http://www.example.org/foo.html#enthält_Unicode-Fragment
`
    descs := []string{"URIs:-", "IRIs:-"}
    patterns := []string{pattern[12:], pattern}
    for i := 0; i <= 1; i++ {
        fmt.Println(descs[i])
        re := pcre.MustCompile(patterns[i], 0)
        t := text
        for {
            se := re.FindIndex([]byte(t), 0)
            if se == nil {
                break
            }
            fmt.Println(t[se[0]:se[1]])
            t = t[se[1]:]
        }
        fmt.Println()
    }
}
```


{{out}}

```txt

URIs:-
http://en.wikipedia.org/wiki/Erich_K
http://mediawiki.org/).
http://en.wikipedia.org/wiki/-)
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path/embedded?punct/uation.
ftp://domain.name/dangling_close_paren)
http://www.example.org/foo.html#includes_fragment
http://www.example.org/foo.html#enth

IRIs:-
http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer).
http://mediawiki.org/).
http://en.wikipedia.org/wiki/-)
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path/embedded?punct/uation.
ftp://domain.name/dangling_close_paren)
http://www.example.org/foo.html#includes_fragment
http://www.example.org/foo.html#enthält_Unicode-Fragment

```


=={{header|Icon}} and {{header|Unicon}}==
This example follows RFC 3986 very closely (see Talk page for discussion).  For better IP parsing see [[Parse_an_IP_Address]].
This solution doesn't handle IRIs per RFC 3987.  Neither Icon nor Unicon natively support Unicode although ObjectIcon does.

Delimited examples of the form ''<URI>'' or ''"URI"'' will be correctly parse in any event.  Handling of other possibly ambiguous examples that include valid URI characters is done by the 'findURItext' and 'disambURI' procedures. All candidate URIs are returned since once information is removed it will be lost and may be difficult for a user to reconstruct.  This solution deals with all of the trailing character and balance considerations. 


```Icon
procedure main()
   every write(findURItext("this URI contains an illegal character, parentheses_
               and a misplaced full stop:\n_
               http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). _
               which is handled by http://mediawiki.org/).\n_
               and another one just to confuse the parser: _
               http://en.wikipedia.org/wiki/-)\n_
               \")\" is handled the wrong way by the mediawiki parser.\n_
               ftp://domain.name/path(balanced_brackets)/foo.html\n_
               ftp://domain.name/path(balanced_brackets)/ending.in.dot.\n_
               ftp://domain.name/path(unbalanced_brackets/ending.in.dot.\n_
               leading junk ftp://domain.name/path/embedded?punct/uation.\n_
               leading junk ftp://domain.name/dangling_close_paren)\n_
               if you have other interesting URIs for testing, please add them here:\n_
               blah (foo://domain.hld/))))"))
end

$define GENDELIM   ':/?#[]@'
$define SUBDELIM   '!$&()*+,;=\''
$define UNRESERVED &letters ++ &digits ++ '-._~'
$define RESERVED   GENDELIM++SUBDELIM
$define HEXDIGITS  '0123456789aAbBcCdDeEfF'

procedure findURItext(s)      #: generate all syntatically valid URI's from s
   local u,p
   s ? while tab(upto(&letters)) || (u := 2(p := &pos, URI())) do  { 
      suspend u                     # return parsed URI 
      every suspend disambURI(u,p)  # deal with text ambiguities, return many
      }
end

procedure disambURI(u,p)      #: generate disambiguous URIs from parsed
   local u2
   repeat  {
      if any('.,;?',u[-1]) then 
         suspend u := u[1:-1]             # remove trailing .,;? from URI
      else if u[-1] == "'" == &subject[p-:=1] then 
         suspend u := u[1:-1]             # remove trailing ' from 'URI'
      else if any('()',u[-1]) then   {    
         every u ? u2 := tab(bal())          
         if u ~==:= u2 then suspend u     # longest balanced URI wrt ()          
         }
      else break                          # done
      }       
end  
                  
procedure URI()               #: match longest URI at cursor
   static sc2
   initial sc2 := &letters ++ &digits ++ '+-.'                    # scheme 
   suspend (
      ( tab(any(&letters)) || (tab(many(sc2)) |="") || =":" ) ||  # scheme
      ( (="//" || authority() || arbsp("/",segment)) |            # heir ...
        (="/" || ( path_rootless() |="")) |
        path_rootless() |
        ="" 
      ) ||         
      ( ( ="?" || queryfrag() ) |="" ) ||                         # query
      ( ( ="#" || queryfrag() ) |="" )                            # fragment
      )
end

procedure queryfrag()         #: match a query or fragment
   static pc
   initial pc := UNRESERVED ++ SUBDELIM ++ ':@/?'
   suspend arbcp(pc,pctencode)   
end

procedure segment(n)          #: match a pchar segment
   static sc
   initial sc := UNRESERVED ++ SUBDELIM ++ ':@'
   suspend arbcp(sc,pctencode,n)
end

procedure segmentnc(n)        #: match a pchar--':' segment
   static sc
   initial sc := UNRESERVED ++ SUBDELIM ++ '@'
   suspend arbcp(sc,pctencode,n)
end

procedure path_rootless()     #: match a rootless path
   suspend segment(1) || arbsp("/",segment)
end

procedure authority()         #: match authority
   static uic,rnc
   initial {
      rnc := UNRESERVED ++ SUBDELIM    # regular name
      uic := rnc ++ ':'                # userinfo      
      }
   suspend  ( (arbcp(uic,pctencode) || ="@") |="")  ||      # userinfo
            ( IPsimple() | arbcp(rnc,pctencode) )   ||      # host
            ( (=":" || tab(many(&digits))) |="")
end  
      
procedure IPsimple()          #: match ip address (trickable )
   static i4c,i6c,ifc
   initial {
      i4c := &digits ++ '.'
      i6c := HEXDIGITS ++ '.:'
      ifc := UNRESERVED ++ SUBDELIM ++ ':'
      }
   suspend ( 
      ="[" || 
         (  tab(many(i6c)) |  
            ( ="v"||tab(any(HEXDIGITS))||="."||tab(any(ifc))||tab(many(ifc)) )
      ) || ="]" ) | tab(many(i4c))
end  

procedure arbcp(cs,pr,n)      #: match arbitrary numbers of (cset|proc,n)
   local p,i
   /n := 0                    # for 0* / 1*
   runerr(0 > n,205)
   p := &pos
   i := 0
   while tab(many(cs)) | pr() do i +:= 1
   if i >= n then suspend &subject[p:&pos]
   &pos := p                  # restore &pos
end

procedure arbsp(st,pr,n)      #: match arbitrary numbers of (string || proc,n)
   local p,i
   /n := 0                    # for 0* / 1*
   runerr(0 > n,205)
   p := &pos
   i := 0
   while =st || pr() do i +:= 1 
   if i >= n then suspend &subject[p:&pos]
   &pos := p                  # restore &pos
end

procedure pctencode()         #: match 1 % encoded single byte character
   suspend ="%" || tab(any(HEXDIGITS)) || tab(any(HEXDIGITS))
end
```


Output:
```txt
stop:
http://en.wikipedia.org/wiki/Erich_K
http://mediawiki.org/).
http://mediawiki.org/)
http://mediawiki.org/
parser:
http://en.wikipedia.org/wiki/-)
http://en.wikipedia.org/wiki/-
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(balanced_brackets)/ending.in.dot
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot
ftp://domain.name/path/embedded?punct/uation.
ftp://domain.name/path/embedded?punct/uation
ftp://domain.name/dangling_close_paren)
ftp://domain.name/dangling_close_paren
here:
foo://domain.hld/))))
foo://domain.hld/

```



## jq

{{works with|jq|with regex}}

The following uses essentially the same regular expression as is used in the [[#Tcl]] article (as of June 2015), and the results using the given input text are identical.  Note in particular that scheme-only strings such as "stop:" are not extracted.

```jq
# input: a JSON string
# output: a stream of URIs
# Each input string may contain more than one URI.
def findURIs:
    match( "
	[a-z][-a-z0-9+.]*:		# Scheme...
	(?=[/\\w])			# ... but not just the scheme
	(?://[-\\w.@:]+)?		# Host
	[-\\w.~/%!$&'()*+,;=]*		# Path
	(?:\\?[-\\w.~%!$&'()*+,;=/?]*)?	# Query
	(?:[#][-\\w.~%!$&'()*+,;=/?]*)?	# Fragment
 
     "; "gx")
    | .string ;

# Example: read in a file of arbitrary text and
# produce a stream of the URIs that are identified.
split("\n")[] | findURIs
```


{{out}}

```sh
$ jq -R -r -f Find_URI_in_text.jq Find_URI_in_text.txt
http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer).
http://mediawiki.org/).
http://en.wikipedia.org/wiki/-)
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path/embedded?punct/uation.
ftp://domain.name/dangling_close_paren)
```



## Kotlin

The regular expression used here is essentially the same as the one in the Tcl entry. However, the flag expression (?U) is needed to enable matching of Unicode characters. Without this only ASCII characters are matched.

```scala
// version 1.2.21

val pattern =
    "(?U)" +                              // Enable matching of non-ascii characters
    "[a-z][-a-z0-9+.]*:" +	          // Scheme...
    "(?=[/\\w])" +                        // ... but not just the scheme
    "(?://[-\\w.@:]+)?" +                 // Host
    "[-\\w.~/%!\$&'()*+,;=]*" +           // Path
    "(?:\\?[-\\w.~%!\$&'()*+,;=/?]*)?" +  // Query
    "(?:\\#[-\\w.~%!\$&'()*+,;=/?]*)?"    // Fragment

fun main(args: Array<String>) {
    val text = """
        |this URI contains an illegal character, parentheses and a misplaced full stop:
        |http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (which is handled by http://mediawiki.org/).
        |and another one just to confuse the parser: http://en.wikipedia.org/wiki/-)
        |")" is handled the wrong way by the mediawiki parser.
        |ftp://domain.name/path(balanced_brackets)/foo.html
        |ftp://domain.name/path(balanced_brackets)/ending.in.dot.
        |ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
        |leading junk ftp://domain.name/path/embedded?punct/uation.
        |leading junk ftp://domain.name/dangling_close_paren)
        |if you have other interesting URIs for testing, please add them here:
        |http://www.example.org/foo.html#includes_fragment
        |http://www.example.org/foo.html#enthält_Unicode-Fragment
    """.trimMargin()
    val patterns = listOf(pattern.drop(4), pattern)
    val descs = listOf("URIs:-", "IRIs:-")
    for (i in 0..1) {
        println(descs[i])
        val regex = Regex(patterns[i])
        val matches = regex.findAll(text)
        matches.forEach { println(it.value) }
        println()
    }
}
```


{{out}}

```txt

URIs:-
http://en.wikipedia.org/wiki/Erich_K
http://mediawiki.org/).
http://en.wikipedia.org/wiki/-)
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path/embedded?punct/uation.
ftp://domain.name/dangling_close_paren)
http://www.example.org/foo.html#includes_fragment
http://www.example.org/foo.html#enth

IRIs:-
http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer).
http://mediawiki.org/).
http://en.wikipedia.org/wiki/-)
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path/embedded?punct/uation.
ftp://domain.name/dangling_close_paren)
http://www.example.org/foo.html#includes_fragment
http://www.example.org/foo.html#enthält_Unicode-Fragment

```



## Objeck

Used a regex instead of writing a parser.

```objeck

﻿use RegEx;

class FindUri {
  function : Main(args : String[]) ~ Nil {
    text := "this URI contains an illegal character, parentheses and a misplaced full stop:
http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (which is handled by http://mediawiki.org/).
and another one just to confuse the parser: http://en.wikipedia.org/wiki/-)
\")\" is handled the wrong way by the mediawiki parser.
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
leading junk ftp://domain.name/path/embedded?punct/uation.
leading junk ftp://domain.name/dangling_close_paren)
if you have other interesting URIs for testing, please add them here:";

    found := RegEx->New("\\w*://(\\w|\\(|\\)|/|,|;|'|\\?|\\.)*")->Find(text);
    count := found->Size();
    "Found: {$count}"->PrintLine();
    each(i : found) {
      found->Get(i)->As(String)->PrintLine();
    };
  }
}
```



```txt

Count: 8
http://en.wikipedia.org/wiki/Erich_K
http://mediawiki.org/).
http://en.wikipedia.org/wiki/
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path/embedded?punct/uation.
ftp://domain.name/dangling_close_paren)

```



## Perl 6

This needs an installed URI distribution. {{works with|Rakudo|2018.03}}

```perl6
use v6;
use IETF::RFC_Grammar::URI;

say q:to/EOF/.match(/ <IETF::RFC_Grammar::URI::absolute-URI> /, :g).list.join("\n");
    this URI contains an illegal character, parentheses and a misplaced full stop:
    http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (which is handled by http://mediawiki.org/).
    and another one just to confuse the parser: http://en.wikipedia.org/wiki/-)
    ")" is handled the wrong way by the mediawiki parser.
    ftp://domain.name/path(balanced_brackets)/foo.html
    ftp://domain.name/path(balanced_brackets)/ending.in.dot.
    ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
    leading junk ftp://domain.name/path/embedded?punct/uation.
    leading junk ftp://domain.name/dangling_close_paren)
    EOF

say $/[*-1];
say "We matched $/[*-1], which is a $/[*-1].^name() at position $/[*-1].from() to $/[*-1].to()"

```


Like most of the solutions here it does not comply to IRI but only to URI:


```txt
stop:
http://en.wikipedia.org/wiki/Erich_K
http://mediawiki.org/).
parser:
http://en.wikipedia.org/wiki/-)
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path/embedded
ftp://domain.name/dangling_close_paren)
｢ftp://domain.name/dangling_close_paren)｣
 IETF::RFC_Grammar::URI::absolute_URI => ｢ftp://domain.name/dangling_close_paren)｣
  scheme => ｢ftp｣
We matched ftp://domain.name/dangling_close_paren), which is a Match, at position 554 to 593
```


The last lines show that we get Match objects back that we can query to get all kinds of information.
We even get the information what subrules matched, and since these are also Match objects we can obtain
their match position in the text.


## Phix

There is a scanForUrls() routine in demo\edita\src\easynclr.e you might be interested in which manages this (without regex) but it is a (very) long time since I wrote it and it is quite strongly coupled in with syntax colouring and other editor gubbins.


## PHP

Trivial example using PHP's built-in filter_var() function (which does not support IRIs).

```PHP
$tests = array(
    'this URI contains an illegal character, parentheses and a misplaced full stop:',
    'http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (which is handled by http://mediawiki.org/).',
    'and another one just to confuse the parser: http://en.wikipedia.org/wiki/-)',
    '")" is handled the wrong way by the mediawiki parser.',
    'ftp://domain.name/path(balanced_brackets)/foo.html',
    'ftp://domain.name/path(balanced_brackets)/ending.in.dot.',
    'ftp://domain.name/path(unbalanced_brackets/ending.in.dot.',
    'leading junk ftp://domain.name/path/embedded?punct/uation.',
    'leading junk ftp://domain.name/dangling_close_paren)',
    'if you have other interesting URIs for testing, please add them here:',
    'http://www.example.org/foo.html#includes_fragment',
    'http://www.example.org/foo.html#enthält_Unicode-Fragment',
    ' http://192.168.0.1/admin/?hackme=%%%%%%%%%true',
    'blah (foo://domain.hld/))))',
    'https://haxor.ur:4592/~mama/####&?foo'
);

foreach ( $tests as $test ) {
    foreach( explode( ' ', $test ) as $uri ) {
        if ( filter_var( $uri, FILTER_VALIDATE_URL ) )
            echo $uri, PHP_EOL;
    }
}

```

{{Out}}

```txt

http://mediawiki.org/).
http://en.wikipedia.org/wiki/-)
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path/embedded?punct/uation.
ftp://domain.name/dangling_close_paren)
http://www.example.org/foo.html#includes_fragment
http://192.168.0.1/admin/?hackme=%%%%%%%%%true
https://haxor.ur:4592/~mama/&####&?foo

```



## Pike


```Pike
string uritext = #"this URI contains an illegal character, parentheses and a misplaced full stop:
http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). 
which is handled by http://mediawiki.org/).
and another one just to confuse the parser: http://en.wikipedia.org/wiki/-)
\")\" is handled the wrong way by the mediawiki parser.
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
leading junk ftp://domain.name/path/embedded?punct/uation.
leading junk ftp://domain.name/dangling_close_paren)
if you have other interesting URIs for testing, please add them here:";

array find_uris(string uritext)
{
    array uris=({}); 
    int pos=0; 
    while((pos = search(uritext, ":", pos+1))>0)
    { 
        int prepos = sizeof(array_sscanf(reverse(uritext[pos-20..pos-1]), "%[a-zA-Z0-9+.-]%s")[0]); 
        int postpos = sizeof(array_sscanf(uritext[pos+1..], "%[^\n\r\t <>\"]%s")[0]); 

        if ((<'.',',','?','!',';'>)[uritext[pos+postpos]])
            postpos--;
        if (uritext[pos-prepos-1]=='(' && uritext[pos+postpos]==')')
            postpos--;
        if (uritext[pos-prepos-1]=='\'' && uritext[pos+postpos]=='\'')
            postpos--;  
        uris+= ({ uritext[pos-prepos..pos+postpos] });
    }
    return uris;
}

find_uris(uritext);
Result: ({ /* 11 elements */
            "stop:",
            "http://en.wikipedia.org/wiki/Erich_K\303\244stner_(camera_designer)",
            "http://mediawiki.org/)",
            "parser:",
            "http://en.wikipedia.org/wiki/-)",
            "ftp://domain.name/path(balanced_brackets)/foo.html",
            "ftp://domain.name/path(balanced_brackets)/ending.in.dot",
            "ftp://domain.name/path(unbalanced_brackets/ending.in.dot",
            "ftp://domain.name/path/embedded?punct/uation",
            "ftp://domain.name/dangling_close_paren)",
            "here:"
        })
```



## Racket


{{trans|Tcl}}


```racket
#lang racket

(define sample
  #<<EOS
this URI contains an illegal character, parentheses and a misplaced full stop:
http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (which is handled by http://mediawiki.org/).
and another one just to confuse the parser: http://en.wikipedia.org/wiki/-)
")" is handled the wrong way by the mediawiki parser.
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
leading junk ftp://domain.name/path/embedded?punct/uation.
leading junk ftp://domain.name/dangling_close_paren)
EOS
  )

(define uri-ere-bits
  '("[a-z][-a-z0-9+.]*:"              ; Scheme...
    "(?=[/\\w])"                      ; ... but not just the scheme
    "(?://[-\\w.@:]+)?"               ; Host
    "[-\\w.~/%!$&'()*+,;=]*"          ; Path
    "(?:\\?[-\\w.~%!$&'()*+,;=/?]*)?" ; Query
    "(?:[#][-\\w.~%!$&'()*+,;=/?]*)?" ; Fragment
    ))

(define uri-re (pregexp (apply string-append uri-ere-bits)))

(for-each (compose displayln ~s) (regexp-match* uri-re sample))
(regexp-match-positions* uri-re sample)

(module+ test
  ;; "ABNF for Syntax Specifications" http://tools.ietf.org/html/rfc2234
  ;; defines ALPHA as:
  ;;   ALPHA = %x41-5A / %x61-7A   ; A-Z / a-z
  (unless (= 228 (char->integer #\ä))
    (error "a-umlaut is not 228, and therefore might be an ALPHA")))
```


{{out}}

Tcl's \w matches non-ASCII alphabetic characters. We finish the Kaestner match after the K because a-umlaut is not an ASCII character.

Match positions differ from the [[#Tcl]] version because:
* sample does not start with a newline in racket (the here string handles that differently to Tcl braces)
* the cdr of the pairs is the index AFTER the last character of the match


```txt
"http://en.wikipedia.org/wiki/Erich_K"
"http://mediawiki.org/)."
"http://en.wikipedia.org/wiki/-)"
"ftp://domain.name/path(balanced_brackets)/foo.html"
"ftp://domain.name/path(balanced_brackets)/ending.in.dot."
"ftp://domain.name/path(unbalanced_brackets/ending.in.dot."
"ftp://domain.name/path/embedded?punct/uation."
"ftp://domain.name/dangling_close_paren)"
((79 . 115) (162 . 185) (230 . 261) (316 . 366) (367 . 423) (424 . 481) (495 . 540) (554 . 593))
```



## REXX


```rexx
/*REXX program scans a text (contained within REXX pgm) to extract URIs.*/
text='this URI contains an illegal character, parentheses and a misplaced full stop:',
     'http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (which is handled by http://mediawiki.org/).',
     'and another one just to confuse the parser: http://en.wikipedia.org/wiki/-)',
     '")" is handled the wrong way by the mediawiki parser.',
     'ftp://domain.name/path(balanced_brackets)/foo.html',
     'ftp://domain.name/path(balanced_brackets)/ending.in.dot.',
     'ftp://domain.name/path(unbalanced_brackets/ending.in.dot.',
     'leading junk ftp://domain.name/path/embedded?punct/uation.',
     'leading junk ftp://domain.name/dangling_close_paren)',
     'if you have other interesting URIs for testing, please add them here:'

@abc='abcdefghijklmnopqrstuvwxyz'; @abcs=@abc||translate(@abc)
@scheme=@abcs || 0123456789 || '+-.'
@unreserved=@abcs || 0123456789 || '-._~'
@reserved=@unreserved"/?#[]@!$&)(*+,;=\'"
t=space(text)' '                       /*variable  T  is a working copy.*/
#=0                                    /*count of URI's found so far.   */
                                       /*▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄*/
  do  while  t\=''                     /*scan text for multiple URIs.   */
  y=pos(':',t)                         /*locate a colon in the text body*/
  if y==0  then leave                  /*Colon found?   No, we're done. */
  if y==1  then do                     /*handle a bare colon by itself. */
                parse var t . t        /*ignore the bare colon (:).     */
                iterate                /*go & keep scanning for a colon.*/
                end                    /* [↑]  a rare special case.     */
  sr=reverse(left(t,y-1))              /*extract the scheme and reverse.*/
  se=verify(sr,@scheme)                /*locate the end of the scheme.  */
  t=substr(t,y+1)                      /*assign an adjusted new text.   */
  if se\==0  then sr=left(sr,se-1)     /*possibly crop the scheme name. */
  s=reverse(sr)                        /*reverse again to rectify name. */
  he=verify(t,@reserved)               /*locate the end of the hier-part*/
  s=s':'left(t,he-1)                   /*extract & append the hier-part.*/
  t=substr(t,he)                       /*assign an adjusted new text.   */
  #=#+1                                /*bump the URI counter.          */
  !.#=s                                /*assign the URI to an array.    */
  end   /*while t\='' */               /* [↑]  scan the text for URIs.  */
                                       /*▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀*/
 do k=1  for #;   say !.k;   end       /*stick a fork in it, we're done.*/
```

'''output'''

```txt

stop:
http://en.wikipedia.org/wiki/Erich_K
http://mediawiki.org/).
parser:
http://en.wikipedia.org/wiki/-)
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path/embedded?punct/uation.
ftp://domain.name/dangling_close_paren)
here:

```



## Ruby


```ruby

require  'uri'

str = 'this URI contains an illegal character, parentheses and a misplaced full stop:
http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (which is handled by http://mediawiki.org/).
and another one just to confuse the parser: http://en.wikipedia.org/wiki/-)
")" is handled the wrong way by the mediawiki parser.
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
leading junk ftp://domain.name/path/embedded?punct/uation.
leading junk ftp://domain.name/dangling_close_paren)
if you have other interesting URIs for testing, please add them here:'


puts URI.extract(str) 

puts "\nFiltered for HTTP and HTTPS:"
puts URI.extract(str, ["http", "https"])

puts "\nThis is the (extendible) list of supported schemes: #{URI.scheme_list.keys}"
```

{{Output}}

```txt

stop:
http://en.wikipedia.org/wiki/Erich_K
http://mediawiki.org/).
parser:
http://en.wikipedia.org/wiki/-)
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path/embedded?punct/uation.
ftp://domain.name/dangling_close_paren)
here:

Filtered for HTTP and HTTPS:
http://en.wikipedia.org/wiki/Erich_K
http://mediawiki.org/).
http://en.wikipedia.org/wiki/-)

This is the (extendible) list of supported schemes: ["FTP", "HTTP", "HTTPS", "LDAP", "LDAPS", "MAILTO"]

```



## Tcl

This uses regular expressions to do the matching. It doesn't match a URL without a scheme (too problematic in general text) and it requires more than ''just'' the scheme too, but apart from that it matches slightly too broad a range of strings (though not usually problematically much). Matches some IRIs correctly too, but does not tackle the <tt>&lt;bracketed&gt;</tt> form (especially not if it includes extra spaces).

```tcl
proc findURIs {text args} {
    # This is an ERE with embedded comments. Rare, but useful with something
    # this complex.
    set URI {(?x)
	[a-z][-a-z0-9+.]*:		# Scheme...
	(?=[/\w])			# ... but not just the scheme
	(?://[-\w.@:]+)?		# Host
	[-\w.~/%!$&'()*+,;=]*		# Path
	(?:\?[-\w.~%!$&'()*+,;=/?]*)?	# Query
	(?:[#][-\w.~%!$&'()*+,;=/?]*)?	# Fragment
    }
    regexp -inline -all {*}$args -- $URI $text
}
```

;Demonstrating<nowiki>:</nowiki>
Note that the last line of output is showing that we haven't just extracted the URI substrings, but can also get the match positions within the text.

```tcl
set sample {
this URI contains an illegal character, parentheses and a misplaced full stop:
http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (which is handled by http://mediawiki.org/).
and another one just to confuse the parser: http://en.wikipedia.org/wiki/-)
")" is handled the wrong way by the mediawiki parser.
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
leading junk ftp://domain.name/path/embedded?punct/uation.
leading junk ftp://domain.name/dangling_close_paren)
}

puts [join [findURIs $sample] \n]
puts [findURIs $sample -indices]
```

{{out}}

```txt

http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer).
http://mediawiki.org/).
http://en.wikipedia.org/wiki/-)
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
ftp://domain.name/path/embedded?punct/uation.
ftp://domain.name/dangling_close_paren)
{80 140} {163 185} {231 261} {317 366} {368 423} {425 481} {496 540} {555 593}

```



## TXR


```txr
@(define path (path))@\
  @(local x y)@\
  @(cases)@\
    (@(path x))@(path y)@(bind path `(@x)@y`)@\
  @(or)@\
    @{x /[.,;'!?][^ \t\f\v]/}@(path y)@(bind path `@x@y`)@\
  @(or)@\
    @{x /[^ .,;'!?()\t\f\v]/}@(path y)@(bind path `@x@y`)@\
  @(or)@\
    @(bind path "")@\
  @(end)@\
@(end)
@(define url (url))@\
  @(local proto domain path)@\
  @{proto /[A-Za-z]+/}://@{domain /[^ \/\t\f\v]+/}@\
  @(cases)/@(path path)@\
    @(bind url `@proto://@domain/@path`)@\
  @(or)@\
    @(bind url `@proto://@domain`)@\
  @(end)@\
@(end)
@(collect)
@  (all)
@line
@  (and)
@     (coll)@(url url)@(end)@(flatten url)
@  (end)
@(end)
@(output)
LINE 
    URLS
----------------------
@  (repeat)
@line
@    (repeat)
    @url
@    (end)
@  (end)
@(end)
```


Test file:

```txt
$ cat url-data 
Blah blah http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (Handled by http://mediawiki.org/).
Confuse the parser: http://en.wikipedia.org/wiki/-)
ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
leading junk ftp://domain.name/path/embedded?punct/uation.
leading junk ftp://domain.name/dangling_close_paren)
```


Run:


```txt
$ txr url.txr url-data 
LINE 
    URLS
----------------------
Blah blah http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer). (Handled by http://mediawiki.org/).
    http://en.wikipedia.org/wiki/Erich_Kästner_(camera_designer)
    http://mediawiki.org/
Confuse the parser: http://en.wikipedia.org/wiki/-)
    http://en.wikipedia.org/wiki/-
ftp://domain.name/path(balanced_brackets)/foo.html
    ftp://domain.name/path(balanced_brackets)/foo.html
ftp://domain.name/path(balanced_brackets)/ending.in.dot.
    ftp://domain.name/path(balanced_brackets)/ending.in.dot
ftp://domain.name/path(unbalanced_brackets/ending.in.dot.
    ftp://domain.name/path
leading junk ftp://domain.name/path/embedded?punct/uation.
    ftp://domain.name/path/embedded?punct/uation
leading junk ftp://domain.name/dangling_close_paren)
    ftp://domain.name/dangling_close_paren
```

