+++
title = "URL parser"
description = ""
date = 2019-10-20T18:37:01Z
aliases = []
[extra]
id = 19432
[taxonomies]
categories = ["task"]
tags = []
+++

URLs are strings with a simple syntax:
   scheme://[username:password@]domain[:port]/path?query_string#fragment_id


## Task

Parse a well-formed URL to retrieve the relevant information:   '''scheme''', '''domain''', '''path''', ...


Note:   this task has nothing to do with [[URL encoding]] or [[URL decoding]].


According to the standards, the characters:
::::   <big><big>    ! * ' ( ) ; : @ & = + $ , / ? % # [ ]    </big></big>
only need to be percent-encoded   ('''%''')   in case of possible confusion.

Also note that the '''path''', '''query''' and '''fragment''' are case sensitive, even if the '''scheme''' and '''domain''' are not.

The way the returned information is provided (set of variables, array, structured, record, object,...)
is language-dependent and left to the programmer, but the code should be clear enough to reuse.

Extra credit is given for clear error diagnostics.

*   Here is the official standard:     https://tools.ietf.org/html/rfc3986,
*   and here is a simpler   BNF:     http://www.w3.org/Addressing/URL/5_URI_BNF.html.


;Test cases:
According to T. Berners-Lee

'''<nowiki>foo://example.com:8042/over/there?name=ferret#nose</nowiki>'''     should parse into:
::*   scheme = foo
::*   domain = example.com
::*   port = :8042
::*   path = over/there
::*   query = name=ferret
::*   fragment = nose


'''<nowiki>urn:example:animal:ferret:nose</nowiki>'''     should parse into:
::*   scheme = urn
::*   path = example:animal:ferret:nose


'''other URLs that must be parsed include:'''
:*   <nowiki> jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true </nowiki>
:*   <nowiki> ftp://ftp.is.co.za/rfc/rfc1808.txt                                      </nowiki>
:*   <nowiki> http://www.ietf.org/rfc/rfc2396.txt#header1                             </nowiki>
:*   <nowiki> ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two               </nowiki>
:*   <nowiki> mailto:John.Doe@example.com                                             </nowiki>
:*   <nowiki> news:comp.infosystems.www.servers.unix                                  </nowiki>
:*   <nowiki> tel:+1-816-555-1212                                                     </nowiki>
:*   <nowiki> telnet://192.0.2.16:80/                                                 </nowiki>
:*   <nowiki> urn:oasis:names:specification:docbook:dtd:xml:4.1.2                     </nowiki>





## ALGOL 68

Uses the URI parser here: [[URL_parser/URI_parser_ALGOL68]].


```algol68
PR read "uriParser.a68" PR

PROC test uri parser = ( STRING uri )VOID:
     BEGIN
         URI result := parse uri( uri );
         print( ( uri, ":", newline ) );
         IF NOT ok OF result
         THEN
             # the parse failed #
             print( ( "    ", error OF result, newline ) )
         ELSE
             # parsed OK #
             print(                                    ( "      scheme: ",      scheme OF result, newline ) );
             IF   userinfo OF result /= "" THEN print( ( "    userinfo: ",    userinfo OF result, newline ) ) FI;
             IF       host OF result /= "" THEN print( ( "        host: ",        host OF result, newline ) ) FI;
             IF       port OF result /= "" THEN print( ( "        port: ",        port OF result, newline ) ) FI;
             IF       path OF result /= "" THEN print( ( "        path: ",        path OF result, newline ) ) FI;
             IF      query OF result /= "" THEN print( ( "       query: ",       query OF result, newline ) ) FI;
             IF fragment id OF result /= "" THEN print( ( " fragment id: ", fragment id OF result, newline ) ) FI
         FI;
         print( ( newline ) )
     END # test uri parser # ;

BEGIN test uri parser( "foo://example.com:8042/over/there?name=ferret#nose"                      )
    ; test uri parser( "urn:example:animal:ferret:nose"                                          )
    ; test uri parser( "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true" )
    ; test uri parser( "ftp://ftp.is.co.za/rfc/rfc1808.txt"                                      )
    ; test uri parser( "http://www.ietf.org/rfc/rfc2396.txt#header1"                             )
    ; test uri parser( "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two"               )
    ; test uri parser( "mailto:John.Doe@example.com"                                             )
    ; test uri parser( "news:comp.infosystems.www.servers.unix"                                  )
    ; test uri parser( "tel:+1-816-555-1212"                                                     )
    ; test uri parser( "telnet://192.0.2.16:80/"                                                 )
    ; test uri parser( "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"                     )
    ; test uri parser( "ssh://alice@example.com"                                                 )
    ; test uri parser( "https://bob:pass@example.com/place"                                      )
    ; test uri parser( "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"           )
END
```

```txt

foo://example.com:8042/over/there?name=ferret#nose:
      scheme: foo
        host: example.com
        port: 8042
        path: /over/there
       query: name=ferret
 fragment id: nose

urn:example:animal:ferret:nose:
      scheme: urn
        path: example:animal:ferret:nose

jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true:
      scheme: jdbc
        path: mysql://test_user:ouupppssss@localhost:3306/sakila
       query: profileSQL=true

ftp://ftp.is.co.za/rfc/rfc1808.txt:
      scheme: ftp
        host: ftp.is.co.za
        path: /rfc/rfc1808.txt

http://www.ietf.org/rfc/rfc2396.txt#header1:
      scheme: http
        host: www.ietf.org
        path: /rfc/rfc2396.txt
 fragment id: header1

ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two:
      scheme: ldap
        host: 2001:db8::7
        path: /c=GB
       query: objectClass=one&objectClass=two

mailto:John.Doe@example.com:
      scheme: mailto
        path: John.Doe@example.com

news:comp.infosystems.www.servers.unix:
      scheme: news
        path: comp.infosystems.www.servers.unix

tel:+1-816-555-1212:
      scheme: tel
        path: +1-816-555-1212

telnet://192.0.2.16:80/:
      scheme: telnet
        host: 192.0.2.16
        port: 80
        path: /

urn:oasis:names:specification:docbook:dtd:xml:4.1.2:
      scheme: urn
        path: oasis:names:specification:docbook:dtd:xml:4.1.2

ssh://alice@example.com:
      scheme: ssh
    userinfo: alice
        host: example.com

https://bob:pass@example.com/place:
      scheme: https
    userinfo: bob:pass
        host: example.com
        path: /place

http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64:
      scheme: http
        host: example.com
        path: /
       query: a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64


```



## Arturo



```arturo
url "foo://example.com:8042/over/there?name=ferret#nose"

log $(getUrlComponents url)
```


```txt
#{
	domain          "example.com"
	fragment        "nose"
	password        ""
	path            "/over/there"
	port            "8042"
	query           #{
	                	name            "ferret"
	                }
	scheme          "foo"
	user            ""
}
```


## C#


```c#
using System;

namespace RosettaUrlParse
{
    class Program
    {
        static void ParseUrl(string url)
        {
            var u = new Uri(url);
            Console.WriteLine("URL:         {0}", u.AbsoluteUri);
            Console.WriteLine("Scheme:      {0}", u.Scheme);
            Console.WriteLine("Host:        {0}", u.DnsSafeHost);
            Console.WriteLine("Port:        {0}", u.Port);
            Console.WriteLine("Path:        {0}", u.LocalPath);
            Console.WriteLine("Query:       {0}", u.Query);
            Console.WriteLine("Fragment:    {0}", u.Fragment);
            Console.WriteLine();
        }
        static void Main(string[] args)
        {
            ParseUrl("foo://example.com:8042/over/there?name=ferret#nose");
            ParseUrl("urn:example:animal:ferret:nose");
            ParseUrl("jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true");
            ParseUrl("ftp://ftp.is.co.za/rfc/rfc1808.txt");
            ParseUrl("http://www.ietf.org/rfc/rfc2396.txt#header1");
            ParseUrl("ldap://[2001:db8::7]/c=GB?objectClass?one");
            ParseUrl("mailto:John.Doe@example.com");
            ParseUrl("news:comp.infosystems.www.servers.unix");
            ParseUrl("tel:+1-816-555-1212");
            ParseUrl("telnet://192.0.2.16:80/");
            ParseUrl("urn:oasis:names:specification:docbook:dtd:xml:4.1.2");
        }
    }
}

```



## Elixir


```elixir
test_cases = [
  "foo://example.com:8042/over/there?name=ferret#nose",
  "urn:example:animal:ferret:nose",
  "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
  "ftp://ftp.is.co.za/rfc/rfc1808.txt",
  "http://www.ietf.org/rfc/rfc2396.txt#header1",
  "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
  "mailto:John.Doe@example.com",
  "news:comp.infosystems.www.servers.unix",
  "tel:+1-816-555-1212",
  "telnet://192.0.2.16:80/",
  "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
  "ssh://alice@example.com",
  "https://bob:pass@example.com/place",
  "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"
]

Enum.each(test_cases, fn str ->
  IO.puts "\n#{str}"
  IO.inspect URI.parse(str)
end)
```


```txt

foo://example.com:8042/over/there?name=ferret#nose
%URI{authority: "example.com:8042", fragment: "nose", host: "example.com",
 path: "/over/there", port: 8042, query: "name=ferret", scheme: "foo",
 userinfo: nil}

urn:example:animal:ferret:nose
%URI{authority: nil, fragment: nil, host: nil,
 path: "example:animal:ferret:nose", port: nil, query: nil, scheme: "urn",
 userinfo: nil}

jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
%URI{authority: nil, fragment: nil, host: nil,
 path: "mysql://test_user:ouupppssss@localhost:3306/sakila", port: nil,
 query: "profileSQL=true", scheme: "jdbc", userinfo: nil}

ftp://ftp.is.co.za/rfc/rfc1808.txt
%URI{authority: "ftp.is.co.za", fragment: nil, host: "ftp.is.co.za",
 path: "/rfc/rfc1808.txt", port: 21, query: nil, scheme: "ftp", userinfo: nil}

http://www.ietf.org/rfc/rfc2396.txt#header1
%URI{authority: "www.ietf.org", fragment: "header1", host: "www.ietf.org",
 path: "/rfc/rfc2396.txt", port: 80, query: nil, scheme: "http", userinfo: nil}

ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
%URI{authority: "2001:db8::7", fragment: nil, host: "2001:db8::7",
 path: "/c=GB", port: 389, query: "objectClass=one&objectClass=two",
 scheme: "ldap", userinfo: nil}

mailto:John.Doe@example.com
%URI{authority: nil, fragment: nil, host: nil, path: "John.Doe@example.com",
 port: nil, query: nil, scheme: "mailto", userinfo: nil}

news:comp.infosystems.www.servers.unix
%URI{authority: nil, fragment: nil, host: nil,
 path: "comp.infosystems.www.servers.unix", port: nil, query: nil,
 scheme: "news", userinfo: nil}

tel:+1-816-555-1212
%URI{authority: nil, fragment: nil, host: nil, path: "+1-816-555-1212",
 port: nil, query: nil, scheme: "tel", userinfo: nil}

telnet://192.0.2.16:80/
%URI{authority: "192.0.2.16:80", fragment: nil, host: "192.0.2.16", path: "/",
 port: 80, query: nil, scheme: "telnet", userinfo: nil}

urn:oasis:names:specification:docbook:dtd:xml:4.1.2
%URI{authority: nil, fragment: nil, host: nil,
 path: "oasis:names:specification:docbook:dtd:xml:4.1.2", port: nil, query: nil,
 scheme: "urn", userinfo: nil}

ssh://alice@example.com
%URI{authority: "alice@example.com", fragment: nil, host: "example.com",
 path: nil, port: nil, query: nil, scheme: "ssh", userinfo: "alice"}

https://bob:pass@example.com/place
%URI{authority: "bob:pass@example.com", fragment: nil, host: "example.com",
 path: "/place", port: 443, query: nil, scheme: "https", userinfo: "bob:pass"}

http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64
%URI{authority: "example.com", fragment: nil, host: "example.com", path: "/",
 port: 80, query: "a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64", scheme: "http",
 userinfo: nil}

```


=={{header|F_Sharp|F#}}==
<p>DotNet has the builtin class System.URI which parses URI strings.</p>
<p>Some points of interest:</p>
<ul>
<li>The Query and Fragment properties do also show the separators '?' and '#' respectively, if those parts are given in the
URI to parse. This allows to distinguish between a missing query/fragment and a given empty query/fragment.<br/>
To align with the output shown for other languages the separators are removed here.</li>
<li>the Port property is typed as an int. Therefore "not given" is generally shown as -1 (but see the following point.)</li>
<li>The System.URI class does some "Scheme-Based Normalization" (c/f rfc3986 section 6.2.3), i. e. it knows about certain
defaults for some schemes. With the test data this shows with the port numbers for http, ftp, ldap, mailto.</li></ul>

```fsharp
open System
open System.Text.RegularExpressions

let writeline n v = if String.IsNullOrEmpty(v) then () else printfn "%-15s %s" n v

let toUri = fun s -> Uri(s.ToString())
let urisFromString = (Regex(@"\S+").Matches) >> Seq.cast >> (Seq.map toUri)

urisFromString """
    foo://example.com:8042/over/there?name=ferret#nose
    urn:example:animal:ferret:nose
    jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
    ftp://ftp.is.co.za/rfc/rfc1808.txt
    http://www.ietf.org/rfc/rfc2396.txt#header1
    ldap://[2001:db8::7]/c=GB?objectClass?one
    mailto:John.Doe@example.com
    news:comp.infosystems.www.servers.unix
    tel:+1-816-555-1212
    telnet://192.0.2.16:80/
    urn:oasis:names:specification:docbook:dtd:xml:4.1.2
    """
|> Seq.iter (fun u ->
    writeline "\nURI:" (u.ToString())
    writeline "     scheme:" (u.Scheme)
    writeline "     host:" (u.Host)
    writeline "     port:" (if u.Port < 0 then "" else u.Port.ToString())
    writeline "     path:" (u.AbsolutePath)
    writeline "     query:" (if u.Query.Length > 0 then u.Query.Substring(1) else "")
    writeline "     fragment:" (if u.Fragment.Length > 0 then u.Fragment.Substring(1) else "")
    )
```

<pre style="height:3cm">
URI:           foo://example.com:8042/over/there?name=ferret#nose
     scheme:    foo
     host:      example.com
     port:      8042
     path:      /over/there
     query:     name=ferret
     fragment:  nose

URI:           urn:example:animal:ferret:nose
     scheme:    urn
     path:      example:animal:ferret:nose

URI:           jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
     scheme:    jdbc
     path:      mysql://test_user:ouupppssss@localhost:3306/sakila
     query:     profileSQL=true

URI:           ftp://ftp.is.co.za/rfc/rfc1808.txt
     scheme:    ftp
     host:      ftp.is.co.za
     port:      21
     path:      /rfc/rfc1808.txt

URI:           http://www.ietf.org/rfc/rfc2396.txt#header1
     scheme:    http
     host:      www.ietf.org
     port:      80
     path:      /rfc/rfc2396.txt
     fragment:  header1

URI:           ldap://[2001:db8::7]/c=GB?objectClass?one
     scheme:    ldap
     host:      [2001:db8::7]
     port:      389
     path:      /c=GB
     query:     objectClass?one

URI:           mailto:John.Doe@example.com
     scheme:    mailto
     host:      example.com
     port:      25

URI:           news:comp.infosystems.www.servers.unix
     scheme:    news
     path:      comp.infosystems.www.servers.unix

URI:           tel:+1-816-555-1212
     scheme:    tel
     path:      +1-816-555-1212

URI:           telnet://192.0.2.16:80/
     scheme:    telnet
     host:      192.0.2.16
     port:      80
     path:      /

URI:           urn:oasis:names:specification:docbook:dtd:xml:4.1.2
     scheme:    urn
     path:      oasis:names:specification:docbook:dtd:xml:4.1.2
```



## Go

This uses Go's standard [https://golang.org/pkg/net/url/ <tt>net/url</tt>] package.
The [https://golang.org/src/net/url/url.go source code] for this package (excluding tests) is in a single file of ~720 lines.

```go
package main

import (
	"fmt"
	"log"
	"net"
	"net/url"
)

func main() {
	for _, in := range []string{
		"foo://example.com:8042/over/there?name=ferret#nose",
		"urn:example:animal:ferret:nose",
		"jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
		"ftp://ftp.is.co.za/rfc/rfc1808.txt",
		"http://www.ietf.org/rfc/rfc2396.txt#header1",
		"ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
		"mailto:John.Doe@example.com",
		"news:comp.infosystems.www.servers.unix",
		"tel:+1-816-555-1212",
		"telnet://192.0.2.16:80/",
		"urn:oasis:names:specification:docbook:dtd:xml:4.1.2",

		"ssh://alice@example.com",
		"https://bob:pass@example.com/place",
		"http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64",
	} {
		fmt.Println(in)
		u, err := url.Parse(in)
		if err != nil {
			log.Println(err)
			continue
		}
		if in != u.String() {
			fmt.Printf("Note: reassmebles as %q\n", u)
		}
		printURL(u)
	}
}

func printURL(u *url.URL) {
	fmt.Println("    Scheme:", u.Scheme)
	if u.Opaque != "" {
		fmt.Println("    Opaque:", u.Opaque)
	}
	if u.User != nil {
		fmt.Println("    Username:", u.User.Username())
		if pwd, ok := u.User.Password(); ok {
			fmt.Println("    Password:", pwd)
		}
	}
	if u.Host != "" {
		if host, port, err := net.SplitHostPort(u.Host); err == nil {
			fmt.Println("    Host:", host)
			fmt.Println("    Port:", port)
		} else {
			fmt.Println("    Host:", u.Host)
		}
	}
	if u.Path != "" {
		fmt.Println("    Path:", u.Path)
	}
	if u.RawQuery != "" {
		fmt.Println("    RawQuery:", u.RawQuery)
		m, err := url.ParseQuery(u.RawQuery)
		if err == nil {
			for k, v := range m {
				fmt.Printf("        Key: %q Values: %q\n", k, v)
			}
		}
	}
	if u.Fragment != "" {
		fmt.Println("    Fragment:", u.Fragment)
	}
}
```

```txt

foo://example.com:8042/over/there?name=ferret#nose
    Scheme: foo
    Host: example.com
    Port: 8042
    Path: /over/there
    RawQuery: name=ferret
        Key: "name" Values: ["ferret"]
    Fragment: nose
urn:example:animal:ferret:nose
    Scheme: urn
    Opaque: example:animal:ferret:nose
jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
    Scheme: jdbc
    Opaque: mysql://test_user:ouupppssss@localhost:3306/sakila
    RawQuery: profileSQL=true
        Key: "profileSQL" Values: ["true"]
ftp://ftp.is.co.za/rfc/rfc1808.txt
    Scheme: ftp
    Host: ftp.is.co.za
    Path: /rfc/rfc1808.txt
http://www.ietf.org/rfc/rfc2396.txt#header1
    Scheme: http
    Host: www.ietf.org
    Path: /rfc/rfc2396.txt
    Fragment: header1
ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
    Scheme: ldap
    Host: [2001:db8::7]
    Path: /c=GB
    RawQuery: objectClass=one&objectClass=two
        Key: "objectClass" Values: ["one" "two"]
mailto:John.Doe@example.com
    Scheme: mailto
    Opaque: John.Doe@example.com
news:comp.infosystems.www.servers.unix
    Scheme: news
    Opaque: comp.infosystems.www.servers.unix
tel:+1-816-555-1212
    Scheme: tel
    Opaque: +1-816-555-1212
telnet://192.0.2.16:80/
    Scheme: telnet
    Host: 192.0.2.16
    Port: 80
    Path: /
urn:oasis:names:specification:docbook:dtd:xml:4.1.2
    Scheme: urn
    Opaque: oasis:names:specification:docbook:dtd:xml:4.1.2
ssh://alice@example.com
    Scheme: ssh
    Username: alice
    Host: example.com
https://bob:pass@example.com/place
    Scheme: https
    Username: bob
    Password: pass
    Host: example.com
    Path: /place
http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64
    Scheme: http
    Host: example.com
    Path: /
    RawQuery: a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64
        Key: "a" Values: ["1"]
        Key: "b" Values: ["2 2"]
        Key: "c" Values: ["3" "4"]
        Key: "d" Values: ["encoded"]

```



## Groovy

Test:

```groovy
import java.net.URI

[
    "foo://example.com:8042/over/there?name=ferret#nose",
    "urn:example:animal:ferret:nose",
    "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
    "ftp://ftp.is.co.za/rfc/rfc1808.txt",
    "http://www.ietf.org/rfc/rfc2396.txt#header1",
    "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
    "mailto:John.Doe@example.com",
    "news:comp.infosystems.www.servers.unix",
    "tel:+1-816-555-1212",
    "telnet://192.0.2.16:80/",
    "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
].each { String url ->

    // magic happens here
    URI u = url.toURI()

    // Results displayed here
    println """
            |Parsing $url
            |    scheme   = ${u.scheme}
            |    domain   = ${u.host}
            |    port     = ${(u.port + 1) ? u.port : 'default' }
            |    path     = ${u.path ?: u.schemeSpecificPart}
            |    query    = ${u.query}
            |    fragment = ${u.fragment}""".stripMargin()
}
```


Output:
<pre style="height:30ex;overflow:scroll;">
Parsing foo://example.com:8042/over/there?name=ferret#nose
    scheme   = foo
    domain   = example.com
    port     = 8042
    path     = /over/there
    query    = name=ferret
    fragment = nose

Parsing urn:example:animal:ferret:nose
    scheme   = urn
    domain   = null
    port     = default
    path     = example:animal:ferret:nose
    query    = null
    fragment = null

Parsing jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
    scheme   = jdbc
    domain   = null
    port     = default
    path     = mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
    query    = null
    fragment = null

Parsing ftp://ftp.is.co.za/rfc/rfc1808.txt
    scheme   = ftp
    domain   = ftp.is.co.za
    port     = default
    path     = /rfc/rfc1808.txt
    query    = null
    fragment = null

Parsing http://www.ietf.org/rfc/rfc2396.txt#header1
    scheme   = http
    domain   = www.ietf.org
    port     = default
    path     = /rfc/rfc2396.txt
    query    = null
    fragment = header1

Parsing ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
    scheme   = ldap
    domain   = [2001:db8::7]
    port     = default
    path     = /c=GB
    query    = objectClass=one&objectClass=two
    fragment = null

Parsing mailto:John.Doe@example.com
    scheme   = mailto
    domain   = null
    port     = default
    path     = John.Doe@example.com
    query    = null
    fragment = null

Parsing news:comp.infosystems.www.servers.unix
    scheme   = news
    domain   = null
    port     = default
    path     = comp.infosystems.www.servers.unix
    query    = null
    fragment = null

Parsing tel:+1-816-555-1212
    scheme   = tel
    domain   = null
    port     = default
    path     = +1-816-555-1212
    query    = null
    fragment = null

Parsing telnet://192.0.2.16:80/
    scheme   = telnet
    domain   = 192.0.2.16
    port     = 80
    path     = /
    query    = null
    fragment = null

Parsing urn:oasis:names:specification:docbook:dtd:xml:4.1.2
    scheme   = urn
    domain   = null
    port     = default
    path     = oasis:names:specification:docbook:dtd:xml:4.1.2
    query    = null
    fragment = null
```



## Haskell


Example uses [https://hackage.haskell.org/package/network-uri <tt>network-uri</tt>] package:


```haskell
module Main (main) where

import           Data.Foldable (for_)
import           Network.URI
                    ( URI
                    , URIAuth
                    , parseURI
                    , uriAuthority
                    , uriFragment
                    , uriPath
                    , uriPort
                    , uriQuery
                    , uriRegName
                    , uriScheme
                    , uriUserInfo
                    )

uriStrings :: [String]
uriStrings =
    [ "https://bob:pass@example.com/place"
    , "foo://example.com:8042/over/there?name=ferret#nose"
    , "urn:example:animal:ferret:nose"
    , "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true"
    , "ftp://ftp.is.co.za/rfc/rfc1808.txt"
    , "http://www.ietf.org/rfc/rfc2396.txt#header1"
    , "ldap://[2001:db8::7]/c=GB?objectClass?one"
    , "mailto:John.Doe@example.com"
    , "news:comp.infosystems.www.servers.unix"
    , "tel:+1-816-555-1212"
    , "telnet://192.0.2.16:80/"
    , "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
    ]

trimmedUriScheme :: URI -> String
trimmedUriScheme = init . uriScheme

trimmedUriUserInfo :: URIAuth -> Maybe String
trimmedUriUserInfo uriAuth =
    case uriUserInfo uriAuth of
        [] -> Nothing
        userInfo -> if last userInfo == '@' then Just (init userInfo) else Nothing

trimmedUriPath :: URI -> String
trimmedUriPath uri = case uriPath uri of '/' : t -> t; p -> p

trimmedUriQuery :: URI -> Maybe String
trimmedUriQuery uri = case uriQuery uri of '?' : t -> Just t; _ -> Nothing

trimmedUriFragment :: URI -> Maybe String
trimmedUriFragment uri = case uriFragment uri of '#' : t -> Just t; _ -> Nothing

main :: IO ()
main = do
    for_ uriStrings $ \uriString -> do
        case parseURI uriString of
            Nothing -> putStrLn $ "Could not parse" ++ uriString
            Just uri -> do
                putStrLn uriString
                putStrLn $ "  scheme = " ++ trimmedUriScheme uri
                case uriAuthority uri of
                    Nothing -> return ()
                    Just uriAuth -> do
                        case trimmedUriUserInfo uriAuth of
                            Nothing -> return ()
                            Just userInfo ->  putStrLn $ "  user-info = " ++ userInfo
                        putStrLn $ "  domain = " ++ uriRegName uriAuth
                        putStrLn $ "  port = " ++ uriPort uriAuth
                putStrLn $ "  path = " ++ trimmedUriPath uri
                case trimmedUriQuery uri of
                    Nothing -> return ()
                    Just query -> putStrLn $ "  query = " ++ query
                case trimmedUriFragment uri of
                    Nothing -> return ()
                    Just fragment ->  putStrLn $ "  fragment = " ++ fragment
        putStrLn ""
```

```txt

https://bob:pass@example.com/place
  scheme = https
  user-info = bob:pass
  domain = example.com
  port =
  path = place

foo://example.com:8042/over/there?name=ferret#nose
  scheme = foo
  domain = example.com
  port = :8042
  path = over/there
  query = name=ferret
  fragment = nose

urn:example:animal:ferret:nose
  scheme = urn
  path = example:animal:ferret:nose

jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
  scheme = jdbc
  path = mysql://test_user:ouupppssss@localhost:3306/sakila
  query = profileSQL=true

ftp://ftp.is.co.za/rfc/rfc1808.txt
  scheme = ftp
  domain = ftp.is.co.za
  port =
  path = rfc/rfc1808.txt

http://www.ietf.org/rfc/rfc2396.txt#header1
  scheme = http
  domain = www.ietf.org
  port =
  path = rfc/rfc2396.txt
  fragment = header1

ldap://[2001:db8::7]/c=GB?objectClass?one
  scheme = ldap
  domain = [2001:db8::7]
  port =
  path = c=GB
  query = objectClass?one

mailto:John.Doe@example.com
  scheme = mailto
  path = John.Doe@example.com

news:comp.infosystems.www.servers.unix
  scheme = news
  path = comp.infosystems.www.servers.unix

tel:+1-816-555-1212
  scheme = tel
  path = +1-816-555-1212

telnet://192.0.2.16:80/
  scheme = telnet
  domain = 192.0.2.16
  port = :80
  path =

urn:oasis:names:specification:docbook:dtd:xml:4.1.2
  scheme = urn
  path = oasis:names:specification:docbook:dtd:xml:4.1.2

```



## J


As most errors are contextual (e.g. invalid authority, invalid path, unrecognized scheme), we shall defer error testing to the relevant consumers. This might offend some on the grounds of temporary safety, but consumers already bear responsibility to parse and validate their relevant uri element(s).

Our parsing strategy is fixed format recursive descent. (Please do not criticize this on efficiency grounds without first investigating the implementations of other parsers.)

Implementation:


```J
split=:1 :0
  ({. ; ] }.~ 1+[)~ i.&m
)

uriparts=:3 :0
  'server fragment'=. '#' split y
  'sa query'=. '?' split server
  'scheme authpath'=. ':' split sa
  scheme;authpath;query;fragment
)

queryparts=:3 :0
  (0<#y)#<;._1 '?',y
)

authpathparts=:3 :0
  if. '//' -: 2{.y do.
    split=. <;.1 y
    (}.1{::split);;2}.split
  else.
    '';y
  end.
)

authparts=:3 :0
  if. '@' e. y do.
    'userinfo hostport'=. '@' split y
  else.
    hostport=. y [ userinfo=.''
  end.
  if. '[' = {.hostport do.
     'host_t port_t'=. ']' split hostport
     assert. (0=#port_t)+.':'={.port_t
     (':' split userinfo),(host_t,']');}.port_t
  else.
     (':' split userinfo),':' split hostport
  end.
)

taskparts=:3 :0
  'scheme authpath querystring fragment'=. uriparts y
  'auth path'=. authpathparts authpath
  'user creds host port'=. authparts auth
  query=. queryparts querystring
  export=. ;:'scheme user creds host port path query fragment'
  (#~ 0<#@>@{:"1) (,. do each) export
)
```


Task examples:


```j
   taskparts 'foo://example.com:8042/over/there?name=ferret#nose'
┌────────┬─────────────┐
│scheme  │foo          │
├────────┼─────────────┤
│host    │example.com  │
├────────┼─────────────┤
│port    │8042         │
├────────┼─────────────┤
│path    │/over/there  │
├────────┼─────────────┤
│query   │┌───────────┐│
│        ││name=ferret││
│        │└───────────┘│
├────────┼─────────────┤
│fragment│nose         │
└────────┴─────────────┘
   taskparts 'urn:example:animal:ferret:nose'
┌──────┬──────────────────────────┐
│scheme│urn                       │
├──────┼──────────────────────────┤
│path  │example:animal:ferret:nose│
└──────┴──────────────────────────┘
   taskparts 'jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true'
┌──────┬──────────────────────────────────────────────────┐
│scheme│jdbc                                              │
├──────┼──────────────────────────────────────────────────┤
│path  │mysql://test_user:ouupppssss@localhost:3306/sakila│
├──────┼──────────────────────────────────────────────────┤
│query │┌───────────────┐                                 │
│      ││profileSQL=true│                                 │
│      │└───────────────┘                                 │
└──────┴──────────────────────────────────────────────────┘
   taskparts 'ftp://ftp.is.co.za/rfc/rfc1808.txt'
┌──────┬────────────────┐
│scheme│ftp             │
├──────┼────────────────┤
│host  │ftp.is.co.za    │
├──────┼────────────────┤
│path  │/rfc/rfc1808.txt│
└──────┴────────────────┘
   taskparts 'http://www.ietf.org/rfc/rfc2396.txt#header1'
┌────────┬────────────────┐
│scheme  │http            │
├────────┼────────────────┤
│host    │www.ietf.org    │
├────────┼────────────────┤
│path    │/rfc/rfc2396.txt│
├────────┼────────────────┤
│fragment│header1         │
└────────┴────────────────┘
   taskparts 'ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two'
┌──────┬─────────────────────────────────┐
│scheme│ldap                             │
├──────┼─────────────────────────────────┤
│host  │[2001:db8::7]                    │
├──────┼─────────────────────────────────┤
│path  │/c=GB                            │
├──────┼─────────────────────────────────┤
│query │┌───────────────────────────────┐│
│      ││objectClass=one&objectClass=two││
│      │└───────────────────────────────┘│
└──────┴─────────────────────────────────┘
   taskparts 'mailto:John.Doe@example.com'
┌──────┬────────────────────┐
│scheme│mailto              │
├──────┼────────────────────┤
│path  │John.Doe@example.com│
└──────┴────────────────────┘
   taskparts 'news:comp.infosystems.www.servers.unix'
┌──────┬─────────────────────────────────┐
│scheme│news                             │
├──────┼─────────────────────────────────┤
│path  │comp.infosystems.www.servers.unix│
└──────┴─────────────────────────────────┘
   taskparts 'tel:+1-816-555-1212'
┌──────┬───────────────┐
│scheme│tel            │
├──────┼───────────────┤
│path  │+1-816-555-1212│
└──────┴───────────────┘
   taskparts 'telnet://192.0.2.16:80/'
┌──────┬──────────┐
│scheme│telnet    │
├──────┼──────────┤
│host  │192.0.2.16│
├──────┼──────────┤
│port  │80        │
├──────┼──────────┤
│path  │/         │
└──────┴──────────┘
   taskparts 'urn:oasis:names:specification:docbook:dtd:xml:4.1.2'
┌──────┬───────────────────────────────────────────────┐
│scheme│urn                                            │
├──────┼───────────────────────────────────────────────┤
│path  │oasis:names:specification:docbook:dtd:xml:4.1.2│
└──────┴───────────────────────────────────────────────┘
```


Note that the <code>path</code> of the example <code>jdbc</code> uri is itself a uri which may be parsed:


```J
   taskparts 'mysql://test_user:ouupppssss@localhost:3306/sakila'
┌──────┬──────────┐
│scheme│mysql     │
├──────┼──────────┤
│user  │test_user │
├──────┼──────────┤
│pass  │ouupppssss│
├──────┼──────────┤
│host  │localhost │
├──────┼──────────┤
│port  │3306      │
├──────┼──────────┤
│path  │/sakila   │
└──────┴──────────┘
```


Also, examples borrowed from the <code>go</code> implementation:


```J
   taskparts 'ssh://alice@example.com'
┌──────┬───────────┐
│scheme│ssh        │
├──────┼───────────┤
│user  │alice      │
├──────┼───────────┤
│host  │example.com│
└──────┴───────────┘
   taskparts 'https://bob:pass@example.com/place'
┌──────┬───────────┐
│scheme│https      │
├──────┼───────────┤
│user  │bob        │
├──────┼───────────┤
│creds │pass       │
├──────┼───────────┤
│host  │example.com│
├──────┼───────────┤
│path  │/place     │
└──────┴───────────┘
   taskparts 'http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64'
┌──────┬───────────────────────────────────────────┐
│scheme│http                                       │
├──────┼───────────────────────────────────────────┤
│host  │example.com                                │
├──────┼───────────────────────────────────────────┤
│path  │/                                          │
├──────┼───────────────────────────────────────────┤
│query │┌─────────────────────────────────────────┐│
│      ││a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64││
│      │└─────────────────────────────────────────┘│
└──────┴───────────────────────────────────────────┘
```


Note that escape decoding is left to the consumer (as well as decoding things like '+' as a replacement for the space character and determining the absolute significance of relative paths and the details of ip address parsing and so on...). This seems like a good match to the hierarchical nature of uri parsing. See [[URL_decoding#J|URL decoding]] for an implementation of escape decoding.

Note that <code>taskparts</code> was engineered specifically for the requirements of this task -- in idiomatic use you should instead expect to call the relevant '''____parts''' routines directly as illustrated by the first four lines of <code>taskparts</code>.

Note that w3c recommends a handling for query strings which differs from that of RFC-3986. For example, the use of <code>;</code> as replacement for the <code>&</code> delimiter, or the use of the query element name as the query element value when the <code>=</code> delimiter is omitted from the name/value pair. We do not implement that here, as it's not a part of this task. But that sort of implementation could be achieved by replacing the definition of <code>queryparts</code>. And, of course, other treatments of query strings are also possible, should that become necessary...


## Java

In Java, you can use the <code>URI</code> class for this, so it's pretty straightforward. I just did a bit of tweaking to output.
```Java
import java.net.URI;
import java.net.URISyntaxException;
public class WebAddressParser{
    public static void main(String[] args){
        parseAddress("foo://example.com:8042/over/there?name=ferret#nose");
        parseAddress("urn:example:animal:ferret:nose");
    }

    static void parseAddress(String a){
        System.out.println("Parsing " + a);
        try{

            // this line does the work
            URI u = new URI(a);

            System.out.println("\tscheme = " + u.getScheme());
            System.out.println("\tdomain = " + u.getHost());
            System.out.println("\tport = " + (-1==u.getPort()?"default":u.getPort()));
            System.out.println("\tpath = " + (null==u.getPath()?u.getSchemeSpecificPart():u.getPath()));
            System.out.println("\tquery = " + u.getQuery());
            System.out.println("\tfragment = " + u.getFragment());
        }
        catch (URISyntaxException x){
            System.err.println("Oops: " + x);
        }
    }
}

```
I'm only showing two examples, but the others work too, honest.
```txt
Parsing foo://example.com:8042/over/there?name=ferret#nose
	scheme = foo
	domain = example.com
	port = 8042
	path = /over/there
	query = name=ferret
	fragment = nose
Parsing urn:example:animal:ferret:nose
	scheme = urn
	domain = null
	port = default
	path = example:animal:ferret:nose
	query = null
	fragment = null
```



## JavaScript


As JavaScript is (at the time of writing) still the native language of the DOM, the simplest first-pass approach will be to set the ''href'' property of  a DOM element, and read off the various components of the DOM parse from that element.

Here is an example, tested against the JavaScript engines of current versions of Chrome and Safari, of taking this 'Gordian knot' approach to the task:


```JavaScript
(function (lstURL) {

    var e = document.createElement('a'),
        lstKeys = [
            'hash',
            'host',
            'hostname',
            'origin',
            'pathname',
            'port',
            'protocol',
            'search'
        ],

        fnURLParse = function (strURL) {
            e.href = strURL;

            return lstKeys.reduce(
                function (dct, k) {
                    dct[k] = e[k];
                    return dct;
                }, {}
            );
        };

    return JSON.stringify(
        lstURL.map(fnURLParse),
        null, 2
    );

})([
  "foo://example.com:8042/over/there?name=ferret#nose",
  "urn:example:animal:ferret:nose",
  "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
  "ftp://ftp.is.co.za/rfc/rfc1808.txt",
  "http://www.ietf.org/rfc/rfc2396.txt#header1",
  "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
  "mailto:John.Doe@example.com",
  "news:comp.infosystems.www.servers.unix",
  "tel:+1-816-555-1212",
  "telnet://192.0.2.16:80/",
  "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
  "ssh://alice@example.com",
  "https://bob:pass@example.com/place",
  "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"
]);
```


Results of applying this approach in the JavaScript of Safari 8

```JSON
[
  {
    "hash": "#nose",
    "host": "example.com:8042",
    "hostname": "example.com",
    "origin": "foo://example.com:8042",
    "pathname": "/over/there",
    "port": "8042",
    "protocol": "foo:",
    "search": "?name=ferret"
  },
  {
    "hash": "",
    "host": "",
    "hostname": "",
    "origin": "urn://",
    "pathname": "example:animal:ferret:nose",
    "port": "",
    "protocol": "urn:",
    "search": ""
  },
  {
    "hash": "",
    "host": "",
    "hostname": "",
    "origin": "jdbc://",
    "pathname": "mysql://test_user:ouupppssss@localhost:3306/sakila",
    "port": "",
    "protocol": "jdbc:",
    "search": "?profileSQL=true"
  },
  {
    "hash": "",
    "host": "ftp.is.co.za",
    "hostname": "ftp.is.co.za",
    "origin": "ftp://ftp.is.co.za",
    "pathname": "/rfc/rfc1808.txt",
    "port": "",
    "protocol": "ftp:",
    "search": ""
  },
  {
    "hash": "#header1",
    "host": "www.ietf.org",
    "hostname": "www.ietf.org",
    "origin": "http://www.ietf.org",
    "pathname": "/rfc/rfc2396.txt",
    "port": "",
    "protocol": "http:",
    "search": ""
  },
  {
    "hash": "",
    "host": "[2001:db8::7]",
    "hostname": "[2001:db8::7]",
    "origin": "ldap://[2001:db8::7]",
    "pathname": "/c=GB",
    "port": "",
    "protocol": "ldap:",
    "search": "?objectClass=one&objectClass=two"
  },
  {
    "hash": "",
    "host": "",
    "hostname": "",
    "origin": "mailto://",
    "pathname": "John.Doe@example.com",
    "port": "",
    "protocol": "mailto:",
    "search": ""
  },
  {
    "hash": "",
    "host": "",
    "hostname": "",
    "origin": "news://",
    "pathname": "comp.infosystems.www.servers.unix",
    "port": "",
    "protocol": "news:",
    "search": ""
  },
  {
    "hash": "",
    "host": "",
    "hostname": "",
    "origin": "tel://",
    "pathname": "+1-816-555-1212",
    "port": "",
    "protocol": "tel:",
    "search": ""
  },
  {
    "hash": "",
    "host": "192.0.2.16:80",
    "hostname": "192.0.2.16",
    "origin": "telnet://192.0.2.16:80",
    "pathname": "/",
    "port": "80",
    "protocol": "telnet:",
    "search": ""
  },
  {
    "hash": "",
    "host": "",
    "hostname": "",
    "origin": "urn://",
    "pathname": "oasis:names:specification:docbook:dtd:xml:4.1.2",
    "port": "",
    "protocol": "urn:",
    "search": ""
  },
  {
    "hash": "",
    "host": "example.com",
    "hostname": "example.com",
    "origin": "ssh://example.com",
    "pathname": "",
    "port": "",
    "protocol": "ssh:",
    "search": ""
  },
  {
    "hash": "",
    "host": "example.com",
    "hostname": "example.com",
    "origin": "https://example.com",
    "pathname": "/place",
    "port": "",
    "protocol": "https:",
    "search": ""
  },
  {
    "hash": "",
    "host": "example.com",
    "hostname": "example.com",
    "origin": "http://example.com",
    "pathname": "/",
    "port": "",
    "protocol": "http:",
    "search": "?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"
  }
]
```



## Julia

This solution uses Julia's [https://github.com/JuliaWeb/URIParser.jl URIParser] package.  The <code>detailview</code> function shows all of the non-empty components of the <code>URI</code> object created by this parser.  No attempt is made to further parse more complex components, e.g. query or userinfo.  Error detection is limited to indicating whether a string is parsable as a URI and providing a hint as to whether the <code>URI</code> is valid (according to this package's <code>isvalid</code> function).

```Julia

using URIParser
const FIELDS = names(URI)

function detailview(uri::URI, indentlen::Int=4)
    indent = " "^indentlen
    s = String[]
    for f in FIELDS
        d = string(getfield(uri, f))
        !isempty(d) || continue
        f != :port || d != "0" || continue
        push!(s, @sprintf("%s%s:  %s", indent, string(f), d))
    end
    join(s, "\n")
end

test = ["foo://example.com:8042/over/there?name=ferret#nose",
        "urn:example:animal:ferret:nose",
        "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
        "ftp://ftp.is.co.za/rfc/rfc1808.txt",
        "http://www.ietf.org/rfc/rfc2396.txt#header1",
        "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
        "mailto:John.Doe@example.com",
        "news:comp.infosystems.www.servers.unix",
        "tel:+1-816-555-1212",
        "telnet://192.0.2.16:80/",
        "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
        "This is not a URI!",
        "ssh://alice@example.com",
        "https://bob:pass@example.com/place",
        "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"]

isfirst = true
for st in test
    if isfirst
        isfirst = false
    else
        println()
    end
    println("Attempting to parse\n  \"", st, "\" as a URI:")
    uri = try
        URI(st)
    catch
        println("URIParser failed to parse this URI, is it OK?")
        continue
    end
    print("This URI is parsable ")
    if isvalid(uri)
        println("and appears to be valid.")
    else
        println("but may be invalid.")
    end
    println(detailview(uri))
end

```


```txt

Attempting to parse
  "foo://example.com:8042/over/there?name=ferret#nose" as a URI:
This URI is parsable but may be invalid.
    schema:  foo
    host:  example.com
    port:  8042
    path:  /over/there
    query:  name=ferret
    fragment:  nose
    specifies_authority:  true

Attempting to parse
  "urn:example:animal:ferret:nose" as a URI:
This URI is parsable and appears to be valid.
    schema:  urn
    path:  example:animal:ferret:nose
    specifies_authority:  false

Attempting to parse
  "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true" as a URI:
This URI is parsable but may be invalid.
    schema:  jdbc
    path:  mysql://test_user:ouupppssss@localhost:3306/sakila
    query:  profileSQL=true
    specifies_authority:  false

Attempting to parse
  "ftp://ftp.is.co.za/rfc/rfc1808.txt" as a URI:
This URI is parsable and appears to be valid.
    schema:  ftp
    host:  ftp.is.co.za
    path:  /rfc/rfc1808.txt
    specifies_authority:  true

Attempting to parse
  "http://www.ietf.org/rfc/rfc2396.txt#header1" as a URI:
This URI is parsable and appears to be valid.
    schema:  http
    host:  www.ietf.org
    path:  /rfc/rfc2396.txt
    fragment:  header1
    specifies_authority:  true

Attempting to parse
  "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two" as a URI:
This URI is parsable and appears to be valid.
    schema:  ldap
    host:  2001:db8::7
    path:  /c=GB
    query:  objectClass=one&objectClass=two
    specifies_authority:  true

Attempting to parse
  "mailto:John.Doe@example.com" as a URI:
This URI is parsable and appears to be valid.
    schema:  mailto
    path:  John.Doe@example.com
    specifies_authority:  false

Attempting to parse
  "news:comp.infosystems.www.servers.unix" as a URI:
This URI is parsable and appears to be valid.
    schema:  news
    path:  comp.infosystems.www.servers.unix
    specifies_authority:  false

Attempting to parse
  "tel:+1-816-555-1212" as a URI:
This URI is parsable and appears to be valid.
    schema:  tel
    path:  +1-816-555-1212
    specifies_authority:  false

Attempting to parse
  "telnet://192.0.2.16:80/" as a URI:
This URI is parsable and appears to be valid.
    schema:  telnet
    host:  192.0.2.16
    port:  80
    path:  /
    specifies_authority:  true

Attempting to parse
  "urn:oasis:names:specification:docbook:dtd:xml:4.1.2" as a URI:
This URI is parsable and appears to be valid.
    schema:  urn
    path:  oasis:names:specification:docbook:dtd:xml:4.1.2
    specifies_authority:  false

Attempting to parse
  "This is not a URI!" as a URI:
URIParser failed to parse this URI, is it OK?

Attempting to parse
  "ssh://alice@example.com" as a URI:
This URI is parsable but may be invalid.
    schema:  ssh
    host:  example.com
    userinfo:  alice
    specifies_authority:  true

Attempting to parse
  "https://bob:pass@example.com/place" as a URI:
This URI is parsable and appears to be valid.
    schema:  https
    host:  example.com
    path:  /place
    userinfo:  bob:pass
    specifies_authority:  true

Attempting to parse
  "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64" as a URI:
This URI is parsable and appears to be valid.
    schema:  http
    host:  example.com
    path:  /
    query:  a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64
    specifies_authority:  true

```



## Kotlin

Although the java.net.URL class can parse urls just fine, unfortunately (as far as this task is concerned) the constructor throws an exception if it does not recognize the scheme (or 'protocol' as it calls it). To deal with unrecognized protocols such as 'foo', we therefore need to replace them with a valid protocol such as 'http' to trick the URL class into parsing them properly:

```scala
// version 1.1.2

import java.net.URL
import java.net.MalformedURLException

fun parseUrl(url: String) {
    var u: URL
    var scheme: String
    try {
        u = URL(url)
        scheme = u.protocol
    }
    catch (ex: MalformedURLException) {
        val index = url.indexOf(':')
        scheme = url.take(index)
        u = URL("http" + url.drop(index))
    }
    println("Parsing $url")
    println("  scheme   =  $scheme")

    with(u) {
        if (userInfo != null) println("  userinfo =  $userInfo")
        if (!host.isEmpty())  println("  domain   =  $host")
        if (port != -1)       println("  port     =  $port")
        if (!path.isEmpty())  println("  path     =  $path")
        if (query != null)    println("  query    =  $query")
        if (ref != null)      println("  fragment =  $ref")
    }
    println()
}

fun main(args: Array<String>){
    val urls = arrayOf(
        "foo://example.com:8042/over/there?name=ferret#nose",
        "urn:example:animal:ferret:nose",
        "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
        "ftp://ftp.is.co.za/rfc/rfc1808.txt",
        "http://www.ietf.org/rfc/rfc2396.txt#header1",
        "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
        "mailto:John.Doe@example.com",
        "news:comp.infosystems.www.servers.unix",
        "tel:+1-816-555-1212",
        "telnet://192.0.2.16:80/",
        "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
        "ssh://alice@example.com",
        "https://bob:pass@example.com/place",
        "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"
    )
    for (url in urls) parseUrl(url)
}
```


```txt

Parsing foo://example.com:8042/over/there?name=ferret#nose
  scheme   =  foo
  domain   =  example.com
  port     =  8042
  path     =  /over/there
  query    =  name=ferret
  fragment =  nose

Parsing urn:example:animal:ferret:nose
  scheme   =  urn
  path     =  example:animal:ferret:nose

Parsing jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
  scheme   =  jdbc
  path     =  mysql://test_user:ouupppssss@localhost:3306/sakila
  query    =  profileSQL=true

Parsing ftp://ftp.is.co.za/rfc/rfc1808.txt
  scheme   =  ftp
  domain   =  ftp.is.co.za
  path     =  /rfc/rfc1808.txt

Parsing http://www.ietf.org/rfc/rfc2396.txt#header1
  scheme   =  http
  domain   =  www.ietf.org
  path     =  /rfc/rfc2396.txt
  fragment =  header1

Parsing ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
  scheme   =  ldap
  domain   =  [2001:db8::7]
  path     =  /c=GB
  query    =  objectClass=one&objectClass=two

Parsing mailto:John.Doe@example.com
  scheme   =  mailto
  path     =  John.Doe@example.com

Parsing news:comp.infosystems.www.servers.unix
  scheme   =  news
  path     =  comp.infosystems.www.servers.unix

Parsing tel:+1-816-555-1212
  scheme   =  tel
  path     =  +1-816-555-1212

Parsing telnet://192.0.2.16:80/
  scheme   =  telnet
  domain   =  192.0.2.16
  port     =  80
  path     =  /

Parsing urn:oasis:names:specification:docbook:dtd:xml:4.1.2
  scheme   =  urn
  path     =  oasis:names:specification:docbook:dtd:xml:4.1.2

Parsing ssh://alice@example.com
  scheme   =  ssh
  userinfo =  alice
  domain   =  example.com

Parsing https://bob:pass@example.com/place
  scheme   =  https
  userinfo =  bob:pass
  domain   =  example.com
  path     =  /place

Parsing http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64
  scheme   =  http
  domain   =  example.com
  path     =  /
  query    =  a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64

```



## Lua

```lua
local url = require('socket.url')

local tests = {
  'foo://example.com:8042/over/there?name=ferret#nose',
  'urn:example:animal:ferret:nose',
  'jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true',
  'ftp://ftp.is.co.za/rfc/rfc1808.txt',
  'http://www.ietf.org/rfc/rfc2396.txt#header1',
  'ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two',
  'mailto:John.Doe@example.com',
  'news:comp.infosystems.www.servers.unix',
  'tel:+1-816-555-1212',
  'telnet://192.0.2.16:80/',
  'urn:oasis:names:specification:docbook:dtd:xml:4.1.2'
}

for _, test in ipairs(tests) do
  local parsed = url.parse(test)

  io.write('URI: ' .. test .. '\n')

  for k, v in pairs(parsed) do
    io.write(string.format('  %s: %s\n', k, v))
  end

  io.write('\n')
end

```

```txt

URI: foo://example.com:8042/over/there?name=ferret#nose
  fragment: nose
  authority: example.com:8042
  host: example.com
  query: name=ferret
  scheme: foo
  path: /over/there
  port: 8042

URI: urn:example:animal:ferret:nose
  scheme: urn
  path: example:animal:ferret:nose

URI: jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
  query: profileSQL=true
  scheme: jdbc
  path: mysql://test_user:ouupppssss@localhost:3306/sakila

URI: ftp://ftp.is.co.za/rfc/rfc1808.txt
  host: ftp.is.co.za
  path: /rfc/rfc1808.txt
  authority: ftp.is.co.za
  scheme: ftp

URI: http://www.ietf.org/rfc/rfc2396.txt#header1
  fragment: header1
  authority: www.ietf.org
  host: www.ietf.org
  path: /rfc/rfc2396.txt
  scheme: http

URI: ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
  scheme: ldap
  query: objectClass=one&objectClass=two
  host: 2001:db8::7
  path: /c=GB
  authority: [2001:db8::7]

URI: mailto:John.Doe@example.com
  scheme: mailto
  path: John.Doe@example.com

URI: news:comp.infosystems.www.servers.unix
  scheme: news
  path: comp.infosystems.www.servers.unix

URI: tel:+1-816-555-1212
  scheme: tel
  path: +1-816-555-1212

URI: telnet://192.0.2.16:80/
  authority: 192.0.2.16:80
  host: 192.0.2.16
  scheme: telnet
  path: /
  port: 80

URI: urn:oasis:names:specification:docbook:dtd:xml:4.1.2
  scheme: urn
  path: oasis:names:specification:docbook:dtd:xml:4.1.2

```


## M2000 Interpreter


### Using M2000 script to parse URL


```M2000 Interpreter

Module checkit {
      any=lambda (z$)->{=lambda z$ (a$)->instr(z$,a$)>0}
      one=lambda (z$)->{=lambda z$ (a$)->z$=a$}
      number$="0123456789"

      series=Lambda  -> {
                  func=Array([])
                  =lambda  func (&line$, &res$)->{
                        if line$="" then exit
                        k=each(func)
                        def p=0,ok as boolean
                        while k {
                              ok=false :  p++ :  f=array(k)
                              if not f(mid$(line$,p,1)) then exit
                              ok=true
                        }
                        if ok then res$=left$(line$, p) : line$=mid$(line$, p+1)
                        =ok
                  }
      }

      is_any=lambda series, any  (c$) ->series(any(c$))
      is_one=lambda series, one  (c$) ->series(one(c$))
      Is_Alpha=series(lambda (a$)-> a$ ~ "[a-zA-Z]")
      Is_digit=series(any(number$))
      Is_hex=any(number$+"abcdefABCDEF")

      optionals=Lambda  -> {
                  func=Array([])
                  =lambda  func (&line$, &res$)->{
                        k=each(func)
                        def ok as boolean
                        while k {
                              f=array(k)
                              if f(&line$,&res$) then ok=true : exit
                        }
                        =ok
                  }
      }
      repeated=Lambda  (func)-> {
                  =lambda  func (&line$, &res$)->{
                        def ok as boolean, a$
                        res$=""
                        do {
                              sec=len(line$)
                              if not func(&line$,&a$) then exit
                              res$+=a$
                              ok=true
                        } until line$="" or sec=len(line$)
                        =ok
                  }
      }

      oneAndoptional=lambda (func1, func2) -> {
            =lambda  func1, func2 (&line$, &res$)->{
                              def ok as boolean, a$
                              res$=""
                              if not func1(&line$,&res$) then exit
                              if func2(&line$,&a$) then res$+=a$
                              =True
                        }
      }
      many=Lambda  -> {
                  func=Array([])
                  =lambda  func (&line$, &res$)->{
                        k=each(func)
                        def p=0,ok as boolean, acc$
                        oldline$=line$
                        while k  {
                              ok=false
                              res$=""
                              if line$="" then exit
                              f=array(k)
                              if not f(&line$,&res$) then exit
                              acc$+=res$
                              ok=true
                         }
                        if not ok then {line$=oldline$}  else res$=acc$
                        =ok
                  }
      }
      is_safe=series(any("$-_@.&"))
      Is_extra=series(any("!*'(),"+chr$(34)))
      Is_Escape=series(any("%"), is_hex, is_hex)
      \\Is_reserved=series(any("=;/#?: "))
      is_xalpha=optionals(Is_Alpha, is_digit, is_safe, is_extra, is_escape)
      is_xalphas=oneAndoptional(is_xalpha,repeated(is_xalpha))
      is_xpalpha=optionals(is_xalpha, is_one("+"))
      is_xpalphas=oneAndoptional(is_xpalpha,repeated(is_xpalpha))
      Is_ialpha=oneAndoptional(Is_Alpha,repeated(is_xpalphas))
      is_fragmentid=lambda is_xalphas (&lines$, &res$) -> {
            =is_xalphas(&lines$, &res$)
      }
      is_search=oneAndoptional(is_xalphas, repeated(many(series(one("+")), is_xalphas)))
      is_void=lambda (f)-> {
            =lambda f (&oldline$, &res$)-> {
                  line$=oldline$
                  if f(&line$, &res$) then {oldline$=line$ } else res$=""
                  =true
            }
      }
      is_scheme=is_ialpha
      is_path=repeated(oneAndoptional(is_void(is_xpalphas), series(one("/"))))
      is_uri=oneAndoptional(many(is_scheme, series(one(":")), is_path), many(series(one("?")),is_search))
      is_fragmentaddress=oneAndoptional(is_uri, many(series(one("#")),is_fragmentid ))

      data "foo://example.com:8042/over/there?name=ferret#nose"
      data "urn:example:animal:ferret:nose"
      data "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true "
      data "ftp://ftp.is.co.za/rfc/rfc1808.txt"
      data "http://www.ietf.org/rfc/rfc2396.txt#header1"
      data "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two"
      data "mailto:John.Doe@example.com"
      data "tel:+1-816-555-1212"
      data "telnet://192.0.2.16:80/"
      data "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"

      while not empty {
            read What$
            pen 15 {
                  Print What$
            }
            a$=""
            If is_scheme(&What$, &a$) Then  Print "Scheme=";a$ : What$=mid$(What$,2)
            If is_path(&What$, &a$) Then {
                  count=0
                  while left$(a$, 1)="/" { a$=mid$(a$,2): count++}
                  if count>1 then {
                        domain$=leftpart$(a$+"/", "/")
                        a$=rightpart$(a$,"/")
                        if domain$<>"" Then Print "Domain:";Domain$
                        if a$<>"" Then Print "Path:";a$
                  } else.if  left$(What$,1) =":" then {
                        Print "path:";a$+What$: What$=""
                  } Else Print  "Data:"; a$

            }

            if left$(What$,1) =":" then {
                  is_number=repeated(is_digit)
                  What$=mid$(What$,2): If is_number(&What$, &a$) Then Print "Port:";a$
                  if not left$(What$,1)="/" then  exit
                  If is_path(&What$, &a$) Then {
                        while left$(a$, 1)="/" { a$=mid$(a$,2)}
                        if a$<>"" Then Print "Path:";a$
                  }
            }
            if left$(What$, 1)="?" then {
                        What$=mid$(What$,2)
                        If is_search(&What$, &a$) Then  {
                        v$=""
                        if left$(What$, 1)="=" then {
                              What$=mid$(What$,2)
                              If is_search(&What$, &v$) Then  Print "Query:";a$;"=";v$
                        }  else Print "Query:";a$
                        }
            }
            While  left$(What$, 1)="#"  {
            What$=mid$(What$,2)
            if not is_xalphas(&What$, &a$) Then exit
            Print "fragment:";a$
            }
            if What$<>"" Then Print "Data:"; What$
      }
}
Checkit

```

<pre style="height:30ex;overflow:scroll">
foo://example.com:8042/over/there?name=ferret#nose
Scheme=foo
Domain:example.com
Port:8042
Path:over/there
Query:name=ferret
fragment:nose
urn:example:animal:ferret:nose
Scheme=urn
path:example:animal:ferret:nose
jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
Scheme=jdbc
path:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
ftp://ftp.is.co.za/rfc/rfc1808.txt
Scheme=ftp
Domain:ftp.is.co.za
Path:rfc/rfc1808.txt
http://www.ietf.org/rfc/rfc2396.txt#header1
Scheme=http
Domain:www.ietf.org
Path:rfc/rfc2396.txt
fragment:header1
ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
Scheme=ldap
Data:[2001:db8::7]/c=GB?objectClass=one&objectClass=two
mailto:John.Doe@example.com
Scheme=mailto
Data:John.Doe@example.com
tel:+1-816-555-1212
Scheme=tel
Data:+1-816-555-1212
telnet://192.0.2.16:80/
Scheme=telnet
Domain:192.0.2.16
Port:80
urn:oasis:names:specification:docbook:dtd:xml:4.1.2
Scheme=urn
path:oasis:names:specification:docbook:dtd:xml:4.1.2
</pre >

===Using an internal function (variation of String$())===

```M2000 Interpreter

module Checkit {
      Stack New {
            Data "foo://example.com:8042/over/there?name=ferret#nose", "urn:example:animal:ferret:nose"
            Data "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true", "ftp://ftp.is.co.za/rfc/rfc1808.txt"
            Data "http://www.ietf.org/rfc/rfc2396.txt#header1", "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two"
            Data "mailto:John.Doe@example.com", "news:comp.infosystems.www.servers.unix", "tel:+1-816-555-1212"
            Data "telnet://192.0.2.16:80/", "urn:oasis:names:specification:docbook:dtd:xml:4.1.2", "ssh://alice@example.com"
            Data "https://bob:pass@example.com/place", "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"
            a=Array([])
      }
      function prechar$(a$, b$) {
            if a$<>"" then {=quote$(b$+a$)} else ={""}
      }
      z=each(a)
      document s$="["+{
      }
      While z {
            a$=array$(z)
            s1$={           "uri": }+quote$(a$)+{,
                  "authority": }+ quote$(string$(a$ as URLAuthority))+{,
                  "userInfo": }+ quote$(string$(a$ as URLUserInfo))+{,
                  "scheme": }+quote$(string$(a$ as URLScheme))+{,
                  "hostname": }+quote$(string$(a$ as UrlHost))+{,
                  "Port": }+quote$(string$(a$ as UrlPort))+{,
                  "pathname": }+quote$(string$(a$ as UrlPath))+{,
                  "search": }+prechar$(string$(a$ as URLpart 6),"?")+{,
                  "hash": }+prechar$(string$(a$ as UrlFragment),"#")+{
            }
            s$="     {"+{
            }+s1$+"     }"
            \\ z^ is the iteraror's counter (z is an iterator of a, a touple - array in M2000)
            if z^<len(a)-1 then s$=" ,"   ' append to document
            s$={
            }
      }
      s$="]"
      Print "Press any keyboard key or mouse key to continue scrolling"
      Report s$
      Clipboard s$
}
Checkit

```

<pre style="height:30ex;overflow:scroll">
[
     {
           "uri": "foo://example.com:8042/over/there?name=ferret#nose",
                 "authority": "example.com:8042",
                 "userInfo": "",
                 "scheme": "foo",
                 "hostname": "example.com",
                 "Port": "8042",
                 "pathname": "/over/there",
                 "search": "?name=ferret",
                 "hash": "#nose"
     } ,
     {
           "uri": "urn:example:animal:ferret:nose",
                 "authority": "",
                 "userInfo": "",
                 "scheme": "urn",
                 "hostname": "",
                 "Port": "",
                 "pathname": "example:animal:ferret:nose",
                 "search": "",
                 "hash": ""
     } ,
     {
           "uri": "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
                 "authority": "",
                 "userInfo": "",
                 "scheme": "jdbc",
                 "hostname": "",
                 "Port": "",
                 "pathname": "mysql://test_user:ouupppssss@localhost:3306/sakila",
                 "search": "?profileSQL=true",
                 "hash": ""
     } ,
     {
           "uri": "ftp://ftp.is.co.za/rfc/rfc1808.txt",
                 "authority": "ftp.is.co.za",
                 "userInfo": "",
                 "scheme": "ftp",
                 "hostname": "ftp.is.co.za",
                 "Port": "21",
                 "pathname": "/rfc/rfc1808.txt",
                 "search": "",
                 "hash": ""
     } ,
     {
           "uri": "http://www.ietf.org/rfc/rfc2396.txt#header1",
                 "authority": "www.ietf.org",
                 "userInfo": "",
                 "scheme": "http",
                 "hostname": "www.ietf.org",
                 "Port": "80",
                 "pathname": "/rfc/rfc2396.txt",
                 "search": "",
                 "hash": "#header1"
     } ,
     {
           "uri": "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
                 "authority": "2001:db8::7",
                 "userInfo": "",
                 "scheme": "ldap",
                 "hostname": "2001:db8::7",
                 "Port": "389",
                 "pathname": "/c=GB",
                 "search": "?objectClass=one&objectClass=two",
                 "hash": ""
     } ,
     {
           "uri": "mailto:John.Doe@example.com",
                 "authority": "",
                 "userInfo": "",
                 "scheme": "mailto",
                 "hostname": "",
                 "Port": "",
                 "pathname": "John.Doe@example.com",
                 "search": "",
                 "hash": ""
     } ,
     {
           "uri": "news:comp.infosystems.www.servers.unix",
                 "authority": "",
                 "userInfo": "",
                 "scheme": "news",
                 "hostname": "",
                 "Port": "",
                 "pathname": "comp.infosystems.www.servers.unix",
                 "search": "",
                 "hash": ""
     } ,
     {
           "uri": "tel:+1-816-555-1212",
                 "authority": "",
                 "userInfo": "",
                 "scheme": "tel",
                 "hostname": "",
                 "Port": "",
                 "pathname": "+1-816-555-1212",
                 "search": "",
                 "hash": ""
     } ,
     {
           "uri": "telnet://192.0.2.16:80/",
                 "authority": "192.0.2.16:80",
                 "userInfo": "",
                 "scheme": "telnet",
                 "hostname": "192.0.2.16",
                 "Port": "80",
                 "pathname": "",
                 "search": "",
                 "hash": ""
     } ,
     {
           "uri": "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
                 "authority": "",
                 "userInfo": "",
                 "scheme": "urn",
                 "hostname": "",
                 "Port": "",
                 "pathname": "oasis:names:specification:docbook:dtd:xml:4.1.2",
                 "search": "",
                 "hash": ""
     } ,
     {
           "uri": "ssh://alice@example.com",
                 "authority": "alice@example.com",
                 "userInfo": "alice",
                 "scheme": "ssh",
                 "hostname": "example.com",
                 "Port": "",
                 "pathname": "",
                 "search": "",
                 "hash": ""
     } ,
     {
           "uri": "https://bob:pass@example.com/place",
                 "authority": "bob:pass@example.com",
                 "userInfo": "bob:pass",
                 "scheme": "https",
                 "hostname": "example.com",
                 "Port": "443",
                 "pathname": "/place",
                 "search": "",
                 "hash": ""
     } ,
     {
           "uri": "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64",
                 "authority": "example.com",
                 "userInfo": "",
                 "scheme": "http",
                 "hostname": "example.com",
                 "Port": "80",
                 "pathname": "/",
                 "search": "?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64",
                 "hash": ""
     }
]
</pre >


## Mathematica


```Mathematica
URLParse["foo://example.com:8042/over/there?name=ferret#nose"]
```

```txt
<|"Scheme" -> "foo", "User" -> None, "Domain" -> "example.com",
 "Port" -> 8042, "Path" -> {"", "over", "there"},
 "Query" -> {"name" -> "ferret"}, "Fragment" -> "nose"|>
```



## Nim

The uri module provides a parseUri proc...


```nim
import std/[uri, strformat, strutils]

proc printUri(url: string) =
  echo url
  let res = parseUri(url)
  if res.scheme != "":
    echo &"\t  Scheme: {res.scheme}"
  if res.hostname != "":
    echo &"\tHostname: {res.hostname}"
  if res.username != "":
    echo &"\tUsername: {res.username}"
  if res.password != "":
    echo &"\tPassword: {res.password}"
  if res.path != "":
    echo &"\t    Path: {res.path}"
  if res.query != "":
    echo &"\t   Query: {res.query}"
  if res.port != "":
    echo &"\t    Port: {res.port}"
  if res.anchor != "":
    echo &"\t  Anchor: {res.anchor}"
  if res.opaque:
    echo &"\t  Opaque: {res.opaque}"

let urls = @["foo://example.com:8042/over/there?name=ferret#nose",
            "urn:example:animal:ferret:nose",
            "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
            "ftp://ftp.is.co.za/rfc/rfc1808.txt",
            "http://www.ietf.org/rfc/rfc2396.txt#header1",
            "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
            "mailto:John.Doe@example.com",
            "news:comp.infosystems.www.servers.unix",
            "tel:+1-816-555-1212",
            "telnet://192.0.2.16:80/",
            "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
            "ssh://alice@example.com",
            "https://bob:pass@example.com/place",
            "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"]

for url in urls:
  printUri(url)
```

```txt
foo://example.com:8042/over/there?name=ferret#nose
          Scheme: foo
        Hostname: example.com
            Path: /over/there
           Query: name=ferret
            Port: 8042
          Anchor: nose
urn:example:animal:ferret:nose
          Scheme: urn
            Path: example:animal:ferret:nose
          Opaque: true
jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
          Scheme: jdbc
            Path: mysql://test_user:ouupppssss@localhost:3306/sakila
           Query: profileSQL=true
          Opaque: true
ftp://ftp.is.co.za/rfc/rfc1808.txt
          Scheme: ftp
        Hostname: ftp.is.co.za
            Path: /rfc/rfc1808.txt
http://www.ietf.org/rfc/rfc2396.txt#header1
          Scheme: http
        Hostname: www.ietf.org
            Path: /rfc/rfc2396.txt
          Anchor: header1
ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
          Scheme: ldap
        Hostname: 2001:db8::7
            Path: /c=GB
           Query: objectClass=one&objectClass=two
mailto:John.Doe@example.com
          Scheme: mailto
        Hostname: example.com
        Username: John.Doe
          Opaque: true
news:comp.infosystems.www.servers.unix
          Scheme: news
            Path: comp.infosystems.www.servers.unix
          Opaque: true
tel:+1-816-555-1212
          Scheme: tel
            Path: +1-816-555-1212
          Opaque: true
telnet://192.0.2.16:80/
          Scheme: telnet
        Hostname: 192.0.2.16
            Path: /
            Port: 80
urn:oasis:names:specification:docbook:dtd:xml:4.1.2
          Scheme: urn
            Path: oasis:names:specification:docbook:dtd:xml:4.1.2
          Opaque: true
ssh://alice@example.com
          Scheme: ssh
        Hostname: example.com
        Username: alice
https://bob:pass@example.com/place
          Scheme: https
        Hostname: example.com
        Username: bob
        Password: pass
            Path: /place
http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64
          Scheme: http
        Hostname: example.com
            Path: /
           Query: a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64
```



## Objeck



```objeck
use Web.HTTP;

class Test {
  function : Main(args : String[]) ~ Nil {
    urls := [
      "foo://example.com:8042/over/there?name=ferret#nose",
      "urn:example:animal:ferret:nose",
      "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
      "ftp://ftp.is.co.za/rfc/rfc1808.txt",
      "http://www.ietf.org/rfc/rfc2396.txt#header1",
      "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
      "mailto:John.Doe@example.com",
      "news:comp.infosystems.www.servers.unix",
      "tel:+1-816-555-1212",
      "telnet://192.0.2.16:80/",
      "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
      "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64:"];

    each(i : urls) {
      url := Url->New(urls[i]);
      if(url->Parsed()) {
        url->ToString()->PrintLine();
      };
    };
  }
}
```


```txt
foo://example.com:8042/over/there?name=ferret#nose
        scheme='foo'
        host='example.com'
        path='/over/there'
        port='8042'
        query='name=ferret'
        fragment='nose'

urn:example:animal:ferret:nose
        scheme='urn'
        path='example:animal:ferret:nose'

jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
        scheme='jdbc'
        path='mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true'

ftp://ftp.is.co.za/rfc/rfc1808.txt
        scheme='ftp'
        host='ftp.is.co.za'
        path='/rfc/rfc1808.txt'

http://www.ietf.org/rfc/rfc2396.txt#header1
        scheme='http'
        host='www.ietf.org'
        path='/rfc/rfc2396.txt'
        fragment='header1'

ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
        scheme='ldap'
        host='[2001:db8::7]'
        path='/c=GB'
        query='objectClass=one&objectClass=two'

mailto:John.Doe@example.com
        scheme='mailto'
        path='John.Doe@example.com'

news:comp.infosystems.www.servers.unix
        scheme='news'
        path='comp.infosystems.www.servers.unix'

tel:+1-816-555-1212
        scheme='tel'
        path='+1-816-555-1212'

telnet://192.0.2.16:80/
        scheme='telnet'
        host='192.0.2.16'
        path='/'
        port='80'

urn:oasis:names:specification:docbook:dtd:xml:4.1.2
        scheme='urn'
        path='oasis:names:specification:docbook:dtd:xml:4.1.2'

http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64:
        scheme='http'
        host='example.com'
        path='/'
        query='a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64:'
```



## Perl

You can use the URI module from CPAN to parse URIs. Note that the output is a bit different: for example, you don't get the host from the <code>foo://</code> scheme, as host is only valid for schemes that define it.

```perl
#!/usr/bin/perl
use warnings;
use strict;

use URI;

for my $uri (do { no warnings 'qw';
                  qw( foo://example.com:8042/over/there?name=ferret#nose
                      urn:example:animal:ferret:nose
                      jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
                      ftp://ftp.is.co.za/rfc/rfc1808.txt
                      http://www.ietf.org/rfc/rfc2396.txt#header1
                      ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
                      mailto:John.Doe@example.com
                      news:comp.infosystems.www.servers.unix
                      tel:+1-816-555-1212
                      telnet://192.0.2.16:80/
                      urn:oasis:names:specification:docbook:dtd:xml:4.1.2
                   )}) {
    my $u = 'URI'->new($uri);
    print "$uri\n";
    for my $part (qw( scheme path fragment authority host port query )) {
        eval { my $parsed = $u->$part;
               print "\t", $part, "\t", $parsed, "\n" if defined $parsed;
        };
    }

}
```

```txt
foo://example.com:8042/over/there?name=ferret#nose
        scheme  foo
        path    /over/there
        fragment        nose
        authority       example.com:8042
        query   name=ferret
urn:example:animal:ferret:nose
        scheme  urn
        path    example:animal:ferret:nose
jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
        scheme  jdbc
        path    mysql://test_user:ouupppssss@localhost:3306/sakila
        query   profileSQL=true
ftp://ftp.is.co.za/rfc/rfc1808.txt
        scheme  ftp
        path    /rfc/rfc1808.txt
        authority       ftp.is.co.za
        host    ftp.is.co.za
        port    21
http://www.ietf.org/rfc/rfc2396.txt#header1
        scheme  http
        path    /rfc/rfc2396.txt
        fragment        header1
        authority       www.ietf.org
        host    www.ietf.org
        port    80
ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
        scheme  ldap
        path    /c=GB
        authority       [2001:db8::7]
        host    2001:db8::7
        port    389
        query   objectClass=one&objectClass=two
mailto:John.Doe@example.com
        scheme  mailto
        path    John.Doe@example.com
news:comp.infosystems.www.servers.unix
        scheme  news
        path    comp.infosystems.www.servers.unix
        port    119
tel:+1-816-555-1212
        scheme  tel
        path    +1-816-555-1212
telnet://192.0.2.16:80/
        scheme  telnet
        path    /
        authority       192.0.2.16:80
        host    192.0.2.16
        port    80
urn:oasis:names:specification:docbook:dtd:xml:4.1.2
        scheme  urn
        path    oasis:names:specification:docbook:dtd:xml:4.1.2
```



## Perl 6

Uses the URI library which implements a Perl 6 grammar based on the RFC 3986 BNF grammar.

```perl6
use URI;

my @test-uris = <
    foo://example.com:8042/over/there?name=ferret#nose
    urn:example:animal:ferret:nose
    jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
    ftp://ftp.is.co.za/rfc/rfc1808.txt
    http://www.ietf.org/rfc/rfc2396.txt#header1
    ldap://[2001:db8::7]/c=GB?objectClass?one
    mailto:John.Doe@example.com
    news:comp.infosystems.www.servers.unix
    tel:+1-816-555-1212
    telnet://192.0.2.16:80/
    urn:oasis:names:specification:docbook:dtd:xml:4.1.2
>;

my $u = URI.new;

for @test-uris -> $uri {
    say "URI:\t", $uri;
        $u.parse($uri);
        for <scheme host port path query frag> -> $t {
           my $token = try {$u."$t"()} || '';
           say "$t:\t", $token if $token;
        }
    say '';
}
```

```txt
URI:	foo://example.com:8042/over/there?name=ferret#nose
scheme:	foo
host:	example.com
port:	8042
path:	/over/there
query:	name=ferret
frag:	nose

URI:	urn:example:animal:ferret:nose
scheme:	urn
path:	example:animal:ferret:nose

URI:	jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
scheme:	jdbc
path:	mysql://test_user:ouupppssss@localhost:3306/sakila
query:	profileSQL=true

URI:	ftp://ftp.is.co.za/rfc/rfc1808.txt
scheme:	ftp
host:	ftp.is.co.za
port:	21
path:	/rfc/rfc1808.txt

URI:	http://www.ietf.org/rfc/rfc2396.txt#header1
scheme:	http
host:	www.ietf.org
port:	80
path:	/rfc/rfc2396.txt
frag:	header1

URI:	ldap://[2001:db8::7]/c=GB?objectClass?one
scheme:	ldap
host:	[2001:db8::7]
port:	389
path:	/c=GB
query:	objectClass?one

URI:	mailto:John.Doe@example.com
scheme:	mailto
path:	John.Doe@example.com

URI:	news:comp.infosystems.www.servers.unix
scheme:	news
port:	119
path:	comp.infosystems.www.servers.unix

URI:	tel:+1-816-555-1212
scheme:	tel
path:	+1-816-555-1212

URI:	telnet://192.0.2.16:80/
scheme:	telnet
host:	192.0.2.16
port:	80
path:	/

URI:	urn:oasis:names:specification:docbook:dtd:xml:4.1.2
scheme:	urn
path:	oasis:names:specification:docbook:dtd:xml:4.1.2

```



## Phix

There is a rudimentary and undocumented routine in pfile.e, parse_url().

```Phix
include builtins\pfile.e

sequence descs = repeat("",8)
    descs[URL_PROTOCOL]     = "scheme"
    descs[URL_HOSTNAME]     = "domain"
    descs[URL_PORT]         = "port"
    descs[URL_PATH]         = "path"
    descs[URL_USER]         = "user"
    descs[URL_PASSWORD]     = "password"
    descs[URL_QUERY_STRING] = "query"
    descs[URL_FRAGMENT]     = "fragment"

procedure show_url_details(string uri)
sequence r = parse_url(uri)
    ?uri
    for i=1 to length(descs) do
        if r[i]!=0 then
            printf(1,"%s : %s\n",{descs[i],sprint(r[i])})
        end if
    end for
    puts(1,"\n")
end procedure

constant tests = {
    "foo://example.com:8042/over/there?name=ferret#nose",
    "urn:example:animal:ferret:nose",
    "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
    "ftp://ftp.is.co.za/rfc/rfc1808.txt",
    "http://www.ietf.org/rfc/rfc2396.txt#header1",
    "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
    "mailto:John.Doe@example.com",
    "news:comp.infosystems.www.servers.unix",
    "tel:+1-816-555-1212",
    "telnet://192.0.2.16:80/",
    "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
    "ssh://alice@example.com",
    "https://bob:pass@example.com/place",
    "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"
}

for i=1 to length(tests) do
    show_url_details(tests[i])
end for
```

```txt

"foo://example.com:8042/over/there?name=ferret#nose"
scheme : "foo"
domain : "example.com"
port : 8042
path : "/over/there"
query : "name=ferret"
fragment : "nose"

"urn:example:animal:ferret:nose"
scheme : "urn"
path : "example:animal:ferret:nose"

"jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true"
scheme : "jdbc"
path : "mysql://test_user:ouupppssss@localhost:3306/sakila"
query : "profileSQL=true"

"ftp://ftp.is.co.za/rfc/rfc1808.txt"
scheme : "ftp"
domain : "ftp.is.co.za"
path : "/rfc/rfc1808.txt"

"http://www.ietf.org/rfc/rfc2396.txt#header1"
scheme : "http"
domain : "www.ietf.org"
path : "/rfc/rfc2396.txt"
fragment : "header1"

"ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two"
scheme : "ldap"
domain : "[2001:db8::7]"
path : "/c=GB"
query : "objectClass=one&objectClass=two"

"mailto:John.Doe@example.com"
scheme : "mailto"
path : "John.Doe@example.com"

"news:comp.infosystems.www.servers.unix"
scheme : "news"
path : "comp.infosystems.www.servers.unix"

"tel:+1-816-555-1212"
scheme : "tel"
path : "+1-816-555-1212"

"telnet://192.0.2.16:80/"
scheme : "telnet"
domain : "192.0.2.16"
port : 80
path : "/"

"urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
scheme : "urn"
path : "oasis:names:specification:docbook:dtd:xml:4.1.2"

"ssh://alice@example.com"
scheme : "ssh"
domain : "example.com"
user : "alice"

"https://bob:pass@example.com/place"
scheme : "https"
domain : "example.com"
path : "/place"
user : "bob"
password : "pass"

"http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"
scheme : "http"
domain : "example.com"
path : "/"
query : "a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"

```



## PowerShell

I was confused about the '''Path''' parameter.  PowerShell returns '''LocalPath''', '''AbsolutePath''' and '''AbsoluteUri'''; I defaulted to '''LocalPath''', but all properties are returned in the <code>$parsedUrls</code> variable.

```PowerShell

function Get-ParsedUrl
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [System.Uri]
        $InputObject
    )

    Process
    {
        foreach ($url in $InputObject)
        {
            $url | Select-Object -Property Scheme,
                                           @{Name="Domain"; Expression={$_.Host}},
                                           Port,
                                           @{Name="Path"  ; Expression={$_.LocalPath}},
                                           Query,
                                           Fragment,
                                           AbsolutePath,
                                           AbsoluteUri,
                                           Authority,
                                           HostNameType,
                                           IsDefaultPort,
                                           IsFile,
                                           IsLoopback,
                                           PathAndQuery,
                                           Segments,
                                           IsUnc,
                                           OriginalString,
                                           DnsSafeHost,
                                           IdnHost,
                                           IsAbsoluteUri,
                                           UserEscaped,
                                           UserInfo
        }
    }
}

```


```PowerShell

[string[]]$urls = @'
foo://example.com:8042/over/there?name=ferret#nose
urn:example:animal:ferret:nose
jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
ftp://ftp.is.co.za/rfc/rfc1808.txt
http://www.ietf.org/rfc/rfc2396.txt#header1
ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
mailto:John.Doe@example.com
news:comp.infosystems.www.servers.unix
tel:+1-816-555-1212
telnet://192.0.2.16:80/
urn:oasis:names:specification:docbook:dtd:xml:4.1.2
'@ -split [Environment]::NewLine

$parsedUrls = $urls | Get-ParsedUrl

$parsedUrls | Select-Object -Property Scheme, Port, Domain, Path, Query, Fragment | Format-Table

```

```txt

Scheme Port Domain        Path                                               Query                            Fragment
------ ---- ------        ----                                               -----                            --------
foo    8042 example.com   /over/there                                        ?name=ferret                     #nose
urn      -1               example:animal:ferret:nose
jdbc     -1               mysql://test_user:ouupppssss@localhost:3306/sakila ?profileSQL=true
ftp      21 ftp.is.co.za  /rfc/rfc1808.txt
http     80 www.ietf.org  /rfc/rfc2396.txt                                                                    #header1
ldap    389 [2001:db8::7] /c=GB                                              ?objectClass=one&objectClass=two
mailto   25 example.com
news     -1               comp.infosystems.www.servers.unix
tel      -1               +1-816-555-1212
telnet   80 192.0.2.16    /
urn      -1               oasis:names:specification:docbook:dtd:xml:4.1.2

```



## Python

Links to Python Documentation: v2: [https://docs.python.org/2/library/urlparse.html#module-urlparse], v3: [https://docs.python.org/3.4/library/urllib.parse.html]

```Python
import urllib.parse as up # urlparse for Python v2

url = up.urlparse('http://user:pass@example.com:8081/path/file.html;params?query1=1#fragment')

print('url.scheme = ', url.scheme)
print('url.netloc = ', url.netloc)
print('url.hostname = ', url.hostname)
print('url.port = ', url.port)
print('url.path = ', url.path)
print('url.params = ', url.params)
print('url.query = ', url.query)
print('url.fragment = ', url.fragment)
print('url.username = ', url.username)
print('url.password = ', url.password)

```

```txt

url.scheme =  http
url.netloc =  user:pass@example.com:8081
url.hostname =  example.com
url.port =  8081
url.path =  /path/file.html
url.params =  params
url.query =  query1=1
url.fragment =  fragment
url.username =  user
url.password =  pass

```


## R

urltools::url_parse() do all the actually work. The rest is just for nice output.

```rsplus

library(urltools)

urls <- c("foo://example.com:8042/over/there?name=ferret#nose",
          "urn:example:animal:ferret:nose",
          "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
          "ftp://ftp.is.co.za/rfc/rfc1808.txt",
          "http://www.ietf.org/rfc/rfc2396.txt#header1",
          "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
          "mailto:John.Doe@example.com",
          "news:comp.infosystems.www.servers.unix",
          "tel:+1-816-555-1212",
          "telnet://192.0.2.16:80/",
          "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
          "ssh://alice@example.com",
          "https://bob:pass@example.com/place",
          "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64")

for (an_url in urls) {
    parsed <- url_parse(an_url)
    cat(an_url,"\n")
    for (idx in 1:ncol(parsed)) {
      if (!is.na(parsed[[idx]])) {
        cat(colnames(parsed)[[idx]],"\t:",parsed[[idx]],"\n")
      }
    }
    cat("\n")
}

```

```txt

foo://example.com:8042/over/there?name=ferret#nose
scheme 	: foo
domain 	: example.com
port 	: 8042
path 	: over/there
parameter 	: name=ferret
fragment 	: nose

urn:example:animal:ferret:nose
domain 	: urn
port 	: example:animal:ferret:nose

jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
scheme 	: jdbc:mysql
domain 	: localhost
port 	: 3306
path 	: sakila
parameter 	: profileSQL=true

ftp://ftp.is.co.za/rfc/rfc1808.txt
scheme 	: ftp
domain 	: ftp.is.co.za
path 	: rfc/rfc1808.txt

http://www.ietf.org/rfc/rfc2396.txt#header1
scheme 	: http
domain 	: www.ietf.org
path 	: rfc/rfc2396.txt
fragment 	: header1

ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
scheme 	: ldap
path 	: c=GB
parameter 	: objectClass=one&objectClass=two

mailto:John.Doe@example.com
domain 	: example.com

news:comp.infosystems.www.servers.unix
domain 	: news
port 	: comp.infosystems.www.servers.unix

tel:+1-816-555-1212
domain 	: tel
port 	: +1-816-555-1212

telnet://192.0.2.16:80/
scheme 	: telnet
domain 	: 192.0.2.16
port 	: 80

urn:oasis:names:specification:docbook:dtd:xml:4.1.2
domain 	: urn
port 	: oasis:names:specification:docbook:dtd:xml:4.1.2

ssh://alice@example.com
scheme 	: ssh
domain 	: example.com

https://bob:pass@example.com/place
scheme 	: https
domain 	: example.com
path 	: place

http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64
scheme 	: http
domain 	: example.com
parameter 	: a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64

```



## Racket

Links: [http://docs.racket-lang.org/net/url.html?q=url#%28def._%28%28lib._net%2Furl-structs..rkt%29._url%29%29 <code>url</code> structure in Racket documentation].


```racket
#lang racket/base
(require racket/match net/url)
(define (debug-url-string U)
  (match-define (url s u h p pa? (list (path/param pas prms) ...) q f) (string->url U))
  (printf "URL: ~s~%" U)
  (printf "-----~a~%" (make-string (string-length (format "~s" U)) #\-))
  (when #t          (printf "scheme:         ~s~%" s))
  (when u           (printf "user:           ~s~%" u))
  (when h           (printf "host:           ~s~%" h))
  (when p           (printf "port:           ~s~%" p))
  ;; From documentation link in text:
  ;; > For Unix paths, the root directory is not included in `path';
  ;; > its presence or absence is implicit in the path-absolute? flag.
  (printf "path-absolute?: ~s~%" pa?)
  (printf "path  bits:     ~s~%" pas)
  ;; prms will often be a list of lists. this will print iff
  ;; one of the inner lists is not null
  (when (memf pair? prms)
    (printf "param bits:     ~s [interleaved with path bits]~%" prms))
  (unless (null? q) (printf "query:          ~s~%" q))
  (when f           (printf "fragment:       ~s~%" f))
  (newline))

(for-each
 debug-url-string
 '("foo://example.com:8042/over/there?name=ferret#nose"
   "urn:example:animal:ferret:nose"
   "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true"
   "ftp://ftp.is.co.za/rfc/rfc1808.txt"
   "http://www.ietf.org/rfc/rfc2396.txt#header1"
   "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two"
   "mailto:John.Doe@example.com"
   "news:comp.infosystems.www.servers.unix"
   "tel:+1-816-555-1212"
   "telnet://192.0.2.16:80/"
   "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"))
```


```txt
URL: "foo://example.com:8042/over/there?name=ferret#nose"
---------------------------------------------------------
scheme:         "foo"
host:           "example.com"
port:           8042
path-absolute?: #t
path  bits:     ("over" "there")
query:          ((name . "ferret"))
fragment:       "nose"

URL: "urn:example:animal:ferret:nose"
-------------------------------------
scheme:         "urn"
path-absolute?: #f
path  bits:     ("example:animal:ferret:nose")

URL: "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true"
------------------------------------------------------------------------------
scheme:         "jdbc"
path-absolute?: #f
path  bits:     ("mysql:" "" "test_user:ouupppssss@localhost:3306" "sakila")
query:          ((profileSQL . "true"))

URL: "ftp://ftp.is.co.za/rfc/rfc1808.txt"
-----------------------------------------
scheme:         "ftp"
host:           "ftp.is.co.za"
path-absolute?: #t
path  bits:     ("rfc" "rfc1808.txt")

URL: "http://www.ietf.org/rfc/rfc2396.txt#header1"
--------------------------------------------------
scheme:         "http"
host:           "www.ietf.org"
path-absolute?: #t
path  bits:     ("rfc" "rfc2396.txt")
fragment:       "header1"

URL: "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two"
----------------------------------------------------------------
scheme:         "ldap"
host:           "[2001"
path-absolute?: #f
path  bits:     ("db8::7]" "c=GB")
query:          ((objectClass . "one") (objectClass . "two"))
```

IPv6 URL address parses incorrectly. See issue https://github.com/plt/racket/issues/980

```txt
URL: "mailto:John.Doe@example.com"
----------------------------------
scheme:         "mailto"
path-absolute?: #f
path  bits:     ("John.Doe@example.com")

URL: "news:comp.infosystems.www.servers.unix"
---------------------------------------------
scheme:         "news"
path-absolute?: #f
path  bits:     ("comp.infosystems.www.servers.unix")

URL: "tel:+1-816-555-1212"
--------------------------
scheme:         "tel"
path-absolute?: #f
path  bits:     ("+1-816-555-1212")

URL: "telnet://192.0.2.16:80/"
------------------------------
scheme:         "telnet"
host:           "192.0.2.16"
port:           80
path-absolute?: #t
path  bits:     ("")

URL: "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
----------------------------------------------------------
scheme:         "urn"
path-absolute?: #f
path  bits:     ("oasis:names:specification:docbook:dtd:xml:4.1.2")
```




## Ruby

Link to [http://ruby-doc.org/stdlib/libdoc/uri/rdoc/URI.html Ruby Documentation].

As you can see in the output below, the URI library doesn't parse all of these as recommended.

```Ruby
require 'uri'

test_cases = [
  "foo://example.com:8042/over/there?name=ferret#nose",
  "urn:example:animal:ferret:nose",
  "jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true",
  "ftp://ftp.is.co.za/rfc/rfc1808.txt",
  "http://www.ietf.org/rfc/rfc2396.txt#header1",
  "ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two",
  "mailto:John.Doe@example.com",
  "news:comp.infosystems.www.servers.unix",
  "tel:+1-816-555-1212",
  "telnet://192.0.2.16:80/",
  "urn:oasis:names:specification:docbook:dtd:xml:4.1.2",
  "ssh://alice@example.com",
  "https://bob:pass@example.com/place",
  "http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64"
]

class URI::Generic; alias_method :domain, :host; end

test_cases.each do |test_case|
  puts test_case
  uri = URI.parse(test_case)
  %w[ scheme domain port path query fragment user password ].each do |attr|
    puts "  #{attr.rjust(8)} = #{uri.send(attr)}" if uri.send(attr)
  end
end
```

```txt
foo://example.com:8042/over/there?name=ferret#nose
    scheme = foo
    domain = example.com
      port = 8042
      path = /over/there
     query = name=ferret
  fragment = nose
urn:example:animal:ferret:nose
    scheme = urn
jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
    scheme = jdbc
ftp://ftp.is.co.za/rfc/rfc1808.txt
    scheme = ftp
    domain = ftp.is.co.za
      port = 21
      path = rfc/rfc1808.txt
http://www.ietf.org/rfc/rfc2396.txt#header1
    scheme = http
    domain = www.ietf.org
      port = 80
      path = /rfc/rfc2396.txt
  fragment = header1
ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
    scheme = ldap
    domain = [2001:db8::7]
      port = 389
      path = /c=GB
     query = objectClass=one&objectClass=two
mailto:John.Doe@example.com
    scheme = mailto
news:comp.infosystems.www.servers.unix
    scheme = news
tel:+1-816-555-1212
    scheme = tel
telnet://192.0.2.16:80/
    scheme = telnet
    domain = 192.0.2.16
      port = 80
      path = /
urn:oasis:names:specification:docbook:dtd:xml:4.1.2
    scheme = urn
ssh://alice@example.com
    scheme = ssh
    domain = example.com
      path =
      user = alice
https://bob:pass@example.com/place
    scheme = https
    domain = example.com
      port = 443
      path = /place
      user = bob
  password = pass
http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64
    scheme = http
    domain = example.com
      port = 80
      path = /
     query = a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64
```



## Scala


```Scala
import java.net.URI

object WebAddressParser extends App {

  parseAddress("foo://example.com:8042/over/there?name=ferret#nose")
  parseAddress("ftp://ftp.is.co.za/rfc/rfc1808.txt")
  parseAddress("http://example.com/?a=1&b=2+2&c=3&c=4&d=%65%6e%63%6F%64%65%64")
  parseAddress("http://www.ietf.org/rfc/rfc2396.txt#header1")
  parseAddress("https://bob:pass@example.com/place")
  parseAddress("jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true")
  parseAddress("ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two")
  parseAddress("ldap://[2001:db8::7]/c=GB?objectClass?one")
  parseAddress("mailto:John.Doe@example.com")
  parseAddress("news:comp.infosystems.www.servers.unix")
  parseAddress("ssh://alice@example.com")
  parseAddress("tel:+1-816-555-1212")
  parseAddress("telnet://192.0.2.16:80/")
  parseAddress("urn:example:animal:ferret:nose")
  parseAddress("urn:oasis:names:specification:docbook:dtd:xml:4.1.2")
  parseAddress("This is not a URI!")

  private def parseAddress(a: String): Unit = {
    print(f"Parsing $a%-72s")
    try {
      val u = new URI(a)
      print("\u2714\tscheme = " + u.getScheme)
      print("\tdomain = " + u.getHost)
      print("\tport = " + (if (-1 == u.getPort) "default" else u.getPort))
      print("\tpath = " + (if (u.getPath == null) u.getSchemeSpecificPart else u.getPath))
      print("\tquery = " + u.getQuery)
      println("\tfragment = " + u.getFragment)
    } catch { case ex: Throwable => println('\u2718') }
  }
}
```

{{Out}}See it in running in your browser by [https://scastie.scala-lang.org/GZdtfkhfRsa9QPKZQ7W4XQ Scastie (JVM)].

## Tcl


Tcllib's uri package already knows how to decompose many kinds of URIs.  The [http://core.tcl.tk/tcllib/dir?name=modules/uri implementation] is a a quite readable example of this kind of parsing.  For this task, we'll use it directly.

Schemes can be added with <tt>uri::register</tt>, but the rules for this task assume HTTP-style decomposition for unknown schemes, which is done below by reaching into the documented interfaces <tt>$::uri::schemes</tt> and <tt>uri::SplitHttp</tt>.

For some URI types (such as <tt>urn</tt>, <tt>news</tt>, <tt>mailto</tt>), this provides more information than the task description demands, which is simply to parse them all as HTTP URIs.

The <tt>uri</tt> package doesn't presently handle IPv6 syntx as used in the example:  a bug and patch will be submitted presently ..


```Tcl
package require uri
package require uri::urn

# a little bit of trickery to format results:
proc pdict {d} {
    array set \t $d
    parray \t
}

proc parse_uri {uri} {
    regexp {^(.*?):(.*)$} $uri -> scheme rest
    if {$scheme in $::uri::schemes} {
        # uri already knows how to split it:
        set parts [uri::split $uri]
    } else {
        # parse as though it's http:
        set parts [uri::SplitHttp $rest]
        dict set parts scheme $scheme
    }
    dict filter $parts value ?* ;# omit empty sections
}

set tests {
    foo://example.com:8042/over/there?name=ferret#nose
    urn:example:animal:ferret:nose
    jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
    ftp://ftp.is.co.za/rfc/rfc1808.txt
    http://www.ietf.org/rfc/rfc2396.txt#header1
    ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
    mailto:John.Doe@example.com
    news:comp.infosystems.www.servers.unix
    tel:+1-816-555-1212
    telnet://192.0.2.16:80/
    urn:oasis:names:specification:docbook:dtd:xml:4.1.2
}

foreach uri $tests {
    puts \n$uri
    pdict [parse_uri $uri]
}
```


```txt

foo://example.com:8042/over/there?name=ferret#nose
	(fragment) = nose
	(host)     = example.com
	(path)     = over/there
	(port)     = 8042
	(query)    = name=ferret
	(scheme)   = foo

urn:example:animal:ferret:nose
	(nid)    = example
	(nss)    = animal:ferret:nose
	(scheme) = urn

jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
	(path)   = mysql://test_user:ouupppssss@localhost:3306/sakila
	(query)  = profileSQL=true
	(scheme) = jdbc

ftp://ftp.is.co.za/rfc/rfc1808.txt
	(host)   = ftp.is.co.za
	(path)   = rfc/rfc1808.txt
	(scheme) = ftp

http://www.ietf.org/rfc/rfc2396.txt#header1
	(fragment) = header1
	(host)     = www.ietf.org
	(path)     = rfc/rfc2396.txt
	(scheme)   = http

ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
	(host)   = [2001
	(scheme) = ldap

mailto:John.Doe@example.com
	(host)   = example.com
	(scheme) = mailto
	(user)   = John.Doe

news:comp.infosystems.www.servers.unix
	(newsgroup-name) = comp.infosystems.www.servers.unix
	(scheme)         = news

tel:+1-816-555-1212
	(path)   = +1-816-555-1212
	(scheme) = tel

telnet://192.0.2.16:80/
	(host)   = 192.0.2.16
	(port)   = 80
	(scheme) = telnet

urn:oasis:names:specification:docbook:dtd:xml:4.1.2
	(nid)    = oasis
	(nss)    = names:specification:docbook:dtd:xml:4.1.2
	(scheme) = urn
```



## VBScript


```vb

Function parse_url(url)
	parse_url = "URL: " & url
	If InStr(url,"//") Then
		'parse the scheme
		scheme = Split(url,"//")
		parse_url = parse_url & vbcrlf & "Scheme: " & Mid(scheme(0),1,Len(scheme(0))-1)
		'parse the domain
		domain = Split(scheme(1),"/")
		'check if the domain includes a username, password, and port
		If InStr(domain(0),"@") Then
			cred = Split(domain(0),"@")
			If InStr(cred(0),".") Then
				username = Mid(cred(0),1,InStr(1,cred(0),".")-1)
				password = Mid(cred(0),InStr(1,cred(0),".")+1,Len(cred(0))-InStr(1,cred(0),"."))
			ElseIf InStr(cred(0),":") Then
				username = Mid(cred(0),1,InStr(1,cred(0),":")-1)
				password = Mid(cred(0),InStr(1,cred(0),":")+1,Len(cred(0))-InStr(1,cred(0),":"))
			End If
			parse_url = parse_url & vbcrlf & "Username: " & username & vbCrLf &_
				"Password: " & password
			'check if the domain have a port
			If InStr(cred(1),":") Then
				host = Mid(cred(1),1,InStr(1,cred(1),":")-1)
				port = Mid(cred(1),InStr(1,cred(1),":")+1,Len(cred(1))-InStr(1,cred(1),":"))
				parse_url = parse_url & vbCrLf & "Domain: " & host & vbCrLf & "Port: " & port
			Else
				parse_url = parse_url & vbCrLf & "Domain: " & cred(1)
			End If
		ElseIf InStr(domain(0),":") And Instr(domain(0),"[") = False And Instr(domain(0),"]") = False Then
				host = Mid(domain(0),1,InStr(1,domain(0),":")-1)
				port = Mid(domain(0),InStr(1,domain(0),":")+1,Len(domain(0))-InStr(1,domain(0),":"))
				parse_url = parse_url & vbCrLf & "Domain: " & host & vbCrLf & "Port: " & port
		ElseIf Instr(domain(0),"[") And Instr(domain(0),"]:") Then
			host = Mid(domain(0),1,InStr(1,domain(0),"]"))
			port = Mid(domain(0),InStr(1,domain(0),"]")+2,Len(domain(0))-(InStr(1,domain(0),"]")+1))
			parse_url = parse_url & vbCrLf & "Domain: " & host & vbCrLf & "Port: " & port
		Else
			parse_url = parse_url & vbCrLf & "Domain: " & domain(0)
		End If
		'parse the path if exist
		If UBound(domain) > 0 Then
			For i = 1 To UBound(domain)
				If i < UBound(domain) Then
					path = path & domain(i) & "/"
				ElseIf InStr(domain(i),"?") Then
					path = path & Mid(domain(i),1,InStr(1,domain(i),"?")-1)
					If InStr(domain(i),"#") Then
						query = Mid(domain(i),InStr(1,domain(i),"?")+1,InStr(1,domain(i),"#")-InStr(1,domain(i),"?")-1)
						fragment = Mid(domain(i),InStr(1,domain(i),"#")+1,Len(domain(i))-InStr(1,domain(i),"#"))
						path = path & vbcrlf & "Query: " & query & vbCrLf & "Fragment: " & fragment
					Else
						query = Mid(domain(i),InStr(1,domain(i),"?")+1,Len(domain(i))-InStr(1,domain(i),"?"))
						path = path & vbcrlf & "Query: " & query
					End If
				ElseIf InStr(domain(i),"#") Then
					fragment = Mid(domain(i),InStr(1,domain(i),"#")+1,Len(domain(i))-InStr(1,domain(i),"#"))
					path = path & Mid(domain(i),1,InStr(1,domain(i),"#")-1) & vbCrLf &_
						 "Fragment: " & fragment
				Else
					path = path & domain(i)
				End If
			Next
			parse_url = parse_url & vbCrLf & "Path: " & path
		End If
	ElseIf InStr(url,":") Then
		scheme = Mid(url,1,InStr(1,url,":")-1)
		path = Mid(url,InStr(1,url,":")+1,Len(url)-InStr(1,url,":"))
		parse_url = parse_url & vbcrlf & "Scheme: " & scheme & vbCrLf & "Path: " & path
	Else
		parse_url = parse_url & vbcrlf & "Invalid!!!"
	End If

End Function

'test the convoluted function :-(
WScript.StdOut.WriteLine parse_url("foo://example.com:8042/over/there?name=ferret#nose")
WScript.StdOut.WriteLine "-------------------------------"
WScript.StdOut.WriteLine parse_url("jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true")
WScript.StdOut.WriteLine "-------------------------------"
WScript.StdOut.WriteLine parse_url("ftp://ftp.is.co.za/rfc/rfc1808.txt")
WScript.StdOut.WriteLine "-------------------------------"
WScript.StdOut.WriteLine parse_url("http://www.ietf.org/rfc/rfc2396.txt#header1")
WScript.StdOut.WriteLine "-------------------------------"
WScript.StdOut.WriteLine parse_url("ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two")
WScript.StdOut.WriteLine "-------------------------------"
WScript.StdOut.WriteLine parse_url("mailto:John.Doe@example.com")
WScript.StdOut.WriteLine "-------------------------------"
WScript.StdOut.WriteLine parse_url("news:comp.infosystems.www.servers.unix")
WScript.StdOut.WriteLine "-------------------------------"
WScript.StdOut.WriteLine parse_url("tel:+1-816-555-1212")
WScript.StdOut.WriteLine "-------------------------------"
WScript.StdOut.WriteLine parse_url("telnet://192.0.2.16:80/")
WScript.StdOut.WriteLine "-------------------------------"
WScript.StdOut.WriteLine parse_url("urn:oasis:names:specification:docbook:dtd:xml:4.1.2")
WScript.StdOut.WriteLine "-------------------------------"
WScript.StdOut.WriteLine parse_url("this code is messy, long, and needs a makeover!!!")

```


```txt

URL: foo://example.com:8042/over/there?name=ferret#nose
Scheme: foo
Domain: example.com
Port: 8042
Path: over/there
Query: name=ferret
Fragment: nose
-------------------------------
URL: jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true
Scheme: jdbc:mysql
Username: test_user
Password: ouupppssss
Domain: localhost
Port: 3306
Path: sakila
Query: profileSQL=true
-------------------------------
URL: ftp://ftp.is.co.za/rfc/rfc1808.txt
Scheme: ftp
Domain: ftp.is.co.za
Path: rfc/rfc1808.txt
-------------------------------
URL: http://www.ietf.org/rfc/rfc2396.txt#header1
Scheme: http
Domain: www.ietf.org
Path: rfc/rfc2396.txt
Fragment: header1
-------------------------------
URL: ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two
Scheme: ldap
Domain: [2001:db8::7]
Path: c=GB
Query: objectClass=one&objectClass=two
-------------------------------
URL: mailto:John.Doe@example.com
Scheme: mailto
Path: John.Doe@example.com
-------------------------------
URL: news:comp.infosystems.www.servers.unix
Scheme: news
Path: comp.infosystems.www.servers.unix
-------------------------------
URL: tel:+1-816-555-1212
Scheme: tel
Path: +1-816-555-1212
-------------------------------
URL: telnet://192.0.2.16:80/
Scheme: telnet
Domain: 192.0.2.16
Port: 80
Path:
-------------------------------
URL: urn:oasis:names:specification:docbook:dtd:xml:4.1.2
Scheme: urn
Path: oasis:names:specification:docbook:dtd:xml:4.1.2
-------------------------------
URL: this code is messy, long, and needs a makeover!!!
Invalid!!!

```

