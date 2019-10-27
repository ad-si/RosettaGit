+++
title = "Rosetta Code/Rank languages by number of users"
description = ""
date = 2019-09-04T12:31:07Z
aliases = []
[extra]
id = 21688
[taxonomies]
categories = []
tags = []
+++

{{draft task}}[[Category:Text processing]][[Category:Networking and Web Interaction]][[Category:Sorting]][[Category:Rosetta Code related]]

Sort most popular programming languages based on the number of users on Rosetta Code. Show the languages with at least 100 users.

A way to solve the task:

Users of a language X are those referenced in the page https://rosettacode.org/wiki/Category:X_User, or preferably https://rosettacode.org/mw/index.php?title=Category:X_User&redirect=no to avoid redirections. In order to find the list of such categories, it's possible to first parse the entries of http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000. Then download and parse each language users category to count the users.

Sample output on 18 february 2019:


```txt
Language             Users
--------------------------
C                      391
Java                   276
C++                    275
Python                 262
JavaScript             238
Perl                   171
PHP                    167
SQL                    138
UNIX Shell             131
BASIC                  120
C sharp                118
Pascal                 116
Haskell                102
```


A Rosetta Code user usually declares using a language with the [[Template:Mylang|mylang]] template. This template is expected to appear on the User page. However, in some cases it appears in a user Talk page. It's not necessary to take this into account. For instance, among the 373 C users in the table above, 3 are actually declared in a Talk page.

__TOC__


## Go


```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "regexp"
    "sort"
    "strconv"
)

type Result struct {
    lang  string
    users int
}

func main() {
    const minimum = 25
    ex := `"Category:(.+?)( User)?"(\}|,"categoryinfo":\{"size":(\d+),)`
    re := regexp.MustCompile(ex)
    page := "http://rosettacode.org/mw/api.php?"
    action := "action=query"
    format := "format=json"
    fversion := "formatversion=2"
    generator := "generator=categorymembers"
    gcmTitle := "gcmtitle=Category:Language%20users"
    gcmLimit := "gcmlimit=500"
    prop := "prop=categoryinfo"
    rawContinue := "rawcontinue="
    page += fmt.Sprintf("%s&%s&%s&%s&%s&%s&%s&%s", action, format, fversion,
        generator, gcmTitle, gcmLimit, prop, rawContinue)
    resp, _ := http.Get(page)
    body, _ := ioutil.ReadAll(resp.Body)
    matches := re.FindAllStringSubmatch(string(body), -1)
    resp.Body.Close()
    var results []Result
    for _, match := range matches {
        if len(match) == 5 {
            users, _ := strconv.Atoi(match[4]) 
            if users >= minimum {
                result := Result{match[1], users}
                results = append(results, result)
            }
        }
    }
    sort.Slice(results, func(i, j int) bool {
        return results[j].users < results[i].users
    })

    fmt.Println("Rank  Users  Language")
    fmt.Println("----  -----  --------")
    rank := 0
    lastUsers := 0
    lastRank := 0
    for i, result := range results {
        eq := " "
        rank = i + 1
        if lastUsers == result.users {
            eq = "="
            rank = lastRank
        } else {
            lastUsers = result.users
            lastRank = rank
        }
        fmt.Printf(" %-2d%s   %3d    %s\n", rank, eq, result.users, result.lang)
    }
}
```


{{out}}

```txt

Rank  Users  Language
----  -----  --------
 1     397    C
 2     278    C++
 2 =   278    Java
 4     266    Python
 5     240    JavaScript
 6     171    Perl
 7     168    PHP
 8     139    SQL
 9     131    UNIX Shell
 10    121    C sharp
 11    120    BASIC
 12    118    Pascal
 13    102    Haskell
 14     93    Ruby
 15     81    Fortran
 16     70    Visual Basic
 17     65    Prolog
 18     61    Scheme
 19     59    Common Lisp
 20     58    AWK
 20=    58    Lua
 22     52    HTML
 23     46    Batch File
 24     45    Assembly
 24=    45    X86 Assembly
 26     43    Bash
 27     40    Erlang
 27=    40    MATLAB
 29     39    Lisp
 30     38    Forth
 31     36    Visual Basic .NET
 31=    36    Delphi
 33     35    APL
 33=    35    J
 35     34    Tcl
 35=    34    Brainf***
 37     33    Objective-C
 38     32    COBOL
 38=    32    R
 40     30    Go
 40=    30    Mathematica
 42     29    Perl 6
 43     27    Clojure
 44     25    OCaml
 44=    25    AutoHotkey
 44=    25    REXX

```



## Perl


```perl
use strict;
use warnings;
use JSON;
use URI::Escape;
use LWP::UserAgent;

my $client = LWP::UserAgent->new;
$client->agent("Rosettacode Perl task solver");
my $url = 'http://rosettacode.org/mw';
my $minimum = 100;

sub uri_query_string {
    my(%fields) = @_;
    'action=query&format=json&formatversion=2&' .
    join '&', map { $_ . '=' . uri_escape($fields{$_}) } keys %fields
}

sub mediawiki_query {
    my($site, $type, %query) = @_;
    my $url = "$site/api.php?" . uri_query_string(%query);
    my %languages = ();

    my $req = HTTP::Request->new( GET => $url );
    my $response = $client->request($req);
    $response->is_success or die "Failed to GET '$url': ", $response->status_line;
    my $data = decode_json($response->content);
    for my $row ( @{${$data}{query}{pages}} ) {
        next unless defined $$row{categoryinfo} && $$row{title} =~ /User/;
        my($title) = $$row{title} =~ /Category:(.*?) User/;
        my($count) = $$row{categoryinfo}{pages};
        $languages{$title} = $count;
    }
    %languages;
}

my %table = mediawiki_query(
    $url, 'pages',
    ( generator   => 'categorymembers',
      gcmtitle    => 'Category:Language users',
      gcmlimit    => '999',
      prop        => 'categoryinfo',
      rawcontinue => '',
    )
);

for my $k (sort { $table{$b} <=> $table{$a} } keys %table) {
    printf "%4d %s\n", $table{$k}, $k if $table{$k} > $minimum;
}
```

{{out}}

```txt
 397 C
 278 Java
 278 C++
 266 Python
 240 JavaScript
 171 Perl
 168 PHP
 139 SQL
 131 UNIX Shell
 121 C sharp
 120 BASIC
 118 Pascal
 102 Haskell
```



## Perl 6

{{works with|Rakudo|2017.11}}
Use the mediawiki API rather than web scraping since it is much faster and less resource intensive. Show languages with more than 25 users since that is still a pretty short list and to demonstrate how tied rankings are handled. Change the '''$minimum''' parameter to adjust what the cut-off point will be. 

This is all done in a single pass; ties are not detected until a language has the same count as a previous one, so ties are marked by a '''T''' next to the count indicating that '''this''' language has the same count as the '''previous'''.


```perl6
use HTTP::UserAgent;
use URI::Escape;
use JSON::Fast;

my $client = HTTP::UserAgent.new;

my $url = 'http://rosettacode.org/mw';

my $start-time = now;

say "
### ===
 Generated: { DateTime.new(time) } 
### ===
";

my $lang = 1;
my $rank = 0;
my $last = 0;
my $tie = ' ';
my $minimum = 25;

.say for
    mediawiki-query(
        $url, 'pages',
        :generator<categorymembers>,
        :gcmtitle<Category:Language users>,
        :gcmlimit<350>,
        :rawcontinue(),
        :prop<categoryinfo>
    )

    .map({ %( count => .<categoryinfo><pages> || 0,
              lang  => .<title>.subst(/^'Category:' (.+) ' User'/, ->$/ {$0}) ) })

    .sort( { -.<count>, .<lang> } )

    .map( { last if .<count> < $minimum; display(.<count>, .<lang>) } );

say "
### ===
 elapsed: {(now - $start-time).round(.01)} seconds 
### ===
";

sub display ($count, $which) {
    if $last != $count { $last = $count; $rank = $lang; $tie = ' ' } else { $tie = 'T' };
    sprintf "#%3d  Rank: %2d %s  with %-4s users:  %s", $lang++, $rank, $tie, $count, $which;
}

sub mediawiki-query ($site, $type, *%query) {
    my $url = "$site/api.php?" ~ uri-query-string(
        :action<query>, :format<json>, :formatversion<2>, |%query);
    my $continue = '';

    gather loop {
        my $response = $client.get("$url&$continue");
        my $data = from-json($response.content);
        take $_ for $data.<query>.{$type}.values;
        $continue = uri-query-string |($data.<query-continue>{*}».hash.hash or last);
    }
}

sub uri-query-string (*%fields) {
    join '&', %fields.map: { "{.key}={uri-escape .value}" }
}
```


{{out}}

```txt

### ===
 Generated: 2018-06-01T22:09:26Z 
### ===

#  1  Rank:  1    with 380  users:  C
#  2  Rank:  2    with 269  users:  Java
#  3  Rank:  3    with 266  users:  C++
#  4  Rank:  4    with 251  users:  Python
#  5  Rank:  5    with 234  users:  JavaScript
#  6  Rank:  6    with 167  users:  Perl
#  7  Rank:  7    with 166  users:  PHP
#  8  Rank:  8    with 134  users:  SQL
#  9  Rank:  9    with 125  users:  UNIX Shell
# 10  Rank: 10    with 119  users:  BASIC
# 11  Rank: 11    with 116  users:  C sharp
# 12  Rank: 12    with 112  users:  Pascal
# 13  Rank: 13    with 99   users:  Haskell
# 14  Rank: 14    with 93   users:  Ruby
# 15  Rank: 15    with 74   users:  Fortran
# 16  Rank: 16    with 67   users:  Visual Basic
# 17  Rank: 17    with 62   users:  Prolog
# 18  Rank: 18    with 61   users:  Scheme
# 19  Rank: 19    with 58   users:  Common Lisp
# 20  Rank: 20    with 55   users:  Lua
# 21  Rank: 21    with 53   users:  AWK
# 22  Rank: 22    with 52   users:  HTML
# 23  Rank: 23    with 46   users:  Assembly
# 24  Rank: 24    with 44   users:  Batch File
# 25  Rank: 25    with 42   users:  Bash
# 26  Rank: 25 T  with 42   users:  X86 Assembly
# 27  Rank: 27    with 40   users:  Erlang
# 28  Rank: 28    with 38   users:  Forth
# 29  Rank: 29    with 37   users:  MATLAB
# 30  Rank: 30    with 36   users:  Lisp
# 31  Rank: 31    with 35   users:  J
# 32  Rank: 31 T  with 35   users:  Visual Basic .NET
# 33  Rank: 33    with 34   users:  Delphi
# 34  Rank: 34    with 33   users:  APL
# 35  Rank: 34 T  with 33   users:  Ada
# 36  Rank: 34 T  with 33   users:  Brainf***
# 37  Rank: 34 T  with 33   users:  Objective-C
# 38  Rank: 34 T  with 33   users:  Tcl
# 39  Rank: 39    with 32   users:  R
# 40  Rank: 40    with 31   users:  COBOL
# 41  Rank: 41    with 30   users:  Go
# 42  Rank: 42    with 29   users:  Perl 6
# 43  Rank: 43    with 27   users:  Clojure
# 44  Rank: 43 T  with 27   users:  Mathematica
# 45  Rank: 45    with 25   users:  AutoHotkey

### ====== elapsed: 1.45 seconds ======

```



## Phix

See [[Rosetta_Code/Rank_languages_by_popularity#Phix|Rank languages by popularity]], just set output_users to true.
{{out}}

```txt

  1: 397 - C
  2: 278 - C++
  =: 278 - Java
  4: 266 - Python
  5: 240 - JavaScript
  6: 171 - Perl
  7: 168 - PHP
  8: 139 - SQL
  9: 131 - UNIX Shell
 10: 121 - C sharp
 11: 120 - BASIC
 12: 118 - Pascal
 13: 102 - Haskell
 14: 93 - Ruby
 15: 81 - Fortran
 16: 70 - Visual Basic
 17: 65 - Prolog
 18: 61 - Scheme
 19: 59 - Common Lisp
 20: 58 - AWK

```



## Racket


Note: the implementation is very similar to [[Rosetta_Code/Rank_languages_by_popularity#Racket|Rank languages by popularity]].


```racket
#lang racket
 
(require racket/hash
         net/url
         json)
 
(define limit 64)
(define (replacer cat) (regexp-replace #rx"^Category:(.*?) User$" cat "\\1"))
(define category "Category:Language users")
(define entries "users")
 
(define api-url (string->url "http://rosettacode.org/mw/api.php"))
(define (make-complete-url gcmcontinue)
  (struct-copy url api-url
               [query `([format . "json"]
                        [action . "query"]
                        [generator . "categorymembers"]
                        [gcmtitle . ,category]
                        [gcmlimit . "200"]
                        [gcmcontinue . ,gcmcontinue]
                        [continue . ""]
                        [prop . "categoryinfo"])]))

(define @ hash-ref)

(define table (make-hash))
 
(let loop ([gcmcontinue ""])
  (define resp (read-json (get-pure-port (make-complete-url gcmcontinue))))
  (hash-union! table
               (for/hash ([(k v) (in-hash (@ (@ resp 'query) 'pages))])
                 (values (@ v 'title #f) (@ (@ v 'categoryinfo (hash)) 'size 0))))
  (cond [(@ resp 'continue #f) => (λ (c) (loop (@ c 'gcmcontinue)))]))
 
(for/fold ([prev #f] [rank #f] #:result (void))
          ([item (in-list (sort (hash->list table) > #:key cdr))] [i (in-range limit)])
  (match-define (cons cat size) item)
  (define this-rank (if (equal? prev size) rank (add1 i)))
  (printf "Rank: ~a ~a ~a\n"
          (~a this-rank #:align 'right #:min-width 2)
          (~a (format "(~a ~a)" size entries) #:align 'right #:min-width 14)
          (replacer cat))
  (values size this-rank))
```


{{out}}

```txt

Rank:  1    (402 users) C
Rank:  2    (283 users) Java
Rank:  3    (281 users) C++
Rank:  4    (270 users) Python
Rank:  5    (243 users) JavaScript
Rank:  6    (175 users) Perl
Rank:  7    (171 users) PHP
Rank:  8    (142 users) SQL
Rank:  9    (134 users) UNIX Shell
Rank: 10    (123 users) C sharp
Rank: 10    (123 users) BASIC
Rank: 12    (119 users) Pascal
Rank: 13    (105 users) Haskell
Rank: 14     (94 users) Ruby
Rank: 15     (83 users) Fortran
Rank: 16     (71 users) Visual Basic
Rank: 17     (67 users) Prolog
Rank: 18     (63 users) Scheme
Rank: 19     (61 users) Common Lisp
Rank: 20     (59 users) AWK
Rank: 20     (59 users) Lua
Rank: 22     (52 users) HTML
Rank: 23     (46 users) X86 Assembly
Rank: 23     (46 users) Batch File
Rank: 23     (46 users) Assembly
Rank: 26     (44 users) Bash
Rank: 27     (40 users) Erlang
Rank: 27     (40 users) MATLAB
Rank: 29     (39 users) Forth
Rank: 29     (39 users) Lisp
Rank: 31     (37 users) Visual Basic .NET
Rank: 32     (36 users) APL
Rank: 32     (36 users) Tcl
Rank: 32     (36 users) Delphi
Rank: 35     (35 users) J
Rank: 36     (34 users) Brainf***
Rank: 37     (33 users) COBOL
Rank: 37     (33 users) Objective-C
Rank: 39     (32 users) Go
Rank: 39     (32 users) R
Rank: 41     (30 users) Mathematica
Rank: 42     (29 users) Perl 6
Rank: 43     (28 users) Clojure
Rank: 44     (25 users) OCaml
Rank: 44     (25 users) AutoHotkey
Rank: 44     (25 users) REXX
Rank: 47     (24 users) PostScript
Rank: 48     (23 users) Sed
Rank: 48     (23 users) Emacs Lisp
Rank: 48     (23 users) LaTeX
Rank: 51     (22 users) VBScript
Rank: 51     (22 users) CSS
Rank: 51     (22 users) MySQL
Rank: 51     (22 users) Scala
Rank: 55     (20 users) XSLT
Rank: 55     (20 users) Racket
Rank: 57     (19 users) 6502 Assembly
Rank: 58     (18 users) Z80 Assembly
Rank: 58     (18 users) Logo
Rank: 60     (17 users) Factor
Rank: 60     (17 users) Make
Rank: 60     (17 users) 8086 Assembly
Rank: 60     (17 users) F Sharp
Rank: 64     (16 users) PL/I

```



## Stata



```stata
copy "http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000" categ.html, replace
import delimited categ.html, delim("@") enc("utf-8") clear
keep if ustrpos(v1,"/wiki/Category:") & ustrpos(v1,"_User")
gen i = ustrpos(v1,"href=")
gen j = ustrpos(v1,char(34),i+1)
gen k = ustrpos(v1,char(34),j+1)
gen s = usubstr(v1,j+7,k-j-7)
replace i = ustrpos(v1,"title=")
replace j = ustrpos(v1,">",i+1)
replace k = ustrpos(v1," User",j+1)
gen lang = usubstr(v1,j+1,k-j)
keep s lang
gen users=.

forval i=1/`c(N)' {
	local s 
	preserve
	copy `"https://rosettacode.org/mw/index.php?title=`=s[`i']'&redirect=no"' `i'.html, replace
	import delimited `i'.html, delim("@") enc("utf-8") clear
	count if ustrpos(v1,"/wiki/User")
	local m `r(N)'
	restore
	replace users=`m' in `i'
	erase `i'.html
}

drop s
gsort -users lang
compress
leftalign
list in f/50
save rc_users, replace
```


'''Output''' (2019-02-18)


```txt
     +----------------------------+
     | lang                 users |
     |----------------------------|
  1. | C                      391 |
  2. | Java                   276 |
  3. | C++                    275 |
  4. | Python                 262 |
  5. | JavaScript             238 |
     |----------------------------|
  6. | Perl                   171 |
  7. | PHP                    167 |
  8. | SQL                    138 |
  9. | UNIX Shell             131 |
 10. | BASIC                  120 |
     |----------------------------|
 11. | C sharp                118 |
 12. | Pascal                 116 |
 13. | Haskell                102 |
 14. | Ruby                    93 |
 15. | Fortran                 79 |
     |----------------------------|
 16. | Visual Basic            68 |
 17. | Prolog                  65 |
 18. | Scheme                  61 |
 19. | Common Lisp             58 |
 20. | AWK                     57 |
     |----------------------------|
 21. | Lua                     57 |
 22. | HTML                    52 |
 23. | Assembly                45 |
 24. | Batch File              44 |
 25. | X86 Assembly            44 |
     |----------------------------|
 26. | Bash                    43 |
 27. | Erlang                  40 |
 28. | Lisp                    39 |
 29. | MATLAB                  39 |
 30. | Forth                   38 |
     |----------------------------|
 31. | Ada                     36 |
 32. | Visual Basic .NET       36 |
 33. | Delphi                  35 |
 34. | J                       35 |
 35. | APL                     34 |
     |----------------------------|
 36. | Brainf***               34 |
 37. | Tcl                     34 |
 38. | Objective-C             33 |
 39. | Smalltalk               33 |
 40. | COBOL                   32 |
     |----------------------------|
 41. | R                       32 |
 42. | Go                      30 |
 43. | Mathematica             30 |
 44. | Perl 6                  29 |
 45. | Clojure                 27 |
     |----------------------------|
 46. | AutoHotkey              25 |
 47. | REXX                    25 |
 48. | LaTeX                   23 |
 49. | OCaml                   23 |
 50. | Sed                     23 |
     +----------------------------+
```



## zkl

Uses libraries cURL and YAJL (yet another json library)

```zkl
const MIN_USERS=60;
var [const] CURL=Import("zklCurl"), YAJL=Import("zklYAJL")[0];

fcn rsGet{
   continueValue,r,curl := "",List, CURL();
   do{	// eg 5 times
      page:=("http://rosettacode.org/mw/api.php?action=query"
        "&generator=categorymembers&prop=categoryinfo"
	"&gcmtitle=Category%%3ALanguage%%20users"
	"&rawcontinue=&format=json&gcmlimit=350"
	"%s").fmt(continueValue);
      page=curl.get(page);
      page=page[0].del(0,page[1]);  // get rid of HTML header
      json:=YAJL().write(page).close();
      json["query"]["pages"].pump(r.append,'wrap(x){ x=x[1];
         //("2708",Dictionary(title:Category:C User,...,categoryinfo:D(pages:373,size:373,...)))
	 // or title:SmartBASIC
	 if((pgs:=x.find("categoryinfo")) and (pgs=pgs.find("pages")) and
	    pgs>=MIN_USERS) 
	   return(pgs,x["title"].replace("Category:","").replace(" User",""));
	   return(Void.Skip);
      });
      if(continueValue=json.find("query-continue",""))
        continueValue=String("&gcmcontinue=",
	   continueValue["categorymembers"]["gcmcontinue"]);
   }while(continueValue);
   r
}

allLangs:=rsGet();
allLangs=allLangs.sort(fcn(a,b){ a[0]>b[0] });
println("
### ====
 ",Time.Date.prettyDay()," 
### ====
");
foreach n,pgnm in ([1..].zip(allLangs))
   { println("#%3d with %4s users: %s".fmt(n,pgnm.xplode())) }
```

{{out}}

```txt


### ====
 Wednesday, the 20th of December 2017 
### ====

#  1 with  373 users: C
#  2 with  261 users: C++
#  3 with  257 users: Java
#  4 with  243 users: Python
#  5 with  228 users: JavaScript
#  6 with  163 users: PHP
#  7 with  162 users: Perl
#  8 with  131 users: SQL
#  9 with  120 users: UNIX Shell
# 10 with  118 users: BASIC
# 11 with  113 users: C sharp
# 12 with  109 users: Pascal
# 13 with   98 users: Haskell
# 14 with   91 users: Ruby
# 15 with   71 users: Fortran
# 16 with   65 users: Visual Basic
# 17 with   60 users: Scheme

```

