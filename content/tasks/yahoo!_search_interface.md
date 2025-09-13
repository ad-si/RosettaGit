+++
title = "Yahoo! search interface"
description = ""
date = 2018-06-11T13:32:45Z
aliases = []
[extra]
id = 4124
[taxonomies]
categories = ["task", "Programming environment operations"]
tags = []
+++

## Task

{{task|Programming environment operations}}[[Category:Networking and Web Interaction]]

Create a class for searching Yahoo! results.

It must implement a '''Next Page''' method, and read URL, Title and Content from results.





## AutoHotkey

translated from python example

```AutoHotkey
test:
yahooSearch("test", 1)
yahooSearch("test", 2)
return

yahooSearch(query, page)
{
  global
  start := ((page - 1) * 10) + 1
  filedelete, search.txt
  urldownloadtofile, % "http://search.yahoo.com/search?p=" . query
  . "&b=" . start, search.txt
  fileread, content, search.txt
  reg = <a class="yschttl spt" href=".+?" >(.+?)</a></h3></div><div class="abstr">(.+?)</div><span class=url>(.+?)</span>

  index := found := 1
  while (found := regexmatch(content, reg, self, found + 1))
  {
    msgbox % title%A_Index% := fix(self1)
    content%A_Index% := fix(self2)
    url%A_Index% := fix(self3)
  }
}

fix(url)
{
if pos := instr(url, "</a></h3></div>")
StringLeft, url, url, pos - 1
url := regexreplace(url, "<.*?>")
return url
}
```



## C#

Generally it is not a good idea to scrape web pages.
E. g. all implementations for this task which regex for
"<a class=" fail by now, after Yahoo has changed its output format.

```c#
using System;
using System.Net;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class YahooSearch {
    private string query;
    private string content;
    private int page;

    const string yahoo = "http://search.yahoo.com/search?";

    public YahooSearch(string query) : this(query, 0) { }

    public YahooSearch(string query, int page) {
        this.query = query;
        this.page = page;
        this.content = new WebClient()
            .DownloadString(
                string.Format(yahoo + "p={0}&b={1}", query, this.page * 10 + 1)
            );
    }

    public YahooResult[] Results {
        get {
            List<YahooResult> results = new List<YahooResult>();

            Func<string, string, string> substringBefore = (str, before) =>
            {
                int iHref = str.IndexOf(before);
                return iHref < 0 ? "" : str.Substring(0, iHref);
            };
            Func<string, string, string> substringAfter = (str, after) =>
            {
                int iHref = str.IndexOf(after);
                return iHref < 0 ? "" : str.Substring(iHref + after.Length);
            };
            Converter<string, string> getText = p =>
                Regex.Replace(p, "<[^>]*>", x => "");

            Regex rx = new Regex(@"
                <li>
                    <div \s class=""res"">
                        <div>
                            <h3>
                                <a \s (?'LinkAttributes'[^>]+)>
                                    (?'LinkText' .*?)
                                (?></a>)
                            </h3>
                        </div>
                        <div \s class=""abstr"">
                            (?'Abstract' .*?)
                        (?></div>)
                        .*?
                    (?></div>)
                </li>",
                RegexOptions.IgnorePatternWhitespace
                | RegexOptions.ExplicitCapture
            );
            foreach (Match e in rx.Matches(this.content)) {
                string rurl = getText(substringBefore(substringAfter(
                    e.Groups["LinkAttributes"].Value, @"href="""), @""""));
                string rtitle = getText(e.Groups["LinkText"].Value);
                string rcontent = getText(e.Groups["Abstract"].Value);

                results.Add(new YahooResult(rurl, rtitle, rcontent));
            }
            return results.ToArray();
        }
    }

    public YahooSearch NextPage() {
        return new YahooSearch(this.query, this.page + 1);
    }

    public YahooSearch GetPage(int page) {
        return new YahooSearch(this.query, page);
    }
}

class YahooResult {
    public string URL { get; set; }
    public string Title { get; set; }
    public string Content { get; set; }

    public YahooResult(string url, string title, string content) {
        this.URL = url;
        this.Title = title;
        this.Content = content;
    }

    public override string ToString()
    {
        return string.Format("\nTitle: {0}\nLink:  {1}\nText:  {2}",
            Title, URL, Content);
    }
}

// Usage:

class Prog {
    static void Main() {
        foreach (int page in new[] { 0, 1 })
        {
            YahooSearch x = new YahooSearch("test", page);

            foreach (YahooResult result in x.Results)
            {
                Console.WriteLine(result);
            }
        }
    }
}

```



## D

```d
import std.stdio, std.exception, std.regex, std.algorithm, std.string,
       std.net.curl;

struct YahooResult {
    string url, title, content;

    string toString() const {
        return "\nTitle: %s\nLink:  %s\nText:  %s"
               .format(title, url, content);
    }
}

struct YahooSearch {
    private string query, content;
    private uint page;

    this(in string query_, in uint page_ = 0) {
        this.query = query_;
        this.page = page_;
        this.content = "http://search.yahoo.com/search?p=%s&b=%d"
                       .format(query, page * 10 + 1).get.assumeUnique;
    }

    @property results() const {
        immutable re = `<li>
                          <div \s class="res">
                            <div>
                              <h3>
                                <a \s (?P<linkAttributes> [^>]+)>
                                  (?P<linkText> .*?)
                                </a>
                              </h3>
                            </div>
                            <div \s class="abstr">
                              (?P<abstract> .*?)
                            </div>
                            .*?
                          </div>
                        </li>`;

        const clean = (string s) => s.replace("<[^>]*>".regex("g"),"");

        return content.match(re.regex("gx")).map!(m => YahooResult(
            clean(m.captures["linkAttributes"]
                  .findSplitAfter(`href="`)[1]
                  .findSplitBefore(`"`)[0]),
            clean(m.captures["linkText"]),
            clean(m.captures["abstract"])
        ));
    }

    YahooSearch nextPage() const {
        return YahooSearch(query, page + 1);
    }
}

void main() {
    writefln("%(%s\n%)", "test".YahooSearch.results);
}
```

```txt

Title: Test.com
Link:  http://www.test.com/
Text:  Test.com provides a complete software solution for creating online tests and managing enterprise and specialist certification programs, in up to 22 languages.

Title: Speakeasy Speed Test
Link:  http://www.speakeasy.net/speedtest/
Text:  Test your Internet Connection with Speakeasy&#39;s reliable and accurate broadband speed test. What&#39;s your speed?

Title: Test | Define Test at Dictionary.com
Link:  http://dictionary.reference.com/browse/test
Text:  noun 1. the means by which the presence, quality, or genuineness of anything is determined; a means of trial. 2. the trial of the quality of something: to put to the ...


```



## Gambas


```gambas
Public Sub Form_Open()
Dim hWebView As WebView

Me.Arrangement = Arrange.Fill
Me.Maximized = True
Me.Title = "Yahoo! search interface"

hWebView = New WebView(Me)
hWebView.Expand = True
hWebView.URL = "https://www.yahoo.com"

End
```

'''[http://www.cogier.com/gambas/Yahoo!%20search%20interface.png Click here to see output (I have typed 'rosettacode' in the search box)]'''


## GUISS



```guiss
Start,Programs,Applications,Mozilla Firefox,Inputbox:address bar>www.yahoo.co.uk,
Button:Go,Area:browser window,Inputbox:searchbox>elephants,Button:Search
```



## Haskell

Haskell is not an object oriented language, so this example does not implement an object class.
However, it can be interesting as an example of how HTML source code can be parsed using the Parsec library.

```Haskell
import Network.HTTP
import Text.Parsec

data YahooSearchItem = YahooSearchItem {
    itemUrl, itemTitle, itemContent :: String }

data YahooSearch = YahooSearch {
    searchQuery :: String,
    searchPage :: Int,
    searchItems :: [YahooSearchItem] }

-- URL for Yahoo! searches, without giving a page number
yahooUrl = "http://search.yahoo.com/search?p="

-- make an HTTP request and return a YahooSearch
yahoo :: String -> IO YahooSearch
yahoo q = simpleHTTP (getRequest $ yahooUrl ++ q) >>=
    getResponseBody >>= return . YahooSearch q 1 . items

-- get some results and return the next page of results
next :: YahooSearch -> IO YahooSearch
next (YahooSearch q p _) =
    simpleHTTP (getRequest $
    -- add the page number to the search
    yahooUrl ++ q ++ "&b=" ++ show (p + 1)) >>=
    getResponseBody >>= return . YahooSearch q (p + 1) . items

printResults :: YahooSearch -> IO ()
printResults (YahooSearch q p items) = do
    putStrLn $ "Showing Yahoo! search results for query: " ++ q
    putStrLn $ "Page: " ++ show p
    putChar '\n'
    mapM_ printOne items
    where
        printOne (YahooSearchItem itemUrl itemTitle itemContent) = do
            putStrLn $ "URL   : " ++ itemUrl
            putStrLn $ "Title : " ++ itemTitle
            putStrLn $ "Abstr : " ++ itemContent
            putChar '\n'

urlTag, titleTag, contentTag1, contentTag2, ignoreTag,
    ignoreText :: Parsec String () String

-- parse a tag containing the URL of a search result
urlTag = do { string "<a id=\"link-";
    many digit; string "\" class=\"yschttl spt\" href=\"";
    url <- manyTill anyChar (char '"'); manyTill anyChar (char '>');
    return url }

-- the title comes after the URL tag, so parse it first, discard it
-- and get the title text
titleTag = do { urlTag; manyTill anyChar (try (string "</a>")) }

-- parse a tag containing the description of the search result
-- the tag can be named "sm-abs" or "abstr"
contentTag1 = do { string "<div class=\"sm-abs\">";
    manyTill anyChar (try (string "</div>")) }

contentTag2 = do { string "<div class=\"abstr\">";
    manyTill anyChar (try (string "</div>")) }

-- parse a tag and discard it
ignoreTag = do { char ('<'); manyTill anyChar (char '>');
    return "" }

-- parse some text and discard it
ignoreText = do { many1 (noneOf "<"); return "" }

-- return only non-empty strings
nonempty :: [String] -> Parsec String () [String]
nonempty xs = return [ x | x <- xs, not (null x) ]

-- a template to parse a whole source file looking for items of the
-- same class
parseCategory x = do
    res <- many x
    eof
    nonempty res

urls, titles, contents :: Parsec String () [String]

-- parse HTML source looking for URL tags of the search results
urls = parseCategory url where
    url = (try urlTag) <|> ignoreTag <|> ignoreText

-- parse HTML source looking for titles of the search results
titles = parseCategory title where
    title = (try titleTag) <|> ignoreTag <|> ignoreText

-- parse HTML source looking for descriptions of the search results
contents = parseCategory content where
    content = (try contentTag1) <|> (try contentTag2) <|>
        ignoreTag <|> ignoreText

-- parse the HTML source three times looking for URL, title and
-- description of all search results and return them as a list of
-- YahooSearchItem
items :: String -> [YahooSearchItem]
items q =
    let ignoreOrKeep = either (const []) id
        us = ignoreOrKeep $ parse urls "" q
        ts = ignoreOrKeep $ parse titles "" q
        cs = ignoreOrKeep $ parse contents "" q
    in [ YahooSearchItem { itemUrl = u, itemTitle = t, itemContent = c } |
        (u, t, c) <- zip3 us ts cs ]

```

Simple invocation from GHCi:
```txt
yahoo "Rosetta%20code" >>= printResults
```
. Notice that spaces must be expressed as "%20", because spaces are not allowed in URLs.
==Icon and {{header|Unicon}}==
The following uses the Unicon pre-processor and messaging extensions and won't run under Icon without significant modification.
The code provides a suitable demonstration; however, could be made more robust by things such as URL escaping the search string

```Icon
link printf,strings

procedure main()
YS := YahooSearch("rosettacode")
every 1 to 2 do {   # 2 pages
   YS.readnext()
   YS.showinfo()
   }
end

class YahooSearch(urlpat,page,response)  #: class for Yahoo Search

   method readnext()    #: read the next page of search results
      self.page +:= 1   # can't find as w|w/o self
      readurl()
   end

   method readurl()     #: read the url
      url := sprintf(self.urlpat,(self.page-1)*10+1)
      m := open(url,"m")  | stop("Unable to open : ",url)
      every (self.response := "") ||:= |read(m)
      close(m)
      self.response := deletec(self.response,"\x00") # kill stray NULs
   end

   method showinfo()    #: show the info of interest
      self.response ? repeat {
         (tab(find("<")) & ="<a class=\"yschttl spt\" href=\"") | break
         url   := tab(find("\"")) & tab(find(">")+1)
         title := tab(find("<")) & ="</a></h3></div>"
         tab(find("<")) & =("<div class=\"abstr\">" | "<div class=\"sm-abs\">")
         abstr := tab(find("<")) & ="</div>"

         printf("\nTitle : %i\n",title)
         printf("URL   : %i\n",url)
         printf("Abstr : %i\n",abstr)
         }
   end

initially(searchtext)    #: initialize each instance
   urlpat := sprintf("http://search.yahoo.com/search?p=%s&b=%%d",searchtext)
   page := 0
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]
[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings.icn provides deletec]

Sample Output (truncated):
```txt

Title : "<b>Rosetta Code</b> - <b>Rosetta Code</b>"
URL   : "http://rosettacode.org/"
Abstr : "<b>Rosetta Code</b> is a programming chrestomathy site. The idea is to
present solutions to the same task in as many different languages as possible, t
o demonstrate how ..."

Title : "<b>Rosetta Code</b> - Wikipedia, the free <wbr />encyclopedia"
URL   : "http://en.wikipedia.org/wiki/Rosetta_Code"
Abstr : " <b>Rosetta Code</b> is a wiki -based programming chrestomathy website
with solutions to various programming problems in many different programming lan
guages. It was created ..."

Title : "Category:AutoHotkey - <b>Rosetta Code</b>"
URL   : "http://rosettacode.org/wiki/Category:AutoHotkey"
Abstr : "Listed below are all of the tasks on <b>Rosetta Code</b> which have bee
n solved using AutoHotkey."

...


Title : "RosettaCON2011 Tutorials Collection | <wbr />RosettaCommons"
URL   : "http://www.rosettacommons.org/"
Abstr : "Foldit in the news. Cooper et al. 2010 Predicting protein structures wi
th a multiplayer online game, Nature 466 , 756 see also video. Rosetta-3.3 is no
w available!"

Title : "CALL: call a SUBROUTINE - HicEst: <wbr />Windows IDE programming ..."
URL   : "http://www.hicest.com/CALL.htm"
Abstr : "\xe2\x87\x92 Example of a CALL call in &quot;Roman_numerals&quot; (<b>R
osettaCode</b>) CALL transfers control to the first statement of a SUBROUTINE. C
ALL subroutine_name[argument1, ..."
```


## Java

```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class YahooSearch {
    private String query;
    // Page number
    private int page = 1;
    // Regexp to look for the individual results in the returned page
    private static final Pattern pattern = Pattern.compile(
        "<a class=\"yschttl spt\" href=\"[^*]+?\\*\\*([^\"]+?)\">(.+?)</a></h3>.*?<div class=\"(?:sm-abs|abstr)\">(.+?)</div>");

    public YahooSearch(String query) {
        this.query = query;
    }

    public List<YahooResult> search() throws MalformedURLException, URISyntaxException, IOException {
        // Build the search string, starting with the Yahoo search URL,
        // then appending the query and optionally the page number (if > 1)
        StringBuilder searchUrl = new StringBuilder("http://search.yahoo.com/search?");
        searchUrl.append("p=").append(URLEncoder.encode(query, "UTF-8"));
        if (page > 1) {searchUrl.append("&b=").append((page - 1) * 10 + 1);}
        // Query the Yahoo search engine
        URL url = new URL(searchUrl.toString());
        List<YahooResult> result = new ArrayList<YahooResult>();
        StringBuilder sb = new StringBuilder();
        // Get the search results using a buffered reader
        BufferedReader in = null;
        try {
            in = new BufferedReader(new InputStreamReader(url.openStream()));
            // Read the results line by line
            String line = in.readLine();
            while (line != null) {
                sb.append(line);
                line = in.readLine();
            }
        }
        catch (IOException ioe) {
            ioe.printStackTrace();
        }
        finally {
            try {in.close();} catch (Exception ignoreMe) {}
        }
        String searchResult = sb.toString();
        // Look for the individual results by matching the regexp pattern
        Matcher matcher = pattern.matcher(searchResult);
        while (matcher.find()) {
            // Extract the result URL, title and excerpt
            String resultUrl = URLDecoder.decode(matcher.group(1), "UTF-8");
            String resultTitle = matcher.group(2).replaceAll("</?b>", "").replaceAll("<wbr ?/?>", "");
            String resultContent = matcher.group(3).replaceAll("</?b>", "").replaceAll("<wbr ?/?>", "");
            // Create a new YahooResult and add to the list
            result.add(new YahooResult(resultUrl, resultTitle, resultContent));
        }
        return result;
    }

    public List<YahooResult> search(int page) throws MalformedURLException, URISyntaxException, IOException {
        // Set the page number and search
        this.page = page;
        return search();
    }

    public List<YahooResult> nextPage() throws MalformedURLException, URISyntaxException, IOException {
        // Increment the page number and search
        page++;
        return search();
    }

    public List<YahooResult> previousPage() throws MalformedURLException, URISyntaxException, IOException {
        // Decrement the page number and search; if the page number is 1 return an empty list
        if (page > 1) {
            page--;
            return search();
        } else return new ArrayList<YahooResult>();
    }
}

class YahooResult {
    private URL url;
    private String title;
    private String content;

    public URL getUrl() {
        return url;
    }

    public void setUrl(URL url) {
        this.url = url;
    }

    public void setUrl(String url) throws MalformedURLException {
        this.url = new URL(url);
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public YahooResult(URL url, String title, String content) {
        setUrl(url);
        setTitle(title);
        setContent(content);
    }

    public YahooResult(String url, String title, String content) throws MalformedURLException {
        setUrl(url);
        setTitle(title);
        setContent(content);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (title != null) {
            sb.append(",title=").append(title);
        }
        if (url != null) {
            sb.append(",url=").append(url);
        }
        return sb.charAt(0) == ',' ? sb.substring(1) : sb.toString();
    }
}

public class TestYahooSearch {
    public static void main(String[] args) throws MalformedURLException, URISyntaxException, IOException {
        // Create a new search
        YahooSearch search = new YahooSearch("Rosetta code");
        // Get the search results
        List<YahooResult> results = search.search();
        // Show the search results
        for (YahooResult result : results) {
            System.out.println(result.toString());
        }
    }
}
```


## Kotlin

This is based on the C# entry but uses a regular expression based on what appears to be the Yahoo! format as at the date of this entry (4 December 2017).

```scala
// version 1.2.0

import java.net.URL

val rx = Regex("""<div class=\"yst result\">.+?<a href=\"(.*?)\" class=\"\">(.*?)</a>.+?class="abstract ellipsis">(.*?)</p>""")

class YahooResult(var title: String, var link: String, var text: String) {

    override fun toString() = "\nTitle: $title\nLink : $link\nText : $text"
}

class YahooSearch(val query: String, val page: Int = 0) {

    private val content: String

    init {
        val yahoo = "http://search.yahoo.com/search?"
        val url = URL("${yahoo}p=$query&b=${page * 10 + 1}")
        content = url.readText()
    }

    val results: MutableList<YahooResult>
        get() {
            val list = mutableListOf<YahooResult>()
            for (mr in rx.findAll(content)) {
               val title = mr.groups[2]!!.value.replace("<b>", "").replace("</b>", "")
               val link  = mr.groups[1]!!.value
               val text  = mr.groups[3]!!.value.replace("<b>", "").replace("</b>", "")
               list.add (YahooResult(title, link, text))
            }
            return list
        }

    fun nextPage() = YahooSearch(query, page + 1)

    fun getPage(newPage: Int) = YahooSearch(query, newPage)
}

fun main(args: Array<String>) {
    for (page in 0..1) {
        val x = YahooSearch("rosettacode", page)
        println("\nPAGE ${page + 1} =>")
        for (result in x.results.take(3)) println(result)
    }
}
```

Output (restricted to first three results on first two pages):

```txt
PAGE 1 =>

Title: Rosetta Code - Official Site
Link : http://rosettacode.org/wiki/Rosetta_Code
Text : Rosetta Code is a programming chrestomathy site. The idea is to present solutions to the same task in as ...

Title: Rosetta Code - Wikipedia
Link : https://en.wikipedia.org/wiki/Rosetta_Code
Text : Rosetta Code is a wiki-based programming chrestomathy website with implementations of common algorithms ...

Title: Rosetta Code (@rosettacode) | Twitter
Link : https://twitter.com/rosettacode
Text : The latest Tweets from Rosetta Code (@rosettacode). Twitter account for http://t.co/DuRZFWDfRn. The ...

PAGE 2 =>

Title: Rosetta Code Blog
Link : http://blog.rosettacode.org/
Text : As I noted, there was an expectation of downtime as the VPS hostRosetta Code sits on moved from one data ...

Title: Rosetta Code - Wikipedia
Link : https://en.wikipedia.org/wiki/User:Paddy3118/Rosetta_Code
Text : Rosetta Code is a wiki-based programming chrestomathy website with implementations of common algorithms ...

Title: Rosetta Code and ABAP | SAP Blogs
Link : https://blogs.sap.com/2015/03/27/rosetta-code-and-abap/
Text : Last week Christian Drumm (@ceedee666) and Fred Verheul (@fredverheul) had a short conversation on ...
```



## Mathematica

We cannot define a class in Mathematica, so I generate a "Manipulate" object instead.
<lang>Manipulate[
 Column[Flatten[
   StringCases[
    StringCases[
     URLFetch[
      "http://search.yahoo.com/search?p=" <> query <> "&b=" <>
       ToString@page], "<ol" ~~ ___ ~~ "</ol>"],
    "<a" ~~ Shortest[__] ~~ "class=\"yschttl spt\" href=\"" ~~
      Shortest[url__] ~~ "\"" ~~ Shortest[__] ~~ ">" ~~
      Shortest[title__] ~~
      "<div class=\"abstr\">" | "<div class=\"sm-abs\">" ~~
      Shortest[abstr__] ~~ "</div>" :>
     Column[{Hyperlink[Style[#[[1]], Larger], #[[2]]], #[[3]],
         Style[#[[2]], Smaller]} &@
       StringReplace[{title, url,
         abstr}, {"<" ~~ Shortest[__] ~~ ">" -> "",
         "&#" ~~ n : DigitCharacter ... ~~ ";" :>
          FromCharacterCode[FromDigits@n], "&amp;" -> "&",
         "&quot;" -> "\"", "&lt;" -> "<", "&gt;" -> ">"}]]], 1],
  Spacings -> 2], {{input, "", "Yahoo!"},
  InputField[Dynamic@input, String] &}, {{query, ""},
  ControlType -> None}, {{page, 1}, ControlType -> None},
 Row[{Button["Search", page = 1; query = input],
   Button["Prev", page -= 10, Enabled -> Dynamic[page >= 10]],
   Button["Next", page += 10]}]]
```



## Oz

Instead of a class the implementation defines a function which returns a lazy list of result pages. This also makes it possible to request e.g. the first and the third page without any resources wasted on an unneeded second page.

We implement some simple parsing with logic programming. Regular expressions in Oz don't seem to support lazy quantification which makes parsing the result pages with them difficult.

```oz
declare
  [HTTPClient] = {Module.link ['x-ozlib://mesaros/net/HTTPClient.ozf']}
  [StringX] = {Module.link ['x-oz://system/String.ozf']}
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  %% Displays page 1 and 3 of the search results.
  %% The user can request and display more with context menu->Actions->Make Needed.
  proc {ExampleUsage}
     Pages = {YahooSearch "Rosetta code"}
  in
     {Inspector.configure widgetShowStrings true}
     {ForAll {Nth Pages 1} Value.makeNeeded}
     {ForAll {Nth Pages 3} Value.makeNeeded}
     %% Display the infinite list of search result pages.
     {Inspect Pages}
  end

  %% Returns a lazy list of pages.
  %% A page is a lazy list of entries like this: result(url:U title:T content:C).
  fun {YahooSearch Query}
     FetchURL = {CreateURLFetcher}

     fun {Page Nr}
	StartResult = (Nr-1)*10+1
	%% only retrieve it when really needed
	Doc = {Value.byNeed fun {$}
			       {FetchURL "http://search.yahoo.com/search"
				["p"#Query "b"#{Int.toString StartResult}]}
			    end}
	RE = "<a class=\"yschttl spt\" href="
     in
	%% Lazily returns results.
	%% In this way it is possible to build the pages list structure
	%% without creating the single elements
	%% (e.g. retrieve page 1 and 3 but not 2).
	for Match in {Regex.allMatches RE Doc} yield:Yield do
	   Xs = {List.drop Doc Match.0.2}
	in
	   {Yield {ParseEntry Xs}}
	end
     end
  in
     for PageNr in 1;PageNr+1 yield:Yield do
	{Yield {Page PageNr}}
     end
  end

  fun {CreateURLFetcher}
     Client = {New HTTPClient.cgiGET
	       init(inPrms(toFile:false toStrm:true)
		    httpReqPrms
		   )}
     %% close when no longer used
     {Finalize.register Client proc {$ C} {C closeAll(true)} end}

     fun {FetchURL Url Params}
	OutParams
     in
	{Client getService(Url Params ?OutParams ?_)}
	OutParams.sOut
     end
  in
     FetchURL
  end

  %% Xs: String containing HtmL
  %% Result: "result(url:U title:T content:C)" or "parseError"
  fun {ParseEntry Xs}
     proc {Parse Root}
	R1 R2 R3 R4 R4 R5 R6 R7
	Url = {Fix {QuotedString Xs R1}}
	{Const ">" R1 R2}
	Title = {Fix {Until "</a>" R2 R3}}
	{Const "</h3></div>" R3 R4}
	choice
	   %% "enchanted" result?
	   {Const "<div class=\"sm-bd sm-nophoto\" id=\"sm-bd-4-1\">" R4 R5}
	   {Until "</div>" R5 R6 _}
	[] %% result with links into document
	   {Const "<div class=\"sm-bd sm-r\" id=\"sm-bd-8-1\">" R4 R5}
	   {Until "</ul></div>" R5 R6 _}
	[] %% PDF file
	   {Const "<div class=\"format\">" R4 R5}
	   {Until "</a></div>" R5 R6 _}
	[] %% With Review
	   {Const "<div class=\"sm-bd sm-r\" id=\"sm-bd-9-1\">" R4 R5}
	   R6 = nil %% no nice abstract when a review is there
	[] %% normal result
	   R6 = R4
	end
	Abstract =
	choice
	   {Const "<div class=\"abstr\">" R6 R7}
	   {Fix {Until "</div>" R7 _}}
	[] {Const "<div class=\"sm-abs\">" R6 R7}
	   {Fix {Until "</div>" R7 _}}
	[] ""
	end
     in
	Root = result(url:Url title:Title content:Abstract)
     end
  in
     {CondSelect {SearchOne Parse} 1 parseError}
  end

  %% Result: contents of Xs until M is found.
  %% Xs = {Append M Yr}
  fun {Until M Xs ?Yr}
     L R
  in
     {List.takeDrop Xs {Length M} L R}
     if L == M then Yr = R nil
     elsecase Xs of X|Xr then X|{Until M Xr Yr}
     [] nil then Yr = nil nil
     end
  end

  %% Asserts that Xs starts with C. Returns the remainder in Ys.
  proc {Const C Xs ?Ys}
     {List.takeDrop Xs {Length C} C Ys}
  end

  %% Assert that a quoted string follows.
  %% Returns the unquoted string and binds Ys to the remainder of Xs.
  fun {QuotedString &"|Xs ?Ys}
     fun {Loop Xs Ys}
	case Xs of &\\|&"|Xr then  &\\|&"|{Loop Xr Ys}
	[] &"|Xr then Ys = Xr nil
	[] X|Xr then X|{Loop Xr Ys}
	end
     end
  in
     {Loop Xs Ys}
  end

  %% Remove formatting tags.
  fun {Fix Xs}
     {Until "</a></h3>"
      {FoldL ["<b>" "</b>" "<wbr />" "<wbr>" "<b>...</b>"]
       fun {$ Ys Z}
	  {StringX.replace Ys Z ""}
       end
       Xs}
      _}
  end
in
  {ExampleUsage}
```



## Perl


```perl
package YahooSearch;

use Encode;
use HTTP::Cookies;
use WWW::Mechanize;

# --- Internals -------------------------------------------------

sub apply (&$)
 {my $f = shift; local $_ = shift; $f->(); return $_;}

# We construct a cookie to get 100 results per page and prevent
# "enhanced results".
my $search_prefs = 'v=1&n=100&sm=' .
    apply {s/([^a-zA-Z0-9])/sprintf '%%%02X', ord $1/ge}
    join '|',
    map {'!' . $_}
    qw(hsb Zq0 XbM sss dDO VFM RQh uZ0 Fxe yCl GP4 FZK yNC mEG niH);
my $cookies = HTTP::Cookies->new;
$cookies->set_cookie(0, 'sB', $search_prefs, '/', 'search.yahoo.com');

my $mech = new WWW::Mechanize
   (cookie_jar => $cookies,
    stack_depth => 0);

sub read_page
 {my ($next, $page, @results) =
     ($mech->find_link(text => 'Next >')->url,
      decode 'iso-8859-1', $mech->content);
  while ($page =~ m
         {<h3> <a \s class="yschttl \s spt" \s
          href=" ([^"]+) " \s* >                #"
          (.+?) </a>
          .+?
          <div \s class="abstr">
          (.+?) </div>}xg)
     {push @results, {url => $1, title => $2, content => $3};
      foreach ( @{$results[-1]}{qw(title content)} )
         {s/<.+?>//g;
          $_ = encode 'utf8', $_;}}
  return $next, \@results;}

# --- Methods ---------------------------------------------------

sub new
 {my $invocant = shift;
  my $class = ref($invocant) || $invocant;
  $mech->get('http://search.yahoo.com/search?p=' . apply
     {s/([^a-zA-Z0-9 ])/sprintf '%%%02X', ord $1/ge;
      s/ /+/g;}
    shift);
  my ($next, $results) = read_page();
  return bless {link_to_next => $next, results => $results}, $class;}

sub results
 {@{shift()->{results}};}

sub next_page
 {my $invocant = shift;
  my $next = $invocant->{link_to_next};
  unless ($next)
     {$invocant->{results} = [];
      return undef;}
  $mech->get($next);
  ($next, my $results) = read_page();
  $invocant->{link_to_next} = $next;
  $invocant->{results} = $results;
  return 1;}
```



## PicoLisp


```PicoLisp
(load "@lib/http.l")

(de yahoo (Query Page)
   (default Page 1)
   (client "search.yahoo.com" 80
      (pack
         "search?p=" (ht:Fmt Query)
         "&b=" (inc (* 10 (dec Page))) )
      (make
         (while (from "<a class=\"yschttl spt\" href=\"")
            (link
               (make
                  (link (till "\"" T))       # Url
                  (from "<b>")
                  (link (till "<" T))        # Title
                  (from "class=\"abstr\"")
                  (from ">")
                  (link                      # Content
                     (pack
                        (make
                           (loop
                              (link (till "<" T))
                              (T (eof))
                              (T (= "</div" (till ">" T)))
                              (char) ) ) ) ) ) ) ) ) ) )
```

Output:

```txt
: (more (yahoo "test"))
("http://www.test.com/" "Test" "Offers practice online tests for many ...
("http://www.test.com/aboutus.htm" "Test" "Test.com has a successful ...
("http://en.wikipedia.org/wiki/Test" "Test" "YUI Test is a testing ...
("http://en.wikipedia.org/wiki/F-test" "test " "test n. A procedure for ...
...
```



## Python


```python
import urllib
import re

def fix(x):
    p = re.compile(r'<[^<]*?>')
    return p.sub('', x).replace('&amp;', '&')

class YahooSearch:
    def __init__(self, query, page=1):
        self.query = query
        self.page = page
        self.url = "http://search.yahoo.com/search?p=%s&b=%s" %(self.query, ((self.page - 1) * 10 + 1))
        self.content = urllib.urlopen(self.url).read()

    def getresults(self):
        self.results = []

        for i in re.findall('<a class="yschttl spt" href=".+?">(.+?)</a></h3></div>(.+?)</div>.*?<span class=url>(.+?)</span>', self.content):

            title = fix(i[0])
            content = fix(i[1])
            url = fix(i[2])

            self.results.append(YahooResult(title, content, url))

        return self.results

    def getnextpage(self):
        return YahooSearch(self.query, self.page+1)

    search_results = property(fget=getresults)
    nextpage = property(fget=getnextpage)

class YahooResult:
    def __init__(self,title,content,url):
        self.title = title
        self.content = content
        self.url = url

# Usage:

x = YahooSearch("test")

for result in x.search_results:
    print result.title
```



## R

Rather than using regexes to find the content (like some of the other solutions here) this method parses the HTML and finds the appropriate sections.

```R
YahooSearch <- function(query, page=1, .opts=list(), ignoreMarkUpErrors=TRUE)
{
   if(!require(RCurl) || !require(XML))
   {
      stop("Could not load required packages")
   }

   # Replace " " with "%20", etc
   query <- curlEscape(query)

   # Retrieve page
   b <- 10*(page-1)+1
   theurl <- paste("http://uk.search.yahoo.com/search?p=",
      query, "&b=", b, sep="")
   webpage <- getURL(theurl, .opts=.opts)

   # Save search for nextpage function
   .Search <- list(query=query, page=page, .opts=.opts,
      ignoreMarkUpErrors=ignoreMarkUpErrors)
   assign(".Search", .Search, envir=globalenv())

   # Parse HTML; retrieve results block
   webpage <- readLines(tc <- textConnection(webpage)); close(tc)
   if(ignoreMarkUpErrors)
   {
      pagetree <- htmlTreeParse(webpage, error=function(...){})
   } else
   {
      pagetree <- htmlTreeParse(webpage)
   }


   findbyattr <- function(x, id, type="id")
   {
      ids <- sapply(x, function(x) x$attributes[type])
      x[ids==id]
   }

   body <- pagetree$children$html$children$body
   bd <- findbyattr(body$children$div$children, "bd")
   left <- findbyattr(bd$div$children$div$children, "left")
   web <- findbyattr(left$div$children$div$children, "web")
   resol <- web$div$children$ol

   #Get url, title, content from results
   gettextfromnode <- function(x)
   {
      un <- unlist(x$children)
      paste(un[grep("value", names(un))], collapse=" ")
   }

   n <- length(resol)
   results <- list()
   length(results) <- n
   for(i in 1:n)
   {
      mainlink <- resol[[i]]$children$div$children[1]$div$children$h3$children$a
      url <- mainlink$attributes["href"]
      title <- gettextfromnode(mainlink)

      contenttext <- findbyattr(resol[[i]]$children$div$children[2], "abstr", type="class")
      if(length(contenttext)==0)
      {
          contenttext <- findbyattr(resol[[i]]$children$div$children[2]$div$children$div$children,
            "sm-abs", type="class")
      }

      content <- gettextfromnode(contenttext$div)
      results[[i]] <- list(url=url, title=title, content=content)
   }
   names(results) <- as.character(seq(b, b+n-1))
   results
}

nextpage <- function()
{
   if(exists(".Search", envir=globalenv()))
   {
      .Search <- get(".Search", envir=globalenv())
      .Search$page  <- .Search$page + 1L
      do.call(YahooSearch, .Search)
   } else
   {
      message("No search has been performed yet")
   }
}

#Usage
YahooSearch("rosetta code")
nextpage()
```



## Racket


```Racket
#lang racket
(require net/url)
(define *yaho-url* "http://search.yahoo.com/search?p=~a&b=~a")
(define *current-page* 0)
(define *current-query* "")
(define request (compose port->string get-pure-port string->url))

;;strip html tags
(define (remove-tags text)
  (regexp-replace* #px"<[^<]+?>" text ""))

;;search, parse and print
(define (search-yahoo query)
  (unless (string=? *current-query* query) ;different query, go back to page 1
    (set! *current-query* query)
    (set! *current-page* 0))
  (let* ([current-page (number->string (add1 (* 10 *current-page*)))]
         [html (request (format *yaho-url* query current-page))]
         [results (regexp-match* #px"lass=\"yschttl spt\" href=\".+?\">(.+?)<span class=url>(.+?)</span>.+?<div class=\"abstr\">(.+?)</div>" html #:match-select cdr)])
    (for ([result (in-list results)])
      (printf "Title: ~a \n Link: ~a \n Text: ~a \n\n"
              (remove-tags (first result))
              (remove-tags (second result) )
              (remove-tags (third result))))))

;;search nexxt page
(define (next-page)
  (set! *current-page* (add1 *current-page*))
  (search-yahoo *current-query*))
```


# REPL:


```txt

(search-yahoo "Rosetta")
Title: Partner With Our Interactive Marketing Agency Today | Rosetta
 Link: www.rosetta.com
 Text: Learn about the fastest growing interactive marketing agency in the country - Rosetta. Our strategic marketing planning is custom built and connects you with your ...

Title: Official Rosetta Stone® - Language Learning - Learn a Language
 Link: www.rosettastone.com
 Text: Learn a new language with Rosetta Stone®. SUMMER SALE! $349 Levels 1-5 Set + Free Shipping. Ending Soon!

Title: Rosetta (spacecraft) - Wikipedia, the free encyclopedia
 Link: en.wikipedia.org/wiki/Rosetta_probe
 Text: noun 1. a town in N Egypt, at a mouth of the Nile. 2. a female given name . Relevant Questions Why Was The Rosetta Ston... Who Is Rosetta Stone? Why Is The Rosetta ...

Title: Rosetta stone: Definition from Answers.com
 Link: www.answers.com/topic/rosetta-stone
 Text: Rosetta stone n. A basalt tablet bearing inscriptions in Greek and in Egyptian hieroglyphic and demotic scripts that w

Title: Rosetta (1999) - IMDb
 Link: www.imdb.com/title/tt0200071
 Text: The first scene, like almost all others, is a fighting scene. A girl, about 18, is sacked from her factory work because her trial period is over. The girl, Rosetta ...

Title: Welcome to Rosetta Stone® Classroom
 Link: salem-keizersd.rosettastoneclassroom.com
 Text: Welcome to Rosetta Stone® Classroom. First Time Users; © 2013 Rosetta Stone Ltd. All rights reserved.

Title: Rosetta Hardscapes
 Link: www.discoverrosetta.com/index.html
 Text: Rosetta Hardscapes sells and licenses concrete pavers, fire pits, retaining walls and landscaping features with the colors and textures of natural stone.

(next-page)
Title: Rosetta Stone
 Link: www.rosettastone.com/?prid=rosettaclassroom_com
 Text: Find great prices &amp; selection on Rosetta Stone language software for Windows &amp; Mac; shop &amp; buy titles to learn Spanish, French, &amp; more.

Title: rosetta stone spanish | eBay - Electronics, Cars, Fashion ...
 Link: www.ebay.com/sch/i.html?_nkw=rosetta+stone+spanish
 Text: Find great deals on eBay for rosetta stone spanish and rosetta stone spanish latin america. Shop with confidence.

Title: Apple - Rosetta
 Link: www.apple.com/asia/rosetta
 Text: Applications bearing the Universal symbol will run natively on both Intel- and PowerPC-based Mac computers. What about the applications you already own? Enter Rosetta.

Title: Rosetta | Free Music, Tour Dates, Photos, Videos
 Link: www.myspace.com/rosetta
 Text: The International Rosetta Mission was approved in ... Lutetia is revealed by a comprehensive analysis of the data gathered by ESA&#39;s Rosetta spacecraft when it ...

Title: Amazon.com: rosetta stone
 Link: www.amazon.com/s?ie=UTF8&amp;page=1&amp;rh=i%3Aaps%2Ck%3Arosetta...
 Text: Rosetta Stone Spanish (Latin America) Level 1 by Rosetta Stone (CD-ROM - Sept. 14, 2010) - Mac OS X 10.6 Snow Leopard, Windows 7 / 8 / XP. Buy new: $179.00 .

Title: Rosetta - Disney Wiki
 Link: disney.wikia.com/wiki/Rosetta
 Text: Rosetta: Yea, no, I don&#39;t really do mud.Vidia: But, you&#39;re a garden fairy!Rosetta: Ironic, isn&#39;t...

Title: Rosetta - Hamilton, NJ - Company | Facebook
 Link: www.facebook.com/rosetta
 Text: Rosetta, Hamilton, NJ. 2,060 likes · 36 talking about this · 135 were here. We are currently ranked by Ad Age among the top US digital agencies and recently named ...

Title: Rosetta Stone (Game) - CNET Download.com
 Link: download.cnet.com/Rosetta-Stone/3000-2111_4-10835868.html
 Text: Whether your are playing Greek number mode or Egyptian letter mode, the number one rule to keep in mind is keeping the scales balanced but its not that ...

```


## Ruby

Uses {{libheader|RubyGems}} {{libheader|Hpricot}} to parse the HTML.  Someone more skillful than I at XPath or CSS could tighten up the <code>parse_html</code> method.

```ruby
require 'open-uri'
require 'hpricot'

SearchResult = Struct.new(:url, :title, :content)

class SearchYahoo
  @@urlinfo = [nil, 'ca.search.yahoo.com', 80, '/search', nil, nil]

  def initialize(term)
    @term = term
    @page = 1
    @results = nil
    @url = URI::HTTP.build(@@urlinfo)
  end

  def next_result
    if not @results
      @results = []
      fetch_results
    elsif @results.empty?
      next_page
    end
    @results.shift
  end

  def fetch_results
    @url.query = URI.escape("p=%s&b=%d" % [@term, @page])
    doc = open(@url) { |f| Hpricot(f) }
    parse_html(doc)
  end

  def next_page
    @page += 10
    fetch_results
  end

  def parse_html(doc)
    doc.search("div#main").search("div").each do |div|
      next unless div.has_attribute?("class") and div.get_attribute("class").index("res") == 0
      result = SearchResult.new
      div.search("a").each do |link|
        next unless link.has_attribute?("class") and link.get_attribute("class") == "yschttl spt"
        result.url = link.get_attribute("href")
        result.title = link.inner_text
      end
      div.search("div").each do |abstract|
        next unless abstract.has_attribute?("class") and abstract.get_attribute("class").index("abstr")
        result.content = abstract.inner_text
      end
      @results << result
    end
  end
end

s = SearchYahoo.new("test")
15.times do |i|
  result = s.next_result
  puts i+1
  puts result.title
  puts result.url
  puts result.content
  puts
end
```



## Run BASIC


```runbasic
'--------------------------------------------------------------------------
' send this from the server to the clients browser
'--------------------------------------------------------------------------
html "<table border=1 cellpadding=0 cellspacing=0 bgcolor=wheat>"
html "<tr><td align=center colspan=2>Yahoo Search</td></tr>"
html "<tr><td align=right>Find</td><td>"
     textbox #find,findThis$,30

html "</td></tr><tr><td align=right>Page</td><td>"
     textbox #page,findPage$,2

html "</td></tr><tr><td align=center colspan=2>"
     button #s, "Search", [search]
html "        "
     button #ex, "Exit", [exit]

html "</td><td></td></tr></table>"
wait

'--------------------------------------------------------------------------
' get search stuff from the clients browser
'--------------------------------------------------------------------------
[search]
findThis$  = trim$(#find contents$())
findPage$  = trim$(#page contents$())
findPage   = max(val(findPage$),1)           ' must be at least 1

'--------------------------------------------------------------------------
' sho page but keep user interface at the top by not clearing the page (cls)
'  so they can change the search or page
' -------------------------------------------------------------------------
url$ = "http://search.yahoo.com/search?p=";findThis$;"&b=";((findPage - 1) * 10) + 1
html httpget$(url$)
wait

[exit]
cls                   ' clear browser screen and get outta here
wait
```

This user input sits at the top of the yahoo page so they can select a new search or page
<div>[[File:yahooSearchRunBasic.png‎]]</div>


## Tcl

```tcl
package require http

proc fix s {
    string map {<b>...</b> "" <b> "" </b> "" <wbr> "" "<wbr />" ""} \
            [regsub "</a></h3></div>.*" $s ""]
}
proc YahooSearch {term {page 1}} {
    # Build the (ugly) scraper URL
    append re {<a class="yschttl spt" href=".+?" >(.+?)</a></h3>}
    append re {</div><div class="abstr">(.+?)}
    append re {</div><span class=url>(.+?)</span>}

    # Perform the query; note that this handles special characters
    # in the query term correctly
    set q [http::formatQuery p $term b [expr {$page*10-9}]]
    set token [http::geturl http://search.yahoo.com/search?$q]
    set data [http::data $token]
    http::cleanup $token

    # Assemble the results into a nice list
    set results {}
    foreach {- title content url} [regexp -all -inline $re $data] {
        lappend results [fix $title] [fix $content] [fix $url]
    }

    # set up the call for the next page
    interp alias {} Nextpage {} YahooSearch $term [incr page]

    return $results
}

# Usage:  get the first two pages of results
foreach {title content url} [YahooSearch "test"] {
    puts $title
}
foreach {title content url} [Nextpage] {
     puts $title
}
```

With Tcl 8.6, more options are available for managing the global state, through objects and coroutines. First, an object-based solution that takes the basic <tt>YahooSearch</tt> functionality and dresses it up to be more Tcl-like:

```tcl
package require Tcl 8.6

oo::class create WebSearcher {
    variable page term results
    constructor searchTerm {
        set page 0
        set term $searchTerm
        my nextPage
    }
    # This next method *is* a very Tcl-ish way of doing iteration.
    method for {titleVar contentsVar urlVar body} {
        upvar 1 $titleVar t $contentsVar c $urlVar v
        foreach {t c v} $results {
            uplevel 1 $body
        }
    }
    # Reuse the previous code for simplicity rather than writing it anew
    # Of course, if we were serious about this, we'd put the code here properly
    method nextPage {} {
        set results [YahooSearch $term [incr page]]
        return
    }
}

# How to use. Note the 'foreach' method use below; new "keywords" as methods!
set ytest [WebSearcher new "test"]
$ytest for title - url {
    puts "\"$title\" : $url"
}
$ytest nextPage
$ytest for title - url {
    puts "\"$title\" : $url"
}
$ytest delete ;# standard method that deletes the object
```

However, the paradigm of an iterator is also interesting and is more appropriately supported through a coroutine. This version conceals the fact that the service produces output in pages; care should be taken with it because it can produce rather a lot of network traffic...

```tcl
package require Tcl 8.6

proc yahoo! term {
    coroutine yahoo![incr ::yahoo] apply {term {
        yield [info coroutine]
        while 1 {
            set results [YahooSearch $term [incr step]]
            if {[llength $results] == 0} {
                return -code break
            }
            foreach {t c u} $results {
                yield [dict create title $t content $c url $u]
            }
        }
    }} $term
}

# test by getting first fifty titles...
set it [yahoo! "test"]
for {set i 50} {$i>0} {incr i -1} {
    puts [dict get [$it] title]
    after 300  ;# Slow the code down... :-)
}
```


Another approach:  uses a class as specified in the task.  Also, uses an html parser from tcllib (parsing html with regular expressions is a particular annoyance of [[User:Glennj|mine]]).

{{tcllib|textutil::adjust}}<!-- for formatting output -->

```tcl
package require Tcl 8.6
package require http
package require htmlparse
package require textutil::adjust

oo::class create yahoosearch {

    method search {s} {
        my variable searchterm page baseurl
        set searchterm $s
        set page 1
        set baseurl {http://ca.search.yahoo.com/search}
    }

    method getresults {} {
        my variable state results current_data
        set results [list]
        set current_data [dict create]
        set state looking_for_results
        htmlparse::parse -cmd [list [self] html_parser_callback] [my gethtml]
    }

    method nextpage {} {
        my variable page
        incr page 10
        my getresults
    }

    method nextresult {} {
        my variable results page
        if { ! [info exists results]} {
            my getresults
        } elseif {[llength $results] == 0} {
            my nextpage
        }
        set results [lassign $results result]
        return $result
    }

    method gethtml {} {
        my variable searchterm page baseurl
        set url [format {%s?%s} $baseurl [::http::formatQuery p $searchterm b $page]]
        set response [http::geturl $url]
        set html [http::data $response]
        http::cleanup $response
        return $html
    }

    method html_parser_callback {tag slash param textBehindTheTag} {
        my variable state results current_data
        switch -exact -- $state {
            looking_for_results {
                if {$tag eq "div" && [string first {id="main"} $param] != -1} {
                    set state ready
                }
            }
            ready {
                if {($tag eq "div" && [string first {class="res} $param] != -1) ||
                    ($tag eq "html" && $slash eq "/")
                } { #" -- unbalanced quote disturbs syntax highlighting
                    if {[dict size $current_data] > 0} {lappend results $current_data}
                    set current_data [dict create]
                    set state getting_url
                }
            }
            getting_url {
                if {$tag eq "a" && [string match "*yschttl spt*" $param]} {
                    if {[regexp {href="(.+?)"} $param - url]} {
                        dict set current_data url $url
                    } else {
                        dict set current_data url "no href in tag params: '$param'"
                    }
                    dict set current_data title $textBehindTheTag
                    set state getting_title
                }
            }
            getting_title {
                if {$tag eq "a" && $slash eq "/"} {
                    set state looking_for_abstract
                } else {
                    dict append current_data title $textBehindTheTag
                }
            }
            looking_for_abstract {
                if {$tag eq "span" && [string first {class="url} $param] != -1} {
                    set state ready
                } elseif {$tag eq "div" && [string first {class="abstr} $param] != -1} {
                    dict set current_data abstract $textBehindTheTag
                    set state getting_abstract
                }
            }
            getting_abstract {
                if {$tag eq "div" && $slash eq "/"} {
                    set state ready
                } else {
                    dict append current_data abstract $textBehindTheTag
                }
            }
        }
    }
}

yahoosearch create searcher
searcher search "search text here"

for {set x 1} {$x <= 15} {incr x} {
    set result [searcher nextresult]
    dict with result {
        puts $title
        puts $url
        puts [textutil::adjust::indent [textutil::adjust::adjust $abstract] "  "]
        puts ""
    }
}
```



## TXR


The following gives us a shell utility which we can invoke with arguments like "rosetta 0" to get the first page of search results for "rosetta".

The two arguments are handled as if they were two lines of text from a data source using @(next :args). We throw an exception if there is no match (insufficient arguments are supplied). The @(cases) directive has strictly ordered evaluation, so the throw in the second branch does not happen if the first branch has a successful pattern match. If the similar @(maybe) or @(some) directives were used, this wouldn't work.

A little sprinkling of regex is used.


```txr
#!/usr/bin/txr -f
@(next :args)
@(cases)
@  QUERY
@  PAGE
@(or)
@  (throw error "specify query and page# (from zero)")
@(end)
@(next (open-command "!wget -O - http://search.yahoo.com/search?p=@QUERY\&b=@{PAGE}1 2> /dev/null"))
@(all)
@  (coll)<a class="yschttl spt" href="@URL" @/[^>]+/>@TITLE</a>@(end)
@(and)
@  (coll)<div class="@/abstr|sm-abs/">@ABSTR</div>@(end)
@(end)
@(output)
@  (repeat)
TITLE: @TITLE
URL: @URL
TEXT: @ABSTR
---
@  (end)
@(end)

```


Sample run:


```txt
$ ./yahoosearch.txr rosetta 0
TITLE: <b>Rosetta</b> | Partner With Our Interactive <wbr />Marketing Agency Today
URL: http://www.rosetta.com/Pages/default.aspx
TEXT: Learn about the fastest growing interactive marketing agency in the country - <b>Rosetta</b>. Our strategic marketing planning is custom built and connects you with your ...
---
TITLE: Official <b>Rosetta</b> StoneÂ® - Learn a <wbr />Language Online - Language ...
URL: http://www.rosettastone.com/
TEXT: <b>Rosetta</b> Stone is the world&#39;s #1 language-learning software. Our comprehensive foreign language program provides language learning for individuals and language learning ...
---
TITLE: <b>Rosetta</b> (software) - Wikipedia, the <wbr />free encyclopedia
URL: http://en.wikipedia.org/wiki/Rosetta_(software)
TEXT: Rosettais a lightweight dynamic translatorfor Mac OS Xdistributed by Apple. It enabled applications compiled for the PowerPCfamily of processors to run on Apple systems that use...
---
TITLE: <b>Rosetta</b> (spacecraft) - Wikipedia, the <wbr />free encyclopedia
URL: http://en.wikipedia.org/wiki/Rosetta_space_probe
TEXT: Rosettais a robotic spacecraftof the European Space Agencyon a mission to study the comet 67P/ChuryumovâGerasimenko. <b>Rosetta </b>consists of two main elements: the <b>Rosetta </b>space probeand...
---
TITLE: Apple - Mac
URL: http://www.apple.com/mac/
TEXT: Discover the world of Mac. Check out MacBook, iMac, iLife, and more. Download QuickTime, Safari, and widgets for free.
---
TITLE: <b>Rosetta</b> | Free Music, Tour Dates, <wbr />Photos, Videos
URL: http://www.myspace.com/rosetta
TEXT:  <b>Rosetta</b>&#39;s official profile including the latest music, albums, songs, music videos and more updates.
---
TITLE: <b>Rosetta</b>
URL: http://rosettaband.com/
TEXT: Metal for astronauts. Philadelphia, since 2003. Contact us at rosettaband@gmail.com Twitter | Facebook
---
TITLE: <b>Rosetta</b>
URL: http://rosetta.jpl.nasa.gov/
TEXT: The <b>Rosetta</b> spacecraft is on its way to catch and land a robot on a comet! <b>Rosetta</b> will reach comet &#39;67P/Churyumov-Gerasimenko&#39; (&#39;C-G&#39;) in 2014. The European Space Agency ...
---
TITLE: <b>Rosetta</b> : Multi-script Typography
URL: http://rosettatype.com/
TEXT: <b>Rosetta</b> is a new independent foundry with a strong focus on multi-script typography. We are committed to promote research and knowledge in that area and to support ...
---
TITLE: <b>Rosetta</b> (1999) - IMDb
URL: http://www.imdb.com/title/tt0200071/
TEXT: With Ãmilie Dequenne, Fabrizio Rongione, Anne Yernaux, Olivier Gourmet. Young and impulsive <b>Rosetta</b> lives with her alcoholic mother and, moved by despair, she will ...
---


```



