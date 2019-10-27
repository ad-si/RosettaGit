+++
title = "Template:Citation"
description = ""
date = 2011-01-24T09:14:13Z
aliases = []
[extra]
id = 9182
[taxonomies]
categories = []
tags = []
+++

<includeonly>{{
  #if: {{{inventor-surname|{{{inventor1-surname|{{{inventor-last|{{{inventor1-last|{{{inventor|{{{invent1|{{{invent-1|{{{country-code|{{{3|}}}}}}}}}}}}}}}}}}}}}}}}}}}
<!--
    CITATIONS FOR PATENTS
-->
|{{Citation/patent
 |Surname1 = {{{inventor-surname|{{{inventor1-surname|{{{inventor-last|{{{inventor1-last|{{{inventor|{{{invent-1|{{{invent1|}}}}}}}}}}}}}}}}}}}}}
 |Surname2={{{inventor2-surname|{{{inventor2-last|{{{inventor2|{{{invent2|}}}}}}}}}}}}
 |Surname3={{{inventor3-surname|{{{inventor3-last|{{{inventor3|{{{invent3|}}}}}}}}}}}}
 |Surname4={{{inventor4-surname|{{{inventor4-last|{{{inventor4|{{{invent4|}}}}}}}}}}}}
 |Given1 = {{{inventor-given|{{{inventor1-given|{{{inventor-first|{{{inventor1-first|}}}}}}}}}}}}
 |Given2={{{inventor2-given|{{{inventor2-first|}}}}}}
 |Given3={{{inventor3-given|{{{inventor3-first|}}}}}}
 |Given4={{{inventor4-given|{{{inventor4-first|}}}}}}
 |Inventorlink1={{{inventorlink1|{{{inventorlink|}}}}}}
 |Inventorlink2={{{inventorlink2|}}}
 |Inventorlink3={{{inventorlink3|}}}
 |Inventorlink4={{{inventorlink4|}}}
 |Title={{{title|}}}
 |CountryCode={{{country-code|{{{country|{{{1|}}}}}}}}}
 |PublicationNumber={{{publication-number|{{{patent-number|{{{number|{{{2|}}}}}}}}}}}}
 |Description={{{description|{{{status|{{{3|}}}}}}}}}
 |PublicationDate={{{publication-date|{{{pubdate|}}}}}}
 |IssueDate={{{issue-date|{{{gdate|}}}}}}
 |Year={{{year}}} 
 |FilingDate={{{fdate|}}}
 |PriorityDate={{{pridate|}}}
 |Assignee1={{{assign1|}}}
 |Assignee2={{{assign2|}}}
  |Ref={{{ref|harv}}}
  |Sep = {{#ifeq:{{{separator|{{{seperator}}}}}}|;|&#059;|{{{separator|{{{seperator|,}}}}}}}}
  |PS = {{#if:{{{quote|}}}||{{{postscript|}}}|.}}
  |AuthorSep = {{#ifeq:{{{author-separator|}}}|;|&#059;|{{{author-separator|&#059;}}}}}&#32;
}}<!--
    CITATIONS FOR THINGS LIKE BOOKS AND PERIODICALS
-->
|{{Citation/core
  |AuthorMask = {{{author-mask|{{{authormask|}}}}}}
  |Surname1 = {{{last|{{{surname|{{{last1|{{{surname1|{{{author1|{{{author|{{{authors|}}}}}}}}}}}}}}}}}}}}}
  |Surname2 = {{{last2|{{{surname2|{{{author2|}}}}}}}}}
  |Surname3 = {{{last3|{{{surname3|{{{author3|}}}}}}}}}
  |Surname4 = {{{last4|{{{surname4|{{{author4|}}}}}}}}}
  |Surname5 = {{{last5|{{{surname5|{{{author5|}}}}}}}}}
  |Surname6 = {{{last6|{{{surname6|{{{author6|}}}}}}}}}
  |Surname7 = {{{last7|{{{surname7|{{{author7|}}}}}}}}}
  |Surname8 = {{{last8|{{{surname8|{{{author8|}}}}}}}}}
  |Surname9 = {{{last9|{{{surname9|{{{author9|}}}}}}}}}
  |Given1 = {{{first1|{{{given1|{{{first|{{{given|}}}}}}}}}}}}
  |Given2 = {{{first2|{{{given2|}}}}}}
  |Given3 = {{{first3|{{{given3|}}}}}}
  |Given4 = {{{first4|{{{given4|}}}}}}
  |Given5 = {{{first5|{{{given5|}}}}}}
  |Given6 = {{{first6|{{{given6|}}}}}}
  |Given7 = {{{first7|{{{given7|}}}}}}
  |Given8 = {{{first8|{{{given8|}}}}}}
  |Given9 = {{{first9|{{{given9|}}}}}}
  |Authorlink1 = {{{author-link|{{{author1-link|{{{authorlink|{{{authorlink1|}}}}}}}}}}}}
  |Authorlink2 = {{{author2-link|{{{authorlink2|}}}}}}
  |Authorlink3 = {{{author3-link|{{{authorlink3|}}}}}}
  |Authorlink4 = {{{author4-link|{{{authorlink4|}}}}}}
  |Authorlink5 = {{{author5-link|{{{authorlink5|}}}}}}
  |Authorlink6 = {{{author6-link|{{{authorlink6|}}}}}}
  |Authorlink7 = {{{author7-link|{{{authorlink7|}}}}}}
  |Authorlink8 = {{{author8-link|{{{authorlink8|}}}}}}
  |Authorlink9 = {{{author9-link|{{{authorlink9|}}}}}}
  |Coauthors   = {{{coauthor|{{{coauthors|}}}}}}
  |Year={{{year|{{    <!-- attempt to derive year from date, if possible -->
             #if: {{{date|}}}
             |{{
                #iferror:{{#time:Y|{{{date|}}} }}
                |{{#iferror:{{#time:Y|{{{publication-date|einval}}} }}||{{#time:Y|{{{publication-date|}}} }}}}
                |{{#time:Y|{{{date|}}} }}
              }}
             |{{{publication-date|}}} <!-- last resort -->
           }}
        }}}
  |YearNote = {{{origyear|}}}
  |Date = {{#if:{{{date|}}}|{{{date}}}|{{{day|}}} {{{month|}}} {{{year|{{{publication-date|}}}}}}}}
  |DateFormat={{{dateformat|}}}
  |Title={{{title|}}}
  |URL={{#if:{{{archiveurl|}}}|{{{archiveurl|}}}|{{{url|}}}}}
  |Series={{{series|{{{version|}}}}}}
  |Periodical = {{{journal|{{{periodical|{{{newspaper|{{{magazine|{{{work|}}}}}}}}}}}}}}}
  |Volume = {{{volume|}}}
  |Issue = {{{issue|{{{number|}}}}}}
  |At = {{
          #if: {{{journal|{{{periodical|{{{newspaper|{{{magazine|}}}}}}}}}}}}
          |{{{pages|{{{page|{{{at|}}}}}}}}}
          |{{
             #if: {{{page|}}}
             |{{#if:{{{nopp|}}}||p. }}{{{page}}} 
             |{{
                #if: {{{pages|}}}
                |{{#if:{{{nopp|}}}||pp. }}{{{pages}}}
                |{{{at|}}}
              }}
           }}
        }}
  |IncludedWorkTitle = {{{chapter|{{{contribution|}}}}}}
  |IncludedWorkURL = {{{chapter-url|{{{chapterurl|{{{contribution-url|}}}}}}}}}
  |Other = {{{others|}}}
  |Edition = {{{edition|}}}
  |Place = {{{place|{{{location|}}}}}}
  |PublicationPlace = {{{publication-place|{{{place|{{{location|}}}}}}}}}
  |Publisher = {{{publisher|}}}
  |PublicationDate = {{{publication-date|}}}
  |EditorSurname1 = {{{editor-last|{{{editor-surname|{{{editor1-last|{{{editor1-surname|{{{editor1|{{{editor|{{{editors|}}}}}}}}}}}}}}}}}}}}}
  |EditorSurname2 = {{{editor2-last|{{{editor2-surname|{{{editor2|}}}}}}}}}
  |EditorSurname3 = {{{editor3-last|{{{editor3-surname|{{{editor3|}}}}}}}}}
  |EditorSurname4 = {{{editor4-last|{{{editor4-surname|{{{editor4|}}}}}}}}}
  |EditorGiven1 = {{{editor-first|{{{editor-given|{{{editor1-first|{{{editor1-given|}}}}}}}}}}}}
  |EditorGiven2={{{editor2-first|{{{editor2-given|}}}}}}
  |EditorGiven3={{{editor3-first|{{{editor3-given|}}}}}}
  |EditorGiven4={{{editor4-first|{{{editor4-given|}}}}}}
  |Editorlink1={{{editor-link|{{{editor1-link|}}}}}}
  |Editorlink2={{{editor2-link|}}}
  |Editorlink3={{{editor3-link|}}}
  |Editorlink4={{{editor4-link|}}}
  |language = {{{language|{{{in|}}}}}}
  |format = {{{format|}}}
  |ID={{{id|{{{ID|}}}}}}
  |ISBN={{{isbn|{{{ISBN|}}}}}}
  |ISSN={{{issn|{{{ISSN|}}}}}}
  |OCLC={{{oclc|{{{OCLC|}}}}}}
  |PMID={{{pmid|{{{PMID|}}}}}}
  |PMC={{{pmc|{{{PMC|}}}}}}
  |Embargo={{{pmc-embargo-date|1010-10-10}}}
  |Bibcode={{{bibcode|}}}
  |DOI={{{doi|{{{DOI|}}}}}}
  |DoiBroken={{{doi_inactivedate|{{{doi_brokendate|}}}}}}
  |AccessDate={{{access-date|{{{accessdate|}}}}}}
  |laysummary = {{{laysummary|}}}
  |quote = {{{quote|}}}
  |laydate = {{{laydate|}}}
  |Ref={{{ref|harv}}}
  |Sep = {{#ifeq:{{{separator|{{{seperator}}}}}}|;|&#059;|{{{separator|{{{seperator|,}}}}}}}}
  |PS = {{#if:{{{quote|}}}||{{{postscript|}}}}}
  |AuthorSep = {{#ifeq:{{{author-separator|}}}|;|&#059;|{{{author-separator|&#059;}}}}}&#32;
  |NameSep = {{{author-name-separator|,}}}&#32;
  |amp = {{{lastauthoramp|}}}
  |Trunc = {{#if:{{{display-authors|}}}|{{{display-authors}}}|8}}
  |ArchiveURL= {{{archiveurl|}}}
  |OriginalURL = {{{url|}}}|ArchiveDate= {{{archivedate|}}}
}}}}{{#if:{{{accessdaymonth|}}}{{{accessmonthday|}}}{{{accessday|}}}{{{accessmonth|}}}{{{accessyear|}}}{{{day|}}}{{{access-date|}}}{{{dateformat|}}}|[[Category:Pages containing cite templates with deprecated parameters|{{NAMESPACE}} {{PAGENAME}}]]}}</includeonly><noinclude>
{{Documentation}}
</noinclude>
