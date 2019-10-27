+++
title = "Template:Cite book"
description = ""
date = 2010-10-11T15:22:11Z
aliases = []
[extra]
id = 7129
[taxonomies]
categories = []
tags = []
+++

{{Citation/core
 |Citation class=book
  |AuthorMask = {{{authormask|{{{author-mask|}}}}}}
  |Surname1 = {{{last|{{{surname|{{{last1|{{{surname1|{{{author1|{{{author|{{{authors|{{{author|}}}}}}}}}}}}}}}}}}}}}}}}
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
  |Coauthors = {{{coauthor|{{{coauthors|}}}}}}
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
  |Title={{{title|}}}
  |TransTitle={{{trans_chapter|}}}
  |TransItalic={{{trans_title|}}}
  |URL={{{url|}}}
  |TitleType={{{type|}}}
  |Series={{{series|}}}
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
  |EditorSurname1 = {{{editor-last|{{{editor-surname|{{{editor1-last|{{{editor1-surname|{{{editor|{{{editors|}}}}}}}}}}}}}}}}}}
  |EditorSurname2 = {{{editor2-last|{{{editor2-surname|}}}}}}
  |EditorSurname3 = {{{editor3-last|{{{editor3-surname|}}}}}}
  |EditorSurname4 = {{{editor4-last|{{{editor4-surname|}}}}}}
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
  |OCLC={{{oclc|{{{OCLC|}}}}}}
  |Bibcode={{{bibcode|}}}
  |DOI={{{doi|{{{DOI|}}}}}}
  |DoiBroken={{{doi_brokendate|}}}
  |AccessDate={{{access-date|{{{accessdate|}}}}}}
  |DateFormat={{{dateformat|none}}}
  |quote = {{{quote|}}}
  |laysummary = {{{laysummary|}}}
  |laydate = {{{laydate|}}}
  |Ref={{{ref|}}}
  |Sep = {{{separator|{{{seperator|.}}}}}}
  |PS = {{#if:{{{quote|}}}||{{{postscript|.}}}}}
  |AuthorSep = {{#ifeq:{{{author-separator|}}}|;|&#059;|{{{author-separator|&#059;}}}}}&#32;
  |NameSep = {{{author-name-separator|,}}}&#32;
  |Trunc = {{{display-authors|8}}}
  |amp = {{{lastauthoramp|}}}
}}{{#if:{{{accessdaymonth|}}}{{{accessmonthday|}}}{{{accessday|}}}{{{accessmonth|}}}{{{accessyear|}}}{{{day|}}}{{{access-date|}}}{{{dateformat|}}}|[[Category:Pages containing cite templates with deprecated parameters|{{NAMESPACE}} {{PAGENAME}}]]}}<noinclude>
Copied from WP for Reflist
{{pp-template|small=yes}}
{{documentation}}
{{template}}</noinclude>
