+++
title = "MediaWiki:Common.css"
description = ""
date = 2016-01-04T04:44:06Z
aliases = []
[extra]
id = 4
[taxonomies]
categories = []
tags = []
+++

/* 

```txt
<nowiki>*/

textarea {
     font-family:Consolas,Monaco,Lucida Console,Liberation Mono,DejaVu Sans Mono,Bitstream Vera Sans Mono,Courier New, monospace;
}
.ti89b {
     font-family: 'TI Uni', monospace;
}

.wikicode {
     font-weight: bold;
}

/* GeSHi code highlighting */
.kw1 {
     font-weight: bold;
     color: #0000ff;
}

.kw2 {
     font-weight: bold;
     color: #0000cc;
}

.kw3 {
     font-weight: bold;
     color: #3333cc;
}

.kw4 {
     font-weight: bold;
     color: #6666cc;
}

.kw5 {
     font-weight: bold;
     color: #9999cc;
}

.br0 {
     font-weight: bold;
     color: #009900;
}

.nu0 {
     font-weight: bold;
     color: #009999;
}

.co1 {
     color: #666666;
     font-style: italic;
}

.co2 {
     font-weight: bold;
     color: #666666;
     font-style: italic;
}

.coMULTI{
     font-weight: bold;
     color: #666;
     font-style: italic;
}

.me1 {
     font-weight: bold;
     color: #993399;
}

.me2 {
     font-weight: bold;
     color: #cc66cc;
}

.st0, .st_h {
     color: #ff0000;
}

/* Whitespace classes */
.re2 { background-color: #ccccff; }
.re3 { background-color: #ffcccc; }

/* Pad Google AdSense box in portlet in sidebar */
#p-googleadsense .pBody {
    padding-top: 5px;
    text-align:  center;
}

/* Highlight clicked reference in blue to help navigation */

ol.references > li:target {
 background-color: #DEF;
}

/* wikitable/prettytable class for skinning normal tables */

table.wikitable,
table.prettytable {
  margin: 1em 1em 1em 0;
  background: #f9f9f9;
  border: 1px #aaa solid;
  border-collapse: collapse;
}

table.wikitable th, table.wikitable td,
table.prettytable th, table.prettytable td {
  border: 1px #aaa solid;
  padding: 0.2em;
}

table.wikitable th,
table.prettytable th {
  background: #f2f2f2;
  text-align: center;
}

table.wikitable caption,
table.prettytable caption {
  margin-left: inherit;
  margin-right: inherit;
  font-weight: bold;
}

/* default skin for navigation boxes */
table.navbox {
    background-color: #f9f9f9;
    border: 1px solid #aaa;
    clear: both;
    font-size: 90%;
    margin: 1em 0em 0em;
    padding: 5px;
    text-align: center;
    width: 100%;
}

table.navbox th {
    background-color: #ccf;
    padding-left: 1em;
    padding-right: 1em;
}

table.navbox tr:not(:first-child) th {
    background-color: #ddf;
}

@media print {
    .navbox {
        display: none;
    }
}

/* Infobox template style */

.infobox {
   border: 1px solid #aaa;
   background-color: #f9f9f9;
   color: black;
   margin-bottom: 0.5em;
   margin-left: 1em;
   padding: 0.2em;
   float: right;
   clear: right;
}
.infobox td,
.infobox th {
   vertical-align: top;
}
.infobox caption {
   font-size: larger;
   margin-left: inherit;
}
.infobox.bordered {
   border-collapse: collapse;
}
.infobox.bordered td,
.infobox.bordered th {
   border: 1px solid #aaa;
}
.infobox.bordered .borderless td,
.infobox.bordered .borderless th {
   border: 0;
}

.infobox.sisterproject {
   width: 20em;
   font-size: 90%;
}



@media print {
    .infobox.sisterproject {
        display: none;
    }
}

/* styles for bordered infobox with merged rows */
.infobox.bordered .mergedtoprow td,
.infobox.bordered .mergedtoprow th {
   border: 0;
   border-top: 1px solid #aaa;
   border-right: 1px solid #aaa;
}

.infobox.bordered .mergedrow td,
.infobox.bordered .mergedrow th {
   border: 0;
   border-right: 1px solid #aaa;
}



#disambig {
    border-top: 1px solid #ccc; 
    border-bottom: 1px solid #ccc;
}


/* Standard talk template style */

.Talk-Notice  {
    border: 1px solid #C0C090;
    background-color: #F8EABA;
    margin-bottom: 3px;
    width: 85%;
    border-spacing: 3px;
    margin-left: auto;
    margin-right: auto;
}

.Talk-Notice:after {
  content: "The CSS for this template should be changed. See [[Wikipedia:Template Standardisation]].";
}

/* Make template background appear correctly on all browsers */
.Talk-Notice td {
    background: inherit;
}

/* Persondata */
table.persondata {
    border: 1px solid #aaa;
    display: none;
    speak: none;
}
.persondata-label {
    color: #aaa;
}

/* Makes redirects appear in italics on [[Special:Allpages]] */
.allpagesredirect {
    font-style: italic;
}

/* Choose whether to have AD/BC dates or CE/BCE dates*/

/* First, the default : display both : See templates ADCE and BCEBC for how these are used*/
.Use_Default_Date_Convention { display: inline; }
.Use_AD_and_BC { display: none; }
.Use_BCE_and_CE { display: none; }

/* If you want to display AD and BC add the following to User:You/monobook.css page */
/*
.Use_Default_Date_Convention { display: none; }
.Use_AD_and_BC { display:inline; }
.Use_BCE_and_CE { display:none; }
*/

/*If you want to display CE and BCE add the following to User:You/monobook.css page */
/*
.Use_Default_Date_Convention { display: none; }
.Use_AD_and_BC { display:none; }
.Use_BCE_and_CE {display:inline; }
*/

/* Class for links with loudspeaker icon next to them */

.audiolink a{
    background: url("http://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Loudspeaker.svg/11px-Loudspeaker.svg.png") center left no-repeat !important;
    padding-left: 16px !important;
    padding-right: 0 !important;
}

/* Icons for medialist templates [[Template:Listen]], [[Template:Multi-listen_start]], [[Template:Video]], [[Template:Multi-video_start]] */

div.listenlist {
    background: url("http://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Gnome-speakernotes.png/30px-Gnome-speakernotes.png");
    padding-left: 40px;
}

div.videolist, div.multivideolist {
    background: url("http://upload.wikimedia.org/wikipedia/en/thumb/2/20/Tango-video-x-generic.png/40px-Tango-video-x-generic.png");
    padding-left: 50px;
}

/* Style rules for media list templates */

div.medialist {
    min-height: 50px;
    margin: 1em;
    background-position: top left;
    background-repeat: no-repeat;
}

div.medialist ul {
    list-style-type: none; 
    list-style-image: none;
    margin: 0;
}

div.medialist ul li {
    padding-bottom: 0.5em;
}

div.medialist ul li li {
    font-size: 91%;
    padding-bottom: 0;
}

/* Change the external link icon to an Adobe icon for all PDF files */
/* (in browsers that support these CSS selectors, like Mozilla and Opera) */
#bodyContent a[href$=".pdf"].external, 
#bodyContent a[href*=".pdf?"].external, 
#bodyContent a[href*=".pdf#"].external,
#bodyContent a[href$=".PDF"].external, 
#bodyContent a[href*=".PDF?"].external, 
#bodyContent a[href*=".PDF#"].external {
    background: url(http://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Icons-mini-file_acrobat.gif/15px-Icons-mini-file_acrobat.gif) center right no-repeat;
    padding-right: 16px;
}

/* Change the external link icon to an Adobe icon anywhere the PDFlink class */
/* is used (notably Template:PDFlink). This works in IE, unlike the above. */
span.PDFlink a {
    background: url(http://upload.wikimedia.org/wikipedia/commons/thumb/2/23/Icons-mini-file_acrobat.gif/15px-Icons-mini-file_acrobat.gif) center right no-repeat !important;
    padding-right: 17px !important;
}

/* Content in columns with CSS instead of tables [[Template:Columns]] */
div.columns-2 div.column {
    float: left;
    width: 50%;
    min-width: 300px;
}

div.columns-3 div.column {
    float: left;
    width: 33.3%;
    min-width: 200px;
}

div.columns-4 div.column {
    float: left;
    width: 25%;
    min-width: 150px;
}

div.columns-5 div.column {
    float: left;
    width: 20%;
    min-width: 120px;
}

/*Add formatting to make sure that "external references" from [[Template:Ref]] do
  not get URL expansion, not even when printed. The mechanism up to MediaWiki 1.4 was
  that the HTML code contained a SPAN following the anchor A; this SPAN had the class
  "urlexpansion", which was not displayed on screen, but was shown when the medium was
  "print". The rules below ensure (a) that there is no extra padding to the right of
  the anchor (displayed as "[<number>]"), (b) that there is no "external link arrow" for
  the link, and (c) that this SPAN of class "urlexpansion" is never shown.
  ~~~~
*/

.plainlinksneverexpand {
  background: none ! important;
  padding: 0 ! important;
}

.plainlinksneverexpand .urlexpansion {
  display: none ! important;
}

/* Make sure that ext links displayed within "plainlinksneverexpand" don't get
   the arrow...
*/
.plainlinksneverexpand a {
   background: none !important;
   padding: 0 !important;
}

/* With MediaWiki 1.5, the mechanism has changed: instead of a SPAN of class "urlexpansion"
   following the anchor A, the anchor itself now has class "external autonumber" and the
   expansion is inserted when printing (see the common printing style sheet at
   http://en.wikipedia.org/skins-1.5/common/commonPrint.css) using the ":after" pseudo-
   element of CSS. We have to switch this off for links due to Template:Ref!
   ~~~~
*/
.plainlinksneverexpand a.external.text:after {
  display: none !important;
}
.plainlinksneverexpand a.external.autonumber:after {
  display: none !important;
}

/* Messagebox templates */

.messagebox {
   border: 1px solid #aaa;
   background-color: #f9f9f9;
   width: 80%;
   margin: 0 auto 1em auto;
   padding: .2em;
}

.messagebox.merge {
   border: 1px solid #c0b8cc;
   background-color: #f0e5ff;
   text-align: center;
}

.messagebox.cleanup {
   border: 1px solid #9f9fff;
   background-color: #efefff;
   text-align: center;
}

.messagebox.standard-talk {
   border: 1px solid #c0c090;
   background-color: #f8eaba;
}

.messagebox.taskdescription {
   border: 1px solid #3333ff;
   background-color: #9999ff;
   text-align: center;
}

/* Put a checker background at the image description page only visible if the image has transparent background */

#file img {background: url("http://upload.wikimedia.org/wikipedia/commons/5/5d/Checker-16x16.png") repeat;}

/* Support for Template:IPA, Template:Unicode and Template:Polytonic. The inherit declaration resets the font for all browsers except MSIE6.  The empty comment must remain. Please copy any changes to [[Template:IPA fonts]] and [[Template:Unicode fonts]]. */
.IPA {
        font-family: "Chrysanthi Unicode", "Doulos SIL", Gentium, GentiumAlt, Code2000, "TITUS Cyberbit Basic", "DejaVu Sans", "Bitstream Cyberbit", "Arial Unicode MS", "Lucida Sans Unicode", "Hiragino Kaku Gothic Pro", "Matrix Unicode";
        font-family /**/:inherit;
}
.Unicode {
        font-family: Code2000, "TITUS Cyberbit Basic", "Doulos SIL", "Chrysanthi Unicode", "Bitstream Cyberbit", "Bitstream CyberBase", Thryomanes, Gentium, GentiumAlt, "Lucida Grande", "Arial Unicode MS", "Microsoft Sans Serif", "Lucida Sans Unicode";
        font-family /**/:inherit;
}
.latinx {
        font-family: Code2000, "TITUS Cyberbit Basic", "Microsoft Sans Serif";
        font-family /**/:inherit;
}
.polytonic {
        font-family: Athena, Gentium, "Palatino Linotype", "Arial Unicode MS", "Lucida Sans Unicode", "Lucida Grande", Code2000; 
        font-family /**/:inherit;
}
.mufi {
        font-family: Alphabetum, Cardo, LeedsUni, Junicode, "TITUS Cyberbit Basic", ALPHA-Demo;
}

#wpSave {
  font-weight: bold;
}

/* class hiddenStructure is defunct. See [[Wikipedia:hiddenStructure]] */
.hiddenStructure {
   display: inline ! important;
   color: #f00; 
   background-color: #0f0;
}

/* Removes underlines from links */
.nounderlines a { 
  text-decoration: none;
}

/* Remove underline from IPA links */
.IPA a:link, .IPA a:visited {
  text-decoration: none;
}

/* Removes useless links from printout */
@media print {
    #privacy, #about, #disclaimer {display:none;}
}

#EnWpMpBook { background-image: url(http://upload.wikimedia.org/wikipedia/en/7/7e/MP-open-book.png); }
#EnWpMpSearch { background: url(http://upload.wikimedia.org/wikipedia/en/a/ae/MP-magnifying-glass.png) no-repeat top right; }
#EnWpMpSearchInner { float: right; width: 20em; text-align: center; }
#EnWpMpBook2 { background-image: url(http://upload.wikimedia.org/wikipedia/commons/8/8e/MP-open-book2.png); }

.messagebox.small-talk {
  width: 238px;
  font-size: 85%;
  float: right;
  clear: both;
  margin: 0 0 1em 1em;
  line-height: 1.25em; 
  background: #F8EABA;
}

/* Standard Navigationsleisten, aka box hiding thingy from .de.  Documentation at [[Wikipedia:NavFrame]]. */

div.Boxmerge,
div.NavFrame {
        margin: 0px;
        padding: 2px;
        border: 1px solid #aaa;
        text-align: center;
        border-collapse: collapse;
        font-size: 95%;
}
div.Boxmerge div.NavFrame {
        border-style: none;
        border-style: hidden;
}
div.NavFrame + div.NavFrame {
        border-top-style: none;
        border-top-style: hidden;
}
div.NavPic {
        background-color: #fff;
        margin: 0px;
        padding: 2px;
        float: left;
}
div.NavFrame div.NavHead {
        height: 1.6em;
        font-weight: bold;
        font-size: 100%;
        background-color: #efefef;
        position:relative;
}
div.NavFrame p {
        font-size: 100%;
}
div.NavFrame div.NavContent {
        font-size: 100%;
}
div.NavFrame div.NavContent p {
        font-size: 100%;
}
div.NavEnd {
        margin: 0px;
        padding: 0px;
        line-height: 1px;
        clear: both;
}
a.NavToggle {
        position:absolute;
        top:0px;
        right:3px;
        font-weight:normal;
        font-size:smaller;
}

/* Coloured watchlist numbers */
.mw-plusminus-pos {
  color:darkgreen;
}

/* .mw-plusminus-null currently at developer default */

.mw-plusminus-neg {
  color:darkred;
}

.dablink {
  font-style:italic;
  padding-left:2em;
}

pre {
  overflow: auto;
}

/*</nowiki>
```
*/
