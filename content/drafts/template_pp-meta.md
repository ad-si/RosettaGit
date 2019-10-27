+++
title = "Template:Pp-meta"
description = ""
date = 2010-04-25T17:13:20Z
aliases = []
[extra]
id = 7122
[taxonomies]
categories = []
tags = []
+++

{{#ifeq:{{#switch:{{lc:{{{type}}}}}
  |move=<!--
 -->{{#ifeq:
      {{#switch:{{lc:{{{demolevel|undefined}}}}}
        |semi
        |autoconfirmed=autoconfirmed
        |administrator
        |full
        |sysop=sysop
        |undefined={{PROTECTIONLEVEL:move}}
        |#default=<!--fallback value: null
   -->}}
      |sysop|yes|no
    }}
  |create=<!--
 -->{{#if:
      {{#switch:{{lc:{{{demolevel|undefined}}}}}
        |semi
        |autoconfirmed=autoconfirmed
        |administrator
        |full
        |sysop=sysop
        |undefined={{PROTECTIONLEVEL:create}}
        |#default=<!--fallback value: null
   -->}}
      |yes|no
    }}
|#default<!--includes all other types-->=<!--
 -->{{#if:
      {{#switch:{{lc:{{{demolevel|undefined}}}}}
        |semi
        |autoconfirmed=autoconfirmed
        |administrator
        |full
        |sysop=sysop
        |undefined={{PROTECTIONLEVEL:edit}}
        |#default=<!--fallback value: null
   -->}}
      |{{#ifeq:{{#switch:{{lc:{{{disallowlevel|}}}}}
                 |semi
                 |autoconfirmed=autoconfirmed
                 |administrator
                 |full
                 |sysop=sysop
                 |#default=<!--fallback value: null-->}}
         |{{#switch:{{lc:{{{demolevel|undefined}}}}}
            |semi
            |autoconfirmed=autoconfirmed
            |administrator
            |full
            |sysop=sysop
            |undefined={{PROTECTIONLEVEL:edit}}
            |#default=<!--fallback value: null
       -->}}
         |no|yes
       }}
   |no}}
}}|yes|{{#ifeq:{{lc:{{{small|}}}}}|yes|
<div class="metadata topicon" id="protected-icon" style="display:none; right:55px;">[[Image:{{{image|{{#switch:{{lc:{{{type}}}}}
 |full=Padlock.svg
 |semi=Padlock-silver-medium.svg
 |move=Padlock-olive.svg
 |indef=Padlock-red.svg
 |office=Padlock-black.svg
 |create=Padlock-skyblue.svg
 |#default=Transparent.gif
}}}}}|20px|link={{{icon-link|Wikipedia:Protection policy#{{lc:{{{type}}}}}}}}|{{{icon-text|This {{pagetype|subjectspace=yes}} is {{#switch:{{lc:{{{type}}}}}
 |semi=semi-
 |move=move-
 |indef=permanently<nowiki> </nowiki>
 |create=creation-
 |office=<!--null, but should this have a special tag?-->
 |full
 |#default=<!--null-->
}}protected{{#ifeq:{{lc:{{{type}}}}}|indef||{{#if:{{{expiry|}}}|<nowiki> </nowiki>until {{#time:F j, Y|{{{expiry}}}}}}}}}{{#if:{{{icon-reason|}}}|<nowiki> </nowiki>{{{icon-reason}}}}}.}}}]]</div>
|<!-- else, not small -->
{{mbox 
| demospace = {{{demospace|}}}
| type = protection
| image = [[Image:{{{image|{{#switch:{{lc:{{{type}}}}}
 |full=Padlock.svg
 |semi=Padlock-silver-medium.svg
 |move=Padlock-olive.svg
 |indef=Padlock-red.svg
 |office=Padlock-black.svg
 |create=Padlock-skyblue.svg
 |#default=Transparent.gif
}}}}}|40px|{{{icon-text|This page is {{#switch:{{lc:{{{type}}}}}
 |semi=semi-
 |move=move-
 |indef=permanently<nowiki> </nowiki>
 |create=creation-
 |office=<!--null, but should this have a special tag?-->
 |full
 |#default=<!--null-->
}}protected.}}}]]
| text = '''{{{reason-text|{{#switch:{{lc:{{{type}}}}}
 |full=This page is currently [[Wikipedia:This page is protected|protected]] from editing
 |semi=Editing of this {{pagetype|subjectspace=yes}} by [[Wikipedia:User access levels#Autoconfirmed_users|new]] or [[Wikipedia:User access levels#Anonymous_users|unregistered]] users is currently [[Wikipedia:Protection policy|disabled]]
 |move=This {{pagetype|subjectspace=yes}} is currently [[Wikipedia:This page is protected|protected]] from [[Help:Moving a page|page moves]]
 |indef=This page is [[Wikipedia:This page is protected|protected]] from editing ''indefinitely''
 |office=This {{pagetype|subjectspace=yes}} is currently [[Wikipedia:This page is protected|protected]] from editing
 |create=[[Help:Starting a new page|Recreation]] of this {{pagetype|subjectspace=yes}} [[Wikipedia:This page is protected|has been disabled]]
}}{{#ifeq:{{lc:{{{type}}}}}|indef||{{#if:{{{expiry|}}}|&#32;until {{#time:F j, Y|{{{expiry}}}}}}}}}{{{reason<includeonly>|</includeonly>}}}.}}}'''<br /> {{{explanation-text|{{#ifeq:{{lc:{{{dispute}}}}}|yes|This protection is '''not''' an endorsement of the {{#ifeq:{{{type}}}|move|[{{fullurl:Special:Log|type=move&page={{FULLPAGENAMEE}}}} current title]|[{{fullurl:{{FULLPAGENAMEE}}|action=history}} current version]}}.}} See the [[Wikipedia:Protection policy|protection policy]] and [{{fullurl:Special:Log|type=protect&page={{FULLPAGENAMEE}}}} protection log] for more details. {{#switch:{{lc:{{{type}}}}}
 |full|indef=Please discuss any changes on the [[{{TALKPAGENAME}}|talk page]]; you may use the {{tlx|editprotected}} template to ask an [[Wikipedia:Administrator|administrator]] to make the edit if it is supported by [[Wikipedia:Consensus|consensus]]. {{#ifeq:{{NAMESPACE}}|{{ns:8}}<!--MediaWiki-->||You may also [[Wikipedia:Requests for page protection|request]] that this page be unprotected.}}
 |semi=If you cannot edit this {{pagetype|subjectspace=yes}} and you wish to make a change, you can {{#ifeq:{{NAMESPACE}}|{{TALKSPACE}}||[[Template:Editsemiprotected|request an edit]], [[{{TALKPAGENAME}}|discuss changes on the talk page]],}} [[Wikipedia:Requests for page protection#Current requests for unprotection|request unprotection]], [[Special:Userlogin|log in]], or <span class="plainlinks">[http://en.wikipedia.org/w/index.php?title=Special:Userlogin&type=signup <span style="color:#002bb8;" title="Sign in / create account">create an account</span>].
 |move=The page may still be edited but cannot be moved until unprotected. Please discuss any suggested moves on the [[{{TALKPAGENAME}}|talk page]] or at [[Wikipedia:Requested moves]].  You can also [[Wikipedia:Requests for page protection|request]] that the page be unprotected.  
 |office=If you are able to edit this page, please discuss all changes and additions on the [[{{TALKPAGENAME}}|talk page]] first. '''Do not remove protection from this article unless you are authorized by the Wikimedia Foundation to do so.'''
 |create=Please see the {{#if:{{{xfd|}}}|'''[[{{{xfd}}}|deletion discussion]]''' or the}} [{{fullurl:Special:Log|type=delete&page={{FULLPAGENAMEE}}}} deletion log] for details of why this page was deleted. If you would like to create a page at this title, you must first [[Wikipedia:Requests for page protection|request]] for it to be unprotected, or contact the administrator who deleted the page for the deleted material to be restored. If unsuccessful, you can use [[Wikipedia:Deletion review|deletion review]].
}}}}}
}}
}}|[[Category:Wikipedia pages with incorrect protection templates]]}}<!--End if small--><includeonly>{{#ifeq:{{lc:{{{categories|no}}}}}|no||{{{categories|}}}}}</includeonly><noinclude>
Copied from WP for Reflist

{{documentation}}
<!-- Add categories and interwikis to the /doc subpage, not here! -->
</noinclude>
