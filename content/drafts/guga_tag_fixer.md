+++
title = "Guga Tag Fixer"
description = ""
date = 2014-04-15T14:57:58Z
aliases = []
[extra]
id = 3341
[taxonomies]
categories = []
tags = []
+++

I'm a bot.

I fix deprecated code tags.

My owner is [[User:Guga360|Guga360]].

==Image==

[[Image:Bot.png|Look at me!]]

==Source Code==


```python
#       codetagfixer.py
#       
#       Copyright 2009 Guga360 <guga@guga-desktop>
#       
#       This program is free software; you can redistribute it and/or modify
#       it under the terms of the GNU General Public License as published by
#       the Free Software Foundation; either version 2 of the License, or
#       (at your option) any later version.
#       
#       This program is distributed in the hope that it will be useful,
#       but WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#       GNU General Public License for more details.
#       
#       You should have received a copy of the GNU General Public License
#       along with this program; if not, write to the Free Software
#       Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#       MA 02110-1301, USA.

import urllib
import urllib2
import re
import xml.dom.minidom

langs = ['ada', 'cpp-qt', 'pascal', 'lscript', 'z80', 'visualprolog',
'html4strict', 'cil', 'objc', 'asm', 'progress', 'teraterm', 'hq9plus',
'genero', 'tsql', 'email', 'pic16', 'tcl', 'apt_texts', 'io', 'apache',
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

langs = langs+[i.capitalize() for i in langs]+[i.upper() for i in langs]
icrtlangs = [i.capitalize() for i in langs]+[i.upper() for i in langs]

slang = '/lang'

def convert(text):
    """Remove deprecated wikisource tags from a text."""
    
    for i in langs:
        text = text.replace("<%s>" % i,"<lang %s>" % i.lower())
        text = text.replace("</%s>" % i, "<%s>" % slang)
    for i in icrtlangs:
        text = text.replace("<code %s>" % i, "<code %s>" % i.lower())    
 
    text = re.sub("(?s)<code (.+?)>(.*?)</code>", r"
```\1
\2<%s>" % slang, text)
    text = re.sub("(?s)<code>(.*?)</code>", r"<tt>\1</tt>", text)
    
    return text

def get(task):
    """Get a wikisource from a article in mediawiki."""
    
    return urllib.urlopen("http://www.rosettacode.org/w/index.php?title=%s&action=raw" % task).read()

def get_token(user, password):
    """Get an edit token."""
    
    query = urllib.urlencode({'lgname':user,'lgpassword':password})
    result = urllib.urlopen("http://www.rosettacode.org/w/api.php?action=login&format=xml",query).read()    
    parse = xml.dom.minidom.parseString(result)
    login = parse.getElementsByTagName("login")[0]
    global cookie
    cookie = login.getAttribute("cookieprefix")+"_session="+login.getAttribute("sessionid")

    result = urllib2.Request("http://www.rosettacode.org/w/api.php?action=query&prop=info&intoken=edit&titles=Main%20Page&format=xml",headers={"Cookie":cookie})
    result = urllib2.urlopen(result).read()    
    parse = xml.dom.minidom.parseString(result)
    token = parse.getElementsByTagName("page")[0].getAttribute("edittoken")
    
    return token

def edit(article, content, token):
    """Change a wikisource from a wikimedia article."""
    
    query = urllib.urlencode({
    "title":article,
    "text":content,
    "token":token,
    "bot":"yes"
    })
    
    req = urllib2.Request("http://www.rosettacode.org/w/api.php?action=edit&format=xml",data=query, headers={"Cookie":cookie})
    req = urllib2.urlopen(req).read()
    
    return req

def allpages():
    """Return a list of all articles in a wikimedia project."""
    
    pages = []
    query = urllib2.Request("http://www.rosettacode.org/w/api.php?action=query&list=allpages&aplimit=5000&format=xml",headers={"Cookie":cookie})
    query = urllib2.urlopen(query).read()
                                   
    parse = xml.dom.minidom.parseString(query).getElementsByTagName("p")
    
    for i in parse:
        pages.append(i.getAttribute("title"))
        
    return pages

def apply2(article, token):
    """Get a article, convert it, and replace it wikisource."""

    converted = convert(get(urllib.quote(article.replace(" ","_"))))  
    print edit(article, converted, token) 

token = get_token("Bot","Why you are looking here?")   
pages = allpages()

for i in pages:
    apply2(i, token)
```

