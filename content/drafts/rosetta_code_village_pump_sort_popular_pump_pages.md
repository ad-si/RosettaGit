+++
title = "Rosetta Code:Village Pump/Sort popular pump pages"
description = ""
date = 2015-10-25T06:09:18Z
aliases = []
[extra]
id = 4082
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Sort popular pump pages
|summary=A Python implementation of pump sorting code.
}}
I was reading [[http://blog.rosettacode.org/?p=184]].
I'm programming the Village Pump sorter right now.

I will just read all <nowiki>{{vp|...}}</nowiki> and get last edit date.
And just sort it by edit date. Very simple.

IMO, it's better than rotate new pump pages to the top of list, and old to  bottom. --[[User:Guga360|Guga360]] 21:22, 24 April 2009 (UTC)
:My code is 80% done. --[[User:Guga360|Guga360]] 22:01, 24 April 2009 (UTC)

I already runned it.
It's working perfectly. (I used GugaTagFixer to test it.)
I will post source code here. --[[User:Guga360|Guga360]] 22:37, 24 April 2009 (UTC)


```python
import urllib
import urllib2
import time
import re
import xml.dom.minidom

class Page:
	def __init__(self, title, ts):
		self.title = title
		self.time = time.mktime(time.strptime(ts,"%Y-%m-%dT%H:%M:%SZ"))

def login(user, password):
	global cookie
	
	query = urllib.urlencode({'lgname':user,'lgpassword':password})
	result = urllib.urlopen("http://www.rosettacode.org/w/api.php?action=login&format=xml",query).read()    
	parse = xml.dom.minidom.parseString(result)	
	login = parse.getElementsByTagName("login")[0]	
	cookie = login.getAttribute("cookieprefix")+"_session="+login.getAttribute("sessionid")

def get_token():
	global cookie

	result = urllib2.Request("http://www.rosettacode.org/w/api.php?action=query&prop=info&intoken=edit&titles=Main%20Page&format=xml",headers={"Cookie":cookie})
	result = urllib2.urlopen(result).read()    
	parse = xml.dom.minidom.parseString(result)
	token = parse.getElementsByTagName("page")[0].getAttribute("edittoken")
	return token
	
def GetTimeStamp(x):
	return x.getElementsByTagName("rev")[0].getAttribute("timestamp")
	
def getsortedvp():		
	edits = []

	get1 = urllib.urlopen("http://www.rosettacode.org/wiki/Village_Pump:Home&action=raw").read()
	pumps = re.findall("\{\{vp\|(.+?)\}\}",get1)[1:] # ignore first (example)

	for i in pumps:
		get2 = urllib.urlopen("http://www.rosettacode.org/w/api.php?action=query&prop=revisions&format=xml&titles=Village_Pump:Home/%s" % i.replace(" ","_")).read()
		x = xml.dom.minidom.parseString(get2)
		edits.append(Page(i,GetTimeStamp(x)))

	from operator import attrgetter
	edits = sorted(edits, key=attrgetter('time'), reverse=True)

	final = """This is the new place for Rosetta Code community activity. To start a new "thread", just append a "/" to the end of the URL of this page and give your thread a short title. On the new page, give a more detailed description of the topic as a heading, and write away. Also, link to the new topic on this page. Ex: <nowiki>{{vp|New topic}}</nowiki>.

"""+'\n'.join(["*{{vp|%s}}" % i.title for i in edits])
	
	return final	
	
def edit(article, content, token):
    global cookie	
 
    query = urllib.urlencode({
    "title":article,
    "text":content,
    "token":token,
    "bot":"yes"
    })
 
    req = urllib2.Request("http://www.rosettacode.org/w/api.php?action=edit&format=xml",data=query, headers={"Cookie":cookie})
    req = urllib2.urlopen(req).read()
 
    return req	

login("VPSortBot","password")
token = get_token()

edit("Village_Pump:Home",getsortedvp(), token)
```


--[[User:Guga360|Guga360]] 22:41, 24 April 2009 (UTC)
==What happened?==
It seems like we never did anything with this code. What's up with that? --[[User:Mwn3d|Mwn3d]] 12:55, 26 June 2009 (UTC)
