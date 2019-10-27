+++
title = "Rosetta Code:Village Pump/tasks descriptions only"
description = ""
date = 2019-01-30T16:43:53Z
aliases = []
[extra]
id = 18927
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=task descriptions only
|summary=How to get a list of programming tasks *without* solutions
}}
I'm going on a long plane trip. I'd like to take a printout of all the tasks but without the solutions. I have in mind to solve some of the solutions on paper whilst airborne, taking the list of solutions and the programming language manual.
[[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 07:04, 27 March 2015 (UTC)
:Not sure it is what you were thinking, but I want this for a [[specific]] Language. How can that be done?
:--[[User:RLRandallx|RLRandallx]] 21:55, 20 May 2016 (UTC)

:Sounds like a good idea for a task "Extract task description from an RC task" (i.e. down to the first <nowiki>{{header|...}}</nowiki> :-)
:--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:18, 27 March 2015 (UTC)

:I'd approach this by downloading all the tasks (approximately: all the rosetta wiki links from http://rosettacode.org/wiki/Category:Programming_Tasks which do not have a ':' in the url's path), and then clip them off starting at the line which contains "mw-headline"). That should get you close enough?
:In other words, something like this:

:
```bash
wget -k http://rosettacode.org/wiki/Category:Programming_Tasks
lynx -force_html -dump Category%3AProgramming_Tasks | awk '/http:..rosettacode.org.wiki/{print $2}' | grep -v ':.*:' | xargs wget --wait=1 -kp
find rosettacode.org/wiki -type f | xargs perl -i -0777 -pe 's/\n[^\n]*mw-headline.*//s'
```


:Change perl's -i option to -i.bak if you feel like you might want to save the originals.

:You'll probably want a <code>.html</code> extension on those files, though, so also:

:
```bash
find rosettacode.org/wiki -type f | while read f; do mv $f $f.html; done
```


:This will leave the table of contents in place, if but you could remove that if you like, using a similar approach. (Make a copy of your work before experimenting, so you do not overburden the site. When the site is overburdened, nobody can pull down content from it - including you.)

:I hope this helps. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 14:58, 27 March 2015 (UTC)

:I feel this task should be a builtin task for the website. Could someone please add it to the Main Menu? The essential code is above.
:--[[User:RLRandallx|RLRandallx]] 21:07, 20 May 2016 (UTC)

::I was just fiddling inside of my webbrowser and came up with the following javascript-ish pseudocode. A solution using JScript or VBScript inside (gasp) Internet Explorer would seem to be the go. (Doing it on Linux might be a bit trickier.)

::
```javascript

pages = document.getElementById("mw-pages")
anchors = pages.getElementsByTagName("a")
document.location = anchors[0].href // each one

start = document.getElementsByClassName("infobox")[0]
cursor = start
desc = ""
while (cursor.tagName !== "TABLE") { 
  there = cursor; 
  desc = desc + there.innerText + "\n"; 
  cursor = cursor.nextElementSibling 
}
// desc contains the task description
```


::The while loop should handle the different amounts of text between the infobox markup and the beginning of the contents table. [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 09:53, 16 April 2015 (UTC)

::Here's a SlimerJS solution. Not perfect but seems to work reasonably well (it's funny watching all the little windows open and close.) A little annoyingly, SlimerJS doesn't implement innerText, so there are the odd occasions where there's markup in the file. Also, occasionally there's a TABLE that's not the contents before the contents, thus truncating the description.
::
```javascript
var fs = require('fs');

function innerCall(nam, ref) {
	var ipage = require("webpage").create();
	ipage.open(ref, function () {
		var description = ipage.evaluate( function () {
			var start = document.getElementsByClassName("infobox")[0];
			var cursor = start.nextElementSibling;
			var desc = "";
			while (cursor.tagName !== "TABLE") {
				there = cursor;
				desc = desc + there.innerHTML + "\n"; 
				cursor = cursor.nextElementSibling;
			}
			return desc;
		});
		var fileName = nam + ".txt";
		fileName = fileName.replace(/\//g,"_");
		fs.write(fileName,description);
		//console.log(nam + " " + ref + "\n" + description);
		ipage.close();
        });
        
}


var page = require("webpage").create();
page.open("http://rosettacode.org/wiki/Category:Programming_Tasks", function () {
    var anchors = page.evaluate(function () {
        return document.getElementById("mw-pages").getElementsByTagName("a");
    });
    for (var i = 0; i < anchors.length; i++ ) {
        //console.log(anchors[i].innerHTML + ' ' + anchors[i].href);
        innerCall(anchors[i].innerHTML, anchors[i].href);
        slimer.wait(100);
    }
    page.close();
    //phantom.exit();
});
```
 

::I would have used PhantomJs but had trouble building it and lost patience rapidly. [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 15:21, 16 April 2015 (UTC)

::Ah silly me, I should have done a 'sudo apt install phantomjs'. Trying that now. Meanwhile, I'm happy to make this a challenge. Worthwhile? [[User:Axtens|Axtens]] ([[User talk:Axtens|talk]]) 15:41, 16 April 2015 (UTC)

::One could try to make this itself a programming task 🤔 [[User:Rainb|Rainb]] ([[User talk:Rainb|talk]]) 12:45, 30 January 2019 (UTC)

:::Indeed one could; and maybe call it something like [[Rosetta_Code/Tasks_without_examples]], except that already exists. --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 16:42, 30 January 2019 (UTC)
