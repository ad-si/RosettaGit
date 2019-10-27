+++
title = "Rosetta Code:Village Pump/Unimplemented tasks"
description = ""
date = 2010-11-10T02:04:58Z
aliases = []
[extra]
id = 3551
[taxonomies]
categories = []
tags = []
+++

{{Vptopic
|topic=Unimplemented tasks
|summary=Announcement of the unimpl pages, and of ImplSearchBot
}}
=It exists!=
I'm really, ''really'' sorry at how insanely busy the Recent Changes log will be by the time this is done.  In other news, I have written a bot that will keep a page of tasks for which each language has not had implementations written for it.  The process of creating these pages is very CPU intensive, and spams the heck out of the Recent Changes log, so I don't think I'm going to have the bot run very frequently.  Probably once per week.  And, yes, it will be getting its own bot account, same as GugaTagFixer.  On the bright side, we ''finally'' have these lists!  --[[User:Short Circuit|Short Circuit]] 08:07, 16 February 2009 (UTC)
:Interesting! How is one expected to find these pages? Are they automatically linked from the language template? How about a [[:Category:Unimplemented tasks]] to index them all? (I was expecting this kind of feature to be an extension to WikiMedia itself, accessed dynamically as a [[Special:SpecialPages|special page]].) --[[User:IanOsgood|IanOsgood]] 15:43, 16 February 2009 (UTC)
:Bug: you should include the Category:RC* tasks with a leading ':' to avoid putting these pages into the category.
:: There's another bug there, too.  The RC* tasks show up in every list, regardless of whether or not they've actually been solved there.  That last problem can be fixed by putting the task category in the language category, but I'll work on the unintentional inclusion into the RC* categories once we settle on what day of the week to actually do the updates; The process of retrieving the data adds a pretty significant load to the server. --[[User:Short Circuit|Short Circuit]] 17:38, 16 February 2009 (UTC)
:Personally, I wouldn't put these pages into each language category. Instead link explicitly from the {language} template. --[[User:IanOsgood|IanOsgood]] 15:49, 16 February 2009 (UTC)
::Agreed. A straight link would be better because putting them in the categories disrupts the count of tasks in the categories. --[[User:Mwn3d|Mwn3d]] 16:40, 16 February 2009 (UTC)
::: Add the appropriate link to [[Template:Language]], and remove it from [[Template:unimpl_header]]. --[[User:Short Circuit|Short Circuit]] 17:38, 16 February 2009 (UTC)
:Useful! But maybe putting a "last update" date could help understanding how recent the list is. --[[User:ShinTakezou|ShinTakezou]] 17:06, 16 February 2009 (UTC)
::I think there's a wiki variable you can add to [[Template:unimpl_header]] for that. --[[User:Short Circuit|Short Circuit]] 17:38, 16 February 2009 (UTC)

=To Bot or Not to Bot?=
'''I think that using bots isn't a good thing, it's better try to use a [[PHP]] Script to display unimplemented tasks, take a look at my code. This using rewrite engine will be awesome.'''--[[User:Guga360|Guga360]] 21:59, 16 February 2009 (UTC)

```php

<?
if (!isset($_GET['lang'])) die("Language not specified.");
$lang = $_GET['lang'];

function categorytitles($category) {
	$titlesarr = array();
	
	$titles = unserialize(file_get_contents("http://www.rosettacode.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:$category&cmlimit=500&format=php"));
	$titles = $titles['query']['categorymembers'];
	
	foreach ($titles as $i) {
		array_push($titlesarr, $i['title']);
	}
	
	return $titlesarr;
}

$alltasks = categorytitles("Programming_Tasks");
$langtasks = categorytitles($lang);
$notimplemented =  array_diff($alltasks, $langtasks);

?>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">

<html>

<head>
 <title>Not implemented tasks in <? echo $lang; ?> </title>
 <meta http-equiv="content-type" content="text/html;charset=utf-8">
</head>

<body>

<div class="title">
 Not implemented tasks in <? echo $lang; ?>:
 

</div>

<div class="list">
 <ul>
<?
foreach ($notimplemented as $i) {
	?>
  <li><span class="item"><? echo $i; ?></span></li>
<?
}
	?>
 </ul>
</div>
	
</body>
</html>

```

: There are two main reasons I didn't go with that approach.
:# The intersection is calculated every time the page is viewed, which means two queries for each view.  That will fail under loads far lighter than MediaWiki will otherwise normally handle.
:# The data doesn't show up as a MediaWiki page, so it doesn't have a talk page, isn't editable, doesn't show up in the "What links here" list, isn't trivially linkable, doesn't support MediaWiki templates, doesn't match the user's skin, doesn't take advantage of the caching and tuning done for MW, etc.
: My original attempt was to put the entire list in one page, but Apache gave me an Internal Server Error when I tried feeding MediaWiki a 828KB chunk of wikicode. --[[User:Short Circuit|Short Circuit]] 22:36, 16 February 2009 (UTC)
