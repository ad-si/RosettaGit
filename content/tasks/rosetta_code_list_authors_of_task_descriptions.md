+++
title = "Rosetta Code/List authors of task descriptions"
description = ""
date = 2019-06-09T22:11:36Z
aliases = []
[extra]
id = 12279
[taxonomies]
categories = ["task"]
tags = []
+++

{{draft task}}In this task, the goal is to compile an authorship list for task descriptions. A pseudocode example (in imperative style) that should accomplish this is as follows:


```pseudocode
for each task page
  grab page source, discard everything after the first ==section==.
Cache as $previous. Note $author.
  for each revision
    grab page source, discard everything after first ==section==.
Cache as $previous2. Note $author2
    compare $previous2 to $previous. If different, record $author to $list.
    replace $previous with $previous2
    replace $author with $author2
```


The following resources for HTTP interface information for MediaWiki may prove to be useful:
* [https://www.mediawiki.org/wiki/Index.php#Raw https://www.mediawiki.org/wiki/Index.php#Raw]
* [https://www.mediawiki.org/wiki/Index.php#History https://www.mediawiki.org/wiki/Index.php#History]
* [https://www.mediawiki.org/wiki/API:Main_page https://www.mediawiki.org/wiki/API:Main_page]

Conversely, some languages have libraries which abstract these interfaces into language-native idioms. Use of these abstractions is perfectly fine.


Please '''DO NOT''' add a full output for ''each'' programming language; just show a representative sample. For an full listing, see [[Rosetta_Code/List_authors_of_task_descriptions/Full_list]].



## Perl 6

The pseudocode above is no longer really useful as the page format has changed significantly since this task was written. Rather than checking '''every''' edit to see if it was a change to the task description, we'll just assume the user that created the page is the task author. This isn't 100% accurate; a very few pages got renamed and recreated by someone other than the original author without preserving the history, so they are misreported (15 Puzzle Game for instance,) but is as good as it is likely to get without extensive manual intervention. Subsequent edits to the task description are not credited. As it is, we must still make ''thousands'' of requests and pound the server pretty hard. Checking '''every''' edit would make the task several of orders of magnitude more abusive of the server (and my internet connection.) 


```perl6
use HTTP::UserAgent;
use URI::Escape;
use JSON::Fast;
use Sort::Naturally;

# Friendlier descriptions for task categories
my %cat = (
    'Programming_Tasks' => 'Task',
    'Draft_Programming_Tasks' => 'Draft'
);

my $client = HTTP::UserAgent.new;

my $url = 'http://rosettacode.org/mw';

my $tablefile = './RC_Authors.txt';
my $hashfile  = './RC_Authors.json';

my %tasks;

# clear screen
run($*DISTRO.is-win ?? 'cls' !! 'clear');

%tasks = $hashfile.IO.e ?? $hashfile.IO.slurp.&from-json !! ( );
sleep 1;

#=begin update

note 'Retrieving task information...';

my %filter;
for %cat.keys.sort -> $category {
    mediawiki-query(
        $url, 'pages',
        :generator<categorymembers>,
        :gcmtitle("Category:$category"),
        :gcmlimit<350>,
        :rawcontinue(),
        :prop<title>
    ).map( { %filter{.<title>} = %cat{$category} } )
}

my $delete = %tasks.keys (-) %filter.keys;

%tasks.delete($_) for $delete.keys; #Tasks that have changed names or been removed

my @add;
for %filter.keys -> $title {
    if %tasks{$title}:exists {
        %tasks{$title}<category> = %filter{$title} # update status
    } else {
        @add.push: $title => %filter{$title} # New Tasks
    }
}

if @add {
    .say for 'Adding new tasks:', |@add;
}

for @add -> $task {
    mediawiki-query(
        $url, 'pages',
        :titles($task.key),
        :prop<revisions>,
        :rvprop<user|timestamp>,
        :rvstart<2000-01-01T01:01:01Z>,
        :rvdir<newer>,
        :rvlimit<1>
    ).map: {
        print clear, 1 + $++, ' ', .[0]<title>;
        %tasks{.[0]<title>}<category> = $task.value;
        %tasks{.[0]<title>}<author> = .[0]<revisions>[0]<user>;
        %tasks{.[0]<title>}<date> = .[0]<revisions>[0]<timestamp>.subst(/'T'.+$/, '')
    }
}

print clear;

# Save information to a local file
note "\nTask information saved to local file: {$hashfile.IO.absolute}";
$hashfile.IO.spurt(%tasks.&to-json);

#=end update

# Load information from local file
%tasks = $hashfile.IO.e ?? $hashfile.IO.slurp.&from-json !! ( );

# Convert saved task / author info to a table
note "\nBuilding table...";
my $count    = +%tasks;
my $taskcnt  = +%tasks.grep: *.value.<category> eq %cat<Programming_Tasks>;
my $draftcnt = $count - $taskcnt;

# Open a file handle to dump table in
my $out = open($tablefile, :w)  or die "$!\n";

# Add table boilerplate and header
$out.say:
    "\{|class=\"wikitable sortable\"\n",
    "|+ As of { Date.today } :: Total Tasks: { $count }:: Tasks: { $taskcnt }",
    " ::<span style=\"background-color:#ffd\"> Draft Tasks: { $draftcnt } </span>",
    ":: By {+%tasks{*}».<author>.unique} Authors\n",
    "! Author !! Tasks !! Authored"
;

# Get sorted unique list of task authors
for %tasks{*}».<author>.unique.sort(*.&naturally) -> $author {

    # Get list of tasks by this author
    my @these = %tasks.grep( { $_.value.<author> eq $author } );
    my $s = +@these == 1 ?? '' !! 's';

    # Add author and contributions link to the first two cells
    $out.say:
    $author ~~ /\d/
      ?? "|- id=\"$author\"\n|data-sort-value=\"{ sort-key $author }\"|[[User:$author|$author]]\n"~
         "|data-sort-value=\"{ +@these }\"|[[Special:Contributions/$author|"~
         "{ +@these } task{ $s }]]"
      !! "|- id=\"$author\"\n|[[User:$author|$author]]\n"~
         "|data-sort-value=\"{ +@these }\"|[[Special:Contributions/$author|"~
         "{ +@these } task{ $s }]]"
    ;

    if +@these > 2 {
        $out.say: "|style=\"padding: 0px;\"|\n",
          "\{|class=\"broadtable sortable\" style=\"width: 100%;\"\n",
          "! Task Name !! Date Added !! Status";
    }
    else {
        $out.say: "|style=\"padding: 0px;\"|\n",
          "\{|class=\"broadtable\" style=\"width: 100%;\"";
   }

    # Tasks by this author, sorted by name
    for @these.sort({.key.&naturally}) -> $task {

        my $color = $task.value.<category> eq 'Draft' ?? '#ffd' !! '#fff';

        # add the task link, date and status to the table in the second cell
        $out.say: "|-\n|style=\"background-color: $color;\"",
          ( $task.key ~~ /\d/
            ?? " data-sort-value=\"{ sort-key $task.key }\"| [[{uri-escape $task.key}|{$task.key}]]\n"
            !! "| [[{uri-escape $task.key}|{$task.key}]]\n"
          ),
          "|style=\"width: 10em; background-color: $color;\"| {$task.value.<date>}\n",
          "|style=\"width: 6em; background-color: $color;\"| {$task.value.<category>}",
    }
     $out.say: '|}'
}
$out.say( "|}\n" );
$out.close;


note "Table file saved as: {$tablefile.IO.absolute}";

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

sub uri-query-string (*%fields) { %fields.map({ "{.key}={uri-escape .value}" }).join("&") }

sub sort-key ($a) { $a.lc.subst(/(\d+)/, ->$/ {0~(65+($0.chars)).chr~$0},:g) }

sub clear { "\r" ~ ' ' x 100 ~ "\r" }
```


See full output at [[Rosetta_Code/List_authors_of_task_descriptions/Full_list]]

{|class="wikitable sortable"
|+ As of 2018-04-10 :: Total Tasks: 1080:: Tasks: 871 ::<span style="background-color:#ffd"> Draft Tasks: 209 </span>:: By 251 Authors
! Author !! Tasks !! Authored
|-
|data-sort-value="0B2powers"|[[User:2Powers|2Powers]]
|data-sort-value="2"|[[Special:Contributions/2Powers|2 tasks]]
|style="padding: 0px;"|
{|class="broadtable" style="width: 100%;"
|-
|style="background-color: #ffd;"| [[Names%20to%20numbers|Names to numbers]]
|style="width: 10em; background-color: #ffd;"| 2013-05-16
|style="width: 6em; background-color: #ffd;"| Draft
|-
|style="background-color: #ffd;"| [[Solving%20coin%20problems|Solving coin problems]]
|style="width: 10em; background-color: #ffd;"| 2013-05-16
|style="width: 6em; background-color: #ffd;"| Draft
|}
|-
|data-sort-value="0C12.0D175.0C32.0C19"|[[User:12.175.32.19|12.175.32.19]]
|data-sort-value="1"|[[Special:Contributions/12.175.32.19|1 task]]
|style="padding: 0px;"|
{|class="broadtable" style="width: 100%;"
|-
|style="background-color: #fff;"| [[Soundex|Soundex]]
|style="width: 10em; background-color: #fff;"| 2009-11-12
|style="width: 6em; background-color: #fff;"| Task
|}
|-
|data-sort-value="0C12me0C21"|[[User:12Me21|12Me21]]
|data-sort-value="1"|[[Special:Contributions/12Me21|1 task]]
|style="padding: 0px;"|
{|class="broadtable" style="width: 100%;"
|-
|style="background-color: #fff;"| [[Draw%20a%20rotating%20cube|Draw a rotating cube]]
|style="width: 10em; background-color: #fff;"| 2015-05-04
|style="width: 6em; background-color: #fff;"| Task
|}
|-
|colspan="3"|many rows omitted...
|-
|data-sort-value="zorro0E1024"|[[User:Zorro1024|Zorro1024]]
|data-sort-value="2"|[[Special:Contributions/Zorro1024|2 tasks]]
|style="padding: 0px;"|
{|class="broadtable" style="width: 100%;"
|-
|style="background-color: #fff;"| [[Perfect%20shuffle|Perfect shuffle]]
|style="width: 10em; background-color: #fff;"| 2015-04-16
|style="width: 6em; background-color: #fff;"| Task
|-
|style="background-color: #ffd;"| [[Vector|Vector]]
|style="width: 10em; background-color: #ffd;"| 2015-03-21
|style="width: 6em; background-color: #ffd;"| Draft
|}
|-
|data-sort-value="zzo0C38"|[[User:Zzo38|Zzo38]]
|data-sort-value="1"|[[Special:Contributions/Zzo38|1 task]]
|style="padding: 0px;"|
{|class="broadtable" style="width: 100%;"
|-
|style="background-color: #fff;"| [[Thue-Morse|Thue-Morse]]
|style="width: 10em; background-color: #fff;"| 2015-09-20
|style="width: 6em; background-color: #fff;"| Task
|}
|-
|[[User:Русский|Русский]]
|data-sort-value="3"|[[Special:Contributions/Русский|3 tasks]]
|style="padding: 0px;"|
{|class="broadtable sortable" style="width: 100%;"
! Task Name !! Date Added !! Status
|-
|style="background-color: #fff;" data-sort-value="main step of gost 0F28147-0C89"| [[Main%20step%20of%20GOST%2028147-89|Main step of GOST 28147-89]]
|style="width: 10em; background-color: #fff;"| 2012-08-31
|style="width: 6em; background-color: #fff;"| Task
|-
|style="background-color: #ffd;"| [[Old%20Russian%20measure%20of%20length|Old Russian measure of length]]
|style="width: 10em; background-color: #ffd;"| 2013-01-09
|style="width: 6em; background-color: #ffd;"| Draft
|-
|style="background-color: #ffd;"| [[Transportation%20problem|Transportation problem]]
|style="width: 10em; background-color: #ffd;"| 2013-05-24
|style="width: 6em; background-color: #ffd;"| Draft
|}
|}
