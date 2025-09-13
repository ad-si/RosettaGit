+++
title = "Rosetta Code/Run examples"
description = ""
date = 2018-06-22T22:57:10Z
aliases = []
[extra]
id = 10925
[taxonomies]
categories = ["task"]
tags = []
+++

This task is based on an idea hatched from this [[C1R Implementation]].

Write a program that will accept as input the name of a task from Rosetta Code and the name of a language. The program should then download the solution for the specified task in the specified language, present the source to the user and prompt the user to confirm running the example. 

The program should verify that the tools needed to compile or run the solution are present before running it. 
If the solution can not be run, a graceful exit should happen. (i.e. the program should not crash)

Besides its own language, the program should support at least two other languages. (Ideally it would support most of the languages available, but that is too much to ask. Though some languages are simple, e.g. python, pike, perl, bash and several more only need the solution saved in a file and given to the language as argument to run, so it should be easy to support many languages like that).

If you know what is needed to support a particular language, please help to add support for that language to implementations in other languages.

Extra credit: add a function to get a list of all solutions of a given language and run them to create a report on which solutions failed to run.

More credit: also test if the output of a solution compares to a given result. The expected output should be loaded from a file with the name of the task. (This way all implementations can share the same set of files, and anyone can add more files. In the future the content of these files could be stored directly in a section of the task on Rosetta Code itself.)


## Liberty BASIC


```lb

'   ********************************************************************
'   **                                                                **
'   **    parseAndRun.bas  v26b  tenochtitlanuk    November 2012      **
'   **                                                                **
'   **      select a LB solution from RC site & run it locally        **
'   **                                                                **
'   ********************************************************************
'retrieve proper temporary path and filename to save downloaded HTML:
Source$ = GetTempFileName$("htm")
'nomainwin

'   Download main RC LB page which has current tasks on it. Save as 'source.html'
'   run "C:\Program Files\Mozilla Firefox\firefox.exe http://rosettacode.org/wiki/Category:Liberty_BASIC"   'testing routine
print " Fetching current RC page of completed Liberty BASIC RC solutions."
'result = DownloadToFile( "http://rosettacode.org/wiki/Category:Liberty_BASIC", "E:\source.html")
result = DownloadToFile( "http://rosettacode.org/wiki/Category:Liberty_BASIC", Source$)

if result <>0 then  print "Error downloading LB solved tasks.": end else print: print " Displaying solved tasks.": print

'   Load source into a string. Go through and save in a 2D array all topic titles
'       and the appropriate web addresses to find them.
'open "E:\source.html" for input as #f
open Source$ for input as #f
    html$ = input$( #f, lof( #f))
close #f
kill Source$   'remove temp file

dim solutions$( 500, 2)

global count
count   =1
first   =0
last    =0
reading =0

'   The first topic is the '100 doors' so skip all html jump ref's earlier than this.
do
    r$ =getHtmlSection$( html$, first, last)
    if instr( r$, "/rosettacode.org/mw/index.php") then exit do '   We've read all LB solved tasks.
    if r$ ="wiki/100_doors" then reading =1

    if reading =1 then   '   we can start recording path & name
        solutions$( count, 1) ="http://rosettacode.org/" +r$ +"#Liberty_BASIC"

        special =instr( r$, "%2B"):    if special <>0 then r$ =left$( r$, special -1) +"+" +mid$( r$, special +3)
        special =instr( r$, "%27"):    if special <>0 then r$ =left$( r$, special -1) +"'" +mid$( r$, special +3)
        special =instr( r$, "%C3%A8"): if special <>0 then r$ =left$( r$, special -1) +chr$( 232) +mid$( r$, special +6)
        solutions$( count, 0) =mid$( r$, 6)  '   we want the bit beyond '/wiki/'
        if instr(  solutions$( count, 0), "/") then
            newName$ =""
            for ii =1 to len(  solutions$( count, 0) )
                n$ =mid$( solutions$( count, 0), ii, 1)
                if n$ ="/" then n$ ="_"
                newName$ =newName$ +n$
            next ii
            solutions$( count, 0) =newName$
        end if
        print count, solutions$( count, 0)'; tab( 60); solutions$( count, 1)
        count =count +1
    end if
loop until 0
print: print count -1; " tasks solved in LB."

'input " Choose task # "; R                                              '   Choose a page to try.
for R =1 to 283
print
print " Choosing a task at random viz #"; R; " out of "; count -1; " completed in LB."
print " Task is "; chr$( 34); solutions$( R, 0); chr$( 34)
print

'********************run "C:\Program Files\Mozilla Firefox\firefox.exe " +solutions$( R, 1)

'   Fetch the RC task page with all the sol'ns including LB one.
print " Downloading the page for this task."
result = DownloadToFile( solutions$( R, 1), "rx.html")

if result <>0 then  print "Error downloading.": end

print " Now finding the LB section of the html code."   '   Now finding the appropriate LB section on this topic.

open "rx.html" for input as #r
    L =lof( #r)
    print " Length of source html of this topic's page is "; L
    t$ =input$( #r, L)
close #r

preamble$ =">Liberty BASIC</a></span></h2>" +chr$( 10)

lP =len( preamble$)
print " Finding the preamble string at ";
beg =instr( t$, preamble$)' +len( preamble$)
print beg

lookFor$ ="source" +chr$( 34) +">"
beg =instr( t$, lookFor$, beg)    '   get to start of BASIC code.
beg =beg +len( lookFor$)

print " Found LB section at "; beg;

fin =instr( t$, "
```
", beg)
print " and ending at "; fin

print " Chopping off unwanted earlier & later sections of html source."
t$ =mid$( t$, beg, fin -beg)    '   discard earlier & later parts of html code.

open solutions$( R, 0) +".txt" for output as #LbText
    #LbText t$;
close #LbText

L =len( t$)

print " Relevant html code LB section being parsed for LB BASIC code."

'   Read the rest of the LB code section to 
```
 section ..
LB$  =""
j    =1

print " Dropping html tags & translating html entities."
print
print " LB code follows."
print

do
    nxtChr$  =mid$( t$, j, 1)
    select case '   _______________________________________________________________________________________________________
        case ( nxtChr$ =chr$( 10)) or ( nxtChr$ =chr$( 13))
            j =L
            print "End reached- CRLF"

        case nxtChr$ ="<"                                                           '   we've found a html tag. Omit.
            'print " Starting a tag with a <";
            item$ ="<"
            do                                                                      '   keep looking until find a '>' or finish...
                j =j +1
                nxtChr$  =mid$( t$, j, 1)
                item$ =item$ +nxtChr$
            loop until nxtChr$ =">"
            'print " Closing a tag with a >."
            if item$ ="
```
" then j =L                                            '   end reached
            if item$ ="<br />" then LB$ =LB$ +chr$( 10)                             '   code for CRLF
            if item$ ="<br/>" then LB$ =LB$ +chr$( 10)                             '   code for CRLF, now
            if j <>L then j =j +1

        case nxtChr$ ="&"                                                           '   we've found an html entity.
                                                                                    '   replace with plain-text equivalents.
            'print " html entity starting with & ";
            select case '   ..............................................................................
                case mid$( t$, j+1, 5) ="quot;"
                    LB$ =LB$ +chr$( 34): j =j +6                                    '   &guot;    "
                case mid$( t$, j+1, 3) ="gt;"
                    LB$ =LB$ +">": j =j +4                                          '   >      >
                case mid$( t$, j+1, 3) ="lt;"
                    LB$ =LB$ +"<": j =j +4                                          '   <      <
                case right$( mid$( t$, j, 5), 1) =";"
                    v =val( mid$( t$, j +2, 2)): j =j +5                            '   2-digit character-code
                    if v =39 then LB$ =LB$ +chr$( 39) else LB$ =LB$ +chr$( v)       '       eg '  ( 40,41)   '()
                case right$( mid$( t$, j, 6), 1) =";"                               '   3-digit character-code
                    v =val( mid$( t$, j +2, 3))
                    if v =160 then v =32    'print "Hard space!"                    '   convert   hard- to soft-space.
                    j =j +6: LB$ =LB$ +chr$( v)
            end select  '   ..............................................................................
            'print " and finishing with ;"
        case else  '   not an html entity nor a tag. Use as-is unless it's the final hard-space plus semi-colon..
            if mid$( t$, j +1, 5) ="#160;" and mid$( t$, j +5, 6) ="
```
" then j =L else LB$ =LB$ +nxtChr$: j =j +1

    end select  '   _________________________________________________________________________________________________________
    scan
loop until j >= fin -beg -4

print: print LB$

open solutions$( R, 0) +".bas" for output as #LB
    #LB LB$;
close #LB

print
print " Done"

timer 5000, [on2]
wait
[on2]
timer 0

'   Run with LB.
'   *************************************run chr$( 34) +"C:\Program Files\Liberty BASIC v4.04\liberty.exe" +chr$( 34) +" -R E:\" +solutions$( R, 0) +".bas"
next R
end
    '   **************************************************************
Function DownloadToFile( urlfile$, localfile$)
    open "URLmon" for dll as #url
    calldll #url, "URLDownloadToFileA",_
    0 as long,_         'null
    urlfile$ as ptr,_   'url to download
    localfile$ as ptr,_ 'save file name
    0 as long,_         'reserved, must be 0
    0 as long,_         'callback address, can be 0
    DownloadToFile as ulong  '0=success
    close #url
end function
end

function getHtmlSection$( string$, byref first, last)
    a                    =instr(  string$, "<a href=" +chr$( 34), first)
        if a =0 then getHtmlSection$ ="  Sorry! html link not found": exit function
    b                    =instr(  string$, chr$( 34), a +9)
    getHtmlSection$      =mid$(   string$, a +10, b -a -10)
    first                =b +1
            '   Reset value of "first" so that in the next call to
            '   getHtmlSection$( the next html link can be found
end function

function GetTempFileName$(prefix$)
    TempPath$=GetTempPath$()
    TempFile$ = space$(256)+chr$(0)

    calldll #kernel32, "GetTempFileNameA",_
    TempPath$ as ptr,_  'directory for temp file
    prefix$ as ptr,_    'desired prefix for temp filename
    0 as ulong,_        '0=file created,nonzero=you must create file
    TempFile$ as ptr,_  'string buffer to hold qualified path and filename
    result as ulong     'nonzero=success

    'TempFile$ holds complete path and filename info
    GetTempFileName$ = TempFile$
    end function

Function GetTempPath$()
    CallDLL #kernel32, "GetTempPathA",_
    0 as long,_
    _NULL as long,_
    length as long

    buf$ = space$(length)

    CallDLL #kernel32, "GetTempPathA",_
    length as long,_
    buf$ as ptr,_
    ret as long

    GetTempPath$ = buf$
End Function

```



## Perl 6

This is a fairly comprehensive task code runner. It is set up to work for Perl 6 by default, but has basic configurations to run Perl and Python  tasks as well. It can be easily tweaked to work with other languages by adding a load-lang('whatever'){} routine similar to the Perl6, Perl and Python ones. (And ensuring that the appropriate compiler is installed and accessible.) There is so much variation to the task requirements and calling conventions that it would be problematic to make a general purpose, language agnostic code runner so some configuration is necessary to make it work with other languages.

By default, this will download the Perl 6 section of any (every) task that has a Perl 6 example, extract the code blocks and attempt to run them. Many tasks require files or user interaction to proceed, others are not complete runnable code blocks (example code fragments), some tasks run forever.  To try to deal with and compensate for this, this implementation can load a %resource hash that will: supply input files where necessary, skip unrunnable code fragments, limit long and/or infinite running blocks, supply user interaction code where possible, and skip blocks where user interaction is unavoidable.

There are several command line options to control its actions. See the README in the repository for details.

The complete implementation is too large and cumbersome to post in it's entirety here, only the main task retrieval and execution code is included.

For the whole ball of wax see [https://github.com/thundergnat/rc-run the rc-run github repository].

Run with no parameters to run every implemented task on Rosetta Code. Feed it a task name to only download / run that task. Give it command line switches to adjust its behaviour.

Note: This is set up to run under Linux. It could be adapted for Windows (or OSX I suppose) fairly easily but I don't have access to those OSs, nor do I care to seek it.


```perl6
use HTTP::UserAgent;
use URI::Escape;
use JSON::Fast;
use MONKEY-SEE-NO-EVAL;

my %*SUB-MAIN-OPTS = :named-anywhere;

unit sub MAIN(
    Str $run = '',        # Task or file name
    Str :$lang = 'perl6', # Language, default perl6 - should be same as in <lang *> markup
    Int :$skip = 0,       # Skip # to continue partially into a list
    Bool :f(:$force),     # Override any task skip parameter in %resource hash
    Bool :l(:$local),     # Only use code from local cache
    Bool :r(:$remote),    # Only use code from remote server (refresh local cache)
    Bool :q(:$quiet),     # Less verbose, don't display source code
    Bool :d(:$deps),      # Load dependencies
    Bool :p(:$pause),     # pause after each task
    Bool :b(:$broken),    # pause after each task which is broken or fails in some way
);

die 'You can select local or remote, but not both...' if $local && $remote;

## INITIALIZATION

my $client   = HTTP::UserAgent.new;
my $url      = 'http://rosettacode.org/mw';

my %c = ( # text colors
    code  => "\e[0;92m", # green
    delim => "\e[0;93m", # yellow
    cmd   => "\e[1;96m", # cyan
    warn  => "\e[0;91m", # red
    dep   => "\e[40;36m",
    clr   => "\e[0m",    # clear formatting
);

my $view      = 'xdg-open';       # image viewer, this will open default under Linux
my %l         = load-lang($lang); # load languge parameters
my %resource  = load-resources($lang);
my $get-tasks = True;

my @tasks;

run('clear');

## FIGURE OUT WHICH TASKS TO RUN

if $run {
    if $run.IO.e and $run.IO.f {# is it a file?
        @tasks = $run.IO.lines; # yep, treat each line as a task name
    } else {                    # must be a single task name
        @tasks = ($run);        # treat it so
    }
    $get-tasks = False;         # don't need to retrieve task names from web
}

if $get-tasks { # load tasks from web if cache is not found, older than one day or forced
    if !"%l<dir>.tasks".IO.e or (now - "%l<dir>.tasks".IO.modified) > 86400 or $remote {
        note 'Retrieving task list from site.';
        @tasks = mediawiki-query( # get tasks from web
        $url, 'pages',
        :generator<categorymembers>,
        :gcmtitle("Category:%l<language>"),
        :gcmlimit<350>,
        :rawcontinue(),
        :prop<title>
        )»<title>.grep( * !~~ /^'Category:'/ ).sort;
        "%l<dir>.tasks".IO.spurt: @tasks.sort.join("\n");
    } else {
        note 'Using cached task list.';
        @tasks = "%l<dir>.tasks".IO.slurp.lines; # load tasks from file
    }
}

note "Skipping first $skip tasks..." if $skip;
my $redo;

## MAIN LOOP

for @tasks -> $title {
    $redo = False;
    next if $++ < $skip;
    next unless $title ~~ /\S/; # filter blank lines (from files)
    say my $tasknum = $skip + ++$, ")  $title";

    my $name = $title.subst(/<-[-0..9A..Za..z]>/, '_', :g);
    my $taskdir = "./rc/%l<dir>/$name";

    my $modified = "$taskdir/$name.txt".IO.e ?? "$taskdir/$name.txt".IO.modified !! 0;

    my $entry;
    if $remote or !"$taskdir/$name.txt".IO.e or ((now - $modified) > 86400 * 7) {
        my $page = $client.get("{ $url }/index.php?title={ uri-escape $title }&action=raw").content;

        uh-oh("Whoops, can't find page: $url/$title :check spelling.") and next if $page.elems == 0;
        say "Getting code from: http://rosettacode.org/wiki/{ $title.subst(' ', '_', :g) }#%l<language>";

        $entry = $page.comb(/'=={{header|' $(%l<header>) '}}==' .+? [<?before \n'=='<-[={]>*'{{header'> || $] /).Str //
          uh-oh("No code found\nMay be bad markup");

        if $entry ~~ /^^ 'See [[' (.+?) '/' $(%l<language>) / { # no code on main page, check sub page
            $entry = $client.get("{ $url }/index.php?title={ uri-escape $/[0].Str ~ '/' ~ %l<language> }&action=raw").content;
        }
        mkdir $taskdir unless $taskdir.IO.d;
        spurt( "$taskdir/$name.txt", $entry );
    } else {
        if "$taskdir/$name.txt".IO.e {
            $entry = "$taskdir/$name.txt".IO.slurp;
            say "Loading code from: $taskdir/$name.txt";
        } else {
            uh-oh("Task code $taskdir/$name.txt not found, check spelling or run remote.");
            next;
        }
    }

    my @blocks = $entry.comb: %l<tag>;

    unless @blocks {
        uh-oh("No code found\nMay be bad markup") unless %resource{"$name"}<skip> ~~ /'ok to skip'/;
        say "Skipping $name: ", %resource{"$name"}<skip>, "\n" if %resource{"$name"}<skip>
    }

    for @blocks.kv -> $k, $v {
        my $n = +@blocks == 1 ?? '' !! $k;
        spurt( "$taskdir/$name$n%l<ext>", $v );
        if %resource{"$name$n"}<skip> && !$force {
            dump-code ("$taskdir/$name$n%l<ext>");
            if %resource{"$name$n"}<skip> ~~ /'broken'/ {
                uh-oh(%resource{"$name$n"}<skip>);
                pause if $broken;
            } else {
                say "Skipping $name$n: ", %resource{"$name$n"}<skip>, "\n";
            }
            next;
        }
        say "\nTesting $name$n";
        run-it($taskdir, "$name$n", $tasknum);
    }
    say  %c<delim>, '=' x 79, %c<clr>;
    redo if $redo;
    pause if $pause;

}

## SUBROUTINES

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

sub run-it ($dir, $code, $tasknum) {
    my $current = $*CWD;
    chdir $dir;
    if %resource{$code}<file> -> $fn {
        copy "$current/rc/resources/{$fn}", "./{$fn}"
    }
    dump-code ("$code%l<ext>") unless $quiet;
    check-dependencies("$code%l<ext>", $lang) if $deps;
    my @cmd = %resource{$code}<cmd> ?? |%resource{$code}<cmd> !! "%l<exe> $code%l<ext>\n";
    for @cmd -> $cmd {
        say "\nCommand line: {%c<cmd>}$cmd",%c<clr>;
        try shell $cmd;
        CATCH {
            when /'exit code: 137'/ { }
            default {
                .resume unless $broken;
                uh-oh($_);
                if pause.lc eq 'r' {
                   unlink "$code.txt";
                   $redo = True;
               }
           }
        }
    }
    chdir $current;
    say "\nDone task #$tasknum: $code";
}

sub pause {
    prompt "Press enter to procede:> ";
    # or
    # sleep 5;
}

sub dump-code ($fn) {
    say "\n", %c<delim>, ('vvvvvvvv' xx 7).join(' CODE '), %c<clr>, "\n", %c<code>;
    print $fn.IO.slurp;
    say %c<clr>,"\n\n",%c<delim>,('^^^^^^^^' xx 7).join(' CODE '),%c<clr>;
}

sub uri-query-string (*%fields) { %fields.map({ "{.key}={uri-escape .value}" }).join('&') }

sub clear { "\r" ~ ' ' x 100 ~ "\r" }

sub uh-oh ($err) { put %c<warn>, "{'#' x 79}\n\n $err \n\n{'#' x 79}", %c<clr> }

multi check-dependencies ($fn, 'perl6') {
    my @use = $fn.IO.slurp.comb(/<?after ^^ \h* 'use '> \N+? <?before \h* ';'>/);
    if +@use {
        for @use -> $module {
            next if $module eq any('v6','nqp', 'NativeCall') or $module.contains('MONKEY')
              or $module.contains('experimental') or $module.starts-with('lib');
            my $installed = $*REPO.resolve(CompUnit::DependencySpecification.new(:short-name($module)));
            print %c<dep>;
            if $installed {
                say 'ok:            ', $module
            } else {
                say 'not installed: ', $module;
                shell("zef install $module");
            }
            print %c<clr>;
        }
    }
}

multi check-dependencies ($fn, 'perl') {
    my @use = $fn.IO.slurp.comb(/<?after $$ \h* 'use '> \w+['::'\w+]* <?before \N+? ';'>/);
    if +@use {
        for @use -> $module {
            next if $module eq $module.lc;
            my $installed = shell( "%l<exe> -e 'eval \"use {$module}\"; exit 1 if \$@'" );
            print %c<dep>;
            if $installed {
                say 'ok:            ', $module
            } else {
                say 'not installed: ', $module;
                try shell("sudo cpan $module");
            }
            print %c<clr>;
        }
    }
}

multi check-dependencies  ($fn, $unknown) {
    note "Sorry, don't know how to handle dependancies for $unknown language."
};

multi load-lang ('perl6') { ( # Language specific variables. Adjust to suit.
    language => 'Perl_6', # language category name
    exe      => 'perl6',  # executable name to run perl6 in a shell
    ext      => '.p6',    # file extension for perl6 code
    dir      => 'perl6',  # directory to save tasks to
    header   => 'Perl 6', # header text
    # tags marking blocks of code - spaced out to placate wiki formatter
    # and to avoid getting tripped up when trying to run _this_ task
    tag => rx/<?after '<lang ' 'perl6' '>' > .*? <?before '</' 'lang>'>/,
) }

multi load-lang ('perl') { (
    language => 'Perl',
    exe      => 'perl',
    ext      => '.pl',
    dir      => 'perl',
    header   => 'Perl',
    tag => rx/:i <?after '<lang ' 'perl' '>' > .*? <?before '</' 'lang>'>/,
) }

multi load-lang ('python') { (
    language => 'Python',
    exe      => 'python',
    ext      => '.py',
    dir      => 'python',
    header   => 'Python',
    tag => rx/:i <?after '<lang ' 'python' '>' > .*? <?before '</' 'lang>'>/,
) }

multi load-lang ($unknown) { die "Sorry, don't know how to handle $unknown language." };

multi load-resources ($unknown) { () };
```


{{out}} with command line: '''perl6 RC-run.p6 -q "Determine if a string is numeric"'''

```txt
Retrieving tasks
1 Determine if a string is numeric
Getting code from: http://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Perl_6

Testing Determine_if_a_string_is_numeric

Command line: perl6 Determine_if_a_string_is_numeric.p6
               Coerce     Don't coerce
    String   whitespace    whitespace
       <1>      True         True
     <1.2>      True         True
   <1.2.3>     False        False
      <-6>      True         True
     <1/2>      True         True
     <12e>     False        False
     <B17>     False        False
 <1.3e+12>      True         True
  <1.3e12>      True         True
 <-2.6e-3>      True         True
    <zero>     False        False
      <0x>     False        False
   <0xA10>      True         True
  <0b1001>      True         True
    <0o16>      True         True
    <0o18>     False        False
    <2+5i>      True         True
   <1 1 1>     False        False
        <>      True        False
       < >      True        False

Done Determine_if_a_string_is_numeric

### =========================================================================

```

Or, if the full %resource hash is loaded it will automatically feed input parameters to tasks that require them:

'''perl6 RC-run.p6 Lucky_and_even_lucky_numbers''' -q

```txt
Retrieving tasks
1 Lucky_and_even_lucky_numbers
Getting code from: http://rosettacode.org/wiki/Lucky_and_even_lucky_numbers#Perl_6

Testing Lucky_and_even_lucky_numbers

Command line: perl6 Lucky_and_even_lucky_numbers.p6 20 , lucky

79

Command line: perl6 Lucky_and_even_lucky_numbers.p6 1 20

(1 3 7 9 13 15 21 25 31 33 37 43 49 51 63 67 69 73 75 79)

Command line: perl6 Lucky_and_even_lucky_numbers.p6 1 20 evenlucky

(2 4 6 10 12 18 20 22 26 34 36 42 44 50 52 54 58 68 70 76)

Command line: perl6 Lucky_and_even_lucky_numbers.p6 6000 -6100

(6009 6019 6031 6049 6055 6061 6079 6093)

Command line: perl6 Lucky_and_even_lucky_numbers.p6 6000 -6100 evenlucky

(6018 6020 6022 6026 6036 6038 6050 6058 6074 6090 6092)

Done Lucky_and_even_lucky_numbers

### =========================================================================

```



## Run BASIC


```runbasic
bf$	= "<SPAN STYLE='font-family:Arial; font-weight:700; font-size:12pt'>"
a$	= httpGet$("http://rosettacode.org/wiki/Category:Run_BASIC") ' get RB tasks from [RC]
a1$	= word$(a$,2,"Pages in category ""Run BASIC")
a1$	= word$(a1$,1,"</tr></table>")
i	= 2
b$	= word$(a1$,i,"<li><a href=""/wiki/")
'
' Create a drop down window for selection of a task
'
html bf$;"<center><TABLE BORDER=1 CELLPADDING=0 CELLSPACING=0 bgcolor=wheat>"
html "<TR align=center><TD colspan=2>Tasks</TD></TR><TR>"
html "<TD align=right>Task</TD><TD>"

html "<select size=10 id='runProg' name='runProg'>" 
while b$ <> ""
  b$	= left$(b$,instr(b$,"""")-1)
  b$	= strRep$(b$,"%2B","+")
  b$	= strRep$(b$,"%27","'")
  html "<option>"+b$+"</option>"
  i 	= i + 1
  b$	= word$(a1$,i,"<li><a href=""/wiki/")
wend
html "</select></TD></TR><TR><TD colspan=2 ALIGN=CENTER>"

' BUTTON options to Run It or Exit
    button #run, "Run It", [runProg]
    button #ex, "Exit", [quit]

html "</TD></TR></TABLE></center>"  ' close the drop down table and wait
wait

[runProg]
progName$	= #request get$("runProg")
print progName$
a$	= httpGet$("http://rosettacode.org/wiki/"+progName$)

i	= instr(a$,"<a href=""#Run_BASIC"">")

a$	= mid$(a$,i-6,6)
a$	= word$(a$,2,"-")
a$	= word$(a$,1,"""")
cls                            ' clear screen
'print a$                      ' this is the program number used in the [RC] editor

a$ = httpGet$("http://rosettacode.org/mw/index.php?title="+progName$+"&action=edit&section="+a$)

a$	= word$(a$,2,"{header|Run BASIC}")
i	= instr(a$,">")
a$	= mid$(a$,i+1)
i 	= instr(a$,"/lang>")
a$	= left$(a$,i-5)
a$	= strRep$(a$,"&lt;","<")               ' this is the good program code
' place the code in the rb$ file
rb$     = DefaultDir$ + "\projects\a_project\rcCode.bas" ' RC program
open rb$ for output as #f
print #f,a$
close #f

print "
### =============== Run Basic Solution ========================
"
run rb$,#handle      ' point RunBasic to the file with the program
render #handle       ' render the runned code
[quit]               ' that's it folks
end

' --------------------------------
' string replace rep str with
' --------------------------------
FUNCTION strRep$(str$,rep$,with$)
ln  = len(rep$)
ln1 = ln - 1
i = 1
while i <= len(str$)
    if mid$(str$,i,ln) = rep$ then
        strRep$ = strRep$ + with$
        i = i + ln1
    else
        strRep$ = strRep$ + mid$(str$,i,1)
    end if
i = i + 1
WEND
END FUNCTION
```



## Tcl

This code only includes support for running Tcl task solutions, but it can download any language's; it assumes that the first <nowiki><lang…></nowiki> is sufficient when it comes to task extraction (definitely not true universally, but mostly good enough).
```tcl
# Code to download task contents from find-bare-lang-tags task
package require Tcl 8.5
package require http
package require uri

proc getUrlWithRedirect {base args} {
    set url $base?[http::formatQuery {*}$args]
    while 1 {
	set t [http::geturl $url]
	if {[http::status $t] ne "ok"} {
	    error "Oops: url=$url\nstatus=$s\nhttp code=[http::code $token]"
	}
	if {[string match 2?? [http::ncode $t]]} {
	    return $t
	}
	# OK, but not 200? Must be a redirect...
	set url [uri::resolve $url [dict get [http::meta $t] Location]]
	http::cleanup $t
    }
}
proc getTaskContent {task} {
    set token [getUrlWithRedirect http://rosettacode.org/mw/index.php \
	    title $task action raw]
    set content [http::data $token]
    http::cleanup $token
    return $content
}

# Code to extract the first <lang> section for a language
proc getTaskCodeForLanguage {task language} {
    set content [getTaskContent $task]
    set startRE {==\s*\{\{header\|@LANG@(?:\|[^{}]+)?\}\}\s*==}
    set startRE [string map [list @LANG@ $language] $startRE]
    if {![regexp -indices $startRE $content start]} {
	error "$language does not implement task \"$task\""
    }
    if {![regexp -indices -start [lindex $start end] \
	      "==\\s*\\\{\\\{header" $content end]} {
	set end {end end}
    }
    set content [string range $content [lindex $start 1] [lindex $end 0]]
    # Extended format RE used to allow embedding within _this_ task's <lang>!
    if {![regexp {(?x)<lang .*?>(.*?)</ lang>} $content -> solution]} {
	error "$language solution of task \"$task\" has no useful code"
    }
    return "$solution\n"
}

# How to download and run a Tcl task
proc runTclTaskForLanguage {task} {
    puts "Fetching task solution..."
    set solution [getTaskCodeForLanguage $task Tcl]
    set filename rcsoln_[string map {/ _ " " _} $task].tcl
    set f [open $filename w]
    puts $f $solution
    close $f
    puts "Executing task solution with: tclsh $filename"
    exec [info nameofexecutable] $filename <@stdin >@stdout 2>@stderr
}
runTclTaskForLanguage {*}$argv
```



## UNIX Shell

See [[C1R Implementation]] for an incomplete implementation. (only supports [[C]])
