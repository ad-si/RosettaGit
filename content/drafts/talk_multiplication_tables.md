+++
title = "Talk:Multiplication tables"
description = ""
date = 2012-07-10T07:53:02Z
aliases = []
[extra]
id = 8166
[taxonomies]
categories = []
tags = []
+++

==Perl 6 Multiplication Table Output glitch==
(Copied from Tims talk page)

You might want to check [[Multiplication tables#Perl_6|this output]]. The axis '+' is off. --[[User:Paddy3118|Paddy3118]] 03:50, 27 August 2010 (UTC)

:Hi Tim, i see you've made an edit, but the problem persists. I'll go into it a little more:

:I see the following:
:<lang>   x┃   1   2   3   4   5   6   7   8   9  10  11  12
 ━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   1┃   1   2   3   4   5   6   7   8   9  10  11  12
```

:When I think it should be:
:<lang>   x┃   1   2   3   4   5   6   7   8   9  10  11  12
 ━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   1┃   1   2   3   4   5   6   7   8   9  10  11  12
```
 
:I think you may have a field width off by one or two too many? --[[User:Paddy3118|Paddy3118]] 04:06, 27 August 2010 (UTC)

Hi again, I see you made another edit. I checked the Python entry which uses Unicode characters for the table but shows the output under the lang tag. I temporarily edited the Perl 6 output to show what happens when the output uses lang python, and it still looked 'off'. All I can suggest is to look at the output of the Python or other examples that seem to have the output you are after and compare the outputs character-by-character to find out what is happening. --[[User:Paddy3118|Paddy3118]] 05:55, 27 August 2010 (UTC)

I don't think my output is off.  It works fine in my browser, and in my constant width terminal screens from which pasted the output.  $width is 3, and that's how many ━ characters it is supposed to be putting out, and that's how many it is putting out before the ╋.  I wonder if your font widths are off somehow in your browser.  --Larry

Hi again. What I have found is that on both IE8 and Mozilla 3.6.8 on Windows XP  I find the dash that you use to be much longer than the space or any other character as they are supposed to be displayed in a mono-spaced font, and indeed, the body of the table is correctly and equally mono-spaced. The dash you use shows up as too long. The non-ASCII dash used in the Python example is of the correct and equal size??? I'll copy this to the appropriate talk page for other input. --[[User:Paddy3118|Paddy3118]] 18:39, 27 August 2010 (UTC)

Anyone else see a problem with the dashes on the Perl-6 entry? --[[User:Paddy3118|Paddy3118]] 18:44, 27 August 2010 (UTC)

I do not have a working copy of perl 6, so I can not test this change, but I would suggest:


```perl6
 --~~~~
my $max = 12;
my $width = chars $max**2;
my $f = "%{$width}s";
 
say 'x'.fmt($f), '│ ', (1..$max).fmt($f);
say '─' x $width, '┼', '─' x $max*$width + $max;
for 1..$max -> $i {
    say $i.fmt($f), '│ ', (
        for 1..$max -> $j {
            $i <= $j ?? $i*$j !! '';
        }
    ).fmt($f);
}
```


--[[User:Rdm|Rdm]] 19:47, 2 September 2010 (UTC)

: In my browser (Firefox 3.5.11 on Linux), it looks like the following:
<div style="border: solid">
[[File:Bildschirmfoto-Talk-Multiplication_tables_-_Rosetta_Code_-_Mozilla_Firefox.png]]
</div>
: As you see, the "wrong" version looks right, and the "right" version looks wrong. --[[User:Ce|Ce]] 21:16, 2 September 2010 (UTC)

:: Yes.  The problem is that windows does not deal with those line drawing characters very well.  But that proposed fix was not a good fix -- it deals with line drawing characters which are scaled wrong under windows by removing some characters, which is means it fails under linux where they are the right size.  But have you tried my fix -- my fix used a different set of line drawing characters? (I have used that other set on this site with fewer windows induced problems.) --[[User:Rdm|Rdm]] 21:58, 2 September 2010 (UTC)

::: I had checked using firefox 3.6 over gnome on Solaris 10 and still saw the problem. [[File:Mult.gif]] 
::: --[[User:Paddy3118|Paddy3118]] 06:43, 3 September 2010 (UTC). --[[User:Paddy3118|Paddy3118]] 08:35, 3 September 2010 (UTC)

==REXX==
: I think this test for EBCDIC is incorrect:
:: ebcdic='f0'==1                         /*is this an EBCDIC machine? */ 
: correct:
:: ebcdic=('f1'x=1)
--[[User:Walterpachl|Walterpachl]] 07:25, 10 July 2012 (UTC)
