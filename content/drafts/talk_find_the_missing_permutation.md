+++
title = "Talk:Find the missing permutation"
description = ""
date = 2010-08-17T19:24:44Z
aliases = []
[extra]
id = 5248
[taxonomies]
categories = []
tags = []
+++

== Perl shuffle ==
An odd observation...I used a [[Perl]] script derived from the [[Knuth shuffle]] task to shuffle an ordered list of permutations. I find it interesting that the left two columns show a greater vertical repeat frequency and length than the right two columns. --[[User:Short Circuit|Michael Mol]] 06:32, 24 December 2009 (UTC)
: Looking at it, I suspect that the Perl impl of that task has a bug in it; it doesn't appear to always guarantee to consider shuffling the first element. (I think. I might have also misread it.) For the Tcl version, what I did was do some frequency analysis to check whether the shuffle was fair; we shuffled the list 1,2,3,4,5 a hundred thousand times and counted up the total for each position in the list across all the runs; when the total for each column was close to 300k, we had a reasonable estimate that there weren't any subtle errors. (We checked for gross errors by eyeballing it.) My perl is very rusty though, so I'm not quite sure how to write the same thing. Perhaps later...
: Do you want me to add in the Tcl solution for this task? –[[User:Dkf|Donal Fellows]] 09:41, 24 December 2009 (UTC)

Hmm, when I test the Perl <tt>shuffle</tt> code with:

```perl
my @tot = (0, 0, 0, 0, 0);
for my $x (1 .. 100000) {
  my @a = shuffle(1, 2, 3, 4, 5);
  for (0 .. 4) {
    $tot[$_] += $a[$_];
  }
}
print "totals: @tot\n"
```

I get totals that indicate that things are OK. Not sure what's wrong then, if anything. Don't think it really matters though; the list of permutations doesn't have an ''obviously'' missing element, so a short program is indeed the best way to find the missing item. –[[User:Dkf|Donal Fellows]] 10:31, 24 December 2009 (UTC)
:I wrote that function. I based it closely on the Wikipedia pseudocode, and I was especially careful to avoid fencepost errors. I'm pretty sure it's correct. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 12:56, 24 December 2009 (UTC)
:: I suspect that it was just someone forgetting that every permutation should be equally likely, even the “unlikely” ones. The testing code indicates fairness (or balanced wrongness of course, but I can't conceive of that code doing it). –[[User:Dkf|Donal Fellows]] 13:15, 24 December 2009 (UTC)
::: Heh. I didn't try running several times, so that one result set definitely isn't a representative sample. However I do use that Perl script for a few other things at home (It's great for shuffling things before they get to xargs when doing '''find'''-based playlists).  I tried adding a reverse and reshuffle stage (the Knuth shuffle itself sits in its own function), and had elements pop up near the beginning of the list I'd forgotten I'd even had. That suggests to me there's something broken about my Perl implementation's rand function. As the Wp artical mentions, your shuffle quality is limited by your source of random numbers. I'll write a program some time today to do a more representative search and check of the results I noticed.  --[[User:Short Circuit|Michael Mol]] 16:05, 24 December 2009 (UTC)
:::: Yeah, [http://www.jstatsoft.org/v11/i01/paper this paper (see page 48)] suggests that <code>perl</code>'s <code>rand</code> isn't guaranteed to be much good. —[[User:Underscore|Underscore]] ([[User talk:Underscore|Talk]]) 17:15, 24 December 2009 (UTC)
::::: The table on p.53 shows about what I'd expect; the standard rand() is not suitable for either crypto or (proper) Monte Carlo use, and this is normal for a general use RNG in any language. But for shuffling a short list of permutations for a RC task, well, it's just fine. The mixing up is just to make the answer not blindingly obvious after all. –[[User:Dkf|Donal Fellows]] 19:00, 24 December 2009 (UTC)
:::::: Python uses [[wp:Mersenne twister]] as its PRNG which is supposed to be of high quality. There is a Perl implementation [http://search.cpan.org/search?module=Math%3A%3ARandom%3A%3AMT%3A%3AAuto om CPAN]. --[[User:Paddy3118|Paddy3118]] 11:10, 25 December 2009 (UTC)
::::: BTW, it might be worth seeing if there's any ideas for tasks that can be harvested from that paper. After all, it's always good to encourage less-experienced programmers to understand the limits of the PRNGs they're using. –[[User:Dkf|Donal Fellows]] 19:03, 24 December 2009 (UTC)

== Prototype Tcl Solution ==

<tt><nowiki>
## Tcl
</nowiki>

<nowiki>{{libheader|tcllib}}</nowiki></tt>

```tcl
package require struct::list
package require struct::set

# Make complete list of permutations of a string of characters
proc allCharPerms s {
    set perms {}
    set p [struct::list firstperm [split $s {}]]
    while {$p ne ""} {
	lappend perms [join $p {}]
	set p [struct::list nextperm $p]
    }
    return $perms
}

# The set of provided permutations (wrapped for convenience)
set have {
    ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA CBAD ABDC
    ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB
}
# Get the real list of permutations...
set all [allCharPerms [lindex $have 0]]
# Find the missing one(s)!
set missing [struct::set difference $all $have]
puts "missing permutation(s): $missing"
```

: Which version of Tcl does that require? struct::list Doesn't seem to come with Ubuntu 9.10's Tcl v 8.5:

```txt
shortcircuit@dodo~
11:27:02 $ tclsh8.
tclsh8.4  tclsh8.5  
shortcircuit@dodo~
11:27:02 $ tclsh8.5 missing.tcl 
can't find package struct::list
    while executing
"package require struct::list"
    (file "missing.tcl" line 1)
shortcircuit@dodo~
11:27:11 $
```
 --[[User:Short Circuit|Michael Mol]] 16:30, 24 December 2009 (UTC)
:: Requires a version of tcllib (which I ''did'' note; just marked nowiki to stop this talk page from going into the system) which it looks like Ubuntu (at the very least) doesn't install by default. It's been part of the overall tcllib system since at least tcllib 1.4 (that's the earliest version tag I can find in the CVS repo) which was from 2003. (I don't list tcllib itself in the script because that'd be a nonsense, analogous to – and nearly as silly as – requiring the whole of CPAN in one program...)
::: Not only did it not install it by default, it wasn't in the "Suggest" or "Recommend" section for the tcl8.4 and tcl8.5 packages, which was why I couldn't find it. (A bug report should probably be filed for that.) --[[User:Short Circuit|Michael Mol]] 00:12, 25 December 2009 (UTC)
:::: I don't know who's the maintainer of the Ubuntu (or Debian) package for tcllib (or even Tcl for that matter). –[[User:Dkf|Donal Fellows]] 08:51, 25 December 2009 (UTC)
:::: I'll also note that I've been using tcllib in quite a few tasks, and I'm actually not a heavy user of it by Tcl community standards. I guess my user profile is a bit odd. ;-)  –[[User:Dkf|Donal Fellows]] 09:00, 25 December 2009 (UTC)
::::: I hadn't tried running any until now, I suppose. :)  I just wanted to see which permutation it identified as missing.  When I didn't see much under the "recommended" section for the tcl packages, I presumed that the standard library was tied in. --[[User:Short Circuit|Michael Mol]] 09:58, 25 December 2009 (UTC)
:::::: Some of that I know the reason for; tcllib is not part of Tcl itself precisely to allow it to evolve more rapidly. We're deliberately very conservative with the language itself, with a focus on keeping everything well-engineered and reliable. tcllib gains packages much more quickly; all it takes is for someone to write code, docs, tests and to attach a suitable license. Oh, and attract the attention of the overall maintainers, but that's usually 1–2 working days so not a problem. OTOH, we've had persistent problems with packaging for OSes, with things going relatively well on some platforms (where the people concerned will actually talk to us properly) and poorly on others. I do see from a [http://developers.slashdot.org/story/09/12/24/0319228/Helping-Perl-Packagers-Package-Perl recent slashdot story] that we're not the only people in that position though, so I'm not very worried. –[[User:Dkf|Donal Fellows]] 15:48, 25 December 2009 (UTC)

== Any notable problems with this task? ==

If nothing's been found particularly wrong with this task by Sunday night, I'll switch it over as a full task. --[[User:Short Circuit|Michael Mol]] 19:01, 24 December 2009 (UTC)
: It looks good to go now. It's clear what we have to do, and it's nicely solved with a permutation engine and some set arithmetic. –[[User:Dkf|Donal Fellows]] 19:13, 24 December 2009 (UTC)
:: removed draft --[[User:Glennj|glennj]] 13:24, 29 December 2009 (UTC)

== J ==

###  prototype J soln 

The J solution, <code>(] A.~ A.@i.~ -.~ i.@!@#@]) /:~@:{.</code>, could be simplified (rather, made more elegant with e.g. <code>&.</code>) if we don't have to generalize to other permutation orders (i.e. we it explicitly stated we could depend on the universe of symbols being 'ABCD', or the task specified the universe-of-symbols being an input).  Right now the solution must determine the universe-of-symbols from the first permutation.
: I would prefer the universe of symbols to be undefined, and for the code to be generic. You might as well think of the data being 0x41, 0x42, 0x43 and 0x44 integer values, 65.0f, 66.0f, 67.0f, 68.0f, or even Rosemary, Paprika, Sage, Thyme; My intent was to consider and compare the permutations of symbols, not <em>necessarily</em> the exact symbols used. It would probably be best to avoid hardcoded assumptions about the universe of symbols --[[User:Short Circuit|Michael Mol]] 05:14, 28 December 2009 (UTC)
: You could also exhibit both solutions, stating in the adjoining text that one is more general than the other. Like that you'd illustrate a point about the J language as well as providing the solution in various levels of elegance. –[[User:Dkf|Donal Fellows]] 13:46, 28 December 2009 (UTC)

== Fortran example incorrect? ==

The fortran example was marked incorrect. Indeed, the result is wrong when compiled with gfortran. However, g95 gives the correct result. I believe this to be a bug in gfortran. I narrowed down the problem in the following test program:

```fortran
program missing_permutation_test

  implicit none
  character (4), dimension (23), parameter :: list =                    &
    & (/'ABCD', 'CABD', 'ACDB', 'DACB', 'BCDA', 'ACBD', 'ADCB', 'CDAB', &
    &   'DABC', 'BCAD', 'CADB', 'CDBA', 'CBAD', 'ABDC', 'ADBC', 'BDCA', &
    &   'DCBA', 'BACD', 'BADC', 'BDAC', 'CBDA', 'DBCA', 'DCAB'/)
  integer :: i

  write (*, *) list (:) (1 : 1)
  write (*, *) list (1) (1 : 1)
  write (*, *) list (:) (1 : 1) == list (1) (1 : 1)
  i = 1
  write (*, *) list (1) (i : i)
  write (*, *) list (:) (1 : 1) == list (1) (i : i)

end program missing_permutation_test
```

The output of this program when compiled with gfortran is:
<lang> ACADBAACDBCCCAABDBBBCDD
 A
 T F T F F T T F F F F F F T T F F F F F F F F
 A
 F F F F F F F F F F F F F F F F F F F F F F F
```

It seems to me that comparing <code>list (:) (1 : 1)</code> to either <code>list (1) (1 : 1)</code> or <code>list (1) (i : i)</code> when <code>i = 1</code> should give the same result. However, I am not 100% certain about my claim.
When compiled with g95 we get:
<lang> ACADBAACDBCCCAABDBBBCDD
 A
 T F T F F T T F F F F F F T T F F F F F F F F
 A
 T F T F F T T F F F F F F T T F F F F F F F F
```

For now, I will leave the "incorrect" label, until we find a definite answer.
: I investigated this issue a bit further. The posted program is a correct fortran program (90 and later) and should give the correct result (see e.g. [http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/b327e38e69e4e622/2ea18cbd2a5af40f?lnk=raot this thread]). The version of gfortran tried by me (4.3.3 20080904 (prerelease)) gives a wrong result. More recent versions give the correct result. I am removing the "incorrect label". --[[User:Dsnouck|Dsnouck]] 07:56, 27 May 2010 (UTC)


:: The bug is more subtle than it seemed; the problem is bound to how gfortran handles parameters, in fact removing the parameter attribute it works the same in gfortran. I suppose there's also some relationship with how "strings" are handled. Using 1:1 and i:i "indipendently" does not change the result, as we expect, but when combined with == and slicing of a var with parameter attribute (immutable), the problem arises (and also it seems the error propagate, see the following long test code)


```fortran
program test 

  implicit none 
  character (2), dimension (2), parameter :: list = (/'ab', 'ba'/) 
  character (2), dimension (2) :: la = (/ 'a', 'b' /)
  character, dimension(2) :: p
  character :: t, t1
  character(2) :: tt
  integer :: i
  

  i = 1
  write (*, *) 'list(:)(1:1)  = ', list (:) (1:1)  ! a, b
  write (*, *) ' size of prev = ', size(list (:) (1:1))  ! check that it is a, b
  write(*, *)  'shape of prev : ', shape(list (:) (1:1))

  write (*, *) '1:1 = ', list (1) (1:1)  ! a
  write (*, *) 'i:i = ', list (1) (i:i)  ! a   both! no bug is shown here by gfortran
  t = list(1)(i:i)    ! t = 'a'
  t1 = list(1)(1:1)   ! t1 = 'a' (character(1))
  tt = list(1)(i:i)   ! tt = 'a' (character(2))
  write(*, *) 't use  i:i = ', t,  '| len = ', len(t)
  write(*, *) 't1 use 1:1 = ', t1, '| len = ', len(t1)
  write(*, *) 'tt         = ', tt, '| len = ', len(tt)
  write(*, *) 't == t1   is ', t == t1
  print *
  print *, "'ab' sliced by i:i == 'a' and == t and == tt"
  write (*, *) list(1)(i:i) == 'a', &    ! T
               list(1)(i:i) == t,  &     ! T
               list(1)(i:i) == tt        ! T
  print *

  !! both agree on T F result
  print *, "direct comparing of 1 char strings"
  write(*,*) 'a' == (/ 'a', 'b' /)
  write(*,*) (/'a', 'b' /) == 'a'
  print *

  !! T F
  print *, "list(1)(1:1) == (/ 'a', 'b' /)"
  write(*,*) list(1)(1:1) == (/ 'a', 'b' /)
  print *, "char(1) t ('a') == char(2), dim(2) la ['a', 'b']"
  write(*, *) t == la
  print *
  !! T F
  print *, "list(1)(1:1) [char(2) 'a' sliced] == char(2)dim(2) sliced ['a', 'b']"
  write(*,*) "1:1 1:1|", list(1)(1:1) == list(:)(1:1)
  write(*,*) "i:i 1:1|", list(1)(i:i) == list(:)(1:1)
  write(*,*) "i:i i:i|", list(1)(i:i) == list(:)(i:i)
  write(*,*) "1:1 i:i|", list(1)(1:1) == list(:)(i:i)
  print *

  print *, "list(:)(1:1) == X" ! shows the error is someway "propagated" to X
  write(*,*) "X is t  |", list(:)(1:1) == t
  write(*,*) "X is t1 |", list(:)(1:1) == t1
  write(*,*) "X is 'a'|", list(:)(1:1) == 'a'
  write(*,*) "X is tt |", list(:)(1:1) == tt
  print *, "reference: (/ 'a', 'b' /) == tt"
  write(*,*) (/'a', 'b'/) == tt
  print *

  print *, "casting to non parameter"
  p = list(:)(1:1)
  print *, "p = ", p, "| size(p) = ", size(p)
  print *, "p == X"
  write(*, *) "X is t  |", p == t
  write(*, *) "X is t1 |", p == t1
  write(*, *) "X is tt |", p == tt
  
  print *
  !! same output for g95 and gfortran
  print *, "checking diffs between char(1) and char(2)"
  write(*,'(A,A)') 'ab', 'cd'   !abcd
  write(*,'(A,A)') tt, t1       !a a
  write(*,'(A,A)') t1, tt       !aa

end program test
```


:: I also suspect that the difference is that when we are using constant number, the slicing is done at compile time, while when there's a variable that must be evaluated, the slicing is done at runtime and problems arises for the "immutable" nature of the parameter. So as already said a work-around is to remove the parameter attribute...! &mdash;[[User:ShinTakezou|ShinTakezou]] 19:45, 27 May 2010 (UTC)

== Simple solution ==

Since there's exactly one permutation missing, there's actually a very simple solution which probably beats all solutions given so far in execution time (unless I've overlooked something, all solutions try to generate all permutations and find them in the list):
* For each position, count how often each letter appears.
* The one which appears one time less than the others is at this position in the missing permutation.
In the given example:
*Position 1: A:6, B:6, C:6, '''D:5'''
*Position 2: A:6, '''B:5''', C:6, D:6
*Position 3: '''A:5''', B:6, C:6, D:6
*Position 4: A:6, B:6, '''C:5''', D:6
Therefore the missing permutation is DBAC. --[[User:Ce|Ce]] 19:49, 16 August 2010 (UTC)

This approach is not very general, but you are right that the task does not need a general solution.  For what it's worth here is this approach in J:


```j
   ,(~. #~ 5 = #/.~)"1 |:data
DBAC
```


This was only about 30% faster but it's also a lot shorter -- mostly because I did not bother using any names for the functional part.

--[[User:Rdm|Rdm]] 19:24, 17 August 2010 (UTC)
