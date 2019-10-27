+++
title = "Talk:Addition-chain exponentiation"
description = ""
date = 2017-12-16T21:27:05Z
aliases = []
[extra]
id = 10400
[taxonomies]
categories = []
tags = []
+++

==Incorrect==

Note that the table displayed on the task page is incorrect.  Currently for the exponent for a↑15 it has: (d←(b←a×a)×b)×d×d×a.

But this means b←a↑2 and d←a↑4 and the result calculated is a↑13 --[[User:Rdm|Rdm]] 15:04, 27 August 2011 (UTC)

I think it should be: d×d×(d←a×b×(b←a×a)) or b×d×(d←b×(b←a×a×a)) but my preferred notation (where the operations are strictly right to left) conflicts with the format used in the table (where assignment is right to left but assignments get used left to right). --[[User:Rdm|Rdm]] 15:16, 27 August 2011 (UTC)

Fixed.  ThanX [[User:NevilleDNZ|NevilleDNZ]] 05:39, 28 August 2011 (UTC)

== Alternative "special objects" to using Matrices ==

I'm thinking that with matrices minimising multiplications is important,  the other fields where this could be important (e.g. arbitrary length modulo arithmetic in cryptography) are rather abstract.  

Note: I'd be pleased to allow arbitrary length modulo arithmetic (for cryptography) as an optional alternative to matrices.

''Real'' and ''complex'' variables could be used, but in reality these can be done via calls to ''log'' and ''exp'' functions and to not provide any interesting test cases.

But maybe there is a better "special object" (for the sake of this task) is available, any suggestions?

[[User:NevilleDNZ|NevilleDNZ]] 03:42, 27 August 2011 (UTC)

: Maybe just drop the matrix part altogether?  Finding shortest chains is enough of a problem in itself, plus it's not like we need even more matrix related tasks on RC.  --[[User:Ledrug|Ledrug]] 05:05, 27 August 2011 (UTC)

I didn't immediately see this task as a ''finding the shortest chain'' kind of exercise.  Basically I was looking at it from a practical point of view.  In that here is a real problem, and have is a real application and/or test case.  Hence a reason to solve it. Without a real problem, and a real test case the task would look naked.

Also it means that the test case can be described in the "present tense", rather then as an abstraction.

Having said all that... I also am nervous about the matrix manipulation as I think it is enough to scare off the majority of contributors.  And (you are right) it isn't core to the task.  Maybe the matrix exponentiation could be relegated to a Kudos as it is an important application of any such algorithm.

Maybe someone could reword the task as I am at a bit of a loss on exactly an alternative way of describing it. 

BTW this problem is a nice (and at the same time nasty) little puzzle to work on.... ¢ Who needs Sudoku when you have rosettacode.org? Yay!  Rack your brains, but possibly produce something beautiful & useful at the end. ¢

[[User:NevilleDNZ|NevilleDNZ]] 06:11, 27 August 2011 (UTC)

: A '''little''' puzzle is quite an understatement -- the task asks for the optimal chain of a 9 digit number, for which I don't know if there is a practical method (even though its factorization is known, it might not help: see 77 and 154).  Secondly, matrix-related tasks show up a lot on RC, which is both tedious to do and difficult to do right.  It also has another problem: binary expo requires fixed amount of storage for temp matrix result (the current square), while an optimal chain may require variable temp storage (chain for 9 requires saving result 3 or 4 for future use depending on the chain taken) which can be a pain for languages without GC.  I think it's sufficient to just mention where addition chains can be useful, while the task only needs to implement the part of finding the chains for a given number.  --[[User:Ledrug|Ledrug]] 06:34, 27 August 2011 (UTC)

OK.  I will drop the use of matrices, I also find using libraries a pain as often the libraries are language release specific. As an alternative test case we can use the languages built in [[wp:complex numbers|complex numbers]] type,  and the test cases: 
* 1.0000254989+0.0000577896i<sup>31415</sup> = 2
* 1.0000220632+0.0000500026i<sup>27182</sup> = 2.

And then only the chains for 31415 and 27182 are required.

[[User:NevilleDNZ|NevilleDNZ]] 08:09, 27 August 2011 (UTC)

::I'd like to vote up modular exponentiation of integers.  Poking around on the internet, there is lots of great research out there on addition chains and it seems to be largely motivated by RSA encryption.  Exponentiation of matrices and complex numbers are fine, but as applications for addition chains I don't see them as any more compelling that exponentiation of simple floating point numbers.  If we wanted to avoid big integers we could pick 16 bit numbers and still have an interesting example.  &mdash;[[User:Sonia|Sonia]] 23:08, 13 February 2012 (UTC)

== Inefficient chain finding ==
As a reference to optimal chain length.  Optimal addition chain is NP-complete. In other words, don't use the following for big numbers where "big" is "larger than a few tens, probably 20".

```python
cache = {0: 0}
def chain(maxn, r = (1,)):
	n, l = r[0], len(r) - 1
	if n <= maxn and (not n in cache or l <= cache[n]):
		cache[n] = l
		for i in r: chain(maxn, (n + i,) + r)

xx = 200
chain(xx)
for i in range(xx + 1):
	print(i, cache[i])
```
--[[User:Ledrug|Ledrug]] 09:08, 27 August 2011 (UTC)

== wikipedia content ==

Given that the bulk of the task was copied wholesale from the wikipedia page, perhaps the following paragraph should be included as well?

:On the other hand, the addition-chain method is much more complicated, since the determination of a shortest addition chain seems quite difficult: no efficient optimal methods are currently known for arbitrary exponents, and the related problem of finding a shortest addition chain for a given set of exponents has been proven [[wp:NP-complete|NP-complete]].<ref><nowiki>{{Cite journal|first1=Peter|last1=Downey|first2=Benton|last2=Leong|first3=Ravi|last3=Sethi|title=Computing sequences with addition chains|journal=SIAM Journal on Computing|volume=10|issue=3|year=1981|pages=638–646|doi=10.1137/0210047}}</nowiki></ref> Even given a shortest chain, addition-chain exponentiation requires more memory than the binary method, because it must potentially store many previous exponents from the chain simultaneously.  In practice, therefore, shortest addition-chain exponentiation is primarily used for small fixed exponents for which a shortest chain can be precomputed and is not too large.
<references/>

And the following two paragraphs at wikipedia also look relevant.

--[[User:Rdm|Rdm]] 14:51, 27 August 2011 (UTC)

: This appears to be a really hard problem.  Most relevant page on the web so far: [[http://wwwhomes.uni-bielefeld.de/achim/addition_chain.html]], and in particular the paper [[http://wwwhomes.uni-bielefeld.de/achim/ac.ps.gz]].  I have doubts on the possibility of anyone implementing it on RC. --[[User:Ledrug|Ledrug]] 15:59, 27 August 2011 (UTC)
: From the search tool on the page linked above:<lang>1 2 4 5 10 20 40 80 160 320 321 641 1282 1923 3846 7692 15384 15705 31410 31415 
1 2 3 5 10 20 23 43 53 106 212 424 848 1696 3392 6784 13568 13591 27182
```


::Computing optimal addition chains for a sequence of multiplications is harder than for a single multiplication.  I believe I have an implementation for the simple case asked for here.  --[[User:Rdm|Rdm]] 16:39, 27 August 2011 (UTC)
::That said, the requested matrix multiplications look to be producing ridiculously large results.  When I tried using floating point, I got a result full of negative and positive infinities.  So computing the result will require extended precision numbers and I expect the result will be quite verbose.  --[[User:Rdm|Rdm]] 16:44, 27 August 2011 (UTC)
::::I got that at first too, but I had a typo.  When I fixed it, my results agreed with those of the posted MATLAB solution. A^24 is an identity matrix, by the way, so A^31415 = A^23 and A^21718 = A^14. &mdash;[[User:Sonia|Sonia]] 23:08, 13 February 2012 (UTC)
:::Oh well, turns out that my approach is non-optimal in some cases.  When I compare the first 100 exponents (1..100) with the A003313 values listed in the task description, I am using one multiplication too many for these cases: 23 31 33 43 46 47 49 59 61 62 65 66 69 77 79 83 86 92 93 94 98 99. Here are two of my cases where I should be doing better:  
```j
   addch 31
A*B*C*D*D*D=:C*C=:B*B=:A*A
   addch 33
D*D*D =:A*B*C*C=:B*B=:A*A
```
  It would be interesting to see the optimal solution for an exponent of 23 -- that's the first case where mine fails, and is the first prime exponent where binary chain is inadequate.  --[[User:Rdm|Rdm]] 17:06, 27 August 2011 (UTC)
::::
```python
cache = [[(0,)], [(1,)]]
def chain(n):
	if len(cache) >= n + 1: return cache[n]
	v = []
	for m in range((n + 1)//2, n):
		for r in chain(m):
			if len(v) and len(r) >= len(v[0]): break
			if not n - r[0] in r: continue

			rr = (n,) + r
			if not len(v) or len(v[0]) > len(rr):
				v = [rr]
			elif len(v[0]) == len(rr):
				v.append(rr)
	cache.append(v)
	return v

print chain(23)
```
--[[User:Ledrug|Ledrug]] 19:36, 27 August 2011 (UTC)
:::: How does your notation work? In <code>L*M*M=:L*L =:G*I*J*K*K*K=:J*J=:I*I=:H*H=:G*G =:A*B*C*F*F*F=:E*E=:D*D=:C*C=:B*B=:A*A</code>, what does <code>G=:A*B*C*...</code> even mean? --[[User:Ledrug|Ledrug]] 20:09, 27 August 2011 (UTC)
::::: <code>=:</code> is an assignment operation in J, and precedence is the same for multiplication and assignment, with long right scope.  So that expression is equivalent to <code>L*(M*(M=(L*(L=(G*(I*(J*(K*(K*(K=(J*(J=(I*(I=(H*(H=(G*(G=(A*(B*(C*(F*(F*(F=(E*(E=(D*(D=(C*(C=(B*(B=(A*A)))))))))))))))))))))))))))))))))</code> in C-like languages.  --[[User:Rdm|Rdm]] 20:48, 27 August 2011 (UTC)
::::: btw, I get an error from your python program: 
```python
  File "<stdin>", line 17
    print chain(23)
```
 but I should have a chance to sit down and work out something soon.  --[[User:Rdm|Rdm]] 20:52, 27 August 2011 (UTC)
::::: Never seen that error before.  Blame windows.  Anyway, output is<lang>[(23, 13, 10, 5, 3, 2, 1), (23, 14, 9, 5, 4, 2, 1),
 (23, 18, 9, 5, 4, 2, 1), (23, 20, 10, 5, 3, 2, 1)]
```
 --[[User:Ledrug|Ledrug]] 21:11, 27 August 2011 (UTC)

:::::: Ok... looking at the prime numbers less than 100 where my approach was inadequate (which was just binary chain for prime numbers), I can see a rapidly increasing problem: 
```j
23 6   4
31 7  42
43 7   4
47 8  63
59 8  24
61 8  72
79 9 185
83 8   4
```
 First column is the prime number, second column is sequence length, third column is the number of possibilities to compute that exponent in minimal length based on a minimal length chain.  I just do not see any good way of managing the complexity of this system. --[[User:Rdm|Rdm]] 22:43, 27 August 2011 (UTC)
::::::: I'm not sure why you think prime numbers are more troublesome than composites: if you are doing factorization, your method is doomed.  Check the output for 77, none of the sequences contains either 7 or 11. --[[User:Ledrug|Ledrug]] 23:27, 27 August 2011 (UTC)
:::::::: Yes, 33 was enough to show that factorization can be worse than addition chain (length binary chain for 3 + 1 + length binary chain 11 is longer than binary chain for 33).  --[[User:Rdm|Rdm]] 02:32, 28 August 2011 (UTC)
Knuth's programs: [[http://www-cs-faculty.stanford.edu/~knuth/programs/achain4.w]] and [[http://www-cs-faculty.stanford.edu/~knuth/programs/achain-all.w]].  Pretty fast, but doesn't explain the algorithm very well, maybe because the algorithm only appears complicated to mere mortals.  --[[User:Ledrug|Ledrug]] 20:59, 31 August 2011 (UTC)
:in [http://www-cs-faculty.stanford.edu/~knuth/programs/achain4.w achain4] he asks people to first read [http://www-cs-faculty.stanford.edu/~knuth/programs/achain2.w achain2] and in achain2 he asks people to first read [http://www-cs-faculty.stanford.edu/~knuth/programs/achain1.w achain1] and in achain1 he asks people to first read [http://www-cs-faculty.stanford.edu/~knuth/programs/achain0.w achain0]. And these make frequent reference to Theorem 4.6.3C (which apparently is on page 469 of his Art of Computer Programming -- which I do not have access to right now).  --[[User:Rdm|Rdm]] 21:29, 31 August 2011 (UTC)
==MATLAB non-solution==
An assumption that a language solves the task is not a solution to the task.  &mdash;[[User:Sonia|Sonia]] 23:08, 13 February 2012 (UTC)
:You're right. I marked the solution as wrong. It's not even an attempt to provide something (like binary exponentiation, at least, though not enough either). [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 09:25, 30 April 2015 (UTC)

==Display count==
+1 for this requirement.  Of course the number of multiplications is determined by the length of the chain, but it's a fun and simple little demonstration to actually count the multiplications that happen rather than report the number that should be required. &mdash;[[User:Sonia|Sonia]] 23:08, 13 February 2012 (UTC)
==Show chain==
The chains themselves are so interesting!  It's interesting that a number can have different chains of the same length. I think we should add a requirement to show the chains. &mdash;[[User:Sonia|Sonia]] 23:08, 13 February 2012 (UTC)
==Associativity==
Exponentiation is the obvious application for addition chains, but shouldn't they work for any associative binary operation?  I expect exponentiation will make the best application for a task requirement, but we might mention associativity in the description. &mdash;[[User:Sonia|Sonia]] 23:08, 13 February 2012 (UTC)
:Actually, except for low exponent, I doubt any software would use optimal solution, since it's really a pain to find the solutions. Also, there is a more general solution allowing inverse operations. It is interesting for multiplication (if addition and subtraction have roughly the same cost), but probably not for exponentiation, where multiplication/division are usually not expected to have the same cost. Hence for really optimal use, one would have to analyze carefully the costs of each variant. However, the task is still theoretically interesting, and finding a good algorithm for large exponents is not so easy. A backtracking program I wrote some time ago in Fortran 77 took almost 24 hours to compute optimal solutions for all exponents up to 2048, I don't think it's fast enough, by far. And usually heuristics only give suboptimal solutions. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 09:30, 30 April 2015 (UTC)

==Optimal solution, A003313, and κῦδος==
Any technique that guarantees optimal solutions produces A003313 so if the task allows only optimal solutions, then 1) the task requires an expensive computation, and 2) κῦδος are cheap for any valid solution.  I would like to see the task allow solutions that are not guaranteed to be optimal but which generally improve on binary solutions.  This is arguing for the practical result of accelerating a computation as opposed to the mathematical result of finding a limit.  The rich literature out there seems mostly focused on the practical application of finding near-optimal solutions (for the purpose of accelerating RSA encryption.)  κῦδος could still go to the mathematical result of A003313, but it could be made more challenging by requiring later terms of the sequence.  Terms 8190-8195, for example, which are verifiable from a link from the OEIS page.  Actually I'd like to see κῦδος awarded for reproducing those terms using a near-optimal technique. &mdash;[[User:Sonia|Sonia]] 23:08, 13 February 2012 (UTC)
:It's another task then. I don't think it's a problem to ask for optimal solutions. Actually, it's even very interesting: among the 5 solutions so far, 4 are wrong. A good task to separate those who can read a question from those who can't. I didn't check the C one carefully though: it's certainly wrong in the sense that it does not compute matrix exponentiation, but this is really irrelevant, the interesting part is the computation of ''optimal'' addition chains. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 17:19, 19 July 2015 (UTC)

== Montgomery reduction ==

I just tried a fun experiment of combining this task with the [[Montgomery reduction]] task and it worked just fine.  It's not that much of a stretch since Montgomery reduction is just another way to do multiplication, but still it was fun to see addition chain exponentiation work with this alternative operation and also see it work on some bigger numbers.  The 100 bit exponents of my example there take about 150 multiplies using binary exponentiation but about 130 multiplies with the addition chains.  &mdash;[[User:Sonia|Sonia]] 01:59, 2 March 2012 (UTC)

== Go is wrong, among others ==

The Go solution starts with the sentence "'''A non-optimal solution'''". The tasks states: "Note: Binary exponentiation does not usually produce the best solution. '''Provide only optimal solutions.'''".

Actually, the program states in the comments "the techniques here work only with 'star' chains". But star chains are known '''not''' to be optimal.

The answer is thus wrong.

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 21:14, 20 July 2015 (UTC)

: I do not think this is valid reasoning.

: The wikipedia mention of [[wp:Addition_chain#Brauer_chain|star chains]] does indeed mention that star chains can fail to produce invalid values. However, the example given is for an exponent of 1227873210934869755325459645562028180483410421970385642998266670782559244925047578059104483100233309036904622010710304861903062287481691063068371652744142574931000480320516175613904206157567455536712489864433925819285983619453998073177028402644441528674227. Here, the addition-chain has 6110 elements while the optimal chain has at most 6109 elements (but apparently no one has been able to prove that they have found an optimal chain for that exponent).

: So by your reasoning, any implementation which limits itself to 64 bit integers is also an invalid implementation because that implementation would not produce an optimal result for an exponent of 1227873210934869755325459645562028180483410421970385642998266670782559244925047578059104483100233309036904622010710304861903062287481691063068371652744142574931000480320516175613904206157567455536712489864433925819285983619453998073177028402644441528674227.

: That said, if you can modify the task in some reasonable way which brings out the flaws of star chains, I would accept your reasoning. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:43, 20 July 2015 (UTC)
::The article does not say it's the smallest counterexample. For you, if a statement is not (yet) proved, it's considered to be true? Wrong. Either you have a proof that star chains produce optimal solutions for 31415 and 27182, either a program based on star chain is useless. When the task asks for optimal solution, that means provably optimal solution, not ''maybe optimal, maybe not''. [[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 21:56, 20 July 2015 (UTC)


Quoting [http://strangelyconsistent.org/blog/t3-addition-chains this]: '''" A Brauer-based algorithm will fail the first time at N = 12509."''' (if this site is not enough for you, I'll try to give you the reference in Knuth's TAOCP then, as I'm sure to have read this in one of the volumes). It's far below the values of the task, which is a big problem for me. And the article linked in the Go program does not claim to give optimal solution, on the contrary: '''"Even though minimal-length cf-chains are not optimal, they have the nice property of being easy to compute [...]"'''.
This should close the question on the (in)correctness of the Go program.

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 23:10, 20 July 2015 (UTC)

== One more value to find ==

I added 12509 to the values of the task, 31415 and 27182. Actually, 12509 is the [http://strangelyconsistent.org/blog/t3-addition-chains smallest] for which star chains fail to give an optimal answer, and since the optimality of star chains is questionnable for 31415 and 27182, it's better to clearly exclude this approach. The task insists in asking for an optimal solution, and not a suboptimal one, thus one has to comply with this: algorithms which are faster but suboptimal are perfectly acceptable from an engineering point of view, but do not qualify for an answer to this task. Here we need the guarantee that the answer is correct, that is, really optimal.

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 23:30, 20 July 2015 (UTC)

== Should this task be deleted? ==

Is it reasonable for tasks to require running time in months or years? 

I'm not sure how this task is going to satisfy any meaningful language comparison niche.  --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:53, 21 July 2015 (UTC)


I was just going to make this same point when I found this section :-)

RC is about comparing languages not about original research in a proven difficult area. In the life of this task we have only a few examples and more comment on their correctness. I too think that the task is not written for an RC audience. It could be improved/saved if a particular algorithm or set of algorithms are approved in the task description and pseudo code given otherwise the task is too broad and contentious. 

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 06:58, 21 July 2015 (UTC)
:Many tasks are only suited to a few languages, it's the very purpose of having so many languages: if one could handle all tasks as well or better than any other, there would be no point in programming in any other language. We know it's not the case.
:Furthermore, this is an old problem, described in Knuth's TAOCP. I think it's still a good candidate for RC, but maybe it should be amended to use much lower exponents: in the 200-600 range, it should be reasonably fast.
:However, it should then still be clearly stated whether star chains are accepted or not: I don't think it's a good idea since the algorithm provides no guarantee that the result is optimal, it's only an observation made by comparing with the true optimal solution. This would encourage solving a problem with the wrong tool, just because it happens to give the right answer by chance (and it does not with 12509, as explained above). It's also possible to state explicitly that star chains are accepted because we know by other means that the answer is right in the asked cases (but then one has to choose exponents for which it is really known, of course, and give proper references).
:On the other hand, it would also be doable with the current exponents, but then the task should mention a reference to an efficient algorithm.
:For low exponents, I can give a backtracking algorithm relatively easy to adapt: in Frotran 77, thus only ''if'' and ''goto'' are really needed, and it can be translated to a recursive algorithm, hence most language families can do it.
:Another remark: I don't see much point in asking for matrix exponentiation since the core problem is in optimizing addition chains. That's adding a mostly trivial task, but which may require much code in some languages, for no benefit as there is already a task about this: [[Matrix-exponentiation operator]].
:[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 08:15, 21 July 2015 (UTC)

::Hi Arbautjc, if you see a way to tackle:
::# Giving an algorithm (in an accessible manner).
::# Excessive run times.
::# Matrices? (Possibly remove)?

::Then you could probably turn around the task and get more examples I think. --[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:22, 21 July 2015 (UTC)
:Should this task be deleted?  As it is, I would say yes.  It has many problems.  It's current advocate, Arbautjc, feels optimal solutions should be required yet there no implementations of optimal chain algorithms (#include is not an implemenation) and people have cited reasons why optimal chain algorithms are not likely to be a good fit for RC.  &mdash;[[User:Sonia|Sonia]] ([[User talk:Sonia|talk]]) 02:09, 10 August 2015 (UTC)
::The task as it is right now is too complex for RC. However, addition chains are IMO perfectly fit to RC. Choose lower bounds and it's easily feasible. However, if you choose lower bounds, it is known that star chains are optimal (but it is known by comparison to an optimal algorithm, not by an independant proof, AFAIK). The lowest N for which they fail to be optimal is greater than 10000, far above what is reasonably feasible with a simple backtracking algorithm. Therefore, if lower bounds are chosen, you may as well give the possibility to use star chains. So far, I have not had much time to write a new task for this (I have never written a task on RC yet, by the way).
::[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 15:20, 17 August 2015 (UTC)

As is, this page should be deleted. It is an exercise of the mind until shown that it has significance in daily computing.  

In general, one should be able to find existing open source code as a working example that passes a test, such as from Perl's cpan for example. If this page is to be kept, the esoteric math symbols and terms should be translated.
[[User:tekbasse] ([[User talk:tekbasse|talk]]) 5:46, 1 March 2017 (UTC)
:Ridiculous. Is there a rule requiring that Rosetta Code tasks be useful in daily computing? Of course not. Esoteric math symbols? Maybe you think computer science has nothing to do with math. But this is also blatantly wrong. This problem has been the subject of several research articles, and good algorithms exist, period. That you may be unable to implement them is nobody's concern. There are other more or less difficult algorithms on RC, and I have no problem with that. RC is not limited to "Hello World" and "How do I write a for loop?", thankfully. [[User:Eoraptor|Eoraptor]] ([[User talk:Eoraptor|talk]]) 21:27, 16 December 2017 (UTC)

== Fast algorithm ==
There is indeed a fast algorithm, much faster than the straightforward backtracking algorithm I used. See the following article:

Neill Michael Clift,
''"Calculating optimal addition chains"'', 
'''Computing''', March 2011, Volume 91, Issue 3, pp 265–284, 
[https://doi.org/10.1007/s00607-010-0118-8 DOI:10.1007/s00607-010-0118-8]

[[User:Arbautjc|Arbautjc]] ([[User talk:Arbautjc|talk]]) 11:45, 29 August 2016 (UTC)
