+++
title = "Talk:Law of cosines - triples"
description = ""
date = 2018-09-27T01:19:05Z
aliases = []
[extra]
id = 22004
[taxonomies]
categories = []
tags = []
+++

== Showing the sides in order? ==
I see the samples show the 60 degree triangles with the sides in ascending order, however ( a, b, c ) = ( 5, 7, 8 ) isn't a solution of
a^2 + b^2 - ab = c^2.

Where the task says "Find all integer solutions to the three specific cases, in order;", presumably this should be in order of ascending first (lowest) side?



--[[User:Tigerofdarkness|Tigerofdarkness]] ([[User talk:Tigerofdarkness|talk]]) 13:28, 23 September 2018 (UTC)
:You are right. Thanks for bringing this up. I fixed my Factor submission. --[[User:Chunes|Chunes]] ([[User talk:Chunes|talk]]) 14:28, 23 September 2018 (UTC)

:: I changed the wording of that task's requirement   (hopefully, it is more clearer).   In any case, that's what I took it to mean, that the triangles are to be shown in increasing (ascending) order of the first side found.   -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 06:56, 24 September 2018 (UTC)

== How to verify the number of triangles for the   ''extra credit''? ==
Now that there're several computer programming examples that have different output for the   '''optional extra credit'''   requirement,   how does one verify which one is correct?     -- [[User:Gerard Schildberger|Gerard Schildberger]] ([[User talk:Gerard Schildberger|talk]]) 11:07, 24 September 2018 (UTC)

:How about we each create a temporary "Law of cosines - triples/tmp/<language>" page containing just a list of the thousands of triples ?
:* One space-separated triple per line
:* Any line starting '#' treated as a comment, no other comments allowed.
:The tmp/... pages all to be deleted on 1st October 2018.

: We would have a week to examine each others results and refine the task. [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 12:25, 24 September 2018 (UTC)

::Paddy, I suspect there may be a problem with the expression: int(c2**0.5) which occurs 3 times in your Python entry. If the power operator produces a value which is slightly less than the 'correct' integer value, then the 'int' function will round it down and the wrong tuple will be added to the set. 

::I'd see if you get a different answer for the extra credit if you use instead: int(round(c2**0.5)). --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 14:46, 24 September 2018 (UTC)


::Thanks to user Hout for the extra Python solution. I ran it then extracted the generation of the list for the extended credit solution and then filtered out occurrences of the same triangle to get a still differing result:
::
```python

t60u = triangles(f60unequal, 10000)

len(t60u)
Out[4]: 18394

t60u[0]
Out[5]: '[3, 7, 8]'

t60new = set([tuple(sorted(tri)) for tri in t60u])

len(t60new)
Out[7]: 16161

```

 
::I will try removing the **.5 tonight. Thanks.
 [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:15, 25 September 2018 (UTC)


::P.S. I had actually developed a python <code>method2</code> based on dicts (and the fact that dicts are ordered by default in Python3.6). I debugged the two implementations together and they agreed when tested against each other but the Python set version was slightly faster so I only published that. I'll invetstigate further tonight. [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 09:25, 25 September 2018 (UTC)


### The two different python methods that I developed together

Here you go:

```python
N = 13

def method1(N=N):
    squares = [x**2 for x in range(0, N+1)]
    sqrset = set(squares)
    tri90, tri60, tri120 = (set() for _ in range(3))
    for a in range(1, N+1):
        a2 = squares[a]
        for b in range(1, a + 1):
            b2 = squares[b]
            c2 = a2 + b2
            if c2 in sqrset:
                tri90.add(tuple(sorted((a, b, int(c2**0.5)))))
                continue
            ab = a * b
            c2 -= ab
            if c2 in sqrset:
                tri60.add(tuple(sorted((a, b, int(c2**0.5)))))
                continue
            c2 += 2 * ab
            if c2 in sqrset:
                tri120.add(tuple(sorted((a, b, int(c2**0.5)))))
    return  sorted(tri90), sorted(tri60), sorted(tri120)
#%%
if __name__ == '__main__':
    print(f'Integer triangular triples for sides 1..{N}:')
    for angle, triples in zip([90, 60, 120], method1(N)):
        print(f'  {angle:3}° has {len(triples)} solutions:\n    {triples}')
    _, t60, _ = method1(10_000)
    notsame = sum(1 for a, b, c in t60 if a != b or b != c)
    print('Extra credit:', notsame)

def method2(N=N):   # Python 3.6+
    sqr2root = {x**2: x for x in range(1, N+1)}
    tri90, tri60, tri120 = (set() for _ in range(3))
    for a2, a in sqr2root.items():
        for b2, b in sqr2root.items():
            if b > a: 
                break
            c2 = a2 + b2
            if c2 in sqr2root:
                tri90.add(tuple(sorted((a, b, sqr2root[c2]))))
                continue
            ab = a * b
            c2 -= ab
            if c2 in sqr2root:
                tri60.add(tuple(sorted((a, b, sqr2root[c2]))))
                continue
            c2 += 2 * ab
            if c2 in sqr2root:
                tri120.add(tuple(sorted((a, b, sqr2root[c2]))))
    return  sorted(tri90), sorted(tri60), sorted(tri120)
#%%
if __name__ == '__main__':
    tt1 = method1()
    tt2 = method2()
    assert tt1 == tt2
    #
    method1_t60 = t60
    _, method2_t60, _ = method2(10_000)
    assert method1_t60 == method2_t60
    notsame2 = sum(1 for a, b, c in method2_t60 if a != b or b != c)
    print('Extra credit:', notsame2)
    assert notsame == notsame2
```

{{out}}

```txt
Integer triangular triples for sides 1..13:
   90° has 3 solutions:
    [(3, 4, 5), (5, 12, 13), (6, 8, 10)]
   60° has 15 solutions:
    [(1, 1, 1), (2, 2, 2), (3, 3, 3), (3, 7, 8), (4, 4, 4), (5, 5, 5), (5, 7, 8), (6, 6, 6), (7, 7, 7), (8, 8, 8), (9, 9, 9), (10, 10, 10), (11, 11, 11), (12, 12, 12), (13, 13, 13)]
  120° has 2 solutions:
    [(3, 5, 7), (7, 8, 13)]
Extra credit: 17806
Extra credit: 17806
```


# method1 uses sets and is slightly faster than method2 which is why I only posted it.
# method 2 uses `sqr2root` mapping squares to their square root, and furthermore, does not use floating point fractional exponation in its generation.
# the seconf `if __name__...` block computes the same stats using method2 and asserts that the answers are equivalent.


Looking again at my code, Ithink it's right, but, maybe I'm not seeing an off by one error? I am not sure.

I guess the next thing to do is compare Houts dict based solution and look at the differences. [[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]])
: Some particular 60 degree matches which I see in my output but not (I think) in yours include:
: (8, 15, 13), (16, 30, 26),  (24, 45, 39), (32, 60, 52) etc ... up to (4704, 8820, 7644)  (all of which, as far as I can see match the pattern ((a^2) + (b^2)) - (a * b) == c ^ 2)  [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:05, 26 September 2018 (UTC)
:: PS I notice that you are braver than me and mutating the referent of the name '''c2'''. Is it possible that an earlier mutation is (in a subset of cases) shadowing a later test ? You have ''c2 -= ab'' and then ''c2 += 2 * ab'' [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 20:18, 26 September 2018 (UTC)

:::Yep, that's the reason for the discrepancy, Hout :)

:::I just ran it through my Go version and there's apparently 588 cases where both (a*a + b*b) and (a*a + b*b -a*b) are perfect squares. So they are being filtered out by the 90° case. 

:::As 18394 - 588 = 17806 that explains Paddy's results. --[[User:PureFox|PureFox]] ([[User talk:PureFox|talk]]) 21:20, 26 September 2018 (UTC)
:::: Aha ! Well spotted :-) Mutation is a tricky business ... [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:43, 26 September 2018 (UTC)



### =I woke in the night...=


With an idea that the continue statements might be at fault.

(I am disowning them in my use of English, but it was me).

I commented-out all the continue statements in method1 and method2 as I literally had that lightbulb moment of thinking that in my loops, I didn't allow for one triple to be in more than one of the three groups.

Before sleeping I had added Houts case as method3; seen that hehad captured cases that I had not; but was unable to work out why and had gonne to bed with the problem


```python
# Houts code with a slight modification to his `main` function so that it returned `triangles(f60unequal, 10000)`
# ...
#%%

method3_t60_uneven_strings = main() # Houts'
method3_t60_uneven = [tuple(eval(triple_list_str)) 
                      for triple_list_str in method3_t60_uneven_strings]
method2_t60_uneven = [(a, b, c) for a, b, c in method2_t60 if a != b or b != c]
method1_t60_uneven = [(a, b, c) for a, b, c in method1_t60 if a != b or b != c]
#%%
methods_t60_uneven = [method1_t60_uneven, method2_t60_uneven, method3_t60_uneven]
whos_methods = "Paddys set, Paddys dict, Houts dict".split(", ")

print("\n# Stated extra credit answers")
for who, t60u in zip(whos_methods, methods_t60_uneven):
    print(f"  {who:12s}: {len(t60u)}")
    

print("\n# Filtered (again in some cases) for unequal sides")
for who, t60u in zip(whos_methods, methods_t60_uneven):
    t60uf = [(a, b, c) for a, b, c in t60u if a != b or b != c]
    print(f"  {who:12s}: {len(t60uf)}")

print("\n# Filtered, ordered, and duplicates removed: size changes")
for who, t60u in zip(whos_methods, methods_t60_uneven):
    t60ufod = set([tuple(sorted([a, b, c]))
                   for a, b, c in t60u if a != b or b != c])
    print(f"  {who:12s}: From {len(t60u)} to {len(t60ufod)}")

diff13 = sorted(set(method1_t60_uneven) - set(method3_t60_uneven))
diff31 = sorted(set(method3_t60_uneven) - set(method1_t60_uneven))
print(f'\n# I have {len(diff13)} triples that Hout does not have')
print(f'# Hout has {len(diff31)} triples that I do not have')
```



;Original output:

```txt
60 degrees - uneven triangles of maximum side 10000. Total:
18394

# Stated extra credit answers
  Paddys set  : 17806
  Paddys dict : 17806
  Houts dict  : 18394

# Filtered (again in some cases) for unequal sides
  Paddys set  : 17806
  Paddys dict : 17806
  Houts dict  : 18394

# Filtered, ordered, and duplicates removed: size changes
  Paddys set  : From 17806 to 17806
  Paddys dict : From 17806 to 17806
  Houts dict  : From 18394 to 18394

# I have 0 triples that Hout does not have
# Hout has 588 triples that I do not have
```


;Output when those continue statements in method1 and method2 are removed:

```txt
# Stated extra credit answers
  Paddys set' : 18394
  Paddys dict': 18394
  Houts dict  : 18394

# Filtered (again in some cases) for unequal sides
  Paddys set' : 18394
  Paddys dict': 18394
  Houts dict  : 18394

# Filtered, ordered, and duplicates removed: size changes
  Paddys set' : From 18394 to 18394
  Paddys dict': From 18394 to 18394
  Houts dict  : From 18394 to 18394

# I have 0 triples that Hout does not have
# Hout has 0 triples that I do not have
```


'''Yay!!!''' From the discussions of sharper minds above, I found that you had already found the answer, but I thought I would go ahead and finish my public debugging session. 

Thanks people :-)
 
[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 01:18, 27 September 2018 (UTC)
