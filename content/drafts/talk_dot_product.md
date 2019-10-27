+++
title = "Talk:Dot product"
description = ""
date = 2016-02-28T01:47:23Z
aliases = []
[extra]
id = 20052
[taxonomies]
categories = []
tags = []
+++

===I suggest replacing the 'for the functional fetishists' JavaScript version===

The second JavaScript example, presented (in a light or satirical but perfectly friendly tone) as an example of functional program construction, seems to disqualify itself a bit as an actually plausible or representative exemplar by:

# Disrupting functional composition by using exception handling, rather than returning 'bottom', undefined, or a Maybe monad value when the two lists differ in length, and
# not demonstrating the use of the obvious functional primitives and composition in this case: '''sum(zipWith(product, xs, ys)'''


Unless there are objections, I would be inclined to replace it with a slightly more plausible and typical functional definition, built from standard reusable primitives in a way which more visibly corresponds to the definition of a dot product. Using an iterative implementation of zipWith, and assuming ES5 JavaScript (which doesn't perform tail recursion), we could perhaps write something like:


```JavaScript
(function () {
    'use strict';

    function dotProduct(xs, ys) {
        return xs.length === ys.length ? (
            sum(zipWith(product, xs, ys))
        ) : undefined;
    }

    // [n] -> n
    function sum(xs) {
        return xs.reduce(function (a, x) {
            return a + x;
        }, 0);
    }
	
    // n -> n -> n
    function product(a, b) {
        return a * b;
    }

    // (a->b->c) -> [a]->[b]->[c]
    function zipWith(f, xs, ys) {
        var nx = xs.length,
            ny = ys.length,
            lng = (nx < ny ? nx : ny);

        for (var i = 0, lst = []; i < lng; i++) {
            lst.push(f(xs[i], ys[i]));
        }
        return lst;
    }

    return dotProduct([1, 3, -5], [4, -2, -1]);

    // -> 3
})();
```
 -- --[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 16:56, 27 February 2016 (UTC)

:I'd be inclined to say go for it.

:That said, I'd also be inclined to replace zipWith with something like:

:
```javascript
    function zipWith(f, xs, ys) {
        return xs.length === ys.length ? (
            xs.map(function(x, i) {return f(x,ys[i])})
        ) : undefined;
    }
```


:Or, if you feel like <code>zipWith</code> should work with mis-matched argument lengths, give this implementation some other name. ... There is some use for the mismatched approach, but most of the time - including this example - that's just an error.

:(I am not really fond of javascript's implementation of .map(), but this kind of thing is what it was designed for.)

:That said, "not demonstrating feature FOO" isn't really valid criticism, in my opinion. So maybe just fix the exception issue???

:And, thanks! --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:36, 27 February 2016 (UTC)

:: Thanks – that seems sensible – On zip and zipWith implementations, the ones that I am used to return a result with the length of the shorter input, which has the advantage I think, of allowing them to be both well-defined and a little more robust - reducing the range of values which will bottom out, and allowing for cases where the short match is specifically what's sought. The Haskell prelude zipWidth, for example is:
::
```Haskell
zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith _f []     _bs    = []
zipWith _f _as    []     = []
zipWith f  (a:as) (b:bs) = f a b : zipWith f as bs
```

:: On the incumbent function, my reservation is not so much about the properties of the code per se as the characterisation of it as 'functional', when it would actually break functional composition. Perhaps just leave it as a variant, without the jokey dedication ? [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 21:18, 27 February 2016 (UTC)
::: Hmm.... from my point of view, "robust" in the context of "programmer interaction" means: "easily describable" and "useful". But you also want to bubble problems up to the programmer. And, in my experience, "mis-matched lengths" is almost always a problem. If the programmer really wanted the shortest of the two, it's easy enough to discard the unwanted elements from the longer list.
::: Not sure about the larger issue, though -- I can see both sides of this and haven't made up my mind. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:25, 27 February 2016 (UTC)
::::Not sure how that looks – I've adjusted zipWith to return undefined when array lengths vary (it certainly does simplify the top level of dotProduct), and for the moment I've left the earlier code in place, and just reframed its characterisation. [[User:Hout|Hout]] ([[User talk:Hout|talk]]) 22:08, 27 February 2016 (UTC)
::::(and, just now, swapping in your zipWith - which is much cleaner and more pleasing)[[User:Hout|Hout]] ([[User talk:Hout|talk]]) 00:41, 28 February 2016 (UTC)
:::::This looks good. Thanks. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 01:47, 28 February 2016 (UTC)
