+++
title = "CoffeeScript"
description = ""
date = 2014-03-15T18:34:45Z
aliases = []
[extra]
id = 8897
[taxonomies]
categories = []
tags = []
+++

'''CoffeeScript''' is a programming language that compiles to [JavaScript](https://rosettacode.org/wiki/:Category:JavaScript). The language adds syntactic sugar inspired by [Ruby](https://rosettacode.org/wiki/:Category:Ruby) and [Python](https://rosettacode.org/wiki/:Category:Python) to enhance JavaScript's brevity and readability, as well as adding more sophisticated features like array comprehension and pattern matching.

With 4000+ watchers and 300+ forks (as of December 2011), CoffeeScript is ranked as one of the "most interesting" projects on Github. [http://github.com/repositories] The language has a relatively large following in the Ruby community, and has been used in production by [http://thinkvitamin.com/mobile/new-rails-like-framework-from-37signals-for-html5-mobile-apps/ 37signals].  Ruby on Rails began to include CoffeeScript  in its asset pipeline in Rails version 3.1 (see [http://www.rubyinside.com/rails-3-1-adopts-coffeescript-jquery-sass-and-controversy-4669.html]).

## History
On December 13, 2009, Jeremy Ashkenas made the first git commit of CoffeeScript with the comment: "Initial commit of the mystery language." The compiler was written in Ruby. On December 24th, he made the first tagged and documented release, 0.1.0. On February 21, 2010, he committed version 0.5, which replaced the Ruby compiler with one written in pure CoffeeScript. By that time the project had attracted several other contributors on Github, and was receiving over 300 page hits per day.

On December 24, 2010, Ashkenas announced the release of stable 1.0.0.

## Examples
A common JavaScript snippet using the jQuery library is


```javascript

$(document).ready(function() {
  // Initialization code goes here
});

```

or even shorter,

```javascript

$(function() {
  // Initialization code goes here
});

```


In CoffeeScript, the <code>function</code> keyword is replaced by the <code>-></code> symbol, and indentation is used instead of curly braces (except when defining an [associative array](https://rosettacode.org/wiki/associative_array)), as in Python.  Also, parentheses can usually be omitted. Thus, the CoffeeScript equivalent of the snippet above is

<!-- Ruby is probably the most similar language that GeSHi supports -->

```ruby

$(document).ready ->
  # Initialization code goes here

```

or

```ruby

$ ->
  # Initialization code goes here

```


## Compiling
The CoffeeScript compiler has been written in CoffeeScript since version 0.5, and can be run either in the browser or through Node.js. The official site at CoffeeScript.org has a "Try CoffeeScript" button in the menu bar; clicking it opens a modal window in which you can enter CoffeeScript, see the JavaScript output, and run it directly in the browser.

## Citations
# Github. "[http://github.com/repositories Interesting Repositories]", Github, Nov 10, 2010.
# Carson, Ryan. "[http://thinkvitamin.com/mobile/new-rails-like-framework-from-37signals-for-html5-mobile-apps/ New Rails-like Framework from 37signals for HTML5 Mobile Apps]", Think Vitamin blog, Nov 8, 2010.
# Hagenburger, Nico. "[http://www.hagenburger.net/TALKS/rails-3.1-frontend-performance.html Rails 3.1 – A Sneak Preview]", presentation for [http://railscamp-hamburg.de/ Railscamp Hamburg] on Oct 23, 2010.
# Ashkenas, Jeremy. "[http://github.com/jashkenas/coffee-script/issues/830 The Plan for 1.0]", Github issue tracker, Nov 4, 2010.

## External links
* [http://github.com/jashkenas/coffee-script/ GitHub repository]
* [http://twitter.com/coffeescript @CoffeeScript] - Twitter feed
* [Wikipedia Entry](https://en.wikipedia.org/wiki/coffeescript)

[Category:JavaScript programming language family](https://rosettacode.org/wiki/Category:JavaScript_programming_language_family)
