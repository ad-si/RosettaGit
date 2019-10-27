+++
title = "Talk:Constrained genericity"
description = ""
date = 2010-09-09T07:23:19Z
aliases = []
[extra]
id = 8164
[taxonomies]
categories = []
tags = []
+++

== C++ example, concepts and C++0x ==

Are there any C++ compilers which implemented the feature, and which haven't deprecated their use? My understanding of the C++ standardizations process is that many of the features that get included in the planning stages are already available as compiler extension. In that case, I think [[Template:Works with]] might be an appropriate solution, with a note that it's a compiler extension. --[[User:Short Circuit|Michael Mol]] 16:29, 27 August 2010 (UTC)

: There was ConceptGCC (now in a branch of the official gcc archive, but no longer actively developed, and not part of any official gcc release). I just tested with the latest conceptgcc I have installed: gcc-Version 4.3.0 20070330 (experimental) (Indiana University ConceptGCC -- BoostCon Edition) and found that it didn't yet implement all of the used features: std::Movable wasn't provided (but replacing with pure typename works), and some logic for std::Derived is missing: calling t.munch() doesn't work (there's a mistake in the code on the page in that it uses "->" instead of ".", but fixing that doesn't help). I don't know how much of the missing functionality was implemented later. As far as I know, there's no current compiler which implements concepts.
: One possibility would be to park the code on the discussion page (or maybe I park it in a subpage of my user page) until (hopefully) concepts get reintroduced to a later version of the standard.
: When I added the entry, I was completely convinced that concepts would come (after all, they had been a highlighted feature of the next C++ version, and a lot of other features were defined in terms of concepts at that time). --[[User:Ce|Ce]] 20:22, 27 August 2010 (UTC)

==C++ code moved here==
Concepts have been removed from C++0x, but might appear in some future version. Code parked here to be put back if concepts reappear. --[[User:Ce|Ce]] 07:22, 9 September 2010 (UTC)


###  The moved section 

The current C++ standard doesn't support constrained genericity (however you can emulate it by having the container refer to the corresponding eat function without actually calling it). The next version will, however, allow it through concepts:

```cpp>#include <concepts

#include <vector>

auto concept Eatable<typename T> // auto makes it apply automatically
{
  void eat(T);
};

template<std::Moveable T>
 requires Eatable<T>
class FoodBox
{
public:
  std::vector<T> food;
};
```

The only requirement to implement an Eatable type is, indeed, that a suitable function <tt>eat</tt> is defined for it (to put it in the FoodBox, in addition it has to be Moveable, since <tt>std::vector</tt> requires that; but that's ortogonal to the type being Eatable). A possible implementation of an eatable type could be:

```cpp
class Banana {};
void eat(Banana const &) {}
```

Even a built-in type can be made eatable by defining a suitable <tt>eat</tt> function. The following makes <tt>double</tt> an eatable type:

```cpp
void eat(double) {}
```


Another way to make an existing type eatable is to use a concept map. Let's assume we have an abstract base class <tt>Food</tt> which looks like this;

```cpp
class Food
{
public:
  virtual void munch() = 0;
  virtual ~Food() {}
};
```

Then we can make all classes derived from Food eatable using <tt>Food::munch()</tt> for <tt>eat</tt> with the following concept map template:

```cpp>template<std::DerivedFrom<Food> T

 concept_map Eatable<T>
{
  void eat(T const& t) { t.munch(); }
}
```

The difference to a global function <tt>void eat(Food const&)</tt> is that the function in the concept map is only visible to functions using that concept, thus reducing namespace polution. Functions directly operating on <tt>Food</tt> objects can use the interface provided by <tt>Food</tt> itself, e.g. <tt>apple.munch()</tt>, or explicitly invoke <tt>Eatable<Food>::eat(apple)</tt>. Of course, concept maps also work with built-in types:

```cpp
concept_map Eatable<int>
{
  void eat(int) {}
}
```

