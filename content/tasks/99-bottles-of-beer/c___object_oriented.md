+++
title = "C++ - Object Oriented"
description = ""
date = 2009-08-06T20:57:13Z
aliases = []
[extra]
id = 4665
[taxonomies]
categories = []
tags = []
+++

Another solution, which in addition correctly handles the grammar. This solution is object-oriented. It is completely overkill for this problem.

{{works with|GCC|4.1.2 20061115 (prerelease) (SUSE Linux)}}

```cpp

#include <iostream>
#include <string>
#include <sstream>

namespace bottle_song
{
  //
### ===========================================================


  // ***********************************
  // * Abstract base class for things. *
  // ***********************************

  class thing
  {
  public:
    // return the singular of the thing
    virtual std::string singular() const = 0;

    // return the plural of the thing
    virtual std::string plural() const = 0;

    // we need a virtual destructor, too
    virtual ~thing() {}
  };

  //
### ===========================================================


  // ***************
  // * Containers. *
  // ***************

  // Containers are things which can contain other things. The
  // following class makes any thing into a container. The container
  // class is actually a decorator which makes any thing into a
  // container. Note that the contained thing is actually mutable,
  // even if the container is not. Note that the container can only
  // contain a single thing; if it shall contain several things, make
  // it contain a collection instead.

  class container: public thing
  {
  public:
    // The format gives the name. %self% is replaced by the containing
    // object's name (in proper pluralization), %contained% is
    // replaced by the contained object's name.
    container(std::string fmt, thing const& what, thing const& contained);
    std::string singular() const;
    std::string plural() const;
  private:
    std::string format;
    thing const& self;
    thing const& contained_thing;
    // helper function to replace strings
    static void replace(std::string& str, std::string from, std::string to);
  };

  container::container(std::string fmt,
                       thing const& what,
                       thing const& contained):
    format(fmt),
    self(what),
    contained_thing(contained)
  {
  }

  std::string container::singular() const
  {
    std::string result = format;
    replace(result, "%self%", self.singular());
    replace(result, "%contained%", contained_thing.singular());
    return result;
  }

  std::string container::plural() const
  {
    std::string result = format;
    replace(result, "%self%", self.plural());
    replace(result, "%contained%", contained_thing.singular());
    return result;
  }

  void container::replace(std::string& str, std::string from, std::string to)
  {
    std::string::size_type pos = str.find(from);
    if (pos != std::string::npos)
      str.replace(pos, from.length(), to);
  }
  //
### ===========================================================


  // *********************************
  // * A collection of equal things. *
  // *********************************

  // In the context of this program, a collection of things is again
  // considered a single thing.
  // This is a concrete class.
  class equal_collection: public thing
  {
  public:
    // constructor
    equal_collection(int count, thing const& what);

    // get singular
    std::string singular() const;

    // get plural. This has to be implemented, even if it isn't used,
    // because the inherited version is pure virtual, and not
    // implementing this would make the class abstract.
    std::string plural() const;

    // this just returns whether thwere are still things left to take away.
    bool there_is_some_left();

    // this takes one thing away from the collection. Taking a thing
    // away from an empty collection is undefined behaviour (i.e. not
    // explicitly checked).
    void take_one_away();
  private:
    int count_of_things;
    thing const& type_of_thing;
  };

  // equal_collection constructor
  equal_collection::equal_collection(int count, thing const& what):
    count_of_things(count),
    type_of_thing(what)
  {
  }

  // get singular. The singular of the collection is just the number
  // followed by the thing, proper pluralized. The fact that it's
  // grammatically still a plural form doesn't matter for the problem
  // at hand.
  std::string equal_collection::singular() const
  {
    std::ostringstream oss;
    oss << count_of_things << " ";
    if (count_of_things == 1)
      oss << type_of_thing.singular();
    else
      oss << type_of_thing.plural();
    return oss.str();
  }

  // get plural. For collections, the plural is just "times " followed
  // by the singular. That is 3 collections of 4 bottles each give 3
  // times 4 bottles.
  std::string equal_collection::plural() const
  {
    return "times " + singular();
  }

  // tell if there are still things to take away. There are things to
  // take away if there are more than 0 things.
  bool equal_collection::there_is_some_left()
  {
    return count_of_things > 0;
  }

  // take one thing away from the collection. That is, just decrement
  // the count of things.
  void equal_collection::take_one_away()
  {
    --count_of_things;
  }

  //
### ===========================================================


  // ************
  // * The beer *
  // ************

  class beer: public thing
  {
  public:
    std::string singular() const { return "beer"; }
    std::string plural() const { return "beers"; }
  };

  //
### ===========================================================


  // **************
  // * The bottle *
  // **************

  class bottle: public thing
  {
  public:
    std::string singular() const { return "bottle"; }
    std::string plural() const { return "bottles"; }
  };

  //
### ===========================================================


  // ************
  // * The wall *
  // ************

  class wall: public thing
  {
  public:
    std::string singular() const { return "wall"; }
    std::string plural() const { return "walls"; }
  };

  //
### ===========================================================


  // this is the class for the song.
  class song
  {
  public:
    song(int bottle_count);
    void sing(std::ostream& where); // note: singing the song modifies it!
  private:
    beer beverage;
    bottle drink_source;
    container bottle_of_beer;
    equal_collection collection_of_bottles;
    wall bottle_storage;
    container wall_of_bottles;
  };

  song::song(int bottle_count):
    bottle_of_beer("%self% of %contained%", drink_source, beverage),
    collection_of_bottles(bottle_count, bottle_of_beer),
    wall_of_bottles("%contained% on the %self%",
                    bottle_storage, collection_of_bottles)
  {
  }

  void song::sing(std::ostream& where)
  {
    while (collection_of_bottles.there_is_some_left())
    {
      where << wall_of_bottles.singular() << ".\n"
            << collection_of_bottles.singular() << ".\n"
            << "Take one down, pass it around.\n";
      collection_of_bottles.take_one_away();
      where << wall_of_bottles.singular() << ".\n\n";
    }
  }
}

int main()
{
  bottle_song::song song(100);
  song.sing(std::cout);
  return 0;
}

```

