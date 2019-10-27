+++
title = "99 Bottles of Beer/Java/Object Oriented"
description = ""
date = 2009-08-06T20:54:51Z
aliases = []
[extra]
id = 4664
[taxonomies]
categories = []
tags = []
+++

Another solution for [[99 Bottles of Beer]], which in addition correctly handles the grammar. This solution is object-oriented. It is completely overkill for this problem.


```java

/*************************
 * Interface for things. *
 *************************/
interface Thing {
    String singular();
    String plural();
}

/***************
 * Containers. *
 ***************

 Containers are things which can contain other things. The
 following class makes any thing into a container. The container
 class is actually a decorator which makes any thing into a
 container. Note that the contained thing is actually mutable,
 even if the container is not. Note that the container can only
 contain a single thing; if it shall contain several things, make
 it contain a collection instead. */
class Container implements Thing {
    /** The format gives the name. %self% is replaced by the containing
     *  object's name (in proper pluralization), %contained% is
     *  replaced by the contained object's name. */
    private final String format;
    private final Thing self;
    private final Thing containedThing;

    public Container(String fmt, Thing what, Thing contained) {
        format = fmt;
        self = what;
        containedThing = contained;
    }

    public String singular() {
        return format.replace("%self%", self.singular())
            .replace("%contained%", containedThing.singular());
    }

    public String plural() {
        return format.replace("%self%", self.plural())
            .replace("%contained%", containedThing.singular());
    }
}

/*********************************
 * A collection of equal things. *
 *********************************

 In the context of this program, a collection of things is again
 considered a single thing.
 This is a concrete class. */
class EqualCollection implements Thing {
    private int countOfThings;
    private final Thing typeOfThing;

    public EqualCollection(int count, Thing what) {
        countOfThings = count;
        typeOfThing = what;
    }

    /** get singular. The singular of the collection is just the number
     *  followed by the thing, proper pluralized. The fact that it's
     *  grammatically still a plural form doesn't matter for the problem
     *  at hand. */
    public String singular() {
        StringBuilder sb = new StringBuilder();
        sb.append(countOfThings + " ");
        if (countOfThings == 1)
            sb.append(typeOfThing.singular());
        else
            sb.append(typeOfThing.plural());
        return sb.toString();
    }

    /** get plural. For collections, the plural is just "times " followed
     *  by the singular. That is 3 collections of 4 bottles each give 3
     *  times 4 bottles.
     *  This has to be implemented, even if it isn't used,
     *  because the it is specified by the interface, and this
     *  class is not abstract. */
    public String plural() {
        return "times " + singular();
    }

    /** tell if there are still things to take away. There are things to
     *  take away if there are more than 0 things. */
    public boolean thereIsSomeLeft() {
        return countOfThings > 0;
    }

    /** this takes one thing away from the collection. Taking a thing
     *  away from an empty collection is undefined behaviour (i.e. not
     *  explicitly checked). */
    public void takeOneAway() {
        --countOfThings;
    }
}

/************
 * The beer *
 ************/
class Beer implements Thing {
    public String singular() { return "beer"; }
    public String plural() { return "beers"; }
}

/**************
 * The bottle *
 **************/
class Bottle implements Thing {
    public String singular() { return "bottle"; }
    public String plural() { return "bottles"; }
}

/************
 * The wall *
 ************/
class Wall implements Thing {
    public String singular() { return "wall"; }
    public String plural() { return "walls"; }
}

/** this is the class for the song. */
public class Song {
    private final Thing beverage = new Beer();
    private final Thing drinkSource = new Bottle();
    private final Thing bottleOfBeer =
        new Container("%self% of %contained%", drinkSource, beverage);
    private final EqualCollection collectionOfBottles;
    private final Thing bottleStorage = new Wall();
    private final Thing wallOfBottles;

    public Song(int bottleCount) {
        collectionOfBottles =
            new EqualCollection(bottleCount, bottleOfBeer);
        wallOfBottles =
            new Container("%contained% on the %self%", bottleStorage, collectionOfBottles);
    }

    public void sing(java.io.PrintStream where) {
        while (collectionOfBottles.thereIsSomeLeft()) {
            where.println(wallOfBottles.singular() + ".");
            where.println(collectionOfBottles.singular() + ".");
            where.println("Take one down, pass it around.");
            collectionOfBottles.takeOneAway();
            where.println(wallOfBottles.singular() + ".");
            where.println();
        }
    }

    public static void main(String[] args) {
        Song song = new Song(100);
        song.sing(System.out);
    }
}

```

