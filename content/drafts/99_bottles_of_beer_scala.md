+++
title = "99 Bottles of Beer/Scala"
description = ""
date = 2019-05-04T20:31:05Z
aliases = []
[extra]
id = 5295
[taxonomies]
categories = []
tags = []
+++

{{collection|99 Bottles of Beer}}
{{libheader|Scala}}

==The trivial solution==
The trivial solution to it would be this:

```scala
99 to 1 by -1 foreach { n =>
  println(
    f"$n%d bottles of beer on the wall\n" +
      f"$n%d bottles of beer\n" +
      f"Take one down, pass it around\n" +
      f"${n - 1}%d bottles of beer on the wall\n")
}
```


### Running in parallel

The above n parallel using a ParRange, fast but shuffles the output.

```Scala
(99 to 1 by -1).par foreach { n =>
  println(
    f"$n%d bottles of beer on the wall\n" +
      f"$n%d bottles of beer\n" +
      f"Take one down, pass it around\n" +
      f"${n - 1}%d bottles of beer on the wall\n")
}
```

==Regex solution==

```Scala
object NinetyNineBottlesOfBeer {
  val verse = """|99 bottles of beer on the wall
                 |99 bottles of beer
                 |Take one down, pass it around
                 |98 bottles of beer on the wall""".stripMargin
  
  val song = new scala.collection.mutable.Queue() ++= verse.lines += ""
  val Bottles = "(\\d+) bottles of beer.*".r
  
  def changeLine(line: String) = line match {
      case Bottles("0") => song clear ()
      case Bottles(n) => song enqueue line.replace(n, n.toInt - 1 toString)
      case _ => song enqueue line
    }
  
  def sing = while(!song.isEmpty) {
    val line = song dequeue ()
    println(line)
    changeLine(line)
  }
}
```

==A preferred ☺ sollution ==

```Scala
// Note - As Of Scala 2.11.0 The 'Scala Actors' Library Has Been Deprecated In Favor Of Akka.
object Song {
  import scala.actors._
  import scala.actors.Actor._
  
  abstract class Beverage { def name = this.toString.toLowerCase }
  case object Beer extends Beverage
  
  object Wall {
    private var contents: List[Beverage] = Nil

    def count(what: Beverage) = contents count (_ == what)
    def isEmpty = contents isEmpty
    def stock(n: Int, what: Beverage) = contents :::= List.fill(n)(what)
    def get(what: Beverage) {
      def takeOneFrom(contents: List[Beverage]): List[Beverage] = contents match {
        case `what` :: rest => rest
        case other :: rest => other :: takeOneFrom(rest)
        case Nil => println("Sorry, we are out of "+what.toString.toLowerCase); Nil
      }
      contents = takeOneFrom(contents)
    }
  }

  sealed abstract class Messages
  case class SingSong(what: Beverage) extends Messages
  case class HowManyMore(what: Beverage) extends Messages
  case class HowManyNow(what: Beverage) extends Messages
  case class ThereAreStill(n: Int, what: Beverage) extends Messages
  case class ThereAreNow(n: Int, what: Beverage) extends Messages
  case class Gimme(what: Beverage) extends Messages
  case class HereIs(what: Beverage) extends Messages
  case object ClosingTime extends Messages
  
  def plural(count: Int, noun: String, nouns: String) = if (count == 1) noun else nouns
  def countIt(n: Int, what: Beverage) = "%d %s of %s" format (n, plural(n, "bottle", "bottles"), what.name) 
  
  object Waitress extends Actor {
    def tellThem(what: String) = println("%s on the wall" format what)
    
    def act = loop {
      react {
        case HowManyMore(it) =>
          val total = Wall count it
          tellThem(countIt(total, it))
          reply (ThereAreStill(total, it))
        case Gimme(it) =>
          print("Take one down, ")
          Wall get it
          reply (HereIs(it))
        case HowManyNow(it) =>
          val total = Wall count it
          tellThem(countIt(total, it))
          if (Wall isEmpty) {
              reply (ClosingTime) // You don't have to go home, but you can't stay here
              exit
          } else
          reply (ThereAreNow(total, it))
        case _ =>
          println("You wish, honey!")
      }
    }
  }
  
  object Patrons extends Actor {
    def act = loop {
      react {
        case SingSong(what: Beverage) =>
          Waitress ! HowManyMore(what)
        case ThereAreStill(n, it) =>
          println(countIt(n, it))
          Waitress ! Gimme(it)
        case HereIs(it) =>
          println("pass it around")
          Waitress ! HowManyNow(it)
        case ThereAreNow(n, it) =>
          println()
          Waitress ! HowManyMore(it)
        case ClosingTime =>
          exit
        case _ =>
          println("Say what???")
      }
    }
  }
  
  def Sing99Beers = {
    Wall stock (99, Beer)
    Waitress.start
    Patrons.start
    
    Patrons ! SingSong(Beer)
  }
}
```

