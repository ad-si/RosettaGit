+++
title = "Conway's Game of Life/Scala"
description = ""
date = 2010-01-08T15:28:52Z
aliases = []
[extra]
id = 5290
[taxonomies]
categories = []
tags = []
+++

{{collection|Conway's Game of Life}}
{{works with|Scala|2.8}}
A Conway "board" has infinite dimensions -- in fact, Conway's Game of Life is Turing complete -- so
the algorithm below avoids using fixed-size structures such as arrays. Instead, each generation is
represented by a set of the coordinates which are "alive".

The solution will be presented in four sections: coordinates, generations, sample patterns and tester.
The first two implement everything that is needed for Conway's Game of Life. The sample patterns
includes four classes of patterns that are tested. The tester checks some specific characteristic of
each pattern, and then shows the three generations of blinker, as requested.

'''Coordinates'''

The coordinates to any cell in the board is represented as an immutable tuple of integers. To speed up 
computation speed, each coordinate keeps a list of all its neighbors. This is computed on
demand, as some coordinates may never become alive and, thus, never require it.

All coordinates are memoized (cached), so that the list of neighbors will never need to be recomputed
for any coordinate. This can lead to memory starvation, however.

No coordinate will be accepted at the limits of the representation (Int). If any such coordinate is
created, an exception will be thrown to indicate the board can no longer be correctly represented.


```scala
class Coord private (val x: Int, val y: Int) {
  private val offsets = List(-1, 0, 1)
  private def offsetsOf(n: Int) = offsets map (_ + n)
  
  /**
   * A memoized list of all neighbors of a coordinate
   */
  lazy val neighbors = for {
    xn <- offsetsOf(x) if Coord.legal(xn)
    yn <- offsetsOf(y) if Coord.legal(yn) && (x, y) != (xn, yn)
  } yield Coord(xn, yn)

  // Coordinates can be used as offsets
  def +(c: Coord) = Coord(x + c.x, y + c.y)
  def -(c: Coord) = Coord(x - c.x, y - c.y)

  override def equals(other: Any) = other match {
    case that: Coord => this.x == that.x && this.y == that.y
    case _ => false
  }
  override def hashCode = ((x * 41) + y) * 41 + 41
  override def toString = "Coord(%d, %d)" format (x, y)
}

object Coord {
  // A Conway board is infinite in size; throw an exception if our hard limits are reached
  private def legal(n: Int) = {
    n.ensuring(Int.MinValue < _, "Coord too low").ensuring(_ < Int.MaxValue, "Coord too high")
    true
  }
  private val cache = new scala.collection.mutable.HashMap[(Int,Int), Coord]
  
  /**
   * Factory for coordinates. All coordinates are memoized.
   */
  def apply(x: Int, y: Int) = {
    require(legal(x) && legal(y))
    cache getOrElseUpdate ((x,y), new Coord(x, y))
  }
  
  /**
   * Coordinate extractor
   */
  def unapply(c: Coord) = Some((c.x, c.y))
  
  /**
   * An Ordering for coordinates which sorts by the X coordinate
   */
  val xOrdering = Ordering.fromLessThan((_: Coord).x < (_: Coord).x)

  /**
   * An Ordering for coordinates which sorts by the Y coordinate
   */
  val yOrdering = Ordering.fromLessThan((_: Coord).y < (_: Coord).y)

  /**
   * Any Tuple2[Int, Int] can be used as a Coordinate through this implict
   * conversion.
   */
  implicit def coordFromTuple(t: (Int, Int)) = apply(t._1, t._2)
}
```


'''Generation'''

Generations are represented as immutable sets of coordinates for the cells which
are alive at that generation. They extend Scala's immutable Set, but adds a few
methods. They can return the next generation, of course, and also have a few
methods to help with that. When converted to string, it will return a representation
of the smallest board that fully contains the pattern. It can also return
representation for fixed-coordinates windows. Last, it can return a new generation
with the pattern recentered.


```scala
class Generation(val alive: Set[Coord]) extends Set[Coord] {
  import Generation._

  // Abstract methods that need to be defined as a Set
  def contains(elem: Coord): Boolean = alive contains elem
  def iterator: Iterator[Coord] = alive.iterator
  def +(elem: Coord): Generation = if (alive contains elem) this else Generation(alive + elem)
  def -(elem: Coord): Generation = if (alive contains elem) Generation(alive - elem) else this
  
  // Helper methods to be able to pass tuples of Int instead of Coord
  def apply(x: Int, y: Int): Boolean = apply(Coord(x, y))
  def +(x: Int, y: Int): Generation = this.+(Coord(x, y))
  def -(x: Int, y: Int): Generation = this.-(Coord(x, y))
  def ++(coords: Iterator[(Int,Int)]): Generation = Generation(alive ++ (coords map (c => c: Coord))) 
  def ++(coords: Traversable[(Int,Int)]): Generation = Generation(alive ++ (coords map (c => c: Coord)))

  /**
   * A list containing all coordinates that are neighbors of a cell which is alive, together
   * with the number of alive cells it is neighbor of.
   */
  lazy val neighbors = alive.toList flatMap (_ neighbors) groupBy identity map { case (c, l) => (c, l.size) }

  // Filter all neighbors for desired characteristics     
  def neighborhood(filter: Filter) = for (filter(coord) <- neighbors) yield coord
  def babies = neighborhood(Fecund)
  def adults = alive & neighborhood(Stable).toSet
  
  /**
   * The next generation is composed of babies from fecund neighborhoods and adults on stable
   * neighborhoods.
   */
  def nextGeneration = Generation(adults ++ babies)

  /**
   * Return a string with the representation of this generation on a window
   * defined by its upper-left and lower-right coordinates.
   */
  def windowToString(upperLeft: Coord, lowerRight: Coord) = {
    def toChar(c: Coord) = if (alive contains c) 'X' else ' '
    def toRow(y: Int) = for (x <- upperLeft.x to lowerRight.x) yield toChar(Coord(x, y))
    def toMatrix = for (y <- upperLeft.y to lowerRight.y by -1) yield toRow(y).mkString
    toMatrix mkString "\n"
  }
  
  /**
   * This generation's upper left corner
   */
  lazy val upperLeft = {
    val x = alive min Coord.xOrdering x;
    val y = alive max Coord.yOrdering y;
    Coord(x, y)
  }

  /**
   * This generation's lower right corner
   */
  lazy val lowerRight = {
    val x = alive max Coord.xOrdering x;
    val y = alive min Coord.yOrdering y;
    Coord(x, y)
  }
  
  /**
   * Recenter the pattern without altering its disposition
   */
  def recenter = {
    val offset = Coord(
      upperLeft.x + (lowerRight.x - upperLeft.x) / 2,
      lowerRight.y + (upperLeft.y - lowerRight.y) / 2
    )
    Generation(alive map (_ - offset))
  }

  override def equals(other: Any) = other match {
    case that: Generation => this.alive == that.alive
    case _ => false
  }
  override def hashCode = alive.hashCode
  override def toString = windowToString(upperLeft, lowerRight)
}

object Generation {
  def apply(coords: Iterator[Coord]): Generation = apply(coords.toSeq)
  def apply(coords: Traversable[Coord]): Generation = apply(coords.toSet)
  def apply(alive: Set[Coord]) = new Generation(alive)
  def apply() = new Generation(Set.empty[Coord])
  
  // Helper class to filter neighbors for desired qualities
  class Filter(f: ((Coord, Int)) => Option[Coord]) {
    def unapply(t: (Coord, Int)): Option[Coord] = f(t)
  }
  object Filter {
    def apply(f: ((Coord, Int)) => Option[Coord]) = new Filter(f)
  }
  
  // A fecund filter will return all coordinates with three neighbors alive
  val Fecund = Filter {
    case (c, 3) => Some(c)
    case _ => None
  }
  
  // A stable filter will return all coordinates with two or three neighbors alive
  val Stable = Filter {
    case (c, 2) => Some(c)
    case (c, 3) => Some(c)
    case _ => None
  }
}
```


'''Conway's Game of Life Patterns'''

A number of patterns are provided, which are used to test the program. They are divided
into still lives, oscillators, spaceships and methuselahs. Still lives are immutable 
patterns. Oscillators are patterns that follow a fixed length cycle, after which they
repeat themselves. Spaceships are patterns that move across the board -- they also have 
a fixed length cycle, after which they repeat themselves at some offset from their original
position. Methuselahs are patterns that take a long time to stabilize. When stabilized, they
consist of still lives, oscillators and gliders, but have a fixed population size.

A few helper methods are provided to get these patterns into use.


```scala
object ConwayPatterns {
  // Lists for all patterns available
  def stillLives = List(block, beehive, loaf, boat) map ((_, 1))
  def oscillators = oscillators2.map((_, 2)) ::: oscillators3.map((_, 3)) 
  def oscillators2 = List(blinker, toad, beacon)
  def oscillators3 = List(pulsar)
  def spaceships = List(glider, LWSS) map ((_, 4)) 
  def methuselahs = List((diehard, 130), (acorn, 5206), (rPentomino, 1103))

  // Still Lives patterns
  val block = """|
                 | XX
                 | XX
                 |"""
  val beehive = """|
                   |  XX
                   | X  X
                   |  XX
                   |"""
  val loaf = """|
                |  XX
                | X  X
                |  X X
                |   X
                |"""
  val boat = """|
                | XX
                | X X
                |  X
                |"""
  
  // Oscillators patterns
  val blinker = """|
                   |
                   | XXX
                   |
                   |"""
  val toad = """|
                |
                |  XXX
                | XXX
                |
                |"""
  val beacon = """|
                  | XX
                  | XX
                  |   XX
                  |   XX
                  |"""
  val pulsar = """|
                  |
                  |    XXX   XXX
                  |
                  |  X    X X    X
                  |  X    X X    X
                  |  X    X X    X
                  |    XXX   XXX
                  |
                  |    XXX   XXX
                  |  X    X X    X
                  |  X    X X    X
                  |  X    X X    X
                  |
                  |    XXX   XXX
                  |
                  |"""

  // Spaceship patterns
  val glider = """|
                  |   X
                  | X X
                  |  XX
                  |"""
  val LWSS = """|
                |
                |  XXXX
                | X   X
                |     X
                | X  X
                |"""
               
  // Methuselah patterns
  val diehard = """|
                   |       X
                   | XX
                   |  X   XXX
                   |"""
  
  val acorn = """|
                 |  X
                 |    X
                 | XX  XXX
                 |"""
                 
  val rPentomino = """|
                      | XX
                      |  XX
                      |  X
                      |"""
                      
  // Helper methods
  // Enable constructing sets of coordinates from string patterns.
  implicit def coordsFromPattern(pattern: String) = for {
    (xs, y) <- pattern.stripMargin.split('\n').map(_.zipWithIndex).zipWithIndex.iterator
    (c, x) <- xs.iterator
    if c != ' '
  } yield Coord(x, y)
                      
  // Move a set of coordinates to a point
  def moveTo(pattern: String, to: Coord) = (pattern: Iterator[Coord]) map (_ + to)
  def moveTo(coords: Iterator[Coord], to: Coord) = coords map (_ + to)
  def moveTo(coords: Traversable[Coord], to: Coord) = coords map (_ + to)
  
}
```


'''Tester'''

We test still lives, oscillators and spaceships for their cycle length -- where still lives
are considered to have a cycle length of 1 -- it repeats itself every generation. Methuselahs
are tested for the number of generations until the population stabilizes. The test for 
stabilization is not fully accurate, as it just checks that the population is the same after
a number of generations.

It also prints three generations of a blinker, as requested.


```scala
object ConwayTester {
  import ConwayPatterns._
  val MaxGenerations = 5500 // Give up at MaxGenerations to avoid spending too much time on errors
  val WindowSize = 10 // To check for stable populations, use a window this big
  import Coord.{xOrdering, yOrdering} 

  /**
   * Return an iterator for the generations of a starting pattern
   */
  def conwayIterator(first: Generation) = Iterator.iterate(first)(_.nextGeneration)
  
  /**
   * Return the period (number of different generations) for oscillators
   */
  def getPeriod(first: Generation) = {
    val it = conwayIterator(first)
    it.next // drop first generation
    (it take MaxGenerations takeWhile (_ != first) length) + 1
  }
  
  /**
   * Return the period (number of different generations, ignoring offset) for
   * spaceships.
   */
  def getSpaceshipPeriod(first: Generation) = {
    val it = conwayIterator(first) map (_.recenter)
    it.next // drop first generation
    (it take MaxGenerations takeWhile (_ != first) length) + 1
  }
  
  /**
   * Return the number of generations until the population of a pattern
   * stabilizes. This test only checks a window of generations for
   * population size, as the test for spaceships won't work when multiple
   * spaceships are present.
   */
  def getUnstableGenerations(first: Generation) = (
    conwayIterator(first) 
    take MaxGenerations
    map (_.size)
    sliding WindowSize
    map (_.removeDuplicates.length)
    takeWhile (_ > 1) 
    length
  )
  
  /**
   * Return the first generation, properly centered, for a given pattern
   * as represented by a string.
   */
  def initPattern(pattern: String) = 
    Generation(pattern: Iterator[Coord]).recenter
    
  /**
   * For each pattern passed, apply a function which will measure some characteristic
   * of the generations of that pattern, and assert it is equal to expected value.
   */
  def testHarness(patterns: Traversable[(String, Int)], test: Generation => Int, msg: String) =
    assert(patterns forall { case (pattern, period) => test(initPattern(pattern)) == period }, msg)

  // Available tests
  def testStillLives = testHarness(stillLives, getPeriod _, "Failure in still lives")
  def testOscillators = testHarness(oscillators, getPeriod _, "Failure in oscillators")
  def testSpaceships = testHarness(spaceships, getSpaceshipPeriod _, "Failure in spaceships")
  def testMethuselahs = testHarness(methuselahs, getUnstableGenerations _, "Failure in methuselahs")
  
  /**
   * Do all available tests
   */
  def testAll {
    testStillLives
    testOscillators
    testSpaceships
    testMethuselahs
  }
  
  /**
   * Do all available tests, and print three generations of a blinker on a 3x3 window.
   */
  def main(args: Array[String]) {
    val upperLeft = Coord(-1, 1)
    val lowerRight = Coord(1, -1)
    testAll
    println("Passed all tests. Printing three generations of blinker:")
    conwayIterator(initPattern(blinker)).zipWithIndex.take(3).toList zip List("st", "nd", "rd") foreach { 
      case ((generation, nth), suffix) => 
        println((nth + 1) + suffix + " generation: \n"+generation.windowToString(upperLeft, lowerRight)+"\n")
    }
  }
}
```


Output:


```txt

Passed all tests. Printing three generations of blinker:
1st generation:

XXX


2nd generation:
 X
 X
 X

3rd generation:

XXX


```

