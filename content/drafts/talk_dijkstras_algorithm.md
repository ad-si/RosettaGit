+++
title = "Talk:Dijkstra's algorithm"
description = ""
date = 2017-01-19T16:05:52Z
aliases = []
[extra]
id = 11027
[taxonomies]
categories = []
tags = []
+++

== Why is this a draft task? ==

I was surprised to see that this is a draft task.  It's a pretty classic programming problem, and there are now six implementations.  I don't see any obvious flaws in the problem statement. --[[User:Showell|Showell]] 18:33, 24 January 2012 (UTC)

:It works the way it is.  I would like better though if it prescribed some test data.  The small example from WP would be okay.  A large data set would be great, but my idea of linking words turned out kind of strange, and no other ideas have surfaced.  I also think it would be nice if the task asked for solutions to note anything relevant about optimization.  The priority queue in particular is a popular optimization of Dijkstra's original algorithm. &mdash;[[User:Sonia|Sonia]] 02:42, 25 January 2012 (UTC)
:: I admit I didn't grok what distance metric you were using, so I ignored that part of your code. :-) In general though, I concur that a common example is a good idea. –[[User:Dkf|Donal Fellows]] 13:50, 25 January 2012 (UTC)
:The WP example just numbers the vertices, but I liked the way most solutions here use letters.  The documentation requirement is something I've wanted on a number of other tasks.  It should not only illustrate where variant algorithms exist, but help people comparing solutions recognize if two solutions implement the same algorithm or different variants. &mdash;[[User:Sonia|Sonia]] 18:52, 25 January 2012 (UTC)

:I'm floating a few more changes because it would be nice for this to come out of draft status looking all polished.  I moved the documentation requirement to EC because I didn't want it to scare people from submitting initial solutions, and because I saw the tendency for documentation to get wordy.  The directed/undirected point is important, and there's another point I'll give a new section below, single path/path tree. &mdash;[[User:Sonia|Sonia]] 21:25, 6 February 2012 (UTC)

Ugh! This draft task is a mess. The solutions are in three groups: those who think it is a directed graph and show their output, those who think it is an undirected graph and show their output (a larger number than the directed graph camp), and those who don't show any output of the search at all (the largest group of all, though not a majority). No way can we take this to a full task with this much disagreement. Output of the shortest path and its cost ''must'' be present (use {{tmpl|output?}} mercilessly), and we must resolve which type of graph we're dealing with; changing the task after someone has solved it ''and not using the {{tmpl|update}} task to let them know'' isn't playing fair. –[[User:Dkf|Donal Fellows]] ([[User talk:Dkf|talk]]) 16:23, 11 May 2014 (UTC)

== Java example cleanup ==

I've done some cleaning up of the Java example, making many of the problems in the code much less prominent. (For example, I've reduced the use of effectively-global variables and I've split the I/O from the algorithm.) More work is needed to finish transforming it from something that stank of old-skool [[C]], but it's a long way there. I've not really addressed any of the issues raised in the alertbox (e.g., the arbitrary node limit is still there — not that you'd want to have to type in the distance matrix for anything that large anyway) but they should now be much easier for someone else to address. –[[User:Dkf|Donal Fellows]] 03:22, 9 December 2011 (UTC)

I think there may be a bug: The java one uses a TreeSet to maintain the unvisited set - but it is doing equality comparisons with compareTo, which is looking at the distance and not the identity of the node. So if two distinct nodes happen to have the same dist value at the same time, then either one could get removed from the unvisited set.

There are several bugs in the Java realization of this algorithm 


For example  for the Graph     

private static final Graph.Edge[] GRAPH = {
            new Graph.Edge("17", "1", 20),
            new Graph.Edge("1", "17", 20),
            new Graph.Edge("1", "7", 2),
            new Graph.Edge("7", "14", 15),
            new Graph.Edge("1", "3", 5),
            new Graph.Edge("3", "14", 2),
            new Graph.Edge("1 ", "2", 5),

    };

   private static final String START = "17";
    private static final String END = "14";

The path from the vertex "17" to "14" will be 

17 -> 1 -> 7 -> 14 with summary distance 37 
while it must me 

17 -> 1 -> 3 -> 14 with summary distance 27


please see the fixed version of this algorithm below 


package graph;

/**
 * @author Archil Matchavariani
 */

import java.util.*;

public class Dijkstra {
//    private static final Graph.Edge[] GRAPH = {
//            new Graph.Edge("a", "b", 7),
//            new Graph.Edge("a", "c", 9),
//            new Graph.Edge("a", "f", 14),
//            new Graph.Edge("b", "c", 10),
//            new Graph.Edge("b", "d", 15),
//            new Graph.Edge("c", "d", 11),
//            new Graph.Edge("c", "f", 2),
//            new Graph.Edge("d", "e", 6),
//            new Graph.Edge("e", "f", 9),
//    };
//    private static final String START = "a";
//    private static final String END = "e";


    private static final Graph.Edge[] GRAPH = {
            new Graph.Edge("17", "1", 20),
            new Graph.Edge("1", "17", 20),
            new Graph.Edge("1", "7", 2),
            new Graph.Edge("7", "14", 15),
            new Graph.Edge("1", "3", 5),
            new Graph.Edge("3", "14", 2),
            new Graph.Edge("1 ", "2", 5),
            new Graph.Edge("1 ", "6", 5),

    };

  
    private static final String START = "17";
    private static final String END = "14";

    public static void main(String[] args) {
        Graph g = new Graph(GRAPH);
        g.dijkstra(START);
        g.printPath(END);
        //g.printAllPaths();
    }
}

class Graph {
    private final Map<String, Vertex> graph; // mapping of vertex names to Vertex objects, built from a set of Edges

    /**
     * One edge of the graph (only used by Graph constructor)
     */
    public static class Edge {
        public final String v1, v2;
        public final int dist;

        public Edge(String v1, String v2, int dist) {
            this.v1 = v1.trim();
            this.v2 = v2.trim();
            this.dist = dist;
        }
    }

    /**
     * One vertex of the graph, complete with mappings to neighbouring vertices
     */
    public static class Vertex implements Comparable<Vertex> {
        public final String name;
        public int dist = Integer.MAX_VALUE; // MAX_VALUE assumed to be infinity
        public Vertex previous = null;
        public final Map<Vertex, Integer> neighbours = new HashMap<>();

        public Vertex(String name) {
            this.name = name;
        }

        private void printPath() {
            if (this == this.previous) {
                System.out.printf("%s", this.name);
            } else if (this.previous == null) {
                System.out.printf("%s(unreached)", this.name);
            } else {
                this.previous.printPath();
                System.out.printf(" -> %s(%d)", this.name, this.dist);
            }
        }

        public int compareTo(Vertex other) {
            return Integer.compare(dist, other.dist);
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Vertex vertex = (Vertex) o;

            return name != null ? name.equals(vertex.name) : vertex.name == null;

        }

        @Override
        public int hashCode() {
            return name != null ? name.hashCode() : 0;
        }
    }

    /**
     * Builds a graph from a set of edges
     */
    public Graph(Edge[] edges) {
        graph = new HashMap<>(edges.length);

        //one pass to find all vertices
        for (Edge e : edges) {
            if (!graph.containsKey(e.v1)) graph.put(e.v1, new Vertex(e.v1));
            if (!graph.containsKey(e.v2)) graph.put(e.v2, new Vertex(e.v2));
        }

        //another pass to set neighbouring vertices
        for (Edge e : edges) {
            graph.get(e.v1).neighbours.put(graph.get(e.v2), e.dist);
            //graph.get(e.v2).neighbours.put(graph.get(e.v1), e.dist); // also do this for an undirected graph
        }
    }

    /**
     * Runs dijkstra using a specified source vertex
     */
    public void dijkstra(String startName) {
        if (!graph.containsKey(startName)) {
            System.err.printf("Graph doesn't contain start vertex \"%s\"\n", startName);
            return;
        }
        final Vertex source = graph.get(startName);
        PriorityQueue<Vertex> q = new PriorityQueue<>();
        source.dist = 0;
        source.previous = source;
        q.add(source);

       

        dijkstra(q);
    }

    /**
     * Implementation of dijkstra's algorithm using a binary heap.
     */
    private void dijkstra(final PriorityQueue<Vertex> q) {
        Vertex u, v;
        while (!q.isEmpty()) {

            u = q.poll(); // vertex with shortest distance (first iteration will return source)
            if (u.dist == Integer.MAX_VALUE) break; // we can ignore u (and any other remaining vertices) since they are unreachable

            //look at distances to each neighbour
            for (Map.Entry<Vertex, Integer> a : u.neighbours.entrySet()) {
                v = a.getKey(); //the neighbour in this iteration

                final int alternateDist = u.dist + a.getValue();
                if (alternateDist < v.dist) { // shorter path to neighbour found
                    q.remove(v);
                    v.dist = alternateDist;
                    v.previous = u;
                    q.add(v);
                }
            }
        }
    }

    /**
     * Prints a path from the source to the specified vertex
     */
    public void printPath(String endName) {
        if (!graph.containsKey(endName)) {
            System.err.printf("Graph doesn't contain end vertex \"%s\"\n", endName);
            return;
        }

        graph.get(endName).printPath();
        System.out.println();
    }

    /**
     * Prints the path from the source to every vertex (output order is not guaranteed)
     */
    public void printAllPaths() {
        for (Vertex v : graph.values()) {
            v.printPath();
            System.out.println();
        }
    }
}

the code quality  is not the best but  the functionality is correct now

== Floyd–Warshall ==

The "[[wp:Floyd–Warshall_algorithm|Floyd–Warshall algorithm]]" is also interesting, from a graph comparison point of view, since it finds all shortest paths in a graph in O(|V|^3) time, where the most highly optimized version of Dijkstra's algorithm requires O(|E| + |V| log |V|) for a path and there are O(|V|^2) potential paths. --[[User:Rdm|Rdm]] 20:47, 9 December 2011 (UTC)

:Feel free to start a task for it. Many of these algorithms have been listed on [[Rosetta Code:Village Pump/Suggest a programming task]] but nobody has taken the initiative to start them. --[[User:Spoon!|Spoon!]] 22:47, 9 December 2011 (UTC)

== Example size and heaps ==

I posted a couple of ideas.  The WP data seems an obvious choice for the basic task.  I experimented some with large graphs.  My first implementation wasn't heap based and I was surprised that it could handle graphs of a few thousand vertices.  I was also surprised that my heap based version only ran about 12 times faster for a graph of this size (21K nodes.)

Given how well the O(n^2) algorithm works, it should be fine for the basic task and I don't think we should fuss that it's inefficient.  The heap enhancement makes a nice requirement for extra credit.  To show it off seems to require a really big graph though!  This thing with unixdict.txt is what I came up with for something that is repeatable.  &mdash;[[User:Sonia|Sonia]] 00:30, 10 December 2011 (UTC)
: <s>21k nodes, but how many edges?</s> NVM, only 240k edges, which is low (a normal graph expects edge count to be somewhat comparable to V<sup>2</sup>, your graph is only about 1/2000 of that, which is to say, instead of a web, that graphis is more like trees: very few loops (is it guaranteed to be connected?) --[[User:Ledrug|Ledrug]] 00:40, 10 December 2011 (UTC)
: Some possibly relavent points: 1) The Go code spent most of its time matching strings to find overlaps, that's O(n^2) with n = 20k ish;   2) the edge count is relatively low, and the code stops after reaching the target node (instead of finding the distances to all nodes).  It is a valid use of the algorithm, but since the edge count isn't high and there are some peculiarities in the way distances are computed (nodes of long words tend to have longer distances to everything else, and low edge count makes the graph more tree like which sort of forces the search to follow a narrow path), the program didn't have to search too much to finish.  Of 20k ish nodes, it only need to navigate through 2820.  3) minor: the tree doesn't seem to be fully connected.  There may be a small number of nodes not reachable from the starting point.
: The Go code is fine for what it's doing, but it's best not to draw conclusions about the performance based only on the current test case. --[[User:Ledrug|Ledrug]] 06:55, 10 December 2011 (UTC)
:: All very good points.  Constructing the edge list took 133ms, constructing the linked representation took 225ms, and then the path search took only 7ms.  It does seem likely that the graph wouldn't be fully connected.  Spoon, thanks for the simplifications to the Go code! &mdash;[[User:Sonia|Sonia]] 00:20, 11 December 2011 (UTC)

== Bug in C implementation ==
The line:

    int i = h->index[v] || ++h->len;

Does not evaluate correctly the expression. If h->index[v] == 0 then ++h->len is not assigned to i, instead 1 is assigned. In theory the compiler should assign the right side expression to i but is not working with gcc. For example, the graph:

    add_edge(g, 'a', 'b', 1);
    add_edge(g, 'b', 'c', 1);
    add_edge(g, 'c', 'd', 1);
    add_edge(g, 'd', 'e', 1);
    add_edge(g, 'e', 'f', 1);
    add_edge(g, 'f', 'a', 1);
    add_edge(g, 'a', 'g', 2);
    add_edge(g, 'g', 'b', 2);
    add_edge(g, 'c', 'g', 2);
    add_edge(g, 'g', 'd', 2);
    add_edge(g, 'e', 'g', 2);
    add_edge(g, 'f', 'g', 2);
    dijkstra(g, 'a', 'e');
    print_path(g, 'e');

Will output '5 agde' and is wrong because path '4 abcde' is shorter. Next line fixes the problem:

   int i = h->index[v] ? h->index[v] : ++h->len;

== Directed vs undirected graphs ==

Dijkstra's algorithm can work on directed or undirected graphs. (An undirected graph can be converted into an equivalent directed graph, where for each edge in the original graph, you have two equal-weighted edges in opposite directions.) The example that the task asks the user to solve does not directly mention that it is an undirected graph; however, the solutions assume that it is an undirected graph, which is okay assuming that that was what was intended I guess. However, to be general, each solution's main algorithm implementing function should still be able to accept an adjacency matrix that is not an undirected graph. --[[User:Spoon!|Spoon!]] 08:43, 26 January 2012 (UTC)

:Excellent point and something I failed to appreciate.  I just looked at the picture and didn't read carefully. &mdash;[[User:Sonia|Sonia]] 18:40, 27 January 2012 (UTC)

:I just changed the task to specify a directed graph, and keeping the same example data, this changes the solution from acfe to acde, thus invalidating existing solutions.  Sure the task could be written to keep acfe the correct solution, but I thought directedness was important enough to point out with a new solution. &mdash;[[User:Sonia|Sonia]] 21:25, 6 February 2012 (UTC)

== Single path / Path Tree ==
WP mentions the algorithm can produce a path tree, but discussions and examples tend to talk about single paths.  I think it's fine for the task to ask for a single path solution, but now while it's in draft is the time for people to speak up for path trees if they like.  Interestingly, Dijkstra's '59 paper doesn't mention path trees, but only the single path problem.  (Problem 1 in the paper is a minimal spaning tree, which is something different.) &mdash;[[User:Sonia|Sonia]] 21:25, 6 February 2012 (UTC)

== Is the GO solution wrong? ==

[a c d e] length 26
... or what do I misunderstand?
 
Ahhhh see directed vs. undirected above.
Sorry --[[User:Walterpachl|Walterpachl]] 08:55, 21 December 2012 (UTC)

== Replaced 'pushleft' with 'appendleft' in the Python snippet ==

Must specify explicitly that the python snippet solution should be at least in python 2.7 due to the dictionary comprehension feature used. And also, must replace the 'pushleft' with 'appendleft'.

Updated 'queue' to 'deque' and re-updated 'pushleft' to 'appendleft' and tested running in python 2.7 and python 3.5 on 1/19/2017

== Javascript version ==
var roads = [
    {from: 0, to: 1, drivingTime: 5},
    {from: 0, to: 2, drivingTime: 10},
    {from: 1, to: 2, drivingTime: 10},
    {from: 1, to: 3, drivingTime: 2},
    {from: 2, to: 3, drivingTime: 2},
    {from: 2, to: 4, drivingTime: 5},
    {from: 3, to: 2, drivingTime: 2},
    {from: 3, to: 4, drivingTime: 10}
];

function navigate(roads, start, finish) {
  var vert = [],Q;
  var neighb = {}, dist = {},prev = {};
  
  for(var edge of roads){
    vert.indexOf(edge.from) === -1 && vert.push(edge.from);
    vert.indexOf(edge.to) === -1 && vert.push(edge.to);
    
    if(!neighb[edge.from]) neighb[edge.from] = [];
    neighb[edge.from].push({end : edge.to,cost : edge.drivingTime});
  }
  
  vert.forEach((val) => {dist[val] = Infinity;prev[val] = null});
  dist[start] = 0;
  
  Q = vert;
  while(Q.length > 0){
    var min = Infinity;
    var u;
    Q.forEach((val) => {
      if(dist[val] < min){
        min = dist[val];
        u = val;
      }
    });
    
    Q = Q.slice(0,Q.indexOf(u)).concat(Q.slice(Q.indexOf(u) + 1,Q.length));
    if(dist[u] == Infinity || u == finish) break;
    
    if(neighb[u]){
      neighb[u].forEach((val) => {
        var alt = dist[u] + val.cost;
        if(alt < dist[val.end]){
          dist[val.end] = alt;
          prev[val.end] = u;
        }
      });
    }
  }
  
  var path = [];
  u = finish;
  while(prev[u] !== undefined){
    path.unshift(u);
    u = prev[u];
  }
  return path.length > 0 ? path : null;
}

== Perl version faulty ==

The push_priority() function of the perl implementation is faulty.  This can most clearly be seen by adding an edge from 'b' to 'e' of weight 1 making s-b-e the shortest path.  Yet this new path is not picked up.  The vertices appears to be dequeued from the function in their inserted order, rather than by priority.  In the function the binary search comparison appears to be comparing hash addresses.

:Fixed. [[User:Trizen|Trizen]] ([[User talk:Trizen|talk]])
