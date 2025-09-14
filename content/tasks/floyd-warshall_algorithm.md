+++
title = "Floyd-Warshall algorithm"
description = ""
date = 2019-04-26T23:41:18Z
aliases = []
[extra]
id = 19642
[taxonomies]
categories = ["task", "Routing algorithms"]
tags = []
languages = [
  "360_assembly",
  "c",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "elixir",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "perl",
  "perl_6",
  "phix",
  "php",
  "prolog",
  "python",
  "racket",
  "rexx",
  "ruby",
  "sequencel",
  "sidef",
  "tcl",
  "visual_basic_.net",
  "zkl",
]
+++

The [[wp:Floyd–Warshall_algorithm|Floyd–Warshall algorithm]] is an algorithm for finding shortest paths in a weighted graph with positive or negative edge weights.



## Task

Find the lengths of the shortest paths between all pairs of vertices of the given directed graph. Your code may assume that the input has already been checked for loops, parallel edges and negative cycles.

[[File:Floyd_warshall_graph.gif|||center]]

Print the pair, the distance and (optionally) the path.



;Example

```txt
pair     dist    path
1 -> 2    -1     1 -> 3 -> 4 -> 2
1 -> 3    -2     1 -> 3
1 -> 4     0     1 -> 3 -> 4
2 -> 1     4     2 -> 1
2 -> 3     2     2 -> 1 -> 3
2 -> 4     4     2 -> 1 -> 3 -> 4
3 -> 1     5     3 -> 4 -> 2 -> 1
3 -> 2     1     3 -> 4 -> 2
3 -> 4     2     3 -> 4
4 -> 1     3     4 -> 2 -> 1
4 -> 2    -1     4 -> 2
4 -> 3     1     4 -> 2 -> 1 -> 3
```




## See also

* [https://www.youtube.com/watch?v=8WSZQwNtXPU Floyd-Warshall Algorithm - step by step guide (youtube)]




## 360 Assembly

```360asm
*        Floyd-Warshall algorithm - 06/06/2018
FLOYDWAR CSECT
         USING  FLOYDWAR,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         MVC    A+8,=F'-2'         a(1,3)=-2
         MVC    A+VV*4,=F'4'       a(2,1)= 4
         MVC    A+VV*4+8,=F'3'     a(2,3)= 3
         MVC    A+VV*8+12,=F'2'    a(3,4)= 2
         MVC    A+VV*12+4,=F'-1'   a(4,2)=-1
         LA     R8,1               k=1
       DO WHILE=(C,R8,LE,V)        do k=1 to v
         LA     R10,A                @a
         LA     R6,1                 i=1
       DO WHILE=(C,R6,LE,V)          do i=1 to v
         LA     R7,1                   j=1
       DO WHILE=(C,R7,LE,V)            do j=1 to v
         LR     R1,R6                    i
         BCTR   R1,0
         MH     R1,=AL2(VV)
         AR     R1,R8                    k
         SLA    R1,2
         L      R9,A-4(R1)               a(i,k)
         LR     R1,R8                    k
         BCTR   R1,0
         MH     R1,=AL2(VV)
         AR     R1,R7                    j
         SLA    R1,2
         L      R3,A-4(R1)               a(k,j)
         AR     R9,R3                    w=a(i,k)+a(k,j)
         L      R2,0(R10)                a(i,j)
       IF CR,R2,GT,R9 THEN               if a(i,j)>w then
         ST     R9,0(R10)                  a(i,j)=w
       ENDIF    ,                        endif
         LA     R10,4(R10)               next @a
         LA     R7,1(R7)                 j++
       ENDDO    ,                      enddo j
         LA     R6,1(R6)               i++
       ENDDO    ,                    enddo i
         LA     R8,1(R8)             k++
       ENDDO    ,                  enddo k
         LA     R10,A              @a
         LA     R6,1               f=1
       DO WHILE=(C,R6,LE,V)        do f=1 to v
         LA     R7,1                 t=1
       DO WHILE=(C,R7,LE,V)          do t=1 to v
       IF CR,R6,NE,R7 THEN             if f^=t then do
         LR     R1,R6                    f
         XDECO  R1,XDEC                  edit f
         MVC    PG+0(4),XDEC+8           output f
         LR     R1,R7                    t
         XDECO  R1,XDEC                  edit t
         MVC    PG+8(4),XDEC+8           output t
         L      R2,0(R10)                a(f,t)
         XDECO  R2,XDEC                  edit a(f,t)
         MVC    PG+12(4),XDEC+8          output a(f,t)
         XPRNT  PG,L'PG                  print
       ENDIF    ,                      endif
         LA     R10,4(R10)             next @a
         LA     R7,1(R7)               t++
       ENDDO    ,                    enddo t
         LA     R6,1(R6)             f++
       ENDDO    ,                  enddo f
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
VV       EQU    4
V        DC     A(VV)
A        DC     (VV*VV)F'99999999' a(vv,vv)
PG       DC     CL80'   . ->    .   .'
XDEC     DS     CL12
         YREGS
         END    FLOYDWAR
```

```txt

   1 ->    2  -1
   1 ->    3  -2
   1 ->    4   0
   2 ->    1   4
   2 ->    3   2
   2 ->    4   4
   3 ->    1   5
   3 ->    2   1
   3 ->    4   2
   4 ->    1   3
   4 ->    2  -1
   4 ->    3   1

```



## C

Reads the graph from a file, prints out usage on incorrect invocation.

```C

#include<limits.h>
#include<stdlib.h>
#include<stdio.h>

typedef struct{
	int sourceVertex, destVertex;
	int edgeWeight;
}edge;

typedef struct{
	int vertices, edges;
	edge* edgeMatrix;
}graph;

graph loadGraph(char* fileName){
	FILE* fp = fopen(fileName,"r");

	graph G;
	int i;

	fscanf(fp,"%d%d",&G.vertices,&G.edges);

	G.edgeMatrix = (edge*)malloc(G.edges*sizeof(edge));

	for(i=0;i<G.edges;i++)
		fscanf(fp,"%d%d%d",&G.edgeMatrix[i].sourceVertex,&G.edgeMatrix[i].destVertex,&G.edgeMatrix[i].edgeWeight);

	fclose(fp);

	return G;
}

void floydWarshall(graph g){
	int processWeights[g.vertices][g.vertices], processedVertices[g.vertices][g.vertices];
	int i,j,k;

	for(i=0;i<g.vertices;i++)
		for(j=0;j<g.vertices;j++){
			processWeights[i][j] = SHRT_MAX;
			processedVertices[i][j] = (i!=j)?j+1:0;
		}

	for(i=0;i<g.edges;i++)
		processWeights[g.edgeMatrix[i].sourceVertex-1][g.edgeMatrix[i].destVertex-1] = g.edgeMatrix[i].edgeWeight;

	for(i=0;i<g.vertices;i++)
		for(j=0;j<g.vertices;j++)
			for(k=0;k<g.vertices;k++){
				if(processWeights[j][i] + processWeights[i][k] < processWeights[j][k]){
					processWeights[j][k] = processWeights[j][i] + processWeights[i][k];
					processedVertices[j][k] = processedVertices[j][i];
				}
			}

	printf("pair    dist   path");
	for(i=0;i<g.vertices;i++)
		for(j=0;j<g.vertices;j++){
			if(i!=j){
				printf("\n%d -> %d %3d %5d",i+1,j+1,processWeights[i][j],i+1);
				k = i+1;
				do{
					k = processedVertices[k-1][j];
					printf("->%d",k);
				}while(k!=j+1);
			}
		}
}

int main(int argC,char* argV[]){
	if(argC!=2)
		printf("Usage : %s <file containing graph data>");
	else
		floydWarshall(loadGraph(argV[1]));
	return 0;
}

```

Input file, first row specifies number of vertices and edges.

```txt

4 5
1 3 -2
3 4 2
4 2 -1
2 1 4
2 3 3

```

Invocation and output:

```txt

C:\rosettaCode>fwGraph.exe fwGraph.txt
pair    dist   path
1 -> 2  -1     1->3->4->2
1 -> 3  -2     1->3
1 -> 4   0     1->3->4
2 -> 1   4     2->1
2 -> 3   2     2->1->3
2 -> 4   4     2->1->3->4
3 -> 1   5     3->4->2->1
3 -> 2   1     3->4->2
3 -> 4   2     3->4
4 -> 1   3     4->2->1
4 -> 2  -1     4->2
4 -> 3   1     4->2->1->3

```



## C++


```cpp
#include <iostream>
#include <vector>
#include <sstream>

void print(std::vector<std::vector<double>> dist, std::vector<std::vector<int>> next) {
  std::cout << "(pair, dist, path)" << std::endl;
  const auto size = std::size(next);
  for (auto i = 0; i < size; ++i) {
    for (auto j = 0; j < size; ++j) {
      if (i != j) {
        auto u = i + 1;
        auto v = j + 1;
        std::cout << "(" << u << " -> " << v << ", " << dist[i][j]
          << ", ";
        std::stringstream path;
        path << u;
        do {
          u = next[u - 1][v - 1];
          path << " -> " << u;
        } while (u != v);
        std::cout << path.str() << ")" << std::endl;
      }
    }
  }
}

void solve(std::vector<std::vector<int>> w_s, const int num_vertices) {
  std::vector<std::vector<double>> dist(num_vertices);
  for (auto& dim : dist) {
    for (auto i = 0; i < num_vertices; ++i) {
      dim.push_back(INT_MAX);
    }
  }
  for (auto& w : w_s) {
    dist[w[0] - 1][w[1] - 1] = w[2];
  }
  std::vector<std::vector<int>> next(num_vertices);
  for (auto i = 0; i < num_vertices; ++i) {
    for (auto j = 0; j < num_vertices; ++j) {
      next[i].push_back(0);
    }
    for (auto j = 0; j < num_vertices; ++j) {
      if (i != j) {
        next[i][j] = j + 1;
      }
    }
  }
  for (auto k = 0; k < num_vertices; ++k) {
    for (auto i = 0; i < num_vertices; ++i) {
      for (auto j = 0; j < num_vertices; ++j) {
        if (dist[i][j] > dist[i][k] + dist[k][j]) {
          dist[i][j] = dist[i][k] + dist[k][j];
          next[i][j] = next[i][k];
        }
      }
    }
  }
  print(dist, next);
}

int main() {
  std::vector<std::vector<int>> w = {
    { 1, 3, -2 },
    { 2, 1, 4 },
    { 2, 3, 3 },
    { 3, 4, 2 },
    { 4, 2, -1 },
  };
  int num_vertices = 4;
  solve(w, num_vertices);
  std::cin.ignore();
  std::cin.get();
  return 0;
}
```


```txt
(pair, dist, path)
(1 -> 2, -1, 1 -> 3 -> 4 -> 2)
(1 -> 3, -2, 1 -> 3)
(1 -> 4, 0, 1 -> 3 -> 4)
(2 -> 1, 4, 2 -> 1)
(2 -> 3, 2, 2 -> 1 -> 3)
(2 -> 4, 4, 2 -> 1 -> 3 -> 4)
(3 -> 1, 5, 3 -> 4 -> 2 -> 1)
(3 -> 2, 1, 3 -> 4 -> 2)
(3 -> 4, 2, 3 -> 4)
(4 -> 1, 3, 4 -> 2 -> 1)
(4 -> 2, -1, 4 -> 2)
(4 -> 3, 1, 4 -> 2 -> 1 -> 3)
```


## C#
```c#
using System;

namespace FloydWarshallAlgorithm {
    class Program {
        static void FloydWarshall(int[,] weights, int numVerticies) {
            double[,] dist = new double[numVerticies, numVerticies];
            for (int i = 0; i < numVerticies; i++) {
                for (int j = 0; j < numVerticies; j++) {
                    dist[i, j] = double.PositiveInfinity;
                }
            }

            for (int i = 0; i < weights.GetLength(0); i++) {
                dist[weights[i, 0] - 1, weights[i, 1] - 1] = weights[i, 2];
            }

            int[,] next = new int[numVerticies, numVerticies];
            for (int i = 0; i < numVerticies; i++) {
                for (int j = 0; j < numVerticies; j++) {
                    if (i != j) {
                        next[i, j] = j + 1;
                    }
                }
            }

            for (int k = 0; k < numVerticies; k++) {
                for (int i = 0; i < numVerticies; i++) {
                    for (int j = 0; j < numVerticies; j++) {
                        if (dist[i, k] + dist[k, j] < dist[i, j]) {
                            dist[i, j] = dist[i, k] + dist[k, j];
                            next[i, j] = next[i, k];
                        }
                    }
                }
            }

            PrintResult(dist, next);
        }

        static void PrintResult(double[,] dist, int[,] next) {
            Console.WriteLine("pair     dist    path");
            for (int i = 0; i < next.GetLength(0); i++) {
                for (int j = 0; j < next.GetLength(1); j++) {
                    if (i != j) {
                        int u = i + 1;
                        int v = j + 1;
                        string path = string.Format("{0} -> {1}    {2,2:G}     {3}", u, v, dist[i, j], u);
                        do {
                            u = next[u - 1, v - 1];
                            path += " -> " + u;
                        } while (u != v);
                        Console.WriteLine(path);
                    }
                }
            }
        }

        static void Main(string[] args) {
            int[,] weights = { { 1, 3, -2 }, { 2, 1, 4 }, { 2, 3, 3 }, { 3, 4, 2 }, { 4, 2, -1 } };
            int numVerticies = 4;

            FloydWarshall(weights, numVerticies);
        }
    }
}
```



## D

```D
import std.stdio;

void main() {
    int[][] weights = [
        [1, 3, -2],
        [2, 1, 4],
        [2, 3, 3],
        [3, 4, 2],
        [4, 2, -1]
    ];
    int numVertices = 4;

    floydWarshall(weights, numVertices);
}

void floydWarshall(int[][] weights, int numVertices) {
    import std.array;

    real[][] dist = uninitializedArray!(real[][])(numVertices, numVertices);
    foreach(dim; dist) {
        dim[] = real.infinity;
    }

    foreach (w; weights) {
        dist[w[0]-1][w[1]-1] = w[2];
    }

    int[][] next = uninitializedArray!(int[][])(numVertices, numVertices);
    for (int i=0; i<next.length; i++) {
        for (int j=0; j<next.length; j++) {
            if (i != j) {
                next[i][j] = j+1;
            }
        }
    }

    for (int k=0; k<numVertices; k++) {
        for (int i=0; i<numVertices; i++) {
            for (int j=0; j<numVertices; j++) {
                if (dist[i][j] > dist[i][k] + dist[k][j]) {
                    dist[i][j] = dist[i][k] + dist[k][j];
                    next[i][j] = next[i][k];
                }
            }
        }
    }

    printResult(dist, next);
}

void printResult(real[][] dist, int[][] next) {
    import std.conv;
    import std.format;

    writeln("pair     dist    path");
    for (int i=0; i<next.length; i++) {
        for (int j=0; j<next.length; j++) {
            if (i!=j) {
                int u = i+1;
                int v = j+1;
                string path = format("%d -> %d    %2d     %s", u, v, cast(int) dist[i][j], u);
                do {
                    u = next[u-1][v-1];
                    path ~= text(" -> ", u);
                } while (u != v);
                writeln(path);
            }
        }
    }
}
```


```txt
pair     dist    path
1 -> 2    -1     1 -> 3 -> 4 -> 2
1 -> 3    -2     1 -> 3
1 -> 4     0     1 -> 3 -> 4
2 -> 1     4     2 -> 1
2 -> 3     2     2 -> 1 -> 3
2 -> 4     4     2 -> 1 -> 3 -> 4
3 -> 1     5     3 -> 4 -> 2 -> 1
3 -> 2     1     3 -> 4 -> 2
3 -> 4     2     3 -> 4
4 -> 1     3     4 -> 2 -> 1
4 -> 2    -1     4 -> 2
4 -> 3     1     4 -> 2 -> 1 -> 3
```



## EchoLisp

Transcription of the Floyd-Warshall algorithm, with best path computation.

```scheme

(lib 'matrix)

;; in : initialized dist and next matrices
;; out : dist and next matrices
;; O(n^3)

(define (floyd-with-path n dist next (d 0))
 	(for* ((k n) (i n) (j n))
	 #:break (< (array-ref dist j j) 0) => 'negative-cycle
 	(set! d (+ (array-ref dist i k) (array-ref dist k j)))
	 (when (< d (array-ref dist i j))
		 (array-set! dist i j d)
		 (array-set! next i j (array-ref next i k)))))

;; utilities

;; init random edges costs, matrix 66% filled
(define (init-edges n dist next)
   (for* ((i n) (j n))
	(array-set! dist i i 0)
   	(array-set! next i j null)
 	#:continue (= j i)
 	(array-set! dist i j Infinity)
	 #:continue (< (random) 0.3)
	 (array-set! dist i j (1+ (random 100)))
 	(array-set! next i j j)))

;; show path from u to v
(define (path u v)
	(cond
	 ((= u v) (list u))
	 ((null? (array-ref next u v)) null)
 	 (else (cons u (path (array-ref next u v) v)))))

(define( mdist u v) ;; show computed distance
	  (array-ref dist u v))

(define (task)
	 (init-edges n dist next)
	 (array-print dist) ;; show init distances
	 (floyd-with-path n dist next))

```

```txt

(define n 8)
(define next (make-array n n))
(define dist (make-array n n))
(task)

  0    Infinity   Infinity   13         98         Infinity   35         47
  8    0          Infinity   Infinity   83         77         16         3
  73   3          0          3          76         84         91         Infinity
  30   49         Infinity   0          41         Infinity   4          4
  22   83         92         Infinity   0          30         27         98
  6    Infinity   Infinity   24         59         0          Infinity   Infinity
  60   Infinity   45         Infinity   67         100        0          Infinity
  72   15         95         21         Infinity   Infinity   27         0


(array-print dist) ;; computed distances

  0    32   62   13   54   84   17   17
  8    0    61   21   62   77   16   3
  11   3    0    3    44   74   7    6
  27   19   49   0    41   71   4    4
  22   54   72   35   0    30   27   39
  6    38   68   19   59   0    23   23
  56   48   45   48   67   97   0    51
  23   15   70   21   62   92   25   0

(path 1 3)  → (1 0 3)
(mdist 1 0) → 8
(mdist 0 3) → 13
(mdist 1 3) → 21 ;; = 8 + 13
(path 7 6) → (7 3 6)
(path 6 7) → (6 2 1 7)


```



## Elixir


```elixir
defmodule Floyd_Warshall do
  def main(n, edge) do
    {dist, next} = setup(n, edge)
    {dist, next} = shortest_path(n, dist, next)
    print(n, dist, next)
  end

  defp setup(n, edge) do
    big = 1.0e300
    dist = for i <- 1..n, j <- 1..n, into: %{}, do: {{i,j},(if i==j, do: 0, else: big)}
    next = for i <- 1..n, j <- 1..n, into: %{}, do: {{i,j}, nil}
    Enum.reduce(edge, {dist,next}, fn {u,v,w},{dst,nxt} ->
      { Map.put(dst, {u,v}, w), Map.put(nxt, {u,v}, v) }
    end)
  end

  defp shortest_path(n, dist, next) do
    (for k <- 1..n, i <- 1..n, j <- 1..n, do: {k,i,j})
    |> Enum.reduce({dist,next}, fn {k,i,j},{dst,nxt} ->
         if dst[{i,j}] > dst[{i,k}] + dst[{k,j}] do
           {Map.put(dst, {i,j}, dst[{i,k}] + dst[{k,j}]), Map.put(nxt, {i,j}, nxt[{i,k}])}
         else
           {dst, nxt}
         end
       end)
  end

  defp print(n, dist, next) do
    IO.puts "pair     dist    path"
    for i <- 1..n, j <- 1..n, i != j,
        do: :io.format "~w -> ~w  ~4w     ~s~n", [i, j, dist[{i,j}], path(next, i, j)]
  end

  defp path(next, i, j), do: path(next, i, j, [i]) |> Enum.join(" -> ")

  defp path(_next, i, i, list), do: Enum.reverse(list)
  defp path(next, i, j, list) do
    u = next[{i,j}]
    path(next, u, j, [u | list])
  end
end

edge = [{1, 3, -2}, {2, 1, 4}, {2, 3, 3}, {3, 4, 2}, {4, 2, -1}]
Floyd_Warshall.main(4, edge)
```


```txt

pair     dist    path
1 -> 2    -1     1 -> 3 -> 4 -> 2
1 -> 3    -2     1 -> 3
1 -> 4     0     1 -> 3 -> 4
2 -> 1     4     2 -> 1
2 -> 3     2     2 -> 1 -> 3
2 -> 4     4     2 -> 1 -> 3 -> 4
3 -> 1     5     3 -> 4 -> 2 -> 1
3 -> 2     1     3 -> 4 -> 2
3 -> 4     2     3 -> 4
4 -> 1     3     4 -> 2 -> 1
4 -> 2    -1     4 -> 2
4 -> 3     1     4 -> 2 -> 1 -> 3

```



## FreeBASIC

```freebasic
' FB 1.05.0 Win64

Const POSITIVE_INFINITY As Double = 1.0/0.0

Sub printResult(dist(any, any) As Double, nxt(any, any) As Integer)
  Dim As Integer u, v
  Print("pair     dist    path")
  For i As Integer = 0 To UBound(nxt, 1)
    For j As Integer = 0 To UBound(nxt, 1)
      If i <> j Then
        u = i + 1
        v = j + 1
        Print Str(u); " -> "; Str(v); "    "; dist(i, j); "     "; Str(u);
        Do
          u = nxt(u - 1, v - 1)
          Print " -> "; Str(u);
        Loop While u <> v
        Print
      End If
    Next j
  Next i
End Sub

Sub floydWarshall(weights(Any, Any) As Integer, numVertices As Integer)
  Dim dist(0 To numVertices - 1, 0 To numVertices - 1) As Double
  For i As Integer = 0 To numVertices - 1
    For j As Integer = 0 To numVertices - 1
      dist(i, j) = POSITIVE_INFINITY
    Next j
  Next i

  For x As Integer = 0 To UBound(weights, 1)
    dist(weights(x, 0) - 1, weights(x, 1) - 1) = weights(x, 2)
  Next x

  Dim nxt(0 To numVertices - 1, 0 To numVertices - 1) As Integer
  For i As Integer = 0 To numVertices - 1
    For j As Integer = 0 To numVertices - 1
      If i <> j Then nxt(i, j) = j + 1
    Next j
  Next i

  For k As Integer = 0 To numVertices - 1
    For i As Integer = 0 To numVertices - 1
      For j As Integer = 0 To numVertices - 1
        If (dist(i, k) + dist(k, j)) < dist(i, j) Then
          dist(i, j) = dist(i, k) + dist(k, j)
          nxt(i, j) = nxt(i, k)
        End If
      Next j
    Next i
  Next k

  printResult(dist(), nxt())
End Sub

Dim weights(4, 2) As Integer = {{1, 3, -2}, {2, 1, 4}, {2, 3, 3}, {3, 4, 2}, {4, 2, -1}}
Dim numVertices As Integer = 4
floydWarshall(weights(), numVertices)
Print
Print "Press any key to quit"
Sleep
```


```txt

pair     dist    path
1 -> 2    -1     1 -> 3 -> 4 -> 2
1 -> 3    -2     1 -> 3
1 -> 4     0     1 -> 3 -> 4
2 -> 1     4     2 -> 1
2 -> 3     2     2 -> 1 -> 3
2 -> 4     4     2 -> 1 -> 3 -> 4
3 -> 1     5     3 -> 4 -> 2 -> 1
3 -> 2     1     3 -> 4 -> 2
3 -> 4     2     3 -> 4
4 -> 1     3     4 -> 2 -> 1
4 -> 2    -1     4 -> 2
4 -> 3     1     4 -> 2 -> 1 -> 3

```


=={{header|F_Sharp|F#}}==
===Floyd's algorithm===

```fsharp

//Floyd's algorithm: Nigel Galloway August 5th 2018
let Floyd (n:'a[]) (g:Map<('a*'a),int>)= //nodes graph(Map of adjacency list)
  let ix n g=Seq.init (pown g n) (fun x->List.unfold(fun (a,b)->if a=0 then None else Some(b%g,(a-1,b/g)))(n,x))
  let fN w (i,j,k)=match Map.tryFind(i,j) w,Map.tryFind(i,k) w,Map.tryFind(k,j) w with
                        |(None  ,Some j,Some k)->Some(j+k)
                        |(Some i,Some j,Some k)->if (j+k) < i then Some(j+k) else None
                        |_                     ->None
  let n,z=ix 3 (Array.length n)|>Seq.choose(fun (i::j::k::_)->if i<>j&&i<>k&&j<>k then Some(n.[i],n.[j],n.[k]) else None)
       |>Seq.fold(fun (n,n') ((i,j,k) as g)->match fN n g with |Some g->(Map.add (i,j) g n,Map.add (i,j) k n')|_->(n,n')) (g,Map.empty)
  (n,(fun x y->seq{
               let rec fN n g=seq{
                 match Map.tryFind (n,g) z with
                 |Some r->yield! fN n r; yield Some r;yield! fN r g
                 |_->yield None}
               yield! fN x y |> Seq.choose id; yield y}))

```



### The Task


```fsharp

let fW=Map[((1,3),-2);((3,4),2);((4,2),-1);((2,1),4);((2,3),3)]
let N,G=Floyd [|1..4|] fW
List.allPairs [1..4] [1..4]|>List.filter(fun (n,g)->n<>g)|>List.iter(fun (n,g)->printfn "%d->%d %d %A" n g N.[(n,g)] (n::(List.ofSeq (G n g))))

```

```txt

1->2 -1 [1; 3; 4; 2]
1->3 -2 [1; 3]
1->4 0 [1; 3; 4]
2->1 4 [2; 1]
2->3 2 [2; 1; 3]
2->4 4 [2; 1; 3; 4]
3->1 5 [3; 4; 2; 1]
3->2 1 [3; 4; 2]
3->4 2 [3; 4]
4->1 3 [4; 2; 1]
4->2 -1 [4; 2]
4->3 1 [4; 2; 1; 3]

```



## Go


```go
package main

import (
  "fmt"
  "strconv"
)

// A Graph is the interface implemented by graphs that
// this algorithm can run on.
type Graph interface {
  Vertices() []Vertex
  Neighbors(v Vertex) []Vertex
  Weight(u, v Vertex) int
}

// Nonnegative integer ID of vertex
type Vertex int

// ig is a graph of integers that satisfies the Graph interface.
type ig struct {
  vert  []Vertex
  edges map[Vertex]map[Vertex]int
}

func (g ig) edge(u, v Vertex, w int) {
  if _, ok := g.edges[u]; !ok {
    g.edges[u] = make(map[Vertex]int)
  }
  g.edges[u][v] = w
}
func (g ig) Vertices() []Vertex { return g.vert }
func (g ig) Neighbors(v Vertex) (vs []Vertex) {
  for k := range g.edges[v] {
    vs = append(vs, k)
  }
  return vs
}
func (g ig) Weight(u, v Vertex) int { return g.edges[u][v] }
func (g ig) path(vv []Vertex) (s string) {
  if len(vv) == 0 {
    return ""
  }
  s = strconv.Itoa(int(vv[0]))
  for _, v := range vv[1:] {
    s += " -> " + strconv.Itoa(int(v))
  }
  return s
}

const Infinity = int(^uint(0) >> 1)

func FloydWarshall(g Graph) (dist map[Vertex]map[Vertex]int, next map[Vertex]map[Vertex]*Vertex) {
  vert := g.Vertices()
  dist = make(map[Vertex]map[Vertex]int)
  next = make(map[Vertex]map[Vertex]*Vertex)
  for _, u := range vert {
    dist[u] = make(map[Vertex]int)
    next[u] = make(map[Vertex]*Vertex)
    for _, v := range vert {
      dist[u][v] = Infinity
    }
    dist[u][u] = 0
    for _, v := range g.Neighbors(u) {
      v := v
      dist[u][v] = g.Weight(u, v)
      next[u][v] = &v
    }
  }
  for _, k := range vert {
    for _, i := range vert {
      for _, j := range vert {
        if dist[i][k] < Infinity && dist[k][j] < Infinity {
          if dist[i][j] > dist[i][k]+dist[k][j] {
            dist[i][j] = dist[i][k] + dist[k][j]
            next[i][j] = next[i][k]
          }
        }
      }
    }
  }
  return dist, next
}

func Path(u, v Vertex, next map[Vertex]map[Vertex]*Vertex) (path []Vertex) {
  if next[u][v] == nil {
    return
  }
  path = []Vertex{u}
  for u != v {
    u = *next[u][v]
    path = append(path, u)
  }
  return path
}

func main() {
  g := ig{[]Vertex{1, 2, 3, 4}, make(map[Vertex]map[Vertex]int)}
  g.edge(1, 3, -2)
  g.edge(3, 4, 2)
  g.edge(4, 2, -1)
  g.edge(2, 1, 4)
  g.edge(2, 3, 3)

  dist, next := FloydWarshall(g)
  fmt.Println("pair\tdist\tpath")
  for u, m := range dist {
    for v, d := range m {
      if u != v {
        fmt.Printf("%d -> %d\t%3d\t%s\n", u, v, d, g.path(Path(u, v, next)))
      }
    }
  }
}
```

```txt

pair	dist	path
1 -> 2	 -1	1 -> 3 -> 4 -> 2
1 -> 3	 -2	1 -> 3
1 -> 4	  0	1 -> 3 -> 4
2 -> 1	  4	2 -> 1
2 -> 3	  2	2 -> 1 -> 3
2 -> 4	  4	2 -> 1 -> 3 -> 4
3 -> 1	  5	3 -> 4 -> 2 -> 1
3 -> 2	  1	3 -> 4 -> 2
3 -> 4	  2	3 -> 4
4 -> 1	  3	4 -> 2 -> 1
4 -> 2	 -1	4 -> 2
4 -> 3	  1	4 -> 2 -> 1 -> 3

```



## Haskell


Necessary imports

```haskell
import Control.Monad (join)
import Data.List (union)
import Data.Map hiding (foldr, union)
import Data.Maybe (fromJust, isJust)
import Data.Semigroup
import Prelude hiding (lookup, filter)
```


First we define a general datatype to represent the shortest path. Type <code>a</code> represents a distance. It could be a number, in case of weighted graph or boolean value for just a directed graph. Type <code>b</code> goes for vertice labels (integers, chars, strings...)


```haskell
data Shortest b a = Shortest { distance :: a, path :: [b] }
                  deriving Show
```


Next we note that shortest paths form a semigroup with following "addition" rule:


```haskell
instance (Ord a, Eq b) => Semigroup (Shortest b a) where
  a <> b = case distance a `compare` distance b of
    GT -> b
    LT -> a
    EQ -> a { path = path a `union` path b }
```


It finds minimal path by <code>distance</code>, and in case of equal distances joins both paths. We will lift this semigroup to monoid using <code>Maybe</code> wrapper.

Graph is represented as a <code>Map</code>, containing pairs of vertices and corresponding weigts. The distance table is a <code>Map</code>, containing pairs of joint vertices and corresponding shortest paths.

Now we are ready to define the main part of the Floyd-Warshall algorithm, which processes properly prepared distance table <code>dist</code> for given list of vertices <code>v</code>:

```haskell
floydWarshall v dist = foldr innerCycle (Just <$> dist) v
  where
    innerCycle k dist = (newDist <$> v <*> v) `setTo` dist
      where
        newDist i j =
          ((i,j), do a <- join $ lookup (i, k) dist
                     b <- join $ lookup (k, j) dist
                     return $ Shortest (distance a <> distance b) (path a))

        setTo = unionWith (<>) . fromList
```


The <code>floydWarshall</code> produces only first steps of shortest paths. Whole paths are build by following function:


```haskell
buildPaths d = mapWithKey (\pair s -> s { path = buildPath pair}) d
  where
    buildPath (i,j)
      | i == j    = [[j]]
      | otherwise = do k <- path $ fromJust $ lookup (i,j) d
                       p <- buildPath (k,j)
                       [i : p]
```


All pre- and postprocessing is done by the main function <code>findMinDistances</code>:

```haskell
findMinDistances v g =
  let weights = mapWithKey (\(_,j) w -> Shortest w [j]) g
      trivial = fromList [ ((i,i), Shortest mempty []) | i <- v ]
      clean d = fromJust <$> filter isJust (d \\ trivial)
  in buildPaths $ clean $ floydWarshall v (weights <> trivial)
```


'''Examples''':

The sample graph:

```haskell
g = fromList [((2,1), 4)
             ,((2,3), 3)
             ,((1,3), -2)
             ,((3,4), 2)
             ,((4,2), -1)]
```

the helper function

```haskell
showShortestPaths v g = mapM_ print $ toList $ findMinDistances v g
```


Weights as distances:

```txt
λ> showShortestPaths [1..4] (Sum <$> g)
((1,2),Shortest {distance = Sum {getSum = -1}, path = [[1,3,4,2]]})
((1,3),Shortest {distance = Sum {getSum = -2}, path = [[1,3]]})
((1,4),Shortest {distance = Sum {getSum = 0}, path = [[1,3,4]]})
((2,1),Shortest {distance = Sum {getSum = 4}, path = [[2,1]]})
((2,3),Shortest {distance = Sum {getSum = 2}, path = [[2,1,3]]})
((2,4),Shortest {distance = Sum {getSum = 4}, path = [[2,1,3,4]]})
((3,1),Shortest {distance = Sum {getSum = 5}, path = [[3,4,2,1]]})
((3,2),Shortest {distance = Sum {getSum = 1}, path = [[3,4,2]]})
((3,4),Shortest {distance = Sum {getSum = 2}, path = [[3,4]]})
((4,1),Shortest {distance = Sum {getSum = 3}, path = [[4,2,1]]})
((4,2),Shortest {distance = Sum {getSum = -1}, path = [[4,2]]})
((4,3),Shortest {distance = Sum {getSum = 1}, path = [[4,2,1,3]]})
```


Unweighted directed graph

```txt
λ> showShortestPaths [1..4] (Any . (/= 0) <$> g)
((1,2),Shortest {distance = Any {getAny = True}, path = [[1,3,4,2]]})
((1,3),Shortest {distance = Any {getAny = True}, path = [[1,3]]})
((1,4),Shortest {distance = Any {getAny = True}, path = [[1,3,4]]})
((2,1),Shortest {distance = Any {getAny = True}, path = [[2,1]]})
((2,3),Shortest {distance = Any {getAny = True}, path = [[2,1,3],[2,3]]})
((2,4),Shortest {distance = Any {getAny = True}, path = [[2,1,3,4],[2,3,4]]})
((3,1),Shortest {distance = Any {getAny = True}, path = [[3,4,2,1]]})
((3,2),Shortest {distance = Any {getAny = True}, path = [[3,4,2]]})
((3,4),Shortest {distance = Any {getAny = True}, path = [[3,4]]})
((4,1),Shortest {distance = Any {getAny = True}, path = [[4,2,1]]})
((4,2),Shortest {distance = Any {getAny = True}, path = [[4,2]]})
((4,3),Shortest {distance = Any {getAny = True}, path = [[4,2,1,3],[4,2,3]]})
```

For some pairs several possible paths are found.

Uniformly weighted graph:

```txt
λ> showShortestPaths [1..4] (const (Sum 1) <$> g)
((1,2),Shortest {distance = Sum {getSum = 3}, path = [[1,3,4,2]]})
((1,3),Shortest {distance = Sum {getSum = 1}, path = [[1,3]]})
((1,4),Shortest {distance = Sum {getSum = 2}, path = [[1,3,4]]})
((2,1),Shortest {distance = Sum {getSum = 1}, path = [[2,1]]})
((2,3),Shortest {distance = Sum {getSum = 1}, path = [[2,3]]})
((2,4),Shortest {distance = Sum {getSum = 2}, path = [[2,3,4]]})
((3,1),Shortest {distance = Sum {getSum = 3}, path = [[3,4,2,1]]})
((3,2),Shortest {distance = Sum {getSum = 2}, path = [[3,4,2]]})
((3,4),Shortest {distance = Sum {getSum = 1}, path = [[3,4]]})
((4,1),Shortest {distance = Sum {getSum = 2}, path = [[4,2,1]]})
((4,2),Shortest {distance = Sum {getSum = 1}, path = [[4,2]]})
((4,3),Shortest {distance = Sum {getSum = 2}, path = [[4,2,3]]})
```


Graph labeled by chars:


```haskell
g2 = fromList [(('A','S'), 1)
             ,(('A','D'), -1)
             ,(('S','E'), 2)
             ,(('D','E'), 4)]
```



```txt
λ> showShortestPaths "ASDE" (Sum <$> g2)
(('A','D'),Shortest {distance = Sum {getSum = -1}, path = ["AD"]})
(('A','E'),Shortest {distance = Sum {getSum = 3}, path = ["ASE","ADE"]})
(('A','S'),Shortest {distance = Sum {getSum = 1}, path = ["AS"]})
(('D','E'),Shortest {distance = Sum {getSum = 4}, path = ["DE"]})
(('S','E'),Shortest {distance = Sum {getSum = 2}, path = ["SE"]})
```



## J



```J
floyd=: verb define
  for_j. i.#y do.
    y=. y <. j ({"1 +/ {) y
  end.
)
```


Example use:


```J
graph=: ".;._2]0 :0
  0  _ _2 _  NB. 1->3 costs _2
  4  0  3 _  NB. 2->1 costs 4; 2->3 costs 3
  _  _  0 2  NB. 3->4 costs 2
  _ _1  _ 0  NB. 4->2 costs _1
)

   floyd graph
0 _1 _2 0
4  0  2 4
5  1  0 2
3 _1  1 0
```


The graph matrix holds the costs of each directed node. Row index corresponds to starting node. Column index corresponds to ending node. Unconnected nodes have infinite cost.

This approach turns out to be faster than the more concise <./ .+~^:_ for many relatively small graphs (though <code>floyd</code> happens to be slightly slower for the task example).

'''Path Reconstruction'''

This draft task currently asks for path reconstruction, which is a different (related) algorithm:


```J
floydrecon=: verb define
  n=. ($y)$_(I._=,y)},($$i.@#)y
  for_j. i.#y do.
    d=. y <. j ({"1 +/ {) y
    b=. y~:d
    y=. d
    n=. (n*-.b)+b * j{"1 n
  end.
)

task=: verb define
  dist=. floyd y
  next=. floydrecon y
  echo 'pair  dist   path'
  for_i. i.#y do.
    for_k. i.#y do.
      ndx=. <i,k
      if. (i~:k)*_>ndx{next do.
        txt=. (":1+i),'->',(":1+k)
        txt=. txt,_5{.":ndx{dist
        txt=. txt,'    ',":1+i
        j=. i
        while. j~:k do.
          assert. j~:(<j,k){next
          j=. (<j,k){next
          txt=. txt,'->',":1+j
        end.
        echo txt
      end.
    end.
  end.
  i.0 0
)
```


Draft output:


```J
   task graph
pair  dist   path
1->2   _1    1->3->4->2
1->3   _2    1->3
1->4    0    1->3->4
2->1    4    2->1
2->3    2    2->1->3
2->4    4    2->1->3->4
3->1    5    3->4->2->1
3->2    1    3->4->2
3->4    2    3->4
4->1    3    4->2->1
4->2   _1    4->2
4->3    1    4->2->1->3
```



## Java


```java
import static java.lang.String.format;
import java.util.Arrays;

public class FloydWarshall {

    public static void main(String[] args) {
        int[][] weights = {{1, 3, -2}, {2, 1, 4}, {2, 3, 3}, {3, 4, 2}, {4, 2, -1}};
        int numVertices = 4;

        floydWarshall(weights, numVertices);
    }

    static void floydWarshall(int[][] weights, int numVertices) {

        double[][] dist = new double[numVertices][numVertices];
        for (double[] row : dist)
            Arrays.fill(row, Double.POSITIVE_INFINITY);

        for (int[] w : weights)
            dist[w[0] - 1][w[1] - 1] = w[2];

        int[][] next = new int[numVertices][numVertices];
        for (int i = 0; i < next.length; i++) {
            for (int j = 0; j < next.length; j++)
                if (i != j)
                    next[i][j] = j + 1;
        }

        for (int k = 0; k < numVertices; k++)
            for (int i = 0; i < numVertices; i++)
                for (int j = 0; j < numVertices; j++)
                    if (dist[i][k] + dist[k][j] < dist[i][j]) {
                        dist[i][j] = dist[i][k] + dist[k][j];
                        next[i][j] = next[i][k];
                    }

        printResult(dist, next);
    }

    static void printResult(double[][] dist, int[][] next) {
        System.out.println("pair     dist    path");
        for (int i = 0; i < next.length; i++) {
            for (int j = 0; j < next.length; j++) {
                if (i != j) {
                    int u = i + 1;
                    int v = j + 1;
                    String path = format("%d -> %d    %2d     %s", u, v,
                            (int) dist[i][j], u);
                    do {
                        u = next[u - 1][v - 1];
                        path += " -> " + u;
                    } while (u != v);
                    System.out.println(path);
                }
            }
        }
    }
}
```


```txt
pair     dist    path
1 -> 2    -1     1 -> 3 -> 4 -> 2
1 -> 3    -2     1 -> 3
1 -> 4     0     1 -> 3 -> 4
2 -> 1     4     2 -> 1
2 -> 3     2     2 -> 1 -> 3
2 -> 4     4     2 -> 1 -> 3 -> 4
3 -> 1     5     3 -> 4 -> 2 -> 1
3 -> 2     1     3 -> 4 -> 2
3 -> 4     2     3 -> 4
4 -> 1     3     4 -> 2 -> 1
4 -> 2    -1     4 -> 2
4 -> 3     1     4 -> 2 -> 1 -> 3
```



## JavaScript

```javascript
var graph = [];
for (i = 0; i < 10; ++i) {
  graph.push([]);
  for (j = 0; j < 10; ++j)
    graph[i].push(i == j ? 0 : 9999999);
}

for (i = 1; i < 10; ++i) {
  graph[0][i] = graph[i][0] = parseInt(Math.random() * 9 + 1);
}

for (k = 0; k < 10; ++k) {
  for (i = 0; i < 10; ++i) {
    for (j = 0; j < 10; ++j) {
      if (graph[i][j] > graph[i][k] + graph[k][j])
        graph[i][j] = graph[i][k] + graph[k][j]
    }
  }
}

console.log(graph);
```



## jq

In this section, we represent the graph by a JSON object giving the weights: if u and v are the (string) labels of two nodes connected with an arrow from u to v, then .[u][v] is the associated weight:

```jq

def weights: {
  "1": {"3": -2},
  "2": {"1" : 4, "3": 3},
  "3": {"4": 2},
  "4": {"2": -1}
};
```


The algorithm given here is a direct implementation of the definitional algorithm:

```jq
def fwi:
  . as $weights
  | keys_unsorted as $nodes
  # construct the dist matrix
  | reduce $nodes[] as $u ({};
      reduce $nodes[] as $v (.;
        .[$u][$v] = infinite))
  | reduce $nodes[] as $u (.; .[$u][$u] = 0 )
  | reduce $nodes[] as $u (.;
      reduce ($weights[$u]|keys_unsorted[]) as $v (.;
        .[$u][$v] = $weights[$u][$v] ))
  | reduce $nodes[] as $w (.;
      reduce $nodes[] as $u (.;
        reduce $nodes[] as $v (.;
	  (.[$u][$w] + .[$w][$v]) as $x
	  | if .[$u][$v] > $x then .[$u][$v] = $x
	    else . end )))
;


weights | fwi
```

```txt
{
  "1": {
    "1": 0,
    "2": -1,
    "3": -2,
    "4": 0
  },
  "2": {
    "1": 4,
    "2": 0,
    "3": 2,
    "4": 4
  },
  "3": {
    "1": 5,
    "2": 1,
    "3": 0,
    "4": 2
  },
  "4": {
    "1": 3,
    "2": -1,
    "3": 1,
    "4": 0
  }
}
```



## Julia

```julia
# Floyd-Warshall algorithm: https://rosettacode.org/wiki/Floyd-Warshall_algorithm
# v0.6

function floydwarshall(weights::Matrix, nvert::Int)
    dist = fill(Inf, nvert, nvert)
    for i in 1:size(weights, 1)
        dist[weights[i, 1], weights[i, 2]] = weights[i, 3]
    end
    # return dist
    next = collect(j != i ? j : 0 for i in 1:nvert, j in 1:nvert)

    for k in 1:nvert, i in 1:nvert, j in 1:nvert
        if dist[i, k] + dist[k, j] < dist[i, j]
            dist[i, j] = dist[i, k] + dist[k, j]
            next[i, j] = next[i, k]
        end
    end

    # return next
    function printresult(dist, next)
        println("pair     dist    path")
        for i in 1:size(next, 1), j in 1:size(next, 2)
            if i != j
                u = i
                path = @sprintf "%d -> %d    %2d     %s" i j dist[i, j] i
                while true
                    u = next[u, j]
                    path *= " -> $u"
                    if u == j break end
                end
                println(path)
            end
        end
    end
    printresult(dist, next)
end

floydwarshall([1 3 -2; 2 1 4; 2 3 3; 3 4 2; 4 2 -1], 4)
```



## Kotlin

```scala
// version 1.1

object FloydWarshall {
    fun doCalcs(weights: Array<IntArray>, nVertices: Int) {
        val dist = Array(nVertices) { DoubleArray(nVertices) { Double.POSITIVE_INFINITY } }
        for (w in weights) dist[w[0] - 1][w[1] - 1] = w[2].toDouble()
        val next = Array(nVertices) { IntArray(nVertices) }
        for (i in 0 until next.size) {
            for (j in 0 until next.size) {
                if (i != j) next[i][j] = j + 1
            }
        }
        for (k in 0 until nVertices) {
            for (i in 0 until nVertices) {
                for (j in 0 until nVertices) {
                    if (dist[i][k] + dist[k][j] < dist[i][j]) {
                        dist[i][j] = dist[i][k] + dist[k][j]
                        next[i][j] = next[i][k]
                    }
                }
            }
        }
        printResult(dist, next)
    }

    private fun printResult(dist: Array<DoubleArray>, next: Array<IntArray>) {
        var u: Int
        var v: Int
        var path: String
        println("pair     dist    path")
        for (i in 0 until next.size) {
            for (j in 0 until next.size) {
                if (i != j) {
                    u = i + 1
                    v = j + 1
                    path = ("%d -> %d    %2d     %s").format(u, v, dist[i][j].toInt(), u)
                    do {
                        u = next[u - 1][v - 1]
                        path += " -> " + u
                    } while (u != v)
                    println(path)
                }
            }
        }
    }
}

fun main(args: Array<String>) {
    val weights = arrayOf(
            intArrayOf(1, 3, -2),
            intArrayOf(2, 1, 4),
            intArrayOf(2, 3, 3),
            intArrayOf(3, 4, 2),
            intArrayOf(4, 2, -1)
    )
    val nVertices = 4
    FloydWarshall.doCalcs(weights, nVertices)
}
```


```txt

pair     dist    path
1 -> 2    -1     1 -> 3 -> 4 -> 2
1 -> 3    -2     1 -> 3
1 -> 4     0     1 -> 3 -> 4
2 -> 1     4     2 -> 1
2 -> 3     2     2 -> 1 -> 3
2 -> 4     4     2 -> 1 -> 3 -> 4
3 -> 1     5     3 -> 4 -> 2 -> 1
3 -> 2     1     3 -> 4 -> 2
3 -> 4     2     3 -> 4
4 -> 1     3     4 -> 2 -> 1
4 -> 2    -1     4 -> 2
4 -> 3     1     4 -> 2 -> 1 -> 3

```



## Lua

```lua
function printResult(dist, nxt)
    print("pair     dist    path")
    for i=0, #nxt do
        for j=0, #nxt do
            if i ~= j then
                u = i + 1
                v = j + 1
                path = string.format("%d -> %d    %2d     %s", u, v, dist[i][j], u)
                repeat
                    u = nxt[u-1][v-1]
                    path = path .. " -> " .. u
                until (u == v)
                print(path)
            end
        end
    end
end

function floydWarshall(weights, numVertices)
    dist = {}
    for i=0, numVertices-1 do
        dist[i] = {}
        for j=0, numVertices-1 do
            dist[i][j] = math.huge
        end
    end

    for _,w in pairs(weights) do
        -- the weights array is one based
        dist[w[1]-1][w[2]-1] = w[3]
    end

    nxt = {}
    for i=0, numVertices-1 do
        nxt[i] = {}
        for j=0, numVertices-1 do
            if i ~= j then
                nxt[i][j] = j+1
            end
        end
    end

    for k=0, numVertices-1 do
        for i=0, numVertices-1 do
            for j=0, numVertices-1 do
                if dist[i][k] + dist[k][j] < dist[i][j] then
                    dist[i][j] = dist[i][k] + dist[k][j]
                    nxt[i][j] = nxt[i][k]
                end
            end
        end
    end

    printResult(dist, nxt)
end

weights = {
    {1, 3, -2},
    {2, 1, 4},
    {2, 3, 3},
    {3, 4, 2},
    {4, 2, -1}
}
numVertices = 4
floydWarshall(weights, numVertices)
```

```txt
pair     dist    path
1 -> 2    -1     1 -> 3 -> 4 -> 2
1 -> 3    -2     1 -> 3
1 -> 4     0     1 -> 3 -> 4
2 -> 1     4     2 -> 1
2 -> 3     2     2 -> 1 -> 3
2 -> 4     4     2 -> 1 -> 3 -> 4
3 -> 1     5     3 -> 4 -> 2 -> 1
3 -> 2     1     3 -> 4 -> 2
3 -> 4     2     3 -> 4
4 -> 1     3     4 -> 2 -> 1
4 -> 2    -1     4 -> 2
4 -> 3     1     4 -> 2 -> 1 -> 3
```


=={{header|Modula-2}}==

```modula2
MODULE FloydWarshall;
FROM FormatString IMPORT FormatString;
FROM SpecialReals IMPORT Infinity;
FROM Terminal IMPORT ReadChar,WriteString,WriteLn;

CONST NUM_VERTICIES = 4;
TYPE
    IntArray = ARRAY[0..NUM_VERTICIES-1],[0..NUM_VERTICIES-1] OF INTEGER;
    RealArray = ARRAY[0..NUM_VERTICIES-1],[0..NUM_VERTICIES-1] OF REAL;

PROCEDURE FloydWarshall(weights : ARRAY OF ARRAY OF INTEGER);
VAR
    dist : RealArray;
    next : IntArray;
    i,j,k : INTEGER;
BEGIN
    FOR i:=0 TO NUM_VERTICIES-1 DO
        FOR j:=0 TO NUM_VERTICIES-1 DO
            dist[i,j] := Infinity;
        END
    END;
    k := HIGH(weights);
    FOR i:=0 TO k DO
        dist[weights[i,0]-1,weights[i,1]-1] := FLOAT(weights[i,2]);
    END;
    FOR i:=0 TO NUM_VERTICIES-1 DO
        FOR j:=0 TO NUM_VERTICIES-1 DO
            IF i#j THEN
                next[i,j] := j+1;
            END
        END
    END;
    FOR k:=0 TO NUM_VERTICIES-1 DO
        FOR i:=0 TO NUM_VERTICIES-1 DO
            FOR j:=0 TO NUM_VERTICIES-1 DO
                IF dist[i,j] > dist[i,k] + dist[k,j] THEN
                    dist[i,j] := dist[i,k] + dist[k,j];
                    next[i,j] := next[i,k];
                END
            END
        END
    END;
    PrintResult(dist, next);
END FloydWarshall;

PROCEDURE PrintResult(dist : RealArray; next : IntArray);
VAR
    i,j,u,v : INTEGER;
    buf : ARRAY[0..63] OF CHAR;
BEGIN
    WriteString("pair     dist    path");
    WriteLn;
    FOR i:=0 TO NUM_VERTICIES-1 DO
        FOR j:=0 TO NUM_VERTICIES-1 DO
            IF i#j THEN
                u := i + 1;
                v := j + 1;
                FormatString("%i -> %i    %2i     %i", buf, u, v, TRUNC(dist[i,j]), u);
                WriteString(buf);
                REPEAT
                    u := next[u-1,v-1];
                    FormatString(" -> %i", buf, u);
                    WriteString(buf);
                UNTIL u=v;
                WriteLn
            END
        END
    END
END PrintResult;

TYPE WeightArray = ARRAY[0..4],[0..2] OF INTEGER;
VAR weights : WeightArray;
BEGIN
    weights := WeightArray{
        {1,  3, -2},
        {2,  1,  4},
        {2,  3,  3},
        {3,  4,  2},
        {4,  2, -1}
    };

    FloydWarshall(weights);

    ReadChar
END FloydWarshall.
```



## Perl


```perl6
sub FloydWarshall{
	my $edges = shift;
	my (@dist, @seq);
	my $num_vert = 0;
	# insert given dists into dist matrix
	map {
		$dist[$_->[0] - 1][$_->[1] - 1] = $_->[2];
		$num_vert = $_->[0] if $num_vert < $_->[0];
		$num_vert = $_->[1] if $num_vert < $_->[1];
	} @$edges;
	my @vertices = 0..($num_vert - 1);
	# init sequence/"next" table
	for my $i(@vertices){
		for my $j(@vertices){
			$seq[$i][$j] = $j if $i != $j;
		}
	}
	# diagonal of dists matrix
	#map {$dist[$_][$_] = 0} @vertices;
	for my $k(@vertices){
		for my $i(@vertices){
			next unless defined $dist[$i][$k];
			for my $j(@vertices){
				next unless defined $dist[$k][$j];
				if($i != $j && (!defined($dist[$i][$j])
						|| $dist[$i][$j] > $dist[$i][$k] + $dist[$k][$j])){
					$dist[$i][$j] = $dist[$i][$k] + $dist[$k][$j];
					$seq[$i][$j] = $seq[$i][$k];
				}
			}
		}
	}
	# print table
	print "pair     dist    path\n";
	for my $i(@vertices){
		for my $j(@vertices){
			next if $i == $j;
			my @path = ($i + 1);
			while($seq[$path[-1] - 1][$j] != $j){
				push @path, $seq[$path[-1] - 1][$j] + 1;
			}
			push @path, $j + 1;
			printf "%d -> %d  %4d     %s\n",
				$path[0], $path[-1], $dist[$i][$j], join(' -> ', @path);
		}
	}
}

my $graph = [[1, 3, -2], [2, 1, 4], [2, 3, 3], [3, 4, 2], [4, 2, -1]];
FloydWarshall($graph);
```

```txt
pair     dist    path
1 -> 2    -1     1 -> 3 -> 4 -> 2
1 -> 3    -2     1 -> 3
1 -> 4     0     1 -> 3 -> 4
2 -> 1     4     2 -> 1
2 -> 3     2     2 -> 1 -> 3
2 -> 4     4     2 -> 1 -> 3 -> 4
3 -> 1     5     3 -> 4 -> 2 -> 1
3 -> 2     1     3 -> 4 -> 2
3 -> 4     2     3 -> 4
4 -> 1     3     4 -> 2 -> 1
4 -> 2    -1     4 -> 2
4 -> 3     1     4 -> 2 -> 1 -> 3
```



## Perl 6

```perl6
sub Floyd-Warshall (Int $n, @edge) {
    my @dist = [0, |(Inf xx $n-1)], *.Array.rotate(-1) … !*[*-1];
    my @next = [0 xx $n] xx $n;

    for @edge -> ($u, $v, $w) {
        @dist[$u-1;$v-1] = $w;
        @next[$u-1;$v-1] = $v-1;
    }

    for [X] ^$n xx 3 -> ($k, $i, $j) {
        if @dist[$i;$j] > my $sum = @dist[$i;$k] + @dist[$k;$j] {
            @dist[$i;$j] = $sum;
            @next[$i;$j] = @next[$i;$k];
        }
    }

    say ' Pair  Distance     Path';
    for [X] ^$n xx 2 -> ($i, $j){
        next if $i == $j;
        my @path = $i;
        @path.push: @next[@path[*-1];$j] until @path[*-1] == $j;
        printf("%d → %d  %4d       %s\n", $i+1, $j+1, @dist[$i;$j],
          @path.map( *+1 ).join(' → '));
    }
}

Floyd-Warshall(4, [[1, 3, -2], [2, 1, 4], [2, 3, 3], [3, 4, 2], [4, 2, -1]]);
```

```txt
 Pair  Distance     Path
1 → 2    -1       1 → 3 → 4 → 2
1 → 3    -2       1 → 3
1 → 4     0       1 → 3 → 4
2 → 1     4       2 → 1
2 → 3     2       2 → 1 → 3
2 → 4     4       2 → 1 → 3 → 4
3 → 1     5       3 → 4 → 2 → 1
3 → 2     1       3 → 4 → 2
3 → 4     2       3 → 4
4 → 1     3       4 → 2 → 1
4 → 2    -1       4 → 2
4 → 3     1       4 → 2 → 1 → 3

```



## Phix

Direct translation of the wikipedia pseudocode

```Phix
constant inf = 1e300*1e300

function Path(integer u, integer v, sequence next)
    if next[u,v]=null then
       return ""
    end if
    sequence path = {sprintf("%d",u)}
    while u!=v do
       u = next[u,v]
       path = append(path,sprintf("%d",u))
    end while
    return join(path,"->")
end function

procedure FloydWarshall(integer V, sequence weights)
    sequence dist = repeat(repeat(inf,V),V)
    sequence next = repeat(repeat(null,V),V)
    for k=1 to length(weights) do
      integer {u,v,w} = weights[k]
      dist[u,v] := w  -- the weight of the edge (u,v)
      next[u,v] := v
    end for
    -- standard Floyd-Warshall implementation
    for k=1 to V do
      for i=1 to V do
        for j=1 to V do
          atom d = dist[i,k] + dist[k,j]
          if dist[i,j] > d then
            dist[i,j] := d
            next[i,j] := next[i,k]
          end if
        end for
      end for
    end for
    printf(1,"pair  dist  path\n")
    for u=1 to V do
      for v=1 to V do
        if u!=v then
          printf(1,"%d->%d   %2d   %s\n",{u,v,dist[u,v],Path(u,v,next)})
        end if
      end for
    end for
end procedure

constant V = 4
constant weights = {{1, 3, -2}, {2, 1, 4}, {2, 3, 3}, {3, 4, 2}, {4, 2, -1}}
FloydWarshall(V,weights)
```

```txt

pair  dist  path
1->2   -1   1->3->4->2
1->3   -2   1->3
1->4    0   1->3->4
2->1    4   2->1
2->3    2   2->1->3
2->4    4   2->1->3->4
3->1    5   3->4->2->1
3->2    1   3->4->2
3->4    2   3->4
4->1    3   4->2->1
4->2   -1   4->2
4->3    1   4->2->1->3

```



## PHP


```php
<?php
$graph = array();
for ($i = 0; $i < 10; ++$i) {
    $graph[] = array();
    for ($j = 0; $j < 10; ++$j)
        $graph[$i][] = $i == $j ? 0 : 9999999;
}

for ($i = 1; $i < 10; ++$i) {
    $graph[0][$i] = $graph[$i][0] = rand(1, 9);
}

for ($k = 0; $k < 10; ++$k) {
    for ($i = 0; $i < 10; ++$i) {
        for ($j = 0; $j < 10; ++$j) {
            if ($graph[$i][$j] > $graph[$i][$k] + $graph[$k][$j])
                $graph[$i][$j] = $graph[$i][$k] + $graph[$k][$j];
        }
    }
}

print_r($graph);
?>
```



## Prolog

Works with SWI-Prolog as of Jan 2019

```prolog
:- use_module(library(clpfd)).

path(List, To, From, [From], W) :-
    select([To,From,W],List,_).
path(List, To, From, [Link|R], W) :-
    select([To,Link,W1],List,Rest),
    W #= W1 + W2,
    path(Rest, Link, From, R, W2).

find_path(Din, From, To, [From|Pout], Wout) :-
    between(1, 4, From),
    between(1, 4, To),
    dif(From, To),
    findall([W,P], (
                path(Din, From, To, P, W),
                all_distinct(P)
            ), Paths),
    sort(Paths, [[Wout,Pout]|_]).


print_all_paths :-
    D = [[1, 3, -2], [2, 3, 3], [2, 1, 4], [3, 4, 2], [4, 2, -1]],
    format('Pair\t  Dist\tPath~n'),
    forall(
        find_path(D, From, To, Path, Weight),(
            atomic_list_concat(Path, ' -> ', PPath),
            format('~p -> ~p\t  ~p\t~w~n', [From, To, Weight, PPath]))).
```

```txt
?- print_all_paths.
Pair      Dist  Path
1 -> 2    -1    1 -> 3 -> 4 -> 2
1 -> 3    -2    1 -> 3
1 -> 4    0     1 -> 3 -> 4
2 -> 1    4     2 -> 1
2 -> 3    2     2 -> 1 -> 3
2 -> 4    4     2 -> 1 -> 3 -> 4
3 -> 1    5     3 -> 4 -> 2 -> 1
3 -> 2    1     3 -> 4 -> 2
3 -> 4    2     3 -> 4
4 -> 1    3     4 -> 2 -> 1
4 -> 2    -1    4 -> 2
4 -> 3    1     4 -> 2 -> 1 -> 3
true.

?-
```



## Python

```python
from math import inf
from itertools import product

def floyd_warshall(n, edge):
    rn = range(n)
    dist = [[inf] * n for i in rn]
    nxt  = [[0]   * n for i in rn]
    for i in rn:
        dist[i][i] = 0
    for u, v, w in edge:
        dist[u-1][v-1] = w
        nxt[u-1][v-1] = v-1
    for k, i, j in product(rn, repeat=3):
        sum_ik_kj = dist[i][k] + dist[k][j]
        if dist[i][j] > sum_ik_kj:
            dist[i][j] = sum_ik_kj
            nxt[i][j]  = nxt[i][k]
    print("pair     dist    path")
    for i, j in product(rn, repeat=2):
        if i != j:
            path = [i]
            while path[-1] != j:
                path.append(nxt[path[-1]][j])
            print("%d → %d  %4d       %s"
                  % (i + 1, j + 1, dist[i][j],
                     ' → '.join(str(p + 1) for p in path)))

if __name__ == '__main__':
    floyd_warshall(4, [[1, 3, -2], [2, 1, 4], [2, 3, 3], [3, 4, 2], [4, 2, -1]])
```


```txt
pair     dist    path
1 → 2    -1       1 → 3 → 4 → 2
1 → 3    -2       1 → 3
1 → 4     0       1 → 3 → 4
2 → 1     4       2 → 1
2 → 3     2       2 → 1 → 3
2 → 4     4       2 → 1 → 3 → 4
3 → 1     5       3 → 4 → 2 → 1
3 → 2     1       3 → 4 → 2
3 → 4     2       3 → 4
4 → 1     3       4 → 2 → 1
4 → 2    -1       4 → 2
4 → 3     1       4 → 2 → 1 → 3
```



## Racket

```racket
#lang typed/racket
(require math/array)

;; in : initialized dist and next matrices
;; out : dist and next matrices
;; O(n^3)
(define-type Next-T (Option Index))
(define-type Dist-T Real)
(define-type Dists (Array Dist-T))
(define-type Nexts (Array Next-T))
(define-type Settable-Dists (Settable-Array Dist-T))
(define-type Settable-Nexts (Settable-Array Next-T))

(: floyd-with-path (-> Index Dists Nexts (Values Dists Nexts)))
(: init-edges (-> Index (Values Settable-Dists Settable-Nexts)))

(define (floyd-with-path n dist-in next-in)
  (define dist : Settable-Dists (array->mutable-array dist-in))
  (define next : Settable-Nexts (array->mutable-array next-in))
  (for* ((k n) (i n) (j n))
    (when (negative? (array-ref dist (vector j j)))
      (raise 'negative-cycle))
    (define i.k (vector i k))
    (define i.j (vector i j))
    (define d (+ (array-ref dist i.k) (array-ref dist (vector k j))))
    (when (< d (array-ref dist i.j))
      (array-set! dist i.j d)
      (array-set! next i.j (array-ref next i.k))))
  (values dist next))

;; utilities

;; init random edges costs, matrix 66% filled
(define (init-edges n)
  (define dist : Settable-Dists (array->mutable-array (make-array (vector n n) 0)))
  (define next : Settable-Nexts (array->mutable-array (make-array (vector n n) #f)))
  (for* ((i n) (j n) #:unless (= i j))
    (define i.j (vector i j))
    (array-set! dist i.j +Inf.0)
    (unless (< (random) 0.3)
      (array-set! dist i.j (add1 (random 100)))
      (array-set! next i.j j)))
  (values dist next))

;; show path from u to v
(: path (-> Nexts Index Index (Listof Index)))
(define (path next u v)
  (let loop : (Listof Index) ((u : Index u) (rv : (Listof Index) null))
    (if (= u v)
        (reverse (cons u rv))
        (let ((nxt (array-ref next (vector u v))))
          (if nxt (loop nxt (cons u rv)) null)))))

;; show computed distance
(: mdist (-> Dists Index Index Dist-T))
(define (mdist dist u v)
  (array-ref dist (vector u v)))

(module+ main
  (define n 8)
  (define-values (dist next) (init-edges n))
  (define-values (dist+ next+) (floyd-with-path n dist next))
  (displayln "original dist")
  dist
  (displayln "new dist and next")
  dist+
  next+
  ;; note, these path and dist calls are not as carefully crafted as
  ;; the echolisp ones (in fact they're verbatim copied)
  (displayln "paths and distances")
  (path  next+ 1 3)
  (mdist dist+ 1 0)
  (mdist dist+ 0 3)
  (mdist dist+ 1 3)
  (path next+ 7 6)
  (path next+ 6 7))
```


```txt
original dist
(mutable-array
 #[#[0 51 +inf.0 11 44 13 +inf.0 86]
   #[48 0 70 +inf.0 65 78 77 54]
   #[29 +inf.0 0 +inf.0 78 14 +inf.0 24]
   #[40 79 52 0 +inf.0 99 37 88]
   #[71 62 +inf.0 7 0 +inf.0 +inf.0 +inf.0]
   #[89 65 83 +inf.0 91 0 41 70]
   #[69 34 +inf.0 49 +inf.0 89 0 20]
   #[2 56 +inf.0 60 +inf.0 75 +inf.0 0]])
new dist and next
(mutable-array
 #[#[0 51 63 11 44 13 48 68]
   #[48 0 70 59 65 61 77 54]
   #[26 77 0 37 70 14 55 24]
   #[40 71 52 0 84 53 37 57]
   #[47 62 59 7 0 60 44 64]
   #[63 65 83 74 91 0 41 61]
   #[22 34 85 33 66 35 0 20]
   #[2 53 65 13 46 15 50 0]])
(mutable-array
 #[#[#f 1 3 3 4 5 3 3]
   #[0 #f 2 0 4 0 6 7]
   #[7 7 #f 7 7 5 5 7]
   #[0 6 2 #f 0 0 6 6]
   #[3 1 3 3 #f 3 3 3]
   #[6 1 2 6 4 #f 6 6]
   #[7 1 7 7 7 7 #f 7]
   #[0 0 0 0 0 0 0 #f]])
paths and distances
'(1 0 3)
48
11
59
'(7 0 3 6)
'(6 7)
```



## REXX


```rexx
/*REXX program uses Floyd-Warshall algorithm to find shortest distance between vertices.*/
v=4              /*███       {1}       ███*/     /*number of vertices in weighted graph.*/
@.= 99999999     /*███    4 /   \ -2   ███*/     /*the default distance  (edge weight). */
@.1.3=-2         /*███     /  3  \     ███*/     /*the distance (weight) for an edge.   */
@.2.1= 4         /*███  {2} ────► {3}  ███*/     /* "     "         "     "   "   "     */
@.2.3= 3         /*███     \     /     ███*/     /* "     "         "     "   "   "     */
@.3.4= 2         /*███   -1 \   / 2    ███*/     /* "     "         "     "   "   "     */
@.4.2=-1         /*███       {4}       ███*/     /* "     "         "     "   "   "     */
            do     k=1  for v
              do   i=1  for v
                do j=1  for v;  _=@.i.k + @.k.j
                if @.i.j>_  then @.i.j=_         /*use a new distance (weight) for edge.*/
                end   /*j*/
              end     /*i*/
            end       /*k*/
w=12                                             /*width of the columns for the output. */
say center('vertices', w)  center('distance', w) /*display the  1st  line of the title. */
say center('pair'    , w)  center('(weight)', w) /*   "     "   2nd    "   "  "    "    */
say copies('═'       , w)  copies('═'       , w) /*   "     "   3rd    "   "  "    "    */
                                                 /* [↓]  display edge distances (weight)*/
   do   f=1  for v                               /*process each of the "from" vertices. */
     do t=1  for v;   if f==t  then iterate      /*   "      "   "  "   "to"      "     */
     say center(f '─►' t, w)   right(@.f.t, w%2) /*show the distance between 2 vertices.*/
     end   /*t*/
   end     /*f*/                                 /*stick a fork in it,  we're all done. */
```

'''output'''   when using the defaults:

```txt

  vertices     distance
    pair       (weight)
════════════ ════════════
   1 ─► 2        -1
   1 ─► 3        -2
   1 ─► 4         0
   2 ─► 1         4
   2 ─► 3         2
   2 ─► 4         4
   3 ─► 1         5
   3 ─► 2         1
   3 ─► 4         2
   4 ─► 1         3
   4 ─► 2        -1
   4 ─► 3         1

```



## Ruby


```ruby
def floyd_warshall(n, edge)
  dist = Array.new(n){|i| Array.new(n){|j| i==j ? 0 : Float::INFINITY}}
  nxt = Array.new(n){Array.new(n)}
  edge.each do |u,v,w|
    dist[u-1][v-1] = w
    nxt[u-1][v-1] = v-1
  end

  n.times do |k|
    n.times do |i|
      n.times do |j|
        if dist[i][j] > dist[i][k] + dist[k][j]
          dist[i][j] = dist[i][k] + dist[k][j]
          nxt[i][j] = nxt[i][k]
        end
      end
    end
  end

  puts "pair     dist    path"
  n.times do |i|
    n.times do |j|
      next  if i==j
      u = i
      path = [u]
      path << (u = nxt[u][j])  while u != j
      path = path.map{|u| u+1}.join(" -> ")
      puts "%d -> %d  %4d     %s" % [i+1, j+1, dist[i][j], path]
    end
  end
end

n = 4
edge = [[1, 3, -2], [2, 1, 4], [2, 3, 3], [3, 4, 2], [4, 2, -1]]
floyd_warshall(n, edge)
```


```txt

pair     dist    path
1 -> 2    -1     1 -> 3 -> 4 -> 2
1 -> 3    -2     1 -> 3
1 -> 4     0     1 -> 3 -> 4
2 -> 1     4     2 -> 1
2 -> 3     2     2 -> 1 -> 3
2 -> 4     4     2 -> 1 -> 3 -> 4
3 -> 1     5     3 -> 4 -> 2 -> 1
3 -> 2     1     3 -> 4 -> 2
3 -> 4     2     3 -> 4
4 -> 1     3     4 -> 2 -> 1
4 -> 2    -1     4 -> 2
4 -> 3     1     4 -> 2 -> 1 -> 3

```



## SequenceL

```sequencel>import <Utilities/Sequence.sl
;
import <Utilities/Math.sl>;

ARC ::= (To: int, Weight: float);
arc(t,w) := (To: t, Weight: w);
VERTEX ::= (Label: int, Arcs: ARC(1));
vertex(l,arcs(1)) := (Label: l, Arcs: arcs);

getArcsFrom(vertex, graph(1)) :=
    let
        index := firstIndexOf(graph.Label, vertex);
    in
        [] when index = 0
    else
        graph[index].Arcs;

getWeightTo(vertex, arcs(1)) :=
    let
        index := firstIndexOf(arcs.To, vertex);
    in
        0 when index = 0
    else
        arcs[index].Weight;

throughK(k, dist(2)) :=
    let
        newDist[i, j] := min(dist[i][k] + dist[k][j], dist[i][j]);
    in
        dist when k > size(dist)
    else
        throughK(k + 1, newDist);

floydWarshall(graph(1)) :=
    let
        initialResult[i,j] := 1.79769e308 when i /= j else 0
                              foreach i within 1 ... size(graph),
                                      j within 1 ... size(graph);

        singleResult[i,j] := getWeightTo(j, getArcsFrom(i, graph))
                             foreach i within 1 ... size(graph),
                                     j within 1 ... size(graph);

        start[i,j] :=
                initialResult[i,j] when singleResult[i,j] = 0
            else
                singleResult[i,j];
    in
        throughK(1, start);

main() :=
    let
        graph := [vertex(1, [arc(3,-2)]),
                  vertex(2, [arc(1,4), arc(3,3)]),
                  vertex(3, [arc(4,2)]),
                  vertex(4, [arc(2,-1)])];
    in
        floydWarshall(graph);
```


```txt

[[0,-1,-2,0],[4,0,2,4],[5,1,0,2],[3,-1,1,0]]

```



## Sidef

```ruby
func floyd_warshall(n, edge) {
    var dist = n.of {|i| n.of { |j| i == j ? 0 : Inf }}
    var nxt  = n.of { n.of(nil) }
    for u,v,w in edge {
        dist[u-1][v-1] = w
         nxt[u-1][v-1] = v-1
    }

    [^n] * 3 -> cartesian { |k, i, j|
        if (dist[i][j] > dist[i][k]+dist[k][j]) {
            dist[i][j] = dist[i][k]+dist[k][j]
            nxt[i][j] = nxt[i][k]
        }
    }
 
    var summary = "pair     dist    path\n"
    for i,j (^n ~X ^n) {
        i==j && next
        var u = i
        var path = [u]
        while (u != j) {
            path << (u = nxt[u][j])
        }
        path.map!{|u| u+1 }.join!(" -> ")
        summary += ("%d -> %d  %4d     %s\n" % (i+1, j+1, dist[i][j], path))
    }

    return summary
}

var n = 4
var edge = [[1, 3, -2], [2, 1, 4], [2, 3, 3], [3, 4, 2], [4, 2, -1]]
print floyd_warshall(n, edge)
```

```txt

pair     dist    path
1 -> 2    -1     1 -> 3 -> 4 -> 2
1 -> 3    -2     1 -> 3
1 -> 4     0     1 -> 3 -> 4
2 -> 1     4     2 -> 1
2 -> 3     2     2 -> 1 -> 3
2 -> 4     4     2 -> 1 -> 3 -> 4
3 -> 1     5     3 -> 4 -> 2 -> 1
3 -> 2     1     3 -> 4 -> 2
3 -> 4     2     3 -> 4
4 -> 1     3     4 -> 2 -> 1
4 -> 2    -1     4 -> 2
4 -> 3     1     4 -> 2 -> 1 -> 3

```



## Tcl


The implementation of Floyd-Warshall in tcllib is [https://core.tcl.tk/tcllib/finfo?name=modules/struct/graphops.tcl quite readable];  this example merely initialises a graph from an adjacency list then calls the tcllib code:


```Tcl
package require Tcl 8.5     ;# for {*} and [dict]
package require struct::graph
package require struct::graph::op

struct::graph g

set arclist {
    a b
    a p
    b m
    b c
    c d
    d e
    e f
    f q
    f g
}

g node insert {*}$arclist

foreach {from to} $arclist {
    set a [g arc insert $from $to]
    g arc setweight $a 1.0
}

set paths [::struct::graph::op::FloydWarshall g]

set paths [dict filter $paths key {a *}]        ;# filter for paths starting at "a"
set paths [dict filter $paths value {[0-9]*}]   ;# whose cost is not "Inf"
set paths [lsort -stride 2 -index 1 -real -decreasing $paths]   ;# and print the longest first
puts $paths
```


```txt
{a q} 6.0 {a g} 6.0 {a f} 5.0 {a e} 4.0 {a d} 3.0 {a m} 2.0 {a c} 2.0 {a p} 1.0 {a b} 1.0 {a a} 0
```



## Visual Basic .NET

```vbnet
Module Module1

    Sub PrintResult(dist As Double(,), nxt As Integer(,))
        Console.WriteLine("pair     dist    path")
        For i = 1 To nxt.GetLength(0)
            For j = 1 To nxt.GetLength(1)
                If i <> j Then
                    Dim u = i
                    Dim v = j
                    Dim path = String.Format("{0} -> {1}    {2,2:G}     {3}", u, v, dist(i - 1, j - 1), u)
                    Do
                        u = nxt(u - 1, v - 1)
                        path += String.Format(" -> {0}", u)
                    Loop While u <> v
                    Console.WriteLine(path)
                End If
            Next
        Next
    End Sub

    Sub FloydWarshall(weights As Integer(,), numVerticies As Integer)
        Dim dist(numVerticies - 1, numVerticies - 1) As Double
        For i = 1 To numVerticies
            For j = 1 To numVerticies
                dist(i - 1, j - 1) = Double.PositiveInfinity
            Next
        Next

        For i = 1 To weights.GetLength(0)
            dist(weights(i - 1, 0) - 1, weights(i - 1, 1) - 1) = weights(i - 1, 2)
        Next

        Dim nxt(numVerticies - 1, numVerticies - 1) As Integer
        For i = 1 To numVerticies
            For j = 1 To numVerticies
                If i <> j Then
                    nxt(i - 1, j - 1) = j
                End If
            Next
        Next

        For k = 1 To numVerticies
            For i = 1 To numVerticies
                For j = 1 To numVerticies
                    If dist(i - 1, k - 1) + dist(k - 1, j - 1) < dist(i - 1, j - 1) Then
                        dist(i - 1, j - 1) = dist(i - 1, k - 1) + dist(k - 1, j - 1)
                        nxt(i - 1, j - 1) = nxt(i - 1, k - 1)
                    End If
                Next
            Next
        Next

        PrintResult(dist, nxt)
    End Sub

    Sub Main()
        Dim weights = {{1, 3, -2}, {2, 1, 4}, {2, 3, 3}, {3, 4, 2}, {4, 2, -1}}
        Dim numVeritices = 4

        FloydWarshall(weights, numVeritices)
    End Sub

End Module
```

```txt
pair     dist    path
1 -> 2    -1     1 -> 3 -> 4 -> 2
1 -> 3    -2     1 -> 3
1 -> 4     0     1 -> 3 -> 4
2 -> 1     4     2 -> 1
2 -> 3     2     2 -> 1 -> 3
2 -> 4     4     2 -> 1 -> 3 -> 4
3 -> 1     5     3 -> 4 -> 2 -> 1
3 -> 2     1     3 -> 4 -> 2
3 -> 4     2     3 -> 4
4 -> 1     3     4 -> 2 -> 1
4 -> 2    -1     4 -> 2
4 -> 3     1     4 -> 2 -> 1 -> 3
```



## zkl


```zkl
fcn FloydWarshallWithPathReconstruction(dist){ // dist is munged
   V:=dist[0].len();
   next:=V.pump(List,V.pump(List,Void.copy).copy);  // VxV matrix of Void
   foreach u,v in (V,V){ if(dist[u][v]!=Void and u!=v) next[u][v] = v }
   foreach k,i,j in (V,V,V){
      a,b,c:=dist[i][j],dist[i][k],dist[k][j];
      if( (a!=Void and b!=Void and c!=Void and a>b+c) or  // Inf math
	  (a==Void and b!=Void and c!=Void) ){
	 dist[i][j] = b+c;
	 next[i][j] = next[i][k];
      }
   }
   return(dist,next)
}
fcn path(next,u,v){
   if(Void==next[u][v]) return(T);
   path:=List(u);
   while(u!=v){ path.append(u = next[u][v]) }
   path
}
fcn printM(m){ m.pump(Console.println,rowFmt) }
fcn rowFmt(row){ ("%5s "*row.len()).fmt(row.xplode()) }
```


```zkl
const V=4;
dist:=V.pump(List,V.pump(List,Void.copy).copy);  // VxV matrix of Void
foreach i in (V){ dist[i][i] = 0 }	   // zero vertexes

/* Graph from the Wikipedia:
   1  2  3  4
 d ----------
1| 0  X -2  X
2| 4  0  3  X
3| X  X  0  2
4| X -1  X  0
*/
dist[0][2]=-2; dist[1][0]=4; dist[1][2]=3; dist[2][3]=2; dist[3][1]=-1;

dist,next:=FloydWarshallWithPathReconstruction(dist);
println("Shortest distance array:"); printM(dist);
println("\nPath array:");	     printM(next);
println("\nAll paths:");
foreach u,v in (V,V){
   if(p:=path(next,u,v)) p.println();
}
```

```txt

Shortest distance array:
    0    -1    -2     0
    4     0     2     4
    5     1     0     2
    3    -1     1     0

Path array:
 Void     2     2     2
    0  Void     0     0
    3     3  Void     3
    1     1     1  Void

All paths:
L(0,2,3,1)
L(0,2)
L(0,2,3)
L(1,0)
L(1,0,2)
L(1,0,2,3)
L(2,3,1,0)
L(2,3,1)
L(2,3)
L(3,1,0)
L(3,1)
L(3,1,0,2)

```

