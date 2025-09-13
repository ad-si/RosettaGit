+++
title = "I'm a software engineer, get me out of here"
description = ""
date = 2019-09-18T03:06:33Z
aliases = []
[extra]
id = 21941
[taxonomies]
categories = ["task"]
tags = []
+++

Your latest contract has hit a snag. You came to update the army payroll system, but awoke this morning to the sound of mortars landing not far away and panicked generals banging on you door. The President has loaded his gold on trucks and needs to find the shortest route to safety.  You are given the following map. The top left hand corner is (0,0). You and The President are located at HQ in the centre of the country (11,11). Cells marked 0 indicate safety. Numbers other than 0 indicate the number of cells that his party will travel in a day in any direction up, down, left, right, or diagonally. 

```txt

         00000         
      00003130000      
    000321322221000    
   00231222432132200   
  0041433223233211100  
  0232231612142618530  
 003152122326114121200 
 031252235216111132210 
 022211246332311115210 
00113232262121317213200
03152118212313211411110
03231234121132221411410
03513213411311414112320
00427534125412213211400
 013322444412122123210 
 015132331312411123120 
 003333612214233913300 
  0219126511415312570  
  0021321524341325100  
   00211415413523200   
    000122111322000    
      00001120000      
         00000         

```

Part 1 Use Dijkstra's algorithm to find a list of the shortest routes from HQ to safety.


Part 2

Six days later and you are called to another briefing. The good news is The President and his gold are safe, so your invoice may be paid if you can get out of here. To do this a number of troop repositions will be required. It is concluded that you need to know the shortest route from each cell to every other cell. You decide to use Floyd's algorithm. Print the shortest route from (21,11) to (1,11) and from (1,11) to (21,11), and the longest shortest route between any two points.


Extra Credit

# Is there any cell in the country that can not be reached from HQ?
# Which cells will it take longest to send reinforcements to from HQ?


Related tasks:
# [[Dijkstra's algorithm]]
# [[Floyd-Warshall algorithm]]

=={{header|F_Sharp|F#}}==
This task uses [http://www.rosettacode.org/wiki/Dijkstra%27s_algorithm#F.23 Dijkstra's algorithm (F#)]

This task uses [http://www.rosettacode.org/wiki/CSV_data_manipulation#F.23 readCSV (F#)]

### Part 1


```fsharp

let safety=readCSV '\t' "gmooh.dat"|>Seq.choose(fun n->if n.value="0" then Some (n.row,n.col) else None)
let board=readCSV '\t' "gmooh.dat"|>Seq.choose(fun n->match n.value with |"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" as g->Some((n.row,n.col),int g)|_->None)|>Map.ofSeq
let adjacent((n,g),v)=List.choose(fun y->if y=(n,g) then None else match Map.tryFind y board with |None->None|_->Some ((y),1)) [(n+v,g);(n-v,g);(n,g+v);(n,g-v);(n+v,g+v);(n+v,g-v);(n-v,g+v);(n-v,g-v);]
let adjacencyList=new System.Collections.Generic.Dictionary<(int*int),list<(int*int)*int>>()
let rec mkAdj start=
  let n=((start),Map.find start board)
  let g=adjacent n
  adjacencyList.Add((fst n),g)
  List.iter(fun ((n),_)->if (not (adjacencyList.ContainsKey n )) then mkAdj n) g
mkAdj (11,11)
let nodes=adjacencyList.Keys |> List.ofSeq
let G=nodes |>List.collect(fun n->List.map(fun (n',g)->(((n),(n')),g))(adjacencyList.Item n))|>Map.ofList
let paths=Dijkstra nodes G (11,11)
let _,res=safety|>Seq.choose(fun n->paths n) |> Seq.groupBy(fun n->List.length n)|>Seq.minBy fst
res |> Seq.iter (printfn "%A")

```

```txt

[(11, 11); (10, 11); (7, 11); (6, 12); (0, 12)]
[(11, 11); (10, 11); (7, 11); (7, 12); (1, 6)]
[(11, 11); (10, 10); (8, 8); (4, 8); (1, 8)]
[(11, 11); (11, 12); (8, 9); (2, 9); (1, 9)]
[(11, 11); (10, 10); (8, 10); (5, 13); (1, 13)]
[(11, 11); (10, 11); (7, 8); (4, 11); (1, 14)]
[(11, 11); (11, 12); (8, 9); (2, 15); (1, 15)]
[(11, 11); (11, 12); (8, 9); (2, 15); (1, 16)]
[(11, 11); (10, 10); (8, 10); (5, 7); (2, 4)]
[(11, 11); (10, 11); (7, 8); (7, 5); (2, 5)]
[(11, 11); (11, 12); (8, 15); (9, 16); (2, 16)]
[(11, 11); (12, 10); (11, 9); (9, 9); (3, 3)]
[(11, 11); (10, 11); (7, 8); (4, 5); (3, 4)]
[(11, 11); (12, 11); (12, 14); (8, 18); (3, 18)]
[(11, 11); (12, 11); (9, 14); (6, 17); (4, 19)]
[(11, 11); (11, 12); (8, 9); (8, 3); (6, 1)]
[(11, 11); (12, 11); (12, 8); (8, 4); (6, 2)]
[(11, 11); (11, 12); (11, 15); (11, 17); (7, 21)]
[(11, 11); (11, 12); (8, 9); (8, 3); (8, 1)]
[(11, 11); (12, 11); (12, 8); (12, 4); (9, 1)]
[(11, 11); (11, 12); (8, 9); (14, 3); (11, 0)]
[(11, 11); (10, 11); (7, 8); (7, 5); (12, 0)]
[(11, 11); (12, 10); (13, 10); (13, 5); (13, 0)]
[(11, 11); (12, 11); (12, 8); (16, 4); (13, 1)]
[(11, 11); (12, 11); (12, 14); (16, 18); (13, 21)]
[(11, 11); (12, 11); (12, 8); (12, 4); (15, 1)]
[(11, 11); (11, 12); (11, 15); (11, 17); (15, 21)]
[(11, 11); (12, 11); (12, 8); (16, 4); (16, 1)]
[(11, 11); (10, 11); (10, 14); (12, 16); (16, 20)]
[(11, 11); (12, 11); (12, 14); (16, 18); (16, 21)]
[(11, 11); (12, 11); (15, 8); (15, 5); (18, 2)]
[(11, 11); (10, 11); (13, 8); (14, 7); (18, 3)]
[(11, 11); (12, 11); (15, 8); (18, 5); (19, 4)]
[(11, 11); (11, 12); (14, 15); (16, 15); (19, 18)]
[(11, 11); (12, 11); (15, 11); (16, 12); (20, 16)]
[(11, 11); (10, 11); (13, 11); (17, 15); (20, 18)]
[(11, 11); (12, 10); (13, 10); (18, 15); (21, 15)]
[(11, 11); (11, 12); (14, 9); (18, 13); (22, 9)]
[(11, 11); (12, 11); (15, 8); (18, 11); (22, 11)]
[(11, 11); (11, 12); (14, 9); (18, 13); (22, 13)]

```


### Part 2

This task uses [[Floyd-Warshall algorithm#F.23]]

```fsharp

let board=readCSV '\t' "gmooh.dat"|>Seq.choose(fun n->match n.value with |"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" as g->Some((n.row,n.col),int g)|_->None)|>Map.ofSeq
let nodes=board|>Seq.map(fun n->n.Key)|>Set.ofSeq
let adjacent (n,g) v=List.choose(fun y->if y=(n,g) then None else match Set.contains y nodes with |true->Some ((y),1)|_->None) [(n+v,g);(n-v,g);(n,g+v);(n,g-v);(n+v,g+v);(n+v,g-v);(n-v,g+v);(n-v,g-v);]
let adjacencyList=board |>Seq.collect (fun n->Seq.map(fun ((n'),g')->((n.Key,n'),g'))(adjacent n.Key n.Value))|> Map.ofSeq
let _,paths=Floyd (nodes|>Set.toArray) adjacencyList
paths (21,11) (1,11) |>Seq.iteri(fun n g->if n>0 then printf "->"; printf "%A" g else printf "%A" g) ; printfn ""
paths (1,11) (21,11) |>Seq.iteri(fun n g->if n>0 then printf "->"; printf "%A" g else printf "%A" g) ; printfn ""

```

```txt

(20, 10)->(19, 9)->(18, 9)->(13, 4)->(6, 11)->(4, 11)->(1, 11)
(2, 10)->(5, 13)->(9, 9)->(15, 3)->(20, 8)->(20, 10)->(21, 11)

```



## Go

A more or less faithful translation though adjusted to Go's 0-based indices and the cell coordinates are therefore 1 less than the Phix results.

Initially, using a simple breadth-first search. Parts 1 and 2 and extra credit. 

```go
package main

import (
    "fmt"
    "strings"
)

var gmooh = strings.Split(
    `.........00000.........
......00003130000......
....000321322221000....
...00231222432132200...
..0041433223233211100..
..0232231612142618530..
.003152122326114121200.
.031252235216111132210.
.022211246332311115210.
00113232262121317213200
03152118212313211411110
03231234121132221411410
03513213411311414112320
00427534125412213211400
.013322444412122123210.
.015132331312411123120.
.003333612214233913300.
..0219126511415312570..
..0021321524341325100..
...00211415413523200...
....000122111322000....
......00001120000......
.........00000.........`, "\n")

var width, height = len(gmooh[0]), len(gmooh)

type pyx [2]int // {y, x}

var d = []pyx{{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1, 0}, {-1, 1}, {0, 1}, {1, 1}}

type route [3]int // {cost, fromy, fromx}

var zeroRoute = route{0, 0, 0}
var routes [][]route // route for each gmooh[][]

func (p pyx) destruct() (int, int) {
    return p[0], p[1]
}

func (r route) destruct() (int, int, int) {
    return r[0], r[1], r[2]
}

func search(y, x int) {
    // Simple breadth-first search, populates routes.
    // This isn't strictly Dijkstra because graph edges are not weighted.
    cost := 0
    routes = make([][]route, height)
    for i := 0; i < width; i++ {
        routes[i] = make([]route, width)
    }
    routes[y][x] = route{0, y, x} // zero-cost, the starting point
    var next []route
    for {
        n := int(gmooh[y][x] - '0')
        for di := 0; di < len(d); di++ {
            dx, dy := d[di].destruct()
            rx, ry := x+n*dx, y+n*dy
            if rx >= 0 && rx < width && ry >= 0 && ry < height && gmooh[rx][ry] >= '0' {
                ryx := routes[ry][rx]
                if ryx == zeroRoute || ryx[0] > cost+1 {
                    routes[ry][rx] = route{cost + 1, y, x}
                    if gmooh[ry][rx] > '0' {
                        next = append(next, route{cost + 1, ry, rx})
                        // If the graph was weighted, at this point
                        // that would get shuffled up into place.
                    }
                }
            }
        }
        if len(next) == 0 {
            break
        }
        cost, y, x = next[0].destruct()
        next = next[1:]
    }
}

func getRoute(y, x int) []pyx {
    cost := 0
    res := []pyx{{y, x}}
    for {
        cost, y, x = routes[y][x].destruct()
        if cost == 0 {
            break
        }
        res = append(res, pyx{0, 0})
        copy(res[1:], res[0:])
        res[0] = pyx{y, x}
    }
    return res
}

func showShortest() {
    shortest := 9999
    var res []pyx
    for x := 0; x < width; x++ {
        for y := 0; y < height; y++ {
            if gmooh[y][x] == '0' {
                ryx := routes[y][x]
                if ryx != zeroRoute {
                    cost := ryx[0]
                    if cost <= shortest {
                        if cost < shortest {
                            res = res[:0]
                            shortest = cost
                        }
                        res = append(res, pyx{y, x})
                    }
                }
            }
        }
    }
    areis, s := "is", ""
    if len(res) > 1 {
        areis = "are"
        s = "s"
    }
    fmt.Printf("There %s %d shortest route%s of %d days to safety:\n", areis, len(res), s, shortest)
    for _, r := range res {
        fmt.Println(getRoute(r[0], r[1]))
    }
}

func showUnreachable() {
    var res []pyx
    for x := 0; x < width; x++ {
        for y := 0; y < height; y++ {
            if gmooh[y][x] >= '0' && routes[y][x] == zeroRoute {
                res = append(res, pyx{y, x})
            }
        }
    }
    fmt.Println("\nThe following cells are unreachable:")
    fmt.Println(res)
}

func showLongest() {
    longest := 0
    var res []pyx
    for x := 0; x < width; x++ {
        for y := 0; y < height; y++ {
            if gmooh[y][x] >= '0' {
                ryx := routes[y][x]
                if ryx != zeroRoute {
                    rl := ryx[0]
                    if rl >= longest {
                        if rl > longest {
                            res = res[:0]
                            longest = rl
                        }
                        res = append(res, pyx{y, x})
                    }
                }
            }
        }
    }
    fmt.Printf("\nThere are %d cells that take %d days to send reinforcements to:\n", len(res), longest)
    for _, r := range res {
        fmt.Println(getRoute(r[0], r[1]))
    }
}

func main() {
    search(11, 11)
    showShortest()

    search(21, 11)
    fmt.Println("\nThe shortest route from {21,11} to {1,11}:")
    fmt.Println(getRoute(1, 11))

    search(1, 11)
    fmt.Println("\nThe shortest route from {1,11} to {21,11}:")
    fmt.Println(getRoute(21, 11))

    search(11, 11)
    showUnreachable()
    showLongest()
}
```


```txt

There are 40 shortest routes of 4 days to safety:
[[11 11] [11 12] [8 9] [14 3] [11 0]]
[[11 11] [10 11] [7 8] [7 5] [12 0]]
[[11 11] [12 10] [13 10] [13 5] [13 0]]
[[11 11] [11 12] [8 9] [8 3] [6 1]]
[[11 11] [11 12] [8 9] [8 3] [8 1]]
[[11 11] [10 10] [8 8] [12 4] [9 1]]
[[11 11] [10 10] [12 8] [16 4] [13 1]]
[[11 11] [10 10] [8 8] [12 4] [15 1]]
[[11 11] [10 10] [12 8] [16 4] [16 1]]
[[11 11] [10 10] [8 8] [8 4] [6 2]]
[[11 11] [12 11] [15 8] [15 5] [18 2]]
[[11 11] [11 10] [10 9] [9 9] [3 3]]
[[11 11] [10 11] [13 8] [14 7] [18 3]]
[[11 11] [10 10] [8 10] [5 7] [2 4]]
[[11 11] [10 11] [7 8] [4 5] [3 4]]
[[11 11] [10 10] [12 8] [16 4] [19 4]]
[[11 11] [10 11] [7 8] [7 5] [2 5]]
[[11 11] [10 11] [7 11] [7 12] [1 6]]
[[11 11] [10 10] [8 8] [4 8] [1 8]]
[[11 11] [10 10] [8 10] [5 13] [1 9]]
[[11 11] [11 12] [14 9] [18 13] [22 9]]
[[11 11] [12 11] [15 8] [18 11] [22 11]]
[[11 11] [10 10] [8 12] [6 12] [0 12]]
[[11 11] [10 10] [8 10] [5 13] [1 13]]
[[11 11] [11 12] [14 9] [18 13] [22 13]]
[[11 11] [10 11] [7 8] [4 11] [1 14]]
[[11 11] [11 12] [8 9] [2 15] [1 15]]
[[11 11] [12 10] [13 10] [18 15] [21 15]]
[[11 11] [11 12] [8 9] [2 15] [1 16]]
[[11 11] [11 12] [8 9] [2 15] [2 16]]
[[11 11] [10 10] [12 8] [16 12] [20 16]]
[[11 11] [12 11] [12 14] [8 18] [3 18]]
[[11 11] [11 12] [14 15] [16 15] [19 18]]
[[11 11] [10 11] [13 11] [17 15] [20 18]]
[[11 11] [12 11] [9 14] [6 17] [4 19]]
[[11 11] [10 11] [10 14] [12 16] [16 20]]
[[11 11] [11 12] [11 15] [11 17] [7 21]]
[[11 11] [12 11] [12 14] [16 18] [13 21]]
[[11 11] [11 12] [11 15] [11 17] [15 21]]
[[11 11] [12 11] [12 14] [16 18] [16 21]]

The shortest route from {21,11} to {1,11}:
[[21 11] [20 10] [19 9] [18 9] [13 4] [6 11] [4 11] [1 11]]

The shortest route from {1,11} to {21,11}:
[[1 11] [2 10] [5 13] [9 9] [15 3] [20 8] [20 10] [21 11]]

The following cells are unreachable:
[[4 3] [2 18] [18 20]]

There are 5 cells that take 6 days to send reinforcements to:
[[11 11] [10 10] [12 8] [16 12] [20 12] [21 11] [22 12]]
[[11 11] [11 12] [14 15] [16 17] [17 16] [18 16] [20 14]]
[[11 11] [12 11] [9 14] [6 17] [4 17] [3 17] [3 19]]
[[11 11] [10 11] [7 11] [7 12] [7 18] [7 20] [6 20]]
[[11 11] [10 11] [10 14] [12 16] [12 20] [15 20] [17 20]]

```


Alternative using Floyd-Warshall for Part 2, and finding the longest shortest path between any two points.

```go
package main

import (
    "fmt"
    "math"
    "strings"
)

var gmooh = strings.Split(
    `.........00000.........
......00003130000......
....000321322221000....
...00231222432132200...
..0041433223233211100..
..0232231612142618530..
.003152122326114121200.
.031252235216111132210.
.022211246332311115210.
00113232262121317213200
03152118212313211411110
03231234121132221411410
03513213411311414112320
00427534125412213211400
.013322444412122123210.
.015132331312411123120.
.003333612214233913300.
..0219126511415312570..
..0021321524341325100..
...00211415413523200...
....000122111322000....
......00001120000......
.........00000.........`, "\n")

var width, height = len(gmooh[0]), len(gmooh)

type pyx [2]int // {y, x}

var d = []pyx{{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1, 0}, {-1, 1}, {0, 1}, {1, 1}}

var dist, next [][]int
var pmap []pyx

const (
    max = math.MaxInt32
    min = -1
)

func (p pyx) destruct() (int, int) {
    return p[0], p[1]
}

func fwPath(u, v int) string {
    res := ""
    if next[u][v] != min {
        path := []string{fmt.Sprintf("%v", pmap[u])}
        for u != v {
            u = next[u][v]
            path = append(path, fmt.Sprintf("%v", pmap[u]))
        }
        res = strings.Join(path, "->")
    }
    return res
}

func showFwPath(u, v int) {
    fmt.Printf("%v->%v   %2d   %s\n", pmap[u], pmap[v], dist[u][v], fwPath(u, v))
}

func floydWarshall() {
    point := 0
    var weights []pyx
    points := make([][]int, height)
    for i := 0; i < width; i++ {
        points[i] = make([]int, width)
    }
    // First number the points.
    for x := 0; x < width; x++ {
        for y := 0; y < height; y++ {
            if gmooh[y][x] >= '0' {
                points[y][x] = point
                point++
                pmap = append(pmap, pyx{y, x})
            }
        }
    }
    // ...and then a set of edges (all of which have a "weight" of 1 day)
    for x := 0; x < width; x++ {
        for y := 0; y < height; y++ {
            if gmooh[y][x] > '0' {
                n := int(gmooh[y][x] - '0')
                for di := 0; di < len(d); di++ {
                    dx, dy := d[di].destruct()
                    rx, ry := x+n*dx, y+n*dy
                    if rx >= 0 && rx < width && ry >= 0 && ry < height && gmooh[rx][ry] >= '0' {
                        weights = append(weights, pyx{points[y][x], points[ry][rx]})
                    }
                }
            }
        }
    }
    // Before applying Floyd-Warshall.
    vv := len(pmap)
    dist = make([][]int, vv)
    next = make([][]int, vv)
    for i := 0; i < vv; i++ {
        dist[i] = make([]int, vv)
        next[i] = make([]int, vv)
        for j := 0; j < vv; j++ {
            dist[i][j] = max
            next[i][j] = min
        }
    }
    for k := 0; k < len(weights); k++ {
        u, v := weights[k].destruct()
        dist[u][v] = 1 // the weight of the edge (u,v)
        next[u][v] = v
    }
    // Standard Floyd-Warshall implementation,
    // with the optimization of avoiding processing of self/infs,
    // which surprisingly makes quite a noticeable difference.
    for k := 0; k < vv; k++ {
        for i := 0; i < vv; i++ {
            if i != k && dist[i][k] != max {
                for j := 0; j < vv; j++ {
                    if j != i && j != k && dist[k][j] != max {
                        dd := dist[i][k] + dist[k][j]
                        if dd < dist[i][j] {
                            dist[i][j] = dd
                            next[i][j] = next[i][k]
                        }
                    }
                }
            }
        }
    }
    showFwPath(points[21][11], points[1][11])
    showFwPath(points[1][11], points[21][11])

    var maxd, mi, mj int
    for i := 0; i < vv; i++ {
        for j := 0; j < vv; j++ {
            if j != i {
                dd := dist[i][j]
                if dd != max && dd > maxd {
                    maxd, mi, mj = dd, i, j
                }
            }
        }
    }
    fmt.Println("\nMaximum shortest distance:")
    showFwPath(mi, mj)
}

func main() {
    floydWarshall()
}
```


```txt

[21 11]->[1 11]    7   [21 11]->[20 10]->[19 10]->[14 10]->[10 10]->[8 8]->[4 8]->[1 11]
[1 11]->[21 11]    7   [1 11]->[2 10]->[5 13]->[9 9]->[15 3]->[20 8]->[20 10]->[21 11]

Maximum shortest distance:
[7 3]->[20 14]    9   [7 3]->[8 4]->[10 6]->[11 7]->[15 11]->[16 11]->[17 12]->[17 16]->[18 16]->[20 14]

```



## Julia

Uses the LightGraphs package.

```julia

using LightGraphs

const grid = reshape(Vector{UInt8}(replace("""
         00000         
      00003130000      
    000321322221000    
   00231222432132200   
  0041433223233211100  
  0232231612142618530  
 003152122326114121200 
 031252235216111132210 
 022211246332311115210 
00113232262121317213200
03152118212313211411110
03231234121132221411410
03513213411311414112320
00427534125412213211400
 013322444412122123210 
 015132331312411123120 
 003333612214233913300 
  0219126511415312570  
  0021321524341325100  
   00211415413523200   
    000122111322000    
      00001120000      
         00000         """, "\n" => "")), 23, 23)

const board = map(c -> c == UInt8(' ') ? -1 : c - UInt8('0'), grid)
const startingpoints = [i for i in 1:529 if board[i] > 0]
const safety = [i for i in 1:529 if board[i] == 0]
const legalendpoints = [i for i in 1:529 if board[i] >= 0]

function adjacent(i)
    k, ret = board[i], Int[]
    row, col = divrem(i - 1, 23) .+ 1
    col > k && push!(ret, i - k)
    23 - col >= k && push!(ret, i + k)
    row > k && push!(ret, i - 23 * k)
    row + k <= 23 && push!(ret, i + 23 * k)
    row > k && col > k && push!(ret, i - 24 * k)
    row + k <= 23 && (23 - col >= k) && push!(ret, i + 24 * k)
    row > k && (23 - col >= k) && push!(ret, i - 22 * k)
    row + k <= 23 && col > k && push!(ret, i + 22 * k)
    ret
end

const graph = SimpleDiGraph(529)

for i in 1:529
    if board[i] > 0
        for p in adjacent(i)
            if board[p] >= 0
                add_edge!(graph, i, p)
            end
        end
    end
end

"""
    allnpaths(graph, a, b, vec, n)

Return a vector of int vectors, each of which is a path from a to a member of
vec and where n is the length of each path and the nodes in a path do not repeat.
"""
function allnpaths(graph, a, vec, n)
    ret = [[a]]
    for j in 2:n
        nextret = Vector{Vector{Int}}()
        for path in ret, x in neighbors(graph, path[end])
            if !(x in path) && (j < n || x in vec)
                push!(nextret, [path; x])
            end
        end
        ret = nextret
    end
    return (ret == [[a]] && a != b) ? [] : ret
end

function pathtostring(path)
    ret = ""
    for node in path
        c = CartesianIndices(board)[node]
        ret *= "($(c[2]-1), $(c[1]-1)) "
    end
    ret
end

function pathlisting(paths)
    join([pathtostring(p) for p in paths], "\n")
end

println("Part 1:")
let
    start = 23 * 11 + 12
    pathsfromcenter = dijkstra_shortest_paths(graph, start)
    safepaths = filter(p -> length(p) > 1, enumerate_paths(pathsfromcenter, safety))
    safelen = mapreduce(length, min, safepaths)
    paths = unique(allnpaths(graph, start, safety, safelen))
    println("The $(length(paths)) shortest paths to safety are:\n",
        pathlisting(paths))
end

println("\nPart 2:")
let
    p = enumerate_paths(bellman_ford_shortest_paths(graph, 21 * 23 + 12), 23 + 12)
    println("One shortest route from (21, 11) to (1, 11): ", pathtostring(p))

    p = enumerate_paths(bellman_ford_shortest_paths(graph, 23 + 12), 21 * 23 + 12)
    println("\nOne shortest route from (1, 11) to (21, 11): ", pathtostring(p))

    allshortpaths = [enumerate_paths(bellman_ford_shortest_paths(graph, 23 + 12), p) for p in startingpoints]
    maxlen, idx = findmax(map(length, allshortpaths))
    println("\nLongest Shortest Route (length $(maxlen - 1)) is: ", pathtostring(allshortpaths[idx]))
end

println("\nExtra Credit Questions:")
let
    println("\nIs there any cell in the country that can not be reached from HQ (11, 11)?")
    frombase = bellman_ford_shortest_paths(graph, 11 * 23 + 12)
    unreached = Int[]
    for pt in legalendpoints
        path = enumerate_paths(frombase, pt)
        if isempty(path) && pt != 11 * 23 + 12
            push!(unreached, pt)
        end
    end
    print("There are $(length(unreached)): ")
    println(pathtostring(unreached))

    println("\nWhich cells will it take longest to send reinforcements to from HQ (11, 11)?")
    p = [enumerate_paths(frombase, x) for x in legalendpoints]
    maxlen = mapreduce(length, max, p)
    allmax = [path for path in p if length(path) == maxlen]
    println("There are $(length(allmax)) of length $(maxlen - 1):")
    println(pathlisting(allmax))
end

```
```txt

Part 1:
The 71 shortest paths to safety are:
(11, 11) (10, 10) (8, 8) (4, 8) (1, 8)
(11, 11) (10, 10) (8, 8) (8, 4) (6, 2)
(11, 11) (10, 10) (8, 8) (12, 4) (9, 1)
(11, 11) (10, 10) (8, 8) (12, 4) (15, 1)
(11, 11) (10, 10) (8, 10) (5, 7) (2, 4)
(11, 11) (10, 10) (8, 10) (5, 13) (1, 9)
(11, 11) (10, 10) (8, 10) (5, 13) (1, 13)
(11, 11) (10, 10) (8, 12) (6, 12) (0, 12)
(11, 11) (10, 10) (12, 8) (8, 4) (6, 2)
(11, 11) (10, 10) (12, 8) (12, 4) (9, 1)
(11, 11) (10, 10) (12, 8) (12, 4) (15, 1)
(11, 11) (10, 10) (12, 8) (16, 4) (13, 1)
(11, 11) (10, 10) (12, 8) (16, 4) (16, 1)
(11, 11) (10, 10) (12, 8) (16, 4) (19, 4)
(11, 11) (10, 10) (12, 8) (16, 12) (20, 16)
(11, 11) (10, 11) (7, 8) (4, 5) (3, 4)
(11, 11) (10, 11) (7, 8) (4, 8) (1, 8)
(11, 11) (10, 11) (7, 8) (4, 11) (1, 8)
(11, 11) (10, 11) (7, 8) (4, 11) (1, 14)
(11, 11) (10, 11) (7, 8) (7, 5) (2, 5)
(11, 11) (10, 11) (7, 8) (7, 5) (12, 0)
(11, 11) (10, 11) (7, 11) (6, 12) (0, 12)
(11, 11) (10, 11) (7, 11) (7, 12) (1, 6)
(11, 11) (10, 11) (10, 14) (12, 16) (16, 20)
(11, 11) (10, 11) (13, 8) (14, 7) (18, 3)
(11, 11) (10, 11) (13, 11) (17, 15) (20, 18)
(11, 11) (10, 12) (9, 12) (7, 12) (1, 6)
(11, 11) (11, 10) (10, 9) (9, 9) (3, 3)
(11, 11) (11, 10) (11, 9) (9, 9) (3, 3)
(11, 11) (11, 12) (8, 9) (2, 9) (1, 8)
(11, 11) (11, 12) (8, 9) (2, 9) (1, 9)
(11, 11) (11, 12) (8, 9) (2, 15) (1, 14)
(11, 11) (11, 12) (8, 9) (2, 15) (1, 15)
(11, 11) (11, 12) (8, 9) (2, 15) (1, 16)
(11, 11) (11, 12) (8, 9) (2, 15) (2, 16)
(11, 11) (11, 12) (8, 9) (8, 3) (6, 1)
(11, 11) (11, 12) (8, 9) (8, 3) (8, 1)
(11, 11) (11, 12) (8, 9) (14, 3) (11, 0)
(11, 11) (11, 12) (8, 12) (6, 12) (0, 12)
(11, 11) (11, 12) (8, 15) (9, 16) (2, 16)
(11, 11) (11, 12) (11, 9) (9, 9) (3, 3)
(11, 11) (11, 12) (11, 15) (11, 17) (7, 21)
(11, 11) (11, 12) (11, 15) (11, 17) (15, 21)
(11, 11) (11, 12) (14, 9) (18, 5) (19, 4)
(11, 11) (11, 12) (14, 9) (18, 13) (22, 9)
(11, 11) (11, 12) (14, 9) (18, 13) (22, 13)
(11, 11) (11, 12) (14, 12) (16, 12) (20, 16)
(11, 11) (11, 12) (14, 15) (16, 15) (19, 18)
(11, 11) (12, 10) (11, 9) (9, 9) (3, 3)
(11, 11) (12, 10) (13, 10) (13, 5) (13, 0)
(11, 11) (12, 10) (13, 10) (18, 5) (19, 4)
(11, 11) (12, 10) (13, 10) (18, 15) (21, 15)
(11, 11) (12, 10) (13, 11) (17, 15) (20, 18)
(11, 11) (12, 11) (9, 14) (6, 17) (4, 19)
(11, 11) (12, 11) (12, 8) (8, 4) (6, 2)
(11, 11) (12, 11) (12, 8) (12, 4) (9, 1)
(11, 11) (12, 11) (12, 8) (12, 4) (15, 1)
(11, 11) (12, 11) (12, 8) (16, 4) (13, 1)
(11, 11) (12, 11) (12, 8) (16, 4) (16, 1)
(11, 11) (12, 11) (12, 8) (16, 4) (19, 4)
(11, 11) (12, 11) (12, 8) (16, 12) (20, 16)
(11, 11) (12, 11) (12, 14) (8, 18) (3, 18)
(11, 11) (12, 11) (12, 14) (16, 18) (13, 21)
(11, 11) (12, 11) (12, 14) (16, 18) (16, 21)
(11, 11) (12, 11) (12, 14) (16, 18) (19, 18)
(11, 11) (12, 11) (15, 8) (15, 5) (18, 2)
(11, 11) (12, 11) (15, 8) (18, 5) (19, 4)
(11, 11) (12, 11) (15, 8) (18, 11) (22, 11)
(11, 11) (12, 11) (15, 11) (16, 12) (20, 16)
(11, 11) (12, 11) (15, 14) (16, 15) (19, 18)
(11, 11) (12, 12) (13, 11) (17, 15) (20, 18)

Part 2:
One shortest route from (21, 11) to (1, 11): (21, 11) (21, 12) (19, 14) (14, 14) (12, 14) (8, 18) (3, 13) (1, 11)

One shortest route from (1, 11) to (21, 11): (1, 11) (2, 10) (5, 13) (9, 9) (15, 3) (20, 8) (20, 10) (21, 11)

Longest Shortest Route (length 9) is: (1, 11) (2, 10) (5, 13) (9, 9) (15, 15) (16, 14) (16, 17) (17, 16) (18, 16) (20, 14)

Extra Credit Questions:

Is there any cell in the country that can not be reached from HQ (11, 11)?
There are 3: (2, 18) (4, 3) (18, 20)

Which cells will it take longest to send reinforcements to from HQ (11, 11)?
There are 5 of length 6:
(11, 11) (12, 11) (9, 14) (6, 17) (4, 17) (4, 18) (3, 19)
(11, 11) (10, 11) (7, 11) (7, 12) (7, 18) (7, 20) (6, 20)
(11, 11) (11, 12) (11, 15) (13, 17) (15, 19) (15, 20) (17, 20)
(11, 11) (11, 12) (14, 15) (16, 17) (17, 16) (18, 16) (20, 14)
(11, 11) (12, 12) (13, 11) (17, 15) (20, 12) (21, 11) (22, 12)

```



## Phix

Using a simple breadth-first search. Parts 1 and 2 and extra credit.

```Phix
constant gmooh = split("""
.........00000.........
......00003130000......
....000321322221000....
...00231222432132200...
..0041433223233211100..
..0232231612142618530..
.003152122326114121200.
.031252235216111132210.
.022211246332311115210.
00113232262121317213200
03152118212313211411110
03231234121132221411410
03513213411311414112320
00427534125412213211400
.013322444412122123210.
.015132331312411123120.
.003333612214233913300.
..0219126511415312570..
..0021321524341325100..
...00211415413523200...
....000122111322000....
......00001120000......
.........00000.........""",'\n')
 
constant width = length(gmooh[1]),
         height = length(gmooh),
         d = {{-1,-1},{0,-1},{+1,-1},
              {-1, 0},       {+1, 0},
              {-1,+1},{0,+1},{+1,+1}}
 
sequence routes -- {cost,fromy,fromx} for each gmooh[][].
 
procedure search(integer y, x)
-- simple breadth-first search, populates routes
-- (this isn't strictly dijkstra, because graph edges are not weighted)
integer cost = 0
sequence route = {{y,x}},
         next = {}
    routes = repeat(repeat(0,width),height)
    routes[y,x] = {0,y,x} -- zero-cost the starting point
    while 1 do
        integer n = gmooh[y,x]-'0'
        for di=1 to length(d) do
            integer {dx,dy} = d[di]
            integer {rx,ry} = {x+n*dx,y+n*dy}
            if rx>=1 and rx<=width 
            and ry>=1 and ry<=height
            and gmooh[ry,rx]>='0' then
                object ryx = routes[ry,rx]
                if ryx=0
                or ryx[1]>cost+1 then
                    routes[ry,rx] = {cost+1,y,x}
                    if gmooh[ry,rx]>'0' then
                        next = append(next,{cost+1,ry,rx})
                        -- (if the graph was weighted, at this point
                        --   that would get shuffled up into place.)
                    end if
                end if
            end if
        end for
        if length(next)=0 then exit end if
        {cost,y,x} = next[1]
        next = next[2..$]
    end while
end procedure

function get_route(sequence yx)
integer {y,x} = yx, cost
sequence res = {{y,x}}
    while 1 do
        {cost,y,x} = routes[y,x]
        if cost=0 then exit end if
        res = prepend(res,{y,x})
    end while
    return res
end function

procedure show_shortest_routes_to_safety()
integer shortest = 9999
sequence res = {}
    for x=1 to width do
        for y=1 to height do
            if gmooh[y,x]='0' then
                object ryx = routes[y,x]
                if ryx!=0 then
                    integer cost = ryx[1]
                    if cost<=shortest then
                        if cost<shortest then
                            res = {}
                            shortest = cost
                        end if
                        res = append(res,{y,x})
                    end if
                end if
            end if
        end for
    end for
    string {areis,s} = iff(length(res)>1?{"are","s"}:{"is",""})
    printf(1,"There %s %d shortest route%s of %d days to safety:\n",{areis,length(res),s,shortest})
    for i=1 to length(res) do
        ?get_route(res[i])
    end for
end procedure

procedure show_unreachable()
sequence res = {}
    for x=1 to width do
        for y=1 to height do
            if gmooh[y,x]>='0'
            and routes[y,x]=0 then
                res = append(res,{y,x})
            end if
        end for
    end for
    puts(1,"The following cells are unreachable:\n")
    ?res
end procedure
 
procedure show_longest()
integer longest = 0
sequence res = {}
    for x=1 to width do
        for y=1 to height do
            if gmooh[y,x]>='0' then
                object ryx = routes[y,x]
                if ryx!=0 then
                    integer rl = ryx[1]
                    if rl>=longest then
                        if rl>longest then
                            res = {}
                            longest = rl
                        end if
                        res  = append(res,{y,x})
                    end if
                end if
            end if
        end for
    end for
    printf(1,"There are %d cells that take %d days to send reinforcements to\n",{length(res),longest})
    for i=1 to length(res) do
        ?get_route(res[i])
    end for
end procedure

procedure main()
    search(12,12)
    show_shortest_routes_to_safety()
 
    -- see also below
    search(22,12)
    puts(1,"The shortest route from 22,12 to 2,12:\n")
    ?get_route({2,12})

    search(2,12)
    puts(1,"The shortest route from 2,12 to 22,12:\n")
    ?get_route({22,12})
 
    search(12,12)
    -- </see also below>

    show_unreachable()
    show_longest()

end procedure
main()
```

Note: Phix indexes are 1-based and therefore so too are these results.

```txt

There are 40 shortest routes of 4 days to safety:
The shortest route from 22,12 to 2,12:
The shortest route from 2,12 to 22,12:
The following cells are unreachable:
There are 5 cells that take 6 days to send reinforcements to
```

Alternative using Floyd-Warshall for Part 2, and finding the longest shortest path between any two points.

```Phix
--(same constants as above: gmooh, width, height, d)
constant inf = 1e300*1e300

sequence dist, next, pmap = {}
 
function fw_path(integer u, v)
sequence res = {}
    if next[u,v]!=null then
        sequence path = {sprintf("{%d,%d}",pmap[u])}
        while u!=v do
           u = next[u,v]
           path = append(path,sprintf("{%d,%d}",pmap[u]))
        end while
        res = join(path,"->")
    end if
    return res
end function

procedure show_fw_path(integer u, v)
    printf(1,"{%d,%d}->{%d,%d}   %2d   %s\n",pmap[u]&pmap[v]&{dist[u,v],fw_path(u,v)})
end procedure
 
procedure FloydWarshall()
integer point = 0
sequence weights = {},
         points = repeat(repeat(0,width),height)
    -- First number the points...
    for x=1 to width do
        for y=1 to height do
            if gmooh[y,x]>='0' then
                point += 1
                points[y,x] = point
                pmap = append(pmap,{y,x})
            end if
        end for
    end for
    -- ...and then a set of edges (all of which have a "weight" of 1 day)
    for x=1 to width do
        for y=1 to height do
            if gmooh[y,x]>'0' then
                integer n = gmooh[y,x]-'0'
                for di=1 to length(d) do
                    integer {dx,dy} = d[di]
                    integer {rx,ry} = {x+n*dx,y+n*dy}
                    if rx>=1 and rx<=width 
                    and ry>=1 and ry<=height
                    and gmooh[ry,rx]>='0' then
--                      weights = append(weights,{points[y,x],points[ry,rx],1})
                        weights = append(weights,{points[y,x],points[ry,rx]})
                    end if
                end for
            end if
        end for
    end for
    -- Before applying Floyd-Warshall
    integer V = length(pmap)
    dist = repeat(repeat(inf,V),V)
    next = repeat(repeat(null,V),V)
    for k=1 to length(weights) do
--      integer {u,v,w} = weights[k]
        integer {u,v} = weights[k]
--      dist[u,v] := w  -- the weight of the edge (u,v)
        dist[u,v] := 1  -- the weight of the edge (u,v)
        next[u,v] := v
    end for
    -- standard Floyd-Warshall implementation,
    -- with the optimisation of avoiding processing of self/infs,
    -- which surprisingly makes quite a noticeable difference.
    for k=1 to V do
        for i=1 to V do
            if i!=k and dist[i,k]!=inf then
                for j=1 to V do
                    if j!=i and j!=k and dist[k,j]!=inf then
                        atom d = dist[i,k] + dist[k,j]
                        if d<dist[i,j] then
                            dist[i,j] := d
                            next[i,j] := next[i,k]
                        end if
                    end if
                end for
            end if
        end for
    end for
    show_fw_path(points[22,12],points[2,12])
    show_fw_path(points[2,12],points[22,12])

    integer maxd = 0, mi, mj
    for i=1 to V do
        for j=1 to V do
            if j!=i then
                atom d = dist[i,j]
                if d!=inf and d>maxd then
                    {maxd,mi,mj} = {d,i,j}
                end if
            end if
        end for
    end for
    printf(1,"Maximum shortest distance:\n") 
    show_fw_path(mi,mj)
    
end procedure   

FloydWarshall()
```

```txt

{22,12}->{2,12}    7   {22,12}->{21,11}->{20,11}->{15,11}->{11,11}->{9,9}->{5,9}->{2,12}
{2,12}->{22,12}    7   {2,12}->{3,11}->{6,14}->{10,10}->{16,4}->{21,9}->{21,11}->{22,12}
Maximum shortest distance:
{8,4}->{21,15}   9   {8,4}->{9,5}->{11,7}->{12,8}->{16,12}->{17,12}->{18,13}->{18,17}->{19,17}->{21,15}

```

