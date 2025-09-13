+++
title = "Nonogram solver"
description = ""
date = 2019-10-20T15:33:14Z
aliases = []
[extra]
id = 16746
[taxonomies]
categories = ["task"]
tags = []
+++

{{task}} [[Category:Puzzles]]
A [[wp:Nonogram|nonogram]] is a puzzle that provides
numeric clues used to fill in a grid of cells,
establishing for each cell whether it is filled or not.
The puzzle solution is typically a picture of some kind.

Each row and column of a rectangular grid is annotated with the lengths
of its distinct runs of occupied cells.
Using only these lengths you should find one valid configuration
of empty and occupied cells, or show a failure message.

;Example

```txt
Problem:                 Solution:

. . . . . . . .  3       . # # # . . . .  3
. . . . . . . .  2 1     # # . # . . . .  2 1
. . . . . . . .  3 2     . # # # . . # #  3 2
. . . . . . . .  2 2     . . # # . . # #  2 2
. . . . . . . .  6       . . # # # # # #  6
. . . . . . . .  1 5     # . # # # # # .  1 5
. . . . . . . .  6       # # # # # # . .  6
. . . . . . . .  1       . . . . # . . .  1
. . . . . . . .  2       . . . # # . . .  2
1 3 1 7 5 3 4 3          1 3 1 7 5 3 4 3
2 1 5 1                  2 1 5 1
```

The problem above could be represented by two lists of lists:

```txt
x = [[3], [2,1], [3,2], [2,2], [6], [1,5], [6], [1], [2]]
y = [[1,2], [3,1], [1,5], [7,1], [5], [3], [4], [3]]
```

A more compact representation of the same problem uses strings,
where the letters represent the numbers, A=1, B=2, etc:

```txt
x = "C BA CB BB F AE F A B"
y = "AB CA AE GA E C D C"
```


## Task

For this task, try to solve the 4 problems below, read from a “<tt>nonogram_problems.txt</tt>” file that has this content
(the blank lines are separators):

```txt
C BA CB BB F AE F A B
AB CA AE GA E C D C

F CAC ACAC CN AAA AABB EBB EAA ECCC HCCC
D D AE CD AE A DA BBB CC AAB BAA AAB DA AAB AAA BAB AAA CD BBA DA

CA BDA ACC BD CCAC CBBAC BBBBB BAABAA ABAD AABB BBH BBBD ABBAAA CCEA AACAAB BCACC ACBH DCH ADBE ADBB DBE ECE DAA DB CC
BC CAC CBAB BDD CDBDE BEBDF ADCDFA DCCFB DBCFC ABDBA BBF AAF BADB DBF AAAAD BDG CEF CBDB BBB FC

E BCB BEA BH BEK AABAF ABAC BAA BFB OD JH BADCF Q Q R AN AAN EI H G
E CB BAB AAA AAA AC BB ACC ACCA AGB AIA AJ AJ ACE AH BAF CAG DAG FAH FJ GJ ADK ABK BL CM
```


'''Extra credit''': generate nonograms with unique solutions, of desired height and width.



This task is the problem n.98 of the "[https://sites.google.com/site/prologsite/prolog-problems 99 Prolog Problems]" by Werner Hett (also thanks to Paul Singleton for the idea and the examples).



## Related tasks

* [[Nonoblock]].


## See also

* [[wp:AC-3_algorithm|Arc Consistency Algorithm]]
* http://www.haskell.org/haskellwiki/99_questions/Solutions/98 (Haskell)
* http://twanvl.nl/blog/haskell/Nonograms (Haskell)
* http://picolisp.com/5000/!wiki?99p98 (PicoLisp)




## C++


### The Solver


```cpp

// A class to solve Nonogram (Hadje) Puzzles
// Nigel Galloway - January 23rd., 2017
template<uint _N, uint _G> class Nonogram {
  enum class ng_val : char {X='#',B='.',V='?'};
  template<uint _NG> struct N {
    N() {}
    N(std::vector<int> ni,const int l) : X{},B{},Tx{},Tb{},ng(ni),En{},gNG(l){}
    std::bitset<_NG> X, B, T, Tx, Tb;
    std::vector<int> ng;
    int En, gNG;
    void        fn (const int n,const int i,const int g,const int e,const int l){
      if (fe(g,l,false) and fe(g+l,e,true)){
      if ((n+1) < ng.size()) {if (fe(g+e+l,1,false)) fn(n+1,i-e-1,g+e+l+1,ng[n+1],0);}
      else {
        if (fe(g+e+l,gNG-(g+e+l),false)){Tb &= T.flip(); Tx &= T.flip(); ++En;}
      }}
      if (l<=gNG-g-i-1) fn(n,i,g,e,l+1);
    }
    void        fi (const int n,const bool g) {X.set(n,g); B.set(n, not g);}
    ng_val      fg (const int n) const{return (X.test(n))? ng_val::X : (B.test(n))? ng_val::B : ng_val::V;}
    inline bool fe (const int n,const int i, const bool g){
      for (int e = n;e<n+i;++e) if ((g and fg(e)==ng_val::B) or (!g and fg(e)==ng_val::X)) return false; else T[e] = g;
      return true;
    }
    int         fl (){
      if (En == 1) return 1;
      Tx.set(); Tb.set(); En=0;
      fn(0,std::accumulate(ng.cbegin(),ng.cend(),0)+ng.size()-1,0,ng[0],0);
      return En;
    }}; // end of N
  std::vector<N<_G>> ng;
  std::vector<N<_N>> gn;
  int En, zN, zG;
  void setCell(uint n, uint i, bool g){ng[n].fi(i,g); gn[i].fi(n,g);}
public:
  Nonogram(const std::vector<std::vector<int>>& n,const std::vector<std::vector<int>>& i,const std::vector<std::string>& g = {}) : ng{}, gn{}, En{}, zN(n.size()), zG(i.size()) {
    for (int n=0; n<zG; n++) gn.push_back(N<_N>(i[n],zN));
    for (int i=0; i<zN; i++) {
      ng.push_back(N<_G>(n[i],zG));
      if (i < g.size()) for(int e=0; e<zG or e<g[i].size(); e++) if (g[i][e]=='#') setCell(i,e,true);
    }}
  bool solve(){
    int i{}, g{};
    for (int l = 0; l<zN; l++) {
      if ((g = ng[l].fl()) == 0) return false; else i+=g;
      for (int i = 0; i<zG; i++) if (ng[l].Tx[i] != ng[l].Tb[i]) setCell (l,i,ng[l].Tx[i]);
    }
    for (int l = 0; l<zG; l++) {
      if ((g = gn[l].fl()) == 0) return false; else i+=g;
      for (int i = 0; i<zN; i++) if (gn[l].Tx[i] != gn[l].Tb[i]) setCell (i,l,gn[l].Tx[i]);
    }
    if (i == En)    return false; else En = i;
    if (i == zN+zG) return true;  else return solve();
  }
  const std::string toStr() const {
    std::ostringstream n;
    for (int i = 0; i<zN; i++){for (int g = 0; g<zG; g++){n << static_cast<char>(ng[i].fg(g));}n<<std::endl;}
    return n.str();
  }};

```



### The Task


```cpp

// For the purpose of this task I provide a little code to read from a file in the required format
// Note though that Nonograms may contain blank lines and values greater than 24
int main(){
  std::ifstream n ("nono.txt");
  if (!n) {
    std::cerr << "Unable to open nono.txt.\n";
    exit(EXIT_FAILURE);
  }
  std::string i;
  getline(n,i);
  std::istringstream g(i);
  std::string e;
  std::vector<std::vector<int>> N;
    while (g >> e) {
      std::vector<int> G;
      for (char l : e) G.push_back((int)l-64);
      N.push_back(G);
    }
  getline(n,i);
  std::istringstream gy(i);
  std::vector<std::vector<int>> G;
    while (gy >> e) {
      std::vector<int> N;
      for (char l : e) N.push_back((int)l-64);
      G.push_back(N);
    }
  Nonogram<32,32> myN(N,G);
  if (!myN.solve()) std::cout << "I don't believe that this is a nonogram!" << std::endl;
  std::cout << "\n" << myN.toStr() << std::endl;
}

```

```txt

C BA CB BB F AE F A B
AB CA AE GA E C D C

.###....
##.#....
.###..##
..##..##
..######
#.#####.
######..
....#...
...##...

F CAC ACAC CN AAA AABB EBB EAA ECCC HCCC
D D AE CD AE A DA BBB CC AAB BAA AAB DA AAB AAA BAB AAA CD BBA DA

..........######....
........###.#..###..
...#..###...#....###
..###.##############
...#..#............#
..#.#.##..........##
#####..##........##.
#####...#........#..
#####..###.###.###..
########.###.###.###

CA BDA ACC BD CCAC CBBAC BBBBB BAABAA ABAD AABB BBH BBBD ABBAAA CCEA AACAAB BCACC ACBH DCH ADBE ADBB DBE ECE DAA DB CC
BC CAC CBAB BDD CDBDE BEBDF ADCDFA DCCFB DBCFC ABDBA BBF AAF BADB DBF AAAAD BDG CEF CBDB BBB FC

....###.#...........
....##.####.#.......
....#.###.###.......
..##.####...........
.###.###.#....###...
###..##.##...#.###..
##..##.##....##.##..
....##.#.#..##.#.#..
....#.##.#...####...
....#.#.##.....##...
.....##.##..########
....##.##...##..####
....#.##.##.#...#..#
###..###.#####.....#
#.#.###.#....#....##
##..###.#....###.###
.#.###.##.########..
.####.###.########..
...#.####.##.#####..
...#.####.##...##...
....####..##...#####
...#####.###...#####
...####.#..........#
..####.##...........
..###.###...........

E BCB BEA BH BEK AABAF ABAC BAA BFB OD JH BADCF Q Q R AN AAN EI H G
E CB BAB AAA AAA AC BB ACC ACCA AGB AIA AJ AJ ACE AH BAF CAG DAG FAH FJ GJ ADK ABK BL CM

....................#####
..##..............###..##
.##..............#####..#
##.............########..
##....#####.###########..
#.#..##....#....######...
#..##.....#.......###....
##........#.............#
.##.....######.........##
..###############....####
.....##########..########
....##.#.####.###..######
........#################
........#################
.......##################
.......#...##############
.......#.#.##############
........#####...#########
.................########
..................#######

```



### Bonus GCHQ Xmas Puzzle

[[https://www.gchq.gov.uk/news-article/christmas-card-cryptographic-twist-charity GCHQ Xmas Puzzle]] is a Nonogram. They say "We pre-shaded a few cells to help people get started. Without this, the puzzle would have been slightly ambiguous, though the error correction used in QR codes means that the URL would have been recovered anyway. As a small Easter egg, the pre-shaded cells spell out “GCHQ” in Morse code."

```cpp

int main(){
  const std::vector<std::vector<int>> Ngchq={{        7,3,1, 1,7},
                                             {      1,1,2,2, 1,1},
                                             {  1,3,1,3,1,1, 3,1},
                                             {  1,3,1,1,6,1, 3,1},
                                             {  1,3,1,5,2,1, 3,1},
                                             {        1,1,2, 1,1},
                                             {    7,1,1,1,1, 1,7},
                                             {               3,3},
                                             {1,2,3,1,1,3,1, 1,2},
                                             {      1,1,3,2, 1,1},
                                             {      4,1,4,2, 1,2},
                                             {  1,1,1,1,1,4, 1,3},
                                             {      2,1,1,1, 2,5},
                                             {      3,2,2,6, 3,1},
                                             {      1,9,1,1, 2,1},
                                             {      2,1,2,2, 3,1},
                                             {    3,1,1,1,1, 5,1},
                                             {          1,2, 2,5},
                                             {    7,1,2,1,1, 1,3},
                                             {    1,1,2,1,2, 2,1},
                                             {      1,3,1,4, 5,1},
                                             {      1,3,1,3,10,2},
                                             {      1,3,1,1, 6,6},
                                             {      1,1,2,1, 1,2},
                                             {        7,2,1, 2,5}};
  const std::vector<std::vector<int>> Ggchq={{        7,2,1,1,7},
                                             {      1,1,2,2,1,1},
                                             {1,3,1,3,1,3,1,3,1},
                                             {  1,3,1,1,5,1,3,1},
                                             {  1,3,1,1,4,1,3,1},
                                             {      1,1,1,2,1,1},
                                             {    7,1,1,1,1,1,7},
                                             {            1,1,3},
                                             {    2,1,2,1,8,2,1},
                                             {  2,2,1,2,1,1,1,2},
                                             {        1,7,3,2,1},
                                             {  1,2,3,1,1,1,1,1},
                                             {        4,1,1,2,6},
                                             {    3,3,1,1,1,3,1},
                                             {        1,2,5,2,2},
                                             {2,2,1,1,1,1,1,2,1},
                                             {    1,3,3,2,1,8,1},
                                             {            6,2,1},
                                             {      7,1,4,1,1,3},
                                             {        1,1,1,1,4},
                                             {      1,3,1,3,7,1},
                                             {1,3,1,1,1,2,1,1,4},
                                             {      1,3,1,4,3,3},
                                             {    1,1,2,2,2,6,1},
                                             {      7,1,3,2,1,1}};

  std::vector<std::string> n = {"",
                                "",
                                "",
                                "...##.......##.......#",
                                "",
                                "",
                                "",
                                "",
                                "......##..#...##..#",
                                "",
                                "",
                                "",
                                "",
                                "",
                                "",
                                "",
                                "......#....#....#...#",
                                "",
                                "",
                                "",
                                "",
                                "...##....##....#....##"};
  Nonogram<25,25> myN(Ngchq,Ggchq,n);
  if (!myN.solve()) std::cout << "I don't believe that this is a nonogram!" << std::endl;
  std::cout << "\n" << myN.toStr() << std::endl;
}

```

```txt

#######.###...#.#.#######
#.....#.##.##.....#.....#
#.###.#.....###.#.#.###.#
#.###.#.#..######.#.###.#
#.###.#..#####.##.#.###.#
#.....#..##.......#.....#
#######.#.#.#.#.#.#######
........###...###........
#.##.###..#.#.###.#..#.##
#.#......###.##....#...#.
.####.#.####.##.#....##..
.#.#...#...#.#.####.#.###
..##..#.#.#......##.#####
...###.##.##.######.###.#
#.#########.#.#..##....#.
.##.#..##...##.###.....#.
###.#.#.#..#....#####.#..
........#...##.##...#####
#######.#..##...#.#.#.###
#.....#.##..#..##...##.#.
#.###.#...####..#####..#.
#.###.#.###.##########.##
#.###.#.#..######.######.
#.....#..##......#.#.##..
#######.##...#.##...#####

```



## C#


```c#
using System;
using System.Collections.Generic;
using static System.Linq.Enumerable;

public static class NonogramSolver
{
    public static void Main2() {
        foreach (var (x, y) in new [] {
            ("C BA CB BB F AE F A B", "AB CA AE GA E C D C"),
            ("F CAC ACAC CN AAA AABB EBB EAA ECCC HCCC",
                "D D AE CD AE A DA BBB CC AAB BAA AAB DA AAB AAA BAB AAA CD BBA DA"),
            ("CA BDA ACC BD CCAC CBBAC BBBBB BAABAA ABAD AABB BBH BBBD ABBAAA CCEA AACAAB BCACC ACBH DCH ADBE ADBB DBE ECE DAA DB CC",
                "BC CAC CBAB BDD CDBDE BEBDF ADCDFA DCCFB DBCFC ABDBA BBF AAF BADB DBF AAAAD BDG CEF CBDB BBB FC"),
            ("E BCB BEA BH BEK AABAF ABAC BAA BFB OD JH BADCF Q Q R AN AAN EI H G",
                "E CB BAB AAA AAA AC BB ACC ACCA AGB AIA AJ AJ ACE AH BAF CAG DAG FAH FJ GJ ADK ABK BL CM")
            })
        {
            Solve(x, y);
            Console.WriteLine();
        }
    }

    static void Solve(string rowLetters, string columnLetters) {
        var r = rowLetters.Split(" ").Select(row => row.Select(s => s - 'A' + 1).ToArray()).ToArray();
        var c = columnLetters.Split(" ").Select(column => column.Select(s => s - 'A' + 1).ToArray()).ToArray();
        Solve(r, c);
    }

    static void Solve(int[][] rowRuns, int[][] columnRuns) {
        int len = columnRuns.Length;
        var rows = rowRuns.Select(row => Generate(len, row)).ToList();
        var columns = columnRuns.Select(column => Generate(rowRuns.Length, column)).ToList();
        Reduce(rows, columns);
        foreach (var list in rows) {
            if (list.Count != 1) Console.WriteLine(Repeat('?', len).Spaced());
            else Console.WriteLine(list[0].ToString().PadLeft(len, '0').Replace('1', '#').Replace('0', '.').Reverse().Spaced());
        }
    }

    static List<BitSet> Generate(int length, params int[] runs) {
        var list = new List<BitSet>();
        BitSet initial = BitSet.Empty;
        int[] sums = new int[runs.Length];
        sums[0] = 0;
        for (int i = 1; i < runs.Length; i++) sums[i] = sums[i - 1] + runs[i - 1] + 1;
        for (int r = 0; r < runs.Length; r++) initial = initial.AddRange(sums[r], runs[r]);
        Generate(list, BitSet.Empty.Add(length), runs, sums, initial, 0, 0);
        return list;
    }

    static void Generate(List<BitSet> result, BitSet max, int[] runs, int[] sums, BitSet current, int index, int shift) {
        if (index == runs.Length) {
            result.Add(current);
            return;
        }
        while (current.Value < max.Value) {
            Generate(result, max, runs, sums, current, index + 1, shift);
            current = current.ShiftLeftAt(sums[index] + shift);
            shift++;
        }
    }

    static void Reduce(List<List<BitSet>> rows, List<List<BitSet>> columns) {
        for (int count = 1; count > 0; ) {
            foreach (var (rowIndex, row) in rows.WithIndex()) {
                var allOn  = row.Aggregate((a, b) => a & b);
                var allOff = row.Aggregate((a, b) => a | b);
                foreach (var (columnIndex, column) in columns.WithIndex()) {
                    count  = column.RemoveAll(c => allOn.Contains(columnIndex) && !c.Contains(rowIndex));
                    count += column.RemoveAll(c => !allOff.Contains(columnIndex) && c.Contains(rowIndex));
                }
            }
            foreach (var (columnIndex, column) in columns.WithIndex()) {
                var allOn  = column.Aggregate((a, b) => a & b);
                var allOff = column.Aggregate((a, b) => a | b);
                foreach (var (rowIndex, row) in rows.WithIndex()) {
                    count += row.RemoveAll(r => allOn.Contains(rowIndex) && !r.Contains(columnIndex));
                    count += row.RemoveAll(r => !allOff.Contains(rowIndex) && r.Contains(columnIndex));
                }
            }
        }
    }

    static IEnumerable<(int index, T element)> WithIndex<T>(this IEnumerable<T> source) {
        int i = 0;
        foreach (T element in source) {
            yield return (i++, element);
        }
    }

    static string Reverse(this string s) {
        char[] array = s.ToCharArray();
        Array.Reverse(array);
        return new string(array);
    }

    static string Spaced(this IEnumerable<char> s) => string.Join(" ", s);

    struct BitSet //Unused functionality elided.
    {
        public static BitSet Empty => default;
        private readonly int bits;
        public int Value => bits;

        private BitSet(int bits) => this.bits = bits;

        public BitSet Add(int item) => new BitSet(bits | (1 << item));
        public BitSet AddRange(int start, int count) => new BitSet(bits | (((1 << (start + count)) - 1) - ((1 << start) - 1)));
        public bool Contains(int item) => (bits & (1 << item)) != 0;
        public BitSet ShiftLeftAt(int index)  => new BitSet((bits >> index << (index + 1)) | (bits & ((1 << index) - 1)));
        public override string ToString() => Convert.ToString(bits, 2);

        public static BitSet operator &(BitSet a, BitSet b) => new BitSet(a.bits & b.bits);
        public static BitSet operator |(BitSet a, BitSet b) => new BitSet(a.bits | b.bits);
    }

}
```

<pre style="height:30ex;overflow:scroll">
. # # # . . . .
# # . # . . . .
. # # # . . # #
. . # # . . # #
. . # # # # # #
# . # # # # # .
# # # # # # . .
. . . . # . . .
. . . # # . . .

. . . . . . . . . . # # # # # # . . . .
. . . . . . . . # # # . # . . # # # . .
. . . # . . # # # . . . # . . . . # # #
. . # # # . # # # # # # # # # # # # # #
. . . # . . # . . . . . . . . . . . . #
. . # . # . # # . . . . . . . . . . # #
# # # # # . . # # . . . . . . . . # # .
# # # # # . . . # . . . . . . . . # . .
# # # # # . . # # # . # # # . # # # . .
# # # # # # # # . # # # . # # # . # # #

. . . . # # # . # . . . . . . . . . . .
. . . . # # . # # # # . # . . . . . . .
. . . . # . # # # . # # # . . . . . . .
. . # # . # # # # . . . . . . . . . . .
. # # # . # # # . # . . . . # # # . . .
# # # . . # # . # # . . . # . # # # . .
# # . . # # . # # . . . . # # . # # . .
. . . . # # . # . # . . # # . # . # . .
. . . . # . # # . # . . . # # # # . . .
. . . . # . # . # # . . . . . # # . . .
. . . . . # # . # # . . # # # # # # # #
. . . . # # . # # . . . # # . . # # # #
. . . . # . # # . # # . # . . . # . . #
# # # . . # # # . # # # # # . . . . . #
# . # . # # # . # . . . . # . . . . # #
# # . . # # # . # . . . . # # # . # # #
. # . # # # . # # . # # # # # # # # . .
. # # # # . # # # . # # # # # # # # . .
. . . # . # # # # . # # . # # # # # . .
. . . # . # # # # . # # . . . # # . . .
. . . . # # # # . . # # . . . # # # # #
. . . # # # # # . # # # . . . # # # # #
. . . # # # # . # . . . . . . . . . . #
. . # # # # . # # . . . . . . . . . . .
. . # # # . # # # . . . . . . . . . . .

. . . . . . . . . . . . . . . . . . . . # # # # #
. . # # . . . . . . . . . . . . . . # # # . . # #
. # # . . . . . . . . . . . . . . # # # # # . . #
# # . . . . . . . . . . . . . # # # # # # # # . .
# # . . . . # # # # # . # # # # # # # # # # # . .
# . # . . # # . . . . # . . . . # # # # # # . . .
# . . # # . . . . . # . . . . . . . # # # . . . .
# # . . . . . . . . # . . . . . . . . . . . . . #
. # # . . . . . # # # # # # . . . . . . . . . # #
. . # # # # # # # # # # # # # # # . . . . # # # #
. . . . . # # # # # # # # # # . . # # # # # # # #
. . . . # # . # . # # # # . # # # . . # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . # # # # # # # # # # # # # # # # # #
. . . . . . . # . . . # # # # # # # # # # # # # #
. . . . . . . # . # . # # # # # # # # # # # # # #
. . . . . . . . # # # # # . . . # # # # # # # # #
. . . . . . . . . . . . . . . . . # # # # # # # #
. . . . . . . . . . . . . . . . . . # # # # # # #
```



## Common Lisp


```lisp
(defpackage :ac3
  (:use :cl)
  (:export :var
           :domain
           :satisfies-p
           :constraint-possible-p
           :ac3)
  (:documentation "Implements the AC3 algorithm. Extend VAR with the variable
types for your particular problem and implement SATISFIES-P and
CONSTRAINT-POSSIBLE-P for your variables. Initialize the DOMAIN of your variables
with unary constraints already satisfied and then pass them to AC3 in a list."))

(in-package :ac3)

(defclass var ()
  ((domain :initarg :domain :accessor domain))
  (:documentation "The base variable type from which all other
variables should extend."))

(defgeneric satisfies-p (a b va vb)
  (:documentation "Determine if constrainted variables A and B are
satisfied by the instantiation of their respective values VA and VB."))

(defgeneric constraint-possible-p (a b)
  (:documentation "Determine if variables A and B can even be
checked for a binary constraint."))

(defun arc-reduce (a b)
  "Assuming A and B truly form a constraint, prune all values
from A that do not satisfy any value in B. Return T if the domain
of A changed by any amount, NIL otherwise."
  (let (change)
    (setf (domain a)
          (loop for va in (domain a)
             when (loop for vb in (domain b)
                     do (when (satisfies-p a b va vb)
                          (return t))
                     finally (setf change t) (return nil))
             collect va))
    change))

(defun binary-constraint-p (a b)
  "Check if variables A and B could form a constraint, then return T
if any of their values form a contradiction, NIL otherwise."
  (when (constraint-possible-p a b)
    (block found
      (loop for va in (domain a)
         do (loop for vb in (domain b)
               do (unless (satisfies-p a b va vb)
                    (return-from found t)))))))

(defun ac3 (vars)
  "Run the Arc Consistency 3 algorithm on the given set of variables.
Assumes unary constraints have already been satisfied."
  ;; Form a worklist of the constraints of every variable to every other variable.
  (let ((worklist (loop for x in vars
                     append (loop for y in vars
                               when (and (not (eq x y))
                                         (binary-constraint-p x y))
                               collect (cons x y)))))
    ;; Prune the worklist of satisfied arcs until it is empty.
    (loop while worklist
       do (destructuring-bind (x . y) (pop worklist)
            (when (arc-reduce x y)
              (if (domain x)
                  ;; If the current arc's domain was reduced, then append any arcs it
                  ;; is still constrained with to the end of the worklist, as they
                  ;; need to be rechecked.
                  (setf worklist (nconc worklist (loop for z in vars
                                                    when (and (not (eq x z))
                                                              (not (eq y z))
                                                              (binary-constraint-p x z))
                                                    collect (cons z x))))
                  (error "No values left in ~a" x))))
       finally (return vars))))

(defpackage :nonogram
  (:use :cl :ac3)
  (:documentation "Utilize the AC3 package to solve nonograms."))

(in-package :nonogram)

(defclass line (var)
  ((depth :initarg :depth :accessor depth))
  (:documentation "A LINE is a variable that represents either a
column or row of cells and all of the permutations of values those
cells can assume"))

(defmethod print-object ((o line) s)
  (print-unreadable-object (o s :type t)
    (with-slots (depth domain) o
      (format s ":depth ~a :domain ~a" depth domain))))

(defclass row (line) ())

(defclass col (line) ())

(defmethod satisfies-p ((a line) (b line) va vb)
  (eq (aref va (depth b))
      (aref vb (depth a))))

(defmethod constraint-possible-p ((a line) (b line))
  (not (eq (type-of a) (type-of b))))

(defun make-line-domain (runs length &optional (start 0) acc)
  "Enumerate all valid permutations of a line's values."
  (if runs
      (loop for i from start
         to (- length
               (reduce #'+ (cdr runs))
               (length (cdr runs))
               (car runs))
         append (make-line-domain (cdr runs) length (+ 1 i (car runs)) (cons i acc)))
      (list (reverse acc))))

(defun make-line (type runs depth length)
  "Create and initialize a ROW or COL instance."
  (make-instance
   type :depth depth :domain
   (loop for value in (make-line-domain runs length)
      collect (let ((arr (make-array length :initial-element nil)))
                (loop for pos in value
                   for run in runs
                   do (loop for i from pos below (+ pos run)
                         do (setf (aref arr i) t)))
                arr))))

(defun make-lines (type run-set length)
  "Initialize a set of lines."
  (loop for runs across run-set
     for depth from 0
     collect (make-line type runs depth length)))

(defun nonogram (problem)
  "Given a nonogram problem description, solve it and print the result."
  (let* ((nrows (length (aref problem 0)))
         (ncols (length (aref problem 1)))
         (vars (ac3 (append (make-lines 'row (aref problem 0) ncols)
                            (make-lines 'col (aref problem 1) nrows)))))
    (loop for var in vars
       while (eq 'row (type-of var))
       do (terpri)
         (loop for cell across (car (domain var))
            do (format t "~a " (if cell #\# #\.))))))

(defparameter *test-set*
  '("C BA CB BB F AE F A B"
    "AB CA AE GA E C D C"))

;; Helper functions to read and parse problems from a file.

(defun parse-word (word)
  (map 'list (lambda (c) (1+ (- (char-code c) (char-code #\A)))) word))

(defun parse-line (line)
  (map 'vector #'parse-word (uiop:split-string (string-upcase line))))

(defun parse-nonogram (rows columns)
  (vector (parse-line rows)
          (parse-line columns)))

(defun read-until-line (stream)
  (loop (let ((line (read-line stream)))
          (when (> (length (string-trim '(#\space) line)) 0)
            (print line)
            (return line)))))

(defun solve-from-file (file)
  (handler-case
      (with-open-file (s file)
        (loop
           (terpri)
           (nonogram (parse-nonogram (read-until-line s)
                                     (read-until-line s)))))
    (end-of-file ())))
```

```txt
CL-USER> (time (nonogram::solve-from-file "c:/Users/cro/Dropbox/Projects/rosetta-code/nonogram_problems.txt"))


"C BA CB BB F AE F A B"
"AB CA AE GA E C D C"
. # # # . . . .
# # . # . . . .
. # # # . . # #
. . # # . . # #
. . # # # # # #
# . # # # # # .
# # # # # # . .
. . . . # . . .
. . . # # . . .

"F CAC ACAC CN AAA AABB EBB EAA ECCC HCCC"
"D D AE CD AE A DA BBB CC AAB BAA AAB DA AAB AAA BAB AAA CD BBA DA"
. . . . . . . . . . # # # # # # . . . .
. . . . . . . . # # # . # . . # # # . .
. . . # . . # # # . . . # . . . . # # #
. . # # # . # # # # # # # # # # # # # #
. . . # . . # . . . . . . . . . . . . #
. . # . # . # # . . . . . . . . . . # #
# # # # # . . # # . . . . . . . . # # .
# # # # # . . . # . . . . . . . . # . .
# # # # # . . # # # . # # # . # # # . .
# # # # # # # # . # # # . # # # . # # #

"CA BDA ACC BD CCAC CBBAC BBBBB BAABAA ABAD AABB BBH BBBD ABBAAA CCEA AACAAB BCACC ACBH DCH ADBE ADBB DBE ECE DAA DB CC"
"BC CAC CBAB BDD CDBDE BEBDF ADCDFA DCCFB DBCFC ABDBA BBF AAF BADB DBF AAAAD BDG CEF CBDB BBB FC"
. . . . # # # . # . . . . . . . . . . .
. . . . # # . # # # # . # . . . . . . .
. . . . # . # # # . # # # . . . . . . .
. . # # . # # # # . . . . . . . . . . .
. # # # . # # # . # . . . . # # # . . .
# # # . . # # . # # . . . # . # # # . .
# # . . # # . # # . . . . # # . # # . .
. . . . # # . # . # . . # # . # . # . .
. . . . # . # # . # . . . # # # # . . .
. . . . # . # . # # . . . . . # # . . .
. . . . . # # . # # . . # # # # # # # #
. . . . # # . # # . . . # # . . # # # #
. . . . # . # # . # # . # . . . # . . #
# # # . . # # # . # # # # # . . . . . #
# . # . # # # . # . . . . # . . . . # #
# # . . # # # . # . . . . # # # . # # #
. # . # # # . # # . # # # # # # # # . .
. # # # # . # # # . # # # # # # # # . .
. . . # . # # # # . # # . # # # # # . .
. . . # . # # # # . # # . . . # # . . .
. . . . # # # # . . # # . . . # # # # #
. . . # # # # # . # # # . . . # # # # #
. . . # # # # . # . . . . . . . . . . #
. . # # # # . # # . . . . . . . . . . .
. . # # # . # # # . . . . . . . . . . .

"E BCB BEA BH BEK AABAF ABAC BAA BFB OD JH BADCF Q Q R AN AAN EI H G"
"E CB BAB AAA AAA AC BB ACC ACCA AGB AIA AJ AJ ACE AH BAF CAG DAG FAH FJ GJ ADK ABK BL CM"
. . . . . . . . . . . . . . . . . . . . # # # # #
. . # # . . . . . . . . . . . . . . # # # . . # #
. # # . . . . . . . . . . . . . . # # # # # . . #
# # . . . . . . . . . . . . . # # # # # # # # . .
# # . . . . # # # # # . # # # # # # # # # # # . .
# . # . . # # . . . . # . . . . # # # # # # . . .
# . . # # . . . . . # . . . . . . . # # # . . . .
# # . . . . . . . . # . . . . . . . . . . . . . #
. # # . . . . . # # # # # # . . . . . . . . . # #
. . # # # # # # # # # # # # # # # . . . . # # # #
. . . . . # # # # # # # # # # . . # # # # # # # #
. . . . # # . # . # # # # . # # # . . # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . # # # # # # # # # # # # # # # # # #
. . . . . . . # . . . # # # # # # # # # # # # # #
. . . . . . . # . # . # # # # # # # # # # # # # #
. . . . . . . . # # # # # . . . # # # # # # # # #
. . . . . . . . . . . . . . . . . # # # # # # # #
. . . . . . . . . . . . . . . . . . # # # # # # #
Evaluation took:
  0.906 seconds of real time
  0.906250 seconds of total run time (0.890625 user, 0.015625 system)
  100.00% CPU
  1 form interpreted
  59 lambdas converted
  2,979,778,058 processor cycles
  58,974,976 bytes consed
```



## D

```d
import std.stdio, std.range, std.file, std.algorithm, std.string;

/// Create all patterns of a row or col that match given runs.
auto genRow(in int w, in int[] s) pure nothrow @safe {
    static int[][] genSeg(in int[][] o, in int sp) pure nothrow @safe {
        if (o.empty)
            return [[2].replicate(sp)];

        typeof(return) result;
        foreach (immutable x; 1 .. sp - o.length + 2)
            foreach (const tail; genSeg(o[1 .. $], sp - x))
                result ~= [2].replicate(x) ~ o[0] ~ tail;
        return result;
    }

    const ones = s.map!(i => [1].replicate(i)).array;
    return genSeg(ones, w + 1 - s.sum).map!dropOne;
}

/// Fix inevitable value of cells, and propagate.
void deduce(in int[][] hr, in int[][] vr) {
    static int[] allowable(in int[][] row) pure nothrow @safe {
        //return row.dropOne.fold!q{ a[] |= b[] }(row[0].dup);
        return reduce!q{ a[] |= b[] }(row[0].dup, row.dropOne);
    }

    static bool fits(in int[] a, in int[] b)
    pure /*nothrow*/ @safe /*@nogc*/ {
        return zip(a, b).all!(xy => xy[0] & xy[1]);
    }

    immutable int w = vr.length,
                  h = hr.length;
    auto rows = hr.map!(x => genRow(w, x).array).array;
    auto cols = vr.map!(x => genRow(h, x).array).array;
    auto canDo = rows.map!allowable.array;

    // Initially mark all columns for update.
    bool[uint] modRows, modCols;
    modCols = true.repeat.enumerate!uint.take(w).assocArray;

    /// See if any value a given column is fixed; if so,
    /// mark its corresponding row for future fixup.
    void fixCol(in int n) /*nothrow*/ @safe {
        const c = canDo.map!(x => x[n]).array;
        cols[n] = cols[n].remove!(x => !fits(x, c)); // Throws.
        foreach (immutable i, immutable x; allowable(cols[n]))
            if (x != canDo[i][n]) {
                modRows[i] = true;
                canDo[i][n] &= x;
            }
    }

    /// Ditto, for rows.
    void fixRow(in int n) /*nothrow*/ @safe {
        const c = canDo[n];
        rows[n] = rows[n].remove!(x => !fits(x, c)); // Throws.
        foreach (immutable i, immutable x; allowable(rows[n]))
            if (x != canDo[n][i]) {
                modCols[i] = true;
                canDo[n][i] &= x;
            }
    }

    void showGram(in int[][] m) {
        // If there's 'x', something is wrong.
        // If there's '?', needs more work.
        m.each!(x => writefln("%-(%c %)", x.map!(i => "x#.?"[i])));
        writeln;
    }

    while (modCols.length > 0) {
        modCols.byKey.each!fixCol;
        modCols = null;
        modRows.byKey.each!fixRow;
        modRows = null;
    }

    if (cartesianProduct(h.iota, w.iota)
        .all!(ij => canDo[ij[0]][ij[1]] == 1 || canDo[ij[0]][ij[1]] == 2))
        "Solution would be unique".writeln;
    else
        "Solution may not be unique, doing exhaustive search:".writeln;

    // We actually do exhaustive search anyway. Unique
    // solution takes no time in this phase anyway.
    auto out_ = new const(int)[][](h);

    uint tryAll(in int n = 0) {
        if (n >= h) {
            foreach (immutable j; 0 .. w)
                if (!cols[j].canFind(out_.map!(x => x[j]).array))
                    return 0;
            showGram(out_);
            return 1;
        }
        typeof(return) sol = 0;
        foreach (const x; rows[n]) {
            out_[n] = x;
            sol += tryAll(n + 1);
        }
        return sol;
    }

    immutable n = tryAll;
    switch (n) {
        case 0:  "No solution.".writeln;     break;
        case 1:  "Unique solution.".writeln; break;
        default: writeln(n, " solutions."); break;
    }
    writeln;
}

void solve(in string p, in bool showRuns=true) {
    immutable s = p.splitLines.map!(l => l.split.map!(w =>
                    w.map!(c => int(c - 'A' + 1)).array).array).array;
                    //w.map!(c => c - 'A' + 1))).to!(int[][][]);

    if (showRuns) {
        writeln("Horizontal runs: ", s[0]);
        writeln("Vertical runs: ", s[1]);
    }
    deduce(s[0], s[1]);
}

void main() {
    // Read problems from file.
    immutable fn = "nonogram_problems.txt";
    fn.readText.split("\n\n").filter!(p => !p.strip.empty).each!(p => p.strip.solve);

    "Extra example not solvable by deduction alone:".writeln;
    "B B A A\nB B A A".solve;

    "Extra example where there is no solution:".writeln;
    "B A A\nA A A".solve;
}
```

```txt
Horizontal runs: [[3], [2, 1], [3, 2], [2, 2], [6], [1, 5], [6], [1], [2]]
Vertical runs: [[1, 2], [3, 1], [1, 5], [7, 1], [5], [3], [4], [3]]
Solution would be unique
. # # # . . . .
# # . # . . . .
. # # # . . # #
. . # # . . # #
. . # # # # # #
# . # # # # # .
# # # # # # . .
. . . . # . . .
. . . # # . . .

Unique solution.

Horizontal runs: [[6], [3, 1, 3], [1, 3, 1, 3], [3, 14], [1, 1, 1], [1, 1, 2, 2], [5, 2, 2], [5, 1, 1], [5, 3, 3, 3], [8, 3, 3, 3]]
Vertical runs: [[4], [4], [1, 5], [3, 4], [1, 5], [1], [4, 1], [2, 2, 2], [3, 3], [1, 1, 2], [2, 1, 1], [1, 1, 2], [4, 1], [1, 1, 2], [1, 1, 1], [2, 1, 2], [1, 1, 1], [3, 4], [2, 2, 1], [4, 1]]
Solution would be unique
. . . . . . . . . . # # # # # # . . . .
. . . . . . . . # # # . # . . # # # . .
. . . # . . # # # . . . # . . . . # # #
. . # # # . # # # # # # # # # # # # # #
. . . # . . # . . . . . . . . . . . . #
. . # . # . # # . . . . . . . . . . # #
# # # # # . . # # . . . . . . . . # # .
# # # # # . . . # . . . . . . . . # . .
# # # # # . . # # # . # # # . # # # . .
# # # # # # # # . # # # . # # # . # # #

Unique solution.

Horizontal runs: [[3, 1], [2, 4, 1], [1, 3, 3], [2, 4], [3, 3, 1, 3], [3, 2, 2, 1, 3], [2, 2, 2, 2, 2], [2, 1, 1, 2, 1, 1], [1, 2, 1, 4], [1, 1, 2, 2], [2, 2, 8], [2, 2, 2, 4], [1, 2, 2, 1, 1, 1], [3, 3, 5, 1], [1, 1, 3, 1, 1, 2], [2, 3, 1, 3, 3], [1, 3, 2, 8], [4, 3, 8], [1, 4, 2, 5], [1, 4, 2, 2], [4, 2, 5], [5, 3, 5], [4, 1, 1], [4, 2], [3, 3]]
Vertical runs: [[2, 3], [3, 1, 3], [3, 2, 1, 2], [2, 4, 4], [3, 4, 2, 4, 5], [2, 5, 2, 4, 6], [1, 4, 3, 4, 6, 1], [4, 3, 3, 6, 2], [4, 2, 3, 6, 3], [1, 2, 4, 2, 1], [2, 2, 6], [1, 1, 6], [2, 1, 4, 2], [4, 2, 6], [1, 1, 1, 1, 4], [2, 4, 7], [3, 5, 6], [3, 2, 4, 2], [2, 2, 2], [6, 3]]
Solution would be unique
. . . . # # # . # . . . . . . . . . . .
. . . . # # . # # # # . # . . . . . . .
. . . . # . # # # . # # # . . . . . . .
. . # # . # # # # . . . . . . . . . . .
. # # # . # # # . # . . . . # # # . . .
# # # . . # # . # # . . . # . # # # . .
# # . . # # . # # . . . . # # . # # . .
. . . . # # . # . # . . # # . # . # . .
. . . . # . # # . # . . . # # # # . . .
. . . . # . # . # # . . . . . # # . . .
. . . . . # # . # # . . # # # # # # # #
. . . . # # . # # . . . # # . . # # # #
. . . . # . # # . # # . # . . . # . . #
# # # . . # # # . # # # # # . . . . . #
# . # . # # # . # . . . . # . . . . # #
# # . . # # # . # . . . . # # # . # # #
. # . # # # . # # . # # # # # # # # . .
. # # # # . # # # . # # # # # # # # . .
. . . # . # # # # . # # . # # # # # . .
. . . # . # # # # . # # . . . # # . . .
. . . . # # # # . . # # . . . # # # # #
. . . # # # # # . # # # . . . # # # # #
. . . # # # # . # . . . . . . . . . . #
. . # # # # . # # . . . . . . . . . . .
. . # # # . # # # . . . . . . . . . . .

Unique solution.

Horizontal runs: [[5], [2, 3, 2], [2, 5, 1], [2, 8], [2, 5, 11], [1, 1, 2, 1, 6], [1, 2, 1, 3], [2, 1, 1], [2, 6, 2], [15, 4], [10, 8], [2, 1, 4, 3, 6], [17], [17], [18], [1, 14], [1, 1, 14], [5, 9], [8], [7]]
Vertical runs: [[5], [3, 2], [2, 1, 2], [1, 1, 1], [1, 1, 1], [1, 3], [2, 2], [1, 3, 3], [1, 3, 3, 1], [1, 7, 2], [1, 9, 1], [1, 10], [1, 10], [1, 3, 5], [1, 8], [2, 1, 6], [3, 1, 7], [4, 1, 7], [6, 1, 8], [6, 10], [7, 10], [1, 4, 11], [1, 2, 11], [2, 12], [3, 13]]
Solution would be unique
. . . . . . . . . . . . . . . . . . . . # # # # #
. . # # . . . . . . . . . . . . . . # # # . . # #
. # # . . . . . . . . . . . . . . # # # # # . . #
# # . . . . . . . . . . . . . # # # # # # # # . .
# # . . . . # # # # # . # # # # # # # # # # # . .
# . # . . # # . . . . # . . . . # # # # # # . . .
# . . # # . . . . . # . . . . . . . # # # . . . .
# # . . . . . . . . # . . . . . . . . . . . . . #
. # # . . . . . # # # # # # . . . . . . . . . # #
. . # # # # # # # # # # # # # # # . . . . # # # #
. . . . . # # # # # # # # # # . . # # # # # # # #
. . . . # # . # . # # # # . # # # . . # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . # # # # # # # # # # # # # # # # # #
. . . . . . . # . . . # # # # # # # # # # # # # #
. . . . . . . # . # . # # # # # # # # # # # # # #
. . . . . . . . # # # # # . . . # # # # # # # # #
. . . . . . . . . . . . . . . . . # # # # # # # #
. . . . . . . . . . . . . . . . . . # # # # # # #

Unique solution.

Extra example not solvable by deduction alone:
Horizontal runs: [[2], [2], [1], [1]]
Vertical runs: [[2], [2], [1], [1]]
Solution may not be unique, doing exhaustive search:
# # . .
# # . .
. . # .
. . . #

# # . .
# # . .
. . . #
. . # .

. # # .
# # . .
# . . .
. . . #

3 solutions.

Extra example where there is no solution:
Horizontal runs: [[2], [1], [1]]
Vertical runs: [[1], [1], [1]]
Solution may not be unique, doing exhaustive search:
No solution.
```

The output is the same as the Python entry. The run-time with ldc2 compiler is about 0.29 seconds.

=={{header|F_Sharp|F#}}==

```fsharp

(*
I define a discriminated union to provide Nonogram Solver functionality.
Nigel Galloway May 28th., 2016
*)
type N =
  |X |B |V
  static member fn n i =
    let     fn n i = [for g = 0 to i-n do yield Array.init (n+g) (fun e -> if e >= g then X else B)]
    let rec fi n i = [
      match n with
      | h::t -> match t with
                | [] -> for g in fn h i do yield Array.append g (Array.init (i-g.Length) (fun _ -> B))
                | _  -> for g in fn h ((i-List.sum t)+t.Length) do for a in fi t (i-g.Length-1) do yield Array.concat[g;[|B|];a]
      | []   -> yield Array.init i (fun _ -> B)
    ]
    fi n i
  static member fi n i = Array.map2 (fun n g -> match (n,g) with |X,X->X |B,B->B |_->V) n i
  static member fg (n: N[]) (i: N[][]) g = n |> Seq.mapi (fun e n -> i.[e].[g] = n || i.[e].[g] = V) |> Seq.forall (fun n -> n)
  static member fe (n: N[][]) = n|> Array.forall (fun n -> Array.forall (fun n -> n <> V) n)
  static member fl n = n |> Array.Parallel.map (fun n -> Seq.reduce (fun n g -> N.fi n g) n)
  static member fa (nga: list<N []>[]) ngb = Array.Parallel.mapi (fun i n -> List.filter (fun n -> N.fg n ngb i) n) nga
  static member fo n i g e =
    let na = N.fa n e
    let ia = N.fl na
    let ga = N.fa g ia
    (na, ia, ga, (N.fl ga))
  static member toStr n = match n with |X->"X"|B->"."|V->"?"
  static member presolve ((na: list<N []>[]), (ga: list<N []>[])) =
    let nb = N.fl na
    let x = N.fa ga nb
    let rec fn n i g e l =
      let na,ia,ga,ea = N.fo n i g e
      let el = ((Array.map (fun n -> List.length n) na), (Array.map (fun n -> List.length n) ga))
      if ((fst el) = (fst l)) && ((snd el) = (snd l)) then (n,i,g,e,(Array.forall (fun n -> n = 1) (fst l))) else fn na ia ga ea el
    fn na nb x (N.fl x) ((Array.map (fun n -> List.length n) na), (Array.map (fun n -> List.length n) ga))

```

For the purposes of this task I provide a little code to read the input from a file

```fsharp

let fe (n : array<string>) i = n |> Array.collect (fun n -> [|N.fn [for g in n -> ((int)g-64)] i|])
let fl (n : array<string>) (i : array<string>) = (fe n i.Length), (fe i n.Length)
let rFile =
  try
    use file = File.OpenText @"nonogram.txt"
    Some(fl (file.ReadLine().Split ' ') (file.ReadLine().Split ' '))
  with | _  -> printfn "Error reading file" ; None

```

This may be used:

```fsharp

let n,i,g,e,l = N.presolve rFile.Value
if l then i |> Array.iter (fun n -> n |> Array.iter (fun n -> printf "%s" (N.toStr n));printfn "") else printfn "No unique solution"

```

```txt

C BA CB BB F AE F A B
AB CA AE GA E C D C

.XXX....
XX.X....
.XXX..XX
..XX..XX
..XXXXXX
X.XXXXX.
XXXXXX..
....X...
...XX...

F CAC ACAC CN AAA AABB EBB EAA ECCC HCCC
D D AE CD AE A DA BBB CC AAB BAA AAB DA AAB AAA BAB AAA CD BBA DA

..........XXXXXX....
........XXX.X..XXX..
...X..XXX...X....XXX
..XXX.XXXXXXXXXXXXXX
...X..X............X
..X.X.XX..........XX
XXXXX..XX........XX.
XXXXX...X........X..
XXXXX..XXX.XXX.XXX..
XXXXXXXX.XXX.XXX.XXX

CA BDA ACC BD CCAC CBBAC BBBBB BAABAA ABAD AABB BBH BBBD ABBAAA CCEA AACAAB BCACC ACBH DCH ADBE ADBB DBE ECE DAA DB CC
BC CAC CBAB BDD CDBDE BEBDF ADCDFA DCCFB DBCFC ABDBA BBF AAF BADB DBF AAAAD BDG CEF CBDB BBB FC

....XXX.X...........
....XX.XXXX.X.......
....X.XXX.XXX.......
..XX.XXXX...........
.XXX.XXX.X....XXX...
XXX..XX.XX...X.XXX..
XX..XX.XX....XX.XX..
....XX.X.X..XX.X.X..
....X.XX.X...XXXX...
....X.X.XX.....XX...
.....XX.XX..XXXXXXXX
....XX.XX...XX..XXXX
....X.XX.XX.X...X..X
XXX..XXX.XXXXX.....X
X.X.XXX.X....X....XX
XX..XXX.X....XXX.XXX
.X.XXX.XX.XXXXXXXX..
.XXXX.XXX.XXXXXXXX..
...X.XXXX.XX.XXXXX..
...X.XXXX.XX...XX...
....XXXX..XX...XXXXX
...XXXXX.XXX...XXXXX
...XXXX.X..........X
..XXXX.XX...........
..XXX.XXX...........

E BCB BEA BH BEK AABAF ABAC BAA BFB OD JH BADCF Q Q R AN AAN EI H G
E CB BAB AAA AAA AC BB ACC ACCA AGB AIA AJ AJ ACE AH BAF CAG DAG FAH FJ GJ ADK ABK BL CM

....................XXXXX
..XX..............XXX..XX
.XX..............XXXXX..X
XX.............XXXXXXXX..
XX....XXXXX.XXXXXXXXXXX..
X.X..XX....X....XXXXXX...
X..XX.....X.......XXX....
XX........X.............X
.XX.....XXXXXX.........XX
..XXXXXXXXXXXXXXX....XXXX
.....XXXXXXXXXX..XXXXXXXX
....XX.X.XXXX.XXX..XXXXXX
........XXXXXXXXXXXXXXXXX
........XXXXXXXXXXXXXXXXX
.......XXXXXXXXXXXXXXXXXX
.......X...XXXXXXXXXXXXXX
.......X.X.XXXXXXXXXXXXXX
........XXXXX...XXXXXXXXX
.................XXXXXXXX
..................XXXXXXX

```



## Go

```go
package main

import (
    "fmt"
    "strings"
)

type BitSet []bool

func (bs BitSet) and(other BitSet) {
    for i := range bs {
        if bs[i] && other[i] {
            bs[i] = true
        } else {
            bs[i] = false
        }
    }
}

func (bs BitSet) or(other BitSet) {
    for i := range bs {
        if bs[i] || other[i] {
            bs[i] = true
        } else {
            bs[i] = false
        }
    }
}

func iff(cond bool, s1, s2 string) string {
    if cond {
        return s1
    }
    return s2
}

func newPuzzle(data [2]string) {
    rowData := strings.Fields(data[0])
    colData := strings.Fields(data[1])
    rows := getCandidates(rowData, len(colData))
    cols := getCandidates(colData, len(rowData))

    for {
        numChanged := reduceMutual(cols, rows)
        if numChanged == -1 {
            fmt.Println("No solution")
            return
        }
        if numChanged == 0 {
            break
        }
    }

    for _, row := range rows {
        for i := 0; i < len(cols); i++ {
            fmt.Printf(iff(row[0][i], "# ", ". "))
        }
        fmt.Println()
    }
    fmt.Println()
}

// collect all possible solutions for the given clues
func getCandidates(data []string, le int) [][]BitSet {
    var result [][]BitSet
    for _, s := range data {
        var lst []BitSet
        a := []byte(s)
        sumBytes := 0
        for _, b := range a {
            sumBytes += int(b - 'A' + 1)
        }
        prep := make([]string, len(a))
        for i, b := range a {
            prep[i] = strings.Repeat("1", int(b-'A'+1))
        }
        for _, r := range genSequence(prep, le-sumBytes+1) {
            bits := []byte(r[1:])
            bitset := make(BitSet, len(bits))
            for i, b := range bits {
                bitset[i] = b == '1'
            }
            lst = append(lst, bitset)
        }
        result = append(result, lst)
    }
    return result
}

func genSequence(ones []string, numZeros int) []string {
    le := len(ones)
    if le == 0 {
        return []string{strings.Repeat("0", numZeros)}
    }
    var result []string
    for x := 1; x < numZeros-le+2; x++ {
        skipOne := ones[1:]
        for _, tail := range genSequence(skipOne, numZeros-x) {
            result = append(result, strings.Repeat("0", x)+ones[0]+tail)
        }
    }
    return result
}

/* If all the candidates for a row have a value in common for a certain cell,
   then it's the only possible outcome, and all the candidates from the
   corresponding column need to have that value for that cell too. The ones
   that don't, are removed. The same for all columns. It goes back and forth,
   until no more candidates can be removed or a list is empty (failure).
*/

func reduceMutual(cols, rows [][]BitSet) int {
    countRemoved1 := reduce(cols, rows)
    if countRemoved1 == -1 {
        return -1
    }
    countRemoved2 := reduce(rows, cols)
    if countRemoved2 == -1 {
        return -1
    }
    return countRemoved1 + countRemoved2
}

func reduce(a, b [][]BitSet) int {
    countRemoved := 0
    for i := 0; i < len(a); i++ {
        commonOn := make(BitSet, len(b))
        for j := 0; j < len(b); j++ {
            commonOn[j] = true
        }
        commonOff := make(BitSet, len(b))

        // determine which values all candidates of a[i] have in common
        for _, candidate := range a[i] {
            commonOn.and(candidate)
            commonOff.or(candidate)
        }

        // remove from b[j] all candidates that don't share the forced values
        for j := 0; j < len(b); j++ {
            fi, fj := i, j
            for k := len(b[j]) - 1; k >= 0; k-- {
                cnd := b[j][k]
                if (commonOn[fj] && !cnd[fi]) || (!commonOff[fj] && cnd[fi]) {
                    lb := len(b[j])
                    copy(b[j][k:], b[j][k+1:])
                    b[j][lb-1] = nil
                    b[j] = b[j][:lb-1]
                    countRemoved++
                }
            }
            if len(b[j]) == 0 {
                return -1
            }
        }
    }
    return countRemoved
}

func main() {
    p1 := [2]string{"C BA CB BB F AE F A B", "AB CA AE GA E C D C"}

    p2 := [2]string{
        "F CAC ACAC CN AAA AABB EBB EAA ECCC HCCC",
        "D D AE CD AE A DA BBB CC AAB BAA AAB DA AAB AAA BAB AAA CD BBA DA",
    }

    p3 := [2]string{
        "CA BDA ACC BD CCAC CBBAC BBBBB BAABAA ABAD AABB BBH " +
            "BBBD ABBAAA CCEA AACAAB BCACC ACBH DCH ADBE ADBB DBE ECE DAA DB CC",
        "BC CAC CBAB BDD CDBDE BEBDF ADCDFA DCCFB DBCFC ABDBA BBF AAF BADB DBF " +
            "AAAAD BDG CEF CBDB BBB FC",
    }

    p4 := [2]string{
        "E BCB BEA BH BEK AABAF ABAC BAA BFB OD JH BADCF Q Q R AN AAN EI H G",
        "E CB BAB AAA AAA AC BB ACC ACCA AGB AIA AJ AJ " +
            "ACE AH BAF CAG DAG FAH FJ GJ ADK ABK BL CM",
    }

    for _, puzzleData := range [][2]string{p1, p2, p3, p4} {
        newPuzzle(puzzleData)
    }
}
```


```txt

. # # # . . . .
# # . # . . . .
. # # # . . # #
. . # # . . # #
. . # # # # # #
# . # # # # # .
# # # # # # . .
. . . . # . . .
. . . # # . . .

. . . . . . . . . . # # # # # # . . . .
. . . . . . . . # # # . # . . # # # . .
. . . # . . # # # . . . # . . . . # # #
. . # # # . # # # # # # # # # # # # # #
. . . # . . # . . . . . . . . . . . . #
. . # . # . # # . . . . . . . . . . # #
# # # # # . . # # . . . . . . . . # # .
# # # # # . . . # . . . . . . . . # . .
# # # # # . . # # # . # # # . # # # . .
# # # # # # # # . # # # . # # # . # # #

. . . . # # # . # . . . . . . . . . . .
. . . . # # . # # # # . # . . . . . . .
. . . . # . # # # . # # # . . . . . . .
. . # # . # # # # . . . . . . . . . . .
. # # # . # # # . # . . . . # # # . . .
# # # . . # # . # # . . . # . # # # . .
# # . . # # . # # . . . . # # . # # . .
. . . . # # . # . # . . # # . # . # . .
. . . . # . # # . # . . . # # # # . . .
. . . . # . # . # # . . . . . # # . . .
. . . . . # # . # # . . # # # # # # # #
. . . . # # . # # . . . # # . . # # # #
. . . . # . # # . # # . # . . . # . . #
# # # . . # # # . # # # # # . . . . . #
# . # . # # # . # . . . . # . . . . # #
# # . . # # # . # . . . . # # # . # # #
. # . # # # . # # . # # # # # # # # . .
. # # # # . # # # . # # # # # # # # . .
. . . # . # # # # . # # . # # # # # . .
. . . # . # # # # . # # . . . # # . . .
. . . . # # # # . . # # . . . # # # # #
. . . # # # # # . # # # . . . # # # # #
. . . # # # # . # . . . . . . . . . . #
. . # # # # . # # . . . . . . . . . . .
. . # # # . # # # . . . . . . . . . . .

. . . . . . . . . . . . . . . . . . . . # # # # #
. . # # . . . . . . . . . . . . . . # # # . . # #
. # # . . . . . . . . . . . . . . # # # # # . . #
# # . . . . . . . . . . . . . # # # # # # # # . .
# # . . . . # # # # # . # # # # # # # # # # # . .
# . # . . # # . . . . # . . . . # # # # # # . . .
# . . # # . . . . . # . . . . . . . # # # . . . .
# # . . . . . . . . # . . . . . . . . . . . . . #
. # # . . . . . # # # # # # . . . . . . . . . # #
. . # # # # # # # # # # # # # # # . . . . # # # #
. . . . . # # # # # # # # # # . . # # # # # # # #
. . . . # # . # . # # # # . # # # . . # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . # # # # # # # # # # # # # # # # # #
. . . . . . . # . . . # # # # # # # # # # # # # #
. . . . . . . # . # . # # # # # # # # # # # # # #
. . . . . . . . # # # # # . . . # # # # # # # # #
. . . . . . . . . . . . . . . . . # # # # # # # #
. . . . . . . . . . . . . . . . . . # # # # # # #

```



## Java

```java
import java.util.*;
import static java.util.Arrays.*;
import static java.util.stream.Collectors.toList;

public class NonogramSolver {

    static String[] p1 = {"C BA CB BB F AE F A B", "AB CA AE GA E C D C"};

    static String[] p2 = {"F CAC ACAC CN AAA AABB EBB EAA ECCC HCCC", "D D AE "
        + "CD AE A DA BBB CC AAB BAA AAB DA AAB AAA BAB AAA CD BBA DA"};

    static String[] p3 = {"CA BDA ACC BD CCAC CBBAC BBBBB BAABAA ABAD AABB BBH "
        + "BBBD ABBAAA CCEA AACAAB BCACC ACBH DCH ADBE ADBB DBE ECE DAA DB CC",
        "BC CAC CBAB BDD CDBDE BEBDF ADCDFA DCCFB DBCFC ABDBA BBF AAF BADB DBF "
        + "AAAAD BDG CEF CBDB BBB FC"};

    static String[] p4 = {"E BCB BEA BH BEK AABAF ABAC BAA BFB OD JH BADCF Q Q "
        + "R AN AAN EI H G", "E CB BAB AAA AAA AC BB ACC ACCA AGB AIA AJ AJ "
        + "ACE AH BAF CAG DAG FAH FJ GJ ADK ABK BL CM"};

    public static void main(String[] args) {
        for (String[] puzzleData : new String[][]{p1, p2, p3, p4})
            newPuzzle(puzzleData);
    }

    static void newPuzzle(String[] data) {
        String[] rowData = data[0].split("\\s");
        String[] colData = data[1].split("\\s");

        List<List<BitSet>> cols, rows;
        rows = getCandidates(rowData, colData.length);
        cols = getCandidates(colData, rowData.length);

        int numChanged;
        do {
            numChanged = reduceMutual(cols, rows);
            if (numChanged == -1) {
                System.out.println("No solution");
                return;
            }
        } while (numChanged > 0);

        for (List<BitSet> row : rows) {
            for (int i = 0; i < cols.size(); i++)
                System.out.print(row.get(0).get(i) ? "# " : ". ");
            System.out.println();
        }
        System.out.println();
    }

    // collect all possible solutions for the given clues
    static List<List<BitSet>> getCandidates(String[] data, int len) {
        List<List<BitSet>> result = new ArrayList<>();

        for (String s : data) {
            List<BitSet> lst = new LinkedList<>();

            int sumChars = s.chars().map(c -> c - 'A' + 1).sum();
            List<String> prep = stream(s.split(""))
                    .map(x -> repeat(x.charAt(0) - 'A' + 1, "1")).collect(toList());

            for (String r : genSequence(prep, len - sumChars + 1)) {
                char[] bits = r.substring(1).toCharArray();
                BitSet bitset = new BitSet(bits.length);
                for (int i = 0; i < bits.length; i++)
                    bitset.set(i, bits[i] == '1');
                lst.add(bitset);
            }
            result.add(lst);
        }
        return result;
    }

    // permutation generator, translated from Python via D
    static List<String> genSequence(List<String> ones, int numZeros) {
        if (ones.isEmpty())
            return asList(repeat(numZeros, "0"));

        List<String> result = new ArrayList<>();
        for (int x = 1; x < numZeros - ones.size() + 2; x++) {
            List<String> skipOne = ones.stream().skip(1).collect(toList());
            for (String tail : genSequence(skipOne, numZeros - x))
                result.add(repeat(x, "0") + ones.get(0) + tail);
        }
        return result;
    }

    static String repeat(int n, String s) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++)
            sb.append(s);
        return sb.toString();
    }

    /* If all the candidates for a row have a value in common for a certain cell,
    then it's the only possible outcome, and all the candidates from the
    corresponding column need to have that value for that cell too. The ones
    that don't, are removed. The same for all columns. It goes back and forth,
    until no more candidates can be removed or a list is empty (failure). */

    static int reduceMutual(List<List<BitSet>> cols, List<List<BitSet>> rows) {
        int countRemoved1 = reduce(cols, rows);
        if (countRemoved1 == -1)
            return -1;

        int countRemoved2 = reduce(rows, cols);
        if (countRemoved2 == -1)
            return -1;

        return countRemoved1 + countRemoved2;
    }

    static int reduce(List<List<BitSet>> a, List<List<BitSet>> b) {
        int countRemoved = 0;

        for (int i = 0; i < a.size(); i++) {

            BitSet commonOn = new BitSet();
            commonOn.set(0, b.size());
            BitSet commonOff = new BitSet();

            // determine which values all candidates of ai have in common
            for (BitSet candidate : a.get(i)) {
                commonOn.and(candidate);
                commonOff.or(candidate);
            }

            // remove from bj all candidates that don't share the forced values
            for (int j = 0; j < b.size(); j++) {
                final int fi = i, fj = j;

                if (b.get(j).removeIf(cnd -> (commonOn.get(fj) && !cnd.get(fi))
                        || (!commonOff.get(fj) && cnd.get(fi))))
                    countRemoved++;

                if (b.get(j).isEmpty())
                    return -1;
            }
        }
        return countRemoved;
    }
}
```


```txt
. # # # . . . .
# # . # . . . .
. # # # . . # #
. . # # . . # #
. . # # # # # #
# . # # # # # .
# # # # # # . .
. . . . # . . .
. . . # # . . .

. . . . . . . . . . # # # # # # . . . .
. . . . . . . . # # # . # . . # # # . .
. . . # . . # # # . . . # . . . . # # #
. . # # # . # # # # # # # # # # # # # #
. . . # . . # . . . . . . . . . . . . #
. . # . # . # # . . . . . . . . . . # #
# # # # # . . # # . . . . . . . . # # .
# # # # # . . . # . . . . . . . . # . .
# # # # # . . # # # . # # # . # # # . .
# # # # # # # # . # # # . # # # . # # #

. . . . # # # . # . . . . . . . . . . .
. . . . # # . # # # # . # . . . . . . .
. . . . # . # # # . # # # . . . . . . .
. . # # . # # # # . . . . . . . . . . .
. # # # . # # # . # . . . . # # # . . .
# # # . . # # . # # . . . # . # # # . .
# # . . # # . # # . . . . # # . # # . .
. . . . # # . # . # . . # # . # . # . .
. . . . # . # # . # . . . # # # # . . .
. . . . # . # . # # . . . . . # # . . .
. . . . . # # . # # . . # # # # # # # #
. . . . # # . # # . . . # # . . # # # #
. . . . # . # # . # # . # . . . # . . #
# # # . . # # # . # # # # # . . . . . #
# . # . # # # . # . . . . # . . . . # #
# # . . # # # . # . . . . # # # . # # #
. # . # # # . # # . # # # # # # # # . .
. # # # # . # # # . # # # # # # # # . .
. . . # . # # # # . # # . # # # # # . .
. . . # . # # # # . # # . . . # # . . .
. . . . # # # # . . # # . . . # # # # #
. . . # # # # # . # # # . . . # # # # #
. . . # # # # . # . . . . . . . . . . #
. . # # # # . # # . . . . . . . . . . .
. . # # # . # # # . . . . . . . . . . .

. . . . . . . . . . . . . . . . . . . . # # # # #
. . # # . . . . . . . . . . . . . . # # # . . # #
. # # . . . . . . . . . . . . . . # # # # # . . #
# # . . . . . . . . . . . . . # # # # # # # # . .
# # . . . . # # # # # . # # # # # # # # # # # . .
# . # . . # # . . . . # . . . . # # # # # # . . .
# . . # # . . . . . # . . . . . . . # # # . . . .
# # . . . . . . . . # . . . . . . . . . . . . . #
. # # . . . . . # # # # # # . . . . . . . . . # #
. . # # # # # # # # # # # # # # # . . . . # # # #
. . . . . # # # # # # # # # # . . # # # # # # # #
. . . . # # . # . # # # # . # # # . . # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . # # # # # # # # # # # # # # # # # #
. . . . . . . # . . . # # # # # # # # # # # # # #
. . . . . . . # . # . # # # # # # # # # # # # # #
. . . . . . . . # # # # # . . . # # # # # # # # #
. . . . . . . . . . . . . . . . . # # # # # # # #
. . . . . . . . . . . . . . . . . . # # # # # # #
```




## Julia


```julia
using Base.Iterators

struct NonogramPuzzle
    nrows::Int
    ncols::Int
    xhints::Vector{Vector{Int}}
    yhints::Vector{Vector{Int}}
    solutions:: Vector{Any}
    NonogramPuzzle(xh, yh) = new(length(xh), length(yh), xh, yh, Vector{NTuple{4,Array{Int64,1}}}())
end

ycols2xrows(ycols) = [[ycols[i][j] for i in eachindex(ycols)] for j in eachindex(ycols[1])]

function hintsfromcol(rowvec, col, nrows)
    hints = Vector{Int}()
    hintrun = 0
    for row in rowvec
        if row[col] != 0
            hintrun += 1
            if col == nrows
                push!(hints, hintrun)
            end
        elseif hintrun > 0
            push!(hints, hintrun)
            hintrun = 0
        end
    end
    hints
end

function nonoblocks(hints, len)
    minsized(arr) = vcat(map(x -> vcat(fill(1, x), [0]), arr)...)[1:end-1]
    minlen(arr) = sum(arr) + length(arr) - 1
    if isempty(hints)
        return fill(0, len)
    elseif minlen(hints) == len
        return minsized(hints)
    end
    possibilities = Vector{Vector{Int}}()
    allbuthead = hints[2:end]
    for leftspace in 0:(len - minlen(hints))
        header = vcat(fill(0, leftspace), fill(1, hints[1]), [0])
        rightspace = len - length(header)
        if isempty(allbuthead)
            push!(possibilities, rightspace <= 0 ? header[1:len] : vcat(header, fill(0, rightspace)))
        elseif minlen(allbuthead) == rightspace
            push!(possibilities, vcat(header, minsized(allbuthead)))
        else
            foreach(x -> push!(possibilities, vcat(header, x)), nonoblocks(allbuthead, rightspace))
        end
    end
    possibilities
end

function exclude!(xchoices, ychoices)
    andvec(a) = findall(x -> x == 1, foldl((x, y) -> [x[i] & y[i] for i in 1:length(x)], a))
    orvec(a)  = findall(x -> x == 0, foldl((x, y) -> [x[i] | y[i] for i in 1:length(x)], a))
    filterbyval!(arr, val, pos) =  if !isempty(arr) filter!(x -> x[pos] == val, arr); end
    ensurevecvec(arr::Vector{Vector{Int}}) = arr
    ensurevecvec(arr::Vector{Int}) = [arr]
    function excl!(choices, otherchoices)
        for i in 1:length(choices)
            if length(choices[i]) > 0
                all1 = andvec(choices[i])
                all0 = orvec(choices[i])
                foreach(n -> filterbyval!(otherchoices[n], 1, i), all1)
                foreach(n -> filterbyval!(otherchoices[n], 0, i), all0)
            end
        end
    end
    xclude!(x, y) = (excl!(x, y); x = map(ensurevecvec, x); y = map(ensurevecvec, y); (x, y))
    xlen, ylen = sum(map(length, xchoices)), sum(map(length, ychoices))
    while true
        ychoices, xchoices = xclude!(ychoices, xchoices)
        if any(isempty, xchoices)
            return
        end
        xchoices, ychoices = xclude!(xchoices, ychoices)
        if any(isempty, ychoices)
            return
        end
        newxlen, newylen = sum(map(length, xchoices)), sum(map(length, ychoices))
        if newxlen == xlen && newylen == ylen
            return
        end
        xlen, ylen = newxlen, newylen
    end
end

function trygrids(nonogram)
    xchoices = [nonoblocks(nonogram.xhints[i], nonogram.ncols) for i in 1:nonogram.nrows]
    ychoices = [nonoblocks(nonogram.yhints[i], nonogram.nrows) for i in 1:nonogram.ncols]
    exclude!(xchoices, ychoices)
    if all(x -> length(x) == 1, xchoices)
        println("Unique solution.")
        push!(nonogram.solutions, [x[1] for x in xchoices])
    elseif all(x -> length(x) == 1, ychoices)
        println("Unique solution.")
        ycols = [y[1] for y in ychoices]
        push!(nonogram.solutions, ycols2xrows(ycols))
    else
        println("Brute force: $(prod(map(length, xchoices))) possibilities.")
        for stack in product(xchoices...)
            arr::Vector{Vector{Int}} = [i isa Vector ? i : [i] for i in stack]
            if all(x -> length(x) == nonogram.ncols, arr) &&
               all(y -> hintsfromcol(arr, y, nonogram.nrows) == nonogram.yhints[y], 1:nonogram.ncols)
                push!(nonogram.solutions, arr)
            end
        end
        nsoln = length(nonogram.solutions)
        println(nsoln == 0 ? "No" : nsoln, " solutions.")
    end
end

# The first puzzle below requires brute force, and the second has no solutions.
const testnonograms = """
B B A A
B B A A

B A A
A A A

C BA CB BB F AE F A B
AB CA AE GA E C D C

F CAC ACAC CN AAA AABB EBB EAA ECCC HCCC
D D AE CD AE A DA BBB CC AAB BAA AAB DA AAB AAA BAB AAA CD BBA DA

CA BDA ACC BD CCAC CBBAC BBBBB BAABAA ABAD AABB BBH BBBD ABBAAA CCEA AACAAB BCACC ACBH DCH ADBE ADBB DBE ECE DAA DB CC
BC CAC CBAB BDD CDBDE BEBDF ADCDFA DCCFB DBCFC ABDBA BBF AAF BADB DBF AAAAD BDG CEF CBDB BBB FC

E BCB BEA BH BEK AABAF ABAC BAA BFB OD JH BADCF Q Q R AN AAN EI H G
E CB BAB AAA AAA AC BB ACC ACCA AGB AIA AJ AJ ACE AH BAF CAG DAG FAH FJ GJ ADK ABK BL CM
"""

function processtestpuzzles(txt)
    solutiontxt(a) = (s = ""; for r in a for c in r; s *= (c == 0 ? "." : "#") end; s *= "\n" end; s)
    txtline2ints(s) = [[UInt8(ch - 'A' + 1) for ch in r] for r in split(s, r"\s+")]
    linepairs = uppercase.(string.(split(txt, "\n\n")))
    pcount = 0
    for xyhints in linepairs
        xh, yh = map(x -> txtline2ints(strip(x)), split(xyhints, "\n"))
        nonogram = NonogramPuzzle(xh, yh)
        println("\nPuzzle $(pcount += 1):")
        trygrids(nonogram)
        foreach(x -> println(solutiontxt(x), "\n"), nonogram.solutions)
    end
end

processtestpuzzles(testnonograms)

```


```txt

Puzzle 1:
Brute force: 144 possibilities.
2 solutions.
.##.
##..
#...
...#


##..
##..
..#.
...#




Puzzle 2:
Brute force: 8 possibilities.
No solutions.


Puzzle 3:
Unique solution.
.###....
##.#....
.###..##
..##..##
..######
#.#####.
######..
....#...
...##...




Puzzle 4:
Unique solution.
..........######....
........###.#..###..
...#..###...#....###
..###.##############
...#..#............#
..#.#.##..........##
#####..##........##.
#####...#........#..
#####..###.###.###..
########.###.###.###




Puzzle 5:
Unique solution.
....###.#...........
....##.####.#.......
....#.###.###.......
..##.####...........
.###.###.#....###...
###..##.##...#.###..
##..##.##....##.##..
....##.#.#..##.#.#..
....#.##.#...####...
....#.#.##.....##...
.....##.##..########
....##.##...##..####
....#.##.##.#...#..#
###..###.#####.....#
#.#.###.#....#....##
##..###.#....###.###
.#.###.##.########..
.####.###.########..
...#.####.##.#####..
...#.####.##...##...
....####..##...#####
...#####.###...#####
...####.#..........#
..####.##...........
..###.###...........




Puzzle 6:
Unique solution.
....................#####
..##..............###..##
.##..............#####..#
##.............########..
##....#####.###########..
#.#..##....#....######...
#..##.....#.......###....
##........#.............#
.##.....######.........##
..###############....####
.....##########..########
....##.#.####.###..######
........#################
........#################
.......##################
.......#...##############
.......#.#.##############
........#####...#########
.................########
..................#######
```



## Kotlin

```scala
// version 1.2.0

import java.util.BitSet

typealias BitSets = List<MutableList<BitSet>>

val rx = Regex("""\s""")

fun newPuzzle(data: List<String>) {
    val rowData = data[0].split(rx)
    val colData = data[1].split(rx)
    val rows = getCandidates(rowData, colData.size)
    val cols = getCandidates(colData, rowData.size)

    do {
        val numChanged = reduceMutual(cols, rows)
        if (numChanged == -1) {
            println("No solution")
            return
        }
    }
    while (numChanged > 0)

    for (row in rows) {
        for (i in 0 until cols.size) {
            print(if (row[0][i]) "# " else ". ")
        }
        println()
    }
    println()
}

// collect all possible solutions for the given clues
fun getCandidates(data: List<String>, len: Int): BitSets {
    val result = mutableListOf<MutableList<BitSet>>()
    for (s in data) {
        val lst = mutableListOf<BitSet>()
        val a = s.toCharArray()
        val sumChars = a.sumBy { it - 'A' + 1 }
        val prep = a.map { "1".repeat(it - 'A' + 1) }

        for (r in genSequence(prep, len - sumChars + 1)) {
            val bits = r.substring(1).toCharArray()
            val bitset = BitSet(bits.size)
            for (i in 0 until bits.size) bitset[i] = bits[i] == '1'
            lst.add(bitset)
        }
        result.add(lst)
    }
    return result
}

fun genSequence(ones: List<String>, numZeros: Int): List<String> {
    if (ones.isEmpty()) return listOf("0".repeat(numZeros))
    val result = mutableListOf<String>()
    for (x in 1 until numZeros - ones.size + 2) {
        val skipOne = ones.drop(1)
        for (tail in genSequence(skipOne, numZeros - x)) {
            result.add("0".repeat(x) + ones[0] + tail)
        }
    }
    return result
}

/* If all the candidates for a row have a value in common for a certain cell,
    then it's the only possible outcome, and all the candidates from the
    corresponding column need to have that value for that cell too. The ones
    that don't, are removed. The same for all columns. It goes back and forth,
    until no more candidates can be removed or a list is empty (failure).
*/

fun reduceMutual(cols: BitSets, rows: BitSets): Int {
    val countRemoved1 = reduce(cols, rows)
    if (countRemoved1 == -1) return -1
    val countRemoved2 = reduce(rows, cols)
    if (countRemoved2 == -1) return -1
    return countRemoved1 + countRemoved2
}

fun reduce(a: BitSets, b: BitSets): Int {
    var countRemoved = 0
    for (i in 0 until a.size) {
        val commonOn = BitSet()
        commonOn[0] = b.size
        val commonOff = BitSet()

        // determine which values all candidates of a[i] have in common
        for (candidate in a[i]) {
            commonOn.and(candidate)
            commonOff.or(candidate)
        }

        // remove from b[j] all candidates that don't share the forced values
        for (j in 0 until b.size) {
            val fi = i
            val fj = j
            if (b[j].removeIf { cnd ->
                (commonOn[fj] && !cnd[fi]) ||
                (!commonOff[fj] && cnd[fi]) }) countRemoved++
            if (b[j].isEmpty()) return -1
        }
    }
    return countRemoved
}

val p1 = listOf("C BA CB BB F AE F A B", "AB CA AE GA E C D C")

val p2 = listOf(
    "F CAC ACAC CN AAA AABB EBB EAA ECCC HCCC",
    "D D AE CD AE A DA BBB CC AAB BAA AAB DA AAB AAA BAB AAA CD BBA DA"
)

val p3 = listOf(
    "CA BDA ACC BD CCAC CBBAC BBBBB BAABAA ABAD AABB BBH " +
    "BBBD ABBAAA CCEA AACAAB BCACC ACBH DCH ADBE ADBB DBE ECE DAA DB CC",
    "BC CAC CBAB BDD CDBDE BEBDF ADCDFA DCCFB DBCFC ABDBA BBF AAF BADB DBF " +
    "AAAAD BDG CEF CBDB BBB FC"
)

val p4 = listOf(
    "E BCB BEA BH BEK AABAF ABAC BAA BFB OD JH BADCF Q Q R AN AAN EI H G",
    "E CB BAB AAA AAA AC BB ACC ACCA AGB AIA AJ AJ " +
    "ACE AH BAF CAG DAG FAH FJ GJ ADK ABK BL CM"
)

fun main(args: Array<String>) {
    for (puzzleData in listOf(p1, p2, p3, p4)) {
        newPuzzle(puzzleData)
    }
}
```


```txt

. # # # . . . .
# # . # . . . .
. # # # . . # #
. . # # . . # #
. . # # # # # #
# . # # # # # .
# # # # # # . .
. . . . # . . .
. . . # # . . .

. . . . . . . . . . # # # # # # . . . .
. . . . . . . . # # # . # . . # # # . .
. . . # . . # # # . . . # . . . . # # #
. . # # # . # # # # # # # # # # # # # #
. . . # . . # . . . . . . . . . . . . #
. . # . # . # # . . . . . . . . . . # #
# # # # # . . # # . . . . . . . . # # .
# # # # # . . . # . . . . . . . . # . .
# # # # # . . # # # . # # # . # # # . .
# # # # # # # # . # # # . # # # . # # #

. . . . # # # . # . . . . . . . . . . .
. . . . # # . # # # # . # . . . . . . .
. . . . # . # # # . # # # . . . . . . .
. . # # . # # # # . . . . . . . . . . .
. # # # . # # # . # . . . . # # # . . .
# # # . . # # . # # . . . # . # # # . .
# # . . # # . # # . . . . # # . # # . .
. . . . # # . # . # . . # # . # . # . .
. . . . # . # # . # . . . # # # # . . .
. . . . # . # . # # . . . . . # # . . .
. . . . . # # . # # . . # # # # # # # #
. . . . # # . # # . . . # # . . # # # #
. . . . # . # # . # # . # . . . # . . #
# # # . . # # # . # # # # # . . . . . #
# . # . # # # . # . . . . # . . . . # #
# # . . # # # . # . . . . # # # . # # #
. # . # # # . # # . # # # # # # # # . .
. # # # # . # # # . # # # # # # # # . .
. . . # . # # # # . # # . # # # # # . .
. . . # . # # # # . # # . . . # # . . .
. . . . # # # # . . # # . . . # # # # #
. . . # # # # # . # # # . . . # # # # #
. . . # # # # . # . . . . . . . . . . #
. . # # # # . # # . . . . . . . . . . .
. . # # # . # # # . . . . . . . . . . .

. . . . . . . . . . . . . . . . . . . . # # # # #
. . # # . . . . . . . . . . . . . . # # # . . # #
. # # . . . . . . . . . . . . . . # # # # # . . #
# # . . . . . . . . . . . . . # # # # # # # # . .
# # . . . . # # # # # . # # # # # # # # # # # . .
# . # . . # # . . . . # . . . . # # # # # # . . .
# . . # # . . . . . # . . . . . . . # # # . . . .
# # . . . . . . . . # . . . . . . . . . . . . . #
. # # . . . . . # # # # # # . . . . . . . . . # #
. . # # # # # # # # # # # # # # # . . . . # # # #
. . . . . # # # # # # # # # # . . # # # # # # # #
. . . . # # . # . # # # # . # # # . . # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . . # # # # # # # # # # # # # # # # #
. . . . . . . # # # # # # # # # # # # # # # # # #
. . . . . . . # . . . # # # # # # # # # # # # # #
. . . . . . . # . # . # # # # # # # # # # # # # #
. . . . . . . . # # # # # . . . # # # # # # # # #
. . . . . . . . . . . . . . . . . # # # # # # # #
. . . . . . . . . . . . . . . . . . # # # # # # #

```



## Perl


```perl
use strict;
use warnings;

my $file = 'nonogram_problems.txt';
open my $fd, '<', $file or die "$! opening $file";

while(my $row = <$fd> )
  {
  $row =~ /\S/ or next;
  my $column = <$fd>;
  my @rpats = makepatterns($row);
  my @cpats = makepatterns($column);
  my @rows = ( '.' x @cpats ) x @rpats;
  for( my $prev = ''; $prev ne "@rows"; )
    {
    $prev = "@rows";
    try(\@rows, \@rpats);
    my @cols = map { join '', map { s/.//; $& } @rows } 0..$#cpats;
    try(\@cols, \@cpats);
    @rows = map { join '', map { s/.//; $& } @cols } 0..$#rpats;
    }
  print "\n", "@rows" =~ /\./ ? "Failed\n" : map { tr/01/.#/r, "\n" } @rows;
  }

sub try
  {
  my ($lines, $patterns) = @_;
  for my $i ( 0 .. $#$lines )
    {
    while( $lines->[$i] =~ /\./g )
      {
      for my $try ( 0, 1 )
        {
        $lines->[$i] =~ s/.\G/$try/r =~ $patterns->[$i] or
          $lines->[$i] =~ s// 1 - $try /e;
        }
      }
    }
  }

sub makepatterns {
    map { qr/^$_$/                          # convert strings to regex
        } map {  '[0.]*'                    # prepend static pattern
               . join('[0.]+',              # join with static pattern
                       map { "[1.]{$_}"     # require to match exactly 'n' times
                           } map { -64+ord  # convert letter value to repetition count 'n'
                                 } split // # for each letter in group
                     )
               . '[0.]*'                    # append static pattern
              } split ' ', shift;           # for each letter grouping
}
```

```txt

.###....
##.#....
.###..##
..##..##
..######
#.#####.
######..
....#...
...##...

..........######....
........###.#..###..
...#..###...#....###
..###.##############
...#..#............#
..#.#.##..........##
#####..##........##.
#####...#........#..
#####..###.###.###..
########.###.###.###

....###.#...........
....##.####.#.......
....#.###.###.......
..##.####...........
.###.###.#....###...
###..##.##...#.###..
##..##.##....##.##..
....##.#.#..##.#.#..
....#.##.#...####...
....#.#.##.....##...
.....##.##..########
....##.##...##..####
....#.##.##.#...#..#
###..###.#####.....#
#.#.###.#....#....##
##..###.#....###.###
.#.###.##.########..
.####.###.########..
...#.####.##.#####..
...#.####.##...##...
....####..##...#####
...#####.###...#####
...####.#..........#
..####.##...........
..###.###...........

....................#####
..##..............###..##
.##..............#####..#
##.............########..
##....#####.###########..
#.#..##....#....######...
#..##.....#.......###....
##........#.............#
.##.....######.........##
..###############....####
.....##########..########
....##.#.####.###..######
........#################
........#################
.......##################
.......#...##############
.......#.#.##############
........#####...#########
.................########
..................#######

```



## Phix

Deduction only, no exhaustive search.

```Phix
sequence x, y, grid
integer unsolved

function count_grid()
integer res = length(x)*length(y)
    for i=1 to length(x) do
        for j=1 to length(y) do
            res -= grid[i][j]!='?'
        end for
    end for
    return res
end function

function match_mask(string neat, string mask, integer ms, integer me)
    for i=ms to me do
        if mask[i]!='?' then
            if mask[i]!=neat[i] then return 0 end if
        end if
    end for
    return 1
end function

function innr(string mask, sequence blocks, integer mi=1, string res="", string neat=mask)
    if length(blocks)=0 then
        for i=mi to length(neat) do
            neat[i] = ' '
        end for
        if match_mask(neat,mask,mi,length(mask)) then
            if length(res)=0 then
                res = neat
            else
                for i=1 to length(neat) do
                    if neat[i]!=res[i] then
                        res[i] = '?'
                    end if
                end for
            end if
        end if
    else
        integer b = blocks[1]
        blocks = blocks[2..$]
        integer l = (sum(blocks)+length(blocks)-1),
                e = length(neat)-l-b
        for i=mi to e do
            for j=i to i+b-1 do
                neat[j] = '#'
            end for
            if i+b<=length(neat) then
                neat[i+b] = ' '
            end if
            if match_mask(neat,mask,mi,min(i+b,length(mask))) then
                res = innr(mask,blocks,i+b+1,res,neat)
            end if
            neat[i] = ' '
        end for
    end if
    return res
end function

function inner(string mask, sequence blocks)
    string res = innr(mask,blocks)
    return iff(length(res)?res:mask)
end function

global function vmask(sequence source, integer column)
string res = repeat(' ',length(source))
    for i=1 to length(source) do
        res[i] = source[i][column]
    end for
    return res
end function

function logic()
integer wasunsolved = unsolved
    for i=1 to length(x) do
        grid[i] = inner(grid[i],x[i])
    end for
    for j=1 to length(y) do
        string tmp = inner(vmask(grid,j),y[j])
        for i=1 to length(tmp) do
            grid[i][j] = tmp[i]
        end for
    end for
    unsolved = count_grid()
    return wasunsolved!=unsolved
end function

sequence tests=split("""
C BA CB BB F AE F A B
AB CA AE GA E C D C

F CAC ACAC CN AAA AABB EBB EAA ECCC HCCC
D D AE CD AE A DA BBB CC AAB BAA AAB DA AAB AAA BAB AAA CD BBA DA

CA BDA ACC BD CCAC CBBAC BBBBB BAABAA ABAD AABB BBH BBBD ABBAAA CCEA AACAAB BCACC ACBH DCH ADBE ADBB DBE ECE DAA DB CC
BC CAC CBAB BDD CDBDE BEBDF ADCDFA DCCFB DBCFC ABDBA BBF AAF BADB DBF AAAAD BDG CEF CBDB BBB FC

E BCB BEA BH BEK AABAF ABAC BAA BFB OD JH BADCF Q Q R AN AAN EI H G
E CB BAB AAA AAA AC BB ACC ACCA AGB AIA AJ AJ ACE AH BAF CAG DAG FAH FJ GJ ADK ABK BL CM""",'\n')
--Alternatively:
--integer fn = open("nonogram_problems.txt","r")
--tests = get_text(fn,GT_LF_STRIPPED)
--close(fn)

function unpack(string s)
sequence res = split(s)
    for i=1 to length(res) do
        string ri = res[i]
        sequence r = {}
        for j=1 to length(ri) do
            r &= ri[j]-'A'+1
        end for
        res[i] = r
    end for
    return res
end function

for i=1 to length(tests) by 3 do
    x = unpack(tests[i])
    y = unpack(tests[i+1])
    grid = repeat(repeat('?',length(y)),length(x))
    unsolved = length(x)*length(y)

    while unsolved do
        if not logic() then
            ?"partial"
            exit
        end if
    end while

    puts(1,join(grid,"\n")&"\n")
end for
```

<pre style="float:left">
 ###
## #
 ###  ##
  ##  ##
  ######
# #####
######
    #
   ##

```

<pre style="float:left">
          ######
        ### #  ###
   #  ###   #    ###
  ### ##############
   #  #            #
  # # ##          ##
#####  ##        ##
#####   #        #
#####  ### ### ###
######## ### ### ###

```

<pre style="font-size: 10px; float:left">
    ### #
    ## #### #
    # ### ###
  ## ####
 ### ### #    ###
###  ## ##   # ###
##  ## ##    ## ##
    ## # #  ## # #
    # ## #   ####
    # # ##     ##
     ## ##  ########
    ## ##   ##  ####
    # ## ## #   #  #
###  ### #####     #
# # ### #    #    ##
##  ### #    ### ###
 # ### ## ########
 #### ### ########
   # #### ## #####
   # #### ##   ##
    ####  ##   #####
   ##### ###   #####
   #### #          #
  #### ##
  ### ###

```


```txt

                    #####
  ##              ###  ##
 ##              #####  #
##             ########
##    ##### ###########
# #  ##    #    ######
#  ##     #       ###
##        #             #
 ##     ######         ##
  ###############    ####
     ##########  ########
    ## # #### ###  ######
        #################
        #################
       ##################
       #   ##############
       # # ##############
        #####   #########
                 ########
                  #######

```



## Prolog

module(clpfd) is written by '''Markus Triska'''

Solution written by '''Lars Buitinck'''

Module solve-nonogram.pl

```Prolog
/*
* Nonogram/paint-by-numbers solver in SWI-Prolog. Uses CLP(FD),
* in particular the automaton/3 (finite-state/RE) constraint.
* Copyright (c) 2011 Lars Buitinck.
* Do with this code as you like, but don't remove the copyright notice.
*/

:- use_module(library(clpfd)).

nono(RowSpec, ColSpec, Grid) :-
	rows(RowSpec, Grid),
	transpose(Grid, GridT),
	rows(ColSpec, GridT).

rows([], []).
rows([C|Cs], [R|Rs]) :-
	row(C, R),
	rows(Cs, Rs).

row(Ks, Row) :-
	sum(Ks, #=, Ones),
	sum(Row, #=, Ones),
	arcs(Ks, Arcs, start, Final),
	append(Row, [0], RowZ),
	automaton(RowZ, [source(start), sink(Final)], [arc(start,0,start) | Arcs]).

% Make list of transition arcs for finite-state constraint.
arcs([], [], Final, Final).
arcs([K|Ks], Arcs, CurState, Final) :-
	gensym(state, NextState),
	(   K == 0
	->  Arcs = [arc(CurState,0,CurState), arc(CurState,0,NextState) | Rest],
	    arcs(Ks, Rest, NextState, Final)
	;   Arcs = [arc(CurState,1,NextState) | Rest],
	    K1 #= K-1,
	    arcs([K1|Ks], Rest, NextState, Final)).


make_grid(Grid, X, Y, Vars) :-
	length(Grid,X),
	make_rows(Grid, Y, Vars).

make_rows([], _, []).
make_rows([R|Rs], Len, Vars) :-
	length(R, Len),
	make_rows(Rs, Len, Vars0),
	append(R, Vars0, Vars).

print([]).
print([R|Rs]) :-
	print_row(R),
	print(Rs).

print_row([]) :- nl.
print_row([X|R]) :-
	(   X == 0
	->  write(' ')
	;   write('x')),
	print_row(R).

nonogram(Rows, Cols) :-
	length(Rows, X),
	length(Cols, Y),
	make_grid(Grid, X, Y, Vars),
	nono(Rows, Cols, Grid),
	label(Vars),
	print(Grid).
```

File nonogram.pl, used to read data in a file.

```Prolog
nonogram :-
	open('C:/Users/Utilisateur/Documents/Prolog/Rosetta/nonogram/nonogram.txt',
	     read, In, []),
	repeat,
	     read_line_to_codes(In, Line_1),
	     read_line_to_codes(In, Line_2),
	     compute_values(Line_1, [], [], Lines),
	     compute_values(Line_2, [], [], Columns),
	     nonogram(Lines, Columns) , nl, nl,
	read_line_to_codes(In, end_of_file),
	close(In).

compute_values([], Current, Tmp, R) :-
	reverse(Current, R_Current),
	reverse([R_Current | Tmp], R).

compute_values([32 | T], Current, Tmp, R) :-
	!,
	reverse(Current, R_Current),
	compute_values(T, [], [R_Current | Tmp], R).

compute_values([X | T], Current, Tmp, R) :-
	V is X - 64,
	compute_values(T, [V | Current], Tmp, R).
```



## Python

First fill cells by deduction, then search through all combinations.  It could take up a huge amount of storage, depending on the board size.


###  Python 2


```python
from itertools import izip

def gen_row(w, s):
    """Create all patterns of a row or col that match given runs."""
    def gen_seg(o, sp):
        if not o:
            return [[2] * sp]
        return [[2] * x + o[0] + tail
                for x in xrange(1, sp - len(o) + 2)
                for tail in gen_seg(o[1:], sp - x)]

    return [x[1:] for x in gen_seg([[1] * i for i in s], w + 1 - sum(s))]


def deduce(hr, vr):
    """Fix inevitable value of cells, and propagate."""
    def allowable(row):
        return reduce(lambda a, b: [x | y for x, y in izip(a, b)], row)

    def fits(a, b):
        return all(x & y for x, y in izip(a, b))

    def fix_col(n):
        """See if any value in a given column is fixed;
        if so, mark its corresponding row for future fixup."""
        c = [x[n] for x in can_do]
        cols[n] = [x for x in cols[n] if fits(x, c)]
        for i, x in enumerate(allowable(cols[n])):
            if x != can_do[i][n]:
                mod_rows.add(i)
                can_do[i][n] &= x

    def fix_row(n):
        """Ditto, for rows."""
        c = can_do[n]
        rows[n] = [x for x in rows[n] if fits(x, c)]
        for i, x in enumerate(allowable(rows[n])):
            if x != can_do[n][i]:
                mod_cols.add(i)
                can_do[n][i] &= x

    def show_gram(m):
        # If there's 'x', something is wrong.
        # If there's '?', needs more work.
        for x in m:
            print " ".join("x#.?"[i] for i in x)
        print

    w, h = len(vr), len(hr)
    rows = [gen_row(w, x) for x in hr]
    cols = [gen_row(h, x) for x in vr]
    can_do = map(allowable, rows)

    # Initially mark all columns for update.
    mod_rows, mod_cols = set(), set(xrange(w))

    while mod_cols:
        for i in mod_cols:
            fix_col(i)
        mod_cols = set()
        for i in mod_rows:
            fix_row(i)
        mod_rows = set()

    if all(can_do[i][j] in (1, 2) for j in xrange(w) for i in xrange(h)):
        print "Solution would be unique" # but could be incorrect!
    else:
        print "Solution may not be unique, doing exhaustive search:"

    # We actually do exhaustive search anyway. Unique solution takes
    # no time in this phase anyway, but just in case there's no
    # solution (could happen?).
    out = [0] * h

    def try_all(n = 0):
        if n >= h:
            for j in xrange(w):
                if [x[j] for x in out] not in cols[j]:
                    return 0
            show_gram(out)
            return 1
        sol = 0
        for x in rows[n]:
            out[n] = x
            sol += try_all(n + 1)
        return sol

    n = try_all()
    if not n:
        print "No solution."
    elif n == 1:
        print "Unique solution."
    else:
        print n, "solutions."
    print


def solve(p, show_runs=True):
    s = [[[ord(c) - ord('A') + 1 for c in w] for w in l.split()]
         for l in p.splitlines()]
    if show_runs:
        print "Horizontal runs:", s[0]
        print "Vertical runs:", s[1]
    deduce(s[0], s[1])


def main():
    # Read problems from file.
    fn = "nonogram_problems.txt"
    for p in (x for x in open(fn).read().split("\n\n") if x):
        solve(p)

    print "Extra example not solvable by deduction alone:"
    solve("B B A A\nB B A A")

    print "Extra example where there is no solution:"
    solve("B A A\nA A A")

main()
```

```txt

Horizontal runs: [[3], [2, 1], [3, 2], [2, 2], [6], [1, 5], [6], [1], [2]]
Vertical runs: [[1, 2], [3, 1], [1, 5], [7, 1], [5], [3], [4], [3]]
Solution would be unique
. # # # . . . .
# # . # . . . .
. # # # . . # #
. . # # . . # #
. . # # # # # #
# . # # # # # .
# # # # # # . .
. . . . # . . .
. . . # # . . .

Unique solution

(... etc. ...)

```



###  Python 3

Above code altered to work with Python 3:

```python
from functools import reduce

def gen_row(w, s):
    """Create all patterns of a row or col that match given runs."""
    def gen_seg(o, sp):
        if not o:
            return [[2] * sp]
        return [[2] * x + o[0] + tail
                for x in range(1, sp - len(o) + 2)
                for tail in gen_seg(o[1:], sp - x)]

    return [x[1:] for x in gen_seg([[1] * i for i in s], w + 1 - sum(s))]


def deduce(hr, vr):
    """Fix inevitable value of cells, and propagate."""
    def allowable(row):
        return reduce(lambda a, b: [x | y for x, y in zip(a, b)], row)

    def fits(a, b):
        return all(x & y for x, y in zip(a, b))

    def fix_col(n):
        """See if any value in a given column is fixed;
        if so, mark its corresponding row for future fixup."""
        c = [x[n] for x in can_do]
        cols[n] = [x for x in cols[n] if fits(x, c)]
        for i, x in enumerate(allowable(cols[n])):
            if x != can_do[i][n]:
                mod_rows.add(i)
                can_do[i][n] &= x

    def fix_row(n):
        """Ditto, for rows."""
        c = can_do[n]
        rows[n] = [x for x in rows[n] if fits(x, c)]
        for i, x in enumerate(allowable(rows[n])):
            if x != can_do[n][i]:
                mod_cols.add(i)
                can_do[n][i] &= x

    def show_gram(m):
        # If there's 'x', something is wrong.
        # If there's '?', needs more work.
        for x in m:
            print(" ".join("x#.?"[i] for i in x))
        print()

    w, h = len(vr), len(hr)
    rows = [gen_row(w, x) for x in hr]
    cols = [gen_row(h, x) for x in vr]
    can_do = list(map(allowable, rows))

    # Initially mark all columns for update.
    mod_rows, mod_cols = set(), set(range(w))

    while mod_cols:
        for i in mod_cols:
            fix_col(i)
        mod_cols = set()
        for i in mod_rows:
            fix_row(i)
        mod_rows = set()

    if all(can_do[i][j] in (1, 2) for j in range(w) for i in range(h)):
        print("Solution would be unique")  # but could be incorrect!
    else:
        print("Solution may not be unique, doing exhaustive search:")

    # We actually do exhaustive search anyway. Unique solution takes
    # no time in this phase anyway, but just in case there's no
    # solution (could happen?).
    out = [0] * h

    def try_all(n = 0):
        if n >= h:
            for j in range(w):
                if [x[j] for x in out] not in cols[j]:
                    return 0
            show_gram(out)
            return 1
        sol = 0
        for x in rows[n]:
            out[n] = x
            sol += try_all(n + 1)
        return sol

    n = try_all()
    if not n:
        print("No solution.")
    elif n == 1:
        print("Unique solution.")
    else:
        print(n, "solutions.")
    print()


def solve(s, show_runs=True):
    s = [[[ord(c) - ord('A') + 1 for c in w] for w in l.split()]
         for l in p.splitlines()]
    if show_runs:
        print("Horizontal runs:", s[0])
        print("Vertical runs:", s[1])
    deduce(s[0], s[1])

```



## Racket

<div><small>''<nowiki>[</nowiki>See [[Example:Nonogram solver/Racket]] for editing of this section<nowiki>]</nowiki>''</small></div>
## REXX

Nonogram Solver/Rexx:

```rexx
/*REXX*/
    Parse Arg fn
    Parse Var fn ou'.'
    maxpn = 10000               /* maximum possibilities to check through */
    output = ou'.out.txt'
 /* read row/col values into rowpp. and colpp. arrays */
    cc = linein(fn)
    rows = words(cc)
    dd = linein(fn)
    cols = words(dd)
    char = '0ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijk'
    cntr = 0
    Do i = 1 To rows
       rowpp.i = CV(cc,i)
       cntr = cntr + sum
    End
    cntc = 0
    Do i = 1 To cols
       colpp.i = CV(dd,i)
       cntc = cntc + sum
    End
    If (cntr <> cntc)|(cntr = 0) Then Do
       Say 'error Sum of rows <> sum of cols'
       Exit 999
    End
    Say cntr 'colored cells'
    ar = copies('-',rows*cols)
 /* values are -=unknown .=blank @=Color */
 /* PREFILL  array */
    'erase' output
 /**********COL PREFILL ************/
    Do col = 1 To cols
       r = colpp.col
       Parse Var r z r
       Do While r <> ''
          Parse Var r q r
          z = z + q + 1
       End
       result = copies('-',rows)
       If z = rows Then result = FILL_LINE(colpp.col)
       Else If z = 0 Then result = copies('.',rows)
       Do row = 1 To rows
          ar = overlay(substr(result,row,1),ar,(row-1)*cols+col)
       End
    End
 /**********ROW PREFILL ************/
    Do row = 1 To rows
       c = rowpp.row
       Parse Var c t c
       Do While c <> ''
          Parse Var c q c
          t = t + q + 1
       End
       result = substr(ar,(row-1)*cols+1,cols)
       If t = cols Then result = left(FILL_LINE(rowpp.row),cols)
       Else If t = 0 Then result = copies('.',cols)
       ar = overlay(result,ar,(row-1)*cols+1)
    End
 /********** ok here we loop ************/
    cnttry = 1
    nexttry = 2
    next.cnttry = ar
    sol = 0
    Do label nextpos While cnttry < nexttry
       Say 'trying' cnttry 'of' nexttry-1
       ar = next.cnttry
       cnttry = cnttry + 1
       Do Until sar = ar
          sar = ar
          Do row = 1 To rows
             /**********process rows ************/
             rowcol = substr(ar,(row-1)*cols+1,cols)
             pp = rowpp.row
             If PROCESSROW() Then Iterate nextpos
             Else ar = overlay(left(rowcol,cols),ar,(row-1)*cols+1)
          End
          Do col = 1 To cols
             rowcol = ''
             Do row = 1 To rows
                rowcol = rowcol || substr(ar,(row-1)*cols+ col,1)
             End
             pp = colpp.col
             If PROCESSROW() Then Iterate nextpos
             Do row = 1 To rows
                ar = overlay(substr(rowcol,row,1),ar,(row-1)*cols + col)
             End
          End
          If pos('-',ar) = 0 Then Do       /* hurray we have a solution */
             /* at this point we need to verify solution */
             If CHECKBOARD() Then Iterate nextpos   /* too bad didn't match */
             sol = sol + 1
             Call LINEOUT output,'This is solution no:' sol
             Call DUMPBOARD
             Iterate nextpos
          End
          If sar = ar Then Do
             fnd = pos('-',ar)
             next.nexttry = overlay('.',ar,fnd)
             nexttry = nexttry + 1
             ar = overlay('@',ar,fnd)
          End
       End
    End nextpos
    If sol = 0 Then sol = 'No'
    Say sol 'solutions found'
    Exit

 CHECKBOARD:
    Do row = 1 To rows
      /**********process rows ************/
       rowcol = substr(ar,(row-1)*cols+1,cols)
       pp = rowpp.row
       If CHECKROW() Then Return 1
    End
    Do col = 1 To cols
       rowcol = ''
       Do row = 1 To rows
          rowcol = rowcol || substr(ar,(row-1)*cols+ col,1)
       End
       pp = colpp.col
       If CHECKROW() Then Return 1
    End
    Return 0                                               /* we did it */

 CHECKROW:
    len_item = length(rowcol)
    st = 1
    If pp = 0 Then Return rowcol <> copies('.',len_item)
    Else If pp = len_item Then Return rowcol <> copies('@',len_item)
    Do While (pp <> '') & (st <= len_item)
       Parse Var pp p1 pp
       of = pos('@',rowcol'@',st)
       If of > len_item Then Return 1
       If substr(rowcol,of,p1) <> copies('@',p1) Then Return 1
       st = of + p1
       If substr(rowcol'.',st,1) <> '.' Then Return 1
    End
    Return 0


 DUMPBOARD:
    Parse Arg qr
    p = '..'
    q = '..'
    Do i = 1 To cols
       n = right(i,2)
       p = p left(n,1)
       q = q right(n,1)
    End
    Call LINEOUT output, p
    Call LINEOUT output, q
    Do i = 1 To rows
       o = right(i,2)
       p = substr(ar,(i-1)*cols+1,cols)
       Do j = 1 To cols
          Parse Var p z +1 p
          o = o z
       End
       Call LINEOUT output, o
    End
    Return

 FILL_LINE:
    Parse Arg items
    oo = ''
    Do While items <> ''
       Parse Var items a items
       oo = oo||copies('@',a)'.'
    End
    Return oo

 CV:
    Parse Arg cnts, rwcl
    str = word(cnts,rwcl)
    ret = ''
    sum = 0
    Do k = 1 To length(str)
       this = pos(substr(str,k,1),char)-1
       ret = ret this
       sum = sum + this
    End
    Return space(ret)

 PROCESSROW:                           /* rowcol pp in, rowcol pp of ol */
    prerow = rowcol
    len_item = length(rowcol)
    If pos('-',rowcol) = 0 Then Do
       pp = ''
       Return 0
    End
    of = 1
    kcnt = 0
    /* reduce the left side with already populated values */
    Do While (of < len_item) & (pp <> '')
       kcnt = kcnt + 1
       If kcnt > len_item Then Return 1
       If substr(rowcol,of,1) = '.' Then Do
          k = verify(substr(rowcol,of)'%','.')
          of = of + k - 1
          Iterate
       End
       nl = word(pp,1)
       len = verify(substr(rowcol,of)'%','-@') - 1
       If len < nl Then Do
          rowcol = overlay(copies('.',len),rowcol,of)
          of = of + len
          Iterate
       End
       If (len = nl) & (pos('@',substr(rowcol,of,nl))>0) Then Do
          rowcol = overlay(copies('@',nl),rowcol,of)
          of = of + nl
          pp = subword(pp,2)
          Iterate
       End
       If substr(rowcol,of,1) = '@' Then Do
          rowcol = overlay(copies('@',nl)'.',rowcol,of)
          of = of + nl
          pp = subword(pp,2)
          Iterate
       End
       Leave
    End
    /* reduce the right side with already populated values */
    ofm = len_item + 1 - of
    ol = 1
    kcnt = 0
    Do While (ol < ofm) & (pp <> '')
       kcnt = kcnt + 1
       If kcnt > len_item Then Return 1
       revrow = reverse(rowcol)
       If substr(revrow,ol,1) = '.' Then Do
          k = verify(substr(revrow,ol)'%','.')
          ol = ol + k - 1
          Iterate
       End
       nl = word(pp,words(pp))
       len = verify(substr(revrow,ol)'%','-@') - 1
       If len < nl Then Do
          rowcol = overlay(copies('.',len),rowcol,len_item-ol-len+2)
          ol = ol + len
          Iterate
       End
       If (len = nl) & (pos('@',substr(revrow,ol,nl))>0) Then Do
          rowcol = overlay(copies('@',nl),rowcol,len_item-ol-nl+2)
          ol = ol + nl
          pp = subword(pp,1,words(pp)-1)
          Iterate
       End
       If substr(revrow,ol,1) = '@' Then Do
          rowcol = overlay('.'copies('@',nl),rowcol,len_item-ol-nl+1)
          ol = ol + nl
          pp = subword(pp,1,words(pp)-1)
          Iterate
       End
       Leave
    End
    If pp = 0 Then pp = ''
    If pp = '' Then rowcol = changestr('-',rowcol,'.')
    If pp <> '' Then Do
       lv = len_item-of-ol+2
       pos. = ''
       pn = 0
       pi = substr(rowcol,of,lv)
       If (copies('-',length(pi)) = pi) Then Do
          len = CNT(pp)
          If (len + mx) <= lv Then Do
             Return 0
          End
       End
       /* oh oh need to check for posibilities */
       Call TRY '',pp
       If pn > maxpn Then Do
          over = over + 1
          Return 0
       End
       fnd = 0
       fu = pos.1
       Do z = 2 To pn
          Do j = 1 To lv
             If substr(fu,j,1) <> substr(pos.z,j,1) Then fu = overlay('-',fu,j)
          End
       End
       Do z = 1 To lv
          If substr(fu,z,1) <> '-' Then rowcol = overlay(substr(fu,z,1),rowcol,of+z-1)
       End
    End
    Return 0
 TRY: Procedure Expose pn pos. maxpn lv pi
    Parse Arg prev,pp
    If pp = '' Then Do
       rem = substr(pi,length(prev)+1)
       If translate(rem,'..','.-') <> copies('.',length(rem)) Then Return
       prev = left(prev||copies('.',lv),lv)
       pn = pn + 1
       If pn > maxpn Then Return
       pos.pn = prev
       Return
    End
    Parse Var pp p1 pp
    If length(prev)+p1 > lv Then Return
    Do i = 0 To lv - length(prev)-p1
       If translate(substr(pi,length(prev)+1,i),'..','.-') = copies('.',i) Then
         If translate(substr(pi,length(prev)+i+1,p1),'@@','@-') = copies('@',p1) Then
           If substr(pi,length(prev)+i+p1+1,1) <> '@' Then
             Call TRY prev||copies('.',i)||copies('@',p1)'.',pp
    End
    Return
 CNT: Procedure Expose mx
    Parse Arg len items
    mx = len
    Do While items <> ''
       Parse Var items ii items
       len = len + ii + 1
       If ii > mx Then mx = ii
    End
    Return len
```

```txt

 Puzzle
 C BA CB BB F AE F A B
 AB CA AE GA E C D C
 This is solution no: 1
 ..
 .. 1 2 3 4 5 6 7 8
  1 . @ @ @ . . . .
  2 @ @ . @ . . . .
  3 . @ @ @ . . @ @
  4 . . @ @ . . @ @
  5 . . @ @ @ @ @ @
  6 @ . @ @ @ @ @ .
  7 @ @ @ @ @ @ . .
  8 . . . . @ . . .
  9 . . . @ @ . . .

 Puzzle
 F CAC ACAC CN AAA AABB EBB EAA ECCC HCCC
 D D AE CD AE A DA BBB CC AAB BAA AAB DA AAB AAA BAB AAA CD BBA DA
 This is solution no: 1
 ..                   1 1 1 1 1 1 1 1 1 1 2
 .. 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
  1 . . . . . . . . . . @ @ @ @ @ @ . . . .
  2 . . . . . . . . @ @ @ . @ . . @ @ @ . .
  3 . . . @ . . @ @ @ . . . @ . . . . @ @ @
  4 . . @ @ @ . @ @ @ @ @ @ @ @ @ @ @ @ @ @
  5 . . . @ . . @ . . . . . . . . . . . . @
  6 . . @ . @ . @ @ . . . . . . . . . . @ @
  7 @ @ @ @ @ . . @ @ . . . . . . . . @ @ .
  8 @ @ @ @ @ . . . @ . . . . . . . . @ . .
  9 @ @ @ @ @ . . @ @ @ . @ @ @ . @ @ @ . .
 10 @ @ @ @ @ @ @ @ . @ @ @ . @ @ @ . @ @ @

 Puzzle
 CA BDA ACC BD CCAC CBBAC BBBBB BAABAA ABAD AABB BBH BBBD ABBAAA CCEA AACAAB BCACC ACBH DCH ADBE ADBB DBE ECE DAA DB CC
 BC CAC CBAB BDD CDBDE BEBDF ADCDFA DCCFB DBCFC ABDBA BBF AAF BADB DBF AAAAD BDG CEF CBDB BBB FC

 This is solution no: 1
 ..                   1 1 1 1 1 1 1 1 1 1 2
 .. 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
  1 . . . . @ @ @ . @ . . . . . . . . . . .
  2 . . . . @ @ . @ @ @ @ . @ . . . . . . .
  3 . . . . @ . @ @ @ . @ @ @ . . . . . . .
  4 . . @ @ . @ @ @ @ . . . . . . . . . . .
  5 . @ @ @ . @ @ @ . @ . . . . @ @ @ . . .
  6 @ @ @ . . @ @ . @ @ . . . @ . @ @ @ . .
  7 @ @ . . @ @ . @ @ . . . . @ @ . @ @ . .
  8 . . . . @ @ . @ . @ . . @ @ . @ . @ . .
  9 . . . . @ . @ @ . @ . . . @ @ @ @ . . .
 10 . . . . @ . @ . @ @ . . . . . @ @ . . .
 11 . . . . . @ @ . @ @ . . @ @ @ @ @ @ @ @
 12 . . . . @ @ . @ @ . . . @ @ . . @ @ @ @
 13 . . . . @ . @ @ . @ @ . @ . . . @ . . @
 14 @ @ @ . . @ @ @ . @ @ @ @ @ . . . . . @
 15 @ . @ . @ @ @ . @ . . . . @ . . . . @ @
 16 @ @ . . @ @ @ . @ . . . . @ @ @ . @ @ @
 17 . @ . @ @ @ . @ @ . @ @ @ @ @ @ @ @ . .
 18 . @ @ @ @ . @ @ @ . @ @ @ @ @ @ @ @ . .
 19 . . . @ . @ @ @ @ . @ @ . @ @ @ @ @ . .
 20 . . . @ . @ @ @ @ . @ @ . . . @ @ . . .
 21 . . . . @ @ @ @ . . @ @ . . . @ @ @ @ @
 22 . . . @ @ @ @ @ . @ @ @ . . . @ @ @ @ @
 23 . . . @ @ @ @ . @ . . . . . . . . . . @
 24 . . @ @ @ @ . @ @ . . . . . . . . . . .
 25 . . @ @ @ . @ @ @ . . . . . . . . . . .

 Puzzle
 E BCB BEA BH BEK AABAF ABAC BAA BFB OD JH BADCF Q Q R AN AAN EI H G
 E CB BAB AAA AAA AC BB ACC ACCA AGB AIA AJ AJ ACE AH BAF CAG DAG FAH FJ GJ ADK ABK BL CM
 This is solution no: 1
 ..                   1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2
 .. 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
  1 . . . . . . . . . . . . . . . . . . . . @ @ @ @ @
  2 . . @ @ . . . . . . . . . . . . . . @ @ @ . . @ @
  3 . @ @ . . . . . . . . . . . . . . @ @ @ @ @ . . @
  4 @ @ . . . . . . . . . . . . . @ @ @ @ @ @ @ @ . .
  5 @ @ . . . . @ @ @ @ @ . @ @ @ @ @ @ @ @ @ @ @ . .
  6 @ . @ . . @ @ . . . . @ . . . . @ @ @ @ @ @ . . .
  7 @ . . @ @ . . . . . @ . . . . . . . @ @ @ . . . .
  8 @ @ . . . . . . . . @ . . . . . . . . . . . . . @
  9 . @ @ . . . . . @ @ @ @ @ @ . . . . . . . . . @ @
 10 . . @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ . . . . @ @ @ @
 11 . . . . . @ @ @ @ @ @ @ @ @ @ . . @ @ @ @ @ @ @ @
 12 . . . . @ @ . @ . @ @ @ @ . @ @ @ . . @ @ @ @ @ @
 13 . . . . . . . . @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @
 14 . . . . . . . . @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @
 15 . . . . . . . @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @
 16 . . . . . . . @ . . . @ @ @ @ @ @ @ @ @ @ @ @ @ @
 17 . . . . . . . @ . @ . @ @ @ @ @ @ @ @ @ @ @ @ @ @
 18 . . . . . . . . @ @ @ @ @ . . . @ @ @ @ @ @ @ @ @
 19 . . . . . . . . . . . . . . . . . @ @ @ @ @ @ @ @
 20 . . . . . . . . . . . . . . . . . . @ @ @ @ @ @ @

 Puzzle
 GCAAG AABBAA ACACAACA ACAAFACA ACAEBACA AABAA GAAAAAG CC ABCAACAAB AACBAA DADBAB AAAAADAC BAAABE CBBFCA AIAABA BABBCA CAAAAEA ABBE GABAAAC AABABBA ACADEA ACACJB ACAAFF AABAAB GBABE
 GBAAG AABBAA ACACACACA ACAAEACA ACAADACA AAABAA GAAAAAG AAC BABAHBA BBABAAAB AGCBA ABCAAAAA DAABF CCAAACA ABEBB BBAAAAABA ACCBAHA FBA GADAAC AAAAD ACACGA ACAAABAAD ACADCC AABBBFA GACBAA
 This is solution no: 1
 ..                   1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2
 .. 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
  1 @ @ @ @ @ @ @ . @ @ @ . . . @ . @ . @ @ @ @ @ @ @
  2 @ . . . . . @ . @ @ . @ @ . . . . . @ . . . . . @
  3 @ . @ @ @ . @ . . . . . @ @ @ . @ . @ . @ @ @ . @
  4 @ . @ @ @ . @ . @ . . @ @ @ @ @ @ . @ . @ @ @ . @
  5 @ . @ @ @ . @ . . @ @ @ @ @ . @ @ . @ . @ @ @ . @
  6 @ . . . . . @ . . @ @ . . . . . . . @ . . . . . @
  7 @ @ @ @ @ @ @ . @ . @ . @ . @ . @ . @ @ @ @ @ @ @
  8 . . . . . . . . @ @ @ . . . @ @ @ . . . . . . . .
  9 @ . @ @ . @ @ @ . . @ . @ . @ @ @ . @ . . @ . @ @
 10 @ . @ . . . . . . @ @ @ . @ @ . . . . @ . . . @ .
 11 . @ @ @ @ . @ . @ @ @ @ . @ @ . @ . . . . @ @ . .
 12 . @ . @ . . . @ . . . @ . @ . @ @ @ @ . @ . @ @ @
 13 . . @ @ . . @ . @ . @ . . . . . . @ @ . @ @ @ @ @
 14 . . . @ @ @ . @ @ . @ @ . @ @ @ @ @ @ . @ @ @ . @
 15 @ . @ @ @ @ @ @ @ @ @ . @ . @ . . @ @ . . . . @ .
 16 . @ @ . @ . . @ @ . . @ @ . . @ @ @ . . . . . @ .
 17 @ @ @ . @ . @ . @ . . . . @ . . @ @ @ @ @ . @ . .
 18 . . . . . . . . @ . . @ @ . . @ @ . . . @ @ @ @ @
 19 @ @ @ @ @ @ @ . @ . . . @ @ . . @ . @ . @ . @ @ @
 20 @ . . . . . @ . @ @ . . @ . . @ @ . . . @ @ . @ .
 21 @ . @ @ @ . @ . . . @ @ @ @ . . @ @ @ @ @ . . @ .
 22 @ . @ @ @ . @ . @ @ @ . @ @ @ @ @ @ @ @ @ @ . @ @
 23 @ . @ @ @ . @ . @ . . @ @ @ @ @ @ . @ @ @ @ @ @ .
 24 @ . . . . . @ . . @ @ . . . . . . @ . @ . @ @ . .
 25 @ @ @ @ @ @ @ . @ @ . . . @ . @ @ . . . @ @ @ @ @
 This is solution no: 2
 ..                   1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2
 .. 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
  1 @ @ @ @ @ @ @ . @ @ @ . . . @ . @ . @ @ @ @ @ @ @
  2 @ . . . . . @ . @ @ . @ @ . . . . . @ . . . . . @
  3 @ . @ @ @ . @ . . . . . @ @ @ . @ . @ . @ @ @ . @
  4 @ . @ @ @ . @ . @ . . @ @ @ @ @ @ . @ . @ @ @ . @
  5 @ . @ @ @ . @ . . @ @ @ @ @ . @ @ . @ . @ @ @ . @
  6 @ . . . . . @ . . @ @ . . . . . . . @ . . . . . @
  7 @ @ @ @ @ @ @ . @ . @ . @ . @ . @ . @ @ @ @ @ @ @
  8 . . . . . . . . @ @ @ . . . @ @ @ . . . . . . . .
  9 @ . @ @ . @ @ @ . . @ . @ . @ @ @ . . @ . @ . @ @
 10 @ . @ . . . . . . @ @ @ . @ @ . . . @ . . . . @ .
 11 . @ @ @ @ . @ . @ @ @ @ . @ @ . @ . . . . @ @ . .
 12 . @ . @ . . . @ . . . @ . @ . @ @ @ @ . @ . @ @ @
 13 . . @ @ . . @ . @ . @ . . . . . . @ @ . @ @ @ @ @
 14 . . . @ @ @ . @ @ . @ @ . @ @ @ @ @ @ . @ @ @ . @
 15 @ . @ @ @ @ @ @ @ @ @ . @ . @ . . @ @ . . . . @ .
 16 . @ @ . @ . . @ @ . . @ @ . . @ @ @ . . . . . @ .
 17 @ @ @ . @ . @ . @ . . . . @ . . @ @ @ @ @ . @ . .
 18 . . . . . . . . @ . . @ @ . . @ @ . . . @ @ @ @ @
 19 @ @ @ @ @ @ @ . @ . . . @ @ . . @ . @ . @ . @ @ @
 20 @ . . . . . @ . @ @ . . @ . . @ @ . . . @ @ . @ .
 21 @ . @ @ @ . @ . . . @ @ @ @ . . @ @ @ @ @ . . @ .
 22 @ . @ @ @ . @ . @ @ @ . @ @ @ @ @ @ @ @ @ @ . @ @
 23 @ . @ @ @ . @ . @ . . @ @ @ @ @ @ . @ @ @ @ @ @ .
 24 @ . . . . . @ . . @ @ . . . . . . @ . @ . @ @ . .
 25 @ @ @ @ @ @ @ . @ @ . . . @ . @ @ . . . @ @ @ @ @
 This is solution no: 3
 ..                   1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2
 .. 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
  1 @ @ @ @ @ @ @ . @ @ @ . . . @ . @ . @ @ @ @ @ @ @
  2 @ . . . . . @ . @ @ . @ @ . . . . . @ . . . . . @
  3 @ . @ @ @ . @ . . . . . @ @ @ . @ . @ . @ @ @ . @
  4 @ . @ @ @ . @ . @ . . @ @ @ @ @ @ . @ . @ @ @ . @
  5 @ . @ @ @ . @ . . @ @ @ @ @ . @ @ . @ . @ @ @ . @
  6 @ . . . . . @ . . @ @ . . . . . . . @ . . . . . @
  7 @ @ @ @ @ @ @ . @ . @ . @ . @ . @ . @ @ @ @ @ @ @
  8 . . . . . . . . @ @ @ . . . @ @ @ . . . . . . . .
  9 @ . @ @ . @ @ @ . . @ . @ . @ @ @ . @ . . @ . @ @
 10 @ . @ . . . . . . @ @ @ . @ @ . . . . @ . . . @ .
 11 . @ @ @ @ . @ . @ @ @ @ . @ @ . @ . . . . @ @ . .
 12 . @ . @ . . . @ . . . @ . @ . @ @ @ @ . @ . @ @ @
 13 . . @ @ . . @ . @ . @ . . . . . . @ @ . @ @ @ @ @
 14 . . . @ @ @ . @ @ . @ @ . @ @ @ @ @ @ . @ @ @ . @
 15 @ . @ @ @ @ @ @ @ @ @ . @ . @ . . @ @ . . . . @ .
 16 . @ @ . @ . . @ @ . . . @ @ . @ @ @ . . . . . @ .
 17 @ @ @ . @ . @ . @ . . @ . . . . @ @ @ @ @ . @ . .
 18 . . . . . . . . @ . . . @ @ . @ @ . . . @ @ @ @ @
 19 @ @ @ @ @ @ @ . @ . . @ @ . . . @ . @ . @ . @ @ @
 20 @ . . . . . @ . @ @ . . @ . . @ @ . . . @ @ . @ .
 21 @ . @ @ @ . @ . . . @ @ @ @ . . @ @ @ @ @ . . @ .
 22 @ . @ @ @ . @ . @ @ @ . @ @ @ @ @ @ @ @ @ @ . @ @
 23 @ . @ @ @ . @ . @ . . @ @ @ @ @ @ . @ @ @ @ @ @ .
 24 @ . . . . . @ . . @ @ . . . . . . @ . @ . @ @ . .
 25 @ @ @ @ @ @ @ . @ @ . . . @ . @ @ . . . @ @ @ @ @
 This is solution no: 4
 ..                   1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2
 .. 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
  1 @ @ @ @ @ @ @ . @ @ @ . . . @ . @ . @ @ @ @ @ @ @
  2 @ . . . . . @ . @ @ . @ @ . . . . . @ . . . . . @
  3 @ . @ @ @ . @ . . . . . @ @ @ . @ . @ . @ @ @ . @
  4 @ . @ @ @ . @ . @ . . @ @ @ @ @ @ . @ . @ @ @ . @
  5 @ . @ @ @ . @ . . @ @ @ @ @ . @ @ . @ . @ @ @ . @
  6 @ . . . . . @ . . @ @ . . . . . . . @ . . . . . @
  7 @ @ @ @ @ @ @ . @ . @ . @ . @ . @ . @ @ @ @ @ @ @
  8 . . . . . . . . @ @ @ . . . @ @ @ . . . . . . . .
  9 @ . @ @ . @ @ @ . . @ . @ . @ @ @ . . @ . @ . @ @
 10 @ . @ . . . . . . @ @ @ . @ @ . . . @ . . . . @ .
 11 . @ @ @ @ . @ . @ @ @ @ . @ @ . @ . . . . @ @ . .
 12 . @ . @ . . . @ . . . @ . @ . @ @ @ @ . @ . @ @ @
 13 . . @ @ . . @ . @ . @ . . . . . . @ @ . @ @ @ @ @
 14 . . . @ @ @ . @ @ . @ @ . @ @ @ @ @ @ . @ @ @ . @
 15 @ . @ @ @ @ @ @ @ @ @ . @ . @ . . @ @ . . . . @ .
 16 . @ @ . @ . . @ @ . . . @ @ . @ @ @ . . . . . @ .
 17 @ @ @ . @ . @ . @ . . @ . . . . @ @ @ @ @ . @ . .
 18 . . . . . . . . @ . . . @ @ . @ @ . . . @ @ @ @ @
 19 @ @ @ @ @ @ @ . @ . . @ @ . . . @ . @ . @ . @ @ @
 20 @ . . . . . @ . @ @ . . @ . . @ @ . . . @ @ . @ .
 21 @ . @ @ @ . @ . . . @ @ @ @ . . @ @ @ @ @ . . @ .
 22 @ . @ @ @ . @ . @ @ @ . @ @ @ @ @ @ @ @ @ @ . @ @
 23 @ . @ @ @ . @ . @ . . @ @ @ @ @ @ . @ @ @ @ @ @ .
 24 @ . . . . . @ . . @ @ . . . . . . @ . @ . @ @ . .
 25 @ @ @ @ @ @ @ . @ @ . . . @ . @ @ . . . @ @ @ @ @

```

