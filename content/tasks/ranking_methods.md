+++
title = "Ranking methods"
description = ""
date = 2019-09-21T23:50:50Z
aliases = []
[extra]
id = 17751
[taxonomies]
categories = ["task"]
tags = []
+++

The numerical rank of competitors in a competition shows if one is better than, equal to, or worse than another based on their results in a competition.

The numerical rank of a competitor can be assigned in several [[wp:Ranking|different ways]].


## Task

The following scores are accrued for all competitors of a competition (in best-first order):

```txt
44 Solomon
42 Jason
42 Errol
41 Garry
41 Bernard
41 Barry
39 Stephen
```


For each of the following ranking methods, create a function/method/procedure/subroutine... that applies the ranking method to an ordered list of scores with scorers:
# Standard. (Ties share what would have been their first ordinal number).
# Modified. (Ties share what would have been their last ordinal number).
# Dense. (Ties share the next available integer).
# Ordinal. ((Competitors take the next available integer. Ties are not treated otherwise).
# Fractional. (Ties share the mean of what would have been their ordinal numbers).



See the [[wp:Ranking|wikipedia article]] for a fuller description.

Show here, on this page, the ranking of the test scores under each of the numbered ranking methods.





## AutoHotkey


```AutoHotkey
Rank(data, opt:=1){ ; opt = 1 Standard (default), 2 Modified, 3 Dense, 4 Ordinal, 5 Fractional
	for index, val in StrSplit(data, "`n", "`r") {
		RegExMatch(val, "^(\d+)\s+(.*)", Match)
		if !(Match1=prev)
			n := index
		prev := Match1
		Res1 .= n "`t" Match "`n"
		Res4 .= index "`t" Match "`n"
		Temp .= n ":" index " " Match "`n"
	}
	n:=0
	while pos := RegExMatch(Temp, "`asm)^(\d+).*?\R(?!\1)|.+", Match, pos?pos+StrLen(Match):1) {
		n += StrSplit(Trim(Match, "`r`n"), "`n", "`r").MaxIndex()
		Res2 .= RegExReplace(Match, "`am)^\d+:\d+", n "`t")
		Res3 .= RegExReplace(Match, "`am)^\d+:\d+", A_Index "`t")
		R := 0
		for index, val in StrSplit(Match, "`n", "`r")
			R += StrSplit(val, ":").2
		Res5 .= RegExReplace(Match, "`am)^\d+:\d+", RegExReplace(R / StrSplit(Trim(Match, "`r`n"), "`n", "`r").MaxIndex(), "\.?0+$") "`t")
	}
	return Res%opt%
}
```

Example:
```AutoHotkey
data =
(
44 Solomon
42 Jason
42 Errol
41 Garry
41 Bernard
41 Barry
39 Stephen
)

MsgBox, 262144, ,% ""
. "Standard Ranking:`n"		Rank(data)
. "`nModified Ranking:`n"	Rank(data, 2)
. "`nDense Ranking:`n" 		Rank(data, 3)
. "`nOrdinal Ranking:`n" 	Rank(data, 4)
. "`nFractional Ranking:`n"	Rank(data, 5)
return
```

Output:
```txt
Standard Ranking:
1	44 Solomon
2	42 Jason
2	42 Errol
4	41 Garry
4	41 Bernard
4	41 Barry
7	39 Stephen

Modified Ranking:
1	 44 Solomon
3	 42 Jason
3	 42 Errol
6	 41 Garry
6	 41 Bernard
6	 41 Barry
7	 39 Stephen

Dense Ranking:
1	 44 Solomon
2	 42 Jason
2	 42 Errol
3	 41 Garry
3	 41 Bernard
3	 41 Barry
4	 39 Stephen

Ordinal Ranking:
1	44 Solomon
2	42 Jason
3	42 Errol
4	41 Garry
5	41 Bernard
6	41 Barry
7	39 Stephen

Fractional Ranking:
1	 44 Solomon
2.5	 42 Jason
2.5	 42 Errol
5	 41 Garry
5	 41 Bernard
5	 41 Barry
7	 39 Stephen
```



## AWK

This uses separate files for each method of ranking:

```awk
##
## Dense ranking in file: ranking_d.awk
##

BEGIN{ lastresult = "!"; lastrank = 0 }

function d_rank(){
    if($1==lastresult){
        print lastrank, $0
    }else{
        lastresult = $1
        print ++lastrank, $0 }
}
//{d_rank() }

##
## Fractional ranking in file: ranking_f.awk
##

BEGIN{
    last = "!"
    flen = 0 }

function f_rank(){
    item = $0
    if($1!=last){
        if(flen){
            sum = 0
            for(fl=0; fl < flen;){
                $0 = fifo[fl++]
                sum += $1 }
            mean = sum / flen
            for(fl=0; fl < flen;){
                $0 = fifo[fl++]
                $1 = ""
                printf("%3g %s\n", mean, $0) }
            flen = 0
    }}
    $0 = item
    last = $1
    fifo[flen++] = sprintf("%i %s", FNR, item)
}
//{f_rank()}

END{ if(flen){
        sum = 0
        for(fl=0; fl < flen;){
            $0 = fifo[fl++]
            sum += $1 }
        mean = sum / flen
        for(fl=0; fl < flen;){
            $0 = fifo[fl++]
            $1 = ""
            printf("%3g %s\n", mean, $0) }}}

##
## Modified competition ranking in file: ranking_mc.awk
##

BEGIN{
    lastresult = "!"
    flen = 0 }

function mc_rank(){
    if($1==lastresult){
        fifo[flen++] = $0
    }else{
        for(fl=0; fl < flen;){
            print FNR-1, fifo[fl++]}
        flen = 0
        fifo[flen++] = $0
        lastresult = $1}
}
//{mc_rank()}

END{ for(fl=0; fl < flen;){
        print FNR, fifo[fl++]} }

##
## Ordinal ranking in file: ranking_o.awk
##

function o_rank(){ print FNR, $0 }
//{o_rank() }

##
## Standard competition ranking in file: ranking_sc.awk
##

BEGIN{ lastresult = lastrank = "!" }

function sc_rank(){
    if($1==lastresult){
        print lastrank, $0
    }else{
        print FNR, $0
        lastresult = $1
        lastrank = FNR}
}
//{sc_rank()}

```


The input as a file <code>ranking.txt</code>:

```txt
44 Solomon
42 Jason
42 Errol
41 Garry
41 Bernard
41 Barry
39 Stephen
```


```txt
C:\Users\RC\Code>awk -f ranking_sc.awk ranking.txt
1 44 Solomon
2 42 Jason
2 42 Errol
4 41 Garry
4 41 Bernard
4 41 Barry
7 39 Stephen

C:\Users\RC\Code>awk -f ranking_mc.awk ranking.txt
1 44 Solomon
3 42 Jason
3 42 Errol
6 41 Garry
6 41 Bernard
6 41 Barry
7 39 Stephen

C:\Users\RC\Code>awk -f ranking_d.awk ranking.txt
1 44 Solomon
2 42 Jason
2 42 Errol
3 41 Garry
3 41 Bernard
3 41 Barry
4 39 Stephen

C:\Users\RC\Code>awk -f ranking_o.awk ranking.txt
1 44 Solomon
2 42 Jason
3 42 Errol
4 41 Garry
5 41 Bernard
6 41 Barry
7 39 Stephen

C:\Users\RC\Code>awk -f ranking_f.awk ranking.txt
  1  44 Solomon
2.5  42 Jason
2.5  42 Errol
  5  41 Garry
  5  41 Bernard
  5  41 Barry
  7  39 Stephen

C:\Users\RC\Code>
```



## C

Takes the scores as input via a file, prints out usage on incorrect invocation.

```C

#include<stdlib.h>
#include<stdio.h>

typedef struct{
	int score;
	char name[100];
}entry;

void ordinalRanking(entry* list,int len){

	int i;

	printf("\n\nOrdinal Ranking\n---------------");

	for(i=0;i<len;i++)
		printf("\n%d\t%d\t%s",i+1,list[i].score,list[i].name);
}

void standardRanking(entry* list,int len){

	int i,j=1;

	printf("\n\nStandard Ranking\n----------------");

	for(i=0;i<len;i++){
		printf("\n%d\t%d\t%s",j,list[i].score,list[i].name);
		if(list[i+1].score<list[i].score)
			j = i+2;
	}
}

void denseRanking(entry* list,int len){

	int i,j=1;

	printf("\n\nDense Ranking\n-------------");

	for(i=0;i<len;i++){
		printf("\n%d\t%d\t%s",j,list[i].score,list[i].name);
		if(list[i+1].score<list[i].score)
			j++;
	}
}

void modifiedRanking(entry* list,int len){

	int i,j,count;

	printf("\n\nModified Ranking\n----------------");

	for(i=0;i<len-1;i++){
		if(list[i].score!=list[i+1].score){
			printf("\n%d\t%d\t%s",i+1,list[i].score,list[i].name);
			count = 1;
			for(j=i+1;list[j].score==list[j+1].score && j<len-1;j++)
				count ++;
			for(j=0;j<count-1;j++)
				printf("\n%d\t%d\t%s",i+count+1,list[i+j+1].score,list[i+j+1].name);
			i += (count-1);
		}
	}
	printf("\n%d\t%d\t%s",len,list[len-1].score,list[len-1].name);
}

void fractionalRanking(entry* list,int len){

	int i,j,count;
	float sum = 0;

	printf("\n\nFractional Ranking\n------------------");

	for(i=0;i<len;i++){
		if(i==len-1 || list[i].score!=list[i+1].score)
			printf("\n%.1f\t%d\t%s",(float)(i+1),list[i].score,list[i].name);
		else if(list[i].score==list[i+1].score){
			sum = i;
			count = 1;
			for(j=i;list[j].score==list[j+1].score;j++){
				sum += (j+1);
				count ++;
			}
			for(j=0;j<count;j++)
				printf("\n%.1f\t%d\t%s",sum/count + 1,list[i+j].score,list[i+j].name);
			i += (count-1);
		}
	}
}

void processFile(char* fileName){
	FILE* fp = fopen(fileName,"r");
	entry* list;
	int i,num;

	fscanf(fp,"%d",&num);

	list = (entry*)malloc(num*sizeof(entry));

	for(i=0;i<num;i++)
		fscanf(fp,"%d%s",&list[i].score,list[i].name);

	fclose(fp);

	ordinalRanking(list,num);
	standardRanking(list,num);
	denseRanking(list,num);
	modifiedRanking(list,num);
	fractionalRanking(list,num);
}

int main(int argC,char* argV[])
{
	if(argC!=2)
		printf("Usage %s <score list file>");
	else
		processFile(argV[1]);
	return 0;
}

```

Input file, first row is number of records :

```txt

7
44 Solomon
42 Jason
42 Errol
41 Garry
41 Bernard
41 Barry
39 Stephen

```

Output :

```txt

C:\rosettaCode>ranking.exe rankData.txt


Ordinal Ranking
---------------
1       44      Solomon
2       42      Jason
3       42      Errol
4       41      Garry
5       41      Bernard
6       41      Barry
7       39      Stephen

Standard Ranking
----------------
1       44      Solomon
2       42      Jason
2       42      Errol
4       41      Garry
4       41      Bernard
4       41      Barry
7       39      Stephen

Dense Ranking
-------------
1       44      Solomon
2       42      Jason
2       42      Errol
3       41      Garry
3       41      Bernard
3       41      Barry
4       39      Stephen

Modified Ranking
----------------
1       44      Solomon
3       42      Jason
3       42      Errol
6       41      Garry
6       41      Bernard
6       41      Barry
7       39      Stephen

Fractional Ranking
------------------
1.0     44      Solomon
2.5     42      Jason
2.5     42      Errol
5.0     41      Garry
5.0     41      Bernard
5.0     41      Barry
7.0     39      Stephen

```



## C++

```cpp
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <map>
#include <ostream>
#include <set>
#include <vector>

template<typename T>
std::ostream& print(std::ostream& os, const T& src) {
    auto it = src.cbegin();
    auto end = src.cend();

    os << "[";
    if (it != end) {
        os << *it;
        it = std::next(it);
    }
    while (it != end) {
        os << ", " << *it;
        it = std::next(it);
    }

    return os << "]";
}

typedef std::map<std::string, int> Map;
typedef Map::value_type MapEntry;

void standardRank(const Map& scores) {
    std::cout << "Standard Rank" << std::endl;

    std::vector<int> list;
    for (auto& elem : scores) {
        list.push_back(elem.second);
    }
    std::sort(list.begin(), list.end(), std::greater<int>{});
    list.erase(std::unique(list.begin(), list.end()), list.end());

    int rank = 1;
    for (auto value : list) {
        int temp = rank;
        for (auto& e : scores) {
            if (e.second == value) {
                std::cout << temp << " " << value << " " << e.first.c_str() << std::endl;
                rank++;
            }
        }
    }

    std::cout << std::endl;
}

void modifiedRank(const Map& scores) {
    std::cout << "Modified Rank" << std::endl;

    std::vector<int> list;
    for (auto& elem : scores) {
        list.push_back(elem.second);
    }
    std::sort(list.begin(), list.end(), std::greater<int>{});
    list.erase(std::unique(list.begin(), list.end()), list.end());

    int rank = 0;
    for (auto value : list) {
        rank += std::count_if(scores.begin(), scores.end(), [value](const MapEntry& e) { return e.second == value; });
        for (auto& e : scores) {
            if (e.second == value) {
                std::cout << rank << " " << value << " " << e.first.c_str() << std::endl;
            }
        }
    }

    std::cout << std::endl;
}

void denseRank(const Map& scores) {
    std::cout << "Dense Rank" << std::endl;

    std::vector<int> list;
    for (auto& elem : scores) {
        list.push_back(elem.second);
    }
    std::sort(list.begin(), list.end(), std::greater<int>{});
    list.erase(std::unique(list.begin(), list.end()), list.end());

    int rank = 1;
    for (auto value : list) {
        for (auto& e : scores) {
            if (e.second == value) {
                std::cout << rank << " " << value << " " << e.first.c_str() << std::endl;
            }
        }
        rank++;
    }

    std::cout << std::endl;
}

void ordinalRank(const Map& scores) {
    std::cout << "Ordinal Rank" << std::endl;

    std::vector<int> list;
    for (auto& elem : scores) {
        list.push_back(elem.second);
    }
    std::sort(list.begin(), list.end(), std::greater<int>{});
    list.erase(std::unique(list.begin(), list.end()), list.end());

    int rank = 1;
    for (auto value : list) {
        for (auto& e : scores) {
            if (e.second == value) {
                std::cout << rank++ << " " << value << " " << e.first.c_str() << std::endl;
            }
        }
    }

    std::cout << std::endl;
}

void fractionalRank(const Map& scores) {
    std::cout << "Ordinal Rank" << std::endl;

    std::vector<int> list;
    for (auto& elem : scores) {
        list.push_back(elem.second);
    }
    std::sort(list.begin(), list.end(), std::greater<int>{});
    list.erase(std::unique(list.begin(), list.end()), list.end());

    int rank = 0;
    for (auto value : list) {
        double avg = 0.0;
        int cnt = 0;

        for (auto& e : scores) {
            if (e.second == value) {
                rank++;
                cnt++;
                avg += rank;
            }
        }
        avg /= cnt;

        for (auto& e : scores) {
            if (e.second == value) {
                std::cout << std::setprecision(1) << std::fixed << avg << " " << value << " " << e.first.c_str() << std::endl;
            }
        }
    }

    std::cout << std::endl;
}

int main() {
    using namespace std;

    map<string, int> scores{
        {"Solomon", 44},
        {"Jason", 42},
        {"Errol", 42},
        {"Gary", 41},
        {"Bernard", 41},
        {"Barry", 41},
        {"Stephen", 39}
    };

    standardRank(scores);
    modifiedRank(scores);
    denseRank(scores);
    ordinalRank(scores);
    fractionalRank(scores);

    return 0;
}
```

```txt
Standard Rank
1 44 Solomon
2 42 Errol
2 42 Jason
4 41 Barry
4 41 Bernard
4 41 Gary
7 39 Stephen

Modified Rank
1 44 Solomon
3 42 Errol
3 42 Jason
6 41 Barry
6 41 Bernard
6 41 Gary
7 39 Stephen

Dense Rank
1 44 Solomon
2 42 Errol
2 42 Jason
3 41 Barry
3 41 Bernard
3 41 Gary
4 39 Stephen

Ordinal Rank
1 44 Solomon
2 42 Errol
3 42 Jason
4 41 Barry
5 41 Bernard
6 41 Gary
7 39 Stephen

Ordinal Rank
1.0 44 Solomon
2.5 42 Errol
2.5 42 Jason
5.0 41 Barry
5.0 41 Bernard
5.0 41 Gary
7.0 39 Stephen
```


## C#
```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace RankingMethods {
    class Program {
        static void Main(string[] args) {
            Dictionary<string, int> scores = new Dictionary<string, int> {
                ["Solomon"] = 44,
                ["Jason"] = 42,
                ["Errol"] = 42,
                ["Gary"] = 41,
                ["Bernard"] = 41,
                ["Barry"] = 41,
                ["Stephen"] = 39,
            };

            StandardRank(scores);
            ModifiedRank(scores);
            DenseRank(scores);
            OrdinalRank(scores);
            FractionalRank(scores);
        }

        static void StandardRank(Dictionary<string, int> data) {
            Console.WriteLine("Standard Rank");

            var list = data.Values.Distinct().ToList();
            list.Sort((a, b) => b.CompareTo(a));

            int rank = 1;
            foreach (var value in list) {
                int temp = rank;
                foreach (var k in data.Keys) {
                    if (data[k] == value) {
                        Console.WriteLine("{0} {1} {2}", temp, value, k);
                        rank++;
                    }
                }
            }

            Console.WriteLine();
        }

        static void ModifiedRank(Dictionary<string, int> data) {
            Console.WriteLine("Modified Rank");

            var list = data.Values.Distinct().ToList();
            list.Sort((a, b) => b.CompareTo(a));

            int rank = 0;
            foreach (var value in list) {
                foreach (var k in data.Keys) {
                    if (data[k] == value) {
                        rank++;
                    }
                }

                foreach (var k in data.Keys) {
                    if (data[k] == value) {
                        Console.WriteLine("{0} {1} {2}", rank, data[k], k);
                    }
                }
            }

            Console.WriteLine();
        }

        static void DenseRank(Dictionary<string, int> data) {
            Console.WriteLine("Dense Rank");

            var list = data.Values.Distinct().ToList();
            list.Sort((a, b) => b.CompareTo(a));

            int rank = 1;
            foreach (var value in list) {
                foreach (var k in data.Keys) {
                    if (data[k] == value) {
                        Console.WriteLine("{0} {1} {2}", rank, data[k], k);
                    }
                }
                rank++;
            }

            Console.WriteLine();
        }

        static void OrdinalRank(Dictionary<string, int> data) {
            Console.WriteLine("Ordinal Rank");

            var list = data.Values.Distinct().ToList();
            list.Sort((a, b) => b.CompareTo(a));

            int rank = 1;
            foreach (var value in list) {
                foreach (var k in data.Keys) {
                    if (data[k] == value) {
                        Console.WriteLine("{0} {1} {2}", rank, data[k], k);
                        rank++;
                    }
                }
            }

            Console.WriteLine();
        }

        static void FractionalRank(Dictionary<string, int> data) {
            Console.WriteLine("Fractional Rank");

            var list = data.Values.Distinct().ToList();
            list.Sort((a, b) => b.CompareTo(a));

            int rank = 0;
            foreach (var value in list) {
                double avg = 0;
                int cnt = 0;

                foreach (var k in data.Keys) {
                    if (data[k] == value) {
                        rank++;
                        cnt++;
                        avg += rank;
                    }
                }
                avg /= cnt;

                foreach (var k in data.Keys) {
                    if (data[k] == value) {
                        Console.WriteLine("{0:F1} {1} {2}", avg, data[k], k);
                    }
                }
            }

            Console.WriteLine();
        }
    }
}
```

```txt
Standard Rank
1 44 Solomon
2 42 Jason
2 42 Errol
4 41 Gary
4 41 Bernard
4 41 Barry
7 39 Stephen

Modified Rank
1 44 Solomon
3 42 Jason
3 42 Errol
6 41 Gary
6 41 Bernard
6 41 Barry
7 39 Stephen

Dense Rank
1 44 Solomon
2 42 Jason
2 42 Errol
3 41 Gary
3 41 Bernard
3 41 Barry
4 39 Stephen

Ordinal Rank
1 44 Solomon
2 42 Jason
3 42 Errol
4 41 Gary
5 41 Bernard
6 41 Barry
7 39 Stephen

Fractional Rank
1.0 44 Solomon
2.5 42 Jason
2.5 42 Errol
5.0 41 Gary
5.0 41 Bernard
5.0 41 Barry
7.0 39 Stephen
```



## D


```D
import std.algorithm;
import std.stdio;

void main() {
    immutable scores = [
        "Solomon": 44,
        "Jason": 42,
        "Errol": 42,
        "Garry": 41,
        "Bernard": 41,
        "Barry": 41,
        "Stephen": 39
    ];

    scores.standardRank;
    scores.modifiedRank;
    scores.denseRank;
    scores.ordinalRank;
    scores.fractionalRank;
}

/*
Standard ranking
1 44 Solomon
2 42 Jason
2 42 Errol
4 41 Garry
4 41 Bernard
4 41 Barry
7 39 Stephen
*/
void standardRank(const int[string] data) {
    writeln("Standard Rank");

    int rank = 1;
    foreach (value; data.values.dup.sort!"a>b".uniq) {
        int temp = rank;
        foreach(k,v; data) {
            if (v==value) {
                writeln(temp, " ", v, " ", k);
                rank++;
            }
        }
    }

    writeln;
}

/*
Modified ranking
1 44 Solomon
3 42 Jason
3 42 Errol
6 41 Garry
6 41 Bernard
6 41 Barry
7 39 Stephen
*/
void modifiedRank(const int[string] data) {
    writeln("Modified Rank");

    int rank = 0;
    foreach (value; data.values.dup.sort!"a>b".uniq) {
        foreach(k,v; data) {
            if (v==value) {
                rank++;
            }
        }
        foreach(k,v; data) {
            if (v==value) {
                writeln(rank, " ", v, " ", k);
            }
        }
    }

    writeln;
}

/*
Dense ranking
1 44 Solomon
2 42 Jason
2 42 Errol
3 41 Garry
3 41 Bernard
3 41 Barry
4 39 Stephen
*/
void denseRank(const int[string] data) {
    writeln("Dense Rank");

    int rank = 1;
    foreach (value; data.values.dup.sort!"a>b".uniq) {
        foreach(k,v; data) {
            if (v==value) {
                writeln(rank, " ", v, " ", k);
            }
        }
        rank++;
    }

    writeln;
}

/*
Ordinal ranking
1 44 Solomon
2 42 Jason
3 42 Errol
4 41 Garry
5 41 Bernard
6 41 Barry
7 39 Stephen
*/
void ordinalRank(const int[string] data) {
    writeln("Ordinal Rank");

    int rank = 1;
    foreach (value; data.values.dup.sort!"a>b".uniq) {
        foreach(k,v; data) {
            if (v==value) {
                writeln(rank, " ", v, " ", k);
                rank++;
            }
        }
    }

    writeln;
}

/*
Fractional ranking
1,0 44 Solomon
2,5 42 Jason
2,5 42 Errol
5,0 41 Garry
5,0 41 Bernard
5,0 41 Barry
7,0 39 Stephen
*/
void fractionalRank(const int[string] data) {
    writeln("Fractional Rank");

    int rank = 0;
    foreach (value; data.values.dup.sort!"a>b".uniq) {
        real avg = 0;
        int cnt;

        foreach(k,v; data) {
            if (v==value) {
                rank++;
                cnt++;
                avg+=rank;
            }
        }
        avg /= cnt;

        foreach(k,v; data) {
            if (v==value) {
                writef("%0.1f ", avg);
                writeln(v, " ", k);
            }
        }
    }

    writeln;
}
```


```txt
Standard Rank
1 44 Solomon
2 42 Errol
2 42 Jason
4 41 Garry
4 41 Bernard
4 41 Barry
7 39 Stephen

Modified Rank
1 44 Solomon
3 42 Errol
3 42 Jason
6 41 Garry
6 41 Bernard
6 41 Barry
7 39 Stephen

Dense Rank
1 44 Solomon
2 42 Errol
2 42 Jason
3 41 Garry
3 41 Bernard
3 41 Barry
4 39 Stephen

Ordinal Rank
1 44 Solomon
2 42 Errol
3 42 Jason
4 41 Garry
5 41 Bernard
6 41 Barry
7 39 Stephen

Fractional Rank
1.0 44 Solomon
2.5 42 Errol
2.5 42 Jason
5.0 41 Garry
5.0 41 Bernard
5.0 41 Barry
7.0 39 Stephen
```



## Elixir

```elixir
defmodule Ranking do
  def methods(data) do
    IO.puts "stand.\tmod.\tdense\tord.\tfract."
    Enum.group_by(data, fn {score,_name} -> score end)
    |> Enum.map(fn {score,pairs} ->
         names = Enum.map(pairs, fn {_,name} -> name end) |> Enum.reverse
         {score, names}
       end)
    |> Enum.sort_by(fn {score,_} -> -score end)
    |> Enum.with_index
    |> Enum.reduce({1,0,0}, fn {{score, names}, i}, {s_rnk, m_rnk, o_rnk} ->
         d_rnk = i + 1
         m_rnk = m_rnk + length(names)
         f_rnk = ((s_rnk + m_rnk) / 2) |> to_string |> String.replace(".0","")
         o_rnk = Enum.reduce(names, o_rnk, fn name,acc ->
           IO.puts "#{s_rnk}\t#{m_rnk}\t#{d_rnk}\t#{acc+1}\t#{f_rnk}\t#{score} #{name}"
           acc + 1
         end)
         {s_rnk+length(names), m_rnk, o_rnk}
       end)
  end
end

~w"44 Solomon
   42 Jason
   42 Errol
   41 Garry
   41 Bernard
   41 Barry
   39 Stephen"
|> Enum.chunk(2)
|> Enum.map(fn [score,name] -> {String.to_integer(score),name} end)
|> Ranking.methods
```


```txt

stand.  mod.    dense   ord.    fract.
1       1       1       1       1       44 Solomon
2       3       2       2       2.5     42 Jason
2       3       2       3       2.5     42 Errol
4       6       3       4       5       41 Garry
4       6       3       5       5       41 Bernard
4       6       3       6       5       41 Barry
7       7       4       7       7       39 Stephen

```



## Factor

```factor
USING: arrays assocs formatting fry generalizations io kernel
math math.ranges math.statistics math.vectors sequences
splitting.monotonic ;
IN: rosetta-code.ranking

CONSTANT: ranks {
    { 44 "Solomon" } { 42 "Jason" } { 42 "Errol" }
    { 41 "Garry" } { 41 "Bernard" } { 41 "Barry" }
    { 39 "Stephen" }
}

: rank ( seq quot -- seq' )
    '[ [ = ] monotonic-split [ length ] map dup @ [ <array> ]
    2map concat ] call ; inline

: standard ( seq -- seq' ) [ cum-sum0 1 v+n ] rank ;
: modified ( seq -- seq' ) [ cum-sum ] rank ;
: dense    ( seq -- seq' ) [ length [1,b] ] rank ;
: ordinal  ( seq -- seq' ) length [1,b] ;

: fractional ( seq -- seq' )
    [ dup cum-sum swap [ dupd - [a,b) mean ] 2map ] rank ;

: .rank ( quot -- )
    [ ranks dup keys ] dip call swap
    [ first2 "%5u %d %s\n" printf ] 2each ; inline

: ranking-demo ( -- )
    "Standard ranking"   [ standard   ]
    "Modified ranking"   [ modified   ]
    "Dense ranking"      [ dense      ]
    "Ordinal ranking"    [ ordinal    ]
    "Fractional ranking" [ fractional ]
    [ [ print ] [ .rank nl ] bi* ] 2 5 mnapply ;

MAIN: ranking-demo
```

```txt

Standard ranking
    1 44 Solomon
    2 42 Jason
    2 42 Errol
    4 41 Garry
    4 41 Bernard
    4 41 Barry
    7 39 Stephen

Modified ranking
    1 44 Solomon
    3 42 Jason
    3 42 Errol
    6 41 Garry
    6 41 Bernard
    6 41 Barry
    7 39 Stephen

Dense ranking
    1 44 Solomon
    2 42 Jason
    2 42 Errol
    3 41 Garry
    3 41 Bernard
    3 41 Barry
    4 39 Stephen

Ordinal ranking
    1 44 Solomon
    2 42 Jason
    3 42 Errol
    4 41 Garry
    5 41 Bernard
    6 41 Barry
    7 39 Stephen

Fractional ranking
    1 44 Solomon
2+1/2 42 Jason
2+1/2 42 Errol
    5 41 Garry
    5 41 Bernard
    5 41 Barry
    7 39 Stephen

```



## Go


```go
package main

import (
	"fmt"
	"sort"
)

type rankable interface {
	Len() int
	RankEqual(int, int) bool
}

func StandardRank(d rankable) []float64 {
	r := make([]float64, d.Len())
	var k int
	for i := range r {
		if i == 0 || !d.RankEqual(i, i-1) {
			k = i + 1
		}
		r[i] = float64(k)
	}
	return r
}

func ModifiedRank(d rankable) []float64 {
	r := make([]float64, d.Len())
	for i := range r {
		k := i + 1
		for j := i + 1; j < len(r) && d.RankEqual(i, j); j++ {
			k = j + 1
		}
		r[i] = float64(k)
	}
	return r
}

func DenseRank(d rankable) []float64 {
	r := make([]float64, d.Len())
	var k int
	for i := range r {
		if i == 0 || !d.RankEqual(i, i-1) {
			k++
		}
		r[i] = float64(k)
	}
	return r
}

func OrdinalRank(d rankable) []float64 {
	r := make([]float64, d.Len())
	for i := range r {
		r[i] = float64(i + 1)
	}
	return r
}

func FractionalRank(d rankable) []float64 {
	r := make([]float64, d.Len())
	for i := 0; i < len(r); {
		var j int
		f := float64(i + 1)
		for j = i + 1; j < len(r) && d.RankEqual(i, j); j++ {
			f += float64(j + 1)
		}
		f /= float64(j - i)
		for ; i < j; i++ {
			r[i] = f
		}
	}
	return r
}

type scores []struct {
	score int
	name  string
}

func (s scores) Len() int                { return len(s) }
func (s scores) RankEqual(i, j int) bool { return s[i].score == s[j].score }
func (s scores) Swap(i, j int)           { s[i], s[j] = s[j], s[i] }
func (s scores) Less(i, j int) bool {
	if s[i].score != s[j].score {
		return s[i].score > s[j].score
	}
	return s[i].name < s[j].name
}

var data = scores{
	{44, "Solomon"},
	{42, "Jason"},
	{42, "Errol"},
	{41, "Garry"},
	{41, "Bernard"},
	{41, "Barry"},
	{39, "Stephen"},
}

func main() {
	show := func(name string, fn func(rankable) []float64) {
		fmt.Println(name, "Ranking:")
		r := fn(data)
		for i, d := range data {
			fmt.Printf("%4v - %2d %s\n", r[i], d.score, d.name)
		}
	}

	sort.Sort(data)
	show("Standard", StandardRank)
	show("\nModified", ModifiedRank)
	show("\nDense", DenseRank)
	show("\nOrdinal", OrdinalRank)
	show("\nFractional", FractionalRank)
}
```

```txt

Standard Ranking:
   1 - 44 Solomon
   2 - 42 Errol
   2 - 42 Jason
   4 - 41 Barry
   4 - 41 Bernard
   4 - 41 Garry
   7 - 39 Stephen

Modified Ranking:
   1 - 44 Solomon
   3 - 42 Errol
   3 - 42 Jason
   6 - 41 Barry
   6 - 41 Bernard
   6 - 41 Garry
   7 - 39 Stephen

Dense Ranking:
   1 - 44 Solomon
   2 - 42 Errol
   2 - 42 Jason
   3 - 41 Barry
   3 - 41 Bernard
   3 - 41 Garry
   4 - 39 Stephen

Ordinal Ranking:
   1 - 44 Solomon
   2 - 42 Errol
   3 - 42 Jason
   4 - 41 Barry
   5 - 41 Bernard
   6 - 41 Garry
   7 - 39 Stephen

Fractional Ranking:
   1 - 44 Solomon
 2.5 - 42 Errol
 2.5 - 42 Jason
   5 - 41 Barry
   5 - 41 Bernard
   5 - 41 Garry
   7 - 39 Stephen

```



## Haskell


```Haskell
import Data.List (groupBy, sortBy, intercalate)

type Item = (Int, String)

type ItemList = [Item]

type ItemGroups = [ItemList]

type RankItem a = (a, Int, String)

type RankItemList a = [RankItem a]

-- make sure the input is ordered and grouped by score
prepare :: ItemList -> ItemGroups
prepare = groupBy gf . sortBy (flip compare)
  where
    gf (a, _) (b, _) = a == b

-- give an item a rank
rank
  :: Num a
  => a -> Item -> RankItem a
rank n (a, b) = (n, a, b)

-- ranking methods
standard, modified, dense, ordinal :: ItemGroups -> RankItemList Int
standard = ms 1
  where
    ms _ [] = []
    ms n (x:xs) = (rank n <$> x) ++ ms (n + length x) xs

modified = md 1
  where
    md _ [] = []
    md n (x:xs) =
      let l = length x
          nl = n + l
          nl1 = nl - 1
      in (rank nl1 <$> x) ++ md (n + l) xs

dense = md 1
  where
    md _ [] = []
    md n (x:xs) = map (rank n) x ++ md (n + 1) xs

ordinal = zipWith rank [1 ..] . concat

fractional :: ItemGroups -> RankItemList Double
fractional = mf 1.0
  where
    mf _ [] = []
    mf n (x:xs) =
      let l = length x
          o = take l [n ..]
          ld = fromIntegral l
          a = sum o / ld
      in map (rank a) x ++ mf (n + ld) xs

-- sample data
test :: ItemGroups
test =
  prepare
    [ (44, "Solomon")
    , (42, "Jason")
    , (42, "Errol")
    , (41, "Garry")
    , (41, "Bernard")
    , (41, "Barry")
    , (39, "Stephen")
    ]

-- print rank items nicely
nicePrint
  :: Show a
  => String -> RankItemList a -> IO ()
nicePrint xs items = do
  putStrLn xs
  mapM_ np items
  putStr "\n"
  where
    np (a, b, c) = putStrLn $ intercalate "\t" [show a, show b, c]

main :: IO ()
main = do
  nicePrint "Standard:" $ standard test
  nicePrint "Modified:" $ modified test
  nicePrint "Dense:" $ dense test
  nicePrint "Ordinal:" $ ordinal test
  nicePrint "Fractional:" $ fractional test
```

```txt
Standard:
1	44	Solomon
2	42	Jason
2	42	Errol
4	41	Garry
4	41	Bernard
4	41	Barry
7	39	Stephen

Modified:
1	44	Solomon
3	42	Jason
3	42	Errol
6	41	Garry
6	41	Bernard
6	41	Barry
7	39	Stephen

Dense:
1	44	Solomon
2	42	Jason
2	42	Errol
3	41	Garry
3	41	Bernard
3	41	Barry
4	39	Stephen

Ordinal:
1	44	Solomon
2	42	Jason
3	42	Errol
4	41	Garry
5	41	Bernard
6	41	Barry
7	39	Stephen

Fractional:
1.0	44	Solomon
2.5	42	Jason
2.5	42	Errol
5.0	41	Garry
5.0	41	Bernard
5.0	41	Barry
7.0	39	Stephen
```



## J

Implementation:


```J
competitors=:<;._1;._2]0 :0
 44 Solomon
 42 Jason
 42 Errol
 41 Garry
 41 Bernard
 41 Barry
 39 Stephen
)

scores=:  {."1

standard=: 1+i.~
modified=: 1+i:~
dense=: #/.~ # #\@~.
ordinal=: #\
fractional=: #/.~ # ] (+/%#)/. #\

rank=:1 :'<"0@u@:scores,.]'
```


Note that we assume that the competitors are already in the right order. Also, of course (as is common when using J) we use the J command line, because that is portable across operating systems (for example: the OS command line is difficult to use on phones).

Task examples:


```J
   standard rank competitors
┌─┬──┬───────┐
│1│44│Solomon│
├─┼──┼───────┤
│2│42│Jason  │
├─┼──┼───────┤
│2│42│Errol  │
├─┼──┼───────┤
│4│41│Garry  │
├─┼──┼───────┤
│4│41│Bernard│
├─┼──┼───────┤
│4│41│Barry  │
├─┼──┼───────┤
│7│39│Stephen│
└─┴──┴───────┘
   modified rank competitors
┌─┬──┬───────┐
│1│44│Solomon│
├─┼──┼───────┤
│3│42│Jason  │
├─┼──┼───────┤
│3│42│Errol  │
├─┼──┼───────┤
│6│41│Garry  │
├─┼──┼───────┤
│6│41│Bernard│
├─┼──┼───────┤
│6│41│Barry  │
├─┼──┼───────┤
│7│39│Stephen│
└─┴──┴───────┘
   dense rank competitors
┌─┬──┬───────┐
│1│44│Solomon│
├─┼──┼───────┤
│2│42│Jason  │
├─┼──┼───────┤
│2│42│Errol  │
├─┼──┼───────┤
│3│41│Garry  │
├─┼──┼───────┤
│3│41│Bernard│
├─┼──┼───────┤
│3│41│Barry  │
├─┼──┼───────┤
│4│39│Stephen│
└─┴──┴───────┘
   ordinal rank competitors
┌─┬──┬───────┐
│1│44│Solomon│
├─┼──┼───────┤
│2│42│Jason  │
├─┼──┼───────┤
│3│42│Errol  │
├─┼──┼───────┤
│4│41│Garry  │
├─┼──┼───────┤
│5│41│Bernard│
├─┼──┼───────┤
│6│41│Barry  │
├─┼──┼───────┤
│7│39│Stephen│
└─┴──┴───────┘
   fractional rank competitors
┌───┬──┬───────┐
│1  │44│Solomon│
├───┼──┼───────┤
│2.5│42│Jason  │
├───┼──┼───────┤
│2.5│42│Errol  │
├───┼──┼───────┤
│5  │41│Garry  │
├───┼──┼───────┤
│5  │41│Bernard│
├───┼──┼───────┤
│5  │41│Barry  │
├───┼──┼───────┤
│7  │39│Stephen│
└───┴──┴───────┘
```



## Java

```java
import java.util.*;

public class RankingMethods {

    final static String[] input = {"44 Solomon", "42 Jason", "42 Errol",
        "41 Garry", "41 Bernard", "41 Barry", "39 Stephen"};

    public static void main(String[] args) {
        int len = input.length;

        Map<String, int[]> map = new TreeMap<>((a, b) -> b.compareTo(a));
        for (int i = 0; i < len; i++) {
            String key = input[i].split("\\s+")[0];
            int[] arr;
            if ((arr = map.get(key)) == null)
                arr = new int[]{i, 0};
            arr[1]++;
            map.put(key, arr);
        }
        int[][] groups = map.values().toArray(new int[map.size()][]);

        standardRanking(len, groups);
        modifiedRanking(len, groups);
        denseRanking(len, groups);
        ordinalRanking(len);
        fractionalRanking(len, groups);
    }

    private static void standardRanking(int len, int[][] groups) {
        System.out.println("\nStandard ranking");
        for (int i = 0, rank = 0, group = 0; i < len; i++) {
            if (group < groups.length && i == groups[group][0]) {
                rank = i + 1;
                group++;
            }
            System.out.printf("%d %s%n", rank, input[i]);
        }
    }

    private static void modifiedRanking(int len, int[][] groups) {
        System.out.println("\nModified ranking");
        for (int i = 0, rank = 0, group = 0; i < len; i++) {
            if (group < groups.length && i == groups[group][0])
                rank += groups[group++][1];
            System.out.printf("%d %s%n", rank, input[i]);
        }
    }

    private static void denseRanking(int len, int[][] groups) {
        System.out.println("\nDense ranking");
        for (int i = 0, rank = 0; i < len; i++) {
            if (rank < groups.length && i == groups[rank][0])
                rank++;
            System.out.printf("%d %s%n", rank, input[i]);
        }
    }

    private static void ordinalRanking(int len) {
        System.out.println("\nOrdinal ranking");
        for (int i = 0; i < len; i++)
            System.out.printf("%d %s%n", i + 1, input[i]);
    }

    private static void fractionalRanking(int len, int[][] groups) {
        System.out.println("\nFractional ranking");
        float rank = 0;
        for (int i = 0, tmp = 0, group = 0; i < len; i++) {
            if (group < groups.length && i == groups[group][0]) {
                tmp += groups[group++][1];
                rank = (i + 1 + tmp) / 2.0F;
            }
            System.out.printf("%2.1f %s%n", rank, input[i]);
        }
    }
}
```



```txt
Standard ranking
1 44 Solomon
2 42 Jason
2 42 Errol
4 41 Garry
4 41 Bernard
4 41 Barry
7 39 Stephen

Modified ranking
1 44 Solomon
3 42 Jason
3 42 Errol
6 41 Garry
6 41 Bernard
6 41 Barry
7 39 Stephen

Dense ranking
1 44 Solomon
2 42 Jason
2 42 Errol
3 41 Garry
3 41 Bernard
3 41 Barry
4 39 Stephen

Ordinal ranking
1 44 Solomon
2 42 Jason
3 42 Errol
4 41 Garry
5 41 Bernard
6 41 Barry
7 39 Stephen

Fractional ranking
1,0 44 Solomon
2,5 42 Jason
2,5 42 Errol
5,0 41 Garry
5,0 41 Bernard
5,0 41 Barry
7,0 39 Stephen
```




## JavaScript



### ES5


The task formulation doesn't seem to directly explain or determine the order of listing for players whose score is the same.

( This version chooses to use a secondary (alphabetic) sort after the numeric sort by score. That does, of course, affect the ordinal placements for some players)


```JavaScript
(function () {

    var xs = 'Solomon Jason Errol Garry Bernard Barry Stephen'.split(' '),
        ns = [44, 42, 42, 41, 41, 41, 39],

        sorted = xs.map(function (x, i) {
            return { name: x, score: ns[i] };
        }).sort(function (a, b) {
            var c = b.score - a.score;
            return c ? c : a.name < b.name ? -1 : a.name > b.name ? 1 : 0;
        }),

        names = sorted.map(function (x) { return x.name; }),
        scores = sorted.map(function (x) { return x.score; }),

        reversed = scores.slice(0).reverse(),
        unique = scores.filter(function (x, i) {
            return scores.indexOf(x) === i;
        });

    // RANKINGS AS FUNCTIONS OF SCORES: SORTED, REVERSED AND UNIQUE

    var rankings = function (score, index) {
            return {
                name: names[index],
                score: score,

                Ordinal: index + 1,

                Standard: function (n) {
                    return scores.indexOf(n) + 1;
                }(score),

                Modified: function (n) {
                    return reversed.length - reversed.indexOf(n);
                }(score),

                Dense: function (n) {
                    return unique.indexOf(n) + 1;
                }(score),

                Fractional: function (n) {
                    return (
                        (scores.indexOf(n) + 1) +
                        (reversed.length - reversed.indexOf(n))
                    ) / 2;
                }(score)
            };
        },

        tbl = [
            'Name Score Standard Modified Dense Ordinal Fractional'.split(' ')
        ].concat(scores.map(rankings).reduce(function (a, x) {
            return a.concat([
                [x.name, x.score,
                    x.Standard, x.Modified, x.Dense, x.Ordinal, x.Fractional
                ]
            ]);
        }, [])),

        //[[a]] -> bool -> s -> s
        wikiTable = function (lstRows, blnHeaderRow, strStyle) {
            return '{| class="wikitable" ' + (
                strStyle ? 'style="' + strStyle + '"' : ''
            ) + lstRows.map(function (lstRow, iRow) {
                var strDelim = ((blnHeaderRow && !iRow) ? '!' : '|');

                return '\n|-\n' + strDelim + ' ' + lstRow.map(function (v) {
                    return typeof v === 'undefined' ? ' ' : v;
                }).join(' ' + strDelim + strDelim + ' ');
            }).join('') + '\n|}';
        };

    return wikiTable(tbl, true, 'text-align:center');

})();
```


{| class="wikitable" style="text-align:center"
|-
! Name !! Score !! Standard !! Modified !! Dense !! Ordinal !! Fractional
|-
| Solomon || 44 || 1 || 1 || 1 || 1 || 1
|-
| Errol || 42 || 2 || 3 || 2 || 2 || 2.5
|-
| Jason || 42 || 2 || 3 || 2 || 3 || 2.5
|-
| Barry || 41 || 4 || 6 || 3 || 4 || 5
|-
| Bernard || 41 || 4 || 6 || 3 || 5 || 5
|-
| Garry || 41 || 4 || 6 || 3 || 6 || 5
|-
| Stephen || 39 || 7 || 7 || 4 || 7 || 7
|}


### ES6



```JavaScript
((() => {
    const xs = 'Solomon Jason Errol Garry Bernard Barry Stephen'.split(' '),
        ns = [44, 42, 42, 41, 41, 41, 39];

    const sorted = xs.map((x, i) => ({
            name: x,
            score: ns[i]
        }))
        .sort((a, b) => {
            const c = b.score - a.score;
            return c ? c : a.name < b.name ? -1 : a.name > b.name ? 1 : 0;
        });

    const names = sorted.map(x => x.name),
        scores = sorted.map(x => x.score),
        reversed = scores.slice(0)
        .reverse(),
        unique = scores.filter((x, i) => scores.indexOf(x) === i);

    // RANKINGS AS FUNCTIONS OF SCORES: SORTED, REVERSED AND UNIQUE

    // rankings :: Int -> Int -> Dictonary
    const rankings = (score, index) => ({
        name: names[index],
        score,
        Ordinal: index + 1,
        Standard: scores.indexOf(score) + 1,
        Modified: reversed.length - reversed.indexOf(score),
        Dense: unique.indexOf(score) + 1,

        Fractional: (n => (
            (scores.indexOf(n) + 1) +
            (reversed.length - reversed.indexOf(n))
        ) / 2)(score)
    });

    // tbl :: [[[a]]]
    const tbl = [
            'Name Score Standard Modified Dense Ordinal Fractional'.split(' ')
        ].concat(scores.map(rankings)
        .reduce((a, x) => a.concat([
            [x.name, x.score,
                x.Standard, x.Modified, x.Dense, x.Ordinal, x.Fractional
            ]
        ]), []));

    // wikiTable :: [[[a]]] -> Bool -> String -> String
    const wikiTable = (lstRows, blnHeaderRow, strStyle) =>
        `{| class="wikitable" ${strStyle ? 'style="' + strStyle + '"' : ''}
        ${lstRows.map((lstRow, iRow) => {
            const strDelim = ((blnHeaderRow && !iRow) ? '!' : '|');

            return '\n|-\n' + strDelim + ' ' + lstRow
            .map(v => typeof v === 'undefined' ? ' ' : v)
            .join(' ' + strDelim + strDelim + ' ');
        }).join('')}\n|}`;

    return wikiTable(tbl, true, 'text-align:center');
}))();
```


{| class="wikitable" style="text-align:center"

|-
! Name !! Score !! Standard !! Modified !! Dense !! Ordinal !! Fractional
|-
| Solomon || 44 || 1 || 1 || 1 || 1 || 1
|-
| Errol || 42 || 2 || 3 || 2 || 2 || 2.5
|-
| Jason || 42 || 2 || 3 || 2 || 3 || 2.5
|-
| Barry || 41 || 4 || 6 || 3 || 4 || 5
|-
| Bernard || 41 || 4 || 6 || 3 || 5 || 5
|-
| Garry || 41 || 4 || 6 || 3 || 6 || 5
|-
| Stephen || 39 || 7 || 7 || 4 || 7 || 7
|}
## jq

We assume the list of players and their scores has already been sorted,
and that the list takes the form:
 [ player1, score1, player2, score2, ...]

For the sake of brevity, only the ranks are printed.

```jq

# Ties share what would have been their first ordinal number
def standard_ranking:
  . as $raw
  | ([range(1;length;2) | $raw[.]]) as $scores
  | reduce range(1; $scores|length) as $i
      ([1]; if $scores[$i - 1] == $scores[$i] then . + [.[-1]]
            else . + [$i + 1]
            end) ;

def modified_ranking:
  # The helper function resolves [ranks, tentative]
  # by appending the ranks of the ties to "ranks"
  def resolve:
    (.[1] | length) as $length
    | if $length == 0 then .[0]
      else .[1][-1] as $max
           | .[0] + ( .[1] | map( $max) )
      end ;
  . as $raw
  | ([range(1;length;2) | $raw[.]]) as $scores
  | reduce range(1; $scores|length) as $i
      # state: [ranks, tentative]
      ([ [], [1] ];
       if $scores[$i - 1] == $scores[$i] then [.[0], .[1] + [ $i + 1 ]]
       else [ resolve,  [ $i + 1 ] ]
       end )
  | resolve ;

def dense_ranking: # next available
  . as $raw
  | ([range(1;length;2) | $raw[.]]) as $scores
  | reduce range(1; $scores|length) as $i
      ([1]; if $scores[$i - 1] == $scores[$i] then . + [.[-1]]
            else . + [ .[-1] + 1]
            end );

def ordinal_ranking: # unfair to some!
  [ range(1; 1 + length/2) ] ;

def fractional_ranking:
  # The helper function resolves [ranks, tentative]
  # by appending the averages of the tentative ranks to "ranks"
  def resolve:
    (.[1] | length) as $length
    | if $length == 0 then .[0]
      else (.[1] | add  / $length) as $avg
           | .[0] + ( .[1] | map( $avg) )
      end ;
  . as $raw
  | ([range(1;length;2) | $raw[.]]) as $scores
  | reduce range(1; $scores|length) as $i
      # state: [ranks, tentative]
      ([ [], [1] ];
       if $scores[$i - 1] == $scores[$i] then [.[0], .[1] + [ $i + 1 ]]
       else [ resolve,  [ $i + 1 ] ]
       end )
  | resolve ;
```
Task
```jq
def raw:
 [
 "Solomon",  44,
 "Jason"  ,  42,
 "Errol"  ,  42,
 "Garry"  ,  41,
 "Bernard",  41,
 "Barry"  ,  41,
 "Stephen",  39
] ;

def task:
 "standard:   \( raw | standard_ranking)",
 "modified:   \( raw | modified_ranking)",
 "dense:      \( raw | dense_ranking)",
 "ordinal:    \( raw | ordinal_ranking)",
 "fractional: \( raw | fractional_ranking)" ;

task

```

 standard:   [1,2,2,4,4,4,7]
 modified:   [1,3,3,6,6,6,7]
 dense:      [1,2,2,3,3,3,4]
 ordinal:    [1,2,3,4,5,6,7]
 fractional: [1,2.5,2.5,5,5,5,7]


## Julia

'''ties''', a helper function used by some of the ranking methods.  It lists any duplicated scores.

```Julia

function ties{T<:Real}(a::Array{T,1})
    unique(a[2:end][a[2:end] .== a[1:end-1]])
end

```

<code>ties</code> assumes that the there are at least 2 scores in the list to be checked, and the calling functions are designed to avoid calls to it in this case.

'''Standard Ranking Function'''

```Julia

function rankstandard{T<:Real}(a::Array{T,1})
    r = collect(1:length(a))
    1 < r[end] || return r
    for i in ties(a)
        r[a.==i] = r[a.==i][1]
    end
    return r
end

```


'''Modified Ranking Function'''

```Julia

function rankmodified{T<:Real}(a::Array{T,1})
    indexin(a, a)
end

```


'''Dense Ranking Function'''

```Julia

function rankdense{T<:Real}(a::Array{T,1})
    indexin(a, unique(a))
end

```


'''Ordinal Ranking Function'''

```Julia

function rankordinal{T<:Real}(a::Array{T,1})
    collect(1:length(a))
end

```

For ordinal ranking, there are a variety of ways of handling tied scores.  I've taken the easy way out and assumed that the position in the list already reflects any tie-breaking policy.  In this case, there is not much that needs to be done.

'''Fractional Ranking Function'''

```Julia

function rankfractional{T<:Real}(a::Array{T,1})
    r = float64(collect(1:length(a)))
    1.0 < r[end] || return r
    for i in ties(a)
        r[a.==i] = mean(r[a.==i])
    end
    return r
end

```


'''Main'''

```Julia

scores = [44, 42, 42, 41, 41, 41, 39]
names = ["Solomon", "Jason", "Errol", "Garry",
         "Bernard", "Barry", "Stephen"]

srank = rankstandard(scores)
mrank = rankmodified(scores)
drank = rankdense(scores)
orank = rankordinal(scores)
frank = rankfractional(scores)

println("    Name    Score  Std  Mod  Den  Ord  Frac")
for i in 1:length(scores)
    print(@sprintf("   %-7s", names[i]))
    print(@sprintf("%5d ", scores[i]))
    print(@sprintf("%5d", srank[i]))
    print(@sprintf("%5d", mrank[i]))
    print(@sprintf("%5d", drank[i]))
    print(@sprintf("%5d", orank[i]))
    print(@sprintf("%7.2f", frank[i]))
    println()
end

```


```txt

    Name    Score  Std  Mod  Den  Ord  Frac
   Solomon   44     1    1    1    1   1.00
   Jason     42     2    3    2    2   2.50
   Errol     42     2    3    2    3   2.50
   Garry     41     4    6    3    4   5.00
   Bernard   41     4    6    3    5   5.00
   Barry     41     4    6    3    6   5.00
   Stephen   39     7    7    4    7   7.00

```



## Kotlin


```scala
// version 1.0.6

/* all ranking functions assume the array of Pairs is non-empty and already sorted by decreasing order of scores
   and then, if the scores are equal, by reverse alphabetic order of names
*/

fun standardRanking(scores: Array<Pair<Int, String>>): IntArray {
    val rankings = IntArray(scores.size)
    rankings[0] = 1
    for (i in 1 until scores.size) rankings[i] = if (scores[i].first == scores[i - 1].first) rankings[i - 1] else i + 1
    return rankings
}

fun modifiedRanking(scores: Array<Pair<Int, String>>): IntArray {
    val rankings = IntArray(scores.size)
    rankings[0] = 1
    for (i in 1 until scores.size) {
        rankings[i] = i + 1
        val currScore = scores[i].first
        for (j in i - 1 downTo 0) {
            if (currScore != scores[j].first) break
            rankings[j] = i + 1
        }
    }
    return rankings
}

fun denseRanking(scores: Array<Pair<Int, String>>): IntArray {
    val rankings = IntArray(scores.size)
    rankings[0] = 1
    var prevRanking = 1
    for (i in 1 until scores.size) rankings[i] = if (scores[i].first == scores[i - 1].first) prevRanking else ++prevRanking
    return rankings
}

fun ordinalRanking(scores: Array<Pair<Int, String>>) = IntArray(scores.size) { it + 1 }

fun fractionalRanking(scores: Array<Pair<Int, String>>): DoubleArray {
    val rankings = DoubleArray(scores.size)
    rankings[0] = 1.0
    for (i in 1 until scores.size) {
        var k = i
        val currScore = scores[i].first
        for (j in i - 1 downTo 0) {
            if (currScore != scores[j].first) break
            k = j
        }
        val avg = (k..i).average() + 1.0
        for (m in k..i) rankings[m] = avg
    }
    return rankings
}

fun printRankings(title: String, rankings: IntArray, scores: Array<Pair<Int, String>>) {
    println(title + ":")
    for (i in 0 until rankings.size) {
        print ("${rankings[i]}  ")
        println(scores[i].toString().removeSurrounding("(", ")").replace(",", ""))
    }
    println()
}

fun printFractionalRankings(title: String, rankings: DoubleArray, scores: Array<Pair<Int, String>>) {
    println(title + ":")
    for (i in 0 until rankings.size) {
        print ("${"%3.2f".format(rankings[i])}  ")
        println(scores[i].toString().removeSurrounding("(", ")").replace(",", ""))
    }
    println()
}

fun main(args: Array<String>) {
    val scores = arrayOf(44 to "Solomon",  42 to "Jason", 42 to "Errol",  41 to "Garry",
                         41 to "Bernard",  41 to "Barry", 39 to "Stephen")
    printRankings("Standard ranking", standardRanking(scores), scores)
    printRankings("Modified ranking", modifiedRanking(scores), scores)
    printRankings("Dense ranking", denseRanking(scores), scores)
    printRankings("Ordinal ranking", ordinalRanking(scores), scores)
    printFractionalRankings("Fractional ranking", fractionalRanking(scores), scores)
}
```


```txt

Standard ranking:
1  44 Solomon
2  42 Jason
2  42 Errol
4  41 Garry
4  41 Bernard
4  41 Barry
7  39 Stephen

Modified ranking:
1  44 Solomon
3  42 Jason
3  42 Errol
6  41 Garry
6  41 Bernard
6  41 Barry
7  39 Stephen

Dense ranking:
1  44 Solomon
2  42 Jason
2  42 Errol
3  41 Garry
3  41 Bernard
3  41 Barry
4  39 Stephen

Ordinal ranking:
1  44 Solomon
2  42 Jason
3  42 Errol
4  41 Garry
5  41 Bernard
6  41 Barry
7  39 Stephen

Fractional ranking:
1.00  44 Solomon
2.50  42 Jason
2.50  42 Errol
5.00  41 Garry
5.00  41 Bernard
5.00  41 Barry
7.00  39 Stephen

```



## Mathematica


```Mathematica

data = Transpose@{{44, 42, 42, 41, 41, 41, 39}, {"Solomon", "Jason",
     "Errol", "Garry", "Bernard", "Barry", "Stephen"}};

rank[data_, type_] :=
 Module[{t = Transpose@{Sort@data, Range[Length@data, 1, -1]}},
  Switch[type,
   "standard", data/.Rule@@@First/@SplitBy[t, First],
   "modified", data/.Rule@@@Last/@SplitBy[t, First],
   "dense", data/.Thread[#->Range[Length@#]]&@SplitBy[t, First][[All, 1, 1]],
   "ordinal", Reverse@Ordering[data],
   "fractional", data/.Rule@@@(Mean[#]/.{a_Rational:>N[a]}&)/@ SplitBy[t, First]]]

fmtRankedData[data_, type_] :=
 Labeled[Grid[
   SortBy[ArrayFlatten@{{Transpose@{rank[data[[All, 1]], type]},
       data}}, First], Alignment->Left], type<>" ranking:", Top]

Grid@{fmtRankedData[data, #] & /@ {"standard", "modified", "dense",
    "ordinal", "fractional"}}


```


```txt

standard ranking:
1	44	Solomon
3	42	Errol
3	42	Jason
6	41	Barry
6	41	Bernard
6	41	Garry
7	39	Stephen

modified ranking:
1	44	Solomon
2	42	Errol
2	42	Jason
4	41	Barry
4	41	Bernard
4	41	Garry
7	39	Stephen

dense ranking:
1	39	Stephen
2	41	Barry
2	41	Bernard
2	41	Garry
3	42	Errol
3	42	Jason
4	44	Solomon

ordinal ranking:
1	44	Solomon
2	42	Errol
3	42	Jason
4	41	Barry
5	41	Bernard
6	41	Garry
7	39	Stephen

fractional ranking:
1	44	Solomon
2.5	42	Errol
2.5	42	Jason
5	41	Barry
5	41	Bernard
5	41	Garry
7	39	Stephen

```


=={{header|Modula-2}}==
```modula2
MODULE RankingMethods;
FROM FormatString IMPORT FormatString;
FROM RealStr IMPORT RealToFixed;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteCard(c : CARDINAL);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%c", buf, c);
    WriteString(buf)
END WriteCard;

TYPE Entry = RECORD
    name : ARRAY[0..15] OF CHAR;
    score : CARDINAL;
END;

PROCEDURE OrdinalRanking(CONST entries : ARRAY OF Entry);
VAR
    buf : ARRAY[0..31] OF CHAR;
    i : CARDINAL;
BEGIN
    WriteString("Ordinal Ranking");
    WriteLn;
    WriteString("---------------");
    WriteLn;

    FOR i:=0 TO HIGH(entries) DO
        FormatString("%c\t%c\t%s\n", buf, i + 1, entries[i].score, entries[i].name);
        WriteString(buf)
    END;

    WriteLn
END OrdinalRanking;

PROCEDURE StandardRanking(CONST entries : ARRAY OF Entry);
VAR
    buf : ARRAY[0..31] OF CHAR;
    i,j : CARDINAL;
BEGIN
    WriteString("Standard Ranking");
    WriteLn;
    WriteString("---------------");
    WriteLn;

    j := 1;
    FOR i:=0 TO HIGH(entries) DO
        FormatString("%c\t%c\t%s\n", buf, j, entries[i].score, entries[i].name);
        WriteString(buf);
        IF entries[i+1].score < entries[i].score THEN
            j := i + 2
        END
    END;

    WriteLn
END StandardRanking;

PROCEDURE DenseRanking(CONST entries : ARRAY OF Entry);
VAR
    buf : ARRAY[0..31] OF CHAR;
    i,j : CARDINAL;
BEGIN
    WriteString("Dense Ranking");
    WriteLn;
    WriteString("---------------");
    WriteLn;

    j := 1;
    FOR i:=0 TO HIGH(entries) DO
        FormatString("%c\t%c\t%s\n", buf, j, entries[i].score, entries[i].name);
        WriteString(buf);
        IF entries[i+1].score < entries[i].score THEN
            INC(j)
        END
    END;

    WriteLn
END DenseRanking;

PROCEDURE ModifiedRanking(CONST entries : ARRAY OF Entry);
VAR
    buf : ARRAY[0..31] OF CHAR;
    i,j,count : CARDINAL;
BEGIN
    WriteString("Modified Ranking");
    WriteLn;
    WriteString("---------------");
    WriteLn;

    i := 0;
    j := 1;
    WHILE i < HIGH(entries) DO
        IF entries[i].score # entries[i+1].score THEN
            FormatString("%c\t%c\t%s\n", buf, i+1, entries[i].score, entries[i].name);
            WriteString(buf);

            count := 1;
            FOR j:=i+1 TO HIGH(entries)-1 DO
                IF entries[j].score # entries[j+1].score THEN
                    BREAK
                END;
                INC(count)
            END;

            j := 0;
            WHILE j < count-1 DO
                FormatString("%c\t%c\t%s\n", buf, i+count+1, entries[i+j+1].score, entries[i+j+1].name);
                WriteString(buf);
                INC(j)
            END;
            i := i + count - 1
        END;
        INC(i)
    END;

    FormatString("%c\t%c\t%s\n\n", buf, HIGH(entries)+1, entries[HIGH(entries)].score, entries[HIGH(entries)].name);
    WriteString(buf)
END ModifiedRanking;

PROCEDURE FractionalRanking(CONST entries : ARRAY OF Entry);
VAR
    buf : ARRAY[0..32] OF CHAR;
    i,j,count : CARDINAL;
    sum : REAL;
BEGIN
    WriteString("Fractional Ranking");
    WriteLn;
    WriteString("---------------");
    WriteLn;

    sum := 0.0;
    i := 0;
    WHILE i <= HIGH(entries) DO
        IF (i = HIGH(entries) - 1) OR (entries[i].score # entries[i+1].score) THEN
            RealToFixed(FLOAT(i+1),1,buf);
            WriteString(buf);
            FormatString("\t%c\t%s\n", buf, entries[i].score, entries[i].name);
            WriteString(buf)
        ELSE
            sum := FLOAT(i);
            count := 1;

            j := i;
            WHILE entries[j].score = entries[j+1].score DO
                sum := sum + FLOAT(j + 1);
                INC(count);
                INC(j)
            END;
            FOR j:=0 TO count-1 DO
                RealToFixed(sum/FLOAT(count)+1.0,1,buf);
                WriteString(buf);
                FormatString("\t%c\t%s\n", buf, entries[i+j].score, entries[i+j].name);
                WriteString(buf)
            END;
            i := i + count - 1
        END;
        INC(i)
    END
END FractionalRanking;

(* Main *)
TYPE EA = ARRAY[0..6] OF Entry;
VAR entries : EA;
BEGIN
    entries := EA{
        {"Solomon", 44},
        {"Jason", 42},
        {"Errol", 42},
        {"Garry", 41},
        {"Bernard", 41},
        {"Barry", 41},
        {"Stephen", 39}
    };

    OrdinalRanking(entries);
    StandardRanking(entries);
    DenseRanking(entries);
    ModifiedRanking(entries);
    FractionalRanking(entries);

    ReadChar
END RankingMethods.
```

```txt
Ordinal Ranking
---------------
1       44      Solomon
2       42      Jason
3       42      Errol
4       41      Garry
5       41      Bernard
6       41      Barry
7       39      Stephen

Standard Ranking
---------------
1       44      Solomon
2       42      Jason
2       42      Errol
4       41      Garry
4       41      Bernard
5       41      Barry
7       39      Stephen

Dense Ranking
---------------
1       44      Solomon
2       42      Jason
2       42      Errol
3       41      Garry
3       41      Bernard
3       41      Barry
4       39      Stephen

Modified Ranking
---------------
1       44      Solomon
3       42      Jason
3       42      Errol
6       41      Garry
6       41      Bernard
6       41      Barry
7       39      Stephen

Fractional Ranking
---------------
1.0     44      Solomon
2.5     42      Jason
2.5     42      Errol
5.0     41      Garry
5.0     41      Bernard
5.0     41      Barry
7.0     39      Stephen
```



## PARI/GP


Replace "2" with "2.0" in <code>fractional</code> if you prefer decimal to fractional.

```parigp
standard(v)=v=vecsort(v,1,4); my(last=v[1][1]+1); for(i=1,#v, v[i][1]=if(v[i][1]<last,last=v[i][1]; i, v[i-1][1])); v;
modified(v)=v=vecsort(v,1,4); my(last=v[#v][1]-1); forstep(i=#v,1,-1, v[i][1]=if(v[i][1]>last,last=v[i][1]; i, v[i+1][1])); v;
dense(v)=v=vecsort(v,1,4); my(last=v[1][1]+1,rank); for(i=1,#v, v[i][1]=if(v[i][1]<last,last=v[i][1]; rank++, rank)); v;
ordinal(v)=v=vecsort(v,1,4); for(i=1,#v,v[i][1]=i); v;
fractional(v)=my(a=standard(v),b=modified(v)); vector(#v,i,[(a[i][1]+b[i][1])/2,v[i][2]]);

v=[[44,"Solomon"], [42,"Jason"], [42,"Errol"], [41,"Garry"], [41,"Bernard"], [41,"Barry"], [39,"Stephen"]];
standard(v)
modified(v)
dense(v)
ordinal(v)
fractional(v)
```

```txt
%1 = [[1, "Solomon"], [2, "Errol"], [2, "Jason"], [4, "Barry"], [4, "Bernard"], [4, "Garry"], [7, "Stephen"]]
%2 = [[1, "Solomon"], [3, "Errol"], [3, "Jason"], [6, "Barry"], [6, "Bernard"], [6, "Garry"], [7, "Stephen"]]
%3 = [[1, "Solomon"], [2, "Errol"], [2, "Jason"], [3, "Barry"], [3, "Bernard"], [3, "Garry"], [4, "Stephen"]]
%4 = [[1, "Solomon"], [2, "Errol"], [3, "Jason"], [4, "Barry"], [5, "Bernard"], [6, "Garry"], [7, "Stephen"]]
%5 = [[1, "Solomon"], [5/2, "Jason"], [5/2, "Errol"], [5, "Garry"], [5, "Bernard"], [5, "Barry"], [7, "Stephen"]]
```



## Perl

```perl
my %scores = (
    'Solomon' => 44,
    'Jason'   => 42,
    'Errol'   => 42,
    'Garry'   => 41,
    'Bernard' => 41,
    'Barry'   => 41,
    'Stephen' => 39
);

sub tiers {
    my(%s) = @_; my(%h);
    push @{$h{$s{$_}}}, $_ for keys %s;
    @{\%h}{reverse sort keys %h};
}

sub standard {
    my(%s) = @_; my($result);
    my $rank = 1;
    for my $players (tiers %s) {
        $result .= "$rank " . join(', ', sort @$players) . "\n";
        $rank += @$players;
    }
    return $result;
}

sub modified {
    my(%s) = @_; my($result);
    my $rank = 0;
    for my $players (tiers %s) {
        $rank += @$players;
        $result .= "$rank " . join(', ', sort @$players) . "\n";
    }
    return $result;
}

sub dense {
    my(%s) = @_; my($n,$result);
    $result .= sprintf "%d %s\n", ++$n, join(', ', sort @$_) for tiers %s;
    return $result;
}

sub ordinal {
    my(%s) = @_; my($n,$result);
    for my $players (tiers %s) {
        $result .= sprintf "%d %s\n", ++$n, $_ for sort @$players;
    }
    return $result;
}

sub fractional {
    my(%s) = @_; my($result);
    my $rank = 1;
    for my $players (tiers %s) {
        my $beg = $rank;
        my $end = $rank += @$players;
        my $avg = 0;
        $avg += $_/@$players for $beg .. $end-1;
        $result .= sprintf "%3.1f %s\n", $avg, join ', ', sort @$players;
    }
    return $result;
}

print "Standard:\n"    .   standard(%scores) . "\n";
print "Modified:\n"    .   modified(%scores) . "\n";
print "Dense:\n"       .      dense(%scores) . "\n";
print "Ordinal:\n"     .    ordinal(%scores) . "\n";
print "Fractional:\n"  . fractional(%scores) . "\n";
```

<pre  style="height:35ex">Standard:
1 Solomon
2 Errol, Jason
4 Barry, Bernard, Garry
7 Stephen

Modified:
1 Solomon
3 Errol, Jason
6 Barry, Bernard, Garry
7 Stephen

Dense:
1 Solomon
2 Errol, Jason
3 Barry, Bernard, Garry
4 Stephen

Ordinal:
1 Solomon
2 Errol
3 Jason
4 Barry
5 Bernard
6 Garry
7 Stephen

Fractional:
1.0 Solomon
2.5 Errol, Jason
5.0 Barry, Bernard, Garry
7.0 Stephen
```



## Perl 6


```perl6
my @scores =
    Solomon => 44,
    Jason   => 42,
    Errol   => 42,
    Garry   => 41,
    Bernard => 41,
    Barry   => 41,
    Stephen => 39;

sub tiers (@s) { @s.classify(*.value).pairs.sort.reverse.map: { .value».key } }

sub standard (@s) {
    my $rank = 1;
    gather for tiers @s -> @players {
	take $rank => @players;
	$rank += @players;
    }
}

sub modified (@s) {
    my $rank = 0;
    gather for tiers @s -> @players {
	$rank += @players;
	take $rank => @players;
    }
}

sub dense (@s) { tiers(@s).map: { ++$_ => @^players } }

sub ordinal (@s) { @s.map: ++$_ => *.key }

sub fractional (@s) {
    my $rank = 1;
    gather for tiers @s -> @players {
	my $beg = $rank;
	my $end = $rank += @players;
	take [+]($beg ..^ $end) / @players => @players;
    }
}

say   "Standard:";   .perl.say for   standard @scores;
say "\nModified:";   .perl.say for   modified @scores;
say "\nDense:";      .perl.say for      dense @scores;
say "\nOrdinal:";    .perl.say for    ordinal @scores;
say "\nFractional:"; .perl.say for fractional @scores;
```

```txt
Standard:
1 => ["Solomon"]
2 => ["Jason", "Errol"]
4 => ["Garry", "Bernard", "Barry"]
7 => ["Stephen"]

Modified:
1 => ["Solomon"]
3 => ["Jason", "Errol"]
6 => ["Garry", "Bernard", "Barry"]
7 => ["Stephen"]

Dense:
1 => ["Solomon"]
2 => ["Jason", "Errol"]
3 => ["Garry", "Bernard", "Barry"]
4 => ["Stephen"]

Ordinal:
1 => "Solomon"
2 => "Jason"
3 => "Errol"
4 => "Garry"
5 => "Bernard"
6 => "Barry"
7 => "Stephen"

Fractional:
1.0 => ["Solomon"]
2.5 => ["Jason", "Errol"]
5.0 => ["Garry", "Bernard", "Barry"]
7.0 => ["Stephen"]
```



## Phix


```Phix
function ties(sequence scores)
    sequence t = {}, -- {start,num} pairs
             tdx = repeat(0,length(scores))
    integer last = -1
    for i=1 to length(scores) do
        integer this = scores[i][1]
        if this=last then
            t[$][2] += 1
        else
            t = append(t,{i,1})
        end if
        tdx[i] = length(t)
        last = this
    end for
    -- eg {{{1,1},{2,2},{4,3},{7,1}},
    --     {1,2,2,3,3,3,4}}
    return {t,tdx}
end function

enum STANDARD,  -- eg {1,2,2,4,4,4,7}
     MODIFIED,  -- eg {1,3,3,6,6,6,7}
     DENSE,     -- (==tdx)
     ORDINAL,   -- eg {1,2,3,4,5,6,7}
     FRACTION,  -- {1,2.5,2.5,5,5,5,7}
     METHODS = $

function rank(integer i, method, sequence t, tdx)
    integer idx = tdx[i],
            {tx,tn} = t[idx]
    switch method
        case STANDARD: return tx
        case MODIFIED: return tx+tn-1
        case DENSE   : return idx
        case ORDINAL : return i
        case FRACTION: return tx+(tn-1)/2
    end switch
end function

constant scores = {{44, "Solomon"},
                   {42, "Jason"},
                   {42, "Errol"},
                   {41, "Garry"},
                   {41, "Bernard"},
                   {41, "Barry"},
                   {39, "Stephen"}}

sequence {t,tdx} = ties(scores)
printf(1," score name     standard modified dense ordinal fractional\n")
for i=1 to length(scores) do
    sequence ranks = repeat(0,METHODS)
    for method=1 to METHODS do
        ranks[method] = rank(i,method,t,tdx)
    end for
    printf(1,"%5d  %-7s %6g %8g %6g %6g %9g\n",scores[i]&ranks)
end for
```

```txt

 score name     standard modified dense ordinal fractional
   44  Solomon      1        1      1      1         1
   42  Jason        2        3      2      2       2.5
   42  Errol        2        3      2      3       2.5
   41  Garry        4        6      3      4         5
   41  Bernard      4        6      3      5         5
   41  Barry        4        6      3      6         5
   39  Stephen      7        7      4      7         7

```



## PowerShell


```PowerShell

function Get-Ranking
{
    [CmdletBinding(DefaultParameterSetName="Standard")]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [string]
        $InputObject,

        [Parameter(Mandatory=$false,
                   ParameterSetName="Standard")]
        [switch]
        $Standard,

        [Parameter(Mandatory=$false,
                   ParameterSetName="Modified")]
        [switch]
        $Modified,

        [Parameter(Mandatory=$false,
                   ParameterSetName="Dense")]
        [switch]
        $Dense,

        [Parameter(Mandatory=$false,
                   ParameterSetName="Ordinal")]
        [switch]
        $Ordinal,

        [Parameter(Mandatory=$false,
                   ParameterSetName="Fractional")]
        [switch]
        $Fractional
    )

    Begin
    {
        function Get-OrdinalRank ([PSCustomObject[]]$Values)
        {
            for ($i = 0; $i -lt $Values.Count; $i++)
            {
                $Values[$i].Rank = $i + 1
            }

            $Values
        }

        function Get-Rank ([PSCustomObject[]]$Scores)
        {
            foreach ($score in $Scores)
            {
                $score.Group | ForEach-Object {$_.Rank = $score.Rank}
            }

            $Scores.Group
        }

        function New-Competitor ([string]$Name, [int]$Score, [int]$Rank = 0)
        {
            [PSCustomObject]@{
                Name  = $Name
                Score = $Score
                Rank  = $Rank
            }
        }

        $competitors = @()
        $scores = @()
    }
    Process
    {
        @($input) | ForEach-Object {$competitors += New-Competitor -Name $_.Split()[1] -Score $_.Split()[0]}
    }
    End
    {
        $scores = $competitors |
            Sort-Object   -Property Score -Descending |
            Group-Object  -Property Score |
            Select-Object -Property @{Name="Score"; Expression={[int]$_.Name}}, @{Name="Rank"; Expression={0}}, Count, Group

        switch ($PSCmdlet.ParameterSetName)
        {
            "Standard"
            {
                $rank = 1

                for ($i = 0; $i -lt $scores.Count; $i++)
                {
                    $scores[$i].Rank = $rank
                    $rank += $scores[$i].Count
                }

                Get-Rank $scores
            }
            "Modified"
            {
                $rank = 0

                foreach ($score in $scores)
                {
                    $rank = $score.Count + $rank
                    $score.Rank = $rank
                }

                Get-Rank $scores
            }
            "Dense"
            {
                for ($i = 0; $i -lt $scores.Count; $i++)
                {
                    $scores[$i].Rank = $i + 1
                }

                Get-Rank $scores
            }
            "Ordinal"
            {
                Get-OrdinalRank $competitors
            }
            "Fractional"
            {
                Get-OrdinalRank $competitors | Group-Object -Property Score | ForEach-Object {
                    if ($_.Count -gt 1)
                    {
                        $rank = ($_.Group.Rank | Measure-Object -Average).Average

                        foreach ($competitor in $_.Group)
                        {
                            $competitor.Rank = $rank
                        }
                    }
                }

                $competitors
            }
        }
    }
}

```


```PowerShell

$scores = "44 Solomon","42 Jason","42 Errol","41 Garry","41 Bernard","41 Barry","39 Stephen"

```


```PowerShell

$scores | Get-Ranking -Standard

```

```txt

Name    Score Rank
----    ----- ----
Solomon    44    1
Jason      42    2
Errol      42    2
Garry      41    4
Bernard    41    4
Barry      41    4
Stephen    39    7

```


```PowerShell

$scores | Get-Ranking -Modified

```

```txt

Name    Score Rank
----    ----- ----
Solomon    44    1
Jason      42    3
Errol      42    3
Garry      41    6
Bernard    41    6
Barry      41    6
Stephen    39    7

```


```PowerShell

$scores | Get-Ranking -Dense

```

```txt

Name    Score Rank
----    ----- ----
Solomon    44    1
Jason      42    2
Errol      42    2
Garry      41    3
Bernard    41    3
Barry      41    3
Stephen    39    4

```


```PowerShell

$scores | Get-Ranking -Ordinal

```

```txt

Name    Score Rank
----    ----- ----
Solomon    44    1
Jason      42    2
Errol      42    3
Garry      41    4
Bernard    41    5
Barry      41    6
Stephen    39    7

```


```PowerShell

$scores | Get-Ranking -Fractional

```

```txt

Name    Score Rank
----    ----- ----
Solomon    44    1
Jason      42  2.5
Errol      42  2.5
Garry      41    5
Bernard    41    5
Barry      41    5
Stephen    39    7

```



## Python


```python
def mc_rank(iterable, start=1):
    """Modified competition ranking"""
    lastresult, fifo = None, []
    for n, item in enumerate(iterable, start-1):
        if item[0] == lastresult:
            fifo += [item]
        else:
            while fifo:
                yield n, fifo.pop(0)
            lastresult, fifo = item[0], fifo + [item]
    while fifo:
        yield n+1, fifo.pop(0)


def sc_rank(iterable, start=1):
    """Standard competition ranking"""
    lastresult, lastrank = None, None
    for n, item in enumerate(iterable, start):
        if item[0] == lastresult:
            yield lastrank, item
        else:
            yield n, item
            lastresult, lastrank = item[0], n


def d_rank(iterable, start=1):
    """Dense ranking"""
    lastresult, lastrank = None, start - 1,
    for item in iterable:
        if item[0] == lastresult:
            yield lastrank, item
        else:
            lastresult, lastrank = item[0], lastrank + 1
            yield lastrank, item


def o_rank(iterable, start=1):
    """Ordinal  ranking"""
    yield from enumerate(iterable, start)


def f_rank(iterable, start=1):
    """Fractional ranking"""
    last, fifo = None, []
    for n, item in enumerate(iterable, start):
        if item[0] != last:
            if fifo:
                mean = sum(f[0] for f in fifo) / len(fifo)
                while fifo:
                    yield mean, fifo.pop(0)[1]
        last = item[0]
        fifo.append((n, item))
    if fifo:
        mean = sum(f[0] for f in fifo) / len(fifo)
        while fifo:
            yield mean, fifo.pop(0)[1]


if __name__ == '__main__':
    scores = [(44, 'Solomon'),
              (42, 'Jason'),
              (42, 'Errol'),
              (41, 'Garry'),
              (41, 'Bernard'),
              (41, 'Barry'),
              (39, 'Stephen')]

    print('\nScores to be ranked (best first):')
    for s in scores:
        print('        %2i %s' % (s ))
    for ranker in [sc_rank, mc_rank, d_rank, o_rank, f_rank]:
        print('\n%s:' % ranker.__doc__)
        for rank, score in ranker(scores):
            print('  %3g, %r' % (rank, score))
```


```txt
Scores to be ranked (best first):
        44 Solomon
        42 Jason
        42 Errol
        41 Garry
        41 Bernard
        41 Barry
        39 Stephen

Standard competition ranking:
    1, (44, 'Solomon')
    2, (42, 'Jason')
    2, (42, 'Errol')
    4, (41, 'Garry')
    4, (41, 'Bernard')
    4, (41, 'Barry')
    7, (39, 'Stephen')

Modified competition ranking:
    1, (44, 'Solomon')
    3, (42, 'Jason')
    3, (42, 'Errol')
    6, (41, 'Garry')
    6, (41, 'Bernard')
    6, (41, 'Barry')
    7, (39, 'Stephen')

Dense ranking:
    1, (44, 'Solomon')
    2, (42, 'Jason')
    2, (42, 'Errol')
    3, (41, 'Garry')
    3, (41, 'Bernard')
    3, (41, 'Barry')
    4, (39, 'Stephen')

Ordinal  ranking:
    1, (44, 'Solomon')
    2, (42, 'Jason')
    3, (42, 'Errol')
    4, (41, 'Garry')
    5, (41, 'Bernard')
    6, (41, 'Barry')
    7, (39, 'Stephen')

Fractional ranking:
    1, (44, 'Solomon')
  2.5, (42, 'Jason')
  2.5, (42, 'Errol')
    5, (41, 'Garry')
    5, (41, 'Bernard')
    5, (41, 'Barry')
    7, (39, 'Stephen')
```



## Racket


```racket
#lang racket
;; Tim-brown 2014-09-11

;; produces a ranking according to ranking function: rfn
;; INPUT:
;;  lst : (list (score . details))
;;  rfn : (length-scores)
;;         -> (values
;;              ranks-added                  ; how many ranks to add for the next iteration
;;              (idx . rest -> rank-offset)) ; function that produces the rank for the idx^th element
;;                                           ; in the scoring (arity must be >= 1)
(define (rank-list all-scores rfn)
  (let loop ((rank-0 0) (lst (sort all-scores > #:key car)) (acc empty))
    (cond
      [(null? lst) acc]
      [else
       (define 1st-score (caar lst))
       (define (ties-with-1st? cand) (= (car cand) 1st-score))
       (define-values (tied unranked) (splitf-at lst ties-with-1st?))
       ;; all ranking functions should properly handle a singleton tied list
       (define tied-len (length tied))
       (define tied? (> tied-len 1))
       (define-values (ranks-added rank-offset-fn) (rfn tied-len))
       (define ranked-tied (for/list ((t (in-list tied)) (i (in-naturals 1)))
                             (list* tied? (+ rank-0 (rank-offset-fn i)) t)))
       (loop (+ ranks-added rank-0) unranked (append acc ranked-tied))])))

;; Ties share what would have been their first ordinal number
(define (rank-function:Standard l)
  (values l (thunk* 1)))

;; Ties share what would have been their last ordinal number
(define (rank-function:Modified l)
  (values l (thunk* l)))

;; Ties share the next available integer
(define (rank-function:Dense l)
  (values 1 (thunk* 1)))

;; Competitors take the next available integer. Ties are not treated otherwise
(define (rank-function:Ordinal l)
  (values l (λ (n . _) n)))

;; Ties share the mean of what would have been their ordinal numbers
(define (rank-function:Fractional l)
  (values l (thunk* (/ (+ l 1) 2))))

(define score-board
  '((44 . Solomon)
    (42 . Jason)
    (42 . Errol)
    (41 . Garry)
    (41 . Bernard)
    (41 . Barry)
    (39 . Stephen)))

(define format-number
  (match-lambda
    [(? integer? i) i]
    [(and f (app numerator n) (app denominator d))
     (define-values (q r) (quotient/remainder n d))
     (format "~a ~a/~a" q r d)]))

(for ((fn (list
           rank-function:Standard
           rank-function:Modified
           rank-function:Dense
           rank-function:Ordinal
           rank-function:Fractional)))
  (printf "Function: ~s~%" fn)
  (for ((r (in-list (rank-list score-board fn))))
    (printf "~a ~a\t~a\t~a~%"
            (if (car r) "=" " ")
            (format-number (cadr r))
            (caddr r)
            (cdddr r)))
  (newline))
```


```txt
Function: #<procedure:rank-function:Standard>
  1	44	Solomon
= 2	42	Jason
= 2	42	Errol
= 4	41	Garry
= 4	41	Bernard
= 4	41	Barry
  7	39	Stephen

Function: #<procedure:rank-function:Modified>
  1	44	Solomon
= 3	42	Jason
= 3	42	Errol
= 6	41	Garry
= 6	41	Bernard
= 6	41	Barry
  7	39	Stephen

Function: #<procedure:rank-function:Dense>
  1	44	Solomon
= 2	42	Jason
= 2	42	Errol
= 3	41	Garry
= 3	41	Bernard
= 3	41	Barry
  4	39	Stephen

Function: #<procedure:rank-function:Ordinal>
  1	44	Solomon
= 2	42	Jason
= 3	42	Errol
= 4	41	Garry
= 5	41	Bernard
= 6	41	Barry
  7	39	Stephen

Function: #<procedure:rank-function:Fractional>
  1	44	Solomon
= 2 1/2	42	Jason
= 2 1/2	42	Errol
= 5	41	Garry
= 5	41	Bernard
= 5	41	Barry
  7	39	Stephen
```



## REXX


```rexx
/**************************
44 Solomon  1 1 1 1 1
42 Jason    2 3 2 2 2.5
42 Errol    2 3 2 3 2.5
41 Garry    4 6 3 4 5
41 Bernard  4 6 3 5 5
41 Barry    4 6 3 6 5
39 Stephen  7 7 4 7 7
**************************/
Do i=1 To 7
  Parse Value sourceline(i+1) With rank.i name.i .
  /* say rank.i name.i */
  End
pool=0
crank=0
Do i=1 To 7
  If rank.i<>crank Then Do
    pool=pool+1
    lo.pool=i
    hi.pool=i
    n.pool=1
    ii.pool=i
    End
  Else Do
    n.pool=n.pool+1
    hi.pool=i
    ii.pool=ii.pool+i
    End
  crank=rank.i
  pool.i=pool
  End
/*
Do j=1 To pool
  Say 'pool' j n.j lo.j hi.j
  End
*/
cp=0
r=0
cnt.=0
Do i=1 To 7
  p=pool.i
  If p<>cp Then
    r=r+1
  res=rank.i left(name.i,8) lo.p hi.p r i ii.p/n.p
  If res=sourceline(i+1) Then cnt.ok=cnt.ok+1
  Say res
  cp=p
  End
Say cnt.ok 'correct lines'
```

```txt
44 Solomon  1 1 1 1 1
42 Jason    2 3 2 2 2.5
42 Errol    2 3 2 3 2.5
41 Garry    4 6 3 4 5
41 Bernard  4 6 3 5 5
41 Barry    4 6 3 6 5
39 Stephen  7 7 4 7 7
7 correct lines
```



## Ruby


```ruby
ar = "44 Solomon
42 Jason
42 Errol
41 Garry
41 Bernard
41 Barry
39 Stephen".lines.map{|line| line.split}
grouped = ar.group_by{|pair| pair.shift.to_i}
s_rnk = 1
m_rnk = o_rnk = 0
puts "stand.\tmod.\tdense\tord.\tfract."

grouped.each.with_index(1) do |(score, names), d_rnk|
  m_rnk += names.flatten!.size
  f_rnk = (s_rnk + m_rnk)/2.0
  names.each do |name|
    o_rnk += 1
    puts "#{s_rnk}\t#{m_rnk}\t#{d_rnk}\t#{o_rnk}\t#{f_rnk.to_s.sub(".0","")}\t#{score} #{name}"
  end
  s_rnk += names.size
end
```

```txt

stand.	mod.	dense	ord.	fract.
1	1	1	1	1	44 Solomon
2	3	2	2	2.5	42 Jason
2	3	2	3	2.5	42 Errol
4	6	3	4	5	41 Garry
4	6	3	5	5	41 Bernard
4	6	3	6	5	41 Barry
7	7	4	7	7	39 Stephen

```



## Scala

This example uses a type-safe singly-linked object model with no mutable state variables, which makes it longer than the Ruby version above, but demonstrates an object-oriented functional programming approach.

```Scala
object RankingMethods extends App {
    case class Score(score: Int, name: String) // incoming data
    case class Rank[Precision](rank: Precision, names: List[String]) // outgoing results (can be int or double)
    case class State[Precision](n: Int, done: List[Rank[Precision]]) { // internal state, no mutable variables
        def next(n: Int, next: Rank[Precision]) = State(n, next :: done)
    }
    def grouped[Precision](list: List[Score]) = // group names together by score, with highest first
        (scala.collection.immutable.TreeMap[Int, List[Score]]() ++ list.groupBy(-_.score))
        .values.map(_.map(_.name)).foldLeft(State[Precision](1, Nil)) _

    // Ranking methods:

    def rankStandard(list: List[Score]) =
        grouped[Int](list){case (state, names) => state.next(state.n+names.length, Rank(state.n, names))}.done.reverse

    def rankModified(list: List[Score]) =
        rankStandard(list).map(r => Rank(r.rank+r.names.length-1, r.names))

    def rankDense(list: List[Score]) =
        grouped[Int](list){case (state, names) => state.next(state.n+1, Rank(state.n, names))}.done.reverse

    def rankOrdinal(list: List[Score]) =
        list.zipWithIndex.map{case (score, n) => Rank(n+1, List(score.name))}

    def rankFractional(list: List[Score]) =
        rankStandard(list).map(r => Rank((2*r.rank+r.names.length-1.0)/2, r.names))

    // Tests:

    def parseScores(s: String) = s split "\\s+" match {case Array(s,n) => Score(s.toInt, n)}
    val test = List("44 Solomon", "42 Jason", "42 Errol", "41 Garry", "41 Bernard", "41 Barry", "39 Stephen").map(parseScores)

    println("Standard:")
    println(rankStandard(test) mkString "\n")
    println("\nModified:")
    println(rankModified(test) mkString "\n")
    println("\nDense:")
    println(rankDense(test) mkString "\n")
    println("\nOrdinal:")
    println(rankOrdinal(test) mkString "\n")
    println("\nFractional:")
    println(rankFractional(test) mkString "\n")

}
```

```txt
Standard:
Rank(1,List(Solomon))
Rank(2,List(Jason, Errol))
Rank(4,List(Garry, Bernard, Barry))
Rank(7,List(Stephen))

Modified:
Rank(1,List(Solomon))
Rank(3,List(Jason, Errol))
Rank(6,List(Garry, Bernard, Barry))
Rank(7,List(Stephen))

Dense:
Rank(1,List(Solomon))
Rank(2,List(Jason, Errol))
Rank(3,List(Garry, Bernard, Barry))
Rank(4,List(Stephen))

Ordinal:
Rank(1,List(Solomon))
Rank(2,List(Jason))
Rank(3,List(Errol))
Rank(4,List(Garry))
Rank(5,List(Bernard))
Rank(6,List(Barry))
Rank(7,List(Stephen))

Fractional:
Rank(1.0,List(Solomon))
Rank(2.5,List(Jason, Errol))
Rank(5.0,List(Garry, Bernard, Barry))
Rank(7.0,List(Stephen))
```



## Sidef

```ruby
var scores = [
    Pair(Solomon => 44),
    Pair(Jason   => 42),
    Pair(Errol   => 42),
    Pair(Garry   => 41),
    Pair(Bernard => 41),
    Pair(Barry   => 41),
    Pair(Stephen => 39),
]

func tiers(s) {
    s.group_by { .value }.kv.sort.flip.map { .value.map{.key} }
}

func standard(s) {
    var rank = 1
    gather {
        for players in tiers(s) {
            take(Pair(rank, players))
            rank += players.len
        }
    }
}

func modified(s) {
    var rank = 0
    gather {
        for players in tiers(s) {
            rank += players.len
            take(Pair(rank, players))
        }
    }
}

func dense(s) {
    tiers(s).map_kv { |k,v| Pair(k+1, v) }
}

func ordinal(s) {
    s.map_kv { |k,v| Pair(k+1, v.key) }
}

func fractional(s) {
    var rank = 1
    gather {
        for players in tiers(s) {
            var beg = rank
            var end = (rank += players.len)
            take(Pair(sum(beg ..^ end) / players.len, players))
        }
    }
}

func display(r) {
    say r.map {|a| '%3s : %s' % a... }.join("\n")
}

say   "Standard:";   display(  standard(scores))
say "\nModified:";   display(  modified(scores))
say "\nDense:";      display(     dense(scores))
say "\nOrdinal:";    display(   ordinal(scores))
say "\nFractional:"; display(fractional(scores))
```

```txt

Standard:
  1 : ["Solomon"]
  2 : ["Jason", "Errol"]
  4 : ["Garry", "Bernard", "Barry"]
  7 : ["Stephen"]

Modified:
  1 : ["Solomon"]
  3 : ["Jason", "Errol"]
  6 : ["Garry", "Bernard", "Barry"]
  7 : ["Stephen"]

Dense:
  1 : ["Solomon"]
  2 : ["Jason", "Errol"]
  3 : ["Garry", "Bernard", "Barry"]
  4 : ["Stephen"]

Ordinal:
  1 : Solomon
  2 : Jason
  3 : Errol
  4 : Garry
  5 : Bernard
  6 : Barry
  7 : Stephen

Fractional:
  1 : ["Solomon"]
2.5 : ["Jason", "Errol"]
  5 : ["Garry", "Bernard", "Barry"]
  7 : ["Stephen"]

```



## Tcl


```tcl
proc rank {rankingMethod sortedList} {
    # Extract the groups in the data (this is pointless for ordinal...)
    set s [set group [set groups {}]]
    foreach {score who} $sortedList {
	if {$score != $s} {
	    lappend groups [llength $group]
	    set s $score
	    set group {}
	}
	lappend group $who
    }
    lappend groups [llength $group]
    # Construct the rankings; note that we have a zero-sized leading group
    set n 1; set m 0
    foreach g $groups {
	switch $rankingMethod {
	    standard {
		lappend result {*}[lrepeat $g $n]
		incr n $g
	    }
	    modified {
		lappend result {*}[lrepeat $g [incr m $g]]
	    }
	    dense {
		lappend result {*}[lrepeat $g $m]
		incr m
	    }
	    ordinal {
		for {set i 0} {$i < $g} {incr i} {
		    lappend result [incr m]
		}
	    }
	    fractional {
		set val [expr {($n + [incr n $g] - 1) / 2.0}]
		lappend result {*}[lrepeat $g [format %g $val]]
	    }
	}
    }
    return $result
}

set data {
    44 Solomon
    42 Jason
    42 Errol
    41 Garry
    41 Bernard
    41 Barry
    39 Stephen
}
foreach method {standard modified dense ordinal fractional} {
    puts "Using method '$method'...\n  Rank\tScore\tWho"
    foreach rank [rank $method $data] {score who} $data {
	puts "  $rank\t$score\t$who"
    }
}
```

```txt

Using method 'standard'...
  Rank	Score	Who
  1	44	Solomon
  2	42	Jason
  2	42	Errol
  4	41	Garry
  4	41	Bernard
  4	41	Barry
  7	39	Stephen
Using method 'modified'...
  Rank	Score	Who
  1	44	Solomon
  3	42	Jason
  3	42	Errol
  6	41	Garry
  6	41	Bernard
  6	41	Barry
  7	39	Stephen
Using method 'dense'...
  Rank	Score	Who
  1	44	Solomon
  2	42	Jason
  2	42	Errol
  3	41	Garry
  3	41	Bernard
  3	41	Barry
  4	39	Stephen
Using method 'ordinal'...
  Rank	Score	Who
  1	44	Solomon
  2	42	Jason
  3	42	Errol
  4	41	Garry
  5	41	Bernard
  6	41	Barry
  7	39	Stephen
Using method 'fractional'...
  Rank	Score	Who
  1	44	Solomon
  2.5	42	Jason
  2.5	42	Errol
  5	41	Garry
  5	41	Bernard
  5	41	Barry
  7	39	Stephen

```



## Visual FoxPro


```vfp

#DEFINE CTAB CHR(9)
#DEFINE CRLF CHR(13) + CHR(10)
LOCAL lcTxt As String, i As Integer
CLOSE DATABASES ALL
SET SAFETY OFF
CLEAR
CREATE CURSOR scores (score I, name V(8), rownum I AUTOINC)
INDEX ON score TAG score COLLATE "Machine"
SET ORDER TO 0
INSERT INTO scores (score, name) VALUES (44, "Solomon")
INSERT INTO scores (score, name) VALUES (42, "Jason")
INSERT INTO scores (score, name) VALUES (42, "Errol")
INSERT INTO scores (score, name) VALUES (41, "Garry")
INSERT INTO scores (score, name) VALUES (41, "Bernard")
INSERT INTO scores (score, name) VALUES (41, "Barry")
INSERT INTO scores (score, name) VALUES (39, "Stephen")

CREATE CURSOR ranks (sc_rank I, mod_rank I, dense I, ordinal I, fractional B(1), score I, name V(8))
INDEX ON score TAG score COLLATE "Machine"
SET ORDER TO 0
APPEND FROM DBF("scores") FIELDS score, name
Std_Comp()
Modified()
Dense()
Ordinal()
Fractional()
COPY TO ranks.txt TYPE DELIMITED WITH TAB
lcTxt = ""
FOR i = 1 TO FCOUNT()
    lcTxt = lcTxt + FIELD(i) + CTAB
ENDFOR
lcTxt = LEFT(lcTxt, LEN(lcTxt) - 1) + CRLF + FILETOSTR("ranks.txt")
STRTOFILE(lcTxt, "ranks.txt")
MODIFY FILE ranks.txt
SET SAFETY ON

FUNCTION ScoreGroup(aa)
LOCAL n As Integer
SELECT score, COUNT(*) As num FROM scores ;
GROUP BY score ORDER BY score DESC INTO ARRAY aa
n = _TALLY
RETURN n
ENDFUNC

PROCEDURE Std_Comp
LOCAL n, i, nn
LOCAL ARRAY a[1]
SELECT ranks
BLANK FIELDS sc_rank ALL
nn = ScoreGroup(@a)
n = 1
FOR i = 1 TO nn
    REPLACE sc_rank WITH n FOR score = a[i,1]
    n = n + a[i,2]
ENDFOR
ENDPROC

PROCEDURE Modified
LOCAL n, i, nn
LOCAL ARRAY a[1]
SELECT ranks
BLANK FIELDS mod_rank ALL
nn = ScoreGroup(@a)
n = 0
FOR i = 1 TO nn
    n = n + a[i,2]
    REPLACE mod_rank WITH n FOR score = a[i,1]
ENDFOR
ENDPROC

PROCEDURE Dense
LOCAL n, i, nn
LOCAL ARRAY a[1]
SELECT ranks
BLANK FIELDS dense ALL
nn = ScoreGroup(@a)
SELECT ranks
n = 0
FOR i = 1 TO nn
    n = n + a[i,2]
    REPLACE dense WITH i FOR score = a[i,1]
ENDFOR
ENDPROC

PROCEDURE Ordinal
SELECT ranks
BLANK FIELDS ordinal ALL
REPLACE ordinal WITH RECNO() ALL
ENDPROC

PROCEDURE Fractional
LOCAL i As Integer, nn As Integer
LOCAL ARRAY a[1]
SELECT ranks
BLANK FIELDS fractional ALL
SELECT CAST(AVG(rownum) As B(1)), score FROM scores ;
GROUP BY score ORDER BY score DESC INTO ARRAY a
nn = _TALLY
FOR i = 1 TO nn
    REPLACE fractional WITH a[i,1] FOR score = a[i,2]
ENDFOR
ENDPROC

```

```txt

SC_RANK	MOD_RANK	DENSE	ORDINAL	FRACTIONAL	SCORE	NAME
1	1	1	1	1.0	44	"Solomon"
2	3	2	2	2.5	42	"Jason"
2	3	2	3	2.5	42	"Errol"
4	6	3	4	5.0	41	"Garry"
4	6	3	5	5.0	41	"Bernard"
4	6	3	6	5.0	41	"Barry"
7	7	4	7	7.0	39	"Stephen"

```



## zkl


```zkl
fcn group(scores){ // group like scores into one list --> list of lists
   sink:=List();
   scores.reduce('wrap(ps,sn,buf){
      if(sn[0]!=ps){ sink.append(buf.copy()); buf.clear(); }
      buf+sn;
      sn[0];
   },scores[0][0],buf:=List());
   sink.append(buf);
}
fcn print(list,rank){
   list.apply2('wrap(sn){ "%2s: %s (%d)".fmt(rank,sn[1],sn[0]):println(_); });
}
fcn rankViaStandard(scores){
   rank:=1;
   foreach group in (group(scores)){ print(group,rank); rank+=group.len(); }
}
fcn rankViaModified(scores){
   rank:=0;
   foreach group in (group(scores)){ rank+=group.len(); print(group,rank); }
}
fcn rankViaDense(scores){
   rank:=1;
   foreach group in (group(scores)){ print(group,rank); rank+=1; }
}
fcn rankViaOrdinal(scores){
   scores.apply2('wrap(sn,rr){ "%2s: %s (%d)".fmt(rr.inc(),sn[1],sn[0]):println(_); },Ref(1));
}
fcn rankViaFractional(scores){
   rank:=1;
   foreach group in (group(scores)){
      n:=group.len(); r:=rank.reduce(n,'+,0.0)/n; rank+=n;
      print(group,"%5.2f".fmt(r));
   }
}
```


```zkl
   // these are sorted!?!
scores:=T(T(44,"Solomon"), T(42,"Jason"), T(42,"Errol"), T(41,"Garry"),
        T(41,"Bernard"),T(41,"Barry"),T(39,"Stephen"),);
"Standard:"  .println(); rankViaStandard(scores);
"Modified:"  .println(); rankViaModified(scores);
"Dense:"     .println(); rankViaDense(scores);
"Ordinal:"   .println(); rankViaOrdinal(scores);
"Fractional:".println(); rankViaFractional(scores);
```

```txt

Standard:
 1: Solomon (44)
 2: Jason (42)
 2: Errol (42)
 4: Garry (41)
 4: Bernard (41)
 4: Barry (41)
 7: Stephen (39)
Modified:
 1: Solomon (44)
 3: Jason (42)
 3: Errol (42)
 6: Garry (41)
 6: Bernard (41)
 6: Barry (41)
 7: Stephen (39)
Dense:
 1: Solomon (44)
 2: Jason (42)
 2: Errol (42)
 3: Garry (41)
 3: Bernard (41)
 3: Barry (41)
 4: Stephen (39)
Ordinal:
 1: Solomon (44)
 2: Jason (42)
 3: Errol (42)
 4: Garry (41)
 5: Bernard (41)
 6: Barry (41)
 7: Stephen (39)
Fractional:
 1.00: Solomon (44)
 2.50: Jason (42)
 2.50: Errol (42)
 5.00: Garry (41)
 5.00: Bernard (41)
 5.00: Barry (41)
 7.00: Stephen (39)

```

