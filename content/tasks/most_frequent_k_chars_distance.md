+++
title = "Most frequent k chars distance"
description = ""
date = 2019-03-24T21:21:39Z
aliases = []
[extra]
id = 17441
[taxonomies]
categories = ["task"]
tags = []
+++

In [[wp:information theory|information theory]], the '''MostFreqKDistance''' is a [[wp:String metric|string metric]] for quickly estimating how [[wp:Similarity measure|similar]] two [[wp:Order theory|ordered sets]] or [[wp:String (computer science)|strings]] are. The scheme was invented by Sadi Evren SEKER,<ref name="mfkc"/> and initially used in [[wp:text mining|text mining]] applications like [[wp:author recognition|author recognition]].<ref name="mfkc">{{citation
 | last1 = SEKER | first1 = Sadi E. | author1-link = Sadi Evren SEKER
 | last2 = Altun | first2 = Oguz
 | last3 = Ayan | first3 = Ugur
 | last4 = Mert | first4 = Cihan
 | contribution = A Novel String Distance Function based on Most Frequent K Characters
 | volume = 4
 | issue = 2
 | pages = 177-183
 | publisher = [[wp:International Association of Computer Science and Information Technology Press (IACSIT Press)]]
 | title = [[wp:International Journal of Machine Learning and Computing (IJMLC)]]
 | url = http://arxiv.org/abs/1401.6596
 | year = 2014}}.</ref>
This method is originally based on a hashing function, MaxFreqKChars<ref name="hashfunc">{{citation
 | last1 = Seker | first1 = Sadi E. | author1-link = Sadi Evren SEKER
 | last2 = Mert | first2 = Cihan
 | contribution = A Novel Feature Hashing For Text Mining
 | url = http://journal.ibsu.edu.ge/index.php/jtst/article/view/428
 | pages = 37 -41
 | publisher = [[wp:International Black Sea University]]
 | title = Journal of Technical Science and Technologies
 | ISSN = 2298-0032
 | volume = 2
 | issue = 1
 | year = 2013}}.</ref>
classical [[wp:author recognition|author recognition]] problem and idea first came out while studying [[wp:data stream mining|data stream mining]].<ref name="author">{{citation
 | last1 = Seker | first1 = Sadi E. | author1-link = Sadi Evren SEKER
 | last2 = Al-Naami | first2 = Khaled
 | last3 = Khan | first3 = Latifur
 | contribution = Author attribution on streaming data
 | doi = 10.1109/IRI.2013.6642511
 | url = http://ieeexplore.ieee.org/xpl/articleDetails.jsp?tp=&arnumber=6642511
 | pages = 497-503
 | publisher = [[wp:IEEE]]
 | title = Information Reuse and Integration (IRI), 2013 IEEE 14th International Conference on, San Fransisco, USA, Aug 14-16, 2013
 | year = 2013}}.</ref>
The string distance

;Definition
Method has two steps.
* [[wp:Hash function|Hash]] input strings str1 and str2 separately using MostFreqKHashing and output hstr1 and hstr2 respectively
* Calculate string distance (or string similarity coefficient) of two hash outputs, hstr1 and hstr2 and output an integer value

;Most Frequent K Hashing
The first step of algorithm is calculating the hashing based on the most frequent k characters. The hashing algorithm has below steps:
 '''string function''' ''MostFreqKHashing'' ('''string''' inputString, '''int''' K)
     '''def string''' outputString
     '''for each''' distinct characters
         '''count''' occurrence of each character
     '''for''' i '''from''' 0 '''to''' K
         '''char''' c = '''next''' most frequent ''i''<sup>th</sup> character  (if two chars have same frequency than get the first occurrence in inputString)
         '''int''' count = number of occurrence of the character
         '''append''' to ''outputString'', c and count
     '''end for'''
     '''return''' outputString

Aim of 'Most Frequent K Hashing' function is calculating the most count of each character and returning the K most frequent character with the character and count. Rules of hash can be listed as below:
* Output will hold the character and count
* Most frequent character and count will appear before the least frequent at the output
* if two characters have equal frequency, the first appearing in input will appear before at the output

Similar to the most of [[wp:hashing function|hashing functions]], ''Most Frequent K Hashing'' is also a [[wp:one way function]].

;Most Frequent K Distance
Distance calculation between two strings is based on the hash outputs of two strings.
 '''int function''' ''MostFreqKSimilarity'' ('''string''' inputStr1, '''string''' inputStr2)
     '''def int''' similarity
     '''for each''' c = '''next''' character '''from''' inputStr1
         '''lookup''' c '''in''' inputStr2
         '''if''' c '''is not null'''
             similarity '''+=''' frequency of c in inputStr1 + frequency of c in inputStr2
     '''return''' similarity
Above function, simply gets two input strings, previously outputted from the MostFreqKHashing function. From the most frequent k hashing function, the characters and their frequencies are returned. So, the similarity function calculates the similarity based on characters and their frequencies by checking if the same character appears on both strings and if their frequencies are equal.

In some implementations, the distance metric is required instead of similarity coefficient. In order to convert the output of above similarity coefficient to distance metric, the output can be subtracted from any constant value (like the maximum possible output value). For the case, it is also possible to implement a [[wp:wrapper function]] over above two functions.

;;String Distance Wrapper Function
In order to calculate the distance between two strings, below function can be implemented
 '''int function''' MostFreqKSDF ('''string''' inputStr1, '''string''' inputStr2, '''int''' K, '''int''' maxDistance)
     '''return''' maxDistance - MostFreqKSimilarity(MostFreqKHashing(inputStr1,K), MostFreqKHashing(inputStr2,K))

Any call to above string distance function will supply two input strings and a maximum distance value. The function will calculate the similarity and subtract that value from the maximum possible distance. It can be considered as a simple [[wp:additive inverse]] of similarity.

;Examples
Let's consider maximum 2 frequent hashing over two strings ‘research’ and ‘seeking’.

```javascript
MostFreqKHashing('research',2) = 'r2e2'
```

because we have 2 'r' and 2 'e' characters with the highest frequency and we return in the order they appear in the string.

```javascript
MostFreqKHashing('seeking',2) = 'e2s1'
```

Again we have character 'e' with highest frequency and rest of the characters have same frequency of 1, so we return the first character of equal frequencies, which is 's'.
Finally we make the comparison:

```javascript
MostFreqKSimilarity('r2e2','e2s1') = 2
```

We simply compared the outputs and only character occurring in both input is character 'e' and the occurrence in both input is 2.
Instead running the sample step by step as above, we can simply run by using the string distance wrapper function as below:

```javascript
MostFreqKSDF('research', 'seeking',2) = 2
```


Below table holds some sample runs between example inputs for K=2:
{|class="wikitable"
|-
!Inputs
!Hash Outputs
!SDF Output (max from 10)
|-
|'night'
'nacht'
|n1i1
n1a1
|9
|-
|'my'
'a'
|m1y1
a1NULL0
|10
|-
|‘research’
‘research’
|r2e2
r2e2
|8
|-
|‘aaaaabbbb’
‘ababababa’
|a5b4
a5b4
|1
|-
|‘significant’
‘capabilities’
|i3n2
i3a2
|5
|}

Method is also suitable for bioinformatics to compare the genetic strings like in [[wp:fasta]] format

```txt

Str1= LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV
Str2 = EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG
MostFreqKHashing(str1,2) = L9T8
MostFreqKHashing(str2,2) = F9L8
MostFreqKSDF(str1,str2,2,100) = 83

```




## C++


```cpp
#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <algorithm>
#include <utility>
#include <sstream>

std::string mostFreqKHashing ( const std::string & input , int k ) {
   std::ostringstream oss ;
   std::map<char, int> frequencies ;
   for ( char c : input ) {
      frequencies[ c ] = std::count ( input.begin( ) , input.end( ) , c ) ;
   }
   std::vector<std::pair<char , int>> letters ( frequencies.begin( ) , frequencies.end( ) ) ;
   std::sort ( letters.begin( ) , letters.end( ) , [input] ( std::pair<char, int> a ,
	         std::pair<char, int> b ) { char fc = std::get<0>( a ) ; char fs = std::get<0>( b ) ;
	         int o = std::get<1>( a ) ; int p = std::get<1>( b ) ; if ( o != p ) { return o > p ; }
	         else { return input.find_first_of( fc ) < input.find_first_of ( fs ) ; } } ) ;
   for ( int i = 0 ; i < letters.size( ) ; i++ ) {
      oss << std::get<0>( letters[ i ] ) ;
      oss << std::get<1>( letters[ i ] ) ;
   }
   std::string output ( oss.str( ).substr( 0 , 2 * k ) ) ;
   if ( letters.size( ) >= k ) {
      return output ;
   }
   else {
      return output.append( "NULL0" ) ;
   }
}

int mostFreqKSimilarity ( const std::string & first , const std::string & second ) {
   int i = 0 ;
   while ( i < first.length( ) - 1  ) {
      auto found = second.find_first_of( first.substr( i , 2 ) ) ;
      if ( found != std::string::npos )
	 return std::stoi ( first.substr( i , 2 )) ;
      else
	 i += 2 ;
   }
   return 0 ;
}

int mostFreqKSDF ( const std::string & firstSeq , const std::string & secondSeq , int num ) {
   return mostFreqKSimilarity ( mostFreqKHashing( firstSeq , num ) , mostFreqKHashing( secondSeq , num ) ) ;
}

int main( ) {
   std::string s1("LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV" ) ;
   std::string s2( "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG" ) ;
   std::cout << "MostFreqKHashing( s1 , 2 ) = " << mostFreqKHashing( s1 , 2 ) << '\n' ;
   std::cout << "MostFreqKHashing( s2 , 2 ) = " << mostFreqKHashing( s2 , 2 ) << '\n' ;
   return 0 ;
}

```

```txt
MostFreqKHashing( s1 , 2 ) = L9T8
MostFreqKHashing( s2 , 2 ) = F9L8

```



## Go

```go
package main

import (
    "fmt"
    "sort"
)

type cf struct {
    c rune
    f int
}

func reverseStr(s string) string {
    runes := []rune(s)
    for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
        runes[i], runes[j] = runes[j], runes[i]
    }
    return string(runes)
}

func indexOfCf(cfs []cf, r rune) int {
    for i, cf := range cfs {
        if cf.c == r {
            return i
        }
    }
    return -1
}

func minOf(i, j int) int {
    if i < j {
        return i
    }
    return j
}

func mostFreqKHashing(input string, k int) string {
    var cfs []cf
    for _, r := range input {
        ix := indexOfCf(cfs, r)
        if ix >= 0 {
            cfs[ix].f++
        } else {
            cfs = append(cfs, cf{r, 1})
        }
    }
    sort.SliceStable(cfs, func(i, j int) bool {
        return cfs[i].f > cfs[j].f // descending, preserves order when equal
    })
    acc := ""
    min := minOf(len(cfs), k)
    for _, cf := range cfs[:min] {
        acc += fmt.Sprintf("%c%c", cf.c, cf.f)
    }
    return acc
}

func mostFreqKSimilarity(input1, input2 string) int {
    similarity := 0
    runes1, runes2 := []rune(input1), []rune(input2)
    for i := 0; i < len(runes1); i += 2 {
        for j := 0; j < len(runes2); j += 2 {
            if runes1[i] == runes2[j] {
                freq1, freq2 := runes1[i+1], runes2[j+1]
                if freq1 != freq2 {
                    continue // assuming here that frequencies need to match
                }
                similarity += int(freq1)
            }
        }
    }
    return similarity
}

func mostFreqKSDF(input1, input2 string, k, maxDistance int) {
    fmt.Println("input1 :", input1)
    fmt.Println("input2 :", input2)
    s1 := mostFreqKHashing(input1, k)
    s2 := mostFreqKHashing(input2, k)
    fmt.Printf("mfkh(input1, %d) = ", k)
    for i, c := range s1 {
        if i%2 == 0 {
            fmt.Printf("%c", c)
        } else {
            fmt.Printf("%d", c)
        }
    }
    fmt.Printf("\nmfkh(input2, %d) = ", k)
    for i, c := range s2 {
        if i%2 == 0 {
            fmt.Printf("%c", c)
        } else {
            fmt.Printf("%d", c)
        }
    }
    result := maxDistance - mostFreqKSimilarity(s1, s2)
    fmt.Printf("\nSDF(input1, input2, %d, %d) = %d\n\n", k, maxDistance, result)
}

func main() {
    pairs := [][2]string{
        {"research", "seeking"},
        {"night", "nacht"},
        {"my", "a"},
        {"research", "research"},
        {"aaaaabbbb", "ababababa"},
        {"significant", "capabilities"},
    }
    for _, pair := range pairs {
        mostFreqKSDF(pair[0], pair[1], 2, 10)
    }

    s1 := "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV"
    s2 := "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG"
    mostFreqKSDF(s1, s2, 2, 100)
    s1 = "abracadabra12121212121abracadabra12121212121"
    s2 = reverseStr(s1)
    mostFreqKSDF(s1, s2, 2, 100)
}
```


```txt

input1 : research
input2 : seeking
mfkh(input1, 2) = r2e2
mfkh(input2, 2) = e2s1
SDF(input1, input2, 2, 10) = 8

input1 : night
input2 : nacht
mfkh(input1, 2) = n1i1
mfkh(input2, 2) = n1a1
SDF(input1, input2, 2, 10) = 9

input1 : my
input2 : a
mfkh(input1, 2) = m1y1
mfkh(input2, 2) = a1
SDF(input1, input2, 2, 10) = 10

input1 : research
input2 : research
mfkh(input1, 2) = r2e2
mfkh(input2, 2) = r2e2
SDF(input1, input2, 2, 10) = 6

input1 : aaaaabbbb
input2 : ababababa
mfkh(input1, 2) = a5b4
mfkh(input2, 2) = a5b4
SDF(input1, input2, 2, 10) = 1

input1 : significant
input2 : capabilities
mfkh(input1, 2) = i3n2
mfkh(input2, 2) = i3a2
SDF(input1, input2, 2, 10) = 7

input1 : LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV
input2 : EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG
mfkh(input1, 2) = L9T8
mfkh(input2, 2) = F9L8
SDF(input1, input2, 2, 100) = 100

input1 : abracadabra12121212121abracadabra12121212121
input2 : 12121212121arbadacarba12121212121arbadacarba
mfkh(input1, 2) = 112a10
mfkh(input2, 2) = 112210
SDF(input1, input2, 2, 100) = 88

```



## Haskell


```Haskell
module MostFrequentK
   where
import Data.List ( nub , sortBy )
import qualified Data.Set as S

count :: Eq a => [a] -> a -> Int
count [] x = 0
count ( x:xs ) k
   |x == k = 1 + count xs k
   |otherwise = count xs k

orderedStatistics :: String -> [(Char , Int)]
orderedStatistics s = sortBy myCriterion $ nub $ zip s ( map (\c -> count s c ) s )
   where
      myCriterion :: (Char , Int) -> (Char , Int) -> Ordering
      myCriterion (c1 , n1) (c2, n2)
	 |n1 > n2 = LT
	 |n1 < n2 = GT
	 |n1 == n2 = compare ( found c1 s ) ( found c2 s )
      found :: Char -> String -> Int
      found e s = length $ takeWhile (/= e ) s

mostFreqKHashing :: String -> Int -> String
mostFreqKHashing s n = foldl ((++)) [] $ map toString $ take n $ orderedStatistics s
   where
      toString :: (Char , Int) -> String
      toString ( c , i ) = c : show i

mostFreqKSimilarity :: String -> String -> Int
mostFreqKSimilarity s t = snd $ head $ S.toList $ S.fromList ( doublets s ) `S.intersection`
                           S.fromList ( doublets t )
   where
      toPair :: String -> (Char , Int)
      toPair s = ( head s , fromEnum ( head $ tail s ) - 48 )
      doublets :: String -> [(Char , Int)]
      doublets str = map toPair [take 2 $ drop start str | start <- [0 , 2 ..length str - 2]]

mostFreqKSDF :: String -> String -> Int ->Int
mostFreqKSDF s t n = mostFreqKSimilarity ( mostFreqKHashing s n ) (mostFreqKHashing t n )

```

```txt
mostFrequentKHashing "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV" 2
"L9T8"
*MostFrequentK> mostFrequentKHashing "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG" 2
"F9L8"

```



## J

'''Solution''':
```j
NB. String Distance Wrapper Function
mfksDF     =: {:@:[ - (mfks@:(mfkh&.>)~ {.)~

NB. Most Frequent K Distance
mfks       =:  score@:(charMap@:[ {"1 charVals@:])/@:kHashes
  score    =.  ([ +/ .* =)/                  NB. (+ +/ .* *.&:*)/  for sum += freq_in_left + freq_in_right
  charMap  =.  (,&< i.&> <@:~.@:,)&;/
  charVals =.  (; , 0:)"1
  kHashes  =.  0 1 |: ({.&>~ [: <./ #&>)

NB. Most Frequent K Hashing
mfkh       =:   _&$: : (takeK freqHash)      NB. Default LHA of _ means "return complete frequency table"
  takeK    =.  (<.#) {. ]
  freqHash =.  ~. (] \:~ ,.&:(<"0)) #/.~

NB. No need to fix mfksDF
mfkh =: mfkh f.
mfks =: mfks f.
```


'''Examples''':
```j
verb define ''
	fkh =. ;@:,@:(":&.>) NB. format k hash

	assert. 'r2e2 e2s1' (-: [: fkh 2&mfkh)&>&;: 'research seeking'
	assert. 2 = mfks 2 mfkh&.> 'research';'seeking'

	assert. 'n1i1 n1a1' (-: [: fkh 2&mfkh)&>&;: 'night nacht'
	assert. 9 = 2 10 mfksDF 'night';'nacht'

	assert. 'm1y1 a1'  (-: [: fkh 2&mfkh)&>&;: 'my a'
	assert. 10 = 2 10 mfksDF 'my';'a'

	assert. 'r2e2' -: fkh 2 mfkh 'research'
	assert. 6 = 2 10 mfksDF 'research';'research'  NB. task says 8; right answer is 6

	assert. 'a5b4 a5b4' (-: [: fkh 2&mfkh)&>&;: 'aaaaabbbb ababababa'
	assert. 1 = 2 10 mfksDF 'aaaaabbbb';'ababababa'

	assert. 'i3n2 i3a2' (-: [: fkh 2&mfkh)&>&;:  'significant capabilities'
	assert. 7 = 2 10 mfksDF  'significant';'capabilities' NB. task says 5; right answer is 7

	assert. 'L9T8 F9L8' (-: [: fkh 2&mfkh)&>&;: 'LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG'
	assert. 100 = 2 100 mfksDF 'LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV';'EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG'
	NB. task says 83; right answer is 100

	'pass'
)
   pass
```

'''Notes''': As of press time, there are significant discrepancies between the task description, its pseudocode, the test cases provided, and the two other existing implementations. See the [[Talk:Most frequent k chars distance#Prank_Page.3F|talk page]] for the assumptions made in this implementation to reconcile these discrepancies (in particular, in the scoring function).




## Java

Translation of the pseudo-code of the Wikipedia article [[wp:Most frequent k characters]] to [[wp:java]] implementation of three functions given in the definition section are given below with [[wp:JavaDoc]] comments:


```java
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;


public class SDF {

    /** Counting the number of occurrences of each character
     * @param character array
     * @return hashmap : Key = char, Value = num of occurrence
     */
    public static HashMap<Character, Integer> countElementOcurrences(char[] array) {

        HashMap<Character, Integer> countMap = new HashMap<Character, Integer>();

        for (char element : array) {
            Integer count = countMap.get(element);
            count = (count == null) ? 1 : count + 1;
            countMap.put(element, count);
        }

        return countMap;
    }

    /**
     * Sorts the counted numbers of characters (keys, values) by java Collection List
     * @param HashMap (with key as character, value as number of occurrences)
     * @return sorted HashMap
     */
    private static <K, V extends Comparable<? super V>>
            HashMap<K, V> descendingSortByValues(HashMap<K, V> map) {
	List<Map.Entry<K, V>> list = new ArrayList<Map.Entry<K, V>>(map.entrySet());
	// Defined Custom Comparator here
	Collections.sort(list, new Comparator<Map.Entry<K, V>>() {
		public int compare(Map.Entry<K, V> o1, Map.Entry<K, V> o2) {
		    return o2.getValue().compareTo(o1.getValue());
		}
	    });

	// Here I am copying the sorted list in HashMap
	// using LinkedHashMap to preserve the insertion order
	HashMap<K, V> sortedHashMap = new LinkedHashMap<K, V>();
	for (Map.Entry<K, V> entry : list) {
	    sortedHashMap.put(entry.getKey(), entry.getValue());
	}
	return sortedHashMap;
    }
    /**
     * get most frequent k characters
     * @param array of characters
     * @param limit of k
     * @return hashed String
     */
    public static String mostOcurrencesElement(char[] array, int k) {
        HashMap<Character, Integer> countMap = countElementOcurrences(array);
        System.out.println(countMap);
        Map<Character, Integer> map = descendingSortByValues(countMap);
        System.out.println(map);
        int i = 0;
        String output = "";
        for (Map.Entry<Character, Integer> pairs : map.entrySet()) {
	    if (i++ >= k)
		break;
            output += "" + pairs.getKey() + pairs.getValue();
        }
        return output;
    }
    /**
     * Calculates the similarity between two input strings
     * @param input string 1
     * @param input string 2
     * @param maximum possible limit value
     * @return distance as integer
     */
    public static int getDiff(String str1, String str2, int limit) {
        int similarity = 0;
	int k = 0;
	for (int i = 0; i < str1.length() ; i = k) {
	    k ++;
	    if (Character.isLetter(str1.charAt(i))) {
		int pos = str2.indexOf(str1.charAt(i));

		if (pos >= 0) {
		    String digitStr1 = "";
		    while ( k < str1.length() && !Character.isLetter(str1.charAt(k))) {
			digitStr1 += str1.charAt(k);
			k++;
		    }

		    int k2 = pos+1;
		    String digitStr2 = "";
		    while (k2 < str2.length() && !Character.isLetter(str2.charAt(k2)) ) {
			digitStr2 += str2.charAt(k2);
			k2++;
		    }

		    similarity += Integer.parseInt(digitStr2)
			+ Integer.parseInt(digitStr1);

		}
	    }
	}
	return Math.abs(limit - similarity);
    }
    /**
     * Wrapper function
     * @param input string 1
     * @param input string 2
     * @param maximum possible limit value
     * @return distance as integer
     */
    public static int SDFfunc(String str1, String str2, int limit) {
        return getDiff(mostOcurrencesElement(str1.toCharArray(), 2), mostOcurrencesElement(str2.toCharArray(), 2), limit);
    }

    public static void main(String[] args) {
        String input1 = "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV";
        String input2 = "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG";
        System.out.println(SDF.SDFfunc(input1,input2,100));

    }

}
```



## Kotlin

The code for the MostFreqKSimilarity function differs in this task from that in the associated Wikipedia article. Also the description is inconsistent with the code in both cases.

What I've decided to assume is that the frequency for commonly occurring characters must be the same in both strings for it to be added to the 'similarity' variable which seems to be the implication of both descriptions. This gives the same results as the Wikipedia article for all except the last example where it gives 100 rather than 83.

It's also evident that you can't just add the frequency of each character to the output string of the MostFreqKHashing function and then expect to be able to parse it afterwards. This is because, in the general case, any printing characters (including digits) could occur in the input string and the frequency could be more than 9. I've therefore encoded the frequency as the character with the same unicode value rather than the frequency itself.

I have no idea what NULL0 is supposed to mean so I've ignored that.

```scala
// version 1.1.51

fun mostFreqKHashing(input: String, k: Int): String =
    input.groupBy { it }.map { Pair(it.key, it.value.size) }
                        .sortedByDescending { it.second } // preserves original order when equal
                        .take(k)
                        .fold("") { acc, v -> acc + "${v.first}${v.second.toChar()}" }

fun mostFreqKSimilarity(input1: String, input2: String): Int {
    var similarity = 0
    for (i in 0 until input1.length step 2) {
        for (j in 0 until input2.length step 2) {
            if (input1[i] == input2[j]) {
                val freq1 = input1[i + 1].toInt()
                val freq2 = input2[j + 1].toInt()
                if (freq1 != freq2) continue  // assuming here that frequencies need to match
                similarity += freq1
            }
        }
    }
    return similarity
}

fun mostFreqKSDF(input1: String, input2: String, k: Int, maxDistance: Int) {
    println("input1 : $input1")
    println("input2 : $input2")
    val s1 = mostFreqKHashing(input1, k)
    val s2 = mostFreqKHashing(input2, k)
    print("mfkh(input1, $k) = ")
    for ((i, c) in s1.withIndex()) print(if (i % 2 == 0) c.toString() else c.toInt().toString())
    print("\nmfkh(input2, $k) = ")
    for ((i, c) in s2.withIndex()) print(if (i % 2 == 0) c.toString() else c.toInt().toString())
    val result = maxDistance - mostFreqKSimilarity(s1, s2)
    println("\nSDF(input1, input2, $k, $maxDistance) = $result\n")
}

fun main(args: Array<String>) {
    val pairs = listOf(
        Pair("research", "seeking"),
        Pair("night", "nacht"),
        Pair("my", "a"),
        Pair("research", "research"),
        Pair("aaaaabbbb", "ababababa"),
        Pair("significant", "capabilities")
    )
    for (pair in pairs) mostFreqKSDF(pair.first, pair.second, 2, 10)

    var s1 = "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV"
    var s2 = "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG"
    mostFreqKSDF(s1, s2, 2, 100)
    s1 = "abracadabra12121212121abracadabra12121212121"
    s2 = s1.reversed()
    mostFreqKSDF(s1, s2, 2, 100)
}
```


```txt

input1 : research
input2 : seeking
mfkh(input1, 2) = r2e2
mfkh(input2, 2) = e2s1
SDF(input1, input2, 2, 10) = 8

input1 : night
input2 : nacht
mfkh(input1, 2) = n1i1
mfkh(input2, 2) = n1a1
SDF(input1, input2, 2, 10) = 9

input1 : my
input2 : a
mfkh(input1, 2) = m1y1
mfkh(input2, 2) = a1
SDF(input1, input2, 2, 10) = 10

input1 : research
input2 : research
mfkh(input1, 2) = r2e2
mfkh(input2, 2) = r2e2
SDF(input1, input2, 2, 10) = 6

input1 : aaaaabbbb
input2 : ababababa
mfkh(input1, 2) = a5b4
mfkh(input2, 2) = a5b4
SDF(input1, input2, 2, 10) = 1

input1 : significant
input2 : capabilities
mfkh(input1, 2) = i3n2
mfkh(input2, 2) = i3a2
SDF(input1, input2, 2, 10) = 7

input1 : LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV
input2 : EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG
mfkh(input1, 2) = L9T8
mfkh(input2, 2) = F9L8
SDF(input1, input2, 2, 100) = 100

input1 : abracadabra12121212121abracadabra12121212121
input2 : 12121212121arbadacarba12121212121arbadacarba
mfkh(input1, 2) = 112a10
mfkh(input2, 2) = 112210
SDF(input1, input2, 2, 100) = 88

```



## Perl


```Perl
#!/usr/bin/perl
use strict ;
use warnings ;

sub mostFreqHashing {
   my $inputstring = shift ;
   my $howmany = shift ;
   my $outputstring ;
   my %letterfrequencies = findFrequencies ( $inputstring ) ;
   my @orderedChars = sort { $letterfrequencies{$b} <=> $letterfrequencies{$a} ||
      index( $inputstring , $a ) <=> index ( $inputstring , $b ) } keys %letterfrequencies ;
   for my $i ( 0..$howmany - 1 ) {
      $outputstring .= ( $orderedChars[ $i ] . $letterfrequencies{$orderedChars[ $i ]} ) ;
   }
   return $outputstring ;
}

sub findFrequencies {
   my $input = shift ;
   my %letterfrequencies ;
   for my $i ( 0..length( $input ) - 1 ) {
      $letterfrequencies{substr( $input , $i , 1 ) }++ ;
   }
   return %letterfrequencies ;
}

sub mostFreqKSimilarity {
   my $first = shift ;
   my $second = shift ;
   my $similarity = 0 ;
   my %frequencies_first = findFrequencies( $first ) ;
   my %frequencies_second = findFrequencies( $second ) ;
   foreach my $letter ( keys %frequencies_first ) {
      if ( exists ( $frequencies_second{$letter} ) ) {
	 $similarity += ( $frequencies_second{$letter} + $frequencies_first{$letter} ) ;
      }
   }
   return $similarity ;
}

sub mostFreqKSDF {
   (my $input1 , my $input2 , my $k , my $maxdistance ) = @_ ;
   return $maxdistance - mostFreqKSimilarity( mostFreqHashing( $input1 , $k) ,
	 mostFreqHashing( $input2 , $k) ) ;
}

my $firststring = "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV" ;
my $secondstring = "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG" ;
print "MostFreqKHashing ( " . '$firststring , 2)' . " is " . mostFreqHashing( $firststring , 2 ) . "\n" ;
print "MostFreqKHashing ( " . '$secondstring , 2)' . " is " . mostFreqHashing( $secondstring , 2 ) . "\n" ;

```

```txt
MostFreqKHashing ( $firststring , 2) is L9T8
MostFreqKHashing ( $secondstring , 2) is F9L8
```



## Perl 6

My initial impressions of this task are registered on the discussion page under "Prank Page?".

The "most frequent k characters" hashing function is straightforward enough to implement. The distance function is incomprehensible though. The description doesn't match the pseudo-code and neither of them match the examples. I'm not going to bother trying to figure it out unless there is some possible value.

Maybe I am too hasty though. Lets give it a try. Implement a MFKC routine and run an assortment of words through it to get a feel for how it hashes different words.


```perl6
# Fairly straightforward implementation, actually returns a list of pairs
# which can be joined to make a string or manipulated further.

sub mfkh ($string, \K = 2) {
    my %h;
    $string.comb.kv.map: { %h{$^v}[1] //= $^k; %h{$^v}[0]++ };
    %h.sort( { -$_.value[0], $_.value[1] } ).head(K).map( { $_.key => $_.value[0] } );
}

# lets try running 150 or so words from unixdic.txt through it to see
# how many unique hash values it comes up with.

my @test-words = <
    aminobenzoic arginine asinine biennial biennium brigantine brinkmanship
    britannic chinquapin clinging corinthian declination dickinson dimension
    dinnertime dionysian diophantine dominican financial financier finessing
    fingernail fingerprint finnish giovanni hopkinsian inaction inalienable
    inanimate incaution incendiary incentive inception incident incidental
    incinerate incline inclusion incommunicable incompletion inconceivable
    inconclusive incongruity inconsiderable inconsiderate inconspicuous
    incontrovertible inconvertible incurring incursion indefinable indemnify
    indemnity indeterminacy indian indiana indicant indifferent indigene
    indigenous indigent indispensable indochina indochinese indoctrinate
    indonesia inequivalent inexplainable infantile inferential inferring
    infestation inflammation inflationary influential information infringe
    infusion ingenious ingenuity ingestion ingredient inhabitant inhalation
    inharmonious inheritance inholding inhomogeneity inkling inoffensive
    inordinate inorganic inputting inseminate insensible insincere insinuate
    insistent insomnia insomniac insouciant installation instinct instinctual
    insubordinate insubstantial insulin insurrection intangible intelligent
    intensify intensive interception interruption intestinal intestine
    intoxicant introduction introversion intrusion invariant invasion inventive
    inversion involution justinian kidnapping kingpin lineprinter liniment
    livingston mainline mcginnis minion minneapolis minnie pigmentation
    pincushion pinion quinine quintessential resignation ruination seminarian
    triennial wilkinson wilmington wilsonian wineskin winnie winnipeg
>;

say @test-words.map( { join '', mfkh($_)».kv } ).Bag;
```

```txt
Bag(i2n2(151))
```

One... Nope, I was right, it ''is'' pretty much worthless.


## Phix

```Phix
function mostFreqKHashing(string input, integer k)
    string cfs = ""
    sequence cfsnx = {}
    for i=1 to length(input) do
        integer r = input[i],
                ix = find(r,cfs)
        if ix>0 then
            cfsnx[ix][1] -= 1
        else
            cfs &= r
            cfsnx = append(cfsnx,{-1,length(cfs)})
        end if
    end for
    cfsnx = sort(cfsnx) -- (aside: the idx forces stable sort)
    sequence acc := {}
    for i=1 to min(length(cfs),k) do
        integer {n,ix} = cfsnx[i]
        acc &= {cfs[ix], -n}
    end for
    return acc
end function

function mostFreqKSimilarity(sequence input1, input2)
    integer similarity := 0
    for i=1 to length(input1) by 2 do
        for j=1 to length(input2) by 2 do
            if input1[i] == input2[j] then
                integer freq1 = input1[i+1],
                        freq2 = input2[j+1]
                if freq1=freq2 then
                    similarity += freq1
                end if
            end if
        end for
    end for
    return similarity
end function

function flat(sequence s)
    string res = ""
    for i=1 to length(s) by 2 do
        res &= sprintf("%c%d",s[i..i+1])
    end for
    return res
end function

procedure mostFreqKSDF(string input1, input2, integer k, maxDistance)
    printf(1,"input1 : %s\n", {input1})
    printf(1,"input2 : %s\n", {input2})
    sequence s1 := mostFreqKHashing(input1, k),
             s2 := mostFreqKHashing(input2, k)
    printf(1,"mfkh(input1, %d) = %s\n", {k,flat(s1)})
    printf(1,"mfkh(input2, %d) = %s\n", {k,flat(s2)})
    integer result := maxDistance - mostFreqKSimilarity(s1, s2)
    printf(1,"SDF(input1, input2, %d, %d) = %d\n\n", {k, maxDistance, result})
end procedure

constant tests = {{"research", "seeking"},
                  {"night", "nacht"},
                  {"my", "a"},
                  {"research", "research"},
                  {"aaaaabbbb", "ababababa"},
                  {"significant", "capabilities"}}
for i=1 to length(tests) do
    string {t1,t2} = tests[i]
    mostFreqKSDF(t1, t2, 2, 10)
end for

string s1 := "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV",
       s2 := "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG"
mostFreqKSDF(s1, s2, 2, 100)
s1 = "abracadabra12121212121abracadabra12121212121"
s2 = reverse(s1)
mostFreqKSDF(s1, s2, 2, 100)
```

Output matches Go and Kotlin.


## Python

'''unoptimized and limited'''

```python
import collections
def MostFreqKHashing(inputString, K):
    occuDict = collections.defaultdict(int)
    for c in inputString:
        occuDict[c] += 1
    occuList = sorted(occuDict.items(), key = lambda x: x[1], reverse = True)
    outputStr = ''.join(c + str(cnt) for c, cnt in occuList[:K])
    return outputStr

#If number of occurrence of the character is not more than 9
def MostFreqKSimilarity(inputStr1, inputStr2):
    similarity = 0
    for i in range(0, len(inputStr1), 2):
        c = inputStr1[i]
        cnt1 = int(inputStr1[i + 1])
        for j in range(0, len(inputStr2), 2):
            if inputStr2[j] == c:
                cnt2 = int(inputStr2[j + 1])
                similarity += cnt1 + cnt2
                break
    return similarity

def MostFreqKSDF(inputStr1, inputStr2, K, maxDistance):
    return maxDistance - MostFreqKSimilarity(MostFreqKHashing(inputStr1,K), MostFreqKHashing(inputStr2,K))
```


'''optimized'''

A version that replaces the intermediate string with OrderedDict to reduce the time complexity of lookup operation:

```python
import collections
def MostFreqKHashing(inputString, K):
    occuDict = collections.defaultdict(int)
    for c in inputString:
        occuDict[c] += 1
    occuList = sorted(occuDict.items(), key = lambda x: x[1], reverse = True)
    outputDict = collections.OrderedDict(occuList[:K])
    #Return OrdredDict instead of string for faster lookup.
    return outputDict

def MostFreqKSimilarity(inputStr1, inputStr2):
    similarity = 0
    for c, cnt1 in inputStr1.items():
        #Reduce the time complexity of lookup operation to about O(1).
        if c in inputStr2:
            cnt2 = inputStr2[c]
            similarity += cnt1 + cnt2
    return similarity

def MostFreqKSDF(inputStr1, inputStr2, K, maxDistance):
    return maxDistance - MostFreqKSimilarity(MostFreqKHashing(inputStr1,K), MostFreqKHashing(inputStr2,K))
```

Test:

```python
str1 = "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV"
str2 = "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG"
K = 2
maxDistance = 100
dict1 = MostFreqKHashing(str1, 2)
print("%s:"%dict1)
print(''.join(c + str(cnt) for c, cnt in dict1.items()))
dict2 = MostFreqKHashing(str2, 2)
print("%s:"%dict2)
print(''.join(c + str(cnt) for c, cnt in dict2.items()))
print(MostFreqKSDF(str1, str2, K, maxDistance))
```

```txt

OrderedDict([('L', 9), ('T', 8)]):
L9T8
OrderedDict([('F', 9), ('L', 8)]):
F9L8
83

```



## Racket



```Racket
#lang racket

(define (MostFreqKHashing inputString K)
  (define t (make-hash))
  (for ([c (in-string inputString)] [i (in-naturals)])
    (define b (cdr (hash-ref! t c (λ() (cons i (box 0))))))
    (set-box! b (add1 (unbox b))))
  (define l (for/list ([(k v) (in-hash t)]) (list (car v) k (unbox (cdr v)))))
  (map cdr (take (sort (sort l < #:key car) > #:key caddr) K)))

(define (MostFreqKSimilarity inputStr1 inputStr2) ; not strings in this impl.
  (for*/sum ([c1 (in-list inputStr1)] [c2 (in-value (assq (car c1) inputStr2))]
             #:when c2)
    (+ (cadr c1) (cadr c2))))

(define (MostFreqKSDF inputStr1 inputStr2 K maxDistance)
  (- maxDistance (MostFreqKSimilarity (MostFreqKHashing inputStr1 K)
                                      (MostFreqKHashing inputStr2 K))))

(MostFreqKSDF
 "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV"
 "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG"
 2 100)
;; => 83

;; (Should add more tests, but it looks like there's a bunch of mistakes
;; in the given tests...)

```



## Ring


```ring

# Project : Most frequent k chars distance

load "stdlib.ring"
str1 = "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV"
str2 = "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG"
see "Str1 = " + str1 + nl
see "Str2 = " + str2 + nl

MostFreqKHashing(str1,"str1")
MostFreqKHashing(str2,"str2")

func MostFreqKHashing(str3,strp)
        chr = newlist(26,2)
        for n = 1 to 26
             str = char(n+64)
             cstr = count(str3,str)
             chr[n][1] = str
             chr[n][2] = cstr
        next
        chr = sortsecond(chr)
        chr = reverse(chr)
        see "MostFreqKHashing(" + strp + ",2) = "
        see chr[1][1] + chr[1][2] + chr[2][1] + chr[2][2] + nl

func count(cString,dString)
       sum = 0
       while substr(cString,dString) > 0
               sum++
               cString = substr(cString,substr(cString,dString)+len(string(sum)))
       end
       return sum

func sortsecond(alist)
        aList = sort(alist,2)
        for n=1 to len(alist)-1
             for m=n to len(aList)-1
                   if alist[m+1][2] = alist[m][2] and alist[m+1][1] < alist[m][1]
                      temp = alist[m+1]
                      alist[m+1] = alist[m]
                      alist[m] = temp ok
             next
       next
       return aList

```

Output:

```txt

Str1 = LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV
Str2 = EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG
MostFreqKHashing(str1,2) = L9T8
MostFreqKHashing(str2,2) = F9L8

```



## Sidef


```ruby
func _MostFreqKHashing(string, k) {

    var seen = Hash()
    var chars = string.chars
    var freq = chars.freq
    var schars = freq.keys.sort_by {|c| -freq{c} }

    var mfkh = []
    for i in ^k {
        chars.each { |c|
            seen{c} && next
            if (freq{c} == freq{schars[i]}) {
                seen{c} = true
                mfkh << Hash(c => c, f => freq{c})
                break
            }
        }
    }

    mfkh << (k-seen.len -> of { Hash(c => :NULL, f => 0) }...)
    mfkh
}

func MostFreqKSDF(a, b, k, d) {

    var mfkh_a = _MostFreqKHashing(a, k);
    var mfkh_b = _MostFreqKHashing(b, k);

    d - gather {
        mfkh_a.each { |s|
            s{:c} == :NULL && next
            mfkh_b.each { |t|
                s{:c} == t{:c} &&
                    take(s{:f} + (s{:f} == t{:f} ? 0 : t{:f}))
            }
        }
    }.sum
}

func MostFreqKHashing(string, k) {
    gather {
        _MostFreqKHashing(string, k).each { |h|
            take("%s%d" % (h{:c}, h{:f}))
        }
    }.join
}


var str1 = "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV"
var str2 = "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG"

say "str1 = #{str1.dump}"
say "str2 = #{str2.dump}"

say ''

say("MostFreqKHashing(str1, 2) = ", MostFreqKHashing(str1, 2))
say("MostFreqKHashing(str2, 2) = ", MostFreqKHashing(str2, 2))
say("MostFreqKSDF(str1, str2, 2, 100) = ", MostFreqKSDF(str1, str2, 2, 100))

say ''

var arr = [
    %w(night nacht),
    %w(my a),
    %w(research research),
    %w(aaaaabbbb ababababa),
    %w(significant capabilities),
]

var k = 2
var limit = 10

for s,t in arr {
    "mfkh(%s, %s, #{k}) = [%s, %s] (SDF: %d)\n".printf(
        s.dump, t.dump,
        MostFreqKHashing(s, k).dump,
        MostFreqKHashing(t, k).dump,
        MostFreqKSDF(s, t, k, limit),
    )
}
```

```txt

str1 = "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV"
str2 = "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG"

MostFreqKHashing(str1, 2) = L9T8
MostFreqKHashing(str2, 2) = F9L8
MostFreqKSDF(str1, str2, 2, 100) = 83

mfkh("night", "nacht", 2) = ["n1i1", "n1a1"] (SDF: 9)
mfkh("my", "a", 2) = ["m1y1", "a1NULL0"] (SDF: 10)
mfkh("research", "research", 2) = ["r2e2", "r2e2"] (SDF: 6)
mfkh("aaaaabbbb", "ababababa", 2) = ["a5b4", "a5b4"] (SDF: 1)
mfkh("significant", "capabilities", 2) = ["i3n2", "i3a2"] (SDF: 7)

```



## Tcl

```tcl
package require Tcl 8.6

proc MostFreqKHashing {inputString k} {
    foreach ch [split $inputString ""] {dict incr count $ch}
    join [lrange [lsort -stride 2 -index 1 -integer -decreasing $count] 0 [expr {$k*2-1}]] ""
}
proc MostFreqKSimilarity {hashStr1 hashStr2} {
    while {$hashStr2 ne ""} {
	regexp {^(.)(\d+)(.*)$} $hashStr2 -> ch n hashStr2
	set lookup($ch) $n
    }
    set similarity 0
    while {$hashStr1 ne ""} {
	regexp {^(.)(\d+)(.*)$} $hashStr1 -> ch n hashStr1
	if {[info exist lookup($ch)]} {
	    incr similarity $n
	    incr similarity $lookup($ch)
	}
    }
    return $similarity
}
proc MostFreqKSDF {inputStr1 inputStr2 k limit} {
    set h1 [MostFreqKHashing $inputStr1 $k]
    set h2 [MostFreqKHashing $inputStr2 $k]
    expr {$limit - [MostFreqKSimilarity $h1 $h2]}
}
```

Demonstrating:

```tcl
set str1 "LCLYTHIGRNIYYGSYLYSETWNTGIMLLLITMATAFMGYVLPWGQMSFWGATVITNLFSAIPYIGTNLV"
set str2 "EWIWGGFSVDKATLNRFFAFHFILPFTMVALAGVHLTFLHETGSNNPLGLTSDSDKIPFHPYYTIKDFLG"
puts [MostFreqKHashing $str1 2]
puts [MostFreqKHashing $str2 2]
puts [MostFreqKSDF $str1 $str2 2 100]
```

```txt

L9T8
F9L8
83

```

;A more efficient metric calculator
This version is appreciably more efficient because it does not compute the intermediate string representation “hash”, instead working directly on the intermediate dictionaries and lists:

```tcl
proc MostFreqKSDF {inputStr1 inputStr2 k limit} {
    set c1 [set c2 {}]
    foreach ch [split $inputStr1 ""] {dict incr c1 $ch}
    foreach ch [split $inputStr2 ""] {dict incr c2 $ch}
    set c2 [lrange [lsort -stride 2 -index 1 -integer -decreasing $c2[set c2 {}]] 0 [expr {$k*2-1}]]
    set s 0
    foreach {ch n} [lrange [lsort -stride 2 -index 1 -integer -decreasing $c1[set c1 {}]] 0 [expr {$k*2-1}]] {
	if {[dict exists $c2 $ch]} {
	    incr s [expr {$n + [dict get $c2 $ch]}]
	}
    }
    return [expr {$limit - $s}]
}
```

It computes the identical value on the identical inputs.

## References
