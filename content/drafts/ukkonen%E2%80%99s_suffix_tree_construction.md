+++
title = "Ukkonen’s Suffix Tree Construction"
description = ""
date = 2019-04-25T00:53:10Z
aliases = []
[extra]
id = 18764
[taxonomies]
categories = []
tags = []
+++

[[Category:String algorithms]]
{{draft task}}
Suffix Trees are very useful in numerous string processing and computational biology problems.

The task is to create a function which implements Ukkonen’s algorithm to create a useful Suffix Tree as described:

 [http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-1/ Part 1]
 [http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-2/ Part 2]
 [http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-3/ Part 3]
 [http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-4/ Part 4]
 [http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-5/ Part 5]
 [http://www.geeksforgeeks.org/ukkonens-suffix-tree-construction-part-6/ Part 6]

Using [[Arithmetic-geometric mean/Calculate Pi]] generate the first 1000, 10000, and 100000 decimal places of pi. Using your implementation with an alphabet of 0 through 9 (plus $ say to make the tree explicit) find the longest repeated string in each list. Time your results and demonstrate that your implementation is linear (i.e. that 10000 takes approx. 10 times as long as 1000). You may vary the size of the lists of decimal places of pi to give reasonable answers.


## Go

This is a translation of the C code [https://www.geeksforgeeks.org/suffix-tree-application-3-longest-repeated-substring/ here] which is an extended form of the code in Part 6 of the task description for finding the longest repeated substring of a given string. In the interests of brevity, the extensive comments in the C version have been largely omitted. The C code doesn't compile as it stands but I have added a fix in the Talk Page.

For convenience I have included the code from the [[Arithmetic-geometric_mean/Calculate_Pi#Go]] task in the same package.

It takes around 25 seconds on my machine (Celeron @1.6GHz) to calculate the first 100,000 (or so) decimal places of Pi. Having done that, the timings for extracting the longest repeated sequence of digits are quick and fairly linear as expected.

As the task doesn't say whether overlapping sequences are to be counted, I've assumed that they are as this is what the algorithm naturally produces.

```go
package main

import (
    "fmt"
    "math/big"
    "time"
)

var maxChar = 128

type Node struct {
    children    []*Node
    suffixLink  *Node
    start       int
    end         *int
    suffixIndex int
}

var (
    text                 string
    root                 *Node
    lastNewNode          *Node
    activeNode           *Node
    activeEdge           = -1
    activeLength         = 0
    remainingSuffixCount = 0
    leafEnd              = -1
    rootEnd              *int
    splitEnd             *int
    size                 = -1
)

func newNode(start int, end *int) *Node {
    node := new(Node)
    node.children = make([]*Node, maxChar)
    node.suffixLink = root
    node.start = start
    node.end = end
    node.suffixIndex = -1
    return node
}

func edgeLength(n *Node) int {
    if n == root {
        return 0
    }
    return *(n.end) - n.start + 1
}

func walkDown(currNode *Node) bool {
    if activeLength >= edgeLength(currNode) {
        activeEdge += edgeLength(currNode)
        activeLength -= edgeLength(currNode)
        activeNode = currNode
        return true
    }
    return false
}

func extendSuffixTree(pos int) {
    leafEnd = pos
    remainingSuffixCount++
    lastNewNode = nil
    for remainingSuffixCount > 0 {
        if activeLength == 0 {
            activeEdge = pos
        }
        if activeNode.children[text[activeEdge]] == nil {
            activeNode.children[text[activeEdge]] = newNode(pos, &leafEnd)
            if lastNewNode != nil {
                lastNewNode.suffixLink = activeNode
                lastNewNode = nil
            }
        } else {
            next := activeNode.children[text[activeEdge]]
            if walkDown(next) {
                continue
            }
            if text[next.start+activeLength] == text[pos] {
                if lastNewNode != nil && activeNode != root {
                    lastNewNode.suffixLink = activeNode
                    lastNewNode = nil
                }
                activeLength++
                break
            }
            temp := next.start + activeLength - 1
            splitEnd = &temp
            split := newNode(next.start, splitEnd)
            activeNode.children[text[activeEdge]] = split
            split.children[text[pos]] = newNode(pos, &leafEnd)
            next.start += activeLength
            split.children[text[next.start]] = next
            if lastNewNode != nil {
                lastNewNode.suffixLink = split
            }
            lastNewNode = split
        }
        remainingSuffixCount--
        if activeNode == root && activeLength > 0 {
            activeLength--
            activeEdge = pos - remainingSuffixCount + 1
        } else if activeNode != root {
            activeNode = activeNode.suffixLink
        }
    }
}

func setSuffixIndexByDFS(n *Node, labelHeight int) {
    if n == nil {
        return
    }
    if n.start != -1 {
        // Uncomment line below to print suffix tree
        // fmt.Print(text[n.start: *(n.end) +1])
    }
    leaf := 1
    for i := 0; i < maxChar; i++ {
        if n.children[i] != nil {
            // Uncomment the 3 lines below to print suffix index
            //if leaf == 1 && n.start != -1 {
            //    fmt.Printf(" [%d]\n", n.suffixIndex)
            //}
            leaf = 0
            setSuffixIndexByDFS(n.children[i], labelHeight+edgeLength(n.children[i]))
        }
    }
    if leaf == 1 {
        n.suffixIndex = size - labelHeight
        // Uncomment line below to print suffix index
        //fmt.Printf(" [%d]\n", n.suffixIndex)
    }
}

func buildSuffixTree() {
    size = len(text)
    temp := -1
    rootEnd = &temp
    root = newNode(-1, rootEnd)
    activeNode = root
    for i := 0; i < size; i++ {
        extendSuffixTree(i)
    }
    labelHeight := 0
    setSuffixIndexByDFS(root, labelHeight)
}

func doTraversal(n *Node, labelHeight int, maxHeight, substringStartIndex *int) {
    if n == nil {
        return
    }
    if n.suffixIndex == -1 {
        for i := 0; i < maxChar; i++ {
            if n.children[i] != nil {
                doTraversal(n.children[i], labelHeight+edgeLength(n.children[i]),
                    maxHeight, substringStartIndex)
            }
        }
    } else if n.suffixIndex > -1 && (*maxHeight < labelHeight-edgeLength(n)) {
        *maxHeight = labelHeight - edgeLength(n)
        *substringStartIndex = n.suffixIndex
    }
}

func getLongestRepeatedSubstring(s string) {
    maxHeight := 0
    substringStartIndex := 0
    doTraversal(root, 0, &maxHeight, &substringStartIndex)
    // Uncomment line below to print maxHeight and substringStartIndex
    // fmt.Printf("maxHeight %d, substringStartIndex %d\n", maxHeight, substringStartIndex)
    if s == "" {
        fmt.Printf("  %s is: ", text)
    } else {
        fmt.Printf("  %s is: ", s)
    }
    k := 0
    for ; k < maxHeight; k++ {
        fmt.Printf("%c", text[k+substringStartIndex])
    }
    if k == 0 {
        fmt.Print("No repeated substring")
    }
    fmt.Println()
}

func calculatePi() *big.Float {
    one := big.NewFloat(1)
    two := big.NewFloat(2)
    four := big.NewFloat(4)
    prec := uint(325 * 1024) // enough to calculate Pi to 100,182 decimal digits

    a := big.NewFloat(1).SetPrec(prec)
    g := new(big.Float).SetPrec(prec)

    // temporary variables
    t := new(big.Float).SetPrec(prec)
    u := new(big.Float).SetPrec(prec)

    g.Quo(a, t.Sqrt(two))
    sum := new(big.Float)
    pow := big.NewFloat(2)

    for a.Cmp(g) != 0 {
        t.Add(a, g)
        t.Quo(t, two)
        g.Sqrt(u.Mul(a, g))
        a.Set(t)
        pow.Mul(pow, two)
        t.Sub(t.Mul(a, a), u.Mul(g, g))
        sum.Add(sum, t.Mul(t, pow))
    }

    t.Mul(a, a)
    t.Mul(t, four)
    pi := t.Quo(t, u.Sub(one, sum))
    return pi
}

func main() {
    tests := []string{
        "GEEKSFORGEEKS$",
        "AAAAAAAAAA$",
        "ABCDEFG$",
        "ABABABA$",
        "ATCGATCGA$",
        "banana$",
        "abcpqrabpqpq$",
        "pqrpqpqabab$",
    }
    fmt.Println("Longest Repeated Substring in:\n")
    for _, test := range tests {
        text = test
        buildSuffixTree()
        getLongestRepeatedSubstring("")
    }
    fmt.Println()

    pi := calculatePi()
    piStr := fmt.Sprintf("%v", pi)
    piStr = piStr[2:] // remove initial 3.
    numbers := []int{1e3, 1e4, 1e5}
    maxChar = 58
    for _, number := range numbers {
        start := time.Now()
        text = piStr[0:number] + "$"
        buildSuffixTree()
        getLongestRepeatedSubstring(fmt.Sprintf("first %d d.p. of Pi", number))
        elapsed := time.Now().Sub(start)
        fmt.Printf("  (this took %s)\n\n", elapsed)
    }
}
```


{{out}}
Sample run:

```txt

Longest Repeated Substring in:

  GEEKSFORGEEKS$ is: GEEKS
  AAAAAAAAAA$ is: AAAAAAAAA
  ABCDEFG$ is: No repeated substring
  ABABABA$ is: ABABA
  ATCGATCGA$ is: ATCGA
  banana$ is: ana
  abcpqrabpqpq$ is: ab
  pqrpqpqabab$ is: ab

  first 1000 d.p. of Pi is: 23846
  (this took 7.728858ms)

  first 10000 d.p. of Pi is: 7111369
  (this took 57.524478ms)

  first 100000 d.p. of Pi is: 041021944
  (this took 599.770281ms)

```

