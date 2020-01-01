+++
title = "Eertree"
description = ""
date = 2019-07-19T23:40:17Z
aliases = []
[extra]
id = 21218
[taxonomies]
categories = []
tags = []
+++

[[Category:String algorithms]]
[[Category:Palindromes]]
{{task}}

An '''eertree''' is a data structure designed for efficient processing of certain palindrome tasks, for instance counting the number of sub-palindromes in an input string.

The data structure has commonalities to both ''tries'' and ''suffix trees''.
  See links below.


;Task:
Construct an eertree for the string "eertree", then output all sub-palindromes by traversing the tree.


;See also:
*   Wikipedia entry:   [https://en.wikipedia.org/wiki/Trie trie].
*   Wikipedia entry:   [https://en.wikipedia.org/wiki/Suffix_tree suffix tree]
*   [https://arxiv.org/abs/1506.04862 Cornell University Library, Computer Science, Data Structures and Algorithms ───► EERTREE: An Efficient Data Structure for Processing Palindromes in Strings].





## C++

{{trans|D}}

```cpp
#include <iostream>
#include <functional>
#include <map>
#include <vector>

struct Node {
    int length;
    std::map<char, int> edges;
    int suffix;

    Node(int l) : length(l), suffix(0) {
        /* empty */
    }

    Node(int l, const std::map<char, int>& m, int s) : length(l), edges(m), suffix(s) {
        /* empty */
    }
};

constexpr int evenRoot = 0;
constexpr int oddRoot = 1;

std::vector<Node> eertree(const std::string& s) {
    std::vector<Node> tree = {
        Node(0, {}, oddRoot),
        Node(-1, {}, oddRoot)
    };
    int suffix = oddRoot;
    int n, k;

    for (size_t i = 0; i < s.length(); ++i) {
        char c = s[i];
        for (n = suffix; ; n = tree[n].suffix) {
            k = tree[n].length;
            int b = i - k - 1;
            if (b >= 0 && s[b] == c) {
                break;
            }
        }

        auto it = tree[n].edges.find(c);
        auto end = tree[n].edges.end();
        if (it != end) {
            suffix = it->second;
            continue;
        }
        suffix = tree.size();
        tree.push_back(Node(k + 2));
        tree[n].edges[c] = suffix;
        if (tree[suffix].length == 1) {
            tree[suffix].suffix = 0;
            continue;
        }
        while (true) {
            n = tree[n].suffix;
            int b = i - tree[n].length - 1;
            if (b >= 0 && s[b] == c) {
                break;
            }
        }
        tree[suffix].suffix = tree[n].edges[c];
    }

    return tree;
}

std::vector<std::string> subPalindromes(const std::vector<Node>& tree) {
    std::vector<std::string> s;

    std::function<void(int, std::string)> children;
    children = [&children, &tree, &s](int n, std::string p) {
        auto it = tree[n].edges.cbegin();
        auto end = tree[n].edges.cend();
        for (; it != end; it = std::next(it)) {
            auto c = it->first;
            auto m = it->second;

            std::string pl = c + p + c;
            s.push_back(pl);
            children(m, pl);
        }
    };
    children(0, "");

    auto it = tree[1].edges.cbegin();
    auto end = tree[1].edges.cend();
    for (; it != end; it = std::next(it)) {
        auto c = it->first;
        auto n = it->second;

        std::string ct(1, c);
        s.push_back(ct);

        children(n, ct);
    }

    return s;
}

int main() {
    using namespace std;

    auto tree = eertree("eertree");
    auto pal = subPalindromes(tree);

    auto it = pal.cbegin();
    auto end = pal.cend();

    cout << "[";
    if (it != end) {
        cout << it->c_str();
        it++;
    }
    while (it != end) {
        cout << ", " << it->c_str();
        it++;
    }
    cout << "]" << endl;

    return 0;
}
```

{{out}}

```txt
[ee, e, r, t, rtr, ertre, eertree]
```


## C#
{{trans|Java}}

```c#
using System;
using System.Collections.Generic;

namespace Eertree {
    class Node {
        public Node(int length) {
            this.Length = length;
            // empty or
            this.Edges = new Dictionary<char, int>();
        }

        public Node(int length, Dictionary<char, int> edges, int suffix) {
            this.Length = length;
            this.Edges = edges;
            this.Suffix = suffix;
        }

        public int Length { get; set; }
        public Dictionary<char, int> Edges { get; set; }
        public int Suffix { get; set; }
    }

    class Program {
        const int EVEN_ROOT = 0;
        const int ODD_ROOT = 1;

        static List<Node> Eertree(string s) {
            List<Node> tree = new List<Node> {
                //new Node(0, null, ODD_ROOT), or
                new Node(0, new Dictionary<char, int>(), ODD_ROOT),
                //new Node(-1, null, ODD_ROOT) or
                new Node(-1, new Dictionary<char, int>(), ODD_ROOT)
            };
            int suffix = ODD_ROOT;
            int n, k;
            for (int i = 0; i < s.Length; i++) {
                char c = s[i];
                for (n = suffix; ; n = tree[n].Suffix) {
                    k = tree[n].Length;
                    int b = i - k - 1;
                    if (b >= 0 && s[b] == c) {
                        break;
                    }
                }
                if (tree[n].Edges.ContainsKey(c)) {
                    suffix = tree[n].Edges[c];
                    continue;
                }
                suffix = tree.Count;
                tree.Add(new Node(k + 2));
                tree[n].Edges[c] = suffix;
                if (tree[suffix].Length == 1) {
                    tree[suffix].Suffix = 0;
                    continue;
                }
                while (true) {
                    n = tree[n].Suffix;
                    int b = i - tree[n].Length - 1;
                    if (b >= 0 && s[b] == c) {
                        break;
                    }
                }
                tree[suffix].Suffix = tree[n].Edges[c];
            }
            return tree;
        }

        static List<string> SubPalindromes(List<Node> tree) {
            List<string> s = new List<string>();
            SubPalindromes_children(0, "", tree, s);
            foreach (var c in tree[1].Edges.Keys) {
                int m = tree[1].Edges[c];
                string ct = c.ToString();
                s.Add(ct);
                SubPalindromes_children(m, ct, tree, s);
            }
            return s;
        }

        static void SubPalindromes_children(int n, string p, List<Node> tree, List<string> s) {
            foreach (var c in tree[n].Edges.Keys) {
                int m = tree[n].Edges[c];
                string p1 = c + p + c;
                s.Add(p1);
                SubPalindromes_children(m, p1, tree, s);
            }
        }

        static void Main(string[] args) {
            List<Node> tree = Eertree("eertree");
            List<string> result = SubPalindromes(tree);
            string listStr = string.Join(", ", result);
            Console.WriteLine("[{0}]", listStr);
        }
    }
}
```

{{out}}

```txt
[ee, e, r, t, rtr, ertre, eertree]
```



## D

{{trans|Go}}

```D
import std.array;
import std.stdio;

void main() {
    auto tree = eertree("eertree");
    writeln(subPalindromes(tree));
}

struct Node {
    int length;
    int[char] edges;
    int suffix;
}

const evenRoot = 0;
const oddRoot = 1;

Node[] eertree(string s) {
    Node[] tree = [
        Node(0, null, oddRoot),
        Node(-1, null, oddRoot),
    ];
    int suffix = oddRoot;
    int n, k;
    foreach (i, c; s) {
        for (n=suffix; ; n=tree[n].suffix) {
            k = tree[n].length;
            int b = i-k-1;
            if (b>=0 && s[b]==c) {
                break;
            }
        }
        if (c in tree[n].edges) {
            suffix = tree[n].edges[c];
            continue;
        }
        suffix = tree.length;
        tree ~= Node(k+2);
        tree[n].edges[c] = suffix;
        if (tree[suffix].length == 1) {
            tree[suffix].suffix = 0;
            continue;
        }
        while (true) {
            n = tree[n].suffix;
            int b = i-tree[n].length-1;
            if (b>=0 && s[b]==c) {
                break;
            }
        }
        tree[suffix].suffix = tree[n].edges[c];
    }
    return tree;
}

auto subPalindromes(Node[] tree) {
    auto s = appender!(string[]);
    void children(int n, string p) {
        foreach (c, n; tree[n].edges) {
            p = c ~ p ~ c;
            s ~= p;
            children(n, p);
        }
    }
    children(0, "");
    foreach (c, n; tree[1].edges) {
        string ct = [c].idup;
        s ~= ct;
        children(n, ct);
    }
    return s.data;
}
```

{{out}}

```txt
["ee", "e", "r", "t", "rtr", "ertre", "eertree"]
```



## Go


```go
package main

import "fmt"

func main() {
    tree := eertree([]byte("eertree"))
    fmt.Println(subPalindromes(tree))
}

type edges map[byte]int

type node struct {
    length int
    edges
    suffix int
}

const evenRoot = 0
const oddRoot = 1

func eertree(s []byte) []node {
    tree := []node{
        evenRoot: {length: 0, suffix: oddRoot, edges: edges{}},
        oddRoot:  {length: -1, suffix: oddRoot, edges: edges{}},
    }
    suffix := oddRoot
    var n, k int
    for i, c := range s {
        for n = suffix; ; n = tree[n].suffix {
            k = tree[n].length
            if b := i - k - 1; b >= 0 && s[b] == c {
                break
            }
        }
        if e, ok := tree[n].edges[c]; ok {
            suffix = e
            continue
        }
        suffix = len(tree)
        tree = append(tree, node{length: k + 2, edges: edges{}})
        tree[n].edges[c] = suffix
        if tree[suffix].length == 1 {
            tree[suffix].suffix = 0
            continue
        }
        for {
            n = tree[n].suffix
            if b := i - tree[n].length - 1; b >= 0 && s[b] == c {
                break
            }
        }
        tree[suffix].suffix = tree[n].edges[c]
    }
    return tree
}

func subPalindromes(tree []node) (s []string) {
    var children func(int, string)
    children = func(n int, p string) {
        for c, n := range tree[n].edges {
            c := string(c)
            p := c + p + c
            s = append(s, p)
            children(n, p)
        }
    }
    children(0, "")
    for c, n := range tree[1].edges {
        c := string(c)
        s = append(s, c)
        children(n, c)
    }
    return
}
```

{{out}}

```txt

[ee e r t rtr ertre eertree]

```



## Java

{{trans|D}}

```Java
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Eertree {
    public static void main(String[] args) {
        List<Node> tree = eertree("eertree");
        List<String> result = subPalindromes(tree);
        System.out.println(result);
    }

    private static class Node {
        int length;
        Map<Character, Integer> edges = new HashMap<>();
        int suffix;

        public Node(int length) {
            this.length = length;
        }

        public Node(int length, Map<Character, Integer> edges, int suffix) {
            this.length = length;
            this.edges = edges != null ? edges : new HashMap<>();
            this.suffix = suffix;
        }
    }

    private static final int EVEN_ROOT = 0;
    private static final int ODD_ROOT = 1;

    private static List<Node> eertree(String s) {
        List<Node> tree = new ArrayList<>();
        tree.add(new Node(0, null, ODD_ROOT));
        tree.add(new Node(-1, null, ODD_ROOT));
        int suffix = ODD_ROOT;
        int n, k;
        for (int i = 0; i < s.length(); ++i) {
            char c = s.charAt(i);
            for (n = suffix; ; n = tree.get(n).suffix) {
                k = tree.get(n).length;
                int b = i - k - 1;
                if (b >= 0 && s.charAt(b) == c) {
                    break;
                }
            }
            if (tree.get(n).edges.containsKey(c)) {
                suffix = tree.get(n).edges.get(c);
                continue;
            }
            suffix = tree.size();
            tree.add(new Node(k + 2));
            tree.get(n).edges.put(c, suffix);
            if (tree.get(suffix).length == 1) {
                tree.get(suffix).suffix = 0;
                continue;
            }
            while (true) {
                n = tree.get(n).suffix;
                int b = i - tree.get(n).length - 1;
                if (b >= 0 && s.charAt(b) == c) {
                    break;
                }
            }
            tree.get(suffix).suffix = tree.get(n).edges.get(c);
        }
        return tree;
    }

    private static List<String> subPalindromes(List<Node> tree) {
        List<String> s = new ArrayList<>();
        subPalindromes_children(0, "", tree, s);
        for (Map.Entry<Character, Integer> cm : tree.get(1).edges.entrySet()) {
            String ct = String.valueOf(cm.getKey());
            s.add(ct);
            subPalindromes_children(cm.getValue(), ct, tree, s);
        }
        return s;
    }

    // nested methods are a pain, even if lambdas make that possible for Java
    private static void subPalindromes_children(final int n, final String p, final List<Node> tree, List<String> s) {
        for (Map.Entry<Character, Integer> cm : tree.get(n).edges.entrySet()) {
            Character c = cm.getKey();
            Integer m = cm.getValue();
            String pl = c + p + c;
            s.add(pl);
            subPalindromes_children(m, pl, tree, s);
        }
    }
}
```

{{out}}

```txt
[ee, r, t, rtr, ertre, eertree, e]
```



## Julia

{{trans|Python}}

```julia
mutable struct Node
    edges::Dict{Char, Node}
    link::Union{Node, Missing}
    sz::Int
    Node() = new(Dict(), missing, 0)
end

sizednode(x) = (n = Node(); n.sz = x; n)

function eertree(str)
    nodes = Vector{Node}()
    oddroot = sizednode(-1)
    evenroot = sizednode(0)
    oddroot.link = evenroot
    evenroot.link = oddroot
    S = "0"
    maxsuffix = evenroot

    function maxsuffixpal(startnode,a::Char)
        # Traverse the suffix-palindromes of tree looking for equality with a
        u = startnode
        i = length(S)
        k = u.sz
        while u !== oddroot && S[i - k] != a
            if u === u.link
                throw("circular reference above oddroot")
            end
            u = u.link
            k = u.sz
        end
        u
    end

    function addchar(a::Char)
        Q = maxsuffixpal(maxsuffix, a)
        creatednode = !haskey(Q.edges, a)
        if creatednode
            P = sizednode(Q.sz + 2)
            push!(nodes, P)
            if P.sz == 1
                P.link = evenroot
            else
                P.link = maxsuffixpal(Q.link, a).edges[a]
            end
            Q.edges[a] = P            # adds edge (Q, P)
        end
        maxsuffix = Q.edges[a]        # P becomes the new maxsuffix
        S *= string(a)
        creatednode
    end

    function getsubpalindromes()
        result = Vector{String}()
        getsubpalindromes(oddroot, [oddroot], "", result)
        getsubpalindromes(evenroot, [evenroot], "", result)
        result
    end

    function getsubpalindromes(nd, nodestohere, charstohere, result)
        for (lnkname, nd2) in nd.edges
            getsubpalindromes(nd2, vcat(nodestohere, nd2), charstohere * lnkname, result)
        end
        if nd !== oddroot && nd !== evenroot
            assembled = reverse(charstohere) *
                (nodestohere[1] === evenroot ? charstohere : charstohere[2:end])
            push!(result, assembled)
        end
    end

    println("Results of processing string \"$str\":")
    for c in str
        addchar(c)
    end
    println("Number of sub-palindromes: ", length(nodes))
    println("Sub-palindromes: ", getsubpalindromes())
end

eertree("eertree")

```
 {{output}}
```txt

Results of processing string "eertree":
Number of sub-palindromes: 7
Sub-palindromes: ["e", "r", "eertree", "ertre", "rtr", "t", "ee"]

```



## Kotlin

{{trans|Python}}

```scala
// version 1.1.4

class Node {
    val edges = mutableMapOf<Char, Node>()  // edges (or forward links)
    var link: Node? = null                  // suffix link (backward links)
    var len = 0                             // the length of the node
}

class Eertree(str: String) {
    val nodes = mutableListOf<Node>()

    private val rto = Node()                // odd length root node, or node -1
    private val rte = Node()                // even length root node, or node 0
    private val s = StringBuilder("0")      // accumulated input string, T = S[1..i]
    private var maxSufT = rte               // maximum suffix of tree T

    init {
        // Initialize and build the tree
        rte.link = rto
        rto.link = rte
        rto.len  = -1
        rte.len  = 0
        for (ch in str) add(ch)
    }

    private fun getMaxSuffixPal(startNode: Node, a: Char): Node {
        // We traverse the suffix-palindromes of T in the order of decreasing length.
        // For each palindrome we read its length k and compare T[i-k] against a
        // until we get an equality or arrive at the -1 node.
        var u = startNode
        val i = s.length
        var k = u.len
        while (u !== rto && s[i - k - 1] != a) {
            if (u === u.link!!) throw RuntimeException("Infinite loop detected")
            u = u.link!!
            k = u.len
        }
        return u
    }

    private fun add(a: Char): Boolean {
        // We need to find the maximum suffix-palindrome P of Ta
        // Start by finding maximum suffix-palindrome Q of T.
        // To do this, we traverse the suffix-palindromes of T
        // in the order of decreasing length, starting with maxSuf(T)
        val q = getMaxSuffixPal(maxSufT, a)

        // We check Q to see whether it has an outgoing edge labeled by a.
        val createANewNode = a !in q.edges.keys

        if (createANewNode) {
            // We create the node P of length Q + 2
            val p = Node()
            nodes.add(p)
            p.len = q.len + 2
            if (p.len == 1) {
                // if P = a, create the suffix link (P, 0)
                p.link = rte
            }
            else {
                // It remains to create the suffix link from P if |P|>1. Just
                // continue traversing suffix-palindromes of T starting with the
                // the suffix link of Q.
                p.link = getMaxSuffixPal(q.link!!, a).edges[a]
            }

            // create the edge (Q, P)
            q.edges[a] = p
        }

        // P becomes the new maxSufT
        maxSufT = q.edges[a]!!

        // Store accumulated input string
        s.append(a)

        return createANewNode
    }

    fun getSubPalindromes(): List<String> {
        // Traverse tree to find sub-palindromes
        val result = mutableListOf<String>()
        // Odd length words
        getSubPalindromes(rto, listOf(rto), "", result)
        // Even length words
        getSubPalindromes(rte, listOf(rte), "", result)
        return result
    }

    private fun getSubPalindromes(nd: Node, nodesToHere: List<Node>,
                          charsToHere: String, result: MutableList<String>) {
        // Each node represents a palindrome, which can be reconstructed
        // by the path from the root node to each non-root node.

        // Traverse all edges, since they represent other palindromes
        for ((lnkName, nd2) in nd.edges) {
            getSubPalindromes(nd2, nodesToHere + nd2, charsToHere + lnkName, result)
        }

        // Reconstruct based on charsToHere characters.
        if (nd !== rto && nd !== rte) { // Don't print for root nodes
            val assembled = charsToHere.reversed() +
                if (nodesToHere[0] === rte)  // Even string
                    charsToHere
                else  // Odd string
                    charsToHere.drop(1)
            result.add(assembled)
        }
    }
}

fun main(args: Array<String>) {
    val str = "eertree"
    println("Processing string '$str'")
    val eertree = Eertree(str)
    println("Number of sub-palindromes: ${eertree.nodes.size}")
    val result = eertree.getSubPalindromes()
    println("Sub-palindromes: $result")
}
```


{{out}}

```txt

Processing string 'eertree'
Number of sub-palindromes: 7
Sub-palindromes: [e, r, eertree, ertre, rtr, t, ee]

```



## M2000 Interpreter


```M2000 Interpreter

If Version<9.5 Then exit
If Version=9.5 And Revision<2 Then Exit
Class Node {
      inventory myedges
      length, suffix=0
      Function edges(s$) {
            =-1 : if exist(.myedges, s$) then =eval(.myedges)
      }
      Module edges_append (a$, where) {
            Append .myedges, a$:=where
      }
Class:
      Module Node(.length) {
            Read ? .suffix, .myedges
      }
}
function eertree(s$) {
      Const evenRoot=0, oddRoot=1
      Inventory Tree= oddRoot:=Node(-1,1),evenRoot:=Node(0,1)
      k=0
      suffix=oddRoot
      for i=0 to len(s$)-1 {
            c$=mid$(s$,i+1,1)
            n=suffix
            Do {
                 k=tree(n).length
                 b=i-k-1
                  if b>=0 then if mid$(s$,b+1,1)=c$ Then exit
                  n =tree(n).suffix
            } Always
            e=tree(n).edges(c$)
            if e>=0 then suffix=e :continue
            suffix=len(Tree)

            Append Tree, len(Tree):=Node(k+2)
            Tree(n).edges_append c$, suffix
            If tree(suffix).length=1 then tree(suffix).suffix=0 : continue
            Do {
                  n=tree(n).suffix
                  b=i-tree(n).length-1
                  if b>0 Then If  mid$(s$, b+1,1)=c$ then exit
            } Always
            e=tree(n).edges(c$)
            if e>=0 then tree(suffix).suffix=e

      }
      =tree
}
children=lambda (s, tree,  n, root$="")->{
            L=Len(tree(n).myEdges)
            if L=0 then =s : exit
            L--
            For i=0 to L {
                  c=tree(n).myEdges
                  c$=Eval$(c, i)  ' read keys at position i
                  nxt=c(i!)   '  read value using position
                  p$ = if$(n=1 -> c$, c$+root$+c$)
                  append s, (p$,)
                  \\ better use lambda() and not children()
                  \\ for recursion when we copy this lambda to other identifier.
                  s = lambda(s, tree, nxt, p$)
            }
         = s
      }
aString=Lambda ->{
          Push Quote$(Letter$)
}
aLine=Lambda ->{
      Shift 2  ' swap two top stack items
      if stackitem$()="" then  { Drop}  Else Push letter$+", "+Letter$
}
Palindromes$=Lambda$ children, aString, aLine (Tree)-> {
            ="("+children(children((,), Tree, 0), Tree, 1)#Map(aString)#Fold$(aline,"")+")"
 }

Print Palindromes$(eertree("eertree"))

```

{{out}}

```txt

("ee", "e", "r", "t", "rtr", "ertre", "eertree")

```



## Objeck

{{trans|Java}}

```objeck
use Collection.Generic;

class Eertree {
  function : Main(args : String[]) ~ Nil {
    tree := GetEertree("eertree");
    Show(SubPalindromes(tree));
  }

  function : GetEertree(s : String) ~ Vector<Node> {
    tree := Vector->New()<Node>;
    tree->AddBack(Node->New(0, Nil, 1));
    tree->AddBack(Node->New(-1, Nil, 1));
    suffix := 1;

    n : Int; k : Int;
    for(i := 0; i < s->Size(); ++i;) {
      c := s->Get(i);

      done := false;
      for (j := suffix; <>done; j := tree->Get(j)->GetSuffix();) {
        k := tree->Get(j)->GetLength();
        b := i - k - 1;
        if (b >= 0 & s->Get(b) = c) {
          n := j;
          done := true;
        };
      };
      skip := false;
      if (tree->Get(n)->GetEdges()->Has(c)) {
        suffix := tree->Get(n)->GetEdges()->Find(c)->Get();
        skip := true;
      };

      if(<>skip) {
        suffix := tree->Size();
        tree->AddBack(Node->New(k + 2));
        tree->Get(n)->GetEdges()->Insert(c, suffix);
        if (tree->Get(suffix)->GetLength() = 1) {
          tree->Get(suffix)->SetSuffix(0);
          skip := true;
        };

        if(<>skip) {
          done := false;
          while (<>done) {
            n := tree->Get(n)->GetSuffix();
            b := i - tree->Get(n)->GetLength() - 1;
            if (b >= 0 & s->Get(b) = c) {
              done := true;
            };
          };
          tree->Get(suffix)->SetSuffix(tree->Get(n)->GetEdges()->Find(c)->Get());
        };
      };
    };

    return tree;
  }

  function : SubPalindromes(tree : Vector<Node>) ~ Vector<String> {
    s := Vector->New()<String>;
    SubPalindromesChildren(0, "", tree, s);

    keys := tree->Get(1)->GetEdges()->GetKeys()<CharHolder>;
    each(k : keys) {
      key := keys->Get(k);
      str := key->Get()->ToString();
      s->AddBack(str);
      value := tree->Get(1)->GetEdges()->Find(key)->As(IntHolder)->Get();
      SubPalindromesChildren(value, str, tree, s);
    };

    return s;
  }

  function : SubPalindromesChildren(n : Int, p : String, tree : Vector<Node>, s : Vector<String>)  ~ Nil {
    keys := tree->Get(n)->GetEdges()->GetKeys()<CharHolder>;
    each(k : keys) {
      key := keys->Get(k);
      c := key->Get();
      value := tree->Get(n)->GetEdges()->Find(key)->As(IntHolder)->Get();
      str := ""; str += c; str += p; str += c;
      s->AddBack(str);
      SubPalindromesChildren(value, str, tree, s);
    };
  }

  function : Show(result : Vector<String>) ~ Nil {
    out := "[";
    each(i : result) {
      out += result->Get(i);
      if(i + 1 < result->Size()) {
        out += ", ";
      };
    };
    out += "]";
    out->PrintLine();
  }
}

class Node {
  @length : Int;
  @edges : Map<CharHolder, IntHolder>;
  @suffix : Int;

  New(length : Int, edges : Map<CharHolder, IntHolder>, suffix : Int) {
    @length := length;
    @edges := edges <> Nil ? edges : Map->New()<CharHolder, IntHolder>;
    @suffix := suffix;
  }

  New(length : Int) {
    @length := length;
    @edges := Map->New()<CharHolder, IntHolder>;
  }

  method : public : GetLength() ~ Int {
    return @length;
  }

  method : public : GetSuffix() ~ Int {
    return @suffix;
  }

  method : public : SetSuffix(suffix : Int) ~ Nil {
    @suffix := suffix;
  }

  method : public : GetEdges() ~ Map<CharHolder, IntHolder> {
    return @edges;
  }
}
```


{{output}}

```txt

[ee, e, r, t, rtr, ertre, eertree]

```



## Perl

{{trans|Perl 6}}

```perl
$str = "eertree";

for $n (1 .. length($str)) {
   for $m (1 .. length($str)) {
      $strrev = "";
      $strpal = substr($str, $n-1, $m);
      if ($strpal ne "") {
         for $p (reverse 1 .. length($strpal)) {
            $strrev .= substr($strpal, $p-1, 1);
         }
         ($strpal eq $strrev) and push @pal, $strpal;
      }
   }
}

print join ' ', grep {not $seen{$_}++} @pal, "\n";
```

{{out}}

```txt
e ee eertree ertre r rtr t
```



## Perl 6

{{trans|Ring}}

```perl6
#!/usr/bin/env perl6

use v6;

my $str = "eertree";
my @pal = ();
my ($strrev,$strpal);

for (1 .. $str.chars) -> $n {
   for (1 .. $str.chars) -> $m {
      $strrev = "";
      $strpal = $str.substr($n-1, $m);
      if ($strpal ne "") {
         for ($strpal.chars ... 1) -> $p {
            $strrev ~= $strpal.substr($p-1,1);
         }
         ($strpal eq $strrev) and @pal.push($strpal);
      }
   }
}

say @pal.unique;

```

{{out}}

```txt

(e ee eertree ertre r rtr t)

```



## Phix

If you use this in anger it may be wise to replace {string chars, sequence next} with a dictionary, which can obviously be either a new dictionary for each node, or perhaps better a single/per tree dictionary keyed on {n,ch}.

```Phix
enum LEN,SUFF,CHARS,NEXT

function node(integer len, suffix=1, string chars="", sequence next={})
    return {len,suffix,chars,next} -- must match above enum!
end function

function eertree(string s)
sequence tree = {node(-1),  -- odd lengths
                 node(0)}   -- even lengths
integer suff = 2    -- max suffix palindrome

    for i=1 to length(s) do
        integer cur = suff, curlen, ch = s[i], k
        while (true) do
            curlen = tree[cur][LEN]
            k = i-1-curlen
            if k>=1 and s[k]==ch then
                exit
            end if
            cur = tree[cur][SUFF]
        end while
        k = find(ch,tree[cur][CHARS])
        if k then
            suff = tree[cur][NEXT][k]
        else
            tree = append(tree,node(curlen+2))
            suff = length(tree)
            tree[cur][CHARS] &= ch
            tree[cur][NEXT] &= suff

            if tree[suff][LEN]==1 then
                tree[suff][SUFF] = 2
            else
                while (true) do
                    cur = tree[cur][SUFF]
                    curlen = tree[cur][LEN]
                    k = i-1-curlen
                    if k>=0 and s[k]==ch then
                        k = find(ch,tree[cur][CHARS])
                        if k then
                            tree[suff][SUFF] = tree[cur][NEXT][k]
                        end if
                        exit
                    end if
                end while
            end if
        end if
    end for
    return tree
end function

function children(sequence s, tree, integer n, string root="")
    for i=1 to length(tree[n][CHARS]) do
        integer c = tree[n][CHARS][i],
                nxt = tree[n][NEXT][i]
        string p = iff(n=1 ? c&""
                           : c&root&c)
        s = append(s, p)
        s = children(s, tree, nxt, p)
    end for
    return s
end function

procedure main()
    sequence tree = eertree("eertree")
    puts(1,"tree:\n")
    for i=1 to length(tree) do
        sequence ti = tree[i]
        ti[NEXT] = sprint(ti[NEXT])
        printf(1,"[%d]: len:%2d  suffix:%d  chars:%-5s next:%s\n",i&ti)
    end for
    puts(1,"\n")

    -- odd then even lengths:
    ?children(children(s,tree,1), tree, 2)
end procedure
main()
```

{{out}}
The tree matches Fig 1 in the pdf linked above.

```txt

tree:
[1]: len:-1  suffix:1  chars:ert   next:{3,5,6}
[2]: len: 0  suffix:1  chars:e     next:{4}
[3]: len: 1  suffix:2  chars:      next:{}
[4]: len: 2  suffix:3  chars:      next:{}
[5]: len: 1  suffix:2  chars:      next:{}
[6]: len: 1  suffix:2  chars:r     next:{7}
[7]: len: 3  suffix:5  chars:e     next:{8}
[8]: len: 5  suffix:3  chars:e     next:{9}
[9]: len: 7  suffix:4  chars:      next:{}

{"e","r","t","rtr","ertre","eertree","ee"}

```



## Python



```python
#!/bin/python
from __future__ import print_function

class Node(object):
	def __init__(self):
		self.edges = {} # edges (or forward links)
		self.link = None # suffix link (backward links)
		self.len = 0 # the length of the node

class Eertree(object):
	def __init__(self):
		self.nodes = []
		# two initial root nodes
		self.rto = Node() #odd length root node, or node -1
		self.rte = Node() #even length root node, or node 0

		# Initialize empty tree
		self.rto.link = self.rte.link = self.rto;
		self.rto.len = -1
		self.rte.len = 0
		self.S = [0] # accumulated input string, T=S[1..i]
		self.maxSufT = self.rte # maximum suffix of tree T

	def get_max_suffix_pal(self, startNode, a):
		# We traverse the suffix-palindromes of T in the order of decreasing length.
		# For each palindrome we read its length k and compare T[i-k] against a
		# until we get an equality or arrive at the -1 node.
		u = startNode
		i = len(self.S)
		k = u.len
		while id(u) != id(self.rto) and self.S[i - k - 1] != a:
			assert id(u) != id(u.link) #Prevent infinte loop
			u = u.link
			k = u.len

		return u

	def add(self, a):

		# We need to find the maximum suffix-palindrome P of Ta
		# Start by finding maximum suffix-palindrome Q of T.
		# To do this, we traverse the suffix-palindromes of T
		# in the order of decreasing length, starting with maxSuf(T)
		Q = self.get_max_suffix_pal(self.maxSufT, a)

		# We check Q to see whether it has an outgoing edge labeled by a.
		createANewNode = not a in Q.edges

		if createANewNode:
			# We create the node P of length Q+2
			P = Node()
			self.nodes.append(P)
			P.len = Q.len + 2
			if P.len == 1:
				# if P = a, create the suffix link (P,0)
				P.link = self.rte
			else:
				# It remains to create the suffix link from P if |P|>1. Just
				# continue traversing suffix-palindromes of T starting with the suffix
				# link of Q.
				P.link = self.get_max_suffix_pal(Q.link, a).edges[a]

			# create the edge (Q,P)
			Q.edges[a] = P

		#P becomes the new maxSufT
		self.maxSufT = Q.edges[a]

		#Store accumulated input string
		self.S.append(a)

		return createANewNode

	def get_sub_palindromes(self, nd, nodesToHere, charsToHere, result):
		#Each node represents a palindrome, which can be reconstructed
		#by the path from the root node to each non-root node.

		#Traverse all edges, since they represent other palindromes
		for lnkName in nd.edges:
			nd2 = nd.edges[lnkName] #The lnkName is the character used for this edge
			self.get_sub_palindromes(nd2, nodesToHere+[nd2], charsToHere+[lnkName], result)

		#Reconstruct based on charsToHere characters.
		if id(nd) != id(self.rto) and id(nd) != id(self.rte): #Don't print for root nodes
			tmp = "".join(charsToHere)
			if id(nodesToHere[0]) == id(self.rte): #Even string
				assembled = tmp[::-1] + tmp
			else: #Odd string
				assembled = tmp[::-1] + tmp[1:]
			result.append(assembled)

if __name__=="__main__":
	st = "eertree"
	print ("Processing string", st)
	eertree = Eertree()
	for ch in st:
		eertree.add(ch)

	print ("Number of sub-palindromes:", len(eertree.nodes))

	#Traverse tree to find sub-palindromes
	result = []
	eertree.get_sub_palindromes(eertree.rto, [eertree.rto], [], result) #Odd length words
	eertree.get_sub_palindromes(eertree.rte, [eertree.rte], [], result) #Even length words
	print ("Sub-palindromes:", result)
```


{{out}}

```txt
Processing string eertree
Number of sub-palindromes: 7
Sub-palindromes: ['r', 'e', 'eertree', 'ertre', 'rtr', 't', 'ee']
```



## Racket

{{trans|Python}}


```racket
#lang racket
(struct node (edges ; edges (or forward links)
              link ; suffix link (backward links)
              len) ; the length of the node
  #:mutable)

(define (new-node link len) (node (make-hash) link len))

(struct eertree (nodes
                 rto ; odd length root node, or node -1
                 rte ; even length root node, or node 0
                 S ; accumulated input string, T=S[1..i]
                 max-suf-t) ; maximum suffix of tree T
  #:mutable)

(define (new-eertree)
  (let* ((rto (new-node #f -1))
         (rte (new-node rto 0)))
    (eertree null rto rte (list 0) rte)))

(define (eertree-get-max-suffix-pal et start-node a)
  #| We traverse the suffix-palindromes of T in the order of decreasing length.
     For each palindrome we read its length k and compare T[i-k] against a
     until we get an equality or arrive at the -1 node. |#
  (match et
    [(eertree nodes rto rte (and S (app length i)) max-suf-t)
     (let loop ((u start-node))
       (let ((k (node-len u)))
         (if (or (eq? u rto) (= (list-ref S (- i k 1)) a))
             u
             (let ((u→ (node-link u)))
               (when (eq? u u→) (error 'eertree-get-max-suffix-pal "infinite loop"))
               (loop u→)))))]))

(define (eertree-add! et a)
  #| We need to find the maximum suffix-palindrome P of Ta
     Start by finding maximum suffix-palindrome Q of T.
     To do this, we traverse the suffix-palindromes of T
     in the order of decreasing length, starting with maxSuf(T) |#
  (match (eertree-get-max-suffix-pal et (eertree-max-suf-t et) a)
    [(node Q.edges Q.→ Q.len)
     ;; We check Q to see whether it has an outgoing edge labeled by a.
     (define new-node? (not (hash-has-key? Q.edges a)))
     (when new-node?
       (define P (new-node #f (+ Q.len 2))) ; We create the node P of length Q+2
       (set-eertree-nodes! et (append (eertree-nodes et) (list P)))
       (define P→
         (if (= (node-len P) 1)
             (eertree-rte et) ; if P = a, create the suffix link (P,0)
             ;; It remains to c reate the suffix link from P if |P|>1.
             ;; Just continue traversing suffix-palindromes of T starting with the suffix link of Q.
             (hash-ref (node-edges (eertree-get-max-suffix-pal et Q.→ a)) a)))
       (set-node-link! P P→)
       (hash-set! Q.edges a P)) ; create the edge (Q,P)

     (set-eertree-max-suf-t! et (hash-ref Q.edges a)) ; P becomes the new maxSufT
     (set-eertree-S! et (append (eertree-S et) (list a))) ; Store accumulated input string
     new-node?]))

(define (eertree-get-sub-palindromes et)
  (define (inr nd (node-path (list nd)) (char-path/rev null))
    ;; Each node represents a palindrome, which can be reconstructed by the path from the root node to
    ;; each non-root node.
    (let ((deeper ; Traverse all edges, since they represent other palindromes
           (for/fold ((result null)) (([→-name nd2] (in-hash (node-edges nd))))
             ; The lnk-name is the character used for this edge
             (append result (inr nd2 (append node-path (list nd2)) (cons →-name char-path/rev)))))
          (root-node? (or (eq? (eertree-rto et) nd) (eq? (eertree-rte et) nd))))
      (if root-node? ; Don't add root nodes
          deeper
          (let ((even-string? (eq? (car node-path) (eertree-rte et)))
                (char-path (reverse char-path/rev)))
            (cons (append char-path/rev (if even-string? char-path (cdr char-path))) deeper)))))
  inr)

(define (eertree-get-palindromes et)
  (define sub (eertree-get-sub-palindromes et))
  (append (sub (eertree-rto et))
          (sub (eertree-rte et))))

(module+ main
  (define et (new-eertree))
  ;; eertree works in integer space, so we'll map to/from char space here
  (for ((c "eertree")) (eertree-add! et (char->integer c)))
  (map (compose list->string (curry map integer->char)) (eertree-get-palindromes et)))

```


{{out}}

```txt
'("t" "rtr" "ertre" "eertree" "r" "e" "ee")
```



## REXX

This REXX program is modeled after the   '''Ring'''   example.

```rexx
/*REXX program creates a list of (unique) sub─palindromes that exist in an input string.*/
parse arg x .                                    /*obtain optional input string from CL.*/
if x=='' | x==","  then x= 'eertree'             /*Not specified?  Then use the default.*/
L= length(x)                                     /*the length (in chars) of input string*/
@.= .                                            /*@ tree indicates uniqueness of pals. */
$=                                               /*list of unsorted & unique palindromes*/
   do     j=1  for L                             /*start at the left side of the string.*/
       do k=1  for L                             /*traverse from left to right of string*/
       parse var  x   =(j)  y   +(k)             /*extract a substring from the string. */
       if reverse(y)\==y | @.y\==.  then iterate /*Partial string a palindrome?  Skip it*/
       @.y= y                                    /*indicate a sub─palindrome was found. */
       $= $' '  y                                /*append the sub─palindrome to the list*/
       end   /*k*/                               /* [↑]  an extra blank is inserted.    */
   end       /*j*/

say '──────── The input string that is being used: '     space(x)
say '──────── The number of sub─palindromes found: '     words($)
say '──────── The  list  of sub─palindromes found: '     strip($)
                                                 /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

──────── The input string that is being used:  eertree
──────── The number of sub─palindromes found:  7
──────── The  list  of sub─palindromes found:  e  ee  eertree  ertre  r  rtr  t

```



## Ring


```ring

# Project : Eertree

str = "eertree"
pal = []
for n=1 to len(str)
    for m=1 to len(str)
        strrev = ""
        strpal = substr(str, n, m)
        if strpal != ""
           for p=len(strpal) to 1 step -1
               strrev = strrev + strpal[p]
           next
           if strpal = strrev
              add(pal, strpal)
           ok
        ok
    next
next
sortpal = sort(pal)
for n=len(sortpal) to 2 step -1
    if sortpal[n] = sortpal[n-1]
       del(sortpal, n)
    ok
next
see sortpal + nl

```

Output:

```txt

e
ee
eertree
ertre
r
rtr
t

```



## zkl

{{trans|Python}}

```zkl
class Node{
   fcn init(length){
      var edges=Dictionary(),	# edges (or forward links). (char:Node)
          link=Void,		# suffix link (backward links)
	  sz  =length;		# node length.
    }
}
class Eertree{
   fcn init(string=Void){
      var nodes=List(),
		# two initial root nodes
	  rto=Node(-1), # odd  length root node, or node -1
	  rte=Node(0);  # even length root node, or node  0

      rto.link=rte.link=rto;    # Initialize empty tree
      var S      =Data(Void,0), # accumulated input string, T=S[1..i], byte buffer
          maxSufT=rte;          # maximum suffix of tree T
      if(string) string.pump(addChar);  // go ahead and build the tree
   }
   fcn get_max_suffix_pal(startNode,a){
    # We traverse the suffix-palindromes of T in the order of decreasing length.
    # For each palindrome we read its length k and compare T[i-k] against a
    # until we get an equality or arrive at the -1 node.
      u,i,k := startNode, S.len(), u.sz;
      while(u.id!=rto.id and S.charAt(i - k - 1)!=a){
	 _assert_(u.id!=u.link.id);    # Prevent infinte loop
	 u,k = u.link,u.sz;
      }
      return(u);
   }
   fcn addChar(a){
	# We need to find the maximum suffix-palindrome P of Ta
	# Start by finding maximum suffix-palindrome Q of T.
	# To do this, we traverse the suffix-palindromes of T
	# in the order of decreasing length, starting with maxSuf(T)
      Q:=get_max_suffix_pal(maxSufT,a);
        # We check Q to see whether it has an outgoing edge labeled by a.
      createANewNode:=(not Q.edges.holds(a));
      if(createANewNode){
	 P:=Node(Q.sz + 2); nodes.append(P);
	 if(P.sz==1) P.link=rte;  # if P = a, create the suffix link (P,0)
	 else # It remains to create the suffix link from P if |P|>1. Just
	      # continue traversing suffix-palindromes of T starting with the suffix
	      # link of Q.
	    P.link=get_max_suffix_pal(Q.link,a).edges[a];
	 Q.edges[a]=P;    # create the edge (Q,P)
      }
      maxSufT=Q.edges[a]; # P becomes the new maxSufT
      S.append(a);	  # Store accumulated input string
      return(createANewNode);  // in case anyone wants to know a is new edge
   }
   fcn get_sub_palindromes{
      result:=List();
      sub_palindromes(rto, T(rto),"", result); # Odd length words
      sub_palindromes(rte, T(rte),"", result); # Even length words
      result
   }
   fcn [private] sub_palindromes(nd, nodesToHere, charsToHere, result){
        // nodesToHere needs to be read only
	# Each node represents a palindrome, which can be reconstructed
	# by the path from the root node to each non-root node.

	# Traverse all edges, since they represent other palindromes
      nd.edges.pump(Void,'wrap([(lnkName,nd2)]){
	 sub_palindromes(nd2, nodesToHere+nd2, charsToHere+lnkName, result);
      });

      # Reconstruct based on charsToHere characters.
      if(nd.id!=rto.id and nd.id!=rte.id){ # Don't print for root nodes
	 if(nodesToHere[0].id==rte.id) # Even string
	    assembled:=charsToHere.reverse() + charsToHere;
	 else assembled:=charsToHere.reverse() + charsToHere[1,*]; # Odd string
	 result.append(assembled);
      }
   }
}
```


```zkl
st:="eertree";
println("Processing string \"", st,"\"");
eertree:=Eertree(st);
println("Number of sub-palindromes: ", eertree.nodes.len());
println("Sub-palindromes: ", eertree.get_sub_palindromes());
```

{{out}}

```txt

Processing string "eertree"
Number of sub-palindromes: 7
Sub-palindromes: L("e","r","eertree","ertre","rtr","t","ee")

```

