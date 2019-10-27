+++
title = "Suffix tree"
description = ""
date = 2019-05-08T23:47:53Z
aliases = []
[extra]
id = 13459
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
[[Category:String algorithms]]
A [[wp:suffix tree|suffix tree]] is a data structure commonly used in [[wp:string algorithms|string algorithms]].

Given a string S of length n, its suffix tree is a tree T such that:

* T has exactly n leaves numbered from 1 to n.
* Except for the root, every internal node has at least two children.
* Each edge of T is labelled with a non-empty substring of S.
* No two edges starting out of a node can have string labels beginning with the same character.
* The string obtained by concatenating all the string labels found on the path from the root to leaf i spells out suffix S[i..n], for i from 1 to n.


Such a tree does not exist for all strings.  To ensure existence, a character that is not found in S must be appended at its end.  The character '$' is traditionally used for this purpose.

For this task, build and display the suffix tree of the string "banana$".  Displaying the tree can be done using the code from the [[visualize a tree]] task, but any other convenient method is accepted.

There are several ways to implement the tree data structure, for instance how edges should be labelled.  Latitude is given in this matter, but notice that a simple way to do it is to label each node with the label of the edge leading to it.

The computation time for an efficient algorithm should be <math>O(n)</math>, but such an algorithm might be difficult to implement.  An easier, <math>O(n^2)</math> algorithm is accepted.


## C++

{{trans|D}}

```cpp>#include <functional

#include <iostream>
#include <vector>

struct Node {
    std::string sub = "";   // a substring of the input string
    std::vector<int> ch;    // vector of child nodes

    Node() {
        // empty
    }

    Node(const std::string& sub, std::initializer_list<int> children) : sub(sub) {
        ch.insert(ch.end(), children);
    }
};

struct SuffixTree {
    std::vector<Node> nodes;

    SuffixTree(const std::string& str) {
        nodes.push_back(Node{});
        for (size_t i = 0; i < str.length(); i++) {
            addSuffix(str.substr(i));
        }
    }

    void visualize() {
        if (nodes.size() == 0) {
            std::cout << "<empty>\n";
            return;
        }

        std::function<void(int, const std::string&)> f;
        f = [&](int n, const std::string & pre) {
            auto children = nodes[n].ch;
            if (children.size() == 0) {
                std::cout << "- " << nodes[n].sub << '\n';
                return;
            }
            std::cout << "+ " << nodes[n].sub << '\n';

            auto it = std::begin(children);
            if (it != std::end(children)) do {
                if (std::next(it) == std::end(children)) break;
                std::cout << pre << "+-";
                f(*it, pre + "| ");
                it = std::next(it);
            } while (true);

            std::cout << pre << "+-";
            f(children[children.size() - 1], pre + "  ");
        };

        f(0, "");
    }

private:
    void addSuffix(const std::string & suf) {
        int n = 0;
        size_t i = 0;
        while (i < suf.length()) {
            char b = suf[i];
            int x2 = 0;
            int n2;
            while (true) {
                auto children = nodes[n].ch;
                if (x2 == children.size()) {
                    // no matching child, remainder of suf becomes new node
                    n2 = nodes.size();
                    nodes.push_back(Node(suf.substr(i), {}));
                    nodes[n].ch.push_back(n2);
                    return;
                }
                n2 = children[x2];
                if (nodes[n2].sub[0] == b) {
                    break;
                }
                x2++;
            }
            // find prefix of remaining suffix in common with child
            auto sub2 = nodes[n2].sub;
            size_t j = 0;
            while (j < sub2.size()) {
                if (suf[i + j] != sub2[j]) {
                    // split n2
                    auto n3 = n2;
                    // new node for the part in common
                    n2 = nodes.size();
                    nodes.push_back(Node(sub2.substr(0, j), { n3 }));
                    nodes[n3].sub = sub2.substr(j); // old node loses the part in common
                    nodes[n].ch[x2] = n2;
                    break; // continue down the tree
                }
                j++;
            }
            i += j; // advance past part in common
            n = n2; // continue down the tree
        }
    }
};

int main() {
    SuffixTree("banana$").visualize();
}
```

{{out}}

```txt
+
+-- banana$
+-+ a
| +-+ na
| | +-- na$
| | +-- $
| +-- $
+-+ na
| +-- na$
| +-- $
+-- $
```



## D

{{trans|Kotlin}}

```D
import std.stdio;

struct Node {
    string sub = ""; // a substring of the input string
    int[] ch;        // array of child nodes

    this(string sub, int[] children ...) {
        this.sub = sub;
        ch = children;
    }
}

struct SuffixTree {
    Node[] nodes;

    this(string str) {
        nodes ~= Node();
        for (int i=0; i<str.length; ++i) {
            addSuffix(str[i..$]);
        }
    }

    private void addSuffix(string suf) {
        int n = 0;
        int i = 0;
        while (i < suf.length) {
            char b  = suf[i];
            int x2 = 0;
            int n2;
            while (true) {
                auto children = nodes[n].ch;
                if (x2 == children.length) {
                    // no matching child, remainder of suf becomes new node.
                    n2 = nodes.length;
                    nodes ~= Node(suf[i..$]);
                    nodes[n].ch ~= n2;
                    return;
                }
                n2 = children[x2];
                if (nodes[n2].sub[0] == b) {
                    break;
                }
                x2++;
            }
            // find prefix of remaining suffix in common with child
            auto sub2 = nodes[n2].sub;
            int j = 0;
            while (j < sub2.length) {
                if (suf[i + j] != sub2[j]) {
                    // split n2
                    auto n3 = n2;
                    // new node for the part in common
                    n2 = nodes.length;
                    nodes ~= Node(sub2[0..j], n3);
                    nodes[n3].sub = sub2[j..$];  // old node loses the part in common
                    nodes[n].ch[x2] = n2;
                    break;  // continue down the tree
                }
                j++;
            }
            i += j;  // advance past part in common
            n = n2;  // continue down the tree
        }
    }

    void visualize() {
        if (nodes.length == 0) {
            writeln("<empty>");
            return;
        }

        void f(int n, string pre) {
            auto children = nodes[n].ch;
            if (children.length == 0) {
                writefln("╴ %s", nodes[n].sub);
                return;
            }
            writefln("┐ %s", nodes[n].sub);
            foreach (c; children[0..$-1]) {
                write(pre, "├─");
                f(c, pre ~ "│ ");
            }
            write(pre, "└─");
            f(children[$-1], pre ~ "  ");
        }

        f(0, "");
    }
}

void main() {
    SuffixTree("banana$").visualize();
}
```

{{out}}

```txt
┐ 
├─╴ banana$
├─┐ a
│ ├─┐ na
│ │ ├─╴ na$
│ │ └─╴ $
│ └─╴ $
├─┐ na
│ ├─╴ na$
│ └─╴ $
└─╴ $
```



## Go

Vis function from [[Visualize_a_tree#Unicode]].

```go
package main

import "fmt"

func main() {
    vis(buildTree("banana$"))
}

type tree []node

type node struct {
    sub string // a substring of the input string
    ch  []int  // list of child nodes
}

func buildTree(s string) tree {
    t := tree{node{}} // root node
    for i := range s {
        t = t.addSuffix(s[i:])
    }
    return t
}

func (t tree) addSuffix(suf string) tree {
    n := 0
    for i := 0; i < len(suf); {
        b := suf[i]
        ch := t[n].ch
        var x2, n2 int
        for ; ; x2++ {
            if x2 == len(ch) {
                // no matching child, remainder of suf becomes new node.
                n2 = len(t)
                t = append(t, node{sub: suf[i:]})
                t[n].ch = append(t[n].ch, n2)
                return t
            }
            n2 = ch[x2]
            if t[n2].sub[0] == b {
                break
            }
        }
        // find prefix of remaining suffix in common with child
        sub2 := t[n2].sub
        j := 0
        for ; j < len(sub2); j++ {
            if suf[i+j] != sub2[j] {
                // split n2
                n3 := n2
                // new node for the part in common
                n2 = len(t)
                t = append(t, node{sub2[:j], []int{n3}})
                t[n3].sub = sub2[j:] // old node loses the part in common
                t[n].ch[x2] = n2
                break // continue down the tree
            }
        }
        i += j // advance past part in common
        n = n2 // continue down the tree
    }
    return t
}

func vis(t tree) {
    if len(t) == 0 {
        fmt.Println("<empty>")
        return
    }
    var f func(int, string)
    f = func(n int, pre string) {
        children := t[n].ch
        if len(children) == 0 {
            fmt.Println("╴", t[n].sub)
            return
        }
        fmt.Println("┐", t[n].sub)
        last := len(children) - 1
        for _, ch := range children[:last] {
            fmt.Print(pre, "├─")
            f(ch, pre+"│ ")
        }
        fmt.Print(pre, "└─")
        f(children[last], pre+"  ")
    }
    f(0, "")
}
```

{{out}}

```txt

┐ 
├─╴ banana$
├─┐ a
│ ├─┐ na
│ │ ├─╴ na$
│ │ └─╴ $
│ └─╴ $
├─┐ na
│ ├─╴ na$
│ └─╴ $
└─╴ $

```



## J


Implementation:


```J
classify=: {.@> </. ]

build_tree=:3 :0
  tree=. ,:_;_;''
  if. 0=#y do. tree return.end.
  if. 1=#y do. tree,(#;y);0;y return.end.
  for_box.classify y do.
    char=. {.>{.>box
    subtree=. }.build_tree }.each>box
    ndx=.I.0=1&{::"1 subtree
    n=.#tree
    if. 1=#ndx do.
      counts=. 1 + 0&{::"1 subtree
      parents=. (n-1) (+*]&*) 1&{::"1 subtree
      edges=. (ndx}~ <@(char,ndx&{::)) 2&{"1 subtree
      tree=. tree, counts;"0 1 parents;"0 edges
    else.
      tree=. tree,(__;0;,char),(1;n;0) + ::]&.>"1 subtree
    end.
  end.
)
 
suffix_tree=:3 :0
  assert. -.({:e.}:)y
  tree=. B=:|:build_tree <\. y
  ((1+#y)-each {.tree),}.tree
)
```


Task example:


```J
   suffix_tree 'banana$'
┌──┬───────┬─┬──┬───┬─┬─┬──┬───┬─┬─┐
│__│1      │_│_ │2  │4│6│_ │3  │5│7│
├──┼───────┼─┼──┼───┼─┼─┼──┼───┼─┼─┤
│_ │0      │0│2 │3  │3│2│0 │7  │7│0│
├──┼───────┼─┼──┼───┼─┼─┼──┼───┼─┼─┤
│  │banana$│a│na│na$│$│$│na│na$│$│$│
└──┴───────┴─┴──┴───┴─┴─┴──┴───┴─┴─┘
```


The first row is the leaf number (_ for internal nodes).

The second row is parent index (_ for root node).

The third row is the edge's substring (empty for root node).

Visualizing, using [[Visualize_a_tree#J|showtree]] and prefixing the substring leading to each leaf with the leaf number (in brackets):


```J
fmttree=: ;@(1&{) showtree~ {: (,~ }.`('[','] ',~":)@.(_>|))each {.

   fmttree suffix_tree 'banana$'
    ┌─ [1] banana$                    
    │                       ┌─ [2] na$
    │             ┌─ na ────┴─ [4] $  
────┼─ a ─────────┴─ [6] $            
    │             ┌─ [3] na$          
    ├─ na ────────┴─ [5] $            
    └─ [7] $                          

```



## Java

{{trans|Kotlin}}

```Java
import java.util.ArrayList;
import java.util.List;

public class SuffixTreeProblem {
    private static class Node {
        String sub = "";                       // a substring of the input string
        List<Integer> ch = new ArrayList<>();  // list of child nodes
    }

    private static class SuffixTree {
        private List<Node> nodes = new ArrayList<>();

        public SuffixTree(String str) {
            nodes.add(new Node());
            for (int i = 0; i < str.length(); ++i) {
                addSuffix(str.substring(i));
            }
        }

        private void addSuffix(String suf) {
            int n = 0;
            int i = 0;
            while (i < suf.length()) {
                char b = suf.charAt(i);
                List<Integer> children = nodes.get(n).ch;
                int x2 = 0;
                int n2;
                while (true) {
                    if (x2 == children.size()) {
                        // no matching child, remainder of suf becomes new node.
                        n2 = nodes.size();
                        Node temp = new Node();
                        temp.sub = suf.substring(i);
                        nodes.add(temp);
                        children.add(n2);
                        return;
                    }
                    n2 = children.get(x2);
                    if (nodes.get(n2).sub.charAt(0) == b) break;
                    x2++;
                }
                // find prefix of remaining suffix in common with child
                String sub2 = nodes.get(n2).sub;
                int j = 0;
                while (j < sub2.length()) {
                    if (suf.charAt(i + j) != sub2.charAt(j)) {
                        // split n2
                        int n3 = n2;
                        // new node for the part in common
                        n2 = nodes.size();
                        Node temp = new Node();
                        temp.sub = sub2.substring(0, j);
                        temp.ch.add(n3);
                        nodes.add(temp);
                        nodes.get(n3).sub = sub2.substring(j);  // old node loses the part in common
                        nodes.get(n).ch.set(x2, n2);
                        break;  // continue down the tree
                    }
                    j++;
                }
                i += j;  // advance past part in common
                n = n2;  // continue down the tree
            }
        }

        public void visualize() {
            if (nodes.isEmpty()) {
                System.out.println("<empty>");
                return;
            }
            visualize_f(0, "");
        }

        private void visualize_f(int n, String pre) {
            List<Integer> children = nodes.get(n).ch;
            if (children.isEmpty()) {
                System.out.println("- " + nodes.get(n).sub);
                return;
            }
            System.out.println("┐ " + nodes.get(n).sub);
            for (int i = 0; i < children.size() - 1; i++) {
                Integer c = children.get(i);
                System.out.print(pre + "├─");
                visualize_f(c, pre + "│ ");
            }
            System.out.print(pre + "└─");
            visualize_f(children.get(children.size() - 1), pre + "  ");
        }
    }

    public static void main(String[] args) {
        new SuffixTree("banana$").visualize();
    }
}
```

{{out}}

```txt
┐ 
├─- banana$
├─┐ a
│ ├─┐ na
│ │ ├─- na$
│ │ └─- $
│ └─- $
├─┐ na
│ ├─- na$
│ └─- $
└─- $
```



## Kotlin

{{trans|Go}}

```scala
// version 1.1.3

class Node {
    var sub = ""                    // a substring of the input string
    var ch  = mutableListOf<Int>()  // list of child nodes
}

class SuffixTree(val str: String) {
    val nodes = mutableListOf<Node>(Node())

    init {
        for (i in 0 until str.length) addSuffix(str.substring(i))
    }

    private fun addSuffix(suf: String) {
        var n = 0
        var i = 0
        while (i < suf.length) {
            val b  = suf[i]
            val children = nodes[n].ch
            var x2 = 0
            var n2: Int
            while (true) {
                if (x2 == children.size) {
                    // no matching child, remainder of suf becomes new node.
                    n2 = nodes.size
                    nodes.add(Node().apply { sub = suf.substring(i) } )
                    children.add(n2)
                    return
                }
                n2 = children[x2]
                if (nodes[n2].sub[0] == b) break
                x2++
            }
            // find prefix of remaining suffix in common with child
            val sub2 = nodes[n2].sub
            var j = 0
            while (j < sub2.length) {
                if (suf[i + j] != sub2[j]) {
                    // split n2
                    val n3 = n2
                    // new node for the part in common
                    n2 = nodes.size
                    nodes.add(Node().apply {
                        sub = sub2.substring(0, j)
                        ch.add(n3)
                    })
                    nodes[n3].sub = sub2.substring(j)  // old node loses the part in common
                    nodes[n].ch[x2] = n2
                    break  // continue down the tree
                }
                j++
            }
            i += j  // advance past part in common
            n = n2  // continue down the tree
        }
    }

    fun visualize() {
        if (nodes.isEmpty()) {
            println("<empty>")
            return
        }

        fun f(n: Int, pre: String) {
            val children = nodes[n].ch
            if (children.isEmpty()) {
                println("╴ ${nodes[n].sub}")
                return
            }
            println("┐ ${nodes[n].sub}")
            for (c in children.dropLast(1)) {
                print(pre + "├─")
                f(c, pre + "│ ")
            }
            print(pre + "└─")
            f(children.last(), pre + "  ")
        }

        f(0, "")
    }
}

fun main(args: Array<String>) {
    SuffixTree("banana$").visualize()
}
```


{{out}}

```txt

┐ 
├─╴ banana$
├─┐ a
│ ├─┐ na
│ │ ├─╴ na$
│ │ └─╴ $
│ └─╴ $
├─┐ na
│ ├─╴ na$
│ └─╴ $
└─╴ $

```



## Perl

{{trans|Perl 6}}

```Perl
use strict;
use warnings;
use Data::Dumper;
 
sub classify {
    my $h = {};
    for (@_) { push @{$h->{substr($_,0,1)}}, $_ }
    return $h;
}
sub suffixes {
    my $str = shift;
    map { substr $str, $_ } 0 .. length($str) - 1;
}
sub suffix_tree {
    return +{} if @_ == 0;
    return +{ $_[0] => +{} } if @_ == 1;
    my $h = {};
    my $classif = classify @_;
    for my $key (keys %$classif) {
        my $subtree = suffix_tree(
            map { substr $_, 1 } @{$classif->{$key}}
        );
        my @subkeys = keys %$subtree;
        if (@subkeys == 1) {
            my ($subkey) = @subkeys;
            $h->{"$key$subkey"} = $subtree->{$subkey};
        } else { $h->{$key} = $subtree }
    }
    return $h;
}
print +Dumper suffix_tree suffixes 'banana$';
```

{{out}}

```txt
$VAR1 = {
          '$' => {},
          'a' => {
                   '$' => {},
                   'na' => {
                             'na$' => {},
                             '$' => {}
                           }
                 },
          'banana$' => {},
          'na' => {
                    'na$' => {},
                    '$' => {}
                  }
        };
```



## Perl 6

{{Works with|Rakudo|2018.04}}
Here is quite a naive algorithm, probably <math>O(n^2)</math>.

The display code is a variant of the [[visualize_a_tree#Perl6|visualize a tree]] task code.


```perl6
multi suffix-tree(Str $str) { suffix-tree flat map &flip, [\~] $str.flip.comb }
multi suffix-tree(@a) {
    hash
    @a == 0 ?? () !!
    @a == 1 ?? ( @a[0] => [] ) !!
    gather for @a.classify(*.substr(0, 1)) {
        my $subtree = suffix-tree(grep *.chars, map *.substr(1), .value[]);
        if $subtree == 1 {
            my $pair = $subtree.pick;
            take .key ~ $pair.key => $pair.value;
        } else {
            take .key => $subtree;
        }
    }
}

my $tree = root => suffix-tree 'banana$';

.say for visualize-tree $tree, *.key, *.value.List;

sub visualize-tree($tree, &label, &children,
                   :$indent = '',
                   :@mid = ('├─', '│ '),
                   :@end = ('└─', '  '),
) {
    sub visit($node, *@pre) {
        gather {
            take @pre[0] ~ $node.&label;
            my @children = sort $node.&children;
            my $end = @children.end;
            for @children.kv -> $_, $child {
                when $end { take visit($child, (@pre[1] X~ @end)) }
                default   { take visit($child, (@pre[1] X~ @mid)) }
            }
        }
    }
    flat visit($tree, $indent xx 2);
}
```


{{out}}

```txt
root
├─$
├─a
│ ├─$
│ └─na
│   ├─$
│   └─na$
├─banana$
└─na
  ├─$
  └─na$
```



## Phix

{{trans|D}}

```Phix
-- tree nodes are simply {string substr, sequence children_idx}
enum SUB=1, CHILDREN=2

function addSuffix(sequence t, string suffix)
    int n = 1, i = 1
    while i<=length(suffix) do
        integer ch = suffix[i], x2 = 1, n2
        while (true) do
            sequence children = t[n][CHILDREN]
            if x2>length(children) then
                -- no matching child, remainder of suffix becomes new node.
                t = append(t,{suffix[i..$],{}})
                t[n][CHILDREN] &= length(t)
                return t
            end if
            n2 = children[x2]
            if t[n2][SUB][1]==ch then exit end if
            x2 += 1
        end while
        -- find prefix of remaining suffix in common with child
        string prefix = t[n2][SUB]
        int j = 0
        while j<length(prefix) do
            if suffix[i+j]!=prefix[j+1] then
                -- split n2: new node for the part in common
                t = append(t,{prefix[1..j],{n2}})
                -- old node loses the part in common
                t[n2][SUB] = prefix[j+1..$]
                -- and child idx moves to newly created node
                n2 = length(t)
                t[n][CHILDREN][x2] = n2
                exit    -- continue down the tree
            end if
            j += 1
        end while
        i += j  -- advance past part in common
        n = n2  -- continue down the tree
    end while
    return t
end function

function SuffixTree(string s)
    sequence t = {{"",{}}}
    for i=1 to length(s) do
        t = addSuffix(t,s[i..$])
    end for
    return t
end function
 
procedure visualize(sequence t, integer n=1, string pre="")
    if length(t)=0 then
        printf(1,"<empty>\n");
        return;
    end if
    sequence children = t[n][CHILDREN]
    if length(children)=0 then
        printf(1,"- %s\n", {t[n][SUB]})
        return
    end if
    printf(1,"+ %s\n", {t[n][SUB]})
    integer l = length(children)
    for i=1 to l do
        puts(1,pre&"+-")
        visualize(t,children[i],pre&iff(i=l?"  ":"| "))
    end for
end procedure

sequence t = SuffixTree("banana$")
visualize(t)
```

{{out}}

```txt

+
+-- banana$
+-+ a
| +-+ na
| | +-- na$
| | +-- $
| +-- $
+-+ na
| +-- na$
| +-- $
+-- $

```



## Python

{{trans|D}}

```python
class Node:
    def __init__(self, sub="", children=[]):
        self.sub = sub
        self.ch = children

class SuffixTree:
    def __init__(self, str):
        self.nodes = [Node()]
        for i in range(len(str)):
            self.addSuffix(str[i:])

    def addSuffix(self, suf):
        n = 0
        i = 0
        while i < len(suf):
            b = suf[i]
            x2 = 0
            while True:
                children = self.nodes[n].ch
                if x2 == len(children):
                    # no matching child, remainder of suf becomes new node
                    n2 = len(self.nodes)
                    self.nodes.append(Node(suf[i:], []))
                    self.nodes[n].ch.append(n2)
                    return
                n2 = children[x2]
                if self.nodes[n2].sub[0] == b:
                    break
                x2 = x2 + 1

            # find prefix of remaining suffix in common with child
            sub2 = self.nodes[n2].sub
            j = 0
            while j < len(sub2):
                if suf[i + j] != sub2[j]:
                    # split n2
                    n3 = n2
                    # new node for the part in common
                    n2 = len(self.nodes)
                    self.nodes.append(Node(sub2[:j], [n3]))
                    self.nodes[n3].sub = sub2[j:] # old node loses the part in common
                    self.nodes[n].ch[x2] = n2
                    break # continue down the tree
                j = j + 1
            i = i + j   # advance past part in common
            n = n2      # continue down the tree

    def visualize(self):
        if len(self.nodes) == 0:
            print "<empty>"
            return

        def f(n, pre):
            children = self.nodes[n].ch
            if len(children) == 0:
                print "--", self.nodes[n].sub
                return
            print "+-", self.nodes[n].sub
            for c in children[:-1]:
                print pre, "+-",
                f(c, pre + " | ")
            print pre, "+-",
            f(children[-1], pre + "  ")

        f(0, "")

SuffixTree("banana$").visualize()
```

{{out}}

```txt
+-
 +- -- banana$
 +- +- a
 |  +- +- na
 |  |  +- -- na$
 |  |  +- -- $
 |  +- -- $
 +- +- na
 |  +- -- na$
 |  +- -- $
 +- -- $
```



## Racket

See [http://planet.racket-lang.org/package-source/dyoo/suffixtree.plt/1/2/planet-docs/manual/index.html#(def._((planet._main..rkt._(dyoo._suffixtree..plt._1._2))._tree-walk)) Suffix trees with Ukkonen’s algorithm]
by Danny Yoo for more information on how to use suffix trees in Racket.


```racket
#lang racket
(require (planet dyoo/suffixtree))
(define tree (make-tree))
(tree-add! tree (string->label "banana$"))

(define (show-node nd dpth)
  (define children (node-children nd))
  (printf "~a~a ~a~%" (match dpth
                        [(regexp #px"(.*) $" (list _ d)) (string-append d "`")]
                        [else else]) (if (null? children) "--" "-+") (label->string (node-up-label nd)))
  (let l ((children children))
    (match children
      ((list) (void))
      ((list c) (show-node c (string-append dpth "  ")))
      ((list c ct ...) (show-node c (string-append dpth " |")) (l ct)))))

(show-node (tree-root tree) "")
```


{{out}}

```txt
-+ 
 |-- $
 |-+ a
 | |-- $
 | `-+ na
 |   |-- $
 |   `-- na$
 |-+ na
 | |-- $
 | `-- na$
 `-- banana$
```



## Sidef

{{trans|Perl 6}}

```ruby
func suffix_tree(Str t) {
    suffix_tree(^t.len -> map { t.substr(_) })
}

func suffix_tree(a {.len == 1}) {
    Hash(a[0] => nil) 
}

func suffix_tree(Arr a) {
    var h = Hash()
    for k,v in (a.group_by { .char(0) }) {
        var subtree = suffix_tree(v.map { .substr(1) })
        var subkeys = subtree.keys
        if (subkeys.len == 1) {
            var subk = subkeys[0]
            h{k + subk} = subtree{subk}
        }
        else {
            h{k} = subtree
        }
    }
    return h
}

say suffix_tree('banana$')
```

{{out}}

```txt

Hash(
    "$" => nil,
    "a" => Hash(
        "$" => nil,
        "na" => Hash(
            "$" => nil,
            "na$" => nil
        )
    ),
    "banana$" => nil,
    "na" => Hash(
        "$" => nil,
        "na$" => nil
    )
)

```

