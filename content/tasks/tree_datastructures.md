+++
title = "Tree datastructures"
description = ""
date = 2019-10-21T23:30:52Z
aliases = []
[extra]
id = 22586
[taxonomies]
categories = ["task"]
tags = []
+++

The following shows a tree of data with nesting denoted by visual levels of indentation:

```txt
RosettaCode
    rocks
        code
        comparison
        wiki
    mocks
        trolling
```


A common datastructure for trees is to define node structures having a name and a, (possibly empty), list of child nodes. The nesting of nodes captures the indentation of the tree. Lets call this '''the nest form'''.

```txt
# E.g. if child nodes are surrounded by brackets
#      and separated by commas then:
RosettaCode(rocks(code, ...), ...)
# But only an _example_
```


Another datastructure for trees is to construct from the root an ordered list of the nodes level of indentation and the name of that node. The indentation for the root node is zero; node 'rocks is indented by one level from the left, and so on. Lets call this '''the indent form'''.

```txt
0 RosettaCode
1 rocks
2 code
...
```


## Task

# Create/use a nest datastructure format and textual representation for arbitrary trees.
# Create/use an indent datastructure format and textual representation for arbitrary trees.
# Create methods/classes/proceedures/routines etc to:
## Change from a nest tree datastructure to an indent one.
## Change from an indent tree datastructure to a nest one
# Use the above to encode the example at the start into the nest format, and show it.
# transform the initial nest format to indent format and show it.
# transform the indent format to final nest format and show it.
# Compare initial and final nest formats which should be the same.

;Note:
* It's all about showing aspects of the contrasting datastructures as they hold the tree.
* Comparing nested datastructures is secondary - saving formatted output as a string then a string compare would suffice for this task, if its easier.
* The word "trolling"  is substituted for the original, less appropriate, "golfing" in the tree above as golfing can be friendly fun! (just not for RC examples). '''Please update language examples appropriately'''.



Show all output on this page.


## Go


```go
package main

import (
    "fmt"
    "io"
    "os"
    "strings"
)

type nNode struct {
    name     string
    children []nNode
}

type iNode struct {
    level int
    name  string
}

func printNest(n nNode, level int, w io.Writer) {
    if level == 0 {
        fmt.Fprintln(w, "\n==Nest form==\n")
    }
    fmt.Fprintf(w, "%s%s\n", strings.Repeat("  ", level), n.name)
    for _, c := range n.children {
        fmt.Fprintf(w, "%s", strings.Repeat("  ", level+1))
        printNest(c, level+1, w)
    }
}

func toNest(iNodes []iNode, start, level int, n *nNode) {
    if level == 0 {
        n.name = iNodes[0].name
    }
    for i := start + 1; i < len(iNodes); i++ {
        if iNodes[i].level == level+1 {
            c := nNode{iNodes[i].name, nil}
            toNest(iNodes, i, level+1, &c)
            n.children = append(n.children, c)
        } else if iNodes[i].level <= level {
            return
        }
    }
}

func printIndent(iNodes []iNode, w io.Writer) {
    fmt.Fprintln(w, "\n==Indent form==\n")
    for _, n := range iNodes {
        fmt.Fprintf(w, "%d %s\n", n.level, n.name)
    }
}

func toIndent(n nNode, level int, iNodes *[]iNode) {
    *iNodes = append(*iNodes, iNode{level, n.name})
    for _, c := range n.children {
        toIndent(c, level+1, iNodes)
    }
}

func main() {
    n1 := nNode{"RosettaCode", nil}
    n2 := nNode{"rocks", []nNode{{"code", nil}, {"comparison", nil}, {"wiki", nil}}}
    n3 := nNode{"mocks", []nNode{{"trolling", nil}}}
    n1.children = append(n1.children, n2, n3)

    var sb strings.Builder
    printNest(n1, 0, &sb)
    s1 := sb.String()
    fmt.Print(s1)

    var iNodes []iNode
    toIndent(n1, 0, &iNodes)
    printIndent(iNodes, os.Stdout)

    var n nNode
    toNest(iNodes, 0, 0, &n)
    sb.Reset()
    printNest(n, 0, &sb)
    s2 := sb.String()
    fmt.Print(s2)

    fmt.Println("\nRound trip test satisfied? ", s1 == s2)
}
```


```txt

==Nest form==

RosettaCode
    rocks
        code
        comparison
        wiki
    mocks
        trolling

==Indent form==

0 RosettaCode
1 rocks
2 code
2 comparison
2 wiki
1 mocks
2 trolling

==Nest form==

RosettaCode
    rocks
        code
        comparison
        wiki
    mocks
        trolling

Round trip test satisfied?  true

```



## Perl 6

Code golf is a entertaining passtime, even if it isn't appropriate for this site. To a large extent, I agree with [[User:Hout|Hout]], I am not really on board with mocking anybody, especially espousing it as an official RosettaCode position. So, feel free to mark this incorrect.


```perl6
#`(
Sort of vague as to what we are trying to accomplish here. If we are just
trying to transform from one format to another, probably easiest to just
perform string manipulations.
)

my $level = '  ';

my $tree = q:to/END/;
    RosettaCode
      encourages
        code
          diversity
          comparison
      discourages
        golfing
        trolling
        emphasising execution speed
    code-golf.io
      encourages
        golfing
      discourages
        comparison
    END

sub nested-to-indent { $^str.subst: /^^ ($($level))*/, -> $/ { "{+$0} " }, :g }
sub indent-to-nested { $^str.subst: /^^ (\d+) \s* /, -> $/ { "{$level x +$0}" }, :g }

say $tree;
say my $indent = $tree.&nested-to-indent;
say my $nest = $indent.&indent-to-nested;

use Test;
is($tree, $nest, 'Round-trip equals original');

#`(
If, on the other hand, we want perform more complex transformations; better to
load it into a native data structure which will then allow us to manipulate it
however we like.
)

my @forest = $tree.comb( /^^\S.+? <?before $ | ^^\S>/);
my $forest;
{
    my $last = -1;

    for @forest -> $tree {
        for $tree.lines -> $line {
            $line ~~ /^^ ($($level))* /;
            my $this;
            $forest ~= do {
                given ($this = +$0) cmp $last {
                    when More { "\['{$line.trim}', " }
                    when Same { "'{$line.trim}', " }
                    when Less { "{']' x $last - $this}, '{$line.trim}', " }
                }
            }
            $last = $this;
        }
    }

    $forest ~= ']' x 1 + $last;
    use MONKEY-SEE-NO-EVAL;
    $forest.=EVAL;
}

say "\nNative data structure:\n", $forest.perl;

{
    use JSON::Fast;
    say "\nJSON:\n", $forest.&to-json;
}

{
    use YAML;
    say "\nYAML:\n", $forest.&dump;
}
```

```txt
RosettaCode
  encourages
    code
      diversity
      comparison
  discourages
    golfing
    trolling
    emphasising execution speed
code-golf.io
  encourages
    golfing
  discourages
    comparison

0 RosettaCode
1 encourages
2 code
3 diversity
3 comparison
1 discourages
2 golfing
2 trolling
2 emphasising execution speed
0 code-golf.io
1 encourages
2 golfing
1 discourages
2 comparison

RosettaCode
  encourages
    code
      diversity
      comparison
  discourages
    golfing
    trolling
    emphasising execution speed
code-golf.io
  encourages
    golfing
  discourages
    comparison

ok 1 - Round-trip equals original

Native data structure:
$["RosettaCode", ["encourages", ["code", ["diversity", "comparison"]], "discourages", ["golfing", "trolling", "emphasising execution speed"]], "code-golf.io", ["encourages", ["golfing"], "discourages", ["comparison"]]]

JSON:
[
  "RosettaCode",
  [
    "encourages",
    [
      "code",
      [
        "diversity",
        "comparison"
      ]
    ],
    "discourages",
    [
      "golfing",
      "trolling",
      "emphasising execution speed"
    ]
  ],
  "code-golf.io",
  [
    "encourages",
    [
      "golfing"
    ],
    "discourages",
    [
      "comparison"
    ]
  ]
]

YAML:
---
- RosettaCode
- - encourages
  - - code
    - - diversity
      - comparison
  - discourages
  - - golfing
    - trolling
    - emphasising execution speed
- code-golf.io
- - encourages
  - - golfing
  - discourages
  - - comparison
...
```



## Python

Just arranges the standard lists and tuples for the datastructures allowing pprint to show the different arrangement of storage.


```python
from pprint import pprint as pp

def to_indent(node, depth=0, flat=None):
    if flat is None:
        flat = []
    if node:
        flat.append((depth, node[0]))
    for child in node[1]:
        to_indent(child, depth + 1, flat)
    return flat

def to_nest(lst, depth=0, level=None):
    if level is None:
        level = []
    while lst:
        d, name = lst[0]
        if d == depth:
            children = []
            level.append((name, children))
            lst.pop(0)
        elif d > depth:  # down
            to_nest(lst, d, children)
        elif d < depth:  # up
            return
    return level[0] if level else None
                    
if __name__ == '__main__':
    print('Start Nest format:')
    nest = ('RosettaCode', [('rocks', [('code', []), ('comparison', []), ('wiki', [])]), 
                            ('mocks', [('trolling', [])])])
    pp(nest, width=25)

    print('\n... To Indent format:')
    as_ind = to_indent(nest)
    pp(as_ind, width=25)

    print('\n... To Nest format:')
    as_nest = to_nest(as_ind)
    pp(as_nest, width=25)

    if nest != as_nest:
        print("Whoops round-trip issues")
```


```txt
Start Nest format:
('RosettaCode',
 [('rocks',
   [('code', []),
    ('comparison', []),
    ('wiki', [])]),
  ('mocks',
   [('trolling', [])])])

... To Indent format:
[(0, 'RosettaCode'),
 (1, 'rocks'),
 (2, 'code'),
 (2, 'comparison'),
 (2, 'wiki'),
 (1, 'mocks'),
 (2, 'trolling')]

... To Nest format:
('RosettaCode',
 [('rocks',
   [('code', []),
    ('comparison', []),
    ('wiki', [])]),
  ('mocks',
   [('trolling', [])])])
```



## zkl


```zkl
fcn nestToIndent(nestTree){
   fcn(out,node,level){
      out.append(List(level,node[0]));	// (n,name) or ("..",name)
      if(node.len()>1){		// (name children), (name, (tree))
	 level+=1;
	 foreach child in (node[1,*]){ 
	    if(String.isType(child)) out.append(List(level,child));
	    else self.fcn(out,child,level)
	 }
      }
      out
   }(List(),nestTree,0)
}
fcn nestToString(nestTree,dot="."){
   fcn(out,dot,node,level){
      out.writeln(dot*level,node[0]);	// (name)
      if(node.len()>1){			// (name children), (name, (tree))
	 level+=1;
	 foreach child in (node[1,*]){ 
	    if(String.isType(child)) out.writeln(dot*level,child);
	    else self.fcn(out,dot,child,level)
	 }
      }
      out
   }(Data(),dot,nestTree,0).text
}

fcn indentToNest(iTree,depth=0,nTree=List()){
   while(iTree){	// (n,name)
      d, name := iTree[0];
      if(d==depth){
         nTree.append(name);
	 iTree.pop(0);
      }
      else if(d>depth){		// assume can't skip levels down
	 if(nTree.len()>1 and not List.isType((nm:=nTree[-1]))){
	    nTree[-1]=(children:=List(nm));
	    indentToNest(iTree,d,children);
	 }else{
	    nTree.append(children:=List(name));
	    iTree.pop(0);
	    indentToNest(iTree,d+1,children);
	 }
      }
      else break;  // d<depth
   }
   return(nTree)
}
fcn indentToString(indentTree){ indentTree.apply("concat"," ").concat("\n") }
```


```zkl
tree:=L("RosettaCode",
         L("rocks","code","comparison","wiki"),
	 L("mocks","golfing")  );

println("Nest tree internal format:\n",tree.toString(*,*));
println("Formated:\n",nestToString(tree));

indentTree:=nestToIndent(tree);
println("To indent format:\n",indentToString(indentTree));

nestTree:=indentToNest(indentTree);
println("\nIndent to nested format:\n",nestTree);
println("Is this tree the same as what we started with? ",nestTree==tree);
```

```txt

Nest tree internal format:
L("RosettaCode",L("rocks","code","comparison","wiki"),L("mocks","golfing"))
Formated:
RosettaCode
.rocks
..code
..comparison
..wiki
.mocks
..golfing

To indent format:
0 RosettaCode
1 rocks
2 code
2 comparison
2 wiki
1 mocks
2 golfing

Indent to nested format:
L("RosettaCode",L("rocks","code","comparison","wiki"),L("mocks","golfing"))
Is this tree the same as what we started with? True

```

I'm choosing to only allow one root per tree/forest so the Perl6 example is coded differently:

```zkl
perl6trees:=L(
   L("RosettaCode",
     L("encourages",
       L("code",
	 "diversity","comparison")),
     L("discourages",
       "golfing","trolling","emphasising execution speed"),
   ),
   L("code-golf.io",
     L("encourages","golfing"),
     L("discourages","comparison"),
   )
);
println(perl6trees.apply(nestToString).concat());
iTrees := perl6trees.apply(nestToIndent);
println(iTrees.apply(indentToString).concat("\n"));
(iTrees.apply(indentToNest)==perl6trees).println();
```

<pre style="height:40ex">
RosettaCode
  encourages
    code
      diversity
      comparison
  discourages
    golfing
    trolling
    emphasising execution speed
code-golf.io
  encourages
    golfing
  discourages
    comparison

0 RosettaCode
1 encourages
2 code
3 diversity
3 comparison
1 discourages
2 golfing
2 trolling
2 emphasising execution speed
0 code-golf.io
1 encourages
2 golfing
1 discourages
2 comparison
True

```

