+++
title = "Functional coverage tree"
description = ""
date = 2019-10-15T18:26:41Z
aliases = []
[extra]
id = 19505
[taxonomies]
categories = ["Data structures", "Tree structures", "task"]
tags = []
languages = [
  "go",
  "haskell",
  "j",
  "javascript",
  "julia",
  "kotlin",
  "perl",
  "python",
  "racket",
]
+++

## Task
Functional coverage is a measure of how much a particular function of a system 
has been verified as correct. It is used heavily in tracking the completeness 
of the verification of complex System on Chip (SoC) integrated circuits, where 
it can also be used to track how well the functional ''requirements'' of the 
system have been verified.

This task uses a sub-set of the calculations sometimes used in tracking 
functional coverage but uses a more familiar(?) scenario.

### Task Description
The head of the clean-up crews for "The Men in a very dark shade of grey when 
viewed at night" has been tasked with managing the cleansing of two properties
after an incident involving aliens.

She arranges the task hierarchically with a manager for the crews working on 
each house who return with a breakdown of how they will report on progress in 
each house.

The overall hierarchy of (sub)tasks is as follows, 

```txt
cleaning
    house1
        bedrooms
        bathrooms
            bathroom1
            bathroom2
            outside lavatory
        attic
        kitchen
        living rooms
            lounge
            dining room
            conservatory
            playroom
        basement
        garage
        garden
    house2
        upstairs
            bedrooms
                suite 1
                suite 2
                bedroom 3
                bedroom 4
            bathroom
            toilet
            attics
        groundfloor
            kitchen
            living rooms
                lounge
                dining room
                conservatory
                playroom
            wet room & toilet
            garage
            garden
            hot tub suite
        basement
            cellars
            wine cellar
            cinema
```


The head of cleanup knows that her managers will report fractional completion of leaf tasks (tasks with no child tasks of their own), and she knows that she will want to modify the weight of values of completion as she sees fit.

Some time into the cleaning, and some coverage reports have come in and she thinks see needs to weight the big house2 60-40 with respect to coverage from house1 She prefers a tabular view of her data where missing weights are assumed to be 1.0 and missing coverage 0.0.

```txt
NAME_HIERARCHY                  |WEIGHT  |COVERAGE  |
cleaning                        |        |          |
    house1                      |40      |          |
        bedrooms                |        |0.25      |
        bathrooms               |        |          |
            bathroom1           |        |0.5       |
            bathroom2           |        |          |
            outside_lavatory    |        |1         |
        attic                   |        |0.75      |
        kitchen                 |        |0.1       |
        living_rooms            |        |          |
            lounge              |        |          |
            dining_room         |        |          |
            conservatory        |        |          |
            playroom            |        |1         |
        basement                |        |          |
        garage                  |        |          |
        garden                  |        |0.8       |
    house2                      |60      |          |
        upstairs                |        |          |
            bedrooms            |        |          |
                suite_1         |        |          |
                suite_2         |        |          |
                bedroom_3       |        |          |
                bedroom_4       |        |          |
            bathroom            |        |          |
            toilet              |        |          |
            attics              |        |0.6       |
        groundfloor             |        |          |
            kitchen             |        |          |
            living_rooms        |        |          |
                lounge          |        |          |
                dining_room     |        |          |
                conservatory    |        |          |
                playroom        |        |          |
            wet_room_&_toilet   |        |          |
            garage              |        |          |
            garden              |        |0.9       |
            hot_tub_suite       |        |1         |
        basement                |        |          |
            cellars             |        |1         |
            wine_cellar         |        |1         |
            cinema              |        |0.75      |
```


;Calculation:
The coverage of a node in the tree is calculated as the [[wp:Weighted arithmetic mean|weighted average]] of the coverage of its children evaluated bottom-upwards in the tree.

'''The task is to''' calculate the overall coverage of the cleaning task and display the coverage at all levels of the hierarchy on this page, in a manner that visually shows the hierarchy, weights and coverage of all nodes.

;Extra Credit:
After calculating the coverage for all nodes, one can also calculate the additional/delta top level coverage that would occur if any (sub)task were to be fully covered from its current fractional coverage. This is done by multiplying the extra coverage that could be gained <math>1-coverage</math> for any node, by the product of the `powers` of its parent nodes from the top down to the node.

The power of a direct child of any parent is given by the power of the parent multiplied by the weight of the child divided by the sum of the weights of all the direct children.

The pseudo code would be:

    method delta_calculation(this, power):
        sum_of_weights = sum(node.weight for node in children)
        this.delta  = (1 - this.coverage) * power
        for node in self.children:
            node.delta_calculation(power * node.weight / sum_of_weights)
        return this.delta

Followed by a call to:

    top.delta_calculation(power=1)



'''Note:''' to aid in getting the data into your program you might want to use an alternative, more functional description of the starting data given on the discussion page.


## Go

{{trans|Kotlin}}

```go
package main

import "fmt"

type FCNode struct {
    name     string
    weight   int
    coverage float64
    children []*FCNode
    parent   *FCNode
}

func newFCN(name string, weight int, coverage float64) *FCNode {
    return &FCNode{name, weight, coverage, nil, nil}
}

func (n *FCNode) addChildren(nodes []*FCNode) {
    for _, node := range nodes {
        node.parent = n
        n.children = append(n.children, node)
    }
    n.updateCoverage()
}

func (n *FCNode) setCoverage(value float64) {
    if n.coverage != value {
        n.coverage = value
        // update any parent's coverage
        if n.parent != nil {
            n.parent.updateCoverage()
        }
    }
}

func (n *FCNode) updateCoverage() {
    v1 := 0.0
    v2 := 0
    for _, node := range n.children {
        v1 += float64(node.weight) * node.coverage
        v2 += node.weight
    }
    n.setCoverage(v1 / float64(v2))
}

func (n *FCNode) show(level int) {
    indent := level * 4
    nl := len(n.name) + indent
    fmt.Printf("%*s%*s  %3d   | %8.6f |\n", nl, n.name, 32-nl, "|", n.weight, n.coverage)
    if len(n.children) == 0 {
        return
    }
    for _, child := range n.children {
        child.show(level + 1)
    }
}

var houses = []*FCNode{
    newFCN("house1", 40, 0),
    newFCN("house2", 60, 0),
}

var house1 = []*FCNode{
    newFCN("bedrooms", 1, 0.25),
    newFCN("bathrooms", 1, 0),
    newFCN("attic", 1, 0.75),
    newFCN("kitchen", 1, 0.1),
    newFCN("living_rooms", 1, 0),
    newFCN("basement", 1, 0),
    newFCN("garage", 1, 0),
    newFCN("garden", 1, 0.8),
}

var house2 = []*FCNode{
    newFCN("upstairs", 1, 0),
    newFCN("groundfloor", 1, 0),
    newFCN("basement", 1, 0),
}

var h1Bathrooms = []*FCNode{
    newFCN("bathroom1", 1, 0.5),
    newFCN("bathroom2", 1, 0),
    newFCN("outside_lavatory", 1, 1),
}

var h1LivingRooms = []*FCNode{
    newFCN("lounge", 1, 0),
    newFCN("dining_room", 1, 0),
    newFCN("conservatory", 1, 0),
    newFCN("playroom", 1, 1),
}

var h2Upstairs = []*FCNode{
    newFCN("bedrooms", 1, 0),
    newFCN("bathroom", 1, 0),
    newFCN("toilet", 1, 0),
    newFCN("attics", 1, 0.6),
}

var h2Groundfloor = []*FCNode{
    newFCN("kitchen", 1, 0),
    newFCN("living_rooms", 1, 0),
    newFCN("wet_room_&_toilet", 1, 0),
    newFCN("garage", 1, 0),
    newFCN("garden", 1, 0.9),
    newFCN("hot_tub_suite", 1, 1),
}

var h2Basement = []*FCNode{
    newFCN("cellars", 1, 1),
    newFCN("wine_cellar", 1, 1),
    newFCN("cinema", 1, 0.75),
}

var h2UpstairsBedrooms = []*FCNode{
    newFCN("suite_1", 1, 0),
    newFCN("suite_2", 1, 0),
    newFCN("bedroom_3", 1, 0),
    newFCN("bedroom_4", 1, 0),
}

var h2GroundfloorLivingRooms = []*FCNode{
    newFCN("lounge", 1, 0),
    newFCN("dining_room", 1, 0),
    newFCN("conservatory", 1, 0),
    newFCN("playroom", 1, 0),
}

func main() {
    cleaning := newFCN("cleaning", 1, 0)

    house1[1].addChildren(h1Bathrooms)
    house1[4].addChildren(h1LivingRooms)
    houses[0].addChildren(house1)

    h2Upstairs[0].addChildren(h2UpstairsBedrooms)
    house2[0].addChildren(h2Upstairs)
    h2Groundfloor[1].addChildren(h2GroundfloorLivingRooms)
    house2[1].addChildren(h2Groundfloor)
    house2[2].addChildren(h2Basement)
    houses[1].addChildren(house2)

    cleaning.addChildren(houses)
    topCoverage := cleaning.coverage
    fmt.Printf("TOP COVERAGE = %8.6f\n\n", topCoverage)
    fmt.Println("NAME HIERARCHY                 | WEIGHT | COVERAGE |")
    cleaning.show(0)

    h2Basement[2].setCoverage(1) // change Cinema node coverage to 1
    diff := cleaning.coverage - topCoverage
    fmt.Println("\nIf the coverage of the Cinema node were increased from 0.75 to 1")
    fmt.Print("the top level coverage would increase by ")
    fmt.Printf("%8.6f to %8.6f\n", diff, topCoverage+diff)
    h2Basement[2].setCoverage(0.75) // restore to original value if required
}
```


{{out}}

```txt

TOP COVERAGE = 0.409167

NAME HIERARCHY                 | WEIGHT | COVERAGE |
cleaning                       |    1   | 0.409167 |
    house1                     |   40   | 0.331250 |
        bedrooms               |    1   | 0.250000 |
        bathrooms              |    1   | 0.500000 |
            bathroom1          |    1   | 0.500000 |
            bathroom2          |    1   | 0.000000 |
            outside_lavatory   |    1   | 1.000000 |
        attic                  |    1   | 0.750000 |
        kitchen                |    1   | 0.100000 |
        living_rooms           |    1   | 0.250000 |
            lounge             |    1   | 0.000000 |
            dining_room        |    1   | 0.000000 |
            conservatory       |    1   | 0.000000 |
            playroom           |    1   | 1.000000 |
        basement               |    1   | 0.000000 |
        garage                 |    1   | 0.000000 |
        garden                 |    1   | 0.800000 |
    house2                     |   60   | 0.461111 |
        upstairs               |    1   | 0.150000 |
            bedrooms           |    1   | 0.000000 |
                suite_1        |    1   | 0.000000 |
                suite_2        |    1   | 0.000000 |
                bedroom_3      |    1   | 0.000000 |
                bedroom_4      |    1   | 0.000000 |
            bathroom           |    1   | 0.000000 |
            toilet             |    1   | 0.000000 |
            attics             |    1   | 0.600000 |
        groundfloor            |    1   | 0.316667 |
            kitchen            |    1   | 0.000000 |
            living_rooms       |    1   | 0.000000 |
                lounge         |    1   | 0.000000 |
                dining_room    |    1   | 0.000000 |
                conservatory   |    1   | 0.000000 |
                playroom       |    1   | 0.000000 |
            wet_room_&_toilet  |    1   | 0.000000 |
            garage             |    1   | 0.000000 |
            garden             |    1   | 0.900000 |
            hot_tub_suite      |    1   | 1.000000 |
        basement               |    1   | 0.916667 |
            cellars            |    1   | 1.000000 |
            wine_cellar        |    1   | 1.000000 |
            cinema             |    1   | 0.750000 |

If the coverage of the Cinema node were increased from 0.75 to 1
the top level coverage would increase by 0.016667 to 0.425833

```



## Haskell

Using a function from a text outline to an updated text outline.


The raw table (supplied in the task description) is read in from a text file, parsed to a tree structure, and updated by two traversals (one bottom-up and one top down) before being serialised back to a completed outline text, with an additional 'Share of Residue' column:
{{Trans|Python}}

```haskell
{-# LANGUAGE OverloadedStrings #-}

import System.Directory (doesFileExist)
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Arrow ((&&&), first)
import Numeric (showFFloat)
import Data.Char (isSpace)
import Data.Bool (bool)
import Data.Tree

data Coverage = Coverage
  { name :: T.Text
  , weight :: Float
  , coverage :: Float
  , share :: Float
  } deriving (Show)

-- TEST ---------------------------------------------------
fp = "./coverageOutline.txt"

main :: IO ()
main =
  doesFileExist fp >>=
  bool
    (print $ "File not found: " ++ fp)
    (T.readFile fp >>= T.putStrLn . updatedCoverageOutline)

-- UPDATED COVERAGE OUTLINE -------------------------------
updatedCoverageOutline :: T.Text -> T.Text
updatedCoverageOutline s =
  let delimiter = "|"
      indentedLines = T.lines s
      columnNames = init $ tokenizeWith delimiter (head indentedLines)
  in T.unlines
       [ tabulation delimiter (columnNames ++ ["SHARE OF RESIDUE"])
       , indentedLinesFromTree "    " (showCoverage delimiter) $
         withResidueShares 1.0 $
         foldTree
           weightedCoverage
           (parseTreeFromOutline delimiter indentedLines)
       ]

-- WEIGHTED COVERAGE AND SHARES OF REMAINING WORK ---------
weightedCoverage :: Coverage -> Forest Coverage -> Tree Coverage
weightedCoverage x xs =
  let cws = ((coverage &&& weight) . rootLabel) <$> xs
      totalWeight = foldr ((+) . snd) 0 cws
  in Node
       (x
        { coverage =
          foldr (\(c, w) a -> (c * w) + a) (coverage x) cws /
          bool 1 totalWeight (0 < totalWeight)
        })
       xs

withResidueShares :: Float -> Tree Coverage -> Tree Coverage
withResidueShares shareOfTotal tree =
  let go fraction node =
        let forest = subForest node
            weights = (weight . rootLabel) <$> forest
            weightTotal = sum weights
            nodeRoot = rootLabel node
        in Node
             (nodeRoot
              { share = fraction * (1 - coverage nodeRoot)
              })
             (zipWith go (((fraction *) . (/ weightTotal)) <$> weights) forest)
  in go shareOfTotal tree

-- OUTLINE PARSE ------------------------------------------
parseTreeFromOutline :: T.Text -> [T.Text] -> Tree Coverage
parseTreeFromOutline delimiter indentedLines =
  (partialRecord . tokenizeWith delimiter) <$>
  head (forestFromLineIndents $ indentLevelsFromLines $ tail indentedLines)

forestFromLineIndents :: [(Int, T.Text)] -> [Tree T.Text]
forestFromLineIndents pairs =
  let go [] = []
      go ((n, s):xs) =
        let (firstTreeLines, rest) = span ((n <) . fst) xs
        in Node s (go firstTreeLines) : go rest
  in go pairs

indentLevelsFromLines :: [T.Text] -> [(Int, T.Text)]
indentLevelsFromLines xs =
  let pairs = T.span isSpace <$> xs
      indentUnit =
        foldr
          (\x a ->
              let w = (T.length . fst) x
              in bool a w (w < a && 0 < w))
          (maxBound :: Int)
          pairs
  in first (flip div indentUnit . T.length) <$> pairs

partialRecord :: [T.Text] -> Coverage
partialRecord xs =
  let [name, weightText, coverageText] = take 3 (xs ++ repeat "")
  in Coverage
     { name = name
     , weight = defaultOrRead 1.0 weightText
     , coverage = defaultOrRead 0.0 coverageText
     , share = 0.0
     }

defaultOrRead :: Float -> T.Text -> Float
defaultOrRead n txt = either (const n) fst $ T.rational txt

tokenizeWith :: T.Text -> T.Text -> [T.Text]
tokenizeWith delimiter = fmap T.strip . T.splitOn delimiter

-- SERIALISATION OF TREE TO TABULATED OUTLINE -------------
indentedLinesFromTree :: T.Text -> (T.Text -> a -> T.Text) -> Tree a -> T.Text
indentedLinesFromTree tab showRoot tree =
  let go indent node =
        showRoot indent (rootLabel node) :
        (subForest node >>= go (T.append tab indent))
  in T.unlines $ go "" tree

showCoverage :: T.Text -> T.Text -> Coverage -> T.Text
showCoverage delimiter indent x =
  tabulation
    delimiter
    ([T.append indent (name x), T.pack (showN 0 (weight x))] ++
     ((T.pack . showN 4) <$> ([coverage, share] <*> [x])))

tabulation :: T.Text -> [T.Text] -> T.Text
tabulation delimiter =
  T.intercalate (T.append delimiter " ") .
  zipWith (`T.justifyLeft` ' ') [31, 9, 9, 9]

justifyRight :: Int -> a -> [a] -> [a]
justifyRight n c = (drop . length) <*> (replicate n c ++)

showN :: Int -> Float -> String
showN p n = justifyRight 7 ' ' (showFFloat (Just p) n "")

-- GENERIC ------------------------------------------------
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go
  where
    go (Node x ts) = f x (map go ts)
```

{{Out}}

```txt
NAME_HIERARCHY                 | WEIGHT   | COVERAGE | SHARE OF RESIDUE
cleaning                       |       1  |  0.4092  |  0.5908  
    house1                     |      40  |  0.3312  |  0.2675  
        bedrooms               |       1  |  0.2500  |  0.0375  
        bathrooms              |       1  |  0.5000  |  0.0250  
            bathroom1          |       1  |  0.5000  |  0.0083  
            bathroom2          |       1  |  0.0000  |  0.0167  
            outside_lavatory   |       1  |  1.0000  |  0.0000  
        attic                  |       1  |  0.7500  |  0.0125  
        kitchen                |       1  |  0.1000  |  0.0450  
        living_rooms           |       1  |  0.2500  |  0.0375  
            lounge             |       1  |  0.0000  |  0.0125  
            dining_room        |       1  |  0.0000  |  0.0125  
            conservatory       |       1  |  0.0000  |  0.0125  
            playroom           |       1  |  1.0000  |  0.0000  
        basement               |       1  |  0.0000  |  0.0500  
        garage                 |       1  |  0.0000  |  0.0500  
        garden                 |       1  |  0.8000  |  0.0100  
    house2                     |      60  |  0.4611  |  0.3233  
        upstairs               |       1  |  0.1500  |  0.1700  
            bedrooms           |       1  |  0.0000  |  0.0500  
                suite_1        |       1  |  0.0000  |  0.0125  
                suite_2        |       1  |  0.0000  |  0.0125  
                bedroom_3      |       1  |  0.0000  |  0.0125  
                bedroom_4      |       1  |  0.0000  |  0.0125  
            bathroom           |       1  |  0.0000  |  0.0500  
            toilet             |       1  |  0.0000  |  0.0500  
            attics             |       1  |  0.6000  |  0.0200  
        groundfloor            |       1  |  0.3167  |  0.1367  
            kitchen            |       1  |  0.0000  |  0.0333  
            living_rooms       |       1  |  0.0000  |  0.0333  
                lounge         |       1  |  0.0000  |  0.0083  
                dining_room    |       1  |  0.0000  |  0.0083  
                conservatory   |       1  |  0.0000  |  0.0083  
                playroom       |       1  |  0.0000  |  0.0083  
            wet_room_&_toilet  |       1  |  0.0000  |  0.0333  
            garage             |       1  |  0.0000  |  0.0333  
            garden             |       1  |  0.9000  |  0.0033  
            hot_tub_suite      |       1  |  1.0000  |  0.0000  
        basement               |       1  |  0.9167  |  0.0167  
            cellars            |       1  |  1.0000  |  0.0000  
            wine_cellar        |       1  |  1.0000  |  0.0000  
            cinema             |       1  |  0.7500  |  0.0167  
```





## J


Implementation (raw data):


```J
raw=: 0 :0
NAME_HIERARCHY                  |WEIGHT  |COVERAGE  |
cleaning                        |        |          |
    house1                      |40      |          |
        bedrooms                |        |0.25      |
        bathrooms               |        |          |
            bathroom1           |        |0.5       |
            bathroom2           |        |          |
            outside_lavatory    |        |1         |
        attic                   |        |0.75      |
        kitchen                 |        |0.1       |
        living_rooms            |        |          |
            lounge              |        |          |
            dining_room         |        |          |
            conservatory        |        |          |
            playroom            |        |1         |
        basement                |        |          |
        garage                  |        |          |
        garden                  |        |0.8       |
    house2                      |60      |          |
        upstairs                |        |          |
            bedrooms            |        |          |
                suite_1         |        |          |
                suite_2         |        |          |
                bedroom_3       |        |          |
                bedroom_4       |        |          |
            bathroom            |        |          |
            toilet              |        |          |
            attics              |        |0.6       |
        groundfloor             |        |          |
            kitchen             |        |          |
            living_rooms        |        |          |
                lounge          |        |          |
                dining_room     |        |          |
                conservatory    |        |          |
                playroom        |        |          |
            wet_room_&_toilet   |        |          |
            garage              |        |          |
            garden              |        |0.9       |
            hot_tub_suite       |        |1         |
        basement                |        |          |
            cellars             |        |1         |
            wine_cellar         |        |1         |
            cinema              |        |0.75      |
)
```


Implementation (unpacking raw data):


```J
labels=: {.<;._2;._2 raw
'hier wspec cspec'=:|:}.<;._2;._2 raw
level=: (%+./) (0 i.~' '&=)"1 hier
weight=: (+ 0=]) ,".wspec
coverage=:  ,".cspec
```


To understand this implementation, it's best to run it and inspect the data. 

That said: 

each of the above names is a column variable (with one value for each row in the dataset).

level has values 0, 1, 2, 3 or 4 (depending on depth of indent), and the calculation relies on each indent level using the same number of spaces. It might be smarter to use <code>level=: (i.~ ~.) (0 i.~' '&=)"1 hier</code> which only relies on consistent indentation at each level, but ultimately a general case implementation would have to enforce an indentation standard and that sort of mechanism is out of scope for this task.

weight fills in the blanks for weights (1 if not otherwise specified). This calculation is simplified because we do not have to worry about any explicitly 0 weights.

coverage fills in the blanks for coverage (0 if not otherwise specified).

Implementation (translation of leaf coverage to functional coverage):


```J
merge=: ;@(({.@[,(+}.)~)&.> [: +/\1,_1}.#@>)
unrooted=: ([: merge <@(_1,$:@}.);.1)^:(0<#)
parent=:  unrooted level
parent_cover=: (] (1}.~.parent)}~ 1}. * %&(parent +//. ]) [)^:_
```


<code>unrooted</code> translates indentation information to a [[Tree_traversal#J:_Alternate_implementation|parent tree structure]]. However, the limitations of recursion require we distinguish the parent node from its children, so we use _1 to denote the parent node of the recursive intermediate result unrooted trees. (This works well with using arithmetic to adjust sub-tree indices based on the lengths of preceding sub-trees.) <code>merge</code> combines a boxed sequence of these subtrees to form a single tree - we also rely on the first node of each tree being both _1 and the root node.

Thus, <code>parent_cover</code> propagates coverage to parent nodes based on the weighted average of coverage at the children.

Task example (format and show result):


```J
   1 1 }._1 }.":labels,each ":each hier;(,.weight);,.weight parent_cover coverage
NAME_HIERARCHY                  │WEIGHT  │COVERAGE  │
cleaning                        │ 1      │0.409167  │
    house1                      │40      │ 0.33125  │
        bedrooms                │ 1      │    0.25  │
        bathrooms               │ 1      │     0.5  │
            bathroom1           │ 1      │     0.5  │
            bathroom2           │ 1      │       0  │
            outside_lavatory    │ 1      │       1  │
        attic                   │ 1      │    0.75  │
        kitchen                 │ 1      │     0.1  │
        living_rooms            │ 1      │    0.25  │
            lounge              │ 1      │       0  │
            dining_room         │ 1      │       0  │
            conservatory        │ 1      │       0  │
            playroom            │ 1      │       1  │
        basement                │ 1      │       0  │
        garage                  │ 1      │       0  │
        garden                  │ 1      │     0.8  │
    house2                      │60      │0.461111  │
        upstairs                │ 1      │    0.15  │
            bedrooms            │ 1      │       0  │
                suite_1         │ 1      │       0  │
                suite_2         │ 1      │       0  │
                bedroom_3       │ 1      │       0  │
                bedroom_4       │ 1      │       0  │
            bathroom            │ 1      │       0  │
            toilet              │ 1      │       0  │
            attics              │ 1      │     0.6  │
        groundfloor             │ 1      │0.316667  │
            kitchen             │ 1      │       0  │
            living_rooms        │ 1      │       0  │
                lounge          │ 1      │       0  │
                dining_room     │ 1      │       0  │
                conservatory    │ 1      │       0  │
                playroom        │ 1      │       0  │
            wet_room_&_toilet   │ 1      │       0  │
            garage              │ 1      │       0  │
            garden              │ 1      │     0.9  │
            hot_tub_suite       │ 1      │       1  │
        basement                │ 1      │0.916667  │
            cellars             │ 1      │       1  │
            wine_cellar         │ 1      │       1  │
            cinema              │ 1      │    0.75  │
```


Extra credit:


```J
trace=: (~.@,each  (0 >. parent)&{)^:_  i.#parent
power=: */@:{&(parent (] % (i.~ ~.)@[ { +//.) weight)@> trace

   power*1-weight parent_cover coverage
0.590833 0.2675 0.0375 0.025 0.00833333 0.0166667 0 0.0125 0.045 0.0375 0.0125 0.0125 0.0125 0 0.05 0.05 0.01 0.323333 0.17 0.05 0.0125 0.0125 0.0125 0.0125 0.05 0.05 0.02 0.136667 0.0333333 0.0333333 0.00833333 0.00833333 0.00833333 0.00833333 0.0333333 0.0333333 0.00333333 0 0.0166667 0 0 0.0166667
```


Explanation:

<code>trace</code> is, for each node, the set of nodes (or indices of nodes - since we use indices to identify nodes) leading from that node to its root.

<code>parent (] % (i.~ ~.)@[ { +//.) weight</code> is the weight of each node divided by the total weight for all nodes with the same parent.

<code>power</code> is the product of these relative weights for each member of the trace.

And, <code>weight parent_cover coverage</code> was the functional coverage for each node.



## JavaScript

Parsing the outline text to a tree structure, and traversing this with two computations (one bottom-up, and one top-down), before serialising the updated tree to a new outline text.

{{Trans|Haskell}}
{{Trans|Python}}

```javascript
(() => {
    'use strict';

    // updatedCoverageOutline :: String -> String
    const updatedCoverageOutline = outlineText => {
        const
            delimiter = '|',
            indentedLines = indentLevelsFromLines(lines(outlineText)),
            columns = init(tokenizeWith(delimiter)(snd(indentedLines[0])));

        // SERIALISATION OF UPDATED PARSE TREE (TO NEW OUTLINE TEXT)
        return tabulation(delimiter)(
            columns.concat('SHARE OF RESIDUE\n')
        ) + unlines(
            indentedLinesFromTree(
                showCoverage(delimiter))('    ')(

                // TWO TRAVERSAL COMPUTATIONS

                withResidueShares(1.0)(
                    foldTree(weightedCoverage)(

                        // PARSE TREE (FROM OUTLINE TEXT)
                        fmapTree(compose(
                            partialRecord, tokenizeWith(delimiter)
                        ))(fst(
                            forestFromLineIndents(tail(indentedLines))
                        ))
                    )
                ))
        );
    };

    // TEST -----------------------------------------------
    // main :: IO ()
    const main = () =>
        console.log(
            // strOutline is included as literal text
            // at the foot of this code listing.
            updatedCoverageOutline(strOutline)
        );

    // COVERAGE AND SHARES OF RESIDUE ---------------------

    // weightedCoverage :: Dict -> Forest Dict -> Tree Dict
    const weightedCoverage = x => xs => {
        const
            cws = map(compose(
                fanArrow(x => x.coverage)(x => x.weight),
                root
            ))(xs),
            totalWeight = cws.reduce((a, tpl) => a + snd(tpl), 0);
        return Node(
            insertDict('coverage')(
                cws.reduce((a, tpl) => {
                    const [c, w] = Array.from(tpl);
                    return a + (c * w);
                }, x.coverage) / (
                    0 < totalWeight ? totalWeight : 1
                )
            )(x)
        )(xs);
    };


    // withResidueShares :: Float -> Tree Dict -> Tree Dict
    const withResidueShares = shareOfTotal => tree => {
        const go = fraction => node => {
            const
                nodeRoot = node.root,
                forest = node.nest,
                weights = forest.map(x => x.root.weight),
                weightTotal = sum(weights);
            return Node(
                insertDict('share')(
                    fraction * (1 - nodeRoot.coverage)
                )(nodeRoot)
            )(
                zipWith(go)(
                    weights.map(w => fraction * (w / weightTotal))
                )(forest)
            );
        };
        return go(shareOfTotal)(tree);
    };


    // OUTLINE PARSED TO TREE -----------------------------

    // forestFromLineIndents :: [(Int, String)] -> [Tree String]
    const forestFromLineIndents = tuples => {
        const go = xs =>
            0 < xs.length ? (() => {
                const [n, s] = Array.from(xs[0]);
                // Lines indented under this line,
                // tupled with all the rest.
                const [firstTreeLines, rest] = Array.from(
                    span(x => n < x[0])(xs.slice(1))
                );
                // This first tree, and then the rest.
                return [Node(s)(go(firstTreeLines))]
                    .concat(go(rest));
            })() : [];
        return go(tuples);
    };

    // indentLevelsFromLines :: [String] -> [(Int, String)]
    const indentLevelsFromLines = xs => {
        const
            indentTextPairs = xs.map(compose(
                firstArrow(length), span(isSpace)
            )),
            indentUnit = minimum(indentTextPairs.flatMap(pair => {
                const w = fst(pair);
                return 0 < w ? [w] : [];
            }));
        return indentTextPairs.map(
            firstArrow(flip(div)(indentUnit))
        );
    };

    // partialRecord :: [String] -> Dict
    const partialRecord = xs => {
        const [name, weightText, coverageText] = take(3)(
            xs.concat(['', '', ''])
        );
        return {
            name: name || '?',
            weight: parseFloat(weightText) || 1.0,
            coverage: parseFloat(coverageText) || 0.0,
            share: 0.0
        };
    };

    // tokenizeWith :: String -> String -> [String]
    const tokenizeWith = delimiter =>
        // A sequence of trimmed tokens obtained by
        // splitting s on the supplied delimiter.
        s => s.split(delimiter).map(x => x.trim());


    // TREE SERIALIZED TO OUTLINE -------------------------

    // indentedLinesFromTree :: (String -> a -> String) ->
    // String -> Tree a -> [String]
    const indentedLinesFromTree = showRoot =>
        strTab => tree => {
            const go = indent =>
                node => [showRoot(indent)(node.root)]
                .concat(node.nest.flatMap(go(strTab + indent)));
            return go('')(tree);
        };

    // showN :: Int -> Float -> String
    const showN = p =>
        n => justifyRight(7)(' ')(n.toFixed(p));

    // showCoverage :: String -> String -> Dict -> String
    const showCoverage = delimiter =>
        indent => x => tabulation(delimiter)(
            [indent + x.name, showN(0)(x.weight)]
            .concat([x.coverage, x.share].map(showN(4)))
        );

    // tabulation :: String -> [String] -> String
    const tabulation = delimiter =>
        // Up to 4 tokens drawn from the argument list,
        // as a single string with fixed left-justified
        // white-space widths, between delimiters.
        compose(
            intercalate(delimiter + ' '),
            zipWith(flip(justifyLeft)(' '))([31, 9, 9, 9])
        );


    // GENERIC AND REUSABLE FUNCTIONS ---------------------

    // Node :: a -> [Tree a] -> Tree a
    const Node = v => xs => ({
        type: 'Node',
        root: v, // any type of value (consistent across tree)
        nest: xs || []
    });

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = a => b => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (...fs) =>
        x => fs.reduceRight((a, f) => f(a), x);

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // div :: Int -> Int -> Int
    const div = x => y => Math.floor(x / y);

    // either :: (a -> c) -> (b -> c) -> Either a b -> c
    const either = fl => fr => e =>
        'Either' === e.type ? (
            undefined !== e.Left ? (
                fl(e.Left)
            ) : fr(e.Right)
        ) : undefined;

    // Compose a function from a simple value to a tuple of
    // the separate outputs of two different functions

    // fanArrow (&&&) :: (a -> b) -> (a -> c) -> (a -> (b, c))
    const fanArrow = f => g => x => Tuple(f(x))(
        g(x)
    );

    // Lift a simple function to one which applies to a tuple,
    // transforming only the first item of the tuple

    // firstArrow :: (a -> b) -> ((a, c) -> (b, c))
    const firstArrow = f => xy => Tuple(f(xy[0]))(
        xy[1]
    );

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f =>
        1 < f.length ? (
            (a, b) => f(b, a)
        ) : (x => y => f(y)(x));

    // fmapTree :: (a -> b) -> Tree a -> Tree b
    const fmapTree = f => tree => {
        const go = node => Node(f(node.root))(
            node.nest.map(go)
        );
        return go(tree);
    };

    // foldTree :: (a -> [b] -> b) -> Tree a -> b
    const foldTree = f => tree => {
        const go = node => f(node.root)(
            node.nest.map(go)
        );
        return go(tree);
    };

    // foldl1 :: (a -> a -> a) -> [a] -> a
    const foldl1 = f => xs =>
        1 < xs.length ? xs.slice(1)
        .reduce(uncurry(f), xs[0]) : xs[0];

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // init :: [a] -> [a]
    const init = xs =>
        0 < xs.length ? (
            xs.slice(0, -1)
        ) : undefined;

    // insertDict :: String -> a -> Dict -> Dict
    const insertDict = k => v => dct =>
        Object.assign({}, dct, {
            [k]: v
        });

    // intercalate :: [a] -> [[a]] -> [a]
    // intercalate :: String -> [String] -> String
    const intercalate = sep =>
        xs => xs.join(sep);

    // isSpace :: Char -> Bool
    const isSpace = c => /\s/.test(c);

    // justifyLeft :: Int -> Char -> String -> String
    const justifyLeft = n => cFiller => s =>
        n > s.length ? (
            s.padEnd(n, cFiller)
        ) : s;

    // justifyRight :: Int -> Char -> String -> String
    const justifyRight = n => cFiller => s =>
        n > s.length ? (
            s.padStart(n, cFiller)
        ) : s;

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // lines :: String -> [String]
    const lines = s => s.split(/[\r\n]/);

    // map :: (a -> b) -> [a] -> [b]
    const map = f => xs =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // minimum :: Ord a => [a] -> a
    const minimum = xs =>
        0 < xs.length ? (
            foldl1(a => x => x < a ? x : a)(xs)
        ) : undefined;

    // root :: Tree a -> a
    const root = tree => tree.root;

    // showLog :: a -> IO ()
    const showLog = (...args) =>
        console.log(
            args
            .map(JSON.stringify)
            .join(' -> ')
        );

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // span :: (a -> Bool) -> [a] -> ([a], [a])
    const span = p => xs => {
        const iLast = xs.length - 1;
        return splitAt(
            until(i => iLast < i || !p(xs[i]))(
                succ
            )(0)
        )(xs);
    };

    // splitAt :: Int -> [a] -> ([a], [a])
    const splitAt = n => xs =>
        Tuple(xs.slice(0, n))(
            xs.slice(n)
        );

    // succ :: Enum a => a -> a
    const succ = x =>
        1 + x;

    // sum :: [Num] -> Num
    const sum = xs =>
        xs.reduce((a, x) => a + x, 0);

    // tail :: [a] -> [a]
    const tail = xs =>
        0 < xs.length ? xs.slice(1) : [];

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = n => xs =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // uncurry :: (a -> b -> c) -> ((a, b) -> c)
    const uncurry = f =>
        (x, y) => f(x)(y);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = p => f => x => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = f => xs => ys =>
        xs.slice(
            0, Math.min(xs.length, ys.length)
        ).map((x, i) => f(x)(ys[i]));

    // SOURCE OUTLINE -----------------------------------------

    const strOutline = `NAME_HIERARCHY                  |WEIGHT  |COVERAGE  |
cleaning                        |        |          |
    house1                      |40      |          |
        bedrooms                |        |0.25      |
        bathrooms               |        |          |
            bathroom1           |        |0.5       |
            bathroom2           |        |          |
            outside_lavatory    |        |1         |
        attic                   |        |0.75      |
        kitchen                 |        |0.1       |
        living_rooms            |        |          |
            lounge              |        |          |
            dining_room         |        |          |
            conservatory        |        |          |
            playroom            |        |1         |
        basement                |        |          |
        garage                  |        |          |
        garden                  |        |0.8       |
    house2                      |60      |          |
        upstairs                |        |          |
            bedrooms            |        |          |
                suite_1         |        |          |
                suite_2         |        |          |
                bedroom_3       |        |          |
                bedroom_4       |        |          |
            bathroom            |        |          |
            toilet              |        |          |
            attics              |        |0.6       |
        groundfloor             |        |          |
            kitchen             |        |          |
            living_rooms        |        |          |
                lounge          |        |          |
                dining_room     |        |          |
                conservatory    |        |          |
                playroom        |        |          |
            wet_room_&_toilet   |        |          |
            garage              |        |          |
            garden              |        |0.9       |
            hot_tub_suite       |        |1         |
        basement                |        |          |
            cellars             |        |1         |
            wine_cellar         |        |1         |
            cinema              |        |0.75      |`;

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
NAME_HIERARCHY                 | WEIGHT   | COVERAGE | SHARE OF RESIDUE
cleaning                       |       1  |  0.4092  |  0.5908  
    house1                     |      40  |  0.3313  |  0.2675  
        bedrooms               |       1  |  0.2500  |  0.0375  
        bathrooms              |       1  |  0.5000  |  0.0250  
            bathroom1          |       1  |  0.5000  |  0.0083  
            bathroom2          |       1  |  0.0000  |  0.0167  
            outside_lavatory   |       1  |  1.0000  |  0.0000  
        attic                  |       1  |  0.7500  |  0.0125  
        kitchen                |       1  |  0.1000  |  0.0450  
        living_rooms           |       1  |  0.2500  |  0.0375  
            lounge             |       1  |  0.0000  |  0.0125  
            dining_room        |       1  |  0.0000  |  0.0125  
            conservatory       |       1  |  0.0000  |  0.0125  
            playroom           |       1  |  1.0000  |  0.0000  
        basement               |       1  |  0.0000  |  0.0500  
        garage                 |       1  |  0.0000  |  0.0500  
        garden                 |       1  |  0.8000  |  0.0100  
    house2                     |      60  |  0.4611  |  0.3233  
        upstairs               |       1  |  0.1500  |  0.1700  
            bedrooms           |       1  |  0.0000  |  0.0500  
                suite_1        |       1  |  0.0000  |  0.0125  
                suite_2        |       1  |  0.0000  |  0.0125  
                bedroom_3      |       1  |  0.0000  |  0.0125  
                bedroom_4      |       1  |  0.0000  |  0.0125  
            bathroom           |       1  |  0.0000  |  0.0500  
            toilet             |       1  |  0.0000  |  0.0500  
            attics             |       1  |  0.6000  |  0.0200  
        groundfloor            |       1  |  0.3167  |  0.1367  
            kitchen            |       1  |  0.0000  |  0.0333  
            living_rooms       |       1  |  0.0000  |  0.0333  
                lounge         |       1  |  0.0000  |  0.0083  
                dining_room    |       1  |  0.0000  |  0.0083  
                conservatory   |       1  |  0.0000  |  0.0083  
                playroom       |       1  |  0.0000  |  0.0083  
            wet_room_&_toilet  |       1  |  0.0000  |  0.0333  
            garage             |       1  |  0.0000  |  0.0333  
            garden             |       1  |  0.9000  |  0.0033  
            hot_tub_suite      |       1  |  1.0000  |  0.0000  
        basement               |       1  |  0.9167  |  0.0167  
            cellars            |       1  |  1.0000  |  0.0000  
            wine_cellar        |       1  |  1.0000  |  0.0000  
            cinema             |       1  |  0.7500  |  0.0167 
```



## Julia

Most implementations of functional coverage are going to store values in a database. The implementation
stores the tree in a CSV file with an index to the parent of each entry to allow reconstitution of the tree. 

```Julia
using CSV, DataFrames, Formatting

function updatecoverage(dfname, outputname)
    df = CSV.read(dfname)
    dchild = Dict{Int, Vector{Int}}([i => Int[] for i in 0:maximum(df[!, 1])])

    for row in eachrow(df)
        push!(dchild[row[3]], row[1])
    end

    function coverage(t)
        return dchild[t] == [] ? df[t, :COVERAGE] * df[t, :WEIGHT] :
            sum(coverage, dchild[t]) / sum(x -> df[x, :WEIGHT], dchild[t]) * df[t, :WEIGHT]
    end

    df[!, :COVERAGE] .= coverage.(df.NUMBER)

    function possibleincrease(t)
        if !isempty(dchild[t])
            return 0.0
        else
            newcoverage = deepcopy(df.COVERAGE)
            newcoverage[t] = 1.0
            oldcoverage = newcoverage[1]
            function potentialcoverage(t)
                return dchild[t] == [] ? newcoverage[t] * df[t, :WEIGHT] :
                    sum(potentialcoverage, dchild[t]) / sum(x -> df[x, :WEIGHT],
                        dchild[t]) * df[t, :WEIGHT]
            end

            newcoverage .= potentialcoverage.(df[!, 1])
            return newcoverage[1] - oldcoverage
        end
    end

    df.POTENTIAL = possibleincrease.(df[!, 1])

    CSV.write(outputname, df)
end

function displaycoveragedb(dfname)
    df = CSV.read(dfname)
    indentlevel(t) = (i = 0; while (j = df[t, 3]) != 0 i += 1; t = j end; i)
    indent1 = [indentlevel(i) for i in df.NUMBER]
    maxindent = maximum(indent1)
    indent2 = maxindent .- indent1
    showpot = size(df)[2] == 6
    println("INDEX       NAME_HIERARCHY                         WEIGHT      COVERAGE    (POTENTIAL INCREASE)")
    for (i, row) in enumerate(eachrow(df))
        println(rpad(row[1], 7), "       "^indent1[i], rpad(row[2], 20), "       "^indent2[i],
            rpad(row[4], 8), rpad(format(row[5]), 12), showpot && row[6] != 0 ? format(row[6]) : "")
    end
end

const dbname = "coverage.csv"
const newdbname = "coverageupdated.csv"

println("Input data:")
displaycoveragedb(dbname)
updatecoverage(dbname, newdbname)
println("\nUpdated data:")
displaycoveragedb(newdbname)

```
{{out}}

```txt

Input data:
INDEX       NAME_HIERARCHY                         WEIGHT      COVERAGE    (POTENTIAL INCREASE)
1      cleaning                                        1       0
2             house1                                   40      0
3                    bedrooms                          1       0.25
4                    bathrooms                         1       0
5                           bathroom1                  1       0.5
6                           bathroom2                  1       0
7                           outside_lavatory           1       1
8                    attic                             1       0.75
9                    kitchen                           1       0.1
10                   living_rooms                      1       0
11                          lounge                     1       0
12                          dining_room                1       0
13                          conservatory               1       0
14                          playroom                   1       1
15                   basement                          1       0
16                   garage                            1       0
17                   garden                            1       0.8
18            house2                                   60      0
19                   upstairs                          1       0
20                          bedrooms                   1       0
21                                 suite_1             1       0
22                                 suite_2             1       0
23                                 bedroom_3           1       0
24                                 bedroom_4           1       0
25                          bathroom                   1       0
26                          toilet                     1       0
27                          attics                     1       0.6
28                   groundfloor                       1       0
29                          kitchen                    1       0
30                          living_rooms               1       0
31                                 lounge              1       0
32                                 dining_room         1       0
33                                 conservatory        1       0
34                                 playroom            1       0
35                          wet_room_&_toilet          1       0
36                          garage                     1       0
37                          garden                     1       0.9
38                          hot_tub_suite              1       1
39                   basement                          1       0
40                          cellars                    1       1
41                          wine_cellar                1       1
42                          cinema                     1       0.75

Updated data:
INDEX       NAME_HIERARCHY                         WEIGHT      COVERAGE    (POTENTIAL INCREASE)
1      cleaning                                        1       0.409167
2             house1                                   40      13.25
3                    bedrooms                          1       0.25        0.0375
4                    bathrooms                         1       0.5
5                           bathroom1                  1       0.5         0.008333
6                           bathroom2                  1       0           0.016667
7                           outside_lavatory           1       1
8                    attic                             1       0.75        0.0125
9                    kitchen                           1       0.1         0.045
10                   living_rooms                      1       0.25
11                          lounge                     1       0           0.0125
12                          dining_room                1       0           0.0125
13                          conservatory               1       0           0.0125
14                          playroom                   1       1
15                   basement                          1       0           0.05
16                   garage                            1       0           0.05
17                   garden                            1       0.8         0.01
18            house2                                   60      27.666667
19                   upstairs                          1       0.15
20                          bedrooms                   1       0
21                                 suite_1             1       0           0.0125
22                                 suite_2             1       0           0.0125
23                                 bedroom_3           1       0           0.0125
24                                 bedroom_4           1       0           0.0125
25                          bathroom                   1       0           0.05
26                          toilet                     1       0           0.05
27                          attics                     1       0.6         0.02
28                   groundfloor                       1       0.316667
29                          kitchen                    1       0           0.033333
30                          living_rooms               1       0
31                                 lounge              1       0           0.008333
32                                 dining_room         1       0           0.008333
33                                 conservatory        1       0           0.008333
34                                 playroom            1       0           0.008333
35                          wet_room_&_toilet          1       0           0.033333
36                          garage                     1       0           0.033333
37                          garden                     1       0.9         0.003333
38                          hot_tub_suite              1       1
39                   basement                          1       0.916667
40                          cellars                    1       1
41                          wine_cellar                1       1
42                          cinema                     1       0.75        0.016667

```

The input CSV file used:

```txt

NUMBER,NAME_HIERARCHY,PARENT_NUMBER,WEIGHT,COVERAGE
1,cleaning,0,1,0
2,house1,1,40,0
3,bedrooms,2,1,0.25
4,bathrooms,2,1,0
5,bathroom1,4,1,0.5
6,bathroom2,4,1,0
7,outside_lavatory,4,1,1
8,attic,2,1,0.75
9,kitchen,2,1,0.1
10,living_rooms,2,1,0
11,lounge,10,1,0
12,dining_room,10,1,0
13,conservatory,10,1,0
14,playroom,10,1,1
15,basement,2,1,0
16,garage,2,1,0
17,garden,2,1,0.8
18,house2,1,60,0
19,upstairs,18,1,0
20,bedrooms,19,1,0
21,suite_1,20,1,0
22,suite_2,20,1,0
23,bedroom_3,20,1,0
24,bedroom_4,20,1,0
25,bathroom,19,1,0
26,toilet,19,1,0
27,attics,19,1,0.6
28,groundfloor,18,1,0
29,kitchen,28,1,0
30,living_rooms,28,1,0
31,lounge,30,1,0
32,dining_room,30,1,0
33,conservatory,30,1,0
34,playroom,30,1,0
35,wet_room_&_toilet,28,1,0
36,garage,28,1,0
37,garden,28,1,0.9
38,hot_tub_suite,28,1,1
39,basement,18,1,0
40,cellars,39,1,1
41,wine_cellar,39,1,1
42,cinema,39,1,0.75

```



## Kotlin


```scala
// version 1.2.10

class FCNode(val name: String, val weight: Int = 1, coverage: Double = 0.0) {

    var coverage = coverage
        set(value) {
            if (field != value) {
               field = value
               // update any parent's coverage
               if (parent != null) parent!!.updateCoverage()
            }
        }

    val children = mutableListOf<FCNode>()
    var parent: FCNode? = null

    fun addChildren(nodes: List<FCNode>) {
        children.addAll(nodes)
        nodes.forEach { it.parent = this }
        updateCoverage()
    }

    private fun updateCoverage() {
        val v1 = children.sumByDouble { it.weight * it.coverage }
        val v2 = children.sumBy { it.weight }
        coverage = v1 / v2
    }

    fun show(level: Int = 0) {
        val indent = level * 4
        val nl = name.length + indent
        print(name.padStart(nl))
        print("|".padStart(32 - nl))
        print("  %3d   |".format(weight))
        println(" %8.6f |".format(coverage))
        if (children.size == 0) return
        for (child in children) child.show(level + 1)
    }
}

val houses = listOf(
    FCNode("house1", 40),
    FCNode("house2", 60)
)

val house1 = listOf(
    FCNode("bedrooms", 1, 0.25),
    FCNode("bathrooms"),
    FCNode("attic", 1, 0.75),
    FCNode("kitchen", 1, 0.1),
    FCNode("living_rooms"),
    FCNode("basement"),
    FCNode("garage"),
    FCNode("garden", 1, 0.8)
)

val house2 = listOf(
    FCNode("upstairs"),
    FCNode("groundfloor"),
    FCNode("basement")
)

val h1Bathrooms = listOf(
    FCNode("bathroom1", 1, 0.5),
    FCNode("bathroom2"),
    FCNode("outside_lavatory", 1, 1.0)
)

val h1LivingRooms = listOf(
    FCNode("lounge"),
    FCNode("dining_room"),
    FCNode("conservatory"),
    FCNode("playroom", 1, 1.0)
)

val h2Upstairs = listOf(
    FCNode("bedrooms"),
    FCNode("bathroom"),
    FCNode("toilet"),
    FCNode("attics", 1, 0.6)
)

val h2Groundfloor = listOf(
    FCNode("kitchen"),
    FCNode("living_rooms"),
    FCNode("wet_room_&_toilet"),
    FCNode("garage"),
    FCNode("garden", 1, 0.9),
    FCNode("hot_tub_suite", 1, 1.0)
)

val h2Basement = listOf(
    FCNode("cellars", 1, 1.0),
    FCNode("wine_cellar", 1, 1.0),
    FCNode("cinema", 1, 0.75)
)

val h2UpstairsBedrooms = listOf(
    FCNode("suite_1"),
    FCNode("suite_2"),
    FCNode("bedroom_3"),
    FCNode("bedroom_4")
)

val h2GroundfloorLivingRooms = listOf(
    FCNode("lounge"),
    FCNode("dining_room"),
    FCNode("conservatory"),
    FCNode("playroom")
)

fun main(args: Array<String>) {
    val cleaning = FCNode("cleaning")

    house1[1].addChildren(h1Bathrooms)
    house1[4].addChildren(h1LivingRooms)
    houses[0].addChildren(house1)

    h2Upstairs[0].addChildren(h2UpstairsBedrooms)
    house2[0].addChildren(h2Upstairs)
    h2Groundfloor[1].addChildren(h2GroundfloorLivingRooms)
    house2[1].addChildren(h2Groundfloor)
    house2[2].addChildren(h2Basement)
    houses[1].addChildren(house2)

    cleaning.addChildren(houses)
    val topCoverage = cleaning.coverage
    println("TOP COVERAGE = ${"%8.6f".format(topCoverage)}\n")
    println("NAME HIERARCHY                 | WEIGHT | COVERAGE |")
    cleaning.show()

    h2Basement[2].coverage = 1.0  // change Cinema node coverage to 1.0
    val diff = cleaning.coverage - topCoverage
    println("\nIf the coverage of the Cinema node were increased from 0.75 to 1.0")
    print("the top level coverage would increase by ")
    println("${"%8.6f".format(diff)} to ${"%8.6f".format(topCoverage + diff)}")    
    h2Basement[2].coverage = 0.75  // restore to original value if required
}
```


{{out}}

```txt

TOP COVERAGE = 0.409167

NAME HIERARCHY                 | WEIGHT | COVERAGE |
cleaning                       |    1   | 0.409167 |
    house1                     |   40   | 0.331250 |
        bedrooms               |    1   | 0.250000 |
        bathrooms              |    1   | 0.500000 |
            bathroom1          |    1   | 0.500000 |
            bathroom2          |    1   | 0.000000 |
            outside_lavatory   |    1   | 1.000000 |
        attic                  |    1   | 0.750000 |
        kitchen                |    1   | 0.100000 |
        living_rooms           |    1   | 0.250000 |
            lounge             |    1   | 0.000000 |
            dining_room        |    1   | 0.000000 |
            conservatory       |    1   | 0.000000 |
            playroom           |    1   | 1.000000 |
        basement               |    1   | 0.000000 |
        garage                 |    1   | 0.000000 |
        garden                 |    1   | 0.800000 |
    house2                     |   60   | 0.461111 |
        upstairs               |    1   | 0.150000 |
            bedrooms           |    1   | 0.000000 |
                suite_1        |    1   | 0.000000 |
                suite_2        |    1   | 0.000000 |
                bedroom_3      |    1   | 0.000000 |
                bedroom_4      |    1   | 0.000000 |
            bathroom           |    1   | 0.000000 |
            toilet             |    1   | 0.000000 |
            attics             |    1   | 0.600000 |
        groundfloor            |    1   | 0.316667 |
            kitchen            |    1   | 0.000000 |
            living_rooms       |    1   | 0.000000 |
                lounge         |    1   | 0.000000 |
                dining_room    |    1   | 0.000000 |
                conservatory   |    1   | 0.000000 |
                playroom       |    1   | 0.000000 |
            wet_room_&_toilet  |    1   | 0.000000 |
            garage             |    1   | 0.000000 |
            garden             |    1   | 0.900000 |
            hot_tub_suite      |    1   | 1.000000 |
        basement               |    1   | 0.916667 |
            cellars            |    1   | 1.000000 |
            wine_cellar        |    1   | 1.000000 |
            cinema             |    1   | 0.750000 |

If the coverage of the Cinema node were increased from 0.75 to 1.0
the top level coverage would increase by 0.016667 to 0.425833

```



## Perl


```perl
#!/usr/bin/perl

use strict;
use warnings;

print $_->[0] for walktree( do { local $/; <DATA> } );

sub walktree
  {
  my @parts;
  while( $_[0] =~ /(( *)\S.*\n)((?:\2 .*\n)*)/g )
    {
    my ($head, $body, $w, $wsum) = ($1, $3, 0, 0);
    $head =~ /^.*?\|(\S*) *\|(\S*) *\|/;
    my $weight = sprintf '%-8s', $1 || 1;
    my $coverage = sprintf '%-10s', $2 || 0;
    $w += $_->[1], $wsum += $_->[1] * $_->[2], $head .= $_->[0]
      for walktree( $body );
    $w and $coverage = sprintf '%-10.8g', $wsum / $w;
    push @parts, [ $head =~ s/\|.*/|$weight|$coverage|/r, $weight, $coverage ];
    }
  return @parts;
  }

__DATA__
NAME_HIERARCHY                  |WEIGHT  |COVERAGE  |
cleaning                        |        |          |
    house1                      |40      |          |
        bedrooms                |        |0.25      |
        bathrooms               |        |          |
            bathroom1           |        |0.5       |
            bathroom2           |        |          |
            outside_lavatory    |        |1         |
        attic                   |        |0.75      |
        kitchen                 |        |0.1       |
        living_rooms            |        |          |
            lounge              |        |          |
            dining_room         |        |          |
            conservatory        |        |          |
            playroom            |        |1         |
        basement                |        |          |
        garage                  |        |          |
        garden                  |        |0.8       |
    house2                      |60      |          |
        upstairs                |        |          |
            bedrooms            |        |          |
                suite_1         |        |          |
                suite_2         |        |          |
                bedroom_3       |        |          |
                bedroom_4       |        |          |
            bathroom            |        |          |
            toilet              |        |          |
            attics              |        |0.6       |
        groundfloor             |        |          |
            kitchen             |        |          |
            living_rooms        |        |          |
                lounge          |        |          |
                dining_room     |        |          |
                conservatory    |        |          |
                playroom        |        |          |
            wet_room_&_toilet   |        |          |
            garage              |        |          |
            garden              |        |0.9       |
            hot_tub_suite       |        |1         |
        basement                |        |          |
            cellars             |        |1         |
            wine_cellar         |        |1         |
            cinema              |        |0.75      |

```

{{out}}

```txt

NAME_HIERARCHY                  |WEIGHT  |COVERAGE  |
cleaning                        |1       |0.40916667|
    house1                      |40      |0.33125   |
        bedrooms                |1       |0.25      |
        bathrooms               |1       |0.5       |
            bathroom1           |1       |0.5       |
            bathroom2           |1       |0         |
            outside_lavatory    |1       |1         |
        attic                   |1       |0.75      |
        kitchen                 |1       |0.1       |
        living_rooms            |1       |0.25      |
            lounge              |1       |0         |
            dining_room         |1       |0         |
            conservatory        |1       |0         |
            playroom            |1       |1         |
        basement                |1       |0         |
        garage                  |1       |0         |
        garden                  |1       |0.8       |
    house2                      |60      |0.46111111|
        upstairs                |1       |0.15      |
            bedrooms            |1       |0         |
                suite_1         |1       |0         |
                suite_2         |1       |0         |
                bedroom_3       |1       |0         |
                bedroom_4       |1       |0         |
            bathroom            |1       |0         |
            toilet              |1       |0         |
            attics              |1       |0.6       |
        groundfloor             |1       |0.31666667|
            kitchen             |1       |0         |
            living_rooms        |1       |0         |
                lounge          |1       |0         |
                dining_room     |1       |0         |
                conservatory    |1       |0         |
                playroom        |1       |0         |
            wet_room_&_toilet   |1       |0         |
            garage              |1       |0         |
            garden              |1       |0.9       |
            hot_tub_suite       |1       |1         |
        basement                |1       |0.91666667|
            cellars             |1       |1         |
            wine_cellar         |1       |1         |
            cinema              |1       |0.75      |

```


## Python


### Python: Using lists and tuples

It's actually some of the raw code used when researching this task.


```python
from itertools import zip_longest


fc2 = '''\
cleaning,,
    house1,40,
        bedrooms,,.25
        bathrooms,,
            bathroom1,,.5
            bathroom2,,
            outside_lavatory,,1
        attic,,.75
        kitchen,,.1
        living_rooms,,
            lounge,,
            dining_room,,
            conservatory,,
            playroom,,1
        basement,,
        garage,,
        garden,,.8
    house2,60,
        upstairs,,
            bedrooms,,
                suite_1,,
                suite_2,,
                bedroom_3,,
                bedroom_4,,
            bathroom,,
            toilet,,
            attics,,.6
        groundfloor,,
            kitchen,,
            living_rooms,,
                lounge,,
                dining_room,,
                conservatory,,
                playroom,,
            wet_room_&_toilet,,
            garage,,
            garden,,.9
            hot_tub_suite,,1
        basement,,
            cellars,,1
            wine_cellar,,1
            cinema,,.75

'''

NAME, WT, COV = 0, 1, 2

def right_type(txt):
    try:
        return float(txt)
    except ValueError:
        return txt

def commas_to_list(the_list, lines, start_indent=0):
    '''
    Output format is a nest of lists and tuples
    lists are for coverage leaves without children items in the list are name, weight, coverage
    tuples are 2-tuples for nodes with children. The first element is a list representing the
    name, weight, coverage of the node (some to be calculated); the second element is a list of
    child elements which may be 2-tuples or lists as above.
    
    the_list is modified in-place
    lines must be a generator of successive lines of input like fc2
    '''
    for n, line in lines:
        indent = 0
        while line.startswith(' ' * (4 * indent)):
            indent += 1
        indent -= 1
        fields = [right_type(f) for f in line.strip().split(',')]
        if indent == start_indent:
            the_list.append(fields)
        elif indent > start_indent:
            lst = [fields]
            sub = commas_to_list(lst, lines, indent)
            the_list[-1] = (the_list[-1], lst)
            if sub not in (None, ['']) :
                the_list.append(sub)
        else:
            return fields if fields else None
    return None


def pptreefields(lst, indent=0, widths=['%-32s', '%-8g', '%-10g']):
    '''
    Pretty prints the format described from function commas_to_list as a table with 
    names in the first column suitably indented and all columns having a fixed 
    minimum column width.
    '''
    lhs = ' ' * (4 * indent)
    for item in lst:
        if type(item) != tuple:
            name, *rest = item
            print(widths[0] % (lhs + name), end='|')
            for width, item in zip_longest(widths[1:len(rest)], rest, fillvalue=widths[-1]):
                if type(item) == str:
                    width = width[:-1] + 's'
                print(width % item, end='|')
            print()
        else:
            item, children = item
            name, *rest = item
            print(widths[0] % (lhs + name), end='|')
            for width, item in zip_longest(widths[1:len(rest)], rest, fillvalue=widths[-1]):
                if type(item) == str:
                    width = width[:-1] + 's'
                print(width % item, end='|')
            print()
            pptreefields(children, indent+1)


def default_field(node_list):
    node_list[WT] = node_list[WT] if node_list[WT] else 1.0
    node_list[COV] = node_list[COV] if node_list[COV] else 0.0

def depth_first(tree, visitor=default_field):
    for item in tree:
        if type(item) == tuple:
            item, children = item
            depth_first(children, visitor)
        visitor(item)
            

def covercalc(tree):
    '''
    Depth first weighted average of coverage
    '''
    sum_covwt, sum_wt = 0, 0
    for item in tree:
        if type(item) == tuple:
            item, children = item
            item[COV] = covercalc(children)
        sum_wt  += item[WT]
        sum_covwt += item[COV] * item[WT]
    cov = sum_covwt / sum_wt
    return cov

if __name__ == '__main__':        
    lstc = []
    commas_to_list(lstc, ((n, ln) for n, ln in enumerate(fc2.split('\n'))))
    #pp(lstc, width=1, indent=4, compact=1)
    
    #print('\n\nEXPANDED DEFAULTS\n')
    depth_first(lstc)
    #pptreefields(['NAME_HIERARCHY WEIGHT COVERAGE'.split()] + lstc)
    
    print('\n\nTOP COVERAGE = %f\n' % covercalc(lstc))
    depth_first(lstc)
    pptreefields(['NAME_HIERARCHY WEIGHT COVERAGE'.split()] + lstc)
```


{{out}}

```txt
TOP COVERAGE = 0.409167

NAME_HIERARCHY                  |WEIGHT  |COVERAGE  |
cleaning                        |1       |0.409167  |
    house1                      |40      |0.33125   |
        bedrooms                |1       |0.25      |
        bathrooms               |1       |0.5       |
            bathroom1           |1       |0.5       |
            bathroom2           |1       |0         |
            outside_lavatory    |1       |1         |
        attic                   |1       |0.75      |
        kitchen                 |1       |0.1       |
        living_rooms            |1       |0.25      |
            lounge              |1       |0         |
            dining_room         |1       |0         |
            conservatory        |1       |0         |
            playroom            |1       |1         |
        basement                |1       |0         |
        garage                  |1       |0         |
        garden                  |1       |0.8       |
    house2                      |60      |0.461111  |
        upstairs                |1       |0.15      |
            bedrooms            |1       |0         |
                suite_1         |1       |0         |
                suite_2         |1       |0         |
                bedroom_3       |1       |0         |
                bedroom_4       |1       |0         |
            bathroom            |1       |0         |
            toilet              |1       |0         |
            attics              |1       |0.6       |
        groundfloor             |1       |0.316667  |
            kitchen             |1       |0         |
            living_rooms        |1       |0         |
                lounge          |1       |0         |
                dining_room     |1       |0         |
                conservatory    |1       |0         |
                playroom        |1       |0         |
            wet_room_&_toilet   |1       |0         |
            garage              |1       |0         |
            garden              |1       |0.9       |
            hot_tub_suite       |1       |1         |
        basement                |1       |0.916667  |
            cellars             |1       |1         |
            wine_cellar         |1       |1         |
            cinema              |1       |0.75      |
```



### Python: Class based and extra credit

A cleaner implementation that uses the class static variable path2node as in the previous example so you don't have to traverse the tree to work out the position to add new nodes. This relies on parent nodes appearing before their children which is the case in the order of the add_node calls.


```python
# -*- coding: utf-8 -*-

SPACES = 4
class Node:
    path2node = {}
    
    def add_node(self, pathname, wt, cov):
        path2node = self.path2node
        path, name = pathname.strip().rsplit('/', 1)
        node = Node(name, wt, cov)
        path2node[pathname] = node
        path2node[path].child.append(node) # Link the tree

    def __init__(self, name="", wt=1, cov=0.0, child=None):
        if child is None:
            child = []
        self.name, self.wt, self.cov, self.child = name, wt, cov, child
        self.delta = None
        self.sum_wt = wt
        if name == "": 
            # designate the top of the tree
            self.path2node[name] = self
    
    
    def __repr__(self, indent=0):
        name, wt, cov, delta, child = (self.name, self.wt, self.cov, 
                                       self.delta, self.child)
        lhs = ' ' * (SPACES * indent) + "Node(%r," % name
        txt = '%-40s wt=%2g, cov=%-8.5g, delta=%-10s, child=[' \
              % (lhs, wt, cov, ('n/a' if delta is None else '%-10.7f' % delta))
        if not child:
            txt += (']),\n')
        else:
            txt += ('\n')
            for c in child:
                txt += c.__repr__(indent + 1)
            txt += (' ' * (SPACES * indent) + "]),\n")
        return txt

    def covercalc(self):
        '''
        Depth first weighted average of coverage
        '''
        child = self.child
        if not child:
            return self.cov
        sum_covwt, sum_wt = 0, 0
        for node in child:
            nwt = node.wt
            ncov = node.covercalc()
            sum_wt += nwt
            sum_covwt += ncov * nwt
        cov = sum_covwt / sum_wt
        self.sum_wt = sum_wt
        self.cov = cov
        return cov

    def deltacalc(self, power=1.0):
        '''
        Top down distribution of weighted residuals
        '''
        sum_wt = self.sum_wt
        self.delta = delta = (1 - self.cov) * power
        for node in self.child:
            node.deltacalc(power * node.wt / sum_wt)
        return delta


def isclose(a, b, rel_tol=1e-9, abs_tol=1e-9):
    return abs(a-b) <= max( rel_tol * max(abs(a), abs(b)), abs_tol )
    
    
if __name__ == '__main__': 
    top = Node()    # Add placeholder for top of tree
    add_node = top.add_node
    
    add_node('/cleaning', 1, 0)
    add_node('/cleaning/house1', 40, 0)
    add_node('/cleaning/house1/bedrooms', 1, 0.25)
    add_node('/cleaning/house1/bathrooms', 1, 0)
    add_node('/cleaning/house1/bathrooms/bathroom1', 1, 0.5)
    add_node('/cleaning/house1/bathrooms/bathroom2', 1, 0)
    add_node('/cleaning/house1/bathrooms/outside_lavatory', 1, 1)
    add_node('/cleaning/house1/attic', 1, 0.75)
    add_node('/cleaning/house1/kitchen', 1, 0.1)
    add_node('/cleaning/house1/living_rooms', 1, 0)
    add_node('/cleaning/house1/living_rooms/lounge', 1, 0)
    add_node('/cleaning/house1/living_rooms/dining_room', 1, 0)
    add_node('/cleaning/house1/living_rooms/conservatory', 1, 0)
    add_node('/cleaning/house1/living_rooms/playroom', 1, 1)
    add_node('/cleaning/house1/basement', 1, 0)
    add_node('/cleaning/house1/garage', 1, 0)
    add_node('/cleaning/house1/garden', 1, 0.8)
    add_node('/cleaning/house2', 60, 0)
    add_node('/cleaning/house2/upstairs', 1, 0)
    add_node('/cleaning/house2/upstairs/bedrooms', 1, 0)
    add_node('/cleaning/house2/upstairs/bedrooms/suite_1', 1, 0)
    add_node('/cleaning/house2/upstairs/bedrooms/suite_2', 1, 0)
    add_node('/cleaning/house2/upstairs/bedrooms/bedroom_3', 1, 0)
    add_node('/cleaning/house2/upstairs/bedrooms/bedroom_4', 1, 0)
    add_node('/cleaning/house2/upstairs/bathroom', 1, 0)
    add_node('/cleaning/house2/upstairs/toilet', 1, 0)
    add_node('/cleaning/house2/upstairs/attics', 1, 0.6)
    add_node('/cleaning/house2/groundfloor', 1, 0)
    add_node('/cleaning/house2/groundfloor/kitchen', 1, 0)
    add_node('/cleaning/house2/groundfloor/living_rooms', 1, 0)
    add_node('/cleaning/house2/groundfloor/living_rooms/lounge', 1, 0)
    add_node('/cleaning/house2/groundfloor/living_rooms/dining_room', 1, 0)
    add_node('/cleaning/house2/groundfloor/living_rooms/conservatory', 1, 0)
    add_node('/cleaning/house2/groundfloor/living_rooms/playroom', 1, 0)
    add_node('/cleaning/house2/groundfloor/wet_room_&_toilet', 1, 0)
    add_node('/cleaning/house2/groundfloor/garage', 1, 0)
    add_node('/cleaning/house2/groundfloor/garden', 1, 0.9)
    add_node('/cleaning/house2/groundfloor/hot_tub_suite', 1, 1)
    add_node('/cleaning/house2/basement', 1, 0)
    add_node('/cleaning/house2/basement/cellars', 1, 1)
    add_node('/cleaning/house2/basement/wine_cellar', 1, 1)
    add_node('/cleaning/house2/basement/cinema', 1, 0.75)

    top = top.child[0]  # Remove artificial top
    cover = top.covercalc()
    delta = top.deltacalc()
    print('TOP COVERAGE = %g\n' % cover)
    print(top)
    assert isclose((delta + cover), 1.0), "Top level delta + coverage should " \
                                          "equal 1 instead of (%f + %f)" % (delta, cover)

```


{{out}}

The deltas where checked by, for example, changing the coverage of the cinema in house2 to be 1.0 instead of 0.75 and observing an additional 0.0166667 increase in the top level coverage at node 'cleaning'.


```txt
TOP COVERAGE = 0.409167

Node('cleaning',                         wt= 1, cov=0.40917 , delta=0.5908333 , child=[
    Node('house1',                       wt=40, cov=0.33125 , delta=0.2675000 , child=[
        Node('bedrooms',                 wt= 1, cov=0.25    , delta=0.0375000 , child=[]),
        Node('bathrooms',                wt= 1, cov=0.5     , delta=0.0250000 , child=[
            Node('bathroom1',            wt= 1, cov=0.5     , delta=0.0083333 , child=[]),
            Node('bathroom2',            wt= 1, cov=0       , delta=0.0166667 , child=[]),
            Node('outside_lavatory',     wt= 1, cov=1       , delta=0.0000000 , child=[]),
        ]),
        Node('attic',                    wt= 1, cov=0.75    , delta=0.0125000 , child=[]),
        Node('kitchen',                  wt= 1, cov=0.1     , delta=0.0450000 , child=[]),
        Node('living_rooms',             wt= 1, cov=0.25    , delta=0.0375000 , child=[
            Node('lounge',               wt= 1, cov=0       , delta=0.0125000 , child=[]),
            Node('dining_room',          wt= 1, cov=0       , delta=0.0125000 , child=[]),
            Node('conservatory',         wt= 1, cov=0       , delta=0.0125000 , child=[]),
            Node('playroom',             wt= 1, cov=1       , delta=0.0000000 , child=[]),
        ]),
        Node('basement',                 wt= 1, cov=0       , delta=0.0500000 , child=[]),
        Node('garage',                   wt= 1, cov=0       , delta=0.0500000 , child=[]),
        Node('garden',                   wt= 1, cov=0.8     , delta=0.0100000 , child=[]),
    ]),
    Node('house2',                       wt=60, cov=0.46111 , delta=0.3233333 , child=[
        Node('upstairs',                 wt= 1, cov=0.15    , delta=0.1700000 , child=[
            Node('bedrooms',             wt= 1, cov=0       , delta=0.0500000 , child=[
                Node('suite_1',          wt= 1, cov=0       , delta=0.0125000 , child=[]),
                Node('suite_2',          wt= 1, cov=0       , delta=0.0125000 , child=[]),
                Node('bedroom_3',        wt= 1, cov=0       , delta=0.0125000 , child=[]),
                Node('bedroom_4',        wt= 1, cov=0       , delta=0.0125000 , child=[]),
            ]),
            Node('bathroom',             wt= 1, cov=0       , delta=0.0500000 , child=[]),
            Node('toilet',               wt= 1, cov=0       , delta=0.0500000 , child=[]),
            Node('attics',               wt= 1, cov=0.6     , delta=0.0200000 , child=[]),
        ]),
        Node('groundfloor',              wt= 1, cov=0.31667 , delta=0.1366667 , child=[
            Node('kitchen',              wt= 1, cov=0       , delta=0.0333333 , child=[]),
            Node('living_rooms',         wt= 1, cov=0       , delta=0.0333333 , child=[
                Node('lounge',           wt= 1, cov=0       , delta=0.0083333 , child=[]),
                Node('dining_room',      wt= 1, cov=0       , delta=0.0083333 , child=[]),
                Node('conservatory',     wt= 1, cov=0       , delta=0.0083333 , child=[]),
                Node('playroom',         wt= 1, cov=0       , delta=0.0083333 , child=[]),
            ]),
            Node('wet_room_&_toilet',    wt= 1, cov=0       , delta=0.0333333 , child=[]),
            Node('garage',               wt= 1, cov=0       , delta=0.0333333 , child=[]),
            Node('garden',               wt= 1, cov=0.9     , delta=0.0033333 , child=[]),
            Node('hot_tub_suite',        wt= 1, cov=1       , delta=0.0000000 , child=[]),
        ]),
        Node('basement',                 wt= 1, cov=0.91667 , delta=0.0166667 , child=[
            Node('cellars',              wt= 1, cov=1       , delta=0.0000000 , child=[]),
            Node('wine_cellar',          wt= 1, cov=1       , delta=0.0000000 , child=[]),
            Node('cinema',               wt= 1, cov=0.75    , delta=0.0166667 , child=[]),
        ]),
    ]),
]),
```




### Python: Composition of pure functions

Parsing the task statement text directly to a tree of dictionaries, folding a '''weightedTreeAverage''' function over that tree, and further decorating it with a top-down residue-share traversal.

Mainly uses pre-existing generic functions, including '''forestFromLineIndents''', '''foldTree''' and '''fmapTree''':
{{Works with|Python|3.7}}

```python
'''Functional coverage tree'''

from itertools import chain, product
from functools import reduce


# main :: IO ()
def main():
    '''Tabular outline serialisation of a parse tree
       decorated with computations of:
       1. Weighted coverage of each tree node.
       2. Each node's share of the total project's
          remaining work.
    '''
    columnWidths = [31, 9, 9, 9]
    delimiter = '|'

    reportLines = lines(REPORT)
    columnTitles = init(columnNames(delimiter)(reportLines[0]))

    # SERIALISATION OF DECORATED PARSE TREE
    print(titleLine(delimiter)(columnWidths)(
        columnTitles + ['share of residue']
    ))
    print(indentedLinesFromTree('    ', tabulation(columnWidths))(

        # TWO COMPUTATIONS BY TRAVERSAL
        withResidueShares(1.0)(
            foldTree(weightedCoverage)(

                # TREE FROM PARSE OF OUTLINE TEXT
                fmapTree(
                    recordFromKeysDefaultsDelimiterAndLine(columnTitles)(
                        [str, float, float])(['?', 1.0, 0.0])(delimiter)
                )(
                    forestFromLineIndents(indentLevelsFromLines(
                        reportLines[1:]
                    ))[0]
                )
            )
        )
    ))


# WEIGHTED COVERAGE, AND SHARE OF TOTAL RESIDUE -----------

# weightedCoverage :: Tree Dict ->
# [Tree Dict] -> Tree Dict
def weightedCoverage(x):
    '''The weighted coverage of a tree node,
       as a function of the weighted averages
       of its children.
    '''
    def go(xs):
        cws = [(r['coverage'], r['weight']) for r in [root(x) for x in xs]]
        totalWeight = reduce(lambda a, x: a + x[1], cws, 0)
        return Node(dict(
            x, **{
                'coverage': round(reduce(
                    lambda a, cw: a + (cw[0] * cw[1]),
                    cws, x['coverage']
                ) / (totalWeight if 0 < totalWeight else 1), 5)
            }
        ))(xs)
    return lambda xs: go(xs)


# withResidueShares :: Float -> Tree Dict -> Tree Dict
def withResidueShares(shareOfTotal):
    '''A Tree of dictionaries additionally decorated with each
       node's proportion of the total project's outstanding work.
    '''
    def go(fraction, node):
        [nodeRoot, nodeNest] = apList([root, nest])([node])
        weights = [root(x)['weight'] for x in nodeNest]
        siblingsTotal = sum(weights)
        return Node(
            insertDict('residual_share')(
                round(fraction * (1 - nodeRoot['coverage']), 5)
            )(nodeRoot)
        )(
            map(
                go,
                [fraction * (w / siblingsTotal) for w in weights],
                nodeNest
            )
        )
    return lambda tree: go(shareOfTotal, tree)


# OUTLINE TABULATION --------------------------------------

# tabulation :: [Int] -> String -> Dict -> String
def tabulation(columnWidths):
    '''Indented string representation of a node
       in a functional coverage tree.
    '''
    return lambda indent, dct: '| '.join(map(
        lambda k, w: (
            (indent if 10 < w else '') + str(dct.get(k, ''))
        ).ljust(w, ' '),
        dct.keys(),
        columnWidths
    ))


# titleLine :: String -> [Int] -> [String] -> String
def titleLine(delimiter):
    '''A string consisting of a spaced and delimited
       series of upper-case column titles.
    '''
    return lambda columnWidths: lambda ks: (
        delimiter + ' '
    ).join(map(
        lambda k, w: k.ljust(w, ' '),
        [k.upper() for k in ks],
        columnWidths
    ))


# GENERIC AND REUSABLE FUNCTIONS --------------------------

# Node :: a -> [Tree a] -> Tree a
def Node(v):
    '''Constructor for a Tree node which connects a
       value of some kind to a list of zero or
       more child trees.
    '''
    return lambda xs: {'type': 'Tree', 'root': v, 'nest': xs}


# Tuple (,) :: a -> b -> (a, b)
def Tuple(x):
    '''Constructor for a pair of values,
       possibly of two different types.
    '''
    return lambda y: (
        x + (y,)
    ) if isinstance(x, tuple) else (x, y)


# apList (<*>) :: [(a -> b)] -> [a] -> [b]
def apList(fs):
    '''The application of each of a list of functions,
       to each of a list of values.
    '''
    return liftA2List(identity)(fs)


# columnNames :: String -> String -> [String]
def columnNames(delimiter):
    '''A list of lower-case keys derived from
       a header line and a delimiter character.
    '''
    return compose(
        fmapList(compose(toLower, strip)),
        splitOn(delimiter)
    )


# compose :: ((a -> a), ...) -> (a -> a)
def compose(*fs):
    '''Composition, from right to left,
       of a series of functions.
    '''
    return lambda x: reduce(
        lambda a, f: f(a),
        fs[::-1], x
    )


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list or string over which a function f
       has been mapped.
       The list monad can be derived by using an (a -> [b])
       function which wraps its output in a list (using an
       empty list to represent computational failure).
    '''
    return lambda xs: (''.join if isinstance(xs, str) else list)(
        chain.from_iterable(map(f, xs))
    )


# div :: Int -> Int -> Int
def div(x):
    '''Integer division.'''
    return lambda y: x // y


def firstArrow(f):
    '''A simple function lifted to one which applies
       to a tuple, transforming only its first item.
    '''
    return lambda xy: Tuple(f(xy[0]))(
        xy[1]
    )


# flip :: (a -> b -> c) -> b -> a -> c
def flip(f):
    '''The (curried or uncurried) function f with its
       arguments reversed.
    '''
    return lambda a: lambda b: f(b)(a)


# fmapList :: (a -> b) -> [a] -> [b]
def fmapList(f):
    '''fmap over a list.
       f lifted to a function over a list.
    '''
    return lambda xs: [f(x) for x in xs]


# fmapTree :: (a -> b) -> Tree a -> Tree b
def fmapTree(f):
    '''A new tree holding the results of
       applying f to each root in
       the existing tree.
    '''
    def go(x):
        return Node(f(x['root']))(
            [go(v) for v in x['nest']]
        )
    return lambda tree: go(tree)


# foldTree :: (a -> [b] -> b) -> Tree a -> b
def foldTree(f):
    '''The catamorphism on trees. A summary
       value obtained by a depth-first fold.
    '''
    def go(node):
        return f(node['root'])([
            go(x) for x in node['nest']
        ])
    return lambda tree: go(tree)


# forestFromLineIndents :: [(Int, String)] -> [Tree String]
def forestFromLineIndents(tuples):
    '''A list of trees derived from a list of lines paired
       with integers giving their levels of indentation.
    '''
    def go(xs):
        if xs:
            (intIndent, txt) = xs[0]
            (firstTreeLines, rest) = span(
                compose(lt(intIndent), fst)
            )(xs[1:])
            return [Node(txt)(go(firstTreeLines))] + go(rest)
        else:
            return []
    return go(tuples)


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# identity :: a -> a
def identity(x):
    '''The identity function.'''
    return x


# indentLevelsFromLines :: [String] -> [(Int, String)]
def indentLevelsFromLines(xs):
    '''Each input line stripped of leading
       white space, and tupled with a preceding integer
       giving its level of indentation from 0 upwards.
    '''
    indentTextPairs = list(map(
        compose(firstArrow(len), span(isSpace)),
        xs
    ))
    indentUnit = min(concatMap(
        lambda x: [x[0]] if x[0] else []
    )(indentTextPairs))
    return list(map(
        firstArrow(flip(div)(indentUnit)),
        indentTextPairs
    ))


# indentedLinesFromTree :: String -> (String -> a -> String) ->
# [Tree a] -> String
def indentedLinesFromTree(strTab, f):
    '''An indented line rendering of a tree, in which
       the function f stringifies a root value.
    '''
    def go(indent):
        return lambda node: [f(indent, node['root'])] + concatMap(
            go(strTab + indent)
        )(node['nest'])
    return lambda tree: unlines(go('')(tree))


# init :: [a] -> [a]
def init(xs):
    '''A list containing all the elements
       of xs except the last.
    '''
    return xs[:-1]


# insertDict :: String -> a -> Dict -> Dict
def insertDict(k):
    '''A new dictionary updated with a (k, v) pair.'''
    def go(v, dct):
        return dict(dct, **{k: v})
    return lambda v: lambda dct: go(v, dct)


# isSpace :: Char -> Bool
# isSpace :: String -> Bool
def isSpace(s):
    '''True if s is not empty, and
       contains only white space.
    '''
    return s.isspace()


# liftA2List :: (a -> b -> c) -> [a] -> [b] -> [c]
def liftA2List(f):
    '''The binary operator f lifted to a function over two
       lists. f applied to each pair of arguments in the
       cartesian product of xs and ys.
    '''
    return lambda xs: lambda ys: [
        f(x)(y) for x, y in product(xs, ys)
    ]


# lines :: String -> [String]
def lines(s):
    '''A list of strings,
       (containing no newline characters)
       derived from a single new-line delimited string.
    '''
    return s.splitlines()


# lt (<) :: Ord a => a -> a -> Bool
def lt(x):
    '''True if x < y.'''
    return lambda y: (x < y)


# nest :: Tree a -> [Tree a]
def nest(t):
    '''Accessor function for children of tree node.'''
    return t['nest'] if 'nest' in t else None


# recordFromKeysDefaultsAndLine :: String ->
# { name :: String, weight :: Float, completion :: Float }
def recordFromKeysDefaultsDelimiterAndLine(columnTitles):
    '''A dictionary of key-value pairs, derived from a
       delimited string, together with ordered lists of
       key-names, types, default values, and a delimiter.
    '''
    return lambda ts: lambda vs: lambda delim: lambda s: dict(
        map(
            lambda k, t, v, x: (k, t(x) if x else v),
            columnTitles, ts, vs,
            map(strip, splitOn(delim)(s))
        )
    )


# root :: Tree a -> a
def root(t):
    '''Accessor function for data of tree node.'''
    return t['root'] if 'root' in t else None


# strip :: String -> String
def strip(s):
    '''A copy of s without any leading or trailling
       white space.
    '''
    return s.strip()


# span :: (a -> Bool) -> [a] -> ([a], [a])
def span(p):
    '''The longest (possibly empty) prefix of xs
       that contains only elements satisfying p,
       tupled with the remainder of xs.
       span p xs is equivalent to (takeWhile p xs, dropWhile p xs).
    '''
    def go(xs):
        lng = len(xs)
        return splitAt(
            until(lambda i: (lng == i) or not p(xs[i]))(succ)(0)
        )(xs)
    return lambda xs: go(xs)


# Unimplemented <- splitOn for lists (Eq a => [a] -> [a] -> [[a]])
# splitOn :: String -> String -> [String]
def splitOn(pat):
    '''A list of the strings delimited by
       instances of a given pattern in s.
    '''
    return lambda xs: (
        xs.split(pat) if isinstance(xs, str) else None
    )


# splitAt :: Int -> [a] -> ([a], [a])
def splitAt(n):
    '''A tuple pairing the prefix of length n
       with the rest of xs.
    '''
    return lambda xs: (xs[0:n], xs[n:])


# succ :: Enum a => a -> a
def succ(x):
    '''The successor of a value.
       For numeric types, (1 +).
    '''
    return 1 + x if isinstance(x, int) else (
        chr(1 + ord(x))
    )


# toLower :: String -> String
def toLower(s):
    '''String in lower case.'''
    return s.lower()


# unlines :: [String] -> String
def unlines(xs):
    '''A single string formed by the intercalation
       of a list of strings with the newline character.
    '''
    return '\n'.join(xs)


# until :: (a -> Bool) -> (a -> a) -> a -> a
def until(p):
    '''The result of repeatedly applying f until p holds.
       The initial seed value is x.
    '''
    def go(f, x):
        v = x
        while not p(v):
            v = f(v)
        return v
    return lambda f: lambda x: go(f, x)


# MAIN ----------------------------------------------------
if __name__ == '__main__':
    REPORT = '''NAME_HIERARCHY                  |WEIGHT  |COVERAGE  |
    cleaning                        |        |          |
        house1                      |40      |          |
            bedrooms                |        |0.25      |
            bathrooms               |        |          |
                bathroom1           |        |0.5       |
                bathroom2           |        |          |
                outside_lavatory    |        |1         |
            attic                   |        |0.75      |
            kitchen                 |        |0.1       |
            living_rooms            |        |          |
                lounge              |        |          |
                dining_room         |        |          |
                conservatory        |        |          |
                playroom            |        |1         |
            basement                |        |          |
            garage                  |        |          |
            garden                  |        |0.8       |
        house2                      |60      |          |
            upstairs                |        |          |
                bedrooms            |        |          |
                    suite_1         |        |          |
                    suite_2         |        |          |
                    bedroom_3       |        |          |
                    bedroom_4       |        |          |
                bathroom            |        |          |
                toilet              |        |          |
                attics              |        |0.6       |
            groundfloor             |        |          |
                kitchen             |        |          |
                living_rooms        |        |          |
                    lounge          |        |          |
                    dining_room     |        |          |
                    conservatory    |        |          |
                    playroom        |        |          |
                wet_room_&_toilet   |        |          |
                garage              |        |          |
                garden              |        |0.9       |
                hot_tub_suite       |        |1         |
            basement                |        |          |
                cellars             |        |1         |
                wine_cellar         |        |1         |
                cinema              |        |0.75      |'''
    main()
```

{{Out}}

```txt
NAME_HIERARCHY                 | WEIGHT   | COVERAGE | SHARE OF RESIDUE
cleaning                       | 1.0      | 0.40917  | 0.59083  
    house1                     | 40.0     | 0.33125  | 0.2675   
        bedrooms               | 1.0      | 0.25     | 0.0375   
        bathrooms              | 1.0      | 0.5      | 0.025    
            bathroom1          | 1.0      | 0.5      | 0.00833  
            bathroom2          | 1.0      | 0.0      | 0.01667  
            outside_lavatory   | 1.0      | 1.0      | 0.0      
        attic                  | 1.0      | 0.75     | 0.0125   
        kitchen                | 1.0      | 0.1      | 0.045    
        living_rooms           | 1.0      | 0.25     | 0.0375   
            lounge             | 1.0      | 0.0      | 0.0125   
            dining_room        | 1.0      | 0.0      | 0.0125   
            conservatory       | 1.0      | 0.0      | 0.0125   
            playroom           | 1.0      | 1.0      | 0.0      
        basement               | 1.0      | 0.0      | 0.05     
        garage                 | 1.0      | 0.0      | 0.05     
        garden                 | 1.0      | 0.8      | 0.01     
    house2                     | 60.0     | 0.46111  | 0.32333  
        upstairs               | 1.0      | 0.15     | 0.17     
            bedrooms           | 1.0      | 0.0      | 0.05     
                suite_1        | 1.0      | 0.0      | 0.0125   
                suite_2        | 1.0      | 0.0      | 0.0125   
                bedroom_3      | 1.0      | 0.0      | 0.0125   
                bedroom_4      | 1.0      | 0.0      | 0.0125   
            bathroom           | 1.0      | 0.0      | 0.05     
            toilet             | 1.0      | 0.0      | 0.05     
            attics             | 1.0      | 0.6      | 0.02     
        groundfloor            | 1.0      | 0.31667  | 0.13667  
            kitchen            | 1.0      | 0.0      | 0.03333  
            living_rooms       | 1.0      | 0.0      | 0.03333  
                lounge         | 1.0      | 0.0      | 0.00833  
                dining_room    | 1.0      | 0.0      | 0.00833  
                conservatory   | 1.0      | 0.0      | 0.00833  
                playroom       | 1.0      | 0.0      | 0.00833  
            wet_room_&_toilet  | 1.0      | 0.0      | 0.03333  
            garage             | 1.0      | 0.0      | 0.03333  
            garden             | 1.0      | 0.9      | 0.00333  
            hot_tub_suite      | 1.0      | 1.0      | 0.0      
        basement               | 1.0      | 0.91667  | 0.01667  
            cellars            | 1.0      | 1.0      | 0.0      
            wine_cellar        | 1.0      | 1.0      | 0.0      
            cinema             | 1.0      | 0.75     | 0.01667 
```



## Racket


To save on paper, the coverage table needs to be saved to a file
(in this case <code>data/functional-coverage.txt</code>).


```racket
#lang racket/base
(require racket/list racket/string racket/match racket/format racket/file)

(struct Coverage (name weight coverage weighted-coverage children) #:transparent #:mutable)

;; -| read/parse |------------------------------------------------------------------------------------
(define (build-hierarchies parsed-lines)
  (define inr
    (match-lambda
      ['() (values null null)]
      [`((,head-indent . ,C) ,tail-lines ...)
       (define child? (match-lambda [(cons i _) #:when (> i head-indent) #t] [_ #f]))
       (define-values (chlds rels) (splitf-at tail-lines child?))
       (define-values (rels-tree rels-rem) (inr rels))
       (values (cons (struct-copy Coverage C (children (build-hierarchies chlds))) rels-tree)
               rels-rem)]))
  (define-values (hierarchies remaining-lines) (inr parsed-lines))
  hierarchies)

(define report-line->indent.c/e-line
  (match-lambda
    [(regexp #px"^( *)([^ ]*) *\\| *([^ ]*) *\\| *([^ ]*) *\\|$"
             (list _
                   (app string-length indent-length)
                   name
                   (or (and (not "") (app string->number wght)) (app (λ (x) 1) wght))
                   (or (and (not "") (app string->number cvrg)) (app (λ (x) 0) cvrg))))
     (cons indent-length (Coverage name wght cvrg 0 #f))]))

(define (report->indent.c/e-list rprt)
  (map report-line->indent.c/e-line (drop (string-split rprt "\n") 1)))

;; -| evaluate |--------------------------------------------------------------------------------------
(define find-wght-cvrg
  (match-lambda
    [(and e (Coverage _ w c _ '())) (struct-copy Coverage e (weighted-coverage (* w c)))]
    [(and e (Coverage _ _ _ _ `(,(app find-wght-cvrg (and cdn+ (Coverage _ c-ws _ c-w/cs _))) ...)))
     (define chld-wghtd-avg (for/sum ((w (in-list c-ws)) (w/c (in-list c-w/cs))) (* w w/c)))
     (struct-copy Coverage e (weighted-coverage (/ chld-wghtd-avg (apply + c-ws))) (children cdn+))]))

;; -| printing |--------------------------------------------------------------------------------------
(define max-description-length
  (match-lambda
    [(Coverage (app string-length name-length) _ _ _
               (list (app max-description-length children-lengths) ...))
     (apply max name-length (map add1 children-lengths))]))

(define (~a/right w x)
  (~a x #:width w #:align 'right))

(define (~a/decimal n dec-dgts)
  (~a/right (+ dec-dgts 3) (if (zero? n) "" (real->decimal-string n dec-dgts))))

(define (print-coverage-tree tree)
  (define mdl (max-description-length tree))
  (printf "| ~a |WEIGT| COVER |WGHTD CVRG|~%" (~a "NAME" #:width mdl #:align 'center))
  (let inr ((depth 0) (tree tree))
    (unless (null? tree)
      (match tree
        [(Coverage name w c w/c chlds)
         (printf "| ~a | ~a | ~a | ~a |~%"
                 (~a (string-append (make-string depth #\space) name) #:width mdl)
                 (~a/right 3 w) (~a/decimal c 2) (~a/decimal w/c 5))
         (for ((c chlds)) (inr (add1 depth) c))]))))

;; ---------------------------------------------------------------------------------------------------
(module+ main
;; data/functional-coverage.txt contains a verbatim copy of
;; the table in the task's description
(for-each
 (compose print-coverage-tree find-wght-cvrg)
 (build-hierarchies (report->indent.c/e-list (file->string "data/functional-coverage.txt")))))
```


{{out}}


```txt
|         NAME         |WEIGT| COVER |WGHTD CVRG|
| cleaning             |   1 |       |  0.40917 |
|  house1              |  40 |       |  0.33125 |
|   bedrooms           |   1 |  0.25 |  0.25000 |
|   bathrooms          |   1 |       |  0.50000 |
|    bathroom1         |   1 |  0.50 |  0.50000 |
|    bathroom2         |   1 |       |          |
|    outside_lavatory  |   1 |  1.00 |  1.00000 |
|   attic              |   1 |  0.75 |  0.75000 |
|   kitchen            |   1 |  0.10 |  0.10000 |
|   living_rooms       |   1 |       |  0.25000 |
|    lounge            |   1 |       |          |
|    dining_room       |   1 |       |          |
|    conservatory      |   1 |       |          |
|    playroom          |   1 |  1.00 |  1.00000 |
|   basement           |   1 |       |          |
|   garage             |   1 |       |          |
|   garden             |   1 |  0.80 |  0.80000 |
|  house2              |  60 |       |  0.46111 |
|   upstairs           |   1 |       |  0.15000 |
|    bedrooms          |   1 |       |          |
|     suite_1          |   1 |       |          |
|     suite_2          |   1 |       |          |
|     bedroom_3        |   1 |       |          |
|     bedroom_4        |   1 |       |          |
|    bathroom          |   1 |       |          |
|    toilet            |   1 |       |          |
|    attics            |   1 |  0.60 |  0.60000 |
|   groundfloor        |   1 |       |  0.31667 |
|    kitchen           |   1 |       |          |
|    living_rooms      |   1 |       |          |
|     lounge           |   1 |       |          |
|     dining_room      |   1 |       |          |
|     conservatory     |   1 |       |          |
|     playroom         |   1 |       |          |
|    wet_room_&_toilet |   1 |       |          |
|    garage            |   1 |       |          |
|    garden            |   1 |  0.90 |  0.90000 |
|    hot_tub_suite     |   1 |  1.00 |  1.00000 |
|   basement           |   1 |       |  0.91667 |
|    cellars           |   1 |  1.00 |  1.00000 |
|    wine_cellar       |   1 |  1.00 |  1.00000 |
|    cinema            |   1 |  0.75 |  0.75000 |
```

