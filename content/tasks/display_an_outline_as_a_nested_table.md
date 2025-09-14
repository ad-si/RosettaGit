+++
title = "Display an outline as a nested table"
description = ""
date = 2019-10-20T23:06:59Z
aliases = []
[extra]
id = 22595
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "javascript",
]
+++

{| class="wikitable" style="text-align: center;"
|-
| style="background: #ffffe6; " colspan=7 | Display an outline as a nested table.
|-
| style="background: #ffebd2; " colspan=3 | Parse the outline to a tree,
| style="background: #f0fff0; " colspan=2 | count the leaves descending from each node,
| style="background: #e6ffff; " colspan=2 | and write out a table with 'colspan' values
|-
| style="background: #ffebd2; " | measuring the indent of each line,
| style="background: #ffebd2; " | translating the indentation to a nested structure,
| style="background: #ffebd2; " | and padding the tree to even depth.
| style="background: #f0fff0; " | defining the width of a leaf as 1,
| style="background: #f0fff0; " | and the width of a parent node as a sum.
| style="background: #e6ffff; " | either as a wiki table,
| style="background: #e6ffff; " | or as HTML.
|-
|  | 
|  | 
|  | 
|  | 
| style="background: #f0fff0; " | (The sum of the widths of its children)
|  | 
|  | 
|}

The graphic representation of outlines is a staple of mind-mapping and the planning of papers, reports, and speeches.

## Task

Given a outline with at least 3 levels of indentation, for example:


```txt
Display an outline as a nested table.
    Parse the outline to a tree,
        measuring the indent of each line,
        translating the indentation to a nested structure,
        and padding the tree to even depth.
    count the leaves descending from each node,
        defining the width of a leaf as 1,
        and the width of a parent node as a sum.
            (The sum of the widths of its children)
    and write out a table with 'colspan' values
        either as a wiki table,
        or as HTML.
```


write a program in your language which translates your outline into a nested table, with WikiTable or HTML [[wp:Wikipedia:Advanced_table_formatting|colspan]] values attached (where needed) to parent nodes in the nested table.

The WikiTable at the top of this page was generated from the indented outline shown above, producing the following markup string:


```txt
{| class="wikitable" style="text-align: center;"
|-
| style="background: #ffffe6; " colspan=7 | Display an outline as a nested table.
|-
| style="background: #ffebd2; " colspan=3 | Parse the outline to a tree,
| style="background: #f0fff0; " colspan=2 | count the leaves descending from each node,
| style="background: #e6ffff; " colspan=2 | and write out a table with 'colspan' values
|-
| style="background: #ffebd2; " | measuring the indent of each line,
| style="background: #ffebd2; " | translating the indentation to a nested structure,
| style="background: #ffebd2; " | and padding the tree to even depth.
| style="background: #f0fff0; " | defining the width of a leaf as 1,
| style="background: #f0fff0; " | and the width of a parent node as a sum.
| style="background: #e6ffff; " | either as a wiki table,
| style="background: #e6ffff; " | or as HTML.
|-
|  | 
|  | 
|  | 
|  | 
| style="background: #f0fff0; " | (The sum of the widths of its children)
|  | 
|  | 
|}
```


;Extra credit:
Use background color to distinguish the main stages of your outline.



;Output:
Display your nested table on this page.




## JavaScript


```javascript
(() => {
    'use strict';

    // main :: IO ()
    const main = () => {
        const outline = `Display an outline as a nested table.
    Parse the outline to a tree,
        measuring the indent of each line,
        translating the indentation to a nested structure,
        and padding the tree to even depth.
    count the leaves descending from each node,
        defining the width of a leaf as 1,
        and the width of a parent node as a sum.
            (The sum of the widths of its children)
    and write out a table with 'colspan' values
        either as a wiki table,
        or as HTML.`

        console.log(
            wikiTableFromForest(
                forestOfEvenDepth(
                    map(compose(
                        coloredKeyLines(Object.keys(dictColors)),
                        paintedTree('backColor')('yellow'),
                        measuredTree
                    ))(
                        forestFromOutline(outline)
                    )
                )
            )
        );
    };

    // TRANSLATION OF OUTLINE TO NESTED TABLE -----------------

    // forestFromOutline :: String -> [Tree String]
    const forestFromOutline = s =>
        // A list of trees, derived from an
        // indented outline text.
        forestFromLineIndents(
            indentLevelsFromLines(lines(s))
        );

    // wikiTableFromForest :: [Tree (String, Dict)] -> String
    const wikiTableFromForest = forest => {
        // Lines of wiki markup representing a nested tree
        // with 'colspan' values for parent nodes,
        // and varying background colors.
        const tableRows = trees => {
            const rows = tail(levels(Node('virtual')(trees)));
            return unlines(concatMap(row =>
                cons('|-')(
                    map(cell => {
                        const
                            dct = cell[1],
                            color = dct.backColor,
                            width = dct.leafSum;
                        return '| ' + (
                            Boolean(color) ? (
                                'style="background: ' +
                                dictColors[color] + '; "'
                            ) : ''
                        ) + (
                            1 < width ? (
                                ' colspan=' + str(width)
                            ) : ''
                        ) + ' | ' + cell[0];
                    })(row)
                )
            )(rows));
        };
        return '{| class="wikitable" style="text-align: center;"\n' +
            tableRows(forest) +
            '\n|}'
    };

    // indentLevelsFromLines :: [String] -> [(Int, String)]
    const indentLevelsFromLines = xs => {
        // A list of (indentLevel, trimmed Text) tuples.
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

    // forestFromLineIndents :: [(Int, String)] -> [Tree String]
    const forestFromLineIndents = tuples => {
        // A list of nested trees derived from
        // a list of indented lines.
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

    // forestOfEvenDepth :: [Tree (a, Dict)] -> [Tree (a, Dict)]
    const forestOfEvenDepth = measuredTrees => {
        // A tree padded downwards so that every branch
        // descends to the same depth.
        const go = n => tree =>
            1 >= n ? (
                tree
            ) : Node(tree.root)(
                0 < tree.nest.length ? (
                    tree.nest.map(go(n - 1))
                ) : [Node(Tuple('')({}))([])]
            );
        return measuredTrees.map(go(
            1 + maximumBy(x => root(x)[1].layerSum)(
                measuredTrees
            ).root[1].layerSum
        ));
    };

    // BACKGROUND COLOURS FOR SECTIONS OF THE TREE ----

    // coloredKeyLines :: [String] ->
    // Tree (String, Dict) -> Tree (String, Dict)
    const coloredKeyLines = colorNames => node =>
        Node(root(node))(
            zipWith(paintedTree('backColor'))(
                take(node.nest.length)(
                    cycle(tail(colorNames))
                )
            )(node.nest)
        );

    // paintedTree :: String -> a -> Tree (b, dict) -> Tree (b, dict)
    const paintedTree = k => v => node => {
        const go = x =>
            Node(Tuple(root(x)[0])(
                insertDict(k)(v)(root(x)[1])
            ))(nest(x).map(go));
        return go(node);
    };

    // dictColors :: Dict
    const dictColors = {
        yellow: '#ffffe6',
        orange: '#ffebd2',
        green: '#f0fff0',
        blue: '#e6ffff',
        pink: '#ffeeff',
        gray: ''
    };


    // GENERIC FUNCTIONS ----------------------------

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

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = f => xs =>
        xs.flatMap(f);

    // cons :: a -> [a] -> [a]
    const cons = x => xs =>
        Array.isArray(xs) ? (
            [x].concat(xs)
        ) : 'GeneratorFunction' !== xs.constructor.constructor.name ? (
            x + xs
        ) : ( // Existing generator wrapped with one additional element
            function*() {
                yield x;
                let nxt = xs.next()
                while (!nxt.done) {
                    yield nxt.value;
                    nxt = xs.next();
                }
            }
        )();

    // cycle :: [a] -> Generator [a]
    function* cycle(xs) {
        const lng = xs.length;
        let i = 0;
        while (true) {
            yield(xs[i])
            i = (1 + i) % lng;
        }
    }

    // div :: Int -> Int -> Int
    const div = x => y => Math.floor(x / y);

    // Lift a simple function to one which applies to a tuple,
    // transforming only the first item of the tuple

    // firstArrow :: (a -> b) -> ((a, c) -> (b, c))
    const firstArrow = f =>
        // A simple function lifted to one which applies
        // to a tuple, transforming only its first item.
        xy => Tuple(f(xy[0]))(
            xy[1]
        );

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f =>
        1 < f.length ? (
            (a, b) => f(b, a)
        ) : (x => y => f(y)(x));

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

    // fromEnum :: Enum a => a -> Int
    const fromEnum = x =>
        typeof x !== 'string' ? (
            x.constructor === Object ? (
                x.value
            ) : parseInt(Number(x))
        ) : x.codePointAt(0);

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // gt :: Ord a => a -> a -> Bool
    const gt = x => y =>
        'Tuple' === x.type ? (
            x[0] > y[0]
        ) : (x > y);

    // insertDict :: String -> a -> Dict -> Dict
    const insertDict = k => v => dct =>
        Object.assign({}, dct, {
            [k]: v
        });

    // isSpace :: Char -> Bool
    const isSpace = c => /\s/.test(c);

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // levels :: Tree a -> [[a]]
    const levels = tree => {
        const xs = [
            [root(tree)]
        ];
        let level = [tree].flatMap(nest);
        while (0 < level.length) {
            xs.push(level.map(root));
            level = level.flatMap(nest);
        }
        return xs;
    };

    // lines :: String -> [String]
    const lines = s => s.split(/[\r\n]/);

    // map :: (a -> b) -> [a] -> [b]
    const map = f => xs =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // max :: Ord a => a -> a -> a
    const max = a => b => gt(b)(a) ? b : a;

    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    const maximumBy = f => xs =>
        0 < xs.length ? (
            xs.slice(1)
            .reduce((a, x) => 0 < f(x)(a) ? x : a, xs[0])
        ) : undefined;

    // measuredTree :: Tree a -> Tree (a, (Int, Int, Int))
    const measuredTree = tree => {
        // A tree in which each node is tupled with
        // a (leafSum, layerSum, nodeSum) measure of its sub-tree,
        // where leafSum is the number of descendant leaves,
        // and layerSum is the number of descendant levels,
        // and nodeSum counts all nodes, including the root.
        // Index is a position in a zero-based top-down
        // left to right series.
        // For additional parent indices, see parentIndexedTree.
        const whni = (w, h, n, i) => ({
            leafSum: w,
            layerSum: h,
            nodeSum: n,
            index: i
        });
        let i = 0;
        return foldTree(
            x => {
                let topDown = i++;
                return xs => Node(
                    Tuple(x)(
                        0 < xs.length ? (() => {
                            const dct = xs.reduce(
                                (a, node) => {
                                    const dimns = node.root[1];
                                    return whni(
                                        a.leafSum + dimns.leafSum,
                                        max(a.layerSum)(
                                            dimns.layerSum
                                        ),
                                        a.nodeSum + dimns.nodeSum,
                                        topDown
                                    );
                                }, whni(0, 0, 0, topDown)
                            );
                            return whni(
                                dct.leafSum,
                                1 + dct.layerSum,
                                1 + dct.nodeSum,
                                topDown
                            );
                        })() : whni(1, 0, 1, topDown)
                    )
                )(xs);
            }
        )(tree);
    };

    // minimum :: Ord a => [a] -> a
    const minimum = xs =>
        0 < xs.length ? (
            foldl1(a => x => x < a ? x : a)(xs)
        ) : undefined;

    // nest :: Tree a -> [a]
    const nest = tree => tree.nest;

    // root :: Tree a -> a
    const root = tree => tree.root;

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

    // str :: a -> String
    const str = x => x.toString();

    // succ :: Enum a => a -> a
    const succ = x => 1 + x;

    // tail :: [a] -> [a]
    const tail = xs => 0 < xs.length ? xs.slice(1) : [];

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
        function() {
            const
                args = Array.from(arguments),
                a = 1 < args.length ? (
                    args
                ) : args[0]; // Tuple object.
            return f(a[0])(a[1]);
        };

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

    // MAIN ---
    return main();
})();
```

{| class="wikitable" style="text-align: center;"
|-
| style="background: #ffffe6; " colspan=7 | Display an outline as a nested table.
|-
| style="background: #ffebd2; " colspan=3 | Parse the outline to a tree,
| style="background: #f0fff0; " colspan=2 | count the leaves descending from each node,
| style="background: #e6ffff; " colspan=2 | and write out a table with 'colspan' values
|-
| style="background: #ffebd2; " | measuring the indent of each line,
| style="background: #ffebd2; " | translating the indentation to a nested structure,
| style="background: #ffebd2; " | and padding the tree to even depth.
| style="background: #f0fff0; " | defining the width of a leaf as 1,
| style="background: #f0fff0; " | and the width of a parent node as a sum.
| style="background: #e6ffff; " | either as a wiki table,
| style="background: #e6ffff; " | or as HTML.
|-
|  | 
|  | 
|  | 
|  | 
| style="background: #f0fff0; " | (The sum of the widths of its children)
|  | 
|  | 
|}
