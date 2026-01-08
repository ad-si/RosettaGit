+++
title = "JavaScript"
description = ""
date = 2019-10-18T20:29:35Z
aliases = []
[extra]
id = 16950
[taxonomies]
categories = []
tags = []
+++

{{collection|The Twelve Days of Christmas}} [[implementation of task::The Twelve Days of Christmas| ]]
[The Twelve Days of Christmas](../) done in JavaScript.


## JavaScript

```JavaScript

var days = [
    'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth',
    'tenth', 'eleventh', 'twelfth',
];

var gifts = [
    "A partridge in a pear tree",
    "Two turtle doves",
    "Three french hens",
    "Four calling birds",
    "Five golden rings",
    "Six geese a-laying",
    "Seven swans a-swimming",
    "Eight maids a-milking",
    "Nine ladies dancing",
    "Ten lords a-leaping",
    "Eleven pipers piping",
    "Twelve drummers drumming"
];

var lines, verses = [], song;

for ( var i = 0; i < 12; i++ ) {

    lines = [];
    lines[0] = "On the " + days[i] + " day of Christmas, my true love gave to me";

    var j = i + 1;
    var k = 0;
    while ( j-- > 0 )
        lines[++k] = gifts[j];


    verses[i] = lines.join('\n');

    if ( i == 0 )
        gifts[0] = "And a partridge in a pear tree";

}

song = verses.join('\n\n');
document.write(song);

```



Alternatively, in a functional style of JavaScript, we can define the ancient song "strPrepn the lstOrdinal[i] strUnit of strHoliday" as an expression, and return that expression in a human-legible and machine-parseable JSON string translation, for further analysis and processing :-)


```JavaScript
JSON.stringify(
  (function (
    strPrepn,
    strHoliday,
    strUnit,
    strRole,
    strProcess,
    strRecipient
  ) {
    var lstOrdinal =
      'first second third fourth fifth sixth\
           seventh eighth ninth tenth eleventh twelfth'
      .split(/\s+/),
      lngUnits = lstOrdinal.length,

      lstGoods =
      'A partridge in a pear tree.\
           Two turtle doves\
           Three french hens\
           Four calling birds\
           Five golden rings\
           Six geese a-laying\
           Seven swans a-swimming\
           Eight maids a-milking\
           Nine ladies dancing\
           Ten lords a-leaping\
           Eleven pipers piping\
           Twelve drummers drumming'
      .split(/\s{2,}/),

      lstReversed = (function () {
        var lst = lstGoods.slice(0);
        return (lst.reverse(), lst);
      })(),

      strProvenance = [strRole, strProcess, strRecipient + ':'].join(' '),

      strPenultimate = lstReversed[lngUnits - 2] + ' and',
      strFinal = lstGoods[0];

    return lstOrdinal.reduce(
      function (sofar, day, i) {
        return sofar.concat(
          [
            [
              [ // abstraction of line 1
                strPrepn,
                'the',
                lstOrdinal[i],
                strUnit,
                'of',
                strHoliday
              ].join(' '),
              strProvenance
            ].concat( // reversed descent through memory
              (i > 1 ? [lstGoods[i]] : []).concat(
                lstReversed.slice(
                  lngUnits - i,
                  lngUnits - 2
                )
              ).concat( // penultimate line ends with 'and'
                [
                  strPenultimate,
                  strFinal
                ].slice(i ? 0 : 1)
              )
            )
          ]
        );
      }, []
    );
  })(
    'On', 'Christmas', 'day', 'my true love', 'gave to', 'me'
  ), null, 2
);
```


Note that the Google Closure compiler's translation of this would be half the size, but rather less legible.
(It does make interesting suggestions though – the semi-colon segmentation of the verses below is a trick that might be worth remembering).


```JavaScript
JSON.stringify(function (h, k, l, f, m, n) {
  var c =
    "first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth"
    .split(" "),
    d = c.length,
    e =
    "A partridge in a pear tree.;Two turtle doves;Three french hens;Four calling birds;Five golden rings;Six geese a-laying;Seven swans a-swimming;Eight maids a-milking;Nine ladies dancing;Ten lords a-leaping;Eleven pipers piping;Twelve drummers drumming"
    .split(";"),
    g = function () {
      var b = e.slice(0);
      return b.reverse(), b;
    }(),
    p = [f, m, n + ":"].join(" "),
    q = g[d - 2] + " and",
    r = e[0];

  return c.reduce(function (b, f, a) {
    return b.concat([[[h, "the", c[a], l, "of", k].join(" "), p].concat((1 <
      a ? [e[a]] : []).concat(g.slice(d - a, d - 2)).concat([q, r].slice(a ?
      0 : 1)))]);
  }, []);
}("On", "Christmas", "day", "my true love", "gave to", "me"), null, 2);
```


Formatted JSON output (the expanded and Closure-compiled versions above both yield the same output).


```JavaScript
[
  [
    "On the first day of Christmas",
    "my true love gave to me:",
    "A partridge in a pear tree."
  ],
  [
    "On the second day of Christmas",
    "my true love gave to me:",
    "Two turtle doves and",
    "A partridge in a pear tree."
  ],
  [
    "On the third day of Christmas",
    "my true love gave to me:",
    "Three french hens",
    "Two turtle doves and",
    "A partridge in a pear tree."
  ],
  [
    "On the fourth day of Christmas",
    "my true love gave to me:",
    "Four calling birds",
    "Three french hens",
    "Two turtle doves and",
    "A partridge in a pear tree."
  ],
  [
    "On the fifth day of Christmas",
    "my true love gave to me:",
    "Five golden rings",
    "Four calling birds",
    "Three french hens",
    "Two turtle doves and",
    "A partridge in a pear tree."
  ]

//... etc.

]
```
