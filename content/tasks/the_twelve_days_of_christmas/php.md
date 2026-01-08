+++
title = "PHP"
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
[The Twelve Days of Christmas](../) done in PHP.


## PHP

```PHP

<?php

header("Content-Type: text/plain; charset=UTF-8");

$days = array(
    'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth',
    'tenth', 'eleventh', 'twelfth',
);

$gifts = array(
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
);

$verses = [];

for ( $i = 0; $i < 12; $i++ ) {
    $lines = [];
    $lines[0] = "On the {$days[$i]} day of Christmas, my true love gave to me";

    $j = $i;
    $k = 0;
    while ( $j >= 0 ) {
        $lines[++$k] = $gifts[$j];
        $j--;
    }

    $verses[$i] = implode(PHP_EOL, $lines);

    if ( $i == 0 )
        $gifts[0] = "And a partridge in a pear tree";
}

echo implode(PHP_EOL . PHP_EOL, $verses);

?>

```


Or using recursion:


```PHP
<?php

$gifts = array(
    'first'    => "A partridge in a pear tree",
    'second'   => "Two turtle doves",
    'third'    => "Three french hens",
    'fourth'   => "Four calling birds",
    'fifth'    => "Five golden rings",
    'sixth'    => "Six geese a-laying",
    'seventh'  => "Seven swans a-swimming",
    'eighth'   => "Eight maids a-milking",
    'ninth'    => "Nine ladies dancing",
    'tenth'    => "Ten lords a-leaping",
    'eleventh' => "Eleven pipers piping",
    'twelfth'  => "Twelve drummers drumming"
);

function print_day( $gifts ) {
    echo "On the ", key( $gifts ), " day of Xmas, my true love gave to me", PHP_EOL;
    foreach( $gifts as $day => $gift ) {
        echo $gift, $day == 'second' ? ' and' : '', PHP_EOL;
    }
    echo PHP_EOL;
}

function twelve_days( $gifts ) {
    if ( ! empty( $gifts ) ) {
        twelve_days( array_slice( $gifts, 1, null, true ) );
        print_day( $gifts );
    }
}

twelve_days( array_reverse( $gifts, true ) );


```

<pre style="font-size:84%;height:55ex">
On the first day of Xmas, my true love gave to me
A partridge in a pear tree

On the second day of Xmas, my true love gave to me
Two turtle doves and
A partridge in a pear tree

On the third day of Xmas, my true love gave to me
Three french hens
Two turtle doves and
A partridge in a pear tree

On the fourth day of Xmas, my true love gave to me
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the fifth day of Xmas, my true love gave to me
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the sixth day of Xmas, my true love gave to me
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the seventh day of Xmas, my true love gave to me
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the eighth day of Xmas, my true love gave to me
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the ninth day of Xmas, my true love gave to me
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the tenth day of Xmas, my true love gave to me
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the eleventh day of Xmas, my true love gave to me
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the twelfth day of Xmas, my true love gave to me
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

```
