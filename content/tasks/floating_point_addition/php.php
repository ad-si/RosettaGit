<?php
$sum = 0.1 + 0.2;
printf("%.17f\n", $sum);             // 0.30000000000000004
var_dump($sum == 0.3);               // bool(false)

// bcmath for exact decimal arithmetic
echo bcadd("0.1", "0.2", 1), "\n";   // 0.3
var_dump(bccomp(bcadd("0.1", "0.2", 1), "0.3", 1) === 0);   // bool(true)
