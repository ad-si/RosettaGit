+++
title = "Rc"
description = ""
date = 2009-08-30T17:37:32Z
aliases = []
[extra]
id = 2485
[taxonomies]
categories = []
tags = []
+++

{{implementation|UNIX Shell}}[[wp:Rc|'''rc''']] is a simple yet powerful shell written by Tom Duff. Originally used on AT&T's Version 10 UNIX, it is now primarily used under [http://en.wikipedia.org/wiki/Plan_9_from_Bell_Labs Plan 9], where it is the default shell. Despite rc's simplicity, it is powerful enough that many of Plan 9's basic utilities are written using it.

==Examples==
Here is a script to check the weather in a given city. (Retrieved from [http://plan9.bell-labs.com/sources/plan9/rc/bin/weather http://plan9.bell-labs.com/sources/plan9/rc/bin/weather])


```rc
#!/bin/rc
rfork e
DEFAULT=ewr
fn usage{
	echo 'usage: weather 3-letter-city-code' >[1=2]
	echo 'for a list of cities in new york, say' >[1=2]
	echo '	weather ny' >[1=2]
	exit usage
}
switch($#*){
case 0
	arg=$DEFAULT
	if(~ $#weather 1)
		arg=$weather
case 1
	arg=$1
case *
	usage
}
switch($arg){
case [a-zA-Z][a-zA-Z][a-zA-Z]
	script=('' '' 'C' '4' '1' '1' $arg '' '' '' '' 'X')
case [a-zA-Z][a-zA-Z]
	script=('' '' 'C' '4' '1' '3' $arg '' '' '' '' 'X')
case *
	usage
}
{
	for(i in $script)
		echo $i
} |
con -nrl tcp!rainmaker.wunderground.com!telnet |
sed -n '/Enter .-letter .* code:/,/CITY FORECAST MENU/p' |
sed 's/Enter .-letter .* code: //' |
sed 's/   Press Return to continue, M to return to menu, X to exit: //' |
grep -v 'CITY FORECAST MENU' |
tr -d '
' |
sed 's/ *$//' |
uniq
```

