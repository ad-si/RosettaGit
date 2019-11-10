+++
title = "Shell"
description = ""
date = 2016-08-15T00:28:46Z
aliases = []
[extra]
id = 18270
[taxonomies]
categories = []
tags = []
+++

<!-- =Task in Shell(s)= -->
{{collection|99 Bottles of Beer}}
[[implementation of task::99 Bottles of Beer| ]]

[[99 Bottles of Beer]] done in any of the Shell-languages.





## AutoHotkey


```AutoHotkey
; RC: 99 bottles of beer
   b = 99
   Loop, %b% {
      s .= b . " bottles of beer on the wall,`n"
        . b . " bottles of beer.`nTake one down, pass it around,`n"
        . b-1 . " bottles of beer on the wall.`n`n"
      b--
   }
   Gui, Add, Edit, w200 h200, %s%
   Gui, Show, , 99 bottles of beer
Return ; end of auto-execute section

GuiClose:
   ExitApp
Return
```


Delayed Sing along

```AutoHotkey
n=99
Gui, Font, s20 cMaroon, Comic Sans MS
Gui, Add, Text, w500 vLyrics, %n% bottles of beer on the wall...
Gui, Show
Loop {
 Sleep, 2000
 GuiControl,,Lyrics,% n!=1 ? n " bottles of beer.":n " bottle of beer."
 Sleep, 2000
 GuiControl,,Lyrics,% n ? "Take one down, pass it around...":"Go to the store, buy some more..."
 Sleep, 2000
 n := n ? --n:99
 GuiControl,,Lyrics,% n!=1 ? n " bottles of beer on the wall.":n " bottle of beer on the wall."
 Sleep, 2000
 GuiControl,,Lyrics,% n!=1 ? n " bottles of beer on the wall...":n " bottle of beer on the wall..."
}
GuiClose:
ExitApp
```


Fast and Short:

```AutoHotkey
b=99
Loop, %b% {
s := b " bottles of beer on the wall, " b " bottles of beer, Take one down, pass it around " b-1 " bottles of beer on the wall"
b--
TrayTip,,%s%
sleep, 40
}
```


With a GUI and slight grammatical variation:

```AutoHotkey
N=o more
Z=99
L:=Z M:=(B:=" bottle")"s"
Loop 99
V.=L (W:=(O:=" of beer")" on the wall")",`n"L O ",`nTake one down and pass it around,`n"(L:=(--Z ? Z:"N"N)(Z=1 ? B:M))W ".`n`n"
Gui,Add,Edit,w600 h250,% V L W ", n"N M O ".`nGo to the store and buy some more, 99"M W "."
Gui,Show
Return
GuiClose:
ExitApp
```


Recursive with slight grammatical variation:

```AutoHotkey
99bottles()
esc::exitapp

99bottles(x=99) {
  ToolTip, % Format("{1:} {2:} of beer on the wall, {1:L} {2:} of beer.{4:}{3:} {2:} of beer on the wall!"
  ,(x?x:"No more")
  ,(x=1?"bottle":"bottles")
  ,(x=1?"no more":x=0?99:x-1)
  ,(x?"`nYou take one down pass it around, ":"`nGo to the store and buy some more, ")),500,300
  sleep 99
  x?99bottles(x-1):return
}
```



## AutoIt


```AutoIt
local $bottleNo=99
local $lyrics=" "

While $bottleNo<>0
	If $bottleNo=1 Then
		$lyrics&=$bottleNo & " bottles of beer on the wall" & @CRLF
		$lyrics&=$bottleNo & " bottles of beer" & @CRLF
		$lyrics&="Take one down, pass it around" & @CRLF
	Else
		$lyrics&=$bottleNo & " bottles of beer on the wall" & @CRLF
		$lyrics&=$bottleNo & " bottles of beer" & @CRLF
		$lyrics&="Take one down, pass it around" & @CRLF
	EndIf
	If $bottleNo=1 Then
		$lyrics&=$bottleNo-1 & " bottle of beer" & @CRLF
	Else
		$lyrics&=$bottleNo-1 & " bottles of beer" & @CRLF
	EndIf
	$bottleNo-=1
WEnd
MsgBox(1,"99",$lyrics)
```


Easier to read output to Console:

```AutoIt

$bottles = 99
$lyrics1 = " bottles of beer on the wall. "
$lyrics2 = " bottles of beer. Take one down and pass it around. "

	For $i = $bottles To 1 Step -1
		If $i = 1 Then
			$lyrics1 = " bottle of beer on the wall. "
			$lyrics2 = " bottle of beer. Take one down and pass it around. "
			$lyrics3 = " Go to the store and get some more! No bottles of beer on the wall!"
			ConsoleWrite($bottles & $lyrics1 & $bottles & $lyrics2 & $lyrics3 & @CRLF)
		Else
			ConsoleWrite($bottles & $lyrics1 & $bottles & $lyrics2 & $bottles - 1 & $lyrics1 & @CRLF)
			$bottles -= 1
		EndIf
	Next

```



## Batch File



```dos
@echo off
setlocal
:main
for /L %%i in (99,-1,1) do (
	call :verse %%i
)
echo no bottles of beer on the wall
echo no bottles of beer
echo go to the store and buy some more
echo 99 bottles of beer on the wall
echo.
set /p q="Keep drinking? "
if %q% == y goto main
if %q% == Y goto main
goto :eof

:verse
call :plural %1 res
echo %res% of beer on the wall
echo %res% of beer
call :oneit %1 res
echo take %res% down and pass it round
set /a c=%1-1
call :plural %c% res
echo %res% of beer on the wall
echo.
goto :eof

:plural
if %1 gtr 1 goto :gtr
if %1 equ 1 goto :equ
set %2=no bottles
goto :eof
:gtr
set %2=%1 bottles
goto :eof
:equ
set %2=1 bottle
goto :eof

:oneit
if %1 equ 1 (
	set %2=it
) else (
	set %2=one
)
goto :eof
```



## friendly interactive shell


```fishshell
set i 99
# Assign s to variable $s
set s s
while test $i != 'No more'
    echo $i bottle$s of beer on the wall,
    echo $i bottle$s of beer.
    echo Take one down, pass it around,
    set i (math $i - 1)
    if test $i -eq 1
        set s ""
    else if test $i -eq 0
        set i 'No more'
    end
    echo $i bottle$s of beer on the wall.
    if test $i != 'No more'
        echo
    end
end
```



## PowerShell


### A standard impementation using a For loop


```PowerShell
for($n=99; $n -gt 0; $n--) {
   "$n bottles of beer on the wall"
   "$n bottles of beer"
   "Take one down, pass it around"
   [string]($n-1) + " bottles of beer on the wall"
   ""
}
```




### My standard implementation using for loop


```PowerShell
[int]$i = 99;
for($i=99; $i -gt 0; $i--) {
    write-host $i  " bottles of beer on the wall";
    write-host $i  " bottles of beer";
    write-host "Take one down, pass it around"
    write-host ($i-1) " bottles of beer on the wall"
    write-host ""
}

```



### Consolidating the static text and using a Do...while loop


```PowerShell
$n=99
do {
   "{0} bottles of beer on the wall`n{0} bottles of beer`nTake one down, pass it around`n{1} bottles of beer on the wall`n" -f $n, --$n
} while ($n -gt 0)
```



### Consolidating the static text and using a Do...until loop


```PowerShell
$n=99
do {
   "{0} bottles of beer on the wall`n{0} bottles of beer`nTake one down, pass it around`n{1} bottles of beer on the wall`n" -f $n, --$n
} until ($n -eq 0)
```




### Consolidating the static text even more


```PowerShell
$s = "{0} bottles of beer on the wall`n{0} bottles of beer`nTake one down, pass it around`n{1} bottles of beer on the wall`n"
$n=99
do { $s -f $n, --$n } while ($n -gt 0)
```



### Using the Pipeline


```Powershell
99..1 | ForEach-Object {
    $s=$( if( $_ -ne 1 ) { 's' } else { '' } )
    $s2=$( if( $_ -ne 2 ) { 's' } else { '' } )
    "$_ bottle$s of beer on the wall`n$_ bottle$s of beer`nTake one down`npass it around`n$( $_ - 1 ) bottle$s2 of beer on the wall`n"}
```




## ProDOS


```ProDOS
editvar /newvar /value=a=99
:a
printline -a- bottles of beer on the wall
printline -a- bottles of beer
printline Take one down, pass it round
editvar /newvar /value=a=-a-1
if -a- /hasvalue 1 goto :1
printline -a- bottles of beer on the wall.
goto :a
:1
printline 1 bottle of beer on the wall
printline 1 bottle of beer
printline take it down, pass it round
printline no bottles of beer on the wall.
editvar /newvar /value=b /userinput=1 /title=Keep drinking?
if -b- /hasvalue yes goto :a else exitprogram
```



## UNIX Shell

{{works with|Bourne Shell}}

```bash
#!/bin/sh

i=99 s=s

while [ $i -gt 0 ]; do
        echo "$i bottle$s of beer on the wall"
        echo "$i bottle$s of beer
Take one down, pass it around"
        # POSIX allows for $(( i - 1 )) but some older Unices didn't have that
        i=`expr $i - 1`
	[ $i -eq 1 ] && s=
        echo "$i bottle$s of beer on the wall
"
done
```


{{works with|Bash}}
{{works with|ksh93}}
{{works with|zsh}}

```bash
bottles() {
  beer=$1
  [ $((beer)) -gt 0 ] && echo -n $beer ||  echo -n "No more"
  echo -n " bottle"
  [ $((beer)) -ne 1 ] && echo -n "s"
  echo -n " of beer"
}

for ((i=99;i>=0;i--)); do
  ((remaining=i))
  echo "$(bottles $remaining) on the wall"
  echo "$(bottles $remaining)"
  if [ $((remaining)) -eq 0 ]; then
    echo "Go to the store and buy some more"
    ((remaining+=99))
  else
    echo "Take one down, pass it around"
    ((remaining--))
  fi
  echo "$(bottles $remaining) on the wall"
  echo
done
```


=
## C Shell
=
See [[99 Bottles of Beer/Shell]]

```csh
@ i=99
set s=s
while ($i > 0)
	echo "$i bottle$s of beer on the wall"
	echo "$i bottle$s of beer"
	echo "Take one down, pass it around"
	@ i = $i - 1
	if ($i == 1) then
		set s=
	else
		set s=s
	endif
	echo "$i bottle$s of beer on the wall"
	echo ""
end
```


=
## es
=
es - extensible shell


```es
i = 99
s = s
while {test $i -gt 0} {
	echo $i bottle$s of beer on the wall
	echo $i bottle$s of beer
	echo Take one down, pass it around
	i = `{expr $i - 1}
	if {test $i -eq 1} {s = ''} {s = s}
	echo $i bottle$s of beer on the wall
	echo
}
```



## UnixPipes


```bash
# Unix Pipes, avoiding all the turing complete sub programs like sed, awk, dc etc.
mkdir 99 || exit 1
trap "rm -rf 99" 1 2 3 4 5 6 7 8

(cd 99
   mkfifo p.b1 p.b2 p.verse1 p.wall p.take
   yes "on the wall" > p.wall &
   yes "Take one down and pass it around, " > p.take &
   (yes "bottles of beer" | nl -s\ | head -n 99 | tac | head -n 98 ;
    echo "One bottle of beer";
    echo "No more bottles of beer") | tee p.b1 p.b2 |
   paste -d"\ " - p.wall p.b1 p.take | head -n 99 > p.verse1 &
   cat p.b2 | tail -99 | paste -d"\ " p.verse1 - p.wall | head -n 99
)
rm -rf 99
```

