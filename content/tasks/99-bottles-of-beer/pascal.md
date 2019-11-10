+++
title = "Pascal"
description = ""
date = 2015-09-05T01:18:18Z
aliases = []
[extra]
id = 18271
[taxonomies]
categories = []
tags = []
+++

<!--
=Pascal=
-->
{{collection|99 Bottles of Beer}}
[[99 Bottles of Beer]] done in Pascal-languages
__toc__

<!--
See [[99 Bottles of Beer/Pascal]]
-->


## Component Pascal

BlackBox Component Builder

```oberon2

MODULE BottlesOfBeer;
IMPORT StdLog;
CONST bottles = 99;

PROCEDURE Part(i: INTEGER);
BEGIN
	StdLog.Int(i);StdLog.String(" bottles of beer on the wall");StdLog.Ln;
	StdLog.Int(i);StdLog.String(" bottles of beer");StdLog.Ln;
	StdLog.String("Take one down, pass it around");StdLog.Ln;
	StdLog.Int(i - 1);StdLog.String(" bottles of beer on the wall.");StdLog.Ln;
	StdLog.Ln
END Part;

PROCEDURE Sing*;
VAR
	i: INTEGER;
BEGIN
	FOR i := bottles TO 1 BY -1 DO
		Part(i)
	END
END Sing;
END BottlesOfBeer.

```

Execute: ^Q BottlesOfBeer.Sing<br/>
Output:

```txt

 99 bottles of beer on the wall
 99 bottles of beer
Take one down, pass it around
 98 bottles of beer on the wall.

 98 bottles of beer on the wall
 98 bottles of beer
Take one down, pass it around
 97 bottles of beer on the wall.

 97 bottles of beer on the wall
 97 bottles of beer
Take one down, pass it around
 96 bottles of beer on the wall.

...


 1 bottles of beer on the wall
 1 bottles of beer
Take one down, pass it around
 0 bottles of beer on the wall.

```




## Delphi

:''See [[#Pascal|Pascal]]''

:''Or


```Delphi
program Hundred_Bottles;

{$APPTYPE CONSOLE}

uses SysUtils;

const C_1_Down = 'Take one down, pass it around' ;

Var i : Integer ;

// As requested, some fun : examples of Delphi basic techniques. Just to make it a bit complex

procedure WriteABottle( BottleNr : Integer ) ;
begin
  Writeln(BottleNr, ' bottles of beer on the wall' ) ;
end ;

begin
  for i := 99 Downto 1 do begin
  WriteABottle(i);
  Writeln( Format('%d bottles of beer' , [i] ) ) ;
  Writeln( C_1_Down ) ;
  WriteABottle(i-1);
  Writeln ;
End ;

end.
```




## Pascal


```pascal
program BottlesOfBeer;

var
    i: integer;

begin
    for i := 99 downto 1 do
        if i <> 1 then
            begin
                writeln(i, ' bottles of beer on the wall');
                writeln(i, ' bottles of beer');
                writeln('Take one down, pass it around');
                if i = 2 then
                    writeln('One bottle of beer on the wall')
                else
                    writeln(i - 1, ' bottles of beer on the wall');
                writeln;
            end
        else
            begin
                writeln('One bottle of beer on the wall');
                writeln('One bottle of beer');
                writeln('Take one down, pass it around');
                writeln('No more bottles of beer on the wall');
            end
end.
```

