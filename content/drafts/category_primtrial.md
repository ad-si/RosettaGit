+++
title = "Category:PrimTrial"
description = ""
date = 2015-02-24T19:46:45Z
aliases = []
[extra]
id = 18631
[taxonomies]
categories = []
tags = []
+++

unit primTrial 
{{works with|Free Pascal}} {{works with|Delphi}}
Maybe NativeUint must be typed in older versions to LongWord aka cardinal


```pascal
unit primTrial;
// NativeUInt: LongWord 32-Bit-OS/ Uint64 64-Bit-OS
{$IFDEF FPC}
  {$MODE DELPHI}
  {$Smartlink ON}
  {$OPTIMIZATION ON,Regvar,PEEPHOLE,CSE,ASMCSE}
  {$CODEALIGN proc=32}
{$ENDIF}

interface
type
  ptPrimeList = array of NativeUint;

  procedure InitPrime;

  function actPrime :NativeUint;
  function isPrime(pr: NativeUint):boolean;
  function isAlmostPrime(n: NativeUint;cnt: NativeUint): boolean;
  function isSemiprime(n: NativeUint): boolean;
  function SmallFactor(pr: NativeUint):NativeUint;

  //next prime
  function NextPrime: NativeUint;
  //next possible prime of number wheel
  function NextPosPrim: NativeUint;
  //next prime greater equal limit
  function PrimeGELimit(Limit:NativeUint):NativeUint;
  function PrimeRange(LowLmt,UpLmt:NativeUint): ptPrimeList;
implementation

uses
  sysutils;
const
  cntsmallPrimes = 6;
  smallPrimes : array[0..cntsmallPrimes-1] of NativeUint = (2,3,5,7,11,13);

  wheelSize = (2-1)*(3-1)*(5-1)*(7-1)*(11-1)*(13-1);
  wheelCircumfence = 2*3*5*7*11*13;
var
  deltaWheel : array[0..wheelSize-1] of byte;
  WheelIdx : nativeUint;
  p,pw  : nativeUint;

procedure InitPrime;
//initialies wheel and prime to startposition
Begin
  p := 2;
  pw := 1;
  WheelIdx := 0;
end;

function actPrime :NativeUint;inline;
Begin
  result := p;
end;

procedure InitWheel;
//search for numbers that are no multiples of smallprimes
//saving only the distance, to keep size small
var
  p0,p1,i,d,res : NativeUint;
Begin
  p0 := 1;d := 0;p1 := p0;
  repeat
    Repeat
      p1 := p1+2;// only odd
      i := 1;
      repeat
        res := p1 mod smallPrimes[i];
        inc(i)
      until (res =0)OR(i >= cntSmallPrimes);
      if res <> 0 then
      Begin
        deltaWheel[d] := p1-p0;
        inc(d);
        break;
      end;
    until false;
    p0 := p1;
  until d >= wheelSize;
end;

function biggerFactor(p: NativeUint):NativeUint;
//trial division by wheel numbers
//reduces count of divisions from 1/2 = 0.5( only odd numbers )
//to 5760/30030 = 0.1918
var
  sp : NativeUint;
  d  : NativeUint;
  r  : NativeUint;
Begin
  sp := 1;d := 0;
  repeat
    sp := sp+deltaWheel[d];
    r := p mod sp;
    d := d+1;
    //IF d = WheelSize then d := 0;
    d := d AND NativeUint(-ord(d<>WheelSize));
    IF r = 0 then
      BREAK;
  until p < sp*sp;
  IF r = 0  then
    result := sp
  else
    result := p;
end;

function SmallFactor(pr: NativeUint):NativeUint;
//checking numbers omitted by biggerFactor
var
  k : NativeUint;
Begin
  result := pr;
  IF pr in [2,3,5,7,11,13] then
    EXIT;
  IF NOT(ODD(pr))then Begin result := 2; EXIT end;
  For k := 1 to cntSmallPrimes-1 do
  Begin
    IF pr Mod smallPrimes[k] = 0 then
    Begin
      result := smallPrimes[k];
      EXIT
    end;
  end;
  k  := smallPrimes[cntsmallPrimes-1];
  IF pr>k*k then
    result := biggerFactor(pr);
end;

function isPrime(pr: NativeUint):boolean;
Begin
  IF pr > 1 then
    isPrime := smallFactor(pr) = pr
  else
    isPrime := false;
end;

function isAlmostPrime(n: NativeUint;cnt: NativeUint): boolean;
var
  fac1,c : NativeUint;
begin
  c := 0;
  repeat
    fac1 := SmallFactor(n);
    n := n div fac1;
    inc(c);
  until (n = 1) OR (c > cnt);
  isAlmostPrime := (n = 1) AND (c = cnt);
end;

function isSemiprime(n: NativeUint): boolean;
begin
  result := isAlmostPrime(n,2);
end;

function NextPosPrim: NativeUint;inline;
var
  WI : NativeUint;
Begin
  result := pw+deltaWheel[WheelIdx];
  WI := (WheelIdx+1);
  WheelIdx := WI AND NativeUint(-ORD(WI<>WheelSize));
  pw := result;
end;

function NextPrime: NativeUint;
Begin
  IF p >= smallPrimes[High(smallPrimes)]then
  Begin
    repeat
    until isPrime(NextPosPrim);
    result := pw;
    p := result;
  end
  else
  Begin
    result := 0;
    while p >= smallPrimes[result] do
      inc(result);
    result := smallPrimes[result];
    p:= result;
  end;
end;

function PrimeGELimit(Limit:NativeUint):NativeUint;
//prime greater or equal limit
Begin
  IF Limit > wheelCircumfence then
  Begin
    WheelIdx:= wheelSize-1;
    result := (Limit DIV wheelCircumfence)*wheelCircumfence-1;
    pw := result;
    //the easy way, no prime test
    while pw <= Limit do
      NextPosPrim;
    result := pw;
    p := result;
    if Not(isPrime(result)) then
      result := NextPrime;
  end
  else
  Begin
    InitPrime;
    repeat
    until (NextPosPrim >= limit) AND isPrime(pw);
    result := pw;
    p := result;
  end;
end;

function PrimeRange(LowLmt,UpLmt:NativeUint): ptPrimeList;
var
  i,newP : NativeUint;
Begin
  IF LowLmt>UpLmt then
  Begin
    setlength(result,0);
    EXIT;
  end;
  i := 0;
  setlength(result,100);
  newP := PrimeGELimit(LowLmt);

  while newP<= UpLmt do
  Begin
    result[i]:= newP;
    inc(i);
    IF  i>High(result) then
      setlength(result,i*2);
    newP := NextPrime;
  end;
  setlength(result,i);
end;

//initialization
Begin
  InitWheel;
  InitPrime;
end.

```

