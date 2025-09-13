+++
title = "Main step of GOST 28147-89"
description = ""
date = 2019-10-05T20:46:52Z
aliases = []
[extra]
id = 12274
[taxonomies]
categories = ["task", "Encryption"]
tags = []
+++

[[wp:GOST (block cipher)|GOST 28147-89]] is a standard symmetric encryption based on a [[wp:Feistel cipher|Feistel network]].


The structure of the algorithm consists of three levels:
#   encryption modes - simple replacement, application range, imposing a range of feedback and authentication code generation;
#   cycles - 32-З, 32-Р and 16-З, is a repetition of the main step;
#   ''main step'', a function that takes a 64-bit block of text and one of the eight 32-bit encryption key elements, and uses the replacement table (8x16 matrix of 4-bit values), and returns encrypted block.


## Task

Implement the main step of this encryption algorithm.





## BBC BASIC

```bbcbasic
      DIM table&(7,15), test%(1)
      table&() =  4, 10,  9,  2, 13,  8,  0, 14,  6, 11,  1, 12,  7, 15,  5,  3, \
      \          14, 11,  4, 12,  6, 13, 15, 10,  2,  3,  8,  1,  0,  7,  5,  9, \
      \           5,  8,  1, 13, 10,  3,  4,  2, 14, 15, 12,  7,  6,  0,  9, 11, \
      \           7, 13, 10,  1,  0,  8,  9, 15, 14,  4,  6, 12, 11,  2,  5,  3, \
      \           6, 12,  7,  1,  5, 15, 13,  8,  4, 10,  9, 14,  0,  3, 11,  2, \
      \           4, 11, 10,  0,  7,  2,  1, 13,  3,  6,  8,  5,  9, 12, 15, 14, \
      \          13, 11,  4,  1,  3, 15,  5,  9,  0, 10, 14,  7,  6,  8,  2, 12, \
      \           1, 15, 13,  0,  5,  7, 10,  4,  9,  2,  3, 14,  6, 11,  8, 12

      test%() = &043B0421, &04320430
      key% = &E2C104F9
      PROCmainstep(test%(), key%, table&())
      PRINT ~ test%(0) test%(1)
      END

      DEF PROCmainstep(n%(), key%, t&())
      LOCAL i%, s%, cell&, new_s%
      s% = FN32(n%(0) + key%)
      FOR i% = 0 TO 3
        cell& = (s% >>> (i%*8)) AND &FF
        new_s% += (t&(i%*2,cell& MOD 16) + 16*t&(i%*2+1,cell& DIV 16)) << (i%*8)
      NEXT
      s% = ((new_s% << 11) OR (new_s% >>> 21)) EOR n%(1)
      n%(1) = n%(0) : n%(0) = s%
      ENDPROC

      DEF FN32(v)
      WHILE v>&7FFFFFFF : v-=2^32 : ENDWHILE
      WHILE v<&80000000 : v+=2^32 : ENDWHILE
      = v
```

'''Output:'''

```txt

   7CF881F   43B0421

```



## C

Version with packed replacement table.


```C
static unsigned char const k8[16] = {	14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7 };
static unsigned char const k7[16] = {	15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10 };
static unsigned char const k6[16] = {	10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8 };
static unsigned char const k5[16] = {	 7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15 };
static unsigned char const k4[16] = {	 2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9 };
static unsigned char const k3[16] = {	12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11 };
static unsigned char const k2[16] = {	 4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1 };
static unsigned char const k1[16] = {	13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7 };

static unsigned char k87[256];
static unsigned char k65[256];
static unsigned char k43[256];
static unsigned char k21[256];

void
kboxinit(void)
{
	int i;
	for (i = 0; i < 256; i++) {
		k87[i] = k8[i >> 4] << 4 | k7[i & 15];
		k65[i] = k6[i >> 4] << 4 | k5[i & 15];
		k43[i] = k4[i >> 4] << 4 | k3[i & 15];
		k21[i] = k2[i >> 4] << 4 | k1[i & 15];
	}
}

static word32
f(word32 x)
{
	x = k87[x>>24 & 255] << 24 | k65[x>>16 & 255] << 16 |
	    k43[x>> 8 & 255] <<  8 | k21[x & 255];
	return x<<11 | x>>(32-11);
}
```



## C++


```cpp
UINT_64 TGost::SWAP32(UINT_32 N1, UINT_32 N2)
{
    UINT_64 N;
	N = N1;
	N = (N<<32)|N2;
	return UINT_64(N);
}

UINT_32 TGost::ReplaceBlock(UINT_32 x)
{
    register i;
    UINT_32 res = 0UL;
    for(i=7;i>=0;i--)
    {
       ui4_0 = x>>(i*4);
       ui4_0 = BS[ui4_0][i];
       res = (res<<4)|ui4_0;
    }
    return res;
}

UINT_64 TGost::MainStep(UINT_64 N,UINT_32 X)
{
   UINT_32 N1,N2,S=0UL;
   N1=UINT_32(N);
   N2=N>>32;
   S = N1 + X % 0x4000000000000;
   S = ReplaceBlock(S);
   S = (S<<11)|(S>>21);
   S ^= N2;
   N2 = N1;
   N1 = S;
   return SWAP32(N2,N1);
}
```


Variable "BS" is the replacement table.


## D

```d
import std.stdio, std.range, std.algorithm;

/// Rotate uint left.
uint rol(in uint x, in uint nBits) @safe pure nothrow @nogc {
    return (x << nBits) | (x >> (32 - nBits));
}

alias Nibble = ubyte; // 4 bits used.
alias SBox = immutable Nibble[16][8];

private bool _validateSBox(in SBox data) @safe pure nothrow @nogc {
    foreach (const ref row; data)
        foreach (ub; row)
            if (ub >= 16) // Verify it's a nibble.
                return false;
    return true;
}

struct GOST(s...) if (s.length == 1 && s[0]._validateSBox) {
    private static generate(ubyte k)() @safe pure nothrow {
        return k87.length.iota
               .map!(i=> (s[0][k][i >> 4] << 4) | s[0][k - 1][i & 0xF])
               .array;
    }

    private uint[2] buffer;
    private static immutable ubyte[256] k87 = generate!7,
                                        k65 = generate!5,
                                        k43 = generate!3,
                                        k21 = generate!1;

    // Endianess problems?
    private static uint f(in uint x) pure nothrow @nogc @safe {
        immutable uint y = (k87[(x >> 24) & 0xFF] << 24) |
                           (k65[(x >> 16) & 0xFF] << 16) |
                           (k43[(x >>  8) & 0xFF] <<  8) |
                            k21[ x        & 0xFF];
        return rol(y, 11);
    }

    // This performs only a step of the encoding.
    public void mainStep(in uint[2] input, in uint key)
    pure nothrow @nogc @safe {
        buffer[0] = f(key + input[0]) ^ input[1];
        buffer[1] = input[0];
    }
}

void main() {
    // S-boxes used by the Central Bank of Russian Federation:
    // http://en.wikipedia.org/wiki/GOST_28147-89
    // (This is a matrix of nibbles).
    enum SBox cbrf = [
      [ 4, 10,  9,  2, 13,  8,  0, 14,  6, 11,  1, 12,  7, 15,  5,  3],
      [14, 11,  4, 12,  6, 13, 15, 10,  2,  3,  8,  1,  0,  7,  5,  9],
      [ 5,  8,  1, 13, 10,  3,  4,  2, 14, 15, 12,  7,  6,  0,  9, 11],
      [ 7, 13, 10,  1,  0,  8,  9, 15, 14,  4,  6, 12, 11,  2,  5,  3],
      [ 6, 12,  7,  1,  5, 15, 13,  8,  4, 10,  9, 14,  0,  3, 11,  2],
      [ 4, 11, 10,  0,  7,  2,  1, 13,  3,  6,  8,  5,  9, 12, 15, 14],
      [13, 11,  4,  1,  3, 15,  5,  9,  0, 10, 14,  7,  6,  8,  2, 12],
      [ 1, 15, 13,  0,  5,  7, 10,  4,  9,  2,  3, 14,  6, 11,  8, 12]];

    GOST!cbrf g;

    // Example from the talk page (bytes swapped for endianess):
    immutable uint[2] input = [0x_04_3B_04_21, 0x_04_32_04_30];
    immutable uint key = 0x_E2_C1_04_F9;

    g.mainStep(input, key);
    writefln("%(%08X %)", g.buffer);
}
```

```txt
07CF881F 043B0421
```



## JavaScript


```JavaScript
var Таблица_замен = [
  [ 4, 10,  9,  2, 13,  8,  0, 14,  6, 11,  1, 12,  7, 15,  5,  3],
  [14, 11,  4, 12,  6, 13, 15, 10,  2,  3,  8,  1,  0,  7,  5,  9],
  [ 5,  8,  1, 13, 10,  3,  4,  2, 14, 15, 12,  7,  6,  0,  9, 11],
  [ 7, 13, 10,  1,  0,  8,  9, 15, 14,  4,  6, 12, 11,  2,  5,  3],
  [ 6, 12,  7,  1,  5, 15, 13,  8,  4, 10,  9, 14,  0,  3, 11,  2],
  [ 4, 11, 10,  0,  7,  2,  1, 13,  3,  6,  8,  5,  9, 12, 15, 14],
  [13, 11,  4,  1,  3, 15,  5,  9,  0, 10, 14,  7,  6,  8,  2, 12],
  [ 1, 15, 13,  0,  5,  7, 10,  4,  9,  2,  3, 14,  6, 11,  8, 12]];

function ОсновнойШаг(блок_текста, элемент_ключа) {
  var N = блок_текста.slice(0);
  var X = элемент_ключа;
  var S = (N[0] + X) & 0xFFFFFFFF;
  var ячейка; var нов_S = 0;
  for (var сч = 0; сч < 4; сч++) {
    ячейка = (S >>> (сч << 3)) & 0xFF;
    нов_S += (Таблица_замен[сч*2][ячейка & 0x0F] + (Таблица_замен[сч*2+1][ячейка >>> 4] << 4)) << (сч << 3);
  }
  S = (((нов_S << 11) + (нов_S >>> 21)) & 0xFFFFFFFF) ^ N[1];
  N[1] = N[0]; N[0] = S;
  return N;
}
```


Note: the variable "блок_текста" is an array of two 32-bit values that make up the block.


## Glagol

Here the function "БеззнСлжПоМод32" sells unsigned add numbers modulo 2<sup>32</sup>, "ИсклИЛИ" implements bitwise XOR of 32-bit variables, and "Замена" by replacing the specified 4-bit block.
<code>
  '''ПЕР'''
    Ключ: '''РЯД''' 8 '''ИЗ''' '''ЦЕЛ''';
    ТаблицаЗамен: '''РЯД''' 8, 16 '''ИЗ''' '''ЯЧЦЕЛ''';

  '''ЗАДАЧА''' БеззнСлжПоМод32(ч1, ч2: '''ЦЕЛ'''): '''ЦЕЛ''';
  '''ПЕР'''
     память1, память2: '''ШИРЦЕЛ''';
     результат: '''ЦЕЛ''';
  '''УКАЗ'''
    память1 := 0; '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(ч1), '''''ОБХОД'''''.''ПолучитьАдрес''(память1), 4);
    память2 := 0; '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(ч2), '''''ОБХОД'''''.''ПолучитьАдрес''(память2), 4);
    '''УВЕЛИЧИТЬ'''(память1, память2);
    '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(память1), '''''ОБХОД'''''.''ПолучитьАдрес''(результат), 4);
    '''ВОЗВРАТ''' результат
  '''КОН''' БеззнСлжПоМод32;

  '''ЗАДАЧА''' ИсклИЛИ(ч1, ч2: '''ЦЕЛ'''): '''ЦЕЛ''';
  '''УКАЗ'''
    '''ВОЗВРАТ''' '''''ОБХОД'''''.''Значение''('''ЦЕЛ''', '''''ОБХОД'''''.''Значение''('''МНОЖ''', ч1) / '''''ОБХОД'''''.''Значение''('''МНОЖ''', ч2))
  '''КОН''' ИсклИЛИ;

  '''ЗАДАЧА''' Замена(стр: '''ЯЧЦЕЛ'''; яч: '''''ОБХОД'''''.''Ячейка''): '''ЯЧЦЕЛ''';
  '''ПЕР'''
    п1, п2, п3: '''ЦЕЛ'''; результат: '''ЯЧЦЕЛ''';
  '''УКАЗ'''
    п1 := 0; п2 := 0; '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(яч), '''''ОБХОД'''''.''ПолучитьАдрес''(п1), 1);
    п1 := Асм.Сдвиг(п1, 4); '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(п1), '''''ОБХОД'''''.''ПолучитьАдрес''(п2), 1);
    п2 := Асм.Сдвиг(п2, -4); п1 := Асм.Сдвиг(п1, -8);
    п3 := ТаблицаЗамен[стр*2, п2] + Асм.Сдвиг(ТаблицаЗамен[стр*2+1, п1], 4);
    '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(п3), '''''ОБХОД'''''.''ПолучитьАдрес''(результат), 1);
    '''ВОЗВРАТ''' результат
  '''КОН''' Замена;

  '''ЗАДАЧА''' ОсновнойШаг(N: '''ШИРЦЕЛ'''; X: '''ЦЕЛ'''): '''ШИРЦЕЛ''';
  '''ПЕР'''
    N1, N2, S: '''ЦЕЛ''';
    сч: '''ЯЧЦЕЛ''';
    ячейка: '''ЯЧЦЕЛ''';
    результат: '''ШИРЦЕЛ''';
  '''УКАЗ'''
    '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(N), '''''ОБХОД'''''.''ПолучитьАдрес''(N1), 4);
    '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(N)+4, '''''ОБХОД'''''.''ПолучитьАдрес''(N2), 4);
    S := БеззнСлжПоМод32(N1, X);
    '''ОТ''' сч := 0 '''ДО''' 3 '''ВЫП '''
      '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(S)+сч, '''''ОБХОД'''''.''ПолучитьАдрес''(ячейка), 1);
      ячейка := Замена(сч, ячейка);
      '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(ячейка), '''''ОБХОД'''''.''ПолучитьАдрес''(S)+сч, 1)
    '''КОН''';
    S := Асм.Вращение(S, 11);
    S := ИсклИЛИ(S, N2);
    N2 := N1; N1 := S;
    '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(N1), '''''ОБХОД'''''.''ПолучитьАдрес''(результат), 4);
    '''''ОБХОД'''''.''Образ''('''''ОБХОД'''''.''ПолучитьАдрес''(N2), '''''ОБХОД'''''.''ПолучитьАдрес''(результат)+4, 4);
    '''ВОЗВРАТ''' результат
  '''КОН''' ОсновнойШаг;
</code>


## Go


```go
package main

import "fmt"

type sBox [8][16]byte

type gost struct {
    k87, k65, k43, k21 [256]byte
    enc                []byte
}

func newGost(s *sBox) *gost {
    var g gost
    for i := range g.k87 {
        g.k87[i] = s[7][i>>4]<<4 | s[6][i&15]
        g.k65[i] = s[5][i>>4]<<4 | s[4][i&15]
        g.k43[i] = s[3][i>>4]<<4 | s[2][i&15]
        g.k21[i] = s[1][i>>4]<<4 | s[0][i&15]
    }
    g.enc = make([]byte, 8)
    return &g
}

func (g *gost) f(x uint32) uint32 {
    x = uint32(g.k87[x>>24&255])<<24 | uint32(g.k65[x>>16&255])<<16 |
        uint32(g.k43[x>>8&255])<<8 | uint32(g.k21[x&255])
    return x<<11 | x>>(32-11)
}

// code above adapted from posted C code

// validation code below follows example on talk page

// cbrf from WP
var cbrf = sBox{
    {4, 10, 9, 2, 13, 8, 0, 14, 6, 11, 1, 12, 7, 15, 5, 3},
    {14, 11, 4, 12, 6, 13, 15, 10, 2, 3, 8, 1, 0, 7, 5, 9},
    {5, 8, 1, 13, 10, 3, 4, 2, 14, 15, 12, 7, 6, 0, 9, 11},
    {7, 13, 10, 1, 0, 8, 9, 15, 14, 4, 6, 12, 11, 2, 5, 3},
    {6, 12, 7, 1, 5, 15, 13, 8, 4, 10, 9, 14, 0, 3, 11, 2},
    {4, 11, 10, 0, 7, 2, 1, 13, 3, 6, 8, 5, 9, 12, 15, 14},
    {13, 11, 4, 1, 3, 15, 5, 9, 0, 10, 14, 7, 6, 8, 2, 12},
    {1, 15, 13, 0, 5, 7, 10, 4, 9, 2, 3, 14, 6, 11, 8, 12},
}

func u32(b []byte) uint32 {
    return uint32(b[0]) | uint32(b[1])<<8 | uint32(b[2])<<16 | uint32(b[3])<<24
}

func b4(u uint32, b []byte) {
    b[0] = byte(u)
    b[1] = byte(u >> 8)
    b[2] = byte(u >> 16)
    b[3] = byte(u >> 24)
}

func (g *gost) mainStep(input []byte, key []byte) {
    key32 := u32(key)
    input1 := u32(input[:4])
    input2 := u32(input[4:])
    b4(g.f(key32+input1)^input2, g.enc[:4])
    copy(g.enc[4:], input[:4])
}

func main() {
    input := []byte{0x21, 0x04, 0x3B, 0x04, 0x30, 0x04, 0x32, 0x04}
    key := []byte{0xF9, 0x04, 0xC1, 0xE2}

    g := newGost(&cbrf)
    g.mainStep(input, key)
    for _, b := range g.enc {
        fmt.Printf("[%02x]", b)
    }
    fmt.Println()
}
```

```txt

[1f][88][cf][07][21][04][3b][04]

```



## Julia

```julia

const k8 = [ 4, 10,  9,  2, 13,  8,  0, 14,  6, 11,  1, 12,  7, 15,  5,  3]
const k7 = [14, 11,  4, 12,  6, 13, 15, 10,  2,  3,  8,  1,  0,  7,  5,  9]
const k6 = [ 5,  8,  1, 13, 10,  3,  4,  2, 14, 15, 12,  7,  6,  0,  9, 11]
const k5 = [ 7, 13, 10,  1,  0,  8,  9, 15, 14,  4,  6, 12, 11,  2,  5,  3]
const k4 = [ 6, 12,  7,  1,  5, 15, 13,  8,  4, 10,  9, 14,  0,  3, 11,  2]
const k3 = [ 4, 11, 10,  0,  7,  2,  1, 13,  3,  6,  8,  5,  9, 12, 15, 14]
const k2 = [13, 11,  4,  1,  3, 15,  5,  9,  0, 10, 14,  7,  6,  8,  2, 12]
const k1 = [ 1, 15, 13,  0,  5,  7, 10,  4,  9,  2,  3, 14,  6, 11,  8, 12]
const k87 = zeros(UInt32,256)
const k65 = zeros(UInt32,256)
const k43 = zeros(UInt32,256)
const k21 = zeros(UInt32,256)
for i in 1:256
    j = (i-1) >> 4 + 1
    k = (i-1) & 15 + 1
    k87[i] = (k1[j] << 4) | k2[k]
    k65[i] = (k3[j] << 4) | k4[k]
    k43[i] = (k5[j] << 4) | k6[k]
    k21[i] = (k7[j] << 4) | k8[k]
end

function f(x)
    y = (k87[(x>>24) & 0xff + 1] << 24) | (k65[(x>>16) & 0xff + 1] << 16) |
        (k43[(x>> 8) & 0xff + 1] <<  8) | k21[x & 0xff + 1]
    (y << 11) | (y >> (32-11))
end

bytes2int(arr) = arr[1] + (UInt32(arr[2]) << 8) + (UInt32(arr[3]) << 16) + (UInt32(arr[4])) << 24
int2bytes(x) = [UInt8(x&0xff), UInt8((x&0xff00)>>8), UInt8((x&0xff0000)>>16), UInt8(x>>24)]

function mainstep(inputbytes, keybytes)
    intkey = bytes2int(keybytes)
    lowint = bytes2int(inputbytes[1:4])
    topint = bytes2int(inputbytes[5:8])
    xorbytes = f(UInt32(intkey) + UInt32(lowint)) ⊻ topint
    vcat(int2bytes(xorbytes), inputbytes[1:4])
end

const input = [0x21, 0x04, 0x3B, 0x04, 0x30, 0x04, 0x32, 0x04]
const key = [0xF9, 0x04, 0xC1, 0xE2]
println("The encoded bytes are $(mainstep(input, key))")

```

```txt

The encoded bytes are UInt8[0x1f, 0x88, 0xcf, 0x07, 0x21, 0x04, 0x3b, 0x04]
```



## Kotlin

```scala
// version 1.1.4-3

fun Byte.toUInt()  = java.lang.Byte.toUnsignedInt(this)

fun Byte.toULong() = java.lang.Byte.toUnsignedLong(this)

fun Int.toULong()  = java.lang.Integer.toUnsignedLong(this)

val s = arrayOf(
    byteArrayOf( 4, 10,  9,  2, 13,  8,  0, 14,  6, 11,  1, 12,  7, 15,  5,  3),
    byteArrayOf(14, 11,  4, 12,  6, 13, 15, 10,  2,  3,  8,  1,  0,  7,  5,  9),
    byteArrayOf( 5,  8,  1, 13, 10,  3,  4,  2, 14, 15, 12,  7,  6,  0,  9, 11),
    byteArrayOf( 7, 13, 10,  1,  0,  8,  9, 15, 14,  4,  6, 12, 11,  2,  5,  3),
    byteArrayOf( 6, 12,  7,  1,  5, 15, 13,  8,  4, 10,  9, 14,  0,  3, 11,  2),
    byteArrayOf( 4, 11, 10,  0,  7,  2,  1, 13,  3,  6,  8,  5,  9, 12, 15, 14),
    byteArrayOf(13, 11,  4,  1,  3, 15,  5,  9,  0, 10, 14,  7,  6,  8,  2, 12),
    byteArrayOf( 1, 15, 13,  0,  5,  7, 10,  4,  9,  2,  3, 14,  6, 11,  8, 12)
)

class Gost(val sBox: Array<ByteArray>) {

    val k87 = ByteArray(256)
    val k65 = ByteArray(256)
    val k43 = ByteArray(256)
    val k21 = ByteArray(256)
    val enc = ByteArray(8)

    init {
        for (i in 0 until 256) {
            val j = i ushr 4
            val k = i and 15
            k87[i] = ((sBox[7][j].toUInt() shl 4) or sBox[6][k].toUInt()).toByte()
            k65[i] = ((sBox[5][j].toUInt() shl 4) or sBox[4][k].toUInt()).toByte()
            k43[i] = ((sBox[3][j].toUInt() shl 4) or sBox[2][k].toUInt()).toByte()
            k21[i] = ((sBox[1][j].toUInt() shl 4) or sBox[0][k].toUInt()).toByte()
        }
    }

    fun f(x: Int): Int {
        val y = (k87[(x ushr 24) and 255].toULong() shl 24) or
                (k65[(x ushr 16) and 255].toULong() shl 16) or
                (k43[(x ushr  8) and 255].toULong() shl  8) or
                (k21[ x and 255].toULong())
        return ((y shl 11) or (y ushr 21)).toInt()
    }

    fun u32(ba: ByteArray): Int =
        (ba[0].toULong() or
        (ba[1].toULong() shl 8) or
        (ba[2].toULong() shl 16) or
        (ba[3].toULong() shl 24)).toInt()

    fun b4(u: Int) {
        enc[0] = u.toByte()
        enc[1] = (u ushr  8).toByte()
        enc[2] = (u ushr 16).toByte()
        enc[3] = (u ushr 24).toByte()
    }

    fun mainStep(input: ByteArray, key: ByteArray) {
        val key32  = u32(key)
        val input1 = u32(input.sliceArray(0..3))
        val input2 = u32(input.sliceArray(4..7))
        val temp   = (key32.toULong() + input1.toULong()).toInt()
        b4(f(temp) xor input2)
        for (i in 0..3) enc[4 + i] = input[i]
    }
}

fun main(args: Array<String>) {
    val input = byteArrayOf(0x21, 0x04, 0x3B, 0x04, 0x30, 0x04, 0x32, 0x04)
    val key = byteArrayOf(0xF9.toByte(), 0x04, 0xC1.toByte(), 0xE2.toByte())
    val g = Gost(s)
    g.mainStep(input, key)
    for (b in g.enc) print("[%02X]".format(b))
    println()
}
```


```txt

[1F][88][CF][07][21][04][3B][04]

```


=={{header|MK-61/52}}==
This program is designed to run on the two modules. This is due to the use of exclusive-or function, requiring the conversion of numbers in a particular format, and related resource consumption.

Code for the first module:
<lang>ИП6	С/П	+	П6	ИП7	С/П	+	П7	ИПE	-
x>=0	14	П7	КИП6	ИП6	ИПE	-	x>=0	20	П6
8	П2	П3	2	П1	4	П0	0	П8	1
П9	КИП2	ИПB	/	[x]	ПA	Вх	{x}	ИПB	*
С/П	ИП9	*	ИП8	+	П8	ИП9	ИПB	*	П9
ИПA	L0	32	ИП8	КП3	L1	25	8	П0	ИП7
ПП	93	П1	ИП6	ПП	93	ИП5	+	П6	ИП1
ИП4	+	П7	ИП4	ИП6	С/П	П4	ИП5	ИП7	С/П
П5	ИП4	ИП6	П4	<->	П6	ИП5	ИП7	П5	<->
П7	БП	00	ИПC	/	[x]	КП0	Вx	{x}	ИПC
*	ИПD	*	В/О
```


Code for the second module:
<lang>П1	<->	П2	Сx	П3	1	П4	19	П0	ИП1
2	/	[x]	П1	Вx	{x}	ИП2	2	/	[x]
П2	Вx	{x}	<->	->	-	x#0	33	ИП4	ИП3
+	П3	ИП4	^	+	П4	L0	10	ИП3	С/П
БП	00
```


<u>Instruction</u>:

Input is double-byte digits. Open text is in registers Р4 - Р7; and Р7 is low digit, Р4 is high digit.

After starting the program enter digits of key; first high, then low digit. After this, a table element changes to the line number in sequence (0 to 7) and the number of columns specified in the indicator.

After that, in the registers X and Y will be located two numbers that are entered in the second program. The result is shown on the display is returned to the first program. Then this action is repeated again.

In addition, the number originally entered: РB = 16, РC = 32, РD = 2048, РE = 65536.


## Nim

```nim
var
  k8 = [14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7]
  k7 = [15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10]
  k6 = [10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8]
  k5 = [ 7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15]
  k4 = [ 2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9]
  k3 = [12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11]
  k2 = [ 4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1]
  k1 = [13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7]

  k87, k65, k43, k21 = newSeq[int64](256)

proc kboxInit =
  for i in 0 .. 255:
    k87[i] = k8[i shr 4] shl 4 or k7[i and 15]
    k65[i] = k6[i shr 4] shl 4 or k5[i and 15]
    k43[i] = k4[i shr 4] shl 4 or k3[i and 15]
    k21[i] = k2[i shr 4] shl 4 or k1[i and 15]

proc f(x): int64 =
  let x = k87[x shr 24 and 255] shl 24 or k65[x shr 16 and 255] shl 16 or
          k43[x shr 8 and 255] shl 8 or k21[x and 255]
  x shl 11 or x shr (32 - 11)
```



## Perl

```perl
use strict;
use warnings;
use ntheory 'fromdigits';

# sboxes from http://en.wikipedia.org/wiki/GOST_(block_cipher)
my @sbox = (
    [4, 10, 9, 2, 13, 8, 0, 14, 6, 11, 1, 12, 7, 15, 5, 3],
    [14, 11, 4, 12, 6, 13, 15, 10, 2, 3, 8, 1, 0, 7, 5, 9],
    [5, 8, 1, 13, 10, 3, 4, 2, 14, 15, 12, 7, 6, 0, 9, 11],
    [7, 13, 10, 1, 0, 8, 9, 15, 14, 4, 6, 12, 11, 2, 5, 3],
    [6, 12, 7, 1, 5, 15, 13, 8, 4, 10, 9, 14, 0, 3, 11, 2],
    [4, 11, 10, 0, 7, 2, 1, 13, 3, 6, 8, 5, 9, 12, 15, 14],
    [13, 11, 4, 1, 3, 15, 5, 9, 0, 10, 14, 7, 6, 8, 2, 12],
    [1, 15, 13, 0, 5, 7, 10, 4, 9, 2, 3, 14, 6, 11, 8, 12]
);

sub rol32 {
    my($y, $n) = @_;
    ($y << $n) % 2**32 | ($y >> (32 - $n))
}

sub GOST_round {
    my($R, $K) = @_;
    my $a = ($R + $K) % 2**32;
    my $b = fromdigits([map { $sbox[$_][($a >> (4*$_))%16] } reverse 0..7],16);
    rol32($b,11);
}

sub feistel_step {
    my($F, $L, $R, $K) = @_;
    $R, $L ^ &$F($R, $K)
}

my @input = (0x21, 0x04, 0x3B, 0x04, 0x30, 0x04, 0x32, 0x04);
my @key   = (0xF9, 0x04, 0xC1, 0xE2);

my $R = fromdigits([reverse @input[0..3]], 256); # 1st half
my $L = fromdigits([reverse @input[4..7]], 256); # 2nd half
my $K = fromdigits([reverse @key        ], 256);

($L,$R) = feistel_step(\&GOST_round, $L, $R, $K);

printf '%02X ', (($L << 32) + $R >> (8*$_))%256 for 0..7;
print "\n";
```

```txt
1F 88 CF 07 21 04 3B 04
```



## Perl 6

Implemented to match explanation on Discussion page:


```perl6
# sboxes from http://en.wikipedia.org/wiki/GOST_(block_cipher)
constant sbox =
    [4, 10, 9, 2, 13, 8, 0, 14, 6, 11, 1, 12, 7, 15, 5, 3],
    [14, 11, 4, 12, 6, 13, 15, 10, 2, 3, 8, 1, 0, 7, 5, 9],
    [5, 8, 1, 13, 10, 3, 4, 2, 14, 15, 12, 7, 6, 0, 9, 11],
    [7, 13, 10, 1, 0, 8, 9, 15, 14, 4, 6, 12, 11, 2, 5, 3],
    [6, 12, 7, 1, 5, 15, 13, 8, 4, 10, 9, 14, 0, 3, 11, 2],
    [4, 11, 10, 0, 7, 2, 1, 13, 3, 6, 8, 5, 9, 12, 15, 14],
    [13, 11, 4, 1, 3, 15, 5, 9, 0, 10, 14, 7, 6, 8, 2, 12],
    [1, 15, 13, 0, 5, 7, 10, 4, 9, 2, 3, 14, 6, 11, 8, 12];

sub infix:<rol³²>(\y, \n) { (y +< n) % 2**32 +| (y +> (32 - n)) }

sub ГОСТ-round(\R, \K) {
    my \a = (R + K) % 2**32;
    my \b = :16[ sbox[$_][(a +> (4 * $_)) % 16] for 7...0 ];
    b rol³² 11;
}

sub feistel-step(&F, \L, \R, \K) { R, L +^ F(R, K) }

my @input = 0x21, 0x04, 0x3B, 0x04, 0x30, 0x04, 0x32, 0x04;
my @key   = 0xF9, 0x04, 0xC1, 0xE2;

my ($L,$R) = @input.reverse.map: { :256[$^a,$^b,$^c,$^d] }
my ($K   ) = @key  .reverse.map: { :256[$^a,$^b,$^c,$^d] }

($L,$R) = feistel-step(&ГОСТ-round, $L, $R, $K);

say [ ($L +< 32 + $R X+> (0, 8 ... 56)) X% 256 ].fmt('%02X');
```


```txt
1F 88 CF 07 21 04 3B 04
```



## Phix

```Phix
constant cbrf = {
      { 4, 10,  9,  2, 13,  8,  0, 14,  6, 11,  1, 12,  7, 15,  5,  3},
      {14, 11,  4, 12,  6, 13, 15, 10,  2,  3,  8,  1,  0,  7,  5,  9},
      { 5,  8,  1, 13, 10,  3,  4,  2, 14, 15, 12,  7,  6,  0,  9, 11},
      { 7, 13, 10,  1,  0,  8,  9, 15, 14,  4,  6, 12, 11,  2,  5,  3},
      { 6, 12,  7,  1,  5, 15, 13,  8,  4, 10,  9, 14,  0,  3, 11,  2},
      { 4, 11, 10,  0,  7,  2,  1, 13,  3,  6,  8,  5,  9, 12, 15, 14},
      {13, 11,  4,  1,  3, 15,  5,  9,  0, 10, 14,  7,  6,  8,  2, 12},
      { 1, 15, 13,  0,  5,  7, 10,  4,  9,  2,  3, 14,  6, 11,  8, 12}}

function generate(integer k)
    sequence res = repeat(0,256)
    for i=1 to length(res) do
        integer hdx = floor((i-1)/16)+1,
                ldx = and_bits(i-1,#F)+1
        res[i] = or_bits(cbrf[k][hdx]*#10,cbrf[k-1][ldx])
    end for
    return res
end function

constant k87 = generate(8),
         k65 = generate(6),
         k43 = generate(4),
         k21 = generate(2)

function r32(atom a)
    if a<0 then a+=#100000000 end if
    return remainder(a,#100000000)
end function

function mainstep(sequence input, atom key)
    atom s = r32(input[1]+key)
    s = r32(or_all({k87[and_bits(floor(s/#1000000),#FF)+1]*#1000000,
                    k65[and_bits(floor(s/#0010000),#FF)+1]*#0010000,
                    k43[and_bits(floor(s/#0000100),#FF)+1]*#0000100,
                    k21[and_bits(floor(s/#0000001),#FF)+1]*#0000001}))
    s = r32(s*power(2,11))+floor(s/power(2,32-11))
    s = xor_bits(s,input[2])
    return {s,input[1]}
end function

sequence res = mainstep({#043B0421, #04320430}, #E2C104F9)
printf(1,"%08x %08x\n",res)
--or, for other-endian:
sequence s = sprintf("%08x",res[1]),
         t = sprintf("%08x",res[2])
s = reverse(split(join_by(s,1,2,""," "),no_empty:=true))
t = reverse(split(join_by(t,1,2,""," "),no_empty:=true))
?{s,t}
```

```txt

07CF881F 043B0421
```



## PicoLisp

```PicoLisp
(setq K1 (13  2  8  4  6 15 11  1 10  9  3 14  5  0 12  7))
(setq K2 ( 4 11  2 14 15  0  8 13  3 12  9  7  5 10  6  1))
(setq K3 (12  1 10 15  9  2  6  8  0 13  3  4 14  7  5 11))
(setq K4 ( 2 12  4  1  7 10 11  6  8  5  3 15 13  0 14  9))
(setq K5 ( 7 13 14  3  0  6  9 10  1  2  8  5 11 12  4 15))
(setq K6 (10  0  9 14  6  3 15  5  1 13 12  7 11  4  2  8))
(setq K7 (15  1  8 14  6 11  3  4  9  7  2 13 12  0  5 10))
(setq K8 (14  4 13  1  2 15 11  8  3 10  6 12  5  9  0  7))

(setq K21
   (mapcar
      '((N)
         (|
            (>> -4 (get K2 (inc (>> 4 N))))
            (get K1 (inc (& N 15))) ) )
      (range 0 255) ) )

(setq K43
   (mapcar
      '((N)
         (|
            (>> -4 (get K4 (inc (>> 4 N))))
            (get K3 (inc (& N 15))) ) )
      (range 0 255) ) )

(setq K65
   (mapcar
      '((N)
         (|
            (>> -4 (get K6 (inc (>> 4 N))))
            (get K5 (inc (& N 15))) ) )
      (range 0 255) ) )

(setq K87
   (mapcar
      '((N)
         (|
            (>> -4 (get K8 (inc (>> 4 N))))
            (get K7 (inc (& N 15))) ) )
      (range 0 255) ) )

(de leftRotate (X C)
   (|
      (& `(hex "FFFFFFFF") (>> (- C) X))
      (>> (- 32 C) X) ) )

(de f (X)
   (leftRotate
      (apply
         |
         (mapcar
            '((Lst N)
               (>>
                  N
                  (get
                     (val Lst)
                     (inc (& 255 (>> (abs N) X))) ) ) )
            '(K87 K65 K43 K21)
            (-24 -16 -8 0) ) )
      11 ) )

(bye)
```



## Python

```python

k8 = [	14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7 ]
k7 = [	15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10 ]
k6 = [	10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8 ]
k5 = [	 7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15 ]
k4 = [	 2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9 ]
k3 = [	12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11 ]
k2 = [	 4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1 ]
k1 = [	13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7 ]

k87 = [0] * 256
k65 = [0] * 256
k43 = [0] * 256
k21 = [0] * 256

def kboxinit():
	for i in range(256):
		k87[i] = k8[i >> 4] << 4 | k7[i & 15]
		k65[i] = k6[i >> 4] << 4 | k5[i & 15]
		k43[i] = k4[i >> 4] << 4 | k3[i & 15]
		k21[i] = k2[i >> 4] << 4 | k1[i & 15]

def f(x):
	x = ( k87[x>>24 & 255] << 24 | k65[x>>16 & 255] << 16 |
	      k43[x>> 8 & 255] <<  8 | k21[x & 255] )
	return x<<11 | x>>(32-11)
```



## Racket

This is a direct translation of the C code, but I have no idea if that code is correct, or a way to test it.  (I suspect that it isn't, at least since the table has different numbers than the wikipedia article...)

```racket

#lang racket

(define k8 (bytes 14  4 13  1  2 15 11  8  3 10  6 12  5  9  0  7))
(define k7 (bytes 15  1  8 14  6 11  3  4  9  7  2 13 12  0  5 10))
(define k6 (bytes 10  0  9 14  6  3 15  5  1 13 12  7 11  4  2  8))
(define k5 (bytes  7 13 14  3  0  6  9 10  1  2  8  5 11 12  4 15))
(define k4 (bytes  2 12  4  1  7 10 11  6  8  5  3 15 13  0 14  9))
(define k3 (bytes 12  1 10 15  9  2  6  8  0 13  3  4 14  7  5 11))
(define k2 (bytes  4 11  2 14 15  0  8 13  3 12  9  7  5 10  6  1))
(define k1 (bytes 13  2  8  4  6 15 11  1 10  9  3 14  5  0 12  7))

(define (mk-k k2 k1)
  (list->bytes (for*/list ([i 16] [j 16]) (+ (* (bytes-ref k2 i) 16) (bytes-ref k1 j)))))

(define k87 (mk-k k8 k7))
(define k65 (mk-k k6 k5))
(define k43 (mk-k k4 k3))
(define k21 (mk-k k2 k1))

(define (f x)
  (define bs (integer->integer-bytes x 4 #f #f))
  (define x*
    (bitwise-and #xFFFFFFFF
                 (integer-bytes->integer
                  (bytes (bytes-ref k21 (bytes-ref bs 0))
                         (bytes-ref k43 (bytes-ref bs 1))
                         (bytes-ref k65 (bytes-ref bs 2))
                         (bytes-ref k87 (bytes-ref bs 3)))
                  #f #f)))
  (bitwise-ior (bitwise-and #xFFFFFFFF (arithmetic-shift x* 11))
               (arithmetic-shift x* (- 11 32))))

```



## REXX

```rexx
/*REXX program implements  main step   GOST 28147-89   based on a Feistel network.      */
numeric digits 12                          /*  ┌── a list of 4─bit S─box values used by */
                                           /*  ↓ the Central Bank of Russian Federation.*/
                @.0  =     4  10   9   2  13   8   0  14   6  11   1  12   7  15   5   3
                @.1  =    14  11   4  12   6  13  15  10   2   3   8   1   0   7   5   9
                @.2  =     5   8   1  13  10   3   4   2  14  15  12   7   6   0   9  11
                @.3  =     7  13  10   1   0   8   9  15  14   4   6  12  11   2   5   3
                @.4  =     6  12   7   1   5  15  13   8   4  10   9  14   0   3  11   2
                @.5  =     4  11  10   0   7   2   1  13   3   6   8   5   9  12  15  14
                @.6  =    13  11   4   1   3  15   5   9   0  10  14   7   6   8   2  12
                @.7  =     1  15  13   0   5   7  10   4   9   2   3  14   6  11   8  12
                                           /* [↓]  build the sub-keys array from above. */
         do r=0  for 8;     do c=0  for 16;        !.r.c=word(@.r, c + 1);     end;    end
z=0
#1=x2d( 43b0421 );     #2=x2d( 4320430 );                         k=#1 + x2d( 0e2c104f9 )
                            do  while  k > x2d( 7ffFFffF );       k=k - 2**32;     end
                            do  while  k < x2d( 80000000 );       k=k + 2**32;     end

  do j=0  for 4;   jj=j + j;        jjp=jj + 1   /*calculate the array'a  "subscripts". */
  $=x2d( right( d2x( k % 2 ** (j * 8) ),  2) )
              cm=$ // 16;       cd=$ % 16        /*perform modulus and integer division.*/
  z=z + (!.jj.cm  +  16 * !.jjp.cd)  *  2**(j*8)
  end   /*i*/                                    /* [↑]  encryption algorithm for S-box.*/
                                                 /* [↓]  encryption algorithm round.    */
k = c2d( bitxor( bitor( d2c(z * 2**11,  4),    d2c(z % 2**21,  4) ),     d2c(#2, 4) ) )
say  center(d2x(k)     ' '    d2x(#1), 79)       /*stick a fork in it,  we're all done. */
```

```txt

                               7CF881F   43B0421

```



## Rust


```rust
use std::convert::TryInto;
use std::env;
use std::num::Wrapping;

const REPLACEMENT_TABLE: [[u8; 16]; 8] = [
    [4, 10, 9, 2, 13, 8, 0, 14, 6, 11, 1, 12, 7, 15, 5, 3],
    [14, 11, 4, 12, 6, 13, 15, 10, 2, 3, 8, 1, 0, 7, 5, 9],
    [5, 8, 1, 13, 10, 3, 4, 2, 14, 15, 12, 7, 6, 0, 9, 11],
    [7, 13, 10, 1, 0, 8, 9, 15, 14, 4, 6, 12, 11, 2, 5, 3],
    [6, 12, 7, 1, 5, 15, 13, 8, 4, 10, 9, 14, 0, 3, 11, 2],
    [4, 11, 10, 0, 7, 2, 1, 13, 3, 6, 8, 5, 9, 12, 15, 14],
    [13, 11, 4, 1, 3, 15, 5, 9, 0, 10, 14, 7, 6, 8, 2, 12],
    [1, 15, 13, 0, 5, 7, 10, 4, 9, 2, 3, 14, 6, 11, 8, 12],
];
const KEYS: [u32; 8] = [
    0xE2C1_04F9,
    0xE41D_7CDE,
    0x7FE5_E857,
    0x0602_65B4,
    0x281C_CC85,
    0x2E2C_929A,
    0x4746_4503,
    0xE00_CE510,
];

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        let plain_text: Vec<u8> = vec![0x04, 0x3B, 0x04, 0x21, 0x04, 0x32, 0x04, 0x30];
        println!(
            "Before one step: {}\n",
            plain_text
                .iter()
                .cloned()
                .fold("".to_string(), |b, y| b + &format!("{:02X} ", y))
        );
        let encoded_text = main_step(plain_text, KEYS[0]);
        println!(
            "After one step : {}\n",
            encoded_text
                .iter()
                .cloned()
                .fold("".to_string(), |b, y| b + &format!("{:02X} ", y))
        );
    } else {
        let mut t = args[1].clone(); // "They call him... Баба Яга"
        t += &" ".repeat((8 - t.len() % 8) % 8);
        let text_bytes = t.bytes().collect::<Vec<_>>();
        let plain_text = text_bytes.chunks(8).collect::<Vec<_>>();
        println!(
            "Plain text  : {}\n",
            plain_text.iter().cloned().fold("".to_string(), |a, x| a
                + "["
                + &x.iter()
                    .fold("".to_string(), |b, y| b + &format!("{:02X} ", y))[..23]
                + "]")
        );
        let encoded_text = plain_text
            .iter()
            .map(|c| encode(c.to_vec()))
            .collect::<Vec<_>>();
        println!(
            "Encoded text: {}\n",
            encoded_text.iter().cloned().fold("".to_string(), |a, x| a
                + "["
                + &x.into_iter()
                    .fold("".to_string(), |b, y| b + &format!("{:02X} ", y))[..23]
                + "]")
        );
        let decoded_text = encoded_text
            .iter()
            .map(|c| decode(c.to_vec()))
            .collect::<Vec<_>>();
        println!(
            "Decoded text: {}\n",
            decoded_text.iter().cloned().fold("".to_string(), |a, x| a
                + "["
                + &x.into_iter()
                    .fold("".to_string(), |b, y| b + &format!("{:02X} ", y))[..23]
                + "]")
        );
        let recovered_text =
            String::from_utf8(decoded_text.iter().cloned().flatten().collect::<Vec<_>>()).unwrap();
        println!("Recovered text: {}\n", recovered_text);
    }
}

fn encode(text_block: Vec<u8>) -> Vec<u8> {
    let mut step = text_block;
    for i in 0..24 {
        step = main_step(step, KEYS[i % 8]);
    }
    for i in (0..8).rev() {
        step = main_step(step, KEYS[i]);
    }
    step
}

fn decode(text_block: Vec<u8>) -> Vec<u8> {
    let mut step = text_block[4..].to_vec();
    let mut temp = text_block[..4].to_vec();
    step.append(&mut temp);
    for key in &KEYS {
        step = main_step(step, *key);
    }
    for i in (0..24).rev() {
        step = main_step(step, KEYS[i % 8]);
    }
    let mut ans = step[4..].to_vec();
    let mut temp = step[..4].to_vec();
    ans.append(&mut temp);
    ans
}

fn main_step(text_block: Vec<u8>, key_element: u32) -> Vec<u8> {
    let mut n = text_block;
    let mut s = (Wrapping(
        u32::from(n[0]) << 24 | u32::from(n[1]) << 16 | u32::from(n[2]) << 8 | u32::from(n[3]),
    ) + Wrapping(key_element))
    .0;
    let mut new_s: u32 = 0;
    for mid in 0..4 {
        let cell = (s >> (mid << 3)) & 0xFF;
        new_s += (u32::from(REPLACEMENT_TABLE[(mid * 2) as usize][(cell & 0x0f) as usize])
            + (u32::from(REPLACEMENT_TABLE[(mid * 2 + 1) as usize][(cell >> 4) as usize]) << 4))
            << (mid << 3);
    }
    s = ((new_s << 11) + (new_s >> 21))
        ^ (u32::from(n[4]) << 24 | u32::from(n[5]) << 16 | u32::from(n[6]) << 8 | u32::from(n[7]));
    n[4] = n[0];
    n[5] = n[1];
    n[6] = n[2];
    n[7] = n[3];
    n[0] = (s >> 24).try_into().unwrap();
    n[1] = ((s >> 16) & 0xFF).try_into().unwrap();
    n[2] = ((s >> 8) & 0xFF).try_into().unwrap();
    n[3] = (s & 0xFF).try_into().unwrap();
    n
}
```

```txt

Without parameters:

Before one step: 04 3B 04 21 04 32 04 30

After one step : 07 CF 88 1F 04 3B 04 21


With parameter "They call him... Баба Яга"

Plain text  : [54 68 65 79 20 63 61 6C][6C 20 68 69 6D 2E 2E 2E][20 D0 91 D0 B0 D0 B1 D0][B0 20 D0 AF D0 B3 D0 B0]

Encoded text: [D6 7C 52 4A EA 9A 58 2D][D9 81 F7 DA ED 89 46 25][0A 75 2D 89 59 8B 3D C4][53 DC D6 E2 79 B6 68 24]

Decoded text: [54 68 65 79 20 63 61 6C][6C 20 68 69 6D 2E 2E 2E][20 D0 91 D0 B0 D0 B1 D0][B0 20 D0 AF D0 B3 D0 B0]

Recovered text: They call him... Баба Яга

```



## Tcl


```tcl
namespace eval ::GOST {
    proc tcl::mathfunc::k {a b} {
	variable ::GOST::replacementTable
	lindex $replacementTable $a $b
    }

    proc mainStep {textBlock idx key} {
	variable replacementTable
	lassign [lindex $textBlock $idx] N0 N1
	set S [expr {($N0 + $key) & 0xFFFFFFFF}]
	set newS 0
	for {set i 0} {$i < 4} {incr i} {
	    set cell [expr {$S >> ($i * 8) & 0xFF}]
	    incr newS [expr {
		(k($i*2, $cell%15) + k($i*2+1, $cell/16) * 16) << ($i * 8)
	    }]
	}
	set S [expr {((($newS << 11) + ($newS >> 21)) & 0xFFFFFFFF) ^ $N1}]
	lset textBlock $idx [list $S $N0]
	return $textBlock
    }
}
```

Note that only the <tt>idx</tt>'th row of the split-up <tt>textBlock</tt> (which contains the two pieces to intermingle/exchange at this step) is altered; it is the responsibility of the caller to iterate over all the steps through the entire plaintext/ciphertext.


## X86 Assembly

Parameter in the call:
* EAX - the youngest part of the transformed block (N1);
* EDX - leading part transformed block (N2);
* ESI - address of the first element of the key;
* EBX - table address changes;
* ECX - the number of major steps.

Output results:
* EDX = N1, EAX = N2 for cycles 32-З, 32-Р;
* EAX = N1, EDX = N2 for cycle 16-З.

Register Usage: all except EDI.

Notes:
* At the end of the run the registers as follows:
* EBX (pointer to table of changes) - the same as that in the early
* ESI (pointer to key) - points to the first byte of the key - it's N * 4 larger than the initial values for the SI cycle reps N (for encryption cycles N = 32 => 4 * N = 128, for authentication code generation cycle N = 16 => 4 * N = 64), larger than the initial values for the authentication code generation cycle.
* ECX = 0
* The contents of the segment registers unchanged.


```Asm
        .386
        .model  flat
        .code
_gost32 proc    near32
        public  _gost32
; The inner loop of a subroutine
; 1. Beginning of the cycle, and preservation of the old N1
iloop:  mov     EBP,EAX
; 2. Adding to the S key modulo 2^32
        add     EAX,[ESI] ; add the key
        add     ESI,4   ; the next element of the key.
; 3. Block-replace in the rotation of S by 8 bits to the left
REPT    3
        xlat            ; recoding byte
        ror     EAX,8   ; AL <- next byte
        add     EBX,100h; next node changes
ENDM
        xlat            ; recoding byte
        sub     EBX,300h; BX -> 1st node changes
; 4. Complete rotation of the S at 3 bits to the left
        rol     EAX,3
; 5. The calculation of the new values of N1,N2
        xor     EAX,EDX
        mov     EDX,EBP
; The completion of the inner loop
        loop    iloop
        ret
_gost32 endp
        end
```



## zkl

This could very well fail on a non-Intel endian-ness.

```zkl
class GOST{
var [const] // Central Bank of Russian Federation S-Boxes
  k8=Data(0,Int, 4, 10,  9,  2, 13,  8,  0, 14,  6, 11,  1, 12,  7, 15,  5,  3),
  k7=Data(0,Int,14, 11,  4, 12,  6, 13, 15, 10,  2,  3,  8,  1,  0,  7,  5,  9),
  k6=Data(0,Int, 5,  8,  1, 13, 10,  3,  4,  2, 14, 15, 12,  7,  6,  0,  9, 11),
  k5=Data(0,Int, 7, 13, 10,  1,  0,  8,  9, 15, 14,  4,  6, 12, 11,  2,  5,  3),
  k4=Data(0,Int, 6, 12,  7,  1,  5, 15, 13,  8,  4, 10,  9, 14,  0,  3, 11,  2),
  k3=Data(0,Int, 4, 11, 10,  0,  7,  2,  1, 13,  3,  6,  8,  5,  9, 12, 15, 14),
  k2=Data(0,Int,13, 11,  4,  1,  3, 15,  5,  9,  0, 10, 14,  7,  6,  8,  2, 12),
  k1=Data(0,Int, 1, 15, 13,  0,  5,  7, 10,  4,  9,  2,  3, 14,  6, 11,  8, 12);

fcn generate(ka,kb)
   { (0).pump(256,Data,'wrap(i){ kb[i/0x10]*0x10 + ka[i%0x10] }) }
var [const] k87=generate(k8,k7), k65=generate(k6,k5),
	    k43=generate(k4,k3), k21=generate(k2,k1);

fcn f(x){ // int --> int
   x3,x2,x1,x0:=x.toLittleEndian(4);
   x=k87[x3] + k65[x2]*0x|100 + k43[x1]*0x1|0000 + k21[x0]*0x100|0000;
   x.shiftLeft(11) + x.shiftRight(21); // roll left 11 bits, leaving bits on top
}

fcn mainStep(input,key){ // input is stream of bytes, little endian 32 bit words
   r:=Data();
   foreach idx in ([0..input.len()-1,8]){
      w0:=input.toLittleEndian(idx,  4);
      w1:=input.toLittleEndian(idx+4,4);
      r.write(f(key+w0).bitXor(w1).toLittleEndian(4),w0.toLittleEndian(4));
   }
   r
}}
```


```zkl
    // Example from the talk page (little endian byte stream)
input:=Data(0,Int,0x21, 0x04, 0x3B, 0x04, 0x30, 0x04, 0x32, 0x04);
key  := 0xE2|C1|04|F9;	// big endian

GOST.mainStep(input,key).bytes().apply("[%02x]".fmt).concat().println();
```

```txt
[1f][88][cf][07][21][04][3b][04]
```


