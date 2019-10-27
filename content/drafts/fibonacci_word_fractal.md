+++
title = "Fibonacci word/fractal"
description = ""
date = 2018-09-21T22:13:07Z
aliases = []
[extra]
id = 15306
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[File:Fib_word_fractal.gif|613px||right]]

The [[Fibonacci word]] may be represented as a fractal as described [http://hal.archives-ouvertes.fr/docs/00/36/79/72/PDF/The_Fibonacci_word_fractal.pdf here]:

:For F_word<sub>m</sub> start with F_wordChar<sub>n=1</sub>
:Draw a segment forward
:If current F_wordChar is 0
::Turn left if n is even
::Turn right if n is odd
:next n and iterate until end of F_word



;Task:
Create and display a fractal similar to [http://hal.archives-ouvertes.fr/docs/00/36/79/72/PDF/The_Fibonacci_word_fractal.pdf Fig 1].





## AutoHotkey

Prints F_Word<sub>30</sub> currently. Segment length and F_Word<sub>n</sub> can be adjusted.
{{libheader|GDIP}}Also see the [http://www.autohotkey.com/board/topic/29449-gdi-standard-library-145-by-tic/ Gdip examples].

```AutoHotkey
#NoEnv
SetBatchLines, -1
p := 0.3	; Segment length (pixels)
F_Word := 30

SysGet, Mon, MonitorWorkArea
W := FibWord(F_Word)
d := 1
x1 := 0
y1 := MonBottom
Width := A_ScreenWidth
Height := A_ScreenHeight

If (!pToken := Gdip_Startup()) {
	MsgBox, 48, Gdiplus Error!, Gdiplus failed to start. Please ensure you have Gdiplus on your system.
	ExitApp
}
OnExit, Shutdown

Gui, 1: -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, 1: Show, NA

hwnd1 := WinExist()
hbm := CreateDIBSection(Width, Height)
hdc := CreateCompatibleDC()
obm := SelectObject(hdc, hbm)
G := Gdip_GraphicsFromHDC(hdc)
Gdip_SetSmoothingMode(G, 4)
pPen := Gdip_CreatePen(0xffff0000, 1)

Loop, Parse, W
{
	if (d = 0)
		x2 := x1 + p, y2 := y1
	else if (d = 1 || d = -3)
		x2 := x1, y2 := y1 - p
	else if (d = 2 || d = -2)
		x2 := x1 - p, y2 := y1
	else if (d = 3 || d = -1)
		x2 := x1, y2 := y1 + p
	Gdip_DrawLine(G, pPen, x1, y1, x2, y2)
	if (!Mod(A_Index, 1500))
		UpdateLayeredWindow(hwnd1, hdc, 0, 0, Width, Height)
	if (A_LoopField = 0) {
		if (!Mod(A_Index, 2))
			d += 1
		else
			d -= 1
	}
	x1 := x2, y1 := y2, d := Mod(d, 4)
}

Gdip_DeletePen(pPen)
UpdateLayeredWindow(hwnd1, hdc, 0, 0, Width, Height)
SelectObject(hdc, obm)
DeleteObject(hbm)
DeleteDC(hdc)
Gdip_DeleteGraphics(G)
return

FibWord(n, FW1=1, FW2=0) {
	Loop, % n - 2
		FW3 := FW2 FW1, FW1 := FW2, FW2 := FW3
	return FW3
}

Esc::
Shutdown:
Gdip_DeletePen(pPen)
SelectObject(hdc, obm)
DeleteObject(hbm)
DeleteDC(hdc)
Gdip_DeleteGraphics(G)
Gdip_Shutdown(pToken)
ExitApp
```



## C

Writes an EPS file that has the 26th fractal.  This is probably cheating.

```c>#include <stdio.h


int main(void)
{
	puts(	"%!PS-Adobe-3.0 EPSF\n"
		"%%BoundingBox: -10 -10 400 565\n"
		"/a{0 0 moveto 0 .4 translate 0 0 lineto stroke -1 1 scale}def\n"
		"/b{a 90 rotate}def");

	char i;
	for (i = 'c'; i <= 'z'; i++)
		printf("/%c{%c %c}def\n", i, i-1, i-2);

	puts("0 setlinewidth z showpage\n%%EOF");

	return 0;
}
```



## C++



```cpp

#include <windows.h>
#include <string>
using namespace std;

class myBitmap
{
public:
    myBitmap() : pen( NULL ) {}
    ~myBitmap()
    {
        DeleteObject( pen );
        DeleteDC( hdc );
        DeleteObject( bmp );
    }
 
    bool create( int w, int h )
    {
        BITMAPINFO	bi;
        ZeroMemory( &bi, sizeof( bi ) );
        bi.bmiHeader.biSize        = sizeof( bi.bmiHeader );
        bi.bmiHeader.biBitCount	   = sizeof( DWORD ) * 8;
	bi.bmiHeader.biCompression = BI_RGB;
	bi.bmiHeader.biPlanes	   = 1;
	bi.bmiHeader.biWidth	   =  w;
	bi.bmiHeader.biHeight	   = -h;
	HDC dc = GetDC( GetConsoleWindow() );
	bmp = CreateDIBSection( dc, &bi, DIB_RGB_COLORS, &pBits, NULL, 0 );
	if( !bmp ) return false;
	hdc = CreateCompatibleDC( dc );
	SelectObject( hdc, bmp );
	ReleaseDC( GetConsoleWindow(), dc ); 
	width = w; height = h;
	clear();
	return true;
    }
 
    void clear()
    {
	ZeroMemory( pBits, width * height * sizeof( DWORD ) );
    }
 
    void setPenColor( DWORD clr )
    {
	if( pen ) DeleteObject( pen );
	pen = CreatePen( PS_SOLID, 1, clr );
	SelectObject( hdc, pen );
    }
 
    void saveBitmap( string path )
    {
	BITMAPFILEHEADER fileheader;
	BITMAPINFO	 infoheader;
	BITMAP		 bitmap;
	DWORD*		 dwpBits;
	DWORD		 wb;
	HANDLE		 file;
 
	GetObject( bmp, sizeof( bitmap ), &bitmap );
	dwpBits = new DWORD[bitmap.bmWidth * bitmap.bmHeight];
	ZeroMemory( dwpBits, bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD ) );
	ZeroMemory( &infoheader, sizeof( BITMAPINFO ) );
	ZeroMemory( &fileheader, sizeof( BITMAPFILEHEADER ) );
 
	infoheader.bmiHeader.biBitCount = sizeof( DWORD ) * 8;
	infoheader.bmiHeader.biCompression = BI_RGB;
	infoheader.bmiHeader.biPlanes = 1;
	infoheader.bmiHeader.biSize = sizeof( infoheader.bmiHeader );
	infoheader.bmiHeader.biHeight = bitmap.bmHeight;
	infoheader.bmiHeader.biWidth = bitmap.bmWidth;
	infoheader.bmiHeader.biSizeImage = bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD );
 
	fileheader.bfType    = 0x4D42;
	fileheader.bfOffBits = sizeof( infoheader.bmiHeader ) + sizeof( BITMAPFILEHEADER );
	fileheader.bfSize    = fileheader.bfOffBits + infoheader.bmiHeader.biSizeImage;
 
	GetDIBits( hdc, bmp, 0, height, ( LPVOID )dwpBits, &infoheader, DIB_RGB_COLORS );
 
	file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
	WriteFile( file, &fileheader, sizeof( BITMAPFILEHEADER ), &wb, NULL );
	WriteFile( file, &infoheader.bmiHeader, sizeof( infoheader.bmiHeader ), &wb, NULL );
	WriteFile( file, dwpBits, bitmap.bmWidth * bitmap.bmHeight * 4, &wb, NULL );
	CloseHandle( file );
 
	delete [] dwpBits;
    }
 
    HDC getDC()     { return hdc; }
    int getWidth()  { return width; }
    int getHeight() { return height; }
 
private:
    HBITMAP bmp;
    HDC	    hdc;
    HPEN    pen;
    void    *pBits;
    int	    width, height;
};
class fiboFractal
{
public:
    fiboFractal( int l )
    {
	bmp.create( 600, 440 );
	bmp.setPenColor( 0x00ff00 );
	createWord( l ); createFractal();
	bmp.saveBitmap( "path_to_save_bitmap" );
    }
private:
    void createWord( int l )
    {
	string a = "1", b = "0", c;
	l -= 2;
	while( l-- )
	{ c = b + a; a = b; b = c; }
	fWord = c;
    }

    void createFractal()
    {
	int n = 1, px = 10, dir, 
	    py = 420, len = 1, 
	    x = 0, y = -len, goingTo = 0;

	HDC dc = bmp.getDC();
	MoveToEx( dc, px, py, NULL );
	for( string::iterator si = fWord.begin(); si != fWord.end(); si++ )
	{
	    px += x; py += y;
	    LineTo( dc, px, py );
	    if( !( *si - 48 ) )
	    {	// odd
		if( n & 1 ) dir = 1;	// right
		else dir = 0;			// left
		switch( goingTo )
		{
		    case 0: // up
		        y = 0;
			if( dir ){ x = len; goingTo = 1; }
			else { x = -len; goingTo = 3; }
		    break;
		    case 1: // right
			x = 0;
			if( dir ) { y = len; goingTo = 2; }
			else { y = -len; goingTo = 0; }
		    break;
		    case 2: // down
			y = 0;
			if( dir ) { x = -len; goingTo = 3; }
			else { x = len; goingTo = 1; }
		    break;
		    case 3: // left
			x = 0;
			if( dir ) { y = -len; goingTo = 0; }
			else { y = len; goingTo = 2; }
		}
            }
	    n++;
        }
    }

    string fWord;
    myBitmap bmp;
};
int main( int argc, char* argv[] )
{
    fiboFractal ff( 23 );
    return system( "pause" );
}

```



## D

This uses the turtle module from the Dragon Curve Task, and the module from the Grayscale Image task.
{{trans|Python}}

```d
import std.range, grayscale_image, turtle;

void drawFibonacci(Color)(Image!Color img, ref Turtle t,
                          in string word, in real step) {
    foreach (immutable i, immutable c; word) {
        t.forward(img, step);
        if (c == '0') {
            if ((i + 1) % 2 == 0)
                t.left(90);
            else
                t.right(90);
        }
    }
}

void main() {
    auto img = new Image!Gray(1050, 1050);
    auto t = Turtle(30, 1010, -90);
    const w = recurrence!q{a[n-1] ~ a[n-2]}("1", "0").drop(24).front;
    img.drawFibonacci(t, w, 1);
    img.savePGM("fibonacci_word_fractal.pgm");
}
```

It prints the level 25 word as the Python entry.


## Elixir

{{trans|Ruby}}

```elixir
defmodule Fibonacci do
  def fibonacci_word, do: Stream.unfold({"1","0"}, fn{a,b} -> {a, {b, b<>a}} end)
  
  def word_fractal(n) do
    word = fibonacci_word |> Enum.at(n)
    walk(to_char_list(word), 1, 0, 0, 0, -1, %{{0,0}=>"S"})
    |> print
  end
  
  defp walk([], _, _, _, _, _, map), do: map
  defp walk([h|t], n, x, y, dx, dy, map) do
    map2 = Map.put(map, {x+dx, y+dy}, (if dx==0, do: "|", else: "-"))
           |> Map.put({x2=x+2*dx, y2=y+2*dy}, "+")
    if h == ?0 do
      if rem(n,2)==0, do: walk(t, n+1, x2, y2, dy, -dx, map2),
                    else: walk(t, n+1, x2, y2, -dy, dx, map2)
    else
      walk(t, n+1, x2, y2, dx, dy, map2)
    end
  end
  
  defp print(map) do
    xkeys = Map.keys(map) |> Enum.map(fn {x,_} -> x end)
    {xmin, xmax} = Enum.min_max(xkeys)
    ykeys = Map.keys(map) |> Enum.map(fn {_,y} -> y end)
    {ymin, ymax} = Enum.min_max(ykeys)
    Enum.each(ymin..ymax, fn y ->
      IO.puts Enum.map(xmin..xmax, fn x -> Map.get(map, {x,y}, " ") end)
    end)
  end
end

Fibonacci.word_fractal(16)
```

Output is same as Ruby.

=={{header|F_Sharp|F#}}==
<p>We output an SVG or rather an HTML with an embedded SVG</p>
<p>Points to note:</p>
<ul>
<li>Rather than using the "usual" Fibonacci catamorphismen 
```fsharp
Seq.unfold(fun (f1, f2) -> Some(f1, (f2, f2+f1))) ("1", "0")
```
 we use the morphism &sigma;: 0 &rarr; 01, 1 &rarr; 0, starting with a single 1, described in the referenced PDF in the task description.</li>
<li>The outer dimension of the SVG is computed. For a simplification we compute bounding boxes for fractals with number 3*k+2 only. These are &cap; formed  or &sup; formed. For 3*k and 3*k+1 fractals the bounding box for the next 3*k+2 fractal is taken. (c/f PDF; Theorem 3, Theorem 4)</li>
</ul>

```fsharp
let sigma s = seq {
    for c in s do if c = '1' then yield '0' else yield '0'; yield '1'
}
let rec fibwordIterator s = seq { yield s; yield! fibwordIterator (sigma s) }

let goto (x, y) (dx, dy) c n =
    let (dx', dy') =
        if c = '0' then
            match (dx, dy), n with
            | (1,0),0 -> (0,1)  | (1,0),1 -> (0,-1)
            | (0,1),0 -> (-1,0) | (0,1),1 -> (1,0)
            | (-1,0),0 -> (0,-1)| (-1,0),1 -> (0,1)
            | (0,-1),0 -> (1,0) | (0,-1),1 -> (-1,0)
            | _ -> failwith "not possible (c=0)"
        else
            (dx, dy)
    (x+dx, y+dy), (dx', dy')

// How much longer a line is, compared to its thickness:
let factor = 2     

let rec draw (x, y) (dx, dy) n = function 
| [] -> ()
| z::zs ->
    printf "%d,%d " (factor*(x+dx)) (factor*(y+dy))
    let (xyd, d') = goto (x, y) (dx, dy) z n
    draw xyd d' (n^^^1) zs

// Seq of (width,height). n-th (n>=0) pair is for fibword fractal f(3*n+2)
let wh = Seq.unfold (fun ((w1,h1,n),(w2,h2)) ->
    Some((if n=0 then (w1,h1) else (h1,w1)), ((w2,h2,n^^^1),(2*w2+w1,w2+h2)))) ((1,0,1),(3,1))

[<EntryPoint>]
let main argv =
    let n = (if argv.Length > 0 then int (System.UInt16.Parse(argv.[0])) else 23)
    let (width,height) = Seq.head <| Seq.skip (n/3) wh
    let fibWord = Seq.toList (Seq.item (n-1) <| fibwordIterator ['1'])
    let (viewboxWidth, viewboxHeight) = ((factor*(width+1)), (factor*(height+1)))
    printf """<!DOCTYPE html>
<html><body><svg height="100%%" width="100%%" viewbox="0 0 %d %d">
  <polyline points="0,0 """ viewboxWidth viewboxHeight
    draw (0,0) (0,1) 1 <| Seq.toList fibWord
    printf """" style="fill:white;stroke:red;stroke-width:1" transform="matrix(1,0,0,-1,1,%d)"/>
  Sorry, your browser does not support inline SVG.
</svg></body></html>""" (viewboxHeight-1)
    0
```

{{out}}
<p>Since file upload to the Wiki is not possible, the raw output for F<sub>11</sub> is given:</p>
<pre style="white-space:pre-wrap"><!DOCTYPE html>
<html><body><svg height="100%" width="100%" viewbox="0 0 36 24">
  <polyline points="0,0 0,2 2,2 4,2 4,0 6,0 8,0 8,2 8,4 6,4 6,6 6,8 8,8 8,10 8,12 6,12 4,12 4,10 2,10 0,10 0,12 0,14 2,14 2,16 2,18 0,18 0,20 0,22 2,22 4,22 4,20 6,20 8,20 8,22 10,22 12,22 12,20 12,18 10,18 10,16 10,14 12,14 14,14 14,16 16,16 18,16 18,14 20,14 22,14 22,16 22,18 20,18 20,20 20,22 22,22 24,22 24,20 26,20 28,20 28,22 30,22 32,22 32,20 32,18 30,18 30,16 30,14 32,14 32,12 32,10 30,10 28,10 28,12 26,12 24,12 24,10 24,8 26,8 26,6 26,4 24,4 24,2 24,0 26,0 28,0 28,2 30,2 32,2 32,0 34,0 " style="fill:white;stroke:red;stroke-width:1" transform="matrix(1,0,0,-1,1,23)"/>
  Sorry, your browser does not support inline SVG.
</svg></body></html>
```



## Factor


```factor
USING: accessors arrays combinators fry images images.loader
kernel literals make match math math.vectors pair-rocket
sequences ;
FROM: fry => '[ _ ;
IN: rosetta-code.fibonacci-word-fractal

! 
###  Turtle code ===========================================


TUPLE: turtle heading loc ;
C: <turtle> turtle

: forward ( turtle -- turtle' )
    dup heading>> [ v+ ] curry change-loc ;
    
MATCH-VARS: ?a ;

CONSTANT: left { { 0 ?a } => [ ?a 0 ] { ?a 0 } => [ 0 ?a neg ] }
CONSTANT: right { { 0 ?a } => [ ?a neg 0 ] { ?a 0 } => [ 0 ?a ] }

: turn ( turtle left/right -- turtle' )
    [ dup heading>> ] dip match-cond 2array >>heading ; inline

! 
###  Fib word ==============================================

   
: fib-word ( n -- str )
    {
        1 => [ "1" ]
        2 => [ "0" ]
        [ [ 1 - fib-word ] [ 2 - fib-word ] bi append ]
    } case ;
    
! 
###  Fractal ===============================================


: fib-word-fractal ( n -- seq )
    [
        [ { 0 -1 } { 10 417 } dup , <turtle> ] dip fib-word
        [
            1 + -rot forward dup loc>> ,
            -rot CHAR: 0 = [
                even? [ left turn ] [ right turn ] if
            ] [ drop ] if drop
        ] with each-index
    ] { } make ;
    
! 
###  Image =================================================


CONSTANT: w 598
CONSTANT: h 428

: init-img-data ( -- seq )
    w h * 4 * [ 255 ] B{ } replicate-as ;

: <fib-word-fractal-img> ( -- img )
    <image>
    ${ w h }         >>dim
    BGRA             >>component-order
    ubyte-components >>component-type
    init-img-data    >>bitmap ;
    
: fract>img ( seq -- img' )
    [ <fib-word-fractal-img> dup ] dip [
        '[ B{ 33 33 33 255 } _ first2 ] dip set-pixel-at
    ] with each ;
    
: main ( -- )
    23 fib-word-fractal fract>img "fib-word-fractal.png"
    save-graphic-image ;
    
MAIN: main
```

{{out}} Similar to fig. 1 from the paper and the image at the top of this page.


## FreeBASIC

On a Windows 32bit system F_word35 is the biggest that can be drawn.

```FreeBASIC
' version 23-06-2015
' compile with: fbc -s console "filename".bas

Dim As String fw1, fw2, fw3
Dim As Integer a, b, d , i, n , x, y, w, h
Dim As Any Ptr img_ptr, scr_ptr

' data for screen/buffer size
Data 1, 2, 3, 2, 2, 2, 2, 2, 7, 10, 8, 14
Dim As Integer s(38,2)
For i = 3 To 9
    Read s(i,1) : Read s(i,2)
Next
For i = 9 To 38 Step 6
    s(i, 1) = s(i -1, 1) +2 : s(i, 2) = s(i -1, 1) + s(i -1, 2)
    s(i +1, 1) = s(i, 2) +2 : s(i +1, 2) = s(i, 2)
    s(i +2, 1) = s(i, 1) + s(i, 2) : s(i +2, 2) = s(i, 2)
    s(i +3, 1) = s(i +1, 1 ) + s(i +2, 1) : s(i +3, 2) = s(i ,2)
    s(i +4, 1) = s(i +3, 1) : s(i +4, 2) = s(i +3, 1) + 2
    s(i +5, 1) = s(i +3, 1) : s(i +5, 2) = s(i +3, 2) + s(i +4, 2) +2
Next

' we need to set screen in order to create image buffer in memory
Screen 21
scr_ptr = ScreenPtr()
If (scr_ptr = 0) Then
    Print "Error: graphics screen not initialized."
    Sleep
    End -1
End If

Do
    Cls
    Do

        Print
        Print "For wich n do you want the Fibonacci Word fractal (3 to 35)."
        While Inkey <> "" : fw1 = Inkey : Wend ' empty keyboard buffer
        Input "Enter or a value smaller then 3 to stop: "; n
        If n < 3 Then
            Print : Print "Stopping."
            Sleep 3000,1
            End
        EndIf
        If n > 35 then
            Print : Print "Fractal is to big, unable to create it."
            Sleep 3000,1
            Continue Do
        End If
    Loop Until n < 36

    fw1 = "1" : fw2 = "0" ' construct the string
    For i = 3 To n
        fw3 = fw2 + fw1
        Swap fw1, fw2    ' swap pointers of fw1 and fw2
        Swap fw2, fw3    ' swap pointers of fw2 and fw3
    Next
    fw1 = "" : fw3 = ""  ' free up memory

    w = s(n, 1) +1 : h = s(n, 2) +1
    ' allocate memory for a buffer to hold the image
    ' use 8 bits to hold the color
    img_ptr = ImageCreate(w,h,0,8)
    If img_ptr = 0 Then  ' check if we have created a image buffer
        Print "Failed to create image."
        Sleep
        End -1
    End If

    x = 0:  y = h -1  : d = 1    ' set starting point and direction flag
    PSet img_ptr, (x, y)         ' set start point
    For a = 1 To Len(fw2)
        Select Case As Const d
            Case 0
                x = x + 2
            Case 1
                y = y - 2
            Case 2
                x = x - 2
            Case 3
                y = y + 2
        End Select
        Line  img_ptr, -(x, y)
        b = fw2[a-1] - Asc("0")
        If b = 0 Then
            If (a And 1) Then
                d = d + 3    ' a = odd
            Else
                d = d + 1    ' a = even
            End If
            d = d And 3
        End If
    Next

    If n < 24 Then  ' size is smaller then screen dispay fractal
        Cls
        Put (5, 5),img_ptr, PSet
    Else
        Print
        Print "Fractal is to big for display."
    End If
    ' saves fractal as bmp file (8 bit palette)
    If n > 23 Then h = 80
    Draw String (0, h +16), "saving fractal as fibword" + Str(n) + ".bmp."
    BSave "F_Word" + Str(n) + ".bmp", img_ptr
    Draw String (0, h +32), "Hit any key to continue."
    Sleep
    ImageDestroy(img_ptr) ' free memory holding the image
Loop
```



## Go

{{libheader|Go Graphics}}
{{trans|Kotlin}}

```go
package main

import (
    "github.com/fogleman/gg"
    "strings"
)

func wordFractal(i int) string {
    if i < 2 {
        if i == 1 {
            return "1"
        }
        return ""
    }
    var f1 strings.Builder
    f1.WriteString("1")
    var f2 strings.Builder
    f2.WriteString("0")
    for j := i - 2; j >= 1; j-- {
        tmp := f2.String()
        f2.WriteString(f1.String())
        f1.Reset()
        f1.WriteString(tmp)
    }
    return f2.String()
}

func draw(dc *gg.Context, x, y, dx, dy float64, wf string) {
    for i, c := range wf {
        dc.DrawLine(x, y, x+dx, y+dy)
        x += dx
        y += dy
        if c == '0' {
            tx := dx
            dx = dy
            if i%2 == 0 {
                dx = -dy
            }
            dy = -tx
            if i%2 == 0 {
                dy = tx
            }
        }
    }
}

func main() {
    dc := gg.NewContext(450, 620)
    dc.SetRGB(0, 0, 0)
    dc.Clear()
    wf := wordFractal(23)
    draw(dc, 20, 20, 1, 0, wf)
    dc.SetRGB(0, 1, 0)
    dc.SetLineWidth(1)
    dc.Stroke()
    dc.SavePNG("fib_wordfractal.png")
}
```


{{out}}

```txt

Image similar to Java entry except green on black background.

```


=={{header|Icon}} and {{header|Unicon}}==
This probably only works in Unicon.  It also defaults to showing the factal for F_word<sub>25</sub> as
larger Fibonacci words quickly exceed the size of window I can display, even with a line segment length of
a single pixel.


```unicon
global width, height

procedure main(A)
    n := integer(A[1]) | 25			    # F_word to use
    sl := integer(A[2]) | 1             # Segment length
    width := integer(A[3]) | 1050       # Width of plot area
    height := integer(A[4]) | 1050      # Height of plot area
    w := fword(n)
    drawFractal(n,w,sl)
end

procedure fword(n)
    static fcache
    initial fcache := table()
    /fcache[n] := case n of {
                     1: "1"
                     2: "0"
                     default: fword(n-1)||fword(n-2)
                     }
    return fcache[n]
end

record loc(x,y)

procedure drawFractal(n,w,sl)
    static lTurn, rTurn
    initial {
        every (lTurn|rTurn) := table()
        lTurn["north"] := "west"; lTurn["west"] := "south"
        lTurn["south"] := "east"; lTurn["east"] := "north"
        rTurn["north"] := "east"; rTurn["east"] := "south"
        rTurn["south"] := "west"; rTurn["west"] := "north"
        }
    
    wparms := ["FibFractal "||n,"g","bg=white","canvas=normal",
               "fg=black","size="||width||","||height,"dx=10","dy=10"]
    &window := open!wparms | stop("Unable to open window")
    p := loc(10,10)
    d := "north"
    every i := 1 to *w do {
       p := draw(p,d,sl)
       if w[i] == "0" then d := if i%2 = 0 then lTurn[d] else rTurn[d]
       }
 
    until Event() == &lpress
    WriteImage("FibFract"||n||".png")
    close(&window)
end

procedure draw(p,d,sl)
    if d == "north"      then p1 := loc(p.x,p.y+sl)
    else if d == "south" then p1 := loc(p.x,p.y-sl)
    else if d == "east"  then p1 := loc(p.x+sl,p.y)
    else                      p1 := loc(p.x-sl,p.y)
    DrawLine(p.x,p.y, p1.x,p1.y)
    return p1
end
```



## J


Plotting the fractal as a parametric equation, this looks reasonably nice:


```J
require 'plot'
plot }:+/\ 0,*/\(^~ 0j_1 0j1 $~ #)'0'=_1{::F_Words 20
```


Note that we need the definition of F_Words from the [[Fibonacci_word#J|Fibonacci word]] page:


```J
F_Words=: (,<@;@:{~&_1 _2)@]^:(2-~[)&('1';'0')
```


However, image uploads are currently disabled, and rendering images of this sort as wikitext gets bulky.

Instead, I'll just describe the algorithm:

This draws a discrete parametric curve. Right turn is 0j_1, left turn is 0j1 (negative and positive square roots of negative 1), straight ahead is 1. So: build a list of alternating 0j_1 and 0j1 and raise them to the first power for the 0s in the fibonacci word list and raise them to the 0th power for the 1s in that list. Then compute the running product, shift a 0 onto the front of the list of products and compute the running sum. (Of course, this would translate to a rather simple loop, also, once you see the pattern.)


## Java

[[File:fib_word_fractal_java.gif|300px|thumb|right]]
{{works with|Java|8}}

```java
import java.awt.*;
import javax.swing.*;

public class FibonacciWordFractal extends JPanel {
    String wordFractal;

    FibonacciWordFractal(int n) {
        setPreferredSize(new Dimension(450, 620));
        setBackground(Color.white);
        wordFractal = wordFractal(n);
    }

    public String wordFractal(int n) {
        if (n < 2)
            return n == 1 ? "1" : "";

        // we should really reserve fib n space here
        StringBuilder f1 = new StringBuilder("1");
        StringBuilder f2 = new StringBuilder("0");

        for (n = n - 2; n > 0; n--) {
            String tmp = f2.toString();
            f2.append(f1);

            f1.setLength(0);
            f1.append(tmp);
        }

        return f2.toString();
    }

    void drawWordFractal(Graphics2D g, int x, int y, int dx, int dy) {
        for (int n = 0; n < wordFractal.length(); n++) {
            g.drawLine(x, y, x + dx, y + dy);
            x += dx;
            y += dy;
            if (wordFractal.charAt(n) == '0') {
                int tx = dx;
                dx = (n % 2 == 0) ? -dy : dy;
                dy = (n % 2 == 0) ? tx : -tx;
            }
        }
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawWordFractal(g, 20, 20, 1, 0);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Fibonacci Word Fractal");
            f.setResizable(false);
            f.add(new FibonacciWordFractal(23), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

{{trans|PARI/GP}}

[[File:FiboWFractal2.png|200px|right|thumb|Output FiboWFractal2.png]]
[[File:FiboWFractal1.png|200px|right|thumb|Output FiboWFractal1.png]]


```javascript

// Plot Fibonacci word/fractal
// FiboWFractal.js - 6/27/16 aev
function pFibowFractal(n,len,canvasId,color) {
  // DCLs
  var canvas = document.getElementById(canvasId);
  var ctx = canvas.getContext("2d"); 
  var w = canvas.width; var h = canvas.height;
  var fwv,fwe,fn,tx,x=10,y=10,dx=len,dy=0,nr;
  // Cleaning canvas, setting plotting color, etc 
  ctx.fillStyle="white"; ctx.fillRect(0,0,w,h);
  ctx.beginPath();
  ctx.moveTo(x,y);
  fwv=fibword(n); fn=fwv.length;
  // MAIN LOOP
  for(var i=0; i<fn; i++) {
    ctx.lineTo(x+dx,y+dy); fwe=fwv[i]; 
    if(fwe=="0") {tx=dx; nr=i%2;
      if(nr==0) {dx=-dy;dy=tx} else {dx=dy;dy=-tx}};
    x+=dx; y+=dy;
  }//fend i
  ctx.strokeStyle = color; ctx.stroke();
}//func end
// Create and return Fibonacci word
function fibword(n) {
  var f1="1",f2="0",fw,fwn,n2,i;
  if (n<5) {n=5}; n2=n+2;
  for (i=0; i<n2; i++) {fw=f2+f1;f1=f2;f2=fw};
  return(fw)
}

```
 

'''Executing:'''

```html

<!-- FiboWFractal2.html -->
<html>
<head>
  <title>Fibonacci word/fractal</title>
  <script src="FiboWFractal.js"></script>
</head>
<body onload="pFibowFractal(31,2,'canvid','red')">
   <h3>Fibonacci word/fractal: n=31, len=2</h3>
   <canvas id="canvid" width="850" height="1150" style="border: 2px inset;"></canvas>
</body>
</html>

<!-- FiboWFractal1.html -->
<html>
<head>
  <title>Fibonacci word/fractal</title>
  <script src="FiboWFractal.js"></script>
</head>
<body onload="pFibowFractal(31,1,'canvid','navy')">
   <h3>Fibonacci word/fractal: n=31, len=1</h3>
   <canvas id="canvid" width="1400" height="1030" style="border: 2px inset;"></canvas>
</body>
</html>

```
 

{{Output}}


```txt

Page with FiboWFractal2.png
Page with FiboWFractal1.png

```



## Julia

{{works with|Julia|0.6}}


```julia
using Luxor, Colors

function fwfractal!(word::AbstractString, t::Turtle)
    left  =  90
    right = -90
    for (n, c) in enumerate(word)
        Forward(t)
        if c == '0'
            Turn(t, ifelse(iseven(n), left, right))
        end
    end
    return t
end

word = last(fiboword(25))

touch("data/fibonaccifractal.png")
Drawing(800, 800, "data/fibonaccifractal.png");
background(colorant"white")
t = Turtle(100, 300)
fwfractal!(word, t)
finish()
preview()
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

import java.awt.*
import javax.swing.*

class FibonacciWordFractal(n: Int) : JPanel() {
    private val wordFractal: String 
 
    init {
        preferredSize = Dimension(450, 620)
        background = Color.black
        wordFractal = wordFractal(n)
    }

    fun wordFractal(i: Int): String {
        if (i < 2) return if (i == 1) "1" else ""
        val f1 = StringBuilder("1")
        val f2 = StringBuilder("0")
 
        for (j in i - 2 downTo 1) {
            val tmp = f2.toString()
            f2.append(f1)
            f1.setLength(0)
            f1.append(tmp)
        }
 
        return f2.toString()
    }

    private fun drawWordFractal(g: Graphics2D, x: Int, y: Int, dx: Int, dy: Int) {
        var x2 = x
        var y2 = y
        var dx2 = dx
        var dy2 = dy 
        for (i in 0 until wordFractal.length) {
            g.drawLine(x2, y2, x2 + dx2, y2 + dy2)
            x2 += dx2
            y2 += dy2
            if (wordFractal[i] == '0') {
                val tx = dx2
                dx2 = if (i % 2 == 0) -dy2 else dy2
                dy2 = if (i % 2 == 0) tx else -tx
            }
        }
    }

    override fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.color = Color.green
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON)
        drawWordFractal(g, 20, 20, 1, 0)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        with(f) {
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            title = "Fibonacci Word Fractal"
            isResizable = false
            add(FibonacciWordFractal(23), BorderLayout.CENTER)
            pack()
            setLocationRelativeTo(null)
            isVisible = true
        }
    }
}
```



## Logo

<code>fibonacci.word.fractal</code> can draw any number of line segments.  A Fibonacci number shows the self-similar nature of the fractal.  The Fibonacci word values which control the turns are generated here by some bit-twiddling iteration.

{{works with|UCB Logo}}

```Logo
; Return the low 1-bits of :n
; For example if n = binary 10110111 = 183
;        then return binary      111 = 7
to low.ones :n
  output ashift (bitxor :n (:n+1)) -1
end

; :fibbinary should be a fibbinary value
; return the next larger fibbinary value
to fibbinary.next :fibbinary
  localmake "filled  bitor :fibbinary (ashift :fibbinary -1)
  localmake "mask    low.ones :filled
  output (bitor :fibbinary :mask) + 1
end

to fibonacci.word.fractal :steps
  localmake "step.length 5  ; length of each step
  localmake "fibbinary 0
  repeat :steps [
    forward :step.length
    if (bitand 1 :fibbinary) = 0 [
      ifelse (bitand repcount 1) = 1 [right 90] [left 90]
    ]
    make "fibbinary  fibbinary.next :fibbinary
  ]
end

setheading 0    ; initial line North
fibonacci.word.fractal 377
```



## Lua

Needs L&Ouml;VE 2D Engine

```lua

RIGHT, LEFT, UP, DOWN = 1, 2, 4, 8
function drawFractals( w )
    love.graphics.setCanvas( canvas )
    love.graphics.clear()
    love.graphics.setColor( 255, 255, 255 )
    local dir, facing, lineLen, px, py, c = RIGHT, UP, 1, 10, love.graphics.getHeight() - 20, 1
    local x, y = 0, -lineLen
    local pts = {}
    table.insert( pts, px + .5 ); table.insert( pts, py + .5 )
    for i = 1, #w do
        px = px + x; table.insert( pts, px + .5 )
        py = py + y; table.insert( pts, py + .5 )
        if w:sub( i, i ) == "0" then 
            if c % 2 == 1 then dir = RIGHT else dir = LEFT end
            if facing == UP then
                if dir == RIGHT then x = lineLen; facing = RIGHT
                else x = -lineLen; facing = LEFT end; y = 0
            elseif facing == RIGHT then
                if dir == RIGHT then y = lineLen; facing = DOWN
                else y = -lineLen; facing = UP end; x = 0
            elseif facing == DOWN then
                if dir == RIGHT then x = -lineLen; facing = LEFT
                else x = lineLen; facing = RIGHT end; y = 0
            elseif facing == LEFT then
                if dir == RIGHT then y = -lineLen; facing = UP
                else y = lineLen; facing = DOWN end; x = 0
            end
        end
        c = c + 1
    end
    love.graphics.line( pts )
    love.graphics.setCanvas()
end
function createWord( wordLen )
    local a, b, w = "1", "0"
    repeat
        w = b .. a; a = b; b = w; wordLen = wordLen - 1
    until wordLen == 0
    return w
end
function love.load()
    wid, hei = love.graphics.getWidth(), love.graphics.getHeight()
    canvas = love.graphics.newCanvas( wid, hei )
    drawFractals( createWord( 21 ) )
end
function love.draw()
    love.graphics.draw( canvas )
end

```



=={{header|Mathematica}} / {{header|Wolfram Language}}==


```mathematica
(*note, this usage of Module allows us to memoize FibonacciWord
  without exposing it to the global scope*)
Module[{FibonacciWord, step},
  FibonacciWord[1] = "1";
  FibonacciWord[2] = "0";
  FibonacciWord[n_Integer?(# > 2 &)] :=
   (FibonacciWord[n] = FibonacciWord[n - 1] <> FibonacciWord[n - 2]);
  
  step["0", {_?EvenQ}] = N@RotationTransform[Pi/2];
  step["0", {_?OddQ}] = N@RotationTransform[-Pi/2];
  step[___] = Identity;
  
  FibonacciFractal[n_] := Module[{steps, dirs},
    steps = MapIndexed[step, Characters[FibonacciWord[n]]];
    dirs = ComposeList[steps, {0, 1}];
    Graphics[Line[FoldList[Plus, {0, 0}, dirs]]]]];
```



## PARI/GP


### Version #1.


In this version only function plotfibofract() was translated from C++,
plus upgraded to plot different kind/size of Fibonacci word/fractals.

[[File:Fibofrac1.png|200px|right|thumb|Output Fibofrac1.png]]
[[File:Fibofrac2.png|200px|right|thumb|Output Fibofrac2.png]]

{{trans|C++}}

{{Works with|PARI/GP|2.7.4 and above}}


```parigp

\\ Fibonacci word/fractals
\\ 4/25/16 aev
fibword(n)={
my(f1="1",f2="0",fw,fwn,n2);
if(n<=4, n=5);n2=n-2;
for(i=1,n2, fw=Str(f2,f1); f1=f2;f2=fw;); fwn=#fw;
fw=Vecsmall(fw);
for(i=1,fwn,fw[i]-=48);
return(fw);
}

nextdir(n,d)={
my(dir=-1);
if(d==0, if(n%2==0, dir=0,dir=1)); \\0-left,1-right
return(dir);
}

plotfibofract(n,sz,len)={
my(fwv,fn,dr,px=10,py=420,x=0,y=-len,g2=0,
   ttl="Fibonacci word/fractal: n=");
plotinit(0); plotcolor(0,6); \\green 
plotscale(0, -sz,sz, -sz,sz);
plotmove(0, px,py);
fwv=fibword(n); fn=#fwv;
for(i=1,fn, 
    plotrline(0,x,y);
    dr=nextdir(i,fwv[i]);
    if(dr==-1, next);
    \\up
    if(g2==0, y=0; if(dr, x=len;g2=1, x=-len;g2=3); next);
    \\right
    if(g2==1, x=0; if(dr, y=len;g2=2, y=-len;g2=0); next);
    \\down
    if(g2==2, y=0; if(dr, x=-len;g2=3, x=len;g2=1); next);
    \\left
    if(g2==3, x=0; if(dr, y=-len;g2=0, y=len;g2=2); next);
   );\\fend i 
plotdraw([0,-sz,-sz]);
print(" *** ",ttl,n," sz=",sz," len=",len," fw-len=",fn);

}

{\\ Executing:
plotfibofract(11,430,20); \\ Fibofrac1.png
plotfibofract(21,430,2);  \\ Fibofrac2.png
}

```
 

{{Output}}


```txt

> plotfibofract(11,430,20); \\ Fibofrac1.png
 *** Fibonacci word/fractal: n=11 sz=430 len=20 fw-len=89

> plotfibofract(21,430,2);  \\ Fibofrac2.png
 *** Fibonacci word/fractal: n=21 sz=430 len=2 fw-len=10946

```



### Version #2.

In this version only function plotfibofract1() was translated from Java,
plus upgraded to plot different kind/size of Fibonacci word/fractals.

[[File:Fibofrac3.png|200px|right|thumb|Output Fibofrac3.png]]
[[File:Fibofrac4.png|200px|right|thumb|Output Fibofrac4.png]]

{{trans|Java}}

{{Works with|PARI/GP|2.7.4 and above}}


```parigp

\\ Fibonacci word/fractals 2nd version
\\ 4/26/16 aev
fibword(n)={
my(f1="1",f2="0",fw,fwn,n2); \\check n2 in v2 ADD it!!
if(n<=4, n=5); n2=n-2;
for(i=1,n2, fw=Str(f2,f1); f1=f2;f2=fw;); fwn=#fw;
fw=Vecsmall(fw);
for(i=1,fwn,fw[i]-=48);
return(fw);
}

plotfibofract1(n,sz,len)={
my(fwv,fn,dx=len,dy=0,nr,ttl="Fibonacci word/fractal, n=");
plotinit(0); plotcolor(0,5); \\red 
plotscale(0, -sz,sz, -sz,sz); plotmove(0, 0,0);
fwv=fibword(n); fn=#fwv;
for(i=1,fn, plotrline(0,dx,dy);
    if(fwv[i]==0, tx=dx; nr=i%2; if(!nr,dx=-dy;dy=tx, dx=dy;dy=-tx));
   );\\fend i 
plotdraw([0,0,0]); 
print(" *** ",ttl,n," sz=",sz," len=",len," fw-len=",fn);
}

{\\ Executing:
plotfibofract1(17,500,6); \\ Fibofrac3.png
plotfibofract1(21,600,1); \\ Fibofrac4.png
}

```
 

{{Output}}


```txt

> plotfibofract1(17,500,6); \\ Fibofrac3.png
 *** Fibonacci word/fractal: n=17 sz=500 len=6 fw-len=1597

> plotfibofract1(21,600,1); \\ Fibofrac4.png
 *** Fibonacci word/fractal: n=21 sz=600 len=1 fw-len=10946

```



## Perl

Creates file fword.png containing the Fibonacci Fractal.

```perl
use strict;
use warnings;
use GD;

my @fword = ( undef, 1, 0 );

sub fword {
	my $n = shift;
	return $fword[$n] if $n<3;
	return $fword[$n] //= fword($n-1).fword($n-2);
}

my $size = 3000;
my $im = new GD::Image($size,$size);
my $white = $im->colorAllocate(255,255,255);
my $black = $im->colorAllocate(0,0,0);       
$im->transparent($white);
$im->interlaced('true');

my @pos   = (0,0);
my @dir   = (0,5);
my @steps = split //, fword 23;
my $i     = 1;
for( @steps ) {
	my @next = ( $pos[0]+$dir[0], $pos[1]+$dir[1] );
	$im->line( @pos, @next, $black );
	@dir = (  $dir[1], -$dir[0] ) if 0==$_ && 1==$i%2; # odd
	@dir = ( -$dir[1],  $dir[0] ) if 0==$_ && 0==$i%2; # even
	$i++;
	@pos = @next;
}

open my $out, ">", "fword.png" or die "Cannot open output file.\n";
binmode $out;
print $out $im->png;
close $out;

```



## Perl 6


```perl6
constant @fib-word = '1', '0', { $^b ~ $^a } ... *;

sub MAIN($m = 17, $scale = 3) {
    (my %world){0}{0} = 1;
    my $loc = 0+0i;
    my $dir = i;
    my $n = 1;

    for @fib-word[$m].comb {
        when '0' {
            step;
            if $n %% 2 { turn-left }
            else { turn-right; }
        }
        $n++;
    }

    braille-graphics %world;

    sub step {
        for ^$scale {
            $loc += $dir;
            %world{$loc.im}{$loc.re} = 1;
        }
    }

    sub turn-left  { $dir *= i; }
    sub turn-right { $dir *= -i; }

}

sub braille-graphics (%a) {
    my ($ylo, $yhi, $xlo, $xhi);
    for %a.keys -> $y {
	$ylo min= +$y; $yhi max= +$y;
	for %a{$y}.keys -> $x {
	    $xlo min= +$x; $xhi max= +$x;
	}
    }

    for $ylo, $ylo + 4 ...^ * > $yhi -> \y {
	for $xlo, $xlo + 2 ...^ * > $xhi -> \x {
	    my $cell = 0x2800;
	    $cell += 1   if %a{y + 0}{x + 0};
	    $cell += 2   if %a{y + 1}{x + 0};
	    $cell += 4   if %a{y + 2}{x + 0};
	    $cell += 8   if %a{y + 0}{x + 1};
	    $cell += 16  if %a{y + 1}{x + 1};
	    $cell += 32  if %a{y + 2}{x + 1};
	    $cell += 64  if %a{y + 3}{x + 0};
	    $cell += 128 if %a{y + 3}{x + 1};
	    print chr($cell);
	}
	print "\n";
    }
}
```

{{out}}
<small><small>
```txt
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣇⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⠃⠘⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⣇⣸⠉⣇⣀⠀⣀⣸⠉⣇⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⡖⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢈⣉⡁⠀⠀⠀⢈⣉⡁⢈⣉⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠧⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⡏⢹⣀⡏⠉⠀⠉⢹⣀⡏⢹⣀⡀⢀⣀⡏⢹⣀⡏⠉⠀⠉⢹⣀⡏⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⡄⢠⠤⡄⠀⠀⠀⢠⠤⡄⢠⠤⠇⠸⠤⡄⢠⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⠚⠀⠓⢲⠀⡖⠚⠀⠓⠚⠀⠀⠀⠀⠓⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⢀⣀⡏⢹⣀⡀⢀⣀⡏⢹⣀⡏⠉⠀⠉⢹⣀⡏⢹⣀⡀⢀⣀⡏⢹⣀⡏⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠸⠤⡄⢠⠤⠇⠸⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⠤⠇⠸⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⢰⠒⡆⢰⠒⠃⠘⠒⡆⢰⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⠒⡆⢰⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⣏⣉⠀⣉⣉⠀⠀⠀⠀⣉⣉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣉⣉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠸⠤⠇⠸⠤⡄⢠⠤⠇⠸⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⠤⠇⠸⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⠤⠇⠸⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⢰⠒⠃⠘⠒⡆⢰⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⠒⡆⢰⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⠒⡆⢰⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠈⠉⣇⣸⠉⠁⠈⠉⣇⣸⠉⣇⣀⠀⣀⣸⠉⣇⣸⠉⠁⠈⠉⣇⣸⠉⣇⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣸⠉⣇⣸⠉⠁⠈⠉⣇⣸⠉⣇⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠧⢤⠀⡤⢤⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠧⢤⠀⡤⢤⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⠃⠘⠒⠃⠀⠀⠀⠘⠒⠃⠘⠒⡆⢰⠒⠃⠘⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⠒⠃⠘⠒⡆⢰⠒⠃⠘⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⣇⣸⠉⣇⣀⠀⣀⣸⠉⣇⣸⠉⠁⠈⠉⣇⣸⠉⣇⣀⠀⣀⣸⠉⣇⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣸⠉⣇⣀⠀⣀⣸⠉⣇⣸⠉⠁⠈⠉⣇⣸⠉⣇⣀⠀⣀⣸⠉⣇⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠧⢤⠀⡤⠼⠀⠧⢤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⡖⢲⠀⠀⠀⠀⡖⢲⠀⡖⠚⠀⠓⢲⠀⡖⠚⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⡖⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢈⣉⡁⠀⠀⠀⢈⣉⡁⢈⣉⡇⢸⣉⡁⢈⣉⡁⠀⠀⠀⢈⣉⡁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢈⣉⡁⠀⠀⠀⢈⣉⡁⢈⣉⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠧⠼⠀⠀⠀⠀⠧⠼⠀⠧⢤⠀⡤⠼⠀⠧⢤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠧⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡖⠚⠀⠓⢲⠀⡖⠚⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⡏⢹⣀⡏⠉⠀⠉⢹⣀⡏⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⢹⣀⡏⠉⠀⠉⢹⣀⡏⢹⣀⡀⢀⣀⡏⢹⣀⡏⠉⠀⠉⢹⣀⡏⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⡄⢠⠤⡄⠀⠀⠀⢠⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⠤⡄⠀⠀⠀⢠⠤⡄⢠⠤⠇⠸⠤⡄⢠⠤⡄⠀⠀⠀⢠⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⠚⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡖⠚⠀⠓⢲⠀⡖⠚⠀⠓⠚⠀⠀⠀⠀⠓⠚⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣹⠀⣏⣉⠀⣀⣀⠀⠀⠀⠀⣀⣀⠀⣉⣹⠀⣏⣉⠀⣉⣹⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣹⠀⣏⣉⠀⣀⣀⠀⠀⠀⠀⣀⣀⠀⣉⣹⠀⣏⣉⠀⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⠇⠀⠀⠀⠸⠤⠇⠸⠤⡄⢠⠤⠇⠸⠤⠇⠀⠀⠀⠸⠤⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⠇⠀⠀⠀⠸⠤⠇⠸⠤⡄⢠⠤⠇⠸⠤⠇⠀⠀⠀⠸⠤⠇⠸⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⡆⢰⠒⠃⠘⠒⡆⢰⠒⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⡆⢰⠒⠃⠘⠒⡆⢰⠒⡆⠀⠀⠀⢰⠒⡆⢰⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣉⠀⠀⠀⠀⣉⣉⠀⣉⣹⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣉⠀⠀⠀⠀⣉⣉⠀⣉⣹⠀⣏⣉⠀⣉⣉⠀⠀⠀⠀⣀⣀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⠇⠸⠤⡄⢠⠤⠇⠸⠤⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⠇⠸⠤⡄⢠⠤⠇⠸⠤⠇⠀⠀⠀⠸⠤⠇⠸⠤⡄⢠⠤⠇⠸⠤⡄⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⠃⠘⠒⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⠃⠘⠒⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⠃⠘⠒⡆⢰⠒⠃⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⣇⣸⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⣇⣸⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⣇⣸⠉⠁⠈⠉⣇⣸⠉⣇⣀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⢤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⢤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⢤⠀⠀⠀⠀⡤⢤⠀⡤⠼
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⠃⠘⠒⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⠃⠘⠒⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⠃⠘⠒⡆⢰⠒⠃⠘⠒⠃⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⡀⢈⣉⡇⢸⣉⡁⢀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⡀⢈⣉⡇⢸⣉⡁⢀⣀⡀⠀⠀⠀⢀⣀⡀⢈⣉⡇⢸⣉⡁⢈⣉⡇⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⠼⠀⠀⠀⠀⠧⠼⠀⠧⢤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⠼⠀⠀⠀⠀⠧⠼⠀⠧⢤⠀⡤⠼⠀⠧⠼⠀⠀⠀⠀⠧⠼⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⢲⠀⠀⠀⠀⡖⢲⠀⡖⠚⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⢲⠀⠀⠀⠀⡖⢲⠀⡖⠚⠀⠓⢲⠀⡖⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⡀⠀⠀⠀⢈⣉⡁⢈⣉⡇⢸⣉⡁⢈⣉⡁⠀⠀⠀⢀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⡀⠀⠀⠀⢈⣉⡁⢈⣉⡇⢸⣉⡁⢈⣉⡁⠀⠀⠀⢈⣉⡁⢈⣉⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠧⠼⠀⠀⠀⠀⠧⠼⠀⠧⢤⠀⡤⠼⠀⠧⢤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠧⠼⠀⠀⠀⠀⠧⠼⠀⠧⢤⠀⡤⠼⠀⠧⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡖⠚⠀⠓⢲⠀⡖⠚⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⡏⢹⣀⡏⠉⠀⠉⢹⣀⡏⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⢹⣀⡏⠉⠀⠉⢹⣀⡏⢹⣀⡀⢀⣀⡏⢹⣀⡏⠉⠀⠉⢹⣀⡏⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⢹⣀⡏⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⡄⢠⠤⡄⠀⠀⠀⢠⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⠤⡄⠀⠀⠀⢠⠤⡄⢠⠤⠇⠸⠤⡄⢠⠤⡄⠀⠀⠀⢠⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⠚⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡖⠚⠀⠓⢲⠀⡖⠚⠀⠓⠚⠀⠀⠀⠀⠓⠚⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣹⠀⣏⣉⠀⣀⣀⠀⠀⠀⠀⣀⣀⠀⣉⣹⠀⣏⣉⠀⣉⣹⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣹⠀⣏⣉⠀⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⠇⠀⠀⠀⠸⠤⠇⠸⠤⡄⢠⠤⠇⠸⠤⠇⠀⠀⠀⠸⠤⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⠇⠀⠀⠀⠸⠤⠇⠸⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⡆⠀⠀⠀⢰⠒⡆⢰⠒⠃⠘⠒⡆⢰⠒⡆⠀⠀⠀⢰⠒⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⡆⠀⠀⠀⢰⠒⡆⢰⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣹⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣹⠀⣏⣉⠀⠉⠉⠀⠀⠀⠀⠉⠉⠀⣉⣹⠀⣏⣉⠀⣉⣹⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣹⠀⣏⣉⠀⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠧⢤⠀⡤⢤⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠧⢤⠀⡤⠼⠀⠧⢤⠀⡤⢤⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⠃⠘⠒⠃⠀⠀⠀⠘⠒⠃⠘⠒⡆⢰⠒⠃⠘⠒⠃⠀⠀⠀⠘⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⠒⠃⠀⠀⠀⠘⠒⠃⠘⠒⡆⢰⠒⠃⠘⠒⠃⠀⠀⠀⠘⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⣇⣸⠉⣇⣀⠀⣀⣸⠉⣇⣸⠉⠁⠈⠉⣇⣸⠉⣇⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣸⠉⣇⣸⠉⠁⠈⠉⣇⣸⠉⣇⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⡤⢤⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠧⢤⠀⡤⢤⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠧⢤⠀⡤⢤⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⢰⠒⠃⠘⠒⡆⢰⠒⠃⠘⠒⠃⠀⠀⠀⠘⠒⠃⠘⠒⡆⢰⠒⠃⠘⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⠒⠃⠘⠒⡆⢰⠒⠃⠘⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⢀⣀⡀⢈⣉⡇⢸⣉⡁⢈⣉⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣉⡁⢈⣉⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣉⡁⢈⣉⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⡤⠼⠀⠧⠼⠀⠀⠀⠀⠧⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠧⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠧⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠓⢲⠀⡖⢲⠀⠀⠀⠀⡖⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡖⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠈⠉⠁⢈⣉⡇⢸⣉⡁⢈⣉⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣉⡁⢈⣉⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠸⠤⡄⢠⠤⠇⠸⠤⡄⢠⠤⡄⠀⠀⠀⢠⠤⡄⢠⠤⠇⠸⠤⡄⢠⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠓⠚⠀⠀⠀⠀⠓⠚⠀⠓⢲⠀⡖⠚⠀⠓⠚⠀⠀⠀⠀⠓⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⡏⢹⣀⡏⠉⠀⠉⢹⣀⡏⢹⣀⡀⢀⣀⡏⢹⣀⡏⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⡄⢠⠤⡄⠀⠀⠀⢠⠤⡄⢠⠤⠇⠸⠤⡄⢠⠤⡄⠀⠀⠀⢠⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⠚⠀⠓⢲⠀⡖⠚⠀⠓⠚⠀⠀⠀⠀⠓⠚⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣹⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣹⠀⣏⣉⠀⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⠤⠇⠀⠀⠀⠸⠤⠇⠸⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⡆⠀⠀⠀⢰⠒⡆⢰⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣏⣉⠀⣉⣹⠀⣏⣉⠀⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⠃⠘⠒⠃⠀⠀⠀⠘⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⣇⣸⠉⣇⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠃
```
</small></small>


## Phix

Output matches Fig 1 (at the top of the page)
{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\FibonacciFractal.exw
--
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

procedure drawFibonacci(integer x, y, dx, dy, n)
string prev = "1", word = "0"
    for i=3 to n do {prev,word} = {word,word&prev} end for
    for i=1 to length(word) do
        cdCanvasLine(cddbuffer, x, y, x+dx, y+dy)
        x += dx   y += dy
        if word[i]=='0' then
            {dx,dy} = iff(remainder(i,2)?{dy,-dx}:{-dy,dx})
        end if
    end for
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    drawFibonacci(20, 20, 0, 1, 23)
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    cdCanvasSetForeground(cddbuffer, CD_GREEN)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "620x450")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas, "RESIZE=NO")
    IupSetAttribute(dlg, "TITLE", "Fibonacci Fractal")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    IupMap(dlg)
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## Python

{{trans|Unicon}}
Note that for Python 3, [http://docs.python.org/py3k/library/functools.html#functools.lru_cache functools.lru_cache] could be used instead of the memoize decorator below.

```python
from functools import wraps
from turtle import *

def memoize(obj):
    cache = obj.cache = {}
    @wraps(obj)
    def memoizer(*args, **kwargs):
        key = str(args) + str(kwargs)
        if key not in cache:
            cache[key] = obj(*args, **kwargs)
        return cache[key]
    return memoizer

@memoize
def fibonacci_word(n):
    assert n > 0
    if n == 1:
        return "1"
    if n == 2:
        return "0"
    return fibonacci_word(n - 1) + fibonacci_word(n - 2)

def draw_fractal(word, step):
    for i, c in enumerate(word, 1):
        forward(step)
        if c == "0":
            if i % 2 == 0:
                left(90)
            else:
                right(90)

def main():
    n = 25 # Fibonacci Word to use.
    step = 1 # Segment length.
    width = 1050 # Width of plot area.
    height = 1050 # Height of plot area.
    w = fibonacci_word(n)

    setup(width=width, height=height)
    speed(0)
    setheading(90)
    left(90)
    penup()
    forward(500)
    right(90)
    backward(500)
    pendown()
    tracer(10000)
    hideturtle()

    draw_fractal(w, step)

    # Save Poscript image.
    getscreen().getcanvas().postscript(file="fibonacci_word_fractal.eps")
    exitonclick()

if __name__ == '__main__':
    main()
```

The output image is probably the same.


## R

{{trans|PARI/GP}}
{{Works with|R|3.3.1 and above}}
[[File:FiboFractR23.png|right|thumb|Output FiboFractR23.png]]
[[File:FiboFractR25.png|right|thumb|Output FiboFractR25.png]]


```r

## Fibonacci word/fractal  2/20/17 aev
## Create Fibonacci word order n
fibow <- function(n) {
  t2="0"; t1="01"; t="";
  if(n<2) {n=2}
  for (i in 2:n) {t=paste0(t1,t2); t2=t1; t1=t}
  return(t)
}
## Plot Fibonacci word/fractal:
## n - word order, w - width, h - height, d - segment size, clr - color.
pfibofractal <- function(n, w, h, d, clr) {
  dx=d; x=y=x2=y2=tx=dy=nr=0;
  if(n<2) {n=2}
  fw=fibow(n); nf=nchar(fw);
  pf = paste0("FiboFractR", n, ".png");
  ttl=paste0("Fibonacci word/fractal, n=",n);
  cat(ttl,"nf=", nf, "pf=", pf,"\n");
  plot(NA, xlim=c(0,w), ylim=c(-h,0), xlab="", ylab="", main=ttl)
  for (i in 1:nf) {
    fwi=substr(fw, i, i);
    x2=x+dx; y2=y+dy;
    segments(x, y, x2, y2, col=clr); x=x2; y=y2;
    if(fwi=="0") {tx=dx; nr=i%%2;
      if(nr==0) {dx=-dy;dy=tx} else {dx=dy;dy=-tx}}
  }
  dev.copy(png, filename=pf, width=w, height=h); # plot to png-file
  dev.off(); graphics.off();  # Cleaning
}

## Executing:
pfibofractal(23, 1000, 1000, 1, "navy")
pfibofractal(25, 2300, 1000, 1, "red")

```
 

{{Output}}

```txt

> pfibofractal(23, 1000, 1000, 1, "navy")
Fibonacci word/fractal, n=23 nf= 75025 pf= FiboFractR23.png 
> pfibofractal(25, 2300, 1000, 1, "red")
Fibonacci word/fractal, n=25 nf= 196418 pf= FiboFractR25.png 

```



## REXX

Programming note:   the starting point   (<big>'''.'''</big>)   and the ending point   (<big>'''∙'''</big>)   are also shown to help visually identify the end points.

About half of the REXX program is dedicated to plotting the appropriate characters.

The output of this REXX program is written to the screen as well as a disk file.

```rexx
/*REXX program generates a  Fibonacci word,  then displays the  fractal curve.          */
parse arg ord .                                  /*obtain optional arguments from the CL*/
if ord==''  then ord=23                          /*Not specified?   Then use the default*/
s=FibWord(ord)                                   /*obtain the  order  of Fibonacci word.*/
                                   x=0;    maxX=0;    dx=0;     b=' ';       @.=b;    xp=0
                                   y=0;    maxY=0;    dy=1;               @.0.0=.;    yp=0
  do n=1  for length(s);  x=x+dx;  y=y+dy        /*advance the plot for the next point. */
  maxX=max(maxX,x);  maxY=max(maxY,y)            /*set the maximums for displaying plot.*/
  c='│';  if dx\==0  then c="─";      if n==1  then c='┌'      /*is this the first plot?*/
  @.x.y=c                                        /*assign a plotting character for curve*/
  if @(xp-1,yp)\==b  then if  @(xp,yp-1)\==b  then call @ xp,yp,'┐'   /*fix─up a corner.*/
  if @(xp-1,yp)\==b  then if  @(xp,yp+1)\==b  then call @ xp,yp,'┘'   /*   "   "    "   */
  if @(xp+1,yp)\==b  then if  @(xp,yp+1)\==b  then call @ xp,yp,'└'   /*   "   "    "   */
  if @(xp+1,yp)\==b  then if  @(xp,yp-1)\==b  then call @ xp,yp,'┌'   /*   "   "    "   */
  xp=x;    yp=y;    z=substr(s,n,1)              /*save old x,y;  assign plot character.*/
  if z==1    then iterate                        /*Is Z equal to unity?  Then ignore it.*/
  ox=dx;     oy=dy;   dx=0;    dy=0              /*save   DX,DY   as the old versions.  */
  d=-n//2;   if d==0  then d=1                   /*determine the sign for the chirality.*/
  if oy\==0  then dx=-sign(oy)*d                 /*Going  north|south?   Go  east|west  */
  if ox\==0  then dy= sign(ox)*d                 /*  "     east|west?     " south|north */
  end   /*n*/

call @  x, y, '∙'                                /*set the last point that was plotted. */

      do r=maxY   to 0  by -1;  _=               /*show single row at a time, top first.*/
         do c=0  to maxX;  _=_ || @.c.r;  end  /*c*/;   _=strip(_, 'T')  /*build a line.*/
      if _==''   then iterate                    /*if the line is blank, then ignore it.*/
      say _;     call lineout "FIBFRACT.OUT", _  /*display the line; also write to disk.*/
      end   /*r*/                                /* [↑]  only display the non-blank rows*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
@: parse arg xx,yy,p;     if arg(3)==''  then return @.xx.yy;     @.xx.yy=p;        return
/*──────────────────────────────────────────────────────────────────────────────────────*/
FibWord: procedure; parse arg x;  !.=0;   !.1=1  /*obtain the order of  Fibonacci word. */
                    do k=3  to x; k1=k-1; k2=k-2 /*generate the   Kth       "       "   */
                    !.k=!.k1 || !.k2             /*construct the next       "       "   */
                    end   /*k*/                  /* [↑]  generate a         "       "   */
         return !.x                              /*return the    Xth        "       "   */
```

'''output'''   when using the input:   <tt> 17 </tt>


(The output is shown <sup>1</sup>/<sub>2</sub> size.)
<b>
<pre style="font-size:50%">
┌─┐ ┌─┐   ┌─┐ ┌─┐       ┌─┐ ┌─┐   ┌─┐ ┌─┐                 ┌─┐ ┌─┐   ┌─┐ ┌─┐       ┌─┐ ┌─┐   ┌─┐ ┌─┐
│ └─┘ │   │ └─┘ │       │ └─┘ │   │ └─┘ │                 │ └─┘ │   │ └─┘ │       │ └─┘ │   │ └─┘ │
└┐   ┌┘   └┐   ┌┘       └┐   ┌┘   └┐   ┌┘                 └┐   ┌┘   └┐   ┌┘       └┐   ┌┘   └┐   ┌┘
 │   │ ┌─┐ │   │         │   │ ┌─┐ │   │                   │   │ ┌─┐ │   │         │   │ ┌─┐ │   │
┌┘   └─┘ └─┘   └┐       ┌┘   └─┘ └─┘   └┐                 ┌┘   └─┘ └─┘   └┐       ┌┘   └─┘ └─┘   └┐
│ ┌─┐       ┌─┐ │       │ ┌─┐       ┌─┐ │                 │ ┌─┐       ┌─┐ │       │ ┌─┐       ┌─┐ │
└─┘ │       │ └─┘       └─┘ │       │ └─┘                 └─┘ │       │ └─┘       └─┘ │       │ └─┘
   ┌┘       └┐   ┌─┐ ┌─┐   ┌┘       └┐                       ┌┘       └┐   ┌─┐ ┌─┐   ┌┘       └┐
   │         │   │ └─┘ │   │         │                       │         │   │ └─┘ │   │         │
   └┐       ┌┘   └┐   ┌┘   └┐       ┌┘                       └┐       ┌┘   └┐   ┌┘   └┐       ┌┘
┌─┐ │       │ ┌─┐ │   │ ┌─┐ │       │ ┌─┐                 ┌─┐ │       │ ┌─┐ │   │ ┌─┐ │       │ ┌─┐
│ └─┘       └─┘ └─┘   └─┘ └─┘       └─┘ │                 │ └─┘       └─┘ └─┘   └─┘ └─┘       └─┘ │
└┐   ┌─┐ ┌─┐                 ┌─┐ ┌─┐   ┌┘                 └┐   ┌─┐ ┌─┐                 ┌─┐ ┌─┐   ┌┘
 │   │ └─┘ │                 │ └─┘ │   │                   │   │ └─┘ │                 │ └─┘ │   │
┌┘   └┐   ┌┘                 └┐   ┌┘   └┐                 ┌┘   └┐   ┌┘                 └┐   ┌┘   └┐
│ ┌─┐ │   │                   │   │ ┌─┐ │                 │ ┌─┐ │   │                   │   │ ┌─┐ │
└─┘ └─┘   └┐                 ┌┘   └─┘ └─┘                 └─┘ └─┘   └┐                 ┌┘   └─┘ └─┘
       ┌─┐ │                 │ ┌─┐       ┌─┐ ┌─┐   ┌─┐ ┌─┐       ┌─┐ │                 │ ┌─┐
       │ └─┘                 └─┘ │       │ └─┘ │   │ └─┘ │       │ └─┘                 └─┘ │
       └┐                       ┌┘       └┐   ┌┘   └┐   ┌┘       └┐                       ┌┘
        │                       │         │   │ ┌─┐ │   │         │                       │
       ┌┘                       └┐       ┌┘   └─┘ └─┘   └┐       ┌┘                       └┐
       │ ┌─┐                 ┌─┐ │       │ ┌─┐       ┌─┐ │       │ ┌─┐                 ┌─┐ │
       └─┘ │                 │ └─┘       └─┘ │       │ └─┘       └─┘ │                 │ └─┘
┌─┐ ┌─┐   ┌┘                 └┐   ┌─┐ ┌─┐   ┌┘       └┐   ┌─┐ ┌─┐   ┌┘                 └┐   ┌─┐ ┌─┐
│ └─┘ │   │                   │   │ └─┘ │   │         │   │ └─┘ │   │                   │   │ └─┘ │
└┐   ┌┘   └┐                 ┌┘   └┐   ┌┘   └┐       ┌┘   └┐   ┌┘   └┐                 ┌┘   └┐   ┌┘
 │   │ ┌─┐ │                 │ ┌─┐ │   │ ┌─┐ │       │ ┌─┐ │   │ ┌─┐ │                 │ ┌─┐ │   │
┌┘   └─┘ └─┘                 └─┘ └─┘   └─┘ └─┘       └─┘ └─┘   └─┘ └─┘                 └─┘ └─┘   └┐
│ ┌─┐       ┌─┐ ┌─┐   ┌─┐ ┌─┐                                         ┌─┐ ┌─┐   ┌─┐ ┌─┐       ┌─┐ │
└─┘ │       │ └─┘ │   │ └─┘ │                                         │ └─┘ │   │ └─┘ │       │ └─┘
   ┌┘       └┐   ┌┘   └┐   ┌┘                                         └┐   ┌┘   └┐   ┌┘       └┐
   │         │   │ ┌─┐ │   │                                           │   │ ┌─┐ │   │         │
   └┐       ┌┘   └─┘ └─┘   └┐                                         ┌┘   └─┘ └─┘   └┐       ┌┘
┌─┐ │       │ ┌─┐       ┌─┐ │                                         │ ┌─┐       ┌─┐ │       │ ┌─┐
│ └─┘       └─┘ │       │ └─┘                                         └─┘ │       │ └─┘       └─┘ │
└┐   ┌─┐ ┌─┐   ┌┘       └┐                                               ┌┘       └┐   ┌─┐ ┌─┐   ┌┘
 │   │ └─┘ │   │         │                                               │         │   │ └─┘ │   │
┌┘   └┐   ┌┘   └┐       ┌┘                                               └┐       ┌┘   └┐   ┌┘   └┐
│ ┌─┐ │   │ ┌─┐ │       │ ┌─┐                                         ┌─┐ │       │ ┌─┐ │   │ ┌─┐ │
└─┘ └─┘   └─┘ └─┘       └─┘ │                                         │ └─┘       └─┘ └─┘   └─┘ └─┘
                 ┌─┐ ┌─┐   ┌┘                                         └┐   ┌─┐ ┌─┐
                 │ └─┘ │   │                                           │   │ └─┘ │
                 └┐   ┌┘   └┐                                         ┌┘   └┐   ┌┘
                  │   │ ┌─┐ │                                         │ ┌─┐ │   │
                 ┌┘   └─┘ └─┘                                         └─┘ └─┘   └┐
                 │ ┌─┐                                                       ┌─┐ │
                 └─┘ │                                                       │ └─┘
                    ┌┘                                                       └┐
                    │                                                         │
                    └┐                                                       ┌┘
                 ┌─┐ │                                                       │ ┌─┐
                 │ └─┘                                                       └─┘ │
                 └┐   ┌─┐ ┌─┐                                         ┌─┐ ┌─┐   ┌┘
                  │   │ └─┘ │                                         │ └─┘ │   │
                 ┌┘   └┐   ┌┘                                         └┐   ┌┘   └┐
                 │ ┌─┐ │   │                                           │   │ ┌─┐ │
                 └─┘ └─┘   └┐                                         ┌┘   └─┘ └─┘
┌─┐ ┌─┐   ┌─┐ ┌─┐       ┌─┐ │                                         │ ┌─┐       ┌─┐ ┌─┐   ┌─┐ ┌─┐
│ └─┘ │   │ └─┘ │       │ └─┘                                         └─┘ │       │ └─┘ │   │ └─┘ │
└┐   ┌┘   └┐   ┌┘       └┐                                               ┌┘       └┐   ┌┘   └┐   ┌┘
 │   │ ┌─┐ │   │         │                                               │         │   │ ┌─┐ │   │
┌┘   └─┘ └─┘   └┐       ┌┘                                               └┐       ┌┘   └─┘ └─┘   └┐
│ ┌─┐       ┌─┐ │       │ ┌─┐                                         ┌─┐ │       │ ┌─┐       ┌─┐ │
└─┘ │       │ └─┘       └─┘ │                                         │ └─┘       └─┘ │       │ └─┘
   ┌┘       └┐   ┌─┐ ┌─┐   ┌┘                                         └┐   ┌─┐ ┌─┐   ┌┘       └┐
   │         │   │ └─┘ │   │                                           │   │ └─┘ │   │         │
   └┐       ┌┘   └┐   ┌┘   └┐                                         ┌┘   └┐   ┌┘   └┐       ┌┘
┌─┐ │       │ ┌─┐ │   │ ┌─┐ │                                         │ ┌─┐ │   │ ┌─┐ │       │ ┌─┐
. └─┘       └─┘ └─┘   └─┘ └─┘                                         └─┘ └─┘   └─┘ └─┘       └─┘ └∙

```

</b>
The '''output''' of this REXX program for this Rosetta Code task requirements can be seen here   ───►   [[Fibonacci word/fractal/FIBFRACT.REX]].





## Racket


Prime candidate for Turtle Graphics.
I've used a '''values-turtle''', which means you don't get the joy of seeing the turltle
bimble around the screen. But it allows the size of the image to be set (useful if you
want to push the <sub>n</sub> much higher than 23 or so!

We use '''word-order''' 23, which gives a classic n shape (inverted horseshoe).

Save the (first) implementation of [[Fibonacci word]] to '''Fibonacci-word.rkt'''; since
we do not ''generate'' the words here.


```racket
#lang racket
(require "Fibonacci-word.rkt")
(require graphics/value-turtles)

(define word-order 23) ; is a 3k+2 fractal, shaped like an n
(define height 420)
(define width 600)

(define the-word
  (parameterize ((f-word-max-length #f))
    (F-Word word-order)))

(for/fold ((T (turtles width height
                       0 height ; in BL corner
                       (/ pi -2)))) ; point north
  ((i (in-naturals))
   (j (in-string (f-word-str the-word))))
  (match* (i j)
    ((_ #\1) (draw 1 T))
    (((? even?) #\0) (turn -90 (draw 1 T)))
    ((_ #\0) (turn 90 (draw 1 T)))))
```



## Ruby


```ruby
def fibonacci_word(n)
  words = ["1", "0"]
  (n-1).times{ words << words[-1] + words[-2] }
  words[n]
end

def print_fractal(word)
  area = Hash.new(" ")
  x = y = 0
  dx, dy = 0, -1
  area[[x,y]] = "S"
  word.each_char.with_index(1) do |c,n|
    area[[x+dx, y+dy]] = dx.zero? ? "|" : "-"
    x, y = x+2*dx, y+2*dy
    area[[x, y]] = "+"
    dx,dy = n.even? ? [dy,-dx] : [-dy,dx]  if c=="0"
  end
  
  (xmin, xmax), (ymin, ymax) = area.keys.transpose.map(&:minmax)
  for y in ymin..ymax
    puts (xmin..xmax).map{|x| area[[x,y]]}.join
  end
end

word = fibonacci_word(16)
print_fractal(word)
```


{{out}}
<pre style="height: 190ex; font-size: 50%;">
+-+-+   +-+-+       +-+-+   +-+-+               +-+-+   +-+-+       +-+-+   +-+-+                                   +-+-+   +-+-+       +-+-+   +-+-+               +-+-+   +-+-+       +-+-+   +-+-+  
|   |   |   |       |   |   |   |               |   |   |   |       |   |   |   |                                   |   |   |   |       |   |   |   |               |   |   |   |       |   |   |   |  
+   +-+-+   +       +   +-+-+   +               +   +-+-+   +       +   +-+-+   +                                   +   +-+-+   +       +   +-+-+   +               +   +-+-+   +       +   +-+-+   +  
|           |       |           |               |           |       |           |                                   |           |       |           |               |           |       |           |  
+-+       +-+       +-+       +-+               +-+       +-+       +-+       +-+                                   +-+       +-+       +-+       +-+               +-+       +-+       +-+       +-+  
  |       |           |       |                   |       |           |       |                                       |       |           |       |                   |       |           |       |    
  +       +   +-+-+   +       +                   +       +   +-+-+   +       +                                       +       +   +-+-+   +       +                   +       +   +-+-+   +       +    
  |       |   |   |   |       |                   |       |   |   |   |       |                                       |       |   |   |   |       |                   |       |   |   |   |       |    
+-+       +-+-+   +-+-+       +-+               +-+       +-+-+   +-+-+       +-+                                   +-+       +-+-+   +-+-+       +-+               +-+       +-+-+   +-+-+       +-+  
|                               |               |                               |                                   |                               |               |                               |  
+   +-+-+               +-+-+   +               +   +-+-+               +-+-+   +                                   +   +-+-+               +-+-+   +               +   +-+-+               +-+-+   +  
|   |   |               |   |   |               |   |   |               |   |   |                                   |   |   |               |   |   |               |   |   |               |   |   |  
+-+-+   +               +   +-+-+               +-+-+   +               +   +-+-+                                   +-+-+   +               +   +-+-+               +-+-+   +               +   +-+-+  
        |               |                               |               |                                                   |               |                               |               |          
      +-+               +-+       +-+-+   +-+-+       +-+               +-+                                               +-+               +-+       +-+-+   +-+-+       +-+               +-+        
      |                   |       |   |   |   |       |                   |                                               |                   |       |   |   |   |       |                   |        
      +                   +       +   +-+-+   +       +                   +                                               +                   +       +   +-+-+   +       +                   +        
      |                   |       |           |       |                   |                                               |                   |       |           |       |                   |        
      +-+               +-+       +-+       +-+       +-+               +-+                                               +-+               +-+       +-+       +-+       +-+               +-+        
        |               |           |       |           |               |                                                   |               |           |       |           |               |          
+-+-+   +               +   +-+-+   +       +   +-+-+   +               +   +-+-+                                   +-+-+   +               +   +-+-+   +       +   +-+-+   +               +   +-+-+  
|   |   |               |   |   |   |       |   |   |   |               |   |   |                                   |   |   |               |   |   |   |       |   |   |   |               |   |   |  
+   +-+-+               +-+-+   +-+-+       +-+-+   +-+-+               +-+-+   +                                   +   +-+-+               +-+-+   +-+-+       +-+-+   +-+-+               +-+-+   +  
|                                                                               |                                   |                                                                               |  
+-+       +-+-+   +-+-+                                   +-+-+   +-+-+       +-+                                   +-+       +-+-+   +-+-+                                   +-+-+   +-+-+       +-+  
  |       |   |   |   |                                   |   |   |   |       |                                       |       |   |   |   |                                   |   |   |   |       |    
  +       +   +-+-+   +                                   +   +-+-+   +       +                                       +       +   +-+-+   +                                   +   +-+-+   +       +    
  |       |           |                                   |           |       |                                       |       |           |                                   |           |       |    
+-+       +-+       +-+                                   +-+       +-+       +-+                                   +-+       +-+       +-+                                   +-+       +-+       +-+  
|           |       |                                       |       |           |                                   |           |       |                                       |       |           |  
+   +-+-+   +       +                                       +       +   +-+-+   +                                   +   +-+-+   +       +                                       +       +   +-+-+   +  
|   |   |   |       |                                       |       |   |   |   |                                   |   |   |   |       |                                       |       |   |   |   |  
+-+-+   +-+-+       +-+                                   +-+       +-+-+   +-+-+                                   +-+-+   +-+-+       +-+                                   +-+       +-+-+   +-+-+  
                      |                                   |                                                                               |                                   |                        
              +-+-+   +                                   +   +-+-+               +-+-+   +-+-+       +-+-+   +-+-+               +-+-+   +                                   +   +-+-+                
              |   |   |                                   |   |   |               |   |   |   |       |   |   |   |               |   |   |                                   |   |   |                
              +   +-+-+                                   +-+-+   +               +   +-+-+   +       +   +-+-+   +               +   +-+-+                                   +-+-+   +                
              |                                                   |               |           |       |           |               |                                                   |                
              +-+                                               +-+               +-+       +-+       +-+       +-+               +-+                                               +-+                
                |                                               |                   |       |           |       |                   |                                               |                  
                +                                               +                   +       +   +-+-+   +       +                   +                                               +                  
                |                                               |                   |       |   |   |   |       |                   |                                               |                  
              +-+                                               +-+               +-+       +-+-+   +-+-+       +-+               +-+                                               +-+                
              |                                                   |               |                               |               |                                                   |                
              +   +-+-+                                   +-+-+   +               +   +-+-+               +-+-+   +               +   +-+-+                                   +-+-+   +                
              |   |   |                                   |   |   |               |   |   |               |   |   |               |   |   |                                   |   |   |                
              +-+-+   +                                   +   +-+-+               +-+-+   +               +   +-+-+               +-+-+   +                                   +   +-+-+                
                      |                                   |                               |               |                               |                                   |                        
+-+-+   +-+-+       +-+                                   +-+       +-+-+   +-+-+       +-+               +-+       +-+-+   +-+-+       +-+                                   +-+       +-+-+   +-+-+  
|   |   |   |       |                                       |       |   |   |   |       |                   |       |   |   |   |       |                                       |       |   |   |   |  
+   +-+-+   +       +                                       +       +   +-+-+   +       +                   +       +   +-+-+   +       +                                       +       +   +-+-+   +  
|           |       |                                       |       |           |       |                   |       |           |       |                                       |       |           |  
+-+       +-+       +-+                                   +-+       +-+       +-+       +-+               +-+       +-+       +-+       +-+                                   +-+       +-+       +-+  
  |       |           |                                   |           |       |           |               |           |       |           |                                   |           |       |    
  +       +   +-+-+   +                                   +   +-+-+   +       +   +-+-+   +               +   +-+-+   +       +   +-+-+   +                                   +   +-+-+   +       +    
  |       |   |   |   |                                   |   |   |   |       |   |   |   |               |   |   |   |       |   |   |   |                                   |   |   |   |       |    
+-+       +-+-+   +-+-+                                   +-+-+   +-+-+       +-+-+   +-+-+               +-+-+   +-+-+       +-+-+   +-+-+                                   +-+-+   +-+-+       +-+  
|                                                                                                                                                                                                   |  
+   +-+-+               +-+-+   +-+-+       +-+-+   +-+-+                                                                                   +-+-+   +-+-+       +-+-+   +-+-+               +-+-+   +  
|   |   |               |   |   |   |       |   |   |   |                                                                                   |   |   |   |       |   |   |   |               |   |   |  
+-+-+   +               +   +-+-+   +       +   +-+-+   +                                                                                   +   +-+-+   +       +   +-+-+   +               +   +-+-+  
        |               |           |       |           |                                                                                   |           |       |           |               |          
      +-+               +-+       +-+       +-+       +-+                                                                                   +-+       +-+       +-+       +-+               +-+        
      |                   |       |           |       |                                                                                       |       |           |       |                   |        
      +                   +       +   +-+-+   +       +                                                                                       +       +   +-+-+   +       +                   +        
      |                   |       |   |   |   |       |                                                                                       |       |   |   |   |       |                   |        
      +-+               +-+       +-+-+   +-+-+       +-+                                                                                   +-+       +-+-+   +-+-+       +-+               +-+        
        |               |                               |                                                                                   |                               |               |          
+-+-+   +               +   +-+-+               +-+-+   +                                                                                   +   +-+-+               +-+-+   +               +   +-+-+  
|   |   |               |   |   |               |   |   |                                                                                   |   |   |               |   |   |               |   |   |  
+   +-+-+               +-+-+   +               +   +-+-+                                                                                   +-+-+   +               +   +-+-+               +-+-+   +  
|                               |               |                                                                                                   |               |                               |  
+-+       +-+-+   +-+-+       +-+               +-+                                                                                               +-+               +-+       +-+-+   +-+-+       +-+  
  |       |   |   |   |       |                   |                                                                                               |                   |       |   |   |   |       |    
  +       +   +-+-+   +       +                   +                                                                                               +                   +       +   +-+-+   +       +    
  |       |           |       |                   |                                                                                               |                   |       |           |       |    
+-+       +-+       +-+       +-+               +-+                                                                                               +-+               +-+       +-+       +-+       +-+  
|           |       |           |               |                                                                                                   |               |           |       |           |  
+   +-+-+   +       +   +-+-+   +               +   +-+-+                                                                                   +-+-+   +               +   +-+-+   +       +   +-+-+   +  
|   |   |   |       |   |   |   |               |   |   |                                                                                   |   |   |               |   |   |   |       |   |   |   |  
+-+-+   +-+-+       +-+-+   +-+-+               +-+-+   +                                                                                   +   +-+-+               +-+-+   +-+-+       +-+-+   +-+-+  
                                                        |                                                                                   |                                                          
                                  +-+-+   +-+-+       +-+                                                                                   +-+       +-+-+   +-+-+                                    
                                  |   |   |   |       |                                                                                       |       |   |   |   |                                    
                                  +   +-+-+   +       +                                                                                       +       +   +-+-+   +                                    
                                  |           |       |                                                                                       |       |           |                                    
                                  +-+       +-+       +-+                                                                                   +-+       +-+       +-+                                    
                                    |       |           |                                                                                   |           |       |                                      
                                    +       +   +-+-+   +                                                                                   +   +-+-+   +       +                                      
                                    |       |   |   |   |                                                                                   |   |   |   |       |                                      
                                  +-+       +-+-+   +-+-+                                                                                   +-+-+   +-+-+       +-+                                    
                                  |                                                                                                                               |                                    
                                  +   +-+-+                                                                                                               +-+-+   +                                    
                                  |   |   |                                                                                                               |   |   |                                    
                                  +-+-+   +                                                                                                               +   +-+-+                                    
                                          |                                                                                                               |                                            
                                        +-+                                                                                                               +-+                                          
                                        |                                                                                                                   |                                          
                                        +                                                                                                                   +                                          
                                        |                                                                                                                   |                                          
                                        +-+                                                                                                               +-+                                          
                                          |                                                                                                               |                                            
                                  +-+-+   +                                                                                                               +   +-+-+                                    
                                  |   |   |                                                                                                               |   |   |                                    
                                  +   +-+-+                                                                                                               +-+-+   +                                    
                                  |                                                                                                                               |                                    
                                  +-+       +-+-+   +-+-+                                                                                   +-+-+   +-+-+       +-+                                    
                                    |       |   |   |   |                                                                                   |   |   |   |       |                                      
                                    +       +   +-+-+   +                                                                                   +   +-+-+   +       +                                      
                                    |       |           |                                                                                   |           |       |                                      
                                  +-+       +-+       +-+                                                                                   +-+       +-+       +-+                                    
                                  |           |       |                                                                                       |       |           |                                    
                                  +   +-+-+   +       +                                                                                       +       +   +-+-+   +                                    
                                  |   |   |   |       |                                                                                       |       |   |   |   |                                    
                                  +-+-+   +-+-+       +-+                                                                                   +-+       +-+-+   +-+-+                                    
                                                        |                                                                                   |                                                          
+-+-+   +-+-+       +-+-+   +-+-+               +-+-+   +                                                                                   +   +-+-+               +-+-+   +-+-+       +-+-+   +-+-+  
|   |   |   |       |   |   |   |               |   |   |                                                                                   |   |   |               |   |   |   |       |   |   |   |  
+   +-+-+   +       +   +-+-+   +               +   +-+-+                                                                                   +-+-+   +               +   +-+-+   +       +   +-+-+   +  
|           |       |           |               |                                                                                                   |               |           |       |           |  
+-+       +-+       +-+       +-+               +-+                                                                                               +-+               +-+       +-+       +-+       +-+  
  |       |           |       |                   |                                                                                               |                   |       |           |       |    
  +       +   +-+-+   +       +                   +                                                                                               +                   +       +   +-+-+   +       +    
  |       |   |   |   |       |                   |                                                                                               |                   |       |   |   |   |       |    
+-+       +-+-+   +-+-+       +-+               +-+                                                                                               +-+               +-+       +-+-+   +-+-+       +-+  
|                               |               |                                                                                                   |               |                               |  
+   +-+-+               +-+-+   +               +   +-+-+                                                                                   +-+-+   +               +   +-+-+               +-+-+   +  
|   |   |               |   |   |               |   |   |                                                                                   |   |   |               |   |   |               |   |   |  
+-+-+   +               +   +-+-+               +-+-+   +                                                                                   +   +-+-+               +-+-+   +               +   +-+-+  
        |               |                               |                                                                                   |                               |               |          
      +-+               +-+       +-+-+   +-+-+       +-+                                                                                   +-+       +-+-+   +-+-+       +-+               +-+        
      |                   |       |   |   |   |       |                                                                                       |       |   |   |   |       |                   |        
      +                   +       +   +-+-+   +       +                                                                                       +       +   +-+-+   +       +                   +        
      |                   |       |           |       |                                                                                       |       |           |       |                   |        
      +-+               +-+       +-+       +-+       +-+                                                                                   +-+       +-+       +-+       +-+               +-+        
        |               |           |       |           |                                                                                   |           |       |           |               |          
+-+-+   +               +   +-+-+   +       +   +-+-+   +                                                                                   +   +-+-+   +       +   +-+-+   +               +   +-+-+  
|   |   |               |   |   |   |       |   |   |   |                                                                                   |   |   |   |       |   |   |   |               |   |   |  
S   +-+-+               +-+-+   +-+-+       +-+-+   +-+-+                                                                                   +-+-+   +-+-+       +-+-+   +-+-+               +-+-+   +-+

```



## Scala

'''Note:''' will be computing an SVG image - not very efficient, but very cool. worked for me in the scala REPL with ''-J-Xmx2g'' argument.

```scala

def fibIt = Iterator.iterate(("1","0")){case (f1,f2) => (f2,f1+f2)}.map(_._1)

def turnLeft(c: Char): Char = c match {
  case 'R' => 'U'
  case 'U' => 'L'
  case 'L' => 'D'
  case 'D' => 'R'
}

def turnRight(c: Char): Char = c match {
  case 'R' => 'D'
  case 'D' => 'L'
  case 'L' => 'U'
  case 'U' => 'R'
}

def directions(xss: List[(Char,Char)], current: Char = 'R'): List[Char] = xss match {
  case Nil => current :: Nil
  case x :: xs => x._1 match {
    case '1' => current :: directions(xs, current)
    case '0' => x._2 match {
      case 'E' => current :: directions(xs, turnLeft(current))
      case 'O' => current :: directions(xs, turnRight(current))
    }
  }
}
     
def buildIt(xss: List[Char], old: Char = 'X', count: Int = 1): List[String] = xss match {
  case Nil => s"$old$count" :: Nil
  case x :: xs if x == old => buildIt(xs,old,count+1)
  case x :: xs => s"$old$count" :: buildIt(xs,x)
}
     
def convertToLine(s: String, c: Int): String = (s.head, s.tail) match {
  case ('R',n) => s"l ${c * n.toInt} 0"
  case ('U',n) => s"l 0 ${-c * n.toInt}"
  case ('L',n) => s"l ${-c * n.toInt} 0"
  case ('D',n) => s"l 0 ${c * n.toInt}"
}

def drawSVG(xStart: Int, yStart: Int, width: Int, height: Int, fibWord: String, lineMultiplier: Int, color: String): String = {
  val xs = fibWord.zipWithIndex.map{case (c,i) => (c, if(c == '1') '_' else i % 2 match{case 0 => 'E'; case 1 => 'O'})}.toList
  val fractalPath = buildIt(directions(xs)).tail.map(convertToLine(_,lineMultiplier))
  s"""<?xml version="1.0" encoding="utf-8"?><!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"><svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" width="${width}px" height="${height}px" viewBox="0 0 $width $height"><path d="M $xStart $yStart ${fractalPath.mkString(" ")}" style="stroke:#$color;stroke-width:1" stroke-linejoin="miter" fill="none"/></svg>"""
}

drawSVG(0,25,550,530,fibIt.drop(18).next,3,"000")

```


{{out}}

[https://www.dropbox.com/s/vb4cu4fvq2f6cvz/fibfract.svg?dl=0 output string saved as an SVG file] - BTW, would appreciate help on getting the image to display here nicely. couldn't figure out how to do that...


## Scilab

This script uses Scilab's [[Fibonacci_word#Iterative_method|iterative solution]] to generate Fibonacci words, and the interpreting the words to generate the fractal is similar to [[Langton's_ant#Scilab|Langton's ant]]. The result is displayed in a graphic window.

<lang>final_length = 37;

word_n = '';
word_n_1 = '';
word_n_2 = '';

for i = 1:final_length
    if i == 1 then
        word_n = '1';
    elseif i == 2
        word_n = '0';
    elseif i == 3
        word_n = '01';
        word_n_1 = '0';
    else
        word_n_2 = word_n_1;
        word_n_1 = word_n;
        word_n = word_n_1 + word_n_2;
    end
end

word = strsplit(word_n);
fractal_size = sum(word' == '0');
fractal = zeros(1+fractal_size,2);

direction_vectors = [1,0; 0,-1; -1,0; 0,1];
direction = direction_vectors(4,:);
direction_name = 'N';

for j = 1:length(word_n);
    fractal(j+1,:) = fractal(j,:) + direction;
    if word(j) == '0' then
        if pmodulo(j,2) then
            //right
            select direction_name
            case 'N' then
                direction = direction_vectors(1,:);
                direction_name = 'E';
            case 'E' then
                direction = direction_vectors(2,:);
                direction_name = 'S';
            case 'S' then
                direction = direction_vectors(3,:);
                direction_name = 'W';
            case 'W' then
                direction = direction_vectors(4,:);
                direction_name = 'N';
            end
        else
            //left
            select direction_name
            case 'N' then
                direction = direction_vectors(3,:);
                direction_name = 'W';
            case 'W' then
                direction = direction_vectors(2,:);
                direction_name = 'S';
            case 'S' then
                direction = direction_vectors(1,:);
                direction_name = 'E';
            case 'E' then
                direction = direction_vectors(4,:);
                direction_name = 'N';
            end
        end
    end
end

scf(0); clf();
plot2d(fractal(:,1),fractal(:,2));
set(gca(),'isoview','on');
```



## Sidef

{{trans|Perl 6}}

```ruby
var(m=17, scale=3) = ARGV.map{.to_i}...

(var world = Hash.new){0}{0} = 1
var loc = 0
var dir = 1i

var fib = ['1', '0']
func fib_word(n) {
    fib[n] \\= (fib_word(n-1) + fib_word(n-2))
}

func step {
    scale.times {
        loc += dir
        world{loc.im}{loc.re} = 1
    }
}

func turn_left  { dir *=  1i }
func turn_right { dir *= -1i }

var n = 1
fib_word(m).each { |c|
    if (c == '0') {
        step()
        n % 2 == 0 ? turn_left()
                   : turn_right()
    } else { n++ }
}

func braille_graphics(a) {
    var (xlo, xhi, ylo, yhi) = ([Inf, -Inf]*2)...

    a.each_key { |y|
        ylo.min!(y.to_i)
        yhi.max!(y.to_i)
        a{y}.each_key { |x|
            xlo.min!(x.to_i)
            xhi.max!(x.to_i)
        }
    }

    for y in (ylo..yhi `by` 4) {
        for x in (xlo..xhi `by` 2) {
            var cell = 0x2800

            a{y+0}{x+0} && (cell += 1)
            a{y+1}{x+0} && (cell += 2)
            a{y+2}{x+0} && (cell += 4)
            a{y+0}{x+1} && (cell += 8)
            a{y+1}{x+1} && (cell += 16)
            a{y+2}{x+1} && (cell += 32)
            a{y+3}{x+0} && (cell += 64)
            a{y+3}{x+1} && (cell += 128)

            print cell.chr
        }
        print "\n"
    }
}

braille_graphics(world)
```

{{out}}

```txt

$ sidef fib_word_fractal.sf 12 3
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣇⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⢤⠀⡤⠼⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠒⠃⠘⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⣇⣸⠉⣇⣀⠀⣀⣸⠉⣇⣀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⡖⢲⠀⠀
⠀⠀⠀⠀⢀⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢈⣉⡁⠀⠀⠀⢈⣉⡁⢈⣉⡇
⠀⠀⠀⡤⠼⠀⠧⢤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠼⠀⠧⢤⠀⡤⠼⠀⠧⠼⠀⠀
⠀⠀⠀⠓⢲⠀⡖⠚⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠓⢲⠀⡖⠚⠀⠓⢲⠀⠀⠀⠀⠀
⠉⢹⣀⡏⠉⠀⠉⢹⣀⡏⢹⣀⡀⢀⣀⡏⢹⣀⡏⠉⠀⠉⢹⣀⡏⠉⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⢠⠤⡄⢠⠤⠇⠸⠤⡄⢠⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⡖⠚⠀⠓⠚⠀⠀⠀⠀⠓⠚⠀⠓⢲⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠉⢹⣀⡏⢹⣀⡀⢀⣀⡏⢹⣀⡏⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⠤⠇⠸⠤⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⠒⡆⢰⠒⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀

```



## Tcl

{{libheader|Tk}}
<!-- I've tested this up to F_37; it required a *lot* of memory (good thing I'm using a 64-bit build…) -->

```tcl
package require Tk

# OK, this stripped down version doesn't work for n<2…
proc fibword {n} {
    set fw {1 0}
    while {[llength $fw] < $n} {
	lappend fw [lindex $fw end][lindex $fw end-1]
    }
    return [lindex $fw end]
}
proc drawFW {canv fw {w {[$canv cget -width]}} {h {[$canv cget -height]}}} {
    set w [subst $w]
    set h [subst $h]

    # Generate the coordinate list using line segments of unit length
    set d 3; # Match the orientation in the sample paper
    set eo [set x [set y 0]]
    set coords [list $x $y]
    foreach c [split $fw ""] {
	switch $d {
	    0 {lappend coords [incr x] $y}
	    1 {lappend coords $x [incr y]}
	    2 {lappend coords [incr x -1] $y}
	    3 {lappend coords $x [incr y -1]}
	}
	if {$c == 0} {
	    set d [expr {($d + ($eo ? -1 : 1)) % 4}]
	}
	set eo [expr {!$eo}]
    }

    # Draw, and rescale to fit in canvas
    set id [$canv create line $coords]
    lassign [$canv bbox $id] x1 y1 x2 y2
    set sf [expr {min(($w-20.) / ($y2-$y1), ($h-20.) / ($x2-$x1))}]
    $canv move $id [expr {-$x1}] [expr {-$y1}]
    $canv scale $id 0 0 $sf $sf
    $canv move $id 10 10
    # Return the item ID to allow user reconfiguration
    return $id
}

pack [canvas .c -width 500 -height 500]
drawFW .c [fibword 23]
```



## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
{{trans|D}}
[[File:Fibonacci word fractal.zkl.jpg|250px|thumb|right]]

```zkl
fcn drawFibonacci(img,x,y,word){ // word is "01001010...", 75025 characters
   dx:=0; dy:=1; // turtle direction
   foreach i,c in ([1..].zip(word)){ // Walker.zip(list)-->Walker of zipped list
      a:=x; b:=y; x+=dx; y+=dy;
      img.line(a,b, x,y, 0x00ff00);
      if (c=="0"){
         dxy:=dx+dy;
	 if(i.isEven){ dx=(dx - dxy)%2; dy=(dxy - dy)%2; }// turn left
	 else 	     { dx=(dxy - dx)%2; dy=(dy - dxy)%2; }// turn right
      }
   }
}

img:=PPM(1050,1050);
fibWord:=L("1","0"); do(23){ fibWord.append(fibWord[-1] + fibWord[-2]); }
drawFibonacci(img,20,20,fibWord[-1]);
img.write(File("foo.ppm","wb"));
```

