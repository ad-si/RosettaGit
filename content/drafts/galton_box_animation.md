+++
title = "Galton box animation"
description = ""
date = 2019-05-31T10:51:28Z
aliases = []
[extra]
id = 9552
[taxonomies]
categories = []
tags = []
+++

{{task|Animation}}
[[Category:Randomness]]
{{omit from|GUISS}}

Generate an animated simulation of [[wp:Bean_machine|Sir Francis Galton's device]].  
An example can be found to the right. [[File:Galtonbox-Unicon.PNG|thumb|Example of a Galton Box at the end of animation.]]

In a Galton box, there are a set of pins arranged in a triangular pattern. 
A number of balls are dropped so that they fall in line with the top pin, deflecting to the left or the right of the pin. The ball continues to fall to the left or right of subsequent pins before arriving at one of the collection points between and to the sides of the bottom row of pins. 

For the purpose of this task the box should have at least 5 pins on the bottom row.  
Your solution can use graphics or ASCII animation.  
Provide a sample of the output/display such as a screenshot.

Your solution can have either one or more balls in flight at the same time.  
If multiple balls are in flight, ensure they don't interfere with each other.

Your solution should allow users to specify the number of balls or it should run until full or a preset limit.  Optionally, display the number of balls.


## AutoHotkey

Uses an edit box for the (text based) animation

```AutoHotkey
AutoTrim Off
; User settings
bottompegs := 6
SleepTime  := 200
fallspace  := 30

; create the board
out := (pad2 := Space(bottompegs*2+1)) "`n"
Loop % bottompegs
{
	out .= Space(bottompegs-A_Index+1)
	Loop % A_Index
		out .= "* "
	out .= Space(bottompegs-A_Index+1) . "`n" 
}
StringTrimRight, strboard, out, 1 ; remove last newline
Loop % fallspace-1
	strboard .= "`n" . pad2
strboard .= "`n"
Loop % bottompegs*2+1
	strboard .= "="

; Create Gui
Gui Font, , Consolas
Gui -Caption
Gui Margin, 0, 0
Gui, Add, edit, -VScroll vE, % strboard
Gui Show
Loop
{
	ballX := bottompegs+1, BallY := 1
	strboard := ChangeChar(strboard, BallX, ballY, "O")
	GuiControl,, E, % strboard
	sleep SleepTime
	; Make ball fall and bounce
	Loop % bottompegs
	{
		strboard := ChangeChar(strboard, BallX, BallY, " ")
		ballY += 1
		ballX += RandAdd()
		; MsgBox % ballX ", " ballY
		GuiControl,, E, % strboard := ChangeChar(strboard, ballX, ballY, "O")
		sleep SleepTime
	}
	; now fall to the bottom
	While GetChar(strboard, BallX, BallY+1) = A_Space
	{
		strboard := ChangeChar(strboard, BallX, BallY, " ")
		BallY += 1
		strboard := ChangeChar(strboard, BallX, BallY, "O")
		GuiControl,, E, % strboard
		sleep SleepTime
	}
}
~Esc::
GuiClose:
ExitApp

Space(n){
	If n
		return " " Space(n-1)
	return ""
}
RandAdd(){
	Random, n, 3, 4
	return (n=3 ? -1 : 1)
}

GetChar(s, x, y){
	Loop Parse, s, `n
		if (A_Index = y)
			return SubStr(A_LoopField, x, 1)
}
ChangeChar(s, x, y, c){
	Loop Parse, s, `n
	{
		If (A_Index = y)
		{
			Loop Parse, A_LoopField
				If (A_Index = x)
					out .= c
				else    out .= A_LoopField
		}
		else out .= A_LoopField
		     out .= "`n"
	}
	StringTrimRight, out, out, 1 ; removes the last newline
	return out
}
```
While the number of pegs, and falling space are configurable, here's output shortly after starting one configuration:

```txt
            
      *       
     * *O     
    * * *     
   * * * *    
  * * * * *   
 * * * * * *  
             
             
    O        
  O O        
  O O O O    
O O O O O    

### =======

```



## BASIC256

[[File:Galton box BASIC-256.gif|right|150px|thumb|Galton box animation created with BASIC-256]]

```basic256
graphsize 150,125
fastgraphics
color black
rect 0,0,graphwidth,graphheight
refresh

N = 10		# number of balls
M = 5		# number of pins in last row
dim ball(N,5)	# (pos_x to center, level, x, y, direction}
dim cnt(M+1)

rad = 6
slow = 0.3
diamond = {0,rad,rad,0,0,-rad,-rad,0}
stepx = {rad/sqr(2),rad/2,rad/2,(1-1/sqr(2))*rad,0}
stepy = {(1-1/sqr(2))*rad,rad/2,rad/2,rad/sqr(2),rad}
CX = graphwidth/2 : CY = graphheight/2
iters = 0

# Draw pins
for i = 1 to M
	y = 3*rad*i
	for j = 1 to i 
		dx = (j-i\2-1)*4*rad + ((i-1)%2)*2*rad
		color purple
		stamp CX+dx,y,1.0,diamond
		color darkpurple
		stamp CX+dx,y,0.6,diamond
	next j
next i
gosub saverefresh

R = 0 : C = 0
font "Tahoma",10,50
do
	# Release ball
	if R<N then
		R = R + 1
		ball[R-1,2] = CX : ball[R-1,3] = rad*(1-stepx[?]) : ball[R-1,4] = 0
		# How many balls are released
		color black
		text 5,5,(R-1)+" balls"
		color green
		text 5,5,(R)+" balls"
	end if
	# Animate balls on this step
	for it = 0 to stepx[?]-1
		for b = 0 to R-1 
			gosub moveball
		next b
		gosub saverefresh
		pause slow/stepx[?]
	next it
	# Where to go on the next step?
	for b = 0 to R-1 
		ball[b,1] = ball[b,1] + 1
		if ball[b,1]<=M then
			if rand>=0.5 then 
				ball[b,4] = 1
			else
				ball[b,4] = -1
			end if
			ball[b,0] = ball[b,0] + ball[b,4]
		else
			if ball[b,4]<>0 then
				gosub eraseball
				i = (ball[b,0]+M)/2
				cnt[i] = cnt[i] + 1
				ball[b,4] = 0
				C = C + 1
			end if
		end if
	next b
	# Draw counter
	color green
	y = 3*rad*(M+1)
	for j = 0 to M
		dx = (j-(M+1)\2)*4*rad + (M%2)*2*rad
		stamp CX+dx,y,{-1.2*rad,0,1.2*rad,0,1.2*rad,2*cnt[j],-1.2*rad,2*cnt[j]}
	next j
	gosub saverefresh
until C >= N
end

moveball:
	if ball[b,1]>M then return
	gosub eraseball
	if ball[b,4]<>0.0 then
		ball[b,2] = ball[b,2]+ball[b,4]*stepx[it]
		ball[b,3] = ball[b,3]+stepy[it]
	else 
		ball[b,3] = ball[b,3]+rad
	end if
	gosub drawball

drawball:
	color darkgreen
	circle ball[b,2],ball[b,3],rad-1
	color green
	circle ball[b,2],ball[b,3],rad-2
	return 

eraseball:
	color black
	circle ball[b,2],ball[b,3],rad-1
	return

saverefresh:
	num$ = string(iters)	
	for k = 1 to 4-length(num$)
		num$ = "0"+num$ 
	next k
	imgsave num$+"-Galton_box_BASIC-256.png", "PNG"
	iters = iters + 1
	refresh
	return
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}
[[Image:quincunx_bbc.gif|right]]

```bbcbasic
      maxBalls% = 10
      DIM ballX%(maxBalls%), ballY%(maxBalls%)
      
      VDU 23,22,180;400;8,16,16,128
      ORIGIN 180,0
      OFF
      
      REM Draw the pins:
      GCOL 9
      FOR row% = 1 TO 7
        FOR col% = 1 TO row%
          CIRCLE FILL 40*col% - 20*row% - 20, 800 - 40*row%, 12
        NEXT
      NEXT row%
      
      REM Animate
      last% = 0
      tick% = 0
      GCOL 3,3
      REPEAT
        IF RND(10) = 5 IF (tick% - last%) > 10 THEN
          FOR ball% = 1 TO maxBalls%
            IF ballY%(ball%) = 0 THEN
              ballX%(ball%) = 0
              ballY%(ball%) = 800
              last% = tick%
              EXIT FOR
            ENDIF
          NEXT
        ENDIF
        FOR ball% = 1 TO maxBalls%
          IF ballY%(ball%) CIRCLE FILL ballX%(ball%), ballY%(ball%), 12
          IF POINT(ballX%(ball%),ballY%(ball%)-10) = 12 OR ballY%(ball%) < 12 THEN
            IF ballY%(ball%) > 500 END
            ballY%(ball%) = 0
          ENDIF
        NEXT
        WAIT 2
        FOR ball% = 1 TO maxBalls%
          IF ballY%(ball%) THEN
            CIRCLE FILL ballX%(ball%), ballY%(ball%), 12
            ballY%(ball%) -= 4
            IF POINT(ballX%(ball%),ballY%(ball%)-10) = 9 THEN
              ballX%(ball%) += 40 * (RND(2) - 1.5)
            ENDIF
          ENDIF
        NEXT
        tick% += 1
      UNTIL FALSE
```



## C


```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>

#define BALLS 1024
int n, w, h = 45, *x, *y, cnt = 0;
char *b;

#define B(y, x) b[(y)*w + x]
#define C(y, x) ' ' == b[(y)*w + x]
#define V(i) B(y[i], x[i])
inline int rnd(int a) { return (rand()/(RAND_MAX/a))%a; }

void show_board()
{
	int i, j;
	for (puts("\033[H"), i = 0; i < h; i++, putchar('\n'))
		for (j = 0; j < w; j++, putchar(' '))
			printf(B(i, j) == '*' ?
				C(i - 1, j) ? "\033[32m%c\033[m" :
				"\033[31m%c\033[m" : "%c", B(i, j));
}

void init()
{
	int i, j;
	puts("\033[H\033[J");
	b = malloc(w * h);
	memset(b, ' ', w * h);

	x = malloc(sizeof(int) * BALLS * 2);
	y = x + BALLS;

	for (i = 0; i < n; i++)
		for (j = -i; j <= i; j += 2)
			B(2 * i+2, j + w/2) = '*';
	srand(time(0));
}

void move(int idx)
{
	int xx = x[idx], yy = y[idx], c, kill = 0, sl = 3, o = 0;

	if (yy < 0) return;
	if (yy == h - 1) { y[idx] = -1; return; }

	switch(c = B(yy + 1, xx)) {
	case ' ':	yy++; break;
	case '*':	sl = 1;
	default:	if (xx < w - 1 && C(yy, xx + 1) && C(yy + 1, xx + 1))
				if (!rnd(sl++)) o = 1;
			if (xx && C(yy, xx - 1) && C(yy + 1, xx - 1))
				if (!rnd(sl++)) o = -1;
			if (!o) kill = 1;
			xx += o;
	}

	c = V(idx); V(idx) = ' ';
	idx[y] = yy, idx[x] = xx;
	B(yy, xx) = c;
	if (kill) idx[y] = -1;
}

int run(void)
{
	static int step = 0;
	int i;
	for (i = 0; i < cnt; i++) move(i);
	if (2 == ++step && cnt < BALLS) {
		step = 0;
		x[cnt] = w/2;
		y[cnt] = 0;
		if (V(cnt) != ' ') return 0;
		V(cnt) = rnd(80) + 43;
		cnt++;
	}
	return 1;
}

int main(int c, char **v)
{
	if (c < 2 || (n = atoi(v[1])) <= 3) n = 5;
	if (n >= 20) n = 20;
	w = n * 2 + 1;
	init();

	do { show_board(), usleep(60000); } while (run());

	return 0;
}
```

Sample out put at begining of a run:
```txt

          *

        *   *

      *   *   *

    *   *   *   *

  *   *   *   *   *

```



## C++

Windows GDI version.

```cpp

#include "stdafx.h"
#include <windows.h>
#include <stdlib.h>

const int BMP_WID = 410, BMP_HEI = 230, MAX_BALLS = 120;

class myBitmap {
public:
    myBitmap() : pen( NULL ), brush( NULL ), clr( 0 ), wid( 1 ) {}
    ~myBitmap() {
        DeleteObject( pen ); DeleteObject( brush );
        DeleteDC( hdc ); DeleteObject( bmp );
    }
    bool create( int w, int h ) {
        BITMAPINFO bi;
        ZeroMemory( &bi, sizeof( bi ) );
        bi.bmiHeader.biSize        = sizeof( bi.bmiHeader );
        bi.bmiHeader.biBitCount    = sizeof( DWORD ) * 8;
        bi.bmiHeader.biCompression = BI_RGB;
        bi.bmiHeader.biPlanes      = 1;
        bi.bmiHeader.biWidth       =  w;
        bi.bmiHeader.biHeight      = -h;

        HDC dc = GetDC( GetConsoleWindow() );
        bmp = CreateDIBSection( dc, &bi, DIB_RGB_COLORS, &pBits, NULL, 0 );
        if( !bmp ) return false;
        hdc = CreateCompatibleDC( dc );
        SelectObject( hdc, bmp );
        ReleaseDC( GetConsoleWindow(), dc );
        width = w; height = h;
        return true;
    }
    void clear( BYTE clr = 0 ) {
        memset( pBits, clr, width * height * sizeof( DWORD ) );
    }
    void setBrushColor( DWORD bClr ) {
        if( brush ) DeleteObject( brush );
        brush = CreateSolidBrush( bClr );
        SelectObject( hdc, brush );
    }
    void setPenColor( DWORD c ) {
        clr = c; createPen();
    }
    void setPenWidth( int w ) {
        wid = w; createPen();
    }
    HDC getDC() const     { return hdc; }
    int getWidth() const  { return width; }
    int getHeight() const { return height; }
private:
    void createPen() {
        if( pen ) DeleteObject( pen );
        pen = CreatePen( PS_SOLID, wid, clr );
        SelectObject( hdc, pen );
    }
    HBITMAP bmp;
    HDC     hdc;
    HPEN    pen;
    HBRUSH  brush;
    void    *pBits;
    int     width, height, wid;
    DWORD   clr;
};
class point {
public:
    int x; float y;
    void set( int a, float b ) { x = a; y = b; }
};
typedef struct {
    point position, offset;
    bool alive, start;
}ball;
class galton {
public :
    galton() {
        bmp.create( BMP_WID, BMP_HEI );
        initialize();
    }
    void setHWND( HWND hwnd ) { _hwnd = hwnd; }
    void simulate() {
        draw(); update(); Sleep( 1 );
    }
private:
    void draw() {
        bmp.clear();
        bmp.setPenColor( RGB( 0, 255, 0 ) );
        bmp.setBrushColor( RGB( 0, 255, 0 ) );
        int xx, yy;
        for( int y = 3; y < 14; y++ ) {
            yy = 10 * y;
            for( int x = 0; x < 41; x++ ) {
                xx = 10 * x;
                if( pins[y][x] )
                    Rectangle( bmp.getDC(), xx - 3, yy - 3, xx + 3, yy + 3 );
            }
        }
        bmp.setPenColor( RGB( 255, 0, 0 ) );
        bmp.setBrushColor( RGB( 255, 0, 0 ) );
        ball* b; 
        for( int x = 0; x < MAX_BALLS; x++ ) {
            b = &balls[x];
            if( b->alive )
                Rectangle( bmp.getDC(), static_cast<int>( b->position.x - 3 ), static_cast<int>( b->position.y - 3 ), 
                                        static_cast<int>( b->position.x + 3 ), static_cast<int>( b->position.y + 3 ) );
        }
        for( int x = 0; x < 70; x++ ) {
            if( cols[x] > 0 ) {
                xx = 10 * x;
                Rectangle( bmp.getDC(), xx - 3, 160, xx + 3, 160 + cols[x] );
            }
        }
        HDC dc = GetDC( _hwnd );
        BitBlt( dc, 0, 0, BMP_WID, BMP_HEI, bmp.getDC(), 0, 0, SRCCOPY );
        ReleaseDC( _hwnd, dc );
    }
    void update() {
        ball* b;
        for( int x = 0; x < MAX_BALLS; x++ ) {
            b = &balls[x];
            if( b->alive ) {
                b->position.x += b->offset.x; b->position.y += b->offset.y;
                if( x < MAX_BALLS - 1 && !b->start && b->position.y > 50.0f ) {
                    b->start = true;
                    balls[x + 1].alive = true;
                }
                int c = ( int )b->position.x, d = ( int )b->position.y + 6;
                if( d > 10 || d < 41 ) {
                    if( pins[d / 10][c / 10] ) {
                        if( rand() % 30 < 15 ) b->position.x -= 10;
                        else b->position.x += 10;
                    }
                }
                if( b->position.y > 160 ) {
                    b->alive = false;
                    cols[c / 10] += 1;
                }
            }
        }
    }
    void initialize() {
        for( int x = 0; x < MAX_BALLS; x++ ) {
            balls[x].position.set( 200, -10 );
            balls[x].offset.set( 0, 0.5f );
            balls[x].alive = balls[x].start = false;
        }
        balls[0].alive = true;
        for( int x = 0; x < 70; x++ )
            cols[x] = 0;
        for( int y = 0; y < 70; y++ )
            for( int x = 0; x < 41; x++ )
                pins[x][y] = false;
        int p;
        for( int y = 0; y < 11; y++ ) {
            p = ( 41 / 2 ) - y;
            for( int z = 0; z < y + 1; z++ ) {
                pins[3 + y][p] = true;
                p += 2;
            }
        }
    }
    myBitmap bmp;
    HWND _hwnd;
    bool pins[70][40];
    ball balls[MAX_BALLS];
    int cols[70];
};
class wnd {
public:
    int wnd::Run( HINSTANCE hInst ) {
        _hInst = hInst;
        _hwnd = InitAll();
        _gtn.setHWND( _hwnd );
        ShowWindow( _hwnd, SW_SHOW );
        UpdateWindow( _hwnd );
        MSG msg;
        ZeroMemory( &msg, sizeof( msg ) );
        while( msg.message != WM_QUIT ) {
            if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) != 0 ) {
                TranslateMessage( &msg );
                DispatchMessage( &msg );
            } else _gtn.simulate();
        }
        return UnregisterClass( "_GALTON_", _hInst );
    }
private:
    static int WINAPI wnd::WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam ) {
        switch( msg ) {
            case WM_DESTROY: PostQuitMessage( 0 ); break;
            default:
                return static_cast<int>( DefWindowProc( hWnd, msg, wParam, lParam ) );
        }
        return 0;
    }
    HWND InitAll() {
        WNDCLASSEX wcex;
        ZeroMemory( &wcex, sizeof( wcex ) );
        wcex.cbSize           = sizeof( WNDCLASSEX );
        wcex.style           = CS_HREDRAW | CS_VREDRAW;
        wcex.lpfnWndProc   = ( WNDPROC )WndProc;
        wcex.hInstance     = _hInst;
        wcex.hCursor       = LoadCursor( NULL, IDC_ARROW );
        wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
        wcex.lpszClassName = "_GALTON_";
        RegisterClassEx( &wcex );
        RECT rc;
        SetRect( &rc, 0, 0, BMP_WID, BMP_HEI );
        AdjustWindowRect( &rc, WS_CAPTION, FALSE );
        return CreateWindow( "_GALTON_", ".: Galton Box -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, rc.right - rc.left, rc.bottom - rc.top, NULL, NULL, _hInst, NULL );
    }
    HINSTANCE _hInst;
    HWND      _hwnd;
    galton    _gtn;
};
int APIENTRY WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow ) {
    srand( GetTickCount() );
    wnd myWnd; 
    return myWnd.Run( hInstance );
}

```



## D

To keep the code simpler some corner cases are ignored.

```d
import std.stdio, std.algorithm, std.random, std.array;

enum int boxW = 41, boxH = 37; // Galton box width and height.
enum int pinsBaseW = 19;       // Pins triangle base size.
enum int nMaxBalls = 55;       // Number of balls.

static assert(boxW >= 2 && boxH >= 2);
static assert((boxW - 4) >= (pinsBaseW * 2 - 1));
static assert((boxH - 3) >= pinsBaseW);
enum centerH = pinsBaseW + (boxW - (pinsBaseW * 2 - 1)) / 2 - 1;

enum Cell : char { empty  = ' ',
                   ball   = 'o',
                   wall   = '|',
                   corner = '+',
                   floor  = '-',
                   pin    = '.' }

Cell[boxW][boxH] box; // Galton box. Will be printed upside-down.

struct Ball {
    int x, y; // Position.

    this(in int x_, in int y_) nothrow @safe @nogc
    in {
        assert(box[y_][x_] == Cell.empty);
    } body {
        this.x = x_;
        this.y = y_;
        box[y][x] = Cell.ball;
    }

    nothrow const @safe @nogc invariant {
        assert(x >= 0 && x < boxW && y >= 0 && y < boxH);
        assert(box[y][x] == Cell.ball);
    }

    void doStep() {
        if (y <= 0)
            return; // Reached the bottom of the box.

        final switch (box[y - 1][x]) with (Cell) {
            case empty:
                box[y][x] = Cell.empty;
                y--;
                box[y][x] = Cell.ball;
                break;
            case ball, wall, corner, floor:
                // It's frozen. (It always piles on other balls).
                break;
            case pin:
                box[y][x] = Cell.empty;
                y--;
                if (box[y][x - 1] == Cell.empty && box[y][x + 1] == Cell.empty) {
                    x += uniform(0, 2) * 2 - 1;
                    box[y][x] = Cell.ball;
                    return;
                } else if (box[y][x - 1] == Cell.empty) {
                    x++;
                } else {
                    x--;
                }
                box[y][x] = Cell.ball;
                break;
        }
    }
}

void initializeBox() {
    // Set ceiling and floor:
    box[0][] = Cell.corner ~ [Cell.floor].replicate(boxW - 2) ~ Cell.corner;
    box[$ - 1][] = box[0][];

    // Set walls:
    foreach (immutable r; 1 .. boxH - 1)
        box[r][0] = box[r][$ - 1] = Cell.wall;

    // Set pins:
    foreach (immutable nPins; 1 .. pinsBaseW + 1)
        foreach (pin; 0 .. nPins)
            box[boxH - 2 - nPins][centerH + 1 - nPins + pin * 2] = Cell.pin;
}

void drawBox() {
    foreach_reverse (const ref row; box)
        writefln("%(%c%)", row);
}

void main() {
    initializeBox;
    Ball[] balls;

    foreach (const i; 0 .. nMaxBalls + boxH) {
        writefln("\nStep %d:", i);
        if (i < nMaxBalls)
            balls ~= Ball(centerH, boxH - 2); // Add ball.
        drawBox;

        // Next step for the simulation.
        // Frozen balls are kept in balls array for simplicity.
        foreach (ref b; balls)
            b.doStep;
    }
}
```

{{out}}

```txt

Step 0:
+---------------------------------------+
|                   o                   |
|                   .                   |
|                  . .                  |
|                 . . .                 |
|                . . . .                |
|               . . . . .               |
|              . . . . . .              |
|             . . . . . . .             |
|            . . . . . . . .            |
|           . . . . . . . . .           |
|          . . . . . . . . . .          |
|         . . . . . . . . . . .         |
|        . . . . . . . . . . . .        |
|       . . . . . . . . . . . . .       |
|      . . . . . . . . . . . . . .      |
|     . . . . . . . . . . . . . . .     |
|    . . . . . . . . . . . . . . . .    |
|   . . . . . . . . . . . . . . . . .   |
|  . . . . . . . . . . . . . . . . . .  |
| . . . . . . . . . . . . . . . . . . . |
|                                       |
|                                       |
|                                       |
|                                       |
|                                       |
|                                       |
|                                       |
|                                       |
|                                       |
|                                       |
|                                       |
|                                       |
|                                       |
|                                       |
|                                       |
+---------------------------------------+

...

Step 39:
+---------------------------------------+
|                   o                   |
|                  o.                   |
|                  . .o                 |
|                 . .o.                 |
|                .o. . .                |
|               .o. . . .               |
|              . . .o. . .              |
|             . . .o. . . .             |
|            . . .o. . . . .            |
|           . . . .o. . . . .           |
|          . . . . . .o. . . .          |
|         . . . . . .o. . . . .         |
|        . . . .o. . . . . . . .        |
|       . . . . . .o. . . . . . .       |
|      . . . . . . .o. . . . . . .      |
|     . . . . . .o. . . . . . . . .     |
|    . . . .o. . . . . . . . . . . .    |
|   . . . . . . . . .o. . . . . . . .   |
|  . . . . . .o. . . . . . . . . . . .  |
| . . . . . . . .o. . . . . . . . . . . |
|                      o                |
|                  o                    |
|                  o                    |
|                    o                  |
|                o                      |
|                  o                    |
|                  o                    |
|            o                          |
|                    o                  |
|                    o                  |
|                    o                  |
|              o         o              |
|              o                        |
|              o o o                    |
|              o o     o                |
+---------------------------------------+

...

Step 91:
+---------------------------------------+
|                                       |
|                   .                   |
|                  . .                  |
|                 . . .                 |
|                . . . .                |
|               . . . . .               |
|              . . . . . .              |
|             . . . . . . .             |
|            . . . . . . . .            |
|           . . . . . . . . .           |
|          . . . . . . . . . .          |
|         . . . . . . . . . . .         |
|        . . . . . . . . . . . .        |
|       . . . . . . . . . . . . .       |
|      . . . . . . . . . . . . . .      |
|     . . . . . . . . . . . . . . .     |
|    . . . . . . . . . . . . . . . .    |
|   . . . . . . . . . . . . . . . . .   |
|  . . . . . . . . . . . . . . . . . .  |
| . . . . . . . . . . . . . . . . . . . |
|                                       |
|                                       |
|                o                      |
|                o   o                  |
|                o   o                  |
|                o   o                  |
|                o   o                  |
|                o   o                  |
|                o   o   o              |
|                o o o   o              |
|                o o o   o              |
|              o o o o   o              |
|              o o o o o o o            |
|          o o o o o o o o o o          |
|      o   o o o o o o o o o o          |
+---------------------------------------+
```


## Elm


```elm
import Html.App exposing (program)
import Time exposing (Time, every, millisecond)
import Color exposing (Color, black, red, blue, green)
import Collage exposing (collage)
import Collage exposing (collage,polygon, filled, move, Form, circle)
import Element exposing (toHtml)
import Html exposing (Attribute, Html, text, div, input, button)
import Html.Attributes as A exposing (type', min, placeholder, value, style, disabled)
import Html.Events exposing (onInput, targetValue, onClick)
import Dict exposing (Dict, get, insert)
import String exposing (toInt)
import Result exposing (withDefault)
import Random.Pcg as Random exposing (Seed, bool, initialSeed, independentSeed, step, map)

width = 500
height = 600
hscale = 10.0
vscale = hscale * 2
margin = 30
levelCount = 12
radius = hscale/ 2.0

type State = InBox Int Int Seed | Falling Int Float Float Float | Landed Int Float

type Coin = Coin State Color

colorCycle : Int -> Color
colorCycle i =
  case i % 3 of
    0 -> red
    1 -> blue
    _ -> green

initCoin : Int -> Seed -> Coin
initCoin indx seed = Coin (InBox 0 0 seed) (colorCycle indx)

drawCoin : Coin -> Form
drawCoin (Coin state color) = 
  let dropLevel = toFloat (height//2 - margin)
      (level, shift, distance) = 
        case state of
          InBox level shift seed -> (level, shift, 0)
          Falling shift distance _ _-> (levelCount, shift, distance)
          Landed shift distance -> (levelCount, shift, distance)
      position = 
        (             hscale * toFloat shift
        , dropLevel - vscale * (toFloat level) - distance + radius / 2.0)

  in radius |> circle |> filled color |> move position 

drawGaltonBox : List Form
drawGaltonBox = 
  let levels = [0..levelCount-1]
 
      -- doubles :
      -- [0,2,4,6,8...]
      doubles = List.map (\n -> 2 * n) levels

      -- sequences :
      -- [[0], [0,2], [0,2,4], [0,2,4,6], [0,2,4,6,8],...]
      sequences = case List.tail (List.scanl (::) [] (doubles)) of
        Nothing -> []
        Just ls -> ls

      -- galtonCoords :
      -- [                            (0,0), 
      --                       (-1,1),      (1,1), 
      --                (-2,2),       (0,2),      (2,2), 
      --         (-3,3),       (-1,3),      (1,3),      (3,3), 
      --  (-4,4),       (-2,4),       (0,4),      (2,4),      (4,4), ...]
      galtonCoords = 
        List.map2 
          (\ls level -> List.map (\n -> (n - level, level)) ls) 
          sequences 
          levels
        |> List.concat

      peg = polygon [(0,0), (-4, -8), (4, -8)] |> filled black 

      apex = toFloat (height//2 - margin)

  in List.map (\(x,y) -> move (hscale*toFloat x,  apex - vscale*toFloat y) peg) galtonCoords

coinsInBin : Int -> Dict Int Int -> Int
coinsInBin binNumber bins = 
  case get binNumber bins of
    Nothing -> 0
    Just n -> n

addToBins : Int -> Dict Int Int -> Dict Int Int
addToBins binNumber bins = 
  insert binNumber (coinsInBin binNumber bins + 1) bins

updateCoin : (Coin, Dict Int Int) -> (Coin, Dict Int Int)
updateCoin (Coin state color as coin, bins) = 
  case state of
    InBox level shift seed ->
      let deltaShift = map (\b -> if b then 1 else -1) bool
          (delta, newSeed) = step deltaShift seed
          newShift = shift+delta
          newLevel = (level)+1
      in if (newLevel < levelCount) then
           (Coin (InBox newLevel newShift newSeed) color, bins)
         else -- transition to falling
           let maxDrop = toFloat (height - 2 * margin) - toFloat (levelCount) * vscale
               floor = maxDrop - toFloat (coinsInBin newShift bins) * (radius*2 + 1)
           in (Coin (Falling newShift -((vscale)/2.0) 10 floor) color, addToBins newShift bins)

    Falling shift distance velocity floor -> 
      let newDistance = distance + velocity
      in if (newDistance < floor) then
           (Coin (Falling shift newDistance (velocity + 1) floor) color, bins)
         else -- transtion to landed
           (Coin (Landed shift floor) color, bins)

    Landed _ _ -> (coin, bins) -- unchanged

type alias Model = 
  { coins : List Coin
  , bins : Dict Int Int
  , count : Int
  , started : Bool
  , seedInitialized : Bool
  , seed : Seed
  }

init : (Model, Cmd Msg)
init =
  ( { coins = []
    , bins = Dict.empty
    , count = 0
    , started = False
    , seedInitialized = False
    , seed = initialSeed 45 -- This will not get used.  Actual seed used is time dependent and set when the first coin drops.
    }, Cmd.none)

type Msg = Drop Time | Tick Time | SetCount String | Go

update : Msg -> Model -> (Model, Cmd Msg)
update action model = 
  case action of
    Go ->
      ({model | started = model.count > 0}, Cmd.none)

    SetCount countString -> 
      ({ model | count = toInt countString |> withDefault 0 }, Cmd.none)

    Drop t -> 
      if (model.started && model.count > 0) then
          let newcount = model.count - 1
              seed' =  if model.seedInitialized then model.seed else initialSeed (truncate t)
              (seed'', coinSeed) = step independentSeed seed'
          in ({ model  
              | coins = initCoin (truncate t) coinSeed :: model.coins
              , count = newcount
              , started = newcount > 0
              , seedInitialized = True
              , seed = seed''}, Cmd.none)
      else
         (model, Cmd.none)

    Tick _ -> 
      -- foldr to execute update, append to coins, replace bins
      let (updatedCoins, updatedBins) =
        List.foldr (\coin (coinList, bins) -> 
                       let (updatedCoin, updatedBins) = updateCoin (coin, bins) 
                       in (updatedCoin :: coinList, updatedBins))
                   ([], model.bins)
                   model.coins
      in ({ model | coins = updatedCoins, bins = updatedBins}, Cmd.none)

view : Model -> Html Msg
view model = 
  div []
    [ input
        [ placeholder "How many?"
        , let showString = if model.count > 0 then model.count |> toString else ""
          in value showString
        , onInput SetCount
        , disabled model.started
        , style [ ("height", "20px") ]
        , type' "number"
        , A.min "1"
        ]
        []

     , button
        [ onClick Go 
        , disabled model.started
        , style [ ("height", "20px") ]
        ]
        [ Html.text "GO!" ]

     , let coinForms = (List.map (drawCoin) model.coins)
       in collage width height (coinForms ++ drawGaltonBox) |> toHtml
    ]

subscriptions model =
    Sub.batch
        [ every (40*millisecond) Tick
        , every (200*millisecond) Drop
        ]

main =
  program 
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }
```


Link to live demo: http://dc25.github.io/galtonBoxAnimationElm/ .  Follow the link, enter a number and press the GO button.


## Factor

{{works with|Factor|0.99 development release 2019-03-17}}

```factor
USING: accessors arrays calendar colors combinators
combinators.short-circuit fonts fry generalizations kernel
literals locals math math.ranges math.vectors namespaces opengl
random sequences timers ui ui.commands ui.gadgets
ui.gadgets.worlds ui.gestures ui.pens.solid ui.render ui.text ;
IN: rosetta-code.galton-box-animation

CONSTANT: pegs $[ 20 300 40 <range> ]
CONSTANT: speed 90
CONSTANT: balls 140
CONSTANT: peg-color  T{ rgba f 0.60 0.4 0.60 1.0 }
CONSTANT: ball-color T{ rgba f 0.80 1.0 0.20 1.0 }
CONSTANT: slot-color T{ rgba f 0.00 0.2 0.40 1.0 }
CONSTANT: bg-color   T{ rgba f 0.02 0.0 0.02 1.0 }

CONSTANT: font $[
    monospace-font
    t >>bold?
    T{ rgba f 0.80 1.0 0.20 1.0 } >>foreground
    T{ rgba f 0.02 0.0 0.02 1.0 } >>background
]

TUPLE: galton < gadget balls { frame initial: 1 } ;

DEFER: on-tick

: <galton-gadget> ( -- gadget )
    galton new bg-color <solid> >>interior V{ } clone >>balls
    dup [ on-tick ] curry f speed milliseconds <timer>
    start-timer ;

: add-ball ( gadget -- )
    dup frame>> balls <
    [ { 250 -20 } swap balls>> [ push ] keep ] when drop ;

: draw-msg ( -- )
    { 10 10 }
    [ font "Press <space> for new animation" draw-text ]
    with-translation ;

: draw-slots ( -- )
    slot-color gl-color { 70 350 } { 70 871 }
    10 [ 2dup gl-line [ { 40 0 } v+ ] bi@ ] times 2drop
    { 70 871 } { 430 871 } gl-line ;

: diamond-side ( loc1 loc2 loc3 -- )
    [ v+ dup ] [ v+ gl-line ] bi* ;

: draw-diamond ( loc color -- )
    gl-color {
        [ { 0 -10 } { 10 10 } ]
        [ { 10 0 } { -10 10 } ]
        [ { 0 10 } { -10 -10 } ]
        [ { -10 0 } { 10 -10 } ]
    } [ diamond-side ] map-compose cleave ;

: draw-peg-row ( loc n -- )
    <iota> [ 40 * 0 2array v+ peg-color draw-diamond ] with
    each ;

: draw-peg-triangle ( -- )
    { 250 40 } 1
    8 [ 2dup draw-peg-row [ { -20 40 } v+ ] dip 1 + ] times
    2drop ;

: draw-balls ( gadget -- )
    balls>> [ ball-color draw-diamond ] each ;

: rand-side ( loc -- loc' ) { { 20 20 } { -20 20 } } random v+ ;

:: collide? ( GADGET BALL -- ? )
    BALL second :> y
    BALL { 0 20 } v+ :> tentative
    { [ y 860 = ] [ tentative GADGET balls>> member? ] } 0|| ;

:: update-ball ( GADGET BALL -- BALL' )
    {
        { [ BALL second pegs member? ] [ BALL rand-side ] }
        { [ GADGET BALL collide? ] [ BALL ] }
        [ BALL { 0 20 } v+ ]
    } cond ;

: update-balls ( gadget -- )
    dup '[ [ _ swap update-ball ] map ] change-balls drop ;

: on-tick ( gadget -- )
    {
        [ dup frame>> odd? [ add-ball ] [ drop ] if ]
        [ relayout-1 ] [ update-balls ]
        [ [ 1 + ] change-frame drop ]
    } cleave ;

M: galton pref-dim* drop { 500 900 } ;

M: galton draw-gadget*
    draw-peg-triangle draw-msg draw-slots draw-balls ;

: com-new ( gadget -- ) V{ } clone >>balls 1 >>frame drop ;

galton "gestures" f {
    { T{ key-down { sym " " } } com-new }
} define-command-map

MAIN-WINDOW: galton-box-animation
    {
        { title "Galton Box Animation" }
        { window-controls 
            { normal-title-bar close-button minimize-button } }
    } <galton-gadget> >>gadgets ;
```

{{out}}
Image taken from the program mid-animation: [https://i.imgur.com/E2ge7LE.png]


## Go

{{trans|D}}

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

const boxW = 41      // Galton box width
const boxH = 37      // Galton box height.
const pinsBaseW = 19 // Pins triangle base.
const nMaxBalls = 55 // Number of balls.

const centerH = pinsBaseW + (boxW-pinsBaseW*2+1)/2 - 1

const (
    empty  = ' '
    ball   = 'o'
    wall   = '|'
    corner = '+'
    floor  = '-'
    pin    = '.'
)

type Ball struct{ x, y int }

func newBall(x, y int) *Ball {
    if box[y][x] != empty {
        panic("Tried to create a new ball in a non-empty cell. Program terminated.")
    }
    b := Ball{x, y}
    box[y][x] = ball
    return &b
}

func (b *Ball) doStep() {
    if b.y <= 0 {
        return // Reached the bottom of the box.
    }
    cell := box[b.y-1][b.x]
    switch cell {
    case empty:
        box[b.y][b.x] = empty
        b.y--
        box[b.y][b.x] = ball
    case pin:
        box[b.y][b.x] = empty
        b.y--
        if box[b.y][b.x-1] == empty && box[b.y][b.x+1] == empty {
            b.x += rand.Intn(2)*2 - 1
            box[b.y][b.x] = ball
            return
        } else if box[b.y][b.x-1] == empty {
            b.x++
        } else {
            b.x--
        }
        box[b.y][b.x] = ball
    default:
        // It's frozen - it always piles on other balls.
    }
}

type Cell = byte

/* Galton box. Will be printed upside down. */
var box [boxH][boxW]Cell

func initializeBox() {
    // Set ceiling and floor
    box[0][0] = corner
    box[0][boxW-1] = corner
    for i := 1; i < boxW-1; i++ {
        box[0][i] = floor
    }
    for i := 0; i < boxW; i++ {
        box[boxH-1][i] = box[0][i]
    }

    // Set walls
    for r := 1; r < boxH-1; r++ {
        box[r][0] = wall
        box[r][boxW-1] = wall
    }

    // Set rest to empty initially
    for i := 1; i < boxH-1; i++ {
        for j := 1; j < boxW-1; j++ {
            box[i][j] = empty
        }
    }

    // Set pins
    for nPins := 1; nPins <= pinsBaseW; nPins++ {
        for p := 0; p < nPins; p++ {
            box[boxH-2-nPins][centerH+1-nPins+p*2] = pin
        }
    }
}

func drawBox() {
    for r := boxH - 1; r >= 0; r-- {
        for c := 0; c < boxW; c++ {
            fmt.Printf("%c", box[r][c])
        }
        fmt.Println()
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    initializeBox()
    var balls []*Ball
    for i := 0; i < nMaxBalls+boxH; i++ {
        fmt.Println("\nStep", i, ":")
        if i < nMaxBalls {
            balls = append(balls, newBall(centerH, boxH-2)) // add ball
        }
        drawBox()

        // Next step for the simulation.
        // Frozen balls are kept in balls slice for simplicity
        for _, b := range balls {
            b.doStep()
        }
    }
}
```


{{out}}
Sample output (showing last step only):

```txt

Step 91 :
+---------------------------------------+
|                                       |
|                   .                   |
|                  . .                  |
|                 . . .                 |
|                . . . .                |
|               . . . . .               |
|              . . . . . .              |
|             . . . . . . .             |
|            . . . . . . . .            |
|           . . . . . . . . .           |
|          . . . . . . . . . .          |
|         . . . . . . . . . . .         |
|        . . . . . . . . . . . .        |
|       . . . . . . . . . . . . .       |
|      . . . . . . . . . . . . . .      |
|     . . . . . . . . . . . . . . .     |
|    . . . . . . . . . . . . . . . .    |
|   . . . . . . . . . . . . . . . . .   |
|  . . . . . . . . . . . . . . . . . .  |
| . . . . . . . . . . . . . . . . . . . |
|                                       |
|                                       |
|                o                      |
|                o                      |
|                o o                    |
|                o o                    |
|                o o                    |
|              o o o                    |
|              o o o                    |
|              o o o o   o              |
|              o o o o   o              |
|            o o o o o o o              |
|            o o o o o o o              |
|          o o o o o o o o              |
|          o o o o o o o o o            |
+---------------------------------------+

```



## Haskell


```haskell
import Data.Map hiding (map, filter)
import Graphics.Gloss
import Control.Monad.Random

data Ball = Ball { position :: (Int, Int), turns :: [Int] }

type World = ( Int           -- number of rows of pins
             , [Ball]        -- sequence of balls
             , Map Int Int ) -- counting bins

updateWorld :: World -> World
updateWorld (nRows, balls, bins)
  | y < -nRows-5  = (nRows, map update bs, bins <+> x)
  | otherwise     = (nRows, map update balls, bins)
  where
    (Ball (x,y) _) : bs = balls

    b <+> x = unionWith (+) b (singleton x 1)

    update (Ball (x,y) turns)
      | -nRows <= y && y < 0 = Ball (x + head turns, y - 1) (tail turns)
      | otherwise            = Ball (x, y - 1) turns
        
drawWorld :: World -> Picture
drawWorld (nRows, balls, bins) = pictures [ color red ballsP
                                          , color black binsP
                                          , color blue pinsP ]
  where ballsP = foldMap (disk 1) $ takeWhile ((3 >).snd) $ map position balls        
        binsP  = foldMapWithKey drawBin bins
        pinsP  = foldMap (disk 0.2) $ [1..nRows] >>= \i ->
                                          [1..i] >>= \j -> [(2*j-i-1, -i-1)]

        disk r pos = trans pos $ circleSolid (r*10)
        drawBin x h = trans (x,-nRows-7)
                    $ rectangleUpperSolid 20 (-fromIntegral h)
        trans (x,y) = Translate (20 * fromIntegral x) (20 * fromIntegral y)

startSimulation :: Int -> [Ball] -> IO ()
startSimulation nRows balls = simulate display white 50 world drawWorld update
  where display = InWindow "Galton box" (400, 400) (0, 0)
        world = (nRows, balls, empty)
        update _ _ = updateWorld

main = evalRandIO balls >>= startSimulation 10
  where balls = mapM makeBall [1..]
        makeBall y = Ball (0, y) <$> randomTurns
        randomTurns = filter (/=0) <$> getRandomRs (-1, 1)
```


=={{header|Icon}} and {{header|Unicon}}==
The code here is adapted from the Unicon Book.
[[File:Galtonbox-Unicon.PNG|thumb|right]]


```Icon
link graphics

global pegsize, pegsize2, height, width, delay

procedure main(args)    # galton box simulation from Unicon book
   pegsize2 := (pegsize := 10) * 2    # pegs & steps
   delay := 2                         # ms delay
   setup_galtonwindow(pegsize)
   n := integer(args[1]) | 100        # balls to drop 
   every 1 to n do galton(pegsize)
   WDone()
end

procedure setup_galtonwindow(n)  # Draw n levels of pegs, 
local xpos, ypos, i, j
   # Pegboard size is 2n-1 square
   # Expected max value of histogram is (n, n/2)/2^n 
   # ... approximate with something simpler?

   height := n*n/2*pegsize + (width := (2*n+1)*pegsize)
   Window("size=" || width || "," || height, "fg=grayish-white")
   WAttrib("fg=dark-grey")
   every ypos := (i := 1 to n) * pegsize2 do {
      xpos := width/2 - (i - 1) * pegsize - pegsize/2 - pegsize2
      every 1 to i do
         FillArc(xpos +:= pegsize2, ypos, pegsize, pegsize)
      }
   WAttrib("fg=black","drawop=reverse")      # set drawing mode for balls
end

procedure galton(n)                          # drop a ball into the galton box
local xpos, ypos, oldx, oldy
   xpos := oldx := width/2 - pegsize/2
   ypos := oldy := pegsize
   every 1 to n do {                         # For every ball...
      xpos +:= ((?2 = 1) | -1) * pegsize     # +/- pegsize
      animate(.oldx, .oldy, oldx := xpos, oldy := ypos +:= pegsize2)
      }
   animate(xpos, ypos, xpos, ypos + 40)      # Now the ball falls ...
   animate(xpos, ypos+40, xpos, ypos + 200)  # ... to the floor
   draw_ball(xpos)                           # Record this ball
end

procedure animate(xfrom, yfrom, xto, yto)
   animate_actual(xfrom, yfrom, xto, yfrom, 4)
   animate_actual(xto, yfrom, xto, yto, 10)
end


procedure animate_actual(xfrom, yfrom, xto, yto, steps) # attribs already set
local x, y, xstep, ystep, lastx, lasty
   x -:= xstep := (xto - (x := xfrom))/steps
   y -:= ystep := (yto - (y := yfrom))/steps
   every 1 to steps do {      
      FillArc(lastx := x +:= xstep, lasty := y +:= ystep, pegsize, pegsize)
      WDelay(delay)      # wait in ms
      FillArc(x, y, pegsize, pegsize)
      }
end

procedure draw_ball(x)                      
static ballcounts
initial ballcounts := table(0)
   FillArc(x, height-(ballcounts[x] +:= 1)*pegsize, pegsize, pegsize)
end
```



## J


First, we need to represent our pins:


```j
initpins=: '* ' {~ '1'&i.@(-@|. |."_1 [: ":@-.&0"1 <:~/~)@i.
```


For example:


```j
   initpins 4
   *   
  * *  
 * * * 
* * * *
```


Note that we could introduce other pin arrangements, for example a Sierpinski triangle:


```j
initSpins=: [: }.@|. (1- 2&^@>:) ]\ [: ,] (,~ ,.~)@]^:[ ,: bind '* '
```


... but this will not be too interesting to use, because of the lack of interior pins for the balls to bounce off of.

Anyways, once we have that, we can add balls to our picture:


```j
init=: ' ',. ' ',.~ ] ,~ ' ',~ ' o' {~  (# ' ' ~: 1&{.)
```


For example: 


```j
   3 (init initpins) 4
    o    
    o    
    o    
         
    *    
   * *   
  * * *  
 * * * * 
```


Now we just need some way of updating our datastructure.

We will need a mechanism to shift a ball left or right if it's above a pin:

<lang>bounce=: (C.~ ] <"1@:+ 0 1 -~/~ ? @: (2"0))"1 [: I. 'o*'&E."1&.|:
```


And, a mechanism to make the balls fall:

<lang>shift=: 4 :0
  fill=. {.0#,y
  x |.!.fill y
)
```


And then we need to separate out the balls from the pins, so the balls fall and the pins do not.  Note also that in this representation, balls will have to fall when they bounce because they cannot occupy the same space that a pin occupies.

We will also want some way of preventing the balls from falling forever.  For this task it's probably sufficient to introduce a baseline just deep enough to hold the stacks (which have passed through the pins) and have later balls instantly fall as close as they can to the baseline once they are passed the pins.


```j
pins=: '*'&=
balls=: 'o'&=

bounce=: (C.~ 0 1 <@(-/~) [: (+ ?@2:"0) I.)"1 

nxt=: ' ',~ [: clean ' *o' {~ pins + 2 * _1 shift balls bounce balls *. 1 shift pins

clean2=: ({. , -.&' '"1&.|:&.|.@}.)~ 1 + >./@(# | '*' i:~"1 |:)
clean1=: #~ 1 1 -.@E. *./"1@:=&' '
clean=: clean1@clean2
```


For example:


```j
   nxt nxt 3 (init initpins) 4
         
    o    
    o    
   o*    
   * *   
  * * *  
 * * * * 
         
```


Or, showing an entire animation sequence:


```j
   nxt&.>^:a: <7 (init ' ',.' ',.~ initpins) 5
┌─────────────┬─────────────┬─────────────┬─────────────┬─────────────┬─────────────┬─────────────┬─────────────┬─────────────┬─────────────┬─────────────┬─────────────┬─────────────┬─────────────┐
│      o      │             │             │             │             │             │             │             │             │             │             │             │             │             │
│      o      │      o      │      o      │      o      │      o      │      o      │      o      │      o      │      *o     │      *      │      *      │      *      │      *      │      *      │
│      o      │      o      │      o      │      o      │      o      │      o      │      o      │      *o     │     *o*     │     *o*     │     * *     │     * *     │     * *     │     * *     │
│      o      │      o      │      o      │      o      │      o      │      o      │      *o     │     * *o    │    * * *o   │    * *o*    │    * *o*    │    * * *    │    * * *    │    * * *    │
│      o      │      o      │      o      │      o      │      o      │      *o     │     * *o    │    * *o*    │   * *o* *   │   * * * *o  │   * *o* *   │   * *o* *   │   * * * *   │   * * * *   │
│      o      │      o      │      o      │      o      │     o*      │    o* *     │    *o* *    │   * *o* *   │  * * *o* *  │  * *o* * *  │  * * * * *o │  * * *o* *  │  * *o* * *  │  * * * * *  │
│      o      │      o      │      o      │      *o     │     *o*     │    * *o*    │   * * *o*   │  * * *o* *  │       o     │       o     │       o     │       o     │       o     │       o     │
│             │      o      │      *o     │     * *o    │    * *o*    │   * *o* *   │  * * *o* *  │       o     │       o     │       o     │       o     │       o     │       o     │       o     │
│      *      │      *      │     * *     │    * * *    │   * * * *   │  * * * * *  │             │             │             │       o     │     o o     │     o o   o │       o     │     o o     │
│     * *     │     * *     │    * * *    │   * * * *   │  * * * * *  │             │             │             │             │             │             │             │     o o   o │     o o   o │
│    * * *    │    * * *    │   * * * *   │  * * * * *  │             │             │             │             │             │             │             │             │             │             │
│   * * * *   │   * * * *   │  * * * * *  │             │             │             │             │             │             │             │             │             │             │             │
│  * * * * *  │  * * * * *  │             │             │             │             │             │             │             │             │             │             │             │             │
│             │             │             │             │             │             │             │             │             │             │             │             │             │             │
└─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┴─────────────┘
```



## Java

The balls keep track of where they are, and we just have to move them down and print. You might easily adjust this to take command line input for the numbers of pins and balls. I'm sure that this could be a lot shorter...

```Java
import java.util.Random;
import java.util.List;
import java.util.ArrayList;

public class GaltonBox {
    public static void main( final String[] args ) {
        new GaltonBox( 8, 200 ).run();
    }

    private final int        m_pinRows;
    private final int        m_startRow;
    private final Position[] m_balls;
    private final Random     m_random = new Random();

    public GaltonBox( final int pinRows, final int ballCount ) {
        m_pinRows  = pinRows;
        m_startRow = pinRows + 1;
        m_balls    = new Position[ ballCount ];

        for ( int ball = 0; ball < ballCount; ball++ )
            m_balls[ ball ] = new Position( m_startRow, 0, 'o' );
    }

    private static class Position {
        int  m_row;
        int  m_col;
        char m_char;

        Position( final int row, final int col, final char ch ) {
            m_row  = row;
            m_col  = col;
            m_char = ch;
        }
    }

    public void run() {
        for ( int ballsInPlay = m_balls.length; ballsInPlay > 0;  ) {
            ballsInPlay = dropBalls();
            print();
        }
    }

    private int dropBalls() {
        int ballsInPlay = 0;
        int ballToStart = -1;

        // Pick a ball to start dropping
        for ( int ball = 0; ball < m_balls.length; ball++ )
            if ( m_balls[ ball ].m_row == m_startRow )
                ballToStart = ball;

        // Drop balls that are already in play
        for ( int ball = 0; ball < m_balls.length; ball++ )
            if ( ball == ballToStart ) {
                m_balls[ ball ].m_row = m_pinRows;
                ballsInPlay++;
            }
            else if ( m_balls[ ball ].m_row > 0 && m_balls[ ball ].m_row != m_startRow ) {
                m_balls[ ball ].m_row -= 1;
                m_balls[ ball ].m_col += m_random.nextInt( 2 );
                if ( 0 != m_balls[ ball ].m_row )
                    ballsInPlay++;
            }

        return ballsInPlay;
    }

    private void print() {
        for ( int row = m_startRow; row --> 1;  ) {
            for ( int ball = 0; ball < m_balls.length; ball++ )
                if ( m_balls[ ball ].m_row == row )
                    printBall( m_balls[ ball ] );
            System.out.println();
            printPins( row );
        }
        printCollectors();
        System.out.println();
    }

    private static void printBall( final Position pos ) {
        for ( int col = pos.m_row + 1; col --> 0;  )
            System.out.print( ' ' );
        for ( int col = 0; col < pos.m_col; col++ )
            System.out.print( "  " );
        System.out.print( pos.m_char );
    }

    private void printPins( final int row ) {
        for ( int col = row + 1; col --> 0;  )
            System.out.print( ' ' );
        for ( int col = m_startRow - row; col --> 0;  )
            System.out.print( ". " );
        System.out.println();
    }

    private void printCollectors() {
        final List<List<Position>> collectors = new ArrayList<List<Position>>();

        for ( int col = 0; col < m_startRow; col++ ) {
            final List<Position> collector = new ArrayList<Position>();

            collectors.add( collector );
            for ( int ball = 0; ball < m_balls.length; ball++ )
                if ( m_balls[ ball ].m_row == 0 && m_balls[ ball ].m_col == col )
                    collector.add( m_balls[ ball ] );
        }

        for ( int row = 0, rows = longest( collectors ); row < rows; row++ ) {
            for ( int col = 0; col < m_startRow; col++ ) {
                final List<Position> collector = collectors.get( col );
                final int            pos       = row + collector.size() - rows;

                System.out.print( '|' );
                if ( pos >= 0 )
                    System.out.print( collector.get( pos ).m_char );
                else
                    System.out.print( ' ' );
            }
            System.out.println( '|' );
        }
    }

    private static final int longest( final List<List<Position>> collectors ) {
        int result = 0;

        for ( final List<Position> collector : collectors )
            result = Math.max( collector.size(), result );

        return result;
    }
}
```

{{out}}
When only five balls have begun to fall through the pins:

```txt
         o
         . 
          o
        . . 
           o
       . . . 
        o
      . . . . 
         o
     . . . . . 

    . . . . . . 

   . . . . . . . 

  . . . . . . . . 
```


Later, some balls have arrived in the collectors:

```txt
         o
         . 
          o
        . . 
         o
       . . . 
          o
      . . . . 
         o
     . . . . . 
          o
    . . . . . . 
           o
   . . . . . . . 
        o
  . . . . . . . . 
| | | | | |o| | | |
| | | |o|o|o| | | |
```

Note that the collectors are as deep as required.

Finally, all the balls are in the collectors:

```txt
         . 

        . . 

       . . . 

      . . . . 

     . . . . . 

    . . . . . . 

   . . . . . . . 

  . . . . . . . . 
| | | | |o| | | | |
| | | | |o| | | | |
| | | | |o| | | | |
| | | | |o| | | | |
| | | | |o| | | | |
| | | | |o| | | | |
| | | | |o| | | | |
| | | | |o| | | | |
| | | |o|o| | | | |
| | | |o|o| | | | |
| | | |o|o| | | | |
| | | |o|o| | | | |
| | | |o|o| | | | |
| | | |o|o| | | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o| | | |
| | | |o|o|o|o| | |
| | | |o|o|o|o| | |
| | | |o|o|o|o| | |
| | | |o|o|o|o| | |
| | | |o|o|o|o| | |
| | | |o|o|o|o| | |
| | | |o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o| | |
| | |o|o|o|o|o|o| |
| | |o|o|o|o|o|o| |
| | |o|o|o|o|o|o| |
| |o|o|o|o|o|o|o| |
| |o|o|o|o|o|o|o| |
| |o|o|o|o|o|o|o| |
```



## JavaScript

Works with NodeJs

```JavaScript
const readline = require('readline');

/**
 * Galton Box animation
 * @param {number} layers The number of layers in the board
 * @param {number} balls The number of balls to pass through
 */
const galtonBox = (layers, balls) => {
  const speed = 100;
  const ball = 'o';
  const peg = '.';
  const result = [];

  const sleep = ms => new Promise(resolve => {
    setTimeout(resolve,ms)
  });

  /**
   * The board is represented as a 2D array.
   * @type {Array<Array<string>>}
   */
  const board = [...Array(layers)]
      .map((e, i) => {
        const sides = Array(layers - i).fill(' ');
        const a = Array(i + 1).fill(peg).join(' ').split('');
        return [...sides, ...a, ...sides];
      });

  /**
   * @return {Array<string>}
   */
  const emptyRow = () => Array(board[0].length).fill(' ');

  /**
   * @param {number} i
   * @returns {number}
   */
  const bounce = i => Math.round(Math.random()) ? i - 1 : i + 1;

  /**
   * Prints the current state of the board and the collector
   */
  const show = () => {
    readline.cursorTo(process.stdout, 0, 0);
    readline.clearScreenDown(process.stdout);
    board.forEach(e => console.log(e.join('')));
    result.reverse();
    result.forEach(e => console.log(e.join('')));
    result.reverse();
  };


  /**
   * Collect the result.
   * @param {number} idx
   */
  const appendToResult = idx => {
    const row = result.find(e => e[idx] === ' ');
    if (row) {
      row[idx] = ball;
    } else {
      const newRow = emptyRow();
      newRow[idx] = ball;
      result.push(newRow);
    }
  };

  /**
   * Move the balls through the board
   * @returns {boolean} True if the there are balls in the board.
   */
  const iter = () => {
    let hasNext = false;
    [...Array(bordSize)].forEach((e, i) => {
      const rowIdx = (bordSize - 1) - i;
      const idx = board[rowIdx].indexOf(ball);
      if (idx > -1) {
        board[rowIdx][idx] = ' ';
        const nextRowIdx = rowIdx + 1;
        if (nextRowIdx < bordSize) {
          hasNext = true;
          const nextRow = board[nextRowIdx];
          nextRow[bounce(idx)] = ball;
        } else {
          appendToResult(idx);
        }
      }
    });
    return hasNext;
  };

  /**
   * Add a ball to the board.
   * @returns {number} The number of balls left to add.
   */
  const addBall = () => {
    board[0][apex] = ball;
    balls = balls - 1;
    return balls;
  };

  board.unshift(emptyRow());
  result.unshift(emptyRow());

  const bordSize = board.length;
  const apex = board[1].indexOf(peg);

  /**
   * Run the animation
   */
  (async () => {
    while (addBall()) {
      await sleep(speed).then(show);
      iter();
    }
    await sleep(speed).then(show);
    while (iter()) {
      await sleep(speed).then(show);
    }
    await sleep(speed).then(show);
  })();


};

galtonBox(12, 50);
```

{{out}}

```txt
                         
            .            
           . .           
          . . .          
         . . . .         
        . . . . .        
       . . . . . .       
      . . . . . . .      
     . . . . . . . .     
    . . . . . . . . .    
   . . . . . . . . . .   
  . . . . . . . . . . .  
 . . . . . . . . . . . . 
          o              
          o              
          o              
          o              
          o   o          
          o   o          
          o   o          
          o o o          
          o o o          
          o o o o        
          o o o o        
        o o o o o        
        o o o o o o      
      o o o o o o o      
      o o o o o o o o   
```



## Julia

This is a proof of concept code. It does not use external packages. The ASCII animation is running in the stdout terminal, which has to be at least 28 character wide, and 40 character high, and accept ANSI control sequences for positioning the cursor, and clearing the screen (e.g. the Windows console, or ConEmu).

6 pins in 6 rows are hard coded. The next ball is released at the press of the Enter key. Keep it depressed for continuous running.
The balls are randomly deflected left or right at hitting the pins, and they fall to the bins at the bottom, which extend downwards.
The timer function sets the speed of the animation. Change the "interval" parameter to larger values for slower movement.
Pressing x then Enter exits, other keys are ignored.
{{works with|Julia|1.0}}


```julia
using Random
function drawball(timer)
    global r, c, d
    print("\e[$r;$(c)H ")       # clear last ball position (r,c)
    if (r+=1) > 14
        close(timer)
        b = (bin[(c+2)>>2] += 1)# update count in bin
        print("\e[$b;$(c)Ho")   # lengthen bar of balls in bin
    else
        r in 3:2:13 && c in 17-r:4:11+r && (d = 2bitrand()-1)
        print("\e[$r;$(c+=d)Ho")# show ball moving in direction d
    end
end

print("\e[2J")                  # clear screen
for r = 3:2:13, c = 17-r:4:11+r # 6 pins in 6 rows
    print("\e[$r;$(c)H^")       # draw pins
end
print("\e[15;2H-------------------------")

bin = fill(15,7)                # positions of top of bins
while "x" != readline() >= ""   # x-Enter: exit, {keys..}Enter: next ball
    global r,c,d = 0,14,0
    t = Timer(drawball, 0, interval=0.1)
    while r < 15 sleep(0.01) end
    print("\e[40;1H")           # move cursor far down
end
```

{{out}}

```txt

             ^

           ^   ^

         ^   ^   ^

       ^   ^   ^   ^

     ^   ^   ^   ^   ^

   ^   ^   ^   ^   ^   ^

--------------------------
 o   o   o   o   o   o   o
     o   o   o   o   o
         o   o   o   o
         o   o   o   o
         o   o   o   o
         o   o   o
             o   o
             o   o
             o   o
             o   o
             o
             o
             o
             o
             o
```



## Kotlin

{{trans|D}}

```scala
// version 1.2.10

import java.util.Random

val boxW = 41       // Galton box width.
val boxH = 37       // Galton box height.
val pinsBaseW = 19  // Pins triangle base.
val nMaxBalls = 55  // Number of balls.

val centerH = pinsBaseW + (boxW - pinsBaseW * 2 + 1) / 2 - 1
val rand = Random()

enum class Cell(val c: Char) {
    EMPTY(' '),
    BALL('o'),
    WALL('|'),
    CORNER('+'),
    FLOOR('-'),
    PIN('.')
}

/* Galton box. Will be printed upside down. */
val box = List(boxH) { Array<Cell>(boxW) { Cell.EMPTY } }

class Ball(var x: Int, var y: Int) {

    init {
        require(box[y][x] == Cell.EMPTY)
        box[y][x] = Cell.BALL
    }

    fun doStep() {
        if (y <= 0) return  // Reached the bottom of the box.
        val cell = box[y - 1][x]
        when (cell) {
            Cell.EMPTY -> {
                box[y][x] = Cell.EMPTY
                y--
                box[y][x] = Cell.BALL
            }

            Cell.PIN -> {
                box[y][x] = Cell.EMPTY
                y--
                if (box[y][x - 1] == Cell.EMPTY && box[y][x + 1] == Cell.EMPTY) {
                    x += rand.nextInt(2) * 2 - 1
                    box[y][x] = Cell.BALL
                    return
                }
                else if (box[y][x - 1] == Cell.EMPTY) x++
                else x--
                box[y][x] = Cell.BALL
            }

            else -> {
                // It's frozen - it always piles on other balls.
            }
        }
    }
}

fun initializeBox() {
    // Set ceiling and floor:
    box[0][0] = Cell.CORNER
    box[0][boxW - 1] = Cell.CORNER
    for (i in 1 until boxW - 1) box[0][i] = Cell.FLOOR
    for (i in 0 until boxW) box[boxH - 1][i] = box[0][i]

    // Set walls:
    for (r in 1 until boxH - 1) {
        box[r][0] = Cell.WALL
        box[r][boxW - 1] = Cell.WALL
    }

    // Set pins:
    for (nPins in 1..pinsBaseW) {
        for (pin in 0 until nPins) {
            box[boxH - 2 - nPins][centerH + 1 - nPins + pin * 2] = Cell.PIN
        }
    }
}

fun drawBox() {
    for (row in box.reversed()) {
        for (i in row.indices) print(row[i].c)
        println()
    }
}

fun main(args: Array<String>) {
    initializeBox()
    val balls = mutableListOf<Ball>()
    for (i in 0 until nMaxBalls + boxH) {
        println("\nStep $i:")
        if (i < nMaxBalls) balls.add(Ball(centerH, boxH - 2))  // Add ball.
        drawBox()

        // Next step for the simulation.
        // Frozen balls are kept in balls list for simplicity
        for (b in balls) b.doStep()
    }
}
```


Sample output (showing final step only):

```txt

Step 91:
+---------------------------------------+
|                                       |
|                   .                   |
|                  . .                  |
|                 . . .                 |
|                . . . .                |
|               . . . . .               |
|              . . . . . .              |
|             . . . . . . .             |
|            . . . . . . . .            |
|           . . . . . . . . .           |
|          . . . . . . . . . .          |
|         . . . . . . . . . . .         |
|        . . . . . . . . . . . .        |
|       . . . . . . . . . . . . .       |
|      . . . . . . . . . . . . . .      |
|     . . . . . . . . . . . . . . .     |
|    . . . . . . . . . . . . . . . .    |
|   . . . . . . . . . . . . . . . . .   |
|  . . . . . . . . . . . . . . . . . .  |
| . . . . . . . . . . . . . . . . . . . |
|                                       |
|                                       |
|                  o                    |
|                  o                    |
|                o o                    |
|                o o                    |
|                o o o                  |
|                o o o                  |
|                o o o                  |
|                o o o                  |
|                o o o o                |
|                o o o o o              |
|          o     o o o o o o            |
|          o o o o o o o o o            |
|        o o o o o o o o o o o o        |
+---------------------------------------+

```



## Perl

Output shows of final state for a run with 50 coins.
{{trans|Perl 6}}

```perl
use strict;
use warnings;

use List::Util 'any';
use Time::HiRes qw(sleep);
use List::AllUtils <pairwise pairs>;

use utf8;
binmode STDOUT, ':utf8';

my $coins      = shift || 100;
my $peg_lines  = shift || 13;
my $row_count  = $peg_lines;
my $peg        = '^';
my @coin_icons = ("\N{UPPER HALF BLOCK}", "\N{LOWER HALF BLOCK}");

my @coins = (undef) x (3 + $row_count + 4);
my @stats = (0) x ($row_count * 2);
$coins[0] = 0; # initialize with first coin

while (1) {
    my $active = 0;
    # if a coin falls through the bottom, count it
    $stats[$coins[-1] + $row_count]++ if defined $coins[-1];

    # move every coin down one row
    for my $line (reverse 1..(3+$row_count+3) ) {
        my $coinpos = $coins[$line - 1];

        #$coins[$line] = do if (! defined $coinpos) x
        if (! defined $coinpos) {
            $coins[$line] = undef
        } elsif (hits_peg($coinpos, $line)) {
            # when a coin from above hits a peg, it will bounce to either side.
            $active = 1;
            $coinpos += rand() < .5 ? -1 : 1;
            $coins[$line] = $coinpos
        } else {
            # if there was a coin above, it will fall to this position.
            $active = 1;
            $coins[$line] = $coinpos;
        }
    }
    # let the coin dispenser blink and turn it off if we run out of coins
    if (defined $coins[0]) {
        $coins[0] = undef;
    } elsif (--$coins > 0) {
        $coins[0] = 0
    }

    for (<0 1>) {
        display_board(\@coins, \@stats, $_);
        sleep .1;
    }
    exit unless $active;
}

sub display_board {
    my($p_ref, $s_ref, $halfstep) = @_;
    my @positions = @$p_ref;
    my @stats     = @$s_ref;
    my $coin      = $coin_icons[$halfstep];

    my @board = do {
        my @tmpl;

        sub out {
            my(@stuff) = split '', shift;
            my @line;
            push @line, ord($_) for @stuff;
            [@line];
        }

        push @tmpl, out("  " . " "x(2 * $row_count)) for 1..3;
        my @a = reverse 1..$row_count;
        my @b = 1..$row_count;
        my @pairs = pairwise { ($a, $b) } @a, @b;
        for ( pairs @pairs ) {
            my ( $spaces, $pegs ) = @$_;
            push @tmpl, out("  " . " "x$spaces . join(' ',($peg) x $pegs) . " "x$spaces);
        }
        push @tmpl, out("  " . " "x(2 * $row_count)) for 1..4;
        @tmpl;
    };

    my $midpos = $row_count + 2;

    our @output;
    {
        # collect all the output and output it all at once at the end
        sub printnl { my($foo) = @_; push @output, $foo . "\n" }
        sub printl  { my($foo) = @_; push @output, $foo        }

        # make some space above the picture
        printnl("") for 0..9;

        # place the coins
        for my $line (0..$#positions) {
            my $pos = $positions[$line];
            next unless defined $pos;
            $board[$line][$pos + $midpos] = ord($coin);
        }
        # output the board with its coins
        for my $line (@board) {
            printnl join '', map { chr($_) } @$line;
        }

        # show the statistics
        my $padding = 0;
        while (any {$_> 0} @stats) {
            $padding++;
            printl "  ";
            for my $i (0..$#stats) {
                if ($stats[$i] == 1) {
                        printl "\N{UPPER HALF BLOCK}";
                        $stats[$i]--;
                } elsif ($stats[$i] <= 0) {
                        printl " ";
                        $stats[$i] = 0
                } else {
                        printl "\N{FULL BLOCK}";
                        $stats[$i]--; $stats[$i]--;
                }
            }
            printnl("");
        }
        printnl("") for $padding..(10-1);
    }

    print join('', @output) . "\n";
}

sub hits_peg {
    my($x, $y) = @_;
    3 <= $y && $y < (3 + $row_count) and -($y - 2) <= $x && $x <= $y - 2
        ? not 0 == ($x - $y) % 2
        : 0
}
```

{{out}}

```txt
         ^
        ^ ^
       ^ ^ ^
      ^ ^ ^ ^
     ^ ^ ^ ^ ^
    ^ ^ ^ ^ ^ ^
   ^ ^ ^ ^ ^ ^ ^




    █ █ █ █ █ █
      █ █ █ █
      █ █ █ █
      █ █ █ █
      █ █ █ ▀
      ▀ █ █
        █

```



## Perl 6

[[File:Galton_box_perl6.gif|thumb|UPPER HALF BLOCK and LOWER HALF BLOCK alternate to give a somewhat smooth animation.]]
{{works with|rakudo|2015-09-12}}

```perl6
my $row-count = 6;
 
constant $peg = "*";
constant @coin-icons = "\c[UPPER HALF BLOCK]", "\c[LOWER HALF BLOCK]";
 
sub display-board(@positions, @stats is copy, $halfstep) {
    my $coin = @coin-icons[$halfstep.Int];
 
    state @board-tmpl = {
        # precompute a board
        my @tmpl;
        sub out(*@stuff) {
            @tmpl.push: $[@stuff.join>>.ords.flat];
        }
        # three lines of space above
        for 1..3 {
            out "  ", " " x (2 * $row-count);
        }
        # $row-count lines of pegs
        for flat ($row-count...1) Z (1...$row-count) -> $spaces, $pegs {
            out "  ", " " x $spaces, ($peg xx $pegs).join(" "), " " x $spaces;
        }
        # four lines of space below
        for 1..4 {
            out "  ", " " x (2 * $row-count);
        }
        @tmpl
    }();
 
    my $midpos = $row-count + 2;
 
    my @output;
    {
        # collect all the output and output it all at once at the end
        sub say(Str $foo) {
            @output.push: $foo, "\n";
        }
        sub print(Str $foo) {
            @output.push: $foo;
        }
 
        # make some space above the picture
        say "" for ^10;
 
        my @output-lines = map { [ @$_ ] }, @board-tmpl;
        # place the coins
        for @positions.kv -> $line, $pos {
            next unless $pos.defined;
            @output-lines[$line][$pos + $midpos] = $coin.ord;
        }
        # output the board with its coins
        for @output-lines -> @line {
            say @line.chrs;
        }
 
        # show the statistics
        my $padding = 0;
        while any(@stats) > 0 {
            $padding++;
            print "  ";
            @stats = do for @stats -> $stat {
                given $stat {
                    when 1 {
                        print "\c[UPPER HALF BLOCK]";
                        $stat - 1;
                    }
                    when * <= 0 {
                        print " ";
                        0
                    }
                    default {
                        print "\c[FULL BLOCK]";
                        $stat - 2;
                    }
                }
            }
            say "";
        }
        say "" for $padding...^10;
    }
    say @output.join("");
}
 
sub simulate($coins is copy) {
    my $alive = True;
 
    sub hits-peg($x, $y) {
        if 3 <= $y < 3 + $row-count and -($y - 2) <= $x <= $y - 2 {
            return not ($x - $y) %% 2;
        }
        return False;
    }
 
    my @coins = Int xx (3 + $row-count + 4);
    my @stats = 0 xx ($row-count * 2);
    # this line will dispense coins until turned off.
    @coins[0] = 0;
    while $alive {
        $alive = False;
        # if a coin falls through the bottom, count it
        given @coins[*-1] {
            when *.defined {
                @stats[$_ + $row-count]++;
            }
        }
 
        # move every coin down one row
        for ( 3 + $row-count + 3 )...1 -> $line {
            my $coinpos = @coins[$line - 1];
 
            @coins[$line] = do if not $coinpos.defined {
                Nil
            } elsif hits-peg($coinpos, $line) {
                # when a coin from above hits a peg, it will bounce to either side.
                $alive = True;
                ($coinpos - 1, $coinpos + 1).pick;
            } else {
                # if there was a coin above, it will fall to this position.
                $alive = True;
                $coinpos;
            }
        }
        # let the coin dispenser blink and turn it off if we run out of coins
        if @coins[0].defined {
            @coins[0] = Nil
        } elsif --$coins > 0 {
            @coins[0] = 0 
        }
 
        # smooth out the two halfsteps of the animation
        my $start-time;
        ENTER { $start-time = now }
        my $wait-time = now - $start-time;
 
        sleep 0.1 - $wait-time if $wait-time < 0.1;
        for @coin-icons.keys {
            sleep $wait-time max 0.1;
            display-board(@coins, @stats, $_);
        }
    }
}
 
sub MAIN($coins = 20, $peg-lines = 6) {
    $row-count = $peg-lines;
    simulate($coins);
}
```



## Phix

First, a console version:

```Phix
constant balls = 80
clear_screen()
sequence screen = repeat(repeat(' ',23),12)
                & repeat(join(repeat(':',12)),12)
                & {repeat('.',23)},
         Pxy = repeat({12,1},balls)
for peg=1 to 10 do
    screen[peg+2][13-peg..11+peg] = join(repeat('.',peg))
end for
puts(1,join(screen,"\n"))
text_color(BRIGHT_RED)
bool moved = true
integer top = ' '   -- (new drop every other iteration)
while moved or top!=' ' do
    moved = false
    for i=1 to balls do
        integer {Px,Py} = Pxy[i]
        if Py!=1 or top=' ' then
            integer Dx = 0, Dy = 0
            if screen[Py+1,Px]=' ' then     -- can vertical?
                Dy = 1
            else
                Dx = {-1,+1}[rand(2)]       -- try l;r or r;l
                if screen[Py+1,Px+Dx]!=' ' then Dx = -Dx end if
                if screen[Py+1,Px+Dx]==' ' then 
                    Dy = 1
                end if
            end if
            if Dy then
                position(Py,Px)  puts(1," ")  screen[Py,Px] = ' '
                Px += Dx
                Py += Dy
                position(Py,Px)  puts(1,"o")  screen[Py,Px] = 'o'
                Pxy[i] = {Px,Py}
                moved = true
                if Py=2 then top = 'o' end if
            end if
        end if
    end for
    position(26,1)
    sleep(0.2)
    if get_key()!=-1 then exit end if
    top = screen[2][12]
end while
```

{{out}}

```txt

           .o
          . .
         .o. .
        . . . .
       . .o. . .
      . . . . . .
     . . . . .o. .
    . . . . . . . .
   . . . . .o. . . .
  . . . . . . . . . .
: : : : :o: : : : : : :
: : : : : : : : : : : :
: : : : : : : : :o: : :
: : : : : : : : : : : :
: : : : : : : :o: : : :
: : : : : : : : : : : :
: : : : :o:o:o: : : : :
: : : : :o:o:o: : : : :
: : :o: :o:o:o: : : : :
: : : : :o:o:o:o: : : :
: : : :o:o:o:o:o: : : :
: : :o:o:o:o:o:o: : : :
.......................

```

Also, here is a slightly nicer and resize-able gui version:
{{libheader|pGUI}}

```Phix
--
-- demo\rosetta\GaltonBox.exw
--
constant TITLE = "Galton Box"

include pGUI.e

Ihandle dlg, canvas, timershow
cdCanvas cddbuffer, cdcanvas

integer brem = 80
sequence balls = {{0,1,0}}
sequence bins = repeat(0,8)

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    integer {w, h} = IupGetIntInt(canvas, "DRAWSIZE"), x, y
    atom xx, yy
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    -- draw the pins, then balls, then bins
    cdCanvasSetForeground(cddbuffer, CD_DARK_GREEN)
    integer pinsize = min(floor(h/40),floor(w/50))
    for y=4 to 16 by 2 do
        for x=-(y-4) to (y-4) by 4 do
            xx = w/2 + x*w/32
            yy = h -y*h/32
            cdCanvasSector(cddbuffer, xx, yy, pinsize, pinsize, 0, 360) 
        end for
    end for
    cdCanvasSetForeground(cddbuffer, CD_INDIGO)
    for i=1 to length(balls) do
        {x, y} = balls[i]
        xx = w/2 + x*w/32
        yy = h -y*h/32
        cdCanvasSector(cddbuffer, xx, yy, pinsize*4, pinsize*4, 0, 360) 
    end for
    cdCanvasLineWidth(cddbuffer,w/9)
    for i=1 to length(bins) do
        xx = w/2+(i*4-18)*w/32
        yy = bins[i]*h/64+10
        cdCanvasLine(cddbuffer,xx,10,xx,yy)
    end for
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function timer_cb(Ihandle ih)
integer x, y=9, dx
    if length(balls) then
        {x,y,dx} = balls[1]
        if y>20 then
            bins[(x+18)/4] += 1
            balls = balls[2..$]
        end if
    end if
    for i=1 to length(balls) do
        {x,y,dx} = balls[i]
        if y>15 then
            dx = 0
        elsif and_bits(y,1)=0 then
            dx = {-1,+1}[rand(2)]
        end if
        balls[i] = {x+dx,y+1,dx}
    end for
    if y>4 and brem!=0 then
        brem -= 1
        balls = append(balls,{0,1,0})
    end if
    if brem=0 and length(balls)=0 then
        IupSetAttribute(timershow,"RUN","NO")
    end if
    IupUpdate(canvas)
    return IUP_IGNORE
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_GREY)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "640x380")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    timershow = IupTimer(Icallback("timer_cb"), 80)

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", TITLE)
    IupSetCallback(dlg, "K_ANY", Icallback("esc_close"))

    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PicoLisp


```PicoLisp
(de galtonBox (Pins Height)
   (let (Bins (need (inc (* 2 Pins)) 0)  X 0  Y 0)
      (until (= Height (apply max Bins))
         (call 'clear)
         (cond
            ((=0 Y) (setq X (inc Pins)  Y 1))
            ((> (inc 'Y) Pins)
               (inc (nth Bins X))
               (zero Y) ) )
         ((if (rand T) inc dec) 'X)
         (for Row Pins
            (for Col (+ Pins Row 1)
               (let D (dec (- Col (- Pins Row)))
                  (prin
                     (cond
                        ((and (= X Col) (= Y Row)) "o")
                        ((and (gt0 D) (bit? 1 D)) ".")
                        (T " ") ) ) ) )
            (prinl) )
         (prinl)
         (for H (range Height 1)
            (for B Bins
               (prin (if (>= B H) "o" " ")) )
            (prinl) )
         (wait 200) ) ) )
```

Test:

```PicoLisp
(galtonBox 9 11)
```

{{Out}}

```txt
# Snapshot after a few seconds:
         .
        . .
       . . .
      . . . .
     . . . . .
    .o. . . . .
   . . . . . . .
  . . . . . . . .
 . . . . . . . . .









        o
        o o
      o o o o
```


```txt
# Final state:
         .
        . .
       . . .
      . . . .
     . . . . .
    . . . . . .
   . . . . . . .
  . . . . . . . .
 . . . . . . . . .

        o
        o
        o
        o
        o
      o o
      o o
      o o
      o o o
      o o o o o
    o o o o o o
```



## Prolog

Works with SWI-Prolog and XPCE.[[File:Prolog_Galton_Box_1.png|thumb|Sample display of Prolog solution]]

```Prolog
:- dynamic tubes/1.
:- dynamic balls/2.
:- dynamic stop/1.

% number of rows of pins (0 -> 9)
row(9).

galton_box :-
	retractall(tubes(_)),
	retractall(balls(_,_)),
	retractall(stop(_)),
	assert(stop(@off)),
	new(D, window('Galton Box')),
	send(D, size, size(520,700)),
	display_pins(D),
	new(ChTubes, chain),
	assert(tubes(ChTubes)),
	display_tubes(D, ChTubes),
	new(Balls, chain),
	new(B, ball(D)),
	send(Balls, append, B),
	assert(balls(Balls, D)),
	send(D, done_message, and(message(ChTubes, clear),
				  message(ChTubes, free),
				  message(Balls, for_all, message(@arg1, free)),
				  message(Balls, clear),
				  message(Balls, free),
				  message(@receiver, destroy))),
	send(D, open).

% class pin, balls travel between pins
:- pce_begin_class(pin, circle, "pin").

initialise(P, Pos) :->
	send(P, send_super, initialise, 18),
	send(P, fill_pattern, new(_, colour(@default, 0, 0, 0))),
	get(Pos, x, X),
	get(Pos, y, Y),
	send(P, geometry, x := X, y := Y).
:- pce_end_class.


% class tube, balls fall in them
:- pce_begin_class(tube, path, "tube where balls fall").

variable(indice, any, both, "index of the tube in the list").
variable(balls, any, both, "number of balls inside").

initialise(P, Ind, D) :->
	row(Row),
	send(P, send_super, initialise, kind := poly),
	send(P, slot, balls, 0),
	send(P, slot, indice, Ind),
	X0 is 228 - Row * 20 + Ind * 40,
	X1 is X0 + 20,
	Y1 is 600, Y0 is 350,
	send_list(P, append, [point(X0, Y0), point(X0, Y1),
			      point(X1,Y1), point(X1,Y0)]),
	send(D, display, P).

% animation stop when a tube is full
add_ball(P) :->
	get(P, slot, balls, B),
	B1 is B+1,
	send(P, slot, balls, B1),
	(   B1 = 12
	->  retract(stop(_)), assert(stop(@on))
	;   true).
:- pce_end_class.


% class ball
:- pce_begin_class(ball, circle, "ball").

variable(angle, any, both, "angle of the ball with the pin").
variable(dir, any, both, "left / right").
variable(pin, point, both, "pin under the ball when it falls").
variable(position, point, both, "position of the ball").
variable(max_descent, any, both, "max descent").
variable(state, any, both, "in_pins / in_tube").
variable(window, any, both, "window to display").
variable(mytimer, timer, both, "timer of the animation").

initialise(P, W) :->
	send(P, send_super, initialise, 18),
	send(P, pen, 0),
	send(P, state, in_pins),
	send(P, fill_pattern, new(_, colour(@default, 65535, 0, 0))),
	Ang is 3 * pi / 2,
	send(P, slot, angle, Ang),
	send(P, slot, window, W),
	send(P, geometry, x := 250, y := 30),
	send(P, pin, point(250, 50)),
	send(P, choose_dir),
	send(P, mytimer, new(_, timer(0.005, message(P, move_ball)))),
	send(W, display, P),
	send(P?mytimer, start).

% method called when the object is destroyed
% first the timer is stopped
% then all the resources are freed
unlink(P) :->
	send(P?mytimer, stop),
	send(P, send_super, unlink).

choose_dir(P) :->
	I is random(2),
	(   I = 1 -> Dir = left; Dir = right),
	send(P, dir, Dir).

move_ball(P) :->
	get(P, state, State),
	(   State = in_pins
	->  send(P, move_ball_in_pins)
	;   send(P, move_ball_in_tube)).

move_ball_in_pins(P) :->
	get(P, slot, angle, Ang),
	get(P, slot, pin, Pin),
	get(P, slot, dir, Dir),
	(   Dir = left -> Ang1 is Ang-0.15 ; Ang1 is Ang + 0.15),
	get(Pin, x, PX),
	get(Pin, y, PY),
	X is 21 * cos(Ang1) +  PX,
	Y is 21 * sin(Ang1) + PY,
	send(P, geometry, x := X, y := Y),
	send(P?window, display, P),
	(   abs(Ang1 - pi) < 0.1
	->  PX1 is PX - 20,
	    send(P, next_move, PX1, PY)
	;   abs(Ang1 - 2 * pi) < 0.1
	->  PX1 is PX + 20,
	    send(P, next_move, PX1, PY)
	;   send(P, slot, angle, Ang1)).

next_move(P, PX, PY) :->
	row(Row),

	    Ang2 is 3 * pi / 2,
	    PY1 is PY + 30,
	    (	PY1 =:= (Row + 1) * 30 + 50
	    ->	send(P, slot, state, in_tube),
		NumTube is round((PX - 228 + Row * 20) / 40),
		tubes(ChTubes),
		get(ChTubes, find,
		    message(@prolog, same_value,@arg1?indice, NumTube),
		    Tube),
		send(Tube, add_ball),
		get(Tube, slot, balls, Balls),
		Max_descent is 600 - Balls * 20,
		send(P, slot, max_descent, Max_descent),
		send(P, slot, position, point(PX, PY))
	    ;	send(P, choose_dir),
		send(P, slot, angle, Ang2),
		send(P, slot, pin, point(PX, PY1))).

move_ball_in_tube(P) :->
	get(P, slot, position, Descente),
	get(Descente, x, PX1),
	get(Descente, y, PY),
	PY1 is PY+4,
	send(P, geometry, x := PX1, y := PY1),
	get(P, slot, max_descent, Max_descent),
	(   Max_descent =< PY1
	->  send(P?mytimer, stop),
	    (	stop(@off) ->  send(@prolog, next_ball); true)
	;   send(P, slot, position, point(PX1, PY1))),
	send(P?window, display, P).

:- pce_end_class.


next_ball :-
	retract(balls(Balls, D)),
	new(B, ball(D)),
	send(Balls, append, B),
	assert(balls(Balls, D)).

% test to find the appropriate tube
same_value(V, V).

display_pins(D) :-
	row(Row),
	forall(between(0, Row, I),
	       (  Start is 250 - I * 20,
		  Y is I * 30 + 50,
	          forall(between(0, I, J),
			 (   X is Start + J * 40,
			     new(P, pin(point(X,Y))),
			     send(D, display, P))))).

display_tubes(D, Ch) :-
	row(Row),
	Row1 is Row+1,
	forall(between(0, Row1, I),
	       (   new(T, tube(I, D)),
		   send(Ch, append, T),
		   send(D, display, T))).

```



## PureBasic

{{trans|Unicon}}
[[File:PureBasic_galtonbox.png|thumb|Sample display of PureBasic solution]]

```purebasic
Global pegRadius, pegSize, pegSize2, height, width, delay, histogramSize, ball

Procedure eventLoop()
  Protected event
  Repeat
    event = WindowEvent()
    If event = #PB_Event_CloseWindow
      End
    EndIf
  Until event = 0 
EndProcedure
 
Procedure animate_actual(x1, y1, x2, y2, steps)
  Protected x.f, y.f, xstep.f, ystep.f, i, lastX.f, lastY.f
  x = x1
  y = y1
  xstep = (x2 - x1)/steps
  ystep = (y2 - y1)/steps
  For i = 1 To steps
    lastX = x
    lastY = y
    StartDrawing(CanvasOutput(0))
      DrawingMode(#PB_2DDrawing_XOr)
      Circle(x, y, pegRadius, RGB(0, 255, 255))
    StopDrawing()
    eventLoop()
    Delay(delay)      ; wait in ms
    StartDrawing(CanvasOutput(0))
      DrawingMode(#PB_2DDrawing_XOr)
      Circle(x, y, pegRadius, RGB(0, 255, 255))
    StopDrawing()
    eventLoop()
    x + xstep
    y + ystep
  Next
EndProcedure

Procedure draw_ball(xpos, ypos)                      
  Static Dim ballcounts(0) ;tally drop positions
  If xpos > ArraySize(ballcounts())
    Redim ballcounts(xpos)
  EndIf 
  ballcounts(xpos) + 1
  animate_actual(xpos, ypos, xpos, height - ballcounts(xpos) * pegSize, 20)
  StartDrawing(CanvasOutput(0))
    Circle(xpos, height - ballcounts(xpos) * pegSize, pegRadius, RGB(255, 0, 0))
  StopDrawing()
  eventLoop()
  If ballcounts(xpos) <= histogramSize
    ProcedureReturn 1 
  EndIf
  SetWindowTitle(0, "Ended after " + Str(ball) + " balls") ;histogramSize exceeded
EndProcedure 

Procedure animate(x1, y1, x2, y2)
  animate_actual(x1, y1, x2, y1, 4)
  animate_actual(x2, y1, x2, y2, 10)
EndProcedure 

Procedure galton(pegRows)
  ;drop a ball into the galton box
  Protected xpos, ypos, i, oldX, oldY

  oldX = width / 2 - pegSize / 2
  xpos = oldX
  oldY = pegSize
  ypos = oldY
  animate_actual(oldX, 0, xpos, ypos, 10)
  For i = 1 To pegRows 
    If Random(1)
      xpos + pegSize 
    Else
      xpos - pegSize
    EndIf 
    ypos + pegSize2
    animate(oldX, oldY, xpos, ypos)
    oldX = xpos
    oldY = ypos
  Next
  
  ProcedureReturn draw_ball(xpos, ypos)
EndProcedure

Procedure setup_window(numRows, ballCount)  
  ;Draw numRows levels of pegs
  Protected xpos, ypos, i, j
  
  width = (2 * numRows + 2) * pegSize
  histogramSize = (ballCount + 2) / 3
  If histogramSize > 500 / pegSize: histogramSize = 500 / pegSize: EndIf
  height = width + histogramSize * pegSize
  OpenWindow(0, 0, 0, width, height, "Galton box animation", #PB_Window_SystemMenu)
  CanvasGadget(0, 0, 0, width, height) 
  
  StartDrawing(CanvasOutput(0))
    Box(0, 0, width, height, RGB($EB, $EB, $EB))
    For i = 1 To numRows
      ypos = i * pegSize2
      xpos = width / 2 - (i - 1) * pegSize - pegSize / 2
      For j = 1 To i
        Circle(xpos, ypos, pegRadius, RGB(0, 0, 255))
        xpos + pegSize2
      Next
    Next
    For i = 1 To numRows
      Line((numRows - i + 1) * pegSize2 - pegSize / 2, width - pegSize, 1, histogramSize * pegSize, 0)
    Next 
  StopDrawing()
EndProcedure

;based on the galton box simulation from Unicon book
Define pegRows = 10, ballCount
pegRadius = 4
pegSize = pegRadius * 2 + 1
pegSize2 = pegSize * 2 
delay = 2                      ; ms delay

Repeat
  ballCount = Val(InputRequester("Galton box simulator","How many balls to drop?", "100"))
Until ballCount > 0

setup_window(pegRows, ballCount)
eventLoop()
For ball = 1 To ballCount
  If Not galton(pegRows): Break: EndIf
Next
Repeat: eventLoop(): ForEver
```


## Python


```Python
#!/usr/bin/python

import sys, os
import random
import time

def print_there(x, y, text):
     sys.stdout.write("\x1b7\x1b[%d;%df%s\x1b8" % (x, y, text))
     sys.stdout.flush()


class Ball():
    def __init__(self):
        self.x = 0
        self.y = 0
        
    def update(self):
        self.x += random.randint(0,1)
        self.y += 1

    def fall(self):
        self.y +=1


class Board():
    def __init__(self, width, well_depth, N):
        self.balls = []
        self.fallen = [0] * (width + 1)
        self.width = width
        self.well_depth = well_depth
        self.N = N
        self.shift = 4
        
    def update(self):
        for ball in self.balls:
            if ball.y < self.width:
                ball.update()
            elif ball.y < self.width + self.well_depth - self.fallen[ball.x]:
                ball.fall()
            elif ball.y == self.width + self.well_depth - self.fallen[ball.x]:
                self.fallen[ball.x] += 1
            else:
                pass
                
    def balls_on_board(self):
        return len(self.balls) - sum(self.fallen)
                
    def add_ball(self):
        if(len(self.balls) <= self.N):
            self.balls.append(Ball())

    def print_board(self):
        for y in range(self.width + 1):
            for x in range(y):
                print_there( y + 1 ,self.width - y + 2*x + self.shift + 1, "#")
    def print_ball(self, ball):
        if ball.y <= self.width:
            x = self.width - ball.y + 2*ball.x + self.shift
        else:
            x = 2*ball.x + self.shift
        y = ball.y + 1
        print_there(y, x, "*")
         
    def print_all(self):
        print(chr(27) + "[2J")
        self.print_board();
        for ball in self.balls:
            self.print_ball(ball)


def main():
    board = Board(width = 15, well_depth = 5, N = 10)
    board.add_ball() #initialization
    while(board.balls_on_board() > 0):
         board.print_all()
         time.sleep(0.25)
         board.update()
         board.print_all()
         time.sleep(0.25)
         board.update()
         board.add_ball()


if __name__=="__main__":
    main()
```



## Racket

This does not use the default #lang racket.  Required is advanced student with teachpacks universe and image.
Multiple balls are added each step, but they do not collide.


```Racket

;a ball's position...row is a natural number and col is an integer where 0 is the center
(define-struct pos (row col))
;state of simulation...list of all positions and vector of balls (index = bin)
(define-struct st (poss bins))
;increment vector @i
(define (vector-inc! v i) (vector-set! v i (add1 (vector-ref v i))))

(define BALL-RADIUS 6)
;for balls to fit perfectly between diamond-shaped pins, the side length is
;determined by inscribing the diamond in the circle
(define PIN-SIDE-LENGTH (* (sqrt 2) BALL-RADIUS))
;ultimate pin width and height
(define PIN-WH (* 2 BALL-RADIUS))
(define PIN-HOR-SPACING (* 2 PIN-WH))
;vertical space is the height of an equilateral triangle with side length = PIN-HOR-SPACING
(define PIN-VER-SPACING (* 1/2 (sqrt 3) PIN-HOR-SPACING))
;somewhat copying BASIC256's graphics
;determines how thick the outline will be
(define FILL-RATIO 0.7)
;freeze is a function that converts the drawing code into an actual bitmap forever
(define PIN (freeze (overlay (rotate 45 (square (* FILL-RATIO PIN-SIDE-LENGTH) "solid" "purple"))
                             (rotate 45 (square PIN-SIDE-LENGTH "solid" "magenta")))))
(define BALL (freeze (overlay (circle (* FILL-RATIO BALL-RADIUS) "solid" "green")
                              (circle BALL-RADIUS "solid" "dark green"))))
(define BIN-COLOR (make-color 255 128 192))
;# balls bin can fit
(define BIN-CAPACITY 10)
(define BIN-HEIGHT (* BIN-CAPACITY PIN-WH))
(define BIN (freeze (beside/align "bottom"
                                  (line 0 BIN-HEIGHT BIN-COLOR)
                                  (line PIN-WH 0 BIN-COLOR)
                                  (line 0 BIN-HEIGHT BIN-COLOR))))

(define draw-background
  (let ([background #f])
    (λ (height)
      (if (image? background)
          background
          (let* ([w (+ (image-width BIN) (* PIN-HOR-SPACING height))]
                 [h (+ PIN-WH (image-height BIN) (* PIN-VER-SPACING height))]
                 [draw-background (λ () (rectangle w h "solid" "black"))])
            (begin (set! background (freeze (draw-background))) background))))))

;draws images using x horizontal space between center points
(define (spaced/x x is)
  (if (null? is)
      (empty-scene 0 0)
      (let spaced/x ([n 1] [i (car is)] [is (cdr is)])
        (if (null? is)
            i
            (overlay/xy i (* -1 n x) 0 (spaced/x (add1 n) (car is) (cdr is)))))))
  
(define (draw-pin-row r) (spaced/x PIN-HOR-SPACING (make-list (add1 r) PIN)))

;draws all pins, using saved bitmap for efficiency
(define draw-pins
  (let ([bmp #f])
    (λ (height)
      (let ([draw-pins (λ () (foldl (λ (r i) (overlay/align/offset
                                              ;vertically line up all pin rows
                                              "center" "bottom" (draw-pin-row r)
                                              ;shift down from the bottom of accum'ed image by ver spacing
                                              0 (- PIN-VER-SPACING) i))
                                    (draw-pin-row 0) (range 1 height 1)))])
        (if (image? bmp)
            bmp
            (begin (set! bmp (freeze (draw-pins))) bmp))))))

(define (draw-ball p i)
  ;the ball starts at the top of the image
  (overlay/align/offset "center" "top" BALL (* -1 (pos-col p) PIN-WH) (* -1 (pos-row p) PIN-VER-SPACING) i))

;bin has balls added from bottom, stacked exactly on top of each other
;the conditional logic is needed because above can't handle 0 or 1 things
(define (draw-bin n)
  (if (zero? n)
      BIN
      (overlay/align "center" "bottom"
                     (if (= n 1) BALL (apply above (make-list n BALL)))
                     BIN)))

;main drawing function
(define (draw height s)
  (let* ([bins (spaced/x PIN-HOR-SPACING (map draw-bin (vector->list (st-bins s))))]
         ;pins above bins
         [w/pins (above (draw-pins height) bins)]
         ;draw this all one ball diameter (PIN-WH) below top
         [w/background (overlay/align/offset "center" "top" w/pins
                                             0 (- PIN-WH) (draw-background height))])
    ;now accumulate in each ball
    (foldl draw-ball w/background (st-poss s))))

;a ball moves down by increasing its row and randomly changing its col by -1 or 1
(define (next-row height p)
  (make-pos (add1 (pos-row p))
            (+ -1 (* 2 (random 2)) (pos-col p))))

;each step, every ball goes to the next row and new balls are added at the top center
;balls that fall off go into bins
(define (tock height s)
  (let* ([new-ps (map (λ (p) (next-row height p)) (st-poss s))]
         ;live balls haven't gone past the last row of pins
         [live (filter (λ (p) (< (pos-row p) height)) new-ps)]
         ;dead balls have (partition from normal Racket would be useful here...)
         [dead (filter (λ (p) (>= (pos-row p) height)) new-ps)]
         ;adjust col from [-x,x] to [0,2x]
         [bin-indices (map (λ (p) (quotient (+ (pos-col p) height) 2)) dead)])
    ;add new balls to the live balls
    (make-st (append (make-list (random 4) (make-pos 0 0)) live)
             ;sum dead ball positions into bins
             (begin (for-each (λ (i) (vector-inc! (st-bins s) i)) bin-indices)
                    (st-bins s)))))

;run simulation with empty list of positions to start, stepping with "tock" and drawing with "draw"
(define (run height)
  (big-bang (make-st '() (make-vector (add1 height) 0))
            (on-tick (λ (ps) (tock height ps)) 0.5)
            (to-draw (λ (ps) (draw height ps)))))

```



## REXX

The REXX version displays an ASCII version of a working Galton box.

Balls are dropped continuously   (up to a number specified or the default),   the default is enough rows of

pins to fill the top   <big><sup>1</sup>/<sub>3</sub></big>   rows of the terminal screen.

Extra code could've been added to check for which OS (operating system) is being used and which REXX is 

running to determine which program to clear the terminal (screen).   Currently, it is 
hard-coded to   '''CLS'''.

```rexx
/*REXX pgm simulates Sir Francis Galton's box, aka: Galton Board, quincunx, bean machine*/
if !all(arg())  then exit                        /*Any documentation was wanted?   Done.*/
if !cms         then address ''                  /*handle ADDRESS for CMS oper. system. */
trace off                                        /*suppress error messages from a HALT. */
signal on halt                                   /*allow the user to  halt  the program.*/
parse arg rows balls freeze seed .               /*obtain optional arguments from the CL*/
if rows ==''  |  rows==","   then   rows=   0    /*Not specified?  Then use the default.*/
if balls==''  | balls==","   then  balls= 100    /* "      "         "   "   "     "    */
if freeze=='' | freeze==","  then freeze=   0    /* "      "         "   "   "     "    */
if datatype(seed, 'W')   then call random ,,seed /*Was a seed specified?  Then use seed.*/
parse value  scrsize()  with  sd  sw  .          /*obtain the terminal depth and width. */
if sd==0  then sd= 40                            /*Not defined by the OS?  Use a default*/
if sw==0  then sw= 80                            /* "     "     "  "   "    "  "    "   */
sd= sd - 3                                       /*define the usable       screen depth.*/
sw= sw - 1;  if sw//2  then sw= sw -1            /*   "    "    "     odd     "   width.*/
if rows==0  then rows= (sw - 2 ) % 3             /*pins are on the first third of screen*/
pin  = '·';            ball = '☼'                /*define chars for a  pin  and a  ball.*/
call gen                                         /*gen a triangle of pins with some rows*/
call run                                         /*simulate a Galton box with some balls*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen: @.=;  do r=1  for rows;          $=         /*build a triangle of pins for the box.*/
           if r//2  then iterate                 /* [↑]  an empty odd row (with no pins)*/
                do pins=1  for r%2;   $= $  pin;  end  /*pins*/   /*build a row of pins.*/
           @.r= center( strip($, 'T'), sw)       /*an easy method to build a triangle.  */
           end     /*r*/;     #= 0;       return /*#:   is the number of balls dropped. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
drop: static= 1                                  /*used to indicate all balls are static*/
        do c=sd-1  by -1  to 1;        n= c + 1  /*D:  current row;   N:  the next row. */
        x= pos(ball, @.c);             y= x - 1  /*X:  position of a ball on the C line.*/
        if x==0  then iterate                    /*No balls here?  Then nothing to drop.*/
          do forever;   y= pos(ball, @.c, y+1)   /*drop most balls down to the next row.*/
          if y==0  then iterate c                /*down with this row, go look at next. */
          z= substr(@.n, y, 1)                   /*another ball is blocking this fall.  */
          if z==' '  then do;  @.n= overlay(ball, @.n, y)   /*drop a ball straight down.*/
                               @.c= overlay(' ' , @.c, y)   /*make current ball a ghost.*/
                               static=0                     /*indicate balls are moving.*/
                               iterate                      /*go keep looking for balls.*/
                          end
          if z==pin  then do;  ?= random(,999);   d= -1     /*assume falling to the left*/
                                    if ?//2  then d=  1     /*if odd random#, fall right*/
                               if substr(@.n, y+d, 1)\==' '  then iterate /*blocked fall*/
                               @.n= overlay(ball, @.n, y+d)
                               @.c= overlay(' ' , @.c, y  )
                               static=0                     /*indicate balls are moving.*/
                               iterate                      /*go keep looking for balls.*/
                          end
          end   /*forever*/
        end     /*c*/                  /* [↓]   step//2    is used to avoid collisions. */
                                                            /* [↓]  drop a new ball ?   */
     if #<balls & step//2  then do;    @.1= center(ball, sw+1);      # = # + 1;        end
                           else if static  then exit 2      /*insure balls are static.  */
     return
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: !cls;                    info= right(" step" step, sw%4)       /*title for screen.*/
               do g=sd  by -1  until @.g\=''                         /*G: last data row.*/
               end   /*g*/                                           /* [↓]  show a row.*/
        do r=1  for g;  _= strip(@.r, 'T');  if r==2  then _= _ left('', 9)  info;   say _
        end   /*r*/;    if step==freeze  then do;   say 'press ENTER ···';    pull;    end
      return
/*══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
run:       do step=1;  call drop;  call show;  end  /*step*/    /*'til run out of balls.*/
halt: say '***warning***  REXX program'    !fn     "execution halted by user.";     exit 1
!all:  !!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:  if symbol('!CALL')\=="VAR"  then !call=;  return !call
!env:  !env='ENVIRONMENT';  if !sys=='MSDOS' | !brexx | !r4 | !roo  then !env= 'SYSTEM';  if !os2  then !env= 'OS2'!env;  !ebcdic= 3=='f3'x;  if !crx  then !env= 'DOS';  return
!fid:  parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;  call !sys;   if !dos  then do;  _= lastpos('\', !fn);  !fm= left(!fn, _);  !fn= substr(!fn, _+1);  parse var !fn !fn '.' !ft;  end;   return word(0 !fn !ft !fm, 1 + ('0'arg(1) ) )
!rex:  parse upper version !ver !vernum !verdate .; !brexx= 'BY'==!vernum; !kexx= 'KEXX'==!ver; !pcrexx= 'REXX/PERSONAL'==!ver | 'REXX/PC'==!ver; !r4= 'REXX-R4'==!ver; !regina= 'REXX-REGINA'==left(!ver, 11); !roo= 'REXX-ROO'==!ver; call !env;  return
!sys:  !cms= !sys=='CMS';  !os2= !sys=='OS2';  !tso= !sys=='TSO' | !sys=='MVS';  !vse= !sys=='VSE';  !dos= pos('DOS', !sys)\==0 | pos('WIN', !sys)\==0 | !sys=='CMD';  !crx= left(!sys, 6)=='DOSCRX';  call !rex;  return
!var:  call !fid;  if !kexx  then return space( dosenv( arg(1) ) );              return space( value( arg(1), , !env) )
```

Programming note:   the last seven lines of this REXX program are some general purpose (boilerplate code) that, among other things, finds:
::*   the REXX program's filename, filetype (file extension), and filemode (and/or path)
::*   if the user wants documentation presented (not germane to this REXX program) 
::*   the environment name (not germane)
::*   what command to be used to clear the terminal screen
::*   the name of the operating system being used (not germane)
::*   the name of the REXX interpreter being used (not germane)
::*   various other bits of information (not germane)

It is only intended to be used to make this particular REXX program independent of any particular REXX interpreter and/or independent of knowing which program is to be used for clearing the terminal screen.   As such, the boilerplate code isn't commented and isn't intended to be a teaching tool. 

This REXX program makes use of   '''SCRSIZE'''   REXX program (or
BIF) which is used to determine the screen

width and depth of the terminal (console).    Some REXXes don't have this BIF.

The   '''SCRSIZE.REX'''   REXX program is included here   ───►   [[SCRSIZE.REX]].

The terminal size used for this display was   '''64'''<small>x</small>'''96'''.

{{out|output|text=  when the REXX program was "stopped" by using the inputs of     <tt> , , 100 </tt>   so as to freeze the program to capture a screenshot:}}

```txt

                                              ☼·                          step 100

                                              ·☼·

                                             · ·☼·

                                            · ·☼· ·

                                          ☼· · · · ·

                                          · · ·☼· · ·

                                         ·☼· · · · · ·

                                        · · · · ·☼· · ·

                                       · · · · · · ·☼· ·

                                      · · ·☼· · · · · · ·

                                     · · · ·☼· · · · · · ·

                                    · · · ·☼· · · · · · · ·

                                   · · ·☼· · · · · · · · · ·

                                  · · · · · · · ·☼· · · · · ·

                                 · · · · · · ·☼· · · · · · · ·

                                                ☼

                                                  ☼

                                                ☼

                                            ☼

                                                  ☼

                                                      ☼

                                                        ☼

                                                ☼

                                      ☼

                                              ☼

                                                    ☼

                                            ☼

                                            ☼

                                            ☼ ☼   ☼
                                            ☼ ☼   ☼
                                      ☼   ☼ ☼ ☼ ☼ ☼     ☼
                                      ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼   ☼
press ENTER ···

```

{{out|output|text=  when using the above inputs;   this is the final screenshot:}}

```txt

                                               ·                          step 350

                                              · ·

                                             · · ·

                                            · · · ·

                                           · · · · ·

                                          · · · · · ·

                                         · · · · · · ·

                                        · · · · · · · ·

                                       · · · · · · · · ·

                                      · · · · · · · · · ·

                                     · · · · · · · · · · ·

                                    · · · · · · · · · · · ·

                                   · · · · · · · · · · · · ·

                                  · · · · · · · · · · · · · ·

                                 · · · · · · · · · · · · · · ·

                                            ☼
                                            ☼ ☼
                                            ☼ ☼
                                            ☼ ☼ ☼
                                            ☼ ☼ ☼
                                            ☼ ☼ ☼
                                            ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                            ☼ ☼ ☼ ☼
                                          ☼ ☼ ☼ ☼ ☼ ☼
                                          ☼ ☼ ☼ ☼ ☼ ☼
                                          ☼ ☼ ☼ ☼ ☼ ☼
                                        ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼
                                        ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼
                                        ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼
                                      ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼
                                      ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼
                                      ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼
                                      ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼ ☼ 

```



## Ruby

{{libheader|Shoes}}
[[File:galtonbox.shoes.png|thumb|Sample display of Ruby solution]]

```ruby
$rows_of_pins = 12
$width = $rows_of_pins * 10 + ($rows_of_pins+1)*14

Shoes.app(
    :width => $width + 14,
    :title => "Galton Box"
) do
  @bins = Array.new($rows_of_pins+1, 0)

  @x_coords = Array.new($rows_of_pins) {Array.new}
  @y_coords = Array.new($rows_of_pins)
  stack(:width => $width) do
    stroke gray
    fill gray
    1.upto($rows_of_pins) do |row|
      y = 14 + 24*row
      @y_coords[row-1] = y
      row.times do |i|
        x = $width / 2 + (i - 0.5*row)*24 + 14
        @x_coords[row-1] << x
        oval x+2, y, 6
      end
    end
  end
  @y_coords << @y_coords[-1] + 24
  @x_coords << @x_coords[-1].map {|x| x-12} + [@x_coords[-1][-1]+12]

  @balls = stack(:width => $width) do
    stroke red
    fill red
  end.move(0,0)

  @histogram = stack(:width => $width) do
    nostroke
    fill black
  end.move(0, @y_coords[-1] + 10)

  @paused = false
  keypress do |key|
    case key
    when "\x11", :control_q
      exit
    when "\x10", :control_p
      @paused = !@paused
    end
  end

  @ball_row = 0
  @ball_col = 0
  animate(2*$rows_of_pins) do
    if not @paused
      y = @y_coords[@ball_row] - 12
      x = @x_coords[@ball_row][@ball_col]
      @balls.clear {oval x, y, 10}
      @ball_row += 1
      if @ball_row <= $rows_of_pins
        @ball_col += 1 if rand >= 0.5
      else
        @bins[@ball_col] += 1
        @ball_row = @ball_col = 0
        update_histogram
      end
    end
  end

  def update_histogram
    y = @y_coords[-1] + 10
    @histogram.clear do
      @bins.each_with_index do |num, i|
        if num > 0
          x = @x_coords[-1][i]
          rect x-6, 0, 24, num
        end
      end
    end
  end
end
```



## Tcl

{{trans|C}}

```tcl
package require Tcl 8.6

oo::class create GaltonBox {
    variable b w h n x y cnt step dropping

    constructor {BALLS {NUMPEGS 5} {HEIGHT 24}} {
	set n $NUMPEGS
	set w [expr {$n*2 + 1}]
	set h $HEIGHT
	puts -nonewline "\033\[H\033\[J"
	set x [set y [lrepeat $BALLS 0]]
	set cnt 0
	set step 0
	set dropping 1

	set b [lrepeat $h [lrepeat $w " "]]
	for {set i 0} {$i < $n} {incr i} {
	    for {set j [expr {-$i}]} {$j <= $i} {incr j 2} {
		lset b [expr {2*$i+2}] [expr {$j+$w/2}] "*"
	    }
	}
    }

    method show {} {
	puts -nonewline "\033\[H"
	set oldrow {}
	foreach row $b {
	    foreach char $row oldchar $oldrow {
		if {$char ne "*"} {
		    puts -nonewline "$char "
		} elseif {$oldchar eq " "} {
		    puts -nonewline "\033\[32m*\033\[m "
		} else {
		    puts -nonewline "\033\[31m*\033\[m "
		}
	    }
	    set oldrow $row
	    puts ""
	}
    }

    method Move idx {
	set xx [lindex $x $idx]
	set yy [lindex $y $idx]
	set kill 0

	if {$yy < 0} {return 0}
	if {$yy == $h-1} {
	    lset y $idx -1
	    return 0
	}

	switch [lindex $b [incr yy] $xx] {
	    "*" {
		incr xx [expr {2*int(2 * rand()) - 1}]
		if {[lindex $b [incr yy -1] $xx] ne " "} {
		    set dropping 0
		}
	    }
	    "o" {
		incr yy -1
		set kill 1
	    }
	}

	set c [lindex $b [lindex $y $idx] [lindex $x $idx]]
	lset b [lindex $y $idx] [lindex $x $idx] " "
	lset b $yy $xx $c
	if {$kill} {
	    lset y $idx -1
	} else {
	    lset y $idx $yy
	}
	lset x $idx $xx
	return [expr {!$kill}]
    }

    method step {} {
	set moving 0
	for {set i 0} {$i < $cnt} {incr i} {
	    set moving [expr {[my Move $i] || $moving}]
	}
	if {2 == [incr step] && $cnt < [llength $x] && $dropping} {
	    set step 0
	    lset x $cnt [expr {$w / 2}]
	    lset y $cnt 0
	    if {[lindex $b [lindex $y $cnt] [lindex $x $cnt]] ne " "} {
		return 0
	    }
	    lset b [lindex $y $cnt] [lindex $x $cnt] "o"
	    incr cnt
	}
	return [expr {($moving || $dropping)}]
    }
}

GaltonBox create board 1024 {*}$argv
while true {
    board show
    if {[board step]} {after 60} break
}
```

After a sample run with input parameters <tt>10 55</tt>:

```txt

                                          
                                          
                    *                     
                                          
                  *   *                   
                                          
                *   *   *                 
                                          
              *   *   *   *               
                                          
            *   *   *   *   *             
                                          
          *   *   *   *   *   *           
                                          
        *   *   *   *   *   *   *         
                                          
      *   *   *   *   *   *   *   *       
                                          
    *   *   *   *   *   *   *   *   *     
                    o   o                 
  *   *   *   *   * o * o *   *   *   *   
                    o   o                 
                    o   o                 
                    o   o                 
                    o   o                 
                    o   o                 
                    o   o                 
                    o   o                 
                    o   o                 
                    o   o                 
            o       o   o                 
            o       o   o                 
            o       o   o                 
            o       o   o                 
            o       o   o                 
            o   o   o   o   o             
            o   o   o   o   o             
            o   o   o   o   o             
            o   o   o   o   o             
            o   o   o   o   o             
            o   o   o   o   o             
            o   o   o   o   o             
            o   o   o   o   o             
            o   o   o   o   o             
            o   o   o   o   o             
            o   o   o   o   o             
            o   o   o   o   o             
            o   o   o   o   o             
        o   o   o   o   o   o             
        o   o   o   o   o   o   o         
    o   o   o   o   o   o   o   o         
    o   o   o   o   o   o   o   o         
    o   o   o   o   o   o   o   o         
    o   o   o   o   o   o   o   o         
    o   o   o   o   o   o   o   o         

```

There is a much more comprehensive solution to this on the [http://wiki.tcl.tk/8825 Tcler's Wiki].<!-- Too long to reproduce here -->


## XPL0


This ''Peeks'' into some IBM-PC specific locations and hence is not entirely portable.


```XPL0
include c:\cxpl\codes;          \intrinsic code declarations
define  Balls = 80;             \maximum number of balls
int     Bx(Balls), By(Balls),   \character cell coordinates of each ball
        W, I, J, Peg, Dir;
[W:= Peek($40, $4A);            \get screen width in characters
Clear;  CrLf(6);  CrLf(6);
for Peg:= 1 to 10 do                            \draw pegs
        [for I:= 1 to 12-Peg do ChOut(6,^ );    \space over to first peg
         for I:= 1 to Peg do [ChOut(6,^.);  ChOut(6,^ )];
        CrLf(6);
        ];
for J:= 0 to 12-1 do                            \draw slots
        [for I:= 0 to 12-1 do [ChOut(6,^:);  ChOut(6,^ )];
        CrLf(6);
        ];
for I:= 0 to 23-1 do ChOut(6,^.);               \draw bottom
for I:= 0 to Balls-1 do                         \make source of balls at top
        [Bx(I):= 11;  By(I):= 1];
Attrib($C);                                     \make balls bright red
repeat                                          \balls away! ...
    for I:= 0 to Balls-1 do                     \for all the balls ...
        [Cursor(Bx(I), By(I));  ChOut(6, ^ );   \erase ball's initial position
        if Peek($B800, (Bx(I)+(By(I)+1)*W)*2) = ^ \is ball above empty location?
        then    By(I):= By(I)+1                 \yes: fall straight down
        else    [Dir:= Ran(3)-1;                \no: randomly fall right or left
                if Peek($B800, (Bx(I)+Dir+(By(I)+1)*W)*2) = ^  then
                        [Bx(I):= Bx(I)+Dir;  By(I):= By(I)+1];
                ];
        Cursor(Bx(I), By(I));  ChOut(6, ^o);    \draw ball at its new position
        ];
    Sound(0, 3, 1);                             \delay about 1/6 second
until KeyHit;                                   \continue until a key is struck
]
```


{{Out}}

```txt

           o
          o.
          .o.
         . .o.
        .o. . .
       . .o. . .
      . .o. . . .
     . .o. . .o. .
    . . .o. . . . .
   . . . . . .o. . .
  . . . . .o. .o. . .
: : : : : : : : : : : :
: : : : :o: : : : : : :
: : : :o: : : : : : : :
: : : : : : : : : : : :
: : : : :o: : : : : : :
: : : : : : : :o: : : :
: : : : : :o: : : : : :
: : :o: : :o: : : :o: :
: : : : : :o:o: : : : :
: : : : :o:o:o: : : : :
: : : : :o:o:o: : : : :
: : :o:o:o:o:o:o: : : :
.......................

```



## Yabasic


```Yabasic
bola$ = "0000ff"
obst$ = "000000"

maxBalls = 10
cx = 1
cy = 2
dim Balls(maxBalls, 2)

open window 600,600
window origin "ct"

maxh = peek("winheight")

REM Draw the pins:

FOR row = 1 TO 7
  FOR col = 1 TO row
    FILL circle 40*col - 20*row, 40*row+80, 10
  NEXT col
NEXT row

REM Animate
tick = 0
bolas = 0
color 0,0,255

do
	if (bolas < maxBalls) then
		if tick = 3 then
			tick = 0
			bolas = bolas + 1
			Balls(bolas, cx) = 20
			Balls(bolas, cy) = 10
		end if
		tick = tick + 1
	end if
	for n = 1 to bolas
		if Balls(n, cy) then
			color$ = right$(getbit$(Balls(n,cx),Balls(n,cy) + 10,Balls(n,cx),Balls(n,cy) + 10),6)
			if (color$ = bola$) or (Balls(n,cy) >= maxh - 15) then
				Balls(n,cy) = 0
				break
			end if
			clear fill circle Balls(n,cx),Balls(n,cy),10
			if color$ = obst$ then
				if int(ran(2)) then
					Balls(n,cx) = Balls(n,cx) - 20
				else
					Balls(n,cx) = Balls(n,cx) + 20
				end if
			end if
			Balls(n,cy) = Balls(n,cy)+10
			fill circle Balls(n,cx),Balls(n,cy),10
			wait .001
		else
			wait .001
		end if
	next n
loop

```



## zig



```zig
const std = @import("std");
const rand = std.rand;
const time = std.os.time;

const PEG_LINES = 20;

fn boardSize(comptime peg_lines: u16) u16 {
    var i: u16 = 0;
    var size: u16 = 0;
    inline while (i <= PEG_LINES) : (i += 1) {
        size += i+1;
    }
    return size;
}

const BOARD_SIZE = boardSize(PEG_LINES);

fn stepBoard(board: *[BOARD_SIZE]u1, count: *[PEG_LINES+1]u8) void {
    var prng = rand.DefaultPrng.init(time.timestamp());

    var p: u8 = 0;
    var sum: u16 = 0;
    while (p <= PEG_LINES) : (p += 1) {
        const pegs = PEG_LINES-p;
        var i: u16 = 0;
        while (i < pegs+1) : (i += 1) {
            if (pegs != PEG_LINES and board[BOARD_SIZE-1-sum-i] == 1) {
                if (prng.random.boolean()) {
                    board.*[BOARD_SIZE-1-sum-i+pegs+1] = 1;
                } else {
                    board.*[BOARD_SIZE-1-sum-i+pegs+2] = 1;
                }
            } else if (pegs == PEG_LINES and board[BOARD_SIZE-1-sum-i] == 1) {
                count.*[pegs-i] += 1;
            }
            board.*[BOARD_SIZE-1-sum-i] = 0;
        }
        sum += pegs+1;
    }
}

fn printBoard(board: *[BOARD_SIZE]u1, count: *[PEG_LINES+1]u8) !void {
    var stdout = try std.io.getStdOut();
    try stdout.write("\x1B[2J\x1B[1;1H");
    var pegs: u16 = 0;
    var sum: u16 = 0;
    while (pegs <= PEG_LINES) : (pegs += 1) {
        var i: u16 = 0;
        while (i < (PEG_LINES-pegs)) : (i += 1) try stdout.write(" ");
        i = 0;
        while (i < pegs+1) : (i += 1) {
            const spot = if (board[i+sum] == 1) "o" else " ";
            try stdout.write(spot);
            if (i != pegs) try stdout.write("*");
        }
        sum += pegs+1;
        try stdout.write("\n");
    }
    for (count) |n| {
        const num_char = [2]u8{'0'+n, ' '};
        try stdout.write(num_char);
    }
    try stdout.write("\n");
}

pub fn main() !void {
    var board: [BOARD_SIZE]u1 = []u1{0} ** BOARD_SIZE;
    var bottom_count: [PEG_LINES+1]u8 = []u8{0} ** (PEG_LINES+1);

    var i: u16 = 0;
    while (i < 35) : (i += 1) {
        if (i < 10) board[0] = 1;

        try printBoard(&board, &bottom_count);
        stepBoard(&board, &bottom_count);
        time.sleep(150000000);
    }
}
```

