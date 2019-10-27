+++
title = "Plasma effect"
description = ""
date = 2019-04-28T10:26:51Z
aliases = []
[extra]
id = 20432
[taxonomies]
categories = []
tags = []
+++

{{task}}

The [[wp:Plasma_effect|plasma effect]] is a visual effect created by applying various functions, notably sine and cosine, to the color values of screen pixels. When animated (not a task requirement) the effect may give the impression of a colorful flowing liquid.


;Task
Create a plasma effect.




;See also
* [http://lodev.org/cgtutor/plasma.html Computer Graphics Tutorial (lodev.org)]
* [http://www.bidouille.org/prog/plasma Plasma (bidouille.org)]




## C


### ASCII version for Windows

If you don't want to bother with Graphics libraries, try out this nifty implementation on Windows :

```C

#include<windows.h>
#include<stdlib.h>
#include<stdio.h>
#include<time.h>
#include<math.h>

#define pi M_PI

int main()
{
	CONSOLE_SCREEN_BUFFER_INFO info;
    int cols, rows;
	time_t t;
	int i,j;

    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info);
    cols = info.srWindow.Right - info.srWindow.Left + 1;
    rows = info.srWindow.Bottom - info.srWindow.Top + 1;
	
	HANDLE console;
	
	console = GetStdHandle(STD_OUTPUT_HANDLE);
	
	system("@clear||cls");
	
	srand((unsigned)time(&t));
	
	for(i=0;i<rows;i++)
		for(j=0;j<cols;j++){
			SetConsoleTextAttribute(console,fabs(sin(pi*(rand()%254 + 1)/255.0))*254);
			printf("%c",219);
		}
		
	getchar();
	
	return 0;
}

```



### Graphics version

And here's the Graphics version, requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library. Prints out usage on incorrect invocation.

```C

#include<graphics.h>
#include<stdlib.h>
#include<math.h>
#include<time.h>

#define pi M_PI

void plasmaScreen(int width,int height){
	
	int x,y,sec;
	double dx,dy,dv;
	time_t t;
	
	initwindow(width,height,"WinBGIm Plasma");
	
	while(1){
		time(&t);
		sec = (localtime(&t))->tm_sec;
		
	for(x=0;x<width;x++)
		for(y=0;y<height;y++){
			dx = x + .5 * sin(sec/5.0);
			dy = y + .5 * cos(sec/3.0);
			
			dv = sin(x*10 + sec) + sin(10*(x*sin(sec/2.0) + y*cos(sec/3.0)) + sec) + sin(sqrt(100*(dx*dx + dy*dy)+1) + sec);
			
			setcolor(COLOR(255*fabs(sin(dv*pi)),255*fabs(sin(dv*pi + 2*pi/3)),255*fabs(sin(dv*pi + 4*pi/3))));
			
			putpixel(x,y,getcolor());
		}
	delay(1000);
	}
}

int main(int argC,char* argV[])
{
	if(argC != 3)
		printf("Usage : %s <Two positive integers separated by a space specifying screen size>",argV[0]);
	else{
		plasmaScreen(atoi(argV[1]),atoi(argV[2]));
	}
	return 0;
}

```



## C++

[[File:plasma.png]]

Windows version.

```cpp

#include <windows.h>
#include <math.h>
#include <string>

const int BMP_SIZE = 240, MY_TIMER = 987654;

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
    void saveBitmap( std::string path ) {
        BITMAPFILEHEADER fileheader;
        BITMAPINFO       infoheader;
        BITMAP           bitmap;
        DWORD            wb;

        GetObject( bmp, sizeof( bitmap ), &bitmap );
        DWORD* dwpBits = new DWORD[bitmap.bmWidth * bitmap.bmHeight];

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

        HANDLE file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
        WriteFile( file, &fileheader, sizeof( BITMAPFILEHEADER ), &wb, NULL );
        WriteFile( file, &infoheader.bmiHeader, sizeof( infoheader.bmiHeader ), &wb, NULL );
        WriteFile( file, dwpBits, bitmap.bmWidth * bitmap.bmHeight * 4, &wb, NULL );
        CloseHandle( file );

        delete [] dwpBits;
    }
    HDC getDC() const     { return hdc; }
    DWORD* bits()          { return ( DWORD* )pBits; }
private:
    void createPen() {
        if( pen ) DeleteObject( pen );
        pen = CreatePen( PS_SOLID, wid, clr );
        SelectObject( hdc, pen );
    }
    HBITMAP bmp; HDC    hdc;
    HPEN    pen; HBRUSH brush;
    void    *pBits; int width, height, wid;
    DWORD    clr;
};
class plasma
{
public:
    plasma() {
        currentTime = 0; _WD = BMP_SIZE >> 1; _WV = BMP_SIZE << 1;
        _bmp.create( BMP_SIZE, BMP_SIZE ); _bmp.clear();
        plasma1 = new BYTE[BMP_SIZE * BMP_SIZE * 4];
        plasma2 = new BYTE[BMP_SIZE * BMP_SIZE * 4];
        int i, j, dst = 0;
        double temp;
        for( j = 0; j < BMP_SIZE * 2; j++ ) {
            for( i = 0; i < BMP_SIZE * 2; i++ ) {
                plasma1[dst] = ( BYTE )( 128.0 + 127.0 * ( cos( ( double )hypot( BMP_SIZE - j, BMP_SIZE - i ) / 64.0 ) ) );
                plasma2[dst] = ( BYTE )( ( sin( ( sqrt( 128.0 + ( BMP_SIZE - i ) * ( BMP_SIZE - i ) + 
                               ( BMP_SIZE - j ) * ( BMP_SIZE - j ) ) - 4.0 ) / 32.0 ) + 1 ) * 90.0 );
                dst++;
            }
        }
    }
    void update() {
        DWORD dst;
        BYTE a, c1,c2, c3;
        currentTime += ( double )( rand() % 2 + 1 );

        int x1 = _WD + ( int )( ( _WD - 1 ) * sin( currentTime  / 137 ) ),
            x2 = _WD + ( int )( ( _WD - 1 ) * sin( -currentTime /  75 ) ),
            x3 = _WD + ( int )( ( _WD - 1 ) * sin( -currentTime / 125 ) ),
            y1 = _WD + ( int )( ( _WD - 1 ) * cos( currentTime  / 123 ) ),
            y2 = _WD + ( int )( ( _WD - 1 ) * cos( -currentTime /  85 ) ),
            y3 = _WD + ( int )( ( _WD - 1 ) * cos( -currentTime / 108 ) );

        int src1 = y1 * _WV + x1, src2 = y2 * _WV + x2, src3 = y3 * _WV + x3;
        
        DWORD* bits = _bmp.bits();
        for( int j = 0; j < BMP_SIZE; j++ ) {
            dst = j * BMP_SIZE;
            for( int i= 0; i < BMP_SIZE; i++ ) {
                a = plasma2[src1] + plasma1[src2] + plasma2[src3];
                c1 = a << 1; c2 = a << 2; c3 = a << 3;
                bits[dst + i] = RGB( c1, c2, c3 );
                src1++; src2++; src3++;
            }
            src1 += BMP_SIZE; src2 += BMP_SIZE; src3 += BMP_SIZE;
        }
        draw();
    }
    void setHWND( HWND hwnd ) { _hwnd = hwnd; }
private:
    void draw() {
        HDC dc = _bmp.getDC(), wdc = GetDC( _hwnd );
        BitBlt( wdc, 0, 0, BMP_SIZE, BMP_SIZE, dc, 0, 0, SRCCOPY );
        ReleaseDC( _hwnd, wdc );
    }
    myBitmap _bmp; HWND _hwnd; float _ang;
    BYTE *plasma1, *plasma2;
    double currentTime; int _WD, _WV;
};
class wnd
{
public:
    wnd() { _inst = this; }
    int wnd::Run( HINSTANCE hInst ) {
        _hInst = hInst; _hwnd = InitAll();
        SetTimer( _hwnd, MY_TIMER, 15, NULL );
        _plasma.setHWND( _hwnd );
        ShowWindow( _hwnd, SW_SHOW );
        UpdateWindow( _hwnd );
        MSG msg;
        ZeroMemory( &msg, sizeof( msg ) );
        while( msg.message != WM_QUIT ) {
            if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) != 0 ) {
                TranslateMessage( &msg );
                DispatchMessage( &msg );
            }
        }
        return UnregisterClass( "_MY_PLASMA_", _hInst );
    }
private:
    void wnd::doPaint( HDC dc ) { _plasma.update(); }
    void wnd::doTimer()         { _plasma.update(); }
    static int WINAPI wnd::WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam ) {
        switch( msg ) {
            case WM_PAINT: {
                    PAINTSTRUCT ps;
                    _inst->doPaint( BeginPaint( hWnd, &ps ) );
                    EndPaint( hWnd, &ps );
                    return 0;
                }
            case WM_DESTROY: PostQuitMessage( 0 ); break;
            case WM_TIMER: _inst->doTimer(); break;
            default: return DefWindowProc( hWnd, msg, wParam, lParam );
        }
        return 0;
    }
    HWND InitAll() {
        WNDCLASSEX wcex;
        ZeroMemory( &wcex, sizeof( wcex ) );
        wcex.cbSize        = sizeof( WNDCLASSEX );
        wcex.style         = CS_HREDRAW | CS_VREDRAW;
        wcex.lpfnWndProc   = ( WNDPROC )WndProc;
        wcex.hInstance     = _hInst;
        wcex.hCursor       = LoadCursor( NULL, IDC_ARROW );
        wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
        wcex.lpszClassName = "_MY_PLASMA_";

        RegisterClassEx( &wcex );

        RECT rc = { 0, 0, BMP_SIZE, BMP_SIZE };
        AdjustWindowRect( &rc, WS_SYSMENU | WS_CAPTION, FALSE );
        int w = rc.right - rc.left, h = rc.bottom - rc.top;
        return CreateWindow( "_MY_PLASMA_", ".: Plasma -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, w, h, NULL, NULL, _hInst, NULL );
    }
    static wnd* _inst; HINSTANCE _hInst; HWND _hwnd; plasma _plasma;
};
wnd* wnd::_inst = 0;
int APIENTRY WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow ) {
    wnd myWnd;
    return myWnd.Run( hInstance );
}

```



## Ceylon

Be sure to import javafx.base, javafx.graphics and ceylon.numeric in your module file.
{{trans|Java}}

```ceylon

import javafx.application {
    Application
}
import javafx.stage {
    Stage
}
import javafx.scene {
    Scene
}
import javafx.scene.layout {
    BorderPane
}
import javafx.scene.image {
    WritableImage,
    ImageView
}
import ceylon.numeric.float {
    sin,
    sqrt,
    remainder
}
import javafx.scene.paint {
    Color
}
import javafx.animation {
    AnimationTimer
}

shared void run() {
    Application.launch(`Plasma`);
}

shared class Plasma() extends Application() {

    function createPlasma(Integer width, Integer height) => [
        for (j in 0:height) [
            for (i in 0:width)
            let (x = i.float, y = j.float)
            ( sin(x / 16.0)
            + sin(y / 8.0)
            + sin((x + y) / 16.0)
            + sin(sqrt(x ^ 2.0 + y ^ 2.0) / 8.0)
            + 4.0 )
            / 8.0
        ]
    ];

    void writeImage(Float[][] plasma, WritableImage img, Float hueShift = 0.0) {
        value writer = img.pixelWriter;
        for(j->row in plasma.indexed) {
            for(i->percent in row.indexed) {
                value hue = remainder(hueShift + percent, 1.0)  * 360.0;
                writer.setColor(i, j, Color.hsb(hue, 1.0, 1.0));
            }
        }
    }

    shared actual void start(Stage primaryStage) {

        value w = 500;
        value h = 500;
        value plasma = createPlasma(w, h);
        value img = WritableImage(w, h);
        writeImage(plasma, img);

        value root = BorderPane();
        root.center = ImageView(img);

        variable value hueShift = 0.0;
        value timer = object extends AnimationTimer() {
            shared actual void handle(Integer now) {
                hueShift = remainder(hueShift + 0.02, 1.0);
                writeImage(plasma, img, hueShift);
            }
        };
        timer.start();

        value scene = Scene(root);
        primaryStage.title = "Plasma";
        primaryStage.setScene(scene);
        primaryStage.sizeToScene();
        primaryStage.show();
    }

}
```



## Common Lisp

{{libheader|lispbuilder-sdl}}
{{libheader|simple-rgb}}
plasma_demo.lisp:

```lisp
(require :lispbuilder-sdl)
(require :simple-rgb)

(defparameter *palette*
  (let ((palette-aux (make-array 256 :element-type 'fixnum)))
    (dotimes (i 256)
      (let ((color_i (simple-rgb:hsv->rgb (simple-rgb:hsv (/ i 255.0) 1.0 1.0))))
        (setf (aref palette-aux i) (loop :for component :across color_i
                                       :for i :from 0
                                       :sum (ash component (* 8 i))))))
    palette-aux)
  "palette")

(defun value->color (palette palette-shift index)
  (aref palette (mod (+ index palette-shift) (length palette))))

(defun return-color-by-pos (x y &optional w h)
  "returns a color index"
  (floor
   (/ (+ (+ 128.0 (* 128.0 (sin (/ x 16.0))))
         (+ 128.0 (* 128.0 (sin (/ y 8.0))))
         (+ 128.0 (* 128.0 (sin (/ (+ x y) 16.0))))
         (+ 128.0 (* 128.0 (sin (/ (sqrt (+ (* x x) (* y y))) 8.0)))))
      4.0)))

(defun return-color-by-pos-another (x y &optional w h)
  "a different function that returns a color index"
  (floor
   (/ (+ (+ 128.0 (* 128.0 (sin (/ x 16.0))))
         (+ 128.0 (* 128.0 (sin (/ y 32.0))))
         (+ 128.0 (* 128.0 (sin (/ (sqrt (+ (expt (/ (- x w) 2.0) 2) (expt (/ (- y h) 2.0) 2))) 8.0))))
         (+ 128.0 (* 128.0 (sin (/ (sqrt (+ (* x x) (* y y))) 8.0)))))
      4.0)))

(defun plasma-render (surface palette-shift)
  "render plasma"
  (let ((width (sdl:width surface))
        (height (sdl:height surface)))
    (sdl-base::with-pixel (s (sdl:fp surface))
      (dotimes (h height)
        (dotimes (w width)
          (sdl-base::write-pixel s w h (value->color *palette* palette-shift (funcall #'return-color-by-pos-another w h width height)))))))
  surface)

(defun demo/plasma ()
  "main function: shows a window rendering a plasma efect"
  (sdl:with-init ()
    (let ((win (sdl:window 320 240
                           :bpp 24
                           :resizable nil
                           :title-caption "demo/plasma"
                           :icon-caption "demo/plasma")))
      (let ((palette-shift 0))
      (sdl:update-display win)
      (sdl:with-events ()
        (:idle
         (plasma-render win palette-shift)
         (sdl:update-display win)
         (incf palette-shift))
        (:video-expose-event () (sdl:update-display win))
        (:key-down-event (:key key)
                         (when (or
                                (sdl:key= key :sdl-key-escape)
                                (sdl:key= key :sdl-key-q))
                           (sdl:push-quit-event)))
        (:quit-event () t))))))

(demo/plasma)
```


## FreeBASIC


```freebasic
' version 12-04-2017
' compile with: fbc -s gui
' Computer Graphics Tutorial (lodev.org), last example

#Define dist(a, b, c, d) Sqr(((a - c) * (a - c) + (b - d) * (b - d)))

Const As ULong w = 256
Const As ULong h = 256
ScreenRes w, h, 24
WindowTitle ("Plasma effect")

Dim As ULong x, y
Dim As UByte c
Dim As Double time_, value

Do
    time_ += .99
    ScreenLock
    For x = 0 To w -1
        For y = 0 To h -1
            value = Sin(dist(x + time_, y, 128, 128) / 8) _
            + Sin(dist(x, y, 64, 64) / 8) _
            + Sin(dist(x, y + time_ / 7, 192, 64) / 7) _
            + Sin(dist(x, y, 192, 100) / 8) + 4
            ' c = Int(value) * 32
            c = int(value * 32)
            PSet(x, y), RGB(c, c * 2, 255 - c)
        Next
    Next
    ScreenUnLock
    Sleep 1

    If Inkey <> "" Or Inkey = Chr(255) + "k" Then
        End
    End If

Loop
```



## Go

This uses Go's 'image' packages in its standard library to create an animated GIF.

When played this is broadly similar to the Java entry on which it is based. The whole animation completes in 4 seconds and repeats indefinitely.

Although the .gif works fine in Firefox it might not do so in EOG due to optimizations made during its creation. If so, then the following ImageMagick command should fix it:

```txt

  $ convert plasma.gif -coalesce plasma2.gif
  $ eog plasma2.gif

```


```go
package main

import (
    "image"
    "image/color"
    "image/gif"
    "log"
    "math"
    "os"
)

func setBackgroundColor(img *image.Paletted, w, h int, ci uint8) {
    for x := 0; x < w; x++ {
        for y := 0; y < h; y++ {
            img.SetColorIndex(x, y, ci)
        }
    }
}

func hsb2rgb(hue, sat, bri float64) (r, g, b int) {
    u := int(bri*255 + 0.5)
    if sat == 0 {
        r, g, b = u, u, u
    } else {
        h := (hue - math.Floor(hue)) * 6
        f := h - math.Floor(h)
        p := int(bri*(1-sat)*255 + 0.5)
        q := int(bri*(1-sat*f)*255 + 0.5)
        t := int(bri*(1-sat*(1-f))*255 + 0.5)
        switch int(h) {
        case 0:
            r, g, b = u, t, p
        case 1:
            r, g, b = q, u, p
        case 2:
            r, g, b = p, u, t
        case 3:
            r, g, b = p, q, u
        case 4:
            r, g, b = t, p, u
        case 5:
            r, g, b = u, p, q
        }
    }
    return
}

func main() {
    const degToRad = math.Pi / 180
    const nframes = 100
    const delay = 4 // 40ms
    w, h := 640, 640
    anim := gif.GIF{LoopCount: nframes}
    rect := image.Rect(0, 0, w, h)
    palette := make([]color.Color, nframes+1)
    palette[0] = color.White
    for i := 1; i <= nframes; i++ {
        r, g, b := hsb2rgb(float64(i)/nframes, 1, 1)
        palette[i] = color.RGBA{uint8(r), uint8(g), uint8(b), 255}
    }
    for f := 1; f <= nframes; f++ {
        img := image.NewPaletted(rect, palette)
        setBackgroundColor(img, w, h, 0) // white background
        for y := 0; y < h; y++ {
            for x := 0; x < w; x++ {
                fx, fy := float64(x), float64(y)
                value := math.Sin(fx / 16)
                value += math.Sin(fy / 8)
                value += math.Sin((fx + fy) / 16)
                value += math.Sin(math.Sqrt(fx*fx+fy*fy) / 8)
                value += 4 // shift range from [-4, 4] to [0, 8]
                value /= 8 // bring range down to [0, 1]
                _, rem := math.Modf(value + float64(f)/float64(nframes))
                ci := uint8(nframes*rem) + 1
                img.SetColorIndex(x, y, ci)
            }
        }
        anim.Delay = append(anim.Delay, delay)
        anim.Image = append(anim.Image, img)
    }
    file, err := os.Create("plasma.gif")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close() 
    if err2 := gif.EncodeAll(file, &anim); err != nil {
        log.Fatal(err2)
    }    
}
```



## Gosu

[[File:Gosu_plasma.png|200px|thumb|right]]
{{trans|Java}}

```gosu

uses javax.swing.*
uses java.awt.*
uses java.awt.image.*
uses java.awt.event.ActionEvent
uses java.awt.image.BufferedImage#*
uses java.lang.Math#*

var size = 400
EventQueue.invokeLater(\ -> showPlasma())

function showPlasma() {
  var frame = new JFrame("Plasma") {:Resizable = false, :DefaultCloseOperation = JFrame.EXIT_ON_CLOSE}
  frame.add(new Plasma(), BorderLayout.CENTER)
  frame.pack()
  frame.setLocationRelativeTo(null)
  frame.Visible = true 
}
  
class Plasma extends JPanel  {
  var hueShift: float
  property get plasma: float[][] = createPlasma(size, size)
  property get img: BufferedImage = new BufferedImage(size, size, TYPE_INT_RGB)
  
  construct() {
    PreferredSize = new Dimension(size, size)
    new Timer(50, \ e -> {hueShift+=0.02 repaint()}).start()
  }
  
  private function createPlasma(w: int, h: int): float[][] {
    var buffer = new float[h][w]
    for(y in 0..|h)
      for(x in 0..|w) {
        var value = (sin(x / 16) + sin(y / 8) + sin((x + y) / 16) + sin(sqrt(x * x + y * y) / 8) + 4) / 8
        buffer[y][x] = value as float
      }
    return buffer
  }

  override function paintComponent(g: Graphics) {
    for(y in 0..|plasma.length)
      for(x in 0..|plasma[0].length)
        img.setRGB(x, y, Color.HSBtoRGB(hueShift + plasma[y][x], 1, 1))
    g.drawImage(img, 0, 0, null)
  }
}

```



## J

[[File:J-viewmat-plasma.png|200px|thumb|right]]

```j
require 'trig viewmat'
plasma=: 3 :0
  'w h'=. y
  X=. (i. % <:) w
  Y=. (i. % <:) h
  x1=. sin X*16
  y1=. sin Y*32
  xy1=. sin (Y+/X)*16
  xy2=. sin (Y +&.*:/ X)*32
  xy1+xy2+y1+/x1
)
```



```j>   viewmat plasma 256 256</lang



## Java

[[File:plasma_effect_java.png|200px|thumb|right]]
{{works with|Java|8}}

```java
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import static java.awt.image.BufferedImage.*;
import static java.lang.Math.*;
import javax.swing.*;

public class PlasmaEffect extends JPanel {
    float[][] plasma;
    float hueShift = 0;
    BufferedImage img;

    public PlasmaEffect() {
        Dimension dim = new Dimension(640, 640);
        setPreferredSize(dim);
        setBackground(Color.white);

        img = new BufferedImage(dim.width, dim.height, TYPE_INT_RGB);
        plasma = createPlasma(dim.height, dim.width);

        // animate about 24 fps and shift hue value with every frame
        new Timer(42, (ActionEvent e) -> {
            hueShift = (hueShift + 0.02f) % 1;
            repaint();
        }).start();
    }

    float[][] createPlasma(int w, int h) {
        float[][] buffer = new float[h][w];

        for (int y = 0; y < h; y++)
            for (int x = 0; x < w; x++) {

                double value = sin(x / 16.0);
                value += sin(y / 8.0);
                value += sin((x + y) / 16.0);
                value += sin(sqrt(x * x + y * y) / 8.0);
                value += 4; // shift range from -4 .. 4 to 0 .. 8
                value /= 8; // bring range down to 0 .. 1

                // requires VM option -ea
                assert (value >= 0.0 && value <= 1.0) : "Hue value out of bounds";

                buffer[y][x] = (float) value;
            }
        return buffer;
    }

    void drawPlasma(Graphics2D g) {
        int h = plasma.length;
        int w = plasma[0].length;
        for (int y = 0; y < h; y++)
            for (int x = 0; x < w; x++) {
                float hue = hueShift + plasma[y][x] % 1;
                img.setRGB(x, y, Color.HSBtoRGB(hue, 1, 1));
            }
        g.drawImage(img, 0, 0, null);
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        drawPlasma(g);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Plasma Effect");
            f.setResizable(false);
            f.add(new PlasmaEffect(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

{{trans|Java}}

```javascript
<!DOCTYPE html>
<html lang='en'>
<head>
    <meta charset='UTF-8'>
    <style>
        canvas {
            position: absolute;
            top: 50%;
            left: 50%;
            width: 700px;
            height: 500px;
            margin: -250px 0 0 -350px;
        }
        body {
            background-color: navy;
        }
    </style>
</head>
<body>
    <canvas></canvas>
    <script>
        'use strict';
        var canvas = document.querySelector('canvas');
        canvas.width = 700;
        canvas.height = 500;

        var g = canvas.getContext('2d');

        var plasma = createPlasma(canvas.width, canvas.height);
        var hueShift = 0;

        function createPlasma(w, h) {
            var buffer = new Array(h);

            for (var y = 0; y < h; y++) {
                buffer[y] = new Array(w);

                for (var x = 0; x < w; x++) {

                    var value = Math.sin(x / 16.0);
                    value += Math.sin(y / 8.0);
                    value += Math.sin((x + y) / 16.0);
                    value += Math.sin(Math.sqrt(x * x + y * y) / 8.0);
                    value += 4; // shift range from -4 .. 4 to 0 .. 8
                    value /= 8; // bring range down to 0 .. 1

                    buffer[y][x] = value;
                }
            }
            return buffer;
        }

        function drawPlasma(w, h) {
            var img = g.getImageData(0, 0, w, h);

            for (var y = 0; y < h; y++) {

                for (var x = 0; x < w; x++) {

                    var hue = hueShift + plasma[y][x] % 1;
                    var rgb = HSVtoRGB(hue, 1, 1);
                    var pos = (y * w + x) * 4;
                    img.data[pos] = rgb.r;
                    img.data[pos + 1] = rgb.g;
                    img.data[pos + 2] = rgb.b;
                }
            }
            g.putImageData(img, 0, 0);
        }

        /* copied from stackoverflow */
        function HSVtoRGB(h, s, v) {
            var r, g, b, i, f, p, q, t;

            i = Math.floor(h * 6);
            f = h * 6 - i;
            p = v * (1 - s);
            q = v * (1 - f * s);
            t = v * (1 - (1 - f) * s);
            switch (i % 6) {
                case 0: r = v, g = t, b = p; break;
                case 1: r = q, g = v, b = p; break;
                case 2: r = p, g = v, b = t; break;
                case 3: r = p, g = q, b = v; break;
                case 4: r = t, g = p, b = v; break;
                case 5: r = v, g = p, b = q; break;
            }
            return {
                r: Math.round(r * 255),
                g: Math.round(g * 255),
                b: Math.round(b * 255)
            };
        }

        function drawBorder() {
            g.strokeStyle = "white";
            g.lineWidth = 10;
            g.strokeRect(0, 0, canvas.width, canvas.height);
        }

        function animate(lastFrameTime) {
            var time = new Date().getTime();
            var delay = 42;

            if (lastFrameTime + delay < time) {
                hueShift = (hueShift + 0.02) % 1;
                drawPlasma(canvas.width, canvas.height);
                drawBorder();
                lastFrameTime = time;
            }
            
            requestAnimationFrame(function () {
                animate(lastFrameTime);
            });
        }

        g.fillRect(0, 0, canvas.width, canvas.height);
        animate(0);
    </script>

</body>
</html>
```



## Julia

{{trans|Perl}}

```julia
using Luxor, Colors

Drawing(800, 800)

function plasma(wid, hei)
    for x in 1:wid, y in 1:hei
        sethue(parse(Colorant, HSV(180 + 45sin(x/19) + 45sin(y/9) +
            45sin((x+y)/25) + 45sin(sqrt(x^2 + y^2)/8), 1, 1)))
        circle(Point(x, y), 1, :fill)
    end
end

@png plasma(800, 800)

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

import java.awt.*
import java.awt.image.BufferedImage
import javax.swing.*

class PlasmaEffect : JPanel() {
    private val plasma: Array<FloatArray>
    private var hueShift = 0.0f
    private val img: BufferedImage

    init {
        val dim = Dimension(640, 640)
        preferredSize = dim
        background = Color.white
        img = BufferedImage(dim.width, dim.height, BufferedImage.TYPE_INT_RGB)
        plasma = createPlasma(dim.height, dim.width)
        // animate about 24 fps and shift hue value with every frame
        Timer(42) {
            hueShift = (hueShift + 0.02f) % 1
            repaint()
        }.start()
    }

    private fun createPlasma(w: Int, h: Int): Array<FloatArray> {
        val buffer = Array(h) { FloatArray(w) }
        for (y in 0 until h)
            for (x in 0 until w) {
                var value = Math.sin(x / 16.0)
                value += Math.sin(y / 8.0)
                value += Math.sin((x + y) / 16.0)
                value += Math.sin(Math.sqrt((x * x + y * y).toDouble()) / 8.0)
                value += 4.0  // shift range from -4 .. 4 to 0 .. 8
                value /= 8.0  // bring range down to 0 .. 1
                if (value < 0.0 || value > 1.0) throw RuntimeException("Hue value out of bounds")
                buffer[y][x] = value.toFloat()
            }
        return buffer
    }

    private fun drawPlasma(g: Graphics2D) {
        val h = plasma.size
        val w = plasma[0].size
        for (y in 0 until h)
            for (x in 0 until w) {
                val hue = hueShift + plasma[y][x] % 1
                img.setRGB(x, y, Color.HSBtoRGB(hue, 1.0f, 1.0f))
            }
        g.drawImage(img, 0, 0, null)
    }

    override fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        drawPlasma(g);
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        f.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        f.title = "Plasma Effect"
        f.isResizable = false
        f.add(PlasmaEffect(), BorderLayout.CENTER)
        f.pack()
        f.setLocationRelativeTo(null)
        f.isVisible = true
    }
}
```



## Lua

Needs L&Ouml;VE 2D Engine
{{trans|C++}}

```lua

_ = love.graphics
p1, p2, points = {}, {}, {}

function hypotenuse( a, b )
    return a * a + b * b
end
function love.load()
    size = _.getWidth()
    currentTime, doub, half = 0, size * 2, size / 2
    local b1, b2
  
    for j = 0, size * 2 do
        for i = 0, size * 2 do
            b1 = math.floor( 128 + 127 * ( math.cos( math.sqrt( hypotenuse( size - j , size - i ) ) / 64 ) ) )
            b2 = math.floor( ( math.sin( ( math.sqrt( 128.0 + hypotenuse( size - i, size - j ) ) - 4.0 ) / 32.0 ) + 1 ) * 90 )
            table.insert( p1, b1 ); table.insert( p2, b2 )
        end
    end
end
function love.draw()
    local a, c1, c2, c3, s1, s2, s3
    currentTime = currentTime + math.random( 2 ) * 3
    local x1 = math.floor( half + ( half -  2 ) * math.sin(  currentTime /  47 ) )
    local x2 = math.floor( half + ( half /  7 ) * math.sin( -currentTime / 149 ) )
    local x3 = math.floor( half + ( half -  3 ) * math.sin( -currentTime / 157 ) )
    local y1 = math.floor( half + ( half / 11 ) * math.cos(  currentTime /  71 ) )
    local y2 = math.floor( half + ( half -  5 ) * math.cos( -currentTime / 181 ) )
    local y3 = math.floor( half + ( half / 23 ) * math.cos( -currentTime / 137 ) )
    s1 = y1 * doub + x1; s2 = y2 * doub + x2; s3 = y3 * doub + x3
    for j = 0, size do
        for i = 0, size do
            a = p2[s1] + p1[s2] + p2[s3]
            c1 = a * 2; c2 = a * 4; c3 = a * 8
            table.insert( points, { i, j, c1, c2, c3, 255 } )
            s1 = s1 + 1; s2 = s2 + 1; s3 = s3 + 1;
        end
        s1 = s1 + size; s2 = s2 + size; s3 = s3 + size
    end
    _.points( points ) 
end

```



## Ol


```scheme

; creating the "plasma" image buffer
(import (lib math))
(define plasma
   (fold append #null
      (map (lambda (y)
            (map (lambda (x)
                  (let ((value (/
                           (+ (sin (/ y 4))
                              (sin (/ (+ x y) 8))
                              (sin (/ (sqrt (+ (* x x) (* y y))) 8))
                              4) 8)))
                     value))
               (iota 256)))
         (iota 256))))

```


```scheme

; rendering the prepared buffer (using OpenGL)
(import (lib gl))
(import (OpenGL version-1-1))
(gl:set-window-size 256 256)

(glBindTexture GL_TEXTURE_2D 0)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 GL_LUMINANCE
   256 256
   0 GL_LUMINANCE GL_FLOAT (cons (fft* fft-float) plasma))

(glEnable GL_TEXTURE_2D)

(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)
   (glBegin GL_QUADS)
      (glTexCoord2f 0 0)
      (glVertex2f -1 -1)
      (glTexCoord2f 0 1)
      (glVertex2f -1 1)
      (glTexCoord2f 1 1)
      (glVertex2f 1 1)
      (glTexCoord2f 1 0)
      (glVertex2f 1 -1)
   (glEnd)
   #null))

```



## Racket


Uses `return-color-by-pos` from [[#Lisp]], because it was almost lift and shift


```racket
#lang racket
;;  from lisp (cos I could just lift the code)
(require images/flomap
         2htdp/universe
         racket/flonum)

;; copied from pythagoras-triangle#racket
(define (real-remainder x q) (- x (* (floor (/ x q)) q)))

(define (HSV->RGB H S V)
  (define C (* V S)) ; chroma
  (define H′ (/ H 60))
  (define X (* C (- 1 (abs (- (real-remainder H′ 2) 1)))))
  (define-values (R1 G1 B1)
    (cond
      [(< H′ 1) (values C X 0.)]
      [(< H′ 2) (values X C 0.)]
      [(< H′ 3) (values 0. C X)]
      [(< H′ 4) (values 0. X C)]
      [(< H′ 5) (values X 0. C)]
      [(< H′ 6) (values C 0. X)]
      [else (values 0. 0. 0.)]))
  (define m (- V C))
  (values (+ R1 m) (+ G1 m) (+ B1 m)))


(define ((colour-component-by-pos ϕ) k x y)
  (let ((rv
         (/ (+ (+ 1/2 (* 1/2 (sin (+ ϕ (/ x 16.0)))))
               (+ 1/2 (* 1/2 (sin (+ ϕ (/ y 8.0)))))
               (+ 1/2 (* 1/2 (sin (+ ϕ (/ (+ x y) 16.0)))))
               (+ 1/2 (* 1/2 (sin (+ ϕ (/ (sqrt (+ (sqr x) (sqr y))) 8.0))))))
            4.0)))
    rv))

(define ((plasma-flomap (ϕ 0)) w h)
  (build-flomap 1 w h (colour-component-by-pos ϕ)))

(define ((plasma-image (ϕ 0)) w h)
  (flomap->bitmap ((plasma-flomap ϕ) w h)))

(define ((colour-plasma plsm) t)
  (let ((w (flomap-width plsm))
        (h (flomap-height plsm)))
    (flomap->bitmap
     (build-flomap* 3 w h
                    (λ (x y)
                      (define-values (r g b)
                        (HSV->RGB (real-remainder
                                   (+ (* t 5.)
                                      (* 360 (flomap-ref plsm 0 x y)))
                                   360.) 1. 1.))
                      (flvector r g b))))))

;((plasma-image) 200 200)

;((plasma-image (/ pi 32)) 200 200)

(define plsm ((plasma-flomap) 300 300))
  (animate (λ (t)
             ((colour-plasma plsm) t)))
```



## Perl

{{trans|Perl 6}}

```perl
use Imager;

sub plasma {
    my ($w, $h) = @_;

    my $img = Imager->new(xsize => $w, ysize => $h);

    for my $x (0 .. $w-1) {
        for my $y (0 .. $h-1) {
            my $hue = 4 + sin($x/19) + sin($y/9) + sin(($x+$y)/25) + sin(sqrt($x**2 + $y**2)/8);
            $img->setpixel(x => $x, y => $y, color => {hsv => [360 * $hue / 8, 1, 1]});
        }
    }

    return $img;
}

my $img = plasma(400, 400);
$img->write(file => 'plasma-perl.png');
```

Off-site image: [https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/plasma.png Plasma effect]


## Perl 6

[[File:Plasma-perl6.png|200px|thumb|right]]
{{works with|Rakudo|2018.09}}

```perl6
use Image::PNG::Portable;

my ($w, $h) = 400, 400;
my $out = Image::PNG::Portable.new: :width($w), :height($h);

plasma($out);

$out.write: 'Plasma-perl6.png';

sub plasma ($png) {
    (^$w).race.map: -> $x {
        for ^$h -> $y {
            my $hue = 4 + sin($x / 19) + sin($y / 9) + sin(($x + $y) / 25) + sin(sqrt($x² + $y²) / 8);
            $png.set: $x, $y, |hsv2rgb($hue/8, 1, 1);
        }
    }
}

sub hsv2rgb ( $h, $s, $v ){
    my $c = $v * $s;
    my $x = $c * (1 - abs( (($h*6) % 2) - 1 ) );
    my $m = $v - $c;
    (do given $h {
        when   0..^1/6 { $c, $x, 0 }
        when 1/6..^1/3 { $x, $c, 0 }
        when 1/3..^1/2 { 0, $c, $x }
        when 1/2..^2/3 { 0, $x, $c }
        when 2/3..^5/6 { $x, 0, $c }
        when 5/6..1    { $c, 0, $x }
    } ).map: ((*+$m) * 255).Int
}
```



## Phix

{{libheader|pGUI}}
{{trans|JavaScript}}

```Phix
-- demo\rosetta\plasma.exw
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

sequence plasma
integer pw = 0, ph = 0

procedure createPlasma(integer w, h)
    plasma = repeat(repeat(0,w),h)
    for y=1 to h do
        for x=1 to w do
            atom v = sin(x/16)
            v += sin(y/8)
            v += sin((x+y)/16)
            v += sin(sqrt(x*x + y*y)/8)
            v += 4 -- shift range from -4 .. 4 to 0 .. 8
            v /= 8 -- bring range down to 0 .. 1
            plasma[y][x] = v
        end for
    end for
    pw = w
    ph = h
end procedure

atom hueShift = 0

procedure drawPlasma(integer w, h)
    hueShift = remainder(hueShift + 0.02,1)
    sequence rgb3 = repeat(repeat(0,w*h),3)
    integer cx = 1
    for y=1 to h do
        for x=1 to w do
            atom hue = hueShift + remainder(plasma[y][x],1)
            integer i = floor(hue * 6)
            atom t = 255,
                 f = (hue * 6 - i)*t,
                 q = t - f, 
                 r, g, b
            switch mod(i,6) do
                case 0: r = t; g = f; b = 0
                case 1: r = q; g = t; b = 0
                case 2: r = 0; g = t; b = f
                case 3: r = 0; g = q; b = t
                case 4: r = f; g = 0; b = t
                case 5: r = t; g = 0; b = q
            end switch
            rgb3[1][cx] = r
            rgb3[2][cx] = g
            rgb3[3][cx] = b
            cx += 1
        end for
    end for
    cdCanvasPutImageRectRGB(cddbuffer, w, h, rgb3, 0, 0, 0, 0, 0, 0, 0, 0) 
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    atom {w,h} = IupGetIntInt(canvas, "DRAWSIZE")
    if pw!=w or ph!=h then
        createPlasma(w,h)
    end if
    cdCanvasActivate(cddbuffer)
    drawPlasma(w,h)
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function timer_cb(Ihandle /*ih*/)
    IupUpdate(canvas)
    return IUP_IGNORE
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_WHITE)
    cdCanvasSetForeground(cddbuffer, CD_GRAY)
    return IUP_DEFAULT
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "450x300")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Plasma")
    IupCloseOnEscape(dlg)

    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    Ihandle timer = IupTimer(Icallback("timer_cb"), 50)
    IupMainLoop()
    IupClose()
end procedure

main()
```

And here's a simple console ditty, similar I think to C's ASCII version for Windows, though this also works on Linux:

```Phix
sequence s = video_config() 
for i=1 to s[VC_SCRNLINES]*s[VC_SCRNCOLS]-1 do
    bk_color(rand(16)-1)
    text_color(rand(16)-1)
    puts(1,"\xDF")
end for
{} = wait_key()
```



## Python

{{trans|Perl 6}}


```python
import math
import colorsys
from PIL import Image

def plasma (w, h):
	out = Image.new("RGB", (w, h))
	pix = out.load()
	for x in range (w):
		for y in range(h):
			hue = 4.0 + math.sin(x / 19.0) + math.sin(y / 9.0) \
				+ math.sin((x + y) / 25.0) + math.sin(math.sqrt(x**2.0 + y**2.0) / 8.0)
			hsv = colorsys.hsv_to_rgb(hue/8.0, 1, 1)
			pix[x, y] = tuple([int(round(c * 255.0)) for c in hsv])
	return out

if __name__=="__main__":
	im = plasma(400, 400)
	im.show()
```



## Ring


```ring

# Project : Plasma effect

load "guilib.ring"

paint = null

new qapp
       {
        win1 = new qwidget()
       {
                   setwindowtitle("Plasma effect")
                   setgeometry(100,100,500,600)

                   label1 = new qlabel(win1)
                   {
                   setgeometry(10,10,400,400)
                   settext("")
                    }

                   new qpushbutton(win1)
                   {
                   setgeometry(150,500,100,30)
                   settext("Draw")
                   setclickevent("Draw()")
                  }
                  show()
       }
       exec()
       }

func draw

        p1    = new qpicture()
        color = new qcolor() { setrgb(0,0,255,255) }   ### <<< BLUE
        pen   = new qpen() { setcolor(color) setwidth(1) }

        paint = new qpainter()
        {
                   begin(p1)
                   setpen(pen)

                   w = 256 
                   h = 256
                   time = 0

                   for x = 0 to w -1
                         for y = 0 to h -1
                               time = time + 0.99 
                               value = sin(dist(x + time, y, 128, 128) / 8) +
                                           sin(dist(x, y, 64, 64) / 8) +
                                           sin(dist(x, y + time / 7, 192, 64) / 7) +
                                           sin(dist(x, y, 192, 100) / 8) + 4
                               c = floor(value * 32)
                               r = c
                               g = (c*2)%255
                               b = 255-c
                               color2 = new qcolor()
                               color2.setrgb(r,g,b,255)
                               pen.setcolor(color2) 
                               setpen(pen)     
                               drawpoint(x,y)
                        next
                   next
        endpaint()
        }
        label1 { setpicture(p1) show() }
        return

func dist(a, b, c, d)
        d = sqrt(((a - c) * (a - c) + (b - d) * (b - d)))
        return d

```

Output:

[http://www.dropbox.com/s/gdioouv328m2d60/PlasmaEffect.jpg?dl=0 Plasma effect]


## Scala


### Java Swing Interoperability


```Scala
import java.awt._
import java.awt.event.ActionEvent
import java.awt.image.BufferedImage

import javax.swing._

import scala.math.{sin, sqrt}

object PlasmaEffect extends App {

  SwingUtilities.invokeLater(() =>
    new JFrame("Plasma Effect") {

      class PlasmaEffect extends JPanel {
        private val (w, h) = (640, 640)
        private var hueShift = 0.0f

        override def paintComponent(gg: Graphics): Unit = {
          val g = gg.asInstanceOf[Graphics2D]

          def drawPlasma(g: Graphics2D) = {
            val img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)

            for (y <- 0 until h;
                 x <- 0 until w) {

              def design =
                (sin(x / 16f) + sin(y / 8f) + sin((x + y) / 16f) + sin(sqrt(x * x + y * y) / 8f) + 4).toFloat / 8

              img.setRGB(x, y, Color.HSBtoRGB(hueShift + design % 1, 1, 1))
            }
            g.drawImage(img, 0, 0, null)
          }

          super.paintComponent(gg)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          drawPlasma(g)
        }

        // animate about 24 fps and shift hue value with every frame
        new Timer(42, (_: ActionEvent) => {
          hueShift = (hueShift + 0.02f) % 1
          repaint()
        }).start()

        setBackground(Color.white)
        setPreferredSize(new Dimension(h, w))
      }

      add(new PlasmaEffect, BorderLayout.CENTER)
      pack()
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setLocationRelativeTo(null)
      setResizable(false)
      setVisible(true)
    })

}
```


## Sidef

{{trans|Perl 6}}

```ruby
require('Imager')

class Plasma(width=400, height=400) {

    has img = nil

    method init {
        img = %O|Imager|.new(xsize => width, ysize => height)
    }

    method generate {
        for y=(^height), x=(^width) {
            var hue = (4 + sin(x/19) + sin(y/9) + sin((x+y)/25) + sin(hypot(x, y)/8))
            img.setpixel(x => x, y => y, color => Hash(hsv => [360 * hue / 8, 1, 1]))
        }
    }

    method save_as(filename) {
        img.write(file => filename)
    }
}

var plasma = Plasma(256, 256)
plasma.generate
plasma.save_as('plasma.png')
```

Output image: [https://github.com/trizen/rc/blob/master/img/plasma-effect-sidef.png Plasma effect]
