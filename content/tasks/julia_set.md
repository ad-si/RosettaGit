+++
title = "Julia set"
description = ""
date = 2019-10-14T01:08:24Z
aliases = []
[extra]
id = 20208
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "awk",
  "basic",
  "c",
  "cobol",
  "cpp",
  "crystal",
  "csharp",
  "easylang",
  "elixir",
  "fsharp",
  "go",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "mathematica",
  "perl",
  "perl_6",
  "phix",
  "processing",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sidef",
  "sinclair_zx81_basic",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Generate and draw a [[wp:Julia_set|Julia set]].




## Related tasks

*   [[Mandelbrot_set|Mandelbrot Set]]





## ALGOL 68

{{trans|AWK}} (which is itself a translation of COBOL).


Uses the Algol 68G specific argc and argv procedures. Note argv( 1 ) is the path of the Algol 68G interpreter and argv( 2 ) is the source being executed.

```algol68
BEGIN
    REAL c real, c imaginary;
    STRING real and imaginary := IF argc < 3 THEN "-0.8"  ELSE argv( 3 ) FI
                               + " "
                               + IF argc < 4 THEN "0.156" ELSE argv( 4 ) FI
                               + " "
                               ;
    FILE numbers;
    associate( numbers, real and imaginary );
    get( numbers, ( c real, c imaginary ) );
    print( ( fixed( c real, -8, 4 ), fixed( c imaginary, -8, 4 ), newline ) );
    FOR v FROM -100 BY 10 TO 100 DO
        FOR h FROM -280 BY 10 TO 280 DO
            REAL x := h / 200;
            REAL y := v / 100;
            CHAR plot char := "#";
            FOR i TO 50
            WHILE
                REAL z real      = ( x * x ) - ( y * y ) + c real;
                REAL z imaginary = ( x * y * 2 ) + c imaginary;
                IF z real * z real <= 10000
                THEN TRUE
                ELSE
                    plot char := " ";
                    FALSE
                FI
            DO
                x := z real;
                y := z imaginary
            OD;
            print( ( plot char ) )
        OD;
        print( ( newline ) )
    OD
END
```

```txt

 -0.8000  0.1560



                            # #
                           # #
                        ####   ####
                       ###### ######## #
         ##            ######## ##  # #     ########
            ## #      #########  #      #  ##### # #
        ######## ###    ########     # #    ### #  ## # #
  ####  #####       #     #####     #       #####  ####
# # ##  # ###    # #     ########    ### ########
     # # #####  #      #  #########      # ##
     ########     # #  ## ########            ##
                 # ######## ######
                      ####   ####
                           # #
                          # #




```



## AWK

```AWK

# syntax: GAWK -f JULIA_SET.AWK [real imaginary]
BEGIN {
    c_real      = (ARGV[1] != "") ? ARGV[1] : -0.8
    c_imaginary = (ARGV[2] != "") ? ARGV[2] : 0.156
    printf("%s %s\n",c_real,c_imaginary)
    for (v=-100; v<=100; v+=10) {
      for (h=-280; h<=280; h+=10) {
        x = h / 200
        y = v / 100
        plot_char = "#"
        for (i=1; i<=50; i++) {
          z_real = x * x - y * y + c_real
          z_imaginary = x * y * 2 + c_imaginary
          if (z_real ^ 2 > 10000) {
            plot_char = " "
            break
          }
          x = z_real
          y = z_imaginary
        }
        printf("%1s",plot_char)
      }
      printf("\n")
    }
    exit(0)
}

```

```txt

-0.8 0.156



                            # #
                           # #
                        ####   ####
                       ###### ######## #
         ##            ######## ##  # #     ########
            ## #      #########  #      #  ##### # #
        ######## ###    ########     # #    ### #  ## # #
  ####  #####       #     #####     #       #####  ####
# # ##  # ###    # #     ########    ### ########
     # # #####  #      #  #########      # ##
     ########     # #  ## ########            ##
                 # ######## ######
                      ####   ####
                           # #
                          # #

```



## BASIC


=
## Sinclair ZX81 BASIC
=
I don't know exactly how long this takes to run; but I left it for about three and a half hours and when I came back it had already finished. If you can't wait to see the results, I've posted a screenshot [http://edmundgriffiths.com/zxjulia.jpg here]. I also haven't tested it with only 1k of RAM—but I suspect it needs at least 2k.

You can try changing lines 10 and 20 to run the program with different values of the complex constant <tt>C</tt>+<tt>D</tt><math>i</math>, or lines 50 and 60 to zoom in.

```basic
 10 LET C=-.8
 20 LET D=.156
 30 FOR V=43 TO 0 STEP -1
 40 FOR H=0 TO 63
 50 LET X=(H-32)/21
 60 LET Y=(V-22)/21
 70 FOR A=1 TO 50
 80 LET R=X*X-Y*Y+C
 90 LET I=2*X*Y+D
100 IF R*R>1000 THEN GOTO 150
110 LET X=R
120 LET Y=I
130 NEXT A
140 PLOT H,V
150 NEXT H
160 NEXT V
```


=
## ZX Spectrum Basic
=
Higher resolution is obtainable, if you have the time to wait for it.

```zxbasic
 10 LET creal=-0.8
 20 LET cimag=0.156
 30 FOR v=-16 TO 16
 40 FOR h=-64 TO 64
 50 LET x=h/40
 60 LET y=v/20
 70 FOR i=1 TO 50
 80 LET zreal=x*x-y*y+creal
 90 LET zimag=x*y*2+cimag
100 IF zreal*zreal>1000 THEN GO TO 150
110 LET x=zreal
120 LET y=zimag
130 NEXT i
140 PLOT h+100,150-v
150 NEXT h
160 NEXT v
```

Screenshot [http://edmundgriffiths.com/spectrumjulia.jpg here].


## C

Interactive implementation which takes the following 6 parameters as input :

```txt

<executable name> <width of graphics window> <height of graphics window> <real part of complex number> <imag part of complex number> <limiting radius> <Number of iterations to be tested>

```

Prints out usage on incorrect invocation. Requires the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library.

```C

#include<graphics.h>
#include<stdlib.h>
#include<math.h>

typedef struct{
	double x,y;
}complex;

complex add(complex a,complex b){
	complex c;
	c.x = a.x + b.x;
	c.y = a.y + b.y;
	return c;
}

complex sqr(complex a){
	complex c;
	c.x = a.x*a.x - a.y*a.y;
	c.y = 2*a.x*a.y;
	return c;
}

double mod(complex a){
	return sqrt(a.x*a.x + a.y*a.y);
}

complex mapPoint(int width,int height,double radius,int x,int y){
	complex c;
	int l = (width<height)?width:height;

	c.x = 2*radius*(x - width/2.0)/l;
	c.y = 2*radius*(y - height/2.0)/l;

	return c;
}

void juliaSet(int width,int height,complex c,double radius,int n){
	int x,y,i;
	complex z0,z1;

	for(x=0;x<=width;x++)
		for(y=0;y<=height;y++){
			z0 = mapPoint(width,height,radius,x,y);
			for(i=1;i<=n;i++){
				z1 = add(sqr(z0),c);
				if(mod(z1)>radius){
					putpixel(x,y,i%15+1);
					break;
				}
				z0 = z1;
			}
			if(i>n)
				putpixel(x,y,0);
		}
}

int main(int argC, char* argV[])
{
	int width, height;
	complex c;

	if(argC != 7)
		printf("Usage : %s <width and height of screen, real and imaginary parts of c, limit radius and iterations>");
	else{
		width = atoi(argV[1]);
		height = atoi(argV[2]);

		c.x = atof(argV[3]);
		c.y = atof(argV[4]);

		initwindow(width,height,"Julia Set");
		juliaSet(width,height,c,atof(argV[5]),atoi(argV[6]));

		getch();
	}

	return 0;
}

```



## C++

[[File:JuliaSetCpp.png|200px|thumb|right]]

```cpp

#include <windows.h>
#include <string>
#include <complex>

const int BMP_SIZE = 600, ITERATIONS = 512;
const long double FCT = 2.85, hFCT = FCT / 2.0;

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
        HANDLE file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS,
            FILE_ATTRIBUTE_NORMAL, NULL );
        WriteFile( file, &fileheader, sizeof( BITMAPFILEHEADER ), &wb, NULL );
        WriteFile( file, &infoheader.bmiHeader, sizeof( infoheader.bmiHeader ), &wb, NULL );
        WriteFile( file, dwpBits, bitmap.bmWidth * bitmap.bmHeight * 4, &wb, NULL );
        CloseHandle( file );
        delete [] dwpBits;
    }
    HDC getDC() const     { return hdc; }
    int getWidth() const  { return width; }
    int getHeight() const { return height; }
    DWORD* bits() const { return ( DWORD* )pBits; }
private:
    void createPen() {
        if( pen ) DeleteObject( pen );
        pen = CreatePen( PS_SOLID, wid, clr );
        SelectObject( hdc, pen );
    }
    HBITMAP bmp; HDC    hdc;
    HPEN    pen; HBRUSH brush;
    void    *pBits; int    width, height, wid;
    DWORD    clr;
};
class julia {
public:
    void draw( std::complex<long double> k ) {
        bmp.create( BMP_SIZE, BMP_SIZE );
        DWORD* bits = bmp.bits();
        int res, pos;
        std::complex<long double> c, factor( FCT / BMP_SIZE, FCT / BMP_SIZE ) ;

        for( int y = 0; y < BMP_SIZE; y++ ) {
            pos = y * BMP_SIZE;

            c.imag( ( factor.imag() * y ) + -hFCT );

            for( int x = 0; x < BMP_SIZE; x++ ) {
                c.real( factor.real() * x + -hFCT );
                res = inSet( c, k );
                if( res ) {
                    int n_res = res % 255;
                    if( res < ( ITERATIONS >> 1 ) ) res = RGB( n_res << 2, n_res << 3, n_res << 4 );
                    else res = RGB( n_res << 4, n_res << 2, n_res << 5 );
                }
                bits[pos++] = res;
            }
        }
        bmp.saveBitmap( "./js.bmp" );
    }
private:
    int inSet( std::complex<long double> z, std::complex<long double> c ) {
        long double dist;//, three = 3.0;
        for( int ec = 0; ec < ITERATIONS; ec++ ) {
            z = z * z; z = z + c;
            dist = ( z.imag() * z.imag() ) + ( z.real() * z.real() );
            if( dist > 3 ) return( ec );
        }
        return 0;
    }
    myBitmap bmp;
};
int main( int argc, char* argv[] ) {
    std::complex<long double> c;
    long double factor = FCT / BMP_SIZE;
    c.imag( ( factor * 184 ) + -1.4 );
    c.real( ( factor * 307 ) + -2.0 );
    julia j; j.draw( c ); return 0;
}

```


## C#
```c#
using System.Drawing;
// Note: You have to add the System.Drawing assembly
//  (right-click "references," Add Reference, Assemblies, Framework,
//    System.Drawing, OK)
using System.Linq;

namespace RosettaJuliaSet
{
    class Program
    {
        static void Main(string[] args)
        {
            const int w = 800;
            const int h = 600;
            const int zoom = 1;
            const int maxiter = 255;
            const int moveX = 0;
            const int moveY = 0;
            const double cX = -0.7;
            const double cY = 0.27015;
            double zx, zy, tmp;
            int i;

            var colors = (from c in Enumerable.Range(0, 256)
                          select Color.FromArgb((c >> 5) * 36, (c >> 3 & 7) * 36, (c & 3) * 85)).ToArray();

            var bitmap = new Bitmap(w, h);
            for (int x = 0; x < w; x++)
            {
                for (int y = 0; y < h; y++)
                {
                    zx = 1.5 * (x - w / 2) / (0.5 * zoom * w) + moveX;
                    zy = 1.0 * (y - h / 2) / (0.5 * zoom * h) + moveY;
                    i = maxiter;
                    while (zx * zx + zy * zy < 4 && i > 1)
                    {
                        tmp = zx * zx - zy * zy + cX;
                        zy = 2.0 * zx * zy + cY;
                        zx = tmp;
                        i -= 1;
                    }
                    bitmap.SetPixel(x, y, colors[i]);
                }
            }
            bitmap.Save("julia-set.png");
        }
    }
}

```


C# also makes it relatively easy to do a multi-threaded version, which should run faster than the above:


```c#

        public struct CalculatedPoint
        {
            public int x;
            public int y;
            public int i;
        }

        static void MultiThreaded()
        {
            const int w = 800;
            const int h = 600;
            const int zoom = 1;
            const int maxiter = 255;
            const int moveX = 0;
            const int moveY = 0;
            const double cX = -0.7;
            const double cY = 0.27015;

            // Precalculate a pallette of 256 colors
            var colors = (from c in Enumerable.Range(0, 256)
                          select Color.FromArgb((c >> 5) * 36, (c >> 3 & 7) * 36, (c & 3) * 85)).ToArray();

            // The "AsParallel" below invokes PLINQ, making evaluation parallel using as many cores as
            // are available.
            var calculatedPoints = Enumerable.Range(0, w * h).AsParallel().Select(xy =>
              {
                  double zx, zy, tmp;
                  int x, y;
                  int i = maxiter;
                  y = xy / w;
                  x = xy % w;
                  zx = 1.5 * (x - w / 2) / (0.5 * zoom * w) + moveX;
                  zy = 1.0 * (y - h / 2) / (0.5 * zoom * h) + moveY;
                  while (zx * zx + zy * zy < 4 && i > 1)
                  {
                      tmp = zx * zx - zy * zy + cX;
                      zy = 2.0 * zx * zy + cY;
                      zx = tmp;
                      i -= 1;
                  }
                  return new CalculatedPoint { x = x, y = y, i = i };
              });

            // Bitmap is not multi-threaded, so main thread needs to read in the results as they
            // come in and plot the pixels.
            var bitmap = new Bitmap(w, h);
            foreach (CalculatedPoint cp in calculatedPoints)
                bitmap.SetPixel(cp.x, cp.y, colors[cp.i]);
            bitmap.Save("julia-set-multi.png");
        }
```



## COBOL

Plots—in ASCII or EBCDIC art—a Julia set for the function <i>f</i>(<i>z</i>) = <i>z</i><sup>2</sup> + <i>c</i>, based on a value of <i>c</i> input by the user (real part then imaginary part, pressing the carriage return key after each). The sample output is for the inputs <tt>-0.8</tt> and <tt>0.156</tt>.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. JULIA-SET-PROGRAM.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-COMPLEX-CONSTANT.
    05 C-REAL             PIC S9V999.
    05 C-IMAGINARY        PIC S9V999.
01  WS-ARGAND-PLANE.
    05 X                  PIC S9(9)V999.
    05 Y                  PIC S9(9)V999.
01  WS-COMPLEX-VARIABLE.
    05 Z-REAL             PIC S9(9)V999.
    05 Z-IMAGINARY        PIC S9(9)V999.
01  WS-TEMPORARY-RESULTS.
    05 X-SQUARED          PIC S9(9)V999.
    05 Y-SQUARED          PIC S9(9)V999.
    05 X-TIMES-Y          PIC S9(9)V999.
    05 Z-REAL-SQUARED     PIC S9(9)V999.
01  WS-LOOP-COUNTERS.
    05 HORIZONTAL         PIC 999.
    05 VERTICAL           PIC 999.
    05 ITERATIONS         PIC 99.
77  WS-PLOT-CHARACTER     PIC X.
PROCEDURE DIVISION.
INPUT-COMPLEX-CONSTANT-PARAGRAPH.
    ACCEPT C-REAL      FROM CONSOLE.
    ACCEPT C-IMAGINARY FROM CONSOLE.
CONTROL-PARAGRAPH.
    PERFORM OUTER-LOOP-PARAGRAPH  VARYING VERTICAL   FROM 1 BY 10
    UNTIL VERTICAL IS GREATER THAN 320.
    STOP RUN.
OUTER-LOOP-PARAGRAPH.
    PERFORM COMPUTATION-PARAGRAPH VARYING HORIZONTAL FROM 1 BY 10
    UNTIL HORIZONTAL IS GREATER THAN 560.
    DISPLAY '' UPON CONSOLE.
COMPUTATION-PARAGRAPH.
    SUBTRACT 280   FROM HORIZONTAL GIVING X.
    SUBTRACT 160   FROM VERTICAL   GIVING Y.
    DIVIDE   X     BY   200        GIVING X.
    DIVIDE   Y     BY   100        GIVING Y.
    MOVE     '#'   TO   WS-PLOT-CHARACTER.
    PERFORM COMPLEX-MULTIPLICATION-PARAGRAPH
    VARYING ITERATIONS FROM   1  BY      1
    UNTIL   ITERATIONS        IS GREATER THAN 50
    OR      WS-PLOT-CHARACTER IS EQUAL   TO   SPACE.
    DISPLAY WS-PLOT-CHARACTER UPON CONSOLE WITH NO ADVANCING.
COMPLEX-MULTIPLICATION-PARAGRAPH.
    MULTIPLY X         BY   X         GIVING X-SQUARED.
    MULTIPLY Y         BY   Y         GIVING Y-SQUARED.
    SUBTRACT Y-SQUARED FROM X-SQUARED GIVING Z-REAL.
    ADD      C-REAL    TO   Z-REAL.
    MULTIPLY X         BY   Y         GIVING X-TIMES-Y.
    MULTIPLY X-TIMES-Y BY   2         GIVING Z-IMAGINARY.
    ADD C-IMAGINARY    TO   Z-IMAGINARY.
    MULTIPLY Z-REAL    BY   Z-REAL    GIVING Z-REAL-SQUARED.
    IF  Z-REAL-SQUARED IS   GREATER   THAN   10000 THEN
    MOVE SPACE         TO   WS-PLOT-CHARACTER.
    MOVE Z-REAL        TO   X.
    MOVE Z-IMAGINARY   TO   Y.
```

```txt










                            ###
                          ## ##
                       # ###   #### #
                      ################        ###
         #    #      # ############### #    #######
       ##   #####      ######## #       #  #####      ##
       ########  ###    ########    # ###   ### # ##  #
  ## #  ####     # #    # ####      #        ####  ### #
  # #   ###      # #     #########    # ## ######
  #  #########  #         #########    ######  #
     ### ###     ## ## ############           #
       #   #     ######### ## ## #
                    #####    ####
                           # #
                          ###
                           #








```



## Crystal

```ruby
require "complex"

def julia(c_real, c_imag)
  puts Complex.new(c_real, c_imag)
  -1.0.step(to: 1.0, by: 0.04) do |v|
    puts -1.4.step(to: 1.4, by: 0.02).map{|h| judge(c_real, c_imag, h, v)}.join
  end
end

def judge(c_real, c_imag, x, y)
  50.times do
    z_real = (x * x - y * y) + c_real
    z_imag = x * y * 2 + c_imag
    return " "  if z_real**2 > 10000
    x, y = z_real, z_imag
  end
  "#"
end

julia(-0.8, 0.156)

```

```txt

-0.8 + 0.156i




                                                                        #
                                                                         #
                                                                      ##
                                                                     #######
                                                                      ##  ####
                                                                            ##
                                                                # ####  ##  #
                                                        #      #  ###   ###       # ##     #
                                                         #    ######          ##########  # #
                                                         # #   ######       ########## #
                                                         #      ###  ### # ############  ######                         #
                                                        ################   ###################   #               #   ###  #
                                                         ################ #################### ##              #  ###   ##
                         ##                            ##############################    ###  #### #          #######     ###
                       #  ##        ##               #  #####################  # #### ##    #### ###        ########### ######
                       # #    #####                     #####################     #              # ##       ######################
                     ###    ####### ####  #            #######################    #                ###      ############  ###   ##      ##
                  ## ##  ## #################          ########################                     ###      ###########         ##   ### ##
                  #################  ##  ###### #        # ################### #             #####   ##       ######## #      ### #  ######
                  ############  ## ##       # ###            ##############   #            ##  ###   ##       #   #### #      # ##     ###
        ##        ###########  ##             ####          #############    ## #         ##   ### ##         ## #######      ##      #####
     #########    #############                 ###           #  # ####### #  #           ###                 #############    #########
  #####      ##      ####### ##         ## ###   ##         # ##    #############          ####             ##  ###########        ##
   ###     ## #      # ####   #       ##   ###  ##            #   ##############            ### #       ## ##  ############
  ######  # ###      # ########       ##   #####             # ################### #        # ######  ##  #################
### ###   ##         ###########      ###                     ########################          ################# ##  ## ##
   ##      ##   ###  ############      ###                #    #######################            #  #### #######    ###
           ######################       ## #              #     #####################                     #####    # #
               ###### ###########        ### ####    ## #### #  #####################  #               ##        ##  #
                ###     #######          # ####  ###    ##############################                            ##
                   ##   ###  #              ## #################### ################
                  #  ###   #               #   ###################   ################
                    #                         ######  ############ # ###  ###      #
                                                     # ##########       ######   # #
                                                # #  ##########          ######    #
                                                 #     ## #       ###   ###  #      #
                                                                #  ##  #### #
                                                               ##
                                                               ####  ##
                                                                 #######
                                                                     ##
                                                                   #
                                                                    #


```



## EasyLang


[https://easylang.online/apps/julia-set.html Run it]

<lang>floatvars
cx = -0.7
cy = 0.27015
for y% range 300
  for x% range 300
    iter% = 0
    zx = (x% - 150) / 100
    zy = (y% - 150) / 150
    while zx * zx + zy * zy < 4 and iter% < 128
      h = zx * zx - zy * zy + cx
      zy = 2 * zx * zy + cy
      zx = h
      iter% += 1
    .
    if iter% = 128
      color_red 0
    else
      color_red iter% / 16
    .
    move x% / 3 y% / 3
    rect 0.4 0.4
  .
.
```



## Elixir

```elixir
defmodule Julia do
  def set(c_real, c_imag) do
    IO.puts "#{c_real}, #{c_imag}"
    vlist = Enum.take_every(-100..100, 4)
    hlist = Enum.take_every(-280..280, 4)
    Enum.each(vlist, fn v ->
      Enum.map(hlist, fn h ->
        loop(c_real, c_imag, h/200, v/100, "#", 0)
      end) |> IO.puts
    end)
  end

  defp loop(_, _, _, _, char, i) when i>=50, do: char
  defp loop(_, _, _, _, " ", _), do: " "
  defp loop(c_real, c_imag, x, y, char, i) do
    z_real = (x * x - y * y) + c_real
    z_imag = x * y * 2 + c_imag
    char = if z_real * z_real > 10000, do: " ", else: char
    loop(c_real, c_imag, z_real, z_imag, char, i+1)
  end
end

c_real = if r=Enum.at(System.argv, 0), do: Float.parse(r) |> elem(0), else: -0.8
c_imag = if c=Enum.at(System.argv, 1), do: Float.parse(c) |> elem(0), else: 0.156
Julia.set(c_real, c_imag)
```


```txt
-0.8, 0.156




                                                                        #
                                                                         #
                                                                      ##
                                                                     #######
                                                                      ##  ####
                                                                            ##
                                                                # ####  ##  #
                                                        #      #  ###   ###       # ##     #
                                                         #    ######          ##########  # #
                                                         # #   ######       ########## #
                                                         #      ###  ### # ############  ######                         #
                                                        ################   ###################   #               #   ###  #
                                                         ################ #################### ##              #  ###   ##
                         ##                            ##############################    ###  #### #          #######     ###
                       #  ##        ##               #  #####################  # #### ##    #### ###        ########### ######
                       # #    #####                     #####################     #              # ##       ######################
                     ###    ####### ####  #            #######################    #                ###      ############  ###   ##      ##
                  ## ##  ## #################          ########################                     ###      ###########         ##   ### ###
                  #################  ##  ###### #        # ################### #             #####   ##       ######## #      ### #  ######
                  ############  ## ##       # ###            ##############   #            ##  ###   ##       #   #### #      # ##     ###
        ##        ###########  ##             ####          #############    ## #         ##   ### ##         ## #######      ##      #####
     #########    #############                 ###           #  # ####### #  #           ###                 #############    #########
  #####      ##      ####### ##         ## ###   ##         # ##    #############          ####             ##  ###########        ##
   ###     ## #      # ####   #       ##   ###  ##            #   ##############            ### #       ## ##  ############
  ######  # ###      # ########       ##   #####             # ################### #        # ######  ##  #################
### ###   ##         ###########      ###                     ########################          ################# ##  ## ##
   ##      ##   ###  ############      ###                #    #######################            #  #### #######    ###
           ######################       ## #              #     #####################                     #####    # #
               ###### ###########        ### ####    ## #### #  #####################  #               ##        ##  #
                ###     #######          # ####  ###    ##############################                            ##
                   ##   ###  #              ## #################### ################
                  #  ###   #               #   ###################   ################
                    #                         ######  ############ # ###  ###      #
                                                     # ##########       ######   # #
                                                # #  ##########          ######    #
                                                 #     ## #       ###   ###  #      #
                                                                #  ##  #### #
                                                               ##
                                                               ####  ##
                                                                 #######
                                                                     ##
                                                                   #
                                                                    #





```




## F#

''' Basic generation code '''

```fsharp

let getJuliaValues width height centerX centerY zoom maxIter =
  let initzx x = 1.5 * float(x - width/2) / (0.5 * zoom * float(width))
  let initzy y = 1.0 * float(y - height/2) / (0.5 * zoom * float(height))
  let calc y x =
    let rec loop i zx zy =
      if i=maxIter then 0
      elif zx*zx + zy*zy >= 4.0 then i
      else loop (i + 1) (zx*zx - zy*zy + centerX) (2.0*zx*zy + centerY)
    loop 0 (initzx x) (initzy y)
  [0..height-1] |> List.map(fun y->[0..width-1] |> List.map (calc y))

```


''' Text display '''

```fsharp

getJuliaValues 80 25 -0.7 0.27015 1.0 50
|> List.map(fun row-> row |> List.map (function | 0 ->" " |_->".") |> String.concat "")
|> List.iter (printfn "%s")

```

```txt

................................................................................
.......................................... .....................................
........................................   .....................................
.........................................    ...................................
......................................        ..................................
...................................          .....   ...........................
..................................   .         .     .   ...... .    ...........
................................... .....     ..        ...... .    . ..........
................. ....... .............                  .. ....      . ........
.................    . .  ... ............                      .        .. ....
..............        .   .   ..  .     .                                .     .
.................                         ..                  .  ..       ......
....... .. . ....                                               .... . .. ......
.......       ..  .                  ..                         ................
..     .                                .     .  ..   .   .        .............
..... ..        .                      ............ ...  . .    ................
......... .      .... ..                  ............. ....... ................
........... .    . ......        ..     ..... ..................................
............    . ......   .     .         .   .................................
............................   .....          ..................................
...................................        .....................................
....................................    ........................................
......................................   .......................................
...................................... .........................................
................................................................................

```


''' Graphic Display '''

```fsharp

open System.Drawing
open System.Windows.Forms

let showGraphic (colorForIter: int -> Color) width height centerX centerY zoom maxIter =
  new Form()
  |> fun frm ->
    frm.Width <- width
    frm.Height <- height
    frm.BackgroundImage <-
      new Bitmap(width,height)
      |> fun bmp ->
        getJuliaValues width height centerX centerY zoom maxIter
        |> List.mapi (fun y row->row |> List.mapi (fun x v->((x,y),v))) |> List.collect id
        |> List.iter (fun ((x,y),v) -> bmp.SetPixel(x,y,(colorForIter v)))
        bmp
    frm.Show()

let toColor = (function | 0 -> (0,0,0) | n -> ((31 &&& n) |> fun x->(0, 18 + x * 5, 36 + x * 7))) >> Color.FromArgb

showGraphic toColor 640 480 -0.7 0.27015 1.0 5000

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Julia_set this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go

Using the Goroutines results in a performance improvement of about three times on my four-core machine.

```Go
package main

import (
	"image"
	"image/color"
	"image/png"
	"log"
	"os"
	"sync"
)

func main() {
	const (
		width, height = 800.0, 600.0
		maxIter       = 255
		cX, cY        = -0.7, 0.27015
		fileName      = "julia.png"
	)
	img := image.NewNRGBA(image.Rect(0, 0, width, height))

	var wg sync.WaitGroup
	wg.Add(width)
	for x := 0; x < width; x++ {
		thisx := float64(x)
		go func() {
			var tmp, zx, zy float64
			var i uint8
			for y := 0.0; y < height; y++ {
				zx = 1.5 * (thisx - width/2) / (0.5 * width)
				zy = (y - height/2) / (0.5 * height)
				i = maxIter
				for zx*zx+zy*zy < 4.0 && i > 0 {
					tmp = zx*zx - zy*zy + cX
					zy = 2.0*zx*zy + cY
					zx = tmp
					i--
				}
				img.Set(int(thisx), int(y), color.RGBA{i, i, i << 3, 255})
			}
			wg.Done()
		}()
	}
	wg.Wait()
	imgFile, err := os.Create(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer imgFile.Close()
	if err := png.Encode(imgFile, img); err != nil {
		imgFile.Close()
		log.Fatal(err)
	}
}
```



## J

[[File:example-j-julia.png|200px|thumb|right]]

```j
load '~addons/graphics/fvj4/complex_dynamics.ijs'
pal2=: 255,~0,<.(254$1 0.8 0.6)*Hue 5r6*(i.%<:)254
g=: [: %: 0.3746j0.102863 0.132565j0.389103 _0.373935j_0.353777 1&p.
view_image pal2;b=:g escapetc (10 255) 500 zl_clur _1.5 1.5j1.5
```


See also: [http://webbox.lafayette.edu/~reiterc/j/fvj4/index.html Fractals Visualization and J, 4th edition, Part 1] (by Clifford A. Reiter), Chapter 6

See http://webbox.lafayette.edu/~reiterc/mvp/ec_julia/index.html for some other examples. (That said, note that this is a link into a small college site and it might drift over time. In the past, for example, you would have had to use 'www' where it currently says 'webbox')


## Java

[[File:julia_set_java.png|200px|thumb|right]]
```java
import java.awt.*;
import java.awt.image.BufferedImage;
import javax.swing.*;

public class JuliaSet extends JPanel {
    private final int maxIter = 300;
    private final double zoom = 1;
    private double cY, cX;

    public JuliaSet() {
        setPreferredSize(new Dimension(800, 600));
        setBackground(Color.white);
    }

    void drawJuliaSet(Graphics2D g) {
        int w = getWidth();
        int h = getHeight();
        BufferedImage image = new BufferedImage(w, h,
                BufferedImage.TYPE_INT_RGB);

        cX = -0.7;
        cY = 0.27015;
        double moveX = 0, moveY = 0;
        double zx, zy;

        for (int x = 0; x < w; x++) {
            for (int y = 0; y < h; y++) {
                zx = 1.5 * (x - w / 2) / (0.5 * zoom * w) + moveX;
                zy = (y - h / 2) / (0.5 * zoom * h) + moveY;
                float i = maxIter;
                while (zx * zx + zy * zy < 4 && i > 0) {
                    double tmp = zx * zx - zy * zy + cX;
                    zy = 2.0 * zx * zy + cY;
                    zx = tmp;
                    i--;
                }
                int c = Color.HSBtoRGB((maxIter / i) % 1, 1, i > 0 ? 1 : 0);
                image.setRGB(x, y, c);
            }
        }
        g.drawImage(image, 0, 0, null);
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        drawJuliaSet(g);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Julia Set");
            f.setResizable(false);
            f.add(new JuliaSet(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

take a look [http://paulo-jorente.de/tests/juliaset/ here].

```javascript

var maxIterations = 450, minX = -.5, maxX = .5,
    minY = -.5, maxY = .5, wid, hei, ctx,
    jsX = 0.285, jsY = 0.01;

function remap( x, t1, t2, s1, s2 ) {
    var f = ( x - t1 ) / ( t2 - t1 ),
        g = f * ( s2 - s1 ) + s1;
    return g;
}
function getColor( c ) {
    var r, g, b, p = c / 32,
        l = ~~( p * 6 ), o = p * 6 - l,
        q = 1 - o;

    switch( l % 6 ) {
        case 0: r = 1; g = o; b = 0; break;
        case 1: r = q; g = 1; b = 0; break;
        case 2: r = 0; g = 1; b = o; break;
        case 3: r = 0; g = q; b = 1; break;
        case 4: r = o; g = 0; b = 1; break;
        case 5: r = 1; g = 0; b = q; break;
    }
    var c = "#" + ( "00" + ( ~~( r * 255 ) ).toString( 16 ) ).slice( -2 ) +
                  ( "00" + ( ~~( g * 255 ) ).toString( 16 ) ).slice( -2 ) +
                  ( "00" + ( ~~( b * 255 ) ).toString( 16 ) ).slice( -2 );
    return (c);
}
function drawFractal() {
    var a, as, za, b, bs, zb, cnt, clr
    for( var j = 0; j < hei; j++ ) {
        for( var i = 0; i < wid; i++ ) {
            a = remap( i, 0, wid, minX, maxX )
            b = remap( j, 0, hei, minY, maxY )
            cnt = 0;
            while( ++cnt < maxIterations ) {
                za = a * a; zb = b * b;
                if( za + zb > 4 ) break;
                as = za - zb; bs = 2 * a * b;
                a = as + jsX; b = bs + jsY;
            }
            if( cnt < maxIterations ) {
                ctx.fillStyle = getColor( cnt );
            }
            ctx.fillRect( i, j, 1, 1 );
        }
    }
}
function init() {
    var canvas = document.createElement( "canvas" );
    wid = hei = 800;
    canvas.width = wid; canvas.height = hei;
    ctx = canvas.getContext( "2d" );
    ctx.fillStyle = "black"; ctx.fillRect( 0, 0, wid, hei );
    document.body.appendChild( canvas );
    drawFractal();
}

```



## jq

```jq
# Example values:
# $re : -0.8
# $im : 0.156
{}
| range(-100; 101; 10) as $v
| (( range (-280; 281; 10) as $h
  | .x = $h / 200
  | .y = $v / 100
  | .plot = "#"
  | .i = 0
  | until (.i == 50 or .plot == ".";
           .i += 1
           | .z_real = ((.x * .x) - (.y * .y) + $re)
           | .z_imag = ((.x * .y * 2) + $im)
 	   | if pow(.z_real; 2) > 10000 then .plot = " "
             else .x = .z_real | .y = .z_imag
   	     end )
  | .plot ), "\n")
```


With the above program in a file called julia.jq, the following invocation of jq 1.5  produces the same output as shown in the awk entry on this page:


```txt
jq -nrj -f julia.jq --argjson re -0.8 --argjson im 0.156

```


(If your jq does not support the --argjson options, then use --arg instead, and add the `tonumber` conversions at the beginning of the program.)


## Julia


The following code creates the fractal as a ppm file named julia.ppm.  There is no need of an external library to create this image since the ppm format is straightforward to generate.


```julia

function iter(z,c)
  n = 0
  while (abs2(z)<4)  z = z^2+c ; n+=1 end
  return n
end

coord(i,j,w,h,a,b) = 2*a*(i-1)/(w-1) - a + im * (2*b*(j-1)/(h-1) - b)

palette(n) = string(min(3n,255)," ", min(n,255)," ", 0);

julia(c) = (w,h,a,b,i,j) -> palette(iter(coord(i,j,w,h,a,b), c))

writeppm(f; width=600,height=300,a=2,b=1,file="julia.ppm") =
  open(file, "w") do out
    write(out, string("P3\n", width, " ", height, "\n255\n"))
    writedlm(out, [f(width,height,a,b,i,j) for j = 1:height, i = 1:width], '\n')
  end

```


We can then produce a 600x300 ppm image of the Julia set associated to the parameter ''-0.786+0.147i'' as follows.


```txt
writeppm(julia(-0.786+0.147im))
```


The following code makes use of the library '''Images''' to build a png image.

[[File:JuliaSet_julia.png|250px|thumb|right]]

```julia
using Images

const w, h = 800, 600
const img = Array{RGB{Float64}}(undef, h, w)

const maxIter = 50
const c = -0.8+0.156im

@inline function hsv2rgb(h, s, v)
    const c = v * s
    const x = c * (1 - abs(((h/60) % 2) - 1))
    const m = v - c

    const r,g,b =
        if h < 60
            (c, x, 0)
        elseif h < 120
            (x, c, 0)
        elseif h < 180
            (0, c, x)
        elseif h < 240
            (0, x, c)
        elseif h < 300
            (x, 0, c)
        else
            (c, 0, x)
        end

    (b + m), (g + m), (r + m)
end

for x in 1:w
    for y in 1:h
        i = maxIter
        z = Complex((x - w/2) / w * 3, (y - h/2) / h * 2)
        while abs(z) < 2 && (i -= 1) > 0
            z = z*z + c
        end
        r,g,b = hsv2rgb(i / maxIter * 360, 1, i > 1 ? 1 : 0)
        img[y,x] = RGB{Float64}(r, g, b)
    end
end

save("JuliaSet.png", img)
```



## Kotlin

```scala
import java.awt.*
import java.awt.image.BufferedImage
import javax.swing.JFrame

class Julia_panel : javax.swing.JPanel() {
    init {
        preferredSize = Dimension(800, 600)
        background = Color.white
    }

    public override fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        with (gg as Graphics2D) {
            setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
            val w = width
            val h = height
            val image = BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
            for (x in 0..w - 1) {
                for (y in 0..h - 1) {
                    var zx = 1.5 * (x - w / 2) / (0.5 * zoom * w) + moveX
                    var zy = (y - h / 2) / (0.5 * zoom * h) + moveY
                    var i = maxIter.toFloat()
                    while (zx * zx + zy * zy < 4 && i > 0) {
                        val tmp = zx * zx - zy * zy + cX
                        zy = 2.0 * zx * zy + cY
                        zx = tmp
                        i--
                    }
                    image.setRGB(x, y, Color.HSBtoRGB(maxIter / i % 1, 1f, (if (i > 0) 1 else 0).toFloat()))
                }
            }
            drawImage(image, 0, 0, null)
        }
    }

    private val maxIter = 300
    private val zoom = 1
    private val moveX = 0.0
    private val moveY = 0.0
    private val cY = -0.7
    private val cX = 0.27015
}

fun main(args: Array<String>) {
    with (JFrame()) {
        defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        title = "Julia Set"
        isResizable = false
        add(Julia_panel(), java.awt.BorderLayout.CENTER)
        pack()
        setLocationRelativeTo(null)
        isVisible = true
    }
}
```



## Mathematica

Mathematica provides built-in functions for Julia sets.
Generate the set of points for the -0.77 +0.22 I Julia set with step sizes of 0.01

```Mathematica
JuliaSetPoints[-0.77 + 0.22 I, "ClosenessTolerance" -> 0.01]
```

Visualize the same Julia set

```Mathematica
JuliaSetPlot[-0.77 + 0.22 I]
```



## Perl

[[File:JuliaSet.perl.png|250px|thumb|right]]

```perl
use Imager;

my($w, $h, $zoom) = (800, 600, 1);
my $img = Imager->new(xsize => $w, ysize => $h, channels => 3);

my $maxIter = 255;
my ($cX, $cY) = (-0.7, 0.27015);
my ($moveX, $moveY) = (0, 0);

my $color = Imager::Color->new('#000000');

foreach my $x (0 .. $w - 1) {
    foreach my $y (0 .. $h - 1) {
        my $zx = (1.5 * ($x - $w / 2) / (0.5 * $zoom * $w) + $moveX);
        my $zy = (($y - $h / 2) / (0.5 * $zoom * $h) + $moveY);
        my $i = $maxIter;
        while ($zx**2 + $zy**2 < 4 and --$i >= 0) {
            ($zy, $zx) = (2 * $zx * $zy + $cY, $zx**2 - $zy**2 + $cX);
        }
        $color->set(hsv => [$i / $maxIter * 360, 1, $i > 0 ? 1 : 0]);
        $img->setpixel(x => $x, y => $y, color => $color);
    }
}

$img->write(file => 'julia_set.png');
```



## Perl 6

{{trans|Perl}} with the pallette swapped, just because.
[[File:Julia-set-perl6.png|250px|thumb|right]]

```perl6
use Image::PNG::Portable;

my ($w, $h) = 800, 600;
my $out = Image::PNG::Portable.new: :width($w), :height($h);

my $maxIter = 255;
my $c = -0.7 + 0.27015i;

julia($out);

$out.write: 'Julia-set-perl6.png';

sub julia ( $png ) {
    ^$w .race.map: -> $x {
        for ^$h -> $y {
            my $z = Complex.new(($x - $w / 2) / $w * 3, ($y - $h / 2) / $h * 2);
            my $i = $maxIter;
            while (abs($z) < 2 and --$i) {
                $z = $z*$z + $c;
            }
            $png.set: $x, $y, |hsv2rgb($i / $maxIter, 1, ?$i).reverse;
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

Interactive gui (zoom/pan incomplete).

```Phix
-- demo\rosetta\Julia_set.exw
include pGUI.e

constant title = "Julia set"
Ihandle dlg, cxv, cxl, cyv, cyl, ispin, pspin, clrzn, label, bb, redraw

atom cX = -0.7,
     cY = -0.353777
integer iter = 255,
        pwr = 2,
        zoom = 1,       -- (not yet used/to do)
        moveX = 0,      -- drag?? (to do)
        moveY = 0

constant clrzns = {{8,32,16},
                   {2,4,8},
                   {1,1,8}}

sequence colourisation = clrzns[1]

function julia(integer width, integer height)
    atom tpt25 = time()+0.25
    sequence img = repeat(repeat(0,width),height)
    for x=1 to width do
        for y=1 to height do
            atom zx := 1.5*((x-1)-width/2)/(0.5*zoom*width)+moveX,
                 zy := 1.0*((y-1)-height/2)/(0.5*zoom*height)+moveY;
            integer i := iter;
            while ((zx*zx+zy*zy)<4) and (i>1) do
                atom pn = power(zx*zx+zy*zy,pwr/2),
                     pa = pwr*atan2(zy, zx)
                zx = pn*cos(pa)+cX
                zy = pn*sin(pa)+cY
                i -= 1;
            end while
--          img[y,x] = {i*2,i*4,i*8}        -- (experiment thusly)
            img[y,x] = sq_mul(i,colourisation)
        end for
        if time()>tpt25 then
            IupSetStrAttribute(dlg, "TITLE", "%s (generating - %3.2f%%)",{title,x/width*100})
            IupFlush()
            tpt25 = time()+0.25
        end if
    end for
    img = flatten(img)
    Ihandle new_img = IupImageRGB(width, height, img)
    return new_img
end function

function redraw_cb(Ihandln /*redraw*/)
    Ihandln image = IupGetAttributeHandle(label, "IMAGE")
    IupSetAttributeHandle(label, "IMAGE", NULL)
    if image!=NULL then IupDestroy(image) end if
    IupSetAttribute(redraw,"ACTIVE","NO")
    IupRefreshChildren(bb)
    integer {w,h} = IupGetIntInt(bb, "RASTERSIZE")
    image = julia(w,h)
    IupSetAttribute(redraw,"ACTIVE","YES")
    IupUnmap(label)
    IupSetAttribute(label,"RASTERSIZE",NULL)
    IupSetAttributeHandle(label, "IMAGE", image)
    IupMap(label)
    IupRefresh(label)
    IupSetStrAttribute(dlg, "TITLE", title)
    return IUP_DEFAULT
end function
constant cb_redraw = Icallback("redraw_cb")

function valuechanged_cb(Ihandle ih)
    atom a = IupGetFloat(ih, "VALUE")
    switch ih do
        case cxv:   cX = a  IupSetStrAttribute(cxl,"TITLE","cY: %f",{cX})
        case cyv:   cY = a  IupSetStrAttribute(cyl,"TITLE","cY: %f",{cY})
        case ispin: iter = a
        case pspin: pwr = a
        case clrzn: colourisation = clrzns[a]
    end switch
    return IUP_DEFAULT
end function
constant cb_valuechanged = Icallback("valuechanged_cb")

procedure create_dlg()

    Ihandle lx1 = IupLabel("+")
            cxl = IupLabel(sprintf("cX: %f",cX))
    Ihandle lx2 = IupLabel("-"),
            hx1 = IupHbox({lx1, IupFill(), cxl, IupFill(), lx2})
            cxv = IupValuator(NULL,"MIN=-2.5, MAX=+1")
    Ihandle bxv = IupVbox({hx1, cxv})

    Ihandle ly1 = IupLabel("+")
            cyl = IupLabel(sprintf("cY: %f",cY))
    Ihandle ly2 = IupLabel("-"),
            hx2 = IupHbox({ly1, IupFill(), cyl, IupFill(), ly2})
            cyv = IupValuator(NULL,"MIN=-1, MAX=+1")
    Ihandle byv = IupVbox({hx2, cyv})

    IupSetCallback(cxv, "VALUECHANGED_CB", cb_valuechanged)
    IupSetCallback(cyv, "VALUECHANGED_CB", cb_valuechanged)
    IupSetFloat(cxv, "VALUE", cX)
    IupSetFloat(cyv, "VALUE", cY)

    Ihandle ilbl = IupLabel("iter'ns:","PADDING=0x3")
            ispin = IupText("VALUECHANGED_CB", cb_valuechanged,
                            "SPIN=Yes, SPINMIN=1, SPINMAX=500, RASTERSIZE=48x")
    IupSetInt(ispin,"VALUE",iter)
    Ihandle ibox = IupHbox({IupFill(),ilbl,ispin,IupFill()})

    Ihandle plbl = IupLabel("power:","PADDING=0x3")
            pspin = IupText("VALUECHANGED_CB", cb_valuechanged,
                            "SPIN=Yes, SPINMIN=2, SPINMAX=6, RASTERSIZE=48x")
    IupSetInt(pspin,"VALUE",pwr)
    Ihandle pbox = IupHbox({IupFill(),plbl,pspin,IupFill()})

    Ihandle clbl = IupLabel("colourization:","PADDING=0x3")
    clrzn = IupList("DROPDOWN=YES")
    for i=1 to length(clrzns) do
        IupSetStrAttributeId(clrzn,"",i,sprint(clrzns[i]))
    end for
    IupSetInt(clrzn,"VISIBLEITEMS",length(clrzns)+1)
    IupSetInt(clrzn,"VALUE",1)
    IupSetCallback(clrzn, "VALUECHANGED_CB", cb_valuechanged)
    Ihandle cbox = IupHbox({IupFill(),IupVbox({clbl,clrzn}),IupFill()})

    redraw = IupButton("redraw",cb_redraw)
    Ihandle rbox = IupHbox({IupFill(),redraw,IupFill()},"EXPAND=YES, MARGIN=10x20")

    Ihandle params = IupVbox({bxv,byv,ibox,pbox,cbox,rbox},
                              "GAP=5, EXPAND=NO, EXPANDCHILDREN=YES, MARGIN=3x3")

    label = IupLabel("please wait...","ALIGNMENT=ACENTER:ACENTER, RASTERSIZE=800x600")
    bb = IupBackgroundBox(IupHbox({IupVbox({label,IupFill()}),IupFill()}),"EXPAND=YES, SHRINK=YES")

    dlg = IupDialog(IupHbox({params,bb}))
    IupSetAttribute(dlg, "TITLE", title)
    IupCloseOnEscape(dlg)
end procedure

procedure main()
    IupOpen()
    create_dlg()
    IupShow(dlg)
    {} = redraw_cb(NULL)
    IupMainLoop()
    IupClose()
end procedure
main()
```



## Processing


```java
void setup() {
  size(640, 480);
}

float cX = -0.7;
float cY = 0.27015;
float zx, zy;
float maxIter = 300;

void draw() {
  for (int x = 0; x < width; x++) {
    for (int y = 0; y < height; y++) {
      zx = 1.5 * (x - width / 2) / (0.5 * width);
      zy = (y - height / 2) / (0.5 * height);
      float i = maxIter;
      while (zx * zx + zy * zy < 4 && i > 0) {
        float tmp = zx * zx - zy * zy + cX;
        zy = 2.0 * zx * zy + cY;
        zx = tmp;
        i -= 1;
      }
      color c = hsv2rgb(i / maxIter * 360, 1, i > 1 ? 1 : 0);
      set(x, y, c);
    }
  }
  noLoop();
}

color hsv2rgb(float h, float s, float v) {
  float c = v * s;
  float x = c * (1 - abs(((h/60) % 2) - 1));
  float m = v - c;

  float r, g, b;
  if (h < 60) {
    r = c;
    g = x;
    b = 0;
  } else if (h < 120) {
    r = x;
    g = c;
    b = 0;
  } else if (h < 180) {
    r = 0;
    g = c;
    b = x;
  } else if (h < 240) {
    r = 0;
    g = x;
    b = c;
  } else if (h < 300) {
    r = x;
    g = 0;
    b = c;
  } else {
    r = c;
    g = 0;
    b = x;
  }

  int ri = round((r + m) * 255);
  int gi = round((g + m) * 255);
  int bi = round((b + m) * 255);

  return color(ri, gi, bi);
}
```



## Python


### Naive approach

```python
from PIL import Image

if __name__ == "__main__":
	w, h, zoom = 800,600,1
	bitmap = Image.new("RGB", (w, h), "white")
	pix = bitmap.load()

	cX, cY = -0.7, 0.27015
	moveX, moveY = 0.0, 0.0
	maxIter = 255

	for x in range(w):
		for y in range(h):
			zx = 1.5*(x - w/2)/(0.5*zoom*w) + moveX
			zy = 1.0*(y - h/2)/(0.5*zoom*h) + moveY
			i = maxIter
			while zx*zx + zy*zy < 4 and i > 1:
				tmp = zx*zx - zy*zy + cX
				zy,zx = 2.0*zx*zy + cY, tmp
				i -= 1
			# convert byte to RGB (3 bytes), kinda magic to get nice colors
			pix[x,y] = (i << 21) + (i << 10) + i*8

	bitmap.show()
```



### Vectorized

Efficient version using vectorized operations in NumPy.

[https://imgur.com/a/o3j9Zzn Example output.]

```python
"""
Solution from:
https://codereview.stackexchange.com/questions/210271/generating-julia-set
"""
from functools import partial
from numbers import Complex
from typing import Callable

import matplotlib.pyplot as plt
import numpy as np


def douady_hubbard_polynomial(z: Complex,
                              c: Complex) -> Complex:
    """
    Monic and centered quadratic complex polynomial
    https://en.wikipedia.org/wiki/Complex_quadratic_polynomial#Map
    """
    return z ** 2 + c


def julia_set(mapping: Callable[[Complex], Complex],
              *,
              min_coordinate: Complex,
              max_coordinate: Complex,
              width: int,
              height: int,
              iterations_count: int = 256,
              threshold: float = 2.) -> np.ndarray:
    """
    As described in https://en.wikipedia.org/wiki/Julia_set
    :param mapping: function defining Julia set
    :param min_coordinate: bottom-left complex plane coordinate
    :param max_coordinate: upper-right complex plane coordinate
    :param height: pixels in vertical axis
    :param width: pixels in horizontal axis
    :param iterations_count: number of iterations
    :param threshold: if the magnitude of z becomes greater
    than the threshold we assume that it will diverge to infinity
    :return: 2D pixels array of intensities
    """
    im, re = np.ogrid[min_coordinate.imag: max_coordinate.imag: height * 1j,
                      min_coordinate.real: max_coordinate.real: width * 1j]
    z = (re + 1j * im).flatten()

    live, = np.indices(z.shape)  # indexes of pixels that have not escaped
    iterations = np.empty_like(z, dtype=int)

    for i in range(iterations_count):
        z_live = z[live] = mapping(z[live])
        escaped = abs(z_live) > threshold
        iterations[live[escaped]] = i
        live = live[~escaped]
        if live.size == 0:
            break
    else:
        iterations[live] = iterations_count

    return iterations.reshape((height, width))


if __name__ == '__main__':
    mapping = partial(douady_hubbard_polynomial,
                      c=-0.7 + 0.27015j)  # type: Callable[[Complex], Complex]

    image = julia_set(mapping,
                      min_coordinate=-1.5 - 1j,
                      max_coordinate=1.5 + 1j,
                      width=800,
                      height=600)
    plt.axis('off')
    plt.imshow(image,
               cmap='nipy_spectral_r',
               origin='lower')
    plt.show()

```



## Racket

[[File:JuliaSet_racket.png|250px|thumb|right]]

```racket

;; Based on Mandelbrot code (GPL) from:
;;  https://github.com/hebr3/Mandelbrot-Set-Racket/blob/master/Mandelbrot.v6.rkt
;; Julia set algoithm (and coloring) from:
;;  http://lodev.org/cgtutor/juliamandelbrot.html
;; HSV code (GPL) based on:
;;  https://github.com/takikawa/pict-utils/blob/master/pict-utils/hsv.rkt

#lang racket

;; Required to generate image
(require picturing-programs)

;; CONSTANTS - NUMBERS
(define DEPTH  300)
(define WIDTH  800)
(define HEIGHT 600)

;; Structures
(struct posn [x y] #:transparent)

;; CONSTANTS - GRAPHIC
(define BACKGROUND (rectangle WIDTH HEIGHT 'solid 'grey))
(define jcnst (posn -0.7 0.27015))

;; PROCEDURES
;; make an RGB color from HSV values
(define (make-color/hsv hue saturation value)
  (define chroma (* saturation value))
  (define hue* (/ (remainder* hue (* 2 pi)) (/ pi 3)))
  (define X (* chroma (- 1 (abs (- (remainder* hue* 2) 1)))))
  (define-values (r1 g1 b1)
    (cond [(and (<= 0 hue*) (< hue* 1)) (values chroma X 0)]
          [(and (<= 1 hue*) (< hue* 2)) (values X chroma 0)]
          [(and (<= 2 hue*) (< hue* 3)) (values 0 chroma X)]
          [(and (<= 3 hue*) (< hue* 4)) (values 0 X chroma)]
          [(and (<= 4 hue*) (< hue* 5)) (values X 0 chroma)]
          [(and (<= 5 hue*) (< hue* 6)) (values chroma 0 X)]))
  (define m (- value chroma))
  (apply make-color (map (λ (x) (exact-round (* 255 (+ x m))))
                         (list r1 g1 b1))))

;; general remainder
(define (remainder* n1 n2)
  (define num-divides (/ n1 n2))
  (- n1 (* (floor num-divides) n2)))

;; Posn -> Number
;; Returns the magnitude of the posn
(define (posn-mag pt)
  (let ([pt-x (posn-x pt)]
        [pt-y (posn-y pt)])
    (sqrt (+ (* pt-x pt-x)
                 (* pt-y pt-y)))))

;; Posn Posn -> Posn
;; Posn addition
(define (posn+ pt1 pt2)
  (let ([pt1-x (posn-x pt1)]
        [pt1-y (posn-y pt1)]
        [pt2-x (posn-x pt2)]
        [pt2-y (posn-y pt2)])
    (posn (+ pt1-x pt2-x)
          (+ pt1-y pt2-y))))

;; Posn Posn -> Posn
;; Posn multiplication
(define (posn* pt1 pt2)
  (let ([x1 (posn-x pt1)]
        [y1 (posn-y pt1)]
        [x2 (posn-x pt2)]
        [y2 (posn-y pt2)])
    (posn (- (* x1 x2) (* y1 y2))
          (+ (* x1 y2) (* x2 y1)))))

;; Posn -> Posn
;; Posn square
(define (posn-sqr pt)
  (posn* pt pt))

;; Posn -> Number
;; Returns the julia set  escape number for a given complex number
;; given in rectangular coordinates.
(define (julia-set-number  start)
  (define (iter result count)
    (cond [(> (posn-mag result) 2) (sub1 count)]
          [(> count DEPTH) DEPTH]
          [else (iter (posn+ jcnst (posn-sqr result))
                      (add1 count))]))
  (iter start 1))

;; Number -> Number
;; Returns the scaled location of a point
(define (scaled-x x)
  (/ (* 1.5 (- x (/ WIDTH 2))) (* 0.5 WIDTH)))
(define (scaled-y y)
  (/ (- y (/ HEIGHT 2)) (* 0.5 HEIGHT)))

;; Generates image
(define M-Image
  (map-image
   (λ (x y c)
     (let* ([ref (julia-set-number  (posn (scaled-x x) (scaled-y y)))])
       (cond [(= ref DEPTH) (name->color 'black)]
             [else (make-color/hsv (* 2 (* pi (/ ref DEPTH))) 1 1)]) ))
 BACKGROUND))

M-Image ;show image if using drracket

(save-image M-Image "julias.png")

```



## REXX

:::: which is a
```rexx
/*REXX program  displays  an  ASCII plot   (character plot)   of a  Julia set.          */
parse arg real imag fine .                       /*obtain optional arguments from the CL*/
if real=='' | real==","  then real= -0.8         /*Not specified?  Then use the default.*/
if imag=='' | imag==","  then imag=  0.156       /* "      "         "   "   "     "    */
if fine=='' | fine==","  then fine= 50           /* "      "         "   "   "     "    */
_=scrsize(); parse var _ sd sw; sd=sd-4; sw=sw-1 /*obtain useable area for the terminal.*/
                                                 /*$:  the plot line that is constructed*/
         do   v= -sd%2  to sd%2;     $=          /*step through  vertical   axis values.*/
           do h= -sw%2  to sw%2                  /*  "     "    horizontal    "     "   */
           x=h/sw*2                              /*calculate the initial   X   value.   */
           y=v/sd*2                              /*    "      "     "      Y     "      */
           @='■';    do fine                     /*FINE: is the "fineness" for the plot.*/
                     zr=x*x - y*y + real         /*calculate a new   real   Julia point.*/
                     zi=x*y*2     + imag         /*    "     "  "  imaginal   "     "   */
                     if zr**2>10000  then do; @=' '; leave; end    /*is  ZR  too large? */
                     x=zr;    y=zi                                 /*use this new point.*/
                     end   /*50*/
           $=$ || @                              /*append the plot char to the plot line*/
           end            /*h*/
         if $\=''  then say strip($, 'T')        /*only display a plot line if non-blank*/
         end   /*v*/                             /*stick a fork in it,  we're all done. */
```

This REXX program makes use of   '''scrsize'''   REXX program (or BIF) which is used to determine the screen size of the terminal (console),

and the plot size is adjusted according.


The   '''SCRSIZE.REX'''   REXX program is included here   ──►   [[SCRSIZE.REX]].



(final)  '''output'''   when using the default input   (screen size was 200<small>x</small>420):

(Shown at   <sup>'''1'''</sup>/<sub>'''6'''</sub>   size.)

'''output'''   when the defaults are used:

```txt
                                                                                                                                                                                                                          ■■
                                                                                                                                                                                                                          ■ ■
                                                                                                                                                                                                                       ■■ ■■
                                                                                                                                                                                                                       ■■■■■■■ ■
                                                                                                                                                                                                                      ■■■■ ■■■
                                                                                                                                                                                                                             ■■■■
                                                                                                                                                                                                                      ■■■■■   ■■
                                                                                                                                                                                                                  ■■■■   ■■  ■■■
                                                                                                                                                                                                                ■■■■■     ■■■
                                                                                                                                                                                                        ■■■■   ■■■■■■              ■■   ■
                                                                                                                                                                                                         ■■■    ■■■■ ■■        ■■■■■■■   ■
                                                                                                                                                                                                          ■      ■■■■■■■■ ■■■■■■■■■■■■■■■■■■■ ■■
                                                                                                                                                                                                          ■■■ ■■■■■■■■■■■  ■■■■■■■■■■■■■■   ■  ■■■
                                                                                                                                                                                                             ■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■■■■
                                                                                                                                                                                                        ■  ■ ■■■■■■■■■■■■■■  ■■■■■■■■■■■■■ ■
                                                                                                                                                                                                           ■■■■■■■■■■■■■■■    ■■■■■ ■■■■■■■■■■
                                                                                                                                                                                                            ■■■■■■■■■■■ ■    ■■■     ■■■■■■■■■■
                                                                                                                                                                                                                 ■■■■            ■ ■■■■■■■■■■■■ ■
                                                                                                                                                                                                                                         ■■■■   ■ ■
                                                                                                                                                                                                                                     ■■■■■■■■■■
                                                                                                                                                                                                                      ■  ■             ■■■■■■■■■
                                                                                                                                                                                                         ■    ■ ■ ■■■■■■■■■■■■         ■■ ■■■■■■
                                                                                                                                                                                                  ■■■   ■■  ■■■■■■■■        ■■■        ■■■■■■■  ■■
                                                                                                                                                                                      ■  ■       ■■ ■ ■■■■■■■■■ ■            ■■■      ■■ ■■■■■
                                                                                                                                                                                      ■■■■       ■■  ■■■■■■■ ■■        ■■■■■■■■■       ■■■■■
                                                                                                                                                                                        ■     ■■■■■■■■■■■■■■          ■■■ ■■■■■       ■■■■■■ ■
                                                                                                                                                                                     ■■  ■■  ■■■■■■■■■■   ■           ■■■           ■■■■■                                   ■■ ■                            ■■
                                                                                                                                                                                    ■■■      ■■■■■■■■■■■ ■■■           ■■■■■     ■■■■■■■■                         ■         ■■■■                      ■ ■ ■  ■■
                                                                                                                                                       ■        ■                 ■ ■■■  ■■■  ■■■■■■■■■■■■              ■■■■■■■■■■■■■■  ■                          ■   ■■■ ■■■■ ■■                  ■■■■■■ ■ ■
                                                                                                                                                     ■■  ■  ■■■                 ■■■■■■■■■■■■■■■■■■■■■■■■■               ■  ■ ■■■ ■■■                      ■  ■■■■    ■■■■■■  ■ ■     ■    ■         ■■■■■■  ■■
                                                                                                                                                      ■   ■■■■■■■■■■             ■■■■■■■■■■■■■■■■■     ■■ ■                     ■                    ■■  ■   ■■■■■   ■■■■■       ■■■■■■    ■■         ■■■ ■■■■■■
                                                                                                                                                     ■■■ ■■■■■■■■■ ■■■■       ■ ■■■■■■■■■■■■■■■■■■■    ■■■■                                          ■  ■■■■■■■■■■■■■■■■         ■■■■■■■■■■■■■■■■■■        ■■  ■
                                                                                                                                                   ■■  ■■■■■■■      ■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■                                          ■  ■■■■■■■■■■■■■■■■■■■■■■■■■   ■■■■■■■■■ ■■■■       ■■    ■■■■
                                                                                                                                                       ■■■■■         ■■■      ■■■■■■■■■■■■■■■■■■■■■■■■■■                                        ■■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■      ■■■ ■   ■■■
                                                                                                                                                              ■■  ■■  ■■      ■ ■■■■■■■■■■■■■■■■■■■■■■■■■■                                    ■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■        ■■      ■■■■
                                                                                                                                                             ■■  ■   ■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■                             ■■■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■ ■       ■■■■■■■■■ ■
                                                                                                                                                            ■■     ■          ■■ ■■■■■■■■■■■■■■■■■■■■ ■  ■■■  ■■                        ■■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■           ■ ■
                                                                                                                                                         ■ ■■■                  ■■■■■■■■■■■■■■■■■■■■      ■■■ ■■                        ■■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
                                                                                                                                                         ■■■■■                ■   ■■■■■■■■■■■■■■■         ■■■■              ■        ■ ■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                                                                                      ■ ■
                                                                                                                                                     ■ ■ ■■■■■■■■             ■■■     ■■■■■■■■■■■                 ■  ■     ■■■■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■      ■    ■ ■ ■■ ■■ ■     ■
                                                                                                                                                      ■  ■ ■■■   ■           ■■        ■■■■■■■■■■■          ■■■■■■■■   ■ ■■■■■■ ■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■  ■■■   ■■■■■■■■■■■■■■■■■  ■                                                                                                      ■ ■■■
                                                                                                                                                      ■■■■■■■■■■■■■■■■■■  ■■■■■■  ■■■ ■■■■■■■■■■■■■■■■■  ■■■■■■■■■■■■■■■■■   ■■■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■      ■■■■■■■■■■■■■■■■■■■■■■                                                                                                            ■■■■
                                                                                                                                                     ■■■■■■■■■■■■■■      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     ■■■■■■■■■■■■■■■■■■■■■■■■■ ■■■■■■■    ■■■                                                              ■                               ■■
                                                                                                                                                     ■■■■■■■■■■■■■■     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■               ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■ ■■■   ■■■                                                          ■■■■■■■             ■
                                                                                                                                            ■■ ■■■     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■           ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■     ■■■  ■■ ■                                                         ■■    ■■■       ■■   ■■■■■■■■■■■■■■
                                                                                                                                            ■■■    ■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■         ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■      ■■  ■■                                                              ■    ■    ■■■■■■■ ■■■■■■■■
                                                                                                                                           ■■■  ■ ■ ■          ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■        ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                     ■■■■  ■■■    ■■■■■■■■■■■■■■■      ■■■■
                                                                                                                                           ■   ■           ■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■      ■                                                                 ■■■          ■■■■■■■■■■  ■■■      ■■
                                                                                                                                               ■■         ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                             ■        ■■■■■          ■■■■■■■■■■■■■           ■■
          ■■■  ■■                                                                                                                              ■■■■■   ■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                             ■■  ■  ■■■■■■■■■■■   ■■ ■■■■■■■■■■■■■
            ■■■ ■                                                                                                                             ■  ■■■■■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                            ■■  ■■■■■■■   ■■■■■   ■■■■■■ ■   ■ ■■
         ■■■       ■■■■■■ ■                                                                                                                    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■             ■■■■■■■■■■■■■■■■■    ■■■■■■■■■■■■■■■■■■■■■■ ■  ■■                                    ■■     ■■ ■■■■■■■■■■■■■■■■■■■■■     ■
         ■■■■■  ■■■■■■■                                                                                                                         ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■             ■■■■■■■■■■■■■■       ■■■■■■■■■■■■■■■■■■■    ■■ ■■                                   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■■■■
      ■ ■ ■■■■■■■■■■■■■■■■■■                                        ■                                                                             ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■      ■      ■■■■■■■■■■■■          ■■■■■■■■■■■■■■■■■ ■                                            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■ ■■
        ■■■■■■■■■  ■■  ■■■■■ ■■                                ■ ■■                                                                      ■■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■■■■■  ■■■■■■■■■■■■   ■■■■   ■■■■■■■■■■■■■■■■■■■■■■■■                                   ■ ■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     ■■■
       ■ ■■■■■■■■         ■■■■                                 ■■■■■■■■                                                                   ■■   ■     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■        ■■■   ■■■■■■■■■■■■■■■■■■■    ■■■■■■■■■■■■■ ■■  ■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                               ■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■
                          ■■■ ■                   ■                 ■■■■                                                                      ■■ ■  ■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                    ■■■■■■■■■■■■■■■     ■■■                    ■■    ■ ■■ ■■■■■      ■■■■■■■■■■■■■■■                               ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■■
                   ■      ■■■■                  ■■ ■  ■■  ■■■■■■■■■   ■■                                                                      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                    ■■■■■■■■■■■■■■■       ■■■■■■■■               ■■■■           ■■■  ■■■■■■■■■■■■■■■    ■■                           ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
              ■■■    ■■    ■■                ■■■     ■■■■■■■    ■■■  ■■                                                                       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                     ■■■■■■■  ■■            ■  ■■               ■             ■■■■■■■■■■■■■■■■■■■ ■  ■■■                        ■ ■■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
          ■ ■■■   ■■■■■   ■■■          ■ ■■ ■■■■■  ■■■■■■■■     ■■■■■                        ■                                                 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                  ■■■■■■■  ■              ■                                ■■■    ■■■■■■■■■■■■■■■■■                            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
       ■ ■■■■■    ■■    ■■■ ■         ■■■■■■■■■■■■■■■■■■■ ■                  ■    ■       ■■                                                    ■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                   ■■■■                                                                   ■■■■■■■■■■■ ■                          ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
 ■■ ■ ■■■■■ ■      ■■■■■ ■■         ■■■■■■■■■■■■■■■■■■■■               ■■■■■■■■■   ■■      ■■■ ■                                             ■■ ■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■              ■■■■■■■■■■■                                                                ■■■■■■■■■■■■■■■■■■ ■■                     ■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  ■   ■■■■■■                       ■ ■■■■■■■■■■■■■■■■■■■■■■■        ■■■■■■■■■■■■■■■■■■■      ■                                               ■  ■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■           ■■■■■■■■■                                                                  ■   ■■■■■■■■■■■  ■ ■                     ■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  ■■■■■■■   ■■                   ■ ■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■ ■■■■■■■■■■■■■■■■■■ ■    ■  ■■                                                   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■        ■■■■■■    ■■■■■                                                                     ■■■■■■■■                           ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
 ■■■■■■■■■■                      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■■■■■■■■■■■■■■■■■■■                                                              ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■■    ■■■■ ■                                                                 ■■■■■■■■■■■■■■                            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■                  ■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■  ■   ■■■ ■■                                                      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■        ■■■■■■■■  ■                                                                      ■■■■■■■■■■ ■■■                    ■ ■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■ ■■ ■       ■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■■■■■■■■■■■■■■■■■  ■■■                                     ■■ ■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■  ■■   ■■                                                                         ■■■■■■   ■                       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  ■■■■■■■■■■   ■■    ■     ■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■ ■                                      ■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                                      ■■■■■■■■■■■■                         ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
    ■■■■■■■     ■■■■■■■■  ■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                                         ■■■■■■■■■■                                ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
 ■   ■■■■■■■■■■■■■■■■■■■      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■■ ■ ■                                            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■                                                           ■■ ■■■  ■■■■■                 ■■■■■■■  ■■                       ■ ■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■
■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     ■■■■■■■■■■■■■     ■■■■■■■■■■■■■■■■■■■■■■■■■■                                                   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                      ■■  ■■■■■■■■■■■■■■■■■■              ■■■■■■■■                         ■■ ■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■          ■■■■■■■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■                                   ■ ■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■ ■                                                   ■■  ■■■■■■■■■■■■■■■■■■■■■■            ■■■■■■■■                            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■      ■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■        ■■■■■■■■■   ■■■■■■■■■■■■■    ■■■■■■■■■■■■■■    ■                                  ■ ■■ ■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                  ■   ■■■■■■■■■■■■■  ■  ■■■■■■■■■■■        ■■■■■■■■■ ■                           ■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■             ■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■■■■■■■■■■ ■■■   ■■■■■■■■ ■■■   ■ ■■■■■■■■■■■■■■ ■■                                            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                ■■■■ ■■■■■■■■■■         ■■■■■■■■■■          ■■■■■■■ ■■                            ■■■■■■■■■ ■■■■■■■■■■■■■■■■■■■          ■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■■■■■■■■■■■■■■■■■  ■■■■■■            ■■■■        ■■■■■■■■■■■■■■■■■■■■■■■■■■                                         ■■■■■■■ ■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■■■■■■■■■                                                  ■■ ■■■■■■■■ ■         ■■■■■■■■■■■         ■■■■■■■■                                          ■  ■■■■■■■■■■■■■■■■          ■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     ■■  ■■■■■■■■■■■     ■■■■■■                       ■■■   ■■■■   ■■■■■■■■■■■■■■■■ ■■                                                ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■             ■■■■■                                                 ■■■■■■■■■■          ■■■■■■■■■■■■■         ■■■■■■■■                              ■■■■■           ■■■■■■■■■■■■■■■■  ■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■            ■■■■■■■ ■        ■■■                                  ■■■■■■■■■■■■■■■■  ■ ■■                                      ■■■■      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■               ■■    ■■                                           ■■■■■■■■ ■         ■■■■■■■■■■■■■         ■■■■■■■ ■                           ■■■   ■■■■           ■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■              ■■■■ ■■                                             ■■ ■■■■■■■■■■■■■■                                         ■  ■■■     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     ■■■ ■               ■■■  ■■■■■                                     ■■■  ■■■■■■■■          ■■■■■■■■■■■          ■■■■■■■■■                            ■■■■ ■■■             ■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■            ■■■■                                                         ■■■■■■■■■■■                                      ■■  ■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                    ■■■■■■■   ■■   ■                                     ■■■■■■■■■■■■         ■■■■■■■■■         ■■■■■■■■ ■■                                  ■■        ■■■■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■        ■■■■■■■                                                         ■■■■■■■■■■■■■■■■■                                         ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                   ■■■■■■    ■■■■                                        ■■■■■■■■■             ■■■■■■■■■■    ■■■■■■■■■■■  ■                                 ■ ■■■■■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     ■■■■  ■■■                                                          ■■■■■■■■■■■    ■                                          ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                    ■■■■■■■■■■■                                           ■■■■■■■■■            ■■■■■■■■■■■■■■■■■■■■■ ■                                       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■■■■■ ■                                                             ■■■■■■■■                                            ■■■■■■■■■■■■■■■■■■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                          ■                                          ■    ■■■■■■■■■■■            ■■■■■■■■■■■■■■■■■ ■■                                     ■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■         ■                                                           ■■■■■■■■■■■■■                                            ■■■■■■■■■■■■■■■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                    ■ ■■■■■■■■■                  ■    ■■■  ■                                            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                       ■■■■■■■■■■■                                                ■■ ■■         ■■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■■         ■■ ■■                                                ■■■■■■■■■■■                                                                       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                            ■  ■■■    ■                  ■■■■■■■■■ ■                                                                    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■■■■■■■■■■■■■■■                                            ■■■■■■■■■■■■■                                                           ■         ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■                                     ■■ ■■■■■■■■■■■■■■■■■            ■■■■■■■■■■■    ■                                          ■                          ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■■■■■■■■■■■■■■■■■■                                            ■■■■■■■■                                                             ■ ■■■■■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                       ■ ■■■■■■■■■■■■■■■■■■■■■            ■■■■■■■■■                                           ■■■■■■■■■■■                    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                          ■    ■■■■■■■■■■■                                                          ■■■  ■■■■     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■■■■■ ■                                 ■  ■■■■■■■■■■■    ■■■■■■■■■■             ■■■■■■■■■                                        ■■■■    ■■■■■■                   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                         ■■■■■■■■■■■■■■■■■                                                         ■■■■■■■        ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
 ■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■■■■        ■■                                  ■■ ■■■■■■■■         ■■■■■■■■■         ■■■■■■■■■■■■                                     ■   ■■   ■■■■■■■                    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■  ■■                                      ■■■■■■■■■■■                                                         ■■■■            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■             ■■■ ■■■■                            ■■■■■■■■■          ■■■■■■■■■■■          ■■■■■■■■  ■■■                                     ■■■■■  ■■■               ■ ■■■     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     ■■■  ■                                         ■■■■■■■■■■■■■■ ■■                                             ■■ ■■■■              ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
 ■■■■■■■■■■■■■■■■■■■■■■■■■■           ■■■■   ■■■                           ■ ■■■■■■■         ■■■■■■■■■■■■■         ■ ■■■■■■■■                                           ■■    ■■               ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■      ■■■■                                      ■■ ■  ■■■■■■■■■■■■■■■■                                  ■■■        ■ ■■■■■■■            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
          ■  ■■■■■■■■■■■■■■■■           ■■■■■                              ■■■■■■■■         ■■■■■■■■■■■■■          ■■■■■■■■■■                                                 ■■■■■             ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                ■■ ■■■■■■■■■■■■■■■■   ■■■■   ■■■                       ■■■■■■     ■■■■■■■■■■■  ■■     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
■■■■          ■■■■■■■■■■■■■■■■  ■                                          ■■■■■■■■         ■■■■■■■■■■■         ■ ■■■■■■■■ ■■                                                  ■■■■■■■■■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■ ■■■■■■■                                         ■■■■■■■■■■■■■■■■■■■■■■■■■■        ■■■■            ■■■■■■  ■■■■■■■■■■■■■■■■■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  ■■■          ■■■■■■■■■■■■■■■■■■■ ■■■■■■■■■                            ■■ ■■■■■■■          ■■■■■■■■■■         ■■■■■■■■■■ ■■■■                                                ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                            ■■ ■■■■■■■■■■■■■■ ■   ■■■ ■■■■■■■■   ■■■ ■■■■■■■■■■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  ■             ■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■                           ■ ■■■■■■■■■        ■■■■■■■■■■■  ■  ■■■■■■■■■■■■■   ■                                                  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■ ■■ ■                                  ■    ■■■■■■■■■■■■■■    ■■■■■■■■■■■■■   ■■■■■■■■■        ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
  ■■      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                            ■■■■■■■■            ■■■■■■■■■■■■■■■■■■■■■■  ■■                                                   ■ ■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■ ■                                   ■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■■■■■■■          ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■
 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■ ■■                         ■■■■■■■■              ■■■■■■■■■■■■■■■■■■  ■■                                                      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                   ■■■■■■■■■■■■■■■■■■■■■■■■■■     ■■■■■■■■■■■■■     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■
■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■ ■                       ■■  ■■■■■■■                 ■■■■■  ■■■ ■■                                                           ■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                            ■ ■ ■■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■      ■■■■■■■■■■■■■■■■■■■   ■
 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                ■■■■■■■■■■                                                                                         ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■  ■■■■■■■■     ■■■■■■■
 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                         ■■■■■■■■■■■■                                                                                      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■                                      ■ ■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■     ■    ■■   ■■■■■■■■■■
 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                       ■   ■■■■■■                                                                         ■■   ■■  ■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■ ■■                                     ■■■  ■■■■■■■■■■■■■■■■■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■       ■ ■■ ■■■■■■■■■■■■■
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■ ■                    ■■■ ■■■■■■■■■■                                                                      ■  ■■■■■■■■        ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                      ■■ ■■■   ■  ■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■                  ■■■■■■■■■■■■■
     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                            ■■■■■■■■■■■■■■                                                                 ■ ■■■■    ■■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                              ■■■■■■■■■■■■■■■■■■■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■                      ■■■■■■■■■■
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                           ■■■■■■■■                                                                     ■■■■■    ■■■■■■        ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                   ■■  ■    ■ ■■■■■■■■■■■■■■■■■■ ■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■ ■                   ■■   ■■■■■■■
    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■                     ■ ■  ■■■■■■■■■■■   ■                                                                  ■■■■■■■■■           ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■  ■                                               ■      ■■■■■■■■■■■■■■■■■■■        ■■■■■■■■■■■■■■■■■■■■■■■ ■                       ■■■■■■   ■
  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■                     ■■ ■■■■■■■■■■■■■■■■■■                                                                ■■■■■■■■■■■              ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■ ■■                                             ■ ■■■      ■■   ■■■■■■■■■               ■■■■■■■■■■■■■■■■■■■■         ■■ ■■■■■      ■ ■■■■■ ■ ■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                          ■ ■■■■■■■■■■■                                                                   ■■■■                   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■                                                    ■■       ■    ■                  ■ ■■■■■■■■■■■■■■■■■■■         ■ ■■■    ■■    ■■■■■ ■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                            ■■■■■■■■■■■■■■■■■    ■■■                                ■              ■  ■■■■■■■                  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                 ■                        ■■■■■     ■■■■■■■■  ■■■■■ ■■ ■          ■■■   ■■■■■   ■■■ ■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■■ ■                        ■■■  ■ ■■■■■■■■■■■■■■■■■■■             ■               ■■  ■            ■■  ■■■■■■■                     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                       ■■  ■■■    ■■■■■■■     ■■■                ■■    ■■    ■■■
■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                           ■■    ■■■■■■■■■■■■■■■  ■■■           ■■■■               ■■■■■■■■       ■■■■■■■■■■■■■■■                    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                      ■■   ■■■■■■■■■  ■■  ■ ■■                  ■■■■      ■
■■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                               ■■■■■■■■■■■■■■■      ■■■■■ ■■ ■    ■■                    ■■■     ■■■■■■■■■■■■■■■                    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■  ■ ■■                                                                      ■■■■                 ■                   ■ ■■■
■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■                               ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■  ■■ ■■■■■■■■■■■■■    ■■■■■■■■■■■■■■■■■■■   ■■■        ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     ■   ■■                                                                   ■■■■■■■■                                 ■■■■         ■■■■■■■■ ■
■■■     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■ ■                                   ■■■■■■■■■■■■■■■■■■■■■■■■   ■■■■   ■■■■■■■■■■■■  ■■■■■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■■                                                                      ■■ ■                                ■■ ■■■■■  ■■  ■■■■■■■■■
■■ ■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                            ■ ■■■■■■■■■■■■■■■■■          ■■■■■■■■■■■■      ■      ■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                             ■                                        ■■■■■■■■■■■■■■■■■■ ■ ■
          ■■■■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                   ■■ ■■    ■■■■■■■■■■■■■■■■■■■       ■■■■■■■■■■■■■■             ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                                                                         ■■■■■■■  ■■■■■
            ■     ■■■■■■■■■■■■■■■■■■■■■ ■■     ■■                                    ■■  ■ ■■■■■■■■■■■■■■■■■■■■■■    ■■■■■■■■■■■■■■■■■             ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■                                                                                                                    ■ ■■■■■■       ■■■
         ■■ ■   ■ ■■■■■■   ■■■■■   ■■■■■■■  ■■                                            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■■■■■  ■                                                                                                                             ■ ■■■
            ■■■■■■■■■■■■■ ■■   ■■■■■■■■■■■  ■  ■■                                             ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■   ■■■■■                                                                                                                              ■■  ■■■
■■           ■■■■■■■■■■■■■          ■■■■■        ■                                             ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■         ■■
  ■■      ■■■  ■■■■■■■■■■          ■■■                                                                 ■      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■           ■   ■
■■■■      ■■■■■■■■■■■■■■■    ■■■  ■■■■                                                                     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■        ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■          ■ ■ ■  ■■■
        ■■■■■■■■ ■■■■■■■    ■    ■                                                              ■■  ■■      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■         ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■    ■■■
 ■■■■■■■■■■■■■■   ■■       ■■■    ■■                                                         ■ ■■  ■■■     ■  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■           ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     ■■■ ■■
               ■             ■■■■■■■                                                          ■■■   ■■■ ■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■               ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     ■■■■■■■■■■■■■■
■■                               ■                                                              ■■■    ■■■■■■■ ■■■■■■■■■■■■■■■■■■■■■■■■■     ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■            ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■      ■■■■■■■■■■■■■■
■■■■                                                                                                            ■■■■■■■■■■■■■■■■■■■■■■      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■■■   ■■■■■■■■■■■■■■■■■  ■■■■■■■■■■■■■■■■■ ■■■  ■■■■■■  ■■■■■■■■■■■■■■■■■■
■■■ ■                                                                                                      ■  ■■■■■■■■■■■■■■■■■   ■■■  ■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■ ■■■■■■ ■   ■■■■■■■■          ■■■■■■■■■■■        ■■           ■   ■■■ ■  ■
                                                                                                           ■     ■ ■■ ■■ ■ ■    ■      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■■■■     ■  ■                 ■■■■■■■■■■■     ■■■             ■■■■■■■■ ■ ■
 ■ ■                                                                                                                                      ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    ■■ ■        ■              ■■■■         ■■■■■■■■■■■■■■■   ■                ■■■■■
                                                                                                                                          ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   ■■■                        ■■ ■■■      ■■■■■■■■■■■■■■■■■■■■                  ■■■ ■
                                                                                                                            ■ ■           ■■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■■                        ■■  ■■■  ■ ■■■■■■■■■■■■■■■■■■■■ ■■          ■     ■■
                                                                                                                       ■ ■■■■■■■■■       ■ ■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■■■                             ■    ■■■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■   ■  ■■
                                                                                                                      ■■■■      ■■        ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ ■■                                    ■■■■■■■■■■■■■■■■■■■■■■■■■■ ■      ■■  ■■  ■■
                                                                                                                      ■■■   ■ ■■■      ■■ ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  ■■                                        ■■■■■■■■■■■■■■■■■■■■■■■■■■      ■■■         ■■■■■
                                                                                                                    ■■■■    ■■       ■■■■ ■■■■■■■■■   ■■■■■■■■■■■■■■■■■■■■■■■■■  ■                                          ■■■■■■■■■■■■■■■■■■■■■■■■■       ■■■      ■■■■■■■  ■■
                                                                                                                   ■  ■■        ■■■■■■■■■■■■■■■■■■         ■■■■■■■■■■■■■■■■  ■                                          ■■■■    ■■■■■■■■■■■■■■■■■■■ ■       ■■■■ ■■■■■■■■■ ■■■
                                                                                                                   ■■■■■■ ■■■         ■■    ■■■■■■       ■■■■■   ■■■■■   ■  ■■                    ■                     ■ ■■     ■■■■■■■■■■■■■■■■■             ■■■■■■■■■■   ■
                                                                                                                     ■■  ■■■■■■         ■    ■     ■ ■  ■■■■■■    ■■■■  ■                      ■■■ ■■■ ■  ■               ■■■■■■■■■■■■■■■■■■■■■■■■■                 ■■■  ■  ■■
                                                                                                                     ■ ■ ■■■■■■                  ■■ ■■■■ ■■■   ■                          ■  ■■■■■■■■■■■■■■              ■■■■■■■■■■■■  ■■■  ■■■ ■                 ■        ■
                                                                                                                    ■■  ■ ■ ■                      ■■■■         ■                         ■■■■■■■■     ■■■■■           ■■■ ■■■■■■■■■■■      ■■■
                                                                                                                     ■■                            ■ ■■                                   ■■■■■           ■■■           ■   ■■■■■■■■■■  ■■  ■■
                                                                                                                                                                                     ■ ■■■■■■       ■■■■■ ■■■          ■■■■■■■■■■■■■■     ■
                                                                                                                                                                                       ■■■■■       ■■■■■■■■■        ■■ ■■■■■■■  ■■       ■■■■
                                                                                                                                                                                     ■■■■■ ■■      ■■■            ■ ■■■■■■■■■ ■ ■■       ■  ■
                                                                                                                                                                                 ■■  ■■■■■■■        ■■■        ■■■■■■■■  ■■   ■■■
                                                                                                                                                                                   ■■■■■■ ■■         ■■■■■■■■■■■■ ■ ■    ■
                                                                                                                                                                                   ■■■■■■■■■             ■  ■
                                                                                                                                                                                    ■■■■■■■■■■
                                                                                                                                                                                ■ ■   ■■■■
                                                                                                                                                                                  ■ ■■■■■■■■■■■■ ■            ■■■■
                                                                                                                                                                                    ■■■■■■■■■■     ■■■    ■ ■■■■■■■■■■■
                                                                                                                                                                                     ■■■■■■■■■■ ■■■■■    ■■■■■■■■■■■■■■■
                                                                                                                                                                                       ■ ■■■■■■■■■■■■■  ■■■■■■■■■■■■■■ ■  ■
                                                                                                                                                                                  ■■■■■   ■■■■■■■■■■■■■■■■■■■■■■■■■■■■
                                                                                                                                                                                 ■■■  ■   ■■■■■■■■■■■■■■  ■■■■■■■■■■■ ■■■
                                                                                                                                                                                   ■■ ■■■■■■■■■■■■■■■■■■■ ■■■■■■■■      ■
                                                                                                                                                                                         ■   ■■■■■■■        ■■ ■■■■    ■■■
                                                                                                                                                                                          ■   ■■              ■■■■■■   ■■■■
                                                                                                                                                                                                      ■■■     ■■■■■
                                                                                                                                                                                                   ■■■  ■■   ■■■■
                                                                                                                                                                                                   ■■   ■■■■■
                                                                                                                                                                                                  ■■■■
                                                                                                                                                                                                     ■■■ ■■■■
                                                                                                                                                                                                   ■ ■■■■■■■
                                                                                                                                                                                                       ■■ ■■
                                                                                                                                                                                                      ■ ■
                                                                                                                                                                                                       ■■

```

Other outputs are shown in this Rosetta Code task's   ''discussion''   page.




## Ring


```ring

# Project : Julia Set

load "guilib.ring"

new qapp
        {
        win1 = new qwidget() {
                  setwindowtitle("Julia set")
                  setgeometry(100,100,500,400)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,400,400)
                              settext("")
                  }
                  new qpushbutton(win1) {
                          setgeometry(150,300,100,30)
                          settext("draw")
                          setclickevent("draw()")
                  }
                  show()
        }
        exec()
        }

func draw
        p1 = new qpicture()
               color = new qcolor() {
               setrgb(0,0,255,255)
        }
        pen = new qpen() {
                 setcolor(color)
                 setwidth(1)
        }
        paint = new qpainter() {
                  begin(p1)
                  setpen(pen)

        creal=-0.8
        cimag=0.156
        for v=-16 to 16
             for h=-64 to 64
                   x=h/40
                   y=v/20
                   for i=1 to 50
                         flag = 1
                         zreal=x*x-y*y+creal
                         zimag=x*y*2+cimag
                         if zreal*zreal>1000 flag = 0 loop ok
                         x=zreal
                         y=zimag
                  next
                  if flag = 1
                     drawpoint(h+100,150-v)
                  ok
             next
        next
        endpaint()
        }
        label1 { setpicture(p1) show() }

```



## Ruby

```ruby
def julia(c_real, c_imag)
  puts Complex(c_real, c_imag)
  -1.0.step(1.0, 0.04) do |v|
    puts -1.4.step(1.4, 0.02).map{|h| judge(c_real, c_imag, h, v)}.join
  end
end

def judge(c_real, c_imag, x, y)
  50.times do
    z_real = (x * x - y * y) + c_real
    z_imag = x * y * 2 + c_imag
    return " "  if z_real**2 > 10000
    x, y = z_real, z_imag
  end
  "#"
end

julia(-0.8, 0.156)
```

```txt
-0.8+0.156i




                                                                        #
                                                                         #
                                                                      ##
                                                                     #######
                                                                      ##  ####
                                                                            ##
                                                                # ####  ##  #
                                                        #      #  ###   ###       # ##     #
                                                         #    ######          ##########  # #
                                                         # #   ######       ########## #
                                                         #      ###  ### # ############  ######                         #
                                                        ################   ###################   #               #   ###  #
                                                         ################ #################### ##              #  ###   ##
                         ##                            ##############################    ###  #### #          #######     ###
                       #  ##        ##               #  #####################  # #### ##    #### ###        ########### ######
                       # #    #####                     #####################     #              # ##       ######################
                     ###    ####### ####  #            #######################    #                ###      ############  ###   ##      ##
                  ## ##  ## #################          ########################                     ###      ###########         ##   ### ###
                  #################  ##  ###### #        # ################### #             #####   ##       ######## #      ### #  ######
                  ############  ## ##       # ###            ##############   #            ##  ###   ##       #   #### #      # ##     ###
        ##        ###########  ##             ####          #############    ## #         ##   ### ##         ## #######      ##      #####
     #########    #############                 ###           #  # ####### #  #           ###                 #############    #########
  #####      ##      ####### ##         ## ###   ##         # ##    #############          ####             ##  ###########        ##
   ###     ## #      # ####   #       ##   ###  ##            #   ##############            ### #       ## ##  ############
  ######  # ###      # ########       ##   #####             # ################### #        # ######  ##  #################
### ###   ##         ###########      ###                     ########################          ################# ##  ## ##
   ##      ##   ###  ############      ###                #    #######################            #  #### #######    ###
           ######################       ## #              #     #####################                     #####    # #
               ###### ###########        ### ####    ## #### #  #####################  #               ##        ##  #
                ###     #######          # ####  ###    ##############################                            ##
                   ##   ###  #              ## #################### ################
                  #  ###   #               #   ###################   ################
                    #                         ######  ############ # ###  ###      #
                                                     # ##########       ######   # #
                                                # #  ##########          ######    #
                                                 #     ## #       ###   ###  #      #
                                                                #  ##  #### #
                                                               ##
                                                               ####  ##
                                                                 #######
                                                                     ##
                                                                   #
                                                                    #

```



## Rust


```rust
extern crate image;

use image::{ImageBuffer, Pixel, Rgb};

fn main() {
    // 4 : 3 ratio is nice
    let width = 8000;
    let height = 6000;

    let mut img = ImageBuffer::new(width as u32, height as u32);

    // constants to tweak for appearance
    let cx = -0.9;
    let cy = 0.27015;
    let iterations = 110;

    for x in 0..width {
        for y in 0..height {
            let inner_height = height as f32;
            let inner_width = width as f32;
            let inner_y = y as f32;
            let inner_x = x as f32;

            let mut zx = 3.0 * (inner_x - 0.5 * inner_width) / (inner_width);
            let mut zy = 2.0 * (inner_y - 0.5 * inner_height) / (inner_height);

            let mut i = iterations;

            while zx * zx + zy * zy < 4.0 && i > 1 {
                let tmp = zx * zx - zy * zy + cx;
                zy = 2.0 * zx * zy + cy;
                zx = tmp;
                i -= 1;
            }

            // guesswork to make the rgb color values look okay
            let r = (i << 3) as u8;
            let g = (i << 5) as u8;
            let b = (i << 4) as u8;
            let pixel = Rgb::from_channels(r, g, b, 0);
            img.put_pixel(x as u32, y as u32, pixel);
        }
    }

    let _ = img.save("output.png");

}
```



## Scala


### Java Swing Interoperability


```Scala
import java.awt._
import java.awt.image.BufferedImage

import javax.swing._

object JuliaSet extends App {
  SwingUtilities.invokeLater(() =>
    new JFrame("Julia Set") {

      class JuliaSet() extends JPanel {
        private val (maxIter, zoom) = (300, 1)

        override def paintComponent(gg: Graphics): Unit = {
          val g = gg.asInstanceOf[Graphics2D]

          def drawJuliaSet(g: Graphics2D): Unit = {
            val (w, h) = (getWidth, getHeight)
            val image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
            val (cX, cY) = (-0.7, 0.27015)
            val moveX, moveY = 0
            var zx, zy = 0.0

            for (x <- 0 until w;
                 y <- 0 until h) {
              zx = 1.5 * (x - w / 2) / (0.5 * zoom * w) + moveX
              zy = (y - h / 2) / (0.5 * zoom * h) + moveY
              var i: Float = maxIter
              while (zx * zx + zy * zy < 4 && i > 0) {
                val tmp = zx * zx - zy * zy + cX
                zy = 2.0 * zx * zy + cY
                zx = tmp
                i -= 1
              }
              val c = Color.HSBtoRGB((maxIter / i) % 1, 1, if (i > 0) 1 else 0)
              image.setRGB(x, y, c)
            }
            g.drawImage(image, 0, 0, null)
          }

          super.paintComponent(gg)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          drawJuliaSet(g)
        }

        setBackground(Color.white)
        setPreferredSize(new Dimension(800, 600))
      }

      add(new JuliaSet, BorderLayout.CENTER)
      pack()
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setLocationRelativeTo(null)
      setResizable(false)
      setVisible(true)
    }
  )

}
```


## Sidef

[[File:JuliaSet_sidef.png|250px|thumb|right]]

```ruby
require('Imager')

var (w, h) = (640, 480)
var img = %s'Imager'.new(xsize => w, ysize => h, channels => 3)

var maxIter = 50
var c = Complex(-0.388, 0.613)

var color = %s'Imager::Color'.new('#000000')

for x,y in (^w ~X ^h) {
    var i = maxIter
    var z = Complex((x - w/2) / w * 3, (y - h/2) / h * 2)
    while (z.abs < 2 && --i) {
        z = (z*z + c)
    }
    color.set(hsv => [i / maxIter * 360, 1, i])
    img.setpixel(x => x, y => y, color => color)
}

img.write(file => "JuliaSet_sidef.png")
```


This version generates an ASCII representation:

```ruby
var (w, h) = (141, 50)

var maxIter = 40
var c = Complex(-0.8, 0.156)

for y in ^h {
    for x in ^w {
        var i = maxIter
        var z = Complex(3 * (x - w/2) / w, 2 * (y - h/2) / h)
        while (z.abs < 2 && --i) {
            z = (z*z + c)
        }
        print (i > 0 ? ' ' : '#')
    }
    print "\n"
}
```

```txt
                                                                      ##
                                                                      ##
                                                                     #######
                                                                      ########
                                                                        #   ##
                                                                 # ###  ##  #
                                                         #      #####   ####       ##     ##
                                                          #    ######         ##########   #
                                                               ########     ##########    #
                                                          ##   ###### ########################                       ###
                                                          ################ ################### ##             ## # ### #
                                                        #  #####################################             #  ###  ##
                          # #                           ############################# ###########          # ######    ###
                          #   #       ##                  ########################### #    ########       ################# #
                          # #    ####                    #####################  ###             ####      #####################
                        ###    ############             ######################  # #               ###     ################ ###     # # #
                        ###  # #################         ######################                    ##      ########### #      ##  ######
                      ###########################           ####################           ######  ##       ##########     ### #   ###   # ##
                     ################ ##      ######          #################           ######## ##       #  ######     ## ##   ####     ##
         #  #         ########### #             ####         #############   ####        ###  #######       ###########    ##    ######
        ##########    ############                ##           #  ##########  #           ##                ############    ##########
       ######    ##    ###########       #######  ###        ####   #############         ####             # ###########         #  #
###     ####   ## ##     ######  #       ## ########           #################          ######      ## ################
 ## #   ###   # ###     ##########       ##  ######           ####################           ###########################
      ######  ##      # ###########      ##                    ######################         ################# #  ###
      # # #     ### ################     ###               # #  ######################             ############    ###
               #####################      ####             ###  #####################                    ####    # #
                 # #################       ########    # ###########################                  ##       #   #
                    ###    ###### #          ########### #############################                           # #
                       ##  ###  #             #####################################  #
                      # ### # ##             ## ################### ################
                      ###                       ######################## ######   ##
                                                   #    ##########     ########
                                                  #   ##########         ######    #
                                                  ##     ##       ####   #####      #
                                                                 #  ##  ### #
                                                                ##   #
                                                                ########
                                                                  #######
                                                                      ##
                                                                    ##
```



## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
[[File:JuliaSet.zkl.jpg|250px|thumb|right]]

```zkl
fcn juliaSet{
   w,h,zoom:=800,600, 1;
   bitmap:=PPM(w,h,0xFF|FF|FF);  // White background

   cX,cY:=-0.7, 0.27015;
   moveX,moveY:=0.0, 0.0;
   maxIter:=255;

   foreach x,y in (w,h){
      zx:=1.5*(x - w/2)/(0.5*zoom*w) + moveX;
      zy:=1.0*(y - h/2)/(0.5*zoom*h) + moveY;
      i:=maxIter;
      while(zx*zx + zy*zy < 4 and i > 1){
	 tmp:=zx*zx - zy*zy + cX;
	 zy,zx=2.0*zx*zy + cY, tmp;
	 i-=1;
      }
      // convert byte to RGB (3 bytes), kinda magic to get nice colors
      bitmap[x,y]=i.shiftLeft(21) + i.shiftLeft(10) + i*8;
   }

   bitmap.writeJPGFile("juliaSet.jpg",True);
}();
```

