+++
title = "Image noise"
description = ""
date = 2019-08-25T21:15:24Z
aliases = []
[extra]
id = 8370
[taxonomies]
categories = ["task", "Graphics"]
tags = []
languages = [
  "ada",
  "axe",
  "bbc_basic",
  "c",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "euler_math_toolbox",
  "factor",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "liberty_basic",
  "maple",
  "maxscript",
  "nim",
  "ocaml",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "processing",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ruby",
  "run_basic",
  "scala",
  "tcl",
  "visual_basic_dotnet",
  "xpl0",
]
+++

## Task

Generate a random black and white   '''320'''<small>x</small>'''240'''   image continuously,
showing FPS (frames per second).


;A sample image:  [[Image:NoiseOutput.png|600px||center|sample]]





## Ada

noise.ads:

```Ada
with Lumen.Image;

package Noise is

   function Create_Image (Width, Height : Natural) return Lumen.Image.Descriptor;

end Noise;
```


noise.adb:

```Ada
with Ada.Numerics.Discrete_Random;

package body Noise is
   type Color is (Black, White);
   package Color_Random is new Ada.Numerics.Discrete_Random (Color);
   Color_Gen : Color_Random.Generator;

   function Create_Image (Width, Height : Natural) return Lumen.Image.Descriptor is
      Result : Lumen.Image.Descriptor;
   begin
      Color_Random.Reset (Color_Gen);
      Result.Width := Width;
      Result.Height := Height;
      Result.Complete := True;
      Result.Values := new Lumen.Image.Pixel_Matrix (1 .. Width, 1 .. Height);
      for X in 1 .. Width loop
         for Y in 1 .. Height loop
            if Color_Random.Random (Color_Gen) = Black then
               Result.Values (X, Y) := (R => 0, G => 0, B => 0, A => 0);
            else
               Result.Values (X, Y) := (R => 255, G => 255, B => 255, A => 0);
            end if;
         end loop;
      end loop;
      return Result;
   end Create_Image;

end Noise;
```


test_noise.adb:

```Ada
with Ada.Calendar;
with Ada.Text_IO;
with System.Address_To_Access_Conversions;
with Lumen.Window;
with Lumen.Image;
with Lumen.Events.Animate;
with GL;
with Noise;

procedure Test_Noise is
   package Float_IO is new Ada.Text_IO.Float_IO (Float);

   Program_End : exception;

   Win : Lumen.Window.Handle;
   Image : Lumen.Image.Descriptor;
   Tx_Name : aliased GL.GLuint;
   Wide : Natural := 320;
   High : Natural := 240;
   First_Frame : Ada.Calendar.Time;
   Frame_Count : Natural := 0;

   -- Create a texture and bind a 2D image to it
   procedure Create_Texture is
      use GL;

      package GLB is new System.Address_To_Access_Conversions (GLubyte);

      IP : GLpointer;
   begin  -- Create_Texture
      -- Allocate a texture name
      glGenTextures (1, Tx_Name'Unchecked_Access);

      -- Bind texture operations to the newly-created texture name
      glBindTexture (GL_TEXTURE_2D, Tx_Name);

      -- Select modulate to mix texture with color for shading
      glTexEnvi (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      -- Wrap textures at both edges
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

      -- How the texture behaves when minified and magnified
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

      -- Create a pointer to the image.  This sort of horror show is going to
      -- be disappearing once Lumen includes its own OpenGL bindings.
      IP := GLB.To_Pointer (Image.Values.all'Address).all'Unchecked_Access;

      -- Build our texture from the image we loaded earlier
      glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA, GLsizei (Image.Width), GLsizei (Image.Height), 0,
                    GL_RGBA, GL_UNSIGNED_BYTE, IP);
   end Create_Texture;

   -- Set or reset the window view parameters
   procedure Set_View (W, H : in Natural) is
      use GL;
   begin  -- Set_View
      GL.glEnable (GL.GL_TEXTURE_2D);
      glClearColor (0.8, 0.8, 0.8, 1.0);

      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;
      glViewport (0, 0, GLsizei (W), GLsizei (H));
      glOrtho (0.0, GLdouble (W), GLdouble (H), 0.0, -1.0, 1.0);

      glMatrixMode (GL_MODELVIEW);
      glLoadIdentity;
   end Set_View;

   -- Draw our scene
   procedure Draw is
      use GL;
   begin  -- Draw
      -- clear the screen
      glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      GL.glBindTexture (GL.GL_TEXTURE_2D, Tx_Name);

      -- fill with a single textured quad
      glBegin (GL_QUADS);
      begin
         glTexCoord2f (1.0, 0.0);
         glVertex2i (GLint (Wide), 0);

         glTexCoord2f (0.0, 0.0);
         glVertex2i (0, 0);

         glTexCoord2f (0.0, 1.0);
         glVertex2i (0, GLint (High));

         glTexCoord2f (1.0, 1.0);
         glVertex2i (GLint (Wide), GLint (High));
      end;
      glEnd;

      -- flush rendering pipeline
      glFlush;

      -- Now show it
      Lumen.Window.Swap (Win);
   end Draw;

   -- Simple event handler routine for keypresses and close-window events
   procedure Quit_Handler (Event : in Lumen.Events.Event_Data) is
   begin  -- Quit_Handler
      raise Program_End;
   end Quit_Handler;

   -- Simple event handler routine for Exposed events
   procedure Expose_Handler (Event : in Lumen.Events.Event_Data) is
      pragma Unreferenced (Event);
   begin  -- Expose_Handler
      Draw;
   end Expose_Handler;

   -- Simple event handler routine for Resized events
   procedure Resize_Handler (Event : in Lumen.Events.Event_Data) is
   begin  -- Resize_Handler
      Wide := Event.Resize_Data.Width;
      High := Event.Resize_Data.Height;
      Set_View (Wide, High);
      Draw;
   end Resize_Handler;

   procedure Next_Frame (Frame_Delta : in Duration) is
      pragma Unreferenced (Frame_Delta);
      use type Ada.Calendar.Time;
   begin
      Frame_Count := Frame_Count + 1;
      if Ada.Calendar.Clock >= First_Frame + 1.0 then
         Ada.Text_IO.Put ("FPS: ");
         Float_IO.Put (Float (Frame_Count), 5, 1, 0);
         Ada.Text_IO.New_Line;
         First_Frame := Ada.Calendar.Clock;
         Frame_Count := 0;
      end if;
      Image := Noise.Create_Image (Width => Wide, Height => High);
      Create_Texture;
      Draw;
   end Next_Frame;
begin
   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win           => Win,
                        Name          => "Noise fractal",
                        Width         => Wide,
                        Height        => High,
                        Events        => (Lumen.Window.Want_Exposure  => True,
                                          Lumen.Window.Want_Key_Press => True,
                                          others                      => False));

   -- Set up the viewport and scene parameters
   Set_View (Wide, High);

   -- Now create the texture and set up to use it
   Image := Noise.Create_Image (Width => Wide, Height => High);
   Create_Texture;

   First_Frame := Ada.Calendar.Clock;

   -- Enter the event loop
   declare
      use Lumen.Events;
   begin
      Animate.Select_Events (Win   => Win,
                             Calls => (Key_Press    => Quit_Handler'Unrestricted_Access,
                                       Exposed      => Expose_Handler'Unrestricted_Access,
                                       Resized      => Resize_Handler'Unrestricted_Access,
                                       Close_Window => Quit_Handler'Unrestricted_Access,
                                       others       => No_Callback),
                             FPS   => Animate.Flat_Out,
                             Frame => Next_Frame'Unrestricted_Access);
   end;
exception
   when Program_End =>
      null;
end Test_Noise;
```



## Axe

Note that since the calculator's screen is 96x64, this example uses those dimensions instead of 320x240.

Instead of naively drawing each pixel to the screen, this implementation directly writes random numbers to the buffer, which randomly sets each pixel. Because Axe does not have native clock support, the FPS counter is simulated by counting "ticks" at the known interrupt rate of 107.79 Hz (rounded to 108).

It is possible to notice some vertical line patterns in the noise. This is likely due to the pseudorandom number generator and not the drawing method.

This example gets steady 48 FPS on a TI-84 Plus Silver Edition running at 15 MHz. It gets 26 FPS when running at 6 MHz. Some pixels appear gray because the duration of each frame is shorter than the screen's notoriously slow response time. If a pixel is toggled very quickly, it never has time to fully transition to white or black.


```axe
.Enable 15 MHz full speed mode
Full
.Disable memory access delays (adds 1 FPS)
Fullʳ

.Flush key presses
While getKey(0)
End

.Setup
0→F
0→N
Fix 5
fnInt(FPS,6)

Repeat getKey(0)
 NOISE()
 F++
 .Reset the FPS counter before it overflows
 F>606?0→F→N
 Text(0,0,F*108/N▶Dec)
 DispGraph
End

.Clean up
LnRegʳ
Fix 4
Return

.Draws random noise to the buffer
Lbl NOISE
ClrDraw
For(I,0,5)
 For(J,0,63)
  rand→{J*12+(I*2)+L₆}ʳ
 End
End
Return

.Increments the tick counter
Lbl FPS
N++
Return
```



## BBC BASIC

```bbcbasic
      dx% = 320
      dy% = 240
      images% = 100000
      VDU 23,22,dx%;dy%;8,8,16,0

      REM Create a block of random data in memory:
      DIM random% dx%*dy%+images%
      FOR R% = random% TO random%+dx%*dy%+images%
        ?R% = RND(256)-1
      NEXT

      REM Create a BMP file structure:
      DIM bmpfile{bfType{l&,h&}, bfSize%, bfReserved%, bfOffBits%, \
      \   biSize%, biWidth%, biHeight%, biPlanes{l&,h&}, biBitCount{l&,h&}, \
      \   biCompression%, biSizeImage%, biXPelsPerMeter%, biYPelsPerMeter%, \
      \   biClrUsed%, biClrImportant%, biPalette%(255)}
      bmpfile.biSize% = 40
      bmpfile.biWidth% = dx%
      bmpfile.biHeight% = dy%
      bmpfile.biPlanes.l& = 1
      bmpfile.biBitCount.l& = 8
      FOR C% = 0 TO 255
        bmpfile.biPalette%(C%) = C% OR C%<<8 OR C%<<16
      NEXT

      REM Display image at a random offset into the data:
      frame% = 0
      TIME = 0
      REPEAT
        bmpfile.bfOffBits% = random% - bmpfile{} + RND(images%)
        OSCLI "MDISPLAY " + STR$~bmpfile{}
        frame% += 1
        IF TIME>10 THEN
          SYS "SetWindowText", @hwnd%, "BBC BASIC: " + STR$(frame%*100 DIV TIME) + " fps"
        ENDIF
      UNTIL FALSE
```

[[File:image_noise_bbc.jpg]]


## C

```cpp
#include <iostream>
#include <stdio.h>
#include <time.h>
#include <SDL/SDL.h>

unsigned int frames = 0;
unsigned int t_acc = 0;

void print_fps ()
{
  static Uint32 last_t = 0;
  Uint32 t = SDL_GetTicks();
  Uint32 dt = t - last_t;
  t_acc += dt;
  if (t_acc > 1000)
  {
    unsigned int el_time = t_acc / 1000;
    printf("- fps: %g\n",
            (float) frames / (float) el_time);
    t_acc = 0;
    frames = 0;
  }
  last_t = t;
}

void blit_noise(SDL_Surface *surf)
{
  unsigned int i;
  long dim = surf->w * surf->h;
  while (1)
  {
    SDL_LockSurface(surf);
    for (i=0; i < dim; ++i) {
      ((unsigned char *)surf->pixels)[i] = ((rand() & 1) ? 255 : 0);
    }
    SDL_UnlockSurface(surf);
    SDL_Flip(surf);
    ++frames;
    print_fps();
  }
}

int main(void)
{
  SDL_Surface *surf = NULL;
  srand((unsigned int)time(NULL));
  SDL_Init(SDL_INIT_TIMER | SDL_INIT_VIDEO);
  surf = SDL_SetVideoMode(320, 240, 8, SDL_DOUBLEBUF | SDL_HWSURFACE);
  blit_noise(surf);
}
```


### Fast OpenGL method

Depending on your hardware, you might be able to get thousands of frames per second.  Compiled with <code>gcc -lglut -lGL -g -Wall -O2</code>.

```c
#include <GL/glut.h>
#include <GL/gl.h>
#include <stdio.h>
#include <time.h>

#define W 320
#define H 240
#define slen W * H / sizeof(int)

time_t start, last;

void render()
{
	static int frame = 0, bits[slen];
	register int i = slen, r;
	time_t t;

	r = bits[0] + 1;
	while (i--) r *= 1103515245, bits[i] = r ^ (bits[i] >> 16);

	glClear(GL_COLOR_BUFFER_BIT);
	glBitmap(W, H, 0, 0, 0, 0, (void*)bits);
	glFlush();

	if (!(++frame & 15)) {
		if ((t = time(0)) > last) {
			last = t;
			printf("\rfps: %ld  ", frame / (t - start));
			fflush(stdout);
		}
	}
}

int main(int argc, char **argv)
{
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_INDEX);
	glutInitWindowSize(W, H);
	glutCreateWindow("noise");
	glutDisplayFunc(render);
	glutIdleFunc(render);

	last = start = time(0);

	glutMainLoop();
	return 0;
}
```



## C++

[[File:noise_cpp.png]]

```cpp

#include <windows.h>
#include <sstream>
#include <tchar.h>
//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const unsigned int BMP_WID = 320, BMP_HEI = 240, WHITE = 16777215, BLACK = 0;

//--------------------------------------------------------------------------------------------------
class myBitmap
{
public:
    myBitmap() : pen( NULL ), brush( NULL ), clr( 0 ), wid( 1 ) {}
    ~myBitmap()
    {
	DeleteObject( pen ); DeleteObject( brush );
	DeleteDC( hdc ); DeleteObject( bmp );
    }

    bool create( int w, int h )
    {
	BITMAPINFO    bi;
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

    void clear( BYTE clr = 0 )
    {
	memset( pBits, clr, width * height * sizeof( DWORD ) );
    }

    void setBrushColor( DWORD bClr )
    {
	if( brush ) DeleteObject( brush );
	brush = CreateSolidBrush( bClr );
	SelectObject( hdc, brush );
    }

    void setPenColor( DWORD c ) { clr = c; createPen(); }
    void setPenWidth( int w ) { wid = w; createPen(); }

    void saveBitmap( string path )
    {
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

    void* getBits( void ) const { return pBits; }
    HDC getDC() const     { return hdc; }
    int getWidth() const  { return width; }
    int getHeight() const { return height; }

private:
    void createPen()
    {
	if( pen ) DeleteObject( pen );
	pen = CreatePen( PS_SOLID, wid, clr );
	SelectObject( hdc, pen );
    }

    HBITMAP bmp;
    HDC     hdc;
    HPEN    pen;
    HBRUSH  brush;
    void*   pBits;
    int     width, height, wid;
    DWORD   clr;
};
//--------------------------------------------------------------------------------------------------
class bmpNoise
{
public:
    bmpNoise()
    {
	QueryPerformanceFrequency( &_frequency );
	_bmp.create( BMP_WID, BMP_HEI );
	_frameTime = _fps = 0; _start = getTime(); _frames = 0;
    }

    void mainLoop()
    {
	float now = getTime();
	if( now - _start > 1.0f ) { _fps = static_cast<float>( _frames ) / ( now - _start ); _start = now; _frames = 0; }
	HDC wdc, dc = _bmp.getDC();
	unsigned int* bits = reinterpret_cast<unsigned int*>( _bmp.getBits() );

	for( int y = 0; y < BMP_HEI; y++ )
	{
	    for( int x = 0; x < BMP_WID; x++ )
	    {
		if( rand() % 10 < 5 ) memset( bits, 255, 3 );
		else memset( bits, 0, 3 );
		bits++;
	    }
	}
	ostringstream o; o << _fps; TextOut( dc, 0, 0, o.str().c_str(), o.str().size() );

	wdc = GetDC( _hwnd );
	BitBlt( wdc, 0, 0, BMP_WID, BMP_HEI, dc, 0, 0, SRCCOPY );
	ReleaseDC( _hwnd, wdc );
	_frames++; _frameTime = getTime() - now;
	if( _frameTime > 1.0f ) _frameTime = 1.0f;
    }

    void setHWND( HWND hwnd ) { _hwnd = hwnd; }

private:
    float getTime()
    {
	LARGE_INTEGER liTime; QueryPerformanceCounter( &liTime );
	return liTime.QuadPart  / ( float )_frequency.QuadPart;
    }
    myBitmap      _bmp;
    HWND          _hwnd;
    float         _start, _fps, _frameTime;
    unsigned int  _frames;
    LARGE_INTEGER _frequency;
};
//--------------------------------------------------------------------------------------------------
class wnd
{
public:
    wnd() { _inst = this; }
    int wnd::Run( HINSTANCE hInst )
    {
	_hInst = hInst; _hwnd = InitAll();
        _noise.setHWND( _hwnd );
	ShowWindow( _hwnd, SW_SHOW );
	UpdateWindow( _hwnd );

	MSG msg;
	ZeroMemory( &msg, sizeof( msg ) );
	while( msg.message != WM_QUIT )
	{
	    if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) != 0 )
	    {
		TranslateMessage( &msg );
		DispatchMessage( &msg );
	    }
	    else
	    {
		_noise.mainLoop();
	    }
	}
	return UnregisterClass( "_MY_NOISE_", _hInst );
    }
private:
    static int WINAPI wnd::WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
    {
	switch( msg )
	{
	    case WM_DESTROY: PostQuitMessage( 0 ); break;
	    default:
	        return DefWindowProc( hWnd, msg, wParam, lParam );
	}
	return 0;
    }

    HWND InitAll()
    {
	WNDCLASSEX wcex;
	ZeroMemory( &wcex, sizeof( wcex ) );
	wcex.cbSize           = sizeof( WNDCLASSEX );
	wcex.style           = CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc   = ( WNDPROC )WndProc;
	wcex.hInstance     = _hInst;
	wcex.hCursor       = LoadCursor( NULL, IDC_ARROW );
	wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
	wcex.lpszClassName = "_MY_NOISE_";

	RegisterClassEx( &wcex );

	RECT rc = { 0, 0, BMP_WID, BMP_HEI };
	AdjustWindowRect( &rc, WS_SYSMENU | WS_CAPTION, FALSE );
	int w = rc.right - rc.left, h = rc.bottom - rc.top;
	return CreateWindow( "_MY_NOISE_", ".: Noise image -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, w, h, NULL, NULL, _hInst, NULL );
    }

    static wnd* _inst;
    HINSTANCE   _hInst;
    HWND        _hwnd;
    bmpNoise    _noise;
};
wnd* wnd::_inst = 0;
//--------------------------------------------------------------------------------------------------
int APIENTRY _tWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow )
{
    srand( GetTickCount() ); wnd myWnd;
    return myWnd.Run( hInstance );
}
//--------------------------------------------------------------------------------------------------

```


## C#
Max 185 FPS on .NET 4.0/Windows 7 64-bit on Athlon II X4 620 - ATI Radeon X1200.


```c#
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.Linq;
using System.Runtime.InteropServices;
using System.Windows.Forms;

class Program
{
    static Size size = new Size(320, 240);
    static Rectangle rectsize = new Rectangle(new Point(0, 0), size);
    static int numpixels = size.Width * size.Height;
    static int numbytes = numpixels * 3;

    static PictureBox pb;
    static BackgroundWorker worker;

    static double time = 0;
    static double frames = 0;
    static Random rand = new Random();

    static byte tmp;
    static byte white = 255;
    static byte black = 0;
    static int halfmax = int.MaxValue / 2; // more voodoo! calling Next() is faster than Next(2)!

    static IEnumerable<byte> YieldVodoo()
    {
        // Yield 3 times same number (i.e 255 255 255) for numpixels times.

        for (int i = 0; i < numpixels; i++)
        {
            tmp = rand.Next() < halfmax ? black : white; // no more lists!

            // no more loops! yield! yield! yield!
            yield return tmp;
            yield return tmp;
            yield return tmp;
        }
    }

    static Image Randimg()
    {
        // Low-level bitmaps
        var bitmap = new Bitmap(size.Width, size.Height);
        var data = bitmap.LockBits(rectsize, ImageLockMode.WriteOnly, PixelFormat.Format24bppRgb);

        Marshal.Copy(
            YieldVodoo().ToArray<byte>(),// source
            0, // start
            data.Scan0, // scan0 is a pointer to low-level bitmap data
            numbytes); // number of bytes in source

        bitmap.UnlockBits(data);
        return bitmap;
    }

    [STAThread]
    static void Main()
    {
        var form = new Form();

        form.AutoSize = true;
        form.Size = new Size(0, 0);
        form.Text = "Test";

        form.FormClosed += delegate
        {
            Application.Exit();
        };

        worker = new BackgroundWorker();

        worker.DoWork += delegate
        {
            System.Threading.Thread.Sleep(500); // remove try/catch, just wait a bit before looping

            while (true)
            {
                var a = DateTime.Now;
                pb.Image = Randimg();
                var b = DateTime.Now;

                time += (b - a).TotalSeconds;
                frames += 1;

                if (frames == 30)
                {
                    Console.WriteLine("{0} frames in {1:0.000} seconds. ({2:0} FPS)", frames, time, frames / time);

                    time = 0;
                    frames = 0;
                }
            }
        };

        worker.RunWorkerAsync();

        FlowLayoutPanel flp = new FlowLayoutPanel();
        form.Controls.Add(flp);

        pb = new PictureBox();
        pb.Size = size;

        flp.AutoSize = true;
        flp.Controls.Add(pb);

        form.Show();
        Application.Run();
    }
}
```



## Common Lisp

noise_sdl.lisp:

```lisp
;; (require :lispbuilder-sdl)

(defun draw-noise (surface)
  "draws noise on the surface. Returns the surface"
  (let ((width (sdl:width surface))
	(height (sdl:height surface))
	(i-white (sdl:map-color sdl:*white* surface))
	(i-black (sdl:map-color sdl:*black* surface)))
    (sdl-base::with-pixel (s (sdl:fp surface))
      (dotimes (h height)
	(dotimes (w width)
	  (sdl-base::write-pixel s w h (if (zerop (random 2))
					   i-white i-black ))))))
  surface)

(defun draw-fps (surface)
  "draws fps text-info on surface. Returns surface"
  (sdl:with-surface (s surface)
    (sdl:draw-string-solid-* (format nil "FPS: ~,3f" (sdl:average-fps))
			     20 20 :surface s :color sdl:*magenta*)))

(defun main ()
  "main function, initializes the library and creates de display window"
  (setf *random-state* (make-random-state))
  (sdl:with-init (SDL:SDL-INIT-VIDEO SDL:SDL-INIT-TIMER)
    (let ((main-window (sdl:window 320 240
				   :title-caption "noise_sdl.lisp"
				   :bpp 8
				   :flags (logior SDL:SDL-DOUBLEBUF SDL:SDL-HW-SURFACE)
				   :fps (make-instance 'sdl:fps-unlocked))))
      (sdl:initialise-default-font)
      (sdl:with-events ()
	(:idle ()
	       (sdl:update-display (draw-fps (draw-noise main-window))))
	(:video-expose-event ()
			     (sdl:update-display))
	(:quit-event () T)))))

(main)

```




## D

```D
import std.stdio, std.random, sdl.SDL;

void main() {
  SDL_Init(SDL_INIT_TIMER | SDL_INIT_VIDEO);
  auto surface = SDL_SetVideoMode(320,240,8, SDL_DOUBLEBUF|SDL_HWSURFACE);

  uint frameNumber, totalTime, lastTime;
  while (true) {
    SDL_LockSurface(surface);
    foreach (i; 0 .. surface.w * surface.h)
      (cast(ubyte*)surface.pixels)[i] = (uniform(0, 2) ? 255 : 0);
    SDL_UnlockSurface(surface);
    SDL_Flip(surface);
    frameNumber++;

    uint time = SDL_GetTicks();
    totalTime += time - lastTime;
    if (totalTime > 1000) {
      writeln("FPS: ", frameNumber / (totalTime / 1000.0));
      totalTime = frameNumber = 0;
    }
    lastTime = time;
  }
}
```

This D version shows about 155 FPS, while on the same PC the C version shows about 180 FPS.

Generating random bits with the C core.stdc.stdlib.rand the performance becomes about the same of the C version.


## Euler Math Toolbox


Currently, Euler Math Toolbox does not have optimized routines to plot matrices with color information. The frames per second are consequently not really good.


```Euler Math Toolbox

>function noiseimg () ...
$aspect(320,240); clg;
$count=0; now=time;
$repeat
$  plotrgb(intrandom(240,420,2)-1,[0,0,1024,1024]);
$  wait(0);
$  count=count+1;
$  until testkey();
$end;
$return count/(time-now);
$endfunction
>noiseimg
 2.73544353263

```


=={{header|F Sharp|F#}}==
This implementation includes two methods to update the pixels values. One uses unsafe methods and can do 350 fps on my machine, the second uses safe code to marshal the new values onto the bitmap data and can do 240 fps on the same machine.

```fsharp
open System.Windows.Forms
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices
open System.Diagnostics
open Microsoft.FSharp.NativeInterop
#nowarn "9"

let rnd = System.Random()

// Draw pixels using unsafe native pointer accessor.
// This updates the bitmap as fast as possible.
let drawbits_fast (size:int) (bits:BitmapData) =
    let mutable (p:nativeptr<byte>) = NativePtr.ofNativeInt(bits.Scan0)
    for n = 0 to size-1 do
        let c = rnd.Next(2) * 255
        NativePtr.set p 2 (byte c)
        NativePtr.set p 1 (byte c)
        NativePtr.set p 0 (byte c)
        NativePtr.set p 3 (byte 255)
        p <- NativePtr.add p 4

// A reasonably efficient updater using marshalling to copy an array of generated
// integers onto the managed bitmap pixel data (see the C# example as well).
let drawbits_safe (size:int) (bits:BitmapData) =
    let data = Array.init size (fun n ->
            let c = rnd.Next(2) * 255
            0xff000000 ||| (c <<< 16) ||| (c <<< 8) ||| c)
    Marshal.Copy(data, 0, bits.Scan0, size) |> ignore

// Create a new bitmap and update using the specified function
let make_image (width:int) (height:int) f =
    let size = width * height
    let bmp = new Bitmap(width, height)
    let bits = bmp.LockBits(Rectangle(0,0,width,height), ImageLockMode.WriteOnly, PixelFormat.Format32bppArgb)
    f size bits
    bmp.UnlockBits(bits)
    bmp

// Draw 30 frames and record the time and display the frames per second
// This function is run asynchronously to avoid blocking the main GUI thread.
let drawImage (box:PictureBox) (label:Label) f = async {
    while true do
        let timer = new Stopwatch()
        timer.Start()
        for frames = 0 to 29 do
            let bmp = make_image 320 240 f
            box.Image <- bmp
        timer.Stop()
        let fps = 30000. / timer.Elapsed.TotalMilliseconds
        label.Text <- sprintf "%.1f fps" fps }

[<System.STAThread>]
[<EntryPoint>]
let main args =
    let form = new Form(AutoSize=true,
                        Size=new Size(0,0),
                        Text="image noise demo")
    let panel = new FlowLayoutPanel(AutoSize=true,FlowDirection=FlowDirection.TopDown)
    let box = new PictureBox(AutoSize=true)
    let label = new Label(AutoSize=true, Text="Ready")
    form.FormClosed.Add(fun eventArgs -> Async.CancelDefaultToken()
                                         Application.Exit())
    form.Controls.Add(panel)
    panel.Controls.Add(box)
    panel.Controls.Add(label)
    if args.Length > 0 && args.[0] = "-safe" then
        drawImage box label drawbits_safe |> Async.Start
    else
        drawImage box label drawbits_fast |> Async.Start
    form.Show()
    Application.Run()
    0

```



## Factor


~150 FPS


```factor
USING: accessors calendar images images.viewer kernel math
math.parser models models.arrow random sequences threads timers
ui ui.gadgets ui.gadgets.labels ui.gadgets.packs ;
IN: rosetta-code.image-noise

: bits>pixels ( bits -- bits' pixels )
    [ -1 shift ] [ 1 bitand ] bi 255 * ; inline

: ?generate-more-bits ( a bits -- a bits' )
    over 32 mod zero? [ drop random-32 ] when ; inline

: <random-images-bytes> ( dim -- bytes )
    [ 0 0 ] dip product  [
        ?generate-more-bits
        [ 1 + ] [ bits>pixels ] bi*
    ] B{ } replicate-as 2nip ;

: <random-bw-image> ( -- image )
    <image>
        { 320 240 } [ >>dim ] [ <random-images-bytes> >>bitmap ] bi
        L >>component-order
        ubyte-components >>component-type ;

TUPLE: bw-noise-gadget < image-control timers cnt old-cnt fps-model ;

: animate-image ( control -- )
    [ 1 + ] change-cnt
    model>> <random-bw-image> swap set-model ;

: update-cnt ( gadget -- )
    [ cnt>> ] [ old-cnt<< ] bi ;

: fps ( gadget -- fps )
    [ cnt>> ] [ old-cnt>> ] bi - ;

: fps-monitor ( gadget -- )
    [ fps ] [ update-cnt ] [ fps-model>> set-model ] tri ;

: start-animation ( gadget -- )
    [ [ animate-image ] curry 1 nanoseconds every ] [ timers>> push ] bi ;

: start-fps ( gadget -- )
    [ [ fps-monitor ] curry 1 seconds every ] [ timers>> push ] bi ;

: setup-timers ( gadget -- )
    [ start-animation ] [ start-fps ] bi ;

: stop-animation ( gadget -- )
    timers>> [ [ stop-timer ] each ] [ delete-all ] bi ;

M: bw-noise-gadget graft* [ call-next-method ] [ setup-timers ] bi ;

M: bw-noise-gadget ungraft* [ stop-animation ] [ call-next-method ] bi ;

: <bw-noise-gadget> ( -- gadget )
    <random-bw-image> <model> bw-noise-gadget new-image-gadget*
    0 >>cnt 0 >>old-cnt 0 <model> >>fps-model V{ } clone >>timers ;

: fps-gadget ( model -- gadget )
    [ number>string ] <arrow> <label-control>
    "FPS: " <label>
    <shelf> swap add-gadget swap add-gadget ;

: with-fps ( gadget -- gadget' )
    [ fps-model>> fps-gadget ]
    [ <pile> swap add-gadget swap add-gadget ] bi ;

MAIN-WINDOW: open-noise-window
    { { title "Black and White noise" } }
    <bw-noise-gadget> with-fps >>gadgets ;
```




## FreeBASIC


```freebasic
' version 13-07-2018
' compile with: fbc -s console
'           or: fbc -s gui

' hit any to key to stop program

Randomize Timer
Screen 13

If ScreenPtr = 0 Then
    Print "Error setting video mode!"
    End
End If

Palette 0, 0                  ' black
Palette 1, RGB(255, 255, 255) ' white

Dim As UInteger c, x, y, Col
Dim As Double fps, t = Timer

' empty keyboard buffer
While InKey <> "" : Wend

While InKey = ""

    For x = 0 To 319
        For y = 0 To 199
            ' color is as integer, a float gets rounded off by FreeBASIC
            PSet(x, y), Rnd
        Next
    Next
    c += 1
    fps = c / (Timer - t)
    WindowTitle "fps = " + Str(fps)

Wend
```



## Go


```go
package main

import (
    "code.google.com/p/x-go-binding/ui/x11"
    "fmt"
    "image"
    "image/color"
    "image/draw"
    "log"
    "os"
    "time"
)

var randcol = genrandcol()

func genrandcol() <-chan color.Color {
    c := make(chan color.Color)

    go func() {
        for {
            select {
            case c <- image.Black:
            case c <- image.White:
            }
        }
    }()

    return c
}

func gennoise(screen draw.Image) {
    for y := 0; y < 240; y++ {
        for x := 0; x < 320; x++ {
            screen.Set(x, y, <-randcol)
        }
    }
}

func fps() chan<- bool {
    up := make(chan bool)

    go func() {
        var frames int64
        var lasttime time.Time
        var totaltime time.Duration

        for {
            <-up
            frames++
            now := time.Now()
            totaltime += now.Sub(lasttime)
            if totaltime > time.Second {
                fmt.Printf("FPS: %v\n", float64(frames)/totaltime.Seconds())
                frames = 0
                totaltime = 0
            }
            lasttime = now
        }
    }()

    return up
}

func main() {
    win, err := x11.NewWindow()
    if err != nil {
        fmt.Println(err)
        os.Exit(1)
    }
    defer win.Close()

    go func() {
        upfps := fps()
        screen := win.Screen()

        for {
            gennoise(screen)

            win.FlushImage()

            upfps <- true
        }
    }()

    for _ = range win.EventChan() {
    }
}

```


### Optimized example

A second example that is somewhat more optimized but maybe more complicated.
(~3000fps on a Thinkpad x220 laptop)

```go
package main

/* Note, the x-go-binding/ui/x11 lib is under development and has as a temp solution
   set the x window to a static height and with, you have to manualy set these
   to 240 x 320 in code.google.com/p/x-go-binding/ui/x11/conn.go */

import "code.google.com/p/x-go-binding/ui"
import "code.google.com/p/x-go-binding/ui/x11"
import "fmt"
import "image"
import "image/draw"
import "log"
import "math/rand"
import "runtime"
import "time"

var bw[65536][64]byte
var frameCount = make(chan uint)

func main() {
   tc := runtime.NumCPU()
   runtime.GOMAXPROCS(tc)

   // Initiate a new x11 screen to print images onto
   win, err := x11.NewWindow()
   if err != nil {
      log.Fatalln(err)
   }
   defer win.Close()
   screen := win.Screen()
   _, ok := screen.(*image.RGBA)
   if !ok {
      log.Fatalln("screen isn't an RGBA image.")
   }

   //Create lookup table for every combination of 16 black/white pixels
   var i, j uint
   for i = 0; i< 65536; i++ {
      for j = 0; j < 16; j++ {
      if i & (1 << j) > 0 {
            bw[i][j*4 + 0] = 0xFF
            bw[i][j*4 + 1] = 0xFF
            bw[i][j*4 + 2] = 0xFF
         }
      }
   }
   // Start fps counter in a new goroutin
   go fps()
   // Start goroutines
   for i := 0; i < tc; i++ {
       go createNoise(win, screen)
   }
   createNoise(win, screen)
}

func createNoise(win ui.Window, screen draw.Image) {
   var rnd, rnd2 uint64
   var rnd16a, rnd16b, rnd16c, rnd16d uint16
   var img [240 * 320 * 4]byte
   // Populate the image with pixel data
   for {
      for i := 0; i < len(img); i += 256 {
         rnd = uint64(rand.Int63())
         if (i % 63) == 0 {
            rnd2 = uint64(rand.Int63())
         }
         rnd |= rnd2 & 1 << 63 // we have to set the 64'th bit from the rand.Int63() manualy
         rnd16a = uint16( rnd        & 0x000000000000FFFF)
         rnd16b = uint16((rnd >> 16) & 0x000000000000FFFF)
         rnd16c = uint16((rnd >> 32) & 0x000000000000FFFF)
         rnd16d = uint16((rnd >> 48) & 0x000000000000FFFF)
         copy(img[i    :i+ 64], bw[rnd16a][:])
         copy(img[i+ 64:i+128], bw[rnd16b][:])
         copy(img[i+128:i+192], bw[rnd16c][:])
         copy(img[i+192:i+256], bw[rnd16d][:])
         rnd2 = rnd2 >> 1 // rotate to next random bit
      }
      // Copy pixel data to the screen
      copy(screen.(*image.RGBA).Pix, img[:])
      frameCount <- 1
      win.FlushImage()
   }
}

func fps() {
   last := time.Now()
   var fps uint
   for {
      // wait for a frameCount update
      <-frameCount
      fps++
      if time.Since(last) >= time.Second {
         fmt.Println("fps:", fps)
         fps = 0
         last = time.Now()
      }
   }
}

```

[[File:GoNoise.png]]


## Haskell

This uses the GLFW-b bindings for GLFW 3 support:

```txt
cabal install glfw-b
```


```Haskell
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Foreign as F
import qualified System.Random as R

height = 240
width = 320
numbytes = height * width
imagesize = GL.Size (fromIntegral width) (fromIntegral height)

main :: IO ()
main = do
  isInit <- GLFW.init
  if isInit
  then do
    m <- GLFW.createWindow width height "" Nothing Nothing
    case m of
      Just win -> do
             GLFW.makeContextCurrent m
             GLFW.swapInterval 1
             GLFW.setKeyCallback win $ Just keyCallback
             glLoop win
             GLFW.destroyWindow win
      Nothing  -> return ()
    GLFW.terminate
  else return ()

glLoop :: GLFW.Window -> IO ()
glLoop win = do
  foreignPixels <- F.mallocForeignPtrArray numbytes
  F.withForeignPtr foreignPixels
       (\pixels -> do
          let pixelData = GL.PixelData GL.Luminance GL.UnsignedByte pixels
          loop pixelData pixels 0)
    where
      loop pixelData pixels frames = do
               close <- GLFW.windowShouldClose win
               if close
               then return ()
               else do
                   randomizePixels pixels
                   GL.drawPixels imagesize pixelData
                   GLFW.swapBuffers win
                   GLFW.pollEvents
                   time <- GLFW.getTime
                   let fps =
                           case time of
                             Just t  -> show $ (fromIntegral frames) / t
                             Nothing -> "???"
                   GLFW.setWindowTitle win $ "FPS: " ++ fps
                   loop pixelData pixels $ frames + 1

randomizePixels :: F.Ptr GL.GLubyte -> IO ()
randomizePixels ptr = iter 0 numbytes
    where
      iter index range
          | index == range = return ()
          | otherwise = do
        v <- R.randomRIO (0, 1)
        F.pokeElemOff ptr index $ v * 255
        iter (index + 1) range

keyCallback :: GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState ->
               GLFW.ModifierKeys -> IO ()
keyCallback win key _ action _ =
    case key of
      GLFW.Key'Q -> GLFW.setWindowShouldClose win True
      _          -> return ()
```


=={{header|Icon}} and {{header|Unicon}}==

Icon/Unicon provide a portable graphics interface that runs on multiple platforms.  The frame rates will be lower than many of the other languages.
There are several possible approaches to painting this random noise.
* Using DrawPoint(c,r) for each pixel base on essentially a coin flip  (speed ~= 1x)
* Using DrawPoint!L on a list L := [c1,r1,c2,r2,...] of foreground pixels to be painted based on a coin flip (speed ~= 2x)
* Using DrawImage to draw a randomly constructed bi-level images(see pg 157 of the graphics book, speed ~= 10x)


```Icon
link printf

procedure main()
   &window := open("B&W noise 320x240","g","size=320,240","bg=white","fg=black") |
              stop("Open window failed ")
   runtime := 10 # seconds to run
   sec := &now
   frames := 0
   until (&now - sec) >= runtime do {
      s := "320,#"
      every 1 to 240 & 1 to 320/4 do s ||:= ?"0123456789ABCDEF"
      DrawImage(0,0,s)
      frames +:= 1
      }
   sec := &now - sec
   printf("frames=%d, elapsed time=%r, fps=%r\n",frames,sec, frames/real(sec))
   Event()   # wait for any window event
   close(&window)
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides a family of print formatting routines]


## J


```j
coclass'example'
(coinsert[require)'jzopengl'

P=: 0 : 0
pc p nosize;
xywh 0 0 160 120;cc c isigraph opengl;
pas 0 0;pcenter;
rem form end;
 pshow;
 timer 1;
)

timestamp=: (6!:8'') %~ 6!:9

create=:3 :0
  ogl=:''conew'jzopengl'
  frames=:0
  start=: timestamp''
  sys_timer_base_=: ''1 :('p_c_paint_',(;coname''),'_')
  wd P
)

p_run=: 3 : 0
  ''conew'example'
)

destroy=:3 :0
  end=:timestamp''
  smoutput 'frames per second: ',":frames%end-start
  wd 'timer 0'
  destroy__ogl''
  wd'pclose'
  codestroy''
)

p_close=: destroy

p_c_paint=: 3 : 0
  rc__ogl''
  glClear GL_COLOR_BUFFER_BIT
  glBegin GL_POINTS
    glVertex _1+2*53050 2?@$ 0
  glEnd''
  show__ogl''
  frames=:frames+1
)

p_run''
```


The script auto-starts when run (that last line <code><nowiki>p_run''</nowiki></code> is responsible for the auto-start.

Average FPS are displayed when the window is closed.

With this revision, on my laptop, I typically get in the range of 58..59 FPS, with a cpu load from J of about 3% (sometimes as much as 5%, sometimes as low as 0.1%).  I am probably limited by v-sync, and (hypothetically speaking) if I tuned my opengl drivers I perhaps could get significantly faster fps.  However, since my screen only refreshes approximately 60 times per second, anything over that would be meaningless.

About some of the constants:  160 120 corresponds to a 320 by 240 rendering area (this J version 6 mechanism will be obsolete soon, but I think we should wait for the new code bases stabilize before replacing this code), and the rendering area is not resizable.  53050 was picked because on average half of the pixels will be dark and half of them will be light: We have a black background and 53050 random pixel coordinates (out of 75800 total pixels) being set to being light -- because so many of them will randomly overlap we have on average approximately 50% of each.  It would be more efficient to use a pixel shader, pushing the randomizing process into the graphics hardware. But, with this small of a display, efficiency is not really an issue with this approach.


## Java

This could be done more concisely, but the version below features the following enhancements:

- Resizable window

- Real-time blurring of noise behind the FPS display

- Cycling through FPS display modes using mouse clicks

- Very fast: 1000+ FPS on a 2.8 GHz Core Duo (with 64-bit JRE). This is capped because the maximum resolution of the timers available is 1 ms

```java
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.Arrays;
import java.util.Random;
import javax.swing.*;

public class ImageNoise {
    int framecount = 0;
    int fps = 0;
    BufferedImage image;
    Kernel kernel;
    ConvolveOp cop;
    JFrame frame = new JFrame("Java Image Noise");

    JPanel panel = new JPanel() {
        private int show_fps = 0; // 0 = blur + FPS; 1 = FPS only; 2 = neither
        private MouseAdapter ma = new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                show_fps = (show_fps + 1) % 3;
            }
        };
        {addMouseListener(ma);}

        @Override
        public Dimension getPreferredSize() {
            return new Dimension(320, 240);
        }

        @Override
        @SuppressWarnings("fallthrough")
        public void paintComponent(Graphics g1) {
            Graphics2D g = (Graphics2D) g1;
            drawNoise();
            g.drawImage(image, 0, 0, null);

            switch (show_fps) {
            case 0:
                // add blur behind FPS
                int xblur = getWidth() - 130, yblur = getHeight() - 32;
                BufferedImage bc = image.getSubimage(xblur, yblur, 115, 32);
                BufferedImage bs = new BufferedImage(bc.getWidth(), bc.getHeight(),
                                                     BufferedImage.TYPE_BYTE_GRAY);
                cop.filter(bc, bs);
                g.drawImage(bs, xblur, yblur , null);
            case 1:
                // add FPS text; case fallthough is deliberate
                g.setColor(Color.RED);
                g.setFont(new Font("Monospaced", Font.BOLD, 20));
                g.drawString("FPS: " + fps, getWidth() - 120, getHeight() - 10);
            }
            framecount++;
        }
    };

    // Timer to trigger update display, with 1 ms delay
    Timer repainter = new Timer(1, new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
            panel.repaint();
        }
    });

    // Timer to check FPS, once per second
    Timer framerateChecker = new Timer(1000, new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
            fps = framecount;
            framecount = 0;
        }
    });

    public ImageNoise() {
        // Intitalize kernel describing blur, and convolve operation based on this
        float[] vals = new float[121];
        Arrays.fill(vals, 1/121f);
        kernel = new Kernel(11, 11, vals);
        cop = new ConvolveOp(kernel, ConvolveOp.EDGE_NO_OP, null);

        // Initialize frame and timers
        frame.add(panel);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        frame.setVisible(true);
        repainter.start();
        framerateChecker.start();
    }

    void drawNoise() {
        int w = panel.getWidth(), h = panel.getHeight();

        // Check if our image is null or window has been resized, requiring new image
        if (null == image || image.getWidth() != w || image.getHeight() != h) {
            image = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_GRAY);
        }
        Random rand = new Random();
        int[] data = new int[w * h];
        // Each int has 32 bits so we can use each bit for a different pixel - much faster
        for (int x = 0; x < w * h / 32; x++) {
            int r = rand.nextInt();
            for (int i = 0; i < 32; i++) {
                data[x * 32 + i] = (r & 1) * Integer.MAX_VALUE;
                r >>>= 1;
            }
        }
        // Copy raw data to the image's raster
        image.getRaster().setPixels(0, 0, w, h, data);
    }

    public static void main(String[] args) {
        // Invoke GUI on the Event Dispatching Thread
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                ImageNoise i = new ImageNoise();
            }
        });
    }
}
```
[[File:javanoise2.png]]


## JavaScript

[http://jsfiddle.net/bZJvr/ jsFiddle Demo]


```javascript><body

<canvas id='c'></canvas>

<script>
var canvas = document.getElementById('c');
var ctx = canvas.getContext('2d');

var w = canvas.width = 320;
var h = canvas.height = 240;
var t1 = new Date().getTime();
var frame_count = 0;
ctx.font = 'normal 400 24px/2 Unknown Font, sans-serif';
var img = ctx.createImageData(w, h);

var index_init = 0;
for (var x = 0; x < w; x++) {
    for (var y = 0; y < h; y++) {
        img.data[index_init + 3] = 255; // alpha
        index_init += 4;
    }
}

function animate() {
    var index = 0;
    for (var x = 0; x < w; x++) {
        for (var y = 0; y < h; y++) {
            var value = (Math.random() > 0.5) ? 255 : 0;
            img.data[index    ] = value;
            img.data[index + 1] = value;
            img.data[index + 2] = value;
            // alpha channel is constant
            index += 4;
        }
    }

    ctx.putImageData(img, 0, 0);

    frame_count++;
    if (frame_count % 50 == 0) {
        var fps = frame_count / (new Date().getTime() - t1) * 1000;
        window.status = fps.toFixed(2) + " fps";
    }

    setTimeout(animate, 0);
}

animate();
</script>
</body>
```

About 59 frames/second on Firefox 4.


## Julia

With Gtk, gets about 80 frames per second on a older, dual core 3 Ghz processor.

```julia
using Gtk, GtkUtilities

 function randbw(ctx, w, h)
    pic = zeros(Int64, w, h)
    for i in 1:length(pic)
        pic[i] = rand([1, 0])
    end
    copy!(ctx, pic)
end

const can = @GtkCanvas()
const win = GtkWindow(can, "Image Noise", 320, 240)

@guarded draw(can) do widget
    ctx = getgc(can)
    h = height(can)
    w = width(can)
    randbw(ctx, w, h)
end

show(can)
const cond = Condition()
endit(w) = notify(cond)
signal_connect(endit, win, :destroy)

while true
    frames = 0
    t = time()
    for _ in 1:100
        draw(can)
        show(can)
        sleep(0.0001)
        frames += 1
    end
    fps = round(frames / (time() - t), digits=1)
    set_gtk_property!(win, :title, "Image Noise: $fps fps")
end

wait(cond)

```



## Kotlin

```scala
// version 1.2.10

import java.awt.*
import java.awt.event.*
import java.awt.image.*
import java.util.Random
import javax.swing.*

class ImageNoise {
    var framecount = 0
    var fps = 0
    lateinit var image: BufferedImage
    val kernel: Kernel
    lateinit var cop: ConvolveOp
    val frame = JFrame("Java Image Noise")

    val panel = object : JPanel() {
        private var showFps = 0  // 0 = blur + FPS; 1 = FPS only; 2 = neither
        private val ma = object : MouseAdapter() {
            override fun mouseClicked(e: MouseEvent) {
                showFps = (showFps + 1) % 3
            }
        }

        init {
            addMouseListener(ma)
            preferredSize = Dimension(320, 240)
        }

        override fun paintComponent(g1: Graphics) {
            val g = g1 as Graphics2D
            drawNoise()
            g.drawImage(image, 0, 0, null)
            if (showFps == 0) {
                // add blur behind FPS
                val xblur = width - 130
                val yblur = height - 32
                val bc = image.getSubimage(xblur, yblur, 115, 32)
                val bs = BufferedImage(
                    bc.width, bc.height, BufferedImage.TYPE_BYTE_GRAY
                )
                cop.filter(bc, bs)
                g.drawImage(bs, xblur, yblur, null)
            }
            if (showFps <= 1) {
                // add FPS text
                g.color = Color.RED
                g.font = Font("Monospaced", Font.BOLD, 20)
                g.drawString("FPS: $fps", width - 120, height - 10)
            }
            framecount++
        }
    }

    // Timer to trigger update display, with 1 ms delay
    val repainter = Timer(1, object: ActionListener {
        override fun actionPerformed(e: ActionEvent) = panel.repaint()
    })

    // Timer to check FPS, once per second
    val framerateChecker = Timer(1000, object : ActionListener {
        override fun actionPerformed(e: ActionEvent) {
            fps = framecount
            framecount = 0
        }
    })

    init {
        // Intitalize kernel describing blur, and convolve operation based on this
        val vals = FloatArray(121) { 1.0f / 121.0f }
        kernel = Kernel(11, 11, vals)
        cop = ConvolveOp(kernel, ConvolveOp.EDGE_NO_OP, null)

        // Initialize frame and timers
        with (frame) {
            add(panel)
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            pack()
            isVisible = true
        }
        repainter.start()
        framerateChecker.start()
    }

    fun drawNoise() {
        val w = panel.width
        val h = panel.height

        // Check if our image is initialized or window has been resized,
        // requiring new image
        if (!this::image.isInitialized || image.width != w || image.height != h) {
            image = BufferedImage(w, h, BufferedImage.TYPE_BYTE_GRAY)
        }
        val rand = Random()
        val data = IntArray(w * h)
        // Each int has 32 bits so we can use each bit for a different pixel
        // - much faster
        for (x in 0 until w * h / 32) {
            var r = rand.nextInt()
            for (i in 0..31) {
                data[x * 32 + i] = (r and 1) * Int.MAX_VALUE
                r = r ushr 1
            }
        }
        // Copy raw data to the image's raster
        image.raster.setPixels(0, 0, w, h, data)
    }
}

fun main(args: Array<String>) {
    // Invoke GUI on the Event Dispatching Thread
    SwingUtilities.invokeLater(object: Runnable {
        override fun run() {
            ImageNoise()
        }
    })
}
```


```txt

Similar to Java entry

```



## Liberty BASIC

Generates the random bitmap programmatically, then chops it each time in a different way.

```lb

WindowWidth  =411
w            =320
WindowHeight =356
h            =240


open "Noise" for graphics_nsb as #w

#w "trapclose [quit]"
#w "down"

print "Creating BMP header"

'bitmap header, 320x240 pixels 256 colors
data 66,77,54,48,1,0,0,0,0,0,54,4,0,0,40,0,0,0,64,1
data 0,0,240,0,0,0,1,0,8,0,0,0,0,0,0,44,1,0,0,0
data 0,0,0,0,0,0,0,1,0,0,0,1,0,0

head$=""
for i = 1 to 54
    read c
    head$=head$+chr$(c)
next

print "Creating BMP grayscale palette"
pal$=""
for i = 0 to 255
    pal$ = pal$ _
        + chr$(i) _
        + chr$(i) _
        + chr$(i) _
        + chr$(0)
next

print "Creating BMP random body"
    'create bitmap body
    body$=""
    for x =1 To w
        l$=""
        for y =1 To h
            l$=l$+chr$((rnd(1)>0.5)*255)
        next
        body$=body$+l$
    next

[main]
    scan
    ts =time$( "ms")
    'randomly "splice" the body: 1111222222-> 2222221111
    splice=int(len(body$)*rnd(1))+1
    body$= mid$(body$,splice+1)+left$(body$,splice)
    'write BMP
    open "noise.bmp" for output as #1
        #1 head$;pal$;
        #1 body$;
    close #1
    'load bmp
    loadbmp "noise", "noise.bmp"
    #w "cls"
    'drawbmp
    #w "drawbmp noise 0 0"

    tf =time$( "ms")
    dt =tf -ts
    if dt = 0 then dt = 1
    print "Framerate per second ="; using( "#.###", 1/(dt/1000)), "Ms per frame =";dt
    goto [main]

[quit]
    unloadbmp "noise"
    close #w
end

```



## Maple

*Note, you will need to insert a plot component to get this to work.

```Maple

a:=0;
t:=time[real]();
while a< 50 do
a:=a+1;
data := Matrix(`<|>`(`<,>`(Statistics:-Sample(Uniform(0, 1), [1000, 2])), LinearAlgebra:-RandomVector(1000, generator = rand(0 .. 2))));

f := plots:-pointplot(data[NULL .. NULL, 1], data[NULL .. NULL, 2], symbolsize = 20, symbol = solidbox, colorscheme = ["valuesplit", data[NULL .. NULL, 3], [0 = "Black", 1 = "White", 2 = "Grey"]], axes = none);
DocumentTools:-SetProperty('Plot0', 'value', plots:-display(f,plots:-textplot([0.5,0.5,sprintf("FPS: %a", (a/(time[real]()-t)))],color=red)), refresh=true);

end do:

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica

time = AbsoluteTime[]; Animate[
 Column[{Row[{"FPS: ", Round[n/(AbsoluteTime[] - time)]}],
   RandomImage[1, {320, 240}]}], {n, 1, Infinity, 1}]

```



## MAXScript


```MAXScript

try destroydialog testRollout catch ()

fn randomBitmap width height =
(
	local newBmp = bitmap width height

	for row = 0 to (height-1) do
	(
		local pixels =  for i in 1 to width collect (white*random 0 1)
		setpixels newBmp [0,row] pixels
	)

	return newBmp
)

rollout testRollout "Test" width:320 height:240
(
	bitmap image width:320 height:240 pos:[0,0]
	timer updateTimer interval:1 active:true

	on updateTimer tick do
	(
		local startTime = timestamp()
		image.bitmap = randomBitmap 320 240
		local endTime = timestamp()
		local fps = ((endTime-startTime)/1000.0)*60.0

		if mod updatetimer.ticks 10 == 0 do (testRollout.title = ("Test (FPS: "+fps as string+")"))

	)
)

createdialog testrollout

```



## Nim


Naive implementation:


```nim
import random

import rapid/gfx

var
  window = initRWindow()
    .size(320, 240)
    .title("Rosetta Code - image noise")
    .open()
  surface = window.openGfx()

surface.loop:
  draw ctx, step:
    ctx.clear(gray(0))
    ctx.begin()
    for y in 0..window.height:
      for x in 0..window.width:
        if rand(0..1) == 0:
          ctx.point((x.float, y.float))
    ctx.draw(prPoints)
    echo 1 / (step / 60)
  update step:
    discard step
```


Optimized implementation using shaders:


```nim
import random

import rapid/gfx

var
  window = initRWindow()
    .size(320, 240)
    .title("Rosetta Code - image noise")
    .open()
  surface = window.openGfx()

let
  noiseShader = surface.newRProgram(RDefaultVshSrc, """
    uniform float time;

    float rand(vec2 pos) {
      return fract(sin(dot(pos.xy + time, vec2(12.9898,78.233))) * 43758.5453123);
    }

    vec4 rFragment(vec4 col, sampler2D tex, vec2 pos, vec2 uv) {
      return vec4(vec3(step(0.5, rand(uv))), 1.0);
    }
  """)

surface.vsync = false
surface.loop:
  draw ctx, step:
    noiseShader.uniform("time", time())
    ctx.program = noiseShader
    ctx.begin()
    ctx.rect(0, 0, surface.width, surface.height)
    ctx.draw()
    echo 1 / (step / 60)
  update step:
    discard step
```


On a Ryzen 5 1600 and a Vega 56 the naive implementation struggles to maintain 10 FPS. The GPU-powered version, however, reaches up to 12000-13000 FPS.


## OCaml

with the [[OCamlSDL|ocaml-sdl]] bindings:

```ocaml
let frames =
  { contents = 0 }

let t_acc =
  { contents = 0 }

let last_t =
  { contents = Sdltimer.get_ticks () }

let print_fps () =
  let t = Sdltimer.get_ticks () in
  let dt = t - !last_t in
  t_acc := !t_acc + dt;
  if !t_acc > 1000 then begin
    let el_time = !t_acc / 1000 in
    Printf.printf
      "- fps: %g\n%!"
      (float !frames /. float el_time);
    t_acc := 0;
    frames := 0;
  end;
  last_t := t

let blit_noise surf =
  let ba = Sdlvideo.pixel_data_8 surf in
  let dim = Bigarray.Array1.dim ba in
  while true do
    for i = 0 to pred dim do
      ba.{i} <- if Random.bool () then max_int else 0
    done;
    Sdlvideo.flip surf;
    incr frames;
    print_fps ()
  done

let blit_noise surf =
  try blit_noise surf
  with _ -> Sdl.quit ()

let () =
  Sdl.init [`VIDEO; `TIMER];
  Random.self_init();
  let surf =
    Sdlvideo.set_video_mode
      ~w:320 ~h:240 ~bpp:8
      [(*`HWSURFACE;*) `DOUBLEBUF]
  in
  Sys.catch_break true;
  blit_noise surf
```


compile to native-code with:
 ocamlopt bigarray.cmxa -I +sdl sdl.cmxa noise_fps_sdl.ml -o noise_fps_sdl.opt

or using findlib:
 ocamlfind opt -linkpkg -package sdl noise_fps_sdl.ml
 ./a.out

compile to bytecode with:
 ocamlc bigarray.cma -I +sdl sdl.cma noise_fps_sdl.ml -o noise_fps_sdl.byte

In script mode, run with:
 ocaml bigarray.cma -I +sdl sdl.cma noise_fps_sdl.ml

-----

In a more idiomatic way, using the modules Graphics and Unix from the standard OCaml library:

```ocaml
open Graphics

let white = (rgb 255 255 255)
let black = (rgb 0 0 0)

let t_last = ref (Unix.gettimeofday())

let () =
  open_graph "";
  let width = 320
  and height = 240 in
  resize_window width height;
  try
    while true do
      for y = 0 to pred height do
        for x = 0 to pred width do
          set_color (if Random.bool() then white else black);
          plot x y
        done;
      done;
      let t = Unix.gettimeofday() in
      Printf.printf "- fps: %f\n" (1.0 /. (t -. !t_last));
      t_last := t
    done
  with _ ->
    flush stdout;
    close_graph ()
```


run this script with:
 ocaml unix.cma graphics.cma g.ml

-----

And using an [[Image_Noise/OCaml/Xlib|OCaml-Xlib bindings]], or an [[Image_Noise/OCaml/Alleg|OCaml-Allegro binding]].

-----

[[Image_Noise/OCaml/OpenGL|Equivalent]] of the C-OpenGL method.


## Perl


```perl
use Gtk3 '-init';
use Glib qw/TRUE FALSE/;
use Time::HiRes qw/ tv_interval gettimeofday/;

my $time0 = [gettimeofday];
my $frames = -8; # account for set-up steps before drawing

my $window = Gtk3::Window->new();
$window->set_default_size(320, 240);
$window->set_border_width(0);
$window->set_title("Image_noise");
$window->set_app_paintable(TRUE);

my $da = Gtk3::DrawingArea->new();
$da->signal_connect('draw' => \&draw_in_drawingarea);
$window->add($da);
$window->show_all();
Glib::Timeout->add (1, \&update);

Gtk3->main;

sub draw_in_drawingarea {
  my ($widget, $cr, $data) = @_;
  $cr->set_line_width(1);
  for $x (1..320) {
    for $y (1..240) {
      int rand 2 ? $cr->set_source_rgb(0, 0, 0) : $cr->set_source_rgb(1, 1, 1);
      $cr->rectangle( $x, $y, 1, 1);
      $cr->stroke;
    }
  }
}

sub update {
    $da->queue_draw;
    my $elapsed = tv_interval( $time0, [gettimeofday] );
    $frames++;
    printf "fps: %.1f\n", $frames/$elapsed if $frames > 5;
    return TRUE;
}
```



## Perl 6

Variant of a script packaged with the SDL2::Raw module.


```perl6
use NativeCall;
use SDL2::Raw;

my int ($w, $h) = 320, 240;

SDL_Init(VIDEO);

my SDL_Window $window = SDL_CreateWindow(
    "White Noise - Perl 6",
    SDL_WINDOWPOS_CENTERED_MASK, SDL_WINDOWPOS_CENTERED_MASK,
    $w, $h,
    RESIZABLE
);

my SDL_Renderer $renderer = SDL_CreateRenderer( $window, -1, ACCELERATED +| TARGETTEXTURE );

my $noise_texture = SDL_CreateTexture($renderer, %PIXELFORMAT<RGB332>, STREAMING, $w, $h);

my $pixdatabuf = CArray[int64].new(0, $w, $h, $w);

sub render {
    my int $pitch;
    my int $cursor;

    # work-around to pass the pointer-pointer.
    my $pixdata = nativecast(Pointer[int64], $pixdatabuf);
    SDL_LockTexture($noise_texture, SDL_Rect, $pixdata, $pitch);

    $pixdata = nativecast(CArray[int8], Pointer.new($pixdatabuf[0]));

    loop (my int $row; $row < $h; $row = $row + 1) {
        loop (my int $col; $col < $w; $col = $col + 1) {
            $pixdata[$cursor + $col] = Bool.roll ?? 0xff !! 0x0;
        }
        $cursor = $cursor + $pitch;
    }

    SDL_UnlockTexture($noise_texture);

    SDL_RenderCopy($renderer, $noise_texture, SDL_Rect, SDL_Rect);
    SDL_RenderPresent($renderer);
}

my $event = SDL_Event.new;

main: loop {

    while SDL_PollEvent($event) {
        my $casted_event = SDL_CastEvent($event);

        given $casted_event {
            when *.type == QUIT {
                last main;
            }
        }
    }

    render();
    print fps;
}

say '';

sub fps {
    state $fps-frames = 0;
    state $fps-now    = now;
    state $fps        = '';
    $fps-frames++;
    if now - $fps-now >= 1 {
        $fps = [~] "\b" x 40, ' ' x 20, "\b" x 20 ,
            sprintf "FPS: %5.2f  ", ($fps-frames / (now - $fps-now)).round(.01);
        $fps-frames = 0;
        $fps-now = now;
    }
    $fps
}
```



## Pascal

It is SDL-internally limited to 200 fps.

```pascal
Program ImageNoise;

uses
  SDL;

var
  surface: PSDL_Surface;
  pixel: ^byte;
  frameNumber, totalTime, lastTime, time, i: longint;

begin
  frameNumber := 0;
  totalTime   := 0;
  lastTime    := 0;
  randomize;
  SDL_Init(SDL_INIT_TIMER or SDL_INIT_VIDEO);
  surface := SDL_SetVideoMode(320, 240, 8, SDL_DOUBLEBUF or SDL_HWSURFACE);

  while (true) do
  begin
    pixel := surface^.pixels;
    SDL_LockSurface(surface);
    for i := 1 to (surface^.w * surface^.h) do
    begin
      pixel^ := random(2)*255;
      inc(pixel);
    end;
    SDL_UnlockSurface(surface);
    SDL_Flip(surface);
    inc (frameNumber);

    time := SDL_GetTicks();
    totalTime := totalTime + time - lastTime;
    if (totalTime > 1000) then
    begin
      writeln('FPS: ', frameNumber / (totalTime / 1000.0):5:1);
      frameNumber := 0;
      totalTime := 0;
    end;
    lastTime := time;
  end;
end.
```



## Phix

Not optimised, gets about 130 fps.

```Phix
-- demo\rosetta\ImageNoise.exw
include pGUI.e

Ihandle dlg, canvas, timer
cdCanvas cddbuffer, cdcanvas

constant TITLE = "Image noise"

integer fps = 129   -- (typical value)

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    integer {w, h} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    sequence bw = repeat(0,w*h)
    for x=0 to w-1 do
        for y=0 to h-1 do
            if rand(2)=2 then bw[x*h+y+1] = 255 end if
        end for
    end for
    cdCanvasPutImageRectRGB(cddbuffer, w, h, {bw,bw,bw})
    cdCanvasFlush(cddbuffer)
    fps += 1
    return IUP_DEFAULT
end function

atom t1 = time()

function timer_cb(Ihandle /*ih*/)
    if time()>t1 then
        IupSetStrAttribute(dlg, "TITLE", "%s [%g FPS])",{TITLE,fps})
        fps = 0
        t1 = time()+1
    end if
    IupUpdate(canvas)
    return IUP_IGNORE
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    return IUP_DEFAULT
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "320x240")
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    timer = IupTimer(Icallback("timer_cb"), 10)

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", TITLE)
    IupCloseOnEscape(dlg)

    IupShow(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PicoLisp

This solution works on ErsatzLisp, the Java version of PicoLisp. It creates a 'JFrame' window, and calls inlined Java code to handle the image.

```PicoLisp
(javac "ImageNoise" "JPanel" NIL
      "java.util.*"
      "java.awt.*" "java.awt.image.*" "javax.swing.*" )

   int DX, DY;
   int[] Pixels;
   MemoryImageSource Source;
   Image Img;
   Random Rnd;

   public ImageNoise(int dx, int dy) {
      DX = dx;
      DY = dy;
      Pixels = new int[DX * DY];
      Source = new MemoryImageSource(DX, DY, Pixels, 0, DX);
      Source.setAnimated(true);
      Img = createImage(Source);
      Rnd = new Random();
   }

   public void paint(Graphics g) {update(g);}
   public void update(Graphics g) {g.drawImage(Img, 0, 0, this);}

   public void draw() {
      for (int i = 0; i < Pixels.length; ++i) {
         int c = Rnd.nextInt(255);
         Pixels[i] = 0xFF000000 | c<<16 | c<<8 | c;
      }
      Source.newPixels();
      paint(getGraphics());
   }
/**/

(de imageNoise (DX DY Fps)
   (let
      (Frame (java "javax.swing.JFrame" T "Image Noise")
         Noise (java "ImageNoise" T DX DY)
         Button (java "javax.swing.JButton" T "OK") )
      (java Frame "add" Noise)
      (java Frame "add" "South" Button)
      (java Button "addActionListener"
         (interface "java.awt.event.ActionListener"
            'actionPerformed '((Ev) (bye)) ) )
      (java Frame "setSize" DX DY)
      (java Frame "setVisible" T)
      (task (/ -1000 Fps) 0
         Image Noise
         (java Image "draw") ) ) )

# Start with 25 frames per second
(imageNoise 320 240 25)
```



## PL/I

<lang>Image_Noise: procedure options (main); /* 3 November 2013 */
   declare (start_time, end_time) float (18);
   declare (frame, m, n) fixed binary;

   start_time = secs();
   get (m, n);
   do frame = 1 to 100; /* Generate 100 frames. */
      call display (m, n);
      put skip data (frame);
   end;
   end_time = secs();

   put skip list ('Average FPS =' || fixed(100/(end_time-start_time), 6) );

display: procedure (m, n);
   declare (m, n) fixed binary;
   declare screen(0:m, 0:n) bit (1);
   declare i fixed binary;
   declare random builtin;

      screen = '0'b; /* clear screen for this frame */
      do i = 0 to m*n*random;
         screen(random*m, random*n) = random > 0.5;
      end;
end display;

end Image_Noise;
```




## Processing


```processing
color black = color(0);
color white = color(255);

void setup(){
  size(320,240);
  // frameRate(300); // 60 by default
}

void draw(){
  loadPixels();
  for(int i=0; i<pixels.length; i++){
    if(random(1)<0.5){
      pixels[i] = black;
    }else{
      pixels[i] = white;
    }
  }
  updatePixels();
  fill(0,128);
  rect(0,0,60,20);
  fill(255);
  text(frameRate, 5,15);
}
```




## PureBasic


```PureBasic
#filter=0.2       ; Filter parameter for the FPS-calculation
#UpdateFreq=100   ; How often to update the FPS-display

OpenWindow(0,400,300,320,240,"PureBasic")
Define w=WindowWidth(0), h=WindowHeight(0)
Define x, y, T, TOld, FloatingMedium.f, cnt
InitSprite()
OpenWindowedScreen(WindowID(0),0,0,w,h,1,0,0,#PB_Screen_NoSynchronization)
Repeat
  StartDrawing(ScreenOutput())
  For y=0 To h-1
    For x=0 To w-1
      If Random(1)
        Plot(x,y,#Black)
      Else
        Plot(x,y,#White)
      EndIf
    Next
  Next
  StopDrawing()
  FlipBuffers()
  cnt+1
  If cnt>=#UpdateFreq
    cnt =0
    TOld=T
    T   =ElapsedMilliseconds()
    FloatingMedium*(1-#filter)+1000*#filter/(T-TOld)
    SetWindowTitle(0,"PureBasic: "+StrF(#UpdateFreq*FloatingMedium,2)+" FPS")
    Repeat ; Handle all events
      Event=WindowEvent()
      If Event=#PB_Event_CloseWindow
        End
      EndIf
    Until Not Event
  EndIf
ForEver
```

[[Image:Image_Noise_in_PureBasic.png‎]]


## Python

```python
import time
import random
import Tkinter
import Image, ImageTk # PIL libray

class App(object):
    def __init__(self, size, root):
        self.root = root
        self.root.title("Image Noise Test")

        self.img = Image.new("RGB", size)
        self.label = Tkinter.Label(root)
        self.label.pack()

        self.time = 0.0
        self.frames = 0
        self.size = size
        self.loop()

    def loop(self):
        self.ta = time.time()
        # 13 FPS boost. half integer idea from C#.
        rnd = random.random
        white = (255, 255, 255)
        black = (0, 0, 0)
        npixels = self.size[0] * self.size[1]
        data = [white if rnd() > 0.5 else black for i in xrange(npixels)]
        self.img.putdata(data)
        self.pimg = ImageTk.PhotoImage(self.img)
        self.label["image"] = self.pimg
        self.tb = time.time()

        self.time += (self.tb - self.ta)
        self.frames += 1

        if self.frames == 30:
            try:
                self.fps = self.frames / self.time
            except:
                self.fps = "INSTANT"
            print ("%d frames in %3.2f seconds (%s FPS)" %
                  (self.frames, self.time, self.fps))
            self.time = 0
            self.frames = 0

        self.root.after(1, self.loop)

def main():
    root = Tkinter.Tk()
    app = App((320, 240), root)
    root.mainloop()

main()
```

About 28 FPS max, Python 2.6.6.


## Racket


```racket

#lang racket
(require 2htdp/image 2htdp/universe)

(define black (color   0   0   0 255))
(define white (color 255 255 255 255))

(define-struct world (last fps))

(define (noise w h)
  (color-list->bitmap
   (for*/list ([x (in-range w)] [y (in-range h)])
     (if (zero? (random 2)) black white))
   w h))

(define (draw w)
  (underlay/xy
   (noise 320 240) 0 0
   (text (number->string (world-fps w)) 64 "Red")))

(define (handle-tick w)
  (define cm (current-inexact-milliseconds))
  (make-world cm (exact-floor (/ 1000.0 (- cm (world-last w))))))

(big-bang (make-world 1 0)
          [on-draw draw]
          [on-tick handle-tick (/ 1. 120)])

```



## REXX


```rexx
/*REXX program times (elapsed) the generation of 100 frames of random black&white image.*/
parse arg sw sd im .                             /*obtain optional args from the C.L.   */
if sw=='' | sw==","  then sw=320                 /*SW  specified?  No, then use default.*/
if sd=='' | sd==","  then sd=240                 /*SD      "        "    "   "      "   */
if im=='' | im==","  then im=100                 /*IM      "        "    "   "      "   */
call time 'R'                                    /*reset the REXX elapsed (clock) timer.*/
              do   im                            /*generate    IM     number of images. */
              call genFrame  sw, sd              /*generate single image of size SW x SD*/
              /*■■■ display frame here ■■■*/     /*do (or don't) display the frame num. */
              end   /*im*/                       /*generate, but don't display the image*/
                                                 /*measures  ↓  elapsed time in seconds.*/
say 'The average frames/second: '    format(im/time("E"), , 2)     /*show frames/second.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
genFrame:  parse arg x,y;   @.0= 'ff000000'x                   /*hex: the color  black. */
                            @.1= 'ffFFffFF'x                   /* "    "    "    white. */
           $=                                                  /*nullify image string.  */
                  do y;  _=                                    /*nullify an output row. */
                            do x;  ?=random(0,1);  _=_ || @.?  /*color is black │ white.*/
                            end   /*x*/                        /* [↑]  build a whole row*/
                  $=$ || _                                     /*append row to $ string.*/
                  end             /*y*/                        /* [↑]  build the image. */
           return
```



## Ruby

```ruby
require 'rubygems'
require 'gl'
require 'glut'

W, H = 320, 240
SIZE = W * H

Glut.glutInit ARGV
Glut.glutInitWindowSize W, H

Glut.glutIdleFunc lambda {
  i = Time.now
  noise = (1..SIZE).map { rand > 0.5 ? 0xFFFFFFFF : 0xFF000000 }.pack("I*")

  Gl.glClear Gl::GL_COLOR_BUFFER_BIT
  Gl.glDrawPixels W, H, Gl::GL_RGBA, Gl::GL_UNSIGNED_BYTE, noise
  Gl.glFlush

  puts 1.0 / (Time.now - i)
}

Glut.glutCreateWindow "noise"
Glut.glutMainLoop
```



## Run BASIC


```runbasic
begSec = time$("seconds")
graphic #g, 320,240
tics  = 320 * 240
for i = 1 to tics
    x = int((rnd(1) * 320) + 1)
    y = int((rnd(1) * 240) + 1)
    if int(x mod 2) then  #g "color black ; set "; x; " "; y else #g "color white ; set "; x; " "; y
next i
endSec = time$("seconds")
totSec = endSec - begSec
print "Seconds;";totSec;" Count:";tics;" Tics / sec:";tics/totSec;" fps:";1/totSec
render #g
#g "flush"
```



## Scala

This is basically the same as the Java version, except without using BufferedImage.

```scala

import java.awt.event.{ActionEvent, ActionListener}
import swing.{Panel, MainFrame, SimpleSwingApplication}
import javax.swing.Timer
import java.awt.{Font, Color, Graphics2D, Dimension}

object ImageNoise extends SimpleSwingApplication {
  var delay_ms = 2
  var framecount = 0
  var fps = 0

  def top = new MainFrame {
    contents = panel
  }

  val panel = new Panel {
    preferredSize = new Dimension(320, 240)

    override def paintComponent(g: Graphics2D) {
      for (x <- 0 to size.width; y <- 0 to size.height) {
        val c = if (math.random > 0.5) Color.BLACK else Color.WHITE
        g.setColor(c)
        g.fillRect(x, y, 1, 1)
      }
      g.setColor(Color.RED)
      g.setFont(new Font("Monospaced", Font.BOLD, 20))
      g.drawString("FPS: " + fps, size.width - 100, size.height - 10)
      framecount += 1
    }
  }

  val repainter = new Timer(delay_ms, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      panel.repaint
    }
  })

  val framerateChecker = new Timer(1000, new ActionListener {
    def actionPerformed(e: ActionEvent) {
      fps = framecount
      framecount = 0
    }
  })

  repainter.start()
  framerateChecker.start()
}
```


## Tcl

```tcl
package require Tk

proc generate {img width height} {
    set data {}
    for {set i 0} {$i<$height} {incr i} {
	set line {}
	for {set j 0} {$j<$width} {incr j} {
	    lappend line [lindex "#000000 #FFFFFF" [expr {rand() < 0.5}]]
	}
	lappend data $line
    }
    $img put $data
}

set time 0.0
set count 0

proc looper {} {
    global time count
    set t [lindex [time {generate noise 320 240}] 0]
    set time [expr {$time + $t}]
    if {[incr count] >= 30} {
	set time [expr {$time / 1000000.0}]
	set fps [expr {$count / $time}]
	puts [format "%d frames in %3.2f seconds (%f FPS)" $count $time $fps]
	set time 0.0
	set count 0
    }
    after 1 looper
}

image create photo noise -width 320 -height 240
pack [label .l -image noise]
update
looper
```


## Visual Basic .NET

Windows Forms Application.


```vbnet
Imports System.Drawing.Imaging

Public Class frmSnowExercise
    Dim bRunning As Boolean = True

    Private Sub Form1_Load(ByVal sender As System.Object,
                           ByVal e As System.EventArgs) Handles MyBase.Load

        ' Tell windows we want to handle all the painting and that we want it
        '  to double buffer the form's rectangle (Double Buffering
        '  removes/ reduces flickering).
        SetStyle(ControlStyles.AllPaintingInWmPaint Or ControlStyles.UserPaint _
            Or ControlStyles.OptimizedDoubleBuffer, True)
        UpdateStyles()

        ' Prevent the user from resizing the window. Our draw code is not
        ' setup to recalculate on the fly.
        FormBorderStyle = Windows.Forms.FormBorderStyle.FixedSingle
        MaximizeBox = False

        ' The window size and the client rectangle aren't the same.
        ' To get the proper dimensions for our exercise we need to
        ' figure out the difference and add it to our 320x240
        ' requirement.
        Width = 320 + Size.Width - ClientSize.Width
        Height = 240 + Size.Height - ClientSize.Height

        ' Pop the window, bring it to the front and give windows time to
        ' reflect the changes.
        Show()
        Activate()
        Application.DoEvents()

        ' Hit the loop and keep going until we receive a close request.
        RenderLoop()

        ' We're done. Exit the application.
        Close()

    End Sub

    Private Sub Form1_KeyPress(ByVal sender As Object, ByVal e As _
            System.Windows.Forms.KeyPressEventArgs) Handles Me.KeyPress
        ' Close the application when the user hits escape.
        If e.KeyChar = ChrW(Keys.Escape) Then bRunning = False
    End Sub

    Private Sub Form1_FormClosing(ByVal sender As Object, ByVal e As _
            System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        ' We'll cancel the form close request if we're still running so we
        ' don't get an error during runtime and set the close request flag.
        e.Cancel = bRunning
        bRunning = False
    End Sub

    Private Sub RenderLoop()

        Const cfPadding As Single = 5.0F

        Dim b As New Bitmap(ClientSize.Width, ClientSize.Width,
                            PixelFormat.Format32bppArgb)
        Dim g As Graphics = Graphics.FromImage(b)
        Dim r As New Random(Now.Millisecond)
        Dim oBMPData As BitmapData = Nothing
        Dim oPixels() As Integer = Nothing
        Dim oBlackWhite() As Integer = {Color.White.ToArgb, Color.Black.ToArgb}
        Dim oStopwatch As New Stopwatch
        Dim fElapsed As Single = 0.0F
        Dim iLoops As Integer = 0
        Dim sFPS As String = "0.0 FPS"
        Dim oFPSSize As SizeF = g.MeasureString(sFPS, Font)
        Dim oFPSBG As RectangleF = New RectangleF(ClientSize.Width - cfPadding -
                      oFPSSize.Width, cfPadding, oFPSSize.Width, oFPSSize.Height)

        ' Get ourselves a nice, clean, black canvas to work with.
        g.Clear(Color.Black)

        ' Prep our bitmap for a read.
        oBMPData = b.LockBits(New Rectangle(0, 0, b.Width, b.Height),
                          ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)

        ' Allocate sufficient space for the pixel data and
        ' flash copy it to our array.
        ' We want an integer to hold the color for each pixel in the canvas.
        Array.Resize(oPixels, b.Width * b.Height)
        Runtime.InteropServices.Marshal.Copy(oBMPData.Scan0,
                                             oPixels, 0, oPixels.Length)
        b.UnlockBits(oBMPData)
        ' Start looping.
        Do
            ' Find our frame time and add it to the total amount of time
            ' elapsed since our last FPS update (once per second).
            fElapsed += oStopwatch.ElapsedMilliseconds / 1000.0F
            oStopwatch.Reset() : oStopwatch.Start()
            ' Adjust the number of loops since the last whole second has elapsed
            iLoops += 1
            If fElapsed >= 1.0F Then
                ' Since we've now had a whole second elapse
                ' figure the Frames Per Second,
                ' measure our string,
                ' setup our backing rectangle for the FPS string
                '        (so it's clearly visible over the snow)
                ' reset our loop counter
                ' and our elapsed counter.
                sFPS = (iLoops / fElapsed).ToString("0.0") & " FPS"
                oFPSSize = g.MeasureString(sFPS, Font)
                oFPSBG = New RectangleF(ClientSize.Width - cfPadding -
                    oFPSSize.Width, cfPadding, oFPSSize.Width, oFPSSize.Height)
                ' We don't set this to 0 in case our frame time has gone
                '  a bit over 1 second since last update.
                fElapsed -= 1.0F
                iLoops = 0
            End If

            ' Generate our snow.
            For i As Integer = 0 To oPixels.GetUpperBound(0)
                oPixels(i) = oBlackWhite(r.Next(oBlackWhite.Length))
            Next

            ' Prep the bitmap for an update.
            oBMPData = b.LockBits(New Rectangle(0, 0, b.Width, b.Height),
                       ImageLockMode.WriteOnly, PixelFormat.Format32bppArgb)
            ' Flash copy the new data into our bitmap.
            Runtime.InteropServices.Marshal.Copy(oPixels, 0, oBMPData.Scan0,
                                                 oPixels.Length)
            b.UnlockBits(oBMPData)

            ' Draw the backing for our FPS display.
            g.FillRectangle(Brushes.Black, oFPSBG)
            ' Draw our FPS.
            g.DrawString(sFPS, Font, Brushes.Yellow, oFPSBG.Left, oFPSBG.Top)

            ' Update the form's background and draw.
            BackgroundImage = b
            Invalidate(ClientRectangle)

            ' Let windows handle some queued events.
            Application.DoEvents()
        Loop While bRunning

    End Sub
End Class

```

Sample:<BR>
[[Image:SHSnowExercise.jpg]]


## XPL0

Measured 102 FPS on a Presario 2100 Notebook Athlon 2500. (The Paint
intrinsic could display this image at 327 FPS if Ran(2) was faster.)


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
int CpuReg,                     \address of CPU register array (from GetReg)
    FPS,                        \frames per second, the display's update rate
    Sec,                        \current second of time (from real-time clock)
    SecOld,                     \previous second of time
    X, Y;
[SetVid($101);                  \set 640x480 graphics
CpuReg:= GetReg;                \get address of array to access CPU registers
FPS:= 0;
repeat  CpuReg(0):= $0200;      \get current time in seconds from BIOS
        SoftInt($1A);           \software interrupt
        Sec:= CpuReg(3)>>8 & $FF; \register DH contains seconds
        if Sec = SecOld then    \if same as before then
                FPS:= FPS+1     \ bump FPS counter
        else    [SecOld:= Sec;  \otherwise save old seconds and
                CrLf(6);
                IntOut(6, FPS); \ display FPS counter (once per second)
                Text(6, " FPS");
                FPS:= 0;        \ reset FPS counter
                ];
        for Y:= 0, 240-1 do     \fill image with random black and white pixels
            for X:= 0, 320-1 do
                Point(X, Y, if Ran(2) then $F\white\ else 0\black\);
until KeyHit;
SetVid(3);                      \restore normal text mode
]
```

