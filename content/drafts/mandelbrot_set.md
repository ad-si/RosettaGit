+++
title = "Mandelbrot set"
description = ""
date = 2019-10-14T01:05:49Z
aliases = []
[extra]
id = 4039
[taxonomies]
categories = []
tags = []
+++

{{wikipedia|Mandelbrot_set}}
{{task|Fractals}} [[Category:Graphics]] [[Category:Raster graphics operations]] 


;Task:
Generate and draw the [[wp:Mandelbrot set|Mandelbrot set]]. 


Note that there are [http://en.wikibooks.org/wiki/Fractals/Iterations_in_the_complex_plane/Mandelbrot_set many algorithms] to draw Mandelbrot set and there are [http://en.wikibooks.org/wiki/Pictures_of_Julia_and_Mandelbrot_sets many functions] which generate it .





## ACL2


```Lisp
(defun abs-sq (z)
   (+ (expt (realpart z) 2)
      (expt (imagpart z) 2)))

(defun round-decimal (x places)
   (/ (floor (* x (expt 10 places)) 1)
      (expt 10 places)))

(defun round-complex (z places)
   (complex (round-decimal (realpart z) places)
            (round-decimal (imagpart z) places)))

(defun mandel-point-r (z c limit)
   (declare (xargs :measure (nfix limit)))
   (cond ((zp limit) 0)
         ((> (abs-sq z) 4) limit)
         (t (mandel-point-r (+ (round-complex (* z z) 15) c)
                            c
                            (1- limit)))))

(defun mandel-point (z iters)
   (- 5 (floor (mandel-point-r z z iters) (/ iters 5))))

(defun draw-mandel-row (im re cols width iters)
   (declare (xargs :measure (nfix cols)))
   (if (zp cols)
       nil
       (prog2$ (cw (coerce
                    (list
                     (case (mandel-point (complex re im)
                                         iters)
                           (5 #\#)
                           (4 #\*)
                           (3 #\.)
                           (2 #\.)
                           (otherwise #\Space))) 'string))
               (draw-mandel-row im
                                (+ re (/ (/ width 3)))
                                (1- cols)
                                width iters))))

(defun draw-mandel (im rows width height iters)
   (if (zp rows)
       nil
       (progn$ (draw-mandel-row im -2 width width iters)
               (cw "~%")
               (draw-mandel (- im (/ (/ height 2)))
                            (1- rows)
                            width
                            height
                            iters))))

(defun draw-mandelbrot (width iters)
   (let ((height (floor (* 1000 width) 3333)))
        (draw-mandel 1 height width height iters)))
```


{{out}}

```txt
&gt; (draw-mandelbrot 60 100)
                                        #                   
                                     ..                     
                                   .####                    
                            .     # .##.                    
                             ##*###############.            
                           #.##################             
                          .######################.          
                 ######.  #######################           
               ##########.######################            
##############################################              
               ##########.######################            
                 ######.  #######################           
                          .######################.          
                           #.##################             
                             ##*###############.            
                            .     # .##.                    
                                   .####                    
                                     ..                     
```



## Ada

{{libheader|Lumen}}
mandelbrot.adb:

```Ada
with Lumen.Binary;
package body Mandelbrot is
   function Create_Image (Width, Height : Natural) return Lumen.Image.Descriptor is
      use type Lumen.Binary.Byte;
      Result : Lumen.Image.Descriptor;
      X0, Y0 : Float;
      X, Y, Xtemp : Float;
      Iteration   : Float;
      Max_Iteration : constant Float := 1000.0;
      Color : Lumen.Binary.Byte;
   begin
      Result.Width := Width;
      Result.Height := Height;
      Result.Complete := True;
      Result.Values := new Lumen.Image.Pixel_Matrix (1 .. Width, 1 .. Height);
      for Screen_X in 1 .. Width loop
         for Screen_Y in 1 .. Height loop
            X0 := -2.5 + (3.5 / Float (Width) * Float (Screen_X));
            Y0 := -1.0 + (2.0 / Float (Height) * Float (Screen_Y));
            X := 0.0;
            Y := 0.0;
            Iteration := 0.0;
            while X * X + Y * Y <= 4.0 and then Iteration < Max_Iteration loop
               Xtemp := X * X - Y * Y + X0;
               Y := 2.0 * X * Y + Y0;
               X := Xtemp;
               Iteration := Iteration + 1.0;
            end loop;
            if Iteration = Max_Iteration then
               Color := 255;
            else
               Color := 0;
            end if;
            Result.Values (Screen_X, Screen_Y) := (R => Color, G => Color, B => Color, A => 0);
         end loop;
      end loop;
      return Result;
   end Create_Image;

end Mandelbrot;
```


mandelbrot.ads:

```Ada
with Lumen.Image;

package Mandelbrot is

   function Create_Image (Width, Height : Natural) return Lumen.Image.Descriptor;

end Mandelbrot;
```


test_mandelbrot.adb:

```Ada
with System.Address_To_Access_Conversions;
with Lumen.Window;
with Lumen.Image;
with Lumen.Events;
with GL;
with Mandelbrot;

procedure Test_Mandelbrot is

   Program_End : exception;

   Win : Lumen.Window.Handle;
   Image : Lumen.Image.Descriptor;
   Tx_Name : aliased GL.GLuint;
   Wide, High : Natural := 400;

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
--        Image := Mandelbrot.Create_Image (Width => Wide, Height => High);
--        Create_Texture;
      Draw;
   end Resize_Handler;

begin
   -- Create Lumen window, accepting most defaults; turn double buffering off
   -- for simplicity
   Lumen.Window.Create (Win           => Win,
                        Name          => "Mandelbrot fractal",
                        Width         => Wide,
                        Height        => High,
                        Events        => (Lumen.Window.Want_Exposure  => True,
                                          Lumen.Window.Want_Key_Press => True,
                                          others                      => False));

   -- Set up the viewport and scene parameters
   Set_View (Wide, High);

   -- Now create the texture and set up to use it
   Image := Mandelbrot.Create_Image (Width => Wide, Height => High);
   Create_Texture;

   -- Enter the event loop
   declare
      use Lumen.Events;
   begin
      Select_Events (Win   => Win,
                     Calls => (Key_Press    => Quit_Handler'Unrestricted_Access,
                               Exposed      => Expose_Handler'Unrestricted_Access,
                               Resized      => Resize_Handler'Unrestricted_Access,
                               Close_Window => Quit_Handler'Unrestricted_Access,
                               others       => No_Callback));
   end;
exception
   when Program_End =>
      null;
end Test_Mandelbrot;
```


{{out}}
[[File:Ada_Mandelbrot.gif]]


## ALGOL 68

{{works with|Algol 68 Genie 1.19.0}}

Plot part of the Mandelbrot set as a pseudo-gif image.


```algol68
 
INT pix = 300, max iter = 256, REAL zoom = 0.33 / pix;
[-pix : pix, -pix : pix] INT plane;
COMPL ctr = 0.05 I 0.75 # center of set #;

# Compute the length of an orbit. #
PROC iterate = (COMPL z0) INT:
  BEGIN COMPL z := 0, INT iter := 1;
        WHILE (iter +:= 1) < max iter # not converged # AND ABS z < 2 # not diverged #
        DO z := z * z + z0
        OD;
        iter
  END;

# Compute set and find maximum orbit length. #     
INT max col := 0;
FOR x FROM -pix TO pix
DO FOR y FROM -pix TO pix
   DO COMPL z0 = ctr + (x * zoom) I (y * zoom);
      IF (plane [x, y] := iterate (z0)) < max iter
      THEN (plane [x, y] > max col | max col := plane [x, y])
      FI
   OD
OD;

# Make a plot. #
FILE plot;
INT num pix = 2 * pix + 1;
make device (plot, "gif", whole (num pix, 0) + "x" + whole (num pix, 0));
open (plot, "mandelbrot.gif", stand draw channel);
FOR x FROM -pix TO pix
DO FOR y FROM -pix TO pix
   DO INT col = (plane [x, y] > max col | max col | plane [x, y]);
      REAL c = sqrt (1- col / max col); # sqrt to enhance contrast #
      draw colour (plot, c, c, c);
      draw point (plot, (x + pix) / (num pix - 1), (y + pix) / (num pix  - 1))
   OD
OD;
close (plot)

```



## ALGOL W

Generates an ASCII Mandelbrot Set. Translated from the sample program in the Compiler/AST Interpreter task.

```algolw
begin
    % This is an integer ascii Mandelbrot generator, translated from the   %
    % Compiler/AST Interpreter Task's ASCII Mandelbrot Set example program %
    integer leftEdge, rightEdge, topEdge, bottomEdge, xStep, yStep, maxIter;
    leftEdge   := -420;
    rightEdge  :=  300;
    topEdge    :=  300;
    bottomEdge := -300;
    xStep      :=    7;
    yStep      :=   15;
 
    maxIter    :=  200;
 
    for y0 := topEdge step - yStep until bottomEdge do begin
        for x0 := leftEdge step xStep until rightEdge do begin
            integer x, y, i;
            string(1) theChar;
            y := 0;
            x := 0;
            theChar := " ";
            i := 0;
            while i < maxIter do begin
                integer x_x, y_y;
                x_x := (x * x) div 200;
                y_y := (y * y) div 200;
                if x_x + y_y > 800 then begin
                    theChar := code( decode( "0" ) + i );
                    if i > 9 then theChar := "@";
                    i := maxIter
                end;
                y := x * y div 100 + y0;
                x := x_x - y_y + x0;
                i := i + 1
            end while_i_lt_maxIter ;
            writeon( theChar );
        end for_x0 ;
        write();
    end for_y0
end.

```

{{out}}

```txt

1111111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222211111
1111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222222222211
1111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222
1111111111111111222222222222222222233333333333333333333333222222222222222222222222222222222222222222222
1111111111111112222222222222333333333333333333333333333333333333222222222222222222222222222222222222222
1111111111111222222222233333333333333333333333344444456655544443333332222222222222222222222222222222222
1111111111112222222233333333333333333333333444444445567@@6665444444333333222222222222222222222222222222
11111111111222222333333333333333333333334444444445555679@@@@7654444443333333222222222222222222222222222
1111111112222223333333333333333333333444444444455556789@@@@98755544444433333332222222222222222222222222
1111111122223333333333333333333333344444444445556668@@@    @@@76555544444333333322222222222222222222222
1111111222233333333333333333333344444444455566667778@@      @987666555544433333333222222222222222222222
111111122333333333333333333333444444455556@@@@@99@@@@@@    @@@@@@877779@5443333333322222222222222222222
1111112233333333333333333334444455555556679@   @@@               @@@@@@ 8544333333333222222222222222222
1111122333333333333333334445555555556666789@@@                        @86554433333333322222222222222222
1111123333333333333444456666555556666778@@ @                         @@87655443333333332222222222222222
111123333333344444455568@887789@8777788@@@                            @@@@65444333333332222222222222222
111133334444444455555668@@@@@@@@@@@@99@@@                              @@765444333333333222222222222222
111133444444445555556778@@@         @@@@                                @855444333333333222222222222222
11124444444455555668@99@@             @                                 @655444433333333322222222222222
11134555556666677789@@                                                @86655444433333333322222222222222
111                                                                 @@876555444433333333322222222222222
11134555556666677789@@                                                @86655444433333333322222222222222
11124444444455555668@99@@             @                                 @655444433333333322222222222222
111133444444445555556778@@@         @@@@                                @855444333333333222222222222222
111133334444444455555668@@@@@@@@@@@@99@@@                              @@765444333333333222222222222222
111123333333344444455568@887789@8777788@@@                            @@@@65444333333332222222222222222
1111123333333333333444456666555556666778@@ @                         @@87655443333333332222222222222222
1111122333333333333333334445555555556666789@@@                        @86554433333333322222222222222222
1111112233333333333333333334444455555556679@   @@@               @@@@@@ 8544333333333222222222222222222
111111122333333333333333333333444444455556@@@@@99@@@@@@    @@@@@@877779@5443333333322222222222222222222
1111111222233333333333333333333344444444455566667778@@      @987666555544433333333222222222222222222222
1111111122223333333333333333333333344444444445556668@@@    @@@76555544444333333322222222222222222222222
1111111112222223333333333333333333333444444444455556789@@@@98755544444433333332222222222222222222222222
11111111111222222333333333333333333333334444444445555679@@@@7654444443333333222222222222222222222222222
1111111111112222222233333333333333333333333444444445567@@6665444444333333222222222222222222222222222222
1111111111111222222222233333333333333333333333344444456655544443333332222222222222222222222222222222222
1111111111111112222222222222333333333333333333333333333333333333222222222222222222222222222222222222222
1111111111111111222222222222222222233333333333333333333333222222222222222222222222222222222222222222222
1111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222
1111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222222222211
1111111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222211111

```



## Applesoft BASIC


This version takes into account the Apple II's funky 280&times;192 6-color display, which has an effective resolution of only 140&times;192 in color.


```basic

10  HGR2
20  XC = -0.5           : REM CENTER COORD X
30  YC = 0              : REM   "      "   Y
40  S = 2               : REM SCALE
45  IT = 20             : REM ITERATIONS
50  XR = S * (280 / 192): REM TOTAL RANGE OF X
60  YR = S              : REM   "     "   "  Y
70  X0 = XC - (XR/2)    : REM MIN VALUE OF X
80  X1 = XC + (XR/2)    : REM MAX   "   "  X
90  Y0 = YC - (YR/2)    : REM MIN   "   "  Y
100 Y1 = YC + (YR/2)    : REM MAX   "   "  Y
110 XM = XR / 279       : REM SCALING FACTOR FOR X
120 YM = YR / 191       : REM    "      "     "  Y
130 FOR YI = 0 TO 3     : REM INTERLEAVE
140   FOR YS = 0+YI TO 188+YI STEP 4 : REM Y SCREEN COORDINATE
145   HCOLOR=3 : HPLOT 0,YS TO 279,YS
150     FOR XS = 0 TO 278 STEP 2     : REM X SCREEN COORDINATE
170       X = XS * XM + X0  : REM TRANSL SCREEN TO TRUE X
180       Y = YS * YM + Y0  : REM TRANSL SCREEN TO TRUE Y
190       ZX = 0
200       ZY = 0
210       XX = 0
220       YY = 0
230       FOR I = 0 TO IT
240         ZY = 2 * ZX * ZY + Y
250         ZX = XX - YY + X
260         XX = ZX * ZX
270         YY = ZY * ZY
280         C = IT-I
290         IF XX+YY >= 4 GOTO 301
300       NEXT I
301       IF C >= 8 THEN C = C - 8 : GOTO 301
310       HCOLOR = C : HPLOT XS, YS TO XS+1, YS
320     NEXT XS
330   NEXT YS
340 NEXT YI

```


By making the following modifications, the same code will render the Mandelbrot set in monochrome at full 280&times;192 resolution.


```basic

150 FOR XS = 0 TO 279
301 C = (C - INT(C/2)*2)*3
310 HCOLOR = C: HPLOT XS, YS

```



## AutoHotkey


```autohotkey
Max_Iteration := 256
Width := Height := 400

File := "MandelBrot." Width ".bmp"
Progress, b2 w400 fs9, Creating Colours ...
Gosub, CreateColours
Gosub, CreateBitmap
Progress, Off
Gui, -Caption
Gui, Margin, 0, 0
Gui, Add, Picture,, %File%
Gui, Show,, MandelBrot
Return

GuiClose:
GuiEscape:
ExitApp



;---------------------------------------------------------------------------
CreateBitmap: ; create and save a 32bit bitmap file
;---------------------------------------------------------------------------
    ; define header details
    HeaderBMP  := 14
    HeaderDIB  := 40
    DataOffset := HeaderBMP + HeaderDIB
    ImageSize  := Width * Height * 4 ; 32bit
    FileSize   := DataOffset + ImageSize
    Resolution := 3780 ; from mspaint

    ; create bitmap header
    VarSetCapacity(IMAGE, FileSize, 0)
    NumPut(Asc("B")   , IMAGE, 0x00, "Char")
    NumPut(Asc("M")   , IMAGE, 0x01, "Char")
    NumPut(FileSize   , IMAGE, 0x02, "UInt")
    NumPut(DataOffset , IMAGE, 0x0A, "UInt")
    NumPut(HeaderDIB  , IMAGE, 0x0E, "UInt")
    NumPut(Width      , IMAGE, 0x12, "UInt")
    NumPut(Height     , IMAGE, 0x16, "UInt")
    NumPut(1          , IMAGE, 0x1A, "Short") ; Planes
    NumPut(32         , IMAGE, 0x1C, "Short") ; Bits per Pixel
    NumPut(ImageSize  , IMAGE, 0x22, "UInt")
    NumPut(Resolution , IMAGE, 0x26, "UInt")
    NumPut(Resolution , IMAGE, 0x2A, "UInt")

    ; fill in Data
    Gosub, CreatePixels

    ; save Bitmap to file
    FileDelete, %File%
    Handle := DllCall("CreateFile", "Str", File, "UInt", 0x40000000
            , "UInt", 0, "UInt", 0, "UInt", 2, "UInt", 0, "UInt", 0)
    DllCall("WriteFile", "UInt", Handle, "UInt", &IMAGE, "UInt"
            , FileSize, "UInt *", Bytes, "UInt", 0)
    DllCall("CloseHandle", "UInt", Handle)

Return



;---------------------------------------------------------------------------
CreatePixels: ; create pixels for [-2 < x < 1] [-1.5 < y < 1.5]
;---------------------------------------------------------------------------
    Loop, % Height // 2 + 1 {
        yi := A_Index - 1
        y0 := -1.5 + yi / Height * 3 ; range -1.5 .. +1.5
        Progress, % 200*yi // Height, % "Current line: " 2*yi " / " Height
        Loop, %Width% {
            xi := A_Index - 1
            x0 := -2 + xi / Width * 3 ; range -2 .. +1
            Gosub, Mandelbrot
            p1 := DataOffset + 4 * (Width * yi + xi)
            NumPut(Colour, IMAGE, p1, "UInt")
            p2 := DataOffset + 4 * (Width * (Height-yi) + xi)
            NumPut(Colour, IMAGE, p2, "UInt")
        }
    }
Return



;---------------------------------------------------------------------------
Mandelbrot: ; calculate a colour for each pixel
;---------------------------------------------------------------------------
    x := y := Iteration := 0
    While, (x*x + y*y <= 4) And (Iteration < Max_Iteration) {
        xtemp := x*x - y*y + x0
        y := 2*x*y + y0
        x := xtemp
        Iteration++
    }
    Colour := Iteration = Max_Iteration ? 0 : Colour_%Iteration%

Return



;---------------------------------------------------------------------------
CreateColours: ; borrowed from PureBasic example
;---------------------------------------------------------------------------
    Loop, 64 {
        i4 := (i3 := (i2 := (i1 := A_Index - 1) + 64) + 64) + 64
        Colour_%i1% := RGB(4*i1 + 128, 4*i1, 0)
        Colour_%i2% := RGB(64, 255, 4*i1)
        Colour_%i3% := RGB(64, 255 - 4*i1, 255)
        Colour_%i4% := RGB(64, 0, 255 - 4*i1)
    }
Return



;---------------------------------------------------------------------------
RGB(r, g, b) { ; return 24bit color value
;---------------------------------------------------------------------------
    Return, (r&0xFF)<<16 | g<<8 | b
}
```



## AWK


```AWK
BEGIN {
  XSize=59; YSize=21;
  MinIm=-1.0; MaxIm=1.0;MinRe=-2.0; MaxRe=1.0;
  StepX=(MaxRe-MinRe)/XSize; StepY=(MaxIm-MinIm)/YSize;
  for(y=0;y<YSize;y++)
  {
    Im=MinIm+StepY*y;
    for(x=0;x<XSize;x++)
        {
      Re=MinRe+StepX*x; Zr=Re; Zi=Im;
      for(n=0;n<30;n++)
          {
        a=Zr*Zr; b=Zi*Zi;
        if(a+b>4.0) break;
        Zi=2*Zr*Zi+Im; Zr=a-b+Re;
      }
      printf "%c",62-n;
    }
    print "";
  }
  exit;
}
```

{{out}}

```txt

>>>>>>
### ==<<<<<<<<<<<<<<<;;;;;;:::96032:;;;;<<<<=======

>>>>>===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### =

>>>>===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<======
>>>==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<====
>>==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<===
>>=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<==
>=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<=
><<<<;;;;;:::972456-567763                      +9;;<<<<<<<
><;;;;;;::::9875&      .3                       *9;;;<<<<<<
>;;;;;;::997564'        '                       8:;;;<<<<<<
>::988897735/                                 &89:;;;<<<<<<
>::988897735/                                 &89:;;;<<<<<<
>;;;;;;::997564'        '                       8:;;;<<<<<<
><;;;;;;::::9875&      .3                       *9;;;<<<<<<
><<<<;;;;;:::972456-567763                      +9;;<<<<<<<
>=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<=
>>=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<==
>>==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<===
>>>==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<====
>>>>===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<======
>>>>>===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### =


```



## B

This implements a 16bit fixed point arithmetic Mandelbrot set calculation.
{{works with|The Amsterdam Compiler Kit - B|V6.1pre1}}                                                

```B
main() {
  auto cx,cy,x,y,x2,y2;
  auto iter;

  auto xmin,xmax,ymin,ymax,maxiter,dx,dy;

  xmin = -8601;
  xmax =  2867;
  ymin = -4915;
  ymax =  4915;

  maxiter = 32;

  dx = (xmax-xmin)/79;
  dy = (ymax-ymin)/24;

  cy=ymin;
  while( cy<=ymax ) {
    cx=xmin;
    while( cx<=xmax ) {
      x = 0;
      y = 0;
      x2 = 0;
      y2 = 0;
      iter=0;
      while( iter<maxiter ) {
        if( x2+y2>16384 ) break;
        y = ((x*y)>>11)+cy;
        x = x2-y2+cx;
        x2 = (x*x)>>12;
        y2 = (y*y)>>12;
        iter++;
      }
      putchar(' '+iter);
      cx =+ dx;
    }
    putchar(13);
    putchar(10);
    cy =+ dy;
  }

  return(0);
}
```

{{out}}

```txt

!!!!!!!!!!!!!!!"""""""""""""####################################""""""""""""""""
!!!!!!!!!!!!!"""""""""#######################$$$$$$$%'+)%%%$$$$$#####"""""""""""
!!!!!!!!!!!"""""""#######################$$$$$$$$%%%&&(+,)++&%$$$$$$######""""""
!!!!!!!!!"""""#######################$$$$$$$$$$%%%%&')*5:/+('&%%$$$$$$#######"""
!!!!!!!!""""#####################$$$$$$$$$$%%%&&&''),@@@@@@@,'&%%%%%$$$$########
!!!!!!!"""####################$$$$$$$$%%%&'())((())*,@@@@@@/+))('&&&&)'%$$######
!!!!!!""###################$$$$$%%%%%%&&&'+.@@=/<@@@@@@@@@@@@@@@/++@..93%%$#####
!!!!!"################$$$%%%%%%%%%%&&&&'),+2@@@@@@@@@@@@@@@@@@@@@@@@@1(&&%$$####
!!!!"##########$$$$$%%&(-(''''''''''''(*,5@@@@@@@@@@@@@@@@@@@@@@@@@@@@+)-&%$$###
!!!!####$$$$$$$$%%%%%&'(*-@1.+.@-4+))**@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@4-(&%$$$##
!!!!#$$$$$$$$$%%%%%%'''++.6@@@@@@@@@8/0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@3(%%$$$$#
!!!#$$$$$$$%&&&&''()/-5.5@@@@@@@@@@@@@>@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@?'&%%$$$$#
!!!(**+/+<523/80/46@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@4+)'&&%%$$$$#
!!!#$$$$$$$%&&&&''().-2.@@@@@@@@@@@@@@?@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'&%%$$$$#
!!!!#$$$$$$$$$%%%%%&'''/,.7@@@@@@@@@;/0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@0'%%$$$$#
!!!!####$$$$$$$$%%%%%&'(*-:2.,/?-5+))**@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@4+(&%$$$##
!!!!"##########$$$$$%%&(-(''''(''''''((*,4@@@@@@@@@@@@@@@@@@@@@@@@@@@4+).&%$$###
!!!!!"################$$$%%%%%%%%%%&&&&')<,4@@@@@@@@@@@@@@@@@@@@@@@@@/('&%%$####
!!!!!!""##################$$$$$$%%%%%%&&&'*.@@@0@@@@@@@@@@@@@@@@1,,@//9)%%$#####
!!!!!!!"""####################$$$$$$$$%%%&(())((()**-@@@@@@/+)))'&&&')'%$$######
!!!!!!!!""""#####################$$$$$$$$$$%%%&&&''(,@@@@@@@+'&&%%%%%$$$########
!!!!!!!!!"""""#######################$$$$$$$$$$%%%%&')*7@0+('&%%%$$$$$#######"""
!!!!!!!!!!!"""""""######################$$$$$$$$$%%%&&(+-).*&%$$$$$$######""""""
!!!!!!!!!!!!!"""""""""#######################$$$$$$%%'3(%%%$$$$$######""""""""""
!!!!!!!!!!!!!!!""""""""""""#####################################""""""""""""""""

```



## BASIC

{{works with|QBasic}}

This is almost exactly the same as the pseudocode from [[wp:Mandelbrot set#For_programmers|the Wikipedia entry's "For programmers" section]] (which it's closely based on, of course). The image generated is very blocky ("low-res") due to the selected video mode, but it's fairly accurate.


```qbasic
SCREEN 13
WINDOW (-2, 1.5)-(2, -1.5)
FOR x0 = -2 TO 2 STEP .01
    FOR y0 = -1.5 TO 1.5 STEP .01
        x = 0
        y = 0

        iteration = 0
        maxIteration = 223

        WHILE (x * x + y * y <= (2 * 2) AND iteration < maxIteration)
            xtemp = x * x - y * y + x0
            y = 2 * x * y + y0

            x = xtemp

            iteration = iteration + 1
        WEND

        IF iteration <> maxIteration THEN
            c = iteration
        ELSE
            c = 0
        END IF

        PSET (x0, y0), c + 32
    NEXT
NEXT
```


=
## BASIC256
=

<lang basic-256>fastgraphics

graphsize 384,384
refresh
kt = 319 : m = 4.0
xmin = -2.1 : xmax = 0.6 : ymin = -1.35 : ymax = 1.35
dx = (xmax - xmin) / graphwidth : dy = (ymax - ymin) / graphheight

for x = 0 to graphwidth
	jx = xmin + x * dx
	for y = 0 to graphheight
		jy = ymin + y * dy
		k = 0 : wx = 0.0 : wy = 0.0
		do
			tx = wx * wx - wy * wy + jx
			ty = 2.0 * wx * wy + jy
			wx = tx
			wy = ty
			r = wx * wx + wy * wy
			k = k + 1
		until r > m or k > kt
		
		if k > kt then
			color black
		else 
			if k < 16 then color k * 8, k * 8, 128 + k * 4
			if k >= 16 and k < 64 then color 128 + k - 16, 128 + k - 16, 192 + k - 16
			if k >= 64 then color kt - k, 128 + (kt - k) / 2, kt - k
		end if
		plot x, y
	next y
	refresh
next x
imgsave "Mandelbrot_BASIC-256.png", "PNG"

```

{{out|Image generated by the script}}
[[File:Mandelbrot BASIC-256.jpg|220px]]

=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}

```bbcbasic
      sizex% = 300 : sizey% = 300
      maxiter% = 128
      VDU 23,22,sizex%;sizey%;8,8,16,128
      ORIGIN 0,sizey%
      GCOL 1
      FOR X% = 0 TO 2*sizex%-2 STEP 2
        xi = X%/200 - 2
        FOR Y% = 0 TO sizey%-2 STEP 2
          yi = Y% / 200
          x = 0
          y = 0
          FOR I% = 1 TO maxiter%
            IF x*x+y*y > 4 EXIT FOR
            xt = xi + x*x-y*y
            y = yi + 2*x*y
            x = xt
          NEXT
          IF I%>maxiter% I%=0
          COLOUR 1,I%*15,I%*8,0
          PLOT X%,Y% : PLOT X%,-Y%
        NEXT
      NEXT X%
```

[[File:Mandelbrot_bbc.gif]]

=
## Liberty BASIC
=
Any words of description go outside of lang tags.

```lb
nomainwin

WindowWidth  =440
WindowHeight =460

open "Mandelbrot Set" for graphics_nsb_nf as #w

#w "trapclose [quit]"
#w "down"

for x0 = -2 to 1 step .0033
    for y0 = -1.5 to 1.5 step .0075
        x = 0
        y = 0

        iteration    =   0
        maxIteration = 255

        while ( ( x *x +y *y) <=4) and ( iteration <maxIteration)
            xtemp      =x *x -y *y +x0
            y          =2 *x *y +y0
            x          = xtemp
            iteration  = iteration + 1
        wend

        if iteration <>maxIteration then
            c =iteration
        else
            c =0
        end if

        call pSet x0, y0, c
        scan
    next
next

#w "flush"

wait

sub pSet x, y, c
    xScreen = 10 +( x +2)   /3 *400
    yScreen = 10 +( y +1.5) /3 *400
    if c =0 then
        col$ ="red"
    else
        if c mod 2 =1 then col$ ="lightgray" else col$ ="white"
    end if
    #w "color "; col$
    #w "set "; xScreen; " "; yScreen
end sub

[quit]
close #w
end

```


=
## OS/8 BASIC
=
Works under BASIC on a PDP-8 running OS/8. Various emulators exist including simh's PDP-8 emulator and the [http://www.bernhard-baehr.de/pdp8e/pdp8e.html PDP-8/E Simulator] for Classic Macintosh and OS X.

```qbasic
10 X1=59\Y1=21
20 I1=-1.0\I2=1.0\R1=-2.0\R2=1.0
30 S1=(R2-R1)/X1\S2=(I2-I1)/Y1
40 FOR Y=0 TO Y1
50 I3=I1+S2*Y
60 FOR X=0 TO X1
70 R3=R1+S1*X\Z1=R3\Z2=I3
80 FOR N=0 TO 30
90 A=Z1*Z1\B=Z2*Z2
100 IF A+B>4.0 GOTO 130
110 Z2=2*Z1*Z2+I3\Z1=A-B+R3
120 NEXT N
130 PRINT CHR$(62-N);
140 NEXT X
150 PRINT
160 NEXT Y
170 END
```

{{out}}

```txt
>>>>>>
### ==<<<<<<<<<<<<<<<;;;;;;:::96032:;;;;<<<<========

>>>>>===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### ==

>>>>===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<
### =

>>>==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<=====
>>==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<====
>>=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<===
>=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<==
><<<<;;;;;:::972456-567763                      +9;;<<<<<<<=
><;;;;;;::::9875&      .3                       *9;;;<<<<<<=
>;;;;;;::997564'        '                       8:;;;<<<<<<=
>::988897735/                                 &89:;;;<<<<<<=
>::988897735/                                 &89:;;;<<<<<<=
>;;;;;;::997564'        '                       8:;;;<<<<<<=
><;;;;;;::::9875&      .3                       *9;;;<<<<<<=
><<<<;;;;;:::972456-567763                      +9;;<<<<<<<=
>=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<==
>>=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<===
>>==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<====
>>>==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<=====
>>>>===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<
### =

>>>>>===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### ==

>>>>>>
### ==<<<<<<<<<<<<<<<;;;;;;:::96032:;;;;<<<<========

```


=
## Quite BASIC
=

```Quite BASIC
      
1000 REM Mandelbrot Set Project
1010 REM Quite BASIC Math Project
1015 REM 'http://www.quitebasic.com/prj/math/mandelbrot/
1020 REM ------------------------ 
1030 CLS
1040 PRINT "This program plots a graphical representation of the famous Mandelbrot set.  It takes a while to finish so have patience and don't have too high expectations;  the graphics resolution is not very high on our canvas."
2000 REM Initialize the color palette
2010 GOSUB 3000
2020 REM L is the maximum iterations to try
2030 LET L = 100
2040 FOR I = 0 TO 100
2050 FOR J = 0 TO 100
2060 REM Map from pixel coordinates (I,J) to math (U,V)
2060 LET U = I / 50 - 1.5
2070 LET V = J / 50 - 1
2080 LET X = U
2090 LET Y = V
2100 LET N = 0
2110 REM Inner iteration loop starts here 
2120 LET R = X * X
2130 LET Q = Y * Y
2140 IF R + Q > 4 OR N >= L THEN GOTO 2190
2150 LET Y = 2 * X * Y + V
2160 LET X = R - Q + U
2170 LET N = N + 1
2180 GOTO 2120
2190 REM Compute the color to plot
2200 IF N < 10 THEN LET C = "black" ELSE LET C = P[ROUND(8 * (N-10) / (L-10))]
2210 PLOT I, J, C 
2220 NEXT J
2230 NEXT I
2240 END
3000 REM Subroutine -- Set up Palette
3010 ARRAY P
3020 LET P[0] = "black"
3030 LET P[1] = "magenta"
3040 LET P[2] = "blue"
3050 LET P[3] = "green"
3060 LET P[4] = "cyan"
3070 LET P[5] = "red"
3080 LET P[6] = "orange"
3090 LET P[7] = "yellow"
3090 LET P[8] = "white"
3100 RETURN

```


=
## Run BASIC
=

```Runbasic
'Mandelbrot V4 for RunBasic
'Based on LibertyBasic solution
'copy the code and go to runbasic.com
'http://rosettacode.org/wiki/Mandelbrot_set#Liberty_BASIC
'May 2015 (updated 29 Apr 2018)
'
'Note - we only get so much processing time on the server, so the
'graph is computed in three or four pieces
'
WindowWidth  = 320  'RunBasic max size 800 x 600
WindowHeight = 320
'print zone -2 to 1 (X)
'print zone -1.5 to 1.5 (Y)  
a = -1.5  'graph -1.5 to -0.75, first "loop" 
b = -0.75  'adjust for max processor time (y0 for loop below)

'open "Mandelbrot Set" for graphics_nsb_nf as #w  not used in RunBasic
 
graphic #w, WindowWidth, WindowHeight
'#w "trapclose [quit]"       not used in RunBasic
'#w "down"                   not used in RunBasic
 
cls 
'#w flush() 
#w cls("black")
render #w
 '#w flush()
input "OK, hit enter to continue"; guess
cls
 
[man_calc]
'3/screen size 3/800 = 0.00375  ** 3/790 = 0.0037974
'3/screen size (y) 3/600 = .005 ** 3/590 = 0.0050847
'3/215 = .0139 .0068 = 3/440
cc = 3/299
'
    for x0 = -2 to 1 step cc    
    for y0 = a to b step  cc 
        x = 0
        y = 0
 
        iteration    =   0
        maxIteration = 255 
 
        while ( ( x *x +y *y) <=4) and ( iteration <maxIteration)
            xtemp      =x *x -y *y +x0
            y          =2 *x *y +y0
            x          = xtemp
            iteration  = iteration + 1
        wend
 
        if iteration <>maxIteration then
            c =iteration
        else
            c =0
        end if
 
        call pSet x0, y0, c
        'scan why scan? (wait for user input) with RunBasic ?
    next
next
 
'#w flush()  'what is flush? RunBasic uses the render command.
render #w
 
input "OK, hit enter to continue"; guess
cls
a = a + 0.75
b = b + 0.75
if b > 1.6 then goto[quit] else goto[man_calc]
 
sub pSet x, y, c
    xScreen = 5+(x +2)   /3 * 300 'need positive screen number
    yScreen = 5+(y +1.5) /3 * 300 'and 5x5 boarder
    if c =0 then
        col$ ="red"
    else
        if c mod 2 =1 then col$ ="lightgray" else col$ ="white"
    end if
    #w "color "; col$
    #w "set "; xScreen; " "; yScreen
end sub
 
[quit]
'cls
print
print "This is a Mandelbrot Graph output from www.runbasic.com" 
render #w
print "All done, good bye."
end

```


=
## Sinclair ZX81 BASIC
=
{{trans|QBasic}}
Requires at least 2k of RAM.

Glacially slow, but does eventually produce a tolerable low-resolution image (screenshot [http://edmundgriffiths.com/zxmandelbrot.jpg here]). You can adjust the constants in lines 30 and 40 to zoom in on a particular area, if you like.

```zxbasic
 10 FOR I=0 TO 63
 20 FOR J=43 TO 0 STEP -1
 30 LET X=(I-52)/31
 40 LET Y=(J-22)/31
 50 LET XA=0
 60 LET YA=0
 70 LET ITER=0
 80 LET XTEMP=XA*XA-YA*YA+X
 90 LET YA=2*XA*YA+Y
100 LET XA=XTEMP
110 LET ITER=ITER+1
120 IF XA*XA+YA*YA<=4 AND ITER<200 THEN GOTO 80
130 IF ITER=200 THEN PLOT I, J
140 NEXT J
150 NEXT I
```


=
## Microsoft Small Basic
=

```Small BASIC

GraphicsWindow.Show()
size = 500
half = 250
GraphicsWindow.Width = size * 1.5
GraphicsWindow.Height = size
GraphicsWindow.Title = "Mandelbrot"
For px = 1 To size * 1.5
  x_0 = px/half - 2
  For py = 1 To size
    y_0 = py/half - 1
    x = x_0
    y = y_0
    i = 0
    While(c <= 2 AND i<100)
      x_1 = Math.Power(x, 2) - Math.Power(y, 2) + x_0
      y_1 = 2 * x * y + y_0
      c = Math.Power(Math.Power(x_1, 2) + Math.Power(y_1, 2), 0.5)
      x = x_1
      y = y_1
      i = i + 1
    EndWhile
    If i < 99 Then
      GraphicsWindow.SetPixel(px, py, GraphicsWindow.GetColorFromRGB((255/25)*i, (255/25)*i, (255/5)*i))
    Else 
      GraphicsWindow.SetPixel(px, py, "black")
    EndIf
    c=0
 EndFor
EndFor

```


=
## Visual BASIC for Applications on Excel
=
{{works with|Excel 2013}}
Based on the BBC BASIC version. Create a spreadsheet with -2 to 2 in row 1 and -2 to 2 in the A column (in steps of your choosing). In the cell B2, call the function with =mandel(B$1,$A2) and copy the cell to all others in the range. Conditionally format the cells to make the colours pleasing (eg based on values, 3-color scale, min value 2 [colour red], midpoint number 10 [green] and highest value black. Then format the cells with the custom type "";"";"" to remove the numbers.

```VBA

Function mandel(xi As Double, yi As Double)

maxiter = 256
x = 0
y = 0

For i = 1 To maxiter
    If ((x * x) + (y * y)) > 4 Then Exit For
    xt = xi + ((x * x) - (y * y))
    y = yi + (2 * x * y)
    x = xt
    Next
    
mandel = i
End Function

```

[[File:vbamandel.png]]
Edit: I don't seem to be able to upload the screenshot, so I've shared it here: https://goo.gl/photos/LkezpuQziJPAtdnd9

==={{header|Microsoft Super Extended Color BASIC (Tandy Color Computer 3)}}===


```COCO3BASIC

1 REM MANDELBROT SET - TANDY COCO 3
2 POKE 65497,1
10 HSCREEN 2
20 HCLS
30 X1=319:Y1=191
40 I1=-1.0:I2=1.0:R1=-2:R2=1.0
50 S1=(R2-R1)/X1:S2=(I2-I1)/Y1
60 FOR Y=0 TO Y1
70 I3=I1+S2*Y
80 FOR X=0 TO X1
90 R3=R1+S1*X:Z1=R3:Z2=I3
100 FOR N=0 TO 30
110 A=Z1*Z1:B=Z2*Z2
120 IF A+B>4.0 GOTO 150
130 Z2=2*Z1*Z2+I3:Z1=A-B+R3
140 NEXT N
150 HSET(X,Y,N-16*INT(N/16))
160 NEXT X
170 NEXT Y
180 GOTO 180

```



## Befunge

Using 14-bit fixed point arithmetic for simplicity and portability. It should work in most interpreters, but the exact output is implementation dependent, and some will be unbearably slow.

X scale is (-2.0, 0.5); Y scale is (-1, 1); Max iterations 94 with the ASCII character set as the "palette".


```Befunge>0
:00p58*`#@_0>:01p78vv$$<
@^+1g00,+55_v# !`\+*9<>4v$
@v30p20"?~^"< ^+1g10,+*8<$
@>p0\>\::*::882**02g*0v >^
`*:*" d":+*:-*"[Z"+g3 < |<
v-*"[Z"+g30*g20**288\--\<#
>2**5#>8*:*/00g"P"*58*:*v^
v*288 p20/**288:+*"[Z"+-<:
>*%03 p58*:*/01g"3"* v>::^
   \_^#!:-1\+-*2*:*85<^

```


{{out}}

```txt
}}}}}}}}}|||||||{{{{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzyyyyxwusjuthwyzzzzzzz{{{{{{{
}}}}}}}}|||||||{{{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzzyyyyxwwvtqptvwxyyzzzzzzz{{{{{
}}}}}}}||||||{{{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzzzyyyyxwvuqaZlnvwxyyyzzzzzzz{{{{
}}}}}}|||||{{{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzzzyyyyxxvqXp^g Ynslvxyyyyyzzzzz{{{
}}}}}}||||{{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzzzyyyxxxxwvtp      6puwxyyyyyyzzzz{{
}}}}}||||{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzzyyyxxxxxwwvvqc      &8uvwxxxyyyyyzzz{
}}}}|||{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzyyyywwvtvvvvuutsp      Hrtuuvwxxxxwqxyzz
}}}}||{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzyyyyyxwvqemrttj m id+    PRUiPp_rvvvvudwxyz
}}}||{{{{{{{{{{{{{{{{{{{{{zzzzzzyyyyyyyxxxwurf  ZnW                4nrslnobgwyy
}}}||{{{{{{{{{{{{{{{{{{{zzzzyyyyyyyyyyxxxwvusg                        N  Uquxyy
}}||{{{{{{{{{{{{{{{{{zzzzyyyyyyyyyyyxxxxwvrrrkC                          grwxxy
}}|{{{{{{{{{{{{{{{zzzzyxxxxxyyyyyxxxxxwwukM!f                            ptvwxy
}}|{{{{{{{{{{zzzzzzyyxwsuwwwwwwwwwwwwwvvurn[                              ptuox
}|{{{{{{{zzzzzzzzyyyxxvptuuvvumsuvvvvvvu`                                   hjx
}|{{{{zzzzzzzzzyyyyyxxwusogoqsqg]pptuuttlc                                 ntwx
}{{{zzzzzzzzzyyyyyyxxwwuto  -    O jpssrO                                  nsvx
}{{zzzzzzzzzyyyyyyxwwwvrrT4          TonR                                  Ufwy
}{zzzzzzzzyyyyyxxwttuutqe             Dj                                   $uxy
}zzzzzzzzyxxxxxwwvuppnpn               `                                   twxy
}yyyxxwvwwwxwvvvrtppc  Y                                                 auwxxy
                                                                       dqtvwxyy
}yyyxxwvwwwxwvvvrtppc  Y                                                 auwxxy
}zzzzzzzzyxxxxxwwvuppnpn               `                                   twxy
}{zzzzzzzzyyyyyxxwttuutqe             Dj                                   $uxy
}{{zzzzzzzzzyyyyyyxwwwvrrT4          TonR                                  Ufwy
}{{{zzzzzzzzzyyyyyyxxwwuto  -    O jpssrO                                  nsvx
}|{{{{zzzzzzzzzyyyyyxxwusogoqsqg]pptuuttlc                                 ntwx
}|{{{{{{{zzzzzzzzyyyxxvptuuvvumsuvvvvvvu`                                   hjx
}}|{{{{{{{{{{zzzzzzyyxwsuwwwwwwwwwwwwwvvurn[                              ptuox
}}|{{{{{{{{{{{{{{{zzzzyxxxxxyyyyyxxxxxwwukM!f                            ptvwxy
}}||{{{{{{{{{{{{{{{{{zzzzyyyyyyyyyyyxxxxwvrrrkC                          grwxxy
}}}||{{{{{{{{{{{{{{{{{{{zzzzyyyyyyyyyyxxxwvusg                        N  Uquxyy
}}}||{{{{{{{{{{{{{{{{{{{{{zzzzzzyyyyyyyxxxwurf  ZnW                4nrslnobgwyy
}}}}||{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzyyyyyxwvqemrttj m id+    PRUiPp_rvvvvudwxyz
}}}}|||{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzyyyywwvtvvvvuutsp      Hrtuuvwxxxxwqxyzz
}}}}}||||{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzzyyyxxxxxwwvvqc      &8uvwxxxyyyyyzzz{
}}}}}}||||{{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzzzyyyxxxxwvtp      6puwxyyyyyyzzzz{{
}}}}}}|||||{{{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzzzyyyyxxvqXp^g Ynslvxyyyyyzzzzz{{{
}}}}}}}||||||{{{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzzzyyyyxwvuqaZlnvwxyyyzzzzzzz{{{{
}}}}}}}}|||||||{{{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzzyyyyxwwvtqptvwxyyzzzzzzz{{{{{
}}}}}}}}}|||||||{{{{{{{{{{{{{{{{{{{{{{{{{{zzzzzzzzzyyyyxwusjuthwyzzzzzzz{{{{{{{
```



## Bourne Again SHell

{{works with|BASH|4}}

```bash
((xmin=-8601))  # int(-2.1*4096)
((xmax=2867))   # int( 0.7*4096)
 
((ymin=-4915))  # int(-1.2*4096)
((ymax=4915))   # int( 1.2*4096)

((maxiter=30))

((dx=(xmax-xmin)/72))
((dy=(ymax-ymin)/24))

C='0123456789'
((lC=${#C}))

for((cy=ymax;cy>=ymin;cy-=dy)) ; do
	for((cx=xmin;cx<=xmax;cx+=dx)) ; do
		((x=0,y=0,x2=0,y2=0))
		for((iter=0;iter<maxiter && x2+y2<=16384;iter++)) ; do
			((y=((x*y)>>11)+cy,x=x2-y2+cx,x2=(x*x)>>12,y2=(y*y)>>12))
		done
		((c=iter%lC))
		echo -n ${C:$c:1}
	done
	echo
done
```


{{out}}

```txt

1111111111111222222222222333333333333333333333333333333333222222222222222
1111111111112222222233333333333333333333344444456015554444333332222222222
1111111111222222333333333333333333333444444445556704912544444433333222222
1111111112222333333333333333333333444444444555678970508655544444333333222
1111111222233333333333333333333444444444556667807000002076555544443333333
1111112223333333333333333333444444455577898889016000003099766662644333333
1111122333333333333333334444455555566793000800000000000000931045875443333
1111123333333333333344455555555566668014000000000000000000000009865544333
1111233333333344445568277777777777880600000000000000000000000009099544433
1111333344444445555678041513450199023000000000000000000000000000807544433
1112344444445555556771179000000000410000000000000000000000000000036544443
1114444444566667782404400000000000000000000000000000000000000000775544443
1119912160975272040000000000000000000000000000000000000000000219765544443
1114444444566667792405800000000000000000000000000000000000000000075544443
1113344444445555556773270000000000500000000000000000000000000000676544443
1111333344444445555678045623255199020000000000000000000000000000707544433
1111233333333444445568177777877777881500000000000000000000000009190544433
1111123333333333333344455555555566668126000000000000000000000009865544333
1111122333333333333333334444455555566793000100000000000000941355975443333
1111112223333333333333333334444444455588908889016000003099876670654433333
1111111222233333333333333333334444444445556667800000002976555554443333333
1111111112222333333333333333333333444444444555679060608655544444333333222
1111111111222222333333333333333333333444444445556702049544444433333222222
1111111111112222222233333333333333333333344444456205554444333333222222222
1111111111111222222222222333333333333333333333333333333333222222222222222

```



## Brace

This is a simple Mandelbrot plotter.  A longer version based on this smooths colors, and avoids calculating the time-consuming black pixels: http://sam.ai.ki/brace/examples/mandelbrot.d/1

```brace
#!/usr/bin/env bx
use b

Main():
	num outside = 16, ox = -0.5, oy = 0, r = 1.5
	long i, max_i = 100, rb_i = 30
	space()
	uint32_t *px = pixel()
	num d = 2*r/h, x0 = ox-d*w_2, y0 = oy+d*h_2
	for(y, 0, h):
		cmplx c = x0 + (y0-d*y)*I
		repeat(w):
			cmplx w = 0
			for i=0; i < max_i && cabs(w) < outside; ++i
				w = w*w + c
			*px++ = i < max_i ? rainbow(i*359 / rb_i % 360) : black
			c += d
```


An example plot from the longer version:

[[File:brace-mandelbrot-small.png]]

=={{header|Brainf***}}==
<lang Brainf***>
     A mandelbrot set fractal viewer in brainf*ck written by Erik Bosman
+++++++++++++[->++>>>+++++>++>+<<<<<<]>>>>>++++++>--->>>>>>>>>>+++++++++++++++[[
>>>>>>>>>]+[<<<<<<<<<]>>>>>>>>>-]+[>>>>>>>>[-]>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>[-]+
<<<<<<<+++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>>>>+>>>>>>>>>>>>>>>>>>>>>>>>>>
>+<<<<<<<<<<<<<<<<<[<<<<<<<<<]>>>[-]+[>>>>>>[>>>>>>>[-]>>]<<<<<<<<<[<<<<<<<<<]>>
>>>>>[-]+<<<<<<++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>>>+<<<<<<+++++++[-[->>>
>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>>>+<<<<<<<<<<<<<<<<[<<<<<<<<<]>>>[[-]>>>>>>[>>>>>
>>[-<<<<<<+>>>>>>]<<<<<<[->>>>>>+<<+<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>
[>>>>>>>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+<<+<<<+<<]>>>>>>>>]<<<<<<<<<[<<<<<<<
<<]>>>>>>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+<<+<<<<<]>>>>>>>>>+++++++++++++++[[
>>>>>>>>>]+>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+[
>+>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>[-<<<<+>>>>]<<<<[->>>>+<<<<<[->>[
-<<+>>]<<[->>+>>+<<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>>>>]<<<<<<<
<<[>[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<]>[->>>>>>>>>+<<<<<<<<<]<+>>>>>>>>]<<<<<<<<<
[>[-]<->>>>[-<<<<+>[<->-<<<<<<+>>>>>>]<[->+<]>>>>]<<<[->>>+<<<]<+<<<<<<<<<]>>>>>
>>>>[>+>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>>[-<<<<<+>>>>>]<<<<<[->>>>>+
<<<<<<[->>>[-<<<+>>>]<<<[->>>+>+<<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>
>>>>>>>]<<<<<<<<<[>>[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<<]>>[->>>>>>>>>+<<<<<<<<<]<<
+>>>>>>>>]<<<<<<<<<[>[-]<->>>>[-<<<<+>[<->-<<<<<<+>>>>>>]<[->+<]>>>>]<<<[->>>+<<
<]<+<<<<<<<<<]>>>>>>>>>[>>>>[-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<+>>>>>>>>>>>>>
>>>>>>>>>>>>>>>>>>>>>>>]>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>+++++++++++++++[[>>>>
>>>>>]<<<<<<<<<-<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+>>>>>>>>>>>>>>>>>>>>>+<<<[<<<<<<
<<<]>>>>>>>>>[>>>[-<<<->>>]+<<<[->>>->[-<<<<+>>>>]<<<<[->>>>+<<<<<<<<<<<<<[<<<<<
<<<<]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>>>[-<<<<->>>>]+<<<<[->>>>-<[-<<<+>>>]<<<[->
>>+<<<<<<<<<<<<[<<<<<<<<<]>>>[-]+>>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>>>>>>>>]<<<<<<
<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]<<<<<<<[->+>>>-<<<<]>>>>>>>>>+++++++++++++++++++
+++++++>>[-<<<<+>>>>]<<<<[->>>>+<<[-]<<]>>[<<<<<<<+<[-<+>>>>+<<[-]]>[-<<[->+>>>-
<<<<]>>>]>>>>>>>>>>>>>[>>[-]>[-]>[-]>>>>>]<<<<<<<<<[<<<<<<<<<]>>>[-]>>>>>>[>>>>>
[-<<<<+>>>>]<<<<[->>>>+<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>[-<<<<<<<<
<+>>>>>>>>>]>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>+++++++++++++++[[>>>>>>>>>]+>[-
]>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+[>+>>>>>>>>]<<<
<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>>[-<<<<<+>>>>>]<<<<<[->>>>>+<<<<<<[->>[-<<+>>]<
<[->>+>+<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>>>>]<<<<<<<<<[>[->>>>
>>>>>+<<<<<<<<<]<<<<<<<<<<]>[->>>>>>>>>+<<<<<<<<<]<+>>>>>>>>]<<<<<<<<<[>[-]<->>>
[-<<<+>[<->-<<<<<<<+>>>>>>>]<[->+<]>>>]<<[->>+<<]<+<<<<<<<<<]>>>>>>>>>[>>>>>>[-<
<<<<+>>>>>]<<<<<[->>>>>+<<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>+>>>>>>>>
]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>>[-<<<<<+>>>>>]<<<<<[->>>>>+<<<<<<[->>[-<<+
>>]<<[->>+>>+<<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>>>>]<<<<<<<<<[>
[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<]>[->>>>>>>>>+<<<<<<<<<]<+>>>>>>>>]<<<<<<<<<[>[-
]<->>>>[-<<<<+>[<->-<<<<<<+>>>>>>]<[->+<]>>>>]<<<[->>>+<<<]<+<<<<<<<<<]>>>>>>>>>
[>>>>[-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<+>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
]>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>[-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<+>
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>]>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>++++++++
+++++++[[>>>>>>>>>]<<<<<<<<<-<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+[>>>>>>>>[-<<<<<<<+
>>>>>>>]<<<<<<<[->>>>>>>+<<<<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>>>>[
-]>>>]<<<<<<<<<[<<<<<<<<<]>>>>+>[-<-<<<<+>>>>>]>[-<<<<<<[->>>>>+<++<<<<]>>>>>[-<
<<<<+>>>>>]<->+>]<[->+<]<<<<<[->>>>>+<<<<<]>>>>>>[-]<<<<<<+>>>>[-<<<<->>>>]+<<<<
[->>>>->>>>>[>>[-<<->>]+<<[->>->[-<<<+>>>]<<<[->>>+<<<<<<<<<<<<[<<<<<<<<<]>>>[-]
+>>>>>>[>>>>>>>>>]>+<]]+>>>[-<<<->>>]+<<<[->>>-<[-<<+>>]<<[->>+<<<<<<<<<<<[<<<<<
<<<<]>>>>[-]+>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>>>>>>>>]<<<<<<<<]>>>>>>>>]<<<<<<<<<
[<<<<<<<<<]>>>>[-<<<<+>>>>]<<<<[->>>>+>>>>>[>+>>[-<<->>]<<[->>+<<]>>>>>>>>]<<<<<
<<<+<[>[->>>>>+<<<<[->>>>-<<<<<<<<<<<<<<+>>>>>>>>>>>[->>>+<<<]<]>[->>>-<<<<<<<<<
<<<<<+>>>>>>>>>>>]<<]>[->>>>+<<<[->>>-<<<<<<<<<<<<<<+>>>>>>>>>>>]<]>[->>>+<<<]<<
<<<<<<<<<<]>>>>[-]<<<<]>>>[-<<<+>>>]<<<[->>>+>>>>>>[>+>[-<->]<[->+<]>>>>>>>>]<<<
<<<<<+<[>[->>>>>+<<<[->>>-<<<<<<<<<<<<<<+>>>>>>>>>>[->>>>+<<<<]>]<[->>>>-<<<<<<<
<<<<<<<+>>>>>>>>>>]<]>>[->>>+<<<<[->>>>-<<<<<<<<<<<<<<+>>>>>>>>>>]>]<[->>>>+<<<<
]<<<<<<<<<<<]>>>>>>+<<<<<<]]>>>>[-<<<<+>>>>]<<<<[->>>>+>>>>>[>>>>>>>>>]<<<<<<<<<
[>[->>>>>+<<<<[->>>>-<<<<<<<<<<<<<<+>>>>>>>>>>>[->>>+<<<]<]>[->>>-<<<<<<<<<<<<<<
+>>>>>>>>>>>]<<]>[->>>>+<<<[->>>-<<<<<<<<<<<<<<+>>>>>>>>>>>]<]>[->>>+<<<]<<<<<<<
<<<<<]]>[-]>>[-]>[-]>>>>>[>>[-]>[-]>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>>>[-<
<<<+>>>>]<<<<[->>>>+<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>+++++++++++++++[
[>>>>>>>>>]+>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+
[>+>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>[-<<<<+>>>>]<<<<[->>>>+<<<<<[->>
[-<<+>>]<<[->>+>+<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>>>>]<<<<<<<<
<[>[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<]>[->>>>>>>>>+<<<<<<<<<]<+>>>>>>>>]<<<<<<<<<[
>[-]<->>>[-<<<+>[<->-<<<<<<<+>>>>>>>]<[->+<]>>>]<<[->>+<<]<+<<<<<<<<<]>>>>>>>>>[
>>>[-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<+>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>]>
>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>[-]>>>>+++++++++++++++[[>>>>>>>>>]<<<<<<<<<-<<<<<
<<<<[<<<<<<<<<]>>>>>>>>>-]+[>>>[-<<<->>>]+<<<[->>>->[-<<<<+>>>>]<<<<[->>>>+<<<<<
<<<<<<<<[<<<<<<<<<]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>>>[-<<<<->>>>]+<<<<[->>>>-<[-
<<<+>>>]<<<[->>>+<<<<<<<<<<<<[<<<<<<<<<]>>>[-]+>>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>
>>>>>>>]<<<<<<<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>[-<<<+>>>]<<<[->>>+>>>>>>[>+>>>
[-<<<->>>]<<<[->>>+<<<]>>>>>>>>]<<<<<<<<+<[>[->+>[-<-<<<<<<<<<<+>>>>>>>>>>>>[-<<
+>>]<]>[-<<-<<<<<<<<<<+>>>>>>>>>>>>]<<<]>>[-<+>>[-<<-<<<<<<<<<<+>>>>>>>>>>>>]<]>
[-<<+>>]<<<<<<<<<<<<<]]>>>>[-<<<<+>>>>]<<<<[->>>>+>>>>>[>+>>[-<<->>]<<[->>+<<]>>
>>>>>>]<<<<<<<<+<[>[->+>>[-<<-<<<<<<<<<<+>>>>>>>>>>>[-<+>]>]<[-<-<<<<<<<<<<+>>>>
>>>>>>>]<<]>>>[-<<+>[-<-<<<<<<<<<<+>>>>>>>>>>>]>]<[-<+>]<<<<<<<<<<<<]>>>>>+<<<<<
]>>>>>>>>>[>>>[-]>[-]>[-]>>>>]<<<<<<<<<[<<<<<<<<<]>>>[-]>[-]>>>>>[>>>>>>>[-<<<<<
<+>>>>>>]<<<<<<[->>>>>>+<<<<+<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>+>[-<-<<<<+>>>>
>]>>[-<<<<<<<[->>>>>+<++<<<<]>>>>>[-<<<<<+>>>>>]<->+>>]<<[->>+<<]<<<<<[->>>>>+<<
<<<]+>>>>[-<<<<->>>>]+<<<<[->>>>->>>>>[>>>[-<<<->>>]+<<<[->>>-<[-<<+>>]<<[->>+<<
<<<<<<<<<[<<<<<<<<<]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>[-<<->>]+<<[->>->[-<<<+>>>]<
<<[->>>+<<<<<<<<<<<<[<<<<<<<<<]>>>[-]+>>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>>>>>>>>]<
<<<<<<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>[-<<<+>>>]<<<[->>>+>>>>>>[>+>[-<->]<[->+
<]>>>>>>>>]<<<<<<<<+<[>[->>>>+<<[->>-<<<<<<<<<<<<<+>>>>>>>>>>[->>>+<<<]>]<[->>>-
<<<<<<<<<<<<<+>>>>>>>>>>]<]>>[->>+<<<[->>>-<<<<<<<<<<<<<+>>>>>>>>>>]>]<[->>>+<<<
]<<<<<<<<<<<]>>>>>[-]>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+<<+<<<<<]]>>>>[-<<<<+>
>>>]<<<<[->>>>+>>>>>[>+>>[-<<->>]<<[->>+<<]>>>>>>>>]<<<<<<<<+<[>[->>>>+<<<[->>>-
<<<<<<<<<<<<<+>>>>>>>>>>>[->>+<<]<]>[->>-<<<<<<<<<<<<<+>>>>>>>>>>>]<<]>[->>>+<<[
->>-<<<<<<<<<<<<<+>>>>>>>>>>>]<]>[->>+<<]<<<<<<<<<<<<]]>>>>[-]<<<<]>>>>[-<<<<+>>
>>]<<<<[->>>>+>[-]>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+<<+<<<<<]>>>>>>>>>[>>>>>>
>>>]<<<<<<<<<[>[->>>>+<<<[->>>-<<<<<<<<<<<<<+>>>>>>>>>>>[->>+<<]<]>[->>-<<<<<<<<
<<<<<+>>>>>>>>>>>]<<]>[->>>+<<[->>-<<<<<<<<<<<<<+>>>>>>>>>>>]<]>[->>+<<]<<<<<<<<
<<<<]]>>>>>>>>>[>>[-]>[-]>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>[-]>[-]>>>>>[>>>>>[-<<<<+
>>>>]<<<<[->>>>+<<<+<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>>>>[-<<<<<+>>>>>
]<<<<<[->>>>>+<<<+<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>+++++++++++++++[[>>>>
>>>>>]+>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]>[-]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+[>+>>
>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>[-<<<<+>>>>]<<<<[->>>>+<<<<<[->>[-<<+
>>]<<[->>+>>+<<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>>>>]<<<<<<<<<[>
[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<]>[->>>>>>>>>+<<<<<<<<<]<+>>>>>>>>]<<<<<<<<<[>[-
]<->>>>[-<<<<+>[<->-<<<<<<+>>>>>>]<[->+<]>>>>]<<<[->>>+<<<]<+<<<<<<<<<]>>>>>>>>>
[>+>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>->>>>>[-<<<<<+>>>>>]<<<<<[->>>>>+<<<<
<<[->>>[-<<<+>>>]<<<[->>>+>+<<<<]+>>>>>>>>>]<<<<<<<<[<<<<<<<<<]]>>>>>>>>>[>>>>>>
>>>]<<<<<<<<<[>>[->>>>>>>>>+<<<<<<<<<]<<<<<<<<<<<]>>[->>>>>>>>>+<<<<<<<<<]<<+>>>
>>>>>]<<<<<<<<<[>[-]<->>>>[-<<<<+>[<->-<<<<<<+>>>>>>]<[->+<]>>>>]<<<[->>>+<<<]<+
<<<<<<<<<]>>>>>>>>>[>>>>[-<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<+>>>>>>>>>>>>>>>>>
>>>>>>>>>>>>>>>>>>>]>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>+++++++++++++++[[>>>>>>>>
>]<<<<<<<<<-<<<<<<<<<[<<<<<<<<<]>>>>>>>>>-]+>>>>>>>>>>>>>>>>>>>>>+<<<[<<<<<<<<<]
>>>>>>>>>[>>>[-<<<->>>]+<<<[->>>->[-<<<<+>>>>]<<<<[->>>>+<<<<<<<<<<<<<[<<<<<<<<<
]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>>>[-<<<<->>>>]+<<<<[->>>>-<[-<<<+>>>]<<<[->>>+<
<<<<<<<<<<<[<<<<<<<<<]>>>[-]+>>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>>>>>>>>]<<<<<<<<]>
>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>->>[-<<<<+>>>>]<<<<[->>>>+<<[-]<<]>>]<<+>>>>[-<<<<
->>>>]+<<<<[->>>>-<<<<<<.>>]>>>>[-<<<<<<<.>>>>>>>]<<<[-]>[-]>[-]>[-]>[-]>[-]>>>[
>[-]>[-]>[-]>[-]>[-]>[-]>>>]<<<<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>>>[-]>>>>]<<<<<<<<<
[<<<<<<<<<]>+++++++++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>+>>>>>>>>>+<<<<<<<<
<<<<<<[<<<<<<<<<]>>>>>>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+[-]>>[>>>>>>>>>]<<<<<
<<<<[>>>>>>>[-<<<<<<+>>>>>>]<<<<<<[->>>>>>+<<<<<<<[<<<<<<<<<]>>>>>>>[-]+>>>]<<<<
<<<<<<]]>>>>>>>[-<<<<<<<+>>>>>>>]<<<<<<<[->>>>>>>+>>[>+>>>>[-<<<<->>>>]<<<<[->>>
>+<<<<]>>>>>>>>]<<+<<<<<<<[>>>>>[->>+<<]<<<<<<<<<<<<<<]>>>>>>>>>[>>>>>>>>>]<<<<<
<<<<[>[-]<->>>>>>>[-<<<<<<<+>[<->-<<<+>>>]<[->+<]>>>>>>>]<<<<<<[->>>>>>+<<<<<<]<
+<<<<<<<<<]>>>>>>>-<<<<[-]+<<<]+>>>>>>>[-<<<<<<<->>>>>>>]+<<<<<<<[->>>>>>>->>[>>
>>>[->>+<<]>>>>]<<<<<<<<<[>[-]<->>>>>>>[-<<<<<<<+>[<->-<<<+>>>]<[->+<]>>>>>>>]<<
<<<<[->>>>>>+<<<<<<]<+<<<<<<<<<]>+++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>+<<<
<<[<<<<<<<<<]>>>>>>>>>[>>>>>[-<<<<<->>>>>]+<<<<<[->>>>>->>[-<<<<<<<+>>>>>>>]<<<<
<<<[->>>>>>>+<<<<<<<<<<<<<<<<[<<<<<<<<<]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>>>>>>[-<
<<<<<<->>>>>>>]+<<<<<<<[->>>>>>>-<<[-<<<<<+>>>>>]<<<<<[->>>>>+<<<<<<<<<<<<<<[<<<
<<<<<<]>>>[-]+>>>>>>[>>>>>>>>>]>[-]+<]]+>[-<[>>>>>>>>>]<<<<<<<<]>>>>>>>>]<<<<<<<
<<[<<<<<<<<<]>>>>[-]<<<+++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>-<<<<<[<<<<<<<
<<]]>>>]<<<<.>>>>>>>>>>[>>>>>>[-]>>>]<<<<<<<<<[<<<<<<<<<]>++++++++++[-[->>>>>>>>
>+<<<<<<<<<]>>>>>>>>>]>>>>>+>>>>>>>>>+<<<<<<<<<<<<<<<[<<<<<<<<<]>>>>>>>>[-<<<<<<
<<+>>>>>>>>]<<<<<<<<[->>>>>>>>+[-]>[>>>>>>>>>]<<<<<<<<<[>>>>>>>>[-<<<<<<<+>>>>>>
>]<<<<<<<[->>>>>>>+<<<<<<<<[<<<<<<<<<]>>>>>>>>[-]+>>]<<<<<<<<<<]]>>>>>>>>[-<<<<<
<<<+>>>>>>>>]<<<<<<<<[->>>>>>>>+>[>+>>>>>[-<<<<<->>>>>]<<<<<[->>>>>+<<<<<]>>>>>>
>>]<+<<<<<<<<[>>>>>>[->>+<<]<<<<<<<<<<<<<<<]>>>>>>>>>[>>>>>>>>>]<<<<<<<<<[>[-]<-
>>>>>>>>[-<<<<<<<<+>[<->-<<+>>]<[->+<]>>>>>>>>]<<<<<<<[->>>>>>>+<<<<<<<]<+<<<<<<
<<<]>>>>>>>>-<<<<<[-]+<<<]+>>>>>>>>[-<<<<<<<<->>>>>>>>]+<<<<<<<<[->>>>>>>>->[>>>
>>>[->>+<<]>>>]<<<<<<<<<[>[-]<->>>>>>>>[-<<<<<<<<+>[<->-<<+>>]<[->+<]>>>>>>>>]<<
<<<<<[->>>>>>>+<<<<<<<]<+<<<<<<<<<]>+++++[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>>
+>>>>>>>>>>>>>>>>>>>>>>>>>>>+<<<<<<[<<<<<<<<<]>>>>>>>>>[>>>>>>[-<<<<<<->>>>>>]+<
<<<<<[->>>>>>->>[-<<<<<<<<+>>>>>>>>]<<<<<<<<[->>>>>>>>+<<<<<<<<<<<<<<<<<[<<<<<<<
<<]>>>>[-]+>>>>>[>>>>>>>>>]>+<]]+>>>>>>>>[-<<<<<<<<->>>>>>>>]+<<<<<<<<[->>>>>>>>
-<<[-<<<<<<+>>>>>>]<<<<<<[->>>>>>+<<<<<<<<<<<<<<<[<<<<<<<<<]>>>[-]+>>>>>>[>>>>>>
>>>]>[-]+<]]+>[-<[>>>>>>>>>]<<<<<<<<]>>>>>>>>]<<<<<<<<<[<<<<<<<<<]>>>>[-]<<<++++
+[-[->>>>>>>>>+<<<<<<<<<]>>>>>>>>>]>>>>>->>>>>>>>>>>>>>>>>>>>>>>>>>>-<<<<<<[<<<<
<<<<<]]>>>]

```



{{out}}

```txt
AAAAAAAAAAAAAAAABBBBBBBBBBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDEGFFEEEEDDDDDDCCCCCCCCCBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
AAAAAAAAAAAAAAABBBBBBBBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDEEEFGIIGFFEEEDDDDDDDDCCCCCCCCCBBBBBBBBBBBBBBBBBBBBBBBBBB
AAAAAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDEEEEFFFI KHGGGHGEDDDDDDDDDCCCCCCCCCBBBBBBBBBBBBBBBBBBBBBBB
AAAAAAAAAAAABBBBBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDEEEEEFFGHIMTKLZOGFEEDDDDDDDDDCCCCCCCCCBBBBBBBBBBBBBBBBBBBBB
AAAAAAAAAAABBBBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDEEEEEEFGGHHIKPPKIHGFFEEEDDDDDDDDDCCCCCCCCCCBBBBBBBBBBBBBBBBBB
AAAAAAAAAABBBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDDEEEEEEFFGHIJKS  X KHHGFEEEEEDDDDDDDDDCCCCCCCCCCBBBBBBBBBBBBBBBB
AAAAAAAAABBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDDEEEEEEFFGQPUVOTY   ZQL[MHFEEEEEEEDDDDDDDCCCCCCCCCCCBBBBBBBBBBBBBB
AAAAAAAABBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDDEEEEEFFFFFGGHJLZ         UKHGFFEEEEEEEEDDDDDCCCCCCCCCCCCBBBBBBBBBBBB
AAAAAAABBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDEEEEFFFFFFGGGGHIKP           KHHGGFFFFEEEEEEDDDDDCCCCCCCCCCCBBBBBBBBBBB
AAAAAAABBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDEEEEEFGGHIIHHHHHIIIJKMR        VMKJIHHHGFFFFFFGSGEDDDDCCCCCCCCCCCCBBBBBBBBB
AAAAAABBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDEEEEEEFFGHK   MKJIJO  N R  X      YUSR PLV LHHHGGHIOJGFEDDDCCCCCCCCCCCCBBBBBBBB
AAAAABBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDEEEEEEEEEFFFFGH O    TN S                       NKJKR LLQMNHEEDDDCCCCCCCCCCCCBBBBBBB
AAAAABBCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDEEEEEEEEEEEEFFFFFGHHIN                                 Q     UMWGEEEDDDCCCCCCCCCCCCBBBBBB
AAAABBCCCCCCCCCCCCCCCCCCCCCCCCCDDDDEEEEEEEEEEEEEEEFFFFFFGHIJKLOT                                     [JGFFEEEDDCCCCCCCCCCCCCBBBBB
AAAABCCCCCCCCCCCCCCCCCCCCCCDDDDEEEEEEEEEEEEEEEEFFFFFFGGHYV RQU                                     QMJHGGFEEEDDDCCCCCCCCCCCCCBBBB
AAABCCCCCCCCCCCCCCCCCDDDDDDDEEFJIHFFFFFFFFFFFFFFGGGGGGHIJN                                            JHHGFEEDDDDCCCCCCCCCCCCCBBB
AAABCCCCCCCCCCCDDDDDDDDDDEEEEFFHLKHHGGGGHHMJHGGGGGGHHHIKRR                                           UQ L HFEDDDDCCCCCCCCCCCCCCBB
AABCCCCCCCCDDDDDDDDDDDEEEEEEFFFHKQMRKNJIJLVS JJKIIIIIIJLR                                               YNHFEDDDDDCCCCCCCCCCCCCBB
AABCCCCCDDDDDDDDDDDDEEEEEEEFFGGHIJKOU  O O   PR LLJJJKL                                                OIHFFEDDDDDCCCCCCCCCCCCCCB
AACCCDDDDDDDDDDDDDEEEEEEEEEFGGGHIJMR              RMLMN                                                 NTFEEDDDDDDCCCCCCCCCCCCCB
AACCDDDDDDDDDDDDEEEEEEEEEFGGGHHKONSZ                QPR                                                NJGFEEDDDDDDCCCCCCCCCCCCCC
ABCDDDDDDDDDDDEEEEEFFFFFGIPJIIJKMQ                   VX                                                 HFFEEDDDDDDCCCCCCCCCCCCCC
ACDDDDDDDDDDEFFFFFFFGGGGHIKZOOPPS                                                                      HGFEEEDDDDDDCCCCCCCCCCCCCC
ADEEEEFFFGHIGGGGGGHHHHIJJLNY                                                                        TJHGFFEEEDDDDDDDCCCCCCCCCCCCC
A                                                                                                 PLJHGGFFEEEDDDDDDDCCCCCCCCCCCCC
ADEEEEFFFGHIGGGGGGHHHHIJJLNY                                                                        TJHGFFEEEDDDDDDDCCCCCCCCCCCCC
ACDDDDDDDDDDEFFFFFFFGGGGHIKZOOPPS                                                                      HGFEEEDDDDDDCCCCCCCCCCCCCC
ABCDDDDDDDDDDDEEEEEFFFFFGIPJIIJKMQ                   VX                                                 HFFEEDDDDDDCCCCCCCCCCCCCC
AACCDDDDDDDDDDDDEEEEEEEEEFGGGHHKONSZ                QPR                                                NJGFEEDDDDDDCCCCCCCCCCCCCC
AACCCDDDDDDDDDDDDDEEEEEEEEEFGGGHIJMR              RMLMN                                                 NTFEEDDDDDDCCCCCCCCCCCCCB
AABCCCCCDDDDDDDDDDDDEEEEEEEFFGGHIJKOU  O O   PR LLJJJKL                                                OIHFFEDDDDDCCCCCCCCCCCCCCB
AABCCCCCCCCDDDDDDDDDDDEEEEEEFFFHKQMRKNJIJLVS JJKIIIIIIJLR                                               YNHFEDDDDDCCCCCCCCCCCCCBB
AAABCCCCCCCCCCCDDDDDDDDDDEEEEFFHLKHHGGGGHHMJHGGGGGGHHHIKRR                                           UQ L HFEDDDDCCCCCCCCCCCCCCBB
AAABCCCCCCCCCCCCCCCCCDDDDDDDEEFJIHFFFFFFFFFFFFFFGGGGGGHIJN                                            JHHGFEEDDDDCCCCCCCCCCCCCBBB
AAAABCCCCCCCCCCCCCCCCCCCCCCDDDDEEEEEEEEEEEEEEEEFFFFFFGGHYV RQU                                     QMJHGGFEEEDDDCCCCCCCCCCCCCBBBB
AAAABBCCCCCCCCCCCCCCCCCCCCCCCCCDDDDEEEEEEEEEEEEEEEFFFFFFGHIJKLOT                                     [JGFFEEEDDCCCCCCCCCCCCCBBBBB
AAAAABBCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDEEEEEEEEEEEEFFFFFGHHIN                                 Q     UMWGEEEDDDCCCCCCCCCCCCBBBBBB
AAAAABBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDEEEEEEEEEFFFFGH O    TN S                       NKJKR LLQMNHEEDDDCCCCCCCCCCCCBBBBBBB
AAAAAABBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDEEEEEEFFGHK   MKJIJO  N R  X      YUSR PLV LHHHGGHIOJGFEDDDCCCCCCCCCCCCBBBBBBBB
AAAAAAABBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDEEEEEFGGHIIHHHHHIIIJKMR        VMKJIHHHGFFFFFFGSGEDDDDCCCCCCCCCCCCBBBBBBBBB
AAAAAAABBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDEEEEFFFFFFGGGGHIKP           KHHGGFFFFEEEEEEDDDDDCCCCCCCCCCCBBBBBBBBBBB
AAAAAAAABBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDDEEEEEFFFFFGGHJLZ         UKHGFFEEEEEEEEDDDDDCCCCCCCCCCCCBBBBBBBBBBBB
AAAAAAAAABBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDDEEEEEEFFGQPUVOTY   ZQL[MHFEEEEEEEDDDDDDDCCCCCCCCCCCBBBBBBBBBBBBBB
AAAAAAAAAABBBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDDEEEEEEFFGHIJKS  X KHHGFEEEEEDDDDDDDDDCCCCCCCCCCBBBBBBBBBBBBBBBB
AAAAAAAAAAABBBBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDEEEEEEFGGHHIKPPKIHGFFEEEDDDDDDDDDCCCCCCCCCCBBBBBBBBBBBBBBBBBB
AAAAAAAAAAAABBBBBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDDDEEEEEFFGHIMTKLZOGFEEDDDDDDDDDCCCCCCCCCBBBBBBBBBBBBBBBBBBBBB
AAAAAAAAAAAAABBBBBBBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDDDEEEEFFFI KHGGGHGEDDDDDDDDDCCCCCCCCCBBBBBBBBBBBBBBBBBBBBBBB
AAAAAAAAAAAAAAABBBBBBBBBBBBBCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDDDDDDDDDDEEEFGIIGFFEEEDDDDDDDDCCCCCCCCCBBBBBBBBBBBBBBBBBBBBBBBBBB
```



## C



### PPM non interactive

Here is one file program. It directly creates ppm file.

```C
 /* 
 c program:
 --------------------------------
  1. draws Mandelbrot set for Fc(z)=z*z +c
  using Mandelbrot algorithm ( boolean escape time )
 -------------------------------         
 2. technique of creating ppm file is  based on the code of Claudio Rocchini
 http://en.wikipedia.org/wiki/Image:Color_complex_plot.jpg
 create 24 bit color graphic file ,  portable pixmap file = PPM 
 see http://en.wikipedia.org/wiki/Portable_pixmap
 to see the file use external application ( graphic viewer)
  */
 #include <stdio.h>
 #include <math.h>
 int main()
 {
          /* screen ( integer) coordinate */
        int iX,iY;
        const int iXmax = 800; 
        const int iYmax = 800;
        /* world ( double) coordinate = parameter plane*/
        double Cx,Cy;
        const double CxMin=-2.5;
        const double CxMax=1.5;
        const double CyMin=-2.0;
        const double CyMax=2.0;
        /* */
        double PixelWidth=(CxMax-CxMin)/iXmax;
        double PixelHeight=(CyMax-CyMin)/iYmax;
        /* color component ( R or G or B) is coded from 0 to 255 */
        /* it is 24 bit color RGB file */
        const int MaxColorComponentValue=255; 
        FILE * fp;
        char *filename="new1.ppm";
        char *comment="# ";/* comment should start with # */
        static unsigned char color[3];
        /* Z=Zx+Zy*i  ;   Z0 = 0 */
        double Zx, Zy;
        double Zx2, Zy2; /* Zx2=Zx*Zx;  Zy2=Zy*Zy  */
        /*  */
        int Iteration;
        const int IterationMax=200;
        /* bail-out value , radius of circle ;  */
        const double EscapeRadius=2;
        double ER2=EscapeRadius*EscapeRadius;
        /*create new file,give it a name and open it in binary mode  */
        fp= fopen(filename,"wb"); /* b -  binary mode */
        /*write ASCII header to the file*/
        fprintf(fp,"P6\n %s\n %d\n %d\n %d\n",comment,iXmax,iYmax,MaxColorComponentValue);
        /* compute and write image data bytes to the file*/
        for(iY=0;iY<iYmax;iY++)
        {
             Cy=CyMin + iY*PixelHeight;
             if (fabs(Cy)< PixelHeight/2) Cy=0.0; /* Main antenna */
             for(iX=0;iX<iXmax;iX++)
             {         
                        Cx=CxMin + iX*PixelWidth;
                        /* initial value of orbit = critical point Z= 0 */
                        Zx=0.0;
                        Zy=0.0;
                        Zx2=Zx*Zx;
                        Zy2=Zy*Zy;
                        /* */
                        for (Iteration=0;Iteration<IterationMax && ((Zx2+Zy2)<ER2);Iteration++)
                        {
                            Zy=2*Zx*Zy + Cy;
                            Zx=Zx2-Zy2 +Cx;
                            Zx2=Zx*Zx;
                            Zy2=Zy*Zy;
                        };
                        /* compute  pixel color (24 bit = 3 bytes) */
                        if (Iteration==IterationMax)
                        { /*  interior of Mandelbrot set = black */
                           color[0]=0;
                           color[1]=0;
                           color[2]=0;                           
                        }
                     else 
                        { /* exterior of Mandelbrot set = white */
                             color[0]=255; /* Red*/
                             color[1]=255;  /* Green */ 
                             color[2]=255;/* Blue */
                        };
                        /*write color to the file*/
                        fwrite(color,1,3,fp);
                }
        }
        fclose(fp);
        return 0;
 }</lang >


### PPM Interactive

[[file:mandel-C-GL.png|center|400px]]
Infinitely zoomable OpenGL program.  Adjustable colors, max iteration, black and white, screen dump, etc. Compile with <code>gcc mandelbrot.c -lglut -lGLU -lGL -lm</code>

* [[OpenBSD]] users, install freeglut package, and compile with <code>make mandelbrot CPPFLAGS='-I/usr/local/include `pkg-config glu --cflags`' LDLIBS='-L/usr/local/lib -lglut `pkg-config glu --libs` -lm'</code>

{{libheader|GLUT}}

```c>#include <stdio.h

#include <stdlib.h>
#include <math.h>
#include <GL/glut.h>
#include <GL/gl.h>
#include <GL/glu.h>
 
void set_texture();
 
typedef struct {unsigned char r, g, b;} rgb_t;
rgb_t **tex = 0;
int gwin;
GLuint texture;
int width, height;
int tex_w, tex_h;
double scale = 1./256;
double cx = -.6, cy = 0;
int color_rotate = 0;
int saturation = 1;
int invert = 0;
int max_iter = 256;
 
void render()
{
	double	x = (double)width /tex_w,
		y = (double)height/tex_h;
 
	glClear(GL_COLOR_BUFFER_BIT);
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
 
	glBindTexture(GL_TEXTURE_2D, texture);
 
	glBegin(GL_QUADS);
 
	glTexCoord2f(0, 0); glVertex2i(0, 0);
	glTexCoord2f(x, 0); glVertex2i(width, 0);
	glTexCoord2f(x, y); glVertex2i(width, height);
	glTexCoord2f(0, y); glVertex2i(0, height);
 
	glEnd();
 
	glFlush();
	glFinish();
}
 
int dump = 1;
void screen_dump()
{
	char fn[100];
	int i;
	sprintf(fn, "screen%03d.ppm", dump++);
	FILE *fp = fopen(fn, "w");
	fprintf(fp, "P6\n%d %d\n255\n", width, height);
	for (i = height - 1; i >= 0; i--)
		fwrite(tex[i], 1, width * 3, fp);
	fclose(fp);
	printf("%s written\n", fn);
}
 
void keypress(unsigned char key, int x, int y)
{
	switch(key) {
	case 'q':	glFinish();
			glutDestroyWindow(gwin);
			return;
	case 27:	scale = 1./256; cx = -.6; cy = 0; break;
 
	case 'r':	color_rotate = (color_rotate + 1) % 6;
			break;
 
	case '>': case '.':
			max_iter += 128;
			if (max_iter > 1 << 15) max_iter = 1 << 15;
			printf("max iter: %d\n", max_iter);
			break;
 
	case '<': case ',':
			max_iter -= 128;
			if (max_iter < 128) max_iter = 128;
			printf("max iter: %d\n", max_iter);
			break;
 
	case 'c':	saturation = 1 - saturation;
			break;
 
	case 's':	screen_dump(); return;
	case 'z':	max_iter = 4096; break;
	case 'x':	max_iter = 128; break;
	case ' ':	invert = !invert;
	}
	set_texture();
}
 
void hsv_to_rgb(int hue, int min, int max, rgb_t *p)
{
	if (min == max) max = min + 1;
	if (invert) hue = max - (hue - min);
	if (!saturation) {
		p->r = p->g = p->b = 255 * (max - hue) / (max - min);
		return;
	}
	double h = fmod(color_rotate + 1e-4 + 4.0 * (hue - min) / (max - min), 6);
#	define VAL 255
	double c = VAL * saturation;
	double X = c * (1 - fabs(fmod(h, 2) - 1));
 
	p->r = p->g = p->b = 0;
 
	switch((int)h) {
	case 0: p->r = c; p->g = X; return;
	case 1:	p->r = X; p->g = c; return;
	case 2: p->g = c; p->b = X; return;
	case 3: p->g = X; p->b = c; return;
	case 4: p->r = X; p->b = c; return;
	default:p->r = c; p->b = X;
	}
}
 
void calc_mandel()
{
	int i, j, iter, min, max;
	rgb_t *px;
	double x, y, zx, zy, zx2, zy2;
	min = max_iter; max = 0;
	for (i = 0; i < height; i++) {
		px = tex[i];
		y = (i - height/2) * scale + cy;
		for (j = 0; j  < width; j++, px++) {
			x = (j - width/2) * scale + cx;
			iter = 0;
 
			zx = hypot(x - .25, y);
			if (x < zx - 2 * zx * zx + .25) iter = max_iter;
			if ((x + 1)*(x + 1) + y * y < 1/16) iter = max_iter;
 
			zx = zy = zx2 = zy2 = 0;
			for (; iter < max_iter && zx2 + zy2 < 4; iter++) {
				zy = 2 * zx * zy + y;
				zx = zx2 - zy2 + x;
				zx2 = zx * zx;
				zy2 = zy * zy;
			}
			if (iter < min) min = iter;
			if (iter > max) max = iter;
			*(unsigned short *)px = iter;
		}
	}
 
	for (i = 0; i < height; i++)
		for (j = 0, px = tex[i]; j  < width; j++, px++)
			hsv_to_rgb(*(unsigned short*)px, min, max, px);
}
 
void alloc_tex()
{
	int i, ow = tex_w, oh = tex_h;
 
	for (tex_w = 1; tex_w < width;  tex_w <<= 1);
	for (tex_h = 1; tex_h < height; tex_h <<= 1);
 
	if (tex_h != oh || tex_w != ow)
		tex = realloc(tex, tex_h * tex_w * 3 + tex_h * sizeof(rgb_t*));
 
	for (tex[0] = (rgb_t *)(tex + tex_h), i = 1; i < tex_h; i++)
		tex[i] = tex[i - 1] + tex_w;
}
 
void set_texture()
{
	alloc_tex();
	calc_mandel();
 
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, texture);
	glTexImage2D(GL_TEXTURE_2D, 0, 3, tex_w, tex_h,
		0, GL_RGB, GL_UNSIGNED_BYTE, tex[0]);
 
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	render();
}
 
void mouseclick(int button, int state, int x, int y)
{
	if (state != GLUT_UP) return;
 
	cx += (x - width / 2) * scale;
	cy -= (y - height/ 2) * scale;
 
	switch(button) {
	case GLUT_LEFT_BUTTON: /* zoom in */
		if (scale > fabs(x) * 1e-16 && scale > fabs(y) * 1e-16)
			scale /= 2;
		break;
	case GLUT_RIGHT_BUTTON: /* zoom out */
		scale *= 2;
		break;
	/* any other button recenters */
	}
	set_texture();
}
 
 
void resize(int w, int h)
{
	printf("resize %d %d\n", w, h);
	width = w;
	height = h;
 
	glViewport(0, 0, w, h);
	glOrtho(0, w, 0, h, -1, 1);
 
	set_texture();
}
 
void init_gfx(int *c, char **v)
{
	glutInit(c, v);
	glutInitDisplayMode(GLUT_RGB);
	glutInitWindowSize(640, 480);
	glutDisplayFunc(render);
 
	gwin = glutCreateWindow("Mandelbrot");
 
	glutKeyboardFunc(keypress);
	glutMouseFunc(mouseclick);
	glutReshapeFunc(resize);
	glGenTextures(1, &texture);
	set_texture();
}
 
int main(int c, char **v)
{
	init_gfx(&c, v);
	printf("keys:\n\tr: color rotation\n\tc: monochrome\n\ts: screen dump\n\t"
		"<, >: decrease/increase max iteration\n\tq: quit\n\tmouse buttons to zoom\n");
 
	glutMainLoop();
	return 0;
}
```


### ASCII

Not mine, found it on Ken Perlin's homepage, this deserves a place here to illustrate how awesome C can be:

```C

main(k){float i,j,r,x,y=-16;while(puts(""),y++<15)for(x
=0;x++<84;putchar(" .:-;!/>)|&IH%*#"[k&15]))for(i=k=r=0;
j=r*r-i*i-2+x/25,i=2*r*i+y/10,j*j+i*i<11&&k++<111;r=j);}

```

There may be warnings on compiling but disregard them, the output will be produced nevertheless. Such programs are called obfuscated and C excels when it comes to writing such cryptic programs. Google IOCCC for more.

```txt

.............::::::::::::::::::::::::::::::::::::::::::::::::.......................
.........::::::::::::::::::::::::::::::::::::::::::::::::::::::::...................
.....::::::::::::::::::::::::::::::::::-----------:::::::::::::::::::...............
...:::::::::::::::::::::::::::::------------------------:::::::::::::::.............
:::::::::::::::::::::::::::-------------;;;!:H!!;;;--------:::::::::::::::..........
::::::::::::::::::::::::-------------;;;;!!/>&*|I !;;;--------::::::::::::::........
::::::::::::::::::::-------------;;;;;;!!/>)|.*#|>/!!;;;;-------::::::::::::::......
::::::::::::::::-------------;;;;;;!!!!//>|:    !:|//!!!;;;;-----::::::::::::::.....
::::::::::::------------;;;;;;;!!/>)I>>)||I#     H&))>////*!;;-----:::::::::::::....
::::::::----------;;;;;;;;;;!!!//)H:  #|              IH&*I#/;;-----:::::::::::::...
:::::---------;;;;!!!!!!!!!!!//>|.H:                     #I>/!;;-----:::::::::::::..
:----------;;;;!/||>//>>>>//>>)|%                         %|&/!;;----::::::::::::::.
--------;;;;;!!//)& .;I*-H#&||&/                           *)/!;;-----::::::::::::::
-----;;;;;!!!//>)IH:-        ##                            #&!!;;-----::::::::::::::
;;;;!!!!!///>)H%.**           *                            )/!;;;------:::::::::::::
                                                         &)/!!;;;------:::::::::::::
;;;;!!!!!///>)H%.**           *                            )/!;;;------:::::::::::::
-----;;;;;!!!//>)IH:-        ##                            #&!!;;-----::::::::::::::
--------;;;;;!!//)& .;I*-H#&||&/                           *)/!;;-----::::::::::::::
:----------;;;;!/||>//>>>>//>>)|%                         %|&/!;;----::::::::::::::.
:::::---------;;;;!!!!!!!!!!!//>|.H:                     #I>/!;;-----:::::::::::::..
::::::::----------;;;;;;;;;;!!!//)H:  #|              IH&*I#/;;-----:::::::::::::...
::::::::::::------------;;;;;;;!!/>)I>>)||I#     H&))>////*!;;-----:::::::::::::....
::::::::::::::::-------------;;;;;;!!!!//>|:    !:|//!!!;;;;-----::::::::::::::.....
::::::::::::::::::::-------------;;;;;;!!/>)|.*#|>/!!;;;;-------::::::::::::::......
::::::::::::::::::::::::-------------;;;;!!/>&*|I !;;;--------::::::::::::::........
:::::::::::::::::::::::::::-------------;;;!:H!!;;;--------:::::::::::::::..........
...:::::::::::::::::::::::::::::------------------------:::::::::::::::.............
.....::::::::::::::::::::::::::::::::::-----------:::::::::::::::::::...............
.........::::::::::::::::::::::::::::::::::::::::::::::::::::::::...................
.............::::::::::::::::::::::::::::::::::::::::::::::::.......................

```



## C++

This generic function assumes that the image can be accessed like a two-dimensional array of colors. It may be passed a true array (in which case the Mandelbrot set will simply be drawn into that array, which then might be saved as image file), or a class which maps the subscript operator to the pixel drawing routine of some graphics library. In the latter case, there must be functions get_first_dimension and get_second_dimension defined for that type, to be found by argument dependent lookup. The code provides those functions for built-in arrays.

```cpp>#include <cstdlib

#include <complex>

// get dimensions for arrays
template<typename ElementType, std::size_t dim1, std::size_t dim2>
 std::size_t get_first_dimension(ElementType (&a)[dim1][dim2])
{
  return dim1;
}

template<typename ElementType, std::size_t dim1, std::size_t dim2>
 std::size_t get_second_dimension(ElementType (&a)[dim1][dim2])
{
  return dim2;
}


template<typename ColorType, typename ImageType>
 void draw_Mandelbrot(ImageType& image,                                   //where to draw the image
                      ColorType set_color, ColorType non_set_color,       //which colors to use for set/non-set points
                      double cxmin, double cxmax, double cymin, double cymax,//the rect to draw in the complex plane
                      unsigned int max_iterations)                          //the maximum number of iterations
{
  std::size_t const ixsize = get_first_dimension(image);
  std::size_t const iysize = get_first_dimension(image);
  for (std::size_t ix = 0; ix < ixsize; ++ix)
    for (std::size_t iy = 0; iy < iysize; ++iy)
    {
      std::complex<double> c(cxmin + ix/(ixsize-1.0)*(cxmax-cxmin), cymin + iy/(iysize-1.0)*(cymax-cymin));
      std::complex<double> z = 0;
      unsigned int iterations;

      for (iterations = 0; iterations < max_iterations && std::abs(z) < 2.0; ++iterations) 
        z = z*z + c;

      image[ix][iy] = (iterations == max_iterations) ? set_color : non_set_color;

    }
}
```


Note this code has not been executed.


## Cixl

Displays a zooming Mandelbrot using ANSI graphics.


```cixl

use: cx;

define: max 4.0;
define: max-iter 570;

let: (max-x max-y) screen-size;
let: max-cx $max-x 2.0 /;
let: max-cy $max-y 2.0 /;
let: rows Stack<Str> new;
let: buf Buf new;
let: zoom 0 ref;

func: render()()
  $rows clear
  
  $max-y 2 / {
    let: y;
    $buf 0 seek

    $max-x {
      let: x;
      let: (zx zy) 0.0 ref %%;
      let: cx $x $max-cx - $zoom deref /;
      let: cy $y $max-cy - $zoom deref /;
      let: i #max-iter ref;

      {
        let: nzx $zx deref ** $zy deref ** - $cx +;
	$zy $zx deref *2 $zy deref * $cy + set
	$zx $nzx set
        $i &-- set-call	
        $nzx ** $zy deref ** + #max < $i deref and
      } while

      let: c $i deref % -7 bsh bor 256 mod;
      $c {$x 256 mod $y 256 mod} {0 0} if-else $c new-rgb $buf set-bg
      @@s $buf print
    } for

    $rows $buf str push   
  } for

  1 1 #out move-to
  $rows {#out print} for
  $rows riter {#out print} for;

#out hide-cursor
raw-mode

let: poll Poll new;
let: is-done #f ref;

$poll #in {
  #in read-char _
  $is-done #t set
} on-read

{
  $zoom &++ set-call
  render
  $poll 0 wait _
  $is-done deref !
} while

#out reset-style
#out clear-screen
1 1 #out move-to
#out show-cursor
normal-mode

```


=={{header|C sharp|C#}}==

```csharp
using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Threading;
using System.Windows.Forms;

/// <summary>
/// Generates bitmap of Mandelbrot Set and display it on the form.
/// </summary>
public class MandelbrotSetForm : Form
{
    const double MaxValueExtent = 2.0;
    Thread thread;

    static double CalcMandelbrotSetColor(ComplexNumber c)
    {
        // from http://en.wikipedia.org/w/index.php?title=Mandelbrot_set
        const int MaxIterations = 1000;
        const double MaxNorm = MaxValueExtent * MaxValueExtent;

        int iteration = 0;
        ComplexNumber z = new ComplexNumber();
        do
        {
            z = z * z + c;
            iteration++;
        } while (z.Norm() < MaxNorm && iteration < MaxIterations);
        if (iteration < MaxIterations)
            return (double)iteration / MaxIterations;
        else
            return 0; // black
    }

    static void GenerateBitmap(Bitmap bitmap)
    {
        double scale = 2 * MaxValueExtent / Math.Min(bitmap.Width, bitmap.Height);
        for (int i = 0; i < bitmap.Height; i++)
        {
            double y = (bitmap.Height / 2 - i) * scale;
            for (int j = 0; j < bitmap.Width; j++)
            {
                double x = (j - bitmap.Width / 2) * scale;
                double color = CalcMandelbrotSetColor(new ComplexNumber(x, y));
                bitmap.SetPixel(j, i, GetColor(color));
            }
        }
    }

    static Color GetColor(double value)
    {
        const double MaxColor = 256;
        const double ContrastValue = 0.2;
        return Color.FromArgb(0, 0,
            (int)(MaxColor * Math.Pow(value, ContrastValue)));
    }
    
    public MandelbrotSetForm()
    {
        // form creation
        this.Text = "Mandelbrot Set Drawing";
        this.BackColor = System.Drawing.Color.Black;
        this.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch;
        this.MaximizeBox = false;
        this.StartPosition = FormStartPosition.CenterScreen;
        this.FormBorderStyle = FormBorderStyle.FixedDialog;
        this.ClientSize = new Size(640, 640);
        this.Load += new System.EventHandler(this.MainForm_Load);
    }

    void MainForm_Load(object sender, EventArgs e)
    {
        thread = new Thread(thread_Proc);
        thread.IsBackground = true;
        thread.Start(this.ClientSize);
    }

    void thread_Proc(object args)
    {
        // start from small image to provide instant display for user
        Size size = (Size)args;
        int width = 16;
        while (width * 2 < size.Width)
        {
            int height = width * size.Height / size.Width;
            Bitmap bitmap = new Bitmap(width, height, PixelFormat.Format24bppRgb);
            GenerateBitmap(bitmap);
            this.BeginInvoke(new SetNewBitmapDelegate(SetNewBitmap), bitmap);
            width *= 2;
            Thread.Sleep(200);
        }
        // then generate final image
        Bitmap finalBitmap = new Bitmap(size.Width, size.Height, PixelFormat.Format24bppRgb);
        GenerateBitmap(finalBitmap);
        this.BeginInvoke(new SetNewBitmapDelegate(SetNewBitmap), finalBitmap);
    }

    void SetNewBitmap(Bitmap image)
    {
        if (this.BackgroundImage != null)
            this.BackgroundImage.Dispose();
        this.BackgroundImage = image;
    }

    delegate void SetNewBitmapDelegate(Bitmap image);

    static void Main()
    {
        Application.Run(new MandelbrotSetForm());
    }
}

struct ComplexNumber
{
    public double Re;
    public double Im;

    public ComplexNumber(double re, double im)
    {
        this.Re = re;
        this.Im = im;
    }

    public static ComplexNumber operator +(ComplexNumber x, ComplexNumber y)
    {
        return new ComplexNumber(x.Re + y.Re, x.Im + y.Im);
    }

    public static ComplexNumber operator *(ComplexNumber x, ComplexNumber y)
    {
        return new ComplexNumber(x.Re * y.Re - x.Im * y.Im,
            x.Re * y.Im + x.Im * y.Re);
    }

    public double Norm()
    {
        return Re * Re + Im * Im;
    }
}
```



## Clojure

{{trans|Perl}}

```lisp
(ns mandelbrot
  (:refer-clojure :exclude [+ * <])
  (:use (clojure.contrib complex-numbers)
        (clojure.contrib.generic [arithmetic :only [+ *]]
                                 [comparison :only [<]]
                                 [math-functions :only [abs]])))
(defn mandelbrot? [z]
  (loop [c 1
         m (iterate #(+ z (* % %)) 0)]
    (if (and (> 20 c)
             (< (abs (first m)) 2) )
      (recur (inc c)
             (rest m))
      (if (= 20 c) true false))))

(defn mandelbrot []
  (for [y (range 1 -1 -0.05)
	x (range -2 0.5 0.0315)] 
    (if (mandelbrot? (complex x y)) "#" " ")))

(println (interpose \newline (map #(apply str %) (partition 80 (mandelbrot)))))

```



## COBOL

EBCDIC art.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. MANDELBROT-SET-PROGRAM.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  COMPLEX-ARITHMETIC.
    05 X               PIC S9V9(9).
    05 Y               PIC S9V9(9).
    05 X-A             PIC S9V9(6).
    05 X-B             PIC S9V9(6).
    05 Y-A             PIC S9V9(6).
    05 X-A-SQUARED     PIC S9V9(6).
    05 Y-A-SQUARED     PIC S9V9(6).
    05 SUM-OF-SQUARES  PIC S9V9(6).
    05 ROOT            PIC S9V9(6).
01  LOOP-COUNTERS.
    05 I               PIC 99.
    05 J               PIC 99.
    05 K               PIC 999.
77  PLOT-CHARACTER     PIC X.
PROCEDURE DIVISION.
CONTROL-PARAGRAPH.
    PERFORM OUTER-LOOP-PARAGRAPH
    VARYING I FROM 1 BY 1 UNTIL I IS GREATER THAN 24.
    STOP RUN.
OUTER-LOOP-PARAGRAPH.
    PERFORM INNER-LOOP-PARAGRAPH
    VARYING J FROM 1 BY 1 UNTIL J IS GREATER THAN 64.
    DISPLAY ''.
INNER-LOOP-PARAGRAPH.
    MOVE SPACE TO PLOT-CHARACTER.
    MOVE ZERO  TO X-A.
    MOVE ZERO  TO Y-A.
    MULTIPLY J   BY   0.0390625   GIVING X.
    SUBTRACT 1.5 FROM X.
    MULTIPLY I   BY   0.083333333 GIVING Y.
    SUBTRACT 1 FROM Y.
    PERFORM ITERATION-PARAGRAPH VARYING K FROM 1 BY 1
    UNTIL K IS GREATER THAN 100 OR PLOT-CHARACTER IS EQUAL TO '#'.
    DISPLAY PLOT-CHARACTER WITH NO ADVANCING.
ITERATION-PARAGRAPH.
    MULTIPLY X-A BY X-A GIVING X-A-SQUARED.
    MULTIPLY Y-A BY Y-A GIVING Y-A-SQUARED.
    SUBTRACT Y-A-SQUARED FROM X-A-SQUARED GIVING X-B.
    ADD      X   TO X-B.
    MULTIPLY X-A BY Y-A GIVING Y-A.
    MULTIPLY Y-A BY 2   GIVING Y-A.
    SUBTRACT Y   FROM Y-A.
    MOVE     X-B TO   X-A.
    ADD X-A-SQUARED TO Y-A-SQUARED GIVING SUM-OF-SQUARES.
    MOVE FUNCTION SQRT (SUM-OF-SQUARES) TO ROOT.
    IF ROOT IS GREATER THAN 2 THEN MOVE '#' TO PLOT-CHARACTER.
```

{{out}}

```txt

################################################################
#################################   ############################
################################     ###########################
############################## ##   ############################
########################  #               ######################
########################                      ##################
#####################                          #################
####################                             ###############
######## ##    #####                            ################
#######           #                             ################
######            #                            #################
                                            ####################
######            #                            #################
#######           #                             ################
######## ##    #####                            ################
####################                             ###############
#####################                          #################
########################                      ##################
########################  #               ######################
############################## ##   ############################
################################     ###########################
#################################   ############################
################################################################
################################################################
```



## Common Lisp


```lisp
(defpackage #:mandelbrot
  (:use #:cl))

(in-package #:mandelbrot)

(deftype pixel () '(unsigned-byte 8))
(deftype image () '(array pixel))

(defun write-pgm (image filespec)
  (declare (image image))
  (with-open-file (s filespec :direction :output :element-type 'pixel :if-exists :supersede)
    (let* ((width  (array-dimension image 1))
           (height (array-dimension image 0))
           (header (format nil "P5~A~D ~D~A255~A" #\Newline width height #\Newline #\Newline)))
      (loop for c across header
            do (write-byte (char-code c) s))
      (dotimes (row height)
        (dotimes (col width)
          (write-byte (aref image row col) s))))))

(defparameter *x-max* 800)
(defparameter *y-max* 800)
(defparameter *cx-min* -2.5)
(defparameter *cx-max* 1.5)
(defparameter *cy-min* -2.0)
(defparameter *cy-max* 2.0)
(defparameter *escape-radius* 2)
(defparameter *iteration-max* 40)

(defun mandelbrot (filespec)
  (let ((pixel-width  (/ (- *cx-max* *cx-min*) *x-max*))
        (pixel-height (/ (- *cy-max* *cy-min*) *y-max*))
        (image (make-array (list *y-max* *x-max*) :element-type 'pixel :initial-element 0)))
    (loop for y from 0 below *y-max*
          for cy from *cy-min* by pixel-height
          do (loop for x from 0 below *x-max*
                   for cx from *cx-min* by pixel-width
                   for iteration = (loop with c = (complex cx cy)
                                         for iteration from 0 below *iteration-max*
                                         for z = c then (+ (* z z) c)
                                         while (< (abs z) *escape-radius*)
                                         finally (return iteration))
                   for pixel = (round (* 255 (/ (- *iteration-max* iteration) *iteration-max*)))
                   do (setf (aref image y x) pixel)))
    (write-pgm image filespec)))
```



## D


### Textual Version

This uses <code>std.complex</code> because D built-in complex numbers are deprecated.

```d
void main() {
    import std.stdio, std.complex;

    for (real y = -1.2; y < 1.2; y += 0.05) {
        for (real x = -2.05; x < 0.55; x += 0.03) {
            auto z = 0.complex;
            foreach (_; 0 .. 100)
                z = z ^^ 2 + complex(x, y);
            write(z.abs < 2 ? '#' : '.');
        }
        writeln;
    }
}
```

{{out}}

```txt
.......................................................................................
.......................................................................................
.......................................................................................
.......................................................................................
.......................................................................................
.......................................................................................
.......................................................................................
................................................................##.....................
.............................................................######....................
.............................................................#######...................
..............................................................######...................
..........................................................#.#.###..#.#.................
...................................................##....################..............
..................................................###.######################.###.......
...................................................############################........
................................................###############################........
................................................################################.......
.............................................#####################################.....
..............................................###################################......
..............................##.####.#......####################################......
..............................###########....####################################......
............................###############.######################################.....
............................###############.#####################################......
........................##.#####################################################.......
......#.#####################################################################..........
........................##.#####################################################.......
............................###############.#####################################......
............................###############.######################################.....
..............................###########....####################################......
..............................##.####.#......####################################......
..............................................###################################......
.............................................#####################################.....
................................................################################.......
................................................###############################........
...................................................############################........
..................................................###.######################.###.......
...................................................##....################..............
..........................................................#.#.###..#.#.................
..............................................................######...................
.............................................................#######...................
.............................................................######....................
................................................................##.....................
.......................................................................................
.......................................................................................
.......................................................................................
.......................................................................................
.......................................................................................
.......................................................................................
.......................................................................................
```



### More Functional Textual Version

The output is similar.

```d
void main() {
    import std.stdio, std.complex, std.range, std.algorithm;

    foreach (immutable y; iota(-1.2, 1.2, 0.05))
        iota(-2.05, 0.55, 0.03).map!(x => 0.complex
            .recurrence!((a, n) => a[n - 1] ^^ 2 + complex(x, y))
            .drop(100).front.abs < 2 ? '#' : '.').writeln;
}
```



### Graphical Version

{{libheader|QD}} {{libheader|SDL}} {{libheader|Phobos}}

```d
import qd;

double lensqr(cdouble c) { return c.re * c.re + c.im * c.im; }

const Limit = 150;

void main() {
  screen(640, 480);
  for (int y = 0; y < screen.h; ++y) {
    flip; events;
    for (int x = 0; x < screen.w; ++x) {
      auto
        c_x = x * 1.0 / screen.w - 0.5,
        c_y = y * 1.0 / screen.h - 0.5,
        c = c_y * 2.0i + c_x * 3.0 - 1.0,
        z = 0.0i + 0.0,
        i = 0;
      for (; i < Limit; ++i) {
        z = z * z + c;
        if (lensqr(z) > 4) break;
      }
      auto value = cast(ubyte) (i * 255.0 / Limit);
      pset(x, y, rgb(value, value, value));
    }
  }
  while (true) { flip; events; }
}
```



## Dart

Implementation in Google Dart works on http://try.dartlang.org/ (as of 10/18/2011) since the language is very new, it may break in the future.
The implementation uses a incomplete Complex class supporting operator overloading.
<lang>class Complex {
  double _r,_i;

  Complex(this._r,this._i);
  double get r => _r;
  double get i => _i;
  String toString() => "($r,$i)";

  Complex operator +(Complex other) => new Complex(r+other.r,i+other.i);
  Complex operator *(Complex other) =>
      new Complex(r*other.r-i*other.i,r*other.i+other.r*i);
  double abs() => r*r+i*i;
}

void main() {
  double start_x=-1.5;
  double start_y=-1.0;
  double step_x=0.03;
  double step_y=0.1;

  for(int y=0;y<20;y++) {
    String line="";
    for(int x=0;x<70;x++) {
      Complex c=new Complex(start_x+step_x*x,start_y+step_y*y);
      Complex z=new Complex(0.0, 0.0);
      for(int i=0;i<100;i++) {
        z=z*(z)+c;
        if(z.abs()>2) {
          break;
        }
      }
      line+=z.abs()>2 ? " " : "*";
    }
    print(line);
  }
}
```



## Dc


### ASCII output

{{works with|GNU Dc}}
{{works with|OpenBSD Dc}}

This can be done in a more Dc-ish way, e.g. by moving the loop macros' definitions to the initialisations in the top instead of saving the macro definition of inner loops over and over again in outer loops.


```dc
 _2.1 sx # xmin = -2.1
  0.7 sX # xmax =  0.7

 _1.2 sy # ymin = -1.2
  1.2 sY # ymax =  1.2

   32 sM # maxiter = 32

   80 sW # image width
   25 sH # image height

    8 k  # precision

[ q ] sq # quitter helper macro

# for h from 0 to H-1
0 sh
[
  lh lH =q # quit if H reached

  # for w from 0 to W-1
  0 sw
  [
    lw lW =q # quit if W reached

    # (w,h) -> (R,I)
    #           | |
    #           | ymin + h*(ymax-ymin)/(height-1)
    #           xmin + w*(xmax-xmin)/(width-1)

    lX lx - lW 1 - / lw * lx + sR
    lY ly - lH 1 - / lh * ly + sI

    # iterate for (R,I)

    0 sr #     r:=0
    0 si #     i:=0
    0 sa #     a:=0 (r squared)
    0 sb #     b:=0 (i squared)
    0 sm #     m:=0

    # do while m!=M and a+b=<4
    [
      lm lM =q # exit if m==M
      la lb + 4<q # exit if >4

      2 lr * li * lI + si # i:=2*r*i+I
      la lb - lR + sr     # r:=a-b+R
      lm 1 + sm           # m+=1
      lr 2 ^ sa           # a:=r*r
      li 2 ^ sb           # b:=i*i

      l0 x                # loop
    ] s0
    l0 x

    lm 32 + P             # print "pixel"

    lw 1 + sw             # w+=1
    l1 x                  # loop
  ] s1
  l1 x

  A P                     # linefeed

  lh 1 + sh               # h+=1
  l2 x                    # loop
] s2
l2 x
```

{{out}}

```txt

!!!!!!!!!!!!!!!"""""""""""""####################################""""""""""""""""
!!!!!!!!!!!!!"""""""""#######################$$$$$$$%'0(%%%$$$$$#####"""""""""""
!!!!!!!!!!!"""""""#######################$$$$$$$$%%%&&(++)++&$$$$$$$######""""""
!!!!!!!!!"""""#######################$$$$$$$$$$%%%%&')*@;/*('&%%$$$$$$#######"""
!!!!!!!!""""#####################$$$$$$$$$$%%%&&&''),@@@@@@@+'&%%%%%$$$$########
!!!!!!!"""####################$$$$$$$$%%%&'())((())*-@@@@@@.+))('&&&&+&%$$######
!!!!!!""###################$$$$$%%%%%%&&&'+.@@@08@@@@@@@@@@@@@@@/+,@//@)%%$#####
!!!!!"################$$$%%%%%%%%%%&&&&')-+7@@@@@@@@@@@@@@@@@@@@@@@@@4(&&%$$####
!!!!"##########$$$$$%%&(,('''''''''''((*-5@@@@@@@@@@@@@@@@@@@@@@@@@@@3+)4&%$$###
!!!!####$$$$$$$$%%%%%&'(*-@1.+/@-4+))**@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@3+'&%$$$##
!!!!#$$$$$$$$$%%%%%%'''++.7@@@@@@@@@9/0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@<6'%%$$$$#
!!!#$$$$$$$%&&&&''().-2.6@@@@@@@@@@@@@>@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'&%%$$$$#
!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2+)'&&%%$$$$#
!!!#$$$$$$$%&&&&''().-2.6@@@@@@@@@@@@@>@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@'&%%$$$$#
!!!!#$$$$$$$$$%%%%%%'''++.7@@@@@@@@@9/0@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@<6'%%$$$$#
!!!!####$$$$$$$$%%%%%&'(*-@1.+/@-4+))**@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@3+'&%$$$##
!!!!"##########$$$$$%%&(,('''''''''''((*-5@@@@@@@@@@@@@@@@@@@@@@@@@@@3+)4&%$$###
!!!!!"################$$$%%%%%%%%%%&&&&')-+7@@@@@@@@@@@@@@@@@@@@@@@@@4(&&%$$####
!!!!!!""###################$$$$$%%%%%%&&&'+.@@@08@@@@@@@@@@@@@@@/+,@//@)%%$#####
!!!!!!!"""####################$$$$$$$$%%%&'())((())*-@@@@@@.+))('&&&&+&%$$######
!!!!!!!!""""#####################$$$$$$$$$$%%%&&&''),@@@@@@@+'&%%%%%$$$$########
!!!!!!!!!"""""#######################$$$$$$$$$$%%%%&')*@;/*('&%%$$$$$$#######"""
!!!!!!!!!!!"""""""#######################$$$$$$$$%%%&&(++)++&$$$$$$$######""""""
!!!!!!!!!!!!!"""""""""#######################$$$$$$$%'0(%%%$$$$$#####"""""""""""
!!!!!!!!!!!!!!!"""""""""""""####################################""""""""""""""""

```


===PGM (P5) output===
This is a condensed version of the ASCII output variant modified to generate a PGM (P5) image.

```dc
_2.1 sx   0.7 sX  _1.2 sy   1.2 sY
32 sM
640 sW 480 sH
8 k
[P5] P A P
lW n 32 P lH n A P
lM 1 - n A P
[ q ] sq
0 sh
[
  lh lH =q
  0 sw
  [
    lw lW =q
    lX lx - lW 1 - / lw * lx + sR
    lY ly - lH 1 - / lh * ly + sI
    0 sr 0 si 0 sa 0 sb 0 sm
    [
      lm lM =q
      la lb + 4<q
      2 lr * li * lI + si
      la lb - lR + sr
      lm 1 + sm
      lr 2 ^ sa
      li 2 ^ sb
      l0 x
    ] s0
    l0 x
    lm 1 - P
    lw 1 + sw
    l1 x
  ] s1
  l1 x
  lh 1 + sh
  l2 x
] s2
l2 x
```


=={{header|DEC BASIC-PLUS}}==
Works under RSTS/E v7.0 on the [[wp:SIMH|simh]] PDP-11 emulator. For installation procedures for RSTS/E, see [http://www.eecis.udel.edu/~mader/delta/downloadrsts.html here].
<lang>10 X1=59\Y1=21
20 I1=-1.0\I2=1.0\R1=-2.0\R2=1.0
30 S1=(R2-R1)/X1\S2=(I2-I1)/Y1
40 FOR Y=0 TO Y1
50 I3=I1+S2*Y
60 FOR X=0 TO X1
70 R3=R1+S1*X\Z1=R3\Z2=I3
80 FOR N=0 TO 30
90 A=Z1*Z1\B=Z2*Z2
100 IF A+B>4.0 THEN GOTO 130
110 Z2=2*Z1*Z2+I3\Z1=A-B+R3
120 NEXT N
130 PRINT STRING$(1%,62%-N);
140 NEXT X
150 PRINT
160 NEXT Y
170 END

```

{{out}}

```txt
>>>>>>
### ==<<<<<<<<<<<<<<<;;;;;;:::96032:;;;;<<<<========

>>>>>===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### ==

>>>>===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<
### =

>>>==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<=====
>>==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<====
>>=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<===
>=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<==
><<<<;;;;;:::972456-567763                      +9;;<<<<<<<=
><;;;;;;::::9875&      .3                       *9;;;<<<<<<=
>;;;;;;::997564'        '                       8:;;;<<<<<<=
>::988897735/                                 &89:;;;<<<<<<=
>::988897735/                                 &89:;;;<<<<<<=
>;;;;;;::997564'        '                       8:;;;<<<<<<=
><;;;;;;::::9875&      .3                       *9;;;<<<<<<=
><<<<;;;;;:::972456-567763                      +9;;<<<<<<<=
>=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<==
>>=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<===
>>==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<====
>>>==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<=====
>>>>===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<
### =

>>>>>===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### ==

>>>>>>
### ==<<<<<<<<<<<<<<<;;;;;;:::96032:;;;;<<<<========

```



## DWScript

{{trans|D}}

```delphi
const maxIter = 256;

var x, y, i : Integer;
for y:=-39 to 39 do begin
   for x:=-39 to 39 do begin
      var c := Complex(y/40-0.5, x/40);
      var z := Complex(0, 0);
      for i:=1 to maxIter do begin
         z := z*z + c;
         if Abs(z)>=4 then Break;
      end;
      if i>=maxIter then
         Print('#')
      else Print('.');
    end;
    PrintLn('');
end;
```



## EasyLang


[https://easylang.online/apps/mandelbrot.html Run it]

<lang>floatvars
for y% range 300
  cy = (y% - 150) / 120
  for x% range 300
    cx = (x% - 220) / 120
    n% = 0
    x = 0
    y = 0
    while x * x + y * y < 4 and n% < 128
      h = x * x - y * y + cx
      y = 2 * x * y + cy
      x = h
      n% += 1
    .
    if n% = 128
      color_red 0
    else
      color_red n% / 16
    .
    move x% / 3 y% / 3
    rect 0.4 0.4
  .
.
```



## eC

[[File:Mandelbrot4.png]]

[http://ecere.com/apps/mandelbrot/ (Try it in a WebApp)]

Drawing code:

```eC
void drawMandelbrot(Bitmap bmp, float range, Complex center, ColorAlpha * palette, int nPalEntries, int nIterations, float scale)
{
   int x, y;
   int w = bmp.width, h = bmp.height;
   ColorAlpha * picture = (ColorAlpha *)bmp.picture;
   double logOf2 = log(2);
   Complex d
   {
      w > h ? range : range * w / h,
      h > w ? range : range * h / w
   };
   Complex C0 { center.a - d.a/2, center.b - d.b/2 };
   Complex C = C0;
   double delta = d.a / w;

   for(y = 0; y < h; y++, C.a = C0.a, C.b += delta)
   {
      for(x = 0; x < w; x++, picture++, C.a += delta)
      {
         Complex Z { };
         int i;
         double ii = 0;
         bool out = false;
         double Za2 = Z.a * Z.a, Zb2 = Z.b * Z.b;
         for(i = 0; i < nIterations; i++)
         {
            double z2;
            Z = { Za2 - Zb2, 2*Z.a*Z.b };
            Z.a += C.a;
            Z.b += C.b;
            Za2 = Z.a * Z.a, Zb2 = Z.b * Z.b;
            z2 = Za2 + Zb2;

            if(z2 >= 2*2)
            {
               ii = (double)(i + 1 - log(0.5 * log(z2)) / logOf2);
               out = true;
               break;
            }
         }
         if(out)
         {
            float si = (float)(ii * scale);
            int i0 = ((int)si) % nPalEntries;
            *picture = palette[i0];
         }
         else
            *picture = black;
      }
   }
}
```

Interactive class with Rubberband Zoom:

```eC
class Mandelbrot : Window
{
   caption = $"Mandelbrot";
   borderStyle = sizable;
   hasMaximize = true;
   hasMinimize = true;
   hasClose = true;
   clientSize = { 600, 600 };

   Point mouseStart, mouseEnd;
   bool dragging;
   bool needUpdate;

   float scale;
   int nIterations; nIterations = 256;
   ColorAlpha * palette;
   int nPalEntries;
   Complex center { -0.75, 0 };

   float range; range = 4;
   Bitmap bmp { };

   Mandelbrot()
   {
      static ColorKey keys[] =
      {
         { navy, 0.0f },
         { Color { 146, 213, 237 }, 0.198606268f },
         { white, 0.3f },
         { Color { 255, 255, 124 }, 0.444250882f },
         { Color { 255, 100, 0 }, 0.634146333f },
         { navy, 1 }
      };

      nPalEntries = 30000;
      palette = new ColorAlpha[nPalEntries];
      scale = nPalEntries / 175.0f;
      PaletteGradient(palette, nPalEntries, keys, sizeof(keys)/sizeof(keys[0]), 1.0);
      needUpdate = true;
   }

   ~Mandelbrot() { delete palette; }

   void OnRedraw(Surface surface)
   {
      if(needUpdate)
      {
         drawMandelbrot(bmp, range, center, palette, nPalEntries, nIterations, scale);
         needUpdate = false;
      }
      surface.Blit(bmp, 0,0, 0,0, bmp.width, bmp.height);

      if(dragging)
      {
         surface.foreground = lime;
         surface.Rectangle(mouseStart.x, mouseStart.y, mouseEnd.x, mouseEnd.y);
      }
   }

   bool OnLeftButtonDown(int x, int y, Modifiers mods)
   {
      mouseEnd = mouseStart = { x, y };
      Capture();
      dragging = true;
      Update(null);
      return true;
   }

   bool OnLeftButtonUp(int x, int y, Modifiers mods)
   {
      if(dragging)
      {
         int dx = Abs(mouseEnd.x - mouseStart.x), dy = Abs(mouseEnd.y - mouseStart.y);
         if(dx > 4 && dy > 4)
         {
            int w = clientSize.w, h = clientSize.h;
            float rangeX = w > h ? range : range * w / h;
            float rangeY = h > w ? range : range * h / w;

            center.a += ((mouseStart.x + mouseEnd.x) - w) / 2.0f * rangeX / w;
            center.b += ((mouseStart.y + mouseEnd.y) - h) / 2.0f * rangeY / h;

            range = dy > dx ? dy * range / h : dx * range / w;

            needUpdate = true;
            Update(null);
         }
         ReleaseCapture();
         dragging = false;
      }
      return true;
   }

   bool OnMouseMove(int x, int y, Modifiers mods)
   {
      if(dragging)
      {
         mouseEnd = { x, y };
         Update(null);
      }
      return true;
   }

   bool OnRightButtonDown(int x, int y, Modifiers mods)
   {
      range = 4;
      nIterations = 256;
      center = { -0.75, 0 };
      needUpdate = true;
      Update(null);
      return true;
   }

   void OnResize(int width, int height)
   {
      bmp.Allocate(null, width, height, 0, pixelFormat888, false);
      needUpdate = true;
      Update(null);
   }

   bool OnKeyHit(Key key, unichar ch)
   {
      switch(key)
      {
         case space: case keyPadPlus: case plus:
            nIterations += 256;
            needUpdate = true;
            Update(null);
            break;
      }
      return true;
   }
}

Mandelbrot mandelbrotForm {};
```



## EchoLisp


```scheme

(lib 'math) ;; fractal function
(lib 'plot)

;; (fractal z zc n) iterates z := z^2 + c, n times
;; 100 iterations
(define (mset z) (if (= Infinity (fractal 0 z 100)) Infinity z))

;; plot function argument inside square (-2 -2), (2,2)
(plot-z-arg mset -2 -2)

;; result here [http://www.echolalie.org/echolisp/help.html#fractal]

```



## Elixir


```elixir
defmodule Mandelbrot do
  def set do
    xsize = 59
    ysize = 21
    minIm = -1.0
    maxIm = 1.0
    minRe = -2.0
    maxRe = 1.0
    stepX = (maxRe - minRe) / xsize
    stepY = (maxIm - minIm) / ysize
    Enum.each(0..ysize, fn y ->
      im = minIm + stepY * y
      Enum.map(0..xsize, fn x ->
        re = minRe + stepX * x
        62 - loop(0, re, im, re, im, re*re+im*im)
      end) |> IO.puts
    end)
  end
  
  defp loop(n, _, _, _, _, _) when n>=30, do: n
  defp loop(n, _, _, _, _, v) when v>4.0, do: n-1
  defp loop(n, re, im, zr, zi, _) do
    a = zr * zr
    b = zi * zi
    loop(n+1, re, im, a-b+re, 2*zr*zi+im, a+b)
  end
end

Mandelbrot.set
```


{{out}}

```txt

??????
### ==<<<<<<<<<<<<<<<;;;;;;:::96032:;;;;<<<<========

?????===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### ==

????===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<
### =

???==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<=====
??==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<====
??=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<===
?=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<==
?<<<<;;;;;:::972456-567763                      +9;;<<<<<<<=
?<;;;;;;::::9875&      .3                       *9;;;<<<<<<=
?;;;;;;::997564'        '                       8:;;;<<<<<<=
?::988897735/                                 &89:;;;<<<<<<=
?::988897735/                                 &89:;;;<<<<<<=
?;;;;;;::997564'        '                       8:;;;<<<<<<=
?<;;;;;;::::9875&      .3                       *9;;;<<<<<<=
?<<<<;;;;;:::972456-567763                      +9;;<<<<<<<=
?=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<==
??=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<===
??==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<====
???==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<=====
????===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<
### =

?????===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### ==

??????
### ==<<<<<<<<<<<<<<<;;;;;;:::96032:;;;;<<<<========


```



## Erlang

{{trans|Haskell}}

Function ''seq_float/2'' is copied from  [https://gist.github.com/andruby/241489 Andrew Fecheyr's GitHubGist]. 

Using module complex from [https://github.com/ghulette/mandelbrot-erlang/blob/master/simple/complex.erl Geoff Hulette's GitHub repository]

[https://github.com/ghulette/mandelbrot-erlang Geoff Hulette's GitHub repository] provides two alternative implementations which are very interesting.


```erlang

-module(mandelbrot).

-export([test/0]).

magnitude(Z) ->
  R = complex:real(Z),
  I = complex:imaginary(Z),
  R * R + I * I.

mandelbrot(A, MaxI, Z, I) ->
    case (I < MaxI) and (magnitude(Z) < 2.0) of
        true ->
            NZ = complex:add(complex:mult(Z, Z), A),
            mandelbrot(A, MaxI, NZ, I + 1);
        false ->
            case I of 
                MaxI ->
                    $*;
                _ ->
                    $ 
            end
    end.

test() ->
    lists:map(
        fun(S) -> io:format("~s",[S]) end, 
        [
            [
                begin 
                    Z = complex:make(X, Y),
                    mandelbrot(Z, 50, Z, 1)
                end
            || X <- seq_float(-2, 0.5, 0.0315)
            ] ++ "\n"
        || Y <- seq_float(-1,1, 0.05)
        ] ),
    ok.

% **************************************************
% Copied from https://gist.github.com/andruby/241489
% **************************************************

seq_float(Min, Max, Inc, Counter, Acc) when (Counter*Inc + Min) >= Max -> 
  lists:reverse([Max|Acc]);
seq_float(Min, Max, Inc, Counter, Acc) -> 
  seq_float(Min, Max, Inc, Counter+1, [Inc * Counter + Min|Acc]).
seq_float(Min, Max, Inc) -> 
  seq_float(Min, Max, Inc, 0, []).

% **************************************************

```


Output:

```txt

                                                                                 
                                                                                 
                                                                                 
                                                           **                    
                                                         ******                  
                                                       ********                  
                                                         ******                  
                                                      ******** **   *            
                                              ***   *****************            
                                              ************************  ***      
                                              ****************************       
                                           ******************************        
                                            ******************************       
                                         ************************************    
                                *         **********************************     
                           ** ***** *     **********************************     
                           ***********   ************************************    
                         ************** ************************************     
                         ***************************************************     
                     *****************************************************       
                   *****************************************************         
                     *****************************************************       
                         ***************************************************     
                         ************** ************************************     
                           ***********   ************************************    
                           ** ***** *     **********************************     
                                *         **********************************     
                                         ************************************    
                                            ******************************       
                                           ******************************        
                                              ****************************       
                                              ************************  ***      
                                              ***   *****************            
                                                      ******** **   *            
                                                         ******                  
                                                       ********                  
                                                         ******                  
                                                           **                    
                                                                                 
                                                                                 
                                                                                 


```



## ERRE


```ERRE

PROGRAM MANDELBROT

!$KEY
!$INCLUDE="PC.LIB"

BEGIN

SCREEN(7)
GR_WINDOW(-2,1.5,2,-1.5)
FOR X0=-2 TO 2 STEP 0.01 DO
    FOR Y0=-1.5 TO 1.5 STEP 0.01 DO
        X=0
        Y=0

        ITERATION=0
        MAX_ITERATION=223

        WHILE (X*X+Y*Y<=(2*2) AND ITERATION<MAX_ITERATION) DO
            X_TEMP=X*X-Y*Y+X0
            Y=2*X*Y+Y0

            X=X_TEMP

            ITERATION=ITERATION+1
        END WHILE

        IF ITERATION<>MAX_ITERATION THEN
            C=ITERATION
          ELSE
            C=0
        END IF

        PSET(X0,Y0,C)
    END FOR
END FOR
END PROGRAM

```

Note: This is a PC version which uses EGA 16-color 320x200. Graphic commands are taken from
PC.LIB library.

=={{header|F Sharp|F#}}==

```fsharp
open System.Drawing 
open System.Windows.Forms
type Complex =
    { 
        re : float;
        im : float
    }
let cplus (x:Complex) (y:Complex) : Complex = 
    {
        re = x.re + y.re;
        im = x.im + y.im
    }
let cmult (x:Complex) (y:Complex) : Complex = 
    {
        re = x.re * y.re - x.im * y.im;
        im = x.re * y.im + x.im * y.re;
    }

let norm (x:Complex) : float =
    x.re*x.re + x.im*x.im

type Mandel = class
    inherit Form
    static member xPixels = 500
    static member yPixels = 500
    val mutable bmp : Bitmap
    member x.mandelbrot xMin xMax yMin yMax maxIter =
        let rec mandelbrotIterator z c n =
            if (norm z) > 2.0 then false else
                match n with
                    | 0 -> true
                    | n -> let z' = cplus ( cmult z z ) c in
                            mandelbrotIterator z' c (n-1)
        let dx = (xMax - xMin) / (float (Mandel.xPixels))
        let dy = (yMax - yMin) / (float (Mandel.yPixels))
        in
        for xi = 0 to Mandel.xPixels-1 do
            for yi = 0 to Mandel.yPixels-1 do
                let c = {re = xMin + (dx * float(xi) ) ;
                         im = yMin + (dy * float(yi) )} in
                if (mandelbrotIterator {re=0.;im=0.;} c maxIter) then
                    x.bmp.SetPixel(xi,yi,Color.Azure)
                else
                    x.bmp.SetPixel(xi,yi,Color.Black)
            done
        done

    member public x.generate () = x.mandelbrot (-1.5) 0.5 (-1.0) 1.0 200 ; x.Refresh()

    new() as x = {bmp = new Bitmap(Mandel.xPixels , Mandel.yPixels)} then
        x.Text <- "Mandelbrot set" ;
        x.Width <- Mandel.xPixels ;
        x.Height <- Mandel.yPixels ;
        x.BackgroundImage <- x.bmp;
        x.generate();
        x.Show();   
end

let f = new Mandel()
do Application.Run(f)
```


=== Alternate version, applicable to text and GUI ===
''' Basic generation code '''

```fsharp

let getMandelbrotValues width height maxIter ((xMin,xMax),(yMin,yMax)) =
  let mandIter (cr:float,ci:float) =
    let next (zr,zi) = (cr + (zr * zr - zi * zi)), (ci + (zr * zi + zi * zr))
    let rec loop = function
      | step,_ when step=maxIter->0
      | step,(zr,zi) when ((zr * zr + zi * zi) > 2.0) -> step
      | step,z -> loop ((step + 1), (next z))
    loop (0,(0.0, 0.0))
  let forPos =
    let dx, dy = (xMax - xMin) / (float width), (yMax - yMin) / (float height)
    fun y x -> mandIter ((xMin + dx * float(x)), (yMin + dy * float(y)))
  [0..height-1] |> List.map(fun y->[0..width-1] |> List.map (forPos y))

```


''' Text display '''

```fsharp

getMandelbrotValues 80 25 50 ((-2.0,1.0),(-1.0,1.0))
|> List.map(fun row-> row |> List.map (function | 0 ->" " |_->".") |> String.concat "")
|> List.iter (printfn "%s")

```


Results:
{{out}}

```txt

................................................................................
................................................................................
.................................................  .............................
................................................     ...........................
.................................................    ...........................
.......................................   .               ......................
........................................                       .................
....................................                          ..................
....................................                           .................
..........................  ......                              ................
.......................         ...                             ................
.....................            .                              ................
.................                                              .................
.................                                              .................
.....................            .                              ................
.......................         ...                             ................
..........................  ......                              ................
....................................                           .................
....................................                          ..................
........................................                       .................
.......................................   .               ......................
.................................................    ...........................
................................................     ...........................
.................................................  .............................
................................................................................

```


''' Graphics display '''

```fsharp

open System.Drawing 
open System.Windows.Forms

let showGraphic (colorForIter: int -> Color) (width: int) (height:int) maxIter view =
  new Form()
  |> fun frm ->
    frm.Width <- width
    frm.Height <- height
    frm.BackgroundImage <- 
      new Bitmap(width,height)
      |> fun bmp ->
        getMandelbrotValues width height maxIter view
        |> List.mapi (fun y row->row |> List.mapi (fun x v->((x,y),v))) |> List.collect id
        |> List.iter (fun ((x,y),v) -> bmp.SetPixel(x,y,(colorForIter v)))
        bmp
    frm.Show()

let toColor = (function | 0 -> (0,0,0) | n -> ((31 &&& n) |> fun x->(0, 18 + x * 5, 36 + x * 7))) >> Color.FromArgb

showGraphic toColor 640 480 5000 ((-2.0,1.0),(-1.0,1.0))

```



## Factor



```Factor

! with ("::") or without (":") generalizations:
! : [a..b] ( steps a b -- a..b ) 2dup swap - 4 nrot 1 - / <range> ;
::  [a..b] ( steps a b -- a..b ) a b b a - steps 1 - / <range> ;

: >char ( n -- c )
    dup -1 = [ drop 32 ] [ 26 mod CHAR: a + ] if ;

! iterates z' = z^2 + c, Factor does complex numbers!
: iter ( c z -- z' ) dup * + ;

: unbound ( c -- ? ) absq 4 > ;

:: mz ( c max i z -- n )
  {
    { [ i max >= ] [ -1 ] }
    { [ z unbound ] [ i ] }
    [ c max i 1 + c z iter mz ]
  } cond ;

: mandelzahl ( c max -- n ) 0 0 mz ;

:: mandel ( w h max -- )
    h -1. 1. [a..b] ! range over y
    [   w -2. 1. [a..b] ! range over x
        [ dupd swap rect> max mandelzahl >char ] map
        >string print
        drop ! old y
    ] each
    ;

70 25 1000 mandel


```


{{out}}

```txt
bbbbbbbcccccdddddddddddddddddddeeeeeeeffghjpjl feeeeedddddcccccccccccc
bbbbbbccccddddddddddddddddddeeeeeeeefffghikopjhgffeeeeedddddcccccccccc
bbbbbcccddddddddddddddddddeeeeeeeefffggjotx etiigfffeeeeddddddcccccccc
bbbbccddddddddddddddddddeeeeeeeffgggghhjq     iihgggfffeedddddddcccccc
bbbccddddddddddddddddeeeeeefffghvasjjqqyqt   upqlrjhhhkhfedddddddccccc
bbbcdddddddddddddddeeeeffffffgghks  c             qnbpfmgfedddddddcccc
bbcdddddddddddddeefffffffffgggipmt                    qhgfeedddddddccc
bbdddddddddeeeefhlggggggghhhhils                      ljigfeedddddddcc
bcddddeeeeeefffghmllkjiljjiijle                         yhfeedddddddcc
bddeeeeeeeffffghhjoj do   clmq                         qlgfeeedddddddc
bdeeeeeefffffhiijpu         sm                         ohffeeedddddddc
beffeefgggghhjocsu                                    higffeeedddddddc
                                                    cmihgffeeedddddddd
beffeefgggghhjocsu                                    higffeeedddddddc
bdeeeeeefffffhiijpu         sd                         ohffeeedddddddc
bddeeeeeeeffffghhjoj do   clmq                         qlgfeeedddddddc
bcddddeeeeeefffghmllkjiljjiijle                         yhfeedddddddcc
bbdddddddddeeeefhlggggggghhhhils                      ljigfeedddddddcc
bbcdddddddddddddeefffffffffgggipmt                    qhgfeedddddddccc
bbbcdddddddddddddddeeeeffffffgghks  c             qnbpfmgfedddddddcccc
bbbccddddddddddddddddeeeeeefffghvasjjqqyqt   upqlrjhhhkhfedddddddccccc
bbbbccddddddddddddddddddeeeeeeeffgggghhjq     iihgggfffeedddddddcccccc
bbbbbcccddddddddddddddddddeeeeeeeefffggjotx etiigfffeeeeddddddcccccccc
bbbbbbccccddddddddddddddddddeeeeeeeefffghikopjhgffeeeeedddddcccccccccc
bbbbbbbcccccdddddddddddddddddddeeeeeeeffghjpjl feeeeedddddcccccccccccc



```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Mandelbrot_set this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

This uses [[grayscale image]] utilities.

```Forth
500 value max-iter

: mandel ( gmp  F: imin imax rmin rmax -- )
  0e 0e { F: imin F: imax F: rmin F: rmax F: Zr F: Zi }
  dup bheight 0 do
    i s>f dup bheight s>f f/ imax imin f- f* imin f+ TO Zi
    dup bwidth 0 do
      i s>f dup bwidth s>f f/ rmax rmin f- f* rmin f+ TO Zr
      Zr Zi max-iter
      begin  1- dup
      while  fover fdup f* fover fdup f*
             fover fover f+ 4e f<
      while  f- Zr f+
             frot frot f* 2e f* Zi f+
      repeat fdrop fdrop
             drop 0        \ for a pretty grayscale image, replace with: 255 max-iter */
      else   drop 255
      then   fdrop fdrop
      over i j rot g!
    loop
  loop    drop ;

80 24 graymap
dup -1e 1e -2e 1e mandel
dup gshow
free bye
```



## Fortran

{{Works with|Fortran|90 and later}}

```fortran
program mandelbrot

  implicit none
  integer  , parameter :: rk       = selected_real_kind (9, 99)
  integer  , parameter :: i_max    =  800
  integer  , parameter :: j_max    =  600
  integer  , parameter :: n_max    =  100
  real (rk), parameter :: x_centre = -0.5_rk
  real (rk), parameter :: y_centre =  0.0_rk
  real (rk), parameter :: width    =  4.0_rk
  real (rk), parameter :: height   =  3.0_rk
  real (rk), parameter :: dx_di    =   width / i_max
  real (rk), parameter :: dy_dj    = -height / j_max
  real (rk), parameter :: x_offset = x_centre - 0.5_rk * (i_max + 1) * dx_di
  real (rk), parameter :: y_offset = y_centre - 0.5_rk * (j_max + 1) * dy_dj
  integer, dimension (i_max, j_max) :: image
  integer   :: i
  integer   :: j
  integer   :: n
  real (rk) :: x
  real (rk) :: y
  real (rk) :: x_0
  real (rk) :: y_0
  real (rk) :: x_sqr
  real (rk) :: y_sqr

  do j = 1, j_max
    y_0 = y_offset + dy_dj * j
    do i = 1, i_max
      x_0 = x_offset + dx_di * i
      x = 0.0_rk
      y = 0.0_rk
      n = 0
      do
        x_sqr = x ** 2
        y_sqr = y ** 2
        if (x_sqr + y_sqr > 4.0_rk) then
          image (i, j) = 255
          exit
        end if
        if (n == n_max) then
          image (i, j) = 0
          exit
        end if
        y = y_0 + 2.0_rk * x * y
        x = x_0 + x_sqr - y_sqr
        n = n + 1
      end do
    end do
  end do
  open  (10, file = 'out.pgm')
  write (10, '(a/ i0, 1x, i0/ i0)') 'P2', i_max, j_max, 255
  write (10, '(i0)') image
  close (10)

end program mandelbrot
```




## Frink

This draws a graphical Mandelbrot set using Frink's built-in graphics and complex arithmetic.

```Frink

// Maximum levels for each pixel.
levels = 60

// Create a random color for each level.
colors = new array[[levels]]
for a = 0 to levels-1
   colors@a = new color[randomFloat[0,1], randomFloat[0,1], randomFloat[0,1]]

// Make this number smaller for higher resolution.
stepsize = .005

g = new graphics
g.antialiased[false]

for im = -1.2 to 1.2 step stepsize
{
   imag = i * im
   for real = -2 to 1 step stepsize
   {  
      C = real + imag
      z = 0
      count = -1

      do
      {
         z = z^2 + C
         count=count+1;
      } while abs[z] < 4 and count < levels

      g.color[colors@((count-1) mod levels)]
      g.fillRectSize[real, im, stepsize, stepsize]
   }
}

g.show[]

```



## Futhark

{{incorrect|Futhark|Futhark's syntax has changed, so this example will not compile}}
Computes escapes for each pixel, but not the colour.


```Futhark

default(f32)

type complex = (f32, f32)

fun dot(c: complex): f32 =
  let (r, i) = c
  in r * r + i * i

fun multComplex(x: complex, y: complex): complex =
  let (a, b) = x
  let (c, d) = y
  in (a*c - b * d,
      a*d + b * c)

fun addComplex(x: complex, y: complex): complex =
  let (a, b) = x
  let (c, d) = y
  in (a + c,
      b + d)

fun divergence(depth: int, c0: complex): int =
  loop ((c, i) = (c0, 0)) = while i < depth && dot(c) < 4.0 do
    (addComplex(c0, multComplex(c, c)),
     i + 1)
  in i

fun mandelbrot(screenX: int, screenY: int, depth: int, view: (f32,f32,f32,f32)): [screenX][screenY]int =
  let (xmin, ymin, xmax, ymax) = view
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  in map (fn (x: int): [screenY]int  =>
           map  (fn (y: int): int  =>
                  let c0 = (xmin + (f32(x) * sizex) / f32(screenX),
                            ymin + (f32(y) * sizey) / f32(screenY))
                  in divergence(depth, c0))
                (iota screenY))
         (iota screenX)

fun main(screenX: int, screenY: int, depth: int, xmin: f32, ymin: f32, xmax: f32, ymax: f32): [screenX][screenY]int =
  mandelbrot(screenX, screenY, depth, (xmin, ymin, xmax, ymax))

```



## GLSL


This example works directly on Shadertoy link[https://www.shadertoy.com/view/XsfGWS]


```glsl
void main(void)
{
	vec2 uv = gl_FragCoord.xy / iResolution.xy;
	float scale = iResolution.y / iResolution.x;
	uv=((uv-0.5)*5.5);
	uv.y*=scale;
	uv.y+=0.0;
	uv.x-=0.5;


	vec2 z = vec2(0.0, 0.0);
	vec3 c = vec3(0.0, 0.0, 0.0);
	float v;
	
	for(int i=0;(i<170);i++)
	{

		if(((z.x*z.x+z.y*z.y) >= 4.0)) break;
		z = vec2(z.x*z.x - z.y*z.y, 2.0*z.y*z.x) + uv;
		
		
		if((z.x*z.x+z.y*z.y) >= 2.0)
		{
			c.b=float(i)/20.0;
			c.r=sin((float(i)/5.0));
		}

	}
	
	
	gl_FragColor = vec4(c,1.0);
}
```



## gnuplot

The output from gnuplot is controlled by setting the appropriate values for the options <code>terminal</code> and <code>output</code>.

```gnuplot
set terminal png
set output 'mandelbrot.png'
```

The following script draws an image of the number of iterations it takes to escape the circle with radius <code>rmax</code> with a maximum of <code>nmax</code>.

```gnuplot
rmax = 2
nmax = 100
complex (x, y) = x * {1, 0} + y * {0, 1}
mandelbrot (z, z0, n) = n == nmax || abs (z) > rmax ? n : mandelbrot (z ** 2 + z0, z0, n + 1)
set samples 200
set isosamples 200
set pm3d map
set size square
splot [-2 : .8] [-1.4 : 1.4] mandelbrot (complex (0, 0), complex (x, y), 0) notitle
```

{{out}}
[[File:mandelbrot.png]]


## Go

;Text
Prints an 80-char by 41-line depiction.

```go
package main

import "fmt"
import "math/cmplx"

func mandelbrot(a complex128) (z complex128) {
    for i := 0; i < 50; i++ {
        z = z*z + a
    }
    return
}

func main() {
    for y := 1.0; y >= -1.0; y -= 0.05 {
        for x := -2.0; x <= 0.5; x += 0.0315 {
            if cmplx.Abs(mandelbrot(complex(x, y))) < 2 {
                fmt.Print("*")
            } else {
                fmt.Print(" ")
            }
        }
        fmt.Println("")
    }
}
```

;Graphical
[[File:GoMandelbrot.png|thumb|right|.png image]]

```go
package main

import (
    "fmt"
    "image"
    "image/color"
    "image/draw"
    "image/png"
    "math/cmplx"
    "os"
)

const (
    maxEsc = 100
    rMin   = -2.
    rMax   = .5
    iMin   = -1.
    iMax   = 1.
    width  = 750
    red    = 230
    green  = 235
    blue   = 255
)

func mandelbrot(a complex128) float64 {
    i := 0
    for z := a; cmplx.Abs(z) < 2 && i < maxEsc; i++ {
        z = z*z + a
    }
    return float64(maxEsc-i) / maxEsc
}

func main() {
    scale := width / (rMax - rMin)
    height := int(scale * (iMax - iMin))
    bounds := image.Rect(0, 0, width, height)
    b := image.NewNRGBA(bounds)
    draw.Draw(b, bounds, image.NewUniform(color.Black), image.ZP, draw.Src)
    for x := 0; x < width; x++ {
        for y := 0; y < height; y++ {
            fEsc := mandelbrot(complex(
                float64(x)/scale+rMin,
                float64(y)/scale+iMin))
            b.Set(x, y, color.NRGBA{uint8(red * fEsc),
                uint8(green * fEsc), uint8(blue * fEsc), 255})

        }
    }
    f, err := os.Create("mandelbrot.png")
    if err != nil {
        fmt.Println(err)
        return
    }
    if err = png.Encode(f, b); err != nil {
        fmt.Println(err)
    }
    if err = f.Close(); err != nil {
        fmt.Println(err)
    }
}
```



## Haskell

{{trans|Ruby}}

```haskell
import Data.Bool
import Data.Complex (Complex((:+)), magnitude)

mandelbrot
  :: RealFloat a
  => Complex a -> Complex a
mandelbrot a = iterate ((a +) . (^ 2)) 0 !! 50

main :: IO ()
main =
  mapM_
    putStrLn
    [ [ bool ' ' '*' (2 > magnitude (mandelbrot (x :+ y)))
      | x <- [-2,-1.9685 .. 0.5] ]
    | y <- [1,0.95 .. -1] ]
```


Save the code to file m.hs and run :
 runhaskell m.hs

{{Out}}

```txt
                            
                                                           **                   
                                                         ******                 
                                                       ********                 
                                                         ******                 
                                                      ******** **   *           
                                              ***   *****************           
                                              ************************  ***     
                                              ****************************      
                                           ******************************       
                                            ******************************      
                                         ************************************   
                                *         **********************************    
                           ** ***** *     **********************************    
                           ***********   ************************************   
                         ************** ************************************    
                         ***************************************************    
                     *****************************************************      
 ***********************************************************************        
                     *****************************************************      
                         ***************************************************    
                         ************** ************************************    
                           ***********   ************************************   
                           ** ***** *     **********************************    
                                *         **********************************    
                                         ************************************   
                                            ******************************      
                                           ******************************       
                                              ****************************      
                                              ************************  ***     
                                              ***   *****************           
                                                      ******** **   *           
                                                         ******                 
                                                       ********                 
                                                         ******                 
                                                           **  
```



## Haxe

This version compiles for flash version 9 or greater.
The compilation command is

```haxe
haxe -swf mandelbrot.swf -main Mandelbrot
```



```haxe
class Mandelbrot extends flash.display.Sprite
{
    inline static var MAX_ITER = 255;

    public static function main() {
        var w = flash.Lib.current.stage.stageWidth;
        var h = flash.Lib.current.stage.stageHeight;
        var mandelbrot = new Mandelbrot(w, h);
        flash.Lib.current.stage.addChild(mandelbrot);
        mandelbrot.drawMandelbrot();
    }

    var image:flash.display.BitmapData;
    public function new(width, height) {
        super();
        var bitmap:flash.display.Bitmap;
        image = new flash.display.BitmapData(width, height, false);
        bitmap = new flash.display.Bitmap(image);
        this.addChild(bitmap);
    }

    public function drawMandelbrot() {
        image.lock();
        var step_x = 3.0 / (image.width-1);
        var step_y = 2.0 / (image.height-1);
        for (i in 0...image.height) {
            var ci = i * step_y - 1.0;
            for (j in 0...image.width) {
                var k = 0;
                var zr = 0.0;
                var zi = 0.0;
                var cr = j * step_x - 2.0;
                while (k <= MAX_ITER && (zr*zr + zi*zi) <= 4) {
                    var temp = zr*zr - zi*zi + cr;
                    zi = 2*zr*zi + ci;
                    zr = temp;
                    k ++;
                }
                paint(j, i, k);
            }
        }
        image.unlock();
    }

    inline function paint(x, y, iter) {
        var color = iter > MAX_ITER? 0 : iter * 0x100;
        image.setPixel(x, y, color);
    }
}
```



## Huginn


```huginn
#! /bin/sh
exec huginn -E "${0}" "${@}"
#! huginn

import Algorithms as algo;
import Mathematics as math;
import Terminal as term;

mandelbrot( x, y ) {
  c = math.Complex( x, y );
  z = math.Complex( 0., 0. );
  s = -1;
  for ( i : algo.range( 50 ) ) {
    z = z * z + c;
    if ( | z | > 2. ) {
      s = i;
      break;
    }
  }
  return ( s );
}

main( argv_ ) {
  imgSize = term_size( argv_ );
  yRad = 1.2;
  yScale = 2. * yRad / real( imgSize[0] );
  xScale = 3.3 / real( imgSize[1] );
  glyphTab = [ ".", ":", "-", "+", "+" ].resize( 12, "*" ).resize( 26, "%" ).resize( 50, "@" ).push( " " );
  for ( y : algo.range( imgSize[0] ) ) {
    line = "";
    for ( x : algo.range( imgSize[1] ) ) {
      line += glyphTab[ mandelbrot( xScale * real( x ) - 2.3, yScale * real( y ) - yRad ) ];
    }
    print( line + "\n" );
  }
  return ( 0 );
}

term_size( argv_ ) {
  lines = 25;
  columns = 80;
  if ( size( argv_ ) == 3 ) {
    lines = integer( argv_[1] );
    columns = integer( argv_[2] );
  } else {
    lines = term.lines();
    columns = term.columns();
    if ( ( lines % 2 ) == 0 ) {
      lines -= 1;
    }
  }
  lines -= 1;
  columns -= 1;
  return ( ( lines, columns ) );
}
```


{{out}}
<small>

```txt

........................:::::::::::::::::::------------------------------------------------------::::::::::::::::::::::::::::::::::::
......................::::::::::::::::------------------------------------++++++++++++++++++++---------::::::::::::::::::::::::::::::
....................:::::::::::::-----------------------------------+++++++++++++*******+++++++++++--------::::::::::::::::::::::::::
...................:::::::::::----------------------------------++++++++++++++++****%%******++++++++++---------::::::::::::::::::::::
.................:::::::::----------------------------------++++++++++++++++++++******% %****++++++++++++---------:::::::::::::::::::
................::::::::---------------------------------+++++++++++++++++++++******%%%%%*****+++++++++++++----------::::::::::::::::
...............::::::---------------------------------++++++++++++++++++++*****%%%%%   @%%%@***++++++++++++++----------::::::::::::::
..............:::::--------------------------------++++++++++++++++++***********@%        @%******+++++++++++++-----------:::::::::::
.............::::-------------------------------+++++++++++++++++****************@        %%***************+++++------------:::::::::
............:::------------------------------+++++++++++++++++****@%%%****%@@%@% %%@    @%%%% %*% ********%**++++------------::::::::
...........:::----------------------------+++++++++++++++++********%   @%@@                      %%%**%@%%@@**+++++------------::::::
..........:::-------------------------+++++++++++++++++++**********%                                @    %%***++++++------------:::::
..........:----------------------+++++++++++++++++++++*********%%%%                                    %%******++++++------------::::
.........:-----------------++++++++****************************                                           %*****+++++-------------:::
.........----------+++++++++++++++****%********%%*********** @%%                                          % %%**++++++-------------::
........:------++++++++++++++++++*******%@@%%**%% %%%*******%%                                             %%***+++++++-------------:
........---+++++++++++++++++++++********%@           @%%%**%                                               %%***+++++++-------------:
.......:-+++++++++++++++++++++*******%%%%                @%%                                               @****++++++++-------------
.......-+++++++++++++***********%**@*%@                    %                                               %***+++++++++-------------
.......++++++*******************%%    @                                                                  %*****+++++++++-------------
.......                                                                                               %%******++++++++++-------------
.......++++++*******************%%    @                                                                  %*****+++++++++-------------
.......-+++++++++++++***********%**@*%@                    %                                               %***+++++++++-------------
.......:-+++++++++++++++++++++*******%%%%                @%%                                               @****++++++++-------------
........---+++++++++++++++++++++********%@           @%%%**%                                               %%***+++++++-------------:
........:------++++++++++++++++++*******%@@%%**%% %%%*******%%                                             %%***+++++++-------------:
.........----------+++++++++++++++****%********%%*********** @%%                                          % %%**++++++-------------::
.........:-----------------++++++++****************************                                           %*****+++++-------------:::
..........:----------------------+++++++++++++++++++++*********%%%%                                    %%******++++++------------::::
..........:::-------------------------+++++++++++++++++++**********%                                @    %%***++++++------------:::::
...........:::----------------------------+++++++++++++++++********%   @%@@                      %%%**%@%%@@**+++++------------::::::
............:::------------------------------+++++++++++++++++****@%%%****%@@%@% %%@    @%%%% %*% ********%**++++------------::::::::
.............::::-------------------------------+++++++++++++++++****************@        %%***************+++++------------:::::::::
..............:::::--------------------------------++++++++++++++++++***********@%        @%******+++++++++++++-----------:::::::::::
...............::::::---------------------------------++++++++++++++++++++*****%%%%%   @%%%@***++++++++++++++----------::::::::::::::
................::::::::---------------------------------+++++++++++++++++++++******%%%%%*****+++++++++++++----------::::::::::::::::
.................:::::::::----------------------------------++++++++++++++++++++******% %****++++++++++++---------:::::::::::::::::::
...................:::::::::::----------------------------------++++++++++++++++****%%******++++++++++---------::::::::::::::::::::::
....................:::::::::::::-----------------------------------+++++++++++++*******+++++++++++--------::::::::::::::::::::::::::
......................::::::::::::::::------------------------------------++++++++++++++++++++---------::::::::::::::::::::::::::::::

```

</small>

=={{header|Icon}} and {{header|Unicon}}==

```Icon
link graphics

procedure main()
    width := 750
    height := 600
    limit := 100
    WOpen("size="||width||","||height)
    every x:=1 to width & y:=1 to height do
    {
        z:=complex(0,0)
        c:=complex(2.5*x/width-2.0,(2.0*y/height-1.0))
        j:=0
        while j<limit & cAbs(z)<2.0 do
        {
           z := cAdd(cMul(z,z),c)
           j+:= 1
        }
        Fg(mColor(j,limit))
        DrawPoint(x,y)
    }
    WriteImage("./mandelbrot.gif")
    WDone()
end

procedure mColor(x,limit)
   max_color := 2^16-1
   color := integer(max_color*(real(x)/limit))

   return(if x=limit
          then "black"
          else color||","||color||",0")
end

record complex(r,i)

procedure cAdd(x,y)
    return complex(x.r+y.r,x.i+y.i)
end

procedure cMul(x,y)
    return complex(x.r*y.r-x.i*y.i,x.r*y.i+x.i*y.r)
end

procedure cAbs(x)
    return sqrt(x.r*x.r+x.i*x.i)
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/gprocs/graphics.icn graphics is required ]

{{improve|Unicon|The example is correct; however, Unicon implemented additional graphical features and a better example may be possible.}}


## IDL

IDL - Interactive Data Language 
(free implementation: GDL - GNU Data Language
http://gnudatalanguage.sourceforge.net)

```IDL

PRO Mandelbrot,xRange,yRange,xPixels,yPixels,iterations

xPixelstartVec = Lindgen( xPixels) * Float(xRange[1]-xRange[0]) / $
                 xPixels + xRange[0]
yPixelstartVec = Lindgen( yPixels) * Float(YRANGE[1]-yrange[0])$
                 / yPixels + yRange[0]

constArr = Complex( Rebin( xPixelstartVec, xPixels, yPixels),$
                     Rebin( Transpose(yPixelstartVec), xPixels, yPixels))

valArr = ComplexArr( xPixels, yPixels)

res = IntArr( xPixels, yPixels)

oriIndex = Lindgen( Long(xPixels) * yPixels)

FOR i = 0, iterations-1 DO BEGIN ; only one loop needed

    ; calculation for whole array at once
    valArr = valArr^2 - constArr

    whereIn = Where( Abs( valArr) LE 4.0d, COMPLEMENT=whereOut)

    IF whereIn[0] EQ -1 THEN BREAK

    valArr = valArr[ whereIn]

    constArr = constArr[ whereIn]

    IF whereOut[0] NE -1 THEN BEGIN

        res[ oriIndex[ whereOut]] = i+1

        oriIndex = oriIndex[ whereIn]
    ENDIF
ENDFOR

tv,res ; open a window and show the result

END


Mandelbrot,[-1.,2.3],[-1.3,1.3],640,512,200

END


```

from the command line:

```IDL

GDL>.run mandelbrot

```

or

```IDL

GDL> Mandelbrot,[-1.,2.3],[-1.3,1.3],640,512,200

```



## Inform 7

{{libheader|Glimmr Drawing Commands by Erik Temple}}
{{works with|Glulx virtual machine}}

```inform7
"Mandelbrot"

The story headline is "A Non-Interactive Set".

Include Glimmr Drawing Commands by Erik Temple.

[Q20 fixed-point or floating-point: see definitions below]
Use floating-point math.

Finished is a room.

The graphics-window is a graphics g-window spawned by the main-window.
The position is g-placeabove.

When play begins:
	let f10 be 10 as float;
	now min re is ( -20 as float ) fdiv f10;
	now max re is ( 6 as float ) fdiv f10;
	now min im is ( -12 as float ) fdiv f10;
	now max im is ( 12 as float ) fdiv f10;
	now max iterations is 100;
	add color g-Black to the palette;
	add color g-Red to the palette;
	add hex "#FFA500" to the palette;
	add color g-Yellow to the palette;
	add color g-Green to the palette;
	add color g-Blue to the palette;
	add hex "#4B0082" to the palette;
	add hex "#EE82EE" to the palette;
	open up the graphics-window.

Min Re is a number that varies.
Max Re is a number that varies.
Min Im is a number that varies.
Max Im is a number that varies.

Max Iterations is a number that varies.

Min X is a number that varies.
Max X is a number that varies.
Min Y is a number that varies.
Max Y is a number that varies.

The palette is a list of numbers that varies.

[vertically mirrored version]
Window-drawing rule for the graphics-window when max im is fneg min im:
	clear the graphics-window;
	let point be { 0, 0 };
	now min X is 0 as float;
	now min Y is 0 as float;
	let mX be the width of the graphics-window minus 1;
	let mY be the height of the graphics-window minus 1;
	now max X is mX as float;
	now max Y is mY as float;
	let L be the column order with max mX;
	repeat with X running through L:
		now entry 1 in point is X;
		repeat with Y running from 0 to mY / 2:
			now entry 2 in point is Y;
			let the scaled point be the complex number corresponding to the point;
			let V be the Mandelbrot result for the scaled point;
			let C be the color corresponding to V;
			if C is 0, next;
			draw a rectangle (C) in the graphics-window at the point with size 1 by 1;
			now entry 2 in point is mY - Y;
			draw a rectangle (C) in the graphics-window at the point with size 1 by 1;
		yield to VM;
	rule succeeds.

[slower non-mirrored version]
Window-drawing rule for the graphics-window:
	clear the graphics-window;
	let point be { 0, 0 };
	now min X is 0 as float;
	now min Y is 0 as float;
	let mX be the width of the graphics-window minus 1;
	let mY be the height of the graphics-window minus 1;
	now max X is mX as float;
	now max Y is mY as float;
	let L be the column order with max mX;
	repeat with X running through L:
		now entry 1 in point is X;
		repeat with Y running from 0 to mY:
			now entry 2 in point is Y;
			let the scaled point be the complex number corresponding to the point;
			let V be the Mandelbrot result for the scaled point;
			let C be the color corresponding to V;
			if C is 0, next;
			draw a rectangle (C) in the graphics-window at the point with size 1 by 1;
		yield to VM;
	rule succeeds.

To decide which list of numbers is column order with max (N - number):
	let L be a list of numbers;
	let L2 be a list of numbers;
	let D be 64;
	let rev be false;
	while D > 0:
		let X be 0;
		truncate L2 to 0 entries;
		while X <= N:
			if D is 64 or X / D is odd, add X to L2;
			increase X by D;
		if rev is true:
			reverse L2;
			let rev be false;
		otherwise:
			let rev be true;
		add L2 to L;
		let D be D / 2;
	decide on L.

To decide which list of numbers is complex number corresponding to (P - list of numbers):
	let R be a list of numbers;
	extend R to 2 entries;
	let X be entry 1 in P as float;
	let X be (max re fsub min re) fmul (X fdiv max X);
	let X be X fadd min re;
	let Y be entry 2 in P as float;
	let Y be (max im fsub min im) fmul (Y fdiv max Y);
	let Y be Y fadd min im;
	now entry 1 in R is X;
	now entry 2 in R is Y;
	decide on R.

To decide which number is Mandelbrot result for (P - list of numbers):
	let c_re be entry 1 in P;
	let c_im be entry 2 in P;
	let z_re be 0 as float;
	let z_im be z_re;
	let threshold be 4 as float;
	let runs be 0;
	while 1 is 1:
		[ z = z * z ]
		let r2 be z_re fmul z_re;
		let i2 be z_im fmul z_im;
		let ri be z_re fmul z_im;
		let z_re be r2 fsub i2;
		let z_im be ri fadd ri;
		[ z = z + c ]
		let z_re be z_re fadd c_re;
		let z_im be z_im fadd c_im;
		let norm be (z_re fmul z_re) fadd (z_im fmul z_im);
		increase runs by 1;
		if norm is greater than threshold, decide on runs;
		if runs is max iterations, decide on 0.

To decide which number is color corresponding to (V - number):
	let L be the number of entries in the palette;
	let N be the remainder after dividing V by L;
	decide on entry (N + 1) in the palette.

Section - Fractional numbers (for Glulx only)

To decide which number is (N - number) as float: (- (numtof({N})) -).
To decide which number is (N - number) fadd (M - number): (- (fadd({N}, {M})) -).
To decide which number is (N - number) fsub (M - number): (- (fsub({N}, {M})) -).
To decide which number is (N - number) fmul (M - number): (- (fmul({N}, {M})) -).
To decide which number is (N - number) fdiv (M - number): (- (fdiv({N}, {M})) -).
To decide which number is fneg (N - number): (- (fneg({N})) -).
To yield to VM: (- glk_select_poll(gg_event); -).

Use Q20 fixed-point math translates as (- Constant Q20_MATH; -).
Use floating-point math translates as (- Constant FLOAT_MATH; -).

Include (-
#ifdef Q20_MATH;
! Q11.20 format: 1 sign bit, 11 integer bits, 20 fraction bits
[ numtof n r; @shiftl n 20 r; return r; ];
[ fadd n m; return n+m; ];
[ fsub n m; return n-m; ];
[ fmul n m; n = n + $$1000000000; @sshiftr n 10 n; m = m + $$1000000000; @sshiftr m 10 m; return n * m; ]; 
[ fdiv n m; @sshiftr m 20 m; return n / m; ];
[ fneg n; return -n; ];
#endif;

#ifdef FLOAT_MATH;
[ numtof f; @"S2:400" f f; return f; ];
[ fadd n m; @"S3:416" n m n; return n; ];
[ fsub n m; @"S3:417" n m n; return n; ];
[ fmul n m; @"S3:418" n m n; return n; ];
[ fdiv n m; @"S3:419" n m n; return n; ];
[ fneg n; @bitxor n $80000000 n; return n; ];
#endif;
-).
```


Newer Glulx interpreters provide 32-bit floating-point operations, but this solution also supports fixed-point math which is more widely supported and accurate enough for a zoomed-out view. Inform 6 inclusions are used for the low-level math functions in either case. The rendering process is extremely slow, since the graphics system is not optimized for pixel-by-pixel drawing, so this solution includes an optimization for vertical symmetry (as in the default view) and also includes extra logic to draw the lines in a more immediately useful order.

[[File:Mandelbrot-Inform7.png]]


## J

The characteristic function of the Mandelbrot can be defined as follows:

```j
mcf=. (<: 2:)@|@(] ((*:@] + [)^:((<: 2:)@|@])^:1000) 0:) NB. 1000 iterations test
```

The Mandelbrot set can be drawn as follows:

```j
domain=. |.@|:@({.@[ + ] *~ j./&i.&>/@+.@(1j1 + ] %~ -~/@[))&>/

load 'viewmat'
viewmat mcf "0 @ domain (_2j_1 1j1) ; 0.01 NB. Complex interval and resolution
```



A smaller version, based on a black&white implementation of viewmat (and paraphrased, from html markup to wiki markup), is shown here:


```j
   viewmat mcf "0 @ domain (_2j_1 1j1) ; 0.1 NB. Complex interval and resolution
```


The output is HTML-heavy and can be found [[Mandelbrot_set/J/Output|here]] 
(split out to make editing this page easier).


## Java

{{libheader|Swing}} {{libheader|AWT}}

```java
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import javax.swing.JFrame;

public class Mandelbrot extends JFrame {

    private final int MAX_ITER = 570;
    private final double ZOOM = 150;
    private BufferedImage I;
    private double zx, zy, cX, cY, tmp;

    public Mandelbrot() {
        super("Mandelbrot Set");
        setBounds(100, 100, 800, 600);
        setResizable(false);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        I = new BufferedImage(getWidth(), getHeight(), BufferedImage.TYPE_INT_RGB);
        for (int y = 0; y < getHeight(); y++) {
            for (int x = 0; x < getWidth(); x++) {
                zx = zy = 0;
                cX = (x - 400) / ZOOM;
                cY = (y - 300) / ZOOM;
                int iter = MAX_ITER;
                while (zx * zx + zy * zy < 4 && iter > 0) {
                    tmp = zx * zx - zy * zy + cX;
                    zy = 2.0 * zx * zy + cY;
                    zx = tmp;
                    iter--;
                }
                I.setRGB(x, y, iter | (iter << 8));
            }
        }
    }

    @Override
    public void paint(Graphics g) {
        g.drawImage(I, 0, 0, this);
    }

    public static void main(String[] args) {
        new Mandelbrot().setVisible(true);
    }
}
```



## JavaScript

{{works with|Firefox|3.5.11}}

This needs the canvas tag of HTML 5 (it will not run on IE8 and lower or old browsers).

The code can be run directly from the Javascript console in modern browsers by copying and pasting it.


```javascript
function mandelIter(cx, cy, maxIter) {
  var x = 0.0;
  var y = 0.0;
  var xx = 0;
  var yy = 0;
  var xy = 0;

  var i = maxIter;
  while (i-- && xx + yy <= 4) {
    xy = x * y;
    xx = x * x;
    yy = y * y;
    x = xx - yy + cx;
    y = xy + xy + cy;
  }
  return maxIter - i;
}

function mandelbrot(canvas, xmin, xmax, ymin, ymax, iterations) {
  var width = canvas.width;
  var height = canvas.height;

  var ctx = canvas.getContext('2d');
  var img = ctx.getImageData(0, 0, width, height);
  var pix = img.data;
  
  for (var ix = 0; ix < width; ++ix) {
    for (var iy = 0; iy < height; ++iy) {
      var x = xmin + (xmax - xmin) * ix / (width - 1);
      var y = ymin + (ymax - ymin) * iy / (height - 1);
      var i = mandelIter(x, y, iterations);
      var ppos = 4 * (width * iy + ix);
      
      if (i > iterations) {
        pix[ppos] = 0;
        pix[ppos + 1] = 0;
        pix[ppos + 2] = 0;
      } else {
        var c = 3 * Math.log(i) / Math.log(iterations - 1.0);
        
        if (c < 1) {
          pix[ppos] = 255 * c;
          pix[ppos + 1] = 0;
          pix[ppos + 2] = 0;
        }
        else if ( c < 2 ) {
          pix[ppos] = 255;
          pix[ppos + 1] = 255 * (c - 1);
          pix[ppos + 2] = 0;
        } else {
          pix[ppos] = 255;
          pix[ppos + 1] = 255;
          pix[ppos + 2] = 255 * (c - 2);
        }
      }
      pix[ppos + 3] = 255;
    }
  }
  
  ctx.putImageData(img, 0, 0);
}

var canvas = document.createElement('canvas');
canvas.width = 900;
canvas.height = 600;

document.body.insertBefore(canvas, document.body.childNodes[0]);

mandelbrot(canvas, -2, 1, -1, 1, 1000);
```


{{out}} with default parameters:
[[File:Mandelbrot-Javascript.png]]


###  ES6/WebAssembly 


With ES6 and WebAssembly, the program can run faster.  Of course, this requires a compiled WASM file, but one can easily build
one for instance with the [https://mbebenita.github.io/WasmExplorer/ WebAssembly explorer]


```javascript
var mandelIter;
fetch("./mandelIter.wasm")
    .then(res => {
        if (res.ok) return res.arrayBuffer();
        throw new Error('Unable to fetch WASM.');
    })
    .then(bytes => { return WebAssembly.compile(bytes); })
    .then(module => { return WebAssembly.instantiate(module); })
    .then(instance => { WebAssembly.instance = instance; draw(); })

function mandelbrot(canvas, xmin, xmax, ymin, ymax, iterations) {
    // ...
    var i = WebAssembly.instance.exports.mandelIter(x, y, iterations);
    // ...
}

function draw() {
    // canvas initialization if necessary
    // ...
    mandelbrot(canvas, -2, 1, -1, 1, 1000);
    // ...
}
```



## jq

[[Image:Mandelbrot-Javascript.png|thumb|Thumbnail of SVG produced by jq program]]
{{Works with|jq|1.4}}

The Mandelbrot function as defined here is similar to the JavaScript
implementation but generates SVG.  The resulting picture is the same.

'''Preliminaries'''

```jq
# SVG STUFF
  def svg(id; width; height): 
    "<svg width='\(width // "100%")' height='\(height // "100%") '
        id='\(id)'
        xmlns='http://www.w3.org/2000/svg'>";

  def pixel(x;y;r;g;b;a):
    "<circle cx='\(x)' cy='\(y)' r='1' fill='rgb(\(r|floor),\(g|floor),\(b|floor))' />";

# "UNTIL"
  # As soon as "condition" is true, then emit . and stop:
  def do_until(condition; next):
    def u: if condition then . else (next|u) end;
    u;

```

```jq

def Mandeliter( cx; cy; maxiter ):
  # [i, x, y, x^2+y^2]
  [ maxiter, 0.0, 0.0, 0.0 ]
  | do_until( .[0] == 0 or .[3] > 4;
      .[1] as $x | .[2] as $y
      | ($x * $y) as $xy
      | ($x * $x) as $xx
      | ($y * $y) as $yy
      | [ (.[0] - 1),         # i
          ($xx - $yy + cx),   # x
          ($xy + $xy + cy),   # y
          ($xx+$yy)           # xx+yy
        ] )
    | maxiter - .[0];
 
# width and height should be specified as the number of pixels.
# obj == { xmin: _, xmax: _, ymin: _, ymax: _ }
def Mandelbrot( obj; width; height; iterations ):
  def pixies:
    range(0; width) as $ix
    | (obj.xmin + ((obj.xmax - obj.xmin) * $ix / (width - 1))) as $x 
    | range(0; height) as $iy
    | (obj.ymin + ((obj.ymax - obj.ymin) * $iy / (height - 1))) as $y
    | Mandeliter( $x; $y; iterations ) as $i
    | if $i == iterations then
        pixel($ix; $iy; 0; 0; 0; 255)
      else
        (3 * ($i|log)/((iterations - 1.0)|log)) as $c  # redness
        | if $c < 1 then
            pixel($ix;$iy; 255*$c; 0; 0; 255)
          elif $c < 2 then
            pixel($ix;$iy; 255; 255*($c-1); 0; 255)
          else
            pixel($ix;$iy; 255; 255; 255*($c-2); 255)
          end
      end;

  svg("mandelbrot"; "100%"; "100%"),
  pixies,
  "</svg>";
```

'''Example''':

```jq
 Mandelbrot( {"xmin": -2, "xmax": 1, "ymin": -1, "ymax":1}; 900; 600; 1000 ) 
```


'''Execution:'''

```txt

 $ jq -n -r -f mandelbrot.jq > mandelbrot.svg

```


The output can be viewed in a web browser such as Chrome, Firefox, or Safari.


## Julia


Generates an ASCII representation:

```python
function mandelbrot(a)
    z = 0
    for i=1:50
        z = z^2 + a
    end
    return z
end

for y=1.0:-0.05:-1.0
    for x=-2.0:0.0315:0.5
        abs(mandelbrot(complex(x, y))) < 2 ? print("*") : print(" ")
    end
    println()
end
```


This generates a PNG image:

```python
using Images

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

    (r + m), (b + m), (g + m)
end

function mandelbrot()

    const w, h = 1000, 1000

    const zoom  = 0.5
    const moveX = 0
    const moveY = 0

    const img = Array{RGB{Float64}}(h, w)
    const maxIter = 30

    for x in 1:w
        for y in 1:h
            i = maxIter
            const c = Complex(
                (2*x - w) / (w * zoom) + moveX,
                (2*y - h) / (h * zoom) + moveY
            )
            z = c
            while abs(z) < 2 && (i -= 1) > 0
                z = z^2 + c
            end
            const r,g,b = hsv2rgb(i / maxIter * 360, 1, i / maxIter)
            img[y,x] = RGB{Float64}(r, g, b)
        end
    end

    save("mandelbrot_set.png", img)
end

mandelbrot()
```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.2

import java.awt.Graphics
import java.awt.image.BufferedImage
import javax.swing.JFrame

class Mandelbrot: JFrame("Mandelbrot Set") {
    companion object {
        private const val MAX_ITER = 570
        private const val ZOOM = 150.0
    }

    private val img: BufferedImage

    init {
        setBounds(100, 100, 800, 600)
        isResizable = false
        defaultCloseOperation = EXIT_ON_CLOSE
        img = BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
        for (y in 0 until height) {
            for (x in 0 until width) {
                var zx = 0.0
                var zy = 0.0
                val cX = (x - 400) / ZOOM
                val cY = (y - 300) / ZOOM
                var iter = MAX_ITER
                while (zx * zx + zy * zy < 4.0 && iter > 0) {
                    val tmp = zx * zx - zy * zy + cX
                    zy = 2.0 * zx * zy + cY
                    zx = tmp
                    iter--
                }
                img.setRGB(x, y, iter or (iter shl 7))
            }
        }
    }

    override fun paint(g: Graphics) {
        g.drawImage(img, 0, 0, this)
    }
}

fun main(args: Array<String>) {
    Mandelbrot().isVisible = true
}
```



## LabVIEW

{{works with|LabVIEW|8.0 Full Development Suite}}<br/><br/>
[[File:Mandelbrot_set_diagram.png|800px]]
[[File:Mandelbrot_set_panel.png|400px]]


## Lang5


```Lang5

: d2c(*,*) 2 compress 'c dress ;        # Make a complex number.

: iterate(c) [0 0](c) "dup * over +" steps reshape execute ;

: print_line(*) "#*+-. " "" split swap subscript "" join . "\n" . ;

75 iota 45 - 20 /                       # x coordinates
29 iota 14 - 10 /                       # y cordinates
'd2c outer                              # Make complex matrix.

10 'steps set                           # How many iterations?

iterate abs int 5 min 'print_line apply # Compute & print 

```



## Lasso


```Lasso

define mandelbrotBailout => 16
define mandelbrotMaxIterations => 1000

define mandelbrotIterate(x, y) => {
	local(cr = #y - 0.5,
		ci = #x, 
		zi = 0.0, 
		zr = 0.0, 
		i = 0, 
		temp, zr2, zi2)

	{
		++#i;
		#temp = #zr * #zi
		#zr2 = #zr * #zr
		#zi2 = #zi * #zi
				
		#zi2 + #zr2 > mandelbrotBailout?
			return #i
		#i > mandelbrotMaxIterations?
			return 0

		#zr = #zr2 - #zi2 + #cr
		#zi = #temp + #temp + #ci
		
		currentCapture->restart
	}()
}

define mandelbrotTest() => {
	local(x, y = -39.0)
	{
		stdout('\n')
		#x = -39.0
		{
			mandelbrotIterate(#x / 40.0, #y / 40.0) == 0?
				stdout('*')
				| stdout(' ');
			++#x
			#x <= 39.0?
				currentCapture->restart
		}();
		++#y
		
		#y <= 39.0?
			currentCapture->restart
	}()
	stdout('\n')
}

mandelbrotTest

```

{{out}}
<small>

```txt

                                       *                                       
                                       *                                       
                                       *                                       
                                       *                                       
                                       *                                       
                                      ***                                      
                                     *****                                     
                                     *****                                     
                                      ***                                      
                                       *                                       
                                   *********                                   
                                 *************                                 
                                ***************                                
                             *********************                             
                             *********************                             
                              *******************                              
                              *******************                              
                              *******************                              
                              *******************                              
                            ***********************                            
                              *******************                              
                              *******************                              
                             *********************                             
                              *******************                              
                              *******************                              
                               *****************                               
                                ***************                                
                                 *************                                 
                                   *********                                   
                                       *                                       
                                ***************                                
                            ***********************                            
                         * ************************* *                         
                         *****************************                         
                      * ******************************* *                      
                       *********************************                       
                      ***********************************                      
                    ***************************************                    
               *** ***************************************** ***               
               *************************************************               
                ***********************************************                
                 *********************************************                 
                 *********************************************                 
                ***********************************************                
                ***********************************************                
              ***************************************************              
               *************************************************               
               *************************************************               
              ***************************************************              
              ***************************************************              
         *    ***************************************************    *         
       *****  ***************************************************  *****       
       ****** *************************************************** ******       
      ******* *************************************************** *******      
    ***********************************************************************    
    ********* *************************************************** *********    
       ****** *************************************************** ******       
       *****  ***************************************************  *****       
              ***************************************************              
              ***************************************************              
              ***************************************************              
              ***************************************************              
               *************************************************               
               *************************************************               
              ***************************************************              
                ***********************************************                
                ***********************************************                
                  *******************************************                  
                   *****************************************                   
                 *********************************************                 
                **** ****************** ****************** ****                
                 ***  ****************   ****************  ***                 
                  *    **************     **************    *                  
                         ***********       ***********                         
                         **  *****           *****  **                         
                          *   *                 *   *                          

```

</small>


## LIL

From the source distribution. Produces a PBM, not shown here.

```tcl
#
# A mandelbrot generator that outputs a PBM file. This can be used to measure
# performance differences between LIL versions and measure performance
# bottlenecks (although keep in mind that LIL is not supposed to be a fast
# language, but a small one which depends on C for the slow parts - in a real
# program where for some reason mandelbrots are required, the code below would
# be written in C). The code is based on the mandelbrot test for the Computer
# Language Benchmarks Game at http://shootout.alioth.debian.org/
#
# In my current computer (Intel Core2Quad Q9550 @ 2.83GHz) running x86 Linux
# the results are (using the default 256x256 size):
#
#  2m3.634s  - commit 1c41cdf89f4c1e039c9b3520c5229817bc6274d0 (Jan 10 2011)
#
# To test call
#
#  time ./lil mandelbrot.lil > mandelbrot.pbm
#
# with an optimized version of lil (compiled with CFLAGS=-O3 make).
#

set width [expr $argv]
if not $width { set width 256 }
set height $width
set bit_num 0
set byte_acc 0
set iter 50
set limit 2.0

write "P4\n${width} ${height}\n"

for {set y 0} {$y < $height} {inc y} {
   for {set x 0} {$x < $width} {inc x} {
       set Zr 0.0 Zi 0.0 Tr 0.0 Ti 0.0
       set Cr [expr 2.0 * $x / $width - 1.5]
       set Ci [expr 2.0 * $y / $height - 1.0]
       for {set i 0} {$i < $iter && $Tr + $Ti <= $limit * $limit} {inc i} {
           set Zi [expr 2.0 * $Zr * $Zi + $Ci]
           set Zr [expr $Tr - $Ti + $Cr]
           set Tr [expr $Zr * $Zr]
           set Ti [expr $Zi * $Zi]
       }

       set byte_acc [expr $byte_acc << 1]
       if [expr $Tr + $Ti <= $limit * $limit] {
           set byte_acc [expr $byte_acc | 1]
       }

       inc bit_num

       if [expr $bit_num == 8] {
           writechar $byte_acc
           set byte_acc 0
           set bit_num 0
       } {if [expr $x == $width - 1] {
           set byte_acc [expr 8 - $width % 8]
           writechar $byte_acc
           set byte_acc 0
           set bit_num 0
       }}
   }
}
```



## Logo

{{works with|UCB Logo}}

```logo
to mandelbrot :left :bottom :side :size
  cs setpensize [1 1]
  make "inc :side/:size
  make "zr :left
  repeat :size [
    make "zr :zr + :inc
    make "zi :bottom
    pu
    setxy repcount - :size/2  minus :size/2
    pd
    repeat :size [
      make "zi :zi + :inc
      setpencolor count.color calc :zr :zi
      fd 1 ] ]
end

to count.color :count
  ;op (list :count :count :count)
  if :count > 256 [op 0]	; black
  if :count > 128 [op 7]	; white
  if :count >  64 [op 5]	; magenta
  if :count >  32 [op 6]	; yellow
  if :count >  16 [op 4]	; red
  if :count >   8 [op 2]	; green
  if :count >   4 [op 1]	; blue
  op 3				; cyan
end

to calc :zr :zi [:count 0] [:az 0] [:bz 0]
  if :az*:az + :bz*:bz > 4 [op :count]
  if :count > 256 [op :count]
  op (calc :zr :zi (:count + 1) (:zr + :az*:az - :bz*:bz) (:zi + 2*:az*:bz))
end

mandelbrot -2 -1.25 2.5 400
```



## Lua

Needs L&Ouml;VE 2D Engine<br />Zoom in: drag the mouse; zoom out: right click

```lua

local maxIterations = 250
local minX, maxX, minY, maxY = -2.5, 2.5, -2.5, 2.5
local miX, mxX, miY, mxY
function remap( x, t1, t2, s1, s2 )
    local f = ( x - t1 ) / ( t2 - t1 )
    local g = f * ( s2 - s1 ) + s1
    return g;
end
function drawMandelbrot()
    local pts, a, as, za, b, bs, zb, cnt, clr = {}
    for j = 0, hei - 1 do
        for i = 0, wid - 1 do
            a = remap( i, 0, wid, minX, maxX )
            b = remap( j, 0, hei, minY, maxY )
            cnt = 0; za = a; zb = b
            while( cnt < maxIterations ) do
                as = a * a - b * b; bs = 2 * a * b
                a = za + as; b = zb + bs
                if math.abs( a ) + math.abs( b ) > 16 then break end
                cnt = cnt + 1
            end
            if cnt == maxIterations then clr = 0
            else clr = remap( cnt, 0, maxIterations, 0, 255 )
            end
            pts[1] = { i, j, clr, clr, 0, 255 }
            love.graphics.points( pts )
        end
    end
end
function startFractal()
    love.graphics.setCanvas( canvas ); love.graphics.clear()
    love.graphics.setColor( 255, 255, 255 )
    drawMandelbrot(); love.graphics.setCanvas()
end
function love.load()
    wid, hei = love.graphics.getWidth(), love.graphics.getHeight()
    canvas = love.graphics.newCanvas( wid, hei )
    startFractal()
end
function love.mousepressed( x, y, button, istouch )
    if button ==  1 then
        startDrag = true; miX = x; miY = y
    else
        minX = -2.5; maxX = 2.5; minY = minX; maxY = maxX
        startFractal()
        startDrag = false
    end
end
function love.mousereleased( x, y, button, istouch )
    if startDrag then
        local l
        if x > miX then mxX = x
        else l = x; mxX = miX; miX = l
        end
        if y > miY then mxY = y
        else l = y; mxY = miY; miY = l
        end
        miX = remap( miX, 0, wid, minX, maxX ) 
        mxX = remap( mxX, 0, wid, minX, maxX )
        miY = remap( miY, 0, hei, minY, maxY ) 
        mxY = remap( mxY, 0, hei, minY, maxY )
        minX = miX; maxX = mxX; minY = miY; maxY = mxY
        startFractal()
    end
end
function love.draw()
    love.graphics.draw( canvas )
end

```



## M2000 Interpreter

Console is a bitmap so we can plot on it. A subroutine plot different size of pixels so we get Mandelbrot image at final same size for 32X26 for a big pixel of 16x16 pixels to 512x416 for a 1:1 pixel. Iterations for each pixel set to 25. Module can get left top corner as twips, and the size factor from 1 to 16 (size of output is 512x416 pixels for any factor).


```M2000 Interpreter

Module Mandelbrot(x=0&,y=0&,z=1&) {
      If z<1  then z=1
      If z>16 then z=16
      Const iXmax=32*z
      Const iYmax=26*z
      Def single Cx, Cy, CxMin=-2.05, CxMax=0.85, CyMin=-1.2,  CyMax=1.2
      Const PixelWidth=(CxMax-CxMin)/iXmax, iXm=(iXmax-1)*PixelWidth
      Const PixelHeight=(CyMax-CyMin)/iYmax,Ph2=PixelHeight/2
      Const Iteration=25
      Const EscRadious=2.5, ER2=EscRadious**2
      Def single preview
      preview=iXmax*twipsX*(z/16)
      Def long yp, xp, dx, dy, dx1, dy1
      Let dx=twipsx*(16/z), dx1=dx-1
      Let dy=twipsy*(16/z), dy1=dy-1
      yp=y
      For iY=0 to (iYmax-1)*PixelHeight step PixelHeight {
            Cy=CyMin+iY
            xp=x
            if abs(Cy)<Ph2 Then Cy=0
            For iX=0 to iXm Step PixelWidth {
                  Let  Cx=CxMin+iX,Zx=0,Zy=0,Zx2=Zx**2,Zy2=Zy**2
                  For It=Iteration to 1 {Let Zy=2*Zx*Zy+Cy,Zx=Zx2-Zy2+Cx,Zx2=Zx**2,Zy2=Zy**2 :if Zx2+Zy2>ER2 Then exit
                  }
                  if it>13 then {it-=13} else.if it=0 then SetPixel(xp,yp,0): xp+=dx : continue
                  it*=10:SetPixel(xp,yp,color(it, it,255)) :xp+=dx
            } : yp+=dy
      }
      Sub SetPixel()
            move number, number: fill  dx1, dy1, number
      End Sub
}
Cls 1,0
sz=(1,2,4,8,16)
i=each(sz)
While i {
      Mandelbrot 250*twipsx,100*twipsy, array(i)
}


```


Version 2 without Subroutine. Also there is a screen refresh every 2 seconds.

```M2000 Interpreter

Module Mandelbrot(x=0&,y=0&,z=1&) {
      If z<1  then z=1
      If z>16 then z=16
      Const iXmax=32*z
      Const iYmax=26*z
      Def single Cx, Cy, CxMin=-2.05, CxMax=0.85, CyMin=-1.2,  CyMax=1.2
      Const PixelWidth=(CxMax-CxMin)/iXmax, iXm=(iXmax-1)*PixelWidth
      Const PixelHeight=(CyMax-CyMin)/iYmax,Ph2=PixelHeight/2
      Const Iteration=25
      Const EscRadious=2.5, ER2=EscRadious**2
      Def single preview
      preview=iXmax*twipsX*(z/16)
      Def long yp, xp, dx, dy, dx1, dy1
      Let dx=twipsx*(16/z), dx1=dx-1
      Let dy=twipsy*(16/z), dy1=dy-1
      yp=y
      Refresh 2000
      For iY=0 to (iYmax-1)*PixelHeight step PixelHeight {
            Cy=CyMin+iY
            xp=x
            if abs(Cy)<Ph2 Then Cy=0
            move xp, yp
            For iX=0 to iXm Step PixelWidth {
                  Let  Cx=CxMin+iX,Zx=0,Zy=0,Zx2=Zx**2,Zy2=Zy**2
                  For It=Iteration to 1 {Let Zy=2*Zx*Zy+Cy,Zx=Zx2-Zy2+Cx,Zx2=Zx**2,Zy2=Zy**2 :if Zx2+Zy2>ER2 Then exit
                  }
                  if it>13 then {it-=13} else.if it=0 then fill  dx1, dy1, 0: Step 0,-dy1: continue
                  it*=10:fill  dx1, dy1, color(it, it,255): Step 0,-dy1
            } : yp+=dy
      }

}
Cls 1,0
sz=(1,2,4,8,16)
i=each(sz)
While i {
      Mandelbrot 250*twipsx,100*twipsy, array(i)
}

```



## Maple

<lang>ImageTools:-Embed(Fractals[EscapeTime]:-Mandelbrot(500, -2.0-1.35*I, .7+1.35*I, output = layer1));
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
The implementation could be better. But this is a start...

```mathematica
eTime[z0_, maxIter_Integer: 100] := (Length@NestWhileList[(# + z0)^2 &, 0, (Abs@# <= 2) &, 1, maxIter]) - 1

DistributeDefinitions[eTime];
mesh = ParallelTable[eTime[(x + I*y), 1000], {y, 1.2, -1.2, -0.01}, {x, -1.72, 1, 0.01}];
ReliefPlot[mesh, Frame -> False]
```

Faster version:

```mathematica
cf = With[{
      mandel = Block[{z = #, c = #}, 
        Catch@Do[If[Abs[z] > 2, Throw@i]; z = z^2 + c, {i, 100}]] &
    },
   Compile[{},Table[mandel[y + x I], {x, -1, 1, 0.005}, {y, -2, 0.5, 0.005}]]
  ];
ArrayPlot[cf[]] 
```

Built-in function:

```mathematica
MandelbrotSetPlot[]
```


==Mathmap ==

 filter mandelbrot (gradient coloration)
    c=ri:(xy/xy:[X,X]*1.5-xy:[0.5,0]);
    z=ri:[0,0]; # initial value z0 = 0 
    # iteration of z
    iter=0;
    while abs(z)<2 && iter<31
    do
        z=z*z+c;  # z(n+1) = fc(zn)
        iter=iter+1
    end;
    coloration(iter/32) # color of pixel
 end


## MATLAB


This solution uses the escape time algorithm to determine the coloring of the coordinates on the complex plane. The code can be reduced to a single line via vectorization after the Escape Time Algorithm function definition, but the code becomes unnecessarily obfuscated. Also, this code uses a lot of memory. You will need a computer with a lot of memory to compute the set with high resolution.


```MATLAB
function [theSet,realAxis,imaginaryAxis] = mandelbrotSet(start,gridSpacing,last,maxIteration)

    %Define the escape time algorithm
    function escapeTime = escapeTimeAlgorithm(z0)
        
        escapeTime = 0;
        z = 0;
        
        while( (abs(z)<=2) && (escapeTime < maxIteration) )
            z = (z + z0)^2;            
            escapeTime = escapeTime + 1;
        end
                
    end
    
    %Define the imaginary axis
    imaginaryAxis = (imag(start):imag(gridSpacing):imag(last));
    
    %Define the real axis
    realAxis = (real(start):real(gridSpacing):real(last));
    
    %Construct the complex plane from the real and imaginary axes
    complexPlane = meshgrid(realAxis,imaginaryAxis) + meshgrid(imaginaryAxis(end:-1:1),realAxis)'.*i;
    
    %Apply the escape time algorithm to each point in the complex plane 
    theSet = arrayfun(@escapeTimeAlgorithm, complexPlane);
    

    %Draw the set
    pcolor(realAxis,imaginaryAxis,theSet);
    shading flat;
    
end
```


To use this function you must specify the:
<ol>
  <li>lower left hand corner of the complex plane from which to start the image,  
  <li>the grid spacing in both the imaginary and real directions,
  <li>the upper right hand corner of the complex plane at which to end the image and
  <li>the maximum iterations for the escape time algorithm.
</ol>

For example:
<ol>
  <li>Lower Left Corner: -2.05-1.2i
  <li>Grid Spacing: 0.004+0.0004i
  <li>Upper Right Corner: 0.45+1.2i
  <li>Maximum Iterations: 500
</ol>

Sample usage:

```MATLAB
mandelbrotSet(-2.05-1.2i,0.004+0.0004i,0.45+1.2i,500);
```



## Metapost


```Metapost
prologues:=3;
outputtemplate:="%j-%c.svg";
outputformat:="svg";


def mandelbrot(expr maxX, maxY) =
  max_iteration := 500;

  color col[];
  for i := 0 upto max_iteration:
    t := i / max_iteration;
    col[i] = (t,t,t);
  endfor;
  

  for px := 0 upto maxX:
    for py := 0 upto maxY:
      xz := px * 3.5 / maxX - 2.5;  % (-2.5,1)
      yz := py * 2 / maxY - 1;      % (-1,1)

      x := 0;
      y := 0;

      iteration := 0;

      forever: exitunless ((x*x + y*y < 4) and (iteration < max_iteration));
        xtemp := x*x - y*y + xz;
        y := 2*x*y + yz;
        x := xtemp;
        iteration := iteration + 1;
      endfor;

      draw (px,py) withpen pencircle withcolor col[iteration];

    endfor;
  endfor;
enddef;


beginfig(1);
  mandelbrot(200, 150);
endfig;

end
```


Sample usage:

```Bash
mpost -numbersystem="double" mandelbrot.mp
```



## MiniScript


```MiniScript
ZOOM = 100
MAX_ITER = 40
gfx.clear color.black
for y in range(0,200)
	for x in range(0,300)
		zx = 0
		zy = 0
		cx = (x - 200) / ZOOM
		cy = (y - 100) / ZOOM
		for iter in range(MAX_ITER)
			if zx*zx + zy*zy > 4 then break
			tmp = zx * zx - zy * zy + cx
			zy = 2 * zx * zy + cy
			zx = tmp
		end for
		if iter then
			gfx.setPixel x, y, rgb(255-iter*6, 0, iter*6)
		end if
	end for
end for
```


(Will upload an output image as soon as image uploading is fixed.)

=={{header|Modula-3}}==

```modula3
MODULE Mandelbrot EXPORTS Main;

IMPORT Wr, Stdio, Fmt, Word;

CONST m = 50;
      limit2 = 4.0;

TYPE UByte = BITS 8 FOR [0..16_FF];

VAR width := 200;
    height := 200;
    bitnum: CARDINAL := 0;
    byteacc: UByte := 0;
    isOverLimit: BOOLEAN;
    Zr, Zi, Cr, Ci, Tr, Ti: REAL;

BEGIN
  
  Wr.PutText(Stdio.stdout, "P4\n" & Fmt.Int(width) & " " & Fmt.Int(height) & "\n");

  FOR y := 0 TO height - 1 DO
    FOR x := 0 TO width - 1 DO
      Zr := 0.0; Zi := 0.0;
      Cr := 2.0 * FLOAT(x) / FLOAT(width) - 1.5;
      Ci := 2.0 * FLOAT(y) / FLOAT(height) - 1.0;
      
      FOR i := 1 TO m + 1 DO
        Tr := Zr*Zr - Zi*Zi + Cr;
        Ti := 2.0*Zr*Zi + Ci;
        Zr := Tr; Zi := Ti;
        isOverLimit := Zr*Zr + Zi*Zi > limit2;
        IF isOverLimit THEN EXIT; END;
      END;
      
      IF isOverLimit THEN
        byteacc := Word.Xor(Word.LeftShift(byteacc, 1), 16_00);
      ELSE
        byteacc := Word.Xor(Word.LeftShift(byteacc, 1), 16_01);
      END;

      INC(bitnum);
      
      IF bitnum = 8 THEN
        Wr.PutChar(Stdio.stdout, VAL(byteacc, CHAR));
        byteacc := 0;
        bitnum := 0;
      ELSIF x = width - 1 THEN
        byteacc := Word.LeftShift(byteacc, 8 - (width MOD 8));
        Wr.PutChar(Stdio.stdout, VAL(byteacc, CHAR));
        byteacc := 0;
        bitnum := 0
      END;
      Wr.Flush(Stdio.stdout);
    END;
  END;
END Mandelbrot.
```



## MySQL


See http://arbitraryscrawl.blogspot.co.uk/2012/06/fractsql.html for an explanation.


```mysql

-- Table to contain all the data points
CREATE TABLE points (
  c_re DOUBLE,
  c_im DOUBLE,
  z_re DOUBLE DEFAULT 0,
  z_im DOUBLE DEFAULT 0,
  znew_re DOUBLE DEFAULT 0,
  znew_im DOUBLE DEFAULT 0,
  steps INT DEFAULT 0,
  active CHAR DEFAULT 1
);

DELIMITER |

-- Iterate over all the points in the table 'points'
CREATE PROCEDURE itrt (IN n INT)
BEGIN
  label: LOOP
    UPDATE points
      SET
        znew_re=POWER(z_re,2)-POWER(z_im,2)+c_re,
        znew_im=2*z_re*z_im+c_im,
        steps=steps+1
      WHERE active=1;
    UPDATE points SET
        z_re=znew_re,
        z_im=znew_im,
        active=IF(POWER(z_re,2)+POWER(z_im,2)>4,0,1)
      WHERE active=1;
    SET n = n - 1;
    IF n > 0 THEN
      ITERATE label;
    END IF;
    LEAVE label;
  END LOOP label;
END|

-- Populate the table 'points'
CREATE PROCEDURE populate (
  r_min DOUBLE,
  r_max DOUBLE,
  r_step DOUBLE,
  i_min DOUBLE,
  i_max DOUBLE,
  i_step DOUBLE)
BEGIN
  DELETE FROM points;
  SET @rl = r_min;
  SET @a = 0;
  rloop: LOOP
    SET @im = i_min;
    SET @b = 0;
    iloop: LOOP
      INSERT INTO points (c_re, c_im)
        VALUES (@rl, @im);
      SET @b=@b+1;
      SET @im=i_min + @b * i_step;
      IF @im < i_max THEN
        ITERATE iloop;
      END IF;
      LEAVE iloop;
    END LOOP iloop;
      SET @a=@a+1;
    SET @rl=r_min + @a * r_step;
    IF @rl < r_max THEN
      ITERATE rloop;
    END IF;
    LEAVE rloop;
  END LOOP rloop;
END|

DELIMITER ;

-- Choose size and resolution of graph
--             R_min, R_max, R_step, I_min, I_max, I_step
CALL populate( -2.5,  1.5,   0.005,  -2,    2,     0.005 );

-- Calculate 50 iterations
CALL itrt( 50 );

-- Create the image (/tmp/image.ppm)
-- Note, MySQL will not over-write an existing file and you may need
-- administrator access to delete or move it
SELECT @xmax:=COUNT(c_re) INTO @xmax FROM points GROUP BY c_im LIMIT 1;
SELECT @ymax:=COUNT(c_im) INTO @ymax FROM points GROUP BY c_re LIMIT 1;
SET group_concat_max_len=11*@xmax*@ymax;
SELECT
  'P3', @xmax, @ymax, 200,
  GROUP_CONCAT(
    CONCAT(
      IF( active=1, 0, 55+MOD(steps, 200) ), ' ',
      IF( active=1, 0, 55+MOD(POWER(steps,3), 200) ), ' ',
      IF( active=1, 0, 55+MOD(POWER(steps,2), 200) ) )
    ORDER BY c_im ASC, c_re ASC SEPARATOR ' ' )
    INTO OUTFILE '/tmp/image.ppm'
  FROM points;

```



## Nim


{{trans|Python}}


```nim
import complex

proc mandelbrot(a: Complex): Complex =
  for i in 0 .. <50:
    result = result * result + a
 
iterator stepIt(start, step: float, iterations: int): auto =
  for i in 0 .. iterations:
    yield start + float(i) * step

var rows = ""
for y in stepIt(1.0, -0.05, 41):
  for x in stepIt(-2.0, 0.0315, 80):
    if abs(mandelbrot((x,y))) < 2:
      rows.add('*')
    else:
      rows.add(' ')
  rows.add("\n")
 
echo rows
```



## OCaml



```ocaml
#load "graphics.cma";;

let mandelbrot xMin xMax yMin yMax xPixels yPixels maxIter =
  let rec mandelbrotIterator z c n =
    if (Complex.norm z) > 2.0 then false else
      match n with
      | 0 -> true
      | n -> let z' = Complex.add (Complex.mul z z) c in
             mandelbrotIterator z' c (n-1) in
  Graphics.open_graph
    (" "^(string_of_int xPixels)^"x"^(string_of_int yPixels));
  let dx = (xMax -. xMin) /. (float_of_int xPixels) 
  and dy = (yMax -. yMin) /. (float_of_int yPixels) in
  for xi = 0 to xPixels - 1 do
    for yi = 0 to yPixels - 1 do
      let c = {Complex.re = xMin +. (dx *. float_of_int xi);
               Complex.im = yMin +. (dy *. float_of_int yi)} in
      if (mandelbrotIterator Complex.zero c maxIter) then
        (Graphics.set_color Graphics.white;
         Graphics.plot xi yi )
      else
        (Graphics.set_color Graphics.black;
         Graphics.plot xi yi )
    done
  done;;
 
mandelbrot (-1.5) 0.5 (-1.0) 1.0 500 500 200;;
```



## Octave


This code runs rather slowly and produces coloured Mandelbrot set by accident ([[Media:Mandel-Octave.jpg|output image]]). 


```octave
#! /usr/bin/octave -qf
global width = 200;
global height = 200;
maxiter = 100;

z0 = 0;
global cmax = 1 + i;
global cmin = -2 - i;

function cs = pscale(c)
  global cmax;
  global cmin;
  global width;
  global height;
  persistent px = (real(cmax-cmin))/width;
  persistent py = (imag(cmax-cmin))/height;
  cs = real(cmin) + px*real(c) + i*(imag(cmin) + py*imag(c));
endfunction

ms = zeros(width, height);
for x = 0:width-1
  for y = 0:height-1
    z0 = 0;
    c = pscale(x+y*i);
    for ic = 1:maxiter
      z1 = z0^2 + c;
      if ( abs(z1) > 2 ) break; endif
      z0 = z1;
    endfor
    ms(x+1, y+1) = ic/maxiter;
  endfor
endfor

saveimage("mandel.ppm", round(ms .* 255).', "ppm");
```



## Ol


```scheme

(define x-size 59)
(define y-size 21)
(define min-im -1)
(define max-im 1)
(define min-re -2)
(define max-re 1)

(define step-x (/ (- max-re min-re) x-size))
(define step-y (/ (- max-im min-im) y-size))

(for-each (lambda (y)
      (let ((im (+ min-im (* step-y y))))
         (for-each (lambda (x)
            (let*((re (+ min-re (* step-x x)))
                  (zr (inexact re))
                  (zi (inexact im)))
               (let loop ((n 0) (zi zi) (zr zr))
                  (let ((a (* zr zr))
                        (b (* zi zi)))
                     (cond
                        ((> (+ a b) 4)
                           (display (string (- 62 n))))
                        ((= n 30)
                           (display (string (- 62 n))))
                        (else
                           (loop (+ n 1) (+ (* 2 zr zi) im) (- (+ a re) b))))))))
            (iota x-size))
         (print)))
   (iota y-size))

```


Output:

```txt

>>>>>>
### ==<<<<<<<<<<<<<<<;;;;;;:::96032:;;;;<<<<=======

>>>>>===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### =

>>>>===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<======
>>>==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<====
>>==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<===
>>=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<==
>=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<=
><<<<;;;;;:::972456-567763                      +9;;<<<<<<<
><;;;;;;::::9875&      .3                       *9;;;<<<<<<
>;;;;;;::997564'        '                       8:;;;<<<<<<
>::988897735/                                 &89:;;;<<<<<<
>::988897735/                                 &89:;;;<<<<<<
>;;;;;;::997564'        '                       8:;;;<<<<<<
><;;;;;;::::9875&      .3                       *9;;;<<<<<<
><<<<;;;;;:::972456-567763                      +9;;<<<<<<<
>=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<=
>>=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<==
>>==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<===
>>>==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<====
>>>>===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<======
>>>>>===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### =


```



## PARI/GP

Define function mandelbrot():

```parigp
mandelbrot() = 
{
  forstep(y=-1, 1, 0.05, 
    forstep(x=-2, 0.5, 0.0315,
      print1(((c)->my(z=c);for(i=1,20,z=z*z+c;if(abs(z)>2,return(" ")));"#")(x+y*I)));
    print());
}
```


Output:
```txt
gp > mandelbrot()
                                                                                
                                                                                
                                                            #                   
                                                        #  ###  #               
                                                        ########                
                                                       #########                
                                                         ######                 
                                             ##    ## ############  #           
                                              ### ###################      #    
                                              #############################     
                                              ############################      
                                          ################################      
                                           ################################     
                                         #################################### # 
                          #     #        ###################################    
                          ###########    ###################################    
                           ###########   #####################################  
                         ############## ####################################    
                        ####################################################    
                     ######################################################     
#########################################################################       
                     ######################################################     
                        ####################################################    
                         ############## ####################################    
                           ###########   #####################################  
                          ###########    ###################################    
                          #     #        ###################################    
                                         #################################### # 
                                           ################################     
                                          ################################      
                                              ############################      
                                              #############################     
                                              ### ###################      #    
                                             ##    ## ############  #           
                                                         ######                 
                                                       #########                
                                                        ########                
                                                        #  ###  #               
                                                            #                   
                                                                                
                                                                                
```



## Pascal

{{trans|C}}

```pascal
program mandelbrot;

const
   ixmax = 800;
   iymax = 800;
   cxmin = -2.5;
   cxmax =  1.5;
   cymin = -2.0;
   cymax =  2.0;
   maxcolorcomponentvalue = 255;
   maxiteration = 200;
   escaperadius = 2;

type
   colortype = record
      red   : byte;
      green : byte;
      blue  : byte;
   end;

var
   ix, iy      : integer;
   cx, cy      : real;
   pixelwidth  : real = (cxmax - cxmin) / ixmax;
   pixelheight : real = (cymax - cymin) / iymax;
   filename    : string = 'new1.ppm';
   comment     : string = '# ';
   outfile     : textfile;
   color       : colortype;
   zx, zy      : real;
   zx2, zy2    : real;
   iteration   : integer;
   er2         : real = (escaperadius * escaperadius);

begin
   {$I-}
   assign(outfile, filename);
   rewrite(outfile);
   if ioresult <> 0 then
   begin
      writeln(stderr, 'unable to open output file: ', filename);
      exit;
   end;

   writeln(outfile, 'P6');
   writeln(outfile, ' ', comment);
   writeln(outfile, ' ', ixmax);
   writeln(outfile, ' ', iymax);
   writeln(outfile, ' ', maxcolorcomponentvalue);

   for iy := 1 to iymax do
   begin
      cy := cymin + (iy - 1)*pixelheight;
      if abs(cy) < pixelheight / 2 then cy := 0.0;
      for ix := 1 to ixmax do
      begin
         cx := cxmin + (ix - 1)*pixelwidth;
         zx := 0.0;
         zy := 0.0;
         zx2 := zx*zx;
         zy2 := zy*zy;
         iteration := 0;
         while (iteration < maxiteration) and (zx2 + zy2 < er2) do
         begin
            zy := 2*zx*zy + cy;
            zx := zx2 - zy2 + cx;
            zx2 := zx*zx;
            zy2 := zy*zy;
            iteration := iteration + 1;
         end;
         if iteration = maxiteration then
         begin
            color.red   := 0;
            color.green := 0;
            color.blue  := 0;
         end
         else
         begin
            color.red   := 255;
            color.green := 255;
            color.blue  := 255;
         end;
         write(outfile, chr(color.red), chr(color.green), chr(color.blue));
      end;
   end;

   close(outfile);
end.

```



## Perl

translation / optimization of the ruby solution

```perl
use Math::Complex;

sub mandelbrot {
    my ($z, $c) = @_[0,0];
    for (1 .. 20) {
        $z = $z * $z + $c;
        return $_ if abs $z > 2;
    }
}

for (my $y = 1; $y >= -1; $y -= 0.05) {
    for (my $x = -2; $x <= 0.5; $x += 0.0315)
        {print mandelbrot($x + $y * i) ? ' ' : '#'}
    print "\n"
}
```



## Perl 6

{{Works with|rakudo|2016-05-01}}
[[File:Mandel-perl6.png|thumb]]
Variant of a Mandelbrot script from the [http://modules.perl6.org/ Perl 6 ecosystem]. Produces a [[Write ppm file|Portable Pixel Map]] to STDOUT. 
Redirect into a file to save it. 
Converted to a .png file for display here.


```perl6
constant @color_map = map ~*.comb(/../).map({:16($_)}), < 
000000 0000fc 4000fc 7c00fc bc00fc fc00fc fc00bc fc007c fc0040 fc0000 fc4000
fc7c00 fcbc00 fcfc00 bcfc00 7cfc00 40fc00 00fc00 00fc40 00fc7c 00fcbc 00fcfc
00bcfc 007cfc 0040fc 7c7cfc 9c7cfc bc7cfc dc7cfc fc7cfc fc7cdc fc7cbc fc7c9c
fc7c7c fc9c7c fcbc7c fcdc7c fcfc7c dcfc7c bcfc7c 9cfc7c 7cfc7c 7cfc9c 7cfcbc
7cfcdc 7cfcfc 7cdcfc 7cbcfc 7c9cfc b4b4fc c4b4fc d8b4fc e8b4fc fcb4fc fcb4e8
fcb4d8 fcb4c4 fcb4b4 fcc4b4 fcd8b4 fce8b4 fcfcb4 e8fcb4 d8fcb4 c4fcb4 b4fcb4
b4fcc4 b4fcd8 b4fce8 b4fcfc b4e8fc b4d8fc b4c4fc 000070 1c0070 380070 540070
700070 700054 700038 70001c 700000 701c00 703800 705400 707000 547000 387000
1c7000 007000 00701c 007038 007054 007070 005470 003870 001c70 383870 443870
543870 603870 703870 703860 703854 703844 703838 704438 705438 706038 707038
607038 547038 447038 387038 387044 387054 387060 387070 386070 385470 384470
505070 585070 605070 685070 705070 705068 705060 705058 705050 705850 706050
706850 707050 687050 607050 587050 507050 507058 507060 507068 507070 506870
506070 505870 000040 100040 200040 300040 400040 400030 400020 400010 400000
401000 402000 403000 404000 304000 204000 104000 004000 004010 004020 004030
004040 003040 002040 001040 202040 282040 302040 382040 402040 402038 402030
402028 402020 402820 403020 403820 404020 384020 304020 284020 204020 204028
204030 204038 204040 203840 203040 202840 2c2c40 302c40 342c40 3c2c40 402c40
402c3c 402c34 402c30 402c2c 40302c 40342c 403c2c 40402c 3c402c 34402c 30402c
2c402c 2c4030 2c4034 2c403c 2c4040 2c3c40 2c3440 2c3040
>;
 
constant MAX_ITERATIONS = 50;
my $width = my $height = +(@*ARGS[0] // 31);
 
sub cut(Range $r, UInt $n where $n > 1) {
    $r.min, * + ($r.max - $r.min) / ($n - 1) ... $r.max
}
 
my @re = cut(-2 .. 1/2, $height);
my @im = cut( 0 .. 5/4, $width div 2 + 1) X* 1i;
 
sub mandelbrot(Complex $z is copy, Complex $c) {
    for 1 .. MAX_ITERATIONS {
	$z = $z*$z + $c;
	return $_ if $z.abs > 2;
    }
    return 0;
}
 
say "P3";
say "$width $height";
say "255";
 
for @re -> $re {
    put @color_map[|.reverse, |.[1..*]][^$width] given
    my @ = map &mandelbrot.assuming(0i, *), $re «+« @im;
}
```



## Phix

;Ascii
This is included in the distribution (with some extra validation) as demo\mandle.exw

```Phix
--
-- Mandlebrot set in ascii art demo.
-- 
constant b=" .:,;!/>)|&IH%*#"
atom r, i, c, C, z, Z, t, k
    for y=30 to 0 by -1 do
        C = y*0.1-1.5
        puts(1,'\n')
        for x=0 to 74 do
            c = x*0.04-2
            z = 0
            Z = 0
            r = c
            i = C
            k = 0
            while k<112 do
                t = z*z-Z*Z+r
                Z = 2*z*Z+i
                z = t
                if z*z+Z*Z>10 then exit end if
                k += 1
            end while
            puts(1,b[remainder(k,16)+1])
        end for
    end for
```

;Graphical
This is included in the distribution as demo\arwendemo\mandel.exw

```Phix
include arwen.ew
include ..\arwen\dib256.ew

constant HelpText = "Left-click drag with the mouse to move the image.\n"&
                    " (the image is currently only redrawn on mouseup).\n"&
                    "Right-click-drag with the mouse to select a region to zoom in to.\n"&
                    "Use the mousewheel to zoom in and out (nb: can be slow).\n"&
                    "Press F2 to select iterations, higher==more detail but slower.\n"&
                    "Resize the window as you please, but note that going fullscreen, \n"&
                    "especially at high iteration, may mean a quite long draw time.\n"&
                    "Press Escape to close the window."

procedure Help()
    void = messageBox("Mandelbrot Set",HelpText,MB_OK)
end procedure

integer cWidth = 520    -- client area width
integer cHeight = 480   -- client area height

constant Main = create(Window, "Mandelbrot Set", 0, 0, 50, 50, cWidth+16, cHeight+38, 0),
         mainHwnd = getHwnd(Main),
         mainDC = getPrivateDC(Main),

         mIter = create(Menu, "", 0, 0, 0,0,0,0,0),
         iterHwnd = getHwnd(mIter),
         mIter50 = create(MenuItem,"50 (fast, low detail)",     0, mIter, 0,0,0,0,0),
         mIter100 = create(MenuItem,"100 (default)",            0, mIter, 0,0,0,0,0),
         mIter500 = create(MenuItem,"500",                      0, mIter, 0,0,0,0,0),
         mIter1000 = create(MenuItem,"1000 (slow, high detail)",0, mIter, 0,0,0,0,0),
         m50to1000 = {mIter50,mIter100,mIter500,mIter1000},
         i50to1000 = {     50,     100,     500,     1000}
        
integer mainDib = 0

constant whitePen = c_func(xCreatePen, {0,1,BrightWhite})
constant NULL_BRUSH = 5,
         NullBrushID = c_func(xGetStockObject,{NULL_BRUSH})

atom t0
integer iter
atom x0, y0     -- top-left coords to draw
atom scale      -- controls width/zoom

procedure init()
    x0 = -2
    y0 = -1.25
    scale = 2.5/cHeight
    iter = 100
    void = c_func(xSelectObject,{mainDC,whitePen})
    void = c_func(xSelectObject,{mainDC,NullBrushID})
end procedure
init()

function in_set(atom x, atom y)
atom u,t
    if x>-0.75 then
        u = x-0.25
        t = u*u+y*y
        return ((2*t+u)*(2*t+u)>t)
    else
        return ((x+1)*(x+1)+y*y)>0.0625
    end if
end function

function pixel_colour(atom x0, atom y0, integer iter)
integer count = 1
atom x = 0, y = 0
    while (count<=iter) and (x*x+y*y<4) do
        count += 1
        {x,y} = {x*x-y*y+x0,2*x*y+y0}
    end while
    if count<=iter  then return count end if
    return 0
end function

procedure mandel(atom x0, atom y0, atom scale)
atom x,y
integer c   
    t0 = time()
    y = y0
    for yi=1 to cHeight do
        x = x0
        for xi=1 to cWidth do
            c = 0   -- default to black
            if in_set(x,y) then
                c = pixel_colour(x,y,iter)
            end if
            setDibPixel(mainDib, xi, yi, c)
            x += scale
        end for
        y += scale
    end for
end procedure

integer firsttime = 1
integer drawBox = 0
integer drawTime = 0

procedure newDib()
sequence pal

    if mainDib!=0 then
        {} = deleteDib(mainDib)
    end if
    mainDib = createDib(cWidth, cHeight)
    pal = repeat({0,0,0},256)
    for i=2 to 256 do
        pal[i][1] = i*5
        pal[i][2] = 0
        pal[i][3] = i*10
    end for
    setDibPalette(mainDib, 1, pal)
    mandel(x0,y0,scale)
    drawTime = 2
end procedure

procedure reDraw()
    setText(Main,"Please Wait...")
    mandel(x0,y0,scale)
    drawTime = 2
    repaintWindow(Main,False)
end procedure

procedure zoom(integer z)
    while z do
        if z>0 then
            scale /= 1.1
            z -= 1
        else
            scale *= 1.1
            z += 1
        end if
    end while
    reDraw()
end procedure

integer dx=0,dy=0   -- mouse down coords
integer mx=0,my=0   -- mouse move/up coords

function mainHandler(integer id, integer msg, atom wParam, object lParam)
integer x, y    -- scratch vars
atom scale10

    if msg=WM_SIZE then -- (also activate/firsttime)
        {{},{},x,y} = getClientRect(Main)
        if firsttime or cWidth!=x or cHeight!=y then
            scale *= cWidth/x
            {cWidth, cHeight} = {x,y}
            newDib()
            firsttime = 0
        end if
    elsif msg=WM_PAINT then
        copyDib(mainDC, 0, 0, mainDib)
        if drawBox then
            void = c_func(xRectangle, {mainDC, dx, dy, mx, my})
        end if
        if drawTime then
            if drawTime=2 then
                setText(Main,sprintf("Mandelbrot Set [generated in %gs]",time()-t0))
            else
                setText(Main,"Mandelbrot Set")
            end if
            drawTime -= 1
        end if
    elsif msg=WM_CHAR then
        if wParam=VK_ESCAPE then
            closeWindow(Main)
        elsif wParam='+' then zoom(+1)
        elsif wParam='-' then zoom(-1)
        end if
    elsif msg=WM_LBUTTONDOWN
       or msg=WM_RBUTTONDOWN then
        {dx,dy} = lParam
    elsif msg=WM_MOUSEMOVE then
        if and_bits(wParam,MK_LBUTTON) then
            {mx,my} = lParam
            -- minus dx,dy (see WM_LBUTTONUP)
            -- DEV maybe a timer to redraw, but probably too slow...
            --  (this is where we need a background worker thread,
            --   ideally one we can direct to abandon what it is
            --   currently doing and start work on new x,y instead)
        elsif and_bits(wParam,MK_RBUTTON) then
            {mx,my} = lParam
            drawBox = 1
            repaintWindow(Main,False)
        end if
    elsif msg=WM_MOUSEWHEEL then
        wParam = floor(wParam/#10000)
        if wParam>=#8000 then   -- sign bit set
            wParam-=#10000
        end if
        wParam = floor(wParam/120)  -- (gives +/-1, usually)
        zoom(wParam)
    elsif msg=WM_LBUTTONUP then
        {mx,my} = lParam
        drawBox = 0
        x0 += (dx-mx)*scale
        y0 += (dy-my)*scale
        reDraw()
    elsif msg=WM_RBUTTONUP then
        {mx,my} = lParam
        drawBox = 0
        if mx!=dx and my!=dy then
            x0 += min(mx,dx)*scale
            y0 += min(my,dy)*scale
            scale *= (abs(mx-dx))/cHeight
            reDraw()
        end if
    elsif msg=WM_KEYDOWN then
        if wParam=VK_F1 then
            Help()
        elsif wParam=VK_F2 then
            {x,y} = getWindowRect(Main)
            void = c_func(xTrackPopupMenu, {iterHwnd,TPM_LEFTALIGN,x+20,y+40,0,mainHwnd,NULL})
        elsif find(wParam,{VK_UP,VK_DOWN,VK_LEFT,VK_RIGHT}) then
            drawBox = 0
            scale10 = scale*10
            if wParam=VK_UP then
                y0 += scale10
            elsif wParam=VK_DOWN then
                y0 -= scale10
            elsif wParam=VK_LEFT then
                x0 += scale10
            elsif wParam=VK_RIGHT then
                x0 -= scale10
            end if
            reDraw()
        end if
    elsif msg=WM_COMMAND then
        id = find(id,m50to1000)
        if id!=0 then
            iter = i50to1000[id]
            reDraw()
        end if
    end if
    return 0
end function
setHandler({Main,mIter50,mIter100,mIter500,mIter1000}, routine_id("mainHandler"))

WinMain(Main,SW_NORMAL)
void = deleteDib(0)
```



## PHP

{{libheader|GD Graphics Library}}
{{works with|PHP|5.3.5}}

[[File:Mandel-php.png|thumb|right|Sample output]]


```PHP
$min_x=-2;
$max_x=1;
$min_y=-1;
$max_y=1;

$dim_x=400;
$dim_y=300;

$im = @imagecreate($dim_x, $dim_y)
  or die("Cannot Initialize new GD image stream");
header("Content-Type: image/png");
$black_color = imagecolorallocate($im, 0, 0, 0);
$white_color = imagecolorallocate($im, 255, 255, 255);

for($y=0;$y<=$dim_y;$y++) {
  for($x=0;$x<=$dim_x;$x++) {
    $c1=$min_x+($max_x-$min_x)/$dim_x*$x;
    $c2=$min_y+($max_y-$min_y)/$dim_y*$y;

    $z1=0;
    $z2=0;

    for($i=0;$i<100;$i++) {
      $new1=$z1*$z1-$z2*$z2+$c1;
      $new2=2*$z1*$z2+$c2;
      $z1=$new1;
      $z2=$new2;
      if($z1*$z1+$z2*$z2>=4) {
        break;
      }
    }
    if($i<100) {
      imagesetpixel ($im, $x, $y, $white_color);
    }
  }
}

imagepng($im);
imagedestroy($im);

```



## PicoLisp


```PicoLisp
(scl 6)

(let Ppm (make (do 300 (link (need 400))))
   (for (Y . Row) Ppm
      (for (X . @) Row
         (let (ZX 0  ZY 0  CX (*/ (- X 250) 1.0 150)  CY (*/ (- Y 150) 1.0 150)  C 570)
            (while (and (> 4.0 (+ (*/ ZX ZX 1.0) (*/ ZY ZY 1.0))) (gt0 C))
               (let Tmp (- (*/ ZX ZX 1.0) (*/ ZY ZY 1.0) (- CX))
                  (setq
                     ZY (+ (*/ 2 ZX ZY 1.0) CY)
                     ZX Tmp ) )
               (dec 'C) )
            (set (nth Ppm Y X) (list 0 C C)) ) ) )
   (out "img.ppm"
      (prinl "P6")
      (prinl 400 " " 300)
      (prinl 255)
      (for Y Ppm (for X Y (apply wr X))) ) )
```



## PostScript


```postscript
%!PS-Adobe-2.0
%%BoundingBox: 0 0 300 200
%%EndComments
/origstate save def
/ld {load def} bind def
/m /moveto ld /g /setgray ld
/dot { currentpoint 1 0 360 arc fill } bind def
%%EndProlog
% param
/maxiter 200 def
% complex manipulation
/complex { 2 array astore } def
/real { 0 get } def
/imag { 1 get } def
/cmul { /a exch def /b exch def
    a real b real mul
    a imag b imag mul sub
    a real b imag mul
    a imag b real mul add
    2 array astore
} def
/cadd { aload pop 3 -1 roll aload pop
    3 -1 roll add
    3 1 roll add exch 2 array astore
} def
/cconj { aload pop neg 2 array astore } def
/cabs2 { dup cconj cmul 0 get} def
% mandel
200 100 translate
-200 1 100 { /x exch def
  -100 1 100 { /y exch def
    /z0 0.0 0.0 complex def
    0 1 maxiter { /iter exch def
	x 100 div y 100 div complex
	z0 z0 cmul
	cadd dup /z0 exch def
	cabs2 4 gt {exit} if
    } for
    iter maxiter div g
    x y m dot
  } for
} for
%
showpage
origstate restore
%%EOF
```



## PowerShell


```PowerShell

$x = $y = $i = $j = $r = -16
$colors = [Enum]::GetValues([System.ConsoleColor])

while(($y++) -lt 15)
{
    for($x=0; ($x++) -lt 84; Write-Host " " -BackgroundColor ($colors[$k -band 15]) -NoNewline)
    {
        $i = $k = $r = 0

        do
        {
            $j = $r * $r - $i * $i -2 + $x / 25
            $i = 2 * $r * $i + $y / 10
            $r = $j
        }
        while (($j * $j + $i * $i) -lt 11 -band ($k++) -lt 111)
    }

    Write-Host
}

```



## OpenEdge/Progress

<lang Progress (OpenEdge ABL)>DEFINE VARIABLE print_str AS CHAR NO-UNDO INIT ''.
DEFINE VARIABLE X1 AS DECIMAL NO-UNDO INIT 50.
DEFINE VARIABLE Y1 AS DECIMAL NO-UNDO INIT 21.
DEFINE VARIABLE X AS DECIMAL NO-UNDO.
DEFINE VARIABLE Y AS DECIMAL NO-UNDO.
DEFINE VARIABLE N AS DECIMAL NO-UNDO.
DEFINE VARIABLE I3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE R3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE Z1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE Z2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE A AS DECIMAL NO-UNDO.
DEFINE VARIABLE B AS DECIMAL NO-UNDO.
DEFINE VARIABLE I1 AS DECIMAL NO-UNDO INIT -1.0.
DEFINE VARIABLE I2 AS DECIMAL NO-UNDO INIT 1.0.
DEFINE VARIABLE R1 AS DECIMAL NO-UNDO INIT -2.0.
DEFINE VARIABLE R2 AS DECIMAL NO-UNDO INIT 1.0.
DEFINE VARIABLE S1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE S2 AS DECIMAL NO-UNDO.


S1 = (R2 - R1) / X1.
S2 = (I2 - I1) / Y1.
DO Y = 0 TO Y1 - 1:
  I3 = I1 + S2 * Y.
  DO X = 0 TO X1 - 1:
    R3 = R1 + S1 * X.
    Z1 = R3.
    Z2 = I3.
    DO N = 0 TO 29:
      A = Z1 * Z1.
      B = Z2 * Z2.
      IF A + B > 4.0 THEN
        LEAVE.
      Z2 = 2 * Z1 * Z2 + I3.
      Z1 = A - B + R3.
    END.
    print_str = print_str + CHR(62 - N).
  END.
  print_str = print_str + '~n'.
END.

OUTPUT TO "C:\Temp\out.txt".
MESSAGE print_str.
OUTPUT CLOSE.

```
Example :<BR>


## Processing


```Prolog

// Following code is a zoomable Mandelbrot. 
// Of course, you want to click on an interesting area 
// with contrast and more colors to zoom in.

double x, y, zr, zi, zr2, zi2, cr, ci, n;
double zmx1, zmx2, zmy1, zmy2, f, di, dj;
double fn1, fn2, fn3, re, gr, bl, xt, yt, i, j;
 
void setup() {
  size(500, 500);
  di = 0;
  dj = 0;
  f = 10;
  fn1 = random(20); 
  fn2 = random(20); 
  fn3 = random(20);
  zmx1 = int(width / 4);
  zmx2 = 2;
  zmy1 = int(height / 4);
  zmy2 = 2;
}
 
void draw() {
  if (i <= width) i++;
  x =  (i +  di)/ zmx1 - zmx2;
  for ( j = 0; j <= height; j++) {
    y = zmy2 - (j + dj) / zmy1;
    zr = 0;
    zi = 0;
    zr2 = 0; 
    zi2 = 0; 
    cr = x;   
    ci = y;  
    n = 1;
    while (n < 200 && (zr2 + zi2) < 4) {
      zi2 = zi * zi;
      zr2 = zr * zr;
      zi = 2 * zi * zr + ci;
      zr = zr2 - zi2 + cr;
      n++;
    }  
    re = (n * fn1) % 255;
    gr = (n * fn2) % 255;
    bl = (n * fn3) % 255;
    stroke((float)re, (float)gr, (float)bl); 
    point((float)i, (float)j);
  }
}
 
void mousePressed() {
  background(200); 
  xt = mouseX;
  yt = mouseY;
  di = di + xt - float(width / 2);
  dj = dj + yt - float(height / 2);
  zmx1 = zmx1 * f;
  zmx2 = zmx2 * (1 / f);
  zmy1 = zmy1 * f;
  zmy2 = zmy2 * (1 / f);
  di = di * f;
  dj = dj * f;
  i = 0;
  j = 0;
}
```
'''Output examples''' :<BR>
https://drive.google.com/open?id=0B8HXpZALHgx-WWV3dXJwRXdvbWs

https://drive.google.com/open?id=0B8HXpZALHgx-MktORkZKelFBbkU


## Prolog

SWI-Prolog has a graphic interface XPCE :

```Prolog
:- use_module(library(pce)).

mandelbrot :-
    new(D, window('Mandelbrot Set')),
    send(D, size, size(700, 650)),
    new(Img, image(@nil, width := 700, height := 650, kind := pixmap)),

    forall(between(0,699, I),
           (   forall(between(0,649, J),
              (   get_RGB(I, J, R, G, B),
                  R1 is (R * 256) mod 65536,
                  G1 is (G * 256) mod 65536,
                  B1 is (B * 256) mod 65536,
                  send(Img, pixel(I, J, colour(@default, R1, G1, B1))))))),
    new(Bmp, bitmap(Img)),
    send(D, display, Bmp, point(0,0)),
    send(D, open).

get_RGB(X, Y, R, G, B) :-
    CX is (X - 350) / 150,
    CY is (Y - 325) / 150,
    Iter = 570,
    compute_RGB(CX, CY, 0, 0, Iter, It),
    IterF is It \/ It << 15,
    R is IterF >> 16,
    Iter1 is IterF - R << 16,
    G is Iter1 >> 8,
    B  is Iter1 - G << 8.

compute_RGB(CX, CY, ZX, ZY, Iter, IterF) :-
    ZX * ZX + ZY * ZY < 4,
    Iter > 0,
    !,
    Tmp is  ZX * ZX - ZY * ZY + CX,
    ZY1 is 2 * ZX * ZY + CY,
    Iter1 is Iter - 1,
    compute_RGB(CX, CY, Tmp, ZY1, Iter1, IterF).

compute_RGB(_CX, _CY, _ZX, _ZY, Iter, Iter).
```
Example :<BR>
[[FILE:Mandelbrot.jpg]]


## PureBasic

PureBasic forum: [http://www.purebasic.fr/german/viewtopic.php?f=4&t=22107 discussion]

```PureBasic
EnableExplicit

#Window1   = 0
#Image1    = 0
#ImgGadget = 0

#max_iteration =  64
#width         = 800
#height        = 600
Define.d x0 ,y0 ,xtemp ,cr, ci
Define.i i, n, x, y ,Event ,color

Dim Color.l (255)
For n = 0 To 63
  Color(   0 + n ) = RGB(  n*4+128, 4 * n, 0 )
  Color(  64 + n ) = RGB(  64, 255, 4 * n )
  Color( 128 + n ) = RGB(  64, 255 - 4 * n , 255 )
  Color( 192 + n ) = RGB(  64, 0, 255 - 4 * n )
Next

If OpenWindow(#Window1, 0, 0, #width, #height, "'Mandelbrot set' PureBasic Example", #PB_Window_SystemMenu )
    If CreateImage(#Image1, #width, #height)
       ImageGadget(#ImgGadget, 0, 0, #width, #height, ImageID(#Image1))
       For y.i = 1 To #height -1
         StartDrawing(ImageOutput(#Image1))
         For x.i = 1 To  #width -1
           x0 = 0
           y0 = 0;
           cr = (x / #width)*2.5 -2
           ci = (y / #height)*2.5 -1.25
           i = 0
           While  (x0*x0 + y0*y0 <= 4.0) And i < #max_iteration
             i +1
             xtemp = x0*x0 - y0*y0 + cr
             y0    = 2*x0*y0 + ci
             x0    = xtemp
           Wend
           If i >= #max_iteration
              Plot(x, y,  0 )
           Else
              Plot(x, y,  Color(i & 255))
           EndIf
           
         Next
         StopDrawing()
         SetGadgetState(#ImgGadget, ImageID(#Image1))
         Repeat
           Event = WindowEvent()
           If Event = #PB_Event_CloseWindow
             End
           EndIf
         Until Event = 0 
       Next
    EndIf
    Repeat
      Event = WaitWindowEvent()
    Until Event = #PB_Event_CloseWindow
  EndIf
```
Example:

[[File:Mandelbrot-PureBasic.png]]


## Python

Translation of the ruby solution

```python
# Python 3.0+ and 2.5+
try:
    from functools import reduce
except:
    pass


def mandelbrot(a):
    return reduce(lambda z, _: z * z + a, range(50), 0)

def step(start, step, iterations):
    return (start + (i * step) for i in range(iterations))

rows = (("*" if abs(mandelbrot(complex(x, y))) < 2 else " "
        for x in step(-2.0, .0315, 80))
        for y in step(1, -.05, 41))

print("\n".join("".join(row) for row in rows))

```


A more "Pythonic" version of the code:

```python

import math

def mandelbrot(z , c , n=40):
    if abs(z) > 1000:
        return float("nan")
    elif n > 0:
        return mandelbrot(z ** 2 + c, c, n - 1) 
    else:
        return z ** 2 + c

print("\n".join(["".join(["#" if not math.isnan(mandelbrot(0, x + 1j * y).real) else " "
                 for x in [a * 0.02 for a in range(-80, 30)]]) 
                 for y in [a * 0.05 for a in range(-20, 20)]])
     )

```


Finally, we can also use Matplotlib to visualize the Mandelbrot set with Python:
{{libheader|matplotlib}}
{{libheader|NumPy}}

```python
from pylab import *
from numpy import NaN

def m(a):
	z = 0
	for n in range(1, 100):
		z = z**2 + a
		if abs(z) > 2:
			return n
	return NaN

X = arange(-2, .5, .002)
Y = arange(-1,  1, .002)
Z = zeros((len(Y), len(X)))

for iy, y in enumerate(Y):
	print (iy, "of", len(Y))
	for ix, x in enumerate(X):
		Z[iy,ix] = m(x + 1j * y)

imshow(Z, cmap = plt.cm.prism, interpolation = 'none', extent = (X.min(), X.max(), Y.min(), Y.max()))
xlabel("Re(c)")
ylabel("Im(c)")
savefig("mandelbrot_python.svg")
show()
```



Another Numpy version using masks to avoid (explicit) nested loops. 
Runs about 16x faster for the same resolution.


```python
import matplotlib.pyplot as plt
import numpy as np

npts = 300
max_iter = 100

X = np.linspace(-2, 1, 2 * npts)
Y = np.linspace(-1, 1, npts)

#broadcast X to a square array
C = X[:, None] + 1J * Y
#initial value is always zero
Z = np.zeros_like(C)

exit_times = max_iter * np.ones(C.shape, np.int32)
mask = exit_times > 0

for k in range(max_iter):
    Z[mask] = Z[mask] * Z[mask] + C[mask]
    mask, old_mask = abs(Z) < 2, mask
    #use XOR to detect the area which has changed 
    exit_times[mask ^ old_mask] = k

plt.imshow(exit_times.T,
           cmap=plt.cm.prism,
           extent=(X.min(), X.max(), Y.min(), Y.max()))

```



## R


```R
iterate.until.escape <- function(z, c, trans, cond, max=50, response=dwell) {
  #we iterate all active points in the same array operation,
  #and keeping track of which points are still iterating.
  active <- seq_along(z)
  dwell <- z
  dwell[] <- 0
  for (i in 1:max) {
    z[active] <- trans(z[active], c[active]);
    survived <- cond(z[active])
    dwell[active[!survived]] <- i
    active <- active[survived]
    if (length(active) == 0) break
  }
  eval(substitute(response))
}

re = seq(-2, 1, len=500)
im = seq(-1.5, 1.5, len=500)
c <- outer(re, im, function(x,y) complex(real=x, imaginary=y))
x <- iterate.until.escape(array(0, dim(c)), c,
                          function(z,c)z^2+c, function(z)abs(z) <= 2,
                          max=100)
image(x)
```



## Racket


```racket

#lang racket

(require racket/draw)

(define (iterations a z i)
  (define z′ (+ (* z z) a))
  (if (or (= i 255) (> (magnitude z′) 2))
      i
      (iterations a z′ (add1 i))))

(define (iter->color i)
  (if (= i 255)
      (make-object color% "black")
      (make-object color% (* 5 (modulo i 15)) (* 32 (modulo i 7)) (* 8 (modulo i 31)))))

(define (mandelbrot width height)
  (define target (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap target]))
  (for* ([x width] [y height])
    (define real-x (- (* 3.0 (/ x width)) 2.25))
    (define real-y (- (* 2.5 (/ y height)) 1.25))
    (send dc set-pen (iter->color (iterations (make-rectangular real-x real-y) 0 0)) 1 'solid)
    (send dc draw-point x y))
  (send target save-file "mandelbrot.png" 'png))

(mandelbrot 300 200)

```



## REXX


### version 1

{{trans|AWK}}
This REXX version doesn't depend on the ASCII sequence of glyphs;   an internal character string was used that mimics a part of the ASCII glyph sequence.

```rexx
/*REXX program  generates and displays a Mandelbrot set as an ASCII art character image.*/
@ = '>=<;:9876543210/.-,+*)(''&%$#"!'            /*the characters used in the display.  */
Xsize = 59;  minRE = -2;  maxRE = +1;     stepX = (maxRE-minRE) / Xsize
Ysize = 21;  minIM = -1;  maxIM = +1;     stepY = (maxIM-minIM) / Ysize

  do y=0  for ysize;      im=minIM + stepY*y
  $=
        do x=0  for Xsize;   re=minRE + stepX*x;    zr=re;    zi=im

            do n=0  for 30;  a=zr**2;   b=zi**2;    if a+b>4  then leave
            zi=zr*zi*2 + im;            zr=a-b+re
            end   /*n*/

        $=$ || substr(@, n+1, 1)                 /*append number (as a char) to $ string*/
        end       /*x*/
  say $                                          /*display a line of  character  output.*/
  end             /*y*/                          /*stick a fork in it,  we're all done. */
```

'''output'''   using the internal defaults:

```txt

>>>>>>
### ==<<<<<<<<<<<<<<<;;;;;;:::96032:;;;;<<<<=======

>>>>>===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### =

>>>>===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<======
>>>==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<====
>>==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<===
>>=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<==
>=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<=
><<<<;;;;;:::972456-567763                      +9;;<<<<<<<
><;;;;;;::::9875&      .3                       *9;;;<<<<<<
>;;;;;;::997564'        '                       8:;;;<<<<<<
>::988897735/                                 &89:;;;<<<<<<
>::988897735/                                 &89:;;;<<<<<<
>;;;;;;::997564'        '                       8:;;;<<<<<<
><;;;;;;::::9875&      .3                       *9;;;<<<<<<
><<<<;;;;;:::972456-567763                      +9;;<<<<<<<
>=<<<<<<<<;;;:599999999886                    %78:;;<<<<<<=
>>=<<<<<<<<<<<;;;::::::999752                 *79:;<<<<<<==
>>==<<<<<<<<<<<<<;;;;::::996. &2           45335:;<<<<<<===
>>>==<<<<<<<<<<<<<<<;;;;;;:98888764     5789999:;;<<<<<====
>>>>===<<<<<<<<<<<<<<<;;;;;;;::9974    (.9::::;;<<<<<======
>>>>>===<<<<<<<<<<<<<<<<;;;;;;;:::873*079::;;;;<<<<<
### =


```



### version 2

This REXX version uses glyphs that are "darker" (with a white background) around the output's peripheral.

```rexx
/*REXX program  generates and displays a Mandelbrot set as an ASCII art character image.*/
@ = '█▓▒░@9876543210=.-,+*)(·&%$#"!'             /*the characters used in the display.  */
Xsize = 59;  minRE = -2;  maxRE = +1;     stepX = (maxRE-minRE) / Xsize
Ysize = 21;  minIM = -1;  maxIM = +1;     stepY = (maxIM-minIM) / Ysize

  do y=0  for ysize;      im=minIM + stepY*y
  $=
        do x=0  for Xsize;   re=minRE + stepX*x;    zr=re;    zi=im

            do n=0  for 30;  a=zr**2;   b=zi**2;    if a+b>4  then leave
            zi=zr*zi*2 + im;            zr=a-b+re
            end   /*n*/

        $=$ || substr(@, n+1, 1)                 /*append number (as a char) to $ string*/
        end       /*x*/
  say $                                          /*display a line of  character  output.*/
  end             /*y*/                          /*stick a fork in it,  we're all done. */
```

'''output'''   using the internal defaults:

```txt

██████▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░@@@96032@░░░░▒▒▒▒▓▓▓▓▓▓▓▓▓▓
█████▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░@@@873*079@@░░░░▒▒▒▒▒▓▓▓▓▓▓▓
████▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░@@9974    (.9@@@@░░▒▒▒▒▒▓▓▓▓▓▓
███▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░@98888764     5789999@░░▒▒▒▒▒▓▓▓▓
██▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░@@@@996. &2           45335@░▒▒▒▒▒▒▓▓▓
██▓▒▒▒▒▒▒▒▒▒▒▒░░░@@@@@@999752                 *79@░▒▒▒▒▒▒▓▓
█▓▒▒▒▒▒▒▒▒░░░@599999999886                    %78@░░▒▒▒▒▒▒▓
█▒▒▒▒░░░░░@@@972456-567763                      +9░░▒▒▒▒▒▒▒
█▒░░░░░░@@@@9875&      .3                       *9░░░▒▒▒▒▒▒
█░░░░░░@@997564·        ·                       8@░░░▒▒▒▒▒▒
█@@988897735=                                 &89@░░░▒▒▒▒▒▒
█@@988897735=                                 &89@░░░▒▒▒▒▒▒
█░░░░░░@@997564·        ·                       8@░░░▒▒▒▒▒▒
█▒░░░░░░@@@@9875&      .3                       *9░░░▒▒▒▒▒▒
█▒▒▒▒░░░░░@@@972456-567763                      +9░░▒▒▒▒▒▒▒
█▓▒▒▒▒▒▒▒▒░░░@599999999886                    %78@░░▒▒▒▒▒▒▓
██▓▒▒▒▒▒▒▒▒▒▒▒░░░@@@@@@999752                 *79@░▒▒▒▒▒▒▓▓
██▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░@@@@996. &2           45335@░▒▒▒▒▒▒▓▓▓
███▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░@98888764     5789999@░░▒▒▒▒▒▓▓▓▓
████▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░@@9974    (.9@@@@░░▒▒▒▒▒▓▓▓▓▓▓
█████▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░@@@873*079@@░░░░▒▒▒▒▒▓▓▓▓▓▓▓

```



### version 3

This REXX version produces a larger output   (it uses the full width of the terminal screen (less one),   and the height is one-half of the width. 

```rexx
/*REXX program  generates and displays a Mandelbrot set as an ASCII art character image.*/
@ = '█▓▒░@9876543210=.-,+*)(·&%$#"!'             /*the characters used in the display.  */
parse arg Xsize Ysize .                          /*obtain optional arguments from the CL*/
if Xsize==''  then Xsize=linesize() - 1          /*X:  the (usable) linesize  (minus 1).*/
if Ysize==''  then Ysize=Xsize%2 + (Xsize//2==1) /*Y:  half the linesize (make it even).*/
minRE = -2;     maxRE = +1;       stepX = (maxRE-minRE) / Xsize
minIM = -1;     maxIM = +1;       stepY = (maxIM-minIM) / Ysize

  do y=0  for ysize;      im=minIM + stepY*y
  $=
        do x=0  for Xsize;   re=minRE + stepX*x;    zr=re;    zi=im

            do n=0  for 30;  a=zr**2;   b=zi**2;    if a+b>4  then leave
            zi=zr*zi*2 + im;            zr=a-b+re
            end   /*n*/

        $=$ || substr(@, n+1, 1)                 /*append number (as a char) to $ string*/
        end       /*x*/
  say $                                          /*display a line of  character  output.*/
  end             /*y*/                          /*stick a fork in it,  we're all done. */
```

'''output'''   using the internal defaults:

```txt

████████▓▓▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░@@@@985164(9@░░░░░░▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓
███████▓▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░@@@@98763=5799@░░░░░░▒▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓
███████▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░░@@@@98763.-2789@@@░░░░░░▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓
██████▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░░@@@@985 2.  1448@@@@░░░░░░▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓
█████▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░░@@@999874      *=79@@@@@░░░░▒▒▒▒▒▒▒▒▓▓▓▓▓▓▓▓
█████▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░@@999998873       17899@@@@@░░░▒▒▒▒▒▒▒▒▓▓▓▓▓▓▓
████▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░@@98888888764      #4678899999@@░░▒▒▒▒▒▒▒▒▓▓▓▓▓▓
████▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░@@@@9343,665 322=     =215357888709@░░▒▒▒▒▒▒▒▒▓▓▓▓▓
███▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░@@@@@9986  + 32               ,56554)79@░▒▒▒▒▒▒▒▒▒▓▓▓▓
███▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░@@@@@@@999863   +                  2· ",59@░░▒▒▒▒▒▒▒▒▓▓▓▓
███▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░@@@@@@@@@@9998763                      $   379@@░▒▒▒▒▒▒▒▒▒▓▓▓
██▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░@@@@@@@@@@999986.2 $                        +689@@░░▒▒▒▒▒▒▒▒▒▓▓
██▓▒▒▒▒▒▒▒▒▒▒▒▒▒░░░@689999@@@99999887                             05789@░░▒▒▒▒▒▒▒▒▒▓▓
██▒▒▒▒▒▒▒▒▒▒░░░░░@@9717888877888888763.                            2558@░░░▒▒▒▒▒▒▒▒▒▓
█▓▒▒▒▒▒▒░░░░░░░@@@996)566761467777762                                 4@░░░▒▒▒▒▒▒▒▒▒▓
█▓▒▒▒▒░░░░░░░░@@@@99763 & 42&..366651·                              .68@░░░░▒▒▒▒▒▒▒▒▓
█▒▒▒░░░░░░░░@@@@@@98864*  )   $  343                                259@░░░░▒▒▒▒▒▒▒▒▒
█▒▒░░░░░░░░@@@@@@988753.          11                                 #9@░░░░▒▒▒▒▒▒▒▒▒
█▒░░░░░░░░@@@@@9877650             -                                289@░░░░▒▒▒▒▒▒▒▒▒
█░░░░░░░░@9999887%413+             %                               &69@@░░░░▒▒▒▒▒▒▒▒▒
█░@@@@@89999888763 % (                                             389@@░░░░▒▒▒▒▒▒▒▒▒
█@@99872676676422                                                 5789@@░░░░▒▒▒▒▒▒▒▒▒
█@@99872676676422                                                 5789@@░░░░▒▒▒▒▒▒▒▒▒
█░@@@@@89999888763 % (                                             389@@░░░░▒▒▒▒▒▒▒▒▒
█░░░░░░░░@9999887%413+             %                               &69@@░░░░▒▒▒▒▒▒▒▒▒
█▒░░░░░░░░@@@@@9877650             -                                289@░░░░▒▒▒▒▒▒▒▒▒
█▒▒░░░░░░░░@@@@@@988753.          11                                 #9@░░░░▒▒▒▒▒▒▒▒▒
█▒▒▒░░░░░░░░@@@@@@98864*  )   $  343                                259@░░░░▒▒▒▒▒▒▒▒▒
█▓▒▒▒▒░░░░░░░░@@@@99763 & 42&..366651·                              .68@░░░░▒▒▒▒▒▒▒▒▓
█▓▒▒▒▒▒▒░░░░░░░@@@996)566761467777762                                 4@░░░▒▒▒▒▒▒▒▒▒▓
██▒▒▒▒▒▒▒▒▒▒░░░░░@@9717888877888888763.                            2558@░░░▒▒▒▒▒▒▒▒▒▓
██▓▒▒▒▒▒▒▒▒▒▒▒▒▒░░░@689999@@@99999887                             05789@░░▒▒▒▒▒▒▒▒▒▓▓
██▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░@@@@@@@@@@999986.2 $                        +689@@░░▒▒▒▒▒▒▒▒▒▓▓
███▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░@@@@@@@@@@9998763                      $   379@@░▒▒▒▒▒▒▒▒▒▓▓▓
███▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░@@@@@@@999863   +                  2· ",59@░░▒▒▒▒▒▒▒▒▓▓▓▓
███▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░@@@@@9986  + 32               ,56554)79@░▒▒▒▒▒▒▒▒▒▓▓▓▓
████▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░@@@@9343,665 322=     =215357888709@░░▒▒▒▒▒▒▒▒▓▓▓▓▓
████▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░@@98888888764      #4678899999@@░░▒▒▒▒▒▒▒▒▓▓▓▓▓▓
█████▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░@@999998873       17899@@@@@░░░▒▒▒▒▒▒▒▒▓▓▓▓▓▓▓
█████▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░░@@@999874      *=79@@@@@░░░░▒▒▒▒▒▒▒▒▓▓▓▓▓▓▓▓
██████▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░░@@@@985 2.  1448@@@@░░░░░░▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓
███████▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░░@@@@98763.-2789@@@░░░░░░▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓
███████▓▓▓▓▓▓▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒░░░░░░░░░@@@@98763=5799@░░░░░░▒▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓▓

```

This REXX program makes use of   '''linesize'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console). 

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]]. 




## Ring


```ring

load "guilib.ring"

new qapp 
        {
        win1 = new qwidget() {
               setwindowtitle("Mandelbrot set")
               setgeometry(100,100,500,500)
               label1 = new qlabel(win1) {
                        setgeometry(10,10,400,400)
                        settext("")
               }
               new qpushbutton(win1) {
                   setgeometry(200,400,100,30)
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
        new qpainter() {
            begin(p1)
            setpen(pen)

        x1=300 y1=250
        i1=-1 i2=1 r1=-2 r2=1
        s1=(r2-r1)/x1 s2=(i2-i1)/y1
        for y=0 to y1
            i3=i1+s2*y
            for x=0 to x1
                r3=r1+s1*x z1=r3 z2=i3
                for n=0 to 30
                    a=z1*z1 b=z2*z2
                    if a+b>4 exit ok
                       z2=2*z1*z2+i3 z1=a-b+r3
                next
                if n != 31 drawpoint(x,y) ok
            next 
        next

        endpaint()
        }
        label1 { setpicture(p1) show() }

```


Output:

[[File:CalmoSoftMandelbrot.jpg]]


## Ruby

Text only, prints an 80-char by 41-line depiction. Found [http://www.xcombinator.com/2008/02/22/ruby-inject-and-the-mandelbrot-set/ here].

```ruby
require 'complex'

def mandelbrot(a)
  Array.new(50).inject(0) { |z,c| z*z + a }
end

(1.0).step(-1,-0.05) do |y|
  (-2.0).step(0.5,0.0315) do |x|
    print mandelbrot(Complex(x,y)).abs < 2 ? '*' : ' '
  end
  puts
end
```


{{trans|Tcl}}

Uses the text progress bar from [[Median filter#Ruby]]

```ruby
class RGBColour
  def self.mandel_colour(i)
    self.new( 16*(i % 15), 32*(i % 7), 8*(i % 31) )
  end
end

class Pixmap
  def self.mandelbrot(width, height)
    mandel = Pixmap.new(width,height)
    pb = ProgressBar.new(width) if $DEBUG
    width.times do |x|
      height.times do |y|
        x_ish = Float(x - width*11/15) / (width/3)
        y_ish = Float(y - height/2) / (height*3/10)
        mandel[x,y] = RGBColour.mandel_colour(mandel_iters(x_ish, y_ish))
      end
      pb.update(x) if $DEBUG
    end
    pb.close if $DEBUG
    mandel
  end

  def self.mandel_iters(cx,cy)
    x = y = 0.0
    count = 0
    while Math.hypot(x,y) < 2 and count < 255
      x, y = (x**2 - y**2 + cx), (2*x*y + cy)
      count += 1
    end
    count
  end 
end

Pixmap.mandelbrot(300,300).save('mandel.ppm')
```



## Rust

Dependencies: image, num-complex

```rust
extern crate image;
extern crate num_complex;

use std::fs::File;
use num_complex::Complex;

fn main() {
    let max_iterations = 256u16;
    let img_side = 800u32;
    let cxmin = -2f32;
    let cxmax = 1f32;
    let cymin = -1.5f32;
    let cymax = 1.5f32;
    let scalex = (cxmax - cxmin) / img_side as f32;
    let scaley = (cymax - cymin) / img_side as f32;

    // Create a new ImgBuf
    let mut imgbuf = image::ImageBuffer::new(img_side, img_side);

    // Calculate for each pixel
    for (x, y, pixel) in imgbuf.enumerate_pixels_mut() {
        let cx = cxmin + x as f32 * scalex;
        let cy = cymin + y as f32 * scaley;

        let c = Complex::new(cx, cy);
        let mut z = Complex::new(0f32, 0f32);

        let mut i = 0;
        for t in 0..max_iterations {
            if z.norm() > 2.0 {
                break;
            }
            z = z * z + c;
            i = t;
        }

        *pixel = image::Luma([i as u8]);
    }

    // Save image
    let fout = &mut File::create("fractal.png").unwrap();
    image::ImageLuma8(imgbuf).save(fout, image::PNG).unwrap();
}
```



## Scala

{{works with|Scala|2.8}}
Uses RgbBitmap from [[Basic_bitmap_storage#Scala|Basic Bitmap Storage]] task and Complex number class from [[Arithmetic/Complex#Scala|this]]
programming task.

```scala
import org.rosettacode.ArithmeticComplex._
import java.awt.Color

object Mandelbrot
{
   def generate(width:Int =600, height:Int =400)={
      val bm=new RgbBitmap(width, height)

      val maxIter=1000
      val xMin = -2.0
      val xMax =  1.0
      val yMin = -1.0
      val yMax =  1.0

      val cx=(xMax-xMin)/width
      val cy=(yMax-yMin)/height

      for(y <- 0 until bm.height; x <- 0 until bm.width){
         val c=Complex(xMin+x*cx, yMin+y*cy)
         val iter=itMandel(c, maxIter, 4)
         bm.setPixel(x, y, getColor(iter, maxIter))
      }
      bm
   }

   def itMandel(c:Complex, imax:Int, bailout:Int):Int={
      var z=Complex()
      for(i <- 0 until imax){
         z=z*z+c;
         if(z.abs > bailout) return i
      }
      imax;
   }

   def getColor(iter:Int, max:Int):Color={
      if (iter==max) return Color.BLACK

      var c=3*math.log(iter)/math.log(max-1.0)
      if(c<1) new Color((255*c).toInt, 0, 0)
      else if(c<2) new Color(255, (255*(c-1)).toInt, 0)
      else new Color(255, 255, (255*(c-2)).toInt)
   }
}
```

Read–eval–print loop

```scala
import scala.swing._
import javax.swing.ImageIcon
val imgMandel=Mandelbrot.generate()
val mainframe=new MainFrame(){title="Test"; visible=true
   contents=new Label(){icon=new ImageIcon(imgMandel.image)}
}
```



## Scheme

This implementation writes an image of the Mandelbrot set to a plain pgm file. The set itself is drawn in white, while the exterior is drawn in black.

```scheme
(define x-centre -0.5)
(define y-centre 0.0)
(define width 4.0)
(define i-max 800)
(define j-max 600)
(define n 100)
(define r-max 2.0)
(define file "out.pgm")
(define colour-max 255)
(define pixel-size (/ width i-max))
(define x-offset (- x-centre (* 0.5 pixel-size (+ i-max 1))))
(define y-offset (+ y-centre (* 0.5 pixel-size (+ j-max 1))))

(define (inside? z)
  (define (*inside? z-0 z n)
    (and (< (magnitude z) r-max)
         (or (= n 0)
             (*inside? z-0 (+ (* z z) z-0) (- n 1)))))
  (*inside? z 0 n))

(define (boolean->integer b)
  (if b colour-max 0))

(define (pixel i j)
  (boolean->integer
    (inside?
      (make-rectangular (+ x-offset (* pixel-size i))
                        (- y-offset (* pixel-size j))))))

(define (plot)
  (with-output-to-file file
    (lambda ()
      (begin (display "P2") (newline)
             (display i-max) (newline)
             (display j-max) (newline)
             (display colour-max) (newline)
             (do ((j 1 (+ j 1))) ((> j j-max))
                 (do ((i 1 (+ i 1))) ((> i i-max))
                     (begin (display (pixel i j)) (newline))))))))

(plot)
```



## Scratch


[[File:scratch-mandelbrot.gif]]


## Sass/SCSS


```coffeescript

$canvasWidth: 200;
$canvasHeight: 200;
$iterations: 20;
$xCorner: -2;
$yCorner: -1.5;
$zoom: 3;
$data: ()!global;
@mixin plot ($x,$y,$count){
  $index: ($y * $canvasWidth + $x) * 4;
  $r: $count * -12 + 255;
  $g: $count * -12 + 255;
  $b: $count * -12 + 255;
  $data: append($data, $x + px $y + px 0 rgb($r,$g,$b), comma)!global;
}

@for $x from 1 to $canvasWidth {
    @for $y from 1 to $canvasHeight {
      $count: 0;
      $size: 0;
      $cx: $xCorner + (($x * $zoom) / $canvasWidth);
      $cy: $yCorner + (($y * $zoom) / $canvasHeight);

      $zx: 0;
      $zy: 0;

      @while $count < $iterations and $size <= 4  {
        $count: $count + 1;
        $temp:  ($zx * $zx) - ($zy * $zy);
        $zy:  (2 * $zx * $zy) + $cy;
        $zx:  $temp + $cx;
        $size:  ($zx * $zx) + ($zy * $zy);
      }

      @include plot($x, $y, $count); 
    }
}
.set {
  height: 1px;
  width: 1px;
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate($canvasWidth*0.5px, $canvasWidth*0.5px);
  box-shadow: $data;
}


```



## Seed7



```txt

$ include "seed7_05.s7i";
  include "float.s7i";
  include "complex.s7i";
  include "draw.s7i";
  include "keybd.s7i";

# Display the Mandelbrot set, that are points z[0] in the complex plane
# for which the sequence z[n+1] := z[n] ** 2 + z[0] (n >= 0) is bounded.
# Since this program is computing intensive it should be compiled with
# hi comp -O2 mandelbr

const integer: pix is 200;
const integer: max_iter is 256;

var array color: colorTable is max_iter times black;

const func integer: iterate (in complex: z0) is func
  result
    var integer: iter is 1;
  local
    var complex: z is complex.value;
  begin
    z := z0;
    while sqrAbs(z) < 4.0 and  # not diverged
        iter < max_iter do     # not converged
      z *:= z;
      z +:= z0;
      incr(iter);
    end while;
  end func;

const proc: displayMandelbrotSet (in complex: center, in float: zoom) is func
  local
    var integer: x is 0;
    var integer: y is 0;
    var complex: z0 is complex.value;
  begin
    for x range -pix to pix do
      for y range -pix to pix do
        z0 := center + complex(flt(x) * zoom, flt(y) * zoom);
        point(x + pix, y + pix, colorTable[iterate(z0)]);
      end for;    
    end for;
  end func;

const proc: main is func
  local
    const integer: num_pix is 2 * pix + 1;
    var integer: col is 0;
  begin
    screen(num_pix, num_pix);
    clear(curr_win, black);
    KEYBOARD := GRAPH_KEYBOARD;
    for col range 1 to pred(max_iter) do
      colorTable[col] := color(65535 - (col * 5003) mod 65535,
                                       (col * 257)  mod 65535,
                                       (col * 2609) mod 65535);
    end for;
    displayMandelbrotSet(complex(-0.75, 0.0), 1.3 / flt(pix));
    DRAW_FLUSH;
    readln(KEYBOARD);
  end func;

```



Original source: [http://seed7.sourceforge.net/algorith/graphic.htm#mandelbr]


## SequenceL

'''SequenceL Code for Computing and Coloring:'''


```sequencel>import <Utilities/Complex.sl
;
import <Utilities/Sequence.sl>;
import <Utilities/Math.sl>;

COLOR_STRUCT ::= (R: int(0), G: int(0), B: int(0));
rgb(r(0), g(0), b(0)) := (R: r, G: g, B: b);

RESULT_STRUCT ::= (FinalValue: Complex(0), Iterations: int(0));
makeResult(val(0), iters(0)) := (FinalValue: val, Iterations: iters);

zSquaredOperation(startingNum(0), currentNum(0)) :=
    complexAdd(startingNum, complexMultiply(currentNum, currentNum));

zSquared(minX(0), maxX(0), resolutionX(0), minY(0), maxY(0), resolutionY(0), maxMagnitude(0), maxIters(0))[Y,X] := 
    let
        stepX := (maxX - minX) / resolutionX;
        stepY := (maxY - minY) / resolutionY;
        
        currentX := X * stepX + minX;
        currentY := Y * stepY + minY;
        
    in 
        operateUntil(zSquaredOperation, makeComplex(currentX, currentY), makeComplex(currentX, currentY), maxMagnitude, 0, maxIters)
    foreach Y within 0 ... (resolutionY - 1),
            X within 0 ... (resolutionX - 1);

operateUntil(operation(0), startingNum(0), currentNum(0), maxMagnitude(0), currentIters(0), maxIters(0)) :=
    let
        operated := operation(startingNum, currentNum);
    in
        makeResult(currentNum, maxIters) when currentIters >= maxIters
    else
        makeResult(currentNum, currentIters) when complexMagnitude(currentNum) >= maxMagnitude
    else
        operateUntil(operation, startingNum, operated, maxMagnitude, currentIters + 1, maxIters);

//region Smooth Coloring

COLOR_COUNT := size(colorSelections);

colorRange := range(0, 255, 1);

colors := 
    let
        first[i] := rgb(0, 0, i) foreach i within colorRange;
        second[i] := rgb(i, i, 255) foreach i within colorRange;
        third[i] := rgb(255, 255, i) foreach i within reverse(colorRange);
        fourth[i] := rgb(255, i, 0) foreach i within reverse(colorRange);
        fifth[i] := rgb(i, 0, 0) foreach i within reverse(colorRange);

        red[i] :=   rgb(i, 0, 0) foreach i within colorRange;
        redR[i] :=  rgb(i, 0, 0) foreach i within reverse(colorRange);
        green[i] := rgb(0, i, 0) foreach i within colorRange;
        greenR[i] :=rgb(0, i, 0) foreach i within reverse(colorRange);
        blue[i] :=  rgb(0, 0, i) foreach i within colorRange;
        blueR[i] := rgb(0, 0, i) foreach i within reverse(colorRange);

    in
        //red ++ redR ++ green ++ greenR ++ blue ++ blueR;  
        first ++ second ++ third ++ fourth ++ fifth;
        //first ++ fourth;

colorSelections := range(1, size(colors), 30);

getSmoothColorings(zSquaredResult(2), maxIters(0))[Y,X] :=
    let
        current := zSquaredResult[Y,X];
        
        zn := complexMagnitude(current.FinalValue);
        nu := ln(ln(zn) / ln(2)) / ln(2);
        
        result := abs(current.Iterations + 1 - nu);
        
        index := floor(result);
        rem := result - index;
                
        color1 := colorSelections[(index mod COLOR_COUNT) + 1];
        color2 := colorSelections[((index + 1) mod COLOR_COUNT) + 1];
    in
        rgb(0, 0, 0) when current.Iterations = maxIters
    else
        colors[color1] when color2 < color1
    else
        colors[floor(linearInterpolate(color1, color2, rem))];
        
linearInterpolate(v0(0), v1(0), t(0)) := (1 - t) * v0 + t * v1;

//endregion
```


'''C++ Driver Code:'''

{{libheader|CImg}}

```c
#include "SL_Generated.h"
#include "../../../ThirdParty/CImg/CImg.h"

using namespace std;
using namespace cimg_library;

int main(int argc, char ** argv)
{
    int cores = 0;

    Sequence<Sequence<_sl_RESULT_STRUCT> > computeResult;
    Sequence<Sequence<_sl_COLOR_STRUCT> > colorResult;

    sl_init(cores);

    int maxIters = 1000;
    int imageWidth = 1920;
    int imageHeight = 1200;
    double maxMag = 256;

    double xmin = -2.5;
    double xmax = 1.0;
    double ymin = -1.0;
    double ymax = 1.0;

    CImg<unsigned char> visu(imageWidth, imageHeight, 1, 3);
    CImgDisplay draw_disp(visu, "Mandelbrot Fractal in SequenceL");

    bool redraw = true;

    SLTimer t;

    double computeTime;
    double colorTime;
    double renderTime;
    
    while(!draw_disp.is_closed())
    {
        if(redraw)
        {
            redraw = false;
            
            t.start();
            sl_zSquared(xmin, xmax, imageWidth, ymin, ymax, imageHeight, maxMag, maxIters, cores, computeResult);
            t.stop();
            computeTime = t.getTime();

            t.start();
            sl_getSmoothColorings(computeResult, maxIters, cores, colorResult);
            t.stop();
            colorTime = t.getTime();

            t.start();

            visu.fill(0);
            for(int i = 1; i <= colorResult.size(); i++)
            {
                for(int j = 1; j <= colorResult[i].size(); j++)
                {
                    visu(j-1,i-1,0,0) = colorResult[i][j].R;
                    visu(j-1,i-1,0,1) = colorResult[i][j].G;
                    visu(j-1,i-1,0,2) = colorResult[i][j].B;
                }
            }
            visu.display(draw_disp);

            t.stop();

            renderTime = t.getTime();

            draw_disp.set_title("X:[%f, %f] Y:[%f, %f] | Mandelbrot Fractal in SequenceL | Compute Time: %f | Color Time: %f | Render Time: %f | Total FPS: %f", xmin, xmax, ymin, ymax, cores, computeTime, colorTime, renderTime, 1 / (computeTime + colorTime + renderTime));
        }
        
        draw_disp.wait();

        double xdiff = (xmax - xmin);
        double ydiff = (ymax - ymin);

        double xcenter = ((1.0 * draw_disp.mouse_x()) / imageWidth) * xdiff + xmin;
        double ycenter = ((1.0 * draw_disp.mouse_y()) / imageHeight) * ydiff + ymin;

        if(draw_disp.button()&1)
        {
            redraw = true;
            xmin = xcenter - (xdiff / 4);
            xmax = xcenter + (xdiff / 4);
            ymin = ycenter - (ydiff / 4);
            ymax = ycenter + (ydiff / 4);
        }
        else if(draw_disp.button()&2)
        {
            redraw = true;
            xmin = xcenter - xdiff;
            xmax = xcenter + xdiff;
            ymin = ycenter - ydiff;
            ymax = ycenter + ydiff;
        }
    }

    sl_done();

    return 0;
}
```


{{out}}
[https://i.imgur.com/xeM4u9O.png Output Screenshot]



## Sidef


```ruby
func mandelbrot(z) {
    var c = z
    {   z = (z*z + c)
        z.abs > 2 && return true
    } * 20
    return false
}
 
for y range(1, -1, -0.05) {
    for x in range(-2, 0.5, 0.0315) {
        print(mandelbrot(x + y.i) ? ' ' : '#')
    }
    print "\n"
}
```



## Spin

{{works with|BST/BSTC}}
{{works with|FastSpin/FlexSpin}}
{{works with|HomeSpun}}
{{works with|OpenSpin}}

```spin
con
  _clkmode = xtal1+pll16x
  _clkfreq = 80_000_000

  xmin=-8601    ' int(-2.1*4096)
  xmax=2867     ' int( 0.7*4096)

  ymin=-4915    ' int(-1.2*4096)
  ymax=4915     ' int( 1.2*4096)

  maxiter=25

obj
  ser : "FullDuplexSerial"

pub main | c,cx,cy,dx,dy,x,y,x2,y2,iter

  ser.start(31, 30, 0, 115200)

  dx:=(xmax-xmin)/79
  dy:=(ymax-ymin)/24

  cy:=ymin
  repeat while cy=<ymax
    cx:=xmin
    repeat while cx=<xmax
      x:=0
      y:=0
      x2:=0
      y2:=0
      iter:=0
      repeat while iter=<maxiter and x2+y2=<16384
        y:=((x*y)~>11)+cy
        x:=x2-y2+cx
        iter+=1
        x2:=(x*x)~>12
        y2:=(y*y)~>12
      cx+=dx
      ser.tx(iter+32)
    cy+=dy
    ser.str(string(13,10))

  waitcnt(_clkfreq+cnt)
  ser.stop
```


{{out}}

```txt

!!!!!!!!!!!!!!!"""""""""""""####################################""""""""""""""""
!!!!!!!!!!!!!"""""""""#######################$$$$$$$%'+)%%%$$$$$#####"""""""""""
!!!!!!!!!!!"""""""#######################$$$$$$$$%%%&&(+,)++&%$$$$$$######""""""
!!!!!!!!!"""""#######################$$$$$$$$$$%%%%&')*5:/+('&%%$$$$$$#######"""
!!!!!!!!""""#####################$$$$$$$$$$%%%&&&''),:::::::,'&%%%%%$$$$########
!!!!!!!"""####################$$$$$$$$%%%&'())((())*,::::::/+))('&&&&)'%$$######
!!!!!!""###################$$$$$%%%%%%&&&'+.:::/::::::::::::::::/++:..93%%$#####
!!!!!"################$$$%%%%%%%%%%&&&&'),+2:::::::::::::::::::::::::1(&&%$$####
!!!!"##########$$$$$%%&(-(''''''''''''(*,5::::::::::::::::::::::::::::+)-&%$$###
!!!!####$$$$$$$$%%%%%&'(*-:1.+.:-4+))**:::::::::::::::::::::::::::::::4-(&%$$$##
!!!!#$$$$$$$$$%%%%%%'''++.6:::::::::8/0::::::::::::::::::::::::::::::::3(%%$$$$#
!!!#$$$$$$$%&&&&''()/-5.5::::::::::::::::::::::::::::::::::::::::::::::'&%%$$$$#
!!!(**+/+:523/80/46::::::::::::::::::::::::::::::::::::::::::::::::4+)'&&%%$$$$#
!!!#$$$$$$$%&&&&''().-2.:::::::::::::::::::::::::::::::::::::::::::::::'&%%$$$$#
!!!!#$$$$$$$$$%%%%%&'''/,.7::::::::::/0::::::::::::::::::::::::::::::::0'%%$$$$#
!!!!####$$$$$$$$%%%%%&'(*-:2.,/:-5+))**:::::::::::::::::::::::::::::::4+(&%$$$##
!!!!"##########$$$$$%%&(-(''''(''''''((*,4:::::::::::::::::::::::::::4+).&%$$###
!!!!!"################$$$%%%%%%%%%%&&&&'):,4:::::::::::::::::::::::::/('&%%$####
!!!!!!""##################$$$$$$%%%%%%&&&'*.:::0::::::::::::::::1,,://9)%%$#####
!!!!!!!"""####################$$$$$$$$%%%&(())((()**-::::::/+)))'&&&')'%$$######
!!!!!!!!""""#####################$$$$$$$$$$%%%&&&''(,:::::::+'&&%%%%%$$$########
!!!!!!!!!"""""#######################$$$$$$$$$$%%%%&')*7:0+('&%%%$$$$$#######"""
!!!!!!!!!!!"""""""######################$$$$$$$$$%%%&&(+-).*&%$$$$$$######""""""
!!!!!!!!!!!!!"""""""""#######################$$$$$$%%'3(%%%$$$$$######""""""""""
!!!!!!!!!!!!!!!""""""""""""#####################################""""""""""""""""

```



## SPL


```spl
w,h = #.scrsize()
sfx = -2.5; sfy = -2*h/w; fs = 4/w
#.aaoff()
> y, 1...h
  > x, 1...w
    fx = sfx + x*fs; fy = sfy + y*fs
    #.drawpoint(x,y,color(fx,fy):3)
  <
<
color(x,y)=
  zr = x; zi = y; n = 0; maxn = 150
  > zr*zr+zi*zi<4 & n<maxn
    zrn = zr*zr-zi*zi+x; zin = 2*zr*zi+y
    zr = zrn; zi = zin; n += 1
  <
  ? n=maxn, <= 0,0,0
  <= #.hsv2rgb(n/maxn*360,1,1):3
.
```



## Tcl

{{libheader|Tk}}
This code makes extensive use of Tk's built-in photo image system, which provides a 32-bit RGBA plotting surface that can be then quickly drawn in any number of places in the application. It uses a computational color scheme that was easy to code...

```tcl
package require Tk

proc mandelIters {cx cy} {
    set x [set y 0.0]
    for {set count 0} {hypot($x,$y) < 2 && $count < 255} {incr count} {
        set x1 [expr {$x*$x - $y*$y + $cx}]
        set y1 [expr {2*$x*$y + $cy}]
        set x $x1; set y $y1
    }
    return $count
}
proc mandelColor {iter} {
    set r [expr {16*($iter % 15)}]
    set g [expr {32*($iter % 7)}]
    set b [expr {8*($iter % 31)}]
    format "#%02x%02x%02x" $r $g $b
}
image create photo mandel -width 300 -height 300
# Build picture in strips, updating as we go so we have "progress" monitoring
# Also set the cursor to tell the user to wait while we work.
pack [label .mandel -image mandel -cursor watch]
update
for {set x 0} {$x < 300} {incr x} {
    for {set y 0} {$y < 300} {incr y} {
        set i [mandelIters [expr {($x-220)/100.}] [expr {($y-150)/90.}]]
        mandel put [mandelColor $i] -to $x $y
    }
    update
}
.mandel configure -cursor {}
```



## TeX

{{libheader|pst-fractal}}
The <code>pst-fractal</code> package includes a Mandelbrot set drawn by emitting [[PostScript]] code (using [[PSTricks]]), so the actual work done in the printer or PostScript interpreter.


```TeX
% Plain TeX
\input pst-fractal
\psfractal[type=Mandel,xWidth=14cm,yWidth=12cm,maxIter=30,dIter=20]
          (-2.5,-1.5)(1,1.5)
\end
```


The coordinates are a rectangle in the complex plane to draw, scaled up to <code>xWidth,yWidth</code>.  More iterations with <code>maxIter</code> is higher resolution but slower.  <code>dIter</code> is a scale factor for the colours.
The <code>pstricks-examples</code> package which is samples from the PSTricks book includes similar for [[LaTeX]] (<code>25-02-6.ltx</code> and <code>33-02-6.ltx</code>).

{{libheader|PGF}}
The PGF <code>shadings</code> library includes a Mandelbrot set.  In PGF 3.0 the calculations are done in [[PostScript]] code emitted, so the output size is small but it only does 10 iterations so is very low resolution.


```TeX
\documentclass{article}
\usepackage{tikz}
\usetikzlibrary{shadings}
\begin{document}
\begin{tikzpicture}
  \shade[shading=Mandelbrot set] (0,0) rectangle (4,4);
\end{tikzpicture}
\end{document}
```


{{libheader|LuaTeX}}
[[LuaLaTeX]] plus pgfplots code can be found at [http://texwelt.de/wissen/fragen/3960/fraktale-mit-pgfplots http://texwelt.de/wissen/fragen/3960/fraktale-mit-pgfplots].  The calculations are done by inline [[Lua]] code and the resulting bitmap shown with a PGF plot.

=={{header|TI-83 BASIC}}==
Based on the [[Mandelbrot_set#BASIC|BASIC Version]]. Due to the TI-83's lack of power, it takes around 2 hours to complete at 16 iterations.

```ti83b
PROGRAM:MANDELBR
:Input "ITER. ",D
:For(A,Xmin,Xmax,ΔX)
:For(B,Ymin,Ymax,ΔY)
:0→X
:0→Y
:0→I
:D→M
:While X^2+Y^2≤4 and I<M
:X^2-Y^2+A→R
:2XY+B→Y
:R→X
:I+1→I
:End
:If I≠M
:Then
:I→C
:Else
:0→C
:End
:If C<1
:Pt-On(A,B)
:End
:End
:End

```



## TXR

{{trans|Scheme}}

Creates same <code>mandelbrot.pgm</code> file.


```txrlisp
(defvar x-centre -0.5)
(defvar y-centre 0.0)
(defvar width 4.0)
(defvar i-max 800)
(defvar j-max 600)
(defvar n 100)
(defvar r-max 2.0)
(defvar file "mandelbrot.pgm")
(defvar colour-max 255)
(defvar pixel-size (/ width i-max))
(defvar x-offset (- x-centre (* 0.5 pixel-size (+ i-max 1))))
(defvar y-offset (+ y-centre (* 0.5 pixel-size (+ j-max 1))))

;; with-output-to-file macro
(defmacro with-output-to-file (name . body)
  ^(let ((*stdout* (open-file ,name "w")))
     (unwind-protect (progn ,*body) (close-stream *stdout*))))

;; complex number library
(defmacro cplx (x y) ^(cons ,x ,y))
(defmacro re (c) ^(car ,c))
(defmacro im (c) ^(cdr ,c))

(defsymacro c0 '(0 . 0))

(macro-time 
  (defun with-cplx-expand (specs body)
    (tree-case specs
       (((re im expr) . rest) 
        ^(tree-bind (,re . ,im) ,expr ,(with-cplx-expand rest body)))
       (() (tree-case body
             ((a b . rest) ^(progn ,a ,b ,*rest))
             ((a) a)
             (x (error "with-cplx: invalid body ~s" body))))
       (x (error "with-cplx: bad args ~s" x)))))

(defmacro with-cplx (specs . body)
  (with-cplx-expand specs body))

(defun c+ (x y)
  (with-cplx ((a b x) (c d y))
    (cplx (+ a c) (+ b d))))

(defun c* (x y)
  (with-cplx ((a b x) (c d y))
    (cplx (- (* a c) (* b d)) (+ (* b c) (* a d)))))

(defun modulus (z)
  (with-cplx ((a b z))
    (sqrt (+ (* a a) (* b b)))))

;; Mandelbrot routines
(defun inside-p (z0 : (z c0) (n n))
  (and (< (modulus z) r-max)
       (or (zerop n)
           (inside-p z0 (c+ (c* z z) z0) (- n 1)))))

(defmacro int-bool (b)
  ^(if ,b colour-max 0))

(defun pixel (i j)
  (int-bool
    (inside-p
      (cplx (+ x-offset (* pixel-size i))
            (- y-offset (* pixel-size j))))))

;; Mandelbrot loop and output
(defun plot ()
  (with-output-to-file file
    (format t "P2\n~s\n~s\n~s\n" i-max j-max colour-max)
    (each ((j (range 1 j-max)))
      (each ((i (range 1 i-max)))
        (format *stdout* "~s " (pixel i j)))
      (put-line "" *stdout*))))

(plot)
```



## uBasic/4tH

uBasic does not support floating point calculations, so fixed point arithmetic is used, with Value 10000 representing 1.0. The Mandelbrot image is drawn using ASCII characters 1-9 to show number of iterations. Iteration count 10 or more is represented with '@'. To compensate the aspect ratio of
the font, step sizes in x and y directions are different.
<lang>A =-21000                              ' Left Edge = -2.1
B = 15000                              ' Right Edge = 1.5
C = 15000                              ' Top Edge = 1.5
D =-15000                              ' Bottom Edge = -1.5
E = 200                                ' Max Iteration Depth
F = 350                                ' X Step Size
G = 750                                ' Y Step Size

For L = C To D Step -G                 ' Y0
    For K = A To B-1 Step F            ' X0
        V = 0                          ' Y
        U = 0                          ' X
        I = 32                         ' Char To Be Displayed
        For O = 0 To E-1               ' Iteration
            X = (U/10 * U) / 1000      ' X*X
            Y = (V/10 * V) / 1000      ' Y*Y
            If (X + Y > 40000)
                I = 48 + O             ' Print Digit 0...9
                If (O > 9)             ' If Iteration Count > 9,
                    I = 64             '  Print '@'
                Endif
                Break
            Endif
            Z = X - Y + K              ' Temp = X*X - Y*Y + X0
            V = (U/10 * V) / 500 + L   ' Y = 2*X*Y + Y0
            U = Z                      ' X = Temp
        Next
        Gosub I                        '  Ins_char(I)
    Next
    Print
Next

End
                                       ' Translate number to ASCII
32 Print " "; : Return
48 Print "0"; : Return
49 Print "1"; : Return
50 Print "2"; : Return
51 Print "3"; : Return
52 Print "4"; : Return
53 Print "5"; : Return
54 Print "6"; : Return
55 Print "7"; : Return
56 Print "8"; : Return
57 Print "9"; : Return
64 Print "@"; : Return
```

Output:

```txt
1111111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222211111
1111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222222222211
1111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222
1111111111111111222222222222222222233333333333333333333333222222222222222222222222222222222222222222222
1111111111111112222222222222333333333333333333333333333333333333322222222222222222222222222222222222222
1111111111111222222222233333333333333333333333344444457655544443333332222222222222222222222222222222222
1111111111112222222233333333333333333333333444444445567@@6665444444333333222222222222222222222222222222
11111111111222222333333333333333333333334444444445555678@@@@7654444443333332222222222222222222222222222
1111111112222223333333333333333333333444444444455556789@@@@98655544444433333332222222222222222222222222
1111111122222333333333333333333333344444444445556668@@@   @@@@76555544444333333322222222222222222222222
1111111122233333333333333333333344444444445566667778@@      @987666555544433333333222222222222222222222
111111122333333333333333333333444444455556@@@@@99@@@@@@    @@@@@@87777@95443333333322222222222222222222
1111112233333333333333333334444455555556678@@  @@                @@@@@@@8544333333333222222222222222222
1111122333333333333333334445555555555666789@@@                        @86554433333333322222222222222222
1111123333333333333444466666555556666778@@@@                         @@87655443333333332222222222222222
111123333333344444455568@887789@8777788@@@                            @@@@65443333333332222222222222222
1111333344444444455556679@@@@@@@@@@@99@@@                              @@765444333333333222222222222222
1111334444444455555567789@@         @@@@                                @855444333333333222222222222222
11114444444455555668@99@@@            @                                @@655444433333333322222222222222
11134555556666677789@@@ @                                             @86655444433333333322222222222222
111                                                                 @@876555444433333333322222222222222
11134555556666677789@@@ @                                             @86655444433333333322222222222222
11114444444455555668@99@@@            @                                @@655444433333333322222222222222
1111334444444455555567789@@         @@@@                                @855444333333333222222222222222
1111333344444444455556679@@@@@@@@@@@99@@@                              @@765444333333333222222222222222
111123333333344444455568@887789@8777788@@@                            @@@@65443333333332222222222222222
1111123333333333333444466666555556666778@@@@                         @@87655443333333332222222222222222
1111122333333333333333334445555555555666789@@@                        @86554433333333322222222222222222
1111112233333333333333333334444455555556678@@  @@                @@@@@@@8544333333333222222222222222222
111111122333333333333333333333444444455556@@@@@99@@@@@@    @@@@@@87777@95443333333322222222222222222222
1111111122233333333333333333333344444444445566667778@@      @987666555544433333333222222222222222222222
1111111122222333333333333333333333344444444445556668@@@   @@@@76555544444333333322222222222222222222222
1111111112222223333333333333333333333444444444455556789@@@@98655544444433333332222222222222222222222222
11111111111222222333333333333333333333334444444445555678@@@@7654444443333332222222222222222222222222222
1111111111112222222233333333333333333333333444444445567@@6665444444333333222222222222222222222222222222
1111111111111222222222233333333333333333333333344444457655544443333332222222222222222222222222222222222
1111111111111112222222222222333333333333333333333333333333333333322222222222222222222222222222222222222
1111111111111111222222222222222222233333333333333333333333222222222222222222222222222222222222222222222
1111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222
1111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222222222211
1111111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222211111

0 OK, 0:1726    
```



## Vedit macro language

Vedit macro language does not support floating point calculations, so fixed point arithmetic is used, with Value 10000 representing 1.0.
The Mandelbrot image is drawn using ASCII characters 1-9 to show number of iterations. Iteration count 10 or more is represented with '@'.
To compensate the aspect ratio of the font, step sizes in x and y directions are different.

```vedit
#1 =-21000              // left edge = -2.1
#2 = 15000              // right edge = 1.5
#3 = 15000              // top edge = 1.5
#4 =-15000              // bottom edge = -1.5
#5 = 200                // max iteration depth
#6 = 350                // x step size
#7 = 750                // y step size

Buf_Switch(Buf_Free)
for (#12 = #3; #12 > #4; #12 -= #7) {                   // y0
    for (#11 = #1; #11 < #2; #11 += #6) {               // x0
        #22 = 0                                         // y
        #21 = 0                                         // x
        #9 = ' '                                        // char to be displayed
        for (#15 = 0; #15 < #5; #15++) {                // iteration
            #31 = (#21/10 * #21) / 1000                 // x*x
            #32 = (#22/10 * #22) / 1000                 // y*y
            if (#31 + #32 > 40000) {
                #9 = '0' + #15                          // print digit 0...9
                if (#15 > 9) {                          // if iteration count > 9,
                    #9 = '@'                            //  print '@'
                }
                break
            }
            #33 = #31 - #32 + #11                       // temp = x*x - y*y + x0
            #22 = (#21/10 * #22) / 500 + #12            // y = 2*x*y + y0
            #21 = #33                                   // x = temp
        }
        Ins_Char(#9)
    }
    Ins_Newline
}
BOF

```

{{out}}
<small>

```txt

1111111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222211111
1111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222222222211
1111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222
1111111111111111222222222222222222233333333333333333333333222222222222222222222222222222222222222222222
1111111111111112222222222222333333333333333333333333333333333333322222222222222222222222222222222222222
1111111111111222222222233333333333333333333333344444457655544443333332222222222222222222222222222222222
1111111111112222222233333333333333333333333444444445567@@6665444444333333222222222222222222222222222222
11111111111222222333333333333333333333334444444445555678@@@@7654444443333332222222222222222222222222222
1111111112222223333333333333333333333444444444455556789@@@@98655544444433333332222222222222222222222222
1111111122222333333333333333333333344444444445556668@@@   @@@@76555544444333333322222222222222222222222
1111111122233333333333333333333344444444445566667778@@      @987666555544433333333222222222222222222222
111111122333333333333333333333444444455556@@@@@99@@@@@@    @@@@@@87777@95443333333322222222222222222222
1111112233333333333333333334444455555556678@@  @@                @@@@@@@8544333333333222222222222222222
1111122333333333333333334445555555555666789@@@                        @86554433333333322222222222222222
1111123333333333333444466666555556666778@@@@                         @@87655443333333332222222222222222
111123333333344444455568@887789@8777788@@@                            @@@@65443333333332222222222222222
1111333344444444455556679@@@@@@@@@@@99@@@                              @@765444333333333222222222222222
1111334444444455555567789@@         @@@@                                @855444333333333222222222222222
11114444444455555668@99@@@            @                                @@655444433333333322222222222222
11134555556666677789@@@ @                                             @86655444433333333322222222222222
111                                                                 @@876555444433333333322222222222222
11134555556666677789@@@ @                                             @86655444433333333322222222222222
11114444444455555668@99@@@            @                                @@655444433333333322222222222222
1111334444444455555567789@@         @@@@                                @855444333333333222222222222222
1111333344444444455556679@@@@@@@@@@@99@@@                              @@765444333333333222222222222222
111123333333344444455568@887789@8777788@@@                            @@@@65443333333332222222222222222
1111123333333333333444466666555556666778@@@@                         @@87655443333333332222222222222222
1111122333333333333333334445555555555666789@@@                        @86554433333333322222222222222222
1111112233333333333333333334444455555556678@@  @@                @@@@@@@8544333333333222222222222222222
111111122333333333333333333333444444455556@@@@@99@@@@@@    @@@@@@87777@95443333333322222222222222222222
1111111122233333333333333333333344444444445566667778@@      @987666555544433333333222222222222222222222
1111111122222333333333333333333333344444444445556668@@@   @@@@76555544444333333322222222222222222222222
1111111112222223333333333333333333333444444444455556789@@@@98655544444433333332222222222222222222222222
11111111111222222333333333333333333333334444444445555678@@@@7654444443333332222222222222222222222222222
1111111111112222222233333333333333333333333444444445567@@6665444444333333222222222222222222222222222222
1111111111111222222222233333333333333333333333344444457655544443333332222222222222222222222222222222222
1111111111111112222222222222333333333333333333333333333333333333322222222222222222222222222222222222222
1111111111111111222222222222222222233333333333333333333333222222222222222222222222222222222222222222222
1111111111111111112222222222222222222222222222222222222222222222222222222222222222222222222222222222222
1111111111111111111122222222222222222222222222222222222222222222222222222222222222222222222222222222211

```

</small>


## XPL0


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
int     X, Y,                   \screen coordinates of current point
        Cnt;                    \iteration counter
real    Cx, Cy,                 \coordinates scaled to +/-2 range
        Zx, Zy,                 \complex accumulator
        Temp;                   \temporary scratch
[SetVid($112);                                  \set 640x480x24 graphics mode
for Y:= 0 to 480-1 do                           \for all points on the screen...
    for X:= 0 to 640-1 do
        [Cx:= (float(X)/640.0 - 0.5) * 4.0;     \range: -2.0 to +2.0
         Cy:= (float(Y-240)/240.0) * 1.5;       \range: -1.5 to +1.5
         Cnt:= 0;  Zx:= 0.0;  Zy:= 0.0;         \initialize
         loop   [if Zx*Zx + Zy*Zy > 2.0 then    \Z heads toward infinity
                    [Point(X, Y, Cnt<<21+Cnt<<10+Cnt<<3); \set color of pixel to
                    quit;                       \ rate it approached infinity
                    ];                          \move on to next point
                Temp:= Zx*Zy;
                Zx:= Zx*Zx - Zy*Zy + Cx;        \calculate next iteration of Z
                Zy:= 2.0*Temp + Cy;
                Cnt:= Cnt+1;                    \count number of iterations
                if Cnt >= 1000 then quit;       \assume point is in Mandelbrot
                ];                              \ set and leave it colored black
        ];
X:= ChIn(1);                                    \wait for keystroke
SetVid($03);                                    \restore normal text display
]
```

{{out}}
[[File:MandelXPL0.png]]


## XSLT

The fact that you can create an image of the Mandelbrot Set with XSLT is sometimes under-appreciated. 
However, it has been discussed extensively [http://thedailywtf.com/Articles/Stupid-Coding-Tricks-XSLT-Mandelbrot.aspx on the internet] so is best reproduced here, and the code can be executed directly in your browser at that site.

{{omit from|M4}}


```xml

<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- XSLT Mandelbrot - written by Joel Yliluoma 2007, http://iki.fi/bisqwit/ -->

<xsl:output method="html" indent="no"
  doctype-public="-//W3C//DTD HTML 4.01//EN"
  doctype-system="http://www.w3.org/TR/REC-html40/strict.dtd"
 />

<xsl:template match="/fractal">
 <html>
  <head>
   <title>XSLT fractal</title>
   <style type="text/css">
body { color:#55F; background:#000 }
pre { font-family:monospace; font-size:7px }
pre span { background:<xsl:value-of select="background" /> }
   </style>
  </head>
  <body>
   <div style="position:absolute;top:20px;left:20em">
    Copyright © 1992,2007 Joel Yliluoma
    (<a href="http://iki.fi/bisqwit/">http://iki.fi/bisqwit/</a>)
   </div>
   <h1 style="margin:0px">XSLT fractal</h1>
   
```txt
<xsl:call-template name="bisqwit-mandelbrot" />
```

  </body>
 </html>
</xsl:template>

<xsl:template name="bisqwit-mandelbrot"
  ><xsl:call-template name="bisqwit-mandelbrot-line">
   <xsl:with-param name="y" select="y/min"/>
  </xsl:call-template
></xsl:template>

<xsl:template name="bisqwit-mandelbrot-line"
 ><xsl:param name="y"
 /><xsl:call-template name="bisqwit-mandelbrot-column">
  <xsl:with-param name="x" select="x/min"/>
  <xsl:with-param name="y" select="$y"/>
 </xsl:call-template
 ><xsl:if test="$y < y/max"
  ><br
  /><xsl:call-template name="bisqwit-mandelbrot-line">
   <xsl:with-param name="y" select="$y + y/step"/>
  </xsl:call-template
 ></xsl:if
></xsl:template>

<xsl:template name="bisqwit-mandelbrot-column"
 ><xsl:param name="x"
 /><xsl:param name="y"
 /><xsl:call-template name="bisqwit-mandelbrot-slot">
  <xsl:with-param name="x" select="$x" />
  <xsl:with-param name="y" select="$y" />
  <xsl:with-param name="zr" select="$x" />
  <xsl:with-param name="zi" select="$y" />
 </xsl:call-template
 ><xsl:if test="$x < x/max"
  ><xsl:call-template name="bisqwit-mandelbrot-column">
   <xsl:with-param name="x" select="$x + x/step"/>
   <xsl:with-param name="y" select="$y" />
  </xsl:call-template
 ></xsl:if
></xsl:template>

<xsl:template name="bisqwit-mandelbrot-slot"
><xsl:param name="x"
 /><xsl:param name="y"
 /><xsl:param name="zr"
 /><xsl:param name="zi"
 /><xsl:param name="iter" select="0"
 /><xsl:variable name="zrsqr" select="($zr * $zr)"
 /><xsl:variable name="zisqr" select="($zi * $zi)"
 /><xsl:choose>
  <xsl:when test="(4*scale*scale >= $zrsqr + $zisqr) and (maxiter > $iter+1)"
   ><xsl:call-template name="bisqwit-mandelbrot-slot">
    <xsl:with-param name="x" select="$x" />
    <xsl:with-param name="y" select="$y" />
    <xsl:with-param name="zi" select="(2 * $zr * $zi) div scale + $y" />
    <xsl:with-param name="zr" select="($zrsqr - $zisqr) div scale + $x" />
    <xsl:with-param name="iter" select="$iter + 1" />
   </xsl:call-template
  ></xsl:when>
  <xsl:otherwise
   ><xsl:variable name="magnitude" select="magnitude[@value=$iter]"
    /><span style="color:{$magnitude/color}"
   ><xsl:value-of select="$magnitude/symbol"
  /></span></xsl:otherwise>
 </xsl:choose
></xsl:template>
 
</xsl:stylesheet>

```



## Yabasic


```Yabasic
open window 640, 320
wid = 4
xcenter = -1: ycenter = 0
ms = 0
for xcoord = 0 to 639
   for ycoord = 0 to 160
       ms = 0
       ca =(xcoord-320)/640*wid+xcenter
       cb =(ycoord-160)/640*wid+ycenter
       x = 0: y=0

       for t = 1 to 20
           xnew = x*x-y*y+ca
           ynew = 2*x*y+cb
           x=xnew:y=ynew
           magnitudesquared=x*x+y*y
           ms = magnitudesquared
           if (magnitudesquared > 100) break
           //if(magnitudesquared < 100) then : color 0,0,0 : dot xcoord, ycoord : end if
       next t
       ms = ms+1
       if(ms > 250) then
       	    color 32,64,mod(ms,255)
            dot xcoord, ycoord
            dot xcoord, 320- ycoord
        elseif (ms > 150) then
            color mod(ms,255),64,32
            dot xcoord, ycoord
            dot xcoord, 320-ycoord
        else
            color 0,0,0
            dot xcoord, ycoord
            dot xcoord, 320-ycoord
        end if
    next ycoord
next xcoord

```



## Z80 Assembly


```Z80

;
;  Compute a Mandelbrot set on a simple Z80 computer.
;
; Porting this program to another Z80 platform should be easy and straight-
; forward: The only dependencies on my homebrew machine are the system-calls 
; used to print strings and characters. These calls are performed by loading
; IX with the number of the system-call and performing an RST 08. To port this
; program to another operating system just replace these system-calls with 
; the appropriate versions. Only three system-calls are used in the following:
; _crlf: Prints a CR/LF, _puts: Prints a 0-terminated string (the adress of 
; which is expected in HL), and _putc: Print a single character which is 
; expected in A. RST 0 give control back to the monitor.
;
#include        "mondef.asm"

                org     ram_start

scale           equ     256                     ; Do NOT change this - the 
                                                ; arithmetic routines rely on
                                                ; this scaling factor! :-)
divergent       equ     scale * 4

                ld      hl, welcome             ; Print a welcome message
                ld      ix, _puts
                rst     08

; for (y = <initial_value> ; y <= y_end; y += y_step)
; {
outer_loop      ld      hl, (y_end)             ; Is y <= y_end?
                ld      de, (y)
                and     a                       ; Clear carry
                sbc     hl, de                  ; Perform the comparison
                jp      m, mandel_end           ; End of outer loop reached

;    for (x = x_start; x <= x_end; x += x_step)
;    {
                ld      hl, (x_start)           ; x = x_start
                ld      (x), hl
inner_loop      ld      hl, (x_end)             ; Is x <= x_end?
                ld      de, (x)
                and     a
                sbc     hl, de
                jp      m, inner_loop_end       ; End of inner loop reached

;      z_0 = z_1 = 0;
                ld      hl, 0
                ld      (z_0), hl
                ld      (z_1), hl

;      for (iteration = iteration_max; iteration; iteration--)
;      {
                ld      a, (iteration_max)
                ld      b, a
iteration_loop  push    bc                      ; iteration -> stack
;        z2 = (z_0 * z_0 - z_1 * z_1) / SCALE;
                ld      de, (z_1)               ; Compute DE HL = z_1 * z_1
                ld      bc, de
                call    mul_16
                ld      (z_0_square_low), hl    ; z_0 ** 2 is needed later again
                ld      (z_0_square_high), de

                ld      de, (z_0)               ; Compute DE HL = z_0 * z_0
                ld      bc, de
                call    mul_16
                ld      (z_1_square_low), hl    ; z_1 ** 2 will be also needed
                ld      (z_1_square_high), de

                and     a                       ; Compute subtraction
                ld      bc, (z_0_square_low)
                sbc     hl, bc
                ld      (scratch_0), hl         ; Save lower 16 bit of result
                ld      hl, de
                ld      bc, (z_0_square_high)
                sbc     hl, bc
                ld      bc, (scratch_0)         ; HL BC = z_0 ** 2 - z_1 ** 2

                ld      c, b                    ; Divide by scale = 256
                ld      b, l                    ; Discard the rest
                push    bc                      ; We need BC later

;        z3 = 2 * z0 * z1 / SCALE;
                ld      hl, (z_0)               ; Compute DE HL = 2 * z_0 * z_1
                add     hl, hl
                ld      de, hl
                ld      bc, (z_1)
                call    mul_16

                ld      b, e                    ; Divide by scale (= 256)
                ld      c, h                    ; BC contains now z_3

;        z1 = z3 + y;
                ld      hl, (y)
                add     hl, bc
                ld      (z_1), hl

;        z_0 = z_2 + x;
                pop     bc                      ; Here BC is needed again :-)
                ld      hl, (x)
                add     hl, bc
                ld      (z_0), hl

;        if (z0 * z0 / SCALE + z1 * z1 / SCALE > 4 * SCALE)
                ld      hl, (z_0_square_low)    ; Use the squares computed
                ld      de, (z_1_square_low)    ; above
                add     hl, de
                ld      bc, hl                  ; BC contains lower word of sum

                ld      hl, (z_0_square_high)
                ld      de, (z_1_square_high)
                adc     hl, de

                ld      h, l                    ; HL now contains (z_0 ** 2 + 
                ld      l, b                    ; z_1 ** 2) / scale

                ld      bc, divergent
                and     a
                sbc     hl, bc

;          break;
                jp      c, iteration_dec        ; No break
                pop     bc                      ; Get latest iteration counter
                jr      iteration_end           ; Exit loop

;        iteration++;
iteration_dec   pop     bc                      ; Get iteration counter
                djnz    iteration_loop          ; We might fall through!
;      }
iteration_end
;      printf("%c", display[iteration % 7]);
                ld      a, b
                and     $7                      ; lower three bits only (c = 0)
                sbc     hl, hl
                ld      l, a
                ld      de, display             ; Get start of character array
                add     hl, de                  ; address and load the 
                ld      a, (hl)                 ; character to be printed
                ld      ix, _putc               ; Print the character
                rst     08

                ld      de, (x_step)            ; x += x_step
                ld      hl, (x)
                add     hl, de
                ld      (x), hl

                jp      inner_loop
;    }
;    printf("\n");
inner_loop_end  ld      ix, _crlf               ; Print a CR/LF pair
                rst     08

                ld      de, (y_step)            ; y += y_step
                ld      hl, (y)
                add     hl, de
                ld      (y), hl                 ; Store new y-value

                jp      outer_loop
; }

mandel_end      ld      hl, finished            ; Print finished-message
                ld      ix, _puts
                rst     08

                rst     0                       ; Return to the monitor

welcome         defb    "Generating a Mandelbrot set"
                defb    cr, lf, eos
finished        defb    "Computation finished.", cr, lf, eos

iteration_max   defb    10                      ; How many iterations
x               defw    0                       ; x-coordinate
x_start         defw    -2 * scale              ; Minimum x-coordinate
x_end           defw    5 *  scale / 10         ; Maximum x-coordinate
x_step          defw    4  * scale / 100        ; x-coordinate step-width
y               defw    -1 * scale              ; Minimum y-coordinate
y_end           defw    1  * scale              ; Maximum y-coordinate
y_step          defw    1  * scale / 10         ; y-coordinate step-width
z_0             defw    0
z_1             defw    0
scratch_0       defw    0
z_0_square_high defw    0
z_0_square_low  defw    0
z_1_square_high defw    0
z_1_square_low  defw    0
display         defb    " .-+*=#@"              ; 8 characters for the display

;
;   Compute DEHL = BC * DE (signed): This routine is not too clever but it 
; works. It is based on a standard 16-by-16 multiplication routine for unsigned
; integers. At the beginning the sign of the result is determined based on the
; signs of the operands which are negated if necessary. Then the unsigned
; multiplication takes place, followed by negating the result if necessary.
;
mul_16          xor     a                       ; Clear carry and A (-> +)
                bit     7, b                    ; Is BC negative?
                jr      z, bc_positive          ; No
                sub     c                       ; A is still zero, complement
                ld      c, a
                ld      a, 0
                sbc     a, b
                ld      b, a
                scf                             ; Set carry (-> -)
bc_positive     bit     7, D                    ; Is DE negative?
                jr      z, de_positive          ; No
                push    af                      ; Remember carry for later!
                xor     a
                sub     e
                ld      e, a
                ld      a, 0
                sbc     a, d
                ld      d, a
                pop     af                      ; Restore carry for complement
                ccf                             ; Complement Carry (-> +/-?)
de_positive     push    af                      ; Remember state of carry
                and     a                       ; Start multiplication
                sbc     hl, hl
                ld      a, 16                   ; 16 rounds
mul_16_loop     add     hl, hl
                rl      e
                rl      d
                jr      nc, mul_16_exit
                add     hl, bc
                jr      nc, mul_16_exit
                inc     de
mul_16_exit     dec     a
                jr      nz, mul_16_loop
                pop     af                      ; Restore carry from beginning
                ret     nc                      ; No sign inversion necessary
                xor     a                       ; Complement DE HL
                sub     l
                ld      l, a
                ld      a, 0
                sbc     a, h
                ld      h, a
                ld      a, 0
                sbc     a, e
                ld      e, a
                ld      a, 0
                sbc     a, d
                ld      d, a
                ret

```

{{out}}

```txt

Generating a Mandelbrot set
.......       @@@@@@@@@@@@@@@@@@@@########===*+  .  *######@@@@@ 
......     @@@@@@@@@@@@@@@@@@@@#########====+-.    -*===#####@@@@
.....   @@@@@@@@@@@@@@@@@@@@#########===**+-         +*====####@@
....  @@@@@@@@@@@@@@@@@@@########==*+-.-+-..         .-+****+*##@
...  @@@@@@@@@@@@@@@@@#####=====**+-                          -=#
.. @@@@@@@@@@@@@@@###
### ===
***+-                          -*==
..@@@@@@@@@@####==- +*******++++-                             -+=
. @@@@#######===**-          ...                               -*
.@@#######======++-                                            +=
.#######=****+-                                               +*=
.=**++ .-....                                               .+*==
.#=====****+++.                                              -*==
.@#######=====*---                                            .*=
.@@@########====*++.                                           +=
. @@@@@@@######==*+ .-++- --++--.                               =
.. @@@@@@@@@@@@@##==*
### =
****+                            -+*=
... @@@@@@@@@@@@@@@@####
### =
***+-                          +==
...  @@@@@@@@@@@@@@@@@@@######====*-    .               --+-  *##
....   @@@@@@@@@@@@@@@@@@@@########==****++-        .-+**====###@
.....    @@@@@@@@@@@@@@@@@@@@##########===*+         *===#####@@@
.......     @@@@@@@@@@@@@@@@@@@@@########===*+-.  .+*=######@@@@@
Computation finished.

```



## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
{{trans|XPL0}}
[[File:Mandelbrot.zkl.jpg|300px|thumb|right]]

```zkl
fcn mandelbrot{  // lord this is slooooow
   bitmap:=PPM(640,480);
   foreach y,x in ([0..479],[0..639]){
      cx:=(x.toFloat()/640 - 0.5)*4.0;     //range: -2.0 to +2.0
      cy:=((y-240).toFloat()/240.0)*1.5;   //range: -1.5 to +1.5
      cnt:=0; zx:=0.0; zy:=0.0;
      do(1000){
      	 if(zx*zx + zy*zy > 2.0){	//z heads toward infinity
	    //set color of pixel to rate it approaches infinity
	    bitmap[x,y]=cnt.shiftLeft(21) + cnt.shiftLeft(10) + cnt*8;
	    break;
	 }
	 temp:=zx*zy;
	 zx=zx*zx - zy*zy + cx;		//calculate next iteration of z
	 zy=2.0*temp + cy;
	 cnt+=1;
      }
   }
   bitmap.write(File("foo.ppm","wb"));
}();
```



{{omit from|Lilypond}}
{{omit from|ML/I}}

[[Category:Geometry]]
