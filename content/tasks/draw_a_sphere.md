+++
title = "Draw a sphere"
description = ""
date = 2019-09-05T15:55:36Z
aliases = []
[extra]
id = 9389
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "arendelle",
  "ats",
  "autohotkey",
  "awk",
  "basic",
  "basic256",
  "batch_file",
  "bbc_basic",
  "befunge",
  "brlcad",
  "c",
  "clojure",
  "csharp",
  "d",
  "delphi",
  "dwscript",
  "erre",
  "freebasic",
  "futurebasic",
  "go",
  "haskell",
  "html",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "liberty_basic",
  "lingo",
  "locomotive_basic",
  "logo",
  "lua",
  "m2000_interpreter",
  "maple",
  "matlab",
  "maxima",
  "nim",
  "openscad",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "postscript",
  "processing",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ruby",
  "run_basic",
  "scala",
  "sidef",
  "sinclair_zx81_basic",
  "smalltalk",
  "svg",
  "swift",
  "tcl",
  "tex",
  "vbscript",
  "xpl0",
  "yabasic",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task

Draw a sphere.

The sphere can be represented graphically, or in ASCII art, depending on the language capabilities.

Either static or rotational projection is acceptable for this task.





## Ada

Translation from the C code at http://cairographics.org/samples/gradient
Uses the Cairo component of GtkAda to create and save as png
[[File:SphereAda.png|thumb|right]]

```Ada
with Glib;                use Glib;
with Cairo;               use Cairo;
with Cairo.Png;           use Cairo.Png;
with Cairo.Pattern;       use Cairo.Pattern;
with Cairo.Image_Surface; use Cairo.Image_Surface;
with Ada.Numerics;

procedure Sphere is
   subtype Dub is Glib.Gdouble;

   Surface    : Cairo_Surface;
   Cr         : Cairo_Context;
   Pat        : Cairo_Pattern;
   Status_Out : Cairo_Status;
   M_Pi       : constant Dub := Dub (Ada.Numerics.Pi);

begin
   Surface := Create (Cairo_Format_ARGB32, 512, 512);
   Cr      := Create (Surface);
   Pat     :=
      Cairo.Pattern.Create_Radial (230.4, 204.8, 51.1, 204.8, 204.8, 256.0);
   Cairo.Pattern.Add_Color_Stop_Rgba (Pat, 0.0, 1.0, 1.0, 1.0, 1.0);
   Cairo.Pattern.Add_Color_Stop_Rgba (Pat, 1.0, 0.0, 0.0, 0.0, 1.0);
   Cairo.Set_Source (Cr, Pat);
   Cairo.Arc (Cr, 256.0, 256.0, 153.6, 0.0, 2.0 * M_Pi);
   Cairo.Fill (Cr);
   Cairo.Pattern.Destroy (Pat);
   Status_Out := Write_To_Png (Surface, "SphereAda.png");
   pragma Assert (Status_Out = Cairo_Status_Success);
end Sphere;
```


This uses a very loose binding to SDL as found in any GPS installation. For it to work, you must choose New Project From Templte|Empty Game


```Ada

with Display; use Display;
with Display.Basic; use Display.Basic;

procedure Main is
   Ball : Shape_Id := New_Circle
     (X      => 0.0,
      Y      => 0.0,
      Radius => 20.0,
      Color  => Blue);
begin
   null;
end Main;

```



## Arendelle



```txt
[ #j ,
   [ #i ,
      { ( #x - 19 ) ^ 2 +
        ( #y - 14 ) ^ 2 &lt; 125 , p
      } r
   ] [ #i , l ] d
]
```



## ATS

<lang>
(*
** Solution to Draw_a_sphere.dats
*)

(* ****** ****** *)
//
#include
"share/atspre_define.hats" // defines some names
#include
"share/atspre_staload.hats" // for targeting C
#include
"share/HATS/atspre_staload_libats_ML.hats" // for ...
#include
"share/HATS/atslib_staload_libats_libc.hats" // for libc
//
(* ****** ****** *)

extern
fun
Draw_a_sphere
(
  R: double, k: double, ambient: double
) : void // end of [Draw_a_sphere]

(* ****** ****** *)

implement
Draw_a_sphere
(
  R: double, k: double, ambient: double
) = let
    fun normalize(v0: double, v1: double, v2: double): (double, double, double) = let
        val len = sqrt(v0*v0+v1*v1+v2*v2)
    in
        (v0/len, v1/len, v2/len)
    end // end of [normalize]

    fun dot(v0: double, v1: double, v2: double, x0: double, x1: double, x2: double): double = let
        val d = v0*x0+v1*x1+v2*x2
        val sgn = gcompare_val_val<double> (d, 0.0)
    in
        if sgn < 0 then ~d else 0.0
    end // end of [dot]

    fun print_char(i: int): void =
        if i = 0 then print!(".") else
        if i = 1 then print!(":") else
        if i = 2 then print!("!") else
        if i = 3 then print!("*") else
        if i = 4 then print!("o") else
        if i = 5 then print!("e") else
        if i = 6 then print!("&") else
        if i = 7 then print!("#") else
        if i = 8 then print!("%") else
        if i = 9 then print!("@") else print!(" ")

    val i_start = floor(~R)
    val i_end = ceil(R)
    val j_start = floor(~2 * R)
    val j_end = ceil(2 * R)
    val (l0, l1, l2) = normalize(30.0, 30.0, ~50.0)

    fun loopj(j: int, j_end: int, x: double): void = let
        val y = j / 2.0 + 0.5;
        val sgn = gcompare_val_val<double> (x*x + y*y, R*R)
        val (v0, v1, v2) = normalize(x, y, sqrt(R*R - x*x - y*y))
        val b = pow(dot(l0, l1, l2, v0, v1, v2), k) + ambient
        val intensity = 9.0 - 9.0*b
        val sgn2 = gcompare_val_val<double> (intensity, 0.0)
        val sgn3 = gcompare_val_val<double> (intensity, 9.0)
    in
    (   if sgn > 0 then print_char(10) else
        if sgn2 < 0 then print_char(0) else
        if sgn3 >= 0 then print_char(8) else
        print_char(g0float2int(intensity));
        if j < j_end then loopj(j+1, j_end, x)
    )
    end // end of [loopj]

    fun loopi(i: int, i_end: int, j: int, j_end: int): void = let
        val x = i + 0.5
        val () = loopj(j, j_end, x)
        val () = println!()
    in
        if i < i_end then loopi(i+1, i_end, j, j_end)
    end // end of [loopi]

in
    loopi(g0float2int(i_start), g0float2int(i_end), g0float2int(j_start), g0float2int(j_end))
end

(* ****** ****** *)

implement
main0() = () where
{
  val () = DrawSphere(20.0, 4.0, .1)
  val () = DrawSphere(10.0, 2.0, .4)
} (* end of [main0] *)

(* ****** ****** *)

```



## AutoHotkey

```ahk
#NoEnv
SetBatchLines, -1
#SingleInstance, Force

; Uncomment if Gdip.ahk is not in your standard library
#Include, Gdip.ahk

; Settings
X := 200, Y := 200, Width := 200, Height := 200 ; Location and size of sphere
rotation := -30 ; degrees
ARGB := 0xFFFF0000 ; Color=Solid Red

If !pToken := Gdip_Startup() ; Start gdi+
{
	MsgBox, 48, gdiplus error!, Gdiplus failed to start. Please ensure you have gdiplus on your system
	ExitApp
}
OnExit, Exit

Gui, -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs ; Create GUI
Gui, Show, NA ; Show GUI
hwnd1 := WinExist() ; Get a handle to this window we have created in order to update it later
hbm := CreateDIBSection(A_ScreenWidth, A_ScreenHeight) ; Create a gdi bitmap drawing area
hdc := CreateCompatibleDC() ; Get a device context compatible with the screen
obm := SelectObject(hdc, hbm) ; Select the bitmap into the device context
pGraphics := Gdip_GraphicsFromHDC(hdc) ; Get a pointer to the graphics of the bitmap, for use with drawing functions
Gdip_SetSmoothingMode(pGraphics, 4) ; Set the smoothing mode to antialias = 4 to make shapes appear smother

Gdip_TranslateWorldTransform(pGraphics, X, Y)
Gdip_RotateWorldTransform(pGraphics, rotation)

; Base ellipse
pBrush := Gdip_CreateLineBrushFromRect(0, 0, Width, Height, ARGB, 0xFF000000)
Gdip_FillEllipse(pGraphics, pBrush, 0, 0, Width, Height)

; First highlight ellipse
pBrush := Gdip_CreateLineBrushFromRect(Width*0.1, Height*0.01, Width*0.8, Height*0.6, 0x33FFFFFF, 0x00FFFFFF)
Gdip_FillEllipse(pGraphics, pBrush, Width*0.1, Height*0.01, Width*0.8, Height*0.6)

; Second highlight ellipse
pBrush := Gdip_CreateLineBrushFromRect(Width*0.3, Height*0.02, Width*0.3, Height*0.2, 0xBBFFFFFF, 0x00FFFFFF)
Gdip_FillEllipse(pGraphics, pBrush, Width*0.3, Height*0.02, Width*0.3, Height*0.2)


UpdateLayeredWindow(hwnd1, hdc, 0, 0, A_ScreenWidth, A_ScreenHeight)
SelectObject(hdc, obm) ; Select the object back into the hdc
Gdip_DeletePath(Path)
Gdip_DeleteBrush(pBrush)
DeleteObject(hbm) ; Now the bitmap may be deleted
DeleteDC(hdc) ; Also the device context related to the bitmap may be deleted
Gdip_DeleteGraphics(G) ; The graphics may now be deleted
Return

Exit:
; gdi+ may now be shutdown on exiting the program
Gdip_Shutdown(pToken)
ExitApp
```


## AWK


```AWK

# syntax: GAWK -f DRAW_A_SPHERE.AWK
# converted from VBSCRIPT
BEGIN {
    draw_sphere(20,4,0.1)
    draw_sphere(10,2,0.4)
    exit(0)
}
function draw_sphere(radius,k,ambient, b,i,intensity,j,leng_shades,light,line,shades,vec,x,y) {
    leng_shades = split0(".:!*oe&#%@",shades,"")
    split("30,30,-50",light,",")
    normalize(light)
    for (i=int(-radius); i<=ceil(radius); i++) {
      x = i + 0.5
      line = ""
      for (j=int(-2*radius); j<=ceil(2*radius); j++) {
        y = j / 2 + 0.5
        if (x*x + y*y <= radius*radius) {
          vec[1] = x
          vec[2] = y
          vec[3] = sqrt(radius*radius - x*x - y*y)
          normalize(vec)
          b = dot(light,vec) ^ k + ambient
          intensity = int((1-b) * leng_shades)
          if (intensity < 0) {
            intensity = 0
          }
          if (intensity >= leng_shades) {
            intensity = leng_shades
          }
          line = line shades[intensity]
        }
        else {
          line = line " "
        }
      }
      printf("%s\n",line)
    }
}
function ceil(x,  tmp) {
    tmp = int(x)
    return (tmp != x) ? tmp+1 : tmp
}
function dot(x,y,  tmp) {
    tmp = x[1]*y[1] + x[2]*y[2] + x[3]*y[3]
    return (tmp < 0) ? -tmp : 0
}
function normalize(v,  tmp) {
    tmp = sqrt(v[1]*v[1] + v[2]*v[2] + v[3]*v[3])
    v[1] /= tmp
    v[2] /= tmp
    v[3] /= tmp
}
function split0(str,array,fs,  arr,i,n) { # same as split except indices start at zero
    n = split(str,arr,fs)
    for (i=1; i<=n; i++) {
      array[i-1] = arr[i]
    }
    return(n)
}

```

```txt

                               ############%%%%%
                        #&&&eeeeeeeee&&&&&&#####%%%%%%%
                    &eeeoooooooooooooeeeee&&&&#####%%%%%%%%
                 &eoo***************oooooeeee&&&&####%%%%%%%%%
              &eo***!!!!!:::!!!!!!!****ooooeee&&&&#####%%%%%%%%%%
            eoo*!!!::::::::::::::!!!!****oooeeee&&&#####%%%%%%%%%%%
          eo**!!:::............::::!!!!***oooeeee&&&#####%%%%%%%%%%%%
        &eo*!!::.................:::!!!!***oooeee&&&&#####%%%%%%%%%%%%%
       eo*!!::...................::::!!!***oooeeee&&&#####%%%%%%%%%%%%%%
     #eo*!!::.....................:::!!!***oooeeee&&&&####%%%%%%%%%%%%%%%@
    &eo*!!::......................:::!!!***oooeeee&&&&#####%%%%%%%%%%%%%%%@
   &eo*!!::......................::::!!!***oooeeee&&&&#####%%%%%%%%%%%%%%%@@
  #eo**!!::......................:::!!!****oooeee&&&&#####%%%%%%%%%%%%%%%%%@@
  &eo**!:::....................::::!!!!***oooeeee&&&&#####%%%%%%%%%%%%%%%%%@@
 &eoo**!!::...................::::!!!!***ooooeee&&&&######%%%%%%%%%%%%%%%%%@@@
 &eoo**!!:::................::::!!!!****ooooeee&&&&&#####%%%%%%%%%%%%%%%%%%@@@
#&eoo**!!!::::...........:::::!!!!!****oooeeee&&&&&######%%%%%%%%%%%%%%%%%%@@@@
#&eeoo**!!!!::::::::::::::::!!!!!****ooooeeee&&&&&######%%%%%%%%%%%%%%%%%%%@@@@
#&eeooo***!!!!!::::::::!!!!!!!*****ooooeeeee&&&&&######%%%%%%%%%%%%%%%%%%%@@@@@
#&&eeooo****!!!!!!!!!!!!!!!******oooooeeee&&&&&#######%%%%%%%%%%%%%%%%%%%%@@@@@
##&&eeoooo********************oooooeeeee&&&&&&######%%%%%%%%%%%%%%%%%%%%%%@@@@@
##&&&eeeoooooo***********oooooooeeeeee&&&&&&#######%%%%%%%%%%%%%%%%%%%%%%@@@@@@
%##&&&eeeeeooooooooooooooooooeeeeeee&&&&&&#######%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@
%%###&&&&eeeeeeeeeeoeeeeeeeeeeee&&&&&&&&########%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@
 %%###&&&&&&eeeeeeeeeeeeeeee&&&&&&&&&#########%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@
 %%%#####&&&&&&&&&&&&&&&&&&&&&&&&##########%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@
  %%%%#######&&&&&&&&&&&&&&&############%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@
  %%%%%%#############################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@
   %%%%%%%%######################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@
    %%%%%%%%%%%%%%%#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@
       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@@@
          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@@@
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@@@@
              %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@@@@@@
                 @%%%%%%%%%%%%%%%%%%%%%%%%%%@@@@@@@@@@@@@@@@@@
                    @@@@%%%%%%%%%%%%@@@@@@@@@@@@@@@@@@@@@@@
                        @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                               @@@@@@@@@@@@@@@@@

             ::..::::!!**o
         .............::!!**oe
      ..................::!!*ooee
    .....................::!!*ooeee
   .......................:!!**ooeee
  ........................:!!**ooeeee
 ........................::!!**ooeeee&
:........................::!!**ooeeee&&
........................::!!**oooeeee&&
:......................::!!***ooeeeee&&
:....................::!!!***ooeeeee&&&
!::................:::!!***oooeeeeee&&&
*!!::..........::::!!!****oooeeeeee&&&&
 **!!::::::::::!!!!!***ooooeeeeee&&&&&
  o***!!!!!!!!!!*****ooooeeeeeee&&&&&
   eooo*********ooooooeeeeeeee&&&&&&
    eeeoooooooooooeeeeeeeeee&&&&&&&
      eeeeeeeeeeeeeeeeeee&&&&&&&&
         &eeeeeeeeee&&&&&&&&&&
             &&&&&&&&&&&&&


```



## BASIC


=
## BASIC256
=
This is modeled after the [http://rosettacode.org/wiki/Draw_a_sphere#Tcl Tcl] implementation. Thus, the output of this is almost the same to the output of Tcl implementation below.

```basic256
clg
color white
rect 0,0,graphwidth, graphheight
For n = 1 to 100
color rgb(2*n,2*n,2*n)
circle 150-2*n/3,150-n/2,150-n
next n
```


=
## BBC BASIC
=
Using Direct3D.

```bbcbasic
      MODE 8
      INSTALL @lib$+"D3DLIB"
      D3DTS_VIEW = 2
      D3DTS_PROJECTION = 3
      D3DRS_SPECULARENABLE = 29

      SYS "LoadLibrary", @lib$+"D3DX8BBC.DLL" TO d3dx%
      IF d3dx%=0 ERROR 100, "Couldn't load D3DX8BBC.DLL"
      SYS "GetProcAddress", d3dx%, "D3DXCreateSphere" TO `D3DXCreateSphere`
      SYS "GetProcAddress", d3dx%, "D3DXMatrixLookAtLH" TO `D3DXMatrixLookAtLH`
      SYS "GetProcAddress", d3dx%, "D3DXMatrixPerspectiveFovLH" TO `D3DXMatrixPerspectiveFovLH`

      DIM eyepos%(2), lookat%(2), up%(2), mat%(3,3)

      DIM D3Dlight8{Type%, Diffuse{r%,g%,b%,a%}, Specular{r%,g%,b%,a%}, \
      \ Ambient{r%,g%,b%,a%}, Position{x%,y%,z%}, Direction{x%,y%,z%}, \
      \ Range%, Falloff%, Attenuation0%, Attenuation1%, Attenuation2%, \
      \ Theta%, Phi%}

      DIM D3Dmaterial8{Diffuse{r%,g%,b%,a%}, Ambient{r%,g%,b%,a%}, \
      \ Specular{r%,g%,b%,a%}, Emissive{r%,g%,b%,a%}, Power%}

      DIM D3Dbasemesh8{QueryInterface%, Addref%, Release%, \
      \ DrawSubset%, GetNumFaces%, GetNumVertices%, GetFVF%, \
      \ GetDeclaration%, GetOptions%, GetDevice%, \
      \ CloneMeshFVF%, CloneMesh%, GetVertexBuffer%, GetIndexBuffer%, \
      \ LockVertexBuffer%, UnlockVertexBuffer%, LockIndexBuffer%, \
      \ UnlockIndexBuffer%, GetAttributeTable%}

      DIM D3Ddevice8{QueryInterface%, AddRef%, Release%, TestCooperativeLevel%, \
      \ GetAvailableTextureMem%, ResourceManagerDiscardBytes%, GetDirect3D%, \
      \ GetDeviceCaps%, GetDisplayMode%, GetCreationParameters%, SetCursorProperties%, \
      \ SetCursorPosition%, ShowCursor%, CreateAdditionalSwapChain%, Reset%, \
      \ Present%, GetBackBuffer%, GetRasterStatus%, SetGammaRamp%, GetGammaRamp%, \
      \ CreateTexture%, CreateVolumeTexture%, CreateCubeTexture%, CreateVertexBuffer%, \
      \ CreateIndexBuffer%, CreateRenderTarget%, CreateDepthStencilSurface%, \
      \ CreateImageSurface%, CopyRects%, UpdateTexture%, GetFrontBuffer%, \
      \ SetRenderTarget%, GetRenderTarget%, GetDepthStencilSurface%, BeginScene%, \
      \ EndScene%, Clear%, SetTransform%, GetTransform%, MultiplyTransform%, \
      \ SetViewport%, GetViewport%, SetMaterial%, GetMaterial%, SetLight%, GetLight%, \
      \ LightEnable%, GetLightEnable%, SetClipPlane%, GetClipPlane%, SetRenderState%, \
      \ GetRenderState%, BeginStateBlock%, EndStateBlock%, ApplyStateBlock%, \
      \ CaptureStateBlock%, DeleteStateBlock%, CreateStateBlock%, SetClipStatus%, \
      \ GetClipStatus%, GetTexture%, SetTexture%, GetTextureStageState%, \
      \ SetTextureStageState%, ValidateDevice%, GetInfo%, SetPaletteEntries%, \
      \ GetPaletteEntries%, SetCurrentTexturePalette%, GetCurrentTexturePalette%, \
      \ DrawPrimitive%, DrawIndexedPrimitive%, DrawPrimitiveUP%, \
      \ DrawIndexedPrimitiveUP%, ProcessVertices%, CreateVertexShader%, \
      \ SetVertexShader%, GetVertexShader%, DeleteVertexShader%, \
      \ SetVertexShaderConstant%, GetVertexShaderConstant%, GetVertexShaderDeclaration%, \
      \ GetVertexShaderFunction%, SetStreamSource%, GetStreamSource%, SetIndices%, \
      \ GetIndices%, CreatePixelShader%, SetPixelShader%, GetPixelShader%, \
      \ DeletePixelShader%, SetPixelShaderConstant%, GetPixelShaderConstant%, \
      \ GetPixelShaderFunction%, DrawRectPatch%, DrawTriPatch%, DeletePatch%}

      pDevice%=FN_initd3d(@hwnd%, 1, 1)
      IF pDevice%=0 ERROR 100, "Couldn't create Direct3D8 device"
      !(^D3Ddevice8{}+4) = !pDevice%

      SYS `D3DXCreateSphere`, pDevice%, FN_f4(1), 50, 50, ^meshSphere%, 0
      IF meshSphere% = 0 ERROR 100, "D3DXCreateSphere failed"
      !(^D3Dbasemesh8{}+4) = !meshSphere%

      REM. Point-source light:
      D3Dlight8.Type%=1 : REM. point source
      D3Dlight8.Diffuse.r%  = FN_f4(1)
      D3Dlight8.Diffuse.g%  = FN_f4(1)
      D3Dlight8.Diffuse.b%  = FN_f4(1)
      D3Dlight8.Specular.r% = FN_f4(1)
      D3Dlight8.Specular.g% = FN_f4(1)
      D3Dlight8.Specular.b% = FN_f4(1)
      D3Dlight8.Position.x% = FN_f4(2)
      D3Dlight8.Position.y% = FN_f4(1)
      D3Dlight8.Position.z% = FN_f4(4)
      D3Dlight8.Range%      = FN_f4(10)
      D3Dlight8.Attenuation0% = FN_f4(1)

      REM. Material:
      D3Dmaterial8.Diffuse.r%  = FN_f4(0.2)
      D3Dmaterial8.Diffuse.g%  = FN_f4(0.6)
      D3Dmaterial8.Diffuse.b%  = FN_f4(1.0)
      D3Dmaterial8.Specular.r% = FN_f4(0.4)
      D3Dmaterial8.Specular.g% = FN_f4(0.4)
      D3Dmaterial8.Specular.b% = FN_f4(0.4)
      D3Dmaterial8.Power%      = FN_f4(100)

      fovy = RAD(30)
      aspect = 5/4
      znear = 1
      zfar = 1000
      bkgnd% = &7F7F7F
      eyepos%() = 0, 0, FN_f4(6)
      lookat%() = 0, 0, 0
      up%() = 0, FN_f4(1), 0

      SYS D3Ddevice8.Clear%, pDevice%, 0, 0, 3, bkgnd%, FN_f4(1), 0
      SYS D3Ddevice8.BeginScene%, pDevice%
      SYS D3Ddevice8.SetLight%, pDevice%, 0, D3Dlight8{}
      SYS D3Ddevice8.LightEnable%, pDevice%, 0, 1
      SYS D3Ddevice8.SetMaterial%, pDevice%, D3Dmaterial8{}
      SYS D3Ddevice8.SetRenderState%, pDevice%, D3DRS_SPECULARENABLE, 1

      SYS `D3DXMatrixLookAtLH`, ^mat%(0,0), ^eyepos%(0), ^lookat%(0), ^up%(0)
      SYS D3Ddevice8.SetTransform%, pDevice%, D3DTS_VIEW, ^mat%(0,0)

      SYS `D3DXMatrixPerspectiveFovLH`, ^mat%(0,0), FN_f4(fovy), \
      \                                 FN_f4(aspect), FN_f4(znear), FN_f4(zfar)
      SYS D3Ddevice8.SetTransform%, pDevice%, D3DTS_PROJECTION, ^mat%(0,0)

      SYS D3Dbasemesh8.DrawSubset%, meshSphere%, 0
      SYS D3Ddevice8.EndScene%, pDevice%
      SYS D3Ddevice8.Present%, pDevice%, 0, 0, 0, 0

      SYS D3Ddevice8.Release%, pDevice%
      SYS D3Dbasemesh8.Release%, meshSphere%
      SYS "FreeLibrary", d3dx%
      END

```

[[File:Sphere_BBC.jpeg]]

===[[DarkBASIC]]===

Some simple 3D objects are built into DarkBASIC. Creating a sphere only takes 1 line:


```darkbasic
MAKE OBJECT SPHERE 1,1
```


=
## FreeBASIC
=

```FreeBASIC
' "\" = a integer division (CPU)
' "/" = a floating point division (FPU)
' the compiler takes care of the conversion between floating point and integer
' compile with: FBC -s console "filename.bas" or FBC -s GUI "filename.bas"
' filename is whatever name you give it, .bas is mandatory

' Sphere using XPL0 code from rosetacode sphere page
' Altered freebasic version to compile in default mode
' version 17-06-2015
' compile with: fbc -s console or fbc -s gui
#Define W 640
#Define H 480

ScreenRes W, H, 32   ' set 640x480x32 graphics mode, 32 bits color mode
WindowTitle "32 bpp Cyan Sphere FreeBASIC"

' wait until keypress
' Color(RGB(255,255,255),RGB(0,0,0))         ' default white foreground, black background
Locate 50,2
Print "Enter any key to start"
Sleep

Dim As UInteger R = 100, R2 = R * R          ' radius, in pixels; radius squared
Dim As UInteger X0 = W \ 2, Y0 = H \ 2       ' coordinates of center of screen
Dim As Integer X, Y, C, D2                   ' coords, color, distance from center squared

For Y = -R To R                              ' for all the coordinates near the circle
  For X = -R To R                            ' which is under the sphere
    D2 = X * X + Y * Y
    If D2 <= R2 Then                         ' coordinate is inside circle under sphere
                                             ' height of point on surface of sphere above X,Y
      C = Sqr(R2 - D2) - ( X + Y) / 2 + 130  ' color is proportional; offset X and Y, and

      Color C Shl 8 + C                      ' = color RGB(0, C, C)
                                             ' green + blue = cyan
      PSet(X + X0, Y + Y0)
    End If
  Next
Next

' wait until keypress
Locate 50,2
Color(RGB(255,255,255),RGB(0,0,0))           ' foreground color is changed
' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

needs #Lang "fblite", #Lang "qb" or #Lang "deprecated" to compile.

```FreeBASIC
'Sphere for FreeBASIC May 2015
'spherefb4.bas
'Sphere using XPL0 code from rosetacode sphere page
'
screenres 640,480,32   '\set 640x480x32 graphics mode
windowtitle "32 bpp Blue Sphere FreeBASIC"
'
' wait until keypress
locate 50,2
color(rgb(255,255,255),rgb(0,0,0))
Print "Enter any key to start"
sleep
 R=100 : R2=R*R	               '\radius, in pixels; radius squared
 X0=640/2 : Y0=480/2           '\coordinates of center of screen
 dim as integer X, Y, Z, C, D2 '\coords, color, distance from center squared
'
for Y= -R to +R                '\for all the coordinates near the circle
    for X = -R to +R          '\ which is under the sphere
        D2 = X*X + Y*Y        '
        C = 0                  '\default color is black
        if D2 <= R2 then       '\coordinate is inside circle under sphere
            Z = sqr(R2-D2)     '\height of point on surface of sphere above X,Y
            C = Z-(X+Y)/2+130  ' \color is proportional; offset X and Y, and
        endif
         color c                ' \ shift color to upper limit of its range
                                '\green + blue = cyan orginal line don't understand
         Pset(X+X0, Y+Y0)
    next x
  next y
'
' wait until keypress
locate 50,2
color(rgb(255,255,255),rgb(0,0,0))
Print "Enter any key to exit "
sleep
END
```


=
## Liberty BASIC
=

```lb

WindowWidth  =420
WindowHeight =460

nomainwin

open "Sphere" for graphics_nsb_nf as #w

#w "down ; fill lightgray"

xS =200
yS =200
for radius =150 to 0 step -1
    level$ =str$( int( 256 -256 *radius /150))
    c$ =level$ +" " +level$ +" " +level$
    #w "color ";     c$
    #w "backcolor "; c$
    #w "place "; xS; " "; yS
    xS =xS -0.5
    yS =yS -0.2
    #w "circlefilled "; radius
next radius

#w "flush"
wait
close #w
end

```


=
## Locomotive Basic
=
Translated from ERRE version, this will print a 39x20 text sphere onscreen.
The variables in line 80 can be used to adjust size (r), spotlight (k), reflective light (ambient).

```locobasic

10 MODE 2:s$=".:!*oe&#%@"
20 DIM v(2),vec(2)
30 v(0)=30:v(1)=30:v(2)=-50
40 lung=SQR(v(0)*v(0)+v(1)*v(1)+v(2)*v(2))
50 v(0)=v(0)/lung
60 v(1)=v(1)/lung
70 v(2)=v(2)/lung
80 r=10:k=2:ambient=0.4
90 FOR i=INT(-r) TO INT(r)
100 x=i+0.5
110 FOR j=INT(-2*r) TO INT(2*r)
120 y=j/2+0.5
130 IF (x*x+y*y<=r*r) THEN GOSUB 1000 ELSE PRINT" ";
140 NEXT j
150 PRINT
160 NEXT i
170 END
1000 vec(0)=x
1010 vec(1)=y
1020 vec(2)=SQR(r*r-x*x-y*y)
1030 GOSUB 2000
1040 GOSUB 3000
1050 b=d^k+ambient
1060 intensity%=(1-b)*(LEN(s$)-1)
1070 IF (intensity%<0) THEN intensity%=0
1080 IF (intensity%>LEN(s$)-1) THEN intensity%=LEN(s$)-2
1090 PRINT MID$(s$,intensity%+1,1);
1100 RETURN
2000 lung=SQR(vec(0)*vec(0)+vec(1)*vec(1)+vec(2)*vec(2))
2010 vec(0)=vec(0)/lung
2020 vec(1)=vec(1)/lung
2030 vec(2)=vec(2)/lung
2040 RETURN
3000 d=v(0)*vec(0)+v(1)*vec(1)+v(2)*vec(2)
3010 IF d<0 THEN d=-d ELSE d=0
3020 RETURN

```


=
## PureBasic
=
3D Sphere animation.

```PureBasic
; Original by Comtois @ 28/03/06
;
; Updated/Formated by Fluid Byte @ March.24,2009
;
; http://www.purebasic.fr/english/viewtopic.php?p=281258#p281258

Declare CreateSphere(M,P)
Declare UpdateMesh()

#_SIZEVERT = 36
#_SIZETRIS = 6
#FULLSCREEN = 0

Structure VECTOR
  X.f
  Y.f
  Z.f
EndStructure

Structure VERTEX
  X.f
  Y.f
  Z.f
  NX.f
  NY.f
  NZ.f
  Color.l
  U.f
  V.f
EndStructure

Structure TRIANGLE
  V1.w
  V2.w
  V3.w
EndStructure

Macro CALC_NORMALS
  *PtrV\NX = *PtrV\X
  *PtrV\NY = *PtrV\Y
  *PtrV\NZ = *PtrV\Z
EndMacro

Global *VBuffer, *IBuffer
Global Meridian = 50, Parallele = 50, PasLength = 4, Length

Define EventID, i, NbSommet, CameraMode, Angle.f, Pas.f = 0.5

InitEngine3D() : InitSprite() : InitKeyboard()

Add3DArchive(GetTemporaryDirectory(),#PB_3DArchive_FileSystem)
Add3DArchive(#PB_Compiler_Home + "Examples\Sources\Data\",#PB_3DArchive_FileSystem)

If #FULLSCREEN
  OpenScreen(800,600,32,"Sphere 3D")
Else
  OpenWindow(0,0,0,800,600,"Sphere 3D",#PB_Window_SystemMenu | 1)
  OpenWindowedScreen(WindowID(0),0,0,800,600,0,0,0)
EndIf

;-Texture
CreateImage(0,128,128)
StartDrawing(ImageOutput(0))
For i = 0 To 127 Step 4
  Box(0,i,ImageWidth(0),2,RGB(255,255,255))
  Box(0,i + 2,ImageWidth(0),2,RGB(0,0,155))
Next i
StopDrawing()
SaveImage(0,GetTemporaryDirectory() + "temp.bmp") : FreeImage(0)

;-Material
CreateMaterial(0,LoadTexture(0,"temp.bmp"))
RotateMaterial(0,0.1,#PB_Material_Animated)

;-Mesh
CreateSphere(Meridian,Parallele)

;-Entity
CreateEntity(0,MeshID(0),MaterialID(0))
ScaleEntity(0,60,60,60)

;-Camera
CreateCamera(0,0,0,100,100)
MoveCamera(0,0,0,-200)
CameraLookAt(0,EntityX(0),EntityY(0),EntityZ(0))

;-Light
AmbientColor(RGB(105, 105, 105))
CreateLight(0, RGB(255, 255,  55), EntityX(0) + 150, EntityY(0)      , EntityZ(0))
CreateLight(1, RGB( 55, 255, 255), EntityX(0) - 150, EntityY(0)      , EntityZ(0))
CreateLight(2, RGB( 55,  55, 255), EntityX(0)      , EntityY(0) + 150, EntityZ(0))
CreateLight(3, RGB(255,  55, 255), EntityX(0)      , EntityY(0) - 150, EntityZ(0))

; ----------------------------------------------------------------------------------------------------
; MAINLOOP
; ----------------------------------------------------------------------------------------------------

Repeat
  If #FULLSCREEN = 0
    Repeat
      EventID = WindowEvent()

      Select EventID
        Case #PB_Event_CloseWindow : End
      EndSelect
    Until EventID = 0
  EndIf

  Angle + Pas
  RotateEntity(0, Angle, Angle,Angle)

  If PasLength > 0 : UpdateMesh() : EndIf

  If ExamineKeyboard()
    If KeyboardReleased(#PB_Key_F1)
      CameraMode = 1 - CameraMode
      CameraRenderMode(0, CameraMode)
    EndIf
  EndIf

  RenderWorld()
  FlipBuffers()
Until KeyboardPushed(#PB_Key_Escape)

; ----------------------------------------------------------------------------------------------------
; FUNCTIONS
; ----------------------------------------------------------------------------------------------------

Procedure CreateSphere(M,P)
  ; M = Meridian
  ; P = Parallele
  ; The radius is 1. Front to remove it later, it's just for the demo.

  If M < 3 Or P < 2  : ProcedureReturn 0 : EndIf

  Protected Normale.VECTOR, NbSommet, i, j, Theta.f, cTheta.f, sTheta.f
  Protected Alpha.f, cAlpha.f, sAlpha.f, *PtrV.VERTEX, *PtrF.TRIANGLE, NbTriangle

  NbSommet = 2 + ((M + 1) * P)
  *VBuffer = AllocateMemory(#_SIZEVERT * Nbsommet)

  For i = 0 To M
    Theta  = i * #PI * 2.0 / M
    cTheta = Cos(theta)
    sTheta = Sin(theta)

    For j = 1 To P
      Alpha  = j * #PI / (P + 1)
      cAlpha = Cos(Alpha)
      sAlpha = Sin(Alpha)
      *PtrV = *VBuffer + #_SIZEVERT * ((i * P) + (j - 1))
      *PtrV\X = sAlpha * cTheta
      *PtrV\Y = sAlpha * sTheta
      *PtrV\Z = cAlpha
      *PtrV\U  = Theta / (2.0 * #PI)
      *PtrV\V  = Alpha / #PI
      CALC_NORMALS
    Next j
  Next i

  ; Southpole
  *PtrV = *VBuffer + #_SIZEVERT * ((M + 1) * P)
  *PtrV\X =  0
  *PtrV\Y =  0
  *PtrV\Z = -1
  *PtrV\U =  0
  *PtrV\V =  0
  CALC_NORMALS

  ; Northpole
  *PtrV + #_SIZEVERT
  *PtrV\X = 0
  *PtrV\Y = 0
  *PtrV\Z = 1
  *PtrV\U = 0
  *PtrV\V = 0
  CALC_NORMALS

  ; Les facettes
  NbTriangle = 4 * M * P
  *IBuffer = AllocateMemory(#_SIZETRIS * NbTriangle)
  *PtrF = *IBuffer

  For i = 0 To M - 1
    For j = 1 To P - 1
      *PtrF\V1 = ((i + 1) * P) + j
      *PtrF\V2 = ((i + 1) * P) + (j - 1)
      *PtrF\V3 = (i * P) + (j - 1)
      *PtrF + #_SIZETRIS
      *PtrF\V3 = ((i + 1) * P) + j        ;Recto
      *PtrF\V2 = ((i + 1) * P) + (j - 1)  ;Recto
      *PtrF\V1 = (i * P) + (j - 1)        ;Recto
      *PtrF + #_SIZETRIS
      *PtrF\V1 = i * P + j
      *PtrF\V2 = ((i + 1) * P) + j
      *PtrF\V3 = (i * P) + (j - 1)
      *PtrF + #_SIZETRIS
      *PtrF\V3 = i * P + j               ;Recto
      *PtrF\V2 = ((i + 1) * P) + j       ;Recto
      *PtrF\V1 = (i * P) + (j - 1)       ;Recto
      *PtrF + #_SIZETRIS
    Next j
  Next i

  ; The Poles
  For i = 0 To M - 1
    *PtrF\V3 = (M + 1) * P + 1
    *PtrF\V2 = (i + 1) * P
    *PtrF\V1 = i * P
    *PtrF + #_SIZETRIS
    *PtrF\V1 = (M + 1) * P + 1   ;Recto
    *PtrF\V2 = (i + 1) * P       ;Recto
    *PtrF\V3 = i * P             ;Recto
    *PtrF + #_SIZETRIS
  Next i

  For i = 0 To M - 1
    *PtrF\V3 = (M + 1) * P
    *PtrF\V2 = i * P + (P - 1)
    *PtrF\V1 = (i + 1) * P + (P - 1)
    *PtrF + #_SIZETRIS
    *PtrF\V1 = (M + 1) * P              ;Recto
    *PtrF\V2 = i * P + (P - 1)          ;Recto
    *PtrF\V3 = (i + 1) * P + (P - 1)    ;Recto
    *PtrF + #_SIZETRIS
  Next i

  If CreateMesh(0,100)
    Protected Flag = #PB_Mesh_Vertex | #PB_Mesh_Normal | #PB_Mesh_UVCoordinate | #PB_Mesh_Color
    SetMeshData(0,Flag,*VBuffer,NbSommet)
    SetMeshData(0,#PB_Mesh_Face,*IBuffer,NbTriangle)
    ProcedureReturn 1
  EndIf

  ProcedureReturn 0
EndProcedure

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Procedure UpdateMesh()
  Protected NbTriangle = 4 * Meridian * Parallele

  Length + PasLength

  If Length >= NbTriangle
    PasLength = 0
    Length = Nbtriangle
  EndIf

  SetMeshData(0,#PB_Mesh_Face,*IBuffer,Length)
EndProcedure
```

[[image:PB_Animated_sphere.png]]

===[[QBasic]]===


```QBASIC
SCREEN 13 ' enter high-color graphic mode

' sets palette colors B/N
FOR i = 0 TO 255
 PALETTE 255 - i, INT(i / 4) + INT(i / 4) * 256 + INT(i / 4) * 65536
NEXT i
PALETTE 0, 0

' draw the sphere
FOR i = 255 TO 0 STEP -1
 x = 50 + i / 3
 y = 99
 CIRCLE (x, y), i / 3, i
 PAINT (x, y), i
NEXT i

' wait until keypress
DO: LOOP WHILE INKEY$ = ""
END
```


=
## Run BASIC
=

```runbasic
'Run BASIC White Sphere, Black background
'runbasic.com
graphic #win, 300, 300
#win size(1)
 R=100
 R2=R*R
 X0=300/2
 Y0=300/2
for Y = -150 to 150
for X = -150 to 150
 D2 = X*X + Y*Y
 C = 0
 if D2 <= R2 then Z = sqr(R2-D2) : C = int(Z-(X+Y)/2+130)
 #win color(C,C,C)
 #win set(X+X0, Y+Y0)
next X
next Y
render #win
```



```Runbasic
'This is a simple Circle
graphic #g, 300, 300 	'create a graphic object
#g place(100,100) 	'place the drawing pen at 100,100
#g circle(75) 		'make a circle with radius 75
render #g 		'show it
```


=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM. A screenshot of the output is [http://www.edmundgriffiths.com/zx81sphere.jpg here].

```basic
10 LET I=21
20 LET J=2
30 FOR K=-PI TO PI STEP 0.07
40 PLOT 21+I*SIN K,22+21*COS K
50 PLOT 21+21*SIN K,22+(I-1)*COS K
60 NEXT K
70 LET I=I-J
80 LET J=J+1
90 IF I>0 THEN GOTO 30
```



## Batch File

In my console the sphere looked more or less spheric, but this site has a larger interval between lines, so the result looks more like an egg.  The code of this sample is not ported from the C sample. Integer square root by Aacini.

```dos
@echo off
setlocal enabledelayedexpansion
mode con cols=80

set /a r=220,cent=340,r2=r/2
set "spaces=                                   "
set "block1=MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM"
set "block2=#########"
set "block3=XXXXXXXXX"
set "block4=ooooooooo"
set "block5=?????????"
set "block6=*********"
set "block7=~~~~~~~~~"
set "block8=---------"

set wy=0
set linea=
echo                           Batch-File ASCII Ball
echo.
for /L %%y in (-%r%,10,%r%) do (
   set /a "w1=r*r-%%y*%%y"
   call:sqrt2 w1 w1
   set /a "w1=14*w1/10,wy=(cent-w1),cnt=0,sp=wy/10,centre=cent/10-sp"
   call set "linea=%%spaces:~0,!sp!%%%%block1:~0,!centre!%%
   set /a wy=0,sum=0
   for %%i in (30 80 120 150 170 185 195 200) do (
        set /a "cnt+=1,wy2=(%%i+r2)*w1/r,ww=(wy2+5)/10-sum,wy=wy2,sum+=ww"
        call set miblock=%%block!cnt!%%
        call set "Linea=%%linea%%%%miblock:~0,!ww!%%"
   )
   call echo(!linea!
)
echo.
exit /b

:sqrt2   [num] calculates integer square root . By AAcini
set "s=!%~1!"
set /A "x=s/(11*1024)+40,x=(s/x+x)>>1,x=(s/x+x)>>1,x=(s/x+x)>>1,x=(s/x+x)>>1,x=(s/x+x)>>1,x+=(s-x*x)>>31
set %~2=%x%
exit /b
```

```txt

                          Batch-File ASCII Ball


                        MMMMMMMMMMMMMMMM##XXo?~
                     MMMMMMMMMMMMMMMMMMMMM###XXoo?*~
                  MMMMMMMMMMMMMMMMMMMMMMMMMM###XXXoo??*-
                MMMMMMMMMMMMMMMMMMMMMMMMMMMMM####XXXooo?**-
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#####XXXooo??*~
             MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#####XXXXooo??*~-
           MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#####XXXXooo???*~-
          MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#####XXXXXooo??**~
         MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#####XXXXXooo??**~-
        MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM######XXXXXooo???*~~
       MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM######XXXXXooo???**~
      MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXooo???**~
      MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM######XXXXXoooo???**~
     MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXoooo??**~~
    MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM######XXXXXoooo???**~-
    MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM######XXXXXXoooo???**~-
    MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXoooo???**~-
   MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXXoooo??**~~-
   MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXXoooo???**~-
   MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXXoooo???**~-
   MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXXoooo???**~-
   MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXoooo???**~~
   MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXXoooo???**~-
   MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXXoooo???**~-
   MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXXoooo???**~-
   MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXXoooo??**~~-
    MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXoooo???**~-
    MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM######XXXXXXoooo???**~-
    MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM######XXXXXoooo???**~-
     MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXoooo??**~~
      MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM######XXXXXoooo???**~
      MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#######XXXXXooo???**~
       MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM######XXXXXooo???**~
        MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM######XXXXXooo???*~~
         MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#####XXXXXooo??**~-
          MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#####XXXXXooo??**~
           MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#####XXXXooo???*~-
             MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#####XXXXooo??*~-
              MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM#####XXXooo??*~
                MMMMMMMMMMMMMMMMMMMMMMMMMMMMM####XXXooo?**-
                  MMMMMMMMMMMMMMMMMMMMMMMMMM###XXXoo??*-
                     MMMMMMMMMMMMMMMMMMMMM###XXoo?*~
                        MMMMMMMMMMMMMMMM##XXo?~


```



## Befunge

While based on the C implementation, the algorithm has been considerably simplified to try and avoid floating point (which Befunge doesn't support) and minimise the need for sqrt calculations (which we approximate using the Babylonian method).

The first four values on the stack define the radius (45* = 20) and the light vector (65*65*"2" = 30;30;50). The ''k'' parameter has been hardcoded to 2, and the ambient light is approximated by adjusting the shade characters (defined on the last line).

Also note that the z-coordinate of the light vector is negated at runtime to more closely match the C defaults. This is preferable to making the initial constant negative since negative data values aren't supported across all Befunge implementations.


```befunge
45*65*65*"2"30p20p10p::00p2*40p4*5vv<
>60p140g->:::*00g50g*60g40g-:*-\-v0>1
^_@#`\g0<|`\g04:+1, <*84$$_v#`\0:<>p^
>v>g2+:5^$>g:*++*/7g^>*:9$#<"~"/:"~"v
g:^06,+55<^03*<v09p07%"~"p09/"~"p08%<
^>#0 *#12#<0g:^>+::"~~"90g*80g+*70gv|
g-10g*+:9**00gv|!*`\2\`-20::/2-\/\+<>
%#&eo*!:..^g05<>$030g-*9/\20g*+60g40^
```


```txt
                               eeeeeeeeeee&&&&&#
                        eooo********oooooooeeee&&&&###%
                    oo****!!!!!!!!********ooooeeeee&&&###%%
                 o**!!!!!!!!!!!!!!!!!!!*****oooooeeee&&&####%%
              o*!!!!:::::::::::::::!!!!!!*****ooooeeee&&&&####%%%
            **!!:::::::....::::::::::!!!!!!****ooooeeeee&&&####%%%%
          **!!::::.............::::::::!!!!!****ooooeeee&&&&####%%%%%
        o*!!::::.................::::::!!!!!!****ooooeeee&&&&####%%%%%%
       *!!::::....................::::::!!!!!****ooooeeeee&&&&####%%%%%%
     e*!!:::......................::::::!!!!!*****ooooeeee&&&&####%%%%%%%%
    o*!!:::.......................::::::!!!!!*****ooooeeee&&&&#####%%%%%%%%
   o*!!::::.......................::::::!!!!!****oooooeeee&&&&#####%%%%%%%%%
  e**!!:::.......................::::::!!!!!*****ooooeeeee&&&&#####%%%%%%%%%%
  o*!!!:::......................:::::::!!!!!*****ooooeeeee&&&&#####%%%%%%%%%%
 o**!!!::::...................::::::::!!!!!*****oooooeeee&&&&&#####%%%%%%%%%%%
 o**!!!:::::................::::::::!!!!!!*****oooooeeeee&&&&#####%%%%%%%%%%%%
eo**!!!:::::::............:::::::::!!!!!!*****oooooeeeee&&&&&#####%%%%%%%%%%%%%
eo***!!!:::::::::::::::::::::::::!!!!!!******oooooeeeee&&&&&#####%%%%%%%%%%%%%%
eoo**!!!!!::::::::::::::::::::!!!!!!!!******oooooeeeee&&&&&######%%%%%%%%%%%%%%
eoo***!!!!!!:::::::::::::::!!!!!!!!!******ooooooeeeee&&&&&######%%%%%%%%%%%%%%%
eeoo****!!!!!!!!!!!!!!!!!!!!!!!!!*******ooooooeeeeee&&&&&######%%%%%%%%%%%%%%%%
&eooo*****!!!!!!!!!!!!!!!!!!!!********ooooooeeeeee&&&&&&######%%%%%%%%%%%%%%%%%
&eeoooo*******!!!!!!!!!!***********oooooooeeeeeee&&&&&&######%%%%%%%%%%%%%%%%%#
&&eeeoooo***********************ooooooooeeeeeee&&&&&&#######%%%%%%%%%%%%%%%%%%#
 &&eeeooooooo**************ooooooooooeeeeeeee&&&&&&#######%%%%%%%%%%%%%%%%%%%#
 #&&eeeeeoooooooooooooooooooooooooeeeeeeee&&&&&&&########%%%%%%%%%%%%%%%%%%%##
  #&&&eeeeeeeooooooooooooooooeeeeeeeeee&&&&&&&&########%%%%%%%%%%%%%%%%%%%%##
  %##&&&&eeeeeeeeeeeeeeeeeeeeeeeeee&&&&&&&&&&########%%%%%%%%%%%%%%%%%%%%%##&
   %###&&&&&&eeeeeeeeeeeeeeeeee&&&&&&&&&&&#########%%%%%%%%%%%%%%%%%%%%%%##&
    %####&&&&&&&&&&&&&&&&&&&&&&&&&&&&&##########%%%%%%%%%%%%%%%%%%%%%%%%##&
     %%######&&&&&&&&&&&&&&&&&&&&############%%%%%%%%%%%%%%%%%%%%%%%%%%##e
       %%%################################%%%%%%%%%%%%%%%%%%%%%%%%%%%##&
        %%%%%########################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###&
          %%%%%%%%%###########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###&
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###&
              %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##&&
                 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%###&
                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####&
                        %%%%%%%%%%%%%%%%%%%%%%%%%%####&
                               %%%%%%%%%%%%#####
```



## Brlcad



```brlcad
opendb balls.g y            # Create a database to hold our shapes
units cm                     # Set the unit of measure
in ball.s sph 0 0 0 3  # Create a sphere of radius 3 cm named ball.s with its centre at 0,0,0
```



## C

The lighting calculation is somewhere between crude and bogus, but hey, I'm shading it with ASCII characters, don't expect too much.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

const char *shades = ".:!*oe&#%@";

double light[3] = { 30, 30, -50 };
void normalize(double * v)
{
        double len = sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
        v[0] /= len; v[1] /= len; v[2] /= len;
}

double dot(double *x, double *y)
{
        double d = x[0]*y[0] + x[1]*y[1] + x[2]*y[2];
        return d < 0 ? -d : 0;
}

void draw_sphere(double R, double k, double ambient)
{
        int i, j, intensity;
        double b;
        double vec[3], x, y;
        for (i = floor(-R); i <= ceil(R); i++) {
                x = i + .5;
                for (j = floor(-2 * R); j <= ceil(2 * R); j++) {
                        y = j / 2. + .5;
                        if (x * x + y * y <= R * R) {
                                vec[0] = x;
                                vec[1] = y;
                                vec[2] = sqrt(R * R - x * x - y * y);
                                normalize(vec);
                                b = pow(dot(light, vec), k) + ambient;
                                intensity = (1 - b) * (sizeof(shades) - 1);
                                if (intensity < 0) intensity = 0;
                                if (intensity >= sizeof(shades) - 1)
                                        intensity = sizeof(shades) - 2;
                                putchar(shades[intensity]);
                        } else
                                putchar(' ');
                }
                putchar('\n');
        }
}


int main()
{
        normalize(light);
        draw_sphere(20, 4, .1);
        draw_sphere(10, 2, .4);

        return 0;
}
```

```txt
                               #############%%%%
                       ##&&eeeeeeeeee&&&&&&&####%%%%%%%%
                   &&eeooooooooooooooeeeee&&&&######%%%%%%%%
                 eeoo**************oooooooeeee&&&&####%%%%%%%%
             &&oo**!!!!!!::!!!!!!!!****oooooee&&&&######%%%%%%%%%%
           eeoo!!!!::::::::::::::!!!!*****ooeeee&&&&####%%%%%%%%%%%%
         ee**!!::::............::::!!!!***ooooeeee&&######%%%%%%%%%%%%
       &&oo!!::..................::!!!!*****ooeeee&&&&####%%%%%%%%%%%%%%
       oo!!::....................::::!!*****ooeeee&&&&####%%%%%%%%%%%%%%
     ee**!!::....................::::!!*****ooeeee&&&&####%%%%%%%%%%%%%%%%
   &&oo!!::......................::::!!*****ooeeee&&&&######%%%%%%%%%%%%%%%%
   ee**!!::......................::::!!*****ooeeee&&&&######%%%%%%%%%%%%%%%%
 ##oo**!!::......................::!!!!*****ooeeee&&&&####%%%%%%%%%%%%%%%%%%%%
 &&oo**::::....................::::!!!!***ooooeeee&&&&####%%%%%%%%%%%%%%%%%%%%
 eeoo**!!::..................::::!!!!*****ooooee&&&&######%%%%%%%%%%%%%%%%%%%%
 eeoo**!!::................::::!!!!****oooooeeee&&&&######%%%%%%%%%%%%%%%%%%%%
#eeoo**!!::::............::::!!!!!!****oooeeee&&&&&&######%%%%%%%%%%%%%%%%%%%%%%
#eeoo**!!!!::::::::::::::::!!!!!!****oooooeeee&&&&######%%%%%%%%%%%%%%%%%%%%%%%%
#eeoooo**!!!!!!::::::::!!!!!!******ooooeeeee&&&&&&######%%%%%%%%%%%%%%%%%%%%%%%%
#&&eeoo****!!!!!!!!!!!!!!!!******ooooeeeee&&&&&&######%%%%%%%%%%%%%%%%%%%%%%%%%%
#&&eeoooo********************ooooooeeee&&&&&&&######%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#&&&&eeoooooo************ooooooeeeeee&&&&&&&########%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%##&&eeeeeeooooooooooooooooooeeeeee&&&&&&&########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%####&&&&eeeeeeeeeeeeeeeeeeeeee&&&&&&&&#########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%##&&&&&&eeeeeeeeeeeeeeee&&&&&&&&&&#########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%######&&&&&&&&&&&&&&&&&&&&&&&&###########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%########&&&&&&&&&&&&&&############%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%%%%%##############################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%%%%%%%######################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%%%%%%%%%%%%%%%####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                               %%%%%%%%%%%%%%%%%

             ::..:::::!!**
         .............::!!**oo
     ...................::!!**ooee
   .......................::!!ooeeee
   .......................::!!**ooee
 .........................::!!**ooeeee
 .........................::!!**ooeeee
:.........................::!!**ooeeeeee
........................::!!**ooooeeeeee
:.......................::!!**ooeeeeeeee
:.....................::!!****ooeeeeeeee
!::................:::!!****ooeeeeeeeeee
*!!::..........::::!!!****ooooeeeeeeeeee
 **!!::::::::::!!!!*****ooooeeeeeeeeee
 oo**!!!!!!!!!!*******ooooeeeeeeeeeeee
   oooo********oooooooeeeeeeeeeeeeee
   eeeeooooooooooeeeeeeeeeeeeeeeeeee
     eeeeeeeeeeeeeeeeeeeeeeeeeeeee
         eeeeeeeeeeeeeeeeeeeee
             eeeeeeeeeeeee
```



### Fun with 3D noise texture

[[file:sphere-perlin.png]]

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAXD 8
int g[] = { -1, 1, -1, 1 };
/* Perlin-like noise */
inline void
hashed(int *data, int *out, int len) {
#	define ror(a, d) ((a << (d)) | (a >> (32 - d)))
	register unsigned int h = 0x12345678, tmp;
	unsigned int *d = (void*)data;
	int i = len;

	while (i--) {
		tmp = *d++;
		h += ror(h, 15) ^ ror(tmp, 5);
	}

	h ^= ror(h, 7);
	h += ror(h, 23);
	h ^= ror(h, 19);
	h += ror(h, 11);
	h ^= ror(h, 13);
	h += ror(h, 17);
#	undef ror
	for (i = len; i--; ) {
		out[i] = g[h & 3];
		h >>= 2;
	}
}

double scale[MAXD], scale_u[MAXD];
void noise_init()
{
	int i;
	for (i = 1; i < MAXD; i++) {
		scale[i] = 1 / (1 + sqrt(i + 1));
		scale_u[i] = scale[i] / sqrt(i + 1);
	}
}

double noise(double *x, int d)
{
#	define sum(s, x) for (s = 0, j = 0; j < d; j++) s += x
	register int i, j;
	int n[MAXD], o[MAXD], g[MAXD], tmp;
	double s, r, t, w, ret, u[MAXD];

	sum(s, x[j]);
	s *= scale[d];

	for (i = 0; i < d; i++) {
		o[i] = i;
		t = x[i] + s;
		u[i] = t - (n[i] = floor(t));
	}
	o[d] = 0;

	for (i = 0; i < d - 1; i++)
		for (j = i; j < d; j++)
			if (u[o[i]] < u[o[j]])
				tmp = o[i], o[i] = o[j], o[j] = tmp;

	ret = w = 0, r = 1;
	for (s = 0, j = 0; j < d; j++) s += n[j];
	s *= scale_u[d];

	for (i = 0; i <= d; i++) {
		for (j = 0; j < d; j++)
			u[j] = x[j] + s - n[j];

		for (t = (d + 1.) / (2 * d), j = 0; j < d; j++) {
			t -= u[j] * u[j];
			if (t <= 0) break;
		}

		if (t >= 0) {
			r = 0;
			hashed(n, g, d);
			for (j = 0; j < d; j++)
				if (g[j]) r += (g[j] == 1 ? u[j] : -u[j]);
			t *= t;
			ret += r * t * t;
		}

		if (i < d) {
			n[o[i]]++;
			s += scale_u[d];
		}
	}
	return ret * (d * d);
}

double get_noise2(double x, double y)
{
	int i, ws;
	double r = 0, v[2];

	for (i = 1, ws = 0; i <= 128; i <<= 1) {
		v[0] = x * i, v[1] = y * i;
		r += noise(v, 2);
		ws ++;
	}
	r /= ws;
	return r;
}

double get_noise3(double x, double y, double z)
{
	int i, ws;
	double r = 0, v[3], w;

	for (i = 1, ws = 0; i <= 32; i <<= 1) {
		v[0] = x * i, v[1] = y * i, v[2] = z * i;
		w = 1./sqrt(i);
		r += noise(v, 3) * w;
		ws += w;
	}
	return r / ws;
}


int main(int c, char** v)
{
	unsigned char pix[256 * 256], *p;
	int i, j;
	double x, y, z, w;
	FILE *fp;

	noise_init();

	for (p = pix, i = 0; i < 256 * 256; i++) *p++ = 0;

	for (p = pix, i = 0; i < 256; i++) {
		y = (i - 128) / 125.;
		for (j = 0; j < 256; j++, p++) {
			x = (j - 128) / 125.;
			*p = (get_noise2(i/256., j/256.) + 1) / 6 * i;

			z = 1- x*x - y*y;
			if (z < 0) continue;

			z = sqrt(z);

			w = get_noise3(x, y, z);

			w = (w + 1) / 2;
			w *= (1 + x - y + z) / 3.5;
			if (w < 0) w = 0;

			*p = w * 255;
		}
	}

	fp = fopen("out.pgm", "w+");
	fprintf(fp, "P5\n256 256\n255\n");
	fwrite(pix, 1, 256 * 256, fp);
	fclose(fp);

	return 0;
}
```


## C#
```java
using System;

namespace Sphere {
    internal class Program {
        private const string Shades = ".:!*oe%&#@";
        private static readonly double[] Light = {30, 30, -50};

        private static void Normalize(double[] v) {
            double len = Math.Sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
            v[0] /= len;
            v[1] /= len;
            v[2] /= len;
        }

        private static double Dot(double[] x, double[] y) {
            double d = x[0]*y[0] + x[1]*y[1] + x[2]*y[2];
            return d < 0 ? -d : 0;
        }

        public static void DrawSphere(double r, double k, double ambient) {
            var vec = new double[3];
            for(var i = (int)Math.Floor(-r); i <= (int)Math.Ceiling(r); i++) {
                double x = i + .5;
                for(var j = (int)Math.Floor(-2*r); j <= (int)Math.Ceiling(2*r); j++) {
                    double y = j/2.0 + .5;
                    if(x*x + y*y <= r*r) {
                        vec[0] = x;
                        vec[1] = y;
                        vec[2] = Math.Sqrt(r*r - x*x - y*y);
                        Normalize(vec);
                        double b = Math.Pow(Dot(Light, vec), k) + ambient;
                        int intensity = (b <= 0)
                                            ? Shades.Length - 2
                                            : (int)Math.Max((1 - b)*(Shades.Length - 1), 0);
                        Console.Write(Shades[intensity]);
                    }
                    else
                        Console.Write(' ');
                }
                Console.WriteLine();
            }
        }

        private static void Main() {
            Normalize(Light);
            DrawSphere(6, 4, .1);
            DrawSphere(10, 2, .4);
            Console.ReadKey();
        }
    }
}
```



## Clojure

```clojure

(use 'quil.core)

(def w 500)
(def h 400)

(defn setup []
  (background 0))

(defn draw []
  (push-matrix)
  (translate 250 200 0)
  (sphere 100)
  (pop-matrix))

(defsketch main
  :title "sphere"
  :setup setup
  :size [w h]
  :draw draw
  :renderer :opengl)

```


[http://i.imgur.com/fkzH5wM.png]


## D

```d
import std.stdio, std.math, std.algorithm, std.numeric;

alias V3 = double[3];
immutable light = normalize([30.0, 30.0, -50.0]);

V3 normalize(V3 v) pure @nogc {
    v[] /= dotProduct(v, v) ^^ 0.5;
    return v;
}

double dot(in ref V3 x, in ref V3 y) pure nothrow @nogc {
    immutable double d = dotProduct(x, y);
    return d < 0 ? -d : 0;
}

void drawSphere(in double R, in double k, in double ambient) @nogc {
    enum shades = ".:!*oe&#%@";
    foreach (immutable i; cast(int)floor(-R) .. cast(int)ceil(R) + 1) {
        immutable double x = i + 0.5;
        foreach (immutable j; cast(int)floor(-2 * R) ..
                              cast(int)ceil(2 * R) + 1) {
            immutable double y = j / 2. + 0.5;
            if (x ^^ 2 + y ^^ 2 <= R ^^ 2) {
                immutable vec = [x, y, (R^^2 - x^^2 - y^^2) ^^ 0.5]
                                .normalize;
                immutable double b = dot(light, vec) ^^ k + ambient;
                int intensity = cast(int)((1 - b) * (shades.length-1));
                intensity = min(shades.length - 1, max(intensity, 0));
                shades[intensity].putchar;
            } else
                ' '.putchar;
        }
        '\n'.putchar;
    }
}

void main() {
    drawSphere(20, 4, 0.1);
    drawSphere(10, 2, 0.4);
}
```



## Delphi

'''Under Microsoft Windows:'''
If you notice the big sphere loses its roundness, then try increasing the width of the Windows console. By default it’s 80; so put it to something bigger, let’s say 90.

'''Steps:''' Run the CMD Windows shell. Then follow this path to setup the new width: '''Main Menu-> Properties -> Layout -> Window Size -> Width'''.


```Delphi

program DrawASphere;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Math;

type
  TDouble3  = array[0..2] of Double;
  TChar10 = array[0..9] of Char;

var
  shades: TChar10 = ('.', ':', '!', '*', 'o', 'e', '&', '#', '%', '@');
  light: TDouble3 = (30, 30, -50 );

  procedure normalize(var v: TDouble3);
  var
    len: Double;
  begin
    len:= sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
    v[0] := v[0] / len;
    v[1] := v[1] / len;
    v[2] := v[2] / len;
  end;

  function dot(x, y: TDouble3): Double;
  begin
    Result:= x[0]*y[0] + x[1]*y[1] + x[2]*y[2];
    Result:= IfThen(Result < 0, -Result, 0 );
  end;

  procedure drawSphere(R, k, ambient: Double);
  var
    vec: TDouble3;
    x, y, b: Double;
    i, j,
    intensity: Integer;
  begin
    for i:= Floor(-R) to Ceil(R) do
    begin
      x := i + 0.5;
      for j:= Floor(-2*R) to Ceil(2 * R) do
      begin
        y:= j / 2 + 0.5;
        if(x * x + y * y <= R * R) then
        begin
          vec[0]:= x;
          vec[1]:= y;
          vec[2]:= sqrt(R * R - x * x - y * y);
          normalize(vec);
          b:= Power(dot(light, vec), k) + ambient;
          intensity:= IfThen(b <= 0,
                             Length(shades) - 2,
                             Trunc(max( (1 - b) * (Length(shades) - 1), 0 )));
          Write(shades[intensity]);
        end
        else
          Write(' ');
      end;
      Writeln;
    end;
  end;

begin
  normalize(light);
  drawSphere(19, 4, 0.1);
  drawSphere(10, 2, 0.4);
  Readln;
end.

```


```txt

                             &&&&&&&&&&#######
                       &eeeeeooeeeeeeee&&&&&&#######
                   eeooo*********oooooeeeee&&&&&#######%
                eo***!!!!!!!!!!!*****ooooeeee&&&&&#######%%
             eo**!!!::::::::::!!!!!****ooooeeee&&&&########%%%
           eo*!!::::........:::::!!!!****oooeeee&&&&########%%%%
         eo*!!::..............:::::!!!***ooooeeee&&&&#########%%%%
       eo*!!::..................:::!!!!***oooeeee&&&&&########%%%%%%
      eo*!::....................::::!!!***ooooeeee&&&&#########%%%%%%
     o**!::.....................::::!!!***ooooeeee&&&&#########%%%%%%%
    eo*!::......................::::!!!***ooooeeee&&&&##########%%%%%%%
   eo*!!::......................:::!!!!***ooooeee&&&&&##########%%%%%%%%
  eo**!!::.....................:::!!!!***ooooeeee&&&&&##########%%%%%%%%%
 &eo**!!::...................::::!!!!****oooeeeee&&&&&##########%%%%%%%%%%
 eeo**!!:::................:::::!!!!****ooooeeee&&&&&##########%%%%%%%%%%%
&eoo**!!!::::............:::::!!!!****oooooeeee&&&&&###########%%%%%%%%%%%%
&eeo***!!!::::::::::::::::::!!!!!****ooooeeeee&&&&&&##########%%%%%%%%%%%%%
&eeoo***!!!!::::::::::::!!!!!!*****oooooeeeee&&&&&&###########%%%%%%%%%%%%%
&&eeoo****!!!!!!!!!!!!!!!!!!*****oooooeeeee&&&&&&############%%%%%%%%%%%%%%
&&eeeooo*****!!!!!!!!!!*******ooooooeeeeee&&&&&&############%%%%%%%%%%%%%%%
#&&eeeoooo*****************oooooooeeeeee&&&&&&&############%%%%%%%%%%%%%%%%
#&&&eeeeoooooooooooooooooooooooeeeeeee&&&&&&&#############%%%%%%%%%%%%%%%%%
##&&&&eeeeeoooooooooooooooeeeeeeeee&&&&&&&&##############%%%%%%%%%%%%%%%%%%
 ###&&&&eeeeeeeeeeeeeeeeeeeeeee&&&&&&&&&&##############%%%%%%%%%%%%%%%%%%%
 ####&&&&&&&eeeeeeeeeeeeeee&&&&&&&&&&&################%%%%%%%%%%%%%%%%%%%%
  #####&&&&&&&&&&&&&&&&&&&&&&&&&&&&#################%%%%%%%%%%%%%%%%%%%%%
   ########&&&&&&&&&&&&&&&&&&&####################%%%%%%%%%%%%%%%%%%%%%%
    ############################################%%%%%%%%%%%%%%%%%%%%%%%
     %#######################################%%%%%%%%%%%%%%%%%%%%%%%%%
      %%##################################%%%%%%%%%%%%%%%%%%%%%%%%%%%
       %%%%###########################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         %%%%%%#################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                             %%%%%%%%%%%%%%%%%

             ::...:::!!!*o
         ..............::!!*oo
      ..................::!!**ooe
    .....................::!!**ooee
   .......................::!!**ooee
  ........................::!!**oooee
 .........................::!!**oooeee
:........................::!!!**oooeeee
........................::!!!**ooooeeee
:......................::!!!***oooeeeee
:....................:::!!!***oooeeeeee
!:.................:::!!!****oooeeeeeee
*!:::...........::::!!!!***ooooeeeeeeee
 *!!!:::::::::::!!!!!****oooooeeeeeeee
  o**!!!!!!!!!!!!!*****oooooeeeeeeeee
   oo**************ooooooeeeeeeeeeee
    eoooooooooooooooooeeeeeeeeeeeee
      eeeooooooooeeeeeeeeeeeeeeee
         eeeeeeeeeeeeeeeeeeeee
             eeeeeeeeeeeee

```



## DWScript

[[image:DWScript-sphere.pbm.png|thumb|right|PBM output magnified 5 times]]
{{trans|C}} but adapted to spit out a [[wp:Netpbm_format|PGM]] image

```delphi

type
   TFloat3  = array[0..2] of Float;

var
   light : TFloat3 = [ 30, 30, -50 ];

procedure normalize(var v : TFloat3);
var
   len: Float;
begin
    len := sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
    v[0] /= len;
    v[1] /= len;
    v[2] /= len;
end;

function dot(x, y : TFloat3) : Float;
begin
    Result := x[0]*y[0] + x[1]*y[1] + x[2]*y[2];
    if Result<0 then
       Result:=-Result
    else Result:=0;
end;

procedure drawSphere(R, k, ambient : Float);
var
   vec : TFloat3;
   x, y, b : Float;
   i, j, size, intensity : Integer;
begin
   size:=Trunc(Ceil(R)-Floor(-R)+1);
   PrintLn('P2');
   PrintLn(IntToStr(size)+' '+IntToStr(size));
   PrintLn('255');
   for i := Floor(-R) to Ceil(R) do begin
      x := i + 0.5;
      for j := Floor(-R) to Ceil(R) do begin
         y := j + 0.5;
         if (x * x + y * y <= R * R) then begin
            vec[0] := x;
            vec[1] := y;
            vec[2] := sqrt(R * R - x * x - y * y);
            normalize(vec);
            b := Power(dot(light, vec), k) + ambient;
            intensity := ClampInt( Round(b*255), 0, 255);
            Print(intensity);
            Print(' ')
         end else Print('0 ');
      end;
      PrintLn('');
   end;
end;

normalize(light);
drawSphere(19, 4, 0.1);

```



## ERRE

Using ASCII art: output is written to 'SPHERE.PRN' sequential file.

```ERRE
PROGRAM SPHERE

CONST SHADES$=".:!*oe&#%@"

DIM LIGHT[2],X[2],Y[2],V[2],VEC[2]

PROCEDURE DOT(X[],Y[]->D)
        D=X[0]*Y[0]+X[1]*Y[1]+X[2]*Y[2]
        IF D<0 THEN D=-D ELSE D=0 END IF
END PROCEDURE

PROCEDURE NORMALIZE(V[]->V[])
        LUNG=SQR(V[0]*V[0]+V[1]*V[1]+V[2]*V[2])
        V[0]=V[0]/LUNG
        V[1]=V[1]/LUNG
        V[2]=V[2]/LUNG
END PROCEDURE

PROCEDURE PDRAW(R,K,AMBIENT)
        FOR I=INT(-R) TO INT(R) DO
                X=I+0.5
                FOR J=INT(-2*R) TO INT(2*R) DO
                        Y=J/2+0.5
                        IF (X*X+Y*Y<=R*R) THEN
                                VEC[0]=X
                                VEC[1]=Y
                                VEC[2]=SQR(R*R-X*X-Y*Y)
                                NORMALIZE(VEC[]->VEC[])
                                DOT(LIGHT[],VEC[]->D)
                                B=D^K+AMBIENT
                                INTENSITY%=(1-B)*(LEN(SHADES$)-1)
                                IF (INTENSITY%<0) THEN INTENSITY%=0 END IF
                                IF (INTENSITY%>=LEN(SHADES$)-1) THEN
                                        INTENSITY%=LEN(SHADES$)-2
                                END IF
                                PRINT(#1,MID$(SHADES$,INTENSITY%+1,1);)
                           ELSE
                                PRINT(#1,(" ");)
                        END IF
                END FOR
                PRINT(#1,)
        END FOR
END PROCEDURE

BEGIN
    LIGHT[]=(30,30,-50)
    OPEN("O",1,"SPHERE.PRN")
       NORMALIZE(LIGHT[]->LIGHT[])
       PDRAW(10,2,0.4)

       PRINT(#1,STRING$(79,"="))
       PDRAW(20,4,0.1)
    CLOSE(1)
END PROGRAM

```

```txt
             !::::::!!!**o
         ............:::!!**oe
      :................::!!**ooee
    :...................::!!**ooeee
   ......................::!!**ooeee
  .......................::!!**ooeeee
 .......................:::!!**ooeeeee
:.......................::!!***ooeeeeee
:......................::!!!**oooeeeeee
:....................:::!!!**oooeeeeeee
!:..................:::!!***oooeeeeeeee
!!:..............::::!!!***oooeeeeeeeee
*!!::::.....::::::!!!!***ooooeeeeeeeeee
 o*!!!!::::::!!!!!!****ooooeeeeeeeeeee
  o****!!!!!!!******oooooeeeeeeeeeeee
   eooo********oooooooeeeeeeeeeeeeee
    eeeoooooooooooeeeeeeeeeeeeeeeee
      eeeeeeeeeeeeeeeeeeeeeeeeeee
         eeeeeeeeeeeeeeeeeeeee
             eeeeeeeeeeeee


### =========================================================================

                               ##############%%%
                        #&&eeeeeeeeeee&&&&&&######%%%%%
                    &eeeoooooooooooooeeeee&&&&&######%%%%%%
                 &eooo**************oooooeeee&&&&&#####%%%%%%%
              &eoo**!!!!!!!!!!!!!!*****ooooeeee&&&&######%%%%%%%%
            eoo**!!!::::::::::::!!!!****ooooeeee&&&&######%%%%%%%%%
          eoo*!!!::::.......::::::!!!!****oooeeee&&&&######%%%%%%%%%%
        &eo*!!:::..............::::!!!!***ooooeeee&&&&######%%%%%%%%%%%
       eo**!!::.................::::!!!****oooeeee&&&&######%%%%%%%%%%%%
     &eo*!!:::..................::::!!!!***oooeeee&&&&&######%%%%%%%%%%%%%
    &eo*!!:::...................::::!!!!***oooeeee&&&&&######%%%%%%%%%%%%%%
   &eo**!!::....................::::!!!****oooeeee&&&&&######%%%%%%%%%%%%%%%
  #eoo*!!:::...................::::!!!!***ooooeeee&&&&#######%%%%%%%%%%%%%%%%
  &eo**!!:::.................:::::!!!!****oooeeee&&&&&#######%%%%%%%%%%%%%%%%
 &eoo**!!::::...............:::::!!!!****ooooeeee&&&&#######%%%%%%%%%%%%%%%%%%
 &eoo**!!!::::...........::::::!!!!*****ooooeeee&&&&&#######%%%%%%%%%%%%%%%%%%
#&eoo***!!!::::::::::::::::::!!!!!****ooooeeeee&&&&&#######%%%%%%%%%%%%%%%%%%%%
#&eeoo***!!!!::::::::::::!!!!!!!*****ooooeeeee&&&&&#######%%%%%%%%%%%%%%%%%%%%%
#&eeooo****!!!!!!!!!!!!!!!!!!******ooooeeeee&&&&&&#######%%%%%%%%%%%%%%%%%%%%%%
#&&eeooo******!!!!!!!!!!!*******ooooooeeeee&&&&&&#######%%%%%%%%%%%%%%%%%%%%%%%
#&&&eeooooo******************ooooooeeeeee&&&&&&########%%%%%%%%%%%%%%%%%%%%%%%%
##&&&eeeooooooo********oooooooooeeeeeee&&&&&&#########%%%%%%%%%%%%%%%%%%%%%%%%%
###&&&eeeeeooooooooooooooooooeeeeeee&&&&&&&&#########%%%%%%%%%%%%%%%%%%%%%%%%%%
%###&&&&eeeeeeeeeeeoeeeeeeeeeeeee&&&&&&&&##########%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %####&&&&&eeeeeeeeeeeeeeeeee&&&&&&&&&&##########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%#####&&&&&&&&&&&&&&&&&&&&&&&&&&&############%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%#######&&&&&&&&&&&&&&&&&&&&##############%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%############&&&&&###################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%%%%##############################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%#######################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%%%%%%%%%%%%#########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                               %%%%%%%%%%%%%%%%%


```




## FutureBasic


```futurebasic

include "Tlbx agl.incl"
include "Tlbx glut.incl"

output file "Rotating Sphere"

local fn SphereDraw
'~'1
begin globals
dim as double  sRotation // 'static' var
end globals

// Speed of rotation
sRotation += 2.9
glMatrixMode( _GLMODELVIEW )

glLoadIdentity()

// Position parameters: x axis, y axis, z axis
// Set to center of screen:
glTranslated( 0.0, 0.0, 0.0 )

// Rotation (wobble) parameters: angle, x, y
glRotated( sRotation, -0.45, -0.8, -0.6 )

// Set color of sphere's wireframe
glColor3d( 1.0, 0.0, 0.3 )

// Set width of sphere's wireframe lines
glLineWidth( 1.5 )

// Apply above to GLUT's built-in sphere wireframe
// Size & frame parameters: radius, slices, stack
fn glutWireSphere( 0.8, 25, 25 )

end fn

// main program
dim as GLint           attrib(2)
dim as CGrafPtr        port
dim as AGLPixelFormat  fmt
dim as AGLContext      glContext
dim as EventRecord     ev
dim as GLboolean       yesOK

// Make a window
window 1, @"Rotating Sphere", (0,0) - (500,500)

// Minimal setup of OpenGL
attrib(0) = _AGLRGBA
attrib(1) = _AGLDOUBLEBUFFER
attrib(2) = _AGLNONE

fmt = fn aglChoosePixelFormat( 0, 0, attrib(0) )
glContext = fn aglCreateContext( fmt, 0 )
aglDestroyPixelFormat( fmt )

// Set the FB window as port for drawing
port = window( _wndPort )
yesOK = fn aglSetDrawable( glContext, port )
yesOK = fn aglSetCurrentContext( glContext )

// Background color: red, green, blue, alpha
glClearColor( 0.0, 0.0, 0.0, 0.0 )

// 60/s HandleEvents Trigger
poke long event - 8, 1
do
// Clear window
glClear( _GLCOLORBUFFERBIT )
// Run animation
fn SphereDraw
aglSwapBuffers( glContext )
HandleEvents
until gFBquit

```




## Go

[[file:GoSphere.png|right|thumb|Output png]]
Using image library rather than ASCII art.

```go
package main

import (
    "fmt"
    "image"
    "image/color"
    "image/png"
    "math"
    "os"
)

type vector [3]float64

func normalize(v *vector) {
    invLen := 1 / math.Sqrt(dot(v, v))
    v[0] *= invLen
    v[1] *= invLen
    v[2] *= invLen
}

func dot(x, y *vector) float64 {
    return x[0]*y[0] + x[1]*y[1] + x[2]*y[2]
}

func drawSphere(r int, k, amb float64, dir *vector) *image.Gray {
    w, h := r*4, r*3
    img := image.NewGray(image.Rect(-w/2, -h/2, w/2, h/2))
    vec := new(vector)
    for x := -r; x < r; x++ {
        for y := -r; y < r; y++ {
            if z := r*r - x*x - y*y; z >= 0 {
                vec[0] = float64(x)
                vec[1] = float64(y)
                vec[2] = math.Sqrt(float64(z))
                normalize(vec)
                s := dot(dir, vec)
                if s < 0 {
                    s = 0
                }
                lum := 255 * (math.Pow(s, k) + amb) / (1 + amb)
                if lum < 0 {
                    lum = 0
                } else if lum > 255 {
                    lum = 255
                }
                img.SetGray(x, y, color.Gray{uint8(lum)})
            }
        }
    }
    return img
}

func main() {
    dir := &vector{-30, -30, 50}
    normalize(dir)
    img := drawSphere(200, 1.5, .2, dir)
    f, err := os.Create("sphere.png")
    if err != nil {
        fmt.Println(err)
        return
    }
    if err = png.Encode(f, img); err != nil {
        fmt.Println(err)
    }
    if err = f.Close(); err != nil {
        fmt.Println(err)
    }
}
```



## HTML


See [[Draw_a_sphere#Javascript]]


## Haskell

[[File:Sphere_Haskell.png|thumb|right]]

```haskell
import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT.Objects
import Graphics.UI.GLUT

setProjection :: IO ()
setProjection = do
  matrixMode $= Projection
  ortho (-1) 1 (-1) 1 0 (-1)

grey1,grey9,red,white :: Color4 GLfloat
grey1 = Color4 0.1 0.1 0.1 1
grey9 = Color4 0.9 0.9 0.9 1
red   = Color4 1   0   0   1
white = Color4 1   1   1   1

setLights :: IO ()
setLights = do
  let l = Light 0
  ambient  l $= grey1
  diffuse  l $= white
  specular l $= white
  position l $= Vertex4 (-4) 4 3 (0 :: GLfloat)
  light    l $= Enabled
  lighting   $= Enabled

setMaterial :: IO ()
setMaterial = do
  materialAmbient   Front $= grey1
  materialDiffuse   Front $= red
  materialSpecular  Front $= grey9
  materialShininess Front $= (32 :: GLfloat)

display :: IO()
display = do
  clear [ColorBuffer]
  renderObject Solid $ Sphere' 0.8 64 64
  swapBuffers

main :: IO()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Sphere"
  clearColor $= Color4 0.0 0.0 0.0 0.0
  setProjection
  setLights
  setMaterial
  displayCallback $= display
  mainLoop
```


==Icon and {{header|Unicon}}==
Unicon provides a built-in interface to openGL including some higher level abstractions (for more information see [[Icon%2BUnicon/Intro#Graphics.2C_Network_Messaging.2C_etc.|Unicon Technical References, 3D Graphics]]).  The example below draws a blue sphere on a black background and waits for input to quit.[[File:Sphere_unicon.PNG|thumb|Unicon Sphere]]


```Unicon
procedure main()
W := open("Demo", "gl", "size=400,400", "bg=black") | stop("can't open window!")
WAttrib(W, "slices=40", "rings=40", "light0=on, ambient white; diffuse gold; specular gold; position 5, 0, 0" )
Fg(W, "emission blue")
DrawSphere(W, 0, 0, -5, 1)
Event(W)
end
```


<div style="border: 1px solid #000000; overflow: auto; width: 100%"></div>

## J


[[File:J-sphere.png|thumb|J Sphere]]

The simplest way to draw a sphere is to run the sphere demo code from J's simple demos.  (This assumes J version 6.)

Normally you would bring up this demo by using the menu system:

 Studio
  > Demos...
   > opengl simple... [ok]
     > sphere [Run]

But bringing up the example with a line of code is trivial enough:


```j
load 'system/examples/graphics/opengl/simple/sphere.ijs'
```

<div style="border: 1px solid #000000; overflow: auto; width: 100%"></div>


### Raytracing Solution


Here's a version using raytracing computed in J. luminosity is an array of luminosity values with theoretical maximum 1 and minimum 0, and viewmat is used to display this.


```j
'R k ambient' =. 10 2 0.4
light =. (% +/&.:*:) 30 30 _50
pts =. (0&*^:(0={:))@:(,,(0>.(*:R)-+)&.*:)"0/~ i:15j200
luminosity =. (>:ambient) %~ (ambient * * +/&.:*:"1 pts) + k^~ 0>. R%~ pts +/@:*"1 -light

load 'viewmat'
torgb =. 256 #. [: <. 255 255 255 *"1 0 ]
'rgb' viewmat torgb luminosity
```



## Java

```java
public class Sphere{
    static char[] shades = {'.', ':', '!', '*', 'o', 'e', '&', '#', '%', '@'};

    static double[] light = { 30, 30, -50 };
    private static void normalize(double[] v){
        double len = Math.sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
        v[0] /= len; v[1] /= len; v[2] /= len;
    }

    private static double dot(double[] x, double[] y){
        double d = x[0]*y[0] + x[1]*y[1] + x[2]*y[2];
        return d < 0 ? -d : 0;
    }

    public static void drawSphere(double R, double k, double ambient){
        double[] vec = new double[3];
        for(int i = (int)Math.floor(-R); i <= (int)Math.ceil(R); i++){
            double x = i + .5;
            for(int j = (int)Math.floor(-2 * R); j <= (int)Math.ceil(2 * R); j++){
                double y = j / 2. + .5;
                if(x * x + y * y <= R * R) {
                    vec[0] = x;
                    vec[1] = y;
                    vec[2] = Math.sqrt(R * R - x * x - y * y);
                    normalize(vec);
                    double b = Math.pow(dot(light, vec), k) + ambient;
                    int intensity = (b <= 0) ?
                                shades.length - 2 :
                                (int)Math.max((1 - b) * (shades.length - 1), 0);
                    System.out.print(shades[intensity]);
                } else
                    System.out.print(' ');
            }
            System.out.println();
        }
    }

    public static void main(String[] args){
        normalize(light);
        drawSphere(20, 4, .1);
        drawSphere(10, 2, .4);
    }
}
```

```txt
                               &&&&&&&&&&#######
                        &eeeeeeeeeeeeeeee&&&&&&#######%
                    &eoooo*******oooooooeeeee&&&&&########%
                 eoo****!!!!!!!!******oooooeeee&&&&&########%%
              eoo**!!!!::::::::!!!!!*****ooooeeee&&&&&########%%%
            eo**!!::::::...:::::::!!!!!***ooooeeee&&&&&########%%%%
          eo*!!:::.............:::::!!!!***ooooeeee&&&&&########%%%%%
        eo*!!:::.................::::!!!!***ooooeeee&&&&#########%%%%%%
       eo*!!::....................::::!!!****oooeeee&&&&&#########%%%%%%
     &o**!::......................::::!!!****oooeeee&&&&&##########%%%%%%%
    &o**!::.......................::::!!!****oooeeee&&&&&##########%%%%%%%%
   &oo*!!::.......................:::!!!!***ooooeeee&&&&&##########%%%%%%%%%
  &eo*!!::.......................::::!!!****ooooeeee&&&&&##########%%%%%%%%%%
  eo**!!::......................::::!!!!***ooooeeeee&&&&&##########%%%%%%%%%%
 &eo**!!:::...................:::::!!!!****ooooeeee&&&&&###########%%%%%%%%%%%
 eeo**!!::::................:::::!!!!!****ooooeeee&&&&&&###########%%%%%%%%%%%
&eeo***!!:::::...........::::::!!!!!****oooooeeee&&&&&&###########%%%%%%%%%%%%%
&eeoo**!!!!::::::::::::::::::!!!!!*****ooooeeeee&&&&&&############%%%%%%%%%%%%%
&eeooo***!!!!::::::::::::!!!!!!!*****oooooeeeee&&&&&&############%%%%%%%%%%%%%%
&&eeooo***!!!!!!!!!!!!!!!!!!!******oooooeeeeee&&&&&&############%%%%%%%%%%%%%%%
&&eeeooo******!!!!!!!!!!********ooooooeeeeee&&&&&&&############%%%%%%%%%%%%%%%%
#&&eeeooooo******************oooooooeeeeee&&&&&&&#############%%%%%%%%%%%%%%%%%
#&&&eeeeoooooooo******oooooooooooeeeeeee&&&&&&&&#############%%%%%%%%%%%%%%%%%%
##&&&&eeeeeooooooooooooooooooeeeeeeee&&&&&&&&&##############%%%%%%%%%%%%%%%%%%%
 ##&&&&&eeeeeeeeeeeeeeeeeeeeeeeeee&&&&&&&&&################%%%%%%%%%%%%%%%%%%%
 ####&&&&&&eeeeeeeeeeeeeeeeeee&&&&&&&&&&&################%%%%%%%%%%%%%%%%%%%%%
  #####&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#################%%%%%%%%%%%%%%%%%%%%%%
  %#######&&&&&&&&&&&&&&&&&&&&&&&&###################%%%%%%%%%%%%%%%%%%%%%%%%
   %###########&&&&&&&&&&&&&#######################%%%%%%%%%%%%%%%%%%%%%%%%%
    %############################################%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%#######################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       %%#################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%#########################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          %%%%%%%%#############%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                               %%%%%%%%%%%%%%%%%

             ::...:::!!!*o
         ..............::!!*oo
      ..................::!!**ooe
    .....................::!!**ooee
   .......................::!!**ooee
  ........................::!!**oooee
 .........................::!!**oooeee
:........................::!!!**oooeeee
........................::!!!**ooooeeee
:......................::!!!***oooeeeee
:....................:::!!!***oooeeeeee
!:.................:::!!!****oooeeeeeee
*!:::...........::::!!!!***ooooeeeeeeee
 *!!!:::::::::::!!!!!****oooooeeeeeeee
  o**!!!!!!!!!!!!!*****oooooeeeeeeeee
   oo**************ooooooeeeeeeeeeee
    eoooooooooooooooooeeeeeeeeeeeee
      eeeooooooooeeeeeeeeeeeeeeee
         eeeeeeeeeeeeeeeeeeeee
             eeeeeeeeeeeee

```


## JavaScript


This Javascript entry uses an HTML wrapper to offer easy running and some interactivity. It is made as such, though, that the entire HTML wrapper can be removed (except for a canvas with id <code>c</code>) and still work. If you remove the HTML, call the <code>draw_sphere</code> function to draw the thing.


```javascript
<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Draw a sphere</title>
<script>
var light=[30,30,-50],gR,gk,gambient;

function normalize(v){
	var len=Math.sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);
	v[0]/=len;
	v[1]/=len;
	v[2]/=len;
	return v;
}

function dot(x,y){
	var d=x[0]*y[0]+x[1]*y[1]+x[2]*y[2];
	return d<0?-d:0;
}

function draw_sphere(R,k,ambient){
	var i,j,intensity,b,vec,x,y,cvs,ctx,imgdata,idx;
	cvs=document.getElementById("c");
	ctx=cvs.getContext("2d");
	cvs.width=cvs.height=2*Math.ceil(R)+1;
	imgdata=ctx.createImageData(2*Math.ceil(R)+1,2*Math.ceil(R)+1);
	idx=0;
	for(i=Math.floor(-R);i<=Math.ceil(R);i++){
		x=i+.5;
		for(j=Math.floor(-R);j<=Math.ceil(R);j++){
			y=j+.5;
			if(x*x+y*y<=R*R){
				vec=[x,y,Math.sqrt(R*R-x*x-y*y)];
				vec=normalize(vec);
				b=Math.pow(dot(light,vec),k)+ambient;
				intensity=(1-b)*256;
				if(intensity<0)intensity=0;
				if(intensity>=256)intensity=255;
				imgdata.data[idx++]=imgdata.data[idx++]=255-~~(intensity); //RG
				imgdata.data[idx++]=imgdata.data[idx++]=255; //BA
			} else {
				imgdata.data[idx++]=imgdata.data[idx++]=imgdata.data[idx++]=imgdata.data[idx++]=255; //RGBA
			}
		}
	}
	ctx.putImageData(imgdata,0,0);
}

light=normalize(light);
</script>
</head>
<body onload="gR=200;gk=4;gambient=.2;draw_sphere(gR,gk,gambient)">
R=<input type="range" id="R" name="R" min="5" max="500" value="200" step="5" onchange="document.getElementById('lR').innerHTML=gR=parseFloat(this.value);draw_sphere(gR,gk,gambient)">
<label for="R" id="lR">200</label>

k=<input type="range" id="k" name="k" min="0" max="10" value="4" step=".25" onchange="document.getElementById('lk').innerHTML=gk=parseFloat(this.value);draw_sphere(gR,gk,gambient)">
<label for="k" id="lk">4</label>

ambient=<input type="range" id="ambient" name="ambient" min="0" max="1" value=".2" step=".05" onchange="document.getElementById('lambient').innerHTML=gambient=parseFloat(this.value);draw_sphere(gR,gk,gambient)">
<label for="ambient" id="lambient">0.2</label>

<canvas id="c">Unsupportive browser...</canvas>

</body>
</html>
```



## jq

The approach adopted here is to generate an SVG file, which may then be viewed, for example, in a web browser.

```jq
def svg:
  "<svg width='100%' height='100%' version='1.1'
    xmlns='http://www.w3.org/2000/svg'
    xmlns:xlink='http://www.w3.org/1999/xlink'>" ;

# A radial gradient to make a circle look like a sphere.
# "colors" should be [startColor, intermediateColor, endColor]
# or null for ["white", "teal", "black"]
def sphericalGradient(id; colors):
  "<defs>
        <radialGradient id = '\(id)' cx = '30%' cy = '30%' r = '100%' fx='10%' fy='10%' >
            <stop stop-color = '\(colors[0]//"white")' offset =   '0%'/>
            <stop stop-color = '\(colors[1]//"teal")'  offset =  '50%'/>
            <stop stop-color = '\(colors[1]//"black")' offset = '100%'/>
        </radialGradient>
    </defs>" ;

def sphere(cx; cy; r; gradientId):
   "<circle fill='url(#\(gradientId))' cx='\(cx)' cy='\(cy)' r='\(r)' />" ;
```


'''Example:'''

```jq
def draw_sphere:
  svg,
   "<title>Teal sphere</title>",
    sphericalGradient("tealGradient"; null), # define the gradient to use
    sphere(100;100;100; "tealGradient"),     # draw a sphere using the gradient
    sphere(100;300;100; "tealGradient"),     # draw another sphere using the same gradient
  "</svg>" ;

draw_sphere
```

```sh
$ jq -r -n -f spheres.jq > spheres.svg
```


One way to view the output as an image is to point your browser to the generated SVG.


## Julia

```julia
# v0.6

function draw_sphere(r, k, ambient, light)
    shades = ('.', ':', '!', '*', 'o', 'e', '&', '#', '%', '@')
    for i in floor(Int, -r):ceil(Int, r)
        x = i + 0.5
        line = IOBuffer()
        for j in floor(Int, -2r):ceil(2r)
            y = j / 2 + 0.5
            if x ^ 2 + y ^ 2 ≤ r ^ 2
                v = normalize([x, y, sqrt(r ^ 2 - x ^ 2 - y ^ 2)])
                b = dot(light, v) ^ k + ambient
                intensity = ceil(Int, (1 - b) * (length(shades) - 1))
                if intensity < 1
                    intensity = 1 end
                if intensity > length(shades)
                    intensity = length(shades) end
                print(shades[intensity])
            else
                print(' ')
            end
        end
        println()
    end
end

light = normalize([30, 30, -50])
draw_sphere(20, 4, 0.1, light)
draw_sphere(10, 2, 0.4, light)
```



## Kotlin

```scala
// version 1.0.6

const val shades = ".:!*oe&#%@"
val light  = doubleArrayOf(30.0, 30.0, -50.0)

fun normalize(v: DoubleArray) {
    val len = Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])
    v[0] /= len; v[1] /= len; v[2] /= len
}

fun dot(x: DoubleArray, y: DoubleArray): Double {
    val d = x[0] * y[0] + x[1] * y[1] + x[2] * y[2]
    return if (d < 0.0) -d else 0.0
}

fun drawSphere(r: Double, k: Double, ambient: Double) {
    val vec = DoubleArray(3)
    var intensity: Int
    var b : Double
    var x: Double
    var y: Double
    for (i in Math.floor(-r).toInt() .. Math.ceil(r).toInt()) {
        x = i + 0.5
        for (j in Math.floor(-2.0 * r).toInt() .. Math.ceil(2.0 * r).toInt()) {
            y = j / 2.0 + 0.5
            if (x * x + y * y <= r * r) {
                vec[0] = x
                vec[1] = y
                vec[2] = Math.sqrt(r * r - x * x - y * y)
                normalize(vec)
                b = Math.pow(dot(light, vec), k) + ambient
                intensity = ((1.0 - b) * (shades.length - 1)).toInt()
                if (intensity < 0) intensity = 0
                if (intensity >= shades.length - 1) intensity = shades.length - 2
                print(shades[intensity])
            }
            else print(' ')
        }
        println()
    }
}

fun main(args: Array<String>) {
    normalize(light)
    drawSphere(20.0, 4.0, 0.1)
    drawSphere(10.0, 2.0, 0.4)
}
```


```txt

                               &&&&&&&&&&#######
                        &eeeeeeeeeeeeeeee&&&&&&#######%
                    &eoooo*******oooooooeeeee&&&&&########%
                 eoo****!!!!!!!!******oooooeeee&&&&&########%%
              eoo**!!!!::::::::!!!!!*****ooooeeee&&&&&########%%%
            eo**!!::::::...:::::::!!!!!***ooooeeee&&&&&########%%%%
          eo*!!:::.............:::::!!!!***ooooeeee&&&&&########%%%%%
        eo*!!:::.................::::!!!!***ooooeeee&&&&#########%%%%%%
       eo*!!::....................::::!!!****oooeeee&&&&&#########%%%%%%
     &o**!::......................::::!!!****oooeeee&&&&&##########%%%%%%%
    &o**!::.......................::::!!!****oooeeee&&&&&##########%%%%%%%%
   &oo*!!::.......................:::!!!!***ooooeeee&&&&&##########%%%%%%%%%
  &eo*!!::.......................::::!!!****ooooeeee&&&&&##########%%%%%%%%%%
  eo**!!::......................::::!!!!***ooooeeeee&&&&&##########%%%%%%%%%%
 &eo**!!:::...................:::::!!!!****ooooeeee&&&&&###########%%%%%%%%%%%
 eeo**!!::::................:::::!!!!!****ooooeeee&&&&&&###########%%%%%%%%%%%
&eeo***!!:::::...........::::::!!!!!****oooooeeee&&&&&&###########%%%%%%%%%%%%%
&eeoo**!!!!::::::::::::::::::!!!!!*****ooooeeeee&&&&&&############%%%%%%%%%%%%%
&eeooo***!!!!::::::::::::!!!!!!!*****oooooeeeee&&&&&&############%%%%%%%%%%%%%%
&&eeooo***!!!!!!!!!!!!!!!!!!!******oooooeeeeee&&&&&&############%%%%%%%%%%%%%%%
&&eeeooo******!!!!!!!!!!********ooooooeeeeee&&&&&&&############%%%%%%%%%%%%%%%%
#&&eeeooooo******************oooooooeeeeee&&&&&&&#############%%%%%%%%%%%%%%%%%
#&&&eeeeoooooooo******oooooooooooeeeeeee&&&&&&&&#############%%%%%%%%%%%%%%%%%%
##&&&&eeeeeooooooooooooooooooeeeeeeee&&&&&&&&&##############%%%%%%%%%%%%%%%%%%%
 ##&&&&&eeeeeeeeeeeeeeeeeeeeeeeeee&&&&&&&&&################%%%%%%%%%%%%%%%%%%%
 ####&&&&&&eeeeeeeeeeeeeeeeeee&&&&&&&&&&&################%%%%%%%%%%%%%%%%%%%%%
  #####&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#################%%%%%%%%%%%%%%%%%%%%%%
  %#######&&&&&&&&&&&&&&&&&&&&&&&&###################%%%%%%%%%%%%%%%%%%%%%%%%
   %###########&&&&&&&&&&&&&#######################%%%%%%%%%%%%%%%%%%%%%%%%%
    %############################################%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%#######################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       %%#################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%#########################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          %%%%%%%%#############%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                               %%%%%%%%%%%%%%%%%

             ::...:::!!!*o
         ..............::!!*oo
      ..................::!!**ooe
    .....................::!!**ooee
   .......................::!!**ooee
  ........................::!!**oooee
 .........................::!!**oooeee
:........................::!!!**oooeeee
........................::!!!**ooooeeee
:......................::!!!***oooeeeee
:....................:::!!!***oooeeeeee
!:.................:::!!!****oooeeeeeee
*!:::...........::::!!!!***ooooeeeeeeee
 *!!!:::::::::::!!!!!****oooooeeeeeeee
  o**!!!!!!!!!!!!!*****oooooeeeeeeeee
   oo**************ooooooeeeeeeeeeee
    eoooooooooooooooooeeeeeeeeeeeee
      eeeooooooooeeeeeeeeeeeeeeee
         eeeeeeeeeeeeeeeeeeeee
             eeeeeeeeeeeee

```



## Lingo


```lingo
----------------------------------------
-- Draw a circle
-- @param {image} img
-- @param {integer} x
-- @param {integer} y
-- @param {integer} r
-- @param {integer} lineSize
-- @param {color} drawColor
----------------------------------------
on circle (img, x, y, r, lineSize, drawColor)
  props = [:]
  props[#shapeType] = #oval
  props[#lineSize] = lineSize
  props[#color] = drawColor
  img.draw(x-r, y-r, x+r, y+r, props)
end
```



## Logo


Drawing a sphere is actually very simple in logo, using the ''perspective'' function to make life easier.
```logo
to sphere :r
cs perspective ht ;making the room ready to use
repeat 180 [polystart circle :r polyend down 1]
polyview
end
```



## Lua

```Lua
require ("math")

shades = {'.', ':', '!', '*', 'o', 'e', '&', '#', '%', '@'}

function normalize (vec)
    len = math.sqrt(vec[1]^2 + vec[2]^2 + vec[3]^2)
    return {vec[1]/len, vec[2]/len, vec[3]/len}
end

light = normalize{30, 30, -50}

function dot (vec1, vec2)
    d = vec1[1]*vec2[1] + vec1[2]*vec2[2] + vec1[3]*vec2[3]
    return d < 0 and -d or 0
end

function draw_sphere (radius, k, ambient)
    for i = math.floor(-radius),-math.floor(-radius) do
        x = i + .5
        local line = ''
        for j = math.floor(-2*radius),-math.floor(-2*radius) do
            y = j / 2 + .5
            if x^2 + y^2 <= radius^2 then
                vec = normalize{x, y, math.sqrt(radius^2 - x^2 - y^2)}
                b = dot(light,vec) ^ k + ambient
                intensity = math.floor ((1 - b) * #shades)
                line = line .. (shades[intensity] or shades[1])
            else
                line = line .. ' '
            end
        end
        print (line)
    end
end

draw_sphere (20, 4, 0.1)
draw_sphere (10, 2, 0.4)
```

```txt
                               &&&&&&&&&&&&#####
                        &eeeoooooooooeeeeee&&&&&#######
                    eooo*************oooooeeee&&&&&########
                 eo**!!!!!!!!!!!!!!!*****ooooeeee&&&&#########
              eo*!!!:::::...:::::::!!!!****oooeeee&&&&&##########
            o**!:::..............::::!!!!***ooooeee&&&&&###########
          o*!!::...................::::!!!***ooooeee&&&&&############
        eo*!::......................::::!!!***oooeeee&&&&&#############
       o*!::.........................:::!!!***ooooeee&&&&&##############
     &o*!::..........................:::!!!***ooooeeee&&&&###############%
    eo*!::...........................:::!!!***ooooeeee&&&&&###############%
   eo*!::............................:::!!!***ooooeeee&&&&&###############%%
  &o*!!::...........................:::!!!!***oooeeee&&&&&#################%%
  eo*!!:...........................::::!!!***ooooeeee&&&&&#################%%
 eo**!!::.........................::::!!!****oooeeee&&&&&&#################%%%
 eo**!!::.......................::::!!!!****oooeeeee&&&&&##################%%%
&eo**!!:::....................:::::!!!!***ooooeeeee&&&&&&##################%%%%
&eoo**!!::::................:::::!!!!****ooooeeeee&&&&&&###################%%%%
&eoo***!!!:::::........:::::::!!!!!****oooooeeeee&&&&&&###################%%%%%
&eeoo***!!!!:::::::::::::::!!!!!!*****ooooeeeee&&&&&&&####################%%%%%
&&eeoo****!!!!!!!!!!!!!!!!!!!!*****oooooeeeeee&&&&&&######################%%%%%
&&eeeooo******!!!!!!!!!!!*******ooooooeeeeee&&&&&&&######################%%%%%%
#&&eeeooooo******************oooooooeeeeee&&&&&&&#######################%%%%%%%
##&&&eeeeoooooooooo*ooooooooooooeeeeeeee&&&&&&&&#######################%%%%%%%%
 ##&&&eeeeeeooooooooooooooooeeeeeeeee&&&&&&&&&########################%%%%%%%%
 ###&&&&&eeeeeeeeeeeeeeeeeeeeeeee&&&&&&&&&&##########################%%%%%%%%%
  ####&&&&&&&eeeeeeeeeeeeeee&&&&&&&&&&&&############################%%%%%%%%%
  ######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&##############################%%%%%%%%%%
   ########&&&&&&&&&&&&&&&&&&&&&&################################%%%%%%%%%%%
    ###############&&&&&#######################################%%%%%%%%%%%%
     #########################################################%%%%%%%%%%%%
       #####################################################%%%%%%%%%%%%
        #################################################%%%%%%%%%%%%%%
          #############################################%%%%%%%%%%%%%%
            ########################################%%%%%%%%%%%%%%%
              ##################################%%%%%%%%%%%%%%%%%
                 %##########################%%%%%%%%%%%%%%%%%%
                    %%%%############%%%%%%%%%%%%%%%%%%%%%%%
                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                               %%%%%%%%%%%%%%%%%

             ........::!!*
         ...............::!!*o
      ....................::!**oo
    .......................::!**ooo
   ........................::!!**ooo
  .........................::!!**oooo
 ..........................::!!**ooooe
...........................::!!**ooooee
..........................::!!***ooooee
.........................::!!!**oooooee
.......................:::!!!**oooooeee
:.....................::!!!***ooooooeee
!::................:::!!!!***ooooooeeee
 !!::..........:::::!!!****ooooooeeeee
  *!!!::::::::::!!!!!****oooooooeeeee
   o***!!!!!!!!!******ooooooooeeeeee
    ooo***********ooooooooooeeeeeee
      oooooooooooooooooooeeeeeeee
         eooooooooooeeeeeeeeee
             eeeeeeeeeeeee

```


## M2000 Interpreter


```M2000 Interpreter

Module CheckIt {
      Er$="Pset is a new statement"
      If Version<9.4 Then Error Er$
      If Version=9.4 then If revision<26 then Error Er$
      Form 60, 40
      Cls 0 ' Black
      Gradient 0,1
      Pen 14 ' Yellow
      Set Fast !
      Refresh 500
      Module Sphere (R as long, X0 as long, Y0 as long, fun){
            R2 = R * R
            Def Long X, Y, D2
            Let Scale=twipsx/R*13.5
            For Y = -R To R  step twipsx  {
            Move X0-R, Y+Y0
            For X = -R To R step twipsy  {
                  D2 = X **2 + Y **2
                  IF R2>D2 THEN Pset Fun(Max.Data(Min.Data((Sqrt(R2 - D2) - ( X + Y) / 2 )*Scale ,255),0))
                  Step twipsx
            }
            }
      }
      Blue=lambda (c)->{
            c1=c/4+192
            =Color(c,c,c1)
      }
      Blue1=lambda (c)->{
            c1=c/4+Random(150,192)
            =Color(c,c,c1)
      }
      Mystery=lambda m=1 (c)->{
            c1=c/4+m
            m+=10
            if m>192 then m=1
            =Color(c,c,c1)
      }
      Mystery2=lambda m=1, p=true  (c)->{
            c1=c/4+m
           if p then m+=10
           Else m=-10
            if m>192 then m-=10 : p=false
            If m<0 then m+=10: p=true
            =Color(c,c,c1)
      }
      Buffer Alfa as byte*8
      Trans =lambda  Alfa (c) -> {
            Return Alfa, 0:=-point as long
            Return Alfa, 4:=-color(c,c, c/4+192) as long
            for i=0 to 2: Return Alfa, i:=(Eval(Alfa, i)+Eval(Alfa, i+4))/2: Next i
            =-Eval(Alfa, 0 as long)
      }
      Sphere 2400, 9000,7000, Blue
      Sphere 800, 6000, 7000, Blue1
      Sphere 1200, 5000,5000, Mystery
      Sphere 1200, 10000,6000, Mystery2
      Sphere 1200, 8000,5000, trans
}
Checkit

```


[[https://2.bp.blogspot.com/-ZWy2xDxXbzg/W98lAuNSY9I/AAAAAAAAHZ0/DFYluvWtz_cwwAUKfblujnW6mTC5XVs1QCLcBGAs/s1600/sphere.png]image]


## Maple

[[File:Sphere_Maple.png|thumb]]

```maple
plots[display](plottools[sphere](), axes = none, style = surface);
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica has many 3D drawing capabilities. To create a sphere with radius one centered at (0,0,0):

```Mathematica
Graphics3D[Sphere[{0,0,0},1]]
```



## MATLAB

To create the unit sphere:

```MATLAB>figure; sphere</lang



## Maxima


```maxima
/* Two solutions */
plot3d(1, [theta, 0, %pi], [phi, 0, 2 * %pi],
[transform_xy, spherical_to_xyz], [grid, 30, 60],
[box, false], [legend, false])$

load(draw)$
draw3d(xu_grid=30, yv_grid=60, surface_hide=true,
   parametric_surface(cos(phi)*sin(theta),
                      sin(phi)*sin(theta),
                      cos(theta),
                      theta, 0, %pi, phi, 0, 2 * %pi))$
```



## Nim

```nim
import math

type Point = tuple[x,y,z: float]

const shades = ".:!*oe&#%@"

proc normalize(x, y, z: float): Point =
  let len = sqrt(x*x + y*y + z*z)
  (x / len, y / len, z / len)

proc dot(a, b: Point): float =
  result = max(0, - a.x*b.x - a.y*b.y - a.z*b.z)

let light = normalize(30.0, 30.0, -50.0)

proc drawSphere(r, k, ambient) =
  for i in -r .. r:
    let x = i.float + 0.5
    for j in -2*r .. 2*r:
      let y = j.float / 2.0 + 0.5
      if x*x + y*y <= float r*r:
        let
          v = normalize(x, y, sqrt(float(r*r) - x*x - y*y))
          b = pow(dot(light, v), k) + ambient
          i = clamp(int((1.0 - b) * shades.high.float), 0, shades.high)
        stdout.write shades[i]
      else: stdout.write ' '
    stdout.write "\n"

drawSphere 20, 4.0, 0.1
drawSphere 10, 2.0, 0.4
```

```txt
                               &&&&&&&&&&#######
                        &eeeeeeeeeeeeeeee&&&&&&#######%
                    &eoooo*******oooooooeeeee&&&&&########%
                 eoo****!!!!!!!!******oooooeeee&&&&&########%%
              eoo**!!!!::::::::!!!!!*****ooooeeee&&&&&########%%%
            eo**!!::::::...:::::::!!!!!***ooooeeee&&&&&########%%%%
          eo*!!:::.............:::::!!!!***ooooeeee&&&&&########%%%%%
        eo*!!:::.................::::!!!!***ooooeeee&&&&#########%%%%%%
       eo*!!::....................::::!!!****oooeeee&&&&&#########%%%%%%
     &o**!::......................::::!!!****oooeeee&&&&&##########%%%%%%%
    &o**!::.......................::::!!!****oooeeee&&&&&##########%%%%%%%%
   &oo*!!::.......................:::!!!!***ooooeeee&&&&&##########%%%%%%%%%
  &eo*!!::.......................::::!!!****ooooeeee&&&&&##########%%%%%%%%%%
  eo**!!::......................::::!!!!***ooooeeeee&&&&&##########%%%%%%%%%%
 &eo**!!:::...................:::::!!!!****ooooeeee&&&&&###########%%%%%%%%%%%
 eeo**!!::::................:::::!!!!!****ooooeeee&&&&&&###########%%%%%%%%%%%
&eeo***!!:::::...........::::::!!!!!****oooooeeee&&&&&&###########%%%%%%%%%%%%%
&eeoo**!!!!::::::::::::::::::!!!!!*****ooooeeeee&&&&&&############%%%%%%%%%%%%%
&eeooo***!!!!::::::::::::!!!!!!!*****oooooeeeee&&&&&&############%%%%%%%%%%%%%%
&&eeooo***!!!!!!!!!!!!!!!!!!!******oooooeeeeee&&&&&&############%%%%%%%%%%%%%%%
&&eeeooo******!!!!!!!!!!********ooooooeeeeee&&&&&&&############%%%%%%%%%%%%%%%%
#&&eeeooooo******************oooooooeeeeee&&&&&&&#############%%%%%%%%%%%%%%%%%
#&&&eeeeoooooooo******oooooooooooeeeeeee&&&&&&&&#############%%%%%%%%%%%%%%%%%%
##&&&&eeeeeooooooooooooooooooeeeeeeee&&&&&&&&&##############%%%%%%%%%%%%%%%%%%%
 ##&&&&&eeeeeeeeeeeeeeeeeeeeeeeeee&&&&&&&&&################%%%%%%%%%%%%%%%%%%%
 ####&&&&&&eeeeeeeeeeeeeeeeeee&&&&&&&&&&&################%%%%%%%%%%%%%%%%%%%%%
  #####&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#################%%%%%%%%%%%%%%%%%%%%%%
  %#######&&&&&&&&&&&&&&&&&&&&&&&&###################%%%%%%%%%%%%%%%%%%%%%%%%
   %###########&&&&&&&&&&&&&#######################%%%%%%%%%%%%%%%%%%%%%%%%%
    %############################################%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%#######################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       %%#################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%#########################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          %%%%%%%%#############%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                               %%%%%%%%%%%%%%%%%

             ::...:::!!!*o
         ..............::!!*oo
      ..................::!!**ooe
    .....................::!!**ooee
   .......................::!!**ooee
  ........................::!!**oooee
 .........................::!!**oooeee
:........................::!!!**oooeeee
........................::!!!**ooooeeee
:......................::!!!***oooeeeee
:....................:::!!!***oooeeeeee
!:.................:::!!!****oooeeeeeee
*!:::...........::::!!!!***ooooeeeeeeee
 *!!!:::::::::::!!!!!****oooooeeeeeeee
  o**!!!!!!!!!!!!!*****oooooeeeeeeeee
   oo**************ooooooeeeeeeeeeee
    eoooooooooooooooooeeeeeeeeeeeee
      eeeooooooooeeeeeeeeeeeeeeee
         eeeeeeeeeeeeeeeeeeeee
             eeeeeeeeeeeee
```



## Openscad

Drawing a sphere is easy in openscad:


```openscad
// This will produce a sphere of radius 5
sphere(5);
```


## Pascal

After changing "{$APPTYPE CONSOLE}" to "{$mode delphi}" or "{$mode objfpc}" the Delphi example works with FreePascal.



## Perl

This produces a PGM image which can't be uploaded on rosettacode at the moment.  It looks similar as the Perl 6 solution, though.


```perl
use strict;
use warnings;

my $x = my $y = 255;
$x |= 1; # must be odd
my $depth = 255;

my $light = Vector->new(rand, rand, rand)->normalized;

print "P2\n$x $y\n$depth\n";

my ($r, $ambient) = (($x - 1)/2, 0);
my ($r2) = $r ** 2;
{
    for my $x (-$r .. $r) {
	my $x2 = $x**2;
	for my $y (-$r .. $r) {
	    my $y2 = $y**2;
	    my $pixel = 0;
	    if ($x2 + $y2 < $r2) {
		my $v = Vector->new($x, $y, sqrt($r2 - $x2 - $y2))->normalized;
		my $I = $light . $v + $ambient;
		$I = $I < 0 ? 0 : $I > 1 ? 1 : $I;
		$pixel = int($I * $depth);
	    }
	    print $pixel;
	    print $y == $r ? "\n" : " ";
	}
    }
}

package Vector {
    sub new {
	my $class = shift;
	bless ref($_[0]) eq 'Array' ? $_[0] : [ @_ ], $class;
    }
    sub normalized {
	my $this = shift;
	my $norm = sqrt($this . $this);
	ref($this)->new( map $_/$norm, @$this );
    }
    use overload q{.} => sub {
	my ($a, $b) = @_;
	my $sum = 0;
	for (0 .. @$a - 1) {
	    $sum += $a->[$_] * $b->[$_]
	}
	return $sum;
    },
    q{""} => sub { sprintf "Vector:[%s]", join ' ', @{shift()} };
}
```



## Perl 6



### Pure Perl 6

The C code is modified to output a .pgm file.
[[File:Sphere-perl6.png|thumb]]

```perl6
my $width = my $height = 255; # must be odd

my @light = normalize([ 3, 2, -5 ]);

my $depth = 255;

sub MAIN ($outfile = 'sphere-perl6.pgm') {
    spurt $outfile, "P5\n$width $height\n$depth\n"; # .pgm header
    my $out = open( $outfile, :a, :bin ) orelse .die;
    $out.write( Blob.new(draw_sphere( ($width-1)/2, .9, .2) ) );
    $out.close;
}

sub normalize (@vec) { @vec »/» ([+] @vec »*« @vec).sqrt }

sub dot (@x, @y) { -([+] @x »*« @y) max 0 }

sub draw_sphere ( $rad, $k, $ambient ) {
    my @pixels[$height];
    my $r2 = $rad * $rad;
    my @range = -$rad .. $rad;
    @range.hyper.map: -> $x {
        my @row[$width];
        @range.map: -> $y {
            if (my $x2 = $x * $x) + (my $y2 = $y * $y) < $r2 {
                my @vector = normalize([$x, $y, ($r2 - $x2 - $y2).sqrt]);
                my $intensity = dot(@light, @vector) ** $k + $ambient;
                my $pixel = (0 max ($intensity * $depth).Int) min $depth;
                @row[$y+$rad] = $pixel;
            }
            else {
                @row[$y+$rad] = 0;
            }
        }
        @pixels[$x+$rad] = @row;
    }
    flat |@pixels.map: *.list;
}
```



### Cairo graphics library


```perl6
use Cairo;

given Cairo::Image.create(Cairo::FORMAT_ARGB32, 256, 256) {
    given Cairo::Context.new($_) {

        my Cairo::Pattern::Solid $bg .= create(.5,.5,.5);
        .rectangle(0, 0, 256, 256);
        .pattern($bg);
        .fill;
        $bg.destroy;

        my Cairo::Pattern::Gradient::Radial $shadow .=
           create(105.2, 102.4, 15.6, 102.4,  102.4, 128.0);
        $shadow.add_color_stop_rgba(0, .3, .3, .3, .3);
        $shadow.add_color_stop_rgba(1, .1, .1, .1, .02);
        .pattern($shadow);
        .arc(136.0, 134.0, 110, 0, 2 * pi);
        .fill;
        $shadow.destroy;

        my Cairo::Pattern::Gradient::Radial $sphere .=
           create(115.2, 102.4, 25.6, 102.4,  102.4, 128.0);
        $sphere.add_color_stop_rgba(0, 1, 1, .698, 1);
        $sphere.add_color_stop_rgba(1, .923, .669, .144, 1);
        .pattern($sphere);
        .arc(128.0, 128.0, 110, 0, 2 * pi);
        .fill;
        $sphere.destroy;
    };
    .write_png('sphere2-perl6.png');
}
```

See [https://github.com/thundergnat/rc/blob/master/img/sphere2-perl6.png sphere2-perl6.png] (offsite .png image)


## Phix

{{trans|Go}} (Go gets credit for the dot/normalize/drawSphere routines, but this draws on screen rather than to png file)
Sphere will resize to match the window.

```Phix
--
-- demo\rosetta\Draw_a_sphere.exw
--
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer, cdcanvas

function dot(sequence x, sequence y)
    return sum(sq_mul(x,y))
end function

function normalize(sequence v)
    atom len = sqrt(dot(v, v))
    if len=0 then return {0,0,0} end if
    return sq_mul(v,1/len)
end function

procedure drawSphere(integer width, height, atom k, atom amb, sequence direction)
integer r = floor((min(width,height)-20)/2)
integer cx = floor(width/2)
integer cy = floor(height/2)
integer lum
    for x=-r to r do
        for y=-r to r do
            integer z = r*r-x*x-y*y
            if z>=0 then
                atom s = dot(direction, normalize({x,y,sqrt(z)}))
                lum = and_bits(#FF,255*(power(iff(s<0?0:s),k)+amb)/(1+amb))
                lum += lum*#100+lum*#10000
                cdCanvasPixel(cddbuffer, x+cx, y+cy, lum)
            end if
        end for
    end for
end procedure

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
integer {width, height} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    drawSphere(width,height,1.5,0.2,normalize({-30,-30,50}))
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_BLACK)
    return IUP_DEFAULT
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "340x340") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", "Draw a sphere")
    IupSetCallback(dlg, "K_ANY",     Icallback("esc_close"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL) -- release the minimum limitation
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PicoLisp

This is for the 64-bit version.

```PicoLisp
(load "@lib/openGl.l")

(glutInit)
(glutInitDisplayMode (| GLUT_RGBA GLUT_DOUBLE GLUT_ALPHA GLUT_DEPTH))
(glutInitWindowSize 400 400)
(glutCreateWindow "Sphere")

(glEnable GL_LIGHTING)
(glEnable GL_LIGHT0)
(glLightiv GL_LIGHT0 GL_POSITION (10 10 -10 0))

(glEnable GL_COLOR_MATERIAL)
(glColorMaterial GL_FRONT_AND_BACK GL_AMBIENT_AND_DIFFUSE)

(glClearColor 0.3 0.3 0.5 0)
(glColor4f 0.0 0.8 0.0 1.0)

(displayPrg
   (glClear (| GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
   (glutSolidSphere 0.9 40 32)
   (glFlush)
   (glutSwapBuffers) )

# Exit upon mouse click
(mouseFunc '((Btn State X Y) (bye)))
(glutMainLoop)
```


```PicoLisp
(scl 24)

(setq *Shades
  (list "." ":" "!" "*" "o" "e" "&" "#" "%" "@"))

(setq *Light
  (list 30.0 30.0 -50.0))

(de normalize (V)
  (let Len
    (sqrt
      (sum
        (quote (X)
          (** X 2))
        V))
    (mapcar
      (quote (X)
        (*/ X 1.0 Len))
      V)))

(de dot (X Y)
  (let D (sum (quote (A B) (*/ A B 1.0)) X Y)
    (if (< D 0) (- D) 0)))

(de floor (N)
  (* 1.0 (*/ (- N 0.5) 1.0)))

(de ceil (N)
  (* 1.0 (*/ (+ N 0.5) 1.0)))

(de drawSphere (R K Ambient)
  (let Vec NIL
    (for (I (floor (- R)) (<= I (ceil R)) (+ I 1.0))
      (let X (+ I 0.5)
        (for (J (floor (* -2 R)) (<= J (ceil (* 2 R))) (+ J 1.0))
          (let Y (+ (/ J 2) 0.5)
            (if (<= (+ (*/ X X 1.0) (*/ Y Y 1.0)) (*/ R R 1.0))
              (prog
                (setq Vec
                  (list X Y
                    (sqrt
                      (* 1.0
                        (- (*/ R R 1.0)
                           (*/ X X 1.0)
                           (*/ Y Y 1.0))))))
                (setq Vec (normalize Vec))
                (let (B NIL
                      Intensity NIL)
                  (setq B (+ (/ (** (dot *Light Vec) K) (** 1.0 (- K 1))) Ambient))
                  (setq Intensity
                    (if (<= B 0)
                      (- (length *Shades) 2)
                      (max (format (round (*/ (- 1.0 B) (* (- (length *Shades) 1) 1.0) 1.0) 0)) 0)))
                  (prin (nth *Shades (+ Intensity 1) 1))))
              (prin " "))))
        (prinl)))))

(setq *Light (normalize *Light))
(drawSphere 20.0 4 0.1)
(drawSphere 10.0 2 0.4)
```

```txt
                               ##############%%%
                        #&&eeeeeeeeeee&&&&&&######%%%%%
                    &eeeoooooooooooooeeeee&&&&&######%%%%%%
                 &eooo**************oooooeeee&&&&&#####%%%%%%%
              &eoo**!!!!!!!!!!!!!!*****ooooeeee&&&&######%%%%%%%%
            eoo**!!!::::::::::::!!!!****ooooeeee&&&&######%%%%%%%%%
          eoo*!!!::::.......::::::!!!!****oooeeee&&&&######%%%%%%%%%%
        &eo*!!:::..............::::!!!!***ooooeeee&&&&######%%%%%%%%%%%
       eo**!!::.................::::!!!****oooeeee&&&&######%%%%%%%%%%%%
     &eo*!!:::..................::::!!!!***oooeeee&&&&&######%%%%%%%%%%%%%
    &eo*!!:::...................::::!!!!***oooeeee&&&&&######%%%%%%%%%%%%%%
   &eo**!!::....................::::!!!****oooeeee&&&&&######%%%%%%%%%%%%%%%
  #eoo*!!:::...................::::!!!!***ooooeeee&&&&#######%%%%%%%%%%%%%%%%
  &eo**!!:::.................:::::!!!!****oooeeee&&&&&#######%%%%%%%%%%%%%%%%
 &eoo**!!::::...............:::::!!!!****ooooeeee&&&&#######%%%%%%%%%%%%%%%%%%
 &eoo**!!!::::...........::::::!!!!*****ooooeeee&&&&&#######%%%%%%%%%%%%%%%%%%
#&eoo***!!!::::::::::::::::::!!!!!****ooooeeeee&&&&&#######%%%%%%%%%%%%%%%%%%%%
#&eeoo***!!!!::::::::::::!!!!!!!*****ooooeeeee&&&&&#######%%%%%%%%%%%%%%%%%%%%%
#&eeooo****!!!!!!!!!!!!!!!!!!******ooooeeeee&&&&&&#######%%%%%%%%%%%%%%%%%%%%%%
#&&eeooo******!!!!!!!!!!!*******ooooooeeeee&&&&&&#######%%%%%%%%%%%%%%%%%%%%%%%
#&&&eeooooo******************ooooooeeeeee&&&&&&########%%%%%%%%%%%%%%%%%%%%%%%%
##&&&eeeooooooo********oooooooooeeeeeee&&&&&&#########%%%%%%%%%%%%%%%%%%%%%%%%%
###&&&eeeeeooooooooooooooooooeeeeeee&&&&&&&&#########%%%%%%%%%%%%%%%%%%%%%%%%%%
%###&&&&eeeeeeeeeeeoeeeeeeeeeeeee&&&&&&&&##########%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %####&&&&&eeeeeeeeeeeeeeeeee&&&&&&&&&&##########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %%#####&&&&&&&&&&&&&&&&&&&&&&&&&&&############%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%#######&&&&&&&&&&&&&&&&&&&&##############%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  %%%%############&&&&&###################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%%%%##############################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%#######################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%%%%%%%%%%%%#########%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                               %%%%%%%%%%%%%%%%%


             !::::::!!!**o
         ............:::!!**oe
      :................::!!**ooee
    :...................::!!**ooeee
   ......................::!!**ooeee
  .......................::!!**ooeeee
 .......................:::!!**ooeeeee
:.......................::!!***ooeeeeee
:......................::!!!**oooeeeeee
:....................:::!!!**oooeeeeeee
!:..................:::!!***oooeeeeeeee
!!:..............::::!!!***oooeeeeeeeee
*!!::::.....::::::!!!!***ooooeeeeeeeeee
 o*!!!!::::::!!!!!!****ooooeeeeeeeeeee
  o****!!!!!!!******oooooeeeeeeeeeeee
   eooo********oooooooeeeeeeeeeeeeee
    eeeoooooooooooeeeeeeeeeeeeeeeee
      eeeeeeeeeeeeeeeeeeeeeeeeeee
         eeeeeeeeeeeeeeeeeeeee
             eeeeeeeeeeeee



```



## PostScript

Gradient filled circle:

```PostScript
%!PS-Adobe-3.0
%%BoundingBox 0 0 300 300

150 150 translate 0 0 130 0 360 arc

/Pattern setcolorspace
<<      /PatternType    2
        /Shading <<
                /ShadingType    3
                /ColorSpace     /DeviceRGB
                /Coords         [-60 60 0 0 0 100]
                /Function <<
                        /FunctionType   2
                        /Domain         [0 1]
                        /C0             [1 1 1]
                        /C1             [0 0 0]
                        /N              2
                >>
        >>
>> matrix makepattern setcolor fill

showpage
%%EOF

```


=={{header|POV-Ray}}==

This is what POVray was made for. An example with a sky, surface and transparency:


```POVray

camera { location  <0.0 , .8 ,-3.0> look_at 0}

light_source{< 3,3,-3> color rgb 1}

sky_sphere { pigment{ gradient <0,1,0> color_map {[0 color rgb <.2,.1,0>][.5 color rgb 1]} scale 2}}

plane {y,-2 pigment { hexagon color rgb .7 color rgb .5 color rgb .6 }}

sphere { 0,1
  texture {
    pigment{ color rgbft <.8,1,1,.4,.4> }
    finish { phong 1 reflection {0.40 metallic 0.5} }
  }
  interior { ior 1.5}
}

```


Yields this:

[[image:PovRay-sphere.jpg]]


## Processing

3D rendering is built into Processing.


```Processing
void setup() {
  size(500,500,P3D);
  background(200);
}

void draw() {
  stroke(200);
  translate(250,250);
  lights();
  sphere(100);
}
```



## Python

===Ascii-Art===
```python
import math

shades = ('.',':','!','*','o','e','&','#','%','@')

def normalize(v):
	len = math.sqrt(v[0]**2 + v[1]**2 + v[2]**2)
	return (v[0]/len, v[1]/len, v[2]/len)

def dot(x,y):
	d = x[0]*y[0] + x[1]*y[1] + x[2]*y[2]
	return -d if d < 0 else 0

def draw_sphere(r, k, ambient, light):
	for i in range(int(math.floor(-r)),int(math.ceil(r)+1)):
		x = i + 0.5
		line = ''

		for j in range(int(math.floor(-2*r)),int(math.ceil(2*r)+1)):
			y = j/2 + 0.5
			if x*x + y*y <= r*r:
				vec = normalize((x,y,math.sqrt(r*r - x*x - y*y)))
				b = dot(light,vec)**k + ambient
				intensity = int((1-b)*(len(shades)-1))
				line += shades[intensity] if 0 <= intensity < len(shades) else shades[0]
			else:
				line += ' '

		print(line)

light = normalize((30,30,-50))
draw_sphere(20,4,0.1, light)
draw_sphere(10,2,0.4, light)
```

```txt
                                &&&&&&&&&&######
                        &&eeeeeeeeeeeeeeee&&&&&&######%%
                    &&oooo********ooooooeeeeee&&&&########%%
                  oo****!!!!!!!!******ooooooeeee&&&&########%%
              eeoo**!!!!::::::::!!!!******ooooeeee&&&&########%%%%
            ee**!!::::::....::::::!!!!!!**ooooeeee&&&&&&########%%%%
          ee**!!::..............::::!!!!****ooooeeee&&&&########%%%%%%
        ee**!!::..................::::!!!!**ooooeeee&&&&##########%%%%%%
        oo!!::....................::::!!!!****ooeeee&&&&&&########%%%%%%
      oo**::......................::::!!!!****ooeeee&&&&&&##########%%%%%%
    &&**!!::......................::::!!!!****ooeeee&&&&&&##########%%%%%%%%
    oo**!!::......................::::!!!!**ooooeeee&&&&&&##########%%%%%%%%
  &&oo!!::........................::::!!****ooooeeee&&&&&&##########%%%%%%%%%%
  ee**!!::......................::::!!!!****ooooeeee&&&&&&##########%%%%%%%%%%
  ee**!!::::..................::::::!!!!****ooooeeee&&&&############%%%%%%%%%%
  ee**!!::::................::::::!!!!****ooooeeee&&&&&&############%%%%%%%%%%
&&ee****!!::::............::::::!!!!****ooooooeeee&&&&&&##########%%%%%%%%%%%%%%
&&eeoo**!!!!::::::::::::::::::!!!!******ooooeeee&&&&&&############%%%%%%%%%%%%%%
&&eeoo****!!!!::::::::::::!!!!!!******ooooeeeeee&&&&&&############%%%%%%%%%%%%%%
&&eeoooo**!!!!!!!!!!!!!!!!!!!!******ooooeeeeee&&&&&&############%%%%%%%%%%%%%%%%
&&eeeeoo******!!!!!!!!!!********ooooooeeeeee&&&&&&&&############%%%%%%%%%%%%%%%%
##&&eeoooooo******************ooooooeeeeee&&&&&&&&############%%%%%%%%%%%%%%%%%%
##&&eeeeoooooooo******ooooooooooooeeeeee&&&&&&&&##############%%%%%%%%%%%%%%%%%%
##&&&&eeeeeeooooooooooooooooooeeeeeeee&&&&&&&&##############%%%%%%%%%%%%%%%%%%%%
  ##&&&&eeeeeeeeeeeeeeeeeeeeeeeeee&&&&&&&&&&################%%%%%%%%%%%%%%%%%%
  ####&&&&&&eeeeeeeeeeeeeeeeee&&&&&&&&&&&&################%%%%%%%%%%%%%%%%%%%%
  ######&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&##################%%%%%%%%%%%%%%%%%%%%%%
  %%######&&&&&&&&&&&&&&&&&&&&&&&&####################%%%%%%%%%%%%%%%%%%%%%%%%
    ############&&&&&&&&&&&&########################%%%%%%%%%%%%%%%%%%%%%%%%
    %%############################################%%%%%%%%%%%%%%%%%%%%%%%%%%
      %%######################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%%########################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          %%%%%%%%##############%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                %%%%%%%%%%%%%%%%

              ::..::::!!**
          ..............::!!oo
      ..................::!!**ooee
    ......................::!!**ooee
    ......................::!!**ooee
  ........................::!!**ooooee
  ........................::!!**ooooee
::........................::!!**ooooeeee
........................::!!!!**ooooeeee
::......................::!!****ooeeeeee
::....................::!!!!**ooooeeeeee
!!..................::!!!!****ooeeeeeeee
**::::..........::::!!!!****ooooeeeeeeee
  !!!!::::::::::!!!!!!****ooooeeeeeeee
  oo**!!!!!!!!!!!!******ooooeeeeeeeeee
    oo**************ooooooeeeeeeeeee
    eeooooooooooooooooeeeeeeeeeeeeee
      eeeeooooooooeeeeeeeeeeeeeeee
          eeeeeeeeeeeeeeeeeeee
              eeeeeeeeeeee

```


==={{libheader|Pygame}}===
[[File:PythonSphere.png|thumb|Python Sphere]]
Renders a sphere with random Perlin noise.

This code contains unnecessary functions which are part of a 3D graphics library I wrote.

Uses Pygame and Python 3.2.2

```python

import pygame
from pygame.locals import *
import time
import sys
import random
import math

```


```python

class Tricubic:
    def __init__(self,pts):
        self.coefficients = []
        for plane in pts:
            planecoeffs = []
            for line in plane:
                p = (line[3]-line[2])-(line[0]-line[1])
                q = (line[0]-line[1])-p
                r = line[2]-line[0]
                s = line[1]
                planecoeffs.append([p,q,r,s])
            self.coefficients.append(planecoeff)
    def Eval(at):
        return Misc.Cubic([CoeffBicubic(coeffs[0],d),CoeffBicubic(coeffs[1],d),CoeffBicubic(coeffs[2],d),CoeffBicubic(coeffs[3],d)],d.z)
    def CoeffCubic(coeffs,d):
        return (coeffs[0]*(d.x**3))+(coeffs[1]*(d.x**2))+(coeffs[2]*d.x)+coeffs[3]
    def CoeffBicubic(coeffs,d):
        return Misc.Cubic([CoeffCubic(coeffs[0],d),CoeffCubic(coeffs[1],d),CoeffCubic(coeffs[2],d),CoeffCubic(coeffs[3],d)],d.y)
class Misc:
    def LinePara(line,t):
        return Vector3.Add(line[0],Vector3.Scale(Vector3.Subtract(line[1],line[0]),t))
    def LUR(at,above):
        look = at.Unit()
        right = Vector3.Cross(look,above).Unit()
        up = Vector3.Scale(Vector3.Cross(look,right),-1)
        return [look,up,right]
    def LinePlane(line,triangle,cp=True):
        try:
            u = Vector3.Subtract(triangle.points[1].point,triangle.points[0])
            v = Vector3.Subtract(triangle.points[2],triangle.points[0])
            n = Vector3.Cross(u,v)
            r = (Vector3.Dot(n,Vector3.Subtract(triangle.points[0],line.start))/Vector3.Dot(n,line.direction))
            if stp:
                point = Vector3.Add(Vector3.Scale(line.direction,r),line.start)
                w = Vector3.Subtract(point,triangle.points[0])
                udv = Vector3.Dot(u,v)
                wdv = Vector3.Dot(w,v)
                vdv = Vector3.Dot(v,v)
                wdu = Vector3.Dot(w,u)
                udu = Vector3.Dot(u,u)
                denominator = (udv**2)-(udu*vdv)
                s = ((udv*wdv)-(vdv*wdu))/denominator
                t = ((udv*wdu)-(udu*wdv))/denominator
                return [r,Vector2(s,t),point]
                print('hooray')
            else:
                return [r]
        except:
            return None
    def Cubic(pts,d):
        p = (pts[3]-pts[2])-(pts[0]-pts[1])
        q = (pts[0]-pts[1])-p
        r = pts[2]-pts[0]
        s = pts[1]
        return (p*(d**3))+(q*(d**2))+(r*d)+s
    def Bicubic(pts,d):
        return Misc.Cubic([Misc.Cubic(pts[0],d.x),Misc.Cubic(pts[1],d.x),Misc.Cubic(pts[2],d.x),Misc.Cubic(pts[3],d.x)],d.y)
    def Tricubic(pts,d):
        return Misc.Cubic([Misc.Bicubic(pts[0],d),Misc.Bicubic(pts[1],d),Misc.Bicubic(pts[2],d),Misc.Bicubic(pts[3],d)],d.z)
    def Quadcubic(pts,d):
        return Misc.Cubic([Misc.Tricubic(pts[0],d),Misc.Tricubic(pts[1],d),Misc.Tricubic(pts[2],d),Misc.Tricubic(pts[3],d)],d.w)
    def Linear(pts,d):
        return (pts[2]*d)+(pts[1]*(1-d))
    def Bilinear(pts,d):
        return Misc.Linear([0,Misc.Linear(pts[1],d.x),Misc.Linear(pts[2],d.x)],d.y)
    def Trilinear(pts,d):
        return Misc.Linear([0,Misc.Bilinear(pts[1],d),Misc.Bilinear(pts[2],d)],d.z)
    def LP2(line,triangle,cp=True):
        try:
            bla = triangle.points[1]
            bla = triangle.points[0]
            u = Vector3.Subtract(triangle.points[1].point,triangle.points[0].point)
            v = Vector3.Subtract(triangle.points[2].point,triangle.points[0].point)
            n = Vector3.Cross(u,v)
            d = Vector3.Subtract(line[1],line[0])
            r = (Vector3.Dot(n,Vector3.Subtract(triangle.points[0].point,line[0]))/Vector3.Dot(n,d))
            if cp:
                point = Vector3.Add(Vector3.Scale(d,r),line[0])
                w = Vector3.Subtract(point,triangle.points[0].point)
                udv = Vector3.Dot(u,v)
                wdv = Vector3.Dot(w,v)
                vdv = Vector3.Dot(v,v)
                wdu = Vector3.Dot(w,u)
                udu = Vector3.Dot(u,u)
                denominator = (udv**2)-(udu*vdv)
                s = ((udv*wdv)-(vdv*wdu))/denominator
                t = ((udv*wdu)-(udu*wdv))/denominator
                return (r,Vector2(s,t),point)
            else:
                return (r)
        except:
            return None
    def Phong(normal,viewer,light,material,term):
        # light (vector_to,diffuse,specular)
        # material (ambient,diffuse,specular,shininess)
        n = normal.Unit()
        v = viewer.Unit()
        l = light[0].Unit()
        ldn = Vector3.Dot(l,n)
        #print(ldn)
        val = 0
        if ldn > 0:
            val += material[1][term]*ldn*light[1][term]
            rdv = Vector3.Dot(Vector3.Subtract(Vector3.Scale(n,2*ldn),l),v)
            if rdv > 0:
                val += (material[2][term]*(rdv**material[3])*light[2][term])
        #print(val)
        return val
    def Lighting(ambient,normal,viewer,lights,material,term):
        # lights [(vector_to,diffuse,specular)]
        # material (ambient,diffuse,specular,shininess)
        val = material[0][term]*ambient[term]
        for light in lights:
            val += Misc.Phong(normal,viewer,light,material,term)
        return val
    def Lighting2(start,direction,ambient,intersect,triangle,lights):
        coord = intersect[1]
        val = Color.Add(Color.Multiply(ambient,Color.Multiply(triangle.material.color['ambient'],triangle.Map('ambient',coord))),
                        Color.Multiply(triangle.material.color['glow'],triangle.Map('glow',coord)))
        for light in lights:
            for n in range(3):
                val[n] += Misc.Phong(triangle.InterpolatedNormal(coord),
                                     Vector3.Scale(direction,-1),
                                     (light.To(intersect[2]),light.Diffuse(intersect[2]),light.Specular(intersect[2])),
                                     (Color(),
                                      Color.Multiply(triangle.material.color['diffuse'],triangle.Map('diffuse',coord)),
                                      Color.Multiply(triangle.material.color['specular'],triangle.Map('specular',coord)),
                                      triangle.material.shiny),n)
        return val
    def Ray(start,direction,scene,color=True,sector=None):
        intersect = None
        intersected = None
        col = None
        for triangle in scene.triangles:
            possible = True
            if sector != None:
                possible = False
                for point in triangle.points:
                    if not(point.sector.x < sector.x):
                        possible = True
                if possible:
                    possible = False
                    for point in triangle.points:
                        if not(point.sector.x > sector.x):
                            possible = True
                if possible:
                    possible = False
                    for point in triangle.points:
                        if not(point.sector.y < sector.y):
                            possible = True
                if possible:
                    possible = False
                    for point in triangle.points:
                        if not(point.sector.y > sector.y):
                            possible = True
            possible = True
            if possible:
                tmp = Misc.LP2([start,Vector3.Add(start,direction)],triangle,color)
                write = False
                if type(tmp) == type(5.1):
                    tmp = None
                if (tmp != None):
                    if (intersect == None):
                        if (tmp[0] > 0) and (tmp[1].x >= 0) and (tmp[1].y >= 0) and (tmp[1].x+tmp[1].y <= 1):
                            write = True
                    elif (tmp[0] > 0) and (tmp[0] < intersect[0]) and (tmp[1].x >= 0) and (tmp[1].y >= 0) and (tmp[1].x+tmp[1].y <= 1):
                        write = True
                if write:
                    intersect = tmp
                    intersected = triangle
        if color and (intersect != None):
            applicable = []
            for light in scene.lights:
                block = Misc.Ray(intersect[2],light.To(intersect[2]),scene,False)
                if block == None:
                    applicable.append(light)
                elif light.location != None:
                    if Vector3.Subtract(light.location,intersect[2]).Magnitude() < block[0]:
                        applicable.append(light)
            col = Misc.Lighting2(start,direction,scene.ambient,intersect,intersected,applicable)
            return (intersect,col)
        else:
            return intersect
class DirLight:
    def __init__(self,direction,diffuse,specular):
        self.location = None
        self.direction = direction.Unit()
        self.diffuse = diffuse
        self.specular = specular
    def To(self,frm):
        return Vector3.Scale(self.direction,-1)
    def Diffuse(self,to):
        return self.diffuse
    def Specular(self,to):
        return self.specular
class Material:
    def __init__(self):
        self.color = {'ambient':Color(1,1,1),
                      'diffuse':Color(1,1,1),
                      'specular':Color(1,1,1),
                      'glow':Color(1,1,1)}
        self.maps = {'ambient':Map(),
                     'diffuse':Map(),
                     'specular':Map(),
                     'glow':Map(),
                     'bump':Map()}
        self.shiny = 10
class Map:
    def __init__(self,surface=None):
        self.surface = surface
        if self.surface != None:
            self.width = self.surface.get_width()
            self.height = self.surface.get_height()
    def __getitem__(self,index):
        if self.surface == None:
            return Color(1,1,1)
        else:
            try:
                return Color.From255(self.surface.get_at((int(index.x*(self.width-1)),int(index.y*(self.height-1)))))
            except:
                return Color(0,0,1)
class Color:
    def __init__(self,r=0,g=0,b=0):
        self.r = r
        self.g = g
        self.b = b
    def __getitem__(self,index):
        if index == 0:
            return self.r
        elif index == 1:
            return self.g
        elif index == 2:
            return self.b
    def __setitem__(self,index,value):
        if index == 0:
            self.r = value
        elif index == 1:
            self.g = value
        elif index == 2:
            self.b = value
    def Multiply(A,B):
        return Color(A.r*B.r,A.g*B.g,A.b*B.b)
    def Add(A,B):
        return Color(A.r+B.r,A.g+B.g,A.b+B.b)
    def From255(A):
        return Color(A.r/255,A.g/255,A.b/255)
class Vertex:
    def __init__(self,point,normal,maps):
        self.bpoint = point
        self.bnormal = normal
        self.maps = maps
        for name in ['ambient','diffuse','specular','glow','bump']:
            try:
                bla = self.maps[name]
            except:
                self.maps[name] = Vector2()
        self.sector = None
    def Transform(self,points,norms):
        self.point = Matrix2.Multiply(self.bpoint.Horizontal(),points).Vectorize()
        self.normal = Matrix2.Multiply(self.bnormal.Horizontal(),norms).Vectorize()
class Triangle:
    def __init__(self,vertices,material=Material()):
        self.points = vertices
        self.material = material
    def Map(self,name,coord):
        pts = []
        for n in range(3):
            pts.append(self.points[n].maps[name])
        loc = Vector2.Add(pts[0],
                          Vector2.Add(Vector2.Scale(Vector2.Subtract(pts[1],pts[0]),coord.x),
                                      Vector2.Scale(Vector2.Subtract(pts[2],pts[0]),coord.y)))
        #print(loc.x,loc.y)
        return self.material.maps[name][loc]
    def InterpolatedNormal(self,coord):
        return Vector3.Add(Vector3.Scale(self.points[0].normal,1-coord.x-coord.y),
                           Vector3.Add(Vector3.Scale(self.points[1].normal,coord.x),Vector3.Scale(self.points[2].normal,coord.y))).Unit()
class Line:
    def __init__(self,A,B=None,direction=None):
        self.start = A
        if B != None:
            self.direction = Vector3.Subtract(B,A).Unit()
        elif direction != None:
            self.direction = direction
        else:
            raise RuntimeError('Neither B nor direction are specified')
class Scene:
    def __init__(self):
        self.triangles = []
        self.vertices = []
        self.lights = []
        self.exterior = []
        self.ambient = 0
class Matrix2:
    def __init__(self,data=[[]]):
        self.FromData(data)
    def __getitem__(self,index):
        return self.data[index[1]][index[0]]
    def __setitem__(self,index,value):
        self.data[index[1]][index[0]]=value
    def Dimension(self):
        self.rows = len(self.data)
        self.cols = len(self.data[0])
    def FromData(self,data):
        self.data = data
        length=len(data[0])
        for row in data:
            if len(row)!=length:
                self.data=None
                raise RuntimeError('Data rows are not of uniform length.')
        self.Dimension()
    def Multiply(A,B):
        if A.cols!=B.rows:
            raise RuntimeError('Column count of Matrix2 \"A\" does not match row count of Matrix2 \"B\".')
        matrix = Matrix2.Empty(B.cols,A.rows)
        x=0
        while x<matrix.cols:
            y=0
            while y<matrix.rows:
                val=0
                n=0
                while n<A.cols:
                    val+=A[(n,y)]*B[(x,n)]
                    n+=1
                matrix[(x,y)]=val
                y+=1
            x+=1
        return matrix
    def Scalar(A,n):
        pass
    def Empty(rows,cols):
        data = []
        row = [0]*rows
        n = 0
        while n < cols:
            data.append(row[:])
            n+=1
        matrix=Matrix2(data)
        matrix.Dimension()
        return matrix
    def Identity(cols):
        matrix = Matrix2.Empty(cols,cols)
        n = 0
        while n < cols:
            matrix[(n,n)]=1
            n += 1
        return matrix
    def Vectorize(self):
        if self.cols==1:
            if self.rows!=4:
                raise RuntimeError('Only 1 by 4 or 4 by 1 Matrix2s can be cast to Vector3s.')
            vertical=True
        elif self.rows==1:
            if self.cols!=4:
                raise RuntimeError('Only 1 by 4 or 4 by 1 Matrix2s can be cast to Vector3s.')
            vertical = False
        else:
            raise RuntimeError('Only 1 by 4 or 4 by 1 Matrix2s can be cast to Vector3s.')
        vector=[0]*4
        n=0
        while n<4:
            if vertical:
                vector[n]=self[(0,n)]
            else:
                vector[n]=self[(n,0)]
            n+=1
        return Vector3(vector[0],vector[1],vector[2],vector[3])
    def Print(self,decimals,spaces):
        length=0
        for row in self.data:
            for val in row:
                string=str(round(val,decimals))
                if length<len(string):
                    length=len(string)
        text=''
        for row in self.data:
            temp=''
            for value in row:
                val=str(round(float(value),decimals))
                pads=length-len(val)
                pad=int(pads/2)
                temp+=(' '*pad)+val+(' '*(pads-pad))+(' '*spaces)
            text+=(' '*spaces)+temp[0:len(temp)-1]+(' '*spaces)+'\n'
        return(text[0:len(text)-1])
    def RotX(angle):
        return Matrix2([
            [1,0,0,0],
            [0,math.cos(angle),0-math.sin(angle),0],
            [0,math.sin(angle),math.cos(angle),0],
            [0,0,0,1]])
    def RotY(angle):
        return Matrix2([
            [math.cos(angle),0,0-math.sin(angle),0],
            [0,1,0,0],
            [math.sin(angle),0,math.cos(angle),0],
            [0,0,0,1]])
    def RotZ(angle):
        return Matrix2([
            [math.cos(angle),0-math.sin(angle),0,0],
            [math.sin(angle),math.cos(angle),0,0],
            [0,0,1,0],
            [0,0,0,1]])
    def Translate(vector):
        return Matrix2([
            [1,0,0,0],
            [0,1,0,0],
            [0,0,1,0],
            [vector.x,vector.y,vector.z,1]])
    def Scale(vector):
        return Matrix2([
            [vector.x,0,0,0],
            [0,vector.y,0,0],
            [0,0,vector.z,0],
            [0,0,0,1]])
    def Clone(self):
        data = []
        for row in self.data:
            data.append(row[:])
        return Matrix2(data)
    def Inverse(self):
        adjoint = self.Adjoint()
        det = self.Determinant()
        if det == 0:
            raise RuntimeError('Cannot find the inverse of a matrix with a determinant of 0')
        inverse = Matrix2.Empty(self.rows,self.cols)
        x = 0
        while x < self.cols:
            y = 0
            while y < self.rows:
                inverse[(x,y)] = adjoint[(x,y)]/det
                y += 1
            x += 1
        return inverse
    def Transpose(self):
        transpose = Matrix2.Empty(self.cols,self.rows)
        x = 0
        while x < self.cols:
            y = 0
            while y < self.rows:
                transpose[(y,x)] = self[(x,y)]
                y += 1
            x += 1
        return transpose
    def Adjoint(self):
        return self.Cofactors().Transpose()
    def Determinant(self):
        if self.rows != self.cols:
            raise RuntimeError('Cannot find the determinant of a non-square matrix')
        if self.rows == 1:
            return self[(0,0)]
        cofactors = self.Cofactors()
        determinant = 0
        n = 0
        while n < self.cols:
            determinant += self[(n,0)]*cofactors[(n,0)]
            n += 1
        return determinant
    def Minors(self):
        if self.rows != self.cols:
            raise RuntimeError('Cannot find the minors of a non-square matrix')
        if self.rows == 1:
            raise RuntimeError('Cannot find the minors of a 1 by 1 matrix')
        minors = Matrix2.Empty(self.rows,self.cols)
        lines = range(self.rows)
        x = 0
        while x < self.cols:
            y = 0
            while y < self.cols:
                tiny = Matrix2.Empty(self.rows-1,self.cols-1)
                ox = 0
                nx = 0
                while ox < self.cols:
                    oy = 0
                    ny = 0
                    while oy < self.cols:
                        if not((ox == x) or (oy == y)):
                            tiny[(nx,ny)] = self[(ox,oy)]
                        if oy != y:
                            ny += 1
                        oy += 1
                    if ox != x:
                        nx += 1
                    ox += 1
                minors[(x,y)] = tiny.Determinant()
                y += 1
            x += 1
        return minors
    def Cofactors(self):
        minors = self.Minors()
        cofactors = Matrix2.Empty(self.rows,self.cols)
        x = 0
        while x < self.cols:
            y = 0
            while y < self.rows:
                if int((x+y)/2) == ((x+y)/2):
                    cofactors[(x,y)] = minors[(x,y)]
                else:
                    cofactors[(x,y)] = -1*minors[(x,y)]
                y += 1
            x += 1
        return cofactors
    def Perspective(e):
        return Matrix2([
            [1,0,0,0],
            [0,1,0,0],
            [0,0,1,1/e[2]],
            [-e[0],-e[1],0,0]])
    def Add(A,B):
        if A.rows != B.rows:
            RuntimeError('The row counts of Matrix \"A\" and Matrix \"B\" are not identical.')
        if A.cols != B.cols:
            RuntimeError('The column counts of Matrix \"A\" and Matrix \"B\" are not identical.')
        matrix = Matrix.Empty(A.rows,A.cols)
        for x in range(A.cols):
            for y in range(A.rows):
                matrix[(x,y)] = A[(x,y)]+B[(x,y)]
        return matrix
    def Subtract(A,B):
        if A.rows != B.rows:
            RuntimeError('The row counts of Matrix \"A\" and Matrix \"B\" are not identical.')
        if A.cols != B.cols:
            RuntimeError('The column counts of Matrix \"A\" and Matrix \"B\" are not identical.')
        matrix = Matrix.Empty(A.rows,A.cols)
        for x in range(A.cols):
            for y in range(A.rows):
                matrix[(x,y)] = A[(x,y)]+B[(x,y)]
        return matrix
    def DivHomogeneous(self):
        if (self.cols,self.rows) == (1,4):
            for y in range(3):
                self[(0,y)] = self[(0,y)]/self[(0,3)]
            self[(0,3)] = 1
        if (self.cols,self.rows) == (4,1):
            for x in range(3):
                self[(x,0)] = self[(x,0)]/self[(3,0)]
            self[(3,0)] = 1
        else:
            raise RuntimeError('1 by 4 or 4 by 1 Matrix2 expected')
    def Object(pos,look,up,right):
        return Matrix2([
            [right.x,right.y,right.z,0],
            [up.x,up.y,up.z,0],
            [look.x,look.y,look.z,0],
            [pos.x,pos.y,pos.z,1]])
    def Camera(eye,look,up,right):
        return Matrix2([
            [right.x,up.x,look.x,0],
            [right.y,up.y,look.y,0],
            [right.z,up.z,look.z,0],
            [-Vector3.Dot(eye,right),
             -Vector3.Dot(eye,up),
             -Vector3.Dot(eye,look),1]])
    def YPR(rot):
        return Matrix2.Multiply(
            Matrix2.Multiply(Matrix2.RotZ(rot.z),
                             Matrix2.RotX(rot.x)),
            Matrix2.RotY(rot.y))
class Vector2:
    def __init__(self,data=0,y=0):
        if (type(data) == type(5)) or (type(data) == type(5.1)):
            self.x = data
            self.y = y
        else:
            self.x = data[0]
            self.y = data[1]
    def __getitem__(self,index):
        if index == 0:
            return self.x
        elif index == 1:
            return self.y
    def __setitem__(self,index,value):
        if index == 0:
            self.x = value
        elif index == 1:
            self.y = 1
    def Add(A,B):
        return Vector2(A.x+B.x,A.y+B.y)
    def Subtract(A,B):
        return Vector2(A.x-B.x,A.y-B.y)
    def Scale(A,n):
        return Vector2(A.x*n,A.y*n)
    def Magnitude(self):
        return ((self.x**2)+(self.y**2))**.5
    def Unit(self):
        return Vector2.Scale(self,1/self.Magnitude())
    def Clone(self):
        return Vector2(self.x,self.y)
class Vector3:
    def __init__(self,data=0,y=0,z=0,w=1):
        if (type(data) == type(5)) or (type(data) == type(5.1)):
            self.x = data/w
            self.y = y/w
            self.z = z/w
        else:
            try:
                temp = data[3]
            except:
                temp = 1
            self.x = data[0]/temp
            self.y = data[1]/temp
            self.z = data[2]/temp
    def __getitem__(self,index):
        if index == 0:
            return self.x
        elif index == 1:
            return self.y
        elif index == 2:
            return self.z
    def __setitem__(self,index,value):
        if index == 0:
            self.x = value
        elif index == 1:
            self.y = value
        elif index == 2:
            self.z = value
    def Vertical(self):
        return Matrix2([[self.x],[self.y],[self.z],[1]])
    def Horizontal(self):
        return Matrix2([[self.x,self.y,self.z,1]])
    def Dot(A,B):
        return (A.x*B.x)+(A.y*B.y)+(A.z*B.z)
    def Cross(A,B):
        return Vector3([
            (A.y*B.z)-(A.z*B.y),
            (A.z*B.x)-(A.x*B.z),
            (A.x*B.y)-(A.y*B.x)])
    def Add(A,B):
        return Vector3(A.x+B.x,A.y+B.y,A.z+B.z)
    def Subtract(A,B):
        return Vector3(A.x-B.x,A.y-B.y,A.z-B.z)
    def Scale(A,n):
        return Vector3(A.x*n,A.y*n,A.z*n)
    def Magnitude(self):
        return ((self.x**2)+(self.y**2)+(self.z**2))**.5
    def Print(self,decimals,spaces):
        return self.Horizontal().Print(decimals,spaces)
    def Same(A,B):
        same = False
        if A.x == B.x:
            if A.y == B.y:
                if A.z == B.z:
                    same = True
        return same
    def Unit(self):
        return Vector3.Scale(self,1/self.Magnitude())
    def Clone(self):
        return Vector3(self.x,self.y,self.z)
class Vector4:
    def __init__(self,data=0,y=0,z=0,w=0):
        if (type(data) == type(5)) or (type(data) == type(5.1)):
            self.x = data
            self.y = y
            self.z = z
            self.w = w
        else:
            self.x = data[0]
            self.y = data[0]
            self.z = data[0]
            self.w = data[0]

points = [Vector3([-1,-1,0]),Vector3([1,-1,0]),Vector3([0,1,0])]
width = 255
height = width
screen = pygame.display.set_mode((width,height),0,32)
scl = 2
pos =  Vector3([0,0,5])
view = Vector3([0,0,1])
frames = 0

def Transform(point,mat):
   return Matrix2.Multiply(point.Horizontal(),mat).Vectorize()

def RV():
   return Vector3([random.random(),random.random(),random.random()])

green = pygame.Color(0,255,0)
def XY(bla):
   return (((width*bla[0])+width)/2,((height*bla[1])+width)/2)

screen.fill(pygame.Color(0,0,0))
size = 255

world = Matrix2.Identity(4)
inv = world.Inverse()
invt = world.Inverse().Transpose()
center = Vector3(0,0,2)


def Texture(size):
   texture = []
   for pa in range(size):
      plane = []
      for pb in range(size):
         line = []
         for pc in range(size):
            line.append(random.random())
         plane.append(line)
      texture.append(plane)
   return texture

lights = [(Vector3(-10,6,-9),[.7,.7*.9,.7*.8],[.7,.7*.9,.9*.8])]
lights = [(Vector3(-10,6,-9),[.8,.8,.8],[.7,.7,.7])]

depth = 3
groups = []
for n in range(1):
   textures = []
   for n in range(depth):
      textures.append(Texture(4**(n+1)))
   groups.append(textures)

def Select(texture,at):
   sel = []
   for pa in range(4):
      aplane = texture[pa+math.floor(at.z)]
      bplane = []
      for pb in range(4):
         aline = aplane[pb+math.floor(at.y)]
         bline = []
         for pc in range(4):
            bline.append(aline[pc+math.floor(at.x)])
         bplane.append(bline)
      sel.append(bplane)
   return (sel,Vector3(at.x%1,at.y%1,at.z%1))
def Round(val):
   return val-(val-math.floor(val))

theta = math.tan(70*math.pi/360)
for x in range(width):
   for event in pygame.event.get():
      if event.type == QUIT:
         pygame.quit()
         sys.exit()
      if event.type == KEYDOWN:
         pass
   for y in range(height):
      l = Vector3(theta*2*((x/width)-.5),theta*2*((y/width)-.5),1).Unit()
      ldc = Vector3.Dot(l,center)
      d = ldc-(((ldc**2)-Vector3.Dot(center,center)+1)**.5)
      if type(d) != type((-1)**.5):
         intersection = Vector3.Scale(l,d)
         normal = Vector3.Subtract(intersection,center).Unit()
         point = Transform(normal,world)

         s = Vector3.Scale(Vector3.Add(point,Vector3(1,1,1)),.5)
         val = 0
         for i in range(depth):
            sel = Select(groups[0][i],Vector3.Scale(s,4**i))
            val += Misc.Tricubic(sel[0],sel[1])*((1/2)**i)/4

         val = (25*val)%1
         vals = [0,Misc.Linear([0,.3,1],val),1]

         coloring = []
         for i in range(3):
            #light = Misc.Lighting([1,1,1],normal,Vector3.Scale(intersection,-1),lights,([0,.03*val,.03],[0,.7*val,.7],[.3,.3,.3],7),i)
            light = Misc.Lighting([.1,.1,.1],normal,Vector3.Scale(intersection,-1),lights,(vals,vals,[1,1,1],10),i)
            if light > 1:
               light = 1
            elif light < 0:
               light = 0
            coloring.append(round(255*light))
         screen.set_at((x,height-y),pygame.Color(coloring[0],coloring[1],coloring[2]))
   pygame.display.update()
pygame.image.save(screen,"PythonSphere.png")
while True:
   for event in pygame.event.get():
      if event.type == QUIT:
         pygame.quit()
         sys.exit()
      if event.type == KEYDOWN:
         pass

```


==={{libheader|VPython}}===
'''Short version''':

```python
from visual import *
scene.title = "VPython: Draw a sphere"
sphere()    # using defaults, see http://www.vpython.org/contents/docs/defaults.html
```


'''Regular version''', with some window-dressing:

```python

from __future__ import print_function, division
from visual import *

title = "VPython: Draw a sphere"
scene.title = title
print( "%s\n" % title )

print( 'Drag with right mousebutton to rotate view'  )
print( 'Drag up+down with middle mousebutton to zoom')

scene.autocenter = True

# uncomment any (or all) of those variants:
S1 = sphere(pos=(0.0, 0.0, 0.0), radius=1.0, color=color.blue)
#S2 = sphere(pos=(2.0, 0.0, 0.0), radius=1.0, material=materials.earth)
#S3 = sphere(pos=(0.0, 2.0, 0.0), radius=1.0, material=materials.BlueMarble)
#S4 = sphere(pos=(0.0, 0.0, 2.0), radius=1.0,
#            color=color.orange, material=materials.marble)

while True:                 # Animation-loop
    rate(100)
    pass                    # no animation in this demo

```



## Racket


[[File:Racket-sphere-plot.png|200px|thumb|right]]

Using the Typed Racket language with the plot library:


```racket

#lang typed/racket

(require plot/typed)
(plot3d (polar3d (λ (θ ρ) 1)) #:altitude 25)

```



## REXX

This program is modeled after the   '''C'''   version.

The REXX language doesn't have a   '''SQRT'''   function, so a version is included here.

Same with the   '''CEIL'''ing   and   '''FLOOR'''   functions.

Programming note:   the output will appear slightly different when executed on an EBCDIC machine   (due to different dithering characters).

```rexx
/*REXX program expresses a  lighted sphere  with  simple characters  used for shading.  */
call drawSphere  19,  4,   2/10,  '30 30 -50'    /*draw a sphere with a radius of  19.  */
call drawSphere  10,  2,   4/10,  '30 30 -50'    /*  "  "    "     "  "    "    "  ten. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ceil:  procedure;   parse arg x;  _= trunc(x);                    return _ +(x>0) * (x\=_)
floor: procedure;   parse arg x;  _= trunc(x);                    return _ -(x<0) * (x\=_)
norm:  parse arg $a $b $c;        _= sqrt($a**2 + $b**2 + $c**2); return  $a/_  $b/_  $c/_
/*──────────────────────────────────────────────────────────────────────────────────────*/
drawSphere: procedure;  parse arg r, k, ambient, lightSource /*obtain the four arguments*/
       if 8=='f8'x  then shading= ".:!*oe&#%@"               /* EBCDIC dithering chars. */
                    else shading= "·:!°oe@░▒▓"               /* ASCII      "       "    */
       parse value  norm(lightSource)    with    s1  s2  s3  /*normalize light source.  */
       shadeLen= length(shading) - 1;    rr= r**2;   r2= r+r /*handy─dandy variables.   */

         do   i=floor(-r )  to ceil(r );   x= i       + .5;       xx= x**2;       $=
           do j=floor(-r2)  to ceil(r2);   y= j * .5  + .5;       yy= y**2;       z= xx+yy
           if z<=rr  then do                                 /*is point within sphere ? */
                          parse value  norm(x  y  sqrt(rr - xx - yy) )   with   v1  v2  v3
                          dot= min(0, s1*v1 + s2*v2 + s3*v3) /*the dot product of above.*/
                          b= -dot**k  +  ambient             /*calculate the brightness.*/
                          if b<=0  then brite= shadeLen
                                   else brite= max(0,  (1-b) * shadeLen)  % 1
                          $= $ || substr(shading, brite + 1,  1)
                          end                                /* [↑]  build display line.*/
                     else $= $' '                            /*append a blank to line.  */
           end   /*j*/                                       /*[↓] strip trailing blanks*/
         say strip($, 'T')                                   /*show a line of the sphere*/
         end     /*i*/                                       /* [↑]  display the sphere.*/
       return
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt:  procedure; parse arg x;  if x=0  then return 0; d= digits(); numeric digits; h= d+6
       numeric form; m.=9; parse value format(x,2,1,,0) 'E0' with g "E" _ .; g= g*.5'e'_%2
         do j=0  while h>9;      m.j=h;              h=h%2+1;        end  /*j*/
         do k=j+5  to 0  by -1;  numeric digits m.k; g=(g+x/g)*.5;   end  /*k*/;  return g
```

(Shown at   <big>'''<sup>1</sup>/<sub>2</sub>'''</big>   size.)

<pre style="font-size:50%">
                             eeeeeeeeee@@@@@@@
                       eoooooooooooooooeeeee@@@@@@@░
                   oo°°°!!!!!!!!°°°°°°ooooeeeee@@@@@@@░░
                o°°!!!:::::::::!!!!!°°°°°ooooeeee@@@@@@@░░░
             o°!!::::·········:::::!!!!°°°ooooeeeee@@@@@@@░░░░
           o°!:::················::::!!!°°°°oooeeeee@@@@@@@░░░░░
         o°!::····················::::!!!°°°°oooeeeee@@@@@@@░░░░░░
       o°!::·······················:::!!!!°°°ooooeeee@@@@@@@@░░░░░░░
      o°!::·························:::!!!°°°ooooeeeee@@@@@@@░░░░░░░░
     o°!:···························:::!!!°°°°oooeeeee@@@@@@@@░░░░░░░░
    o°!::··························::::!!!°°°ooooeeeee@@@@@@@@░░░░░░░░░
   o°!::···························:::!!!!°°°ooooeeeee@@@@@@@@░░░░░░░░░░
  o°!!::··························::::!!!°°°°ooooeeeee@@@@@@@@░░░░░░░░░░░
 eo°!!::·························::::!!!°°°°ooooeeeee@@@@@@@@@░░░░░░░░░░░░
 oo°!!::························::::!!!°°°°ooooeeeeee@@@@@@@@░░░░░░░░░░░░░
eoo°!!:::·····················::::!!!!°°°°oooooeeeee@@@@@@@@@░░░░░░░░░░░░░░
eoo°°!!:::·················:::::!!!!!°°°°ooooeeeeee@@@@@@@@@░░░░░░░░░░░░░░░
eoo°°!!!:::::··········:::::::!!!!!°°°°oooooeeeeee@@@@@@@@@@░░░░░░░░░░░░░░░
eeoo°°°!!!:::::::::::::::::!!!!!!°°°°°oooooeeeeee@@@@@@@@@@░░░░░░░░░░░░░░░░
eeooo°°°!!!!!!!:::::::!!!!!!!!°°°°°°oooooeeeeee@@@@@@@@@@@░░░░░░░░░░░░░░░░░
@eeooo°°°°°!!!!!!!!!!!!!!!°°°°°°°ooooooeeeeeee@@@@@@@@@@@░░░░░░░░░░░░░░░░░░
@@eeeoooo°°°°°°°°°°°°°°°°°°°°°oooooooeeeeeee@@@@@@@@@@@@░░░░░░░░░░░░░░░░░░░
@@@eeeoooooo°°°°°°°°°°°°°oooooooooeeeeeeee@@@@@@@@@@@@░░░░░░░░░░░░░░░░░░░░░
 @@@eeeeeooooooooooooooooooooooeeeeeeeee@@@@@@@@@@@@@░░░░░░░░░░░░░░░░░░░░░
 @@@@@eeeeeeeooooooooooooeeeeeeeeeeee@@@@@@@@@@@@@@░░░░░░░░░░░░░░░░░░░░░░░
  @@@@@@eeeeeeeeeeeeeeeeeeeeeeeee@@@@@@@@@@@@@@@@░░░░░░░░░░░░░░░░░░░░░░░░
   @@@@@@@@@eeeeeeeeeeeeeeeee@@@@@@@@@@@@@@@@@@░░░░░░░░░░░░░░░░░░░░░░░░░
    ░@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@░░░░░░░░░░░░░░░░░░░░░░░░░░░
     ░░@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
      ░░░░@@@@@@@@@@@@@@@@@@@@@@@@@@@░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
       ░░░░░░░@@@@@@@@@@@@@@@@@@░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
         ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
           ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
             ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
                ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
                   ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
                       ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
                             ░░░░░░░░░░░░░░░░░

             ::···:::!!!°o
         ··············::!!°oo
      ··················::!!°°ooe
    ·····················::!!°°ooee
   ·······················::!!°°ooee
  ························::!!°°oooee
 ·························::!!°°oooeee
:························::!!!°°oooeeee
························::!!!°°ooooeeee
:······················::!!!°°°oooeeeee
:····················:::!!!°°°oooeeeeee
!:·················:::!!!°°°°oooeeeeeee
°!:::···········::::!!!!°°°ooooeeeeeeee
 °!!!:::::::::::!!!!!°°°°oooooeeeeeeee
  o°°!!!!!!!!!!!!!°°°°°oooooeeeeeeeee
   oo°°°°°°°°°°°°°°ooooooeeeeeeeeeee
    eoooooooooooooooooeeeeeeeeeeeee
      eeeooooooooeeeeeeeeeeeeeeee
         eeeeeeeeeeeeeeeeeeeee
             eeeeeeeeeeeee

```



## Ruby

Shoes comes with this sample program.
[[File:sphere.shoes.png|thumb|right]]

```ruby>Shoes.app :width =
 500, :height => 500, :resizable => false do
  image 400, 470, :top => 30, :left => 50 do
    nostroke
    fill "#127"
    image :top => 230, :left => 0 do
      oval 70, 130, 260, 40
      blur 30
    end
    oval 10, 10, 380, 380
    image :top => 0, :left => 0 do
      fill "#46D"
      oval 30, 30, 338, 338
      blur 10
    end
    fill gradient(rgb(1.0, 1.0, 1.0, 0.7), rgb(1.0, 1.0, 1.0, 0.0))
    oval 80, 14, 240, 176
    image :top => 0, :left => 0 do
      fill "#79F"
      oval 134, 134, 130, 130
      blur 40
    end
    image :top => 150, :left => 40, :width => 320, :height => 260 do
      fill gradient(rgb(0.7, 0.9, 1.0, 0.0), rgb(0.7, 0.9, 1.0, 0.6))
      oval 60, 60, 200, 136
      blur 20
    end
  end
end
```



## Scala


```Scala
object Sphere extends App {
  private val (shades, light) = (Seq('.', ':', '!', '*', 'o', 'e', '&', '#', '%', '@'), Array(30d, 30d, -50d))

  private def drawSphere(r: Double, k: Double, ambient: Double): Unit = {
    def dot(x: Array[Double], y: Array[Double]) = {
      val d = x.head * y.head + x(1) * y(1) + x.last * y.last
      if (d < 0) -d else 0D
    }

    for (i <- math.floor(-r).toInt to math.ceil(r).toInt; x = i + .5)
      println(
        (for (j <- math.floor(-2 * r).toInt to math.ceil(2 * r).toInt; y = j / 2.0 + .5)
          yield if (x * x + y * y <= r * r) {

            def intensity(vec: Array[Double]) = {
              val b = math.pow(dot(light, vec), k) + ambient
              if (b <= 0) shades.length - 2
              else math.max((1 - b) * (shades.length - 1), 0).toInt
            }

            shades(intensity(normalize(Array(x, y, scala.math.sqrt(r * r - x * x - y * y)))))
          } else ' ').mkString)
  }

  private def normalize(v: Array[Double]): Array[Double] = {
    val len = math.sqrt(v.head * v.head + v(1) * v(1) + v.last * v.last)
    v.map(_ / len)
  }

  normalize(light).copyToArray(light)
  drawSphere(20, 4, .1)
  drawSphere(10, 2, .4)

}
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/uSm8bJ9/0 ScalaFiddle (JavaScript)] or by [https://scastie.scala-lang.org/TtVHUp3aS0eDEB6YCW4gKg Scastie (JVM)].


## Sidef

Produces a PGM image.

```ruby
func normalize (vec)  { vec »/» (vec »*« vec -> sum.sqrt) }
func dot       (x, y) { -(x »*« y -> sum) `max` 0 }
 
var x = var y = 255
x += 1 if x.is_even    # must be odd
 
var light = normalize([ 3, 2, -5 ])
var depth = 255
 
func draw_sphere(rad, k, ambient) {
    var pixels = []
    var r2 = (rad * rad)
    var range = (-rad .. rad)
    for x,y in (range ~X range) {
        if ((var x2 = x*x) + (var y2 = y*y) < r2) {
            var vector = normalize([x, y, (r2 - x2 - y2).sqrt])
            var intensity = (dot(light, vector)**k + ambient)
            var pixel = (0 `max` (intensity*depth -> int) `min` depth)
            pixels << pixel
        }
        else {
            pixels << 0
        }
    }
    return pixels
}
 
var outfile = %f'sphere-sidef.pgm'
var out = outfile.open('>:raw')
 
out.say("P5\n#{x} #{y}\n#{depth}")    # .pgm header
out.print(draw_sphere((x-1)/2, .9, .2).map{.chr}.join)
out.close
```



## Smalltalk


there are various OpenGL bindings available; here is a translation of the bare-bones code from C/Go:

although there is a Point3 class in some loadable library, here is some self contained code, defining a local anon Point3D class.

```Smalltalk

Point3D :=
   Point subclass:#Point3D
        instanceVariableNames:'z'
        classVariableNames:''
        poolDictionaries:''
        category:''
        inEnvironment:nil.

Point3D compile:'z ^ z'.
Point3D compile:'z:v z := v'.

normalize := [:v | |invLen|
    invLen := 1 / (dot value:v value:v) sqrt.
    v x: v x * invLen.
    v y: v y * invLen.
    v z: v z * invLen.
].

dot := [:a :b |
    (a x * b x) + (a y * b y) + (a z * b z)
].

drawSphere := [:r :k :amb :dir |
    |w h imh vec img|

    w := r*4. h := r*3.
    img := Image width:w height:h depth:8.
    img photometric:#blackIs0; createPixelStore.
    vec := Point3D new.
    0-r to:r do:[:x |
        0-r to:r do:[:y |
            |z s lum|
            (z := (r*r) - (x*x) - (y*y)) >= 0 ifTrue:[
                vec x: x.
                vec y: y.
                vec z: z sqrt.
                normalize value:vec.
                s := dot value:dir value:vec.
                s < 0 ifTrue:[ s := 0 ].
                lum := 255 * ((s raisedTo: k) + amb) / (1 + amb).
                lum < 0 ifTrue:[
                    lum := 0
                ] ifFalse:[ lum > 255 ifTrue:[
                    lum := 255
                ]].
                img atX:(x+(w//2)) y:(y+(h//2)) put:(Color greyByte:lum).
            ]
        ]
    ].
    img
].

main := [
    |dir img|

    dir := Point3D new x:-30; y:-30; z:50; yourself.
    normalize value:dir.
    img := drawSphere value: 100 value: 1.5 value: 0.2 value: dir.
    img displayOn:(View new extent:400@400; openAndWait).
    img saveOn:'sphere.png'.
].

main value.

```

[[file:sphere-smalltalk.png]]


## SVG

Not quite a sphere.
[[file:sphere-ish.svg]]


## Swift

In Playground for example:

```Swift

class Sphere: UIView{

  override func drawRect(rect: CGRect)
  {
    let context = UIGraphicsGetCurrentContext()
    let locations: [CGFloat] = [0.0, 1.0]

    let colors = [UIColor.whiteColor().CGColor,
      UIColor.blueColor().CGColor]

    let colorspace = CGColorSpaceCreateDeviceRGB()

    let gradient = CGGradientCreateWithColors(colorspace,
      colors, locations)

    var startPoint = CGPoint()
    var endPoint = CGPoint()
    startPoint.x = self.center.x - (self.frame.width * 0.1)
    startPoint.y = self.center.y - (self.frame.width * 0.15)
    endPoint.x = self.center.x
    endPoint.y = self.center.y
    let startRadius: CGFloat = 0
    let endRadius: CGFloat = self.frame.width * 0.38

    CGContextDrawRadialGradient (context, gradient, startPoint,
      startRadius, endPoint, endRadius,
      0)
  }
}

var s = Sphere(frame: CGRectMake(0, 0, 200, 200))

```




## Tcl

Assuming the task is to draw a <i>likeness</i> of a sphere, this would usually do:


```Tcl
proc grey {n} {format "#%2.2x%2.2x%2.2x" $n $n $n}

pack [canvas .c -height 400 -width 640 -background white]

for {set i 0} {$i < 255} {incr i} {
  set h [grey $i]
  .c create arc [expr {100+$i/5}] [expr {50+$i/5}] [expr {400-$i/1.5}] [expr {350-$i/1.5}] \
                 -start 0 -extent 359 -fill $h -outline $h
}
```

Results in this image:

[[image:Tcl-spheroid.gif]]


## TeX


The PGF <code>shadings</code> library includes a "ball" for a 3-D style highlight.  For example with [[LaTeX]],


```TeX
\documentclass{article}
\usepackage{tikz}
\usetikzlibrary{shadings}
\begin{document}
\begin{tikzpicture}
  \shade[ball color=black] (0,0) circle (4);
\end{tikzpicture}
\end{document}
```



## VBScript

```vb
shades = Array(".", ":", "!", "*", "o", "e", "&", "#", "%", "@")
light = Array(30, 30, -50)

Sub Normalize(v)
   length = Sqr(v(0)*v(0) + v(1)*v(1) + v(2)*v(2))
   v(0) = v(0)/length : v(1) = v(1)/length : v(2) = v(2)/length
End Sub

Function Dot(x, y)
   d = x(0)*y(0) + x(1)*y(1) + x(2)*y(2)
   If d < 0 Then Dot = -d Else Dot = 0 End If
End Function

'floor function is the Int function
'ceil function implementation
Function Ceil(x)
    Ceil = Int(x)
    If Ceil <> x Then Ceil = Ceil + 1 End if
End Function

Sub DrawSphere(R, k, ambient)
   Dim i, j, intensity, inten, b, x, y
   Dim vec(3)
   For i = Int(-R) to Ceil(R)
      x = i + 0.5
      line = ""
      For j = Int(-2*R) to Ceil(2*R)
         y = j / 2 + 0.5
         If x * x + y * y <= R*R Then
            vec(0) = x
            vec(1) = y
            vec(2) = Sqr(R * R - x * x - y * y)
            Normalize vec
            b = dot(light, vec)^k + ambient
            intensity = Int((1 - b) * UBound(shades))
            If intensity < 0 Then intensity = 0 End If
            If intensity >= UBound(shades) Then
               intensity = UBound(shades)
            End If
            line = line & shades(intensity)
         Else
            line = line & " "
         End If
      Next
      WScript.StdOut.WriteLine line
   Next
End Sub

Normalize light
DrawSphere 20, 4, 0.1
DrawSphere 10,2,0.4
```

```txt
                               &&&&&&&&&&#######
                        &eeeeeeeeeeeeeeee&&&&&&#######%
                    &eoooo*******oooooooeeeee&&&&&########%
                 eoo****!!!!!!!!******oooooeeee&&&&&########%%
              eoo**!!!!::::::::!!!!!*****ooooeeee&&&&&########%%%
            eo**!!::::::...:::::::!!!!!***ooooeeee&&&&&########%%%%
          eo*!!:::.............:::::!!!!***ooooeeee&&&&&########%%%%%
        eo*!!:::.................::::!!!!***ooooeeee&&&&#########%%%%%%
       eo*!!::....................::::!!!****oooeeee&&&&&#########%%%%%%
     &o**!::......................::::!!!****oooeeee&&&&&##########%%%%%%%
    &o**!::.......................::::!!!****oooeeee&&&&&##########%%%%%%%%
   &oo*!!::.......................:::!!!!***ooooeeee&&&&&##########%%%%%%%%%
  &eo*!!::.......................::::!!!****ooooeeee&&&&&##########%%%%%%%%%%
  eo**!!::......................::::!!!!***ooooeeeee&&&&&##########%%%%%%%%%%
 &eo**!!:::...................:::::!!!!****ooooeeee&&&&&###########%%%%%%%%%%%
 eeo**!!::::................:::::!!!!!****ooooeeee&&&&&&###########%%%%%%%%%%%
&eeo***!!:::::...........::::::!!!!!****oooooeeee&&&&&&###########%%%%%%%%%%%%%
&eeoo**!!!!::::::::::::::::::!!!!!*****ooooeeeee&&&&&&############%%%%%%%%%%%%%
&eeooo***!!!!::::::::::::!!!!!!!*****oooooeeeee&&&&&&############%%%%%%%%%%%%%%
&&eeooo***!!!!!!!!!!!!!!!!!!!******oooooeeeeee&&&&&&############%%%%%%%%%%%%%%%
&&eeeooo******!!!!!!!!!!********ooooooeeeeee&&&&&&&############%%%%%%%%%%%%%%%%
#&&eeeooooo******************oooooooeeeeee&&&&&&&#############%%%%%%%%%%%%%%%%%
#&&&eeeeoooooooo******oooooooooooeeeeeee&&&&&&&&#############%%%%%%%%%%%%%%%%%%
##&&&&eeeeeooooooooooooooooooeeeeeeee&&&&&&&&&##############%%%%%%%%%%%%%%%%%%%
 ##&&&&&eeeeeeeeeeeeeeeeeeeeeeeeee&&&&&&&&&################%%%%%%%%%%%%%%%%%%%
 ####&&&&&&eeeeeeeeeeeeeeeeeee&&&&&&&&&&&################%%%%%%%%%%%%%%%%%%%%%
  #####&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#################%%%%%%%%%%%%%%%%%%%%%%
  %#######&&&&&&&&&&&&&&&&&&&&&&&&###################%%%%%%%%%%%%%%%%%%%%%%%%
   %###########&&&&&&&&&&&&&#######################%%%%%%%%%%%%%%%%%%%%%%%%%
    %############################################%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%#######################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       %%#################################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%#########################%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
          %%%%%%%%#############%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                               %%%%%%%%%%%%%%%%%

             ::...:::!!!*o
         ..............::!!*oo
      ..................::!!**ooe
    .....................::!!**ooee
   .......................::!!**ooee
  ........................::!!**oooee
 .........................::!!**oooeee
:........................::!!!**oooeeee
........................::!!!**ooooeeee
:......................::!!!***oooeeeee
:....................:::!!!***oooeeeeee
!:.................:::!!!****oooeeeeeee
*!:::...........::::!!!!***ooooeeeeeeee
 *!!!:::::::::::!!!!!****oooooeeeeeeee
  o**!!!!!!!!!!!!!*****oooooeeeeeeeee
   oo**************ooooooeeeeeeeeeee
    eoooooooooooooooooeeeeeeeeeeeee
      eeeooooooooeeeeeeeeeeeeeeee
         eeeeeeeeeeeeeeeeeeeee
             eeeeeeeeeeeee
```



## XPL0

[[File:SphereXPL0.png|right]]

```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations
def     R=100, R2=R*R;          \radius, in pixels; radius squared
def     X0=640/2, Y0=480/2;     \coordinates of center of screen
int     X, Y, Z, C, D2;         \coords, color, distance from center squared
[SetVid($112);                  \set 640x480x24 graphics mode
for Y:= -R to +R do             \for all the coordinates near the circle
    for X:= -R to +R do         \ which is under the sphere
        [D2:= X*X + Y*Y;
        C:= 0;                  \default color is black
        if D2 <= R2 then        \coordinate is inside circle under sphere
            [Z:= sqrt(R2-D2);   \height of point on surface of sphere above X,Y
            C:= Z-(X+Y)/2+130;  \color is proportional; offset X and Y, and
            ];                  \ shift color to upper limit of its range
        Point(X+X0, Y+Y0, C<<8+C); \green + blue = cyan
       ];
repeat until KeyHit;            \wait for keystroke
SetVid($03);                    \restore normal text mode
]
```



## Yabasic


```Yabasic
ancho = 640 : alto = 480
open window 640,480
backcolor 16,16,16
clear window
sphera()

sub sphera()
	local n

	for n = 1 to 100
		color 2*n, 2*n, 2*n
		fill circle ancho/2-2*n/3, alto/2-n/2, 150-n
	next n
end sub
```



## zkl

Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl
[[File:Sphere.zkl.jpg|200px|thumb|right]]

```zkl
img:=PPM(640,480);
R:=100; R2:=R*R;           //radius, in pixels; radius squared
X0:=640/2; Y0:=480/2;      //coordinates of center of screen
foreach Y in ([-R..R]){    //for all the coordinates near the circle
   foreach X in ([-R..R]){ // which is under the sphere
      D2:=X*X + Y*Y;
      C:=0;                //default color is black
      if(D2<=R2){          //coordinate is inside circle under sphere
	 Z:=(R2-D2).toFloat().sqrt();//height of point on surface of sphere above X,Y
	 C=0x82+Z-(X+Y)/2; //color is proportional; offset X and Y, and
      }                    // shift color to upper limit of its range
      img[X+X0,Y+Y0]=C.shiftLeft(8)+C; //green + blue = cyan
   }
}
img.write(File("foo.ppm","wb"));
```

The radius of 100 is the max before the color calculation overflows 24 bits so for a radius (R) of, say 200, use

```zkl
img[X+X0,Y+Y0]=C*140+C;
```

Perhaps a more useful solution is to use GnuPlot (I grabbed the code from http://ayapin-film.sakura.ne.jp/Gnuplot/):
[[File:GnuplotSphere.zkl.png|250px|thumb|right]]

```zkl
#<<<
cmd:=0'|
set term wxt
set parametric
set urange [0:pi]
set vrange [0:2*pi]
set xyplane 0
set view equal xyz
set pm3d hidden3d 100 depthorder
set style line 100 lt 7 lw 0.1
set palette defined (0 "dark-blue", 1 "light-blue")
unset key
set samples 24
set isosamples 36
set title 'sphere (pm3d)' font "Times,20"
R = 3
splot R*sin(u)*cos(v), R*sin(u)*sin(v), R*cos(u) w pm3d
|;
#<<<

gnuplot:=System.popen("gnuplot","w");
gnuplot.write(cmd); gnuplot.flush();
ask("Hit return to finish"); gnuplot.close();
```

Where "term wxt" is X11 on my Linux box. A window pops up and stays until the pipe is closed.


## ZX Spectrum Basic


```zxbasic
1 REM fast
50 REM spheer with hidden lines and rotation
100 CLS
110 PRINT "sphere with lenght&wide-circles"
120 PRINT "_______________________________"''
200 INPUT "rotate x-as:";a
210 INPUT "rotate y-as:";b
220 INPUT "rotate z-as:";c
225 INPUT "distance lines(10-45):";d
230 LET u=128: LET v=87: LET r=87: LET bm=PI/180: LET h=.5
240 LET s1=SIN (a*bm): LET s2=SIN (b*bm): LET s3=SIN (c*bm)
250 LET c1=COS (a*bm): LET c2=COS (b*bm): LET c3=COS (c*bm)
260 REM calc rotate matrix
270 LET ax=c2*c3: LET ay=-c2*s3: LET az=s2
280 LET bx=c1*s3+s1*s2*c3
290 LET by=c1*c3-s1*s2*s3: LET bz=-s1*c2
300 LET cx=s1*s3-c1*s2*c3
310 LET cy=s1*c3+c1*s2*s3: LET cz=c1*c2
400 REM draw outer
410 CLS : CIRCLE u,v,r
500 REM draw lenght-circle
510 FOR l=0 TO 180-d STEP d
515 LET f1=0
520 FOR p=0 TO 360 STEP 5
530 GO SUB 1000: REM xx,yy,zz calc
540 IF yy>0 THEN LET f2=0: LET f1=0: GO TO 580
550 LET xb=INT (u+xx+h): LET yb=INT (v+zz+h): LET f2=1
560 IF f1=0 THEN LET x1=xb: LET y1=yb: LET f1=1: GO TO 580
570 PLOT x1,y1: DRAW xb-x1,yb-y1: LET x1=xb: LET y1=yb: LET f1=f2
580 NEXT p
590 NEXT l
600 REM draw wide-circle
610 FOR p=-90+d TO 90-d STEP d
615 LET f1=0
620 FOR l=0 TO 360 STEP 5
630 GO SUB 1000: REM xx,yy,zz
640 IF yy>0 THEN LET f2=0: LET f1=0: GO TO 680
650 LET xb=INT (u+xx+h): LET yb=INT (v+zz+h): LET f2=1
660 IF f1=0 THEN LET x1=xb: LET y1=yb: LET f1=1: GO TO 680
670 PLOT x1,y1: DRAW xb-x1,yb-y1: LET x1=xb: LET y1=yb: LET f1=f2
680 NEXT l
690 NEXT p
700 PRINT #0;"...press any key...": PAUSE 0: RUN
999 REM sfere-coordinates>>>Cartesis Coordinate
1000 LET x=r*COS (p*bm)*COS (l*bm)
1010 LET y=r*COS (p*bm)*SIN (l*bm)
1020 LET z=r*SIN (p*bm)
1030 REM p(x,y,z) rotate to p(xx,yy,zz)
1040 LET xx=ax*x+ay*y+az*z
1050 LET yy=bx*x+by*y+bz*z
1060 LET zz=cx*x+cy*y+cz*z
1070 RETURN
```


