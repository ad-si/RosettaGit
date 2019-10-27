+++
title = "Video display modes"
description = ""
date = 2019-06-11T15:03:07Z
aliases = []
[extra]
id = 9889
[taxonomies]
categories = []
tags = []
+++

{{task}}[[Category:Initialization]]
The task is to demonstrate how to switch video display modes within the language. A brief description of the supported video modes would be useful.


## Applesoft BASIC

There are ten possible modes:
 TEXT, page 1, 40 x 24
 GR, page 1, 40 x 40, 16 colors, mixed with four lines of text
 HGR, page 1, 280 x 160, 6 colors, mixed with four lines of text
 HGR2, page 2, 280 x 192, 6 colors, full screen
 text, page 2, 40 x 24
 gr, page 1, 40 x 48, 16 colors, full screen
 gr, page 2, 40 x 40, 16 colors, mixed with four lines of text 
 gr, page 2, 40 x 48, 16 colors, full screen
 hgr, page 1, 280 x 192, 6 colors, full screen
 hgr, page 2, 280 x 160, 6 colors, mixed with four lines of text


```ApplesoftBasic
10 GR
20 FOR I = 0 TO 15 : COLOR = I : PLOT I,I : NEXT
30 HGR
40 FOR I = 1 TO 6 : HCOLOR = I : HPLOT I * 2, I TO I * 2 + 1, I : NEXT
50 TEXT
60 HOME : FOR I = 0 TO 7 : VTAB I + 1 : FOR J = 0 TO 31 : POKE PEEK(40) + PEEK(41) * 256 + 4 + J, I * 32 + J : NEXT J, I
70 HGR2
80 FOR I = 1 TO 6 : HCOLOR = I : HPLOT I * 2, I + 6 TO I * 2 + 1, I + 6 : NEXT
90 TEXT
100 GET A$
110 IF A$ = "H" THEN POKE -16297,0 : REM HI-RESOLUTION
120 IF A$ = "L" THEN POKE -16298,0 : REM LO-RESOLUTION
130 IF A$ = "2" THEN POKE -16299,0 : REM PAGE 2
140 IF A$ = "1" THEN POKE -16300,0 : REM PAGE 1
150 IF A$ = "M" THEN POKE -16301,0 : REM MIXED TEXT
160 IF A$ = "F" THEN POKE -16302,0 : REM FULL SCREEN
170 IF A$ = "T" THEN POKE -16303,0 : REM TEXT
180 IF A$ = "G" THEN POKE -16304,0 : REM GRAPHICS
190 IF A$ <> "Q" THEN 100
200 TEXT
```



## BBC BASIC


```bbcbasic>10 MODE 1: REM 320x256 4 colour graphics</lang


=={{header|GW-BASIC}}==

'''This example is wrong! GWBASIC can use ONLY EGA-Graphic until SCREEN 9 (640 x 350 - 16 colors) with color monitor or SCREEN 10 (640 x 350 - 2 "colors") for monochrome monitor.
GWBASIC version used: 3.23'''


```gwbasic
10 REM GW Basic can switch VGA modes
20 SCREEN 18: REM Mode 12h 640x480 16 colour graphics
```



## ERRE

ERRE language (for PC) supports these modes (accessible with SCREEN procedure of PC.LIB library):
* SCREEN(0) => text 80x25 or 40x25 -- standard
* SCREEN(1) => 320x200 4 colors
* SCREEN(2) => 640x200 2 colors
* SCREEN(7) => 320x200 16 colors
* SCREEN(8) => 640x200 16 colors
* SCREEN(9) => 640x350 16 colors 
* SCREEN(10) => 640x350 for MDA monitors (if you have one .....)
It's possible to activate all VGA and SVGA modes using DOS interrupts. On the distribution disk there is an example to activate 320x200 - 256 colors.

ERRE language (for C-64) support high resolution graphic (320x200) using HGR.LIB library.


## Go

{{trans|UNIX Shell}}
{{works with|Ubuntu 16.04}}

```go
package main

import (
    "fmt"
    "log"
    "os/exec"
    "time"
)

func main() {
    // query supported display modes
    out, err := exec.Command("xrandr", "-q").Output()
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(out))
    time.Sleep(3 * time.Second)

    // change display mode to 1024x768 say (no text output)
    err = exec.Command("xrandr", "-s", "1024x768").Run()
    if err != nil {
        log.Fatal(err)
    }
    time.Sleep(3 * time.Second)

    // change it back again to 1366x768 (or whatever is optimal for your system)
    err = exec.Command("xrandr", "-s", "1366x768").Run()
    if err != nil {
        log.Fatal(err)
    }
}
```


{{out}}

```txt

Screen 0: minimum 8 x 8, current 1366 x 768, maximum 32767 x 32767
eDP1 connected primary 1366x768+0+0 (normal left inverted right x axis y axis) 344mm x 193mm
   1366x768      60.00*+  48.01  
   1360x768      59.80    59.96  
   1280x720      60.00  
   1024x768      60.00  
   1024x576      60.00  
   960x540       60.00  
   800x600       60.32    56.25  
   864x486       60.00  
   640x480       59.94  
   720x405       60.00  
   680x384       60.00  
   640x360       60.00  
DP1 disconnected (normal left inverted right x axis y axis)
DP2 disconnected (normal left inverted right x axis y axis)
HDMI1 disconnected (normal left inverted right x axis y axis)
HDMI2 disconnected (normal left inverted right x axis y axis)
VIRTUAL1 disconnected (normal left inverted right x axis y axis)

```



## Groovy

{{trans|Kotlin}}
{{works with|Ubuntu 14.04}}

```groovy
def invoke(String cmd) { println(cmd.execute().text) }

invoke("xrandr -q")
Thread.sleep(3000)

invoke("xrandr -s 1024x768")
Thread.sleep(3000)

invoke("xrandr -s 1366x768")
```


{{out}}

```txt
Screen 0: minimum 640 x 480, current 5080 x 1898, maximum 5080 x 1920
default connected 5080x1898+0+0 0mm x 0mm
   2880x1800     60.00  
   1440x900      60.00  
   2560x1600     60.00  
   2048x1280     60.00  
   1024x768      60.00  
   800x600       60.00  
   640x480       60.00  
   1680x1050     60.00  
   1280x800      60.00  
   5080x1898      1.00* 
   5080x1920      2.00  
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages on X-windows based systems and assume
<tt>xrandr</tt> is installed:

{{trans|UNIX Shell}}

```unicon
procedure main(A)
    mode := A[1]
    if \mode then system("xrandr -s " || \mode || " >/dev/null")
    else system("xrandr -q")    # Display available modes
end
```


Output with no arguments:


```txt

->vdm
Screen 0: minimum 8 x 8, current 3840 x 1200, maximum 8192 x 8192
DVI-I-0 disconnected (normal left inverted right x axis y axis)
VGA-0 connected 1920x1200+0+0 (normal left inverted right x axis y axis) 518mm x 324mm
   1920x1200      60.0*+
   1600x1200      75.0     70.0     65.0     60.0  
   1280x1024      75.0     60.0  
   1280x960       60.0  
   1152x864       75.0  
   1024x768       75.0     70.1     60.0  
   800x600        75.0     72.2     60.3     56.2  
   640x480        75.0     72.8     59.9  
TV-0 disconnected (normal left inverted right x axis y axis)
DVI-I-1 connected 1920x1200+1920+0 (normal left inverted right x axis y axis) 518mm x 324mm
   1920x1200      60.0*+
   1680x1050      60.0  
   1600x1200      60.0  
   1280x1024      75.0     60.0  
   1280x960       75.0  
   1024x768       75.0     60.0  
   800x600        75.0     60.3  
   640x480        75.0     59.9  
->

```



## Julia

{{trans|Phix}}

```julia

if Base.Sys.islinux()
    run(`xrandr -s 640x480`)
    sleep(3)
    run(`xrandr -s 1280x960`)
else # windows
    run(`mode CON: COLS=40 LINES=100`)
    sleep(3)
    run(`mode CON: COLS=100 LINES=50`)
end

```



## Kotlin

{{trans|UNIX Shell}}
{{works with|Ubuntu 14.04}}

```scala
// version 1.1.51

import java.util.Scanner

fun runSystemCommand(command: String) {
    val proc = Runtime.getRuntime().exec(command)
    Scanner(proc.inputStream).use {
        while (it.hasNextLine()) println(it.nextLine())
    }
    proc.waitFor()
    println()
}

fun main(args: Array<String>) {
    // query supported display modes  
    runSystemCommand("xrandr -q")
    Thread.sleep(3000)

    // change display mode to 1024x768 say (no text output)
    runSystemCommand("xrandr -s 1024x768")
    Thread.sleep(3000)

    // change it back again to 1366x768 (or whatever is optimal for your system)
    runSystemCommand("xrandr -s 1366x768")
}
```


{{out}}

```txt

Screen 0: minimum 320 x 200, current 1366 x 768, maximum 32767 x 32767
eDP1 connected primary 1366x768+0+0 (normal left inverted right x axis y axis) 344mm x 193mm
   1366x768       60.0*+   48.0  
   1360x768       59.8     60.0  
   1024x768       60.0  
   800x600        60.3     56.2  
   640x480        59.9  
HDMI1 disconnected (normal left inverted right x axis y axis)
DP1 disconnected (normal left inverted right x axis y axis)
HDMI2 disconnected (normal left inverted right x axis y axis)
HDMI3 disconnected (normal left inverted right x axis y axis)
DP2 disconnected (normal left inverted right x axis y axis)
VIRTUAL1 disconnected (normal left inverted right x axis y axis)

```



## Locomotive Basic

The Amstrad CPC464 supports three video modes:
* Mode 0 - Graphics: 160x200    Text: 20x25    Colours: 16
* Mode 1 - Graphics: 320x200    Text: 40x25    Colours: 4
* Mode 2 - Graphics: 640x200    Text: 80x25    Colours: 2
Note that text can be displayed using conventional means in all display modes.

```locobasic>10 MODE 0: REM switch to mode 0</lang



## QBasic

'''This example is wrong! QBASIC can use EGA/VGA Graphics until SCREEN 13 (320x200 - 256 colors) or SCREEN 12 (640x480 - 16 colors).QBASIC version used: 1.1'''

```qbasic
'QBasic can switch VGA modes
SCREEN 18 'Mode 12h 640x480 16 colour graphics
```



## Perl

Same caveats as with Perl 6.
{{trans|Perl 6}}

```perl
$| = 1;

my @info = `xrandr -q`;
$info[0] =~ /current (\d+) x (\d+)/;
my $current = "$1x$2";

my @resolutions;
for (@info) {
    push @resolutions, $1 if /^\s+(\d+x\d+)/
}

system("xrandr -s $resolutions[-1]");
print "Current resolution $resolutions[-1].\n";
for (reverse 1 .. 9) {
    print "\rChanging back in $_ seconds...";
    sleep 1;
}
system("xrandr -s $current");
print "\rResolution returned to $current.\n";
```



## Perl 6

Perl 6 runs on several different operating systems over many different architectures so can not easily assume direct control over hardware. Instead, like most modern programming languages, it relies on the current OS and windowing system to provide APIs.

Here is an example which will work for most Debian based Linuxs (and probably others) running some variant of X11 with a single active monitor.

{{works with|Rakudo|2018.05}}


```perl6
my @info = QX('xrandr -q').lines;

@info[0] ~~ /<?after 'current '>(\d+) ' x ' (\d+)/;
my $current = "$0x$1";

my @resolutions;
@resolutions.push: $0 if $_ ~~ /^\s+(\d+'x'\d+)/ for @info;

QX("xrandr -s @resolutions[*-1]");
say "Current resolution {@resolutions[*-1]}.";
for 9 ... 1 {
    print "\rChanging back in $_ seconds...";
    sleep 1;
}
QX("xrandr -s $current");
say "\rResolution returned to {$current}.     ";
```



## Phix


```Phix
if platform()=LINUX then
    {} = system_exec("xrandr -s 640x480")
    sleep(3)
    {} = system_exec("xrandr -s 1280x960")
else -- WINDOWS
    puts(1,"") -- (ensure console exists)
    system("mode CON: COLS=40 LINES=25")
    sleep(3)
    system("mode CON: COLS=80 LINES=25")
end if
```

Obviously running xrandr -q or mode -? will tell you more.


## Python

{{libheader|win32api}}
{{libheader|win32con}}
{{libheader|win32pywintypes}}
This program changes the resolution the screen is running at to 640x480.

```python
import win32api
import win32con
import pywintypes
devmode=pywintypes.DEVMODEType()
devmode.PelsWidth=640
devmode.PelsHeight=480
devmode.Fields=win32con.DM_PELSWIDTH | win32con.DM_PELSHEIGHT
win32api.ChangeDisplaySettings(devmode,0)
```



## REXX

This method only works in ''DOS prompt'' either under (native) DOS or Microsoft WINDOWS. 

DOS (under Microsoft Windows) will support:
* columns   of   '''11 ──&gt; 32,766'''   (inclusive)
* lines           of   '''1 ──&gt; 32,766'''   (inclusive)


### version 1: no checking for which OS


```rexx
/*REXX program to switch video display modes based on columns and lines.*/

parse arg cols lines .
'MODE'   "CON:       COLS="cols     'LINES='lines
                                       /*stick a fork in it, we're done.*/
```


### version 2: checks for which OS

The prologue code (at the bottom of the program) is a collection of some general-purpose subroutines which determine:
* which environment (operating system) the REXX interpreter is running under
* if Windows/NT/XP/Vista/7/8 (the NT family) is running
* which REXX is being executed
* what literal to use to obtain the environmental variables (for the '''value''' bif)
* what the fileName, fileType/fileExt, fileMode/path is of the REXX program
* which command to use to clear the terminal screen
* invokes $H to show general documentation (1st and only arg = ?)
* invokes $H to show a flow diagram (1st and only arg = ?FLOW) 
* invokes $H to show sample uses (1st and only arg = ?SAMPLE)
* invokes $H to show the author &amp; contact info (1st and only arg = ?AUTHOR)

All the prologue was left intact to give a general feel of the scope of the boilerplate code.
The prologue code is in many REXX programs and it's easier to keep them on one line for copying purposes and sorting.

```rexx
/*REXX program to switch video display modes based on columns and lines.*/
parse arg !;  if !all()  then exit     /*exit if documentation specified*/
if \!dos  &  \!os2       then exit     /*if this isn't DOS,  then exit. */

parse arg cols lines .
'MODE'   "CON:       COLS="cols     'LINES='lines

exit                                   /*stick a fork in it, we're done.*/
/*══════════════════════════════════general 1-line subs═════════════════*/
!all:!!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:if symbol('!CALL')\=="VAR" then !call=;return !call
!env:!env='ENVIRONMENT';if !sys=='MSDOS'|!brexx|!r4|!roo then !env='SYSTEM';if !os2 then !env='OS2'!env;!ebcdic=1=='f0'x;return
!fid:parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;call !sys;if !dos then do;_=lastpos('\',!fn);!fm=left(!fn,_);!fn=substr(!fn,_+1);parse var !fn !fn '.' !ft;end;return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex:parse upper version !ver !vernum !verdate .;!brexx='BY'==!vernum;!kexx='KEXX'==!ver;!pcrexx='REXX/PERSONAL'==!ver|'REXX/PC'==!ver;!r4='REXX-R4'==!ver;!regina='REXX-REGINA'==left(!ver,11);!roo='REXX-ROO'==!ver;call !env;return
!sys:!cms=!sys=='CMS';!os2=!sys=='OS2';!tso=!sys=='TSO'|!sys=='MVS';!vse=!sys=='VSE';!dos=pos('DOS',!sys)\==0|pos('WIN',!sys)\==0|!sys=='CMD';call !rex;return
!var:call !fid;if !kexx then return space(dosenv(arg(1)));return space(value(arg(1),,!env))
```



## Ring


```ring

system("mode 40, 25")

```


## Scala

{{trans|UNIX Shell}}
{{works with|Ubuntu 14.04}}

```scala
object VideoDisplayModes extends App {

  import java.util.Scanner

  def runSystemCommand(command: String) {
    val proc = Runtime.getRuntime.exec(command)

    val a: Unit = {
      val a = new Scanner(proc.getInputStream)
      while (a.hasNextLine) println(a.nextLine())
    }
    proc.waitFor()
    println()
  }

  // query supported display modes
  runSystemCommand("xrandr -q")
  Thread.sleep(3000)

  // change display mode to 1024x768 say (no text output)
  runSystemCommand("xrandr -s 1024x768")
  Thread.sleep(3000)

  // change it back again to 1366x768 (or whatever is optimal for your system)
  runSystemCommand("xrandr -s 1366x768")

}
```


## smart BASIC


'''GRAPHICS''' switches to graphics view (as opposed to text view).

'''GRAPHICS MODE X''' sets graphics commands compositing mode to X, where X is one of the following modes:

CLEAR, COLOR, COLORBURN, COLORDODGE, COPY, DARKEN, DESTATOP, DESTIN, DESTOUT, DESTOVER, DIFFERENCE, EXCLUSION, HARDLIGHT, HUE, LIGHTEN, LUMINOSITY, MULTIPLY, NORMAL (''default''), OVERLAY, PLUSDARKER, PLUSLIGHTER, SATURATION, SCREEN, SOFTLIGHT, SOURCEATOP, SOURCEIN, SOURCEOUT, XOR.

'''EXAMPLE:'''

```qbasic
GRAPHICS
FILL RECT 50,50 SIZE 50
GRAPHICS MODE CLEAR
FILL RECT 50,50 SIZE 25
```



## UNIX Shell

If the system runs X11 and supports XRANDR, then

```bash
$ xrandr -q
```

lists the available modes, and

```bash
$ xrandr -s 1024x768
```

sets the screen to the given size.

With modern LCD monitors, this feature is not very useful. These monitors have a single best mode, and the X server discovers and uses that mode by default. Smaller screen modes might work, but make a blurry picture.


## XPL0


```XPL0
code SetVid=45;
SetVid(Mode)
```

Any display mode supported by the IBM-PC-compatible hardware and by the VGA or VESA standards can be enabled by calling the SetVid intrinsic routine. This works without problem on computers booted into DOS and under versions of Windows up until XP. DOSBox[http://www.dosbox.com/] gets around most of the incompatibilities introduced by WinXP and later versions.

Example display modes:
 $03 - CGA 80x25 text in 16 colors (x4)
 $12 - VGA 640x480x4 graphics
 $13 - VGA 320x200x8
 $101 - VESA 640x480x8
 $118 - VESA 1024x768x24

On modern LCD displays (as opposed to older CRT monitors) images can look terrible if the selected mode does not match the native resolution of the LCD.

{{omit from|Blast}}
{{omit from|Brlcad}}
{{omit from|C|This is an OS feature, not a language one}}
{{omit from|Lilypond}}
{{omit from|Mathematica}}
{{omit from|ML/I}}
{{omit from|Openscad}}
{{omit from|Tcl|Tcl is usually hosted on systems where the display device either isn't switchable or that feature isn't exposed to normal programs. If the OS provides an API to do it, Tcl can call that API using normal foreign-function interface methods, but that's really not very interesting.}}
{{omit from|zkl}}
{{omit from|ZX Spectrum Basic}}
