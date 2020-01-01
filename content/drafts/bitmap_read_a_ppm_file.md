+++
title = "Bitmap/Read a PPM file"
description = ""
date = 2018-09-13T01:11:13Z
aliases = []
[extra]
id = 3225
[taxonomies]
categories = []
tags = []
+++

{{task|Raster graphics operations}}
[[Category:Input Output]]

Using the data storage type defined [[Basic_bitmap_storage|on this page]] for raster images, read an image from a PPM file (binary P6 prefered).
(Read [[wp:Netpbm_format|the definition of PPM file]] on Wikipedia.)

'''Task''': Use [[write ppm file]] solution and [[grayscale image]] solution with this one in order to convert a color image to grayscale one.


## Ada


```ada
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO;   use Ada.Streams.Stream_IO;

function Get_PPM (File : File_Type) return Image is
   use Ada.Characters.Latin_1;
   use Ada.Integer_Text_IO;

   function Get_Line return String is -- Skips comments
      Byte   : Character;
      Buffer : String (1..80);
   begin
      loop
         for I in Buffer'Range loop
            Character'Read (Stream (File), Byte);
            if Byte = LF then
               exit when Buffer (1) = '#';
               return Buffer (1..I - 1);
            end if;
            Buffer (I) := Byte;
         end loop;
         if Buffer (1) /= '#' then
            raise Data_Error;
         end if;
      end loop;
   end Get_Line;

   Height : Integer;
   Width  : Integer;
begin
   if Get_Line /= "P6" then
      raise Data_Error;
   end if;
   declare
      Line  : String  := Get_Line;
      Start : Integer := Line'First;
      Last  : Positive;
   begin
      Get (Line, Width, Last);                     Start := Start + Last;
      Get (Line (Start..Line'Last), Height, Last); Start := Start + Last;
      if Start <= Line'Last then
         raise Data_Error;
      end if;
      if Width < 1 or else Height < 1 then
         raise Data_Error;
      end if;
   end;
   if Get_Line /= "255" then
      raise Data_Error;
   end if;
   declare
      Result : Image (1..Height, 1..Width);
      Buffer : String (1..Width * 3);
      Index  : Positive;
   begin
      for I in Result'Range (1) loop
         String'Read (Stream (File), Buffer);
         Index := Buffer'First;
         for J in Result'Range (2) loop
            Result (I, J) :=
               (  R => Luminance (Character'Pos (Buffer (Index))),
                  G => Luminance (Character'Pos (Buffer (Index + 1))),
                  B => Luminance (Character'Pos (Buffer (Index + 2)))
               );
            Index := Index + 3;
         end loop;
      end loop;
      return Result;
   end;
end Get_PPM;
```

The implementation propagates Data_Error when the file format is incorrect. End_Error is propagated when the file end is prematurely met. The following example illustrates conversion of a color file to grayscale.

```ada
declare
   F1, F2 : File_Type;
begin
   Open (F1, In_File, "city.ppm");
   Create (F2, Out_File, "city_grayscale.ppm");
   Put_PPM (F2, Color (Grayscale (Get_PPM (F1))));
   Close (F1);
   Close (F2);
end;
```


## AutoHotkey

{{works with | AutoHotkey_L}}
Only ppm6 files supported.


```AutoHotkey
img := ppm_read("lena50.ppm") ;
x := img[4,4] ; get pixel(4,4)
y := img[24,24] ; get pixel(24,24)
msgbox % x.rgb() " " y.rgb()
img.write("lena50copy.ppm")
return

ppm_read(filename, ppmo=0) ; only ppm6 files supported
{
if !ppmo  ; if image not already in memory, read from filename
  fileread, ppmo, % filename

  index := 1
  pos := 1

  loop, parse, ppmo, `n, `r
  {
    if (substr(A_LoopField, 1, 1) == "#")
      continue
loop,
{
 if !pos := regexmatch(ppmo, "\d+", pixel, pos)
break
    bitmap%A_Index% := pixel
    if (index == 4)
      Break
    pos := regexmatch(ppmo, "\s", x, pos)
    index ++
}
  }

  type := bitmap1
  width := bitmap2
  height := bitmap3
  maxcolor := bitmap4
  bitmap := Bitmap(width, height, color(0,0,0))
  index := 1
  i := 1
  j := 1
 bits := pos
loop % width * height
  {
      bitmap[i, j, "r"]  := numget(ppmo, 3 * A_Index + bits, "uchar")
      bitmap[i, j, "g"]  := numget(ppmo, 3 * A_Index + bits + 1, "uchar")
      bitmap[i, j, "b"]  := numget(ppmo, 3 * A_Index + bits + 2, "uchar")

      if (j == width)
{
	j := 1
	i += 1
}
      else
	j++
}
 return bitmap
  }
#include bitmap_storage.ahk ; from http://rosettacode.org/wiki/Basic_bitmap_storage/AutoHotkey
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      f% = OPENIN("c:\lena.ppm")
      IF f%=0 ERROR 100, "Failed to open input file"

      IF GET$#f% <> "P6" ERROR 101, "File is not in P6 format"
      REPEAT
        in$ = GET$#f%
      UNTIL LEFT$(in$,1) <> "#"
      size$ = in$
      max$ = GET$#f%

      Width% = VAL(size$)
      space% = INSTR(size$, " ")
      Height% = VALMID$(size$, space%)

      VDU 23,22,Width%;Height%;8,16,16,128

      FOR y% = Height%-1 TO 0 STEP -1
        FOR x% = 0 TO Width%-1
          r% = BGET#f% : g% = BGET#f% : b% = BGET#f%
          l% = INT(0.3*r% + 0.59*g% + 0.11*b% + 0.5)
          PROCsetpixel(x%,y%,l%,l%,l%)
        NEXT
      NEXT y%
      END

      DEF PROCsetpixel(x%,y%,r%,g%,b%)
      COLOUR 1,r%,g%,b%
      GCOL 1
      LINE x%*2,y%*2,x%*2,y%*2
      ENDPROC
```



## C

It is up to the caller to open the file and pass the handler to the function. So this code can be used in
[[Read image file through a pipe]] without modification. It only understands the P6 file format.

Interface:


```c
image get_ppm(FILE *pf);
```


Implementation:


```c
#include "imglib.h"

#define PPMREADBUFLEN 256
image get_ppm(FILE *pf)
{
        char buf[PPMREADBUFLEN], *t;
        image img;
        unsigned int w, h, d;
        int r;

        if (pf == NULL) return NULL;
        t = fgets(buf, PPMREADBUFLEN, pf);
        /* the code fails if the white space following "P6" is not '\n' */
        if ( (t == NULL) || ( strncmp(buf, "P6\n", 3) != 0 ) ) return NULL;
        do
        { /* Px formats can have # comments after first line */
           t = fgets(buf, PPMREADBUFLEN, pf);
           if ( t == NULL ) return NULL;
        } while ( strncmp(buf, "#", 1) == 0 );
        r = sscanf(buf, "%u %u", &w, &h);
        if ( r < 2 ) return NULL;

        r = fscanf(pf, "%u", &d);
        if ( (r < 1) || ( d != 255 ) ) return NULL;
        fseek(pf, 1, SEEK_CUR); /* skip one byte, should be whitespace */

        img = alloc_img(w, h);
        if ( img != NULL )
        {
            size_t rd = fread(img->buf, sizeof(pixel), w*h, pf);
            if ( rd < w*h )
            {
               free_img(img);
               return NULL;
            }
            return img;
        }
}
```


The following acts as a filter to convert a PPM file read from standard input into a PPM gray image, and it outputs the converted image to standard output (see [[Grayscale image]], [[Write ppm file]], and [[Raster graphics operations]] in general):


```c
#include <stdio.h>
#include "imglib.h"

int main()
{
   image source;
   grayimage idest;

   source = get_ppm(stdin);
   idest = tograyscale(source);
   free_img(source);
   source = tocolor(idest);
   output_ppm(stdout, source);
   free_img(source); free_img((image)idest);
   return 0;
}
```


## C#
Tested with [[Write ppm file#C.23|this solution.]]


```c#
using System.IO;
class PPMReader
{
    public static Bitmap ReadBitmapFromPPM(string file)
    {
        var reader = new BinaryReader(new FileStream(file, FileMode.Open));
        if (reader.ReadChar() != 'P' || reader.ReadChar() != '6')
            return null;
        reader.ReadChar(); //Eat newline
        string widths = "", heights = "";
        char temp;
        while ((temp = reader.ReadChar()) != ' ')
            widths += temp;
        while ((temp = reader.ReadChar()) >= '0' && temp <= '9')
            heights += temp;
        if (reader.ReadChar() != '2' || reader.ReadChar() != '5' || reader.ReadChar() != '5')
            return null;
        reader.ReadChar(); //Eat the last newline
        int width = int.Parse(widths),
            height = int.Parse(heights);
        Bitmap bitmap = new Bitmap(width, height);
        //Read in the pixels
        for (int y = 0; y < height; y++)
            for (int x = 0; x < width; x++)
                bitmap.SetPixel(x, y, new Bitmap.Color()
                {
                    Red = reader.ReadByte(),
                    Green = reader.ReadByte(),
                    Blue = reader.ReadByte()
                });
        return bitmap;
    }
}
```



## Common Lisp


The function read-ppm-image reads either a P6 or P3 file depending on the file contents. The package description assumes that you have the [[Basic bitmap storage#Common Lisp]] package.


```lisp

(in-package #:rgb-pixel-buffer)

(defparameter *whitespaces-chars* '(#\SPACE #\RETURN #\TAB #\NEWLINE #\LINEFEED))

(defun read-header-chars (stream &optional (delimiter-list *whitespaces-chars*))
  (do ((c (read-char stream nil :eof)
          (read-char stream nil :eof))
       (vals nil (if (or (null c) (char= c  #\#)) vals (cons c vals))))   ;;don't collect comment chars
       ((or (eql c :eof) (member c delimiter-list)) (map 'string #'identity (nreverse vals)))   ;;return strings
    (when (char= c #\#)   ;;skip comments
      (read-line stream))))

(defun read-ppm-file-header (file)
  (with-open-file (s file :direction :input)
    (do ((failure-count 0 (1+ failure-count))
	 (tokens nil (let ((t1 (read-header-chars s)))
		       (if (> (length t1) 0)
			   (cons t1 tokens)
			   tokens))))
	((>= (length tokens) 4) (values (nreverse tokens)
			      (file-position s)))
      (when (>= failure-count 10)
	(error (format nil "File ~a does not seem to be a proper ppm file - maybe too many comment lines" file)))
      (when (= (length tokens) 1)
	(when (not (or (string= (first tokens) "P6") (string= (first tokens) "P3")))
	  (error (format nil "File ~a is not a ppm file - wrong magic-number. Read ~a instead of P6 or P3 " file (first tokens))))))))

(defun read-ppm-image (file)
  (flet ((image-data-reader (stream start-position width height image-build-function read-function)
	   (file-position stream start-position)
	   (dotimes (row height)
	     (dotimes (col width)
	       (funcall image-build-function row col (funcall read-function stream))))))
    (multiple-value-bind (header file-pos) (read-ppm-file-header file)
      (let* ((image-type (first header))
	     (width (parse-integer (second header) :junk-allowed t))
	     (height (parse-integer (third header) :junk-allowed t))
	     (max-value (parse-integer (fourth header) :junk-allowed t))
	     (image (make-rgb-pixel-buffer width height)))
	(when (> max-value 255)
	  (error "unsupported depth - convert to 1byte depth with pamdepth"))
	(cond ((string= "P6" image-type)
	       (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
		 (image-data-reader stream
				    file-pos
				    width
				    height
				    #'(lambda (w h val)
					(setf (rgb-pixel image w h) val))
				    #'(lambda (stream)
					(make-rgb-pixel (read-byte stream)
							(read-byte stream)
							(read-byte stream))))
		 image))
	      ((string= "P3" image-type)
	       (with-open-file (stream file :direction :input)
		 (image-data-reader stream
				    file-pos
				    width
				    height
				    #'(lambda (w h val)
					(setf (rgb-pixel image w h) val))
				    #'(lambda (stream)
					(make-rgb-pixel (read stream)
							(read stream)
							(read stream))))
		 image))
	      (t 'unsupported))
      image))))

(export 'read-ppm-image)

```

To read the feep.ppm file as shown on the description page for the ppm format use:

```lisp

(read-ppm-image "feep.ppm")

```



## D

The Image module contains a loadPPM6 function to load binary PPM images.


## E



```e>def chr := <import:java.lang.makeCharacter
.asChar

def readPPM(inputStream) {
  # Proper native-to-E stream IO facilities have not been designed and
  # implemented yet, so we are borrowing Java's. Poorly. This *will* be
  # improved eventually.

  # Reads one header token, skipping comments and whitespace, and exactly
  # one trailing whitespace character
  def readToken() {
    var token := ""
    var c := chr(inputStream.read())
    while (c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '#') {
      if (c == '#') {
        while (c != '\n') { c := chr(inputStream.read()) }
      }
      # skip over initial whitespace
      c := chr(inputStream.read())
    }
    while (!(c == ' ' || c == '\t' || c == '\r' || c == '\n')) {
      if (c == '#') {
        while (c != '\n') { c := chr(inputStream.read()) }
      } else {
        token += E.toString(c)
        c := chr(inputStream.read())
      }
    }
    return token
  }

  # Header
  require(readToken() == "P6")
  def width := __makeInt(readToken())
  def height := __makeInt(readToken())
  def maxval := __makeInt(readToken())

  def size := width * height * 3

  # Body
  # See [[Basic bitmap storage]] for the definition and origin of sign()
  def data := <elib:tables.makeFlexList>.fromType(<type:java.lang.Byte>, size)
  if (maxval >= 256) {
    for _ in 1..size {
      data.push(sign((inputStream.read() * 256 + inputStream.read()) * 255 // maxval))
    }
  } else {
    for _ in 1..size {
      data.push(sign(inputStream.read() * 255 // maxval))
    }
  }

  def image := makeImage(width, height)
  image.replace(data.snapshot())
  return image
}
```


[[Category:E examples needing attention]]Note: As of this writing the [[grayscale image]] task has not been implemented, so the task code (below) won't actually run yet. But readPPM above has been tested separately.


```e
def readPPMTask(inputFile, outputFile) {
  makeGrayscale \
    .fromColor(readPPM(<import:java.io.makeFileInputStream>(inputFile))) \
    .toColor() \
    .writePPM(<import:java.io.makeFileOutputStream>(outputFile))
}
```




## Erlang



```erlang

% This module provides basic operations on ppm files:
% Read from file, create ppm in memory (from generic bitmap) and save to file.
% Writing PPM files was introduced in roseta code task 'Bitmap/Write a PPM file'
% but the same code is included here to provide whole set of operations on ppm
% needed for purposes of this task.

-module(ppm).

-export([ppm/1, write/2, read/1]).

% constants for writing ppm file
-define(WHITESPACE, <<10>>).
-define(SPACE, <<32>>).

% constants for reading ppm file
-define(WHITESPACES, [9, 10, 13, 32]).
-define(PPM_HEADER, "P6").

% data structure introduced in task Bitmap (module ros_bitmap.erl)
-record(bitmap, {
    mode = rgb,
    pixels = nil,
    shape = {0, 0}
  }).

%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% read ppm file from file
read(Filename) ->
    {ok, File} = file:read_file(Filename),
    parse(File).

% create ppm image from bitmap record
ppm(Bitmap) ->
    {Width, Height} = Bitmap#bitmap.shape,
    Pixels = ppm_pixels(Bitmap),
    Maxval = 255,  % original ppm format maximum
    list_to_binary([
      header(), width_and_height(Width, Height), maxval(Maxval), Pixels]).

% write bitmap as ppm file
write(Bitmap, Filename) ->
    Ppm = ppm(Bitmap),
    {ok, File} = file:open(Filename, [binary, write]),
    file:write(File, Ppm),
    file:close(File).

%%%%%%%%% Reading PPM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse(Binary) ->
    {?PPM_HEADER, Data} = get_next_token(Binary),
    {Width, HeightAndRest} = get_next_token(Data),
    {Height, MaxValAndRest} = get_next_token(HeightAndRest),
    {_MaxVal, RawPixels} = get_next_token(MaxValAndRest),
    Shape = {list_to_integer(Width), list_to_integer(Height)},
    Pixels = load_pixels(RawPixels),
    #bitmap{pixels=Pixels, shape=Shape}.

% load binary as a list of RGB triplets
load_pixels(Binary) when is_binary(Binary)->
    load_pixels([], Binary).
load_pixels(Acc, <<>>) ->
    array:from_list(lists:reverse(Acc));
load_pixels(Acc, <<R, G, B, Rest/binary>>) ->
    load_pixels([<<R,G,B>>|Acc], Rest).

is_whitespace(Byte) ->
    lists:member(Byte, ?WHITESPACES).

% get next part of PPM file, skip whitespaces, and return the rest of a binary
get_next_token(Binary) ->
    get_next_token("", true, Binary).
get_next_token(CurrentToken, false, <<Byte, Rest/binary>>) ->
    case is_whitespace(Byte) of
        true ->
            {lists:reverse(CurrentToken), Rest};
        false ->
            get_next_token([Byte | CurrentToken], false, Rest)
    end;
get_next_token(CurrentToken, true, <<Byte, Rest/binary>>) ->
    case is_whitespace(Byte) of
        true ->
            get_next_token(CurrentToken, true, Rest);
        false ->
            get_next_token([Byte | CurrentToken], false, Rest)
    end.

%%%%%%%%% Writing PPM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
header() ->
    [<<"P6">>, ?WHITESPACE].

width_and_height(Width, Height) ->
    [encode_decimal(Width), ?SPACE, encode_decimal(Height), ?WHITESPACE].

maxval(Maxval) ->
   [encode_decimal(Maxval), ?WHITESPACE].

ppm_pixels(Bitmap) ->
    % 24 bit color depth
    array:to_list(Bitmap#bitmap.pixels).

encode_decimal(Number) ->
    integer_to_list(Number).

```


Usage in accordance with Grayscale Task:

```erlang

Colorful = ppm:read("colorful.ppm"),
Gray = ros_bitmap:convert(ros_bitmap:convert(Colorful, grey), rgb),
ppm:write(Gray, "gray.ppm"),

```



## Euphoria


```euphoria
include get.e

function get2(integer fn)
    sequence temp
    temp = get(fn)
    return temp[2] - temp[1]*temp[1]
end function

function read_ppm(sequence filename)
    sequence image, line
    integer dimx, dimy, maxcolor
    atom fn
    fn = open(filename, "rb")
    if fn < 0 then
        return -1 -- unable to open
    end if
    line = gets(fn)
    if not equal(line,"P6\n") then
        return -1 -- only ppm6 files are supported
    end if
    dimx = get2(fn)
    if dimx < 0 then
        return -1
    end if
    dimy = get2(fn)
    if dimy < 0 then
        return -1
    end if
    maxcolor = get2(fn)
    if maxcolor != 255 then
        return -1 -- maxcolors other then 255 are not supported
    end if
    image = repeat(repeat(0,dimy),dimx)
    for y = 1 to dimy do
        for x = 1 to dimx do
            image[x][y] = getc(fn)*#10000 + getc(fn)*#100 + getc(fn)
        end for
    end for
    close(fn)
    return image
end function
```


Converting an image to grayscale:

```euphoria
sequence image
image = read_ppm("image.ppm")
image = to_gray(image)
image = to_color(image)
write_ppm("image_gray.ppm",image)
```



## FBSL

Read a colored PPM file, convert it to grayscale and write back to disk under a different name. Sanity checks are omitted for brevity.

'''24-bpp P6 PPM solution:'''
[[File:FBSLLena.png|right]]

```qbasic
#ESCAPECHARS ON

DIM colored = ".\\Lena.ppm", grayscale = ".\\LenaGry.ppm"
DIM head, tail, r, g, b, l, ptr, blobsize

FILEGET(FILEOPEN(colored, BINARY), FILELEN(colored)): FILECLOSE(FILEOPEN) ' Load buffer
blobsize = INSTR(FILEGET, "\n255\n") + 4 ' Get sizeof PPM header
head = @FILEGET + blobsize: tail = @FILEGET + FILELEN ' Set loop bounds

FOR ptr = head TO tail STEP 3 ' Transform color triplets
	r = PEEK(ptr + 0, 1) ' Read colors stored in RGB order
	g = PEEK(ptr + 1, 1)
	b = PEEK(ptr + 2, 1)
	l = 0.2126 * r + 0.7152 * g + 0.0722 * b ' Derive luminance
	POKE(ptr + 0, CHR(l))(ptr + 1, CHR)(ptr + 2, CHR) ' Write grayscale
NEXT

FILEPUT(FILEOPEN(grayscale, BINARY_NEW), FILEGET): FILECLOSE(FILEOPEN) ' Save buffer
```



## Forth


```forth
: read-ppm { fid -- bmp }
  pad dup 80 fid read-line throw 0= abort" Partial line"
  s" P6" compare abort" Only P6 supported."
  pad dup 80 fid read-line throw 0= abort" Partial line"
  0. 2swap >number
  1 /string		\ skip space
  0. 2swap >number
  2drop drop nip    ( w h )
  bitmap { bmp }
  pad dup 80 fid read-line throw 0= abort" Partial line"
  s" 255" compare abort" Only 8-bits per color channel supported"
  0 pad !
  bmp bdim
  0 do
    dup 0 do
      pad 3 fid read-file throw
      3 - abort" Not enough pixel data in file"
      pad @ i j bmp b!
    loop
  loop drop
  bmp ;

\ testing round-trip
4 3 bitmap value test
red test bfill
green 1 2 test b!

s" red.ppm" w/o create-file throw
test over write-ppm
close-file throw

s" red.ppm" r/o open-file throw
dup read-ppm value test2
close-file throw

: bsize ( bmp -- len ) bdim * pixels bdata ;

test dup bsize  test2 dup bsize  compare .    \ 0 if identical
```



## Fortran

{{works with|Fortran|90 and later}}

(This function is part of module RCImageIO, see [[Write ppm file#Fortran|Write ppm file]])


```fortran
subroutine read_ppm(u, img)
  integer, intent(in) :: u
  type(rgbimage), intent(out) :: img
  integer :: i, j, ncol, cc
  character(2) :: sign
  character :: ccode

  img%width = 0
  img%height = 0
  nullify(img%red)
  nullify(img%green)
  nullify(img%blue)

  read(u, '(A2)') sign
  read(u, *) img%width, img%height
  read(u, *) ncol

  write(0,*) sign
  write(0,*) img%width, img%height
  write(0,*) ncol

  if ( ncol /= 255 ) return

  call alloc_img(img, img%width, img%height)

  if ( valid_image(img) ) then
     do j=1, img%height
        do i=1, img%width
           read(u, '(A1)', advance='no', iostat=status) ccode
           cc = iachar(ccode)
           img%red(i,j) = cc
           read(u, '(A1)', advance='no', iostat=status) ccode
           cc = iachar(ccode)
           img%green(i,j) = cc
           read(u, '(A1)', advance='no', iostat=status) ccode
           cc = iachar(ccode)
           img%blue(i,j) = cc
        end do
     end do
  end if

end subroutine read_ppm
```


'''Notes''':

* doing formatted I/O with Fortran is a pain... And unformatted does not mean ''free''; Fortran2003 has ''streams'', but they are not implemented (yet) in GNU Fortran compiler. Here (as in the write part) I've tried to handle the PPM format through formatted I/O. The tests worked but I have not tried still everything.
* comments after the first line are not handled

## Go


```go
package raster

import (
    "errors"
    "io"
    "io/ioutil"
    "os"
    "regexp"
    "strconv"
)

// ReadFrom constructs a Bitmap object from an io.Reader.
func ReadPpmFrom(r io.Reader) (b *Bitmap, err error) {
    var all []byte
    all, err = ioutil.ReadAll(r)
    if err != nil {
        return
    }
    bss := rxHeader.FindSubmatch(all)
    if bss == nil {
        return nil, errors.New("unrecognized ppm header")
    }
    x, _ := strconv.Atoi(string(bss[3]))
    y, _ := strconv.Atoi(string(bss[6]))
    maxval, _ := strconv.Atoi(string(bss[9]))
    if maxval > 255 {
        return nil, errors.New("16 bit ppm not supported")
    }
    allCmts := append(append(append(bss[1], bss[4]...), bss[7]...), bss[10]...)
    b = NewBitmap(x, y)
    b.Comments = rxComment.FindAllString(string(allCmts), -1)
    b3 := all[len(bss[0]):]
    var n1 int
    for i := range b.px {
        b.px[i].R = byte(int(b3[n1]) * 255 / maxval)
        b.px[i].G = byte(int(b3[n1+1]) * 255 / maxval)
        b.px[i].B = byte(int(b3[n1+2]) * 255 / maxval)
        n1 += 3
    }
    return
}

const (
    // single whitespace character
    ws = "[ \n\r\t\v\f]"
    // isolated comment
    cmt = "#[^\n\r]*"
    // comment sub expression
    cmts = "(" + ws + "*" + cmt + "[\n\r])"
    // number with leading comments
    num = "(" + cmts + "+" + ws + "*|" + ws + "+)([0-9]+)"
)

var rxHeader = regexp.MustCompile("^P6" + num + num + num +
    "(" + cmts + "*" + ")" + ws)
var rxComment = regexp.MustCompile(cmt)

// ReadFile writes binary P6 format PPM from the specified filename.
func ReadPpmFile(fn string) (b *Bitmap, err error) {
    var f *os.File
    if f, err = os.Open(fn); err != nil {
        return
    }
    if b, err = ReadPpmFrom(f); err != nil {
        return
    }
    return b, f.Close()
}
```

Demonstration program, also demonstrating functions from task [[Grayscale image]]:

```go
package main

// Files required to build supporting package raster are found in:
// * This task (immediately above)
// * Bitmap
// * Grayscale image
// * Write a PPM file

import (
    "raster"
    "fmt"
)

func main() {
    // (A file with this name is output by the Go solution to the task
    // "Bitmap/Read an image through a pipe," but of course any 8-bit
    //  P6 PPM file should work.)
    b, err := raster.ReadPpmFile("pipein.ppm")
    if err != nil {
        fmt.Println(err)
        return
    }
    b = b.Grmap().Bitmap()
    err = b.WritePpmFile("grayscale.ppm")
    if err != nil {
        fmt.Println(err)
    }
}
```



## Haskell

The definition of <tt>Bitmap.Netpbm.readNetpbm</tt> is given [[Write ppm file|here]].

```haskell
import Bitmap
import Bitmap.RGB
import Bitmap.Gray
import Bitmap.Netpbm

import Control.Monad
import Control.Monad.ST

main =
    (readNetpbm "original.ppm" :: IO (Image RealWorld RGB)) >>=
    stToIO . toGrayImage >>=
    writeNetpbm "new.pgm"
```

The above writes a PGM, not a PPM, since the image being output is in grayscale. If you actually want a gray PPM, convert the <tt>Image RealWorld Gray</tt> back to an <tt>Image RealWorld RGB</tt> first:

```haskell
main =
    (readNetpbm "original.ppm" :: IO (Image RealWorld RGB)) >>=
    stToIO . (toRGBImage <=< toGrayImage) >>=
    writeNetpbm "new.ppm"
```



## J

'''Solution:'''

Uses <tt>makeRGB</tt> from [[Basic bitmap storage#J|Basic bitmap storage]].

```j
require 'files'

readppm=: monad define
  dat=. fread y                                           NB. read from file
  msk=. 1 ,~ (*. 3 >: +/\) (LF&=@}: *. '#'&~:@}.) dat     NB. mark field ends
  't wbyh maxval dat'=. msk <;._2 dat                     NB. parse
  'wbyh maxval'=. 2 1([ {. [: _99&". (LF,' ')&charsub)&.> wbyh;maxval  NB. convert to numeric
  if. (_99 0 +./@e. wbyh,maxval) +. 'P6' -.@-: 2{.t do. _1 return. end.
  (a. i. dat) makeRGB |.wbyh                              NB. convert to basic bitmap format
)
```


'''Example:'''

Using utilities and file from [[Grayscale image#J|Grayscale image]] and [[Write ppm file#J|Write ppm file]].

Writes a gray PPM file (a color format) which is bigger than necessary. A PGM file would be more appropriate.

```j
myimg=: readppm jpath '~temp/myimg.ppm'
myimgGray=: toColor toGray myimg
myimgGray writeppm jpath '~temp/myimgGray.ppm'
```



## Julia

{{works with|Julia|0.6}}


```julia
using Images, FileIO, Netpbm

rgbimg = load("data/bitmapInputTest.ppm")
greyimg = Gray.(rgbimg)
save("data/bitmapOutputTest.ppm", greyimg)
```



## Kotlin

For convenience, we repeat the code for the class used in the [[Bitmap]] task here and integrate the code in the [[Grayscale image]] task within it.

```scala
// Version 1.2.40

import java.awt.Color
import java.awt.Graphics
import java.awt.image.BufferedImage
import java.io.FileInputStream
import java.io.PushbackInputStream
import java.io.File
import javax.imageio.ImageIO

class BasicBitmapStorage(width: Int, height: Int) {
    val image = BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)

    fun fill(c: Color) {
        val g = image.graphics
        g.color = c
        g.fillRect(0, 0, image.width, image.height)
    }

    fun setPixel(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB())

    fun getPixel(x: Int, y: Int) = Color(image.getRGB(x, y))

    fun toGrayScale() {
        for (x in 0 until image.width) {
            for (y in 0 until image.height) {
                var rgb  = image.getRGB(x, y)
                val red   = (rgb shr 16) and 0xFF
                val green = (rgb shr  8) and 0xFF
                val blue  =  rgb and 0xFF
                val lumin = (0.2126 * red + 0.7152 * green + 0.0722 * blue).toInt()
                rgb = (lumin shl 16) or (lumin shl 8) or lumin
                image.setRGB(x, y, rgb)
            }
        }
    }
}

fun PushbackInputStream.skipComment() {
    while (read().toChar() != '\n') {}
}

fun PushbackInputStream.skipComment(buffer: ByteArray) {
    var nl: Int
    while (true) {
        nl = buffer.indexOf(10) // look for newline at end of comment
        if (nl != -1) break
        read(buffer)  // read another buffer full if newline not yet found
    }
    val len = buffer.size
    if (nl < len - 1) unread(buffer, nl + 1, len - nl - 1)
}

fun Byte.toUInt() = if (this < 0) 256 + this else this.toInt()

fun main(args: Array<String>) {
    // use file, output.ppm, created in the Bitmap/Write a PPM file task
    val pbis = PushbackInputStream(FileInputStream("output.ppm"), 80)
    pbis.use {
        with (it) {
            val h1 = read().toChar()
            val h2 = read().toChar()
            val h3 = read().toChar()
            if (h1 != 'P' || h2 != '6' || h3 != '\n') {
                println("Not a P6 PPM file")
                System.exit(1)
            }
            val sb = StringBuilder()
            while (true) {
                val r = read().toChar()
                if (r == '#') { skipComment(); continue }
                if (r == ' ') break  // read until space reached
                sb.append(r.toChar())
            }
            val width = sb.toString().toInt()
            sb.setLength(0)
            while (true) {
                val r = read().toChar()
                if (r == '#') { skipComment(); continue }
                if (r == '\n') break  // read until new line reached
                sb.append(r.toChar())
            }
            val height = sb.toString().toInt()
            sb.setLength(0)
            while (true) {
                val r = read().toChar()
                if (r == '#') { skipComment(); continue }
                if (r == '\n') break  // read until new line reached
                sb.append(r.toChar())
            }
            val maxCol = sb.toString().toInt()
            if (maxCol !in 0..255) {
                println("Maximum color value is outside the range 0..255")
                System.exit(1)
            }
            var buffer = ByteArray(80)
            // get rid of any more opening comments before reading data
            while (true) {
                read(buffer)
                if (buffer[0].toChar() == '#') {
                    skipComment(buffer)
                }
                else {
                    unread(buffer)
                    break
                }
            }
            // read data
            val bbs = BasicBitmapStorage(width, height)
            buffer = ByteArray(width * 3)
            var y = 0
            while (y < height) {
                read(buffer)
                for (x in 0 until width) {
                    val c = Color(
                        buffer[x * 3].toUInt(),
                        buffer[x * 3 + 1].toUInt(),
                        buffer[x * 3 + 2].toUInt()
                    )
                    bbs.setPixel(x, y, c)
                }
                y++
            }
            // convert to grayscale and save to a file
            bbs.toGrayScale()
            val grayFile = File("output_gray.jpg")
            ImageIO.write(bbs.image, "jpg", grayFile)
        }
    }
}
```


## M2000 Interpreter

Now function Bitmap has double signature. With two numbers make a bitmap,with all pixels white. With one number, expect that it is a file number and read file, and then return the bitmap.


```M2000 Interpreter

Module Checkit {
      Function Bitmap  {
            If match("NN") then {
                 Read x as long, y as long
            } else.if Match("N") Then  {
                \\ is a file?
                  Read f
                  if not Eof(f) then {
                        Line Input #f,  p3$
                              If p3$="P3" Then {
                                    Line Input #f, Comment$
                                    if left$(Comment$,1)="#" then {
                                          Line Input #f, Dimension$
                                    } else  Dimension$=Comment$
                                    long x=Val(piece$(Dimension$," ")(0))
                                    long y=Val(piece$(Dimension$," ")(1))
                                    do {
                                          Line Input #f, P255$
                                    } until left$(P255$, 1)<>"#"
                                    If not P255$="255" then Error "Not proper ppm format"
                              }
                  }
            } else Error "No proper arguments"


            if x<1 or y<1 then  Error "Wrong dimensions"
            structure rgb {
                  red as byte
                  green as byte
                  blue as byte
            }
            m=len(rgb)*x mod 4
            if m>0 then m=4-m  ' add some bytes to raster line
            m+=len(rgb) *x
            Structure rasterline {
                  {
                        pad as byte*m
                  }
                  \\ union pad+hline
                  hline as rgb*x
            }

            Structure Raster {
                  magic as integer*4
                  w as integer*4
                  h as integer*4
                  lines as rasterline*y
            }
            Buffer Clear Image1 as Raster
            \\ 24 chars as header to be used from bitmap render build in functions
            Return Image1, 0!magic:="cDIB", 0!w:=Hex$(x,2), 0!h:=Hex$(y, 2)
            \\ fill white (all 255)
            \\ Str$(string) convert to ascii, so we get all characters from words  width to byte width
            if not valid(f) then  Return Image1, 0!lines:=Str$(String$(chrcode$(255), Len(rasterline)*y))
            Buffer Clear Pad as Byte*4
            SetPixel=Lambda Image1, Pad,aLines=Len(Raster)-Len(Rasterline), blines=-Len(Rasterline) (x, y, c) ->{
                  where=alines+3*x+blines*y
                  if c>0 then c=color(c)
                  c-!
                  Return Pad, 0:=c as long
                  Return Image1, 0!where:=Eval(Pad, 2) as byte, 0!where+1:=Eval(Pad, 1) as byte, 0!where+2:=Eval(Pad, 0) as byte

            }
            GetPixel=Lambda Image1,aLines=Len(Raster)-Len(Rasterline), blines=-Len(Rasterline) (x,y) ->{
                  where=alines+3*x+blines*y
                  =color(Eval(image1, where+2 as byte), Eval(image1, where+1 as byte), Eval(image1, where as byte))
            }
            StrDib$=Lambda$ Image1, Raster -> {
                  =Eval$(Image1, 0, Len(Raster))
            }
            CopyImage=Lambda Image1 (image$) -> {
                  if left$(image$,12)=Eval$(Image1, 0, 24 ) Then  {
                         Return Image1, 0:=Image$
                  } Else Error "Can't Copy Image"
            }
            Export2File=Lambda Image1, x, y (f) -> {
                  \\ use this between open and close
                  Print #f, "P3"
                  Print #f,"# Created using M2000 Interpreter"
                  Print #f, x;" ";y
                  Print #f, 255
                  x2=x-1
                  where=24
                  For y1= 0 to y-1 {
                        a$=""
                        For x1=0 to x2 {
                              Print #f, a$;Eval(Image1, where+2 as byte);" ";
                              Print #f,  Eval(Image1, where+1 as byte);" ";
                              Print #f,  Eval(Image1, where as byte);
                              where+=3
                              a$=" "
                        }
                        Print #f
                        m=where mod 4
                        if m<>0 then where+=4-m
                  }
            }
            if valid(F) then {
                  'load  RGB values form file
                  x0=x-1
                  where=24
                        For y1=y-1 to 0 {
                              do {
                                          Line Input #f, aline$
                              } until left$(aline$,1)<>"#"
                              flush   ' empty stack
                              Stack aline$  ' place all values to stack as FIFO
                              For x1=0 to x0 {
                                    \\ now read from stack using Number
                                    Return Image1, 0!where+2:=Number as byte, 0!where+1:=Number as byte, 0!where:=Number as byte
                                    where+=3
                              }
                              m=where mod 4
                              if m<>0 then where+=4-m
                        }
            }
            Group Bitmap {
                  SetPixel=SetPixel
                  GetPixel=GetPixel
                  Image$=StrDib$
                  Copy=CopyImage
                  ToFile=Export2File
            }
            =Bitmap
      }
      A=Bitmap(10, 10)
      Call A.SetPixel(5,5, color(128,0,255))
      Open "A.PPM" for Output as #F
            Call A.ToFile(F)
      Close #f
      Open "A.PPM" for Input as #F
      Try {
            C=Bitmap(f)
            Copy 400*twipsx,200*twipsy use C.Image$()
      }
      Close #f
      ' is the same as this one
        Open "A.PPM" for Input as #F
            Line Input #f,  p3$
            If p3$="P3" Then {
                  Line Input #f, Comment$
                  if left$(Comment$,1)="#" then {
                        Line Input #f, Dimension$
                  } else  Dimension$=Comment$
                  Long x=Val(piece$(Dimension$," ")(0))
                  Long y=Val(piece$(Dimension$," ")(1))
                  do {
                        Line Input #f, P255$
                  } until left$(P255$, 1)<>"#"
                  If not P255$="255" then Error "Not proper ppm format"
                  B=Bitmap(x, y)
                  x0=x-1
                  For y1=y-1 to 0 {
                        do {
                                    Line Input #f, aline$
                        } until left$(aline$,1)<>"#"
                        flush   ' empty stack
                        Stack aline$  ' place all values to stack as FIFO
                        For x1=0 to x0 {
                              \\ now read from stack
                              Read red, green, blue
                              Call B.setpixel(x1, y1, Color(red, green, blue))
                        }
                  }
            }
      Close #f
      If valid("B") then  Copy 200*twipsx,200*twipsy use B.Image$()
}
Checkit


```


=={{header|Mathematica}}/ {{header|Wolfram Language}}==

```Mathematica
Import["file.ppm","PPM"]

```



## Lua


```lua
function Read_PPM( filename )
    local fp = io.open( filename, "rb" )
    if fp == nil then return nil end

    local data = fp:read( "*line" )
    if data ~= "P6" then return nil end

    repeat
        data = fp:read( "*line" )
    until string.find( data, "#" ) == nil

    local image = {}
    local size_x, size_y

    size_x = string.match( data, "%d+" )
    size_y = string.match( data, "%s%d+" )

    data = fp:read( "*line" )
    if tonumber(data) ~= 255 then return nil end

    for i = 1, size_x do
        image[i] = {}
    end

    for j = 1, size_y do
        for i = 1, size_x do
            image[i][j] = { string.byte( fp:read(1) ), string.byte( fp:read(1) ), string.byte( fp:read(1) ) }
        end
    end

    fp:close()

    return image
end
```



## Nim


```nim
import strutils

proc readPPM(f: TFile): Image =
  if f.readLine != "P6":
    raise newException(E_base, "Invalid file format")

  var line = ""
  while f.readLine(line):
    if line[0] != '#':
      break

  var parts = line.split(" ")
  result = img(parseInt parts[0], parseInt parts[1])

  if f.readLine != "255":
    raise newException(E_base, "Invalid file format")

  var
    arr: array[256, int8]
    read = f.readBytes(arr, 0, 256)
    pos = 0

  while read != 0:
    for i in 0 .. < read:
      case pos mod 3
      of 0: result.pixels[pos div 3].r = arr[i].uint8
      of 1: result.pixels[pos div 3].g = arr[i].uint8
      of 2: result.pixels[pos div 3].b = arr[i].uint8
      else: discard

      inc pos

    read = f.readBytes(arr, 0, 256)
```



## OCaml



```ocaml
let read_ppm ~filename =
  let ic = open_in filename in
  let line = input_line ic in
  if line <> "P6" then invalid_arg "not a P6 ppm file";
  let line = input_line ic in
  let line =
    try if line.[0] = '#'  (* skip comments *)
    then input_line ic
    else line
    with _ -> line
  in
  let width, height =
    Scanf.sscanf line "%d %d" (fun w h -> (w, h))
  in
  let line = input_line ic in
  if line <> "255" then invalid_arg "not a 8 bit depth image";
  let all_channels =
    let kind = Bigarray.int8_unsigned
    and layout = Bigarray.c_layout
    in
    Bigarray.Array3.create kind layout 3 width height
  in
  let r_channel = Bigarray.Array3.slice_left_2 all_channels 0
  and g_channel = Bigarray.Array3.slice_left_2 all_channels 1
  and b_channel = Bigarray.Array3.slice_left_2 all_channels 2
  in
  for y = 0 to pred height do
    for x = 0 to pred width do
      r_channel.{x,y} <- (input_byte ic);
      g_channel.{x,y} <- (input_byte ic);
      b_channel.{x,y} <- (input_byte ic);
    done;
  done;
  close_in ic;
  (all_channels,
   r_channel,
   g_channel,
   b_channel)
```


and converting a given color file to grayscale:

```ocaml
let () =
  let img = read_ppm ~filename:"logo.ppm" in
  let img = to_color(to_grayscale ~img) in
  output_ppm ~oc:stdout ~img;
;;
```

sending the result to <tt>stdout</tt> allows to see the result without creating a temporary file sending it through a pipe to the '''display''' utility of ''ImageMagick'':
 ocaml script.ml | display -


## Oz

The read function in module <code>"BitmapIO.oz"</code>:

```oz
functor
import
   Bitmap
   Open
export
   Read
   %% Write
define
   fun {Read Filename}
      F = {New Open.file init(name:Filename)}

      fun {ReadColor8 _}
	 Bytes = {F read(list:$ size:3)}
      in
	 {List.toTuple color Bytes}
      end

      fun {ReadColor16 _}
	 Bytes = {F read(list:$ size:6)}
      in
	 {List.toTuple color {Map {PairUp Bytes} FromBytes}}
      end
   in
      try
	 Magic = {F read(size:2 list:$)}
	 if Magic \= "P6" then raise bitmapIO(read unsupportedFormat(Magic)) end end
	 Width = {ReadNumber F}
	 Height = {ReadNumber F}
	 MaxVal = {ReadNumber F}
	 MaxVal =< 0xffff = true
	 Reader = if MaxVal =< 0xff then ReadColor8 else ReadColor16 end
	 B = {Bitmap.new Width Height}
      in
	 {Bitmap.transform B Reader}
	 B
      finally
	 {F close}
      end
   end

   fun {ReadNumber F}
      Ds
   in
      {SkipWS F}
      Ds = for collect:Collect break:Break do
	      [C] = {F read(list:$ size:1)}
	   in
	      if {Char.isDigit C} then {Collect C}
	      else {Break}
	      end
	   end
      {SkipWS F}
      {String.toInt Ds}
   end

   proc {SkipWS F}
      [C] = {F read(list:$ size:1)}
   in
      if {Char.isSpace C} then {SkipWS F}
      elseif C == &# then
	 {SkipLine F}
      else
	 {F seek(whence:current offset:~1)}
      end
   end

   proc {SkipLine F}
      [C] = {F read(list:$ size:1)}
   in
      if C \= &\n andthen  C \= &\r then {SkipLine F} end
   end

   fun {PairUp Xs}
      case Xs of X1|X2|Xr then [X1 X2]|{PairUp Xr}
      [] nil then nil
      end
   end

   fun {FromBytes [C1 C2]}
      C1 * 0x100 + C2
   end

   %% Omitted: Write
end
```


The actual task:

```oz
declare
  [BitmapIO Grayscale] = {Module.link ['BitmapIO.ozf' 'Grayscale.ozf']}

  B = {BitmapIO.read "image.ppm"}
  G = {Grayscale.toGraymap B}
in
  {BitmapIO.write {Grayscale.fromGraymap G} "greyimage.ppm"}
```



## Perl


{{libheader|Imlib2}}


```perl
#! /usr/bin/perl

use strict;
use Image::Imlib2;

my $img = Image::Imlib2->load("out0.ppm");

# let's do something with it now
$img->set_color(255, 255, 255, 255);
$img->draw_line(0,0, $img->width,$img->height);
$img->image_set_format("png");
$img->save("out1.png");

exit 0;
```




## Perl 6

{{works with|Rakudo|2017.09}}
Uses pieces from [[Bitmap#Perl_6| Bitmap]], [[Bitmap/Write_a_PPM_file#Perl_6| Write a PPM file]] and [[Grayscale_image#Perl_6| Grayscale image]] tasks. Included here to make a complete, runnable program.


```perl6
class Pixel { has UInt ($.R, $.G, $.B) }
class Bitmap {
    has UInt ($.width, $.height);
    has Pixel @.data;
}

role PGM {
    has @.GS;
    method P5 returns Blob {
	"P5\n{self.width} {self.height}\n255\n".encode('ascii')
	~ Blob.new: self.GS
    }
}

sub load-ppm ( $ppm ) {
    my $fh    = $ppm.IO.open( :enc('ISO-8859-1') );
    my $type  = $fh.get;
    my ($width, $height) = $fh.get.split: ' ';
    my $depth = $fh.get;
    Bitmap.new( width => $width.Int, height => $height.Int,
      data => ( $fh.slurp.ords.rotor(3).map:
        { Pixel.new(R => $_[0], G => $_[1], B => $_[2]) } )
    )
}

sub grayscale ( Bitmap $bmp ) {
    $bmp.GS = map { (.R*0.2126 + .G*0.7152 + .B*0.0722).round(1) min 255 }, $bmp.data;
}

my $filename = './camelia.ppm';

my Bitmap $b = load-ppm( $filename ) but PGM;

grayscale($b);

'./camelia-gs.pgm'.IO.open(:bin, :w).write: $b.P5;
```


See [https://github.com/thundergnat/rc/blob/master/img/camelia.png camelia], and [https://github.com/thundergnat/rc/blob/master/img/camelia-gs.png camelia-gs] images. (converted to .png as .ppm format is not widely supported).


## Phix

Based on [[Bitmap/Read_a_PPM_file#Euphoria|Euphoria]], requires write_ppm() from [[Bitmap/Write_a_PPM_file#Phix|Write_a_PPM_file]], to_gray from [[Grayscale_image#Phix|Grayscale_image]]

```Phix
function read_ppm(sequence filename)
sequence image, line
integer dimx, dimy, maxcolor
atom fn = open(filename, "rb")
    if fn<0 then
        return -1 -- unable to open
    end if
    line = gets(fn)
    if line!="P6\n" then
        return -1 -- only ppm6 files are supported
    end if
    line = gets(fn)
    {{dimx,dimy}} = scanf(line,"%d %d%s")
    line = gets(fn)
    {{maxcolor}} = scanf(line,"%d%s")
    image = repeat(repeat(0,dimy),dimx)
    for y=1 to dimy do
        for x=1 to dimx do
            image[x][y] = getc(fn)*#10000 + getc(fn)*#100 + getc(fn)
        end for
    end for
    close(fn)
    return image
end function

sequence img = read_ppm("Lena.ppm")
    img = to_gray(img)
    write_ppm("LenaGray.ppm",img)
```



## PicoLisp


```PicoLisp
(de ppmRead (File)
   (in File
      (unless (and `(hex "5036") (rd 2))  # P6
         (quit "Wrong file format" File) )
      (rd 1)
      (let (DX 0  DY 0  Max 0  C)
         (while (>= 9 (setq C (- (rd 1) `(char "0"))) 0)
            (setq DX (+ (* 10 DX) C)) )
         (while (>= 9 (setq C (- (rd 1) `(char "0"))) 0)
            (setq DY (+ (* 10 DY) C)) )
         (while (>= 9 (setq C (- (rd 1) `(char "0"))) 0)
            (setq Max (+ (* 10 Max) C)) )
         (prog1
            (make (do DY (link (need DX))))
            (for Y @
               (map
                  '((X) (set X (list (rd 1) (rd 1) (rd 1))))
                  Y ) ) ) ) ) )
```

Read a color image "img.ppm", convert and write to "img.pgm":

```PicoLisp
(pgmWrite (ppm->pgm (ppmRead "img.ppm")) "img.pgm")
```



## PL/I


```PL/I

/* BITMAP FILE: read in a file in PPM format, P6 (binary). 14/5/2010 */
test: procedure options (main);
   declare (m, n, max_color, i, j) fixed binary (31);
   declare ch character (1), ID character (2);
   declare 1 pixel union,
            2 color bit(24) aligned,
            2 primary_colors,
               3 R char (1),
               3 G char (1),
               3 B char (1);
   declare in file record;

   open file (in) title ('/IMAGE.PPM,TYPE(FIXED),RECSIZE(1)' ) input;

   call get_char;
   ID = ch;
   call get_char;
   substr(ID, 2,1) = ch;
   /* Read in the dimensions of the image */
   call get_integer (m);
   call get_integer (n);

   /* Read in the maximum color size used */
   call get_integer (max_color);
      /* The previous call reads in ONE line feed or CR or other terminator */
      /* character. */

begin;
   declare image (0:m-1,0:n-1) bit (24);

   do i = 0 to hbound(image, 1);
      do j = 0 to hbound(image,2);
         read file (in) into (R);
         read file (in) into (G);
         read file (in) into (B);
         image(i,j) = color;
      end;
   end;
end;

get_char: procedure;
   do until (ch ^= ' ');
      read file (in) into (ch);
   end;
end get_char;

get_integer: procedure (value);
   declare value fixed binary (31);

   do until (ch = ' ');
      read file (in) into (ch);
   end;
   value = 0;
   do until (is_digit(ch));
      value = value*10 + ch;
      read file (in) into (ch);
   end;
end get_integer;

is_digit: procedure (ch) returns (bit(1));
   declare ch character (1);
   return (index('0123456789', ch) > 0);
end is_digit;
end test;
```



## PureBasic


```PureBasic
Structure PPMColor
  r.c
  g.c
  b.c
EndStructure

Procedure LoadImagePPM(Image, file$)
  ; Author Roger Rösch (Nickname Macros)
  IDFile = ReadFile(#PB_Any, file$)
  If IDFile
    If CreateImage(Image, 1, 1)
      Format$ = ReadString(IDFile)
      ReadString(IDFile) ; skip comment
      Dimensions$ = ReadString(IDFile)
      w           = Val(StringField(Dimensions$, 1, " "))
      h           = Val(StringField(Dimensions$, 2, " "))
      ResizeImage(Image, w, h)
      StartDrawing(ImageOutput(Image))
      max = Val(ReadString(IDFile))           ; Maximal Value for a color
      Select Format$
        Case "P3" ; File in ASCII format
          ; Exract everey number remaining in th file into an array using an RegEx
          Stringlen = Lof(IDFile) - Loc(IDFile)
          content$  = Space(Stringlen)
          Dim color.s(0)
          ReadData(IDFile, @content$, Stringlen)
          CreateRegularExpression(1, "\d+")
          ExtractRegularExpression(1, content$, color())
          ; Plot color information on our empty Image
          For y = 0 To h - 1
            For x = 0 To w - 1
              pos = (y*w + x)*3
              r=Val(color(pos))*255 / max
              g=Val(color(pos+1))*255 / max
              b=Val(color(pos+2))*255 / max
              Plot(x, y, RGB(r,g,b))
            Next
          Next
        Case "P6" ;File In binary format
          ; Read whole bytes into a buffer because its faster than reading single ones
          Bufferlen = Lof(IDFile) - Loc(IDFile)
          *Buffer   = AllocateMemory(Bufferlen)
          ReadData(IDFile, *Buffer, Bufferlen)
          ; Plot color information on our empty Image
          For y = 0 To h - 1
            For x = 0 To w - 1
              *color.PPMColor = pos + *Buffer
              Plot(x, y, RGB(*color\r*255 / max, *color\g*255 / max, *color\b*255 / max))
              pos + 3
            Next
          Next
      EndSelect
      StopDrawing()
      ; Return 1 if successfully loaded to behave as other PureBasic functions
      ProcedureReturn 1
    EndIf
  EndIf
EndProcedure
```


To complete the task, the following code should be added to the above fragment and to the PureBasic solutions for [[Grayscale_image#PureBasic|Grayscale image]] and [[Bitmap/Write_a_PPM_file#PureBasic|Write a PPM file]]

```PureBasic
Define file.s, file2.s, image = 3
file = OpenFileRequester("Select source image file", "", "PPM image (*.ppm)|*.ppm", 0)
If file And LCase(GetExtensionPart(file)) = "ppm"
  LoadImagePPM(image, file)
  ImageGrayout(image)
  file2 = Left(file, Len(file) - Len(GetExtensionPart(file))) + "_grayscale." + GetExtensionPart(file)
  SaveImageAsPPM(image, file2, 1)
EndIf
```



## Python

{{works with|Python|3.1}}

Extending the example given [[Basic_bitmap_storage#Alternative_version|here]]

```python
# With help from http://netpbm.sourceforge.net/doc/ppm.html

# String masquerading as ppm file (version P3)
import io

ppmtxt = '''P3
# feep.ppm
4 4
15
 0  0  0    0  0  0    0  0  0   15  0 15
 0  0  0    0 15  7    0  0  0    0  0  0
 0  0  0    0  0  0    0 15  7    0  0  0
15  0 15    0  0  0    0  0  0    0  0  0
'''


def tokenize(f):
    for line in f:
        if line[0] != '#':
            for t in line.split():
                yield t

def ppmp3tobitmap(f):
    t = tokenize(f)
    nexttoken = lambda : next(t)
    assert 'P3' == nexttoken(), 'Wrong filetype'
    width, height, maxval = (int(nexttoken()) for i in range(3))
    bitmap = Bitmap(width, height, Colour(0, 0, 0))
    for h in range(height-1, -1, -1):
        for w in range(0, width):
            bitmap.set(w, h, Colour( *(int(nexttoken()) for i in range(3))))

    return bitmap

print('Original Colour PPM file')
print(ppmtxt)
ppmfile = io.StringIO(ppmtxt)
bitmap = ppmp3tobitmap(ppmfile)
print('Grey PPM:')
bitmap.togreyscale()
ppmfileout = io.StringIO('')
bitmap.writeppmp3(ppmfileout)
print(ppmfileout.getvalue())


'''
The print statements above produce the following output:

Original Colour PPM file
P3
# feep.ppm
4 4
15
 0  0  0    0  0  0    0  0  0   15  0 15
 0  0  0    0 15  7    0  0  0    0  0  0
 0  0  0    0  0  0    0 15  7    0  0  0
15  0 15    0  0  0    0  0  0    0  0  0

Grey PPM:
P3
# generated from Bitmap.writeppmp3
4 4
11
    0  0  0    0  0  0    0  0  0    4  4  4
    0  0  0   11 11 11    0  0  0    0  0  0
    0  0  0    0  0  0   11 11 11    0  0  0
    4  4  4    0  0  0    0  0  0    0  0  0

'''
```



## Racket


```racket

#lang racket
(require racket/draw)

(define (read-ppm port)
  (parameterize ([current-input-port port])
    (define magic (read))
    (define width (read))
    (define height (read))
    (define maxcol (read))
    (define bm (make-object bitmap% width height))
    (define dc (new bitmap-dc% [bitmap bm]))
    (send dc set-smoothing 'unsmoothed)
    (define (adjust v) (* 255 (/ v maxcol)))
    (for/list ([x width])
      (for/list ([y height])
        (define red (read))
        (define green (read))
        (define blue (read))
        (define color (make-object color% (adjust red) (adjust green) (adjust blue)))
        (send dc set-pen color 1 'solid)
        (send dc draw-point x y)))
    bm))

```




## REXX

The     input file         '''Lenna50.ppm'''         is a '''PPM''' format of

the input file    '''Lenna50.jpg'''    used elsewhere on Rosetta Code.

This REXX program handles alternative delimiters as well as comments within the PPM header.

```rexx
/*REXX program reads a PPM formatted image file,  and creates a gray─scale image of it. */
parse arg iFN oFN                                /*obtain optional argument from the CL.*/
if iFN=='' | iFN==","  then  iFN= 'Lenna50'      /*Not specified?  Then use the default.*/
if oFN=='' | oFN==","  then  oFN= 'greyscale'    /* "      "         "   "   "     "    */
iFID= iFN'.ppm';             oFID= oFN'.ppm'     /*complete the  input and output  FIDs.*/
call charin iFID, 1, 0                           /*set the position of the input file.  */
y=charin(iFID, , copies(9, digits() ) )          /*read the entire input file  ───►  X  */
parse var  y   id  3 c 4 3 width height # pixels /*extract header info from the PPM hdr.*/
      LF= 'a'x                                   /*define a comment separator  (in hdr).*/      /* ◄─── LF delimiters & comments*/
if c==LF  then do;  commentEND=pos(LF, y, 4)     /*point to the last char in the comment*/      /* ◄─── LF delimiters & comments*/
                    parse var  y   =(commentEND)  +1  width  height          #  pixels          /* ◄─── LF delimiters & comments*/
               end                                                                              /* ◄─── LF delimiters & comments*/
                                                 /* [↓]  has an alternative delimiter?  */      /* ◄─── LF delimiters & comments*/
z=pos(LF, height);  if z\==0  then parse var  height height    =(z)   +1     #  pixels          /* ◄─── LF delimiters & comments*/
z=pos(LF, #     );  if z\==0  then parse var  #      #         =(z)   +1        pixels          /* ◄─── LF delimiters & comments*/
chunk=4000                                       /*chunk size to be written at one time.*/
LenPixels= length(pixels)

  do j=0  for 256;  _=d2c(j);   @._=j;   @@.j=_  /*build two tables for fast conversions*/
  end   /*j*/

call charout oFID, ,  1                          /*set the position of the output file. */
call charout oFID, id || width height #' '       /*write the header followed by a blank.*/
!=1
    do until !>=LenPixels;            $=         /*$:      partial output string so far.*/
      do !=!  by 3  for chunk                    /*chunk:  # pixels converted at 1 time.*/
      parse var pixels  =(!)  r +1   g +1   b +1 /*obtain the next  RGB  of a PPM pixel.*/
      if r==''  then leave                       /*has the end─of─string been reached?  */
      _=(.2126*@.r  + .7152*@.g  + .0722*@.b )%1 /*an integer RGB greyscale of a pixel. */
      $=$  ||  @@._  ||  @@._  ||  @@._          /*lump (grey)  R G B  pixels together. */
      end   /*!*/                                /* [↑]  D2C  converts decimal ───► char*/
    call charout oFID, $                         /*write the next bunch of pixels.      */
    end     /*until*/

call charout oFID                                /*close the output file just to be safe*/
say 'File '       oFID       " was created."     /*stick a fork in it,  we're all done. */
```

{{out|output}}

```txt

File  greyscale.ppm  was created.

```



## Ruby

Extending [[Basic_bitmap_storage#Ruby]]

```ruby
class Pixmap
  # 'open' is a class method
  def self.open(filename)
    bitmap = nil
    File.open(filename, 'r') do |f|
      header = [f.gets.chomp, f.gets.chomp, f.gets.chomp]
      width, height = header[1].split.map {|n| n.to_i }
      if header[0] != 'P6' or header[2] != '255' or width < 1 or height < 1
        raise StandardError, "file '#{filename}' does not start with the expected header"
      end
      f.binmode
      bitmap = self.new(width, height)
      height.times do |y|
        width.times do |x|
          # read 3 bytes
          red, green, blue = f.read(3).unpack('C3')
          bitmap[x,y] = RGBColour.new(red, green, blue)
        end
      end
    end
    bitmap
  end
end

# create an image: a green cross on a blue background
colour_bitmap = Pixmap.new(20, 30)
colour_bitmap.fill(RGBColour::BLUE)
colour_bitmap.height.times {|y| [9,10,11].each {|x| colour_bitmap[x,y]=RGBColour::GREEN}}
colour_bitmap.width.times  {|x| [14,15,16].each {|y| colour_bitmap[x,y]=RGBColour::GREEN}}
colour_bitmap.save('testcross.ppm')

# then, convert to grayscale
Pixmap.open('testcross.ppm').to_grayscale!.save('testgray.ppm')
```



## Scala

Uses the [[Basic_bitmap_storage#Scala|Basic Bitmap Storage]] and [[Grayscale_image#Scala|Grayscale Bitmap]] classes.

See also Task [[Write_ppm_file#Scala|Write a PPM File]] for save code.


```scala
import scala.io._
import scala.swing._
import java.io._
import java.awt.Color
import javax.swing.ImageIcon

object Pixmap {
   private case class PpmHeader(format:String, width:Int, height:Int, maxColor:Int)

   def load(filename:String):Option[RgbBitmap]={
      implicit val in=new BufferedInputStream(new FileInputStream(filename))
      val header=readHeader
      if(header.format=="P6")
      {
         val bm=new RgbBitmap(header.width, header.height);
         for(y <- 0 until bm.height; x <- 0 until bm.width; c=readColor)
            bm.setPixel(x, y, c)
         return Some(bm)
      }
      None
   }

   private def readHeader(implicit in:InputStream)={
      var format=readLine

      var line=readLine
      while(line.startsWith("#"))   //skip comments
         line=readLine

      val parts=line.split("\\s")
      val width=parts(0).toInt
      val height=parts(1).toInt
      val maxColor=readLine.toInt

      new PpmHeader(format, width, height, maxColor)
   }

   private def readColor(implicit in:InputStream)=new Color(in.read, in.read, in.read)

   private def readLine(implicit in:InputStream)={
      var out=""
      var b=in.read
      while(b!=0xA){out+=b.toChar; b=in.read}
      out
   }
}
```


Usage:

```scala
object PixmapTest {
   def main(args: Array[String]): Unit = {
      val img=Pixmap.load("image.ppm").get
      val grayImg=BitmapOps.grayscale(img);
      Pixmap.save(grayImg, "image_gray.ppm")

      val mainframe=new MainFrame(){
         title="Test"
         visible=true
         contents=new Label(){
            icon=new ImageIcon(grayImg.image)
         }
      }
   }
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "draw.s7i";
  include "color.s7i";

const func PRIMITIVE_WINDOW: getPPM (in string: fileName) is func
  result
    var PRIMITIVE_WINDOW: aWindow is PRIMITIVE_WINDOW.value;
  local
    var file: ppmFile is STD_NULL;
    var string: line is "";
    var integer: width is 0;
    var integer: height is 0;
    var integer: x is 0;
    var integer: y is 0;
    var color: pixColor is black;
  begin
    ppmFile := open(fileName, "r");
    if ppmFile <> STD_NULL then
      if getln(ppmFile) = "P6" then
        repeat
          line := getln(ppmFile);
        until line = "" or line[1] <> '#';
        read(ppmFile, width);
        readln(ppmFile, height);
        aWindow := newPixmap(width, height);
        for y range 0 to pred(height) do
          for x range 0 to pred(width) do
            pixColor.redLight   := ord(getc(ppmFile));
            pixColor.greenLight := ord(getc(ppmFile));
            pixColor.blueLight  := ord(getc(ppmFile));
          end for;
        end for;
      end if;
      close(ppmFile);
    end if;
  end func;
```



## Tcl

{{libheader|Tk}}
The actual PPM reader is built into the photo image engine:

```tcl
package require Tk

proc readPPM {image file} {
    $image read $file -format ppm
}
```

Thus, to read a PPM, convert it to grayscale, and write it back out again becomes this (which requires Tcl 8.6 for <code>try</code>/<code>finally</code>); the PPM reader and writer are inlined because they are trivial at the script level:

```tcl
package require Tk

proc grayscaleFile {filename {newFilename ""}} {
    set buffer [image create photo]
    if {$newFilename eq ""} {set newFilename $filename}
    try {
        $buffer read $filename -format ppm
        set w [image width $buffer]
        set h [image height $buffer]
        for {set x 0} {$x<$w} {incr x} {
            for {set y 0} {$y<$h} {incr y} {
                lassign [$buffer get $x $y] r g b
                set l [expr {int(0.2126*$r + 0.7152*$g + 0.0722*$b)}]
                $buffer put [format "#%02x%02x%02x" $l $l $l] -to $x $y
            }
        }
        $buffer write $newFilename -format ppm
    } finally {
        image delete $buffer
    }
}
```


However, the Tk library also has built-in the ability to convert code to grayscale directly during the saving of an image to a file, leading to this minimal solution:

```tcl
package require Tk

proc grayscaleFile {filename {newFilename ""}} {
    set buffer [image create photo]
    if {$newFilename eq ""} {set newFilename $filename}
    try {
        $buffer read $filename -format ppm
        $buffer write $newFilename -format ppm -grayscale
    } finally {
        image delete $buffer
    }
}
```



## UNIX Shell

{{works with|ksh93}}
Ref: [[Bitmap#UNIX Shell]]

Add the following functions to the <tt>RGBColor_t</tt> type

```bash
    function setrgb {
        _.r=$1
        _.g=$2
        _.b=$3
    }
    function grayscale {
        integer x=$(( round( 0.2126*_.r + 0.7152*_.g + 0.0722*_.b ) ))
        _.r=$x
        _.g=$x
        _.b=$x
    }
```


Add the following function to the <tt>Bitmap_t</tt> type

```bash
    function grayscale {
        RGBColor_t c
        for ((y=0; y<_.height; y++)); do
            for ((x=0; x<_.width; x++)); do
                c.setrgb ${_.data[y][x]}
                c.grayscale
                _.data[y][x]=$(c.to_s)
            done
        done
    }

    function read {
        exec 4<"$1"
        typeset filetype
        read -u4 filetype
        if [[ $filetype != "P3" ]]; then
            print -u2 "error: I can only read P3 type PPM files"
        else
            read -u4 _.width _.height
            integer maxval
            read -u4 maxval
            integer x y r g b
            typeset -a bytes
            for ((y=0; y<_.height; y++)); do
                read -u4 -A bytes
                for ((x=0; x<_.width; x++)); do
                    r=${bytes[3*x+0]}
                    g=${bytes[3*x+1]}
                    b=${bytes[3*x+2]}
                    if (( r > maxval || g > maxval || b > maxval )); then
                        print -u2 "error: invalid color ($r $g $b), max=$maxval"
                        return 1
                    fi
                    _.data[y][x]="$r $g $b"
                done
            done
        fi
        exec 4<&-
    }
```


Now we can:

```bash
Bitmap_t c
c.read "$HOME/tmp/bitmap.ppm"
c.to_s

if [[ $(c.to_s) == $(cat "$HOME/tmp/bitmap.ppm") ]]; then
    echo looks OK
else
    echo something is wrong
fi

c.grayscale
c.to_s
c.write "$HOME/tmp/bitmap_g.ppm"
```



## Vedit macro language


```vedit
//   Load a PPM file
//     @10 = filename
//   On return:
//     #10 points to buffer containing pixel data,
//     #11 = width,  #12 = height.

:LOAD_PPM:
File_Open(@10)
BOF
Search("|X", ADVANCE)		// skip "P6"
#11 = Num_Eval(ADVANCE)		// #11 = width
Match("|X", ADVANCE)		// skip separator
#12 = Num_Eval(ADVANCE)		// #12 = height
Match("|X", ADVANCE)
Search("|X", ADVANCE)		// skip maxval (assume 255)
Del_Block(0,CP)			// remove the header
Return
```


Example of usage. In addition to LOAD_PPM routine above, you need routine RGB_TO_GRAYSCALE from [[Grayscale image]] and routine SAVE_PPM from [[Write ppm file]].

```vedit
// Load RGB image
Reg_Set(10, "|(USER_MACRO)\example.ppm")
Call("LOAD_PPM")

// Convert to grayscale
#10 = Buf_Num
Call("RGB_TO_GRAYSCALE")
Buf_Switch(#10) Buf_Quit(OK)

// Convert to RGB
Call("GRAYSCALE_TO_RGB")

// Save the image
Reg_Set(10, "|(USER_MACRO)\example_gray.ppm")
Call("SAVE_PPM")

// Cleanup and exit
Buf_Switch(#20) Buf_Quit(OK)
return
```



## XPL0

The simplicity of redirecting an input file on the command line doesn't
work for files that contain binary data ($03 will abort a program). Image
files larger than 1280x1024 are clipped to the screen dimensions.


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

func OpenInFile;                \Open for input the file typed on command line
int  CpuReg, Handle;
char CmdTail($80);
[CpuReg:= GetReg;
Blit(CpuReg(11), $81, CpuReg(12), CmdTail, $7F);       \get copy of command line
Trap(false);                    \turn off error trapping
Handle:= FOpen(CmdTail, 0);     \open named file for input
FSet(Handle, ^I);               \assign file to input device 3
OpenI(3);                       \initialize input buffer pointers
if GetErr then return false;
Trap(true);
return true;
];

int  C, X, Y, Width, Height, Max, Lum;
real Red, Green, Blue;
[if not OpenInFile then [Text(0, "File not found");  exit];
if ChIn(3)#^P or ChIn(3)#^6 then [Text(0, "Not P6 PPM file");  exit];
repeat  loop    [C:= ChIn(3);
                if C # ^# then quit;
                repeat C:= ChIn(3) until C=$0A\EOL\;
                ];
until   C>=^0 & C<=^9;
Backup;                         \back up so IntIn re-reads first digit
Width:=  IntIn(3);              \(skips any whitespace)
Height:= IntIn(3);
Max:=    IntIn(3) + 1;          \(255/15=17; 256/16=16)
case of
  Width<= 640 & Height<=480: SetVid($112);
  Width<= 800 & Height<=600: SetVid($115);
  Width<=1024 & Height<=768: SetVid($118)
other SetVid($11B);             \1280x1024
for Y:= 0 to Height-1 do
    for X:= 0 to Width-1 do
        [Red  := float(ChIn(3)*256/Max) * 0.21; \convert color to grayscale
         Green:= float(ChIn(3)*256/Max) * 0.72;
         Blue := float(ChIn(3)*256/Max) * 0.07;
         Lum:= fix(Red) + fix(Green) + fix(Blue);
         Point(X, Y, Lum<<16 + Lum<<8 + Lum);
        ];
X:= ChIn(1);                    \wait for keystroke
SetVid(3);                      \restore normal text display
]
```



## Yabasic


```Yabasic
sub readPPM(f$)
	local ff, x, y, t$, dcol$, wid, hei

	if f$ = "" print "No PPM file name indicate." : return false

	ff = open (f$, "rb")
	if not ff print "File ", f$, " not found."  : return false

	input #ff t$, wid, hei, dcol$

	if t$ = "P6" then
		open window wid, hei

		for x = 0 to hei - 1
		 	for y = 0 to wid - 1
		 		color peek(#ff), peek(#ff), peek(#ff)
		 		dot y, x
		 	next y
		next x

		close #ff
	else
		print "File is NOT PPM P6 type." : return false
	end if

	return true
end sub
```



## zkl

{{trans|FBSL}}
Read a colored PPM file, convert it (in place) to grayscale and write the new image back to disk under a different name. Sanity checks are omitted.

I used a slightly different image from what is shown, but the results are the same.
[[File:FBSLLena.png|right]]

```zkl
//24-bpp P6 PPM solution:
image:=File("lena.ppm","rb").read();
start:=image.find("\n255\n")+5;   // Get sizeof PPM header

foreach n in ([start..image.len()-1,3]){  //  Transform color triplets
   r,g,b:=image[n,3];  // Read colors stored in RGB order
   l:=(0.2126*r + 0.7152*g + 0.0722*b).toInt(); // Derive luminance
   image[n,3]=T(l,l,l);
}

File("lenaGrey.ppm","wb").write(image);
```




{{omit from|AWK}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|PARI/GP}}
