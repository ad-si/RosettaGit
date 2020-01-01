+++
title = "File extension is in extensions list"
description = ""
date = 2019-10-12T10:21:37Z
aliases = []
[extra]
id = 17841
[taxonomies]
categories = []
tags = []
+++

{{draft task}} [[Category:File_System_Operations]] [[Category:String_manipulation]]

[[wp:Filename extension|Filename extensions]] are a rudimentary but commonly used way of identifying files types.

{{task heading}}

Given an arbitrary filename and a list of extensions, tell whether the filename has one of those extensions.

Notes:
* The check should be case insensitive.
* The extension must occur at the very end of the filename, and be immediately preceded by a dot (<code>.</code>).
* You may assume that none of the given extensions are the empty string, and none of them contain a dot. Other than that they may be arbitrary strings.


;''Extra credit:''
: Allow extensions to contain dots. This way, users of your function/program have full control over what they consider as the extension in cases like:
   <big>archive.tar.gz</big>
: Please state clearly whether or not your solution does this.

{{task heading|Test cases}}

The following test cases all assume this list of extensions:   <code>zip</code>, <code>rar</code>, <code>7z</code>, <code>gz</code>, <code>archive</code>, <code>A##</code>

{| class="wikitable"
|-
! Filename
! Result
|-
| <code>MyData.a##</code>       || true
|-
| <code>MyData.tar.Gz</code>    || true
|-
| <code>MyData.gzip</code>      || false
|-
| <code>MyData.7z.backup</code> || false
|-
| <code>MyData...</code>        || false
|-
| <code>MyData</code>           || false
|}

If your solution does the extra credit requirement, add <code>tar.bz2</code>  to the list of extensions, and check the following additional test cases:

{| class="wikitable"
|-
! Filename
! Result
|-
| <code>MyData_v1.0.tar.bz2</code> || true
|-
| <code>MyData_v1.0.bz2</code>     || false
|}

{{task heading|Motivation}}

Checking if a file is in a certain category of file formats with known extensions (e.g. archive files, or image files) is a common problem in practice, and may be approached differently from extracting and outputting an arbitrary extension ''(see e.g. <code>FileNameExtensionFilter</code> in Java)''.

It also requires less assumptions about the format of an extension, because the calling code can decide what extensions are valid.

For these reasons, this task exists in addition to the [[Extract file extension]] task.

{{task heading|Related tasks}}

* [[Extract file extension]]
* [[String matching]]

<hr>


## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}

```algol68
# returns the length of str                                         #
OP   LENGTH = ( STRING str )INT: ( UPB str - LWB str ) + 1;
# returns TRUE if str ends with ending FALSE otherwise              #
PRIO ENDSWITH = 9;
OP   ENDSWITH = ( STRING str, STRING ending )BOOL:
     IF  INT str length    = LENGTH str;
         INT ending length = LENGTH ending;
         ending length > str length
     THEN
         # the ending is longer than the string                     #
         FALSE
     ELSE
         # the string is at least as long as the ending             #
         str[ ( str length - ending length ) + 1 : AT 1 ] = ending
     FI # ENDSWITH # ;
# returns str cnverted to upper case                                #
OP   TOUPPER   = ( STRING str )STRING:
     BEGIN
        STRING result := str;
        FOR s pos FROM LWB result TO UPB result DO
            result[ s pos ] := to upper( result[ s pos ] )
        OD;
        result
     END # TOUPPER # ;
# tests whether file name has one of the extensions and returns     #
# the index of the extension in extensions or LWB extensions - 1    #
# if it does not end with one of the extensions                     #
# the tests are not case-sensitive                                  #
PROC has extension in list = ( STRING file name, []STRING extensions )INT:
     BEGIN
        INT    extension number := LWB extensions - 1;
        STRING upper name        = TOUPPER file name;
        FOR pos FROM LWB extensions TO UPB extensions WHILE extension number < LWB extensions DO
            IF upper name ENDSWITH ( "." + TOUPPER extensions[ pos ] )
            THEN
                # found the extension                              #
                extension number := pos
            FI
        OD;
        extension number
     END # has extension # ;
# test the has extension in list procedure                          #
PROC test has extension in list = ( STRING file name, []STRING extensions, BOOL expected result )VOID:
     IF INT extension number = has extension in list( file name, extensions );
        extension number < LWB extensions
     THEN
        # the file does not have one of the extensions              #
        print( ( file name
               , " does not have an extension in the list "
               , IF expected result THEN "NOT AS EXPECTED" ELSE "" FI
               , newline
               )
             )
     ELSE
        # the file does have one of the extensions                  #
        print( ( file name
               , " has extension """
               , extensions[ extension number ]
               , """ "
               , IF NOT expected result THEN "NOT AS EXPECTED" ELSE "" FI
               , newline
               )
             )
     FI # test has extension in list # ;
# the extensions for the task                                       #
[]STRING task extensions = ( "zip", "rar", "7z", "gz", "archive", "A##" );
# test the file names in the standard task                          #
test has extension in list( "MyData.a##",          task extensions, TRUE  );
test has extension in list( "MyData.tar.Gz",       task extensions, TRUE  );
test has extension in list( "MyData.gzip",         task extensions, FALSE );
test has extension in list( "MyData.7z.backup",    task extensions, FALSE );
test has extension in list( "MyData...",           task extensions, FALSE );
test has extension in list( "MyData",              task extensions, FALSE );
# the extensions for the extra credit                               #
[]STRING ec extensions = ( "zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2" );
# test the file names in the extra credit                           #
test has extension in list( "MyData_v1.0.tar.bz2", ec extensions,   TRUE  );
test has extension in list( "MyData_v1.0.bz2",     ec extensions,   FALSE )
```

{{out}}

```txt

MyData.a## has extension "A##"
MyData.tar.Gz has extension "gz"
MyData.gzip does not have an extension in the list
MyData.7z.backup does not have an extension in the list
MyData... does not have an extension in the list
MyData does not have an extension in the list
MyData_v1.0.tar.bz2 has extension "tar.bz2"
MyData_v1.0.bz2 does not have an extension in the list
```



## Arturo



```arturo
FileExtensions #("zip" "rar" "7z" "gz" "archive" "A##")

hasExtension [file]{
	lowercased $(map FileExtensions { lowercase & })
	return $(contains lowercased $(lowercase $(replace $(getExtension file) "." "")))
}

files #("MyData.a##" "MyData.tar.Gz" "MyData.gzip" "MyData.7z.backup" "MyData..." "MyData")

loop files {
	print & + " => hasExtension: " + $(hasExtension &)
}
```


{{out}}


```txt
MyData.a## => hasExtension: true
MyData.tar.Gz => hasExtension: true
MyData.gzip => hasExtension: false
MyData.7z.backup => hasExtension: false
MyData... => hasExtension: false
MyData => hasExtension: false
```



## AWK


This solution meets the extended criteria stated in the problem.


```AWK

# syntax: GAWK -f FILE_EXTENSION_IS_IN_EXTENSIONS_LIST.AWK
BEGIN {
    n = split("zip,rar,7z,gz,archive,A##,tar.bz2", arr, ",")
    for (i=1; i<=n; i++) {
      ext_arr[tolower(arr[i])] = ""
    }
    filenames = "MyData.a##,MyData.tar.Gz,MyData.gzip,MyData.7z.backup,MyData...,MyData,MyData_v1.0.tar.bz2,MyData_v1.0.bz2"
    n = split(filenames, fn_arr, ",")

    for (i=1; i<=n; i++) {
      ext_found = ""
      for (ext in ext_arr) {
        if (tolower(fn_arr[i]) ~ (".*\\." ext "$")) {
          ext_found = ext
          break
        }
      }
      ans = (ext_found == "") ? "is not in list" : ("is in list: " ext_found)
      printf("%s extension %s\n", fn_arr[i], ans)
    }
    exit(0)
}
```

{{out}}

```txt

MyData.a## extension is in list: a##
MyData.tar.Gz extension is in list: gz
MyData.gzip extension is not in list
MyData.7z.backup extension is not in list
MyData... extension is not in list
MyData extension is not in list
MyData_v1.0.tar.bz2 extension is in list: tar.bz2
MyData_v1.0.bz2 extension is not in list

```



## Batch File

This solution does '''not''' contain support for dots in extension names.

```dos

@echo off
setlocal enabledelayedexpansion

set "extensions=.zip .rar .7z .gz .archive .A##"

:loop
if "%~1"=="" exit /b
set onlist=0

for %%i in (%extensions%) do if /i "%~x1"=="%%i" set onlist=1

if %onlist%==1 (
  echo Filename: "%~1" ^| Extension: "%~x1" ^| TRUE
) else (
  echo Filename: "%~1" ^| Extension: "%~x1" ^| FALSE
)

shift
goto loop

```

{{out}}

```txt

Filename: "MyData.a##" | Extension: ".a##" | TRUE
Filename: "MyData.tar.Gz" | Extension: ".Gz" | TRUE
Filename: "MyData.gzip" | Extension: ".gzip" | FALSE
Filename: "MyData.7z.backup" | Extension: ".backup" | FALSE
Filename: "MyData..." | Extension: "" | FALSE
Filename: "MyData" | Extension: "" | FALSE

```




## C



### C: dots allowed

There is no magic in extensions: they are just trailing characters in names.

```c
/*
 * File extension is in extensions list (dots allowed).
 *
 * This problem is trivial because the so-called extension is simply the end
 * part of the name.
 */

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <locale.h>
#include <string.h>

#ifdef _Bool
#include <stdbool.h>
#else
#define bool int
#define true  1
#define false 0
#endif

/*
 * The implemented algorithm is not the most efficient one: for N extensions
 * of length M it has the cost O(N * M).
 */
int checkFileExtension(char* fileName, char* fileExtensions)
{
    char* fileExtension = fileExtensions;

    if ( *fileName )
    {
        while ( *fileExtension )
        {
            int fileNameLength = strlen(fileName);
            int extensionLength = strlen(fileExtension);
            if ( fileNameLength >= extensionLength )
            {
                char* a = fileName + fileNameLength - extensionLength;
                char* b = fileExtension;
                while ( *a && toupper(*a++) == toupper(*b++) )
                    ;
                if ( !*a )
                    return true;
            }
            fileExtension += extensionLength + 1;
        }
    }
    return false;
}

void printExtensions(char* extensions)
{
    while( *extensions )
    {
        printf("%s\n", extensions);
        extensions += strlen(extensions) + 1;
    }
}

bool test(char* fileName, char* extension, bool expectedResult)
{
    bool result = checkFileExtension(fileName,extension);
    bool returnValue = result == expectedResult;
    printf("%20s  result: %-5s  expected: %-5s  test %s\n",
        fileName,
        result         ? "true"   : "false",
        expectedResult ? "true"   : "false",
        returnValue    ? "passed" : "failed" );
    return returnValue;
}

int main(void)
{
    static char extensions[] = ".zip\0.rar\0.7z\0.gz\0.archive\0.A##\0.tar.bz2\0";

    setlocale(LC_ALL,"");

    printExtensions(extensions);
    printf("\n");

    if ( test("MyData.a##",         extensions,true )
    &&   test("MyData.tar.Gz",      extensions,true )
    &&   test("MyData.gzip",        extensions,false)
    &&   test("MyData.7z.backup",   extensions,false)
    &&   test("MyData...",          extensions,false)
    &&   test("MyData",             extensions,false)
    &&   test("MyData_v1.0.tar.bz2",extensions,true )
    &&   test("MyData_v1.0.bz2",    extensions,false)
    &&   test("filename",           extensions,false)
    )
        printf("\n%s\n", "All tests passed.");
    else
        printf("\n%s\n", "Last test failed.");

    printf("\n%s\n", "press enter");
    getchar();
    return 0;
}
```

{{out}}

```txt

.zip
.rar
.7z
.gz
.archive
.A##
.tar.bz2

          MyData.a##  result: true   expected: true   test passed
       MyData.tar.Gz  result: true   expected: true   test passed
         MyData.gzip  result: false  expected: false  test passed
    MyData.7z.backup  result: false  expected: false  test passed
           MyData...  result: false  expected: false  test passed
              MyData  result: false  expected: false  test passed
 MyData_v1.0.tar.bz2  result: true   expected: true   test passed
     MyData_v1.0.bz2  result: false  expected: false  test passed
            filename  result: false  expected: false  test passed


All tests passed.

press enter

```



### C: another solution

According to this, an extension is whatever comes after the '''last''' dot. Dotless filename won't match anything.


```c
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

/* this should be the way we check if we can use posix/bsd strcasecmp */
#if !defined(_BSD_SOURCE) && !defined(_DEFAULT_SOURCE)
#include <ctype.h>
int strcasecmp(const char *s1, const char *s2) {
  for(; tolower(*s1) == tolower(*s2); ++s1, ++s2)
    if(*s1 == 0)
      return 0;
  return *(unsigned char *)s1 < *(unsigned char *)s2 ? -1 : 1;
}
#else
#include <strings.h>
#endif


bool ext_is_in_list(const char *filename, const char *extlist[])
{
  size_t i;

  const char *ext = strrchr(filename, '.');
  if (ext) {
    for (i = 0; extlist[i] != NULL; ++i) {
      if (strcasecmp(ext, extlist[i]) == 0)
	return true;
    }
  }

  return false;
}


// testing
const char *fnames[] = {
    "text.txt",
    "text.TXT",
    "test.tar.gz",
    "test/test2.exe",
    "test\\test2.exe",
    "test",
    "a/b/c\\d/foo",
    "foo.c",
    "foo.C",
    "foo.C++",
    "foo.c#",
    "foo.zkl",
    "document.pdf",
    NULL
};

const char *exts[] = {
    ".txt", ".gz", ".bat", ".c",
    ".c++", ".exe", ".pdf",
    NULL
};

int  main(void)
{
  size_t i;

  for (i = 0; fnames[i]; ++i) {
    printf("%s: %s\n", fnames[i],
	   ext_is_in_list(fnames[i], exts) ? "yes" : "no");
  }

  return 0;
}

```



## D


### Variant 1


```d

import std.stdio;
import std.string;
import std.range;
import std.algorithm;

void main()
{
  auto exts = ["zip", "rar", "7z", "gz", "archive", "A##"];
  auto filenames = ["MyData.a##",
                    "MyData.tar.Gz",
                    "MyData.gzip",
                    "MyData.7z.backup",
                    "MyData...",
                    "MyData"];

  writeln("extensions: ", exts);
  writeln;

  foreach(filename; filenames)
  {
    string extension = filename.drop(filename.lastIndexOf(".") + 1).toLower;

    bool found;
    foreach(ext; exts)
    {
      if (extension == ext.toLower)
      {
        found = true;
        break;
      }
    }

    writeln(filename, " : ", found);
  }



```


{{out}}

```txt

extensions: ["zip", "rar", "7z", "gz", "archive", "A##"]

MyData.a## : true
MyData.tar.Gz : true
MyData.gzip : false
MyData.7z.backup : false
MyData... : false
MyData : false


```




### Variant 2


```d

import std.stdio;
import std.string;
import std.range;
import std.algorithm;

void main()
{
  auto exts = ["zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"];
  auto filenames = ["MyData.a##",
                    "MyData.tar.Gz",
                    "MyData.gzip",
                    "MyData.7z.backup",
                    "MyData...",
                    "MyData",
                    "MyData_v1.0.tar.bz2",
                    "MyData_v1.0.bz2"];

  writeln("extensions: ", exts);

  writeln;
  foreach(filename; filenames)
  {
    bool found;
    foreach(ext; exts)
    {
      if (filename.toLower.endsWith("." ~ ext.toLower))
      {
        found = true;
        break;
      }
    }

    writeln(filename, " : ", found);
  }

}


```


{{out}}

```txt

extensions: ["zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"]

MyData.a## : true
MyData.tar.Gz : true
MyData.gzip : false
MyData.7z.backup : false
MyData... : false
MyData : false
MyData_v1.0.tar.bz2 : true
MyData_v1.0.bz2 : false


```



## Factor

This solution allows dots in file extensions.

```factor
USING: formatting kernel qw sequences splitting unicode ;
IN: rosetta-code.file-extension-list

CONSTANT: extensions qw{ zip rar 7z gz archive A## tar.bz2 }
CONSTANT: filenames qw{
    MyData.a## MyData.tar.Gz MyData.gzip MyData.7z.backup
    MyData... MyData MyData_v1.0.tar.bz2 MyData_v1.0.bz2
}

: ext-in-list? ( filename list -- ? )
    [ >lower "." split ] dup [ map ] curry bi*
    [ tail? t = ] with find nip >boolean ;

extensions "List of file extensions: %[%s, %]\n\n" printf
"File extension in extensions list?\n" printf
filenames [
    dup extensions ext-in-list? "%19s  %u\n" printf
] each
```

{{out}}

```txt

List of file extensions: { zip, rar, 7z, gz, archive, A##, tar.bz2 }

File extension in extensions list?
         MyData.a##  t
      MyData.tar.Gz  t
        MyData.gzip  f
   MyData.7z.backup  f
          MyData...  f
             MyData  f
MyData_v1.0.tar.bz2  t
    MyData_v1.0.bz2  f

```



## Fortran

The plan is to use the extractor function for file name extensions that was defined in [[Extract_file_extension#Fortran|another problem]] to obtain the extension's text, then use that text to index a character string of approved extensions. These are defined with the period included, but no extension's text can include a period - all start with a period and continue with letters or digits only - so the specification of a list of such items need not mess about with quotes and commas and so forth. The deed is done via function EXTIN(FNAME,LIST), but there is a lot of support stuff in the absence of an existing library to call upon.

When the task was revised to include a # as a possible character in the file name extension part, the scan in FEXT that allowed only certain characters via GOODEXT required changing. One possibility was to increase the size of GOODEXT so as to encompass the three characters in ODDITIES, that includes a #. But it also includes a : (all this is from a different context) and in file names, under DOS, this character is special. So, all-in-all, it seemed better to retreat from exclusions and simply allow all characters, as per the original specification. This done, the function EXTIN that checked that the extension was one of a specified set seemed no longer restricted to file name extensions, so instead it became FOUND(TEXT,LIST) and the period, the only special character now, is used as the delimiter in LIST.

Petty details include the list employing capitals only, as the match is to not distinguish capital from lower case letters, so in the usual way the candidate extension's text is converted to capitals. The list could be subjected to UPCASE, but it is bad form to damage what might be a constant, and one possibly in read-only storage at that. An internal working copy could be made which would then be fed to UPCASE, except that this would be a waste on every invocation. A further trick involves appending a period to the candidate text so that for example ".JP" becomes ".JP." - otherwise a ".JP" would be found in the sequence ".JPG" which would be wrong, so as a result, the list of texts must have a period appended to its last entry, otherwise it would not be findable. Again, this could be done internally, via <code>INDEX(LIST//".",EXT(1:L)//".")</code> at a run-time cost.

Some systems supply an UPCASE (or similar name) to convert text that hopefully would run faster than this example, which relies on searching for the position in a list of letters. A very common alternative is to calculate using character code numerical values, via something like
```Fortran
        IT = ICHAR(TEXT(I:I)) - ICHAR("a")	!More symbols precede "a" than "A".
        IF (IT.GE.0 .AND. IT.LE.25) TEXT(I:I) = CHAR(IT + ICHAR("A"))	!In a-z? Convert!
```
 except that this relies on the letters having contiguous character codes, and in EBCDIC they don't - other symbols are mixed in. (Honest!) Faster still would be to use the character code to index an array of 256 pre-computed values.

A final annoyance is the presence of trailing spaces because character variables are of fixed size and so must be made "surely long enough" for the longest expectation. This may not cause trouble on output as spaces look just as blank as blank space, but they may well cause trouble in the internal tests. Thus integer function LSTNB reports the last non-blank, and so a test text can be presented with no trailing spaces, as in TEST(I)(1:LSTNB(TEST(I))) or similar. With Fortran 2003, there is a facility for redefining the sizes of character variables on-the-fly so that this problem can be evaded.

The MODULE protocol is employed for the convenience of not having to respecify the type of the functions in every calling routine, and also to facilitate the collection of types of characters. Otherwise, prior to F90 there would have to be various COMMON statements, or routines would each simply respecify whatever character sets they needed.
```Fortran
      MODULE TEXTGNASH	!Some text inspection.
       CHARACTER*10 DIGITS		!Integer only.
       CHARACTER*11 DDIGITS		!With a full stop masquerading as a decimal point.
       CHARACTER*13 SDDIGITS		!Signed decimal digits.
       CHARACTER*4  EXPONENTISH		!With exponent parts.
       CHARACTER*17 NUMBERISH		!The complete mix.
       CHARACTER*16 HEXLETTERS		!Extended for base sixteen.
       CHARACTER*62 DIGILETTERS		!File nameish but no .
       CHARACTER*26 LITTLELETTERS,BIGLETTERS	!These are well-known.
       CHARACTER*52 LETTERS		!The union thereof.
       CHARACTER*66 NAMEISH		!Allowing digits and . and _ as well.
       CHARACTER*3  ODDITIES		!And allow these in names also.
       CHARACTER*1 CHARACTER(72)	!Prepare a work area.
       EQUIVALENCE			!Whose components can be fingered.
     1  (CHARACTER( 1),EXPONENTISH,NUMBERISH),	!Start with numberish symbols that are not nameish.
     2  (CHARACTER( 5),SDDIGITS),		!Since the sign symbols are not nameish.
     3  (CHARACTER( 7),DDIGITS,NAMEISH),	!Computerish names might incorporate digits and a .
     4  (CHARACTER( 8),DIGITS,HEXLETTERS,DIGILETTERS),	!A proper name doesn't start with a digit.
     5  (CHARACTER(18),BIGLETTERS,LETTERS),	!Just with a letter.
     6  (CHARACTER(44),LITTLELETTERS),		!The second set.
     7  (CHARACTER(70),ODDITIES)		!Tack this on the end.
       DATA EXPONENTISH /"eEdD"/	!These on the front.
       DATA SDDIGITS /"+-.0123456789"/	!Any of these can appear in a floating point number.
       DATA BIGLETTERS    /"ABCDEFGHIJKLMNOPQRSTUVWXYZ"/	!Simple.
       DATA LITTLELETTERS /"abcdefghijklmnopqrstuvwxyz"/	!Subtly different.
       DATA ODDITIES /"_:#"/		!Allow these in names also. This strains := usage!

       CHARACTER*62 GOODEXT	!These are all the characters allowed
       EQUIVALENCE (CHARACTER(8),GOODEXT)	!Starts with the first digit.
       INTEGER MEXT		!A fixed bound.
       PARAMETER (MEXT = 28)	!This is perfect.
       CONTAINS
        INTEGER FUNCTION LSTNB(TEXT)  !Sigh. Last Not Blank.
Concocted yet again by R.N.McLean (whom God preserve) December MM.
Code checking reveals that the Compaq compiler generates a copy of the string and then finds the length of that when using the latter-day intrinsic LEN_TRIM. Madness!
Can't   DO WHILE (L.GT.0 .AND. TEXT(L:L).LE.' ')	!Control chars. regarded as spaces.
Curse the morons who think it good that the compiler MIGHT evaluate logical expressions fully.
Crude GO TO rather than a DO-loop, because compilers use a loop counter as well as updating the index variable.
Comparison runs of GNASH showed a saving of ~3% in its mass-data reading through the avoidance of DO in LSTNB alone.
Crappy code for character comparison of varying lengths is avoided by using ICHAR which is for single characters only.
Checking the indexing of CHARACTER variables for bounds evoked astounding stupidities, such as calculating the length of TEXT(L:L) by subtracting L from L!
Comparison runs of GNASH showed a saving of ~25-30% in its mass data scanning for this, involving all its two-dozen or so single-character comparisons, not just in LSTNB.
         CHARACTER*(*),INTENT(IN):: TEXT	!The bumf. If there must be copy-in, at least there need not be copy back.
         INTEGER L		!The length of the bumf.
          L = LEN(TEXT)		!So, what is it?
    1     IF (L.LE.0) GO TO 2	!Are we there yet?
          IF (ICHAR(TEXT(L:L)).GT.ICHAR(" ")) GO TO 2	!Control chars are regarded as spaces also.
          L = L - 1		!Step back one.
          GO TO 1		!And try again.
    2     LSTNB = L		!The last non-blank, possibly zero.
         RETURN			!Unsafe to use LSTNB as a variable.
        END FUNCTION LSTNB	!Compilers can bungle it.

        SUBROUTINE UPCASE(TEXT)	!In the absence of an intrinsic...
Converts any lower case letters in TEXT to upper case...
Concocted yet again by R.N.McLean (whom God preserve) December MM.
Converting from a DO loop evades having both an iteration counter to decrement and an index variable to adjust.
Could use the character code to index an array, instead of searching a sequence...
         CHARACTER*(*) TEXT	!The stuff to be modified.
         INTEGER I,L,IT	!Fingers.
          L = LEN(TEXT)		!Get a local value, in case LEN engages in oddities.
          I = L			!Start at the end and work back..
    1     IF (I.LE.0) RETURN 	!Are we there yet? Comparison against zero should not require a subtraction.
          IT = INDEX(LITTLELETTERS,TEXT(I:I))	!Well?
          IF (IT .GT. 0) TEXT(I:I) = BIGLETTERS(IT:IT)	!One to convert?
          I = I - 1		!Back one.
          GO TO 1		!Inspect..
        END SUBROUTINE UPCASE	!Easy. In EBCDIC, letters are NOT contiguous, and other symbols appear.

        CHARACTER*(MEXT) FUNCTION FEXT(FNAME)	!Return the file extension part.
         CHARACTER*(*) FNAME	!May start with the file's path name blather.
         INTEGER L1,L2		!Fingers to the text.
          L2 = LEN(FNAME)	!The last character of the file name.
          L1 = L2		!Starting at the end...
   10     IF (L1.GT.0) THEN	!Damnit, can't rely on DO WHILE(safe & test)
            IF (FNAME(L1:L1).NE.".") THEN	!So do the two parts explicitly.
              L1 = L1 - 1		!Well, that was a valid character for an extension.
              GO TO 10			!So, move back one and try again.
            END IF		!Until a period is found.
            L1 = L1 - 1		!Here. Thus include the period.
            GO TO 20		!And escape.
          END IF		!Keep on moving back.
          L1 = L2		!If we're here, no period was found.
   20     FEXT = FNAME(L1 + 1:L2)	!The text of the extension.
        END FUNCTION FEXT	!Possibly, blank.

        LOGICAL FUNCTION FOUND(TEXT,LIST)	!Is TEXT found in the LIST?
         CHARACTER*(*) TEXT	!The text.
         CHARACTER*(*) LIST	!A sequence, separated by the periods. Like ".EXE.TXT.etc."
         CHARACTER*(LEN(TEXT)) EXT	!A scratchpad of sufficient size.
          L = LSTNB(TEXT)	!Find its last non-blank.
          IF (L.LE.0) THEN	!A null text?
            FOUND = .FALSE.		!Yep. Can't be in the list.
           ELSE			!Otherwise,
            EXT(1:L) = TEXT(1:L)	!A copy I can damage.
            CALL UPCASE(EXT(1:L))	!Simplify.
            FOUND = INDEX(LIST,EXT(1:L)//".") .GT. 0	!Found somewhere?
          END IF		!The period can't be a character in an extension name.
        END FUNCTION FOUND	!So, possibilities collapse.
      END MODULE TEXTGNASH	!Enough for this.

      PROGRAM POKE
      USE TEXTGNASH
      INTEGER I,LEX,LFN
      INTEGER TESTS
      PARAMETER (TESTS = 12)
      CHARACTER*80 TEST(TESTS)	!A collection.
      CHARACTER*(MEXT) EXT
      DATA TEST/
     1 "Picture.jpg",
     2 "http://mywebsite.com/picture/image.png",
     3 "myuniquefile.longextension",
     4 "IAmAFilenameWithoutExtension",
     5 "/path/to.my/file",
     6 "file.odd_one",
     7 "Mydata.a##",
     8 "Mydata.tar.Gz",
     9 "MyData.gzip",
     o "MyData.7z.backup",
     1 "Mydata...",
     2 "Mydata"/
      CHARACTER*(*) IMAGE	!A sequence of approved texts, delimited by .
      PARAMETER (IMAGE = ".ZIP.RAR.7Z.GZ.ARCHIVE.A##.")	!All in capitals, and ending with a . too.

      WRITE (6,1) IMAGE
    1 FORMAT ("To note file name extensions that are amongst ",A,/
     1 "File name",40X,"Extension",7X,"Test")

      DO I = 1,TESTS	!Step through the candidates.
       LFN = LSTNB(TEST(I))		!Thus do without trailing spaces.
       EXT = FEXT(TEST(I)(1:LFN))	!Grab the file name's extension text.
       LEX = LSTNB(EXT)			!More trailing spaces.
       WRITE (6,2) TEST(I)(1:LFN),EXT(1:LEX),FOUND(EXT(1:LEX),IMAGE)
    2  FORMAT (A48,A16,L)	!Produces tidy columns, aligned right.
      END DO		!On to the next.
      END

```

The previous results were when only "image" style files were approved. The approval is no longer so restrictive.

```txt

To note file name extensions that are amongst .ZIP.RAR.7Z.GZ.ARCHIVE.A##.
File name                                        Extension       Test
                                     Picture.jpg            .jpg F
          http://mywebsite.com/picture/image.png            .png F
                      myuniquefile.longextension  .longextension F
                    IAmAFilenameWithoutExtension                 F
                                /path/to.my/file        .my/file F
                                    file.odd_one        .odd_one F
                                      Mydata.a##            .a## T
                                   Mydata.tar.Gz             .Gz T
                                     MyData.gzip           .gzip F
                                MyData.7z.backup         .backup F
                                       Mydata...               . F
                                          Mydata                 F

```

The approval function relies on a period being special and that no extension contains one, as is stated in the specification. Thus, the likes of "tar.bz2" can't be added to the list of approved extensions. To do so would require some way of removing context dependency from within the list of approved tests, for instance by converting it to some sort of list of elements each of which has a length. So, if there were no more than 255 texts in a list, then character one would state their number, followed by the length of the first text (up to 255) then that number of characters, then the length of the second text, and so on. Then function FOUND would be fully general at the price of a more difficult initialisation of its list.

Similarly, the scan of a file name to find the extension part relies on no extension containing a period. If the caller were to determine the text of the extension (rather than using FEXT, which stops with the rightmost period) then whatever results could be presented to function FOUND without difficulty.

Users of Windows systems may have the option "Hide extensions for known file types" unticked, but even so, "shortcut" files will not have their file name extension of <code>.lnk</code> shown...


## Go

This allows extensions to include a dot and, in the case of success, displays the extension matched (ignoring case) in the list. In the case of failure, the actual (minimum) file extension is displayed.

```go
package main

import (
    "fmt"
    "strings"
)

var extensions = []string{"zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"}

func fileExtInList(filename string) (bool, string) {
    filename2 := strings.ToLower(filename)
    for _, ext := range extensions {
        ext2 := "." + strings.ToLower(ext)
        if strings.HasSuffix(filename2, ext2) {
            return true, ext
        }
    }
    s := strings.Split(filename, ".")
    if len(s) > 1 {
        t := s[len(s)-1]
        if t != "" {
            return false, t
        } else {
            return false, "<empty>"
        }
    } else {
        return false, "<none>"
    }
}

func main() {
    fmt.Println("The listed extensions are:")
    fmt.Println(extensions, "\n")
    tests := []string{
        "MyData.a##", "MyData.tar.Gz", "MyData.gzip",
        "MyData.7z.backup", "MyData...", "MyData",
        "MyData_v1.0.tar.bz2", "MyData_v1.0.bz2",
    }
    for _, test := range tests {
        ok, ext := fileExtInList(test)
        fmt.Printf("%-20s => %-5t  (extension = %s)\n", test, ok, ext)
    }
}
```


{{out}}

```txt

The listed extensions are:
[zip rar 7z gz archive A## tar.bz2]

MyData.a##           => true   (extension = A##)
MyData.tar.Gz        => true   (extension = gz)
MyData.gzip          => false  (extension = gzip)
MyData.7z.backup     => false  (extension = backup)
MyData...            => false  (extension = <empty>)
MyData               => false  (extension = <none>)
MyData_v1.0.tar.bz2  => true   (extension = tar.bz2)
MyData_v1.0.bz2      => false  (extension = bz2)

```



## Haskell



```Haskell
import Data.List
import qualified Data.Char as Ch

toLower :: String -> String
toLower = map Ch.toLower

isExt :: String -> [String] -> Bool
isExt filename extensions = any (`elem` (tails . toLower $ filename)) $ map toLower extensions
```


The code defining isExt could be done point free:

```Haskell
isExt filename = any (`elem` (tails . toLower $ filename)) . map toLower
```


Overcoming the one-liner urge on behalf of being more comprehensible would give:

```Haskell
isExt filename extensions = any (`elem` allTails) lowerExtensions
                            where allTails = tails . toLower $ filename
			          lowerExtensions = map toLower extensions
```


Given definition handles composite extensions as well.

'''Testing:'''


```Haskell
main = mapM_ (\f -> putStr(f ++ "\t") >> print (f `isExt` extensions)) files
  where
    files = [ "MyData.a##"
            , "MyData.tar.Gz"
            , "MyData.gzip"
            , "MyData.7z.backup"
            , "MyData..."
            , "MyData"
            , "MyData_v1.0.tar.bz2"
            , "MyData_v1.0.bz2" ]
    extensions = ["zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"]
```

{{Out}}

```txt
λ> main
MyData.a##	True
MyData.tar.Gz	True
MyData.gzip	True
MyData.7z.backup	False
MyData...	False
MyData	False
MyData_v1.0.tar.bz2	True
MyData_v1.0.bz2	False
```



We can also express this directly in terms of the '''isSuffix''' function, taking care not to reproduce the small glitch in the draft above, which shows a false positive for '''zip''' in '''.gzip''' (see task description bullet 2).


```haskell
import Data.List (find, isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)


extensionFound :: [String] -> String -> Maybe String
extensionFound xs fp = find (`isSuffixOf` fp) (('.' :) <$> xs)


-- TESTS --------------------------------------------------
main :: IO ()
main =
  putStrLn $
  fTable
    "Any matching extensions found:\n"
    id
    (fromMaybe "n/a")
    (extensionFound
       (lcased ["zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"]))
    (lcased
       [ "MyData.a##"
       , "MyData.tar.Gz"
       , "MyData.gzip"
       , "MyData.7z.backup"
       , "MyData..."
       , "MyData"
       , "MyData_v1.0.tar.bz2"
       , "MyData_v1.0.bz2"
       ])


-- STRINGS ------------------------------------------------

fTable :: String -> (a -> String) -> (b -> String) -> (a -> b) -> [a] -> String
fTable s xShow fxShow f xs =
  let w = maximum (length . xShow <$> xs)
      rjust n c = (drop . length) <*> (replicate n c ++)
  in unlines $
     s : fmap (((++) . rjust w ' ' . xShow) <*> ((" -> " ++) . fxShow . f)) xs

lcased :: [String] -> [String]
lcased = fmap (fmap toLower)
```

{{Out}}

```txt
Any matching extensions found:

         mydata.a## -> .a##
      mydata.tar.gz -> .gz
        mydata.gzip -> n/a
   mydata.7z.backup -> n/a
          mydata... -> n/a
             mydata -> n/a
mydata_v1.0.tar.bz2 -> .tar.bz2
    mydata_v1.0.bz2 -> n/a
```



## J


'''Solution:'''

```j
isSuffix=: -~&# = {:@I.@E.
isExt=: ('.'&,&.>@[ ([: +./ isSuffix&(tolower@>)/) boxopen@])
```


'''Usage:'''

```j
   Exts=:  <;._1 ' zip rar 7z gz archive A## tar.bz2'
   TestFiles=: <;._1 ' MyData.a## MyData.tar.Gz MyData.gzip MyData.7z.backup MyData... MyData MyData_v1.0.tar.bz2 MyData_v1.0.bz2'
   Exts isExt TestFiles
1 1 0 0 0 0 1 0
```



## Java



{{works with|Java|1.5+}}

```java5
import java.util.Arrays;
import java.util.Comparator;

public class FileExt{
	public static void main(String[] args){
		String[] tests = {"text.txt", "text.TXT", "test.tar.gz", "test/test2.exe", "test\\test2.exe", "test", "a/b/c\\d/foo"};
		String[] exts = {".txt",".gz","",".bat"};

		System.out.println("Extensions: " + Arrays.toString(exts) + "\n");

		for(String test:tests){
			System.out.println(test +": " + extIsIn(test, exts));
		}
	}

	public static boolean extIsIn(String test, String... exts){
		int lastSlash = Math.max(test.lastIndexOf('/'), test.lastIndexOf('\\')); //whichever one they decide to use today
		String filename = test.substring(lastSlash + 1);//+1 to get rid of the slash or move to index 0 if there's no slash

		//end of the name if no dot, last dot index otherwise
		int lastDot = filename.lastIndexOf('.') == -1 ? filename.length() : filename.lastIndexOf('.');
		String ext = filename.substring(lastDot);//everything at the last dot and after is the extension

		Arrays.sort(exts);//sort for the binary search

		return Arrays.binarySearch(exts, ext, new Comparator<String>() { //just use the built-in binary search method
			@Override                                                //it will let us specify a Comparator and it's fast enough
			public int compare(String o1, String o2) {
				return o1.compareToIgnoreCase(o2);
			}
		}) >= 0;//binarySearch returns negative numbers when it's not found
	}
}

```

{{out}}

```txt
Extensions: [.txt, .gz, , .bat]

text.txt: true
text.TXT: true
test.tar.gz: true
test/test2.exe: false
test\test2.exe: false
test: true
a/b/c\d/foo: true
```


### Using FileNameExtensionFilter

{{works with|Java|6+}}
This version is the same as the main version only replace the definition for <code>extIsIn</code> with:

```java5
public static boolean extIsIn(String test, String... exts){
	for(int i = 0; i < exts.length; i++){
		exts[i] = exts[i].replaceAll("\\.", "");
	}
	return (new FileNameExtensionFilter("extension test", exts)).accept(new File(test));
}
```

It would be one line if not for the dot requirement. <code>FileNameExtensionFilter</code> requires that the extensions have no dots. It also requires that the extensions contain characters (i.e. not the empty string) so that test would need to be removed.


## jq


{{update|jq}}

{{works with|jq|1.4}}

```jq
# Input: filename
# Output: if the filename ends with one of the extensions (ignoring case), output that extension; else output null.
# Assume that the list of file extensions consists of lower-case strings, including a leading period.
def has_extension(list):
  def ascii_downcase: explode | map( if 65 <= . and . <= 90 then . + 32  else . end) | implode;
  rindex(".") as $ix
  | if $ix then (.[$ix:] | ascii_downcase) as $ext
       | if list | index($ext) then $ext else null end
    else null
    end;
```

'''Examples:'''

```jq
("c:", "txt", "text.txt", "text.TXT", "foo.c", "foo.C++", "document.pdf")
| has_extension([".txt", ".c"]) as $ext
| if $ext then "\(.) has extension \($ext)"
  else "\"\(.)\" does not have an admissible file extension"
  end
```


{{out}}

```jq
$ jq -r -n -f File_extension_is_in_extensions_list.jq
"c:" does not have an admissible file extension
"txt" does not have an admissible file extension
text.txt has extension .txt
text.TXT has extension .txt
foo.c has extension .c
"foo.C++" does not have an admissible file extension
"document.pdf" does not have an admissible file extension
```



## Julia

{{works with|Julia|0.6}}


```julia
isext(filename, extensions) = any(x -> endswith(lowercase(filename), lowercase(x)), "." .* extensions)

# Test
extensions = ["zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"]
for f in ["MyData.a##", "MyData.tar.Gz", "MyData.gzip", "MyData.7z.backup", "MyData...", "MyData", "MyData_v1.0.tar.bz2", "MyData_v1.0.bz2"]
    @printf("%20s : %5s\n", f, isext(f, extensions))
end
```


{{out}}

```txt
          MyData.a## :  true
       MyData.tar.Gz :  true
         MyData.gzip : false
    MyData.7z.backup : false
           MyData... : false
              MyData : false
 MyData_v1.0.tar.bz2 :  true
     MyData_v1.0.bz2 : false
```



## Kotlin


```scala
// version 1.1

/* implicitly allows for extensions containing dots */
fun String.isFileExtensionListed(extensions: List<String>): Boolean {
    return extensions.any { toLowerCase().endsWith("." + it.toLowerCase()) }
}

fun main(args: Array<String>) {
    val extensions = listOf("zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2")
    val fileNames  = listOf(
        "MyData.a##",
        "MyData.tar.Gz",
        "MyData.gzip",
        "MyData.7z.backup",
        "MyData...",
        "MyData",
        "MyData_v1.0.tar.bz2",
        "MyData_v1.0.bz2"
    )

    for (fileName in fileNames) {
        println("${fileName.padEnd(19)} -> ${fileName.isFileExtensionListed(extensions)}")
    }
}
```


{{out}}

```txt

MyData.a##          -> true
MyData.tar.Gz       -> true
MyData.gzip         -> false
MyData.7z.backup    -> false
MyData...           -> false
MyData              -> false
MyData_v1.0.tar.bz2 -> true
MyData_v1.0.bz2     -> false

```



## Lua


```lua
-- Data declarations
local extentions = {"zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"}
local testCases = {
  "MyData.a##",
  "MyData.tar.Gz",
  "MyData.gzip",
  "MyData.7z.backup",
  "MyData...",
  "MyData",
  "MyData_v1.0.tar.bz2",
  "MyData_v1.0.bz2"
}

-- Return boolean of whether example has a file extension found in extList
function extMatch (extList, example)
  for _, extension in pairs(extList) do
    if example:lower():match("%." .. extension:lower() .. "$") then
      return true
    end
  end
  return false
end

-- Main procedure
for _, case in pairs(testCases) do
  print(case .. ": " .. tostring(extMatch(extentions, case)))
end
```

{{out}}

```txt
MyData.a##: true
MyData.tar.Gz: true
MyData.gzip: false
MyData.7z.backup: false
MyData...: false
MyData: false
MyData_v1.0.tar.bz2: true
MyData_v1.0.bz2: false
```



## Objeck


```objeck

class FileExtension  {
  function : Main(args : String[]) ~ Nil {
    files := ["MyData.a##", "MyData.tar.Gz", "MyData.gzip",
      "MyData.7z.backup", "MyData...", "MyData", "MyData_v1.0.tar.bz2", "MyData_v1.0.bz2"];
    exts := ["zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"];
    each(i : files) {
      HasExt(files[i], exts);
    };
  }

  function : HasExt(file : String, exts : String[]) ~ Nil {
    full_file := file->ToLower();
    each(i : exts) {
      full_ext := ".";
      full_ext += exts[i]->ToLower();
      if(full_file->EndsWith(full_ext)) {
	IO.Console->Print(file)->Print(" has extension \"")->Print(exts[i])->PrintLine("\"");
        return;
      };
    };

    IO.Console->Print(file)->PrintLine(" does not have an extension in the list");
  }
}
```


{{out}}

```txt
MyData.a## has extension "A##"
MyData.tar.Gz has extension "gz"
MyData.gzip does not have an extension in the list
MyData.7z.backup does not have an extension in the list
MyData... does not have an extension in the list
MyData does not have an extension in the list
MyData_v1.0.tar.bz2 has extension "tar.bz2"
MyData_v1.0.bz2 does not have an extension in the list
```



## PARI/GP


{{update|PARI/GP}}

The examples were taken from the [[#AWK|AWK]] code.


```parigp
lower(s)=
{
  my(v=Vecsmall(s));
  for(i=1,#v,
    if(v[i]<91 && v[i]>64, v[i]+=32)
  );
  \\Strchr(v); \\ Use to return a string rather than a t_VECSMALL
  v;
}
checkExt(ext, file)=
{
  ext=apply(lower,ext);
  my(v=lower(file),e);
  for(i=1,#ext,
    e=ext[i];
    if(#v>#e && v[#v-#e+1..#v]==e && v[#v-#e]==46,
      return(1)
    )
  );
}
ext=["txt","gz","bat","c","c++","exe","pdf"];
filenames=["c:","txt","text.txt","text.TXT","test.tar.gz","test/test2.exe","test","foo.c","foo.C","foo.C++","foo.c#","foo.zkl","document.pdf"];
select(f -> checkExt(ext, f), filenames)
```

{{out}}

```txt
%1 = ["text.txt", "text.TXT", "test.tar.gz", "test/test2.exe", "foo.c", "foo.C", "foo.C++", "document.pdf"]
```



## Perl


Does the extra credit requirement.

{{trans|Perl 6}}


```perl
sub check_extension {
    my ($filename, @extensions) = @_;
    my $extensions = join '|', map quotemeta, @extensions;
    scalar $filename =~ / \. (?: $extensions ) $ /xi
}
```


Testing:

```perl
my @extensions = ('zip', 'rar', '7z', 'gz', 'archive', 'A##', 'tar.bz2');
my @files= (
    'MyData.a##', 'MyData.tar.Gz', 'MyData.gzip', 'MyData.7z.backup',
    'MyData...', 'MyData', 'MyData_v1.0.tar.bz2', 'MyData_v1.0.bz2'
);
printf "%-19s - %s\n",
    $_, check_extension($_, @extensions) ? 'true' : 'false'
    for @files;
```


{{out}}

```txt

MyData.a##          - true
MyData.tar.Gz       - true
MyData.gzip         - false
MyData.7z.backup    - false
MyData...           - false
MyData              - false
MyData_v1.0.tar.bz2 - true
MyData_v1.0.bz2     - false

```




## Perl 6


Does the extra credit requirement.


```perl6
sub check-extension ($filename, *@extensions) {
    so $filename ~~ /:i '.' @extensions $/
}

# Testing:

my @extensions = <zip rar 7z gz archive A## tar.bz2>;
my @files= <
    MyData.a##  MyData.tar.Gz  MyData.gzip  MyData.7z.backup  MyData...  MyData
    MyData_v1.0.tar.bz2  MyData_v1.0.bz2
>;
say "{$_.fmt: '%-19s'} - {check-extension $_, @extensions}" for @files;
```


{{out}}

```txt

MyData.a##          - True
MyData.tar.Gz       - True
MyData.gzip         - False
MyData.7z.backup    - False
MyData...           - False
MyData              - False
MyData_v1.0.tar.bz2 - True
MyData_v1.0.bz2     - False

```



## Phix


```Phix
constant extensions = lower({"zip","rar","7z","gz","archive","A##","tar.bz2"})

global function get_known_extension(string filename)
    filename = get_file_name(filename)
    for i=1 to length(filename) do
        if filename[i]='.' then
            string extension = lower(filename[i+1..$])
            if find(extension,extensions) then
                return extension
            end if
        end if
    end for
    return ""
end function

constant tests = {"MyData.a##",
                  "MyData.tar.Gz",
                  "MyData.gzip",
                  "MyData.7z.backup",
                  "MyData...",
                  "MyData",
                  "MyData_v1.0.tar.bz2",
                  "MyData_v1.0.bz2"}

for i=1 to length(tests) do
    string ti = tests[i],
           ext = get_known_extension(ti)
    printf(1,"%-20s %-10s %s\n",{ti,ext,{"true","false"}[2-(find(ext,extensions)!=0)]})
end for
```

{{out}}

```txt

MyData.a##           a##        true
MyData.tar.Gz        gz         true
MyData.gzip                     false
MyData.7z.backup                false
MyData...                       false
MyData                          false
MyData_v1.0.tar.bz2  tar.bz2    true
MyData_v1.0.bz2                 false

```



## Python



```Python

def isExt(fileName, extensions):
  return True in map(fileName.lower().endswith, ("." + e.lower() for e in extensions))

```


and we could also enrich the return value to a Maybe option type, returning Nothing if no file extension match is found, or Just the particular extension, if a match is found:


```python
'''Check for a specific set of file extensions'''


# extensionFound :: [Extension] -> FileName -> Maybe Extension
def extensionFound(xs):
    '''Nothing if no matching extension is found,
       or Just the extension (drawn from xs, and
       a suffix of the filename, immediately following
       a dot character).
    '''
    return lambda fn: find(fn.lower().endswith)(
        ['.' + x.lower() for x in xs]
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Check filenames for a particular set of extensions.'''

    # checkExtension :: FileName -> Maybe Extension
    def checkExtension(fn):
        return extensionFound([
            'zip', 'rar', '7z', 'gz', 'archive', 'A##', 'tar.bz2'
        ])(fn)

    print(
        fTable(__doc__ + ':\n')(str)(str)(
            compose(fromMaybe('n/a'))(checkExtension)
        )([
            'MyData.a##',
            'MyData.tar.Gz',
            'MyData.gzip',
            'MyData.7z.backup',
            'MyData...',
            'MyData',
            'MyData_v1.0.tar.bz2',
            'MyData_v1.0.bz2'
        ])
    )


# GENERIC -------------------------------------------------

# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# find :: (a -> Bool) -> [a] -> Maybe a
def find(p):
    '''Just the first element in the list that matches p,
       or Nothing if no elements match.
    '''
    def go(xs):
        for x in xs:
            if p(x):
                return Just(x)
        return Nothing()
    return lambda xs: go(xs)


# fromMaybe :: a -> Maybe a -> a
def fromMaybe(x):
    '''The default value x if mb is Nothing,
       or the value contained in mb.
    '''
    return lambda mb: x if (
        mb.get('Nothing')
    ) else mb.get('Just')


# DISPLAY -------------------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Check for a specific set of file extensions:

         MyData.a## -> .a##
      MyData.tar.Gz -> .gz
        MyData.gzip -> n/a
   MyData.7z.backup -> n/a
          MyData... -> n/a
             MyData -> n/a
MyData_v1.0.tar.bz2 -> .tar.bz2
    MyData_v1.0.bz2 -> n/a
```



## Racket



```racket

#lang racket

(define extensions '(".zip"
                     ".rar"
                     ".7z"
                     ".gz"
                     ".archive"
                     ".a##"
                     ".tar.bz2"))

(define filenames '("MyData.a##"
                    "MyData.tar.Gz"
                    "MyData.gzip"
                    "MyData.7z.backup"
                    "MyData..."
                    "MyData"
                    "MyData_v1.0.tar.bz2"
                    "MyData_v1.0.bz2"))

(define (string-right s n)
    (if (< (string-length s) n)
        s
        (substring s (- (string-length s) n))))

(define (file-extension-in-list? f lst)
  (let ([lcase (string-downcase f)])
    (ormap (lambda (x) (equal? (string-right lcase (string-length x)) x)) extensions)))

(for ((f (in-list filenames)))
  (printf "~a ~a~%" (~a #:width 20 f) (file-extension-in-list? f extensions)))

```

{{out}}

```txt

MyData.a##           #t
MyData.tar.Gz        #t
MyData.gzip          #f
MyData.7z.backup     #f
MyData...            #f
MyData               #f
MyData_v1.0.tar.bz2  #t
MyData_v1.0.bz2      #f

```



## REXX

This REXX version handles the extra credit requirement.

```rexx
/*REXX program displays  if a filename  has a  known extension  (as per a list of EXTs).*/
$= 'zip rar 7z gz archive A## tar.bz2';  upper $ /*a list of "allowable" file extensions*/
parse arg fn                                     /*obtain optional argument from the CL.*/
@.=                                              /*define the default for the  @. array.*/
if fn\=''  then       @.1 = strip(fn)            /*A filename specified?   Then use it. */
           else do;   @.1 = "MyData.a##"         /*                        else use list*/
                      @.2 = "MyData.tar.Gz"
                      @.3 = "MyData.gzip"
                      @.4 = "MyData.7z.backup"
                      @.5 = "MyData..."
                      @.6 = "MyData"
                      @.7 = "MyData_v1.0.tar.bz2"
                      @.8 = "MyData_v1.0.bz2"
                end
#=words($)

  do j=1  while @.j\==''                         /*traipse through  @ list of file exts.*/
  file=@.j;   upper file                         /*get a filename; and then uppercase it*/

      do k=1  for #  until right(file, length(x) )==x   /*Search $ list, is ext in list?*/
      x=. || word($, k)                          /*construct the extension of the file. */
      end   /*k*/                                /* [↓]  display file, and a nay or yea.*/

  say  right(@.j, 40)     ' '     right( word( "false true",  1 + (k<=#) ),  5)
  end       /*j*/                                /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

                              MyData.a##    true
                           MyData.tar.Gz    true
                             MyData.gzip   false
                        MyData.7z.backup   false
                               MyData...   false
                                  MyData   false
                     MyData_v1.0.tar.bz2    true
                         MyData_v1.0.bz2   false

```



## Ring


```ring

# Project : File extension is in extensions list

extensions = [".zip", ".rar", ".7z", ".gz", ".archive", ".a##", ".tar.bz2"]

filenames = ["MyData.a##", "MyData.tar.gz", "MyData.gzip", "MyData.7z.backup",
                   "MyData...", "MyData", "MyData_v1.0.tar.bz2", "MyData_v1.0.bz2"]

for n = 1 to len(filenames)
    flag = 0
    for m = 1 to len(extensions)
        if right(filenames[n], len(extensions[m])) = extensions[m]
           flag = 1
           see filenames[n] + " -> " + extensions[m] + " -> " + " true" + nl
           exit
        ok
    next
    if flag = 0
       see filenames[n] + " -> " + "false" + nl
    ok
next

```

Output:

```txt

MyData.a## -> .a## ->  true
MyData.tar.gz -> .gz ->  true
MyData.gzip -> false
MyData.7z.backup -> false
MyData... -> false
MyData -> false
MyData_v1.0.tar.bz2 -> .tar.bz2 ->  true
MyData_v1.0.bz2 -> false

```



## Ruby


```ruby
def is_ext(filename, extensions)
  if filename.respond_to?(:each)
    filename.each do |fn|
      is_ext(fn, extensions)
    end
  else
    fndc = filename.downcase
    extensions.each do |ext|
      bool = fndc.end_with?(?. + ext.downcase)
      puts "%20s : %s" % [filename, bool] if bool
    end
  end
end

```

{{out}}

```txt

is_ext ['MyData.a##', 'MyData.tar.Gz', 'MyData.gzip', 'MyData.7z.backup', 'MyData...', 'MyData', 'MyData.tar.bz2'],
       %w(zip rar 7z gz archive A## tar.bz2)
#=>
          MyData.a## : true
       MyData.tar.Gz : true
      MyData.tar.bz2 : true

```



## Rust

Does extra credit.

```Rust
fn main() {
    let exts = ["zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"];
    let filenames = [
        "MyData.a##",
        "MyData.tar.Gz",
        "MyData.gzip",
        "MyData.7z.backup",
        "MyData...",
        "MyData",
        "MyData_v1.0.tar.bz2",
        "MyData_v1.0.bz2",
    ];

    println!("extenstions: {:?}\n", exts);

    for filename in filenames.iter() {
        let check = exts.iter().any(|ext| {
            filename
                .to_lowercase()
                .ends_with(&format!(".{}", ext.to_lowercase()))
        });
        println!("{:20} {}", filename, check);
    }
}

```

{{out}}

```txt

extensions: ["zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2"]

MyData.a##           true
MyData.tar.Gz        true
MyData.gzip          false
MyData.7z.backup     false
MyData...            false
MyData               false
MyData_v1.0.tar.bz2  true
MyData_v1.0.bz2      false

```



## Scala



```Scala
  def isExt(fileName: String, extensions: List[String]): Boolean = {
    extensions.map { _.toLowerCase }.exists { fileName.toLowerCase endsWith "." + _ }
  }

```



## Sidef

{{trans|Perl}}

```ruby
func check_extension(filename, extensions) {
    filename ~~ Regex('\.(' + extensions.map { .escape }.join('|') + ')\z', :i)
}
```


Testing:

```ruby
var extensions = ['zip', 'rar', '7z', 'gz', 'archive', 'A##', 'tar.bz2']

var files = [
    'MyData.a##', 'MyData.tar.Gz', 'MyData.gzip', 'MyData.7z.backup',
    'MyData...', 'MyData', 'MyData_v1.0.tar.bz2', 'MyData_v1.0.bz2'
]

for file in files {
    printf("%-19s - %s\n", file, check_extension(file, extensions))
}
```

{{out}}

```txt

MyData.a##          - true
MyData.tar.Gz       - true
MyData.gzip         - false
MyData.7z.backup    - false
MyData...           - false
MyData              - false
MyData_v1.0.tar.bz2 - true
MyData_v1.0.bz2     - false

```



## Tcl



```tcl


# This example includes the extra credit.
# With a slight variation, a matching suffix can be identified.
# Note that suffixes with two or more dots (ie a dot in suffix) are checked for each case.
# This way, filename.1.txt will match for txt, and filename3.tar.gz.1 will match for tar.gz.1 for example.

# Example input data:
set f_list [list \
                "MyData.a##" \
                MyData.tar.Gz \
                MyData.gzip \
                MyData.7z.backup \
                "MyData..." \
                MyData \
                MyData_v1.0.tar.bz2 \
                MyData_v1.0.bz2 ]
set suffix_input_list [list zip rar 7z gz archive "A##" tar.bz2 ]

# Prefix a dot to any suffix that does not begin with a dot.
set suffix_list [list ]
foreach s $suffix_input_list {
    if { [string range $s 0 0] ne "." } {
        set s2 "."
    } else {
        set s2 ""
    }
    append s2 $s
    lappend suffix_list [string tolower $s2]
}

# Check each filename
foreach filename0 $f_list {
    set filename1 [string tolower [file tail $filename0]]
    set suffix1 [file extension $filename1]
    set file_suffix_list [list $suffix1]
    set filename2 [file rootname $filename1]
    set i 0
    # i is an infinite loop breaker. In case there is some unforseen case..
    while { $filename2 ne "" && $filename2 ne $filename1 && $i < 32} {
        # Another suffix is possible
        set suffix2 [file extension $filename2]
        if { $suffix2 ne "" } {
            # found another suffix
            append suffix2 $suffix1
            lappend file_suffix_list $suffix2
        }
        set suffix1 $suffix2
        set filename1 $filename2
        set filename2 [file rootname $filename2]
        incr i
    }
    set a_suffix_found_p 0
    foreach file_suffix $file_suffix_list {
        if { $file_suffix in $suffix_list } {
            set a_suffix_found_p 1
        }
    }
    puts -nonewline "${filename0}\t"
    if { $a_suffix_found_p } {
        puts "true"
    } else {
        puts "false"
    }
}


```

{{out}}

```txt

MyData.a##	true
MyData.tar.Gz	true
MyData.gzip	false
MyData.7z.backup	false
MyData...	false
MyData	false
MyData_v1.0.tar.bz2	true
MyData_v1.0.bz2	false


```



## zkl


```zkl
fcn hasExtension(fnm){
   var [const] extensions=T(".zip",".rar",".7z",".gz",".archive",".a##");
   nm,ext:=File.splitFileName(fnm)[-2,*].apply("toLower");
   if(extensions.holds(ext)) True;
   else if(ext==".bz2" and ".tar"==File.splitFileName(nm)[-1]) True;
   else False
}
nms:=T("MyData.a##","MyData.tar.Gz","MyData.gzip","MyData.7z.backup",
       "MyData...","MyData",
       "MyData_v1.0.tAr.bz2","MyData_v1.0.bz2");
foreach nm in (nms){ println("%20s : %s".fmt(nm,hasExtension(nm))); }
```

{{out}}

```txt

          MyData.a## : True
       MyData.tar.Gz : True
         MyData.gzip : False
    MyData.7z.backup : False
           MyData... : False
              MyData : False
 MyData_v1.0.tAr.bz2 : True
     MyData_v1.0.bz2 : False

```

