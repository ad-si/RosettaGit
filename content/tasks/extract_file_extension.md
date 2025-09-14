+++
title = "Extract file extension"
description = ""
date = 2019-10-12T10:27:05Z
aliases = []
[extra]
id = 19093
[taxonomies]
categories = ["task"]
languages = [
  "algol_68",
  "algol_w",
  "arturo",
  "awk",
  "batch_file",
  "c",
  "cpp",
  "csharp",
  "emacs_lisp",
  "d",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "gambas",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "lua",
  "objeck",
  "ocaml",
  "oforth",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "sed",
  "sidef",
  "tcl",
  "tuscript",
  "vbscript",
  "visual_basic",
  "zkl",
]
tags = []
+++

[[wp:Filename extension|Filename extensions]] are a rudimentary but commonly used way of identifying files types.

Write a function or program that
* takes one string argument representing the path/URL to a file
* returns the filename extension according to the below specification, or an empty string if the filename has no extension.


If your programming language (or standard library) has built-in functionality for extracting a filename extension, show how it would be used and how exactly its behavior differs from this specification.

For the purposes of this task, a filename extension

* occurs at the very end of the filename
* consists of a period, followed solely by one or more ASCII letters or digits (A-Z, a-z, 0-9)

{| class="wikitable"
|-
! Input
! Output
! Comment
|-
| <code><nowiki>http://example.com/download.tar.gz</nowiki></code>
| <code>.gz</code>
|
|-
| <code>CharacterModel.3DS</code>
| <code>.3DS</code>
|
|-
| <code>.desktop</code>
| <code>.desktop</code>
|
|-
| <code>document</code>
| <code></code>
| ''empty string''
|-
| <code>document.txt_backup</code>
| <code></code>
| ''empty string, because <code>_</code> is not a letter or number''
|-
| <code>/etc/pam.d/login</code>
| <code></code>
| ''empty string, as the period is in the parent directory name rather than the filename''
|}

<hr>


## ALGOL 68


```algol68
# extracts a file-extension from the end of a pathname. The file extension is #
# defined as a dot followed by one or more letters or digits                  #
OP  EXTENSION = ( STRING pathname )STRING:
    IF LWB pathname >= UPB pathname THEN
        # the pathname has 0 or 1 characters and so has no extension          #
        ""
    ELIF NOT isalnum( pathname[ UPB pathname ] ) THEN
        # the final character is not a letter or digit - no extension         #
        ""
    ELSE
        # could have an extension                                             #
        INT    pos := UPB pathname;
        WHILE pos > LWB pathname AND isalnum( pathname[ pos ] ) DO
            pos -:= 1
        OD;
        IF pathname[ pos ] = "." THEN
            # the character before the letters and digits was a "."           #
            pathname[ pos : ]
        ELSE
            # no "." before the letters and digits - no extension             #
            ""
        FI
    FI ; # EXTENSION #

# test the EXTENSION operator                                                 #
PROC test extension = ( STRING pathname, STRING expected extension )VOID:
    BEGIN
        STRING extension = EXTENSION pathname;
        write( ( ( pathname
                 + " got extension: ("
                 + extension
                 + ") "
                 + IF extension = expected extension THEN "" ELSE "NOT" FI
                 + " as expected"
                 )
               , newline
               )
             )
    END ; # text extension #


main:
( test extension( "http://example.com/download.tar.gz", ".gz"      )
; test extension( "CharacterModel.3DS",                 ".3DS"     )
; test extension( ".desktop",                           ".desktop" )
; test extension( "document",                           ""         )
; test extension( "document.txt_backup",                ""         )
; test extension( "/etc/pam.d/login",                   ""         )
)
```

```txt

http://example.com/download.tar.gz got extension: (.gz)  as expected
CharacterModel.3DS got extension: (.3DS)  as expected
.desktop got extension: (.desktop)  as expected
document got extension: ()  as expected
document.txt_backup got extension: ()  as expected
/etc/pam.d/login got extension: ()  as expected
```



## ALGOL W



```algolw
begin
    % extracts a file-extension from the end of a pathname.                   %
    % The file extension is defined as a dot followed by one or more letters  %
    % or digits. As Algol W only has fixed length strings we limit the        %
    % extension to 32 characters and the pathname to 256 (the longest string  %
    % allowed by Algol W)                                                     %
    string(32) procedure extension( string(256) value pathname ) ;
    begin

        integer pathPos;

        % position to the previous character in the pathname                  %
        procedure prev         ; pathPos := pathPos - 1;
        % get the character as pathPos from pathname                          %
        string(1) procedure ch ; pathname( pathPos // 1 );
        % checks for a letter or digit - assumes the letters are contiguous   %
        % in the character set - not true for EBCDIC                          %
        logical   procedure isLetterOrDigit( string(1) value c ) ;
                  ( c <= "z" and c >= "a" ) or ( c <= "Z" and c >= "A" )
                                            or ( c <= "9" and c >= "0" ) ;

        % find the length of the pathname with trailing blanks removed        %
        pathPos := 255;
        while pathPos >= 0 and ch = " " do prev;

        % extract the extension if possible                                   %
        if pathPos <= 0
        then ""       % no extension: 0 or 1 character pathname               %
        else if not isLetterOrDigit( ch )
        then ""       % no extension: last character not a letter/digit       %
        else begin
            while pathPos > 0 and isLetterOrDigit( ch ) do prev;
            if ch not = "."
            then ""   % no extension: letters/digits not preceeded by "."     %
            else begin
                % have an extension                                           %
                string(32) ext;
                ext := " ";
                % algol W substring lengths must be compile-time constants    %
                % hence the loop to copy the extension characters             %
                for charPos := 0 until 31 do begin
                    if pathPos <= 255 then begin
                        ext( charPos // 1 ) := pathname( pathPos // 1 );
                        pathPos := pathPos + 1
                    end
                end for_charPos ;
                ext
            end
        end

    end extension ;


    % test the extension procedure                                            %
    procedure testExtension( string(256) value pathname
                           ; string(32)  value expectedExtension
                           ) ;
    begin
        string(32) ext;
        ext := extension( pathname );
        write( pathname( 0 // 40 )
             , " -> ("
             , ext( 0 // 16 )
             , ") "
             , if ext = expectedExtension then "" else "NOT"
             , " as expected"
             )
    end ; % text extension %

    testExtension( "http://example.com/download.tar.gz", ".gz"      );
    testExtension( "CharacterModel.3DS",                 ".3DS"     );
    testExtension( ".desktop",                           ".desktop" );
    testExtension( "document",                           ""         );
    testExtension( "document.txt_backup",                ""         );
    testExtension( "/etc/pam.d/login",                   ""         );

end.
```

```txt

http://example.com/download.tar.gz       -> (.gz             )  as expected
CharacterModel.3DS                       -> (.3DS            )  as expected
.desktop                                 -> (.desktop        )  as expected
document                                 -> (                )  as expected
document.txt_backup                      -> (                )  as expected
/etc/pam.d/login                         -> (                )  as expected

```



## Arturo



```arturo
files #("http://example.com/download.tar.gz" "CharacterModel.3DS" ".desktop" "document" "document.txt_backup" "/etc/pam.d/login")

loop files {
	ext $(if $(not $(contains $(getExtension &) "_")) { $(getExtension &) } { ""+"" })
	print & + " => extension: " + ext
}
```


```txt
http://example.com/download.tar.gz => extension: .gz
CharacterModel.3DS => extension: .3DS
.desktop => extension:
document => extension:
document.txt_backup => extension:
/etc/pam.d/login => extension:
```



## AWK


The following code shows two methods.

The first one was provided by an earlier contributor and shows a little more awk syntax and builtins (albeit with a bug fixed: it was testing for underscores in the extension but not other characters such as hyphens). It can be adjusted to allow any character in the extension other than /, \, : or . by replacing <code>[^a-zA-Z0-9]</code> with <code>[\\/\\\\:\\.]</code>.


```AWK

BEGIN {
    arr[++i] = "picture.jpg"
    arr[++i] = "http://mywebsite.com/picture/image.png"
    arr[++i] = "myuniquefile.longextension"
    arr[++i] = "IAmAFileWithoutExtension"
    arr[++i] = "/path/to.my/file"
    arr[++i] = "file.odd_one"

    for (j=1; j<=i; j++) {
      printf("%-40s '%s'\n",arr[j],extract_ext(arr[j]))
    }
    exit(0)
}
function extract_ext(fn,  sep1,sep2,tmp) {
    while (fn ~ (sep1 = ":|\\\\|\\/")) { # ":" or "\" or "/"
      fn = substr(fn,match(fn,sep1)+1)
    }
    while (fn ~ (sep2 = "\\.")) { # "."
      fn = substr(fn,match(fn,sep2)+1)
      tmp = 1
    }
    if (fn ~ /[^a-zA-Z0-9]/ || tmp == 0) {
    return("")
    }
    return(fn)
}

```


The second method is shorter and dispenses with the need to search for and remove the path components first. It too can be modified to allow all valid extensions (not just those described in the specification), by replacing <code>\\.[A-Za-z0-9]+$</code> with <code>\\.[^\\/\\\\:\\.]+$</code>.


```AWK

BEGIN {
    arr[++i] = "picture.jpg"
    arr[++i] = "http://mywebsite.com/picture/image.png"
    arr[++i] = "myuniquefile.longextension"
    arr[++i] = "IAmAFileWithoutExtension"
    arr[++i] = "/path/to.my/file"
    arr[++i] = "file.odd_one"

    for (j=1; j<=i; j++) {
      printf("%-40s '%s'\n",arr[j],extract_ext(arr[j]))
    }
    exit(0)
}
function extract_ext(fn,  pos) {
	pos = match(fn, "\\.[^\\/\\\\:\\.]+$")
	if (pos == 0) {
		return ("")
	} else {
		return (substr(fn,pos+1))
	}
}

```

<p>Both examples give the output:</p>

```txt

picture.jpg                              'jpg'
http://mywebsite.com/picture/image.png   'png'
myuniquefile.longextension               'longextension'
IAmAFileWithoutExtension                 ''
/path/to.my/file                         ''
file.odd_one                             ''

```



## Batch File


```dos
@echo off

:loop
if "%~1"=="" exit /b
echo File Path: "%~1" ^| File Extension "%~x1"
shift
goto loop
```

```txt
File Path: "http://example.com/download.tar.gz" | File Extension ".gz"
File Path: "CharacterModel.3DS" | File Extension ".3DS"
File Path: ".desktop" | File Extension ".desktop"
File Path: "document" | File Extension ""
File Path: "document.txt_backup" | File Extension ".txt_backup"
File Path: "/etc/pam.d/login" | File Extension ""
```



## C



```C>#include <assert.h

#include <ctype.h>
#include <string.h>
#include <stdio.h>

/* Returns a pointer to the extension of 'string'.
 * If no extension is found, returns a pointer to the end of 'string'. */
char* file_ext(const char *string)
{
    assert(string != NULL);
    char *ext = strrchr(string, '.');

    if (ext == NULL)
        return (char*) string + strlen(string);

    for (char *iter = ext + 1; *iter != '\0'; iter++) {
        if (!isalnum((unsigned char)*iter))
            return (char*) string + strlen(string);
    }

    return ext;
}

int main(void)
{
    const char *testcases[][2] = {
        {"http://example.com/download.tar.gz", ".gz"},
        {"CharacterModel.3DS", ".3DS"},
        {".desktop", ".desktop"},
        {"document", ""},
        {"document.txt_backup", ""},
        {"/etc/pam.d/login", ""}
    };

    int exitcode = 0;
    for (size_t i = 0; i < sizeof(testcases) / sizeof(testcases[0]); i++) {
        const char *ext = file_ext(testcases[i][0]);
        if (strcmp(ext, testcases[i][1]) != 0) {
            fprintf(stderr, "expected '%s' for '%s', got '%s'\n",
                testcases[i][1], testcases[i][0], ext);
            exitcode = 1;
        }
    }
    return exitcode;
}
```



## C++



```cpp
#include <string>
#include <algorithm>
#include <iostream>
#include <vector>
#include <regex>

std::string findExtension ( const std::string & filename ) {
   auto position = filename.find_last_of ( '.' ) ;
   if ( position == std::string::npos )
      return "" ;
   else {
      std::string extension ( filename.substr( position + 1 ) ) ;
      if (std::regex_search (extension, std::regex("[^A-Za-z0-9]") ))
         return "" ;
      else
         return extension ;
   }
}

int main( ) {
   std::vector<std::string> filenames {"picture.jpg" , "http://mywebsite.com/picture/image.png" ,
      "myuniquefile.longextension" , "IAmAFileWithoutExtension" , "/path/to.my/file" ,
      "file.odd_one", "thisismine." } ;
   std::vector<std::string> extensions( filenames.size( ) ) ;
   std::transform( filenames.begin( ) , filenames.end( ) , extensions.begin( ) , findExtension ) ;
   for ( int i = 0 ; i < filenames.size( ) ; i++ )
      std::cout << filenames[i] << " has extension : " << extensions[i] << " !\n" ;
   return 0 ;
}

```

```txt
picture.jpg has extension : jpg !
http://mywebsite.com/picture/image.png has extension : png !
myuniquefile.longextension has extension : longextension !
IAmAFileWithoutExtension has extension :  !
/path/to.my/file has extension :  !
file.odd_one has extension :  !
thisismine. has extension :  !

```


## C#

<lang [[C sharp|C#]]>public static string FindExtension(string filename) {
    int indexOfDot = filename.Length;
    for (int i = filename.Length - 1; i >= 0; i--) {
        char c = filename[i];
        if (c == '.') {
            indexOfDot = i;
            break;
        }
        if (c >= '0' && c <= '9') continue;
        if (c >= 'A' && c <= 'Z') continue;
        if (c >= 'a' && c <= 'z') continue;
        break;
    }
    //The dot must be followed by at least one other character,
    //so if the last character is a dot, return the empty string
    return indexOfDot + 1 == filename.Length ? "" : filename.Substring(indexOfDot);
}
```


'''Using regular expressions (C# 6)'''
<lang [[C sharp|C#]]>public static string FindExtension(string filename) => Regex.Match(filename, @"\.[A-Za-z0-9]+$").Value;
```



## Emacs Lisp



```Lisp
(file-name-extension "foo.txt")
=>
"txt"
```


No extension is distinguished from empty extension but an <code>(or ... "")</code> can give <code>""</code> for both if desired


```Lisp
(file-name-extension "foo.") => ""
(file-name-extension "foo")  => nil
```


An Emacs backup <code>~</code> or <code>.~NUM~</code> are not part of the extension, but otherwise any characters are allowed.


```Lisp
(file-name-extension "foo.txt~")        => "txt"
(file-name-extension "foo.txt.~1.234~") => "txt"
```



## D


###  Variant 1


```D

import std.stdio;
import std.path;

void main()
{
  auto filenames = ["http://example.com/download.tar.gz",
                    "CharacterModel.3DS",
                    ".desktop",
                    "document",
                    "document.txt_backup",
                    "/etc/pam.d/login"]

  foreach(filename; filenames)
    writeln(filename, " -> ", filename.extension);

}

```


```txt

http://example.com/download.tar.gz -> .gz
CharacterModel.3DS -> .3DS
.desktop ->
document ->
document.txt_backup -> .txt_backup
/etc/pam.d/login ->

```



###  Variant 2


```D

import std.stdio;
import std.string;
import std.range;
import std.algorithm;

void main()
{
  auto filenames = ["http://example.com/download.tar.gz",
                    "CharacterModel.3DS",
                    ".desktop",
                    "document",
                    "document.txt_backup",
                    "/etc/pam.d/login"]

  foreach(filename; filenames)
  {
    string ext;
    auto idx = filename.lastIndexOf(".");
    if(idx >= 0)
    {
      auto tmp = filename.drop(idx);
      if(!tmp.canFind("/", "\\", "_", "*");
        ext = tmp;
    }
    writeln(filename, " -> ", ext);
  }

}


```

```txt

http://example.com/download.tar.gz -> .gz
CharacterModel.3DS -> .3DS
.desktop -> .desktop
document ->
document.txt_backup ->
/etc/pam.d/login ->

```



## Factor

Factor's <tt>file-extension</tt> word allows symbols to be in the extension and omits the dot from its output.

```factor
USING: assocs formatting kernel io io.pathnames math qw
sequences ;
IN: rosetta-code.file-extension

qw{
  http://example.com/download.tar.gz
  CharacterModel.3DS
  .desktop
  document
  document.txt_backup
  /etc/pam.d/login
}

dup [ file-extension ] map zip
"Path" "| Extension" "%-35s%s\n" printf
47 [ "-" write ] times nl
[ "%-35s| %s\n" vprintf ] each
```

```txt

Path                               | Extension
-----------------------------------------------
http://example.com/download.tar.gz | gz
CharacterModel.3DS                 | 3DS
.desktop                           | desktop
document                           |
document.txt_backup                | txt_backup
/etc/pam.d/login                   |

```



## Forth



```forth
: invalid? ( c -- f )
   toupper dup [char] A [char] Z 1+ within
   swap [char] 0 [char] 9 1+ within or 0= ;
: extension ( addr1 u1 -- addr2 u2 )
   dup 0= if exit then
   2dup over +
   begin 1- 2dup <= while dup c@ invalid? until then
   \ no '.' found
   2dup - 0> if 2drop dup /string exit then
   \ invalid char
   dup c@ [char] . <> if 2drop dup /string exit then
   swap -
   \ '.' is last char
   2dup 1+ = if drop dup then
   /string ;

: type.quoted ( addr u -- )
   [char] ' emit type [char] ' emit ;
: test ( addr u -- )
   2dup type.quoted ."  => " extension type.quoted cr ;
: tests
   s" http://example.com/download.tar.gz" test
   s" CharacterModel.3DS" test
   s" .desktop" test
   s" document" test
   s" document.txt_backup" test
   s" /etc/pam.d/login" test ;
```

```txt
cr tests
'http://example.com/download.tar.gz' => '.gz'
'CharacterModel.3DS' => '.3DS'
'.desktop' => '.desktop'
'document' => ''
'document.txt_backup' => ''
'/etc/pam.d/login' => ''
 ok
```



## Fortran

The plan is to scan backwards from the end of the text until a non-extensionish character is encountered. If it is a period, then a valid file extension has been spanned. Otherwise, no extension. Yet again the "no specification" on the possibility of shortcut evaluation of compound logical expressions prevents the structured use of a DO WHILE(L1 > 0 & TEXT(L1:L1)''etc'') loop because the possible evaluation of both parts of the expression means that the second part may attempt to access character zero of a text. So, the compound expression has to be broken into two separate parts.

The source incorporates a collection of character characterisations via suitable spans of a single sequence of characters. Unfortunately, the PARAMETER statement does not allow its constants to appear in EQUIVALENCE statements, so the text is initialised by DATA statements, and thus loses the protection of read-only given to constants defined via PARAMETER statements. The statements are from a rather more complex text scanning scheme, as all that are needed here are the symbols of GOODEXT.

The text scan could instead check for a valid character via something like <code> ("a" <= C & C <= "z") | ("A" <= C & C <= "Z") | (0 <= C & C <= "9")</code> but this is not just messy but unreliable - in EBCDIC for example there are gaps in the sequence of letters that are occupied by other symbols. So instead, a test via INDEX into a sequence of all the valid symbols. If one was in a hurry, for eight-bit character codes, an array GOODEXT of 256 logical values could be indexed by the numerical value of the character.
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
       EQUIVALENCE (CHARACTER(8),GOODEXT)
c       PARAMETER (GOODEXT = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"	!for an approved
c     1                    //"abcdefghijklmnopqrstuvwxyz"	!file "extension" part
c     2                    //"0123456789")			!Of a file name.
       INTEGER MEXT		!A fixed bound.
       PARAMETER (MEXT = 28)	!This should do.
       CONTAINS
        CHARACTER*(MEXT) FUNCTION FEXT(FNAME)	!Return the file extension part.
         CHARACTER*(*) FNAME	!May start with the file's path name blather.
         INTEGER L1,L2		!Fingers to the text.
          L2 = LEN(FNAME)	!The last character of the file name.
          L1 = L2		!Starting at the end...
   10     IF (L1.GT.0) THEN	!Damnit, can't rely on DO WHILE(safe & test)
            IF (INDEX(GOODEXT,FNAME(L1:L1)).GT.0) THEN	!So do the two parts explicitly.
              L1 = L1 - 1		!Well, that was a valid character for an extension.
              GO TO 10			!So, move back one and try again.
            END IF		!Until the end of valid stuff.
            IF (FNAME(L1:L1).EQ.".") THEN	!Stopped here. A proper introduction?
              L1 = L1 - 1			!Yes. Include the period.
              GO TO 20				!And escape.
            END IF		!Otherwise, not valid stuff.
          END IF		!Keep on moving back.
          L1 = L2		!If we're here, no period was found.
   20     FEXT = FNAME(L1 + 1:L2)	!The text of the extension.
        END FUNCTION FEXT	!Possibly, blank.
      END MODULE TEXTGNASH	!Enough for this.

      PROGRAM POKE
      USE TEXTGNASH

      WRITE (6,*) FEXT("Picture.jpg")
      WRITE (6,*) FEXT("http://mywebsite.com/picture/image.png")
      WRITE (6,*) FEXT("myuniquefile.longextension")
      WRITE (6,*) FEXT("IAmAFileWithoutExtension")
      WRITE (6,*) FEXT("/path/to.my/file")
      WRITE (6,*) FEXT("file.odd_one")
      WRITE (6,*)
      WRITE (6,*) "Now for the new test collection..."
      WRITE (6,*) FEXT("http://example.com/download.tar.gz")
      WRITE (6,*) FEXT("CharacterModel.3DS")
      WRITE (6,*) FEXT(".desktop")
      WRITE (6,*) FEXT("document")
      WRITE (6,*) FEXT("document.txt_backup")
      WRITE (6,*) FEXT("/etc/pam.d/login")
      WRITE (6,*) "Approved characters: ",GOODEXT
      END
```

The output cheats a little, in that trailing spaces appear just as blankly as no spaces. The result of FEXT could be presented to TRIM (if that function is available), or the last non-blank could be found. With F2003, a scheme to enable character variables to be redefined to take on a current length is available, and so trailing spaces could no longer appear. This facility would also solve the endlessly annoying question of "how long is long enough", manifested in parameter MEXT being what might be a perfect solution. Once, three was the maximum extension length (not counting the period), then perhaps six, but now, what?

```txt

 .jpg
 .png
 .longextension




 Now for the new test collection...
 .gz
 .3DS
 .desktop



 Approved characters:
 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz

```

Note that if FEXT were presented with a file name containing trailing spaces, it would declare no extension to be present.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Function isAlphaNum(s As String) As Boolean
  Return ("a" <= s AndAlso s <= "z") OrElse ("A" <= s AndAlso s <= "Z") OrElse("0" <= s AndAlso s <= "9")
End Function

Function extractFileExt(filePath As String) As String
  If filePath = "" Then Return ""
  Dim index As Integer = InstrRev(filePath, ".")
  If index = 0 Then Return ""
  Dim ext As String  = Mid(filePath, index + 1)
  If ext = "" Then Return ""
  For i As Integer = 1 To Len(ext)
    If Not isAlphaNum(Mid(ext, i, 1)) Then Return ""
  Next
  Return ext
End Function

Dim filePaths(1 To 6) As String = _
{ _
  "http://example.com/download.tar.gz", _
  "CharacterModel.3DS", _
  ".desktop", _
  "document", _
  "document.txt_backup", _
  "/etc/pam.d/login" _
}

Print "File path"; Tab(40); "Extension"
Print "
### ===
"; Tab(40); "
### ===
"
Print
For i As Integer = 1 To 6
  Print filePaths(i); Tab(40);
  Dim ext As String = extractFileExt(filePaths(i))
  If ext = "" Then
    Print "(empty string)"
  Else
    Print ext
  End If
Next
Print
Print "Press any key to quit"
Sleep
```


```txt

File path                              Extension

### ======                              ======


http://example.com/download.tar.gz     gz
CharacterModel.3DS                     3DS
.desktop                               desktop
document                               (empty string)
document.txt_backup                    (empty string)
/etc/pam.d/login                       (empty string)

```



## Gambas

As Gambas has its own tools for file extension extraction I have used those rather than complicate the code to match the requested criteria.

'''[https://gambas-playground.proko.eu/?gist=d52464fe8c05c857311d49184299814a Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sDir As String = "/sbin"
Dim sFileList As String[] = Dir(sDir)
Dim sTemp As String
Dim sFile As String

For Each sTemp In sFileList
  sFile = sDir &/ sTemp
  Print File.Name(sFile) & Space(25 - Len(File.Name(sFile)));
  Print File.Ext(sFile)
Next

End
```

Output:

```txt

....
mount.ntfs               ntfs
iptables-save
mkfs.minix               minix
exfatlabel
modprobe
vgrename
mkfs.ext2                ext2
lsmod
umount.ecryptfs_private  ecryptfs_private
fstab-decode
mount.ecryptfs           ecryptfs
....

```



## Go



```go
package main

import "fmt"

func Ext(path string) string {
	for i := len(path) - 1; i >= 0; i-- {
		c := path[i]
		switch {
		case c == '.':
			return path[i:]
		case '0' <= c && c <= '9':
		case 'A' <= c && c <= 'Z':
		case 'a' <= c && c <= 'z':
		default:
			return ""
		}
	}
	return ""
}

func main() {
	type testcase struct {
		input  string
		output string
	}

	tests := []testcase{
		{"http://example.com/download.tar.gz", ".gz"},
		{"CharacterModel.3DS", ".3DS"},
		{".desktop", ".desktop"},
		{"document", ""},
		{"document.txt_backup", ""},
		{"/etc/pam.d/login", ""},
	}

	for _, testcase := range tests {
		ext := Ext(testcase.input)
		if ext != testcase.output {
			panic(fmt.Sprintf("expected %q for %q, got %q",
				testcase.output, testcase.input, ext))
		}
	}
}
```



## Haskell



```Haskell
module FileExtension
   where

myextension :: String -> String
myextension s
   |not $ elem '.' s = ""
   |elem '/' extension || elem '_' extension = ""
   |otherwise = '.' : extension
      where
	 extension = reverse ( takeWhile ( /= '.' ) $ reverse s )

```

```txt
map myextension ["http://example.com/download.tar.gz", "CharacterModel.3DS", ".desktop", "document", "document.txt_backup", "/etc/pam.d/login"]
[".gz",".3DS",".desktop","","",""]

```



### Posix compliant

On Unix systems, the penultimate file extension would be recognised, so using the Haskell library function '''takeExtension''':


```haskell
import System.FilePath.Posix (FilePath, takeExtension)

fps :: [FilePath]
fps =
  [ "http://example.com/download.tar.gz"
  , "CharacterModel.3DS"
  , ".desktop"
  , "document"
  , "document.txt_backup"
  , "/etc/pam.d/login"
  ]

main :: IO ()
main = mapM_ print $ takeExtension <$> fps
```

```txt
".gz"
".3DS"
".desktop"
""
".txt_backup"
""
```



## J


'''Implementation:'''


```J
require'regex'
ext=: '[.][a-zA-Z0-9]+$'&rxmatch ;@rxfrom ]
```


Obviously most of the work here is done by the regex implementation ([[wp:Perl Compatible Regular Expressions|pcre]], if that matters - and this particular kind of expression tends to be a bit more concise expressed in [[Perl|perl]] than in J...).

Perhaps of interest is that this is an example of a J fork - here we have three verbs separated by spaces. Unlike a unix system fork (which spins up child process which is an almost exact clone of the currently running process), a J fork is three independently defined verbs. The two verbs on the edge get the fork's argument and the verb in the middle combines those two results.

The left verb uses rxmatch to find the beginning position of the match and its length. The right verb is the identity function. The middle verb extracts the desired characters from the original argument. (For a non-match, the length of the "match" is zero so the empty string is extracted.)


'''Alternative non-regex Implementation'''

```J
ext=: (}.~ i:&'.')@(#~ [: -. [: +./\. -.@e.&('.',AlphaNum_j_)
```



'''Task examples:'''

```J
   ext 'http://example.com/download/tar.gz'
.gz
   ext 'CharacterModel.3DS'
.3DS

   Examples=: 'http://example.com/download.tar.gz';'CharacterModel.3DS';'.desktop';'document';'document.txt_backup';'/etc/pam.d/login'
   ext each Examples
┌───┬────┬────────┬┬┬┐
│.gz│.3DS│.desktop││││
└───┴────┴────────┴┴┴┘
```



## Java


```java
public class Test {

    public static void main(String[] args) {
        String[] filenames = { "http://example.com/download.tar.gz",
                               "CharacterModel.3DS",
                               ".desktop",
                               "document",
                               "document.txt_backup",
                               "/etc/pam.d/login"
        };

        for (String filename : filenames) {
            String ext = "null";
            int idx = filename.lastIndexOf('.');
            if (idx != -1) {
                String tmp = filename.substring(idx);
                if (tmp.matches("\\.[a-zA-Z0-9]+")) {
                    ext = tmp;
                }
            }
            System.out.println(filename + " -> " + ext);
        }
    }
}
```

```txt
http://example.com/download.tar.gz -> .gz
CharacterModel.3DS -> .3DS
.desktop -> .desktop
document -> null
document.txt_backup -> null
/etc/pam.d/login -> null
```



## javascript


```javascript
let filenames = ["http://example.com/download.tar.gz", "CharacterModel.3DS", ".desktop", "document", "document.txt_backup", "/etc/pam.d/login"];
let r = /\.[a-zA-Z0-9]+$/;
filenames.forEach((e) => console.log(e + " -> " + (r.test(e) ? r.exec(e)[0] : "")));
```

```txt
http://example.com/download.tar.gz -> .gz
CharacterModel.3DS -> .3DS
.desktop -> .desktop
document ->
document.txt_backup ->
/etc/pam.d/login ->
```



With JS embedded in browsers and other applications across most or all operating systems, we need some flexibility in any reusable '''takeExtension''' function.

One approach is to define a more general curried function, from which we can obtain various simpler and OS-specific functions by specialisation:


```javascript
(() => {
    'use strict';

    // OS-INDEPENDENT CURRIED FUNCTION --------------------

    // takeExtension :: Regex String -> FilePath -> String
    const takeExtension = charSet => fp => {
        const
            rgx = new RegExp('^[' + charSet + ']+$'),
            xs = fp.split('/').slice(-1)[0].split('.'),
            ext = 1 < xs.length ? (
                xs.slice(-1)[0]
            ) : '';
        return rgx.test(ext) ? (
            '.' + ext
        ) : '';
    };

    // OS-SPECIFIC SPECIALIZED FUNCTIONS ------------------

    // takePosixExtension :: FilePath -> String
    const takePosixExtension = takeExtension('A-Za-z0-9\_\-');

    // takeWindowsExtension :: FilePath -> String
    const takeWindowsExtension = takeExtension('A-Za-z0-9');


    // TEST -------------------------------------------
    // main :: IO()
    const main = () => {
        [
            ['Posix', takePosixExtension],
            ['Windows', takeWindowsExtension]
        ].forEach(
            ([osName, f]) => console.log(
                tabulated(
                    '\n\ntake' + osName +
                    'Extension :: FilePath -> String:\n',
                    x => x.toString(),
                    x => "'" + x.toString() + "'",
                    f,
                    [
                        "http://example.com/download.tar.gz",
                        "CharacterModel.3DS",
                        ".desktop",
                        "document",
                        "document.txt_backup",
                        "/etc/pam.d/login"
                    ]
                ),
                '\n'
            )
        )

    };

    // GENERIC FUNCTIONS FOR TESTING AND DISPLAY OF RESULTS

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
            i
        };

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (f, g) => x => f(g(x));

    // justifyRight :: Int -> Char -> String -> String
    const justifyRight = (n, cFiller, s) =>
        n > s.length ? (
            s.padStart(n, cFiller)
        ) : s;

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // Map over lists or strings

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);


    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    const maximumBy = (f, xs) =>
        0 < xs.length ? (
            xs.slice(1)
            .reduce((a, x) => 0 < f(x, a) ? x : a, xs[0])
        ) : undefined;

    // tabulated :: String -> (a -> String) ->
    //                        (b -> String) ->
    //           (a -> b) -> [a] -> String
    const tabulated = (s, xShow, fxShow, f, xs) => {
        // Heading -> x display function ->
        //           fx display function ->
        //    f -> values -> tabular string
        const
            ys = map(xShow, xs),
            w = maximumBy(comparing(x => x.length), ys).length,
            rows = zipWith(
                (a, b) => justifyRight(w, ' ', a) + ' -> ' + b,
                ys,
                map(compose(fxShow, f), xs)
            );
        return s + '\n' + unlines(rows);
    };

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));


    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // Use of `take` and `length` here allows zipping with non-finite lists
    // i.e. generators like cycle, repeat, iterate.

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const
            lng = Math.min(length(xs), length(ys)),
            as = take(lng, xs),
            bs = take(lng, ys);
        return Array.from({
            length: lng
        }, (_, i) => f(as[i], bs[i], i));
    };

    // MAIN ---
    return main();
})();
```

```txt

takePosixExtension :: FilePath -> String:

http://example.com/download.tar.gz -> '.gz'
                CharacterModel.3DS -> '.3DS'
                          .desktop -> '.desktop'
                          document -> ''
               document.txt_backup -> '.txt_backup'
                  /etc/pam.d/login -> ''


takeWindowsExtension :: FilePath -> String:

http://example.com/download.tar.gz -> '.gz'
                CharacterModel.3DS -> '.3DS'
                          .desktop -> '.desktop'
                          document -> ''
               document.txt_backup -> ''
                  /etc/pam.d/login -> ''
```



## jq


The following definitions include the delimiting period.

In the first section, a version intended for jq version 1.4 is presented.
A simpler definition using "match", a regex feature of subsequent versions of jq, is then given.

```jq
def file_extension:
  def alphanumeric: explode | unique
  | reduce .[] as $i
      (true;
       if . then $i | (97 <= . and . <= 122) or (65 <= . and . <= 90) or (48 <= . and . <= 57)
       else false
       end );
  rindex(".") as $ix
  | if $ix then .[1+$ix:] as $ext
    | if $ext|alphanumeric then ".\($ext)" # include the period
      else ""
      end
    else ""
    end;
```


```jq
def file_extension:
  (match( "(\\.[a-zA-Z0-9]*$)" ) | .captures[0].string)
  // "" ;
```


'''Examples''':

Using either version above gives the same results.

```jq
"http://example.com/download.tar.gz",
"CharacterModel.3DS",
".desktop",
"document",
"document.txt_backup",
"/etc/pam.d/login"
| "\(.) has extension: \(file_extension)"
```



```sh
$ jq -r -n -f Extract_file_extension.jq
```

```txt
http://example.com/download.tar.gz has extension: .gz
CharacterModel.3DS has extension: .3DS
.desktop has extension: .desktop
document has extension:
document.txt_backup has extension:
/etc/pam.d/login has extension:

```



## Jsish


```javascript
#!/usr/bin/env jsish
/* Extract filename extension (for a limited subset of possible extensions) in Jsish */
function extractExtension(filename) {
    var extPat = /\.[a-z0-9]+$/i;
    var ext = filename.match(extPat);
    return ext ? ext[0] : '';
}

if (Interp.conf('unitTest')) {
    var files = ["http://example.com/download.tar.gz", "CharacterModel.3DS",
                 ".desktop", "document", "document.txt_backup", "/etc/pam.d/login"];
    for (var fn of files) puts(fn, quote(extractExtension(fn)));
}

/*
=!EXPECTSTART!=
http://example.com/download.tar.gz ".gz"
CharacterModel.3DS ".3DS"
.desktop ".desktop"
document ""
document.txt_backup ""
/etc/pam.d/login ""
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish --U extractExtension.jsi
http://example.com/download.tar.gz ".gz"
CharacterModel.3DS ".3DS"
.desktop ".desktop"
document ""
document.txt_backup ""
/etc/pam.d/login ""

prompt$ jsish -u extractExtension.jsi
[PASS] extractExtension.jsi
```



## Julia


```julia
extension(url::String) = try match(r"\.[A-Za-z0-9]+$", url).match catch "" end

@show extension("http://example.com/download.tar.gz")
@show extension("CharacterModel.3DS")
@show extension(".desktop")
@show extension("document")
@show extension("document.txt_backup")
@show extension("/etc/pam.d/login")
```


```txt
extension("http://example.com/download.tar.gz") = ".gz"
extension("CharacterModel.3DS") = ".3DS"
extension(".desktop") = ".desktop"
extension("document") = ""
extension("document.txt_backup") = ""
extension("/etc/pam.d/login") = ""
```



## Kotlin


```scala
// version 1.0.6

val r = Regex("[^a-zA-Z0-9]") // matches any non-alphanumeric character

fun extractFileExtension(path: String): String {
    if (path.isEmpty()) return ""
    var fileName = path.substringAfterLast('/')
    if (path == fileName) fileName = path.substringAfterLast('\\')
    val splits = fileName.split('.')
    if (splits.size == 1) return ""
    val ext = splits.last()
    return if (r.containsMatchIn(ext)) "" else "." + ext
}

fun main(args: Array<String>) {
    val paths = arrayOf(
        "http://example.com/download.tar.gz",
        "CharacterModel.3DS",
        ".desktop",
        "document",
        "document.txt_backup",
        "/etc/pam.d/login",
        "c:\\programs\\myprogs\\myprog.exe",          // using back-slash as delimiter
        "c:\\programs\\myprogs\\myprog.exe_backup"    // ditto
    )
    for (path in paths) {
        val ext =  extractFileExtension(path)
        println("${path.padEnd(37)} -> ${if (ext.isEmpty()) "(empty string)" else ext}")
    }
}
```


```txt

http://example.com/download.tar.gz    -> .gz
CharacterModel.3DS                    -> .3DS
.desktop                              -> .desktop
document                              -> (empty string)
document.txt_backup                   -> (empty string)
/etc/pam.d/login                      -> (empty string)
c:\programs\myprogs\myprog.exe        -> .exe
c:\programs\myprogs\myprog.exe_backup -> (empty string)

```



## Lua


```Lua
-- Lua pattern docs at http://www.lua.org/manual/5.1/manual.html#5.4.1
function fileExt (filename) return filename:match("(%.%w+)$") or "" end

local testCases = {
    "http://example.com/download.tar.gz",
    "CharacterModel.3DS",
    ".desktop",
    "document",
    "document.txt_backup",
    "/etc/pam.d/login"
}
for _, example in pairs(testCases) do
    print(example .. ' -> "' .. fileExt(example) .. '"')
end
```

```txt
http://example.com/download.tar.gz -> ".gz"
CharacterModel.3DS -> ".3DS"
.desktop -> ".desktop"
document -> ""
document.txt_backup -> ""
/etc/pam.d/login -> ""
```




## Objeck


```objeck
use Query.RegEx;

class FindExtension {
  function : Main(args : String[]) ~ Nil {
    file_names := [
      "http://example.com/download.tar.gz", "CharacterModel.3DS",
      ".desktop", "document",  "document.txt_backup", "/etc/pam.d/login"];

    each(i : file_names) {
      file_name := file_names[i];
      System.IO.Console->Print(file_name)->Print(" has extension: ")->PrintLine(GetExtension(file_name));
    };
  }

  function : GetExtension(file_name : String) ~ String {
    index := file_name->FindLast('.');
    if(index < 0) {
      return "";
    };

    ext := file_name->SubString(index, file_name->Size() - index);
    if(ext->Size() < 1) {
      return "";
    };

    if(<>RegEx->New("\\.([a-z]|[A-Z]|[0-9])+")->MatchExact(ext)) {
      return "";
    };

    return ext;
  }
}
```


```txt

http://example.com/download.tar.gz has extension: .gz
CharacterModel.3DS has extension: .3DS
.desktop has extension: .desktop
document has extension:
document.txt_backup has extension:
/etc/pam.d/login has extension:

```



## OCaml

Since OCaml 4.04 there is a function '''[http://caml.inria.fr/pub/docs/manual-ocaml/libref/Filename.html#VALextension Filename.extension]''':

```ocaml
let () =
  let filenames = [
    "http://example.com/download.tar.gz";
    "CharacterModel.3DS";
    ".desktop";
    "document";
    "document.txt_backup";
    "/etc/pam.d/login"]
  in
  List.iter (fun filename ->
    Printf.printf " '%s' => '%s'\n" filename (Filename.extension filename)
  ) filenames
```

differs a little bit from the specification of this task.
```txt

 'http://example.com/download.tar.gz' => '.gz'
 'CharacterModel.3DS' => '.3DS'
 '.desktop' => ''
 'document' => ''
 'document.txt_backup' => '.txt_backup'
 '/etc/pam.d/login' => ''

```



## Oforth


If extension is not valid, returns null, not "".
Easy to change if "" is required.


```Oforth
: fileExt( s -- t )
| i |
   s lastIndexOf('.') dup ->i ifNull: [ null return ]
   s extract(i 1+, s size) conform(#isAlpha) ifFalse: [ null return ]
   s extract(i, s size)
;
```


```txt

>"http://example.com/download.tar.gz" fileExt .
.gz ok
>
ok
>"CharacterModel.3DS" fileExt .
.3DS ok
>
ok
>".desktop" fileExt .
.desktop ok
>"document" fileExt .
null ok
>"document.txt_backup" fileExt .
null ok
>"/etc/pam.d/login" fileExt .
null ok
>
```



## Perl


```perl
sub extension {
    my $path = shift;
    $path =~ / \. [a-z0-9]+ $ /xi;
    $& // '';
}
```


Testing:

```perl
printf "%-35s %-11s\n", $_, "'".extension($_)."'"
for qw[
    http://example.com/download.tar.gz
    CharacterModel.3DS
    .desktop
    document
    document.txt_backup
    /etc/pam.d/login
];
```


```txt

http://example.com/download.tar.gz  '.gz'
CharacterModel.3DS                  '.3DS'
.desktop                            '.desktop'
document                            ''
document.txt_backup                 ''
/etc/pam.d/login                    ''

```



## Perl 6


The built-in <code>IO::Path</code> class has an <code>.extension</code> method:


```perl6
say $path.IO.extension;
```

Contrary to this task's specification, it
* doesn't include the dot in the output
* doesn't restrict the extension to letters and numbers.


Here's a custom implementation which does satisfy the task requirements:


```perl6
sub extension (Str $path --> Str) {
    $path.match(/:i ['.' <[a..z0..9]>+]? $ /).Str
}

# Testing:

printf "%-35s %-11s %-12s\n", $_, extension($_).perl, $_.IO.extension.perl
for <
    http://example.com/download.tar.gz
    CharacterModel.3DS
    .desktop
    document
    document.txt_backup
    /etc/pam.d/login
>;
```


```txt

http://example.com/download.tar.gz  ".gz"       "gz"
CharacterModel.3DS                  ".3DS"      "3DS"
.desktop                            ".desktop"  "desktop"
document                            ""          ""
document.txt_backup                 ""          "txt_backup"
/etc/pam.d/login                    ""          ""

```



## Phix


```Phix
function getExtension(string filename)
    for i=length(filename) to 1 by -1 do
        integer ch = filename[i]
        if ch='.' then return filename[i..$] end if
        if find(ch,"\\/_") then exit end if
    end for
    return ""
end function

constant tests = {"mywebsite.com/picture/image.png",
                  "http://mywebsite.com/picture/image.png",
                  "myuniquefile.longextension",
                  "IAmAFileWithoutExtension",
                  "/path/to.my/file",
                  "file.odd_one",
                  "http://example.com/download.tar.gz",
                  "CharacterModel.3DS",
                  ".desktop",
                  "document",
                  "document.txt_backup",
                  "/etc/pam.d/login"}
for i=1 to length(tests) do
    printf(1,"%s ==> %s\n",{tests[i],getExtension(tests[i])})
end for
```

```txt

mywebsite.com/picture/image.png ==> .png
http://mywebsite.com/picture/image.png ==> .png
myuniquefile.longextension ==> .longextension
IAmAFileWithoutExtension ==>
/path/to.my/file ==>
file.odd_one ==>
http://example.com/download.tar.gz ==> .gz
CharacterModel.3DS ==> .3DS
.desktop ==> .desktop
document ==>
document.txt_backup ==>
/etc/pam.d/login ==>

```

The builtin get_file_extension() could also be used, however that routine differs from the task description in that "libglfw.so.3.1" => "so", and all results are lowercase even if the input is not.


## PicoLisp


```PicoLisp
(de extension (F)
   (and
      (fully
         '((C)
            (or
               (>= "Z" C "A")
               (>= "z" C "a")
               (>= "9" C "0") ) )
      (setq F (stem (member "." (chop F)) ".")) )
      (pack F) ) )
(println (extension "http://example.com/download.tar.gz"))
(println (extension "CharacterModel.3DS"))
(println (extension ".desktop"))
(println (extension "document"))
(println (extension "document.txt_backup"))
(println (extension "/etc/pam.d/login"))
```

```txt

"gz"
"3DS"
"desktop"
NIL
NIL
NIL

```



## PowerShell


```PowerShell
function extension($file){
    $ext = [System.IO.Path]::GetExtension($file)
    if (-not [String]::IsNullOrEmpty($ext)) {
        if($ext.IndexOf("_") -ne -1) {$ext = ""}
    }
    $ext
}
extension "http://example.com/download.tar.gz"
extension "CharacterModel.3DS"
extension ".desktop"
extension "document"
extension "document.txt_backup"
extension "/etc/pam.d/login"
```

<b>Output:</b>

```txt
.gz
.3DS
.desktop



```



## Python


Uses [https://docs.python.org/3/library/re.html#re.search re.search].


```python
import re
def extractExt(url):
  m = re.search(r'\.[A-Za-z0-9]+$', url)
  return m.group(0) if m else ""

```


and one way of allowing for OS-specific variations in the character sets permitted in file extensions is to write a general and reusable curried function, from which we can obtain simpler OS-specific functions by specialisation:


```python
'''Obtaining OS-specific file extensions'''

import os
import re


# OS-INDEPENDENT CURRIED FUNCTION -------------------------

# takeExtension :: Regex String -> FilePath -> String
def takeExtension(charSet):
    '''The extension part (if any) of a file name.
       (Given a regex string representation of the
       character set accepted in extensions by the OS).'''
    def go(fp):
        m = re.search(
            r'\.[' + charSet + ']+$',
            (fp).split(os.sep)[-1]
        )
        return m[0] if m else ''
    return lambda fp: go(fp)


# DERIVED (OS-SPECIFIC) FUNCTIONS -------------------------


# takePosixExtension :: FilePath -> String
def takePosixExtension(fp):
    '''The file extension, if any,
       of a Posix file path.'''
    return takeExtension(r'A-Za-z0-9\-\_')(fp)


# takeWindowsExtension :: FilePath -> String
def takeWindowsExtension(fp):
    '''The file extension, if any,
       of a Windows file path.'''
    return takeExtension(r'A-Za-z0-9')(fp)


# TEST ----------------------------------------------------
def main():
    '''Tests'''

    for f in [takePosixExtension, takeWindowsExtension]:
        print(
            tabulated(f.__name__ + ' :: FilePath -> String:')(
                str
            )(str)(f)([
                "http://example.com/download.tar.gz",
                "CharacterModel.3DS",
                ".desktop",
                "document",
                "document.txt_backup",
                "/etc/pam.d/login"
            ])
        )
        print()


# GENERIC -------------------------------------------------


# tabulated :: String -> (a -> String) ->
#                        (b -> String) ->
#                        (a -> b) -> [a] -> String
def tabulated(s):
    '''Heading -> x display function -> fx display function ->
          number of columns -> f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(lambda x: len(xShow(x)), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + ' -> ' + fxShow(f(x)) for x in xs
        ])
    return lambda xShow: lambda fxShow: (
        lambda f: lambda xs: go(
            xShow, fxShow, f, xs
        )
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
takePosixExtension :: FilePath -> String:
http://example.com/download.tar.gz -> .gz
                CharacterModel.3DS -> .3DS
                          .desktop -> .desktop
                          document ->
               document.txt_backup -> .txt_backup
                  /etc/pam.d/login ->

takeWindowsExtension :: FilePath -> String:
http://example.com/download.tar.gz -> .gz
                CharacterModel.3DS -> .3DS
                          .desktop -> .desktop
                          document ->
               document.txt_backup ->
                  /etc/pam.d/login ->
```



## Racket



```Racket

#lang racket

;; Note that for a real implementation, Racket has a
;; `filename-extension` in its standard library, but don't use it here
;; since it requires a proper name (fails on ""), returns a byte-string,
;; and handles path values so might run into problems with unicode
;; string inputs.

(define (string-extension x)
  (cadr (regexp-match #px"(\\.[[:alnum:]]+|)$" x)))

(define examples '("http://example.com/download.tar.gz"
                   "CharacterModel.3DS"
                   ".desktop"
                   "document"
                   "document.txt_backup"
                   "/etc/pam.d/login"))

(for ([x (in-list examples)])
  (printf "~a | ~a\n" (~a x #:width 34) (string-extension x)))

```


```txt

http://example.com/download.tar.gz | .gz
CharacterModel.3DS                 | .3DS
.desktop                           | .desktop
document                           |
document.txt_backup                |
/etc/pam.d/login                   |

```



## REXX


Using this paraphrased Rosetta Code task's definition that:

a legal file extension   ''only''   consists of mixed-case Latin letters and/or decimal digits.

```rexx
/*REXX pgm extracts the file extension (defined above from the RC task) from a file name*/
@.=                                              /*define default value for the @ array.*/
parse arg fID                                    /*obtain any optional arguments from CL*/
if fID\==''  then @.1 = fID                      /*use the  filename  from the C.L.     */
             else do                             /*No filename given? Then use defaults.*/
                  @.1 = 'http://example.com/download.tar.gz'
                  @.2 = 'CharacterModel.3DS'
                  @.3 = '.desktop'
                  @.4 = 'document'
                  @.5 = 'document.txt_backup'
                  @.6 = '/etc/pam.d/login'
                  end

   do j=1  while  @.j\=='';     x=               /*process  (all of)  the file name(s). */
   p=lastpos(., @.j)                             /*find the last position of a period.  */
   if p\==0  then x=substr(@.j, p+1)             /*Found a dot?  Then get stuff after it*/
   if \datatype(x, 'A')   then x=                /*Not upper/lowercase letters | digits?*/
   if x==''  then x= " [null]"                   /*use a better name for a  "null"  ext.*/
             else x= . || x                      /*prefix the extension with a  period. */
   say 'file extension='       left(x, 20)     "for file name="         @.j
   end       /*j*/                               /*stick a fork in it,  we're all done. */
```

'''output'''   when using the default (internal) inputs:

```txt

file extension= .gz                  for file name= http://example.com/download.tar.gz
file extension= .3DS                 for file name= CharacterModel.3DS
file extension= .desktop             for file name= .desktop
file extension=  [null]              for file name= document
file extension=  [null]              for file name= document.txt_backup
file extension=  [null]              for file name= /etc/pam.d/login

```



## Ring


```ring

# Project : Extract file extension

test = ["http://example.com/download.tar.gz",
           "CharacterModel.3DS",
           ".desktop",
           "document",
           "document.txt_backup",
           "/etc/pam.d/login"]

for n = 1 to len(test)
    flag = 1
    revtest = revstr(test[n])
    nr = substr(revtest, ".")
    if nr > 0
       revtest2 = left(revtest, nr)
       for m = 1 to len(revtest2)
           if (ascii(revtest2[m]) > 64 and ascii(revtest2[m]) < 91) or
              (ascii(revtest2[m]) > 96 and ascii(revtest2[m]) < 123) or
               isdigit(revtest2[m]) or revtest2[m] = "."
           else
               flag = 0
           ok
       next
    else
       flag = 0
    ok
    if flag = 1
       revtest3 = revstr(revtest2)
       see test[n] + " -> " + revtest3 + nl
    else
       see test[n] + " -> (none)" + nl
    ok
next

func revstr(cStr)
       cStr2 = ""
       for x = len(cStr) to 1 step -1
           cStr2 += cStr[x]
       next
       return cStr2

```

Output:

```txt

http://example.com/download.tar.gz -> .gz
CharacterModel.3DS -> .3DS
.desktop -> .desktop
document -> (none)
document.txt_backup -> (none)
/etc/pam.d/login -> (none)

```



## Ruby


```ruby
names =
%w(http://example.com/download.tar.gz
   CharacterModel.3DS
   .desktop
   document
   /etc/pam.d/login)
names.each{|name| p File.extname(name)}

```

'''output'''

```txt

".gz"
".3DS"
""
""
""

```

Apparently, the built-in method does not consider ".desktop" to be a file extension (on Linux).


## Rust


```Rust
use std::path::Path;

fn main() {
    let filenames = &[
        "http://example.com/download.tar.gz",
        "CharacterModel.3DS",
        ".desktop",
        "document",
        "document.txt_backup",
        "/etc/pam.d/login",
    ];

    for filename in filenames {
        println!(
            "{:34} | {:8} | {:?}",
            filename,
            extension(filename),
            Path::new(filename).extension()
        );
    }
}

fn extension(filename: &str) -> &str {
    filename
        .rfind('.')
        .map(|idx| &filename[idx..])
        .filter(|ext| ext.chars().skip(1).all(|c| c.is_ascii_alphanumeric()))
        .unwrap_or("")
}
```

The built-in method requires a filename before the extension, allows any non-period character to appear in the extension, and returns <code>None</code> if no extension is found.
```txt

http://example.com/download.tar.gz | .gz      | Some("gz")
CharacterModel.3DS                 | .3DS     | Some("3DS")
.desktop                           | .desktop | None
document                           |          | None
document.txt_backup                |          | Some("txt_backup")
/etc/pam.d/login                   |          | None

```



## Scala


```scala
package rosetta

object FileExt {

  private val ext = """\.[A-Za-z0-9]+$""".r

  def isExt(fileName: String, extensions: List[String]) =
    extensions.map { _.toLowerCase }.exists { fileName.toLowerCase endsWith "." + _ }

  def extractExt(url: String) = ext findFirstIn url getOrElse("")

}

object FileExtTest extends App {
    val testExtensions: List[String] = List("zip", "rar", "7z", "gz", "archive", "A##", "tar.bz2")

  val isExtTestFiles: Map[String, Boolean] = Map(
      "MyData.a##"          -> true,
      "MyData.tar.Gz"       -> true,
      "MyData.gzip"         -> false,
      "MyData.7z.backup"    -> false,
      "MyData..."           -> false,
      "MyData"              -> false,
      "MyData_v1.0.tar.bz2" -> true,
      "MyData_v1.0.bz2"     -> false
      )

  val extractExtTestFiles: Map[String, String] = Map(
      "http://example.com/download.tar.gz" -> ".gz",
      "CharacterModel.3DS"                 -> ".3DS",
      ".desktop"                           -> ".desktop",
      "document"                           -> "",
      "document.txt_backup"                -> "",
      "/etc/pam.d/login"                   -> "",
      "/etc/pam.d/login.a"                 -> ".a",
      "/etc/pam.d/login."                  -> "",
      "picture.jpg"                        -> ".jpg",
      "http://mywebsite.com/picture/image.png"-> ".png",
      "myuniquefile.longextension"         -> ".longextension",
      "IAmAFileWithoutExtension"           -> "",
      "/path/to.my/file"                   -> "",
      "file.odd_one"                       -> "",
      // Extra, with unicode
	    "café.png"                           -> ".png",
      "file.resumé"                        -> "",
      // with unicode combining characters
	    "cafe\u0301.png"                     -> ".png",
      "file.resume\u0301"                  -> ""
	    )
	      println("isExt() tests:")

  for ((file, isext) <- isExtTestFiles) {
    assert(FileExt.isExt(file, testExtensions) == isext, "Assertion failed for: " + file)
    println("File: " + file + " -> Extension: " + FileExt.extractExt(file))
  }
  println("\nextractExt() tests:")
  for ((url, ext) <- extractExtTestFiles) {
    assert(FileExt.extractExt(url) == ext, "Assertion failed for: " + url)
    println("Url: " + url + " -> Extension: " + FileExt.extractExt(url))
  }
}
```

'''output'''

```txt

Url: picture.jpg -> Extension: .jpg
Url: document.txt_backup -> Extension:
Url: .desktop -> Extension: .desktop
Url: CharacterModel.3DS -> Extension: .3DS
Url: file.resumé -> Extension:
Url: document -> Extension:
Url: café.png -> Extension: .png
Url: /etc/pam.d/login. -> Extension:
Url: http://mywebsite.com/picture/image.png -> Extension: .png
Url: IAmAFileWithoutExtension -> Extension:
Url: /etc/pam.d/login -> Extension:
Url: /etc/pam.d/login.a -> Extension: .a
Url: file.odd_one -> Extension:
Url: /path/to.my/file -> Extension:
Url: myuniquefile.longextension -> Extension: .longextension
Url: café.png -> Extension: .png
Url: file.resumé -> Extension:
Url: http://example.com/download.tar.gz -> Extension: .gz

```



## sed



```sed
-n -re 's:.*(\.[A-Za-z0-9]+)$:\1:p'
```


Example of use:

```bash
for F in "http://example.com/download.tar.gz" "CharacterModel.3DS" ".desktop" "document" "document.txt_backup" "/etc/pam.d/login"
do
  EXT=`echo $F | sed -n -re 's:.*(\.[A-Za-z0-9]+)$:\1:p'`
  echo "$F: $EXT"
done

```

```txt

http://example.com/download.tar.gz: .gz
CharacterModel.3DS: .3DS
.desktop: .desktop
document:
document.txt_backup:
/etc/pam.d/login:

```



## Sidef


```ruby
func extension(filename) {
    filename.match(/(\.[a-z0-9]+)\z/i).to_s
}

var files = [
    'http://example.com/download.tar.gz',
    'CharacterModel.3DS',
    '.desktop',
    'document',
    'document.txt_backup',
    '/etc/pam.d/login',
]

files.each {|f|
    printf("%-36s -> %-11s\n", f.dump, extension(f).dump)
}
```

```txt

"http://example.com/download.tar.gz" -> ".gz"
"CharacterModel.3DS"                 -> ".3DS"
".desktop"                           -> ".desktop"
"document"                           -> ""
"document.txt_backup"                -> ""
"/etc/pam.d/login"                   -> ""

```



## Tcl


Tcl's built in [http://wiki.tcl.tk/10072 file extension] command already almost knows how to do this, except it accepts any character after the dot.  Just for fun, we'll enhance the builtin with a new subcommand with the limitation specified for this problem.


```Tcl
proc assert {expr} {    ;# for "static" assertions that throw nice errors
    if {![uplevel 1 [list expr $expr]]} {
        set msg "{$expr}"
        catch {append msg " {[uplevel 1 [list subst -noc $expr]]}"}
        tailcall throw {ASSERT ERROR} $msg
    }
}

proc file_ext {file} {
    set res ""
    regexp -nocase {\.[a-z0-9]+$} $file res
    return $res
}

set map [namespace ensemble configure file -map]
dict set map ext ::file_ext
namespace ensemble configure file -map $map

# and a test:
foreach {file ext} {
    http://example.com/download.tar.gz      .gz
    CharacterModel.3DS                      .3DS
    .desktop                                .desktop
    document                                ""
    document.txt_backup                     ""
    /etc/pam.d/login                        ""
} {
    set res ""
    assert {[file ext $file] eq $ext}
}
```



## TUSCRIPT


```tuscript

$$ MODE DATA
$$ testcases=*
http://example.com/download.tar.gz
CharacterModel.3DS
.desktop
document
document.txt_backup
/etc/pam.d/login
picture.jpg
http://mywebsite.com/picture/image.png
myuniquefile.longextension
IamAFileWithoutExtension
path/to.my/file
file.odd_one
thisismine

$$ MODE TUSCRIPT,{}

BUILD C_GROUP A0 = *
DATA {&a}
DATA {\0}

BUILD S_TABLE legaltokens=*
DATA :.{1-00}{C:A0}{]}:

LOOP testcase=testcases
 extension=STRINGS (testcase,legaltokens,0,0)
 IF (extension=="") CYCLE
 PRINT testcase, " has extension ", extension
ENDLOOP

```

Output:

```txt

http://example.com/download.tar.gz has extension .gz
CharacterModel.3DS has extension .3DS
.desktop has extension .desktop
picture.jpg has extension .jpg
http://mywebsite.com/picture/image.png has extension .png
myuniquefile.longextension has extension .longextension

```



## VBScript


```vb
Function fileExt(fname)
	Set fso = CreateObject("Scripting.FileSystemObject")
	Set regex = new regExp
        Dim ret

	regex.pattern = "^[A-Za-z0-9]+$"	'Only alphanumeric characters are allowed
	If regex.test(fso.GetExtensionName(fname)) = False Then
		ret = ""
	Else
		ret = "." & fso.GetExtensionName(fname)
	End If
	fileExt = ret
End Function

'Real Start of Program
arr_t = Array("http://example.com/download.tar.gz",	_
	      "CharacterModel.3DS",			_
	      ".desktop",				_
	      "document",				_
	      "document.txt_backup",			_
	      "/etc/pam.d/login")

For Each name In arr_t
	Wscript.Echo "NAME:",name
	Wscript.Echo " EXT:","<" & fileExt(name) & ">"
Next
```


```txt
NAME: http://example.com/download.tar.gz
 EXT: <.gz>
NAME: CharacterModel.3DS
 EXT: <.3DS>
NAME: .desktop
 EXT: <.desktop>
NAME: document
 EXT: <>
NAME: document.txt_backup
 EXT: <>
NAME: /etc/pam.d/login
 EXT: <>
```



## Visual Basic

```vb
Option Explicit
'-----------------------------------------------------------------
Function ExtractFileExtension(ByVal Filename As String) As String
Dim i As Long
Dim s As String
  i = InStrRev(Filename, ".")
  If i Then
    If i < Len(Filename) Then
      s = Mid$(Filename, i)
      For i = 2 To Len(s)
        Select Case Mid$(s, i, 1)
        Case "A" To "Z", "a" To "z", "0" To "9"
            'these characters are OK in an extension; continue
        Case Else
            'this one is not OK in an extension
            Exit Function
        End Select
      Next i
      ExtractFileExtension = s
    End If
  End If
End Function
'-----------------------------------------------------------------
Sub Main()
Dim s As String
  s = "http://example.com/download.tar.gz"
  Debug.Assert ExtractFileExtension(s) = ".gz"
  s = "CharacterModel.3DS"
  Debug.Assert ExtractFileExtension(s) = ".3DS"
  s = ".desktop"
  Debug.Assert ExtractFileExtension(s) = ".desktop"
  s = "document"
  Debug.Assert ExtractFileExtension(s) = ""
  s = "document.txt_backup"
  Debug.Assert ExtractFileExtension(s) = ""
  s = "/etc/pam.d/login"
  Debug.Assert ExtractFileExtension(s) = ""
  s = "desktop."
  Debug.Assert ExtractFileExtension(s) = ""
  s = "a.~.c"
  Debug.Assert ExtractFileExtension(s) = ".c"
  s = "a.b.~"
  Debug.Assert ExtractFileExtension(s) = ""
  s = "a.b.1~2"
  Debug.Assert ExtractFileExtension(s) = ""
End Sub
```



## zkl


The File object has a method splitFileName that does just that, returning a list of the parts. The method knows about the OS it was compiled on (Unix, Windows).

```zkl
fcn extractFileExtension(name){
   var [const] valid=Walker.chain(".",["a".."z"],["A".."Z"],["0".."9")).pump(String);
   ext:=File.splitFileName(name)[-1];
   if(ext - valid) ext="";
   ext
}
```


```zkl
foreach nm in (T("http://example.com/download.tar.gz","CharacterModel.3DS",
		  ".desktop","document",
		  "document.txt_backup","/etc/pam.d/login")){
   println("%35s : %s".fmt(nm,extractFileExtension(nm)));
}
```

Note: on Unix, .desktop is a hidden file, not an extension.

```txt

 http://example.com/download.tar.gz : .gz
                 CharacterModel.3DS : .3DS
                           .desktop :
                           document :
                document.txt_backup :
                   /etc/pam.d/login :

```
