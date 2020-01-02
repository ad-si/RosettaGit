+++
title = "File size distribution"
description = ""
date = 2019-07-29T21:10:56Z
aliases = []
[extra]
id = 21094
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

;Task:
Beginning from the current directory, or optionally from a directory specified as a command-line argument, determine how many files there are of various sizes in a directory hierarchy. 


My suggestion is to sort by logarithmn of file size, since a few bytes here or there, or even a factor of two or three, may not be that significant.
 
Don't forget that empty files may exist, to serve as a marker. 


Is your file system predominantly devoted to a large number of smaller files, or a smaller number of huge files?





## C

The platform independent way to get the file size in C involves opening every file and reading the size. The implementation below works for Windows and utilizes command scripts to get size information quickly even for a large number of files, recursively traversing a large number of directories. Both textual and graphical ( ASCII ) outputs are shown. The same can be done for Linux by a combination of the find, ls and stat commands and my plan was to make it work on both OS types, but I don't have access to a Linux system right now. This would also mean either abandoning scaling the graphical output in order to fit the console buffer or porting that as well, thus including windows.h selectively.

### Windows


```C

#include<windows.h>
#include<string.h>
#include<stdio.h>

#define MAXORDER 25

int main(int argC, char* argV[])
{
	char str[MAXORDER],commandString[1000],*startPath;
	long int* fileSizeLog = (long int*)calloc(sizeof(long int),MAXORDER),max;
	int i,j,len;
	double scale;
	FILE* fp;
	
	if(argC==1)
		printf("Usage : %s <followed by directory to start search from(. for current dir), followed by \n optional parameters (T or G) to show text or graph output>",argV[0]);
	else{
		if(strchr(argV[1],' ')!=NULL){
		len = strlen(argV[1]);
		startPath = (char*)malloc((len+2)*sizeof(char));
		startPath[0] = '\"';
		startPath[len+1]='\"';
		strncpy(startPath+1,argV[1],len);
		startPath[len+2] = argV[1][len];
		sprintf(commandString,"forfiles /p %s /s /c \"cmd /c echo @fsize\" 2>&1",startPath);
	}
	
	else if(strlen(argV[1])==1 && argV[1][0]=='.')
		strcpy(commandString,"forfiles /s /c \"cmd /c echo @fsize\" 2>&1");
	
	else
		sprintf(commandString,"forfiles /p %s /s /c \"cmd /c echo @fsize\" 2>&1",argV[1]);

	fp = popen(commandString,"r");

	while(fgets(str,100,fp)!=NULL){
			if(str[0]=='0')
				fileSizeLog[0]++;
			else
				fileSizeLog[strlen(str)]++;
	}
	
	if(argC==2 || (argC==3 && (argV[2][0]=='t'||argV[2][0]=='T'))){
		for(i=0;i<MAXORDER;i++){
			printf("\nSize Order < 10^%2d bytes : %Ld",i,fileSizeLog[i]);
		}
	}
	
	else if(argC==3 && (argV[2][0]=='g'||argV[2][0]=='G')){
		CONSOLE_SCREEN_BUFFER_INFO csbi;
		int val = GetConsoleScreenBufferInfo(GetStdHandle( STD_OUTPUT_HANDLE ),&csbi);
		if(val)
		{

				max = fileSizeLog[0];
				
				for(i=1;i<MAXORDER;i++)
					(fileSizeLog[i]>max)?max=fileSizeLog[i]:max;
				
				(max < csbi.dwSize.X)?(scale=1):(scale=(1.0*(csbi.dwSize.X-50))/max);
				
				for(i=0;i<MAXORDER;i++){
					printf("\nSize Order < 10^%2d bytes |",i);
					for(j=0;j<(int)(scale*fileSizeLog[i]);j++)
						printf("%c",219);
					printf("%Ld",fileSizeLog[i]);
				}
		}
	
	}
	return 0;
	}
}

```

Invocation and textual output :

```txt

C:\My Projects\threeJS>fileSize.exe "C:\My Projects" t

Size Order < 10^ 0 bytes : 1770
Size Order < 10^ 1 bytes : 1
Size Order < 10^ 2 bytes : 20
Size Order < 10^ 3 bytes : 219
Size Order < 10^ 4 bytes : 1793
Size Order < 10^ 5 bytes : 1832
Size Order < 10^ 6 bytes : 631
Size Order < 10^ 7 bytes : 124
Size Order < 10^ 8 bytes : 26
Size Order < 10^ 9 bytes : 0
Size Order < 10^10 bytes : 0
Size Order < 10^11 bytes : 0
Size Order < 10^12 bytes : 0
Size Order < 10^13 bytes : 0
Size Order < 10^14 bytes : 0
Size Order < 10^15 bytes : 0
Size Order < 10^16 bytes : 0
Size Order < 10^17 bytes : 0
Size Order < 10^18 bytes : 0
Size Order < 10^19 bytes : 0
Size Order < 10^20 bytes : 0
Size Order < 10^21 bytes : 0
Size Order < 10^22 bytes : 0
Size Order < 10^23 bytes : 0
Size Order < 10^24 bytes : 0

```

Invocation and graphical output :

```txt

C:\My Projects\threeJS>fileSize.exe "C:\My Projects" g

Size Order < 10^ 0 bytes |█████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████1770
Size Order < 10^ 1 bytes |1
Size Order < 10^ 2 bytes |██20
Size Order < 10^ 3 bytes |█████████████████████████████219
Size Order < 10^ 4 bytes |████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████1793
Size Order < 10^ 5 bytes |██████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████████1832
Size Order < 10^ 6 bytes |██████████████████████████████████████████████████████████████████████████████████████631
Size Order < 10^ 7 bytes |████████████████124
Size Order < 10^ 8 bytes |███26
Size Order < 10^ 9 bytes |0
Size Order < 10^10 bytes |0
Size Order < 10^11 bytes |0
Size Order < 10^12 bytes |0
Size Order < 10^13 bytes |0
Size Order < 10^14 bytes |0
Size Order < 10^15 bytes |0
Size Order < 10^16 bytes |0
Size Order < 10^17 bytes |0
Size Order < 10^18 bytes |0
Size Order < 10^19 bytes |0
Size Order < 10^20 bytes |0
Size Order < 10^21 bytes |0
Size Order < 10^22 bytes |0
Size Order < 10^23 bytes |0
Size Order < 10^24 bytes |0

```

Note that it is possible to track files up to 10^24 (Yottabyte) in size with this implementation, but if you have a file that large, you shouldn't be needing such programs. :)


## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "log"
    "math"
    "os"
    "path/filepath"
)

func commatize(n int64) string {
    s := fmt.Sprintf("%d", n)
    if n < 0 {
        s = s[1:]
    }
    le := len(s)
    for i := le - 3; i >= 1; i -= 3 {
        s = s[0:i] + "," + s[i:]
    }
    if n >= 0 {
        return s
    }
    return "-" + s
}

func fileSizeDistribution(root string) {
    var sizes [12]int
    files := 0
    directories := 0
    totalSize := int64(0)
    walkFunc := func(path string, info os.FileInfo, err error) error {
        if err != nil {
            return err
        }
        files++
        if info.IsDir() {
            directories++
        }
        size := info.Size()
        if size == 0 {
            sizes[0]++
            return nil
        }
        totalSize += size
        logSize := math.Log10(float64(size))
        index := int(math.Floor(logSize))
        sizes[index+1]++
        return nil
    }
    err := filepath.Walk(root, walkFunc)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("File size distribution for '%s' :-\n\n", root)
    for i := 0; i < len(sizes); i++ {
        if i == 0 {
            fmt.Print("  ")
        } else {
            fmt.Print("+ ")
        }
        fmt.Printf("Files less than 10 ^ %-2d bytes : %5d\n", i, sizes[i])
    }
    fmt.Println("                                  -----")
    fmt.Printf("= Total number of files         : %5d\n", files)
    fmt.Printf("  including directories         : %5d\n", directories)
    c := commatize(totalSize)
    fmt.Println("\n  Total size of files           :", c, "bytes")
}

func main() {
    fileSizeDistribution("./")
}
```


{{out}}

```txt

File size distribution for './' :-

  Files less than 10 ^ 0  bytes :     0
+ Files less than 10 ^ 1  bytes :     0
+ Files less than 10 ^ 2  bytes :     8
+ Files less than 10 ^ 3  bytes :    98
+ Files less than 10 ^ 4  bytes :   163
+ Files less than 10 ^ 5  bytes :    18
+ Files less than 10 ^ 6  bytes :     8
+ Files less than 10 ^ 7  bytes :    18
+ Files less than 10 ^ 8  bytes :     1
+ Files less than 10 ^ 9  bytes :     0
+ Files less than 10 ^ 10 bytes :     0
+ Files less than 10 ^ 11 bytes :     0
                                  -----
= Total number of files         :   314
  including directories         :     7

  Total size of files           : 74,205,408 bytes

```



## Julia

{{works with|Julia|0.6}}


```julia
using Humanize

function sizelist(path::AbstractString)
    rst = Vector{Int}(0)
    for (root, dirs, files) in walkdir(path)
        files = joinpath.(root, files)
        tmp = collect(filesize(f) for f in files if !islink(f))
        append!(rst, tmp)
    end
    return rst
end

byclass(y, classes) = Dict{eltype(classes),Int}(c => count(c[1] .≤ y .< c[2]) for c in classes)

function main(path::AbstractString)
    s = sizelist(path)
    cls = append!([(0, 1)], collect((10 ^ (i-1), 10 ^ i) for i in 1:9))
    f = byclass(s, cls)

    println("filesizes: ")
    for c in cls
        @printf(" - between %8s and %8s bytes: %3i\n", datasize(c[1]), datasize(c[2]), f[c])
    end
    println("\n-> total: $(datasize(sum(s))) bytes and $(length(s)) files")
end

main(".")
```


{{out}}

```txt
filesizes: 
 - between    0.0 B and    1.0 B bytes:   0
 - between    1.0 B and   10.0 B bytes:   1
 - between   10.0 B and  100.0 B bytes:  44
 - between  100.0 B and   1.0 kB bytes: 1068
 - between   1.0 kB and  10.0 kB bytes: 250
 - between  10.0 kB and 100.0 kB bytes:   7
 - between 100.0 kB and   1.0 MB bytes:   4
 - between   1.0 MB and  10.0 MB bytes:   2
 - between  10.0 MB and 100.0 MB bytes:   0
 - between 100.0 MB and   1.0 GB bytes:   0

-> total: 7.3 MB bytes and 1376 files
```



## Kotlin


```scala
// version 1.2.10

import java.io.File
import kotlin.math.log10
import kotlin.math.floor

fun fileSizeDistribution(path: String) {
    val sizes = IntArray(12)
    val p = File(path)
    val files = p.walk()
    var accessible = 0
    var notAccessible = 0
    var totalSize = 0L
    for (file in files) {
        try {
            if (file.isFile()) {
                val len = file.length()
                accessible++
                if (len == 0L) {
                    sizes[0]++
                    continue
                }
                totalSize += len
                val logLen = log10(len.toDouble())
                val index = floor(logLen).toInt()
                sizes[index + 1]++
            }
        }
        catch (se: SecurityException) {
            notAccessible++
        }
    }

    println("File size distribution for '$path' :-\n")
    for (i in 0 until sizes.size) {
        print(if (i == 0) "  " else "+ ")
        print("Files less than 10 ^ ${"%-2d".format(i)} bytes : ")
        println("%5d".format(sizes[i]))
    }
    println("                                  -----")
    println("= Number of accessible files    : ${"%5d".format(accessible)}")
    println("\n  Total size in bytes           : $totalSize")
    println("\n  Number of inaccessible files  : ${"%5d".format(notAccessible)}")
}

fun main(args: Array<String>) {
    fileSizeDistribution("./")  // current directory
}
```


{{out}}

```txt

File size distribution for './' :-

  Files less than 10 ^ 0  bytes :     2
+ Files less than 10 ^ 1  bytes :     0
+ Files less than 10 ^ 2  bytes :    46
+ Files less than 10 ^ 3  bytes :   380
+ Files less than 10 ^ 4  bytes :   558
+ Files less than 10 ^ 5  bytes :    19
+ Files less than 10 ^ 6  bytes :     6
+ Files less than 10 ^ 7  bytes :     5
+ Files less than 10 ^ 8  bytes :     0
+ Files less than 10 ^ 9  bytes :     0
+ Files less than 10 ^ 10 bytes :     0
+ Files less than 10 ^ 11 bytes :     0
                                  -----
= Number of accessible files    :  1016

  Total size in bytes           : 14459732

  Number of inaccessible files  :     0

```



## Perl

{{trans|Perl 6}}

```perl
use File::Find;
use List::Util qw(max);

my %fsize;
$dir = shift || '.';
find(\&fsize, $dir);

$max = max($max,$fsize{$_}) for keys %fsize;
$total += $size while (undef,$size) = each %fsize;

print "File size distribution in bytes for directory: $dir\n";
for (0 .. max(keys %fsize)) {
    printf "# files @ %4sb %8s: %s\n", $_ ? '10e'.($_-1) : 0, $fsize{$_} // 0,
       histogram( $max, $fsize{$_} // 0, 80);
}
print "$total total files.\n";

sub histogram {
    my($max, $value, $width) = @_;
    my @blocks = qw<| ▏ ▎ ▍ ▌ ▋ ▊ ▉ █>;
    my $scaled = int $value * $width / $max;
    my $end =     $scaled % 8;
    my $bar = int $scaled / 8;
    my $B = $blocks[8] x ($bar * 8) . ($end ? $blocks[$end] : '');
}

sub fsize { $fsize{ log10( (lstat($_))[7] ) }++ }
sub log10 { my($s) = @_; $s ? int log($s)/log(10) : 0 }
```

{{out}}

```txt
File size distribution in bytes for directory: .
# files @    0b        5:
# files @ 10e0b    46455: ████████████████████████████████████████████████████████████████████████████████
# files @ 10e1b    26146: ████████████████████████████████████████▋
# files @ 10e2b     3993: ▊
# files @ 10e3b     1222: ▎
# files @ 10e4b       19:
# files @ 10e5b        3:
77843 total files.
```



## Perl 6

{{works with|Rakudo|2017.05}}
By default, process the current and all readable sub-directories, or, pass in a directory path at the command line.


```perl6
sub MAIN($dir = '.') {
    sub log10 (Int $s) { $s ?? $s.log(10).Int !! 0 }
    my %fsize;
    my @dirs = $dir.IO;
    while @dirs {
        for @dirs.pop.dir -> $path {
            %fsize{$path.s.&log10}++ if $path.f;
            @dirs.push: $path if $path.d and $path.r
        }
    }
    my $max = %fsize.values.max;
    my $bar-size = 80;
    say "File size distribution in bytes for directory: $dir\n";
    for 0 .. %fsize.keys.max {
          say sprintf( "# Files @ %5sb %8s: ", $_ ?? "10e{$_-1}" !! 0, %fsize{$_} // 0 ),
              histogram( $max, %fsize{$_} // 0, $bar-size )
    }
    say %fsize.values.sum, ' total files.';
}

sub histogram ($max, $value, $width = 60) {
    my @blocks = <| ▏ ▎ ▍ ▌ ▋ ▊ ▉ █>;
    my $scaled = ($value * $width / $max).Int;
    my ($end, $bar) = $scaled.polymod(8);
    (@blocks[8] x $bar * 8) ~ (@blocks[$end] if $end) ~ "\n"
}
```


{{out}}

```txt
File size distribution in bytes for directory: /home

# Files @     0b      989: ▏

# Files @  10e0b     6655: ████████

# Files @  10e1b    31776: ████████████████████████████████████████

# Files @  10e2b    63165: ████████████████████████████████████████████████████████████████████████████████

# Files @  10e3b    19874: ████████████████████████▏

# Files @  10e4b     7730: ████████▏

# Files @  10e5b     3418: ▌

# Files @  10e6b     1378: ▏

# Files @  10e7b      199:

# Files @  10e8b       45:

135229 total files.
```



## Phix

Works on Windows and Linux. Uses "proper" sizes, ie 1MB==1024KB. Can be quite slow at first, but is pretty fast on the second and subsequent runs, that is once the OS has cached its (low-level) directory reads.

```Phix
sequence sizes = {1},
         res = {0}
atom t1 = time()+1

function store_res(string filepath, sequence dir_entry)
    if not find('d', dir_entry[D_ATTRIBUTES]) then
        atom size = dir_entry[D_SIZE]
        integer sdx = 1
        while size>sizes[sdx] do
            if sdx=length(sizes) then       
                sizes &= sizes[$]*iff(mod(length(sizes),3)?10:10.24)
                res &= 0
            end if
            sdx += 1
        end while
        res[sdx] += 1
        if time()>t1 then
            printf(1,"%,d files found\r",sum(res))
            t1 = time()+1
        end if
    end if
    return 0 -- keep going
end function
integer exit_code = walk_dir(".", routine_id("store_res"), true)

printf(1,"%,d files found\n",sum(res))
integer w = max(res)
include builtins/pfile.e
for i=1 to length(res) do
    integer ri = res[i]
    string s = file_size_k(sizes[i], 5),
           p = repeat('*',floor(60*ri/w))
    printf(1,"files < %s: %s%,d\n",{s,p,ri})
end for
```

{{out}}

```txt

112,160 files found
files <     1: 333
files <    10: *911
files <   100: ******4,731
files <   1KB: ********************************24,332
files <  10KB: ************************************************************45,379
files < 100KB: *********************************25,299
files <   1MB: *************10,141
files <  10MB: *933
files < 100MB: 91
files <   1GB: 8
files <  10GB: 2

```



## Python

The distribution is stored in a '''collections.Counter''' object (like a dictionary with automatic 0 value when a key is not found, useful when incrementing). Anything could be done with this object, here the number of files is printed for increasing sizes. No check is made during the directory walk: usually, safeguards would be needed or the program will fail on any unreadable file or directory (depending on rights, or too deep paths, for instance). Here links are skipped, so it should avoid cycles.


```python
import sys, os
from collections import Counter

def dodir(path):
    global h

    for name in os.listdir(path):
        p = os.path.join(path, name)

        if os.path.islink(p):
            pass
        elif os.path.isfile(p):
            h[os.stat(p).st_size] += 1
        elif os.path.isdir(p):
            dodir(p)
        else:
            pass

def main(arg):
    global h
    h = Counter()
    for dir in arg:
        dodir(dir)
    
    s = n = 0
    for k, v in sorted(h.items()):
        print("Size %d -> %d file(s)" % (k, v))
        n += v
        s += k * v
    print("Total %d bytes for %d files" % (s, n))

main(sys.argv[1:])
```



## Racket



```racket
#lang racket

(define (file-size-distribution (d (current-directory)) #:size-group-function (sgf values))
  (for/fold ((rv (hash)) (Σ 0) (n 0)) ((f (in-directory d)) #:when (file-exists? f))
    (define sz (file-size f))
    (values (hash-update rv (sgf sz) add1 0) (+ Σ sz) (add1 n))))

(define (log10-or-so x) (if (zero? x) #f (round (/ (log x) (log 10)))))

(define number-maybe-<
  (match-lambda** [(#f #f) #f]
                  [(#f _) #t]
                  [(_ #f) #f]
                  [(a b) (< a b)]))

(define ...s? (match-lambda** [(one 1) one] [(one n) (string-append one "s")]))

(define ((report-fsd f) fsd Σ n)
  (for/list ((k (in-list (sort (hash-keys fsd) number-maybe-<))))
    (printf "~a(size): ~a -> ~a ~a~%"
            (object-name f)
            k
            (hash-ref fsd k) (...s? "file" (hash-ref fsd k))))
  (printf "Total: ~a ~a in ~a ~a~%" Σ (...s? "byte" Σ) n (...s? "file" n)))

(module+ test
  (call-with-values (λ () (file-size-distribution #:size-group-function log10-or-so))
                    (report-fsd log10-or-so)))
```


{{out}}

```txt
log10-or-so(size): #f -> 3 files
log10-or-so(size): 0 -> 4 files
log10-or-so(size): 1.0 -> 39 files
log10-or-so(size): 2.0 -> 57 files
log10-or-so(size): 3.0 -> 406 files
log10-or-so(size): 4.0 -> 198 files
log10-or-so(size): 5.0 -> 20 files
log10-or-so(size): 6.0 -> 6 files
Total: 10210127 bytes in 733 files
```



## REXX

This REXX version works for Microsoft Windows using the   '''dir'''   subcommand;   extra code was added for 

older versions of Windows that used suffixes to express big numbers   (the size of a file),   and also versions

that used a mixed case for showing the output text.  

Also, some Windows versions of the   '''dir'''   command insert commas into numbers, so code was added to elide them.

```rexx
/*REXX program displays a histogram of filesize distribution of a directory structure(s)*/
numeric digits 30                                /*ensure enough decimal digits for a #.*/
parse arg ds .                                   /*obtain optional argument from the CL.*/
parse source . . path .                          /*   "   the path of this REXX program.*/
fID= substr(path, 1 + lastpos('\', path) )       /*   "   the filename and the filetype.*/
parse var  fID   fn  '.'                         /*   "   just the pure filename of pgm.*/
sw=max(79, linesize() - 1)                       /*   "   terminal width (linesize) - 1.*/
                                work= fn".OUT"   /*filename for workfile output of  DIR.*/
'DIR'   ds   '/s /-c /a-d  >'   work             /*do (DOS) DIR cmd for a data structure*/
call linein 0, 1                                 /*open output file, point to 1st record*/
maxL= 0;    @.= 00;      g= 0                    /*max len size; log array; # good recs.*/
$=0                                              /*$:  total bytes used by files found. */
     do while lines(work)\==0;  _= linein(work)  /*process the data in the DIR work file*/
     if left(_, 1)==' '    then iterate          /*Is the record not legitimate?  Skip. */
     parse upper  var   _    .  .  sz  .         /*uppercase the suffix  (if any).      */
     sz= space( translate(sz, , ','),  0)        /*remove any commas if present in the #*/

     if \datatype(sz,'W')  then do; #= left(sz, length(sz) - 1)       /*SZ has a suffix?*/
                                    if \datatype(#,'N')  then iterate /*Meat ¬ numeric? */
                                    sz= # * 1024 ** pos( right(sz, 1), 'KMGTPEZYXWVU') / 1
                                end                                   /* [↑]  use suffix*/
     $= $ + sz                                   /*keep a running total for the filesize*/
     if sz==0  then L= 0                         /*handle special case for an empty file*/
               else L= length(sz)                /*obtain the length of filesize number.*/
     g= g + 1                                    /*bump the counter of # of good records*/
     maxL= max(L, maxL)                          /*get max length filesize for alignment*/
     @.L= @.L + 1                                /*bump counter of record size category.*/
     end   /*j*/                                 /* [↑]   categories:  split by log ten.*/

if g==0  then do;  say 'file not found: '  ds;  exit 13;    end        /*no good records*/
say  ' record size range    count   '
hdr= '══════════════════ ══════════ ';     say hdr;         Lhdr=length(hdr)
mC=0                                             /*mC:  the maximum count for any range.*/
     do   t=1  to 2                              /*T==1   is used to find the max count.*/
       do k=0  to maxL;  mC= max(mC, @.k);  if t==1  then iterate           /*1st pass? */
                             if k==0  then y= center('zero',  length( word(hdr, 1)  ) )
                                      else y= '10^'left(k-1,2)  "──► 10^"left(k,2)  '-1'
       say y || right( commas(@.k), 11)   copies('─', max(1, (@.k / mC * sw % 1) - LHdr) )
       end   /*k*/
     end     /*y*/
say
trace off;   'ERASE'  work                       /*perform clean─up (erase a work file).*/
say commas(g)      ' files detected, '       commas($)        " total bytes."
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _;  do j#=length(_)-3  to 1  by -3; _=insert(',', _, j#); end;  return _
```

This REXX program makes use of   '''LINESIZE'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console) so as to maximize the width of the histogram. 

The   '''LINESIZE.REX'''   REXX program is included here   ──►   [[LINESIZE.REX]].


{{out|output|text=  when using the default input:   (which in this case was the   '''C:'''   homedrive, a Windows/XP system.)}}

```txt

 record size range    count
══════════════════ ══════════
       zero             7,611 ─────────
10^0  ──► 10^1  -1        201 ─
10^1  ──► 10^2  -1        884 ─
10^2  ──► 10^3  -1      4,893 ─
10^3  ──► 10^4  -1     18,344 ─────────────────────────────────────────────────────────────────
10^4  ──► 10^5  -1     13,853 ─────────────────────────────────────────
10^5  ──► 10^6  -1      5,235 ─
10^6  ──► 10^7  -1        968 ─
10^7  ──► 10^8  -1        151 ─
10^8  ──► 10^9  -1          3 ─
10^9  ──► 10^10 -1          1 ─

52,144  files detected,  12,519,430,837  total bytes.

```


{{out|output|text=  when using the default input:   (which in this case was the   '''C:'''   homedrive, a Windows 7 system.)}}

```txt

 record size range    count
══════════════════ ══════════
       zero               160 ─
10^0  ──► 10^1  ─1        123 ─
10^1  ──► 10^2  ─1      2,254 ─
10^2  ──► 10^3  ─1     22,752 ─────────
10^3  ──► 10^4  ─1     54,519 ─────────────────────────────────────────────────────────────────
10^4  ──► 10^5  ─1     36,810 ──────────────────────────────────
10^5  ──► 10^6  ─1     17,491 ─
10^6  ──► 10^7  ─1      9,659 ─
10^7  ──► 10^8  ─1        548 ─
10^8  ──► 10^9  ─1        144 ─
10^9  ──► 10^10 ─1          8 ─
10^10 ──► 10^11 ─1          1 ─

144,469  files detected,  118,733,891,020  total bytes.

```


{{out|output|text=  when using the (my)   '''K:'''   drive:}}

```txt

 record size range    count
══════════════════ ══════════
       zero                28 ─
10^0  ──► 10^1  -1        132 ─
10^1  ──► 10^2  -1        812 ─
10^2  ──► 10^3  -1      3,810 ───────────────────────
10^3  ──► 10^4  -1      5,901 ────────────────────────────────────────────────────
10^4  ──► 10^5  -1      6,828 ─────────────────────────────────────────────────────────────────
10^5  ──► 10^6  -1      2,409 ───
10^6  ──► 10^7  -1        231 ─
10^7  ──► 10^8  -1          5 ─

20,156  files detected,  1,569,799,557  total bytes.

```



## Sidef


```ruby
func traverse(Block callback, Dir dir) {
    dir.open(\var dir_h) || return nil
 
    for entry in (dir_h.entries) {
        if (entry.kind_of(Dir)) {
            traverse(callback, entry)
        } else {
            callback(entry)
        }
    }
}
 
var dir = (ARGV ? Dir(ARGV[0]) : Dir.cwd)

var group = Hash()
var files_num = 0
var total_size = 0

traverse({ |file|
    group{file.size+1 -> log10.round} := 0 += 1
    total_size += file.size
    files_num += 1
}, dir)

for k,v in (group.sort_by { |k,_| Num(k) }) {
    say "log10(size) ~~ #{k} -> #{v} files"
}

say "Total: #{total_size} bytes in #{files_num} files"
```

{{out}}

```txt

$ sidef script.sf /usr/bin
log10(size) ~~ 1 -> 4 files
log10(size) ~~ 2 -> 70 files
log10(size) ~~ 3 -> 246 files
log10(size) ~~ 4 -> 1337 files
log10(size) ~~ 5 -> 815 files
log10(size) ~~ 6 -> 167 files
log10(size) ~~ 7 -> 9 files
log10(size) ~~ 8 -> 2 files
Total: 370026462 bytes in 2650 files

```



## zkl


```zkl
pipe:=Thread.Pipe();
    // hoover all files in tree, don't return directories
fcn(pipe,dir){ File.globular(dir,"*",True,8,pipe); }
.launch(pipe,vm.arglist[0]);  // thread

dist,N,SZ,maxd:=List.createLong(50,0),0,0,0;
foreach fnm in (pipe){
   sz,szd:=File.len(fnm), sz.numDigits;
   dist[szd]+=1;
   N+=1; SZ+=sz; maxd=maxd.max(szd);
}
println("Found %d files, %,d bytes, %,d mean.".fmt(N,SZ,SZ/N));
scale:=50.0/(0.0).max(dist);
szchrs,idx,comma:=",nnn"*20, -1, Walker.cycle(0,0,1).next;
println("%15s   %s (* = %.2f)".fmt("File size","Number of files",1.0/scale));
foreach sz,cnt in ([0..].zip(dist[0,maxd])){
   println("%15s : %s".fmt(szchrs[idx,*], "*"*(scale*cnt).round().toInt()));
   idx-=1 + comma();
}
```

{{out}}

```txt

$ zkl flSzDist.zkl ..
Found 1832 files, 108,667,806 bytes, 59,316 mean.
      File size   Number of files (* = 13.44)
              n : *
             nn : ***
            nnn : ********
          n,nnn : **********************************
         nn,nnn : **************************************************
        nnn,nnn : ********************************
      n,nnn,nnn : *******

$ zkl flSzDist.zkl /media/Tunes/
Found 4320 files, 67,627,849,052 bytes, 15,654,594 mean.
      File size   Number of files (* = 69.84)
              n : 
             nn : 
            nnn : 
          n,nnn : *
         nn,nnn : 
        nnn,nnn : 
      n,nnn,nnn : *
     nn,nnn,nnn : **************************************************
    nnn,nnn,nnn : ********
  n,nnn,nnn,nnn : *

```

