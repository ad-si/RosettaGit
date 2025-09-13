+++
title = "Find duplicate files"
description = ""
date = 2019-07-29T17:11:18Z
aliases = []
[extra]
id = 13271
[taxonomies]
categories = ["task"]
tags = []
+++

{{draft task}} In a large directory structure it is easy to inadvertently leave unnecessary copies of files around, which can use considerable disk space and create confusion. Create a program which, given a minimum size and a folder/directory, will find all files of at least ''size'' bytes with duplicate contents under the directory and output or show the sets of duplicate files in order of decreasing size.

The program may be command-line or graphical, and duplicate content may be determined by direct comparison or by calculating a hash of the data. Specify which filesystems or operating systems your program works with if it has any filesystem- or OS-specific requirements. Identify hard links (filenames referencing the same content) in the output if applicable for the filesystem. For extra points detect when whole directory sub-trees are identical, or optionally remove or link identical files.


## Elixir

```elixir
defmodule Files do
  def find_duplicate_files(dir) do
    IO.puts "\nDirectory : #{dir}"
    File.cd!(dir, fn ->
      Enum.filter(File.ls!, fn fname -> File.regular?(fname) end)
      |> Enum.group_by(fn file -> File.stat!(file).size end)
      |> Enum.filter(fn {_, files} -> length(files)>1 end)
      |> Enum.each(fn {size, files} ->
           Enum.group_by(files, fn file -> :erlang.md5(File.read!(file)) end)
           |> Enum.filter(fn {_, files} -> length(files)>1 end)
           |> Enum.each(fn {_md5, fs} ->
                IO.puts "  --------------------------------------------"
                Enum.each(fs, fn file ->
                  IO.puts "  #{inspect File.stat!(file).mtime}\t#{size}  #{file}"
                end)
              end)
         end)
    end)
  end
end

hd(System.argv) |> Files.find_duplicate_files
```


```txt

C:\Elixir>elixir find_dup_file.exs \Windows\System32

Directory : \Windows\System32
  --------------------------------------------
  {{2009, 7, 14}, {1, 0, 32}}   31548  perfd009.dat
  {{2010, 11, 21}, {7, 14, 4}}  31548  perfd011.dat
  --------------------------------------------
  {{2015, 4, 29}, {18, 21, 50}} 5120  msdxm.ocx
  {{2015, 4, 29}, {18, 21, 50}} 5120  dxmasf.dll
  --------------------------------------------
  {{2010, 11, 21}, {3, 23, 55}} 91648  mapi32.dll
  {{2010, 11, 21}, {3, 23, 55}} 91648  mapistub.dll
  --------------------------------------------
  {{2014, 4, 11}, {13, 39, 56}} 18088  msvcp110_clr0400.dll
  {{2014, 4, 11}, {13, 39, 56}} 18088  msvcr100_clr0400.dll
  {{2014, 4, 11}, {13, 39, 56}} 18088  msvcr110_clr0400.dll

```



## Go

In theory this should work on any of the operating systems supported by Go (Linux, macOS, Windows, OpenBSD etc.) though only tested on Ubuntu 16.04.

```go
package main

import (
    "fmt"
    "crypto/md5"
    "io/ioutil"
    "log"
    "os"
    "path/filepath"
    "sort"
    "time"
)

type fileData struct {
    filePath string
    info     os.FileInfo
}

type hash [16]byte

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func checksum(filePath string) hash {
    bytes, err := ioutil.ReadFile(filePath)
    check(err)
    return hash(md5.Sum(bytes))
}

func findDuplicates(dirPath string, minSize int64) [][2]fileData {
    var dups [][2]fileData
    m := make(map[hash]fileData)
    werr := filepath.Walk(dirPath, func(path string, info os.FileInfo, err error) error {
        if err != nil {
            return err
        }
        if !info.IsDir() && info.Size() >= minSize {
            h := checksum(path)
            fd, ok := m[h]
            fd2 := fileData{path, info}
            if !ok {
                m[h] = fd2
            } else {
                dups = append(dups, [2]fileData{fd, fd2})
            }
        }
        return nil
    })
    check(werr)
    return dups
}

func main() {
    dups := findDuplicates(".", 1)
    fmt.Println("The following pairs of files have the same size and the same hash:\n")
    fmt.Println("File name                 Size      Date last modified")
    fmt.Println("
### ====================================================
")
    sort.Slice(dups, func(i, j int) bool {
        return dups[i][0].info.Size() > dups[j][0].info.Size() // in order of decreasing size
    })
    for _, dup := range dups {
        for i := 0; i < 2; i++ {
            d := dup[i]
            fmt.Printf("%-20s  %8d    %v\n", d.filePath, d.info.Size(), d.info.ModTime().Format(time.ANSIC))
        }
        fmt.Println()
    }
}
```


Sample output:

```txt

The following pairs of files have the same size and the same hash:

File name                 Size      Date last modified

### ====================================================

vib.gif                 689113    Wed Sep 26 16:33:34 2018
vibrating.gif           689113    Tue Oct  2 00:38:08 2018

analysis2.txt             6155    Thu Sep 13 12:19:06 2018
temp/analysis3.txt        6155    Fri Dec 28 15:20:54 2018

w_pinstripe.png           2994    Tue Sep 25 12:18:05 2018
wb_pinstripe.png          2994    Tue Sep 25 12:06:53 2018

sox.txt                     63    Sat Dec 22 21:59:23 2018
sox2.txt                    63    Fri Dec 28 12:19:02 2018

```



## Haskell



```txt

- checks for wrong command line input (not existing directory / negative size)
- works on Windows as well as Unix Systems (tested with Mint 17 / Windows 7)

```


```Haskell

import Crypto.Hash.MD5        (hash)
import Data.ByteString as BS  (readFile, ByteString())
import System.Environment     (getArgs, getProgName)
import System.Directory       (doesDirectoryExist, getDirectoryContents)
import System.FilePath.Posix  ((</>))
import Control.Monad          (forM)
import Text.Printf            (printf)
import System.IO              (withFile, IOMode(ReadMode), hFileSize)


type File = (BS.ByteString, -- md5hash
             FilePath)      -- filepath

type FileSize = Integer

getRecursiveContents :: FilePath -> FileSize -> IO [File]
getRecursiveContents curDir maxsize = do
  names <- getDirectoryContents curDir
  let dirs = filter (`notElem` [".", ".."]) names
  files <- forM dirs $ \path -> do
             let path' = curDir </> path
             exists <- doesDirectoryExist path'
             if exists
                then getRecursiveContents path' maxsize
                else genFileHash path' maxsize
  return $ concat files


genFileHash :: FilePath -> FileSize -> IO [File]
genFileHash path maxsize = do
  size <- withFile path ReadMode hFileSize
  if size <= maxsize
    then BS.readFile path >>= \bs -> return [(hash bs, path)]
    else return []

findDuplicates :: FilePath -> FileSize -> IO ()
findDuplicates dir bytes = do
  exists <- doesDirectoryExist dir
  if exists
    then getRecursiveContents dir bytes >>= findSameHashes
    else printf "Sorry, the directory \"%s\" does not exist...\n" dir

findSameHashes :: [File] -> IO ()
findSameHashes []     = return ()
findSameHashes ((hash, fp):xs) = do
  case lookup hash xs of
    (Just dupFile) -> printf "
### =====================
\n\
                            \Found duplicate:\n\
                            \=> %s \n\
                            \=> %s \n\n" fp dupFile
                      >> findSameHashes xs
    (_)            -> findSameHashes xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir, mbytes] | [(bytes ,"")] <- reads mbytes
                   , bytes >= 1 -> findDuplicates dir bytes
    (_) -> do
      name <- getProgName
      printf "Something went wrong - please use ./%s <dir> <bytes>\n" name


```


Example output:


```txt

$./finddups ~/Documents/MyGit/Haskell/ 20000

### =====================

Found duplicate:
=> /home/rewrite/Documents/MyGit/Haskell/.git/logs/HEAD 
=> /home/rewrite/Documents/MyGit/Haskell/.git/logs/refs/heads/master 


### =====================

Found duplicate:
=> /home/rewrite/Documents/MyGit/Haskell/.git/refs/remotes/origin/master 
=> /home/rewrite/Documents/MyGit/Haskell/.git/refs/heads/master 


### =====================

Found duplicate:
=> /home/rewrite/Documents/MyGit/Haskell/RosettaCode/Find-duplicate-files/sampletext.txt 
=> /home/rewrite/Documents/MyGit/Haskell/RosettaCode/otherdup.txt 


### =====================

Found duplicate:
=> /home/rewrite/Documents/MyGit/Haskell/RWH/FileManipulation/toupper-imp.hs 
=> /home/rewrite/Documents/MyGit/Haskell/RWH/FileManipulation/toupper-imp.hs~ 


$./finddups /home/rewrite/NotExistingDir 200000
Sorry, the directory "/home/rewrite/NotExistingDir" does not exist...


$./finddups /home/rewrite/ -100
Something went wrong - please use ./finddups <dir> <bytes>


```



## Objeck

Solution works on Windows, macOS and Linux.

```objeck
use System.IO.File;
use System.Time;
use Collection;

class Duplicate {
  function : Main(args : String[]) ~ Nil {
    if(args->Size() = 2) {
      file_sets := SortDups(GetDups(args[0], args[1]->ToInt()));
      each(i : file_sets) {
        file_set := file_sets->Get(i)->As(Vector);
        if(file_set->Size() > 1) {
          "Duplicates:"->PrintLine();
          "----"->PrintLine();
          each(j : file_set) {
            file_set->Get(j)->As(FileMeta)->ToString()->PrintLine();
          };
        };
        '\n'->Print();
      };
    };
  }

  function : SortDups(unsorted : Vector) ~ Vector {
    sorted := IntMap->New();

    each(i : unsorted) {
      value := unsorted->Get(i)->As(Vector);
      key := value->Get(0)->As(FileMeta)->GetSize();
      sorted->Insert(key, value);
    };

    return sorted->GetValues();
  }

  function : GetDups(dir : String, size : Int) ~ Vector {
    duplicates := StringMap->New();

    files := Directory->List(dir);
    each(i : files) {
      file_name := String->New(dir);
      file_name += '/';
      file_name += files[i];

      file_size := File->Size(file_name);
      if(file_size >= size) {
        file_date := File->ModifiedTime(file_name);
        file_hash := file_size->ToString();
        file_hash += ':';
        file_hash += Encryption.Hash->MD5(FileReader->ReadBinaryFile(file_name))->ToString();
        file_meta := FileMeta->New(file_name, file_size, file_date, file_hash);

        file_set := duplicates->Find(file_hash)->As(Vector);
        if(file_set = Nil) {
          file_set := Vector->New();
          duplicates->Insert(file_hash, file_set);
        };
        file_set->AddBack(file_meta);
      };
    };

    return duplicates->GetValues();
  }
}

class FileMeta {
  @name : String;
  @size : Int;
  @date : Date;
  @hash : String;

  New(name : String, size : Int, date : Date, hash : String) {
    @name := name;
    @size := size;
    @date := date;
    @hash := hash;
  }

  method : public : GetSize() ~ Int {
    return @size;
  }

  method : public : ToString() ~ String {
    date_str := @date->ToShortString();
    return "{$@name}, {$@size}, {$date_str}";
  }
}
```


```txt

$ obr duplicate.obe /tmp/foo 4000
Duplicates:
----
/tmp/foo/bb.obe, 19822, 3/29/2019 8:07:21 PM
/tmp/foo/aa.obe, 19822, 3/29/2019 8:07:17 PM

Duplicates:
----
/tmp/foo/hh.obe, 20020, 3/29/2019 8:47:43 PM
/tmp/foo/gg.obe, 20020, 3/29/2019 8:47:37 PM
/tmp/foo/ee.obe, 20020, 3/29/2019 8:47:33 PM
/tmp/foo/dd.obe, 20020, 3/29/2019 8:47:14 PM

```



## Perl

For supplied directory, compare all files, recursing into sub-directories. By default, showing duplicate files of 1 byte or larger, configurable with command-line option. Using CPAN <code>File</code> modules for enhanced portability.

```perl
use File::Find qw(find);
use File::Compare qw(compare);
use Sort::Naturally;
use Getopt::Std qw(getopts);

my %opts;
$opts{s} = 1;
getopts("s:", \%opts);

sub find_dups {
    my($dir) = @_;

    my @results;
    my %files;
    find {
        no_chdir => 1,
        wanted => sub { lstat; -f _ && (-s >= $opt{s} ) && push @{$files{-s _}}, $_ }
    } => $dir;

    foreach my $files (values %files) {
        next unless @$files;

        my %dups;
        foreach my $a (0 .. @$files - 1) {
            for (my $b = $a + 1 ; $b < @$files ; $b++) {
                next if compare(@$files[$a], @$files[$b]);
                push @{$dups{ @$files[$a] }}, splice @$files, $b--, 1;
            }
        }

        while (my ($original, $clones) = each %dups) {
            push @results, sprintf "%8d %s\n", (stat($original))[7], join ', ', sort $original, @$clones;
        }
    }
    reverse nsort @results;

}

print for find_dups(@ARGV);
```

```txt
     372 aaa.txt, dir2/aaa.txt
      29 bbb.txt, dir1/bbb.txt
```



## Perl 6

This implementation takes a starting directory (defaults to the current directory) and has a few flags to set behaviour: --minsize, minimum file size to look at, defaults to 5 bytes; and --recurse, recurse into the directory structure, default True. It finds files of the same size, calculates hashes to compare, then reports files that hash the same. Uses the very fast but cryptographically poor xxHash library to hash the files.  


```perl6
use Digest::xxHash;

sub MAIN( $dir = '.', :$minsize = 5, :$recurse = True ) {
    my %files;
    my @dirs = $dir.IO.absolute.IO;
    while @dirs {
        my @files = @dirs.pop;
        while @files {
            for @files.pop.dir -> $path {
                %files{ $path.s }.push: $path if $path.f and $path.s >= $minsize;
                @dirs.push: $path if $path.d and $path.r and $recurse
            }
        }
    }

    for %files.sort( +*.key ).grep( *.value.elems > 1)».kv -> ($size, @list) {
        my %dups;
        @list.map: { %dups{ xxHash( $_.slurp :bin ) }.push: $_.Str };
        for %dups.grep( *.value.elems > 1)».value -> @dups {
            say sprintf("%9s : ", scale $size ),  @dups.join(', ');
        }
    }
}

sub scale ($bytes) {
    given $bytes {
        when $_ < 2**10 {  $bytes                    ~ ' B'  }
        when $_ < 2**20 { ($bytes / 2**10).round(.1) ~ ' KB' }
        when $_ < 2**30 { ($bytes / 2**20).round(.1) ~ ' MB' }
        default         { ($bytes / 2**30).round(.1) ~ ' GB' }
    }
}
```

Passing in command line switches: --minsize=0 --recurse=False /home/me/p6

```txt
     0 B : /home/me/p6/vor.ppm, /home/me/p6/ns.txt
   190 B : /home/me/p6/scrub(copy).t, /home/me/p6/scrub.t
  1.3 KB : /home/me/p6/coco.p6, /home/me/p6/coc.p6
 80.5 KB : /home/me/p6/temp.txt, /home/me/p6/temp.html
279.6 KB : /home/me/p6/pentaflake.svg, /home/me/p6/5nflake.svg
```



## Mathematica



```Mathematica
hash="SHA256";
minSize=Quantity[1,"Megabytes"];
allfiles=Once@Select[FileNames["*","",∞],!Once@DirectoryQ[#]&&Once@FileSize[#]>minSize&];
data={#,Once[FileHash[#,hash,All,"HexString"]]}&/@allfiles[[;;5]];
Grid[Select[GatherBy[data,Last],Length[#]>1&][[All,All,1]]]
```

sample directory:

```txt
someFile	eebe4df6d2951e77973b83af039f6565b215f74113028bbc5d8f96b856947abe
someFile2	3e6be6db0858c18573af3fde8308fa9759209079e2e372e21ebd6d3c8512d09e
someFile3	bef0039c33277f743b60b0076871110b96e14de34045aafc8e764349de6043b5
directory\someFile	eebe4df6d2951e77973b83af039f6565b215f74113028bbc5d8f96b856947abe
directory\someFile4	e6385b50ec8b052b141588573f680261db714babe534d8ced8a17985b14f58e9

```

sample output:

```txt
35 MB	{someFile,directory\someFile}
```


## Phix

Works on Windows and Linux. No handling of hard (or soft) links.

```Phix
integer min_size=1
sequence res = {}
atom t1 = time()+1

function store_res(string filepath, sequence dir_entry)
    if not match("backup",filepath) -- (example filter)
    and not find('d', dir_entry[D_ATTRIBUTES]) then
        atom size = dir_entry[D_SIZE]
        if size>=min_size then
            res = append(res,{size,filepath,dir_entry})
            if time()>t1 then
                printf(1,"%d files found\r",length(res))
                t1 = time()+1
            end if
        end if
    end if
    return 0 -- keep going
end function
integer exit_code = walk_dir("demo\\clocks\\love", routine_id("store_res"), true)

res = sort(res,DESCENDING)
printf(1,"%d files found\n",length(res))

integer duplicates = 0
for i=1 to length(res)-1 do
    for j=i+1 to length(res) do
        if res[i][1]!=res[j][1] then exit end if
        string si = join_path({res[i][2],res[i][3][D_NAME]}),
               sj = join_path({res[j][2],res[j][3][D_NAME]})
        integer fni = open(si,"rb"),
                fnj = open(sj,"rb"),
                size = res[i][1]
        bool same = true
        if fni=-1 or fnj=-1 then ?9/0 end if
        for k=1 to size+1 do    -- (check eof as well)
            if getc(fni)!=getc(fnj) then
                same = false
                exit
            end if
        end for
        close(fni)
        close(fnj)
        if same then
            -- prettifying the output left as an exercise...
            ?res[i]
            ?res[j]
            duplicates += 1
        end if
    end for
    if time()>t1 then
        printf(1,"processing %d/%d...\r",{i,length(res)})
        t1 = time()+1
    end if
end for
printf(1,"%d duplicates found\n",duplicates)
```

```txt

136 files found
{2996224,"demo\\clocks\\love\\love-0.9.1-win32",{"love.dll","",2996224,2014,4,1,19,54,33}}
{2996224,"demo\\clocks\\love\\Chemical Me",{"love.dll","a",2996224,2014,4,1,19,54,32}}
{1059840,"demo\\clocks\\love\\love-0.9.1-win32",{"DevIL.dll","",1059840,2014,4,1,19,53,31}}
{1059840,"demo\\clocks\\love\\Chemical Me",{"DevIL.dll","a",1059840,2014,4,1,19,53,30}}
{875472,"demo\\clocks\\love\\love-0.9.1-win32",{"msvcr110.dll","",875472,2012,11,6,0,20,52}}
{875472,"demo\\clocks\\love\\Chemical Me",{"msvcr110.dll","a",875472,2012,11,6,0,20,52}}
{774656,"demo\\clocks\\love\\love-0.9.1-win32",{"SDL2.dll","",774656,2014,4,1,19,53,36}}
{774656,"demo\\clocks\\love\\Chemical Me",{"SDL2.dll","a",774656,2014,4,1,19,53,36}}
{535008,"demo\\clocks\\love\\love-0.9.1-win32",{"msvcp110.dll","",535008,2012,11,6,0,20,52}}
{535008,"demo\\clocks\\love\\Chemical Me",{"msvcp110.dll","a",535008,2012,11,6,0,20,52}}
{349184,"demo\\clocks\\love\\love-0.9.1-win32",{"OpenAL32.dll","",349184,2014,4,1,19,53,33}}
{349184,"demo\\clocks\\love\\Chemical Me",{"OpenAL32.dll","a",349184,2014,4,1,19,53,32}}
{347648,"demo\\clocks\\love\\love-0.9.1-win32",{"lua51.dll","",347648,2014,4,1,19,53,49}}
{347648,"demo\\clocks\\love\\Chemical Me",{"lua51.dll","a",347648,2014,4,1,19,53,48}}
{139264,"demo\\clocks\\love\\love-0.9.1-win32",{"mpg123.dll","",139264,2014,4,1,19,53,52}}
{139264,"demo\\clocks\\love\\Chemical Me",{"mpg123.dll","a",139264,2014,4,1,19,53,52}}
8 duplicates found

```



## Python



```python
from __future__ import print_function
import os
import hashlib
import datetime

def FindDuplicateFiles(pth, minSize = 0, hashName = "md5"):
	knownFiles = {}

	#Analyse files
	for root, dirs, files in os.walk(pth):
		for fina in files:
			fullFina = os.path.join(root, fina)
			isSymLink = os.path.islink(fullFina)
			if isSymLink:
				continue # Skip symlinks
			si = os.path.getsize(fullFina)
			if si < minSize:
				continue
			if si not in knownFiles:
				knownFiles[si] = {}
			h = hashlib.new(hashName)
			h.update(open(fullFina, "rb").read())
			hashed = h.digest()
			if hashed in knownFiles[si]:
				fileRec = knownFiles[si][hashed]
				fileRec.append(fullFina)
			else:
				knownFiles[si][hashed] = [fullFina]

	#Print result
	sizeList = list(knownFiles.keys())
	sizeList.sort(reverse=True)
	for si in sizeList:
		filesAtThisSize = knownFiles[si]
		for hashVal in filesAtThisSize:
			if len(filesAtThisSize[hashVal]) < 2:
				continue
			fullFinaLi = filesAtThisSize[hashVal]
			print ("
### ====Duplicate====
")
			for fullFina in fullFinaLi:
				st = os.stat(fullFina)
				isHardLink = st.st_nlink > 1 
				infoStr = []
				if isHardLink:
					infoStr.append("(Hard linked)")
				fmtModTime = datetime.datetime.utcfromtimestamp(st.st_mtime).strftime('%Y-%m-%dT%H:%M:%SZ')
				print (fmtModTime, si, os.path.relpath(fullFina, pth), " ".join(infoStr))

if __name__=="__main__":

	FindDuplicateFiles('/home/tim/Dropbox', 1024*1024)

```



## Racket


```racket

#lang racket

(struct F (name id size [links #:mutable]))

(require openssl/sha1)
(define (find-duplicate-files path size)
  (define Fs
    (sort
     (fold-files
      (λ(path type acc)
        (define s (and (eq? 'file type) (file-size path)))
        (define i (and s (<= size s) (file-or-directory-identity path)))
        (define ln (and i (findf (λ(x) (equal? i (F-id x))) acc)))
        (when ln (set-F-links! ln (cons (path->string path) (F-links ln))))
        (if (and i (not ln)) (cons (F path i s '()) acc) acc))
      '() path #f)
     > #:key F-size))
  (define (find-duplicates Fs)
    (define t (make-hash))
    (for ([F Fs])
      (define cksum (call-with-input-file (F-name F) sha1))
      (hash-set! t cksum (cons F (hash-ref t cksum '()))))
    (for/list ([(n Fs) (in-hash t)] #:unless (null? (cdr Fs))) Fs))
  (let loop ([Fs Fs])
    (if (null? Fs) '()
        (let-values ([(Fs Rs)
                      (splitf-at Fs (λ(F) (= (F-size F) (F-size (car Fs)))))])
          (append (find-duplicates Fs)
                  (loop Rs))))))

(define (show-duplicates path size)
  (for ([Fs (find-duplicate-files path size)])
    (define (links F)
      (if (null? (F-links F)) ""
          (format " also linked at ~a" (string-join (F-links F) ", "))))
    (printf "~a (~a)~a\n" (F-name (car Fs)) (F-size (car Fs)) (links (car Fs)))
    (for ([F (cdr Fs)]) (printf "  ~a~a\n" (F-name F) (links F)))))

(show-duplicates (find-system-path 'home-dir) 1024)

```



## REXX


### bare bones version
 
This REXX version works with DOS   (with or without Microsoft Windows).

Note that the   '''tFID'''   (temp)   file is hard coded to the   '''C:'''   drive.

Only minimal error checking is performed.

```rexx
/*REXX program to reads a (DOS) directory  and  finds and displays files that identical.*/
sep=center(' files are identical in size and content: ',79,"═")    /*define the header. */
tFID= 'c:\TEMP\FINDDUP.TMP'                      /*use this as a temporary  FileID.     */
arg maxSize aDir                                 /*obtain optional arguments from the CL*/
if maxSize='' | maxSize="," then maxSize=1000000 /*filesize limit (in bytes) [1 million]*/
aDir=strip(aDir)                                 /*remove any leading or trailing blanks*/
if right(aDir,1)\=='\'  then aDir=aDir"\"        /*possibly add a trailing backslash [\]*/
"DIR"  aDir  '/a-d-s-h /oS /s | FIND "/" >' tFID /*the (DOS) DIR output ───► temp file. */
pFN=                                             /*the previous  filename and filesize. */
pSZ=;  do j=0  while lines(tFID)\==0             /*process each of the files in the list*/
       aLine=linein(tFID)                        /*obtain (DOS) DIR's output about a FID*/
       parse var aLine . . sz fn                 /*obtain the filesize and its fileID.  */
       sz=space(translate(sz,,','),0)            /*elide any commas from the size number*/
       if sz>maxSize  then leave                 /*Is the file > maximum?  Ignore file. */
                                                 /* [↓]  files identical?  (1st million)*/
       if sz==pSZ  then  if charin(aDir||pFN,1,sz)==charin(aDir||FN,1,sz)  then do
                                                                                say sep
                                                                                say pLine
                                                                                say aLine
                                                                                say
                                                                                end
       pSZ=sz;      pFN=FN;      pLine=aLine     /*remember the previous stuff for later*/
       end   /*j*/

if lines(tFID)\==0  then 'ERASE' tFID            /*do housecleaning  (delete temp file).*/
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''   when using (checking) with the default root directory:

```txt

══════════════════ files are identical in size and content: ═══════════════════
04/13/2013  19:13                76 another.BK
04/13/2013  19:13                76 another.A

══════════════════ files are identical in size and content: ═══════════════════
04/13/2013  17:15               244 gettfid.1
04/13/2013  17:15               244 junk.1

══════════════════ files are identical in size and content: ═══════════════════
03/03/1995  01:46            10,897 $ERR.BK
03/03/1995  01:46            10,897 $ERR.ORI

```



### with error checking

This version of the REXX program:
::*   checks to see if running under the   '''DOS'''   environment
::*   uses the   '''TEMP'''   folder for storing a temporary file 
::*   verifies that the   '''maxSize'''   is a positive integer
::*   adjusts the name for a generic file specification
::*   uses variables for some command names and command options
::*   shows the number of files examined and also the directory name

```rexx
/*REXX program to reads a (DOS) directory  and  finds and displays files that identical.*/
sep=center(' files are identical in size and content: ',79,"═")    /*define the header. */
parse arg !;     if !all(arg())  then exit                         /*boilerplate HELP(?)*/
signal on halt;  signal on novalue;  signal on syntax              /*handle exceptions, */

if \!dos  then call err 'this program requires the DOS [environment].'
call getTFID                                     /*defines a temporary  File ID for DOS.*/
arg maxSize aDir                                 /*obtain optional arguments from the CL*/
if maxSize='' | maxSize="," then maxSize=1000000 /*filesize limit (in bytes) [1 million]*/
if \isInt(maxSize)      then call err  "maxSize isn't an integer:"       maxSize
if maxSize<0            then call err  "maxSize can't be negative:"      maxSize
if maxSize=0            then call err  "maxSize can't be zero:"          maxSize
aDir=strip(aDir)                                 /*remove any leading or trailing blanks*/
if right(aDir,3)=='*.*' then aDir=substr(aDir,1,length(aDir)-3)   /*adjust the dir name.*/
if right(aDir,1)\=='\'  then aDir=aDir"\"        /*possibly add a trailing backslash [\]*/
@dir    = 'DIR'                                  /*literal for the (DOS)  DIR  command. */
@dirNots= '/a-d-s-h'                             /*ignore DIRs, SYSTEM, and HIDDEN files*/
@dirOpts= '/oS /s'                               /*sort DIR's (+ subdirs) files by size.*/
@filter = '| FIND "/"'                           /*the "lines" must have a slash [/].   */
@erase  = 'ERASE'                                /*literal for the (DOS)  ERASE command.*/
@dir aDir @dirNots @dirOpts @filter '>' tFID     /*(DOS) DIR  output ──► temporary file.*/
pFN=                                             /*the previous  filename and filesize. */
pSZ=;  do j=0  while lines(tFID)\==0             /*process each of the files in the list*/
       aLine=linein(tFID)                        /*obtain (DOS) DIR's output about a FID*/
       parse var aLine . . sz fn                 /*obtain the filesize and its fileID.  */
       sz=space(translate(sz,,','),0)            /*elide any commas from the size number*/
       if sz>maxSize  then leave                 /*Is the file > maximum?  Ignore file. */
                                                 /* [↓]  files identical?  (1st million)*/
       if sz==pSZ  then  if charin(aDir||pFN,1,sz)==charin(aDir||FN,1,sz)  then do
                                                                                say sep
                                                                                say pLine
                                                                                say aLine
                                                                                say
                                                                                end
       pSZ=sz;      pFN=FN;      pLine=aLine     /*remember the previous stuff for later*/
       end   /*j*/

say j  'file's(j)  "examined in"  aDir           /*show information to the screen.*/
if lines(tFID)\==0  then 'ERASE'  tFID           /*do housecleaning  (delete temp file).*/
exit                                             /*stick a fork in it,  we're all done. */
/*═════════════════════════════general 1─line subs══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
!all:  !!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=="NT";!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,"? ?SAMPLES ?AUTHOR ?FLOW")==0 then return 0;!call=']$H';call "$H" !fn !;!call=;return 1
!cal:  if symbol('!CALL')\=="VAR" then !call=; return !call
!env:  !env='ENVIRONMENT'; if !sys=="MSDOS" | !brexx | !r4 | !roo  then !env='SYSTEM'; if !os2  then !env="OS2"!env; !ebcdic=1=='f1'x;     return
!fid:  parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .; call !sys; if !dos  then do; _=lastpos('\',!fn); !fm=left(!fn,_); !fn=substr(!fn,_+1); parse var !fn !fn "." !ft; end;     return word(0 !fn !ft !fm, 1+('0'arg(1)))
!rex:  parse upper version !ver !vernum !verdate .; !brexx='BY'==!vernum; !kexx="KEXX"==!ver; !pcrexx='REXX/PERSONAL'==!ver | "REXX/PC"==!ver; !r4='REXX-R4'==!ver; !regina="REXX-REGINA"==left(!ver,11); !roo='REXX-ROO'==!ver; call !env;   return
!sys:  !cms=!sys=='CMS'; !os2=!sys=="OS2"; !tso=!sys=='TSO' | !sys=="MVS"; !vse=!sys=='VSE'; !dos=pos("DOS",!sys)\==0|pos('WIN',!sys)\==0|!sys=="CMD"; call !rex;                          return
!var:  call !fid; if !kexx  then return space(dosenv(arg(1)));             return space(value(arg(1),,!env))
err:       say;  say;  say  center(' error! ', 60, "*");  say;  do j=1  for arg();  say arg(j);  say;  end;  say;  exit 13
getdTFID:  tfid=p(!var("TMP") !var('TEMP') homedrive()"\"); if substr(tfid,2,1)==':'&substr(tfid,3,1)\=="\" then tfid=insert('\',t,2);        return strip(tfid,"T",'\')"\"arg(1)'.'arg(2)
getTFID:   if symbol('TFID')=="LIT" then tfid=; if tfid\=='' then return tfid; gfn=word(arg(1) !fn,1);gft=word(arg(2) "TMP",1); tfid='TEMP';if !tso  then tfid=gfn"."gft;if !cms  then tfid=gfn','gft",A4";if !dos then tfid=getdTFID(gfn,gft);return tfid
halt:      call err 'program has been halted.'
homedrive: if symbol('HOMEDRIVE')\=="VAR"  then homedrive=p(!var('HOMEDRIVE') "C:");   return homedrive
isint:     return datatype(arg(1),'W')
novalue:   syntax:   call err 'REXX program' condition("C") 'error',condition("D"),'REXX source statement (line' sigl"):",sourceline(sigl)
p:         return word(arg(1),1)
s:         if arg(1)==1  then return arg(3);   return word(arg(2) 's',1)
```

'''output'''   when using the DIR (folder):   H:\#\REX

```txt

══════════════════ files are identical in size and content: ═══════════════════
05/11/2015  18:49               838 UPDATECF.BU
05/11/2015  18:49               838 UPDATECF.TXT

══════════════════ files are identical in size and content: ═══════════════════
03/23/2014  21:55             2,736 EMIRP.RX_
03/26/2014  10:44             2,736 EMIRP2.RX_

══════════════════ files are identical in size and content: ═══════════════════
05/30/2015  17:30             4,542 JUSTIFY.RX_
11/25/2013  06:33             4,542 JUSTIFY.KX_

══════════════════ files are identical in size and content: ═══════════════════
06/15/2014  23:36            13,935 $BLOCK.KX_
05/30/2015  17:28            13,935 $BLOCK.RX_

1568 files examined in H:\#\REX\

```



## Ring


```ring

# Project : Find duplicate files

d = "/Windows/System32"
chdir(d)
dir = dir(d)
dirlist = []
for n = 1 to len(dir)
     if dir[n][2] = 0
        str = read(dir[n][1])
        lenstr = len(str)
        add(dirlist,[lenstr,dir[n][1]])
     ok
next
see "Directory : " + d + nl
see "--------------------------------------------" + nl
dirlist = sortfirst(dirlist)
line = 0
for n = 1 to len(dirlist)-1
     if dirlist[n][1] = dirlist[n+1][1]
        see "" + dirlist[n][1] + " " + dirlist[n][2] + nl
        see "" + dirlist[n+1][1] + " " + dirlist[n+1][2] + nl
        if n < len(dirlist)-2 and dirlist[n+1][1] != dirlist[n+2][1]
           line = 1
        ok
     else
        line = 0
     ok
     if line = 1
        see "--------------------------------------------" + nl
     ok
next

func sortfirst(alist)
        for n = 1 to len(alist) - 1
             for m = n + 1 to len(alist)
                  if alist[m][1] < alist[n][1]
                     swap(alist,m,n)
                  ok
                  if alist[m][1] = alist[n][1] and strcmp(alist[m][2],alist[n][2]) < 0
                     swap(alist,m,n)
                  ok
             next
        next
        return alist

```

Output:

```txt

Directory : /Windows/System32
--------------------------------------------
0 nsprs.dll
0 nsprs.tgz
0 nsprs.tgz
0 serauth1.dll
0 serauth1.dll
0 serauth2.dll
--------------------------------------------
16 jm1ixs2.dll
16 qmtn7ft.dll
--------------------------------------------
......
--------------------------------------------
1189376 Windows.Globalization.dll
1189376 wscui.cpl
--------------------------------------------
1192448 Windows.UI.Xaml.Maps.dll
1192448 dfshim.dll
--------------------------------------------
1295360 MSVPXENC.dll
1295360 comres.dll
--------------------------------------------
1311744 SensorsCpl.dll
1311744 msjet40.dll
--------------------------------------------

```



## Ruby

It confirms once by the file size. When the same, it confirms a digest (md5).

```ruby
require 'digest/md5'

def find_duplicate_files(dir)
  puts "\nDirectory : #{dir}"
  Dir.chdir(dir) do
    file_size = Dir.foreach('.').select{|f| FileTest.file?(f)}.group_by{|f| File.size(f)}
    file_size.each do |size, files|
      next if files.size==1
      files.group_by{|f| Digest::MD5.file(f).to_s}.each do |md5,fs|
        next if fs.size==1
        puts "  --------------------------------------------"
        fs.each{|file| puts "  #{File.mtime(file)}  #{size}  #{file}"}
      end
    end
  end
end

find_duplicate_files("/Windows/System32")
```


Sample Output:

```txt

Directory : /Windows/System32
  --------------------------------------------
  2016-02-09 18:56:09 +0900  5120  dxmasf.dll
  2016-02-09 18:56:09 +0900  5120  msdxm.ocx
  --------------------------------------------
  2015-11-14 08:09:16 +0900  91648  mapi32.dll
  2015-11-14 08:09:16 +0900  91648  mapistub.dll
  --------------------------------------------
  2015-11-05 20:34:06 +0900  18592  msvcp110_clr0400.dll
  2015-11-05 20:34:06 +0900  18592  msvcr100_clr0400.dll
  2015-11-05 20:34:06 +0900  18592  msvcr110_clr0400.dll
  --------------------------------------------
  2009-07-14 10:00:32 +0900  31548  perfd009.dat
  2010-11-21 16:14:04 +0900  31548  perfd011.dat

```

It checked the operation with MS Windows 7.


## Sidef

It uses the portable ''File::Find'' module which means that it should work, virtually, on any platform.

```ruby
# usage: sidef fdf.sf [size] [dir1] [...]

require('File::Find')

func find_duplicate_files(Block code, size_min=0, *dirs) {
    var files = Hash()
    %S<File::Find>.find(
        Hash(
            no_chdir => true,
            wanted   => func(arg) {
                var file = File(arg)
                file.is_file || return()
                file.is_link && return()
                var size = file.size
                size >= size_min || return()
                files{size} := [] << file
            },
        ) => dirs...
    )

    files.values.each { |set|
        set.len > 1 || next
        var dups = Hash()
        for i in (^set.end) {
            for (var j = set.end; j > i; --j) {
                if (set[i].compare(set[j]) == 0) {
                    dups{set[i]} := [] << set.pop_at(j++)
                }
            }
        }
        dups.each{ |k,v| code(k.to_file, v...) }
    }

    return()
}

var duplicates = Hash()
func collect(*files) {
    duplicates{files[0].size} := [] << files
}

find_duplicate_files(collect, Num(ARGV.shift), ARGV...)

for k,v in (duplicates.sort_by { |k| -k.to_i }) {
    say "=> Size: #{k}\n#{'~'*80}"
    for files in v {
        say "#{files.sort.join(%Q[\n])}\n#{'-'*80}"
    }
}
```

Section of sample output:

```txt
% sidef fdf.sf 0 /tmp /usr/bin
=> Size: 5656
~~~~~~~~~~~~~~~~~~~~~~~~~~
/usr/bin/precat
/usr/bin/preunzip
/usr/bin/prezip
--------------------------
=> Size: 2305
~~~~~~~~~~~~~~~~~~~~~~~~~~
/usr/bin/gunzip
/usr/bin/uncompress
--------------------------
=> Size: 2
~~~~~~~~~~~~~~~~~~~~~~~~~~
/tmp/a.txt
/tmp/b.txt
--------------------------
/tmp/m.txt
/tmp/n.txt
--------------------------
```



## Tcl

Only known to work on Unix. Uses both inode number checking and content hashing to do duplicate detection.
```tcl
package require fileutil
package require md5

proc finddupfiles {dir {minsize 1}} {
    foreach fn [fileutil::find $dir] {
	file lstat $fn stat
	if {$stat(size) < $minsize} continue
	dict lappend byino $stat(dev),$stat(ino) $fn
	if {$stat(type) ne "file"} continue
	set f [open $fn "rb"]
	set content [read $f]
	close $f
	set md5 [md5::md5 -hex $content]
	dict lappend byhash $md5 $fn
    }
    set groups {}
    foreach group [dict values $byino] {
	if {[llength $group] <= 1} continue
	set gs [lsort $group]
	dict set groups [lindex $gs 0] $gs
    }
    foreach group [dict values $byhash] {
	if {[llength $group] <= 1} continue
	foreach f $group {
	    if {[dict exists $groups $f]} {
		dict set groups $f [lsort -unique \
			[concat [dict get $groups $f] $group]]
		unset group
		break
	    }
	}
	if {[info exist group]} {
	    set gs [lsort $group]
	    dict set groups [lindex $gs 0] $gs
	}
    }
    set masters {}
    dict for {n g} $groups {
	lappend masters [list $n [llength $g],$n]
    }
    set result {}
    foreach p [lsort -decreasing -index 1 -dictionary $masters] {
	set n [lindex $p 0]
	lappend result $n [dict get $groups $n]
    }
    return $result
}

foreach {leader dupes} [finddupfiles {*}$argv] {
    puts "$leader has duplicates"
    set n 0
    foreach d $dupes {
	if {$d ne $leader} {
	    puts "   dupe #[incr n]: $d"
	}
    }
}
```

Section of sample output:

```txt

./compat/zlib/zconf.h has duplicates
   dupe #1: ./compat/zlib/zconf.h.in
./compat/zlib/contrib/vstudio/vc10/zlib.rc has duplicates
   dupe #1: ./compat/zlib/contrib/vstudio/vc9/zlib.rc
./compat/zlib/contrib/delphi/zlibd32.mak has duplicates
   dupe #1: ./compat/zlib/contrib/pascal/zlibd32.mak

```



## zkl

Uses the MsgHash dll so I don't have to read the entire file to hash it (the built in MD5 only hashes one blob, MshHash can hash a chunked blob).

I tried threading this but, even though it was over twice as fast, not really worth it.

File findDupFiles.zkl:

```zkl
include(zkl.h.zkl);
const FLAGS=FILE.GLOB.IGNORE_CASE + FILE.GLOB.NO_DIRS;
var [const] MsgHash=Import("zklMsgHash");
var recurse=False, fileSpec, minSz=0, maxSz=(0).MAX;

argh:=Utils.Argh(
   T("+R","R","Recurse into subdirectories, starting at <arg>",
	fcn(arg){ recurse=arg }),
   T("+minSz","","Only consider files larger than <arg>",
	fcn(arg){ minSz=arg.toInt() }),
   T("+maxSz","","Only consider files less than <arg>",
	fcn(arg){ maxSz=arg.toInt() }),
);

argh.parse(vm.arglist);
try { fileSpec=argh.loners[0]; }
catch{
   argh.usage("Find duplicate files");
   System.exit(1);
}

fnames:=Data(0,String);
if (recurse) File.globular(recurse,fileSpec,True,FLAGS,fnames);
else	     File.glob(fileSpec,FLAGS).pump(fnames);

files:=Dictionary();  // (len:(name,name...), ...)
foreach fname in (fnames){
   sz:=File.len(fname);
   if(minSz<=sz<=maxSz) files.appendV(File.len(fname),fname);
}

    //////////////////////// group files by size
files=files.pump(List,Void.Xplode,fcn(k,v){ v.len()>1 and v or Void.Skip });
println("Found %d groups of same sized files, %d files total.".fmt(files.len(),
   files.apply("len").sum(0)));

if(not files) System.exit();	// no files found

buffer:=Data(0d100_000);  // we'll resuse this buffer for hashing
hashes:=files.pump(List,'wrap(fnames){ // get the MD5 hash for each file
   fnames.pump(List,'wrap(fname){
      file,hash := File(fname,"rb"), MsgHash.toSink("MD5");
      file.pump(buffer,hash); file.close();
      return(hash.close(),fname); // -->( (hash,name), (hash,name) ... )
   })
},T(Void.Write,Void.Write)); // flatten list of lists of lists to above

   // Hash the file hashes, then scoop out the files with the same hash
buffer:=Dictionary();
files:=hashes.pump(Void,Void.Xplode,buffer.appendV)
       .pump(List,Void.Xplode,fcn(k,v){ v.len()>1 and v or Void.Skip });
  
println("Found %d duplicate files:".fmt(files.apply("len").sum(0)));
foreach group in (files){ println("   ",group.concat(", ")) }
```

```txt

$ zkl findDupFiles.zkl
Find duplicate files
Options:
  --R (-R) <arg>: Recurse into subdirectories, starting at <arg>
  --maxSz <arg>: Only consider files less than <arg>
  --minSz <arg>: Only consider files larger than <arg>

$ zkl findDupFiles.zkl '*' 
Found 16 groups of same sized files, 34 files total.
Found 8 duplicate files:
   unixdict.txt, dict.txt, uuuu.txt
   zz.zkl, zzDup.zkl
   gooperf.dat, zklTmpFile082p1V, test.dat

```

