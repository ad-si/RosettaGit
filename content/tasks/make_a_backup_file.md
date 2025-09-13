+++
title = "Make a backup file"
description = ""
date = 2019-07-24T04:28:15Z
aliases = []
[extra]
id = 10841
[taxonomies]
categories = ["task"]
tags = []
+++

Before writing to a file it is often advisable to make a backup of the original. 
Creating such a backup file is however also not without pitfalls.

In this task you should create a backup file from an existing file and then write new text to the old file.
The following issues should be handled:
* avoid making a copy of the file but instead rename the original and then write a new file with the original filename.
* if a copy needs to be made, please explain why rename is not possible.
* keep in mind symlinks, and do not rename or copy the link but the target. (If there is a link <code>foo -> bar/baz</code>, then <code>bar/baz</code> should be renamed to <code>bar/baz.backup</code> and then the new text should be written to <code>bar/baz</code>.)
* it is assumed that you have permission to write in the target location, thus permission errors need not be handled.
* you may choose the backup filename per preference or given limitations. (It should somehow include the original filename however.)
* please try to avoid executing external commands, and especially avoid calling a shell script.

Some examples on this page assume that the original file already exists. They might fail if some user is trying to create a new file.


## AutoHotkey


```autohotkey
targetfile := "ahk-file"
if FileExist(targetfile)
	FileMove, %targetfile%, %targetfile%.bak
else
	FileAppend,, %targetfile%
file := FileOpen(targetfile, "w")
if !IsObject(file)
{
	MsgBox Can't open "%FileName%" for writing.
	return
}
file.Write("This is a test string.`r`n")
file.Close()
```



## AWK


```AWK

# syntax: GAWK -f MAKE_A_BACKUP_FILE.AWK filename(s)
# see: http://www.gnu.org/software/gawk/manual/gawk.html#Extension-Sample-Inplace
@load "inplace"
BEGIN {
    INPLACE_SUFFIX = ".BAK"
}
BEGINFILE {
    inplace_begin(FILENAME,INPLACE_SUFFIX)
}
1 # rewrite file unchanged
ENDFILE {
    inplace_end(FILENAME,INPLACE_SUFFIX)
}
END {
    exit(0)
}

```


=={{Header|Batch File}}==

```dos

@echo off
setlocal enabledelayedexpansion

:: Takes file input as param #1
set "filePath=%~1"
set "fileName=%~xn1"

:: Save file contents of original file to array line[n]
set i=0
for /f "usebackq delims=" %%a in ("%filePath%") do (
	set /a i+=1
	set "line[!i!]=%%a"
)

:: Rename original file with .backup extension
ren "%filePath%" "%fileName%.backup"

:: Rewrite a new file with the name of the original
echo !line[1]!>"%filePath%"
for /l %%i in (2,1,%i%) do echo !line[%%i]!>>"%filePath%"

```



## Common Lisp

Appends a version number and increments it on each backup

```lisp
(defun parse-integer-quietly (&rest args)
  (ignore-errors (apply #'parse-integer args)))

(defun get-next-version (basename)
  (flet ((parse-version (pathname)
                        (or (parse-integer-quietly 
                              (string-left-trim (file-namestring basename) 
                                                (file-namestring pathname))
                              :start 1) 0)))
    (let* ((files (directory (format nil "~A,*" (namestring basename))))
           (max (reduce #'max files :key #'parse-version)))
      (merge-pathnames (format nil "~a,~d" (file-namestring basename) (1+ max))
                       basename))))

(defun save-with-backup (filename data)
  (let ((file (probe-file filename)))
    (rename-file file (get-next-version file))
    (with-open-file (out file :direction :output)
      (print data out))))
```



## Elixir


```elixir
defmodule RC do
  def backup_file(filename) do
    backup = filename <> ".backup"
    case File.rename(filename, backup) do
      :ok -> :ok
      {:error, reason} -> raise "rename error: #{reason}"
    end
    File.cp!(backup, filename)
  end
end

hd(System.argv) |> RC.backup_file
```



## Go


### Rename

This is the technique of the task description.

```go
package main

import (
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    fn := "myth"
    bx := ".backup"
    // see if it's a link
    var err error
    if tf, err := os.Readlink(fn); err == nil {
        fn = tf
    }
    // stat to preserve permissions.
    var fi os.FileInfo
    if fi, err = os.Stat(fn); err != nil {
        fmt.Println(err)
        return
    }
    // attemp rename
    if err = os.Rename(fn, fn+bx); err != nil {
        fmt.Println(err)
        return
    }
    // create new file
    err = ioutil.WriteFile(fn, []byte("you too!\n"), fi.Mode().Perm())
    if err != nil {
        fmt.Println(err)
    }
}
```


### Copy

Alternative technique copies an existing file to make the backup copy, then updates the origial file.  In an attempt to keep operations atomic, the original file is opened as the first operation and is not closed until all operations are complete.  In an attempt to avoid data loss, the original file is not modified until the backup file is closed.

```go
package main

import (
    "fmt"
    "io"
    "os"
)

func main() {
    err := updateWithBackup("myth", ".backup", func(f *os.File) (err error) {
        if _, err = f.Seek(0, os.SEEK_SET); err != nil {
            return
        }
        if err = f.Truncate(0); err != nil {
            return
        }
        _, err = f.WriteString("you too!\n")
        return
    })
    if err != nil {
        fmt.Println(err)
    }
}

// updateWithBackup opens fn, creates a backup copy named fn+bx, then passes
// the still-open original file to the supplied function up.  up should
// update the file as needed and return any error, but should not close
// the file.  updateWithBackup will then close the file and return any error.
func updateWithBackup(fn, bx string, up func(*os.File) error) (err error) {
    var f *os.File
    if f, err = openWithBackup(fn, bx); err != nil {
        return
    }
    err = up(f)
    if cErr := f.Close(); err == nil {
        err = cErr
    }
    return
}

// openWithBackup opens fn, creates a backup copy, and returns fn still open.
// If fn is a symlink, the destination file is opened instead.  The name of
// the backup file will be fn+bx, or if fn was a symlink, the name of the
// destination file + bx.  Any error encountered is returned.  tf will be
// an open file if and only if err == nil.
func openWithBackup(fn, bx string) (tf *os.File, err error) {
    // follow symlink.
    if target, err := os.Readlink(fn); err == nil {
        fn = target
    }
    // open the target file for exclusive access.
    if tf, err = os.OpenFile(fn, os.O_RDWR, 0); err != nil {
        return
    }
    // deferred function closes target file if an error happens
    // during the backup operation.
    defer func() {
        if err != nil {
            tf.Close()
        }
    }()
    // stat to preserve permissions.
    var fi os.FileInfo
    if fi, err = tf.Stat(); err != nil {
        return
    }
    // create backup file, silently droping any existing backup.
    var bf *os.File
    if bf, err = os.OpenFile(fn+bx, os.O_RDWR|os.O_CREATE|os.O_TRUNC,
        fi.Mode().Perm()); err != nil {
        return
    }
    // copy contents.  hold on to any error.
    _, err = io.Copy(bf, tf)
    // do your best to close backup file whether copy worked or not.
    if cErr := bf.Close(); err == nil {
        err = cErr // return close error if there has been no other error.
    }
    // backup complete (as long as err == nil)
    return
}
```



## Java

```java5
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.*;
 
public class Backup {
	public static void saveWithBackup(String filename, String... data) 
        throws IOException {
		//toRealPath() follows symlinks to their ends
		Path file = Paths.get(filename).toRealPath();
		File backFile = new File(filename + ".backup");
		if(!backFile.exists()) {
			// ensure the backup file exists so we can write to it later
			backFile.createNewFile();
		}
		Path back = Paths.get(filename + ".backup").toRealPath();
		Files.move(file, back, StandardCopyOption.REPLACE_EXISTING);
		try(PrintWriter out = new PrintWriter(file.toFile())){
			for(int i = 0; i < data.length; i++) {
				out.print(data[i]);
				if(i < data.length - 1) {
					out.println();
				}
			}
		}
	}

        public static void main(String[] args) {
		try {
			saveWithBackup("original.txt", "fourth", "fifth", "sixth");
		} catch (IOException e) {
			System.err.println(e);
		}
	} 
}
```


Contents of 'original.txt' ''before'' the program is run and of 'original.txt.backup' ''after'' it is run:

```txt

first
second
third

```


Contents of 'original.txt' ''after'' the program is run:

```txt

fourth
fifth
sixth

```


```java5
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;

public class Backup{
	public static void saveWithBackup(String filename, String... data)
						throws IOException{
		File orig = new File(filename);
		//getCanonicalPath() follows symlinks to their ends
		File backup = new File(orig.getCanonicalPath() + ".backup");
		
		orig.renameTo(backup);
		PrintWriter output = new PrintWriter(orig);
		for(int i = 0; i < data.length; i++) {
			output.print(data[i]);
			if(i < data.length - 1) {
				output.println();
			}
		}
		output.close();
	}

        public static void main(String[] args) {
		try {
			saveWithBackup("original.txt", "fourth", "fifth", "sixth");
		} catch (IOException e) {
			System.err.println(e);
		}
	} 
}
```


Same as version 7+ example.


## Julia

```julia
targetfile = "pycon-china"
mv(realpath(targetfile), realpath(targetfile) * ".bak")
# "a+" for permissions of reading, writing, creating
open(targetfile, "w+") do io
    println(io, "this task was solved during a talk about rosettacode at the PyCon China in 2011")
end
```



## Kotlin

```scala
// version 1.1.51

import java.io.File

fun saveWithBackup(fileName: String, vararg data: String) {
    val orig = File(fileName)
    // canonicalPath follows symlinks to their ends
    val backup = File(orig.canonicalPath + ".backup")
    orig.renameTo(backup)
    val pw = orig.printWriter()
    for (i in data.indices) {
        pw.print(data[i])
        if (i < data.lastIndex) pw.println()
    }
    pw.close()
}

fun main(args: Array<String>) {
    saveWithBackup("original.txt", "fourth", "fifth", "sixth")
}
```


Contents of 'original.txt' ''before'' the program is run and of 'original.txt.backup' ''after'' it is run:

```txt

first
second
third

```


Contents of 'original.txt' ''after'' the program is run:

```txt

fourth
fifth
sixth

```



## Lasso


```Lasso
local(file2use = 'input.txt')

// create file
// only need to do this if this code example has not been run before and input.txt foes not exist, else leave commented out
//local(nf = file(#file2use))
//#nf->doWithClose => { #nf->writeBytes('Hello, World!'->asBytes) }


// move file
local(f = file(#file2use))
local(contents_of_f = #f->readstring)
#f->moveTo(#file2use+'.bak')
#f->close

// make mods to file contents
#contents_of_f->append('\rLasso is awesome!')

// create new file with new contents
local(nf = file(#file2use))
#nf->doWithClose => { #nf->writeBytes(#contents_of_f->asBytes) }
```



## Locomotive Basic


AMSDOS has automatic one-level backups which also work from Locomotive BASIC: If e.g. the file <tt>test.bas</tt> is saved, the data gets written to <tt>test.$$$</tt>. When the file is closed a preexisting <tt>test.bas</tt> gets renamed to <tt>test.bak</tt> and finally <tt>test.$$$</tt> is renamed to <tt>test.bas</tt>. (These backups affect all file types, not just BASIC source code.)


## Perl


```perl
use strict;
use warnings;

sub backup {
    my($filepath,$limit,$ext) = @_;
    my $abs = readlink $filepath // $filepath; # always resolve symlinks
    for my $bnum (reverse 1 .. $limit-1) {
        rename "$abs$ext$bnum", "$abs$ext" . ++$bnum if -e "$abs$ext$bnum";
    }

    if (-e $abs) {
        if ($limit > 0) {
            my $orig = $abs . $ext . '1';
            rename $abs, $orig;
            open(IN,  '<', $orig) or die "can't open $orig: $!";
            open(OUT, '>', $abs)  or die "can't open $abs: $!";

            my $blksize = (stat IN)[11] || 2**14;          # preferred block size?
            my $buf;
            while (my $len = sysread IN, $buf, $blksize) {
                die "System read error: $!\n" if !defined $len;
                my $offset = 0;
                while ($len) {          # Handle partial writes.
                    defined(my $written = syswrite OUT, $buf, $len, $offset)
                        or die "System write error: $!\n";
                    $len    -= $written;
                    $offset += $written;
                };
            }
            close(IN);
            close(OUT);
        }
    } else {
        warn "File not found: $abs" and return 0;
    }
    $abs
}

# back up this program
backup($0,3,'.bk');
```



## Perl 6

```perl6
# Back up the given path/filename with a default extension .bk(n)
# where n is in the range 1 - $limit (default 3).
# Prints 'File not found' to STDERR if the file does not exist.
# Will not do anything if limit is set to less than 1.
# Will not follow symlinks by default.

sub backup (Str $filepath, Int :$limit = 3, Str :$ext = 'bk', Bool :$follow-symlinks = False) {
    my $abs = $follow-symlinks ?? $filepath.IO.resolve.absolute !! $filepath.IO.absolute;
    for (1 ..^ $limit).reverse -> $bnum {
        if "{$abs}.{$ext}{$bnum}".IO.e {
            "{$abs}.{$ext}{$bnum}".IO.rename: "{$abs}.{$ext}{$bnum + 1}";
        }
    }
    if $abs.IO.e {
        if $limit > 0 {
            $abs.IO.rename: "{$abs}.{$ext}1";
            my $in  = "{$abs}.{$ext}1".IO.open :r :bin or note $! and return False;
            my $out = $abs.IO.open :w :bin or note $! and return False;
            my $buffer-size = 32768; # 32Kb
            while my $buf = $in.read($buffer-size) { $out.write($buf) };
            close $in;
            close $out;
        }
    } else {
        note "File not found: $abs" and return False;
    }
    $abs # return filepath on success
}

# back up this program
backup $*PROGRAM-NAME;

# Optionally, specify limit, back-up extension pattern and whether to follow symlinks.
# Optional parameters can be in any order, in any combination.
backup 'myfile', :follow-symlinks, :limit(2), :ext('bak');
```



## Phix

```Phix
targetfile = get_proper_path("test.txt")
if not rename_file(targetfile, targetfile&".bak", overwrite:=true) then
    puts(1,"warning: could not rename file\n")
end if
integer fn = open(targetfile,"w")
if fn=-1 then
    puts(1,"error: cannot open file for writing\n")
else
    puts(fn,"this task was translated from the Python entry\n")
    close(fn)
end if
```

Before basing anything on the above code, though, I would recommend you take a look at 

function saveFile in demo\edix\edix.exw, which does this sort of thing for real: 

test.txt -> test.0001.txt, test.0002.txt, etc, in subdirectories \backups, \backups.0001, \backups.0002, etc.


## PicoLisp

PicoLisp makes use of external commands as much as possible (at least for not time-critical operations), to avoid duplicated functionality.

```PicoLisp
(let Path (in '(realpath "foo") (line T))
   (call 'mv Path (pack Path ".backup"))
   (out Path
      (prinl "This is the new file") ) )
```



## Pike


```Pike
string targetfile = "pycon-china";
targetfile = System.resolvepath(targetfile);
mv(targetfile, targetfile+"~");
Stdio.write_file(targetfile, "this task was solved at the pycon china 2011");
```



## Python

Using [https://docs.python.org/library/os.html os library]

```Python

import os
targetfile = "pycon-china"
os.rename(os.path.realpath(targetfile), os.path.realpath(targetfile)+".bak")
f = open(os.path.realpath(targetfile), "w")
f.write("this task was solved during a talk about rosettacode at the PyCon China in 2011")
f.close()

```

Or using a newer [https://docs.python.org/library/pathlib.html pathlib library] (Python >= 3.4):

```Python

from pathlib import Path

filepath = Path("original_file")
filepath.rename(filepath.with_suffix('.bak'))
with filepath.open('w') as file:
    file.write("New content")

```



## Racket


This version keeps unlimited backups, with <tt>*.bak</tt> being the freshest one, <tt>*.bak1</tt> is an older backup, etc.  So each backup moves all existing names up.


```Racket

#lang racket

(define (move-to-backup file)
  (define backup
    (regexp-replace #rx"^(.*?)(?:\\.bak([0-9]*))?$" file
      (λ (_ base num) (~a base ".bak" (add1 (if num (string->number num) 0))))))
  (eprintf "~s -> ~s\n" file backup)
  (when (file-exists? backup) (move-to-backup backup))
  (rename-file-or-directory file backup)
  backup)

(define (revise path0)
  (define path (path->string (normalize-path path0))) ; chase symlinks
  (when (file-exists? path) (copy-file (move-to-backup path) path))
  (display-to-file "Another line\n" path #:exists 'append))

(revise "fff")

```



## REXX

This REXX version executes under DOS or DOS under Windows.

```rexx
/*REXX program creates a backup file  (for a given file),  then overwrites the old file.*/
parse arg oFID .                                 /*get a required argument from the C.L.*/
if oFID==''  then do                             /*No argument?   Then issue an err msg.*/
                  say '***error*** no fileID was specified.'
                  exit 13
                  end
tFID= oFID'.$$$'                                 /*create temporary name for the backup.*/
call lineout oFID                                /*close the file  (in case it's open). */
call lineout tFID                                /*  "    "    "     "   "    "    "    */
'ERASE'  tFID                                    /*delete the backup file (if it exists)*/
'RENAME' oFID tFID                               /*rename the original file to backup.  */
call lineout oFID, '═══This is line 1.'          /*write one line to the original file. */
                                                 /*stick a fork in it,  we're all done. */
```

The contents of the original file (before execution):   '''A.FILE''':

```txt

     AA                     FFFFFFFFFFFF     IIIIII     LLLL          EEEEEEEEEEEE
    AAAA                    FFFFFFFFFFFF     IIIIII     LLLL          EEEEEEEEEEEE
   AA  AA                   FF        FF       II        LL           EE        EE
  AA    AA                  FF                 II        LL           EE
 AA      AA                 FF   FF            II        LL           EE   EE
 AA      AA                 FFFFFFF            II        LL           EEEEEEE
 AAAAAAAAAA                 FFFFFFF            II        LL           EEEEEEE
 AAAAAAAAAA                 FF   FF            II        LL     LLLL  EE   EE
 AA      AA                 FF                 II        LL     LLLL  EE
 AA      AA                 FF                 II        LL      LL   EE        EE
AAAA    AAAA       ..       FF               IIIIII      LLLLLLLLLL   EEEEEEEEEEEE
AAAA    AAAA       ..       FF               IIIIII      LLLLLLLLLL   EEEEEEEEEEEE

```

The contents of the overwritten file (after execution):   '''A.FILE''':

```txt

═══This is line 1.

```

The backup file is called:   '''A.FILE.$$$'''





## Ruby

This version does not overwrite the backup file if it exists.

```ruby
def backup_and_open(filename)
  filename = File.realpath(filename)
  bkup = filename + ".backup"
  backup_files = Dir.glob(bkup + "*").sort_by do |f|
    f.match(/\d+$/)
    $&.nil? ? 0 : $&.to_i
  end
  backup_files.reverse.each do |fname|
    if m = fname.match(/\.backup\.(\d+)$/)
      File.rename(fname, "%s.%d" % [bkup, m[1].to_i + 1])
    elsif fname == bkup
      File.rename(bkup, bkup + ".1")
    end
  end
  File.rename(filename, bkup)
  File.open(filename, "w") {|handle| yield handle}
end

1.upto(12) {|i| backup_and_open(ARGV[0]) {|fh| fh.puts "backup #{i}"}}
```


Example:

```txt
$ echo "original" > original
$ ln -s original linkfile
$ ruby backup.rb linkfile
$ ls -l linkfile* original*
lrwxrwxrwx  1 glennj mkgroup-l-d   8 Nov 11 11:22 linkfile -> original
-rw-rw-rw-+ 1 glennj mkgroup-l-d  10 Nov 11 11:41 original
-rw-rw-rw-+ 1 glennj mkgroup-l-d  10 Nov 11 11:41 original.backup
-rw-rw-rw-+ 1 glennj mkgroup-l-d  10 Nov 11 11:41 original.backup.1
-rw-rw-rw-+ 1 glennj mkgroup-l-d   9 Nov 11 11:41 original.backup.10
-rw-rw-rw-+ 1 glennj mkgroup-l-d   9 Nov 11 11:37 original.backup.11
-rw-rw-rw-+ 1 glennj mkgroup-l-d   9 Nov 11 11:41 original.backup.2
-rw-rw-rw-+ 1 glennj mkgroup-l-d   9 Nov 11 11:41 original.backup.3
-rw-rw-rw-+ 1 glennj mkgroup-l-d   9 Nov 11 11:41 original.backup.4
-rw-rw-rw-+ 1 glennj mkgroup-l-d   9 Nov 11 11:41 original.backup.5
-rw-rw-rw-+ 1 glennj mkgroup-l-d   9 Nov 11 11:41 original.backup.6
-rw-rw-rw-+ 1 glennj mkgroup-l-d   9 Nov 11 11:41 original.backup.7
-rw-rw-rw-+ 1 glennj mkgroup-l-d   9 Nov 11 11:41 original.backup.8
-rw-rw-rw-+ 1 glennj mkgroup-l-d   9 Nov 11 11:41 original.backup.9
$ cat original original.backup original.backup.1 original.backup.2 original.backup.3 original.backup.4 original.backup.5 original.backup.6 original.backup.7 original.backup.8 original.backup.9 original.backup.10 original.backup.11
backup 12
backup 11
backup 10
backup 9
backup 8
backup 7
backup 6
backup 5
backup 4
backup 3
backup 2
backup 1
original
```



## Scala


### Java Interoperability


```Scala
import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths, StandardCopyOption}

object Backup extends App {

  def saveWithBackup(filename: String, data: String*): Unit = { //toRealPath() follows symlinks to their ends
    val (file, backFile) = (Paths.get(filename).toRealPath(), new File(filename + ".backup"))
    if (!backFile.exists) { // ensure the backup file exists so we can write to it later
      backFile.createNewFile
    }
    val back = Paths.get(filename + ".backup").toRealPath()
    Files.move(file, back, StandardCopyOption.REPLACE_EXISTING)
    val out = new PrintWriter(file.toFile)

    for (i <- 0 until data.length) {
      out.print(data(i))
      if (i < data.length - 1) out.println()
    }
  }

  saveWithBackup("original.txt", "fourth", "fifth", "sixth")

}
```



## Tcl


```tcl
package require Tcl 8.5

proc backupopen {filename mode} {
    set filename [file normalize $filename]
    if {[file exists $filename]} {
	set backups [glob -nocomplain -path $filename ,*]
	set backups [lsort -dictionary \
		[lsearch -all -inline -regexp $backups {,\d+$}]]
	if {![llength $backups]} {
	    set n 0
	} else {
	    set n [regexp -inline {\d+$} [lindex $backups end]]
	}
	while 1 {
	    set backup $filename,[incr n]
	    if {![catch {file copy $filename $backup}]} {
		break
	    }
	}
    }
    return [open $filename $mode]
}
```



## VBA


```vb
Public Sub backup(filename As String)
    If Len(Dir(filename)) > 0 Then
        On Error Resume Next
        Name filename As filename & ".bak"
    Else
        If Len(Dir(filename & ".lnk")) > 0 Then
            On Error Resume Next
            With CreateObject("Wscript.Shell").CreateShortcut(filename & ".lnk")
                link = .TargetPath
                .Close
            End With
            Name link As link & ".bak"
        End If
    End If
End Sub
Public Sub main()
    backup "D:\test.txt"
End Sub
```

