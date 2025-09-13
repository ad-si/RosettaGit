+++
title = "Get system command output"
description = ""
date = 2019-09-26T02:15:35Z
aliases = []
[extra]
id = 17505
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Execute a system command and get its output into the program. The output may be stored in any kind of collection (array, list, etc.).



## Related tasks

* [[Execute_a_system_command | Execute a system command]]





## Ada

```Ada
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with GNAT.Expect;            use GNAT.Expect;
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with GNAT.String_Split;      use GNAT.String_Split;

procedure System_Command is
   Command    : String          := "ls -l";
   Args       : Argument_List_Access;
   Status     : aliased Integer;
   Separators : constant String := LF & CR;
   Reply_List : Slice_Set;

begin
   Args := Argument_String_To_List (Command);
   -- execute the system command and get the output in a single string
   declare
      Response : String :=
        Get_Command_Output
          (Command   => Args (Args'First).all,
           Arguments => Args (Args'First + 1 .. Args'Last),
           Input     => "",
           Status    => Status'Access);
   begin
      Free (Args);
      -- split the output in a slice for easier manipulation
      if Status = 0 then
         Create (S          => Reply_List,
                 From       => Response,
                 Separators => Separators,
                 Mode       => Multiple);
      end if;
   end;
   -- do something with the system output. Just print it out
   for I in 1 .. Slice_Count (Reply_List) loop
      Put_Line (Slice (Reply_List, I));
   end loop;

end System_Command;
```



## Aime


```aime
o_("-- ", sshell().plan("expr", "8", "*", "9").link.b_dump('\n'), " --\n");
```

```txt
-- 72 --
```



## AWK


```AWK

BEGIN {

         # For Windows
         out = system2var("dir")
         print out

         # Non-Windows
         out = getline2var("ls -l")
         print out
}

# For a Windows environment using system() method
function system2var(command    ,tempfile, cmd, out, rec, data, i) {
         tempfile = "C:\\TEMP\\TMP.TMP"
         cmd = command " > " tempfile
         system(cmd)
         close(cmd)
         while (getline rec < tempfile > 0) {
             if ( ++i == 1 )
                 data = rec
             else
                 data = data "\n" rec
         }
         return(data)
}

# If command returns an ERRNO function returns null string
function getline2var(command        ,fish, scale, ship) {
         command = command " 2>/dev/null"
         while ( (command | getline fish) > 0 ) {
             if ( ++scale == 1 )
                 ship = fish
             else
                 ship = ship "\n" fish
         }
         close(command)
         return ship
}
```



## BaCon


```freebasic
' Get system command
result$ = EXEC$("fortune")
PRINT CHOP$(result$)
PRINT "First word: " & TOKEN$(result$, 1)
```


```txt
prompt$ ./get-system-command
Little known fact about Middle Earth: The Hobbits had a very sophisticated
computer network!  It was a Tolkien Ring...
First word: Little

```



## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

:: Without storing the output of the command, it can be viewed by inputting the command
dir


:: Storing the output of 'dir' as "line[]" containing the respective lines of output (starting at line[1])
:: Note: This method removes any empty lines from the output
set tempcount=0
for /f "tokens=*" %%i in ('dir') do (
  set /a tempcount+=1
  set "line!tempcount!=%%i"
)
:: The array would be viewed like this
for /l %%i in (1,1,%tempcount%) do echo !line%%i!


:: Storing the output of 'dir' in a file, then outputting the contents of the file to the screen
:: NOTE: rewrites any file named "out.temp" in the current directory
dir>out.temp
type out.temp
del out.temp

pause>nul

```



## C


```C

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv)
{
    if (argc < 2) return 1;

    FILE *fd;
    fd = popen(argv[1], "r");
    if (!fd) return 1;

    char   buffer[256];
    size_t chread;
    /* String to store entire command contents in */
    size_t comalloc = 256;
    size_t comlen   = 0;
    char  *comout   = malloc(comalloc);

    /* Use fread so binary data is dealt with correctly */
    while ((chread = fread(buffer, 1, sizeof(buffer), fd)) != 0) {
        if (comlen + chread >= comalloc) {
            comalloc *= 2;
            comout = realloc(comout, comalloc);
        }
        memmove(comout + comlen, buffer, chread);
        comlen += chread;
    }

    /* We can now work with the output as we please. Just print
     * out to confirm output is as expected */
    fwrite(comout, 1, comlen, stdout);
    free(comout);
    pclose(fd);
    return 0;
}

```



## C++


```cpp
#include <fstream>
#include <iostream>

std::string execute(const std::string& command) {
    system((command + " > temp.txt").c_str());

    std::ifstream ifs("temp.txt");
    std::string ret{ std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>() };
    ifs.close(); // must close the inout stream so the file can be cleaned up
    if (std::remove("temp.txt") != 0) {
        perror("Error deleting temporary file");
    }
    return ret;
}

int main() {
    std::cout << execute("whoami") << '\n';
}
```


## C#

```c#
using System;

namespace GetSystemCommandOutput {
    class Program {
        static void Main(string[] args) {
            System.Diagnostics.Process process = new System.Diagnostics.Process();
            System.Diagnostics.ProcessStartInfo startInfo = new System.Diagnostics.ProcessStartInfo();
            startInfo.WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden;
            startInfo.FileName = "cmd.exe";
            startInfo.Arguments = "/c echo Hello World";
            startInfo.RedirectStandardOutput = true;
            startInfo.UseShellExecute = false;
            process.StartInfo = startInfo;
            process.Start();

            string output = process.StandardOutput.ReadToEnd();
            Console.WriteLine("Output is {0}", output);
        }
    }
}
```



## Clojure

sh returns a map of exit code, stdout, and stderr from the command:

```clojure
(use '[clojure.java.shell :only [sh]])
(sh "echo" "Hello")
```


```txt
{:exit 0, :out "Hello\n", :err ""}
```



## Common Lisp

```lisp
(trivial-shell:shell-command "uname -imp")
```


```txt

"x86_64 AMD A10-5750M APU with Radeon(tm) HD Graphics AuthenticAMD

```


We can also use functions specific to Common Lisp implementations.
In [http://www.sbcl.org/manual/ SBCL], we have RUN-PROGRAM, which returns
a process object. This object will contain an output stream if we use
the :output keyword. We can then read from the stream:


```lisp
(defparameter *my-proc*
  (sb-ext:run-program "mplayer" (list "/path/to/groovy/tune")
                      :search t :output :stream :wait nil))
(read-line (sb-ext:process-output *my-proc*) nil)
```


A bit more general, using [https://github.com/fare/asdf uiop] and grabbing output as a string:

```lisp
(uiop:run-program '("ls" "-l" "-a") :output :string)
```



## D


```D
import std.process;
import std.stdio;

void main() {
    auto cmd = executeShell("echo hello");

    if (cmd.status == 0) {
        writeln("Output: ", cmd.output);
    } else {
        writeln("Failed to execute command, status=", cmd.status);
    }
}
```


```txt
Output: hello
```



## Factor

<code>with-process-reader</code> is a combinator that encapsulates reading the output of a system command. It also throws an error along with the appropriate exit status in the event of failure.

```factor
USING: io.encodings.utf8 io.launcher ;
"echo hello" utf8 [ contents ] with-process-reader .
```

```txt

"hello\n"

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

'capture the output of the 'dir' command and print it to a text file

Open "dir_output.txt" For Output As #1
Open Pipe "dir" For Input As #2
Dim li As String

While Not Eof(2)
  Line Input #2, li
  Print #1, li
Wend

Close #2
Close #1
End
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=d595094b5bc9c3abb17808b2b00938f9 Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sStore As String

Shell "ls" To sStore
Print sStore

End
```


Output:

```txt

1.txt
20150328 _204330.mp4
barcode.tar
Business costs.ods
cafe
Charlie.fcstd
code128.ttf
Coffee icon.odg
DBStore
delete
delete.csv
Delete.lst
delete's.txt
...

```



## Genie


```genie
[indent=4]
/*
  Get system command output, in Genie

  valac getSystemCommandOutput.gs
  ./getSystemCommandOutput
*/

init
    try
        // Blocking with output capture
        standard_output : string
        standard_error : string
        exit_status : int
        Process.spawn_command_line_sync("sh -c 'ls getSys*'",
            out standard_output, out standard_error, out exit_status)
        print standard_output
    except e : SpawnError
        stderr.printf("%s\n", e.message)
```


Using an extra ''sh'' invocation, to limit the ''ls'' using file name expansion for the sample capture.

```txt
prompt$ valac getSystemCommandOutput.gs
prompt$ ./getSystemCommandOutput
getSystemCommandOutput
getSystemCommandOutput.gs

```



## Go


```go
package main

import (
  "fmt"
  "log"
  "os/exec"
)

func main() {
  output, err := exec.Command("ls", "-l").CombinedOutput()
  if err != nil {
    log.Fatal(err)
  }
  fmt.Print(string(output))
}
```



## Haskell

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.15 --install-ghc runghc --package process

import System.Process (readProcess)

main :: IO ()
main = do
    -- get the output of the process as a list of lines
    results <- lines <$> readProcess "hexdump" ["-C", "/etc/passwd"] ""

    -- print each line in reverse
    mapM_ (putStrLn . reverse) results
```


=={{header|Icon}} and {{header|Unicon}}==

```unicon
#
# piped.icn, Get system command output
#
# Dedicated to the public domain
#
procedure main()
    # start with an empty list
    directory := []

    # ls for UNIX, dir for other, assume Windows
    command := if &features == "UNIX" then "ls" else "dir"

    # open command in pipe mode
    p := open(command, "p") | stop("Cannot open pipe for ", command)

    # read in results and append to list
    while put(directory, read(p))

    # display the fifth entry, if there is one
    write(\directory[5])

    close(p)
end
```


```txt
prompt$ unicon -s piped.icn -x
piped.u

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 OPEN #1:"dirinfo.txt" ACCESS OUTPUT
110 SET DEFAULT CHANNEL 1
120 EXT "dir"
130 CLOSE #1
140 SET DEFAULT CHANNEL 0
```



## J


We will box the result of uname -imp on a linux system, to show that we have captured the command output in J:


```J
   require 'task'
   <shell 'uname -imp'
┌─────────────────────┐
│x86_64 x86_64 x86_64 │
└─────────────────────┘
```


Caution: I have sometimes seen some versions of linux refuse to execute subshells after a few hundred thousand shell commands (the exec system call fails). I've not found any satisfying documentation on why this happens, but I strongly suspect kernel memory fragmentation (the examples where this happened were also using a lot of memory to accumulate results and it happened much more frequently an machines with little memory than on machines with more memory). Exiting J and starting a new process has cleared it up when it has happened. Anyways, I usually prefer to do that kind of processing before J starts, just to be safe.

(I've seen other problems on windows and osx - I am only singling out linux here because it is the most convenient for command line and system command use.)


## Java

```java
import java.io.*;
import java.util.*;

public class SystemCommand {

    public static void main(String args[]) throws IOException {

        String command = "cmd /c dir";
        Process p = Runtime.getRuntime().exec(command);

        try (Scanner sc = new Scanner(p.getInputStream())) {

            System.out.printf("Output of the command: %s %n%n", command);
            while (sc.hasNext()) {
                System.out.println(sc.nextLine());
            }
        }
    }
}
```


Output:


```txt
Output of the command: cmd /c dir

 Het volume in station C heeft geen naam.
 Het volumenummer is 10CE-30C3

 Map van C:\projects\SystemCommand

30-06-2014  00:48    <DIR>          .
30-06-2014  00:48    <DIR>          ..
30-06-2014  00:48    <DIR>          build
30-06-2014  00:46             3.624 build.xml
30-06-2014  00:48    <DIR>          dist
30-06-2014  00:46                85 manifest.mf
30-06-2014  00:46    <DIR>          nbproject
30-06-2014  00:46    <DIR>          src
               2 bestand(en)            3.709 bytes
               6 map(pen)  756.833.009.664 bytes beschikbaar
```



## Jsish


```javascript
var commandOutput = exec('ls -gocart', { retAll:true });
puts(commandOutput.data);
```


The jsish ''exec'' command (like many jsish commands) accepts an optional option object, details available with interactive help:


```txt
# help exec
exec(val:string, options:string|object=void)
Execute an OS command.
If the command ends with '&', set the 'bg' option to true.
The second argument can be a string, which is the same as setting the 'inputStr' option.
By default, returns the string output, unless the 'bg', 'inputStr', 'retCode' or 'retAll' options are used

[exec options]
Option          Type    Description [Flags]
----------------------------------------------------------------------------
bg              BOOL    Run command in background using system() and return OS code.
chdir           STRING  Change to directory.
inputStr        STRING  Use string as input and return OS code.
noError         BOOL    Suppress all OS errors.
noRedir         BOOL    Disable redirect and shell escapes in command.
noShell         BOOL    Do not use native popen which invokes via /bin/sh.
trim            BOOL    Trim trailing whitespace from output.
retAll          BOOL    Return the OS return code and data as an object.
retCode         BOOL    Return only the OS return code.
```


With ''retAll'' the returned object has fields for .code, .status, .data.

The fifth element of the sample capture (''ls -gocart'') being:

```txt
# commandOutput.data.split('\n')[4];
-rw-rw-r--. 1   155 Feb  8 07:52 JSON.jsi
```



## Julia

In a single string:

```julia
ls = readstring(`ls`)
```


In multiple lines:

```julia
ll = readlines(`ls -l`)
```



## Kotlin


```scala
// version 1.0.6

import java.util.Scanner

fun main(args: Array<String>) {
    val command = "cmd /c chcp"
    val p = Runtime.getRuntime().exec(command)
    val sc = Scanner(p.inputStream)
    println(sc.nextLine())
    sc.close()
}
```


```txt

Active code page: 850

```



## LIL

The library from lil.c does not include a system command, but main.c for the lil shell does.


```tcl
set rc [system ls -go]
```


```txt
prompt$ ./lil
Little Interpreted Language Interactive Shell
# set rc [system ls -go]
total 1076
-rw-rw-r--. 1    729 Jan 14  2019 and.lil
drwxrwxr-x. 3   4096 Jan 14  2019 atom
-rw-rw-r--. 1    798 Jan 14  2019 call.lil
-rw-rw-r--. 1   3079 Aug  4 22:44 catcher.lil
drwxrwxr-x. 2   4096 Jan 14  2019 dll
-rw-rw-r--. 1    593 Jan 14  2019 dollar.lil
-rw-rw-r--. 1    697 Jan 14  2019 downeval.lil
-rw-rw-r--. 1    904 Jan 14  2019 enveval.lil
-rw-rw-r--. 1   1172 Jan 14  2019 expr.lil
-rw-rw-r--. 1    180 Jan 14  2019 fileio.lil
-rw-rw-r--. 1    427 Jan 14  2019 filter.lil
drwxrwxr-x. 2   4096 Jan 14  2019 fplil
-rw-rw-r--. 1   1369 Jan 14  2019 funcs.lil
-rw-rw-r--. 1     49 Jan 14  2019 hello.lil
-rw-rw-r--. 1    368 Jan 14  2019 jaileval.lil
-rw-rw-r--. 1 214990 Aug  8 03:18 liblil.a
-rw-rw-r--. 1    843 Jan 14  2019 liblil.tgt
-rwxrwxr-x. 1 174216 Aug  8 03:18 lil
-rw-rw-r--. 1 108062 Aug  8 03:18 lil.c
-rw-rw-r--. 1   5963 Jan 14  2019 lil.h
-rw-rw-r--. 1 214000 Aug  8 03:18 lil.o
-rw-rw-r--. 1    108 Jan 14  2019 lil.pro
-rw-rw-r--. 1   1244 Jan 14  2019 lil.tgt
-rw-rw-r--. 1    362 Jan 14  2019 lil.wpj
-rw-rw-r--. 1    666 Jan 14  2019 lists.lil
-rw-rw-r--. 1    469 Jan 14  2019 local.lil
-rw-rw-r--. 1   6082 Jan 14  2019 main.c
-rw-rw-r--. 1 137440 Jul 26 12:29 main.o
-rw-rw-r--. 1    968 Jan 14  2019 Makefile
-rw-rw-r--. 1    455 Jan 14  2019 Makefile.bcc
-rw-rw-r--. 1   1955 Jan 14  2019 mandelbrot.lil
-rw-rw-r--. 1    603 Jan 14  2019 mkmsvc.bat
-rw-rw-r--. 1    699 Jan 14  2019 mlcmt.lil
-rw-rw-r--. 1   1653 Jan 14  2019 oop_animals.lil
-rw-rw-r--. 1   1929 Jan 14  2019 oop.lil
-rw-rw-r--. 1  57495 Jan 14  2019 readme.txt
-rw-rw-r--. 1    811 Jan 14  2019 recfuncdef.lil
-rw-rw-r--. 1   1231 Jan 14  2019 renamefunc.lil
-rw-rw-r--. 1    333 Jan 14  2019 result.lil
-rw-rw-r--. 1    310 Jan 14  2019 return.lil
-rw-rw-r--. 1   1096 Jan 14  2019 robot.lil
-rw-rw-r--. 1   2368 Jan 14  2019 sm.lil
-rw-rw-r--. 1   1187 Jan 14  2019 strings.lil
-rw-rw-r--. 1    813 Jan 14  2019 topeval.lil
-rw-rw-r--. 1    615 Jan 14  2019 trim.lil
-rw-rw-r--. 1   1720 Jan 14  2019 upeval.lil
drwxrwxr-x. 2   4096 Jan 14  2019 vim
-rw-rw-r--. 1   1995 Jan 14  2019 watch.lil

# length $rc
2107
```



## Lingo

```lingo
sx = xtra("Shell").new()
put sx.shell_cmd("cd C:\dev\lsw\lib & dir")

-- "
<snip>
31.08.2016  21:25    <DIR>          .
31.08.2016  21:25    <DIR>          ..
20.08.2016  04:58    <DIR>          aes
23.06.2016  18:23    <DIR>          audio
21.07.2016  19:19    <DIR>          avmedia
23.06.2016  18:22    <DIR>          base64
23.06.2016  18:21    <DIR>          base9
<snip>"
```



## Lua


```Lua
local output = io.popen("echo Hurrah!")
print(output:read("*all"))
```

```txt
Hurrah!
```



## Mathematica


```Mathematica
RunProcess["date"]
```

```txt
<|"ExitCode" -> 0, "StandardOutput" -> "Wed Oct  4 14:01:01 BST 2017", "StandardError" -> ""|>
```



## M2000 Interpreter

Make a UTF-16LE txt.out from dir using a new cmd with /U

```M2000 Interpreter

Module CheckIt {
      Dos "cd "+quote$(Dir$) +" & cmd /U /C  dir *.txt >txt.out";
      Document txt$
      Repeat {
            Wait 100
            Try  {
                  load.doc txt$, "txt.out"
            }
      } Until doc.len(txt$)<>0
      Report txt$
}
Checkit

```




## Neko


```ActionScript
/* Get system command output, neko */
var process_run = $loader.loadprim("std@process_run", 2);
var process_stdout_read = $loader.loadprim("std@process_stdout_read", 4);
var process_stderr_read = $loader.loadprim("std@process_stderr_read", 4);
var process_stdin_close = $loader.loadprim("std@process_stdin_close", 1);
var process_exit = $loader.loadprim("std@process_exit", 1);
var sys_exit = $loader.loadprim("std@sys_exit", 1);

/* work buffer */
var bufsize = 1024;
var buffer = $smake(bufsize);

/* default command is ls, otherwise pass command line arguments */
var argc = $asize($loader.args);
var cmd = "ls";
var args;

/* Check command line arguments */
if argc > 0 {
    cmd = $loader.args[0];
}
if argc > 1 {
    args = $asub($loader.args, 1, argc - 1);
}

/* spawn process, with arguments */
var proc = process_run(cmd, args);

/* Close input channel - command might be waiting for input */
process_stdin_close(proc);

/* capture and print stdout */
var not_done = true;
var len = 0;
do {
    try {
        len = process_stdout_read(proc, buffer, 0, bufsize);
    } catch exc {
        not_done = false;
    }
    if (not_done) $print($ssub(buffer, 0, len));
} while not_done;

/* capture and print any stderr */
not_done = true;
len = 0;
do {
    try {
        len = process_stderr_read(proc, buffer, 0, bufsize);
    } catch exc {
        not_done = false;
    }
    if (not_done) $print($ssub(buffer, 0, len));
} while not_done;

/* Get the exit status */
var ps = process_exit(proc);
sys_exit(ps);
```


```txt

prompt$ nekoc getcommand.neko
prompt$ neko getcommand | tail -4
webbing.n
xml
ZipEx.hx
zipper.n

prompt$ neko getcommand ls -gocart | tail -4
-rw-rw-r--.  1     121 Sep 18 13:36 swap.neko
-rw-rw-r--.  1    1513 Sep 18 18:32 getcommand.neko
drwxr-xr-x. 15    4096 Sep 18 18:32 .
-rw-rw-r--.  1     615 Sep 18 18:32 getcommand.n
```



## Nim


```nim
import osproc

# Output string and error code
let (lsalStr, errCode) = execCmdEx("ls -al")

echo "Error code: " & $errCode
echo "Output: " & lsalStr


# Output string only
let lsStr = execProcess("ls")

echo "Output: " & lsStr

```



## Objeck


```objeck
class Test {
  function : Main(args : String[]) ~ Nil {
    output := System.Runtime->CommandOutput("ls -l");
    each(i : output) {
      output[i]->PrintLine();
    };
  }
}

```



## ooRexx


### version 1


```ooRexx
/* Execute a system command and retrieve its output into a stem. */
  trace normal

/* Make the default values for the stem null strings. */
  text. = ''

/* Issue the system command.  "address command" is optional.) */
  address command 'ls -l | rxqueue'

/* Remember the return code from the command. */
  ls_rc = rc

/* Remember the number of lines created by the command. */
  text.0 = queued()

/* Fetch each line into a stem variable. */
  do t = 1 to text.0
    parse pull text.t
  end

/* Output each line in reverse order. */
  do t = text.0 to 1 by -1
    say text.t
  end

/* Exit with the system command's return code. */
exit ls_rc
```


### version 2


```oorexx
cmd='dir tu*.rex /od'
cmd '| rxqueue'
Say 'Output of "'cmd'"'
Say
Do While queued()>0
  parse pull text
  Say text
  End
```

```txt
Output of "dir tu*.rex /od"

 Datenträger in Laufwerk I: ist USB DISK
 Volumeseriennummer: 5D55-13AC

 Verzeichnis von I:\

31.08.2016  19:36             1.358 turing.rex
31.08.2016  19:49             1.398 turing2.rex
               2 Datei(en),          2.756 Bytes
               0 Verzeichnis(se),  3.357.933.568 Bytes frei
```


### version 3


```oorexx
dir='dir.dir'
cmd='dir t*.rex /od'
cmd '>'dir
'dir tu*.rex /od >'dir
Say 'Output of "'cmd'"'
Say
Do While lines(dir)>0
  Say linein(dir)
  End
Call lineout oid
```

```txt
identical to version 2's output
```



## PARI/GP


```parigp
externstr("time/t")
```


=={{Header|Perl}}==
Uses the qx{} construct (which is a synonym for backticks, e.g. `command`) to execute a given command and redirect its output.
A (somewhat contrived*) example, capturing only STDOUT:

```perl
my @directories = grep { chomp; -d } `ls`;
for (@directories) {
chomp;
...; # Operate on directories
}
```

* Perl's opendir function should be used in preference to parsing ls--it's safer, faster, and more portable.

Perl also honors shell redirections:

```perl
my $command = shift or die "No command supplied\n";
my @output_and_errors = qx/$command 2>&1/ or die "Couldn't execute command\n";
```

qx// is implemented internally with the built-in function readpipe, which can be invoked directly as readpipe EXPR (where EXPR is some command) and assigned to scalars or lists just like qx/command/ or `command`.

The open command can also be used to open pipes using the -| mode:

```perl
use autodie;
my $enc = ':encoding(UTF-8)';
my $child_pid = open(my $pipe, "-|$enc", 'ls');
while (<$pipe>) {
  # Print all files whose names are all lowercase
    print if m/[^A-Z]+/;
}
```



## Perl 6

If you don't want to execute it in shell (and you probably don't), then use this:

```perl6
say run($command, $arg1, $arg2, :out).out.slurp-rest;
```


Unfortunately, it is very long to type, but that is the only way to pass your variables as arguments safely.

You might be tempted to start using shell when you have to pipe something, but even in that case there is no need to do so. See this code:

```perl6
my $p1 = run 'echo', 'Hello, world', :out;
my $p2 = run 'cat', '-n', :in($p1.out), :out;
say $p2.out.slurp-rest;
```

See [http://doc.perl6.org/type/Proc docs] for more info.

If you really want to run something in shell and you understand potential security problems, then you can use <code>qx//</code> (interpolates environment variables) or <code>qqx//</code> (interpolates normally). See [http://doc.perl6.org/language/quoting the docs for more info].


```perl6
say qx[dir]
```

```txt
Find_URI_in_text.p6  History_variables.p6  K-d_tree.pl
Fractran.pl	     History_variables.pl  XML_Input.p6
```



## Phix


```Phix
constant tmp = "hostname.txt",
         cmd = iff(platform()=WINDOWS?"hostname":"uname -n")
{} = system_exec(sprintf("%s > %s",{cmd,tmp}),4)
string host = trim(get_text(tmp))
{} = delete_file(tmp)
?host
```

```txt

"Pete-PC"

```

See also demo\capture_console.exw (needs a bit more work on linux)


## PicoLisp


```PicoLisp
: (in '(uname "-om") (line T))
-> "aarch64 Android"
```



## PowerShell

Capture system disk label information as an array of strings:

```PowerShell

[string[]]$volume = cmd /c vol

$volume

```

```txt

 Volume in drive C is Ordo-Xenos
 Volume Serial Number is 8C33-162D

```



## Python


```python>>>
 import subprocess
>>> returned_text = subprocess.check_output("dir", shell=True, universal_newlines=True)
>>> type(returned_text)
<class 'str'>
>>> print(returned_text)
 Volume in drive C is Windows
 Volume Serial Number is 44X7-73CE

 Directory of C:\Python33

04/07/2013  06:40    <DIR>          .
04/07/2013  06:40    <DIR>          ..
27/05/2013  07:10    <DIR>          DLLs
27/05/2013  07:10    <DIR>          Doc
27/05/2013  07:10    <DIR>          include
27/05/2013  07:10    <DIR>          Lib
27/05/2013  07:10    <DIR>          libs
16/05/2013  00:15            33,326 LICENSE.txt
15/05/2013  22:49           214,554 NEWS.txt
16/05/2013  00:03            26,624 python.exe
16/05/2013  00:03            27,136 pythonw.exe
15/05/2013  22:49             6,701 README.txt
27/05/2013  07:10    <DIR>          tcl
27/05/2013  07:10    <DIR>          Tools
16/05/2013  00:02            43,008 w9xpopen.exe
               6 File(s)        351,349 bytes
               9 Dir(s)  46,326,947,840 bytes free

>>> # Ref: https://docs.python.org/3/library/subprocess.html
```


## R


```rsplus

system("wc -l /etc/passwd /etc/group", intern = TRUE)

```

```txt

[1] "  49 /etc/passwd" "  80 /etc/group"  " 129 total"

```



## Racket

We use <code>#lang racket/base</code> to show which module system is in. It would be imported anyway if we use the larger <code>#lang racket</code>.

This demonstrates one function: <code>system</system>. It is the simplest of a family of commands in the <code>racket/system</code> collection.

See [http://docs.racket-lang.org/reference/subprocess.html?q=system#%28def._%28%28lib._racket%2Fsystem..rkt%29._system%29%29 documentation for <code>system</code> and friends].


```racket
#lang racket/base

(require racket/system
         (only-in racket/port with-output-to-string)
         tests/eli-tester)

(test
 ;; system runs command and outputs to current output port (which is stdout unless we catch it)
 (system "ls /etc/motd") => #t
 ;; it throws an error on non-zero exit code (so I need to catch it in this error handler)
 (system "false") => #f       ; nothing printed to stdout/stderr
 (system "ls /etc/mosh") => #f ; error report printed to stderr
 ;; output can be captured by redirecting stdout/stderr (which are known as current-output-port and
 ;; current-error-port in racket parlance).
 ;; the command printed a \n, so there is a newline captured by the system command
 (with-output-to-string (λ () (system "ls /etc/motd"))) => "/etc/motd\n"
 ;; no \n is captured when none is captured
 (with-output-to-string (λ () (system "echo -n foo"))) => "foo"
 ;; error is still not captured (it's still printed to stderr)
 (with-output-to-string (λ () (system "echo -n foo; echo bar 1>&2"))) => "foo"
 ;; we can capture both with:
 (let* ((out-str-port (open-output-string))
        (err-str-port (open-output-string))
        (system-rv
         (parameterize ((current-output-port out-str-port) (current-error-port err-str-port))
           (system "echo -n foo; echo bar 1>&2"))))
   (values system-rv (get-output-string out-str-port) (get-output-string err-str-port)))
 => (values #t "foo" "bar\n"))
```


```txt

/etc/motd

```

the following goes to standard error:

```txt

ls: /etc/mosh: No such file or directory
bar

```

and back to standard output:

```txt

7 tests passed

```



## REXX

```rexx
/*REXX program  executes a  system command  and displays the results  (from an array).  */
parse arg xxxCmd                                 /*obtain the (system) command from  CL.*/
trace off                                        /*suppress REXX error msgs for fails.  */
@.= 0                                            /*assign default in case ADDRESS fails.*/
address  system  xxxCmd  with  output  stem  @.  /*issue/execute the command and parms. */
if rc\==0  then  say  copies('─', 40)      ' return code '     rc     " from: "     xxxCmd
                                                 /* [↑]  display if an  error  occurred.*/
           do #=1  for @.0                       /*display the output from the command. */
           say strip(@.#, 'T')                   /*display one line at a time──►terminal*/
           end   /*#*/                           /* [↑]  displays all the output.       */
exit 0                                           /*stick a fork in it,  we're all done. */
```

```txt

 Volume in drive G is -----G-----
 Volume Serial Number is 6826-1B4B

 Directory of G:\

05/22/2012  08:27                30 SUBSET.2
05/24/2012  03:55         2,117,571 SUBSET.20
05/24/2012  03:55         1,132,068 SUBSET.21
05/24/2012  09:56           522,155 SUBSET.22
05/24/2012  09:56           193,293 SUBSET.23
05/24/2012  09:56            71,931 SUBSET.24
05/24/2012  09:56            15,995 SUBSET.25
05/24/2012  09:56             3,188 SUBSET.26
05/24/2012  09:56               471 SUBSET.27
               9 File(s)      4,056,702 bytes
               0 Dir(s)  18,252,660,736 bytes free

```



## Ring


```ring

system("dir C:\Ring\doc")

```

Output:

```txt

 Volume in drive C is Helyi lemez
 Volume Serial Number is F0B2-B1C8

 Directory of C:\Ring\doc

2016. 04. 05.  17:19    <DIR>          .
2016. 04. 05.  17:19    <DIR>          ..
2016. 04. 07.  07:44         3 276 076 Fayed_RingDoc_1.0.chm
2016. 04. 06.  19:00         5 371 211 Fayed_RingDoc_1.0.pdf
               2 File(s)      8 647 287 bytes
               2 Dir(s)  949 801 435 136 bytes free

```



## Run BASIC


```runbasic
a$ = shell$("dir") ' Returns the directory info into a$
print a$    ' prints the directory

```



## Ruby

Many options, google exec or system or %x. Demonstrating backticks:

```ruby
str = `ls`
arr = `ls`.lines
```


## Rust


```rust
use std::process::Command;
use std::io::{Write, self};

fn main() {
    let output = Command::new("/bin/cat")
                            .arg("/etc/fstab")
                            .output()
                            .expect("failed to execute process");

    io::stdout().write(&output.stdout);
}
```



## Scala


```scala
import scala.io.Source

val command = "cmd /c echo Time at %DATE% %TIME%"
val p = Runtime.getRuntime.exec(command)
val sc = Source.fromInputStream(p.getInputStream)
println(sc.mkString)
```


## Sidef

Using backticks:

```ruby
var output = `ls`             # `output` is a string
var lines  = `ls`.lines       # `lines` is an array
```


Using pipes:

```ruby
var pipe   = %p(ls)           # same as: Pipe('ls')
var pipe_h = pipe.open_r      # open the pipe for reading
var lines  = []               # will store the lines of the output
pipe_h.each { |line| lines << line }
```



## Stata

Redirect the output to a temporary file, then read its contents into a result macro r(out).


```stata
program shellout, rclass
tempfile f
tempname m
shell `0' > `f'
file open `m' using "`f'", read binary
file seek `m' eof
file seek `m' query
local n=r(loc)
if `n'>0 {
	file seek `m' tof
	file read `m' %`n's s
	file close `m'
	return local out "`s'"
}
end
```


'''Example:'''


```stata
. shellout dir /b *.dta
. display r(out)
auto.dta
titanium.dta

. shellout python -V
. display r(out)
Python 3.6.2
```



## Swift


```Swift
import Foundation

let process = Process()

process.launchPath = "/usr/bin/env"
process.arguments = ["pwd"]

let pipe = Pipe()
process.standardOutput = pipe

process.launch()

let data = pipe.fileHandleForReading.readDataToEndOfFile()
let output = String.init(data: data, encoding: String.Encoding.utf8)

print(output!)
```



## Tcl

The <code>exec</code> makes this straight-forward for most commands.

```tcl
set data [exec ls -l]
puts "read [string length $data] bytes and [llength [split $data \n]] lines"
```

There are a few exceptions, such as the <tt>DIR</tt> command on Windows, where they need to be run slightly differently due to being system shell builtins rather than executables. In that case, the <code>auto_execok</code> standard library command is used to look up how to run the command (strictly it can be used for any command — it will do path resolution, etc. — but is only necessary for system builtins).

```tcl
set data [exec {*}[auto_execok DIR]]
```

By default, Tcl will use the system encoding (as reported by <code>encoding system</code>) to understand the output byte-stream as characters, and will auto-convert all the various types of newline terminators into <tt>U+00000A</tt> characters. Control over this is possible by launching the subprocess as a pipe, configuring the pipe, and then reading the pipe in its entirety.

```tcl
# This syntax is pretty ugly, alas
set pipe [open |[list ls -l] "r"]
fconfigure $pipe -encoding iso8859-1 -translation lf
set data [read $pipe]
close $pipe
```

This is usually not necessary except when dealing with binary data output.


## VBScript

This program implements a function that executes a DOS command and returns the output to the caller.

```vb
For Each line In ExecCmd("ipconfig /all")
    Wscript.Echo line
Next

'Execute the given command and return the output in a text array.
Function ExecCmd(cmd)

    'Execute the command
    Dim wso : Set wso = CreateObject("Wscript.Shell")
    Dim exec : Set exec = wso.Exec(cmd)
    Dim res : res = ""

    'Read all result text from standard output
    Do
        res = res & VbLf & exec.StdOut.ReadLine
    Loop Until exec.StdOut.AtEndOfStream

    'Return as a text array
    ExecCmd = Split(Mid(res,2),vbLf)
End Function
```



## Ursa

This program reads the output of the ifconfig command into the string stream 'output', then writes it to the screen.

```ursa>
 decl iodevice iod
> decl string<> arg
> append "ifconfig" arg
> set iod (ursa.util.process.start arg)
> decl string<> output
> set output (iod.readlines)
> for (decl int i) (< i (size output)) (inc i)
..	out output<i> endl console
..end for
lo0: flags=8049<UP,LOOPBACK,RUNNING,MULTICAST> mtu 16384
	options=3<RXCSUM,TXCSUM>
	inet6 ::1 prefixlen 128
	inet 127.0.0.1 netmask 0xff000000
	inet6 fe80::1%lo0 prefixlen 64 scopeid 0x1
	nd6 options=1<PERFORMNUD>
gif0: flags=8010<POINTOPOINT,MULTICAST> mtu 1280
stf0: flags=0<> mtu 1280
en0: flags=8863<UP,BROADCAST,SMART,RUNNING,SIMPLEX,MULTICAST> mtu 1500
	options=27<RXCSUM,TXCSUM,VLAN_MTU,TSO4>
	ether d4:9a:20:b8:8d:2c
	nd6 options=1<PERFORMNUD>
	media: autoselect
	status: inactive
en1: flags=8863<UP,BROADCAST,SMART,RUNNING,SIMPLEX,MULTICAST> mtu 1500
	ether 00:26:08:e0:67:cc
	inet6 fe80::226:8ff:fee0:67cc%en1 prefixlen 64 scopeid 0x5
	inet 172.20.30.66 netmask 0xffffff00 broadcast 172.20.30.255
	nd6 options=1<PERFORMNUD>
	media: autoselect
	status: active
fw0: flags=8863<UP,BROADCAST,SMART,RUNNING,SIMPLEX,MULTICAST> mtu 4078
	lladdr d4:9a:20:ff:fe:b8:8d:2c
	nd6 options=1<PERFORMNUD>
	media: autoselect <full-duplex>
	status: inactive
p2p0: flags=8843<UP,BROADCAST,RUNNING,SIMPLEX,MULTICAST> mtu 2304
	ether 02:26:08:e0:67:cc
	media: autoselect
	status: inactive
>
```



## zkl

From the REPL on Linux. Runs a command in the shell with stdout redirected to file, then slurps the file. A bit greasy since there isn't a way to find/generate a unique unused file name.

```zkl
zkl: System.cmd("date >foo.txt")
0  // date return code
zkl: File("foo.txt").read().text
Wed Aug 20 00:28:55 PDT 2014
```

