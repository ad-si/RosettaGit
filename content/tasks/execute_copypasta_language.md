+++
title = "Execute CopyPasta Language"
description = ""
date = 2019-09-28T00:49:52Z
aliases = []
[extra]
id = 22561
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Implement a '''[[CopyPasta Language]]''' interpreter or compiler. These are the commands used by CopyPasta Language:
{| class="wikitable"
!Command
!Description
|-
| style="text-align:center"| <code>Copy</code> || Copy the text of the following line to the clipboard
|-
| style="text-align:center"| <code>CopyFile</code> || Copy the text of the file cited in the following line to the clipboard or in the case where the next line is TheF*ckingCode copies the code to the clipboard
|-
| style="text-align:center"| <code>Duplicate</code> || Duplicate the text in the clipboard as many times as the following line specifies
|-
| style="text-align:center"| <code>Pasta!</code> || Display the clipboard and stop the program
|}



## Go

Here's my tentative attempt at an interpreter for CopyPasta.

I've made the following assumptions:

1. The program to be interpreted is read in from a file whose path is supplied as a command line argument.

2. Writing to the clipboard should always overwrite what (if anything) is already in the clipboard and not append to it.

3. CopyPasta is case sensitive.

4. When writing commands, leading and trailing whitespace should be ignored.

5. Blank commands should be ignored.

6. When a command consumes the following line, that line should not be reprocessed if it is a command itself.

7. The program should terminate with a suitable message if any error is encountered (i.e. no following line, file doesn't exist etc.).

8. When the program is about to end and the contents of the clipboard have (when appropriate) been printed out, the clipboard should be cleared. 

```go
// copypasta.go
package main

import (
    "fmt"
    "github.com/atotto/clipboard"
    "io/ioutil"
    "log"
    "os"
    "runtime"
    "strconv"
    "strings"
)

func check(err error) {
    if err != nil {
        clipboard.WriteAll("") // clear clipboard
        log.Fatal(err)
    }
}

func interpret(source string) {
    source2 := source
    if runtime.GOOS == "windows" {
        source2 = strings.ReplaceAll(source, "\r\n", "\n")
    }
    lines := strings.Split(source2, "\n")
    le := len(lines)
    for i := 0; i < le; i++ {
        lines[i] = strings.TrimSpace(lines[i]) // ignore leading & trailing whitespace
        switch lines[i] {
        case "Copy":
            if i == le-1 {
                log.Fatal("There are no lines after the Copy command.")
            }
            i++
            err := clipboard.WriteAll(lines[i])
            check(err)
        case "CopyFile":
            if i == le-1 {
                log.Fatal("There are no lines after the CopyFile command.")
            }
            i++
            if lines[i] == "TheF*ckingCode" {
                err := clipboard.WriteAll(source)
                check(err)                
            } else {
                bytes, err := ioutil.ReadFile(lines[i])
                check(err)
                err = clipboard.WriteAll(string(bytes))
                check(err)                
            }
        case "Duplicate":
            if i == le-1 {
                log.Fatal("There are no lines after the Duplicate command.")
            }
            i++
            times, err := strconv.Atoi(lines[i])
            check(err)
            if times < 0 {
                log.Fatal("Can't duplicate text a negative number of times.")
            }
            text, err := clipboard.ReadAll()
            check(err)
            err = clipboard.WriteAll(strings.Repeat(text, times+1))
            check(err)
        case "Pasta!":
            text, err := clipboard.ReadAll()
            check(err)
            fmt.Println(text)
            return
        default:
            if lines[i] == "" {
                continue // ignore blank lines
            }
            log.Fatal("Unknown command, " + lines[i])
        }
    }
}

func main() {
    if len(os.Args) != 2 {
        log.Fatal("There should be exactly one command line argument, the CopyPasta file path.")
    }
    bytes, err := ioutil.ReadFile(os.Args[1])
    check(err)
    interpret(string(bytes))
    err = clipboard.WriteAll("") // clear clipboard
    check(err)
}
```


The following files have been used for testing:

```txt

// prog.cp
Copy
Rosetta Code
Duplicate
2
Pasta!

// prog2.cp
CopyFile
pasta.txt
Duplicate
1
Pasta!

// prog3.txt
Copy
Invalid
  Duplicate
1

Goto
3
Pasta!

// pasta.txt
I'm the pasta.txt file.

```

With the following results:

```txt

$ go build copypasta.go
$ ./copypasta
There should be exactly one command line argument, the CopyPasta file path.

$ ./copypasta prog4.cp
open prog4.cp: no such file or directory

$ ./copypasta prog.cp
Rosetta CodeRosetta CodeRosetta Code

$ ./copypasta prog2.cp
I'm the pasta.txt file.
I'm the pasta.txt file.

$ ./copypasta prog3.cp
Unknown command, Goto

```



## zkl


```zkl
var clipBoard=Data(), srcNm=vm.arglist[0];
pasta:=File(srcNm).read().howza(11);	// zkl pastaprog.cp, stripped lines
foreach line in (pasta){
   switch(line.toLower()){
      case("copy"){ clipBoard.clear(next(__lineWalker),"\n") }
      case("copyfile"){
         n:=next(__lineWalker);
	 if(n=="TheF*ckingCode") clipBoard.clear(pasta);
	 else			 clipBoard.clear(File(n).read());
      }
      case("duplicate"){ 
         n,t := next(__lineWalker,True), clipBoard.copy();
	 do(n){ t.append(clipBoard) }	// noop if n<1
	 clipBoard=t;
      }
      case("pasta!"){ print(clipBoard.text); break; }
      case(""){}
      else{ error(__lineWalker,"Unknown command: ") }
   }
}
fcn error(w,msg){
   println("%s: %d: %s%s".fmt(srcNm, w.n, msg, w.value)); 
   System.exit(1) 
}
fcn next(w,wantInt=False){
   try{
      t:=w.next();
      if(wantInt) t=t.toInt();
      return(t)
   }catch(TheEnd){ error(w,"Error: End of file: ") }
    catch{ error(w,wantInt and "Not an int: " or "Error: ") }
}
```

Input programs:
<pre style="height:15ex">
//////////////prog.cp:
Copy
Rosetta Code
Duplicate
2
Pasta!

//////////////prog2.cp:
CopyFile
pasta.txt
Duplicate
1
Pasta!

/////////prog3.cp:
Copy
Invalid
  Duplicate
1

Goto
3
Pasta!

//////////////pasta.txt:
I'm the pasta.txt file.

```

```txt

$ zkl copyPasta.zkl prog.cp
Rosetta Code
Rosetta Code
Rosetta Code

$ zkl copyPasta.zkl prog2.cp
I'm the pasta.txt file.
I'm the pasta.txt file.

$ zkl copyPasta.zkl prog3.cp
prog3.cp: 6: Unknown command: Goto

```

