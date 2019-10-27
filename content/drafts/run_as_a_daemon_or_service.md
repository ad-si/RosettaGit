+++
title = "Run as a daemon or service"
description = ""
date = 2019-01-31T22:24:57Z
aliases = []
[extra]
id = 10877
[taxonomies]
categories = []
tags = []
+++

{{draft task|Programming environment operations}}
A [[wp:Daemon_(computing)|daemon]] is a service that runs in the background independent of a users login session.

Demonstrate how a program disconnects from the terminal to run as a daemon in the background.

Write a small program that writes a message roughly once a second to its stdout which should be redirected to a file.

Note that in some language implementations it may not be possible to disconnect from the terminal, and instead the process needs to be started with stdout (and stdin) redirected to files before program start. If that is the case then a helper program to set up this redirection should be written in the language itself. A shell wrapper, as would be the usual solution on Unix systems, is not appropriate.


## C

==={{libheader|BSD libc}}===
[[BSD]] provides a convenient [http://netbsd.gw.com/cgi-bin/man-cgi?daemon+3+NetBSD-current daemon(3)] function. [[GNU]] libc also provides [http://www.kernel.org/doc/man-pages/online/pages/man3/daemon.3.html daemon(3)], but [[POSIX]] omits it, so it is not portable. Other BSDisms in this program are __progname and <err.h>.

The task also wants to redirect stdout. This program does so with [http://netbsd.gw.com/cgi-bin/man-cgi?dup2+2+NetBSD-current dup2(2)]. Had we wanted to directly write to a file, we could open the file with <code>file = fopen(argv[1], "a")</code>, and write to ''file'' instead of ''stdout''.


```c>#include <err.h

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <syslog.h>
#include <time.h>
#include <unistd.h>

int
main(int argc, char **argv)
{
	extern char *__progname;
	time_t clock;
	int fd;

	if (argc != 2) {
		fprintf(stderr, "usage: %s file\n", __progname);
		exit(1);
	}

	/* Open the file before becoming a daemon. */
	fd = open(argv[1], O_WRONLY | O_APPEND | O_CREAT, 0666);
	if (fd < 0)
		err(1, argv[1]);

	/*
	 * Become a daemon. Lose terminal, current working directory,
	 * stdin, stdout, stderr.
	 */
	if (daemon(0, 0) < 0)
		err(1, "daemon");

	/* Redirect stdout. */
	if (dup2(fd, STDOUT_FILENO) < 0) {
		syslog(LOG_ERR, "dup2: %s", strerror(errno));
		exit(1);
	}
	close(fd);

	/* Dump clock. */
	for (;;) {
		time(&clock);
		fputs(ctime(&clock), stdout);
		if (fflush(stdout) == EOF) {
			syslog(LOG_ERR, "%s: %s", argv[1], strerror(errno));
			exit(1);
		}
		sleep(1);	/* Can wake early or drift late. */
	}
}
```



```txt
$ make dumper
cc -O2 -pipe    -o dumper dumper.c 
$ ./dumper dump
$ tail -f dump
Fri Nov 18 13:50:41 2011
Fri Nov 18 13:50:42 2011
Fri Nov 18 13:50:43 2011
Fri Nov 18 13:50:44 2011
Fri Nov 18 13:50:45 2011
^C
$ pkill -x dumper
$ rm dump
```



## Go

{{libheader|go-daemon}}
{{works with|Ubuntu 16.04}}

```go
package main

import (
    "fmt"
    "github.com/sevlyar/go-daemon"
    "log"
    "os"
    "time"
)

func work() {
    f, err := os.Create("daemon_output.txt")
    if err != nil {
        log.Fatal(err)
    }
    defer f.Close()
    ticker := time.NewTicker(time.Second)
    go func() {
        for t := range ticker.C {
            fmt.Fprintln(f, t) // writes time to file every second
        }
    }()
    time.Sleep(60 * time.Second) // stops after 60 seconds at most
    ticker.Stop()
    log.Print("ticker stopped")
}

func main() {
    cntxt := &daemon.Context{
        PidFileName: "pid",
        PidFilePerm: 0644,
        LogFileName: "log",
        LogFilePerm: 0640,
        WorkDir:     "./",
        Umask:       027,
        Args:        []string{"[Rosetta Code daemon example]"},
    }

    d, err := cntxt.Reborn()
    if err != nil {
        log.Fatal("Unable to run: ", err)
    }
    if d != nil {
        return
    }
    defer cntxt.Release()

    log.Print("- - - - - - - - - - - - - - -")
    log.Print("daemon started")

    work()
}
```


{{out}}
Although the daemon will only run for a maximum of 60 seconds, we kill it after a few seconds so the output is not too long.

```txt

$ go build daemon.go
$ ./daemon
$ kill `cat pid`
$ cat log
2019/01/31 22:14:50 - - - - - - - - - - - - - - -
2019/01/31 22:14:50 daemon started
$ cat daemon_output.txt
2019-01-31 22:14:51.641498031 +0000 GMT m=+1.089305363
2019-01-31 22:14:52.641672923 +0000 GMT m=+2.089480618
2019-01-31 22:14:53.641724473 +0000 GMT m=+3.089531868
2019-01-31 22:14:54.641557151 +0000 GMT m=+4.089364696
2019-01-31 22:14:55.641543175 +0000 GMT m=+5.089350596

```



## PARI/GP

GP scripts cannot run in this fashion directly, but can be compiled into PARI code with <code>gp2c</code>. PARI code, whether from <code>gp2c</code> or not, can be run as a daemon just as [[#C|C]] would be.


## Perl 6


```perl6
#!/usr/bin/env perl6

# Reference:
# https://github.com/hipek8/p6-UNIX-Daemonize/

use v6;
use UNIX::Daemonize;
use File::Temp;

my ($output, $filehandle) = tempfile(:tempdir("/tmp"),:!unlink) or die;

say "Output now goes to ",$output;

daemonize();

loop {
   sleep(1);
   spurt $output, DateTime.now.Str~"\n", :append;
}
```

{{out}}
```txt
root@ubuntu:~# su - david
david@ubuntu:~$ ./dumper.p6
Output now goes to /tmp/x2ovx9JG8b
david@ubuntu:~$ tail -f /tmp/x2ovx9JG8b
2018-12-11T20:20:01.510484+08:00
2018-12-11T20:20:02.513732+08:00
2018-12-11T20:20:03.517063+08:00
2018-12-11T20:20:04.520394+08:00
2018-12-11T20:20:05.524871+08:00
2018-12-11T20:20:06.528244+08:00
2018-12-11T20:20:07.531985+08:00
2018-12-11T20:20:08.537776+08:00
2018-12-11T20:20:09.541606+08:00
2018-12-11T20:20:10.545796+08:00
2018-12-11T20:20:11.549047+08:00
2018-12-11T20:20:12.552704+08:00
^C
david@ubuntu:~$ exit
logout
root@ubuntu:~# tail -f /tmp/x2ovx9JG8b
2018-12-11T20:20:28.623690+08:00
2018-12-11T20:20:29.626978+08:00
2018-12-11T20:20:30.634309+08:00
2018-12-11T20:20:31.637481+08:00
2018-12-11T20:20:32.640794+08:00
2018-12-11T20:20:33.643947+08:00
2018-12-11T20:20:34.647146+08:00
2018-12-11T20:20:35.651008+08:00
^C
root@ubuntu:~# su - david
david@ubuntu:~$ tail -f /tmp/x2ovx9JG8b
2018-12-11T20:20:51.711357+08:00
2018-12-11T20:20:52.715044+08:00
2018-12-11T20:20:53.718921+08:00
2018-12-11T20:20:54.722134+08:00
2018-12-11T20:20:55.725970+08:00
2018-12-11T20:20:56.729160+08:00
2018-12-11T20:20:57.732376+08:00
2018-12-11T20:20:58.735409+08:00
2018-12-11T20:20:59.738886+08:00
2018-12-11T20:21:00.743045+08:00
2018-12-11T20:21:01.748113+08:00
2018-12-11T20:21:02.753204+08:00
2018-12-11T20:21:03.756665+08:00
2018-12-11T20:21:04.759902+08:00
^C
david@ubuntu:~$ pkill -c moar
1

```



## PicoLisp


```PicoLisp
(unless (fork)
   (out "file.log"
      (println *Pid)    # First write the daemon's PID to the file
      (for N 3600       # Write count for about one hour (if not killed)
         (wait 1000)
         (println N)
         (flush) ) )
   (bye) )              # Child terminates after one hour

(bye)                   # Parent terminates immediately
```



## Pike

<code>__FILE__</code> is a preprocessor definition that contains the current filename.
if the first argument is "daemon" the program will be restarted with stdout redirected to "foo".


```Pike
int main(int argc, array argv)
{
    if (sizeof(argv)>1 && argv[1] == "daemon")
    {
        Stdio.File newout = Stdio.File("foo", "wc");
        Process.spawn_pike(({ __FILE__ }), ([ "stdout":newout ]));
        return 1;
    }

    int i = 100;
    while(i--)
    {
        write(i+"\n");
        sleep(0.1);
    }
}
```



## Racket


```racket

#lang racket
(require ffi/unsafe)
((get-ffi-obj 'daemon #f (_fun _int _int -> _int)) 0 0)
(with-output-to-file "/tmp/foo"
  (λ() (for ([i 10]) (displayln (random 1000)) (flush-output) (sleep 1))))

```



## Sidef

When the "daemon" argument is specified, a fork of the program is created with its STDOUT redirected into the file "foo.txt", and the main process is exited.

```ruby
var block = {
    for n in (1..100) {
        STDOUT.say(n)
        Sys.sleep(0.5)
    }
}

if (ARGV[0] == 'daemon') {
    STDERR.say("Daemon mode")
    STDOUT{:fh} = %f'foo.txt'.open_w(){:fh}
    STDOUT.autoflush(true)
    block.fork
    STDERR.say("Exiting")
    Sys.exit(0)
}

STDERR.say("Normal mode")
block.run
```



## Tcl

Tcl doesn't come with tools for converting the process into a daemon, but can build them easily enough. Here's the BSD daemon function mapped into a Tcl command in a package.
{{libheader|Critcl}}

```tcl
package provide daemon 1
package require critcl

critcl::ccode {
    #include <stdlib.h>
}
critcl::cproc daemon {Tcl_Interp* interp} ok {
    if (daemon(0, 0) < 0) {
	Tcl_AppendResult(interp, "cannot switch to daemon operation: ",
		Tcl_PosixError(interp), NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}
```

These tools can then be used to solve this task:

```tcl
### Command line argument parsing
if {$argc < 1} {
    puts "usage: $argv0 file ?message...?"
    exit 1
} elseif {$argc == 1} {
    set filename [lindex $argv 0]
    set message "Hi there!"
} else {
    set message [join [lassign $argv filename]]
}

### Daemonize
package require daemon
daemon
close stdout; open $filename    ;# Redirects stdout!

### Print the message to the file every second until killed
proc every {ms body} {eval $body; after $ms [info level 0]}
every 1000 {puts "[clock format [clock seconds]]: $message"}
vwait forever
```

On Windows, there is a commercial extension to Tcl which allows a script to be installed as a service. Such a script would be much like the one above, but without the daemonization section as that has become a property of the runtime.

{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|Openscad}}
{{omit from|TPP}}
{{omit from|ZX Spectrum Basic}}
