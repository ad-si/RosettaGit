+++
title = "Category:Curses"
description = ""
date = 2017-02-28T22:52:31Z
aliases = []
[extra]
id = 10373
[taxonomies]
categories = []
tags = []
+++

{{library}}
Curses is a library to draw characters on a terminal. Programs can use curses to move the terminal's cursor and display text in any line and column. For input, curses can get individual key presses from the terminal's keyboard. To optimize output, curses delays the screen updates until the program refreshes the screen.

There are at least 4 implementations of curses:

* [[:Category:ncurses|ncurses]].
* [http://netbsd.gw.com/cgi-bin/man-cgi?curses+3+NetBSD-current NetBSD curses].
* [http://pdcurses.sourceforge.net/ PDCurses].
* [http://compute.cnr.berkeley.edu/cgi-bin/man-cgi?curses+3 System V curses].

Curses is terminal-independent, and uses ''termcap'' or ''terminfo'' to send escape sequences to different flavors of terminals. (The exception is PDCurses, which never sends escape sequences, because it uses a DOS console or a graphical interface.)

* '''C''' 
```c>#include <curses.h>
```
 Then link the program with <code
-lcurses</code>. (Some old or strange systems might need <code>-lcurses -ltermlib</code>.)

* '''Common Lisp''' 
```lisp
;; After installing the quicklisp library manager
(ql:quickload :croatoan)
```


* '''Ruby''' 
```ruby
require 'curses'
```


* '''Go''' 
```go
import "github.com/gbin/goncurses"
```
There are a number of curses bindings for Go.  This one is  popular.

[[Category:Terminal control]]
