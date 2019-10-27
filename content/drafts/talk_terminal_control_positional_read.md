+++
title = "Talk:Terminal control/Positional read"
description = ""
date = 2017-06-19T21:32:55Z
aliases = []
[extra]
id = 8514
[taxonomies]
categories = []
tags = []
+++

==Very Difficult==
Just so that people know up front before trying to implement this, it is ''exceptionally'' difficult to do because “read displayed character” (whether at the current or a specific location) is not an operation that forms part of the [http://ascii-table.com/ansi-escape-sequences-vt-100.php standard set of operations]. –[[User:Dkf|Donal Fellows]] 13:01, 17 October 2010 (UTC)

: Or you can just not support vt100 terminals (and I am pretty sure that the basic implementations for these terminal control tasks do not support vt100 terminals, and I wonder about some of the other implementations also -- none really give any clues about hardware support).

: But I also wonder about the utility of "terminal control".  For most applications, on modern computers, you can do a complete update of a terminal's screen with about 2k of data.  These terminal control artifacts date back to the age when 2k of data was prohibitively bulky and slow.  Nowadays, except for interactive gaming, just sending a full screen replacement should be more than adequate for most tasks.  And, nowadays, if I was writing an interactive game, I would much rather be working with OpenGL than with a fixed-width character based terminal.

: Meanwhile, for this task, if you have a local data structure representing how things are supposed to be, that should be adequate for most tasks.  (If you have a system where the display can change due to outside factors, you are not really working with a normal terminal.)

: After thinking about the hardware support issues (or lack-of-support issues), and the utility issues (or lack-of-utility issues), I have decided to stay out of this category of tasks.  I think employing these concepts in a modern program would almost always be a mistake.  --[[User:Rdm|Rdm]] 14:53, 18 October 2010 (UTC)
:: On the subject of utility of terminal control...Over a DSL link, I can have latency/screen draw issues when running irssi over a compressed SSH tunnel to a PuTTY window with over 7000 character cells visible. On a more general note; might I suggest folks try tying into a curses library, so we can get some more code examples in here? --[[User:Short Circuit|Michael Mol]] 01:38, 19 October 2010 (UTC)
::: Ok, but I think the big reason irssi uses a terminal, rather than a windowing environment (gtk or whatever), for display purposes, is backwards compatability?  If the issue is proxying, you can use socks over your ssh connection.  --[[User:Rdm|Rdm]] 12:59, 19 October 2010 (UTC)
:::: I use a terminal-mode IRC client (as opposed to a GUI one such as XChat) for detachable sessions, speed and portability. While detachable, VNC, nx and RDP would all be slower than a terminal over SSH (I contend this would be true of any flexible remote console involving raster imagery), and I can find a terminal emulator for whatever platform I'm using. --[[User:Short Circuit|Michael Mol]] 13:57, 19 October 2010 (UTC)
::::: Ok, but some (perhaps most) of the current terminal control implementations, here, on Rosetta, do not support that kind of terminal.  But also, if I were building the kind of application you are now describing, from scratch, and I wanted to support use by a lot of people, I think I would implement it to support ajaxy web browsers (mostly on localhost), and mobile phones.  --[[User:Rdm|Rdm]] 15:47, 19 October 2010 (UTC)

I love terminal mode programs - and I don't think it is a mistake to implement them on modern operating systems. They are fast, efficient, easy to automate, accessibility friendly and it is possible to extract data from the screen for use in other programs. I don't know what attracts people to slow and manually operated graphical systems, rather than efficient terminal mode data entry systems. I am shocked at the lack of information about terminal control in the various programming language manuals. Being able to maniplate the terminal display is a fundamental operation IMHO.

[[User:Markhobley|Markhobley]] 23:38, 19 October 2010 (UTC)

I vaguely remember looking at that terminal query problem on Unix. From what I remember, a query for a character at a specific location is resolved by the terminal driver (which maintains a buffer).

[[User:Markhobley|Markhobley]] 23:38, 19 October 2010 (UTC)

: Yes, and I have some ideas for how to do it. Tricky though; Unix terminals ''really'' don't give access to that info on the terminal-slave side by default. –[[User:Dkf|Donal Fellows]] 09:32, 20 October 2010 (UTC)

I found the buffers for the consoles at /dev/vcx? so at least we can do those. If there is another set of buffers for the ptys then we have a full solution.

[[User:Markhobley|Markhobley]] 12:09, 24 October 2010 (UTC)

ncurses seems to be able to save and redraw the contents of a terminal running in emulation over a pty stream, so presumably there is code in the ncurses library to read the characters on the screen in order for it to be able to save the screen for redrawing later.

[[User:Markhobley|Markhobley]] 16:11, 27 May 2011 (UTC)

::In the general case, you can't do that, and ncurses does not need to do that.  Specifically: when ncurses is managing the screen, it ignores any activity which did not come through ncurses.  So all it has to do is maintain a data structure which represents how it has rendered the screen.  --[[User:Rdm|Rdm]] 17:14, 27 May 2011 (UTC)


: I was thinking about doing such things, but it requires the terminal to be under the complete control of the program; basically, the task as it stands requires a model of operation that is actually quite rare. –[[User:Dkf|Donal Fellows]] 14:49, 3 December 2010 (UTC)

== Possible with tmux? ==

This task might become possible if the Unix terminal is a [http://tmux.sourceforge.net/ tmux] pane. Inside the pane, run these three shell commands:


```bash
$ tmux capture-pane
$ tmux show-buffer > my-file
$ tmux delete-buffer
```


Then one must find column 3, row 6 of ''my-file''. This is not trivial, because each line ends with ASCII NL, and some lines might end early.

I have not found any way to synchronize this operation if multiple processes (perhaps in different tmux panes) want to read characters at the same time. --[[User:Kernigh|Kernigh]] 19:48, 22 September 2011 (UTC)

=== What '''terminals''' have this capability? ===

This task is "terminal control", but what terminal control languages allow screen contents to be remotely read? Certainly not ANSI/VT100.

Most of the solutions seem to be about local framebuffers or Windows text consoles, which are not "terminals".--[[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 19:36, 19 June 2017 (UTC)

: I am actually not sure if any terminals have this capability. I have read about something analogous being a security with X windows. As a general rule, if you want to support this kind of thing you do it by owning the terminal and keeping complete track of everything you put there. You do need to know its size (and when it changes size) to make this work. You also need to know when something else has damaged the thing to know when you need to re-update it. (Which is basically what paint events are, in most modern gui environments.) --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 19:51, 19 June 2017 (UTC)
:: It clearly has some security implications. Even if the console is accessible only within a given user context, it could be a problem that application A can read something that was put on the screen by application B. Speaking of "keeping track of everything you put there", I was prompted to go looking through the curses API to see whether this could be solved using curses. Curses does know everything that has been put into the screen, so it can do the optimized updates. I didn't find an API for getting characters out, though.--[[User:Kazinator|Kazinator]] ([[User talk:Kazinator|talk]]) 20:43, 19 June 2017 (UTC)
::: The right approach there, as in any context, is probably to maintain your own buffer. --[[User:Rdm|Rdm]] ([[User talk:Rdm|talk]]) 21:32, 19 June 2017 (UTC)
