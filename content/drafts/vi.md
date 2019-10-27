+++
title = "Vi"
description = ""
date = 2014-11-11T13:21:39Z
aliases = []
[extra]
id = 18218
[taxonomies]
categories = []
tags = []
+++

[[wp:vi|VI]] (for "visual editor") is a text editor for "plain text". 

It is the [[wp:Category:Unix text editors|standard-editor]] on most Unix-systems.

The current versions have almost everything you can expect from an modern editor: 

scripting, macro-recording, tags, syntax-coloring, UTF-8, Unicode, GUI etc.

However, the basic concept is quite old, e.g. it also has an '''ex'''-mode 

that was designed to work with old teletype-devices.


There are several variants:
* [[BusyBox]]                     - includes a tiny vi clone
* [[wp:Elvis (text editor)|Elvis]]
* [[wp:nvi|nvi]]                  - "new vi"
* [[wp:vile (editor)|vile]]       - "VI Like Emacs"
* [[wp:Vim (text editor)|vim]]    - "VI-Improved"   Website: [http://www.vim.org vim.org] 
** current version: Vim 7.4.507 (2013-07) 
** See also [[:Category:Vim Script]]


==VI-Quickreference==

'''Usage:''' vi file.txt

The bottom screen-row is used as a statusline, and for entering commands.

There is a "normal" mode that acts as command-mode, and an insert-mode:

```txt

i       - insert text at cursor-position
a       - append text at end-of-line
ESC     - abort command, i.e. end insert-mode, and return to normal/commandmode.

h j k l - move cursor: left/down/up/right  ( use with count: "5k" moves 5 lines up )
          The cursor-keys should work as well
w b     - move cursor one word forward/backward  ( "3w" moves 5 words forward )
G       - Goto last line    ( "33G" goto line 33 )
/abc    - find text "abc"
n       - find next match

x       - delete character  ( use with count: "5x"  deletes 5 char )
dd      - delete line       ( use with count: "2dd" deletes 2 lines )
p P     - paste: insert previously deleted text at/before the current position
u       - undo
r R     - replace single char / enter Replace-mode (like insert, but overwrites)
.       - repeat last change-command

: is the prefix for commands:

:help          - show help-pages 
:set ruler     - toggle display of current line-number in status-line

:r file2.txt   - read: insert contents of file2.txt at current position

:33            - move to line 33

:2,5s/old/new/ - substitute: in lines 2..5 replace "old" with "new"

:w             - write current file ( :w! to force it )
:q             - quit               ( :q! to force it.  Combined: ":wq!" )

```

<small>( Also, look for "vi reference mug" :)</small>


[[Category:Editor]]
