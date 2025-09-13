+++
title = "Execute HQ9+/Ursa"
description = ""
date = 2016-06-10T20:25:36Z
aliases = []
[extra]
id = 20953
[taxonomies]
categories = []
tags = []
+++


This is an HQ9+ interpreter written in Standard Ursa. It expects the filename of an HQ9+ source file as a command-line argument.

```ursa
#
# hq9+ interpreter in ursa
#

# check if a file was provided on the command line; if not, stop
if (> (size args) 2)
	out "error: no file provided" endl console
end if

# safely open the file by creating it first, then read it
decl file f
f.create args<1>
f.open args<1>
decl string hq9
set hq9 (f.readall)
f.close

# interpret the hq9+
# define an int to be the accumlator (even though this isn't technically
# an accumulator)
decl int accum
for (decl int i) (< i (size hq9)) (inc i)
	if (= (lower hq9<i>) "h")
		out "hello world!" endl console
	end if
	if (= (lower hq9<i>) "q")
		out hq9 console
	end if
	if (= hq9<i> "9")
		decl int bottles
		decl string bottlestr
		
		for (set bottles 99) (> bottles 0) (dec bottles)
		        if (= bottles 1)
		                set bottlestr "bottle"
		        else
		                set bottlestr "bottles"
		        end if

		        out bottles " " bottlestr " of beer on the wall" endl console
		        out bottles " " bottlestr " of beer" endl console
		        out "Take one down, pass it around." endl console

		        if (not (= bottles 2))
		                out (int (- bottles 1)) " bottles of beer on the wall." endl endl console
		        else
                		out (int (- bottles 1)) " bottle of beer on the wall." endl endl console
		        end if
		end for
	end if
	if (= hq9<i> "+")
		set accum (int (+ accum 1))
	end if
end for
```

