+++
title = "RCHQ9+/NetRexx"
description = ""
date = 2013-07-01T02:57:35Z
aliases = []
[extra]
id = 14753
[taxonomies]
categories = []
tags = []
+++

{{implementation|HQ9+}}{{collection|RCHQ9+}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method interpretHCQ9p(code, accumulator = 0) public static
  quine = code
  loop for code.length
    parse code cc +1 code
    cc = cc.lower
    if cc \= '' then do
      select case cc
        when 'h' then say 'Hello, world!'
        when 'q' then say quine
        when '9' then ninetyNineBottles()
        when '+' then accumulator = accumulator + 1
        otherwise     nop
        end
      end
    end
  return accumulator

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method ninetyNineBottles() public static
  bottleCount = bottles(99)
  loop bc = 99 to 1 by -1
    say bottleCount 'on the wall'
    say bottleCount
    say 'Take one down, pass it around'
    bottleCount = bottles(bc - 1)
    say bottleCount 'on the wall'
    say
    end bc
  return

method bottles(nb) private static
  select case nb
    when 0 then bm = 'No more bottles'
    when 1 then bm = 'One bottle'
    otherwise   bm = nb 'bottles'
    end
  return bm 'of beer'

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  parse arg code
  say 'HQ9+ Accumulator =' interpretHCQ9p(code)
  return

```

