+++
title = "Template:E 2D utilities"
description = ""
date = 2010-10-11T15:29:35Z
aliases = []
[extra]
id = 4580
[taxonomies]
categories = []
tags = []
+++

<noinclude>Some [[Category:E]] library code for the [[Spiral]] and [[Zig Zag]] tasks. Perhaps ought to be put into generally available E data structure/math libraries.

{{template}}</noinclude>
```e
/** Missing scalar multiplication, but we don't need it. */
def makeVector2(x, y) {
  return def vector {
    to x() { return x }
    to y() { return y }
    to add(other) { return makeVector2(x + other.x(), y + other.y()) }
    to clockwise() { return makeVector2(-y, x) }
  }
}

/** Bugs: (1) The printing is specialized. (2) No bounds check on the column. */
def makeFlex2DArray(rows, cols) {
  def storage := ([null] * (rows * cols)).diverge()
  return def flex2DArray {
    to __printOn(out) {
      for y in 0..!rows {
        for x in 0..!cols {
          out.print(<import:java.lang.makeString>.format("%3d", [flex2DArray[y, x]]))
        }
        out.println()
      }
    }
    to get(r, c) { return storage[r * cols + c] }
    to put(r, c, v) { storage[r * cols + c] := v }
  }
}
```

