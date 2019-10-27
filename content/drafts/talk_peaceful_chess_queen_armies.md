+++
title = "Talk:Peaceful chess queen armies"
description = ""
date = 2019-03-27T10:08:35Z
aliases = []
[extra]
id = 22244
[taxonomies]
categories = []
tags = []
+++

==Original Python exhaustive search==
I was experimenting with various things when doing the Python.
This is the original:

Exhaustive search.

```python
from itertools import combinations, count
from functools import lru_cache, reduce


# n-by-n board
n = 5

def _2d(n=n):
  for i in range(n):
      print('  '.join(f'{i},{j}' for j in range(n)))

def _1d(n=n):
  for i in range(0, n*n, n):
      print(',  '.join(f'{i+j:2}' for j in range(n)))

_bbullet, _wbullet = '\u2022\u25E6'
#_bqueen, _wqueen = 'BW'
_bqueen, _wqueen = '\u265B\u2655'
_bqueenh, _wqueenh = '&#x265b;', '<font color="green">&#x2655;</font>'
_or = set.__or__


def place(m, n):
    "Place m black and white queens, peacefully, on an n-by-n board"
    
    # 2-D Board as 1-D array:  2D(x, y) == 1D(t%n, t//n)
    board = set(range(n*n))

    #placements = list(combinations(board, m))
    placements = {frozenset(c) for c in combinations(board, m)}
    for blacks in placements:
        black_attacks = reduce(_or, 
                               (queen_attacks_from(pos, n) for pos in blacks), 
                               set())
        #for whites in placements:
        for whites in {frozenset(c) for c in combinations(board - black_attacks, m)}:
            if not black_attacks & whites:
                return blacks, whites
    return set(), set()

@lru_cache(maxsize=None)
def queen_attacks_from(pos, n=n):
    a = set([pos])    # Its position
    a.update(range(pos//n*n, pos//n*n+n))    # Its row
    a.update(range(pos%n, n*n, n))           # Its column
    # Diagonals
    x0, y0 = pos%n, pos//n
    for x1 in range(n):
        # l-to-r diag
        y1 = y0 -x0 +x1
        if 0 <= y1 < n: 
            a.add(x1 + y1 * n)
        # r-to-l diag
        y1 = y0 +x0 -x1
        if 0 <= y1 < n: 
            a.add(x1 + y1 * n)
    return a

def pboard(black_white=None, n=n):
    if black_white is None: 
        blk, wht = set(), set()
    else:
        blk, wht = black_white
    print(f"## {len(blk)} black and {len(wht)} white queens "
          f"on a {n}-by-{n} board:", end='')
    for xy in range(n*n):
        if xy %n == 0:
            print()
        ch = ('?' if xy in blk and xy in wht 
              else _bqueen if xy in blk
              else _wqueen if xy in wht
              else _bbullet if (xy%n + xy//n)%2 else _wbullet)
        print('%s' % ch, end='')
    print()

def hboard(black_white=None, n=n):
    if black_white is None: 
        blk, wht = set(), set()
    else:
        blk, wht = black_white
    out = (f"
<b>## {len(blk)} black and {len(wht)} white queens "
           f"on a {n}-by-{n} board:</b>
\n")
    out += "<table>\n  "
    tbl = ''
    for xy in range(n*n):
        if xy %n == 0:
            tbl += '</tr>\n  <tr>\n'
        ch = ('<span style="color:red">?</span>' if xy in blk and xy in wht 
              else _bqueenh if xy in blk
              else _wqueenh if xy in wht
              else "")
        bg = "" if (xy%n + xy//n)%2 else ' bgcolor="silver"'
        tbl += f'    <td style="width:16pt; height:16pt;"{bg}>{ch}</td>\n'
    out += tbl[7:]
    out += '</tr>\n</table>\n
\n'
    return out

if __name__ == '__main__':
    n=2
    html = ''
    for n in range(2, 7):
        print()
        queen_attacks_from.cache_clear()    # memoization cache
        #
        for m in count(1):
            ans = place(m, n)
            if ans[0]:
                pboard(ans, n)
                html += hboard(ans, n)
            else:
                comment = f"# Can't place {m}+ queens on a {n}-by-{n} board"
                print (comment)
                html += f"<b>{comment}</b>

\n\n"
                break
    print('\n')
    html += '
\n'
    #
    m, n = 5, 7
    queen_attacks_from.cache_clear()
    ans = place(m, n)
    pboard(ans, n)    
    html += hboard(ans, n)
    with open('peaceful_queen_armies.htm', 'w') as f:
        f.write(html)
```


{{out}}
The console output Unicode queen characters display wider than other characters in monospace font so the alternative HTML output is shown below.

<div style="overflow:scroll; height:250px;">
<b># Can't place 1+ queens on a 2-by-2 board</b>




<b>## 1 black and 1 white queens on a 3-by-3 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>


<b># Can't place 2+ queens on a 3-by-3 board</b>




<b>## 1 black and 1 white queens on a 4-by-4 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>



<b>## 2 black and 2 white queens on a 4-by-4 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>


<b># Can't place 3+ queens on a 4-by-4 board</b>




<b>## 1 black and 1 white queens on a 5-by-5 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>



<b>## 2 black and 2 white queens on a 5-by-5 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
</tr>
</table>



<b>## 3 black and 3 white queens on a 5-by-5 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>



<b>## 4 black and 4 white queens on a 5-by-5 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>


<b># Can't place 5+ queens on a 5-by-5 board</b>




<b>## 1 black and 1 white queens on a 6-by-6 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>



<b>## 2 black and 2 white queens on a 6-by-6 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>



<b>## 3 black and 3 white queens on a 6-by-6 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>



<b>## 4 black and 4 white queens on a 6-by-6 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>



<b>## 5 black and 5 white queens on a 6-by-6 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver">&#x265b;</td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>


<b># Can't place 6+ queens on a 6-by-6 board</b>






<b>## 5 black and 5 white queens on a 7-by-7 board:</b>

<table>
   <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;">&#x265b;</td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
</tr>
  <tr>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
    <td style="width:16pt; height:16pt;"></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;"><font color="green">&#x2655;</font></td>
    <td style="width:16pt; height:16pt;" bgcolor="silver"></td>
</tr>
</table>


</div>

--[[User:Paddy3118|Paddy3118]] ([[User talk:Paddy3118|talk]]) 10:08, 27 March 2019 (UTC)
