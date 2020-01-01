+++
title = "Ramsey's theorem"
description = ""
date = 2019-09-27T16:55:15Z
aliases = []
[extra]
id = 11157
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

;Task:
Find a graph with 17 Nodes such that any 4 Nodes are neither totally connected nor totally unconnected, so demonstrating [[wp:Ramsey's theorem|Ramsey's theorem]].

A specially-nominated solution may be used, but if so it '''must''' be checked to see if if there are any sub-graphs that are totally connected or totally unconnected.





## 360 Assembly

{{trans|C}}

```360asm
*        Ramsey's theorem          19/03/2017
RAMSEY   CSECT
         USING  RAMSEY,R13         base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,NN)       do i=1 to nn
         LR     R1,R6                i
         MH     R1,=AL2(N)           *n
         LR     R0,R6                i
         AR     R1,R0                i*i+i
         SLA    R1,1                 *2
         LA     R0,2                 2
         STH    R0,A-36(R1)          a(i,i)=2
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,=F'8')    do while i<=8
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,NN)         do j=1 to nn
         LR     R8,R7                  j
         AR     R8,R6                  +i
         BCTR   R8,0                   -1
         SRDA   R8,32                  ~
         D      R8,NN                  /nn
         LA     R8,1(R8)               k=((j+i-1) mod nn)+1
         LR     R1,R7                  j
         MH     R1,=AL2(N)             *n
         LR     R0,R8                  k
         AR     R1,R0                  j*n+ki
         SLA    R1,1                   *2
         LA     R0,1                   1
         STH    R0,A-36(R1)            a(j,k)=1
         LR     R1,R8                  k
         MH     R1,=AL2(N)             *n
         LR     R0,R7                  j
         AR     R1,R0                  k*n+j
         SLA    R1,1                   *2
         LA     R0,1                   1
         STH    R0,A-36(R1)            a(k,j)=1
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         AR     R6,R6                i=i+i
       ENDDO    ,                  enddo i
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,NN)       do i=1 to nn
         LA     R7,1                 j=1
         LA     R10,PG               pgi=0
       DO WHILE=(C,R7,LE,NN)         do j=1 to nn
         LR     R1,R6                  i
         MH     R1,=AL2(N)             *n
         LR     R0,R7                  j
         AR     R1,R0                  i*n+j
         SLA    R1,1                   *2
         LH     R4,A-36(R1)            a(i,j)
       IF CH,R4,EQ,=H'2' THEN          if a(i,j)=2 then
         MVC    0(2,R10),=C' -'          output '-'
       ELSE     ,                      else
         XDECO  R4,XDEC                  edit a(i,j)
         MVC    0(2,R10),XDEC+10         output a(i,j)
       ENDIF    ,                      endif
         LA     R10,2(R10)             pgi+=2
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         XPRNT  PG,L'PG              print buffer
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,NN)       do i=1 to nn
         SR     R0,R0                0
         STH    R0,BH                bh=0
         STH    R0,BV                bv=0
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,NN)         do j=1 to nn
         LR     R1,R6                  i
         MH     R1,=AL2(N)             *n
         LR     R0,R7                  j
         AR     R1,R0                  i*n+j
         SLA    R1,1                   *2
         LH     R2,A-36(R1)            a(i,j)
       IF CH,R2,EQ,=H'1' THEN          if a(i,j)=1 then
         LH     R2,BH                    bh
         LA     R2,1(R2)                 +1
         STH    R2,BH                    bh=bh+1
       ENDIF    ,                      endif
         LR     R1,R7                  j
         MH     R1,=AL2(N)             *n
         LR     R0,R6                  i
         AR     R1,R0                  j*n+i
         SLA    R1,1                   *2
         LH     R2,A-36(R1)            a(j,i)
       IF CH,R2,EQ,=H'1' THEN          if a(j,i)=1 then
         LH     R2,BV                    bv
         LA     R2,1(R2)                 +1
         STH    R2,BV                    bv=bv+1
       ENDIF    ,                      endif
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         L      R2,NN                nn
         SRA    R2,1                 /2
         MVI    XX,X'01'             xx=true
       IF CH,R2,NE,BH THEN           if bh<>nn/2 then
         MVI    XX,X'00'               xx=false
       ENDIF    ,                    endif
         NC     OKH,XX               okh=okh and (bh=nn/2)
         L      R2,NN                nn
         SRA    R2,1                 /2
         MVI    XX,X'01'             xx=true
       IF CH,R2,NE,BV THEN           if bv<>nn/2 then
         MVI    XX,X'00'               xx=false
       ENDIF    ,                    endif
         NC     OKV,XX               okv=okv and (bv=nn/2)
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         MVC    XX,OKH             xx=okh
         NC     XX(1),OKV          xx=okh and okv
       IF CLI,XX,EQ,X'01' THEN     if okh and okv then
         MVC    WOK,=CL4'yes'        wok='yes'
       ELSE     ,                  else
         MVC    WOK,=CL4'no'         wok='no'
       ENDIF    ,                  endif
         MVC    PG,=CL80'check='   output 'check='
         MVC    PG+6(L'WOK),WOK    output wok
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            return_code=0
         BR     R14                exit
N        EQU    17                 n=17
NN       DC     A(N)               nn=n
A        DC     (N*N)H'0'          table a(n,n) halfword init 0
BH       DS     H                  count horizontal
BV       DS     H                  count vertical
OKH      DC     X'01'              check horizontal
OKV      DC     X'01'              check vertical
WOK      DS     CL4                temp ok
XX       DS     X                  temp logical
PG       DC     CL80' '            buffer
XDEC     DS     CL12               temp xdeco
         YREGS
         END    RAMSEY
```

{{out}}

```txt

 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -
check=yes

```


## AWK


```AWK

# syntax: GAWK -f RAMSEYS_THEOREM.AWK
# converted from Ring
BEGIN {
    for (i=1; i<=17; i++) {
      arr[i,i] = -1
    }
    k = 1
    while (k <= 8) {
      for (i=1; i<=17; i++) {
        j = (i + k) % 17
        if (j != 0) {
          arr[i,j] = 1
          arr[j,i] = 1
        }
      }
      k = k * 2
    }
    for (i=1; i<=17; i++) {
      for (j=1; j<=17; j++) {
        printf("%s",arr[i,j]+0)
      }
      printf("\n")
    }
    exit(0)
}

```

{{out}}

```txt

-11101000110001011
1-1110100011000101
11-111010001100010
011-11101000110001
1011-1110100011000
01011-111010001100
001011-11101000110
0001011-1110100011
10001011-111010000
110001011-11101000
0110001011-1110100
00110001011-111010
000110001011-11100
1000110001011-1110
01000110001011-110
101000110001011-10
1101000100000000-1

```



## C

For 17 nodes, (4,4) happens to have a special solution: arrange nodes on a circle, and connect all pairs with distances 1, 2, 4, and 8.  It's easier to prove it on paper and just show the result than let a computer find it (you can call it optimization).

No issue with the code or the output, there seems to be a bug with Rosettacode's tag handlers. - aamrun

```c
#include <stdio.h>

int a[17][17], idx[4];

int find_group(int type, int min_n, int max_n, int depth)
{
	int i, n;
	if (depth == 4) {
		printf("totally %sconnected group:", type ? "" : "un");
		for (i = 0; i < 4; i++) printf(" %d", idx[i]);
		putchar('\n');
		return 1;
	}

	for (i = min_n; i < max_n; i++) {
		for (n = 0; n < depth; n++)
			if (a[idx[n]][i] != type) break;

		if (n == depth) {
			idx[n] = i;
			if (find_group(type, 1, max_n, depth + 1))
				return 1;
		}
	}
	return 0;
}

int main()
{
	int i, j, k;
	const char *mark = "01-";

	for (i = 0; i < 17; i++)
		a[i][i] = 2;

	for (k = 1; k <= 8; k <<= 1) {
		for (i = 0; i < 17; i++) {
			j = (i + k) % 17;
			a[i][j] = a[j][i] = 1;
		}
	}

	for (i = 0; i < 17; i++) {
		for (j = 0; j < 17; j++)
			printf("%c ", mark[a[i][j]]);
		putchar('\n');
	}

	// testcase breakage
	// a[2][1] = a[1][2] = 0;

	// it's symmetric, so only need to test groups containing node 0
	for (i = 0; i < 17; i++) {
		idx[0] = i;
		if (find_group(1, i+1, 17, 1) || find_group(0, i+1, 17, 1)) {
			puts("no good");
			return 0;
		}
	}
	puts("all good");
	return 0;
}
```

{{out}} (17 x 17 connectivity matrix):

```txt

- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -
all good

```



## D

{{trans|Tcl}}

```d
import std.stdio, std.string, std.algorithm, std.range;

/// Generate the connectivity matrix.
immutable(char)[][] generateMatrix() {
    immutable r = format("-%b", 53643);
    return r.length.iota.map!(i => r[$-i .. $] ~ r[0 .. $-i]).array;
}

/**Check that every clique of four has at least one pair connected
and one pair unconnected. It requires a symmetric matrix.*/
string ramseyCheck(in char[][] mat) pure @safe
in {
    foreach (immutable r, const row; mat) {
        assert(row.length == mat.length);
        foreach (immutable c, immutable x; row)
            assert(x == mat[c][r]);
    }
} body {
    immutable N = mat.length;
    char[6] connectivity = '-';

    foreach (immutable a; 0 .. N) {
        foreach (immutable b; 0 .. N) {
            if (a == b) continue;
            connectivity[0] = mat[a][b];
            foreach (immutable c; 0 .. N) {
                if (a == c || b == c) continue;
                connectivity[1] = mat[a][c];
                connectivity[2] = mat[b][c];
                foreach (immutable d; 0 .. N) {
                    if (a == d || b == d || c == d) continue;
                    connectivity[3] = mat[a][d];
                    connectivity[4] = mat[b][d];
                    connectivity[5] = mat[c][d];

                    // We've extracted a meaningful subgraph,
                    // check its connectivity.
                    if (!connectivity[].canFind('0'))
                        return format("Fail, found wholly connected: ",
                                      a, " ", b," ", c, " ", d);
                    else if (!connectivity[].canFind('1'))
                        return format("Fail, found wholly " ~
                                      "unconnected: ",
                                      a, " ", b," ", c, " ", d);
                }
            }
        }
    }

    return "Satisfies Ramsey condition.";
}

void main() {
    const mat = generateMatrix;
    writefln("%-(%(%c %)\n%)", mat);
    mat.ramseyCheck.writeln;
}
```

{{out}}

```txt
- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -
Satisfies Ramsey condition.
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Ramsey do
  def main(n\\17) do
    vertices = Enum.to_list(0 .. n-1)
    g = create_graph(n,vertices)
    edges = for v1 <- :digraph.vertices(g), v2 <- :digraph.out_neighbours(g, v1), do: {v1,v2}
    print_graph(vertices,edges)
    case ramsey_check(vertices,edges) do
      true           -> "Satisfies Ramsey condition."
      {false,reason} -> "Not satisfies Ramsey condition:\n#{inspect reason}"
    end
    |> IO.puts
  end

  def create_graph(n,vertices) do
    g = :digraph.new([:cyclic])
    for v <- vertices, do: :digraph.add_vertex(g,v)
    for i <- vertices, k <- [1,2,4,8] do
      j = rem(i + k, n)
      :digraph.add_edge(g, i, j)
      :digraph.add_edge(g, j, i)
    end
    g
  end

  def print_graph(vertices,edges) do
    Enum.each(vertices, fn j ->
      Enum.map_join(vertices, " ", fn i ->
        cond do
          i==j           -> "-"
          {i,j} in edges -> "1"
          true           -> "0"
        end
      end)
      |> IO.puts
    end)
  end

  def ramsey_check(vertices,edges) do
    listconditions =
      for v1 <- vertices, v2 <- vertices, v3 <- vertices, v4 <- vertices,
          v1 != v2, v1 != v3, v1 != v4, v2 != v3, v2 != v4, v3 != v4
          do
            all_cases = [ {v1,v2} in edges, {v1,v3} in edges, {v1,v4} in edges,
                          {v2,v3} in edges, {v2,v4} in edges, {v3,v4} in edges ]
            {v1, v2, v3, v4, Enum.any?(all_cases), not(Enum.all?(all_cases))}
          end
    if Enum.all?(listconditions, fn {_,_,_,_,c1,c2} -> c1 and c2 end) do
      true
    else
      {false, (for {v1,v2,v3,v4,false,_} <- listconditions, do: {:wholly_unconnected,v1,v2,v3,v4})
           ++ (for {v1,v2,v3,v4,_,false} <- listconditions, do: {:wholly_connected,v1,v2,v3,v4}) }
    end
  end
end

Ramsey.main
```


{{out}}

```txt

- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -
Satisfies Ramsey condition.

```



## FreeBASIC

{{trans|Ring}}
{{trans|Go}}

```freebasic

Dim Shared As Integer i, j, k = 1
Dim Shared As Integer a(17,17), idx(4)
For i = 0 To 17
    a(i,i) = 2
Next i

Function EncontrarGrupo(tipo As Integer, min As Integer, max As Integer, fondo As Integer) As Boolean
    If fondo = 0 Then
        Dim As String c = ""
        If tipo = 0 Then c = "des"
        Print Using "Grupo totalmente &conectado:"; c
        For i = 0 To 4
            Print " " & idx(i)
        Next i
        Print
        Return true
    End If

    For i = min To max
        k = 0
        For j = k To fondo
            If a(idx(k),i) <> tipo Then Exit For
        Next j

        If k = fondo Then
            idx(k) = i
            If EncontrarGrupo(tipo, 1, max, fondo+1) Then Return true
        End If
    Next i
    Return false
End Function

While k <= 8
    For i = 1 To 17
        j = (i + k) Mod 17
        If j <> 0 Then
            a(i,j) = 1 : a(j,i) = 1
        End If
    Next i
    k *= 2
Wend
For i = 1 To 17
    For j = 1 To 17
        If a(i,j) = 2 Then
            Print "- ";
        Else
            Print a(i,j) & " ";
        End If
    Next j
    Print
Next i

' Es simétrico, por lo que solo necesita probar grupos que contengan el nodo 0.
For i = 0 To 17
    idx(0) = i
    If EncontrarGrupo(1, i+1, 17, 1) Or EncontrarGrupo(0, i+1, 17, 1) Then
        Print Chr(10) & "No satisfecho."
        Exit For
    End If
Next i
Print Chr(10) & "Satisface el teorema de Ramsey."
End

```

{{out}}

```txt

- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 0
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 0
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 0
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 0
1 1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 -

Satisface el teorema de Ramsey.

```



## Go

{{trans|C}}

```go
package main

import "fmt"

var (
    a   [17][17]int
    idx [4]int
)

func findGroup(ctype, min, max, depth int) bool {
    if depth == 4 {
        cs := ""
        if ctype == 0 {
            cs = "un"
        }
        fmt.Printf("Totally %sconnected group:", cs)
        for i := 0; i < 4; i++ {
            fmt.Printf(" %d", idx[i])
        }
        fmt.Println()
        return true
    }

    for i := min; i < max; i++ {
        n := 0
        for ; n < depth; n++ {
            if a[idx[n]][i] != ctype {
                break
            }
        }

        if n == depth {
            idx[n] = i
            if findGroup(ctype, 1, max, depth+1) {
                return true
            }
        }
    }
    return false
}

func main() {
    const mark = "01-"

    for i := 0; i < 17; i++ {
        a[i][i] = 2
    }

    for k := 1; k <= 8; k <<= 1 {
        for i := 0; i < 17; i++ {
            j := (i + k) % 17
            a[i][j], a[j][i] = 1, 1
        }
    }

    for i := 0; i < 17; i++ {
        for j := 0; j < 17; j++ {
            fmt.Printf("%c ", mark[a[i][j]])
        }
        fmt.Println()
    }

    // Test case breakage
    // a[2][1] = a[1][2] = 0

    // It's symmetric, so only need to test groups containing node 0.
    for i := 0; i < 17; i++ {
        idx[0] = i
        if findGroup(1, i+1, 17, 1) || findGroup(0, i+1, 17, 1) {
            fmt.Println("No good.")
            return
        }
    }
    fmt.Println("All good.")
}
```


{{out}}

```txt

- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -
All good.

```



## Erlang

{{trans|C}} {{libheader|Erlang digraph}}

```erlang
-module(ramsey_theorem).
-export([main/0]).

main() ->
	Vertices = lists:seq(0,16),
	G = create_graph(Vertices),
	String_ramsey =
		case ramsey_check(G,Vertices) of
			true ->
				"Satisfies Ramsey condition.";
			{false,Reason} ->
				"Not satisfies Ramsey condition:\n"
				++ io_lib:format("~p\n",[Reason])
		end,
	io:format("~s\n~s\n",[print_graph(G,Vertices),String_ramsey]).

create_graph(Vertices) ->
	G = digraph:new([cyclic]),
	[digraph:add_vertex(G,V) || V <- Vertices],
	[begin
		J = ((I + K) rem 17),
		digraph:add_edge(G, I, J),
		digraph:add_edge(G, J, I)
	 end || I <- Vertices, K <- [1,2,4,8]],
	G.

print_graph(G,Vertices) ->
	Edges =
		[{V1,V2} ||
			V1 <- digraph:vertices(G),
			V2 <- digraph:out_neighbours(G, V1)],
	lists:flatten(
		[[
			[case I of
			 	J ->
			 		$-;
			 	_ ->
			 		case lists:member({I,J},Edges) of
			 			true -> $1;
			 			false -> $0
			 		end
			 end,$ ]
		 || I <- Vertices] ++ [$\n] || J <- Vertices]).

ramsey_check(G,Vertices) ->
	Edges =
		[{V1,V2} ||
			V1 <- digraph:vertices(G),
			V2 <- digraph:out_neighbours(G, V1)],
	ListConditions =
		[begin
			All_cases =
				[lists:member({V1,V2},Edges),
				 lists:member({V1,V3},Edges),
				 lists:member({V1,V4},Edges),
				 lists:member({V2,V3},Edges),
				 lists:member({V2,V4},Edges),
				 lists:member({V3,V4},Edges)],
			{V1,V2,V3,V4,
			 lists:any(fun(X) -> X end, All_cases),
			 not(lists:all(fun(X) -> X end, All_cases))}
		end
		 || V1 <- Vertices, V2 <- Vertices, V3 <- Vertices, V4 <- Vertices,
		 	V1/=V2,V1/=V3,V1/=V4,V2/=V3,V2/=V4,V3/=V4],
	case lists:all(fun({_,_,_,_,C1,C2}) -> C1 and C2 end,ListConditions) of
		true -> true;
		false ->
			{false,
				[{wholly_unconnected,V1,V2,V3,V4}
				 || {V1,V2,V3,V4,false,_} <- ListConditions]
				++ [{wholly_connected,V1,V2,V3,V4}
				 || {V1,V2,V3,V4,_,false} <- ListConditions]}
	end.
```

{{out}}

```txt
- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -

Satisfies Ramsey condition.
```



## J

Interpreting this task as "reproduce the output of all the other examples", then here's a stroll to the goal through the J interpreter:
```j
   i.@<.&.(2&^.) N =: 17                                           NB.  Count to N by powers of 2
1 2 4 8
   1 #~ 1 j. 0 _1:} i.@<.&.(2&^.) N =: 17                          NB.  Turn indices into bit mask
1 0 1 0 0 1 0 0 0 0 1
   (, |.) 1 #~ 1 j. 0 _1:} i.@<.&.(2&^.) N =: 17                   NB.  Cat the bitmask with its own reflection
1 0 1 0 0 1 0 0 0 0 1 1 0 0 0 0 1 0 0 1 0 1
   _1 |.^:(<N) _ , (, |.) 1 #~ 1 j. 0 _1:} <: i.@<.&.(2&^.) N=:17  NB.  Then rotate N times to produce the array
_ 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 _ 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 _ 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 _ 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 _ 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 _ 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 _ 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 _ 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 _ 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 _ 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 _ 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 _ 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 _ 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 _ 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 _ 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 _ 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 _

   NB. Packaged up as a re-usable function
   ramsey =: _1&|.^:((<@])`(_ , [: (, |.) 1 #~ 1 j. 0 _1:} [: <: i.@<.&.(2&^.)@]))

   ramsey 17
_ 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 _ 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 _ 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 _ 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 _ 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 _ 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 _ 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 _ 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 _ 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 _ 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 _ 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 _ 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 _ 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 _ 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 _ 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 _ 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 _
```


To test if all combinations of 4 rows and columns contain both a 0 and a 1

```j

   comb=: 4 : 0 M.   NB. All size x combinations of i.y
     if. (x>:y)+.0=x do. i.(x<:y),x else. (0,.x comb&.<: y),1+x comb y-1 end.
   )

   NB. returns 1 iff the subbmatrix of y consisting of the columns and rows labelled x contains both 1 and 0
   checkRow =. 4 : 0 "1 _
     *./ 0 1 e. ,x{"1 x{y
   )

   *./ (4 comb 17) checkRow ramsey 17
1

```



## Java

Translation of Tcl via D
{{works with|Java|8}}

```java
import java.util.Arrays;
import java.util.stream.IntStream;

public class RamseysTheorem {

    static char[][] createMatrix() {
        String r = "-" + Integer.toBinaryString(53643);
        int len = r.length();
        return IntStream.range(0, len)
                .mapToObj(i -> r.substring(len - i) + r.substring(0, len - i))
                .map(String::toCharArray)
                .toArray(char[][]::new);
    }

    /**
     * Check that every clique of four has at least one pair connected and one
     * pair unconnected. It requires a symmetric matrix.
     */
    static String ramseyCheck(char[][] mat) {
        int len = mat.length;
        char[] connectivity = "------".toCharArray();

        for (int a = 0; a < len; a++) {
            for (int b = 0; b < len; b++) {
                if (a == b)
                    continue;
                connectivity[0] = mat[a][b];
                for (int c = 0; c < len; c++) {
                    if (a == c || b == c)
                        continue;
                    connectivity[1] = mat[a][c];
                    connectivity[2] = mat[b][c];
                    for (int d = 0; d < len; d++) {
                        if (a == d || b == d || c == d)
                            continue;
                        connectivity[3] = mat[a][d];
                        connectivity[4] = mat[b][d];
                        connectivity[5] = mat[c][d];

                        // We've extracted a meaningful subgraph,
                        // check its connectivity.
                        String conn = new String(connectivity);
                        if (conn.indexOf('0') == -1)
                            return String.format("Fail, found wholly connected: "
                                    + "%d %d %d %d", a, b, c, d);
                        else if (conn.indexOf('1') == -1)
                            return String.format("Fail, found wholly unconnected: "
                                    + "%d %d %d %d", a, b, c, d);
                    }
                }
            }
        }
        return "Satisfies Ramsey condition.";
    }

    public static void main(String[] a) {
        char[][] mat = createMatrix();
        for (char[] s : mat)
            System.out.println(Arrays.toString(s));
        System.out.println(ramseyCheck(mat));
    }
}
```



```txt
[-, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1]
[1, -, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1]
[1, 1, -, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0]
[0, 1, 1, -, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1]
[1, 0, 1, 1, -, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0]
[0, 1, 0, 1, 1, -, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0]
[0, 0, 1, 0, 1, 1, -, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0]
[0, 0, 0, 1, 0, 1, 1, -, 1, 1, 0, 1, 0, 0, 0, 1, 1]
[1, 0, 0, 0, 1, 0, 1, 1, -, 1, 1, 0, 1, 0, 0, 0, 1]
[1, 1, 0, 0, 0, 1, 0, 1, 1, -, 1, 1, 0, 1, 0, 0, 0]
[0, 1, 1, 0, 0, 0, 1, 0, 1, 1, -, 1, 1, 0, 1, 0, 0]
[0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, -, 1, 1, 0, 1, 0]
[0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, -, 1, 1, 0, 1]
[1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, -, 1, 1, 0]
[0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, -, 1, 1]
[1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, -, 1]
[1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, -]
Satisfies Ramsey condition.
```



## Kotlin

{{trans|C}}

```scala
// version 1.1.0

val a = Array(17) { IntArray(17) }
val idx = IntArray(4)

fun findGroup(type: Int, minN: Int, maxN: Int, depth: Int): Boolean {
    if (depth == 4) {
        print("\nTotally ${if (type != 0) "" else "un"}connected group:")
        for (i in 0 until 4) print(" ${idx[i]}")
        println()
        return true
    }

    for (i in minN until maxN) {
        var n = depth
        for (m in 0 until depth) if (a[idx[m]][i] != type) {
            n = m
            break
        }
        if (n == depth) {
            idx[n] = i
            if (findGroup(type, 1, maxN, depth + 1)) return true
        }
    }
    return false
}

fun main(args: Array<String>) {
    for (i in 0 until 17) a[i][i] = 2
    var j: Int
    var k = 1
    while (k <= 8) {
        for (i in 0 until 17) {
            j = (i + k) % 17
            a[i][j] = 1
            a[j][i] = 1
        }
        k = k shl 1
    }
    val mark = "01-"
    for (i in 0 until 17) {
        for (m in 0 until 17) print("${mark[a[i][m]]} ")
        println()
    }
    for (i in 0 until 17) {
        idx[0] = i
        if (findGroup(1, i + 1, 17, 1) || findGroup(0, i + 1, 17, 1)) {
            println("\nRamsey condition not satisfied.")
            return
        }
    }
    println("\nRamsey condition satisfied.")
}
```


{{out}}

```txt

- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -

Ramsey condition satisfied.

```



## Mathematica

{{needs-review|C|The task has been changed to also require demonstrating that the graph is a solution.}}

```mathematica
CirculantGraph[17, {1, 2, 4, 8}]
```

[[File:Ramsey.png]]


## Mathprog

{{lines too long|Mathprog}}
<lang>/*Ramsey 4 4 17

  This model finds a graph with 17 Nodes such that no clique of 4 Nodes is either fully
  connected, nor fully disconnected

  Nigel_Galloway
  January 18th., 2012
*/
param Nodes := 17;
var Arc{1..Nodes, 1..Nodes}, binary;

clique{a in 1..(Nodes-3), b in (a+1)..(Nodes-2), c in (b+1)..(Nodes-1), d in (c+1)..Nodes} : 1 <= Arc[a,b] + Arc[a,c] + Arc[a,d] + Arc[b,c] + Arc[b,d] + Arc[c,d] <= 5;

end;
```


This may be run with:

```bash
glpsol --minisat --math R_4_4_17.mprog --output R_4_4_17.sol
```

The solution may be viewed on [[Solution Ramsey Mathprog|this page]].
In the solution file, the first section identifies the number of nodes connected in this clique. In the second part of the solution, the status of each arc in the graph (connected=<tt>1</tt>, unconnected=<tt>0</tt>) is shown.


## PARI/GP

This takes the [[#C|C]] solution to its logical extreme.

```parigp


check(M)={
  my(n=#M);
  for(a=1,n-3,
    for(b=a+1,n-2,
      my(goal=!M[a,b]);
      for(c=b+1,n-1,
        if(M[a,c]==goal || M[b,c]==goal, next(2));
        for(d=c+1,n,
          if(M[a,d]==goal || M[b,d]==goal || M[c,d]==goal, next(3));
        )
      );
      print(a" "b);
      return(0)
    )
  );
  1
};

M=matrix(17,17,x,y,my(t=abs(x-y)%17);t==2^min(valuation(t,2),3))
check(M)
```



## Perl

{{trans|Perl 6}}
{{libheader|ntheory}}

```perl
use ntheory qw(forcomb);
use Math::Cartesian::Product;

$n = 17;
push @a, [(0) x $n] for 0..$n-1;
$a[$_][$_] = '-' for 0..$n-1;

for $x (cartesian {@_} [(0..$n-1)], [(1,2,4,8)]) {
    $i = @$x[0];
    $k = @$x[1];
    $j = ($i + $k) % $n;
    $a[$i][$j] = $a[$j][$i] = 1;
}

forcomb {
    my $l = 0;
    @i = @_;
    forcomb { $l += $a[ $i[$_[0]] ][ $i[$_[1]] ]; } (4,2);
    die "Bogus!" unless 0 < $l and $l < 6;
} ($n,4);

print join(' ' ,@$_) . "\n" for @a;
print 'OK'
```

{{out}}

```txt
- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -
OK
```



## Perl 6

{{Works with|rakudo|2018.08}}

```perl6
my $n = 17;
my @a = [ 0 xx $n ] xx $n;
@a[$_;$_] = '-' for ^$n;

for flat ^$n X 1,2,4,8 -> $i, $k {
    my $j = ($i + $k) % $n;
    @a[$i;$j] = @a[$j;$i] = 1;
}
.say for @a;

for combinations($n,4) -> $quartet {
    my $links = [+] $quartet.combinations(2).map: -> $i,$j { @a[$i;$j] }
    die "Bogus!" unless 0 < $links < 6;
}
say "OK";
```

{{out}}

```txt
- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -
OK
```



## Phix

{{trans|Go}}

```Phix
sequence a = repeat(repeat('0',17),17),
         idx = repeat(0,4)

function findGroup(integer ch, lo, hi, depth)
    if depth == 4 then
        string cs = iff(ch='1'?"":"un")
        printf(1,"Totally %sconnected group:%s\n", {cs,sprint(idx)})
        return true
    end if

    for i=lo to hi do
        bool all_same = true
        for n=1 to depth do
            if a[idx[n]][i] != ch then
                all_same = false
                exit
            end if
        end for
        if all_same then
            idx[depth+1] = i
            if findGroup(ch, 1, hi, depth+1) then
                return true
            end if
        end if
    end for
    return false
end function

for i=1 to 17 do
    a[i][i] = '-'
end for

integer k = 1
while k<=8 do
    for i=1 to 17 do
        integer j = mod(i-1+k,17)+1
        {a[i][j], a[j][i]} = "11"
    end for
    k *= 2
end while

-- Test case breakage
--{a[2][1],a[1][2]} @= '0'

puts(1,join(a,'\n')&"\n\n")

bool all_good = true
for i=1 to 17 do
    idx[1] = i
    if findGroup('1', i+1, 17, 1)
    or findGroup('0', i+1, 17, 1) then
        all_good = false
        exit
    end if
end for
printf(1,iff(all_good?"Satisfies Ramsey condition.\n":"No good.\n"))
```

{{out}}

```txt

-1101000110001011
1-110100011000101
11-11010001100010
011-1101000110001
1011-110100011000
01011-11010001100
001011-1101000110
0001011-110100011
10001011-11010001
110001011-1101000
0110001011-110100
00110001011-11010
000110001011-1101
1000110001011-110
01000110001011-11
101000110001011-1
1101000110001011-

Satisfies Ramsey condition.

```



## Python


{{works with|Python|3.4.1}}
{{trans|C}}


```python
range17 = range(17)
a = [['0'] * 17 for i in range17]
idx = [0] * 4


def find_group(mark, min_n, max_n, depth=1):
    if (depth == 4):
        prefix = "" if (mark == '1') else "un"
        print("Fail, found totally {}connected group:".format(prefix))
        for i in range(4):
            print(idx[i])
        return True

    for i in range(min_n, max_n):
        n = 0
        while (n < depth):
            if (a[idx[n]][i] != mark):
                break
            n += 1

        if (n == depth):
            idx[n] = i
            if (find_group(mark, 1, max_n, depth + 1)):
                return True

    return False


if __name__ == '__main__':
    for i in range17:
        a[i][i] = '-'
    for k in range(4):
        for i in range17:
            j = (i + pow(2, k)) % 17
            a[i][j] = a[j][i] = '1'

    # testcase breakage
    # a[2][1] = a[1][2] = '0'

    for row in a:
        print(' '.join(row))

    for i in range17:
        idx[0] = i
        if (find_group('1', i + 1, 17) or find_group('0', i + 1, 17)):
            print("no good")
            exit()

    print("all good")
```


{{out|Output same as C}}


## Racket


{{output?|Racket|
}}

{{incorrect|Racket|The task has been changed to also require demonstrating that the graph is a solution.}}

Kind of a translation of C (ie, reducing this problem to generating a printout of a specific matrix).

```racket
#lang racket

(define N 17)

(define (dist i j)
  (define d (abs (- i j)))
  (if (<= d (quotient N 2)) d (- N d)))

(define v
  (build-vector N
    (λ(i) (build-vector N
            (λ(j) (case (dist i j) [(0) '-] [(1 2 4 8) 1] [else 0]))))))

(for ([row v]) (displayln row))
```



## REXX

Mainline programming was borrowed from   '''C'''.

```rexx
/*REXX program finds and displays a 17 node graph such that any four nodes are neither  */
/*─────────────────────────────────────────── totally connected nor totally unconnected.*/
@.=0;             #=17                           /*initialize the node graph to zero.   */
      do d=0  for #;  @.d.d=2;  end  /*d*/       /*set the diagonal elements to two.    */

      do k=1  by 0  while k<=8                   /*K  is doubled each time through loop.*/
            do i=0  for #;      j= (i+k) // #    /*set a  row,column  and  column,row.  */
            @.i.j=1;            @.j.i=1          /*set two array elements to unity.     */
            end   /*i*/
      k=k+k                                      /*double the value of  K  for each loop*/
      end         /*k*/
                                                 /* [↓]  display a connection grid.     */
      do r=0  for #;  _=;       do c=0  for #    /*build rows;  build column by column. */
                                _=_  @.r.c       /*add  (append)  the column to the row.*/
                                end   /*c*/

      say left('', 9)     translate(_, "-", 2)   /*display the constructed row.         */
      end   /*r*/
                                                 /*verify the sub-graphs connections.   */
!.=0;      ok=1                                  /*Ramsey's connections;   OK  (so far).*/
                                                 /* [↓]  check col. with row connections*/
      do   v=0  for #                            /*check the sub-graphs # of connections*/
        do h=0  for #                            /*check column connections to the rows.*/
        if @.v.h==1  then !._v.v= !._v.v + 1     /*if connected,  then bump the counter.*/
        end   /*h*/                              /* [↑]   Note:  we're counting each    */
      ok=ok  &  !._v.v==# % 2                    /*       connection twice,  so divide  */
      end     /*v*/                              /*       the total by two.             */
                                                 /* [↓]  check col. with row connections*/
      do   h=0  for #                            /*check the sub-graphs # of connections*/
        do v=0  for #                            /*check the row connection to a column.*/
        if @.h.v==1  then !._h.h= !._h.h + 1     /*if connected,  then bump the counter.*/
        end   /*v*/                              /* [↑]   Note:  we're counting each    */
      ok=ok  &  !._h.h==# % 2                    /*       connection twice, so divide   */
      end     /*h*/                              /*       the total by two.             */
say                                              /*stick a fork in it,  we're all done. */
say  space("Ramsey's condition is"   word('not', 1+ok)   "satisfied.")     /*yea─or─nay.*/
```

{{out|output|text=  ('''17x17''' connectivity matrix):}}

```txt

           - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
           1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
           1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
           0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
           1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
           0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
           0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
           0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
           1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
           1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
           0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
           0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
           0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
           1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
           0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
           1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
           1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -

Ramsey's condition is satisfied.

```



## Ring


```ring

# Project : Ramsey's theorem

load "stdlib.ring"

a = newlist(17,17)
for i = 1 to 17
    a[i][i] = -1
next
k = 1
while k <= 8
      for i = 1 to 17
          j = (i + k) % 17
          if j != 0
             a[i][j] = 1
             a[j][i] = 1
          ok
      next
      k = k * 2
end
for i = 1 to 17
    for j = 1 to 17
        see a[i][j] + " "
    next
    see nl
next

```

Output:

```txt

-11101000110001011
1-1110100011000101
11-111010001100010
011-11101000110001
1011-1110100011000
01011-111010001100
001011-11101000110
0001011-1110100011
10001011-111010000
110001011-11101000
0110001011-1110100
00110001011-111010
000110001011-11100
1000110001011-1110
01000110001011-110
101000110001011-10
1101000100000000-1

```



## Ruby


```ruby
a = Array.new(17){['0'] * 17}
17.times{|i| a[i][i] = '-'}
4.times do |k|
  17.times do |i|
    j = (i + 2 ** k) % 17
    a[i][j] = a[j][i] = '1'
  end
end
a.each {|row| puts row.join(' ')}
# check taken from Perl6 version
(0...17).to_a.combination(4) do |quartet|
  links = quartet.combination(2).map{|i,j| a[i][j].to_i}.reduce(:+)
  abort "Bogus" unless 0 < links && links < 6
end
puts "Ok"

```

{{out}}

```txt

- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -
Ok

```



## Run BASIC

{{incorrect|Run BASIC|The task has been changed to also require demonstrating that the graph is a solution.}}

```runbasic
dim a(17,17)
for i = 1 to 17: a(i,i) = -1: next i
k = 1
while k <= 8
  for i = 1 to 17
    j = (i + k) mod 17
    a(i,j) = 1
    a(j,i) = 1
  next i
  k = k * 2
wend
for i = 1 to 17
  for j = 1 to 17
    print a(i,j);" ";
  next j
  print
next i
```


```txt
-1 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 -1 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 -1 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 -1 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 -1 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 -1 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 -1 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 -1 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 -1 1 1 0 1 0 0 0 0
1 1 0 0 0 1 0 1 1 -1 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 -1 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 -1 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 -1 1 1 0 0
1 0 0 0 1 1 0 0 0 1 0 1 1 -1 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 -1 1 0
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -1 0
1 1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 -1
```



## Sidef

{{trans|Ruby}}

```ruby
var a = 17.of { 17.of(0) }

17.times {|i| a[i][i] = '-' }
4.times { |k|
  17.times { |i|
    var j = ((i + 1<<k) % 17)
    a[i][j] = (a[j][i] = 1)
  }
}

a.each {|row| say row.join(' ') }

combinations(17, 4, { |*quartet|
  var links = quartet.combinations(2).map{|p| a.dig(p...) }.sum
  ((0 < links) && (links < 6)) || die "Bogus!"
})
say "Ok"
```

{{out}}

```txt

- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -
Ok

```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

# Generate the connectivity matrix
set init [split [format -%b 53643] ""]
set matrix {}
for {set r $init} {$r ni $matrix} {set r [concat [lindex $r end] [lrange $r 0 end-1]]} {
    lappend matrix $r
}

# Check that every clique of four has at least *one* pair connected and one
# pair unconnected. ASSUMES that the graph is symmetric.
proc ramseyCheck4 {matrix} {
    set N [llength $matrix]
    set connectivity [lrepeat 6 -]
    for {set a 0} {$a < $N} {incr a} {
	for {set b 0} {$b < $N} {incr b} {
	    if {$a==$b} continue
	    lset connectivity 0 [lindex $matrix $a $b]
	    for {set c 0} {$c < $N} {incr c} {
		if {$a==$c || $b==$c} continue
		lset connectivity 1 [lindex $matrix $a $c]
		lset connectivity 2 [lindex $matrix $b $c]
		for {set d 0} {$d < $N} {incr d} {
		    if {$a==$d || $b==$d || $c==$d} continue
		    lset connectivity 3 [lindex $matrix $a $d]
		    lset connectivity 4 [lindex $matrix $b $d]
		    lset connectivity 5 [lindex $matrix $c $d]

		    # We've extracted a meaningful subgraph; check its connectivity
		    if {0 ni $connectivity} {
			puts "FAIL! Found wholly connected: $a $b $c $d"
			return
		    } elseif {1 ni $connectivity} {
			puts "FAIL! Found wholly unconnected: $a $b $c $d"
			return
		    }
		}
	    }
	}
    }
    puts "Satisfies Ramsey condition"
}

puts [join $matrix \n]
ramseyCheck4 $matrix
```

{{out}}

```txt

- 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1
1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0 1
1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1 0
0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0 1
1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0 0
0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0 0
0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1 0
0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1 1
1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0 1
1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0 0
0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0 0
0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1 0
0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0 1
1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1 0
0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1 1
1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 - 1
1 1 0 1 0 0 0 1 1 0 0 0 1 0 1 1 -
Satisfies Ramsey condition

```

