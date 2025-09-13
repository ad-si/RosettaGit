+++
title = "Resistor mesh"
description = ""
date = 2019-07-10T11:34:22Z
aliases = []
[extra]
id = 10354
[taxonomies]
categories = ["task"]
tags = []
+++

{{task}} [[Category:Electronics]]
[[image:resistor-mesh.svg|300px||right]]

## Task

Given   <big> 10&times;10 </big>   grid nodes   (as shown in the image)   interconnected by   <big> 1Ω </big>   resistors as shown,

find the resistance between points   '''A'''   and   '''B'''.


## See also

*   (humor, nerd sniping)   [http://xkcd.com/356/ xkcd.com cartoon]





## Ada

```Ada
with Ada.Text_IO; use Ada.Text_IO;
procedure ResistMesh is
   H, W : constant Positive := 10;
   rowA, colA : constant Positive := 2; -- row/col indexed from 1
   rowB : constant Positive := 7;
   colB : constant Positive := 8;

   type Ntype is (A, B, Free);
   type Vtype is digits 15;
   type Node is record
      volt : Vtype := 0.0;
      name : Ntype := Free;
   end record;
   type NodeMesh is array (Positive range <>, Positive range <>) of Node;
   package IIO is new Ada.Text_IO.Float_IO (Vtype);
   mesh, dmesh : NodeMesh (1 .. H, 1 .. W);
   curA, curB, diff : Vtype;

   procedure set_AB (mesh : in out NodeMesh) is begin
      mesh (rowA, colA).volt :=  1.0;  mesh (rowA, colA).name := A;
      mesh (rowB, colB).volt := -1.0;  mesh (rowB, colB).name := B;
   end set_AB;

   function sides (i : Positive; j : Positive) return Vtype is
      s : Integer := 0;
   begin
      if i /= 1 and i /= H then s := s + 2; else s := s + 1; end if;
      if j /= 1 and j /= W then s := s + 2; else s := s + 1; end if;
      return Vtype (s);
   end sides;

   procedure calc_diff (mesh : NodeMesh; dmesh : out NodeMesh;
      total : out Vtype)  is
      n : Natural;
      v : Vtype := 0.0;
   begin
      total := 0.0;
      for i in Positive range 1 .. H loop
         for j in Positive range 1 .. W loop
            n := 0;    v := 0.0;
            if  i /= 1 then v := v + mesh (i - 1, j).volt; n := n + 1; end if;
            if j /= 1 then  v := v + mesh (i, j - 1).volt; n := n + 1; end if;
            if i < H then v := v + mesh (i + 1, j).volt; n := n + 1; end if;
            if j < W then v := v + mesh (i, j + 1).volt; n := n + 1; end if;
            v := mesh (i, j).volt - v / Vtype (n);
            dmesh (i, j).volt := v;
            if mesh (i, j).name = Free then total := total + v ** 2; end if;
         end loop;
      end loop;
   end calc_diff;

begin

   loop
      set_AB (mesh);
      calc_diff (mesh, dmesh, diff);
      exit when diff < 1.0e-40;
      for i in Positive range 1 .. H loop
         for j in Positive range 1 .. W loop
            mesh (i, j).volt := mesh (i, j).volt - dmesh (i, j).volt;
         end loop;
      end loop;
   end loop;

   curA := dmesh (rowA, colA).volt * sides (rowA, colA);
   curB := dmesh (rowB, colB).volt * sides (rowB, colB);
   diff := 4.0 / (curA - curB);
   IIO.Put (diff, Exp => 0); New_Line;
end ResistMesh;
```

```txt
 1.60899124173073
```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"ARRAYLIB"
      *FLOAT 64
      @% = &F0F

      PRINT "Resistance = "; FNresistormesh(10, 10, 1, 1, 7, 6) " ohms"
      END

      DEF FNresistormesh(ni%, nj%, ai%, aj%, bi%, bj%)
      LOCAL c%, i%, j%, k%, n%, A(), B()
      n% = ni% * nj%
      DIM A(n%-1, n%-1), B(n%-1, 0)
      FOR i% = 0 TO ni%-1
        FOR j% = 0 TO nj%-1
          k% = i% * nj% + j%
          IF i% = ai% AND j% = aj% THEN
            A(k%, k%) = 1
          ELSE
            c% = 0
            IF (i% + 1) < ni% c% += 1 : A(k%, k% + nj%) = -1
            IF i% > 0         c% += 1 : A(k%, k% - nj%) = -1
            IF (j% + 1) < nj% c% += 1 : A(k%, k% + 1)   = -1
            IF j% > 0         c% += 1 : A(k%, k% - 1)   = -1
            A(k%, k%) = c%
          ENDIF
        NEXT
      NEXT
      k% = bi% * nj% + bj%
      B(k%, 0) = 1
      PROC_invert(A())
      B() = A().B()
      = B(k%, 0)
```

```txt
Resistance = 1.60899124173071 ohms
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

#define S 10
typedef struct { double v; int fixed; } node;

#define each(i, x) for(i = 0; i < x; i++)
node **alloc2(int w, int h)
{
	int i;
	node **a = calloc(1, sizeof(node*)*h + sizeof(node)*w*h);
	each(i, h) a[i] = i ? a[i-1] + w : (node*)(a + h);
	return a;
}

void set_boundary(node **m)
{
	m[1][1].fixed =  1; m[1][1].v =  1;
	m[6][7].fixed = -1; m[6][7].v = -1;
}

double calc_diff(node **m, node **d, int w, int h)
{
	int i, j, n;
	double v, total = 0;
	each(i, h) each(j, w) {
		v = 0; n = 0;
		if (i) v += m[i-1][j].v, n++;
		if (j) v += m[i][j-1].v, n++;
		if (i+1 < h) v += m[i+1][j].v, n++;
		if (j+1 < w) v += m[i][j+1].v, n++;

		d[i][j].v = v = m[i][j].v - v / n;
		if (!m[i][j].fixed) total += v * v;
	}
	return total;
}

double iter(node **m, int w, int h)
{
	node **d = alloc2(w, h);
	int i, j;
	double diff = 1e10;
	double cur[] = {0, 0, 0};

	while (diff > 1e-24) {
		set_boundary(m);
		diff = calc_diff(m, d, w, h);
		each(i,h) each(j, w) m[i][j].v -= d[i][j].v;
	}

	each(i, h) each(j, w)
		cur[ m[i][j].fixed + 1 ] += d[i][j].v *
				(!!i + !!j + (i < h-1) + (j < w -1));

	free(d);
	return (cur[2] - cur[0])/2;
}

int main()
{
	node **mesh = alloc2(S, S);
	printf("R = %g\n", 2 / iter(mesh, S, S));
	return 0;
}
```



## C++

```cpp
#include <iomanip>
#include <iostream>
#include <vector>

class Node {
private:
    double v;
    int fixed;

public:
    Node() : v(0.0), fixed(0) {
        // empty
    }

    Node(double v, int fixed) : v(v), fixed(fixed) {
        // empty
    }

    double getV() const {
        return v;
    }

    void setV(double nv) {
        v = nv;
    }

    int getFixed() const {
        return fixed;
    }

    void setFixed(int nf) {
        fixed = nf;
    }
};

void setBoundary(std::vector<std::vector<Node>>& m) {
    m[1][1].setV(1.0);
    m[1][1].setFixed(1);

    m[6][7].setV(-1.0);
    m[6][7].setFixed(-1);
}

double calculateDifference(const std::vector<std::vector<Node>>& m, std::vector<std::vector<Node>>& d, const int w, const int h) {
    double total = 0.0;
    for (int i = 0; i < h; ++i) {
        for (int j = 0; j < w; ++j) {
            double v = 0.0;
            int n = 0;
            if (i > 0) {
                v += m[i - 1][j].getV();
                n++;
            }
            if (j > 0) {
                v += m[i][j - 1].getV();
                n++;
            }
            if (i + 1 < h) {
                v += m[i + 1][j].getV();
                n++;
            }
            if (j + 1 < w) {
                v += m[i][j + 1].getV();
                n++;
            }
            v = m[i][j].getV() - v / n;
            d[i][j].setV(v);
            if (m[i][j].getFixed() == 0) {
                total += v * v;
            }
        }
    }
    return total;
}

double iter(std::vector<std::vector<Node>>& m, const int w, const int h) {
    using namespace std;
    vector<vector<Node>> d;
    for (int i = 0; i < h; ++i) {
        vector<Node> t(w);
        d.push_back(t);
    }

    double curr[] = { 0.0, 0.0, 0.0 };
    double diff = 1e10;

    while (diff > 1e-24) {
        setBoundary(m);
        diff = calculateDifference(m, d, w, h);
        for (int i = 0; i < h; ++i) {
            for (int j = 0; j < w; ++j) {
                m[i][j].setV(m[i][j].getV() - d[i][j].getV());
            }
        }
    }

    for (int i = 0; i < h; ++i) {
        for (int j = 0; j < w; ++j) {
            int k = 0;
            if (i != 0) ++k;
            if (j != 0) ++k;
            if (i < h - 1) ++k;
            if (j < w - 1) ++k;
            curr[m[i][j].getFixed() + 1] += d[i][j].getV()*k;
        }
    }

    return (curr[2] - curr[0]) / 2.0;
}

const int S = 10;
int main() {
    using namespace std;
    vector<vector<Node>> mesh;

    for (int i = 0; i < S; ++i) {
        vector<Node> t(S);
        mesh.push_back(t);
    }

    double r = 2.0 / iter(mesh, S, S);
    cout << "R = " << setprecision(15) << r << '\n';

    return 0;
}
```

```txt
R = 1.60899124172989
```


## C#
```c#
using System;
using System.Collections.Generic;

namespace ResistorMesh {
    class Node {
        public Node(double v, int fixed_) {
            V = v;
            Fixed = fixed_;
        }

        public double V { get; set; }
        public int Fixed { get; set; }
    }

    class Program {
        static void SetBoundary(List<List<Node>> m) {
            m[1][1].V = 1.0;
            m[1][1].Fixed = 1;

            m[6][7].V = -1.0;
            m[6][7].Fixed = -1;
        }

        static double CalcuateDifference(List<List<Node>> m, List<List<Node>> d, int w, int h) {
            double total = 0.0;
            for (int i = 0; i < h; i++) {
                for (int j = 0; j < w; j++) {
                    double v = 0.0;
                    int n = 0;
                    if (i > 0) {
                        v += m[i - 1][j].V;
                        n++;
                    }
                    if (j > 0) {
                        v += m[i][j - 1].V;
                        n++;
                    }
                    if (i + 1 < h) {
                        v += m[i + 1][j].V;
                        n++;
                    }
                    if (j + 1 < w) {
                        v += m[i][j + 1].V;
                        n++;
                    }
                    v = m[i][j].V - v / n;
                    d[i][j].V = v;
                    if (m[i][j].Fixed == 0) {
                        total += v * v;
                    }
                }
            }
            return total;
        }

        static double Iter(List<List<Node>> m, int w, int h) {
            List<List<Node>> d = new List<List<Node>>(h);
            for (int i = 0; i < h; i++) {
                List<Node> t = new List<Node>(w);
                for (int j = 0; j < w; j++) {
                    t.Add(new Node(0.0, 0));
                }
                d.Add(t);
            }

            double[] curr = new double[3];
            double diff = 1e10;

            while (diff > 1e-24) {
                SetBoundary(m);
                diff = CalcuateDifference(m, d, w, h);
                for (int i = 0; i < h; i++) {
                    for (int j = 0; j < w; j++) {
                        m[i][j].V -= d[i][j].V;
                    }
                }
            }

            for (int i = 0; i < h; i++) {
                for (int j = 0; j < w; j++) {
                    int k = 0;
                    if (i != 0) k++;
                    if (j != 0) k++;
                    if (i < h - 1) k++;
                    if (j < w - 1) k++;
                    curr[m[i][j].Fixed + 1] += d[i][j].V * k;
                }
            }

            return (curr[2] - curr[0]) / 2.0;
        }

        const int S = 10;
        static void Main(string[] args) {
            List<List<Node>> mesh = new List<List<Node>>(S);
            for (int i = 0; i < S; i++) {
                List<Node> t = new List<Node>(S);
                for (int j = 0; j < S; j++) {
                    t.Add(new Node(0.0, 0));
                }
                mesh.Add(t);
            }

            double r = 2.0 / Iter(mesh, S, S);
            Console.WriteLine("R = {0:F15}", r);
        }
    }
}
```

```txt
R = 1.608991241729890
```



## D

```d
import std.stdio, std.traits;

enum Node.FP differenceThreshold = 1e-40;

struct Node {
    alias FP = real;
    enum Kind : size_t { free, A, B }

    FP voltage = 0.0;

    /*const*/ private Kind kind = Kind.free;
    // Remove kindGet once kind is const.
    @property Kind kindGet() const pure nothrow @nogc {return kind; }
}

Node.FP iter(size_t w, size_t h)(ref Node[w][h] m) pure nothrow @nogc {
    static void enforceBoundaryConditions(ref Node[w][h] m)
    pure nothrow @nogc {
        m[1][1].voltage =  1.0;
        m[6][7].voltage = -1.0;
    }

    static Node.FP calcDifference(in ref Node[w][h] m,
                                  ref Node[w][h] d) pure nothrow @nogc {
        Node.FP total = 0.0;

        foreach (immutable i; 0 .. h) {
            foreach (immutable j; 0 .. w) {
                Node.FP v = 0.0;
                {
                    size_t n = 0;
                    if (i != 0)    { v += m[i - 1][j].voltage; n++; }
                    if (j != 0)    { v += m[i][j - 1].voltage; n++; }
                    if (i < h - 1) { v += m[i + 1][j].voltage; n++; }
                    if (j < w - 1) { v += m[i][j + 1].voltage; n++; }
                    v = m[i][j].voltage - v / n;
                }

                d[i][j].voltage = v;
                if (m[i][j].kindGet == Node.Kind.free)
                    total += v ^^ 2;
            }
        }

        return total;
    }

    Node[w][h] difference;

    while (true) {
        enforceBoundaryConditions(m);
        if (calcDifference(m, difference) < differenceThreshold)
            break;
        foreach (immutable i, const di; difference)
            foreach (immutable j, const ref dij; di)
                m[i][j].voltage -= dij.voltage;
    }

    Node.FP[EnumMembers!(Node.Kind).length] cur = 0.0;
    foreach (immutable i, const di; difference)
        foreach (immutable j, const ref dij; di)
            cur[m[i][j].kindGet] += dij.voltage *
                                   (!!i + !!j + (i < h-1) + (j < w-1));

    return (cur[Node.Kind.A] - cur[Node.Kind.B]) / 2.0;
}

void main() {
    enum size_t w = 10,
                h = w;
    Node[w][h] mesh;

    // Set A and B Nodes.
    mesh[1][1] = Node( 1.0, Node.Kind.A);
    mesh[6][7] = Node(-1.0, Node.Kind.B);

    writefln("R = %.19f", 2 / mesh.iter);
}
```

```txt
R = 1.6089912417307296556
```



## ERRE

We'll solve the linear system.  We'll write [[wp:Kirchhoff's circuit laws|Kirchhoff's circuit laws]] at each node and search for a voltage distribution that creates a 1A current coming from A exiting in B. The difference of voltage between B and A is then the resistance.

```ERRE

 PROGRAM RESISTENCE_MESH

 !$BASE=1

 !$DYNAMIC
 DIM A[0,0]

 BEGIN

  N=10
  NN=N*N
  !$DIM A[NN,NN+1]

  PRINT(CHR$(12);) !CLS
  ! generate matrix data
  NODE=0
  FOR ROW=1 TO N DO
    FOR COL=1 TO N DO
        NODE=NODE+1
        IF ROW>1 THEN
            A[NODE,NODE]=A[NODE,NODE]+1
            A[NODE,NODE-N]=-1
        END IF
        IF ROW<N THEN
            A[NODE,NODE]=A[NODE,NODE]+1
            A[NODE,NODE+N]=-1
        END IF
        IF COL>1 THEN
            A[NODE,NODE]=A[NODE,NODE]+1
            A[NODE,NODE-1]=-1
        END IF
        IF COL<N THEN
            A[NODE,NODE]=A[NODE,NODE]+1
            A[NODE,NODE+1]=-1
        END IF
    END FOR
  END FOR

  AR=2  AC=2  A=AC+N*(AR-1)
  BR=7  BC=8  B=BC+N*(BR-1)
  A[A,NN+1]=-1
  A[B,NN+1]=1

  PRINT("Nodes ";A,B)

  ! solve linear system
  ! using Gauss-Seidel method
  ! with pivoting
  R=NN

  FOR J=1 TO R DO
    FOR I=J TO R DO
      EXIT IF A[I,J]<>0
    END FOR
    IF I=R+1 THEN
      PRINT("No solution!")
      !$STOP
    END IF
    FOR K=1 TO R+1 DO
      SWAP(A[J,K],A[I,K])
    END FOR
    Y=1/A[J,J]
    FOR K=1 TO R+1 DO
      A[J,K]=Y*A[J,K]
    END FOR
    FOR I=1 TO R DO
      IF I<>J THEN
         Y=-A[I,J]
         FOR K=1 TO R+1 DO
            A[I,K]=A[I,K]+Y*A[J,K]
         END FOR
      END IF
    END FOR
  END FOR
  PRINT("Resistence=";ABS(A[A,NN+1]-A[B,NN+1]))
 END PROGRAM

```

```txt
Nodes 12  68
Resistence=1.608993
```



## Euler Math Toolbox


The functions for this have been implemented in Euler already. Thus the following commands solve this problem.


```Euler Math Toolbox

>load incidence;
>{u,r}=solvePotentialX(makeRectangleX(10,10),12,68); r,
 1.60899124173

```


The necessary functions in the file incidence.e are as follows. There are versions with full matrices. But the functions listed here use compressed matrices and the conjugate gradient method.

<lang>
function makeRectangleX (n:index,m:index)
## Make the incidence matrix of a rectangle grid in compact form.
## see: makeRectangleIncidence
	K=zeros(n*(m-1)+m*(n-1),3);
	k=1;
	for i=1 to n;
		for j=1 to m-1;
		K[k,1]=(i-1)*m+j; K[k,2]=(i-1)*m+j+1; K[k,3]=1;
		k=k+1;
		end;
	end;
	for i=1 to n-1;
		for j=1 to m;
		K[k,1]=(i-1)*m+j; K[k,2]=i*m+j; K[k,3]=1;
		k=k+1;
		end;
	end;
	H=cpxzeros([n*m,n*m]);
	H=cpxset(H,K);
	H=cpxset(H,K[:,[2,1,3]]);
	return H;
endfunction

function solvePotentialX (A:cpx, i:index ,j:index)
## Solve the potential problem of resistance in a graph.
## This functions uses the conjugate gradient method.
## A is a compressed incidence matrix.
## Return the potential u for the nodes in A,
## such that u[i]=1, u[j]=-1, and the flow
## to each knot is equal to the flow from the knot,
## and the flow from i to j is (u[i]-u[j])*A[i,j].
## see: makeIncidence
	n=size(A)[1];
	b=ones(n,1); f=-cpxmult(A,b);
	h=1:n; B=cpxset(A,h'|h'|f);
	B=cpxset(B,i|h'|0);
	B=cpxset(B,[i,i,1]);
	B=cpxset(B,j|h'|0);
	B=cpxset(B,[j,j,1]);
	v=zeros(n,1); v[i]=1; v[j]=-1;
	u=cpxfit(B,v);
	f=(-f[i])*u[i]-cpxmult(A,u)[i];
	return {u,2/f}
endfunction

```


Here is the code for the conjugate gradient method for compressed, sparse matrices from cpx.e.

<lang>
function cgX (H:cpx, b:real column, x0:real column=none, f:index=10)
## Conjugate gradient method to solve Hx=b for compressed H.
##
## This is the method of choice for large, sparse matrices. In most
## cases, it will work well, fast, and accurate.
##
## H must be positive definite. Use cpxfit, if it is not.
##
## The accuarcy can be controlled with an additional parameter
## eps. The algorithm stops, when the error gets smaller then eps, or
## after f*n iterations, if the error gets larger. x0 is an optional
## start vector.
##
## H : compressed matrix (nxm)
## b : column vector (mx1)
## x0 : optional start point (mx1)
## f : number of steps, when the method should be restarted
##
## See: cpxfit, cg, cgXnormal
	if isvar("eps") then localepsilon(eps); endif;
	n=cols(H);
	if x0==none then x=zeros(size(b));
	else; x=x0;
	endif;
	loop 1 to 10
		r=b-cpxmult(H,x); p=r; fehler=r'.r;
		loop 1 to f*n
			if sqrt(fehler)~=0 then return x; endif;
			Hp=cpxmult(H,p);
			a=fehler/(p'.Hp);
			x=x+a*p;
			rn=r-a*Hp;
			fehlerneu=rn'.rn;
			p=rn+fehlerneu/fehler*p;
			r=rn; fehler=fehlerneu;
		end;
	end;
	return x;
endfunction

```

=={{Header|FreeBASIC}}==

```freebasic
' version 01-07-2018
' compile with: fbc -s console

#Define n 10

Dim As UInteger nn = n * n
Dim As Double g(-nn To nn +1, -nn To nn +1)
Dim As UInteger node, row, col

For row = 1 To n
    For col = 1 To n
        node += 1
        If row > 1 Then
            g(node, node) += 1
            g(node, node - n) = -1
        End If
        If row < n Then
            g(node, node) += 1
            g(node, node + n) = -1
        End If
        If col > 1 Then
            g(node, node) += 1
            g(node, node -1) = -1
        End If
        If col < n Then
            g(node, node) += 1
            g(node, node +1) = -1
        End If
    Next
Next

Dim As UInteger ar = 2, ac = 2
Dim As UInteger br = 7, bc = 8
Dim As UInteger a = ac + n * (ar -1)
Dim As UInteger b = bc + n * (br -1)

g(a, nn +1) = -1
g(b, nn +1) = 1

Print : Print "Nodes a: "; a, " b: "; b

' solve linear system using Gauss-Seidel method with pivoting
Dim As UInteger i, j, k
Dim As Double y

Do
    For j = 1 To nn
        For i = j To nn
            If g(i, j) <> 0 Then Exit For
        Next
        If i = nn +1 Then
            Print : Print "No solution"
            Exit Do
        End If
        For k = 1 To nn +1
            Swap g(j, k), g(i, k)
        Next
        y = g(j, j)
        For k = 1 To nn +1
            g(j, k) = g(j, k) / y
        Next
        For i = 1 To nn
            If i <> j Then
                y = -g(i, j)
                For k = 1 To nn +1
                    g(i, k) = g(i, k) + y * g(j, k)
                Next
            End If
        Next
    Next

    Print
    Print "Resistance ="; Abs(g(a, nn +1) - g(b, nn +1)); " Ohm"
    Exit Do
Loop

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Nodes a: 12    b: 68

Resistance = 1.60899124173073 Ohm
```



## Go

```go
package main

import "fmt"

const (
	S = 10
)

type node struct {
	v     float64
	fixed int
}

func alloc2(w, h int) [][]node {
	a := make([][]node, h)

	for i := range a {
		a[i] = make([]node, w)
	}
	return a
}

func set_boundary(m [][]node) {
	m[1][1].fixed = 1
	m[1][1].v = 1
	m[6][7].fixed = -1
	m[6][7].v = -1
}

func calc_diff(m [][]node, d [][]node, w, h int) float64 {
	total := 0.0
	for i := 0; i < h; i++ {
		for j := 0; j < w; j++ {
			v := 0.0
			n := 0
			if i != 0 {
				v += m[i-1][j].v
				n++
			}
			if j != 0 {
				v += m[i][j-1].v
				n++
			}
			if i+1 < h {
				v += m[i+1][j].v
				n++
			}
			if j+1 < w {
				v += m[i][j+1].v
				n++
			}

			v = m[i][j].v - v/float64(n)
			d[i][j].v = v
			if m[i][j].fixed == 0 {
				total += v * v
			}
		}
	}
	return total
}

func iter(m [][]node, w, h int) float64 {
	d := alloc2(w, h)
	diff := 1.0e10
	cur := []float64{0, 0, 0}

	for diff > 1e-24 {
		set_boundary(m)
		diff = calc_diff(m, d, w, h)
		for i := 0; i < h; i++ {
			for j := 0; j < w; j++ {
				m[i][j].v -= d[i][j].v
			}
		}
	}

	for i := 0; i < h; i++ {
		for j := 0; j < w; j++ {
			t := 0
			if i != 0 {
				t += 1
			}
			if j != 0 {
				t += 1
			}
			if i < h-1 {
				t += 1
			}
			if j < w-1 {
				t += 1
			}
			cur[m[i][j].fixed+1] += d[i][j].v * float64(t)
		}
	}
	return (cur[2] - cur[0]) / 2
}

func main() {
	mesh := alloc2(S, S)
	fmt.Printf("R = %g\n", 2/iter(mesh, S, S))
}

```



## Haskell

{{trans|Octave}} All mutations are expressed as monoidal operations.

```haskell
{-# LANGUAGE ParallelListComp #-}
import Numeric.LinearAlgebra (linearSolve, toDense, (!), flatten)
import Data.Monoid ((<>), Sum(..))

rMesh n (ar, ac) (br, bc)
  | n < 2 = Nothing
  | any (\x -> x < 1 || x > n) [ar, ac, br, bc] = Nothing
  | otherwise = between a b <$> voltage
  where
    a = (ac - 1) + n*(ar - 1)
    b = (bc - 1) + n*(br - 1)

    between x y v = abs (v ! a - v ! b)

    voltage = flatten <$> linearSolve matrixG current

    matrixG = toDense $ concat [ element row col node
                               | row <- [1..n], col <- [1..n]
                               | node <- [0..] ]

    element row col node =
      let (Sum c, elements) =
            (Sum 1, [((node, node-n), -1)]) `when` (row > 1) <>
            (Sum 1, [((node, node+n), -1)]) `when` (row < n) <>
            (Sum 1, [((node, node-1), -1)]) `when` (col > 1) <>
            (Sum 1, [((node, node+1), -1)]) `when` (col < n)
      in [((node, node), c)] <> elements

    x `when` p = if p then x else mempty

    current  = toDense [ ((a, 0), -1) , ((b, 0),  1) , ((n^2-1, 0), 0) ]
```


  λ> rMesh 10 (2,2) (7,8)
  Just 1.6089912417307304


## J


We represent the mesh as a [[wp:Ybus matrix|Ybus matrix]] with B as the reference node and A as the first node and invert it to find the Z bus (which represents resistance).  The first element of the first row of this Z matrix is the resistance between nodes A and B. (It has to be the first element because A was the first node. And we can "ignore" B because we made everything be relative to B.) Most of the work is defining <code>Y</code> which represents the Ybus.


```J
nodes=: 10 10 #: i. 100
nodeA=: 1 1
nodeB=: 6 7

NB. verb to pair up coordinates along a specific offset
conn =: [: (#~ e.~/@|:~&0 2) ([ ,: +)"1

ref =: ~. nodeA,nodes-.nodeB                    NB. all nodes, with A first and B omitted
wiring=: /:~ ref i. ,/ nodes conn"2 1 (,-)=i.2  NB. connected pairs (indices into ref)
Yii=: (* =@i.@#) #/.~ {."1 wiring               NB. diagonal of Y represents connections to B
Yij=: -1:`(<"1@[)`]}&(+/~ 0*i.1+#ref) wiring    NB. off diagonal of Y represents wiring
Y=: _1 _1 }. Yii+Yij
```


Here, the result of <code>nodes conn offset</code> represents all pairs of nodes where we can connect the argument nodes to neighboring nodes at the specified offset, and <code>wiring</code> is a list of index pairs representing all connections made by all resistors (note that each connection is represented twice -- node e is connected to node f AND node f is connected to node e).  Yii contains the values for the diagonal elements of the Y bus while Yij contains the values for the off diagonal elements of the Y bus.

So:


```J
   {.{. %. Y
1.60899
```


Or, if we want an exact answer (this is slow), we can assume our resistors are perfect:


```J
   {.{.%. x:Y
455859137025721r283319837425200
```


(here, the letter 'r' separates the numerator from the denominator)

To get a better feel for what the <code>conn</code> operation is doing, here is a small illustration:


```J
   3 3 #: i.9
0 0
0 1
0 2
1 0
1 1
1 2
2 0
2 1
2 2
   (3 3 #: i.9) conn 0 1
0 0
0 1

0 1
0 2

1 0
1 1

1 1
1 2

2 0
2 1

2 1
2 2
```


In other words, each coordinate pair is matched up with the coordinate pair that you would get by adding the offset to the first of the pair. In actual use, we use this four times, with four offsets (two horizontal and two vertical) to get our complete mesh.


## Java

```Java
import java.util.ArrayList;
import java.util.List;

public class ResistorMesh {
    private static final int S = 10;

    private static class Node {
        double v;
        int fixed;

        Node(double v, int fixed) {
            this.v = v;
            this.fixed = fixed;
        }
    }

    private static void setBoundary(List<List<Node>> m) {
        m.get(1).get(1).v = 1.0;
        m.get(1).get(1).fixed = 1;

        m.get(6).get(7).v = -1.0;
        m.get(6).get(7).fixed = -1;
    }

    private static double calcDiff(List<List<Node>> m, List<List<Node>> d, int w, int h) {
        double total = 0.0;
        for (int i = 0; i < h; ++i) {
            for (int j = 0; j < w; ++j) {
                double v = 0.0;
                int n = 0;
                if (i > 0) {
                    v += m.get(i - 1).get(j).v;
                    n++;
                }
                if (j > 0) {
                    v += m.get(i).get(j - 1).v;
                    n++;
                }
                if (i + 1 < h) {
                    v += m.get(i + 1).get(j).v;
                    n++;
                }
                if (j + 1 < w) {
                    v += m.get(i).get(j + 1).v;
                    n++;
                }
                v = m.get(i).get(j).v - v / n;
                d.get(i).get(j).v = v;
                if (m.get(i).get(j).fixed == 0) {
                    total += v * v;
                }
            }
        }
        return total;
    }

    private static double iter(List<List<Node>> m, int w, int h) {
        List<List<Node>> d = new ArrayList<>(h);
        for (int i = 0; i < h; ++i) {
            List<Node> t = new ArrayList<>(w);
            for (int j = 0; j < w; ++j) {
                t.add(new Node(0.0, 0));
            }
            d.add(t);
        }

        double[] cur = new double[3];
        double diff = 1e10;

        while (diff > 1e-24) {
            setBoundary(m);
            diff = calcDiff(m, d, w, h);
            for (int i = 0; i < h; ++i) {
                for (int j = 0; j < w; ++j) {
                    m.get(i).get(j).v -= d.get(i).get(j).v;
                }
            }
        }

        for (int i = 0; i < h; ++i) {
            for (int j = 0; j < w; ++j) {
                int k = 0;
                if (i != 0) k++;
                if (j != 0) k++;
                if (i < h - 1) k++;
                if (j < w - 1) k++;
                cur[m.get(i).get(j).fixed + 1] += d.get(i).get(j).v * k;
            }
        }

        return (cur[2] - cur[0]) / 2.0;
    }

    public static void main(String[] args) {
        List<List<Node>> mesh = new ArrayList<>(S);
        for (int i = 0; i < S; ++i) {
            List<Node> t = new ArrayList<>(S);
            for (int j = 0; j < S; ++j) {
                t.add(new Node(0.0, 0));
            }
            mesh.add(t);
        }

        double r = 2.0 / iter(mesh, S, S);
        System.out.printf("R = %.15f", r);
    }
}
```

```txt
R = 1.608991241729889
```



## Julia

We construct the matrix A that relates voltage v on each node to the injected current b via Av=b, and then we simply solve the linear system to find the resulting voltages (from unit currents at the indicated nodes, i and j) and hence the resistance.
We can write A=D<sup>T</sup>D in terms of the incidence matrix D of the resistor graph (see e.g. Strang, <i>Introduction to Linear Algebra</i>, 4th ed., sec. 8.2).
Because the graph is a rectangular grid, we can in turn write the incidence matrix D in terms of Kronecker products ⊗ (<code>kron</code> in Julia) of "one-dimensional" D<sub>1</sub> matrices (the incidence matrix of a 1d resistor network).
We use Julia's built-in sparse-matrix solvers (based on SuiteSparse) to solve the resulting sparse linear system efficiently

```julia
N = 10
D1 = speye(N-1,N) - spdiagm(ones(N-1),1,N-1,N)
D = [ kron(D1, speye(N)); kron(speye(N), D1) ]
i, j = N*1 + 2, N*7+7
b = zeros(N^2); b[i], b[j] = 1, -1
v = (D' * D) \ b
v[i] - v[j]
```

```txt

1.6089912417307288

```

One advantage of this formulation is that it is easy to generalize to non-constant resistance.
If we have a vector <code>y</code> of admittance (1/resistance) values on each resistor, then one simply replaces <code>D' * D</code> with <code>D' * spdiagm(y) * D</code>.


## Kotlin

```scala
// version 1.1.4-3

typealias List2D<T> = List<List<T>>

const val S = 10

class Node(var v: Double, var fixed: Int)

fun setBoundary(m: List2D<Node>) {
    m[1][1].v =  1.0; m[1][1].fixed =  1
    m[6][7].v = -1.0; m[6][7].fixed = -1
}

fun calcDiff(m: List2D<Node>, d: List2D<Node>, w: Int, h: Int): Double {
    var total = 0.0
    for (i in 0 until h) {
        for (j in 0 until w) {
            var v = 0.0
            var n = 0
            if (i > 0) { v += m[i - 1][j].v; n++ }
            if (j > 0) { v += m[i][j - 1].v; n++ }
            if (i + 1 < h) { v += m[i + 1][j].v; n++ }
            if (j + 1 < w) { v += m[i][j + 1].v; n++ }
            v = m[i][j].v - v / n
            d[i][j].v = v
            if (m[i][j].fixed == 0) total += v * v
        }
    }
    return total
}

fun iter(m: List2D<Node>, w: Int, h: Int): Double {
    val d = List(h) { List(w) { Node(0.0, 0) } }
    val cur = DoubleArray(3)
    var diff = 1e10

    while (diff > 1e-24) {
        setBoundary(m)
        diff = calcDiff(m, d, w, h)
        for (i in 0 until h) {
            for (j in 0 until w) m[i][j].v -= d[i][j].v
        }
    }

    for (i in 0 until h) {
        for (j in 0 until w) {
            var k = 0
            if (i != 0) k++
            if (j != 0) k++
            if (i < h - 1) k++
            if (j < w - 1) k++
            cur[m[i][j].fixed + 1] += d[i][j].v * k
        }
    }
    return (cur[2] - cur[0]) / 2.0
}

fun main(args: Array<String>) {
    val mesh = List(S) { List(S) { Node(0.0, 0) } }
    val r = 2.0 / iter(mesh, S, S)
    println("R = $r")
}
```


```txt

R = 1.608991241729889

```



## Maxima


```maxima
/* Place a current souce between A and B, providing 1 A. Then we are really looking
   for the potential at A and B, since I = R (V(B) - V(A)) where I is given and we want R.

   Atually, we will compute potential at each node, except A where we assume it's 0.
   Without with assumption, there would be infinitely many solutions since potential
   is known up to a constant. For A we will simply write the equation V(A) = 0, to
   keep the program simple.

   Hence, for a general grid of p rows and q columns, there are n = p * q nodes,
   so n unknowns, and n equations. Write Kirchhoff's current law at each node.
   Be careful with the node A (equation A = 0) and the node B (there is a constant
   current to add, from the source, that will go in the constant terms of the system).

   Finally, we have a n x n linear system of equations to solve. Simply use Maxima's
   builtin LU decomposition.

   Since all computations are exact, the result will be also exact, written as a fraction.
   Also, the program can work with any grid, and any two nodes on the grid.

   For those who want more speed and less space, notice the system is sparse and necessarily
   symmetric, so one can use conjugate gradient or any other sparse symmetric solver. */


/* Auxiliary function to get rid of the borders */
ongrid(i, j, p, q) := is(i >= 1 and i <= p and j >= 1 and j <= q)$

grid_resistor(p, q, ai, aj, bi, bj) := block(
   [n: p * q, A, B, M, k, c, V],
   A: zeromatrix(n, n),
   for i thru p do
      for j thru q do (
         k: (i - 1) * q + j,
         if i = ai and j = aj then
            A[k, k]: 1
         else (
            c: 0,
            if ongrid(i + 1, j, p, q) then (c: c + 1, A[k, k + q]: -1),
            if ongrid(i - 1, j, p, q) then (c: c + 1, A[k, k - q]: -1),
            if ongrid(i, j + 1, p, q) then (c: c + 1, A[k, k + 1]: -1),
            if ongrid(i, j - 1, p, q) then (c: c + 1, A[k, k - 1]: -1),
            A[k, k]: c
         )
      ),
   B: zeromatrix(n, 1),
   B[k: (bi - 1) * q + bj, 1]: 1,
   M: lu_factor(A),
   V: lu_backsub(M, B),
   V[k, 1]
)$

grid_resistor(10, 10, 2, 2, 8, 7);
455859137025721 / 283319837425200

bfloat(%), fpprec = 40;
1.608991241730729655954495520510088761201b0

/* Some larger example */
grid_resistor(20, 20, 1, 1, 20, 20);
129548954101732562831760781545158173626645023 / 33283688571680493510612137844679320717594861

bfloat(%), fpprec = 40;
3.89226554090400912102670691601064387507b0
```



## Mathematica

```mathematica
gridresistor[p_, q_, ai_, aj_, bi_, bj_] :=
  Block[{A, B, k, c, V}, A = ConstantArray[0, {p*q, p*q}];
   Do[k = (i - 1) q + j;
    If[{i, j} == {ai, aj}, A[[k, k]] = 1, c = 0;
     If[1 <= i + 1 <= p && 1 <= j <= q, c++; A[[k, k + q]] = -1];
     If[1 <= i - 1 <= p && 1 <= j <= q, c++; A[[k, k - q]] = -1];
     If[1 <= i <= p && 1 <= j + 1 <= q, c++; A[[k, k + 1]] = -1];
     If[1 <= i <= p && 1 <= j - 1 <= q, c++; A[[k, k - 1]] = -1];
     A[[k, k]] = c], {i, p}, {j, q}];
   B = SparseArray[(k = (bi - 1) q + bj) -> 1, p*q];
   LinearSolve[A, B][[k]]];
N[gridresistor[10, 10, 2, 2, 8, 7], 40]
```

```txt
1.608991241730729655954495520510088761201
```



```mathematica
graphresistor[g_, a_, b_] :=
  LinearSolve[
    SparseArray[{{a, a} -> 1, {i_, i_} :> Length@AdjacencyList[g, i],
      Alternatives @@ Join[#, Reverse /@ #] &[
        List @@@ EdgeList[VertexDelete[g, a]]] -> -1}, {VertexCount[
       g], VertexCount[g]}], SparseArray[b -> 1, VertexCount[g]]][[b]];
N[graphresistor[GridGraph[{10, 10}], 12, 77], 40]
```

```txt
1.608991241730729655954495520510088761201
```


=={{header|Modula-2}}==

```modula2
MODULE ResistorMesh;
FROM RConversions IMPORT RealToStringFixed;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

CONST S = 10;

TYPE Node = RECORD
    v : LONGREAL;
    fixed : INTEGER;
END;

PROCEDURE SetBoundary(VAR m : ARRAY OF ARRAY OF Node);
BEGIN
    m[1][1].v := 1.0;
    m[1][1].fixed := 1;

    m[6][7].v := -1.0;
    m[6][7].fixed := -1;
END SetBoundary;

PROCEDURE CalcDiff(VAR m,d : ARRAY OF ARRAY OF Node) : LONGREAL;
VAR
    total,v : LONGREAL;
    i,j,n : INTEGER;
BEGIN
    total := 0.0;
    FOR i:=0 TO S DO
        FOR j:=0 TO S DO
            v := 0.0;
            n := 0;
            IF i>0 THEN
                v := v + m[i-1][j].v;
                INC(n);
            END;
            IF j>0 THEN
                v := v + m[i][j-1].v;
                INC(n);
            END;
            IF i+1<S THEN
                v := v + m[i+1][j].v;
                INC(n);
            END;
            IF j+1<S THEN
                v := v + m[i][j+1].v;
                INC(n);
            END;
            v := m[i][j].v - v / LFLOAT(n);
            d[i][j].v := v;
            IF m[i][j].fixed=0 THEN
                total := total + v*v;
            END;
        END;
    END;
    RETURN total;
END CalcDiff;

PROCEDURE Iter(m : ARRAY OF ARRAY OF Node) : LONGREAL;
VAR
    d : ARRAY[0..S] OF ARRAY[0..S] OF Node;
    i,j,k : INTEGER;
    cur : ARRAY[0..2] OF LONGREAL;
    diff : LONGREAL;
BEGIN
    FOR i:=0 TO S DO
        FOR j:=0 TO S DO
            d[i][j] := Node{0.0,0};
        END;
    END;

    diff := 1.0E10;
    WHILE diff>1.0E-24 DO
        SetBoundary(m);
        diff := CalcDiff(m,d);
        FOR i:=0 TO S DO
            FOR j:=0 TO S DO
                m[i][j].v := m[i][j].v - d[i][j].v;
            END;
        END;
    END;

    FOR i:=0 TO S DO
        FOR j:=0 TO S DO
            k:=0;
            IF i#0 THEN INC(k) END;
            IF j#0 THEN INC(k) END;
            IF i<S-1 THEN INC(k) END;
            IF j<S-1 THEN INC(k) END;
            cur[m[i][j].fixed+1] := cur[m[i][j].fixed+1] + d[i][j].v*LFLOAT(k);
        END;
    END;

    RETURN (cur[2]-cur[0]) / 2.0;
END Iter;

VAR
    mesh : ARRAY[0..S] OF ARRAY[0..S] OF Node;
    buf : ARRAY[0..32] OF CHAR;
    r : LONGREAL;
    pos : CARDINAL;
    ok : BOOLEAN;
BEGIN
    pos := 0;
    r := 2.0 / Iter(mesh);
    WriteString("R = ");
    RealToStringFixed(r, 15,0, buf, pos, ok);
    WriteString(buf);
    WriteString(" ohms");
    WriteLn;

    ReadChar;
END ResistorMesh.
```



## Octave

We'll solve the linear system.  We'll write [[wp:Kirchhoff's circuit laws|Kirchhoff's circuit laws]] at each node and search for a voltage distribution that creates a 1A current coming from A exiting in B.  The difference of voltage between B and A is then the resistance.


```octave
N = 10;
NN = N*N;
G = sparse(NN, NN);

node = 0;
for row=1:N;
    for col=1:N;
	node++;
	if row > 1
	    G(node, node)++;
	    G(node, node - N) = -1;
	end
	if row < N;
	    G(node, node)++;
	    G(node, node + N) = -1;
	end
	if col > 1
	    G(node, node)++;
	    G(node, node - 1) = -1;
	end
	if col < N;
	    G(node, node)++;
	    G(node, node + 1) = -1;
	end
    end
end

current = sparse(NN, 1);

Ar = 2; Ac = 2; A = Ac + N*( Ar - 1 );
Br = 7; Bc = 8; B = Bc + N*( Br - 1 );
current( A ) = -1;
current( B ) = +1;

voltage = G \ current;

VA = voltage( A );
VB = voltage( B );

full( abs( VA - VB ) )
```

```txt
ans =  1.6090
```



## Perl

```perl
use strict;
use warnings;

my ($w, $h) = (9, 9);
my @v = map([ (0) x ($w + 1) ], 0 .. $h); # voltage
my @f = map([ (0) x ($w + 1) ], 0 .. $h); # fixed condition
my @d = map([ (0) x ($w + 1) ], 0 .. $h); # diff

my @n; # neighbors
for my $i (0 .. $h) {
	push @{$n[$i][$_]}, [$i, $_ - 1] for 1 .. $w;
	push @{$n[$i][$_]}, [$i, $_ + 1] for 0 .. $w - 1;
}
for my $j (0 .. $w) {
	push @{$n[$_][$j]}, [$_ - 1, $j] for 1 .. $h;
	push @{$n[$_][$j]}, [$_ + 1, $j] for 0 .. $h - 1;
}

sub set_boundary {
	$f[1][1] = 1; $f[6][7] = -1;
	$v[1][1] = 1; $v[6][7] = -1;
}

sub calc_diff {
	my $total_diff;
	for my $i (0 .. $h) {
		for my $j (0 .. $w) {
			my ($p, $v) = $n[$i][$j];
			$v += $v[$_->[0]][$_->[1]] for @$p;
			$d[$i][$j] = $v = $v[$i][$j] - $v / scalar(@$p);
			$total_diff += $v * $v unless $f[$i][$j];
		}
	}
	$total_diff;
}

sub iter {
	my $diff = 1;
	while ($diff > 1e-15) {
		set_boundary();
		$diff = calc_diff();
		#print "error^2: $diff\n"; # un-comment to see slow convergence
		for my $i (0 .. $h) {
			for my $j (0 .. $w) {
				$v[$i][$j] -= $d[$i][$j];
			}
		}
	}

	my @current = (0) x 3;
	for my $i (0 .. $h) {
		for my $j (0 .. $w) {
			$current[ $f[$i][$j] ] +=
				$d[$i][$j] * scalar(@{$n[$i][$j]});
		}
	}
	return ($current[1] - $current[-1]) / 2;
}

printf "R = %.6f\n", 2 / iter();
```

```txt
R = 1.608991
```



## Perl 6

```perl6
my $S = 10;

my @fixed;

sub allocmesh ($w, $h) {
    gather for ^$h {
	take [0 xx $w];
    }
}

sub force-fixed(@f) {
    @f[1][1] =  1;
    @f[6][7] = -1;
}

sub force-v(@v) {
    @v[1][1] =  1;
    @v[6][7] = -1;
}

sub calc_diff(@v, @d, Int $w, Int $h) {
    my $total = 0;
    for (flat ^$h X ^$w) -> $i, $j {
        my @neighbors = grep *.defined, @v[$i-1][$j], @v[$i][$j-1], @v[$i+1][$j], @v[$i][$j+1];
        my $v = [+] @neighbors;
        @d[$i][$j] = $v = @v[$i][$j] - $v / +@neighbors;
        $total += $v * $v unless @fixed[$i][$j];
    }
    return $total;
}

sub iter(@v, Int $w, Int $h) {
    my @d = allocmesh($w, $h);
    my $diff = 1e10;
    my @cur = 0, 0, 0;

    while $diff > 1e-24 {
        force-v(@v);
        $diff = calc_diff(@v, @d, $w, $h);
        for (flat ^$h X ^$w) -> $i, $j {
            @v[$i][$j] -= @d[$i][$j];
        }
    }

    for (flat ^$h X ^$w) -> $i, $j {
        @cur[ @fixed[$i][$j] + 1 ]
            += @d[$i][$j] * (?$i + ?$j + ($i < $h - 1) + ($j < $w - 1));
    }

    return (@cur[2] - @cur[0]) / 2;
}

my @mesh = allocmesh($S, $S);

@fixed = allocmesh($S, $S);
force-fixed(@fixed);

say 2 / iter(@mesh, $S, $S);
```

```txt
1.60899124172989
```



## Phix

uses inverse() from [[Gauss-Jordan_matrix_inversion#Phix]]
and matrix_mul() from [[Matrix_multiplication#Phix]]

```Phix
function resistormesh(integer ni, nj, ai, aj, bi, bj)
    integer n = ni*nj, k, c
    sequence A = repeat(repeat(0,n),n),
             B = repeat({0},n)
    for i=1 to ni do
        for j=1 to nj do
            k = (i-1)*nj + j--1
            if i=ai and j=aj then
                A[k,k] = 1
            else
                c = 0
                if i<ni then c += 1; A[k,k+nj] = -1 end if
                if i>1 then  c += 1; A[k,k-nj] = -1 end if
                if j<nj then c += 1; A[k,k+1] = -1 end if
                if j>1 then  c += 1; A[k,k-1] = -1 end if
                A[k,k] = c
            end if
        end for
    end for
    k = (bi-1)*nj +bj
    B[k,1] = 1
    A = inverse(A)
    B = matrix_mul(A,B)
    return B[k,1]
end function

printf(1,"Resistance = %.13f ohms\n",resistormesh(10, 10, 2, 2, 8, 7))
```

```txt

Resistance = 1.6089912417307 ohms

```



## Python

```python
DIFF_THRESHOLD = 1e-40

class Fixed:
    FREE = 0
    A = 1
    B = 2

class Node:
    __slots__ = ["voltage", "fixed"]
    def __init__(self, v=0.0, f=Fixed.FREE):
        self.voltage = v
        self.fixed = f

def set_boundary(m):
    m[1][1] = Node( 1.0, Fixed.A)
    m[6][7] = Node(-1.0, Fixed.B)

def calc_difference(m, d):
    h = len(m)
    w = len(m[0])
    total = 0.0

    for i in xrange(h):
        for j in xrange(w):
            v = 0.0
            n = 0
            if i != 0:  v += m[i-1][j].voltage; n += 1
            if j != 0:  v += m[i][j-1].voltage; n += 1
            if i < h-1: v += m[i+1][j].voltage; n += 1
            if j < w-1: v += m[i][j+1].voltage; n += 1
            v = m[i][j].voltage - v / n

            d[i][j].voltage = v
            if m[i][j].fixed == Fixed.FREE:
                total += v ** 2
    return total

def iter(m):
    h = len(m)
    w = len(m[0])
    difference = [[Node() for j in xrange(w)] for i in xrange(h)]

    while True:
        set_boundary(m) # Enforce boundary conditions.
        if calc_difference(m, difference) < DIFF_THRESHOLD:
            break
        for i, di in enumerate(difference):
            for j, dij in enumerate(di):
                m[i][j].voltage -= dij.voltage

    cur = [0.0] * 3
    for i, di in enumerate(difference):
        for j, dij in enumerate(di):
            cur[m[i][j].fixed] += (dij.voltage *
                (bool(i) + bool(j) + (i < h-1) + (j < w-1)))

    return (cur[Fixed.A] - cur[Fixed.B]) / 2.0

def main():
    w = h = 10
    mesh = [[Node() for j in xrange(w)] for i in xrange(h)]
    print "R = %.16f" % (2 / iter(mesh))

main()
```

```txt
R = 1.6089912417307286
```


```python
import sys, copy
from fractions import Fraction

def gauss(a, b):
    n, p = len(a), len(a[0])
    for i in range(n):
        t = abs(a[i][i])
        k = i
        for j in range(i + 1, n):
            if abs(a[j][i]) > t:
                t = abs(a[j][i])
                k = j
        if k != i:
            for j in range(i, n):
                a[i][j], a[k][j] = a[k][j], a[i][j]
            b[i], b[k] = b[k], b[i]
        t = 1/a[i][i]
        for j in range(i + 1, n):
            a[i][j] *= t
        b[i] *= t
        for j in range(i + 1, n):
            t = a[j][i]
            for k in range(i + 1, n):
                a[j][k] -= t*a[i][k]
            b[j] -= t * b[i]
    for i in range(n - 1, -1, -1):
        for j in range(i):
            b[j] -= a[j][i]*b[i]
    return b

def resistor_grid(p, q, ai, aj, bi, bj):
    n = p*q
    I = Fraction(1, 1)
    v = [0*I]*n
    a = [copy.copy(v) for i in range(n)]
    for i in range(p):
        for j in range(q):
            k = i*q + j
            if i == ai and j == aj:
                a[k][k] = I
            else:
                c = 0
                if i + 1 < p:
                    c += 1
                    a[k][k + q] = -1
                if i >= 1:
                    c += 1
                    a[k][k - q] = -1
                if j + 1 < q:
                    c += 1
                    a[k][k + 1] = -1
                if j >= 1:
                    c += 1
                    a[k][k - 1] = -1
                a[k][k] = c*I
    b = [0*I]*n
    k = bi*q + bj
    b[k] = 1
    return gauss(a, b)[k]

def main(arg):
    r = resistor_grid(int(arg[0]), int(arg[1]), int(arg[2]), int(arg[3]), int(arg[4]), int(arg[5]))
    print(r)
    print(float(r))

main(sys.argv[1:])

# Output:
# python grid.py 10 10 1 1 7 6
# 455859137025721/283319837425200
# 1.6089912417307297
```



## Racket

This version avoids mutation... possibly a little more costly than C, but more functional.


```racket
#lang racket
(require racket/flonum)

(define-syntax-rule (fi c t f) (if c f t))

(define (neighbours w h)
  (define h-1 (sub1 h))
  (define w-1 (sub1 w))
  (lambda (i j)
    (+ (fi (zero? i) 1 0)
       (fi (zero? j) 1 0)
       (if (< i h-1) 1 0)
       (if (< j w-1) 1 0))))

(define (mesh-R probes w h)
  (define h-1 (sub1 h))
  (define w-1 (sub1 w))

  (define-syntax-rule (v2ref v r c) ; 2D vector ref
    (flvector-ref v (+ (* r w) c)))

  (define w*h (* w h))

  (define (alloc2 (v 0.))
    (make-flvector w*h v))

  (define nghbrs (neighbours w h))

  (match-define `((,fix+r ,fix+c) (,fix-r ,fix-c)) probes)
  (define fix+idx (+ fix+c (* fix+r w)))
  (define fix-idx (+ fix-c (* fix-r w)))
  (define fix-val
    (match-lambda**
     [((== fix+idx) _) 1.]
     [((== fix-idx) _) -1.]
     [(_ v) v]))

  (define (calc-diff m)
    (define d
      (for*/flvector #:length w*h ((i (in-range h)) (j (in-range w)))
        (define v
          (+ (fi (zero? i) (v2ref m (- i 1) j) 0)
             (fi (zero? j) (v2ref m i (- j 1)) 0)
             (if (< i h-1) (v2ref m (+ i 1) j) 0)
             (if (< j w-1) (v2ref m i (+ j 1)) 0)))
        (- (v2ref m i j) (/ v (nghbrs i j)))))

    (define Δ
      (for/sum ((i (in-naturals)) (d.v (in-flvector d)) #:when (= (fix-val i 0.) 0.))
        (sqr d.v)))

    (values d Δ))

  (define final-d
    (let loop ((m (alloc2)) (d (alloc2)))
      (define m+ ; do this first will get the boundaries on
        (for/flvector #:length w*h ((j (in-naturals)) (m.v (in-flvector m)) (d.v (in-flvector d)))
          (fix-val j (- m.v d.v))))

      (define-values (d- Δ) (calc-diff m+))

      (if (< Δ 1e-24) d (loop m+ d-))))

  (/ 2
     (/ (- (* (v2ref final-d fix+r fix+c) (nghbrs fix+r fix+c))
           (* (v2ref final-d fix-r fix-c) (nghbrs fix-r fix-c)))
        2)))

(module+ main
  (printf "R = ~a~%" (mesh-R '((1 1) (6 7)) 10 10)))
```


```txt
R = 1.6089912417301238
```



## REXX

This version allows specification of the grid size,   the locations of the   '''A'''   and   '''B'''   points,   and the number of decimal digits precision.

Dropping the decimal digits precision   ('''numeric digits''')   to   '''10'''   makes the execution   '''3'''   times faster.

```rexx
/*REXX program calculates the  resistance  between any  two points  on a  resistor grid.*/
if 2=='f2'x  then ohms = "ohms"                  /*EBCDIC machine?    Then use  'ohms'. */
             else ohms = "Ω"                     /* ASCII    "          "   "   Greek Ω.*/
parse arg high wide  Arow Acol  Brow Bcol digs . /*obtain optional arguments from the CL*/
if high=='' | high==","  then high= 10           /*Not specified?  Then use the default.*/
if wide=='' | wide==","  then wide= 10           /* "      "         "   "   "      "   */
if Arow=='' | Arow==","  then Arow=  2           /* "      "         "   "   "      "   */
if Acol=='' | Acol==","  then Acol=  2           /* "      "         "   "   "      "   */
if Brow=='' | Brow==","  then Brow=  7           /* "      "         "   "   "      "   */
if Bcol=='' | Bcol==","  then Bcol=  8           /* "      "         "   "   "      "   */
if digs=='' | digs==","  then digs= 20           /* "      "         "   "   "      "   */
numeric digits digs                              /*use moderate decimal digs (precision)*/
minVal = 1'e-' || (digs*2)                       /*calculate the threshold minimal value*/
say '    minimum value is '  format(minVal,,,,0)  " using "  digs  ' decimal digits';  say
say '    resistor mesh size is: '       wide      "wide, "    high   'high'         ;  say
say '    point  A  is at (row,col): '   Arow"," Acol
say '    point  B  is at (row,col): '   Brow"," Bcol
@.=0;                                      cell.= 1
            do  until  $<=minVal;          v= 0
            @.Arow.Acol=   1  ;            cell.Arow.Acol= 0
            @.Brow.Bcol= '-1' ;            cell.Brow.Bcol= 0
            $=0
                do   i=1  for high;        im= i-1;       ip= i+1
                  do j=1  for wide;        n= 0;          v= 0
                  if i\==1   then do;                     v= v + @.im.j;    n= n+1;    end
                  if j\==1   then do;      jm= j-1;       v= v + @.i.jm;    n= n+1;    end
                  if i<high  then do;                     v= v + @.ip.j;    n= n+1;    end
                  if j<wide  then do;      jp= j+1;       v= v + @.i.jp;    n= n+1;    end
                  v= @.i.j  -  v / n;      #.i.j= v;      if cell.i.j  then $= $ + v*v
                  end   /*j*/
                end     /*i*/
                                 do   r=1  for High
                                   do c=1  for Wide;      @.r.c= @.r.c   -   #.r.c
                                   end   /*c*/
                                 end     /*r*/
            end   /*until*/
say
Acur= #.Arow.Acol  *  sides(Arow, Acol)
Bcur= #.Brow.Bcol  *  sides(Brow, Bcol)
say '    resistance between point  A  and point  B  is: '     4 / (Acur - Bcur)       ohms
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sides:  parse arg row,col;   z=0;    if row\==1 & row\==high  then  z= z+2;    else z= z+1
                                     if col\==1 & col\==wide  then  z= z+2;    else z= z+1
        return z
```

```txt

    minimum value is  1E-40  using  20  decimal digits

    resistor mesh size is:  10 wide,  10 high

    point  A  is at (row,col):  2, 2
    point  B  is at (row,col):  7, 8

    resistance between point  A  and point  B  is:  1.6089912417307296559 Ω

```



## Sidef

```ruby
var (w, h) = (10, 10)

var v = h.of { w.of(0) } # voltage
var f = h.of { w.of(0) } # fixed condition
var d = h.of { w.of(0) } # diff
var n = []               # neighbors

for i in ^h {
    for j in (1 ..^ w  ) { n[i][j] := [] << [i, j-1] }
    for j in (0 ..^ w-1) { n[i][j] := [] << [i, j+1] }
}

for j in ^w {
    for i in (1 ..^ h  ) { n[i][j] := [] << [i-1, j] }
    for i in (0 ..^ h-1) { n[i][j] := [] << [i+1, j] }
}

func set_boundary {
    f[1][1] = 1; f[6][7] = -1;
    v[1][1] = 1; v[6][7] = -1;
}

func calc_diff {
    var total_diff = 0
    for i,j in (^h ~X ^w) {
        var w = n[i][j].map { |a| v.dig(a...) }.sum
        d[i][j] = (w = (v[i][j] - w/n[i][j].len))
        f[i][j] || (total_diff += w*w)
    }
    total_diff
}

func iter {
    var diff = 1
    while (diff > 1e-24) {
        set_boundary()
        diff = calc_diff()
        for i,j in (^h ~X ^w) {
            v[i][j] -= d[i][j]
        }
    }

    var current = 3.of(0)
    for i,j in (^h ~X ^w) {
        current[ f[i][j] ] += (d[i][j] * n[i][j].len)
    }
    (current[1] - current[-1]) / 2
}

say "R = #{2 / iter()}"
```

```txt

R = 1.60899124172988902191367295307304

```



## Tcl

```tcl
package require Tcl 8.6;  # Or 8.5 with the TclOO package

# This code is structured as a class with a little trivial DSL parser
# so it is easy to change what problem is being worked on.
oo::class create ResistorMesh {
    variable forcePoints V fixed w h

    constructor {boundaryConditions} {
	foreach {op condition} $boundaryConditions {
	    switch $op {
		size {
		    lassign $condition w h
		    set fixed [lrepeat $h [lrepeat $w 0]]
		    set V [lrepeat $h [lrepeat $w 0.0]]
		}
		fixed {
		    lassign $condition j i v
		    lset fixed $i $j [incr ctr]
		    lappend forcePoints $j $i $v
		}
	    }
	}
    }

    method CalculateDifferences {*dV} {
	upvar 1 ${*dV} dV
	set error 0.0
	for {set i 0} {$i < $h} {incr i} {
	    for {set j 0} {$j < $w} {incr j} {
		set v 0.0
		set n 0
		if {$i} {
		    set v [expr {$v + [lindex $V [expr {$i-1}] $j]}]
		    incr n
		}
		if {$j} {
		    set v [expr {$v + [lindex $V $i [expr {$j-1}]]}]
		    incr n
		}
		if {$i+1 < $h} {
		    set v [expr {$v + [lindex $V [expr {$i+1}] $j]}]
		    incr n
		}
		if {$j+1 < $w} {
		    set v [expr {$v + [lindex $V $i [expr {$j+1}]]}]
		    incr n
		}
		lset dV $i $j [set v [expr {[lindex $V $i $j] - $v/$n}]]
		if {![lindex $fixed $i $j]} {
		    set error [expr {$error + $v**2}]
		}
	    }
	}
	return $error
    }

    method FindCurrentFixpoint {epsilon} {
	set dV [lrepeat $h [lrepeat $w 0.0]]
	set current {0.0 0.0 0.0}
	while true {
	    # Enforce the boundary conditions
	    foreach {j i v} $forcePoints {
		lset V $i $j $v
	    }
	    # Compute the differences and the error
	    set error [my CalculateDifferences dV]
	    # Apply the differences
	    for {set i 0} {$i < $h} {incr i} {
		for {set j 0} {$j < $w} {incr j} {
		    lset V $i $j [expr {
			[lindex $V $i $j] - [lindex $dV $i $j]}]
		}
	    }
	    # Done if the error is small enough
	    if {$error < $epsilon} break
	}
	# Compute the currents from the error
	for {set i 0} {$i < $h} {incr i} {
	    for {set j 0} {$j < $w} {incr j} {
		lset current [lindex $fixed $i $j] [expr {
		    [lindex $current [lindex $fixed $i $j]] +
		    [lindex $dV $i $j] * (!!$i+!!$j+($i<$h-1)+($j<$w-1))}]
	    }
	}
	# Compute the actual current flowing between source and sink
	return [expr {([lindex $current 1] - [lindex $current 2]) / 2.0}]
    }

    # Public entry point
    method solveForResistance {{epsilon 1e-24}} {
	set voltageDifference [expr {
	    [lindex $forcePoints 2] - [lindex $forcePoints 5]}]
	expr {$voltageDifference / [my FindCurrentFixpoint $epsilon]}
    }
}
```

Setting up and solving this particular problem:

```tcl
ResistorMesh create mesh {
    size  {10 10}
    fixed {1 1  1.0}
    fixed {6 7 -1.0}
}
puts [format "R = %.12g" [mesh solveForResistance]]
```

```txt

R = 1.60899124173

```



## XPL0

```XPL0
code real RlRes=46, RlOut=48;
def  S = 10;

proc SetBoundary(MV, MF);
real MV; int MF;
[MF(1,1):= 1;  MV(1,1):= 1.0;
 MF(6,7):= -1; MV(6,7):= -1.0;
];

func real CalcDiff(MV, MF, DV, W, H);
real MV; int MF; real DV; int W, H;
int  I, J, N; real V, Total;
[Total:= 0.0;
for I:= 0 to H-1 do
    for J:= 0 to W-1 do
        [V:= 0.0;  N:= 0;
        if I then [V:= V + MV(I-1,J);  N:= N+1];
        if J then [V:= V + MV(I,J-1);  N:= N+1];
        if I+1 < H then [V:= V + MV(I+1,J);  N:= N+1];
        if J+1 < W then [V:= V + MV(I,J+1);  N:= N+1];
        V:= MV(I,J) - V/float(N);  DV(I,J):= V;
        if MF(I,J) = 0 then Total:= Total + V*V;
        ];
return Total;
];

func real Iter(MV, MF, W, H);
real MV; int MF, W, H;
real DV, Diff, Cur; int I, J;
[DV:= RlRes(W);  for I:= 0 to W-1 do DV(I):= RlRes(H);
Diff:= 1E10;
Cur:= [0.0, 0.0, 0.0];
while Diff > 1E-24 do
        [SetBoundary(MV, MF);
        Diff:= CalcDiff(MV, MF, DV, W, H);
        for I:= 0 to H-1 do
            for J:= 0 to W-1 do
                MV(I,J):= MV(I,J) - DV(I,J);
        ];
for I:= 0 to H-1 do
    for J:= 0 to W-1 do
        Cur(MF(I,J)+1):= Cur(MF(I,J)+1) +
                DV(I,J) * float(-(I>0) - (J>0) - (I<H-1) - (J<W-1));
                                \middle=4; side=3; corner=2
return (Cur(2)-Cur(0))/2.0;
];

real MeshV(S,S); int MeshF(S,S);
RlOut(0, 2.0 / Iter(MeshV, MeshF, S, S))
```


```txt

    1.60899

```



## Yabasic

```Yabasic
  N=10
  NN=N*N
  DIM A(NN,NN+1)

  NODE=0
  FOR ROW=1 TO N
    FOR COL=1 TO N
        NODE=NODE+1
        IF ROW>1 THEN
            A(NODE,NODE)=A(NODE,NODE)+1
            A(NODE,NODE-N)=-1
        END IF
        IF ROW<N THEN
            A(NODE,NODE)=A(NODE,NODE)+1
            A(NODE,NODE+N)=-1
        END IF
        IF COL>1 THEN
            A(NODE,NODE)=A(NODE,NODE)+1
            A(NODE,NODE-1)=-1
        END IF
        IF COL<N THEN
            A(NODE,NODE)=A(NODE,NODE)+1
            A(NODE,NODE+1)=-1
        END IF
    NEXT
  NEXT

  AR=2 : AC=2 : A=AC+N*(AR-1)
  BR=7 : BC=8 : B=BC+N*(BR-1)
  A(A,NN+1)=-1
  A(B,NN+1)=1

  PRINT "Nodes ",A,B

  // solve linear system
  // using Gauss-Seidel method
  // with pivoting
  R=NN

  FOR J=1 TO R
    FOR I=J TO R
      IF A(I,J)<>0 BREAK
    NEXT
    IF I=R+1 THEN
      PRINT "No solution!"
      END
    END IF
    FOR K=1 TO R+1
      T = A(J,K)
      A(J,K) = A(I,K)
      A(I,K) = T
    NEXT
    Y=1/A(J,J)
    FOR K=1 TO R+1
      A(J,K)=Y*A(J,K)
    NEXT
    FOR I=1 TO R
      IF I<>J THEN
         Y=-A(I,J)
         FOR K=1 TO R+1
            A(I,K)=A(I,K)+Y*A(J,K)
         NEXT
      END IF
    NEXT
  NEXT
  PRINT "Resistence = "; : PRINT ABS(A(A,NN+1)-A(B,NN+1)) USING "%1.13f"
```



## zkl

Uses the GNU Scientific Library.

```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
fcn onGrid(i,j,p,q){ ((0<=i<p) and (0<=j<q)) }
fcn gridResistor(p,q, ai,aj, bi,bj){
   n,A := p*q, GSL.Matrix(n,n);  // zero filled
   foreach i,j in (p,q){
      k:=i*q + j;
      if(i==ai and j==aj) A[k,k]=1;
      else{
	 c:=0;
	 if(onGrid(i+1,j,   p,q)){ c+=1; A[k, k+q]=-1 }
	 if(onGrid(i-1,j,   p,q)){ c+=1; A[k, k-q]=-1 }
	 if(onGrid(i,  j+1, p,q)){ c+=1; A[k, k+1]=-1 }
	 if(onGrid(i,  j-1, p,q)){ c+=1; A[k, k-1]=-1 }
	 A[k,k]=c;
      }
   }
   b:=GSL.Vector(n);  // zero filled
   b[k:=bi*q + bj]=1;
   A.AxEQb(b)[k];
}
```


```zkl
gridResistor(10,10, 1,1, 7,6).println();
```

```txt

1.60899

```

