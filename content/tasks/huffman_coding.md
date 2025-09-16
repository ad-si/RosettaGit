+++
title = "Huffman coding"
description = ""
date = 2019-05-16T19:52:41Z
aliases = []
[extra]
id = 4035
[taxonomies]
categories = ["task", "Compression"]
tags = []
languages = [
  "ada",
  "add_the_new_node_to_the_queue",
  "bbc_basic",
  "bracmat",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "create_a_new_internal_node_with_these_two_nodes_as_children_and_with_probability_equal_to_the_sum_of_the_two_nodes_probabilities",
  "csharp",
  "d",
  "eiffel",
  "erlang",
  "fantom",
  "fortran",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "m2000_interpreter",
  "nim",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "red",
  "remove_the_node_of_highest_priority_lowest_probability_twice_to_get_two_nodes",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "setl",
  "sidef",
  "standard_ml",
  "swift",
  "tcl",
  "ursala",
  "zkl",
]
+++

Huffman encoding is a way to assign binary codes to symbols that reduces the overall number of bits used to encode a typical string of those symbols.

For example, if you use letters as symbols and have details of the frequency of occurrence of those letters in typical strings, then you could just encode each letter with a fixed number of bits, such as in ASCII codes. You can do better than this by encoding more frequently occurring letters such as e and a, with smaller bit strings; and  less frequently occurring letters such as q and x with longer bit strings.

Any string of letters will be encoded as a string of bits that are no-longer of the same length per letter. To successfully decode such as string, the  smaller codes assigned to letters such as 'e' cannot occur as a prefix in the larger codes such as that for 'x'.
:If you were to assign a code 01 for 'e' and code 011 for 'x', then if the bits to decode started as 011... then you would not know if you should decode an 'e' or an 'x'.

The Huffman coding scheme takes each symbol and its weight (or frequency of occurrence), and generates proper encodings for each symbol taking account of the weights of each symbol, so that higher weighted symbols have fewer bits in their encoding. (See the [[wp:Huffman_coding|WP article]] for more information).

A Huffman encoding can be computed by first creating a tree of nodes:

[[Image:Huffman_coding_example.jpg|right|250px]]
# Create a leaf node for each symbol and add it to the [[priority queue]].
# While there is more than one node in the queue:
## Remove the node of highest priority (lowest probability) twice to get two nodes.
## Create a new internal node with these two nodes as children and with probability equal to the sum of the two nodes' probabilities.
## Add the new node to the queue.
# The remaining node is the root node and the tree is complete.



Traverse the constructed binary tree from root to leaves assigning and accumulating a '0' for one branch and a '1' for the other at each node. The accumulated  zeros and ones at each leaf constitute a Huffman encoding for those symbols and weights:


## Task

Using the characters and their frequency from the string:
:::::   ''' '' this is an example for huffman encoding '' '''
create a program to generate a Huffman encoding for each character as a table.





## Ada

huffman.ads:

```Ada
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Finalization;
generic
   type Symbol_Type is private;
   with function "<" (Left, Right : Symbol_Type) return Boolean is <>;
   with procedure Put (Item : Symbol_Type);
   type Symbol_Sequence is array (Positive range <>) of Symbol_Type;
   type Frequency_Type is private;
   with function "+" (Left, Right : Frequency_Type) return Frequency_Type
     is <>;
   with function "<" (Left, Right : Frequency_Type) return Boolean is <>;
package Huffman is
   -- bits = booleans (true/false = 1/0)
   type Bit_Sequence is array (Positive range <>) of Boolean;
   Zero_Sequence : constant Bit_Sequence (1 .. 0) := (others => False);
   -- output the sequence
   procedure Put (Code : Bit_Sequence);

   -- type for freqency map
   package Frequency_Maps is new Ada.Containers.Ordered_Maps
     (Element_Type => Frequency_Type,
      Key_Type     => Symbol_Type);

   type Huffman_Tree is private;
   -- create a huffman tree from frequency map
   procedure Create_Tree
     (Tree        : out Huffman_Tree;
      Frequencies : Frequency_Maps.Map);
   -- encode a single symbol
   function Encode
     (Tree   : Huffman_Tree;
      Symbol : Symbol_Type)
      return   Bit_Sequence;
   -- encode a symbol sequence
   function Encode
     (Tree    : Huffman_Tree;
      Symbols : Symbol_Sequence)
      return    Bit_Sequence;
   -- decode a bit sequence
   function Decode
     (Tree : Huffman_Tree;
      Code : Bit_Sequence)
      return Symbol_Sequence;
   -- dump the encoding table
   procedure Dump_Encoding (Tree : Huffman_Tree);
private
   -- type for encoding map
   package Encoding_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Element_Type => Bit_Sequence,
      Key_Type     => Symbol_Type);

   type Huffman_Node;
   type Node_Access is access Huffman_Node;
   -- a node is either internal (left_child/right_child used)
   -- or a leaf (left_child/right_child are null)
   type Huffman_Node is record
      Frequency   : Frequency_Type;
      Left_Child  : Node_Access := null;
      Right_Child : Node_Access := null;
      Symbol      : Symbol_Type;
   end record;
   -- create a leaf node
   function Create_Node
     (Symbol    : Symbol_Type;
      Frequency : Frequency_Type)
      return      Node_Access;
   -- create an internal node
   function Create_Node (Left, Right : Node_Access) return Node_Access;
   -- fill the encoding map
   procedure Fill
     (The_Node : Node_Access;
      Map      : in out Encoding_Maps.Map;
      Prefix   : Bit_Sequence);

   -- huffman tree has a tree and an encoding map
   type Huffman_Tree is new Ada.Finalization.Controlled with record
      Tree : Node_Access       := null;
      Map  : Encoding_Maps.Map := Encoding_Maps.Empty_Map;
   end record;
   -- free memory after finalization
   overriding procedure Finalize (Object : in out Huffman_Tree);
end Huffman;
```


huffman.adb:

```Ada
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;
package body Huffman is
   package Node_Vectors is new Ada.Containers.Vectors
     (Element_Type => Node_Access,
      Index_Type   => Positive);

   function "<" (Left, Right : Node_Access) return Boolean is
   begin
      -- compare frequency
      if Left.Frequency < Right.Frequency then
         return True;
      elsif Right.Frequency < Left.Frequency then
         return False;
      end if;
      -- same frequency, choose leaf node
      if Left.Left_Child = null and then Right.Left_Child /= null then
         return True;
      elsif Left.Left_Child /= null and then Right.Left_Child = null then
         return False;
      end if;
      -- same frequency, same node type (internal/leaf)
      if Left.Left_Child /= null then
         -- for internal nodes, compare left children, then right children
         if Left.Left_Child < Right.Left_Child then
            return True;
         elsif Right.Left_Child < Left.Left_Child then
            return False;
         else
            return Left.Right_Child < Right.Right_Child;
         end if;
      else
         -- for leaf nodes, compare symbol
         return Left.Symbol < Right.Symbol;
      end if;
   end "<";
   package Node_Vector_Sort is new Node_Vectors.Generic_Sorting;

   procedure Create_Tree
     (Tree        : out Huffman_Tree;
      Frequencies : Frequency_Maps.Map) is
      Node_Queue : Node_Vectors.Vector := Node_Vectors.Empty_Vector;
   begin
      -- insert all leafs into the queue
      declare
         use Frequency_Maps;
         Position : Cursor      := Frequencies.First;
         The_Node : Node_Access := null;
      begin
         while Position /= No_Element loop
            The_Node :=
              Create_Node
                (Symbol    => Key (Position),
                 Frequency => Element (Position));
            Node_Queue.Append (The_Node);
            Next (Position);
         end loop;
      end;
      -- sort by frequency (see "<")
      Node_Vector_Sort.Sort (Node_Queue);
      -- iterate over all elements
      while not Node_Queue.Is_Empty loop
         declare
            First : constant Node_Access := Node_Queue.First_Element;
         begin
            Node_Queue.Delete_First;
            -- if we only have one node left, it is the root node of the tree
            if Node_Queue.Is_Empty then
               Tree.Tree := First;
            else
               -- create new internal node with two smallest frequencies
               declare
                  Second : constant Node_Access := Node_Queue.First_Element;
               begin
                  Node_Queue.Delete_First;
                  Node_Queue.Append (Create_Node (First, Second));
               end;
               Node_Vector_Sort.Sort (Node_Queue);
            end if;
         end;
      end loop;
      -- fill encoding map
      Fill (The_Node => Tree.Tree, Map => Tree.Map, Prefix => Zero_Sequence);
   end Create_Tree;

   -- create leaf node
   function Create_Node
     (Symbol    : Symbol_Type;
      Frequency : Frequency_Type)
      return      Node_Access
   is
      Result : Node_Access := new Huffman_Node;
   begin
      Result.Frequency := Frequency;
      Result.Symbol    := Symbol;
      return Result;
   end Create_Node;

   -- create internal node
   function Create_Node (Left, Right : Node_Access) return Node_Access is
      Result : Node_Access := new Huffman_Node;
   begin
      Result.Frequency   := Left.Frequency + Right.Frequency;
      Result.Left_Child  := Left;
      Result.Right_Child := Right;
      return Result;
   end Create_Node;

   -- fill encoding map
   procedure Fill
     (The_Node : Node_Access;
      Map      : in out Encoding_Maps.Map;
      Prefix   : Bit_Sequence) is
   begin
      if The_Node.Left_Child /= null then
         -- append false (0) for left child
         Fill (The_Node.Left_Child, Map, Prefix & False);
         -- append true (1) for right child
         Fill (The_Node.Right_Child, Map, Prefix & True);
      else
         -- leaf node reached, prefix = code for symbol
         Map.Insert (The_Node.Symbol, Prefix);
      end if;
   end Fill;

   -- free memory after finalization
   overriding procedure Finalize (Object : in out Huffman_Tree) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Name   => Node_Access,
         Object => Huffman_Node);
      -- recursively free all nodes
      procedure Recursive_Free (The_Node : in out Node_Access) is
      begin
         -- free node if it is a leaf
         if The_Node.Left_Child = null then
            Free (The_Node);
         else
            -- free left and right child if node is internal
            Recursive_Free (The_Node.Left_Child);
            Recursive_Free (The_Node.Right_Child);
            -- free node afterwards
            Free (The_Node);
         end if;
      end Recursive_Free;
   begin
      -- recursively free root node
      Recursive_Free (Object.Tree);
   end Finalize;

   -- encode single symbol
   function Encode
     (Tree   : Huffman_Tree;
      Symbol : Symbol_Type)
      return   Bit_Sequence
   is
   begin
      -- simply lookup in map
      return Tree.Map.Element (Symbol);
   end Encode;

   -- encode symbol sequence
   function Encode
     (Tree    : Huffman_Tree;
      Symbols : Symbol_Sequence)
      return    Bit_Sequence
   is
   begin
      -- only one element
      if Symbols'Length = 1 then
         -- see above
         return Encode (Tree, Symbols (Symbols'First));
      else
         -- encode first element, append result of recursive call
         return Encode (Tree, Symbols (Symbols'First)) &
         Encode (Tree, Symbols (Symbols'First + 1 .. Symbols'Last));
      end if;
   end Encode;

   -- decode a bit sequence
   function Decode
     (Tree : Huffman_Tree;
      Code : Bit_Sequence)
      return Symbol_Sequence
   is
      -- maximum length = code length
      Result   : Symbol_Sequence (1 .. Code'Length);
      -- last used index of result
      Last     : Natural     := 0;
      The_Node : Node_Access := Tree.Tree;
   begin
      -- iterate over the code
      for I in Code'Range loop
         -- if current element is true, descent the right branch
         if Code (I) then
            The_Node := The_Node.Right_Child;
         else
            -- false: descend left branch
            The_Node := The_Node.Left_Child;
         end if;
         if The_Node.Left_Child = null then
            -- reached leaf node: append symbol to result
            Last          := Last + 1;
            Result (Last) := The_Node.Symbol;
            -- reset current node to root
            The_Node := Tree.Tree;
         end if;
      end loop;
      -- return subset of result array
      return Result (1 .. Last);
   end Decode;

   -- output a bit sequence
   procedure Put (Code : Bit_Sequence) is
      package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   begin
      for I in Code'Range loop
         if Code (I) then
            -- true = 1
            Int_IO.Put (1, 0);
         else
            -- false = 0
            Int_IO.Put (0, 0);
         end if;
      end loop;
      Ada.Text_IO.New_Line;
   end Put;

   -- dump encoding map
   procedure Dump_Encoding (Tree : Huffman_Tree) is
      use type Encoding_Maps.Cursor;
      Position : Encoding_Maps.Cursor := Tree.Map.First;
   begin
      -- iterate map
      while Position /= Encoding_Maps.No_Element loop
         -- key
         Put (Encoding_Maps.Key (Position));
         Ada.Text_IO.Put (" = ");
         -- code
         Put (Encoding_Maps.Element (Position));
         Encoding_Maps.Next (Position);
      end loop;
   end Dump_Encoding;
end Huffman;
```


example main.adb:

```Ada
with Ada.Text_IO;
with Huffman;
procedure Main is
   package Char_Natural_Huffman_Tree is new Huffman
     (Symbol_Type => Character,
      Put => Ada.Text_IO.Put,
      Symbol_Sequence => String,
      Frequency_Type => Natural);
   Tree         : Char_Natural_Huffman_Tree.Huffman_Tree;
   Frequencies  : Char_Natural_Huffman_Tree.Frequency_Maps.Map;
   Input_String : constant String :=
     "this is an example for huffman encoding";
begin
   -- build frequency map
   for I in Input_String'Range loop
      declare
         use Char_Natural_Huffman_Tree.Frequency_Maps;
         Position : constant Cursor := Frequencies.Find (Input_String (I));
      begin
         if Position = No_Element then
            Frequencies.Insert (Key => Input_String (I), New_Item => 1);
         else
            Frequencies.Replace_Element
              (Position => Position,
               New_Item => Element (Position) + 1);
         end if;
      end;
   end loop;

   -- create huffman tree
   Char_Natural_Huffman_Tree.Create_Tree
     (Tree        => Tree,
      Frequencies => Frequencies);

   -- dump encodings
   Char_Natural_Huffman_Tree.Dump_Encoding (Tree => Tree);

   -- encode example string
   declare
      Code : constant Char_Natural_Huffman_Tree.Bit_Sequence :=
        Char_Natural_Huffman_Tree.Encode
          (Tree    => Tree,
           Symbols => Input_String);
   begin
      Char_Natural_Huffman_Tree.Put (Code);
      Ada.Text_IO.Put_Line
        (Char_Natural_Huffman_Tree.Decode (Tree => Tree, Code => Code));
   end;
end Main;
```


```txt
  = 101
a = 1001
c = 01010
d = 01011
e = 1100
f = 1101
g = 01100
h = 11111
i = 1110
l = 01101
m = 0010
n = 000
o = 0011
p = 01110
r = 01111
s = 0100
t = 10000
u = 10001
x = 11110
1000011111111001001011110010010110010001011100111101001001001110011011100101110100110111110111111100011101110100101001000101110000001010001101011111000001100
this is an example for huffman encoding
```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"SORTSALIB"
      SortUp% = FN_sortSAinit(0,0) : REM Ascending
      SortDn% = FN_sortSAinit(1,0) : REM Descending

      Text$ = "this is an example for huffman encoding"

      DIM tree{(127) ch&, num%, lkl%, lkr%}
      FOR i% = 1 TO LEN(Text$)
        c% = ASCMID$(Text$,i%)
        tree{(c%)}.ch& = c%
        tree{(c%)}.num% += 1
      NEXT

      C% = DIM(tree{()},1) + 1
      CALL SortDn%, tree{()}, tree{(0)}.num%
      FOR i% = 0 TO DIM(tree{()},1)
        IF tree{(i%)}.num% = 0 EXIT FOR
      NEXT
      size% = i%

      linked% = 0
      REPEAT
        C% = size%
        CALL SortUp%, tree{()}, tree{(0)}.num%
        i% = 0 : WHILE tree{(i%)}.lkl% OR tree{(i%)}.lkr% i% += 1 : ENDWHILE
        tree{(i%)}.lkl% = size%
        j% = 0 : WHILE tree{(j%)}.lkl% OR tree{(j%)}.lkr% j% += 1 : ENDWHILE
        tree{(j%)}.lkr% = size%
        linked% += 2
        tree{(size%)}.num% = tree{(i%)}.num% + tree{(j%)}.num%
        size% += 1
      UNTIL linked% = (size% - 1)

      FOR i% = size% - 1 TO 0 STEP -1
        IF tree{(i%)}.ch& THEN
          h$ = ""
          j% = i%
          REPEAT
            CASE TRUE OF
              WHEN tree{(j%)}.lkl% <> 0:
                h$ = "0" + h$
                j% = tree{(j%)}.lkl%
              WHEN tree{(j%)}.lkr% <> 0:
                h$ = "1" + h$
                j% = tree{(j%)}.lkr%
              OTHERWISE:
                EXIT REPEAT
            ENDCASE
          UNTIL FALSE
          VDU tree{(i%)}.ch& : PRINT "  " h$
        ENDIF
      NEXT
      END
```

```txt

   101
n  000
e  1110
f  1101
a  1100
i  1011
s  0110
m  0101
h  0100
o  0011
c  0010
l  0001
r  0000
x  11111
p  11110
d  11101
u  11100
g  11011
t  11010

```


## Bracmat


```bracmat
( "this is an example for huffman encoding":?S
& 0:?chars
& 0:?p
& ( @( !S
     :   ?
         ( [!p %?char [?p ?
         & !char+!chars:?chars
         & ~
         )
     )
  |
  )
& 0:?prioritized
&   whl
  ' ( !chars:?n*%@?w+?chars
    & (!n.!w)+!prioritized:?prioritized
    )
&   whl
  ' ( !prioritized:(?p.?x)+(?q.?y)+?nprioritized
    & (!p+!q.(!p.0,!x)+(!q.1,!y))+!nprioritized:?prioritized
    )
& 0:?L
& ( walk
  =   bits tree bit subtree
    .   !arg:(?bits.?tree)
      &   whl
        ' ( !tree:(?p.?bit,?subtree)+?tree
          & (   !subtree:@
              & (!subtree.str$(!bits !bit))+!L:?L
            | walk$(!bits !bit.!subtree)
            )
          )
  )
& !prioritized:(?.?prioritized)
& walk$(.!prioritized)
& lst$L
& :?encoded
& 0:?p
& ( @( !S
     :   ?
         ( [!p %?char [?p ?
         & !L:?+(!char.?code)+?
         & !encoded !code:?encoded
         & ~
         )
     )
  | out$(str$!encoded)
  )
& ( decode
  =   char bits
    .       !L
          : ?+(?char.?bits&@(!arg:!bits ?arg))+?
        & !char decode$!arg
      | !arg
  )
& out$("decoded:" str$(decode$(str$!encoded)));
```

```txt
(L=
  (" ".101)
+ (a.1001)
+ (c.01010)
+ (d.01011)
+ (e.1100)
+ (f.1101)
+ (g.01100)
+ (h.11111)
+ (i.1110)
+ (l.01101)
+ (m.0010)
+ (n.000)
+ (o.0011)
+ (p.01110)
+ (r.01111)
+ (s.0100)
+ (t.10000)
+ (u.10001)
+ (x.11110));
1000011111111001001011110010010110010001011100111101001001001110011011100101110100110111110111111100011101110100101001000101110000001010001101011111000001100
decoded: this is an example for huffman encoding

```


## C


This code lacks a lot of needed checkings, especially for memory allocation.


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BYTES 256

struct huffcode {
  int nbits;
  int code;
};
typedef struct huffcode huffcode_t;

struct huffheap {
  int *h;
  int n, s, cs;
  long *f;
};
typedef struct huffheap heap_t;

/* heap handling funcs */
static heap_t *_heap_create(int s, long *f)
{
  heap_t *h;
  h = malloc(sizeof(heap_t));
  h->h = malloc(sizeof(int)*s);
  h->s = h->cs = s;
  h->n = 0;
  h->f = f;
  return h;
}

static void _heap_destroy(heap_t *heap)
{
  free(heap->h);
  free(heap);
}

#define swap_(I,J) do { int t_; t_ = a[(I)];	\
      a[(I)] = a[(J)]; a[(J)] = t_; } while(0)
static void _heap_sort(heap_t *heap)
{
  int i=1, j=2; /* gnome sort */
  int *a = heap->h;

  while(i < heap->n) { /* smaller values are kept at the end */
    if ( heap->f[a[i-1]] >= heap->f[a[i]] ) {
      i = j; j++;
    } else {
      swap_(i-1, i);
      i--;
      i = (i==0) ? j++ : i;
    }
  }
}
#undef swap_

static void _heap_add(heap_t *heap, int c)
{
  if ( (heap->n + 1) > heap->s ) {
    heap->h = realloc(heap->h, heap->s + heap->cs);
    heap->s += heap->cs;
  }
  heap->h[heap->n] = c;
  heap->n++;
  _heap_sort(heap);
}

static int _heap_remove(heap_t *heap)
{
  if ( heap->n > 0 ) {
    heap->n--;
    return heap->h[heap->n];
  }
  return -1;
}

/* huffmann code generator */
huffcode_t **create_huffman_codes(long *freqs)
{
  huffcode_t **codes;
  heap_t *heap;
  long efreqs[BYTES*2];
  int preds[BYTES*2];
  int i, extf=BYTES;
  int r1, r2;

  memcpy(efreqs, freqs, sizeof(long)*BYTES);
  memset(&efreqs[BYTES], 0, sizeof(long)*BYTES);

  heap = _heap_create(BYTES*2, efreqs);
  if ( heap == NULL ) return NULL;

  for(i=0; i < BYTES; i++) if ( efreqs[i] > 0 ) _heap_add(heap, i);

  while( heap->n > 1 )
  {
    r1 = _heap_remove(heap);
    r2 = _heap_remove(heap);
    efreqs[extf] = efreqs[r1] + efreqs[r2];
    _heap_add(heap, extf);
    preds[r1] = extf;
    preds[r2] = -extf;
    extf++;
  }
  r1 = _heap_remove(heap);
  preds[r1] = r1;
  _heap_destroy(heap);

  codes = malloc(sizeof(huffcode_t *)*BYTES);

  int bc, bn, ix;
  for(i=0; i < BYTES; i++) {
    bc=0; bn=0;
    if ( efreqs[i] == 0 ) { codes[i] = NULL; continue; }
    ix = i;
    while( abs(preds[ix]) != ix ) {
      bc |= ((preds[ix] >= 0) ? 1 : 0 ) << bn;
      ix = abs(preds[ix]);
      bn++;
    }
    codes[i] = malloc(sizeof(huffcode_t));
    codes[i]->nbits = bn;
    codes[i]->code = bc;
  }
  return codes;
}

void free_huffman_codes(huffcode_t **c)
{
  int i;

  for(i=0; i < BYTES; i++) free(c[i]);
  free(c);
}

#define MAXBITSPERCODE 100

void inttobits(int c, int n, char *s)
{
  s[n] = 0;
  while(n > 0) {
    s[n-1] = (c%2) + '0';
    c >>= 1; n--;
  }
}

const char *test = "this is an example for huffman encoding";

int main()
{
  huffcode_t **r;
  int i;
  char strbit[MAXBITSPERCODE];
  const char *p;
  long freqs[BYTES];

  memset(freqs, 0, sizeof freqs);

  p = test;
  while(*p != '\0') freqs[*p++]++;

  r = create_huffman_codes(freqs);

  for(i=0; i < BYTES; i++) {
    if ( r[i] != NULL ) {
      inttobits(r[i]->code, r[i]->nbits, strbit);
      printf("%c (%d) %s\n", i, r[i]->code, strbit);
    }
  }

  free_huffman_codes(r);

  return 0;
}
```


### Alternative

Using a simple heap-based priority queue.  Heap is an array, while ndoe tree is done by binary links.

```c
#include <stdio.h>
#include <string.h>

typedef struct node_t {
	struct node_t *left, *right;
	int freq;
	char c;
} *node;

struct node_t pool[256] = {{0}};
node qqq[255], *q = qqq - 1;
int n_nodes = 0, qend = 1;
char *code[128] = {0}, buf[1024];

node new_node(int freq, char c, node a, node b)
{
	node n = pool + n_nodes++;
	if (freq) n->c = c, n->freq = freq;
	else {
		n->left = a, n->right = b;
		n->freq = a->freq + b->freq;
	}
	return n;
}

/* priority queue */
void qinsert(node n)
{
	int j, i = qend++;
	while ((j = i / 2)) {
		if (q[j]->freq <= n->freq) break;
		q[i] = q[j], i = j;
	}
	q[i] = n;
}

node qremove()
{
	int i, l;
	node n = q[i = 1];

	if (qend < 2) return 0;
	qend--;
	while ((l = i * 2) < qend) {
		if (l + 1 < qend && q[l + 1]->freq < q[l]->freq) l++;
		q[i] = q[l], i = l;
	}
	q[i] = q[qend];
	return n;
}

/* walk the tree and put 0s and 1s */
void build_code(node n, char *s, int len)
{
	static char *out = buf;
	if (n->c) {
		s[len] = 0;
		strcpy(out, s);
		code[n->c] = out;
		out += len + 1;
		return;
	}

	s[len] = '0'; build_code(n->left,  s, len + 1);
	s[len] = '1'; build_code(n->right, s, len + 1);
}

void init(const char *s)
{
	int i, freq[128] = {0};
	char c[16];

	while (*s) freq[(int)*s++]++;

	for (i = 0; i < 128; i++)
		if (freq[i]) qinsert(new_node(freq[i], i, 0, 0));

	while (qend > 2)
		qinsert(new_node(0, 0, qremove(), qremove()));

	build_code(q[1], c, 0);
}

void encode(const char *s, char *out)
{
	while (*s) {
		strcpy(out, code[*s]);
		out += strlen(code[*s++]);
	}
}

void decode(const char *s, node t)
{
	node n = t;
	while (*s) {
		if (*s++ == '0') n = n->left;
		else n = n->right;

		if (n->c) putchar(n->c), n = t;
	}

	putchar('\n');
	if (t != n) printf("garbage input\n");
}

int main(void)
{
	int i;
	const char *str = "this is an example for huffman encoding";
        char buf[1024];

	init(str);
	for (i = 0; i < 128; i++)
		if (code[i]) printf("'%c': %s\n", i, code[i]);

	encode(str, buf);
	printf("encoded: %s\n", buf);

	printf("decoded: ");
	decode(buf, q[1]);

	return 0;
}
```

```txt
' ': 000
'a': 1000
'c': 01101
'd': 01100
'e': 0101
'f': 0010
'g': 010000
'h': 1101
'i': 0011
'l': 010001
'm': 1111
'n': 101
'o': 1110
'p': 10011
'r': 10010
's': 1100
't': 01111
'u': 01110
'x': 01001
encoded: 0111111010011110000000111100000100010100001010100110001111100110100010101000001011101001000011010111000100010111110001010000101101011011110011000011101010000
decoded: this is an example for huffman encoding
```



## C#


```c#
using System;
using System.Collections.Generic;

namespace Huffman_Encoding
{
    public class PriorityQueue<T> where T : IComparable
    {
        protected List<T> LstHeap = new List<T>();

        public virtual int Count
        {
            get { return LstHeap.Count; }
        }

        public virtual void Add(T val)
        {
            LstHeap.Add(val);
            SetAt(LstHeap.Count - 1, val);
            UpHeap(LstHeap.Count - 1);
        }

        public virtual T Peek()
        {
            if (LstHeap.Count == 0)
            {
                throw new IndexOutOfRangeException("Peeking at an empty priority queue");
            }

            return LstHeap[0];
        }

        public virtual T Pop()
        {
            if (LstHeap.Count == 0)
            {
                throw new IndexOutOfRangeException("Popping an empty priority queue");
            }

            T valRet = LstHeap[0];

            SetAt(0, LstHeap[LstHeap.Count - 1]);
            LstHeap.RemoveAt(LstHeap.Count - 1);
            DownHeap(0);
            return valRet;
        }

        protected virtual void SetAt(int i, T val)
        {
            LstHeap[i] = val;
        }

        protected bool RightSonExists(int i)
        {
            return RightChildIndex(i) < LstHeap.Count;
        }

        protected bool LeftSonExists(int i)
        {
            return LeftChildIndex(i) < LstHeap.Count;
        }

        protected int ParentIndex(int i)
        {
            return (i - 1) / 2;
        }

        protected int LeftChildIndex(int i)
        {
            return 2 * i + 1;
        }

        protected int RightChildIndex(int i)
        {
            return 2 * (i + 1);
        }

        protected T ArrayVal(int i)
        {
            return LstHeap[i];
        }

        protected T Parent(int i)
        {
            return LstHeap[ParentIndex(i)];
        }

        protected T Left(int i)
        {
            return LstHeap[LeftChildIndex(i)];
        }

        protected T Right(int i)
        {
            return LstHeap[RightChildIndex(i)];
        }

        protected void Swap(int i, int j)
        {
            T valHold = ArrayVal(i);
            SetAt(i, LstHeap[j]);
            SetAt(j, valHold);
        }

        protected void UpHeap(int i)
        {
            while (i > 0 && ArrayVal(i).CompareTo(Parent(i)) > 0)
            {
                Swap(i, ParentIndex(i));
                i = ParentIndex(i);
            }
        }

        protected void DownHeap(int i)
        {
            while (i >= 0)
            {
                int iContinue = -1;

                if (RightSonExists(i) && Right(i).CompareTo(ArrayVal(i)) > 0)
                {
                    iContinue = Left(i).CompareTo(Right(i)) < 0 ? RightChildIndex(i) : LeftChildIndex(i);
                }
                else if (LeftSonExists(i) && Left(i).CompareTo(ArrayVal(i)) > 0)
                {
                    iContinue = LeftChildIndex(i);
                }

                if (iContinue >= 0 && iContinue < LstHeap.Count)
                {
                    Swap(i, iContinue);
                }

                i = iContinue;
            }
        }
    }

    internal class HuffmanNode<T> : IComparable
    {
        internal HuffmanNode(double probability, T value)
        {
            Probability = probability;
            LeftSon = RightSon = Parent = null;
            Value = value;
            IsLeaf = true;
        }

        internal HuffmanNode(HuffmanNode<T> leftSon, HuffmanNode<T> rightSon)
        {
            LeftSon = leftSon;
            RightSon = rightSon;
            Probability = leftSon.Probability + rightSon.Probability;
            leftSon.IsZero = true;
            rightSon.IsZero = false;
            leftSon.Parent = rightSon.Parent = this;
            IsLeaf = false;
        }

        internal HuffmanNode<T> LeftSon { get; set; }
        internal HuffmanNode<T> RightSon { get; set; }
        internal HuffmanNode<T> Parent { get; set; }
        internal T Value { get; set; }
        internal bool IsLeaf { get; set; }

        internal bool IsZero { get; set; }

        internal int Bit
        {
            get { return IsZero ? 0 : 1; }
        }

        internal bool IsRoot
        {
            get { return Parent == null; }
        }

        internal double Probability { get; set; }

        public int CompareTo(object obj)
        {
            return -Probability.CompareTo(((HuffmanNode<T>) obj).Probability);
        }
    }

    public class Huffman<T> where T : IComparable
    {
        private readonly Dictionary<T, HuffmanNode<T>> _leafDictionary = new Dictionary<T, HuffmanNode<T>>();
        private readonly HuffmanNode<T> _root;

        public Huffman(IEnumerable<T> values)
        {
            var counts = new Dictionary<T, int>();
            var priorityQueue = new PriorityQueue<HuffmanNode<T>>();
            int valueCount = 0;

            foreach (T value in values)
            {
                if (!counts.ContainsKey(value))
                {
                    counts[value] = 0;
                }
                counts[value]++;
                valueCount++;
            }

            foreach (T value in counts.Keys)
            {
                var node = new HuffmanNode<T>((double) counts[value] / valueCount, value);
                priorityQueue.Add(node);
                _leafDictionary[value] = node;
            }

            while (priorityQueue.Count > 1)
            {
                HuffmanNode<T> leftSon = priorityQueue.Pop();
                HuffmanNode<T> rightSon = priorityQueue.Pop();
                var parent = new HuffmanNode<T>(leftSon, rightSon);
                priorityQueue.Add(parent);
            }

            _root = priorityQueue.Pop();
            _root.IsZero = false;
        }

        public List<int> Encode(T value)
        {
            var returnValue = new List<int>();
            Encode(value, returnValue);
            return returnValue;
        }

        public void Encode(T value, List<int> encoding)
        {
            if (!_leafDictionary.ContainsKey(value))
            {
                throw new ArgumentException("Invalid value in Encode");
            }
            HuffmanNode<T> nodeCur = _leafDictionary[value];
            var reverseEncoding = new List<int>();
            while (!nodeCur.IsRoot)
            {
                reverseEncoding.Add(nodeCur.Bit);
                nodeCur = nodeCur.Parent;
            }

            reverseEncoding.Reverse();
            encoding.AddRange(reverseEncoding);
        }

        public List<int> Encode(IEnumerable<T> values)
        {
            var returnValue = new List<int>();

            foreach (T value in values)
            {
                Encode(value, returnValue);
            }
            return returnValue;
        }

        public T Decode(List<int> bitString, ref int position)
        {
            HuffmanNode<T> nodeCur = _root;
            while (!nodeCur.IsLeaf)
            {
                if (position > bitString.Count)
                {
                    throw new ArgumentException("Invalid bitstring in Decode");
                }
                nodeCur = bitString[position++] == 0 ? nodeCur.LeftSon : nodeCur.RightSon;
            }
            return nodeCur.Value;
        }

        public List<T> Decode(List<int> bitString)
        {
            int position = 0;
            var returnValue = new List<T>();

            while (position != bitString.Count)
            {
                returnValue.Add(Decode(bitString, ref position));
            }
            return returnValue;
        }
    }

    internal class Program
    {
        private const string Example = "this is an example for huffman encoding";

        private static void Main()
        {
            var huffman = new Huffman<char>(Example);
            List<int> encoding = huffman.Encode(Example);
            List<char> decoding = huffman.Decode(encoding);
            var outString = new string(decoding.ToArray());
            Console.WriteLine(outString == Example ? "Encoding/decoding worked" : "Encoding/Decoding failed");

            var chars = new HashSet<char>(Example);
            foreach (char c in chars)
            {
                encoding = huffman.Encode(c);
                Console.Write("{0}:  ", c);
                foreach (int bit in encoding)
                {
                    Console.Write("{0}", bit);
                }
                Console.WriteLine();
            }
            Console.ReadKey();
        }
    }
}
```

[[File:CSharpHuffman.jpg]]


## C++


This code builds a tree to generate huffman codes, then prints the codes.


```cpp
#include <iostream>
#include <queue>
#include <map>
#include <climits> // for CHAR_BIT
#include <iterator>
#include <algorithm>

const int UniqueSymbols = 1 << CHAR_BIT;
const char* SampleString = "this is an example for huffman encoding";

typedef std::vector<bool> HuffCode;
typedef std::map<char, HuffCode> HuffCodeMap;

class INode
{
public:
    const int f;

    virtual ~INode() {}

protected:
    INode(int f) : f(f) {}
};

class InternalNode : public INode
{
public:
    INode *const left;
    INode *const right;

    InternalNode(INode* c0, INode* c1) : INode(c0->f + c1->f), left(c0), right(c1) {}
    ~InternalNode()
    {
        delete left;
        delete right;
    }
};

class LeafNode : public INode
{
public:
    const char c;

    LeafNode(int f, char c) : INode(f), c(c) {}
};

struct NodeCmp
{
    bool operator()(const INode* lhs, const INode* rhs) const { return lhs->f > rhs->f; }
};

INode* BuildTree(const int (&frequencies)[UniqueSymbols])
{
    std::priority_queue<INode*, std::vector<INode*>, NodeCmp> trees;

    for (int i = 0; i < UniqueSymbols; ++i)
    {
        if(frequencies[i] != 0)
            trees.push(new LeafNode(frequencies[i], (char)i));
    }
    while (trees.size() > 1)
    {
        INode* childR = trees.top();
        trees.pop();

        INode* childL = trees.top();
        trees.pop();

        INode* parent = new InternalNode(childR, childL);
        trees.push(parent);
    }
    return trees.top();
}

void GenerateCodes(const INode* node, const HuffCode& prefix, HuffCodeMap& outCodes)
{
    if (const LeafNode* lf = dynamic_cast<const LeafNode*>(node))
    {
        outCodes[lf->c] = prefix;
    }
    else if (const InternalNode* in = dynamic_cast<const InternalNode*>(node))
    {
        HuffCode leftPrefix = prefix;
        leftPrefix.push_back(false);
        GenerateCodes(in->left, leftPrefix, outCodes);

        HuffCode rightPrefix = prefix;
        rightPrefix.push_back(true);
        GenerateCodes(in->right, rightPrefix, outCodes);
    }
}

int main()
{
    // Build frequency table
    int frequencies[UniqueSymbols] = {0};
    const char* ptr = SampleString;
    while (*ptr != '\0')
        ++frequencies[*ptr++];

    INode* root = BuildTree(frequencies);

    HuffCodeMap codes;
    GenerateCodes(root, HuffCode(), codes);
    delete root;

    for (HuffCodeMap::const_iterator it = codes.begin(); it != codes.end(); ++it)
    {
        std::cout << it->first << " ";
        std::copy(it->second.begin(), it->second.end(),
                  std::ostream_iterator<bool>(std::cout));
        std::cout << std::endl;
    }
    return 0;
}
```


```txt
  110
a 1001
c 101010
d 10001
e 1111
f 1011
g 101011
h 0101
i 1110
l 01110
m 0011
n 000
o 0010
p 01000
r 01001
s 0110
t 01111
u 10100
x 10000
```



## Clojure

(Updated to 1.6 & includes pretty-printing).  Uses Java PriorityQueue

```clojure
(require '[clojure.pprint :refer :all])

(defn probs [s]
  (let [freqs (frequencies s) sum (apply + (vals freqs))]
    (into {} (map (fn [[k v]] [k (/ v sum)]) freqs))))

(defn init-pq [weighted-items]
  (let [comp (proxy [java.util.Comparator] []
                (compare [a b] (compare (:priority a) (:priority b))))
        pq (java.util.PriorityQueue. (count weighted-items) comp)]
    (doseq [[item prob] weighted-items] (.add pq { :symbol item, :priority prob }))
    pq))

(defn huffman-tree [pq]
  (while (> (.size pq) 1)
    (let [a (.poll pq) b (.poll pq)
	  new-node {:priority (+ (:priority a) (:priority b)) :left a :right b}]
      (.add pq new-node)))
  (.poll pq))

(defn symbol-map
  ([t] (symbol-map t ""))
  ([{:keys [symbol priority left right] :as t} code]
    (if symbol [{:symbol symbol :weight priority :code code}]
      (concat (symbol-map left (str code \0))
              (symbol-map right (str code \1))))))

(defn huffman-encode [items]
  (-> items probs init-pq huffman-tree symbol-map))

(defn display-huffman-encode [s]
  (->> s huffman-encode (sort-by :weight >) print-table))

(display-huffman-encode "this is an example for huffman encoding")
```


```txt
| :symbol | :weight |  :code |
|---------+---------+--------|
|         |    2/13 |    111 |
|       n |    4/39 |    011 |
|       a |    1/13 |   1001 |
|       e |    1/13 |   1011 |
|       i |    1/13 |   1100 |
|       f |    1/13 |   1101 |
|       h |    2/39 |   0001 |
|       s |    2/39 |   0010 |
|       m |    2/39 |   0100 |
|       o |    2/39 |   0101 |
|       d |    1/39 |  00000 |
|       t |    1/39 |  00001 |
|       c |    1/39 |  00110 |
|       x |    1/39 |  00111 |
|       u |    1/39 |  10000 |
|       l |    1/39 |  10001 |
|       r |    1/39 |  10100 |
|       g |    1/39 | 101010 |
|       p |    1/39 | 101011 |
```


### Alternate Version

Uses c.d.priority-map.  Creates a more shallow tree but appears to meet the requirements.

```clojure
(require '[clojure.data.priority-map :refer [priority-map-keyfn-by]])
(require '[clojure.pprint :refer [print-table]])

(defn init-pq [s]
  (let [c (count s)]
    (->> s frequencies
	(map (fn [[k v]] [k {:sym k :weight (/ v c)}]))
	(into (priority-map-keyfn-by :weight <)))))

(defn huffman-tree [pq]
  (letfn [(build-step
	   [pq]
	   (let [a (second (peek pq)) b (second (peek (pop pq)))
		 nn {:sym (str (:sym a) (:sym b))
		     :weight (+ (:weight a) (:weight b))
		     :left a :right b}]
	     (assoc (pop (pop pq)) (:sym nn) nn)))]
    (->> (iterate build-step pq)
	 (drop-while #(> (count %) 1))
	 first vals first)))

(defn symbol-map [m]
  (letfn [(sym-step
	   [{:keys [sym weight left right] :as m} code]
	   (cond (and left right) #(vector (trampoline sym-step left (str code \0))
					   (trampoline sym-step right (str code \1)))
		 left #(sym-step left (str code \0))
		 right #(sym-step right (str code \1))
		 :else {:sym sym :weight weight :code code}))]
    (trampoline sym-step m "")))

(defn huffman-encode [s]
  (->> s init-pq huffman-tree symbol-map flatten))

(defn display-huffman-encode [s]
  (->> s huffman-encode (sort-by :weight >) print-table))

(display-huffman-encode "this is an example for huffman encoding")
```

```txt
| :sym | :weight | :code |
|------+---------+-------|
|      |    2/13 |   101 |
|    n |    4/39 |   010 |
|    a |    1/13 |  1001 |
|    i |    1/13 |  1101 |
|    e |    1/13 |  1110 |
|    f |    1/13 |  1111 |
|    m |    2/39 |  0000 |
|    o |    2/39 |  0001 |
|    s |    2/39 |  0010 |
|    h |    2/39 | 11001 |
|    g |    1/39 | 00110 |
|    l |    1/39 | 00111 |
|    t |    1/39 | 01100 |
|    u |    1/39 | 01101 |
|    c |    1/39 | 01110 |
|    d |    1/39 | 01111 |
|    p |    1/39 | 10000 |
|    r |    1/39 | 10001 |
|    x |    1/39 | 11000 |
```



## CoffeeScript


```coffeescript

huffman_encoding_table = (counts) ->
  # counts is a hash where keys are characters and
  # values are frequencies;
  # return a hash where keys are codes and values
  # are characters

  build_huffman_tree = ->
    # returns a Huffman tree.  Each node has
    #   cnt: total frequency of all chars in subtree
    #   c: character to be encoded (leafs only)
    #   children: children nodes (branches only)
    q = min_queue()
    for c, cnt of counts
      q.enqueue cnt,
        cnt: cnt
        c: c
    while q.size() >= 2
      a = q.dequeue()
      b = q.dequeue()
      cnt = a.cnt + b.cnt
      node =
        cnt: cnt
        children: [a, b]
      q.enqueue cnt, node
    root = q.dequeue()

  root = build_huffman_tree()

  codes = {}
  encode = (node, code) ->
    if node.c?
      codes[code] = node.c
    else
      encode node.children[0], code + "0"
      encode node.children[1], code + "1"

  encode(root, "")
  codes

min_queue = ->
  # This is very non-optimized; you could use a binary heap for better
  # performance.  Items with smaller priority get dequeued first.
  arr = []
  enqueue: (priority, data) ->
    i = 0
    while i < arr.length
      if priority < arr[i].priority
        break
      i += 1
    arr.splice i, 0,
      priority: priority
      data: data
  dequeue: ->
    arr.shift().data
  size: -> arr.length
  _internal: ->
    arr

freq_count = (s) ->
  cnts = {}
  for c in s
    cnts[c] ?= 0
    cnts[c] += 1
  cnts

rpad = (s, n) ->
  while s.length < n
    s += ' '
  s

examples = [
  "this is an example for huffman encoding"
  "abcd"
  "abbccccddddddddeeeeeeeee"
]

for s in examples
  console.log "---- #{s}"
  counts = freq_count(s)
  huffman_table = huffman_encoding_table(counts)
  codes = (code for code of huffman_table).sort()
  for code in codes
    c = huffman_table[code]
    console.log "#{rpad(code, 5)}: #{c} (#{counts[c]})"
  console.log()

```


```txt

> coffee huffman.coffee
---- this is an example for huffman encoding
000  : n (4)
0010 : s (2)
0011 : m (2)
0100 : o (2)
01010: t (1)
01011: x (1)
01100: p (1)
01101: l (1)
01110: r (1)
01111: u (1)
10000: c (1)
10001: d (1)
1001 : i (3)
101  :   (6)
1100 : a (3)
1101 : e (3)
1110 : f (3)
11110: g (1)
11111: h (2)

---- abcd
00   : a (1)
01   : b (1)
10   : c (1)
11   : d (1)

---- abbccccddddddddeeeeeeeee
0    : e (9)
1000 : a (1)
1001 : b (2)
101  : c (4)
11   : d (8)

```



## Common Lisp


This implementation uses a tree built of <code>huffman-node</code>s,
and a hash table mapping from elements of the input sequence to <code>huffman-node</code>s.
The priority queue is implemented as a sorted list.
(For a more efficient implementation of a priority queue, see the [[Heapsort]] task.)


```lisp
(defstruct huffman-node
  (weight 0 :type number)
  (element nil :type t)
  (encoding nil :type (or null bit-vector))
  (left nil :type (or null huffman-node))
  (right nil :type (or null huffman-node)))

(defun initial-huffman-nodes (sequence &key (test 'eql))
  (let* ((length (length sequence))
         (increment (/ 1 length))
         (nodes (make-hash-table :size length :test test))
         (queue '()))
    (map nil #'(lambda (element)
                 (multiple-value-bind (node presentp) (gethash element nodes)
                   (if presentp
                     (incf (huffman-node-weight node) increment)
                     (let ((node (make-huffman-node :weight increment
                                                    :element element)))
                       (setf (gethash element nodes) node
                             queue (list* node queue))))))
         sequence)
    (values nodes (sort queue '< :key 'huffman-node-weight))))

(defun huffman-tree (sequence &key (test 'eql))
  (multiple-value-bind (nodes queue)
      (initial-huffman-nodes sequence :test test)
    (do () ((endp (rest queue)) (values nodes (first queue)))
      (destructuring-bind (n1 n2 &rest queue-rest) queue
        (let ((n3 (make-huffman-node
                   :left n1
                   :right n2
                   :weight (+ (huffman-node-weight n1)
                              (huffman-node-weight n2)))))
          (setf queue (merge 'list (list n3) queue-rest '<
                             :key 'huffman-node-weight)))))))1

(defun huffman-codes (sequence &key (test 'eql))
  (multiple-value-bind (nodes tree)
      (huffman-tree sequence :test test)
    (labels ((hc (node length bits)
               (let ((left (huffman-node-left node))
                     (right (huffman-node-right node)))
                 (cond
                  ((and (null left) (null right))
                   (setf (huffman-node-encoding node)
                         (make-array length :element-type 'bit
                                     :initial-contents (reverse bits))))
                  (t (hc left (1+ length) (list* 0 bits))
                     (hc right (1+ length) (list* 1 bits)))))))
      (hc tree 0 '())
      nodes)))

(defun print-huffman-code-table (nodes &optional (out *standard-output*))
  (format out "~&Element~10tWeight~20tCode")
  (loop for node being each hash-value of nodes
        do (format out "~&~s~10t~s~20t~s"
                   (huffman-node-element node)
                   (huffman-node-weight node)
                   (huffman-node-encoding node))))
```


Example:


```txt
> (print-huffman-code-table
   (huffman-codes "this is an example for huffman encoding"))
Element   Weight    Code
#\t       1/39      #*10010
#\d       1/39      #*01101
#\m       2/39      #*0100
#\f       1/13      #*1100
#\o       2/39      #*0111
#\x       1/39      #*100111
#\h       2/39      #*1000
#\a       1/13      #*1010
#\s       2/39      #*0101
#\c       1/39      #*00010
#\l       1/39      #*00001
#\u       1/39      #*00011
#\e       1/13      #*1101
#\n       4/39      #*001
#\g       1/39      #*01100
#\p       1/39      #*100110
#\i       1/13      #*1011
#\r       1/39      #*00000
#\Space   2/13      #*111
```



## D


```d
import std.stdio, std.algorithm, std.typecons, std.container, std.array;

auto encode(alias eq, R)(Group!(eq, R) sf) /*pure nothrow @safe*/ {
    auto heap = sf.map!(s => tuple(s[1], [tuple(s[0], "")]))
                .array.heapify!q{b < a};

    while (heap.length > 1) {
        auto lo = heap.front; heap.removeFront;
        auto hi = heap.front; heap.removeFront;
        lo[1].each!((ref pair) => pair[1] = '0' ~ pair[1]);
        hi[1].each!((ref pair) => pair[1] = '1' ~ pair[1]);
        heap.insert(tuple(lo[0] + hi[0], lo[1] ~ hi[1]));
    }
    return heap.front[1].schwartzSort!q{ tuple(a[1].length, a[0]) };
}

void main() /*@safe*/ {
    immutable s = "this is an example for huffman encoding"d;
    foreach (const p; s.dup.sort().group.encode)
        writefln("'%s'  %s", p[]);
}
```

```txt
' '  101
'n'  010
'a'  1001
'e'  1100
'f'  1101
'h'  0001
'i'  1110
'm'  0010
'o'  0011
's'  0111
'g'  00000
'l'  00001
'p'  01100
'r'  01101
't'  10000
'u'  10001
'x'  11110
'c'  111110
'd'  111111
```



## Eiffel

Adapted C# solution.

```eiffel

class HUFFMAN_NODE[T -> COMPARABLE]
inherit
	COMPARABLE
	redefine
		three_way_comparison
	end
create
	leaf_node, inner_node
feature {NONE}
	leaf_node (a_probability: REAL_64; a_value: T)
	do
		probability := a_probability
		value := a_value
		is_leaf := true

		left := void
		right := void
		parent := void
	end

	inner_node (a_left, a_right: HUFFMAN_NODE[T])
	do
		left := a_left
		right := a_right

		a_left.parent := Current
		a_right.parent := Current
		a_left.is_zero := true
		a_right.is_zero := false

		probability := a_left.probability + a_right.probability
		is_leaf := false
	end

feature
	probability: REAL_64
	value: detachable T


	is_leaf: BOOLEAN
	is_zero: BOOLEAN assign set_is_zero

	set_is_zero (a_value: BOOLEAN)
	do
		is_zero := a_value
	end

	left: detachable HUFFMAN_NODE[T]
	right: detachable HUFFMAN_NODE[T]
	parent: detachable HUFFMAN_NODE[T] assign set_parent

	set_parent (a_parent: detachable HUFFMAN_NODE[T])
	do
		parent := a_parent
	end

	is_root: BOOLEAN
	do
		Result := parent = void
	end

	bit_value: INTEGER
	do
		if is_zero then
			Result := 0
		else
			Result := 1
		end
	end
feature -- comparable implementation
	is_less alias "<" (other: like Current): BOOLEAN
	do
		Result := three_way_comparison (other) = -1
	end

	three_way_comparison (other: like Current): INTEGER
	do
		Result := -probability.three_way_comparison (other.probability)
	end
end

class HUFFMAN
create
	make
feature {NONE}
	make(a_string: STRING)
	require
		non_empty_string: a_string.count > 0
	local
		l_queue: HEAP_PRIORITY_QUEUE[HUFFMAN_NODE[CHARACTER]]
		l_counts: HASH_TABLE[INTEGER, CHARACTER]
		l_node: HUFFMAN_NODE[CHARACTER]
		l_left, l_right: HUFFMAN_NODE[CHARACTER]
	do
		create l_queue.make (a_string.count)
		create l_counts.make (10)

		across a_string as  char
		loop
			if not l_counts.has (char.item) then
				l_counts.put (0, char.item)
			end
			l_counts.replace (l_counts.at (char.item) + 1, char.item)
		end

		create leaf_dictionary.make(l_counts.count)

		across l_counts as kv
		loop
			create l_node.leaf_node ((kv.item * 1.0) / a_string.count, kv.key)
			l_queue.put (l_node)
			leaf_dictionary.put (l_node, kv.key)
		end

		from
		until
			l_queue.count <= 1
		loop
			l_left := l_queue.item
			l_queue.remove
			l_right := l_queue.item
			l_queue.remove

			create l_node.inner_node (l_left, l_right)
			l_queue.put (l_node)
		end

		root := l_queue.item
		root.is_zero := false
	end
feature
	root: HUFFMAN_NODE[CHARACTER]
	leaf_dictionary: HASH_TABLE[HUFFMAN_NODE[CHARACTER], CHARACTER]

	encode(a_value: CHARACTER): STRING
	require
		encodable: leaf_dictionary.has (a_value)
	local
		l_node: HUFFMAN_NODE[CHARACTER]
	do
		Result := ""
		if attached  leaf_dictionary.item (a_value) as attached_node then
			l_node := attached_node
			from

			until
				l_node.is_root
			loop
				Result.append_integer (l_node.bit_value)
				if attached l_node.parent as parent then
					l_node := parent
				end
			end

			Result.mirror
		end
	end
end

class
	APPLICATION
create
	make

feature {NONE}
	make -- entry point
	local
		l_str: STRING
		huff: HUFFMAN
		chars: BINARY_SEARCH_TREE_SET[CHARACTER]
	do
		l_str := "this is an example for huffman encoding"

		create huff.make (l_str)

		create chars.make
		chars.fill (l_str)

		from
			chars.start
		until
			chars.off
		loop
			print (chars.item.out + ": " + huff.encode (chars.item) + "%N")
			chars.forth
		end
	end
end

```


```txt
 : 101
a: 1001
c: 01110
d: 01111
e: 1111
f: 1100
g: 01001
h: 11101
i: 1101
l: 10001
m: 0010
n: 000
o: 0011
p: 10000
r: 11100
s: 0110
t: 01000
u: 01011
x: 01010

```


## Erlang

The main part of the code used here is extracted from [https://gist.github.com/rmies/2828351 Michel Rijnders' GitHubGist]. See also [http://codingpraxis.com/erlang/2012/10/23/huffman-coding-in-erlang.html his blog], for a complete description of the original module.

```erlang
-module(huffman).

-export([encode/1, decode/2, main/0]).

encode(Text)  ->
    Tree  = tree(freq_table(Text)),
    Dict = dict:from_list(codewords(Tree)),
    Code = << <<(dict:fetch(Char, Dict))/bitstring>> || Char <- Text >>,
    {Code, Tree, Dict}.

decode(Code, Tree) ->
    decode(Code, Tree, Tree, []).

main() ->
    {Code, Tree, Dict} = encode("this is an example for huffman encoding"),
    [begin
        io:format("~s: ",[[Key]]),
        print_bits(Value)
     end || {Key, Value} <- lists:sort(dict:to_list(Dict))],
    io:format("encoded: "),
    print_bits(Code),
    io:format("decoded: "),
    io:format("~s\n",[decode(Code, Tree)]).

decode(<<>>, _, _, Result) ->
    lists:reverse(Result);
decode(<<0:1, Rest/bits>>, Tree, {L = {_, _}, _R}, Result) ->
    decode(<<Rest/bits>>, Tree, L, Result);
decode(<<0:1, Rest/bits>>, Tree, {L, _R}, Result) ->
    decode(<<Rest/bits>>, Tree, Tree, [L | Result]);
decode(<<1:1, Rest/bits>>, Tree, {_L, R = {_, _}}, Result) ->
    decode(<<Rest/bits>>, Tree, R, Result);
decode(<<1:1, Rest/bits>>, Tree, {_L, R}, Result) ->
    decode(<<Rest/bits>>, Tree, Tree, [R | Result]).

codewords({L, R}) ->
    codewords(L, <<0:1>>) ++ codewords(R, <<1:1>>).

codewords({L, R}, <<Bits/bits>>) ->
    codewords(L, <<Bits/bits, 0:1>>) ++ codewords(R, <<Bits/bits, 1:1>>);
codewords(Symbol, <<Bits/bitstring>>) ->
    [{Symbol, Bits}].

tree([{N, _} | []]) ->
    N;
tree(Ns) ->
    [{N1, C1}, {N2, C2} | Rest] = lists:keysort(2, Ns),
    tree([{{N1, N2}, C1 + C2} | Rest]).

freq_table(Text) ->
    freq_table(lists:sort(Text), []).

freq_table([], Acc) ->
    Acc;
freq_table([S | Rest], Acc) ->
    {Block, MoreBlocks} = lists:splitwith(fun (X) -> X == S end, Rest),
    freq_table(MoreBlocks, [{S, 1 + length(Block)} | Acc]).

print_bits(<<>>) ->
  io:format("\n");
print_bits(<<Bit:1, Rest/bitstring>>) ->
  io:format("~w", [Bit]),
  print_bits(Rest).
```

```txt
 : 111
a: 1011
c: 10010
d: 100111
e: 1010
f: 1101
g: 100110
h: 1000
i: 1100
l: 00001
m: 0101
n: 001
o: 0100
p: 00000
r: 00011
s: 0111
t: 00010
u: 01101
x: 01100
encoded: 0001010001100011111111000111111101100111110100110010110101000000000110101111101010000011111100001101110111010101101100111110100011001001001001111100001100110
decoded: this is an example for huffman encoding
```


=={{header|F_Sharp|F#}}==
```fsharp
type 'a HuffmanTree =
    | Leaf of int * 'a
    | Node of int * 'a HuffmanTree * 'a HuffmanTree

let freq = function Leaf (f, _) | Node (f, _, _) -> f
let freqCompare a b = compare (freq a) (freq b)

let buildTree charFreqs =
    let leaves = List.map (fun (c,f) -> Leaf (f,c)) charFreqs
    let freqSort = List.sortWith freqCompare
    let rec aux = function
        | [] -> failwith "empty list"
        | [a] -> a
        | a::b::tl ->
            let node = Node(freq a + freq b, a, b)
            aux (freqSort(node::tl))
    aux (freqSort leaves)

let rec printTree = function
  | code, Leaf (f, c) ->
      printfn "%c\t%d\t%s" c f (String.concat "" (List.rev code));
  | code, Node (_, l, r) ->
      printTree ("0"::code, l);
      printTree ("1"::code, r)

let () =
  let str = "this is an example for huffman encoding"
  let charFreqs =
    str |> Seq.groupBy id
        |> Seq.map (fun (c, vals) -> (c, Seq.length vals))
        |> Map.ofSeq

  let tree = charFreqs |> Map.toList |> buildTree
  printfn "Symbol\tWeight\tHuffman code";
  printTree ([], tree)
```

```txt
Symbol	Weight	Huffman code
p	1	00000
r	1	00001
g	1	00010
l	1	00011
n	4	001
m	2	0100
o	2	0101
c	1	01100
d	1	01101
h	2	0111
s	2	1000
x	1	10010
t	1	100110
u	1	100111
f	3	1010
i	3	1011
a	3	1100
e	3	1101
 	6	111
```



## Fantom



```fantom

class Node
{
  Float probability := 0.0f
}

class Leaf : Node
{
  Int character

  new make (Int character, Float probability)
  {
    this.character = character
    this.probability = probability
  }
}

class Branch : Node
{
  Node left
  Node right

  new make (Node left, Node right)
  {
    this.left = left
    this.right = right
    probability = this.left.probability + this.right.probability
  }
}

class Huffman
{
  Node[] queue := [,]
  Str:Str table := [:]

  new make (Int[] items)
  {
    uniqueItems := items.dup.unique
    uniqueItems.each |Int item|
    {
      num := items.findAll { it == item }.size
      queue.add (Leaf(item, num.toFloat / items.size))
    }
    createTree
    createTable
  }

  Void createTree ()
  {
    while (queue.size > 1)
    {
      queue.sort |a,b| {a.probability <=> b.probability}
      node1 := queue.removeAt (0)
      node2 := queue.removeAt (0)
      queue.add (Branch (node1, node2))
    }
  }

  Void traverse (Node node, Str encoding)
  {
    if (node is Leaf)
    {
      table[(node as Leaf).character.toChar] = encoding
    }
    else // (node is Branch)
    {
      traverse ((node as Branch).left, encoding + "0")
      traverse ((node as Branch).right, encoding + "1")
    }
  }

  Void createTable ()
  {
    if (queue.size != 1) return // error!
    traverse (queue.first, "")
  }

  override Str toStr ()
  {
    result := "Huffman Encoding Table:\n"
    table.keys.sort.each |Str key|
    {
      result += "$key -> ${table[key]}\n"
    }
    return result
  }
}

class Main
{
  public static Void main ()
  {
    example := "this is an example for huffman encoding"
    huffman := Huffman (example.chars)
    echo ("From \"$example\"")
    echo (huffman)
  }
}

```


```txt

From "this is an example for huffman encoding"
Huffman Encoding Table:
  -> 101
a -> 1100
c -> 10000
d -> 10001
e -> 1101
f -> 1110
g -> 11110
h -> 11111
i -> 1001
l -> 01101
m -> 0011
n -> 000
o -> 0100
p -> 01100
r -> 01110
s -> 0010
t -> 01010
u -> 01111
x -> 01011

```



## Fortran



```fortran
! output:
! d-> 00000, t-> 00001, h-> 0001, s-> 0010,
! c-> 00110, x-> 00111, m-> 0100, o-> 0101,
! n-> 011, u-> 10000, l-> 10001, a-> 1001,
! r-> 10100, g-> 101010, p-> 101011,
! e-> 1011, i-> 1100, f-> 1101,  -> 111
!
! 00001|0001|1100|0010|111|1100|0010|111|1001|011|
! 111|1011|00111|1001|0100|101011|10001|1011|111|
! 1101|0101|10100|111|0001|10000|1101|1101|0100|
! 1001|011|111|1011|011|00110|0101|00000|1100|011|101010|
!
module huffman
implicit none
type node
  character (len=1 ), allocatable :: sym(:)
  character (len=10), allocatable :: code(:)
  integer                         :: freq
contains
  procedure                       :: show => show_node
end type

type queue
  type(node), allocatable :: buf(:)
  integer                 :: n = 0
contains
  procedure :: extractmin
  procedure :: append
  procedure :: siftdown
end type

contains

subroutine siftdown(this, a)
  class (queue)           :: this
  integer                 :: a, parent, child
  associate (x => this%buf)
  parent = a
  do while(parent*2 <= this%n)
    child = parent*2
    if (child + 1 <= this%n) then
      if (x(child+1)%freq < x(child)%freq ) then
        child = child +1
      end if
    end if
    if (x(parent)%freq > x(child)%freq) then
      x([child, parent]) = x([parent, child])
      parent = child
    else
      exit
    end if
  end do
  end associate
end subroutine

function extractmin(this) result (res)
  class(queue) :: this
  type(node)   :: res
  res = this%buf(1)
  this%buf(1) = this%buf(this%n)
  this%n = this%n - 1
  call this%siftdown(1)
end function

subroutine append(this, x)
  class(queue), intent(inout) :: this
  type(node)                  :: x
  type(node), allocatable     :: tmp(:)
  integer                     :: i
  this%n = this%n +1
  if (.not.allocated(this%buf)) allocate(this%buf(1))
  if (size(this%buf)<this%n) then
    allocate(tmp(2*size(this%buf)))
    tmp(1:this%n-1) = this%buf
    call move_alloc(tmp, this%buf)
  end if
  this%buf(this%n) = x
  i = this%n
  do
    i = i / 2
    if (i==0) exit
    call this%siftdown(i)
  end do
end subroutine

function join(a, b) result(c)
  type(node)             :: a, b, c
  integer                :: i, n, n1
  n1 = size(a%sym)
  n = n1 + size(b%sym)
  c%freq = a%freq + b%freq
  allocate (c%sym(n), c%code(n))
  do i = 1, n1
    c%sym(i) = a%sym(i)
    c%code(i) = "0" // trim(a%code(i))
  end do
  do i = 1, size(b%sym)
    c%sym(i+n1) = b%sym(i)
    c%code(i+n1) = "1" // trim(b%code(i))
  end do
end function

subroutine show_node(this)
  class(node) :: this
  integer     :: i
  write(*, "(*(g0,'-> ',g0,:,', '))", advance="no") &
   (this%sym(i), trim(this%code(i)), i=1,size(this%sym))
  print *
end subroutine

function create(letter, freq) result (this)
  character :: letter
  integer   :: freq
  type(node) :: this
  allocate(this%sym(1), this%code(1))
  this%sym(1) = letter ; this%code(1) = ""
  this%freq = freq
end function
end module

program main
  use huffman
  character (len=*), parameter   :: txt = &
   "this is an example for huffman encoding"
  integer                        :: i, freq(0:255) = 0
  type(queue)                    :: Q
  type(node)                     :: x
  do i = 1, len(txt)
    freq(ichar(txt(i:i))) = freq(ichar(txt(i:i))) + 1
  end do
  do i = 0, 255
    if (freq(i)>0) then
      call Q%append(create(char(i), freq(i)))
    end if
  end do
  do i = 1, Q%n-1
    call Q%append(join(Q%extractmin(),Q%extractmin()))
  end do
  x = Q%extractmin()
  call x%show()
  do i = 1, len(txt)
    do k = 1, size(x%sym)
      if (x%sym(k)==txt(i:i)) exit
     end do
     write (*, "(a,'|')", advance="no")  trim(x%code(k))
  end do
  print *
end program

```



## Go

```go
package main

import (
    "container/heap"
    "fmt"
)

type HuffmanTree interface {
    Freq() int
}

type HuffmanLeaf struct {
    freq  int
    value rune
}

type HuffmanNode struct {
    freq        int
    left, right HuffmanTree
}

func (self HuffmanLeaf) Freq() int {
    return self.freq
}

func (self HuffmanNode) Freq() int {
    return self.freq
}

type treeHeap []HuffmanTree

func (th treeHeap) Len() int { return len(th) }
func (th treeHeap) Less(i, j int) bool {
    return th[i].Freq() < th[j].Freq()
}
func (th *treeHeap) Push(ele interface{}) {
    *th = append(*th, ele.(HuffmanTree))
}
func (th *treeHeap) Pop() (popped interface{}) {
    popped = (*th)[len(*th)-1]
    *th = (*th)[:len(*th)-1]
    return
}
func (th treeHeap) Swap(i, j int) { th[i], th[j] = th[j], th[i] }

func buildTree(symFreqs map[rune]int) HuffmanTree {
    var trees treeHeap
    for c, f := range symFreqs {
        trees = append(trees, HuffmanLeaf{f, c})
    }
    heap.Init(&trees)
    for trees.Len() > 1 {
        // two trees with least frequency
        a := heap.Pop(&trees).(HuffmanTree)
        b := heap.Pop(&trees).(HuffmanTree)

        // put into new node and re-insert into queue
        heap.Push(&trees, HuffmanNode{a.Freq() + b.Freq(), a, b})
    }
    return heap.Pop(&trees).(HuffmanTree)
}

func printCodes(tree HuffmanTree, prefix []byte) {
    switch i := tree.(type) {
    case HuffmanLeaf:
        // print out symbol, frequency, and code for this
        // leaf (which is just the prefix)
        fmt.Printf("%c\t%d\t%s\n", i.value, i.freq, string(prefix))
    case HuffmanNode:
        // traverse left
        prefix = append(prefix, '0')
        printCodes(i.left, prefix)
        prefix = prefix[:len(prefix)-1]

        // traverse right
        prefix = append(prefix, '1')
        printCodes(i.right, prefix)
        prefix = prefix[:len(prefix)-1]
    }
}

func main() {
    test := "this is an example for huffman encoding"

    symFreqs := make(map[rune]int)
    // read each symbol and record the frequencies
    for _, c := range test {
        symFreqs[c]++
    }

    // build tree
    tree := buildTree(symFreqs)

    // print out results
    fmt.Println("SYMBOL\tWEIGHT\tHUFFMAN CODE")
    printCodes(tree, []byte{})
}
```

```txt

SYMBOL	WEIGHT	HUFFMAN CODE
n	4	000
m	2	0010
o	2	0011
s	2	0100
u	1	01010
p	1	01011
h	2	0110
d	1	01110
c	1	01111
t	1	10000
l	1	10001
x	1	10010
r	1	100110
g	1	100111
i	3	1010
e	3	1011
 	6	110
f	3	1110
a	3	1111

```

```go
package main

import (
    "container/heap"
    "fmt"
)

type coded struct {
    sym  rune
    code string
}

type counted struct {
    total int
    syms []coded
}

type cHeap []counted

// satisfy heap.Interface
func (c cHeap) Len() int           { return len(c) }
func (c cHeap) Less(i, j int) bool { return c[i].total < c[j].total }
func (c cHeap) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }
func (c *cHeap) Push(ele interface{}) {
    *c = append(*c, ele.(counted))
}
func (c *cHeap) Pop() (popped interface{}) {
    popped = (*c)[len(*c)-1]
    *c = (*c)[:len(*c)-1]
    return
}

func encode(sym2freq map[rune]int) []coded {
    var ch cHeap
    for sym, freq := range sym2freq {
        ch = append(ch, counted{freq, []coded{{sym: sym}}})
    }
    heap.Init(&ch)
    for len(ch) > 1 {
        a := heap.Pop(&ch).(counted)
        b := heap.Pop(&ch).(counted)
        for i, c := range a.syms {
            a.syms[i].code = "0" + c.code
        }
        for i, c := range b.syms {
            b.syms[i].code = "1" + c.code
        }
        heap.Push(&ch, counted{a.total + b.total, append(a.syms, b.syms...)})
    }
    return heap.Pop(&ch).(counted).syms
}

const txt = "this is an example for huffman encoding"

func main() {
    sym2freq := make(map[rune]int)
    for _, c := range txt {
        sym2freq[c]++
    }
    table := encode(sym2freq)
    fmt.Println("Symbol  Weight Huffman Code")
    for _, c := range table {
        fmt.Printf("     %c    %d    %s\n", c.sym, sym2freq[c.sym], c.code)
    }
}
```



## Groovy


Implemented and tested with Groovy 2.3.


```groovy

import groovy.transform.*

@Canonical
@Sortable(includes = ['freq', 'letter'])
class Node {
    String letter
    int freq
    Node left
    Node right
    boolean isLeaf() { left == null && right == null }
}

Map correspondance(Node n, Map corresp = [:], String prefix = '') {
    if (n.isLeaf()) {
        corresp[n.letter] = prefix ?: '0'
    } else {
        correspondance(n.left,  corresp, prefix + '0')
        correspondance(n.right, corresp, prefix + '1')
    }
    return corresp
}

Map huffmanCode(String message) {
    def queue = message.toList().countBy { it } // char frequencies
        .collect { String letter, int freq ->   // transformed into tree nodes
            new Node(letter, freq)
        } as TreeSet // put in a queue that maintains ordering

    while(queue.size() > 1) {
        def (nodeLeft, nodeRight)  = [queue.pollFirst(), queue.pollFirst()]

        queue << new Node(
            freq:   nodeLeft.freq   + nodeRight.freq,
            letter: nodeLeft.letter + nodeRight.letter,
            left: nodeLeft, right: nodeRight
        )
    }

    return correspondance(queue.pollFirst())
}

String encode(CharSequence msg, Map codeTable) {
    msg.collect { codeTable[it] }.join()
}

String decode(String codedMsg, Map codeTable, String decoded = '') {
    def pair = codeTable.find { k, v -> codedMsg.startsWith(v) }
    pair ? pair.key + decode(codedMsg.substring(pair.value.size()), codeTable)
         : decoded
}

```

Usage:

```groovy

def message = "this is an example for huffman encoding"

def codeTable = huffmanCode(message)
codeTable.each { k, v -> println "$k: $v" }

def encoded = encode(message, codeTable)
println encoded

def decoded = decode(encoded, codeTable)
println decoded

```

```txt

g: 00000
l: 00001
h: 0001
m: 0010
o: 0011
n: 010
p: 01100
r: 01101
s: 0111
t: 10000
u: 10001
a: 1001
 : 101
e: 1100
f: 1101
i: 1110
x: 11110
c: 111110
d: 111111
1000000011110011110111100111101100101010111001111010010010011000000111001011101001101101101000110001110111010010100101010111000101111100011111111111001000000
this is an example for huffman encoding


```



## Haskell

Credits go to [http://www.haskell.org/haskellwiki/99_questions/46_to_50#Problem_50 huffman] where you'll also find a non-tree solution. Uses sorted list as a priority queue.

```haskell
import Data.List (group, insertBy, sort, sortBy)
import Control.Arrow ((&&&), second)
import Data.Ord (comparing)

data HTree a
  = Leaf a
  | Branch (HTree a)
           (HTree a)
  deriving (Show, Eq, Ord)

test :: String -> IO ()
test =
  mapM_ (\(a, b) -> putStrLn ('\'' : a : ("' : " ++ b))) .
  serialize . huffmanTree . freq

serialize :: HTree a -> [(a, String)]
serialize (Branch l r) =
  (second ('0' :) <$> serialize l) ++ (second ('1' :) <$> serialize r)
serialize (Leaf x) = [(x, "")]

huffmanTree
  :: (Ord w, Num w)
  => [(w, a)] -> HTree a
huffmanTree =
  snd .
  head . until (null . tail) hstep . sortBy (comparing fst) . fmap (second Leaf)

hstep
  :: (Ord a, Num a)
  => [(a, HTree b)] -> [(a, HTree b)]
hstep ((w1, t1):(w2, t2):wts) =
  insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts

freq
  :: Ord a
  => [a] -> [(Int, a)]
freq = fmap (length &&& head) . group . sort

main :: IO ()
main = test "this is an example for huffman encoding"
```

```haskell
'p' : 00000
'r' : 00001
'g' : 00010
'l' : 00011
'n' : 001
'm' : 0100
'o' : 0101
'c' : 01100
'd' : 01101
'h' : 0111
's' : 1000
'x' : 10010
't' : 100110
'u' : 100111
'f' : 1010
'i' : 1011
'a' : 1100
'e' : 1101
' ' : 111
```



### Using <code>Set</code> as a priority queue

(might be worth it for bigger alphabets):

```haskell
import qualified Data.Set as S

htree :: (Ord t, Num t, Ord a) => S.Set (t, HTree a) -> HTree a
htree ts | S.null ts_1 = t1
         | otherwise = htree ts_3
           where
             ((w1,t1), ts_1) = S.deleteFindMin ts
             ((w2,t2), ts_2) = S.deleteFindMin ts_1
             ts_3 = S.insert (w1 + w2, Branch t1 t2) ts_2

huffmanTree :: (Ord w, Num w, Ord a) => [(w, a)] -> HTree a
huffmanTree =  htree . S.fromList . map (second Leaf)
```


===A non-tree version===
This produces the output required without building the Huffman tree at all,
by building all the trace strings directly while reducing the histogram:

```haskell
import Data.List (sortBy, insertBy, sort, group)
import Control.Arrow (second, (&&&))
import Data.Ord (comparing)

freq :: Ord a => [a] -> [(Int, a)]
freq = map (length &&& head) . group . sort

huffman :: [(Int, Char)] -> [(Char, String)]
huffman = reduce . map (\(p, c) -> (p, [(c ,"")])) . sortBy (comparing fst)
  where add (p1, xs1) (p2, xs2) = (p1 + p2, map (second ('0':)) xs1 ++ map (second ('1':)) xs2)
        reduce [(_, ys)]  = sortBy (comparing fst) ys
        reduce (x1:x2:xs) = reduce $ insertBy (comparing fst) (add x1 x2) xs

test s = mapM_ (\(a, b) -> putStrLn ('\'' : a : "\' : " ++ b)) . huffman . freq $ s

main = do
    test "this is an example for huffman encoding"
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
record huffnode(l,r,n,c)                        # internal and leaf nodes
record huffcode(c,n,b,i)                        # encoding table char, freq, bitstring, bits (int)

procedure main()

s := "this is an example for huffman encoding"

Count := huffcount(s)                           # frequency count
Tree := huffTree(Count)                         # heap and tree

Code := []                                      # extract encodings
CodeT := table()
every x := huffBits(Tree) do
   put( Code, CodeT[c] := huffcode( c := x[-1], Count[c].n, b := x[1:-1], integer("2r"||b) ) )


Code := sortf( Code, 1 )                        # show table in char order
write("Input String : ",image(s))
write(right("char",5), right("freq",5), " encoding" )
every write(right(image((x := !Code).c),5), right(x.n,5), " ", x.b )

end

procedure huffBits(N)                           # generates huffman bitcodes with trailing character
if \N.c then return N.c                         # . append leaf char code
suspend "0" || huffBits(N.l)                    # . left
suspend "1" || huffBits(N.r)                    # . right
end


procedure huffTree(T)                           # two queue huffman tree method
local Q1,Q2,x,n1,n2

Q1 := []                                        # queue of characters and weights
every x := !T do                                # ensure all are huffnodes
   if type(x) == "huffnode" then put(Q1,x) else runerr(205,x)
Q1 := sortf(Q1,3)                               # sort by weight ( 3 means by .n  )

if *Q1 > 1 then Q2 := []
while *Q1+*\Q2 > 1 do {                         # While there is more than one node ...

   n1 := if Q1[1] & ( ( Q1[1].n <= Q2[1].n ) | not Q2[1] ) then get(Q1) else get(Q2)  # lowest weight from Q1 or Q2
   n2 := if Q1[1] & ( ( Q1[1].n <= Q2[1].n ) | not Q2[1] ) then get(Q1) else get(Q2)  # lowest weight from Q1 or Q2

   put( Q2, huffnode( n1, n2, n1.n + n2.n ) )   # new weighted node to end of Q2
}

return (\Q2 | Q1)[1]                            # return the root node
end

procedure huffcount(s)                          # return characters and frequencies in a table of huffnodes by char
local c,T

T := table()
every c := !s do {
   /T[c] := huffnode(,,0,c)
   T[c].n +:= 1
   }
return T
end
```


```txt
Input String : "this is an example for huffman encoding"
 char freq encoding
  " "    6 101
  "a"    3 1100
  "c"    1 10000
  "d"    1 10001
  "e"    3 1101
  "f"    3 1110
  "g"    1 11110
  "h"    2 11111
  "i"    3 1001
  "l"    1 01101
  "m"    2 0011
  "n"    4 000
  "o"    2 0100
  "p"    1 01100
  "r"    1 01110
  "s"    2 0010
  "t"    1 01010
  "u"    1 01111
  "x"    1 01011
```


The following Unicon specific solution takes advantage of the <tt>Heap</tt>
priority queue implementation found in the UniLib <tt>Collections</tt> package
and implements the algorithm given in the problem description.
The program produces Huffman codes based on each line of input.

```Unicon
import Collections

procedure main(A)
    every line := !&input do {
        every (t := table(0))[!line] +:= 1           # Frequency table
        heap := Heap(sort(t), field, "<")            # Initial priority queue
        while heap.size() > 1 do {                   # Tree construction
            every (p1|p2) := heap.get()
            heap.add([&null, p1[2]+p2[2], p1, p2])
            }
        codes := treeWalk(heap.get(),"")             # Get codes from tree
        write("Huffman encoding:")                   # Display codes
        every pair := !sort(codes) do
            write("\t'",\pair[1],"'-> ",pair[2])
        }
end

procedure field(node)  # selector function for Heap
    return node[2]  # field to use for priority ordering
end

procedure treeWalk(node, prefix, codeMap)
    /codeMap := table("")
    if /node[1] then {  # interior node
        treeWalk(node[3], prefix||"0", codeMap)
        treeWalk(node[4], prefix||"1", codeMap)
        }
    else codeMap[node[1]] := prefix
    return codeMap
end
```


A sample run:

```txt

->huffman
this is an example for huffman encoding
Huffman encoding:
        ' '-> 111
        'a'-> 1001
        'c'-> 00110
        'd'-> 00000
        'e'-> 1011
        'f'-> 1101
        'g'-> 101010
        'h'-> 0001
        'i'-> 1100
        'l'-> 10001
        'm'-> 0100
        'n'-> 011
        'o'-> 0101
        'p'-> 101011
        'r'-> 10100
        's'-> 0010
        't'-> 00001
        'u'-> 10000
        'x'-> 00111
aardvarks are ant eaters
Huffman encoding:
        ' '-> 011
        'a'-> 10
        'd'-> 0010
        'e'-> 010
        'k'-> 0011
        'n'-> 0001
        'r'-> 110
        's'-> 1111
        't'-> 1110
        'v'-> 0000
->
```

[http://www.cs.arizona.edu/icon/library/progs/huffstuf.htm HuffStuff provides huffman encoding routines]


## J


'''Solution''' (drawn from [[J:Essays/Huffman_Coding|the J wiki]]):


```j
hc=: 4 : 0
 if. 1=#x do. y
 else. ((i{x),+/j{x) hc (i{y),<j{y [ i=. (i.#x) -. j=. 2{./:x end.
)

hcodes=: 4 : 0
 assert. x -:&$ y           NB. weights and words have same shape
 assert. (0<:x) *. 1=#$x    NB. weights are non-negative
 assert. 1 >: L.y           NB. words are boxed not more than once
 w=. ,&.> y                 NB. standardized words
 assert. w -: ~.w           NB. words are unique
 t=. 0 {:: x hc w           NB. minimal weight binary tree
 ((< S: 0 t) i. w) { <@(1&=)@; S: 1 {:: t
)
```


'''Example''':
```j
   ;"1":L:0(#/.~ (],.(<'    '),.hcodes) ,&.>@~.)'this is an example for huffman encoding'
 t    0 1 0 1 0
 h    1 1 1 1 1
 i    1 0 0 1
 s    0 0 1 0
      1 0 1
 a    1 1 0 0
 n    0 0 0
 e    1 1 0 1
 x    0 1 0 1 1
 m    0 0 1 1
 p    0 1 1 0 0
 l    0 1 1 0 1
 f    1 1 1 0
 o    0 1 0 0
 r    0 1 1 1 0
 u    0 1 1 1 1
 c    1 0 0 0 0
 d    1 0 0 0 1
 g    1 1 1 1 0
```



## Java

This implementation creates an actual tree structure, and then traverses the tree to recover the code.

```java
import java.util.*;

abstract class HuffmanTree implements Comparable<HuffmanTree> {
    public final int frequency; // the frequency of this tree
    public HuffmanTree(int freq) { frequency = freq; }

    // compares on the frequency
    public int compareTo(HuffmanTree tree) {
        return frequency - tree.frequency;
    }
}

class HuffmanLeaf extends HuffmanTree {
    public final char value; // the character this leaf represents

    public HuffmanLeaf(int freq, char val) {
        super(freq);
        value = val;
    }
}

class HuffmanNode extends HuffmanTree {
    public final HuffmanTree left, right; // subtrees

    public HuffmanNode(HuffmanTree l, HuffmanTree r) {
        super(l.frequency + r.frequency);
        left = l;
        right = r;
    }
}

public class HuffmanCode {
    // input is an array of frequencies, indexed by character code
    public static HuffmanTree buildTree(int[] charFreqs) {
        PriorityQueue<HuffmanTree> trees = new PriorityQueue<HuffmanTree>();
        // initially, we have a forest of leaves
        // one for each non-empty character
        for (int i = 0; i < charFreqs.length; i++)
            if (charFreqs[i] > 0)
                trees.offer(new HuffmanLeaf(charFreqs[i], (char)i));

        assert trees.size() > 0;
        // loop until there is only one tree left
        while (trees.size() > 1) {
            // two trees with least frequency
            HuffmanTree a = trees.poll();
            HuffmanTree b = trees.poll();

            // put into new node and re-insert into queue
            trees.offer(new HuffmanNode(a, b));
        }
        return trees.poll();
    }

    public static void printCodes(HuffmanTree tree, StringBuffer prefix) {
        assert tree != null;
        if (tree instanceof HuffmanLeaf) {
            HuffmanLeaf leaf = (HuffmanLeaf)tree;

            // print out character, frequency, and code for this leaf (which is just the prefix)
            System.out.println(leaf.value + "\t" + leaf.frequency + "\t" + prefix);

        } else if (tree instanceof HuffmanNode) {
            HuffmanNode node = (HuffmanNode)tree;

            // traverse left
            prefix.append('0');
            printCodes(node.left, prefix);
            prefix.deleteCharAt(prefix.length()-1);

            // traverse right
            prefix.append('1');
            printCodes(node.right, prefix);
            prefix.deleteCharAt(prefix.length()-1);
        }
    }

    public static void main(String[] args) {
        String test = "this is an example for huffman encoding";

        // we will assume that all our characters will have
        // code less than 256, for simplicity
        int[] charFreqs = new int[256];
        // read each character and record the frequencies
        for (char c : test.toCharArray())
            charFreqs[c]++;

        // build tree
        HuffmanTree tree = buildTree(charFreqs);

        // print out results
        System.out.println("SYMBOL\tWEIGHT\tHUFFMAN CODE");
        printCodes(tree, new StringBuffer());
    }
}
```


```txt
SYMBOL	WEIGHT	HUFFMAN CODE
d	1	00000
t	1	00001
h	2	0001
s	2	0010
c	1	00110
x	1	00111
m	2	0100
o	2	0101
n	4	011
u	1	10000
l	1	10001
a	3	1001
r	1	10100
g	1	101010
p	1	101011
e	3	1011
i	3	1100
f	3	1101
 	6	111
```






## JavaScript

{{works with|SpiderMonkey}} for the <code>print()</code> function.

First, use the Binary Heap implementation from here: http://eloquentjavascript.net/appendix2.html

The Huffman encoder

```javascript
function HuffmanEncoding(str) {
    this.str = str;

    var count_chars = {};
    for (var i = 0; i < str.length; i++)
        if (str[i] in count_chars)
            count_chars[str[i]] ++;
        else
            count_chars[str[i]] = 1;

    var pq = new BinaryHeap(function(x){return x[0];});
    for (var ch in count_chars)
        pq.push([count_chars[ch], ch]);

    while (pq.size() > 1) {
        var pair1 = pq.pop();
        var pair2 = pq.pop();
        pq.push([pair1[0]+pair2[0], [pair1[1], pair2[1]]]);
    }

    var tree = pq.pop();
    this.encoding = {};
    this._generate_encoding(tree[1], "");

    this.encoded_string = ""
    for (var i = 0; i < this.str.length; i++) {
        this.encoded_string += this.encoding[str[i]];
    }
}

HuffmanEncoding.prototype._generate_encoding = function(ary, prefix) {
    if (ary instanceof Array) {
        this._generate_encoding(ary[0], prefix + "0");
        this._generate_encoding(ary[1], prefix + "1");
    }
    else {
        this.encoding[ary] = prefix;
    }
}

HuffmanEncoding.prototype.inspect_encoding = function() {
    for (var ch in this.encoding) {
        print("'" + ch + "': " + this.encoding[ch])
    }
}

HuffmanEncoding.prototype.decode = function(encoded) {
    var rev_enc = {};
    for (var ch in this.encoding)
        rev_enc[this.encoding[ch]] = ch;

    var decoded = "";
    var pos = 0;
    while (pos < encoded.length) {
        var key = ""
        while (!(key in rev_enc)) {
            key += encoded[pos];
            pos++;
        }
        decoded += rev_enc[key];
    }
    return decoded;
}
```


And, using the Huffman encoder

```javascript
var s = "this is an example for huffman encoding";
print(s);

var huff = new HuffmanEncoding(s);
huff.inspect_encoding();

var e = huff.encoded_string;
print(e);

var t = huff.decode(e);
print(t);

print("is decoded string same as original? " + (s==t));
```


<pre style='width: full; overflow: scroll'>this is an example for huffman encoding
'n': 000
's': 0010
'm': 0011
'o': 0100
't': 01010
'x': 01011
'p': 01100
'l': 01101
'r': 01110
'u': 01111
'c': 10000
'd': 10001
'i': 1001
' ': 101
'a': 1100
'e': 1101
'f': 1110
'g': 11110
'h': 11111
0101011111100100101011001001010111000001011101010111100001101100011011101101111001000111010111111011111110111000111100000101110100010000010010001100100011110
this is an example for huffman encoding
is decoded string same as original? true
```



## Julia


```julia

abstract type HuffmanTree end

struct HuffmanLeaf <: HuffmanTree
    ch::Char
    freq::Int
end

struct HuffmanNode <: HuffmanTree
    freq::Int
    left::HuffmanTree
    right::HuffmanTree
end

function makefreqdict(s::String)
    d = Dict{Char, Int}()
    for c in s
        if !haskey(d, c)
            d[c] = 1
        else
            d[c] += 1
        end
    end
    d
end

function huffmantree(ftable::Dict)
    trees::Vector{HuffmanTree} = [HuffmanLeaf(ch, fq) for (ch, fq) in ftable]
    while length(trees) > 1
        sort!(trees, lt = (x, y) -> x.freq < y.freq, rev = true)
        least = pop!(trees)
        nextleast = pop!(trees)
        push!(trees, HuffmanNode(least.freq + nextleast.freq, least, nextleast))
    end
    trees[1]
end

printencoding(lf::HuffmanLeaf, code) = println(lf.ch == ' ' ? "space" : lf.ch, "\t", lf.freq, "\t", code)

function printencoding(nd::HuffmanNode, code)
    code *= '0'
    printencoding(nd.left, code)
    code = code[1:end-1]

    code *= '1'
    printencoding(nd.right, code)
    code = code[1:end-1]
end

const msg = "this is an example for huffman encoding"

println("Char\tFreq\tHuffman code")

printencoding(huffmantree(makefreqdict(msg)), "")

```
```txt

 Char    Freq    Huffman code
 p       1       00000
 c       1       00001
 g       1       00010
 x       1       00011
 n       4       001
 s       2       0100
 h       2       0101
 u       1       01100
 l       1       01101
 m       2       0111
 o       2       1000
 d       1       10010
 r       1       100110
 t       1       100111
 e       3       1010
 f       3       1011
 a       3       1100
 i       3       1101
 space   6       111

```



## Kotlin

This implementation creates an actual tree structure, and then traverses the tree to recover the code.

```kotlin
import java.util.*

abstract class HuffmanTree(var freq: Int) : Comparable<HuffmanTree> {
    override fun compareTo(other: HuffmanTree) = freq - other.freq
}

class HuffmanLeaf(freq: Int, var value: Char) : HuffmanTree(freq)

class HuffmanNode(var left: HuffmanTree, var right: HuffmanTree) : HuffmanTree(left.freq + right.freq)

fun buildTree(charFreqs: IntArray) : HuffmanTree {
    val trees = PriorityQueue<HuffmanTree>()

    charFreqs.forEachIndexed { index, freq ->
        if(freq > 0) trees.offer(HuffmanLeaf(freq, index.toChar()))
    }

    assert(trees.size > 0)
    while (trees.size > 1) {
        val a = trees.poll()
        val b = trees.poll()
        trees.offer(HuffmanNode(a, b))
    }

    return trees.poll()
}

fun printCodes(tree: HuffmanTree, prefix: StringBuffer) {
    when(tree) {
        is HuffmanLeaf -> println("${tree.value}\t${tree.freq}\t$prefix")
        is HuffmanNode -> {
            //traverse left
            prefix.append('0')
            printCodes(tree.left, prefix)
            prefix.deleteCharAt(prefix.lastIndex)
            //traverse right
            prefix.append('1')
            printCodes(tree.right, prefix)
            prefix.deleteCharAt(prefix.lastIndex)
        }
    }
}

fun main(args: Array<String>) {
    val test = "this is an example for huffman encoding"

    val maxIndex = test.max()!!.toInt() + 1
    val freqs = IntArray(maxIndex) //256 enough for latin ASCII table, but dynamic size is more fun
    test.forEach { freqs[it.toInt()] += 1 }

    val tree = buildTree(freqs)
    println("SYMBOL\tWEIGHT\tHUFFMAN CODE")
    printCodes(tree, StringBuffer())
}
```


```txt
SYMBOL	WEIGHT	HUFFMAN CODE
d	1	00000
t	1	00001
h	2	0001
s	2	0010
c	1	00110
x	1	00111
m	2	0100
o	2	0101
n	4	011
u	1	10000
l	1	10001
a	3	1001
r	1	10100
g	1	101010
p	1	101011
e	3	1011
i	3	1100
f	3	1101
6	111
```



## Lua

This implementation proceeds in three steps: determine word frequencies,
construct the Huffman tree, and finally fold the tree into the codes
while outputting them.

```lua
local build_freqtable = function (data)
  local freq = { }

  for i = 1, #data do
    local cur = string.sub (data, i, i)
    local count = freq [cur] or 0
    freq [cur] = count + 1
  end

  local nodes = { }
  for w, f in next, freq do
    nodes [#nodes + 1] = { word = w, freq = f }
  end

  table.sort (nodes, function (a, b) return a.freq > b.freq end) --- reverse order!

  return nodes
end

local build_hufftree = function (nodes)
  while true do
    local n = #nodes
    local left = nodes [n]
    nodes [n] = nil

    local right = nodes [n - 1]
    nodes [n - 1] = nil

    local new = { freq = left.freq + right.freq, left = left, right = right }

    if n == 2 then return new end

    --- insert new node at correct priority
    local prio = 1
    while prio < #nodes and nodes [prio].freq > new.freq do
      prio = prio + 1
    end
    table.insert (nodes, prio, new)
  end
end

local print_huffcodes do
  local rec_build_huffcodes
  rec_build_huffcodes = function (node, bits, acc)
    if node.word == nil then
      rec_build_huffcodes (node.left,  bits .. "0", acc)
      rec_build_huffcodes (node.right, bits .. "1", acc)
      return acc
    else --- leaf
      acc [#acc + 1] = { node.freq, node.word, bits }
    end
    return acc
  end

  print_huffcodes = function (root)
    local codes = rec_build_huffcodes (root, "", { })
    table.sort (codes, function (a, b) return a [1] < b [1] end)
    print ("frequency\tword\thuffman code")
    for i = 1, #codes do
      print (string.format ("%9d\t‘%s’\t“%s”", table.unpack (codes [i])))
    end
  end
end


local huffcode = function (data)
  local nodes = build_freqtable (data)
  local huff = build_hufftree (nodes)
  print_huffcodes (huff)
  return 0
end

return huffcode "this is an example for huffman encoding"


```


```txt

frequency	word	huffman code
        1	‘g’	“01111”
        1	‘p’	“01011”
        1	‘d’	“01100”
        1	‘c’	“01101”
        1	‘t’	“01010”
        1	‘r’	“10000”
        1	‘u’	“11110”
        1	‘x’	“10001”
        1	‘l’	“01110”
        2	‘o’	“11111”
        2	‘m’	“0011”
        2	‘h’	“0010”
        2	‘s’	“0100”
        3	‘i’	“1101”
        3	‘f’	“1110”
        3	‘a’	“1100”
        3	‘e’	“1001”
        4	‘n’	“000”
        6	‘ ’	“101”

```



## M2000 Interpreter


```M2000 Interpreter

Module Huffman {
      comp=lambda (a, b) ->{
            =array(a, 0)<array(b, 0)
      }
      module InsertPQ (a, n, &comp) {
            if len(a)=0 then stack a {data n} : exit
            if comp(n, stackitem(a)) then stack a {push n} : exit
             stack a {
                  push n
                  t=2: b=len(a)
                   m=b
                   While t<=b {
                         t1=m
                        m=(b+t) div 2
                        if m=0 then  m=t1 : exit
                        If comp(stackitem(m),n) then t=m+1:  continue
                        b=m-1
                        m=b
                  }
                  if m>1 then shiftback m
            }
      }

      a$="this is an example for huffman encoding"

      inventory queue freq
      For i=1 to len(a$)   {
            b$=mid$(a$,i,1)
            if exist(freq, b$) then Return freq, b$:=freq(b$)+1 : continue
            append freq, b$:=1
      }
      sort ascending freq
      b=stack
      K=each(freq)
      LenA=len(a$)
      While k {
            InsertPQ b, (Round(Eval(k)/lenA, 4), eval$(k, k^)), &comp
      }
      While len(b)>1 {
            Stack b {
                 Read m1, m2
                 InsertPQ b, (Array(m1)+Array(m2), (m1, m2) ), &comp
            }
      }
      Print  "Size of stack object (has only Root):"; len(b)
      Print "Root probability:";Round(Array(Stackitem(b)), 3)
      inventory encode, decode

      Traverse(stackitem(b), "")
      message$=""
      For i=1 to len(a$)
      message$+=encode$(mid$(a$, i, 1))
      Next i

      Print  message$
      j=1
      check$=""
      For i=1 to len(a$)
            d=each(encode)
            While d {
                  code$=eval$(d)
                  if mid$(message$, j, len(code$))=code$ then {
                        check$+=decode$(code$)
                        Print decode$(code$); : j+=len(code$)
                  }
            }
      Next i
      Print
      Print len(message$);" bits ", if$(a$=check$->"Encoding/decoding worked", "Encoding/Decoding failed")


      Sub Traverse(a, a$)
            local b=array(a,1)
            if type$(b)="mArray"  Else {
                  Print  @(10); quote$(array$(a, 1));" "; a$,@(20),array(a)
                  Append decode, a$ :=array$(a, 1)
                  Append encode, array$(a, 1):=a$
                  Exit Sub
            }
            traverse(array(b), a$+"0")
            traverse(array(b,1), a$+"1")
      End Sub
}
Huffman

```


<pre >
"p" 00000      0,0256
"l" 00001      0,0256
"t" 00010      0,0256
"r" 00011      0,0256
"x" 00100      0,0256
"u" 00101      0,0256
"s" 0011       0,0513
"o" 0100       0,0513
"m" 0101       0,0513
"n" 011        0,1026
"h" 1000       0,0513
"c" 10010      0,0256
"g" 100110     0,0256
"d" 100111     0,0256
"e" 1010       0,0769
"a" 1011       0,0769
"i" 1100       0,0769
"f" 1101       0,0769
" " 111        0,1538
0001010001100001111111000011111101101111110100010010110101000000000110101111101010000011111100000101110111010101101101111110100111001001001001111100011100110
this is an example for huffman encoding

157 bits    Encoding/decoding worked

</pre >

=={{header|Mathematica}} / {{header|Wolfram Language}}==


```mathematica
huffman[s_String] := huffman[Characters[s]];
huffman[l_List] := Module[{merge, structure, rules},

   (*merge front two branches. list is assumed to be sorted*)
   merge[k_] := Replace[k, {{a_, aC_}, {b_, bC_}, rest___} :> {{{a, b}, aC + bC}, rest}];

   structure = FixedPoint[
      Composition[merge, SortBy[#, Last] &],
      Tally[l]][[1, 1]];

   rules = (# -> Flatten[Position[structure, #] - 1]) & /@ DeleteDuplicates[l];

   {Flatten[l /. rules], rules}];
```



## Nim



```nim
import tables, seqUtils

const sampleString = "this is an example for huffman encoding"

type
    # Following range can be changed to produce Huffman codes on arbitrary alphabet (e.g. ternary codes)
    CodeSymbol = range[0..1]
    HuffCode = seq[CodeSymbol]
    Node = ref object
        f: int
        parent: Node
        case isLeaf: bool
        of true:
            c: char
        else:
            childs: array[CodeSymbol, Node]

proc `<`(a: Node, b: Node): bool =
    # For min operator
    a.f < b.f

proc `$`(hc: HuffCode): string =
    result = ""
    for symbol in hc:
        result &= $symbol

proc freeChildList(tree: seq[Node], parent: Node = nil): seq[Node] =
    # Constructs a sequence of nodes which can be adopted
    # Optional parent parameter can be set to ensure node will not adopt itself
    result = @[]
    for node in tree:
        if node.parent == nil and node != parent:
            result.add(node)

proc connect(parent: Node, child: Node) =
    # Only call this proc when sure that parent has a free child slot
    child.parent = parent
    parent.f += child.f
    for i in parent.childs.low..parent.childs.high:
        if parent.childs[i] == nil:
            parent.childs[i] = child
            return

proc generateCodes(codes: TableRef[char, HuffCode], currentNode: Node, currentCode: HuffCode = @[]) =
    if currentNode.isLeaf:
        let key = currentNode.c
        codes[key] = currentCode
        return
    for i in currentNode.childs.low..currentNode.childs.high:
        if currentNode.childs[i] != nil:
            let newCode = currentCode & i
            generateCodes(codes, currentNode.childs[i], newCode)

proc buildTree(frequencies: CountTable[char]): seq[Node] =
    result = newSeq[Node](frequencies.len)
    for i in result.low..result.high:
        let key = toSeq(frequencies.keys)[i]
        result[i] = Node(f: frequencies[key], isLeaf: true, c: key)
    while result.freeChildList.len > 1:
        let currentNode = new Node
        result.add(currentNode)
        for c in currentNode.childs:
            currentNode.connect(min(result.freeChildList(currentNode)))
            if result.freeChildList.len <= 1:
                break

var sampleFrequencies = initCountTable[char]()
for c in sampleString:
    sampleFrequencies.inc(c)
let
    tree = buildTree(sampleFrequencies)
    root = tree.freeChildList[0]
var huffCodes = newTable[char, HuffCode]()
generateCodes(huffCodes, root)
echo huffCodes
```


```txt
{ : 101, a: 1001, c: 01010, d: 01011, e: 1100, f: 1101, g: 01100, h: 11111, i: 1110, l: 01101, m: 0010, n: 000, o: 0011, p: 01110, r: 01111, s: 0100, t: 10000, u: 10001, x: 11110}
```


=={{header|Oberon-2}}==
```oberon2

MODULE HuffmanEncoding;
IMPORT
  Object,
  PriorityQueue,
  Strings,
  Out;
TYPE
  Leaf = POINTER TO LeafDesc;
  LeafDesc = RECORD
    (Object.ObjectDesc)
    c: CHAR;
  END;

  Inner = POINTER TO InnerDesc;
  InnerDesc = RECORD
    (Object.ObjectDesc)
    left,right: Object.Object;
  END;

VAR
  str: ARRAY 128 OF CHAR;
  i: INTEGER;
  f: ARRAY 96 OF INTEGER;
  q: PriorityQueue.Queue;
  a: PriorityQueue.Node;
  b: PriorityQueue.Node;
  c: PriorityQueue.Node;
  h: ARRAY 64 OF CHAR;

PROCEDURE NewLeaf(c: CHAR): Leaf;
VAR
  x: Leaf;
BEGIN
  NEW(x);x.c := c; RETURN x
END NewLeaf;

PROCEDURE NewInner(l,r: Object.Object): Inner;
VAR
  x: Inner;
BEGIN
  NEW(x); x.left := l; x.right := r; RETURN x
END NewInner;


PROCEDURE Preorder(n: Object.Object; VAR x: ARRAY OF CHAR);
BEGIN
  IF n IS Leaf THEN
    Out.Char(n(Leaf).c);Out.String(": ");Out.String(h);Out.Ln
  ELSE
    IF n(Inner).left # NIL THEN
      Strings.Append("0",x);
      Preorder(n(Inner).left,x);
      Strings.Delete(x,(Strings.Length(x) - 1),1)
    END;
    IF n(Inner).right # NIL THEN
      Strings.Append("1",x);
      Preorder(n(Inner).right,x);
      Strings.Delete(x,(Strings.Length(x) - 1),1)
    END
  END
END Preorder;

BEGIN
  str := "this is an example for huffman encoding";

  (* Collect letter frecuencies *)
  i := 0;
  WHILE str[i] # 0X DO INC(f[ORD(CAP(str[i])) - ORD(' ')]);INC(i) END;

  (* Create Priority Queue *)
  NEW(q);q.Clear();

  (* Insert into the queue *)
  i := 0;
  WHILE (i < LEN(f)) DO
    IF f[i] # 0 THEN
      q.Insert(f[i]/Strings.Length(str),NewLeaf(CHR(i + ORD(' '))))
    END;
    INC(i)
  END;

  (* create tree *)
  WHILE q.Length() > 1 DO
    q.Remove(a);q.Remove(b);
    q.Insert(a.w + b.w,NewInner(a.d,b.d));
  END;

  (* tree traversal *)
  h[0] := 0X;q.Remove(c);Preorder(c.d,h);

END HuffmanEncoding.

```

```txt

D: 00000
T: 00001
H: 0001
S: 0010
C: 00110
X: 00111
M: 0100
O: 0101
N: 011
U: 10000
L: 10001
A: 1001
R: 10100
G: 101010
P: 101011
E: 1011
I: 1100
F: 1101
 : 111

```


=={{header|Objective-C}}==
This is not purely Objective-C. It uses Apple's Core Foundation library for its binary heap, which admittedly is very ugly. Thus, this only builds on Mac OS X, not GNUstep.

```objc
#import <Foundation/Foundation.h>



@interface HuffmanTree : NSObject {
	int freq;
}
-(instancetype)initWithFreq:(int)f;
@property (nonatomic, readonly) int freq;
@end

@implementation HuffmanTree
@synthesize freq; // the frequency of this tree
-(instancetype)initWithFreq:(int)f {
	if (self = [super init]) {
		freq = f;
	}
	return self;
}
@end


const void *HuffmanRetain(CFAllocatorRef allocator, const void *ptr) {
	return (__bridge_retained const void *)(__bridge id)ptr;
}
void HuffmanRelease(CFAllocatorRef allocator, const void *ptr) {
	(void)(__bridge_transfer id)ptr;
}
CFComparisonResult HuffmanCompare(const void *ptr1, const void *ptr2, void *unused) {
	int f1 = ((__bridge HuffmanTree *)ptr1).freq;
	int f2 = ((__bridge HuffmanTree *)ptr2).freq;
	if (f1 == f2)
		return kCFCompareEqualTo;
	else if (f1 > f2)
		return kCFCompareGreaterThan;
	else
		return kCFCompareLessThan;
}


@interface HuffmanLeaf : HuffmanTree {
	char value; // the character this leaf represents
}
@property (readonly) char value;
-(instancetype)initWithFreq:(int)f character:(char)c;
@end

@implementation HuffmanLeaf
@synthesize value;
-(instancetype)initWithFreq:(int)f character:(char)c {
	if (self = [super initWithFreq:f]) {
		value = c;
	}
	return self;
}
@end


@interface HuffmanNode : HuffmanTree {
	HuffmanTree *left, *right; // subtrees
}
@property (readonly) HuffmanTree *left, *right;
-(instancetype)initWithLeft:(HuffmanTree *)l right:(HuffmanTree *)r;
@end

@implementation HuffmanNode
@synthesize left, right;
-(instancetype)initWithLeft:(HuffmanTree *)l right:(HuffmanTree *)r {
	if (self = [super initWithFreq:l.freq+r.freq]) {
		left = l;
		right = r;
	}
	return self;
}
@end


HuffmanTree *buildTree(NSCountedSet *chars) {

	CFBinaryHeapCallBacks callBacks = {0, HuffmanRetain, HuffmanRelease, NULL, HuffmanCompare};
	CFBinaryHeapRef trees = CFBinaryHeapCreate(NULL, 0, &callBacks, NULL);

	// initially, we have a forest of leaves
	// one for each non-empty character
	for (NSNumber *ch in chars) {
		int freq = [chars countForObject:ch];
		if (freq > 0)
			CFBinaryHeapAddValue(trees, (__bridge const void *)[[HuffmanLeaf alloc] initWithFreq:freq character:(char)[ch intValue]]);
	}

	NSCAssert(CFBinaryHeapGetCount(trees) > 0, @"String must have at least one character");
	// loop until there is only one tree left
	while (CFBinaryHeapGetCount(trees) > 1) {
		// two trees with least frequency
		HuffmanTree *a = (__bridge HuffmanTree *)CFBinaryHeapGetMinimum(trees);
		CFBinaryHeapRemoveMinimumValue(trees);
		HuffmanTree *b = (__bridge HuffmanTree *)CFBinaryHeapGetMinimum(trees);
		CFBinaryHeapRemoveMinimumValue(trees);

		// put into new node and re-insert into queue
		CFBinaryHeapAddValue(trees, (__bridge const void *)[[HuffmanNode alloc] initWithLeft:a right:b]);
	}
	HuffmanTree *result = (__bridge HuffmanTree *)CFBinaryHeapGetMinimum(trees);
	CFRelease(trees);
	return result;
}

void printCodes(HuffmanTree *tree, NSMutableString *prefix) {
	NSCAssert(tree != nil, @"tree must not be nil");
	if ([tree isKindOfClass:[HuffmanLeaf class]]) {
		HuffmanLeaf *leaf = (HuffmanLeaf *)tree;

		// print out character, frequency, and code for this leaf (which is just the prefix)
		NSLog(@"%c\t%d\t%@", leaf.value, leaf.freq, prefix);

	} else if ([tree isKindOfClass:[HuffmanNode class]]) {
		HuffmanNode *node = (HuffmanNode *)tree;

		// traverse left
		[prefix appendString:@"0"];
		printCodes(node.left, prefix);
		[prefix deleteCharactersInRange:NSMakeRange([prefix length]-1, 1)];

		// traverse right
		[prefix appendString:@"1"];
		printCodes(node.right, prefix);
		[prefix deleteCharactersInRange:NSMakeRange([prefix length]-1, 1)];
	}
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {

	NSString *test = @"this is an example for huffman encoding";

	// read each character and record the frequencies
	NSCountedSet *chars = [[NSCountedSet alloc] init];
	int n = [test length];
	for (int i = 0; i < n; i++)
		[chars addObject:@([test characterAtIndex:i])];

	// build tree
	HuffmanTree *tree = buildTree(chars);

	// print out results
	NSLog(@"SYMBOL\tWEIGHT\tHUFFMAN CODE");
	printCodes(tree, [NSMutableString string]);

    }
    return 0;
}
```


```txt

SYMBOL	WEIGHT	HUFFMAN CODE
g	1	00000
x	1	00001
m	2	0001
d	1	00100
u	1	00101
t	1	00110
r	1	00111
n	4	010
s	2	0110
o	2	0111
p	1	10000
l	1	10001
a	3	1001
 	6	101
f	3	1100
e	3	1101
c	1	11100
h	2	11101
i	3	1111

```



## OCaml

We use a Set (which is automatically sorted) as a priority queue.
```ocaml
type 'a huffman_tree =
  | Leaf of 'a
  | Node of 'a huffman_tree * 'a huffman_tree

module HSet = Set.Make
  (struct
     type t = int * char huffman_tree (* pair of frequency and the tree *)
     let compare = compare
       (* We can use the built-in compare function to order this: it will order
          first by the first element (frequency) and then by the second (the tree),
          the latter of which we don't care about but which helps prevent elements
          from being equal, since Set does not allow duplicate elements *)
   end);;

let build_tree charFreqs =
  let leaves = HSet.of_list (List.map (fun (c,f) -> (f, Leaf c)) charFreqs) in
  let rec aux trees =
    let f1, a = HSet.min_elt trees in
    let trees' = HSet.remove (f1,a) trees in
    if HSet.is_empty trees' then
      a
    else
      let f2, b = HSet.min_elt trees' in
      let trees'' = HSet.remove (f2,b) trees' in
      let trees''' = HSet.add (f1 + f2, Node (a, b)) trees'' in
      aux trees'''
  in
  aux leaves

let rec print_tree code = function
  | Leaf c ->
      Printf.printf "%c\t%s\n" c (String.concat "" (List.rev code));
  | Node (l, r) ->
      print_tree ("0"::code) l;
      print_tree ("1"::code) r

let () =
  let str = "this is an example for huffman encoding" in
  let charFreqs = Hashtbl.create 42 in
  String.iter (fun c ->
      let old =
        try Hashtbl.find charFreqs c
        with Not_found -> 0 in
      Hashtbl.replace charFreqs c (old+1)
    ) str;

  let charFreqs = Hashtbl.fold (fun c f acc -> (c,f)::acc) charFreqs [] in
  let tree = build_tree charFreqs in
  print_string "Symbol\tHuffman code\n";
  print_tree [] tree
```



## Perl


```perl
use 5.10.0;
use strict;

# produce encode and decode dictionary from a tree
sub walk {
	my ($node, $code, $h, $rev_h) = @_;

	my $c = $node->[0];
	if (ref $c) { walk($c->[$_], $code.$_, $h, $rev_h) for 0,1 }
	else        { $h->{$c} = $code; $rev_h->{$code} = $c }

	$h, $rev_h
}

# make a tree, and return resulting dictionaries
sub mktree {
	my (%freq, @nodes);
	$freq{$_}++ for split '', shift;
	@nodes = map([$_, $freq{$_}], keys %freq);

	do {	# poor man's priority queue
		@nodes = sort {$a->[1] <=> $b->[1]} @nodes;
		my ($x, $y) = splice @nodes, 0, 2;
		push @nodes, [[$x, $y], $x->[1] + $y->[1]]
	} while (@nodes > 1);

	walk($nodes[0], '', {}, {})
}

sub encode {
	my ($str, $dict) = @_;
	join '', map $dict->{$_}//die("bad char $_"), split '', $str
}

sub decode {
	my ($str, $dict) = @_;
	my ($seg, @out) = ("");

	# append to current segment until it's in the dictionary
	for (split '', $str) {
		$seg .= $_;
		my $x = $dict->{$seg} // next;
		push @out, $x;
		$seg = '';
	}
	die "bad code" if length($seg);
	join '', @out
}

my $txt = 'this is an example for huffman encoding';
my ($h, $rev_h) = mktree($txt);
for (keys %$h) { print "'$_': $h->{$_}\n" }

my $enc = encode($txt, $h);
print "$enc\n";

print decode($enc, $rev_h), "\n";
```

```txt

'u': 10000
'd': 01111
'a': 1101
'l': 10001
'i': 1110
'g': 11110
'h': 0100
'r': 01110
' ': 101
'p': 01100
't': 01101
'n': 000
'm': 0011
'x': 01011
'f': 1100
'c': 01010
'o': 0010
's': 11111
'e': 1001
0110101001110111111011110111111011101000101100101011110100110110010001100110111000010011101010100100001100110000111101000101100100001010001001111111000011110
this is an example for huffman encoding

```



## Perl 6



### By building a tree


This version uses nested <code>Array</code>s to build a tree [https://commons.wikimedia.org/wiki/File:HuffmanCodeAlg.png like shown in this diagram], and then recursively traverses the finished tree to accumulate the prefixes.

```perl6
sub huffman (%frequencies) {
    my @queue = %frequencies.map({ [.value, .key] }).sort;
    while @queue > 1 {
        given @queue.splice(0, 2) -> ([$freq1, $node1], [$freq2, $node2]) {
            @queue = (|@queue, [$freq1 + $freq2, [$node1, $node2]]).sort;
        }
    }
    hash gather walk @queue[0][1], '';
}

multi walk ($node,            $prefix) { take $node => $prefix; }
multi walk ([$node1, $node2], $prefix) { walk $node1, $prefix ~ '0';
                                         walk $node2, $prefix ~ '1'; }
```



### Without building a tree


This version uses an <code>Array</code> of <code>Pair</code>s to implement a simple priority queue. Each value of the queue is a <code>Hash</code> mapping from letters to prefixes, and when the queue is reduced the hashes are merged on-the-fly, so that the last one remaining is the wanted Huffman table.

```perl6
sub huffman (%frequencies) {
    my @queue = %frequencies.map: { .value => (hash .key => '') };
    while @queue > 1 {
        @queue.=sort;
        my $x = @queue.shift;
        my $y = @queue.shift;
        @queue.push: ($x.key + $y.key) => hash $x.value.deepmap('0' ~ *),
                                               $y.value.deepmap('1' ~ *);
    }
    @queue[0].value;
}

# Testing

for huffman 'this is an example for huffman encoding'.comb.Bag {
    say "'{.key}' : {.value}";
}

# To demonstrate that the table can do a round trip:

say '';
my $original = 'this is an example for huffman encoding';

my %encode-key = huffman $original.comb.Bag;
my %decode-key = %encode-key.invert;
my @codes      = %decode-key.keys;

my $encoded = $original.subst: /./,      { %encode-key{$_} }, :g;
my $decoded = $encoded .subst: /@codes/, { %decode-key{$_} }, :g;

.say for $original, $encoded, $decoded;
```


```txt
'x' : 11000
'p' : 01100
'h' : 0001
'g' : 00000
'a' : 1001
'e' : 1101
'd' : 110011
's' : 0111
'f' : 1110
'c' : 110010
'm' : 0010
' ' : 101
'n' : 010
'o' : 0011
'u' : 10001
't' : 10000
'i' : 1111
'r' : 01101
'l' : 00001

this is an example for huffman encoding
1000000011111011110111110111101100101010111011100010010010011000000111011011110001101101101000110001111011100010100101010111010101100100011110011111101000000
this is an example for huffman encoding
```



## Phix

```Phix
function store_nodes(object key, object data, integer nodes)
    setd({data,key},0,nodes)
    return 1
end function
constant r_store_nodes = routine_id("store_nodes")

function build_freqtable(string data)
integer freq = new_dict(),
        nodes = new_dict()
    for i=1 to length(data) do
        integer di = data[i]
        setd(di,getd(di,freq)+1,freq)
    end for
    traverse_dict(r_store_nodes, nodes, freq)
    destroy_dict(freq)
    return nodes
end function

function build_hufftree(integer nodes)
sequence lkey, rkey, node
integer lfreq, rfreq
    while true do
        lkey = getd_partial_key({0,0},nodes)
        lfreq = lkey[1]
        deld(lkey,nodes)
        rkey = getd_partial_key({0,0},nodes)
        rfreq = rkey[1]
        deld(rkey,nodes)

        node = {lfreq+rfreq,{lkey,rkey}}

        if dict_size(nodes)=0 then exit end if

        setd(node,0,nodes)
    end while
    destroy_dict(nodes)
    return node
end function

procedure build_huffcodes(object node, string bits, integer d)
    {integer freq, object data} = node
    if sequence(data) then
        build_huffcodes(data[1],bits&'0',d)
        build_huffcodes(data[2],bits&'1',d)
    else
        setd(data,{freq,bits},d)
    end if
end procedure

function print_huffcode(integer key, sequence data, integer /*user_data*/)
    printf(1,"'%c' [%d] %s\n",key&data)
    return 1
end function
constant r_print_huffcode = routine_id("print_huffcode")

procedure print_huffcodes(integer d)
    traverse_dict(r_print_huffcode, 0, d)
end procedure

function invert_huffcode(integer key, sequence data, integer rd)
    setd(data[2],key,rd)
    return 1
end function
constant r_invert_huffcode = routine_id("invert_huffcode")

procedure main(string data)
    if length(data)<2 then ?9/0 end if
    integer nodes = build_freqtable(data)
    sequence huff = build_hufftree(nodes)
    integer d = new_dict()
    build_huffcodes(huff, "", d)
    print_huffcodes(d)

    string encoded = ""
    for i=1 to length(data) do
        encoded &= getd(data[i],d)[2]
    end for
    ?encoded

    integer rd = new_dict()
    traverse_dict(r_invert_huffcode, rd, d)
    string decoded = ""
    integer done = 0
    while done<length(encoded) do
        string key = ""
        integer node = 0
        while node=0 do
            done += 1
            key &= encoded[done]
            node = getd_index(key, rd)
        end while
        decoded &= getd_by_index(node,rd)
    end while
    ?decoded

end procedure

main("this is an example for huffman encoding")
```

```txt

' ' [6] 101
'a' [3] 1001
'c' [1] 01010
'd' [1] 01011
'e' [3] 1100
'f' [3] 1101
'g' [1] 01100
'h' [2] 11111
'i' [3] 1110
'l' [1] 01101
'm' [2] 0010
'n' [4] 000
'o' [2] 0011
'p' [1] 01110
'r' [1] 01111
's' [2] 0100
't' [1] 10000
'u' [1] 10001
'x' [1] 11110
"1000011111111001001011110010010110010001011100111101001001001110011011100101110100110111110111111100011101110100101001000101110000001010001101011111000001100"
"this is an example for huffman encoding"

```



## PHP

{{trans|Python}} (not exactly)


```php
<?php
function encode($symb2freq) {
    $heap = new SplPriorityQueue;
    $heap->setExtractFlags(SplPriorityQueue::EXTR_BOTH);
    foreach ($symb2freq as $sym => $wt)
        $heap->insert(array($sym => ''), -$wt);

    while ($heap->count() > 1) {
        $lo = $heap->extract();
        $hi = $heap->extract();
        foreach ($lo['data'] as &$x)
            $x = '0'.$x;
        foreach ($hi['data'] as &$x)
            $x = '1'.$x;
        $heap->insert($lo['data'] + $hi['data'],
                      $lo['priority'] + $hi['priority']);
    }
    $result = $heap->extract();
    return $result['data'];
}

$txt = 'this is an example for huffman encoding';
$symb2freq = array_count_values(str_split($txt));
$huff = encode($symb2freq);
echo "Symbol\tWeight\tHuffman Code\n";
foreach ($huff as $sym => $code)
    echo "$sym\t$symb2freq[$sym]\t$code\n";
?>
```


```txt

Symbol	Weight	Huffman Code
n	4	000
m	2	0010
o	2	0011
t	1	01000
g	1	01001
x	1	01010
u	1	01011
s	2	0110
c	1	01110
d	1	01111
p	1	10000
l	1	10001
a	3	1001
 	6	101
f	3	1100
i	3	1101
r	1	11100
h	2	11101
e	3	1111

```



## PicoLisp

Using a cons cells (freq . char) for leaves, and two cells (freq left . right)
for nodes.

```PicoLisp
(de prio (Idx)
   (while (cadr Idx) (setq Idx @))
   (car Idx) )

(let (A NIL  P NIL  L NIL)
   (for C (chop "this is an example for huffman encoding")
      (accu 'A C 1) )                  # Count characters
   (for X A                            # Build index tree as priority queue
      (idx 'P (cons (cdr X) (car X)) T) )
   (while (or (cadr P) (cddr P))       # Remove entries, insert as nodes
      (let (A (car (idx 'P (prio P) NIL))  B (car (idx 'P (prio P) NIL)))
         (idx 'P (cons (+ (car A) (car B)) A B) T) ) )
   (setq P (car P))
   (recur (P L)                        # Traverse and print
      (if (atom (cdr P))
         (prinl (cdr P)  " " L)
         (recurse (cadr P) (cons 0 L))
         (recurse (cddr P) (cons 1 L)) ) ) )
```

```txt
n 000
m 0100
o 1100
s 0010
c 01010
d 11010
g 00110
l 10110
p 01110
r 11110
t 00001
u 10001
a 1001
  101
e 0011
f 1011
i 0111
x 01111
h 11111
```



## PL/I


```pli
*process source attributes xref or(!);
 hencode: Proc Options(main);
 /*--------------------------------------------------------------------
 * 28.12.013 Walter Pachl  translated from REXX
 *-------------------------------------------------------------------*/
 Dcl debug Bit(1) Init('0'b);
 Dcl (i,j,k) Bin Fixed(15);
 Dcl c Char(1);
 Dcl s Char(100) Var Init('this is an example for huffman encoding');
 Dcl sc Char(1000) Var Init('');
 Dcl sr Char(100)  Var Init('');
 Dcl 1 cocc(100),
      2 c  Char(1),
      2 occ Bin Fixed(31);
 Dcl cocc_n Bin Fixed(15) Init(0);
 dcl 1 node,
      2 id      Bin Fixed(15),         /* Node id               */
      2 c       Char(1),               /* character             */
      2 occ     Bin Fixed(15),         /* number of occurrences */
      2 left    Bin Fixed(15),         /* left child            */
      2 rite    Bin Fixed(15),         /* right child           */
      2 father  Bin Fixed(15),         /* father                */
      2 digit   Pic'9',                /* digit (0 or 1)        */
      2 term    Pic'9';                /* 1=terminal node       */
 node='';
 Dcl 1 m(100) Like node;
 Dcl m_n Bin Fixed(15) Init(0);
 Dcl father(100) Bin Fixed(15);

 Dcl 1 t(100),
      2 char Char(1),
      2 code Char(20) Var;
 Dcl t_n Bin Fixed(15) Init(0);

 Do i=1 To length(s);               /* first collect used characters */
   c=substr(s,i,1);                 /* and number of occurrences     */
   Do j=1 To cocc_n;
     If cocc(j).c=c Then Leave;
     End;
   If j<= cocc_n Then
     cocc(j).occ+=1;
   Else Do;
     cocc(j).c=c;
     cocc(j).occ=1;
     cocc_n+=1;
     End;
   End;

 Do j=1 To cocc_n;                     /* create initial node list   */
   node.id+=1;
   node.c=cocc(j).c;
   node.occ=cocc(j).occ;
   node.term=1;
   Call add_node;
   End;

 If debug Then
   Call show;

 Do While(pairs());  /* while there is more than one fatherless node */
   Call mk_node;                       /* create a father node       */
   If debug Then
     Call show;
   End;

 Call show;                            /* show the node table        */

 Call mk_trans;                        /* create the translate table */
 Put Edit('The translate table:')(Skip,a);
 Do i=1 To t_n;                        /* show it                    */
   Put Edit(t(i).char,' -> ',t(i).code)(Skip,a,a,a);
   End;

 Call encode;                          /* encode the string s -> sc  */

 Put Edit('length(sc)=',length(sc))    /* show it                    */
         (Skip,a,f(3));
 Do i=1 By 70 To length(sc);
   Put Edit(substr(sc,i,70))(Skip,a);
   End;

 Call decode;                          /* decode the string sc -> sr */
 Put Edit('input : ',s)(skip,a,a);
 Put Edit('result: ',sr)(skip,a,a);
 Return;

 add_node: Proc;
 /*--------------------------------------------------------------------
 * Insert the node according to increasing occurrences
 *-------------------------------------------------------------------*/
 il:
   Do i=1 To m_n;
     If m(i).occ>=node.occ Then Do;
       Do k=m_n To i By -1;
         m(k+1)=m(k);
         End;
       Leave il;
       End;
     End;
   m(i)=node;
   m_n+=1;
 End;

 show: Proc;
 /*--------------------------------------------------------------------
 * Show the contents of the node table
 *-------------------------------------------------------------------*/
 Put Edit('The list of nodes:')(Skip,a);
 Put Edit('id c oc  l  r  f d  t')(Skip,a);
 Do i=1 To m_n;
   Put Edit(m(i).id,m(i).c,m(i).occ,
            m(i).left,m(i).rite,m(i).father,m(i).digit,m(i).term)
           (Skip,f(2),x(1),a,4(f(3)),f(2),f(3));
   End;
 End;

 mk_node: Proc;
 /*--------------------------------------------------------------------
 * construct and store a new intermediate node or the top node
 *-------------------------------------------------------------------*/
 Dcl z Bin Fixed(15);
 node='';
 node.id=m_n+1;                /* the next node id                   */
 node.c='*';
 ni=m_n+1;
 loop:
 Do i=1 To m_n;                /* loop over node lines               */
  If m(i).father=0 Then Do;    /* a fatherless node                  */
    z=m(i).id;                 /* its id                             */
    If node.left=0 Then Do;    /* new node has no left child         */
       node.left=z;            /* make this the lect child           */
       node.occ=m(i).occ;      /* occurrences                        */
       m(i).father=ni;         /* store father info                  */
       m(i).digit=0;           /* digit 0 to be used                 */
       father(z)=ni;           /* remember z's father (redundant)    */
       End;
     Else Do;                  /* New node has already left child    */
       node.rite=z;            /* make this the right child          */
       node.occ=node.occ+m(i).occ;  /* add in the occurrences        */
       m(i).father=ni;         /* store father info                  */
       m(i).digit=1;           /* digit 1 to be used                 */
       father(z)=ni;           /* remember z's father (redundant)    */
       Leave loop;
       End;
     End;
   End;
 Call add_node;
 End;

 pairs: Proc Returns(Bit(1));
 /*--------------------------------------------------------------------
 * Return true if there are at least 2 fatherless nodes
 *-------------------------------------------------------------------*/
 Dcl i   Bin Fixed(15);
 Dcl cnt Bin Fixed(15) Init(0);
 Do i=1 To m_n;
   If m(i).father=0 Then Do;
     cnt+=1;
     If cnt>1 Then
       Return('1'b);
     End;
   End;
 Return('0'b);
 End;

 mk_trans: Proc;
 /*--------------------------------------------------------------------
 * Compute the codes for all terminal nodes (characters)
 * and store the relation char -> code in array t(*)
 *-------------------------------------------------------------------*/
 Dcl (i,fi,fid,fidz,node,z) Bin Fixed(15);
 Dcl code Char(20) Var;
 Do i=1 To m_n;     /* now we loop over all lines representing nodes */
   If m(i).term Then Do;   /* for each terminal node                 */
     code=m(i).digit;      /* its digit is the last code digit       */
     node=m(i).id;         /* its id                                 */
     Do fi=1 To 1000;      /* actually Forever                       */
       fid=father(node);   /* id of father                           */
       If fid>0 Then Do;   /* father exists                          */
         fidz=zeile(fid);  /* line that contains the father          */
         code=m(fidz).digit!!code;    /* prepend the digit           */
         node=fid;         /* look for next father                   */
         End;
       Else                /* no father (we reached the top          */
         Leave;
       End;
     If length(code)>1 Then /* more than one character in input      */
       code=substr(code,2); /* remove the the top node's 0           */
     call dbg(m(i).c!!' -> '!!code); /* character is encoded this way*/
 ti_loop:
     Do ti=1 To t_n;
       If t(ti).char>m(i).c Then Do;
         Do tj=t_n To ti By -1
           t(tj+1)=t(tj);
           End;
         Leave ti_loop;
         End;
       End;
     t(ti).char=m(i).c;
     t(ti).code=code;
     t_n+=1;
     Call dbg(t(ti).char!!' -> '!!t(ti).code);
     End;
   End;
 End;

 zeile: Proc(nid) Returns(Bin Fixed(15));
 /*--------------------------------------------------------------------
 * find and return line number containing node-id
 *-------------------------------------------------------------------*/
 Dcl (nid,i) Bin Fixed(15);
 do i=1 To m_n;
   If m(i).id=nid Then
     Return(i);
   End;
 Stop;
 End;

 dbg: Proc(txt);
 /*--------------------------------------------------------------------
 * Show text if debug is enabled
 *-------------------------------------------------------------------*/
 Dcl txt Char(*);
 If debug Then
   Put Skip List(txt);
 End;

 encode: Proc;
 /*--------------------------------------------------------------------
 * encode the string s -> sc
 *-------------------------------------------------------------------*/
 Dcl (i,j) Bin Fixed(15);
 Do i=1 To length(s);
   c=substr(s,i,1);
   Do j=1 To t_n;
     If c=t(j).char Then
       Leave;
     End;
   sc=sc!!t(j).code;
   End;
 End;

 decode: Proc;
 /*--------------------------------------------------------------------
 * decode the string sc -> sr
 *-------------------------------------------------------------------*/
 Dcl (i,j) Bin Fixed(15);
 Do While(sc>'');
   Do j=1 To t_n;
     If substr(sc,1,length(t(j).code))=t(j).code Then
       Leave;
     End;
   sr=sr!!t(j).char;
   sc=substr(sc,length(t(j).code)+1);
   End;
 End;

 End;
```

```txt
The list of nodes:
id c oc  l  r  f d  t
19 g  1  0  0 20 0  1
18 d  1  0  0 20 1  1
17 c  1  0  0 21 0  1
16 u  1  0  0 21 1  1
15 r  1  0  0 22 0  1
12 l  1  0  0 22 1  1
11 p  1  0  0 23 0  1
 9 x  1  0  0 23 1  1
 1 t  1  0  0 24 0  1
23 *  2 11  9 24 1  0
22 *  2 15 12 25 0  0
21 *  2 17 16 25 1  0
20 *  2 19 18 26 0  0
14 o  2  0  0 26 1  1
10 m  2  0  0 27 0  1
 4 s  2  0  0 27 1  1
 2 h  2  0  0 28 0  1
24 *  3  1 23 28 1  0
13 f  3  0  0 29 0  1
 8 e  3  0  0 29 1  1
 6 a  3  0  0 30 0  1
 3 i  3  0  0 30 1  1
27 *  4 10  4 31 0  0
26 *  4 20 14 31 1  0
25 *  4 22 21 32 0  0
 7 n  4  0  0 32 1  1
28 *  5  2 24 33 0  0
30 *  6  6  3 33 1  0
29 *  6 13  8 34 0  0
 5    6  0  0 34 1  1
32 *  8 25  7 35 0  0
31 *  8 27 26 35 1  0
33 * 11 28 30 36 0  0
34 * 12 29  5 36 1  0
35 * 16 32 31 37 0  0
36 * 23 33 34 37 1  0
37 * 39 35 36  0 0  0
The translate table:
  -> 111
a -> 1010
c -> 00010
d -> 01101
e -> 1101
f -> 1100
g -> 01100
h -> 1000
i -> 1011
l -> 00001
m -> 0100
n -> 001
o -> 0111
p -> 100110
r -> 00000
s -> 0101
t -> 10010
u -> 00011
x -> 100111
length(sc)=157
1001010001011010111110110101111101000111111011001111010010010011000001
1101111110001110000011110000001111001100010010100011111101001000100111
01101101100101100
input : this is an example for huffman encoding
result: this is an example for huffman encoding
```



## PowerShell

```PowerShell

function Get-HuffmanEncodingTable ( $String )
    {
    #  Create leaf nodes
    $ID = 0
    $Nodes = [char[]]$String |
        Group-Object |
        ForEach { $ID++; $_ } |
        Select  @{ Label = 'Symbol'  ; Expression = { $_.Name  } },
                @{ Label = 'Count'   ; Expression = { $_.Count } },
                @{ Label = 'ID'      ; Expression = { $ID      } },
                @{ Label = 'Parent'  ; Expression = { 0        } },
                @{ Label = 'Code'    ; Expression = { ''       } }

    #  Grow stems under leafs
    ForEach ( $Branch in 2..($Nodes.Count) )
        {
        #  Get the two nodes with the lowest count
        $LowNodes = $Nodes | Where Parent -eq 0 | Sort Count | Select -First 2

        #  Create a new stem node
        $ID++
        $Nodes += '' |
            Select  @{ Label = 'Symbol'  ; Expression = { ''       } },
                    @{ Label = 'Count'   ; Expression = { $LowNodes[0].Count + $LowNodes[1].Count } },
                    @{ Label = 'ID'      ; Expression = { $ID      } },
                    @{ Label = 'Parent'  ; Expression = { 0        } },
                    @{ Label = 'Code'    ; Expression = { ''       } }

        #  Put the two nodes in the new stem node
        $LowNodes[0].Parent = $ID
        $LowNodes[1].Parent = $ID

        #  Assign 0 and 1 to the left and right nodes
        $LowNodes[0].Code = '0'
        $LowNodes[1].Code = '1'
        }

    #  Assign coding to nodes
    ForEach ( $Node in $Nodes[($Nodes.Count-2)..0] )
        {
        $Node.Code = ( $Nodes | Where ID -eq $Node.Parent ).Code + $Node.Code
        }

    $EncodingTable = $Nodes | Where { $_.Symbol } | Select Symbol, Code | Sort Symbol
    return $EncodingTable
    }

#  Get table for given string
$String = "this is an example for huffman encoding"
$HuffmanEncodingTable = Get-HuffmanEncodingTable $String

#  Display table
$HuffmanEncodingTable | Format-Table -AutoSize

#  Encode string
$EncodedString = $String
ForEach ( $Node in $HuffmanEncodingTable )
    {
    $EncodedString = $EncodedString.Replace( $Node.Symbol, $Node.Code )
    }
$EncodedString

```

```txt

Symbol Code
------ ----
       101
a      1100
c      01011
d      01100
e      1101
f      1110
g      01110
h      11111
i      1001
l      11110
m      0011
n      000
o      0100
p      10001
r      01111
s      0010
t      01010
u      01101
x      10000


0101011111100100101011001001010111000001011101100001100001110001111101101101111001000111110111111011011110111000111100000101110100001011010001100100100001110

```



## Prolog

Works with SWI-Prolog

```Prolog
huffman :-
	L = 'this is an example for huffman encoding',
	atom_chars(L, LA),
	msort(LA, LS),
	packList(LS, PL),
	sort(PL, PLS),
	build_tree(PLS, A),
	coding(A, [], C),
	sort(C, SC),
	format('Symbol~t   Weight~t~30|Code~n'),
	maplist(print_code, SC).

build_tree([[V1|R1], [V2|R2]|T], AF) :-
	V is V1 + V2,
	A = [V, [V1|R1], [V2|R2]],
	(   T=[] -> AF=A ;  sort([A|T], NT), build_tree(NT, AF) ).

coding([_A,FG,FD], Code, CF) :-
	(   is_node(FG) ->  coding(FG, [0 | Code], C1)
			 ;  leaf_coding(FG, [0 | Code], C1) ),
	(   is_node(FD) ->  coding(FD, [1 | Code], C2)
			 ;  leaf_coding(FD, [1 | Code], C2) ),
	append(C1, C2, CF).

leaf_coding([FG,FD], Code, CF) :-
	reverse(Code, CodeR),
	CF = [[FG, FD, CodeR]] .

is_node([_V, _FG, _FD]).

print_code([N, Car, Code]):-
	format('~w :~t~w~t~30|', [Car, N]),
	forall(member(V, Code), write(V)),
	nl.

packList([], []).
packList([X], [[1,X]]) :- !.
packList([X|Rest], [XRun|Packed]):-
    run(X, Rest, XRun, RRest),
    packList(RRest, Packed).

run(V, [], [1,V], []).
run(V, [V|LRest], [N1,V], RRest):-
    run(V, LRest, [N, V], RRest),
    N1 is N + 1.
run(V, [Other|RRest], [1,V], [Other|RRest]):-
    dif(V, Other).
```

```txt
 ?- huffman.
Symbol          Weight        Code
c :             1             01010
d :             1             01011
g :             1             01100
l :             1             01101
p :             1             01110
r :             1             01111
t :             1             10000
u :             1             10001
x :             1             11110
h :             2             11111
m :             2             0010
o :             2             0011
s :             2             0100
a :             3             1001
e :             3             1100
f :             3             1101
i :             3             1110
n :             4             000
  :             6             101

```



## PureBasic

```PureBasic

OpenConsole()

SampleString.s="this is an example for huffman encoding"
datalen=Len(SampleString)

Structure ztree
  linked.c
  ischar.c
  char.c
  number.l
  left.l
  right.l
EndStructure

Dim memc.c(datalen)
CopyMemory(@SampleString, @memc(0), datalen * SizeOf(Character))

Dim tree.ztree(255)

For i=0 To datalen-1
  tree(memc(i))\char=memc(i)
  tree(memc(i))\number+1
  tree(memc(i))\ischar=1
Next

SortStructuredArray(tree(),#PB_Sort_Descending,OffsetOf(ztree\number),#PB_Integer)

For i=0 To 255
  If tree(i)\number=0
    ReDim tree(i-1)
    Break
  EndIf
Next

dimsize=ArraySize(tree())
Repeat
  min1.l=0
  min2.l=0
  For i=0 To dimsize
    If tree(i)\linked=0
      If tree(i)\number<min1 Or min1=0
        min1=tree(i)\number
        hmin1=i
      ElseIf tree(i)\number<min2 Or min2=0
        min2=tree(i)\number
        hmin2=i
      EndIf
    EndIf
  Next

  If min1=0 Or min2=0
    Break
  EndIf

  dimsize+1
  ReDim tree(dimsize)
  tree(dimsize)\number=tree(hmin1)\number+tree(hmin2)\number
  tree(hmin1)\left=dimsize
  tree(hmin2)\right=dimsize
  tree(hmin1)\linked=1
  tree(hmin2)\linked=1
ForEver

i=0
While tree(i)\ischar=1
  str.s=""
  k=i
  ZNEXT:
  If tree(k)\left<>0
    str="0"+str
    k=tree(k)\left
    Goto ZNEXT
  ElseIf tree(k)\right<>0
    str="1"+str
    k=tree(k)\right
    Goto ZNEXT
  EndIf
  PrintN(Chr(tree(i)\char)+" "+str)
  i+1
Wend
Input()

CloseConsole()

```


```txt
  110
n 000
e 1010
f 1001
a 1011
i 1110
h 0010
s 11111
o 0011
m 0100
x 01010
u 01011
l 01100
r 01101
c 01110
g 01111
p 10000
t 10001
d 11110
```



## Python

A [http://paddy3118.blogspot.com/2009/03/huffman-encoding-in-python.html slight modification] of the method outlined in the task description allows the code to be accumulated as the heap is manipulated.

The output is sorted first on length of the code, then on the symbols.


```python
from heapq import heappush, heappop, heapify
from collections import defaultdict

def encode(symb2freq):
    """Huffman encode the given dict mapping symbols to weights"""
    heap = [[wt, [sym, ""]] for sym, wt in symb2freq.items()]
    heapify(heap)
    while len(heap) > 1:
        lo = heappop(heap)
        hi = heappop(heap)
        for pair in lo[1:]:
            pair[1] = '0' + pair[1]
        for pair in hi[1:]:
            pair[1] = '1' + pair[1]
        heappush(heap, [lo[0] + hi[0]] + lo[1:] + hi[1:])
    return sorted(heappop(heap)[1:], key=lambda p: (len(p[-1]), p))

txt = "this is an example for huffman encoding"
symb2freq = defaultdict(int)
for ch in txt:
    symb2freq[ch] += 1
# in Python 3.1+:
# symb2freq = collections.Counter(txt)
huff = encode(symb2freq)
print "Symbol\tWeight\tHuffman Code"
for p in huff:
    print "%s\t%s\t%s" % (p[0], symb2freq[p[0]], p[1])
```


```txt
Symbol  Weight  Huffman Code
    6   101
n   4   010
a   3   1001
e   3   1100
f   3   1101
h   2   0001
i   3   1110
m   2   0010
o   2   0011
s   2   0111
g   1   00000
l   1   00001
p   1   01100
r   1   01101
t   1   10000
u   1   10001
x   1   11110
c   1   111110
d   1   111111
```


An extension to the method outlined above is given [http://paddy3118.blogspot.com/2009/04/abuse-of-pythons-in-built-data.html here].



## Racket


```racket

#lang racket

(require data/heap
         data/bit-vector)

;; A node is either an interior, or a leaf.
;; In either case, they record an item with an associated frequency.
(struct node (freq) #:transparent)
(struct interior node (left right) #:transparent)
(struct leaf node (val) #:transparent)

;; node<=?: node node -> boolean
;; Compares two nodes by frequency.
(define (node<=? x y)
  (<= (node-freq x) (node-freq y)))

;; make-huffman-tree: (listof leaf) -> interior-node
(define (make-huffman-tree leaves)
  (define a-heap (make-heap node<=?))
  (heap-add-all! a-heap leaves)
  (for ([i (sub1 (length leaves))])
    (define min-1 (heap-min a-heap))
    (heap-remove-min! a-heap)
    (define min-2 (heap-min a-heap))
    (heap-remove-min! a-heap)
    (heap-add! a-heap (interior (+ (node-freq min-1) (node-freq min-2))
                                min-1 min-2)))
  (heap-min a-heap))

;; string->huffman-tree: string -> node
;; Given a string, produces its huffman tree.  The leaves hold the characters
;; and their relative frequencies.
(define (string->huffman-tree str)
  (define ht (make-hash))
  (define n (sequence-length str))
  (for ([ch str])
     (hash-update! ht ch add1 (λ () 0)))
  (make-huffman-tree
   (for/list ([(k v) (in-hash ht)])
     (leaf (/ v n) k))))

;; make-encoder: node -> (string -> bit-vector)
;; Given a huffman tree, generates the encoder function.
(define (make-encoder a-tree)
  (define dict (huffman-tree->dictionary a-tree))
  (lambda (a-str)
    (list->bit-vector (apply append (for/list ([ch a-str]) (hash-ref dict ch))))))

;; huffman-tree->dictionary: node -> (hashof val (listof boolean))
;; A helper for the encoder: maps characters to their code sequences.
(define (huffman-tree->dictionary a-node)
  (define ht (make-hash))
  (let loop ([a-node a-node]
             [path/rev '()])
    (cond
      [(interior? a-node)
       (loop (interior-left a-node) (cons #f path/rev))
       (loop (interior-right a-node) (cons #t path/rev))]
      [(leaf? a-node)
        (hash-set! ht (reverse path/rev) (leaf-val a-node))]))

  (for/hash ([(k v) ht])
    (values v k)))

;; make-decoder: interior-node -> (bit-vector -> string)
;; Generates the decoder function from the tree.
(define (make-decoder a-tree)
  (lambda (a-bitvector)
    (define-values (decoded/rev _)
      (for/fold ([decoded/rev '()]
                 [a-node a-tree])
                ([bit a-bitvector])
        (define next-node
          (cond
            [(not bit)
             (interior-left a-node)]
            [else
             (interior-right a-node)]))
        (cond [(leaf? next-node)
               (values (cons (leaf-val next-node) decoded/rev)
                       a-tree)]
              [else
               (values decoded/rev next-node)])))
    (apply string (reverse decoded/rev))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example application:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define msg "this is an example for huffman encoding")

(define tree (string->huffman-tree msg))

;; We can print out the mapping for inspection:
(huffman-tree->dictionary tree)

(define encode (make-encoder tree))
(define encoded (encode msg))

;; Here's what the encoded message looks like:
(bit-vector->string encoded)

(define decode (make-decoder tree))
;; Here's what the decoded message looks like:
(decode encoded)
```



## Red


```Red
Red [file: %huffy.red]

;; message to encode:
msg: "this is an example for huffman encoding"

;;map to collect leave knots  per uniq character of message
m: make map! []

knot: make object! [
	left: right: none    ;; pointer to left/right sibling
	code: none        ;; first holds char for debugging, later binary code
	count: depth: 1     ;;occurence of character - length of branch
]

;;-----------------------------------------
set-code: func  ["recursive function to generate binary code sequence"
			wknot
			wcode [string!]] [
;;-----------------------------------------
	either wknot/left = none [
		wknot/code:  wcode
	] [
		set-code wknot/left rejoin [wcode "1"]
		set-code wknot/right rejoin [wcode "0"]
	]
]	;;-- end func

;-------------------------------
merge-2knots: func ["function to merge 2 knots into 1 new"
	t [block!]][
;-------------------------------
	nknot: copy knot      ;; create new knot
	nknot/count:  t/1/count + t/2/count
	nknot/right: t/1
	nknot/left: t/2
	nknot/depth: t/1/depth + 1
	tab: remove/part t 2	;; delete first 2 knots
	insert t nknot  ;; insert new generated knot
]	;;-- end func

;; count occurence of characters, save in map: m
foreach chr msg [
	either k:  select/case m chr [
			k/count: k/count + 1
	][
		put/case m chr nknot: copy knot
		nknot/code: chr
	]
]

;; create sortable block (=tab) for use as prio queue
foreach k  keys-of m [	append tab: []  :m/:k ]

;; build tree
while [ 1 <  length? tab][
	sort/compare tab function [a b] [
					a/count  <  b/count
		  or (   a/count = b/count and ( a/depth > b/depth ) )
	]
	merge-2knots tab	;; merge 2 knots with lowest count / max depth
]

set-code tab/1 ""		;; generate binary codes, save at leave knot

;;  display codes
foreach k sort keys-of m [
	print [k " = " m/:k/code]
	append codes: "" m/:k/code
]

;; encode orig message string
foreach chr msg [
	k: select/case m chr
	 append msg-new: ""   k/code
]

print [ "length of encoded msg "  length? msg-new]
print [ "length of (binary) codes "  length? codes ]

print ["orig. message: "  msg newline "encoded message: " "^/" msg-new]
prin "decoded: "

;; decode message (destructive! ):
while [ not empty? msg-new ][
  foreach [k v] body-of   m [
    if  t: find/match msg-new v/code   [
      prin k
      msg-new: t
    ]
  ]
 ]

```

```txt
   =  111
a  =  1101
c  =  00101
d  =  00100
e  =  1011
f  =  1100
g  =  10010
h  =  1000
i  =  1010
l  =  00000
m  =  0001
n  =  011
o  =  0101
p  =  00001
r  =  00111
s  =  0100
t  =  100111
u  =  100110
x  =  00110
length of encoded msg  157
length of (binary) codes  85
orig. message:  this is an example for huffman encoding
encoded message:
1001111000101001001111010010011111010111111011001101101000100001000001011111110001010011111110001001101100110000011101011111101101100101010100100101001110010
decoded: this is an example for huffman encoding

```


## REXX


```rexx
/* REXX ---------------------------------------------------------------
* 27.12.2013 Walter Pachl
* 29.12.2013 -"- changed for test of s=xrange('00'x,'ff'x)
* 14.03.2018 -"- use format instead of right to diagnose size poblems
* Stem m contains eventually the following node data
* m.i.0id Node id
* m.i.0c  character
* m.i.0o  number of occurrences
* m.i.0l  left child
* m.i.0r  right child
* m.i.0f  father
* m.i.0d  digit (0 or 1)
* m.i.0t  1=a terminal node 0=an intermediate or the top node
*--------------------------------------------------------------------*/
Parse Arg s
If s='' Then
  s='this is an example for huffman encoding'
Say 'We encode this string:'
Say s
debug=0
o.=0
c.=0
codel.=0
code.=''
father.=0
cl=''                                  /* list of characters         */
do i=1 To length(s)
  Call memorize substr(s,i,1)
  End
If debug Then Do
  Do i=1 To c.0
    c=c.i
    Say i c o.c
    End
  End
n.=0
Do i=1 To c.0
  c=c.i
  n.i.0c=c
  n.i.0o=o.c
  n.i.0id=i
  Call dbg i n.i.0id n.i.0c n.i.0o
  End
n=c.0                                  /* number of nodes            */
m.=0
Do i=1 To n                            /* construct initial array    */
  Do j=1 To m.0                        /* sorted by occurrences      */
    If m.j.0o>n.i.0o Then
      Leave
    End
  Do k=m.0 To j By -1
    k1=k+1
    m.k1.0id=m.k.0id
    m.k1.0c =m.k.0c
    m.k1.0o =m.k.0o
    m.k1.0t =m.k.0t
    End
  m.j.0id=i
  m.j.0c =n.i.0c
  m.j.0o =n.i.0o
  m.j.0t =1
  m.0=m.0+1
  End
If debug Then
  Call show

Do While pairs()>1    /* while there are at least 2 fatherless nodes */
  Call mknode         /* create and fill a new father node           */
  If debug Then
    Call show
  End

Call show
c.=0
Do i=1 To m.0       /* now we loop over all lines representing nodes */
  If m.i.0t Then Do   /* for each terminal node                 */
    code=m.i.0d       /* its digit is the last code digit            */
    node=m.i.0id      /* its id                                      */
    Do fi=1 To 1000   /* actually Forever                            */
      fid=father.node           /* id of father                      */
      If fid<>0 Then Do         /* father exists                     */
        fidz=zeile(fid)         /* line that contains the father     */
        code=m.fidz.0d||code    /* prepend the digit                 */
        node=fid                /* look for next father              */
        End
      Else                      /* no father (we reached the top     */
        Leave
      End
    If length(code)>1 Then      /* more than one character in input  */
      code=substr(code,2)       /* remove the the top node's 0       */
    call dbg m.i.0c '->' code   /* character is encoded this way     */
    char=m.i.0c
    code.char=code
    z=codel.0+1
    codel.z=code
    codel.0=z
    char.code=char
    End
  End

Call show_char2code  /* show used characters and corresponding codes */

codes.=0               /* now we build the array of codes/characters */
Do j=1 To codel.0
  z=codes.0+1
  code=codel.j
  codes.z=code
  chars.z=char.code
  codes.0=z
  Call dbg codes.z '----->' chars.z
  End

sc=''                  /* here we ecnode the string                  */
Do i=1 To length(s)    /* loop over input                            */
  c=substr(s,i,1)      /* a character                                */
  sc=sc||code.c        /* append the corresponding code              */
  End
Say 'Length of encoded string:' length(sc)
Do i=1 To length(sc) by 70
  Say substr(sc,i,70)
  End

sr=''                  /* now decode the string                      */
Do si=1 To 999 While sc<>''
  Do i=codes.0 To 1 By -1              /* loop over codes            */
    cl=length(codes.i)                 /* length of code             */
    If left(sc,cl)==codes.i Then Do    /* found on top of string     */
      sr=sr||chars.i                   /* append character to result */
      sc=substr(sc,cl+1)               /* cut off the used code      */
      Leave                            /* this was one character     */
      End
    End
  End
Say 'Input ="'s'"'
Say 'result="'sr'"'

Exit

show:
/*---------------------------------------------------------------------
* show all lines representing node data
*--------------------------------------------------------------------*/
Say '  i   pp  id   c   f   l r d'
Do i=1 To m.0
  Say format(i,3) format(m.i.0o,4) format(m.i.0id,3),
          format(m.i.0f,3) format(m.i.0l,3) format(m.i.0r,3) m.i.0d m.i.0t
  End
Call dbg copies('-',21)
Return

pairs: Procedure Expose m.
/*---------------------------------------------------------------------
* return number of fatherless nodes
*--------------------------------------------------------------------*/
  res=0
  Do i=1 To m.0
    If m.i.0f=0 Then
      res=res+1
    End
  Return res

mknode:
/*---------------------------------------------------------------------
* construct and store a new intermediate or the top node
*--------------------------------------------------------------------*/
new.=0
ni=m.0+1                 /* the next node id                         */
Do i=1 To m.0            /* loop over node lines                     */
  If m.i.0f=0 Then Do    /* a fatherless node                        */
    z=m.i.0id            /* its id                                   */
    If new.0l=0 Then Do  /* new node has no left child               */
      new.0l=z           /* make this the lect child                 */
      new.0o=m.i.0o      /* occurrences                              */
      m.i.0f=ni          /* store father info                        */
      m.i.0d='0'         /* digit 0 to be used                       */
      father.z=ni        /* remember z's father (redundant)          */
      End
    Else Do              /* New node has already left child          */
      new.0r=z           /* make this the right child                */
      new.0o=new.0o+m.i.0o  /* add in the occurrences                */
      m.i.0f=ni          /* store father info                        */
      m.i.0d=1           /* digit 1 to be used                       */
      father.z=ni        /* remember z's father (redundant)          */
      Leave
      End
    End
  End
Do i=1 To m.0            /* Insert new node according to occurrences */
  If m.i.0o>=new.0o Then Do
    Do k=m.0 To i By -1
      k1=k+1
      m.k1.0id=m.k.0id
      m.k1.0o =m.k.0o
      m.k1.0c =m.k.0c
      m.k1.0l =m.k.0l
      m.k1.0r =m.k.0r
      m.k1.0f =m.k.0f
      m.k1.0d =m.k.0d
      m.k1.0t =m.k.0t
      End
    Leave
    End
  End
m.i.0id=ni
m.i.0c ='*'
m.i.0o =new.0o
m.i.0l =new.0l
m.i.0r =new.0r
m.i.0t =0
father.ni=0
m.0=ni
Return

zeile:
/*---------------------------------------------------------------------
* find and return line number containing node-id
*--------------------------------------------------------------------*/
  do fidz=1 To m.0
    If m.fidz.0id=arg(1) Then
      Return fidz
    End
  Call dbg arg(1) 'not found'
  Pull .

dbg:
/*---------------------------------------------------------------------
* Show text if debug is enabled
*--------------------------------------------------------------------*/
  If debug=1 Then
    Say arg(1)
  Return


memorize: Procedure Expose c. o.
/*---------------------------------------------------------------------
* store characters and corresponding occurrences
*--------------------------------------------------------------------*/
  Parse Arg c
  If o.c=0 Then Do
    z=c.0+1
    c.z=c
    c.0=z
    End
  o.c=o.c+1
  Return

show_char2code:
/*---------------------------------------------------------------------
* show used characters and corresponding codes
*--------------------------------------------------------------------*/
cl=xrange('00'x,'ff'x)
Say 'char --> code'
Do While cl<>''
  Parse Var cl c +1 cl
  If code.c<>'' Then
    Say '   'c '-->' code.c
  End
Return
```

```txt
We encode this string:
this is an example for huffman encoding
  i   pp  id   c   f   l r d
  1    1   1  20   0   0 0 1
  2    1   9  20   0   0 1 1
  3    1  11  21   0   0 0 1
  4    1  12  21   0   0 1 1
  5    1  15  22   0   0 0 1
  6    1  16  22   0   0 1 1
  7    1  17  23   0   0 0 1
  8    1  18  23   0   0 1 1
  9    1  19  24   0   0 0 1
 10    2  23  24  17  18 1 0
 11    2  22  25  15  16 0 0
 12    2  21  25  11  12 1 0
 13    2  20  26   1   9 0 0
 14    2   2  26   0   0 1 1
 15    2   4  27   0   0 0 1
 16    2  10  27   0   0 1 1
 17    2  14  28   0   0 0 1
 18    3  24  28  19  23 1 0
 19    3   3  29   0   0 0 1
 20    3   6  29   0   0 1 1
 21    3   8  30   0   0 0 1
 22    3  13  30   0   0 1 1
 23    4  27  31   4  10 0 0
 24    4  26  31  20   2 1 0
 25    4  25  32  22  21 0 0
 26    4   7  32   0   0 1 1
 27    5  28  33  14  24 0 0
 28    6  30  33   8  13 1 0
 29    6  29  34   3   6 0 0
 30    6   5  34   0   0 1 1
 31    8  32  35  25   7 0 0
 32    8  31  35  27  26 1 0
 33   11  33  36  28  30 0 0
 34   12  34  36  29   5 1 0
 35   16  35  37  32  31 0 0
 36   23  36  37  33  34 1 0
 37   39  37   0  35  36 0 0
char --> code
     --> 111
   a --> 1101
   c --> 100110
   d --> 100111
   e --> 1010
   f --> 1011
   g --> 10010
   h --> 0111
   i --> 1100
   l --> 00011
   m --> 0101
   n --> 001
   o --> 1000
   p --> 00010
   r --> 00000
   s --> 0100
   t --> 01100
   u --> 00001
   x --> 01101
Length of encoded string: 157
0110001111100010011111000100111110100111110100110111010101000100001110
1011110111000000001110111000011011101101011101001111101000110011010001
00111110000110010
Input ="this is an example for huffman encoding"
result="this is an example for huffman encoding"
```



## Ruby

Uses a {{libheader|RubyGems}} package [http://ruby.brian-amberg.de/priority-queue/ PriorityQueue]

```ruby
require 'priority_queue'

def huffman_encoding(str)
  char_count = Hash.new(0)
  str.each_char {|c| char_count[c] += 1}

  pq = CPriorityQueue.new
  # chars with fewest count have highest priority
  char_count.each {|char, count| pq.push(char, count)}

  while pq.length > 1
    key1, prio1 = pq.delete_min
    key2, prio2 = pq.delete_min
    pq.push([key1, key2], prio1 + prio2)
  end

  Hash[*generate_encoding(pq.min_key)]
end

def generate_encoding(ary, prefix="")
  case ary
  when Array
    generate_encoding(ary[0], "#{prefix}0") + generate_encoding(ary[1], "#{prefix}1")
  else
    [ary, prefix]
  end
end

def encode(str, encoding)
  str.each_char.collect {|char| encoding[char]}.join
end

def decode(encoded, encoding)
  rev_enc = encoding.invert
  decoded = ""
  pos = 0
  while pos < encoded.length
    key = ""
    while rev_enc[key].nil?
      key << encoded[pos]
      pos += 1
    end
    decoded << rev_enc[key]
  end
  decoded
end

str = "this is an example for huffman encoding"
encoding = huffman_encoding(str)
encoding.to_a.sort.each {|x| p x}

enc = encode(str, encoding)
dec = decode(enc, encoding)
puts "success!" if str == dec
```


```txt
[" ", "111"]
["a", "1011"]
["c", "00001"]
["d", "00000"]
["e", "1101"]
["f", "1100"]
["g", "00100"]
["h", "1000"]
["i", "1001"]
["l", "01110"]
["m", "10101"]
["n", "010"]
["o", "0001"]
["p", "00101"]
["r", "00111"]
["s", "0110"]
["t", "00110"]
["u", "01111"]
["x", "10100"]
success!

```



## Rust


Adapted C++ solution.


```rust

use std::collections::BTreeMap;
use std::collections::binary_heap::BinaryHeap;

#[derive(Debug, Eq, PartialEq)]
enum NodeKind {
    Internal(Box<Node>, Box<Node>),
    Leaf(char),
}

#[derive(Debug, Eq, PartialEq)]
struct Node {
    frequency: usize,
    kind: NodeKind,
}

impl Ord for Node {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        rhs.frequency.cmp(&self.frequency)
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(&rhs))
    }
}

type HuffmanCodeMap = BTreeMap<char, Vec<u8>>;

fn main() {
    let text = "this is an example for huffman encoding";

    let mut frequencies = BTreeMap::new();
    for ch in text.chars() {
        *frequencies.entry(ch).or_insert(0) += 1;
    }

    let mut prioritized_frequencies = BinaryHeap::new();
    for counted_char in frequencies {
        prioritized_frequencies.push(Node {
            frequency: counted_char.1,
            kind: NodeKind::Leaf(counted_char.0),
        });
    }

    while prioritized_frequencies.len() > 1 {
        let left_child = prioritized_frequencies.pop().unwrap();
        let right_child = prioritized_frequencies.pop().unwrap();
        prioritized_frequencies.push(Node {
            frequency: right_child.frequency + left_child.frequency,
            kind: NodeKind::Internal(Box::new(left_child), Box::new(right_child)),
        });
    }

    let mut codes = HuffmanCodeMap::new();
    generate_codes(
        prioritized_frequencies.peek().unwrap(),
        vec![0u8; 0],
        &mut codes,
    );

    for item in codes {
        print!("{}: ", item.0);
        for bit in item.1 {
            print!("{}", bit);
        }
        println!();
    }
}

fn generate_codes(node: &Node, prefix: Vec<u8>, out_codes: &mut HuffmanCodeMap) {
    match node.kind {
        NodeKind::Internal(ref left_child, ref right_child) => {
            let mut left_prefix = prefix.clone();
            left_prefix.push(0);
            generate_codes(&left_child, left_prefix, out_codes);

            let mut right_prefix = prefix;
            right_prefix.push(1);
            generate_codes(&right_child, right_prefix, out_codes);
        }
        NodeKind::Leaf(ch) => {
            out_codes.insert(ch, prefix);
        }
    }
}

```


Output:

```txt

 : 110
a: 1001
c: 101010
d: 10001
e: 1111
f: 1011
g: 101011
h: 0101
i: 1110
l: 01110
m: 0011
n: 000
o: 0010
p: 01000
r: 01001
s: 0110
t: 01111
u: 10100
x: 10000

```



## Scala

```scala
object Huffman {
  import scala.collection.mutable.{Map, PriorityQueue}

  sealed abstract class Tree
  case class Node(left: Tree, right: Tree) extends Tree
  case class Leaf(c: Char) extends Tree

  def treeOrdering(m: Map[Tree, Int]) = new Ordering[Tree] {
     def compare(x: Tree, y: Tree) = m(y).compare(m(x))
  }

  def stringMap(text: String) = text groupBy (x => Leaf(x) : Tree) mapValues (_.length)

  def buildNode(queue: PriorityQueue[Tree], map: Map[Tree,Int]) {
    val right = queue.dequeue
    val left = queue.dequeue
    val node = Node(left, right)
    map(node) = map(left) + map(right)
    queue.enqueue(node)
  }

  def codify(tree: Tree, map: Map[Tree, Int]) = {
    def recurse(tree: Tree, prefix: String): List[(Char, (Int, String))] = tree match {
      case Node(left, right) => recurse(left, prefix+"0") ::: recurse(right, prefix+"1")
      case leaf @ Leaf(c) => c -> ((map(leaf), prefix)) :: Nil
    }
    recurse(tree, "")
  }

  def encode(text: String) = {
    val map = Map.empty[Tree,Int] ++= stringMap(text)
    val queue = new PriorityQueue[Tree]()(treeOrdering(map)) ++= map.keysIterator

    while(queue.size > 1) {
      buildNode(queue, map)
    }
    codify(queue.dequeue, map)
  }


  def main(args: Array[String]) {
    val text = "this is an example for huffman encoding"
    val code = encode(text)
    println("Char\tWeight\t\tEncoding")
    code sortBy (_._2._1) foreach {
      case (c, (weight, encoding)) => println("%c:\t%3d/%-3d\t\t%s" format (c, weight, text.length, encoding))
    }
  }
}
```


```txt

Char    Weight          Encoding
t:        1/39          011000
p:        1/39          011001
r:        1/39          01101
c:        1/39          01110
x:        1/39          01111
g:        1/39          10110
l:        1/39          10111
u:        1/39          11000
d:        1/39          11001
o:        2/39          1010
s:        2/39          1101
m:        2/39          1110
h:        2/39          1111
f:        3/39          0000
a:        3/39          0001
e:        3/39          0010
i:        3/39          0011
n:        4/39          100
 :        6/39          010

```

==={{header|Scala (Alternate version)}}===
```scala

// this version uses immutable data only, recursive functions and pattern matching
object Huffman {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // recursively build the binary tree needed to Huffman encode the text
  def merge(xs: List[(Tree[Char], Int)]): List[(Tree[Char], Int)] = {
    if (xs.length == 1) xs else {
      val l = xs.head
      val r = xs.tail.head
      val merged = (Branch(l._1, r._1), l._2 + r._2)
      merge((merged :: xs.drop(2)).sortBy(_._2))
    }
  }

  // recursively search the branches of the tree for the required character
  def contains(tree: Tree[Char], char: Char): Boolean = tree match {
    case Leaf(c) => if (c == char) true else false
    case Branch(l, r) => contains(l, char) || contains(r, char)
  }

  // recursively build the path string required to traverse the tree to the required character
  def encodeChar(tree: Tree[Char], char: Char): String = {
    def go(tree: Tree[Char], char: Char, code: String): String = tree match {
      case Leaf(_) => code
      case Branch(l, r) => if (contains(l, char)) go(l, char, code + '0') else go(r, char, code + '1')
    }
    go(tree, char, "")
  }

  def main(args: Array[String]) {
    val text = "this is an example for huffman encoding"
    // transform the text into a list of tuples.
    // each tuple contains a Leaf node containing a unique character and an Int representing that character's weight
    val frequencies = text.groupBy(chars => chars).mapValues(group => group.length).toList.map(x => (Leaf(x._1), x._2)).sortBy(_._2)
    // build the Huffman Tree for this text
    val huffmanTree = merge(frequencies).head._1
    // output the resulting character codes
    println("Char\tWeight\tCode")
    frequencies.foreach(x => println(x._1.value + "\t" + x._2 + s"/${text.length}" + s"\t${encodeChar(huffmanTree, x._1.value)}"))
  }
}
```


```txt

Char    Weight  Code
x       1/39    01100
t       1/39    01101
u       1/39    00010
g       1/39    00011
l       1/39    00000
p       1/39    00001
c       1/39    100110
r       1/39    100111
d       1/39    10010
s       2/39    0111
m       2/39    0100
h       2/39    0101
o       2/39    1000
e       3/39    1100
f       3/39    1101
a       3/39    1010
i       3/39    1011
n       4/39    001
        6/39    111

```



## Scheme



```scheme
(define (char-freq port table)
  (if
   (eof-object? (peek-char port))
   table
   (char-freq port (add-char (read-char port) table))))

(define (add-char char table)
  (cond
   ((null? table) (list (list char 1)))
   ((eq? (caar table) char) (cons (list char (+ (cadar table) 1)) (cdr table)))
   (#t (cons (car table) (add-char char (cdr table))))))

(define (nodeify table)
  (map (lambda (x) (list x '() '())) table))

(define node-freq cadar)

(define (huffman-tree nodes)
  (let ((queue (sort nodes (lambda (x y) (< (node-freq x) (node-freq y))))))
    (if
     (null? (cdr queue))
     (car queue)
     (huffman-tree
      (cons
       (list
        (list 'notleaf (+ (node-freq (car queue)) (node-freq (cadr queue))))
        (car queue)
        (cadr queue))
       (cddr queue))))))

(define (list-encodings tree chars)
  (for-each (lambda (c) (format #t "~a:~a~%" c (encode c tree))) chars))

(define (encode char tree)
  (cond
   ((null? tree) #f)
   ((eq? (caar tree) char) '())
   (#t
    (let ((left (encode char (cadr tree))) (right (encode char (caddr tree))))
      (cond
       ((not (or left right)) #f)
       (left (cons #\1 left))
       (right (cons #\0 right)))))))

(define (decode digits tree)
  (cond
   ((not (eq? (caar tree) 'notleaf)) (caar tree))
   ((eq? (car digits) #\0) (decode (cdr digits) (cadr tree)))
   (#t (decode (cdr digits) (caddr tree)))))

(define input "this is an example for huffman encoding")
(define freq-table (char-freq (open-input-string input) '()))
(define tree (huffman-tree (nodeify freq-table)))
(list-encodings tree (map car freq-table))
```


```txt

t:(1 0 0 1 1)
h:(1 0 0 0)
i:(0 0 1 1)
s:(1 0 1 1)
 :(0 0 0)
a:(0 0 1 0)
n:(1 1 0)
e:(0 1 0 1)
x:(1 0 0 1 0)
m:(1 0 1 0)
p:(1 1 1 0 1)
l:(1 1 1 0 0)
f:(0 1 0 0)
o:(0 1 1 1)
r:(1 1 1 1 1)
u:(1 1 1 1 0)
c:(0 1 1 0 0 1)
d:(0 1 1 0 0 0)
g:(0 1 1 0 1)

```



## SETL


```SETL
var forest := {}, encTab := {};

plaintext := 'this is an example for huffman encoding';

ft := {};
(for c in plaintext)
  ft(c) +:= 1;
end;

forest := {[f, c]: [c, f] in ft};
(while 1 < #forest)
  [f1, n1] := getLFN();
  [f2, n2] := getLFN();
  forest with:= [f1+f2, [n1,n2]];
end;
addToTable('', arb range forest);

(for e = encTab(c))
  print(c, ft(c), e);
end;

print(+/ [encTab(c): c in plaintext]);

proc addToTable(prefix, node);
  if is_tuple node then
    addToTable(prefix + '0', node(1));
    addToTable(prefix + '1', node(2));
  else
    encTab(node) := prefix;
  end;
end proc;

proc getLFN();
  f := min/ domain forest;
  n := arb forest{f};
  forest less:= [f, n];
  return [f, n];
end proc;
```



## Sidef


```ruby
func walk(n, s, h) {
    if (n.contains(:a)) {
        h{n{:a}} = s
        say "#{n{:a}}: #{s}"
        return nil
    }
    walk(n{:0}, s+'0', h)
    walk(n{:1}, s+'1', h)
}

func make_tree(text) {
    var letters = Hash()
    text.each { |c| letters{c} := 0 ++ }
    var nodes = letters.keys.map { |l|
        Hash(a => l, freq => letters{l})
    }

    var n = Hash()
    while (nodes.sort_by!{|c| c{:freq} }.len > 1) {
        n = Hash(:0 => nodes.shift, :1 => nodes.shift)
        n{:freq} = (n{:0}{:freq} + n{:1}{:freq})
        nodes.append(n)
    }

    walk(n, "", n{:tree} = Hash())
    return n
}

func encode(s, t) {
    t = t{:tree}
    s.chars.map{|c| t{c} }.join
}

func decode (enc, tree) {
    var n = tree
    var out = ""

    enc.each {|bit|
        n = n{bit}
        if (n.contains(:a)) {
            out += n{:a}
            n = tree
        }
    }

    return out
}

var text = "this is an example for huffman encoding"
var tree = make_tree(text)
var enc = encode(text, tree)

say enc
say decode(enc, tree)
```

```txt
n: 000
s: 0010
o: 0011
h: 0100
l: 01010
g: 01011
x: 01100
c: 01101
d: 01110
u: 01111
p: 10000
t: 10001
i: 1001
 : 101
f: 1100
a: 1101
e: 1110
r: 11110
m: 11111
1000101001001001010110010010101110100010111100110011011111110000010101110101110000111111010101000111111001100111111101000101111000001101001101110100100001011
this is an example for huffman encoding
```



## Standard ML

```sml
datatype 'a huffman_tree =
         Leaf of 'a
       | Node of 'a huffman_tree * 'a huffman_tree

structure HuffmanPriority = struct
  type priority = int
(* reverse comparison to achieve min-heap *)
  fun compare (a, b) = Int.compare (b, a)
  type item = int * char huffman_tree
  val priority : item -> int = #1
end

structure HPQueue = LeftPriorityQFn (HuffmanPriority)

fun buildTree charFreqs = let
    fun aux trees = let
        val ((f1,a), trees) = HPQueue.remove trees
    in
        if HPQueue.isEmpty trees then
            a
        else let
                val ((f2,b), trees) = HPQueue.remove trees
                val trees = HPQueue.insert ((f1 + f2, Node (a, b)),
                                            trees)
            in
                aux trees
            end
    end
    val trees = HPQueue.fromList (map (fn (c,f) => (f, Leaf c)) charFreqs)
in
    aux trees
end

fun printCodes (revPrefix, Leaf c) =
    print (String.str c ^ "\t" ^
           implode (rev revPrefix) ^ "\n")
  | printCodes (revPrefix, Node (l, r)) = (
    printCodes (#"0"::revPrefix, l);
    printCodes (#"1"::revPrefix, r)
    );

let
    val test = "this is an example for huffman encoding"
    val charFreqs = HashTable.mkTable
                        (HashString.hashString o String.str, op=)
                        (42, Empty)
    val () =
        app (fn c =>
                let val old = getOpt (HashTable.find charFreqs c, 0)
                in HashTable.insert charFreqs (c, old+1)
                end)
            (explode test)
    val tree = buildTree (HashTable.listItemsi charFreqs)
in
    print "SYMBOL\tHUFFMAN CODE\n";
    printCodes ([], tree)
end
```



## Swift

Rather than a priority queue of subtrees, we use the strategy of two sorted lists, one for leaves and one for nodes, and "merge" them as we iterate through them, taking advantage of the fact that any new nodes we create are bigger than any previously created nodes, so go at the end of the nodes list.
```swift
enum HuffmanTree<T>
 {
  case Leaf(T)
  indirect case Node(HuffmanTree<T>, HuffmanTree<T>)

  func printCodes(prefix: String) {
    switch(self) {
    case let .Leaf(c):
      print("\(c)\t\(prefix)")
    case let .Node(l, r):
      l.printCodes(prefix + "0")
      r.printCodes(prefix + "1")
    }
  }
}

func buildTree<T>(freqs: [(T, Int)]) -> HuffmanTree<T> {
  assert(freqs.count > 0, "must contain at least one character")
  // leaves sorted by increasing frequency
  let leaves : [(Int, HuffmanTree<T>)] = freqs.sort { (p1, p2) in p1.1 < p2.1 }.map { (x, w) in (w, .Leaf(x)) }
  // nodes sorted by increasing frequency
  var nodes = [(Int, HuffmanTree<T>)]()
  // iterate through leaves and nodes in order of increasing frequency
  for var i = 0, j = 0; ; {
    assert(i < leaves.count || j < nodes.count)
    // get subtree of least frequency
    var e1 : (Int, HuffmanTree<T>)
    if j == nodes.count || i < leaves.count && leaves[i].0 < nodes[j].0 {
      e1 = leaves[i]
      i++
    } else {
      e1 = nodes[j]
      j++
    }

    // if there's no subtrees left, then that one was the answer
    if i == leaves.count && j == nodes.count {
      return e1.1
    }

    // get next subtree of least frequency
    var e2 : (Int, HuffmanTree<T>)
    if j == nodes.count || i < leaves.count && leaves[i].0 < nodes[j].0 {
      e2 = leaves[i]
      i++
    } else {
      e2 = nodes[j]
      j++
    }
    // create node from two subtrees
    nodes.append((e1.0 + e2.0, .Node(e1.1, e2.1)))
  }
}

func getFreqs<S : SequenceType where S.Generator.Element : Hashable>(seq: S) -> [(S.Generator.Element, Int)] {
  var freqs : [S.Generator.Element : Int] = [:]
  for c in seq {
    freqs[c] = (freqs[c] ?? 0) + 1
  }
  return Array(freqs)
}

let str = "this is an example for huffman encoding"
let charFreqs = getFreqs(str.characters)
let tree = buildTree(charFreqs)
print("Symbol\tHuffman code")
tree.printCodes("")
```

```txt

Symbol	Huffman code
u	00000
t	00001
d	00010
r	00011
c	00100
l	00101
o	0011
m	0100
s	0101
n	011
h	1000
g	10010
p	100110
x	100111
f	1010
a	1011
i	1100
e	1101
 	111

```



## Tcl

```tcl
package require Tcl 8.5
package require struct::prioqueue

proc huffmanEncode {str args} {
    array set opts [concat -dump false $args]

    set charcount [dict create]
    foreach char [split $str ""] {
        dict incr charcount $char
    }

    set pq [struct::prioqueue -dictionary] ;# want lower values to have higher priority
    dict for {char count} $charcount {
        $pq put $char $count
    }

    while {[$pq size] > 1} {
        lassign [$pq peekpriority 2] p1 p2
        $pq put [$pq get 2] [expr {$p1 + $p2}]
    }

    set encoding [walkTree [$pq get]]

    if {$opts(-dump)} {
        foreach {char huffCode} [lsort -index 1 -stride 2 -command compare $encoding] {
            puts "$char\t[dict get $charcount $char]\t$huffCode"
        }
    }
    $pq destroy

    return $encoding
}

proc walkTree {tree {prefix ""}} {
    if {[llength $tree] < 2} {
        return [list $tree $prefix]
    }
    lassign $tree left right
    return [concat [walkTree $left "${prefix}0"] [walkTree $right "${prefix}1"]]
}

proc compare {a b} {
    if {[string length $a] < [string length $b]} {return -1}
    if {[string length $a] > [string length $b]} {return  1}
    return [string compare $a $b]
}

set str "this is an example for huffman encoding"

set encoding [huffmanEncode $str -dump true]

puts $str
puts [string map $encoding $str]
```

<pre style='width:full; overflow:scroll'>n	4	000
 	6	101
s	2	0010
m	2	0011
o	2	0100
i	3	1001
a	3	1100
e	3	1101
f	3	1110
t	1	01010
x	1	01011
p	1	01100
l	1	01101
r	1	01110
u	1	01111
c	1	10000
d	1	10001
g	1	11110
h	2	11111
this is an example for huffman encoding
0101011111100100101011001001010111000001011101010111100001101100011011101101111001000111010111111011111110111000111100000101110100010000010010001100100011110
```



## Ursala

following the algorithm given above

```Ursala
#import std
#import nat
#import flo

code_table = # takes a training dataset to a table <char: code...>

-+
   *^ ~&v?\~&iNC @v ~&t?\~&h ~&plrDSLrnPlrmPCAS/'01',
   ~&itB->h fleq-<&d; ^C\~&tt @hthPX ^V\~&lrNCC plus@bd,
   ^V(div@rrPlX,~&rlNVNC)^*D(plus:-0.@rS,~&)+ *K2 ^/~&h float+ length+-

#cast %csAL

table = code_table 'this is an example for huffman encoding'
```

a quick walk through the code starting from the bottom:
* <code>*K2 ^/~&h float+ length</code> compute character frequencies by partitioning the input list of characters by equality, and transforming each equivalence class to a pair containing its member and its cardinality represented as a floating point number
* <code>^V(div@rrPlX,~&rlNVNC)^*D(plus:-0.@rS,~&)</code> construct a list of unary trees, one for each character class, with its normalized frequency in the root, and the character in the leaf
* <code>~&itB->h</code> while the list contains more than one tree, do the following, and when done take the head of the list
* <code>fleq-<&d;</code> sort the trees in increasing order by their roots
* <code>^C\~&tt @hthPX ^V\~&lrNCC plus@bd</code> change the first two trees in the sorted list to a single binary tree whose root is the sum of their roots
* <code>*^</code> visit the following function on each node of the tree obtained from the loop and propagate the results upward from the leaves
* <code>~&v?\~&iNC</code> if the node is a leaf, construct a singleton list containing the pair of its root (a character) and the empty string (of bits)
* <code>@v ~&t?\~&h</code> if there is only a single subtree, propagate the result already obtained for it
* <code>~&plrDSLrnPlrmPCAS/'01'</code> otherwise there are two subtrees, hence two lists previously computed results propagating upward, so insert a zero into all of the bit strings in the results on the left, and a one into all the ones on the right, concatenate the left and right results, and propagate the contatenation upward

```txt
<
   `r: '00000',
   `l: '00001',
   `c: '00010',
   `u: '00011',
   `n: '001',
   `m: '0100',
   `h: '0101',
   `g: '01100',
   `d: '01101',
   `o: '0111',
   `s: '1000',
   `t: '10010',
   `p: '100110',
   `x: '100111',
   `a: '1010',
   `f: '1011',
   `i: '1100',
   `e: '1101',
   ` : '111'>
```



## zkl

This code was adapted from Perl, Python and most of the other examples.

```zkl
fcn buildHuffman(text){  //-->(encode dictionary, decode dictionary)
   ft:=Dictionary();
   foreach c in (text){ ft[c]=ft.find(c,0)+1 }  // leafs w/count

   // build the tree, which is a list of lists of ...
   tree:=ft.pump(List,fcn([(c,cnt)]){  //-->L( (cnt, ((sym,code))), ...)
      L(cnt, L(L(c,"")))
   }).copy();	// make it writable

   while(tree.len()>1){  // fake up a [lame] priorty queue
      tree=tree.sort(fcn(a,b){ a[0]>b[0] }); //prioritize high to low
      a,b:=tree.pop(-2,2);	        //remove 2 least frequent symbols
      mc:=fcn(n,c){ n[1] = c + n[1]; };	//(sym,code),"0"|"1"
      a[1].apply2(mc,"0"); b[1].apply2(mc,"1"); // mc(a[1],"0")
      tree.append( L(a[0]+b[0],a[1].extend(b[1])) ); //(a,b)-->new node
   }//-->L(L(39, L( L(" ","000"),L("e","0010"),L("a","0011") ...

   tree=tree[0][1].pump(List,fcn(i){ // flatten rather than traverse
	if(T.isType(i))return(Void.Recurse,i,self.fcn); i });
   encodeTable:=tree.toDictionary();  // symbol:Huffman code
   decodeTable:=encodeTable.pump(Dictionary(),"reverse"); // code:symbol
   return(encodeTable,decodeTable);
}
```


```zkl
fcn encode(text,table){ text.pump(String,table.get) }
fcn decode(bits,table){  // this is a horrible decoder, for testing only
   w:=bits.walker(); sink:=Sink(String);
   try{ s:=""; while(1){
      s+=w.next(); if(c:=table.find(s)) { sink.write(c); s=""; }
   }}catch(TheEnd){}
   sink.close();
}
```


```zkl
text:="this is an example for huffman encoding";
encodeTable,decodeTable := buildHuffman(text);
encodeTable.pump(Console.println,fcn(kv){"%s : %s".fmt(kv.xplode())});

e:=encode(text,encodeTable);
"Encode %d characters (%d bits) to %d bits (%d bytes):"
   .fmt(text.len(),text.len()*8,e.len(),(e.len()+7)/8).println();
println(e);

0'|Bits decoded to: "%s"|.fmt(decode(e,decodeTable)).println();
```

```txt

a : 0011
c : 10101
d : 10100
e : 0010
f : 0110
g : 10111
h : 1000
i : 0101
l : 10110
m : 1001
n : 110
o : 01000
p : 11111
r : 11100
s : 0111
t : 01001
u : 11101
x : 11110
  : 000
Encode 39 characters (312 bits) to 157 bits (20 bytes):
0100110000101011100001010111000001111000000101111000111001111111011000100000110010001110000010001110101100110100100111100000010110101010100010100010111010111
Bits decoded to: "this is an example for huffman encoding"

```



