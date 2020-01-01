+++
title = "Tree traversal"
description = ""
date = 2019-10-20T12:47:25Z
aliases = []
[extra]
id = 4486
[taxonomies]
categories = []
tags = []
+++

{{task|Data Structures}}
[[Category:Recursion]]

;Task:
Implement a binary tree where each node carries an integer,   and implement:
:::*   pre-order,
:::*   in-order,
:::*   post-order,     and
:::*   level-order   [[wp:Tree traversal|traversal]].


Use those traversals to output the following tree:
          1
         / \
        /   \
       /     \
      2       3
     / \     /
    4   5   6
   /       / \
  7       8   9

The correct output should look like this:
 preorder:    1 2 4 7 5 3 6 8 9
 inorder:     7 4 2 5 1 8 6 9 3
 postorder:   7 4 5 2 8 9 6 3 1
 level-order: 1 2 3 4 5 6 7 8 9


;See also:
*   Wikipedia article:   [[wp:Tree traversal|Tree traversal]].





## ACL2


```lisp
(defun flatten-preorder (tree)
   (if (endp tree)
       nil
       (append (list (first tree))
               (flatten-preorder (second tree))
               (flatten-preorder (third tree)))))

(defun flatten-inorder (tree)
   (if (endp tree)
       nil
       (append (flatten-inorder (second tree))
               (list (first tree))
               (flatten-inorder (third tree)))))

(defun flatten-postorder (tree)
   (if (endp tree)
       nil
       (append (flatten-postorder (second tree))
               (flatten-postorder (third tree))
               (list (first tree)))))

(defun flatten-level-r1 (tree level levels)
   (if (endp tree)
       levels
       (let ((curr (cdr (assoc level levels))))
            (flatten-level-r1
             (second tree)
             (1+ level)
             (flatten-level-r1
              (third tree)
              (1+ level)
              (put-assoc level
                         (append curr (list (first tree)))
                         levels))))))

(defun flatten-level-r2 (levels max-level)
   (declare (xargs :measure (nfix (1+ max-level))))
   (if (zp (1+ max-level))
       nil
       (append (flatten-level-r2 levels
                                 (1- max-level))
               (reverse (cdr (assoc max-level levels))))))


(defun flatten-level (tree)
   (let ((levels (flatten-level-r1 tree 0 nil)))
      (flatten-level-r2 levels (len levels))))
```



## Ada


```Ada
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Doubly_Linked_Lists;

procedure Tree_Traversal is
   type Node;
   type Node_Access is access Node;
   type Node is record
      Left : Node_Access := null;
      Right : Node_Access := null;
      Data : Integer;
   end record;
   procedure Destroy_Tree(N : in out Node_Access) is
      procedure free is new Ada.Unchecked_Deallocation(Node, Node_Access);
   begin
      if N.Left /= null then
         Destroy_Tree(N.Left);
      end if;
      if N.Right /= null then
         Destroy_Tree(N.Right);
      end if;
      Free(N);
   end Destroy_Tree;
   function Tree(Value : Integer; Left : Node_Access; Right : Node_Access) return Node_Access is
      Temp : Node_Access := new Node;
   begin
      Temp.Data := Value;
      Temp.Left := Left;
      Temp.Right := Right;
      return Temp;
   end Tree;
   procedure Preorder(N : Node_Access) is
   begin
      Put(Integer'Image(N.Data));
      if N.Left /= null then
         Preorder(N.Left);
      end if;
      if N.Right /= null then
         Preorder(N.Right);
      end if;
   end Preorder;
   procedure Inorder(N : Node_Access) is
   begin
      if N.Left /= null then
         Inorder(N.Left);
      end if;
      Put(Integer'Image(N.Data));
      if N.Right /= null then
         Inorder(N.Right);
      end if;
   end Inorder;
   procedure Postorder(N : Node_Access) is
   begin
      if N.Left /= null then
         Postorder(N.Left);
      end if;
      if N.Right /= null then
         Postorder(N.Right);
      end if;
      Put(Integer'Image(N.Data));
   end Postorder;
   procedure Levelorder(N : Node_Access) is
      package Queues is new Ada.Containers.Doubly_Linked_Lists(Node_Access);
      use Queues;
      Node_Queue : List;
      Next : Node_Access;
   begin
      Node_Queue.Append(N);
      while not Is_Empty(Node_Queue) loop
         Next := First_Element(Node_Queue);
         Delete_First(Node_Queue);
         Put(Integer'Image(Next.Data));
         if Next.Left /= null then
            Node_Queue.Append(Next.Left);
         end if;
         if Next.Right /= null then
            Node_Queue.Append(Next.Right);
         end if;
      end loop;
   end Levelorder;
   N : Node_Access;
begin
   N := Tree(1,
      Tree(2,
         Tree(4,
            Tree(7, null, null),
            null),
         Tree(5, null, null)),
      Tree(3,
         Tree(6,
            Tree(8, null, null),
            Tree(9, null, null)),
         null));

   Put("preorder:    ");
   Preorder(N);
   New_Line;
   Put("inorder:     ");
   Inorder(N);
   New_Line;
   Put("postorder:   ");
   Postorder(N);
   New_Line;
   Put("level order: ");
   Levelorder(N);
   New_Line;
   Destroy_Tree(N);
end Tree_traversal;
```



## ALGOL 68

{{trans|C}} - note the strong code structural similarities with C.

Note the changes from the original translation from C in this [http://rosettacode.org/mw/index.php?title=Tree_traversal&diff=78718&oldid=78682 diff].  It contains examples of syntactic sugar available in [[ALGOL 68]].

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards)}}

```algol68
MODE VALUE = INT;
PROC value repr = (VALUE value)STRING: whole(value, 0);

MODE NODES = STRUCT ( VALUE value, REF NODES left, right);
MODE NODE = REF NODES;

PROC tree = (VALUE value, NODE left, right)NODE:
  HEAP NODES := (value, left, right);

PROC preorder = (NODE node, PROC (VALUE)VOID action)VOID:
  IF node ISNT NODE(NIL) THEN
    action(value OF node);
    preorder(left OF node, action);
    preorder(right OF node, action)
  FI;

PROC inorder = (NODE node, PROC (VALUE)VOID action)VOID:
  IF node ISNT NODE(NIL) THEN
    inorder(left OF node, action);
    action(value OF node);
    inorder(right OF node, action)
  FI;

PROC postorder = (NODE node, PROC (VALUE)VOID action)VOID:
  IF node ISNT NODE(NIL) THEN
    postorder(left OF node, action);
    postorder(right OF node, action);
    action(value OF node)
  FI;

PROC destroy tree = (NODE node)VOID:
  postorder(node, (VALUE skip)VOID:
  # free(node) - PR garbage collect hint PR #
    node := (SKIP, NIL, NIL)
  );

# helper queue for level order #
MODE QNODES = STRUCT (REF QNODES next, NODE value);
MODE QNODE = REF QNODES;


MODE QUEUES = STRUCT (QNODE begin, end);
MODE QUEUE = REF QUEUES;

PROC enqueue = (QUEUE queue, NODE node)VOID:
(
  HEAP QNODES qnode := (NIL, node);
  IF end OF queue ISNT QNODE(NIL) THEN
    next OF end OF queue
  ELSE
    begin OF queue
  FI := end OF queue := qnode
);

PROC queue empty = (QUEUE queue)BOOL:
  begin OF queue IS QNODE(NIL);

PROC dequeue = (QUEUE queue)NODE:
(
  NODE out := value OF begin OF queue;
  QNODE second := next OF begin OF queue;
# free(begin OF queue); PR garbage collect hint PR #
  QNODE(begin OF queue) := (NIL, NIL);
  begin OF queue := second;
  IF queue empty(queue) THEN
    end OF queue := begin OF queue
  FI;
  out
);

PROC level order = (NODE node, PROC (VALUE)VOID action)VOID:
(
  HEAP QUEUES queue := (QNODE(NIL), QNODE(NIL));
  enqueue(queue, node);
  WHILE NOT queue empty(queue)
  DO
    NODE next := dequeue(queue);
    IF next ISNT NODE(NIL) THEN
      action(value OF next);
      enqueue(queue, left OF next);
      enqueue(queue, right OF next)
    FI
  OD
);

PROC print node = (VALUE value)VOID:
  print((" ",value repr(value)));

main: (
  NODE node := tree(1,
                tree(2,
                     tree(4,
                          tree(7, NIL, NIL),
                          NIL),
                     tree(5, NIL, NIL)),
                tree(3,
                     tree(6,
                          tree(8, NIL, NIL),
                          tree(9, NIL, NIL)),
                     NIL));

  MODE TEST = STRUCT(
    STRING name,
    PROC(NODE,PROC(VALUE)VOID)VOID order
  );

  PROC test = (TEST test)VOID:(
    STRING pad=" "*(12-UPB name OF test);
    print((name OF test,pad,": "));
    (order OF test)(node, print node);
    print(new line)
  );

  []TEST test list = (
    ("preorder",preorder),
    ("inorder",inorder),
    ("postorder",postorder),
    ("level order",level order)
  );

  FOR i TO UPB test list DO test(test list[i]) OD;

  destroy tree(node)
)
```

Output:

```txt

preorder :     1 2 4 7 5 3 6 8 9
inorder :      7 4 2 5 1 8 6 9 3
postorder :    7 4 5 2 8 9 6 3 1
level-order :  1 2 3 4 5 6 7 8 9

```




## APL


Written in Dyalog APL with dfns.

```APL
preorder ← {l r←⍺ ⍵⍵ ⍵ ⋄ (⊃r)∇⍨⍣(×≢r)⊢(⊃l)∇⍨⍣(×≢l)⊢⍺ ⍺⍺ ⍵}
inorder  ← {l r←⍺ ⍵⍵ ⍵ ⋄ (⊃r)∇⍨⍣(×≢r)⊢⍵ ⍺⍺⍨(⊃l)∇⍨⍣(×≢l)⊢⍺}
postorder← {l r←⍺ ⍵⍵ ⍵ ⋄ ⍵ ⍺⍺⍨(⊃r)∇⍨⍣(×≢r)⊢(⊃l)∇⍨⍣(×≢l)⊢⍺}
lvlorder ← {0=⍴⍵:⍺ ⋄ (⊃⍺⍺⍨/(⌽⍵),⊂⍺)∇⊃∘(,/)⍣2⊢⍺∘⍵⍵¨⍵}
```

These accept four arguments (they are operators, a.k.a. higher-order functions):

```txt
acc visit ___order children bintree
```

returns the accumulator after visiting each node in the order specified by the function.

"acc" is the initial value for the accumulator, and "bintree" is usually the tree to be searched (it is actually the the initial argument fed to visit and children, which in most cases corresponds to the root node and the rest of the tree).

"visit" and "children" are two functions which allow these operators to work on any representation of a tree you can cook up.

"visit" takes the accumulator on the left and the current node data on the right, and returns the modified accumulator (it visits the node).

"children" generates the children of the current node from the current node's data on the right, and the current state of the accumulator on the left if needed.

"pre-", "in-", and "postorder" all work in the same way. First "children" returns the left and right children in "l" and "r", both in a "wrapper" (sort of like the Maybe type in Haskell from the little I know of it). Then the whole function is recursively applied to the left and right children if they're there, and visit is run on the current node. The order of those three operations is what differs in the three operators. Therefor if the current node possesses neither child, then the recursion ends for that branch.

"lvlorder" is a little different. The right argument is actually a list of initial nodes considered at the top level (usually this will just be a list of one element which is the tree). First all the nodes in this list are visited, then the children of each of these nodes are generated and assembled into a single list. The accumulator and this list are passed to the same function recursively, until the list of children nodes to visit is empty. This function is tail-recursive.


Time for an example to clarify all this.

I chose to represent the description's tree using nested arrays (rectangular arrays whose elements can also be rectangular arrays). Each node is of the form

```txt
 value childL childR
```

and empty childL or childR mean and absence of the corresponding child node.


```APL
tree←1(2(4(7⍬⍬)⍬)(5⍬⍬))(3(6(8⍬⍬)(9⍬⍬))⍬)
visit←{⍺,(×≢⍵)⍴⊃⍵}
children←{⊂¨@(×∘≢¨)1↓⍵}
```

Each time the accumulator is initialised as an empty list. Visiting a node means to append its data to the accumulator, and generating children is fetching the two corresponding sublists in the nested array if they're non-empty.

My input into the interactive APL session is indented by 6 spaces.

```txt

      ⍬ visit preorder  children   tree
1 2 4 7 5 3 6 8 9
      ⍬ visit inorder   children   tree
7 4 2 5 1 8 6 9 3
      ⍬ visit postorder children   tree
7 4 5 2 8 9 6 3 1
      ⍬ visit lvlorder  children ,⊂tree
1 2 3 4 5 6 7 8 9

```


These solutions were inspired by the DFS lesson on www.TryApl.org

You should go check it out, as in the lesson it is explained how to implement a DFS operator taking the same two functions as the operators here. What is remarkable is that these same searching operators can be used both on an actual tree data structure, and on an "imaginary" one as well such as the tree of solutions to the N-Queens problem. This is the example used on TryApl.org.


## AppleScript

{{Trans|JavaScript}}(ES6)

```AppleScript
on run
    set tree to {1, {2, {4, {7}, {}}, {5}}, {3, {6, {8}, {9}}, {}}}

    -- asciiTree :: String
    set asciiTree to ¬
        unlines({¬
            "         1", ¬
            "        / \\", ¬
            "       /   \\", ¬
            "      /     \\", ¬
            "     2       3", ¬
            "    / \\     /", ¬
            "   4   5   6", ¬
            "  /       / \\", ¬
            " 7       8   9"})

    script tabulate
        on |λ|(s, xs)
            justifyLeft(14, space, s & ":") & unwords(xs)
        end |λ|
    end script

    set strResult to asciiTree & linefeed & linefeed & ¬
        unlines(zipWith(tabulate, ¬
            ["preorder", "inorder", "postorder", "level-order"], ¬
            ap([preorder, inorder, postorder, levelOrder], [tree])))

    set the clipboard to strResult
    return strResult
end run

-- TRAVERSAL FUNCTIONS --------------------------------------------------------

-- preorder :: Tree Int -> [Int]
on preorder(tree)
    set {v, l, r} to nodeParts(tree)
    if l is {} then
        set lstLeft to []
    else
        set lstLeft to preorder(l)
    end if

    if r is {} then
        set lstRight to []
    else
        set lstRight to preorder(r)
    end if
    v & lstLeft & lstRight
end preorder

-- inorder :: Tree Int -> [Int]
on inorder(tree)
    set {v, l, r} to nodeParts(tree)
    if l is {} then
        set lstLeft to []
    else
        set lstLeft to inorder(l)
    end if

    if r is {} then
        set lstRight to []
    else
        set lstRight to inorder(r)
    end if

    lstLeft & v & lstRight
end inorder

-- postorder :: Tree Int -> [Int]
on postorder(tree)
    set {v, l, r} to nodeParts(tree)
    if l is {} then
        set lstLeft to []
    else
        set lstLeft to postorder(l)
    end if

    if r is {} then
        set lstRight to []
    else
        set lstRight to postorder(r)
    end if
    lstLeft & lstRight & v
end postorder

-- levelOrder :: Tree Int -> [Int]
on levelOrder(tree)
    if length of tree > 0 then
        set {head, tail} to uncons(tree)

        -- Take any value found in the head node
        -- deferring any child nodes to the end of the tail
        -- before recursing

        if head is not {} then
            set {v, l, r} to nodeParts(head)
            v & levelOrder(tail & {l, r})
        else
            levelOrder(tail)
        end if
    else
        {}
    end if
end levelOrder

-- nodeParts :: Tree -> (Int, Tree, Tree)
on nodeParts(tree)
    if class of tree is list and length of tree = 3 then
        tree
    else
        {tree} & {{}, {}}
    end if
end nodeParts


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- A list of functions applied to a list of arguments
-- (<*> | ap) :: [(a -> b)] -> [a] -> [b]
on ap(fs, xs)
    set lngFs to length of fs
    set lngXs to length of xs
    set lst to {}
    repeat with i from 1 to lngFs
        tell mReturn(contents of item i of fs)
            repeat with j from 1 to lngXs
                set end of lst to |λ|(contents of (item j of xs))
            end repeat
        end tell
    end repeat
    return lst
end ap

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- justifyLeft :: Int -> Char -> Text -> Text
on justifyLeft(n, cFiller, strText)
    if n > length of strText then
        text 1 thru n of (strText & replicate(n, cFiller))
    else
        strText
    end if
end justifyLeft

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- uncons :: [a] -> Maybe (a, [a])
on uncons(xs)
    if length of xs > 0 then
        {item 1 of xs, rest of xs}
    else
        missing value
    end if
end uncons

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines

-- unwords :: [String] -> String
on unwords(xs)
    intercalate(space, xs)
end unwords

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```txt
         1
        / \
       /   \
      /     \
     2       3
    / \     /
   4   5   6
  /       / \
 7       8   9

preorder:     1 2 4 7 5 3 6 8 9
inorder:      7 4 2 5 1 8 6 9 3
postorder:    7 4 5 2 8 9 6 3 1
level-order:  1 2 3 4 5 6 7 8 9
```



## ARM Assembly

{{works with|as|Raspberry Pi}}

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program deftree2.s   */

/* Constantes    */
.equ STDOUT, 1                           @ Linux output console
.equ EXIT,   1                           @ Linux syscall
.equ READ,   3
.equ WRITE,  4

.equ NBVAL,    9

/*******************************************/
/* Structures                               */
/********************************************/
/* structure tree     */
    .struct  0
tree_root:                             @ root pointer
    .struct  tree_root + 4
tree_size:                             @ number of element of tree
    .struct  tree_size + 4
tree_fin:
/* structure node tree */
    .struct  0
node_left:                             @ left pointer
    .struct  node_left + 4
node_right:                            @ right pointer
    .struct  node_right + 4
node_value:                            @ element value
    .struct  node_value + 4
node_fin:
/* structure queue*/
    .struct  0
queue_begin:                           @ next pointer
    .struct  queue_begin + 4
queue_end:                             @ element value
    .struct  queue_end + 4
queue_fin:
/* structure node queue    */
    .struct  0
queue_node_next:                       @ next pointer
    .struct  queue_node_next + 4
queue_node_value:                      @ element value
    .struct  queue_node_value + 4
queue_node_fin:
/* Initialized data */
.data
szMessInOrder:        .asciz "inOrder :\n"
szMessPreOrder:       .asciz "PreOrder :\n"
szMessPostOrder:      .asciz "PostOrder :\n"
szMessLevelOrder:     .asciz "LevelOrder :\n"
szCarriageReturn:     .asciz "\n"
/* datas error display */
szMessErreur:         .asciz "Error detected.\n"
/* datas message display */
szMessResult:         .ascii "Element value :"
sValue:               .space 12,' '
                      .asciz "\n"

/* UnInitialized data */
.bss
stTree:               .skip tree_fin    @ place to structure tree
stQueue:              .skip queue_fin   @ place to structure queue
/*  code section */
.text
.global main
main:
    mov r1,#1                           @ node tree value
1:
    ldr r0,iAdrstTree                   @ structure tree address
    bl insertElement                    @ add element value r1
    cmp r0,#-1
    beq 99f
    add r1,#1                           @ increment value
    cmp r1,#NBVAL                       @ end ?
    ble 1b                              @ no -> loop

    ldr r0,iAdrszMessPreOrder
    bl affichageMess
    ldr r3,iAdrstTree                   @ tree root address (begin structure)
    ldr r0,[r3,#tree_root]
    ldr r1,iAdrdisplayElement           @ function to execute
    bl preOrder

    ldr r0,iAdrszMessInOrder
    bl affichageMess
    ldr r3,iAdrstTree
    ldr r0,[r3,#tree_root]
    ldr r1,iAdrdisplayElement           @ function to execute
    bl inOrder

    ldr r0,iAdrszMessPostOrder
    bl affichageMess
    ldr r3,iAdrstTree
    ldr r0,[r3,#tree_root]
    ldr r1,iAdrdisplayElement           @ function to execute
    bl postOrder

    ldr r0,iAdrszMessLevelOrder
    bl affichageMess
    ldr r3,iAdrstTree
    ldr r0,[r3,#tree_root]
    ldr r1,iAdrdisplayElement           @ function to execute
    bl levelOrder
    b 100f
99:                                     @ display error
    ldr r0,iAdrszMessErreur
    bl affichageMess
100:                                    @ standard end of the program
    mov r7, #EXIT                       @ request to exit program
    svc 0                               @ perform system call
iAdrszMessInOrder:         .int szMessInOrder
iAdrszMessPreOrder:        .int szMessPreOrder
iAdrszMessPostOrder:       .int szMessPostOrder
iAdrszMessLevelOrder:      .int szMessLevelOrder
iAdrszMessErreur:          .int szMessErreur
iAdrszCarriageReturn:      .int szCarriageReturn
iAdrstTree:                .int stTree
iAdrstQueue:               .int stQueue
iAdrdisplayElement:        .int displayElement
/******************************************************************/
/*     insert element in the tree                                 */
/******************************************************************/
/* r0 contains the address of the tree structure */
/* r1 contains the value of element              */
/* r0 returns address of element or - 1 if error */
insertElement:
    push {r1-r7,lr}                   @ save  registers
    mov r4,r0
    mov r0,#node_fin                  @ reservation place one element
    bl allocHeap
    cmp r0,#-1                        @ allocation error
    beq 100f
    mov r5,r0
    str r1,[r5,#node_value]           @ store value in address heap
    mov r1,#0
    str r1,[r5,#node_left]            @ init left pointer with zero
    str r1,[r5,#node_right]           @ init right pointer with zero
    ldr r2,[r4,#tree_size]            @ load tree size
    cmp r2,#0                         @ 0 element ?
    bne 1f
    str r5,[r4,#tree_root]            @ yes -> store in root
    b 4f
1:                                    @ else search free address in tree
    ldr r3,[r4,#tree_root]            @ start with address root
    add r6,r2,#1                      @ increment tree size
    clz r7,r6                         @ compute zeroes left bits
    add r7,#1                         @ for sustract the first left bit
    lsl r6,r7                         @ shift number in left
2:
    lsls r6,#1                        @ read left bit
    bcs 3f                            @ is 1 ?
    ldr r1,[r3,#node_left]            @ no store node address in left pointer
    cmp r1,#0                         @ if equal zero
    streq r5,[r3,#node_left]
    beq 4f
    mov r3,r1                         @ else loop with next node
    b 2b
3:                                    @ yes
    ldr r1,[r3,#node_right]           @ store node address in right pointer
    cmp r1,#0                         @ if equal zero
    streq r5,[r3,#node_right]
    beq 4f
    mov r3,r1                         @ else loop with next node
    b 2b
4:
    add r2,#1                         @ increment tree size
    str r2,[r4,#tree_size]
100:
    pop {r1-r7,lr}                    @ restaur registers
    bx lr                             @ return
/******************************************************************/
/*     preOrder                                  */
/******************************************************************/
/* r0 contains the address of the node */
/* r1 function address                 */
preOrder:
    push {r1-r2,lr}                       @ save  registers
    cmp r0,#0
    beq 100f
    mov r2,r0
    blx r1                                @ call function

    ldr r0,[r2,#node_left]
    bl preOrder
    ldr r0,[r2,#node_right]
    bl preOrder
100:
    pop {r1-r2,lr}                        @ restaur registers
    bx lr
/******************************************************************/
/*     inOrder                                  */
/******************************************************************/
/* r0 contains the address of the node */
/* r1 function address                 */
inOrder:
    push {r1-r3,lr}                    @ save  registers
    cmp r0,#0
    beq 100f
    mov r3,r0
    mov r2,r1
    ldr r0,[r3,#node_left]
    bl inOrder
    mov r0,r3
    blx r2                             @ call function

    ldr r0,[r3,#node_right]
    mov r1,r2
    bl inOrder
100:
    pop {r1-r3,lr}                     @ restaur registers
    bx lr                              @ return
/******************************************************************/
/*     postOrder                                  */
/******************************************************************/
/* r0 contains the address of the node */
/* r1 function address                 */
postOrder:
    push {r1-r3,lr}                    @ save  registers
    cmp r0,#0
    beq 100f
    mov r3,r0
    mov r2,r1
    ldr r0,[r3,#node_left]
    bl postOrder

    ldr r0,[r3,#node_right]
    mov r1,r2
    bl postOrder
    mov r0,r3
    blx r2                            @ call function
100:
    pop {r1-r3,lr}                    @ restaur registers
    bx lr                             @ return
/******************************************************************/
/*     levelOrder                                  */
/******************************************************************/
/* r0 contains the address of the node */
/* r1 function address                 */
levelOrder:
    push {r1-r4,lr}                       @ save  registers
    cmp r0,#0
    beq 100f
    mov r2,r1
    mov r1,r0
    ldr r0,iAdrstQueue                    @ adresse queue
    bl enqueueNode                        @ queue the node
1:                                        @ begin loop
    ldr r0,iAdrstQueue
    bl isEmptyQueue                       @ is queue empty
    cmp r0,#0
    beq 100f                              @ yes -> end
    ldr r0,iAdrstQueue
    bl dequeueNode
    mov r3,r0                             @ save node
    blx r2                                @ call function
    ldr r4,[r3,#node_left]                @ left node ok ?
    cmp r4,#0
    beq 2f                                @ no
    ldr r0,iAdrstQueue                    @ yes -> enqueue
    mov r1,r4
    bl enqueueNode
2:
    ldr r4,[r3,#node_right]               @ right node ok ?
    cmp r4,#0
    beq 3f                                @ no
    ldr r0,iAdrstQueue                    @ yes -> enqueue
    mov r1,r4
    bl enqueueNode
3:
    b 1b                                  @ and loop

100:
    pop {r1-r4,lr}                        @ restaur registers
    bx lr                                 @ return
/******************************************************************/
/*     display node                                               */
/******************************************************************/
/* r0 contains node  address          */
displayElement:
    push {r1,lr}                       @ save  registers
    ldr r0,[r0,#node_value]
    ldr r1,iAdrsValue
    bl conversion10S
    ldr r0,iAdrszMessResult
    bl affichageMess
100:
    pop {r1,lr}                        @ restaur registers
    bx lr                              @ return
iAdrszMessResult:          .int szMessResult
iAdrsValue:                .int sValue
/******************************************************************/
/*     enqueue node                                  */
/******************************************************************/
/* r0 contains the address of the queue */
/* r1 contains the value of element  */
/* r0 returns address of element or - 1 if error */
enqueueNode:
    push {r1-r5,lr}                       @ save  registers
    mov r4,r0
    mov r0,#queue_node_fin                @ allocation place heap
    bl allocHeap
    cmp r0,#-1                            @ allocation error
    beq 100f
    mov r5,r0                             @ save heap address
    str r1,[r5,#queue_node_value]         @ store node value
    mov r1,#0
    str r1,[r5,#queue_node_next]          @ init pointer next
    ldr r0,[r4,#queue_end]
    cmp r0,#0
    strne r5,[r0,#queue_node_next]
    streq r5,[r4,#queue_begin]
    str r5,[r4,#queue_end]
    mov r0,#0
    pop {r1-r5,lr}
    bx lr                             @ return
/******************************************************************/
/*     dequeue node                                  */
/******************************************************************/
/* r0 contains the address of the queue */
/* r0 returns address of element or - 1 if error */
dequeueNode:
    push {r1-r5,lr}                       @ save  registers
    ldr r4,[r0,#queue_begin]
    ldr r5,[r4,#queue_node_value]
    ldr r6,[r4,#queue_node_next]
    str r6,[r0,#queue_begin]
    cmp r6,#0
    streq r6,[r0,#queue_end]
    mov r0,r5
100:
    pop {r1-r5,lr}
    bx lr                             @ return
/******************************************************************/
/*     dequeue node                                  */
/******************************************************************/
/* r0 contains the address of the queue */
/* r0 returns 0 if empty else 1  */
isEmptyQueue:
    ldr r0,[r0,#queue_begin]
    cmp r0,#0
    movne r0,#1
    bx lr                             @ return
/******************************************************************/
/*     memory allocation on the heap                                  */
/******************************************************************/
/* r0 contains the size to allocate */
/* r0 returns address of memory heap or - 1 if error */
/* CAUTION : The size of the allowance must be a multiple of 4  */
allocHeap:
    push {r5-r7,lr}                   @ save  registers
    @ allocation
    mov r6,r0                         @ save size
    mov r0,#0                         @ read address start heap
    mov r7,#0x2D                      @ call system 'brk'
    svc #0
    mov r5,r0                         @ save address heap for return
    add r0,r6                         @ reservation place for size
    mov r7,#0x2D                      @ call system 'brk'
    svc #0
    cmp r0,#-1                        @ allocation error
    movne r0,r5                       @ return address memory heap
    pop {r5-r7,lr}                    @ restaur registers
    bx lr                             @ return
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                       @ save  registers
    mov r2,#0                                   @ counter length */
1:                                              @ loop length calculation
    ldrb r1,[r0,r2]                             @ read octet start position + index
    cmp r1,#0                                   @ if 0 its over
    addne r2,r2,#1                              @ else add 1 in the length
    bne 1b                                      @ and loop
                                                @ so here r2 contains the length of the message
    mov r1,r0                                   @ address message in r1
    mov r0,#STDOUT                              @ code to write to the standard output Linux
    mov r7, #WRITE                              @ code call system "write"
    svc #0                                      @ call system
    pop {r0,r1,r2,r7,lr}                        @ restaur registers
    bx lr                                       @ return
/***************************************************/
/*  Converting a register to a signed decimal      */
/***************************************************/
/* r0 contains value and r1 area address    */
conversion10S:
    push {r0-r4,lr}       @ save registers
    mov r2,r1             @ debut zone stockage
    mov r3,#'+'           @ par defaut le signe est +
    cmp r0,#0             @ negative number ?
    movlt r3,#'-'         @ yes
    mvnlt r0,r0           @ number inversion
    addlt r0,#1
    mov r4,#10            @ length area
1:                        @ start loop
    bl divisionpar10U
    add r1,#48            @ digit
    strb r1,[r2,r4]       @ store digit on area
    sub r4,r4,#1          @ previous position
    cmp r0,#0             @ stop if quotient = 0
    bne 1b

    strb r3,[r2,r4]       @ store signe
    subs r4,r4,#1         @ previous position
    blt  100f             @ if r4 < 0 -> end

    mov r1,#' '           @ space
2:
    strb r1,[r2,r4]       @store byte space
    subs r4,r4,#1         @ previous position
    bge 2b                @ loop if r4 > 0
100:
    pop {r0-r4,lr}        @ restaur registers
    bx lr
/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient    */
/* r1 remainder   */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    //mov r3,#0xCCCD                                   @ r3 <- magic_number lower  raspberry 3
    //movt r3,#0xCCCC                                  @ r3 <- magic_number higter raspberry 3
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function
iMagicNumber:  	.int 0xCCCCCCCD


```

{{output}}

```txt

PreOrder :
Element value :         +1
Element value :         +2
Element value :         +4
Element value :         +8
Element value :         +9
Element value :         +5
Element value :         +3
Element value :         +6
Element value :         +7
inOrder :
Element value :         +8
Element value :         +4
Element value :         +9
Element value :         +2
Element value :         +5
Element value :         +1
Element value :         +6
Element value :         +3
Element value :         +7
PostOrder :
Element value :         +8
Element value :         +9
Element value :         +4
Element value :         +5
Element value :         +2
Element value :         +6
Element value :         +7
Element value :         +3
Element value :         +1
LevelOrder :
Element value :         +1
Element value :         +2
Element value :         +3
Element value :         +4
Element value :         +5
Element value :         +6
Element value :         +7
Element value :         +8
Element value :         +9


```


## ATS


```ATS
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)
//
datatype
tree (a:t@ype) =
  | tnil of ()
  | tcons of (tree a, a, tree a)
//
(* ****** ****** *)

symintr ++
infixr (+) ++
overload ++ with list_append

(* ****** ****** *)

#define sing list_sing

(* ****** ****** *)

fun{
a:t@ype
} preorder
  (t0: tree a): List0 a =
  case t0 of
  | tnil () => nil ()
  | tcons (tl, x, tr) => sing(x) ++ preorder(tl) ++ preorder(tr)

(* ****** ****** *)

fun{
a:t@ype
} inorder
  (t0: tree a): List0 a =
  case t0 of
  | tnil () => nil ()
  | tcons (tl, x, tr) => inorder(tl) ++ sing(x) ++ inorder(tr)

(* ****** ****** *)

fun{
a:t@ype
} postorder
  (t0: tree a): List0 a =
  case t0 of
  | tnil () => nil ()
  | tcons (tl, x, tr) => postorder(tl) ++ postorder(tr) ++ sing(x)

(* ****** ****** *)

fun{
a:t@ype
} levelorder
  (t0: tree a): List0 a = let
//
fun auxlst
  (ts: List (tree(a))): List0 a =
  case ts of
  | list_nil () => list_nil ()
  | list_cons (t, ts) =>
    (
      case+ t of
      | tnil () => auxlst (ts)
      | tcons (tl, x, tr) => cons (x, auxlst (ts ++ $list{tree(a)}(tl, tr)))
    )
//
in
  auxlst (sing(t0))
end // end of [levelorder]

(* ****** ****** *)

macdef
tsing(x) = tcons (tnil, ,(x), tnil)

(* ****** ****** *)

implement
main0 () = let
//
val t0 =
tcons{int}
(
 tcons (tcons (tsing (7), 4, tnil ()), 2, tsing (5))
,
1
,
 tcons (tcons (tsing (8), 6, tsing (9)), 3, tnil ())
)
//
in
  println! ("preorder:\t", preorder(t0));
  println! ("inorder:\t", inorder(t0));
  println! ("postorder:\t", postorder(t0));
  println! ("level-order:\t", levelorder(t0));
end (* end of [main0] *)
```


{{out}}

```txt
preorder:	1 2 4 7 5 3 6 8 9
inorder:	7 4 2 5 1 8 6 9 3
postorder:	7 4 5 2 8 9 6 3 1
level-order:	1 2 3 4 5 6 7 8 9
```



## AutoHotkey

{{works with|AutoHotkey_L|45}}

```AutoHotkey
AddNode(Tree,1,2,3,1) ; Build global Tree
AddNode(Tree,2,4,5,2)
AddNode(Tree,3,6,0,3)
AddNode(Tree,4,7,0,4)
AddNode(Tree,5,0,0,5)
AddNode(Tree,6,8,9,6)
AddNode(Tree,7,0,0,7)
AddNode(Tree,8,0,0,8)
AddNode(Tree,9,0,0,9)

MsgBox % "Preorder: "   PreOrder(Tree,1)  ; 1 2 4 7 5 3 6 8 9
MsgBox % "Inorder: "    InOrder(Tree,1)   ; 7 4 2 5 1 8 6 9 3
MsgBox % "postorder: "  PostOrder(Tree,1) ; 7 4 5 2 8 9 6 3 1
MsgBox % "levelorder: " LevOrder(Tree,1)  ; 1 2 3 4 5 6 7 8 9

AddNode(ByRef Tree,Node,Left,Right,Value) {
   if !isobject(Tree)
     Tree := object()

   Tree[Node, "L"] := Left
   Tree[Node, "R"] := Right
   Tree[Node, "V"] := Value
}

PreOrder(Tree,Node) {
ptree := Tree[Node, "V"] " "
        . ((L:=Tree[Node, "L"]) ? PreOrder(Tree,L) : "")
        . ((R:=Tree[Node, "R"]) ? PreOrder(Tree,R) : "")
return ptree
}
InOrder(Tree,Node) {
   Return itree := ((L:=Tree[Node, "L"]) ? InOrder(Tree,L) : "")
        . Tree[Node, "V"] " "
        . ((R:=Tree[Node, "R"]) ? InOrder(Tree,R) : "")
}
PostOrder(Tree,Node) {
   Return ptree := ((L:=Tree[Node, "L"]) ? PostOrder(Tree,L) : "")
        . ((R:=Tree[Node, "R"]) ? PostOrder(Tree,R) : "")
        . Tree[Node, "V"] " "
}
LevOrder(Tree,Node,Lev=1) {
   Static                        ; make node lists static
   i%Lev% .= Tree[Node, "V"] " " ; build node lists in every level
   If (L:=Tree[Node, "L"])
       LevOrder(Tree,L,Lev+1)
   If (R:=Tree[Node, "R"])
       LevOrder(Tree,R,Lev+1)
   If (Lev > 1)
      Return
   While i%Lev%                  ; concatenate node lists from all levels
      t .= i%Lev%, Lev++
   Return t
}
```



## AWK


```awk

function preorder(tree, node, res,  child) {
    if (node == "")
        return
    res[res["count"]++] = node
    split(tree[node], child, ",")
    preorder(tree,child[1],res)
    preorder(tree,child[2],res)
}

function inorder(tree, node, res,   child) {
    if (node == "")
        return
    split(tree[node], child, ",")
    inorder(tree,child[1],res)
    res[res["count"]++] = node
    inorder(tree,child[2],res)
}

function postorder(tree, node, res,     child) {
    if (node == "")
        return
    split(tree[node], child, ",")
    postorder(tree,child[1], res)
    postorder(tree,child[2], res)
    res[res["count"]++] = node
}

function levelorder(tree, node, res,    nextnode, queue, child) {
    if (node == "")
        return

    queue["tail"] = 0
    queue[queue["head"]++] = node

    while (queue["head"] - queue["tail"] >= 1) {

        nextnode = queue[queue["tail"]]
        delete queue[queue["tail"]++]

        res[res["count"]++] = nextnode

        split(tree[nextnode], child, ",")
        if (child[1] != "")
            queue[queue["head"]++] = child[1]
        if (child[2] != "")
            queue[queue["head"]++] = child[2]
    }
    delete queue
}

BEGIN {
    tree["1"] = "2,3"
    tree["2"] = "4,5"
    tree["3"] = "6,"
    tree["4"] = "7,"
    tree["5"] = ","
    tree["6"] = "8,9"
    tree["7"] = ","
    tree["8"] = ","
    tree["9"] = ","

    preorder(tree,"1",result)
    printf "preorder:\t"
    for (n = 0; n < result["count"]; n += 1)
        printf result[n]" "
    printf "\n"
    delete result

    inorder(tree,"1",result)
    printf "inorder:\t"
    for (n = 0; n < result["count"]; n += 1)
        printf result[n]" "
    printf "\n"
    delete result

    postorder(tree,"1",result)
    printf "postorder:\t"
    for (n = 0; n < result["count"]; n += 1)
        printf result[n]" "
    printf "\n"
    delete result

    levelorder(tree,"1",result)
    printf "level-order:\t"
    for (n = 0; n < result["count"]; n += 1)
        printf result[n]" "
    printf "\n"
    delete result
}

```



## Bracmat


```bracmat
(
  ( tree
  =   1
    .   (2.(4.7.) (5.))
        (3.6.(8.) (9.))
  )
& ( preorder
  =   K sub
    .     !arg:(?K.?sub) ?arg
        & !K preorder$!sub preorder$!arg
      |
  )
& out$("preorder:   " preorder$!tree)
& ( inorder
  =   K lhs rhs
    .   !arg:(?K.?sub) ?arg
      & (   !sub:%?lhs ?rhs
          & inorder$!lhs !K inorder$!rhs inorder$!arg
        | !K
        )
  )
& out$("inorder:    " inorder$!tree)
& ( postorder
  =   K sub
    .     !arg:(?K.?sub) ?arg
        & postorder$!sub !K postorder$!arg
      |
  )
& out$("postorder:  " postorder$!tree)
& ( levelorder
  =   todo tree sub
    .   !arg:(.)&
      |   !arg:(?tree.?todo)
        & (   !tree:(?K.?sub) ?tree
            & !K levelorder$(!tree.!todo !sub)
          | levelorder$(!todo.)
          )
  )
& out$("level-order:" levelorder$(!tree.))
&
)
```



## C


```cpp
#include <iostream>
#include <stdio.h>

typedef struct node_s
{
  int value;
  struct node_s* left;
  struct node_s* right;
} *node;

node tree(int v, node l, node r)
{
  node n = malloc(sizeof(struct node_s));
  n->value = v;
  n->left  = l;
  n->right = r;
  return n;
}

void destroy_tree(node n)
{
  if (n->left)
    destroy_tree(n->left);
  if (n->right)
    destroy_tree(n->right);
  free(n);
}

void preorder(node n, void (*f)(int))
{
  f(n->value);
  if (n->left)
    preorder(n->left, f);
  if (n->right)
    preorder(n->right, f);
}

void inorder(node n, void (*f)(int))
{
  if (n->left)
    inorder(n->left, f);
  f(n->value);
  if (n->right)
    inorder(n->right, f);
}

void postorder(node n, void (*f)(int))
{
  if (n->left)
    postorder(n->left, f);
  if (n->right)
    postorder(n->right, f);
  f(n->value);
}

/* helper queue for levelorder */
typedef struct qnode_s
{
  struct qnode_s* next;
  node value;
} *qnode;

typedef struct { qnode begin, end; } queue;

void enqueue(queue* q, node n)
{
  qnode node = malloc(sizeof(struct qnode_s));
  node->value = n;
  node->next = 0;
  if (q->end)
    q->end->next = node;
  else
    q->begin = node;
  q->end = node;
}

node dequeue(queue* q)
{
  node tmp = q->begin->value;
  qnode second = q->begin->next;
  free(q->begin);
  q->begin = second;
  if (!q->begin)
    q->end = 0;
  return tmp;
}

int queue_empty(queue* q)
{
  return !q->begin;
}

void levelorder(node n, void(*f)(int))
{
  queue nodequeue = {};
  enqueue(&nodequeue, n);
  while (!queue_empty(&nodequeue))
  {
    node next = dequeue(&nodequeue);
    f(next->value);
    if (next->left)
      enqueue(&nodequeue, next->left);
    if (next->right)
      enqueue(&nodequeue, next->right);
  }
}

void print(int n)
{
  printf("%d ", n);
}

int main()
{
  node n = tree(1,
                tree(2,
                     tree(4,
                          tree(7, 0, 0),
                          0),
                     tree(5, 0, 0)),
                tree(3,
                     tree(6,
                          tree(8, 0, 0),
                          tree(9, 0, 0)),
                     0));

  printf("preorder:    ");
  preorder(n, print);
  printf("\n");

  printf("inorder:     ");
  inorder(n, print);
  printf("\n");

  printf("postorder:   ");
  postorder(n, print);
  printf("\n");

  printf("level-order: ");
  levelorder(n, print);
  printf("\n");

  destroy_tree(n);

  return 0;
}
```



## C sharp


```csharp
using System;
using System.Collections.Generic;
using System.Linq;

class Node
{
    int Value;
    Node Left;
    Node Right;

    Node(int value = default(int), Node left = default(Node), Node right = default(Node))
    {
        Value = value;
        Left = left;
        Right = right;
    }

    IEnumerable<int> Preorder()
    {
        yield return Value;
        if (Left != null)
            foreach (var value in Left.Preorder())
                yield return value;
        if (Right != null)
            foreach (var value in Right.Preorder())
                yield return value;
    }

    IEnumerable<int> Inorder()
    {
        if (Left != null)
            foreach (var value in Left.Inorder())
                yield return value;
        yield return Value;
        if (Right != null)
            foreach (var value in Right.Inorder())
                yield return value;
    }

    IEnumerable<int> Postorder()
    {
        if (Left != null)
            foreach (var value in Left.Postorder())
                yield return value;
        if (Right != null)
            foreach (var value in Right.Postorder())
                yield return value;
        yield return Value;
    }

    IEnumerable<int> LevelOrder()
    {
        var queue = new Queue<Node>();
        queue.Enqueue(this);
        while (queue.Any())
        {
            var node = queue.Dequeue();
            yield return node.Value;
            if (node.Left != null)
                queue.Enqueue(node.Left);
            if (node.Right != null)
                queue.Enqueue(node.Right);
        }
    }

    static void Main()
    {
        var tree = new Node(1, new Node(2, new Node(4, new Node(7)), new Node(5)), new Node(3, new Node(6, new Node(8), new Node(9))));
        foreach (var traversal in new Func<IEnumerable<int>>[] { tree.Preorder, tree.Inorder, tree.Postorder, tree.LevelOrder })
            Console.WriteLine("{0}:\t{1}", traversal.Method.Name, string.Join(" ", traversal()));
    }
}
```



## C++

'''Compiler:''' [[g++]] (version 4.3.2 20081105 (Red Hat 4.3.2-7))

{{libheader|Boost|1.39.0}}


```cpp
#include <boost/scoped_ptr.hpp>
#include <iostream>
#include <queue>

template<typename T>
class TreeNode {
public:
  TreeNode(const T& n, TreeNode* left = NULL, TreeNode* right = NULL)
    : mValue(n),
      mLeft(left),
      mRight(right) {}

  T getValue() const {
    return mValue;
  }

  TreeNode* left() const {
    return mLeft.get();
  }

  TreeNode* right() const {
    return mRight.get();
  }

  void preorderTraverse() const {
    std::cout << " " << getValue();
    if(mLeft)  { mLeft->preorderTraverse();  }
    if(mRight) { mRight->preorderTraverse(); }
  }

  void inorderTraverse() const {
    if(mLeft)  { mLeft->inorderTraverse();  }
    std::cout << " " << getValue();
    if(mRight) { mRight->inorderTraverse(); }
  }

  void postorderTraverse() const {
    if(mLeft)  { mLeft->postorderTraverse();  }
    if(mRight) { mRight->postorderTraverse(); }
    std::cout << " " << getValue();
  }

  void levelorderTraverse() const {
    std::queue<const TreeNode*> q;
    q.push(this);

    while(!q.empty()) {
      const TreeNode* n = q.front();
      q.pop();
      std::cout << " " << n->getValue();

      if(n->left())  { q.push(n->left());  }
      if(n->right()) { q.push(n->right()); }
    }
  }

protected:
  T mValue;
  boost::scoped_ptr<TreeNode> mLeft;
  boost::scoped_ptr<TreeNode> mRight;

private:
  TreeNode();
};

int main() {
  TreeNode<int> root(1,
    new TreeNode<int>(2,
      new TreeNode<int>(4,
        new TreeNode<int>(7)),
      new TreeNode<int>(5)),
    new TreeNode<int>(3,
      new TreeNode<int>(6,
        new TreeNode<int>(8),
        new TreeNode<int>(9))));

  std::cout << "preorder:   ";
  root.preorderTraverse();
  std::cout << std::endl;

  std::cout << "inorder:    ";
  root.inorderTraverse();
  std::cout << std::endl;

  std::cout << "postorder:  ";
  root.postorderTraverse();
  std::cout << std::endl;

  std::cout << "level-order:";
  root.levelorderTraverse();
  std::cout << std::endl;

  return 0;
}
```



## Ceylon


```ceylon
import ceylon.collection {
	ArrayList
}

shared void run() {

	class Node(label, left = null, right = null) {
		shared Integer label;
		shared Node? left;
		shared Node? right;
		string => label.string;
	}

	void preorder(Node node) {
		process.write(node.string + " ");
		if(exists left = node.left) {
			preorder(left);
		}
		if(exists right = node.right) {
			preorder(right);
		}
	}

	void inorder(Node node) {
		if(exists left = node.left) {
			inorder(left);
		}
		process.write(node.string + " ");
		if(exists right = node.right) {
			inorder(right);
		}
	}

	void postorder(Node node) {
		if(exists left = node.left) {
			postorder(left);
		}
		if(exists right = node.right) {
			postorder(right);
		}
		process.write(node.string + " ");
	}

	void levelOrder(Node node) {
		value nodes = ArrayList<Node> {node};
		while(exists current = nodes.accept()) {
			process.write(current.string + " ");
			if(exists left = current.left) {
				nodes.offer(left);
			}
			if(exists right = current.right) {
				nodes.offer(right);
			}
		}
	}

	value tree = Node {
		label = 1;
		left = Node {
			label = 2;
			left = Node {
				label = 4;
				left = Node {
					label = 7;
				};
			};
			right = Node {
				label = 5;
			};
		};
		right = Node {
			label = 3;
			left = Node {
				label = 6;
				left = Node {
					label = 8;
				};
				right = Node {
					label = 9;
				};
			};
		};
	};

	process.write("preorder:   ");
	preorder(tree);
	print("");
	process.write("inorder:    ");
	inorder(tree);
	print("");
	process.write("postorder:  ");
	postorder(tree);
	print("");
	process.write("levelorder: ");
	levelOrder(tree);
	print("");
}
```



## Clojure


```clojure
(defn walk [node f order]
  (when node
   (doseq [o order]
     (if (= o :visit)
       (f (:val node))
       (walk (node o) f order)))))

(defn preorder [node f]
  (walk node f [:visit :left :right]))

(defn inorder [node f]
  (walk node f [:left :visit :right]))

(defn postorder [node f]
  (walk node f [:left :right :visit]))

(defn queue [& xs]
  (when (seq xs)
   (apply conj clojure.lang.PersistentQueue/EMPTY xs)))

(defn level-order [root f]
  (loop [q (queue root)]
    (when-not (empty? q)
      (if-let [node (first q)]
        (do
          (f (:val node))
          (recur (conj (pop q) (:left node) (:right node))))
        (recur (pop q))))))

(defn vec-to-tree [t]
  (if (vector? t)
    (let [[val left right] t]
      {:val val
       :left (vec-to-tree left)
       :right (vec-to-tree right)})
    t))

(let [tree (vec-to-tree [1 [2 [4 [7]] [5]] [3 [6 [8] [9]]]])
      fs   '[preorder inorder postorder level-order]
      pr-node #(print (format "%2d" %))]
  (doseq [f fs]
    (print (format "%-12s" (str f ":")))
    ((resolve f) tree pr-node)
    (println)))
```



## CoffeeScript


```coffeescript

# In this example, we don't encapsulate binary trees as objects; instead, we have a
# convention on how to store them as arrays, and we namespace the functions that
# operate on those data structures.
binary_tree =
  preorder: (tree, visit) ->
    return unless tree?
    [node, left, right] = tree
    visit node
    binary_tree.preorder left, visit
    binary_tree.preorder right, visit

  inorder: (tree, visit) ->
    return unless tree?
    [node, left, right] = tree
    binary_tree.inorder left, visit
    visit node
    binary_tree.inorder right, visit

  postorder: (tree, visit) ->
    return unless tree?
    [node, left, right] = tree
    binary_tree.postorder left, visit
    binary_tree.postorder right, visit
    visit node

  levelorder: (tree, visit) ->
    q = []
    q.push tree
    while q.length > 0
      t = q.shift()
      continue unless t?
      [node, left, right] = t
      visit node
      q.push left
      q.push right

do ->
  tree = [1, [2, [4, [7]], [5]], [3, [6, [8],[9]]]]
  test_walk = (walk_function_name) ->
    output = []
    binary_tree[walk_function_name] tree, output.push.bind(output)
    console.log walk_function_name, output.join ' '
  test_walk "preorder"
  test_walk "inorder"
  test_walk "postorder"
  test_walk "levelorder"

```

output
<lang>
> coffee tree_traversal.coffee
preorder 1 2 4 7 5 3 6 8 9
inorder 7 4 2 5 1 8 6 9 3
postorder 7 4 5 2 8 9 6 3 1
levelorder 1 2 3 4 5 6 7 8 9

```



## Common Lisp



```lisp
(defun preorder (node f)
  (when node
    (funcall f (first node))
    (preorder (second node) f)
    (preorder (third node)  f)))

(defun inorder (node f)
  (when node
    (inorder (second node) f)
    (funcall f (first node))
    (inorder (third node)  f)))

(defun postorder (node f)
  (when node
    (postorder (second node) f)
    (postorder (third node)  f)
    (funcall f (first node))))

(defun level-order (node f)
  (loop with level = (list node)
        while level
        do
    (setf level (loop for node in level
                      when node
                        do (funcall f (first node))
                        and collect (second node)
                        and collect (third node)))))

(defparameter *tree* '(1 (2 (4 (7))
                            (5))
                         (3 (6 (8)
                               (9)))))

(defun show (traversal-function)
  (format t "~&~(~A~):~12,0T" traversal-function)
  (funcall traversal-function *tree* (lambda (value) (format t " ~A" value))))

(map nil #'show '(preorder inorder postorder level-order))
```


Output:

 preorder:    1 2 4 7 5 3 6 8 9
 inorder:     7 4 2 5 1 8 6 9 3
 postorder:   7 4 2 5 1 8 6 9 3
 level-order: 1 2 3 4 5 6 7 8 9


## Coq



```coq
Require Import Utf8.
Require Import List.

Unset Elimination Schemes.

(* Rose tree, with numbers on nodes *)
Inductive tree := Tree { value : nat ; children : list tree }.

Fixpoint height (t: tree) : nat :=
  1 + fold_left (λ n t, max n (height t)) (children t) 0.

Example leaf n : tree := {| value := n ; children := nil |}.

Example t2 : tree := {| value := 2 ; children := {| value := 4 ; children := leaf 7 :: nil |} :: leaf 5 :: nil |}.

Example t3 : tree := {| value := 3 ; children := {| value := 6 ; children := leaf 8 :: leaf 9 :: nil |} :: nil |}.

Example t9 : tree := {| value := 1 ; children := t2 :: t3 :: nil |}.

Fixpoint preorder (t: tree) : list nat :=
  let '{| value := n ; children := c |} := t in
  n :: flat_map preorder c.

Fixpoint inorder (t: tree) : list nat :=
  let '{| value := n ; children := c |} := t in
  match c with
  | nil => n :: nil
  | ℓ :: r => inorder ℓ ++ n :: flat_map inorder r
  end.

Fixpoint postorder (t: tree) : list nat :=
  let '{| value := n ; children := c |} := t in
  flat_map postorder c ++ n :: nil.

(* Auxiliary function for levelorder, which operates on forests *)
(* Since the recursion is tricky, it relies on a fuel parameter which obviously decreases. *)
Fixpoint levelorder_forest (fuel: nat) (f: list tree) : list nat:=
  match fuel with
  | O => nil
  | S fuel' =>
    let '(p, f) := fold_right (λ t r, let '(x, f) := r in (value t :: x, children t ++ f) ) (nil, nil) f in
    p ++ levelorder_forest fuel' f
  end.

Definition levelorder (t: tree) : list nat :=
  levelorder_forest (height t) (t :: nil).

Compute preorder t9.
Compute inorder t9.
Compute postorder t9.
Compute levelorder t9.

```


## Crystal

{{trans|C++}}

```crystal

class Node(T)
  property left : Nil | Node(T)
  property right : Nil | Node(T)
  property data : T

  def initialize(@data, @left = nil, @right = nil)
  end

  def preorder_traverse
    print " #{data}"
    if left = @left
      left.preorder_traverse
    end
    if right = @right
      right.preorder_traverse
    end
  end

  def inorder_traverse
    if left = @left
      left.inorder_traverse
    end
    print " #{data}"
    if right = @right
      right.inorder_traverse
    end
  end

  def postorder_traverse
    if left = @left
      left.postorder_traverse
    end
    if right = @right
      right.postorder_traverse
    end
    print " #{data}"
  end

  def levelorder_traverse
    queue = Array(Node(T)).new
    queue << self

    until queue.size <= 0
      node = queue.shift

      unless node
        next
      end

      print " #{node.data}"

      if left = node.left
        queue << left
      end
      if right = node.right
        queue << right
      end
    end
  end
end

tree = Node(Int32).new(1,
  Node(Int32).new(2,
    Node(Int32).new(4,
      Node(Int32).new(7)),
    Node(Int32).new(5)),
  Node(Int32).new(3,
    Node(Int32).new(6,
      Node(Int32).new(8),
      Node(Int32).new(9))))

print "preorder:   "
tree.preorder_traverse
print "\ninorder:    "
tree.inorder_traverse
print "\npostorder:  "
tree.postorder_traverse
print "\nlevelorder: "
tree.levelorder_traverse
puts


```

Output:

```txt

preorder:    1 2 4 7 5 3 6 8 9
inorder:     7 4 2 5 1 8 6 9 3
postorder:   7 4 5 2 8 9 6 3 1
levelorder:  1 2 3 4 5 6 7 8 9

```



## D

This code is long because it's very generic.

```d
import std.stdio, std.traits;

const final class Node(T) {
    T data;
    Node left, right;

    this(in T data, in Node left=null, in Node right=null)
    const pure nothrow {
        this.data = data;
        this.left = left;
        this.right = right;
    }
}

// 'static' templated opCall can't be used in Node
auto node(T)(in T data, in Node!T left=null, in Node!T right=null)
pure nothrow {
    return new const(Node!T)(data, left, right);
}

void show(T)(in T x) {
    write(x, " ");
}

enum Visit { pre, inv, post }

// 'visitor' can be any kind of callable or it uses a default visitor.
// TNode can be any kind of Node, with data, left and right fields,
// so this is more generic than a member function of Node.
void backtrackingOrder(Visit v, TNode, TyF=void*)
                      (in TNode node, TyF visitor=null) {
    alias trueVisitor = Select!(is(TyF == void*), show, visitor);
    if (node !is null) {
        static if (v == Visit.pre)
            trueVisitor(node.data);
        backtrackingOrder!v(node.left, visitor);
        static if (v == Visit.inv)
            trueVisitor(node.data);
        backtrackingOrder!v(node.right, visitor);
        static if (v == Visit.post)
            trueVisitor(node.data);
    }
}

void levelOrder(TNode, TyF=void*)
               (in TNode node, TyF visitor=null, const(TNode)[] more=[]) {
    alias trueVisitor = Select!(is(TyF == void*), show, visitor);
    if (node !is null) {
        more ~= [node.left, node.right];
        trueVisitor(node.data);
    }
    if (more.length)
        levelOrder(more[0], visitor, more[1 .. $]);
}

void main() {
    alias N = node;
    const tree = N(1,
                      N(2,
                           N(4,
                                N(7)),
                           N(5)),
                      N(3,
                           N(6,
                                N(8),
                                N(9))));

    write("  preOrder: ");
    tree.backtrackingOrder!(Visit.pre);
    write("\n   inorder: ");
    tree.backtrackingOrder!(Visit.inv);
    write("\n postOrder: ");
    tree.backtrackingOrder!(Visit.post);
    write("\nlevelorder: ");
    tree.levelOrder;
    writeln;
}
```

{{out}}

```txt
  preOrder: 1 2 4 7 5 3 6 8 9
   inorder: 7 4 2 5 1 8 6 9 3
 postOrder: 7 4 5 2 8 9 6 3 1
levelorder: 1 2 3 4 5 6 7 8 9
```



### Alternative Version

{{trans|Haskell}}
Generic as the first version, but not lazy as the Haskell version.

```d
const struct Node(T) {
    T v;
    Node* l, r;
}

T[] preOrder(T)(in Node!T* t) pure nothrow {
    return t ? t.v ~ preOrder(t.l) ~ preOrder(t.r) : [];
}

T[] inOrder(T)(in Node!T* t) pure nothrow {
    return t ? inOrder(t.l) ~ t.v ~ inOrder(t.r) : [];
}

T[] postOrder(T)(in Node!T* t) pure nothrow {
    return t ? postOrder(t.l) ~ postOrder(t.r) ~ t.v : [];
}

T[] levelOrder(T)(in Node!T* t) pure nothrow {
    static T[] loop(in Node!T*[] a) pure nothrow {
        if (!a.length) return [];
        if (!a[0]) return loop(a[1 .. $]);
        return a[0].v ~ loop(a[1 .. $] ~ [a[0].l, a[0].r]);
    }
    return loop([t]);
}

void main() {
    alias N = Node!int;
    auto tree = new N(1,
                     new N(2,
                          new N(4,
                               new N(7)),
                          new N(5)),
                     new N(3,
                          new N(6,
                               new N(8),
                               new N(9))));

    import std.stdio;
    writeln(preOrder(tree));
    writeln(inOrder(tree));
    writeln(postOrder(tree));
    writeln(levelOrder(tree));
}
```

{{out}}

```txt
[1, 2, 4, 7, 5, 3, 6, 8, 9]
[7, 4, 2, 5, 1, 8, 6, 9, 3]
[7, 4, 5, 2, 8, 9, 6, 3, 1]
[1, 2, 3, 4, 5, 6, 7, 8, 9]
```



### Alternative Lazy Version

This version is not complete, it lacks the level order visit.

```d
import std.stdio, std.algorithm, std.range, std.string;

const struct Tree(T) {
    T value;
    Tree* left, right;
}

alias VisitRange(T) = InputRange!(const Tree!T);

VisitRange!T preOrder(T)(in Tree!T* t) /*pure nothrow*/ {
    enum self = mixin("&" ~ __FUNCTION__.split(".").back);
    if (t == null)
        return typeof(return).init.takeNone.inputRangeObject;
    return [*t]
           .chain([t.left, t.right]
                  .filter!(t => t != null)
                  .map!(a => self(a))
                  .joiner)
           .inputRangeObject;
}

VisitRange!T inOrder(T)(in Tree!T* t) /*pure nothrow*/ {
    enum self = mixin("&" ~ __FUNCTION__.split(".").back);
    if (t == null)
        return typeof(return).init.takeNone.inputRangeObject;
    return [t.left]
           .filter!(t => t != null)
           .map!(a => self(a))
           .joiner
           .chain([*t])
           .chain([t.right]
                  .filter!(t => t != null)
                  .map!(a => self(a))
                  .joiner)
           .inputRangeObject;
}

VisitRange!T postOrder(T)(in Tree!T* t) /*pure nothrow*/ {
    enum self = mixin("&" ~ __FUNCTION__.split(".").back);
    if (t == null)
        return typeof(return).init.takeNone.inputRangeObject;
    return [t.left, t.right]
           .filter!(t => t != null)
           .map!(a => self(a))
           .joiner
           .chain([*t])
           .inputRangeObject;
}

void main() {
    alias N = Tree!int;
    const tree = new N(1,
                       new N(2,
                             new N(4,
                                   new N(7)),
                             new N(5)),
                       new N(3,
                             new N(6,
                                   new N(8),
                                   new N(9))));

    tree.preOrder.map!(t => t.value).writeln;
    tree.inOrder.map!(t => t.value).writeln;
    tree.postOrder.map!(t => t.value).writeln;
}
```

{{out}}

```txt
[1, 2, 4, 7, 5, 3, 6, 8, 9]
[7, 4, 2, 5, 1, 8, 6, 9, 3]
[7, 4, 5, 2, 8, 9, 6, 3, 1]
```



## E



```e
def btree := [1, [2, [4, [7, null, null],
                         null],
                     [5, null, null]],
                 [3, [6, [8, null, null],
                         [9, null, null]],
                     null]]

def backtrackingOrder(node, pre, mid, post) {
    switch (node) {
        match ==null {}
        match [value, left, right] {
            pre(value)
            backtrackingOrder(left, pre, mid, post)
            mid(value)
            backtrackingOrder(right, pre, mid, post)
            post(value)
        }
    }
}

def levelOrder(root, func) {
    var level := [root].diverge()
    while (level.size() > 0) {
        for node in level.removeRun(0) {
            switch (node) {
                match ==null {}
                match [value, left, right] {
                    func(value)
                    level.push(left)
                    level.push(right)
}   }   }   }   }

print("preorder:   ")
backtrackingOrder(btree, fn v { print(" ", v) }, fn _ {}, fn _ {})
println()

print("inorder:    ")
backtrackingOrder(btree, fn _ {}, fn v { print(" ", v) }, fn _ {})
println()

print("postorder:  ")
backtrackingOrder(btree, fn _ {}, fn _ {}, fn v { print(" ", v) })
println()

print("level-order:")
levelOrder(btree, fn v { print(" ", v) })
println()
```



## Eiffel

{{works with|EiffelStudio|7.3, Void-Safety disabled}}

Void-Safety has been disabled for simplicity of the code.

```eiffel
note
	description : "Application for tree traversal demonstration"
        output      : "[
    	                Prints preorder, inorder, postorder and levelorder traversal of an example binary tree.
    		      ]"
	author	    : "Jascha Grübel"
	date        : "$2014-01-07$"
	revision    : "$1.0$"

class
	APPLICATION

create
	make

feature {NONE} -- Initialization

	make
			-- Run Tree traversal example.
		local
			tree:NODE
		do
			create tree.make (1)
			tree.set_left_child (create {NODE}.make (2))
			tree.set_right_child (create {NODE}.make (3))
			tree.left_child.set_left_child (create {NODE}.make (4))
			tree.left_child.set_right_child (create {NODE}.make (5))
			tree.left_child.left_child.set_left_child (create {NODE}.make (7))
			tree.right_child.set_left_child (create {NODE}.make (6))
			tree.right_child.left_child.set_left_child (create {NODE}.make (8))
			tree.right_child.left_child.set_right_child (create {NODE}.make (9))

			Io.put_string ("preorder:   ")
			tree.print_preorder
			Io.put_new_line

			Io.put_string ("inorder:    ")
			tree.print_inorder
			Io.put_new_line

			Io.put_string ("postorder:  ")
			tree.print_postorder
			Io.put_new_line

			Io.put_string ("level-order:")
			tree.print_levelorder
			Io.put_new_line

		end

end -- class APPLICATION
```


```eiffel
note
	description    : "A simple node for a binary tree"
        libraries      : "Relies on LINKED_LIST from EiffelBase"
	author         : "Jascha Grübel"
	date           : "$2014-01-07$"
	revision       : "$1.0$"
        implementation : "[
			   All traversals but the levelorder traversal have been implemented recursively.
                           The levelorder traversal is solved iteratively.
			 ]"

class
	NODE
create
	make

feature {NONE} -- Initialization

	make (a_value:INTEGER)
			-- Creates a node with no children.
		do
			value := a_value
			set_right_child(Void)
			set_left_child(Void)
		end

feature -- Modification

	set_right_child (a_node:NODE)
			-- Sets `right_child' to `a_node'.
		do
			right_child:=a_node
		end

	set_left_child (a_node:NODE)
			-- Sets `left_child' to `a_node'.
		do
			left_child:=a_node
		end

feature -- Representation

	print_preorder
			-- Recursively prints the value of the node and all its children in preorder
		do
			Io.put_string (" " + value.out)
			if has_left_child then
				left_child.print_preorder
			end
			if has_right_child then
				right_child.print_preorder
			end
		end

	print_inorder
			-- Recursively prints the value of the node and all its children in inorder
		do
			if has_left_child then
				left_child.print_inorder
			end
			Io.put_string (" " + value.out)
			if has_right_child then
				right_child.print_inorder
			end
		end

	print_postorder
			-- Recursively prints the value of the node and all its children in postorder
		do
			if has_left_child then
				left_child.print_postorder
			end
			if has_right_child then
				right_child.print_postorder
			end
			Io.put_string (" " + value.out)
		end

	print_levelorder
			-- Iteratively prints the value of the node and all its children in levelorder
		local
			l_linked_list:LINKED_LIST[NODE]
			l_node:NODE
		do
			from
				create l_linked_list.make
				l_linked_list.extend (Current)
			until
				l_linked_list.is_empty
			loop
				l_node := l_linked_list.first
				if l_node.has_left_child then
					l_linked_list.extend (l_node.left_child)
				end
				if l_node.has_right_child then
					l_linked_list.extend (l_node.right_child)
				end
				Io.put_string (" " + l_node.value.out)
				l_linked_list.prune (l_node)
			end
		end

feature -- Access

	value:INTEGER
			-- Value stored in the node.

	right_child:NODE
			-- Reference to right child, possibly void.

	left_child:NODE
			-- Reference to left child, possibly void.

	has_right_child:BOOLEAN
			-- Test right child for existence.
		do
			Result := right_child /= Void
		end

	has_left_child:BOOLEAN
			-- Test left child for existence.
		do
			Result := left_child /= Void
		end

end
 -- class NODE
```


## Elena

ELENA 4.1 :

```elena
import extensions;
import extensions'routines;
import system'collections;

singleton DummyNode
{
    get generic()
        = EmptyEnumerable;
}

class Node
{
    rprop int  Value;
    rprop Node Left;
    rprop Node Right;

    constructor new(int value)
    {
        Value := value
    }

    constructor new(int value, Node left)
    {
        Value := value;
        Left := left;
    }

    constructor new(int value, Node left, Node right)
    {
        Value := value;
        Left := left;
        Right := right
    }

    Preorder = new Enumerable::
    {
        Enumerator enumerator() = CompoundEnumerator.new(
                                        SingleEnumerable.new(Value),
                                        (Left ?? DummyNode).Preorder,
                                        (Right ?? DummyNode).Preorder);
    };

    Inorder = new Enumerable::
    {
        Enumerator enumerator()
        {
            if (nil != Left)
            {
                ^ CompoundEnumerator.new(Left.Inorder, SingleEnumerable.new(Value), (Right ?? DummyNode).Inorder)
            }
            else
            {
                ^ SingleEnumerable.new(Value).enumerator()
            }
        }
    };

    Postorder = new Enumerable::
    {
        Enumerator enumerator()
        {
            if (nil == Left)
            {
                ^ SingleEnumerable.new(Value).enumerator()
            }
            else if (nil == Right)
            {
                ^ CompoundEnumerator.new(Left.Postorder, SingleEnumerable.new(Value))
            }
            else
            {
                ^ CompoundEnumerator.new(Left.Postorder, Right.Postorder, SingleEnumerable.new(Value))
            }
        }
    };

    LevelOrder = new Enumerable::
    {
        Queue<Node> queue := class Queue<Node>.allocate(4).push:self;

        Enumerator enumerator() = new Enumerator::
        {
            bool next() = queue.isNotEmpty();

            get()
            {
                Node item := queue.pop();
                Node left := item.Left;
                Node right := item.Right;

                if (nil != left)
                {
                    queue.push(left)
                };
                if (nil != right)
                {
                    queue.push(right)
                };

                ^ item.Value
            }

            reset()
            {
                NotSupportedException.raise()
            }

            enumerable() = queue;
        };
    };
}

public program()
{
   var tree := Node.new(1, Node.new(2, Node.new(4, Node.new(7)), Node.new(5)), Node.new(3, Node.new(6, Node.new(8), Node.new(9))));

   console.printLine("Preorder  :", tree.Preorder);
   console.printLine("Inorder   :", tree.Inorder);
   console.printLine("Postorder :", tree.Postorder);
   console.printLine("LevelOrder:", tree.LevelOrder)
}
```

{{out}}

```txt

Preorder  :1,2,4,7,5,3,6,8,9
Inorder   :7,4,2,5,1,8,6,9,3
Postorder :7,4,5,2,8,9,6,3,1
LevelOrder:1,2,3,4,5,6,7,8,9

```



## Elisa

This is a generic component for binary tree traversals. More information about binary trees in Elisa are given in [http://jklunder.home.xs4all.nl/elisa/part02/doc030.html trees].

```Elisa

component BinaryTreeTraversals (Tree, Element);
type Tree;
type Node = Tree;
     Tree (LeftTree = Tree, Element, RightTree = Tree) -> Tree;
     Leaf (Element)                                    -> Node;
     Node (Tree)                                       -> Node;
     Item (Node)                                       -> Element;

     Preorder (Tree)                                   -> multi (Node);
     Inorder (Tree)                                    -> multi (Node);
     Postorder (Tree)                                  -> multi (Node);
     Level_order(Tree) 		                       -> multi (Node);
begin
     Tree (Lefttree, Item, Righttree) = Tree: [ Lefttree; Item; Righttree ];
     Leaf (anItem) = Tree (null(Tree), anItem, null(Tree) );
     Node (aTree) = aTree;
     Item (aNode) = aNode.Item;

     Preorder (=null(Tree)) = no(Tree);
     Preorder (T) = ( T, Preorder (T.Lefttree), Preorder (T.Righttree));

     Inorder (=null(Tree)) = no(Tree);
     Inorder (T) = ( Inorder (T.Lefttree), T, Inorder (T.Righttree));

     Postorder (=null(Tree)) = no(Tree);
     Postorder (T) = ( Postorder (T.Lefttree), Postorder (T.Righttree), T);

     Level_order(T) = [ Queue = {T};
			 node = Tree:items(Queue);
			   [ result(node);
			     add(Queue, node.Lefttree) when valid(node.Lefttree);
 			     add(Queue, node.Righttree) when valid(node.Righttree);
			   ];
			 no(Tree);
		       ];
end component BinaryTreeTraversals;

```

Tests

```Elisa

use BinaryTreeTraversals (Tree, integer);

BT = Tree(
	Tree(
          Tree(Leaf(7), 4, null(Tree)), 2 , Leaf(5)), 1,
            Tree(
              Tree(Leaf(8), 6, Leaf(9)), 3 ,null(Tree)));

{Item(Preorder(BT))}?
{ 1, 2, 4, 7, 5, 3, 6, 8, 9}

{Item(Inorder(BT))}?
{ 7, 4, 2, 5, 1, 8, 6, 9, 3}

{Item(Postorder(BT))}?
{ 7, 4, 5, 2, 8, 9, 6, 3, 1}

{Item(Level_order(BT))}?
{ 1, 2, 3, 4, 5, 6, 7, 8, 9}

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Tree_Traversal do
  defp tnode, do: {}
  defp tnode(v), do: {:node, v, {}, {}}
  defp tnode(v,l,r), do: {:node, v, l, r}

  defp preorder(_,{}), do: :ok
  defp preorder(f,{:node,v,l,r}) do
    f.(v)
    preorder(f,l)
    preorder(f,r)
  end

  defp inorder(_,{}), do: :ok
  defp inorder(f,{:node,v,l,r}) do
    inorder(f,l)
    f.(v)
    inorder(f,r)
  end

  defp postorder(_,{}), do: :ok
  defp postorder(f,{:node,v,l,r}) do
    postorder(f,l)
    postorder(f,r)
    f.(v)
  end

  defp levelorder(_, []), do: []
  defp levelorder(f, [{}|t]), do: levelorder(f, t)
  defp levelorder(f, [{:node,v,l,r}|t]) do
    f.(v)
    levelorder(f, t++[l,r])
  end
  defp levelorder(f, x), do: levelorder(f, [x])

  def main do
    tree = tnode(1,
                 tnode(2,
                       tnode(4, tnode(7), tnode()),
                       tnode(5, tnode(), tnode())),
                 tnode(3,
                       tnode(6, tnode(8), tnode(9)),
                       tnode()))
    f = fn x -> IO.write "#{x} " end
    IO.write "preorder:   "
    preorder(f, tree)
    IO.write "\ninorder:    "
    inorder(f, tree)
    IO.write "\npostorder:  "
    postorder(f, tree)
    IO.write "\nlevelorder: "
    levelorder(f, tree)
    IO.puts ""
  end
end

Tree_Traversal.main
```


{{out}}

```txt

preorder:   1 2 4 7 5 3 6 8 9
inorder:    7 4 2 5 1 8 6 9 3
postorder:  7 4 5 2 8 9 6 3 1
levelorder: 1 2 3 4 5 6 7 8 9

```



## Erlang


```erlang
-module(tree_traversal).
-export([main/0]).
-export([preorder/2, inorder/2, postorder/2, levelorder/2]).
-export([tnode/0, tnode/1, tnode/3]).

-define(NEWLINE, io:format("~n")).

tnode() -> {}.
tnode(V) -> {node, V, {}, {}}.
tnode(V,L,R) -> {node, V, L, R}.

preorder(_,{}) -> ok;
preorder(F,{node,V,L,R}) ->
    F(V), preorder(F,L), preorder(F,R).

inorder(_,{}) -> ok;
inorder(F,{node,V,L,R}) ->
    inorder(F,L), F(V), inorder(F,R).

postorder(_,{}) -> ok;
postorder(F,{node,V,L,R}) ->
    postorder(F,L), postorder(F,R), F(V).

levelorder(_, []) -> [];
levelorder(F, [{}|T]) -> levelorder(F, T);
levelorder(F, [{node,V,L,R}|T]) ->
    F(V), levelorder(F, T++[L,R]);
levelorder(F, X) -> levelorder(F, [X]).

main() ->
    Tree = tnode(1,
                 tnode(2,
                       tnode(4, tnode(7), tnode()),
                       tnode(5, tnode(), tnode())),
                 tnode(3,
                       tnode(6, tnode(8), tnode(9)),
                       tnode())),
    F = fun(X) -> io:format("~p ",[X]) end,
    preorder(F, Tree), ?NEWLINE,
    inorder(F, Tree), ?NEWLINE,
    postorder(F, Tree), ?NEWLINE,
    levelorder(F, Tree), ?NEWLINE.
```


Output:

```txt
1 2 4 7 5 3 6 8 9
7 4 2 5 1 8 6 9 3
7 4 5 2 8 9 6 3 1
1 2 3 4 5 6 7 8 9

```



## Euphoria


```euphoria
constant VALUE = 1, LEFT = 2, RIGHT = 3

constant tree = {1,
                    {2,
                        {4,
                            {7, 0, 0},
                            0},
                        {5, 0, 0}},
                    {3,
                        {6,
                            {8, 0, 0},
                            {9, 0, 0}},
                        0}}

procedure preorder(object tree)
    if sequence(tree) then
        printf(1,"%d ",{tree[VALUE]})
        preorder(tree[LEFT])
        preorder(tree[RIGHT])
    end if
end procedure

procedure inorder(object tree)
    if sequence(tree) then
        inorder(tree[LEFT])
        printf(1,"%d ",{tree[VALUE]})
        inorder(tree[RIGHT])
    end if
end procedure

procedure postorder(object tree)
    if sequence(tree) then
        postorder(tree[LEFT])
        postorder(tree[RIGHT])
        printf(1,"%d ",{tree[VALUE]})
    end if
end procedure

procedure lo(object tree, sequence more)
    if sequence(tree) then
        more &= {tree[LEFT],tree[RIGHT]}
        printf(1,"%d ",{tree[VALUE]})
    end if
    if length(more) > 0 then
        lo(more[1],more[2..$])
    end if
end procedure

procedure level_order(object tree)
    lo(tree,{})
end procedure

puts(1,"preorder:    ")
preorder(tree)
puts(1,'\n')

puts(1,"inorder:     ")
inorder(tree)
puts(1,'\n')

puts(1,"postorder:   ")
postorder(tree)
puts(1,'\n')

puts(1,"level-order: ")
level_order(tree)
puts(1,'\n')
```


Output:
 preorder:    1 2 4 7 5 3 6 8 9
 inorder:     7 4 2 5 1 8 6 9 3
 postorder:   7 4 5 2 8 9 6 3 1
 level-order: 1 2 3 4 5 6 7 8 9

=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.IO

type Tree<'a> =
   | Tree of 'a * Tree<'a> * Tree<'a>
   | Empty

let rec inorder tree =
    seq {
      match tree with
          | Tree(x, left, right) ->
               yield! inorder left
               yield x
               yield! inorder right
          | Empty -> ()
    }

let rec preorder tree =
    seq {
      match tree with
          | Tree(x, left, right) ->
               yield x
               yield! preorder left
               yield! preorder right
          | Empty -> ()
    }

let rec postorder tree =
    seq {
      match tree with
          | Tree(x, left, right) ->
               yield! postorder left
               yield! postorder right
               yield x
          | Empty -> ()
    }

let levelorder tree =
    let rec loop queue =
        seq {
            match queue with
            | [] -> ()
            | (Empty::tail) -> yield! loop tail
            | (Tree(x, l, r)::tail) ->
                yield x
                yield! loop (tail @ [l; r])
        }
    loop [tree]

[<EntryPoint>]
let main _ =
    let tree =
        Tree (1,
              Tree (2,
                    Tree (4,
                          Tree (7, Empty, Empty),
                          Empty),
                    Tree (5, Empty, Empty)),
              Tree (3,
                    Tree (6,
                          Tree (8, Empty, Empty),
                          Tree (9, Empty, Empty)),
                    Empty))

    let show x = printf "%d " x

    printf "preorder:    "
    preorder tree   |> Seq.iter show
    printf "\ninorder:     "
    inorder tree    |> Seq.iter show
    printf "\npostorder:   "
    postorder tree  |> Seq.iter show
    printf "\nlevel-order: "
    levelorder tree |> Seq.iter show
    0
```



## Factor


```factor
USING: accessors combinators deques dlists fry io kernel
math.parser ;
IN: rosetta.tree-traversal

TUPLE: node data left right ;

CONSTANT: example-tree
    T{ node f 1
        T{ node f 2
            T{ node f 4
                T{ node f 7 f f }
                f
            }
            T{ node f 5 f f }
        }
        T{ node f 3
            T{ node f 6
                T{ node f 8 f f }
                T{ node f 9 f f }
            }
            f
        }
    }

: preorder ( node quot: ( data -- ) -- )
    [ [ data>> ] dip call ]
    [ [ left>> ] dip over [ preorder ] [ 2drop ] if ]
    [ [ right>> ] dip over [ preorder ] [ 2drop ] if ]
    2tri ; inline recursive

: inorder ( node quot: ( data -- ) -- )
    [ [ left>> ] dip over [ inorder ] [ 2drop ] if ]
    [ [ data>> ] dip call ]
    [ [ right>> ] dip over [ inorder ] [ 2drop ] if ]
    2tri ; inline recursive

: postorder ( node quot: ( data -- ) -- )
    [ [ left>> ] dip over [ postorder ] [ 2drop ] if ]
    [ [ right>> ] dip over [ postorder ] [ 2drop ] if ]
    [ [ data>> ] dip call ]
    2tri ; inline recursive

: (levelorder) ( dlist quot: ( data -- ) -- )
    over deque-empty? [ 2drop ] [
        [ dup pop-front ] dip {
            [ [ data>> ] dip call drop ]
            [ drop left>> [ swap push-back ] [ drop ] if* ]
            [ drop right>> [ swap push-back ] [ drop ] if* ]
            [ nip (levelorder) ]
        } 3cleave
    ] if ; inline recursive

: levelorder ( node quot: ( data -- ) -- )
    [ 1dlist ] dip (levelorder) ; inline

: levelorder2 ( node quot: ( data -- ) -- )
    [ 1dlist ] dip
    [ dup deque-empty? not ] swap '[
        dup pop-front
        [ data>> @ ]
        [ left>> [ over push-back ] when* ]
        [ right>> [ over push-back ] when* ] tri
    ] while drop ; inline

: main ( -- )
    example-tree [ number>string write " " write ] {
        [ "preorder:    " write preorder    nl ]
        [ "inorder:     " write inorder     nl ]
        [ "postorder:   " write postorder   nl ]
        [ "levelorder:  " write levelorder  nl ]
        [ "levelorder2: " write levelorder2 nl ]
    } 2cleave ;
```



## Fantom


```fantom

class Tree
{
  readonly Int label
  readonly Tree? left
  readonly Tree? right

  new make (Int label, Tree? left := null, Tree? right := null)
  {
    this.label = label
    this.left = left
    this.right = right
  }

  Void preorder(|Int->Void| func)
  {
    func(label)
    left?.preorder(func) // ?. will not call method if 'left' is null
    right?.preorder(func)
  }

  Void postorder(|Int->Void| func)
  {
    left?.postorder(func)
    right?.postorder(func)
    func(label)
  }

  Void inorder(|Int->Void| func)
  {
    left?.inorder(func)
    func(label)
    right?.inorder(func)
  }

  Void levelorder(|Int->Void| func)
  {
    Tree[] nodes := [this]
    while (nodes.size > 0)
    {
      Tree cur := nodes.removeAt(0)
      func(cur.label)
      if (cur.left != null) nodes.add (cur.left)
      if (cur.right != null) nodes.add (cur.right)
    }
  }
}

class Main
{
  public static Void main ()
  {
    tree := Tree(1,
              Tree(2, Tree(4, Tree(7)), Tree(5)),
              Tree(3, Tree(6, Tree(8), Tree(9))))
    List result := [,]
    collect := |Int a -> Void| { result.add(a) }
    tree.preorder(collect)
    echo ("preorder:    " + result.join(" "))
    result = [,]
    tree.inorder(collect)
    echo ("inorder:     " + result.join(" "))
    result = [,]
    tree.postorder(collect)
    echo ("postorder:   " + result.join(" "))
    result = [,]
    tree.levelorder(collect)
    echo ("levelorder:  " + result.join(" "))
  }
}

```


Output:

```txt

preorder:    1 2 4 7 5 3 6 8 9
inorder:     7 4 2 5 1 8 6 9 3
postorder:   7 4 5 2 8 9 6 3 1
levelorder:  1 2 3 4 5 6 7 8 9

```



## Forth


```forth
\ binary tree (dictionary)
: node ( l r data -- node ) here >r , , , r> ;
: leaf ( data -- node ) 0 0 rot node ;

: >data  ( node -- ) @ ;
: >right ( node -- ) cell+ @ ;
: >left  ( node -- ) cell+ cell+ @ ;

: preorder ( xt tree -- )
  dup 0= if 2drop exit then
  2dup >data swap execute
  2dup >left recurse
       >right recurse ;

: inorder ( xt tree -- )
  dup 0= if 2drop exit then
  2dup >left recurse
  2dup >data swap execute
       >right recurse ;

: postorder ( xt tree -- )
  dup 0= if 2drop exit then
  2dup >left recurse
  2dup >right recurse
       >data swap execute ;

: max-depth ( tree -- n )
  dup 0= if exit then
  dup  >left recurse
  swap >right recurse max 1+ ;

defer depthaction
: depthorder ( depth tree -- )
  dup 0= if 2drop exit then
  over 0=
  if   >data depthaction drop
  else over 1- over >left  recurse
       swap 1- swap >right recurse
  then ;

: levelorder ( xt tree -- )
  swap is depthaction
  dup max-depth 0 ?do
    i over depthorder
  loop drop ;

7 leaf 0      4 node
              5 leaf 2 node
8 leaf 9 leaf 6 node
              0      3 node 1 node value tree

cr ' . tree preorder    \ 1 2 4 7 5 3 6 8 9
cr ' . tree inorder     \ 7 4 2 5 1 8 6 9 3
cr ' . tree postorder   \ 7 4 5 2 8 9 6 3 1
cr tree max-depth .     \ 4
cr ' . tree levelorder  \ 1 2 3 4 5 6 7 8 9
```



## Fortran


### Recursion? Oh dear.

For many years it has been routine to hear murmured exchanges that "Fortran is not a recursive language", which is rather odd because any computer language that allows arithmetic expressions in the usual infix notation as learnt at primary school is fundamentally recursive. Moreover, nothing in Fortran's syntax prevents recursion: routines can invoke each other or themselves without difficulty. It is the implementation that is at fault. Typically, a Fortran compiler produces code for a computer lacking an in-built stack mechanism and this became a habit. For instance, on the IBM1130, entry to a routine was via a BSI instruction, "Branch and Save IAR", which placed the return address (the value of the Instruction Address Register, IAR) at the routine's entry point and commenced execution at the following address. For the IBM360 ''et al'', the instruction was BALR, "Branch and Load Register" (I always edited listings to read BALROG, ahem) whereby the return address was loaded into a specified register. Should such a routine then invoke itself in the same manner, then the first return address ''will be overwritten'' by the new address. Only if the routine included special code to save multiple return addresses could such recursion work.

In other words, there has never been any problem with recursive invocations in Fortran, merely in organising the correct return from them. Unless you used the Burroughs Fortran compiler, which being for a computer whose hardware employed a stack mechanism, meant that it all just worked and there was no reason to prevent recursion from working. Except for a large system for the formal manipulation of mathematical expressions, whose major components repeatedly invoked each other without ever bothering to return: large jobs failed via stack overflow!

Otherwise, one can always write detailed code that gives effect to recursive usage, typically involving a variable called SP and an array called STACK. Oddly, such proceedings for the QuickSort algorithm are often declared to be "iterative", presumably because the absence of formally-declared recursive phrases blocks recognition of recursive action.

In the example source, the mainline, GORILLA, does its recursion via array twiddling and in that spirit, uses multiple lists for the "level" style traversal so that one tree clamber only need be made, whereas the recursive equivalent cheats by commanding one clamber for each level. The recursive routines store their state in part via the position within their code - that is, before, between, or after the recursive invocations, and are much easier to compare. Rather than litter the source with separate routines and their declarations for each of the four styles required, routine TARZAN has the four versions together for easy comparison, distinguished by a CASE statement. Actually, the code could be even more compact as in
```Fortran

      IF (STYLE.EQ."PRE")  CALL OUT(HAS)
      IF (LINKL(HAS).GT.0) CALL TARZAN(LINKL(HAS),STYLE)
      IF (STYLE.EQ."IN")   CALL OUT(HAS)
      IF (LINKR(HAS).GT.0) CALL TARZAN(LINKR(HAS),STYLE)
      IF (STYLE.EQ."POST") CALL OUT(HAS)
```

But that would cloud the simplicity of each separate version, and would be extra messy with the fourth option included. On the other hand, the requirements for formal recursion carry the cost of the entry/exit protocol and moreover must do so for every invocation (though there is sometimes opportunity for end-recursion to be converted into a secret "go to") - avoiding this is why every invocation of TARZAN first checks that it has a live link, rather than coding this once only within TARZAN to return immediately when invoked with a dead link - whereas the array twiddling via SP deals only with what is required and notably, avoids raising the stack if it can. Further, the GORILLA version can if necessary maintain additional information, as is needed for the postorder traversal where, not having state information stored via position in the code (as with the recursive version) it needs to know whether it is returning to a node from which it departed via the rightwards link and so is in the post-traversal state and thus due a postorder action. This could involve an auxiliary array, but here is handled by taking advantage of the sign of the STACK element. This sort of trick might still be possible even if the link values were memory addresses rather than array indices, as many computers do not use their full word size for addressing.

The tree is represented via arrays NODE, LINKL and LINKR, initialised to the set example via some DATA statements rather than being built via a sequence of calls to something like ADDNODE. Old-style Fortran would require separate arrays, though one could mess about with two-dimensional arrays if the type of NODE was compatible. F90 and later enable the definition of compound data types, so that one might speak of NODE(i).CONTENT, NODE(i).LINKLEFT, and NODE(i).LINKRIGHT, or similar. While this offers clear benefits in organisation and documentation there can be surprises, as when a binary search routine was invoked on something like NODE(1:n).KEY and the programme ran a ''lot'' slower than the multi-array version! This was because rather than present the routine with an array having a "stride" other than one, the KEY values were copied from the data aggregate to a work area so that they ''were'' contiguous for the binary search routine, thereby vitiating its speed advantage over a linear search.

Except for the usage of array MIST having an element zero and the use of an array assignment MIST(:,0) = 0, the GORILLA code is old-style Fortran. One could play tricks with EQUIVALENCE statements to arrange that an array's first element was at index zero, but that would rely on the absence of array bound checking and is more difficult with multi-dimensional arrays. Instead, one would make do either by having a separate list length variable, or else remembering the offsets... The MODULE usage requires F90 or later and provides a convenient protocol for global data, otherwise one must mess about with COMMON or parameter hordes. If that were done, the B6700 compiler would have handled it. But for the benefit of trembling modern compilers it also contains the fearsome new attribute, RECURSIVE, to flog the compilers into what was formalised for Algol in 1960 and was available ''for free'' via Burroughs in the 1970s.

On the other hand, the early-style Fortran DO-loop would always execute once, because the test was made only at the end of an iteration, and here, routine JANE does not know the value of MAXLEVEL until ''after'' the first iteration. Code such as
```Fortran

      DO GASP = 1,MAXLEVEL
        CALL TARZAN(1,HOW)
      END DO
```

Would not work with modern Fortran, because the usual approach is to calculate the iteration count from the DO-loop parameters at the ''start'' of the DO-loop, and possibly not execute it at all if that count is not positive. This also means that with each iteration, the count must be decremented ''and'' the index variable adjusted; extra effort. There is no equivalent of Pascal's <code>Repeat ... until ''condition'';</code>, so, in place of a nice "structured" statement with clear interpretation, there is some messy code with a label and a GO TO, oh dear.


### Source


```Fortran

      MODULE ARAUCARIA	!Cunning crosswords, also.
       INTEGER ENUFF		!To suit the set example.
       PARAMETER (ENUFF = 9)	!This will do.
       INTEGER NODE(ENUFF),LINKL(ENUFF),LINKR(ENUFF)	!The nodes, and their links.
       DATA NODE/ 1,2,3,4,5,6,7,8,9/	!Value = index. A rather boring payload.
       DATA LINKL/2,4,6,7,0,8,0,0,0/	!"Left" and "Right" are as looking at the page.
       DATA LINKR/3,5,0,0,0,9,0,0,0/	!If one thinks within the tree, they're the other way around!
C              1	!Thus, looking from the "1", to the right is "2" and to the left is "3".
C             / \	!But, looking at the scheme, to the left is "2" and to the right is "3".
C            /   \	!This latter seems to be the popular view from the outside, not within the data.
C           /     \	!Similarily, although called a "tree", the depiction is upside down!
C          2       3	!How can computers be expected to keep up with this contrariness?
C         / \     /	!Humm, no example of a rightwards link with no leftwards link.
C        4   5   6	!Topologically equivalent, but not so in usage.
C       /       / \
C      7       8   9
       INTEGER N,LIST(ENUFF)	!This is to be developed.
       INTEGER LEVEL,MAXLEVEL	!While these vary in various ways.
       INTEGER GASP		!Communication from JANE.
       CONTAINS	!No checks for invalid links, etc.
        SUBROUTINE OUT(IS)	!Append a value to a list.
         INTEGER IS		!The value.
          N = N + 1		!The list's count so far.
          LIST(N) = IS		!Place.
        END SUBROUTINE OUT	!Eventually, the list can be written in one go.

        RECURSIVE SUBROUTINE TARZAN(HAS,STYLE)	!Skilled at tree traversal, is he.
         INTEGER HAS		!The current position.
         CHARACTER*(*) STYLE	!Traversal type.
          LEVEL = LEVEL + 1	!A leap is made.
          IF (LEVEL.GT.MAXLEVEL) MAXLEVEL = LEVEL	!Staring at the moon.
          SELECT CASE(STYLE)	!And, in what manner?
           CASE ("PRE")		!Declare the position first.
            CALL OUT(HAS)	!Thus.
            IF (LINKL(HAS).GT.0) CALL TARZAN(LINKL(HAS),STYLE)
            IF (LINKR(HAS).GT.0) CALL TARZAN(LINKR(HAS),STYLE)
           CASE ("IN")		!Or in the middle.
            IF (LINKL(HAS).GT.0) CALL TARZAN(LINKL(HAS),STYLE)
            CALL OUT(HAS)	!Thus.
            IF (LINKR(HAS).GT.0) CALL TARZAN(LINKR(HAS),STYLE)
           CASE ("POST")	!Or at the end.
            IF (LINKL(HAS).GT.0) CALL TARZAN(LINKL(HAS),STYLE)
            IF (LINKR(HAS).GT.0) CALL TARZAN(LINKR(HAS),STYLE)
            CALL OUT(HAS)	!Thus.
           CASE ("LEVEL")	!Or at specified levels.
            IF (LEVEL.EQ.GASP) CALL OUT(HAS)	!Such as this?
            IF (LINKL(HAS).GT.0) CALL TARZAN(LINKL(HAS),STYLE)
            IF (LINKR(HAS).GT.0) CALL TARZAN(LINKR(HAS),STYLE)
           CASE DEFAULT		!This shouldn't happen.
            WRITE (6,*) "Unknown style ",STYLE	!But, paranoia.
            STOP "No can do!"		!Rather than flounder about.
          END SELECT		!That was simple.
          LEVEL = LEVEL - 1	!Sag back.
        END SUBROUTINE TARZAN	!Not like George of the Jungle.

        SUBROUTINE JANE(HOW)	!Tells Tarzan what to do.
         CHARACTER*(*) HOW	!A single word suffices.
          N = 0			!No positions trampled.
          LEVEL = 0		!Starting on the ground.
          MAXLEVEL = 0		!The ascent follows.
          IF (HOW.NE."LEVEL") THEN	!Ordinary styles?
            CALL TARZAN(1,HOW)		!Yes. From the root, go...
           ELSE			!But this is not tree-structured.
            GASP = 0		!Instead, we ascend through the canopy in stages.
    1       GASP = GASP + 1		!Up one stage.
            CALL TARZAN(1,HOW)		!And do it all again.
            IF (GASP.LT.MAXLEVEL) GO TO 1	!Are we there yet?
          END IF		!Don't know MAXLEVEL until after the first clamber.
Cast forth the list.
          WRITE (6,10) HOW,NODE(LIST(1:N))	!Show spoor.
   10     FORMAT (A6,"-order:",66(1X,I0))	!Large enough.
          WRITE (6,*)				!Sigh.
        END SUBROUTINE JANE	!That was simple.
      END MODULE ARAUCARIA	!The monkeys are puzzled.

      PROGRAM GORILLA		!No fancy stuff. Just brute force.
      USE ARAUCARIA		!This is for lightweight but cunning monkeys.
      INTEGER IT		!A finger.
      INTEGER SP,STACK(ENUFF)	!The tree may be slim.
      INTEGER SLEVL(ENUFF)	!So prepare for maximum usage.
      INTEGER MIST(ENUFF,0:ENUFF)	!Multiple lists.

Chase the links preorder style: name the node, delve its left link, delve its right link.
      N = 0	!No nodes have been visited.
      SP = 0	!My stack is empty.
      IT = 1	!I start at the root.
   10 N = N + 1			!Another node arrived at.
      LIST(N) = IT		!Finger it.
      IF (LINKL(IT).GT.0) THEN	!A left link?
        IF (LINKR(IT).GT.0) THEN	!Yes. A right link also?
          SP = SP + 1				!Yes. Stack it up.
          STACK(SP) = LINKR(IT)			!For later investigation.
        END IF				!So much for the right link.
        IT = LINKL(IT)			!Fingered by the left link.
        GO TO 10			!See what happens.
      END IF			!But if there is no left link,
      IF (LINKR(IT).GT.0) THEN	!There still might be a right link.
        IT = LINKR(IT)			!There is.
        GO TO 10			!See what happens.
      END IF			!And if there are no links,
      IF (SP.GT.0) THEN		!Perhaps the stack has bottomed out too?
        IT = STACK(SP)			!No, this was deferred.
        SP = SP - 1			!So, pick up where we left off.
        GO TO 10			!And carry on.
      END IF			!So much for unstacking.
      WRITE (6,12) "Preorder",NODE(LIST(1:N))	!I've got a little list!
   12 FORMAT (A12,":",66(1X,I0))
      CALL JANE("PRE")		!Try it fancy style.

Chase the links inorder style: delve left fully, name the node and try its right, then unstack.
      N = 0	!No nodes have been visited.
      SP = 0	!My stack is empty.
      IT = 1	!I start at the root.
   20 SP = SP + 1		!I'm on the way down.
      STACK(SP) = IT		!So, save this position to later retreat to.
      IF (LINKL(IT).GT.0) THEN	!Can I delve further left?
        IT = LINKL(IT)			!Yes.
        GO TO 20			!And see what happens.
      END IF			!So much for diving.
   21 IF (SP.GT.0) THEN	!Can I retreat?
        IT = STACK(SP)		!Yes.
        SP = SP - 1		!Go back to whence I had delved left.
        N = N + 1		!This now counts as a place in order.
        LIST(N) = IT		!So list it.
        IF (LINKR(IT).GT.0) THEN!Have I a rightwards path?
          IT = LINKR(IT)		!Yes. Take it.
          GO TO 20			!And delve therefrom.
        END IF			!This node is now finished with.
        GO TO 21		!So, try for another retreat.
      END IF		!So much for unstacking.
      WRITE (6,12) "Inorder",NODE(LIST(1:N))	!I've got a little list!
      CALL JANE("IN")	!Try with more style.

Chase the links postorder style: delve left fully, delve right, name the node, then unstack.
      N = 0	!No nodes have been visited.
      SP = 0	!My stack is empty.
      IT = 1	!I start at the root.
   30 SP = SP + 1	!Action follows delving,
      STACK(SP) = IT	!So this node will be returned to.
      IF (LINKL(IT).GT.0) THEN	!Take any leftwards link straightaway.
        IT = LINKL(IT)		!Thus.
        GO TO 30		!Thanks to the stack, we'll return to IT (as was).
      END IF		!But if there is no leftwards link to follow,
      IF (LINKR(IT).GT.0) THEN	!Perhaps there is a rightwards one?
        STACK(SP) = -STACK(SP)	!=-IT Mark the stacked finger as a rightwards lurch!
        IT = LINKR(IT)		!The rightwards link is now to be taken.
        GO TO 30		!Thus start on a sub-tree.
      END IF		!But if there is no rightwards link either,
  31  IF (SP.GT.0) THEN	!See if there is anywhere to retreat to.
        IT = STACK(SP)		!The same IT placed at 30 if we dropped into 31.
        SP = SP - 1		!But now we're in a different mood.
        IF (IT.LT.0) THEN	!Returning to what had been a rightwards departure?
          N = N + 1			!Yes! Then this node is post-interest.
          LIST(N) = -IT			!So, time to roll it forth at last.
          GO TO 31			!And retreat some more.
        END IF			!But if we hadn't gone right from IT,
        IF (LINKR(IT).LE.0) THEN!We had gone left.
          N = N + 1			!And now there is nowhere rightwards.
          LIST(N) = IT			!So this node is post-interest.
          GO TO 31			!And retreat some more.
        END IF			!But if there is a rightwards leap,
        SP = SP + 1			!Prepare to return to it,
        STACK(SP) = -IT			!Marked as having gone rightwards.
        IT = LINKR(IT)			!The rightwards move.
        GO TO 30			!Peruse a fresh sub-tree.
      END IF			!And if the stack is reduced,
      WRITE (6,12) "Postorder",NODE(LIST(1:N))	!Results!
      CALL JANE("POST")		!The same again?

Chase the nodes level style.
      SP = 0		!My stack is empty.
      IT = 1		!I start at the root.
      LEVEL = 0		!On the ground.
      MAXLEVEL = 0	!No ascent as yet.
      MIST(:,0) = 0	!At all levels, nothing.
   40 LEVEL = LEVEL + 1			!Every arrival is one level up.
      IF (LEVEL.GT.MAXLEVEL) MAXLEVEL = LEVEL	!Note the most high.
      MIST(LEVEL,0) = MIST(LEVEL,0) + 1	!The count at that level.
      MIST(LEVEL,MIST(LEVEL,0)) = IT	!Add to the level's list.
      IF (LINKL(IT).GT.0) THEN		!Righto, can we go left?
        IF (LINKR(IT).GT.0) THEN	!Yes. Rightwards as well?
          SP = SP + 1				!Yes! This will have to wait.
          STACK(SP) = LINKR(IT)			!So remember it,
          SLEVL(SP) = LEVEL			!And what level we're at now.
        END IF				!I can only go one way at a time.
        IT = LINKL(IT)			!Accept the fingered leftwards lurch.
        GO TO 40			!Go to IT.
      END IF			!But if there is no leftwards link,
      IF (LINKR(IT).GT.0) THEN	!Perhaps there is a rightwards one?
        IT = LINKR(IT)			!There is.
        GO TO 40			!Go to IT.
      END IF			!And if there are no further links,
      IF (SP.GT.0) THEN		!Perhaps we can retreat to what was deferred.
        IT = STACK(SP)			!The finger.
        LEVEL = SLEVL(SP)		!The level.
        SP = SP - 1			!Wind back the stack.
        GO TO 40			!Go to IT.
      END IF			!So much for the stack.
      WRITE (6,12) "Levelorder",	!Roll the lists in ascending LEVEL order.
     1 (NODE(MIST(LEVEL,1:MIST(LEVEL,0))), LEVEL = 1,MAXLEVEL)
      CALL JANE("LEVEL")	!Alternatively...
      END	!So much for that.

```


### Output

Alternately GORILLA-style, and JANE-style:

```txt

    Preorder: 1 2 4 7 5 3 6 8 9
   PRE-order: 1 2 4 7 5 3 6 8 9

     Inorder: 7 4 2 5 1 8 6 9 3
    IN-order: 7 4 2 5 1 8 6 9 3

   Postorder: 7 4 5 2 8 9 6 3 1
  POST-order: 7 4 5 2 8 9 6 3 1

  Levelorder: 1 2 3 4 5 6 7 8 9
 LEVEL-order: 1 2 3 4 5 6 7 8 9

```



## FunL

{{trans|Haskell}}

```funl
data Tree = Empty | Node( value, left, right )

def
  preorder( Empty )          =  []
  preorder( Node(v, l, r) )  =  [v] + preorder( l ) + preorder( r )

  inorder( Empty )           =  []
  inorder( Node(v, l, r) )   =  inorder( l ) + [v] + inorder( r )

  postorder( Empty )         =  []
  postorder( Node(v, l, r) ) =  postorder( l ) + postorder( r ) + [v]

  levelorder( x ) =
    def
      order( [] )                 =  []
      order( Empty         : xs ) =  order( xs )
      order( Node(v, l, r) : xs ) =  v : order( xs + [l, r] )

    order( [x] )

tree =    Node( 1,
            Node( 2,
              Node( 4,
                Node( 7, Empty, Empty ),
                Empty ),
              Node( 5, Empty, Empty ) ),
            Node( 3,
              Node( 6,
                Node( 8, Empty, Empty ),
                Node( 9, Empty, Empty ) ),
              Empty ) )

println( preorder(tree) )
println( inorder(tree) )
println( postorder(tree) )
println( levelorder(tree) )
```


{{out}}


```txt

[1, 2, 4, 7, 5, 3, 6, 8, 9]
[7, 4, 2, 5, 1, 8, 6, 9, 3]
[7, 4, 5, 2, 8, 9, 6, 3, 1]
[1, 2, 3, 4, 5, 6, 7, 8, 9]

```



## GFA Basic


<lang>
maxnodes%=100 ! set a limit to size of tree
content%=0 ! index of content field
left%=1 ! index of left tree
right%=2 ! index of right tree
DIM tree%(maxnodes%,3) ! create space for tree
'
OPENW 1
CLEARW 1
'
@create_tree
PRINT "Preorder:   ";
@preorder_traversal(1)
PRINT ""
PRINT "Inorder:    ";
@inorder_traversal(1)
PRINT ""
PRINT "Postorder:  ";
@postorder_traversal(1)
PRINT ""
PRINT "Levelorder: ";
@levelorder_traversal(1)
PRINT ""
'
~INP(2)
CLOSEW 1
'
' Define the example tree
'
PROCEDURE create_tree
  tree%(1,content%)=1
  tree%(1,left%)=2
  tree%(1,right%)=3
  tree%(2,content%)=2
  tree%(2,left%)=4
  tree%(2,right%)=5
  tree%(3,content%)=3
  tree%(3,left%)=6
  tree%(3,right%)=0 ! 0 is used for no subtree
  tree%(4,content%)=4
  tree%(4,left%)=7
  tree%(4,right%)=0
  tree%(5,content%)=5
  tree%(5,left%)=0
  tree%(5,right%)=0
  tree%(6,content%)=6
  tree%(6,left%)=8
  tree%(6,right%)=9
  tree%(7,content%)=7
  tree%(7,left%)=0
  tree%(7,right%)=0
  tree%(8,content%)=8
  tree%(8,left%)=0
  tree%(8,right%)=0
  tree%(9,content%)=9
  tree%(9,left%)=0
  tree%(9,right%)=0
RETURN
'
' Preorder traversal from given node
'
PROCEDURE preorder_traversal(node%)
  IF node%<>0 ! 0 means there is no node
    PRINT tree%(node%,content%);
    preorder_traversal(tree%(node%,left%))
    preorder_traversal(tree%(node%,right%))
  ENDIF
RETURN
'
' Postorder traversal from given node
'
PROCEDURE postorder_traversal(node%)
  IF node%<>0 ! 0 means there is no node
    postorder_traversal(tree%(node%,left%))
    postorder_traversal(tree%(node%,right%))
    PRINT tree%(node%,content%);
  ENDIF
RETURN
'
' Inorder traversal from given node
'
PROCEDURE inorder_traversal(node%)
  IF node%<>0 ! 0 means there is no node
    inorder_traversal(tree%(node%,left%))
    PRINT tree%(node%,content%);
    inorder_traversal(tree%(node%,right%))
  ENDIF
RETURN
'
' Level order traversal from given node
'
PROCEDURE levelorder_traversal(node%)
  LOCAL nodes%,first_free%,current%
  '
  ' Set up initial queue of nodes
  '
  DIM nodes%(maxnodes%) ! some working space to store queue of nodes
  current%=1
  nodes%(current%)=node%
  first_free%=current%+1
  '
  WHILE nodes%(current%)<>0
    ' add the children of current node onto queue
    IF tree%(nodes%(current%),left%)<>0
      nodes%(first_free%)=tree%(nodes%(current%),left%)
      first_free%=first_free%+1
    ENDIF
    IF tree%(nodes%(current%),right%)<>0
      nodes%(first_free%)=tree%(nodes%(current%),right%)
      first_free%=first_free%+1
    ENDIF
    ' print the current node content
    PRINT tree%(nodes%(current%),content%);
    ' advance to next node
    current%=current%+1
  WEND
RETURN

```



## Go


### Individually allocated nodes

{{trans|C}}
This is like many examples on this page.

```go
package main

import "fmt"

type node struct {
    value       int
    left, right *node
}

func (n *node) iterPreorder(visit func(int)) {
    if n == nil {
        return
    }
    visit(n.value)
    n.left.iterPreorder(visit)
    n.right.iterPreorder(visit)
}

func (n *node) iterInorder(visit func(int)) {
    if n == nil {
        return
    }
    n.left.iterInorder(visit)
    visit(n.value)
    n.right.iterInorder(visit)
}

func (n *node) iterPostorder(visit func(int)) {
    if n == nil {
        return
    }
    n.left.iterPostorder(visit)
    n.right.iterPostorder(visit)
    visit(n.value)
}

func (n *node) iterLevelorder(visit func(int)) {
    if n == nil {
        return
    }
    for queue := []*node{n}; ; {
        n = queue[0]
        visit(n.value)
        copy(queue, queue[1:])
        queue = queue[:len(queue)-1]
        if n.left != nil {
            queue = append(queue, n.left)
        }
        if n.right != nil {
            queue = append(queue, n.right)
        }
        if len(queue) == 0 {
            return
        }
    }
}

func main() {
    tree := &node{1,
        &node{2,
            &node{4,
                &node{7, nil, nil},
                nil},
            &node{5, nil, nil}},
        &node{3,
            &node{6,
                &node{8, nil, nil},
                &node{9, nil, nil}},
            nil}}
    fmt.Print("preorder:    ")
    tree.iterPreorder(visitor)
    fmt.Println()
    fmt.Print("inorder:     ")
    tree.iterInorder(visitor)
    fmt.Println()
    fmt.Print("postorder:   ")
    tree.iterPostorder(visitor)
    fmt.Println()
    fmt.Print("level-order: ")
    tree.iterLevelorder(visitor)
    fmt.Println()
}

func visitor(value int) {
    fmt.Print(value, " ")
}
```

{{out}}

```txt

preorder:    1 2 4 7 5 3 6 8 9
inorder:     7 4 2 5 1 8 6 9 3
postorder:   7 4 5 2 8 9 6 3 1
level-order: 1 2 3 4 5 6 7 8 9

```


### Flat slice

Alternative representation.  Like Wikipedia [http://en.wikipedia.org/wiki/Binary_tree#Arrays Binary tree#Arrays]

```go
package main

import "fmt"

// flat, level-order representation.
// for node at index k, left child has index 2k, right child has index 2k+1.
// a value of -1 means the node does not exist.
type tree []int

func main() {
    t := tree{1, 2, 3, 4, 5, 6, -1, 7, -1, -1, -1, 8, 9}
    visitor := func(n int) {
        fmt.Print(n, " ")
    }
    fmt.Print("preorder:    ")
    t.iterPreorder(visitor)
    fmt.Print("\ninorder:     ")
    t.iterInorder(visitor)
    fmt.Print("\npostorder:   ")
    t.iterPostorder(visitor)
    fmt.Print("\nlevel-order: ")
    t.iterLevelorder(visitor)
    fmt.Println()
}

func (t tree) iterPreorder(visit func(int)) {
    var traverse func(int)
    traverse = func(k int) {
        if k >= len(t) || t[k] == -1 {
            return
        }
        visit(t[k])
        traverse(2*k + 1)
        traverse(2*k + 2)
    }
    traverse(0)
}

func (t tree) iterInorder(visit func(int)) {
    var traverse func(int)
    traverse = func(k int) {
        if k >= len(t) || t[k] == -1 {
            return
        }
        traverse(2*k + 1)
        visit(t[k])
        traverse(2*k + 2)
    }
    traverse(0)
}

func (t tree) iterPostorder(visit func(int)) {
    var traverse func(int)
    traverse = func(k int) {
        if k >= len(t) || t[k] == -1 {
            return
        }
        traverse(2*k + 1)
        traverse(2*k + 2)
        visit(t[k])
    }
    traverse(0)
}

func (t tree) iterLevelorder(visit func(int)) {
    for _, n := range t {
        if n != -1 {
            visit(n)
        }
    }
}
```



## Groovy

Uses Groovy '''Node''' and '''NodeBuilder''' classes

```groovy
def preorder;
preorder = { Node node ->
    ([node] + node.children().collect { preorder(it) }).flatten()
}

def postorder;
postorder = { Node node ->
    (node.children().collect { postorder(it) } + [node]).flatten()
}

def inorder;
inorder = { Node node ->
    def kids = node.children()
    if (kids.empty) [node]
    else if (kids.size() == 1 &&  kids[0].'@right') [node] + inorder(kids[0])
    else inorder(kids[0]) + [node] + (kids.size()>1 ? inorder(kids[1]) : [])
}

def levelorder = { Node node ->
    def nodeList = []
    def level = [node]
    while (!level.empty) {
        nodeList += level
        def nextLevel = level.collect { it.children() }.flatten()
        level = nextLevel
    }
    nodeList
}

class BinaryNodeBuilder extends NodeBuilder {
    protected Object postNodeCompletion(Object parent, Object node) {
        assert node.children().size() < 3
        node
    }
}
```


Verify that '''BinaryNodeBuilder''' will not allow a node to have more than 2 children

```groovy
try {
    new BinaryNodeBuilder().'1' {
        a {}
        b {}
        c {}
    }
    println 'not limited to binary tree\r\n'
} catch (org.codehaus.groovy.transform.powerassert.PowerAssertionError e) {
    println 'limited to binary tree\r\n'
}
```


Test case #1 (from the task definition)

```groovy
//               1
//           /       \
//       2               3
//     /   \           /
//   4       5       6
//  /               / \
// 7               8   9
def tree1 = new BinaryNodeBuilder().
'1' {
    '2' {
        '4' { '7' {} }
        '5' {}
    }
    '3' {
        '6' { '8' {}; '9' {} }
    }
}
```


Test case #2 (tests single right child)

```groovy
//               1
//           /       \
//       2               3
//     /   \           /
//   4       5       6
//    \             / \
//     7           8   9
def tree2 = new BinaryNodeBuilder().
'1' {
    '2' {
        '4' { '7'(right:true) {} }
        '5' {}
    }
    '3' {
        '6' { '8' {}; '9' {} }
    }
}
```


Run tests:

```groovy
def test = { tree ->
    println "preorder:    ${preorder(tree).collect{it.name()}}"
    println "preorder:    ${tree.depthFirst().collect{it.name()}}"

    println "postorder:   ${postorder(tree).collect{it.name()}}"

    println "inorder:     ${inorder(tree).collect{it.name()}}"

    println "level-order: ${levelorder(tree).collect{it.name()}}"
    println "level-order: ${tree.breadthFirst().collect{it.name()}}"

    println()
}
test(tree1)
test(tree2)
```


Output:

```txt
limited to binary tree

preorder:    [1, 2, 4, 7, 5, 3, 6, 8, 9]
preorder:    [1, 2, 4, 7, 5, 3, 6, 8, 9]
postorder:   [7, 4, 5, 2, 8, 9, 6, 3, 1]
inorder:     [7, 4, 2, 5, 1, 8, 6, 9, 3]
level-order: [1, 2, 3, 4, 5, 6, 7, 8, 9]
level-order: [1, 2, 3, 4, 5, 6, 7, 8, 9]

preorder:    [1, 2, 4, 7, 5, 3, 6, 8, 9]
preorder:    [1, 2, 4, 7, 5, 3, 6, 8, 9]
postorder:   [7, 4, 5, 2, 8, 9, 6, 3, 1]
inorder:     [4, 7, 2, 5, 1, 8, 6, 9, 3]
level-order: [1, 2, 3, 4, 5, 6, 7, 8, 9]
level-order: [1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Haskell


```haskell
data Tree a
  = Empty
  | Node { value :: a
         , left :: Tree a
         , right :: Tree a}

preorder, inorder, postorder, levelorder :: Tree a -> [a]
preorder Empty = []
preorder (Node v l r) = v : preorder l ++ preorder r

inorder Empty = []
inorder (Node v l r) = inorder l ++ (v : inorder r)

postorder Empty = []
postorder (Node v l r) = postorder l ++ postorder r ++ [v]

levelorder x = loop [x]
  where
    loop [] = []
    loop (Empty:xs) = loop xs
    loop (Node v l r:xs) = v : loop (xs ++ [l, r])

-- TEST --------------------------------------------------------------
tree :: Tree Int
tree =
  Node
    1
    (Node 2 (Node 4 (Node 7 Empty Empty) Empty) (Node 5 Empty Empty))
    (Node 3 (Node 6 (Node 8 Empty Empty) (Node 9 Empty Empty)) Empty)

asciiTree :: String
asciiTree =
  unlines
    [ "         1"
    , "        / \\"
    , "       /   \\"
    , "      /     \\"
    , "     2       3"
    , "    / \\     /"
    , "   4   5   6"
    , "  /       / \\"
    , " 7       8   9"
    ]

-- OUTPUT --------------------------------------------------------------
main :: IO ()
main = do
  putStrLn asciiTree
  mapM_ putStrLn $
    zipWith
      (\s xs -> justifyLeft 14 ' ' (s ++ ":") ++ unwords (show <$> xs))
      ["preorder", "inorder", "postorder", "level-order"]
      ([preorder, inorder, postorder, levelorder] <*> [tree])
  where
    justifyLeft n c s = take n (s ++ replicate n c)
```

{{Out}}

```txt
         1
        / \
       /   \
      /     \
     2       3
    / \     /
   4   5   6
  /       / \
 7       8   9

preorder:     1 2 4 7 5 3 6 8 9
inorder:      7 4 2 5 1 8 6 9 3
postorder:    7 4 5 2 8 9 6 3 1
level-order:  1 2 3 4 5 6 7 8 9
```



Or, writing the first three traversals in terms of '''foldTree''', and the last as an iteration of bind ('''>>=''') over sub-trees:

{{Trans|Python}}

```haskell
import Data.Tree (Tree(..))

preorder :: a -> [[a]] -> [a]
preorder x xs = x : concat xs

inorder :: a -> [[a]] -> [a]
inorder x [] = [x]
inorder x (y:xs) = y ++ [x] ++ concat xs

postorder :: a -> [[a]] -> [a]
postorder x xs = concat xs ++ [x]

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go
  where
    go (Node x ts) = f x (go <$> ts)

levelOrder :: Tree a -> [a]
levelOrder x =
  takeWhile (not . null) (iterate (concatMap subForest) [x]) >>= fmap rootLabel

-- TEST -------------------------------------------------
tree :: Tree Int
tree =
  Node
    1
    [ Node 2 [Node 4 [Node 7 []], Node 5 []]
    , Node 3 [Node 6 [Node 8 [], Node 9 []]]
    ]

main :: IO ()
main = do
  mapM_ print ([foldTree] <*> [preorder, inorder, postorder] <*> [tree])
  print $ levelOrder tree
```


```txt
[1,2,4,7,5,3,6,8,9]
[7,4,2,5,1,8,6,9,3]
[7,4,5,2,8,9,6,3,1]
[1,2,3,4,5,6,7,8,9]
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
    bTree := [1, [2, [4, [7]], [5]], [3, [6, [8], [9]]]]
    showTree(bTree, preorder|inorder|postorder|levelorder)
end

procedure showTree(tree, f)
    writes(image(f),":\t")
    every writes(" ",f(tree)[1])
    write()
end

procedure preorder(L)
    if \L then suspend L | preorder(L[2|3])
end

procedure inorder(L)
    if \L then suspend inorder(L[2]) | L | inorder(L[3])
end

procedure postorder(L)
    if \L then suspend postorder(L[2|3]) | L
end

procedure levelorder(L)
    if \L then {
        queue := [L]
        while nextnode := get(queue) do {
            every put(queue, \nextnode[2|3])
            suspend nextnode
            }
        }
end
```


Output:


```txt
->bintree
procedure preorder:      1 2 4 7 5 3 6 8 9
procedure inorder:       7 4 2 5 1 8 6 9 3
procedure postorder:     7 4 5 2 8 9 6 3 1
procedure levelorder:    1 2 3 4 5 6 7 8 9
->
```



## J



```J
preorder=: ]S:0
postorder=: ([:; postorder&.>@}.) , >@{.
levelorder=: ;@({::L:1 _~ [: (/: #@>) <S:1@{::)
inorder=: ([:; inorder&.>@(''"_`(1&{)@.(1<#))) , >@{. , [:; inorder&.>@}.@}.
```


Required example:


```J
N2=: conjunction def '(<m),(<n),<y'
N1=: adverb def '(<m),<y'
L=: adverb def '<m'

tree=: 1 N2 (2 N2 (4 N1 (7 L)) 5 L) 3 N1 6 N2 (8 L) 9 L
```


This tree is organized in a pre-order fashion


```J
    preorder tree
1 2 4 7 5 3 6 8 9
```


post-order is not that much different from pre-order, except that the children must extracted before the parent.


```J
    postorder tree
7 4 5 2 8 9 6 3 1
```


Implementing in-order is more complex because we must sometimes test whether we have any leaves, instead of relying on J's implicit looping over lists


```J
    inorder tree
7 4 2 5 1 8 6 9 3
```


level-order can be accomplished by constructing a map of the locations of the leaves, sorting these map locations by their non-leaf indices and using the result to extract all leaves from the tree.  Elements at the same level with the same parent will have the same sort keys and thus be extracted in preorder fashion, which works just fine.


```J
    levelorder tree
1 2 3 4 5 6 7 8 9
```



For J novices, here's the tree instance with a few redundant parenthesis:


```J
    tree=: 1 N2 (2 N2 (4 N1 (7 L)) (5 L)) (3 N1 (6 N2 (8 L) (9 L)))
```


Syntactically, N2 is a binary node expressed as <code>m N2 n y</code>.  N1 is a node with a single child, expressed as <code>m N2 y</code>.  L is a leaf node, expressed as <code>m L</code>.  In all three cases, the parent value (<code>m</code>) for the node appears on the left, and the child tree(s) appear on the right.  (And <code>n</code> must be parenthesized if it is not a single word.)


###  J: Alternate implementation


Of course, there are other ways of representing tree structures in J. One fairly natural approach pairs a list of data with a matching list of parent indices. For example:


```J
example=:1 8 3 4 7 5 9 6 2,: 0 7 0 8 3 8 7 2 0
```


Here, we have two possible ways of identifying the root node. It can be in a known place in the list (index 0, for this example). But it is also the only node which is its own parent. For this task we'll use the more general (and thus slower) approach which allows us to place the root node anywhere in the sequence.

Next, let's define a few utilities:


```J
depth=: +/@((~: , (~: i.@#@{.)~) {:@,)@({~^:a:)

reorder=:4 :0
  'data parent'=. y
  data1=. x{data
  parent1=. x{data1 i. parent{data
  if. 0=L.y do. data1,:parent1 else. data1;parent1 end.
)

data=:3 :'data[''data parent''=. y'
parent=:3 :'parent[''data parent''=. y'

childinds=: [: <:@(2&{.@-.&> #\) (</. #\)`(]~.)`(a:"0)}~
```


Here, <code>data</code>  extracts the list of data items from the tree and <code>parent</code> extracts the structure from the tree.

<code>depth</code> examines the parent structure and returns the distance of each node from the root.

<code>reorder</code> is like indexing, except that it returns an equivalent tree (with the structural elements updated to maintain the original tree structure). The left argument for reorder should select the entire tree. Selecting partial trees is a more complex problem which needs specifications about how to deal with issues such as dangling roots and multiple roots.  (Our abstraction here has no problem ''representing'' trees with multiple roots, but they are not relevant to this task.)

<code>childinds</code> extracts the child pointers which some of these results assume. This implementation assumes we are working with a binary tree (which is an explicit requirement of this task -- the parent node representation is far more general and can represent trees with any number of children at each node, but what would an "inorder" traversal look like with a trinary tree?).

Next, we define our "traversal" routines (actually, we are going a bit overboard here - we really only need to extract the data for this tasks's concept of traversal):


```J
dataorder=: /:@data reorder ]
levelorder=: /:@depth@parent reorder ]

inorder=: inperm@parent reorder ]
inperm=:3 :0
  chil=. childinds y
  node=. {.I.(= i.@#) y
  todo=. i.0 2
  r=. i.0
  whilst. (#todo)+.0<:node do.
    if. 0 <: node do.
      if. 0 <: {.ch=. node{chil do.
        todo=. todo, node,{:ch
        node=. {.ch
      else.
        r=. r, node
        node=. _1 end.
    else.
      r=. r, {.ch=. {: todo
      todo=. }: todo
      node=. {:ch end. end.
  r
)

postorder=: postperm@parent reorder ]
postperm=:3 :0
  chil=. 0,1+childinds y
  todo=. 1+I.(= i.@#) y
  r=. i.0
  whilst. (#todo) do.
    node=. {: todo
    todo=. }: todo
    if. 0 < node do.
      if. #ch=. (node{chil)-.0 do.
        todo=. todo,(-node),|.ch
      else.
        r=. r, <:node end.
    else.
      r=. r, <:|node  end. end.
)

preorder=: preperm@parent reorder ]
preperm=:3 :0
  chil=. childinds y
  todo=. I.(= i.@#) y
  r=. i.0
  whilst. (#todo) do.
    r=. r,node=. {: todo
    todo=. }: todo
    if. #ch=. (node{chil)-._1 do.
      todo=. todo,|.ch end. end.
  r
)
```


These routines assume that children of a node are arranged so that the lower index appears to the left of the higher index. If instead we wanted to rely on the ordering of their values, we could first use <code>dataorder</code> to enforce the assumption that child indexes are ordered properly.

Example use:


```J
   levelorder dataorder example
1 2 3 4 5 6 7 8 9
0 0 0 1 1 2 3 5 5
   inorder dataorder example
7 4 2 5 1 8 6 9 3
1 2 4 2 4 6 8 6 4
   preorder dataorder example
1 2 4 7 5 3 6 8 9
0 0 1 2 1 0 5 6 6
   postorder dataorder example
7 4 5 2 8 9 6 3 1
1 3 3 8 6 6 7 8 8
```


(Once again, all we really need for this task is the first row of those results - the part that represents data.)


## Java

{{works with|Java|1.5+}}

```java5
import java.util.*;

public class TreeTraversal {

        static class Node<T> {
		T value;
		Node<T> left;
		Node<T> right;

		Node(T value) {
			this.value = value;
		}

		void visit() {
			System.out.print(this.value + " ");
		}
	}

	static enum ORDER {
		PREORDER, INORDER, POSTORDER, LEVEL
	}

        static <T> void traverse(Node<T> node, ORDER order) {
		if (node == null) {
			return;
		}
		switch (order) {
		case PREORDER:
			node.visit();
			traverse(node.left, order);
			traverse(node.right, order);
			break;
		case INORDER:
			traverse(node.left, order);
			node.visit();
			traverse(node.right, order);
			break;
		case POSTORDER:
			traverse(node.left, order);
			traverse(node.right, order);
			node.visit();
			break;
		case LEVEL:
			Queue<Node<T>> queue = new LinkedList<>();
			queue.add(node);
			while(!queue.isEmpty()){
				Node<T> next = queue.remove();
				next.visit();
				if(next.left!=null)
					queue.add(next.left);
				if(next.right!=null)
					queue.add(next.right);
			}
		}
	}

	public static void main(String[] args) {

		Node<Integer> one = new Node<Integer>(1);
		Node<Integer> two = new Node<Integer>(2);
		Node<Integer> three = new Node<Integer>(3);
		Node<Integer> four = new Node<Integer>(4);
		Node<Integer> five = new Node<Integer>(5);
		Node<Integer> six = new Node<Integer>(6);
		Node<Integer> seven = new Node<Integer>(7);
		Node<Integer> eight = new Node<Integer>(8);
		Node<Integer> nine = new Node<Integer>(9);

		one.left = two;
		one.right = three;
		two.left = four;
		two.right = five;
		three.left = six;
		four.left = seven;
		six.left = eight;
		six.right = nine;

		traverse(one, ORDER.PREORDER);
		System.out.println();
		traverse(one, ORDER.INORDER);
		System.out.println();
		traverse(one, ORDER.POSTORDER);
		System.out.println();
		traverse(one, ORDER.LEVEL);

	}
}
```

Output:

```txt
1 2 4 7 5 3 6 8 9
7 4 2 5 1 8 6 9 3
7 4 5 2 8 9 6 3 1
1 2 3 4 5 6 7 8 9
```



## JavaScript


### ES5


### =Iteration=

inspired by [[#Ruby|Ruby]]

```javascript
function BinaryTree(value, left, right) {
    this.value = value;
    this.left = left;
    this.right = right;
}
BinaryTree.prototype.preorder  = function(f) {this.walk(f,['this','left','right'])}
BinaryTree.prototype.inorder   = function(f) {this.walk(f,['left','this','right'])}
BinaryTree.prototype.postorder = function(f) {this.walk(f,['left','right','this'])}
BinaryTree.prototype.walk = function(func, order) {
    for (var i in order)
        switch (order[i]) {
            case "this": func(this.value); break;
            case "left": if (this.left) this.left.walk(func, order); break;
            case "right": if (this.right) this.right.walk(func, order); break;
        }
}
BinaryTree.prototype.levelorder = function(func) {
    var queue = [this];
    while (queue.length != 0) {
        var node = queue.shift();
        func(node.value);
        if (node.left) queue.push(node.left);
        if (node.right) queue.push(node.right);
    }
}

// convenience function for creating a binary tree
function createBinaryTreeFromArray(ary) {
    var left = null, right = null;
    if (ary[1]) left = createBinaryTreeFromArray(ary[1]);
    if (ary[2]) right = createBinaryTreeFromArray(ary[2]);
    return new BinaryTree(ary[0], left, right);
}

var tree = createBinaryTreeFromArray([1, [2, [4, [7]], [5]], [3, [6, [8],[9]]]]);

print("*** preorder ***");   tree.preorder(print);
print("*** inorder ***");    tree.inorder(print);
print("*** postorder ***");  tree.postorder(print);
print("*** levelorder ***"); tree.levelorder(print);
```



### =Functional composition=

{{Trans|Haskell}}
(for binary trees consisting of nested lists)


```javascript
(function () {

    function preorder(n) {
        return [n[v]].concat(
            n[l] ? preorder(n[l]) : []
        ).concat(
            n[r] ? preorder(n[r]) : []
        );
    }

    function inorder(n) {
        return (
            n[l] ? inorder(n[l]) : []
        ).concat(
            n[v]
        ).concat(
            n[r] ? inorder(n[r]) : []
        );
    }

    function postorder(n) {
        return (
            n[l] ? postorder(n[l]) : []
        ).concat(
            n[r] ? postorder(n[r]) : []
        ).concat(
            n[v]
        );
    }

    function levelorder(n) {
        return (function loop(x) {
            return x.length ? (
                x[0] ? (
                [x[0][v]].concat(
                        loop(
                            x.slice(1).concat(
                                [x[0][l], x[0][r]]
                            )
                        )
                    )
                ) : loop(x.slice(1))
            ) : [];
        })([n]);
    }

    var v = 0,
        l = 1,
        r = 2,

        tree = [1,
                [2,
                    [4,
                        [7]
                    ],
                    [5]
                ],
                [3,
                    [6,
                        [8],
                        [9]
                    ]
                ]
            ],

        lstTest = [["Traversal", "Nodes visited"]].concat(
            [preorder, inorder, postorder, levelorder].map(
                function (f) {
                    return [f.name, f(tree)];
                }
            )
        );

    // [[a]] -> bool -> s -> s
    function wikiTable(lstRows, blnHeaderRow, strStyle) {
        return '{| class="wikitable" ' + (
            strStyle ? 'style="' + strStyle + '"' : ''
        ) + lstRows.map(function (lstRow, iRow) {
            var strDelim = ((blnHeaderRow && !iRow) ? '!' : '|');

            return '\n|-\n' + strDelim + ' ' + lstRow.map(function (v) {
                return typeof v === 'undefined' ? ' ' : v;
            }).join(' ' + strDelim + strDelim + ' ');
        }).join('') + '\n|}';
    }

    return wikiTable(lstTest, true) + '\n\n' + JSON.stringify(lstTest);

})();
```


Output:

{| class="wikitable"
|-
! Traversal !! Nodes visited
|-
| preorder || 1,2,4,7,5,3,6,8,9
|-
| inorder || 7,4,2,5,1,8,6,9,3
|-
| postorder || 7,4,5,2,8,9,6,3,1
|-
| levelorder || 1,2,3,4,5,6,7,8,9
|}


```JavaScript
[["Traversal","Nodes visited"],
["preorder",[1,2,4,7,5,3,6,8,9]],["inorder",[7,4,2,5,1,8,6,9,3]],
["postorder",[7,4,5,2,8,9,6,3,1]],["levelorder",[1,2,3,4,5,6,7,8,9]]]
```



or, again functionally, but:

# for a tree of nested dictionaries (rather than a simple nested list),
# defining a single '''traverse()''' function
# checking that the tree is indeed binary, and returning ''undefined'' for the ''in-order'' traversal if any node in the tree has more than two children. (The other 3 traversals are still defined for rose trees).



```JavaScript
(function () {
    'use strict';

    // 'preorder' | 'inorder' | 'postorder' | 'level-order'

    // traverse :: String -> Tree {value: a, nest: [Tree]} -> [a]
    function traverse(strOrderName, dctTree) {
        var strName = strOrderName.toLowerCase();

        if (strName.startsWith('level')) {

            // LEVEL-ORDER
            return levelOrder([dctTree]);

        } else if (strName.startsWith('in')) {
            var lstNest = dctTree.nest;

            if ((lstNest ? lstNest.length : 0) < 3) {
                var left = lstNest[0] || [],
                    right = lstNest[1] || [],

                    lstLeft = left.nest ? (
                        traverse(strName, left)
                    ) : (left.value || []),
                    lstRight = right.nest ? (
                        traverse(strName, right)
                    ) : (right.value || []);

                return (lstLeft !== undefined && lstRight !== undefined) ?

                    // IN-ORDER
                    (lstLeft instanceof Array ? lstLeft : [lstLeft])
                    .concat(dctTree.value)
                    .concat(lstRight) : undefined;

            } else { // in-order only defined here for binary trees
                return undefined;
            }

        } else {
            var lstTraversed = concatMap(function (x) {
                return traverse(strName, x);
            }, (dctTree.nest || []));

            return (
                strName.startsWith('pre') ? (

                    // PRE-ORDER
                    [dctTree.value].concat(lstTraversed)

                ) : strName.startsWith('post') ? (

                    // POST-ORDER
                    lstTraversed.concat(dctTree.value)

                ) : []
            );
        }
    }

    // levelOrder :: [Tree {value: a, nest: [Tree]}] -> [a]
    function levelOrder(lstTree) {
        var lngTree = lstTree.length,
            head = lngTree ? lstTree[0] : undefined,
            tail = lstTree.slice(1);

        // Recursively take any value found in the head node
        // of the remaining tail, deferring any child nodes
        // of that head to the end of the tail
        return lngTree ? (
            head ? (
                [head.value].concat(
                    levelOrder(
                        tail
                        .concat(head.nest || [])
                    )
                )
            ) : levelOrder(tail)
        ) : [];
    }

    // concatMap :: (a -> [b]) -> [a] -> [b]
    function concatMap(f, xs) {
        return [].concat.apply([], xs.map(f));
    }

    var dctTree = {
        value: 1,
        nest: [{
            value: 2,
            nest: [{
                value: 4,
                nest: [{
                    value: 7
                }]
            }, {
                value: 5
            }]
        }, {
            value: 3,
            nest: [{
                value: 6,
                nest: [{
                    value: 8
                }, {
                    value: 9
                }]
            }]
        }]
    };


    return ['preorder', 'inorder', 'postorder', 'level-order']
        .reduce(function (a, k) {
            return (
                a[k] = traverse(k, dctTree),
                a
            );
        }, {});

})();
```

{{Out}}

```JavaScript
{"preorder":[1, 2, 4, 7, 5, 3, 6, 8, 9],
"inorder":[7, 4, 2, 5, 1, 8, 6, 9, 3],
"postorder":[7, 4, 5, 2, 8, 9, 6, 3, 1],
"level-order":[1, 2, 3, 4, 5, 6, 7, 8, 9]}
```



### ES6

{{Trans|Haskell}}

```JavaScript
(() => {
    // TRAVERSALS -------------------------------------------------------------

    // preorder Tree a -> [a]
    const preorder = a => [a[v]]
        .concat(a[l] ? preorder(a[l]) : [])
        .concat(a[r] ? preorder(a[r]) : []);

    // inorder Tree a -> [a]
    const inorder = a =>
        (a[l] ? inorder(a[l]) : [])
        .concat(a[v])
        .concat(a[r] ? inorder(a[r]) : []);

    // postorder Tree a -> [a]
    const postorder = a =>
        (a[l] ? postorder(a[l]) : [])
        .concat(a[r] ? postorder(a[r]) : [])
        .concat(a[v]);

    // levelorder Tree a -> [a]
    const levelorder = a => (function go(x) {
        return x.length ? (
            x[0] ? (
                [x[0][v]].concat(
                    go(x.slice(1)
                        .concat([x[0][l], x[0][r]])
                    )
                )
            ) : go(x.slice(1))
        ) : [];
    })([a]);


    // GENERIC FUNCTIONS  -----------------------------------------------------

    // A list of functions applied to a list of arguments
    // <*> :: [(a -> b)] -> [a] -> [b]
    const ap = (fs, xs) => //
        [].concat.apply([], fs.map(f => //
            [].concat.apply([], xs.map(x => [f(x)]))));

    // intercalate :: String -> [a] -> String
    const intercalate = (s, xs) => xs.join(s);

    // justifyLeft :: Int -> Char -> Text -> Text
    const justifyLeft = (n, cFiller, strText) =>
        n > strText.length ? (
            (strText + cFiller.repeat(n))
            .substr(0, n)
        ) : strText;

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) =>
        Array.from({
            length: Math.min(xs.length, ys.length)
        }, (_, i) => f(xs[i], ys[i]));

    // TEST -------------------------------------------------------------------
    // asciiTree :: String
    const asciiTree = unlines([
        '         1',
        '        / \\',
        '       /   \\',
        '      /     \\',
        '     2       3',
        '    / \\     /',
        '   4   5   6',
        '  /       / \\',
        ' 7       8   9'
    ]);

    const [v, l, r] = [0, 1, 2],
    tree = [1, [2, [4, [7]],
                [5]
            ],
            [3, [6, [8],
                [9]
            ]]
        ],

        // fs :: [(Tree a -> [a])]
        fs = [preorder, inorder, postorder, levelorder];

    return asciiTree + '\n\n' +
        intercalate('\n',
            zipWith(
                (f, xs) => justifyLeft(12, ' ', f.name + ':') + unwords(xs),
                fs,
                ap(fs, [tree])
            )
        );
})();
```

{{Out}}

```JavaScript
         1
        / \
       /   \
      /     \
     2       3
    / \     /
   4   5   6
  /       / \
 7       8   9

preorder:   1 2 4 7 5 3 6 8 9
inorder:    7 4 2 5 1 8 6 9 3
postorder:  7 4 5 2 8 9 6 3 1
levelorder: 1 2 3 4 5 6 7 8 9
```



## jq

All the ordering filters defined here produce streams. For the final output, each stream is condensed into an array.

The implementation assumes an array structured recursively as [ node, left, right ], where "left" and "right" may be [] or null equivalently.

```jq
def preorder:
  if length == 0 then empty
  else .[0], (.[1]|preorder), (.[2]|preorder)
  end;

def inorder:
  if length == 0 then empty
  else (.[1]|inorder), .[0] , (.[2]|inorder)
  end;

def postorder:
  if length == 0 then empty
  else (.[1] | postorder), (.[2]|postorder), .[0]
  end;

# Helper functions for levelorder:
  # Produce a stream of the first elements
  def heads: map( .[0] | select(. != null)) | .[];

# Produce a stream of the left/right branches:
  def tails:
    if length == 0 then empty
    else [map ( .[1], .[2] ) | .[] | select( . != null)]
    end;

def levelorder: [.] | recurse( tails ) | heads;

```

'''The task''':

```jq
def task:
  # [node, left, right]
  def atree: [1, [2, [4, [7,[],[]],
                         []],
                     [5, [],[]]],

                 [3, [6, [8,[],[]],
                         [9,[],[]]],
                     []]] ;

  "preorder:   \( [atree|preorder ])",
  "inorder:    \( [atree|inorder  ])",
  "postorder:  \( [atree|postorder ])",
  "levelorder: \( [atree|levelorder])"
;

task
```

{{Out}}
 $ jq -n -c -r -f Tree_traversal.jq
 preorder:   [1,2,4,7,5,3,6,8,9]
 inorder:    [7,4,2,5,1,8,6,9,3]
 postorder:  [7,4,5,2,8,9,6,3,1]
 levelorder: [1,2,3,4,5,6,7,8,9]


## Julia


```Julia
tree = Any[1, Any[2, Any[4, Any[7, Any[],
                                          Any[]],
                                   Any[]],
                            Any[5, Any[],
                                   Any[]]],
                     Any[3, Any[6, Any[8, Any[],
                                          Any[]],
                                   Any[9, Any[],
                                          Any[]]],
                            Any[]]]

preorder(t, f) = if !isempty(t)
                     f(t[1]); preorder(t[2], f); preorder(t[3], f)
                 end

inorder(t, f) = if !isempty(t)
                    inorder(t[2], f); f(t[1]); inorder(t[3], f)
                end

postorder(t, f) = if !isempty(t)
                      postorder(t[2], f); postorder(t[3], f); f(t[1])
                  end

levelorder(t, f) = while !isempty(t)
                       t = mapreduce(x -> isa(x, Number) ? (f(x); []) : x, vcat, t)
                   end

```


{{Out}}

```txt

julia> for f in [preorder, inorder, postorder, levelorder]
           print((lpad("$f: ", 12))); f(tree, x -> print(x, " ")); println()
       end
  preorder: 1 2 4 7 5 3 6 8 9
   inorder: 7 4 2 5 1 8 6 9 3
 postorder: 7 4 5 2 8 9 6 3 1
levelorder: 1 2 3 4 5 6 7 8 9

```



## Kotlin


### procedural style


```scala
data class Node(val v: Int, var left: Node? = null, var right: Node? = null) {
    override fun toString() = "$v"
}

fun preOrder(n: Node?) {
    n?.let {
        print("$n ")
        preOrder(n.left)
        preOrder(n.right)
    }
}

fun inorder(n: Node?) {
    n?.let {
        inorder(n.left)
        print("$n ")
        inorder(n.right)
    }
}

fun postOrder(n: Node?) {
    n?.let {
        postOrder(n.left)
        postOrder(n.right)
        print("$n ")
    }
}

fun levelOrder(n: Node?) {
    n?.let {
        val queue = mutableListOf(n)
        while (queue.isNotEmpty()) {
            val node = queue.removeAt(0)
            print("$node ")
            node.left?.let { queue.add(it) }
            node.right?.let { queue.add(it) }
        }
    }
}

inline fun exec(name: String, n: Node?, f: (Node?) -> Unit) {
    print(name)
    f(n)
    println()
}

fun main(args: Array<String>) {
    val nodes = Array(10) { Node(it) }

    nodes[1].left = nodes[2]
    nodes[1].right = nodes[3]

    nodes[2].left = nodes[4]
    nodes[2].right = nodes[5]

    nodes[4].left = nodes[7]

    nodes[3].left = nodes[6]

    nodes[6].left = nodes[8]
    nodes[6].right = nodes[9]

    exec("   preOrder: ", nodes[1], ::preOrder)
    exec("    inorder: ", nodes[1], ::inorder)
    exec("  postOrder: ", nodes[1], ::postOrder)
    exec("level-order: ", nodes[1], ::levelOrder)
}
```


{{Out}}

```txt

   preOrder: 1 2 4 7 5 3 6 8 9
    inorder: 7 4 2 5 1 8 6 9 3
  postOrder: 7 4 5 2 8 9 6 3 1
level-order: 1 2 3 4 5 6 7 8 9

```


===object-oriented style===

```scala
fun main(args: Array<String>) {
    data class Node(val v: Int, var left: Node? = null, var right: Node? = null) {
        override fun toString() = " $v"

        fun preOrder()  { print(this); left?.preOrder(); right?.preOrder() }
        fun inorder()   { left?.inorder(); print(this); right?.inorder() }
        fun postOrder() { left?.postOrder(); right?.postOrder(); print(this) }

        fun levelOrder() = with(mutableListOf(this)) {
            do {
                val node = removeAt(0)
                print(node)
                node.left?.let { add(it) }
                node.right?.let { add(it) }
            } while (any())
        }

        inline fun exec(name: String, f: (Node) -> Unit) {
            print(name)
            f(this)
            println()
        }
    }

    val nodes = Array(10) { Node(it) }

    nodes[1].left = nodes[2]
    nodes[1].right = nodes[3]
    nodes[2].left = nodes[4]
    nodes[2].right = nodes[5]
    nodes[4].left = nodes[7]
    nodes[3].left = nodes[6]
    nodes[6].left = nodes[8]
    nodes[6].right = nodes[9]

    with(nodes[1]) {
        exec("   preOrder:", Node::preOrder)
        exec("    inorder:", Node::inorder)
        exec("  postOrder:", Node::postOrder)
        exec("level-order:", Node::levelOrder)
    }
}
```



## Lingo


```lingo
-- parent script "BinaryTreeNode"

property _val, _left, _right

on new (me, val)
  me._val = val
  return me
end

on getValue (me)
  return me._val
end

on setLeft (me, node)
  me._left = node
end

on setRight (me, node)
  me._right = node
end

on getLeft (me)
  return me._left
end

on getRight (me)
  return me._right
end
```



```lingo
-- parent script "BinaryTreeTraversal"

on inOrder (me, node, l)
  if voidP(l) then l = []
  if voidP(node) then return l
  if not voidP(node.getLeft()) then l = me.inOrder(node.getLeft(), l)
  l.add(node)
  if not voidP(node.getRight()) then l = me.inOrder(node.getRight(), l)
  return l
end

on preOrder (me, node, l)
  if voidP(l) then l = []
  if voidP(node) then return l
  l.add(node)
  if not voidP(node.getLeft()) then l = me.preOrder(node.getLeft(), l)
  if not voidP(node.getRight()) then l = me.preOrder(node.getRight(), l)
  return l
end

on postOrder (me, node, l)
  if voidP(l) then l = []
  if voidP(node) then return l
  if not voidP(node.getLeft()) then l = me.postOrder(node.getLeft(), l)
  if not voidP(node.getRight()) then l = me.postOrder(node.getRight(), l)
  l.add(node)
  return l
end

on levelOrder (me, node)
  l = []
  queue = [node]
  repeat while queue.count
    node = queue[1]
    queue.deleteAt(1)
    l.add(node)
    if not voidP(node.getLeft()) then queue.add(node.getLeft())
    if not voidP(node.getRight()) then queue.add(node.getRight())
  end repeat
  return l
end

-- print utility function
on serialize (me, l)
  str = ""
  repeat with node in l
    put node.getValue()&" " after str
  end repeat
  delete the last char of str
  return str
end
```


Usage:

```lingo
-- create the tree
l = []
repeat with i = 1 to 10
  l[i] = script("BinaryTreeNode").new(i)
end repeat
l[6].setLeft (l[8])
l[6].setRight(l[9])
l[3].setLeft (l[6])
l[4].setLeft (l[7])
l[2].setLeft (l[4])
l[2].setRight(l[5])
l[1].setLeft (l[2])
l[1].setRight(l[3])

-- print traversal results
trav = script("BinaryTreeTraversal")
put "preorder:    " & trav.serialize(trav.preOrder(l[1]))
put "inorder:     " & trav.serialize(trav.inOrder(l[1]))
put "postorder:   " & trav.serialize(trav.postOrder(l[1]))
put "level-order: " & trav.serialize(trav.levelOrder(l[1]))
```


{{Out}}

```txt

-- "preorder:    1 2 4 7 5 3 6 8 9"
-- "inorder:     7 4 2 5 1 8 6 9 3"
-- "postorder:   7 4 5 2 8 9 6 3 1"
-- "level-order: 1 2 3 4 5 6 7 8 9"

```



## Logo


```logo
; nodes are [data left right], use "first" to get data

to node.left :node
  if empty? butfirst :node [output []]
  output first butfirst :node
end
to node.right :node
  if empty? butfirst :node [output []]
  if empty? butfirst butfirst :node [output []]
  output first butfirst butfirst :node
end
to max :a :b
  output ifelse :a > :b [:a] [:b]
end
to tree.depth :tree
  if empty? :tree [output 0]
  output 1 + max tree.depth node.left :tree  tree.depth node.right :tree
end

to pre.order :tree :action
  if empty? :tree [stop]
  invoke :action first :tree
  pre.order node.left :tree :action
  pre.order node.right :tree :action
end
to in.order :tree :action
  if empty? :tree [stop]
  in.order node.left :tree :action
  invoke :action first :tree
  in.order node.right :tree :action
end
to post.order :tree :action
  if empty? :tree [stop]
  post.order node.left :tree :action
  post.order node.right :tree :action
  invoke :action first :tree
end
to at.depth :n :tree :action
  if empty? :tree [stop]
  ifelse :n = 1 [invoke :action first :tree] [
    at.depth :n-1 node.left  :tree :action
    at.depth :n-1 node.right :tree :action
  ]
end
to level.order :tree :action
  for [i 1 [tree.depth :tree]] [at.depth :i :tree :action]
end

make "tree [1 [2 [4 [7]]
                 [5]]
              [3 [6 [8]
                    [9]]]]

  pre.order :tree [(type ? "| |)]  (print)
   in.order :tree [(type ? "| |)]  (print)
 post.order :tree [(type ? "| |)]  (print)
level.order :tree [(type ? "| |)]  (print)
```



## Logtalk


```logtalk

:- object(tree_traversal).

    :- public(orders/1).
    orders(Tree) :-
        write('Pre-order:   '), pre_order(Tree), nl,
        write('In-order:    '), in_order(Tree), nl,
        write('Post-order:  '), post_order(Tree), nl,
        write('Level-order: '), level_order(Tree).

    :- public(orders/0).
    orders :-
        tree(Tree),
        orders(Tree).

    tree(
        t(1,
            t(2,
                t(4,
                    t(7, t, t),
                    t
                ),
                t(5, t, t)
            ),
            t(3,
                t(6,
                    t(8, t, t),
                    t(9, t, t)
                ),
                t
            )
        )
    ).

    pre_order(t).
    pre_order(t(Value, Left, Right)) :-
        write(Value), write(' '),
        pre_order(Left),
        pre_order(Right).

    in_order(t).
    in_order(t(Value, Left, Right)) :-
        in_order(Left),
        write(Value), write(' '),
        in_order(Right).

    post_order(t).
    post_order(t(Value, Left, Right)) :-
        post_order(Left),
        post_order(Right),
        write(Value), write(' ').

    level_order(t).
    level_order(t(Value, Left, Right)) :-
        % write tree root value
        write(Value), write(' '),
        % write rest of the tree
        level_order([Left, Right], Tail-Tail).

    level_order([], Trees-[]) :-
        (   Trees \= [] ->
            % print next level
            level_order(Trees, Tail-Tail)
        ;   % no more levels
            true
        ).
    level_order([Tree| Trees], Rest0) :-
        (   Tree = t(Value, Left, Right) ->
            write(Value), write(' '),
            % collect the subtrees to print the next level
            append(Rest0, [Left, Right| Tail]-Tail, Rest1),
            % continue printing the current level
            level_order(Trees, Rest1)
        ;   % continue printing the current level
            level_order(Trees, Rest0)
        ).

    % use difference-lists for constant time append
    append(List1-Tail1, Tail1-Tail2, List1-Tail2).

:- end_object.

```

Sample output:

```text

| ?- ?- tree_traversal::orders.
Pre-order:   1 2 4 7 5 3 6 8 9
In-order:    7 4 2 5 1 8 6 9 3
Post-order:  7 4 5 2 8 9 6 3 1
Level-order: 1 2 3 4 5 6 7 8 9
yes

```


## Lua


```Lua
-- Utility
local function append(t1, t2)
    for _, v in ipairs(t2) do
        table.insert(t1, v)
    end
end

-- Node class
local Node = {}
Node.__index = Node

function Node:order(order)
    local r = {}
    append(r, type(self[order[1]]) == "table" and self[order[1]]:order(order) or {self[order[1]]})
    append(r, type(self[order[2]]) == "table" and self[order[2]]:order(order) or {self[order[2]]})
    append(r, type(self[order[3]]) == "table" and self[order[3]]:order(order) or {self[order[3]]})
    return r
end

function Node:levelorder()
    local levelorder = {}
    local queue = {self}
    while next(queue) do
        local node = table.remove(queue, 1)
        table.insert(levelorder, node[1])
        table.insert(queue, node[2])
        table.insert(queue, node[3])
    end
    return levelorder
end

-- Node creator
local function new(value, left, right)
    return value and setmetatable({
        value,
        (type(left) == "table") and new(unpack(left)) or new(left),
        (type(right) == "table") and new(unpack(right)) or new(right),
        }, Node) or nil
end

-- Example
local tree = new(1, {2, {4, 7}, 5}, {3, {6, 8, 9}})
print("preorder:    " .. table.concat(tree:order({1, 2, 3}), " "))
print("inorder:     " .. table.concat(tree:order({2, 1, 3}), " "))
print("postorder:   " .. table.concat(tree:order({2, 3, 1}), " "))
print("level-order: " .. table.concat(tree:levelorder(), " "))
```


## M2000 Interpreter


### Using Tuple as Tree

A tuple is an "auto array" in M2000 Interpreter. (,) is the zero length array.


```M2000 Interpreter

Module CheckIt {
      Null=(,)
      Tree=((((Null,7,Null),4,Null),2,(Null,5,Null)),1,(((Null,8,Null),6,(Null,9,Null)),3,Null))

      Module preorder (T) {
            Print "preorder:    ";
            printtree(T)
            Print
            sub printtree(T)
                  Print T#val(1);" ";
                  If len(T#val(0))>0 then printtree(T#val(0))
                  If len(T#val(2))>0 then printtree(T#val(2))
            end sub
      }
      preorder Tree

      Module inorder (T) {
            Print "inorder:     ";
            printtree(T)
            Print
            sub printtree(T)
                  If len(T#val(0))>0 then printtree(T#val(0))
                  Print T#val(1);" ";
                  If len(T#val(2))>0 then printtree(T#val(2))
            end sub
      }
      inorder Tree

      Module postorder (T) {
            Print "postorder:   ";
            printtree(T)
            Print
            sub printtree(T)
                  If len(T#val(0))>0 then printtree(T#val(0))
                  If len(T#val(2))>0 then printtree(T#val(2))
                  Print T#val(1);" ";
            end sub
      }
      postorder Tree

      Module level_order (T) {
            Print "level-order: ";
            Stack New {
                  printtree(T)
                  if empty then exit
                  Read T
                  Loop
            }
            Print
            sub printtree(T)
                  If Len(T)>0 then
                        Print T#val(1);" ";
                        Data T#val(0), T#val(2)
                  end if
            end sub
      }
      level_order Tree
}
CheckIt

```


### Using OOP

Now tree is nodes with pointers to nodes (a node ifs a Group, the user object)
The "as pointer" is optional, but we can use type check if we want.


```M2000 Interpreter

Module OOP {
      \\ Class is a global function (until this module end)
      Class Null {
      }
      \\ Null is a pointer to an object returned from class Null()
      Global Null->Null()
      Class Node {
      Public:
            x, Group LeftNode, Group RightNode
      Class:
            \\ after class:  anything exist one time,
            \\ not included in final object
            Module Node {
                  .LeftNode<=Null
                  .RightNode<=Null
                  Read .x
                  \\ read ? for optional values
                  Read ? .LeftNode, .RightNode
            }
      }
      \\ NodeTree return a pointer to a new Node
      Function NodeTree {
            \\ ![] pass currrent stack to Node()
            ->Node(![])
      }

      Tree=NodeTree(1, NodeTree(2,NodeTree(4, NodeTree(7)), NodeTree(5)), NodeTree(3, NodeTree(6, NodeTree(8), NodeTree(9))))

      Module preorder (T) {
            Print "preorder:    ";
            printtree(T)
            Print
            sub printtree(T as pointer)
                  If T is Null then Exit sub
                  Print T=>x;" ";
                  printtree(T=>LeftNode)
                  printtree(T=>RightNode)
            end sub
      }
      preorder Tree
      Module inorder (T) {
            Print "inorder:     ";
            printtree(T)
            Print
            sub printtree(T as pointer)
                  If T is Null then Exit sub
                  printtree(T=>LeftNode)
                  Print T=>x;" ";
                  printtree(T=>RightNode)
            end sub
      }
      inorder Tree
      Module postorder (T) {
            Print "postorder:   ";
            printtree(T)
            Print
            sub printtree(T as pointer)
                  If T is Null then Exit sub
                  printtree(T=>LeftNode)
                  printtree(T=>RightNode)
                  Print T=>x;" ";
            end sub
      }
      postorder Tree
      Module level_order (T) {
            Print "level-order: ";
            Stack New {
                  printtree(T)
                  if empty then exit
                  Read T
                  Loop
            }
            Print
            sub printtree(T as pointer)
                  If T is Null else
                        Print T=>x;" ";
                        Data T=>LeftNode, T=>RightNode
                  end if
            end sub
      }
      level_order Tree
}
OOP

```


or we can put modules inside Node Class as methods
also i put a visitor as a call back (a lambda function called as module)


```M2000 Interpreter

Module OOP {
      \\ Class is a global function (until this module end)
      Class Null {
      }
      \\ Null is a pointer to an object returned from class Null()
      Global Null->Null()
      Class Node {
      Public:
            x, Group LeftNode, Group RightNode
            Module preorder (visitor){
                  T->This
                  printtree(T)
                  sub printtree(T as pointer)
                        If T is Null then Exit sub
                        call visitor(T=>x)
                        printtree(T=>LeftNode)
                        printtree(T=>RightNode)
                  end sub
            }
            Module inorder (visitor){
                  T->This
                  printtree(T)
                  sub printtree(T as pointer)
                        If T is Null then Exit sub
                        printtree(T=>LeftNode)
                        call visitor(T=>x)
                        printtree(T=>RightNode)
                  end sub
            }
            Module postorder (visitor) {
                  T->This
                  printtree(T)
                  sub printtree(T as pointer)
                        If T is Null then Exit sub
                        printtree(T=>LeftNode)
                        printtree(T=>RightNode)
                        call visitor(T=>x)
                  end sub
            }
            Module level_order (visitor){
                  T->This
                  Stack New {
                        printtree(T)
                        if empty then exit
                        Read T
                        Loop
                  }
                  sub printtree(T as pointer)
                        If T is Null else
                              call visitor(T=>x)
                              Data T=>LeftNode, T=>RightNode
                        end if
                  end sub
            }
      Class:
            \\ after class:  anything exist one time,
            \\ not included in final object
            Module Node {
                  .LeftNode<=Null
                  .RightNode<=Null
                  Read .x
                  \\ read ? for optional values
                  Read ? .LeftNode, .RightNode
            }
      }
      \\ NodeTree return a pointer to a new Node
      Function NodeTree {
            \\ ![] pass currrent stack to Node()
            ->Node(![])
      }

      Tree=NodeTree(1, NodeTree(2,NodeTree(4, NodeTree(7)), NodeTree(5)), NodeTree(3, NodeTree(6, NodeTree(8), NodeTree(9))))

      printnum=lambda (title$) -> {
            Print
            Print title$;
            =lambda (x)-> {
                  Print x;" ";
            }
      }
      Tree=>preorder printnum("preorder:    ")
      Tree=>inorder printnum("inorder:     ")
      Tree=>postorder printnum("postorder:   ")
      Tree=>level_order printnum("level-order: ")
}
OOP

```


Using Event object as visitor


```M2000 Interpreter

Module OOP {
      \\ Class is a global function (until this module end)
      Class Null {
      }
      \\ Null is a pointer to an object returned from class Null()
      Global Null->Null()
      Class Node {
      Public:
            x, Group LeftNode, Group RightNode
            Module preorder (visitor){
                  T->This
                  printtree(T)
                  sub printtree(T as pointer)
                        If T is Null then Exit sub
                        call event visitor, T=>x
                        printtree(T=>LeftNode)
                        printtree(T=>RightNode)
                  end sub
            }
            Module inorder (visitor){
                  T->This
                  printtree(T)
                  sub printtree(T as pointer)
                        If T is Null then Exit sub
                        printtree(T=>LeftNode)
                        call event visitor, T=>x
                        printtree(T=>RightNode)
                  end sub
            }
            Module postorder (visitor) {
                  T->This
                  printtree(T)
                  sub printtree(T as pointer)
                        If T is Null then Exit sub
                        printtree(T=>LeftNode)
                        printtree(T=>RightNode)
                        call event visitor, T=>x
                  end sub
            }
            Module level_order (visitor){
                  T->This
                  Stack New {
                        printtree(T)
                        if empty then exit
                        Read T
                        Loop
                  }
                  sub printtree(T as pointer)
                        If T is Null else
                              call event visitor, T=>x
                              Data T=>LeftNode, T=>RightNode
                        end if
                  end sub
            }
      Class:
            \\ after class:  anything exist one time,
            \\ not included in final object
            Module Node {
                  .LeftNode<=Null
                  .RightNode<=Null
                  Read .x
                  \\ read ? for optional values
                  Read ? .LeftNode, .RightNode
            }
      }
      \\ NodeTree return a pointer to a new Node
      Function NodeTree {
            \\ ![] pass currrent stack to Node()
            ->Node(![])
      }

      Tree=NodeTree(1, NodeTree(2,NodeTree(4, NodeTree(7)), NodeTree(5)), NodeTree(3, NodeTree(6, NodeTree(8), NodeTree(9))))
      Event PrintAnum {
            read x
      }
      Function PrintThis(x) {
                Print x;" ";
      }
      Event PrintAnum New PrintThis()
      printnum=lambda PrintAnum (title$) -> {
            Print
            Print title$;
            =PrintAnum
      }
      Tree=>preorder printnum("preorder:    ")
      Tree=>inorder printnum("inorder:     ")
      Tree=>postorder printnum("postorder:   ")
      Tree=>level_order printnum("level-order: ")
}
OOP

```


{{out}}

```txt

preorder:    1 2 4 7 5 3 6 8 9
inorder:     7 4 2 5 1 8 6 9 3
postorder:   7 4 5 2 8 9 6 3 1
level-order: 1 2 3 4 5 6 7 8 9
</pre >


## Mathematica


```mathematica
preorder[a_Integer] := a;
preorder[a_[b__]] := Flatten@{a, preorder /@ {b}};
inorder[a_Integer] := a;
inorder[a_[b_, c_]] := Flatten@{inorder@b, a, inorder@c};
inorder[a_[b_]] := Flatten@{inorder@b, a}; postorder[a_Integer] := a;
postorder[a_[b__]] := Flatten@{postorder /@ {b}, a};
levelorder[a_] :=
 Flatten[Table[Level[a, {n}], {n, 0, Depth@a}]] /. {b_Integer[__] :>
    b};
```


Example:

```mathematica
preorder[1[2[4[7], 5], 3[6[8, 9]]]]
inorder[1[2[4[7], 5], 3[6[8, 9]]]]
postorder[1[2[4[7], 5], 3[6[8, 9]]]]
levelorder[1[2[4[7], 5], 3[6[8, 9]]]]
```


Output:

```txt
{1, 2, 4, 7, 5, 3, 6, 8, 9}

{7, 4, 2, 5, 1, 8, 6, 9, 3}

{7, 4, 5, 2, 8, 9, 6, 3, 1}

{1, 2, 3, 4, 5, 6, 7, 8, 9}
```



## Mercury


```mercury
:- module tree_traversal.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- type tree(V)
    --->    empty
    ;       node(V, tree(V), tree(V)).

:- pred preorder(pred(V, A, A), tree(V), A, A).
:- mode preorder(pred(in, di, uo) is det, in, di, uo) is det.

preorder(_, empty, !Acc).
preorder(P, node(Value, Left, Right), !Acc) :-
    P(Value, !Acc),
    preorder(P, Left, !Acc),
    preorder(P, Right, !Acc).

:- pred inorder(pred(V, A, A), tree(V), A, A).
:- mode inorder(pred(in, di, uo) is det, in, di, uo) is det.

inorder(_, empty, !Acc).
inorder(P, node(Value, Left, Right), !Acc) :-
    inorder(P, Left, !Acc),
    P(Value, !Acc),
    inorder(P, Right, !Acc).

:- pred postorder(pred(V, A, A), tree(V), A, A).
:- mode postorder(pred(in, di, uo) is det, in, di, uo) is det.

postorder(_, empty, !Acc).
postorder(P, node(Value, Left, Right), !Acc) :-
    postorder(P, Left, !Acc),
    postorder(P, Right, !Acc),
    P(Value, !Acc).

:- pred levelorder(pred(V, A, A), tree(V), A, A).
:- mode levelorder(pred(in, di, uo) is det, in, di, uo) is det.

levelorder(P, Tree, !Acc) :-
    do_levelorder(P, [Tree], !Acc).

:- pred do_levelorder(pred(V, A, A), list(tree(V)), A, A).
:- mode do_levelorder(pred(in, di, uo) is det, in, di, uo) is det.

do_levelorder(_, [], !Acc).
do_levelorder(P, [empty | Xs], !Acc) :-
   do_levelorder(P, Xs, !Acc).
do_levelorder(P, [node(Value, Left, Right) | Xs], !Acc) :-
   P(Value, !Acc),
   do_levelorder(P, Xs ++ [Left, Right], !Acc).

:- func tree = tree(int).

tree =
    node(1,
        node(2,
            node(4,
                node(7, empty, empty),
                empty
            ),
            node(5, empty, empty)
        ),
        node(3,
            node(6,
                node(8, empty, empty),
                node(9, empty, empty)
            ),
            empty
        )
    ).

main(!IO) :-
     io.write_string("preorder:   " ,!IO),
     preorder(print_value, tree, !IO), io.nl(!IO),
     io.write_string("inorder:    " ,!IO),
     inorder(print_value, tree, !IO), io.nl(!IO),
     io.write_string("postorder:  " ,!IO),
     postorder(print_value, tree, !IO), io.nl(!IO),
     io.write_string("levelorder: " ,!IO),
     levelorder(print_value, tree, !IO), io.nl(!IO).

:- pred print_value(V::in, io::di, io::uo) is det.

print_value(V, !IO) :-
    io.print(V, !IO),
    io.write_string(" ", !IO).
```

Output:

```txt
preorder:   1 2 4 7 5 3 6 8 9
inorder:    7 4 2 5 1 8 6 9 3
postorder:  7 4 5 2 8 9 6 3 1
levelorder: 1 2 3 4 5 6 7 8 9
```



## Nim


```nim
import queues, sequtils

type
  Node[T] = ref TNode[T]
  TNode[T] = object
    data: T
    left, right: Node[T]

proc newNode[T](data: T; left, right: Node[T] = nil): Node[T] =
  Node[T](data: data, left: left, right: right)

proc preorder[T](n: Node[T]): seq[T] =
  if n == nil: @[]
  else: @[n.data] & preorder(n.left) & preorder(n.right)

proc inorder[T](n: Node[T]): seq[T] =
  if n == nil: @[]
  else: inorder(n.left) & @[n.data] & inorder(n.right)

proc postorder[T](n: Node[T]): seq[T] =
  if n == nil: @[]
  else: postorder(n.left) & postorder(n.right) & @[n.data]

proc levelorder[T](n: Node[T]): seq[T] =
  result = @[]
  var queue = initQueue[Node[T]]()
  queue.enqueue(n)
  while queue.len > 0:
    let next = queue.dequeue()
    result.add next.data
    if next.left != nil: queue.enqueue(next.left)
    if next.right != nil: queue.enqueue(next.right)

let tree = 1.newNode(
             2.newNode(
               4.newNode(
                 7.newNode),
               5.newNode),
             3.newNode(
               6.newNode(
                 8.newNode,
                 9.newNode)))

echo preorder tree
echo inorder tree
echo postorder tree
echo levelorder tree
```

Output:

```txt
@[1, 2, 4, 7, 5, 3, 6, 8, 9]
@[7, 4, 2, 5, 1, 8, 6, 9, 3]
@[7, 4, 5, 2, 8, 9, 6, 3, 1]
@[1, 2, 3, 4, 5, 6, 7, 8, 9]
```



## Objeck


```objeck

﻿﻿use Collection;

class Test {
  function : Main(args : String[]) ~ Nil {
    one := Node->New(1);
    two := Node->New(2);
    three := Node->New(3);
    four := Node->New(4);
    five := Node->New(5);
    six := Node->New(6);
    seven := Node->New(7);
    eight := Node->New(8);
    nine := Node->New(9);

    one->SetLeft(two); one->SetRight(three);
    two->SetLeft(four); two->SetRight(five);
    three->SetLeft(six); four->SetLeft(seven);
    six->SetLeft(eight); six->SetRight(nine);

    "Preorder: "->Print(); Preorder(one);
    "\nInorder: "->Print(); Inorder(one);
    "\nPostorder: "->Print(); Postorder(one);
    "\nLevelorder: "->Print(); Levelorder(one);
    "\n"->Print();
  }

  function : Preorder(node : Node) ~ Nil {
    if(node <> Nil) {
      System.IO.Console->Print(node->GetData())->Print(", ");
      Preorder(node->GetLeft());
      Preorder(node->GetRight());
    };
  }

  function : Inorder(node : Node) ~ Nil {
    if(node <> Nil) {
      Inorder(node->GetLeft());
      System.IO.Console->Print(node->GetData())->Print(", ");
      Inorder(node->GetRight());
    };
  }

  function : Postorder(node : Node) ~ Nil {
    if(node <> Nil) {
      Postorder(node->GetLeft());
      Postorder(node->GetRight());
      System.IO.Console->Print(node->GetData())->Print(", ");
    };
  }

  function : Levelorder(node : Node) ~ Nil {
    nodequeue := Collection.Queue->New();
    if(node <> Nil) {
      nodequeue->Add(node);
    };

    while(nodequeue->IsEmpty() = false) {
      next := nodequeue->Remove()->As(Node);
      System.IO.Console->Print(next->GetData())->Print(", ");
      if(next->GetLeft() <> Nil) {
        nodequeue->Add(next->GetLeft());
      };

      if(next->GetRight() <> Nil) {
        nodequeue->Add(next->GetRight());
      };
    };
  }
}

class Node from BasicCompare {
  @left : Node;
  @right : Node;
  @data : Int;

  New(data : Int) {
    Parent();
    @data := data;
  }

  method : public : GetData() ~ Int {
    return @data;
  }

  method : public : SetLeft(left : Node) ~ Nil {
    @left := left;
  }

  method : public : GetLeft() ~ Node {
    return @left;
  }

  method : public : SetRight(right : Node) ~ Nil {
    @right := right;
  }

  method : public : GetRight() ~ Node {
    return @right;
  }

  method : public : Compare(rhs : Compare) ~ Int {
    right : Node := rhs->As(Node);
    if(@data = right->GetData()) {
      return 0;
    }
    else if(@data < right->GetData()) {
      return -1;
    };

    return 1;
  }
}

```


Output:

```txt

Preorder: 1, 2, 4, 7, 5, 3, 6, 8, 9,
Inorder: 7, 4, 2, 5, 1, 8, 6, 9, 3,
Postorder: 7, 4, 5, 2, 8, 9, 6, 3, 1,
Levelorder: 1, 2, 3, 4, 5, 6, 7, 8, 9,

```



## OCaml


```ocaml
type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

let rec preorder f = function
    Empty        -> ()
  | Node (v,l,r) -> f v;
                    preorder f l;
                    preorder f r

let rec inorder f = function
    Empty        -> ()
  | Node (v,l,r) -> inorder f l;
                    f v;
                    inorder f r

let rec postorder f = function
    Empty        -> ()
  | Node (v,l,r) -> postorder f l;
                    postorder f r;
                    f v

let levelorder f x =
  let queue = Queue.create () in
    Queue.add x queue;
    while not (Queue.is_empty queue) do
      match Queue.take queue with
          Empty        -> ()
        | Node (v,l,r) -> f v;
                          Queue.add l queue;
                          Queue.add r queue
    done

let tree =
  Node (1,
        Node (2,
              Node (4,
                    Node (7, Empty, Empty),
                    Empty),
              Node (5, Empty, Empty)),
        Node (3,
              Node (6,
                    Node (8, Empty, Empty),
                    Node (9, Empty, Empty)),
              Empty))

let () =
  preorder   (Printf.printf "%d ") tree; print_newline ();
  inorder    (Printf.printf "%d ") tree; print_newline ();
  postorder  (Printf.printf "%d ") tree; print_newline ();
  levelorder (Printf.printf "%d ") tree; print_newline ()
```

Output:

```txt
1 2 4 7 5 3 6 8 9
7 4 2 5 1 8 6 9 3
2 4 7 5 3 6 8 9 1
1 2 3 4 5 6 7 8 9
```



## Oforth



```Oforth
Object Class new: Tree(v, l, r)

Tree method: initialize(v, l, r)  v := v l := l r := r ;
Tree method: v   @v ;
Tree method: l   @l ;
Tree method: r   @r ;

Tree method: preOrder(f)
   @v f perform
   @l ifNotNull: [ @l preOrder(f) ]
   @r ifNotNull: [ @r preOrder(f) ] ;

Tree method: inOrder(f)
   @l ifNotNull: [ @l inOrder(f) ]
   @v f perform
   @r ifNotNull: [ @r inOrder(f) ] ;

Tree method: postOrder(f)
   @l ifNotNull: [ @l postOrder(f) ]
   @r ifNotNull: [ @r postOrder(f) ]
   @v f perform ;

Tree method: levelOrder(f)
| c n |
   Channel new self over send drop ->c
   while(c notEmpty) [
      c receive ->n
      n v f perform
      n l dup ifNotNull: [ c send ] drop
      n r dup ifNotNull: [ c send ] drop
      ] ;
```


{{out}}

```txt

>Tree new(3, Tree new(6, Tree new(8, null, null), Tree new(9, null, null)), null)
ok
>Tree new(2, Tree new(4, Tree new(7, null, null), null), Tree new(5, null, null))
ok
>1 Tree new
ok
>
ok
>dup preOrder(#.)
1 2 4 7 5 3 6 8 9 ok
>dup inOrder(#.)
7 4 2 5 1 8 6 9 3 ok
>dup postOrder(#.)
7 4 5 2 8 9 6 3 1 ok
>dup levelOrder(#.)
1 2 3 4 5 6 7 8 9 ok

```



## ooRexx


```ooRexx

  one = .Node~new(1);
  two = .Node~new(2);
  three = .Node~new(3);
  four = .Node~new(4);
  five = .Node~new(5);
  six = .Node~new(6);
  seven = .Node~new(7);
  eight = .Node~new(8);
  nine = .Node~new(9);

  one~left = two
  one~right = three
  two~left = four
  two~right = five
  three~left = six
  four~left = seven
  six~left = eight
  six~right = nine

  out = .array~new
  .treetraverser~preorder(one, out);
  say "Preorder:  " out~toString("l", ", ")
  out~empty
  .treetraverser~inorder(one, out);
  say "Inorder:   " out~toString("l", ", ")
  out~empty
  .treetraverser~postorder(one, out);
  say "Postorder: " out~toString("l", ", ")
  out~empty
  .treetraverser~levelorder(one, out);
  say "Levelorder:" out~toString("l", ", ")


::class node
::method init
  expose left right data
  use strict arg data
  left = .nil
  right = .nil

::attribute left
::attribute right
::attribute data

::class treeTraverser
::method preorder class
  use arg node, out
  if node \== .nil then do
      out~append(node~data)
      self~preorder(node~left, out)
      self~preorder(node~right, out)
  end

::method inorder class
  use arg node, out
  if node \== .nil then do
      self~inorder(node~left, out)
      out~append(node~data)
      self~inorder(node~right, out)
  end

::method postorder class
  use arg node, out
  if node \== .nil then do
      self~postorder(node~left, out)
      self~postorder(node~right, out)
      out~append(node~data)
  end

::method levelorder class
  use arg node, out

  if node == .nil then return
  nodequeue = .queue~new
  nodequeue~queue(node)
  loop while \nodequeue~isEmpty
      next = nodequeue~pull
      out~append(next~data)
      if next~left \= .nil then
          nodequeue~queue(next~left)
      if next~right \= .nil then
          nodequeue~queue(next~right)
  end

```

Output:

```txt

Preorder:   1, 2, 4, 7, 5, 3, 6, 8, 9
Inorder:    7, 4, 2, 5, 1, 8, 6, 9, 3
Postorder:  7, 4, 5, 2, 8, 9, 6, 3, 1
Levelorder: 1, 2, 3, 4, 5, 6, 7, 8, 9

```



## Oz


```oz
declare
  Tree = n(1
           n(2
             n(4 n(7 e e) e)
             n(5 e e))
           n(3
             n(6 n(8 e e) n(9 e e))
             e))

  fun {Concat Xs}
     {FoldR Xs Append nil}
  end

  fun {Preorder T}
     case T of e then nil
     [] n(V L R) then
        {Concat [[V]
                 {Preorder L}
                 {Preorder R}]}
     end
  end

  fun {Inorder T}
     case T of e then nil
     [] n(V L R) then
        {Concat [{Inorder L}
                 [V]
                 {Inorder R}]}
     end
  end

  fun {Postorder T}
     case T of e then nil
     [] n(V L R) then
        {Concat [{Postorder L}
                 {Postorder R}
                 [V]]}
     end
  end

  local
     fun {Collect Queue}
        case Queue of nil then nil
        [] e|Xr then {Collect Xr}
        [] n(V L R)|Xr then
           V|{Collect {Append Xr [L R]}}
        end
     end
  in
     fun {Levelorder T}
        {Collect [T]}
     end
  end
in
  {Show {Preorder Tree}}
  {Show {Inorder Tree}}
  {Show {Postorder Tree}}
  {Show {Levelorder Tree}}
```



## Perl

Tree nodes are represented by 3-element arrays: [0] - the value; [1] - left child; [2] - right child.

```perl
sub preorder
{
	my $t = shift or return ();
	return ($t->[0], preorder($t->[1]), preorder($t->[2]));
}

sub inorder
{
	my $t = shift or return ();
	return (inorder($t->[1]), $t->[0], inorder($t->[2]));
}

sub postorder
{
	my $t = shift or return ();
	return (postorder($t->[1]), postorder($t->[2]), $t->[0]);
}

sub depth
{
	my @ret;
	my @a = ($_[0]);
	while (@a) {
		my $v = shift @a or next;
		push @ret, $v->[0];
		push @a, @{$v}[1,2];
	}
	return @ret;
}

my $x = [1,[2,[4,[7]],[5]],[3,[6,[8],[9]]]];

print "pre:   @{[preorder($x)]}\n";
print "in:    @{[inorder($x)]}\n";
print "post:  @{[postorder($x)]}\n";
print "depth: @{[depth($x)]}\n";
```

Output:

```txt
pre:   1 2 4 7 5 3 6 8 9
in:    7 4 2 5 1 8 6 9 3
post:  7 4 5 2 8 9 6 3 1
depth: 1 2 3 4 5 6 7 8 9
```



## Perl 6


```perl6
class TreeNode {
    has TreeNode $.parent;
    has TreeNode $.left;
    has TreeNode $.right;
    has $.value;

    method pre-order {
        flat gather {
            take $.value;
            take $.left.pre-order if $.left;
            take $.right.pre-order if $.right
        }
    }

    method in-order {
        flat gather {
            take $.left.in-order if $.left;
            take $.value;
            take $.right.in-order if $.right;
        }
    }

    method post-order {
        flat gather {
            take $.left.post-order if $.left;
            take $.right.post-order if $.right;
            take $.value;
        }
    }

    method level-order {
        my TreeNode @queue = (self);
        flat gather while @queue.elems {
            my $n = @queue.shift;
            take $n.value;
            @queue.push($n.left) if $n.left;
            @queue.push($n.right) if $n.right;
        }
    }
}

my TreeNode $root .= new( value => 1,
                    left => TreeNode.new( value => 2,
                            left => TreeNode.new( value => 4, left => TreeNode.new(value => 7)),
                            right => TreeNode.new( value => 5)
                    ),
                    right => TreeNode.new( value => 3,
                             left => TreeNode.new( value => 6,
                                     left => TreeNode.new(value => 8),
                                     right => TreeNode.new(value => 9)
                                     )
                             )
                    );

say "preorder:  ",$root.pre-order.join(" ");
say "inorder:   ",$root.in-order.join(" ");
say "postorder: ",$root.post-order.join(" ");
say "levelorder:",$root.level-order.join(" ");
```

{{out}}

```txt
preorder:  1 2 4 7 5 3 6 8 9
inorder:   7 4 2 5 1 8 6 9 3
postorder: 7 4 5 2 8 9 6 3 1
levelorder:1 2 3 4 5 6 7 8 9
```



## Phix

Copy of [[Tree_traversal#Euphoria|Euphoria]].
This is included in the distribution as demo\rosetta\Tree_traversal.exw, which also contains a way to build such a nested structure, and thirdly a "flat list of nodes" tree, that allows more interesting options such as a tag sort.

```Phix
constant VALUE = 1, LEFT = 2, RIGHT = 3

constant tree = {1, {2, {4, {7, 0, 0}, 0},
                        {5, 0, 0}},
                    {3, {6, {8, 0, 0},
                            {9, 0, 0}},
                        0}}

procedure preorder(object tree)
    if sequence(tree) then
        printf(1,"%d ",{tree[VALUE]})
        preorder(tree[LEFT])
        preorder(tree[RIGHT])
    end if
end procedure

procedure inorder(object tree)
    if sequence(tree) then
        inorder(tree[LEFT])
        printf(1,"%d ",{tree[VALUE]})
        inorder(tree[RIGHT])
    end if
end procedure

procedure postorder(object tree)
    if sequence(tree) then
        postorder(tree[LEFT])
        postorder(tree[RIGHT])
        printf(1,"%d ",{tree[VALUE]})
    end if
end procedure

procedure level_order(object tree, sequence more = {})
    if sequence(tree) then
        more &= {tree[LEFT],tree[RIGHT]}
        printf(1,"%d ",{tree[VALUE]})
    end if
    if length(more) > 0 then
        level_order(more[1],more[2..$])
    end if
end procedure

puts(1,"\n preorder:    ")  preorder(tree)
puts(1,"\n inorder:     ")  inorder(tree)
puts(1,"\n postorder:   ")  postorder(tree)
puts(1,"\n level-order: ")  level_order(tree)
```

{{out}}

```txt

 preorder:    1 2 4 7 5 3 6 8 9
 inorder:     7 4 2 5 1 8 6 9 3
 postorder:   7 4 5 2 8 9 6 3 1
 level-order: 1 2 3 4 5 6 7 8 9

```



## PHP


```PHP
class Node {
    private $left;
    private $right;
    private $value;

    function __construct($value) {
        $this->value = $value;
    }

    public function getLeft() {
        return $this->left;
    }
    public function getRight() {
        return $this->right;
    }
    public function getValue() {
        return $this->value;
    }

    public function setLeft($value) {
        $this->left = $value;
    }
    public function setRight($value) {
        $this->right = $value;
    }
    public function setValue($value) {
        $this->value = $value;
    }
}

class TreeTraversal {

    public function preOrder(Node $n) {
        echo $n->getValue() . " ";
        if($n->getLeft() != null) {
            $this->preOrder($n->getLeft());
        }
        if($n->getRight() != null){
            $this->preOrder($n->getRight());
        }
    }

    public function inOrder(Node $n) {
        if($n->getLeft() != null) {
            $this->inOrder($n->getLeft());
        }
        echo $n->getValue() . " ";
        if($n->getRight() != null){
            $this->inOrder($n->getRight());
        }

    }

    public function postOrder(Node $n) {
        if($n->getLeft() != null) {
            $this->postOrder($n->getLeft());
        }
        if($n->getRight() != null){
            $this->postOrder($n->getRight());
        }
        echo $n->getValue() . " ";
    }

    public function levelOrder($arg) {
        $q[] = $arg;
        while (!empty($q)) {
            $n = array_shift($q);
            echo $n->getValue() . " ";
            if($n->getLeft() != null) {
                $q[] = $n->getLeft();
            }
            if($n->getRight() != null){
                $q[] = $n->getRight();
            }
        }
    }
}

$arr = [];
for ($i=1; $i < 10; $i++) {
    $arr[$i] = new Node($i);
}

$arr[6]->setLeft($arr[8]);
$arr[6]->setRight($arr[9]);
$arr[3]->setLeft($arr[6]);
$arr[4]->setLeft($arr[7]);
$arr[2]->setLeft($arr[4]);
$arr[2]->setRight($arr[5]);
$arr[1]->setLeft($arr[2]);
$arr[1]->setRight($arr[3]);

$tree = new TreeTraversal($arr);

echo "preorder:\t";
$tree->preOrder($arr[1]);
echo "\ninorder:\t";
$tree->inOrder($arr[1]);
echo "\npostorder:\t";
$tree->postOrder($arr[1]);
echo "\nlevel-order:\t";
$tree->levelOrder($arr[1]);
```

Output:

```txt
preorder:    1 2 4 7 5 3 6 8 9
inorder:     7 4 2 5 1 8 6 9 3
postorder:   7 4 5 2 8 9 6 3 1
level-order: 1 2 3 4 5 6 7 8 9
```



## PicoLisp


```PicoLisp
(de preorder (Node Fun)
   (when Node
      (Fun (car Node))
      (preorder (cadr Node) Fun)
      (preorder (caddr Node) Fun) ) )

(de inorder (Node Fun)
   (when Node
      (inorder (cadr Node) Fun)
      (Fun (car Node))
      (inorder (caddr Node) Fun) ) )

(de postorder (Node Fun)
   (when Node
      (postorder (cadr Node) Fun)
      (postorder (caddr Node) Fun)
      (Fun (car Node)) ) )

(de level-order (Node Fun)
   (for (Q (circ Node)  Q)
      (let N (fifo 'Q)
         (Fun (car N))
         (and (cadr N) (fifo 'Q @))
         (and (caddr N) (fifo 'Q @)) ) ) )

(setq *Tree
   (1
      (2 (4 (7)) (5))
      (3 (6 (8) (9))) ) )

(for Order '(preorder inorder postorder level-order)
   (prin (align -13 (pack Order ":")))
   (Order *Tree printsp)
   (prinl) )
```

Output:

```txt
preorder:    1 2 4 7 5 3 6 8 9
inorder:     7 4 2 5 1 8 6 9 3
postorder:   7 4 5 2 8 9 6 3 1
level-order: 1 2 3 4 5 6 7 8 9
```



## Prolog

Works with SWI-Prolog.

```Prolog
tree :-
	Tree= [1,
	        [2,
		   [4,
		     [7, nil, nil],
		     nil],
		   [5, nil, nil]],
	        [3,
		 [6,
		   [8, nil, nil],
		   [9,nil, nil]],
		 nil]],

	write('preorder    : '), preorder(Tree), nl,
	write('inorder     : '), inorder(Tree), nl,
	write('postorder   : '), postorder(Tree), nl,
	write('level-order : '), level_order([Tree]).

preorder(nil).
preorder([Node, FG, FD]) :-
	format('~w ', [Node]),
	preorder(FG),
	preorder(FD).


inorder(nil).
inorder([Node, FG, FD]) :-
	inorder(FG),
	format('~w ', [Node]),
	inorder(FD).

postorder(nil).
postorder([Node, FG, FD]) :-
	postorder(FG),
	postorder(FD),
	format('~w ', [Node]).


level_order([]).

level_order(A) :-
	level_order_(A, U-U, S),
	level_order(S).

level_order_([], S-[],S).

level_order_([[Node, FG, FD] | T], CS, FS) :-
	format('~w ', [Node]),
	append_dl(CS, [FG, FD|U]-U, CS1),
	level_order_(T, CS1, FS).

level_order_([nil | T], CS, FS) :-
	level_order_(T, CS, FS).


append_dl(X-Y, Y-Z, X-Z).

```

Output :

```txt
?- tree.
preorder    : 1 2 4 7 5 3 6 8 9
inorder     : 7 4 2 5 1 8 6 9 3
postorder   : 7 4 5 2 8 9 6 3 1
level-order : 1 2 3 4 5 6 7 8 9
true .

```



## PureBasic

{{works with|PureBasic|4.5+}}

```PureBasic
Structure node
  value.i
  *left.node
  *right.node
EndStructure

Structure queue
  List q.i()
EndStructure

DataSection
  tree:
  Data.s "1(2(4(7),5),3(6(8,9)))"
EndDataSection

;Convenient routine to interpret string data to construct a tree of integers.
Procedure createTree(*n.node, *tPtr.Character)
  Protected num.s, *l.node, *ntPtr.Character

  Repeat
    Select *tPtr\c
      Case '0' To '9'
        num + Chr(*tPtr\c)
      Case '('
        *n\value = Val(num): num = ""
        *ntPtr = *tPtr + 1
        If *ntPtr\c = ','
          ProcedureReturn *tPtr
        Else
          *l = AllocateMemory(SizeOf(node))
          *n\left = *l: *tPtr = createTree(*l, *ntPtr)
        EndIf
      Case ')', ',', #Null
        If num: *n\value = Val(num): EndIf
        ProcedureReturn *tPtr
    EndSelect

    If *tPtr\c = ','
      *l = AllocateMemory(SizeOf(node)):
      *n\right = *l: *tPtr = createTree(*l, *tPtr + 1)
    EndIf
    *tPtr + 1
  ForEver
EndProcedure

Procedure enqueue(List q.i(), element)
  LastElement(q())
  AddElement(q())
  q() = element
EndProcedure

Procedure dequeue(List q.i())
  Protected element
  If FirstElement(q())
    element = q()
    DeleteElement(q())
  EndIf
  ProcedureReturn element
EndProcedure

Procedure onVisit(*n.node)
  Print(Str(*n\value) + " ")
EndProcedure

Procedure preorder(*n.node) ;recursive
  onVisit(*n)
  If *n\left
    preorder(*n\left)
  EndIf
  If *n\right
    preorder(*n\right)
  EndIf
EndProcedure

Procedure inorder(*n.node) ;recursive
  If *n\left
    inorder(*n\left)
  EndIf
  onVisit(*n)
  If *n\right
    inorder(*n\right)
  EndIf
EndProcedure

Procedure postorder(*n.node) ;recursive
  If *n\left
    postorder(*n\left)
  EndIf
  If *n\right
    postorder(*n\right)
  EndIf
  onVisit(*n)
EndProcedure

Procedure levelorder(*n.node)
  Dim q.queue(1)
  Protected readQueue = 1, writeQueue, *currNode.node

  enqueue(q(writeQueue)\q(),*n) ;start queue off with root
  Repeat
    readQueue ! 1: writeQueue ! 1
    While ListSize(q(readQueue)\q())
      *currNode = dequeue(q(readQueue)\q())
      If *currNode\left
        enqueue(q(writeQueue)\q(),*currNode\left)
      EndIf
      If *currNode\right
        enqueue(q(writeQueue)\q(),*currNode\right)
      EndIf
      onVisit(*currNode)
    Wend
  Until ListSize(q(writeQueue)\q()) = 0
EndProcedure

If OpenConsole()
  Define root.node
  createTree(root,?tree)

  Print("preorder: ")
  preorder(root)
  PrintN("")
  Print("inorder: ")
  inorder(root)
  PrintN("")
  Print("postorder: ")
  postorder(root)
  PrintN("")
  Print("levelorder: ")
  levelorder(root)
  PrintN("")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
preorder: 1 2 4 7 5 3 6 8 9
inorder: 7 4 2 5 1 8 6 9 3
postorder: 7 4 5 2 8 9 6 3 1
levelorder: 1 2 3 4 5 6 7 8 9
```



## Python



### Python: Procedural



```python
from collections import namedtuple

Node = namedtuple('Node', 'data, left, right')
tree = Node(1,
            Node(2,
                 Node(4,
                      Node(7, None, None),
                      None),
                 Node(5, None, None)),
            Node(3,
                 Node(6,
                      Node(8, None, None),
                      Node(9, None, None)),
                 None))

def printwithspace(i):
    print(i, end=' ')

def dfs(order, node, visitor):
    if node is not None:
        for action in order:
            if action == 'N':
                visitor(node.data)
            elif action == 'L':
                dfs(order, node.left, visitor)
            elif action == 'R':
                dfs(order, node.right, visitor)

def preorder(node, visitor = printwithspace):
    dfs('NLR', node, visitor)

def inorder(node, visitor = printwithspace):
    dfs('LNR', node, visitor)

def postorder(node, visitor = printwithspace):
    dfs('LRN', node, visitor)

def ls(node, more, visitor, order='TB'):
    "Level-based Top-to-Bottom or Bottom-to-Top tree search"
    if node:
        if more is None:
            more = []
        more += [node.left, node.right]
    for action in order:
        if action == 'B' and more:
            ls(more[0], more[1:], visitor, order)
        elif action == 'T' and node:
            visitor(node.data)

def levelorder(node, more=None, visitor = printwithspace):
    ls(node, more, visitor, 'TB')

# Because we can
def reverse_preorder(node, visitor = printwithspace):
    dfs('RLN', node, visitor)

def bottom_up_order(node, more=None, visitor = printwithspace, order='BT'):
    ls(node, more, visitor, 'BT')


if __name__ == '__main__':
    w = 10
    for traversal in [preorder, inorder, postorder, levelorder,
                      reverse_preorder, bottom_up_order]:
        if traversal == reverse_preorder:
            w = 20
            print('\nThe generalisation of function dfs allows:')
        if traversal == bottom_up_order:
            print('The generalisation of function ls allows:')
        print(f"{traversal.__name__:>{w}}:", end=' ')
        traversal(tree)
        print()
```


'''Sample output:'''

```txt
  preorder: 1 2 4 7 5 3 6 8 9
   inorder: 7 4 2 5 1 8 6 9 3
 postorder: 7 4 5 2 8 9 6 3 1
levelorder: 1 2 3 4 5 6 7 8 9

The generalisation of function dfs allows:
    reverse_preorder: 9 8 6 3 5 7 4 2 1
The generalisation of function ls allows:
     bottom_up_order: 9 8 7 6 5 4 3 2 1
```



### Python: Class based


Subclasses a namedtuple adding traversal methods that apply a visitor function to data at nodes of the tree in order

```python
from collections import namedtuple
from sys import stdout

class Node(namedtuple('Node', 'data, left, right')):
    __slots__ = ()

    def preorder(self, visitor):
        if self is not None:
            visitor(self.data)
            Node.preorder(self.left, visitor)
            Node.preorder(self.right, visitor)

    def inorder(self, visitor):
        if self is not None:
            Node.inorder(self.left, visitor)
            visitor(self.data)
            Node.inorder(self.right, visitor)

    def postorder(self, visitor):
        if self is not None:
            Node.postorder(self.left, visitor)
            Node.postorder(self.right, visitor)
            visitor(self.data)

    def levelorder(self, visitor, more=None):
        if self is not None:
            if more is None:
                more = []
            more += [self.left, self.right]
            visitor(self.data)
        if more:
            Node.levelorder(more[0], visitor, more[1:])


def printwithspace(i):
    stdout.write("%i " % i)


tree = Node(1,
            Node(2,
                 Node(4,
                      Node(7, None, None),
                      None),
                 Node(5, None, None)),
            Node(3,
                 Node(6,
                      Node(8, None, None),
                      Node(9, None, None)),
                 None))


if __name__ == '__main__':
    stdout.write('  preorder: ')
    tree.preorder(printwithspace)
    stdout.write('\n   inorder: ')
    tree.inorder(printwithspace)
    stdout.write('\n postorder: ')
    tree.postorder(printwithspace)
    stdout.write('\nlevelorder: ')
    tree.levelorder(printwithspace)
    stdout.write('\n')
```


{{out}}
As above.

===Python: Composition of pure (curried) functions===

Currying by default is probably not particularly 'Pythonic', but it does work well with higher-order functions – giving us more flexibility in compositional structure. It also often protects us from over-proliferation of the slightly noisy '''lambda''' keyword. (See for example the use of the curried version of '''map''' in the code below).

The approach taken here is to focus on the evaluation of expressions, rather than the sequencing of procedures. To keep evaluation simple and easily rearranged, mutation is stripped back wherever possible, and 'pure' functions, with inputs and outputs but, ideally, with no side-effects (and no sensitivities to global variables) are the basic building-block.

Composing pure functions also works well with library-building and code reuse – the literature on functional programming (particularly in the ML / OCaml / Haskell tradition) is rich in reusable abstractions for our toolkit. Some of them have already been absorbed, with standard or adjusted names, into the Python itertools module. (See the itertools module preface, and the '''takewhile''' function below).

Here, for example, for the '''pre-''', '''in-''' and '''post-''' orders, we can define a very general and reusable '''foldTree''' (a catamorphism over trees rather than lists) and just pass 3 different (rather simple) sequencing functions to it.

This level of abstraction and reuse brings real efficiencies – the short and easily-written '''foldTree''', for example, doesn't just traverse and list contents in flexible orders - we can pass all kinds of things to it. For the '''sum''' of all the numbers in the tree, we could write:


```python
'''Tree traversals'''

from itertools import (chain, takewhile)
from functools import (reduce)
from operator import (mul)


# foldTree :: (a -> [b] -> b) -> Tree a -> b
def foldTree(f):
    '''The catamorphism on trees.
       A summary value derived by a depth-first fold.'''
    def go(node):
        return f(root(node))(
            list(map(go, nest(node)))
        )
    return lambda tree: go(tree)


# levels :: Tree a -> [[a]]
def levels(tree):
    '''A list of the nodes at each level of the tree.'''
    fmap = curry(map)
    return list(fmap(fmap(root))(
        takewhile(
            bool,
            iterate(concatMap(nest))([tree])
        )
    ))


# preorder :: a -> [[a]] -> [a]
def preorder(x):
    '''This node followed by the rest.'''
    return lambda xs: [x] + concat(xs)


# inorder :: a -> [[a]] -> [a]
def inorder(x):
    '''Descendants of any first child,
       then this node, then the rest.'''
    return lambda xs: (
        xs[0] + [x] + concat(xs[1:]) if xs else [x]
    )


# postorder :: a -> [[a]] -> [a]
def postorder(x):
    '''Descendants first, then this node.'''
    return lambda xs: concat(xs) + [x]


# levelorder :: Tree a -> [a]
def levelorder(tree):
    '''Top-down concatenation of this node
       with the rows below.'''
    return concat(levels(tree))


# treeSum :: Tree Int -> Int
def treeSum(x):
    '''This node's value + the sum of its descendants.'''
    return lambda xs: x + sum(xs)


# treeSum :: Tree Int -> Int
def treeProduct(x):
    '''This node's value * the product of its descendants.'''
    return lambda xs: x * numericProduct(xs)


# treeMax :: Tree Int -> Int
def treeMax(x):
    '''Maximum value of this node and any descendants.'''
    return lambda xs: max([x] + xs)


# treeMin :: Tree Int -> Int
def treeMin(x):
    '''Minimum value of this node and any descendants.'''
    return lambda xs: min([x] + xs)


# nodeCount :: Tree a -> Int
def nodeCount(_):
    '''One more than the total number of descendants.'''
    return lambda xs: 1 + sum(xs)


# treeWidth :: Tree a -> Int
def treeWidth(_):
    '''Sum of widths of any children, or a minimum of 1.'''
    return lambda xs: sum(xs) if xs else 1


# treeDepth :: Tree a -> Int
def treeDepth(_):
    '''One more than that of the deepest child.'''
    return lambda xs: 1 + (max(xs) if xs else 0)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Tree traversals - accumulating and folding'''

    # tree :: Tree Int
    tree = Node(1)([
        Node(2)([
            Node(4)([
                Node(7)([])
            ]),
            Node(5)([])
        ]),
        Node(3)([
            Node(6)([
                Node(8)([]),
                Node(9)([])
            ])
        ])
    ])

    print(
        fTable(main.__doc__ + ':\n')(fName)(str)(
            lambda f: (
                foldTree(f) if 'levelorder' != fName(f) else f
            )(tree)
        )([
            preorder, inorder, postorder, levelorder,
            treeSum, treeProduct, treeMin, treeMax,
            nodeCount, treeWidth, treeDepth
        ])
    )


# GENERIC -------------------------------------------------

# Node :: a -> [Tree a] -> Tree a
def Node(v):
    '''Contructor for a Tree node which connects a
       value of some kind to a list of zero or
       more child trees.'''
    return lambda xs: {'type': 'Node', 'root': v, 'nest': xs}


# nest :: Tree a -> [Tree a]
def nest(tree):
    '''Accessor function for children of tree node'''
    return tree['nest'] if 'nest' in tree else None


# root :: Dict -> a
def root(tree):
    '''Accessor function for data of tree node'''
    return tree['root'] if 'root' in tree else None


# concat :: [[a]] -> [a]
# concat :: [String] -> String
def concat(xxs):
    '''The concatenation of all the elements in a list.'''
    xs = list(chain.from_iterable(xxs))
    unit = '' if isinstance(xs, str) else []
    return unit if not xs else (
        ''.join(xs) if isinstance(xs[0], str) else xs
    )


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''Concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output a in list,
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# curry :: ((a, b) -> c) -> a -> b -> c
def curry(f):
    '''A curried function derived
       from an uncurried function.'''
    return lambda a: lambda b: f(a, b)


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated applications of f to x.'''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


# numericProduct :: [Num] -> Num
def numericProduct(xs):
    '''The arithmetic product of all numbers in xs.'''
    return reduce(mul, xs, 1)


# FORMATTING ----------------------------------------------

# fName :: (a -> b) -> String
def fName(f):
    '''The name bound to the function.'''
    return f.__name__


# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Tree traversals - accumulating and folding:

   preorder -> [1, 2, 4, 7, 5, 3, 6, 8, 9]
    inorder -> [7, 4, 2, 5, 1, 8, 6, 9, 3]
  postorder -> [7, 4, 5, 2, 8, 9, 6, 3, 1]
 levelorder -> [1, 2, 3, 4, 5, 6, 7, 8, 9]
    treeSum -> 45
treeProduct -> 362880
    treeMin -> 1
    treeMax -> 9
  nodeCount -> 9
  treeWidth -> 4
  treeDepth -> 4
```



## Qi


```qi

(set *tree* [1 [2 [4 [7]]
                  [5]]
               [3 [6 [8]
                     [9]]]])

(define inorder
  []      -> []
  [V]     -> [V]
  [V L]   -> (append (inorder L)
                     [V])
  [V L R] -> (append (inorder L)
                     [V]
                     (inorder R)))

(define postorder
  []      -> []
  [V]     -> [V]
  [V L]   -> (append (postorder L)
                     [V])
  [V L R] -> (append (postorder L)
                     (postorder R)
                     [V]))

(define preorder
  []      -> []
  [V]     -> [V]
  [V L]   -> (append [V]
                     (preorder L))
  [V L R] -> (append [V]
                     (preorder L)
                     (preorder R)))

(define levelorder-0
  []             -> []
  [[]       | Q] -> (levelorder-0 Q)
  [[V | LR] | Q] -> [V | (levelorder-0 (append Q LR))])

(define levelorder
  Node -> (levelorder-0 [Node]))

(preorder (value *tree*))
(postorder (value *tree*))
(inorder (value *tree*))
(levelorder (value *tree*))

```


Output:

```txt
[1 2 4 7 5 3 6 8 9]
[7 4 2 5 1 8 6 9 3]
[7 4 5 2 8 9 6 3 1]
[1 2 3 4 5 6 7 8 9]
```



## Racket



```racket

#lang racket

(define the-tree ; Node: (list <data> <left> <right>)
  '(1 (2 (4 (7 #f #f) #f) (5 #f #f)) (3 (6 (8 #f #f) (9 #f #f)) #f)))

(define (preorder tree visit)
  (let loop ([t tree])
    (when t (visit (car t)) (loop (cadr t)) (loop (caddr t)))))
(define (inorder tree visit)
  (let loop ([t tree])
    (when t (loop (cadr t)) (visit (car t)) (loop (caddr t)))))
(define (postorder tree visit)
  (let loop ([t tree])
    (when t (loop (cadr t)) (loop (caddr t)) (visit (car t)))))
(define (levelorder tree visit)
  (let loop ([trees (list tree)])
    (unless (null? trees)
      ((compose1 loop (curry filter values) append*)
       (for/list ([t trees] #:when t) (visit (car t)) (cdr t))))))

(define (run order)
  (printf "~a:" (object-name order))
  (order the-tree (λ(x) (printf " ~s" x)))
  (newline))
(for-each run (list preorder inorder postorder levelorder))

```


Output:

```txt

preorder: 1 2 4 7 5 3 6 8 9
inorder: 7 4 2 5 1 8 6 9 3
postorder: 7 4 5 2 8 9 6 3 1
levelorder: 1 2 3 4 5 6 7 8 9

```



## REXX


```rexx

/* REXX ***************************************************************
* Tree traversal
=           1
=          / \
=         /   \
=        /     \
=       2       3
=      / \     /
=     4   5   6
=    /       / \
=   7       8   9
=
= The correct output should look like this:
=  preorder:    1 2 4 7 5 3 6 8 9
=  level-order: 1 2 3 4 5 6 7 8 9
=  postorder:   7 4 5 2 8 9 6 3 1
=  inorder:     7 4 2 5 1 8 6 9 3

* 17.06.2012 Walter Pachl not thoroughly tested
**********************************************************************/
debug=0
wl_soll=1 2 4 7 5 3 6 8 9
il_soll=7 4 2 5 1 8 6 9 3
pl_soll=7 4 5 2 8 9 6 3 1
ll_soll=1 2 3 4 5 6 7 8 9

Call mktree
wl.=''; wl='' /* preorder    */
ll.=''; ll='' /* level-order */
        il='' /* inorder     */
        pl='' /* postorder   */

/**********************************************************************
* First walk the tree and construct preorder and level-order lists
**********************************************************************/
done.=0
lvl=1
z=root
Call note z
Do Until z=0
  z=go_next(z)
  Call note z
  End
Call show 'preorder:   ',wl,wl_soll
Do lvl=1 To 4
  ll=ll ll.lvl
  End
Call show 'level-order:',ll,ll_soll

/**********************************************************************
* Next construct postorder list
**********************************************************************/
done.=0
ridone.=0
z=lbot(root)
Call notep z
Do Until z=0
  br=brother(z)
  If br>0 &,
     done.br=0 Then Do
    ridone.br=1
    z=lbot(br)
    Call notep z
    End
  Else
  z=father(z)
  Call notep z
  End
Call show 'postorder:  ',pl,pl_soll

/**********************************************************************
* Finally construct inorder list
**********************************************************************/
done.=0
ridone.=0
z=lbot(root)
Call notei z
Do Until z=0
  z=father(z)
  Call notei z
  ri=node.z.0rite
  If ridone.z=0 Then Do
    ridone.z=1
    If ri>0 Then Do
      z=lbot(ri)
      Call notei z
      End
    End
  End

/**********************************************************************
* And now show the results and check them for correctness
**********************************************************************/
Call show 'inorder:    ',il,il_soll

Exit

show: Parse Arg Which,have,soll
/**********************************************************************
* Show our result and show it it's correct
**********************************************************************/
have=space(have)
If have=soll Then
  tag=''
Else
  tag='*wrong*'
Say which have tag
If tag<>'' Then
  Say '------------>'soll 'is the expected result'
Return

brother: Procedure Expose node.
/**********************************************************************
* Return the right node of this node's father or 0
**********************************************************************/
  Parse arg no
  nof=node.no.0father
  brot1=node.nof.0rite
  Return brot1

notei: Procedure Expose debug il done.
/**********************************************************************
* append the given node to il
**********************************************************************/
  Parse Arg nd
  If nd<>0 &,
     done.nd=0 Then
    il=il nd
  If debug Then
    Say 'notei' nd
  done.nd=1
  Return

notep: Procedure Expose debug pl done.
/**********************************************************************
* append the given node to pl
**********************************************************************/
  Parse Arg nd
  If nd<>0 &,
     done.nd=0 Then Do
    pl=pl nd
    If debug Then
      Say 'notep' nd
    End
  done.nd=1
  Return

father: Procedure Expose node.
/**********************************************************************
* Return the father of the argument
* or 0 if the root is given as argument
**********************************************************************/
  Parse Arg nd
  Return node.nd.0father

lbot: Procedure Expose node.
/**********************************************************************
* From node z: Walk down on the left side until you reach the bottom
* and return the bottom node
* If z has no left son (at the bottom of the tree) returm itself
**********************************************************************/
  Parse Arg z
  Do i=1 To 100
    If node.z.0left<>0 Then
      z=node.z.0left
    Else
      Leave
    End
  Return z

note:
/**********************************************************************
* add the node to the preorder list unless it's already there
* add the node to the level list
**********************************************************************/
  If z<>0 &,                           /* it's a node                */
     done.z=0 Then Do                  /* not yet done               */
    wl=wl z                            /* add it to the preorder list*/
    ll.lvl=ll.lvl z                    /* add it to the level list   */
    done.z=1                           /* remember it's done         */
    End
  Return

go_next: Procedure Expose node. lvl
/**********************************************************************
* find the next node to visit in the treewalk
**********************************************************************/
  next=0
  Parse arg z
  If node.z.0left<>0 Then Do           /* there is a left son        */
    If node.z.0left.done=0 Then Do     /* we have not visited it     */
      next=node.z.0left                /* so we go there             */
      node.z.0left.done=1              /* note we were here          */
      lvl=lvl+1                        /* increase the level         */
      End
    End
  If next=0 Then Do                    /* not moved yet              */
    If node.z.0rite<>0 Then Do         /* there is a right son       */
      If node.z.0rite.done=0 Then Do   /* we have not visited it     */
        next=node.z.0rite              /* so we go there             */
        node.z.0rite.done=1            /* note we were here          */
        lvl=lvl+1                      /* increase the level         */
        End
      End
    End
  If next=0 Then Do                    /* not moved yet              */
    next=node.z.0father                /* go to the father           */
    lvl=lvl-1                          /* decrease the level         */
    End
  Return next                          /* that's the next node       */
                                       /* or zero if we are done     */

mknode: Procedure Expose node.
/**********************************************************************
* create a new node
**********************************************************************/
  Parse Arg name
  z=node.0+1
  node.z.0name=name
  node.z.0father=0
  node.z.0left  =0
  node.z.0rite  =0
  node.0=z
  Return z                        /* number of the node just created */

attleft: Procedure Expose node.
/**********************************************************************
* make son the left son of father
**********************************************************************/
  Parse Arg son,father
  node.son.0father=father
  z=node.father.0left
  If z<>0 Then Do
    node.z.0father=son
    node.son.0left=z
    End
  node.father.0left=son
  Return

attrite: Procedure Expose node.
/**********************************************************************
* make son the right son of father
**********************************************************************/
  Parse Arg son,father
  node.son.0father=father
  z=node.father.0rite
  If z<>0 Then Do
    node.z.0father=son
    node.son.0rite=z
    End
  node.father.0rite=son
  le=node.father.0left
  If le>0 Then
    node.le.0brother=node.father.0rite
  Return

mktree: Procedure Expose node. root
/**********************************************************************
* build the tree according to the task
**********************************************************************/
  node.=0
  a=mknode('A'); root=a
  b=mknode('B'); Call attleft b,a
  c=mknode('C'); Call attrite c,a
  d=mknode('D'); Call attleft d,b
  e=mknode('E'); Call attrite e,b
  f=mknode('F'); Call attleft f,c
  g=mknode('G'); Call attleft g,d
  h=mknode('H'); Call attleft h,f
  i=mknode('I'); Call attrite i,f
  Call show_tree 1
  Return

show_tree: Procedure Expose node.
/**********************************************************************
* Show the tree
*         f
*     l1   1  r1
*   l   r   l   r
*  l r l r l r l r
* 12345678901234567890
**********************************************************************/
  Parse Arg f
  l.=''
                          l.1=overlay(f   ,l.1, 9)

  l1=node.f.0left        ;l.2=overlay(l1  ,l.2, 5)
/*b1=node.f.0brother     ;l.2=overlay(b1  ,l.2, 9) */
  r1=node.f.0rite        ;l.2=overlay(r1  ,l.2,13)

  l1g=node.l1.0left      ;l.3=overlay(l1g ,l.3, 3)
/*b1g=node.l1.0brother   ;l.3=overlay(b1g ,l.3, 5) */
  r1g=node.l1.0rite      ;l.3=overlay(r1g ,l.3, 7)

  l2g=node.r1.0left      ;l.3=overlay(l2g ,l.3,11)
/*b2g=node.r1.0brother   ;l.3=overlay(b2g ,l.3,13) */
  r2g=node.r1.0rite      ;l.3=overlay(r2g ,l.3,15)

  l1ls=node.l1g.0left    ;l.4=overlay(l1ls,l.4, 2)
/*b1ls=node.l1g.0brother ;l.4=overlay(b1ls,l.4, 3) */
  r1ls=node.l1g.0rite    ;l.4=overlay(r1ls,l.4, 4)

  l1rs=node.r1g.0left    ;l.4=overlay(l1rs,l.4, 6)
/*b1rs=node.r1g.0brother ;l.4=overlay(b1rs,l.4, 7) */
  r1rs=node.r1g.0rite    ;l.4=overlay(r1rs,l.4, 8)

  l2ls=node.l2g.0left    ;l.4=overlay(l2ls,l.4,10)
/*b2ls=node.l2g.0brother ;l.4=overlay(b2ls,l.4,11) */
  r2ls=node.l2g.0rite    ;l.4=overlay(r2ls,l.4,12)

  l2rs=node.r2g.0left    ;l.4=overlay(l2rs,l.4,14)
/*b2rs=node.r2g.0brother ;l.4=overlay(b2rs,l.4,15) */
  r2rs=node.r2g.0rite    ;l.4=overlay(r2rs,l.4,16)
  Do i=1 To 4
    Say translate(l.i,' ','0')
    Say ''
    End
  Return
```

{{out}}

```txt
        1

    2       3

  4   5   6

 7       8 9

preorder:    1 2 4 7 5 3 6 8 9
level-order: 1 2 3 4 5 6 7 8 9
postorder:   7 4 5 2 8 9 6 3 1
inorder:     7 4 2 5 1 8 6 9 3
```



## Ruby


```ruby
BinaryTreeNode = Struct.new(:value, :left, :right) do
  def self.from_array(nested_list)
    value, left, right = nested_list
    if value
      self.new(value, self.from_array(left), self.from_array(right))
    end
  end

  def walk_nodes(order, &block)
    order.each do |node|
      case node
      when :left  then left && left.walk_nodes(order, &block)
      when :self  then yield self
      when :right then right && right.walk_nodes(order, &block)
      end
    end
  end

  def each_preorder(&b)  walk_nodes([:self, :left, :right], &b) end
  def each_inorder(&b)   walk_nodes([:left, :self, :right], &b) end
  def each_postorder(&b) walk_nodes([:left, :right, :self], &b) end

  def each_levelorder
    queue = [self]
    until queue.empty?
      node = queue.shift
      yield node
      queue << node.left if node.left
      queue << node.right if node.right
    end
  end
end

root = BinaryTreeNode.from_array [1, [2, [4, 7], [5]], [3, [6, [8], [9]]]]

BinaryTreeNode.instance_methods.select{|m| m=~/.+order/}.each do |mthd|
  printf "%-11s ", mthd[5..-1] + ':'
  root.send(mthd) {|node| print "#{node.value} "}
  puts
end
```


{{out}}

```txt

preorder:   1 2 4 7 5 3 6 8 9
inorder:    7 4 2 5 1 8 6 9 3
postorder:  7 4 5 2 8 9 6 3 1
levelorder: 1 2 3 4 5 6 7 8 9
```



## Rust

This solution uses iteration (rather than recursion) for all traversal types.

```Rust

#![feature(box_syntax, box_patterns)]

use std::collections::VecDeque;

#[derive(Debug)]
struct TreeNode<T> {
    value: T,
    left: Option<Box<TreeNode<T>>>,
    right: Option<Box<TreeNode<T>>>,
}

enum TraversalMethod {
    PreOrder,
    InOrder,
    PostOrder,
    LevelOrder,
}

impl<T> TreeNode<T> {
    pub fn new(arr: &[[i8; 3]]) -> TreeNode<i8> {

        let l = match arr[0][1] {
            -1 => None,
            i @ _ => Some(Box::new(TreeNode::<i8>::new(&arr[(i - arr[0][0]) as usize..]))),
        };
        let r = match arr[0][2] {
            -1 => None,
            i @ _ => Some(Box::new(TreeNode::<i8>::new(&arr[(i - arr[0][0]) as usize..]))),
        };

        TreeNode {
            value: arr[0][0],
            left: l,
            right: r,
        }
    }

    pub fn traverse(&self, tr: &TraversalMethod) -> Vec<&TreeNode<T>> {
        match tr {
            &TraversalMethod::PreOrder => self.iterative_preorder(),
            &TraversalMethod::InOrder => self.iterative_inorder(),
            &TraversalMethod::PostOrder => self.iterative_postorder(),
            &TraversalMethod::LevelOrder => self.iterative_levelorder(),
        }
    }

    fn iterative_preorder(&self) -> Vec<&TreeNode<T>> {
        let mut stack: Vec<&TreeNode<T>> = Vec::new();
        let mut res: Vec<&TreeNode<T>> = Vec::new();

        stack.push(self);
        while !stack.is_empty() {
            let node = stack.pop().unwrap();
            res.push(node);
            match node.right {
                None => {}
                Some(box ref n) => stack.push(n),
            }
            match node.left {
                None => {}
                Some(box ref n) => stack.push(n),
            }
        }
        res
    }

    // Leftmost to rightmost
    fn iterative_inorder(&self) -> Vec<&TreeNode<T>> {
        let mut stack: Vec<&TreeNode<T>> = Vec::new();
        let mut res: Vec<&TreeNode<T>> = Vec::new();
        let mut p = self;

        loop {
            // Stack parents and right children while left-descending
            loop {
                match p.right {
                    None => {}
                    Some(box ref n) => stack.push(n),
                }
                stack.push(p);
                match p.left {
                    None => break,
                    Some(box ref n) => p = n,
                }
            }
            // Visit the nodes with no right child
            p = stack.pop().unwrap();
            while !stack.is_empty() && p.right.is_none() {
                res.push(p);
                p = stack.pop().unwrap();
            }
            // First node that can potentially have a right child:
            res.push(p);
            if stack.is_empty() {
                break;
            } else {
                p = stack.pop().unwrap();
            }
        }
        res
    }

    // Left-to-right postorder is same sequence as right-to-left preorder, reversed
    fn iterative_postorder(&self) -> Vec<&TreeNode<T>> {
        let mut stack: Vec<&TreeNode<T>> = Vec::new();
        let mut res: Vec<&TreeNode<T>> = Vec::new();

        stack.push(self);
        while !stack.is_empty() {
            let node = stack.pop().unwrap();
            res.push(node);
            match node.left {
                None => {}
                Some(box ref n) => stack.push(n),
            }
            match node.right {
                None => {}
                Some(box ref n) => stack.push(n),
            }
        }
        let rev_iter = res.iter().rev();
        let mut rev: Vec<&TreeNode<T>> = Vec::new();
        for elem in rev_iter {
            rev.push(elem);
        }
        rev
    }

    fn iterative_levelorder(&self) -> Vec<&TreeNode<T>> {
        let mut queue: VecDeque<&TreeNode<T>> = VecDeque::new();
        let mut res: Vec<&TreeNode<T>> = Vec::new();

        queue.push_back(self);
        while !queue.is_empty() {
            let node = queue.pop_front().unwrap();
            res.push(node);
            match node.left {
                None => {}
                Some(box ref n) => queue.push_back(n),
            }
            match node.right {
                None => {}
                Some(box ref n) => queue.push_back(n),
            }
        }
        res
    }
}

fn main() {
    // Array representation of task tree
    let arr_tree = [[1, 2, 3],
                    [2, 4, 5],
                    [3, 6, -1],
                    [4, 7, -1],
                    [5, -1, -1],
                    [6, 8, 9],
                    [7, -1, -1],
                    [8, -1, -1],
                    [9, -1, -1]];

    let root = TreeNode::<i8>::new(&arr_tree);

    for method_label in [(TraversalMethod::PreOrder, "pre-order:"),
                         (TraversalMethod::InOrder, "in-order:"),
                         (TraversalMethod::PostOrder, "post-order:"),
                         (TraversalMethod::LevelOrder, "level-order:")]
                            .iter() {
        print!("{}\t", method_label.1);
        for n in root.traverse(&method_label.0) {
            print!(" {}", n.value);
        }
        print!("\n");
    }
}

```

Output is same as Ruby et al.


## Scala

{{works with|Scala|2.11.x}}

```Scala
case class IntNode(value: Int, left: Option[IntNode] = None, right: Option[IntNode] = None) {

  def preorder(f: IntNode => Unit) {
    f(this)
    left.map(_.preorder(f)) // Same as: if(left.isDefined) left.get.preorder(f)
    right.map(_.preorder(f))
  }

  def postorder(f: IntNode => Unit) {
    left.map(_.postorder(f))
    right.map(_.postorder(f))
    f(this)
  }

  def inorder(f: IntNode => Unit) {
    left.map(_.inorder(f))
    f(this)
    right.map(_.inorder(f))
  }

  def levelorder(f: IntNode => Unit) {

    def loVisit(ls: List[IntNode]): Unit = ls match {
      case Nil => None
      case node :: rest => f(node); loVisit(rest ++ node.left ++ node.right)
    }

    loVisit(List(this))
  }
}

object TreeTraversal extends App {
  implicit def intNode2SomeIntNode(n: IntNode) = Some[IntNode](n)

  val tree = IntNode(1,
    IntNode(2,
      IntNode(4,
        IntNode(7)),
      IntNode(5)),
    IntNode(3,
      IntNode(6,
        IntNode(8),
        IntNode(9))))

  List(
    "  preorder: " -> tree.preorder _, // `_` denotes the function value of type `IntNode => Unit` (returning nothing)
    "   inorder: " -> tree.inorder _,
    " postorder: " -> tree.postorder _,
    "levelorder: " -> tree.levelorder _) foreach {
      case (name, func) =>
        val s = new StringBuilder(name)
        func(n => s ++= n.value.toString + " ")
        println(s)
    }
}
```


Output:
```txt

  preorder: 1 2 4 7 5 3 6 8 9
   inorder: 7 4 2 5 1 8 6 9 3
 postorder: 7 4 5 2 8 9 6 3 1
levelorder: 1 2 3 4 5 6 7 8 9

```



## SequenceL


```sequenceL

main(args(2)) :=
    "preorder: " ++ toString(preOrder(testTree)) ++
    "\ninoder: " ++ toString(inOrder(testTree)) ++
    "\npostorder: " ++ toString(postOrder(testTree)) ++
    "\nlevel-order: " ++ toString(levelOrder(testTree));

Node ::= (value : int, left : Node, right : Node);

preOrder(n) := [n.value] ++
               (preOrder(n.left) when isDefined(n, left) else []) ++
               (preOrder(n.right) when isDefined(n, right) else []);

inOrder(n) :=  (inOrder(n.left) when isDefined(n, left) else []) ++
               [n.value] ++
               (inOrder(n.right) when isDefined(n, right) else []);

postOrder(n) := (postOrder(n.left) when isDefined(n, left) else []) ++
                (postOrder(n.right) when isDefined(n, right) else []) ++
                [n.value];

levelOrder(n) := levelOrderHelper([n]);
levelOrderHelper(ns(1)) :=
    let
        n := head(ns);
    in
        [] when size(ns) = 0 else
        [n.value] ++ levelOrderHelper(tail(ns) ++
        ([n.left] when isDefined(n, left) else []) ++
        ([n.right] when isDefined(n, right) else []));

testTree :=
    (value : 1,
     left : (value : 2,
             left : (value : 4,
                     left : (value : 7)),
                     right : (value : 5)),
             right : (value : 3,
                      left : (value : 6,
                              left : (value : 8),
                              right : (value : 9))
             )
    );

```

{{out}}
Output:

```txt

preorder: [1,2,4,7,5,3,6,8,9]
inoder: [7,4,2,5,1,8,6,9,3]
postorder: [7,4,5,2,8,9,6,3,1]
level-order: [1,2,3,4,5,6,7,8,9]

```


## Sidef

{{trans|Perl}}

```ruby
func preorder(t) {
    t ? [t[0], __FUNC__(t[1])..., __FUNC__(t[2])...] : [];
}

func inorder(t) {
    t ? [__FUNC__(t[1])..., t[0], __FUNC__(t[2])...] : [];
}

func postorder(t) {
    t ? [__FUNC__(t[1])..., __FUNC__(t[2])..., t[0]] : [];
}

func depth(t) {
    var a = [t];
    var ret = [];
    while (a.len > 0) {
        var v = (a.shift \\ next);
        ret « v[0];
        a += [v[1,2]];
    };
    return ret;
}

var x = [1,[2,[4,[7]],[5]],[3,[6,[8],[9]]]];
say "pre:   #{preorder(x)}";
say "in:    #{inorder(x)}";
say "post:  #{postorder(x)}";
say "depth: #{depth(x)}";
```

{{out}}

```txt

pre:   1 2 4 7 5 3 6 8 9
in:    7 4 2 5 1 8 6 9 3
post:  7 4 5 2 8 9 6 3 1
depth: 1 2 3 4 5 6 7 8 9

```



## Tcl

{{works with|Tcl|8.6}} or {{libheader|TclOO}}

```tcl
oo::class create tree {
    # Basic tree data structure stuff...
    variable val l r
    constructor {value {left {}} {right {}}} {
	set val $value
	set l $left
	set r $right
    }
    method value {} {return $val}
    method left  {} {return $l}
    method right {} {return $r}
    destructor {
	if {$l ne ""} {$l destroy}
	if {$r ne ""} {$r destroy}
    }

    # Traversal methods
    method preorder {varName script {level 0}} {
	upvar [incr level] $varName var
	set var $val
	uplevel $level $script
	if {$l ne ""} {$l preorder $varName $script $level}
	if {$r ne ""} {$r preorder $varName $script $level}
    }
    method inorder {varName script {level 0}} {
	upvar [incr level] $varName var
	if {$l ne ""} {$l inorder $varName $script $level}
	set var $val
	uplevel $level $script
	if {$r ne ""} {$r inorder $varName $script $level}
    }
    method postorder {varName script {level 0}} {
	upvar [incr level] $varName var
	if {$l ne ""} {$l postorder $varName $script $level}
	if {$r ne ""} {$r postorder $varName $script $level}
	set var $val
	uplevel $level $script
    }
    method levelorder {varName script} {
	upvar 1 $varName var
	set nodes [list [self]]; # A queue of nodes to process
	while {[llength $nodes] > 0} {
	    set nodes [lassign $nodes n]
	    set var [$n value]
	    uplevel 1 $script
	    if {[$n left] ne ""} {lappend nodes [$n left]}
	    if {[$n right] ne ""} {lappend nodes [$n right]}
	}
    }
}
```

Note that in Tcl it is conventional to handle performing something “for each element” by evaluating a script in the caller's scope for each node after setting a caller-nominated variable to the value for that iteration. Doing this transparently while recursing requires the use of a varying ‘level’ parameter to <code>upvar</code> and <code>uplevel</code>, but makes for compact and clear code.

Demo code to satisfy the official challenge instance:

```tcl
# Helpers to make construction and listing of a whole tree simpler
proc Tree nested {
    lassign $nested v l r
    if {$l ne ""} {set l [Tree $l]}
    if {$r ne ""} {set r [Tree $r]}
    tree new $v $l $r
}
proc Listify {tree order} {
    set list {}
    $tree $order v {
	lappend list $v
    }
    return $list
}

# Make a tree, print it a few ways, and destroy the tree
set t [Tree {1 {2 {4 7} 5} {3 {6 8 9}}}]
puts "preorder:    [Listify $t preorder]"
puts "inorder:     [Listify $t inorder]"
puts "postorder:   [Listify $t postorder]"
puts "level-order: [Listify $t levelorder]"
$t destroy
```

Output:

```txt
preorder:    1 2 4 7 5 3 6 8 9
inorder:     7 4 2 5 1 8 6 9 3
postorder:   7 4 5 2 8 9 6 3 1
level-order: 1 2 3 4 5 6 7 8 9
```



## UNIX Shell

Bash (also "sh" on most Unix systems) has arrays.  We implement a node as an association between three arrays: left, right, and value.

```bash
left=()
right=()
value=()

# node node#, left#, right#, value
#
# if value is empty, use node#

node() {
  nx=${1:-'Missing node index'}
  leftx=${2}
  rightx=${3}
  val=${4:-$1}
  value[$nx]="$val"
  left[$nx]="$leftx"
  right[$nx]="$rightx"
}

# define the tree

node 1 2 3
node 2 4 5
node 3 6
node 4 7
node 5
node 6 8 9
node 7
node 8
node 9

# walk NODE# ORDER

walk() {
  local nx=${1-"Missing index"}
  shift
  for branch in "$@" ; do
    case "$branch" in
      left)  if [[ "${left[$nx]}" ]];      then walk ${left[$nx]}  $@ ; fi ;;
      right) if [[ "${right[$nx]}" ]];     then walk ${right[$nx]} $@ ; fi ;;
      self)  printf "%d " "${value[$nx]}"  ;;
    esac
  done
}

apush() {
  local var="$1"
  eval "$var=( \"\${$var[@]}\" \"$2\" )"
}

showname() {
  printf "%-12s " "$1:"
}

showdata() {
  showname "$1"
  shift
  walk "$@"
  echo ''
}

preorder()  { showdata $FUNCNAME $1 self left right ; }
inorder()   { showdata $FUNCNAME $1 left self right ; }
postorder() { showdata $FUNCNAME $1 left right self ; }
levelorder() {
  showname 'level-order'
  queue=( $1 )
  x=0
  while [[ $x < ${#queue[*]} ]]; do
    value="${queue[$x]}"
    printf "%d " "$value"
    for more in "${left[$value]}" "${right[$value]}" ; do
      if [[ -n "$more" ]]; then
	apush queue "$more"
      fi
    done
    : $((x++))
  done
  echo ''
}

preorder   1
inorder    1
postorder  1
levelorder 1
```

The output:

```bash
preorder:    1 2 4 7 5 3 6 8 9
inorder:     7 4 2 5 1 8 6 9 3
postorder:   7 4 5 2 8 9 6 3 1
level-order: 1 2 3 4 5 6 7 8 9
```



## Ursala

Ursala has built-in notation for trees and is perfect for whipping up little
tree walking functions. This source listing shows the tree depicted above
declared as a constant, followed by declarations of four functions
applicable to trees of any type. The main program applies all four of them
to the tree and makes a list of their results, each of which is a list of
natural numbers. The compiler directive #cast %nLL induces the compile-time
side effect of displaying
the result on standard output as a
list of lists of naturals.

```Ursala
tree =

1^:<
   2^: <4^: <7^: <>, 0>, 5^: <>>,
   3^: <6^: <8^: <>, 9^: <>>, 0>>

pre  = ~&dvLPCo
post = ~&vLPdNCTo
in   = ~&vvhPdvtL2CTiQo
lev  = ~&iNCaadSPfavSLiF3RTaq

#cast %nLL

main = <.pre,in,post,lev> tree
```

output:

```txt

<
   <1,2,4,7,5,3,6,8,9>,
   <7,4,2,5,1,8,6,9,3>,
   <7,4,5,2,8,9,6,3,1>,
   <1,2,3,4,5,6,7,8,9>>

```


## VBA

TreeItem Class Module

```VB

Public Value As Integer
Public LeftChild As TreeItem
Public RightChild As TreeItem

```

Module

```VB

Dim tihead As TreeItem

Private Function Add(v As Integer, left As TreeItem, right As TreeItem) As TreeItem
    Dim x As New TreeItem
    x.Value = v
    Set x.LeftChild = left
    Set x.RightChild = right
    Set Add = x
End Function

Private Sub Init()
    Set tihead = Add(1, _
                    Add(2, _
                        Add(4, _
                            Add(7, Nothing, Nothing), _
                            Nothing), _
                        Add(5, Nothing, Nothing)), _
                    Add(3, _
                        Add(6, _
                            Add(8, Nothing, Nothing), _
                            Add(9, Nothing, Nothing)), _
                        Nothing))
End Sub

Private Sub InOrder(ti As TreeItem)
    If Not ti Is Nothing Then
        Call InOrder(ti.LeftChild)
        Debug.Print ti.Value;
        Call InOrder(ti.RightChild)
    End If
End Sub

Private Sub PreOrder(ti As TreeItem)
    If Not ti Is Nothing Then
        Debug.Print ti.Value;
        Call PreOrder(ti.LeftChild)
        Call PreOrder(ti.RightChild)
    End If
End Sub

Private Sub PostOrder(ti As TreeItem)
    If Not ti Is Nothing Then
        Call PostOrder(ti.LeftChild)
        Call PostOrder(ti.RightChild)
        Debug.Print ti.Value;
    End If
End Sub

Private Sub LevelOrder(ti As TreeItem)
    Dim queue As Object
    Set queue = CreateObject("System.Collections.Queue")
    queue.Enqueue ti
    Do While (queue.Count > 0)
        Set next_ = queue.Dequeue
        Debug.Print next_.Value;
        If Not next_.LeftChild Is Nothing Then queue.Enqueue next_.LeftChild
        If Not next_.RightChild Is Nothing Then queue.Enqueue next_.RightChild
    Loop
End Sub

Public Sub Main()
    Init
    Debug.Print "preorder:     ";
    Call PreOrder(tihead)
    Debug.Print vbCrLf; "inorder:      ";
    Call InOrder(tihead)
    Debug.Print vbCrLf; "postorder:    ";
    Call PostOrder(tihead)
    Debug.Print vbCrLf; "level-order:  ";
    Call LevelOrder(tihead)
End Sub

```

{{out}}

```txt

preorder:      1  2  4  7  5  3  6  8  9
inorder:       7  4  2  5  1  8  6  9  3
postorder:     7  4  5  2  8  9  6  3  1
level-order:   1  2  3  4  5  6  7  8  9

```



## zkl


```zkl
class Node{ var [mixin=Node]left,right; var v;
   fcn init(val,[Node]l=Void,[Node]r=Void) { v,left,right=vm.arglist }
}

class BTree{ var [mixin=Node] root;
   fcn init(r){ root=r }
   const VISIT=Void, LEFT="left", RIGHT="right";
   fcn preOrder  { traverse(VISIT,LEFT, RIGHT) }
   fcn inOrder   { traverse(LEFT, VISIT,RIGHT) }
   fcn postOrder { traverse(LEFT, RIGHT,VISIT) }
   fcn [private] traverse(order){  //--> list of Nodes
      sink:=List();
      fcn(sink,[Node]n,order){
         if(n){ foreach o in (order){
	    if(VISIT==o) sink.write(n);
	    else self.fcn(sink,n.setVar(o),order);  // actually get var, eg n.left
	 }}
      }(sink,root,vm.arglist);
      sink
   }
   fcn levelOrder{  // breadth first
      sink:=List(); q:=List(root);
      while(q){
         n:=q.pop(0); l:=n.left; r:=n.right;
	 sink.write(n);
	 if(l) q.append(l);
	 if(r) q.append(r);
      }
      sink
   }
}
```

It is easy to convert to lazy by replacing "sink.write" with "vm.yield" and wrapping the traversal with a Utils.Generator.

```zkl
t:=BTree(Node(1,
	   Node(2,
	      Node(4,Node(7)),
	      Node(5)),
	   Node(3,
	      Node(6, Node(8),Node(9)))));

t.preOrder()  .apply("v").println("  preorder");
t.inOrder()   .apply("v").println("  inorder");
t.postOrder() .apply("v").println("  postorder");
t.levelOrder().apply("v").println("  level-order");
```

The "apply("v")" extracts the contents of var v from each node.
{{out}}

```txt

L(1,2,4,7,5,3,6,8,9)  preorder
L(7,4,2,5,1,8,6,9,3)  inorder
L(7,4,5,2,8,9,6,3,1)  postorder
L(1,2,3,4,5,6,7,8,9)  level-order

```

