+++
title = "AVL tree/C"
description = ""
date = 2019-01-27T03:39:24Z
aliases = []
[extra]
id = 20690
[taxonomies]
categories = []
tags = []
+++

==Code==

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

struct node {
	int payload;
	int height;
	struct node * kid[2];
} dummy = { 0, 0, {&dummy, &dummy} }, *nnil = &dummy;
// internally, nnil is the new nul

struct node *new_node(int value)
{
	struct node *n = calloc(1, sizeof *n);
	*n = (struct node) { value, 1, {nnil, nnil} };
	return n;
}

int max(int a, int b) { return a > b ? a : b; }
inline void set_height(struct node *n) {
	n->height = 1 + max(n->kid[0]->height, n->kid[1]->height);
}

inline int ballance(struct node *n) {
	return n->kid[0]->height - n->kid[1]->height;
}

// rotate a subtree according to dir; if new root is nil, old root is freed
struct node * rotate(struct node **rootp, int dir)
{
	struct node *old_r = *rootp, *new_r = old_r->kid[dir];

	if (nnil == (*rootp = new_r))
		free(old_r);
	else {
		old_r->kid[dir] = new_r->kid[!dir];
		set_height(old_r);
		new_r->kid[!dir] = old_r;
	}
	return new_r;
}

void adjust_balance(struct node **rootp)
{
	struct node *root = *rootp;
	int b = ballance(root)/2;
	if (b) {
		int dir = (1 - b)/2;
		if (ballance(root->kid[dir]) == -b)
			rotate(&root->kid[dir], !dir);
		root = rotate(rootp, dir);
	}
	if (root != nnil) set_height(root);
}

// find the node that contains value as payload; or returns 0
struct node *query(struct node *root, int value)
{
	return root == nnil
		? 0
		: root->payload == value
			? root
			: query(root->kid[value > root->payload], value);
}

void insert(struct node **rootp, int value)
{
	struct node *root = *rootp;

	if (root == nnil)
		*rootp = new_node(value);
	else if (value != root->payload) { // don't allow dup keys
		insert(&root->kid[value > root->payload], value);
		adjust_balance(rootp);
	}
}

void delete(struct node **rootp, int value)
{
	struct node *root = *rootp;
	if (root == nnil) return; // not found

	// if this is the node we want, rotate until off the tree
	if (root->payload == value)
		if (nnil == (root = rotate(rootp, ballance(root) < 0)))
			return;

	delete(&root->kid[value > root->payload], value);
	adjust_balance(rootp);
}

// aux display and verification routines, helpful but not essential
struct trunk {
	struct trunk *prev;
	char * str;
};

void show_trunks(struct trunk *p)
{
	if (!p) return;
	show_trunks(p->prev);
	printf("%s", p->str);
}

// this is very haphazzard
void show_tree(struct node *root, struct trunk *prev, int is_left)
{
	if (root == nnil) return;

	struct trunk this_disp = { prev, "    " };
	char *prev_str = this_disp.str;
	show_tree(root->kid[0], &this_disp, 1);

	if (!prev)
		this_disp.str = "---";
	else if (is_left) {
		this_disp.str = ".--";
		prev_str = "   |";
	} else {
		this_disp.str = "`--";
		prev->str = prev_str;
	}

	show_trunks(&this_disp);
	printf("%d\n", root->payload);

	if (prev) prev->str = prev_str;
	this_disp.str = "   |";

	show_tree(root->kid[1], &this_disp, 0);
	if (!prev) puts("");
}

int verify(struct node *p)
{
	if (p == nnil) return 1;

	int h0 = p->kid[0]->height, h1 = p->kid[1]->height;
	int b = h0 - h1;

	if (p->height != 1 + max(h0, h1) || b < -1 || b > 1) {
		printf("node %d bad, balance %d\n", p->payload, b);
		show_tree(p, 0, 0);
		abort();
	}
	return verify(p->kid[0]) && verify(p->kid[1]);
}

#define MAX_VAL 32
int main(void)
{
	int x;
	struct node *root = nnil;

	srand(time(0));

	for (x = 0; x < 10 * MAX_VAL; x++) {
		// random insertion and deletion
		if (rand()&1)	insert(&root, rand()%MAX_VAL);
		else		delete(&root, rand()%MAX_VAL);

		verify(root);
	}

	puts("Tree is:");
	show_tree(root, 0, 0);

	puts("\nQuerying values:");
	for (x = 0; x < MAX_VAL; x++) {
		struct node *p = query(root, x);
		if (p)	printf("%2d found: %p %d\n", x, p, p->payload);
	}

	for (x = 0; x < MAX_VAL; x++) {
		delete(&root, x);
		verify(root);
	}

	puts("\nAfter deleting all values, tree is:");
	show_tree(root, 0, 0);

	return 0;
}
```

{{out}}

```txt

Tree is:
            .--0
        .--2
       |    `--4
       |        `--5
    .--6
   |   |    .--10
   |    `--12
   |       |    .--13
   |        `--15
---17
   |            .--18
   |        .--19
   |    .--21
   |   |   |    .--24
   |   |    `--25
    `--26
        `--27
            `--30


Querying values:
 0 found: 0x180a210 0
 2 found: 0x180a1b0 2
 4 found: 0x180a010 4
 5 found: 0x180a0d0 5
 6 found: 0x180a0f0 6
10 found: 0x180a190 10
12 found: 0x180a150 12
13 found: 0x180a250 13
15 found: 0x180a050 15
17 found: 0x180a270 17
18 found: 0x180a1d0 18
19 found: 0x180a170 19
21 found: 0x180a070 21
24 found: 0x180a030 24
25 found: 0x180a0b0 25
26 found: 0x180a130 26
27 found: 0x180a1f0 27
30 found: 0x180a110 30

After deleting all values, tree is:

```




###  Efficient AVL tree


The following example implements an AVL tree ''without'' the need of calculating the height of the nodes (which can be quite time consuming if the tree gets large)!
It is based on an example of AVL tree in C# (see [http://www.superstarcoders.com/blogs/posts/efficient-avl-tree-in-c-sharp.aspx]).

The example distinguish between the tree implementation itself (see below) and the data to be stored in the tree (see example below).

<tt>AvlTree.h</tt>

```c
#ifndef AVLTREE_INCLUDED
#define AVLTREE_INCLUDED

typedef struct Tree *Tree;
typedef struct Node *Node;

Tree  Tree_New        (int (*comp)(void *, void *), void (*print)(void *));

void  Tree_Insert     (Tree t, void *data);
void  Tree_DeleteNode (Tree t, Node  node);
Node  Tree_SearchNode (Tree t, void *data);

Node  Tree_FirstNode  (Tree t);
Node  Tree_LastNode   (Tree t);

Node  Tree_PrevNode   (Tree t, Node n);
Node  Tree_NextNode   (Tree t, Node n);

void  Tree_Print      (Tree t);

void *Node_GetData (Node n);

#endif
```


<tt>AvlTree.c</tt>

```c
#include <stdio.h>
#include <stdlib.h>
#include "AvlTree.h"

//
// Private datatypes
//
struct Tree {
    Node root;
    int (*comp) (void *, void *);
    void (*print) (void *);
};

struct Node {
    Node parent;
    Node left;
    Node right;
    void *data;
    int balance;
};

struct trunk {
    struct trunk *prev;
    char *str;
};

//
// Declaration of private functions.
//
void Tree_InsertBalance (Tree t, Node node, int balance);
void Tree_DeleteBalance (Tree t, Node node, int balance);
Node Tree_RotateLeft (Tree t, Node node);
Node Tree_RotateRight (Tree t, Node node);
Node Tree_RotateLeftRight (Tree t, Node node);
Node Tree_RotateRightLeft (Tree t, Node node);
void Tree_Replace (Node target, Node source);

Node Node_New (void *data, Node parent);

void print_tree (Tree t, Node n, struct trunk *prev, int is_left);

//----------------------------------------------------------------------------

// Tree_New --
//
//     Creates a new tree using the parameters 'comp' and 'print' for
//     comparing and printing the data in the nodes.
//
Tree Tree_New (int (*comp)(void *, void *), void (*print)(void *)) {
    Tree t;

    t = malloc (sizeof (*t));
    t->root = NULL;
    t->comp = comp;
    t->print = print;

    return t;
}

// Tree_Insert --
//
//     Insert new data in the tree. If the data is already in the tree,
//     nothing will be done.
//
void Tree_Insert (Tree t, void *data) {
    if (t->root == NULL) {
        t->root = Node_New (data, NULL);
    } else {
        Node node = t->root;
        while (node != NULL) {
            if ((t->comp) (data, node->data) < 0) {
                Node left = node->left;
                if (left == NULL) {
                    node->left = Node_New (data, node);
                    Tree_InsertBalance (t, node, -1);
                    return;
                } else {
                    node = left;
                }
            } else if ((t->comp) (data, node->data) > 0) {
                Node right = node->right;
                if (right == NULL) {
                    node->right = Node_New (data, node);
                    Tree_InsertBalance (t, node, 1);
                    return;
                } else {
                    node = right;
                }
            } else {
                node->data = data;
                return;
            }
        }
    }
}

// Tree_DeleteNode --
//
//     Removes a given node from the tree.
//
void Tree_DeleteNode (Tree t, Node node) {
    Node left = node->left;
    Node right = node->right;
    Node toDelete = node;

    if (left == NULL) {
        if (right == NULL) {
            if (node == t->root) {
                t->root = NULL;
            } else {
                Node parent = node->parent;
                if (parent->left == node) {
                    parent->left = NULL;
                    Tree_DeleteBalance (t, parent, 1);
                } else {
                    parent->right = NULL;
                    Tree_DeleteBalance (t, parent, -1);
                }
            }
        } else {
            Tree_Replace (node, right);
            Tree_DeleteBalance (t, node, 0);
            toDelete = right;
        }
    } else if (right == NULL) {
        Tree_Replace (node, left);
        Tree_DeleteBalance (t, node, 0);
        toDelete = left;
    } else {
        Node successor = right;
        if (successor->left == NULL) {
            Node parent = node->parent;
            successor->parent = parent;
            successor->left = left;
            successor->balance = node->balance;

            if (left != NULL) {
                left->parent = successor;
            }
            if (node == t->root) {
                t->root = successor;
            } else {
                if (parent->left == node) {
                    parent->left = successor;
                } else {
                    parent->right = successor;
                }
            }
            Tree_DeleteBalance (t, successor, -1);
        } else {
            while (successor->left != NULL) {
                successor = successor->left;
            }
            Node parent = node->parent;
            Node successorParent = successor->parent;
            Node successorRight = successor->right;

            if (successorParent->left == successor) {
                successorParent->left = successorRight;
            } else {
                successorParent->right = successorRight;
            }

            if (successorRight != NULL) {
                successorRight->parent = successorParent;
            }

            successor->parent = parent;
            successor->left = left;
            successor->balance = node->balance;
            successor->right = right;
            right->parent = successor;

            if (left != NULL) {
                left->parent = successor;
            }

            if (node == t->root) {
                t->root = successor;
            } else {
                if (parent->left == node) {
                    parent->left = successor;
                } else {
                    parent->right = successor;
                }
            }
            Tree_DeleteBalance (t, successorParent, 1);
        }
    }

    free (toDelete);
}

// Tree_SearchNode --
//
//     Searches the tree for a node containing the given data.
//
Node Tree_SearchNode (Tree t, void *data) {
    Node node = t->root;

    while (node != NULL) {
        if ((t->comp) (data, node->data) < 0) {
            node = node->left;
        } else if ((t->comp) (data, node->data) > 0) {
            node = node->right;
        } else {
            return node;
        }
    }

    return NULL;
}

// Tree_Print --
//
//     Prints an ASCII representation of the tree on screen.
//
void Tree_Print (Tree t) {
    print_tree (t, t->root, 0, 0);
    fflush (stdout);
}

// Tree_FirstNode --
//
//     Returns the node containing the smallest key.
//
Node Tree_FirstNode (Tree t) {
    Node node = t->root;

    while ((node != NULL) && (node->left != NULL)) {
        node = node->left;
    }

    return node;
}

// Tree_LastNode --
//
//     Returns the node containing the biggest key.
//
Node Tree_LastNode (Tree t) {
    Node node = t->root;

    while ((node != NULL) && (node->right != NULL)) {
        node = node->right;
    }

    return node;
}

// Tree_PrevNode --
//
//     Returns the predecessor of the given node.
//
Node Tree_PrevNode (Tree t, Node n) {
    Node nTemp;

    if (n->left != NULL) {
        n = n->left;
        while (n->right != NULL) {
            n = n->right;
        }
    } else {
        nTemp = n;
        n = n->parent;
        while ((n != NULL) && (n->left == nTemp)) {
            nTemp = n;
            n = n->parent;
        }
    }
    return n;
}

// Tree_NextNode --
//
//     Returns the follower of the given node.
//
Node Tree_NextNode (Tree t, Node n) {
    Node nTemp;

    if (n->right != NULL) {
        n = n->right;
        while (n->left != NULL) {
            n = n->left;
        }
    } else {
        nTemp = n;
        n = n->parent;
        while ((n != NULL) && (n->right == nTemp)) {
            nTemp = n;
            n = n->parent;
        }
    }

    return n;
}

// Node_GetData --
//
//     Returns the data in a node.
//
void *Node_GetData (Node n) {
    return n->data;
}

//----------------------------------------------------------------------------
//
// Internal functions.
//

void Tree_InsertBalance (Tree t, Node node, int balance) {
    while (node != NULL) {
        balance = (node->balance += balance);
        if (balance == 0) {
            return;
        } else if (balance == -2) {
            if (node->left->balance == -1) {
                Tree_RotateRight (t, node);
            } else {
                Tree_RotateLeftRight (t, node);
            }
            return;
        } else if (balance == 2) {
            if (node->right->balance == 1) {
                Tree_RotateLeft (t, node);
            } else {
                Tree_RotateRightLeft (t, node);
            }
            return;
        }
        Node parent = node->parent;
        if (parent != NULL) {
            balance = (parent->left == node) ? -1 : 1;
        }
        node = parent;
    }
}

void Tree_DeleteBalance (Tree t, Node node, int balance) {
    while (node != NULL) {
        balance = (node->balance += balance);

        if (balance == -2) {
            if (node->left->balance <= 0) {
                node = Tree_RotateRight (t, node);

                if (node->balance == 1) {
                    return;
                }
            } else {
                node = Tree_RotateLeftRight (t, node);
            }
        } else if (balance == 2) {
            if (node->right->balance >= 0) {
                node = Tree_RotateLeft (t, node);

                if (node->balance == -1) {
                    return;
                }
            } else {
                node = Tree_RotateRightLeft (t, node);
            }
        } else if (balance != 0) {
            return;
        }

        Node parent = node->parent;

        if (parent != NULL) {
            balance = (parent->left == node) ? 1 : -1;
        }

        node = parent;
    }
}

void Tree_Replace (Node target, Node source) {
    Node left = source->left;
    Node right = source->right;

    target->balance = source->balance;
    target->data = source->data;
    target->left = left;
    target->right = right;

    if (left != NULL) {
        left->parent = target;
    }

    if (right != NULL) {
        right->parent = target;
    }
}

Node Tree_RotateLeft (Tree t, Node node) {
    Node right = node->right;
    Node rightLeft = right->left;
    Node parent = node->parent;

    right->parent = parent;
    right->left = node;
    node->right = rightLeft;
    node->parent = right;

    if (rightLeft != NULL) {
        rightLeft->parent = node;
    }

    if (node == t->root) {
        t->root = right;
    } else if (parent->right == node) {
        parent->right = right;
    } else {
        parent->left = right;
    }

    right->balance--;
    node->balance = -right->balance;

    return right;
}

Node Tree_RotateRight (Tree t, Node node) {
    Node left = node->left;
    Node leftRight = left->right;
    Node parent = node->parent;

    left->parent = parent;
    left->right = node;
    node->left = leftRight;
    node->parent = left;

    if (leftRight != NULL) {
        leftRight->parent = node;
    }

    if (node == t->root) {
        t->root = left;
    } else if (parent->left == node) {
        parent->left = left;
    } else {
        parent->right = left;
    }

    left->balance++;
    node->balance = -left->balance;

    return left;
}

Node Tree_RotateLeftRight (Tree t, Node node) {
    Node left = node->left;
    Node leftRight = left->right;
    Node parent = node->parent;
    Node leftRightRight = leftRight->right;
    Node leftRightLeft = leftRight->left;

    leftRight->parent = parent;
    node->left = leftRightRight;
    left->right = leftRightLeft;
    leftRight->left = left;
    leftRight->right = node;
    left->parent = leftRight;
    node->parent = leftRight;

    if (leftRightRight != NULL) {
        leftRightRight->parent = node;
    }

    if (leftRightLeft != NULL) {
        leftRightLeft->parent = left;
    }

    if (node == t->root) {
        t->root = leftRight;
    } else if (parent->left == node) {
        parent->left = leftRight;
    } else {
        parent->right = leftRight;
    }

    if (leftRight->balance == 1) {
        node->balance = 0;
        left->balance = -1;
    } else if (leftRight->balance == 0) {
        node->balance = 0;
        left->balance = 0;
    } else {
        node->balance = 1;
        left->balance = 0;
    }

    leftRight->balance = 0;

    return leftRight;
}

Node Tree_RotateRightLeft (Tree t, Node node) {
    Node right = node->right;
    Node rightLeft = right->left;
    Node parent = node->parent;
    Node rightLeftLeft = rightLeft->left;
    Node rightLeftRight = rightLeft->right;

    rightLeft->parent = parent;
    node->right = rightLeftLeft;
    right->left = rightLeftRight;
    rightLeft->right = right;
    rightLeft->left = node;
    right->parent = rightLeft;
    node->parent = rightLeft;

    if (rightLeftLeft != NULL) {
        rightLeftLeft->parent = node;
    }

    if (rightLeftRight != NULL) {
        rightLeftRight->parent = right;
    }

    if (node == t->root) {
        t->root = rightLeft;
    } else if (parent->right == node) {
        parent->right = rightLeft;
    } else {
        parent->left = rightLeft;
    }

    if (rightLeft->balance == -1) {
        node->balance = 0;
        right->balance = 1;
    } else if (rightLeft->balance == 0) {
        node->balance = 0;
        right->balance = 0;
    } else {
        node->balance = -1;
        right->balance = 0;
    }

    rightLeft->balance = 0;

    return rightLeft;
}

void print_trunks (struct trunk *p) {
    if (!p) {
        return;
    }
    print_trunks (p->prev);
    printf ("%s", p->str);
}

void print_tree (Tree t, Node n, struct trunk *prev, int is_left) {
    if (n == NULL) {
        return;
    }

    struct trunk this_disp = { prev, "     " };
    char *prev_str = this_disp.str;
    print_tree (t, n->right, &this_disp, 1);

    if (!prev) {
        this_disp.str = "---";
    } else if (is_left) {
        this_disp.str = ".--";
        prev_str = "    |";
    } else {
        this_disp.str = "`--";
        prev->str = prev_str;
    }

    print_trunks (&this_disp);
    (t->print) (n->data);
    printf (" (%+d)\n", n->balance);

    if (prev) {
        prev->str = prev_str;
    }
    this_disp.str = "    |";

    print_tree (t, n->left, &this_disp, 0);
    if (!prev) {
        puts ("");
    }
}

Node Node_New (void *data, Node parent) {
    Node n;

    n = malloc (sizeof (*n));
    n->parent = parent;
    n->left = NULL;
    n->right = NULL;
    n->data = data;
    n->balance = 0;

    return n;
}
```


And here's the example which shows how to use the package. It creates in an endless loop random numbers between 0..999 and stores the number (the key) together with its square root (the value) in the tree. If an element with the given key is already in tree, it will be deleted.


```c
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include "AvlTree.h"

#define MAX_KEY_VALUE 1000

typedef struct NodeData {
    int    key;
    double value;
} *NodeData;

int comp (void *a1, void *a2) {
    NodeData nd1 = (NodeData) a1;
    NodeData nd2 = (NodeData) a2;

    if (nd1->key < nd2->key) {
        return -1;
    } else if (nd1->key > nd2->key) {
        return +1;
    } else {
        return 0;
    }
}

void print (void *a) {
    NodeData nd = (NodeData) a;

    printf ("%3d \"%6.3f\"", nd->key, nd->value);
}

int main (int argc, char **argv) {
    Tree tree;
    Node node;
    NodeData nd1 = NULL, nd2;

    tree = Tree_New (comp, print);

    while (1) {
        if (nd1 == NULL) {
            nd1 = malloc (sizeof (*nd1));
        }
        nd1->key = rand () % MAX_KEY_VALUE;
        nd1->value = sqrt (nd1->key);
        if ((node = Tree_SearchNode (tree, nd1)) != NULL) {
            printf (">>> delete key %d\n\n", nd1->key);
            nd2 = Node_GetData (node);
            Tree_DeleteNode (tree, node);
            free (nd2);
        } else {
            printf (">>> insert key %d\n\n", nd1->key);
            Tree_Insert (tree, nd1);
            nd1 = NULL;
        }
        Tree_Print (tree);
        sleep (2);
    }

    return 0;
}
```


After a number of iterations, the tree will look similar to this:

{{out}}

```txt

                         .--980 "31.305" (+0)
                    .--972 "31.177" (+0)
                   |     `--944 "30.725" (+0)
               .--933 "30.545" (+0)
              |    |     .--932 "30.529" (+0)
              |     `--929 "30.480" (+0)
              |          `--920 "30.332" (+0)
          .--917 "30.282" (+0)
         |    |          .--876 "29.597" (+0)
         |    |     .--868 "29.462" (+1)
         |     `--862 "29.360" (+1)
         |          `--847 "29.103" (+0)
     .--843 "29.034" (-1)
    |    |          .--752 "27.423" (+0)
    |    |     .--743 "27.258" (-1)
    |    |    |    |     .--727 "26.963" (+0)
    |    |    |     `--711 "26.665" (+0)
    |    |    |          `--710 "26.646" (+0)
    |     `--705 "26.552" (-1)
    |         |          .--700 "26.458" (-1)
    |         |         |     `--675 "25.981" (+0)
    |         |     .--635 "25.199" (+0)
    |         |    |    |     .--629 "25.080" (+0)
    |         |    |     `--624 "24.980" (+1)
    |          `--592 "24.331" (+1)
    |              |     .--551 "23.473" (+0)
    |               `--549 "23.431" (+1)
---529 "23.000" (+0)
    |               .--508 "22.539" (-1)
    |              |     `--491 "22.159" (+0)
    |          .--484 "22.000" (+1)
    |         |     `--431 "20.761" (+0)
    |     .--427 "20.664" (+0)
    |    |    |          .--425 "20.616" (+0)
    |    |    |     .--397 "19.925" (+1)
    |    |     `--393 "19.824" (+0)
    |    |          `--366 "19.131" (-1)
    |    |               `--363 "19.053" (+0)
     `--351 "18.735" (-1)
         |               .--344 "18.547" (+0)
         |          .--330 "18.166" (+1)
         |     .--310 "17.607" (+1)
         |    |     `--301 "17.349" (+0)
          `--262 "16.186" (-1)
              |          .--236 "15.362" (+0)
              |     .--223 "14.933" (+1)
               `--164 "12.806" (-1)
                   |     .--134 "11.576" (-1)
                   |    |     `--126 "11.225" (+0)
                    `--119 "10.909" (+0)
                         `--108 "10.392" (-1)
                              `-- 30 " 5.477" (+0)

```

