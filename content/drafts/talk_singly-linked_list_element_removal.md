+++
title = "Talk:Singly-linked list/Element removal"
description = ""
date = 2018-11-30T16:47:17Z
aliases = []
[extra]
id = 21180
[taxonomies]
categories = []
tags = []
+++

== Needs description and goal ==

This needs a task description and goal. While it may be a valuable addition to have such a task, this does not yet meet minimum criteria. See [[Singly-linked_list/Element_insertion]] for possible verbiage (edited to suit) --[[User:Thundergnat|Thundergnat]] ([[User talk:Thundergnat|talk]]) 16:20, 27 October 2016 (UTC)

== Taste ==

In a 2016 TED talk, interview with Chris Anderson, Linus Torvalds provides an example of element removal in a singly-linked list while discussing good taste in coding. He got rid of the if ... then statement to handle a special case. See [https://www.ted.com/talks/linus_torvalds_the_mind_behind_linux#t-933770 at 15'40" in the talk].

```C

remove_list_entry(entry)
{
// The "indirect" pointer points to the
// *address* of the thing we'll update

indirect = &head;

// Walk the list, looking for the thing that
// points tot the entry we want to remove

while ((*indirect) != entry)
	indirect = &(*indirect)->next;

// .. and just remove it
*indirect = entry->next;
} 

```

Slightly adapted to make the above code compile and run.

```C

#include <stdio.h>

typedef struct node {
    int val;
    struct node* next;
} node_t;

node_t* head = NULL;
node_t** indirect;

void remove_list_entry(node_t* entry)
{
// The "indirect" pointer points to the
// *address* of the thing we'll update

indirect = &head;

// Walk the list, looking for the thing that
// points tot the entry we want to remove

while ((*indirect) != entry)
	indirect = &(*indirect)->next;

// .. and just remove it
*indirect = entry->next;
}

int main()
{
    head = malloc(sizeof(node_t));
    head->val = 1;
    head->next = malloc(sizeof(node_t));
    head->next->val = 2;
    head->next->next = NULL;
    remove_list_entry(head);
    return 0;
}

```


== Visual Basic .NET of Tastefull ==

The Visual Basic .NET Tastefull version doesn't work if entry equals head, because head is a separate variable. In Torvalds c version the memory address head points to is altered. [[User:Dedalus|Dedalus]] ([[User talk:Dedalus|talk]]) 16:47, 30 November 2018 (UTC)
