+++
title = "Wireworld/Java"
description = ""
date = 2014-08-06T17:48:24Z
aliases = []
[extra]
id = 4912
[taxonomies]
categories = []
tags = []
+++

{{works with|Java|1.5+}}
With help from [[Conway's Game of Life#Java]]:

```java5
public class Wireworld{
  public static final char empt = ' ';
  public static final char head = 'h';
  public static final char tail = 't';
  public static final char ctor = '.';

  public static char[][] iterate(char[][] world){
    char[][] nextGen = new char[world.length][world[0].length];
    for(int row = 0; row < world.length; row++){//each row
      String thisRow = new String(world[row]);
      for(int col = 0; col < thisRow.length(); col++){//each char in the row
        switch (world[row][col]) {
          case head:
            nextGen[row][col] = tail;
            continue;
          case tail:
            nextGen[row][col] = ctor;
            continue;
          case empt:
            nextGen[row][col] = empt;
            continue;
          default:
        }
        String above = "";//neighbors above
        String same = "";//neighbors in the same row
        String below = "";//neighbors below
        if(col == 0){//all the way on the left
          //no one above if on the top row
          //otherwise grab the neighbors from above
          above = (row == 0) ? null : new String(world[row - 1]).substring(col,
                  col + 2);
          same = thisRow.substring(col + 1, col + 2);
          //no one below if on the bottom row
          //otherwise grab the neighbors from below
          below = (row == world.length - 1) ? null : new String(world[row + 1]).substring(col, col + 2);
        }else if (col == thisRow.length() - 1){//right
          //no one above if on the top row
          //otherwise grab the neighbors from above
          above = (row == 0) ? null : new String(world[row - 1]).substring(col - 1,
                  col + 1);
          same = thisRow.substring(col - 1, col);
          //no one below if on the bottom row
          //otherwise grab the neighbors from below
          below = (row == world.length - 1) ? null : new String(world[row + 1]).substring(col - 1, col + 1);
        }else{//anywhere else
          //no one above if on the top row
          //otherwise grab the neighbors from above
          above = (row == 0) ? null : new String(world[row - 1]).substring(col - 1,
                  col + 2);
          same = thisRow.substring(col - 1, col) + thisRow.substring(col + 1, col + 2);
          //no one below if on the bottom row
          //otherwise grab the neighbors from below
          below = (row == world.length - 1) ? null : new String(world[row + 1]).substring(col - 1, col + 2);
        }
        int heads = headsInNeighborhood(above, same, below);
        switch (heads){
          case 1:
          case 2:
            nextGen[row][col] = head;
            break;
          default:
            nextGen[row][col] = ctor;
        }
      }
    }
    return nextGen;
  }

  private static int headsInNeighborhood(String above, String same, String below){
    int ans = 0;
    if(above != null){//no one above
      for(char x : above.toCharArray()){//each neighbor from above
        if(x == head){
          ans++;//count it if a head is here
        }
      }
    }
    for(char x : same.toCharArray()){//two on either side
      if(x == head){
        ans++;//count it if a head is here
      }
    }
    if(below != null){//no one below
      for(char x : below.toCharArray()){//each neighbor below
        if(x == head){
          ans++;//count it if a head is here
        }
      }
    }
    return ans;
  }

  public static void main(String[] args){
    char[][] world = {
      "th.........".toCharArray(),
      ".   .      ".toCharArray(),
      "   ...     ".toCharArray(),
      ".   .      ".toCharArray(),
      "ht.. ......".toCharArray()
    };
    for(int i = 0; i <= 20; i++){
      System.out.println("Iteration " + i + ":");
      printWorld(world);
      System.out.println();
      world = iterate(world);
    }
  }

  private static void printWorld(char[][] world){
    for(char[] row : world){
      System.out.println(row);
    }
  }
}
```

{{out}}
<pre style="height:30ex;overflow:scroll">Iteration 0:
th.........
.   .      
   ...     
.   .      
ht.. ......

Iteration 1:
.th........
h   .      
   ...     
h   .      
t... ......

Iteration 2:
h.th.......
t   .      
   ...     
t   .      
.h.. ......

Iteration 3:
th.th......
.   h      
   ...     
.   .      
hth. ......

Iteration 4:
.th.th.....
h   t      
   hhh     
h   .      
t.th ......

Iteration 5:
h.th.th....
t   .      
   ttt     
t   .      
.h.t ......

Iteration 6:
th.th.th...
.   h      
   ...     
.   .      
hth. ......

Iteration 7:
.th.th.th..
h   t      
   hhh     
h   .      
t.th ......

Iteration 8:
h.th.th.th.
t   .      
   ttt     
t   .      
.h.t ......

Iteration 9:
th.th.th.th
.   h      
   ...     
.   .      
hth. ......

Iteration 10:
.th.th.th.t
h   t      
   hhh     
h   .      
t.th ......
```

