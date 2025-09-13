+++
title = "Kronecker product based fractals"
description = ""
date = 2019-07-12T13:24:48Z
aliases = []
[extra]
id = 21345
[taxonomies]
categories = ["task", "Fractals"]
tags = []
+++

This task is based on   [[Kronecker product| Kronecker product]]   of two matrices. 

If your language has no a built-in function for such product then you need to implement it first.

The essence of fractals is self-replication (at least, self-similar replications). 

So, using   '''n'''   times self-product of the matrix   (filled with '''0'''/'''1''')   we will have a fractal of the   '''n'''<sup>th</sup>   order.

Actually, "self-product" is a Kronecker power of the matrix.

In other words: for a matrix   '''M'''   and a power   '''n'''   create a function like   '''matkronpow(M, n)''', 

which returns   M<small>x</small>M<small>x</small>M<small>x</small>...   ('''n'''   times product).

A formal recurrent <i>algorithm</i> of creating Kronecker power of a matrix is the following:


;Algorithm:
<ul>
  <li>Let M is an initial matrix, and Rn is a resultant block matrix  of the Kronecker power, where n is the power (a.k.a. order).</li>
  <li>Self-product of M, i.e., M x M producing R2 (resultant matrix with order/power 2).</li>
  <li>To receive the next order/power matrix use this recurrent formula: Rn = R(n-1) x M.</li>
  <li>Plot this Rn matrix to produce the <i><b>n</b>th</i> order fractal.</li>
</ul>

Even just looking at the resultant matrix you can see what will be plotted.

There are virtually infinitely many fractals of this type. You are limited only by your creativity and 
the power of your computer.


## Task

Using [[Kronecker_product| Kronecker product]] implement and show two popular and well-known fractals, i.e.:
* [[wp:Vicsek fractal| Vicsek fractal]];
* [[wp:Sierpinski carpet| Sierpinski carpet fractal]].


The last one ([[Sierpinski carpet| Sierpinski carpet]]) is already here on RC, but built using different approaches.



;Test cases:
These 2 fractals (each order/power 4 at least) should be built using the following 2 simple matrices: 

```txt

          │ 0 1 0 │    and    │ 1 1 1 │
          │ 1 1 1 │           │ 1 0 1 │
          │ 0 1 0 │	      │ 1 1 1 │

```


;Note:
* Output could be a graphical or ASCII-art representation, but if an order is set > 4 then printing is not suitable.
* The orientation and distortion of the fractal could be your language/tool specific.
* It would be nice to see one additional fractal of your choice, e.g., based on using a single (double) letter(s) of an alphabet, any sign(s) or already made a resultant matrix of the Kronecker product.


See implementations and results below in JavaScript, PARI/GP and R languages. They have additional samples of "H", "+" and checkerboard fractals.





## C

Although this task is related to [[Kronecker product]], this is computationally a more complex task as the matrix has to be raised to an arbitrary power. Assume matrix A, order i x j has to be raised to power n, the final result will have (i^n)x(j^n) elements. Doing this "conventionally" will require at least (i^n)x(j^n) operations with storage for the same number of elements. This means a storage requirement of 4 x (i^n) x (j^n) bytes for an integer matrix. 

However, if half of the elements of the initial matrix A are zeroes, computations and storage for such elements are wasted as they will never be plotted. The only relevant elements are the 1s.

Thus this implementation treats the initial matrix as a [https://en.wikipedia.org/wiki/Sparse_matrix Sparse matrix]. Doing so cuts down drastically on the required storage and number of operations. The graphical part needs the [http://www.cs.colorado.edu/~main/bgi/cs1300/ WinBGIm] library. 

```C

#include<graphics.h>
#include<stdlib.h>
#include<stdio.h>

typedef struct{
	int row, col;
}cell;

int ROW,COL,SUM=0;

unsigned long raiseTo(int base,int power){
	if(power==0)
		return 1;
	else
		return base*raiseTo(base,power-1);
}

cell* kroneckerProduct(char* inputFile,int power){
	FILE* fp = fopen(inputFile,"r");
	
	int i,j,k,l;
	unsigned long prod;
	int** matrix;
	cell *coreList,*tempList,*resultList;
	
	fscanf(fp,"%d%d",&ROW,&COL);
	
	matrix = (int**)malloc(ROW*sizeof(int*));
	
	for(i=0;i<ROW;i++){
		matrix[i] = (int*)malloc(COL*sizeof(int));
		for(j=0;j<COL;j++){
			fscanf(fp,"%d",&matrix[i][j]);
			if(matrix[i][j]==1)
				SUM++;
		}
	}
	
	coreList = (cell*)malloc(SUM*sizeof(cell));
	resultList = (cell*)malloc(SUM*sizeof(cell));
	
	k = 0;
	
	for(i=0;i<ROW;i++){
		for(j=0;j<COL;j++){
			if(matrix[i][j]==1){
				coreList[k].row = i+1;
				coreList[k].col = j+1;
				resultList[k].row = i+1;
				resultList[k].col = j+1;
				k++;
			}
		}
	}
	
	prod = k;
	
	for(i=2;i<=power;i++){
		tempList = (cell*)malloc(prod*k*sizeof(cell));
		
		l = 0;
		
		for(j=0;j<prod;j++){
			for(k=0;k<SUM;k++){
				tempList[l].row = (resultList[j].row-1)*ROW + coreList[k].row;
				tempList[l].col = (resultList[j].col-1)*COL + coreList[k].col;
				l++;
			}
		}
		
		free(resultList);
		
		prod *= k;
		
		resultList = (cell*)malloc(prod*sizeof(cell));
		
		for(j=0;j<prod;j++){
			resultList[j].row = tempList[j].row;
			resultList[j].col = tempList[j].col;
		}
		free(tempList);
	}
	
	return resultList;
}

int main(){
	char fileName[100];
	int power,i,length;
	
	cell* resultList;
	
	printf("Enter input file name : ");
	scanf("%s",fileName);
	
	printf("Enter power : ");
	scanf("%d",&power);
	
	resultList = kroneckerProduct(fileName,power);
	
	initwindow(raiseTo(ROW,power),raiseTo(COL,power),"Kronecker Product Fractal");
	
	length = raiseTo(SUM,power);

	for(i=0;i<length;i++){
		putpixel(resultList[i].row,resultList[i].col,15);
	}
	
	getch();
	
	closegraph();
	
	return 0;
}

```



## Factor


```factor
USING: io kernel math math.matrices sequences ;

: mat-kron-pow ( m n -- m' ) 1 - [ dup kron ] times ;

: print-fractal ( m -- )
    [ [ 1 = "*" " " ? write ] each nl ] each ;
    
{ { 0 1 0 } { 1 1 1 } { 0 1 0 } }
{ { 1 1 1 } { 1 0 1 } { 1 1 1 } }
{ { 0 1 1 } { 0 1 0 } { 1 1 0 } }
[ 3 mat-kron-pow print-fractal ] tri@
```

Output shown at order 4 and 25% font size.
<pre style="font-size:25%">
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                            *  *  *  *  *  *  *  *  *                            
                           ***************************                           
                            *  *  *  *  *  *  *  *  *                            
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
          *  *  *                    *  *  *                    *  *  *          
         *********                  *********                  *********         
          *  *  *                    *  *  *                    *  *  *          
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
    *        *        *        *        *        *        *        *        *    
   ***      ***      ***      ***      ***      ***      ***      ***      ***   
    *        *        *        *        *        *        *        *        *    
 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 
*********************************************************************************
 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 
    *        *        *        *        *        *        *        *        *    
   ***      ***      ***      ***      ***      ***      ***      ***      ***   
    *        *        *        *        *        *        *        *        *    
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
          *  *  *                    *  *  *                    *  *  *          
         *********                  *********                  *********         
          *  *  *                    *  *  *                    *  *  *          
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                            *  *  *  *  *  *  *  *  *                            
                           ***************************                           
                            *  *  *  *  *  *  *  *  *                            
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
* *   * *         * *   * ** *   * *         * *   * ** *   * *         * *   * *
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
***   ******   ******   ***                           ***   ******   ******   ***
* *   * ** *   * ** *   * *                           * *   * ** *   * ** *   * *
***   ******   ******   ***                           ***   ******   ******   ***
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
*********         *********                           *********         *********
* ** ** *         * ** ** *                           * ** ** *         * ** ** *
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
***   ***         ***   ***                           ***   ***         ***   ***
*********         *********                           *********         *********
* ** ** *         * ** ** *                           * ** ** *         * ** ** *
*********         *********                           *********         *********
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
***   ******   ******   ***                           ***   ******   ******   ***
* *   * ** *   * ** *   * *                           * *   * ** *   * ** *   * *
***   ******   ******   ***                           ***   ******   ******   ***
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
* *   * *         * *   * ** *   * *         * *   * ** *   * *         * *   * *
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
                                        ** **    ** **             ** **    ** **
                                        *  *     *  *              *  *     *  * 
                                       ** **    ** **             ** **    ** ** 
                                        **       **                **       **   
                                        *        *                 *        *    
                                       **       **                **       **    
                                     ** **    ** **             ** **    ** **   
                                     *  *     *  *              *  *     *  *    
                                    ** **    ** **             ** **    ** **    
                                        ** **                      ** **         
                                        *  *                       *  *          
                                       ** **                      ** **          
                                        **                         **            
                                        *                          *             
                                       **                         **             
                                     ** **                      ** **            
                                     *  *                       *  *             
                                    ** **                      ** **             
                               ** **    ** **             ** **    ** **         
                               *  *     *  *              *  *     *  *          
                              ** **    ** **             ** **    ** **          
                               **       **                **       **            
                               *        *                 *        *             
                              **       **                **       **             
                            ** **    ** **             ** **    ** **            
                            *  *     *  *              *  *     *  *             
                           ** **    ** **             ** **    ** **             
                                        ** **    ** **                           
                                        *  *     *  *                            
                                       ** **    ** **                            
                                        **       **                              
                                        *        *                               
                                       **       **                               
                                     ** **    ** **                              
                                     *  *     *  *                               
                                    ** **    ** **                               
                                        ** **                                    
                                        *  *                                     
                                       ** **                                     
                                        **                                       
                                        *                                        
                                       **                                        
                                     ** **                                       
                                     *  *                                        
                                    ** **                                        
                               ** **    ** **                                    
                               *  *     *  *                                     
                              ** **    ** **                                     
                               **       **                                       
                               *        *                                        
                              **       **                                        
                            ** **    ** **                                       
                            *  *     *  *                                        
                           ** **    ** **                                        
             ** **    ** **             ** **    ** **                           
             *  *     *  *              *  *     *  *                            
            ** **    ** **             ** **    ** **                            
             **       **                **       **                              
             *        *                 *        *                               
            **       **                **       **                               
          ** **    ** **             ** **    ** **                              
          *  *     *  *              *  *     *  *                               
         ** **    ** **             ** **    ** **                               
             ** **                      ** **                                    
             *  *                       *  *                                     
            ** **                      ** **                                     
             **                         **                                       
             *                          *                                        
            **                         **                                        
          ** **                      ** **                                       
          *  *                       *  *                                        
         ** **                      ** **                                        
    ** **    ** **             ** **    ** **                                    
    *  *     *  *              *  *     *  *                                     
   ** **    ** **             ** **    ** **                                     
    **       **                **       **                                       
    *        *                 *        *                                        
   **       **                **       **                                        
 ** **    ** **             ** **    ** **                                       
 *  *     *  *              *  *     *  *                                        
** **    ** **             ** **    ** **                                        

```



## gnuplot

File for the load command is the only possible imitation of the fine function in the '''gnuplot'''.
;Note:
* Find '''plotff.gp''' here on RC
* dat-files are [[Kronecker_product_based_fractals#PARI.2FGP| PARI/GP]] generated output files. They are too big to post them here on RC.



[[File:pkf1.png|right|thumb|Output pkf1.png]]
[[File:pkf2.png|right|thumb|Output pkf2.png]]
[[File:pkf3.png|right|thumb|Output pkf3.png]]


```gnuplot

## KPF.gp 4/8/17 aev
## Plotting 3 KPF pictures.
## dat-files are PARI/GP generated output files: 
#cd 'C:\gnupData'

##PKF1 from PARI/GP created file pkf1.dat
ttl = "Vicsec fractal"; clr = '"blue"'; filename = "pkf1"; 
load "plotff.gp"

##PKF2 from PARI/GP created file pkf2.dat
ttl = "Sierpinski carpet fractal"; clr = '"navy"';filename = "pkf2";
load "plotff.gp"

##PKF3 from PARI/GP created file pkf3.dat
ttl = "Sierpinski triangle fractal"; clr = '"dark-green"'; filename = "pkf3";
load "plotff.gp"

```

```txt

3 plotted files: pkf1.png, pkf2.png and pkf3.png.

```



## Go

```go
package main

import "fmt"

type matrix [][]int

func (m1 matrix) kroneckerProduct(m2 matrix) matrix {
    m := len(m1)
    n := len(m1[0])
    p := len(m2)
    q := len(m2[0])
    rtn := m * p
    ctn := n * q
    r := make(matrix, rtn)
    for i := range r {
        r[i] = make([]int, ctn) // all elements zero by default
    }
    for i := 0; i < m; i++ {
        for j := 0; j < n; j++ {
            for k := 0; k < p; k++ {
                for l := 0; l < q; l++ {
                    r[p*i+k][q*j+l] = m1[i][j] * m2[k][l]
                }
            }
        }
    }
    return r
}

func (m matrix) kroneckerPower(n int) matrix {
    pow := m
    for i := 1; i < n; i++ {
        pow = pow.kroneckerProduct(m)
    }
    return pow
}

func (m matrix) print(text string) {
    fmt.Println(text, "fractal :\n")
    for i := range m {
        for j := range m[0] {
            if m[i][j] == 1 {
                fmt.Print("*")
            } else {
                fmt.Print(" ")
            }
        }
        fmt.Println()
    }
    fmt.Println()
}

func main() {
    m1 := matrix{{0, 1, 0}, {1, 1, 1}, {0, 1, 0}}
    m1.kroneckerPower(4).print("Vivsek")

    m2 := matrix{{1, 1, 1}, {1, 0, 1}, {1, 1, 1}}
    m2.kroneckerPower(4).print("Sierpinski carpet")
}
```


```txt

Same as Kotlin entry.

```



## Haskell

This implementation compiles to javascript that runs in the browser using the [https://github.com/ghcjs/ghcjs ghcjs compiler ] .  The [https://github.com/reflex-frp/reflex-dom reflex-dom ] library is used to help with svg rendering.


```haskell
{-# LANGUAGE OverloadedStrings #-}
import Reflex
import Reflex.Dom
import Data.Map as DM (Map, fromList)
import Data.Text (Text, pack)
import Data.List (transpose)

-- Show Vicsek and Sierpinski Carpet fractals
main :: IO ()
main = mainWidget $ do 
  elAttr "h1" ("style" =: "color:black") $ text "Kroneker Product Based Fractals" 
  elAttr "a" ("href" =: "http://rosettacode.org/wiki/Kronecker_product_based_fractals#Haskell") $ text "Rosetta Code / Kroneker product based fractals / Haskell"

  -- Show a Vicsek fractal
  el "br" $ return ()
  elAttr "h2" ("style" =: "color:brown") $ text "Vicsek Fractal" 
  showFractal [[0, 1, 0] ,[1, 1, 1] ,[0, 1, 0] ]

  -- Show a Sierpinski Carpet fractal
  el "br" $ return ()
  elAttr "h2" ("style" =: "color:brown") $ text "Sierpinski Carpet Fractal" 
  showFractal [[1, 1, 1] ,[1, 0, 1] ,[1, 1, 1] ]

-- Size in pixels of an individual cell
cellSize :: Int
cellSize = 8

-- Given a "seed" matrix, generate and display a fractal.
showFractal :: MonadWidget t m => [[Int]] -> m ()
showFractal seed = do
  let boardAttrs w h = 
         fromList [ ("width" , pack $ show $ w * cellSize)
                  , ("height", pack $ show $ h * cellSize)
                  ]
      fractals = iterate (kronekerProduct seed) seed
      shown = fractals !! 3 -- the fourth fractal (starting from 0)
      w = length $ head shown
      h = length shown
  elSvgns "svg" (constDyn $ boardAttrs w h) $ showMatrix shown

-- Compute the Kroneker product of two matrices.
kronekerProduct :: Num a => [[a]] -> [[a]] -> [[a]]
kronekerProduct xs ys = 
    let m0 = flip $ fmap.fmap.(*)
        m1 = flip $ fmap.fmap.m0
    in concat $ fmap (fmap concat.transpose) $ m1 xs ys

-- Show an entire matrix
showMatrix :: MonadWidget t m => [[Int]] -> m ()
showMatrix m = mapM_ showRow $ zip [0..] m 

-- Show a single horizontal row of a matrix
showRow :: MonadWidget t m => (Int,[Int]) -> m ()
showRow (x,r) = mapM_ (showCell x) $ zip [0..] r 

-- Show a circle in a box moved to the correct location on screen
showCell :: MonadWidget t m => Int -> (Int,Int) -> m ()
showCell x (y,on) = 
  let boxAttrs (x,y) = -- Place box on screen
        fromList [ ("transform", 
                    pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                           ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
                   )
                 ] 

      cellAttrs = -- Draw circle in box.
        fromList [ ( "cx",      "0.5")
                 , ( "cy",      "0.5")
                 , ( "r",       "0.45")
                 , ( "style",   "fill:green")
                 ] 

  in if (on==1) then  -- Only draw circle for elements containing 1
       elSvgns "g"  (constDyn $ boxAttrs (x,y)) $ 
         elSvgns "circle" (constDyn $ cellAttrs) $ 
           return ()
     else
       return ()

-- Wrapper around elDynAttrNS'
elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
elSvgns t m ma = do
    (el, val) <- elDynAttrNS' (Just "http://www.w3.org/2000/svg") t m ma
    return val
```


Link to live demo: https://dc25.github.io/rosettaCode__Kronecker_product_based_fractals/ ( a little slow to load ).

## J


Implementation:


```J
V=: -.0 2 6 8 e.~i.3 3
S=: 4 ~:i.3 3
KP=: 1 3 ,/"2@(,/)@|: */

ascii_art=: ' *'{~]

KPfractal=:dyad def 'x&KP^:y,.1'
```


Task examples (order 4, 25% font size):

<pre style="font-size:25%">   ascii_art S KPfractal 4
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
* *   * *         * *   * ** *   * *         * *   * ** *   * *         * *   * *
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
***   ******   ******   ***                           ***   ******   ******   ***
* *   * ** *   * ** *   * *                           * *   * ** *   * ** *   * *
***   ******   ******   ***                           ***   ******   ******   ***
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
*********         *********                           *********         *********
* ** ** *         * ** ** *                           * ** ** *         * ** ** *
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
***   ***         ***   ***                           ***   ***         ***   ***
*********         *********                           *********         *********
* ** ** *         * ** ** *                           * ** ** *         * ** ** *
*********         *********                           *********         *********
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
***   ******   ******   ***                           ***   ******   ******   ***
* *   * ** *   * ** *   * *                           * *   * ** *   * ** *   * *
***   ******   ******   ***                           ***   ******   ******   ***
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
* *   * *         * *   * ** *   * *         * *   * ** *   * *         * *   * *
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
   ascii_art V KPfractal 4
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                            *  *  *  *  *  *  *  *  *                            
                           ***************************                           
                            *  *  *  *  *  *  *  *  *                            
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
          *  *  *                    *  *  *                    *  *  *          
         *********                  *********                  *********         
          *  *  *                    *  *  *                    *  *  *          
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
    *        *        *        *        *        *        *        *        *    
   ***      ***      ***      ***      ***      ***      ***      ***      ***   
    *        *        *        *        *        *        *        *        *    
 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 
*********************************************************************************
 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 
    *        *        *        *        *        *        *        *        *    
   ***      ***      ***      ***      ***      ***      ***      ***      ***   
    *        *        *        *        *        *        *        *        *    
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
          *  *  *                    *  *  *                    *  *  *          
         *********                  *********                  *********         
          *  *  *                    *  *  *                    *  *  *          
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                            *  *  *  *  *  *  *  *  *                            
                           ***************************                           
                            *  *  *  *  *  *  *  *  *                            
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *
```



## Java


This implementation does not use sparse matrices since the powers involved do not exceed 4.


```Java

package kronecker;

/**
 * Uses the Kronecker product powers of two rectangular matrices
 * to generate fractals and tests it with three examples.
 */
public class ProductFractals {
  /**
   * Find the Kronecker product of the arguments.
   * @param a The first matrix to multiply.
   * @param b The second matrix to multiply.
   * @return A new matrix: the Kronecker product of the arguments.
   */
  public static int[][] product(final int[][] a, final int[][] b) {
    // Create matrix c as the matrix to fill and return.
    // The length of a matrix is its number of rows.
    final int[][] c = new int[a.length*b.length][];
    // Fill in the (empty) rows of c.
    // The length of each row is the number of columns.
    for (int ix = 0; ix < c.length; ix++) {
      final int num_cols = a[0].length*b[0].length;
      c[ix] = new int[num_cols];
    }
    // Now fill in the values: the products of each pair.
    // Go through all the elements of a.
    for (int ia = 0; ia < a.length; ia++) {
      for (int ja = 0; ja < a[ia].length; ja++) {
        // For each element of a, multiply it by all the elements of b.
        for (int ib = 0; ib < b.length; ib++) {
          for (int jb = 0; jb < b[ib].length; jb++) {
             c[b.length*ia+ib][b[ib].length*ja+jb] = a[ia][ja] * b[ib][jb];
          }
        }
      }
    }

    // Return the completed product matrix c.
    return c;
  }

  /**
   * Print an image obtained from an integer matrix, using the specified
   * characters to indicate non-zero and zero elements.
   * @param m The matrix to print.
   * @param nz The character to print for a non-zero element.
   * @param z The character to print for a zero element.
   */
  public static void show_matrix(final int[][] m, final char nz, final char z) {
    for (int im = 0; im < m.length; im++) {
      for (int jm = 0; jm < m[im].length; jm++) {
        System.out.print(m[im][jm] == 0 ? z : nz);
      }
      System.out.println();
    }
  }

  /**
   * Compute the specified Kronecker product power
   * of the matrix and return  it.
   * @param m The matrix to raise to the power.
   * @param n The power to which to raise the matrix.
   * @return A new matrix containing the resulting power.
   */
  public static int[][] power(final int[][] m, final int n) {
    // Start with m itself as the first power.
    int[][] m_pow = m;
    // Start the iteration with 1, not 0,
    // since we already have the first power.
    for (int ix = 1; ix < n; ix++) {
      m_pow = product(m, m_pow);
    }
    return m_pow;
  }

  /**
   * Run a test by computing the specified Kronecker product power
   * of the matrix and printing matrix and power.
   * @param m The base matrix raise to the power.
   * @param n The power to which to raise the matrix.
   */
  private static void test(final int[][] m, final int n) {
    System.out.println("Test matrix");
    show_matrix(m, '*', ' ');
    final int[][] m_pow = power(m, n);
    System.out.println("Matrix power " + n);
    show_matrix(m_pow, '*', ' ');
  }

  /**
   * Create the matrix for the first test and run the test.
   */
  private static void test1() {
    // Create the matrix.
    final int[][] m = {{0, 1, 0},
                       {1, 1, 1},
                       {0, 1, 0}};
    // Run the test.
    test(m, 4);
  }

  /**
   * Create the matrix for the second test and run the test.
   */
  private static void test2() {
    // Create the matrix.
    final int[][] m = {{1, 1, 1},
                       {1, 0, 1},
                       {1, 1, 1}};
    // Run the test.
    test(m, 4);
  }

  /**
   * Create the matrix for the second test and run the test.
   */
  private static void test3() {
    // Create the matrix.
    final int[][] m = {{1, 0, 1},
                       {1, 0, 1},
                       {0, 1, 0}};
    // Run the test.
    test(m, 4);
  }

  /**
   * Run the program to run the three tests.
   * @param args Command line arguments (not used).
   */
  public static void main(final String[] args) {
    // Test the product fractals.
    test1();
    test2();
    test3();
  }

}

```


This output uses 50% font size.  Of course, it shows ASCII height distortion.

<pre style="font-size:50%">
Test matrix
 * 
***
 * 
Matrix power 4
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                            *  *  *  *  *  *  *  *  *                            
                           ***************************                           
                            *  *  *  *  *  *  *  *  *                            
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
          *  *  *                    *  *  *                    *  *  *          
         *********                  *********                  *********         
          *  *  *                    *  *  *                    *  *  *          
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
    *        *        *        *        *        *        *        *        *    
   ***      ***      ***      ***      ***      ***      ***      ***      ***   
    *        *        *        *        *        *        *        *        *    
 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 
*********************************************************************************
 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 
    *        *        *        *        *        *        *        *        *    
   ***      ***      ***      ***      ***      ***      ***      ***      ***   
    *        *        *        *        *        *        *        *        *    
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
          *  *  *                    *  *  *                    *  *  *          
         *********                  *********                  *********         
          *  *  *                    *  *  *                    *  *  *          
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                            *  *  *  *  *  *  *  *  *                            
                           ***************************                           
                            *  *  *  *  *  *  *  *  *                            
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
Test matrix
***
* *
***
Matrix power 4
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
* *   * *         * *   * ** *   * *         * *   * ** *   * *         * *   * *
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
***   ******   ******   ***                           ***   ******   ******   ***
* *   * ** *   * ** *   * *                           * *   * ** *   * ** *   * *
***   ******   ******   ***                           ***   ******   ******   ***
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
*********         *********                           *********         *********
* ** ** *         * ** ** *                           * ** ** *         * ** ** *
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
***   ***         ***   ***                           ***   ***         ***   ***
*********         *********                           *********         *********
* ** ** *         * ** ** *                           * ** ** *         * ** ** *
*********         *********                           *********         *********
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
***   ******   ******   ***                           ***   ******   ******   ***
* *   * ** *   * ** *   * *                           * *   * ** *   * ** *   * *
***   ******   ******   ***                           ***   ******   ******   ***
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
* *   * *         * *   * ** *   * *         * *   * ** *   * *         * *   * *
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
Test matrix
* *
* *
 * 
Matrix power 4
* *   * *         * *   * *                           * *   * *         * *   * *
* *   * *         * *   * *                           * *   * *         * *   * *
 *     *           *     *                             *     *           *     * 
* *   * *         * *   * *                           * *   * *         * *   * *
* *   * *         * *   * *                           * *   * *         * *   * *
 *     *           *     *                             *     *           *     * 
   * *               * *                                 * *               * *   
   * *               * *                                 * *               * *   
    *                 *                                   *                 *    
* *   * *         * *   * *                           * *   * *         * *   * *
* *   * *         * *   * *                           * *   * *         * *   * *
 *     *           *     *                             *     *           *     * 
* *   * *         * *   * *                           * *   * *         * *   * *
* *   * *         * *   * *                           * *   * *         * *   * *
 *     *           *     *                             *     *           *     * 
   * *               * *                                 * *               * *   
   * *               * *                                 * *               * *   
    *                 *                                   *                 *    
         * *   * *                                             * *   * *         
         * *   * *                                             * *   * *         
          *     *                                               *     *          
         * *   * *                                             * *   * *         
         * *   * *                                             * *   * *         
          *     *                                               *     *          
            * *                                                   * *            
            * *                                                   * *            
             *                                                     *             
* *   * *         * *   * *                           * *   * *         * *   * *
* *   * *         * *   * *                           * *   * *         * *   * *
 *     *           *     *                             *     *           *     * 
* *   * *         * *   * *                           * *   * *         * *   * *
* *   * *         * *   * *                           * *   * *         * *   * *
 *     *           *     *                             *     *           *     * 
   * *               * *                                 * *               * *   
   * *               * *                                 * *               * *   
    *                 *                                   *                 *    
* *   * *         * *   * *                           * *   * *         * *   * *
* *   * *         * *   * *                           * *   * *         * *   * *
 *     *           *     *                             *     *           *     * 
* *   * *         * *   * *                           * *   * *         * *   * *
* *   * *         * *   * *                           * *   * *         * *   * *
 *     *           *     *                             *     *           *     * 
   * *               * *                                 * *               * *   
   * *               * *                                 * *               * *   
    *                 *                                   *                 *    
         * *   * *                                             * *   * *         
         * *   * *                                             * *   * *         
          *     *                                               *     *          
         * *   * *                                             * *   * *         
         * *   * *                                             * *   * *         
          *     *                                               *     *          
            * *                                                   * *            
            * *                                                   * *            
             *                                                     *             
                           * *   * *         * *   * *                           
                           * *   * *         * *   * *                           
                            *     *           *     *                            
                           * *   * *         * *   * *                           
                           * *   * *         * *   * *                           
                            *     *           *     *                            
                              * *               * *                              
                              * *               * *                              
                               *                 *                               
                           * *   * *         * *   * *                           
                           * *   * *         * *   * *                           
                            *     *           *     *                            
                           * *   * *         * *   * *                           
                           * *   * *         * *   * *                           
                            *     *           *     *                            
                              * *               * *                              
                              * *               * *                              
                               *                 *                               
                                    * *   * *                                    
                                    * *   * *                                    
                                     *     *                                     
                                    * *   * *                                    
                                    * *   * *                                    
                                     *     *                                     
                                       * *                                       
                                       * *                                       
                                        *                                        

```



## JavaScript

Using Version #1 of [[Kronecker_product| Kronecker product]] in JavaScript.
[[File:VicsekFractaljs.png|200px|right|thumb|Output VicsekFractaljs.png]]
[[File:SierpCarpetFractaljs.png|200px|right|thumb|Output SierpCarpetFractaljs.png]]
[[File:CheckbrdFractaljs.png|200px|right|thumb|Output CheckbrdFractaljs.png]]

```javascript

// KPF.js 6/23/16 aev
// HFJS: Plot any matrix mat (filled with 0,1)
function pmat01(mat, color) {
  // DCLs
  var cvs = document.getElementById('canvId');
  var ctx = cvs.getContext("2d"); 
  var w = cvs.width; var h = cvs.height;
  var m = mat[0].length; var n = mat.length;
  // Cleaning canvas and setting plotting color 
  ctx.fillStyle="white"; ctx.fillRect(0,0,w,h);
  ctx.fillStyle=color;
  // MAIN LOOP
  for(var i=0; i<m; i++) {
    for(var j=0; j<n; j++) {
      if(mat[i][j]==1) { ctx.fillRect(i,j,1,1)};
    }//fend j
  }//fend i
}//func end
// Prime functions:
// Create Kronecker product based fractal matrix rm from matrix m (order=ord)
function ckpbfmat(m,ord) {
  var rm=m;
  for(var i=1; i<ord; i++) {rm=mkp(rm,m)};
  //matpp2doc('R 4 ordd',rm,'*'); // ASCII "plotting" - if you wish to try.
  return(rm);
}
// Create and plot Kronecker product based fractal from matrix m (filled with 0/1)
function cpmat(m,ord,color) {
  var kpr;
  kpr=ckpbfmat(m,ord);
  pmat01(kpr,color);
}
// Fractal matrix "pretty" printing to document. 
// mat should be filled with 0 and 1; chr is a char substituting 1.
function matpp2doc(title,mat,chr) {
  var i,j,re='',e; var m=mat.length; var n=mat[0].length;
  document.write('  <b>'+title+'</b>:
```txt
');
  for(var i=0; i<m; i++) {
    for(var j=0; j<n; j++) {
      e=' '; if(mat[i][j]==1) {e=chr}; re+=e; 
    }//fend j
    document.write('  '+re+'<br />'); re='';
  }//fend i
  document.write('
```
');
}
// mkp function (exotic arrow function): Return the Kronecker product
// of the a and b matrices
mkp=(a,b)=>a.map(a=>b.map(b=>a.map(y=>b.map(x=>r.push(y*x)),t.push(r=[]))),t=[])&&t;

```
 

;Required tests:

```html

<!-- VicsekFractal.html -->
<html>
<head>
  <title>Vicsek fractal</title>
  <script src="KPF.js"></script>
</head>
<body onload="cpmat([[0,1,0],[1,1,1],[0,1,0]],6,'navy')">
   <h3>Vicsek fractal</h3>
   <a href="SierpCarpetFractal.html"> Next: Sierpinski carpet fractal</a><br />
   <canvas id="canvId" width="750" height="750" style="border: 1px outset;"></canvas>
</body></html>

```
 


```html

<!-- SierpCarpetFractal.html -->
<html>
<head>
  <title>Sierpinski carpet fractal</title>
  <script src="KPF.js"></script>
</head>
<body onload="cpmat([[1,1,1],[1,0,1],[1,1,1]],6,'brown')">
   <h3>Sierpinski carpet fractal</h3>
   <a href="Checkerboard.html"/> Next: Checkerboard </a><br />
   <canvas id="canvId" width="750" height="750" style="border: 1px outset;"></canvas>
</body></html>

```



```html

<!-- Checkerboard.html -->
<html>
<head>
  <title>Checkerboard</title>
  <script src="KPF.js"></script>
</head>
<body onload="cpmat([[0,1,0,1],[1,0,1,0],[0,1,0,1],[1,0,1,0]],5,'black')">
   <h3>Checkerboard</h3>
   <a href="VicsekFractal.html"/> Next: Vicsek fractal </a><br />
   <canvas id="canvId" width="750" height="750" style="border: 1px outset;"></canvas>
</body></html>

```
 

```txt

Page VicsekFractal.html with VicsekFractaljs.png
Page SierpCarpetFractal.html with SierpCarpetFractaljs.png
Page Checkerboard.html with CheckbrdFractaljs.png

```



## Julia

Julia has a builtin function `kron`:

```julia
function matkronpow(M::Matrix, n::Int)
    P = copy(M)
    for i in 1:n P = kron(P, M) end
    return P
end

function fracprint(M::Matrix)
    for i in 1:size(M, 1)
        for j in 1:size(M, 2)
            print(M[i, j] == 1 ? '*' : ' ')
        end
        println()
    end
end

M = [0 1 0; 1 1 1; 0 1 0]
matkronpow(M, 3) |> fracprint

M = [1 1 1; 1 0 1; 1 1 1]
matkronpow(M, 3) |> fracprint
```


```txt
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                            *  *  *  *  *  *  *  *  *                            
                           ***************************                           
                            *  *  *  *  *  *  *  *  *                            
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
          *  *  *                    *  *  *                    *  *  *          
         *********                  *********                  *********         
          *  *  *                    *  *  *                    *  *  *          
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
    *        *        *        *        *        *        *        *        *    
   ***      ***      ***      ***      ***      ***      ***      ***      ***   
    *        *        *        *        *        *        *        *        *    
 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 
*********************************************************************************
 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 
    *        *        *        *        *        *        *        *        *    
   ***      ***      ***      ***      ***      ***      ***      ***      ***   
    *        *        *        *        *        *        *        *        *    
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
          *  *  *                    *  *  *                    *  *  *          
         *********                  *********                  *********         
          *  *  *                    *  *  *                    *  *  *          
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                            *  *  *  *  *  *  *  *  *                            
                           ***************************                           
                            *  *  *  *  *  *  *  *  *                            
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
* *   * *         * *   * ** *   * *         * *   * ** *   * *         * *   * *
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
***   ******   ******   ***                           ***   ******   ******   ***
* *   * ** *   * ** *   * *                           * *   * ** *   * ** *   * *
***   ******   ******   ***                           ***   ******   ******   ***
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
*********         *********                           *********         *********
* ** ** *         * ** ** *                           * ** ** *         * ** ** *
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
***   ***         ***   ***                           ***   ***         ***   ***
*********         *********                           *********         *********
* ** ** *         * ** ** *                           * ** ** *         * ** ** *
*********         *********                           *********         *********
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
***   ******   ******   ***                           ***   ******   ******   ***
* *   * ** *   * ** *   * *                           * *   * ** *   * ** *   * *
***   ******   ******   ***                           ***   ******   ******   ***
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
* *   * *         * *   * ** *   * *         * *   * ** *   * *         * *   * *
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
```



## Kotlin

This reuses code from the [[Kronecker_product#Kotlin]] task.

```scala
// version 1.2.31

typealias Matrix = Array<IntArray>

fun kroneckerProduct(a: Matrix, b: Matrix): Matrix {
    val m = a.size
    val n = a[0].size
    val p = b.size
    val q = b[0].size
    val rtn = m * p
    val ctn = n * q
    val r: Matrix = Array(rtn) { IntArray(ctn) } // all elements zero by default
    for (i in 0 until m)
        for (j in 0 until n)
            for (k in 0 until p)
                for (l in 0 until q)
                    r[p * i + k][q * j + l] = a[i][j] * b[k][l]
    return r
}

fun kroneckerPower(a: Matrix, n: Int): Matrix {
    var pow = a.copyOf()
    for (i in 1 until n) pow = kroneckerProduct(pow, a)
    return pow
}

fun printMatrix(text: String, m: Matrix) {
    println("$text fractal :\n")
    for (i in 0 until m.size) {
        for (j in 0 until m[0].size) {
            print(if (m[i][j] == 1) "*" else " ")
        }
        println()
    }
    println()
}

fun main(args: Array<String>) {
    var a = arrayOf(
        intArrayOf(0, 1, 0),
        intArrayOf(1, 1, 1),
        intArrayOf(0, 1, 0)
    )
    printMatrix("Vicsek", kroneckerPower(a, 4))

    a = arrayOf(
        intArrayOf(1, 1, 1),
        intArrayOf(1, 0, 1),
        intArrayOf(1, 1, 1)
    )
    printMatrix("Sierpinski carpet", kroneckerPower(a, 4))
}
```


```txt

Vicsek fractal :

                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                            *  *  *  *  *  *  *  *  *                            
                           ***************************                           
                            *  *  *  *  *  *  *  *  *                            
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
          *  *  *                    *  *  *                    *  *  *          
         *********                  *********                  *********         
          *  *  *                    *  *  *                    *  *  *          
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
    *        *        *        *        *        *        *        *        *    
   ***      ***      ***      ***      ***      ***      ***      ***      ***   
    *        *        *        *        *        *        *        *        *    
 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 
*********************************************************************************
 *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  * 
    *        *        *        *        *        *        *        *        *    
   ***      ***      ***      ***      ***      ***      ***      ***      ***   
    *        *        *        *        *        *        *        *        *    
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
          *  *  *                    *  *  *                    *  *  *          
         *********                  *********                  *********         
          *  *  *                    *  *  *                    *  *  *          
             *                          *                          *             
            ***                        ***                        ***            
             *                          *                          *             
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                            *  *  *  *  *  *  *  *  *                            
                           ***************************                           
                            *  *  *  *  *  *  *  *  *                            
                               *        *        *                               
                              ***      ***      ***                              
                               *        *        *                               
                                        *                                        
                                       ***                                       
                                        *                                        
                                     *  *  *                                     
                                    *********                                    
                                     *  *  *                                     
                                        *                                        
                                       ***                                       
                                        *                                        

Sierpinski carpet fractal :

*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
* *   * *         * *   * ** *   * *         * *   * ** *   * *         * *   * *
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
***   ******   ******   ***                           ***   ******   ******   ***
* *   * ** *   * ** *   * *                           * *   * ** *   * ** *   * *
***   ******   ******   ***                           ***   ******   ******   ***
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
*********         *********                           *********         *********
* ** ** *         * ** ** *                           * ** ** *         * ** ** *
*********         *********                           *********         *********
***   ***         ***   ***                           ***   ***         ***   ***
* *   * *         * *   * *                           * *   * *         * *   * *
***   ***         ***   ***                           ***   ***         ***   ***
*********         *********                           *********         *********
* ** ** *         * ** ** *                           * ** ** *         * ** ** *
*********         *********                           *********         *********
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
***   ******   ******   ***                           ***   ******   ******   ***
* *   * ** *   * ** *   * *                           * *   * ** *   * ** *   * *
***   ******   ******   ***                           ***   ******   ******   ***
***************************                           ***************************
* ** ** ** ** ** ** ** ** *                           * ** ** ** ** ** ** ** ** *
***************************                           ***************************
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
* *   * *         * *   * ** *   * *         * *   * ** *   * *         * *   * *
***   ***         ***   ******   ***         ***   ******   ***         ***   ***
*********         ******************         ******************         *********
* ** ** *         * ** ** ** ** ** *         * ** ** ** ** ** *         * ** ** *
*********         ******************         ******************         *********
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
* *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * ** *   * *
***   ******   ******   ******   ******   ******   ******   ******   ******   ***
*********************************************************************************
* ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
*********************************************************************************

```



## Lua

Needs L&Ouml;VE 2D Engine

```lua

function prod( a, b )
    local rt, l = {}, 1
    for m = 1, #a do
        for p = 1, #b do
            rt[l] = {}
            for n = 1, #a[m] do
                for q = 1, #b[p] do
                    table.insert( rt[l], a[m][n] * b[p][q] )
                end
            end
            l = l + 1
        end
    end
    return rt
end
function love.load()
    wid, hei = love.graphics.getWidth(), love.graphics.getHeight()
    canvas = love.graphics.newCanvas( wid, hei )
    mA = { {0,1,0}, {1,1,1}, {0,1,0} }; mB = { {1,0,1}, {0,1,0}, {1,0,1} }
    mC = { {1,1,1}, {1,0,1}, {1,1,1} }; mD = { {1,1,1}, {0,1,0}, {1,1,1} }
end
function drawFractals( m )
    love.graphics.setCanvas( canvas )
    love.graphics.clear()
    love.graphics.setColor( 255, 255, 255 )
    for j = 1, #m do
        for i = 1, #m[j] do
            if m[i][j] == 1 then 
                love.graphics.points( i * .1, j * .1 )
            end
        end
    end
    love.graphics.setCanvas()
end
function love.keypressed( key, scancode, isrepeat )
    local t = {}
    if key == "a" then
        print( "Build Vicsek fractal I" ); t = mA
    elseif key == "b" then 
        print( "Build Vicsek fractal II" ); t = mB
    elseif key == "c" then 
        print( "Sierpinski carpet fractal" ); t = mC
    elseif key == "d" then 
        print( "Build 'H' fractal" ); t = mD
    else return
    end
    for i = 1, 3 do t = prod( t, t ) end
    drawFractals( t )
end
function love.draw()
    love.graphics.draw( canvas )
end

```



## PARI/GP

;Note:
* Find iPlotmat() here on [[User:AnatolV/Helper_Functions| Helper Functions]] page.
* Find matkronprod() here on [[Kronecker_product| Kronecker product]] page.
[[File:VicsekFractalgp.png|200px|right|thumb|Output VicsekFractalgp.png]]
[[File:SierpCarpetFractalgp.png|200px|right|thumb|Output SierpCarpetFractalgp.png]]
[[File:SierpTriFractalgp.png|200px|right|thumb|Output SierpTriFractalgp.png]]

```parigp

\\ Build block matrix applying Kronecker product to the special matrix m
\\ (n times to itself). Then plot Kronecker fractal. 4/25/2016 aev
pkronfractal(m,n=2,clr)={
  my(r=m);
  for(i=1,n, r=matkronprod(r,m));
  iPlotmat(r,clr);
}
\\Requireq tests:
{\\ Vicsek fractal: VicsekFractalgp.png
  my(M=[0,1,0;1,1,1;0,1,0]); print(" *** Vicsek fractal, order 4:");
  pkronfractal(M,4,6);
}
{\\ Sierpinski carpet fractal:  SierpCarpetFractalgp.png
  my(M=[1,1,1;1,0,1;1,1,1]); print(" *** Sierpinski carpet fractal, order 4:");
  pkronfractal(M,4,5);
}
{\\ Sierpinski triangle fractal:  SierpTriFractalgp.png
  my(M=[1,1;0,1]); print(" *** Sierpinski triangle fractal, order 7:");
  pkronfractal(M,7,6);
}

```
 
```txt

 *** Vicsek fractal, order 4:
 *** matrix(243x243) 3125 DOTS

 *** Sierpinski carpet fractal, order 4:
 *** matrix(243x243) 32768 DOTS 

 *** Sierpinski triangle fractal, order 7:
 *** matrix: 256x256, 6561 DOTS

```



## Perl

```perl
use Imager;
use Math::Cartesian::Product;

sub kronecker_product {
    our @a; local *a = shift;
    our @b; local *b = shift;
    my @c;
    cartesian {
        my @cc;
        cartesian {
            push @cc, $_[0] * $_[1];
        } [@{$_[0]}], [@{$_[1]}];
        push @c, [@cc];
    } [@a], [@b];
    @c
}

sub kronecker_fractal {
    my($order, @pattern) = @_;
    my @kronecker = @pattern;
    @kronecker = kronecker_product(\@kronecker, \@pattern) for 0..$order-1;
    @kronecker
}

@vicsek = ( [0, 1, 0], [1, 1, 1], [0, 1, 0] );
@carpet = ( [1, 1, 1], [1, 0, 1], [1, 1, 1] );
@six    = ( [0,1,1,1,0], [1,0,0,0,1], [1,0,0,0,0], [1,1,1,1,0], [1,0,0,0,1], [1,0,0,0,1], [0,1,1,1,0] );

for (['vicsek', \@vicsek, 4],
     ['carpet', \@carpet, 4],
     ['six',    \@six,    3]) {
    ($name, $shape, $order) = @$_;
    @img = kronecker_fractal( $order, @$shape );
    $png = Imager->new(xsize => 1+@{$img[0]}, ysize => 1+@img);
    cartesian {
        $png->setpixel(x => $_[0], y => $_[1], color => $img[$_[1]][$_[0]] ? [255, 255, 32] : [16, 16, 16]);
    } [0..@{$img[0]}-1], [0..$#img];
    $png->write(file => "run/kronecker-$name-perl6.png");
}
```

See [https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/kronecker-vicsek-perl6.png Kronecker-Vicsek], [https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/kronecker-carpet-perl6.png Kronecker-Carpet] and [https://github.com/SqrtNegInf/Rosettacode-Perl5-Smoke/blob/master/ref/kronecker-six-perl6.png Kronecker-Six] images.


## Perl 6

```perl6
sub kronecker-product ( @a, @b ) { (@a X @b).map: { (.[0].list X* .[1].list).Array } }

sub kronecker-fractal ( @pattern, $order = 4 ) {
    my @kronecker = @pattern;
    @kronecker = kronecker-product(@kronecker, @pattern) for ^$order;
    @kronecker
}

use Image::PNG::Portable;

#Task requirements
my @vicsek = ( [0, 1, 0], [1, 1, 1], [0, 1, 0] );
my @carpet = ( [1, 1, 1], [1, 0, 1], [1, 1, 1] );
my @six    = ( [0,1,1,1,0], [1,0,0,0,1], [1,0,0,0,0], [1,1,1,1,0], [1,0,0,0,1], [1,0,0,0,1], [0,1,1,1,0] );

for  'vicsek', @vicsek, 4,
     'carpet', @carpet, 4,
     'six',    @six,    3
  -> $name,    @shape,  $order {
    my @img = kronecker-fractal( @shape, $order );
    my $png = Image::PNG::Portable.new: :width(@img[0].elems), :height(@img.elems);
    (^@img[0]).race(:12batch).map: -> $x {
        for ^@img -> $y {
            $png.set: $x, $y, |( @img[$y;$x] ?? <255 255 32> !! <16 16 16> );
        }
    }
    $png.write: "kronecker-{$name}-perl6.png";
}
```


See [https://github.com/thundergnat/rc/blob/master/img/kronecker-vicsek-perl6.png Kronecker-Vicsek], [https://github.com/thundergnat/rc/blob/master/img/kronecker-carpet-perl6.png Kronecker-Carpet] and [https://github.com/thundergnat/rc/blob/master/img/kronecker-six-perl6.png Kronecker-Six] images.


## Phix


```Phix
function kronecker(sequence a, b)
    integer ar = length(a),
            ac = length(a[1]),
            br = length(b),
            bc = length(b[1])
    sequence res = repeat(repeat(0,ac*bc),ar*br)
    for ia=1 to ar do
        integer i0 = (ia-1)*br
        for ja=1 to ac do
            integer j0 = (ja-1)*bc
            for ib=1 to br do
                integer i = i0+ib
                for jb=1 to bc do
                    integer j = j0+jb
                    res[i,j] = a[ia,ja]*b[ib,jb]
                end for
            end for
        end for
    end for
    return res
end function

function kroneckern(sequence m, integer n)
    sequence res = m
    for i=2 to n do
        res = kronecker(res,m)
    end for
    return res
end function

procedure show(sequence m)
    for i=1 to length(m) do
        string s = repeat(' ',length(m[i]))
        for j=1 to length(s) do
            if m[i][j] then s[j] = '#' end if
        end for 
        puts(1,s&"\n")
    end for
    puts(1,"\n")
end procedure

constant vicsek = {{0,1,0},
                   {1,1,1},
                   {0,1,0}},
         siercp = {{1,1,1},
                   {1,0,1},
                   {1,1,1}},
         xxxxxx = {{0,1,1},
                   {0,1,0},
                   {1,1,0}}

show(kroneckern(vicsek,4))
show(kroneckern(siercp,4))
show(kroneckern(xxxxxx,4))
```

Output same as Julia/Kotlin/Factor


## Python

Generate images of the fractals using PIL.

'''Using only python lists'''

```python
import os
from PIL import Image


def imgsave(path, arr):
    w, h = len(arr), len(arr[0])
    img = Image.new('1', (w, h))
    for x in range(w):
        for y in range(h):
            img.putpixel((x, y), arr[x][y])
    img.save(path)


def get_shape(mat):
    return len(mat), len(mat[0])


def kron(matrix1, matrix2):
    """
    Calculate the kronecker product of two matrices
    """
    final_list = []

    count = len(matrix2)

    for elem1 in matrix1:
        for i in range(count):
            sub_list = []
            for num1 in elem1:
                for num2 in matrix2[i]:
                    sub_list.append(num1 * num2)
            final_list.append(sub_list)

    return final_list


def kronpow(mat):
    """
    Generate an arbitrary number of kronecker powers
    """
    matrix = mat
    while True:
        yield matrix
        matrix = kron(mat, matrix)


def fractal(name, mat, order=6):
    """
    Save fractal as jpg to 'fractals/name'
    """
    path = os.path.join('fractals', name)
    os.makedirs(path, exist_ok=True)

    fgen = kronpow(mat)
    print(name)
    for i in range(order):
        p = os.path.join(path, f'{i}.jpg')
        print('Calculating n =', i, end='\t', flush=True)

        mat = next(fgen)
        imgsave(p, mat)

        x, y = get_shape(mat)
        print('Saved as', x, 'x', y, 'image', p)


test1 = [
    [0, 1, 0],
    [1, 1, 1],
    [0, 1, 0]
]

test2 = [
    [1, 1, 1],
    [1, 0, 1],
    [1, 1, 1]
]

test3 = [
    [1, 0, 1],
    [0, 1, 0],
    [1, 0, 1]
]

fractal('test1', test1)
fractal('test2', test2)
fractal('test3', test3)

```


Because this is not very efficent/fast you should use scipy sparse matrices instead

```python
import os
import numpy as np
from scipy.sparse import csc_matrix, kron
from scipy.misc import imsave


def imgsave(name, arr, *args):
    imsave(name, arr.toarray(), *args)


def get_shape(mat):
    return mat.shape


def kronpow(mat):
    """
    Generate an arbitrary number of kronecker powers
    """
    matrix = mat
    while True:
        yield matrix
        matrix = kron(mat, matrix)


def fractal(name, mat, order=6):
    """
    Save fractal as jpg to 'fractals/name'
    """
    path = os.path.join('fractals', name)
    os.makedirs(path, exist_ok=True)

    fgen = kronpow(mat)
    print(name)
    for i in range(order):
        p = os.path.join(path, f'{i}.jpg')
        print('Calculating n =', i, end='\t', flush=True)

        mat = next(fgen)
        imgsave(p, mat)

        x, y = get_shape(mat)
        print('Saved as', x, 'x', y, 'image', p)


test1 = [
    [0, 1, 0],
    [1, 1, 1],
    [0, 1, 0]
]

test2 = [
    [1, 1, 1],
    [1, 0, 1],
    [1, 1, 1]
]

test3 = [
    [1, 0, 1],
    [0, 1, 0],
    [1, 0, 1]
]

test1 = np.array(test1, dtype='int8')
test1 = csc_matrix(test1)

test2 = np.array(test2, dtype='int8')
test2 = csc_matrix(test2)

test3 = np.array(test3, dtype='int8')
test3 = csc_matrix(test3)

fractal('test1', test1)
fractal('test2', test2)
fractal('test3', test3)
```



## R

Generate and plot 3 Kronecker product based fractals.

Note: Find plotmat() and plotv2() here on [[User:AnatolV/Helper_Functions| Helper Functions]] page.
[[File:VicsekFractalR.png|200px|right|thumb|Output VicsekFractalR.png]]
[[File:SierpCarpetFR.png|200px|right|thumb|Output SierpCarpetFR.png]]
[[File:PlusSignFR.png|200px|right|thumb|Output PlusSignFR.png]]


```r

## Generate and plot Kronecker product based fractals. aev 8/12/16
## gpKronFractal(m, n, pf, clr, ttl, dflg=0, psz=600):
## Where: m - initial matrix (filled with 0/1); n - order of the fractal;
## pf - plot file name (without extension); clr - color; ttl - plot title;
## dflg - writing dump file flag (0/1); psz - picture size.
gpKronFractal <- function(m, n, pf, clr, ttl, dflg=0, psz=600) {
  cat(" *** START:", date(), "n=", n, "clr=", clr, "psz=", psz, "\n");
  cat(" *** Plot file -", pf, "\n");
  r <- m;
  for(i in 1:n) {r = r%x%m};
  plotmat(r, pf, clr, ttl, dflg, psz);
  cat(" *** END:", date(), "\n");
}

## Required tests:
# 1. Vicsek Fractal
M <- matrix(c(0,1,0,1,1,1,0,1,0), ncol=3, nrow=3, byrow=TRUE);
gpKronFractal(M, 4, "VicsekFractalR","red", "Vicsek Fractal n=4")
# 2. Sierpinski carpet fractal
M <- matrix(c(1,1,1,1,0,1,1,1,1), ncol=3, nrow=3, byrow=TRUE);
gpKronFractal(M, 4, "SierpinskiCarpetFR", "maroon", "Sierpinski carpet fractal n=4")

# 3. Plus sign fractal
M <- matrix(c(1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,1,0,0,0,0,0,1,1,1,1,
+0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
gpKronFractal(M, 3, "PlusSignFR", "maroon", "Plus sign fractal, n=3")

# Also, try these 3. I bet you've never seen them before.
# 4. Wider Sierpinski carpet fractal (a.k.a. Sierpinski carpet mutant)
# Note: If your computer is not super fast it could take a lot of time.
#       Use dump flag = 1, to save generated fractal.
#M <- matrix(c(1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1), ncol=5,
#+nrow=5, byrow=TRUE);
#gpKronFractal(M, 4, "SierpinskiCarpetFw", "brown", "Wider Sierpinski carpet fractal n=4", 1)
# 5. "H" fractal (Try all other letters in the alphabet...)
#M <- matrix(c(1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,
#+0,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
#gpKronFractal(M, 3, "HFR", "maroon", "'H' fractal n=3", 1)
# 6. Chessboard fractal. 
#M <- matrix(c(1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,
#     0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1), ncol=8, nrow=8, byrow=TRUE);
#gpKronFractal(M, 2, "ChessBrdFractalR","black", "Chessboard Fractal, n=2")

```
 

```txt

> M <- matrix(c(0,1,0,1,1,1,0,1,0), ncol=3, nrow=3, byrow=TRUE);
> gpKronFractal(M, 4, "VicsekFractalR", "red", "Vicsek Fractal n=4")
 *** START: Mon Aug 29 16:14:14 2016 n= 4 clr= red 
 *** Plot file - VicsekFractalR 
 *** Matrix( 243 x 243 ) 3125  DOTS
 *** END: Mon Aug 29 16:14:14 2016 

> M <- matrix(c(1,1,1,1,0,1,1,1,1), ncol=3, nrow=3, byrow=TRUE);
> gpKronFractal(M, 4, "SierpinskiCarpetFR", "maroon", "Sierpinski carpet fractal n=4")
 *** START: Mon Aug 29 16:16:14 2016 n= 4 clr= maroon 
 *** Plot file - SierpinskiCarpetFR
 *** Matrix( 243 x 243 ) 32768  DOTS
 *** END: Mon Aug 29 16:16:32 2016 

> M <- matrix(c(1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,0,1,1,1,1,0,0,0,0,0,1,1,1,1,
+0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1), ncol=7, nrow=7, byrow=TRUE);
> gpKronFractal(M, 3, "PlusSignFR", "maroon", "Plus sign fractal, n=3")
 *** START: Thu Apr 06 21:45:33 2017 n= 3 clr= maroon psz= 600 
 *** Plot file - PlusSignFR 
 *** Matrix( 2401 x 2401 ) 2560000 DOTS
 *** END: Fri Apr 07 09:31:07 2017 

```



## REXX

This is a work-in-progress, this version shows the 1st order.

```rexx
/*REXX program calculates the   Kronecker product   of   two arbitrary size   matrices. */
parse arg pGlyph .                               /*obtain optional argument from the CL.*/
if pGlyph=='' | pGlyph==","  then pGlyph= '█'    /*Not specified?  Then use the default.*/
if length(pGlyph)==2  then pGlyph= x2c(pGlyph)   /*Plot glyph is 2 chars?   Hexadecimal.*/
if length(pGlyph)==3  then pGlyph= d2c(pGlyph)   /*  "    "    " 3   "      Decimal.    */
       aMat= 3x3  0 1 0 1 1 1 0 1 0              /*define  A  matrix size  and elements.*/
       bMat= 3x3  1 1 1 1 0 1 1 1 1              /*   "    B     "     "    "     "     */
call makeMat 'A', aMat                           /*construct   A   matrix from elements.*/
call makeMat 'B', bMat                           /*    "       B      "     "     "     */
call KronMat 'Kronecker product'                 /*calculate the  Kronecker  product.   */
call showMat 'Kronecker product', result         /*display   the  Kronecker  product.   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
KronMat: parse arg what;              #= 0;           parse var  @.a.shape  aRows  aCols
                                                      parse var  @.b.shape  bRows  bCols
           do       rA=1  for aRows
             do     rB=1  for bRows;  #= # + 1;       ##= 0;         _=
               do   cA=1  for aCols;  x= @.a.rA.cA
                 do cB=1  for bCols;  y= @.b.rB.cB;   ##= ## + 1;   xy= x * y;     _= _ xy
                 @.what.#.##= xy
                 end   /*cB*/
               end     /*cA*/
             end       /*rB*/
           end         /*rA*/;        return aRows * aRows   ||   'X'   ||   bRows * bRows
/*──────────────────────────────────────────────────────────────────────────────────────*/
makeMat: parse arg what, size elements;   arg , row 'X' col .;      @.what.shape= row  col
         #=0;    do   r=1  for row               /* [↓]  bump item#; get item; max width*/
                   do c=1  for col;   #= # + 1;        @.what.r.c= word(elements, #)
                   end   /*c*/                   /* [↑] define an element of WHAT matrix*/
                 end     /*r*/;           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
showMat: parse arg what, size .;   parse var size  row  'X'  col   /*obtain mat name, sz*/
                     do    r=1  for row;    $=                     /*build row by row.  */
                        do c=1  for col;    $= $ || @.what.r.c     /*  "   col  " col.  */
                        end   /*c*/
                     $= translate($, pGlyph, 10)                   /*change──►plot glyph*/
                     say strip($, 'T')                             /*display line──►term*/
                     end     /*r*/;       return
```

```txt

   ███
   █ █
   ███
█████████
█ ██ ██ █
█████████
   ███
   █ █
   ███

```



## Sidef

```ruby
func kronecker_product (a, b) { a ~X b -> map { _[0] ~X* _[1] } }

func kronecker_fractal(pattern, order=4) {
    var kronecker = pattern
    { kronecker = kronecker_product(kronecker, pattern) } * order
    return kronecker
}

var vicsek = [[0,1,0], [1,1,1], [0,1,0]]
var carpet = [[1,1,1], [1,0,1], [1,1,1]]
var six    = [[0,1,1,1,0], [1,0,0,0,1], [1,0,0,0,0],
              [1,1,1,1,0], [1,0,0,0,1], [1,0,0,0,1], [0,1,1,1,0]]

require("Imager")

for name,shape,order in [
    [:vicsek, vicsek, 4],
    [:carpet, carpet, 4],
    [:six,    six,    3],
] {
    var pat = kronecker_fractal(shape, order)
    var img = %O<Imager>.new(xsize => pat[0].len, ysize => pat.len)
    for x,y in (^pat[0].len ~X ^pat.len) {
        img.setpixel(x => x, y => y, color => (pat[y][x] ? <255 255 32> : <16 16 16>))
    }
    img.write(file => "kronecker-#{name}-sidef.png")
}
```

Output images: [https://github.com/trizen/rc/blob/master/img/kronecker-carpet-sidef.png Kronecker Carpet], [https://github.com/trizen/rc/blob/master/img/kronecker-vicsek-sidef.png Kronecker Vicsek] and [https://github.com/trizen/rc/blob/master/img/kronecker-six-sidef.png Kronecker Six]


## zkl

Uses Image Magick and
the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
var [const] GSL=Import.lib("zklGSL");    // libGSL (GNU Scientific Library)
fcn kronecker(A,B){  //--> new Matrix
   m,n, p,q := A.rows,A.cols, B.rows,B.cols;
   r:=GSL.Matrix(m*p, n*q);
   foreach i,j,k,l in (m,n,p,q){ r[p*i + k, q*j + l]=A[i,j]*B[k,l] }
   r
}

fcn kfractal(M,n,fname){
   R:=M;
   do(n){ R=kronecker(R,M) }
   r,c,img := R.rows, R.cols, PPM(r,c,0xFFFFFF);	// white canvas
   foreach i,j in (r,c){ if(R[i,j]) img[i,j]=0x00FF00 } // green dots
   println("%s: %dx%d with %,d points".fmt(fname,R.rows,R.cols,
        R.pump(0,Ref(0).inc,Void.Filter).value)); // count 1s in fractal matrix
   img.writeJPGFile(fname);
}
```


```zkl
var [const] A=GSL.Matrix(3,3).set(0,1,0, 1,1,1, 0,1,0),
            B=GSL.Matrix(3,3).set(1,1,1, 1,0,1, 1,1,1);
kfractal(A,4,"vicsek_k.jpg");
kfractal(B,4,"sierpinskiCarpet_k.jpg");
```

```txt

vicsek_k.jpg: 243x243 with 3,125 points
sierpinskiCarpet_k.jpg: 243x243 with 32,768 points

```

Images at [http://www.zenkinetic.com/Images/RosettaCode/vicsek_k.zkl.jpg Vicsek fractal]
and [http://www.zenkinetic.com/Images/RosettaCode/sierpinskiCarpet_k.zkl.jpg Sierpinski Carpet fractal].
