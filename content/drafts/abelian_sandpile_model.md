+++
title = "Abelian sandpile model"
description = ""
date = 2019-10-14T00:49:48Z
aliases = []
[extra]
id = 22490
[taxonomies]
categories = []
tags = []
+++

{{task}}{{wikipedia|Abelian sandpile model}}


Implement the '''Abelian sandpile model''' also known as '''Bak–Tang–Wiesenfeld model'''. It's history, mathematical definition and properties can be found under it's [https://en.wikipedia.org/wiki/Abelian_sandpile_model wikipedia article].

The task requires the creation of a 2D grid of arbitrary size on which "piles of sand" can be placed. Any "pile" that has 4 or more sand particles on it ''collapses'', resulting in '''four particles being subtracted from the pile''' and '''distributed among it's neighbors.'''

It is recommended to display the output in some kind of image format, as terminal emulators are usually too small to display images larger than a few dozen characters tall. As an example of how to accomplish this, see the [https://rosettacode.org/wiki/Bitmap/Write_a_PPM_file Bitmap/Write a PPM file] task.

'''Examples:'''


```txt

0 0 0 0 0    0 0 0 0 0
0 0 0 0 0    0 0 1 0 0
0 0 4 0 0 -> 0 1 0 1 0
0 0 0 0 0    0 0 1 0 0
0 0 0 0 0    0 0 0 0 0

0 0 0 0 0    0 0 0 0 0
0 0 0 0 0    0 0 1 0 0
0 0 6 0 0 -> 0 1 2 1 0
0 0 0 0 0    0 0 1 0 0
0 0 0 0 0    0 0 0 0 0

0  0 0  0  0    0 0 1 0 0
0  0 0  0  0    0 2 1 2 0
0  0 16 0  0 -> 1 1 0 1 1
0  0 0  0  0    0 2 1 2 0
0  0 0  0  0    0 0 1 0 0

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Abelian_sandpile_model this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth

{{works with|gforth|0.7.3}}



```forth
#! /usr/bin/gforth -d 20M
\ Abelian Sandpile Model

0 assert-level !

\ command-line

: parse-number  s>number? invert throw drop ;
: parse-size    ." size  : " next-arg parse-number dup . cr ;
: parse-height  ." height: " next-arg parse-number dup . cr ;
: parse-args    cr parse-size parse-height ;

parse-args constant HEIGHT constant SIZE

: allot-erase   create here >r dup allot r> swap erase ;
: size^2        SIZE dup * cells ;
: 2cells        [ 2 cells ] literal ;
: -2cells       [ 2cells negate ] literal ;

size^2 allot-erase arr

\ array processing
: ix            swap SIZE * + cells arr + ;
: center        SIZE 2/ dup ;
: write-cell    ix @ u. ;
: write-row     SIZE 0 ?do dup i write-cell loop drop cr ;
: arr.          SIZE 0 ?do i write-row loop ;

\ stack processing

: stack-empty?  dup -1 = ;
: stack-full?   stack-empty? invert ;

\ pgm-handling

: concat        { a1 l1 a2 l2 } l1 l2 + allocate throw dup dup a1 swap l1 cmove a2 swap l1 + l2 cmove l1 l2 + ;
: write-pgm     ." P2" cr SIZE u. SIZE u. cr ." 3" cr arr. ;
: u>s           0 <# #s #> ;
: filename      s" sandpile-" SIZE u>s concat s" -" concat HEIGHT u>s concat s" .pgm" concat ;
: to-pgm        filename w/o create-file throw ['] write-pgm over outfile-execute close-file throw ;

\ sandpile

: prep-arr      HEIGHT center ix ! ;
: prep-stack    -1 HEIGHT 4 u>= if center then ;
: prepare       prep-arr prep-stack ;
: ensure        if else 2drop 0 2rdrop exit then ;
: col>=0        dup 0>= ensure ;
: col<SIZE      dup SIZE < ensure ;
: row>=0        over 0>= ensure ;
: row<SIZE      over SIZE < ensure ;
: legal?        col>=0 col<SIZE row>=0 row<SIZE 2drop true ;
: north         1. d- ;
: east          1+ ;
: south         1. d+ ;
: west          1- ;
: reduce        2dup ix dup -4 swap +! @ 4 < if 2drop then ;
: increase      2dup legal? if 2dup ix dup 1 swap +! @ 4 = if 2swap else 2drop then else 2drop then ; 
: inc-north     2dup north increase ;
: inc-east      2dup east increase ;
: inc-south     2dup south increase ;
: inc-west      2dup west increase ;
: inc-all       inc-north inc-east inc-south inc-west 2drop ;
: simulate      prepare begin stack-full? while 2dup 2>r reduce 2r> inc-all repeat drop to-pgm ." written to " filename type cr ;

simulate bye
```


{{out}}
sandpile with 5000 grains of sand: 
<tt>./sandpile.fs 61 5000</tt>:
[http://commons.wikimedia.org/wiki/File:Sandpile-61-5000.png]

sandpile with 50000 grains of sand:
<tt>./sandpile.fs 201 50000</tt>:
[http://commons.wikimedia.org/wiki/File:Sandpile-201-50000.png]

sandpile with 500000 grains of sand:
<tt>./sandpile.fs 601 500000</tt>:
[http://commons.wikimedia.org/wiki/File:Sandpile-601-500000.png]



## Go

{{trans|Rust}}


Stack management in Go is automatic, starting very small (2KB) for each goroutine and expanding as necessary until the maximum allowed size is reached. 

```go
package main

import (
    "fmt"
    "log"
    "os"
    "strings"
)

const dim = 16 // image size

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

// Outputs the result to the terminal using UTF-8 block characters.
func drawPile(pile [][]uint) {
    chars:= []rune(" ░▓█")
    for _, row := range pile {
        line := make([]rune, len(row))
        for i, elem := range row {
            if elem > 3 { // only possible when algorithm not yet completed.
                elem = 3
            }
            line[i] = chars[elem]
        }
        fmt.Println(string(line))
    }
}

// Creates a .ppm file in the current directory, which contains
// a colored image of the pile.
func writePile(pile [][]uint) {
    file, err := os.Create("output.ppm")
    check(err)
    defer file.Close()
    // Write the signature, image dimensions and maximum color value to the file.
    fmt.Fprintf(file, "P3\n%d %d\n255\n", dim, dim)
    bcolors := []string{"125 0 25 ", "125 80 0 ", "186 118 0 ", "224 142 0 "}
    var line strings.Builder
    for _, row := range pile {        
        for _, elem := range row {
            line.WriteString(bcolors[elem])
        }
        file.WriteString(line.String() + "\n")
        line.Reset() 
    }
}

// Main part of the algorithm, a simple, recursive implementation of the model.
func handlePile(x, y uint, pile [][]uint) {
    if pile[y][x] >= 4 {
        pile[y][x] -= 4
        // Check each neighbor, whether they have enough "sand" to collapse and if they do,
        // recursively call handlePile on them.
        if y > 0 {
            pile[y-1][x]++
            if pile[y-1][x] >= 4 {
                handlePile(x, y-1, pile)
            }
        }
        if x > 0 {
            pile[y][x-1]++
            if pile[y][x-1] >= 4 {
                handlePile(x-1, y, pile)
            }
        }
        if y < dim-1 {
            pile[y+1][x]++
            if pile[y+1][x] >= 4 {
                handlePile(x, y+1, pile)
            }
        }
        if x < dim-1 {
            pile[y][x+1]++
            if pile[y][x+1] >= 4 {
                handlePile(x+1, y, pile)
            }
        }

        // Uncomment this line to show every iteration of the program.
        // Not recommended with large input values.
        // drawPile(pile)

        // Finally call the function on the current cell again,
        // in case it had more than 4 particles.
        handlePile(x, y, pile)
    }
}

func main() {
    // Create 2D grid and set size using the 'dim' constant.
    pile := make([][]uint, dim)
    for i := 0; i < dim; i++ {
        pile[i] = make([]uint, dim)
    }

    // Place some sand particles in the center of the grid and start the algorithm.
    hdim := uint(dim/2 - 1)
    pile[hdim][hdim] = 16
    handlePile(hdim, hdim, pile)
    drawPile(pile)

    // Uncomment this to save the final image to a file
    // after the recursive algorithm has ended.
    // writePile(pile)
}
```


{{out}}

```txt

                
                
                
                
                
       ░        
      ▓░▓       
     ░░ ░░      
      ▓░▓       
       ░        
                
                
                
                
                
                
       

```



## Haskell

{{works with|GHC|8.8.1}}
{{libheader|base|4.13.0.0}}
{{libheader|array|0.5.4.0}}
{{libheader|mtl|2.2.2}}


Using a custom monad to make the code cleaner.

```haskell
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Rosetta.AbelianSandpileModel.ST 
    ( simulate
    , test
    , toPGM
    ) where

import Control.Monad.Reader (asks, MonadReader (..), ReaderT, runReaderT)
import Control.Monad.ST (runST, ST)
import Control.Monad.State (evalStateT, forM_, lift, MonadState (..), StateT, modify, when)
import Data.Array.ST (freeze, readArray, STUArray, thaw, writeArray)
import Data.Array.Unboxed (array, assocs, bounds, UArray, (!))
import Data.Word (Word32)
import System.IO (hPutStr, hPutStrLn, IOMode (WriteMode), withFile)
import Text.Printf (printf)

type Point     = (Int, Int)
type ArrayST s = STUArray s Point Word32
type ArrayU    = UArray Point Word32

newtype M s a = M (ReaderT (S s) (StateT [Point] (ST s)) a)
    deriving (Functor, Applicative, Monad, MonadReader (S s), MonadState [Point])

data S s = S 
    { bMin :: !Point
    , bMax :: !Point
    , arr  :: !(ArrayST s)
    }

runM :: M s a -> S s -> [Point]-> ST s a
runM (M m) = evalStateT . runReaderT m

liftST :: ST s a -> M s a
liftST = M . lift . lift

simulate :: ArrayU -> ArrayU
simulate a = runST $ simulateST a

simulateST :: forall s. ArrayU -> ST s ArrayU
simulateST a = do
    let (p1, p2) = bounds a
        s = [p | (p, c) <- assocs a, c >= 4]
    b <- thaw a :: ST s (ArrayST s)
    let st = S { bMin = p1
               , bMax = p2
               , arr  = b
               }
    runM simulateM st s

simulateM :: forall s. M s ArrayU
simulateM = do
    ps <- get
    case ps of
        []      -> asks arr >>= liftST . freeze
        p : ps' -> do
            c <- changeArr p $ \x -> x - 4
            when (c < 4) $ put ps'
            forM_ [north, east, south, west] $ inc . ($ p)
            simulateM

changeArr :: Point -> (Word32 -> Word32) -> M s Word32
changeArr p f = do
    a    <- asks arr
    oldC <- liftST $ readArray a p
    let newC = f oldC
    liftST $ writeArray a p newC
    return newC

inc :: Point -> M s ()
inc p = do
    b <- inBounds p
    when b $ do
        c <- changeArr p succ
        when (c == 4) $ modify $ (p :)

inBounds :: Point -> M s Bool
inBounds p = do
    st <- ask
    return $ p >= bMin st && p <= bMax st

north, east, south, west :: Point -> Point
north (x, y) = (x, y + 1)
east  (x, y) = (x + 1, y)
south (x, y) = (x, y - 1)
west  (x, y) = (x - 1, y)

toPGM :: ArrayU -> FilePath -> IO ()
toPGM a fp = withFile fp WriteMode $ \h -> do
    let ((x1, y1), (x2, y2)) = bounds a
        width  = x2 - x1 + 1
        height = y2 - y1 + 1
    hPutStrLn h "P2"
    hPutStrLn h $ show width ++ " " ++ show height
    hPutStrLn h "3"
    forM_ [y1 .. y2] $ \y -> do
        forM_ [x1 .. x2] $ \x -> do
            let c = min 3 $ a ! (x, y)
            hPutStr h $ show c ++ " "
        hPutStrLn h ""

initArray :: Int -> Word32 -> ArrayU
initArray size height = array 
    ((-size, -size), (size, size))
    [((x, y), if x == 0 && y == 0 then height else 0) | x <- [-size .. size], y <- [-size .. size]]

test :: Int -> Word32 -> IO ()
test size height = do
    printf "size = %d, height = %d\n" size height
    let a  = initArray size height
        b  = simulate a
        fp = printf "sandpile_%d_%d.pgm" size height
    toPGM b fp
    putStrLn $ "wrote image to " ++ fp
```


{{out}}
sandpile with 1000 grains of sand: <tt>test 15 1000</tt>: [http://commons.wikimedia.org/wiki/File:Sandpile_15_1000.jpg]

sandpile with 10000 grains of sand: <tt>test 40 10000</tt>: [http://commons.wikimedia.org/wiki/File:Sandpile_40_10000.jpg]

sandpile with 100000 grains of sand: <tt>test 150 100000</tt>: [http://commons.wikimedia.org/wiki/File:Sandpile_150_100000.jpg]

sandpile with 1000000 grains of sand: <tt>test 400 1000000</tt>: [http://commons.wikimedia.org/wiki/File:Sandpile_400_1000000.jpg]



## Julia

Modified from code by Hayk Aleksanyan, viewable at github.com/hayk314/Sandpiles, license viewable there.

```julia
module AbelSand

# supports output functionality for the results of the sandpile simulations
# outputs the final grid in CSV format, as well as an image file

using CSV, DataFrames, Images

function TrimZeros(A)
    # given an array A trims any zero rows/columns from its borders
    # returns a 4 tuple of integers, i1, i2, j1, j2, where the trimmed array corresponds to A[i1:i2, j1:j2]
    # A can be either numeric or a boolean array

    i1, j1 = 1, 1
    i2, j2 = size(A)

    zz = typeof(A[1, 1])(0)    # comparison of a value takes into account the type as well

    # i1 is the first row which has non zero element
    for i = 1:size(A, 1)
        q = false
        for k = 1:size(A, 2)
            if A[i, k] != zz
                q = true
                i1 = i
                break
            end
        end

        if q == true
            break
        end
    end

    # i2 is the first from below row with non zero element
    for i in size(A, 1):-1:1
        q = false
        for k = 1:size(A, 2)
            if A[i, k] != zz
                q = true
                i2 = i
                break
            end
        end

        if q == true
            break
        end
    end

    # j1 is the first column with non zero element

    for j = 1:size(A, 2)
        q = false
        for k = 1:size(A, 1)
            if A[k, j] != zz
                j1 = j
                q = true
                break
            end
        end

        if q == true
            break
        end
    end

    # j2 is the last column with non zero element

    for j in size(A, 2):-1:1
        q=false
        for k=1:size(A,1)
            if A[k, j] != zz
                j2 = j
                q=true
                break
            end
        end

        if q==true
            break
        end
    end

    return i1, i2, j1, j2
end

function addLayerofZeros(A, extraLayer)
    # adds layer of zeros from all corners to the given array A

    if extraLayer <= 0
        return A
    end

    N, M = size(A)


    Z = zeros( typeof(A[1,1]), N + 2*extraLayer, M + 2*extraLayer)
    Z[(extraLayer+1):(N + extraLayer ), (extraLayer+1):(M+extraLayer)] = A

    return Z

end

function printIntoFile(A, extraLayer, strFileName, TrimSmallValues = false)
    # exports a 2d matrix A into a csv file
    # @extraLayer is an integers adding layer of 0-s sorrounding the output matrix

    # trimming off very small values; tiny values affect the performance of CSV export
    if TrimSmallValues == true
        A = map(x -> if (abs(x - floor(x)) < 0.01) floor(x) else x end, A) 
    end

    i1, i2, j1, j2  = TrimZeros( A )
    A = A[i1:i2, j1:j2]

    A = addLayerofZeros(A, extraLayer)

    CSV.write(string(strFileName,".csv"), DataFrame(A), writeheader = false)

    return A

end

function Array_magnifier(A, cell_mag, border_mag)
    # A is the main array; @cell_mag is the magnifying size of the cell,
    # @border_mag is the magnifying size of the border between lattice cells

    # creates a new array where each cell of the original array A appears magnified by size = cell_mag


    total_factor = cell_mag + border_mag

    A1 = zeros(typeof(A[1, 1]), total_factor*size(A, 1), total_factor*size(A, 2))

    for i = 1:size(A,1), j = 1:size(A,2), u = ((i-1)*total_factor+1):(i*total_factor),
                                          v = ((j-1)*total_factor+1):(j*total_factor)
        if(( u - (i - 1) * total_factor <= cell_mag) && (v - (j - 1) * total_factor <= cell_mag))
            A1[u, v] = A[i, j] 
        end
    end

    return A1

end

function saveAsGrayImage(A, fileName, cell_mag, border_mag, TrimSmallValues = false)
    # given a 2d matrix A, we save it as a gray image after magnifying by the given factors
    A1 = Array_magnifier(A, cell_mag, border_mag)
    A1 = A1/maximum(maximum(A1))

    # trimming very small values from A1 to improve performance
    if TrimSmallValues == true
        A1 = map(x -> if ( x < 0.01) 0.0 else round(x, digits = 2) end, A1) 
    end

    save(string(fileName, ".png") , colorview(Gray, A1)) 
end

function saveAsRGBImage(A, fileName, color_codes, cell_mag, border_mag)
    # color_codes is a dictionary, where key is a value in A and value is an RGB triplet
    # given a 2d array A, and color codes (mapping from values in A to RGB triples), save A
    # into fileName as png image after applying the magnifying factors

    A1 = Array_magnifier(A, cell_mag, border_mag)
    color_mat = zeros(UInt8, (3, size(A1, 1), size(A1, 2)))

    for i = 1:size(A1,1)
        for j = 1:size(A1,2)
            color_mat[:, i, j]  = get(color_codes, A1[i, j] , [0, 0, 0])
        end
    end

    save(string(fileName, ".png") , colorview(RGB, color_mat/255)) 
end

const N_size = 700       # the radius of the lattice Z^2, the actual size becomes (2*N+1)x(2*N+1)
const dx = [1, 0, -1, 0] # for a given (x,y) in Z^2, (x + dx, y + dy) for all (dx,dy) covers the neighborhood of (x,y)
const dy = [0, 1, 0, -1]

struct L_coord
    # represents a lattice coordinate
    x::Int
    y::Int
end

function FindCoordinate(Z::Array{L_coord,1}, a::Int, b::Int)
    # in the given array Z of coordinates finds the (first) index of the tuple (a,b)
    # if no match, returns -1

    for i=1:length(Z)
        if (Z[i].x == a) && (Z[i].y == b)
            return i
        end
    end

    return -1
end

function move(N)
    # the main function moving the pile sand grains of size N at the origin of Z^2 until the sandpile becomes stable

    Z_lat = zeros(UInt8, 2 * N_size + 1, 2 * N_size + 1)     # models the integer lattice Z^2, we will have at most 4 sands on each vertex
    V_sites = falses(2 * N_size + 1, 2 * N_size + 1)         # all sites which are visited by the sandpile process, are being marked here
    Odometer = zeros(UInt64, 2 * N_size + 1, 2 * N_size + 1) # stores the values of the odometer function


    walking = L_coord[]    # the coordinates of sites which need to move

    V_sites[N_size + 1, N_size + 1] = true

    # i1, ... j2  -> show the boundaries of the box which is visited by the sandpile process
    i1, i2, j1, j2 = N_size + 1, N_size + 1, N_size + 1, N_size + 1 
    n = N

    t1 = time_ns()
    
    while n > 0
        n -= 1

        Z_lat[N_size + 1, N_size + 1] += 1
        if (Z_lat[N_size + 1, N_size + 1] >= 4)
            push!(walking, L_coord(N_size + 1, N_size + 1))
        end

        while(length(walking) > 0)
            w = pop!(walking)
            x = w.x
            y = w.y

            Z_lat[x, y] -= 4
            Odometer[x, y] += 4

            for k = 1:4
                Z_lat[x + dx[k], y + dy[k]] += 1
                V_sites[x + dx[k], y + dy[k]] = true
                if Z_lat[x + dx[k], y + dy[k]] >= 4
                    if FindCoordinate(walking, x + dx[k] , y + dy[k]) == -1
                        push!(walking, L_coord( x + dx[k], y + dy[k]))
                    end
                end
            end

            i1 = min(i1, x - 1)
            i2 = max(i2, x + 1)
            j1 = min(j1, y - 1)
            j2 = max(j2, y + 1)
        end


    end #end of the main while
    t2 = time_ns()

    println("The final boundaries are:: ", (i2 - i1 + 1),"x",(j2 - j1 + 1), "\n")
    print("time elapsed: " , (t2 - t1) / 1.0e9, "\n")

    Z_lat = printIntoFile(Z_lat, 0, string("Abel_Z_", N))
    Odometer = printIntoFile(Odometer, 1, string("Abel_OD_", N))

    saveAsGrayImage(Z_lat, string("Abel_Z_", N), 20, 0)
    color_code = Dict(1=>[255, 128, 255], 2=>[255, 0, 0],3 => [0, 128, 255])
    saveAsRGBImage(Z_lat, string("Abel_Z_color_", N), color_code, 20, 0)

    # for the total elapsed time, it's better to use the @time macros on the main call

    return Z_lat, Odometer # these are trimmed in output module

end # end of function move


end # module


using .AbelSand

Z_lat, Odometer = AbelSand.move(100000)

```
{{out}}
[http://alahonua.com/temp/Abel_Z_color_100000.png Link to PNG output file for N=100000 ie. AbelSand.move(100000)] <br />
[http://alahonua.com/temp/Abel_Z_color_1000000.png Link to PNG output file (run time >90 min) for N=1000000 (move(1000000))]


## Perl


```Perl
#!/usr/bin/perl

use strict; # http://www.rosettacode.org/wiki/Abelian_sandpile_model
use warnings;

my ($high, $wide) = split ' ', qx(stty size);
my $mask = "\0" x $wide . ("\0" . "\177" x ($wide - 2) . "\0") x ($high - 5) .
  "\0" x $wide;
my $pile = $mask =~ s/\177/ rand() < 0.02 ? chr 64 + rand 20 : "\0" /ger;

for ( 1 .. 1e6 )
  {
  print "\e[H", $pile =~ tr/\0-\177/ 1-~/r, "\n$_";
  my $add = $pile =~ tr/\1-\177/\0\0\0\200/r; # set high bit for >=4
  $add =~ /\200/ or last;
  $pile =~ tr/\4-\177/\0-\173/; # subtract 4 if >=4
  for ("\0$add", "\0" x $wide . $add, substr($add, 1), substr $add, $wide)
    {
    $pile |= $_;
    $pile =~ tr/\200-\377/\1-\176/; # add one to each neighbor of >=4
    $pile &= $mask;
    }
  select undef, undef, undef, 0.1; # comment out for full speed
  }
```



## Phix

{{libheader|pGUI}}
Generates moving images similar to the julia output.
The distributed version also has variable speed, additional display modes, and a random dropping toggle.

```Phix
-- demo\rosetta\Abelian_sandpile_model.exw
include pGUI.e

Ihandle dlg, canvas
cdCanvas cddbuffer

sequence board = {{0,0,0},
                  {0,0,0},
                  {0,0,0}}

procedure drop(integer y, x)
    sequence moves = {}
    while true do
        board[y,x] += 1
        if board[y,x]>=4 then
            board[y,x] -= 4
            moves &= {{y,x-1},{y,x+1},{y-1,x},{y+1,x}}
        end if
        -- extend board if rqd (maintain a border of zeroes)
        if x=1 then                             -- extend left
            for i=1 to length(board) do
                board[i] = prepend(board[i],0)
            end for
            for i=1 to length(moves) do
                moves[i][2] += 1
            end for
        elsif x=length(board[1]) then           -- extend right
            for i=1 to length(board) do
                board[i] = append(board[i],0)
            end for
        end if
        -- (copy the all-0 lines from the other end...)
        if y=1 then                             -- extend up
            board = prepend(board,board[$])
            for i=1 to length(moves) do
                moves[i][1] += 1
            end for
        elsif y=length(board) then              -- extend down
            board = append(board,board[1])
        end if
        if length(moves)=0 then exit end if
        {y,x} = moves[$]
        moves = moves[1..$-1]
    end while   
    IupUpdate(canvas)
end procedure

function timer_cb(Ihandle /*ih*/)
    integer y = floor(length(board)/2)+1,
            x = floor(length(board[1])/2)+1
    drop(y,x)
    return IUP_DEFAULT
end function

function redraw_cb(Ihandle ih, integer /*posx*/, integer /*posy*/)
    IupGLMakeCurrent(ih)
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    for y=1 to length(board) do
        for x=1 to length(board[1]) do 
            integer c = board[y][x]
            if c!=0 then
                integer colour = {CD_VIOLET,CD_RED,CD_BLUE}[c]
                cdCanvasPixel(cddbuffer, x, y, colour)
            end if
        end for
    end for
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    IupGLMakeCurrent(ih)
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    cddbuffer = cdCreateCanvas(CD_GL, "300x100 %g", {res})
    cdCanvasSetBackground(cddbuffer, CD_PARCHMENT)
    return IUP_DEFAULT
end function

procedure main()
    IupOpen()
    canvas = IupGLCanvas("RASTERSIZE=300x100")
    IupSetCallbacks({canvas}, {"ACTION", Icallback("redraw_cb"),
                               "MAP_CB", Icallback("map_cb")})
    dlg = IupDialog(canvas,"TITLE=\"Abelian sandpile model\"")
    IupCloseOnEscape(dlg)
    IupShow(dlg)
    Ihandle timer = IupTimer(Icallback("timer_cb"), 10)
    IupMainLoop()
    IupClose()
end procedure
 
main()
```



## Python


```Python

import numpy as np
import matplotlib.pyplot as plt


def iterate(grid):
    changed = False
    for ii, arr in enumerate(grid):
        for jj, val in enumerate(arr):
            if val > 3:
                grid[ii, jj] -= 4
                if ii > 0:
                    grid[ii - 1, jj] += 1
                if ii < len(grid)-1:
                    grid[ii + 1, jj] += 1
                if jj > 0:
                    grid[ii, jj - 1] += 1
                if jj < len(grid)-1:
                    grid[ii, jj + 1] += 1
                changed = True
    return grid, changed


def simulate(grid):
    while True:
        grid, changed = iterate(grid)
        if not changed:
            return grid


if __name__ == '__main__':
    start_grid = np.zeros((10, 10))
    start_grid[4:5, 4:5] = 64
    final_grid = simulate(start_grid.copy())
    plt.figure()
    plt.gray()
    plt.imshow(start_grid)
    plt.figure()
    plt.gray()
    plt.imshow(final_grid)

```

<b>Output:</b> </n>
Before:

```Python

[[0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]
 [0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]
 [0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]
 [0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]
 [0. 0. 0. 0.64. 0. 0. 0. 0. 0.]
 [0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]
 [0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]
 [0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]
 [0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]
 [0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]]

```

After:

```Python

[[0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]
 [0. 0. 0. 1. 2. 1. 0. 0. 0. 0.]
 [0. 0. 2. 2. 2. 2. 2. 0. 0. 0.]
 [0. 1. 2. 2. 2. 2. 2. 1. 0. 0.]
 [0. 2. 2. 2. 0. 2. 2. 2. 0. 0.]
 [0. 1. 2. 2. 2. 2. 2. 1. 0. 0.]
 [0. 0. 2. 2. 2. 2. 2. 0. 0. 0.]
 [0. 0. 0. 1. 2. 1. 0. 0. 0. 0.]
 [0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]
 [0. 0. 0. 0. 0. 0. 0. 0. 0. 0.]]

```



## Rust


```rust
// Set image size.
const DIM: usize = 16;

// This function outputs the result to the console using UTF-8 block characters.
fn draw_pile(pile: &Vec<Vec<usize>>) {
    for row in pile {
        let mut line = String::with_capacity(row.len());
        for elem in row {
            line.push(match elem {
                0 => ' ',
                1 => '░',
                2 => '▒',
                3 => '▓',
                _ => '█'
            });
        }

        println!("{}", line);
    }
}

// This function creates a file called "output.ppm" in the directory the program was run, which contains
// a colored image of the pile.
fn write_pile(pile: &Vec<Vec<usize>>) {
    use std::fs::File; // Used for opening the file.
    use std::io::Write; // Used for writing to the file.

    // Learn more about PPM here: http://netpbm.sourceforge.net/doc/ppm.html
    let mut file = File::create("./output.ppm").unwrap();

    // We write the signature, image dimensions and maximum color value to the file.
    let _ = write!(file, "P3\n {} {}\n255\n", DIM, DIM).unwrap();

    for row in pile {
        let mut line = String::with_capacity(row.len()*6);
        for elem in row {
            line.push_str(match elem {
                0 => "125 0 25 ", // Background color for cells that have no "sand" in them.

                // Depending on how many particles of sand is there in the cell we use a different shade of yellow.
                1 => "125 80 0 ",
                2 => "186 118 0 ",
                3 => "224 142 0 ",

                // It is impossible to have more than 3 particles of sand in one cell after the program has run,
                // however, Rust demands that all branches have to be considered in a match statement, so we
                // explicitly tell the compiler, that this is an unreachable branch.
                _ => unreachable!() 
            });
        }

        let _ = write!(file, "{}", line).unwrap();
    }
}

// This is the main part of the algorithm, a simple, recursive implementation of the model.
fn handle_pile(x: usize, y: usize, pile: &mut Vec<Vec<usize>>) {
    if pile[y][x] >= 4 {
        pile[y][x] -= 4;

        // We check each neighbor, whether they have enough "sand" to collapse and if they do, 
        // we recursively call handle_pile on them.
        if y > 0 {
            pile[y-1][x] += 1;
            if pile[y-1][x] >= 4 {handle_pile(x, y-1, pile)}}

        if x > 0 {
            pile[y][x-1] += 1;
            if pile[y][x-1] >= 4 {handle_pile(x-1, y, pile)}}

        if y < DIM-1 {
            pile[y+1][x] += 1;
            if pile[y+1][x] >= 4 {handle_pile(x, y+1, pile)}}

        if x < DIM-1 {
            pile[y][x+1] += 1;
            if pile[y][x+1] >= 4 {handle_pile(x+1, y, pile)}}

        // Uncomment this line to show every iteration of the program. Not recommended with large input values.
        //draw_pile(&pile);

        // Finally we call the function on the current cell again, in case it had more than 4 particles.
        handle_pile(x,y,pile);
    }
}


fn main() {
    use std::thread::Builder; // Used to spawn a new thread.

    /* Rust by default uses a 2Mb stack, which gets quickly filled (resulting in a stack overflow) if we use any value larger than
     * about 30,000 as our input value. To circumvent this, we spawn a thread with 32Mbs of stack memory, which can easily handle
     * hundreds of thousands of sand particles. I tested the program using 256,000, but it should theoretically work with larger
     * values too. 
     */

    let _ = Builder::new().stack_size(33554432).spawn(|| {
        // This is our 2D grid. It's size can be set using the DIM constant found at the top of the code.
        let mut pile: Vec<Vec<usize>> = vec![vec![0;DIM]; DIM];

        // We place this much sand in the center of the grid.
        pile[DIM/2 - 1][DIM/2 - 1] = 16;

        // We start the algorithm on the pile we just created.
        handle_pile(DIM/2 - 1, DIM/2 - 1, &mut pile);

        
        draw_pile(&pile)
        
        // Uncomment this to save the image to a file after the recursive algorithm has ended.
        //write_pile(&pile)
    }).unwrap().join();
}
```


'''Output:'''

```txt

                
                
                
                
                
       ░        
      ▒░▒       
     ░░ ░░      
      ▒░▒       
       ░        
                
                
                
                
                
                

```

