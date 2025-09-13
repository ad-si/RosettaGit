+++
title = "External sort"
description = ""
date = 2019-06-15T05:55:13Z
aliases = []
[extra]
id = 21147
[taxonomies]
categories = ["task"]
tags = []
+++

{{draft task}}Sort a huge file too large to fit into memory. The algorithm consists in reading a large file to be sorted in chunks of data small enough to fit in main memory, sort each of the chunks, write them out to a temporary file, and finally combined the smaller subfiles into a single larger file. For more info see: https://en.wikipedia.org/wiki/External_sorting

The sorting algorithm can be any popular sort, like quicksort. For simplicity one can assume that the file consists of fixed length integers and that the sort function is less-than (<).


## Go

This is a translation of the C++ code [https://www.geeksforgeeks.org/external-sorting/ here] which implements external sorting using a merge sort. In the interests of brevity, the extensive comments in the C++ version have been largely omitted.

A small test file consisting of random integers has been generated and sorted to demonstrate that the approach works.

```go
package main

import (
    "fmt"
    "io"
    "log"
    "math"
    "math/rand"
    "os"
    "time"
)

type MinHeapNode struct{ element, index int }

type MinHeap struct{ nodes []MinHeapNode }

func left(i int) int {
    return (2*i + 1)
}

func right(i int) int {
    return (2*i + 2)
}

func newMinHeap(nodes []MinHeapNode) *MinHeap {
    mh := new(MinHeap)
    mh.nodes = nodes
    for i := (len(nodes) - 1) / 2; i >= 0; i-- {
        mh.minHeapify(i)
    }
    return mh
}

func (mh *MinHeap) getMin() MinHeapNode {
    return mh.nodes[0]
}

func (mh *MinHeap) replaceMin(x MinHeapNode) {
    mh.nodes[0] = x
    mh.minHeapify(0)
}

func (mh *MinHeap) minHeapify(i int) {
    l, r := left(i), right(i)
    smallest := i
    heapSize := len(mh.nodes)
    if l < heapSize && mh.nodes[l].element < mh.nodes[i].element {
        smallest = l
    }
    if r < heapSize && mh.nodes[r].element < mh.nodes[smallest].element {
        smallest = r
    }
    if smallest != i {
        mh.nodes[i], mh.nodes[smallest] = mh.nodes[smallest], mh.nodes[i]
        mh.minHeapify(smallest)
    }
}

func merge(arr []int, l, m, r int) {
    n1, n2 := m-l+1, r-m
    tl := make([]int, n1)
    tr := make([]int, n2)
    copy(tl, arr[l:])
    copy(tr, arr[m+1:])
    i, j, k := 0, 0, l
    for i < n1 && j < n2 {
        if tl[i] <= tr[j] {
            arr[k] = tl[i]
            k++
            i++
        } else {
            arr[k] = tr[j]
            k++
            j++
        }
    }
    for i < n1 {
        arr[k] = tl[i]
        k++
        i++
    }
    for j < n2 {
        arr[k] = tr[j]
        k++
        j++
    }
}

func mergeSort(arr []int, l, r int) {
    if l < r {
        m := l + (r-l)/2
        mergeSort(arr, l, m)
        mergeSort(arr, m+1, r)
        merge(arr, l, m, r)
    }
}

// Merge k sorted files: es0,es1 etc.
func mergeFiles(outputFile string, n, k int) {
    in := make([]*os.File, k)
    var err error
    for i := 0; i < k; i++ {
        fileName := fmt.Sprintf("es%d", i)
        in[i], err = os.Open(fileName)
        check(err)
    }
    out, err := os.Create(outputFile)
    check(err)
    nodes := make([]MinHeapNode, k)
    i := 0
    for ; i < k; i++ {
        _, err = fmt.Fscanf(in[i], "%d", &nodes[i].element)
        if err == io.EOF {
            break
        }
        check(err)
        nodes[i].index = i
    }
    hp := newMinHeap(nodes[:i])
    count := 0
    for count != i {
        root := hp.getMin()
        fmt.Fprintf(out, "%d ", root.element)
        _, err = fmt.Fscanf(in[root.index], "%d", &root.element)
        if err == io.EOF {
            root.element = math.MaxInt32
            count++
        } else {
            check(err)
        }
        hp.replaceMin(root)
    }
    for j := 0; j < k; j++ {
        in[j].Close()
    }
    out.Close()
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

// Create initial runs, divide them evenly amongst the output files
// and then merge-sort them.
func createInitialRuns(inputFile string, runSize, numWays int) {
    in, err := os.Open(inputFile)
    out := make([]*os.File, numWays)
    for i := 0; i < numWays; i++ {
        fileName := fmt.Sprintf("es%d", i) // es0, es1 etc.
        out[i], err = os.Create(fileName)
        check(err)
    }
    arr := make([]int, runSize)
    moreInput := true
    nextOutputFile := 0
    var i int
    for moreInput {
        for i = 0; i < runSize; i++ {
            _, err := fmt.Fscanf(in, "%d", &arr[i])
            if err == io.EOF {
                moreInput = false
                break
            }
            check(err)
        }
        mergeSort(arr, 0, i-1)
        for j := 0; j < i; j++ {
            fmt.Fprintf(out[nextOutputFile], "%d ", arr[j])
        }
        nextOutputFile++
    }
    for j := 0; j < numWays; j++ {
        out[j].Close()
    }
    in.Close()
}

func externalSort(inputFile, outputFile string, numWays, runSize int) {
    createInitialRuns(inputFile, runSize, numWays)
    mergeFiles(outputFile, runSize, numWays)
}

func main() {
    // Create a small test file of 40 random ints and split it into 4 files
    // of 10 integers each.
    numWays := 4
    runSize := 10
    inputFile := "input.txt"
    outputFile := "output.txt"
    in, err := os.Create(inputFile)
    check(err)
    rand.Seed(time.Now().UnixNano())
    for i := 0; i < numWays*runSize; i++ {
        fmt.Fprintf(in, "%d ", rand.Intn(math.MaxInt32))
    }
    in.Close()
    externalSort(inputFile, outputFile, numWays, runSize)
    // remove temporary files
    for i := 0; i < numWays; i++ {
        fileName := fmt.Sprintf("es%d", i)
        err = os.Remove(fileName)
        check(err)
    }
}
```


Contents of input.txt:

```txt

921996447 760852351 223421434 1245608832 745990119 1414811249 1947335121 762344474 588429291 993452626 2592794 491133923 1275871423 1152039534 649892156 278215570 595760601 1878223040 1267371451 2097209826 1409628494 1147072290 309824251 108477605 1705270413 1821354697 1703557665 473708588 110138202 1292465428 946557804 148800949 1471244316 1508853596 1306802817 1016358698 1661284048 527644251 546155704 337874167

```

Contents of output.txt:

```txt

2592794 108477605 110138202 148800949 223421434 278215570 309824251 337874167 473708588 491133923 527644251 546155704 588429291 595760601 649892156 745990119 760852351 762344474 921996447 946557804 993452626 1016358698 1147072290 1152039534 1245608832 1267371451 1275871423 1292465428 1306802817 1409628494 1414811249 1471244316 1508853596 1661284048 1703557665 1705270413 1821354697 1878223040 1947335121 2097209826

```



## j

Untested on a memory mapped file.

```J

NB. Apply an in-place sorting algorithm to a memory mapped file
NB. in-place sort is translation of in-place python quicksort.

require 'jmf'
JCHAR map_jmf_ 'DATA'; 'file.huge'
NB. The noun DATA now refers to the memory mapped file.
NB. Use: quicksort DATA


NB. use: quicksort DATA
quicksort=: 3 :'qsinternal 0 , <:@:# ARRAY=: y'  NB. ARRAY is global

qsinternal=: 3 :0
 'start stop'=. y
 if. 0 < stop - start do.
  'left right pivot'=. start, stop, start{ARRAY   NB. pivot, left, right = array[start], start, stop
  while. left <: right do.           NB. while left <= right:
   while. pivot > left { ARRAY do.   NB. while array[left] < pivot:
    left=. >: left
   end.
   while. pivot < right { ARRAY do.  NB. while array[right] > pivot:
    right=. <: right                 NB. right -= 1
   end.
   if. left <: right do.             NB. if left <= right:

    NB. mapped files work by reference, assignment not required, but for testing.
    ARRAY=: (left, right) {`(|.@:[)`]} ARRAY NB. array[left], array[right] = array[right], array[left]

    left=. >: left                   NB. left += 1
    right=. <: right                 NB. right -= 1
   end.
  end.
  qsinternal start , right    NB. _quicksort(array, start, right)
  qsinternal left , stop      NB. _quicksort(array, left, stop)
 end.
 i. 0 0  NB. verbs return the final noun
)

```


Demonstration the sorting works:

```txt

   quicksort ?~10
   ARRAY
0 1 2 3 4 5 6 7 8 9
   
```




## Julia


```julia
intfile = open("/tmp/mmap.bin", "r+")

arr = Mmap.mmap(intfile, Vector{Int64}, (div(stat(intfile).size, 8))) # Int64 is 8 bytes

sort!(arr)

```



## Perl

Simulate task by reading from 'DATA' handle and using tiny record limit. As written, works for any numeric input, but could define any kind of customized sorting.

```perl
use strict;
use warnings;

my $max = 4; # records per merge file
my(@chunk,@tempf);

sub mysort ($$) { return $_[0] <=> $_[1] }

sub store {
    my($a) = @_;
    my $f = IO::File->new_tmpfile; # self-deleting after program exit
    print $f sort mysort @$a;
    seek $f, 0, 0 or warn "Oops: $!";
    push(@tempf, { fh => $f, queued => scalar <$f> } );
}

# read input and create sorted temporary files
while (<DATA>) {
    push @chunk, $_;
    store(\@chunk), @chunk = () if @chunk == $max;
}
store(\@chunk) if @chunk;

# merge everything
while (1) {
    my($lowest) = (sort { mysort($a->{queued}, $b->{queued}); } grep(defined $_->{queued}, @tempf) )[0];
    last unless $lowest->{queued};
    print $lowest->{queued};
    $lowest->{queued} = $lowest->{fh}->getline();
}

__DATA__
432
345
321
543
987
456
678
123
765
567
876
654
789
234
```

```txt
123
234
321
345
432
456
543
567
654
678
765
789
876
987
```



## Phix

Slight variation on [[Stream_Merge#Phix|Stream_Merge]]

```Phix
include builtins/pqueue.e
include builtins/pfile.e  -- write_lines() - not [yet] documented
 
procedure add(integer fn, pq)
    object line = gets(fn)
    if line=-1 then
        close(fn)
    else
        pq_add({fn,line}, pq)
    end if
end procedure
 
procedure sort_files(sequence filenames) 
    for i=1 to length(filenames) do
        sequence lines = get_text(filenames[i],GT_LF_STRIPPED),
                 sorted = sort(lines)
        printf(1,"%s:%v => %v\n",{filenames[i],lines,sorted})
        if write_lines(filenames[i],sorted)!=1 then ?9/0 end if
    end for
end procedure 

procedure merge_files(integer outfn, sequence filenames) 
    integer pq = pq_new()
    for i=1 to length(filenames) do
        add(open(filenames[i], "r"),pq)
    end for
    while not pq_empty(pq) do
        {integer fn, string line} = pq_pop(pq)
        puts(outfn,line)
        add(fn, pq)
    end while
    pq_destroy(pq)
end procedure 

procedure test()
    integer nf = rand(5),   -- number of files
            lp = 3          -- lines per file
    sequence filenames = {},
             lines = shuffle(tagset(nf*lp))
    for i=1 to nf do
        string filename = sprintf("file%d.txt",i)
        filenames = append(filenames,filename)
        integer fn = open(filename,"w")
        for l=1 to lp do
            printf(fn,"Line %02d\n",lines[l])
        end for
        lines = lines[lp+1..$]
        close(fn)
    end for
    printf(1,"sorting %d lines split over %d files\n",{nf*lp,nf})
    sort_files(filenames)
    integer outfn = 1 -- or open("results.txt","w")
    merge_files(outfn,filenames)
--  close(outfn)
    for i=1 to nf do
        {} = delete_file(filenames[i])
    end for
end procedure
test()
```

```txt

sorting 9 lines split over 3 files
file1.txt:{"Line 04","Line 01","Line 09"} => {"Line 01","Line 04","Line 09"}
file2.txt:{"Line 06","Line 07","Line 02"} => {"Line 02","Line 06","Line 07"}
file3.txt:{"Line 08","Line 03","Line 05"} => {"Line 03","Line 05","Line 08"}
Line 01
Line 02
Line 03
Line 04
Line 05
Line 06
Line 07
Line 08
Line 09

```



## Python

A technique demonstrated with a short string character data.

```python

#! /usr/bin/python3

'''
    $ # example session in bash
    $ python3 external_sort.py 
    expect 123456789
    memory size 1 passed
    memory size 2 passed
    memory size 3 passed
    memory size 4 passed
    memory size 5 passed
    memory size 6 passed
    memory size 7 passed
    memory size 8 passed
    memory size 9 passed
    memory size 10 passed
    memory size 11 passed
'''

import io

def sort_large_file(n: int, source: open, sink: open, file_opener = open)->None:

    '''
        approach:
            break the source into files of size n
            sort each of these files
            merge these onto the sink
    '''

    # store sorted chunks into files of size n
    mergers = []
    while True:
        text = list(source.read(n))
        if not len(text):
            break;
        text.sort()
        merge_me = file_opener()
        merge_me.write(''.join(text))
        mergers.append(merge_me)
        merge_me.seek(0)

    # merge onto sink
    stack_tops = [f.read(1) for f in mergers]
    while stack_tops:
        c = min(stack_tops)
        sink.write(c)
        i = stack_tops.index(c)
        t = mergers[i].read(1)
        if t:
            stack_tops[i] = t
        else:
            del stack_tops[i]
            mergers[i].close()
            del mergers[i]  # __del__ method of file_opener should delete the file

def main():
    '''
        test case
        sort 6,7,8,9,2,5,3,4,1 with several memory sizes
    '''

    # load test case into a file like object
    input_file_too_large_for_memory = io.StringIO('678925341')

    # generate the expected output
    t = list(input_file_too_large_for_memory.read())
    t.sort()
    expect = ''.join(t)
    print('expect', expect)

    # attempt to sort with several memory sizes
    for memory_size in range(1,12):
        input_file_too_large_for_memory.seek(0)
        output_file_too_large_for_memory = io.StringIO()
        sort_large_file(memory_size, input_file_too_large_for_memory, output_file_too_large_for_memory, io.StringIO)
        output_file_too_large_for_memory.seek(0)
        assert(output_file_too_large_for_memory.read() == expect)
        print('memory size {} passed'.format(memory_size))

if __name__ == '__main__':
   example = main
   example()

```



## REXX

Programming note:   the method used to generate the input file is to generate   '''N'''   records,

breaking up the records into sort work files of no more than   '''10'''   records   (limit).

The sort work files are then sorted with an external sort, and then merged into one big file. 

This particular example uses the DOS   '''SORT'''   and   '''DEL'''   commands.

```rexx
/*REXX pgm reads a file, splits into smaller files, sorts 'em, combines into sorted file*/
parse arg FID n lim seed .                       /*obtain optional arguments from the CL*/
if  FID=='' | FID==","  then FID= 'SORT_EXT.OUT' /*name of the  output  (sorted)  file. */
if    n=='' |   n==","  then   n=  500           /*number of records (rand #s) to gen.  */
if  lim=='' | lim==","  then lim=   10           /*number of records per SORTWORK file. */
if datatype(seed, 'W')  then call random ,,seed  /*Numeric?  Then use it as a rand seed.*/
sWork = 'SORTWORK.'                              /*the filename of the  SORTWORK  files.*/
call gen n,lim                                   /*generate   SORTWORK.nnn  files.      */
call srt #                                       /*sort records in all  SORTWORK  files.*/
call mrg                                         /*merge records in the SORTWORK  files.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
mrg: procedure expose FID sWork;  parse arg #    /*#:   the number of  SORTWORK  files. */
     @.= copies('ff'x, 1e5)                      /*no value should be larger than this. */
     call lineout FID, , 1                       /*position the output file at rec # 1. */
        do j=1  until @.j==@.;    call rdr j     /*read any number of  SORTWORK  files, */
        end   /*j*/                              /*but initially just 1 record per file.*/
     j=j-1                                       /*adj. J; read from a non─existent file*/
        do forever;            y=@.;   z=0       /*find the lowest value for  N  values.*/
          do k=1  for j                          /*traipse through the stemmed  @ array.*/
          if @.k==@.  then     call rdr k        /*Not defined?  Then read a file record*/
          if @.k<<y  then do;  y=@.k;  z=k;  end /*Lowest so far? Then mark this as min.*/
          end   /*k*/                            /* [↑]  note use of << exact comparison*/
        if z==0  then leave                      /*Any more records?   No, close file.  */
        call lineout FID, @.z                    /*write the value to the output file.  */
        call rdr z                               /*re-populate a value from the # file. */
        end   /*forever*/                        /*keep reading/merging until exhausted.*/
     call lineout FID                            /*close the output file (just in case).*/
     'DEL'  sWORK"*"                             /*delete all the  SORTWORK  files.     */
     return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen: procedure expose #;          parse arg m,siz;        d= digits() /*for justify.    */
     # = 0                                       /*number of  SORTWORK.nnn  files so far*/
          do j=1  for m;          #= 1   +   j % siz                  /*create workfile#*/
          call lineout  'SORTWORK.'#,  right(random(, 1e5), d)        /*write rand #.   */
          end   /*j*/
                      do k=1  for #;  call lineout 'SORTWORK.'#;  end /*close a workfile*/
          return
/*──────────────────────────────────────────────────────────────────────────────────────*/
rdr: parse arg a;   @.a=@.;   f=sWork||a;   if lines(f)\==0  then @.a= linein(f);   return
/*──────────────────────────────────────────────────────────────────────────────────────*/
srt: procedure expose sWork;  parse arg #
           do j=1  for #;   fn= sWORK || j;  'SORT'  fn  "/O" fn;  end  /*j*/;      return
```




