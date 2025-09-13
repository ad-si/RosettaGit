+++
title = "Multi-dimensional array"
description = ""
date = 2019-03-25T23:37:34Z
aliases = []
[extra]
id = 18481
[taxonomies]
categories = ["task"]
tags = []
+++

For the purposes of this task, the actual memory layout or access method of this data structure is not mandated. It is enough to:
# State the number and extent of each index to the array.
# Provide specific, ordered, integer indices for all dimensions of the array together with a new value to update the indexed value.
# Provide specific, ordered, numeric indices for all dimensions of the array to obtain the arrays value at that indexed position.

;The task is to:
* State if the language supports multi-dimensional arrays in its syntax and usual implementation.
* State whether the language uses [https://en.wikipedia.org/wiki/Row-major_order row-major or column major order] for multi-dimensional array storage, or any other relevant kind of storage.
* Show how to create a four dimensional array in your language and set, access, set to another value; and access the new value of an integer-indexed item of the array.
 The idiomatic method for the language is preferred.
:* The array should allow a range of five, four, three and two (or two three four five if convenient), in each of the indices, in order. (For example, ''if'' indexing starts at zero for the first index then a range of 0..4 inclusive would suffice).
* State if memory allocation is optimised for the array - especially if contiguous memory is likely to be allocated.
* If the language has exceptional native multi-dimensional array support such as optional bounds checking, reshaping, or being able to state both the lower and upper bounds of index ranges, then this is the task to mention them.

Show all output here, (but you may judiciously use ellipses to shorten repetitive output text).
## ALGOL 68

Algol 68 supports multi-dimensional arrays as standard.
The maximum values for the indexes and the number of indexes is up to the implementation.

The upper and optionally the lower bounds of each index are specified when the array is created.
If omitted the, the lower bound defaults to 1. The bounds can be arbitrary integer expressions.

The upper and lower bounds are separated by ":" and each pair of bounds is separated from the next by a ",".
The array required by the task could be declared as follows:               #

```algol68
[ 1 : 5, 1 : 4, 1 : 3, 1 : 2 ]INT a;
```

As the lower bounds are all 1, this could be abbreviated to:

```algol68
[ 5, 4, 3, 2 ]INT a;
```

Note that the lower and upper bounds can be 0 or negative, e.g. the following declares an array of the same size but different bounds:

```algol68
[ -7 : -3. -3 : 0, -1 : 1, 0 : 1 ]INT x;
```

Individual array elements can be set and accessed by stating the index values separated by ",".
The following sets the lowest element of the array to 0:

```algol68
a[ 1, 1, 1, 1 ] := 0;
```

The following sets the highest element to the lowest:

```algol68
a[ 5, 4, 3, 2 ] := a[ 1, 1, 1, 1 ];
```

Subsets of the array can be set and accessed by stating the index values of the subsets,
e.g. the following sets a pair of elements near the start of the array to a pair of elements near the end:
Selecting a subset of an array is known as slicing.

```algol68
a[ 3 : 4, 1, 1, 1 ] := a[ 5, 4, 2 : 3, 1 ];
```

The whole array can also be assigned, e.g., the following sets the entire array to some values, using nested "literal arrays" (known as row-displays):

```algol68
# Note this requires the lower bounds of each index be 1                #
a := ( ( ( ( 1111, 1112 ), ( 1121, 1122 ), ( 1131, 1132 ) )
       , ( ( 1211, 1212 ), ( 1221, 1222 ), ( 1231, 1232 ) )
       , ( ( 1311, 1312 ), ( 1321, 1322 ), ( 1331, 1332 ) )
       , ( ( 1411, 1412 ), ( 1421, 1422 ), ( 1431, 1432 ) ) )
     , ( ( ( 2111, 2112 ), ( 2121, 2122 ), ( 2131, 2132 ) )
       , ( ( 2211, 2212 ), ( 2221, 2222 ), ( 2231, 2232 ) )
       , ( ( 2311, 2312 ), ( 2321, 2322 ), ( 2331, 2332 ) )
       , ( ( 2411, 2412 ), ( 2421, 2422 ), ( 2431, 2432 ) ) )
     , ( ( ( 3111, 3112 ), ( 3121, 3122 ), ( 3131, 3132 ) )
       , ( ( 3211, 3212 ), ( 3221, 3222 ), ( 3231, 3232 ) )
       , ( ( 3311, 3312 ), ( 3321, 3322 ), ( 3331, 3332 ) )
       , ( ( 3411, 3412 ), ( 3421, 3422 ), ( 3431, 3432 ) ) )
     , ( ( ( 4111, 4112 ), ( 4121, 4122 ), ( 4131, 4132 ) )
       , ( ( 4211, 4212 ), ( 4221, 4222 ), ( 4231, 4232 ) )
       , ( ( 4311, 4312 ), ( 4321, 4322 ), ( 4331, 4332 ) )
       , ( ( 4411, 4412 ), ( 4421, 4422 ), ( 4431, 4432 ) ) )
     , ( ( ( 5111, 5112 ), ( 5121, 5122 ), ( 5131, 5132 ) )
       , ( ( 5211, 5212 ), ( 5221, 5222 ), ( 5231, 5232 ) )
       , ( ( 5311, 5312 ), ( 5321, 5322 ), ( 5331, 5332 ) )
       , ( ( 5411, 5412 ), ( 5421, 5422 ), ( 5431, 5432 ) ) ) );
```

If the lower bounds are not 1, they can be temporarily be changed to 1 by using the "AT" construct, e.g. as in the following:


```algol68
# declare an array the same size as a, but with all lower bounds equal to 2: #
[ 2 : 6, 2 : 5, 2 : 4, 2 : 3 ]INT b;

# set b to the same values as a above:                                       #
b[ AT 1, AT 1, AT 1, AT 1 ] :=
     ( ( ( ( 1111, 1112 ), ( 1121, 1122 ), ( 1131, 1132 ) )
       , ( ( 1211, 1212 ), ( 1221, 1222 ), ( 1231, 1232 ) )
       , ( ( 1311, 1312 ), ( 1321, 1322 ), ( 1331, 1332 ) )
       , ( ( 1411, 1412 ), ( 1421, 1422 ), ( 1431, 1432 ) ) )
     , ( ( ( 2111, 2112 ), ( 2121, 2122 ), ( 2131, 2132 ) )
       , ( ( 2211, 2212 ), ( 2221, 2222 ), ( 2231, 2232 ) )
       , ( ( 2311, 2312 ), ( 2321, 2322 ), ( 2331, 2332 ) )
       , ( ( 2411, 2412 ), ( 2421, 2422 ), ( 2431, 2432 ) ) )
     , ( ( ( 3111, 3112 ), ( 3121, 3122 ), ( 3131, 3132 ) )
       , ( ( 3211, 3212 ), ( 3221, 3222 ), ( 3231, 3232 ) )
       , ( ( 3311, 3312 ), ( 3321, 3322 ), ( 3331, 3332 ) )
       , ( ( 3411, 3412 ), ( 3421, 3422 ), ( 3431, 3432 ) ) )
     , ( ( ( 4111, 4112 ), ( 4121, 4122 ), ( 4131, 4132 ) )
       , ( ( 4211, 4212 ), ( 4221, 4222 ), ( 4231, 4232 ) )
       , ( ( 4311, 4312 ), ( 4321, 4322 ), ( 4331, 4332 ) )
       , ( ( 4411, 4412 ), ( 4421, 4422 ), ( 4431, 4432 ) ) )
     , ( ( ( 5111, 5112 ), ( 5121, 5122 ), ( 5131, 5132 ) )
       , ( ( 5211, 5212 ), ( 5221, 5222 ), ( 5231, 5232 ) )
       , ( ( 5311, 5312 ), ( 5321, 5322 ), ( 5331, 5332 ) )
       , ( ( 5411, 5412 ), ( 5421, 5422 ), ( 5431, 5432 ) ) ) );
```

Bounds checking is standard and there is no standard way to turn it off. There may be implementation-specific mechanisms (probably using pragmatic comments) to do so.

The memory layout of an array is determined by the implementation and is not visible to the programmer.

"Flexible" arrays whose size can change during the execution of the program can be created.
These are used e.g. for the builtin STRING mode.

The size of a flexible array can be changed only be assigning a new array to it.

The bounds of the array can be determined using the LWB and UPB operators.
For a 1 dimensional array, the unary LWB and UPB operators can be used.

```algol68
# E.g. the following prints the lower and upper bounds of the first two      #
#      indexes of a ( in this case, 1, 5, 1 and 4 )                          #
print( ( 1 LWB a, 1 UPB a, 2 LWB a, 2 UPB a, newline ) );
```

```txt
         +1         +5         +1         +4
```



## ALGOL W

Algol W supports multi-dimensional arrays as standard.
The maximum values for the indexes and the number of indexes is up to the implementation.
The upper and lower bounds of each index are specified when the array is declared.
The bounds are evaluated on entry to the block containing the declaration and can be arbitrary integer expressions.

The upper and lower bounds are separated by "::" and each pair of bounds is separated from the next by a ",".
The array required by the task could be declared as follows:

```algolw
integer array a ( 1 :: 5, 1 :: 4, 1 :: 3, 1 :: 2 );
```

note that the lower and upper bounds can be 0 or negative, e.g. the following declares an array of the same size but different bounds:

```algolw
integer array x ( -7 :: -3. -3 :: 0, -1 :: 1, 0 :: 1 );
```


individual array elements can be set and accessed by stating the index
values separated by ",". The following sets the lowest element of the array to 0:

```algolw
a( 1, 1, 1, 1 ) := 0;
```


The following sets the highest element to the lowest:

```algolw
a( 5, 4, 3, 2 ) := a( 1, 1, 1, 1 );
```


Subsets of an array can be specified as parameters to procedures.
E.g.:

```algolw
% declare a procedure that has a three-dimensional array parameter       %
procedure p ( integer array a1 ( *, *, * ) ) ; a1( 1, 2, 1 ) := 3 ;

% call the procedure with a subset of the 4 dimensional array            %
p( a( *, 2, *, * ) );
```


The elements of the array can only be set individually, it is not possible to assign
the whole array or multiple elements other than with element-by-element assignments.

Bounds checking is standard and there is no standard way to turn it off.
There may be implementation-specific mechanisms to do so.

The memory layout of an array is determined by the implementation and is not visible to the programmer.


## C

C supports multi-dimensional arrays, and thanks to the inter-op of arrays and points in C, it is possible to target any section of memory and drill down to the bit level. C uses row-major order for storing arrays. There are many ways to declare arrays, or pointers which can then be used as arrays, as shown below :

```C
/*Single dimensional array of integers*/
int a[10];

/*2-dimensional array, also called matrix of floating point numbers.
This matrix has 3 rows and 2 columns.*/

float b[3][2];

/*3-dimensional array ( Cube ? Cuboid ? Lattice ?) of characters*/

char c[4][5][6];

/*4-dimensional array (Hypercube ?) of doubles*/

double d[6][7][8][9];

/*Note that the right most number in the [] is required, all the others may be omitted.
Thus this is ok : */

int e[][3];

/*But this is not*/

float f[5][4][];

/*But why bother with all those numbers ? You can also write :*/
int *g;

/*And for a matrix*/
float **h;

/*or if you want to show off*/
double **i[];

/*you get the idea*/

char **j[][5];
```

When arrays are declared with all dimensions at the outset, all memory is allocated immediately once the program execution reaches the point where the array is declared. The following implementation shows just a few of the many ways of dealing with arrays :

```C

#include<stdio.h>

int main()
{
	int hyperCube[5][4][3][2];

	/*An element is set*/

	hyperCube[4][3][2][1] = 1;

	/*IMPORTANT : C ( and hence C++ and Java and everyone of the family ) arrays are zero based.
	The above element is thus actually the last element of the hypercube.*/

	/*Now we print out that element*/

	printf("\n%d",hyperCube[4][3][2][1]);

	/*But that's not the only way to get at that element*/
	printf("\n%d",*(*(*(*(hyperCube + 4) + 3) + 2) + 1));

	/*Yes, I know, it's beautiful*/
	*(*(*(*(hyperCube+3)+2)+1)) = 3;

	printf("\n%d",hyperCube[3][2][1][0]);

	return 0;
}

```

Declared as above or by using the functions malloc or calloc of the C Standard Library, memory is allocated contiguously. This obviously assumes that the machine has sufficient memory. The following implementation shows how a multi pointer can work as an array :

```C


#include<stdlib.h>
#include<stdio.h>

/*The stdlib header file is required for the malloc and free functions*/

int main()
{
	/*Declaring a four fold integer pointer, also called
	a pointer to a pointer to a pointer to an integer pointer*/

	int**** hyperCube, i,j,k;

	/*We will need i,j,k for the memory allocation*/

	/*First the five lines*/

	hyperCube = (int****)malloc(5*sizeof(int***));

	/*Now the four planes*/

	for(i=0;i<5;i++){
		hyperCube[i] = (int***)malloc(4*sizeof(int**));

		/*Now the 3 cubes*/

		for(j=0;j<4;j++){
			hyperCube[i][j] = (int**)malloc(3*sizeof(int*));

			/*Now the 2 hypercubes (?)*/

			for(k=0;k<3;k++){
				hyperCube[i][j][k] = (int*)malloc(2*sizeof(int));
			}
		}
	}

	/*All that looping and function calls may seem futile now,
	but imagine real applications when the dimensions of the dataset are
	not known beforehand*/

	/*Yes, I just copied the rest from the first program*/

	hyperCube[4][3][2][1] = 1;

	/*IMPORTANT : C ( and hence C++ and Java and everyone of the family ) arrays are zero based.
	The above element is thus actually the last element of the hypercube.*/

	/*Now we print out that element*/

	printf("\n%d",hyperCube[4][3][2][1]);

	/*But that's not the only way to get at that element*/
	printf("\n%d",*(*(*(*(hyperCube + 4) + 3) + 2) + 1));

	/*Yes, I know, it's beautiful*/
	*(*(*(*(hyperCube+3)+2)+1)) = 3;

	printf("\n%d",hyperCube[3][2][1][0]);

	/*Always nice to clean up after you, yes memory is cheap, but C is 45+ years old,
	and anyways, imagine you are dealing with terabytes of data, or more...*/

	free(hyperCube);

	return 0;
}
```

C does not do bound checking / range checking or for that matter any type of safety check. C Programmers are supposed to know what they are doing, the code can access any part of memory and as long as nothing prevents it, as in the OS, anti virus etc, the program will get hold of that memory. Of course, this means that the program can seriously damage itself, other programs, the machine and anything connected to it, but use it properly and you can write things such as Linux. Reallocation is possible via realloc but it is buggy and data corruption is possible. Linked lists are a much better option.


## C++


```cpp
#include <iostream>
#include <vector>

// convienince for printing the contents of a collection
template<typename T>
std::ostream& operator<<(std::ostream& out, const std::vector<T>& c) {
    auto it = c.cbegin();
    auto end = c.cend();

    out << '[';
    if (it != end) {
        out << *it;
        it = std::next(it);
    }
    while (it != end) {
        out << ", " << *it;
        it = std::next(it);
    }
    return out << ']';
}

void fourDim() {
    using namespace std;

    // create a 4d jagged array, with bounds checking etc...
    vector<vector<vector<vector<int>>>> arr;
    int cnt = 0;

    arr.push_back(vector<vector<vector<int>>>{});
    arr[0].push_back(vector<vector<int>>{});
    arr[0][0].push_back(vector<int>{});
    arr[0][0][0].push_back(cnt++);
    arr[0][0][0].push_back(cnt++);
    arr[0][0][0].push_back(cnt++);
    arr[0][0][0].push_back(cnt++);
    arr[0].push_back(vector<vector<int>>{});
    arr[0][1].push_back(vector<int>{});
    arr[0][1][0].push_back(cnt++);
    arr[0][1][0].push_back(cnt++);
    arr[0][1][0].push_back(cnt++);
    arr[0][1][0].push_back(cnt++);
    arr[0][1].push_back(vector<int>{});
    arr[0][1][1].push_back(cnt++);
    arr[0][1][1].push_back(cnt++);
    arr[0][1][1].push_back(cnt++);
    arr[0][1][1].push_back(cnt++);

    arr.push_back(vector<vector<vector<int>>>{});
    arr[1].push_back(vector<vector<int>>{});
    arr[1][0].push_back(vector<int>{});
    arr[1][0][0].push_back(cnt++);
    arr[1][0][0].push_back(cnt++);
    arr[1][0][0].push_back(cnt++);
    arr[1][0][0].push_back(cnt++);

    cout << arr << '\n';
}

int main() {
    /* C++ does not have native support for multi-dimensional arrays,
     * but classes could be written to make things easier to work with.
     * There are standard library classes which can be used for single dimension arrays.
     * Also raw access is supported through pointers as in C.
     */

    fourDim();

    return 0;
}
```

```txt
[[[[0, 1, 2, 3]], [[4, 5, 6, 7], [8, 9, 10, 11]]], [[[12, 13, 14, 15]]]]
```



## C#

C# supports both multidimensional arrays and jagged arrays (e.g. arrays of arrays).<br/>
A multidimensional array with 2 rows and 3 columns can be initialized with:

```c#
var array = new int[,] { //Dimensions are inferred
    { 1, 2, 3 },
    { 4, 5, 6}
}
//Accessing a single element:
array[0, 0] = 999;

//To create a 4-dimensional array with all zeroes:
var array = new int[5, 4, 3, 2];
```

Under the hood, this is just one object; one contiguous block of memory is allocated.<br/>

For jagged arrays, each dimension must be initialized separately. They can be non-rectangular.

```c#
var array = new int[][] { //Dimensions are inferred
    new [] { 1, 2, 3, 4 },
    new [] { 5, 6, 7, 8, 9, 10 }
}
//Accessing a single element:
array[0][0] = 999;

//To create a 4-dimensional array with all zeroes:
var array = new int[5][][][];
for (int a = 0; a < array.Length; a++) {
    array[a] = new int[4][][];
    for (int b = 0; b < array[a].Length; b++) {
        array[a][b] = new int[3][];
        for (int c = 0; c < array[a][b].Length; c++) {
            array[a][b][c] = new int[2];
        }
    }
}
```


C# also supports arrays with non-zero bounds (mainly for handling arrays that are returned from unmanaged code)

```c#
var array = (int[,,,])Array.CreateInstance(typeof(int), new [] { 5, 4, 3, 2 }, new [] { 10, 10, 10, 10 });
int n = 1;
//Note: GetUpperBound is inclusive
for (int a = array.GetLowerBound(0); a <= array.GetUpperBound(0); a++)
for (int b = array.GetLowerBound(1); b <= array.GetUpperBound(1); b++)
for (int c = array.GetLowerBound(2); c <= array.GetUpperBound(2); c++)
for (int d = array.GetLowerBound(3); d <= array.GetUpperBound(3); d++)
    array[a, b, c, d] = n++;

//To set the first value, we must now use the lower bounds:
array[10, 10, 10, 10] = 999;
//As with all arrays, Length gives the TOTAL length.
Console.WriteLine("Length: " + array.Length);
Console.WriteLine("First 30 elements:");
//The multidimensional array does not implement the generic IEnumerable<int>
//so we need to cast the elements.
Console.WriteLine(string.Join(" ",  array.Cast<int>().Take(30)) + " ...");
```

```txt

Length: 120
First 30 elements:
999 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 ...

```



## D


```D
import std.stdio;

/*
 * Using just what is built-in, D only supports single dimension arrays. Like other languages, arrays can be built up as jagged arrays.
 * Arrays can either be created with a fixed size, or dynamically with support for resizing.
 */
void nativeExample() {
    int[3] staticArray; // Statically allocated array. Will only contain three elements, accessed from 0 to 2 inclusive.
    staticArray[0] = 1;
    staticArray[1] = 2;
    staticArray[2] = 3;
    writeln("Static array: ", staticArray);

    int[] dynamicArray; // Dynamically allocated array.
    dynamicArray.length = 3; // The array can be resized at runtime. If the number elements exceeds the allocated memory, new memory will be given from the heap.
    dynamicArray[0] = 4;
    dynamicArray[1] = 5;
    dynamicArray[2] = 6;
    dynamicArray ~= 7; // New elements can be concatenated.
    writeln("Dynamic array: ", dynamicArray);
}

/*
 * Multi-dimensional arrays can be created as custom types (classes or structs). They can have as many dimensions as are written for support.
 * The indexes can either be standard 0-n, or arbitrary m-n as needed. This example shows just a two dimensional example with standard indexes.
 * As few or as many of these pieces can be implemented as desired (compile-time error is given if a feature is not supported).
 */
struct Matrix(T) {
    // A dynamic array for the actual storage.
    private:
    T[] source;
    uint rows, cols; // dimensions

    public:
    this(uint m, uint n) {
        rows = m;
        cols = n;
        source.length = m*n;
    }

    /// Allow for short access to limits, e.g. a[$-1,$-1]
    int opDollar(size_t pos : 0)() const {
        return rows;
    }
    int opDollar(size_t pos : 1)()  const {
        return cols;
    }

    /// Allow for indexing to read a value, e.g. a[0,0]
    T opIndex(int i, int j) const in {
        assert(0 <= i && i <= rows, "Row index out of bounds");
        assert(0 <= j && j <= cols, "Col index out of bounds");
    } body {
        return source[i*rows + j];
    }

    /// Allow for assigning elements, e.g. a[0,0] = c
    T opIndexAssign(T elem, int i, int j) in {
        assert(0 <= i && i <= rows, "Row index out of bounds");
        assert(0 <= j && j <= cols, "Col index out of bounds");
    } body {
        auto index = rows*i + j;
        T prev = source[index];
        source[index] = elem;
        return prev;
    }

    /// Allow for applying operations and assigning elements, e.g. a[0,0] += c
    T opIndexOpAssign(string op)(T elem, int i, int j) {
        auto index = rows*i + j;
        T prev = source[index];
        mixin("source[index] " ~ op ~ "= elem;");
        return prev;
    }

    /// Support slicing, shown below
    auto opSlice(size_t pos)(int a, int b) in {
        assert(0 <= a && a <= opDollar!pos);
        assert(0 <= b && b <= opDollar!pos);
    } body {
        if (pos == 0) {
        } else {
            assert(0 <= a && a <= cols, "Col slice out of bounds");
            assert(0 <= b && b <= cols, "Col slice out of bounds");
        }
        return [a, b];
    }

    /// Allow for getting a sub-portion of the matrix, e.g. [0..2, 2..4]
    auto opIndex(int[] a, int[] b) const {
        auto t = Matrix!T(a.length, b.length);
        foreach(i, ia; a) {
            foreach(j, jb; b) {
                t[i, j] = this[ia, jb];
            }
        }
        return t;
    }
    auto opIndex(int[] a, int b) const {
        return opIndex(a, [b,b+1]);
    }
    auto opIndex(int a, int[] b) const {
        return opIndex([a,a+1], b);
    }

    /// Assign a single value to every element
    void opAssign(T value) {
        source[0..$] = value;
    }

    /// Assign a single element to a subset of the Matrix
    void opIndexAssign(T elem, int[] a, int[] b) {
        for (int i=a[0]; i<a[1]; i++) {
            auto start = rows * i;
            source[start+b[0]..start+b[1]] = elem;
        }
    }
    void opIndexAssign(T elem, int[] a, int b) {
        opIndexAssign(elem, a, [b, b+1]);
    }
    void opIndexAssign(T elem, int a, int[] b) {
        opIndexAssign(elem, [a, a+1], b);
    }

    /// Define how to write Matrix values as a string. Only does a simple string representation
    void toString(scope void delegate(const(char)[]) sink) const {
        import std.format;

        sink("[");
        foreach (i; 0..opDollar!0) {
            sink("[");
            foreach (j; 0..opDollar!1) {
                formattedWrite(sink, "%s", opIndex(i,j));
                if (j < cols-1) sink(", ");
            }
            if (i < rows-1) sink("]\n ");
            else sink("]");
        }
        sink("]");
    }
}

void customArray() {
    auto multi = Matrix!int(3, 3);
    writeln("Create a multidimensional object:\n", multi);
    writeln("Access an element: ", multi[0,0]);
    multi[0,0] = 1;
    writeln("Assign an element: ", multi[0,0]);
    multi[0,0] += 2;
    writeln("Arithmetic on an element: ", multi[0,0]);
    writeln("Slice a matrix:\n", multi[0..2, 0..2]);
    multi = 5;
    writeln("Assign all elements:\n", multi);
    multi[0..2,1..3] = 4;
    writeln("Assign some elements:\n", multi);
}

void main() {
    nativeExample();
    customArray();
}
```

```txt
Static array: [1, 2, 3]
Dynamic array: [4, 5, 6, 7]
Create a multidimensional object:
[[0, 0, 0]
 [0, 0, 0]
 [0, 0, 0]]
Access an element: 0
Assign an element: 1
Arithmetic on an element: 3
Slice a matrix:
[[3, 0]
 [0, 0]]
Assign all elements:
[[5, 5, 5]
 [5, 5, 5]
 [5, 5, 5]]
Assign some elements:
[[5, 4, 4]
 [5, 4, 4]
 [5, 5, 5]]
```



## EchoLisp

EchoLisp natively supports 1 and 2-dimensions arrays : lib '''matrix'''. The following shows an implementation of multi-dimensional arrays, using contiguous memory and bound checking. Indices are vectors #(i j k ...) of integers.

```scheme
(require 'math) ;; dot-product

;; dims = vector #(d1 d2 .....)
;; allocates a new m-array
(define (make-m-array dims (init 0))
	;; allocate 2 + d1*d2*d3... consecutive cells
	(define msize (apply * (vector->list dims)))
	(define m-array (make-vector (+ 2 msize) init))

	;; compute displacements vector once for all
	;; m-array[0] = [1 d1 d1*d2 d1*d2*d3 ...]
	(define disps (vector-rotate! (vector-dup dims) 1))
	(vector-set! disps 0 1)
	(for [(i(in-range 1 (vector-length disps)) )]
	      (vector-set! disps i (* [disps i] [disps (1- i)])))
	(vector-set! m-array 0 disps)

	(vector-set! m-array 1 dims) ;; remember dims
	m-array)

;; from indices = #(i j k ...) to displacement
(define-syntax-rule (m-array-index ma indices)
	(+ 2 (dot-product (ma 0)  indices)))

;; check i < d1, j < d2, ...
(define (m-array-check ma indices)
	(for [(dim [ma 1]) (idx indices)]
		#:break (>= idx dim) => (error 'm-array:bad-index (list idx  '>= dim))))

;; --------------------
;; A P I
;; --------------------
;; indices is a vector #[i j k ...]
;; (make-m-array (dims) [init])

(define (m-array-dims ma) [ma 1])

; return ma[indices]
(define (m-array-ref ma indices)
	(m-array-check ma indices)
	[ma (m-array-index ma indices)])
; sets ma[indices]
(define (m-array-set! ma indices value )
	(m-array-check ma indices)
	(vector-set! ma (m-array-index ma indices) value))
```

```txt
(define MA (make-m-array #(5 4 3 2) 1))
(m-array-dims MA)
    → #( 5 4 3 2)
(m-array-set! MA #(3 2 1 1 ) '🍓 )
(m-array-ref MA #(3 2 1 1 ))
    → 🍓
MA
    → #( #( 1 5 20 60) #( 5 4 3 2)  1 1 1 1 ...  1 1 🍓 1 1 1 1 1 ... 1 1 1 1 1 1)

(m-array-ref MA #(1 1 42 2))
😡 error: m-array:bad-index (42 >= 3)
```



## Fortran

Messing with multi-dimensional arrays has been central from the introduction of Fortran in 1954. Array indexing is with positive integers and always starts with one, so the DIMENSION statement merely specifies the upper bound of an index via integer constants and an array may have up to three dimensions. All storage for an array is contiguous. Access to an element of a one-dimensional array A is via <code>A(''integer expression'')</code> and early Fortran allowed only simple expressions, no more than c*v ± b which is to say an integer constant times an integer variable plus (or minus) an integer constant. These restrictions simplified the thorough optimisation done by the first compiler. Later compilers abandoned those optimisations, relaxed the constraints on index expressions and allowed more dimensions.

```Fortran
      DIMENSION A(5,4,3,2)                 !Declares a (real) array A of four dimensions, storage permitting.
      X = 3*A(2,I,1,K)                     !Extracts a certain element, multiplies its value by three, result to X.
      A(1,2,3,4) = X + 1                   !Places a value (the result of the expression X + 1) ... somewhere...
```

Compilers typically perform no checking on the bounds of array indices, even when values are being written to some address. An immediate crash would be the best result. Array bound checking may be requested as a compiler option in some cases, resulting in larger code files, slower execution, and some bug detection.

With Fortran 90 came a large expansion of abilities. Lower bounds could also be specified for an array's dimension, arrays defined in subprograms could be sized according to values at execution time (not constants at compile time) as in <code>A(-5:3,N + 7)</code> where N was a parameter to the subroutine, and if that was insufficient, arrays of a desired size could be allocated (and deallocated) according to program logic within a routine.

As well, array data could be manipulated with array processing statements, rather than always having to prepare DO-loops or similar, in particular <code>A = 0</code> would set all values of array A to zero, whatever its dimensionality. Thus, given an array B(0:2,0:2), a two-dimensional array, <code>B(1:2,1:2) = 6*A(1,3:4,1,1:2) - 7</code> would select certain elements scattered about in A, perform the calculation, and store the results in a part of B. Hopefully, element-by element without using a temporary array. Even more complicated selections can be made via the RESHAPE intrinsic now available.

Data are stored in Fortran arrays in "column-major" order, with the result that successive elements are indexed with the ''left''-most subscript varying most rapidly. Thus, given an array <code>DIMENSION A(3,3)</code> the elements are ordered as A(1,1), A(2,1), A(3,1), A(1,2), A(2,2), A(3,2), A(1,3), A(2,3), A(3,3). This doesn't matter when the array is referenced via something like A(''row'',''column''). However, should the array A be written (or read) or initialised via a DATA statement without specification of indices, that is the order of the values. Consider
```Fortran
      INTEGER A(3,3)	!An array, regarded as being a matrix.
      DATA A/1,2,3,	!Some initial values.
     2       4,5,6,	!Laid out as if a matrix.
     3       7,8,9/	!But supplied as consecutive elements.

      WRITE (6,*) "Writing A..."
      WRITE (6,1) A	!Using the array order.
    1 FORMAT (3I2)	!After three values, start a new line.

      WRITE (6,*) "Writing A(row,col) down rows and across columns..."
      DO I = 1,3	!Write each row, one after the other.
        WRITE (6,1) (A(I,J), J = 1,3)	!The columns along a row.
      END DO		!Onto the next row.
      END
```

Output:

```txt
 Writing A...
 1 2 3
 4 5 6
 7 8 9
 Writing A(row,col) down rows and across columns...
 1 4 7
 2 5 8
 3 6 9
```

The same will happen if data were to be supplied with that layout and read by something like <code>READ(in,*) A</code> To read data with that layout but obtain the desired ordering, the read must proceed with explicit specification of the index ordering as with the two loops of the second attempt at WRITE. This done, programming proceeds and functions such as MATMUL will give results as expected.

This affects higher-dimension arrays also. A(3,3,100) will give a hundred sets of 3x3 arrays, which might be used in something like <code>CALL PROCESS(A(1,1,IT))</code> where subroutine PROCESS expects a two-dimensional array and need only engage in two-level indexing rather than three. Conversely, A(100,3,3) will produce confusion.

For large arrays when memory is not truly random-access (on-chip memory versus main memory, main memory versus slower disc storage) a calculation that works along a row will be accessing widely-separated elements in storage, perhaps with only one "hit" per virtual memory page before leaping to another, while an equivalent calculation working down a column will enjoy multiple hits per page. The difference can be large enough to promote usages such as swapping the order of the DO-loops for portions of a calculation, or swapping the order of the indices as by writing <code>A(J,I)</code> when it would be more natural to use <code>A(I,J)</code>.


## Go

Go does not provide multi-dimensional arrays or slices.  They can be implemented as nested arrays or slices.  This is commonly done and is idiomatic enough for 2D arrays and slices, but can become awkward for higher dimensions.  The alternative is to use a single array or allocate a single slice as a flattened representation.  There can be some tradeoffs between the two techniques in syntax, convenience, and in consequences of having a linear memory layout.  Finally a single slice or array can be sliced and assembled into nested slices, maintaining a linear memory layout but providing access through the nested syntax.

The technique shown here defines a multi-dimensional array type.  A constructor allocates a single slice as a flattened representation.  This will be a single linear memory allocation.  Methods access the slice in row-major order.

```go
package main

import "fmt"

type md struct {
    dim []int
    ele []float64
}

func newMD(dim ...int) *md {
    n := 1
    for _, d := range dim {
        n *= d
    }
    return &md{append([]int{}, dim...), make([]float64, n)}
}

func (m *md) index(i ...int) (x int) {
    for d, dx := range m.dim {
        x = x*dx + i[d]
    }
    return
}

func (m *md) at(i ...int) float64 {
    return m.ele[m.index(i...)]
}

func (m *md) set(x float64, i ...int) {
    m.ele[m.index(i...)] = x
}

func (m *md) show(i ...int) {
    fmt.Printf("m%d = %g\n", i, m.at(i...))
}

func main() {
    m := newMD(5, 4, 3, 2)
    m.show(4, 3, 2, 1)
    m.set(87, 4, 3, 2, 1)
    m.show(4, 3, 2, 1)

    for i := 0; i < m.dim[0]; i++ {
        for j := 0; j < m.dim[1]; j++ {
            for k := 0; k < m.dim[2]; k++ {
                for l := 0; l < m.dim[3]; l++ {
                    x := m.index(i, j, k, l)
                    m.set(float64(x)+.1, i, j, k, l)
                }
            }
        }
    }
    fmt.Println(m.ele[:10])
    fmt.Println(m.ele[len(m.ele)-10:])
    m.show(4, 3, 2, 1)
}
```

```txt
m[4 3 2 1] = 0
m[4 3 2 1] = 87
[0.1 1.1 2.1 3.1 4.1 5.1 6.1 7.1 8.1 9.1]
[110.1 111.1 112.1 113.1 114.1 115.1 116.1 117.1 118.1 119.1]
m[4 3 2 1] = 119.1
```



## J

J supports multi-dimensional arrays, and provides a variety of ways of creating and working with them.

Perhaps the simplest mechanism to create a multidimensional array is to reshape a smaller dimensioned array with the desired dimensions:


```J
A1=:5 4 3 2$0
```

This creates an array of 120 zeros arranged in contiguous memory.

Note that items along the leading dimension of the array being reshaped are repeated as necessary to fill in all the values of the array. Thus, this is equivalent:

```J
A2=:5 4 $ 1 3 2$ 0
```

Another candidate for the simplest mechanism to create a multidimensional array is to ask for an array of indices with those dimensions:

```J
   i.2 3 4 5
  0   1   2   3   4
  5   6   7   8   9
 10  11  12  13  14
 15  16  17  18  19

 20  21  22  23  24
 25  26  27  28  29
 30  31  32  33  34
 35  36  37  38  39

 40  41  42  43  44
 45  46  47  48  49
 50  51  52  53  54
 55  56  57  58  59


 60  61  62  63  64
 65  66  67  68  69
 70  71  72  73  74
 75  76  77  78  79

 80  81  82  83  84
 85  86  87  88  89
 90  91  92  93  94
 95  96  97  98  99

100 101 102 103 104
105 106 107 108 109
110 111 112 113 114
115 116 117 118 119
```


Note that these indices are not multi-dimensional indices but simple counting numbers. (Internally, values in the array are arranged contiguously and in this index order.) To obtain the corresponding index, you can use J's antibase verb. For example:


```J
   2 3 4 5#:118
1 2 3 3
```


Normally, in J, you operate on "everything at once", which can be challenging if you have not worked in a language (such as sql or any of a variety of others) which encourages that kind of thinking. Nevertheless, here's some introductory "one at a time" operations:

Pulling a single value from an array:

```J
   (<1 2 3 3) { i.2 3 4 5
118
```

Setting a single value in an array:

```J
A3=: 987 (<1 2 3 3)} i. 2 3 4 5
```

And, to reduce vertical space used in this task, here's an example of extracting a sub-array:

```J
   (<1 2){A3
100 101 102 103 104
105 106 107 108 109
110 111 112 113 114
115 116 117 987 119
```


Note that bounds are checked when indexing from the array, or when combining with another array.

```J
   (<5 5 5 5){A3
|index error
```

and

```J
   A1+A3
|length error
```


For more introductory material on multi-dimensional arrays, you might be interested in [http://www.jsoftware.com/help/learning/05.htm chapter 5] and [http://www.jsoftware.com/help/learning/07.htm chapter 7] of the Learning J book.


## Java

Same description for limitations of the JVM apply here as is described in the Kotlin implementation of the task.

```Java
public class MultiDimensionalArray {
    public static void main(String[] args) {
        // create a regular 4 dimensional array and initialize successive elements to the values 1 to 120
        int m = 1;
        int[][][][] a4 = new int[5][4][3][2];
        for (int i = 0; i < a4.length; ++i) {
            for (int j = 0; j < a4[0].length; ++j) {
                for (int k = 0; k < a4[0][0].length; ++k) {
                    for (int l = 0; l < a4[0][0][0].length; ++l) {
                        a4[i][j][k][l] = m++;
                    }
                }
            }
        }

        System.out.println("First element = " + a4[0][0][0][0]);  // access and print value of first element
        a4[0][0][0][0] = 121;                                     // change value of first element
        System.out.println();

        for (int i = 0; i < a4.length; ++i) {
            for (int j = 0; j < a4[0].length; ++j) {
                for (int k = 0; k < a4[0][0].length; ++k) {
                    for (int l = 0; l < a4[0][0][0].length; ++l) {
                        System.out.printf("%4d", a4[i][j][k][l]);
                    }
                }
            }
        }
    }
}
```

```txt
First element = 1

 121   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
```



## Javascript

Javascript does not natively support multi-dimensional arrays.

But, as it does support one dimensional arrays, we can use them to construct multi-dimensional arrays.

One approach pairs an array of dimensions with an array of values.

```javascript
function array() {
	var dimensions= Array.prototype.slice.call(arguments);
	var N=1, rank= dimensions.length;
	for (var j= 0; j<rank; j++) N*= dimensions[j];
	this.dimensions= dimensions;
	this.values= new Array(N);
}
```


A routine to convert between single dimension indices and multi-dimensional indices can be useful here:


```javascript
function tobase(base, vals) {
  var r= 0, len= base.length;
  for (j= 0; j < len; j++) {
    r*= base[j];
    r+= vals[j];
  }
  return r;
}

function frombase(base, val) {
  var r= new Array(base.length);
  for (j= base.length-1; j>= 0; j--) {
    r[j]= val%base[j];
    val= (val-r[j])/base[j];
  }
  return r;
}
```

For example, array indexing in this approach might be implemented something like:


```javascript

array.prototype.index= function() {
	var indices= Array.prototype.slice.call(arguments);
	return this.values[tobase(this.dimensions, indices)];
}

```

This would allow things like:

```javascript
a= new array(6,7,8,9);
a.index(2,3,5,6);
```


But a proper implementation of the index method should include array bounds checking.

That said, note that operating on the entire array at once avoids the need for any index calculations.

Another approach "nests" arrays inside other arrays:

```javascript

function array(length) {
	var rest= Array.prototype.slice.call(arguments);
	var r= new Array(length);
	if (0<rest.length) {
		for (var j= 0; j<length; j++) {
			r[j]= array.apply(rest);
		}
	}
}
```

This approach is superficially simpler but may turn out to be considerably slower.

That said, with this approach indexing can be performed directly - without implementing any index function. For example a[2][3][4][5], and this convenience may override any concerns about performance.


## jq

jq only supports one-dimensional arrays natively, but these can be
used to represent multi-dimensional arrays since a JSON array can
contain elements of any type.  Furthermore, jq has builtin functions
which are well-suited for updating and accessing the elements in
multi-dimensional arrays.  As illustrated below, these are
getpath(d) and setpath(d; value), where d is an array specifying the indices of a
particular element.

'''Preliminaries'''

(1) The index origin for jq arrays is 0.

(2) All values in jq are immutable, but there are element-wise
operators that in effect modify an array at a particular index; for
example, if ary is an array, and if i is an index into ary, then the
expression 'ary | .[i] = v' in effect sets ary[i] to v, though in fact
it returns a copy of ary with the i-th element set to v.

(3) If ary is an array, then ary[i:j] is the array [a[i], ... a[j-1]], assuming
0<=i<j<=length.  Other notational conveniences are supported, e.g. in
jq 1.5, ary[-i] can be used to refer to ary[length-i].

(4) There is nothing in jq itself to force an array of arrays to be a
"multi-dimensional array" in the sense that it is strictly
"rectangular".  A function for checking whether a JSON array is in
fact a multi-dimensional array in the sense of this article is
presented below.

'''Array Creation'''
To create a one-dimensional array of n nulls, one may write [][n] = null.

To create a multi-dimensional array with dimensions specified by an
array, d, we can define a recursive function as follows:

```jq
# The input is used to initialize the elements of the
# multi-dimensional array:
def multiarray(d):
  . as $in
  | if (d|length) == 1 then [range(0;d[0]) | $in]
    else multiarray(d[1:]) | multiarray( d[0:1] )
    end;
```


A four-dimensional array as specified by the task description can now be created as follows:

0 | multiarray( [5, 4, 3, 2] )

For convenience of reference, let us name this array by wrapping it in
a function:

```jq
def ary: 0 | multiarray( [5, 4, 3, 2] );
```


'''Access and Update'''
To access the [4,3,2,1] element of the previously defined multi-dimensional array, one can either write:

```jq
ary | .[4][3][2][1]
```

or

```jq
ary | getpath( [4,3,2,1])
```


To illustrate, let us define an array of indices:

```jq
def ix: [4,3,2,1];
```


To check that the two approaches for accessing an element are equivalent:

```jq
ary | setpath(ix; 100) | getpath(ix)
#=> 100

ary | setpath(ix; 100) | .[4][3][2][1]
#=> 100
```


'''Ascertaining the Dimensions of a Multi-dimensional Array'''

The following function returns the dimensions of the input if it is a
regular multi-dimensional array; it returns [] for a scalar, and null
for an irregularly shaped array:

```jq
def dimensions:
  def same(f):
    if length == 0 then true
    else (.[0]|f) as $first | reduce .[] as $i (true; if . then ($i|f) == $first else . end)
    end;

  if type == "array"
  then if length == 0 then [0]
       elif same( dimensions ) then [length] + (.[0]|dimensions)
       else null
       end
  else []
  end;
```


For exampe:

```jq
ary | dimensions
#=> [5,4,3,2]
```



## Julia

Julia supports multi-dimensional arrays in its syntax and usual implementation. Multidimensional arrays in Julia are stored in column-major order. This means that arrays are stacked one column at a time, in contiguous memory so storage is optimized. Bounds on array accesses are checked by default, though this can be disabled with a macro pragma if desired for speed.
<br /><br />
Demonstrated below, from a Julia REPL session, using a random integer four-dimensional array:

```julia

julia> a4d = rand(Int, 5, 4, 3, 2)
5×4×3×2 Array{Int64,4}:
[:, :, 1, 1] =
 -4071410509370082480   -361121233222804742   6884099527706004770  -3207257047234122321
  8613938183523990915  -6284413064884272355   2092274063225757339   2883192477194384468
  8063489472918692884   7817079491035828713   4371491775271354348    276862286105940437
 -1772662642291072402   4809985701129914416   8281858591045636672   8406920023376702651
  5149318914785292575  -1515076735924485336  -3446939844768424035   1325012392213218275

[:, :, 2, 1] =
  6880114761106392524   6088903043766771621   3427296371455723937  -3871156425725741320
 -9056731070343072262  -6900689642579036552   6428097039068264082  -8965404397337530418
 -1972367399165031500  -2213273885576423725  -5169665910043775523   4625836493333597033
 -2599106204536686594   8110110151332124377   2213007499408257641  -8439992945948333993
 -5582801554832553928   7198533994867969144  -5086532281938936871  -5785587643130395544

[:, :, 3, 1] =
   -17294958981345227  -3894742251505912349   5910938929594406050  -1362996293154965701
  3987219021607448038   6324367415515825846   3745581879541731998   2844758786713062315
 -6091449020940608221   4456121461951632195  -2584728255467516797  -3497659495227813242
 -5928873932509420551  -3918487907573141316   5830965509944713914   8236501134345492283
 -8221039525025311485  -1377489816166018715   3331466694873878620  -6251825104964406539

[:, :, 1, 2] =
 -3457374235750853577   5435555757951424162  -2045941319469405317   7328458353379957791
  1713055171323579940   7991986037097235116  -7241088987591638401   1660634030535548686
  4673394669827656364   5361350944786403116  -3165400775730699962  -2786870864776919360
  -936605780936708362  -8663743677584705168   2854140834792323092  -8335310793071345837
 -3681644266158007140   -725263380984390472  -3882196050441743539   3104333567303051945

[:, :, 2, 2] =
  6571168566735939900     -2962119128748096  -6995171731724009568  -3311757615633004375
 -2337587211780934167  -4326286389873661148   4350714846343974202   7735721289399158250
 -8213453299747381553  -8821356424402127712  -6240807297203610060  -4705744555934991961
  5320110310888142259   -767643622294485330  -3059460131766056283  -7562911883833519677
 -7578327835336665804  -1125154035165505958   7801099686760373595  -7212481716316687824

[:, :, 3, 2] =
  4758459841719113041  -2608915613406792656  -4400228941420712011   8931157241145543293
  7349481815101847836  -1609425280933983575  -4442082290554782826  -7044337833155803104
 -4991211814494456720   6358355341301107435  -7486441622913196485  -1654445042306345503
 -2789599665862904090   2753632931032283690  -1580743162155175963  -5070035295183713618
  6026427076366641381  -2363816652576128818  -7282095369321808360   9097339410999816372

julia> a4d[1,1,1,1] = 10
10

julia> a4d
5×4×3×2 Array{Int64,4}:
[:, :, 1, 1] =
                   10   -361121233222804742   6884099527706004770  -3207257047234122321
  8613938183523990915  -6284413064884272355   2092274063225757339   2883192477194384468
  8063489472918692884   7817079491035828713   4371491775271354348    276862286105940437
 -1772662642291072402   4809985701129914416   8281858591045636672   8406920023376702651
  5149318914785292575  -1515076735924485336  -3446939844768424035   1325012392213218275

[:, :, 2, 1] =
  6880114761106392524   6088903043766771621   3427296371455723937  -3871156425725741320
 -9056731070343072262  -6900689642579036552   6428097039068264082  -8965404397337530418
 -1972367399165031500  -2213273885576423725  -5169665910043775523   4625836493333597033
 -2599106204536686594   8110110151332124377   2213007499408257641  -8439992945948333993
 -5582801554832553928   7198533994867969144  -5086532281938936871  -5785587643130395544

[:, :, 3, 1] =
   -17294958981345227  -3894742251505912349   5910938929594406050  -1362996293154965701
  3987219021607448038   6324367415515825846   3745581879541731998   2844758786713062315
 -6091449020940608221   4456121461951632195  -2584728255467516797  -3497659495227813242
 -5928873932509420551  -3918487907573141316   5830965509944713914   8236501134345492283
 -8221039525025311485  -1377489816166018715   3331466694873878620  -6251825104964406539

[:, :, 1, 2] =
 -3457374235750853577   5435555757951424162  -2045941319469405317   7328458353379957791
  1713055171323579940   7991986037097235116  -7241088987591638401   1660634030535548686
  4673394669827656364   5361350944786403116  -3165400775730699962  -2786870864776919360
  -936605780936708362  -8663743677584705168   2854140834792323092  -8335310793071345837
 -3681644266158007140   -725263380984390472  -3882196050441743539   3104333567303051945

[:, :, 2, 2] =
  6571168566735939900     -2962119128748096  -6995171731724009568  -3311757615633004375
 -2337587211780934167  -4326286389873661148   4350714846343974202   7735721289399158250
 -8213453299747381553  -8821356424402127712  -6240807297203610060  -4705744555934991961
  5320110310888142259   -767643622294485330  -3059460131766056283  -7562911883833519677
 -7578327835336665804  -1125154035165505958   7801099686760373595  -7212481716316687824

[:, :, 3, 2] =
  4758459841719113041  -2608915613406792656  -4400228941420712011   8931157241145543293
  7349481815101847836  -1609425280933983575  -4442082290554782826  -7044337833155803104
 -4991211814494456720   6358355341301107435  -7486441622913196485  -1654445042306345503
 -2789599665862904090   2753632931032283690  -1580743162155175963  -5070035295183713618
  6026427076366641381  -2363816652576128818  -7282095369321808360   9097339410999816372

julia> a4d[4,3,2,1]
2213007499408257641

```



## Kotlin

Stricly speaking Kotlin only supports single dimensional arrays but multi-dimensional arrays of any dimension can be easily built up as 'arrays of arrays' and individual elements can then be set or accessed by successively applying the indexation operator [].

This means, of course, that multi-dimensional array storage is not contiguous. However, it also means that arrays can be 'jagged' i.e. sub-arrays do not necessarily need to have the same size and that it easy to replace one sub-array with another.

Arrays in Kotlin always start from an index of zero and, in the version which targets the JVM, bounds are checked automatically.

```scala
// version 1.1.2

fun main(args: Array<String>) {
    // create a regular 4 dimensional array and initialize successive elements to the values 1 to 120
    var m = 1
    val a4 = Array<Array<Array<Array<Int>>>>(5) {
        Array<Array<Array<Int>>>(4) {
            Array<Array<Int>>(3) {
                Array<Int>(2) { m++ }
            }
        }
    }

    println("First element = ${a4[0][0][0][0]}")  // access and print value of first element
    a4[0][0][0][0] = 121                          // change value of first element
    println()

    // access and print values of all elements
    val f = "%4d"
    for (i in 0..4)
        for (j in 0..3)
            for (k in 0..2)
                for (l in 0..1)
                    print(f.format(a4[i][j][k][l]))

}
```

```txt
First element = 1

 121   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
```



## Lingo

Lingo has no special multi-dimensional array support, but such arrays can be implemented based on nested lists.<br />
(Note: code partially stolen from Lua solution).

```Lingo
on array ()
    cnt = paramCount()
    if cnt=0 then return
    arr = []
    arr[param(cnt)] = 0
    repeat with d = cnt-1 down to 1
        newArr = []
        repeat with i = 1 to param(d)
            newArr[i] = arr.duplicate() -- duplicate does a deep copy
        end repeat
        arr = newArr.duplicate()
    end repeat
    return arr
end
```



```Lingo
-- creates 4-dimensional array with specified ranges (data defaults to 0)
a = array(2, 3, 4, 5)
put a[1][2][3][4]
-- 0
a[1][2][3][4] = 23
put a[1][2][3][4]
-- 23
a[1][2][3][4] = 42
put a[1][2][3][4]
-- 42
```



## Lua

Lua does not support straightforward declaration of multidimensional arrrays but they can be created in the form of nested tables.  Once such a structure has been created, elements can be read and written in a manner that will be familiar to users of C-like languages.

```Lua
-- Variadic, first argument is the value with which to populate the array.
function multiArray (initVal, ...)
    local function copy (t)
        local new = {}
        for k, v in pairs(t) do
            if type(v) == "table" then
                new[k] = copy(v)
            else
                new[k] = v
            end
        end
        return new
    end
    local dimensions, arr, newArr = {...}, {}
    for i = 1, dimensions[#dimensions] do table.insert(arr, initVal) end
    for d = #dimensions - 1, 1, -1 do
        newArr = {}
        for i = 1, dimensions[d] do table.insert(newArr, copy(arr)) end
        arr = copy(newArr)
    end
    return arr
end

-- Function to print out the specific example created here
function show4dArray (a)
    print("\nPrinting 4D array in 2D...")
    for k, v in ipairs(a) do
        print(k)
        for l, w in ipairs(v) do
            print("\t" .. l)
                for m, x in ipairs(w) do
                    print("\t", m, unpack(x))
                end
        end
    end
end

-- Main procedure
local t = multiArray("a", 2, 3, 4, 5)
show4dArray(t)
t[1][1][1][1] = true
show4dArray(t)
```

```txt
Printing 4D array in 2D...
1
        1
                1       a       a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a
        2
                1       a       a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a
        3
                1       a       a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a
2
        1
                1       a       a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a
        2
                1       a       a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a
        3
                1       a       a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a

Printing 4D array in 2D...
1
        1
                1       true    a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a
        2
                1       a       a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a
        3
                1       a       a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a
2
        1
                1       a       a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a
        2
                1       a       a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a
        3
                1       a       a       a       a       a
                2       a       a       a       a       a
                3       a       a       a       a       a
                4       a       a       a       a       a
```

This is not memory-efficient and any advanced operations will have to be written if required. A much easier solution is to use an extension module such as densearray available from luarocks.org.


## Perl


```perl
use feature 'say';

# Perl arrays are internally always one-dimensional, but multi-dimension arrays are supported via references.
# So a two-dimensional array is an arrays-of-arrays, (with 'rows' that are references to arrays), while a
# three-dimensional array is an array of arrays-of-arrays, and so on. There are no arbitrary limits on the
# sizes or number of dimensions (i.e. the 'depth' of nesting references).

# To generate a zero-initialized 2x3x4x5 array
for $i (0..1) {
  for $j (0..2) {
    for $k (0..3) {
      $a[$i][$j][$k][$l] = [(0)x5];
    }
  }
}

# There is no requirement that the overall shape of array be regular, or that contents of
# the array elements be of the the same type.  Arrays can contain almost any type of values
@b = (
     [1, 2, 4, 8, 16, 32],                                                      # numbers
     [<Mon Tue Wed Thu Fri Sat Sun>],                                           # strings
     [sub{$_[0]+$_[1]}, sub{$_[0]-$_[1]}, sub{$_[0]*$_[1]}, sub{$_[0]/$_[1]}]   # coderefs
);
say $b[0][5];           # prints '32'
say $b[1][2];           # prints 'Wed'
say $b[2][0]->(40,2);   # prints '42', sum of 40 and 2

# Pre-allocation is possible, can result in a more efficient memory layout
# (in general though Perl allows minimal control over memory)
$#$big = 1_000_000;

# But dimensions do not need to be pre-declared or pre-allocated.
# Perl will auto-vivify the necessary storage slots on first access.
$c[2][2] = 42;
# @c =
#    [undef]
#    [undef]
#    [undef, undef, 42]

# Negative indicies to count backwards from the end of each dimension
say $c[-1][-1]; # prints '42'

# Elements of an array can be set one-at-a-time or in groups via slices
my @d = <Mon Tue Ned Sat Fri Thu>;
push @d, 'Sun';
$d[2] = 'Wed';
@d[3..5] = @d[reverse 3..5];
say "@d"; # prints 'Mon Tue Wed Thu Fri Sat Sun'
```



## Perl 6


```perl6
#Perl 6 supports multi dimension arrays natively. There are no arbitrary limits on the number of dimensions or maximum indices. Theoretically, you could have an infinite number of dimensions of infinite length, though in practice more than a few dozen dimensions gets unwieldy. An infinite maximum index is a fairly common idiom though. You can assign an infinite lazy list to an array and it will only produce the values when they are accessed.

my @integers = 1 .. Inf; # an infinite array containing all positive integers

say @integers[100000]; #100001 (arrays are zero indexed.)

# Multi dimension arrays may be predeclared which constrains the indices to the declared size:

my @dim5[3,3,3,3,3];

#Creates a preallocated 5 dimensional array where each branch has 3 storage slots and constrains the size to the declared size.
#It can then be accessed like so:

@dim5[0;1;2;1;0] = 'Perl 6';

say @dim5[0;1;2;1;0]; # Perl 6

#@dim5[0;1;2;1;4] = 'error'; # runtime error: Index 4 for dimension 5 out of range (must be 0..2)

# Note that the dimensions do not _need_ to be predeclared / allocated. Perl 6 will auto-vivify the necessary storage slots on first access.

my @a2;

@a2[0;1;2;1;0] = 'Perl 6';

@a2[0;1;2;1;4] = 'not an error';

# It is easy to access array "slices" in Perl 6.

my @b = map { [$_ X~ 1..5] }, <a b c d>;

.say for @b;
# [a1 a2 a3 a4 a5]
# [b1 b2 b3 b4 b5]
# [c1 c2 c3 c4 c5]
# [d1 d2 d3 d4 d5]

say @b[*;2]; # Get the all of the values in the third "column"
# (a3 b3 c3 d3)

# By default, Perl 6 can store any object in an array, and it is not very compact. You can constrain the type of values that may be stored which can allow the optimizer to store them much more efficiently.

my @c = 1 .. 10; # Stores integers, but not very compactly since there are no constraints on what the values _may_ be

my uint16 @d = 1 .. 10 # Since there are and only can be unsigned 16 bit integers, the optimizer will use a much more compact memory layout.

# Indices must be a positive integer. Negative indices are not permitted, fractional indices will be truncated to an integer.
```



## Phix

Multi-dimensional arrays (sequences in Phix parlance) are not just supported, but ''the'' central and most important
construct of the entire language.

(Quite often the phrase "nested sub-sequences" is specifically used to refer to multi-dimensional capabilities.)

Arrays are allocated piecemeal from the heap, so storage is closer to row-major order than column-major order, with
integers allocated contiguously at the lowest level.

Arrays are not limited by type; they can contain any mixture of integers, floating point numbers, strings, or nested
sub-sequences, and can easily be shrunk or expanded, at any level, at any time.

```Phix
sequence xyz = repeat(repeat(repeat(0,x),y),z)
```

or if you prefer:

```Phix
sequence line = repeat(0,x)
sequence plane = repeat(line,y)
sequence space = repeat(plane,z)
```

Internally Phix uses reference counting with copy-on-write semantics, so the initial space allocated is x+y+z, and as
more and more individual points are populated the total space required gradually increases to x*y*z.

Following the above initialisation, a specific point would be referenced as space[z][y][x] (and obviously space[x][y][z] is perfectly possible with a different initialisation, as long as you accept that two logically adjacent x will be far from physically adjacent).

Note that all indexes are 1-based. You can also use negative indexes to count backwards from the end of the sequence.

```Phix
sequence rqd = repeat(repeat(repeat(repeat(0,2),3),4),5),
         row -- scratch var

    rqd[5][4][3][2] := 5432
    ?rqd[5][4][3][2]

    row = rqd[5][4][3]  -- row is now [0,5432]
    row[1] = 1          -- rqd remains unchanged
    rqd[5][4][3] = row  -- rqd[5][4][3][1] is now 1
    rqd[5][4][2] = repeat(0,10) -- middle element of rqd[5][4] is now longer than the other two
    ?rqd[5][4]
```

```txt
5432
```

Array bounds are automatically checked on all forms of subscripting. Slices can also be referenced, eg
rqd[2..3] or rqd[5][4][1..3]. A slice cannot be followed by further subscripts.
Use the following, experimenting with the nesting value, to quickly dump (smallish) nested sequences in
an easily readable fashion:

```Phix
?"==1=="
    pp(rqd,{pp_Nest,1})
?"==2=="
    pp(rqd,{pp_Nest,2})
?"==3=="
    pp(rqd,{pp_Nest,3})
```

```txt
"==1=="
{{{{0,0}, {0,0}, {0,0}}, {{0,0}, {0,0}, {0,0}}, {{0,0}, {0,0}, {0,0}},
  {{0,0}, {0,0}, {0,0}}},
 ....
 {{{0,0}, {0,0}, {0,0}}, {{0,0}, {0,0}, {0,0}}, {{0,0}, {0,0}, {0,0}},
  {{0,0}, {0,0,0,0,0,0,0,0,0,0}, {1,5432}}}}
"==2=="
{{{{0,0}, {0,0}, {0,0}},
  {{0,0}, {0,0}, {0,0}},
  {{0,0}, {0,0}, {0,0}},
  {{0,0}, {0,0}, {0,0}}},
 ....
 {{{0,0}, {0,0}, {0,0}},
  {{0,0}, {0,0}, {0,0}},
  {{0,0}, {0,0}, {0,0}},
  {{0,0}, {0,0,0,0,0,0,0,0,0,0}, {1,5432}}}}
"==3=="
{{{{0,0},
   {0,0},
   {0,0}},
  ...
  {{0,0},
   {0,0},
   {0,0}}},
 ...
 {{{0,0},
   {0,0},
   {0,0}},
  ...
  {{0,0},
   {0,0,0,0,0,0,0,0,0,0},
   {1,5432}}}}
```



## Python

===Python: In-built===
Python has ''syntax'' (and hidden) support for the access of multi-dimensional arrays, but no in-built datatype that supports it.

A common method of simulating multi-dimensional arrays is to use dicts with N-element tuples as keys.

Function <code>dict_as_mdarray</code> allows for the creation of an initialised multi-dimensional array of a given size. Note how indexing must use the round brackets of a tuple inside the square brackets of normal dict indexing:

```python>>>
 from pprint import pprint as pp   # Pretty printer
>>> from itertools import product
>>>
>>> def dict_as_mdarray(dimensions=(2, 3), init=0.0):
...     return {indices: init for indices in product(*(range(i) for i in dimensions))}
...
>>>
>>> mdarray = dict_as_mdarray((2, 3, 4, 5))
>>> pp(mdarray)
{(0, 0, 0, 0): 0.0,
 (0, 0, 0, 1): 0.0,
 (0, 0, 0, 2): 0.0,
 (0, 0, 0, 3): 0.0,
 (0, 0, 0, 4): 0.0,
 (0, 0, 1, 0): 0.0,
...
 (1, 2, 3, 0): 0.0,
 (1, 2, 3, 1): 0.0,
 (1, 2, 3, 2): 0.0,
 (1, 2, 3, 3): 0.0,
 (1, 2, 3, 4): 0.0}
>>> mdarray[(0, 1, 2, 3)]
0.0
>>> mdarray[(0, 1, 2, 3)] = 6.78
>>> mdarray[(0, 1, 2, 3)]
6.78
>>> mdarray[(0, 1, 2, 3)] = 5.4321
>>> mdarray[(0, 1, 2, 3)]
5.4321
>>> pp(mdarray)
{(0, 0, 0, 0): 0.0,
 (0, 0, 0, 1): 0.0,
 (0, 0, 0, 2): 0.0,
...
 (0, 1, 2, 2): 0.0,
 (0, 1, 2, 3): 5.4321,
 (0, 1, 2, 4): 0.0,
...
 (1, 2, 3, 3): 0.0,
 (1, 2, 3, 4): 0.0}
>>>
```



### Python: numpy library

Python has the widely available [http://www.numpy.org numpy] library for array specific operations. It creates numpy array types that take full advantage of Python's syntax support for [http://wiki.scipy.org/Tentative_NumPy_Tutorial#head-d3f8e5fe9b903f3c3b2a5c0dfceb60d71602cf93 multi-dimensional arrays].

Numpy arrays contain values of a single type arranged in a contiguous block of memory that can be further arranged to be compatible with C language or Fortran array layouts to aid the use of C and Fortran libraries.

The default is row-major order (compatible with C). When dealing with Fortran storage order, care must be taken in order to get the same behavious as in Fortran:

```python
import numpy as np
a = np.array([[1, 2], [3, 4]], order="C")
b = np.array([[1, 2], [3, 4]], order="F")
np.reshape(a, (4,))             # [1, 2, 3, 4]
np.reshape(b, (4,))             # [1, 2, 3, 4]
np.reshape(b, (4,), order="A")  # [1, 3, 2, 4]
```


In Fortran, one would expect the result [1, 3, 2, 4].

```python>>>
 from numpy import *
>>>
>>> mdarray = zeros((2, 3, 4, 5), dtype=int8, order='F')

>>> mdarray
array([[[[0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0]],

        [[0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0]],

        [[0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0]]],


       [[[0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0]],

        [[0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0]],

        [[0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0],
         [0, 0, 0, 0, 0]]]], dtype=int8)
>>> mdarray[0, 1, 2, 3]
0
>>> mdarray[0, 1, 2, 3] = 123
>>> mdarray[0, 1, 2, 3]
123
>>> mdarray[0, 1, 2, 3] = 666
>>> mdarray[0, 1, 2, 3]
-102
>>> mdarray[0, 1, 2, 3] = 255
>>> mdarray[0, 1, 2, 3]
-1
>>> mdarray[0, 1, 2, 3] = -128
>>> mdarray[0, 1, 2, 3]
-128
>>> mdarray
array([[[[   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0]],

        [[   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0],
         [   0,    0,    0, -128,    0],
         [   0,    0,    0,    0,    0]],

        [[   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0]]],


       [[[   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0]],

        [[   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0]],

        [[   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0],
         [   0,    0,    0,    0,    0]]]], dtype=int8)
>>>
```



## Racket


Racket has [http://docs.racket-lang.org/math/array.html multi-dimensional arrays] as part of the standard <tt>math</tt> library.  Instead of repeating the whole thing here, see the [http://docs.racket-lang.org/math/array_quick.html quick start] page of the documentation, which describes all of what's asked here.


## REXX

REXX supports multi-dimension (stemmed) arrays and the limit of the number of dimension varies with individual REXX
interpreters,   ''most'' (but not all)   ''will probably be''   limited by the largest allowable clause
length or source-line length, the <u>smallest</u> of which is '''250''' characters   (included the periods),   which
would be around '''125''' dimensions.   However, most modern REXXes have at least a clause length of '''4,095''' characters.

The REXX language has something called   '''stemmed arrays'''.

For instance, for a stemmed array named   '''antenna'''   with two dimensions,   to reference the
element   '''2, 0'''   (and set that value to the variable   '''g'''),   one could code   (note
the use of periods after the variable name and also between the indices to the array dimensions:

```rexx>g = antenna.2.0</lang

Memory allocation for stemmed arrays is not optimized, and the array elements are not contiguous.

The index to the array dimensions may be any integer, and indeed, even non-numeric   (and possible non-viewable).

```rexx
    a = '"'                    /*set variable  A  to a quote character  ["].  */
    b = '~'                    /*set variable  B  to a tilde character  [~].  */

h.a.b = '+++'                  /*set variable  H.".~  to three pluses  [+++]. */
```

The REXX language does not do bounds checking, but if     '''signal on novalue'''     is in effect, it can be used to trap (read) references of array elements that haven't been assigned a value (at the time of the read reference).

```rexx
/*REXX program shows how to  assign and/or display  values of a multi─dimensional array.*/
                                                 /*REXX arrays can start anywhere.      */
y.=0                                             /*set all values of   Y   array to  0. */
                                                 /* [↑]  bounds need not be specified.  */
#=0                                              /*the count for the number of   SAYs.  */
y.4.3.2.0= 3**7                                  /*set penultimate element to   2187    */
                      do       i=0  for 5
                        do     j=0  for 4
                          do   k=0  for 3
                            do m=0  for 2;   #=#+1             /*bump the  SAY  counter.*/
/*the 1st SAY──► */         say  'y.'i"."j'.'k"."m   '='   y.i.j.k.m
                            end   /*m*/
                          end     /*k*/
                        end       /*j*/
                      end         /*i*/
say
say '# of elements displayed = '  #              /*should be   5 * 4 * 3 * 2    or   5! */
exit                                             /*stick a fork in it,  we're all done. */

                   /* [↓]   other versions of the first (REXX)   SAY   instruction. */
                      say  'y.' || i || . || k || . || m  '='  y.i.j.k.m
                      say  'y.'||i||.||k||.||m            '='  y.i.j.k.m
                      say  'y.'i||.||k||.||m              '='  y.i.j.k.m
```

```txt
y.0.0.0.0 = 0
y.0.0.0.1 = 0
y.0.0.1.0 = 0
y.0.0.1.1 = 0
y.0.0.2.0 = 0
y.0.0.2.1 = 0
y.0.1.0.0 = 0
y.0.1.0.1 = 0
  ·
  ·
  ·
y.4.2.2.1 = 0
y.4.3.0.0 = 0
y.4.3.0.1 = 0
y.4.3.1.0 = 0
y.4.3.1.1 = 0
y.4.3.2.0 = 2187
y.4.3.2.1 = 0

# of elements displayed =  120
```



## Ring


```ring
# Project : Multi-dimensional array

a4 = newlist4(5,4,3,2)

func main()
        m = 1
        for i = 1 to 5
             for j = 1 to 4
                  for k = 1 to 3
                        for l = 1 to 2
                             a4[i][j][k][l] = m
                             m = m + 1
                        next
                  next
             next
       next
       see "First element = " + a4[1][1][1][1] + nl
       a4[1][1][1][1] = 121
       see nl
       for i = 1 to 5
            for j = 1 to 4
                 for k = 1 to 3
                       for l = 1 to 2
                            see "" + a4[i][j][k][l] + " "
                       next
                 next
            next
      next

func newlist(x, y)
       if isstring(x) x=0+x ok
       if isstring(y) y=0+y ok
       alist = list(x)
       for t in alist
            t = list(y)
       next
       return alist

func newlist3(x, y, z)
       if isstring(x) x=0+x ok
       if isstring(y) y=0+y ok
       if isstring(z) z=0+z ok
       alist = list(x)
       for t in alist
            t = newlist(y,z)
       next
       return alist

func newlist4(x, y, z, w)
       if isstring(x) x=0+x ok
       if isstring(y) y=0+y ok
       if isstring(z) z=0+z ok
       if isstring(w) w=0+w ok
       alist = list(x)
       for t in alist
            t = newlist3(y,z,w)
       next
       return alist
```

Output:

```txt
First element = 1

121 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
```



## Scala

===All target-machines===
Scala basically supports single linear arrays but multi-dimensional arrays of any dimension can be easily built up as nested arrays. Individual elements can then be set or accessed by successively applying the indexation and apply operator ().

[https://docs.scala-lang.org/overviews/collections/arrays.html Scala arrays] are considered "last citizens" and are evil because their mutability and side effects. However much effort is done to implement them with the common collections  functions (e.g. head, tail, last, map, flatten etc) in the Scala language to maintain [https://en.wikipedia.org/wiki/Orthogonality_(programming) orthogonality in collections].

Their presents indicates low level engineering. Arrays are more machine-oriented (lineair memory) than problem-oriented (thinking in e.g. collections, sets).

Arrays in Scala always start (of course as the great Dijkstra in his [https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html EWD831] pointed out) from an index of zero and, insofar the target machine can handle, bounds can be checked automatically e.g. by the JVM (throwing an exception) or ES aka JavaScript (resulting "undefined").

```Scala
object MultiDimensionalArray extends App {

  // Create a regular 4 dimensional array and initialize successive elements to the values 1 to 120
  val a4 = Array.fill[Int](5, 4, 3, 2) { m += 1; m }
  var m = 0

  println("First element = " + a4(0)(0)(0)(0)) // access and print value of first element
  println("Last element  = " + a4.last.last.last.last)
  a4(0)(0)(0)(0) = 121 // change value of first element

  println(a4.flatten.flatten.flatten.mkString(", "))

}
```

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/Lx6AG4S/0 (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/MF2p1z1fReyhjGcu4aY4Eg Scastie (remote JVM)].


## Tcl

In Tcl, [http://www.tcl.tk/man/tcl/TclCmd/array.htm arrays] are associative maps and [http://wiki.tcl.tk/440 lists] are closer to what other languages name "arrays".
Either can be used for multidimensional data, but the implementations (and implications!) are quite different.

It's worth briefly discussing both here.  Since lists are closer to the theme of this page, they come first.

### lists

Multi-dimensional lists are easily handled by nesting.  Let's define a helper proc to construct such lists using lrepeat:

```Tcl
proc multilist {value args} {
    set res $value
    foreach dim [lreverse $args] {
        set res [lrepeat $dim $res]
    }
    return $res
}
```

```Tcl
% multilist x 2
x x
% multilist x 2 3
{x x x} {x x x}
% multilist x 2 3 4
```

Both lset and lindex know how to access multi-dimensional lists:

```Tcl
% set ml [multilist 0 2 3 4]
% lset ml 1 2 3 11
% lset ml 1 1 4 12
% lindex $ml 1 2 3
11
```

[http://www.tcl.tk/man/tcl/TclCmd/lsort.htm lsort] and [http://www.tcl.tk/man/tcl/TclCmd/lsearch.htm lsearch] are among other useful commands that support nested lists.

### arrays

Tcl arrays are collections of variables, not collections of values:  thus they cannot nest.  But since keys are simply strings, multidimensional data can be kept like this:

```Tcl
% array set x {
    0,0 a
    0,1 b
    1,0 c
    1,1 d
}
% parray x
x(0,0) = a
x(0,1) = b
x(1,0) = c
x(1,1) = d
% puts $x(0,1)
b
% set a 0
1
% set b 1
% puts $x($b,$a)
c
% set $x($b,$a) "not c"
not c
% parray x $b,$a
x(1,0) = not c
```

Such an array can also be "sliced" with the array command:

```Tcl
% array get x 1,*
1,0 c 1,1 d
% array names x 0,*
0,0 0,1
```

Note however that the order in which elements are returned from these operations is undefined!  The last command might return {0,0 0,1} or {0,1 0,0} depending on how its keys are hashed, which is not under the programmer's control.

Using arrays like this for ordered data is suboptimal for this reason, and because iterating over elements is cumbersome.  But it's a common technique for records:

```Tcl
% array set players {
    1,name      Fido
    1,score     0
    1,colour    green

    2,name      Scratchy
    2,score     99
    2,colour    pink
}
% foreach player {1 2} {
    puts "$players($player,name) is $players($player,colour) and has $players($player,score) points"
}
Fido is green and has 0 points
Scratchy is pink and has 99 points
%
```

The interested reader should also be aware of the difference between arrays and [http://www.tcl.tk/man/tcl/TclCmd/dict.htm dict]ionaries, and know that the latter are often preferred for record-like structures.
