+++
title = "Zhang-Suen thinning algorithm"
description = ""
date = 2019-02-21T20:43:09Z
aliases = []
[extra]
id = 16497
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "autohotkey",
  "c",
  "cpp",
  "d",
  "elena",
  "elixir",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "mathematica",
  "perl",
  "perl_6",
  "phix",
  "pl_i",
  "python",
  "racket",
  "rexx",
  "ruby",
  "sidef",
  "tcl",
  "vba",
]
+++

This is an algorithm used to thin a black and white i.e. one bit per pixel images.

For example, with an input image of:

```txt

 #################                   #############
 ##################               ################
 ###################            ##################
 ########     #######          ###################
   ######     #######         #######       ######
   ######     #######        #######
   #################         #######
   ################          #######
   #################         #######
   ######     #######        #######
   ######     #######        #######
   ######     #######         #######       ######
 ########     #######          ###################
 ########     ####### ######    ################## ######
 ########     ####### ######      ################ ######
 ########     ####### ######         ############# ######

```

It produces the thinned output:

```txt


    # ##########                       #######
     ##        #                   ####       #
     #          #                 ##
     #          #                #
     #          #                #
     #          #                #
     ############               #
     #          #               #
     #          #                #
     #          #                #
     #          #                #
     #                            ##
     #                             ############
                       ###                          ###


```


;Algorithm:
Assume black pixels are one and white pixels zero, and that the input image is a rectangular N by M array of ones and zeroes.

The algorithm operates on all black pixels P1 that can have eight neighbours.

The neighbours are, in order, arranged as:

<table border="4">
     <tr><td>   P9   </td><td>       P2   </td><td>        P3   </td></tr>
     <tr><td>   P8   </td><td><b>    P1   </b></td><td>    P4   </td></tr>
     <tr><td>   P7   </td><td>       P6   </td><td>        P5   </td></tr>
</table>


Obviously the boundary pixels of the image cannot have the full eight neighbours.

* Define <math>A(P1)</math> = the number of transitions from white to black, (0 -> 1) in the sequence P2,P3,P4,P5,P6,P7,P8,P9,P2. (Note the extra P2 at the end - it is circular).
* Define <math>B(P1)</math> = The number of black pixel neighbours of P1. ( = sum(P2 .. P9) )


;Step 1:
All pixels are tested and pixels satisfying all the following conditions (simultaneously) are just noted at this stage.
* (0) The pixel is black and has eight neighbours
* (1) <math>2 <= B(P1) <= 6</math>
* (2) A(P1) = 1
* (3) At least one of P2 and P4 and P6 is white
* (4) At least one of P4 and P6 and P8 is white

After iterating over the image and collecting all the pixels satisfying all step 1 conditions, all these condition satisfying pixels are set to white.


;Step 2:
All pixels are again tested and pixels satisfying all the following conditions are just noted at this stage.
* (0) The pixel is black and has eight neighbours
* (1) <math>2 <= B(P1) <= 6</math>
* (2) A(P1) = 1
* (3) At least one of P2 and P4 and '''P8''' is white
* (4) At least one of '''P2''' and P6 and P8 is white

After iterating over the image and collecting all the pixels satisfying all step 2 conditions, all these condition satisfying pixels are again set to white.


;Iteration:
If any pixels were set in this round of either step 1 or step 2 then all steps are repeated until no image pixels are so changed.


## Task

# Write a routine to perform Zhang-Suen thinning on an image matrix of ones and zeroes.
# Use the routine to thin the following image and show the output here on this page as either a matrix of ones and zeroes, an image, or an ASCII-art image of space/non-space characters.

           00000000000000000000000000000000
           01111111110000000111111110000000
           01110001111000001111001111000000
           01110000111000001110000111000000
           01110001111000001110000000000000
           01111111110000001110000000000000
           01110111100000001110000111000000
           01110011110011101111001111011100
           01110001111011100111111110011100
           00000000000000000000000000000000


;Reference:
* [http://nayefreza.wordpress.com/2013/05/11/zhang-suen-thinning-algorithm-java-implementation/ Zhang-Suen Thinning Algorithm, Java Implementation] by Nayef Reza.
* "Character Recognition Systems: A Guide for Students and Practitioners" By Mohamed Cheriet, Nawwaf Kharma, Cheng-Lin Liu, Ching Suen





## AutoHotkey

Reads input from a text file and writes output to a different text file (first creating the file, if necessary).

```AutoHotkey
FileIn  := A_ScriptDir "\Zhang-Suen.txt"
FileOut := A_ScriptDir "\NewFile.txt"

if (!FileExist(FileIn)) {
	MsgBox, 48, File Not Found, % "File """ FileIn """ not found."
	ExitApp
}
S := {}
N := [2,3,4,5,6,7,8,9,2]

Loop, Read, % FileIn
{
	LineNum := A_Index
	Loop, Parse, A_LoopReadLine
		S[LineNum, A_Index] := A_LoopField
}

Loop {
	FlipCount := 0
	Loop, 2 {
		Noted := [], i := A_Index
		for LineNum, Line in S {
			for PixNum, Pix in Line {
			; (0)
				if (Pix = 0 || (P := GetNeighbors(LineNum, PixNum, S)) = 1)
					continue
			; (1)
				BP := 0
				for j, Val in P
					BP += Val
				if (BP < 2 || BP > 6)
					continue
			; (2)
				AP := 0
				Loop, 8
					if (P[N[A_Index]] = "0" && P[N[A_Index + 1]] = "1")
						AP++
				if (AP != 1)
					continue
			; (3 and 4)
				if (i = 1) {
					if (P[2] + P[4] + P[6] = 3 || P[4] + P[6] + P[8] = 3)
						continue
				}
				else if (P[2] + P[4] + P[8] = 3 || P[2] + P[6] + P[8] = 3)
					continue

				Noted.Insert([LineNum, PixNum])
				FlipCount++
			}
		}
		for j, Coords in Noted
			S[Coords[1], Coords[2]] := 0
	}
	if (!FlipCount)
		break
}

for LineNum, Line in S {
	for PixNum, Pix in Line
		Out .= Pix ? "#" : " "
	Out .= "`n"
}
FileAppend, % Out, % FileOut

GetNeighbors(Y, X, S) {
	Neighbors := []
	if ((Neighbors[8] := S[Y, X - 1]) = "")
		return 1
	if ((Neighbors[4] := S[Y, X + 1]) = "")
		return 1
	Loop, 3
		if ((Neighbors[A_Index = 1 ? 9 : A_Index] := S[Y - 1, X - 2 + A_Index]) = "")
			return 1
	Loop, 3
		if ((Neighbors[8 - A_Index] := S[Y + 1, X - 2 + A_Index]) = "")
			return 1
	return Neighbors
}
```

'''Output:'''

```txt

  #######         ######
  #     #        ##
  #      #       #
  #     #        #
  ##### #        #
       ##        #
        #    #   ##    ##   #
         #         ####

```



## C

Input and out images written from and to files. Format of input file is :

```txt

<Rows> <Columns>
<Blank pixel character> <Image Pixel character>
<Image of specified rows and columns made up of the two pixel types specified in the second line.>

```

The images before and after thinning are also printed on the console.

```C

#include<stdlib.h>
#include<stdio.h>

char** imageMatrix;

char blankPixel,imagePixel;

typedef struct{
	int row,col;
}pixel;

int getBlackNeighbours(int row,int col){

	int i,j,sum = 0;

	for(i=-1;i<=1;i++){
		for(j=-1;j<=1;j++){
			if(i!=0 && j!=0)
				sum+= (imageMatrix[row+i][col+i]==imagePixel);
		}
	}

	return sum;
}

int getBWTransitions(int row,int col){
	return 	((imageMatrix[row-1][col]==blankPixel && imageMatrix[row-1][col+1]==imagePixel)
			+(imageMatrix[row-1][col+1]==blankPixel && imageMatrix[row][col+1]==imagePixel)
			+(imageMatrix[row][col+1]==blankPixel && imageMatrix[row+1][col+1]==imagePixel)
			+(imageMatrix[row+1][col+1]==blankPixel && imageMatrix[row+1][col]==imagePixel)
			+(imageMatrix[row+1][col]==blankPixel && imageMatrix[row+1][col-1]==imagePixel)
			+(imageMatrix[row+1][col-1]==blankPixel && imageMatrix[row][col-1]==imagePixel)
			+(imageMatrix[row][col-1]==blankPixel && imageMatrix[row-1][col-1]==imagePixel)
			+(imageMatrix[row-1][col-1]==blankPixel && imageMatrix[row-1][col]==imagePixel));
}

int zhangSuenTest1(int row,int col){
	int neighbours = getBlackNeighbours(row,col);

	return ((neighbours>=2 && neighbours<=6)
		&& (getBWTransitions(row,col)==1)
		&& (imageMatrix[row-1][col]==blankPixel||imageMatrix[row][col+1]==blankPixel||imageMatrix[row+1][col]==blankPixel)
		&& (imageMatrix[row][col+1]==blankPixel||imageMatrix[row+1][col]==blankPixel||imageMatrix[row][col-1]==blankPixel));
}

int zhangSuenTest2(int row,int col){
	int neighbours = getBlackNeighbours(row,col);

	return ((neighbours>=2 && neighbours<=6)
		&& (getBWTransitions(row,col)==1)
		&& (imageMatrix[row-1][col]==blankPixel||imageMatrix[row][col+1]==blankPixel||imageMatrix[row][col-1]==blankPixel)
		&& (imageMatrix[row-1][col]==blankPixel||imageMatrix[row+1][col]==blankPixel||imageMatrix[row][col+1]==blankPixel));
}

void zhangSuen(char* inputFile, char* outputFile){

	int startRow = 1,startCol = 1,endRow,endCol,i,j,count,rows,cols,processed;

	pixel* markers;

	FILE* inputP = fopen(inputFile,"r");

	fscanf(inputP,"%d%d",&rows,&cols);

	fscanf(inputP,"%d%d",&blankPixel,&imagePixel);

	blankPixel<=9?blankPixel+='0':blankPixel;
	imagePixel<=9?imagePixel+='0':imagePixel;

	printf("\nPrinting original image :\n");

	imageMatrix = (char**)malloc(rows*sizeof(char*));

	for(i=0;i<rows;i++){
		imageMatrix[i] = (char*)malloc((cols+1)*sizeof(char));
		fscanf(inputP,"%s\n",imageMatrix[i]);
		printf("\n%s",imageMatrix[i]);

	}

	fclose(inputP);

	endRow = rows-2;
	endCol = cols-2;
	do{
		markers = (pixel*)malloc((endRow-startRow+1)*(endCol-startCol+1)*sizeof(pixel));
		count = 0;

		for(i=startRow;i<=endRow;i++){
			for(j=startCol;j<=endCol;j++){
				if(imageMatrix[i][j]==imagePixel && zhangSuenTest1(i,j)==1){
					markers[count].row = i;
					markers[count].col = j;
					count++;
				}
			}
		}

		processed = (count>0);

		for(i=0;i<count;i++){
			imageMatrix[markers[i].row][markers[i].col] = blankPixel;
		}

		free(markers);
		markers = (pixel*)malloc((endRow-startRow+1)*(endCol-startCol+1)*sizeof(pixel));
		count = 0;

		for(i=startRow;i<=endRow;i++){
			for(j=startCol;j<=endCol;j++){
				if(imageMatrix[i][j]==imagePixel && zhangSuenTest2(i,j)==1){
					markers[count].row = i;
					markers[count].col = j;
					count++;
				}
			}
		}

		if(processed==0)
			processed = (count>0);

		for(i=0;i<count;i++){
			imageMatrix[markers[i].row][markers[i].col] = blankPixel;
		}

		free(markers);
	}while(processed==1);

	FILE* outputP = fopen(outputFile,"w");

	printf("\n\n\nPrinting image after applying Zhang Suen Thinning Algorithm : \n\n\n");

	for(i=0;i<rows;i++){
		for(j=0;j<cols;j++){
			printf("%c",imageMatrix[i][j]);
			fprintf(outputP,"%c",imageMatrix[i][j]);
		}
		printf("\n");
		fprintf(outputP,"\n");
	}

	fclose(outputP);

	printf("\nImage also written to : %s",outputFile);
}

int main()
{
	char inputFile[100],outputFile[100];

	printf("Enter full path of input image file : ");
	scanf("%s",inputFile);

	printf("Enter full path of output image file : ");
	scanf("%s",outputFile);

	zhangSuen(inputFile,outputFile);

	return 0;
}

```


Contents of input file : zhImage.txt

```txt

10 32
0 1
00000000000000000000000000000000
01111111110000000111111110000000
01110001111000001111001111000000
01110000111000001110000111000000
01110001111000001110000000000000
01111111110000001110000000000000
01110111100000001110000111000000
01110011110011101111001111011100
01110001111011100111111110011100
00000000000000000000000000000000

```


Console interaction :

```txt

Enter full path of input image file : zhImage.txt
Enter full path of output image file : out.txt

Printing original image :

00000000000000000000000000000000
01111111110000000111111110000000
01110001111000001111001111000000
01110000111000001110000111000000
01110001111000001110000000000000
01111111110000001110000000000000
01110111100000001110000111000000
01110011110011101111001111011100
01110001111011100111111110011100
00000000000000000000000000000000


Printing image after applying Zhang Suen Thinning Algorithm :


00000000000000000000000000000000
00111111100000000011111100000000
00100000100000000110000000000000
01000000100000000100000000000000
01000000100000001000000000000000
01111111100000001000000000000000
01000001000000000100000011000000
01000001000001100110000110001100
01000000000010000001111000010000
00000000000000000000000000000000

Image also written to : out.txt

```


Contents of out.txt :

```txt

00000000000000000000000000000000
00111111100000000011111100000000
00100000100000000110000000000000
01000000100000000100000000000000
01000000100000001000000000000000
01111111100000001000000000000000
01000001000000000100000011000000
01000001000001100110000110001100
01000000000010000001111000010000
00000000000000000000000000000000

```



## C++

Compiled with --std=c++14

```cpp
#include <iostream>
#include <string>
#include <sstream>
#include <valarray>
const std::string input {
"................................"
".#########.......########......."
".###...####.....####..####......"
".###....###.....###....###......"
".###...####.....###............."
".#########......###............."
".###.####.......###....###......"
".###..####..###.####..####.###.."
".###...####.###..########..###.."
"................................"
};
const std::string input2 {
".........................................................."
".#################...................#############........"
".##################...............################........"
".###################............##################........"
".########.....#######..........###################........"
"...######.....#######.........#######.......######........"
"...######.....#######........#######......................"
"...#################.........#######......................"
"...################..........#######......................"
"...#################.........#######......................"
"...######.....#######........#######......................"
"...######.....#######........#######......................"
"...######.....#######.........#######.......######........"
".########.....#######..........###################........"
".########.....#######.######....##################.######."
".########.....#######.######......################.######."
".########.....#######.######.........#############.######."
".........................................................."
};

class ZhangSuen;

class Image {
public:
    friend class ZhangSuen;
    using pixel_t = char;
    static const pixel_t BLACK_PIX;
    static const pixel_t WHITE_PIX;

    Image(unsigned width = 1, unsigned height = 1)
    : width_{width}, height_{height}, data_( '\0', width_ * height_)
    {}
    Image(const Image& i) : width_{ i.width_}, height_{i.height_}, data_{i.data_}
    {}
    Image(Image&& i) : width_{ i.width_}, height_{i.height_}, data_{std::move(i.data_)}
    {}
    ~Image() = default;
    Image& operator=(const Image& i) {
        if (this != &i) {
            width_ = i.width_;
            height_ = i.height_;
            data_ = i.data_;
        }
        return *this;
    }
    Image& operator=(Image&& i) {
        if (this != &i) {
            width_ = i.width_;
            height_ = i.height_;
            data_ = std::move(i.data_);
        }
        return *this;
    }
    size_t idx(unsigned x, unsigned y) const noexcept { return y * width_ + x; }
    bool operator()(unsigned x, unsigned y) {
        return data_[idx(x, y)];
    }
    friend std::ostream& operator<<(std::ostream& o, const Image& i) {
        o << i.width_ << " x " << i.height_ << std::endl;
        size_t px = 0;
        for(const auto& e : i.data_) {
            o << (e?Image::BLACK_PIX:Image::WHITE_PIX);
            if (++px % i.width_ == 0)
                o << std::endl;
        }
        return o << std::endl;
    }
    friend std::istream& operator>>(std::istream& in, Image& img) {
        auto it = std::begin(img.data_);
        const auto end = std::end(img.data_);
        Image::pixel_t tmp;
        while(in && it != end) {
            in >> tmp;
            if (tmp != Image::BLACK_PIX && tmp != Image::WHITE_PIX)
                throw "Bad character found in image";
            *it = (tmp == Image::BLACK_PIX)?1:0;
            ++it;
        }
        return in;
    }
    unsigned width() const noexcept { return width_; }
    unsigned height() const noexcept { return height_; }
    struct Neighbours {
        // 9 2 3
        // 8 1 4
        // 7 6 5
        Neighbours(const Image& img, unsigned p1_x, unsigned p1_y)
        : img_{img}
        , p1_{img.idx(p1_x, p1_y)}
        , p2_{p1_ - img.width()}
        , p3_{p2_ + 1}
        , p4_{p1_ + 1}
        , p5_{p4_ + img.width()}
        , p6_{p5_ - 1}
        , p7_{p6_ - 1}
        , p8_{p1_ - 1}
        , p9_{p2_ - 1}
        {}
        const Image& img_;
        const Image::pixel_t& p1() const noexcept { return img_.data_[p1_]; }
        const Image::pixel_t& p2() const noexcept { return img_.data_[p2_]; }
        const Image::pixel_t& p3() const noexcept { return img_.data_[p3_]; }
        const Image::pixel_t& p4() const noexcept { return img_.data_[p4_]; }
        const Image::pixel_t& p5() const noexcept { return img_.data_[p5_]; }
        const Image::pixel_t& p6() const noexcept { return img_.data_[p6_]; }
        const Image::pixel_t& p7() const noexcept { return img_.data_[p7_]; }
        const Image::pixel_t& p8() const noexcept { return img_.data_[p8_]; }
        const Image::pixel_t& p9() const noexcept { return img_.data_[p9_]; }
        const size_t p1_, p2_, p3_, p4_, p5_, p6_, p7_, p8_, p9_;
    };
    Neighbours neighbours(unsigned x, unsigned y) const { return Neighbours(*this, x, y); }
private:
    unsigned height_ { 0 };
    unsigned width_ { 0 };
    std::valarray<pixel_t> data_;
};

constexpr const Image::pixel_t Image::BLACK_PIX = '#';
constexpr const Image::pixel_t Image::WHITE_PIX = '.';

class ZhangSuen {
public:

    // the number of transitions from white to black, (0 -> 1) in the sequence P2,P3,P4,P5,P6,P7,P8,P9,P2
    unsigned transitions_white_black(const Image::Neighbours& a) const {
        unsigned sum = 0;
        sum += (a.p9() == 0) && a.p2();
        sum += (a.p2() == 0) && a.p3();
        sum += (a.p3() == 0) && a.p4();
        sum += (a.p8() == 0) && a.p9();
        sum += (a.p4() == 0) && a.p5();
        sum += (a.p7() == 0) && a.p8();
        sum += (a.p6() == 0) && a.p7();
        sum += (a.p5() == 0) && a.p6();
        return sum;
    }

    // The number of black pixel neighbours of P1. ( = sum(P2 .. P9) )
    unsigned black_pixels(const Image::Neighbours& a) const {
        unsigned sum = 0;
        sum += a.p9();
        sum += a.p2();
        sum += a.p3();
        sum += a.p8();
        sum += a.p4();
        sum += a.p7();
        sum += a.p6();
        sum += a.p5();
        return sum;
    }
    const Image& operator()(const Image& img) {
        tmp_a_ = img;
        size_t changed_pixels = 0;
        do {
            changed_pixels = 0;
            // Step 1
            tmp_b_ = tmp_a_;
            for(size_t y = 1; y < tmp_a_.height() - 1; ++y) {
                for(size_t x = 1; x < tmp_a_.width() - 1; ++x) {
                    if (tmp_a_.data_[tmp_a_.idx(x, y)]) {
                        auto n = tmp_a_.neighbours(x, y);
                        auto bp = black_pixels(n);
                        if (bp >= 2 && bp <= 6) {
                            auto tr = transitions_white_black(n);
                            if (    tr == 1
                                && (n.p2() * n.p4() * n.p6() == 0)
                                && (n.p4() * n.p6() * n.p8() == 0)
                                ) {
                                tmp_b_.data_[n.p1_] = 0;
                                ++changed_pixels;
                            }
                        }
                    }
                }
            }
            // Step 2
            tmp_a_ = tmp_b_;
            for(size_t y = 1; y < tmp_b_.height() - 1; ++y) {
                for(size_t x = 1; x < tmp_b_.width() - 1; ++x) {
                    if (tmp_b_.data_[tmp_b_.idx(x, y)]) {
                        auto n = tmp_b_.neighbours(x, y);
                        auto bp = black_pixels(n);
                        if (bp >= 2 && bp <= 6) {
                            auto tr = transitions_white_black(n);
                            if (    tr == 1
                                && (n.p2() * n.p4() * n.p8() == 0)
                                && (n.p2() * n.p6() * n.p8() == 0)
                                ) {
                                tmp_a_.data_[n.p1_] = 0;
                                ++changed_pixels;
                            }
                        }
                    }
                }
            }
        } while(changed_pixels > 0);
        return tmp_a_;
    }
private:
    Image tmp_a_;
    Image tmp_b_;
};

int main(int argc, char const *argv[])
{
    using namespace std;
    Image img(32, 10);
    istringstream iss{input};
    iss >> img;
    cout << img;
    cout << "ZhangSuen" << endl;
    ZhangSuen zs;
    Image res = std::move(zs(img));
    cout << res << endl;

    Image img2(58,18);
    istringstream iss2{input2};
    iss2 >> img2;
    cout << img2;
    cout << "ZhangSuen with big image" << endl;
    Image res2 = std::move(zs(img2));
    cout << res2 << endl;
    return 0;
}

```


Output:

```txt

32 x 10
................................
.#########.......########.......
.###...####.....####..####......
.###....###.....###....###......
.###...####.....###.............
.#########......###.............
.###.####.......###....###......
.###..####..###.####..####.###..
.###...####.###..########..###..
................................

ZhangSuen
32 x 10
................................
..#######.........######........
..#.....#........##.............
..#......#.......#..............
..#.....#........#..............
..#####.#........#..............
.......##........#..............
........#....#...##....##...#...
.........#.........####.........
................................


58 x 18
..........................................................
.#################...................#############........
.##################...............################........
.###################............##################........
.########.....#######..........###################........
...######.....#######.........#######.......######........
...######.....#######........#######......................
...#################.........#######......................
...################..........#######......................
...#################.........#######......................
...######.....#######........#######......................
...######.....#######........#######......................
...######.....#######.........#######.......######........
.########.....#######..........###################........
.########.....#######.######....##################.######.
.########.....#######.######......################.######.
.########.....#######.######.........#############.######.
..........................................................

ZhangSuen with big image
58 x 18
..........................................................
..........................................................
....#.##########.......................#######............
.....##........#...................####.......#...........
.....#..........#.................##......................
.....#..........#................#........................
.....#..........#................#........................
.....#..........#................#........................
.....############...............#.........................
.....#..........#...............#.........................
.....#..........#................#........................
.....#..........#................#........................
.....#..........#................#........................
.....#............................##......................
.....#.............................############...........
.......................###..........................###...
..........................................................
..........................................................

```



## D

This uses the module from the Bitmap Task. And it performs no heap allocations.

```d
import std.stdio, std.algorithm, std.string, std.functional,
       std.typecons, std.typetuple, bitmap;

struct BlackWhite {
    ubyte c;
    alias c this;
    static immutable black = typeof(this)(0),
                     white = typeof(this)(1);
}

alias Neighbours = BlackWhite[9];
alias Img = Image!BlackWhite;

/// Zhang-Suen thinning algorithm.
Img zhangSuen(Img image1, Img image2) pure nothrow @safe @nogc
in {
    assert(image1.image.all!(x => x == Img.black || x == Img.white));
    assert(image1.nx == image2.nx && image1.ny == image2.ny);
} out(result) {
    assert(result.nx == image1.nx && result.ny == image1.ny);
    assert(result.image.all!(x => x == Img.black || x == Img.white));
} body {
    /// True if inf <= x <= sup.
    static inInterval(T)(in T x, in T inf, in T sup) pure nothrow @safe @nogc {
        return x >= inf && x <= sup;
    }

    /// Return 8-neighbours+1 of point (x,y) of given image, in order.
    static void neighbours(in Img I, in size_t x, in size_t y,
                           out Neighbours n) pure nothrow @safe @nogc {
        n = [I[x,y-1], I[x+1,y-1], I[x+1,y], I[x+1,y+1], // P2,P3,P4,P5
             I[x,y+1], I[x-1,y+1], I[x-1,y], I[x-1,y-1], // P6,P7,P8,P9
             I[x,y-1]];
    }

    if (image1.nx < 3 || image1.ny < 3) {
        image2.image[] = image1.image[];
        return image2;
    }

    immutable static zeroOne = [0, 1]; //**
    Neighbours n;
    bool hasChanged;
    do {
        hasChanged = false;

        foreach (immutable ab; TypeTuple!(tuple(2, 4), tuple(0, 6))) {
            foreach (immutable y; 1 .. image1.ny - 1) {
                foreach (immutable x; 1 .. image1.nx - 1) {
                    neighbours(image1, x, y, n);
                    if (image1[x, y] &&                    // Cond. 0
                        (!n[ab[0]] || !n[4] || !n[6]) &&   // Cond. 4
                        (!n[0] || !n[2] || !n[ab[1]]) &&   // Cond. 3
                        //n[].count([0, 1]) == 1 &&
                        n[].count(zeroOne) == 1 &&         // Cond. 2
                        // n[0 .. 8].sum in iota(2, 7)) {
                        inInterval(n[0 .. 8].sum, 2, 6)) { // Cond. 1
                        hasChanged = true;
                        image2[x, y] = Img.black;
                    } else
                        image2[x, y] = image1[x, y];
                }
            }
            image1.swap(image2);
        }
    } while (hasChanged);

    return image1;
}

void main() {
    immutable before_txt = "
    ##..###
    ##..###
    ##..###
    ##..###
    ##..##.
    ##..##.
    ##..##.
    ##..##.
    ##..##.
    ##..##.
    ##..##.
    ##..##.
    ######.
    .......";

    immutable small_rc = "
    ................................
    .#########.......########.......
    .###...####.....####..####......
    .###....###.....###....###......
    .###...####.....###.............
    .#########......###.............
    .###.####.......###....###......
    .###..####..###.####..####.###..
    .###...####.###..########..###..
    ................................";

    immutable rc = "
    ...........................................................
    .#################...................#############.........
    .##################...............################.........
    .###################............##################.........
    .########.....#######..........###################.........
    ...######.....#######.........#######.......######.........
    ...######.....#######........#######.......................
    ...#################.........#######.......................
    ...################..........#######.......................
    ...#################.........#######.......................
    ...######.....#######........#######.......................
    ...######.....#######........#######.......................
    ...######.....#######.........#######.......######.........
    .########.....#######..........###################.........
    .########.....#######.######....##################.######..
    .########.....#######.######......################.######..
    .########.....#######.######.........#############.######..
    ...........................................................";

    foreach (immutable txt; [before_txt, small_rc, rc]) {
        auto img = Img.fromText(txt);
        "From:".writeln;
        img.textualShow(/*bl=*/ '.', /*wh=*/ '#');
        "\nTo thinned:".writeln;
        img.zhangSuen(img.dup).textualShow(/*bl=*/ '.', /*wh=*/ '#');
        writeln;
    }
}
```

```txt
From:
##..###
##..###
##..###
##..###
##..##.
##..##.
##..##.
##..##.
##..##.
##..##.
##..##.
##..##.
######.
.......

To thinned:
##..###
#.....#
#.....#
#...###
#...#..
#...#..
#...#..
#...#..
#...#..
#...#..
#...#..
#...#..
#####..
.......

From:
................................
.#########.......########.......
.###...####.....####..####......
.###....###.....###....###......
.###...####.....###.............
.#########......###.............
.###.####.......###....###......
.###..####..###.####..####.###..
.###...####.###..########..###..
................................

To thinned:
................................
..#######.........######........
..#.....#........##.............
..#......#.......#..............
..#.....#........#..............
..#####.#........#..............
.......##........#..............
........#....#...##....##...#...
.........#.........####.........
................................

From:
...........................................................
.#################...................#############.........
.##################...............################.........
.###################............##################.........
.########.....#######..........###################.........
...######.....#######.........#######.......######.........
...######.....#######........#######.......................
...#################.........#######.......................
...################..........#######.......................
...#################.........#######.......................
...######.....#######........#######.......................
...######.....#######........#######.......................
...######.....#######.........#######.......######.........
.########.....#######..........###################.........
.########.....#######.######....##################.######..
.########.....#######.######......################.######..
.########.....#######.######.........#############.######..
...........................................................

To thinned:
...........................................................
...........................................................
....#.##########.......................#######.............
.....##........#...................####.......#............
.....#..........#.................##.......................
.....#..........#................#.........................
.....#..........#................#.........................
.....#..........#................#.........................
.....############...............#..........................
.....#..........#...............#..........................
.....#..........#................#.........................
.....#..........#................#.........................
.....#..........#................#.........................
.....#............................##.......................
.....#.............................############............
.......................###..........................###....
...........................................................
...........................................................
```



## Elena

ELENA 3.4 :
```elena
import system'collections.
import system'routines.
import extensions.
import extensions'routines.

const image = (
        "                                                          ",
        " #################                   #############        ",
        " ##################               ################        ",
        " ###################            ##################        ",
        " ########     #######          ###################        ",
        "   ######     #######         #######       ######        ",
        "   ######     #######        #######                      ",
        "   #################         #######                      ",
        "   ################          #######                      ",
        "   #################         #######                      ",
        "   ######     #######        #######                      ",
        "   ######     #######        #######                      ",
        "   ######     #######         #######       ######        ",
        " ########     #######          ###################        ",
        " ########     ####### ######    ################## ###### ",
        " ########     ####### ######      ################ ###### ",
        " ########     ####### ######         ############# ###### ",
        "                                                          ").

nbrs = ((0, -1), (1, -1), (1, 0), (1, 1), (0, 1),
        (-1, 1), (-1, 0), (-1, -1), (0, -1)).

nbrGroups = (((0, 2, 4), (2, 4, 6)), ((0, 2, 6),
        (0, 4, 6))).

extension<Matrix<CharValue>> zhangsuenOp
{
    proceed(r, c, toWhite, firstStep)
    [
        if (self[r][c] != $35)
            [ ^ false ].

        int nn := self numNeighbors(r,c).

        if ((nn < 2) || (nn > 6))
            [ ^ false ].

        if(self numTransitions(r,c) != 1)
            [ ^ false ].

        ifnot (self atLeastOneIsWhite(r,c,firstStep iif(0,1)))
            [ ^ false ].

        toWhite append:{ x = c. y = r. }.

        ^ true.
    ]

    numNeighbors(r,c)
    [
        int count := 0.

        0 till(nbrs length - 1) do(:i)
        [
            if (self[r + nbrs[i][1]][c + nbrs[i][0]] == $35)
                [ count := count + 1. ].
        ].

        ^ count.
    ]

    numTransitions(r,c)
    [
        int count := 0.

        0 till(nbrs length - 1) do(:i)
        [
            if (self[r + nbrs[i][1]][c + nbrs[i][0]] == $32)
            [
                if (self[r + nbrs[i + 1][1]][c + nbrs[i + 1][0]] == $35)
                [
                    count := count + 1.
                ].
            ].
        ].

        ^ count.
    ]

    atLeastOneIsWhite(r, c, step)
    [
        int count := 0.
        var group := nbrGroups[step].
        0 till:2 do(:i)
        [
            0 till(group[i] length) seek(:j)
            [
                var nbr := nbrs[group[i][j]].

                if (self[r + nbr[1]][c + nbr[0]] == $32)
                    [ count := count + 1. ^ true ].

                ^ false.
            ].
        ].

        ^ count > 1.
    ]

    thinImage
    [
        bool firstStep := false.
        bool hasChanged := true.
        var toWhite := List new.

        while (hasChanged || firstStep)
        [
            hasChanged := false.
            firstStep := firstStep inverted.

            1 till(self rows - 1) do(:r)
            [
                1 till(self columns - 1) do(:c)
                [
                    if(self proceed(r,c,toWhite,firstStep))
                        [ hasChanged := true ].
                ].
            ].

            toWhite forEach(:p)[ self[p y][p x] := $32. ].
            toWhite clear.
        ].
    ]

    print
    [
        var it := self enumerator.

        it forEach(:ch) [ console print(ch," ") ].
        while (it next)
        [
            console writeLine.

            it forEach(:ch) [ console print(ch," ") ].
        ].
    ]
}

public program
[
    Matrix<CharValue> grid := MatrixSpace::
    {
        int rows = image length.

        int columns = image[0] length.

        getAt(int i, int j)
            = image[i][j].

        setAt(int i, int j, object o)
        [
            image[i][j] := o.
        ]
    }.

    grid thinImage.

    grid print.

    console readChar
]
```

```txt



      #   # # # # # # # # # #                                               # # # # # # #
        # #                 #                                       # # # #               #
        #                     #                                   # #
        #                     #                                 #
        #                     #                                 #
        #                     #                                 #
        # # # # # # # # # # # #                               #
        #                     #                               #
        #                     #                                 #
        #                     #                                 #
        #                     #                                 #
        #                                                         # #
        #                                                           # # # # # # # # # # # #
                                            # # #                                                     # # #


```



## Elixir

```elixir
defmodule ZhangSuen do
  @neighbours  [{-1,0},{-1,1},{0,1},{1,1},{1,0},{1,-1},{0,-1},{-1,-1}]  # 8 neighbours

  def thinning(str, black \\ ?#) do
    s0 = for {line, i} <- (String.split(str, "\n") |> Enum.with_index),
             {c, j}    <- (to_char_list(line) |> Enum.with_index),
             into: Map.new,
             do: {{i,j}, (if c==black, do: 1, else: 0)}
    {xrange, yrange} = range(s0)
    print(s0, xrange, yrange)
    s1 = thinning_loop(s0, xrange, yrange)
    print(s1, xrange, yrange)
  end

  defp thinning_loop(s0, xrange, yrange) do
    s1 = step(s0, xrange, yrange, 1)            # Step 1
    s2 = step(s1, xrange, yrange, 0)            # Step 2
    if Map.equal?(s0, s2), do: s2, else: thinning_loop(s2, xrange, yrange)
  end

  defp step(s, xrange, yrange, g) do
    for x <- xrange, y <- yrange, into: Map.new, do: {{x,y}, s[{x,y}] - zs(s,x,y,g)}
  end

  defp zs(s, x, y, g) do
    if get(s,x,y) == 0 or                                       # P1
      (get(s,x-1,y) + get(s,x,y+1) + get(s,x+g,y-1+g)) == 3 or  # P2, P4, P6/P8
      (get(s,x-1+g,y+g) + get(s,x+1,y) + get(s,x,y-1)) == 3 do  # P4/P2, P6, P8
      0
    else
      next = for {i,j} <- @neighbours, do: get(s, x+i, y+j)
      bp1 = Enum.sum(next)                                      # B(P1)
      if bp1 in 2..6 do
        ap1 = (next++[hd(next)]) |> Enum.chunk(2,1) |> Enum.count(fn [a,b] -> a<b end)  # A(P1)
        if ap1 == 1, do: 1, else: 0
      else
        0
      end
    end
  end

  defp get(map, x, y), do: Map.get(map, {x,y}, 0)

  defp range(map), do: range(Map.keys(map), 0, 0)
  defp range([], xmax, ymax), do: {0 .. xmax, 0 .. ymax}
  defp range([{x,y} | t], xmax, ymax), do: range(t, max(x,xmax), max(y,ymax))

  @display  %{0 => " ", 1 => "#"}
  defp print(map, xrange, yrange) do
    Enum.each(xrange, fn x ->
      IO.puts (for y <- yrange, do: @display[map[{x,y}]])
    end)
  end
end

str = """
...........................................................
.#################...................#############.........
.##################...............################.........
.###################............##################.........
.########.....#######..........###################.........
...######.....#######.........#######.......######.........
...######.....#######........#######.......................
...#################.........#######.......................
...###############...........#######.......................
...#################.........#######.......................
...######....########........#######.......................
...######.....#######........#######.......................
...######.....#######.........#######.......######.........
.########.....#######..........###################.........
.########.....#######..#####....##################.######..
.########.....#######..#####......################.######..
.########.....#######..#####.........#############.######..
...........................................................
"""
ZhangSuen.thinning(str)

str = """
00000000000000000000000000000000
01111111110000000111111110000000
01110001111000001111001111000000
01110000111000001110000111000000
01110001111000001110000000000000
01111111110000001110000000000000
01110111100000001110000111000000
01110011110011101111001111011100
01110001111011100111111110011100
00000000000000000000000000000000
"""
ZhangSuen.thinning(str, ?1)
```


```txt


 #################                   #############
 ##################               ################
 ###################            ##################
 ########     #######          ###################
   ######     #######         #######       ######
   ######     #######        #######
   #################         #######
   ###############           #######
   #################         #######
   ######    ########        #######
   ######     #######        #######
   ######     #######         #######       ######
 ########     #######          ###################
 ########     #######  #####    ################## ######
 ########     #######  #####      ################ ######
 ########     #######  #####         ############# ######



    # ##########                       #######
     ##        #                   ####       #
     #          #                 ##
     #          #                #
     #          #                #
     #          #                #
     ############               #
     #          #               #
     #          #                #
     #          #                #
     #          #                #
     #                            ##
     #                             ############
                        ##                          ###



 #########       ########
 ###   ####     ####  ####
 ###    ###     ###    ###
 ###   ####     ###
 #########      ###
 ### ####       ###    ###
 ###  ####  ### ####  #### ###
 ###   #### ###  ########  ###


  #######         ######
  #     #        ##
  #      #       #
  #     #        #
  ##### #        #
       ##        #
        #    #   ##    ##   #
         #         ####


```


## FreeBASIC


```freebasic
' version 08-10-2016
' compile with: fbc -s console

Data "00000000000000000000000000000000"
Data "01111111110000000111111110000000"
Data "01110001111000001111001111000000"
Data "01110000111000001110000111000000"
Data "01110001111000001110000000000000"
Data "01111111110000001110000000000000"
Data "01110111100000001110000111000000"
Data "01110011110011101111001111011100"
Data "01110001111011100111111110011100"
Data "00000000000000000000000000000000"
Data "END"

' ------=< MAIN >=------

Dim As UInteger x, y, m, n
Dim As String input_str

Do        ' find out how big it is
  Read input_str
  If input_str = "END" Then Exit Do
  If x < Len(input_str) Then x = Len(input_str)
  y = y + 1
Loop

m = x -1 : n = y -1
ReDim As UByte old(m, n), new_(m, n)

y = 0
Restore   ' restore data pointer
Do        ' put data in array
  Read input_str
  If input_str="END" Then Exit Do
  For x = 0 To Len(input_str) -1
    old(x,y) = input_str[x] - Asc("0")
    ' print image
    If old(x, y) = 0 Then Print "."; Else Print "#";
  Next
  Print
  y = y + 1
Loop

'corners and sides do not change
For x = 0 To m
  new_(x, 0) = old(x, 0)
  new_(x, n) = old(x, n)
Next

For y = 0 To n
  new_(0, y) = old(0, y)
  new_(m, y) = old(m, y)
Next

Dim As UInteger tmp, change, stage = 1
Do
  change = 0
  For y = 1 To n -1
    For x = 1 To m -1
      ' -1-
      If old(x,y) = 0 Then ' first condition, p1 must be black
        new_(x,y) = 0
        Continue For
      End If
      ' -2-
      tmp = old(x, y -1) + old(x +1, y -1)
      tmp = tmp + old(x +1, y) + old(x +1, y +1) + old(x, y +1)
      tmp = tmp + old(x -1, y +1) + old(x -1, y) + old(x -1, y -1)
      If tmp < 2 OrElse tmp > 6 Then ' 2 <= B(p1) <= 6
        new_(x, y) = 1
        Continue For
      End If
      ' -3-
      tmp = 0
      If old(x   , y   ) = 0 And old(x   , y -1) = 1 Then tmp += 1  ' p1 > p2
      If old(x   , y -1) = 0 And old(x +1, y -1) = 1 Then tmp += 1  ' p2 > p3
      If old(x +1, y -1) = 0 And old(x +1, y   ) = 1 Then tmp += 1  ' p3 > p4
      If old(x +1, y   ) = 0 And old(x +1, y +1) = 1 Then tmp += 1  ' p4 > p5
      If old(x +1, y +1) = 0 And old(x   , y +1) = 1 Then tmp += 1  ' p5 > p6
      If old(x   , y +1) = 0 And old(x -1, y +1) = 1 Then tmp += 1  ' p6 > p7
      If old(x -1, y +1) = 0 And old(x -1, y   ) = 1 Then tmp += 1  ' p7 > p8
      If old(x -1, y   ) = 0 And old(x -1, y -1) = 1 Then tmp += 1  ' p8 > p9
      If old(x -1, y -1) = 0 And old(x   , y -1) = 1 Then tmp += 1  ' p9 > p2
      ' tmp = 1 ==> A(P1) = 1
      If tmp <> 1 Then
        new_(x, y) = 1
        Continue For
      End If
      If (stage And 1) = 1 Then
        ' step 1 -4- -5-
        If (old(x, y -1) + old(x +1, y) + old(x, y +1)) = 3 OrElse _
           (old(x +1, y) + old(x, y +1) + old(x -1, y)) = 3 Then
          new_(x, y) = 1
          Continue For
        End If
      Else
        ' step 2 -4- -5-
        If (old(x, y -1) + old(x +1, y) + old(x -1, y)) = 3 OrElse _
           (old(x, y -1) + old(x, y +1) + old(x -1, y)) = 3 Then
          new_(x, y) = 1
          Continue For
        End If
      End If
      ' all condition are met, make p1 white (0)
      new_(x, y) = 0
      change = 1 ' flag change
    Next
  Next

  ' copy new_() into old()
  For y = 0 To n
    For x = 0 To m
      old(x, y) = new_(x, y)
    Next
  Next

  stage += 1
Loop Until change = 0 ' stop when there are no changes made

Print ' print result
Print "End result"
For y = 0 To n
  For x = 0 To m
    If old(x, y) = 0 Then Print "."; Else Print "#";
  Next
  Print
Next


' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
................................
.#########.......########.......
.###...####.....####..####......
.###....###.....###....###......
.###...####.....###.............
.#########......###.............
.###.####.......###....###......
.###..####..###.####..####.###..
.###...####.###..########..###..
................................

End result
................................
..#######.........######........
..#.....#........##.............
..#......#.......#..............
..#.....#........#..............
..#####.#........#..............
.......##........#..............
........#....#...##....##...#...
.........#.........####.........
................................
```



## Fortran

With F90 came standardisation of a variety of array manipulation facilities. Since the image array is to be inspected as a whole then adjusted rather than adjusted step-by-step as it is inspected, the first thought was to employ the special facility of the FOR ALL statement, which is that in an expression such as
```Fortarn
FOR ALL (i = 2:n - 1) A(i) = (A(i - 1) + A(i) + A(i + 1))/3
```
 all right-hand-side expressions will be evaluated with the original values of the array, while in the less special array assignment
```Fortran
A(2:N - 1) = (A(1:N - 2) + A(2:N - 1) + A(3:N))/3
```
 as in the case of the equivalent DO-loop, the processing will be with a mixture of old and new values as the loop proceeds.

So, that suggests something like
```Fortran
      FOR ALL (I = 2:N - 1, J = 2:M - 1)
       WHERE(DOT(I,J) .NE. 0) DOT(I,J) = ADJUST(DOT,I,J)
```

This requires function ADJUST to be a "pure" function, and they are not supposed to perpetrate side effects, such as one reporting that any adjustment was made. Nor is it clear that array DOT must be presented as a parameter either as the entire array or as element DOT(i,j), or if not, that it can be global to function ADJUST - which would also be an impurity - and for that matter, variables I and J could be global also...

Instead, thought turned to more closely following the task specification, which involves producing a list of elements to be adjusted after an inspection pass. Given that array DOT is two-dimensional, it would be nice if an element could be indexed via an expression such as <code>DOT(INDEX)</code> where INDEX was an array of two elements with INDEX(1) = i, and INDEX(2) = j, so as to access DOT(i,j) If this were possible, then obviously one could hope that array INDEX could be extended so as to store the multiple elements of a list of such locations to access, with a view to <code>DOT(INDEX(1:n)) = 0</code> adjusting the image.

Alas, such a syntax form is not accommodated. However, F90 also introduced the ability to define and use compound data types, such as the type PLACE as used below. It is not possible to define a type of a special, recognised form, such as say "SUBSCRIPT LIST" that can be used as dreamt of above, so the components are just ordinary variables. Two ordinary arrays could be used, one for each of the two subscripts, or a compound type could be devised in a hint towards self-documentation. Thus,
```Fortran
      DOT(WHACK(1:WHACKCOUNT).I,WHACK(1:WHACKCOUNT).J) = 0
```


But it doesn't work... After a fair amount of head scratching, not at all assisted by the woolly generalities and inane examples of the compiler's "help" collection, it became apparent that the expression did not work through a list of indices as anticipated, but instead, for ''each'' value of the first index, ''all'' the values of the second index were selected. Thus, instead of the first change being DOT(WHACK('''1''').I,WHACK('''1''').J) only, it was DOT(WHACK('''1''').I,WHACK('''1:WHACKCOUNT''').J) that were being cleared. Accordingly, the fancy syntax has to be abandoned in favour of a specific DO-loop.


```Fortran
      MODULE ZhangSuenThinning	!Image manipulation.
       CONTAINS
        SUBROUTINE ZST(DOT)	!Attempts to thin out thick lines.
         INTEGER DOT(:,:)	!The image in an array, rows down the page.
         TYPE PLACE		!This records an array location.
          INTEGER I			!Via its
          INTEGER J			!Indices.
          END TYPE PLACE	!A lot of baggage.
         TYPE(PLACE) WHACK(UBOUND(DOT,DIM = 1)*UBOUND(DOT,DIM = 2))	!Allow a whack for every dot.
         INTEGER WHACKCOUNT	!Counts up those to be wiped out.
         LOGICAL WHACKED	!Notes if any have been.
         INTEGER STEP,I,N,J,M	!Assistants.
         INTEGER D9(9)		!Holds a 3x3 portion.
         INTEGER HIT1(3,2),HIT2(3,2)	!Lists of elements to inspect for certain tests.
         PARAMETER (HIT1 = (/2,6,8, 4,2,6/))	!Two stages.
         PARAMETER (HIT2 = (/4,8,6, 2,4,8/))	!Each with two hit lists.
          N = UBOUND(DOT,DIM = 1)	!Number of rows.
          M = UBOUND(DOT,DIM = 2)	!Number of columns.
Commence a pass.
   10     WHACKED = .FALSE.	!No damage so far.
          DO STEP = 1,2		!Each pass is in two stages.
            WHACKCOUNT = 0	!No dots have been selected for whitewashing.
            DO I = 2,N - 1	!Scan down the rows.
              DO J = 2,M - 1	!And the columns. Interior dots only.
                IF (DOT(I,J).NE.0) THEN	!Rule 0: Is the dot black? Eight neighbours are present due to loop control.
                  D9(1:3) = DOT(I - 1,J - 1:J + 1)	!Yes. Form a 3x3 mesh.	1 2 3  not  9 2 3
                  D9(4:6) = DOT(I    ,J - 1:J + 1)	!As a 1-D array.	4 5 6       8 1 4
                  D9(7:9) = DOT(I + 1,J - 1:J + 1)	!For eased access.	7 8 9       7 6 5
                  CALL INSPECT(D9,HIT1(1,STEP),HIT2(1,STEP))	!Apply rules one to four, as specified.
                END IF			!So much for a black dot.
              END DO		!On to the next column.
            END DO		!On to the next row.
            IF (WHACKCOUNT.GT.0) THEN	!Are any to be wiped out?
              DO I = 1,WHACKCOUNT		!Yes!
                DOT(WHACK(I).I,WHACK(I).J) = 0		!One by one.
              END DO				!On to the next victim.
Can't use     DOT(WHACK(1:WHACKCOUNT).I,WHACK(1:WHACKCOUNT).J) = 0
              WHACKED = .TRUE.			!There has been a change.
            END IF			!So much for changes.
          END DO		!On to the second stage.
          IF (WHACKED) GO TO 10	!If there had been changes, perhaps there will be more.
         CONTAINS	!Some helpers.
          SUBROUTINE INSPECT(BLOB,HIT1,HIT2)	!Inspect a 3x3 piece according to the four levels of tests as specified.
           INTEGER BLOB(9)		!The piece. BLOB(5) is DOT(I,J), and is expected to be 1.
           INTEGER HIT1(3),HIT2(3)	!Two hit lists.
           INTEGER TWIRL(9)		!traces the periphery of the piece.
           PARAMETER (TWIRL = (/2,3,6,9,8,7,4,1,2/))	!Cycle around the periphery.
           INTEGER B	!A counter.			!Rule:
            B = SUM(BLOB) - BLOB(5)			!1: Count the neighbours having one, not zero.
            IF (2 <= B .AND. B <= 6) THEN		!   The test. Can't have 2 <= B <= 6, alas.
              IF (COUNT(BLOB(TWIRL(1:8))		!2: Counting transitions.
     *              .LT.BLOB(TWIRL(2:9))) .EQ.1) THEN	!   The test of 0 --> positive.
                IF (ANY(BLOB(HIT1).EQ.0)) THEN		!3: At least one must be white.
                  IF (ANY(BLOB(HIT2).EQ.0)) THEN	!4: Of two sets of three.
                    WHACKCOUNT = WHACKCOUNT + 1			!Another one down!
                    WHACK(WHACKCOUNT) = PLACE(I,J)		!This is the place.
                  END IF				!Now back out of the nested IF-statements.
                END IF				!Since the tests must all be passed
              END IF			!Rather than say three out of four.
            END IF		!For the given method.
          END SUBROUTINE INSPECT!That was weird.
        END SUBROUTINE ZST	!But so it goes.

        SUBROUTINE SHOW(A)	!Display an image array on the standard output.
         INTEGER A(:,:)		!Values are expected to be zero and one.
         CHARACTER*1 HIC(0:1)	!But I don't want to look at wads of digits.
         PARAMETER (HIC = (/".","#"/))	!These offer better contrast.
         INTEGER I		!A stepper.
         DO I = 1,UBOUND(A,DIM = 1)	!Work down the given number of rows.
           WRITE (6,"(666A1)") HIC(A(I,:))	!Roll a translated line.
         END DO				!Hopefully, no more than 666 to a line.
        END SUBROUTINE SHOW	!That was straightforward.
      END MODULE ZhangSuenThinning

      PROGRAM POKE	!Just set up the example.
      USE ZhangSuenThinning
      INTEGER N,M		!Parameters for the example.
      PARAMETER (N = 10,M = 32)	!Rows and columns.
      CHARACTER*(M) CANVAS(N)	!Rather than some monster DATA statement,
      PARAMETER (CANVAS = (/	!It is easier to prepare a worksheet.
     1 "                                ",
     2 " 111111111       11111111       ",
     3 " 111   1111     1111  1111      ",
     4 " 111    111     111    111      ",
     5 " 111   1111     111             ",
     6 " 111111111      111             ",
     7 " 111 1111       111    111      ",
     8 " 111  1111  111 1111  1111 111  ",
     9 " 111   1111 111  11111111  111  ",
     o "                                "/))
       INTEGER IMAGE(N,M)	!The image array. Exactly the required size.
       INTEGER I		!A stepper.

       DO I = 1,N		!Read the rows.
         READ (CANVAS(I),"(666I1)") IMAGE(I,:)	!Presumably, 666 will suffice.
       END DO			!A blank is taken as a zero with formatted input.

       WRITE (6,*) "The initial image..."
       CALL SHOW(IMAGE)
       WRITE (6,*)

       CALL ZST(IMAGE)
       WRITE (6,*) "And after 'thinning'..."
       CALL SHOW(IMAGE)

      END PROGRAM POKE
```


Output:

```txt

 The initial image...
................................
.#########.......########.......
.###...####.....####..####......
.###....###.....###....###......
.###...####.....###.............
.#########......###.............
.###.####.......###....###......
.###..####..###.####..####.###..
.###...####.###..########..###..
................................

 And after 'thinning'...
................................
..#######.........######........
..#.....#........##.............
..#......#.......#..............
..#.....#........#..............
..#####.#........#..............
.......##........#..............
........#....#...##....##...#...
.........#.........####.........
................................

```



## Go


```go
package main

import (
    "bytes"
    "fmt"
    "strings"
)

var in = `
00000000000000000000000000000000
01111111110000000111111110000000
01110001111000001111001111000000
01110000111000001110000111000000
01110001111000001110000000000000
01111111110000001110000000000000
01110111100000001110000111000000
01110011110011101111001111011100
01110001111011100111111110011100
00000000000000000000000000000000`

func main() {
    b := wbFromString(in, '1')
    b.zhangSuen()
    fmt.Println(b)
}

const (
    white = 0
    black = 1
)

type wbArray [][]byte // elements are white or black.

// parameter blk is character to read as black.  otherwise kinda rigid,
// expects ascii, leading newline, no trailing newline,
// takes color from low bit of character.
func wbFromString(s string, blk byte) wbArray {
    lines := strings.Split(s, "\n")[1:]
    b := make(wbArray, len(lines))
    for i, sl := range lines {
        bl := make([]byte, len(sl))
        for j := 0; j < len(sl); j++ {
            bl[j] = sl[j] & 1
        }
        b[i] = bl
    }
    return b
}

// rigid again, hard coded to output space for white, # for black,
// no leading or trailing newline.
var sym = [2]byte{
    white: ' ',
    black: '#',
}

func (b wbArray) String() string {
    b2 := bytes.Join(b, []byte{'\n'})
    for i, b1 := range b2 {
        if b1 > 1 {
            continue
        }
        b2[i] = sym[b1]
    }
    return string(b2)
}

// neighbor offsets
var nb = [...][2]int{
    2: {-1, 0}, // p2 offsets
    3: {-1, 1}, // ...
    4: {0, 1},
    5: {1, 1},
    6: {1, 0},
    7: {1, -1},
    8: {0, -1},
    9: {-1, -1}, // p9 offsets
}

func (b wbArray) reset(en []int) (rs bool) {
    var r, c int
    var p [10]byte

    readP := func() {
        for nx := 1; nx <= 9; nx++ {
            n := nb[nx]
            p[nx] = b[r+n[0]][c+n[1]]
        }
    }

    shiftRead := func() {
        n := nb[3]
        p[9], p[2], p[3] = p[2], p[3], b[r+n[0]][c+n[1]]
        n = nb[4]
        p[8], p[1], p[4] = p[1], p[4], b[r+n[0]][c+n[1]]
        n = nb[5]
        p[7], p[6], p[5] = p[6], p[5], b[r+n[0]][c+n[1]]
    }

    // returns "A", count of white->black transitions in circuit of neighbors
    // of an interior pixel b[r][c]
    countA := func() (ct byte) {
        bit := p[9]
        for nx := 2; nx <= 9; nx++ {
            last := bit
            bit = p[nx]
            if last == white {
                ct += bit
            }
        }
        return ct
    }

    // returns "B", count of black pixels neighboring interior pixel b[r][c].
    countB := func() (ct byte) {
        for nx := 2; nx <= 9; nx++ {
            ct += p[nx]
        }
        return ct
    }

    lastRow := len(b) - 1
    lastCol := len(b[0]) - 1

    mark := make([][]bool, lastRow)
    for r = range mark {
        mark[r] = make([]bool, lastCol)
    }

    for r = 1; r < lastRow; r++ {
        c = 1
        readP()
        for { // column loop
            m := false
            // test for failure of any of the five conditions,
            if !(p[1] == black) {
                goto markDone
            }
            if b1 := countB(); !(2 <= b1 && b1 <= 6) {
                goto markDone
            }
            if !(countA() == 1) {
                goto markDone
            }
            {
                e1, e2 := p[en[1]], p[en[2]]
                if !(p[en[0]]&e1&e2 == 0) {
                    goto markDone
                }
                if !(e1&e2&p[en[3]] == 0) {
                    goto markDone
                }
            }
            // no conditions failed, mark this pixel for reset
            m = true
            rs = true // and mark that image changes
        markDone:
            mark[r][c] = m
            c++
            if c == lastCol {
                break
            }
            shiftRead()
        }
    }
    if rs {
        for r = 1; r < lastRow; r++ {
            for c = 1; c < lastCol; c++ {
                if mark[r][c] {
                    b[r][c] = white
                }
            }
        }
    }
    return rs
}

var step1 = []int{2, 4, 6, 8}
var step2 = []int{4, 2, 8, 6}

func (b wbArray) zhangSuen() {
    for {
        rs1 := b.reset(step1)
        rs2 := b.reset(step2)
        if !rs1 && !rs2 {
            break
        }
    }
}
```

```txt


  #######         ######
  #     #        ##
  #      #       #
  #     #        #
  ##### #        #
       ##        #
        #    #   ##    ##   #
         #         ####


```



## Groovy


```groovy
def zhangSuen(text) {
    def image = text.split('\n').collect { line -> line.collect { it == '#' ? 1 : 0} }
    def p2, p3, p4, p5, p6, p7, p8, p9
    def step1 = { (p2 * p4 * p6 == 0) && (p4 * p6 * p8 == 0) }
    def step2 = { (p2 * p4 * p8 == 0) && (p2 * p6 * p8 == 0) }
    def reduce = { step ->
        def toWhite = []
        image.eachWithIndex{ line, y ->
            line.eachWithIndex{ pixel, x ->
                if (!pixel) return
                (p2, p3, p4, p5, p6, p7, p8, p9) = [image[y-1][x], image[y-1][x+1], image[y][x+1], image[y+1][x+1], image[y+1][x], image[y+1][x-1], image[y][x-1], image[y-1][x-1]]
                def a = [[p2,p3],[p3,p4],[p4,p5],[p5,p6],[p6,p7],[p7,p8],[p8,p9],[p9,p2]].collect { a1, a2 -> (a1 == 0 && a2 ==1) ? 1 : 0 }.sum()
                def b = [p2, p3, p4, p5, p6, p7, p8, p9].sum()
                if (a != 1 || b < 2 || b > 6) return

                if (step.call()) toWhite << [y,x]
            }
        }
        toWhite.each { y, x -> image[y][x] = 0 }
        !toWhite.isEmpty()
    }

    while (reduce(step1) | reduce(step2));
    image.collect { line -> line.collect { it ? '#' : '.' }.join('') }.join('\n')
}
```

Testing:

```groovy
def small = """\
    ................................
    .#########.......########.......
    .###...####.....####..####......
    .###....###.....###....###......
    .###...####.....###.............
    .#########......###.............
    .###.####.......###....###......
    .###..####..###.####..####.###..
    .###...####.###..########..###..
    ................................""".stripIndent()

def large = """\
    ...........................................................
    .#################...................#############.........
    .##################...............################.........
    .###################............##################.........
    .########.....#######..........###################.........
    ...######.....#######.........#######.......######.........
    ...######.....#######........#######.......................
    ...#################.........#######.......................
    ...################..........#######.......................
    ...#################.........#######.......................
    ...######.....#######........#######.......................
    ...######.....#######........#######.......................
    ...######.....#######.........#######.......######.........
    .########.....#######..........###################.........
    .########.....#######.######....##################.######..
    .########.....#######.######......################.######..
    .########.....#######.######.........#############.######..
    ...........................................................""".stripIndent()

[small, large].each {
    println "From:"
    println it
    println "To:"
    println zhangSuen(it)
    println()
}
```

Output:

```txt
From:
................................
.#########.......########.......
.###...####.....####..####......
.###....###.....###....###......
.###...####.....###.............
.#########......###.............
.###.####.......###....###......
.###..####..###.####..####.###..
.###...####.###..########..###..
................................
To:
................................
..#######.........######........
..#.....#........##.............
..#......#.......#..............
..#.....#........#..............
..#####.#........#..............
.......##........#..............
........#....#...##....##...#...
.........#.........####.........
................................

From:
...........................................................
.#################...................#############.........
.##################...............################.........
.###################............##################.........
.########.....#######..........###################.........
...######.....#######.........#######.......######.........
...######.....#######........#######.......................
...#################.........#######.......................
...################..........#######.......................
...#################.........#######.......................
...######.....#######........#######.......................
...######.....#######........#######.......................
...######.....#######.........#######.......######.........
.########.....#######..........###################.........
.########.....#######.######....##################.######..
.########.....#######.######......################.######..
.########.....#######.######.........#############.######..
...........................................................
To:
...........................................................
...........................................................
....#.##########.......................#######.............
.....##........#...................####.......#............
.....#..........#.................##.......................
.....#..........#................#.........................
.....#..........#................#.........................
.....#..........#................#.........................
.....############...............#..........................
.....#..........#...............#..........................
.....#..........#................#.........................
.....#..........#................#.........................
.....#..........#................#.........................
.....#............................##.......................
.....#.............................############............
.......................###..........................###....
...........................................................
...........................................................
```



## Haskell


```Haskell
import Data.Array
import qualified Data.List as List

data BW = Black | White
        deriving (Eq, Show)

type Index = (Int, Int)
type BWArray = Array Index BW

toBW :: Char -> BW
toBW '0' = White
toBW '1' = Black
toBW ' ' = White
toBW '#' = Black
toBW _   = error "toBW: illegal char"

toBWArray :: [String] -> BWArray
toBWArray strings = arr
  where
    height = length strings
    width  = minimum $ map length strings
    arr    = listArray ((0, 0), (width - 1, height - 1))
             . map toBW . concat . List.transpose $ map (take width) strings

toChar :: BW -> Char
toChar White = ' '
toChar Black = '#'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : (chunksOf n $ drop n xs)

showBWArray :: BWArray -> String
showBWArray arr =
  List.intercalate "\n" . List.transpose
  . chunksOf (height + 1) . map toChar $ elems arr
  where
    (_, (_, height)) = bounds arr

add :: Num a => (a, a) -> (a, a) -> (a, a)
add (a, b) (x, y) = (a + x, b + y)

within :: Ord a => ((a, a), (a, a)) -> (a, a) -> Bool
within ((a, b), (c, d)) (x, y) =
  a <= x && x <= c &&
  b <= y && y <= d

p2, p3, p4, p5, p6, p7, p8, p9 :: Index
p2 = ( 0, -1)
p3 = ( 1, -1)
p4 = ( 1,  0)
p5 = ( 1,  1)
p6 = ( 0,  1)
p7 = (-1,  1)
p8 = (-1,  0)
p9 = (-1, -1)

ixamap :: Ix i => ((i, a) -> b) -> Array i a -> Array i b
ixamap f a = listArray (bounds a) $ map f $ assocs a

thin :: BWArray -> BWArray
thin arr =
  if pass2 == arr then pass2 else thin pass2
  where
    (low, high)     = bounds arr
    lowB            = low `add` (1, 1)
    highB           = high `add` (-1, -1)
    isInner         = within (lowB, highB)
    offs p          = map (add p) [p2, p3, p4, p5, p6, p7, p8, p9]
    trans c (a, b)  = if a == White && b == Black then c + 1 else c
    zipshift xs     = zip xs (drop 1 xs ++ xs)
    transitions a   = (== (1 :: Int)) . foldl trans 0 . zipshift . map (a !) . offs
    within2to6 n    = 2 <= n && n <= 6
    blacks a p      = within2to6 . length . filter ((== Black) . (a !)) $ offs p
    oneWhite xs a p = any ((== White) . (a !) . add p) xs
    oneRight        = oneWhite [p2, p4, p6]
    oneDown         = oneWhite [p4, p6, p8]
    oneUp           = oneWhite [p2, p4, p8]
    oneLeft         = oneWhite [p2, p6, p8]
    precond a p     = (a ! p == Black) && isInner p && blacks a p && transitions a p
    stage1 a p      = precond a p && oneRight a p && oneDown a p
    stage2 a p      = precond a p && oneUp a p && oneLeft a p
    stager f (p, d) = if f p then White else d
    pass1           = ixamap (stager $ stage1 arr) arr
    pass2           = ixamap (stager $ stage2 pass1) pass1

sampleExA :: [String]
sampleExA =
  ["00000000000000000000000000000000"
  ,"01111111110000000111111110000000"
  ,"01110001111000001111001111000000"
  ,"01110000111000001110000111000000"
  ,"01110001111000001110000000000000"
  ,"01111111110000001110000000000000"
  ,"01110111100000001110000111000000"
  ,"01110011110011101111001111011100"
  ,"01110001111011100111111110011100"
  ,"00000000000000000000000000000000"]

sampleExB :: [String]
sampleExB =
  ["                                                          "
  ," #################                   #############        "
  ," ##################               ################        "
  ," ###################            ##################        "
  ," ########     #######          ###################        "
  ,"   ######     #######         #######       ######        "
  ,"   ######     #######        #######                      "
  ,"   #################         #######                      "
  ,"   ################          #######                      "
  ,"   #################         #######                      "
  ,"   ######     #######        #######                      "
  ,"   ######     #######        #######                      "
  ,"   ######     #######         #######       ######        "
  ," ########     #######          ###################        "
  ," ########     ####### ######    ################## ###### "
  ," ########     ####### ######      ################ ###### "
  ," ########     ####### ######         ############# ###### "
  ,"                                                          "]

main :: IO ()
main = mapM_ (putStrLn . showBWArray . thin . toBWArray) [sampleExA, sampleExB]
```

```txt
  #######         ######
  #     #        ##
  #      #       #
  #     #        #
  ##### #        #
       ##        #
        #    #   ##    ##   #
         #         ####



    # ##########                       #######
     ##        #                   ####       #
     #          #                 ##
     #          #                #
     #          #                #
     #          #                #
     ############               #
     #          #               #
     #          #                #
     #          #                #
     #          #                #
     #                            ##
     #                             ############
                       ###                          ###

```



## J

'''Solution:'''

```j
isBlackPx=: '1'&=;._2             NB. boolean array of black pixels
toImage=: [: , LF ,.~ '01' {~ ]   NB. convert to original representation
frameImg=: 0 ,. 0 , >:@$ {. ]     NB. adds border of 0's to image

neighbrs=: 1 :'(1 1 ,: 3 3)&(u;._3)'  NB. applies verb u to neighbourhoods

Bdry=: 1 2 5 8 7 6 3 0 1          NB. map pixel index to neighbour order
getPx=: { ,                       NB. get desired pixels from neighbourhood
Ap1=: [: +/ 2 </\ Bdry&getPx      NB. count 0->1 transitions
Bp1=: [: +/ [: }. Bdry&getPx      NB. count black neighbours

c11=: (2&<: *. <:&6)@Bp1          NB. step 1, condition 1
c12=: 1 = Ap1                     NB. ...
c13=: 0 e. 1 5 7&getPx
c14=: 0 e. 5 7 3&getPx
c23=: 0 e. 1 5 3&getPx            NB. step2, condition 3
c24=: 0 e. 1 7 3&getPx

cond1=: c11 *. c12 *. c13 *. c14  NB. step1 conditions
cond2=: c11 *. c12 *. c23 *. c24  NB. step2 conditions
whiten=: [ * -.@:*.               NB. make black pixels white
step1=: whiten frameImg@(cond1 neighbrs)
step2=: whiten frameImg@(cond2 neighbrs)

zhangSuen=: [: toImage [: step2@step1^:_ isBlackPx
```

'''Alternative, explicit representation of last verb above'''

```j
zhangSuenX=: verb define
  img=. isBlackPx y
  whilst. 0 < +/ , msk1 +.&-. msk2 do.
    msk1=. (-.@:*. [: frameImg cond1 neighbrs) img
    img=. msk1 * img
    msk2=. (-.@:*. [: frameImg cond2 neighbrs) img
    img=. msk2 * img
  end.
  toImage img
)
```

'''Example Use:'''

```j
toASCII=: ' #' {~ '1'&=;._2       NB. convert to ASCII representation

ExampleImg=: noun define
00000000000000000000000000000000
01111111110000000111111110000000
01110001111000001111001111000000
01110000111000001110000111000000
01110001111000001110000000000000
01111111110000001110000000000000
01110111100000001110000111000000
01110011110011101111001111011100
01110001111011100111111110011100
00000000000000000000000000000000
)

   toASCII zhangSuen ExampleImg

  #######         ######
  #     #        ##
  #      #       #
  #     #        #
  ##### #        #
       ##        #
        #    #   ##    ##   #
         #         ####

```



## Java

```java
import java.awt.Point;
import java.util.*;

public class ZhangSuen {

    final static String[] image = {
        "                                                          ",
        " #################                   #############        ",
        " ##################               ################        ",
        " ###################            ##################        ",
        " ########     #######          ###################        ",
        "   ######     #######         #######       ######        ",
        "   ######     #######        #######                      ",
        "   #################         #######                      ",
        "   ################          #######                      ",
        "   #################         #######                      ",
        "   ######     #######        #######                      ",
        "   ######     #######        #######                      ",
        "   ######     #######         #######       ######        ",
        " ########     #######          ###################        ",
        " ########     ####### ######    ################## ###### ",
        " ########     ####### ######      ################ ###### ",
        " ########     ####### ######         ############# ###### ",
        "                                                          "};

    final static int[][] nbrs = {{0, -1}, {1, -1}, {1, 0}, {1, 1}, {0, 1},
        {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}};

    final static int[][][] nbrGroups = {{{0, 2, 4}, {2, 4, 6}}, {{0, 2, 6},
        {0, 4, 6}}};

    static List<Point> toWhite = new ArrayList<>();
    static char[][] grid;

    public static void main(String[] args) {
        grid = new char[image.length][];
        for (int r = 0; r < image.length; r++)
            grid[r] = image[r].toCharArray();

        thinImage();
    }

    static void thinImage() {
        boolean firstStep = false;
        boolean hasChanged;

        do {
            hasChanged = false;
            firstStep = !firstStep;

            for (int r = 1; r < grid.length - 1; r++) {
                for (int c = 1; c < grid[0].length - 1; c++) {

                    if (grid[r][c] != '#')
                        continue;

                    int nn = numNeighbors(r, c);
                    if (nn < 2 || nn > 6)
                        continue;

                    if (numTransitions(r, c) != 1)
                        continue;

                    if (!atLeastOneIsWhite(r, c, firstStep ? 0 : 1))
                        continue;

                    toWhite.add(new Point(c, r));
                    hasChanged = true;
                }
            }

            for (Point p : toWhite)
                grid[p.y][p.x] = ' ';
            toWhite.clear();

        } while (firstStep || hasChanged);

        printResult();
    }

    static int numNeighbors(int r, int c) {
        int count = 0;
        for (int i = 0; i < nbrs.length - 1; i++)
            if (grid[r + nbrs[i][1]][c + nbrs[i][0]] == '#')
                count++;
        return count;
    }

    static int numTransitions(int r, int c) {
        int count = 0;
        for (int i = 0; i < nbrs.length - 1; i++)
            if (grid[r + nbrs[i][1]][c + nbrs[i][0]] == ' ') {
                if (grid[r + nbrs[i + 1][1]][c + nbrs[i + 1][0]] == '#')
                    count++;
            }
        return count;
    }

    static boolean atLeastOneIsWhite(int r, int c, int step) {
        int count = 0;
        int[][] group = nbrGroups[step];
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < group[i].length; j++) {
                int[] nbr = nbrs[group[i][j]];
                if (grid[r + nbr[1]][c + nbr[0]] == ' ') {
                    count++;
                    break;
                }
            }
        return count > 1;
    }

    static void printResult() {
        for (char[] row : grid)
            System.out.println(row);
    }
}
```


Output:


```txt

    # ##########                       #######
     ##        #                   ####       #
     #          #                 ##
     #          #                #
     #          #                #
     #          #                #
     ############               #
     #          #               #
     #          #                #
     #          #                #
     #          #                #
     #                            ##
     #                             ############
                       ###                          ###
```



## JavaScript

```javascript
function Point(x, y) {
    this.x = x;
    this.y = y;
}
var ZhangSuen = (function () {
    function ZhangSuen() {
    }
    ZhangSuen.image =
    ["                                                          ",
     " #################                   #############        ",
     " ##################               ################        ",
     " ###################            ##################        ",
     " ########     #######          ###################        ",
     "   ######     #######         #######       ######        ",
     "   ######     #######        #######                      ",
     "   #################         #######                      ",
     "   ################          #######                      ",
     "   #################         #######                      ",
     "   ######     #######        #######                      ",
     "   ######     #######        #######                      ",
     "   ######     #######         #######       ######        ",
     " ########     #######          ###################        ",
     " ########     ####### ######    ################## ###### ",
     " ########     ####### ######      ################ ###### ",
     " ########     ####### ######         ############# ###### ",
     "                                                          "];

    ZhangSuen.nbrs = [[0, -1], [1, -1], [1, 0], [1, 1], [0, 1], [-1, 1], [-1, 0], [-1, -1], [0, -1]];

    ZhangSuen.nbrGroups = [[[0, 2, 4], [2, 4, 6]], [[0, 2, 6], [0, 4, 6]]];

    ZhangSuen.toWhite = new Array();
    ;
    ZhangSuen.main = function (args) {
        ZhangSuen.grid = new Array(ZhangSuen.image.length);
        for (var r = 0; r < ZhangSuen.image.length; r++)
            ZhangSuen.grid[r] = (ZhangSuen.image[r]).split('');
        ZhangSuen.thinImage();
    };
    ZhangSuen.thinImage = function () {
        var firstStep = false;
        var hasChanged;
        do {
            hasChanged = false;
            firstStep = !firstStep;
            for (var r = 1; r < ZhangSuen.grid.length - 1; r++) {
                for (var c = 1; c < ZhangSuen.grid[0].length - 1; c++) {
                    if (ZhangSuen.grid[r][c] !== '#')
                        continue;
                    var nn = ZhangSuen.numNeighbors(r, c);
                    if (nn < 2 || nn > 6)
                        continue;
                    if (ZhangSuen.numTransitions(r, c) !== 1)
                        continue;
                    if (!ZhangSuen.atLeastOneIsWhite(r, c, firstStep ? 0 : 1))
                        continue;
                    ZhangSuen.toWhite.push(new Point(c, r));
                    hasChanged = true;
                }
            }
            for (let i = 0; i < ZhangSuen.toWhite.length; i++) {
                var p = ZhangSuen.toWhite[i];
                ZhangSuen.grid[p.y][p.x] = ' ';
            }
            ZhangSuen.toWhite = new Array();
        } while ((firstStep || hasChanged));
        ZhangSuen.printResult();
    };
    ZhangSuen.numNeighbors = function (r, c) {
        var count = 0;
        for (var i = 0; i < ZhangSuen.nbrs.length - 1; i++)
            if (ZhangSuen.grid[r + ZhangSuen.nbrs[i][1]][c + ZhangSuen.nbrs[i][0]] === '#')
                count++;
        return count;
    };
    ZhangSuen.numTransitions = function (r, c) {
        var count = 0;
        for (var i = 0; i < ZhangSuen.nbrs.length - 1; i++)
            if (ZhangSuen.grid[r + ZhangSuen.nbrs[i][1]][c + ZhangSuen.nbrs[i][0]] === ' ') {
                if (ZhangSuen.grid[r + ZhangSuen.nbrs[i + 1][1]][c + ZhangSuen.nbrs[i + 1][0]] === '#')
                    count++;
            }
        return count;
    };
    ZhangSuen.atLeastOneIsWhite = function (r, c, step) {
        var count = 0;
        var group = ZhangSuen.nbrGroups[step];
        for (var i = 0; i < 2; i++)
            for (var j = 0; j < group[i].length; j++) {
                var nbr = ZhangSuen.nbrs[group[i][j]];
                if (ZhangSuen.grid[r + nbr[1]][c + nbr[0]] === ' ') {
                    count++;
                    break;
                }
            }
        return count > 1;
    };
    ZhangSuen.printResult = function () {
        for (var i = 0; i < ZhangSuen.grid.length; i++) {
            var row = ZhangSuen.grid[i];
            console.log(row.join(''));
        }
    };
    return ZhangSuen;
}());
ZhangSuen.main(null);
```


Output:


```txt

    # ##########                       #######
     ##        #                   ####       #
     #          #                 ##
     #          #                #
     #          #                #
     #          #                #
     ############               #
     #          #               #
     #          #                #
     #          #                #
     #          #                #
     #                            ##
     #                             ############
                       ###                          ###
```




## Julia


```julia

const pixelstring =
"00000000000000000000000000000000" *
"01111111110000000111111110000000" *
"01110001111000001111001111000000" *
"01110000111000001110000111000000" *
"01110001111000001110000000000000" *
"01111111110000001110000000000000" *
"01110111100000001110000111000000" *
"01110011110011101111001111011100" *
"01110001111011100111111110011100" *
"00000000000000000000000000000000"
const pixels = reshape([UInt8(c- 48) for c in pixelstring], (32,10))'


function surroundtesting(px, i, j, step)
    if px[i,j] == 0
        return false
    end
    isize, jsize = size(px)
    if i < 1 || j < 1 || i == isize || j == jsize                         # criteria 0.both
        return false
    end
    s = Array{Int,1}(9)
    s[1] = s[9] = px[i-1,j]; s[2] = px[i-1,j+1]; s[3] = px[i,j+1]; s[4] = px[i+1,j+1]
    s[5] = px[i+1,j]; s[6] = px[i+1,j-1]; s[7] = px[i,j-1]; s[8] = px[i-1,j-1]
    b = sum(s[1:8])
    if b < 2 || b > 6                                                     # criteria 1.both
        return false
    end
    if sum([(s[i] == 0 && s[i+1] == 1) for i in 1:length(s)-1]) != 1      # criteria 2.both
        return false
    end
    if step == 1
        rightwhite = s[1] == 0 || s[3] == 0 || s[5] == 0                  # 1.3
        downwhite = s[3] == 0 || s[5] == 0 || s[7] == 0                   # 1.4
        return rightwhite && downwhite
    end
    upwhite = s[1] == 0 || s[3] == 0 || s[7] == 0                         # 2.3
    leftwhite = s[1] == 0 || s[5] == 0 || s[7] == 0                       # 2.4
    return upwhite && leftwhite
end


function zsthinning(mat)
    retmat = copy(mat)
    testmat = zeros(Int, size(mat))
    isize, jsize = size(testmat)
    needredo = true
    loops = 0
    while(needredo)
        loops += 1
        println("loop number $loops")
        needredo = false
        for n in 1:2
            for i in 1:isize, j in 1:jsize
                testmat[i,j] = surroundtesting(retmat, i, j, n) ? 1 : 0
            end
            for i in 1:isize, j in 1:jsize
                if testmat[i,j] == 1
                    retmat[i,j] = 0
                    needredo = true
                end
            end
        end
    end
    retmat
end


function asciiprint(mat)
    for i in 1:size(mat)[1]
        println(join(map(i -> i == 1 ? '#' : ' ', mat[i,:])))
    end
end


asciiprint(zsthinning(pixels))
```

```txt

loop number 1
loop number 2
loop number 3

  #######         ######
  #     #        ##
  #      #       #
  #     #        #
  ##### #        #
       ##        #
        #    #   ##    ##   #
         #         ####

```



## Kotlin

```scala
// version 1.1.2

class Point(val x: Int, val y: Int)

val image = arrayOf(
    "                                                          ",
    " #################                   #############        ",
    " ##################               ################        ",
    " ###################            ##################        ",
    " ########     #######          ###################        ",
    "   ######     #######         #######       ######        ",
    "   ######     #######        #######                      ",
    "   #################         #######                      ",
    "   ################          #######                      ",
    "   #################         #######                      ",
    "   ######     #######        #######                      ",
    "   ######     #######        #######                      ",
    "   ######     #######         #######       ######        ",
    " ########     #######          ###################        ",
    " ########     ####### ######    ################## ###### ",
    " ########     ####### ######      ################ ###### ",
    " ########     ####### ######         ############# ###### ",
    "                                                          "
)

val nbrs = arrayOf(
    intArrayOf( 0, -1), intArrayOf( 1, -1), intArrayOf( 1,  0),
    intArrayOf( 1,  1), intArrayOf( 0,  1), intArrayOf(-1,  1),
    intArrayOf(-1,  0), intArrayOf(-1, -1), intArrayOf( 0, -1)
)

val nbrGroups = arrayOf(
    arrayOf(intArrayOf(0, 2, 4), intArrayOf(2, 4, 6)),
    arrayOf(intArrayOf(0, 2, 6), intArrayOf(0, 4, 6))
)

val toWhite = mutableListOf<Point>()
val grid = Array(image.size) { image[it].toCharArray() }

fun thinImage() {
    var firstStep = false
    var hasChanged: Boolean
    do {
        hasChanged = false
        firstStep = !firstStep
        for (r in 1 until grid.size - 1) {
            for (c in 1 until grid[0].size - 1) {
                if (grid[r][c] != '#') continue
                val nn = numNeighbors(r, c)
                if (nn !in 2..6) continue
                if (numTransitions(r, c) != 1) continue
                val step = if (firstStep) 0 else 1
                if (!atLeastOneIsWhite(r, c, step)) continue
                toWhite.add(Point(c, r))
                hasChanged = true
            }
        }
        for (p in toWhite) grid[p.y][p.x] = ' '
        toWhite.clear()
    }
    while (firstStep || hasChanged)
    for (row in grid) println(row)
}

fun numNeighbors(r: Int, c: Int): Int {
    var count = 0
    for (i in 0 until nbrs.size - 1) {
        if (grid[r + nbrs[i][1]][c + nbrs[i][0]] == '#') count++
    }
    return count
}

fun numTransitions(r: Int, c: Int): Int {
    var count = 0
    for (i in 0 until nbrs.size - 1) {
        if (grid[r + nbrs[i][1]][c + nbrs[i][0]] == ' ') {
            if (grid[r + nbrs[i + 1][1]][c + nbrs[i + 1][0]] == '#') count++
        }
    }
    return count
}

fun atLeastOneIsWhite(r: Int, c: Int, step: Int): Boolean {
    var count = 0;
    val group = nbrGroups[step]
    for (i in 0..1) {
        for (j in 0 until group[i].size) {
            val nbr = nbrs[group[i][j]]
            if (grid[r + nbr[1]][c + nbr[0]] == ' ') {
                count++
                break
            }
        }
    }
    return count > 1
}

fun main(args: Array<String>) {
    thinImage()
}
```


```txt


    # ##########                       #######
     ##        #                   ####       #
     #          #                 ##
     #          #                #
     #          #                #
     #          #                #
     ############               #
     #          #               #
     #          #                #
     #          #                #
     #          #                #
     #                            ##
     #                             ############
                       ###                          ###


```



## Lua


```lua
function zhangSuenThin(img)
    local dirs={
        { 0,-1},
        { 1,-1},
        { 1, 0},
        { 1, 1},
        { 0, 1},
        {-1, 1},
        {-1, 0},
        {-1,-1},
        { 0,-1},
    }

    local black=1
    local white=0

    function A(x, y)
        local c=0
        local current=img[y+dirs[1][2]][x+dirs[1][1]]
        for i=2,#dirs do
            local to_compare=img[y+dirs[i][2]][x+dirs[i][1]]
            if current==white and to_compare==black then
                c=c+1
            end
            current=to_compare
        end
        return c
    end

    function B(x, y)
        local c=0
        for i=2,#dirs do
            local value=img[y+dirs[i][2]][x+dirs[i][1]]
            if value==black then
                c=c+1
            end
        end
        return c
    end

    function common_step(x, y)
        if img[y][x]~=black or x<=1 or x>=#img[y] or y<=1 or y>=#img then
            return false
        end

        local b_value=B(x, y)
        if b_value<2 or b_value>6 then
            return false
        end

        local a_value=A(x, y)
        if a_value~=1 then
            return false
        end
        return true
    end

    function step_one(x, y)
        if not common_step(x, y) then
            return false
        end
        local p2=img[y+dirs[1][2]][x+dirs[1][1]]
        local p4=img[y+dirs[3][2]][x+dirs[3][1]]
        local p6=img[y+dirs[5][2]][x+dirs[5][1]]
        local p8=img[y+dirs[7][2]][x+dirs[7][1]]

        if p4==white or p6==white or p2==white and p8==white then
            return true
        end
        return false
    end

    function step_two(x, y)
        if not common_step(x, y) then
            return false
        end
        local p2=img[y+dirs[1][2]][x+dirs[1][1]]
        local p4=img[y+dirs[3][2]][x+dirs[3][1]]
        local p6=img[y+dirs[5][2]][x+dirs[5][1]]
        local p8=img[y+dirs[7][2]][x+dirs[7][1]]

        if p2==white or p8==white or p4==white and p6==white then
            return true
        end
        return false
    end

    function convert(to_do)
        for k,v in pairs(to_do) do
            img[v[2]][v[1]]=white
        end
    end

    function do_step_on_all(step)
        local to_convert={}
        for y=1,#img do
            for x=1,#img[y] do
                if step(x, y) then
                    table.insert(to_convert, {x,y})
                end
            end
        end
        convert(to_convert)
        return #to_convert>0
    end

    local continue=true
    while continue do
        continue=false
        if do_step_on_all(step_one) then
            continue=true
        end

        if do_step_on_all(step_two) then
            continue=true
        end
    end

    for y=1,#img do
        for x=1,#img[y] do
            io.write(img[y][x]==black and '#' or ' ')
        end
        io.write('\n')
    end
end

local image = {
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0},
    {0,1,1,1,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1,1,0,0,1,1,1,1,0,0,0,0,0,0},
    {0,1,1,1,0,0,0,0,1,1,1,0,0,0,0,0,1,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0},
    {0,1,1,1,0,0,0,1,1,1,1,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0},
    {0,1,1,1,0,1,1,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1,1,0,0,0,0,0,0},
    {0,1,1,1,0,0,1,1,1,1,0,0,1,1,1,0,1,1,1,1,0,0,1,1,1,1,0,1,1,1,0,0},
    {0,1,1,1,0,0,0,1,1,1,1,0,1,1,1,0,0,1,1,1,1,1,1,1,1,0,0,1,1,1,0,0},
    {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
}

zhangSuenThin(image)

```


Output:

```txt


  #######         ######
  #     #        ##
  #      #       #
  #     #        #
  ##### #        #
       ##        #
        #    #   ##    ##   #
         #         ####


```



## Mathematica


Mathematica supports directly the Thinning methods "Morphological" and "MedialAxis".
The Zhang-Suen algorithm implementation could be done with:

```Mathematica
nB[mat_] := Delete[mat // Flatten, 5] // Total;

nA[mat_] := Module[{l},
   l = Flatten[mat][[{2, 3, 6, 9, 8, 7, 4, 1, 2}]];
   Total[Map[If[#[[1]] == 0 && #[[2]] == 1, 1, 0] &,
     Partition[l, 2, 1]]]
   ];

iW1[mat_] :=
  Module[{l = Flatten[mat]},
   If[Apply[Times, l[[{2, 6, 8}]]] + Apply[Times, l[[{4, 6, 8}]]] ==
     0, 0, 1]];
iW2[mat_] :=
  Module[{l = Flatten[mat]},
   If[Apply[Times, l[[{2, 6, 4}]]] + Apply[Times, l[[{4, 2, 8}]]] ==
     0, 0, 1]];

check[i_, j_, dat_, t_] := Module[{mat, d = Dimensions[dat], r, c},
   r = d[[1]];
   c = d[[2]];
   If[i > 1 && i < r && j > 1 && j < c,
    mat = dat[[i - 1 ;; i + 1, j - 1 ;; j + 1]];
    If[dat[[i, j]] == 1 && nA[mat] == 1 && 2 <= nB[mat] <= 6 &&
      If[t == 1, iW1[mat], iW2[mat]] == 0, 0, dat[[i, j]]],
    dat[[i, j]]
    ]];

iter[dat_] :=
  Module[{i =
     Flatten[Outer[List, Range[Dimensions[dat][[1]]],
       Range[Dimensions[dat][[2]]]], 1], tmp},
   tmp = Partition[check[#[[1]], #[[2]], dat, 1] & /@ i,
     Dimensions[dat][[2]]];
   Partition[check[#[[1]], #[[2]], tmp, 2] & /@ i,
    Dimensions[tmp][[2]]]];


FixedPoint[iter, dat]
```


Which results in:
(printMat is only defined to print an text output - the natural Mathemaica way would be to use ArrayPlot function, which create a graphic object which we can't paste into this wiki)

```txt


printMat[mat_] :=
  StringReplace[
   Riffle[Map[StringJoin, Map[ToString, mat, {2}]], "\n"] //
    StringJoin, {"1" -> "#", "0" -> "."}];

dat1 = {{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
    0}, {0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1,
    0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0}, {0, 1, 1, 1, 0, 0, 0, 0, 1,
    1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0,
    0}, {0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0}, {0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0,
    0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0}, {0, 1, 1, 1, 0, 0, 1, 1, 1,
    1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0,
    0}, {0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1,
    1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0}};

printMat[dat1]
printMat[FixedPoint[iter, dat1]]

................................
.#########.......########.......
.###...####.....####..####......
.###....###.....###....###......
.###...####.....###.............
.#########......###.............
.###.####.......###....###......
.###..####..###.####..####.###..
.###...####.###..########..###..
................................
................................
..#######.........######........
..#.....#........##.............
..#......#.......#..............
..#.....#........#..............
..#####.#........#..............
.......##........#..............
........#....#...##....##...#...
.........#.........####.........
................................

dat2 = {{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0,
    0, 0, 0, 0, 0}, {0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    0}, {0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 1,
    1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 1, 1, 1, 1, 1, 1, 0,
    0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
    1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0}, {0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0}, {0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 1, 1, 1, 1, 1, 1, 0,
    0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
    1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0}, {0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1,
    1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0}, {0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 1, 1,
    1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
    0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
    1, 1, 1, 0, 0}, {0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1,
    1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0,
    0}, {0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
    1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0}, {0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

printMat[dat2]
printMat[FixedPoint[iter, dat2]]

...........................................................
.#################...................#############.........
.##################...............################.........
.###################............##################.........
.########.....#######..........###################.........
...######.....#######.........#######.......######.........
...######.....#######........#######.......................
...#################.........#######.......................
...################..........#######.......................
...#################.........#######.......................
...######.....#######........#######.......................
...######.....#######........#######.......................
...######.....#######.........#######.......######.........
.########.....#######..........###################.........
.########.....#######.######....##################.######..
.########.....#######.######......################.######..
.########.....#######.######.........#############.######..
...........................................................
...........................................................
...........................................................
....#.##########.......................#######.............
.....##........#...................####.......#............
.....#..........#.................##.......................
.....#..........#................#.........................
.....#..........#................#.........................
.....#..........#................#.........................
.....############...............#..........................
.....#..........#...............#..........................
.....#..........#................#.........................
.....#..........#................#.........................
.....#..........#................#.........................
.....#............................##.......................
.....#.............................############............
.......................###..........................###....
...........................................................
...........................................................
```



## Perl

```perl
use List::Util qw(sum min);

$source = <<'END';
............................................................
..#################...................#############.........
..##################...............################.........
..###################............##################.........
..########.....#######..........###################.........
....######.....#######.........#######.......######.........
....######.....#######........#######.......................
....#################.........#######.......................
....################..........#######.......................
....#################.........#######.......................
....######.....#######........#######.......................
....######.....#######........#######.......................
....######.....#######.........#######.......######.........
..########.....#######..........###################.........
..########.....#######.######....##################.######..
..########.....#######.######......################.######..
..########.....#######.######.........#############.######..
............................................................
END

for $line (split "\n", $source) {
    push @lines, [map { 1 & ord $_ } split '', $line]
}

$v = @lines;
$h = @{$lines[0]};
push @black, @$_ for @lines;
@p8 = ((-$h-1), (-$h+0), (-$h+1),                    # flatland distances to 8 neighbors.
          0-1,              0+1,
         $h-1,    $h+0,    $h+1)[1,2,4,7,6,5,3,0];   # (in cycle order)

# Candidates have 8 neighbors and are known black
@cand = grep { $black[$_] } map { my $x = $_; map $_*$h + $x, 1..$v-2 } 1..$h-2;

do {
    sub seewhite {
        my($w1,$w2) = @_;
        my(@results);
        sub cycles { my(@neighbors)=@_; my $c; $c += $neighbors[$_] < $neighbors[($_+1)%8] for 0..$#neighbors; return $c }
        sub blacks { my(@neighbors)=@_; sum @neighbors }

        @prior = @cand; @cand = ();
        for $p (@prior) {
            @n = @black[map { $_+$p } @p8];
            if (cycles(@n) == 1 and 2 <= sum(blacks(@n)) and sum(blacks(@n)) <= 6 and min(@n[@$w1]) == 0 and min(@n[@$w2]) == 0) {
                push @results, $p;
            } else {
                push @cand, $p
            }
        }
        return @results;
    }

    @goners1 = seewhite [0,2,4], [2,4,6]; @black[@goners1] = 0 x @goners1;
    @goners2 = seewhite [0,2,6], [0,4,6]; @black[@goners2] = 0 x @goners2;
} until @goners1 == 0 and @goners2 == 0;

while (@black) { push @thinned, join '', qw<. #>[splice(@black,0,$h)] }

print join "\n", @thinned;
```

```txt
............................................................
............................................................
.....#.##########.......................#######.............
......##........#...................####.......#............
......#..........#.................##.......................
......#..........#................#.........................
......#..........#................#.........................
......#..........#................#.........................
......############...............#..........................
......#..........#...............#..........................
......#..........#................#.........................
......#..........#................#.........................
......#..........#................#.........................
......#............................##.......................
......#.............................############............
........................###..........................###....
............................................................
............................................................
```



## Perl 6

Source image may be based on any characters whose low bits are 0 or 1 (which conveniently includes . and #).

```perl6
my $source = qq:to/EOD/;
................................
.#########.......########.......
.###...####.....####..####......
.###....###.....###....###......
.###...####.....###.............
.#########......###.............
.###.####.......###....###......
.###..####..###.####..####.###..
.###...####.###..########..###..
................................
EOD

my @lines = ([.ords X+& 1] for $source.split("\n")); # The low bits Just Work.
my \v = +@lines;
my \h = +@lines[0];
my @black = flat @lines.map: *.values;   # Flatten to 1-dimensional.

my \p8 = [-h-1, -h+0, -h+1,         # Flatland distances to 8 neighbors.
           0-1,        0+1,
           h-1,  h+0,  h+1].[1,2,4,7,6,5,3,0];   # (in cycle order)

# Candidates have 8 neighbors and are known black
my @cand = grep { @black[$_] }, do
    for 1..v-2 X 1..h-2 -> (\y,\x) { y*h + x }

repeat while my @goners1 or my @goners2 {
    sub seewhite (\w1,\w2) {
        sub cycles (@neighbors) { [+] @neighbors Z< @neighbors[].rotate }
        sub blacks (@neighbors) { [+] @neighbors }

        my @prior = @cand; @cand = ();

        gather for @prior -> \p {
            my \n = @black[p8 X+ p];
            if cycles(n) == 1 and 2 <= blacks(n) <= 6 and n[w1].any == 0 and n[w2].any == 0
                 { take p }
            else { @cand.push: p }
        }
    }

    @goners1 = seewhite (0,2,4), (2,4,6);
    @black[@goners1] = 0 xx *;
    say "Ping: {[+] @black} remaining after removing ", @goners1;

    @goners2 = seewhite (0,2,6), (0,4,6);
    @black[@goners2] = 0 xx *;
    say "Pong: {[+] @black} remaining after removing ", @goners2;
}

say @black.splice(0,h).join.trans('01' => '.#') while @black;
```

```txt
Ping: 66 remaining after removing 33 41 49 56 67 71 74 80 83 86 89 99 106 114 119 120 121 131 135 138 146 169 178 195 197 210 215 217 227 230 233 236 238 240 243 246 249 251 253 257 258 259 263 264 266 268 269 270 273 274 279 280 283 284 285
Pong: 47 remaining after removing 65 73 88 97 104 112 129 137 144 161 167 176 193 198 208 216 225 226 231
Ping: 45 remaining after removing 87 194
Pong: 45 remaining after removing
Ping: 45 remaining after removing
Pong: 45 remaining after removing
................................
..#######.........######........
..#.....#........##.............
..#......#.......#..............
..#.....#........#..............
..#####.#........#..............
.......##........#..............
........#....#...##....##...#...
.........#.........####.........
................................
```



## Phix


```Phix
constant n = {{-1,0},{-1,1},{0,1},{1,1},{1,0},{1,-1},{0,-1},{-1,-1},{-1,0}};

function AB(sequence text, integer y, x, step)
integer wtb = 0, bn = 0
integer prev = '#', next
string p2468 = ""
    for i=1 to length(n) do
        next = text[y+n[i][1]][x+n[i][2]]
        wtb += (prev='.' and next<='#')
        bn += (i>1 and next<='#')
        if and_bits(i,1)=0 then p2468 = append(p2468,prev) end if
        prev = next
    end for
    if step=2 then -- make it p6842
        p2468 = p2468[3..4]&p2468[1..2]
    end if
    return {wtb,bn,p2468}
end function

procedure Zhang_Suen(sequence text)
integer wtb, bn, changed, changes
string p2468    -- (p6842 for step 2)
    text = split(text,'\n')
    while 1 do
        changed = 0
        for step=1 to 2 do
            changes = 0
            for y=2 to length(text)-1 do
                for x=2 to length(text[y])-1 do
                    if text[y][x]='#' then
                        {wtb,bn,p2468} = AB(text,y,x,step)
                        if wtb=1
                        and bn>=2 and bn<=6
                        and find('.',p2468[1..3])
                        and find('.',p2468[2..4])then
                            changes = 1
                            text[y][x] = '!'    -- (logically still black)
                        end if
                    end if
                end for
            end for
            if changes then
                for y=2 to length(text)-1 do
                    text[y] = substitute(text[y],"!",".")
                end for
                changed = 1
            end if
        end for
        if not changed then exit end if
    end while
    puts(1,join(text,"\n"))
end procedure

string small_rc = """
................................
.#########.......########.......
.###...####.....####..####......
.###....###.....###....###......
.###...####.....###.............
.#########......###.............
.###.####.......###....###......
.###..####..###.####..####.###..
.###...####.###..########..###..
................................"""
Zhang_Suen(small_rc)
```

```txt

................................
..#######.........######........
..#.....#........##.............
..#......#.......#..............
..#.....#........#..............
..#####.#........#..............
.......##........#..............
........#....#...##....##...#...
.........#.........####.........
................................

```



## PL/I

<lang>zhang: procedure options (main);        /* 8 July 2014 */

   declare pic(10) bit(32) initial (
      '00000000000000000000000000000000'b,
      '01111111110000000111111110000000'b,
      '01110001111000001111001111000000'b,
      '01110000111000001110000111000000'b,
      '01110001111000001110000000000000'b,
      '01111111110000001110000000000000'b,
      '01110111100000001110000111000000'b,
      '01110011110011101111001111011100'b,
      '01110001111011100111111110011100'b,
      '00000000000000000000000000000000'b );
   declare image  (10,32) bit(1) defined pic;
   declare status (10,32) fixed decimal (1);
   declare changes bit(1);
   declare (i, j, k, m, n) fixed binary;

   m = hbound(image,1); n = hbound(image,2);

   call display;

   /* Pixel labelling for pixels surrounding P1, co-ordinates (i,j). */
   /* P9 P2 P3 */
   /* P8 P1 P4 */
   /* P7 P6 P5 */

   do k = 1 to 10 until (^changes);
      changes = '0'b;
      /* Set conditions as follows: */
      /*   (0) The pixel is black and has eight neighbours */
      /*   (1) 2 < = B(P1) < = 6                           */
      /*   (2) A(P1) = 1                                   */
      /*   (3) At least one of P2 and P4 and P6 is white   */
      /*   (4) At least one of P4 and P6 and P8 is white   */
      status = -1;
      do i = 2 to m-1;
         do j = 2 to n-1;
            if image(i,j) then
               if B(i,j) >= 2 & B(i,j) <= 6 then
                  if A(i,j) = 1 then
                     if ^image(i-1,j) | ^image(i,j+1) | ^image(i+1,j) then
                        if ^image(i,j+1) | ^image(i+1,j) | ^image(i,j-1) then
                           status(i,j) = 4;
         end;
      end;
      /* Having determined a status for every bit in the image,   */
      /* change those bits to white.                              */
      do i = 2 to m-1;
         do j = 2 to n-1;
            if status(i,j) ^= -1 then do; image(i,j) = '0'b; changes = '1'b; end;
         end;
      end;

      /* Set conditions as follows: */
      /*   (0) The pixel is black and has eight neighbours */
      /*   (1) 2 < = B(P1) < = 6                           */
      /*   (2) A(P1) = 1                                   */
      /*   (3) At least one of P2 and P4 and P8 is white   */
      /*   (4) At least one of P2 and P6 and P8 is white   */
      status = -1;
      do i = 2 to m-1;
         do j = 2 to n-1;
            if image(i,j) then
               if B(i,j) >= 2 & B(i,j) <= 6 then
                  if A(i,j) = 1 then
                     if ^image(i-1,j) | ^image(i,j+1) | ^image(i,j-1) then
                        if ^image(i-1,j) | ^image(i+1,j) | ^image(i,j-1) then
                           status(i,j) = 4;
         end;
      end;
      /* Having determined a status for every bit in the image,   */
      /* change those bits to white.                              */
      do i = 2 to m-1;
         do j = 2 to n-1;
            if status(i,j) ^= -1 then do; image(i,j) = '0'b; changes = '1'b; end;
         end;
      end;

   end; /* of the "until" loop */

   put skip list ('Final image after ' || trim(k) || ' iterations:');
   call display;

display: procedure;
   declare (i, j) fixed binary;
   declare c character (1);

   do i = 1 to m;
      put skip edit ('row:', i) (A, F(3));
      do j = 1 to n;
         if image(i,j) then c = '.'; else c = ' ';
         put edit (c) (A);
      end;
   end;
   put skip;
end;

/* Returns the number of transitions from white to black from P2 through P9 and P2. */
A: procedure (i,j) returns (fixed binary);
   declare (i,j) fixed binary nonassignable;
   declare n(2:10) bit(1);

   n(2)  = image(i-1,j);  n(3) = image(i-1,j+1);
   n(4)  = image(i, j+1); n(5) = image(i+1,j+1);
   n(6)  = image(i+1,j);  n(7) = image(i+1,j-1);
   n(8)  = image(i,j-1);  n(9) = image(i-1,j-1);
   n(10) = image(i-1,j);

   return ( tally(string(n), '01'b) );
end A;

/* Count the pixel neighbors of P1 that are black */
B: procedure (i, j) returns (fixed binary);
   declare (i,j) fixed binary nonassignable;
   declare s fixed binary;

   s = image(i-1,j-1) + image(i-1,j) + image(i-1,j+1);
   s = s + image(i,j-1) + image(i,j+1);
   return ( s + image(i+1,j-1) + image(i+1,j) + image(i+1,j+1) );
end B;

end zhang;
```


```txt

[Initial configuration:]
row:  1
row:  2 .........       ........
row:  3 ...   ....     ....  ....
row:  4 ...    ...     ...    ...
row:  5 ...   ....     ...
row:  6 .........      ...
row:  7 ... ....       ...    ...
row:  8 ...  ....  ... ....  .... ...
row:  9 ...   .... ...  ........  ...
row: 10

[Intermeduiate "images" omitted]

Final image after 3 iterations:
row:  1
row:  2  .......         ......
row:  3  .     .        ..
row:  4  .      .       .
row:  5  .     .        .
row:  6  ..... .        .
row:  7       ..        .
row:  8        .    .   ..    ..   .
row:  9         .         ....
row: 10

Second image:
Image to be thinned:
row  1:
row  2:  ...............
row  3: ..................
row  4:  ..................
row  5:     ....       .....
row  6:     ....        .....
row  7:     ....         .....
row  8:     ....         .....
row  9:     ....        ......
row 10:     ....        .....
row 11:     ....       .....
row 12:     ....      .....
row 13:     ....     .....
row 14:     .............
row 15:     ..............
row 16:     ...............
row 17:     ....      ......
row 18:     ....       ......
row 19:     ....        .....
row 20:     ....        ......
row 21:     ....         .....
row 22:     ....         .....
row 23:     ....        ......
row 24:     ....       ......
row 25:  ...................
row 26: ...................
row 27:  .................
row 28:

Final image after 3 iterations:
row  1:
row  2:
row  3:   ..............
row  4:      .          .
row  5:      .           .
row  6:      .            .
row  7:      .            .
row  8:      .            .
row  9:      .            .
row 10:      .           ..
row 11:      .           .
row 12:      .          .
row 13:      .          .
row 14:      .         .
row 15:      ...........
row 16:      .          .
row 17:      .          ..
row 18:      .           .
row 19:      .            .
row 20:      .            .
row 21:      .            .
row 22:      .            .
row 23:      .            .
row 24:      .           ..
row 25:      .          ..
row 26:   ... ...........
row 27:
row 28:
```



## Python

Several input images are converted.

```python
# -*- coding: utf-8 -*-

# Example from [http://nayefreza.wordpress.com/2013/05/11/zhang-suen-thinning-algorithm-java-implementation/ this blog post].
beforeTxt = '''\
1100111
1100111
1100111
1100111
1100110
1100110
1100110
1100110
1100110
1100110
1100110
1100110
1111110
0000000\
'''

# Thanks to [http://www.network-science.de/ascii/ this site] and vim for these next two examples
smallrc01 = '''\
00000000000000000000000000000000
01111111110000000111111110000000
01110001111000001111001111000000
01110000111000001110000111000000
01110001111000001110000000000000
01111111110000001110000000000000
01110111100000001110000111000000
01110011110011101111001111011100
01110001111011100111111110011100
00000000000000000000000000000000\
'''

rc01 = '''\
00000000000000000000000000000000000000000000000000000000000
01111111111111111100000000000000000001111111111111000000000
01111111111111111110000000000000001111111111111111000000000
01111111111111111111000000000000111111111111111111000000000
01111111100000111111100000000001111111111111111111000000000
00011111100000111111100000000011111110000000111111000000000
00011111100000111111100000000111111100000000000000000000000
00011111111111111111000000000111111100000000000000000000000
00011111111111111110000000000111111100000000000000000000000
00011111111111111111000000000111111100000000000000000000000
00011111100000111111100000000111111100000000000000000000000
00011111100000111111100000000111111100000000000000000000000
00011111100000111111100000000011111110000000111111000000000
01111111100000111111100000000001111111111111111111000000000
01111111100000111111101111110000111111111111111111011111100
01111111100000111111101111110000001111111111111111011111100
01111111100000111111101111110000000001111111111111011111100
00000000000000000000000000000000000000000000000000000000000\
'''

def intarray(binstring):
    '''Change a 2D matrix of 01 chars into a list of lists of ints'''
    return [[1 if ch == '1' else 0 for ch in line]
            for line in binstring.strip().split()]

def chararray(intmatrix):
    '''Change a 2d list of lists of 1/0 ints into lines of 1/0 chars'''
    return '\n'.join(''.join(str(p) for p in row) for row in intmatrix)

def toTxt(intmatrix):
    '''Change a 2d list of lists of 1/0 ints into lines of '#' and '.' chars'''
    return '\n'.join(''.join(('#' if p else '.') for p in row) for row in intmatrix)

def neighbours(x, y, image):
    '''Return 8-neighbours of point p1 of picture, in order'''
    i = image
    x1, y1, x_1, y_1 = x+1, y-1, x-1, y+1
    #print ((x,y))
    return [i[y1][x],  i[y1][x1],   i[y][x1],  i[y_1][x1],  # P2,P3,P4,P5
            i[y_1][x], i[y_1][x_1], i[y][x_1], i[y1][x_1]]  # P6,P7,P8,P9

def transitions(neighbours):
    n = neighbours + neighbours[0:1]    # P2, ... P9, P2
    return sum((n1, n2) == (0, 1) for n1, n2 in zip(n, n[1:]))

def zhangSuen(image):
    changing1 = changing2 = [(-1, -1)]
    while changing1 or changing2:
        # Step 1
        changing1 = []
        for y in range(1, len(image) - 1):
            for x in range(1, len(image[0]) - 1):
                P2,P3,P4,P5,P6,P7,P8,P9 = n = neighbours(x, y, image)
                if (image[y][x] == 1 and    # (Condition 0)
                    P4 * P6 * P8 == 0 and   # Condition 4
                    P2 * P4 * P6 == 0 and   # Condition 3
                    transitions(n) == 1 and # Condition 2
                    2 <= sum(n) <= 6):      # Condition 1
                    changing1.append((x,y))
        for x, y in changing1: image[y][x] = 0
        # Step 2
        changing2 = []
        for y in range(1, len(image) - 1):
            for x in range(1, len(image[0]) - 1):
                P2,P3,P4,P5,P6,P7,P8,P9 = n = neighbours(x, y, image)
                if (image[y][x] == 1 and    # (Condition 0)
                    P2 * P6 * P8 == 0 and   # Condition 4
                    P2 * P4 * P8 == 0 and   # Condition 3
                    transitions(n) == 1 and # Condition 2
                    2 <= sum(n) <= 6):      # Condition 1
                    changing2.append((x,y))
        for x, y in changing2: image[y][x] = 0
        #print changing1
        #print changing2
    return image


if __name__ == '__main__':
    for picture in (beforeTxt, smallrc01, rc01):
        image = intarray(picture)
        print('\nFrom:\n%s' % toTxt(image))
        after = zhangSuen(image)
        print('\nTo thinned:\n%s' % toTxt(after))
```


Just the example asked for in the task:

```txt
From:
................................
.#########.......########.......
.###...####.....####..####......
.###....###.....###....###......
.###...####.....###.............
.#########......###.............
.###.####.......###....###......
.###..####..###.####..####.###..
.###...####.###..########..###..
................................

To thinned:
................................
..#######.........######........
..#.....#........##.............
..#......#.......#..............
..#.....#........#..............
..#####.#........#..............
.......##........#..............
........#....#...##....##...#...
.........#.........####.........
................................
```



## Racket



```racket
#lang racket
(define (img-01string->vector str)
  (define lines (regexp-split "\n" str))
  (define h (length lines))
  (define w (if (zero? h) 0 (string-length (car lines))))
  (define v (for*/vector #:length (* w h)
              ((l (in-list lines)) (p (in-string l)))
              (match p (#\0 0) (#\1 1) (#\# 1) (#\. 0))))
  (values v h w))

; Task (2) asks for "or an ASCII-art image of space/non-space characters."
; Spaces don't really impress where the borders are, so we'll use a dot.
(define cell->display-char (match-lambda (0 ".") (1 "#") (else "?")))

(define (display-img v w)
  (for ((p (in-vector v)) (col (in-naturals)))
    (printf "~a" (cell->display-char p))
    (when (= (modulo col w) (sub1 w)) (newline))))

; returns vector of ([P1's idx] P1 P2 ... P9)
(define (Pns v w r c)
  (define i (+ c (* r w)))
  (define-syntax-rule (vi+ x) (vector-ref v (+ i x)))
  (define-syntax-rule (vi- x) (vector-ref v (- i x)))
  (vector i (vi+ 0) (vi- w) (vi+ (- 1 w))
          (vi+ 1) (vi+ (+ w 1)) (vi+ w)
          (vi+ (- w 1)) (vi- 1) (vi- (+ w 1))))

; Second argument to in-vector is the start offset;
; We skip offset 0 (idx) and 1 (P1)
(define (B Ps) (for/sum ((Pn (in-vector Ps 2))) Pn))

(define (A Ps)
  (define P2 (vector-ref Ps 2))
  (define-values (rv _)
    (for/fold ((acc 0) (Pn-1 P2))
      ((Pn (in-sequences (in-vector Ps 3) (in-value P2))))
      (values (+ acc (if (and (= 0 Pn-1) (= 1 Pn)) 1 0)) Pn)))
  rv)

(define-syntax-rule (not-all-black? Pa Pb Pc) (zero? (* Pa Pb Pc)))
(define (z-s-thin v h w)
  ; return idx when thin necessary, #f otherwise
  (define (thin? Ps n/bour-check-1 n/bour-check-2)
    (match-define (vector idx P1 P2 _ P4 _ P6 _ P8 _) Ps)
    (and (= P1 1) (<= 2 (B Ps) 6) (= (A Ps) 1)
         (n/bour-check-1 P2 P4 P6 P8)
         (n/bour-check-2 P2 P4 P6 P8)
         idx))

  (define (has-white?-246 P2 P4 P6 P8) (not-all-black? P2 P4 P6))
  (define (has-white?-468 P2 P4 P6 P8) (not-all-black? P4 P6 P8))
  (define (has-white?-248 P2 P4 P6 P8) (not-all-black? P2 P4 P8))
  (define (has-white?-268 P2 P4 P6 P8) (not-all-black? P2 P6 P8))
  (define (step-n even-Pn-check-1 even-Pn-check-2)
    (for*/list ((r (in-range 1 (- h 1)))
                (c (in-range 1 (- w 1)))
                (idx (in-value (thin? (Pns v w r c)
                                      even-Pn-check-1
                                      even-Pn-check-2)))
                #:when idx) idx))

  (define (step-1) (step-n has-white?-246 has-white?-468))
  (define (step-2) (step-n has-white?-248 has-white?-268))
  (define (inner-z-s-thin)
    (define changed-list-1 (step-1))
    (for ((idx (in-list changed-list-1))) (vector-set! v idx 0))
    (define changed-list-2 (step-2))
    (for ((idx (in-list changed-list-2))) (vector-set! v idx 0))
    (unless (and (null? changed-list-1) (null? changed-list-2)) (inner-z-s-thin)))
  (inner-z-s-thin))

(define (read-display-thin-display-image img-str)
  (define-values (v h w) (img-01string->vector img-str))
  (printf "Original image:~%") (display-img v w)
  (z-s-thin v h w)
  (printf "Thinned image:~%") (display-img v w))

(define e.g.-image #<<EOS
00000000000000000000000000000000
01111111110000000111111110000000
01110001111000001111001111000000
01110000111000001110000111000000
01110001111000001110000000000000
01111111110000001110000000000000
01110111100000001110000111000000
01110011110011101111001111011100
01110001111011100111111110011100
00000000000000000000000000000000
EOS
  )

(define e.g.-image/2 #<<EOS
##..###
##..###
##..###
##..###
##..##.
##..##.
##..##.
##..##.
##..##.
##..##.
##..##.
##..##.
######.
.......
EOS
  )

(module+ main
  ; (read-display-thin-display-image e.g.-image/2)
  ; (newline)
  (read-display-thin-display-image e.g.-image))
```


Only the requested image is output:

```txt
Original image:
................................
.#########.......########.......
.###...####.....####..####......
.###....###.....###....###......
.###...####.....###.............
.#########......###.............
.###.####.......###....###......
.###..####..###.####..####.###..
.###...####.###..########..###..
................................
Thinned image:
................................
..#######.........######........
..#.....#........##.............
..#......#.......#..............
..#.....#........#..............
..#####.#........#..............
.......##........#..............
........#....#...##....##...#...
.........#.........####.........
................................
```



## REXX


```rexx
/*REXX program thins a  NxM  character grid  using  the  Zhang-Suen thinning  algorithm.*/
parse arg iFID .;  if iFID==''  then iFID='ZHANG_SUEN.DAT'
white=' ';         @.=white                      /* [↓]  read the input character grid. */
           do row=1  while lines(iFID)\==0;  _=linein(iFID)
           _=translate(_,,.0);               cols.row=length(_)
               do col=1  for cols.row;  @.row.col=substr(_,col,1)
               end   /*col*/                     /* [↑]  assign whole row of characters.*/
           end       /*row*/
rows=row-1                                       /*adjust ROWS because of the  DO loop. */
call show@ 'input file ' iFID  " contents:"      /*display show the input character grid*/

  do  until  changed==0;    changed=0            /*keep slimming until we're finished.  */
       do step=1  for 2                          /*keep track of  step one  or step two.*/
         do     r=1  for rows                    /*process all the  rows  and  columns. */
             do c=1  for cols.r;  !.r.c=@.r.c    /*assign an alternate grid.            */
             if r==1|r==rows|c==1|c==cols.r  then iterate             /*is this an edge?*/
             if @.r.c==white  then iterate       /*Is the character white?  Then skip it*/
             call Ps; b=b()                      /*define   Ps   and also   "b".        */
             if b<2 | b>6     then iterate       /*is   B   within the range ?          */
             if a()\==1       then iterate       /*count the number of transitions.     */    /*  ╔══╦══╦══╗  */
             if step==1       then if (p2 & p4 & p6)  |  p4 & p6 & p8  then iterate           /*  ║p9║p2║p3║  */
             if step==2       then if (p2 & p4 & p8)  |  p2 & p6 & p8  then iterate           /*  ╠══╬══╬══╣  */
             !.r.c=white                         /*set a grid character to  white.      */    /*  ║p8║p1║p4║  */
             changed=1                           /*indicate a character was changed.    */    /*  ╠══╬══╬══╣  */
             end   /*c*/                                                                      /*  ║p7║p6║p5║  */
         end       /*r*/                                                                      /*  ╚══╩══╩══╝  */
       call copy!                                /*copy the alternate to working grid.  */
       end         /*step*/
  end              /*until changed==0*/

call show@  'slimmed output:'                    /*display the slimmed character grid.  */
exit                                             /*stick a fork in it,  we're all done. */
/*─────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
a: return (\p2==p3&p3)+(\p3==p4&p4)+(\p4==p5&p5)+(\p5==p6&p6)+(\p6==p7&p7)+(\p7==p8&p8)+(\p8==p9&p9)+(\p9==p2&p2)
b: return p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9
copy!:                        do r=1  for rows;       do c=1  for cols.r;        @.r.c=!.r.c;  end;  end;  return
show@: say; say arg(1); say;  do r=1  for rows; _=;   do c=1  for cols.r; _=_ || @.r.c; end;  say _; end;  return
/*──────────────────────────────────────────────────────────────────────────────────────*/
Ps:  rm=r-1;  rp=r+1;  cm=c-1;  cp=c+1                       /*calculate some shortcuts.*/
     p2=@.rm.c\==white; p3=@.rm.cp\==white; p4=@.r.cp\==white; p5=@.rp.cp\==white
     p6=@.rp.c\==white; p7=@.rp.cm\==white; p8=@.r.cm\==white; p9=@.rm.cm\==white; return
```

'''output'''   when using the default input:

```txt

input file  ZHANG_SUEN.DAT  contents:


 #################                   #############
 ##################               ################
 ###################            ##################
 ########     #######          ###################
   ######     #######         #######       ######
   ######     #######        #######
   #################         #######
   ################          #######
   #################         #######
   ######     #######        #######
   ######     #######        #######
   ######     #######         #######       ######
 ########     #######          ###################
 ########     ####### ######    ################## ######
 ########     ####### ######      ################ ######
 ########     ####### ######         ############# ######


slimmed output:



    # ##########                       #######
     ##        #                   ####       #
     #          #                 ##
     #          #                #
     #          #                #
     #          #                #
     ############               #
     #          #               #
     #          #                #
     #          #                #
     #          #                #
     #                            ##
     #                             ############
                       ###                          ###

```

'''output'''   when using the default input:   <tt> zhang_suen2.dat </tt>

```txt

input file  zhang_suen2.dat  contents:


 111111111       11111111
 111   1111     1111  1111
 111    111     111    111
 111   1111     111
 111111111      111
 111 1111       111    111
 111  1111  111 1111  1111 111
 111   1111 111  11111111  111


slimmed output:


  1111111         111111
  1     1        11
  1      1       1
  1     1        1
  11111 1        1
       11        1
        1    1   11    11   1
         1         1111

```



## Ruby

First I define a function zs which given a point and its eight neighbours returns 1 if the point may be culled, 0 otherwise. g indicates if this is step 1 or step 2 in the task description. zs may be changed to remember the step independently if the reader does not wish to explore the algorithm.


```ruby
class ZhangSuen
  NEIGHBOUR8 = [[-1,0],[-1,1],[0,1],[1,1],[1,0],[1,-1],[0,-1],[-1,-1]]  # 8 neighbors
  CIRCULARS = NEIGHBOUR8 + [NEIGHBOUR8.first]                       # P2, ... P9, P2
  def initialize(str, black="#")
    s1 = str.each_line.map{|line| line.chomp.each_char.map{|c| c==black ? 1 : 0}}
    s2 = s1.map{|line| line.map{0}}
    xrange = 1 ... s1.size-1
    yrange = 1 ... s1[0].size-1
    printout(s1)
    begin
      @r = 0
      xrange.each{|x| yrange.each{|y| s2[x][y] = s1[x][y] - zs(s1,x,y,1)}}  # Step 1
      xrange.each{|x| yrange.each{|y| s1[x][y] = s2[x][y] - zs(s2,x,y,0)}}  # Step 2
    end until @r == 0
    printout(s1)
  end
  def zs(ng,x,y,g)
    return 0 if ng[x][y] == 0 or                                    # P1
               (ng[x-1][y] + ng[x][y+1] + ng[x+g][y-1+g]) == 3 or   # P2, P4, P6/P8
               (ng[x-1+g][y+g] + ng[x+1][y] + ng[x][y-1]) == 3      # P4/P2, P6, P8
    bp1 = NEIGHBOUR8.inject(0){|res,(i,j)| res += ng[x+i][y+j]}     # B(P1)
    return 0 if bp1 < 2 or 6 < bp1
    ap1 = CIRCULARS.map{|i,j| ng[x+i][y+j]}.each_cons(2).count{|a,b| a<b}   # A(P1)
    return 0 if ap1 != 1
    @r = 1
  end
  def printout(image)
    puts image.map{|row| row.map{|col| " #"[col]}.join}
  end
end

str = <<EOS
...........................................................
.#################...................#############.........
.##################...............################.........
.###################............##################.........
.########.....#######..........###################.........
...######.....#######.........#######.......######.........
...######.....#######........#######.......................
...#################.........#######.......................
...################..........#######.......................
...#################.........#######.......................
...######.....#######........#######.......................
...######.....#######........#######.......................
...######.....#######.........#######.......######.........
.########.....#######..........###################.........
.########.....#######.######....##################.######..
.########.....#######.######......################.######..
.########.....#######.######.........#############.######..
...........................................................
EOS

ZhangSuen.new(str)

task_example = <<EOS
00000000000000000000000000000000
01111111110000000111111110000000
01110001111000001111001111000000
01110000111000001110000111000000
01110001111000001110000000000000
01111111110000001110000000000000
01110111100000001110000111000000
01110011110011101111001111011100
01110001111011100111111110011100
00000000000000000000000000000000
EOS

ZhangSuen.new(task_example, "1")
```


(only the requested result is shown here)

```txt

  #######         ######
  #     #        ##
  #      #       #
  #     #        #
  ##### #        #
       ##        #
        #    #   ##    ##   #
         #         ####


```



## Sidef

```ruby
class ZhangSuen(str, black="1") {
  const NEIGHBOURS = [[-1,0],[-1,1],[0,1],[1,1],[1,0],[1,-1],[0,-1],[-1,-1]]  # 8 neighbors
  const CIRCULARS = (NEIGHBOURS + [NEIGHBOURS.first])                         # P2, ... P9, P2

  has r = 0
  has image = [[]]

  method init {
    var s1 = str.lines.map{|line| line.chars.map{|c| c==black ? 1 : 0 }}
    var s2 = s1.len.of { s1[0].len.of(0) }
    var xr = range(1, s1.end-1)
    var yr = range(1, s1[0].end-1)
    do {
        r = 0
        xr.each{|x| yr.each{|y| s2[x][y] = (s1[x][y] - self.zs(s1,x,y,1)) }}  # Step 1
        xr.each{|x| yr.each{|y| s1[x][y] = (s2[x][y] - self.zs(s2,x,y,0)) }}  # Step 2
    } while !r.is_zero
    image = s1
  }

  method zs(ng,x,y,g) {
       (ng[x][y] == 0)                                   ->
    || (ng[x-1][y] + ng[x][y+1] + ng[x+g][y+g - 1] == 3) ->
    || (ng[x+g - 1][y+g] + ng[x+1][y] + ng[x][y-1] == 3) ->
    && return 0

    var bp1 = NEIGHBOURS.map {|p| ng[x+p[0]][y+p[1]] }.sum  # B(P1)
    return 0 if ((bp1 < 2) || (6 < bp1))

    var ap1 = 0
    CIRCULARS.map {|p| ng[x+p[0]][y+p[1]] }.each_cons(2, {|a,b|
        ++ap1 if (a < b)                                    # A(P1)
    })

    return 0 if (ap1 != 1)
    r = 1
  }

  method display {
    image.each{|row| say row.map{|col| col ? '#' : ' ' }.join }
  }
}

var text = <<EOS
00000000000000000000000000000000
01111111110000000111111110000000
01110001111000001111001111000000
01110000111000001110000111000000
01110001111000001110000000000000
01111111110000001110000000000000
01110111100000001110000111000000
01110011110011101111001111011100
01110001111011100111111110011100
00000000000000000000000000000000
EOS

ZhangSuen.new(text, black: "1").display
```

```txt


  #######         ######
  #     #        ##
  #      #       #
  #     #        #
  ##### #        #
       ##        #
        #    #   ##    ##   #
         #         ####


```



## Tcl

Only the single image is converted.

```tcl
# -*- coding: utf-8 -*-

set data {
00000000000000000000000000000000
01111111110000000111111110000000
01110001111000001111001111000000
01110000111000001110000111000000
01110001111000001110000000000000
01111111110000001110000000000000
01110111100000001110000111000000
01110011110011101111001111011100
01110001111011100111111110011100
00000000000000000000000000000000
}
proc zhang-suen data {
    set data [string trim $data]
    while 1 {
	set n 0
	incr n [step 1 data]
	incr n [step 2 data]
	if !$n break
    }
    return $data
}
proc step {number _data} {
    upvar 1 $_data data
    set xmax [string length [lindex $data 0]]
    set ymax [llength $data]
    switch -- $number {
	1 {set cond {(!$P2 || !$P4 || !$P6) && (!$P4 || !$P6 || !$P8)}}
	2 {set cond {(!$P2 || !$P4 || !$P8) && (!$P2 || !$P6 || !$P8)}}
    }
    set hits {}
    for {set x 1} {$x < $xmax-1} {incr x} {
	for {set y 1} {$y < $ymax-1} {incr y} {
	    if {[getpix $data $x $y] == 1} {
		set b [B $data $x $y]
		if {2 <= $b && $b <= 6} {
		    if {[A $data $x $y] == 1} {
			set P2 [getpix $data $x [expr $y-1]]
			set P4 [getpix $data [expr $x+1] $y]
			set P6 [getpix $data $x [expr $y+1]]
			set P8 [getpix $data [expr $x-1] $y]
			if $cond {lappend hits $x $y}
		    }
		}
	    }
	}
    }
    foreach {x y} $hits {set data [setpix $data $x $y 0]}
    return [llength $hits]
}
proc A {data x y} {
    set res 0
    set last [getpix $data $x [expr $y-1]]
    foreach {dx dy} {1 -1  1 0  1 1  0 1  -1 1  -1 0  -1 -1  0 -1} {
	set this [getpix $data [expr $x+$dx] [expr $y+$dy]]
	if {$this > $last} {incr res}
	set last $this
    }
    return $res
}
proc B {data x y} {
    set res 0
    foreach {dx dy} {1 -1  1 0  1 1  0 1  -1 1  -1 0  -1 -1  0 -1} {
	incr res [getpix $data [expr $x+$dx] [expr $y+$dy]]
    }
    return $res
}
proc getpix {data x y} {
    string index [lindex $data $y] $x
}
proc setpix {data x y val} {
    set row [lindex $data $y]
    lset data $y [string replace $row $x $x $val]
    return $data
}
puts [string map {1 @ 0 .} [join [zhang-suen $data] \n]]
```


```txt

................................
..@@@@@@@.........@@@@@@........
..@.....@........@@.............
..@......@.......@..............
..@.....@........@..............
..@@@@@.@........@..............
.......@@........@..............
........@....@...@@....@@...@...
.........@.........@@@@.........
................................

```


## VBA

```vb
Public n As Variant
Private Sub init()
    n = [{-1,0;-1,1;0,1;1,1;1,0;1,-1;0,-1;-1,-1;-1,0}]
End Sub

Private Function AB(text As Variant, y As Integer, x As Integer, step As Integer) As Variant
    Dim wtb As Integer
    Dim bn As Integer
    Dim prev As String: prev = "#"
    Dim next_ As String
    Dim p2468 As String
    For i = 1 To UBound(n)
        next_ = Mid(text(y + n(i, 1)), x + n(i, 2), 1)
        wtb = wtb - (prev = "." And next_ <= "#")
        bn = bn - (i > 1 And next_ <= "#")
        If (i And 1) = 0 Then p2468 = p2468 & prev
        prev = next_
    Next i
    If step = 2 Then '-- make it p6842
        p2468 = Mid(p2468, 3, 2) & Mid(p2468, 1, 2)
        'p2468 = p2468(3..4)&p2468(1..2)
    End If
    Dim ret(2) As Variant
    ret(0) = wtb
    ret(1) = bn
    ret(2) = p2468
    AB = ret
End Function

Private Sub Zhang_Suen(text As Variant)
    Dim wtb As Integer
    Dim bn As Integer
    Dim changed As Boolean, changes As Boolean
    Dim p2468 As String     '-- (p6842 for step 2)
    Dim x As Integer, y As Integer, step As Integer
    Do While True
        changed = False
        For step = 1 To 2
            changes = False
            For y = 1 To UBound(text) - 1
                For x = 2 To Len(text(y)) - 1
                    If Mid(text(y), x, 1) = "#" Then
                        ret = AB(text, y, x, step)
                        wtb = ret(0)
                        bn = ret(1)
                        p2468 = ret(2)
                        If wtb = 1 _
                            And bn >= 2 And bn <= 6 _
                            And InStr(1, Mid(p2468, 1, 3), ".") _
                            And InStr(1, Mid(p2468, 2, 3), ".") Then
                            changes = True
                            text(y) = Left(text(y), x - 1) & "!" & Right(text(y), Len(text(y)) - x)
                        End If
                    End If
                Next x
            Next y
            If changes Then
                For y = 1 To UBound(text) - 1
                    text(y) = Replace(text(y), "!", ".")
                Next y
                changed = True
            End If
        Next step
        If Not changed Then Exit Do
    Loop
    Debug.Print Join(text, vbCrLf)
End Sub

Public Sub main()
    init
    Dim Small_rc(9) As String
    Small_rc(0) = "................................"
    Small_rc(1) = ".#########.......########......."
    Small_rc(2) = ".###...####.....####..####......"
    Small_rc(3) = ".###....###.....###....###......"
    Small_rc(4) = ".###...####.....###............."
    Small_rc(5) = ".#########......###............."
    Small_rc(6) = ".###.####.......###....###......"
    Small_rc(7) = ".###..####..###.####..####.###.."
    Small_rc(8) = ".###...####.###..########..###.."
    Small_rc(9) = "................................"
    Zhang_Suen (Small_rc)
End Sub
```
```txt
................................
...######.........######........
...#....#.........#....##.......
...#....#.........#......#......
...#....#.........#.............
...####.#.........#.............
.......##.........#.............
........#....#....#....##...#...
.........#....#....####......#..
................................
```

