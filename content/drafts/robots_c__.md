+++
title = "Robots/C++"
description = ""
date = 2017-10-30T23:08:51Z
aliases = []
[extra]
id = 21651
[taxonomies]
categories = []
tags = []
+++

==Code==
Windows Console implementation - No safe teleport is implemeted, just the random one.
[[File:robotsCpp.png|200px|thumb|right]]

```cpp

#include <windows.h>
#include <iostream>
#include <ctime>

const int WID = 62, HEI = 42, INC = 10;

class coord : public COORD {
public:
    coord( short x = 0, short y = 0 ) { set( x, y ); }
    void set( short x, short y ) { X = x; Y = y; }
};
class winConsole {
public:
    static winConsole* getInstamnce() { 
        if( 0 == inst ) { 
            inst = new winConsole(); 
        } 
        return inst; 
    }
    void showCursor( bool s ) { 
        CONSOLE_CURSOR_INFO ci = { 1, s }; 
        SetConsoleCursorInfo( conOut, &ci ); 
    }
    void setColor( WORD clr ) { SetConsoleTextAttribute( conOut, clr ); }
    void setCursor( coord p ) { SetConsoleCursorPosition( conOut, p ); }
    void setSize( int w, int h ) {
        coord crd( w + 1, h + 1 ); 
        SetConsoleScreenBufferSize( conOut, crd );
        SMALL_RECT rc = { 0, 0, WID, HEI }; 
        SetConsoleWindowInfo( conOut, TRUE, &rc );
    }
    void flush() { FlushConsoleInputBuffer( conIn ); }
    void kill() { delete inst; }
private:
    winConsole() { conOut = GetStdHandle( STD_OUTPUT_HANDLE ); 
                   conIn  = GetStdHandle( STD_INPUT_HANDLE ); showCursor( false ); }
    static winConsole* inst;
    HANDLE conOut, conIn;
};
class robots {
public:
    robots() { 
        console = winConsole::getInstamnce();
        console->setSize( WID, HEI );
    }
    ~robots() { console->kill(); }
    void play() {
        char g; do {
            console->showCursor( false ); 
            robotsCount = 10; score = 0; alive = true; 
            clearBoard(); cursor.set( rand() % ( WID - 2 ) + 1, rand() % ( HEI - 2 ) + 1 );
            brd[cursor.X + WID * cursor.Y] = '@'; createBoard();
            do{ 
                displayBoard(); getInput(); 
                if( !aliveRobots ) { 
                    robotsCount += INC; clearBoard(); 
                    brd[cursor.X + WID * cursor.Y] = '@'; createBoard(); 
                }
            } while( alive );
            displayBoard(); console->setCursor( coord( 0, 24 ) ); console->setColor( 0x07 );
            console->setCursor( coord( 10,  8 ) ); 
            std::cout << "+----------------------------------------+";
            console->setCursor( coord( 10,  9 ) ); 
            std::cout << "|               GAME OVER                |";
            console->setCursor( coord( 10, 10 ) ); 
            std::cout << "|            PLAY AGAIN(Y/N)?            |";
            console->setCursor( coord( 10, 11 ) ); 
            std::cout << "+----------------------------------------+";
            console->setCursor( coord( 39, 10 ) ); console->showCursor( true ); 
            console->flush(); std::cin >> g;
        } while( g == 'Y' || g == 'y' );
    }
private:
    void clearBoard() {
        for( int y = 0; y < HEI; y++ ) {
            for( int x = 0; x < WID; x++ ) {
                brd[x + WID * y] = 32;
                if( x == 0 || x == WID - 1 || y == 0 || y == HEI - 1 ) 
                    brd[x + WID * y] = '#';
            }
        }
    }
    void createBoard() {
        aliveRobots = robotsCount;
        int a, b; for( int x = 0; x < robotsCount; x++ ) {
            do {
                a = rand() % WID; b = rand() % HEI;
            } while( brd[a + WID * b] != 32 );
            brd[a + WID * b] = '+';
        }
        printScore();
    }
    void displayBoard() {
        char t; console->setCursor( coord() );
        for( int y = 0; y < HEI; y++ ) {
            for( int x = 0; x < WID; x++ ) {
                t = brd[x + WID * y];
                switch( t ) {
                    case ' ': console->setColor( 0x00 ); break;
                    case '#': console->setColor( 0x09 ); break;
                    case '+': console->setColor( 0x0e ); break;
                    case 'Å': case '*': console->setColor( 0x0c ); break;
                    case '@': console->setColor( 0x0a );
                }
                std::cout << t; 
            }
            std::cout << "\n";
        }
    }
    void getInput() { 
        while( 1 ) {
            if( ( GetAsyncKeyState( 'Q' ) & 0x8000 ) && cursor.X > 1 && cursor.Y > 1 ) 
                { execute( -1, -1 ); break; }
            if( ( GetAsyncKeyState( 'W' ) & 0x8000 ) && cursor.Y > 1 ) 
                { execute( 0, -1 ); break; }
            if( ( GetAsyncKeyState( 'E' ) & 0x8000 ) && cursor.X < WID - 2 && cursor.Y > 1 ) 
                { execute( 1, -1 ); break; }
            if( ( GetAsyncKeyState( 'A' ) & 0x8000 ) && cursor.X > 1 ) 
                { execute( -1, 0 ); break; }
            if( ( GetAsyncKeyState( 'D' ) & 0x8000 ) && cursor.X < WID - 2 ) 
                { execute( 1, 0 ); break; }
            if( ( GetAsyncKeyState( 'Y' ) & 0x8000 ) && cursor.X > 1 && cursor.Y < HEI - 2 ) 
                { execute( -1, 1 ); break; }
            if( ( GetAsyncKeyState( 'X' ) & 0x8000 ) && cursor.Y < HEI - 2 ) 
                { execute( 0, 1 ); break; }
            if( ( GetAsyncKeyState( 'C' ) & 0x8000 ) && cursor.X < WID - 2 && cursor.Y < HEI - 2 ) 
                { execute( 1, 1 ); break; }
            if( ( GetAsyncKeyState( 'T' ) & 0x8000 ) ) 
                { teleport(); moveRobots(); break; }
            if( ( GetAsyncKeyState( 'Z' ) & 0x8000 ) ) 
                { waitForEnd(); break; }
        }
        console->flush(); printScore();
    }
    void teleport() {
        brd[cursor.X + WID * cursor.Y] = 32;
        cursor.X = rand() % ( WID - 2 ) + 1;
        cursor.Y = rand() % ( HEI - 2 ) + 1;
        int x = cursor.X + WID * cursor.Y;
        if( brd[x] == '*' || brd[x] == '+' || brd[x] == '~' ) {
            alive = false; brd[x] = 'Å';
        } else  brd[x] = '@'; 
    }
    void printScore() {
        console->setCursor( coord( 0, HEI ) ); console->setColor( 0x2a );
        std::cout << "      SCORE: " << score << "      ";
    }
    void execute( int x, int y ) {
        brd[cursor.X + WID * cursor.Y] = 32; cursor.X += x; cursor.Y += y;
        brd[cursor.X + WID * cursor.Y] = '@'; moveRobots();
    }
    void waitForEnd() {
        while( aliveRobots && alive ) {
            moveRobots(); displayBoard(); Sleep( 500 );
        }
    }
    void moveRobots() {
        int tx, ty;
        for( int y = 0; y < HEI; y++ ) {
            for( int x = 0; x < WID; x++ ) {
                if( brd[x + WID * y] != '+' ) continue;
                tx = x; ty = y;
                if( tx < cursor.X ) tx++; else if( tx > cursor.X ) tx--;
                if( ty < cursor.Y ) ty++; else if( ty > cursor.Y ) ty--;
                if( tx != x || ty != y ) {
                    brd[x + WID * y] = 32;
                    if( brd[tx + WID * ty] == 32 ) brd[tx + WID * ty] = '~';
                    else checkCollision( tx, ty );
                }
            }
        }
        for( int x = 0; x < WID * HEI; x++ ) {
            if( brd[x] == '~') brd[x] = '+';
        }
    }
    void checkCollision( int x, int y ) {
        if( cursor.X == x && cursor.Y == y ) { 
            alive = false; brd[x + y * WID] = 'Å'; return; 
        }
        x = x + y * WID; 
        if( brd[x] == '*' || brd[x] == '+' || brd[x] == '~' ) {
            if( brd[x] != '*' ) { aliveRobots--; score++; }
            brd[x] = '*'; aliveRobots--; score++;
        }
    }
    winConsole* console; char brd[WID * HEI]; 
    int robotsCount, score, aliveRobots;
    coord cursor; bool alive;
};
winConsole* winConsole::inst = 0;
int main( int argc, char* argv[] ) {
    srand( ( unsigned )time( 0 ) );
    SetConsoleTitle( "Robots" );
    robots g; g.play(); return 0;
}

```

