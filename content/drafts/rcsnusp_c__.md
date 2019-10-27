+++
title = "RCSNUSP/C++"
description = ""
date = 2015-06-26T03:57:00Z
aliases = []
[extra]
id = 19247
[taxonomies]
categories = []
tags = []
+++

This is a simple C++ implementation of Core and Modular SNUSP, Bloated will (maybe) come in the future.

To execute the SNUSP program just start the interpreter with its file name as parameter, like so: interpreter name SNUSP program. 

It has a simple Log feature, to use it type: interpreter name /L SNUSP program.

Input and output are thru Console. 


## C++


```cpp

#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <iomanip>

#define msg0 "|             *** end of code ***              |"
#define msg1 "|            *** stack is empty ***            |"
#define msg2 "|               *?* step out *?*               |"
#define msg3 "|               *!* step out *!*               |"
#define msg4 "|            *** memory blows up ***           |"

typedef unsigned char u8;

enum direction {
    UP, DOWN, LEFT, RIGHT
};

class stackFrame {
public:
    stackFrame() { }
    stackFrame( int ipc, int ipl, direction dir) :
    IPC( ipc ), IPL( ipl ), DIR( dir ) { }
    int IPC, IPL;
    direction DIR;
} ;

class SNUSP {
public:
    SNUSP() : IPC( 0 ), IPL( 0 ), MPT( 0 ), DIR( RIGHT ) { 
        memory.push_back( 0 ); 
    }

    ~SNUSP() { 
        memory.clear();
        code.clear();
    }

    void run( std::string filename, bool lg ) {
        logging = lg;
        if( logging ) {
            std::ofstream o( "log.txt", std::ios_base::trunc );
            strm << "+------+-------+-------+-------+-------+-------+\n| INST |  IPC  |  IPL  |  MPT  |  DIR  |  MEM  |";
            log( strm.str() );
        }

        if( !openFile( filename ) ) {
            std::cout << "Cannot open file '" << filename << "'\n";
            return;
        }

        while( execute( code[IPL][IPC] ) );
        std::cout << "\n\n" << memory[MPT];
    }
private:
    bool execute( u8 u ) {
        stackFrame sf;
        switch( u ) {
            case '<':
                if( --MPT < 0 ) {
                    if( logging ) log( msg4 );
                    return false;
                }
            break;
            case '>':
                if( ++MPT >= static_cast<int>( memory.size() ) ) { memory.push_back( 0 ); }
            break;
            case '+':
                memory[MPT]++;
            break;
            case '-':
                memory[MPT]--;
            break;
            case ',':
                std::cin >> memory[MPT];
            break;
            case '.':
                std::cout << static_cast<u8>( memory[MPT] );
            break;
            case '!':
                if( step() ) {
                    if( logging ) log( msg3 );
                    return false;
                }
            break;
            case '?':
                if( !memory[MPT] ) {
                    if( step() ) {
                        if( logging ) log( msg2 );
                        return false;
                    }
                }
            break;
            case '@':
                stack.push_back( stackFrame( IPC + ( DIR == RIGHT ? 1 : DIR == LEFT ? -1 : 0 ), 
                                             IPL + ( DIR == DOWN  ? 1 : DIR == UP   ? -1 : 0 ), DIR ) );
            break;
            case '#':
                if( !stack.size() ) {
                    if( logging ) log( msg1);
                    return false;
                }
                sf = stack.back();
                stack.pop_back();
                IPC = sf.IPC; IPL = sf.IPL; DIR = sf.DIR;
            break;
            case '/':
                DIR = DIR == RIGHT ? UP : DIR == LEFT ? DOWN : DIR == DOWN ? LEFT : RIGHT;
            break;
            case '\\':
                DIR = DIR == RIGHT ? DOWN : DIR == LEFT ? UP : DIR == DOWN ? RIGHT : LEFT;
            break;
        }

        if( logging ) {
            strm << "|" << std::setw( 5 ) << u  << " |" << std::setw( 6 ) << IPC << " |" << std::setw( 6 ) 
            << IPL << " |" << std::setw( 6 ) << MPT << " |" << std::setw( 6 ) << DIR << " |" << std::setw( 6 )           << memory[MPT] << " |";
            log( strm.str() );
        }

        if( step() ) {
            if( logging ) log( msg0 );
            return false;
        }

        return true;
    }

    bool step() {
        IPC += DIR == RIGHT ? 1 : DIR == LEFT ? -1 : 0;
        IPL += DIR == DOWN  ? 1 : DIR == UP   ? -1 : 0;
        return ( IPL >= static_cast<int>( code.size() ) || IPL < 0 || 
                 IPC >= static_cast<int>( code[IPL].length() ) || IPC < 0 );
    }

    bool openFile( std::string filename ) {
        std::ifstream in;
        in.open( filename.c_str() );
        if( !in.good() ) {
            return false;
        }

        std::string line;
        size_t max_len = 0, len;
        while( std::getline( in, line ) ) {
            len = line.length();
            if( max_len < len ) max_len = len;
            code.push_back( line );
            if( !IPC && !IPL ) {
                std::size_t i = static_cast<int>( line.find( '$' ) );
                if( i < line.npos ) {
                    IPL = code.size() - 1;
                    IPC = i;
                }
            }
        }
        in.close();

        for( std::vector<std::string>::iterator i = code.begin(); i != code.end(); i++ ) {
            if( ( *i ).length() < max_len ) ( *i ).insert( ( *i ).end(), max_len - ( *i ).length(), ' ' );
        }
        return true;
    }

    void log( std::string msg )
    {
        std::ofstream logDegug( "log.txt", std::ios_base::out | std::ios_base::app );
        logDegug << msg << "\n+------+-------+-------+-------+-------+-------+" << std::endl;
        strm.str( "" );
    }

    std::vector<std::string>            code;
    std::vector<stackFrame>             stack;
    std::vector<int>                    memory;
    std::stringstream                   strm;
    int                                 IPC, IPL, MPT;
    direction                           DIR;
    bool                                logging;
};

int main( int argc,  char* argv[] ) {
    if( argc < 2 ) {
        std::cout << "\nDoh!!!\n\nUsage:\t[/L] Filename\n\n\t/L\tLog each command to a log file";
    } else {
        SNUSP s;
        s.run( argv[argc == 3 ? 2 : 1], ( argv[1][0] == '/' && toupper( argv[1][1] ) == 'L' ) );
    }
    std::cout << "\n\n";
    return 0;
}

```

