+++
title = "Go Fish/C++"
description = ""
date = 2015-06-14T18:15:30Z
aliases = []
[extra]
id = 19260
[taxonomies]
categories = []
tags = []
+++

AI is really not that clever but it gets its job done (well, pretty much!).

It follows three simple rules:
* 75% the times it remembers cards asked by its opponent, and asks for the first one it also has.
* ask for a card it fished in the last round, if its different from all its other cards
* cycles thru all its cards, asking for them.

As I said, simple...



## C++


```cpp

#include <time.h>
#include <map>
#include <vector>
#include <algorithm>
#include <string>
#include <iostream>

const std::string s = "CDHS", v = "A23456789TJQK";
const int handCards = 9, drawCards = 3;

class card {
public:
    friend std::ostream& operator<< (std::ostream& os, const card& c ) { 
        os << v[c.val] << s[c.suit]; 
        return os;
    }
    bool isValid()                       { return val > -1; }
    void set( char s, char v )           { suit = s; val = v; }
    char getRank()                       { return v[val]; }
    bool operator == ( const char o )    { return v[val] == o; }
    bool operator < ( const card& a )    { if( val == a.val ) return suit < a.suit; return val < a.val; }
private:
    char                                 suit, val;
};
class deck {
public:
    static deck* instance() {
        if( !inst ) inst = new deck();
        return inst;
    }
    void destroy() {
        delete inst;
        inst = 0;
    }
    card draw() {
        card c;
        if( cards.size() > 0 ) { 
            c = cards.back();
            cards.pop_back();
            return c; 
        }
        c.set( -1, -1 );
        return c;
    }
private:
    deck() { 
        newDeck(); 
    }
    void newDeck() {
        card c; 
        for( char s = 0; s < 4; s++ ) {
            for( char v = 0; v < 13; v++ ) {
                c.set( s, v ); 
                cards.push_back( c ); 
            }
        }
        random_shuffle( cards.begin(), cards.end() );
        random_shuffle( cards.begin(), cards.end() );
    }
    static deck* inst;
    std::vector<card> cards;
};
class player {
public:
    player( std::string n ) : nm( n ) { 
        for( int x = 0; x < handCards; x++ )
            hand.push_back( deck::instance()->draw() );
        sort( hand.begin(), hand.end() );  
    }
    void outputHand() { 
        for( std::vector<card>::iterator x = hand.begin(); x != hand.end(); x++ ) 
            std::cout << ( *x ) << " ";
        std::cout << "\n"; 
    }
    bool addCard( card c ) { 
        hand.push_back( c );
        return checkForBook();
    }
    std::string name() { 
        return nm; 
    }
    bool holds( char c ) { 
        return( hand.end() != find( hand.begin(), hand.end(), c ) ); 
    }
    card takeCard( char c ) {
        std::vector<card>::iterator it = find( hand.begin(), hand.end(), c );
        std::swap( ( *it ), hand.back() );
        card d = hand.back();
        hand.pop_back();
        hasCards();
        sort( hand.begin(), hand.end() ); 
        return d;
    }
    size_t getBooksCount() {
        return books.size();
    }
    void listBooks() {
        for( std::vector<char>::iterator it = books.begin(); it != books.end(); it++ )
            std::cout << ( *it ) << "'s ";
        std::cout << "\n";
    }
    bool checkForBook() {
        bool ret = false;
        std::map<char, int> countMap;
        for( std::vector<card>::iterator it = hand.begin(); it != hand.end(); it++ )
            countMap[( *it ).getRank()]++;
        for( std::map<char, int>::iterator it = countMap.begin(); it != countMap.end(); it++ ) {
            if( ( *it ).second == 4 ) {
                do {
                    takeCard( ( *it ).first );
                } while( holds( ( *it ).first ) );
                books.push_back( ( *it ).first );
                ( *it ).second = 0;
                ret = true;
            }
        }
        sort( hand.begin(), hand.end() );
        return ret;
    }
    bool hasCards() {
        if( hand.size() < 1 ) {
            card c;
            for( int x = 0; x < drawCards; x++ ) {
                c = deck::instance()->draw();
                if( c.isValid() ) addCard( c );
                else break;
            }
        }
        return( hand.size() > 0 );
    }
protected:
    std::string nm; 
    std::vector<card> hand;
    std::vector<char> books;
};
class aiPlayer : public player {
public:
    aiPlayer( std::string n ) : player( n ), askedIdx( -1 ), lastAsked( 0 ), nextToAsk( -1 ) { }
    void rememberCard( char c ) {
        if( asked.end() != find( asked.begin(), asked.end(), c ) || !asked.size() )
            asked.push_back( c );  
    }
    char makeMove() {
        if( askedIdx < 0 || askedIdx >= static_cast<int>( hand.size() ) ) {
            askedIdx = rand() % static_cast<int>( hand.size() );
        }

        char c;
        if( nextToAsk > -1 ) {
            c = nextToAsk;
            nextToAsk = -1;
        } else {
            while( hand[askedIdx].getRank() == lastAsked ) {
                if( ++askedIdx == hand.size() ) {
                    askedIdx = 0;
                    break;
                }
            }
            c = hand[askedIdx].getRank();
            if( rand() % 100 > 25 && asked.size() ) {
                for( std::vector<char>::iterator it = asked.begin(); it != asked.end(); it++ ) {
		    if( holds( *it ) ) {
			c = ( *it );
			break;
		    }
		}
            }
        }
        lastAsked = c;
        return c;
    }
    void clearMemory( char c ) {
        std::vector<char>::iterator it = find( asked.begin(), asked.end(), c );
        if( asked.end() != it ) {
            std::swap( ( *it ), asked.back() );
            asked.pop_back();
        }
    }
    bool addCard( card c ) {
        if( !holds( c.getRank() ) )
            nextToAsk = c.getRank();
        return player::addCard( c );
    }
private:
    std::vector<char> asked;
    char nextToAsk, lastAsked;
    int askedIdx;
};
class goFish {
public:
    goFish() {
        plr = true; 
        std::string n; 
        std::cout << "Hi there, enter your name: "; std::cin >> n; 
        p1 = new player( n ); 
        p2 = new aiPlayer( "JJ" );
    }
    ~goFish() { 
        if( p1 ) delete p1; 
        if( p2 ) delete p2;
        deck::instance()->destroy();
    }
    void play() {
        while( true ) {
            if( process( getInput() ) ) break;
        }
        std::cout << "\n\n";
        showBooks();
        if( p1->getBooksCount() > p2->getBooksCount() ) {
            std::cout << "\n\n\t*** !!! CONGRATULATIONS !!! ***\n\n\n";
        } else {
            std::cout << "\n\n\t*** !!! YOU LOSE - HA HA HA !!! ***\n\n\n";
        }
    }
private:
    void showBooks() {
        if( p1->getBooksCount() > 0 ) {
            std::cout << "\nYour Book(s): ";
            p1->listBooks();
        }
        if( p2->getBooksCount() > 0 ) {
            std::cout << "\nMy Book(s): ";
            p2->listBooks();
        }
    }
    void showPlayerCards() {
        std::cout << "\n\n" << p1->name() << ", these are your cards:\n";
        p1->outputHand();
        showBooks();
    }
    char getInput() {
        char c;
        if( plr ) {
            if( !p1->hasCards() ) return -1;
            showPlayerCards();
            std::string w;
            while( true ) {
                std::cout << "\nWhat card(rank) do you want? "; std::cin >> w;
                c = toupper( w[0] );
                if( p1->holds( c ) ) break; 
                std::cout << p1->name() << ", you can't ask for a card you don't have!\n\n"; 
            }
        } else {
            if( !p2->hasCards() ) return -1;
            c = p2->makeMove();
            showPlayerCards();
            std::string r;
            std::cout << "\nDo you have any " << c << "'s? (Y)es / (G)o Fish ";
            do {
                std::getline( std::cin, r );
                r = toupper( r[0] );
            }
            while( r[0] != 'Y' && r[0] != 'G' );
            bool hasIt = p1->holds( c );
            if( hasIt && r[0] == 'G' )
                std::cout << "Are you trying to cheat me?! I know you do...\n";
            if( !hasIt && r[0] == 'Y' )
                std::cout << "Nooooo, you don't have it!!!\n";
        }
        return c;
    }
    bool process( char c ) {
        if( c < 0 ) return true;
        if( plr ) p2->rememberCard( c );

        player *a, *b;
        a = plr ? p2 : p1;
        b = plr ? p1 : p2;
        bool r;
        if( a->holds( c ) ) {
            while( a->holds( c ) ) {
                r = b->addCard( a->takeCard( c ) );
            }
            if( plr && r )p2->clearMemory( c );
        } else {
            fish();
            plr = !plr;
        }
        return false;
    }
    void fish() {
        std::cout << "\n\n\t  *** GO FISH! ***\n\n";
        card c = deck::instance()->draw();
        if( plr ) {
            std::cout << "Your new card: " << c << ".\n\n******** Your turn is over! ********\n" << std::string( 36, '-' ) << "\n\n";
            if( p1->addCard( c ) ) p2->clearMemory( c.getRank() );
        } else {
            std::cout << "\n********* My turn is over! *********\n" << std::string( 36, '-' ) << "\n\n";
            p2->addCard( c );
        }
    }

    player        *p1;
    aiPlayer    *p2;
    bool        plr;
};
deck* deck::inst = 0;
int main( int argc, char* argv[] ) {
    srand( static_cast<unsigned>( time( NULL ) ) ); 
    goFish f;  f.play(); 
    return 0;
}

```

