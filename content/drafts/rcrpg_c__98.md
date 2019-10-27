+++
title = "RCRPG/C++98"
description = ""
date = 2015-09-11T05:52:38Z
aliases = []
[extra]
id = 19176
[taxonomies]
categories = []
tags = []
+++

Yet another C++ RCRPG, this one has some tweaks though:
* You can lose: If you try to break one of the cave's boundary walls, your sledge will break. If that happens in a room that has no exits (first room), you are trapped and dies!
* If you enter a room that you visited, at least 3 minutes ago, there is a 40% chance that the loot will re-spawn.
* The Treasure Room is placed at random in the cave.

## C++


```cpp

#include <stdio.h>
#include <time.h>
#include <iostream>
#include <algorithm>
#include <map>
#include <vector>
#include <sstream>

typedef unsigned uint;

const std::string directions[] = { "north", "south", "east", "west", "up", "down" };
const std::string objectName[] = { "nothing", "gold", "ladder", "sledge" };
const uint MAX_DOORS = sizeof( directions ) / sizeof( directions[0] ), MX_R = 26;

enum keyWord { north, south, east, west, up, down, inventory, unequip, look, help, quit, attack, drop, take, equip, alias, name, all, nothing, gold, ladder, sledge };
const keyWord inverted[] = { south, north, west, east, down, up };

class position
{
public:
    position() : x(0), y(0), z(0) { }
    position( int a, int b, int c ) : x(a), y(b), z(c) { }
    void set( int a, int b, int c ) {
         x = a; y = b; z = c;
    }
    bool operator ==( const position o ) { 
        return o.x == x && o.y == y && o.z == z; 
    }
    position& operator =( position o ) {
        x = o.x; y = o.y; z = o.z; 
        return *this;
    }
    position& operator +=( const position& o ) {
        x += o.x; y += o.y; z += o.z;
        return *this; 
    }
    friend position operator+( position l, const position& r ) {
        return l += r;
    }
    int    x, y, z;
};

const position dirVec[] = { position( 0, -1, 0 ), position( 0, 1, 0 ), position( 1, 0, 0 ), position( -1, 0, 0 ), position( 0, 0, -1 ), position( 0, 0, 1 ) };

typedef struct {
    keyWord action, subKey;
    std::string str1, str2;
}command;

typedef void ( *callee )( command );

class parser
{
public:
    parser() {
        std::string c[] = { directions[0], directions[1], directions[2], directions[3], directions[4], directions[5], "inventory", "unequip", 
            "look", "help", "quit", "attack", "drop", "take", "equip", "alias", "name", "all", objectName[0], objectName[1], objectName[2], objectName[3] };
        for( uint i = 0; i < sizeof( c ) / sizeof( c[0] ); i++ ) {
            actions.insert( std::make_pair( hashStr( c[i] ), static_cast<keyWord>( i ) ) );
            names.insert( std::make_pair( static_cast<keyWord>( i ), c[i] ) );
        }
    }
    void addAlias( std::string or, std::string nw ) {
        std::map<uint, keyWord>::iterator it = actions.find( hashStr( or ) );
        if( it == actions.end() ) { 
            std::cout << "Keyword " + or + " doesn't seem to exist!\n"; 
            return; 
        }
        actions.insert( std::make_pair( hashStr( nw ), it->second ) );
        std::cout << "Done\n";
    }
    bool parse( std::string a, command& cmd ) {
        std::transform( a.begin(), a.end(), a.begin(), ::tolower );
        std::istringstream iss( a ); 
        std::vector<std::string> vec;
        copy( std::istream_iterator<std::string>( iss ), std::istream_iterator<std::string>(), std::back_inserter<std::vector<std::string> >( vec ) );
        std::map<uint, keyWord>::iterator it = actions.find( hashStr( vec[0] ) );
        if( it == actions.end() ) { 
            std::cout << "'" + vec[0] + "' doesn't make any sense!\n"; 
            return false; 
        }
        cmd.action = it->second; 
        if( cmd.action < attack ) return  true;
        if( cmd.action < name ) {
            if( vec.size() < 2 ) { 
                std::cout << "What should I '" + names[cmd.action] + "'?\n"; 
                return false; 
            }
            if( !vec[1].compare( "coin" ) || !vec[1].compare( "coins" ) ) vec[1] = "gold";
            it = actions.find( hashStr( vec[1] ) );
            if( it == actions.end() ) { 
                std::cout << "I don't know what '" + vec[1] + "' means.\n"; 
                return false; 
            }
            cmd.subKey = it->second;
            if( cmd.action == alias ) {
                if( vec.size() < 3 ) { 
                    std::cout << "Aren't you forgetting something?\n"; 
                    return false; 
                }
                std::map<uint, keyWord>::iterator it = actions.find( hashStr( vec[2] ) );
                if( it != actions.end() ) { 
                    std::cout << "'" + vec[2] + "' is already a keyword!\n"; 
                    return false; 
                }
                if( cmd.subKey > name ) { 
                    std::cout << "No alias for '" + vec[1] + "' is allowed!\n"; 
                    return false; 
                }
                cmd.str1 = vec[1]; 
                cmd.str2 = vec[2];
            }
            return true;
        }
        if( cmd.action == name ) { 
            if( vec.size() < 2 ) { 
                std::cout << "What name?\n"; 
                return false; 
            }
            cmd.str1 = vec[1]; 
            return true; 
        }
        std::cout << "I can't understand '" + vec[0] + "'!\n"; 
        return false;
    }
private:
    unsigned hashStr( std::string s ) {
        unsigned wrd = 0x4e67c6a7, p = 0;
        while( p < s.length() ) 
            wrd ^= ( ( wrd << 5 ) + s[p++] + ( wrd >> 2 ) );
        return wrd;
    }
    std::map<uint, keyWord> actions;
    std::map<keyWord, std::string> names;
};

class treasure
{
public:
    treasure( keyWord k, uint c ) : key(k), cnt(c) { }
    keyWord key;
    uint cnt;
};

class box
{
public:
    void display() {
        if( !stuff.size() ) { 
            std::cout << objectName[0] + ".\n\n"; 
            return; 
        }
        std::ostringstream oss;
        for( std::map<keyWord, uint>::iterator it = stuff.begin(); it != stuff.end(); it++ ) {
            if( it->second > 0 ) {
                oss << " + " << it->second << " " << objectName[it->first - nothing]; std::cout << oss.str();
                if( it->first == gold ) std::cout << ( it->second > 1 ? " coins" : " coin" );
                else std::cout << ( it->second > 1 ? "s" : "" );
                std::cout << "\n"; 
                oss.str( "" );
            }
        }
        std::cout << "\n";
    }
    void stow( std::vector<treasure> t ) {
        for( std::vector<treasure>::iterator i = t.begin(); i != t.end(); i++ ) {
            std::map<keyWord, uint>::iterator it = stuff.find( ( *i ).key );
            if( it == stuff.end() ) stuff.insert( std::make_pair( ( *i ).key, ( *i ).cnt ) ); 
            else it->second += ( *i ).cnt;
        }
    }
    std::vector<treasure> dump( keyWord k, uint c = 0 ) {
        std::vector<treasure> t;
        if( k == all ) {
            for( std::map<keyWord, unsigned>::iterator it = stuff.begin(); it != stuff.end(); it++ ) {
                t.push_back( treasure( ( *it ).first, ( *it ).second ) );
            }
            stuff.clear();
        }
        else {
            std::map<keyWord, unsigned>::iterator it = stuff.find( k );
            if( it == stuff.end() ) return t;
            uint z = ( it->second ); 
            c = k == ladder ? 1 : !c ? z : c;
            it->second -= c; 
            if( !it->second ) stuff.erase( it );
            t.push_back( treasure( k, c ) );
        }
        return t;
    }
    size_t size() {
        return stuff.size();
    }
    uint count( keyWord k ) {
        std::map<keyWord, unsigned>::iterator it = stuff.find( k );
        if( it != stuff.end() ) return ( *it ).second;
        return 0;
    }
private:
    std::map<keyWord, uint> stuff;
};

class room
{
public:
    room( std::string n ) : name(n), visited(false) {
        memset( bRooms, 0, sizeof( bRooms ) ); 
        memset( ladderZ, 0, sizeof( ladderZ ) ); 
    }
    void describe() {
        std::cout << "\n\t** Room: " + name + " **\n\n"; 
        std::vector<std::string> ex; 
        getExits( ex );
        if( !ex.size() ) std::cout << "There are no exits from this room.\n";
        else {
            std::cout << "From here, you can go ";
            for( std::vector<std::string>::iterator x = ex.begin(); x != ex.end(); x++ ) {
                std::cout << *x + " ";
            }
            std::cout << "\n";
        }
        if( loot.size() ) {
            std::cout << "Here you can see:\n"; 
            loot.display();
            return;
        }
        std::cout << "You see nothing useful here.\n"; 
    }
    room* atk( keyWord d, uint lvl ) {
        if( bRooms[d] ) return 0;
        room* r = new room( "no name" ); 
        bRooms[d] = r->bRooms[inverted[d]] = true;
        r->addLoot( false, lvl );
        return r;
    }
    void addLoot( bool forceSledge, uint lvl ) {
        std::vector<treasure> t;
        if( forceSledge ) t.push_back( treasure( sledge, 1 ) );
        else if( rand() % 10 > 6 )  t.push_back( treasure( sledge, 1 ) );
        if( rand() % 10 > 6 ) t.push_back( treasure( gold, rand() % 8 + 1 ) );
        if( rand() % 10 == 2 || ( !ladderZ[lvl] && rand() % 10 < 5 ) ) {
            t.push_back( treasure( ladder, 1 ) ); 
            ladderZ[lvl] = true;
        }
        loot.stow( t );
    }
    void setName( std::string n ) {
        name = n;
        std::cout << "OK\n";
    }
    std::vector<treasure> tak( keyWord k ) {
        return loot.dump( k );
    }
    void drp( std::vector<treasure> t ) {
        loot.stow( t );
    }
    uint count( keyWord k ) {
        return loot.count( k );
    }
    void setVisited() {
        visited = true;
        lastTime = time( 0 );
    }
    bool wasVisited() {
        return visited;
    }
    time_t lastTimeVisited() {
        return lastTime;
    }
    void getExits( std::vector<std::string>& ex ) {
        for( uint x = 0; x < MAX_DOORS; x++ )
            if( bRooms[x] )
                ex.push_back( directions[x] );
    }
private:
    std::string    name;
    box loot;
    time_t lastTime;
    bool bRooms[MAX_DOORS], ladderZ[MX_R], visited;
};

class player
{
public:
    player() : curRoom(0) { }
    ~player() {
        deleteRooms();
    }
    void init() {
        equipped = nothing; deleteRooms();
        curRoom = new room( "Where it all begins" ); 
        curRoom->setVisited(); 
        curRoom->addLoot( true, pos.z );
        do pos.set( rand() % MX_R, rand() % MX_R, rand() % MX_R );
        while( pos.x == 8 && pos.y == 8 && pos.z == 8 ); 
        cave.insert( std::make_pair( pos.x + pos.y * MX_R + MX_R * MX_R * pos.z, curRoom ) );
        lok();
    }
    void lok() {
        curRoom->describe();
        if( equipped > nothing ) std::cout << "You are equipped with a " << objectName[equipped - nothing] << "\n";
    }
    void une() {
        if( equipped == nothing ) { 
            std::cout << "You are equipped with nothing!\n"; 
            return; 
        }
        equipped = nothing;
        std::cout << "Done\n";
    }
    void inv() {
        std::cout << "You are carrying"; 
        std::cout << ( !loot.size() ? " " : ":\n" );
        loot.display();
    }
    void tak( keyWord k ) {
        if( k < gold && k != all ) { 
            std::cout << "I don't know how to do that!\n"; 
            return; 
        }
        if( k == ladder && loot.count( k ) ) { 
            std::cout << "These things are so heavy, you can't carry more than one!\n"; 
            return; 
        }
        std::vector<treasure> t = curRoom->tak( k );
        if( !t.size() ) {
            if( k == all ) std::cout << "There is nothing here to take!\n";
            else std::cout << "I can't take what's not here!\n";
            return;
        }
        loot.stow( t );
        if( k == all ) {
            uint lc = loot.count( ladder );
            while( lc > 1 ) {
                drp( ladder, true );
                lc--;
            }
        }
        std::cout << "Taken\n";
    }
    void equ( keyWord k ) {
        if( k < sledge ) { 
            std::cout << "I don't know how to equip this!\n"; 
            return; 
        }
        if( equipped != nothing ) return;

        if( !loot.count( k ) ) { 
            std::cout << "You are not carrying one of those."; 
            return; 
        }
        equipped = k;
        std::cout << "OK\n";
    }
    bool atk( keyWord k ) {
        if( equipped == nothing ) { 
            std::cout << "I cannot let you hurt yourself!\n"; 
            return true; 
        }
        if( checkBoundaries( k ) ) { 
            std::cout << "The rocks here are too hard, you broke your sledge!\n"; 
            equipped = nothing; 
            std::vector<std::string> ex;
            curRoom->getExits( ex );
            if( !ex.size() ) {
                std::cout << "\nUnfortunately you are trapped in here ---- FOREVER!\n\n"
                             "\t*** G A M E * O V E R ***\n\n";
                return false;
            }
            return true; 
        }
        uint lvl = pos.z + k == up ? -1 : k == down ? 1 : 0;
        room* r = curRoom->atk( k, lvl );
        if( !r ) { 
            std::cout << "There is already a doorway there!\n"; 
            return true; 
        }
        position p = pos + dirVec[k]; 
        cave.insert( std::make_pair( p.x + MX_R * p.y + MX_R * MX_R * p.z, r ) );
        std::cout << "KA-POW!\n";
    }
    void mov( keyWord d ) {
        if( d > down ) { 
            std::cout << "I don't know how to move in that direction!\n"; 
            return; 
        }
        if( d == up && loot.count( ladder ) ) { 
            std::cout << "You are not strong enough to climb up there carrying a ladder!\n"; 
            return; 
        }
        position p = pos + dirVec[d];
        std::map<uint, room*>::iterator it = cave.find( p.x + MX_R * p.y + MX_R * MX_R * p.z );
        if( it == cave.end() ) {
            std::cout << "There is a "; 
            if( d == up ) std::cout << "ceiling";
            else if( d == down ) std::cout << "floor"; 
            else std::cout << "wall";
            std::cout << " blocking your way!\n\n"; 
        }
        else {
            curRoom = it->second; 
            pos = p;
            if( d == up && !curRoom->count( ladder ) ) { 
                std::cout << "I didn't know you could fly!\n"; 
                return; 
            }
            if( curRoom->wasVisited() ) {
                if( rand() % 10 > 5 && difftime( time( 0 ), curRoom->lastTimeVisited() ) > 180 ) curRoom->addLoot( false, pos.z );
            }
            curRoom->setVisited();
        }
    }
    void drp( keyWord k, bool silence = false ) {
        curRoom->drp( loot.dump( k ) );
        if( !silence ) std::cout << "Dropped\n"; 
    }
    room* currentRoom() {
        return curRoom;
    }
    position getPos() {
        return pos;
    }
private:
    void deleteRooms() {
        for( std::map<uint, room*>::iterator it = cave.begin(); it != cave.end(); it++ )
            delete it->second;
        cave.clear();
    }
    bool checkBoundaries( keyWord k ) {
        return k == up && pos.z - 1 < 0 || k == down && pos.z + 1 >= MX_R || k == west && pos.x - 1 < 0 || 
               k == east && pos.x + 1 >= MX_R || k == north && pos.y - 1 < 0 || k == south && pos.y + 1 >= MX_R;
    }
    room* curRoom;
    box loot;
    position pos;
    keyWord equipped;
    std::map<uint, room*> cave;
};

class game
{
public:
    game() {
        instance = this; 
        goal.set( 3, 3, 3 );
        callee c[] = { mov, mov, mov, mov, mov, mov, inv, une, lok, hlp, qit, atk, drp, tak, eqp, als, nam, 0, 0, 0, 0 };
        for( uint i = 0; i < sizeof( c ) / sizeof( c[0] ); i++ )
            functions.insert( std::make_pair( static_cast<keyWord>( i ), c[i] ) );
    }
    void play() {
        command cmdLine; 
        std::string a;
        while( true ) {
            initGame();
            while( !gameOver ) {
                std::cout << "\n>"; 
                std::getline( std::cin, a ); 
                if( !cmdParser.parse( a, cmdLine ) ) continue;
                std::map<keyWord, callee>::iterator it = functions.find( cmdLine.action );
                if( it == functions.end() ) { 
                    std::cout << "What do you mean?"; 
                    continue; 
                }
                it->second( cmdLine );
            }
            if( playerQuit ) return;
            std::cout << "Play again (Y/N)?\n>";
            std::getline( std::cin, a ); 
            if( a[0] != 'y' && a[0] != 'Y' ) return;
        }
    }
private:
    void checkWin() {
        if( plr.getPos() == goal ) {
            std::cout << "\nYou have found the ** TREASURE ROOOM **\n\n"
                "\nAll around you are thousands and thousands of piles of gold\n\n ** and they are all yours! ** \n"
                "\n\nCONGRATULATIONS!!!\n\n\n\n";
            gameOver = true; 
            playerQuit = false;
            return;
        }
        plr.currentRoom()->describe();
    }
    void initGame() {
        gameOver = false; 
        playerQuit = true;
        plr.init();
    }
    void showHelp() {
        std::cout << "\n ** Welcome Dungeon Explorer **\n\nDig your way to Room 8, 8, 8! (...but where is it?)\n\n"
                     "In your journey you can use following commands:\n\n* north, south, east, west, up and down: move in that direction\n"
                     "* attack <direction>: if equipped with sledge, it will open a passage\n  in that direction\n"
                     "* drop <object>: drops the object in the room you are\n* take <object>: takes a object from room into you inventory\n"
                     "* inventory: list all things you are carrying\n* look: describes the room you are in\n""* equip <object>: equip you "
                     "with the object\n* unequip: the opposite of equip\n* name <new name>: rename the room you are in\n"
                     "* alias <command> <new name>: creates a alias for the command\n* quit: terminates the game\n* help: show this long text...\n\n"
                     "  ** To go upwards, you need a ladder! **\n\n";
    }
    static void atk( command c ) { 
        if( !instance->plr.atk( c.subKey ) ) {
            instance->gameOver = true;
            instance->playerQuit = false;
        }
    }
    static void mov( command c ) { instance->plr.mov( c.action ); instance->checkWin(); }
    static void inv( command c ) { instance->plr.inv(); }
    static void une( command c ) { instance->plr.une(); }
    static void lok( command c ) { instance->plr.lok(); }
    static void drp( command c ) { instance->plr.drp( c.subKey ); }
    static void tak( command c ) { instance->plr.tak( c.subKey ); }
    static void eqp( command c ) { instance->plr.equ( c.subKey ); }
    static void nam( command c ) { instance->plr.currentRoom()->setName( c.str1 ); }
    static void als( command c ) { instance->cmdParser.addAlias( c.str1, c.str2 ); }
    static void hlp( command c ) { instance->showHelp(); }
    static void qit( command c ) { instance->gameOver = true; }

    bool gameOver, playerQuit;
    std::map<keyWord, callee> functions;
    parser cmdParser;
    player plr;
    position goal;
    static game* instance;
};

game* game::instance = 0;
int main( int argc, char* argv[] )
{
    srand( static_cast<uint>( time( NULL ) ) );
    game g; g.play();
    return 0;
}

```

