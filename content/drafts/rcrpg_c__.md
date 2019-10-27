+++
title = "RCRPG/C++"
description = ""
date = 2014-02-18T22:08:00Z
aliases = []
[extra]
id = 10597
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
== Commands ==
*''north, south, east, west, up, down'' moves.
*''n, s, e, w, u, d'' are aliases for the above.
*''attack <dir>'' to break down walls. You need to have a sledge equipped.
*''take <item>'' to take an item.
*''take all'' to take everything in the room.
*''drop <item>'' to drop an item.
*''drop all'' to drop everything.
*''equip <item>'' to equip something from your inventory.
*''wield <item>'' is an alias for equip.
*''unequip'' to return what you're wielding to your inventory.
*''name <string>'' to name a room.
*''alias <word> <word>'' to create an alias.
*''inventory, inv, i'' to see what you have.
*''look'' to see the full room description again.

== Code ==
{{works with | C++11}}
{{works with | Visual Studio 2010}}

```cpp>#include <map

#include <set>
#include <ctime>
#include <cctype>
#include <string>
#include <random>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <functional>
using namespace std;

mt19937 rnd_engine((unsigned long)time(NULL));
uniform_real_distribution<double> rnd_distribution;
auto rnd = bind(rnd_distribution, rnd_engine);

typedef function<void (stringstream&)> action;

map<string, action> actions;
map<string, string> unalias;

void canonical(string &s)
{
  transform(s.begin(), s.end(), s.begin(), tolower);
  
  while(unalias.find(s) != unalias.end())
    s = unalias[s];
}

class position
{
public:
  int x, y, z;    // north-south, east-west, up-down

  position(): x(0), y(0), z(0) {}

  bool operator<(const position &p) const
  {
    if(x < p.x)
      return true;
    if(x > p.x)
      return false;
    if(y < p.y)
      return true;
    if(y > p.y)
      return false;
    if(z < p.z)
      return true;
    if(z > p.z)
      return false;
    return false;
  }
};

position playerPos;

class directionSet
{
public:
  set<string> directions;

  bool isOpen(string dir) const
  {
    return directions.find(dir) != directions.end();
  }

  bool open(string dir)
  {
    if(directions.find(dir) != directions.end())
      return false;

    directions.insert(dir);
    return true;
  }

  vector<string> list() const
  {
    vector<string> out;
    set<string>::const_iterator iter;
    for(iter = directions.begin(); iter != directions.end(); ++iter)
      out.push_back(*iter);
    return out;
  }
};

class itemSet
{
public:
  map<string, int> counts;
  
  int count(string item)
  {
    if(counts.find(item) == counts.end())
      return 0;
    return counts[item];
  }

  void add(string item, int number = 1)
  {
    counts[item] += number;
  }

  bool remove(string item)
  {
    if(counts.find(item) == counts.end() || counts[item] == 0)
      return false;

    --counts[item];
    return true;
  }

  void clear()
  {
    counts.clear();
  }

  int transferAll(itemSet &other)
  {
    int total = 0;

    map<string, int>::iterator iter;
    for(iter = counts.begin(); iter != counts.end(); ++iter)
    {
      other.add(iter->first, iter->second);
      total += iter->second;
    }

    counts.clear();
    return total;
  }

  void describe(bool vertical = true) const
  {
    bool any = false;
    map<string, int>::const_iterator iter;
    for(iter = counts.begin(); iter != counts.end(); ++iter)
    {
      if(iter->second == 0)
        continue;

      any = true;

      if(vertical)
        cout << "  ";
      else if(iter != counts.begin())
        cout << ", ";

      if(iter->second == 1)
        cout << "a " << iter->first;
      else
        cout << iter->second << " " << iter->first << "s";

      if(vertical)
        cout << endl;
    }

    if(!any)
      if(vertical)
        cout << "  nothing" << endl;
      else
        cout << "nothing";
  }
};

int sledgeCount = 0;

class room
{
public:
  bool visited;
  itemSet items;
  directionSet exits;
  string name;

  room()
  {
    visited = false;
    name = "";

    if(rnd() < 0.25)
    {
      items.add("sledge");
      sledgeCount++;
    }
    if(rnd() < 0.1)
      items.add("ladder");
    while(rnd() < 0.1)
      items.add("gold");
  }

  void describe(bool full = true)
  {
    visited = true;
    cout << endl;

    if(full)
    {
      cout << "You are in a small room";

      if(name == "")
        cout << " at (" << playerPos.x << ", " << playerPos.y << ", " << playerPos.z << "). ";
      else
        cout << ". You remember this room as " << name << ". ";

      vector<string> e = exits.list();
      if(e.size() == 0)
        cout << "There are no exits here. ";
      else if(e.size() == 1)
        cout << "There is an exit to the " << e[0] << ". ";
      else
      {
        cout << "There are exits to the ";
        for(size_t i = 0; i < e.size(); ++i)
        {
          if(i == e.size() - 1)
            cout << " and ";
          else if(i != 0)
            cout << ", ";
          cout << e[i];
        }
        cout << ". ";
      }

      cout << "You see ";
      items.describe(false);
      cout << " on the floor." << endl;
    }
    else
    {
      if(name == "")
        cout << "(" << playerPos.x << ", " << playerPos.y << ", " << playerPos.z << ")" << endl;
      else
        cout << name << ":" << endl;

      vector<string> e = exits.list();
      if(e.size() == 0)
        cout << "No exits. ";
      else
      {
        cout << "Exits: ";
        for(size_t i = 0; i < e.size(); ++i)
        {
          if(i != 0)
            cout << ", ";
          cout << e[i];
        }
        cout << "." << endl;;
      }

      cout << "Items: ";
      items.describe(false);
      cout << "." << endl;
    }
  }
};

map<position, room> rooms;
itemSet playerInv;
string equipped = "";

void north(stringstream &ss)
{
  if(rooms[playerPos].exits.isOpen("north"))
  {
    playerPos.x++;
    rooms[playerPos].describe(!rooms[playerPos].visited);
  }
  else
    cout << "There's a wall in your way." << endl;
}

void south(stringstream &ss)
{
  if(rooms[playerPos].exits.isOpen("south"))
  {
    playerPos.x--;
    rooms[playerPos].describe(!rooms[playerPos].visited);
  }
  else
    cout << "There's a wall in your way." << endl;
}

void east(stringstream &ss)
{
  if(rooms[playerPos].exits.isOpen("east"))
  {
    playerPos.y++;
    rooms[playerPos].describe(!rooms[playerPos].visited);
  }
  else
    cout << "There's a wall in your way." << endl;
}

void west(stringstream &ss)
{
  if(rooms[playerPos].exits.isOpen("west"))
  {
    playerPos.y--;
    rooms[playerPos].describe(!rooms[playerPos].visited);
  }
  else
    cout << "There's a wall in your way." << endl;
}

void up(stringstream &ss)
{
  if(rooms[playerPos].items.count("ladder") == 0)
    cout << "There's no ladder here." << endl;
  else if(rooms[playerPos].exits.isOpen("up"))
  {
    playerPos.z++;
    rooms[playerPos].describe(!rooms[playerPos].visited);
  }
  else
    cout << "There's a ceiling in your way." << endl;
}

void down(stringstream &ss)
{
  if(rooms[playerPos].exits.isOpen("down"))
  {
    playerPos.z--;
    rooms[playerPos].describe(!rooms[playerPos].visited);
  }
  else
    cout << "There's a floor in your way." << endl;
}

void attack(stringstream &ss)
{
  if(equipped != "sledge")
  {
    cout << "You're not weilding a sledge." << endl;
    return;
  }

  string dir, opp = "";
  ss >> dir;
  canonical(dir);

  position otherPos = playerPos;

  if(dir == "north")
  {
    opp = "south";
    otherPos.x++;
  }
  else if(dir == "south")
  {
    opp = "north";
    otherPos.x--;
  }
  else if(dir == "east")
  {
    opp = "west";
    otherPos.y++;
  }
  else if(dir == "west")
  {
    opp = "east";
    otherPos.y--;
  }
  else if(dir == "up")
  {
    opp = "down";
    otherPos.z++;
  }
  else if(dir == "down")
  {
    opp = "up";
    otherPos.z--;
  }

  if(opp == "")
    cout << "You swing in a fifth direction, and hit nothing." << endl;
  else if(!rooms[playerPos].exits.open(dir))
    cout << "There's already a hole there." << endl;
  else
  {
    rooms[otherPos].exits.open(opp);

    if(dir == "up")
      dir = "ceiling";
    else if(dir == "down")
      dir = "floor";
    else
      dir += " wall";

    cout << "You hammer away at the " << dir << " and make a big hole." << endl;

    if(rnd() < 0.166 && sledgeCount > 1)
    {
      equipped = "";
      cout << "Your sledge breaks!" << endl;
      sledgeCount--;
    }
  }
}

void drop(stringstream &ss)
{
  string item;
  ss >> item;
  canonical(item);

  if(item == "all")
  {
    int n = playerInv.transferAll(rooms[playerPos].items);
    if(n == 0)
      cout << "You don't have anything to drop." << endl;
    else
      cout << "You drop all your stuff on the floor." << endl;
  }
  else if(!playerInv.remove(item))
    cout << "You don't have a " << item << "." << endl;
  else
  {
    rooms[playerPos].items.add(item);
    cout << "You drop a " << item << "." << endl;
  }
}

void take(stringstream &ss)
{
  string item;
  ss >> item;
  canonical(item);

  if(item == "all")
  {
    int n = rooms[playerPos].items.transferAll(playerInv);
    if(n == 0)
      cout << "You don't see anything to take." << endl;
    else
      cout << "You take everything not nailed down." << endl;
  }
  else if(!rooms[playerPos].items.remove(item))
    cout << "You don't see a " << item << " here." << endl;
  else
  {
    playerInv.add(item);
    cout << "You take a " << item << "." << endl;
  }
}

void inventory(stringstream &ss)
{
  cout << "You are weilding " << (equipped == "" ? "nothing" : "a " + equipped) << "." << endl;
  cout << "You are carrying:" << endl;
  playerInv.describe();
}

void name(stringstream &ss)
{
  string newName;
  getline(ss, newName);
  rooms[playerPos].name = newName.substr(1);
  cout << "You'll always remember this room as" << newName << "." << endl;
}

void equip(stringstream &ss)
{
  string item;
  ss >> item;
  canonical(item);

  if(!playerInv.remove(item))
    cout << "You don't have a " << item << " to equip." << endl;
  else if(equipped != "")
  {
    cout << "You unequip your " << equipped << " and wield a " << item << "." << endl;
    playerInv.add(equipped);
    equipped = item;
  }
  else
  {
    cout << "You equip a " << item << "." << endl;
    equipped = item;
  }
}

void unequip(stringstream &ss)
{
  if(equipped == "")
    cout << "You don't have anything equipped." << endl;
  else
  {
    cout << "You stop weilding the " << equipped << "." << endl;
    playerInv.add(equipped);
    equipped = "";
  }
}

void alias(stringstream &ss)
{
  string from, to;
  ss >> from >> to;

  if(actions.find(from) == actions.end() && unalias.find(from) == unalias.end() && 
    from != "gold" && from != "sledge" && from != "ladder")
    cout << "I don't understand " << from << "." << endl;
  else if(actions.find(to) != actions.end() || unalias.find(to) != unalias.end() || 
    to == "gold" || to == "sledge" || to == "ladder")
    cout << "Can't redefine words." << endl;
  else
  {
    unalias[to] = from;
    cout << "Ok." << endl;
  }
}

void look(stringstream &ss)
{
  rooms[playerPos].describe();
}

int main()
{
  actions["north"] = north;
  actions["south"] = south;
  actions["east"] = east;
  actions["west"] = west;
  actions["up"] = up;
  actions["down"] = down;
  actions["attack"] = attack;
  actions["drop"] = drop;
  actions["take"] = take;
  actions["equip"] = equip;
  actions["unequip"] = unequip;
  actions["inventory"] = inventory;
  actions["name"] = name;
  actions["alias"] = alias;
  actions["look"] = look;

  unalias["inv"] = "inventory";
  unalias["i"] = "inventory";
  unalias["n"] = "north";
  unalias["s"] = "south";
  unalias["e"] = "east";
  unalias["w"] = "west";
  unalias["u"] = "up";
  unalias["d"] = "down";
  unalias["wield"] = "equip";

  rooms[playerPos].items.clear();
  rooms[playerPos].items.add("sledge");
  sledgeCount = 1;

  cout << "Welcome to RCRPG, C++ edition. Find your way to (1, 1, 5) to get the treasure and win the game." << endl;

  stringstream dummy;
  look(dummy);

  while(true)
  {
    string input, command;

    cout << "> ";
    getline(cin, input);

    stringstream ss(input);
    ss >> command;

    canonical(command);

    if(actions.find(command) != actions.end())
      actions[command](ss);
    else if(command == "quit")
    {
      cout << "Bye." << endl;
      break;
    }
    else
      cout << "I don't understand." << endl;

    if(playerPos.x == 1 && playerPos.y == 1 && playerPos.z == 5)
    {
      cout << "You made it! And there's the treasure! It's beautiful!" << endl;
      break;
    }
  }
}
```

