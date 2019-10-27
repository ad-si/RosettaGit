+++
title = "Dragon curve/D/QD"
description = ""
date = 2009-11-20T18:38:29Z
aliases = []
[extra]
id = 4992
[taxonomies]
categories = []
tags = []
+++

{{libheader|QD}}

```d
module lsystem;

import qd;

interface LSystemI {
  void setCallback(void delegate(int, int) cb);
  void BaseStep(int depth);
  int x(); int y();
  int x(int); int y(int);
}

class LSystem(int ANGLES) : LSystemI {
  int angle, _x, _y;
  // define as properties so they can appear in the interface description
  int x() { return _x; }
  int y() { return _y; }
  int x(int i) { return _x = i; }
  int y(int i) { return _y = i; }
  
  static assert(ANGLES == 4 || ANGLES == 8, "Unsupported number of angles!");
  void right() { angle ++; if (angle == ANGLES) angle = 0; }
  void left() { angle --; if (angle == -1) angle = ANGLES - 1; }
  
  int stepsize;
  void delegate(int, int) dgPoint;
  void setCallback(typeof(dgPoint) cb) { dgPoint = cb; }
  
  void step() {
    static if (ANGLES == 4) {
      //  0
      // 3 1
      //  2
      const xshift = [0, 1, 0, -1];
      const yshift = [-1, 0, 1, 0];
    } else {
      // 701
      // 6 2
      // 543
      const xshift = [-1, 0, 1, 1, 1, 0, -1, -1];
      const yshift = [-1, -1, -1, 0, 1, 1, 1, 0];
    }
    auto
      newx = x + xshift[angle] * stepsize,
      newy = y + yshift[angle] * stepsize;
    dgPoint(newx, newy);
    x = newx; y = newy;
  }
  abstract void BaseStep(int depth);
}

import tools.base;
// compile-time function
string LSysFunc(string syntax) {
  auto namepos = syntax.ctFind("->");
  assert(namepos != -1, "LSystem function definition syntax: Varname -> Replacement");
  auto name = syntax[0 .. namepos].ctStrip(); syntax = syntax[namepos+2 .. $].ctStrip();
  auto endpos = syntax.ctFind("/");
  string endact;
  if (endpos != -1) {
    endact = syntax[endpos+1 .. $].ctStrip();
    syntax = syntax[0 .. endpos].ctStrip();
  }
  if (endact.length && endact[$-1] != ';') endact ~= ';'; // terminate statement
  string res = "void "~name~"(int depth) { if (!depth) { "~endact~" return; } ";
  foreach (ch; syntax) {
    if (ch == '+') res ~= "right; ";
    else if (ch == '-') res ~= "left; ";
    else if (ch == 'F') res ~= "step; ";
    else res ~= ch~"(depth - 1); ";
  }
  res ~= " }";
  return res;
}

class Heighway : LSystem!(4) {
  mixin(LSysFunc("X -> X+YF+"));
  mixin(LSysFunc("Y -> -FX-Y"));
  override void BaseStep(int depth) { X(depth); }
}

class Lévy : LSystem!(8) {
  mixin(LSysFunc("X -> +X--X+ / step"));
  override void BaseStep(int depth) { X(depth); }
}

void drawSystemFade(rgb start, rgb end, LSystemI ls, int depth) {
  with (ls) {
    int numPoints;
    // acquire number of points
    setCallback = (int x, int y) { numPoints ++; };
    BaseStep(depth);
    // draw with fade from red to green
    int count;
    setCallback = (int newx, int newy) {
      auto color = start.blend(end, count * 1.0 / numPoints);
      line(x, y, newx, newy, color);
      count ++;
    };
    BaseStep(depth);
  }
}

import tools.time: sleep;
void main() {
  screen(640, 480);
  auto h = new Heighway;
  h.x = screen.width * 5 / 8;
  h.y = screen.height * 5 / 6;
  h.stepsize = 1;
  drawSystemFade(Red, Green, h, 16);
  auto l = new Lévy;
  l.x = screen.width * 1 / 6;
  l.y = screen.height * 5 / 6;
  l.stepsize = 1;
  l.right;
  drawSystemFade(Red, Green, l, 14);
  while (true) { flip; events; }
}
```


Screenshot: [http://img14.imageshack.us/img14/781/lsystemwt8.png]
