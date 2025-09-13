+++
title = "Execute SNUSP/D"
description = ""
date = 2010-02-06T14:33:37Z
aliases = []
[extra]
id = 2851
[taxonomies]
categories = []
tags = []
+++



This [D](https://rosettacode.org/wiki/D) implementation supports commands from all the three SNUSP variants, as described on the [Esolang SNUSP page](https://rosettacode.org/wiki/eso:SNUSP), plus an extended mode, '''SUPERNATURAL'''.

'''SUPERNATURAL Mode''':
*'''~ : code input''' (materialization):
#Read the char in current code pointer as input, assign it to memory currently pointed to by memory pointer.
*'''* : join and wait tasks''' (telepathy):
#A task has a state property of free/join/wait;
#A task is by default initialized as free-state; 
#The splited new thread inherites free/join/wait-state from the old thread;
#When a free-state task first executes this '''*''' command, the task enters into a join-state; 
#Then if a join-state task executes another '''*''' command, the task enters into a wait-state;
#A wait-state task stops its execution, and waits until all alive join-state tasks are turning into a wait-state, which then all these wait-state tasks are return to free-state;
#This command enables global synchronization of the tasks.
#Difference to original specification:
::*The original specification does not require threads execution in a specific order. For this command to be useful, the order of execution of tasks(threads) becomes important;
::*In this implementation, the order of execution is first-created-first-executed;
::*The original specification specifies that ''(&)SPLIT''-ed old thread skips the immediate code (see below a->b->c->d example), which may lead to anti-intuition codes (which is good for an esoteric language :). This implementation retain old-thread-skips-immediate-code behavior in ''BLOATED'' mode, but new-thread-skips-immediate-code in ''SUPERNATURAL'' mode ( A->B->C->D example). 1->2->...->8 is thread creation order in ''SUPERNATURAL'' mode.
:::<tt style="line-height: 1em;">$*&\==&\:&\:&\:=\
   ;   ~  ~  ~  ~ &\=> new(B)
   ;  2A 3B 4C 5D  \=> old(A)
   ;   \=!\=!\=!\===*..=.#
   ;   /=!/=!/=!/===*.=.=.#
   ;  1b 6c 7d 8a &\=> old(a)
   ;   ~  ~  ~  ~  \=> new(b)
   \==&/;&/;&/;=/  
</tt>
*'''^ : warp''' (teleport):
#The code pointer will bounce back to the code space boundary in its reverse direction; 
#then forward and stop after the first '''^''' it encounter in normal direction.
#Example : 
:::==a^A&lt;=cp==B^==&lt;=C^==
::when the code pointer cp heading into A's^, next turn, the code pointer will be in C. if no such ^ in the reverse direction, the code pointer will be in ''a'' next turn.

```d
module snud ;
private import std.string, std.random ;

// io interface, which has to be defined in another module
interface IIO {
  void setDebugInput(string s) ;
  void output(int v) ;
  bool inputReady() ;
  int input() ;
}

int rnd(int b)  { return b < 0 ? (-rand()) % (-b + 1) : rand() % (b + 1) ; }
// simple stack template
void push(T)(inout T[] stk, T value) { stk ~= value ; }
T pop(T)(inout T[] stk, bool discard = true) {
  T top = stk[$-1] ;
  if(discard) stk.length = stk.length - 1 ;
  return top ;
}

// a 2x tuple type
struct X(U,V = U) {
    U x ; V y ;
    void to(ref U a, ref V b) { a = x ; b = y ; }
    void from(U a, V b) { x = a ; y = b ; }
}
alias X!(int) I2 ; // intxint, used as code pointer, memory pointer & direction
alias X!(I2)  ST ; // (intxint)x(intxint), used as cpu state [cp,dp]

enum Mode : uint {CORE = 1, MODULAR, BLOATED, SUPERNATURAL } ;

// used to locate '$' and debugInput
string pfind(I2* loc, string[] c, string sl, string sr = null) {
  int rx, dir =  1 , start = 0 , end = c.length - 1 ;
  if(sr){ dir = -1 ; start = c.length - 1 ; end = 0 ; }
  with(*loc)
    for(x = -1, y = start ; y*dir <= end*dir ; y += dir)
      if((x = std.string.find(c[y], sl)) >= 0) {
        if(sr && (rx = std.string.rfind(c[y], sr)) >= 0)
          if(rx > x + sl.length)
            return c[y][x + sl.length..rx] ;
        break ;
      }
  return null ;
}

// a 2d memory space
final class Memory {
  private int[I2] cells ;
  void reset() { foreach(k, v ; cells) cells[k] = 0 ; }
  void opIndexAssign(int value, I2 key) { cells[key] = value ; }
  int opIndex(I2 key) {
    int* vp = key in cells ;  // get value pointer of the key, null if no such key
    if(vp is null) { cells[key] = 0 ; return 0 ; } // initialize the value/key pair
    return *vp ;              // return the already existed value
  }
}

final class CPU {
  final class Task {
    enum {FREE, JOINED, WAITING }
    const int id ;
    I2 cp, dp, mp ;
    int join = FREE ;
    bool quit = false ;
    private ST[] stack ;
    private char curCode ;

    this(I2 Cp, I2 Dp, I2 Mp, int joinstate = FREE) { 
      cp = Cp ; dp = Dp ; mp = Mp ; id = Id++ ; 
      if((join = joinstate) == JOINED) joinwait++ ;
    }

    private void fwd(int step = 1) { with(cp) from(x + dp.x*step, y + dp.y*step) ; }
    private void rfx(int dir) { with(dp) from(dir*y, dir*x) ; }
    // _outer_ is D keyword for an inner class to ref outer class
    private bool hasCode() { return this.outer.hasCode(cp) ; }
    char getCode() { return this.outer.getCode(cp) ; }
    Task execute() {
      curCode = getCode ;
      if(curCode in acceptCmd) // this control which SNUSP variants is used
      switch(curCode) {
        case '<' : mp.x-- ; break ;
        case '>' : mp.x++ ; break ;
        case ':' : mp.y-- ; break ;
        case ';' : mp.y++ ; break ;
        case '+' : m[mp] = m[mp] + 1 ; break ;
        case '-' : m[mp] = m[mp] - 1 ; break ;
        case ',' :
          if(!io.inputReady()) goto RET ; // wait input
          else m[mp] = io.input() ; break ;
        case '.' : io.output(m[mp]) ; break ;
        case '!' : fwd ; break ;
        case '?' : if(m[mp] == 0) fwd ; break ;
        case '%' : m[mp] = rnd(m[mp]) ; break ;
        case '\\': rfx( 1) ; break ;
        case '/' : rfx(-1) ; break ;
        case '@' : stack.push(ST(cp, dp)) ; break ; // save caller state
        case '#' :
          if(stack.length > 0) // return from subroutine
            { stack.pop().to(cp,dp) ; fwd ; break ; }
        case '\0':             // else process as \0, = quit
          quit = true ; goto RET ;
        case '&' :
          if(tasksMode == Mode.BLOATED) // old task skip immediate code
            { fwd    ; queued ~= new Task(cp, dp, mp, join) ; break ; }
          else                            // new task skip immediate code
            { fwd(2) ; queued ~= new Task(cp, dp, mp, join) ; fwd(-2) ; break ; }
        case '~' : fwd ; m[mp] = getCode ; break ; // read next code as input
        case '*' : // join/wait threads
          switch(join) {
            case FREE    : join = JOINED ;  joinwait++ ; break ;    // schedule to join
            case JOINED  : join = WAITING ; joinwait-- ; goto RET ; // start waiting join
            case WAITING :
              if(joinwait <= 0) { join = FREE ; break ; } // all joined, release all
              else goto RET ; // keep waiting
          }
          break ;
        case '^' : // wrap to the boundary in reverse direction then stop after the 1st '^'
          while(hasCode) fwd(-1) ; while(getCode != '^') fwd ; break ;
        default: //other char is a error command, since it should be seived out
          debug throw new Exception("unknown command") ;
      }
      fwd ;      // next code
  RET:           // directly go here, if waiting input/join, or quit
      lastcp = cp ; lastmp = mp ;
      if(quit && join == JOINED) joinwait-- ;
      return this ;
    }
  }

  this(IIO inputoutput) { m = new Memory ; io = inputoutput ; }

  bool hasCode(I2 codePtr)
    { with(codePtr) return !(x < 0 || y < 0 || x >= width || y  >= lines) ; }
  char getCode(I2 codePtr)
    { with(codePtr) return hasCode(codePtr) ? src[y][x] : '\0' ; }
  string program() { if(prog is null) prog = join(src,"\n") ; return prog ; }
  CPU load(string codes) {
    src = splitlines(codes) ; width = 0 ; lines = src.length ; prog = null ;
    foreach(k,l; src)
      { if ((src[k] = stripr(tr(l,"\0"," "))).length > width) width = src[k].length ; }
    foreach(k,l; src) src[k] = l ~ repeat(" ", width - l.length) ;
    debugInput = pfind(&start, src, "debug[", "]debug") ;
    pfind(&start, src, "$") ;
    if(start.x < 0) start = I2(0,0) ; else start.x++ ;
    return this ;
  }
  CPU initialize(Mode mode = Mode.SUPERNATURAL, bool useDebugInput = true) {
    if(useDebugInput) io.setDebugInput(debugInput) ; else io.setDebugInput("") ;
    tasks = [ new Task(start, I2(1,0), I2(0,0)) ] ;    // 1st task
    tick = 0 ; Id = 0 ; joinwait = 0 ; nTaskLeft = 1 ;
    queued = null ; m.reset() ; tasksMode = mode ; acceptCmd = null ;
    foreach(c ; join(command[0..mode],"")) acceptCmd[c] = c ;
    return this ;
  }
  int run(string codes,Mode mode = Mode.SUPERNATURAL, bool useDebugInput = true)
    { return load(codes).initialize(mode, useDebugInput).run() ; }
  int run() { while(nTaskLeft) run1Tick ; return m[lastmp] ; }
  bool run1Tick() {
    if(nTaskLeft > 0) {
      nTaskLeft = 0 ; tick++ ;
      foreach(tsk ; tasks) // execute & update task
        if((tasks[nTaskLeft] = tsk.execute).quit == false)
          nTaskLeft++ ;
      tasks.length = nTaskLeft ;
      if(queued) { tasks ~= queued ; queued = null ; } // join queued tasks
      nTaskLeft = tasks.length ;
    }
    return nTaskLeft > 0 ;
  }

  static const string[] command = ["<>+-,.!?/\\\0","@#",":;&%","~*^"] ;

  Memory m ;
  IIO io ;

  string prog, debugInput ;
  Mode tasksMode ;
  Task[] tasks, queued ;
  I2 start, lastcp, lastmp ;
  uint width, lines, tick, joinwait, nTaskLeft ;
  private string[] src ;
  private char[char] acceptCmd ;
  private uint Id = 0 ;
}
```

Sample SNUSP using a console io :

```d
module rcsnusp ;
import snud ;
import std.stdio, std.file, std.conv ;

extern(C) {
  int kbhit() ;
  int getch() ;
  int printf(in char*,...);
}

final class CIO : IIO {
  private string debugInput ;
  void setDebugInput(string s) { debugInput = cast(string)s.dup.reverse ; }
  void output(int v){ printf("%1c", cast(char) v) ; }
  bool inputReady() { return debugInput.length || kbhit() ; }
  int input()
    { return cast(int)(debugInput.length ? debugInput.pop() : getch()) ; }
}

int main(string[] args) {
  CPU cpu = new CPU(new CIO) ;

  int result ;
  Mode mode = Mode.SUPERNATURAL ;
  bool useDebug = true ;
  if(args.length <= 1) { // from stdin
    string[] p ;
    string b ;
    while((b = readln()) != null) p ~= std.string.chomp(b)  ;
    cpu.load(std.string.join(p,"\n")).initialize(mode,useDebug) ;
    for(int i = 0 ; i < 10 ; i++) { // do some debug action
      cpu.run1Tick ;
      with (cpu.tasks[0])
      writefln( // debug state output 
        "m(%3d,%3d)=[%4d][%1s] c(%3d,%3d,%3d,%3d)=[%1s](%3d) id<%3d> jc(%1d) quit[%3s]", 
        mp.x, mp.y, cpu.m[mp], cast(char) (cpu.m[mp] >= 32 && cpu.m[mp]<128 ? cpu.m[mp] : '?'), 
        cp.x, cp.y, dp.x, dp.y, getCode, getCode, id, join, quit ? "YES" : "NO") ;
    }
    result = cpu.run(std.string.join(p,"\n"), mode, useDebug) ;
  } else {               // from file names
    if(args.length > 2 && std.string.isNumeric(args[2])) 
      mode = cast(Mode) toInt(args[2]) ;
    if(mode < Mode.CORE || mode > Mode.SUPERNATURAL) 
      mode = Mode.SUPERNATURAL ;
    if(args.length > 3) 
      useDebug = std.string.tolower(args[3]) == "no" ? false : true ;
    result = cpu.run(cast(string) std.file.read(args[1]), mode, useDebug) ;
  }
  writefln("\ntick:%d", cpu.tick) ;

  return result ;
}
```

