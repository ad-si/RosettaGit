+++
title = "ALGOL 68/prelude/graph 2d.a68"
description = ""
date = 2012-10-07T02:19:16Z
aliases = []
[extra]
id = 12361
[taxonomies]
categories = []
tags = []
+++


```algol68
# -*- coding: utf-8 -*- #

#############################################
A simple tookit for producing basic 2d graphs
#############################################

COMMENT REQUIRES:
    MODE GREAL -- normally GREAL
    FORMAT greal repr
COMMENT

COMMENT PROVIDES:
    MODE GRAPHDD -- etc
COMMENT

INT x axis = 1, y axis = 2, z axis = 3;
INT axis 2d = 2, axis 3d = 3, flex axis = 0;

MODE NOARG=BITS; # required when partial parameterisation is not supported #

MODE POINT = [axis 2d]GREAL; 
MODE POINTDETAIL = STRUCT(STRINGOPT label, POINTOPTION option);
MODE POINTYIELD = PROC(POINT)VOID;
MODE POINTGEN = PROC(POINTYIELD)VOID;

MODE TICK = STRUCT(INT number, GREAL len);

MODE AXIS = STRUCT (
  INTERVAL interval,
  STRUCT(INTERVAL interval, TICK major, minor)ticks,
  STRINGOPT label, POINT offset
);

MODE LINEOPTION = [0]UNION(COLOUR, LINESTYLE, ICON, SPLINE);
MODE POINTOPTION = [0]UNION(COLOUR, ICON);

MODE VALUELINESTYLE = STRUCT(STRING name, pattern);
MODE LINESTYLE = REF VALUELINESTYLE;
VALUELINESTYLE
  solid              := ("solid",          "---------------------"),
  dotted             := ("dotted",         "- - - - - - - -"),
  dot dashed         := ("dotdashed",      "--- - --- - --- -"),
  short dashed       := ("shortdashed",    "--- --- --- ---"),
  long dashed        := ("longdashed",     "----- ----- -----"),
  dot dot dashed     := ("dotdotdashed",   "--- - - --- - -"),
  dot dot dot dashed := ("dotdotdotdashed","--- - - - --- - - -");

MODE VALUECOLOUR = STRUCT(STRING name, [3]REAL rgb);
MODE COLOUR = REF VALUECOLOUR;
VALUECOLOUR 
  black := ("black", (0,0,0)), 
  grey  := ("grey",  (0.5,0.5,0.5)), 
  white := ("white", (1,1,1)),
  purple:= ("purple",(1,0,1)), 
  red   := ("red",   (1,0,0)), 
  yellow:= ("yellow",(1,1,0)), 
  green := ("green", (0,1,0)), 
  cyan  := ("cyan",  (0,1,1)), 
  blue  := ("blue",  (0,0,1));

MODE VALUESPLINE = STRUCT(STRING spline);
MODE SPLINE = REF VALUESPLINE;

MODE VALUEICON = STRUCT(STRING icon);
MODE ICON = REF VALUEICON;

OP + = (COLOUR a,b)COLOUR: 
  HEAP VALUECOLOUR := (name OF a + "+" + name OF b, rgb OF a + rgb OF b);

[]COLOUR rainbow = (purple, red, yellow, green, cyan, blue, purple);

OP LWB  = (INT axis, REF GRAPHDD graph)REF GREAL: lwb OF interval OF ((axis OF graph)[axis]),
   UPB  = (INT axis, REF GRAPHDD graph)REF GREAL: upb OF interval OF ((axis OF graph)[axis]);

OP INIT = (REF AXIS self)REF AXIS: (
  len OF major OF ticks OF self := len OF minor OF ticks OF self := 0.1 # mm #;
  label OF self := EMPTY;
  offset OF self := POINT(0.0, 0.0 #, 0.0 DB#); # about the origin #
  self
);

MODE AXISOPT = UNION(AXIS, VOID);

MODE WINDOW =
  STRUCT(
    REF FILE file,
    STRING type,  # eg "X" or gif #
    [axis 2d]INT pixels,
    [axis 2d]GREAL mm
  );

# A crude CLASS declaration #
MODE GRAPHDD = STRUCT (
  PROC(NOARG)GRAPHDDMETHODOF method,
  WINDOW window,
  GREAL border,
  [axis 2d]GREAL zoom, pan,
  STRINGOPT title, sub title,
  [axis 2d]AXIS axis
# FLEX[0]AXISOPT y2, #
#  POINTGEN gen point #
);

MODE FUNCTIONINTERVAL = STRUCT(PROC(REAL)REAL function, INTERVAL interval);
MODE CURVE = UNION(POINTGEN, []POINT, FUNCTIONINTERVAL);

MODE GRAPHDDMETHODOF = STRUCT(
  PROC (NOARG)REF GRAPHDD init,
  PROC (LINEOPTION #options#)VOID begin options,
  PROC (NOARG)VOID end options,
  PROC (NOARG)VOID init pan and zoom,
  PROC (POINT #natural#)POINT pan and zoom,
  PROC (NOARG)VOID decorate,
  PROC (NOARG)VOID begin curve,
  PROC (POINTGEN #curve#, LINEOPTION #option#)VOID add curve gen point,
  PROC ([]POINT #curve#, LINEOPTION #option#)VOID add curve array point,
  PROC (FUNCTIONINTERVAL #function interval#, LINEOPTION #option#)VOID add curve function interval,
  PROC (CURVE #curve#, LINEOPTION #option#)VOID add curve,
  PROC (NOARG)VOID end curve
);

OP METHODOF = (GRAPHDD graph)GRAPHDDMETHODOF: (method OF graph)(~);

GREAL zoom = 1;

PROC draw move point = (REF FILE file, POINT point)VOID: draw move(file, point[x axis], point[y axis]);
PROC draw line point = (REF FILE file, POINT point)VOID: draw line(file, point[x axis], point[y axis]);

# Bind the Attributes to the Instance #
PROC method of graph 2d = (REF GRAPHDD self, NOARG skip)GRAPHDDMETHODOF:(
     init of graph 2d(self,#~#),
     begin options of graph 2d(self,),
     end options of graph 2d(self,#~#),
     init pan and zoom of graph 2d(self,#~#),
     pan and zoom of graph 2d(self,),
     decorate of graph 2d(self,#~#),
     begin curve of graph 2d(self,#~#),
     add curve gen point of graph 2d(self,,),
     add curve array point of graph 2d(self,,),
     add curve function interval of graph 2d(self,,),
     add curve of graph 2d(self,,),
     end curve of graph 2d(self,#~#)
   );

PROC init of graph 2d = (REF GRAPHDD self, NOARG skip)REF GRAPHDD: (
    method OF self := method of graph 2d(self,#~#); # Hmmm... #

    title OF self := sub title OF self := EMPTY;
    axis OF self := (INIT LOC AXIS, INIT LOC AXIS);
    window OF self := (HEAP FILE, "X", 
                        (ENTIER(1200*zoom), ENTIER(900*zoom)) # pixels#, 
                        (200*2, 150*2) #mm#  );
    border OF self := 0.05; # 5% #
    self
  );

PROC begin options of graph 2d = (REF GRAPHDD self, LINEOPTION options)VOID:(
   REF FILE file := file OF window OF self;
   FOR i TO UPB options DO
     CASE options[i] IN
       (COLOUR colour):([]REAL v = rgb OF colour; draw colour(file, v[1],v[2],v[3])),
       (ICON icon): SKIP, # unimplemented #
       (LINESTYLE line style): draw line style(file, name OF line style),
       (SPLINE spline): SKIP # unimplemented #
     OUT
       raise value error("Unknown draw options")
     ESAC
   OD
 );

PROC end options of graph 2d = (REF GRAPHDD self, NOARG skip)VOID: SKIP;

PROC init pan and zoom of graph 2d = (REF GRAPHDD self, NOARG skip)VOID:(
    GREAL border = border OF self # 5% #;
    [axis 2d]STRUCT(GREAL lwb, upb)window;
    window[x axis] := (border, border);
    window[y axis] := (border, border*2);
    FOR axis TO UPB window DO
      (zoom OF self)[axis] := (1-lwb OF window[axis]-upb OF window[axis])/(axis UPB self - axis LWB self);
      (pan  OF self)[axis] := lwb OF window[axis] - axis LWB self*(zoom OF self)[axis]
    OD
  );

PROC pan and zoom of graph 2d = (REF GRAPHDD self, POINT point)POINT:(
  POINT out;
  FOR dim TO UPB out DO
    INT axis = (dim = 1 | dim | axis 2d );
    out[dim] := point[dim] * (zoom OF self)[axis] + (pan OF self)[axis]
  OD;
  out
);

PROC decorate of graph 2d = (REF GRAPHDD self, NOARG skip)VOID:(
  REF FILE window = file OF window OF self;
  STRING title = title OF self ORELSE "X vs Y Graph" ;
  draw background colour name (window, "white");
  draw erase (window);
  draw colour name (window, "black");
  GREAL label start = 0.5;
  GREAL pan = 0.03;
  GREAL x axis offset = (offset OF axis OF self)[x axis][y axis];
  GREAL y axis offset = (offset OF axis OF self)[y axis][x axis];
  PROC repr = (GREAL r)STRING: sprintf((greal repr,r));
  POINT point;

# draw title #
  point:= (label start, 1 - pan);
  draw move point(window, point);
  draw text (window, "c", "c", title);
  CASE sub title OF self IN
    (STRING sub title):(
      point:= (label start, 1 - 2*pan);
      draw move point(window, point);
      draw text (window, "c", "c", sub title)
    )
  ESAC;

# label x axis axis #
  point := (pan and zoom OF (METHODOF self))((x axis LWB self, x axis offset));
  draw move (window, label start, point[y axis]-pan);
  draw text (window, "c", "c", (label OF axis OF self)[x axis] ORELSE "X Axis");
  draw move (window, point[x axis], point[y axis]-pan);
  draw text (window, "c", "c", repr(x axis LWB self));
  draw move point(window, point);

# draw x axis axis #
  point := (pan and zoom OF (METHODOF self))((x axis UPB self, x axis offset));
  draw line point (window, point);
  draw move (window, point[x axis], point[y axis]-pan);
  draw text (window, "c", "c", repr(x axis UPB self));

# label y axis axis #
  point := (pan and zoom OF (METHODOF self))((y axis offset, y axis LWB self));
  draw text angle (window, 90);
  draw move (window, point[x axis]-pan, label start);
  draw text (window, "c", "c", (label OF axis OF self)[y axis] ORELSE "Y Axis");
  draw move (window, point[x axis]-pan, point[y axis]);
  draw text (window, "c", "c", repr(y axis LWB self));

# draw y axis axis #
  draw move point(window, point);
  point := (pan and zoom OF (METHODOF self))((y axis offset, y axis UPB self));
  draw line point(window, point);
  draw move (window, point[x axis]-pan, point[y axis]);
  draw text (window, "c", "c", repr(y axis UPB self))
);

PROC begin curve of graph 2d = (REF GRAPHDD self, NOARG skip)VOID:(
  REF FILE window = file OF window OF self;
  STRING title = title OF self ORELSE "Graph";
  STRING sub title = sub title OF self ORELSE "";
  STRING file_name := title+"-"+sub title+"."+type OF window OF self;
  # Avoid white space in a file_name #
  FOR i TO UPB file_name DO IF file_name[i]=" " THEN file_name[i]:="_" FI OD;
  open (window, file_name, stand draw channel);

  draw device (window, type OF window OF self, sprintf(($g(0)"x axis"g(0)$, pixels OF window OF self)));
  (init pan and zoom OF (METHODOF self))(~);
  (decorate OF (METHODOF self))(~)
);

PROC add curve gen point of graph 2d = (REF GRAPHDD self, POINTGEN gen point, LINEOPTION option)VOID:(
  REF FILE window = file OF window OF self;
  BOOL first := TRUE;
  POINT prev specimen;
  (begin options OF (METHODOF self))(option);
   #FOR POINT point IN # gen point #DO#(
   ## (POINT point)VOID: (
        POINT next specimen = (pan and zoom OF (METHODOF self))(point);
        IF UPB next specimen = 2 AND FALSE THEN # simple case: the single line #
          IF first THEN first := FALSE; draw move point ELSE draw line point FI (window, next specimen)
        ELSE # not so simple, multiple lines to plot #
          IF first THEN
            first := FALSE
          ELSE
            FOR axis y2 FROM 2 TO UPB next specimen DO
              draw move(window, prev specimen[x axis], prev specimen[axis y2]);
              draw line(window, next specimen[x axis], next specimen[axis y2])
            OD
          FI;
          prev specimen := next specimen
        FI
   # OD#));
  (end options OF (METHODOF self))(~)
);

PROC add curve array point of graph 2d = (REF GRAPHDD self, []POINT curve, LINEOPTION option)VOID:(
  PROC gen curve = (POINTYIELD yield)VOID:
    FOR i TO UPB curve DO yield(curve[i]) OD;
  (add curve gen point OF (METHODOF self))(gen curve, option)
);

PROC add curve function interval of graph 2d = (REF GRAPHDD self, FUNCTIONINTERVAL function interval, LINEOPTION option)VOID:(
  PROC gen curve = (POINTYIELD yield)VOID:(
    INT n steps = 100;
    REAL x := lwb OF interval OF function interval;
    REAL dx = WIDTH interval OF function interval / n steps;
    FOR i FROM 0 TO n steps DO 
      yield((x, (function OF function interval)(x)));
      x +:= dx
    OD
 );
 (add curve gen point OF (METHODOF self))(gen curve, option)
 ),

PROC add curve of graph 2d = (REF GRAPHDD self, CURVE curve, LINEOPTION option)VOID:(
  CASE curve IN
    (POINTGEN gen): (add curve gen point OF (METHODOF self))(gen, option),
    ([]POINT array): (add curve array point OF (METHODOF self))(array, option),
    (FUNCTIONINTERVAL function interval): (add curve function interval OF (METHODOF self))(function interval, option)
  OUT
    raise undefined("add curve type")
  ESAC
);

PROC end curve of graph 2d = (REF GRAPHDD self, NOARG empty)VOID:(
  REF FILE window = file OF window #OF data# OF self;
  draw show (window);
  close(window)
);

OP INIT = (REF GRAPHDD object)REF GRAPHDD: init of graph 2d(object,~);

SKIP
```

