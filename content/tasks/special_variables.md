+++
title = "Special variables"
description = ""
date = 2019-05-30T08:10:31Z
aliases = []
[extra]
id = 9850
[taxonomies]
categories = ["task", "Basic language learning"]
tags = []
languages = [
  "ada",
  "algol_68",
  "algol_w",
  "autohotkey",
  "awk",
  "batch_file",
  "bbc_basic",
  "bc",
  "bracmat",
  "c",
  "clojure",
  "common_lisp",
  "d",
  "dwscript",
  "dyalect",
  "erlang",
  "forth",
  "fortran",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lasso",
  "lingo",
  "livecode",
  "lua",
  "m2000_interpreter",
  "mathematica",
  "maxima",
  "ml_i",
  "netrexx",
  "oasys",
  "oasys_assembler",
  "ocaml",
  "oforth",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "smalltalk",
  "tcl",
  "ursa",
  "vba",
  "xlisp",
]
+++

{{task|Basic language learning}}[[Category:Special variables]]

Special variables have a predefined meaning within a computer programming language.


## Task

List the special variables used within the language.





## Ada

Ada has no special variables in the standard namespace, nor are any such variables commonplace in packages, almost all are constants or private variables changed through functions.
The closest thing Ada has to special variables are attributes, which are of an object oriented nature dependant on their prefix. Examples:

* When X is of an array type, X'First denotes the first index of the array (Ada arrays are not limited to starting at 0 or 1).
* When X is of a scalar type, X'First denotes the lower bound of that type.
* X'Size gives the number of bits of memory type X is stored as, but can also be assigned to, to force the program to store the type in that number of bits. (Provided its more than the minimum)
* For example, a 2 bit record (structure) X might be stored as a byte by default as a speed optimization, but X'Size := 2; would force it to be stored as two bits.

There are far too many attributes to list here, but a standard informative list can be found in Annex K of the documentation if you have it installed.
Or Here: [http://www2.adacore.com/gap-static/GNAT_Book/html/aarm/AA-K.html Annex K - Language-Defined Attributes]


## AutoHotkey

Refer to [http://www.autohotkey.com/docs/Variables.htm#BuiltIn Built-in Variables] for the complete special variables (those variables are global, except noticed).


## ALGOL 68

```algol68
#!/usr/local/bin/a68g --script #

FORMAT f = $g": ["g"]"l$;

printf((f,
  "pi", pi,
  "random", random, # actually a procedure #

  "flip", flip,
  "flop", flop,
  "TRUE", TRUE,
  "FALSE", FALSE,

  "error char", error char,
  "null character", null character,
CO   "NIL", NIL, NIL is not printable END CO

# "lengths" details how many distinctive precisions are permitted. #
# e.g. int length = 3 suggests 3 distincts types: #
#      INT, LONG INT, and LONG LONG INT #
  "bits shorths", bits shorths,
  "bits lengths", bits lengths,
  "bytes shorths", bytes shorths,
  "bytes lengths", bytes lengths,
  "int shorths", int shorths,
  "int lengths", int lengths,
  "real shorths", real shorths,
  "real lengths", real lengths,

  "max abs char", max abs char,
# short/long int/real also possible #
  "max int", max int,
  "small real", small real,
  "max real", max real,

# "width" indicates how many characters are require to prepresent the value #
# short/long bits/bytes/int/real also possible #
  "bits width", bits width,
  "bytes width", bytes width,
  "int width", int width,
  "real width", real width,
  "exp width", exp width
));

# ALL the following are actually procedures #
print((
  "space: [", space, "]", new line,
  "new line: [", new line, "]", new line,
  "new page: [", new page, "]", new line
CO the following are standard, but not implemented in algol68g
  "char number: [", char number, "]", new line,
  "line number: [", line number, "]", new line,
  "page number: [", page number, "]", new line
END CO
));
SKIP
```

Sample output:

```txt

pi: [+3.14159265358979e  +0]
random: [+6.08495516447216e  -2]
flip: [T]
flop: [F]
TRUE: [T]
FALSE: [F]
error char: [*]
null character: [bits shorths: [         +1]
bits lengths: [         +3]
bytes shorths: [         +1]
bytes lengths: [         +2]
int shorths: [         +1]
int lengths: [         +3]
real shorths: [         +1]
real lengths: [         +3]
max abs char: [       +255]
max int: [+2147483647]
small real: [+2.22044604925031e -16]
max real: [+1.79769313486235e+308]
bits width: [        +32]
bytes width: [        +32]
int width: [        +10]
real width: [        +15]
exp width: [         +3]
space: [ ]
new line: [
]
new page: [
           ]

```

===[[ALGOL 68G]]===
[[ALGOL 68G]] provides some further constants to the scientifically motivated coder:

```algol68
#!/usr/local/bin/a68g --script #

printf(($g": [", g, "] & [",g,"]"l$,
  "Math constants", "long","long long",
    "pi", long pi, long long pi,
  "Physical Constants", "mksa","cgs",
  "Fundamental constants", "mksa","cgs",
  "  speed of light", mksa speed of light, cgs speed of light,
  "  vacuum permeability", mksa vacuum permeability, "~", # cgs vacuum permeability,#
  "  vacuum permittivity", mksa vacuum permittivity, "~", # cgs vacuum permittivity,#
  "  num avogadro", num avogadro,"~",
  "  faraday", mksa faraday, cgs faraday,
  "  boltzmann", mksa boltzmann, cgs boltzmann,
  "  molar gas", mksa molar gas, cgs molar gas,
  "  standard gas volume", mksa standard gas volume, cgs standard gas volume,
  "  planck constant", mksa planck constant, cgs planck constant,
  "  planck constant bar", mksa planck constant bar, cgs planck constant bar,
  "  gauss", mksa gauss, cgs gauss,
  "  micron", mksa micron, cgs micron,
  "  hectare", mksa hectare, cgs hectare,
  "  miles per hour", mksa miles per hour, cgs miles per hour,
  "  kilometers per hour", mksa kilometers per hour, cgs kilometers per hour,
  "Astronomy and astrophysics", "mksa","cgs",
  "  astronomical unit", mksa astronomical unit, cgs astronomical unit,
  "  gravitational constant", mksa gravitational constant, cgs gravitational constant,
  "  light year", mksa light year, cgs light year,
  "  parsec", mksa parsec, cgs parsec,
  "  grav accel", mksa grav accel, cgs grav accel,
  "  solar mass", mksa solar mass, cgs solar mass,
  "Atomic and nuclear physics", "mksa","cgs",
  "  electron charge", mksa electron charge, cgs electron charge,
  "  electron volt", mksa electron volt, cgs electron volt,
  "  unified atomic mass", mksa unified atomic mass, cgs unified atomic mass,
  "  mass electron", mksa mass electron, cgs mass electron,
  "  mass muon", mksa mass muon, cgs mass muon,
  "  mass proton", mksa mass proton, cgs mass proton,
  "  mass neutron", mksa mass neutron, cgs mass neutron,
  "  num fine structure", num fine structure,"~",
  "  rydberg", mksa rydberg, cgs rydberg,
  "  bohr radius", mksa bohr radius, cgs bohr radius,
  "  angstrom", mksa angstrom, cgs angstrom,
  "  barn", mksa barn, cgs barn,
  "  bohr magneton", mksa bohr magneton, cgs bohr magneton,
  "  nuclear magneton", mksa nuclear magneton, cgs nuclear magneton,
  "  electron magnetic moment", mksa electron magnetic moment, cgs electron magnetic moment,
  "  proton magnetic moment", mksa proton magnetic moment, cgs proton magnetic moment,
  "Time", "mksa","cgs",
  "  minute", mksa minute, cgs minute,
  "  hour", mksa hour, cgs hour,
  "  day", mksa day, cgs day,
  "  week", mksa week, cgs week,
  "Imperial units", "mksa","cgs",
  "  inch", mksa inch, cgs inch,
  "  foot", mksa foot, cgs foot,
  "  yard", mksa yard, cgs yard,
  "  mile", mksa mile, cgs mile,
  "  mil", mksa mil, cgs mil,
  "Nautical units", "mksa","cgs",
  "  nautical mile", mksa nautical mile, cgs nautical mile,
  "  fathom", mksa fathom, cgs fathom,
  "  knot", mksa knot, cgs knot,
  "Volume", "mksa","cgs",
  "  acre", mksa acre, cgs acre,
  "  liter", mksa liter, cgs liter,
  "  us gallon", mksa us gallon, cgs us gallon,
  "  canadian gallon", mksa canadian gallon, cgs canadian gallon,
  "  uk gallon", mksa uk gallon, cgs uk gallon,
  "  quart", mksa quart, cgs quart,
  "  pint", mksa pint, cgs pint,
  "Mass and weight", "mksa","cgs",
  "  pound mass", mksa pound mass, cgs pound mass,
  "  ounce mass", mksa ounce mass, cgs ounce mass,
  "  ton", mksa ton, cgs ton,
  "  metric ton", mksa metric ton, cgs metric ton,
  "  uk ton", mksa uk ton, cgs uk ton,
  "  troy ounce", mksa troy ounce, cgs troy ounce,
  "  carat", mksa carat, cgs carat,
  "  gram force", mksa gram force, cgs gram force,
  "  pound force", mksa pound force, cgs pound force,
  "  kilopound force", mksa kilopound force, cgs kilopound force,
  "  poundal", mksa poundal, cgs poundal,
  "Thermal energy and power", "mksa","cgs",
  "  calorie", mksa calorie, cgs calorie,
  "  btu", mksa btu, cgs btu,
  "  therm", mksa therm, cgs therm,
  "  horsepower", mksa horsepower, cgs horsepower,
  "Pressure", "mksa","cgs",
  "  bar", mksa bar, cgs bar,
  "  std atmosphere", mksa std atmosphere, cgs std atmosphere,
  "  torr", mksa torr, cgs torr,
  "  meter of mercury", mksa meter of mercury, cgs meter of mercury,
  "  inch of mercury", mksa inch of mercury, cgs inch of mercury,
  "  inch of water", mksa inch of water, cgs inch of water,
  "  psi", mksa psi, cgs psi,
  "Viscosity", "mksa","cgs",
  "  poise", mksa poise, cgs poise,
  "  stokes", mksa stokes, cgs stokes,
  "Light and illumination", "mksa","cgs",
  "  stilb", mksa stilb, cgs stilb,
  "  lumen", mksa lumen, cgs lumen,
  "  lux", mksa lux, cgs lux,
  "  phot", mksa phot, cgs phot,
  "  footcandle", mksa footcandle, cgs footcandle,
  "  lambert", mksa lambert, cgs lambert,
  "  footlambert", mksa footlambert, cgs footlambert,
  "Radioactivity", "mksa","cgs",
  "  curie", mksa curie, cgs curie,
  "  roentgen", mksa roentgen, cgs roentgen,
  "  rad", mksa rad, cgs rad,
  "Force and energy", "mksa","cgs",
  "  newton", mksa newton, cgs newton,
  "  dyne", mksa dyne, cgs dyne,
  "  joule", mksa joule, cgs joule,
  "  erg", mksa erg, cgs erg
))
```

Output:

```txt

Math constants: [long] & [long long]
pi: [+3.141592653589793238462643383e  +0] & [+3.14159265358979323846264338327950288419716939937510582097494459e  +0]
Physical Constants: [mksa] & [cgs]
Fundamental constants: [mksa] & [cgs]
  speed of light: [+2.99792458000000e  +8] & [+2.99792458000000e +10]
  vacuum permeability: [+1.25663706144000e  -6] & [~]
  vacuum permittivity: [+8.85418781700000e -12] & [~]
  num avogadro: [+6.02214199000001e +23] & [~]
  faraday: [+9.64853429775000e  +4] & [+9.64853429775000e  +3]
  boltzmann: [+1.38065040000000e -23] & [+1.38065040000000e -16]
  molar gas: [+8.31447200000000e  +0] & [+8.31447200000000e  +7]
  standard gas volume: [+2.27109810000000e  -2] & [+2.27109810000000e  +4]
  planck constant: [+6.62606930000000e -34] & [+6.62606930000000e -27]
  planck constant bar: [+1.05457168236445e -34] & [+1.05457168236445e -27]
  gauss: [+1.00000000000000e  -4] & [+1.00000000000000e  +0]
  micron: [+1.00000000000000e  -6] & [+1.00000000000000e  -4]
  hectare: [+1.00000000000000e  +4] & [+1.00000000000000e  +8]
  miles per hour: [+4.47040000000000e  -1] & [+4.47040000000000e  +1]
  kilometers per hour: [+2.77777777778000e  -1] & [+2.77777777778000e  +1]
Astronomy and astrophysics: [mksa] & [cgs]
  astronomical unit: [+1.49597870691000e +11] & [+1.49597870691000e +13]
  gravitational constant: [+6.67300000000000e -11] & [+6.67300000000000e  -8]
  light year: [+9.46053620707001e +15] & [+9.46053620707001e +17]
  parsec: [+3.08567758135000e +16] & [+3.08567758135000e +18]
  grav accel: [+9.80665000000000e  +0] & [+9.80665000000000e  +2]
  solar mass: [+1.98892000000000e +30] & [+1.98892000000000e +33]
Atomic and nuclear physics: [mksa] & [cgs]
  electron charge: [+1.60217648700000e -19] & [+1.60217648700000e -20]
  electron volt: [+1.60217648700000e -19] & [+1.60217648700000e -12]
  unified atomic mass: [+1.66053878200000e -27] & [+1.66053878200000e -24]
  mass electron: [+9.10938188000000e -31] & [+9.10938188000000e -28]
  mass muon: [+1.88353109000000e -28] & [+1.88353109000000e -25]
  mass proton: [+1.67262158000000e -27] & [+1.67262158000000e -24]
  mass neutron: [+1.67492716000000e -27] & [+1.67492716000000e -24]
  num fine structure: [+7.29735253300000e  -3] & [~]
  rydberg: [+2.17987196968000e -18] & [+2.17987196968000e -11]
  bohr radius: [+5.29177208300000e -11] & [+5.29177208300000e  -9]
  angstrom: [+1.00000000000000e -10] & [+1.00000000000000e  -8]
  barn: [+1.00000000000000e -28] & [+1.00000000000000e -24]
  bohr magneton: [+9.27400899000000e -24] & [+9.27400899000000e -21]
  nuclear magneton: [+5.05078317000000e -27] & [+5.05078317000000e -24]
  electron magnetic moment: [+9.28476362000000e -24] & [+9.28476362000000e -21]
  proton magnetic moment: [+1.41060663300000e -26] & [+1.41060663300000e -23]
Time: [mksa] & [cgs]
  minute: [+6.00000000000000e  +1] & [+6.00000000000000e  +1]
  hour: [+3.60000000000000e  +3] & [+3.60000000000000e  +3]
  day: [+8.64000000000000e  +4] & [+8.64000000000000e  +4]
  week: [+6.04800000000000e  +5] & [+6.04800000000000e  +5]
Imperial units: [mksa] & [cgs]
  inch: [+2.54000000000000e  -2] & [+2.54000000000000e  +0]
  foot: [+3.04800000000000e  -1] & [+3.04800000000000e  +1]
  yard: [+9.14400000000000e  -1] & [+9.14400000000000e  +1]
  mile: [+1.60934400000000e  +3] & [+1.60934400000000e  +5]
  mil: [+2.54000000000000e  -5] & [+2.54000000000000e  -3]
Nautical units: [mksa] & [cgs]
  nautical mile: [+1.85200000000000e  +3] & [+1.85200000000000e  +5]
  fathom: [+1.82880000000000e  +0] & [+1.82880000000000e  +2]
  knot: [+5.14444444444000e  -1] & [+5.14444444444000e  +1]
Volume: [mksa] & [cgs]
  acre: [+4.04685642241000e  +3] & [+4.04685642241000e  +7]
  liter: [+1.00000000000000e  -3] & [+1.00000000000000e  +3]
  us gallon: [+3.78541178402000e  -3] & [+3.78541178402000e  +3]
  canadian gallon: [+4.54609000000000e  -3] & [+4.54609000000000e  +3]
  uk gallon: [+4.54609200000000e  -3] & [+4.54609200000000e  +3]
  quart: [+9.46352946004000e  -4] & [+9.46352946004000e  +2]
  pint: [+4.73176473002000e  -4] & [+4.73176473002000e  +2]
Mass and weight: [mksa] & [cgs]
  pound mass: [+4.53592370000000e  -1] & [+4.53592370000000e  +2]
  ounce mass: [+2.83495231250000e  -2] & [+2.83495231250000e  +1]
  ton: [+9.07184740000000e  +2] & [+9.07184740000000e  +5]
  metric ton: [+1.00000000000000e  +3] & [+1.00000000000000e  +6]
  uk ton: [+1.01604690880000e  +3] & [+1.01604690880000e  +6]
  troy ounce: [+3.11034750000000e  -2] & [+3.11034750000000e  +1]
  carat: [+2.00000000000000e  -4] & [+2.00000000000000e  -1]
  gram force: [+9.80665000000000e  -3] & [+9.80665000000000e  +2]
  pound force: [+4.44822161526000e  +0] & [+4.44822161526000e  +5]
  kilopound force: [+4.44822161526000e  +3] & [+4.44822161526000e  +8]
  poundal: [+1.38255000000000e  -1] & [+1.38255000000000e  +4]
Thermal energy and power: [mksa] & [cgs]
  calorie: [+4.18680000000000e  +0] & [+4.18680000000000e  +7]
  btu: [+1.05505585262000e  +3] & [+1.05505585262000e +10]
  therm: [+1.05506000000000e  +8] & [+1.05506000000000e +15]
  horsepower: [+7.45700000000000e  +2] & [+7.45700000000000e  +9]
Pressure: [mksa] & [cgs]
  bar: [+1.00000000000000e  +5] & [+1.00000000000000e  +6]
  std atmosphere: [+1.01325000000000e  +5] & [+1.01325000000000e  +6]
  torr: [+1.33322368421000e  +2] & [+1.33322368421000e  +3]
  meter of mercury: [+1.33322368421000e  +5] & [+1.33322368421000e  +6]
  inch of mercury: [+3.38638815789000e  +3] & [+3.38638815789000e  +4]
  inch of water: [+2.49088900000000e  +2] & [+2.49088900000000e  +3]
  psi: [+6.89475729317000e  +3] & [+6.89475729317000e  +4]
Viscosity: [mksa] & [cgs]
  poise: [+1.00000000000000e  -1] & [+1.00000000000000e  +0]
  stokes: [+1.00000000000000e  -4] & [+1.00000000000000e  +0]
Light and illumination: [mksa] & [cgs]
  stilb: [+1.00000000000000e  +4] & [+1.00000000000000e  +0]
  lumen: [+1.00000000000000e  +0] & [+1.00000000000000e  +0]
  lux: [+1.00000000000000e  +0] & [+1.00000000000000e  -4]
  phot: [+1.00000000000000e  +4] & [+1.00000000000000e  +0]
  footcandle: [+1.07600000000000e  +1] & [+1.07600000000000e  -3]
  lambert: [+1.00000000000000e  +4] & [+1.00000000000000e  +0]
  footlambert: [+1.07639104000000e  +1] & [+1.07639104000000e  -3]
Radioactivity: [mksa] & [cgs]
  curie: [+3.70000000000000e +10] & [+3.70000000000000e +10]
  roentgen: [+2.58000000000000e  -4] & [+2.58000000000000e  -8]
  rad: [+1.00000000000000e  -2] & [+1.00000000000000e  +2]
Force and energy: [mksa] & [cgs]
  newton: [+1.00000000000000e  +0] & [+1.00000000000000e  +5]
  dyne: [+1.00000000000000e  -5] & [+1.00000000000000e  +0]
  joule: [+1.00000000000000e  +0] & [+1.00000000000000e  +7]
  erg: [+1.00000000000000e  -7] & [+1.00000000000000e  +0]

```



## ALGOL W


```algolw
% the Algol W standard environment includes the following standard variables: %

integer                I_W          % field width for integer output %
integer                R_W          % field width for real output %
integer                R_D          % number of decimal places for real output %
string(1)              R_FORMAT     % format for real output:
                                          S - "scaled" normalised mantissa with exponent
                                          A - "aligned" fixed point format
                                          F - "free" either scaled or aligned as appropriate
                                                     for the value and field width
                                    %
integer                S_W          % separator width - number of spaces following non-string output items %
integer                MAXINTEGER   % largest integer value %
real                   EPSILON      % largest positive real number such that 1 + epsilon = 1 %
long real              LONGEPSILON  % largest positive long real number such that 1 + longepsilon = 1 %
long real              MAXREAL      % largest real number %
long real              PI           % approximation to pi %

% the following reference(EXCEPTION) variables control how errors are handled:
    ENDFILE                         - end-of-file
    OVFL                            - overflow
    UNFL                            - underflow
    DIVZERO                         - division by zero
    INTOVFL                         - integer overflow
    INTDIVZERO                      - integer division by zero or modulo 0
    SQRTERR                         - invalid SQRT parameter
    EXPERR                          - invalid EXP parameter
    LNLOGERR                        - invalid LN or LOG parameter
    SINCOSERR                       - invalid SIN or COS parameter

The EXCEPTION record is defined as follows:

record    EXCEPTION( logical    XCPNOTED      - true if the exception has occurred
                   ; integer    XCPLIMIT      - number of times the exception can occur
                                                before the program terminates
                              , XCPACTION     - if the program continues, controls how to
                                                replace the erroneous value
                   ; logical    XCPMARK       - true if an error message should be printed
                                                even if the program continues
                   ; string(64) XCPMSG        - message to describe the exception
                   )

if the relevant EXCEPTION variable is null, the exception is ignored,
otherwise it is processed according to the settings of XCPLIMIT etc.
%
```



## AWK


There are two types of special variables within AWK: Control variables and Informative variables.

* dollarint variables - The dollarint special variables represent fields within a record
* ARGC - An informative variable that provides command line parameter information
* ARGV - An informative array that provides command line parameter information
* CONVFMT - A control variable that specifies the conversion format of numerical strings
* ENVIRON - An informative array that contains the environment strings
* FILENAME - An informative variable that provides the current input [filename]
* FNR - An informative variable that provides the record number within the current file
* FS - A control variable that specifies the input field separator
* NF - An informative variable that provides the number of fields within the current record
* NR - An informative variable that provides the total number of records processed
* OFMT - A control variable that specifies the output format of numerical values
* OFS - A control variable that specifies the output field separator
* ORS - A control variable that specifies the output record separator
* RLENGTH - An informative variable that provides the length of the currently matched substring
* RS - A control variable that specifies the input record separator
* RSTART - An informative variable that provides the start index of the currently matched substring
* SUBSEP - A control variable that specifies the subscript separator for multidimensional arrays


## BBC BASIC

The special (system) variables are as follows:

```txt

@%        The number output format control variable
@cmd$     The command line of a 'compiled' program
@dir$     The directory (folder) from which the program was loaded
@flags%   An integer incorporating BBC BASIC's control flags
@hcsr%    The handle of the mouse pointer (cursor)
@haccel%  The handle of the keyboard accelerator, if used
@hevent%  The handle of the event used to prevent blocking in serial I/O
@hfile%() An array of file handles indexed by channel number
@hmdi%    The Multiple Document Interface window handle (if any)
@hwacc%   The window handle to which keyboard accelerator commands should be sent
@hwnd%    The 'window handle' for the main (output) window
@hwo%     The handle of the WAVEOUTPUT device
@hpal%    The handle for the colour palette
@ispal%   A Boolean which is non-zero if the display is paletted
@lib$     The directory (folder) containing the library files
@lparam%  The LPARAM value (for use with ON MOUSE, ON MOVE and ON SYS)
@memhdc%  The 'device context' for the main (output) window
@midi%    The MIDI device ID (non-zero if a MIDI file is playing)
@msg%     The MSG value (for use with ON MOUSE, ON MOVE and ON SYS)
@ox%      The horizontal offset (in pixels) between the output bitmap and the window contents
@oy%      The vertical offset (in pixels) between the output bitmap and the window contents
@prthdc%  The 'device context' for the current printer (if any)
@tmp$     The temporary directory (folder)
@usr$     The user's Documents directory (folder)
@vdu%     A pointer to the text and graphics parameters
@vdu{}    A structure containing the main text and graphics variables
@wparam%  The WPARAM value (for use with ON MOUSE, ON MOVE and ON SYS)

```



## Batch File

By typing the <code>SET</code> command (without any parameters) in the command prompt, it will display the current environment variables and their current values.

However, there are some special variables that are not listed in the variables displayed by the SET command because their values might change over time. These variables are as follows:


```txt
%CD% - expands to the current directory string.

%DATE% - expands to current date using same format as DATE command.

%TIME% - expands to current time using same format as TIME command.

%RANDOM% - expands to a random decimal number between 0 and 32767.

%ERRORLEVEL% - expands to the current ERRORLEVEL value

%CMDEXTVERSION% - expands to the current Command Processor Extensions
    version number.

%CMDCMDLINE% - expands to the original command line that invoked the
    Command Processor.

%HIGHESTNUMANODENUMBER% - expands to the highest NUMA node number
    on this machine.
```

(Source: by typing <code>SET/?</code> command in the command prompt)


## bc

There are three special variables:
* <code>scale</code>: determines how many digits after the decimal point a result of an expression will have (possible integer values from 0 to <code>BC_SCALE_MAX</code>, i.e. an implementation-specific maximum; initial value is 0)
* <code>ibase</code>: the input radix (possible integer values from 2 to 16; initial value is 10)
* <code>obase</code>: the output radix (possible integer values from 2 to <code>BC_BASE_MAX</code>; initial value is 10)

The GNU implementation adds another special variable, <code>last</code>, that contains the value of the last printed number.


## Bracmat

Every function has a local variable <code>arg</code>. It is the function's actual argument. Pattern matching is used to dissect the argument, if needed. Functions in a pattern have an additional argument, <code>sjt</code>, which is bound to (part of) the subject of the pattern match operation. It is the part of the subject that the function, in the role of a pattern, attempts to match. These variables can be reassigned.

The names of the built-in functions <code>alc, arg, asc, chr, chu, clk, d2x, dbg, den, div, fil, flg, glf, fre, get, low, lst, mem, mod, new, pee, pok, put, ren, rev, rmv, sim, str, swi, sys, tbl, upp, utf, whl, x2d</code> can be used as variable names or names of user defined object member functions, but not as names of user defined functions. Conversely, the name <code>hash</code> can be used for user defined functions, but not for variables. Currentlty, <code>hash</code> is the only predefined object type.

If Bracmat starts in interactive mode, a few more variables are predefined: <code>!v</code> evaluates to a string telling which version of Bracmat you are running. <code>!w</code> and <code>!c</code> evaluate to sections 11 and 12 of the GPL. More vital is the variable <code>main</code>, which is the interpreter's main loop. Setting it to another value changes the behaviour of the interpreter. When running in interactive mode, <code>!</code> or <code>!""</code> evaluates to the last answer, so the empty string is the name of yet another special variable. These variables can be reassigned.


## C

C99 introduced the (read-only) special variable <code>__func__</code> (of type <code>static const char[]</code>) which holds the name of the current function.

Furthermore one could consider <code>errno</code> from <code><errno.h></code> as a special variable although it actually is a macro which expands to an modifiable lvalue of type <code>int</code>. Many library functions set it to a positive value in case of an error.


## Clojure

The following snippet prints a list of the special variables defined in clojure.core, in *earmuff* form.  For further information, consult the [http://clojuredocs.org/quickref/shortdesc/Clojure%20Core documentation].

```clojure

(apply str (interpose " " (sort (filter #(.startsWith % "*") (map str (keys (ns-publics 'clojure.core)))))))

```

```txt
*1 *2 *3 *agent* *allow-unresolved-vars* *assert* *clojure-version* *command-line-args* *compile-files* *compile-path*
*compiler-options* *data-readers* *default-data-reader-fn* *e *err* *file* *flush-on-newline* *fn-loader* *in* *math-context*
*ns* *out* *print-dup* *print-length* *print-level* *print-meta* *print-readably* *read-eval* *source-path* *unchecked-math*
*use-context-classloader* *verbose-defrecords* *warn-on-reflection*
```



## Common Lisp


Note: the term "special variable" has a meaning in Common Lisp and related dialects, different from the way it is being used here. It is a jargon which denotes a dynamically scoped variable. ANSI Common Lisp's standard-defined variables are special variables in both senses of the term.

The following code snippet prints a list of all 44 special variables defined by the Common Lisp standard. For further information about each of them consult the [http://www.lispworks.com/documentation/HyperSpec/Front/X_Alph_9.htm online documentation].

```lisp
(defun special-variables ()
  (flet ((special-var-p (s)
           (and (char= (aref s 0) #\*)
                (find-if-not (lambda (x) (char= x #\*)) s)
                (char= (aref s (1- (length s))) #\*))))
    (let ((lst '()))
      (do-symbols (s (find-package 'cl))
        (when (special-var-p (symbol-name s))
          (push s lst)))
      lst)))

(format t "~a~%" (sort (special-variables) #'string<))
```


```txt
(*BREAK-ON-SIGNALS* *COMPILE-FILE-PATHNAME* *COMPILE-FILE-TRUENAME* *COMPILE-PRINT* *COMPILE-VERBOSE* *DEBUG-IO*
 *DEBUGGER-HOOK* *DEFAULT-PATHNAME-DEFAULTS* *ERROR-OUTPUT* *FEATURES* *GENSYM-COUNTER* *LOAD-PATHNAME*
 *LOAD-PRINT* *LOAD-TRUENAME* *LOAD-VERBOSE* *MACROEXPAND-HOOK* *MODULES* *PACKAGE* *PRINT-ARRAY* *PRINT-BASE*
 *PRINT-CASE* *PRINT-CIRCLE* *PRINT-ESCAPE* *PRINT-GENSYM* *PRINT-LENGTH* *PRINT-LEVEL* *PRINT-LINES*
 *PRINT-MISER-WIDTH* *PRINT-PPRINT-DISPATCH* *PRINT-PRETTY* *PRINT-RADIX* *PRINT-READABLY* *PRINT-RIGHT-MARGIN*
 *QUERY-IO* *RANDOM-STATE* *READ-BASE* *READ-DEFAULT-FLOAT-FORMAT* *READ-EVAL* *READ-SUPPRESS* *READTABLE*
 *STANDARD-INPUT* *STANDARD-OUTPUT* *TERMINAL-IO* *TRACE-OUTPUT*)
```


Inside the REPL, there are more special variables available:

* -: Contains the form that is currently evaluated.

```txt
> (format t "~a" -)
(FORMAT T ~a -)
NIL
```


* *, **, ***: Contain the last, penultimate, antepenultimate primary values that were printed.

```txt
> (+ 1 2)
3
> (values 1 2)
1 ;
2
> (* 4 5)
20
> (list * ** ***)
(20 1 3)
```


* +, ++, +++: Contain the last, penultimate, antepenultimate forms that were evaluated.

```txt
> (+ 1 2)
3
> (values 1 2)
1 ;
2
> (* 4 5)
20
> (list + ++ +++)
((* 4 5) (VALUES 1 2) (+ 1 2))
```


* /, //, ///: Contain a list of the last, penultimate, antepenultimate values that were printed.

```txt
> (floor 10 2)
5 ;
0
> (values 1 'a "foo")
1 ;
A ;
"foo"
> (+ 1 2)
3
> (list / // ///)
((3) (1 A "foo") (5 0))
```



## D

In D there are not many special variables, beside a string[] argument of the main function. Variables like the C "errno" are usually not used, despite the C library is available.

One special boolean variable is __ctfe, that is read-only and it's true inside functions when they are evaluated at compile-time, and false otherwise.
=={{header|Déjà Vu}}==
Calls to some of the standard library functions can be optimized into certain opcodes or sequences of opcodes, namely:

```txt
set setglobal local get getlocal return recurse drop dup swap rot over
[] {} pop-from push-to push-through has get-from set-to raise reraise
call for pass
```

In addition, <code>eva</code> is special:

```dejavu
!print "hey" #is really short for
eva!print "hey"
```

EVA is the part of the standard library that takes care of communication with the outside world. It makes extensive use of the method call syntax, unlike the rest of the standard library, that is why it is special.


## DWScript

DWScript has no special variables in the base language.

Hosts can however define any number of contextual or environmental variables.


## Dyalect


Dyalect has a special <code>this</code> which is available inside methods:


```dyalect
func Integer.double() {
    this + this
}
print(8.double())
```



## Erlang

Erlang has no special variables.
What it does have are special functions, module_info/0 and module_info/1. These are added to a module automatically, without being present in the code.

```Erlang

-module( special_variables ).

-export( [task/0] ).

task() -> ok.

```

```txt

2> special_variables:module_info().
[{exports,[{task,0},{module_info,0},{module_info,1}]},
 {imports,[]},
 {attributes,[{vsn,[11317586745549911665324094732832680475]}]},
 {compile,[{options,[]},
           {version,"4.8"},
           {time,{2013,10,28,20,2,58}},
           {source,"/Users/bengt/rosetta/special_variables.erl"}]}]
3> special_variables:module_info(exports).
[{task,0},{module_info,0},{module_info,1}]

```



## Forth

In a standard system, there are a handful of predefined variables, such as:

* '''BASE''' contains the current input/output numeric base (default 10 for decimal).
* '''STATE''' is a flag telling whether the system is in interpret or compile mode.

The DO-LOOP construct also has accessors '''I''' and '''J''' (and sometimes '''K''') for obtaining the loop indices of the inner and outer nested loops.


### Common Practice

Although not mandated in the Forth language specification, traditionally the language implements system variables as what are called USER variables.  The name dates back to the a time when FORTH was used as a multi-user O/S and therefore each user needed a set of variables to control the state of their instance of the system. The user variables exist in a memory block called the USER AREA and are replicated for each task. When used for embedded systems, Forth is commonly implemented as a multi-tasking system, so this architecture is still relevant today.  On a context switch the system assigns a system VARIABLE called 'UP' (user pointer) to point to the new task's USER AREA. Using UP, the task's local stack pointers can be read into the machine and a fast context switch can be completed. UP is commonly held in a CPU register on machines that have larger register sets.

This following list is an example of a set of USER variables in a small system. Consult the implementation documents for details on the USER variables in a specific FORTH system.

```txt
Name      Type          Description
---------------------------------------------
TIB      integer    Terminal Input Buffer address
U0       integer    current user area address
>IN      integer    holds offset into TIB, used for parsing
BASE     integer    holds number conversion radix
STATE    integer    holds compiler state (true=compiling, false=interpreting)
DP       integer    holds dictionary memory pointer
'SOURCE  integer[2] contains length and address of input source
LATEST   integer    address of last word added to dictionary
HP       integer    HOLD pointer, used for number formatting routines
LP       integer    leave-stack pointer, used by do loops
S0       integer    end of parameter stack
PAD      chars[80]  Generic buffer. (size is implementation dependent)
L0       integer    bottom of leave stack
R0       integer    end of return stack
```


It is worth noting that any of these variable names could be used in a program.
The result would be that the original variable would be hidden from the compiler as the new variable with the same name would be found first. In other words the function of the original variable would NOT change in system.
Forth has a "hyper-static" name space.


## Fortran

Fortran offers no special variables such as Pi, e, etc. as a part of the language, not even the modern special floating-point "values" such as NaN. Indeed, the syntax has no reserved words generally so that <code>GO TO</code> could be the name of a variable without damage to GO TO statements, though it is generally agreed that calling a variable END is provocative... It does have some ''numbers'' that are special: 5 is the input/output "unit number" for keyboard input and 6 for output to "standard output", the screen on desktop computers; in the past there have been other values that were associated to devices such as the card reader, card punch, lineprinter, paper tape reader, and so on at any given installation. But these constants are not given names as mnemonics for their special values, except by the programmer. There is no equivalent of SYSOUT as in WRITE(SYSOUT,''etc'' without definition by the programmer.

Certain statements involve special names in what appear to be assignments of values to or from a special name that has a value just like a named variable does, but these are ''not'' proper variables at all. For instance, in
```Fortran
           INQUIRE(FILE = FILENAME(1:L),EXIST = EXIST,	!Here we go. Does the file exist?
     1      ERR = 666,IOSTAT = IOSTAT) 		!Hopefully, named in good style, etc.
           IF (EXIST) THEN	!So, does the named file already exist?
           ...etc.
```

ERR is a special name, but only inside the context of the INQUIRE (and OPEN, and WRITE, ''etc.'') statement, it is not the name of an existing variable outside that statement whether defined by the language or by the programmer, and if the programmer were to define a variable called ERR it would have no relevance within that INQUIRE statement - though <code>ERR = ERR</code> ''would'' be workable if an ASSIGN statement had assigned statement label 666 to variable ERR. Similarly, the variable named FILENAME is declared by the programmer and because there are no reserved words, could be just FILE. Likewise, EXIST is declared (as LOGICAL) and IOSTAT (as INTEGER) as a mnemonic aid and also to save on the trouble of remembering whether the assignment works left-to-right or right-to-left in each case. It is right-to-left for FILE = ''filename'', input to the INQUIRE statement and left-to-right for EXIST = ''variable'', an output of the INQUIRE statement.


## Go

Go has no special variables in the base language.

A number of the standard packages however, define special variables or constants.  Standard error values are common, as are enumeration-like constants for controling functions.  Examples of some more frequently used package variables might be io.EOF, os.Args, and os.Stdout.

See also [[Topic variable#Go]] for an example of '.' of the template package.


## Haskell


Like C, there are practically no special variables. The program entry point, main, is the one exception.

=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon have special variables known as keywords which are syntactically are preceded by an &.

```Unicon

# &keyword # type returned(indicators) - brief description
# indicators:
#    * - generates multiple values
#    = - modifiable
#    ? - may fail (e.g. status inquiry)
#    U - Unicon
#    G - Icon or Unicon with Graphics
#
&allocated   # integer(*) - report memory allocated in total and by storage regions
&ascii       # cset -  ASCII character set
&clock       # string - time of day
&col         # integer(=G) - column location of pointer
&collections # integer(*) - garbage collection activity in total and by storage region
&column      # integer(U) - source code column
&control     # null(?G) - control key state
&cset        # cset - universal character set
&current     # co-expression - current co-expression
&date        # string - today's date
&dateline    # string - time stamp
&digits      # cset - digit characters
&dump        # integer(=) - termination dump
&e           # real - natural log e
&error       # integer(=) - enable/disable error conversion/fail on error
&errno       # integer(?) - variable containing error number from previous posix command
&errornumber # integer(?) - error number of last error converted to failure
&errortext   # string(?) - error message of last error converted to failure
&errorvalue  # any(?) - erroneous value of last error converted to failure
&errout      # file - standard error file
&eventcode   # integer(=U) - program execution event in monitored program
&eventsource # co-expression(=U) - source of events in monitoring program
&eventvalue  # any(=U) - value from event in monitored program
&fail        # none - always fails
&features    # string(*) - identifying features in this version of Icon/Unicon
&file        # string -  current source file
&host        # string - host machine name
&input       # file - standard input file
&interval    # integer(G) - time between input events
&lcase       # cset - lowercase letters
&ldrag       # integer(G) - left button drag
&letters     # cset - letters
&level       # integer - call depth
&line        # integer - current source line number
&lpress      # integer(G) - left button press
&lrelease    # integer(G) - left button release
&main        # co-expression - main task
&mdrag       # integer(G) - middle button drag
&meta        # null(?G) - meta key state
&mpress      # integer(G) - middle button press
&mrelease    # integer(G) - middle button release
&now         # integer(U) - current time
&null        # null - null value
&output      # file - standard output file
&pick        # string (U) - variable containing the result of 3D selection
&phi         # real - golden ratio
&pos         # integer(=) - string scanning position
&progname    # string(=) - program name
&random      # integer(=) - random number seed
&rdrag       # integer(G) - right button drag
&regions     # integer(*) - region sizes
&resize      # integer(G) - window resize
&row         # integer(=G) - row location of pointer
&rpress      # integer(G) - right button press
&rrelease    # integer(G) - right button release
&shift       # null(?G) - shift key state
&source      # co-expression - invoking co-expression
&storage     # integer(*) - memory in use in each region
&subject     # string - string scanning subject
&syserr      # integer - halt on system error
&time        # integer(=) - elapsed time in milliseconds
&trace       # integer(=) - trace program
&ucase       # cset - upper case letters
&version     # string - version
&window      # window(=G) - the current graphics rendering window
&x           # integer(=G) - pointer horizontal position
&y           # integer(=G) - pointer vertical position
# keywords may also fail if the corresponding feature is not present.
# Other variants of Icon (e.g. MT-Icon) will have different mixes of keywords.
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>BLACK   - The code of colour black.
BLUE    - The code of colour blue.
CYAN    - The code of colour cyan.
DATE$   - The current date in the standard format.
EXLINE  - The number of the last statement that caused an exception.
EXTYPE  - The error code of the last exceotion.
FREE    - The amount of memory free and avaible to the current program.
GREEN   - The code of colour green.
INF     - The largest positive number that the IS-BASIC can candle.
MAGENTA - The code of colour magenta.
PI      - Value of the Pi. This is rounded to 3.141592654
RED     - The code of colour red.
TIME$   - The current time in the standard format.
WHITE   - The code of colour white.
YELLOW  - The code of colour yellow.
```



## J



### special local variables


The names <code>x y u v m n</code> are used as parameters in explicit J definitions:

   y:  right argument
   x:  (optional) left argument
   u:  left argument to an adverb or conjunction
   v:  right argument to a conjunction
   m:  left noun argument to an adverb or conjunction (value error if verb provided)
   n:  right noun argument to a conjunction (value error if verb provided)

Note that the result of an adverb or conjunction that uses either x or y and one of these other names is always a verb.  In this case, x and/or y represent arguments passed to the derived verb.

These names may be used as regular names, but that is bad practice except in the context of debugging or illustration.


### special global variables

In J 602:  Names in locale z are in the path for all locales, including the default locale (base) -- these provide "language features":
<pre style="height:30ex;overflow:scroll">   names_z_''
ARGV           BINPATH        CR             CRLF           DEL            Debug
EAV            EMPTY          FF             FIXFONT        FIXFONTWH      IF64
IFCONSOLE      IFGTK          IFJ6           IFJAVA         IFJHS          IFUNIX
IFWIN          IFWIN32        IFWINCE        IFWINE         IFWINNT        JVERSION
LF             LF2            Note           PROFONT        SYSPPC         TAB
UNAME          adverb         apply          assert         bind           boxopen
boxxopen       break          bx             clear          coclass        cocreate
cocurrent      codestroy      coerase        cofullname     coinsert       coname
conames        conew          conjunction    conl           copath         coreset
cutopen        datatype       def            define         do             drop
dyad           each           edit           empty          erase          every
exit           expand         fetch          inv            inverse        items
jcwdpath       jhostpath      jpath          jpathsep       jsystemdefs    leaf
list           load           loadd          loadp          mbopen         mbsave
monad          nameclass      namelist       names          nc             nl
noun           on             open           pick           require        rows
script         scriptd        scripts        setbreak       sign           sminfo
smoutput       sort           split          startupconsole startupide     table
take           tmoutput       toCRLF         toHOST         toJ            tolower
toupper        type           ucp            ucpcount       utf8           uucp
verb           wcsize         wd             wdbox          wdcenter       wdclipread
wdclipwrite    wde            wdfit          wdforms        wdget          wdhandler
wdinfo         wdishandle     wdisparent     wdmove         wdpclose       wdqshow
wdquery        wdreset        wdselect       wdstatus       winpathsep
```


Names in the locales j and jijs are available by explicity referencing those locales and are used to provide "system features" and "ide features":
<pre style="height:30ex;overflow:scroll">   names_j_''
BOXES              BROWSER            CONFIRMCLOSE       DIRTREEX           DISPLAYLOAD
EPSREADER          FORMAT             FORMSIZES          GetSystemMetrics   IFJIJX
INPUTLOG           INPUTLOGFILE       LOADED             P2UPFONT           PATHJSEP
PATHSEP            PDFREADER          PRINTERFONT        PRINTOPT           PUBLIC
READONLY           SCRIPTS            SHOWSIP            SMPRINT            SM_CMONITORS
SM_CXVIRTUALSCREEN SM_CYVIRTUALSCREEN SM_XVIRTUALSCREEN  SM_YVIRTUALSCREEN  STARTUP
SYSTEMFOLDERS      TARGET             USERFOLDERS        WINPOS             XDIFF
addfname           boxdraw            buildpublic        classwizard        cleantable
config             cutnames           deb                debug              demos
dirmatch           dltb               edit               editfind           editinputlog
editinputprompt    exist              extijs             fexist             fif
filenewform        fileprint          fileprintsetup     filex              fixWINPOS
formedit           formeditrun        forms              fullname           getinputlog
getpath            getscripts         gettarget          gettargetlocale    globaldefs
gridwizard         help               htmlhelp           jpath              lab
lastactive         loadp              open               openfiles          origin
pacman             printfiles         prints             projectmanager     save
saveuserfolders    scriptmake         scripts            wpreset            wpsave
wpset
   names_jijs_''
EMPTY                      FIXFONTDEF                 FKEYS
FTYPES                     IFIOX                      IFMAX
IFREADONLY                 IFSAVED                    IFSHOW
JIJS                       JIJSMAC                    JRECENT
NEWUSER                    PPSCRIPT                   QFORMX
RECENT                     RECENTFILE                 RECENTLOC
RECENTMAX                  SCMP                       SMBLK
SMDESK                     SMHWNDP                    SMINIT
SMNAME                     SMPATH                     SMSEL
SMSIZE                     SMSTYLE                    SMTEXT
SMTORG                     aboutj                     boxfkeys
boxskeys                   checkreadonly              cleartemp
close                      closeijs                   closeijx
closewindows               comparesvn                 create
cutpara                    deb                        destroy
exitijs                    filecase                   fkeycase
fkeylist                   fkeyrun                    fkeyselect
fkeyselect1                flerase                    flexist
flopen                     flread                     flwrite
foldpara                   foldtext                   getSMSEL
getactsize                 getcascade                 getcascade1
getcascades                getfile                    getformx
getline                    getsaveas                  getscrollpos
getselection               getskey                    id2loc
id2name                    id2names                   id2type
ide_maximize               ide_minimize               ide_restore
ifshiftkey                 iftempscript               info
intn                       jijs_aboutj_button         jijs_actrl_fkey
jijs_bctrl_fkey            jijs_bctrlshift_fkey       jijs_cancel
jijs_close                 jijs_close_button          jijs_dctrl_fkey
jijs_default               jijs_demos_button          jijs_ectrl_fkey
jijs_ectrlshift_fkey       jijs_editconfigure_button  jijs_editcopy_button
jijs_editcut_button        jijs_editdirmatch_button   jijs_editexport_button
jijs_editfif_button        jijs_editfind_button       jijs_editformedit_button
jijs_editinputlog_button   jijs_editlint_button       jijs_editpaste_button
jijs_editreadonly_button   jijs_editredo_button       jijs_editselectall_button
jijs_editundo_button       jijs_f1_fkey               jijs_f1ctrl_fkey
jijs_f1shift_fkey          jijs_fctrl_fkey            jijs_fctrlshift_fkey
jijs_filecleartemp_button  jijs_fileexit_button       jijs_filenewclass_button
jijs_filenewijs_button     jijs_fileopen_button       jijs_fileopensystem_button
jijs_fileopenuser_button   jijs_fileprint_button      jijs_fileprintsetup_button
jijs_filerecent_button     jijs_fkeys_button          jijs_forms_button
jijs_gctrl_fkey            jijs_hctrl_fkey            jijs_helpconstants_button
jijs_helpcontext_button    jijs_helpcontrols_button   jijs_helpdictionary_button
jijs_helpforeigns_button   jijs_helpgeneral_button    jijs_helpgl2cmd_button
jijs_helphelp_button       jijs_helpindex_button      jijs_helpphrases_button
jijs_helpprimer_button     jijs_helprelease_button    jijs_helprelnotes_button
jijs_helpuser_button       jijs_helpvocab_button      jijs_helpwdcmd_button
jijs_helpwdover_button     jijs_ictrl_fkey            jijs_jctrl_fkey
jijs_kctrl_fkey            jijs_kctrlshift_fkey       jijs_labadvance_button
jijs_labauthor_button      jijs_labchapters_button    jijs_labs_button
jijs_lctrl_fkey            jijs_lctrlshift_fkey       jijs_max_button
jijs_mctrl_fkey            jijs_nctrl_fkey            jijs_nctrlshift_fkey
jijs_octrl_fkey            jijs_pctrl_fkey            jijs_qctrl_fkey
jijs_rctrl_fkey            jijs_rctrlshift_fkey       jijs_rundebug_button
jijs_runfile_button        jijs_runfiled_button       jijs_runline_button
jijs_runpacman_button      jijs_runprojman_button     jijs_runselection_button
jijs_runwindow_button      jijs_runwindowd_button     jijs_save_button
jijs_saveas_button         jijs_sctrl_fkey            jijs_sellower_button
jijs_selminus_button       jijs_selplus_button        jijs_selplusline1_button
jijs_selplusline2_button   jijs_selsort_button        jijs_seltoggle_button
jijs_selupper_button       jijs_selwrap_button        jijs_tctrl_fkey
jijs_tile_button           jijs_tileacross_button     jijs_tilecascade_button
jijs_togglebox_button      jijs_wctrl_fkey            jijs_wctrlshift_fkey
jrecent_cancel             jrecent_close              jrecent_enter
jrecent_lb_button          jrecent_open_button        jrecent_run
jrecent_run_button         jrecent_rund_button        jrecent_view_button
lint                       marksavedid                name2id
new                        newijs                     newijx
nounrep                    openijs                    parentname
pathname                   pmovex                     qsmact
qsmall                     qsmallforms                qsmallijs
qsmlastijs                 qsmlastxs                  qsmout
qsmsize                    query                      quote
rdist                      readid                     readid16
readonlydefault            recent_open                recent_put
recent_read                recent_run                 recent_save
resizefont                 restorefont                roundint
runcompare                 runexport                  runfile
runimmx0                   runimmx1                   runline
runselection               runwindow                  save
saveas                     saveopenwindows            scmp_cancel
scmp_close                 scmp_close_button          scmp_current_button
scmp_original_button       scmp_revert_button         scmp_run
scmp_show                  select_line                select_text
set_fkeys                  set_skey1                  set_skeys
setfontall                 setpnall                   setreadonly
sh                         smappend                   smclose
smfocus                    smfocusact                 smfocusout
smgetsel                   smmove                     smopen
smprompt                   smread                     smreplace
smsave                     smscroll                   smsel
smselact                   smselout                   smsetcmd
smsetsaved                 smsetselect                smwrite
sysmodifiers               tile                       tile2fit
tile2fit1                  tileacross                 tilecascade
tileget                    todelim                    tofoldername
togglebox                  togglereadonly             togglexs
tolist                     topara                     ucpboxdraw
unboxfkeys                 unboxskeys                 winmax
```




## Java


Java has only a few special variables. There is a <code>String</code>-Array for passing command-line-arguments to the program, and there is a <code>Class</code>-Object that can be accessed in a variable-like manner. It is used for reflection, (like examining and modifing class members, their type and modifiers during runtime).
There is the <code>System</code>-"Object" that contains various (mostly static) data about the enviroment the Java VM runs on, and it's cousin <code>Runtime</code> that provides data that is more prone to change during runtime, like available CPU cores and RAM.

Inside an object there is <code>this</code>, a reference that points to the object itself (like 127.0.0.1 in networking) and are used to qualify member access. There is also <code>super</code> that does the same for the base class (actually "the next class in the inheritance tree"). Both are not demonstrated in the example below.


```Java
import java.util.Arrays;

public class SpecialVariables {

    public static void main(String[] args) {

        //String-Array args contains the command line parameters passed to the program
        //Note that the "Arrays.toString()"-call is just used for pretty-printing
        System.out.println(Arrays.toString(args));

        //<Classname>.class might qualify as a special variable, since it always contains a Class<T>-object that
        //is used in Reflection
        System.out.println(SpecialVariables.class);


        //The following are not really "variables", since they are properly encapsulated:

        //System.getenv() returns a String-String-Map of environment-variables
        System.out.println(System.getenv());

        //System.getProperties() returns a Map of "things somebody might want to know", including OS and architecture
        // the Java VM runs on, various paths like home direcoty of the user that runs the program, class (library) paths,
        System.out.println(System.getProperties());

        //Runtime.getRuntime() returns a Runtime-Object that contains "changing" data about the running Java VM's
        // environment, like available processor cores or available RAM
        System.out.println(Runtime.getRuntime().availableProcessors());

    }
}


```



## JavaScript


<code>this</code> evaluates to the object the immediately enclosing function was called on as a method, if it was. If it was not called as a method, <code>this</code> is either the global environment object (usually <code>window</code> in browsers) in non-strict mode, or <code>undefined</code> in strict mode. <code>this</code> is an expression resembling a variable, but not actually a variable; for example, it is a syntax error to assign to it.


```javascript
var obj = {
  foo: 1,
  bar: function () { return this.foo; }
};
obj.bar(); // returns 1
```


When a function is entered, the ''variable'' <code>arguments</code> is bound to an “arguments object” which is an array-like object containing the function's arguments, as well as some other information. This how [[varargs]] functions are implemented in JavaScript. If the function's parameters contain “<code>arguments</code>” explicitly, then it is ''not'' overridden and functions as an ordinary parameter.


```javascript
function concat() {
  var s = "";
  for (var i = 0; i < arguments.length; i++) {
    s += arguments[i];
  }
  return s;
}
concat("a", "b", "c"); // returns "abc"
```



## jq

Variables in jq are identifiers preceded by the sigil "$", e.g. <code>$x</code>. There are no predefined variables, but jq does allow variables to be assigned string values on the command line.

For example:
```sh
$ jq -n -M --arg x 1 '$x|type'   # (*)
"string"
```

(*) Windows users would write "$x|type".


## Julia

Julia starts with the <code>Base</code> module loaded.  Taking "special variables" to mean names in the default global namespace (<code>Base</code>) that aren't functions, types, or modules, then you can obtain them with

```julia
join(sort(filter(sym -> let n=eval(sym); !(isa(n, Function) || isa(n, Type) || isa(n, Module)); end, names(Base))), ", ")
```

```txt
":, ARGS, CPU_CORES, C_NULL, DL_LOAD_PATH, DevNull, ENDIAN_BOM, ENV, I, Inf, Inf16, Inf32, InsertionSort, JULIA_HOME, LOAD_PATH, MS_ASYNC, MS_INVALIDATE, MS_SYNC, MergeSort, NaN, NaN16, NaN32, OS_NAME, QuickSort, RTLD_DEEPBIND, RTLD_FIRST, RTLD_GLOBAL, RTLD_LAZY, RTLD_LOCAL, RTLD_NODELETE, RTLD_NOLOAD, RTLD_NOW, RoundDown, RoundFromZero, RoundNearest, RoundToZero, RoundUp, STDERR, STDIN, STDOUT, VERSION, WORD_SIZE, catalan, cglobal, e, eu, eulergamma, golden, im, pi, γ, π, φ"
```

(Because of Julia's multiple dispatch that allows a single function to have multiple definitions for different argument types, combined with the ability of functions in modules or variables in the local scope to safely shadow names in the global namespace, having a large global namespace is seen as a convenience rather than a problem.)


## Kotlin

There are two 'special variables' that I can think of in Kotlin:

*  'it' which implicitly refers to the parameter of a lambda expression where it only has one.

* 'field' which implicitly refers to the backing field of a property within its get/set accessors.


The following program illustrates their usage:

```scala
// version 1.0.6

class President(val name: String) {
    var age: Int = 0
        set(value) {
           if (value in 0..125) field = value  // assigning to backing field here
           else throw IllegalArgumentException("$name's age must be between 0 and 125")
        }
}

fun main(args: Array<String>) {
    val pres = President("Donald")
    pres.age = 69
    val pres2 = President("Jimmy")
    pres2.age = 91
    val presidents = mutableListOf(pres, pres2)
    presidents.forEach {
        it.age++  // 'it' is implicit sole parameter of lambda expression
        println("President ${it.name}'s age is currently ${it.age}")
    }
    println()
    val pres3 = President("Theodore")
    pres3.age = 158
}
```


```txt

President Donald's age is currently 70
President Jimmy's age is currently 92

Exception in thread "main" java.lang.IllegalArgumentException: Theodore's age must be between 0 and 125
        at President.setAge(test10.kt:5)
        at Test10Kt.main(test10.kt:21)

```



## Lasso


In Lasso parameters can be referenced as numerical locals within methods or unbound captures. [http://lassoguide.com/language/variables.html?#parameter-pseudo-locals]


```Lasso
{return #1 + ':'+#2}('a','b') // a:b
```



```Lasso
define test(a,b) => #1+':'+#2
test('y','z') // y:z
```



## Lingo

In terms of statement "put <varName>" showing some meaningful output, the following can be rated as "special variables" in Lingo:

-- constants
*BACKSPACE
*EMPTY
*ENTER
*FALSE
*PI
*QUOTE
*RETURN
*SPACE
*TAB
*TRUE
*VOID
<br />
-- core objects
*_global
*_key
*_mouse
*_movie
*_player
*_system


## LiveCode

The most important special variable is known as ''it''. The LC Dictionary says "A special local variable that is used with commands such as get, read from file, convert, ask, and answer. Use the it keyword to get the result of certain commands, or as a handy temporary storage place." It's use is closely followed by a special function called ''result'' that is similarly mentioned in the dictionary as well "Is a global property returning the last value returned by return from a handler, from an engine function, or from an engine command which sets the result."

Further to those, LiveCode comes with a plethora of built-in constants, which are readily listed with the following command:

```LiveCode
put the constantNames>
```
It also provides colours as built-ins, accessible through
```LiveCode
the colornames>
```
 You can search the dictionary in the IDE using text "names" to discover more such as the ''propertyNames'' & the ''commandNames'', though are not strictly pertinent to this task.



## Lua


; arg : global table containing command line parameters with the name of the program at index 0. Not available in interactive mode
; _G : the table with the ''global environment'', i.e. all global variables, including itself and the other variables listed here
; _VERSION : string with the name of the interpreter and the major and minor version, e.g. "Lua 5.2"

To list all global variables:


```lua
for n in pairs(_G) do print(n) end
```


The list will include built-in global functions, whose availability depends on the implementation and compile time configuration.

## M2000 Interpreter

There some read only variables. We can use Help dir$ to get help about dir$.

All identifiers can be change to be used as variables, using a dot. For modules/functions in a group we have to define these variables using a dot.


```M2000 Interpreter

Module Checkit {
      Let inkey$="hello", dir$="Something Else"
      \\ using a dot we tell to interpreter to skip internal identifiers,
      \\ and look for user variables
      Print .inkey$="hello", .dir$="Something Else"

      Print dir$   ' return current path
      do
            Print "wait to press space"
      Until inkey$=" "
}
Checkit
Module check2 {
      Global inkey$="ok"
      Print .inkey$="ok"
}
check2

Module Check3 {
      Group A {
            Module Check3 {
                  \\ using a dot before the name
                  .inkey$="ok"
                  Print .inkey$="ok"
            }
      }
      A.Check3
}
Check3


```




```txt

about$, appdir$, browser$, clipboard$, clipboard.image$, codepage, colors,
command$, computer$, control$, dir$, duration, empty, error$, field,
fontname$, grabframe$, height, hwnd, inkey$, islet, isnum, key$, lan$,
letter$, memory, menu.visible, menu, menuitems, mode, module$, monitor.stack,
monitor.stack.size, motion.wx, motion.wy, motion.x, motion.xw, motion.y, motion.yw,
mouse, mouse.key, mouse.x, mouse.y, mousea.x, mousea.y, movie.counter, movie.device$,
movie.error$, movie.status$, movie, music.counter, now, number, os$, osbit, parameters$,
pen, platform$, point, pos, pos.x, pos.y, printername$, properties$, reportlines, rnd,
row, scale.x, scale.y, speech, sprite$, stack.size, tab, tempname$, temporary$,
this, threads$, tick, timecount, today, twipsx, twipsy, user.name$, volume,
width, x.twips, y.twips

```





## Mathematica


```Mathematica
Grid[Partition[Names["$*"],4]]
->
$Aborted                       $ActivationGroupID             $ActivationKey                 $ActivationUserRegistered
$AddOnsDirectory               $AllowDataUpdates              $AllowDocumentationUpdates     $AllowInternet
$AssertFunction                $Assumptions                   $BaseDirectory                 $BatchInput
$BatchOutput                   $BoxForms                      $ByteOrdering                  $Canceled
$CharacterEncoding             $CharacterEncodings            $CommandLine                   $CompilationTarget
$ConditionHold                 $ConfiguredKernels             $Context                       $ContextPath
$ControlActiveSetting          $CreationDate                  $CurrentLink                   $DateStringFormat
$DefaultFont                   $DefaultFrontEnd               $DefaultImagingDevice          $DefaultPath
$Display                       $DisplayFunction               $DistributedContexts           $DynamicEvaluation
$Echo                          $Epilog                        $ExportFormats                 $Failed
$FinancialDataSource           $FormatType                    $FrontEnd                      $FrontEndSession
$GeoLocation                   $HistoryLength                 $HomeDirectory                 $IgnoreEOF
$ImagingDevices                $ImportFormats                 $InitialDirectory              $Input
$InputFileName                 $Inspector                     $InstallationDate              $InstallationDirectory
$InstalledServices             $InterfaceEnvironment          $InternetProxyRules            $IterationLimit
$KernelCount                   $KernelID                      $Language                      $LaunchDirectory
$LibraryPath                   $LicenseExpirationDate         $LicenseID                     $LicenseProcesses
$LicenseServer                 $LicenseSubprocesses           $LicenseType                   $Line
$Linked                        $LinkSupported                 $LoadedFiles                   $MachineAddresses
$MachineDomain                 $MachineDomains                $MachineEpsilon                $MachineID
$MachineName                   $MachinePrecision              $MachineType                   $MaxExtraPrecision
$MaxLicenseProcesses           $MaxLicenseSubprocesses        $MaxMachineNumber              $MaxNumber
$MaxPiecewiseCases             $MaxPrecision                  $MaxRootDegree                 $MessageGroups
$MessageList                   $MessagePrePrint               $Messages                      $MinMachineNumber
$MinNumber                     $MinorReleaseNumber            $MinPrecision                  $ModuleNumber
$NetworkLicense                $NewMessage                    $NewSymbol                     $Notebooks
$NumberMarks                   $Off                           $OperatingSystem               $Output
$OutputForms                   $OutputSizeLimit               $Packages                      $PacletSite
$ParentLink                    $ParentProcessID               $PasswordFile                  $PatchLevelID
$Path                          $PathnameSeparator             $PerformanceGoal               $PipeSupported
$Post                          $Pre                           $PreferencesDirectory          $PrePrint
$PreRead                       $PrintForms                    $PrintLiteral                  $PrintServiceRequest
$PrintServiceResponse          $PrintShortErrorMessages       $PrintWSDLDebug                $ProcessID
$ProcessorCount                $ProcessorType                 $ProductInformation            $ProgramName
$RandomState                   $RecursionLimit                $ReleaseNumber                 $RootDirectory
$ScheduledTask                 $ScriptCommandLine             $SessionID                     $SetParentLink
$SharedFunctions               $SharedVariables               $SoundDisplay                  $SoundDisplayFunction
$SuppressInputFormHeads        $SynchronousEvaluation         $SyntaxHandler                 $System
$SystemCharacterEncoding       $SystemID                      $SystemWordLength              $TemporaryDirectory
$TemporaryPrefix               $TextStyle                     $TimedOut                      $TimeUnit
$TimeZone                      $TopDirectory                  $TraceOff                      $TraceOn
$TracePattern                  $TracePostAction               $TracePreAction                $Urgent
$UserAddOnsDirectory           $UserBaseDirectory             $UserBasePacletsDirectory      $UserDocumentsDirectory
```






## Maxima


```maxima
/* There are many special variables in Maxima: more than 250 are used for options, for example */
fpprec;  /* precision for big floats */
obase;   /* number base for output */

/* Other variables are read-only, and give the list of user-defined variables, functions... */
infolists; /* give the names of all available lists */
[labels, values, functions, macros, arrays, myoptions, props, aliases, rules, gradefs, dependencies, let_rule_packages, structures]
```



## ML/I


### Input


```ML/I
MCSKIP "WITH" NL
"" Special variables
"" There are four different kinds of variables in ML/I.
"" Permanent (P) variables - these have no special predefined values.
"" Character (C) variables - these have no special predefined values.
"" Temporary (T) variables - a macro has at least three of these, and
""    those have predefined values.
"" System (S) variables - these are for control and status. The number
""    of these is implementation dependent.
MCSKIP MT,<>
MCINS %.
MCDEF TVARDEMO , NL
AS <T-variables are local to the current macro call
T1 is the number of arguments to current macro call - value is %T1.
T2 is the number of macro calls so far - value is %T2.
T3 is the current depth of nesting - value is %T3.
>
TVARDEMO xxx,yyy

MCDEF SVARDEMO WITHS NL
AS <The first nine S-variables are implementation independent
S1 controls startline insertion - value is %S1.
S2 is the current source text line number - value is %S2.
S3 controls error messages related to warning markers - value is %S3.
S4 controls context printout after a <MCNOTE> - value is %S4.
S5 is the count of processing errors - value is %S5.
S6 enables the definition of an atom to be changed - value is %S6.
S7, S8 and S9 are currently unused.

All other S-variables have implementation defined meanings.
>
SVARDEMO
```



### Output


```ML/I
T-variables are local to the current macro call
T1 is the number of arguments to current macro call - value is 2
T2 is the number of macro calls so far - value is 5
T3 is the current depth of nesting - value is 1

The first nine S-variables are implementation independent
S1 controls startline insertion - value is 0
S2 is the current source text line number - value is 32
S3 controls error messages related to warning markers - value is 0
S4 controls context printout after a MCNOTE - value is 0
S5 is the count of processing errors - value is 0
S6 enables the definition of an atom to be changed - value is -1
S7, S8 and S9 are currently unused.

All other S-variables have implementation defined meanings.
```



## NetRexx

For convenience, NetRexx provides some special names for naming commonly-used concepts within terms. These are only recognized if there is no variable of the same name previously seen in the current scope.

The current set of special names includes:
<code>'''ask''', '''class''', '''digits''', '''form''', '''length''', '''null''', '''source''', '''sourceline''', '''super''', '''this''', '''trace''', '''version'''</code>.


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols binary

class RCSpecialVariables

method RCSpecialVariables()
  x = super.toString
  y = this.toString
  say '<super>'x'</super>'
  say '<this>'y'</this>'
  say '<class>'RCSpecialVariables.class'</class>'
  say '<digits>'digits'</digits>'
  say '<form>'form'</form>'
  say '<[1, 2, 3].length>'
  say [1, 2, 3].length
  say '</[1, 2, 3].length>'
  say '<null>'
  say null
  say '</null>'
  say '<source>'source'</source>'
  say '<sourceline>'sourceline'</sourceline>'
  say '<trace>'trace'</trace>'
  say '<version>'version'</version>'

  say 'Type an answer:'
  say '<ask>'ask'</ask>'

  return

method main(args = String[]) public static

  RCSpecialVariables()

  return

```

;Output

```txt

<super>RCSpecialVariables@3487a5cc</super>
<this>RCSpecialVariables@3487a5cc</this>
<class>class RCSpecialVariables</class>
<digits>9</digits>
<form>scientific</form>
<[1, 2, 3].length>
3
</[1, 2, 3].length>
<null>

</null>
<source>Java method RCSpecialVariables.nrx</source>
<sourceline>21</sourceline>
<trace>off</trace>
<version>NetRexx 3.00 11 Jun 2011</version>
Type an answer:
answer
<ask>answer</ask>

```



## OASYS

The only special variable is <tt>player</tt> (of type <tt>object</tt>) which specifies which object the player invokes methods on by default.


## OASYS Assembler

The only special variable is <tt>%@</tt> which specifies which object the player invokes methods on by default.


## OCaml


Some predefined variables from the <code>[http://caml.inria.fr/pub/docs/manual-ocaml/libref/Sys.html Sys]</code> module:


```ocaml
val argv : string array
(** The command line arguments given to the process.
   The first element is the command name used to invoke the program.
   The following elements are the command-line arguments
   given to the program. *)

val executable_name : string
(** The name of the file containing the executable currently running. *)

val interactive : bool ref
(** This reference is initially set to [false] in standalone
   programs and to [true] if the code is being executed under
   the interactive toplevel system [ocaml]. *)

val os_type : string
(** Operating system currently executing the Caml program. One of
-  ["Unix"] (for all Unix versions, including Linux and Mac OS X),
-  ["Win32"] (for MS-Windows, OCaml compiled with MSVC++ or Mingw),
-  ["Cygwin"] (for MS-Windows, OCaml compiled with Cygwin). *)

val word_size : int
(** Size of one word on the machine currently executing the Caml
   program, in bits: 32 or 64. *)

val max_string_length : int
(** Maximum length of a string. *)

val max_array_length : int
(** Maximum length of a normal array.  The maximum length of a float
    array is [max_array_length/2] on 32-bit machines and
    [max_array_length] on 64-bit machines. *)

val ocaml_version : string
(** [ocaml_version] is the version of Objective Caml.
    It is a string of the form ["major.minor[.patchlevel][+additional-info]"],
    where [major], [minor], and [patchlevel] are integers, and
    [additional-info] is an arbitrary string. The [[.patchlevel]] and
    [[+additional-info]] parts may be absent. *)
```


Some predefined variables from the <code>[http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html Pervasives]</code> module:


```ocaml
val max_int : int
(** The greatest representable integer. *)

val min_int : int
(** The smallest representable integer. *)

val max_float : float
(** The largest positive finite value of type [float]. *)

val min_float : float
(** The smallest positive, non-zero, non-denormalized value of type [float]. *)

val epsilon_float : float
(** The difference between [1.0] and the smallest exactly representable
    floating-point number greater than [1.0]. *)
```



## Oforth


Oforth special variables are read-only :

```txt

true
false
null
System.Out
System.In
System.Err
System.Console
System.Args
Systel.NbCores
System.CELLSIZE
System.VERSION
SYstem.MAXTHREADS
System.ASSERTMODE
System.ISWIN
System.ISLUNIX

```



## PARI/GP

There are three special variables in GP: <code>Pi</code> (3.14...), <code>Euler</code> (Euler's gamma), and <code>I</code> (the imaginary unit).

PARI has many special variables.  Probably the most important is <code>avma</code>, the current stack pointer, and the related <code>top</code> and <code>bot</code>; see section 4.3 of the User's Guide to the PARI Library.

Other important special variables are:
* Universal objects <code>gen_0</code>, <code>gen_1</code>, <code>gen_2</code>, <code>gen_m1</code>, <code>gen_m2</code>, <code>ghalf</code>, and <code>gnil</code>. (The manual erroneously omits the last on this list.)  These can be identified with <code>is_universal_constant()</code>.  Note that <code>gen_I</code> is ''not'' a universal object but a function.
* Defaults: <code>DEBUGLEVEL</code> and <code>DEBUGMEM</code>
* Prime-related variables <code>_maxprime</code> (traditionally accessed via <code>maxprime()</code>) and <code>diffptr</code>
* Others: <code>precdl</code>, <code>overflow</code>, <code>hiremainder</code>, ...

The PARI developer's guide lists two more:
* <code>PARI_SIGINT_block</code>: When this is nonzero, SIGINT is blocked.
* <code>PARI_SIGINT_pending</code>: When this is nonzero, a SIGINT has been blocked but not yet handled.


## Perl

A selection of the variables with special meaning to Perl. If you find it hard to remember the 'punctuation' names, longer 'English' names are available via a core module.

```txt
use English;                     # enables use of long variable names

$.     $INPUT_LINE_NUMBER        # sequence number
$,     $OUTPUT_FIELD_SEPARATOR   # output field separator
$;     $SUBSCRIPT_SEPARATOR      # subscript separator for multidimensional array emulation
$_     $ARG                      # topic/current/default variable
$"     $LIST_SEPARATOR           # alternative output field separator
$+     $LAST_PAREN_MATCH         # last substring matched to a regular expression subpattern
$0     $PROGRAM_NAME             # name of the program being executed
$!     $ERRNO                    # error number from host operating system
$@     $EVAL_ERROR               # error from the last "eval" operator
$/     $INPUT_RECORD_SEPARATOR   # input record separator
$\     $OUTPUT_RECORD_SEPARATOR  # output record separator for 'print'
$|     $OUTPUT_AUTOFLUSH         # controls output buffering
$&     $MATCH                    # string matched by last regular expression
$'     $POSTMATCH                # substring following last matched regular expression
$`     $PREMATCH                 # substring preceding last matched regular expression

@ARGV                            # array containing the command line parameters
@F                               # array of fields of each line read in when auto-split is on
@INC                             # array of library search paths

%ENV                             # associative container holding the environment variables
%SIG                             # associative container holding signal handlers
```



## Perl 6


It is probably useful to briefly explain normal variables in Perl 6 before tackling special variables.

Variables in Perl 6 have a prefix sigil to distinguish them from named subroutines, functions, classes, and so on. There is a system of sigils to mark the fundamental structural type of the variable:

```perl6
 $foo   scalar (object)
 @foo   ordered array
 %foo   unordered hash (associative array)
 &foo   code/rule/token/regex
 ::foo  package/module/class/role/subset/enum/type/grammar
```

Sigils indicate overall interface, not the exact type of the bound object. Different sigils imply different minimal abilities. Ordinary sigils indicate normally scoped variables, either lexical or package scoped. Oddly scoped variables include a secondary sigil (a twigil) that indicates what kind of strange scoping the variable is subject to:

```perl6
 $foo               # ordinary scoping
 $.foo              # object attribute public accessor
 $^foo              # self-declared formal positional parameter
 $:foo              # self-declared formal named parameter
 $*foo              # dynamically overridable global variable
 $?foo              # compiler hint variable
 $=foo              # Pod variable
 $<foo>             # match variable, short for $/{'foo'}
 $!foo              # object attribute private storage
 $~foo              # the foo sublanguage seen by the parser at this lexical spot
```


Special Variables:

Perl 6 has deprecated most of the "line-noise" variables from Perl 5 in favor of named variables.


```perl6
 $_                # The implicit variable lexically scoped to the current block
 @_                # Implicit array of parameters to the current block. Still available but rarely used or needed with the improved sub signatures
 $!                # Current Exception object
 $/                # Last match
 $0, $1, $2...     # Captured values from match: $/[0], $/[1], $/[2] ...
 $?ARCH            # Host architecture
 $?XARCH           # Target architecture
 @*ARGS            # command-line arguments
 $*ARGFILES        # The magic command-line input handle
 &?BLOCK           # current block (itself)
 ::?CLASS          # current class (as package or type name)
 $?CLASS           # current class
 @=COMMENT         # All the comment blocks in the file
 %?CONFIG          # configuration hash
 $*CWD             # current working directory
 $=DATA            # data block handle (=begin DATA ... =end)
 @=DATA            # Same as above, but array
 %?DEEPMAGIC       # Controls the mappings of magical names to sub definitions
 $?DISTRO          # Which OS distribution am I compiling under
 $*DISTRO          # Which OS distribution am I running under
 $*EGID            # effective group id
 %*ENV             # system environment variables
 $*ERR             # Standard error handle
 $*EUID            # effective user id
 $*EXECUTABLE_NAME # executable name
 $?FILE            # current filename of source file
 $?GRAMMAR         # current grammar
 $*GID             # group id
 $*IN              # Standard input handle; is an IO object
 @*INC             # where to search for user modules (but not std lib!)
 $?KERNEL          # operating system compiled for
 $*KERNEL          # operating system running under
 %?LANG            # What is the current set of interwoven languages?
 $*LANG            # LANG variable from %*ENV that defines what human language is used
 $?LINE            # current line number in source file
 %*META-ARGS       # Meta-arguments
 $?MODULE          # current module
 %*OPTS            # Options from command line
 %*OPT...          # Options from command line to be passed down
 $*OUT             # Standard output handle
 $?PACKAGE         # current package
 $?PERL            # Which Perl am I compiled for?
 $*PERL            # perl version running under
 $*PID             # system process id
 %=POD             # POD
 $*PROGRAM_NAME    # name of the Perl program being executed
 %*PROTOCOLS       # Stores the methods needed for the uri() function
 ::?ROLE           # current role (as package or type name)
 $?ROLE            # current role
 &?ROUTINE         # current sub or method (itself)
 $?SCOPE           # Current "my" scope
 $*TZ              # Local time zone
 $*UID             # system user id
 $?USAGE           # Default usage message generated at compile time
 $?VM              # Which virtual machine am I compiling under
 $?XVM             # Which virtual machine am I cross-compiling for
```


Also, not really a variable but...

```perl6
 *  # A standalone term that has no fixed value, instead it captures the notion of "Whatever",
    # the meaning of which is decided lazily by whatever it is an argument to.
    # See the "*" section of http://perlcabal.org/syn/S02.html#Built-In_Data_Types
```



## Phix

Phix has no special variables.


## PicoLisp

PicoLisp has no special variables, but some naming conventions concerning the "meaning" of a variable's (i.e. symbol's) value:

```txt
- Global variables start with an asterisk '*'
- Functions and other global symbols start with a lower case letter
- Locally bound symbols start with an upper case letter
- Local functions start with an underscore '_'
- Classes start with a plus-sign '+', where the first letter
   - is in lower case for abstract classes
   - and in upper case for normal classes
- Methods end with a right arrow '>'
- Class variables may be indicated by an upper case letter
```

For historical reasons, the global constant symbols 'T' and 'NIL' do not obey these rules, and are written in upper case.


## PL/I


```PL/I

Special variables in PL/I are termed "Pseudo-variables".
They are used only on the LHS of an assignment statement.

They include:
   REAL to assign the real part of a COMPLEX variable;
   IMAG to assign the imaginary part of a COMPLEX variable;
   SUBSTR is used to assign part of a string (used on the LHS of an assignment);
   STRING when used on the left-hand of an assignment;
   UNSPEC used to assign a bit pattern to a variable;
   ENTRYADDR is used to assign an address to an ENTRY variable that is
             to be invoked;
   ONCHAR resets the current value of the CHAR built-in function (when
          a data conversion error occurs, and it is desired to re-attempt
          the conversion);
   ONSOURCE assigns a new value to the ONSOURCE built-in function
            (may be used when a data conversion error occurs,
             and a re-try of the conversion is to be attempted with modified data);
   ONGSOURCE assigns a new value to the ONGSOURCE built-in function
            (may be used when a data conversion error occurs,
             and may be used when a re-try of the conversion is to be
             attempted with modified data).

```



## PowerShell

This is the list:

```PowerShell

<#
    $$
    $?
    $^
    $_
    $Args
    $ConsoleFileName
    $Error
    $Event
    $EventSubscriber
    $ExecutionContext
    $False
    $ForEach
    $Home
    $Host
    $Input
    $LastExitCode
    $Matches
    $MyInvocation
    $NestedPromptLevel
    $NULL
    $PID
    $Profile
    $PSBoundParameters
    $PsCmdlet
    $PsCulture
    $PSDebugContext
    $PsHome
    $PSitem
    $PSScriptRoot
    $PsUICulture
    $PsVersionTable
    $Pwd
    $Sender
    $ShellID
    $SourceArgs
    $SourceEventArgs
    $This
    $True
#>

```

For descriptions:

```PowerShell

help about_automatic_variables

```



## PureBasic

PureBasic has no 'special variables'.  It does define constants that reflect compiler settings that can be tested and used as a part of compiling.  All other non-explicitly declared values that vary during runtime are returned by functions.


## Python

By default, Python starts execution in a namespace which has direct access to names defined in the globals() dict and the __builtins__ dict. The members of which can be found by the following code:

```Python
names = sorted((set(globals().keys()) | set(__builtins__.__dict__.keys())) - set('_ names i'.split()))
print( '\n'.join(' '.join(names[i:i+8]) for i in range(0, len(names), 8)) )
```

;Output

```txt
ArithmeticError AssertionError AttributeError BaseException BufferError BytesWarning DeprecationWarning EOFError
Ellipsis EnvironmentError Exception False FloatingPointError FutureWarning GeneratorExit IOError
ImportError ImportWarning IndentationError IndexError KeyError KeyboardInterrupt LookupError MemoryError
NameError None NotImplemented NotImplementedError OSError OverflowError PendingDeprecationWarning ReferenceError
ResourceWarning RuntimeError RuntimeWarning StopIteration SyntaxError SyntaxWarning SystemError SystemExit
TabError True TypeError UnboundLocalError UnicodeDecodeError UnicodeEncodeError UnicodeError UnicodeTranslateError
UnicodeWarning UserWarning ValueError Warning WindowsError ZeroDivisionError __build_class__ __builtins__
__debug__ __doc__ __import__ __name__ __package__ abs all any
ascii bin bool bytearray bytes callable chr classmethod
compile complex copyright credits delattr dict dir divmod
enumerate eval exec exit filter float format frozenset
getattr globals hasattr hash help hex id input
int isinstance issubclass iter len license list locals
map max memoryview min next object oct open
ord pow print property quit range repr reversed
round set setattr slice sorted staticmethod str sum
super tuple type vars zip
```



## Racket


Racket does not have special variables in the usual sense.  In fact,
being a language that specializes in making up langugaes it has no
special anything hard-wired in.

But it does have "parameters" -- a kind of mutable values that can be
set for a specific dynamic runtime extent: either around some piece of
code, or globally, or in some thread.  These parameters are used to
configure many aspects of the runtime system, for
example "current-directory" has the obvious meaning.  (But again, the
names that are bound to these parameters are not special, and can be
changed or hidden.)


## REXX


### version 1

The REXX language has three special variables:

:::*   '''RC'''             [the '''r'''eturn '''c'''ode from commands issued to the host]
:::*   '''RESULT'''     [the result '''RETURN'''ed from a subroutine or function]
:::*   '''SIGL'''         [the source line number that did the transfer of control]

Each of the above may be used as a regular REXX variable;   they aren't reserved keywords or reserved variable names.

Because REXX may define (or re-define) any of these variables during execution of the REXX program, its recommended that they be not be used as regular REXX variables.

Initially, the above three special variables aren't defined   (until the appropriate action for their use has been performed).

: If no commands have been issued to the host, then the   '''RC'''   special variable isn't defined.
: If no subroutines have been invoked, then the   '''RESULT'''   special variable isn't defined.
: If no SIGNAL or CALL (or subroutine invocation) has been used, then the   '''SIGL'''   special variable isn't defined.
:::: (This excludes the use of:
:::::::*   '''SIGNAL ON   ααα'''
:::::::*   '''SIGNAL OFF ααα'''
:::: which don't actually transfer control.)

In each case, the three special variable names   ('''RC''',   '''RESULT''',   and   '''SIGL''')   may be in lower/upper/mixed case.


The scope of the special variables is   LOCAL.

```rexx
/*REXX program demonstrates REXX special variables:  RC,  RESULT,  SIGL */
                           /*line two.  */
                           /*line three.*/             say copies('═',79)
rc=1/3                     /*line four. */
signal youWho              /*line five. */
myLoo='this got skipped'   /*line six.  */
youwho:                    /*line seven.*/
sep=copies('─', 9)         /*line eight.*/
say sep  'SIGL=' sigl      /*line nine. */
say sep  'REXX source statement' SIGL '=' sourceline(sigl)
                                                       say copies('═',79)
g=44
call halve  g
say sep  'rc='     rc
say sep  'result=' result
                                                       say copies('═',79)
h=66
hh=halve(h)
say sep  'rc='     rc
say sep  'result=' result
say sep  'hh='     hh
                                                       say copies('═',79)
'DIR  /ad  /b'                         /*display the directories (Bare).*/
say sep  'rc='     rc
say sep  'result=' result
                                                       say copies('═',79)
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────HALVE subroutine────────────────────*/
halve: return arg(1) / 2               /*a simple halving function.     */
```

'''output''' using:
:::* Regina REXX

```txt

═══════════════════════════════════════════════════════════════════════════════
───────── SIGL= 5
───────── REXX source statement 5 = signal youWho              /*line five. */
═══════════════════════════════════════════════════════════════════════════════
───────── rc= 0.333333333
───────── result= 22
═══════════════════════════════════════════════════════════════════════════════
───────── rc= 0.333333333
───────── result= 22
───────── hh= 33
═══════════════════════════════════════════════════════════════════════════════
Documents and Settings
Program Files
Recycled
System Volume Information
TEMP
WINDOWS
───────── rc= 0
───────── result= 22
═══════════════════════════════════════════════════════════════════════════════

```


'''output''' using:
:::* PC/REXX
:::* Personal REXX
:::* R4 REXX
:::* ROO

```txt

═══════════════════════════════════════════════════════════════════════════════
───────── SIGL= 5
───────── REXX source statement 5 = signal youWho              /*line five. */
═══════════════════════════════════════════════════════════════════════════════
───────── rc= 0.333333333
───────── result= 22
═══════════════════════════════════════════════════════════════════════════════
───────── rc= 0.333333333
───────── result= 33
───────── hh= 33
═══════════════════════════════════════════════════════════════════════════════
Documents and Settings
Program Files
Recycled
System Volume Information
TEMP
WINDOWS
───────── rc= 0
───────── result= 33
═══════════════════════════════════════════════════════════════════════════════

```



### version 2

The REXX language has three special variables:

RC is set upon return from a command

RESULT is set or dropped after a CALL to a subroutine:
It is assigned the value given in the RETURN statement
or it is dropped when there was a RETURN without expression. (some REXX implementations set RESULT also upon return from a function invocation - see version 1 and the Discussion)

SIGL is set when control is tranferred within the REXX program.
(i.e. when a subroutine is called, a function is invoked, or a SIGNAL
instruction was used explicitly or as a result of a raised condition)
Note that RETURN transfers control but does not set SIGL.


```rexx
'dir a2.txt'
Say 'rc='rc
'dir 33.*'
Say 'rc='rc

Call square 5
Say 'RESULT='result
Say 'SIGL='sigl

x2=square(3) /* just a simle example */
Say '3**2='||x2

Signal On Novalue
x=y   /* y was not yet assigned a value */

Exit
square: Procedure Expose sigl
Say 'square was invoked from line' sigl':' sourceline(sigl)
Return arg(1)**2

Novalue:
Say 'NOVALUE encountered in line' sigl':' sourceline(sigl)
Exit
```

```txt
 Datentr„ger in Laufwerk E: ist E_DESKTOP
 Volumeseriennummer: 66C1-0469

 Verzeichnis von E:\

28.07.2014  11:53            19.219 A2.txt
               1 Datei(en),         19.219 Bytes
               0 Verzeichnis(se), 198.658.433.024 Bytes frei
rc=0
 Datentr„ger in Laufwerk E: ist E_DESKTOP
 Volumeseriennummer: 66C1-0469

 Verzeichnis von E:\

rc=1
square was invoked from line 6: Call square 5
RESULT=25
SIGL=6
square was invoked from line 10: x2=square(3) /* just a simle example */
3**2=9
NOVALUE encountered in line 14: x=y   /* y was not yet assigned a value */
```



## Ruby

[http://ruby.wikia.com/wiki/Special_variable A list and description of Ruby's Special Variables]


## Scala

Scala has only a few special variables. Inside an object there is this, a reference that points to the object itself (like 127.0.0.1 in networking) and are used to qualify member access. There is also super that does the same for the base class (actually "the next class in the inheritance tree").

Depending on the context, underscore has the following meaning:

```txt
import scala._    // Wild card -- all of scala is imported
import scala.{ Predef => _, _ } // Exception, everything except Predef
def f[M[_]]       // Higher kinded type parameter
def f(m: M[_])    // Existential type
_ + _             // Anonymous function placeholder parameter
m _               // Eta expansion of method into method value
m(_)              // Partial function application
_ => 5            // Discarded parameter
case _ =>         // Wild card pattern -- matches anything
val (a, _) = (1, 2) // same thing
for (_ <- 1 to 10)  // same thing
f(xs: _*)         // Sequence xs is passed as multiple parameters to f(ys: T*)
case Seq(xs @ _*) // Identifier xs is bound to the whole matched sequence
var i: Int = _    // Initialization to the default value
def abc_<>!       // An underscore must separate alphanumerics from symbols on identifiers
t._2              // Part of a method name, such as tuple getters
var i: Int = _    // Initialization with a default value
for (_ <- 1 to 10) doIt() // Discarded val
def f: T; def f_=(t: T) // Combo for creating mutable f member.
```


## Smalltalk

Talking about special (reserved) names, there are:
* self - the current method's receiver
* super - ditto, but different message send lookup
* thisContext - the current stackframe/aka active continuation

Everything else is found via the lexical scope ending in bindings in a namespace.
The global default namespace is called "Smalltalk" and contains (beside bindings for all classes by name) the singletons:
* true
* false
* nil
Technically, these could be redefined, but the system would not work if any of them was. Therefore most compilers refuse code which obviously attempts to do so, and we can think of them as being reserved names as well.
Global binding names are returned by
```smalltalk
Smalltalk keys>
```

Things like the shell environment, command line argument, version numbers etc. are usually not exposed via globals, but instead held in private class variables (static variables), which can be accessed via getter messages (which, by the way, makes it easier to insert a dialect compatibility layer). An example for this would be:
```smalltalk
Float precision>
```
 or <lang smalltalk>Smalltalk version
```


Name conventions:
* class names - upperCase first
* class variables (statics) - ditto
* shared pool variables - ditto
* all other variables (incl. instance variables) - lowercase first
* method names - lowercase first; except for constant/parameter getters, which are sometimes uc-first


## Tcl

There are three major categories of special variables in Tcl: global variables special to the Tcl language, global variables set by Tcl-based interpreters, and local variables with special interpretations.

### Language Globals

These variables are defined by the standard implementation of Tcl, and are present in all Tcl interpreters by default.
;env
:This global array is Tcl's interface to the process's environment variables.
;errorCode
:This global scalar holds a machine-readable description of the last error to occur. (Note that prior to Tcl 8.6, internally-generated exceptions often used <tt>NONE</tt> for this value.)
;errorInfo
:This global scalar holds a stack trace from the last error to occur.
;tcl_library
:This global scalar holds the location of Tcl's own internal library.
;tcl_version, tcl_patchLevel
:This global scalar holds the version of Tcl in use. From Tcl 8.5 onwards, these hold the same (detailed) value.
;tcl_pkgPath
:This global scalar holds a Tcl list of directories where Tcl looks for packages by default. This is used to initialize the '''auto_path''' global variable.
;auto_path
:This global scalar holds a Tcl list of directories where Tcl looks for packages (and auto-loaded scripts, though this facility is deprecated).
;tcl_platform
:This global array holds a description of the platform on which Tcl is executing.
;tcl_precision
:This global scalar holds the number of significant figures to use when converting a floating-point value to a string by default. From Tcl 8.5 onwards it should not be changed. (If you are thinking of using this, consider using the <code>format</code> command instead.)
;tcl_rcFileName
:This global scalar holds the name of a file to <code>source</code> when the interpreter starts in interactive mode.
;tcl_rcRsrcName
:This global scalar is only used on classic Mac OS (now deprecated); consult the documentation for more information.
;tcl_traceCompile
:If enabled at library configuration time, this global scalar allows tracing of the compilation of bytecode in the interpreter.
;tcl_traceExec
:If enabled at library configuration time, this global scalar allows tracing of the execution of bytecode in the interpreter.
;tcl_wordchars, tcl_nonwordchars
:These global scalars hold regular expression fragments that describe the current platform's interpretation of what is and isn't a word.

### Interpreter Globals

These global variables are only features of the most common Tcl-based shells, [[tclsh]] and [[wish]].
;argc
:This global scalar holds the number of arguments (after the script) passed to the Tcl interpreter.
;argv
:This global scalar holds a Tcl list of the arguments (after the script) passed to the Tcl interpreter.
;argv0
:This global scalar holds the name of the main script to execute that was passed to the Tcl interpreter, or the name of the interpreter itself in interactive mode.
;tcl_interactive
:This global scalar holds whether this interpreter is working in interactive mode (i.e., needs to print command prompts, run a REPL, etc.)
;tcl_prompt1, tcl_prompt2
:These global scalars allow customization of the prompt strings in interactive mode.
;geometry
This global scalar holds the user-supplied preferred dimensions of the initial window. Only used by interpreters that load the [[Tk]] library.

### Local Special Variables

This is a language feature of procedures.
;args
:This local variable holds the Tcl list of arguments supplied to the current procedure after all the other formal arguments have been satisfied. Note that it needs to be explicitly listed in the formal arguments ''and'' be last in the list of formal arguments to have this behavior.

== {{header|UNIX Shell}}==

The following variables are reserved for special purposes within the Bourne shell:

* $0 	the invoked command positional variable
* $1 	the first positional variable
* $2 	the second positional variable
* $3 	the third positional variable
* $4 	the fourth positional variable
* $5 	the fifth positional variable
* $6 	the sixth positional variable
* $7 	the seventh positional variable
* $8 	the eighth positional variable
* $9 	the nineth positional variable
* $* 	the dollarstar expands to all command line arguments
* $@ 	the dollarsnail expands to all command line arguments
* $# 	the dollarhash expands to the number of command line arguments given
* $? 	the dollarhook expands to the exit status of the last command executed
* $- 	a list of all options used to invoke the shell
* $$ 	the pid of the currentprocess
* $! 	the pid of the last command executed as a background process

* CDPATH 	Additional locations to be searched by the cd command
* HOME 	The default working directory of the current user
* HUSHLOGIN
* IFS 	Internal field separator. This contains space,tab and newline characters
* LANG 	Determines the default locale in the absence of other locale related environment variables
* LC_ALL 	High precedence override for locale specific behaviour
* LC_CTYPE 	Determines locale specific character classification
* LOGNAME 	The login name of the user
* LS_COLORS
* MAIL 	The full pathname of the users mail file
* MAILCHECK 	The time limit that the shell timer uses before checking for new mail
* MAILPATH
* OPTARG
* OPTIND
* PATH 	The shell search path
* PPID
* PS1 	Primary system prompt
* PS2 	Secondary system prompt
* PS3 	Ternary system prompt
* PS4 	Forth system prompt
* PWD 	Current working directory
* SHACCT
* SHELL 	The name of the current shell
* SHLVL
* TERM 	The current terminal type
* TIMEOUT
* TZ 	The current timezone
* USER 	The current username
* underscore


## Ursa


```ursa
# contains arguments passed to the ursa
# interpreter on the command line
string<> args

# iodevice that points to the console by default
iodevice console

# contains "\n"
string endl

# represents false
boolean false

# represents true
boolean true
```



## VBA

VBA does not have special variables.

## XLISP

XLISP provides the following built-in variables:

```txt
*PACKAGE*
*READTABLE*
*ERROR-HANDLER*
*UNBOUND-HANDLER*
*LOAD-PATH*
*STANDARD-INPUT*
*STANDARD-OUTPUT*
*ERROR-OUTPUT*
*FIXNUM-FORMAT*
*HEXNUM-FORMAT*
*FLONUM-FORMAT*
*PRINT-CASE*
*SOFTWARE-TYPE*
T                ; bound to #T
NIL              ; bound to '()
OBJECT
CLASS
```


== {{header|zkl}} ==

```zkl
__DATE__, __DEBUG__, __FILE__, __LINE__, __NAME__, __TIME__
```

As in the C preprocessor. Some (like __DEBUG__) can be changed, others (like __LINE__, __TIME__) are constants.

== {{header|ZX Spectrum Basic}} ==

The ZX Spectrum does Not make a difference between capital or lower character variable names. A normal variable can be severall letters long, any  DEF FN, FOR/NEXT, DIM (STRING or DATA) array name is a single letter variable name. None of these are special variables. for 128k zx spectrum matters that In REVERSED SENSE some variable names are IMposible becouse of the tokenized form in the 48k basic eg 'not' will be NOT !!!!
There are a set of system variables held at a fixed memory addresses, which can be accessed via PEEK and POKE functions. The system variables and addresses are:

* 23552 KSTATE - Keyboard state.
* 23560 LAST K - Newly pressed key.
* 23561 REPDEL - Delay (in 50ths of a second) before keyboard starts repeating.
* 23562 REPPER - Delay (in 50ths of a second) between keyboard repetions.
* 23563 DEFADD - Address of arguments for a user defined function.
* 23565 K DATA - Second byte of colour controls sequences from keyboard.
* 23566 TVDATA - Colour and location information for television set.
* 23568 STRMS - Addresses of channels attached to streams.
* 23606 CHARS - 256 less than the address of the current character set
* 23608 RASP - Length of the warning buzz.
* 23609 PIP - Length of the keyboard click.
* 23610 ERR NR One less than the error code and starts at 255 (for -1)
* 23611 FLAGS - Various flags to control the BASIC system.
* 23612 TV FLAG - Flags associated with the television output.
* 23613 ERR SP - Address of item on machine stack to be used as error return.
* 23615 LIST SP - Return address from automatic listing.
* 23617 MODE - Cursor type
* 23618 NEWPPC - Line number to be jumped to.
* 23620 NSPPC - Statement number in line to be jumped to
* 23621 PPC - Line number of statement currently being executed.
* 23623 SUBPPC - Statement number within the line that is currently being executed.
* 23624 BORDCR - Colour of the border (overscan area) and input area (times 8)
* 23625 E PPC - Current line number (for list editing)
* 23627 VARS - Address of the variables.
* 23629 DEST - Address of variable in assignment.
* 23631 CHANS - Address of channel data.
* 23633 CURCHL - Address of information being used for input and output.
* 23635 PROG - Address of the BASIC program.
* 23637 NXTLIN - Address of the next line in the program.
* 23639 DATADD - Address of terminator of the last DATA item.
* 23641 E LINE - Address of command being typed in.
* 23643 K CUR - Address of the cursor.
* 23645 CH ADD - Address of the next character to be interpreted
* 23647 X PTR - Address of the character after the syntax error marker.
* 23649 WORKSP - Address of temporary work space.
* 23651 STKBOT - Address of bottom of calculator stack.
* 23653 STKEND - Address of start of free space.
* 23655 BREG - The B register of the calculator.
* 23656 MEM - Address of area used for calculator memory.
* 23658 FLAGS2 - Miscellaneous flags.
* 23659 DF SZ - The number of lines in the input area.
* 23660 S TOP - The number of the top program line in automatic listings.
* 23662 OLDPPC - Line number to which CONTINUE jumps.
* 23664 OSPPC - Number within line of statement to which CONTINUE jumps.
* 23665 FLAGX - Various flags.
* 23666 STRLEN - Length of string type destination in assignment.
* 23668 T ADDR  - Address of next item in the syntax table.
* 23670 SEED - The random number seed.
* 23672 FRAMES - A 3 byte frame counter incremented every 20ms.
* 23675 UDG - Address of the user defined graphics
* 23677 COORDS Coordinates of the last point plotted.
* 23679 P POSN - Column number for the position of the printer carriage.
* 23680 PR CC - Address of next free position in the printer buffer.
* 23682 ECHO E - Column and line number positions in the input buffer.
* 23684 DF CC - Display address for the current print position.
* 23686 DFCCL - Display address in the input area.
* 23688 S POSN - Column and row number of the print position.
* 23690 SPOSNL - Column and row number for the print position in the input area.
* 23692 SCR CT - Scroll counter - Number of lines left before stopping with Scroll?
* 23693 ATTR P - Permanent current colours attributes
* 23694 MASK P - Permanent current colour attribute mask
* 23695 ATTR T - Temporary current colour attributes
* 23696 MASK T - Temporary current colour attribute mask
* 23697 P FLAG - More flags.
* 23698 MEMBOT - Memory area address used by the calculator
* 23728 NMIADD - The address of the users Non Maskable Interrupt service routine.
* 23730 RAMTOP - Address of last byte of BASIC system area.
* 23732 P-RAMT - Address of last byte of physical RAM.


```zxbasic
10 PRINT "The border colour is "; PEEK (23624): REM bordcr
20 PRINT "The ramtop address is "; PEEK (23730) + 256 * PEEK (23731): REM ramtop
30 POKE 23609,50: REM set keyboard pip to 50
```


