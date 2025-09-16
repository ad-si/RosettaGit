+++
title = "Chemical Calculator"
description = ""
date = 2019-10-18T00:05:24Z
aliases = []
[extra]
id = 22230
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_w",
  "coffeescript",
  "csharp",
  "d",
  "factor",
  "go",
  "julia",
  "kotlin",
  "nim",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "visual_basic_.net",
  "zkl",
]
+++

Given a molecule's chemical formula, calculate the   [https://en.wikipedia.org/wiki/Molar_mass <u>molar mass</u>].


;Introduction
* A molecule consists of atoms. E.g. water, H2O, has two hydrogen atoms and one oxygen atom
* The mass of H2O is 1.008 * 2 + 15.999 = 18.015
* An atom name consists of one upper-case letter followed by zero, one or two lower-case letters.
** H           (Hydrogen)
** He               (Helium)
** Uue                   (Ununennium)
* The number of atoms is stated behind the atom or atom group
* An atom group is specified using parenthesis. E.g. Butyric acid, (CH3)2CHCOOH, has two CH3 groups
* A group may contain other groups, e.g. COOH(C(CH3)2)3CH3


;Background
* The mass is dimensionless,   it is relative to   <sup>1</sup>/<sub>12</sub>   of Carbon-12
* Carbon-12   has exactly   6 protons,   6 electrons,   and   6 neutrons
* One mole of   H<sub>2</sub>O   has the mass 18.015 grams
* One mole is defined as exactly   6.02214076×10<sup>23</sup>   particles


The above number is known as the   [https://en.wikipedia.org/wiki/Avogadro_constant Avogadro constant],   which is
named by the International Bureau of Weights and Measures (IBPM);

the initials are taken from   ''Bureau International des Poids et Mesures'',   the official name of the bureau.

An older name for   ''Avogadro constant''   is   ''Avogadro number''.


;Atom masses
A mapping between most recognized element names and atomic mass follows in comma separated value format:

```txt
H,1.008
He,4.002602
Li,6.94
Be,9.0121831
B,10.81
C,12.011
N,14.007
O,15.999
F,18.998403163
Ne,20.1797
Na,22.98976928
Mg,24.305
Al,26.9815385
Si,28.085
P,30.973761998
S,32.06
Cl,35.45
K,39.0983
Ar,39.948
Ca,40.078
Sc,44.955908
Ti,47.867
V,50.9415
Cr,51.9961
Mn,54.938044
Fe,55.845
Ni,58.6934
Co,58.933194
Cu,63.546
Zn,65.38
Ga,69.723
Ge,72.63
As,74.921595
Se,78.971
Br,79.904
Kr,83.798
Rb,85.4678
Sr,87.62
Y,88.90584
Zr,91.224
Nb,92.90637
Mo,95.95
Ru,101.07
Rh,102.9055
Pd,106.42
Ag,107.8682
Cd,112.414
In,114.818
Sn,118.71
Sb,121.76
I,126.90447
Te,127.6
Xe,131.293
Cs,132.90545196
Ba,137.327
La,138.90547
Ce,140.116
Pr,140.90766
Nd,144.242
Pm,145
Sm,150.36
Eu,151.964
Gd,157.25
Tb,158.92535
Dy,162.5
Ho,164.93033
Er,167.259
Tm,168.93422
Yb,173.054
Lu,174.9668
Hf,178.49
Ta,180.94788
W,183.84
Re,186.207
Os,190.23
Ir,192.217
Pt,195.084
Au,196.966569
Hg,200.592
Tl,204.38
Pb,207.2
Bi,208.9804
Po,209
At,210
Rn,222
Fr,223
Ra,226
Ac,227
Pa,231.03588
Th,232.0377
Np,237
U,238.02891
Am,243
Pu,244
Cm,247
Bk,247
Cf,251
Es,252
Fm,257
Ubn,299
Uue,315
```



;Examples:

```python
assert   1.008 == molar_mass('H')                  # hydrogen
assert   2.016 == molar_mass('H2')                 # hydrogen gas
assert  18.015 == molar_mass('H2O')                # water
assert  34.014 == molar_mass('H2O2')               # hydrogen peroxide
assert  34.014 == molar_mass('(HO)2')              # hydrogen peroxide
assert 142.036 == molar_mass('Na2SO4')             # sodium sulfate
assert  84.162 == molar_mass('C6H12')              # cyclohexane
assert 186.295 == molar_mass('COOH(C(CH3)2)3CH3')  # butyric or butanoic acid
assert 176.124 == molar_mass('C6H4O2(OH)4')        # vitamin C
assert 386.664 == molar_mass('C27H46O')            # cholesterol
assert 315     == molar_mass('Uue')                # ununennium
```



;Reference:
:*   Wikipedia article:   [https://en.wikipedia.org/wiki/Molecular_mass Molecular mass]





## ALGOL W

Algol W has fixed length strings and no regular expressions, this parses the molecule with a simple recursive descent parser.

Some error checking is included.

```algolw
begin
    % calculates the molar mass of the specified molecule %
    real procedure molar_mass ( string(256) value molecule ) ; begin
        string(1) c;
        integer   chPos, chMax;
        logical   hadError;
        real      mass;
        % reports a syntax error in the molecule starting at position chPos %
        procedure syntaxError( string(80) value message ) ; begin
            integer mPos;
            write( "Syntax error in molecule: " );
            mPos := 0;
            while mPos < 80 and message( mPos // 1 ) not = "." do begin
                writeon( message( mPos // 1 ) );
                mPos := mPos + 1
            end while_not_end_of_message ;
            write( "    " );for i := 0 until chMax     do writeon( molecule( i // 1 ) );
            write( "    " );for i := 0 until chPos - 1 do writeon( " " );
            writeon( "^" );
            % ensure parsing stops %
            hadError := true;
            chPos    := chMax * 2
        end syntaxError ;
        % gets the next character from the molecule %
        procedure nextChar ; begin
            chPos := chPos + 1;
            c     := if chPos > chMax then " " else molecule( chPos // 1 )
        end nextChar ;
        % parses a compound: a sequence of element names and bracketed compounds, each with   %
        % an optional trailing repeat count                                                   %
        real procedure parseCompound ; begin
            real mass, itemMass;
            % parses an element symbol from the molecule and returns its mass                 %
            real procedure parseAtom ; begin
                string(3)       symbol;
                reference(Atom) element;
                symbol := c;
                nextChar;
                if c >= "a" and c <= "z" then begin
                    symbol( 1 // 1 ) := c;
                    nextChar;
                    if c >= "a" and c <= "z" then begin
                        symbol( 2 // 1 ) := c;
                        nextChar
                    end if_have_lc_letter
                end if_have_lc_letter ;
                % find the element in the table %
                element := atoms( decode( symbol( 0 // 1 ) ) - decode( "A" ) );
                while element not = null and aSymbol(element) not = symbol do element := aNext(element);
                if element not = null then % found the element % aMass(element)
                else begin % unknown element %
                    chPos := chPos - 1;
                    syntaxError( "Unrecognised element." );
                    0
                end
            end parseAtom ;
            mass := 0;
            while not hadError and ( ( c >= "A" and c <= "Z" ) or c = "(" ) do begin
                if c >= "A" and c <= "Z" then itemMass := parseAtom
                else if c = "(" then begin % bracketed group %
                    nextChar;
                    itemMass := parseCompound;
                    if chPos > chMax or molecule( chPos // 1 ) not = ")" then syntaxError( "Expected "")""." );
                    nextChar
                end ;
                if c >= "0" and c <= "9" then begin % have a repeat count %
                    integer count;
                    count := 0;
                    while not hadError and c >= "0" and c <= "9" do begin
                        count := ( count * 10 ) + ( decode( c ) - decode( "0" ) );
                        nextChar
                    end while_not_end_of_number ;
                    itemMass := itemMass * count
                end if_have_a_digit ;
                mass := mass + itemMass
            end while_still_parseing ;
            mass
        end parseCompound ;
        hadError := false;
        % find the end of the molecule %
        chMax := 255;
        while chMax > 0 and molecule( chMax // 1 ) = " " do chMax := chMax - 1;
        % parse the molecule %
        chPos := -1;
        nextChar;
        mass := parseCompound;
        if chPos <= chMax then syntaxError( "Unexpected text after the molecule." );
        mass
    end molar_mass ;
    % record to hold element symbols and masses %
    record Atom( string(3) aSymbol; real aMass; reference(Atom) aNext );
    % hash table of atome, hash is the first character of the symbol - "A" %
    reference(Atom) array atoms ( 0 :: 25 ); for i := 0 until 25 do atoms( i ) := null;
    begin % setup element symbols and masses as specified in the task %
        % adds an element and its mass to the atoms table %
        procedure A ( string(3) value symbol; real value mass ) ; begin
            integer index;
            index := decode( symbol( 0 // 1 ) ) - decode( "A" );
            atoms( index ) := Atom( symbol, mass, atoms( index ) )
        end A ;
        procedure Lanthanides ; begin
            A("La",138.90547);A("Ce",140.116 );A("Pr",140.90766);A("Nd",144.242  );A("Pm",145     );
            A("Sm",150.36   );A("Eu",151.964 );A("Gd",157.25   );A("Tb",158.92535);A("Dy",162.5   );
            A("Ho",164.93033);A("Er",167.259 );A("Tm",168.93422);A("Yb",173.054  );A("Lu",174.9668);
        end Lanthanides ;
        procedure Actinides ; begin
            A("Ac",227      );A("Th",232.0377);A("Pa",231.03588);A("U", 238.02891);A("Np",237     );
            A("Pu",244      );A("Am",243     );A("Cm",247      );A("Bk",247      );A("Cf",251     );
            A("Es",252      );A("Fm",257     ); % Md           % %, No           %  % Lr          %
        end Actinides ;
        A("Li", 6.94       );A("Na",22.98976928 );A("K", 39.0983  );A("Rb", 85.4678 );A("Cs",132.90545196);A("Fr", 223 );
        A("Be", 9.0121831  );A("Mg",24.305      );A("Ca",40.078   );A("Sr", 87.62   );A("Ba",137.327     );A("Ra", 226 );
                                                  A("Sc",44.955908);A("Y",  88.90584);   Lanthanides;         Actinides;
                                                  A("Ti",47.867   );A("Zr", 91.224  );A("Hf",178.49      ); % Rf       %
                                                  A("V", 50.9415  );A("Nb", 92.90637);A("Ta",180.94788   ); % Db       %
                                                  A("Cr",51.9961  );A("Mo", 95.95   );A("W", 183.84      ); % Sg       %
                                                  A("Mn",54.938044); % Tc           % A("Re",186.207     ); % Bh       %
                                                  A("Fe",55.845   );A("Ru",101.07   );A("Os",190.23      ); % Hs       %
                                                  A("Co",58.933194);A("Rh",102.9055 );A("Ir",192.217     ); % Mt       %
                                                  A("Ni",58.6934  );A("Pd",106.42   );A("Pt",195.084     ); % Ds       %
                                                  A("Cu",63.546   );A("Ag",107.8682 );A("Au",196.966569  ); % Rg       %
                                                  A("Zn",65.38    );A("Cd",112.414  );A("Hg",200.592     ); % Cn       %
        A("B", 10.81       );A("Al",26.9815385  );A("Ga",69.723   );A("In",114.818  );A("Tl",204.38      ); % Nh       %
        A("C", 12.011      );A("Si",28.085      );A("Ge",72.63    );A("Sn",118.71   );A("Pb",207.2       ); % Fl       %
        A("N", 14.007      );A("P", 30.973761998);A("As",74.921595);A("Sb",121.76   );A("Bi",208.9804    ); % Ms       %
        A("O", 15.999      );A("S", 32.06       );A("Se",78.971   );A("Te",127.6    );A("Po",209         ); % Lv       %
        A("F", 18.998403163);A("Cl",35.45       );A("Br",79.904   );A("I", 126.90447);A("At",210         ); % Ts       %
        A("Ne",20.1797     );A("Ar",39.948      );A("Kr",83.798   );A("Xe",131.293  );A("Rn",222         ); % Og       %
        % ---------------- first period elements ---> % A("H",    1.008);A("He",   4.002602);
        % --- hypothetical eigth period elements ---> % A("Uue",315    );A("Ubn",299       );
    end;
    begin % test cases %
        procedure test( real value expectedMass; string(256) value molecule ) ; begin
            real mass, diff;
            mass := molar_mass( molecule );
            write( r_format := "A", r_d := 3, r_w := 9
                 , molecule( 0 // 20 ), ":", mass
                 );
            diff := expectedMass - mass;
            if diff > 1'-12 or diff < -1'-12 then writeon( r_format := "A", r_d := 2, r_w := 9
                                                         , " expected:", expectedMass
                                                         )
        end text ;
        test( 1.008, "H" ); test( 2.016, "H2" );         test(  18.015, "H2O"   );
        test( 142.03553856000002, "Na2SO4"            ); test(  84.162, "C6H12" );
        test( 186.29499999999996, "COOH(C(CH3)2)3CH3" ); test( 350.45,  "UueCl" );
    end
end.
```

```txt

H                   :    1.008
H2                  :    2.016
H2O                 :   18.015
Na2SO4              :  142.035
C6H12               :   84.162
COOH(C(CH3)2)3CH3   :  186.295
UueCl               :  350.450

```



## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ChemicalCalculator {
    class Program {
        static Dictionary<string, double> atomicMass = new Dictionary<string, double>() {
            {"H",     1.008 },
            {"He",    4.002602},
            {"Li",    6.94},
            {"Be",    9.0121831},
            {"B",    10.81},
            {"C",    12.011},
            {"N",    14.007},
            {"O",    15.999},
            {"F",    18.998403163},
            {"Ne",   20.1797},
            {"Na",   22.98976928},
            {"Mg",   24.305},
            {"Al",   26.9815385},
            {"Si",   28.085},
            {"P",    30.973761998},
            {"S",    32.06},
            {"Cl",   35.45},
            {"Ar",   39.948},
            {"K",    39.0983},
            {"Ca",   40.078},
            {"Sc",   44.955908},
            {"Ti",   47.867},
            {"V",    50.9415},
            {"Cr",   51.9961},
            {"Mn",   54.938044},
            {"Fe",   55.845},
            {"Co",   58.933194},
            {"Ni",   58.6934},
            {"Cu",   63.546},
            {"Zn",   65.38},
            {"Ga",   69.723},
            {"Ge",   72.630},
            {"As",   74.921595},
            {"Se",   78.971},
            {"Br",   79.904},
            {"Kr",   83.798},
            {"Rb",   85.4678},
            {"Sr",   87.62},
            {"Y",    88.90584},
            {"Zr",   91.224},
            {"Nb",   92.90637},
            {"Mo",   95.95},
            {"Ru",  101.07},
            {"Rh",  102.90550},
            {"Pd",  106.42},
            {"Ag",  107.8682},
            {"Cd",  112.414},
            {"In",  114.818},
            {"Sn",  118.710},
            {"Sb",  121.760},
            {"Te",  127.60},
            {"I",   126.90447},
            {"Xe",  131.293},
            {"Cs",  132.90545196},
            {"Ba",  137.327},
            {"La",  138.90547},
            {"Ce",  140.116},
            {"Pr",  140.90766},
            {"Nd",  144.242},
            {"Pm",  145},
            {"Sm",  150.36},
            {"Eu",  151.964},
            {"Gd",  157.25},
            {"Tb",  158.92535},
            {"Dy",  162.500},
            {"Ho",  164.93033},
            {"Er",  167.259},
            {"Tm",  168.93422},
            {"Yb",  173.054},
            {"Lu",  174.9668},
            {"Hf",  178.49},
            {"Ta",  180.94788},
            {"W",   183.84},
            {"Re",  186.207},
            {"Os",  190.23},
            {"Ir",  192.217},
            {"Pt",  195.084},
            {"Au",  196.966569},
            {"Hg",  200.592},
            {"Tl",  204.38},
            {"Pb",  207.2},
            {"Bi",  208.98040},
            {"Po",  209},
            {"At",  210},
            {"Rn",  222},
            {"Fr",  223},
            {"Ra",  226},
            {"Ac",  227},
            {"Th",  232.0377},
            {"Pa",  231.03588},
            {"U",   238.02891},
            {"Np",  237},
            {"Pu",  244},
            {"Am",  243},
            {"Cm",  247},
            {"Bk",  247},
            {"Cf",  251},
            {"Es",  252},
            {"Fm",  257},
            {"Uue", 315},
            {"Ubn", 299},
        };

        static double Evaluate(string s) {
            s += "[";
            double sum = 0.0;
            string symbol = "";
            string number = "";
            for (int i = 0; i < s.Length; ++i) {
                var c = s[i];
                if ('@' <= c && c <= '[') {
                    // @, A-Z
                    int n = 1;
                    if (number != "") {
                        n = int.Parse(number);
                    }
                    if (symbol != "") {
                        sum += atomicMass[symbol] * n;
                    }
                    if (c == '[') {
                        break;
                    }
                    symbol = c.ToString();
                    number = "";
                } else if ('a' <= c && c <= 'z') {
                    symbol += c;
                } else if ('0' <= c && c <= '9') {
                    number += c;
                } else {
                    throw new Exception(string.Format("Unexpected symbol {0} in molecule", c));
                }
            }
            return sum;
        }

        // Taken from return text.Substring(0, pos) + replace + text.Substring(pos + search.Length);
        static string ReplaceFirst(string text, string search, string replace) {
            int pos = text.IndexOf(search);
            if (pos < 0) {
                return text;
            }
            return text.Substring(0, pos) + replace + text.Substring(pos + search.Length);
        }

        static string ReplaceParens(string s) {
            char letter = 's';
            while (true) {
                var start = s.IndexOf('(');
                if (start == -1) {
                    break;
                }

                for (int i = start + 1; i < s.Length; ++i) {
                    if (s[i] == ')') {
                        var expr = s.Substring(start + 1, i - start - 1);
                        var symbol = string.Format("@{0}", letter);
                        s = ReplaceFirst(s, s.Substring(start, i + 1 - start), symbol);
                        atomicMass[symbol] = Evaluate(expr);
                        letter++;
                        break;
                    }
                    if (s[i] == '(') {
                        start = i;
                        continue;
                    }
                }
            }
            return s;
        }

        static void Main() {
            var molecules = new string[]{
                "H", "H2", "H2O", "H2O2", "(HO)2", "Na2SO4", "C6H12",
                "COOH(C(CH3)2)3CH3", "C6H4O2(OH)4", "C27H46O", "Uue"
            };
            foreach (var molecule in molecules) {
                var mass = Evaluate(ReplaceParens(molecule));
                Console.WriteLine("{0,17} -> {1,7:0.000}", molecule, mass);
            }
        }
    }
}
```

```txt
                H ->   1.008
               H2 ->   2.016
              H2O ->  18.015
             H2O2 ->  34.014
            (HO)2 ->  34.014
           Na2SO4 -> 142.036
            C6H12 ->  84.162
COOH(C(CH3)2)3CH3 -> 186.295
      C6H4O2(OH)4 -> 176.124
          C27H46O -> 386.664
              Uue -> 315.000
```



## CoffeeScript


### No Regular Expression


```coffeescript
ATOMIC_MASS = {H:1.008,C:12.011,O:15.999,Na:22.98976928,S:32.06,Uue:315}

molar_mass = (s) ->
	result = ''
	i = 0
	member = (a,c) ->  a <= s[i] <= c
	next = ->
		i += 1
		s[i-1]
	while i < s.length
		if s[i] == '(' then result += '+' + next()
		else if s[i] == ')' then result += next()
		else if member '0','9'
			result += '*'
			result += next() while member '0','9'
		else if member 'A','Z'
			name = next()
			name += next() while member 'a','z'
			result += '+' + ATOMIC_MASS[name]
	parseFloat eval(result).toFixed 3

assert 1.008, molar_mass 'H'
assert 2.016, molar_mass 'H2'
assert 18.015, molar_mass 'H2O'
assert 34.014, molar_mass 'H2O2'
assert 34.014, molar_mass '(HO)2'
assert 142.036, molar_mass 'Na2SO4'
assert 84.162, molar_mass 'C6H12'
assert 186.295, molar_mass 'COOH(C(CH3)2)3CH3'
assert 176.124, molar_mass 'C6H4O2(OH)4' # Vitamin C
assert 386.664, molar_mass 'C27H46O' # Cholesterol
assert 315, molar_mass 'Uue'
```



### Regular Expression

```coffeescript
ATOMIC_MASS = {H:1.008,C:12.011,O:15.999,Na:22.98976928,S:32.06,Uue:315}

mul = (match, p1, offset, string) -> '*' + p1
add = (match, p1, offset, string) ->
	if p1 == '(' then return '+' + p1
	"+#{ATOMIC_MASS[p1]}"

molar_mass = (s) ->
	s = s.replace /(\d+)/g, mul
	s = s.replace /([A-Z][a-z]{0,2}|\()/g, add
	parseFloat(eval(s).toFixed(3))

assert 1.008, molar_mass('H')
assert 2.016, molar_mass('H2')
assert 18.015, molar_mass('H2O')
assert 34.014, molar_mass('H2O2')
assert 34.014, molar_mass('(HO)2')
assert 142.036, molar_mass('Na2SO4')
assert 84.162, molar_mass('C6H12')
assert 186.295, molar_mass('COOH(C(CH3)2)3CH3')
assert 176.124, molar_mass('C6H4O2(OH)4') # Vitamin C
assert 386.664, molar_mass('C27H46O') # Cholesterol
assert 315, molar_mass('Uue')
```



## D

```d
import std.array;
import std.conv;
import std.format;
import std.stdio;
import std.string;

double[string] atomicMass;
static this() {
    atomicMass = [
        "H":   1.008,
        "He":  4.002602,
        "Li":  6.94,
        "Be":  9.0121831,
        "B":   10.81,
        "C":   12.011,
        "N":   14.007,
        "O":   15.999,
        "F":   18.998403163,
        "Ne":  20.1797,
        "Na":  22.98976928,
        "Mg":  24.305,
        "Al":  26.9815385,
        "Si":  28.085,
        "P":   30.973761998,
        "S":   32.06,
        "Cl":  35.45,
        "Ar":  39.948,
        "K":   39.0983,
        "Ca":  40.078,
        "Sc":  44.955908,
        "Ti":  47.867,
        "V":   50.9415,
        "Cr":  51.9961,
        "Mn":  54.938044,
        "Fe":  55.845,
        "Co":  58.933194,
        "Ni":  58.6934,
        "Cu":  63.546,
        "Zn":  65.38,
        "Ga":  69.723,
        "Ge":  72.630,
        "As":  74.921595,
        "Se":  78.971,
        "Br":  79.904,
        "Kr":  83.798,
        "Rb":  85.4678,
        "Sr":  87.62,
        "Y":   88.90584,
        "Zr":  91.224,
        "Nb":  92.90637,
        "Mo":  95.95,
        "Ru":  101.07,
        "Rh":  102.90550,
        "Pd":  106.42,
        "Ag":  107.8682,
        "Cd":  112.414,
        "In":  114.818,
        "Sn":  118.710,
        "Sb":  121.760,
        "Te":  127.60,
        "I":   126.90447,
        "Xe":  131.293,
        "Cs":  132.90545196,
        "Ba":  137.327,
        "La":  138.90547,
        "Ce":  140.116,
        "Pr":  140.90766,
        "Nd":  144.242,
        "Pm":  145,
        "Sm":  150.36,
        "Eu":  151.964,
        "Gd":  157.25,
        "Tb":  158.92535,
        "Dy":  162.500,
        "Ho":  164.93033,
        "Er":  167.259,
        "Tm":  168.93422,
        "Yb":  173.054,
        "Lu":  174.9668,
        "Hf":  178.49,
        "Ta":  180.94788,
        "W":   183.84,
        "Re":  186.207,
        "Os":  190.23,
        "Ir":  192.217,
        "Pt":  195.084,
        "Au":  196.966569,
        "Hg":  200.592,
        "Tl":  204.38,
        "Pb":  207.2,
        "Bi":  208.98040,
        "Po":  209,
        "At":  210,
        "Rn":  222,
        "Fr":  223,
        "Ra":  226,
        "Ac":  227,
        "Th":  232.0377,
        "Pa":  231.03588,
        "U":   238.02891,
        "Np":  237,
        "Pu":  244,
        "Am":  243,
        "Cm":  247,
        "Bk":  247,
        "Cf":  251,
        "Es":  252,
        "Fm":  257,
        "Uue": 315,
        "Ubn": 299,
    ];
}

double evaluate(string s) {
    s ~= "["; // add end of string marker
    double sum = 0.0;
    string symbol, number;
    for (int i = 0; i < s.length; ++i) {
        auto c = s[i];
        if (c >= '@' && c <= '[') {
            // @, A-Z, [
            int n = 1;
            if (number != "") {
                n = to!int(number);
            }
            if (symbol != "") {
                sum += atomicMass[symbol] * n;
            }
            if (c == '[') {
                break;
            }
            symbol = c.to!string;
            number = "";
        } else if (c >= 'a' && c <= 'z') {
            symbol ~= c;
        } else if (c >= '0' && c <= '9') {
            number ~= c;
        } else {
            throw new Exception("Unexpected symbol " ~ c ~ " in molecule");
        }
    }
    return sum;
}

string replaceParens(string s) {
    char letter = 'a';
    while (true) {
        auto start = s.indexOf('(');
        if (start == -1) {
            break;
        }

        restart:
        for (auto i = start + 1; i < s.length; ++i) {
            if (s[i] == ')') {
                auto expr = s[start + 1 .. i];
                auto symbol = format("@%c", letter);
                s = s.replaceFirst(s[start .. i + 1], symbol);
                atomicMass[symbol] = evaluate(expr);
                letter++;
                break;
            }
            if (s[i] == '(') {
                start = i;
                continue restart;
            }
        }
    }
    return s;
}

void main() {
    auto molecules = [
        "H", "H2", "H2O", "H2O2", "(HO)2", "Na2SO4", "C6H12",
        "COOH(C(CH3)2)3CH3", "C6H4O2(OH)4", "C27H46O", "Uue"
    ];
    foreach (molecule; molecules) {
        auto mass = evaluate(replaceParens(molecule));
        writefln("%17s -> %7.3f", molecule, mass);
    }
    writeln(atomicMass);
}
```

```txt
                H ->   1.008
               H2 ->   2.016
              H2O ->  18.015
             H2O2 ->  34.014
            (HO)2 ->  34.014
           Na2SO4 -> 142.036
            C6H12 ->  84.162
COOH(C(CH3)2)3CH3 -> 186.295
      C6H4O2(OH)4 -> 176.124
          C27H46O -> 386.664
              Uue -> 315.000
```



## Factor

```factor
USING: assocs compiler.units definitions grouping infix.parser
infix.private kernel math.functions math.parser multiline
peg.ebnf qw sequences splitting strings words words.constant ;
IN: rosetta-code.chemical-calculator

<<     ! Do the stuff inside << ... >> at parse time.

HEREDOC: END
H  1.008         He 4.002602      Li 6.94
Be 9.0121831     B  10.81         C  12.011
N  14.007        O  15.999        F  18.998403163
Ne 20.1797       Na 22.98976928   Mg 24.305
Al 26.9815385    Si 28.085        P  30.973761998
S  32.06         Cl 35.45         Ar 39.948
K  39.0983       Ca 40.078        Sc 44.955908
Ti 47.867        V  50.9415       Cr 51.9961
Mn 54.938044     Fe 55.845        Co 58.933194
Ni 58.6934       Cu 63.546        Zn 65.38
Ga 69.723        Ge 72.630        As 74.921595
Se 78.971        Br 79.904        Kr 83.798
Rb 85.4678       Sr 87.62         Y  88.90584
Zr 91.224        Nb 92.90637      Mo 95.95
Ru 101.07        Rh 102.90550     Pd 106.42
Ag 107.8682      Cd 112.414       In 114.818
Sn 118.710       Sb 121.760       Te 127.60
I  126.90447     Xe 131.293       Cs 132.90545196
Ba 137.327       La 138.90547     Ce 140.116
Pr 140.90766     Nd 144.242       Pm 145
Sm 150.36        Eu 151.964       Gd 157.25
Tb 158.92535     Dy 162.500       Ho 164.93033
Er 167.259       Tm 168.93422     Yb 173.054
Lu 174.9668      Hf 178.49        Ta 180.94788
W  183.84        Re 186.207       Os 190.23
Ir 192.217       Pt 195.084       Au 196.966569
Hg 200.592       Tl 204.38        Pb 207.2
Bi 208.98040     Po 209           At 210
Rn 222           Fr 223           Ra 226
Ac 227           Th 232.0377      Pa 231.03588
U  238.02891     Np 237           Pu 244
Am 243           Cm 247           Bk 247
Cf 251           Es 252           Fm 257
END

! Make constants from the pairs in the above string.
" \n" split harvest 2 <groups> [
    first2 [
        [ "rosetta-code.chemical-calculator" create-word ] dip
        string>number define-constant
    ] 2curry with-compilation-unit
] each

>>

! Evaluate a string like "+C+O+O+H+(+C+(+C+H*3)*2)*3+C+H*3"
! Note that the infix vocabulary can work with the constants
! defined above.
: eval-infix ( seq -- n )
    build-infix-ast infix-codegen prepare-operand call( -- x ) ;

! A grammar to put a + before every element/left paren and a *
! before every number and then evaluate the expression.
EBNF: molar-mass [=[
  number = [0-9]+ => [[ "" like "*" prepend ]]
  elt = [A-Z] [a-z]? [a-z]? => [[ sift "" like "+" prepend ]]
  lparen = "(" => [[ "" like "+" prepend ]]
  any = . => [[ 1string ]]
  mass = (elt|number|lparen|any)+ => [[ concat eval-infix ]]
]=]

! assert= doesn't work due to floating point weirdness.
ERROR: failed-assertion expected +/- got ;
: approx-assert= ( x y epsilon -- )
    3dup ~ [ 3drop ] [ swap failed-assertion ] if ;

: chemical-calculator-demo ( -- )
    {
        { 1.008 "H" }
        { 2.016 "H2" }
        { 18.015 "H2O" }
        { 142.03553856 "Na2SO4" }
        { 84.16200000000001 "C6H12" }
        { 186.295 "COOH(C(CH3)2)3CH3" }
    } [ molar-mass 1e-5 approx-assert= ] assoc-each ;

MAIN: chemical-calculator-demo
```


No assertion errors.

=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Chemical_Calculator this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go

This doesn't use regular expressions, RPN or eval (which Go doesn't have). It's just a simple molar mass evaluator written from scratch.

```go
package main

import (
    "fmt"
    "strconv"
    "strings"
)

var atomicMass = map[string]float64{
    "H":   1.008,
    "He":  4.002602,
    "Li":  6.94,
    "Be":  9.0121831,
    "B":   10.81,
    "C":   12.011,
    "N":   14.007,
    "O":   15.999,
    "F":   18.998403163,
    "Ne":  20.1797,
    "Na":  22.98976928,
    "Mg":  24.305,
    "Al":  26.9815385,
    "Si":  28.085,
    "P":   30.973761998,
    "S":   32.06,
    "Cl":  35.45,
    "Ar":  39.948,
    "K":   39.0983,
    "Ca":  40.078,
    "Sc":  44.955908,
    "Ti":  47.867,
    "V":   50.9415,
    "Cr":  51.9961,
    "Mn":  54.938044,
    "Fe":  55.845,
    "Co":  58.933194,
    "Ni":  58.6934,
    "Cu":  63.546,
    "Zn":  65.38,
    "Ga":  69.723,
    "Ge":  72.630,
    "As":  74.921595,
    "Se":  78.971,
    "Br":  79.904,
    "Kr":  83.798,
    "Rb":  85.4678,
    "Sr":  87.62,
    "Y":   88.90584,
    "Zr":  91.224,
    "Nb":  92.90637,
    "Mo":  95.95,
    "Ru":  101.07,
    "Rh":  102.90550,
    "Pd":  106.42,
    "Ag":  107.8682,
    "Cd":  112.414,
    "In":  114.818,
    "Sn":  118.710,
    "Sb":  121.760,
    "Te":  127.60,
    "I":   126.90447,
    "Xe":  131.293,
    "Cs":  132.90545196,
    "Ba":  137.327,
    "La":  138.90547,
    "Ce":  140.116,
    "Pr":  140.90766,
    "Nd":  144.242,
    "Pm":  145,
    "Sm":  150.36,
    "Eu":  151.964,
    "Gd":  157.25,
    "Tb":  158.92535,
    "Dy":  162.500,
    "Ho":  164.93033,
    "Er":  167.259,
    "Tm":  168.93422,
    "Yb":  173.054,
    "Lu":  174.9668,
    "Hf":  178.49,
    "Ta":  180.94788,
    "W":   183.84,
    "Re":  186.207,
    "Os":  190.23,
    "Ir":  192.217,
    "Pt":  195.084,
    "Au":  196.966569,
    "Hg":  200.592,
    "Tl":  204.38,
    "Pb":  207.2,
    "Bi":  208.98040,
    "Po":  209,
    "At":  210,
    "Rn":  222,
    "Fr":  223,
    "Ra":  226,
    "Ac":  227,
    "Th":  232.0377,
    "Pa":  231.03588,
    "U":   238.02891,
    "Np":  237,
    "Pu":  244,
    "Am":  243,
    "Cm":  247,
    "Bk":  247,
    "Cf":  251,
    "Es":  252,
    "Fm":  257,
    "Uue": 315,
    "Ubn": 299,
}

func replaceParens(s string) string {
    var letter byte = 'a'
    for {
        start := strings.IndexByte(s, '(')
        if start == -1 {
            break
        }
    restart:
        for i := start + 1; i < len(s); i++ {
            if s[i] == ')' {
                expr := s[start+1 : i]
                symbol := fmt.Sprintf("@%c", letter)
                s = strings.Replace(s, s[start:i+1], symbol, 1)
                atomicMass[symbol] = evaluate(expr)
                letter++
                break
            }
            if s[i] == '(' {
                start = i
                goto restart
            }
        }
    }
    return s
}

func evaluate(s string) float64 {
    s += string('[') // add end of string marker
    var symbol, number string
    sum := 0.0
    for i := 0; i < len(s); i++ {
        c := s[i]
        switch {
        case c >= '@' && c <= '[': // @, A-Z, [
            n := 1
            if number != "" {
                n, _ = strconv.Atoi(number)
            }
            if symbol != "" {
                sum += atomicMass[symbol] * float64(n)
            }
            if c == '[' {
                break
            }
            symbol = string(c)
            number = ""
        case c >= 'a' && c <= 'z':
            symbol += string(c)
        case c >= '0' && c <= '9':
            number += string(c)
        default:
            panic(fmt.Sprintf("Unexpected symbol %c in molecule", c))
        }
    }
    return sum
}

func main() {
    molecules := []string{
        "H", "H2", "H2O", "H2O2", "(HO)2", "Na2SO4", "C6H12", "COOH(C(CH3)2)3CH3",
        "C6H4O2(OH)4", "C27H46O", "Uue",
    }
    for _, molecule := range molecules {
        mass := evaluate(replaceParens(molecule))
        fmt.Printf("%17s -> %7.3f\n", molecule, mass)
    }
}
```


```txt

                H ->   1.008
               H2 ->   2.016
              H2O ->  18.015
             H2O2 ->  34.014
            (HO)2 ->  34.014
           Na2SO4 -> 142.036
            C6H12 ->  84.162
COOH(C(CH3)2)3CH3 -> 186.295
      C6H4O2(OH)4 -> 176.124
          C27H46O -> 386.664
              Uue -> 315.000

```



## Julia

Note that Julia's 64-bit floating point gets a slightly different result for one of the assertions, hence a small change in the last example. The function uses Julia's own language parser to evaluate the compound as an arithmetic expression.

```julia
const H = 1.008
const He = 4.002602
const Li = 6.94
const Be = 9.0121831
const B =  10.81
const C =  12.011
const N =  14.007
const O =  15.999
const F =  18.998403163
const Ne = 20.1797
const Na = 22.98976928
const Mg = 24.305
const Al = 26.9815385
const Si = 28.085
const P =  30.973761998
const S =  32.06
const Cl = 35.45
const Ar = 39.948
const K =  39.0983
const Ca = 40.078
const Sc = 44.955908
const Ti = 47.867
const V =  50.9415
const Cr = 51.9961
const Mn = 54.938044
const Fe = 55.845
const Co = 58.933194
const Ni = 58.6934
const Cu = 63.546
const Zn = 65.38
const Ga = 69.723
const Ge = 72.630
const As = 74.921595
const Se = 78.971
const Br = 79.904
const Kr = 83.798
const Rb = 85.4678
const Sr = 87.62
const Y =  88.90584
const Zr = 91.224
const Nb = 92.90637
const Mo = 95.95
const Ru = 101.07
const Rh = 102.90550
const Pd = 106.42
const Ag = 107.8682
const Cd = 112.414
const In = 114.818
const Sn = 118.710
const Sb = 121.760
const Te = 127.60
const I =  126.90447
const Xe = 131.293
const Cs = 132.90545196
const Ba = 137.327
const La = 138.90547
const Ce = 140.116
const Pr = 140.90766
const Nd = 144.242
const Pm = 145
const Sm = 150.36
const Eu = 151.964
const Gd = 157.25
const Tb = 158.92535
const Dy = 162.500
const Ho = 164.93033
const Er = 167.259
const Tm = 168.93422
const Yb = 173.054
const Lu = 174.9668
const Hf = 178.49
const Ta = 180.94788
const W =  183.84
const Re = 186.207
const Os = 190.23
const Ir = 192.217
const Pt = 195.084
const Au = 196.966569
const Hg = 200.592
const Tl = 204.38
const Pb = 207.2
const Bi = 208.98040
const Po = 209
const At = 210
const Rn = 222
const Fr = 223
const Ra = 226
const Ac = 227
const Th = 232.0377
const Pa = 231.03588
const U =  238.02891
const Np = 237
const Pu = 244
const Am = 243
const Cm = 247
const Bk = 247
const Cf = 251
const Es = 252
const Fm = 257


function molar_mass(s)
    s = replace(s, r"\d+" => (x) -> "*" * x)
    s = replace(s, r"[A-Z][a-z]{0,2}|\(" => (x) ->"+" * x)
    eval(Meta.parse(s))
end

@assert 1.008 == molar_mass("H")
@assert 2.016 == molar_mass("H2")
@assert 18.015 == molar_mass("H2O")
@assert 142.03553856000002 == molar_mass("Na2SO4")
@assert 84.162 == molar_mass("C6H12")
@assert 186.29500000000002 == molar_mass("COOH(C(CH3)2)3CH3")

```

No assertion errors.


## Kotlin

```scala
var atomicMass = mutableMapOf(
    "H" to 1.008,
    "He" to 4.002602,
    "Li" to 6.94,
    "Be" to 9.0121831,
    "B" to 10.81,
    "C" to 12.011,
    "N" to 14.007,
    "O" to 15.999,
    "F" to 18.998403163,
    "Ne" to 20.1797,
    "Na" to 22.98976928,
    "Mg" to 24.305,
    "Al" to 26.9815385,
    "Si" to 28.085,
    "P" to 30.973761998,
    "S" to 32.06,
    "Cl" to 35.45,
    "Ar" to 39.948,
    "K" to 39.0983,
    "Ca" to 40.078,
    "Sc" to 44.955908,
    "Ti" to 47.867,
    "V" to 50.9415,
    "Cr" to 51.9961,
    "Mn" to 54.938044,
    "Fe" to 55.845,
    "Co" to 58.933194,
    "Ni" to 58.6934,
    "Cu" to 63.546,
    "Zn" to 65.38,
    "Ga" to 69.723,
    "Ge" to 72.630,
    "As" to 74.921595,
    "Se" to 78.971,
    "Br" to 79.904,
    "Kr" to 83.798,
    "Rb" to 85.4678,
    "Sr" to 87.62,
    "Y" to 88.90584,
    "Zr" to 91.224,
    "Nb" to 92.90637,
    "Mo" to 95.95,
    "Ru" to 101.07,
    "Rh" to 102.90550,
    "Pd" to 106.42,
    "Ag" to 107.8682,
    "Cd" to 112.414,
    "In" to 114.818,
    "Sn" to 118.710,
    "Sb" to 121.760,
    "Te" to 127.60,
    "I" to 126.90447,
    "Xe" to 131.293,
    "Cs" to 132.90545196,
    "Ba" to 137.327,
    "La" to 138.90547,
    "Ce" to 140.116,
    "Pr" to 140.90766,
    "Nd" to 144.242,
    "Pm" to 145.0,
    "Sm" to 150.36,
    "Eu" to 151.964,
    "Gd" to 157.25,
    "Tb" to 158.92535,
    "Dy" to 162.500,
    "Ho" to 164.93033,
    "Er" to 167.259,
    "Tm" to 168.93422,
    "Yb" to 173.054,
    "Lu" to 174.9668,
    "Hf" to 178.49,
    "Ta" to 180.94788,
    "W" to 183.84,
    "Re" to 186.207,
    "Os" to 190.23,
    "Ir" to 192.217,
    "Pt" to 195.084,
    "Au" to 196.966569,
    "Hg" to 200.592,
    "Tl" to 204.38,
    "Pb" to 207.2,
    "Bi" to 208.98040,
    "Po" to 209.0,
    "At" to 210.0,
    "Rn" to 222.0,
    "Fr" to 223.0,
    "Ra" to 226.0,
    "Ac" to 227.0,
    "Th" to 232.0377,
    "Pa" to 231.03588,
    "U" to 238.02891,
    "Np" to 237.0,
    "Pu" to 244.0,
    "Am" to 243.0,
    "Cm" to 247.0,
    "Bk" to 247.0,
    "Cf" to 251.0,
    "Es" to 252.0,
    "Fm" to 257.0,
    "Uue" to 315.0,
    "Ubn" to 299.0
)

fun evaluate(s: String): Double {
    val sym = "$s["
    var sum = 0.0
    var symbol = ""
    var number = ""
    for (i in 0 until sym.length) {
        val c = sym[i]
        if (c in '@'..'[') {
            // @, A-Z, [
            var n = 1
            if (number != "") {
                n = Integer.parseInt(number)
            }
            if (symbol != "") {
                sum += atomicMass.getOrElse(symbol) { 0.0 } * n
            }
            if (c == '[') {
                break
            }
            symbol = c.toString()
            number = ""
        } else if (c in 'a'..'z') {
            symbol += c
        } else if (c in '0'..'9') {
            number += c
        } else {
            throw RuntimeException("Unexpected symbol $c in molecule")
        }
    }
    return sum
}

fun replaceParens(s: String): String {
    var letter = 'a'
    var si = s
    while (true) {
        var start = si.indexOf('(')
        if (start == -1) {
            break
        }

        for (i in start + 1 until si.length) {
            if (si[i] == ')') {
                val expr = si.substring(start + 1 until i)
                val symbol = "@$letter"
                si = si.replaceFirst(si.substring(start until i + 1), symbol)
                atomicMass[symbol] = evaluate(expr)
                letter++
                break
            }
            if (si[i] == '(') {
                start = i
                continue
            }
        }
    }
    return si
}

fun main() {
    val molecules = listOf(
        "H", "H2", "H2O", "H2O2", "(HO)2", "Na2SO4", "C6H12",
        "COOH(C(CH3)2)3CH3", "C6H4O2(OH)4", "C27H46O", "Uue"
    )
    for (molecule in molecules) {
        val mass = evaluate(replaceParens(molecule))
        val moleStr = "%17s".format(molecule)
        val massStr = "%7.3f".format(mass)
        println("$moleStr -> $massStr")
    }
}
```

```txt
                H ->   1.008
               H2 ->   2.016
              H2O ->  18.015
             H2O2 ->  34.014
            (HO)2 ->  34.014
           Na2SO4 -> 142.036
            C6H12 ->  84.162
COOH(C(CH3)2)3CH3 -> 186.295
      C6H4O2(OH)4 -> 176.124
          C27H46O -> 386.664
              Uue -> 315.000
```



## Nim

* Nim lacks runtime eval, that's the reason for so much code. (And me being a sloppy programmer)
* Also, seqs can't contain mixed types.


```python
#? replace(sub = "\t", by = "  ")

import tables, strutils, sequtils, math

let ATOMIC_MASS = {"H":1.008, "C":12.011, "O":15.999, "Na":22.98976928, "S":32.06, "Uue":315.0}.toTable

proc pass1(s:string): seq[string] = # "H2O" => @["H","*","2","+","O"]
	result.add "0"
	var i = 0
	proc member(a:char,c:char): bool = i < s.len and a <= s[i] and s[i] <= c
	proc next(): char =
		i += 1
		s[i-1]
	while i < s.len:
		if s[i] == '(':
			result = result.concat @["+","("]
			discard next()
		elif s[i] == ')': result.add $next()
		elif member('0','9'):
			var number = ""
			result.add "*"
			while member('0','9'): number &= $next()
			result.add number
		elif member('A','Z'):
			if i>0 and s[i-1] != '(': result.add "+"
			var name = ""
			name.add next()
			while member('a','z'): name.add next()
			result.add name

proc pass2(s:string): seq[string] = # "H2O" => @["H", "2", "*", "O", "+"]
	let ops = "+*"
	var tokens = pass1 s
	var stack: seq[string]
	var op: string

	for token in tokens:
		case token
		of "(": stack.add token
		of ")":
			while stack.len > 0:
				op = stack.pop()
				if op == "(": break
				result.add op
		else:
			if token in ops:
				while stack.len > 0:
					op = stack[^1]
					if not (op in ops): break
					if ops.find(token) >= ops.find(op): break
					discard stack.pop()
					result.add op
				stack.add token
			else: result.add token

	while stack.len > 0: result.add stack.pop()

proc pass3(s:string): Table[string,int] = # "H2O" => { H:2, O:1 }
	let rpn: seq[string] = pass2 s
	var stack: seq[Table[string,int]] = @[]
	for item in rpn:
		if item == "+":
			let h1:Table[string,int] = stack.pop()
			let h2:Table[string,int] = stack.pop()
			var res: Table[string,int] = initTable[string,int]()
			for key in h1.keys:
				if key != "factor":
					res[key] = h1[key]
			for key in h2.keys:
				if key != "factor":
					if h1.haskey key:
						res[key] = h1[key] + h2[key]
					else:
						res[key] = h2[key]
			stack.add res
		elif item == "*":
			let top: Table[string,int] = stack.pop() #
			let hash: Table[string,int] = stack.pop()
			let factor: int = top["factor"]
			var res: Table[string,int] = initTable[string,int]()
			for key in hash.keys:
				let str : string = key
				let value: int = hash[key]
				res[key] = value * factor
			stack.add res
		elif ATOMIC_MASS.haskey(item):
			let res : Table[string,int] = {item: 1}.toTable
			stack.add res
		else: # number
			let factor : int = parseInt item
			let res : Table[string,int] = {"factor": factor}.toTable
			stack.add res
	return stack.pop()

proc pass4(s: string) : float = # "H2O" => 18.015
	let atoms: Table[string,int] = pass3 s
	for key in atoms.keys:
		let count : int = atoms[key]
		result += float(count) * ATOMIC_MASS[key]
	round result,3

let molar_mass = pass4

assert 18.015 == molar_mass "H2O"
assert 34.014 == molar_mass "H2O2"
assert 34.014 == molar_mass "(HO)2"
assert 142.036 == molar_mass "Na2SO4"
assert 84.162 == molar_mass "C6H12"
assert 186.295 == molar_mass "COOH(C(CH3)2)3CH3"
assert 176.124 == molar_mass "C6H4O2(OH)4" # Vitamin C
assert 386.664 == molar_mass "C27H46O" # Cholesterol
assert 315 == molar_mass "Uue"
```



## Perl


### Grammar


```perl
use strict;
use warnings;
use List::Util;
use Parse::RecDescent;

my $g = Parse::RecDescent->new(<<'EOG');
  {
     my %atomic_weight = <H 1.008 C 12.011 O 15.999 Na 22.99 S 32.06>
  }

  weight   : compound         { $item[1] }
  compound : group(s)         { List::Util::sum( @{$item[1]} ) }
  group    : element /\d+/    { $item[1] * $item[2] }
           | element          { $item[1] }
  element  : /[A-Z][a-z]*/    { $atomic_weight{ $item[1] } }
           | "(" compound ")" { $item[2] }
EOG

for (<H H2 H2O Na2SO4 C6H12 COOH(C(CH3)2)3CH3>) {
    printf "%7.3f %s\n", $g->weight($_), $_
}
```



### Regular Expression


```perl
use strict;
use warnings;
my %atomic_weight = < H 1.008 C 12.011 O 15.999 Na 22.99 S 32.06 >;

sub molar_mass {
    my($mf) = @_;
    my(%count,$mass);
    my $mf_orig = $mf;
    my $mf_std;

    # expand repeated groups
    $mf =~ s/(.*)\((.*?)\)(\d*)/$1 . $2 x ($3 ? $3 : 1) /e while $mf =~ m/\(/;

    # totals for each atom type
    $mf =~ s/([A-Z][a-z]{0,2})(\d*)/ $count{$1} += $2 ? $2 : 1/eg;

    # calculate molar mass and display, along with standardized MF and original MF
    $mass += $count{$_}*$atomic_weight{"$_"} for sort keys %count;
    $mf_std .= 'C' . $count{C} if $count{C};
    $mf_std .= 'H' . $count{H} if $count{H};
    $mf_std .= $_  . $count{$_} for grep { $_ ne 'C' and $_ ne 'H' } sort keys %count;
    $mf     .= $count{$_} * $atomic_weight{$_} for sort keys %count;
    printf "%7.3f %-9s %s\n", $mass, $mf_std, $mf_orig;
}

molar_mass($_) for <H H2 H2O Na2SO4 C6H12 COOH(C(CH3)2)3CH3>
```

```txt
  1.008 H1        H
  2.016 H2        H2
 18.015 H2O1      H2O
142.036 Na2O4S1   Na2SO4
 84.162 C6H12     C6H12
186.295 C11H22O2  COOH(C(CH3)2)3CH3
```



## Perl 6


```perl6
my %ATOMIC_MASS =
    H  =>   1.008       , Fe =>  55.845    , Te => 127.60       , Ir => 192.217    ,
    He =>   4.002602    , Co =>  58.933194 , I  => 126.90447    , Pt => 195.084    ,
    Li =>   6.94        , Ni =>  58.6934   , Xe => 131.293      , Au => 196.966569 ,
    Be =>   9.0121831   , Cu =>  63.546    , Cs => 132.90545196 , Hg => 200.592    ,
    B  =>  10.81        , Zn =>  65.38     , Ba => 137.327      , Tl => 204.38     ,
    C  =>  12.011       , Ga =>  69.723    , La => 138.90547    , Pb => 207.2      ,
    N  =>  14.007       , Ge =>  72.630    , Ce => 140.116      , Bi => 208.98040  ,
    O  =>  15.999       , As =>  74.921595 , Pr => 140.90766    , Po => 209        ,
    F  =>  18.998403163 , Se =>  78.971    , Nd => 144.242      , At => 210        ,
    Ne =>  20.1797      , Br =>  79.904    , Pm => 145          , Rn => 222        ,
    Na =>  22.98976928  , Kr =>  83.798    , Sm => 150.36       , Fr => 223        ,
    Mg =>  24.305       , Rb =>  85.4678   , Eu => 151.964      , Ra => 226        ,
    Al =>  26.9815385   , Sr =>  87.62     , Gd => 157.25       , Ac => 227        ,
    Si =>  28.085       , Y  =>  88.90584  , Tb => 158.92535    , Th => 232.0377   ,
    P  =>  30.973761998 , Zr =>  91.224    , Dy => 162.500      , Pa => 231.03588  ,
    S  =>  32.06        , Nb =>  92.90637  , Ho => 164.93033    , U  => 238.02891  ,
    Cl =>  35.45        , Mo =>  95.95     , Er => 167.259      , Np => 237        ,
    Ar =>  39.948       , Ru => 101.07     , Tm => 168.93422    , Pu => 244        ,
    K  =>  39.0983      , Rh => 102.90550  , Yb => 173.054      , Am => 243        ,
    Ca =>  40.078       , Pd => 106.42     , Lu => 174.9668     , Cm => 247        ,
    Sc =>  44.955908    , Ag => 107.8682   , Hf => 178.49       , Bk => 247        ,
    Ti =>  47.867       , Cd => 112.414    , Ta => 180.94788    , Cf => 251        ,
    V  =>  50.9415      , In => 114.818    , W  => 183.84       , Es => 252        ,
    Cr =>  51.9961      , Sn => 118.710    , Re => 186.207      , Fm => 257        ,
    Mn =>  54.938044    , Sb => 121.760    , Os => 190.23       ,
;
grammar Chemical_formula {
    my @ATOMIC_SYMBOLS = %ATOMIC_MASS.keys.sort;

    rule  TOP      { ^ (<lparen>|<rparen>|<element>)+ $ }
    token quantity { \d+ }
    token lparen   { '(' }
    token rparen   { ')'                    <quantity>? }
    token element  { $<e>=[@ATOMIC_SYMBOLS] <quantity>? }
}
class Chemical_formula_actions {
    has @stack = 0;
    method TOP     ($/) { $/.make: @stack }
    method lparen  ($/) { push @stack, 0  }
    method rparen  ($/) { my $m = @stack.pop;
                          @stack[*-1] += ($<quantity> // 1) * $m }
    method element ($/) { @stack[*-1] += ($<quantity> // 1) * %ATOMIC_MASS{~$<e>} }
}
sub molar_mass ( Str $formula --> Real ) {
    Chemical_formula.parse( $formula, :actions(Chemical_formula_actions.new) )
        orelse die "Chemical formula not recognized: '$formula'";
    return $/.made.[0];
}
say .&molar_mass.fmt('%7.3f '), $_ for <H H2 H2O Na2SO4 C6H12 COOH(C(CH3)2)3CH3>;
```

```txt
  1.008 H
  2.016 H2
 18.015 H2O
142.036 Na2SO4
 84.162 C6H12
186.295 COOH(C(CH3)2)3CH3
```



## Phix

A simple hand-written single-pass formula parser and evaluator in one.

Note the use of string comparison for error checking rather than floats direct, always much safer in general.

Also note that initially it all worked absolutely fine with the default precision (ie "%g" instead of "%.12g"),
and that the higher precision expected value for Na2SO4 also works just fine at both printing precisions.

```Phix
constant elements = new_dict() -- (eg "H" -> 1.008)

function multiplier(string formula, integer fdx)
-- check for a trailing number, or return 1
    integer n = 1
    if fdx<=length(formula) then
        integer ch = formula[fdx]
        if ch>='1' and ch<='9' then
            n = ch-'0'
            fdx += 1
            while fdx<=length(formula) do
                ch = formula[fdx]
                if ch<'0' or ch>'9' then exit end if
                n = n*10 + ch-'0'
                fdx += 1
            end while
        end if
    end if
    return {n,fdx}
end function

procedure molar_mass(string formula, name, atom expected)
    sequence stack = {0} -- (for parenthesis handling)
    integer sdx = 1, fdx = 1, n
    while fdx<=length(formula) do
        integer ch = formula[fdx]
        if ch>='A' and ch<='Z' then
            -- All elements start with capital, rest lower
            integer fend = fdx
            while fend<length(formula) do
                ch = formula[fend+1]
                if ch<'a' or ch>'z' then exit end if
                fend += 1
            end while
            string element = formula[fdx..fend]
            atom mass = getd(element,elements)
            if mass=0 then ?9/0 end if -- missing?
            {n,fdx} = multiplier(formula,fend+1)
            stack[sdx] += n*mass
        elsif ch='(' then
            sdx += 1
            stack &= 0
            fdx += 1
        elsif ch=')' then
            {n,fdx} = multiplier(formula,fdx+1)
            sdx -= 1
            stack[sdx] += stack[$]*n
            stack = stack[1..sdx]
        else
            ?9/0    -- unknown?
        end if
    end while
    if sdx!=1 then ?9/0 end if -- unbalanced brackets?
    if name!="" then formula &= " ("&name&")" end if
--  string res = sprintf("%g",stack[1]),    -- (still fine)
--         e = sprintf("%g",expected)       --     """
    string res = sprintf("%.12g",stack[1]),
           e = sprintf("%.12g",expected)
    if res!=e then res &= " *** ERROR: expected "&e end if
    printf(1,"%26s = %s\n",{formula,res})
end procedure

-- (following clipped for brevity, works fine with whole table from task description pasted in)
constant etext = split("""
H,1.008
C,12.011
O,15.999
Na,22.98976928
S,32.06
Cl,35.45
Uue,315""","\n")
for i=1 to length(etext) do
    {{string element,atom mass}} = scanf(etext[i],"%s,%f")
    setd(element,mass,elements)
end for
molar_mass("H","Hydrogen",1.008)
molar_mass("H2","Hydrogen gas",2.016)
molar_mass("H2O","Water",18.015)
molar_mass("H2O2","Hydrogen peroxide",34.014)
molar_mass("(HO)2","Hydrogen peroxide",34.014)
--molar_mass("Na2SO4","Sodium sulfate",142.036) -- (fine for "%g")
molar_mass("Na2SO4","Sodium sulfate",142.03553856)  --   """
molar_mass("C6H12","Cyclohexane",84.162)
molar_mass("COOH(C(CH3)2)3CH3","",186.295)
molar_mass("C6H4O2(OH)4","Vitamin C",176.124)
molar_mass("C27H46O","Cholesterol",386.664)
molar_mass("Uue","Ununennium",315)
molar_mass("UueCl","",350.45)
```

```txt

              H (Hydrogen) = 1.008
         H2 (Hydrogen gas) = 2.016
               H2O (Water) = 18.015
  H2O2 (Hydrogen peroxide) = 34.014
 (HO)2 (Hydrogen peroxide) = 34.014
   Na2SO4 (Sodium sulfate) = 142.03553856
       C6H12 (Cyclohexane) = 84.162
         COOH(C(CH3)2)3CH3 = 186.295
   C6H4O2(OH)4 (Vitamin C) = 176.124
     C27H46O (Cholesterol) = 386.664
          Uue (Ununennium) = 315
                     UueCl = 350.45

```



## Python

```python
import re

ATOMIC_MASS = {"H":1.008, "C":12.011, "O":15.999, "Na":22.98976928, "S":32.06, "Uue":315}

mul = lambda x : '*' + x.group(0)
def add(x) :
    name = x.group(0)
    return '+' + name if name == '(' else '+' + str(ATOMIC_MASS[name])

def molar_mass(s):
    nazwa = s
    s = re.sub(r"\d+", mul, s)
    s = re.sub(r"[A-Z][a-z]{0,2}|\(", add, s)
    return print("Atomic mass {:17s} {} {:7.3f}".format(nazwa,'\t',round(eval(s),3)))


```

```txt
Atomic mass H                 	   1.008
Atomic mass H2                	   2.016
Atomic mass H2O               	  18.015
Atomic mass H2O2              	  34.014
Atomic mass (HO)2             	  34.014
Atomic mass Na2SO4            	 142.036
Atomic mass C6H12             	  84.162
Atomic mass COOH(C(CH3)2)3CH3 	 186.295
Atomic mass C6H4O2(OH)4       	 176.124
Atomic mass C27H46O           	 386.664
Atomic mass Uue               	 315.000
```



## Racket



```racket
#lang racket

(define table '([H 1.008]
                [C 12.011]
                [O 15.999]
                [Na 22.98976928]
                [S 32.06]
                [Uue 315.0]))

(define (lookup s) (first (dict-ref table s)))

(define (calc s)
  (define toks
    (with-input-from-string (regexp-replaces s '([#px"(\\d+)" " \\1"]
                                                 [#px"([A-Z])" " \\1"]))
      (thunk (sequence->list (in-port)))))

  (let loop ([toks toks])
    (match toks
      ['() 0]
      [(list (? list? sub) (? number? n) toks ...) (+ (* (loop sub) n) (loop toks))]
      [(list (? list? sub) toks ...) (+ (loop sub) (loop toks))]
      [(list sym (? number? n) toks ...) (+ (* (lookup sym) n) (loop toks))]
      [(list sym toks ...) (+ (lookup sym) (loop toks))])))

(define tests '("H"
                "H2"
                "H2O"
                "H2O2"
                "(HO)2"
                "Na2SO4"
                "C6H12"
                "COOH(C(CH3)2)3CH3"
                "C6H4O2(OH)4"
                "C27H46O"
                "Uue"))

(for ([test (in-list tests)])
  (printf "~a: ~a\n"
          (~a test #:align 'right #:min-width 20)
          (~r (calc test) #:precision 3)))
```


```txt

                   H: 1.008
                  H2: 2.016
                 H2O: 18.015
                H2O2: 34.014
               (HO)2: 34.014
              Na2SO4: 142.036
               C6H12: 84.162
   COOH(C(CH3)2)3CH3: 186.295
         C6H4O2(OH)4: 176.124
             C27H46O: 386.664
                 Uue: 315

```



## REXX

This REXX version has some basic error checking to catch malformed chemical formulas.

Also a more precise atomic mass for the (all) known elements is used   (for instance, '''F''').

Some of the elements added for the REXX example are:

    mendelevium (Md), nobelium (No), lawrencium (Lr), rutherfordium (Rf), dubnium (Db),
    seaborgium  (Sg), bohrium (Bh),  hassium (Hs), meitnerium (Mt),  darmstadtium (Ds),
    roentgenium (Rg), copernicium (Cn), nihoniym (Nh), flerovium (Fl),  moscovium (Mc),
    livermorium (Lv), tennessine (Ts),  and  oganesson (Og)

```rexx
/*REXX program  calculates the   molar mass   from a specified chemical formula.        */
numeric digits 30                                /*ensure enough decimal digits for mass*/
/*─────────── [↓]  table of known elements (+2 more) with their atomic mass ────────────*/
@.=             ;  @.Co= 58.933195 ;  @.H =  1.00794   ;  @.Np=237       ;  @.Se= 78.96
                   @.Cr= 51.9961   ;  @.In=114.818     ;  @.N = 14.0067  ;  @.Sg=266
@.Ac=227        ;  @.Cs=132.9054519;  @.Ir=192.217     ;  @.Og=294       ;  @.Si= 28.0855
@.Ag=107.8682   ;  @.Cu= 63.546    ;  @.I =126.904     ;  @.Os=190.23    ;  @.Sm=150.36
@.Al= 26.9815386;  @.C = 12.0107   ;  @.Kr= 83.798     ;  @.O = 15.9994  ;  @.Sn=118.710
@.Am=243        ;  @.Db=262        ;  @.K = 39.0983    ;  @.Pa=231.03588 ;  @.Sr= 87.62
@.Ar= 39.948    ;  @.Ds=271        ;  @.La=138.90547   ;  @.Pb=207.2     ;  @.S = 32.065
@.As= 74.92160  ;  @.Dy=162.500    ;  @.Li=  6.941     ;  @.Pd=106.42    ;  @.Ta=180.94788
@.At=210        ;  @.Er=167.259    ;  @.Lr=262         ;  @.Pm=145       ;  @.Tb=158.92535
@.Au=196.966569 ;  @.Es=252        ;  @.Lu=174.967     ;  @.Po=210       ;  @.Tc= 98
@.Ba=137.327    ;  @.Eu=151.964    ;  @.Lv=292         ;  @.Pr=140.90765 ;  @.Te=127.60
@.Be=  9.012182 ;  @.Fe= 55.845    ;  @.Mc=288         ;  @.Pt=195.084   ;  @.Th=232.03806
@.Bh=264        ;  @.Fl=289        ;  @.Md=258         ;  @.Pu=244       ;  @.Ti= 47.867
@.Bi=208.98040  ;  @.Fm=257        ;  @.Mg= 24.3050    ;  @.P = 30.973762;  @.Tl=204.3833
@.Bk=247        ;  @.Fr=223        ;  @.Mn= 54.938045  ;  @.Ra=226       ;  @.Tm=168.93421
@.Br= 79.904    ;  @.F = 18.9984032;  @.Mo= 95.94      ;  @.Rb= 85.4678  ;  @.Ts=293
@.B = 10.811    ;  @.Ga= 69.723    ;  @.Mt=268         ;  @.Re=186.207   ;  @.U =238.02891
@.Ca= 40.078    ;  @.Gd=157.25     ;  @.Na= 22.98976928;  @.Rf=261       ;  @.V = 50.9415
@.Cd=112.411    ;  @.Ge= 72.64     ;  @.Nb= 92.906     ;  @.Rg=272       ;  @.W =183.84
@.Ce=140.116    ;  @.He=  4.002602 ;  @.Nd=144.242     ;  @.Rh=102.905   ;  @.Xe=131.293
@.Cf=251        ;  @.Hf=178.49     ;  @.Ne= 20.1797    ;  @.Rn=220       ;  @.Yb=173.04
@.Cl= 35.453    ;  @.Hg=200.59     ;  @.Nh=284         ;  @.Ru=101.07    ;  @.Y = 88.90585
@.Cm=247        ;  @.Ho=164.930    ;  @.Ni= 58.6934    ;  @.Sb=121.760   ;  @.Zn= 65.409
@.Cn=285        ;  @.Hs=277        ;  @.No=259         ;  @.Sc= 44.955912;  @.Zr= 91.224
                                                          @.Ubn=299      ;  @.Uue=315
parse arg $
if $='' | $=","  then $= ' H    H2    H2O    H2O2    (HO)2    Na2SO4    C6H12 ' ,
                                 " COOH(C(CH3)2)3CH3    C6H4O2(OH)4     C27H46O     Uue"
  do j=1  for words($);   x= word($, j)          /*obtain the formula of the molecule.  */
  mm= chemCalc(x)                                /*get the molar mass  "  "      "      */
  if mm<0  then iterate                          /*if function had an error, skip output*/
  say right('molar mass of ' x, 50)  " is"   mm  /*display the molar mass of a chemical.*/
  end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
chemCalc: procedure expose @.; parse arg z       /*obtain chemical formula of molecule. */
          lev= 0                                 /*indicates level of parentheses depth.*/
          $.= 0                                  /*the sum of the molar mass  (so far). */
               do k=1  to  length(z);              y= substr(z, k, 1)      /*get a thing*/
               if y=='('  then do;  lev= lev + 1;  iterate k;  end
               if y==')'  then do;  y= substr(z, k+1, 1)
                                    if \datatype(y, 'W')  then do; say "illegal number:" y
                                                                   return -1
                                                               end
                                    n= getNum()                            /*get number.*/
                                    $.lev= $.lev * n;  $$= $.lev; $.lev= 0 /*sum level. */
                                    lev= lev - 1;      $.lev= $.lev + $$   /*add to prev*/
                                    k= k + length(n)                       /*adjust  K. */
                                    iterate   /*k*/
                               end                                         /*[↑] get ele*/
               e=y;   e= getEle();                     upper e             /* and upper.*/
               if   e==.  then do;  say 'missing element: '  e;   return -2;    end
               if @.e==.  then do;  say 'invalid element: '  e;   return -3;    end
               y= substr(z, k+length(e), 1)
               k= k + length(e) - 1                                        /*adjust  K. */
               n= getNum()                                                 /*get number.*/
               if n\==.  then k= k + length(n)                             /*adjust  K. */
                         else n= 1                                         /*no number. */
               $.lev= $.lev   +   n * @.e                                  /*add product*/
               end   /*k*/
          return format($.lev, max(4, pos(., $.lev) ) )                    /*align the #*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
getEle:   if \datatype(y, 'U')  then do;  say err "illegal element: "   y;  return .;  end
                         do i=1  until \datatype(q, 'L');  q= substr(z, k+i, 1)
                         if datatype(q, 'L')  then e= e || q               /*lowercase? */
                         end   /*i*/;                         return e
/*──────────────────────────────────────────────────────────────────────────────────────*/
getNum:   if \datatype(y, 'W')  then return .;             n=
                         do i=1  until \datatype(q, 'W');  q= substr(z, k+i, 1)
                         if datatype(q, 'W')  then n= n || q               /*is a digit?*/
                         end   /*i*/;                         return n
```

```txt

                                  molar mass of  H  is    1.00794
                                 molar mass of  H2  is    2.01588
                                molar mass of  H2O  is   18.01528
                               molar mass of  H2O2  is   34.01468
                              molar mass of  (HO)2  is   34.01468
                             molar mass of  Na2SO4  is  142.04213856
                              molar mass of  C6H12  is   84.15948
                  molar mass of  COOH(C(CH3)2)3CH3  is  186.29118
                        molar mass of  C6H4O2(OH)4  is  176.12412
                            molar mass of  C27H46O  is  386.65354
                                molar mass of  Uue  is  315

```



## Visual Basic .NET

```vbnet
Module Module1

    Dim atomicMass As New Dictionary(Of String, Double) From {
        {"H", 1.008},
        {"He", 4.002602},
        {"Li", 6.94},
        {"Be", 9.0121831},
        {"B", 10.81},
        {"C", 12.011},
        {"N", 14.007},
        {"O", 15.999},
        {"F", 18.998403163},
        {"Ne", 20.1797},
        {"Na", 22.98976928},
        {"Mg", 24.305},
        {"Al", 26.9815385},
        {"Si", 28.085},
        {"P", 30.973761998},
        {"S", 32.06},
        {"Cl", 35.45},
        {"Ar", 39.948},
        {"K", 39.0983},
        {"Ca", 40.078},
        {"Sc", 44.955908},
        {"Ti", 47.867},
        {"V", 50.9415},
        {"Cr", 51.9961},
        {"Mn", 54.938044},
        {"Fe", 55.845},
        {"Co", 58.933194},
        {"Ni", 58.6934},
        {"Cu", 63.546},
        {"Zn", 65.38},
        {"Ga", 69.723},
        {"Ge", 72.63},
        {"As", 74.921595},
        {"Se", 78.971},
        {"Br", 79.904},
        {"Kr", 83.798},
        {"Rb", 85.4678},
        {"Sr", 87.62},
        {"Y", 88.90584},
        {"Zr", 91.224},
        {"Nb", 92.90637},
        {"Mo", 95.95},
        {"Ru", 101.07},
        {"Rh", 102.9055},
        {"Pd", 106.42},
        {"Ag", 107.8682},
        {"Cd", 112.414},
        {"In", 114.818},
        {"Sn", 118.71},
        {"Sb", 121.76},
        {"Te", 127.6},
        {"I", 126.90447},
        {"Xe", 131.293},
        {"Cs", 132.90545196},
        {"Ba", 137.327},
        {"La", 138.90547},
        {"Ce", 140.116},
        {"Pr", 140.90766},
        {"Nd", 144.242},
        {"Pm", 145},
        {"Sm", 150.36},
        {"Eu", 151.964},
        {"Gd", 157.25},
        {"Tb", 158.92535},
        {"Dy", 162.5},
        {"Ho", 164.93033},
        {"Er", 167.259},
        {"Tm", 168.93422},
        {"Yb", 173.054},
        {"Lu", 174.9668},
        {"Hf", 178.49},
        {"Ta", 180.94788},
        {"W", 183.84},
        {"Re", 186.207},
        {"Os", 190.23},
        {"Ir", 192.217},
        {"Pt", 195.084},
        {"Au", 196.966569},
        {"Hg", 200.592},
        {"Tl", 204.38},
        {"Pb", 207.2},
        {"Bi", 208.9804},
        {"Po", 209},
        {"At", 210},
        {"Rn", 222},
        {"Fr", 223},
        {"Ra", 226},
        {"Ac", 227},
        {"Th", 232.0377},
        {"Pa", 231.03588},
        {"U", 238.02891},
        {"Np", 237},
        {"Pu", 244},
        {"Am", 243},
        {"Cm", 247},
        {"Bk", 247},
        {"Cf", 251},
        {"Es", 252},
        {"Fm", 257},
        {"Uue", 315},
        {"Ubn", 299}
    }

    Function Evaluate(s As String) As Double
        s += "["
        Dim sum = 0.0
        Dim symbol = ""
        Dim number = ""
        For i = 1 To s.Length
            Dim c = s(i - 1)
            If "@" <= c AndAlso c <= "[" Then
                ' @,A-Z
                Dim n = 1
                If number <> "" Then
                    n = Integer.Parse(number)
                End If
                If symbol <> "" Then
                    sum += atomicMass(symbol) * n
                End If
                If c = "[" Then
                    Exit For
                End If
                symbol = c.ToString
                number = ""
            ElseIf "a" <= c AndAlso c <= "z" Then
                symbol += c
            ElseIf "0" <= c AndAlso c <= "9" Then
                number += c
            Else
                Throw New Exception(String.Format("Unexpected symbol {0} in molecule", c))
            End If
        Next
        Return sum
    End Function

    Function ReplaceFirst(text As String, search As String, replace As String) As String
        Dim pos = text.IndexOf(search)
        If pos < 0 Then
            Return text
        Else
            Return text.Substring(0, pos) + replace + text.Substring(pos + search.Length)
        End If
    End Function

    Function ReplaceParens(s As String) As String
        Dim letter = "s"c
        While True
            Dim start = s.IndexOf("(")
            If start = -1 Then
                Exit While
            End If

            For i = start + 1 To s.Length - 1
                If s(i) = ")" Then
                    Dim expr = s.Substring(start + 1, i - start - 1)
                    Dim symbol = String.Format("@{0}", letter)
                    s = ReplaceFirst(s, s.Substring(start, i + 1 - start), symbol)
                    atomicMass(symbol) = Evaluate(expr)
                    letter = Chr(Asc(letter) + 1)
                    Exit For
                End If
                If s(i) = "(" Then
                    start = i
                    Continue For
                End If
            Next
        End While
        Return s
    End Function

    Sub Main()
        Dim molecules() As String = {
            "H", "H2", "H2O", "H2O2", "(HO)2", "Na2SO4", "C6H12",
            "COOH(C(CH3)2)3CH3", "C6H4O2(OH)4", "C27H46O", "Uue"
        }
        For Each molecule In molecules
            Dim mass = Evaluate(ReplaceParens(molecule))
            Console.WriteLine("{0,17} -> {1,7:0.000}", molecule, mass)
        Next
    End Sub

End Module
```

```txt
                H ->   1.008
               H2 ->   2.016
              H2O ->  18.015
             H2O2 ->  34.014
            (HO)2 ->  34.014
           Na2SO4 -> 142.036
            C6H12 ->  84.162
COOH(C(CH3)2)3CH3 -> 186.295
      C6H4O2(OH)4 -> 176.124
          C27H46O -> 386.664
              Uue -> 315.000
```



## zkl

Really bad error checking

```zkl
fcn molarMass(str,mass=0.0){
    while(span:=str.span("(",")",False)){  // get inner most () group
      group:=str[span.xplode()];	  // (CH3)
      str   =str.del(span.xplode());      // nuke (CH3)
      w    :=molarMass(group[1,-1],mass); // remove "(" & ")"
      i,s2 := span[0], str[i,*];
      if(m.search(s2))			  // well crap, (CH3)2
         { z:=m.matched[1]; str=str.del(i,z.len()); mass=w*z.toInt() }
      else mass=w;
   }
   ms:=List(mass);	// HO --> (1.008,15.999).sum()
   while(str){
      if(not atomRE.search(str)) throw(Exception.ValueError);
      ns,nm,n := atomRE.matched;
      n=(if(n) n.toInt() else 1);	// H2
      ms.append(atomicMass[nm]*n);
      str=str.del(ns.xplode());		// nuke H or H2
   }
   ms.reduce('+);
}
```


```zkl
var [const] atomicMass = Dictionary(
  "Ac",227.000000, "Ag",107.868200, "Al", 26.981538, "Am",243.000000, "Ar", 39.948000,
  "As", 74.921595, "At",210.000000, "Au",196.966569, "B" , 10.810000, "Ba",137.327000,
  "Be",  9.012183, "Bi",208.980400, "Bk",247.000000, "Br", 79.904000, "C" , 12.011000,
  "Ca", 40.078000, "Cd",112.414000, "Ce",140.116000, "Cf",251.000000, "Cl", 35.450000,
  "Cm",247.000000, "Co", 58.933194, "Cr", 51.996100, "Cs",132.905452, "Cu", 63.546000,
  "Dy",162.500000, "Er",167.259000, "Es",252.000000, "Eu",151.964000, "F" , 18.998403,
  "Fe", 55.845000, "Fm",257.000000, "Fr",223.000000, "Ga", 69.723000, "Gd",157.250000,
  "Ge", 72.630000, "H" ,  1.008000, "He",  4.002602, "Hf",178.490000, "Hg",200.592000,
  "Ho",164.930330, "I" ,126.904470, "In",114.818000, "Ir",192.217000, "K" , 39.098300,
  "Kr", 83.798000, "La",138.905470, "Li",  6.940000, "Lu",174.966800, "Mg", 24.305000,
  "Mn", 54.938044, "Mo", 95.950000, "N" , 14.007000, "Na", 22.989769, "Nb", 92.906370,
  "Nd",144.242000, "Ne", 20.179700, "Ni", 58.693400, "Np",237.000000, "O" , 15.999000,
  "Os",190.230000, "P" , 30.973762, "Pa",231.035880, "Pb",207.200000, "Pd",106.420000,
  "Pm",145.000000, "Po",209.000000, "Pr",140.907660, "Pt",195.084000, "Pu",244.000000,
  "Ra",226.000000, "Rb", 85.467800, "Re",186.207000, "Rh",102.905500, "Rn",222.000000,
  "Ru",101.070000, "S" , 32.060000, "Sb",121.760000, "Sc", 44.955908, "Se", 78.971000,
  "Si", 28.085000, "Sm",150.360000, "Sn",118.710000, "Sr", 87.620000, "Ta",180.947880,
  "Tb",158.925350, "Te",127.600000, "Th",232.037700, "Ti", 47.867000, "Tl",204.380000,
  "Tm",168.934220, "U" ,238.028910, "V" , 50.941500, "W" ,183.840000, "Xe",131.293000,
  "Y" , 88.905840, "Yb",173.054000, "Zn", 65.380000, "Zr", 91.224000,
), m=RegExp("([1-9]+)"),
   atomRE=fcn{  // sort by name length, build RE: "(Lu|Es|Er..|W|Y)([1-9]*)"
	 nms:=atomicMass.keys;
	 ( [(nms.apply("len") : (0).max(_)) .. 1, -1].pump(List, // 2..1
	    'wrap(n){ nms.filter('wrap(nm){ nm.len()==n }) }).flatten()
	   .concat("|","(",")([1-9]*)") )
	 : RegExp(_);
   }();
```


```zkl
foreach cstr in (T("H","H2","H2O","Na2SO4","C6H12","COOH(C(CH3)2)3CH3"))
   { println(cstr," --> ",molarMass(cstr)) }
```

```txt

H --> 1.008
H2 --> 2.016
H2O --> 18.015
Na2SO4 --> 142.036
C6H12 --> 84.162
COOH(C(CH3)2)3CH3 --> 186.295

```

