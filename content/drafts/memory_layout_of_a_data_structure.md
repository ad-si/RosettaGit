+++
title = "Memory layout of a data structure"
description = ""
date = 2019-09-08T20:06:28Z
aliases = []
[extra]
id = 1991
[taxonomies]
categories = []
tags = []
+++

{{task}}
{{omit from|BBC BASIC}}
It is often useful to control the memory layout of fields in a data structure to match an interface control definition, or to interface with hardware. Define a data structure matching the RS-232 Plug Definition. Use the 9-pin definition for brevity.
 Pin Settings for Plug
 (Reverse order for socket.)
 __________________________________________
 1  2  3  4  5  6  7  8  9  10 11 12 13
  14 15 16 17 18 19 20 21 22 23 24 25
 _________________
 1  2  3  4  5
 6  7  8  9
 25 pin                        9 pin
 1 - PG   Protective ground
 2 - TD   Transmitted data     3
 3 - RD   Received data        2
 4 - RTS  Request to send      7
 5 - CTS  Clear to send        8
 6 - DSR  Data set ready       6
 7 - SG   Signal ground        5
 8 - CD   Carrier detect       1
 9 - + voltage (testing)
 10 - - voltage (testing)
 11 -
 12 - SCD  Secondary CD
 13 - SCS  Secondary CTS
 14 - STD  Secondary TD
 15 - TC   Transmit clock
 16 - SRD  Secondary RD
 17 - RC   Receiver clock
 18 -
 19 - SRS  Secondary RTS            
 20 - DTR  Data terminal ready      4
 21 - SQD  Signal quality detector
 22 - RI   Ring indicator           9
 23 - DRS  Data rate select
 24 - XTC  External clock
 25 -


## Ada


```ada
type Bit is mod 2;
type Rs_232_Layout is record
   Carrier_Detect      : Bit;
   Received_Data       : Bit;
   Transmitted_Data    : Bit;
   Data_Terminal_ready : Bit;
   Signal_Ground       : Bit;
   Data_Set_Ready      : Bit;
   Request_To_Send     : Bit;
   Clear_To_Send       : Bit;
   Ring_Indicator      : Bit;
end record;

for Rs_232_Layout use record
   Carrier_Detect      at 0 range 0..0;
   Received_Data       at 0 range 1..1;
   Transmitted_Data    at 0 range 2..2;
   Data_Terminal_Ready at 0 range 3..3;
   Signal_Ground       at 0 range 4..4;
   Data_Set_Ready      at 0 range 5..5;
   Request_To_Send     at 0 range 6..6;
   Clear_To_Send       at 0 range 7..7;
   Ring_Indicator      at 0 range 8..8;
end record;
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{wont work with|ALGOL 68G|Any - tested with release [https://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.15-0.8b/algol68g-mk15-0.8b.fc9.i386.rpm/download algol68g-1.15-0.8b] - bug is order of BITS}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}

```algol68
MODE RSTWOTHREETWO = BITS;
INT ofs = bits width - 9;
INT 
   lwb rs232           = ofs + 1,
   carrier detect      = ofs + 1,
   received data       = ofs + 2,
   transmitted data    = ofs + 3,
   data terminal ready = ofs + 4,
   signal ground       = ofs + 5,
   data set ready      = ofs + 6,
   request to send     = ofs + 7,
   clear to send       = ofs + 8,
   ring indicator      = ofs + 9,
   upb rs232           = ofs + 9;
 
RSTWOTHREETWO rs232 bits := 2r10000000; # up to bits width, OR #
print(("received data: ",received data ELEM rs232bits, new line));

rs232 bits := bits pack((FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE));
print(("received data: ",received data ELEM rs232bits, new line))
```

Output:

```txt

received data: T
received data: T

```


=={{header|C}}/{{header|C++}}==
Note: The order of the fields is implementation-defined (i.e. the first bit might be the least-significant one or the most-significant one). On GCC and MSVC++, the first bit is the least-significant one.

```c
struct RS232_data
{
  unsigned carrier_detect        : 1;
  unsigned received_data         : 1;
  unsigned transmitted_data      : 1;
  unsigned data_terminal_ready   : 1;
  unsigned signal_ground         : 1;
  unsigned data_set_ready        : 1;
  unsigned request_to_send       : 1;
  unsigned clear_to_send         : 1;
  unsigned ring_indicator        : 1;
};
```

The ":1" gives the number of allocated bits. For unused bits (e.g. pin 11 in the 25-pin version above) the field name can be omitted.

Since as stated before the order of bits can't be assured '''but''' it could be important if we need to interact with hardware, the best way is to define ''bit masks''; of course actual writing/reading to/from an hardware "register" greater than a single byte must be done taking care of endianness.


## D


### Tango version

Implementation uses tango's BitArray structure.
{{libheader|tango}}

```D
module controlFieldsInStruct;

import tango.core.BitArray;
import tango.io.Stdout;
import tango.text.convert.Integer;

class RS232Wrapper(int Length = 9)
{
    static assert(Length == 9 || Length == 25, "ERROR, wrong type");
    BitArray ba; 
    static uint[char[]] _map;
 
    public:

    static if (Length == 9) {
        static this() {
            _map = [ cast(char[])
                "CD"  : 1, "RD"  : 2, "TD"  : 3, "DTR" : 4, "SG"  : 5,
                "DSR" : 6, "RTS" : 7, "CTS" : 8, "RI"  : 9 
            ];  
        }   
    } else {
        static this() {
            _map = [ cast(char[])
                "PG"  : 1u, "TD"  :  2, "RD"  :  3, "RTS" :  4, "CTS" :  5,  
                "DSR" :  6, "SG"  :  7, "CD"  :  8, "+"   :  9, "-"   : 10, 
                "SCD" : 12, "SCS" : 13, "STD" : 14, "TC"  : 15, "SRD" : 16, 
                "RC"  : 17, "SRS" : 19, "DTR" : 20, "SQD" : 21, "RI"  : 22, 
                "DRS" : 23, "XTC" : 24
            ];  
        }   
    }   


    this() {
        ba.length = Length;
    }   

    bool opIndex(uint pos) { return ba[pos]; }
    bool opIndexAssign(bool b, uint pos) { return (ba[pos] = b); }
    bool opIndex(char[] name) {
        assert (name in _map, "don't know that plug: " ~ name);
        return opIndex(_map[name]);
    }   
    bool opIndexAssign(bool b, char[] name) {
        assert (name in _map, "don't know that plug: " ~ name);
        return opIndexAssign(b, _map[name]);
    }   
    void opSliceAssign(bool b) { foreach (ref r; ba) r = b; }
    char[] toString() {
        char[] ret = "[";
        foreach (name, value; _map)
            ret ~= name ~ ":" ~ (ba[value]?"1":"0") ~", ";
        ret ~= "]";
        return ret;
    }   
}

int main(char[][] args)
{
    auto ba = new RS232Wrapper!(25);

    // set all bits
    ba[] = 1;
    ba["RD"] = 0;
    ba[5] = 0;

    Stdout (ba).newline;

    return 0;
}
```


Output:
<pre style="overflow:scroll">
[RD:0, RI:1, DSR:1, SG:1, DTR:1, TC:1, TD:1, CD:1, SQD:1, +:1, -:1, SRD:1, RTS:1, SRS:1, STD:1, PG:1, SCD:1, CTS:0, DRS:1, SCS:1, XTC:1, RC:1 ]

```


### Phobos version

Not tested.

```d
import std.bitmanip;

struct RS232_data {
    static if (std.system.endian == std.system.Endian.bigEndian) {
        mixin(bitfields!(bool, "carrier_detect",      1,
                         bool, "received_data",       1,
                         bool, "transmitted_data",    1,
                         bool, "data_terminal_ready", 1,
                         bool, "signal_ground",       1,
                         bool, "data_set_ready",      1,
                         bool, "request_to_send",     1,
                         bool, "clear_to_send",       1,
                         bool, "ring_indicator",      1,
                         bool, "",                    7));
    } else {
        mixin(bitfields!(bool, "",                    7,
                         bool, "ring_indicator",      1,
                         bool, "clear_to_send",       1,
                         bool, "request_to_send",     1,
                         bool, "data_set_ready",      1,
                         bool, "signal_ground",       1,
                         bool, "data_terminal_ready", 1,
                         bool, "transmitted_data",    1,
                         bool, "received_data",       1,
                         bool, "carrier_detect",      1));
    }

    static assert(RS232_data.sizeof == 2);
}

void main() {}
```



## Forth

Low level hardware control is a typical use of Forth. None of this is standard, however, since hardware I/O mechanisms differ on different systems. Forth does not have a structure mechanism, much less bitfields. These would be represented instead via bitmask constants if doing real serial port control.


```forth
 : masks ( n -- ) 0 do 1 i lshift constant loop ;
 
 9 masks DCD RxD TxD DTR SG DSR RTS CTS RI
```


Example usage, assuming I/O primitives '''in''' and '''out''':


```forth
 hex
 3fd constant com1-ctrl
 decimal
 
 : wait-ready
   begin
     com1-ctrl in
     CTS and
   until ;
 : wait-rx
   begin
     com1-ctrl in
     CTS and 0=
   until ;
 
 : send-byte ( b -- )   \ send assuming N81 (no parity, 8 bits data, 1 bit frame)
   255 and
   9 0 do
     RTS com1-ctrl out
     wait-ready
     dup 1 and if TxD else 0 then com1-ctrl out
     wait-rx
     2/
   loop drop ;
```

   
Of course, this is a very simplified view of the full RS-232 protocol. Also, although this represents the order of the pins in a D-9 connector, this would not necessarily be the same as the order of the bits in a control register.


## Fortran


### Modern

F90 introduced the ability to define compound data aggregates, as had been used from the start by COBOL in the 1960s. Thus, one could define 
```Fortran
       TYPE RS232PIN9
        LOGICAL CARRIER_DETECT		!1
        LOGICAL RECEIVED_DATA		!2
        LOGICAL TRANSMITTED_DATA	!3
        LOGICAL DATA_TERMINAL_READY	!4
        LOGICAL SIGNAL_GROUND		!5
        LOGICAL DATA_SET_READY		!6
        LOGICAL REQUEST_TO_SEND		!7
        LOGICAL CLEAR_TO_SEND		!8
        LOGICAL RING_INDICATOR		!9
       END TYPE RS232PIN9 
```

But it would be nearly pointless to do so.

Fortran's LOGICAL type is defined to occupy as much space as the default INTEGER type, which is typically thirty-two bits. This was done to simplify questions of storage size for large collections of assorted types of variables in a COMMON storage area. Further, ''true'' and ''false'' may not be signified by 1 and 0 but by -1 and 0, or, something else. Many compilers continue to support the old-style storage size indications that were introduced for binary computers and so would allow LOGICAL*1, but alas, this does not mean one bit, it means one byte. There is no equivalent of a key word PACKED (as in Pascal for example) whereby it might be indicated that a collection of items are to be stored "adjacent" and not aligned to byte or word boundaries. But, without a storage type equivalent to BIT(1) such as pl/i offers, this won't help either. Such packing can make reading or writing to a variable quite difficult - imagine a 32-bit integer variable offset by three bits - which is why some languages offer the keyword ALIGNED.

However, all is not quite lost. Given that one can access the special storage word containing the status of the nine-pin socket, presumably returning what can be regarded as a thirty-two bit integer (to accommodate a 25-pin socket), then, given certain knowledge of the ordering of the pins versus the bits of the integer (are bits counted from left-to-right or right-to-left, starting at one or at zero?), and certain knowledge that the nine bits are at the high-order end of the word or at the low-order end of the word (the IBM 1130's card reader placed the image of a column of a card, twelve bits, at the high-order end of its sixteen-bit word), it would be merely a matter of tedium to unpack those bits with suitable expressions and place the appropriate values into the components of a variable of type RS232PIN9. Such variables can then be juggled in the usual way, for instance using NAMELIST style I/O whereby the values are presented along with the names of the variables holding them. For output, this could be achieved with suitable FORMAT statements.

There may be available library functions IAND(i,j) and IOR(i,j) for dealing with integer variables, otherwise it becomes a matter of integer arithmetic. Unpacking or re-packing a data structure (say, as found in a disc file record) is straightforward but tedious, and, aside from the sizes and types of the components one must have definite knowledge of the endianness of the data as in the aggregate (read from disc) compared to the endianness of the cpu running your procedure. Easiest is of course to have the same style cpu for both.


### Older style

Without the ability to create data aggregates via TYPE statements, whereby a single variable might sprawl across memory as above, one instead prepared a collection of variables, usually with some systematic name convention linking the names of the parts. These variables would be anywhere in memory and so had no particular memory layout in themselves. However, when stored as a record in a disc file a data structure is created with a definite layout, and this is manifest in the READ and WRITE statements involved. Suppose the statement was <code>WRITE (F,REC = n) THIS,M,NAME</code> meaning that the n'th record of file unit F was to be written. The types of the variables would be known, and their sizes also. Say <code>REAL*8 THIS</code>, <code>INTEGER*1 M</code>, and <code>CHARACTER*28 NAME</code>. Such a record could be read (or written) by <code>READ (F,REC = n) STUFF</code> given a declaration <code>CHARACTER*37 STUFF</code> (counting on fingers as necessary) and the various parts of the data aggregate could be indexed within STUFF. However, the interpretation of the interior bytes of multi-byte items such as integer and floating-point variables is complicated by the endianness of the processor, a confounding nuisance.

It is further possible to declare that STUFF is to occupy the same storage as the named variables. If the declaration was <code>CHARACTER*1 STUFF(37)</code>, then <code>EQUIVALENCE (STUFF(1),THIS),(STUFF(9),M),(STUFF(10),NAME)</code> would mean that STUFF occupied the same storage as those variables, or rather, that the variables occupied the same storage as STUFF - indeed, they could overlay each other, which would be unlikely to be helpful. This could mean that a floating-point or integer variable was ''not'' aligned to a word boundary with the consequent penalty in access, for instance by having THIS start with STUFF(2) because M was put first. Some systems may not allow byte-based addressing, only word-based so complications can arise. But this demonstrates precise knowledge of the memory layout of a data structure. The more modern compilers that allow the TYPE declaration typically do not allow the appearance of such variables in EQUIVALENCE statements, to prevent access to the memory layout of such data structures. Others allow a new version of EQUIVALENCE (which the moderns deprecate) via the MAP statement, but this is not standard Fortran.

As before stated, there is no BIT facility, so packing is to byte boundaries. But, if one is determined to store thousands of records with minimal storage use, it may seem worth the effort to engage in the arithmetic to pack the likes of say three bits, followed by the thirty-two bits of a floating-point value, and so on, into a sequence of bytes which then would be written. In such a situation it may even be worth packing only a portion of the floating-point variable, if reduced precision is acceptable and one is certain of the usage of the bits within such a number. However, given the difficulty of access to the parts of such a packed aggregate, it is usually better to leave the byte/word packing and unpacking to the I/O system as via <code>WRITE (F,REC = n) THIS,M,NAME</code> and then manipulate the variables as their conveniently-aligned in-memory forms as ordinary variables, only repacking to the data structure form with a subsequent WRITE statement.

The INTEGER*''n'' opportunity is not fully flexible in that powers of two are usually the only options so that a value that might fit into INTEGER*3 will have to go into INTEGER*4. In any case this style is not helpful for decimal machines or binary computers whose word size is not a power of two. It is possible to break away from a byte base, especially when there are many variables with small ranges to represent. Suppose that V3 only has values 0, 1, 2; V5 has only 0, 1, 2, 3, 4; V4 only 0, 1, 2, 3; and V2 only 0, 1. Then a set of values could be encoded as a single decimal number: say 1230 for the four variables in that order, which would fit into a two byte integer instead of four one byte integers. That is merely changing base 256 to base 10, notionally, but a better packing is possible, Consider <code>V = V3 + 3*(V5 + 5*(V4 + 4*(V2)))</code> whose maximum value would be 2 + 3*(4 + 5*(3 + 4*1)) = 119, which will fit into one byte. If there were many such variables, then their packed values might require larger integers for even greater sharing. Variables with fractional values can be treated in a similar way, cautiously... 

With careful planning, such a compound value may even have helpful numerical properties, of service for (some) multi-key sorts. In the example, V2 is the high-order value so if a desired sort key happened to include V2, V4, V5, V3 then a four-variable comparison could be done in just one test. Unless the value overflows into the sign bit. This may not be a problem if the sorted order is merely to facilitate a binary search that will use the same ordering, but there can still be surprises. The B6700 used a 48-bit word and its compiler did not offer the then-unknown CHARACTER type. One might store six eight-bit characters in a word without worrying over the ordering that will result - the B6700 had no integer representation as such, using floating-point numbers with no fractional part, so the numerical values resulting from character codes would be quite odd. However, it also worked in base eight and although forty-eight is divisible by three, the requirement for the sign of the exponent and the sign of the value uses only two bits. Thus, one bit, the topmost, did not participate in arithmetic operations and as a result, arithmetic could not distinguish between characters in the high end of a word that differed in their highest bit. Even .EQ. would fail and the compiler offered a special substitute, .IS. to test for equality. Other languages that offered character manipulation (such as the extensions to Algol) used entirely different methods that avoided this problem. 

In a similar way, text content may employ only a limited character set so perhaps five bits per symbol would suffice, or some other packing scheme might suggest itself. There is also a whole world of compression algorithms. The end result is that a data structure manifesting as records in a disc file may be difficult to unpack into a convenient internal form even given a careful description of the layout.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' using bit fields
Type RS232_Pin9
  carrierDetect     : 1 As UByte 
  receivedData      : 1 As UByte 
  transmittedData   : 1 As UByte
  dataTerminalReady : 1 As UByte
  signalGround      : 1 As UByte
  dataSetReady      : 1 As UByte
  requestToSend     : 1 As UByte
  clearToSend       : 1 As UByte
  ringIndicator     : 1 As UByte
End Type

Print SizeOf(RS232_Pin9)  '' 2 bytes
Sleep
```



## Free Pascal

The FPC (Free Pascal compiler) carefully defines the internal memory structure of data type in its “Programmer’s guide”.

```pascal
program rs232(input, output, stdErr);
type
	{$packEnum 2}{$scopedEnums off}
	pin = (carrierDetect, receivedData, transmittedData, dataTerminalReady,
		signalGround, dataSetReady, requestToSend, clearToSend, ringIndicator);
	{$packSet 2}
	pins = set of pin;
var
	signal: pins;
	// for demonstration purposes, in order to reveal the memory layout
	signalMemoryStructure: word absolute signal;
	{$if sizeOf(signal) <> sizeOf(word)} // just as safe-guard
		{$fatal signal size}
	{$endIf}
begin
	signal := [];
	include(signal, signalGround); // equivalent to signal := signal + [signalGround];
	// for demonstration purposes: obviously we know this is always `true`
	if signalGround in signal then
	begin
		writeLn(binStr(signalMemoryStructure, bitSizeOf(signal)));
	end;
end.
```

{{Out}}

```txt
0000000000010000
```



## Go

Go does not have named bits as part of the type system.  Instead, constants are typically defined as shown.  For a word of bits with special meanings like this, a type would be defined though, as shown.  Static typing rules then control assignments and comparisons at the word level.  At the bit level, it helps to follow naming conventions so that, say, using a 9-pin constant on a 25-pin word would be an obvious error in the source code.

```go
package main

import "fmt"

type rs232p9 uint16

const (
	CD9  rs232p9 = 1 << iota // Carrier detect
	RD9                      // Received data
	TD9                      // Transmitted data
	DTR9                     // Data terminal ready
	SG9                      // signal ground
	DSR9                     // Data set ready
	RTS9                     // Request to send
	CTS9                     // Clear to send
	RI9                      // Ring indicator
)

func main() {
	// set some nonsense bits just for example
	p := RI9 | TD9 | CD9
	fmt.Printf("Type=%T value=%#04x\n", p, p)
}
```

{{out}}

```txt

Type=main.rs232p9 value=0x0105

```



## J

J does not support "structures", nor "fields in a structure".  Instead, J supports arrays.  And, of course, J could have labels corresponding to the elements of an array representing the state (voltage, current, logical bit value, whatever) of each pin of a 9-pin RS-232 plug:

```j
labels=: <;._2]0 :0
CD   Carrier detect
RD   Received data
TD   Transmitted data
DTR  Data terminal ready
SG   Signal ground
DSR  Data set ready
RTS  Request to send
CTS  Clear to send
RI   Ring indicator
)
```



## Julia

If the nine-pin version of the serial port is as specified above:

```txt
1  2  3  4  5
6  7  8  9
                        9 pin
PG   Protective ground
TD   Transmitted data     3
RD   Received data        2
RTS  Request to send      7
CTS  Clear to send        8
DSR  Data set ready       6
SG   Signal ground        5
CD   Carrier detect       1
+ voltage (testing)

```

We can then make the following code for a new serial port type:

```Julia

mutable struct NinePinSerialPort
    pins::BitArray
    function NinePinSerialPort()
        this = new()
        this.pins = BitArray(9)
    end
end

const CD = 1
const RD = 2
const TD = 3
const SG = 5
const DSR = 6
const RTS = 7
const CTS = 8

# Here we test the type's code.
port = NinePinSerialPort()
println("Port is now at defaults, which are $port")
port[CTS] = true
println("CD pin of port, which is pin $CD, is now $(port[CD])")
println("CTS pin of port, which is pin $CTS, is now $(port[CTS])")
println("port is now: $port")

```

{{output}}

```txt

Port is now at defaults, which are Bool[false, false, false, false, false, false, false, false, false]
CD pin of port, which is pin 1, is now false
CTS pin of port, which is pin 8, is now true
port is now: Bool[false, false, false, false, false, false, false, true, false]

```



## Kotlin

If it were only desired to access pin settings by position (albeit starting from pin 0 rather than pin 1), then a java.util.BitSet could be used to model this task. 

However, if access by both position and name is required, then a data class with 9 named boolean properties would be more suitable. This automatically generates functions called component1, component2 etc. to get the pin values by pin number. However, a function needs to be written manually to set pin values by pin number:

```scala
// version 1.0.6

const val OFF = false
const val ON  = true

fun toOnOff(b: Boolean) = if (b) "ON" else "OFF"

data class Rs232Pins9(
    var carrierDetect     : Boolean = OFF,
    var receivedData      : Boolean = OFF,
    var transmittedData   : Boolean = OFF,
    var dataTerminalReady : Boolean = OFF,
    var signalGround      : Boolean = OFF,
    var dataSetReady      : Boolean = OFF,
    var requestToSend     : Boolean = OFF,
    var clearToSend       : Boolean = OFF,
    var ringIndicator     : Boolean = OFF
) {
    fun setPin(n: Int, v: Boolean) {
        when (n) {
            1 -> carrierDetect     = v
            2 -> receivedData      = v
            3 -> transmittedData   = v
            4 -> dataTerminalReady = v
            5 -> signalGround      = v  
            6 -> dataSetReady      = v
            7 -> requestToSend     = v
            8 -> clearToSend       = v
            9 -> ringIndicator     = v   
        }
    }
}

fun main(args: Array<String>) {
    val plug = Rs232Pins9(carrierDetect = ON, receivedData = ON) // set first two pins, say
    println(toOnOff(plug.component2()))                          // print value of pin 2 by number
    plug.transmittedData = ON                                    // set pin 3 by name
    plug.setPin(4, ON)                                           // set pin 4 by number
    println(toOnOff(plug.component3()))                          // print value of pin 3 by number
    println(toOnOff(plug.dataTerminalReady))                     // print value of pin 4 by name 
    println(toOnOff(plug.ringIndicator))                         // print value of pin 9 by name
}
```


{{out}}

```txt

ON
ON
ON
OFF

```


=={{header|MATLAB}} / {{header|Octave}}==
Defining structs in MATLAB is kind of bulky, making a class definition might be cleaner for this purpose. If you need to enumerate each pin rather than set the state of the pin using the name of the pin, you can use struct2cell() on the rs232 struct, which will return a cell array whose entries are the value of each of the structs fields in the order in which they were defined.


```MATLAB>>
 rs232 = struct('carrier_detect', logical(1),...
'received_data' , logical(1), ...
'transmitted_data', logical(1),...
'data_terminal_ready', logical(1),...
'signal_ground', logical(1),...
'data_set_ready', logical(1),...
'request_to_send', logical(1),...
'clear_to_send', logical(1),...
'ring_indicator', logical(1))

rs232 = 

         carrier_detect: 1
          received_data: 1
       transmitted_data: 1
    data_terminal_ready: 1
          signal_ground: 1
         data_set_ready: 1
        request_to_send: 1
          clear_to_send: 1
         ring_indicator: 1

>> struct2cell(rs232)

ans = 

    [1]
    [1]
    [1]
    [1]
    [1]
    [1]
    [1]
    [1]
    [1]
```



## Mercury

Mercury does not have data types that mark down to individual bits.  Instead it allows you to use symbolic names (as per the <tt>rs232_pin</tt> type definition below with its constructors like <tt>carrier_detect</tt>) and to easily map those names to a value if you so choose (as per the <tt>to_index/1</tt> function below).

This is, admittedly, more verbose than the equivalent code would be in, say, C, but it permits things which are more difficult to pull off in such languages.  First, the code here is perfectly type safe (despite the use of <tt>'''unsafe'''_set</tt> and <tt>'''unsafe'''_clear</tt>).  It is literally impossible to set or clear bits outside of the bounds of the bit array used in the physical representation.  It is as easy to set and clear individual bits in the bit array as it is to use bit notation in C, and it's easier to do than when using the safer "integer variable with bitwise operators" technique.  Setting and clearing groups of bits is even easier via the <tt>rs232_set_bits/2</tt> function: merely pass in a list of symbolic names.  Changing the bitwise representation is a matter of changing the bit numbers in <tt>to_index/1</tt> and possibly changing the size of the bit array in <tt>rs232_bits/1</tt>.  Indeed the entire underlying representation and implementation can change without the interface changing at all.

Aiding in changing the underlying representation at will is the fact that the exposed type&mdash;<tt>rs232</tt>&mdash; is an opaque data type.  Instead of exposing the fact of the <tt>bitmap</tt> implementation to the outside world, it is carefully concealed by the implementation.  It is impossible for any code using this module to operate on the underlying bitmap with anything other than the API which has been exposed.  This means that should it be deemed desirable to instead use an <tt>int</tt> as the underlying representation, this can be done without changing even one byte of client code.


###  rs232.m 


```Mercury

:- module rs232.

:- interface.

:- import_module bool, io, list, string.

:- type rs232_pin
   ---> carrier_detect
      ; received_data
      ; transmitted_data
      ; data_terminal_ready
      ; signal_ground
      ; data_set_ready
      ; request_to_send
      ; clear_to_send
      ; ring_indicator.

:- type rs232.

:- func rs232_bits       = rs232.
:- func rs232_bits(bool) = rs232.

:- func rs232_set(rs232, rs232_pin)   = rs232.
:- func rs232_clear(rs232, rs232_pin) = rs232.

:- pred rs232_is_set(rs232::in, rs232_pin::in) is semidet.
:- pred rs232_is_clear(rs232::in, rs232_pin::in) is semidet.

:- func rs232_set_bits(rs232, list(rs232_pin))   = rs232.
:- func rs232_clear_bits(rs232, list(rs232_pin)) = rs232.

:- func to_string(rs232) = string.

:- pred write_rs232(rs232::in, io::di, io::uo) is det.

:- implementation.

:- import_module bitmap.

:- type rs232 == bitmap.

rs232_bits          = rs232_bits(no).
rs232_bits(Default) = bitmap.init(9, Default).

rs232_set(A, Pin)   = unsafe_set(A, to_index(Pin)).
rs232_clear(A, Pin) = unsafe_clear(A, to_index(Pin)).

rs232_is_set(A, Pin)   :- unsafe_is_set(A, to_index(Pin)).
rs232_is_clear(A, Pin) :- unsafe_is_clear(A, to_index(Pin)).

rs232_set_bits(A, Pins)   = foldl((func(Pin, B) = rs232_set(B, Pin)), Pins, A).
rs232_clear_bits(A, Pins) = foldl((func(Pin, B) = rs232_clear(B, Pin)), Pins, A).

to_string(A) = bitmap.to_string(A).

write_rs232(A, !IO) :- write_bitmap(resize(A, 16, no), !IO).
                       % cannot write a bitmap that isn't byte-divisible

:- func to_index(rs232_pin) = bit_index.
to_index(carrier_detect)      = 0.
to_index(received_data)       = 1.
to_index(transmitted_data)    = 2.
to_index(data_terminal_ready) = 3.
to_index(signal_ground)       = 4.
to_index(data_set_ready)      = 5.
to_index(request_to_send)     = 6.
to_index(clear_to_send)       = 7.
to_index(ring_indicator)      = 8.

:- end_module rs232.

```


=== rs232_main.m ===

```Mercury
:- module rs232_main.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bitmap, bool, list, rs232.

main(!IO) :-
    Com1 = rs232_set_bits(rs232_bits, [data_terminal_ready, data_set_ready]),
    Com2 = rs232_clear_bits(rs232_bits(yes), [data_terminal_ready, data_set_ready]),

    write_string("Com1 bits = ", !IO),
    write_string(to_string(Com1), !IO), nl(!IO),

    write_string("Com2 bits = ", !IO),
    write_string(to_string(Com2), !IO), nl(!IO),

    write_string("Com1 DTR is ", !IO),
    ( if rs232_is_set(Com1, data_terminal_ready) then
        write_string("set.", !IO), nl(!IO)
    else
        write_string("clear.", !IO), nl(!IO)
    ),

    write_string("Com2 DSR is ", !IO),
    ( if rs232_is_clear(Com2, data_set_ready) then
        write_string("clear.", !IO), nl(!IO)
    else
        write_string("set.", !IO), nl(!IO)
    ).

:- end_module rs232_main.

```



### = Usage and output =

  $ '''mmc --make rs232_main'''
  $ '''./rs232_main'''
  ''Com1 bits = <9:1400>''
  ''Com2 bits = <9:EB80>''
  ''Com1 DTR is set.''
  ''Com2 DSR is clear.''


## Nim


```nim
type
  rs232Data = enum
    carrierDetect,
    receivedData,
    transmittedData,
    dataTerminalReady,
    signalGround,
    dataSetReady,
    requestToSend,
    clearToSend,
    ringIndicator

# Bit vector of 9 bits
var bv = {carrierDetect, signalGround, ringIndicator}
echo cast[uint16](bv) # Conversion of bitvector to 2 bytes for writing

let readValue: uint16 = 123
bv = cast[set[rs232Data]](readValue) # Conversion of a read value to bitvector
echo bv
```

Output:

```txt
273
{carrierDetect, receivedData, dataTerminalReady, signalGround, dataSetReady, requestToSend}
```



## OCaml

'''Library:''' [http://code.google.com/p/ocaml-extlib/ extlib]

```ocaml
open ExtLib
class rs232_data = object
  val d = BitSet.create 9

  method carrier_detect      = BitSet.is_set d 0
  method received_data       = BitSet.is_set d 1
  method transmitted_data    = BitSet.is_set d 2
  method data_terminal_ready = BitSet.is_set d 3
  method signal_ground       = BitSet.is_set d 4
  method data_set_ready      = BitSet.is_set d 5
  method request_to_send     = BitSet.is_set d 6
  method clear_to_send       = BitSet.is_set d 7
  method ring_indicator      = BitSet.is_set d 8

  method set_carrier_detect      b = (if b then BitSet.set else BitSet.unset) d 0
  method set_received_data       b = (if b then BitSet.set else BitSet.unset) d 1
  method set_transmitted_data    b = (if b then BitSet.set else BitSet.unset) d 2
  method set_data_terminal_ready b = (if b then BitSet.set else BitSet.unset) d 3
  method set_signal_ground       b = (if b then BitSet.set else BitSet.unset) d 4
  method set_data_set_ready      b = (if b then BitSet.set else BitSet.unset) d 5
  method set_request_to_send     b = (if b then BitSet.set else BitSet.unset) d 6
  method set_clear_to_send       b = (if b then BitSet.set else BitSet.unset) d 7
  method set_ring_indicator      b = (if b then BitSet.set else BitSet.unset) d 8
end
;;
```



## Pascal

Pascal itself does not define, how data types have to be stored in memory.
It is up to the processor (i. e. compiler) to choose appropriate mechanism.
The [[#Free Pascal|FPC]] (Free Pascal compiler) does predictably define how it stores data.


## Perl


```perl
use Bit::Vector::Minimal qw();
my $vec = Bit::Vector::Minimal->new(size => 24);

my %rs232 = reverse (
     1 => 'PG   Protective ground',
     2 => 'TD   Transmitted data',
     3 => 'RD   Received data',
     4 => 'RTS  Request to send',
     5 => 'CTS  Clear to send',
     6 => 'DSR  Data set ready',
     7 => 'SG   Signal ground',
     8 => 'CD   Carrier detect',
     9 => '+ voltage (testing)',
    10 => '- voltage (testing)',
    12 => 'SCD  Secondary CD',
    13 => 'SCS  Secondary CTS',
    14 => 'STD  Secondary TD',
    15 => 'TC   Transmit clock',
    16 => 'SRD  Secondary RD',
    17 => 'RC   Receiver clock',
    19 => 'SRS  Secondary RTS',
    20 => 'DTR  Data terminal ready',
    21 => 'SQD  Signal quality detector',
    22 => 'RI   Ring indicator',
    23 => 'DRS  Data rate select',
    24 => 'XTC  External clock',
);

$vec->set($rs232{'RD   Received data'}, 1);
$vec->get($rs232{'TC   Transmit clock'});
```



## Perl 6

The following is specced to work, but implementation of shaped arrays is not quite complete.

```perl6
enum T_RS232 <
    carrier_detect
    received_data
    transmitted_data
    data_terminal_ready
    signal_ground
    data_set_ready
    request_to_send
    clear_to_send
    ring_indicator
>;

my bit @signal[T_RS232];

@signal[signal_ground] = 1;
```

In the absence of shaped arrays, you can do the usual bit-twiddling tricks on a native integer of sufficient size.  (Such an integer could presumably be mapped directly to a device register.)

```perl6
$signal +|= 1 +< signal_ground;
```

Using a native int is likelier to work on a big-endian machine in any case.  Another almost-there solution is the mapping of C representational types into Perl 6 for native interfaces, but it does not yet support bit fields.


## Phix

Phix does not support bit-fields directly. The nearest/sanest thing to do probably goes something like this (completely untested)

```Phix
constant CD=1, RD=2, TD=3, DTR=4, ...
atom addr = allocate(2)  -- or wherever
--read
sequence bits = int_to_bits(peek2u(addr),16)
integer dtr = bits[DTR]
--write
bits[DTR] = 1
poke2(addr,bits_to_int(bits))
```

Naturally, you would be well advised to sequester such grubby details away in a small and separate unit/source code file (eg RS232.e) with a domain specific public API that does not leak implementation details (eg keep those constants private). There are 1/2/4/8 byte variants of peek and poke, and int-to-bits can extract anything from 1 to 53 bits on a 32-bit runtime, or up to 64 on a 64-bit runtime.
Alternatively you could use bit-masks, or it may be possible to enhance builtins/cffi.e to manage bit-fields, then again the above C entry does not exactly inspire confidence.


## PicoLisp

PicoLisp can handle bit fields or bit structures only as bignums. They can be
manipulated with '[http://software-lab.de/doc/ref_.html#& &]',
'[http://software-lab.de/doc/ref_.html#| |]' and
'[http://software-lab.de/doc/refX.html#x| x|]',
or tested with '[http://software-lab.de/doc/refB.html#bit? bit?]'.

```PicoLisp
# Define bit constants
(for (N . Mask) '(CD RD TD DTR SG DSR RTS CTS RI)
   (def Mask (>> (- 1 N) 1)) )

# Test if Clear to send
(when (bit? CTS Data)
   ... )
```



## PL/I


```PL/I

declare 1 RS232_layout,
   2 Carrier_Detect       Bit(1),
   2 Received_Data        Bit(1),
   2 Transmitted_Data     Bit(1),
   2 Data_Terminal_ready  Bit(1),
   2 Signal_Ground        Bit(1),
   2 Data_Set_Ready       Bit(1),
   2 Request_To_Send      Bit(1),
   2 Clear_To_Send        Bit(1),
   2 Ring_Indicator       Bit(1);

```



## Python

The ctypes module allows for the creation of Structures that can map between the structures of C and python datatypes. Within Structures, [http://docs.python.org/library/ctypes.html#bit-fields-in-structures-and-unions bit fields] can be created.


```python
from ctypes import Structure, c_int

rs232_9pin  = "_0 CD RD TD DTR SG DSR RTS CTS RI".split()
rs232_25pin = ( "_0  PG  TD  RD  RTS CTS DSR SG  CD  pos neg"
                "_11 SCD SCS STD TC  SRD RC"
                "_18 SRS DTR SQD RI DRS XTC" ).split()

class RS232_9pin(Structure):
    _fields_ = [(__, c_int, 1) for __ in rs232_9pin]

	
class RS232_25pin(Structure):
    _fields_ = [(__, c_int, 1) for __ in rs232_25pin]
```



## Racket



```racket

#lang racket

(require ffi/unsafe)

(define (_autobitmask l)
  (_bitmask (append* (for/list ([x l] [i (in-naturals)]) `(,x = ,(expt 2 i))))))

(define _rs232 (_autobitmask '(CD RD TD DTR SG DSR RTS CTS RI )))

;; Usually it will get used when using foreign functions automatically, but
;; this demonstrates the conversions explicitly
(require (only-in '#%foreign ctype-scheme->c ctype-c->scheme))
((ctype-scheme->c _rs232) '(SG TD RI)) ; -> 276
((ctype-c->scheme _rs232) 276)         ; -> '(TD SG RI)

```



## REXX


### version 1


```rexx
/* REXX ***************************************************************
* Decode Memory structure of  RS-232 Plug Definition
* Not sure if I understood it completely :-) Open for corrections
* You never stop learning (as long as you live)
* 03.08.2012 Walter Pachl
**********************************************************************/
Call decode 'ABC'
Call decode 'XY'
Exit

decode:
  Parse Arg c
  cb=c2b(c)
  If length(cb)=24 Then Do
    Parse Var cb,
     /*    1 - PG  */ Protective ground      +1,
     /* 3  2 - TD  */ Transmitted_data       +1,
     /* 2  3 - RD  */ Received_data          +1,
     /* 7  4 - RTS */ Request_to_send        +1,
     /* 8  5 - CTS */ Clear_to_send          +1,
     /* 6  6 - DSR */ Data_set_ready         +1,
     /* 5  7 - SG  */ Signal_ground          +1,
     /* 1  8 - CD  */ Carrier_detect         +1,
     /*    9 - +   */ plus_voltage           +1,
     /*   10 - -   */ minus_voltage          +1,
     /*   11 -     */ .                      +1,
     /*   12 - SCD */ Secondary_CD           +1,
     /*   13 - SCS */ Secondary_CTS          +1,
     /*   14 - STD */ Secondary_TD           +1,
     /*   15 - TC  */ Transmit_clock         +1,
     /*   16 - SRD */ Secondary_RD           +1,
     /*   17 - RC  */ Receiver_clock         +1,
     /*   18 -     */ .                      +1,
     /*   19 - SRS */ Secondary_RTS          +1,
     /* 4 20 - DTR */ Data_terminal_ready    +1,
     /*   21 - SQD */ Signal_quality_detector+1,
     /* 9 22 - RI  */ Ring_indicator         +1,
     /*   23 - DRS */ Data_rate_select       +1,
     /*   24 - XTC */ External_clock         +1
    Say '24 bins:' cb
    Say ' 1 - PG  Protective ground       ='Protective ground
    Say ' 2 - TD  Transmitted data        ='Transmitted_data
    Say ' 3 - RD  Received data           ='Received_data
    Say ' 4 - RTS Request to send         ='Request_to_send
    Say ' 5 - CTS Clear to send           ='Clear_to_send
    Say ' 6 - DSR Data set ready          ='Data_set_ready
    Say ' 7 - SG  Signal ground           ='Signal_ground
    Say ' 8 - CD  Carrier detect          ='Carrier_detect
    Say ' 9 - +   plus voltage            ='plus_voltage
    Say '10 - -   minus voltage           ='minus_voltage
    Say ' '
    Say '12 - SCD Secondary CD            ='Secondary_CD
    Say '13 - SCS Secondary CTS           ='Secondary_CTS
    Say '14 - STD Secondary TD            ='Secondary_TD
    Say '15 - TC  Transmit clock          ='Transmit_clock
    Say '16 - SRD Secondary RD            ='Secondary_RD
    Say '17 - RC  Receiver clock          ='Receiver_clock
    Say ' '
    Say '19 - SRS Secondary RTS           ='Secondary_RTS
    Say '20 - DTR Data terminal ready     ='Data_terminal_ready
    Say '21 - SQD Signal quality detector ='Signal_quality_detector
    Say '22 - RI  Ring indicator          ='Ring_indicator
    Say '23 - DRS Data rate select        ='Data_rate_select
    Say '24 - XTC External hlock          ='External_clock
    End
  Else Do
    Parse Var cb,
    /* 1  8 - CD  */ Carrier_detect         +1,
    /* 2  3 - RD  */ Received_data          +1,
    /* 3  2 - TD  */ Transmitted_data       +1,
    /* 4 20 - DTR */ Data_terminal_ready    +1,
    /* 5  7 - SG  */ Signal_ground          +1,
    /* 6  6 - DSR */ Data_set_ready         +1,
    /* 7  4 - RTS */ Request_to_send        +1,
    /* 8  5 - CTS */ Clear_to_send          +1,
    /* 9 22 - RI  */ Ring_indicator         +1
    Say ' '
    Say '9-bin:' left(cb,9)
    Say ' 1 CD   Carrier detect      ='Carrier_detect
    Say ' 2 RD   Received data       ='Received_data
    Say ' 3 TD   Transmitted data    ='Transmitted_data
    Say ' 4 DTR  Data terminal ready ='Data_terminal_ready
    Say ' 5 SG   Signal ground       ='Signal_ground
    Say ' 6 DSR  Data set ready      ='Data_set_ready
    Say ' 7 RTS  Request to send     ='Request_to_send
    Say ' 8 CTS  Clear to send       ='Clear_to_send
    Say ' 9 RI   Ring indicator      ='Ring_indicator
    End
  Return
c2b: Procedure
/* REXX ***************************************************************
* c2b Convert a character string to a bit string
* 03.08.2012 Walter Pachl
**********************************************************************/
Parse Arg c
x=c2x(c)
res=''
Do While x<>''
  Parse Var x hb +1 x
  Select
    When hb='0' Then bs='0000'
    When hb='1' Then bs='0001'
    When hb='2' Then bs='0010'
    When hb='3' Then bs='0011'
    When hb='4' Then bs='0100'
    When hb='5' Then bs='0101'
    When hb='6' Then bs='0110'
    When hb='7' Then bs='0111'
    When hb='8' Then bs='1000'
    When hb='9' Then bs='1001'
    When hb='A' Then bs='1010'
    When hb='B' Then bs='1011'
    When hb='C' Then bs='1100'
    When hb='D' Then bs='1101'
    When hb='E' Then bs='1110'
    When hb='F' Then bs='1111'
    End
  res=res||bs
  End
Return res
```

Output EBCDIC:

```txt

24 bins: 110000011100001011000011
 1 - PG  Protective ground       =1
 2 - TD  Transmitted data        =1
 3 - RD  Received data           =0
 4 - RTS Request to send         =0
 5 - CTS Clear to send           =0
 6 - DSR Data set ready          =0
 7 - SG  Signal ground           =0
 8 - CD  Carrier detect          =1
 9 - +   plus voltage            =1
10 - -   minus voltage           =1

12 - SCD Secondary CD            =0
13 - SCS Secondary CTS           =0
14 - STD Secondary TD            =0
15 - TC  Transmit clock          =1
16 - SRD Secondary RD            =0
17 - RC  Receiver clock          =1

19 - SRS Secondary RTS           =0
20 - DTR Data terminal ready     =0
21 - SQD Signal quality detector =0
22 - RI  Ring indicator          =0
23 - DRS Data rate select        =1
24 - XTC External hlock          =1

9-bin: 111001111
 1 CD   Carrier detect      =1
 2 RD   Received data       =1
 3 TD   Transmitted data    =1
 4 DTR  Data terminal ready =0
 5 SG   Signal ground       =0
 6 DSR  Data set ready      =1
 7 RTS  Request to send     =1
 8 CTS  Clear to send       =1
 9 RI   Ring indicator      =1

```



### version 2

Checks could be added to verify the number of pins selected, and also verify if the data (pin readings) specified is valid. 

```rexx
/*REXX program  displays  which  pins  are active of a  9  or  24  pin  RS-232  plug.   */
call rs_232  24,  127                            /*the value for an RS-232  24 pin plug.*/
call rs_232  24, '020304x'                       /* "    "    "   "   "      "  "    "  */
call rs_232   9, '10100000b'                     /* "    "    "   "   "      9  "    "  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
rs_232: arg ,x;    parse arg pins,ox             /*X  is  uppercased  when using  ARG.  */
        @.      = '??? unassigned pin'           /*assign a default for all the pins.   */
        @.24.1  = 'PG  protective ground'
        @.24.2  = 'TD  transmitted data'            ;     @.9.3 = @.24.2
        @.24.3  = 'RD  received data'               ;     @.9.2 = @.24.3
        @.24.4  = 'RTS request to send'             ;     @.9.7 = @.24.4
        @.24.5  = 'CTS clear to send'               ;     @.9.8 = @.24.5
        @.24.6  = 'DSR data set ready'              ;     @.9.6 = @.24.6
        @.24.7  = 'SG  signal ground'               ;     @.9.5 = @.24.7
        @.24.8  = 'CD  carrier detect'              ;     @.9.1 = @.24.8
        @.24.9  = '+   positive voltage'
        @.24.10 = '-   negative voltage'
        @.24.12 = 'SCD secondary CD'
        @.24.13 = 'SCS secondary CTS'
        @.24.14 = 'STD secondary td'
        @.24.15 = 'TC  transmit clock'
        @.24.16 = 'SRD secondary RD'
        @.24.17 = 'RC  receiver clock'
        @.24.19 = 'SRS secondary RTS'
        @.24.20 = 'DTR data terminal ready'         ;     @.9.4 = @.24.20
        @.24.21 = 'SQD signal quality detector'
        @.24.22 = 'RI  ring indicator'              ;     @.9.9 = @.24.22
        @.24.23 = 'DRS data rate select'
        @.24.24 = 'XTC external clock'

                 select
                 when right(x, 1)=='B'  then bits=    strip(x, 'T', "B")
                 when right(x, 1)=='X'  then bits=x2b(strip(x, 'T', "X"))
                 otherwise                   bits=x2b( d2x(x) )
                 end   /*select*/
        say
        bits=right(bits, pins, 0)                /*right justify pin readings (values). */
        say '───────── For a'    pins    "pin RS─232 plug, with a reading of: "   ox
        say
                 do j=1  for pins;     z=substr(bits, j, 1);         if z==0  then iterate
                 say right(j, 5)     'pin is "on": '     @.pins.j
                 end   /*j*/
        return
```

'''output'''   when using the default (internal) inputs:

```txt

───────── For a 24 pin RS─232 plug, with a reading of:  127

   18 pin is "on":  ??? unassigned pin
   19 pin is "on":  SRS secondary RTS
   20 pin is "on":  DTR data terminal ready
   21 pin is "on":  SQD signal quality detector
   22 pin is "on":  RI  ring indicator
   23 pin is "on":  DRS data rate select
   24 pin is "on":  XTC external clock

───────── For a 24 pin RS─232 plug, with a reading of:  020304x

    7 pin is "on":  SG  signal ground
   15 pin is "on":  TC  transmit clock
   16 pin is "on":  SRD secondary RD
   22 pin is "on":  RI  ring indicator

───────── For a 9 pin RS─232 plug, with a reading of:  10100000b

    2 pin is "on":  RD  received data
    4 pin is "on":  DTR data terminal ready

```



## Ruby

Uses the [http://redshift.sourceforge.net/bit-struct/ BitStruct] module, which is handy but awkward to instantiate objects.

```ruby
require 'bit-struct'

class RS232_9 < BitStruct
  unsigned :cd,  1, "Carrier detect"       #1
  unsigned :rd,  1, "Received data"        #2
  unsigned :td,  1, "Transmitted data"     #3
  unsigned :dtr, 1, "Data terminal ready"  #4
  unsigned :sg,  1, "Signal ground"        #5
  unsigned :dsr, 1, "Data set ready"       #6
  unsigned :rts, 1, "Request to send"      #7
  unsigned :cts, 1, "Clear to send"        #8
  unsigned :ri,  1, "Ring indicator"       #9
  
  def self.new_with_int(value)
    data = {}
    fields.each_with_index {|f, i| data[f.name] = value[i]}
    new(data)
  end
end

num = rand(2**9 - 1)
puts "num = #{num}"

sample1 = RS232_9.new([("%09d" % num.to_s(2)).reverse].pack("B*"))
puts sample1.inspect_detailed

sample2 = RS232_9.new_with_int(num)
puts sample2.inspect_detailed

puts "CD is #{sample2.cd == 1 ? 'on' : 'off'}"
```



```txt
num = 37
RS232_9:
                Carrier detect = 1
                 Received data = 0
              Transmitted data = 1
           Data terminal ready = 0
                 Signal ground = 0
                Data set ready = 1
               Request to send = 0
                 Clear to send = 0
                Ring indicator = 0
RS232_9:
                Carrier detect = 1
                 Received data = 0
              Transmitted data = 1
           Data terminal ready = 0
                 Signal ground = 0
                Data set ready = 1
               Request to send = 0
                 Clear to send = 0
                Ring indicator = 0
CD is on
```



## Scala


```Scala
object Rs232Pins9 extends App {

  val (off: Boolean, on: Boolean) = (false, true)
  val plug = new Rs232Pins9(carrierDetect = on, receivedData = on) // set first two pins, say

  def toOnOff(b: Boolean) = if (b) "on" else "off"

  class Rs232Pins9(
                    var carrierDetect: Boolean = off,
                    var receivedData: Boolean = off,
                    var transmittedData: Boolean = off,
                    var dataTerminalReady: Boolean = off,
                    var signalGround: Boolean = off,
                    var dataSetReady: Boolean = off,
                    var requestToSend: Boolean = off,
                    var clearToSend: Boolean = off,
                    var ringIndicator: Boolean = off
                  ) {
    def setPin(n: Int, v: Boolean) {
      (n) match {
        case 1 => carrierDetect = v
        case 2 => receivedData = v
        case 3 => transmittedData = v
        case 4 => dataTerminalReady = v
        case 5 => signalGround = v
        case 6 => dataSetReady = v
        case 7 => requestToSend = v
        case 8 => clearToSend = v
        case 9 => ringIndicator = v
      }
    }
  }

  // println(toOnOff(plug.component2()))                          // print value of pin 2 by number
  plug.transmittedData = on // set pin 3 by name
  plug.setPin(4, on) // set pin 4 by number
  // println(toOnOff(plug.component3()))                          // print value of pin 3 by number
  println(toOnOff(plug.dataTerminalReady)) // print value of pin 4 by name
  println(toOnOff(plug.ringIndicator)) // print value of pin 9 by name
}
```



## Tcl

This Tcl implementation represents the fields as bits in an integer. It provides two functions to get from symbolic pin names to the integer, and vice versa.

```tcl
set rs232_bits {CD RD TD DTR SG DSR RTS CTS RI}

proc rs232_encode args {
    set res 0
    foreach arg $args {
        set pos [lsearch $::rs232_bits $arg]
        if {$pos >=0} {set res [expr {$res | 1<<$pos}]}
    }
    return $res
}
proc rs232_decode int {
    set res {}
    set i -1
    foreach bit $::rs232_bits {
        incr i
        if {$int & 1<<$i} {lappend res $bit}
    }
    return $res
}
#------------------------------ Test suite
foreach {test => expected} {
    {rs232_encode CD} -> 1
    {rs232_decode 1} -> CD
    {rs232_encode CD RD TD} -> 7
    {rs232_decode 7} -> {CD RD TD}
} {
    catch $test res
    if {$res ne $expected} {puts "$test -> $res, expected $expected"}
}
```


{{omit from|ACL2}}
{{omit from|AWK}}
{{omit from|Batch File|No memory management or data structures.}}
{{omit from|Clojure}}
{{omit from|E}}
{{omit from|gnuplot}}
{{omit from|Groovy}}
{{omit from|GUISS}}
{{omit from|Java|Doesn't seem like the kind of thing Java can do}}
{{omit from|JavaScript}}
{{omit from|LaTeX}}
{{omit from|Logtalk}}
{{omit from|Make}}
{{omit from|PARI/GP}}
{{omit from|PlainTeX}}
{{omit from|PureBasic}} <!-- Does not have bit-sized data types. -->
{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} <!-- Does not have user-defined data structures. -->
{{omit from|XSLT}}
{{omit from|ZX Spectrum Basic|Does not support user defined data structures.}}
