+++
title = "Four bit adder/C++"
description = ""
date = 2011-10-03T19:59:32Z
aliases = []
[extra]
id = 10600
[taxonomies]
categories = []
tags = []
+++

Remarks before we start: 
*'...aim of this task is to simulate a four-bit adder chip.' According to this, the class design is laid out like this: starting with 'I/O pins' of our 'chip', we represent its 'inner circuit' by a 'logic block'. Logic blocks have a transfer function which creates signals for the output pins from the signals at the input pins (and the inner state of the block, if necessary). Logic blocks may be connected to form a more complex logic block; thereby we work our way up from the simple gates (AND, OR etc.) via more complex circuits (XOR, half adder, etc.) to the four bit adder. In order to avoid the tedious work of encoding numbers to bits and vice versa, we added conveniency blocks: a source (logic block without input pins) which sets its output pins according to a given decimal number, and a sink (logic block without output pins) which calculates a decimal number from the state of its input pins.
*'... gates ... can be imitated by using the bitwise operators...' I decided to use a boolean for a bit - firstly because the largest 'numbers' in this example are 5 bits wide and secondly because this is should become rather a prove of concept than highly efficient code.
*As C++ provides nice operator overloading and sophisticated templates, we'll make heavy use of these features - just for fun and elegance of syntax!

So, then...

A Pin just stores just its signal value; declared as follows (Pin.h)...

```cpp
#if !defined __PIN_H__
#define __PIN_H__

namespace rosetta
  {
  namespace fourBitAdder
    {
    namespace detail
      {

      class Pin
        {
        protected:
          Pin();
          Pin& Signal(bool value);
          bool Signal()const;
        private:
          bool signal;
        };

      }   //namespace detail
    }     //namespace fourBitAdder
  }       //namespace rosetta

#endif //!defined __PIN_H__
```

... and defined like that (Pin.cpp):

```cpp
#include "Pin.h"
using rosetta::fourBitAdder::detail::Pin;

Pin::Pin()
  :signal(false)
  {}

Pin& Pin::Signal(bool value)
  {
  signal = value;
  return *this;
  }
          
bool Pin::Signal()const
  {
  return signal;
  }
```

An input pin is a Pin which a value may be assigned to and can be got from; also, the input pin may be 
connected to a signal source which then determines the input pin's value. Obviously, in our simple world, 
such a signal source needs to be an output pin (PinIn.h):

```cpp
#if !defined __PININ_H__
#define __PININ_H__

#include "Pin.h"

namespace rosetta
  {
  namespace fourBitAdder
    {
    class PinOut;

    class PinIn
      :public detail::Pin
      {
      friend void operator>>(const PinOut&, PinIn&);

      public:
        PinIn();
        PinIn& operator=(bool signal);
        operator bool()const;
        void Disconnect();
      private:
        void Connect(const PinOut& output);
        const PinOut* signalSource;
      };

    } //namespace fourBitAdder
  }   //namespace rosetta

#endif //!defined __PININ_H__
```

The input pin's definition (PinIn.cpp):

```cpp>#include <memory

#include <cassert>

#include "PinIn.h"
#include "PinOut.h"
using rosetta::fourBitAdder::PinIn;
using rosetta::fourBitAdder::PinOut;

PinIn::PinIn()
  :detail::Pin()
  ,signalSource(NULL)
  {}

PinIn& PinIn::operator=(bool signal)
  {
  assert(signalSource == NULL); //otherwise, this will not have any effect
  return static_cast<PinIn&>(Signal(signal));
  }

PinIn::operator bool()const
  {
  if(signalSource == NULL)
    return Signal();
  else
    return *signalSource;   
  }

void PinIn::Disconnect()
  {
  signalSource = NULL;
  }

void PinIn::Connect(const PinOut& output)
  {
  signalSource = &output;
  }
```

An output pin is a Pin which a value may be assigned to and can be got from; also, the output pin may be 
get its signal from the transfer function of a (logic) block. For this to work out properly, it is necessary
that an output pin knows to which block ('chip') it belongs to and what position it has there. Being
the bridge between a chip's inner circuits (the logic block) and the outside world, an output pin
may be connected to an input pin (PinOut.h):

```cpp
#if !defined __PINOUT_H__
#define __PINOUT_H__

#include "Pin.h"

namespace rosetta
  {
  namespace fourBitAdder
    {
    namespace detail
      {
      class Block;
      template<int, int> class LogicBlock;
      }
    class PinIn;

    class PinOut
      :public detail::Pin
      {
      friend void operator>>(const PinOut&, PinIn&);
      template<int, int> friend class detail::LogicBlock;

      public:
        PinOut();
        operator bool()const;
        PinOut& operator=(bool signal);
      private:
        PinOut(detail::Block& container, int indexOfPinInContainer);
        detail::Block*const container;
        int indexOfPinInContainer;
      };

      void operator>>(const PinOut& output, PinIn& input);

    } //namespace fourBitAdder
  }   //namespace rosetta

#endif //!defined __PINOUT_H__
```

The output pin's definition (PinOut.cpp):

```cpp
#include "cassert"

#include "BaseLogic.h"
using rosetta::fourBitAdder::detail::Block;
#include "PinOut.h"
using rosetta::fourBitAdder::PinOut;

PinOut::PinOut()
  :detail::Pin()
  ,container(NULL)
  {}

PinOut::PinOut(Block& container, int indexOfPinInContainer)
  :detail::Pin()
  ,container(&container)
  ,indexOfPinInContainer(indexOfPinInContainer)
  {}

PinOut::operator bool()const
  {
  if(container == NULL)
    return Signal();
  else
    return container->TransferFunction(indexOfPinInContainer);
  }

PinOut& PinOut::operator=(bool signal)
  {
  assert(container == NULL); //otherwise, this will not have any effect
  return static_cast<PinOut&>(Signal(signal));
  }

void rosetta::fourBitAdder::operator>>(const PinOut& output, PinIn& input)
  {
  input.Connect(output);
  }
```

Now, we define a block which has no more than a pure virtual transfer function. The block serves as
a base for a logic block which adds a certain number of input and output pins to the transfer function.
Note how the numbers of input and output pins come in as template parameters (BaseLogic.h):

```cpp
#if !defined __BASELOGIC_H__
#define __BASELOGIC_H__

#include<memory>
#include<stdexcept>
#include<vector>

#include "PinIn.h"
#include "PinOut.h"

namespace rosetta
  {
  namespace fourBitAdder
    {
    namespace detail
      {

      class Block
        {
        public:
          virtual bool TransferFunction(unsigned indexOfOutput)=0;
        };

      template<int NumIn, int NumOut>
      class LogicBlock
        :Block
        {
        public:
          PinIn& In(unsigned i = 0)const
            {
            if(i >= NumIn)
              throw std::out_of_range("Index of input pin is out of range!");
            return *in[i];
            }

          const PinOut& Out(unsigned i = 0)
          {
          if(i >= NumOut)
            throw std::out_of_range("Index of output pin is out of range!");
          return *out[i];
          }

        protected:
          LogicBlock()
            {
            for(int i =0; i < NumIn; i++)
              in.push_back(std::auto_ptr<PinIn>(new PinIn()));
            for(int i =0; i < NumOut; i++)
              out.push_back(std::auto_ptr<PinOut>(new PinOut(*this, i)));            
            }

          enum 
            {
            NumInputs = NumIn, 
            NumOutputs = NumOut
            };
        private:
          std::vector<std::auto_ptr<PinIn> > in;
          std::vector<std::auto_ptr<PinOut> > out;
        };

      }   //namespace detail
    }     //namespace fourBitAdder
  }       //namespace rosetta
#endif //!defined __BASELOGIC_H__
```

Defining a gate as a logic block with just one output, we declare such a base class. Then, definition of 
the basic AND, OR and NOT gates with an arbitrary number of input pins is as easy as writing down the 
appropriate transfer function.

```cpp
#if !defined __GATES_H__
#define __GATES_H__

#include"PinIn.h"
#include"BaseLogic.h"

namespace rosetta
  {
  namespace fourBitAdder
    {
    namespace detail
      {

      template<int NumIn>
      class Gate
        : public LogicBlock<NumIn, 1>
        {
        protected:
          Gate()
            :LogicBlock()
            {}
        };

      }   //namespace detail

    template<int NumIn>
    class AndGate
      : public detail::Gate<NumIn>
      {
      public:
        bool TransferFunction(unsigned indexOfOutput)
          {
          bool result = true;
          for(unsigned i = 0; i < NumIn; i++)
            result = result && In(i);
          return result;
          }
      };

    template <int NumIn>
    class OrGate
      : public detail::Gate<NumIn>
      {
      public:
        bool TransferFunction(unsigned indexOfOutput)
          {
          bool result = false;
          for(unsigned i = 0; i < NumIn; i++)
            result = result || In(i);
          return result;
          }
      };

    class NotGate
      : public detail::Gate<1>
      {
      public:
        bool TransferFunction(unsigned indexOfOutput)
          {
          return !In(0);
          }
      };

    }     //namespace fourBitAdder
  }       //namespace rosetta

#endif //!defined __GATES_H__
```

Now we are ready for our first circuit built from several gates: the XOR gate, following the schematic
given with this task, consists of two AND, two NOT and one OR gates (XorGate.h).

```cpp
#if !defined __XORGATE_H__
#define __XORGATE_H__

#include "Gates.h"

namespace rosetta
  {
  namespace fourBitAdder
    {

    class XorGate
      : public detail::Gate<2>
      {
      public:
        XorGate();
        bool TransferFunction(unsigned indexOfOutput);

      private:
        AndGate<2> andA, andB;
        NotGate notA, notB;
        OrGate<2> or;
      };

    }     //namespace fourBitAdder
  }       //namespace rosetta

#endif //!defined __XORGATE_H__
```

The XOR gate's definition needs to set up the internal connections of the constituting gates and to
write down the transfer function which infers the XOR's output signal from its input signals.

```cpp
#include "XorGate.h"
using namespace rosetta::fourBitAdder;

XorGate::XorGate()
  :Gate()
  {
  notA.Out()>>andB.In(1);
  notB.Out()>>andA.In(1);
  andA.Out()>>or.In(0);
  andB.Out()>>or.In(1);
  }

bool XorGate::TransferFunction(unsigned indexOfOutput)
  {
  andA.In(0) = In(0);
  notA.In(0) = In(0);
  andB.In(0) = In(1);
  notB.In(0) = In(1);
          
  return or.Out();
  }
```

A half adder has two inputs (bits A and B) and two outputs (sum and carry); it consists of an AND and a XOR gate (HalfAdder.h):

```cpp
#if !defined __HALFADDER_H__
#define __HALFADDER_H__

#include "XorGate.h"
#include "Gates.h"
#include "BaseLogic.h"

namespace rosetta
  {
  namespace fourBitAdder
    {
    class PinOut;
    class PinIn;
    template<int> class AndGate;

    class HalfAdder
      : public detail::LogicBlock<2,2>
      {
      public:
        bool TransferFunction(unsigned indexOfOutput);

        PinIn& BitA();
        PinIn& BitB();
        const PinOut& Sum();
        const PinOut& Carry();

      private:
        AndGate<2> and;
        XorGate xor;
      };

    }     //namespace fourBitAdder
  }       //namespace rosetta

#endif //!defined __HALFADDER_H__
```

Internal wiring is not required for the half adder; we just write down the transfer function which
is able to provide the values for either of the two outputs (HalfAdder.cpp):

```cpp>#include <stdexcept

using std::out_of_range;

#include "HalfAdder.h"
using namespace rosetta::fourBitAdder;

bool HalfAdder::TransferFunction(unsigned indexOfOutput)
  {
  switch(indexOfOutput)
    {
    case 0: // sum
      xor.In(0) = BitA();
      xor.In(1) = BitB();
      return xor.Out();
    case 1: // carry
      and.In(0) = BitA();
      and.In(1) = BitB();
      return and.Out();
    default:
      throw new out_of_range("There are only two output pins.");
    }
  }

PinIn& HalfAdder::BitA() {return In(0);}

PinIn& HalfAdder::BitB() {return In(1);}

const PinOut& HalfAdder::Sum() {return Out(0);}

const PinOut& HalfAdder::Carry() {return Out(1);}

```

A full adder has three inputs (bits A and B as well as carry) and two outputs (sum and carry); it
consists of an OR gate and two half adders (FullAdder.h):

```cpp
#if !defined __FULLADDER_H__
#define __FULLADDER_H__

#include "BaseLogic.h"
#include "Gates.h"
#include "HalfAdder.h"


namespace rosetta
  {
  namespace fourBitAdder
    {
    class PinOut;
    class PinIn;

    class FullAdder
      : public detail::LogicBlock<3,2>
      {
      public:
        FullAdder();
        bool TransferFunction(unsigned indexOfOutput);

        PinIn& CarryInput();
        PinIn& BitA();
        PinIn& BitB();
        const PinOut& Sum();
        const PinOut& CarryResult();

      private:
        OrGate<2> or;
        HalfAdder bitAHalfAdder;
        HalfAdder bitBHalfAdder;
      };

    }     //namespace fourBitAdder
  }       //namespace rosetta

#endif //!defined __FULLADDER_H__
```

The full adder's definition consists of setting up the internal connections of writing down the 
proper transfer function (FullAdder.cpp):

```cpp>#include <stdexcept

using std::out_of_range;

#include "FullAdder.h"
using namespace rosetta::fourBitAdder;

FullAdder::FullAdder()
  {
  bitAHalfAdder.Sum()>>bitBHalfAdder.BitA();
  bitAHalfAdder.Carry()>>or.In(0);
  bitBHalfAdder.Carry()>>or.In(1);
  }

bool FullAdder::TransferFunction(unsigned indexOfOutput)
  {
  bitAHalfAdder.BitA() = CarryInput();
  bitAHalfAdder.BitB() = BitA();
  bitBHalfAdder.BitB() = BitB();

  switch(indexOfOutput)
    {
    case 0: //sum
      return bitBHalfAdder.Sum();     
    case 1: //carry bit
      return or.Out();                
    default:
      throw new out_of_range("There are only two output pins.");
    }
  }

const PinOut& FullAdder::Sum() {return Out(0);}

const PinOut& FullAdder::CarryResult() {return Out(1);}

PinIn& FullAdder::CarryInput(){return In(0);}

PinIn& FullAdder::BitA(){return In(1);}

PinIn& FullAdder::BitB(){return In(2);}
```

Finally, we arrive at the four bit adder, a circuit boasting 8 inputs (2 four bit numbers) and five
outputs (4 bit number plus carry, or just a five bit number) (FourBitAdder.h):

```cpp
#if !defined __FOURBITADDER_H__
#define __FOURBITADDER_H__

#include "BaseLogic.h"

#include <memory>
#include <vector>

#include "FullAdder.h"

namespace rosetta
  {
  namespace fourBitAdder
    {
    class PinOut;
    class PinIn;

    class FourBitAdder
      : public detail::LogicBlock<8,5>
      {
      public:
        FourBitAdder();
        bool TransferFunction(unsigned indexOfOutput);

        PinIn& BitA(unsigned index);
        PinIn& BitB(unsigned index);
        const PinOut& Sum(unsigned index);
        const PinOut& Carry();

      private:
        std::vector<std::auto_ptr<FullAdder> > fullAdder;
      };

    }     //namespace fourBitAdder
  }       //namespace rosetta

#endif //!defined __FOURBITADDER_H__
```

The four bit adder consists of four full adders; the internal connections are simple: the LSB full adder's
carry input is always false, and carry outputs are connected to carry inputs of the next full adder (the
MSB full adder's carry output is the four bit adders carry output). The other input and output pins of our
full adders are related to the four bit adders input and output by the transfer function (FourBitAdder.cpp):

```cpp>#include <stdexcept

using std::out_of_range;
#include <memory>
using std::auto_ptr;
#include <cassert>

#include "FourBitAdder.h"
using namespace rosetta::fourBitAdder;

FourBitAdder::FourBitAdder()
  {
  assert(NumInputs == 8);
  for(int i =0; i < NumInputs/2; i++)
    fullAdder.push_back(auto_ptr<FullAdder>(new FullAdder()));

  fullAdder[0]->CarryInput() = false;
  for(int i = 1; i < NumInputs/2; i++)
      fullAdder[i-1]->CarryResult()>>fullAdder[i]->CarryInput();
  }

bool FourBitAdder::TransferFunction(unsigned indexOfOutput)
  {
  int numBits = NumInputs/2;
  for(int i = 0; i < numBits; i++)
    {
    fullAdder[i]->BitA() = In(i);
    fullAdder[i]->BitB() = In(i+numBits);
    }

  assert(NumOutputs == 5);
  switch(indexOfOutput)
    {
    case 0:
    case 1:
    case 2:
    case 3:
      return fullAdder[indexOfOutput]->Sum();             //bits of resulting sum
    case 4:
      return fullAdder[indexOfOutput - 1]->CarryResult(); //carry bit
    default:
      assert(false);
      return false;
    }
  }

PinIn& FourBitAdder::BitA(unsigned index)
  {
  if(index < NumInputs/2)
    return In(index);
  else
    throw new out_of_range("Input A does not have  that much bits.");
  }

PinIn& FourBitAdder::BitB(unsigned index)
  {
  if(index < NumInputs/2)
    return In(index + NumInputs/2);
  else
    throw new out_of_range("Input A does not have  that much bits.");
  }

const PinOut& FourBitAdder::Sum(unsigned index)
  {
  if(index < (NumOutputs - 1))
    return Out(index);
  else
    throw new out_of_range("Sum output does not have that much bits.");
  }

const PinOut& FourBitAdder::Carry()
  {return Out(4);}

```

Note how the complexity of the code for the XOR, half adder, full adder and four bit adder remained
comparable and quite simple, despite the fact that the number of basic gates involved increased from
5 (XOR) to 6 (half adder) to 13 (full adder) and finally to 52 (four bit adder).

In order to avoid fiddling around with single bits, we provide two conveniency circuits. For input, there
is the source, a logic block providing an arbitrary number of outputs, but no inputs. Instead, it accepts
a simple decimal number as input and sets the signal at its outputs accordingly. A source may be connected
to (some of) the inputs of a logic block and thus allows easy control of the latter (Source.h):

```cpp
#if !defined __SOURCE_H__
#define __SOURCE_H__

#include <stdexcept>
#include <string>
#include <sstream>
#include <ostream>

#include "BaseLogic.h"

namespace rosetta
  {
  namespace fourBitAdder
    {

    template<int NumSources>
    class Source 
      : protected detail::LogicBlock<0, NumSources>
      {
      public:
        Source()
          :LogicBlock()
          ,maxSourceStateDecimal((1<<NumSources) - 1)
          ,sourceStateDecimal(0)
          {}

        Source& operator=(unsigned sourceStateDecimal)
          {
          if(sourceStateDecimal > maxSourceStateDecimal)
            this->sourceStateDecimal = maxSourceStateDecimal;
          else
            this->sourceStateDecimal = sourceStateDecimal;
          return *this;
          }

        operator unsigned()const
          {
          return sourceStateDecimal;
          }

        const PinOut& Bit(int i = 0)
          {
          return detail::LogicBlock<0, NumSources>::Out(i);
          }

        template<typename LogicBlock>
        LogicBlock& operator>>(LogicBlock& consumer)
          {
          for(unsigned i = 0; i < NumSources; i++)
            Out(i)>>consumer.In(i);
          return consumer;
          }

        operator std::string()
          {
          std::stringstream s;
          for(int i = (NumSources - 1); i >= 0; i--)
            s << Out(i);
          return s.str();
          }

      protected:
        bool TransferFunction(unsigned i)
          {
          return (sourceStateDecimal & 1<<i) == 1<<i;
          }

      private:
        const unsigned maxSourceStateDecimal;
        unsigned sourceStateDecimal;
      };

    template <int NumSources>
    std::ostream& operator<<(std::ostream& os, Source<NumSources>& src)
      {
        os<<(std::string)src;
        return os;
      }

    }     //namespace fourBitAdder
  }       //namespace rosetta

#endif //!defined __SOURCE_H__
```

In order to interpret output, there is the sink: a logic block providing an arbitrary number of inputs,
but no outputs. Instead, from its input signals the sink calculates a decimal number. A sink may be 
connected to (some of) the outputs of a logic block and thus allows easy access to that state of the latter (Sink.h):

```cpp
#if !defined __SINK_H__
#define __SINK_H__

#include <stdexcept>
#include <string>
#include <sstream>
#include <ostream>
#include <cassert>

#include "BaseLogic.h"

namespace rosetta
  {
  namespace fourBitAdder
    {

    template<int NumSinks>
    class Sink 
      : protected detail::LogicBlock<NumSinks, 0>
      {
      template<typename LogicBlock, int NumSinks>
      friend void operator>>(const LogicBlock&, Sink<NumSinks>&);

      public:
        Sink()
          :LogicBlock()
          {}

         operator unsigned()const
          {
          unsigned sinkStateDecimal = 0;
          for(unsigned i = 0; i < NumSinks;i++)
            if(In(i))
              sinkStateDecimal += 1<<i;
          return sinkStateDecimal;
          }

        PinIn& In(int i = 0)const
          {
          return detail::LogicBlock<NumSinks, 0>::In(i);
          }

        operator std::string()const
          {
          std::stringstream s;
          for(int i = (NumSinks - 1); i >= 0; i--)
            s << In(i);
          return s.str();
          }

      protected:
        bool TransferFunction(unsigned i)
          {
          assert(false);
          return false;
          }
      };

    template<typename LogicBlock, int NumSinks>
    void operator>>(LogicBlock& producer, Sink<NumSinks>& sink)
      {
      for(unsigned i = 0; i < NumSinks; i++)
        producer.Out(i)>>sink.In(i);
      }

    template <int NumSinks>
    std::ostream& operator<<(std::ostream& os, Sink<NumSinks>& src)
      {
        os<<(std::string)src;
        return os;
      }

    }     //namespace fourBitAdder
  }       //namespace rosetta

#endif //!defined __SINK_H__
```

Now, using the four bit adder with two sources of input and one sink for the result is quite simple:
We create the four necessary objects and connect them properly. With these preparations, 
making the four bit adder do its calculation is just a matter of setting the sources to the
desired numbers and reading their sum from the sink. There's main.cpp:

```cpp>#include <iostream

using std::cout;
using std::endl;

#include "FourBitAdder.h"
#include "Source.h"
#include "Sink.h"
using namespace rosetta::fourBitAdder;

int main(int argc, char** argv)
  {
  //create four bit adder, 'input numbers' and 'output display'
  Source<4> 
    fourBitAdderInputA, 
    fourBitAdderInputB;
  FourBitAdder fba;
  Sink<5> fourBitAdderResult;

  //connect inputs, four bit adder and output
  for(int i = 0; i < 4; i++)
    {
    fourBitAdderInputA.Bit(i)>>fba.BitA(i);
    fourBitAdderInputB.Bit(i)>>fba.BitB(i);
    }
  fba>>fourBitAdderResult;

  //do (and check) all possible additions of two four bit numbers and print the results
  for(unsigned a = 0; a < 16; a++)
    for(unsigned b = 0; b < 16; b++)
      {
      fourBitAdderInputA = a;
      fourBitAdderInputB = b;
      cout<<fourBitAdderInputA<<" + "<<fourBitAdderInputB<<" = "<<fourBitAdderResult<<endl;
      assert(fourBitAdderInputA + fourBitAdderInputB == fourBitAdderResult);
      }

  return 0;
  }
```

Output (source code is compiled by MS Visual C++ 10.0 (WinXP 32 bit) compiler):
<div style="height: 320px;overflow:scroll">

```txt

0000 + 0000 = 00000
0000 + 0001 = 00001
0000 + 0010 = 00010
0000 + 0011 = 00011
0000 + 0100 = 00100
0000 + 0101 = 00101
0000 + 0110 = 00110
0000 + 0111 = 00111
0000 + 1000 = 01000
0000 + 1001 = 01001
0000 + 1010 = 01010
0000 + 1011 = 01011
0000 + 1100 = 01100
0000 + 1101 = 01101
0000 + 1110 = 01110
0000 + 1111 = 01111
0001 + 0000 = 00001
0001 + 0001 = 00010
0001 + 0010 = 00011
0001 + 0011 = 00100
0001 + 0100 = 00101
0001 + 0101 = 00110
0001 + 0110 = 00111
0001 + 0111 = 01000
0001 + 1000 = 01001
0001 + 1001 = 01010
0001 + 1010 = 01011
0001 + 1011 = 01100
0001 + 1100 = 01101
0001 + 1101 = 01110
0001 + 1110 = 01111
0001 + 1111 = 10000
0010 + 0000 = 00010
0010 + 0001 = 00011
0010 + 0010 = 00100
0010 + 0011 = 00101
0010 + 0100 = 00110
0010 + 0101 = 00111
0010 + 0110 = 01000
0010 + 0111 = 01001
0010 + 1000 = 01010
0010 + 1001 = 01011
0010 + 1010 = 01100
0010 + 1011 = 01101
0010 + 1100 = 01110
0010 + 1101 = 01111
0010 + 1110 = 10000
0010 + 1111 = 10001
0011 + 0000 = 00011
0011 + 0001 = 00100
0011 + 0010 = 00101
0011 + 0011 = 00110
0011 + 0100 = 00111
0011 + 0101 = 01000
0011 + 0110 = 01001
0011 + 0111 = 01010
0011 + 1000 = 01011
0011 + 1001 = 01100
0011 + 1010 = 01101
0011 + 1011 = 01110
0011 + 1100 = 01111
0011 + 1101 = 10000
0011 + 1110 = 10001
0011 + 1111 = 10010
0100 + 0000 = 00100
0100 + 0001 = 00101
0100 + 0010 = 00110
0100 + 0011 = 00111
0100 + 0100 = 01000
0100 + 0101 = 01001
0100 + 0110 = 01010
0100 + 0111 = 01011
0100 + 1000 = 01100
0100 + 1001 = 01101
0100 + 1010 = 01110
0100 + 1011 = 01111
0100 + 1100 = 10000
0100 + 1101 = 10001
0100 + 1110 = 10010
0100 + 1111 = 10011
0101 + 0000 = 00101
0101 + 0001 = 00110
0101 + 0010 = 00111
0101 + 0011 = 01000
0101 + 0100 = 01001
0101 + 0101 = 01010
0101 + 0110 = 01011
0101 + 0111 = 01100
0101 + 1000 = 01101
0101 + 1001 = 01110
0101 + 1010 = 01111
0101 + 1011 = 10000
0101 + 1100 = 10001
0101 + 1101 = 10010
0101 + 1110 = 10011
0101 + 1111 = 10100
0110 + 0000 = 00110
0110 + 0001 = 00111
0110 + 0010 = 01000
0110 + 0011 = 01001
0110 + 0100 = 01010
0110 + 0101 = 01011
0110 + 0110 = 01100
0110 + 0111 = 01101
0110 + 1000 = 01110
0110 + 1001 = 01111
0110 + 1010 = 10000
0110 + 1011 = 10001
0110 + 1100 = 10010
0110 + 1101 = 10011
0110 + 1110 = 10100
0110 + 1111 = 10101
0111 + 0000 = 00111
0111 + 0001 = 01000
0111 + 0010 = 01001
0111 + 0011 = 01010
0111 + 0100 = 01011
0111 + 0101 = 01100
0111 + 0110 = 01101
0111 + 0111 = 01110
0111 + 1000 = 01111
0111 + 1001 = 10000
0111 + 1010 = 10001
0111 + 1011 = 10010
0111 + 1100 = 10011
0111 + 1101 = 10100
0111 + 1110 = 10101
0111 + 1111 = 10110
1000 + 0000 = 01000
1000 + 0001 = 01001
1000 + 0010 = 01010
1000 + 0011 = 01011
1000 + 0100 = 01100
1000 + 0101 = 01101
1000 + 0110 = 01110
1000 + 0111 = 01111
1000 + 1000 = 10000
1000 + 1001 = 10001
1000 + 1010 = 10010
1000 + 1011 = 10011
1000 + 1100 = 10100
1000 + 1101 = 10101
1000 + 1110 = 10110
1000 + 1111 = 10111
1001 + 0000 = 01001
1001 + 0001 = 01010
1001 + 0010 = 01011
1001 + 0011 = 01100
1001 + 0100 = 01101
1001 + 0101 = 01110
1001 + 0110 = 01111
1001 + 0111 = 10000
1001 + 1000 = 10001
1001 + 1001 = 10010
1001 + 1010 = 10011
1001 + 1011 = 10100
1001 + 1100 = 10101
1001 + 1101 = 10110
1001 + 1110 = 10111
1001 + 1111 = 11000
1010 + 0000 = 01010
1010 + 0001 = 01011
1010 + 0010 = 01100
1010 + 0011 = 01101
1010 + 0100 = 01110
1010 + 0101 = 01111
1010 + 0110 = 10000
1010 + 0111 = 10001
1010 + 1000 = 10010
1010 + 1001 = 10011
1010 + 1010 = 10100
1010 + 1011 = 10101
1010 + 1100 = 10110
1010 + 1101 = 10111
1010 + 1110 = 11000
1010 + 1111 = 11001
1011 + 0000 = 01011
1011 + 0001 = 01100
1011 + 0010 = 01101
1011 + 0011 = 01110
1011 + 0100 = 01111
1011 + 0101 = 10000
1011 + 0110 = 10001
1011 + 0111 = 10010
1011 + 1000 = 10011
1011 + 1001 = 10100
1011 + 1010 = 10101
1011 + 1011 = 10110
1011 + 1100 = 10111
1011 + 1101 = 11000
1011 + 1110 = 11001
1011 + 1111 = 11010
1100 + 0000 = 01100
1100 + 0001 = 01101
1100 + 0010 = 01110
1100 + 0011 = 01111
1100 + 0100 = 10000
1100 + 0101 = 10001
1100 + 0110 = 10010
1100 + 0111 = 10011
1100 + 1000 = 10100
1100 + 1001 = 10101
1100 + 1010 = 10110
1100 + 1011 = 10111
1100 + 1100 = 11000
1100 + 1101 = 11001
1100 + 1110 = 11010
1100 + 1111 = 11011
1101 + 0000 = 01101
1101 + 0001 = 01110
1101 + 0010 = 01111
1101 + 0011 = 10000
1101 + 0100 = 10001
1101 + 0101 = 10010
1101 + 0110 = 10011
1101 + 0111 = 10100
1101 + 1000 = 10101
1101 + 1001 = 10110
1101 + 1010 = 10111
1101 + 1011 = 11000
1101 + 1100 = 11001
1101 + 1101 = 11010
1101 + 1110 = 11011
1101 + 1111 = 11100
1110 + 0000 = 01110
1110 + 0001 = 01111
1110 + 0010 = 10000
1110 + 0011 = 10001
1110 + 0100 = 10010
1110 + 0101 = 10011
1110 + 0110 = 10100
1110 + 0111 = 10101
1110 + 1000 = 10110
1110 + 1001 = 10111
1110 + 1010 = 11000
1110 + 1011 = 11001
1110 + 1100 = 11010
1110 + 1101 = 11011
1110 + 1110 = 11100
1110 + 1111 = 11101
1111 + 0000 = 01111
1111 + 0001 = 10000
1111 + 0010 = 10001
1111 + 0011 = 10010
1111 + 0100 = 10011
1111 + 0101 = 10100
1111 + 0110 = 10101
1111 + 0111 = 10110
1111 + 1000 = 10111
1111 + 1001 = 11000
1111 + 1010 = 11001
1111 + 1011 = 11010
1111 + 1100 = 11011
1111 + 1101 = 11100
1111 + 1110 = 11101
1111 + 1111 = 11110

```

</div>
