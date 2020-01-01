+++
title = "Zeckendorf arithmetic"
description = ""
date = 2019-10-18T21:00:57Z
aliases = []
[extra]
id = 12489
[taxonomies]
categories = []
tags = []
+++

{{task|Arithmetic operations}}
This task is a ''total immersion'' zeckendorf task; using decimal numbers will attract serious disapprobation.

The task is to implement addition, subtraction, multiplication, and division using [[Zeckendorf number representation]]. [[Zeckendorf number representation#Using_a_C.2B.2B11_User_Defined_Literal|Optionally]] provide decrement, increment and comparitive operation functions.

;Addition
Like binary 1 + 1 = 10, note carry 1 left. There the similarity ends. 10 + 10 = 101, note carry 1 left and 1 right. 100 + 100 = 1001, note carry 1 left and 2 right, this is the general case.

Occurrences of 11 must be changed to 100. Occurrences of 111 may be changed from the right by replacing 11 with 100, or from the left converting 111 to 100 + 100;

;Subtraction
10 - 1 = 1. The general rule is borrow 1 right carry 1 left. eg:

```txt

  abcde
  10100 -
   1000
  _____
    100  borrow 1 from a leaves 100
  + 100  add the carry
  _____
   1001

```

A larger example:

```txt

  abcdef
  100100 -
    1000
  ______
  1*0100 borrow 1 from b
   + 100 add the carry
  ______
  1*1001

Sadly we borrowed 1 from b which didn't have it to lend. So now b borrows from a:

    1001
  + 1000 add the carry
    ____
   10100

```


;Multiplication
Here you teach your computer its zeckendorf tables. eg. 101 * 1001:

```txt

  a = 1 * 101 = 101
  b = 10 * 101 = a + a = 10000
  c = 100 * 101 = b + a = 10101
  d = 1000 * 101 = c + b = 101010

  1001 = d + a therefore 101 * 1001 =

  101010
   + 101
  ______
 1000100

```


;Division
Lets try 1000101 divided by 101, so we can use the same table used for multiplication.

```txt

  1000101 -
   101010 subtract d (1000 * 101)
  _______
     1000 -
      101 b and c are too large to subtract, so subtract a
     ____
        1 so 1000101 divided by 101 is d + a (1001) remainder 1

```


[http://arxiv.org/pdf/1207.4497.pdf Efficient algorithms for Zeckendorf arithmetic] is interesting. The sections on addition and subtraction are particularly relevant for this task.


## C

{{trans|D}}

```c
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

int inv(int a) {
    return a ^ -1;
}

struct Zeckendorf {
    int dVal, dLen;
};

void a(struct Zeckendorf *self, int n) {
    void b(struct Zeckendorf *, int); // forward declare

    int i = n;
    while (true) {
        if (self->dLen < i) self->dLen = i;
        int j = (self->dVal >> (i * 2)) & 3;
        switch (j) {
        case 0:
        case 1:
            return;
        case 2:
            if (((self->dVal >> ((i + 1) * 2)) & 1) != 1) return;
            self->dVal += 1 << (i * 2 + 1);
            return;
        case 3:
            self->dVal = self->dVal & inv(3 << (i * 2));
            b(self, (i + 1) * 2);
            break;
        default:
            break;
        }
        i++;
    }
}

void b(struct Zeckendorf *self, int pos) {
    void increment(struct Zeckendorf *); // forward declare

    if (pos == 0) {
        increment(self);
        return;
    }
    if (((self->dVal >> pos) & 1) == 0) {
        self->dVal += 1 << pos;
        a(self, pos / 2);
        if (pos > 1) a(self, pos / 2 - 1);
    } else {
        self->dVal = self->dVal & inv(1 << pos);
        b(self, pos + 1);
        b(self, pos - (pos > 1 ? 2 : 1));
    }
}

void c(struct Zeckendorf *self, int pos) {
    if (((self->dVal >> pos) & 1) == 1) {
        self->dVal = self->dVal & inv(1 << pos);
        return;
    }
    c(self, pos + 1);
    if (pos > 0) {
        b(self, pos - 1);
    } else {
        increment(self);
    }
}

struct Zeckendorf makeZeckendorf(char *x) {
    struct Zeckendorf z = { 0, 0 };
    int i = strlen(x) - 1;
    int q = 1;

    z.dLen = i / 2;
    while (i >= 0) {
        z.dVal += (x[i] - '0') * q;
        q *= 2;
        i--;
    }

    return z;
}

void increment(struct Zeckendorf *self) {
    self->dVal++;
    a(self, 0);
}

void addAssign(struct Zeckendorf *self, struct Zeckendorf rhs) {
    int gn;
    for (gn = 0; gn < (rhs.dLen + 1) * 2; gn++) {
        if (((rhs.dVal >> gn) & 1) == 1) {
            b(self, gn);
        }
    }
}

void subAssign(struct Zeckendorf *self, struct Zeckendorf rhs) {
    int gn;
    for (gn = 0; gn < (rhs.dLen + 1) * 2; gn++) {
        if (((rhs.dVal >> gn) & 1) == 1) {
            c(self, gn);
        }
    }
    while ((((self->dVal >> self->dLen * 2) & 3) == 0) || (self->dLen == 0)) {
        self->dLen--;
    }
}

void mulAssign(struct Zeckendorf *self, struct Zeckendorf rhs) {
    struct Zeckendorf na = rhs;
    struct Zeckendorf nb = rhs;
    struct Zeckendorf nr = makeZeckendorf("0");
    struct Zeckendorf nt;
    int i;

    for (i = 0; i < (self->dLen + 1) * 2; i++) {
        if (((self->dVal >> i) & 1) > 0) addAssign(&nr, nb);
        nt = nb;
        addAssign(&nb, na);
        na = nt;
    }

    *self = nr;
}

void printZeckendorf(struct Zeckendorf z) {
    static const char *const dig[3] = { "00", "01", "10" };
    static const char *const dig1[3] = { "", "1", "10" };

    if (z.dVal == 0) {
        printf("0");
        return;
    } else {
        int idx = (z.dVal >> (z.dLen * 2)) & 3;
        int i;

        printf(dig1[idx]);
        for (i = z.dLen - 1; i >= 0; i--) {
            idx = (z.dVal >> (i * 2)) & 3;
            printf(dig[idx]);
        }
    }
}

int main() {
    struct Zeckendorf g;

    printf("Addition:\n");
    g = makeZeckendorf("10");
    addAssign(&g, makeZeckendorf("10"));
    printZeckendorf(g);
    printf("\n");
    addAssign(&g, makeZeckendorf("10"));
    printZeckendorf(g);
    printf("\n");
    addAssign(&g, makeZeckendorf("1001"));
    printZeckendorf(g);
    printf("\n");
    addAssign(&g, makeZeckendorf("1000"));
    printZeckendorf(g);
    printf("\n");
    addAssign(&g, makeZeckendorf("10101"));
    printZeckendorf(g);
    printf("\n\n");

    printf("Subtraction:\n");
    g = makeZeckendorf("1000");
    subAssign(&g, makeZeckendorf("101"));
    printZeckendorf(g);
    printf("\n");
    g = makeZeckendorf("10101010");
    subAssign(&g, makeZeckendorf("1010101"));
    printZeckendorf(g);
    printf("\n\n");

    printf("Multiplication:\n");
    g = makeZeckendorf("1001");
    mulAssign(&g, makeZeckendorf("101"));
    printZeckendorf(g);
    printf("\n");
    g = makeZeckendorf("101010");
    addAssign(&g, makeZeckendorf("101"));
    printZeckendorf(g);
    printf("\n");

    return 0;
}
```

{{out}}

```txt
Addition:
101
1001
10101
100101
1010000

Subtraction:
1
1000000

Multiplication:
1000100
1000100
```



## C++

{{works with|C++11}}

```cpp
// For a class N which implements Zeckendorf numbers:
// I define an increment operation ++()
// I define a comparison operation <=(other N)
// I define an addition operation +=(other N)
// I define a subtraction operation -=(other N)
// Nigel Galloway October 28th., 2012
#include <iostream>
enum class zd {N00,N01,N10,N11};
class N {
private:
  int dVal = 0, dLen;
  void _a(int i) {
    for (;; i++) {
      if (dLen < i) dLen = i;
      switch ((zd)((dVal >> (i*2)) & 3)) {
        case zd::N00: case zd::N01: return;
        case zd::N10: if (((dVal >> ((i+1)*2)) & 1) != 1) return;
                      dVal += (1 << (i*2+1)); return;
        case zd::N11: dVal &= ~(3 << (i*2)); _b((i+1)*2);
  }}}
  void _b(int pos) {
    if (pos == 0) {++*this; return;}
    if (((dVal >> pos) & 1) == 0) {
      dVal += 1 << pos;
      _a(pos/2);
      if (pos > 1) _a((pos/2)-1);
      } else {
      dVal &= ~(1 << pos);
      _b(pos + 1);
      _b(pos - ((pos > 1)? 2:1));
  }}
  void _c(int pos) {
    if (((dVal >> pos) & 1) == 1) {dVal &= ~(1 << pos); return;}
    _c(pos + 1);
    if (pos > 0) _b(pos - 1); else ++*this;
    return;
  }
public:
  N(char const* x = "0") {
    int i = 0, q = 1;
    for (; x[i] > 0; i++);
    for (dLen = --i/2; i >= 0; i--) {dVal+=(x[i]-48)*q; q*=2;
  }}
  const N& operator++() {dVal += 1; _a(0); return *this;}
  const N& operator+=(const N& other) {
    for (int GN = 0; GN < (other.dLen + 1) * 2; GN++) if ((other.dVal >> GN) & 1 == 1) _b(GN);
    return *this;
  }
  const N& operator-=(const N& other) {
    for (int GN = 0; GN < (other.dLen + 1) * 2; GN++) if ((other.dVal >> GN) & 1 == 1) _c(GN);
    for (;((dVal >> dLen*2) & 3) == 0 or dLen == 0; dLen--);
    return *this;
  }
  const N& operator*=(const N& other) {
    N Na = other, Nb = other, Nt, Nr;
    for (int i = 0; i <= (dLen + 1) * 2; i++) {
      if (((dVal >> i) & 1) > 0) Nr += Nb;
      Nt = Nb; Nb += Na; Na = Nt;
    }
    return *this = Nr;
  }
  const bool operator<=(const N& other) const {return dVal <= other.dVal;}
  friend std::ostream& operator<<(std::ostream&, const N&);
};
N operator "" N(char const* x) {return N(x);}
std::ostream &operator<<(std::ostream &os, const N &G) {
  const static std::string dig[] {"00","01","10"}, dig1[] {"","1","10"};
  if (G.dVal == 0) return os << "0";
  os << dig1[(G.dVal >> (G.dLen*2)) & 3];
  for (int i = G.dLen-1; i >= 0; i--) os << dig[(G.dVal >> (i*2)) & 3];
  return os;
}

```


### Testing

The following tests addtition:

```cpp
int main(void) {
  N G;
  G = 10N;
  G += 10N;
  std::cout << G << std::endl;
  G += 10N;
  std::cout << G << std::endl;
  G += 1001N;
  std::cout << G << std::endl;
  G += 1000N;
  std::cout << G << std::endl;
  G += 10101N;
  std::cout << G << std::endl;
  return 0;
}
```

{{out}}

```txt

101
1001
10101
100101
1010000

```

The following tests subtraction:

```cpp
int main(void) {
  N G;
  G = 1000N;
  G -= 101N;
  std::cout << G << std::endl;
  G = 10101010N;
  G -= 1010101N;
  std::cout << G << std::endl;
  return 0;
}
```

{{out}}

```txt

1
1000000

```

The following tests multiplication:

```cpp

int main(void) {
  N G = 1001N;
  G *= 101N;
  std::cout << G << std::endl;

  G = 101010N;
  G += 101N;
  std::cout << G << std::endl;
  return 0;
}
```

{{out}}

```txt

1000100
1000100

```


## C#
{{trans|Java}}

```c#
using System;
using System.Text;

namespace ZeckendorfArithmetic {
    class Zeckendorf : IComparable<Zeckendorf> {
        private static readonly string[] dig = { "00", "01", "10" };
        private static readonly string[] dig1 = { "", "1", "10" };

        private int dVal = 0;
        private int dLen = 0;

        public Zeckendorf() : this("0") {
            // empty
        }

        public Zeckendorf(string x) {
            int q = 1;
            int i = x.Length - 1;
            dLen = i / 2;
            while (i >= 0) {
                dVal += (x[i] - '0') * q;
                q *= 2;
                i--;
            }
        }

        private void A(int n) {
            int i = n;
            while (true) {
                if (dLen < i) dLen = i;
                int j = (dVal >> (i * 2)) & 3;
                switch (j) {
                    case 0:
                    case 1:
                        return;
                    case 2:
                        if (((dVal >> ((i + 1) * 2)) & 1) != 1) return;
                        dVal += 1 << (i * 2 + 1);
                        return;
                    case 3:
                        int temp = 3 << (i * 2);
                        temp ^= -1;
                        dVal = dVal & temp;
                        B((i + 1) * 2);
                        break;
                }
                i++;
            }
        }

        private void B(int pos) {
            if (pos == 0) {
                Inc();
                return;
            }
            if (((dVal >> pos) & 1) == 0) {
                dVal += 1 << pos;
                A(pos / 2);
                if (pos > 1) A(pos / 2 - 1);
            }
            else {
                int temp = 1 << pos;
                temp ^= -1;
                dVal = dVal & temp;
                B(pos + 1);
                B(pos - (pos > 1 ? 2 : 1));
            }
        }

        private void C(int pos) {
            if (((dVal >> pos) & 1) == 1) {
                int temp = 1 << pos;
                temp ^= -1;
                dVal = dVal & temp;
                return;
            }
            C(pos + 1);
            if (pos > 0) {
                B(pos - 1);
            }
            else {
                Inc();
            }
        }

        public Zeckendorf Inc() {
            dVal++;
            A(0);
            return this;
        }

        public Zeckendorf Copy() {
            Zeckendorf z = new Zeckendorf {
                dVal = dVal,
                dLen = dLen
            };
            return z;
        }

        public void PlusAssign(Zeckendorf other) {
            for (int gn = 0; gn < (other.dLen + 1) * 2; gn++) {
                if (((other.dVal >> gn) & 1) == 1) {
                    B(gn);
                }
            }
        }

        public void MinusAssign(Zeckendorf other) {
            for (int gn = 0; gn < (other.dLen + 1) * 2; gn++) {
                if (((other.dVal >> gn) & 1) == 1) {
                    C(gn);
                }
            }
            while ((((dVal >> dLen * 2) & 3) == 0) || (dLen == 0)) {
                dLen--;
            }
        }

        public void TimesAssign(Zeckendorf other) {
            Zeckendorf na = other.Copy();
            Zeckendorf nb = other.Copy();
            Zeckendorf nt;
            Zeckendorf nr = new Zeckendorf();
            for (int i = 0; i < (dLen + 1) * 2; i++) {
                if (((dVal >> i) & 1) > 0) {
                    nr.PlusAssign(nb);
                }
                nt = nb.Copy();
                nb.PlusAssign(na);
                na = nt.Copy();
            }
            dVal = nr.dVal;
            dLen = nr.dLen;
        }

        public int CompareTo(Zeckendorf other) {
            return dVal.CompareTo(other.dVal);
        }

        public override string ToString() {
            if (dVal == 0) {
                return "0";
            }

            int idx = (dVal >> (dLen * 2)) & 3;
            StringBuilder sb = new StringBuilder(dig1[idx]);
            for (int i = dLen - 1; i >= 0; i--) {
                idx = (dVal >> (i * 2)) & 3;
                sb.Append(dig[idx]);
            }
            return sb.ToString();
        }
    }

    class Program {
        static void Main(string[] args) {
            Console.WriteLine("Addition:");
            Zeckendorf g = new Zeckendorf("10");
            g.PlusAssign(new Zeckendorf("10"));
            Console.WriteLine(g);
            g.PlusAssign(new Zeckendorf("10"));
            Console.WriteLine(g);
            g.PlusAssign(new Zeckendorf("1001"));
            Console.WriteLine(g);
            g.PlusAssign(new Zeckendorf("1000"));
            Console.WriteLine(g);
            g.PlusAssign(new Zeckendorf("10101"));
            Console.WriteLine(g);
            Console.WriteLine();

            Console.WriteLine("Subtraction:");
            g = new Zeckendorf("1000");
            g.MinusAssign(new Zeckendorf("101"));
            Console.WriteLine(g);
            g = new Zeckendorf("10101010");
            g.MinusAssign(new Zeckendorf("1010101"));
            Console.WriteLine(g);
            Console.WriteLine();

            Console.WriteLine("Multiplication:");
            g = new Zeckendorf("1001");
            g.TimesAssign(new Zeckendorf("101"));
            Console.WriteLine(g);
            g = new Zeckendorf("101010");
            g.PlusAssign(new Zeckendorf("101"));
            Console.WriteLine(g);
        }
    }
}
```

{{out}}

```txt
Addition:
101
1001
10101
100101
1010000

Subtraction:
1
1000000

Multiplication:
1000100
1000100
```



## D

{{trans|Kotlin}}

```D
import std.stdio;

int inv(int a) {
    return a ^ -1;
}

class Zeckendorf {
    private int dVal;
    private int dLen;

    private void a(int n) {
        auto i = n;
        while (true) {
            if (dLen < i) dLen = i;
            auto j = (dVal >> (i * 2)) & 3;
            switch(j) {
                case 0:
                case 1:
                    return;
                case 2:
                    if (((dVal >> ((i + 1) * 2)) & 1) != 1) return;
                    dVal += 1 << (i * 2 + 1);
                    return;
                case 3:
                    dVal = dVal & (3 << (i * 2)).inv();
                    b((i + 1) * 2);
                    break;
                default:
                    assert(false);
            }
            i++;
        }
    }

    private void b(int pos) {
        if (pos == 0) {
            this++;
            return;
        }
        if (((dVal >> pos) & 1) == 0) {
            dVal += 1 << pos;
            a(pos / 2);
            if (pos > 1) a(pos / 2 - 1);
        } else {
            dVal = dVal & (1 << pos).inv();
            b(pos + 1);
            b(pos - (pos > 1 ? 2 : 1));
        }
    }

    private void c(int pos) {
        if (((dVal >> pos) & 1) == 1) {
            dVal = dVal & (1 << pos).inv();
            return;
        }
        c(pos + 1);
        if (pos > 0) {
            b(pos - 1);
        } else {
            ++this;
        }
    }

    this(string x = "0") {
        int q = 1;
        int i = x.length - 1;
        dLen = i / 2;
        while (i >= 0) {
            dVal += (x[i] - '0') * q;
            q *= 2;
            i--;
        }
    }

    auto opUnary(string op : "++")() {
        dVal += 1;
        a(0);
        return this;
    }

    void opOpAssign(string op : "+")(Zeckendorf rhs) {
        foreach (gn; 0..(rhs.dLen + 1) * 2) {
            if (((rhs.dVal >> gn) & 1) == 1) {
                b(gn);
            }
        }
    }

    void opOpAssign(string op : "-")(Zeckendorf rhs) {
        foreach (gn; 0..(rhs.dLen + 1) * 2) {
            if (((rhs.dVal >> gn) & 1) == 1) {
                c(gn);
            }
        }
        while ((((dVal >> dLen * 2) & 3) == 0) || (dLen == 0)) {
            dLen--;
        }
    }

    void opOpAssign(string op : "*")(Zeckendorf rhs) {
        auto na = rhs.dup;
        auto nb = rhs.dup;
        Zeckendorf nt;
        auto nr = "0".Z;
        foreach (i; 0..(dLen + 1) * 2) {
            if (((dVal >> i) & 1) > 0) nr += nb;
            nt = nb.dup;
            nb += na;
            na = nt.dup;
        }
        dVal = nr.dVal;
        dLen = nr.dLen;
    }

    void toString(scope void delegate(const(char)[]) sink) const {
        if (dVal == 0) {
            sink("0");
            return;
        }
        sink(dig1[(dVal >> (dLen * 2)) & 3]);
        foreach_reverse (i; 0..dLen) {
            sink(dig[(dVal >> (i * 2)) & 3]);
        }
    }

    Zeckendorf dup() {
        auto z = "0".Z;
        z.dVal = dVal;
        z.dLen = dLen;
        return z;
    }

    enum dig = ["00", "01", "10"];
    enum dig1 = ["", "1", "10"];
}

auto Z(string val) {
    return new Zeckendorf(val);
}

void main() {
    writeln("Addition:");
    auto g = "10".Z;
    g += "10".Z;
    writeln(g);
    g += "10".Z;
    writeln(g);
    g += "1001".Z;
    writeln(g);
    g += "1000".Z;
    writeln(g);
    g += "10101".Z;
    writeln(g);
    writeln();

    writeln("Subtraction:");
    g = "1000".Z;
    g -= "101".Z;
    writeln(g);
    g = "10101010".Z;
    g -= "1010101".Z;
    writeln(g);
    writeln();

    writeln("Multiplication:");
    g = "1001".Z;
    g *= "101".Z;
    writeln(g);
    g = "101010".Z;
    g += "101".Z;
    writeln(g);
}
```

{{out}}

```txt
Addition:
101
1001
10101
100101
1010000

Subtraction:
1
1000000

Multiplication:
1000100
1000100
```



## Elena

{{trans|C++}}
ELENA 4.1 :

```elena
import extensions;

const dig = new string[]::("00","01","10");
const dig1 = new string[]::("","1","10");

sealed struct ZeckendorfNumber
{
    int dVal;
    int dLen;

    clone()
        = ZeckendorfNumber.newInternal(dVal,dLen);

    cast n(string s)
    {
        int i := s.Length - 1;
        int q := 1;

        dLen := i / 2;
        dVal := 0;

        while (i >= 0)
        {
            dVal += ((intConvertor.convert(s[i]) - 48) * q);
            q *= 2;

            i -= 1
        }
    }

    internal readContent(ref int val, ref int len)
    {
        val := dVal;
        len := dLen;
    }

    private a(int n)
    {
        int i := n;

        while (true)
        {
            if (dLen < i)
            {
                dLen := i
            };

            int v2 := dVal $shr (i * 2);
            int v := (dVal $shr (i * 2)) && 3;

            ((dVal $shr (i * 2)) && 3) =>
                0 { ^ self }
                1 { ^ self }
                2 {
                    ifnot ((dVal $shr ((i + 1) * 2)).allMask:1)
                    {
                        ^ self
                    };

                    dVal += (1 $shl (i*2 + 1));

                    ^ self
                }
                3 {
                    int tmp := 3 $shl (i * 2);
                    tmp := tmp.xor(-1);
                    dVal := dVal && tmp;

                    self.b((i+1)*2)
                };

            i += 1
        }
    }

    inc()
    {
        dVal += 1;
        self.a(0)
    }

    private b(int pos)
    {
        if (pos == 0) { ^ self.inc() };

        ifnot((dVal $shr pos).allMask:1)
        {
            dVal += (1 $shl pos);
            self.a(pos / 2);
            if (pos > 1) { self.a((pos / 2) - 1) }
        }
        else
        {
            dVal := dVal && (1 $shl pos).Inverted;
            self.b(pos + 1);
            int arg := pos - ((pos > 1) ? 2 : 1);
            self.b(/*pos - ((pos > 1) ? 2 : 1)*/arg)
        }
    }

    private c(int pos)
    {
        if ((dVal $shr pos).allMask:1)
        {
            int tmp := 1 $shl pos;
            tmp := tmp.xor(-1);

            dVal := dVal && tmp;

            ^ self
        };

        self.c(pos + 1);

        if (pos > 0)
        {
            self.b(pos - 1)
        }
        else
        {
            self.inc()
        }
    }

    internal constructor sum(ZeckendorfNumber n, ZeckendorfNumber m)
    {
        int mVal := 0;
        int mLen := 0;

        n.readContent(ref dVal, ref dLen);
        m.readContent(ref mVal, ref mLen);

        for(int GN := 0, GN < (mLen + 1) * 2, GN += 1)
        {
            if ((mVal $shr GN).allMask:1)
            {
                self.b(GN)
            }
        }
    }

    internal constructor difference(ZeckendorfNumber n, ZeckendorfNumber m)
    {
        int mVal := 0;
        int mLen := 0;

        n.readContent(ref dVal, ref dLen);
        m.readContent(ref mVal, ref mLen);

        for(int GN := 0, GN < (mLen + 1) * 2, GN += 1)
        {
            if ((mVal $shr GN).allMask:1)
            {
                self.c(GN)
            }
        };

        while (((dVal $shr (dLen*2)) && 3) == 0 || dLen == 0)
        {
            dLen -= 1
        }
    }

    internal constructor product(ZeckendorfNumber n, ZeckendorfNumber m)
    {
        n.readContent(ref dVal, ref dLen);

        ZeckendorfNumber Na := m;
        ZeckendorfNumber Nb := m;
        ZeckendorfNumber Nr := 0n;
        ZeckendorfNumber Nt := 0n;

        for(int i := 0, i < (dLen + 1) * 2, i += 1)
        {
            if (((dVal $shr i) && 1) > 0)
            {
                Nr += Nb
            };
            Nt := Nb;
            Nb += Na;
            Na := Nt
        };

        Nr.readContent(ref dVal, ref dLen);
    }

    internal constructor newInternal(int v, int l)
    {
        dVal := v;
        dLen := l
    }

    get string Printable()
    {
        if (dVal == 0)
            { ^ "0" };

        //int n := dVal $shr (dLen * 2);
        //int r := (dVal $shr (dLen * 2)) && 3;

        string s := dig1[(dVal $shr (dLen * 2)) && 3];
        int i := dLen - 1;
        while (i >= 0)
        {
            s := s + dig[(dVal $shr (i * 2)) && 3];

            i-=1
        };

        ^ s
    }

    add(ZeckendorfNumber n)
        = ZeckendorfNumber.sum(self, n);

    subtract(ZeckendorfNumber n)
        = ZeckendorfNumber.difference(self, n);

    multiply(ZeckendorfNumber n)
        = ZeckendorfNumber.product(self, n);
}

public program()
{
    console.printLine("Addition:");
    var n := 10n;

    n += 10n;
    console.printLine(n);
    n += 10n;
    console.printLine(n);
    n += 1001n;
    console.printLine(n);
    n += 1000n;
    console.printLine(n);
    n += 10101n;
    console.printLine(n);

    console.printLine("Subtraction:");
    n := 1000n;
    n -= 101n;
    console.printLine(n);
    n := 10101010n;
    n -= 1010101n;
    console.printLine(n);

    console.printLine("Multiplication:");
    n := 1001n;
    n *= 101n;
    console.printLine(n);
    n := 101010n;
    n += 101n;
    console.printLine(n)
}
```

{{out}}

```txt

Addition:
101
1001
10101
100101
1010000
Subtraction:
1
1000000
Multiplication:
1000100
1000100

```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "strings"
)

var (
    dig  = [3]string{"00", "01", "10"}
    dig1 = [3]string{"", "1", "10"}
)

type Zeckendorf struct{ dVal, dLen int }

func NewZeck(x string) *Zeckendorf {
    z := new(Zeckendorf)
    if x == "" {
        x = "0"
    }
    q := 1
    i := len(x) - 1
    z.dLen = i / 2
    for ; i >= 0; i-- {
        z.dVal += int(x[i]-'0') * q
        q *= 2
    }
    return z
}

func (z *Zeckendorf) a(i int) {
    for ; ; i++ {
        if z.dLen < i {
            z.dLen = i
        }
        j := (z.dVal >> uint(i*2)) & 3
        switch j {
        case 0, 1:
            return
        case 2:
            if ((z.dVal >> (uint(i+1) * 2)) & 1) != 1 {
                return
            }
            z.dVal += 1 << uint(i*2+1)
            return
        case 3:
            z.dVal &= ^(3 << uint(i*2))
            z.b((i + 1) * 2)
        }
    }
}

func (z *Zeckendorf) b(pos int) {
    if pos == 0 {
        z.Inc()
        return
    }
    if ((z.dVal >> uint(pos)) & 1) == 0 {
        z.dVal += 1 << uint(pos)
        z.a(pos / 2)
        if pos > 1 {
            z.a(pos/2 - 1)
        }
    } else {
        z.dVal &= ^(1 << uint(pos))
        z.b(pos + 1)
        temp := 1
        if pos > 1 {
            temp = 2
        }
        z.b(pos - temp)
    }
}

func (z *Zeckendorf) c(pos int) {
    if ((z.dVal >> uint(pos)) & 1) == 1 {
        z.dVal &= ^(1 << uint(pos))
        return
    }
    z.c(pos + 1)
    if pos > 0 {
        z.b(pos - 1)
    } else {
        z.Inc()
    }
}

func (z *Zeckendorf) Inc() {
    z.dVal++
    z.a(0)
}

func (z1 *Zeckendorf) PlusAssign(z2 *Zeckendorf) {
    for gn := 0; gn < (z2.dLen+1)*2; gn++ {
        if ((z2.dVal >> uint(gn)) & 1) == 1 {
            z1.b(gn)
        }
    }
}

func (z1 *Zeckendorf) MinusAssign(z2 *Zeckendorf) {
    for gn := 0; gn < (z2.dLen+1)*2; gn++ {
        if ((z2.dVal >> uint(gn)) & 1) == 1 {
            z1.c(gn)
        }
    }

    for z1.dLen > 0 && ((z1.dVal>>uint(z1.dLen*2))&3) == 0 {
        z1.dLen--
    }
}

func (z1 *Zeckendorf) TimesAssign(z2 *Zeckendorf) {
    na := z2.Copy()
    nb := z2.Copy()
    nr := new(Zeckendorf)
    for i := 0; i <= (z1.dLen+1)*2; i++ {
        if ((z1.dVal >> uint(i)) & 1) > 0 {
            nr.PlusAssign(nb)
        }
        nt := nb.Copy()
        nb.PlusAssign(na)
        na = nt.Copy()
    }
    z1.dVal = nr.dVal
    z1.dLen = nr.dLen
}

func (z *Zeckendorf) Copy() *Zeckendorf {
    return &Zeckendorf{z.dVal, z.dLen}
}

func (z1 *Zeckendorf) Compare(z2 *Zeckendorf) int {
    switch {
    case z1.dVal < z2.dVal:
        return -1
    case z1.dVal > z2.dVal:
        return 1
    default:
        return 0
    }
}

func (z *Zeckendorf) String() string {
    if z.dVal == 0 {
        return "0"
    }
    var sb strings.Builder
    sb.WriteString(dig1[(z.dVal>>uint(z.dLen*2))&3])
    for i := z.dLen - 1; i >= 0; i-- {
        sb.WriteString(dig[(z.dVal>>uint(i*2))&3])
    }
    return sb.String()
}

func main() {
    fmt.Println("Addition:")
    g := NewZeck("10")
    g.PlusAssign(NewZeck("10"))
    fmt.Println(g)
    g.PlusAssign(NewZeck("10"))
    fmt.Println(g)
    g.PlusAssign(NewZeck("1001"))
    fmt.Println(g)
    g.PlusAssign(NewZeck("1000"))
    fmt.Println(g)
    g.PlusAssign(NewZeck("10101"))
    fmt.Println(g)

    fmt.Println("\nSubtraction:")
    g = NewZeck("1000")
    g.MinusAssign(NewZeck("101"))
    fmt.Println(g)
    g = NewZeck("10101010")
    g.MinusAssign(NewZeck("1010101"))
    fmt.Println(g)

    fmt.Println("\nMultiplication:")
    g = NewZeck("1001")
    g.TimesAssign(NewZeck("101"))
    fmt.Println(g)
    g = NewZeck("101010")
    g.PlusAssign(NewZeck("101"))
    fmt.Println(g)
}
```


{{out}}

```txt

Addition:
101
1001
10101
100101
1010000

Subtraction:
1
1000000

Multiplication:
1000100
1000100

```



## Java

{{trans|Kotlin}}
{{works with|Java|9}}

```Java
import java.util.List;

public class Zeckendorf implements Comparable<Zeckendorf> {
    private static List<String> dig = List.of("00", "01", "10");
    private static List<String> dig1 = List.of("", "1", "10");

    private String x;
    private int dVal = 0;
    private int dLen = 0;

    public Zeckendorf() {
        this("0");
    }

    public Zeckendorf(String x) {
        this.x = x;

        int q = 1;
        int i = x.length() - 1;
        dLen = i / 2;
        while (i >= 0) {
            dVal += (x.charAt(i) - '0') * q;
            q *= 2;
            i--;
        }
    }

    private void a(int n) {
        int i = n;
        while (true) {
            if (dLen < i) dLen = i;
            int j = (dVal >> (i * 2)) & 3;
            switch (j) {
                case 0:
                case 1:
                    return;
                case 2:
                    if (((dVal >> ((i + 1) * 2)) & 1) != 1) return;
                    dVal += 1 << (i * 2 + 1);
                    return;
                case 3:
                    int temp = 3 << (i * 2);
                    temp ^= -1;
                    dVal = dVal & temp;
                    b((i + 1) * 2);
                    break;
            }
            i++;
        }
    }

    private void b(int pos) {
        if (pos == 0) {
            Zeckendorf thiz = this;
            thiz.inc();
            return;
        }
        if (((dVal >> pos) & 1) == 0) {
            dVal += 1 << pos;
            a(pos / 2);
            if (pos > 1) a(pos / 2 - 1);
        } else {
            int temp = 1 << pos;
            temp ^= -1;
            dVal = dVal & temp;
            b(pos + 1);
            b(pos - (pos > 1 ? 2 : 1));
        }
    }

    private void c(int pos) {
        if (((dVal >> pos) & 1) == 1) {
            int temp = 1 << pos;
            temp ^= -1;
            dVal = dVal & temp;
            return;
        }
        c(pos + 1);
        if (pos > 0) {
            b(pos - 1);
        } else {
            Zeckendorf thiz = this;
            thiz.inc();
        }
    }

    public Zeckendorf inc() {
        dVal++;
        a(0);
        return this;
    }

    public void plusAssign(Zeckendorf other) {
        for (int gn = 0; gn < (other.dLen + 1) * 2; gn++) {
            if (((other.dVal >> gn) & 1) == 1) {
                b(gn);
            }
        }
    }

    public void minusAssign(Zeckendorf other) {
        for (int gn = 0; gn < (other.dLen + 1) * 2; gn++) {
            if (((other.dVal >> gn) & 1) == 1) {
                c(gn);
            }
        }
        while ((((dVal >> dLen * 2) & 3) == 0) || (dLen == 0)) {
            dLen--;
        }
    }

    public void timesAssign(Zeckendorf other) {
        Zeckendorf na = other.copy();
        Zeckendorf nb = other.copy();
        Zeckendorf nt;
        Zeckendorf nr = new Zeckendorf();
        for (int i = 0; i < (dLen + 1) * 2; i++) {
            if (((dVal >> i) & 1) > 0) {
                nr.plusAssign(nb);
            }
            nt = nb.copy();
            nb.plusAssign(na);
            na = nt.copy();
        }
        dVal = nr.dVal;
        dLen = nr.dLen;
    }

    private Zeckendorf copy() {
        Zeckendorf z = new Zeckendorf();
        z.dVal = dVal;
        z.dLen = dLen;
        return z;
    }

    @Override
    public int compareTo(Zeckendorf other) {
        return ((Integer) dVal).compareTo(other.dVal);
    }

    @Override
    public String toString() {
        if (dVal == 0) {
            return "0";
        }

        int idx = (dVal >> (dLen * 2)) & 3;
        StringBuilder stringBuilder = new StringBuilder(dig1.get(idx));
        for (int i = dLen - 1; i >= 0; i--) {
            idx = (dVal >> (i * 2)) & 3;
            stringBuilder.append(dig.get(idx));
        }
        return stringBuilder.toString();
    }

    public static void main(String[] args) {
        System.out.println("Addition:");
        Zeckendorf g = new Zeckendorf("10");
        g.plusAssign(new Zeckendorf("10"));
        System.out.println(g);
        g.plusAssign(new Zeckendorf("10"));
        System.out.println(g);
        g.plusAssign(new Zeckendorf("1001"));
        System.out.println(g);
        g.plusAssign(new Zeckendorf("1000"));
        System.out.println(g);
        g.plusAssign(new Zeckendorf("10101"));
        System.out.println(g);

        System.out.println("\nSubtraction:");
        g = new Zeckendorf("1000");
        g.minusAssign(new Zeckendorf("101"));
        System.out.println(g);
        g = new Zeckendorf("10101010");
        g.minusAssign(new Zeckendorf("1010101"));
        System.out.println(g);

        System.out.println("\nMultiplication:");
        g = new Zeckendorf("1001");
        g.timesAssign(new Zeckendorf("101"));
        System.out.println(g);
        g = new Zeckendorf("101010");
        g.plusAssign(new Zeckendorf("101"));
        System.out.println(g);
    }
}
```

{{out}}

```txt
Addition:
101
1001
10101
100101
1010000

Subtraction:
1
1000000

Multiplication:
1000100
1000100
```



## Julia

Influenced by the format of the Tcl and Perl 6 versions, but added other functionality.

```julia
import Base.*, Base.+, Base.-, Base./, Base.show, Base.!=, Base.==, Base.<=, Base.<, Base.>, Base.>=, Base.divrem

const z0 = "0"
const z1 = "1"
const flipordered = (z1 < z0)

mutable struct Z s::String end
Z() = Z(z0)
Z(z::Z) = Z(z.s)

pairlen(x::Z, y::Z) = max(length(x.s), length(y.s))
tolen(x::Z, n::Int) = (s = x.s; while length(s) < n s = z0 * s end; s)

<(x::Z, y::Z) = (l = pairlen(x, y); flipordered ? tolen(x, l) > tolen(y, l) : tolen(x, l) < tolen(y, l))
>(x::Z, y::Z) = (l = pairlen(x, y); flipordered ? tolen(x, l) < tolen(y, l) : tolen(x, l) > tolen(y, l))
==(x::Z, y::Z) = (l = pairlen(x, y); tolen(x, l) == tolen(y, l))
<=(x::Z, y::Z) = (l = pairlen(x, y); flipordered ? tolen(x, l) >= tolen(y, l) : tolen(x, l) <= tolen(y, l))
>=(x::Z, y::Z) = (l = pairlen(x, y); flipordered ? tolen(x, l) <= tolen(y, l) : tolen(x, l) >= tolen(y, l))
!=(x::Z, y::Z) = (l = pairlen(x, y); tolen(x, l) != tolen(y, l))

function tocanonical(z::Z)
    while occursin(z0 * z1 * z1, z.s)
        z.s = replace(z.s, z0 * z1 * z1 => z1 * z0 * z0)
    end
    len = length(z.s)
    if len > 1 && z.s[1:2] == z1 * z1
        z.s = z1 * z0 * z0 * ((len > 2) ? z.s[3:end] : "")
    end
    while (len = length(z.s)) > 1 && string(z.s[1]) == z0
        if len == 2
            if z.s == z0 * z0
                z.s = z0
            elseif z.s == z0 * z1
                z.s = z1
            end
        else
            z.s = z.s[2:end]
        end
    end
    z
end

function inc(z)
    if z.s[end] == z0[1]
        z.s = z.s[1:end-1] * z1[1]
    elseif z.s[end] == z1[1]
        if length(z.s) > 1
            if z.s[end-1:end] == z0 * z1
                z.s = z.s[1:end-2] * z1 * z0
            end
        else
            z.s = z1 * z0
        end
    end
    tocanonical(z)
end

function dec(z)
    if z.s[end] == z1[1]
        z.s = z.s[1:end-1] * z0
    else
        if (m = match(Regex(z1 * z0 * '+' * '$'), z.s)) != nothing
            len = length(m.match)
            if iseven(len)
                z.s = z.s[1:end-len] * (z0 * z1) ^ div(len, 2)
            else
                z.s = z.s[1:end-len] * (z0 * z1) ^ div(len, 2) * z0
            end
        end
    end
    tocanonical(z)
    z
end

function +(x::Z, y::Z)
    a = Z(x.s)
    b = Z(y.s)
    while b.s != z0
        inc(a)
        dec(b)
    end
    a
end

function -(x::Z, y::Z)
    a = Z(x.s)
    b = Z(y.s)
    while b.s != z0
        dec(a)
        dec(b)
    end
    a
end

function *(x::Z, y::Z)
    if (x.s == z0) || (y.s == z0)
        return Z(z0)
    elseif x.s == z1
        return Z(y.s)
    elseif y.s == z1
        return Z(x.s)
    end
    a = Z(x.s)
    b = Z(z1)
    while b != y
        c = Z(z0)
        while c != x
            inc(a)
            inc(c)
        end
        inc(b)
    end
    a
end

function divrem(x::Z, y::Z)
    if y.s == z0
        throw("Zeckendorf division by 0")
    elseif (y.s == z1) || (x.s == z0)
        return Z(x.s)
    end
    a = Z(x.s)
    b = Z(y.s)
    c = Z(z0)
    while a > b
        a = a - b
        inc(c)
    end
    tocanonical(c), tocanonical(a)
end

function /(x::Z, y::Z)
    a, _ = divrem(x, y)
    a
end

show(io::IO, z::Z) = show(io, parse(BigInt, tocanonical(z).s))

function zeckendorftest()
    a = Z("10")
    b = Z("1001")
    c = Z("1000")
    d = Z("10101")

    println("Addition:")
    x = a
    println(x += a)
    println(x += a)
    println(x += b)
    println(x += c)
    println(x += d)

    println("\nSubtraction:")
    x = Z("1000")
    println(x - Z("101"))
    x = Z("10101010")
    println(x - Z("1010101"))

    println("\nMultiplication:")
    x = Z("1001")
    y = Z("101")
    println(x * y)
    println(Z("101010") * y)

    println("\nDivision:")
    x = Z("1000101")
    y = Z("101")
    println(x / y)
    println(divrem(x, y))
end

zeckendorftest()

```
{{output}}
```txt

 Addition:
 101
 1001
 10101
 100101
 1010000

 Subtraction:
 1
 1000000

 Multiplication:
 1000100
 101000101

 Division:
 1001
 (1001, 1)

```



## Kotlin

{{trans|C++}}

```scala
// version 1.1.51

class Zeckendorf(x: String = "0") : Comparable<Zeckendorf> {

    var dVal = 0
    var dLen = 0

    private fun a(n: Int) {
        var i = n
        while (true) {
            if (dLen < i) dLen = i
            val j = (dVal shr (i * 2)) and 3
            when (j) {
                0, 1 -> return

                2 -> {
                    if (((dVal shr ((i + 1) * 2)) and 1) != 1) return
                    dVal += 1 shl (i * 2 + 1)
                    return
                }

                3 -> {
                    dVal = dVal and (3 shl (i * 2)).inv()
                    b((i + 1) * 2)
                }
            }
            i++
        }
    }

    private fun b(pos: Int) {
        if (pos == 0) {
            var thiz = this
            ++thiz
            return
        }
        if (((dVal shr pos) and 1) == 0) {
            dVal += 1 shl pos
            a(pos / 2)
            if (pos > 1) a(pos / 2 - 1)
        }
        else {
            dVal = dVal and (1 shl pos).inv()
            b(pos + 1)
            b(pos - (if (pos > 1) 2 else 1))
        }
    }

    private fun c(pos: Int) {
        if (((dVal shr pos) and 1) == 1) {
            dVal = dVal and (1 shl pos).inv()
            return
        }
        c(pos + 1)
        if (pos > 0) b(pos - 1) else { var thiz = this; ++thiz }
    }

    init {
        var q = 1
        var i = x.length - 1
        dLen = i / 2
        while (i >= 0) {
            dVal += (x[i] - '0').toInt() * q
            q *= 2
            i--
        }
    }

    operator fun inc(): Zeckendorf {
        dVal += 1
        a(0)
        return this
    }

    operator fun plusAssign(other: Zeckendorf) {
        for (gn in 0 until (other.dLen + 1) * 2) {
            if (((other.dVal shr gn) and 1) == 1) b(gn)
        }
    }

    operator fun minusAssign(other: Zeckendorf) {
        for (gn in 0 until (other.dLen + 1) * 2) {
            if (((other.dVal shr gn) and 1) == 1) c(gn)
        }
        while ((((dVal shr dLen * 2) and 3) == 0) || (dLen == 0)) dLen--
    }

    operator fun timesAssign(other: Zeckendorf) {
        var na = other.copy()
        var nb = other.copy()
        var nt: Zeckendorf
        var nr = "0".Z
        for (i in 0..(dLen + 1) * 2) {
            if (((dVal shr i) and 1) > 0) nr += nb
            nt = nb.copy()
            nb += na
            na = nt.copy()
        }
        dVal = nr.dVal
        dLen = nr.dLen
    }

    override operator fun compareTo(other: Zeckendorf) = dVal.compareTo(other.dVal)

    override fun toString(): String {
        if (dVal == 0) return "0"
        val sb = StringBuilder(dig1[(dVal shr (dLen * 2)) and 3])
        for (i in dLen - 1 downTo 0) {
            sb.append(dig[(dVal shr (i * 2)) and 3])
        }
        return sb.toString()
    }

    fun copy(): Zeckendorf {
        val z = "0".Z
        z.dVal = dVal
        z.dLen = dLen
        return z
    }

    companion object {
        val dig = listOf("00", "01", "10")
        val dig1 = listOf("", "1", "10")
    }
}

val String.Z get() = Zeckendorf(this)

fun main(args: Array<String>) {
    println("Addition:")
    var g = "10".Z
    g += "10".Z
    println(g)
    g += "10".Z
    println(g)
    g += "1001".Z
    println(g)
    g += "1000".Z
    println(g)
    g += "10101".Z
    println(g)
    println("\nSubtraction:")
    g = "1000".Z
    g -= "101".Z
    println(g)
    g = "10101010".Z
    g -= "1010101".Z
    println(g)
    println("\nMultiplication:")
    g = "1001".Z
    g *= "101".Z
    println(g)
    g = "101010".Z
    g += "101".Z
    println(g)
}
```


{{out}}

```txt

Addition:
101
1001
10101
100101
1010000

Subtraction:
1
1000000

Multiplication:
1000100
1000100

```



## Perl 6

This is a somewhat limited implementation of Zeckendorf arithmetic operators. They only handle positive integer values. There are no actual calculations, everything is done with string manipulations, so it doesn't matter what glyphs you use for 1 and 0.
{{works with|rakudo|2019.03}}

Implemented arithmetic operators:
  addition: '''+z'''
  subtraction: '''-z'''
  multiplication: '''*z'''
  division: '''/z''' (more of a divmod really)
  post increment: '''++z'''
  post decrement: '''--z'''

Comparison operators:
  equal '''eqz'''
  not equal '''nez'''
  greater than '''gtz'''
  less than '''ltz'''


```perl6
my $z1 = '1'; # glyph to use for a '1'
my $z0 = '0'; # glyph to use for a '0'

sub zorder($a) { ($z0 lt $z1) ?? $a !! $a.trans([$z0, $z1] => [$z1, $z0]) };

######## Zeckendorf comparison operators #########

# less than
sub infix:<ltz>($a, $b) { $a.&zorder lt $b.&zorder };

# greater than
sub infix:<gtz>($a, $b) { $a.&zorder gt $b.&zorder };

# equal
sub infix:<eqz>($a, $b) { $a eq $b };

# not equal
sub infix:<nez>($a, $b) { $a ne $b };

######## Operators for Zeckendorf arithmetic ########

# post increment
sub postfix:<++z>($a is rw) {
    $a = ("$z0$z0"~$a).subst(/("$z0$z0")($z1+ %% $z0)?$/,
      -> $/ { "$z0$z1" ~ ($1 ?? $z0 x $1.chars !! '') });
    $a ~~ s/^$z0+//;
    $a
}

# post decrement
sub postfix:<--z>($a is rw) {
    $a.=subst(/$z1($z0*)$/,
      -> $/ {$z0 ~ "$z1$z0" x $0.chars div 2 ~ $z1 x $0.chars mod 2});
    $a ~~ s/^$z0+(.+)$/$0/;
    $a
}

# addition
sub infix:<+z>($a is copy, $b is copy) { $a++z; $a++z while $b--z nez $z0; $a };

# subtraction
sub infix:<-z>($a is copy, $b is copy) { $a--z; $a--z while $b--z nez $z0; $a };

# multiplication
sub infix:<*z>($a, $b) {
    return $z0 if $a eqz $z0 or $b eqz $z0;
    return $a if $b eqz $z1;
    return $b if $a eqz $z1;
    my $c = $a;
    my $d = $z1;
    repeat {
         my $e = $z0;
         repeat { $c++z; $e++z } until $e eqz $a;
         $d++z;
    } until $d eqz $b;
    $c
};

# division  (really more of a div mod)
sub infix:</z>($a is copy, $b is copy) {
    fail "Divide by zero" if $b eqz $z0;
    return $a if $a eqz $z0 or $b eqz $z1;
    my $c = $z0;
    repeat {
        my $d = $b +z ($z1 ~ $z0);
        $c++z;
        $a++z;
        $a--z while $d--z nez $z0
    } until $a ltz $b;
    $c ~= " remainder $a" if $a nez $z0;
    $c
};

###################### Testing ######################

# helper sub to translate constants into the particular glyphs you used
sub z($a) { $a.trans([<1 0>] => [$z1, $z0]) };

say "Using the glyph '$z1' for 1 and '$z0' for 0\n";

my $fmt = "%-22s = %15s  %s\n";

my $zeck = $z1;

printf( $fmt, "$zeck++z", $zeck++z, '# increment' ) for 1 .. 10;

printf $fmt, "$zeck +z {z('1010')}", $zeck +z= z('1010'), '# addition';

printf $fmt, "$zeck -z {z('100')}", $zeck -z= z('100'), '# subtraction';

printf $fmt, "$zeck *z {z('100101')}", $zeck *z= z('100101'), '# multiplication';

printf $fmt, "$zeck /z {z('100')}", $zeck /z= z('100'), '# division';

printf( $fmt, "$zeck--z", $zeck--z, '# decrement' ) for 1 .. 5;

printf $fmt, "$zeck *z {z('101001')}", $zeck *z= z('101001'), '# multiplication';

printf $fmt, "$zeck /z {z('100')}", $zeck /z= z('100'), '# division';
```


'''Testing Output'''

```txt

Using the glyph '1' for 1 and '0' for 0

1++z                   =              10  # increment
10++z                  =             100  # increment
100++z                 =             101  # increment
101++z                 =            1000  # increment
1000++z                =            1001  # increment
1001++z                =            1010  # increment
1010++z                =           10000  # increment
10000++z               =           10001  # increment
10001++z               =           10010  # increment
10010++z               =           10100  # increment
10100 +z 1010          =          101000  # addition
101000 -z 100          =          100010  # subtraction
100010 *z 100101       =    100001000001  # multiplication
100001000001 /z 100    =       101010001  # division
101010001--z           =       101010000  # decrement
101010000--z           =       101001010  # decrement
101001010--z           =       101001001  # decrement
101001001--z           =       101001000  # decrement
101001000--z           =       101000101  # decrement
101000101 *z 101001    = 101010000010101  # multiplication
101010000010101 /z 100 = 1001010001001 remainder 10  # division
```

Output using 'X' for 1 and 'O' for 0:

```txt

Using the glyph 'X' for 1 and 'O' for 0

X++z                   =              XO  # increment
XO++z                  =             XOO  # increment
XOO++z                 =             XOX  # increment
XOX++z                 =            XOOO  # increment
XOOO++z                =            XOOX  # increment
XOOX++z                =            XOXO  # increment
XOXO++z                =           XOOOO  # increment
XOOOO++z               =           XOOOX  # increment
XOOOX++z               =           XOOXO  # increment
XOOXO++z               =           XOXOO  # increment
XOXOO +z XOXO          =          XOXOOO  # addition
XOXOOO -z XOO          =          XOOOXO  # subtraction
XOOOXO *z XOOXOX       =    XOOOOXOOOOOX  # multiplication
XOOOOXOOOOOX /z XOO    =       XOXOXOOOX  # division
XOXOXOOOX--z           =       XOXOXOOOO  # decrement
XOXOXOOOO--z           =       XOXOOXOXO  # decrement
XOXOOXOXO--z           =       XOXOOXOOX  # decrement
XOXOOXOOX--z           =       XOXOOXOOO  # decrement
XOXOOXOOO--z           =       XOXOOOXOX  # decrement
XOXOOOXOX *z XOXOOX    = XOXOXOOOOOXOXOX  # multiplication
XOXOXOOOOOXOXOX /z XOO = XOOXOXOOOXOOX remainder XO  # division
```



## Phix

Uses a binary representation of Zeckendorf numbers, eg decimal 11 is stored as 0b10100, ie meaning 8+3, but actually 20 in decimal.

As such, they can be directly compared using the standard comparison operators, and printed quite trivially just by using the %b format.

They are however (and not all that surprisingly) pulled apart into individual bits for addition/subtraction, etc.

Does not handle negative numbers or anything >139583862445 (-ve probably doable but messy, >1.4e12 requires a total rewrite, probably using string representation).

```Phix
sequence fib = {1,1}

function zeckendorf(atom n)
-- Same as [[Zeckendorf_number_representation#Phix]]
atom r = 0
    while fib[$]<n do
        fib &= fib[$] + fib[$-1]
    end while
    integer k = length(fib)
    while k>2 and n<fib[k] do
        k -= 1
    end while
    for i=k to 2 by -1 do
        integer c = n>=fib[i]
        r += r+c
        n -= c*fib[i]
    end for
    return r
end function

function decimal(object z)
-- Convert Zeckendorf number(s) to decimal
atom dec = 0, bit = 2
    if sequence(z) then
        for i=1 to length(z) do
            z[i] = decimal(z[i])
        end for
        return z
    end if
    while z do
        if and_bits(z,1) then
            dec += fib[bit]
        end if
        bit += 1
        if bit>length(fib) then
            fib &= fib[$] + fib[$-1]
        end if
        z = floor(z/2)
    end while
    return dec
end function

function to_bits(integer x)
-- Simplified copy of int_to_bits(), but in reverse order,
-- and +ve only but (also only) as many bits as needed, and
-- ensures there are *two* trailing 0 (most significant)
    sequence bits = {}
    if x<0 then ?9/0 end if     -- sanity/avoid infinite loop
    while 1 do
        bits &= remainder(x,2)
        if x=0 then exit end if
        x = floor(x/2)
    end while
    bits &= 0 -- (since eg 101+101 -> 10000)
    return bits
end function

function to_bits2(integer a,b)
-- Apply to_bits() to a and b, and pad to the same length
    sequence sa = to_bits(a), sb = to_bits(b)
    integer diff = length(sa)-length(sb)
    if diff!=0 then
        if diff<0 then  sa &= repeat(0,-diff)
                  else  sb &= repeat(0,+diff)
        end if
    end if
    return {sa,sb}
end function

function to_int(sequence bits)
-- Copy of bits_to_int(), but in reverse order (lsb last)
    atom val = 0, p = 1
    for i=length(bits) to 1 by -1 do
        if bits[i] then
            val += p
        end if
        p += p
    end for
    return val
end function

function zstr(object z)
    if sequence(z) then
        for i=1 to length(z) do
            z[i] = zstr(z[i])
        end for
        return z
    end if
    return sprintf("%b",z)
end function

function rep(sequence res, integer ds, sequence was, wth)
-- helper for cleanup, validates replacements
    integer de = ds+length(was)-1
    if res[ds..de]!=was then ?9/0 end if
    if length(was)!=length(wth) then ?9/0 end if
    res[ds..de] = wth
    return res
end function

function zcleanup(sequence res)
-- (shared by zadd and zsub)
    integer l = length(res)
    -- first stage, left to right, {020x -> 100x', 030x -> 110x', 021x->110x, 012x->101x}
    for i=1 to l-3 do
        switch res[i..i+2]
            case {0,2,0}:   res[i..i+2] = {1,0,0}   res[i+3] += 1
            case {0,3,0}:   res[i..i+2] = {1,1,0}   res[i+3] += 1
            case {0,2,1}:   res[i..i+2] = {1,1,0}
            case {0,1,2}:   res[i..i+2] = {1,0,1}
        end switch
    end for
    -- first stage cleanup
    if l>1 then
        if res[l-1]=3 then      res = rep(res,l-2,{0,3,0},{1,1,1})      -- 030 -> 111
        elsif res[l-1]=2 then
            if res[l-2]=0 then  res = rep(res,l-2,{0,2,0},{1,0,1})      -- 020 -> 101
                          else  res = rep(res,l-3,{0,1,2,0},{1,0,1,0})  -- 0120 -> 1010
            end if
        end if
    end if
    if res[l]=3 then            res = rep(res,l-1,{0,3},{1,1})          -- 03 -> 11
    elsif res[l]=2 then
        if res[l-1]=0 then      res = rep(res,l-1,{0,2},{1,0})          -- 02 -> 10
                      else      res = rep(res,l-2,{0,1,2},{1,0,1})      -- 012 -> 101
        end if
    end if
    -- second stage, pass 1, right to left, 011 -> 100
    for i=length(res)-2 to 1 by -1 do
        if res[i..i+2]={0,1,1} then res[i..i+2] = {1,0,0} end if
    end for
    -- second stage, pass 2, left to right, 011 -> 100
    for i=1 to length(res)-2 do
        if res[i..i+2]={0,1,1} then res[i..i+2] = {1,0,0} end if
    end for
    return to_int(res)
end function

function zadd(integer a, b)
    sequence {sa,sb} = to_bits2(a,b)
    return zcleanup(reverse(sq_add(sa,sb)))
end function

function zinc(integer a)
    return zadd(a,0b1)
end function

function zsub(integer a, b)
    sequence {sa,sb} = to_bits2(a,b)
    sequence res = reverse(sq_sub(sa,sb))
    -- (/not/ combined with the first pass of the add routine!)
    for i=1 to length(res)-2 do
        switch res[i..i+2] do
            case {1, 0, 0}: res[i..i+2] = {0,1,1}
            case {1,-1, 0}: res[i..i+2] = {0,0,1}
            case {1,-1, 1}: res[i..i+2] = {0,0,2}
            case {1, 0,-1}: res[i..i+2] = {0,1,0}
            case {2, 0, 0}: res[i..i+2] = {1,1,1}
            case {2,-1, 0}: res[i..i+2] = {1,0,1}
            case {2,-1, 1}: res[i..i+2] = {1,0,2}
            case {2, 0,-1}: res[i..i+2] = {1,1,0}
        end switch
    end for
    -- copied from PicoLisp: {1,-1} -> {0,1} and {2,-1} -> {1,1}
    for i=1 to length(res)-1 do
        switch res[i..i+1] do
            case {1,-1}: res[i..i+1] = {0,1}
            case {2,-1}: res[i..i+1] = {1,1}
        end switch
    end for
    if find(-1,res) then ?9/0 end if -- sanity check
    return zcleanup(res)
end function

function zdec(integer a)
    return zsub(a,0b1)
end function

function zmul(integer a, b)
integer res = 0
    sequence mult = {a,zadd(a,a)}   -- (as per task desc)
    integer bits = 2
    while bits<b do
        mult = append(mult,zadd(mult[$],mult[$-1]))
        bits *= 2
    end while
    integer bit = 1
    while b do
        if and_bits(b,1) then
            res = zadd(res,mult[bit])
        end if
        b = floor(b/2)
        bit += 1
    end while
    return res
end function

function zdiv(integer a, b)
integer res = 0
    sequence mult = {b,zadd(b,b)}
    integer bits = 2
    while mult[$]<a do
        mult = append(mult,zadd(mult[$],mult[$-1]))
        bits *= 2
    end while
    for i=length(mult) to 1 by -1 do
        integer mi = mult[i]
        if mi<=a then
            res = zadd(res,bits)
            a = zsub(a,mi)
            if a=0 then exit end if
        end if
        bits = floor(bits/2)
    end for
    return {res,a} -- (a is the remainder)
end function

for i=0 to 20 do
    integer zi = zeckendorf(i)
    atom d = decimal(zi)
    printf(1,"%2d: %7b (%d)\n",{i,zi,d})
end for

procedure test(atom a, string op, atom b, object res, string expected)
    string zres = iff(atom(res)?zstr(res):join(zstr(res)," rem ")),
           dres = sprintf(iff(atom(res)?"%d":"%d rem %d"),decimal(res)),
           aka = sprintf("aka %d %s %d = %s",{decimal(a),op,decimal(b),dres}),
           ok = iff(zres=expected?"":" *** ERROR ***!!")
    printf(1,"%s %s %s = %s, %s %s\n",{zstr(a),op,zstr(b),zres,aka,ok})
end procedure

test(0b0,"+",0b0,zadd(0b0,0b0),"0")
test(0b101,"+",0b101,zadd(0b101,0b101),"10000")
test(0b10100,"-",0b1000,zsub(0b10100,0b1000),"1001")
test(0b100100,"-",0b1000,zsub(0b100100,0b1000),"10100")
test(0b1001,"*",0b101,zmul(0b1001,0b101),"1000100")
test(0b1000101,"/",0b101,zdiv(0b1000101,0b101),"1001 rem 1")

test(0b10,"+",0b10,zadd(0b10,0b10),"101")
test(0b101,"+",0b10,zadd(0b101,0b10),"1001")
test(0b1001,"+",0b1001,zadd(0b1001,0b1001),"10101")
test(0b10101,"+",0b1000,zadd(0b10101,0b1000),"100101")
test(0b100101,"+",0b10101,zadd(0b100101,0b10101),"1010000")
test(0b1000,"-",0b101,zsub(0b1000,0b101),"1")
test(0b10101010,"-",0b1010101,zsub(0b10101010,0b1010101),"1000000")
test(0b1001,"*",0b101,zmul(0b1001,0b101),"1000100")
test(0b101010,"+",0b101,zadd(0b101010,0b101),"1000100")

test(0b10100,"+",0b1010,zadd(0b10100,0b1010),"101000")
test(0b101000,"-",0b1010,zsub(0b101000,0b1010),"10100")

test(0b100010,"*",0b100101,zmul(0b100010,0b100101),"100001000001")
test(0b100001000001,"/",0b100,zdiv(0b100001000001,0b100),"101010001 rem 0")
test(0b101000101,"*",0b101001,zmul(0b101000101,0b101001),"101010000010101")
test(0b101010000010101,"/",0b100,zdiv(0b101010000010101,0b100),"1001010001001 rem 10")

test(0b10100010010100,"+",0b1001000001,zadd(0b10100010010100,0b1001000001),"100000000010101")
test(0b10100010010100,"-",0b1001000001,zsub(0b10100010010100,0b1001000001),"10010001000010")
test(0b10000,"*",0b1001000001,zmul(0b10000,0b1001000001),"10100010010100")
test(0b1010001010000001001,"/",0b100000000100000,zdiv(0b1010001010000001001,0b100000000100000),"10001 rem 10100001010101")

test(0b10100,"+",0b1010,zadd(0b10100,0b1010),"101000")
test(0b10100,"-",0b1010,zsub(0b10100,0b1010),"101")
test(0b10100,"*",0b1010,zmul(0b10100,0b1010),"101000001")
test(0b10100,"/",0b1010,zdiv(0b10100,0b1010),"1 rem 101")
integer m = zmul(0b10100,0b1010)
test(m,"/",0b1010,zdiv(m,0b1010),"10100 rem 0")
```

{{out}}

```txt

 0:       0 (0)
 1:       1 (1)
 2:      10 (2)
 3:     100 (3)
 4:     101 (4)
 5:    1000 (5)
 6:    1001 (6)
 7:    1010 (7)
 8:   10000 (8)
 9:   10001 (9)
10:   10010 (10)
11:   10100 (11)
12:   10101 (12)
13:  100000 (13)
14:  100001 (14)
15:  100010 (15)
16:  100100 (16)
17:  100101 (17)
18:  101000 (18)
19:  101001 (19)
20:  101010 (20)
0 + 0 = 0, aka 0 + 0 = 0
101 + 101 = 10000, aka 4 + 4 = 8
10100 - 1000 = 1001, aka 11 - 5 = 6
100100 - 1000 = 10100, aka 16 - 5 = 11
1001 * 101 = 1000100, aka 6 * 4 = 24
1000101 / 101 = 1001 rem 1, aka 25 / 4 = 6 rem 1
10 + 10 = 101, aka 2 + 2 = 4
101 + 10 = 1001, aka 4 + 2 = 6
1001 + 1001 = 10101, aka 6 + 6 = 12
10101 + 1000 = 100101, aka 12 + 5 = 17
100101 + 10101 = 1010000, aka 17 + 12 = 29
1000 - 101 = 1, aka 5 - 4 = 1
10101010 - 1010101 = 1000000, aka 54 - 33 = 21
1001 * 101 = 1000100, aka 6 * 4 = 24
101010 + 101 = 1000100, aka 20 + 4 = 24
10100 + 1010 = 101000, aka 11 + 7 = 18
101000 - 1010 = 10100, aka 18 - 7 = 11
100010 * 100101 = 100001000001, aka 15 * 17 = 255
100001000001 / 100 = 101010001 rem 0, aka 255 / 3 = 85 rem 0
101000101 * 101001 = 101010000010101, aka 80 * 19 = 1520
101010000010101 / 100 = 1001010001001 rem 10, aka 1520 / 3 = 506 rem 2
10100010010100 + 1001000001 = 100000000010101, aka 888 + 111 = 999
10100010010100 - 1001000001 = 10010001000010, aka 888 - 111 = 777
10000 * 1001000001 = 10100010010100, aka 8 * 111 = 888
1010001010000001001 / 100000000100000 = 10001 rem 10100001010101, aka 9876 / 1000 = 9 rem 876
10100 + 1010 = 101000, aka 11 + 7 = 18
10100 - 1010 = 101, aka 11 - 7 = 4
10100 * 1010 = 101000001, aka 11 * 7 = 77
10100 / 1010 = 1 rem 101, aka 11 / 7 = 1 rem 4
101000001 / 1010 = 10100 rem 0, aka 77 / 7 = 11 rem 0

```



## PicoLisp

<lang>(seed (in "/dev/urandom" (rd 8)))

(de unpad (Lst)
   (while (=0 (car Lst))
      (pop 'Lst) )
   Lst )

(de numz (N)
   (let Fibs (1 1)
      (while (>= N (+ (car Fibs) (cadr Fibs)))
         (push 'Fibs (+ (car Fibs) (cadr Fibs))) )
      (make
         (for I (uniq Fibs)
            (if (> I N)
               (link 0)
               (link 1)
               (dec 'N I) ) ) ) ) )

(de znum (Lst)
   (let Fibs (1 1)
      (do (dec (length Lst))
         (push 'Fibs (+ (car Fibs) (cadr Fibs))) )
      (sum
         '((X Y) (unless (=0 X) Y))
         Lst
         (uniq Fibs) ) ) )

(de incz (Lst)
   (addz Lst (1)) )

(de decz (Lst)
   (subz Lst (1)) )

(de addz (Lst1 Lst2)
   (let Max (max (length Lst1) (length Lst2))
      (reorg
         (mapcar + (need Max Lst1 0) (need Max Lst2 0)) ) ) )

(de subz (Lst1 Lst2)
   (use (@A @B)
      (let
         (Max (max (length Lst1) (length Lst2))
            Lst (mapcar - (need Max Lst1 0) (need Max Lst2 0)) )
         (loop
            (while (match '(@A 1 0 0 @B) Lst)
               (setq Lst (append @A (0 1 1) @B)) )
            (while (match '(@A 1 -1 0 @B) Lst)
               (setq Lst (append @A (0 0 1) @B)) )
            (while (match '(@A 1 -1 1 @B) Lst)
               (setq Lst (append @A (0 0 2) @B)) )
            (while (match '(@A 1 0 -1 @B) Lst)
               (setq Lst (append @A (0 1 0) @B)) )
            (while (match '(@A 2 0 0 @B) Lst)
               (setq Lst (append @A (1 1 1) @B)) )
            (while (match '(@A 2 -1 0 @B) Lst)
               (setq Lst (append @A (1 0 1) @B)) )
            (while (match '(@A 2 -1 1 @B) Lst)
               (setq Lst (append @A (1 0 2) @B)) )
            (while (match '(@A 2 0 -1 @B) Lst)
               (setq Lst (append @A (1 1 0) @B)) )
            (while (match '(@A 1 -1) Lst)
               (setq Lst (append @A (0 1))) )
            (while (match '(@A 2 -1) Lst)
               (setq Lst (append @A (1 1))) )
            (NIL (match '(@A -1 @B) Lst)) )
         (reorg (unpad Lst)) ) ) )

(de mulz (Lst1 Lst2)
   (let (Sums (list Lst1) Mulz (0))
      (mapc
         '((X)
            (when (= 1 (car X))
               (setq Mulz (addz (cdr X) Mulz)) )
            Mulz )
         (mapcar
            '((X)
               (cons
                  X
                  (push 'Sums (addz (car Sums) (cadr Sums))) ) )
            (reverse Lst2) ) ) ) )

(de divz (Lst1 Lst2)
   (let Q 0
      (while (lez Lst2 Lst1)
         (setq Lst1 (subz Lst1 Lst2))
         (setq Q (incz Q)) )
      (list Q (or Lst1 (0))) ) )

(de reorg (Lst)
   (use (@A @B)
      (let Lst (reverse Lst)
         (loop
            (while (match '(@A 1 1 @B) Lst)
               (if @B
                  (inc (nth @B 1))
                  (setq @B (1)) )
               (setq Lst (append @A (0 0) @B) ) )
            (while (match '(@A 2 @B) Lst)
               (inc
                  (if (cdr @A)
                     (tail 2 @A)
                     @A ) )
               (if @B
                  (inc (nth @B 1))
                  (setq @B (1)) )
               (setq Lst (append @A (0) @B)) )
            (NIL
               (or
                  (match '(@A 1 1 @B) Lst)
                  (match '(@A 2 @B) Lst) ) ) )
         (reverse Lst) ) ) )

(de lez (Lst1 Lst2)
   (let Max (max (length Lst1) (length Lst2))
      (<= (need Max Lst1 0) (need Max Lst2 0)) ) )

(let (X 0 Y 0)
   (do 1024
      (setq X (rand 1 1024))
      (setq Y (rand 1 1024))
      (test (numz (+ X Y)) (addz (numz X) (numz Y)))
      (test (numz (* X Y)) (mulz (numz X) (numz Y)))
      (test (numz (+ X 1)) (incz (numz X))) )

   (do 1024
      (setq X (rand 129 1024))
      (setq Y (rand 1 128))
      (test (numz (- X Y)) (subz (numz X) (numz Y)))
      (test (numz (/ X Y)) (car (divz (numz X) (numz Y))))
      (test (numz (% X Y)) (cadr (divz (numz X) (numz Y))))
      (test (numz (- X 1)) (decz (numz X))) ) )

(bye)
```



## Python


```python
import copy

class Zeckendorf:
    def __init__(self, x='0'):
        q = 1
        i = len(x) - 1
        self.dLen = int(i / 2)
        self.dVal = 0
        while i >= 0:
            self.dVal = self.dVal + (ord(x[i]) - ord('0')) * q
            q = q * 2
            i = i -1

    def a(self, n):
        i = n
        while True:
            if self.dLen < i:
                self.dLen = i
            j = (self.dVal >> (i * 2)) & 3
            if j == 0 or j == 1:
                return
            if j == 2:
                if (self.dVal >> ((i + 1) * 2) & 1) != 1:
                    return
                self.dVal = self.dVal + (1 << (i * 2 + 1))
                return
            if j == 3:
                temp = 3 << (i * 2)
                temp = temp ^ -1
                self.dVal = self.dVal & temp
                self.b((i + 1) * 2)
            i = i + 1

    def b(self, pos):
        if pos == 0:
            self.inc()
            return
        if (self.dVal >> pos) & 1 == 0:
            self.dVal = self.dVal + (1 << pos)
            self.a(int(pos / 2))
            if pos > 1:
                self.a(int(pos / 2) - 1)
        else:
            temp = 1 << pos
            temp = temp ^ -1
            self.dVal = self.dVal & temp
            self.b(pos + 1)
            self.b(pos - (2 if pos > 1 else 1))

    def c(self, pos):
        if (self.dVal >> pos) & 1 == 1:
            temp = 1 << pos
            temp = temp ^ -1
            self.dVal = self.dVal & temp
            return
        self.c(pos + 1)
        if pos > 0:
            self.b(pos - 1)
        else:
            self.inc()

    def inc(self):
        self.dVal = self.dVal + 1
        self.a(0)

    def __add__(self, rhs):
        copy = self
        rhs_dVal = rhs.dVal
        limit = (rhs.dLen + 1) * 2
        for gn in range(0, limit):
            if ((rhs_dVal >> gn) & 1) == 1:
                copy.b(gn)
        return copy

    def __sub__(self, rhs):
        copy = self
        rhs_dVal = rhs.dVal
        limit = (rhs.dLen + 1) * 2
        for gn in range(0, limit):
            if (rhs_dVal >> gn) & 1 == 1:
                copy.c(gn)
        while (((copy.dVal >> ((copy.dLen * 2) & 31)) & 3) == 0) or (copy.dLen == 0):
            copy.dLen = copy.dLen - 1
        return copy

    def __mul__(self, rhs):
        na = copy.deepcopy(rhs)
        nb = copy.deepcopy(rhs)
        nr = Zeckendorf()
        dVal = self.dVal
        for i in range(0, (self.dLen + 1) * 2):
            if ((dVal >> i) & 1) > 0:
                nr = nr + nb
            nt = copy.deepcopy(nb)
            nb = nb + na
            na = copy.deepcopy(nt)
        return nr

    def __str__(self):
        dig = ["00", "01", "10"]
        dig1 = ["", "1", "10"]

        if self.dVal == 0:
            return '0'
        idx = (self.dVal >> ((self.dLen * 2) & 31)) & 3
        sb = dig1[idx]
        i = self.dLen - 1
        while i >= 0:
            idx = (self.dVal >> (i * 2)) & 3
            sb = sb + dig[idx]
            i = i - 1
        return sb

# main
print "Addition:"
g = Zeckendorf("10")
g = g + Zeckendorf("10")
print g
g = g + Zeckendorf("10")
print g
g = g + Zeckendorf("1001")
print g
g = g + Zeckendorf("1000")
print g
g = g + Zeckendorf("10101")
print g
print

print "Subtraction:"
g = Zeckendorf("1000")
g = g - Zeckendorf("101")
print g
g = Zeckendorf("10101010")
g = g - Zeckendorf("1010101")
print g
print

print "Multiplication:"
g = Zeckendorf("1001")
g = g * Zeckendorf("101")
print g
g = Zeckendorf("101010")
g = g + Zeckendorf("101")
print g
```

{{out}}

```txt
Addition:
101
1001
10101
100101
1010000

Subtraction:
1
1000000

Multiplication:
1000100
1000100
```



## Racket

This implementation only handles natural (non-negative numbers).  The algorithms for addition and subtraction use the techniques explained in the paper "Efficient algorithms for Zeckendorf arithmetic" (http://arxiv.org/pdf/1207.4497.pdf).


```racket
#lang racket (require math)

(define sqrt5 (sqrt 5))
(define phi (* 0.5 (+ 1 sqrt5)))

;; What is the nth fibonnaci number, shifted by 2 so that
;; F(0) = 1, F(1) = 2, ...?
;;
(define (F n)
  (fibonacci (+ n 2)))

;; What is the largest n such that F(n) <= m?
;;
(define (F* m)
  (let ([n (- (inexact->exact (round (/ (log (* m sqrt5)) (log phi)))) 2)])
    (if (<= (F n) m) n (sub1 n))))

(define (zeck->natural z)
  (for/sum ([i (reverse z)]
            [j (in-naturals)])
    (* i (F j))))

(define (natural->zeck n)
  (if (zero? n)
      null
      (for/list ([i (in-range (F* n) -1 -1)])
        (let ([f (F i)])
          (cond [(>= n f) (set! n (- n f))
                          1]
                [else 0])))))

; Extend list to the right to a length of len with repeated padding elements
;
(define (pad lst len [padding 0])
  (append lst (make-list (- len (length lst)) padding)))

; Strip padding elements from the left of the list
;
(define (unpad lst [padding 0])
  (cond [(null? lst) lst]
        [(equal? (first lst) padding) (unpad (rest lst) padding)]
        [else lst]))

;; Run a filter function across a window in a list from left to right
;;
(define (left->right width fn)
  (λ (lst)
    (let F ([a lst])
      (if (< (length a) width)
          a
          (let ([f (fn (take a width))])
            (cons (first f) (F (append (rest f) (drop a width)))))))))

;; Run a function fn across a window in a list from right to left
;;
(define (right->left width fn)
  (λ (lst)
    (let F ([a lst])
      (if (< (length a) width)
          a
          (let ([f (fn (take-right a width))])
            (append (F (append (drop-right a width) (drop-right f 1)))
                    (list (last f))))))))

;; (a0 a1 a2 ... an) -> (a0 a1 a2 ... (fn ... an))
;;
(define (replace-tail width fn)
  (λ (lst)
    (append (drop-right lst width) (fn (take-right lst width)))))

(define (rule-a lst)
  (match lst
    [(list 0 2 0 x) (list 1 0 0 (add1 x))]
    [(list 0 3 0 x) (list 1 1 0 (add1 x))]
    [(list 0 2 1 x) (list 1 1 0 x)]
    [(list 0 1 2 x) (list 1 0 1 x)]
    [else lst]))

(define (rule-a-tail lst)
  (match lst
    [(list x 0 3 0) (list x 1 1 1)]
    [(list x 0 2 0) (list x 1 0 1)]
    [(list 0 1 2 0) (list 1 0 1 0)]
    [(list x y 0 3) (list x y 1 1)]
    [(list x y 0 2) (list x y 1 0)]
    [(list x 0 1 2) (list x 1 0 0)]
    [else lst]))

(define (rule-b lst)
  (match lst
    [(list 0 1 1) (list 1 0 0)]
    [else lst]))

(define (rule-c lst)
  (match lst
    [(list 1 0 0) (list 0 1 1)]
    [(list 1 -1 0) (list 0 0 1)]
    [(list 1 -1 1) (list 0 0 2)]
    [(list 1 0 -1) (list 0 1 0)]
    [(list 2 0 0) (list 1 1 1)]
    [(list 2 -1 0) (list 1 0 1)]
    [(list 2 -1 1) (list 1 0 2)]
    [(list 2 0 -1) (list 1 1 0)]
    [else lst]))

(define (zeck-combine op y z [f identity])
  (let* ([bits (max (add1 (length y)) (add1 (length z)) 4)]
         [f0 (λ (x) (pad (reverse x) bits))]
         [f1 (left->right 4 rule-a)]
         [f2 (replace-tail 4 rule-a-tail)]
         [f3 (right->left 3 rule-b)]
         [f4 (left->right 3 rule-b)])
    ((compose1 unpad f4 f3 f2 f1 f reverse) (map op (f0 y) (f0 z)))))

(define (zeck+ y z)
  (zeck-combine + y z))

(define (zeck- y z)
  (when (zeck< y z) (error (format "~a" `(zeck-: cannot subtract since ,y < ,z))))
  (zeck-combine - y z (left->right 3 rule-c)))

(define (zeck* y z)
  (define (M ry Zn Zn_1 [acc null])
    (if (null? ry)
        acc
        (M (rest ry) (zeck+ Zn Zn_1) Zn
           (if (zero? (first ry)) acc (zeck+ acc Zn)))))
  (cond [(zeck< z y) (zeck* z y)]
        [(null? y) null]               ; 0 * z -> 0
        [else (M (reverse y) z z)]))

(define (zeck-quotient/remainder y z)
  (define (M Zn acc)
    (if (zeck< y Zn)
        (drop-right acc 1)
        (M (zeck+ Zn (first acc)) (cons Zn acc))))
  (define (D x m [acc null])
    (if (null? m)
        (values (reverse acc) x)
        (let* ([v (first m)]
               [smaller (zeck< v x)]
               [bit (if smaller 1 0)]
               [x_ (if smaller (zeck- x v) x)])
          (D x_ (rest m) (cons bit acc)))))
  (D y (M z (list z))))

(define (zeck-quotient y z)
  (let-values ([(quotient _) (zeck-quotient/remainder y z)])
    quotient))

(define (zeck-remainder y z)
  (let-values ([(_ remainder) (zeck-quotient/remainder y z)])
    remainder))

(define (zeck-add1 z)
  (zeck+ z '(1)))

(define (zeck= y z)
  (equal? (unpad y) (unpad z)))

(define (zeck< y z)
  ; Compare equal-length unpadded zecks
  (define (LT a b)
    (if (null? a)
        #f
        (let ([a0 (first a)] [b0 (first b)])
          (if (= a0 b0)
              (LT (rest a) (rest b))
              (= a0 0)))))

  (let* ([a (unpad y)] [len-a (length a)]
         [b (unpad z)] [len-b (length b)])
    (cond [(< len-a len-b) #t]
          [(> len-a len-b) #f]
          [else (LT a b)])))

(define (zeck> y z)
  (not (or (zeck= y z) (zeck< y z))))


;; Examples
;;
(define (example op-name op a b)
  (let* ([y (natural->zeck a)]
         [z (natural->zeck b)]
         [x (op y z)]
         [c (zeck->natural x)])
    (printf "~a ~a ~a = ~a ~a ~a = ~a = ~a\n"
            a op-name b y op-name z x c)))

(example '+ zeck+ 888 111)
(example '- zeck- 888 111)
(example '* zeck* 8 111)
(example '/ zeck-quotient 9876 1000)
(example '% zeck-remainder 9876 1000)

```


{{output}}


```txt
888 + 111 = (1 0 1 0 0 0 1 0 0 1 0 1 0 0) + (1 0 0 1 0 0 0 0 0 1) = (1 0 0 0 0 0 0 0 0 0 1 0 1 0 1) = 999
888 - 111 = (1 0 1 0 0 0 1 0 0 1 0 1 0 0) - (1 0 0 1 0 0 0 0 0 1) = (1 0 0 1 0 0 0 1 0 0 0 0 1 0) = 777
8 * 111 = (1 0 0 0 0) * (1 0 0 1 0 0 0 0 0 1) = (1 0 1 0 0 0 1 0 0 1 0 1 0 0) = 888
9876 / 1000 = (1 0 1 0 0 0 1 0 1 0 0 0 0 0 0 1 0 0 1) / (1 0 0 0 0 0 0 0 0 1 0 0 0 0 0) = (1 0 0 0 1) = 9
9876 % 1000 = (1 0 1 0 0 0 1 0 1 0 0 0 0 0 0 1 0 0 1) % (1 0 0 0 0 0 0 0 0 1 0 0 0 0 0) = (1 0 1 0 0 0 0 1 0 1 0 1 0 1) = 876

```



## Scala

{{works with|Scala|2.9.1}}
The addition is an implementation of an algorithm suggested in http[:]//arxiv.org/pdf/1207.4497.pdf: Efficient Algorithms for Zeckendorf Arithmetic.

```Scala
object ZA extends App {
  import Stream._
  import scala.collection.mutable.ListBuffer

object Z {
  // only for comfort and result checking:
  val fibs: Stream[BigInt] = {def series(i:BigInt,j:BigInt):Stream[BigInt] = i #:: series(j,i+j); series(1,0).tail.tail.tail }
  val z2i: Z => BigInt = z => (z.z.abs.toString.map(_.asDigit).reverse.zipWithIndex.map{case (v,i)=>v*fibs(i)}:\BigInt(0))(_+_)*z.z.signum

  var fmts = Map(Z("0")->List[Z](Z("0")))   //map of Fibonacci multiples table of divisors

  // get multiply table from fmts
  def mt(z: Z): List[Z] = {fmts.getOrElse(z,Nil) match {case Nil => {val e = mwv(z); fmts=fmts+(z->e); e}; case l => l}}

  // multiply weight vector
  def mwv(z: Z): List[Z] = {
    val wv = new ListBuffer[Z]; wv += z; wv += (z+z)
    var zs = "11"; val upper = z.z.abs.toString
    while ((zs.size<upper.size)) {wv += (wv.toList.last + wv.toList.reverse.tail.head); zs = "1"+zs}
    wv.toList
  }

  // get division table (division weight vector)
  def dt(dd: Z, ds: Z): List[Z] = {
    val wv = new ListBuffer[Z]; mt(ds).copyToBuffer(wv)
    var zs = ds.z.abs.toString; val upper = dd.z.abs.toString
    while ((zs.size<upper.size)) {wv += (wv.toList.last + wv.toList.reverse.tail.head); zs = "1"+zs}
    wv.toList
  }
}

case class Z(var zs: String) {
  import Z._
  require ((zs.toSet--Set('-','0','1')==Set()) && (!zs.contains("11")))

  var z: BigInt = BigInt(zs)
  override def toString = z+"Z(i:"+z2i(this)+")"
  def size = z.abs.toString.size

  //--- fa(summand1.z,summand2.z) --------------------------
  val fa: (BigInt,BigInt) => BigInt = (z1, z2) => {
    val v =z1.toString.map(_.asDigit).reverse.padTo(5,0).zipAll(z2.toString.map(_.asDigit).reverse, 0, 0)
    val arr1 = (v.map(p=>p._1+p._2):+0 reverse).toArray
    (0 to arr1.size-4) foreach {i=>     //stage1
      val a = arr1.slice(i,i+4).toList
      val b = (a:\"")(_+_) dropRight 1
      val a1 = b match {
          case "020" => List(1,0,0, a(3)+1)
          case "030" => List(1,1,0, a(3)+1)
          case "021" => List(1,1,0, a(3))
          case "012" => List(1,0,1, a(3))
          case _     => a
      }
      0 to 3 foreach {j=>arr1(j+i) = a1(j)}
    }
    val arr2 = (arr1:\"")(_+_)
      .replace("0120","1010").replace("030","111").replace("003","100").replace("020","101")
      .replace("003","100").replace("012","101").replace("021","110")
      .replace("02","10").replace("03","11")
      .reverse.toArray
    (0 to arr2.size-3) foreach {i=>     //stage2, step1
      val a = arr2.slice(i,i+3).toList
      val b = (a:\"")(_+_)
      val a1 = b match {
          case "110" => List('0','0','1')
          case _     => a
      }
      0 to 2 foreach {j=>arr2(j+i) = a1(j)}
    }
    val arr3 = (arr2:\"")(_+_).concat("0").reverse.toArray
    (0 to arr3.size-3) foreach {i=>     //stage2, step2
      val a = arr3.slice(i,i+3).toList
      val b = (a:\"")(_+_)
      val a1 = b match {
          case "011" => List('1','0','0')
          case _     => a
      }
      0 to 2 foreach {j=>arr3(j+i) = a1(j)}
    }
    BigInt((arr3:\"")(_+_))
  }

  //--- fs(minuend.z,subtrahend.z) -------------------------
  val fs: (BigInt,BigInt) => BigInt = (min,sub) => {
    val zmvr = min.toString.map(_.asDigit).reverse
    val zsvr = sub.toString.map(_.asDigit).reverse.padTo(zmvr.size,0)
    val v = zmvr.zipAll(zsvr, 0, 0).reverse
    val last = v.size-1
    val zma = zmvr.reverse.toArray; val zsa = zsvr.reverse.toArray
    for (i <- 0 to last reverse) {
      val e = zma(i)-zsa(i)
      if (e<0) {
        zma(i-1) = zma(i-1)-1
        zma(i) = 0
        val part = Z((((i to last).map(zma(_))):\"")(_+_))
        val carry = Z(("1".padTo(last-i,"0"):\"")(_+_))
        val sum = part + carry; val sums = sum.z.toString
        (1 to sum.size) foreach {j=>zma(last-sum.size+j)=sums(j-1).asDigit}
        if (zma(i-1)<0) {
          for (j <- 0 to i-1 reverse) {
            if (zma(j)<0) {
              zma(j-1) = zma(j-1)-1
              zma(j) = 0
              val part = Z((((j to last).map(zma(_))):\"")(_+_))
              val carry = Z(("1".padTo(last-j,"0"):\"")(_+_))
              val sum = part + carry; val sums = sum.z.toString
              (1 to sum.size) foreach {k=>zma(last-sum.size+k)=sums(k-1).asDigit}
            }
          }
        }
      }
      else zma(i) = e
      zsa(i) = 0
    }
    BigInt((zma:\"")(_+_))
  }

  //--- fm(multiplicand.z,multplier.z) ---------------------
  val fm: (BigInt,BigInt) => BigInt = (mc, mp) => {
    val mct = mt(Z(mc.toString))
    val mpxi = mp.toString.reverse.map(_.asDigit).zipWithIndex.filter(_._1 != 0).map(_._2)
    (mpxi:\Z("0"))((fi,sum)=>sum+mct(fi)).z
  }

  //--- fd(dividend.z,divisor.z) ---------------------------
  val fd: (BigInt,BigInt) => BigInt = (dd, ds) => {
    val dst = dt(Z(dd.toString),Z(ds.toString)).reverse
    var diff = Z(dd.toString)
    val zd = ListBuffer[String]()
    (0 to dst.size-1) foreach {i=>
      if (dst(i)>diff) zd+="0" else {diff = diff-dst(i); zd+="1"}
    }
    BigInt(zd.mkString)
  }

  val fasig: (Z, Z) => Int = (z1, z2) => if (z1.z.abs>z2.z.abs) z1.z.signum else z2.z.signum
  val fssig: (Z, Z) => Int = (z1, z2) =>
    if ((z1.z.abs>z2.z.abs && z1.z.signum>0)||(z1.z.abs<z2.z.abs && z1.z.signum<0)) 1 else -1

  def +(that: Z): Z =
    if (this==Z("0")) that
    else if (that==Z("0")) this
    else if (this.z.signum == that.z.signum) Z((fa(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*this.z.signum).toString)
    else if (this.z.abs == that.z.abs) Z("0")
    else Z((fs(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*fasig(this, that)).toString)

  def ++ : Z = {val za = this + Z("1"); this.zs = za.zs; this.z = za.z; this}

  def -(that: Z): Z =
    if (this==Z("0")) Z((that.z*(-1)).toString)
    else if (that==Z("0")) this
    else if (this.z.signum != that.z.signum) Z((fa(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*this.z.signum).toString)
    else if (this.z.abs == that.z.abs) Z("0")
    else Z((fs(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*fssig(this, that)).toString)

  def -- : Z = {val zs = this - Z("1"); this.zs = zs.zs; this.z = zs.z; this}

  def * (that: Z): Z =
    if (this==Z("0")||that==Z("0")) Z("0")
    else if (this==Z("1")) that
    else if (that==Z("1")) this
    else Z((fm(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*this.z.signum*that.z.signum).toString)

  def / (that: Z): Option[Z] =
    if (that==Z("0")) None
    else if (this==Z("0")) Some(Z("0"))
    else if (that==Z("1")) Some(Z("1"))
    else if (this.z.abs < that.z.abs) Some(Z("0"))
    else if (this.z == that.z) Some(Z("1"))
    else Some(Z((fd(this.z.abs.max(that.z.abs),this.z.abs.min(that.z.abs))*this.z.signum*that.z.signum).toString))

  def % (that: Z): Option[Z] =
    if (that==Z("0")) None
    else if (this==Z("0")) Some(Z("0"))
    else if (that==Z("1")) Some(Z("0"))
    else if (this.z.abs < that.z.abs) Some(this)
    else if (this.z == that.z) Some(Z("0") )
    else this/that match {case None => None; case Some(z) => Some(this-z*that)}

  def <  (that: Z): Boolean = this.z <  that.z
  def <= (that: Z): Boolean = this.z <= that.z
  def >  (that: Z): Boolean = this.z >  that.z
  def >= (that: Z): Boolean = this.z >= that.z

}

val elapsed: (=> Unit) => Long = f => {val s = System.currentTimeMillis; f; (System.currentTimeMillis - s)/1000}

val add:      (Z,Z) => Z = (z1,z2) => z1+z2
val subtract: (Z,Z) => Z = (z1,z2) => z1-z2
val multiply: (Z,Z) => Z = (z1,z2) => z1*z2
val divide:   (Z,Z) => Option[Z] = (z1,z2) => z1/z2
val modulo:   (Z,Z) => Option[Z] = (z1,z2) => z1%z2

val ops = Map(("+",add),("-",subtract),("*",multiply),("/",divide),("%",modulo))

val calcs = List(
  (Z("101"),"+",Z("10100"))
, (Z("101"),"-",Z("10100"))
, (Z("101"),"*",Z("10100"))
, (Z("101"),"/",Z("10100"))
, (Z("-1010101"),"+",Z("10100"))
, (Z("-1010101"),"-",Z("10100"))
, (Z("-1010101"),"*",Z("10100"))
, (Z("-1010101"),"/",Z("10100"))
, (Z("1000101010"),"+",Z("10101010"))
, (Z("1000101010"),"-",Z("10101010"))
, (Z("1000101010"),"*",Z("10101010"))
, (Z("1000101010"),"/",Z("10101010"))
, (Z("10100"),"+",Z("1010"))
, (Z("100101"),"-",Z("100"))
, (Z("1010101010101010101"),"+",Z("-1010101010101"))
, (Z("1010101010101010101"),"-",Z("-1010101010101"))
, (Z("1010101010101010101"),"*",Z("-1010101010101"))
, (Z("1010101010101010101"),"/",Z("-1010101010101"))
, (Z("1010101010101010101"),"%",Z("-1010101010101"))
, (Z("1010101010101010101"),"+",Z("101010101010101"))
, (Z("1010101010101010101"),"-",Z("101010101010101"))
, (Z("1010101010101010101"),"*",Z("101010101010101"))
, (Z("1010101010101010101"),"/",Z("101010101010101"))
, (Z("1010101010101010101"),"%",Z("101010101010101"))
, (Z("10101010101010101010"),"+",Z("1010101010101010"))
, (Z("10101010101010101010"),"-",Z("1010101010101010"))
, (Z("10101010101010101010"),"*",Z("1010101010101010"))
, (Z("10101010101010101010"),"/",Z("1010101010101010"))
, (Z("10101010101010101010"),"%",Z("1010101010101010"))
, (Z("1010"),"%",Z("10"))
, (Z("1010"),"%",Z("-10"))
, (Z("-1010"),"%",Z("10"))
, (Z("-1010"),"%",Z("-10"))
, (Z("100"),"/",Z("0"))
, (Z("100"),"%",Z("0"))
)

// just for result checking:
import Z._
val iadd: (BigInt,BigInt) => BigInt = (a,b) => a+b
val isub: (BigInt,BigInt) => BigInt = (a,b) => a-b
val imul: (BigInt,BigInt) => BigInt = (a,b) => a*b
val idiv: (BigInt,BigInt) => Option[BigInt] = (a,b) => if (b==0) None else Some(a/b)
val imod: (BigInt,BigInt) => Option[BigInt] = (a,b) => if (b==0) None else Some(a%b)
val iops = Map(("+",iadd),("-",isub),("*",imul),("/",idiv),("%",imod))

println("elapsed time: "+elapsed{
    calcs foreach {case (op1,op,op2) => println(op1+" "+op+" "+op2+" = "
      +{(ops(op))(op1,op2) match {case None => None; case Some(z) => z; case z => z}}
        .ensuring{x=>(iops(op))(z2i(op1),z2i(op2)) match {case None => None == x; case Some(i) => i == z2i(x.asInstanceOf[Z]); case i => i == z2i(x.asInstanceOf[Z])}})}
  }+" sec"
)

}
```

Output:
<pre style="height: 30ex; overflow: scroll">101Z(i:4) + 10100Z(i:11) = 100010Z(i:15)
101Z(i:4) - 10100Z(i:11) = -1010Z(i:-7)
101Z(i:4) * 10100Z(i:11) = 10010010Z(i:44)
101Z(i:4) / 10100Z(i:11) = 0Z(i:0)
-1010101Z(i:-33) + 10100Z(i:11) = -1000001Z(i:-22)
-1010101Z(i:-33) - 10100Z(i:11) = -10010010Z(i:-44)
-1010101Z(i:-33) * 10100Z(i:11) = -101010001010Z(i:-363)
-1010101Z(i:-33) / 10100Z(i:11) = -100Z(i:-3)
1000101010Z(i:109) + 10101010Z(i:54) = 10000101001Z(i:163)
1000101010Z(i:109) - 10101010Z(i:54) = 100000000Z(i:55)
1000101010Z(i:109) * 10101010Z(i:54) = 101000001000101001Z(i:5886)
1000101010Z(i:109) / 10101010Z(i:54) = 10Z(i:2)
10100Z(i:11) + 1010Z(i:7) = 101000Z(i:18)
100101Z(i:17) - 100Z(i:3) = 100001Z(i:14)
1010101010101010101Z(i:10945) + -1010101010101Z(i:-609) = 1010100000000000000Z(i:10336)
1010101010101010101Z(i:10945) - -1010101010101Z(i:-609) = 10000001010101010100Z(i:11554)
1010101010101010101Z(i:10945) * -1010101010101Z(i:-609) = -100010001000001001010010100100001Z(i:-6665505)
1010101010101010101Z(i:10945) / -1010101010101Z(i:-609) = -100101Z(i:-17)
1010101010101010101Z(i:10945) % -1010101010101Z(i:-609) = 1010100100100Z(i:592)
1010101010101010101Z(i:10945) + 101010101010101Z(i:1596) = 10000101010101010100Z(i:12541)
1010101010101010101Z(i:10945) - 101010101010101Z(i:1596) = 1010000000000000000Z(i:9349)
1010101010101010101Z(i:10945) * 101010101010101Z(i:1596) = 10001000100001010001001010001001001Z(i:17468220)
1010101010101010101Z(i:10945) / 101010101010101Z(i:1596) = 1001Z(i:6)
1010101010101010101Z(i:10945) % 101010101010101Z(i:1596) = 101000000001000Z(i:1369)
10101010101010101010Z(i:17710) + 1010101010101010Z(i:2583) = 100001010101010101001Z(i:20293)
10101010101010101010Z(i:17710) - 1010101010101010Z(i:2583) = 10100000000000000000Z(i:15127)
10101010101010101010Z(i:17710) * 1010101010101010Z(i:2583) = 1000100010001000000000001000100010001Z(i:45744930)
10101010101010101010Z(i:17710) / 1010101010101010Z(i:2583) = 1001Z(i:6)
10101010101010101010Z(i:17710) % 1010101010101010Z(i:2583) = 1010000000001000Z(i:2212)
1010Z(i:7) % 10Z(i:2) = 1Z(i:1)
1010Z(i:7) % -10Z(i:-2) = 1Z(i:1)
-1010Z(i:-7) % 10Z(i:2) = -1Z(i:-1)
-1010Z(i:-7) % -10Z(i:-2) = -1Z(i:-1)
100Z(i:3) / 0Z(i:0) = None
100Z(i:3) % 0Z(i:0) = None
elapsed time: 1 sec
```



## Tcl

{{trans|Perl 6}}<!-- mostly for the technique of using incr/decr -->

```tcl
namespace eval zeckendorf {
    # Want to use alternate symbols? Change these
    variable zero "0"
    variable one "1"

    # Base operations: increment and decrement
    proc zincr var {
	upvar 1 $var a
	namespace upvar [namespace current] zero 0 one 1
	if {![regsub "$0$" $a $1$0 a]} {append a $1}
	while {[regsub "$0$1$1" $a "$1$0$0" a]
		|| [regsub "^$1$1" $a "$1$0$0" a]} {}
	regsub ".$" $a "" a
	return $a
    }
    proc zdecr var {
	upvar 1 $var a
	namespace upvar [namespace current] zero 0 one 1
	regsub "^$0+(.+)$" [subst [regsub "${1}($0*)$" $a "$0\[
		string repeat {$1$0} \[regsub -all .. {\\1} {} x]]\[
		string repeat {$1} \[expr {\$x ne {}}]]"]
	    ] {\1} a
	return $a
    }

    # Exported operations
    proc eq {a b} {
	expr {$a eq $b}
    }
    proc add {a b} {
	variable zero
	while {![eq $b $zero]} {
	    zincr a
	    zdecr b
	}
	return $a
    }
    proc sub {a b} {
	variable zero
	while {![eq $b $zero]} {
	    zdecr a
	    zdecr b
	}
	return $a
    }
    proc mul {a b} {
	variable zero
	variable one
	if {[eq $a $zero] || [eq $b $zero]} {return $zero}
	if {[eq $a $one]} {return $b}
	if {[eq $b $one]} {return $a}
	set c $a
	while {![eq [zdecr b] $zero]} {
	    set c [add $c $a]
	}
	return $c
    }
    proc div {a b} {
	variable zero
	variable one
	if {[eq $b $zero]} {error "div zero"}
	if {[eq $a $zero] || [eq $b $one]} {return $a}
	set r $zero
	while {![eq $a $zero]} {
	    if {![eq $a [add [set a [sub $a $b]] $b]]} break
	    zincr r
	}
	return $r
    }
    # Note that there aren't any ordering operations in this version

    # Assemble into a coherent API
    namespace export \[a-y\]*
    namespace ensemble create
}
```

Demonstrating:

```tcl
puts [zeckendorf add "10100" "1010"]
puts [zeckendorf sub "10100" "1010"]
puts [zeckendorf mul "10100" "1010"]
puts [zeckendorf div "10100" "1010"]
puts [zeckendorf div [zeckendorf mul "10100" "1010"] "1010"]
```

{{out}}

```txt

101000
101
101000001
1
10100

```


{{omit from|Brlcad}}
{{omit from|GUISS}}
{{omit from|Lilypond}}
{{omit from|Openscad}}
{{omit from|TPP}}
