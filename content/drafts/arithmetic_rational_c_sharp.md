+++
title = "Arithmetic/Rational/C sharp"
description = ""
date = 2015-12-13T13:20:47Z
aliases = []
[extra]
id = 8795
[taxonomies]
categories = []
tags = []
+++

<noinclude>{{collection|Rational Arithmetic}}</noinclude>

```csharp
using System;

struct Fraction : IEquatable<Fraction>, IComparable<Fraction>
{
    public readonly long Num;
    public readonly long Denom;

    public Fraction(long num, long denom)
    {
        if (num == 0)
        {
            denom = 1;
        }
        else if (denom == 0)
        {
            throw new ArgumentException("Denominator may not be zero", "denom");
        }
        else if (denom < 0)
        {
            num = -num;
            denom = -denom;
        }

        long d = GCD(num, denom);
        this.Num = num / d;
        this.Denom = denom / d;
    }

    private static long GCD(long x, long y)
    {
        return y == 0 ? x : GCD(y, x % y);
    }

    private static long LCM(long x, long y)
    {
        return x / GCD(x, y) * y;
    }

    public Fraction Abs()
    {
        return new Fraction(Math.Abs(Num), Denom);
    }

    public Fraction Reciprocal()
    {
        return new Fraction(Denom, Num);
    }

    #region Conversion Operators

    public static implicit operator Fraction(long i)
    {
        return new Fraction(i, 1);
    }

    public static explicit operator double(Fraction f)
    {
        return f.Num == 0 ? 0 : (double)f.Num / f.Denom;
    }

    #endregion

    #region Arithmetic Operators

    public static Fraction operator -(Fraction f)
    {
        return new Fraction(-f.Num, f.Denom);
    }

    public static Fraction operator +(Fraction a, Fraction b)
    {
        long m = LCM(a.Denom, b.Denom);
        long na = a.Num * m / a.Denom;
        long nb = b.Num * m / b.Denom;
        return new Fraction(na + nb, m);
    }

    public static Fraction operator -(Fraction a, Fraction b)
    {
        return a + (-b);
    }

    public static Fraction operator *(Fraction a, Fraction b)
    {
        return new Fraction(a.Num * b.Num, a.Denom * b.Denom);
    }

    public static Fraction operator /(Fraction a, Fraction b)
    {
        return a * b.Reciprocal();
    }

    public static Fraction operator %(Fraction a, Fraction b)
    {
        long l = a.Num * b.Denom, r = a.Denom * b.Num;
        long n = l / r;
        return new Fraction(l - n * r, a.Denom * b.Denom);
    }

    #endregion

    #region Comparison Operators

    public static bool operator ==(Fraction a, Fraction b)
    {
        return a.Num == b.Num && a.Denom == b.Denom;
    }

    public static bool operator !=(Fraction a, Fraction b)
    {
        return a.Num != b.Num || a.Denom != b.Denom;
    }

    public static bool operator <(Fraction a, Fraction b)
    {
        return (a.Num * b.Denom) < (a.Denom * b.Num);
    }

    public static bool operator >(Fraction a, Fraction b)
    {
        return (a.Num * b.Denom) > (a.Denom * b.Num);
    }

    public static bool operator <=(Fraction a, Fraction b)
    {
        return !(a > b);
    }

    public static bool operator >=(Fraction a, Fraction b)
    {
        return !(a < b);
    }

    #endregion

    #region Object Members

    public override bool Equals(object obj)
    {
        if (obj is Fraction)
            return ((Fraction)obj) == this;
        else
            return false;
    }

    public override int GetHashCode()
    {
        return Num.GetHashCode() ^ Denom.GetHashCode();
    }

    public override string ToString()
    {
        return Num.ToString() + "/" + Denom.ToString();
    }

    #endregion

    #region IEquatable<Fraction> Members

    public bool Equals(Fraction other)
    {
        return other == this;
    }

    #endregion

    #region IComparable<Fraction> Members

    public int CompareTo(Fraction other)
    {
        return (this.Num * other.Denom).CompareTo(this.Denom * other.Num);
    }

    #endregion
}
```

Test program:

```csharp
using System;

static class Program
{
    static void Main(string[] args)
    {
        int max = 1 << 19;
        for (int candidate = 2; candidate < max; candidate++)
        {
            Fraction sum = new Fraction(1, candidate);
            int max2 = (int)Math.Sqrt(candidate);
            for (int factor = 2; factor <= max2; factor++)
            {
                if (candidate % factor == 0)
                {
                    sum += new Fraction(1, factor);
                    sum += new Fraction(1, candidate / factor);
                }
            }

            if (sum == 1)
                Console.WriteLine("{0} is perfect", candidate);
        }
    }
}
```

{{out}}

```txt
6 is perfect
28 is perfect
496 is perfect
8128 is perfect
```

