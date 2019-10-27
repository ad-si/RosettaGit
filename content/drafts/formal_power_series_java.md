+++
title = "Formal power series/Java"
description = ""
date = 2011-03-22T18:12:34Z
aliases = []
[extra]
id = 9383
[taxonomies]
categories = []
tags = []
+++

{{works with|Java|1.5+}}

Java has no generic numeric interface, and has no templates, so we cannot make it work for all numeric types at the same time. Because the Java library does not come with a Fraction class (and I am too lazy to implement one, although there is one in Apache Commons Math), here I will just hard-code it to use doubles (64-bit floating-point numbers) instead of fractions. It won't be as pretty, but it can be changed to Fraction or any other type trivially by substituting the types.

```java5
import java.util.*;

interface Gene {
    double coef(int n);
}

class Term {
    private final List<Double> cache = new ArrayList<Double>();
    private final Gene gene;

    public Term(Gene g) { gene = g; }

    public double get(int n) {
        if (n < 0)
            return 0;
        else if (n >= cache.size())
            for (int i = cache.size(); i <= n; i++)
                cache.add(gene.coef(i));
        return cache.get(n);
    }
}

public class FormalPS {
    private static final int DISP_TERM = 12;
    private static final String X_VAR = "x";
    private Term term;

    public FormalPS() { }
    public void copyFrom(FormalPS foo) {
        term = foo.term;
    }

    public FormalPS(Term t) {
        term = t;
    }

    public FormalPS(final double[] polynomial) {
        this(new Term(new Gene() {
                public double coef(int n) {
                    if (n < 0 || n >= polynomial.length)
                        return 0;
                    else
                        return polynomial[n];
                }
            }));
    }

    public double inverseCoef(int n) {
        double[] res = new double[n + 1];
        res[0] = 1 / term.get(0);
        for (int i = 1; i <= n; i++) {
            res[i] = 0;
            for (int j = 0; j < i; j++)
                res[i] += term.get(i-j) * res[j];
            res[i] *= -res[0];
        }
        return res[n];
    }

    public FormalPS add(final FormalPS rhs) {
        return new FormalPS(new Term(new Gene() {
                public double coef(int n) {
                    return term.get(n) + rhs.term.get(n);
                }
            }));
    }

    public FormalPS sub(final FormalPS rhs) {
        return new FormalPS(new Term(new Gene() {
                public double coef(int n) {
                    return term.get(n) - rhs.term.get(n);
                }
            }));
    }

    public FormalPS mul(final FormalPS rhs) {
        return new FormalPS(new Term(new Gene() {
                public double coef(int n) {
                    double res = 0;
                    for (int i = 0; i <= n; i++)
                        res += term.get(i) * rhs.term.get(n-i);
                    return res;
                }
            }));
    }

    public FormalPS div(final FormalPS rhs) {
        return new FormalPS(new Term(new Gene() {
                public double coef(int n) {
                    double res = 0;
                    for (int i = 0; i <= n; i++)
                        res += term.get(i) * rhs.inverseCoef(n-i);
                    return res;
                }
            }));
    }

    public FormalPS diff() {
        return new FormalPS(new Term(new Gene() {
                public double coef(int n) {
                    return term.get(n+1) * (n+1);
                }
            }));
    }

    public FormalPS intg() {
        return new FormalPS(new Term(new Gene() {
                public double coef(int n) {
                    if (n == 0)
                        return 0;
                    else
                        return term.get(n-1) / n;
                }
            }));
    }

    public String toString() {
        return toString(DISP_TERM);
    }

    public String toString(int dpTerm) {
        StringBuffer s = new StringBuffer();
        {
            double c = term.get(0);
            if (c != 0)
                s.append(c);
        }
        for (int i = 1; i < dpTerm; i++) {
            double c = term.get(i);
            if (c != 0) {
                if (c > 0 && s.length() > 0)
                    s.append("+");
                if (c == 1)
                    s.append(X_VAR);
                else if (c == -1)
                    s.append("-" + X_VAR);
                else
                    s.append(c + X_VAR);
                if (i > 1)
                    s.append(i);
            }
        }
        if (s.length() == 0)
            s.append("0");
        s.append("+...");
        return s.toString();
    }

    public static void main(String[] args) {
        FormalPS cos = new FormalPS();
        FormalPS sin = cos.intg();
        cos.copyFrom(new FormalPS(new double[]{1}).sub(sin.intg()));
        System.out.println("SIN(x) = " + sin);
        System.out.println("COS(x) = " + cos);
    }
}
```

Output:
 SIN(x) = x-0.16666666666666666x3+0.008333333333333333x5-1.984126984126984E-4x7+2.7557319223985893E-6x9-2.505210838544172E-8x11+...
 COS(x) = 1.0-0.5x2+0.041666666666666664x4-0.001388888888888889x6+2.48015873015873E-5x8-2.7557319223985894E-7x10+...
