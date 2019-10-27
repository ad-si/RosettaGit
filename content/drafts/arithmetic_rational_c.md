+++
title = "Arithmetic/Rational/C"
description = ""
date = 2010-04-23T14:22:12Z
aliases = []
[extra]
id = 5288
[taxonomies]
categories = []
tags = []
+++

{{collection|Rational Arithmetic}}

C doesn't support classes, so a series of functions is provided that implements the arithmetic of a rational class.

```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <math.h>    // for fabs in converting float to rational

// a structure to contain the numerator and denominator values
typedef struct sRational {
    int numerator, denominator;
} *Rational, sRational;

int gcd( int a, int b)
{
    int g;
    if (a<b)  {
        g = a; a = b; b = g;
    }
    g = a % b;
    while (g) {
        a = b; b = g;
        g = a % b;
    }
    return abs(b);
}

int lcm( int a, int b)
{
    return (a/gcd(a,b)*b);
}

Rational NewRational( int n, int d)
{
    Rational r = (Rational)malloc(sizeof(sRational));
    int ndgcd;
    if (n!= 0)
        ndgcd = gcd(n,d);
    else {
        ndgcd = 1; d = 1;
    }
    if (r) {
        if (n>0) {
            r->numerator = n/ndgcd;
            r->denominator = d/ndgcd;
        }
        else {
            r->numerator = -n/ndgcd;
            r->denominator = -d/ndgcd;
        }
    }
    if ( d == 0) {
        printf("divide by zer0 error\n");
        exit(1);
    }
    return r;
}
#define Ratl_Delete(r) \
    {  if(r) free(r); \
    r = NULL; }

Rational Ratl_Add( Rational l, Rational r, Rational rzlt)
{
    int  denom;
    denom= lcm(l->denominator, r->denominator);
    rzlt->numerator = denom/l->denominator *l->numerator 
                   + denom/r->denominator *r->numerator ;
    rzlt->denominator = denom;
    if (rzlt->numerator == 0) {
        rzlt->denominator = 1;
    }
    else {
        int d = gcd(rzlt->numerator, rzlt->denominator);
        rzlt->numerator /= d;
        rzlt->denominator /= d;
    }
    return rzlt;
}

Rational Ratl_Negate( Rational l, Rational rzlt)
{
    rzlt->numerator = - l->numerator;
    rzlt->denominator = l->denominator;
    return rzlt;
}

Rational Ratl_Subtract(Rational l, Rational r, Rational rzlt)
{
    return Ratl_Add(l,Ratl_Negate(r, rzlt), rzlt);
}

Rational Ratl_Multiply(Rational l, Rational r, Rational rzlt)
{
    int g1 = gcd(l->numerator, r->denominator);
    int g2 = gcd(r->numerator, l->denominator);
    rzlt->numerator = l->numerator / g1 * r->numerator / g2;
    rzlt->denominator = l->denominator / g2 * r->denominator / g1;
    return rzlt;
}

Rational Ratl_Inverse(Rational l, Rational rzlt)
{
    if (l->numerator == 0) {
        printf("divide by zer0 error\n");
        exit(1);
    }
    else if (l->numerator > 0) {
        rzlt->numerator = l->denominator;
        rzlt->denominator = l->numerator;
    }
    else {
        rzlt->numerator = -l->denominator;
        rzlt->denominator = -l->numerator;
    }
    return rzlt;
}

Rational Ratl_Divide(Rational l, Rational r, Rational rzlt)
{
    return Ratl_Multiply(l, Ratl_Inverse(r, rzlt), rzlt);
}

int ipow(int base, int power )
{
    int v = base, v2 = 1;

    if (power < 0) return 0; // shouldn't happen
    while (power > 0) {
        if (power & 1) 
            v2 *=v;
        v = v*v;
        power >>= 1;
    }
    return v2;
}

Rational Ratl_Pow( Rational l, int power, Rational rzlt)
{
    if (power >= 0) {
        rzlt->numerator = ipow(l->numerator, power);
        rzlt->denominator = ipow(l->denominator, power);
    }
    else {
        rzlt->numerator = ipow(l->denominator, -power);
        rzlt->denominator = ipow(l->numerator, -power);
    }
    return rzlt;
}

int Ratl_Compare(Rational l, Rational r)
{
    int sign = (l->numerator > 0)? 1 : -1;
    sRational  comp;
    Rational pcomp;

    if ( 0 >= l->numerator * r->numerator)        // if opposite signs or one is zero
        return (l->numerator - r->numerator);
    pcomp = Ratl_Divide(l, r, &comp);

    return (pcomp->numerator - pcomp->denominator)*sign;
}

typedef enum   {
    LT, LE, EQ, GE, GT, NE } CompOp;
// boolean comparisons
int Ratl_Cpr( Rational l, CompOp compOp, Rational r)
{
    int v;
    int r1 = Ratl_Compare(l, r);
    switch(compOp) {
        case LT: v = (r1 <0)? 1 : 0; break;
        case LE: v = (r1<=0)? 1 : 0; break;
        case GT: v = (r1 >0)? 1 : 0; break;
        case GE: v = (r1>=0)? 1 : 0; break;
        case EQ: v = (r1==0)? 1 : 0; break;
        case NE: v = (r1!=0)? 1 : 0; break;
    }
    return v;
}

double Ratl_2Real(Rational l) 
{
    return l->numerator *1.0/l->denominator;
}

int Ratl_2Int(Rational l)
{
    return l->numerator /l->denominator;
}

int Ratl_2Proper(Rational l)
{
    int ipart = l->numerator/l->denominator;
    l->numerator %= l->denominator;
    return ipart;
}

char *Ratl_2String(Rational l, char *buf, int blen)
{
    char ibuf[40];
    sprintf(ibuf,"%d/%d", l->numerator, l->denominator );
    if (buf==NULL) return buf;
    if ((int)strlen(ibuf) < blen)
        strcpy(buf, ibuf);
    else  {
        strncpy(buf, ibuf, blen-4);
        strcat(buf, "...");
    }
    return buf;
}

Rational Int_2Ratl(int i, Rational rtnl)
{
    rtnl->numerator = i;
    rtnl->denominator = 1;
    return rtnl;
}

Rational Real_2Ratl(double r, double eps, Rational rtnl)
{
    int denom;
    double v1,v2;
    int isNeg = (r<0);
    if ( isNeg) r = -r;

    denom=0; 
    do {
        denom++;
        v1 = (r+eps)*denom;
        v2 = r*denom;
        v2 = fabs(v2 - (int) v1);
    } while ( v2 > denom*eps);
    rtnl->numerator = (int)v1 * ((isNeg)? -1 : 1);
    rtnl->denominator = denom;
    return rtnl;
}

Rational Ratl_Abs(Rational l, Rational rzlt)
{
    rzlt->numerator = abs(l->numerator);
    rzlt->denominator = abs(l->denominator);   // should already be nonnegative
    return rzlt;
}
```

Testing

```c
void find_perfects()
{
    int n, n2, k;
    int end = 1<<19;
    sRational r1, r2, r3;
    Rational f1, f2;

    Rational sum = NewRational( 0, 1 );
    for (n=2; n< end; n++) {
        sum = Ratl_Inverse(Int_2Ratl(n, &r1), sum);
        n2 = (int)sqrt(n)+1;
        for (k=2; k<n2; k++) {
            if ( n % k == 0) {
                f1 = Ratl_Inverse(Int_2Ratl(k,&r1), &r2);
                f2 = Ratl_Inverse(Int_2Ratl(n/k,&r1),&r3);
                Ratl_Add(sum, f1,sum);
                Ratl_Add(sum, f2,sum);
            }
        }
        if (sum->denominator == 1) {
            printf("Perfect number %d sum is %d\n", n, Ratl_2Int(sum));
        }
    }
    Ratl_Delete(sum);
}


int main(int argc, char *argv[])
{
    char ratstr[32], rs1[32],rs2[32];
    double pi = 3.14159265;
    sRational rtemp1, rtemp2;
    Rational rz;
    Rational r1 = NewRational( 5,7 );
    Rational r2 = NewRational( 4,5 );
    Rational r3 = NewRational( 3,4);

    printf("r3 = %s\n", Ratl_2String(r3, ratstr,32));

    rz = Ratl_Multiply( r1,r2, &rtemp1);
    printf("%s = %s * %s\n", Ratl_2String(rz, ratstr,32),
                     Ratl_2String(r1, rs1,32), Ratl_2String(r2, rs2, 32));

    rz = Ratl_Divide( r1,r3, &rtemp2);
    printf("%s = %s / %s\n", Ratl_2String(rz, ratstr,32),
                     Ratl_2String(r1, rs1,32), Ratl_2String(r3, rs2, 32));

    rz = Ratl_Add( r2,r3, &rtemp1);
    printf("%s = %s + %s\n", Ratl_2String(rz, ratstr,32),
                     Ratl_2String(r2, rs1,32), Ratl_2String(r3, rs2, 32));

    rz = Ratl_Subtract( r2,r3, &rtemp1);
    printf("%s = %s - %s\n", Ratl_2String(rz, ratstr,32),
                     Ratl_2String(r2, rs1,32), Ratl_2String(r3, rs2, 32));

    printf("%d = %s > %s\n", Ratl_Cpr( r2, GT, &rtemp2),
                     Ratl_2String(r2, rs1,32), Ratl_2String(&rtemp2, rs2, 32));

    rz = Ratl_Pow( r2,-3, &rtemp1);
    printf("%s = %s ^ -3\n", Ratl_2String(rz, ratstr,32),
                     Ratl_2String(r2, rs1,32));

    printf("%s = %f\n", Ratl_2String( &rtemp2, ratstr, 32), Ratl_2Real( &rtemp2));

    rz =Real_2Ratl( pi, 0.000001, &rtemp2);
    printf("%10.7f ~= %s ~=%10.7f\n", pi, Ratl_2String(rz, ratstr, 32),
                                   Ratl_2Real(rz));
    find_perfects();
    return 0;
}
```

