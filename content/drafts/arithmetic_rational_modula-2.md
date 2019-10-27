+++
title = "Arithmetic/Rational/Modula-2"
description = ""
date = 2012-02-20T15:17:05Z
aliases = []
[extra]
id = 9955
[taxonomies]
categories = []
tags = []
+++

<noinclude>{{collection|Rational Arithmetic}}</noinclude>
{{works with|FST Modula-2 v4.0|no object oriented code used}}

This is incomplete as the Perfect Numbers task has not been addressed.

;Definition Module

```modula2
DEFINITION MODULE Rational;
    TYPE RAT =  RECORD
                    numerator : INTEGER;
                    denominator : INTEGER;
                END;

    PROCEDURE IGCD( i : INTEGER; j : INTEGER ) : INTEGER;
    PROCEDURE ILCM( i : INTEGER; j : INTEGER ) : INTEGER;
    PROCEDURE IABS( i : INTEGER ) : INTEGER;

    PROCEDURE RNormalize( i : RAT ) : RAT;
    PROCEDURE RCreate( num : INTEGER; dem : INTEGER ) : RAT;
    PROCEDURE RAdd( i : RAT; j : RAT ) : RAT;
    PROCEDURE RSubtract( i : RAT; j : RAT ) : RAT;
    PROCEDURE RMultiply( i : RAT; j : RAT ) : RAT;
    PROCEDURE RDivide( i : RAT; j : RAT ) : RAT;
    PROCEDURE RAbs( i : RAT ) : RAT;
    PROCEDURE RInv( i : RAT ) : RAT;
    PROCEDURE RNeg( i : RAT ) : RAT;

    PROCEDURE RInc( i : RAT ) : RAT;
    PROCEDURE RDec( i : RAT ) : RAT;
    
    PROCEDURE REQ( i : RAT; j : RAT ) : BOOLEAN;
    PROCEDURE RNE( i : RAT; j : RAT ) : BOOLEAN;
    PROCEDURE RLT( i : RAT; j : RAT ) : BOOLEAN;
    PROCEDURE RLE( i : RAT; j : RAT ) : BOOLEAN;
    PROCEDURE RGT( i : RAT; j : RAT ) : BOOLEAN;
    PROCEDURE RGE( i : RAT; j : RAT ) : BOOLEAN;

    PROCEDURE RIsZero( i : RAT ) : BOOLEAN;
    PROCEDURE RIsNegative( i : RAT ) : BOOLEAN;
    PROCEDURE RIsPositive( i : RAT ) : BOOLEAN;

    PROCEDURE RToString( i : RAT; VAR S : ARRAY OF CHAR );
    PROCEDURE RToRational( s : ARRAY OF CHAR ) : RAT;

    PROCEDURE WriteRational( i : RAT );

END Rational.
```


;Implementation Module

```modula2
IMPLEMENTATION MODULE Rational;

    FROM Strings IMPORT Assign, Append, Pos, Copy, Length;
    FROM NumberConversion IMPORT IntToString, StringToInt;

    FROM InOut IMPORT WriteString (*, WriteCard,WriteLine, WriteInt, WriteLn *);

    PROCEDURE IGCD( i : INTEGER; j : INTEGER ) : INTEGER;
    VAR
        res : INTEGER;
    BEGIN
        IF j = 0 THEN
            res := i;
        ELSE
            res := IGCD( j, i MOD j );
        END;

        RETURN res;
    END IGCD;

    PROCEDURE ILCM( i : INTEGER; j : INTEGER ) : INTEGER;
    VAR
        res : INTEGER;
    BEGIN
        res := (i DIV IGCD( i, j ) ) * j;
        RETURN res;
    END ILCM;

    PROCEDURE IABS( i : INTEGER ) : INTEGER;
    VAR
        res : INTEGER;
    BEGIN
        IF i < 0 THEN
            res := i * (-1);
        ELSE
            res := i;
        END;
        RETURN res;
    END IABS;

    PROCEDURE RNormalize( i : RAT ) : RAT;
    VAR
        gcd : INTEGER;
        res : RAT;
    BEGIN
        gcd := IGCD( ABS( i.numerator ), ABS( i.denominator ) );
        IF gcd <> 0 THEN
            res.numerator := i.numerator DIV gcd;
            res.denominator := i.denominator DIV gcd;
            IF ( res.denominator < 0 ) THEN
                res.numerator := res.numerator * (-1);
                res.denominator := res.denominator * (-1);
            END;
        ELSE
            WITH res DO
                numerator := 0;
                denominator := 0;
            END;
        END;
        RETURN res;
    END RNormalize;

    PROCEDURE RCreate( num : INTEGER; dem : INTEGER ) : RAT;
    VAR
        rat : RAT;
    BEGIN
        WITH rat DO
            numerator := num;
            denominator := dem;
        END;
        RETURN RNormalize(rat);
    END RCreate;

    PROCEDURE RAdd( i : RAT; j : RAT ) : RAT;
    BEGIN
        RETURN RCreate( i.numerator * j.denominator + j.numerator * i.denominator, i.denominator * j.denominator );
    END RAdd;

    PROCEDURE RSubtract( i : RAT; j : RAT ) : RAT;
    BEGIN
        RETURN RCreate( i.numerator * j.denominator - j.numerator * i.denominator, i.denominator * j.denominator );
    END RSubtract;

    PROCEDURE RMultiply( i : RAT; j : RAT ) : RAT;
    BEGIN
        RETURN RCreate( i.numerator * j.numerator, i.denominator * j.denominator );
    END RMultiply;

    PROCEDURE RDivide( i : RAT; j : RAT ) : RAT;
    BEGIN
        RETURN RCreate( i.numerator * j.denominator, i.denominator * j.numerator );
    END RDivide;

    PROCEDURE RAbs( i : RAT ) : RAT;
    BEGIN
        RETURN RCreate( IABS( i.numerator ), i.denominator );
    END RAbs;

    PROCEDURE RInv( i : RAT ) : RAT;
    BEGIN
        RETURN RCreate( i.denominator, i.numerator );
    END RInv;

    PROCEDURE RNeg( i : RAT ) : RAT;
    BEGIN
        RETURN RCreate( i.numerator * (-1), i.denominator );
    END RNeg;

    PROCEDURE RInc( i : RAT ) : RAT;
    BEGIN
        RETURN RCreate( i.numerator + i.denominator, i.denominator );
    END RInc;

    PROCEDURE RDec( i : RAT ) : RAT;
    BEGIN
        RETURN RCreate( i.numerator - i.denominator, i.denominator );
    END RDec;

    PROCEDURE REQ( i : RAT; j : RAT ) : BOOLEAN;
    VAR
        ii : RAT;
        jj : RAT;
    BEGIN
        ii := RNormalize( i );
        jj := RNormalize( j );
        RETURN ( ( ii.numerator = jj.numerator ) AND ( ii.denominator = jj.denominator ) );
    END REQ;

    PROCEDURE RNE( i : RAT; j : RAT ) : BOOLEAN;
    BEGIN
        RETURN NOT REQ( i, j );
    END RNE;

    PROCEDURE RLT( i : RAT; j : RAT ) : BOOLEAN;
    BEGIN
        RETURN RIsNegative( RSubtract( i, j ) );
    END RLT;

    PROCEDURE RLE( i : RAT; j : RAT ) : BOOLEAN;
    BEGIN
        RETURN NOT RGT( i, j );
    END RLE;

    PROCEDURE RGT( i : RAT; j : RAT ) : BOOLEAN;
    BEGIN
        RETURN RIsPositive( RSubtract( i, j ) );
    END RGT;

    PROCEDURE RGE( i : RAT; j : RAT ) : BOOLEAN;
    BEGIN
        RETURN NOT RLT( i, j );
    END RGE;

    PROCEDURE RIsZero( i : RAT ) : BOOLEAN;
    BEGIN
        RETURN i.numerator = 0;
    END RIsZero;

    PROCEDURE RIsNegative( i : RAT ) : BOOLEAN;
    BEGIN
        RETURN i.numerator < 0;
    END RIsNegative;

    PROCEDURE RIsPositive( i : RAT ) : BOOLEAN;
    BEGIN
        RETURN i.numerator > 0;
    END RIsPositive;

    PROCEDURE RToString( i : RAT; VAR S : ARRAY OF CHAR );
    VAR
        num : ARRAY [1..15] OF CHAR;
        den : ARRAY [1..15] OF CHAR;
    BEGIN
        IF RIsZero( i ) THEN
            Assign("0", S );
        ELSE
            IntToString( i.numerator, num, 1 );
            Assign( num, S );
            IF ( i.denominator <> 1 ) THEN
                IntToString( i.denominator, den, 1 );
                Append( S, "/" );
                Append( S, den );
            END;
        END;
    END RToString;

    PROCEDURE RToRational( s : ARRAY OF CHAR ) : RAT;
    VAR
        n : CARDINAL;
        numer : INTEGER;
        denom : INTEGER;
        LHS, RHS : ARRAY [ 1..20 ] OF CHAR;
        Flag : BOOLEAN;
    BEGIN
        numer := 0;
        denom := 0;
        n := Pos( "/", s );

        IF n > HIGH( s ) THEN
            StringToInt( s, numer, Flag );
            IF Flag THEN
                denom := 1;
            END;
        ELSE
            Copy( s, 0, n, LHS );
            Copy( s, n+1, Length( s ), RHS );
            StringToInt( LHS, numer, Flag );
            IF Flag THEN
                StringToInt( RHS, denom, Flag );
            END;
        END;
        RETURN RCreate( numer, denom );
    END RToRational;

    PROCEDURE WriteRational( i : RAT );
    VAR
        res : ARRAY [0 .. 80] OF CHAR;
    BEGIN
        RToString( i, res );
        WriteString( res );
    END WriteRational;

END Rational.
```


;Test Program

```modula2
MODULE TestRat;
       FROM InOut IMPORT WriteString, WriteLine;
       FROM Terminal IMPORT KeyPressed;
       FROM Strings IMPORT CompareStr;
       FROM Rational IMPORT RAT, IGCD, RCreate, RToString, RIsZero, RNormalize,
                            RToRational, REQ, RNE, RLT, RLE, RGT, RGE, WriteRational,
                            RAdd, RSubtract, RMultiply, RDivide, RAbs, RNeg, RInv;
VAR
    res : INTEGER;
    a, b, c, d, e, f : RAT;
    ans : ARRAY [1..100] OF CHAR;

PROCEDURE Assert( F : BOOLEAN; S : ARRAY OF CHAR );
BEGIN
    IF ( NOT F) THEN
        WriteLine( S );
    END;
END Assert;

BEGIN

    a := RCreate( 0, 0 );
    Assert( RIsZero( a ), "RIsZero( a )");

    a := RToRational("2");
    RToString( a, ans );
    res := CompareStr( ans, "2" );
    Assert( (res = 0), "CompareStr( RToString( a ), '2' ) = 0");

    a := RToRational("1/2");
    RToString( a, ans );
    res := CompareStr( ans, "1/2");
    Assert( res = 0, "CompareStr( RToString( a, ans ), '1/2') = 0");

    b := RToRational( "2/-12" );
    RToString( b, ans );
    res := CompareStr( ans, "-1/6");
    Assert( res = 0, "CompareStr( RToString( b, ans ), '-1/6') = 0");

    f := RCreate( 0, 9 ); (* rationalizes internally to zero *)

    a := RToRational("1/3");
    b := RToRational("1/2");
    c := RCreate( 1, 3 );

    Assert( NOT REQ( a, b ), "1/3 == 1/2" );
    Assert( REQ( a, c ), "1/3 == 1/3" );
    Assert( RNE( a, b ), "1/3 != 1/2" );
    Assert( RLT( a, b ), "1/3 < 1/2" );
    Assert( NOT RLT(b,a), "1/2 < 1/3" );
    Assert( NOT RLT(a,c), "1/3 < 1/3" );
    Assert( NOT RGT(a,b), "1/3 > 1/2" );
    Assert( RGT(b,a), "1/2 > 1/3" );
    Assert( NOT RGT(a,c), "1/3 > 1/3" );

    Assert( RLE( a, b ), "1/3 <= 1/2" );
    Assert( NOT RLE( b, a ), "1/2 <= 1/3" );
    Assert( RLE( a, c ), "1/3 <= 1/3" );
    Assert( NOT RGE(a,b), "1/3 >= 1/2" );
    Assert( RGE(b,a), "1/2 >= 1/3" );
    Assert( RGE( a,c ), "1/3 >= 1/3" );

    a := RCreate(1,2);
    b := RCreate(1,6);
    a := RAdd( a, b );
    Assert( REQ( a, RToRational("2/3")), "1/2 + 1/6 == 2/3" );

    c := RNeg( a );
    Assert( REQ( a, RCreate( 2,3)), "2/3 == 2/3" );
    Assert( REQ( c, RCreate( 2,-3)), "Neg 1/2 == -1/2" );
    a := RCreate( 2,-3);

    d := RAbs( c );
    Assert( REQ( d, RCreate( 2,3 ) ), "abs(neg(1/2))==1/2" );

    a := RToRational( "1/2");
    b := RSubtract( b, a );

    Assert( REQ( b, RCreate(-1,3) ), "1/6 - 1/2 == -1/3" );

    c := RInv(b);
    RToString( c, ans );
    res := CompareStr( ans, "-3" );
    Assert( res = 0, "inv(1/6 - 1/2) == -3" );

    f := RInv( f ); (* as f normalized to zero, the reciprocal is still zero *)


    b := RCreate( 1, 6);
    b := RAdd( b, RAdd( RCreate( 2,3), RCreate( 4,2 )));
    RToString( b, ans );
    res := CompareStr( ans, "17/6" );
    Assert( res = 0, "1/6 + 2/3 + 4/2 = 17/6" );

    a := RCreate(1,3);
    b := RCreate(1,6);
    c := RCreate(5,6);
    d := RToRational("1/5");
    e := RToRational("2");
    f := RToRational("0/9");

    Assert( REQ( RMultiply( c, d ), b ), "5/6 * 1/5 = 1/6" );
    Assert( REQ( RMultiply( c, RMultiply( d, e ) ), a ), "5/6 * 1/5 * 2 = 1/3" );
    Assert( REQ( RMultiply( c, RMultiply( d, RMultiply( e, f ) ) ), f ), "5/6 * 1/5 * 2 * 0" );
    Assert( REQ( b, RDivide( c, RToRational("5" ) ) ), "5/6 / 5 = 1/6" );

    e := RDivide( c, f ); (* RDivide multiplies so no divide by zero *)

    WriteString("Press any key..."); WHILE NOT KeyPressed() DO END;
END TestRat.
```

