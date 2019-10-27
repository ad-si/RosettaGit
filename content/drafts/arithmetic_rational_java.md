+++
title = "Arithmetic/Rational/Java"
description = ""
date = 2011-01-06T18:44:50Z
aliases = []
[extra]
id = 9123
[taxonomies]
categories = []
tags = []
+++

Modeled after BigInteger/BigDecimal. Instances of this class are immutable, and simplified upon construction. The returned numerator() and denominator() are never negative; the sign can be retrieved via signum(). The denominator for zero is always 1 (e.g. 0/5 is simplified to 0/1), and signed zeros are not supported (e.g. 0/-1 is simplified to 0/1).


```java
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

public class BigRational extends Number implements Comparable<BigRational>
{

  public final static BigRational ZERO = new BigRational(false, BigInteger.ZERO, BigInteger.ONE);
  public final static BigRational ONE = new BigRational(false, BigInteger.ONE, BigInteger.ONE);
  
  private final boolean isNegative;
  private final BigInteger numerator;
  private final BigInteger denominator;
  private final int hashCode;
  
  private BigRational(boolean isNegative, BigInteger nonNegativeNumerator, BigInteger nonNegativeDenominator)
  {
    this.isNegative = isNegative;
    this.numerator = nonNegativeNumerator;
    this.denominator = nonNegativeDenominator;
    this.hashCode = computeHashCode(isNegative, nonNegativeNumerator, nonNegativeDenominator);
  }

  private BigRational(BigInteger numerator, BigInteger denominator, boolean ignoreComponentSigns, boolean forcedSign)
  {
    if (denominator.equals(BigInteger.ZERO))
      throw new ArithmeticException("Denominator is zero");
    boolean isNegative = ignoreComponentSigns ? forcedSign : false;
    if (numerator.equals(BigInteger.ZERO))
    {
      denominator = BigInteger.ONE;
      isNegative = false;
    }
    else
    {
      if (numerator.signum() < 0)
      {
        if (!ignoreComponentSigns)
          isNegative = true;
        numerator = numerator.negate();
      }
      if (denominator.signum() < 0)
      {
        if (!ignoreComponentSigns)
          isNegative = !isNegative;
        denominator = denominator.negate();
      }
      BigInteger gcd = numerator.gcd(denominator);
      if (!gcd.equals(BigInteger.ONE))
      {
        numerator = numerator.divide(gcd);
        denominator = denominator.divide(gcd);
      }
    }
    this.isNegative = isNegative;
    this.numerator = numerator;
    this.denominator = denominator;
    this.hashCode = computeHashCode(isNegative, numerator, denominator);
  }
  
  public BigRational(BigInteger numerator, BigInteger denominator)
  {  this(numerator, denominator, false, false);  }
  
  public BigRational abs()
  {  return isNegative ? new BigRational(false, numerator, denominator) : this;  }
  
  public BigRational add(BigRational br)
  {
    if (isNegative == br.isNegative)
      return addIgnoreNegatives(isNegative, this, br);
    if (isNegative)
      return subtractIgnoreNegatives(br, this);
    return subtractIgnoreNegatives(this, br);
  }
  
  public int compareTo(BigRational br)
  {
    if (isNegative != br.isNegative)
      return isNegative ? -1 : 1;
    return subtract(br).signum();
  }
  
  public BigRational decrement()
  {
    if (isNegative)
      return new BigRational(numerator.add(denominator), denominator, true, true);
    return new BigRational(numerator.subtract(denominator), denominator, true, (numerator.compareTo(denominator) < 0));
  }
  
  public BigInteger denominator()
  {  return denominator;  }
  
  public BigRational divide(BigInteger bi)
  {
    boolean isNegative = (bi.signum() < 0);
    if (isNegative)
      bi = bi.negate();
    isNegative = (isNegative != this.isNegative);
    return new BigRational(numerator, denominator.multiply(bi), true, isNegative);
  }
  
  public BigRational divide(BigRational divisor)
  {  return multiply(divisor.reciprocal());  }
  
  public double doubleValue()
  {  return toBigDecimal(18, RoundingMode.HALF_EVEN).doubleValue();  }
  
  public boolean equals(Object o)
  {
    if ((o == null) || !(o instanceof BigRational))
      return false;
    BigRational br = (BigRational)o;
    return (isNegative == br.isNegative) && numerator.equals(br.numerator) && denominator.equals(br.denominator);
  }
  
  public float floatValue()
  {  return toBigDecimal(9, RoundingMode.HALF_EVEN).floatValue();  }
  
  public int hashCode()
  {  return hashCode;  }
  
  public BigRational increment()
  {
    if (!isNegative)
      return new BigRational(numerator.add(denominator), denominator, true, false);
    return new BigRational(numerator.subtract(denominator), denominator, true, (numerator.compareTo(denominator) > 0));
  }
  
  public int intValue()
  {  return toBigDecimal(12, RoundingMode.HALF_EVEN).intValue();  }
  
  public boolean isWholeNumber()
  {  return denominator.equals(BigInteger.ONE);  }
  
  public boolean isZero()
  {  return numerator.equals(BigInteger.ZERO);  }
  
  public long longValue()
  {  return toBigDecimal(21, RoundingMode.HALF_EVEN).longValue();  }
  
  public BigRational max(BigRational br)
  {  return (compareTo(br) >= 0) ? this : br;  }
  
  public BigRational min(BigRational br)
  {  return (compareTo(br) <= 0) ? this : br;  }
  
  public Object[] mixedFraction()
  {
    BigInteger[] dar = numerator.divideAndRemainder(denominator);
    BigInteger whole = dar[0];
    if (isNegative)
      whole = whole.negate();
    BigRational fraction = new BigRational(dar[1], denominator, true, isNegative);
    return new Object[] { whole, fraction };
  }

  public BigRational multiply(BigInteger bi)
  {
    boolean isNegative = (bi.signum() < 0);
    if (isNegative)
      bi = bi.negate();
    isNegative = (isNegative != this.isNegative);
    return new BigRational(numerator.multiply(bi), denominator, true, isNegative);
  }
  
  public BigRational multiply(BigRational br)
  {
    BigInteger numerator = this.numerator.multiply(br.numerator);
    BigInteger denominator = this.denominator.multiply(br.denominator);
    return new BigRational(numerator, denominator, true, isNegative != br.isNegative);
  }
  
  public BigRational negate()
  {
    if (isZero())
      return this;
    return new BigRational(!isNegative, numerator, denominator);
  }
  
  public BigInteger numerator()
  {  return numerator;  }
  
  public BigRational pow(int n)
  {
    BigInteger numerator = this.numerator.pow(n);
    BigInteger denominator = this.denominator.pow(n);
    return new BigRational(numerator, denominator, true, isNegative ? ((n & 1) != 0) : false);
  }
  
  public BigRational reciprocal()
  {
    if (isZero())
      throw new ArithmeticException("Can not calculate reciprocal of zero");
    return new BigRational(isNegative, denominator, numerator);
  }
  
  public int signum()
  {
    if (isZero())
      return 0;
    return isNegative ? -1 : 1;
  }
  
  public BigRational subtract(BigRational br)
  {
    if (isNegative != br.isNegative)
      return addIgnoreNegatives(isNegative, this, br);
    if (isNegative)
      return subtractIgnoreNegatives(br, this);
    return subtractIgnoreNegatives(this, br);
  }
  
  public BigDecimal toBigDecimal(int desiredPrecision, RoundingMode roundingMode)
  {
    BigDecimal bdNumerator = new BigDecimal(numerator);
    BigDecimal bdDenominator = new BigDecimal(denominator);
    int resultScale = bdNumerator.scale() - bdDenominator.scale() - bdNumerator.precision() + bdDenominator.precision() + desiredPrecision;
    BigDecimal bigDecimalValue = bdNumerator.divide(bdDenominator, resultScale, roundingMode);
    if (bigDecimalValue.precision() > desiredPrecision)
      bigDecimalValue = bigDecimalValue.setScale(bigDecimalValue.scale() - 1, roundingMode);
    if (isNegative)
      bigDecimalValue = bigDecimalValue.negate();
    return bigDecimalValue;
  }
  
  public BigDecimal toBigDecimalExact()
  {
    BigDecimal bigDecimalValue = new BigDecimal(numerator).divide(new BigDecimal(denominator));
    if (isNegative)
      bigDecimalValue = bigDecimalValue.negate();
    return bigDecimalValue;
  }
  
  public String toString()
  {
    if (isWholeNumber())
    {
      if (isNegative)
        return "-" + numerator.toString();
      return numerator.toString();
    }
    if (isNegative)
      return "-" + numerator.toString() + "/" + denominator.toString();
    return numerator.toString() + "/" + denominator.toString();
  }

  private static int computeHashCode(boolean isNegative, BigInteger numerator, BigInteger denominator)
  {
    int c = numerator.hashCode() + denominator.hashCode();
    if (isNegative)
      c = ~c;
    return c;
  }

  private static BigRational addIgnoreNegatives(boolean resultIsNegative, BigRational first, BigRational second)
  {
    first = first.abs();
    second = second.abs();
    BigInteger numerator = null;
    BigInteger denominator = null;
    if (first.denominator.equals(second.denominator))
    {
      numerator = first.numerator.add(second.numerator);
      denominator = first.denominator;
    }
    else
    {
      numerator = first.numerator.multiply(second.denominator).add(second.numerator.multiply(first.denominator));
      denominator = first.denominator.multiply(second.denominator);
    }
    return new BigRational(numerator, denominator, true, resultIsNegative);
  }
  
  private static BigRational subtractIgnoreNegatives(BigRational first, BigRational second)
  {
    first = first.abs();
    second = second.abs();
    BigInteger numerator = null;
    BigInteger denominator = null;
    if (first.denominator.equals(second.denominator))
    {
      numerator = first.numerator.subtract(second.numerator);
      denominator = first.denominator;
    }
    else
    {
      numerator = first.numerator.multiply(second.denominator).subtract(second.numerator.multiply(first.denominator));
      denominator = first.denominator.multiply(second.denominator);
    }
    return new BigRational(numerator, denominator);
  }
  
  public static BigRational valueOf(int integerValue)
  {  return valueOf(integerValue, 1);  }
  
  public static BigRational valueOf(int numerator, int denominator)
  {  return new BigRational(BigInteger.valueOf(numerator), BigInteger.valueOf(denominator));  }
  
  public static BigRational valueOf(long longValue)
  {  return valueOf(longValue, 1);  }
  
  public static BigRational valueOf(long numerator, long denominator)
  {  return new BigRational(BigInteger.valueOf(numerator), BigInteger.valueOf(denominator));  }
  
  public static BigRational valueOf(BigDecimal bigDecimalValue)
  {
    int scale = bigDecimalValue.scale();
    BigInteger numerator = bigDecimalValue.unscaledValue();
    if (scale <= 0)
    {
      if (scale < 0)
        numerator = numerator.multiply(BigInteger.TEN.pow(-scale));
      return new BigRational(numerator, BigInteger.ONE);
    }
    return new BigRational(numerator, BigInteger.TEN.pow(scale));
  }
  
  public static BigRational valueOf(BigDecimal numerator, BigDecimal denominator)
  {  return valueOf(numerator).divide(valueOf(denominator));  }
  
  public static BigRational valueOf(double doubleValue)
  {  return valueOf(new BigDecimal(doubleValue));  }
  
  public static BigRational valueOf(double numerator, double denominator)
  {  return valueOf(numerator).divide(valueOf(denominator));  }
  
  public static BigRational valueOf(String number)
  {
    int slashIndex = number.indexOf("/");
    if (slashIndex > 0)
      return valueOf(number.substring(0, slashIndex), number.substring(slashIndex + 1));
    return valueOf(new BigDecimal(number));
  }
  
  public static BigRational valueOf(String numerator, String denominator)
  {  return valueOf(numerator).divide(valueOf(denominator));  }
  
  /* ******************** Testing ******************** */
  
  public static boolean testEquals(String testName, Object obj, String str)
  {  return testEquals(testName, obj, null, str);  }
  
  public static boolean testEquals(String testName, Object obj1, Object obj2, String str)
  {
    // obj2 is optional
    boolean objEquality = (obj2 == null) || obj1.equals(obj2);
    String objStr = (obj1 instanceof BigDecimal) ? ((BigDecimal)obj1).toPlainString() : obj1.toString();
    boolean strEquality = objStr.equals(str);
    if (objEquality && strEquality)
    {
      System.out.println("PASS: " + testName);
      return true;
    }
    String err = "FAIL: " + testName + ": " + obj1.toString() + " not equal to";
    if (!objEquality)
      err += " object " + obj2.toString();
    if (!strEquality)
    {
      if (!objEquality)
        err += " and";
      err += " string " + str;
    }
    System.out.println(err);
    return false;
  }
  
  public static boolean testTrue(String testName, boolean b)
  {
    System.out.println((b ? "PASS: " : "FAIL: ") + testName);
    return b;
  }
  
  public static boolean testFeatures()
  {
    boolean passedAll = true;
    passedAll &= testEquals("BaseConstructor-1", new BigRational(BigInteger.valueOf(30), BigInteger.valueOf(72)), new BigRational(BigInteger.valueOf(5), BigInteger.valueOf(12)), "5/12");
    passedAll &= testEquals("BaseConstructor-2", new BigRational(BigInteger.valueOf(-30), BigInteger.valueOf(72)), new BigRational(BigInteger.valueOf(5), BigInteger.valueOf(-12)), "-5/12");
    passedAll &= testEquals("BaseConstructor-3", new BigRational(BigInteger.valueOf(-30), BigInteger.valueOf(-72)), new BigRational(BigInteger.valueOf(-5), BigInteger.valueOf(-12)), "5/12");
    passedAll &= testEquals("BaseConstructor-4", new BigRational(BigInteger.valueOf(0), BigInteger.valueOf(5)), new BigRational(BigInteger.valueOf(0), BigInteger.valueOf(-10)), "0");
    passedAll &= testTrue("Inequality-1", !new BigRational(BigInteger.valueOf(5), BigInteger.valueOf(12)).equals(new BigRational(BigInteger.valueOf(-5), BigInteger.valueOf(12))));
    passedAll &= testTrue("Inequality-2", !new BigRational(BigInteger.valueOf(5), BigInteger.valueOf(12)).equals(new BigRational(BigInteger.valueOf(4), BigInteger.valueOf(12))));
    passedAll &= testEquals("IntegerConstructor-1", BigRational.valueOf(5), "5");
    passedAll &= testEquals("IntegerConstructor-2", BigRational.valueOf(5, 12), new BigRational(BigInteger.valueOf(30), BigInteger.valueOf(72)), "5/12");
    passedAll &= testEquals("LongConstructor-1", BigRational.valueOf(10000000000L), "10000000000");
    passedAll &= testEquals("LongConstructor-2", BigRational.valueOf(50000000000L, 120000000000L), new BigRational(BigInteger.valueOf(300000000000L), BigInteger.valueOf(720000000000L)), "5/12");
    passedAll &= testEquals("BigDecimalConstructor-1", BigRational.valueOf(new BigDecimal("7.3412359")), "73412359/10000000");
    passedAll &= testEquals("BigDecimalConstructor-2", BigRational.valueOf(new BigDecimal("7.3412359"), new BigDecimal("2.6876980111")), BigRational.valueOf(15791000, 5781239), "15791000/5781239");
    // Derived from BigDecimal documentation, 0.1 is exactly 0.1000000000000000055511151231257827021181583404541015625. Simplified via WolframAlpha
    passedAll &= testEquals("DoubleConstructor-1", BigRational.valueOf(0.1), BigRational.valueOf(new BigDecimal(0.1)), "3602879701896397/36028797018963968");
    passedAll &= testEquals("DoubleConstructor-2", BigRational.valueOf(0.1, 0.2), BigRational.valueOf(1, 2), "1/2");
    passedAll &= testEquals("StringConstructor-1", BigRational.valueOf("18"), BigRational.valueOf(18), "18");
    passedAll &= testEquals("StringConstructor-2", BigRational.valueOf("18/-4"), BigRational.valueOf(9, -2), "-9/2");
    passedAll &= testEquals("StringConstructor-3", BigRational.valueOf("18", "-4"), BigRational.valueOf(9, -2), "-9/2");
    passedAll &= testEquals("AbsoluteValue", BigRational.valueOf(-1, 2).abs(), BigRational.valueOf(1, 2), "1/2");
    passedAll &= testEquals("Addition-1", BigRational.valueOf("2/5").add(BigRational.valueOf("3/7")), "29/35");
    passedAll &= testEquals("Addition-2", BigRational.valueOf("2/5").add(BigRational.valueOf("-3/7")), "-1/35");
    passedAll &= testEquals("Addition-3", BigRational.valueOf("-2/5").add(BigRational.valueOf("3/7")), "1/35");
    passedAll &= testEquals("Addition-4", BigRational.valueOf("-2/5").add(BigRational.valueOf("-3/7")), "-29/35");
    passedAll &= testEquals("Addition-5", BigRational.valueOf("2/5").add(BigRational.valueOf("4/5")), "6/5");
    passedAll &= testEquals("Subtraction-1", BigRational.valueOf("2/5").subtract(BigRational.valueOf("3/7")), "-1/35");
    passedAll &= testEquals("Subtraction-2", BigRational.valueOf("2/5").subtract(BigRational.valueOf("-3/7")), "29/35");
    passedAll &= testEquals("Subtraction-3", BigRational.valueOf("-2/5").subtract(BigRational.valueOf("3/7")), "-29/35");
    passedAll &= testEquals("Subtraction-4", BigRational.valueOf("-2/5").subtract(BigRational.valueOf("-3/7")), "1/35");
    passedAll &= testEquals("Subtraction-5", BigRational.valueOf("2/7").subtract(BigRational.valueOf("3/7")), "-1/7");
    passedAll &= testTrue("Comparison-1", BigRational.valueOf("2/5").compareTo(BigRational.valueOf("-2/5")) > 0);
    passedAll &= testTrue("Comparison-2", BigRational.valueOf("-2/5").compareTo(BigRational.valueOf("2/5")) < 0);
    passedAll &= testTrue("Comparison-3", BigRational.valueOf("2/5").compareTo(BigRational.valueOf("3/7")) < 0);
    passedAll &= testTrue("Comparison-4", BigRational.valueOf("-2/5").compareTo(BigRational.valueOf("-3/7")) > 0);
    passedAll &= testEquals("Increment-1", BigRational.valueOf("2/5").increment(), "7/5");
    passedAll &= testEquals("Increment-2", BigRational.valueOf("-2/5").increment(), "3/5");
    passedAll &= testEquals("Increment-3", BigRational.valueOf("-7/5").increment(), "-2/5");
    passedAll &= testEquals("Decrement-1", BigRational.valueOf("7/5").decrement(), "2/5");
    passedAll &= testEquals("Decrement-2", BigRational.valueOf("2/5").decrement(), "-3/5");
    passedAll &= testEquals("Decrement-3", BigRational.valueOf("-2/5").decrement(), "-7/5");
    passedAll &= testEquals("Divide-1", BigRational.valueOf("2/5").divide(BigRational.valueOf("3/5")), "2/3");
    passedAll &= testEquals("Divide-2", BigRational.valueOf("-2/5").divide(BigRational.valueOf("3/5")), "-2/3");
    passedAll &= testEquals("Divide-3", BigRational.valueOf("-2/5").divide(BigRational.valueOf("-3/5")), "2/3");
    passedAll &= testTrue("DoubleValue-1", BigRational.valueOf("0.1").doubleValue() == 0.1);
    passedAll &= testTrue("DoubleValue-2", BigRational.valueOf("1.1").doubleValue() == 1.1);
    passedAll &= testTrue("IntValue-1", BigRational.valueOf("7/3").intValue() == 2);
    passedAll &= testTrue("IntValue-2", BigRational.valueOf("8/3").intValue() == 2);
    passedAll &= testTrue("IntValue-2", BigRational.valueOf("9/3").intValue() == 3);
    passedAll &= testEquals("ToBigDecimal-1", BigRational.valueOf("0/3").toBigDecimal(10, RoundingMode.HALF_EVEN), "0.0000000000");
    passedAll &= testEquals("ToBigDecimal-2", BigRational.valueOf("1/3").toBigDecimal(10, RoundingMode.HALF_EVEN), "0.3333333333");
    passedAll &= testEquals("ToBigDecimal-3", BigRational.valueOf("2/3").toBigDecimal(10, RoundingMode.HALF_EVEN), "0.6666666667");
    passedAll &= testEquals("ToBigDecimal-4", BigRational.valueOf("3/3").toBigDecimal(10, RoundingMode.HALF_EVEN), "1.000000000");
    passedAll &= testEquals("ToBigDecimal-5", BigRational.valueOf("4/3").toBigDecimal(10, RoundingMode.HALF_EVEN), "1.333333333");
    passedAll &= testEquals("ToBigDecimal-6", BigRational.valueOf("5/3").toBigDecimal(10, RoundingMode.HALF_EVEN), "1.666666667");
    passedAll &= testEquals("ToBigDecimal-7", BigRational.valueOf("-1/3").toBigDecimal(10, RoundingMode.HALF_EVEN), "-0.3333333333");
    passedAll &= testEquals("ToBigDecimal-8", BigRational.valueOf("-2/3").toBigDecimal(10, RoundingMode.HALF_EVEN), "-0.6666666667");
    passedAll &= testEquals("ToBigDecimal-9", BigRational.valueOf("-3/3").toBigDecimal(10, RoundingMode.HALF_EVEN), "-1.000000000");
    passedAll &= testEquals("Max-1", BigRational.valueOf("2/5").max(BigRational.valueOf("3/7")), "3/7");
    passedAll &= testEquals("Max-2", BigRational.valueOf("2/5").max(BigRational.valueOf("-3/7")), "2/5");
    passedAll &= testEquals("Max-3", BigRational.valueOf("-2/5").max(BigRational.valueOf("-3/7")), "-2/5");
    passedAll &= testEquals("Min-1", BigRational.valueOf("2/5").min(BigRational.valueOf("3/7")), "2/5");
    passedAll &= testEquals("Min-2", BigRational.valueOf("2/5").min(BigRational.valueOf("-3/7")), "-3/7");
    passedAll &= testEquals("Min-3", BigRational.valueOf("-2/5").min(BigRational.valueOf("-3/7")), "-3/7");
    Object[] mixedFraction1 = BigRational.valueOf("7/3").mixedFraction();
    passedAll &= testEquals("MixedFraction-1", mixedFraction1[0], "2");
    passedAll &= testEquals("MixedFraction-2", mixedFraction1[1], "1/3");
    Object[] mixedFraction2 = BigRational.valueOf("-7/3").mixedFraction();
    passedAll &= testEquals("MixedFraction-3", mixedFraction2[0], "-2");
    passedAll &= testEquals("MixedFraction-4", mixedFraction2[1], "-1/3");
    passedAll &= testEquals("Multiply-1", BigRational.valueOf("2/3").multiply(BigRational.valueOf("3/5")), "2/5");
    passedAll &= testEquals("Multiply-2", BigRational.valueOf("-2/3").multiply(BigRational.valueOf("3/5")), "-2/5");
    passedAll &= testEquals("Multiply-3", BigRational.valueOf("2/3").multiply(BigRational.valueOf("-3/5")), "-2/5");
    passedAll &= testEquals("Multiply-4", BigRational.valueOf("-2/3").multiply(BigRational.valueOf("-3/5")), "2/5");
    passedAll &= testEquals("Multiply-5", BigRational.valueOf("2/3").multiply(BigInteger.valueOf(4)), "8/3");
    passedAll &= testEquals("Multiply-6", BigRational.valueOf("-2/3").multiply(BigInteger.valueOf(4)), "-8/3");
    passedAll &= testEquals("Multiply-7", BigRational.valueOf("2/3").multiply(BigInteger.valueOf(-4)), "-8/3");
    passedAll &= testEquals("Multiply-8", BigRational.valueOf("-2/3").multiply(BigInteger.valueOf(-4)), "8/3");
    passedAll &= testEquals("Negate-1", BigRational.valueOf("2/3").negate(), "-2/3");
    passedAll &= testEquals("Negate-2", BigRational.valueOf("-2/3").negate(), "2/3");
    passedAll &= testEquals("Power-1", BigRational.valueOf("2/3").pow(3), "8/27");
    passedAll &= testEquals("Power-2", BigRational.valueOf("-2/3").pow(3), "-8/27");
    passedAll &= testEquals("Power-3", BigRational.valueOf("-2/3").pow(4), "16/81");
    passedAll &= testEquals("Reciprocal-1", BigRational.valueOf("2/3").reciprocal(), "3/2");
    passedAll &= testEquals("Reciprocal-2", BigRational.valueOf("-2/3").reciprocal(), "-3/2");
    passedAll &= testEquals("Reciprocal-3", BigRational.valueOf("1/7").reciprocal(), "7");
    passedAll &= testEquals("Reciprocal-4", BigRational.valueOf("-1/7").reciprocal(), "-7");
    passedAll &= testTrue("Signum-1", BigRational.valueOf("1/7").signum() == 1);
    passedAll &= testTrue("Signum-2", BigRational.valueOf("0/7").signum() == 0);
    passedAll &= testTrue("Signum-3", BigRational.valueOf("-1/7").signum() == -1);
    passedAll &= testEquals("Numerator-1", BigRational.valueOf("2/7").numerator(), "2");
    passedAll &= testEquals("Numerator-2", BigRational.valueOf("-2/7").numerator(), "2");
    passedAll &= testEquals("Denominator-1", BigRational.valueOf("2/7").denominator(), "7");
    passedAll &= testEquals("Denominator-2", BigRational.valueOf("-2/7").denominator(), "7");
    
    System.out.println(passedAll ? "Passed all tests" : "FAILURE: DID NOT PASS ALL TESTS");
    return passedAll;
  }
  
  public static void main(String[] args)
  {
    testFeatures();
    return;
  }
  
}

```

