+++
title = "Category:System.Numerics"
description = ""
date = 2019-01-05T21:27:08Z
aliases = []
[extra]
id = 22127
[taxonomies]
categories = []
tags = []
+++

System.Numerics is a library used by Visual Studio to implement the '''BigInteger''' Class (among others).  Although it does not directly support ''BigRationals'' or ''BigFloats'', one can usually figure out a way to deal with placing a decimal point properly to simulate arbitrary precision floating point operations.

[https://docs.microsoft.com/en-us/dotnet/api/system.numerics?view=netframework-4.7.2 '''System.Numerics'''] MS documentation for .NET Framework 4.7.2<br/>
[https://docs.microsoft.com/en-us/dotnet/api/system.numerics?view=netcore-2.2 '''System.Numerics'''] MS documentaion for .NET Core 2.2)<br/>
''Links added in early 2018, if they don't work, try googling.''

'''Classes:'''<br/> 
 '''Vector''' - Provides a collection of static convenience methods for creating, manipulating, combining, and converting generic vectors.                           ''Note: available in NET.Core only.''

'''Structs:'''<br/>
 '''BigInteger''' - Represents an arbitrarily large signed integer.<br/>
 '''Complex''' - Represents a complex number.<br/>
 '''Matrix3x2''' - Represents a 3x2 matrix.<br/>
 '''Matrix4x4''' - Represents a 4x4 matrix.<br/>
 '''Plane''' - Represents a plane in three-dimensional space.<br/>
 '''Quaternion''' - Represents a vector that is used to encode three-dimensional physical rotations.<br/>
 '''Vector<T>''' - Represents a single vector of a specified numeric type that is suitable for low-level                                                              optimization of parallel algorithms.  ''Note: available in NET.Core only.''<br/>
 '''Vector2''' - Represents a vector with two single-precision floating-point values.<br/>
 '''Vector3''' - Represents a vector with three single-precision floating-point values.<br/>
 '''Vector4''' - Represents a vector with four single-precision floating-point values.

Remarks about the structures and vector types of the System.Numerics namespace:<br/>

The '''BigInteger''' structure, which is a nonprimitive integral type that supports arbitrarily large integers. An integral primitive such as Byte or Int32 includes a ''MinValue'' and a ''MaxValue'' property, which define the lower bound and upper bound supported by that data type. In contrast, the '''BigInteger''' structure has no lower or upper bound, and can contain the value of any integer. 

The '''Complex''' structure, which represents a complex number. A complex number is a number in the form a + bi, where a is the real part, and b is the imaginary part. 

The SIMD-enabled vector types, which include '''Vector2''', '''Vector3''', '''Vector4''', '''Matrix3x2''', '''Matrix4x4''', '''Plane''', and '''Quaternion'''.<br/><br/>

'''BigInteger''' operators and methods:

Operators implemented in the '''BigInteger''' class include Addition, BitwiseAnd, BitwiseOr, Decrement, Division, Equality, ExclusiveOr, Explicit, GreaterThan, GreaterThanOrEqual, Implicit, Increment, Inequality, LeftShift, LessThan, LessThanOrEqual, Modulus, Multiply, OnesComplement, RightShift, Subtraction, UnaryNegation, and UnaryPlus.

Methods implemented in the '''BigInteger''' class include Abs, Add, Compare, CompareTo, Divide, DivRem, Equals, GetByteCount, GetHashCode, GreatestCommonDivisor, Log, Log10, Max, Min, ModPow, Multiply, Negate, Parse, Pow, Remainder, Subtract, ToByteArray, ToString, TryFormat, TryParse, and TryWriteBytes.
