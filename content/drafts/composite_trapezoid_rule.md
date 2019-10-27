+++
title = "Composite Trapezoid Rule"
description = ""
date = 2018-04-08T13:56:52Z
aliases = []
[extra]
id = 21771
[taxonomies]
categories = []
tags = []
+++

In numerical analysis, the trapezoidal rule is used for approximation of a definite integral. The code here is a general purpose code for any equation.
[https://en.wikipedia.org/wiki/Trapezoidal_rule]


== MATLAB ==

function integral = trapezoid(f, a, b, n)
    x = (b-a)/n;
    result = 0.5*f(a) + 0.5*f(b);
    for i = 1:(n-1)
        result = result + f(a + i*x);
    end
    integral = x*result;
end

f is the equation, a is the lower limit, b is the upper limit, and n is the number of trapezoids or number of integration points.
