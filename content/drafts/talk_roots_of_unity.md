+++
title = "Talk:Roots of unity"
description = ""
date = 2019-09-08T15:39:53Z
aliases = []
[extra]
id = 2480
[taxonomies]
categories = []
tags = []
+++

== Seed7 output ==
I think the Seed7 output has its real and imaginary parts reversed. (1.000*i)<sup>2</sup> is -1 not 1. --[[User:Mwn3d|Mwn3d]] 07:14, 21 January 2008 (MST)

Seed7 uses a similar convention as Algol68:
The i is the separator between real and imaginary part
(well in Algol68 it seems to be an i with an underscore which IIRC is a different alphabet).
The output of -0.5000i+0.8660 means -0.5000+i*0.8660 which might be counter intuitive.
I choosed it that way to make the 'parse' function (which converts a string to a complex) easy implementable as:

```txt

(**
 *  Return the conversion of a string to a complex.
 *)
const func complex: (attr complex) parse (in string: stri) is func
  result
    var complex: result is complex.value;
  local
    var integer: iPos is 0;
  begin
    iPos := pos(stri, 'i'); # Find the position of the i
    if iPos <> 0 then
      result.re := float parse (stri[.. pred(iPos)]);
      result.im := float parse (stri[succ(iPos) ..]);
    else
      raise RANGE_ERROR;
    end if;
  end func;

```

Maybe you have suggestions of how the output of a complex number should look like.
If I write a complex number as -0.5000+i*0.8660 the 'parse' function could be:

```txt

(**
 *  Return the conversion of a string to a complex.
 *)
const func complex: (attr complex) parse (in string: stri) is func
  result
    var complex: result is complex.value;
  local
    var integer: iPos is 0;
  begin
    iPos := pos(stri, 'i'); # Find the position of the i
    if iPos > 1 then
      result.re := float parse (stri[.. iPos - 2]);
      result.im := float parse (stri[iPos + 2 ..]);
      if stri[pred(iPos)] = '-' then
        result.im := -result.im;
      elsif stri[pred(iPos)] <> '-' then
        raise RANGE_ERROR;
      end if;
      if stri[succ(iPos) len 1] <> "*" then
        raise RANGE_ERROR;
      end if;
    else
      raise RANGE_ERROR;
    end if;
  end func;

```

What do you think? [[User:Thomas Mertes|Thomas Mertes]] 09:35, 21 January 2008 (MST)
:Whatever is easier is fine as long as other people can figure it out. Maybe the output should explained a bit in the example. --[[User:Mwn3d|Mwn3d]] 09:52, 21 January 2008 (MST)
::I have reconsidered the issue aggain. The output format of a complex number should be self-explanatory. Your question showed me that my solution was counter intuitive. Now I think that writing a complex number as -0.5000+0.8660i would be the least confusing solution. This is also the mathematical notation and the format used by perl and similar to the format used in some other languages.
The Seed7 'parse' function to convert a string in the format -0.5000+0.8660i to a complex would be:

```txt

(**
 *  Return the conversion of a string to a complex.
 *)
const func complex: (attr complex) parse (in string: stri) is func
  result
    var complex: result is complex.value;
  local
    var integer: pos is 0;
    var integer: pos2 is 0;
  begin
    pos := rpos(stri, '+');
    pos2 := rpos(stri, '-');
    if pos2 > pos then
      pos := pos2;
    end if;
    if pos <> 0 and stri[length(stri)] = 'i' then
      result.re := float parse (stri[.. pred(pos)]);
      result.im := float parse (stri[pos .. pred(length(stri))]);
    else
      raise RANGE_ERROR;
    end if;
  end func;

```

This function is less complicated than I thought. The sign of the imaginary part is used as separator between real and imaginary part.
I will include this change in the next release of Seed7. After the release I will change the output of the "Roots of unity" example. [[User:Thomas Mertes|Thomas Mertes]] 06:36, 27 January 2008 (MST)

== Icon question ==

Is the parameter n to the procedure str_rep used? It's not an Icon secret is it? --[[User:Mwn3d|Mwn3d]] 21:30, 10 December 2008 (UTC)

oops :) , thanks for correcting. [[User:Rahul|Rahul]] 21:49, 10 December 2008 (UTC)
