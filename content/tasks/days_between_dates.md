+++
title = "Days between dates"
description = ""
date = 2019-10-18T08:21:10Z
aliases = []
[extra]
id = 22566
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "awk",
  "csharp",
  "d",
  "erlang",
  "factor",
  "go",
  "lua",
  "perl",
  "perl_6",
  "python",
  "rexx",
  "ruby",
  "sidef",
  "zkl",
]
+++

## Task

Calculate the number of days between two dates. Date input should be of the form YYYY-MM-DD.

;Motivation

To demonstrate one of the numerous ways this can be done.



## AWK


```AWK

# syntax: GAWK -f DAYS_BETWEEN_DATES.AWK
BEGIN {
    regexp = "^....-..-..$" # YYYY-MM-DD
    main("1969-12-31","1970-01-01","builtin has bad POSIX start date")
    main("1970-01-01","2038-01-19","builtin has bad POSIX stop date")
    main("1970-01-01","2019-10-02","format OK")
    main("1970-01-01","2019/10/02","format NG")
    main("1995-11-21","1995-11-21","identical dates")
    main("2019-01-01","2019-01-02","positive date")
    main("2019-01-02","2019-01-01","negative date")
    main("2019-01-01","2019-03-01","non-leap year")
    main("2020-01-01","2020-03-01","leap year")
    exit(0)
}
function main(date1,date2,comment,  d1,d2,diff) {
    printf("\t%s\n",comment)
    d1 = days_builtin(date1)
    d2 = days_builtin(date2)
    diff = (d1 == "" || d2 == "") ? "error" : d2-d1
    printf("builtin %10s to %10s = %s\n",date1,date2,diff)
    d1 = days_generic(date1)
    d2 = days_generic(date2)
    diff = (d1 == "" || d2 == "") ? "error" : d2-d1
    printf("generic %10s to %10s = %s\n",date1,date2,diff)
}
function days_builtin(ymd) { # use gawk builtin
    if (ymd !~ regexp) { return("") }
    if (ymd < "1970-01-01" || ymd > "2038-01-18") { return("") } # outside POSIX range
    gsub(/-/," ",ymd)
    return(int(mktime(sprintf("%s 0 0 0",ymd)) / (60*60*24)))
}
function days_generic(ymd,  d,m,y,result) { # use Python formula
    if (ymd !~ regexp) { return("") }
    y = substr(ymd,1,4)
    m = substr(ymd,6,2)
    d = substr(ymd,9,2)
    m = (m + 9) % 12
    y = int(y - int(m/10))
    result = 365*y + int(y/4) - int(y/100) + int(y/400) + int((m*306+5)/10) + (d-1)
    return(result)
}

```

```txt

    builtin has bad POSIX start date
builtin 1969-12-31 to 1970-01-01 = error
generic 1969-12-31 to 1970-01-01 = 1
    builtin has bad POSIX stop date
builtin 1970-01-01 to 2038-01-19 = error
generic 1970-01-01 to 2038-01-19 = 24855
    format OK
builtin 1970-01-01 to 2019-10-02 = 18171
generic 1970-01-01 to 2019-10-02 = 18171
    format NG
builtin 1970-01-01 to 2019/10/02 = error
generic 1970-01-01 to 2019/10/02 = error
    identical dates
builtin 1995-11-21 to 1995-11-21 = 0
generic 1995-11-21 to 1995-11-21 = 0
    positive date
builtin 2019-01-01 to 2019-01-02 = 1
generic 2019-01-01 to 2019-01-02 = 1
    negative date
builtin 2019-01-02 to 2019-01-01 = -1
generic 2019-01-02 to 2019-01-01 = -1
    non-leap year
builtin 2019-01-01 to 2019-03-01 = 59
generic 2019-01-01 to 2019-03-01 = 59
    leap year
builtin 2020-01-01 to 2020-03-01 = 60
generic 2020-01-01 to 2020-03-01 = 60

```


## C#


```c#
using System;
using System.Globalization;

public class Program
{
    public static void Main() => WriteLine(DateDiff("1970-01-01", "2019-10-18"));

    public static int DateDiff(string d1, string d2) {
        var a = DateTime.ParseExact(d1, "yyyy-MM-dd", CultureInfo.InvariantCulture);
        var b = DateTime.ParseExact(d2, "yyyy-MM-dd", CultureInfo.InvariantCulture);
        return (int)(b - a).TotalDays;
    }
}
```

```txt

18187

```



## D


```d
import std.datetime.date;
import std.stdio;

void main() {
    auto fromDate = Date.fromISOExtString("2019-01-01");
    auto toDate = Date.fromISOExtString("2019-10-07");
    auto diff = toDate - fromDate;
    writeln("Number of days between ", fromDate, " and ", toDate, ": ", diff.total!"days");
}
```

```txt
Number of days between 2019-Jan-01 and 2019-Oct-07: 279
```



## Erlang


```erlang


-module(daysbetween).
-export([between/2,dateToInts/2]).

% Return Year or Month or Date from datestring
dateToInts(String, POS) ->
  list_to_integer( lists:nth( POS, string:tokens(String, "-") ) ).

% Alternative form of above
% dateToInts(String,POS) ->
%   list_to_integer( lists:nth( POS, re:split(String ,"-", [{return,list},trim]) ) ).

% Return the number of days between dates formatted "2019-09-30"
between(DateOne,DateTwo) ->
  L = [1,2,3],
  [Y1,M1,D1] =  [ dateToInts(DateOne,X) || X <- L],
  [Y2,M2,D2] =  [ dateToInts(DateTwo,X) || X <- L],
  GregOne = calendar:date_to_gregorian_days(Y1,M1,D1),
  GregTwo = calendar:date_to_gregorian_days(Y2,M2,D2),
  GregTwo - GregOne.



```

erlang shell:

```txt

30> c(daysbetween).
c(daysbetween).
{ok,daysbetween}
31> daysbetween:between("2019-01-01", "2019-09-30").
daysbetween:between("2019-01-01", "2019-09-30").
272

```



## Factor

Factor supports the addition and subtraction of timestamps and durations with the <code>time+</code> and <code>time-</code> words.

```factor
USING: calendar calendar.parser kernel math prettyprint ;

: days-between ( ymd-str ymd-str -- n )
    [ ymd>timestamp ] bi@ time- duration>days abs ;

"2019-01-01" "2019-09-30" days-between .
"2016-01-01" "2016-09-30" days-between .  ! leap year
```

```txt

272
273

```



## Go


```go
package main

import (
    "fmt"
    "log"
    "time"
)

const layout = "2006-01-02" // template for time.Parse

// Parameters assumed to be in YYYY-MM-DD format.
func daysBetween(date1, date2 string) int {
    t1, err := time.Parse(layout, date1)
    check(err)
    t2, err := time.Parse(layout, date2)
    check(err)
    days := int(t1.Sub(t2).Hours() / 24)
    if days < 0 {
        days = -days
    }
    return days
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    date1, date2 := "2019-01-01", "2019-09-30"
    days := daysBetween(date1, date2)
    fmt.Printf("There are %d days between %s and %s\n", days, date1, date2)

    date1, date2 = "2015-12-31", "2016-09-30"
    days = daysBetween(date1, date2)
    fmt.Printf("There are %d days between %s and %s\n", days, date1, date2)
}
```


```txt

There are 272 days between 2019-01-01 and 2019-09-30
There are 274 days between 2015-12-31 and 2016-09-30

```



## Lua

This uses os.difftime to compare two Epoch times. Not to be used with dates before 1970.

```lua
SECONDS_IN_A_DAY = 60 * 60 * 24

-- Convert date string as YYYY-MM-DD to Epoch time.
function parseDate (str)
  local y, m, d = string.match(str, "(%d+)-(%d+)-(%d+)")
  return os.time({year = y, month = m, day = d})
end

-- Main procedure
io.write("Enter date 1: ")
local d1 = parseDate(io.read())
io.write("Enter date 2: ")
local d2 = parseDate(io.read())
local diff = math.ceil(os.difftime(d2, d1) / SECONDS_IN_A_DAY)
print("There are " .. diff .. " days between these dates.")
```

```txt
Enter date 1: 1970-01-01
Enter date 2: 2019-10-02
There are 18171 days between these dates.
```



## Perl

Would not reinvent this wheel.

```perl
use feature 'say';
use Date::Calc qw(Delta_Days);

say Delta_Days(2018,7,13, 2019,9,13);   # triskaidekaphobia
say Delta_Days(1900,1,1,  2000,1,1);    # a century
say Delta_Days(2000,1,1,  2100,1,1);    # another, with one extra leap day
say Delta_Days(2020,1,1,  2019,10,1);   # backwards in time
say Delta_Days(2019,2,29, 2019,3,1);    # croaks
```

```txt
427
36524
36525
-92
Date::Calc::PP::Delta_Days(): Date::Calc::Delta_Days(): not a valid date at Days_between_dates line 10
```



## Perl 6

Dates are first class objects in Perl 6 and may have arithmetic in days done directly on them.

```perl6
say Date.new('2019-09-30') - Date.new('2019-01-01');

say Date.new('2019-03-01') - Date.new('2019-02-01');

say Date.new('2020-03-01') - Date.new('2020-02-01');

say Date.new('2029-03-29') - Date.new('2019-03-29');

say Date.new('2019-01-01') + 90;

say Date.new('2020-01-01') + 90;

say Date.new('2019-02-29') + 30;

CATCH { default { .message.say; exit; } };
```



```txt
272
28
29
3653
2019-04-01
2020-03-31
Day out of range. Is: 29, should be in 1..28
```



## Python


```python


#!/usr/bin/python
import sys

''' Difference between two dates = g(y2,m2,d2) - g(y1,m1,d1)
    Where g() gives us the Gregorian Calendar Day
    Inspired  by discussion at:
    https://stackoverflow.com/questions/12862226/the-implementation-of-calculating-the-number-of-days-between-2-dates
'''

def days( y,m,d ):
  ''' input year and month are shifted to begin the year in march'''
  m = (m + 9) % 12
  y = y - m/10

  ''' with (m*306 + 5)/10 the number of days from march 1 to the current 'm' month '''
  result = 365*y + y/4 - y/100 + y/400 + (m*306 + 5)/10 + ( d - 1 )
  return result

def diff(one,two):
  [y1,m1,d1] = one.split('-')
  [y2,m2,d2] = two.split('-')
  # strings to integers
  year2 = days( int(y2),int(m2),int(d2))
  year1 = days( int(y1), int(m1), int(d1) )
  return year2 - year1

if __name__ == "__main__":
  one = sys.argv[1]
  two = sys.argv[2]
  print diff(one,two)

```


```txt
python days-between.py 2019-01-01 2019-09-30
272

```



## REXX

### bare bones version

Programming note:   the   '''B'''   ('''B'''ase)   an option for the   '''date'''   BIF which indicates to compute the number of

days since the beginning of the Gregorian calendar,   and   '''I'''   which is the option that indicates the date is in

the   '''ISO'''   (International Standards Organization standard 8601:2004)   format.

```rexx
/*REXX program computes the number of days between two dates in the form of  YYYY-MM-DD */
parse arg $1 $2 .                                /*get 2 arguments (dates) from the C.L.*/
say abs( date('B',$1,"I")  -  date('B',$2,"I") )   ' days between '    $1    " and "    $2
                                                 /*stick a fork in it,  we're all done. */
```

```txt

6848  days between  2019-10-02  and  2000-01-01

```



### supports more variations

This REXX version supports more variations in the date format   (allows a single digit month and/or day), as well as

allowing a single asterisk   ('''*''')   to be used for a date   (which signifies that the current date is to be used).

Commas   (''',''')   are inserted into numbers where appropriate.

Also, more informative error messages are generated.

```rexx
/*REXX program computes the number of days between two dates in the form of  YYYY-MM-DD */
parse arg $.1 $.2 _ . 1 . . xtra                 /*obtain two arguments from the  C.L.  */
if $.1=='*'  then $.1= date("I")                 /*obtain current date if it's an *.    */
if $.2=='*'  then $.2= date("I")                 /*   "      "      "   "   "   "  "    */
parse var $.1 yr.1 '-' mon.1 "-" dd.1            /*obtain the constituents of 1st date. */
parse var $.2 yr.2 '-' mon.2 "-" dd.2            /*   "    "       "        " 2nd   "   */
?.1= '1st'
?.2= '2nd'
if _ \== ''              then call err "too many arguments specified: "   xtra
dy.=31                                           /*default number of days for all months*/
parse value 30 with dy.4 1 dy.6 1 dy.9 1 dy.11   /*define 30─day months, Feb. is special*/
@notCorr= "isn't in the correct format:  YYYY-MM-DD "

  do j=1  for 2                                  /*examine both dates for correct format*/
  if $.j           ==''  then call err ?.j "date was not specified."
  if length(yr.j)==0     then call err ?.j "year"  @notCorr '(missing)'
  if isDec(yr.j)         then call err ?.j "year"  @notCorr '(has a non─decimal digit)'
  if yr.j<1 | yr.j>9999  then call err ?.j "year"  @notCorr '(not in the range 1──►9999)'
  if length(mon.j)==0    then call err ?.j "month" @notCorr '(missing)'
  if isDec(mon.j)        then call err ?.j "month" @notCorr '(has a non─decimal digit)'
  if mon.j<1 | mon.j>12  then call err ?.j "month" @notCorr '(not in the range 1──►12)'
  if length(dd.j)==0     then call err ?.j "day"   @notCorr '(missing)'
  if isDec(dd.j)         then call err ?.j "day"   @notCorr '(has a non─decimal digit)'
  mo= mon.j
  if leapYr(yr.j)  then dy.2= 29                 /*Is it a leapyear? Use 29 days for Feb*/
                   else dy.2= 28                 /*Isn't "     "      "  28   "   "   " */
  if dd.j<1 | dd.j>dy.mo then call err ?.j "day"   @notCorr '(day in month is invalid)'

    yr.j= right( yr.j  +0, 4, 0)                 /*force YYYY to be four decimal digits.*/
   mon.j= right(mon.j  +0, 2, 0)                 /*  "    MON  "  "  two    "       "   */
    dd.j= right( dd.j  +0, 2, 0)                 /*  "     DD  "  "   "     "       "   */
     $.j= yr.j'-'mon.j"-"dd.j                    /*reconstitute a date from above parts.*/
  end       /*j*/

say commas(abs(date('B',$.1,"I") -date('B',$.2,"I")))  ' days between '  $.1  " and "  $.2
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: parse arg _; do c_=length(_)-3  to 1  by -3; _=insert(',', _, c_);  end;  return _
err:    say; say '***error*** ' arg(1);  exit 13 /*issue an error message (with text)   */
isDec:  return verify( arg(1), 1234567890) \== 0 /*insure argument is just decimal digs.*/
leapYr: arg _; ly=_//4==0; if ly==0  then return 0; ly=((_//100\==0)|_//400==0); return ly
```

Today   (indicated by the asterisk)   is   2019-10-2

```txt

6,848  days between  2019-10-02  and  2000-01-01

```



## Ruby


```ruby
require "date"

d1, d2 = Date.parse("2019-1-1"), Date.parse("2019-10-19")

p (d1 - d2).to_i  # => -291
p (d2 - d1).to_i  # => 291

```



## Sidef


```ruby
require('Date::Calc')

func days_diff(a,b) {
    %S<Date::Calc>.Delta_Days(a.split('-')..., b.split('-')...)
}

var date1 = "1970-01-01"
var date2 = "2019-10-02"

say "Date 1: #{date1}"
say "Date 2: #{date2}"

var days = days_diff(date1, date2)

say "There are #{days} days between these dates"
```

```txt

Date 1: 1970-01-01
Date 2: 2019-10-02
There are 18171 days between these dates

```



## zkl


```zkl
var [const] TD=Time.Date;
today:=TD.parseDate("--");  // "yyyy-mm-dd" and variations --> (y,m,d)
// or Time.Clock.UTC  --> (y,m,d,h,m,s)
then:=TD.parseDate("2018-9-30");
diff:=TD.deltaDays(then,today.xplode());  // ( (y,m,d), y,m,d )
println("Number of days between %s and %s: %d".fmt(then,today,diff));
println("Number of days between %s and %s: %d".fmt(
   TD.toYMDString(today.xplode()),	// to(y,m,d) not to((y,m,d))
   TD.toYMDString(then.xplode()),
   TD.deltaDays(today,then.xplode())));
```

```txt

Number of days between L(2018,9,30) and L(2019,9,30): 365
Number of days between 2019-09-30 and 2018-09-30: -365

```

