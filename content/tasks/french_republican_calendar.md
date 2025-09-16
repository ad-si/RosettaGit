+++
title = "French Republican calendar"
description = ""
date = 2019-02-25T22:05:03Z
aliases = []
[extra]
id = 21109
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "bbc_basic",
  "freebasic",
  "go",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "sidef",
]
+++

Write a program to convert dates between the [[wp:Gregorian calendar|Gregorian calendar]] and the [[wp:French Republican Calendar|French Republican calendar]].

The year 1 of the Republican calendar began on 22 September 1792. There were twelve months (Vendémiaire, Brumaire, Frimaire, Nivôse, Pluviôse, Ventôse, Germinal, Floréal, Prairial, Messidor, Thermidor, and Fructidor) of 30 days each, followed by five intercalary days or <i>Sansculottides</i> (<i>Fête de la vertu</i> / Virtue Day, <i>Fête du génie</i> / Talent Day, <i>Fête du travail</i> / Labour Day, <i>Fête de l'opinion</i> / Opinion Day, and <i>Fête des récompenses</i> / Honours Day). In leap years (the years 3, 7, and 11) a sixth <i>Sansculottide</i> was added: <i>Fête de la Révolution</i> / Revolution Day.

As a minimum, your program should give correct results for dates in the range from 1 Vendémiaire 1 = 22 September 1792 to 10 Nivôse 14 = 31 December 1805 (the last day when the Republican calendar was officially in use). If you choose to accept later dates, be aware that there are several different methods (described on [[wp:French Republican Calendar|the Wikipedia page]]) about how to determine leap years after the year 14. You should indicate which method you are using. (Because of these different methods, correct programs may sometimes give different results for dates after 1805.)

Test your program by converting the following dates both from Gregorian to Republican and from Republican to Gregorian:

• 1 Vendémiaire 1 = 22 September 1792

• 1 Prairial 3 = 20 May 1795

• 27 Messidor 7 = 15 July 1799 <i>(Rosetta Stone discovered)</i>

• Fête de la Révolution 11 = 23 September 1803

• 10 Nivôse 14 = 31 December 1805



## BBC BASIC

Computes leap years using the "continuous" method: a year in the Republican calendar is a leap year if and only if the number of the <i>following</i> year is divisible by 4 but not by 100 unless also by 400. No attempt is made to deal with ill-formed or invalid input dates.

```bbcbasic
REM >
frrepcal
:
DIM gregorian$(11)
DIM gregorian%(11)
DIM republican$(11)
DIM sansculottides$(5)
gregorian$() = "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"
gregorian%() = 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
REM 7-bit ASCII encoding, so no accents on French words
republican$() = "Vendemiaire", "Brumaire", "Frimaire", "Nivose", "Pluviose", "Ventose", "Germinal", "Floreal", "Prairial", "Messidor", "Thermidor", "Fructidor"
sansculottides$() = "Fete de la vertu", "Fete du genie", "Fete du travail", "Fete de l'opinion", "Fete des recompenses", "Fete de la Revolution"
:
PRINT "*** French  Republican ***"
PRINT "*** calendar converter ***"
PRINT "Enter a date to convert, in the format 'day month year'"
PRINT "e.g.: 1 Prairial 3,"
PRINT "      20 May 1795."
PRINT "For Sansculottides, use 'day year'"
PRINT "e.g.: Fete de l'opinion 9."
PRINT "Or just press 'RETURN' to exit the program."
PRINT
REPEAT
  INPUT LINE "> " src$
  IF src$ <> "" THEN
    PROC_split(src$, day%, month%, year%)
    REM for simplicity, we assume that years up to 1791 are Republican
    REM and years from 1792 onwards are Gregorian
    IF year% < 1792 THEN
      REM convert Republican date to number of days elapsed
      REM since 21 September 1792, then convert that number
      REM to the Gregorian date
      PROC_day_to_gre(FN_rep_to_day(day%, month%, year%), day%, month%, year%)
      PRINT; day%; " "; gregorian$(month% - 1); " " year%
    ELSE
      REM convert Gregorian date to Republican, via
      REM number of days elapsed since 21 September 1792
      PROC_day_to_rep(FN_gre_to_day(day%, month%, year%), day%, month%, year%)
      IF month% = 13 THEN
        PRINT sansculottides$(day% - 1); " "; year%
      ELSE
        PRINT; day%; " "; republican$(month% - 1); " "; year%
      ENDIF
    ENDIF
  ENDIF
UNTIL src$ = ""
END
:
DEF PROC_split(s$, RETURN d%, RETURN m%, RETURN y%)
LOCAL month_and_year$, month$, months$(), i%
DIM months$(11)
IF LEFT$(s$, 4) = "Fete" THEN
  m% = 13
  FOR i% = 0 TO 5
    IF LEFT$(s$, LEN sansculottides$(i%)) = sansculottides$(i%) THEN
      d% = i% + 1
      y% = VAL(RIGHT$(s$, LEN s$ - LEN sansculottides$(i%) - 1))
    ENDIF
  NEXT
ELSE
  d% = VAL(LEFT$(s$, INSTR(s$, " ") - 1))
  month_and_year$ = MID$(s$, INSTR(s$, " ") + 1)
  month$ = LEFT$(month_and_year$, INSTR(month_and_year$, " ") - 1)
  y% = VAL(MID$(month_and_year$, INSTR(month_and_year$, " ") + 1))
  IF y% < 1792 THEN months$() = republican$() ELSE months$() = gregorian$()
  FOR i% = 0 TO 11
    IF months$(i%) = month$ THEN m% = i% + 1
  NEXT
ENDIF
ENDPROC
:
DEF FN_gre_to_day(d%, m%, y%)
REM modified & repurposed from code given at
REM https://www.staff.science.uu.nl/~gent0113/calendar/isocalendar_text5.htm
IF m% < 3 THEN
  y% -= 1
  m% += 12
ENDIF
= INT(365.25 * y%) - INT(y% / 100) + INT(y% / 400) + INT(30.6 * (m% + 1)) + d% - 654842
:
DEF FN_rep_to_day(d%, m%, y%)
REM assume that a year is a leap year iff the _following_ year is
REM divisible by 4, but not by 100 unless also by 400
REM
REM other methods for computing Republican leap years exist
IF m% = 13 THEN
  m% -= 1
  d% += 30
ENDIF
IF FN_rep_leap(y%) THEN d% -= 1
= 365 * y% + INT((y% + 1) / 4) - INT((y% + 1) / 100) + INT((y% + 1) / 400) + 30 * m% + d% - 395
:
DEF PROC_day_to_gre(day%, RETURN d%, RETURN m%, RETURN y%)
y% = INT(day% / 365.25)
d% = day% - INT(365.25 * y%) + 21
y% += 1792
d% += INT(y% / 100) - INT(y% / 400) - 13
m% = 8
WHILE d% > gregorian%(m%)
  d% -= gregorian%(m%)
  m% += 1
  IF m% = 12 THEN
    m% = 0
    y% += 1
    IF FN_gre_leap(y%) THEN gregorian%(1) = 29 ELSE gregorian%(1) = 28
  ENDIF
ENDWHILE
m% += 1
ENDPROC
:
DEF PROC_day_to_rep(day%, RETURN d%, RETURN m%, RETURN y%)
LOCAL sansculottides%
y% = INT(day% / 365.25)
IF FN_rep_leap(y%) THEN y% -= 1
d% = day% - INT(365.25 * y%) + INT((y% + 1) / 100) - INT((y% + 1) / 400)
y% += 1
m% = 1
IF FN_rep_leap(y%) THEN sansculottides% = 6 ELSE sansculottides% = 5
WHILE d% > 30
  d% -= 30
  m% += 1
  IF m% = 13 THEN
    IF d% > sansculottides% THEN
      d% -= sansculottides%
      m% = 1
      y% += 1
      IF FN_rep_leap(y%) THEN sansculottides% = 6 ELSE sansculottides% = 5
    ENDIF
  ENDIF
ENDWHILE
ENDPROC
:
DEF FN_rep_leap(year%)
REM see comment at the beginning of FN_rep_to_day
= ((year% + 1) MOD 4 = 0 AND ((year% + 1) MOD 100 <> 0 OR (year% + 1) MOD 400 = 0))
:
DEF FN_gre_leap(year%)
= (year% MOD 4 = 0 AND (year% MOD 100 <> 0 OR year% MOD 400 = 0))
```

<b>Output for the test dates:</b>

```txt
*** French  Republican ***
*** calendar converter ***
Enter a date to convert, in the format 'day month year'
e.g.: 1 Prairial 3,
      20 May 1795.
For Sansculottides, use 'day year'
e.g.: Fete de l'opinion 9.
Or just press 'RETURN' to exit the program.

> 1 Vendemiaire 1
22 September 1792
> 22 September 1792
1 Vendemiaire 1
> 1 Prairial 3
20 May 1795
> 20 May 1795
1 Prairial 3
> 27 Messidor 7
15 July 1799
> 15 July 1799
27 Messidor 7
> Fete de la Revolution 11
23 September 1803
> 23 September 1803
Fete de la Revolution 11
> 10 Nivose 14
31 December 1805
> 31 December 1805
10 Nivose 14
```

<b>Output for a few subsequent dates:</b>

```txt
> 18 March 1871
27 Ventose 79
> 25 August 1944
7 Fructidor 152
> 19 September 2016
Fete du travail 224
```



## FreeBASIC

Computes leap years using the "continuous" method: a year in the Republican calendar is a leap year if and only if the number of the <i>following</i> year is divisible by 4 but not by 100 unless also by 400. No attempt is made to deal with ill-formed or invalid input dates.

```freebasic
' version 18 Pluviose 227
' compile with: fbc -s console
' retained the original comments for then BBC BASIC entry

#Macro rep_leap (_year)
    ' see comment at the beginning of rep_to_day
    ((_year +1) Mod 4 = 0 And ((_year +1) Mod 100 <> 0 Or (_year +1) Mod 400 = 0))
#EndMacro

#Macro gre_leap (_year)
    (_year Mod 4 = 0 And (_year Mod 100 <> 0 Or _year Mod 400 = 0))
#EndMacro

Dim Shared As UInteger gregorian(11) => {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
Dim Shared As String gregorian_s(11), republican(11), sanscolottides(5)
' 7-bit ASCII encoding, so no accents on French words
Data "January", "February", "March", "April", "May", "June"
Data "July", "August", "September", "October", "November", "December"
Data "Vendemiaire", "Brumaire", "Frimaire","Nivose", "Pluviose", "Ventose"
Data "Germinal", "Floreal", "Prairial", "Messidor", "Thermidor", "Fructidor"
Data "Fete de la Vertu", "Fete du Genie", "Fete du Travail", "Fete de l'Opinion"
Data "Fete des Recompenses","Fete de la Revolution"
Restore
For i As UInteger = 0 To 11
    Read gregorian_s(i)
Next
For i As UInteger = 0 To 11
    Read republican(i)
Next
For i As UInteger = 0 To 5
    Read sanscolottides(i)
Next

Sub split(s As String, ByRef d As UInteger, ByRef m As UInteger, ByRef y As UInteger)

    Dim As String month_and_year, Month
    Dim As UInteger i

    s = LCase(Trim(s)) : d = 0 : m = 0 : y = 0
    If Left(s,4) = "fete" Then
        m = 13
        For i = 0 To 5
            If Left(s, Len(sanscolottides(i))) = LCase(sanscolottides(i)) Then
                d = i +1
                y = Val(Right(s, Len(s) - Len(sanscolottides(i)) -1))
            End If
        Next
    Else
        d = Val(Left(s, InStr(s, " ") -1))
        month_and_year = Mid(s, InStr(s, " ") +1)
        Month = Left(month_and_year, InStr(month_and_year, " ") -1)
        y = Val(Mid(month_and_year, InStr(month_and_year, " ") +1))
        If y < 1792 Then
            For i = 0 To 11
                If LCase(republican(i)) = Month Then m = i +1
            Next
        Else
            For i = 0 To 11
                If LCase(gregorian_s(i)) = Month Then m = i +1
            Next
        End If
    End If

End Sub

Sub day_to_gre(Day As UInteger, ByRef d As UInteger, ByRef m As UInteger, ByRef y As UInteger)

    y = Fix(Day / 365.25)
    d = Day - Fix(365.25 * y) + 21
    y += 1792
    d += y \ 100 - y \ 400 - 13
    m = 8

    While d > gregorian(m)
        d -= gregorian(m)
        m += 1
        If m = 12 Then
            m = 0
            y += 1
            If gre_leap(y) Then gregorian(1) = 29 Else gregorian(1) = 28
        End If
    Wend
    gregorian(1) = 28
    m += 1

End Sub

Function gre_to_day(d As UInteger, m As UInteger, y As UInteger) As UInteger

    ' modified & repurposed from code given at
    ' https://www.staff.science.uu.nl/~gent0113/calendar/isocalendar_text5.htm

    If m < 3 Then
        y -= 1
        m += 12
    End If
    Return Fix(365.25 * y) - y \ 100 + y \ 400 + Fix(30.6 * (m +1)) + d - 654842

End Function

Function rep_to_day(d As UInteger, m As UInteger, y As UInteger) As UInteger

    ' assume that a year is a leap year iff the _following_ year is
    ' divisible by 4, but not by 100 unless also by 400
    '
    ' other methods for computing republican leap years exist

    If m = 13 Then
        m -= 1
        d += 30
    End If
    If rep_leap(y) Then d -= 1

    Return 365 * y + (y +1) \ 4 - (y +1) \ 100 + (y +1) \ 400 + 30 * m + d - 395

End Function

Sub day_to_rep(Day As UInteger, ByRef d As UInteger, ByRef m As UInteger, ByRef y As UInteger)

    Dim As UInteger sansculottides = 5

    y = Fix(Day / 365.25)
    If rep_leap(y) Then y -= 1
    d = Day - Fix(365.25 * y) + (y +1) \ 100 - (y +1) \ 400
    y += 1
    m = 1
    If rep_leap(y) Then sansculottides = 6
    While d > 30
        d -= 30
        m += 1
        If m = 13 Then
            If d > sansculottides Then
                d -= sansculottides
                m = 1
                y += 1
                If rep_leap(y) Then sansculottides = 6 Else sansculottides = 5
            End If
        End If
    Wend

End Sub

' ------=< main >=------

Dim As UInteger Day, Month, Year
Dim As String src

Print "*** French  Republican ***"
Print "*** calendar converter ***"
Print "Enter a date to convert, in the format 'day month year'"
Print "e.g.: 1 Prairial 3,"
Print "      20 May 1795."
Print "For Sansculottides, use 'day year'"
Print "e.g.: Fete de l'opinion 9."
Print "Or just press 'RETURN' to exit the program."
Print

Do
    Line Input "> ", src
    If src <> "" Then
        split(src, Day, Month, Year)
        If Day = 0 Or Month = 0 Or Year <= 0 Then
            Print "Error in input"
            Continue Do
        End If
        ' for simplicity, we assume that years up to 1791 are republican
        ' and years from 1792 onwards are gregorian
        If Year < 1792 Then
            ' convert republican date to number of days elapsed
            ' since 21 september 1792, then convert that number
            ' to the gregorian date
            day_to_gre(rep_to_day(Day, Month, Year),Day, Month, Year)
            Print; Day; " "; gregorian_s(Month -1); " "; Year
        Else
            ' convert gregorian date to republican, via
            ' number of days elapsed since 21 september 1792
            day_to_rep(gre_to_day(Day, Month, Year), Day, Month, Year)
            If Month = 13 Then
                Print sanscolottides(Day -1); " "; Year
            Else
                Print ; Day; " "; republican(Month -1); " "; Year
            End If
        End If
    End If
Loop Until src = ""

End
```

```txt
> 1 Vendemiaire 1                   > 22 September 1792          > 1 Vendemiaire 1          > 23 September 1806          > 1 Vendemiaire 15
22 September 1792                   1 Vendemiaire 1              22 September 1792          1 Vendemiaire 15             23 September 1806
> 22 September 1792                 > 22 September 1793          > 1 Vendemiaire 2          > 24 September 1807          > 1 Vendemiaire 16
1 Vendemiaire 1                     1 Vendemiaire 2              22 September 1793          1 Vendemiaire 16             24 September 1807
> 1 Prairial 3                      > 22 September 1794          > 1 Vendemiaire 3          > 23 September 1808          > 1 Vendemiaire 17
20 May 1795                         1 Vendemiaire 3              22 September 1794          1 Vendemiaire 17             23 September 1808
> 20 May 1795                       > 23 September 1795          > 1 Vendemiaire 4          > 23 September 1809          > 1 Vendemiaire 18
1 Prairial 3                        1 Vendemiaire 4              23 September 1795          1 Vendemiaire 18             23 September 1809
> 27 Messidor 7                     > 22 September 1796          > 1 Vendemiaire 5          > 23 September 1810          > 1 Vendemiaire 19
15 July 1799                        1 Vendemiaire 5              22 September 1796          1 Vendemiaire 19             23 September 1810
> 15 July 1799                      > 22 September 1797          > 1 Vendemiaire 6          > 24 September 1811          > 1 Vendemiaire 20
27 Messidor 7                       1 Vendemiaire 6              22 September 1797          1 Vendemiaire 20             24 September 1811
> Fete de la Revolution 11          > 22 September 1798          > 1 Vendemiaire 7          > 23 September 2015          > 1 Vendemiaire 224
23 September 1803                   1 Vendemiaire 7              22 September 1798          1 Vendemiaire 224            23 September 2015
> 23 September 1803                 > 23 September 1799          > 1 Vendemiaire 8          > 22 September 2016          > 1 Vendemiaire 225
Fete de la Revolution 11            1 Vendemiaire 8              23 September 1799          1 Vendemiaire 225            22 September 2016
> 10 Nivose 14                      > 23 September 1800          > 1 Vendemiaire 9          > 22 September 2017          > 1 Vendemiaire 226
31 December 1805                    1 Vendemiaire 9              23 September 1800          1 Vendemiaire 226            22 September 2017
> 31 December 1805                  > 23 September 1801          > 1 Vendemiaire 10         > 22 September 2018          > 1 Vendemiaire 227
10 Nivose 14                        1 Vendemiaire 10             23 September 1801          1 Vendemiaire 227            22 September 2018
                                    > 23 September 1802          > 1 Vendemiaire 11         > 23 September 2019          > 1 Vendemiaire 228
                                    1 Vendemiaire 11             23 September 1802          1 Vendemiaire 228            23 September 2019
                                    > 24 September 1803          > 1 Vendemiaire 12         > 22 September 2020          > 1 Vendemiaire 229
                                    1 Vendemiaire 12             24 September 1803          1 Vendemiaire 229            22 September 2020
                                    > 23 September 1804          > 1 Vendemiaire 13
                                    1 Vendemiaire 13             23 September 1804
                                    > 23 September 1805          > 1 Vendemiaire 14
                                    1 Vendemiaire 14             23 September 1805
```



## Go

A rather literal port, just for reference.  Far from idiomatic Go.

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

var (
    gregorianStr = []string{"January", "February", "March",
        "April", "May", "June",
        "July", "August", "September",
        "October", "November", "December"}
    gregorian     = []int{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
    republicanStr = []string{"Vendemiaire", "Brumaire", "Frimaire",
        "Nivose", "Pluviose", "Ventose",
        "Germinal", "Floreal", "Prairial",
        "Messidor", "Thermidor", "Fructidor"}
    sansculottidesStr = []string{"Fete de la vertu", "Fete du genie",
        "Fete du travail", "Fete de l'opinion",
        "Fete des recompenses", "Fete de la Revolution"}
)

func main() {
    fmt.Println("*** French  Republican ***")
    fmt.Println("*** calendar converter ***")
    fmt.Println("Enter a date to convert, in the format 'day month year'")
    fmt.Println("e.g.: 1 Prairial 3,")
    fmt.Println("      20 May 1795.")
    fmt.Println("For Sansculottides, use 'day year'")
    fmt.Println("e.g.: Fete de l'opinion 9.")
    fmt.Println("Or just press 'RETURN' to exit the program.")
    fmt.Println()
    for sc := bufio.NewScanner(os.Stdin); ; {
        fmt.Print("> ")
        sc.Scan()
        src := sc.Text()
        if src == "" {
            return
        }
        day, month, year := split(src)
        if year < 1792 {
            day, month, year = dayToGre(repToDay(day, month, year))
            fmt.Println(day, gregorianStr[month-1], year)
        } else {
            day, month, year := dayToRep(greToDay(day, month, year))
            if month == 13 {
                fmt.Println(sansculottidesStr[day-1], year)
            } else {
                fmt.Println(day, republicanStr[month-1], year)
            }
        }
    }
}

func split(s string) (d, m, y int) {
    if strings.HasPrefix(s, "Fete") {
        m = 13
        for i, sc := range sansculottidesStr {
            if strings.HasPrefix(s, sc) {
                d = i + 1
                y, _ = strconv.Atoi(s[len(sc)+1:])
            }
        }
    } else {
        d, _ = strconv.Atoi(s[:strings.Index(s, " ")])
        my := s[strings.Index(s, " ")+1:]
        mStr := my[:strings.Index(my, " ")]
        y, _ = strconv.Atoi(my[strings.Index(my, " ")+1:])
        months := gregorianStr
        if y < 1792 {
            months = republicanStr
        }
        for i, mn := range months {
            if mn == mStr {
                m = i + 1
            }
        }
    }
    return
}

func greToDay(d, m, y int) int {
    if m < 3 {
        y--
        m += 12
    }
    return y*36525/100 - y/100 + y/400 + 306*(m+1)/10 + d - 654842
}

func repToDay(d, m, y int) int {
    if m == 13 {
        m--
        d += 30
    }
    if repLeap(y) {
        d--
    }
    return 365*y + (y+1)/4 - (y+1)/100 + (y+1)/400 + 30*m + d - 395
}

func dayToGre(day int) (d, m, y int) {
    y = day * 100 / 36525
    d = day - y*36525/100 + 21
    y += 1792
    d += y/100 - y/400 - 13
    m = 8
    for d > gregorian[m] {
        d -= gregorian[m]
        m++
        if m == 12 {
            m = 0
            y++
            if greLeap(y) {
                gregorian[1] = 29
            } else {
                gregorian[1] = 28
            }
        }
    }
    m++
    return
}

func dayToRep(day int) (d, m, y int) {
    y = (day-1) * 100 / 36525
    if repLeap(y) {
        y--
    }
    d = day - (y+1)*36525/100 + 365 + (y+1)/100 - (y+1)/400
    y++
    m = 1
    sansculottides := 5
    if repLeap(y) {
        sansculottides = 6
    }
    for d > 30 {
        d -= 30
        m += 1
        if m == 13 {
            if d > sansculottides {
                d -= sansculottides
                m = 1
                y++
                sansculottides = 5
                if repLeap(y) {
                    sansculottides = 6
                }
            }
        }
    }
    return
}

func repLeap(year int) bool {
    return (year+1)%4 == 0 && ((year+1)%100 != 0 || (year+1)%400 == 0)
}

func greLeap(year int) bool {
    return year%4 == 0 && (year%100 != 0 || year%400 == 0)
}
```

```txt

$ go run frc.go
*** French  Republican ***
*** calendar converter ***
Enter a date to convert, in the format 'day month year'
e.g.: 1 Prairial 3,
      20 May 1795.
For Sansculottides, use 'day year'
e.g.: Fete de l'opinion 9.
Or just press 'RETURN' to exit the program.

> 1 Vendemiaire 1
22 September 1792
> 22 September 1792
1 Vendemiaire 1
> 1 Prairial 3
20 May 1795
> 20 May 1795
1 Prairial 3
> 27 Messidor 7
15 July 1799
> 15 July 1799
27 Messidor 7
> Fete de la Revolution 11
23 September 1803
> 23 September 1803
Fete de la Revolution 11
> 10 Nivose 14
31 December 1805
> 31 December 1805
10 Nivose 14
> 
$ 

```

'''More idiomatic'''

A start anyway.  Computations extracted to a package, a type defined for French Republican Dates, time package from standard library used.  Ignores invalid input rather than panicking.

```go
package frc

import (
    "fmt"
    "strconv"
    "strings"
    "time"
)

type Date struct {
    Year  int
    Month int
    Day   int
}

func (d Date) String() string {
    if d.Month == 13 {
        return fmt.Sprintf("%s %d", sansculotidesStr[d.Day-1], d.Year)
    }
    return fmt.Sprintf("%d %s %d", d.Day, republicanStr[d.Month-1], d.Year)
}

func Parse(s string) (dt Date, ok bool) {
    f := strings.Fields(s)
    var err error
    switch {
    case len(f) == 3:
        if dt.Day, err = strconv.Atoi(f[0]); err != nil {
            return
        }
        i := 0
        for republicanStr[i] != f[1] {
            i++
            if i == len(republicanStr) {
                return
            }
        }
        dt.Month = i + 1
        if dt.Year, err = strconv.Atoi(f[2]); err != nil {
            return
        }
        ok = true
        return
    case len(f) > 3:
        for i, sc := range sansculotidesStr {
            if strings.HasPrefix(s, sc) {
                dt.Month = 13
                dt.Day = i + 1
                dt.Year, err = strconv.Atoi(s[len(sc)+1:])
                if err == nil {
                    ok = true
                }
                return
            }
        }
    }
    return
}

func (dt Date) ToGregorian() (y int, m time.Month, d int) {
    yr, mn, dy := dayToGre(repToDay(dt.Day, dt.Month, dt.Year))
    return yr, time.Month(mn), dy
}

func FromGregorian(y int, m time.Month, d int) Date {
    day, month, year := dayToRep(greToDay(d, int(m), y))
    return Date{Year: year, Month: month, Day: day}
}
var (
    gregorian     = []int{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
    republicanStr = []string{"Vendemiaire", "Brumaire", "Frimaire",
        "Nivose", "Pluviose", "Ventose",
        "Germinal", "Floreal", "Prairial",
        "Messidor", "Thermidor", "Fructidor"}
    sansculotidesStr = []string{"Fete de la vertu", "Fete du genie",
        "Fete du travail", "Fete de l'opinion",
        "Fete des recompenses", "Fete de la Revolution"}
)

func greToDay(d, m, y int) int {
    if m < 3 {
        y--
        m += 12
    }
    return y*36525/100 - y/100 + y/400 + 306*(m+1)/10 + d - 654842
}

func repToDay(d, m, y int) int {
    if m == 13 {
        m--
        d += 30
    }
    if repLeap(y) {
        d--
    }
    return 365*y + (y+1)/4 - (y+1)/100 + (y+1)/400 + 30*m + d - 395
}

func dayToGre(day int) (d, m, y int) {
    y = day * 100 / 36525
    d = day - y*36525/100 + 21
    y += 1792
    d += y/100 - y/400 - 13
    m = 8
    for d > gregorian[m] {
        d -= gregorian[m]
        m++
        if m == 12 {
            m = 0
            y++
            if greLeap(y) {
                gregorian[1] = 29
            } else {
                gregorian[1] = 28
            }
        }
    }
    m++
    return
}

func dayToRep(day int) (d, m, y int) {
    y = (day-1) * 100 / 36525
    if repLeap(y) {
        y--
    }
    d = day - (y+1)*36525/100 + 365 + (y+1)/100 - (y+1)/400
    y++
    m = 1
    sansculottides := 5
    if repLeap(y) {
        sansculottides = 6
    }
    for d > 30 {
        d -= 30
        m += 1
        if m == 13 {
            if d > sansculottides {
                d -= sansculottides
                m = 1
                y++
                sansculottides = 5
                if repLeap(y) {
                    sansculottides = 6
                }
            }
        }
    }
    return
}

func repLeap(year int) bool {
    return (year+1)%4 == 0 && ((year+1)%100 != 0 || (year+1)%400 == 0)
}

func greLeap(year int) bool {
    return year%4 == 0 && (year%100 != 0 || year%400 == 0)
}
```


```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "time"

    "frc"
)

func main() {
    fmt.Println("*** French  Republican ***")
    fmt.Println("*** calendar converter ***")
    fmt.Println("Enter a date to convert, in the format 'day month year'")
    fmt.Println("e.g.: 1 Prairial 3,")
    fmt.Println("      20 May 1795.")
    fmt.Println("For Sansculottides, use 'day year'")
    fmt.Println("e.g.: Fete de l'opinion 9.")
    fmt.Println("Or just press 'RETURN' to exit the program.")
    fmt.Println()
    for sc := bufio.NewScanner(os.Stdin); ; {
        fmt.Print("> ")
        sc.Scan()
        src := sc.Text()
        if src == "" {
            return
        }
        f, ok := frc.Parse(src)
        if ok {
            fmt.Println(f.ToGregorian())
            continue
        }
        t, err := time.Parse(`2 January 2006`, src)
        if err == nil {
            fmt.Println(frc.FromGregorian(t.Date()))
        }
    }
}
```



## Kotlin


```scala
// version 1.1.4-3

import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.time.temporal.ChronoUnit.DAYS

/* year = 1..  month = 1..13  day = 1..30 */ 
class FrenchRCDate(val year: Int, val month: Int, val day: Int) {

    init {
        require (year > 0 && month in 1..13)
        if (month < 13) require (day in 1..30)
        else {
            val leap = isLeapYear(year)
            require (day in (if (leap) 1..6 else 1..5))
        }
    }

    override fun toString() =
        if (month < 13) "$day ${months[month - 1]} $year"
        else "${intercal[day - 1]} $year"

    fun toLocalDate(): LocalDate {
        var sumDays = 0L
        for (i in 1 until year) sumDays += if (isLeapYear(i)) 366 else 365
        val dayInYear = (month - 1) * 30 + day - 1
        return introductionDate.plusDays(sumDays + dayInYear)
    }

    companion object {
        /* uses the 'continuous method' for years after 1805 */
        fun isLeapYear(y: Int): Boolean {
            val yy = y + 1
            return (yy % 4 == 0) && (yy % 100 != 0 || yy % 400 == 0)
        }

        fun parse(frcDate: String): FrenchRCDate {
            val splits = frcDate.trim().split(' ')
            if (splits.size == 3) {
                val month = months.indexOf(splits[1]) + 1
                require(month in 1..13)
                val year = splits[2].toIntOrNull() ?: 0
                require(year > 0)
                val monthLength = if (month < 13) 30 else if (isLeapYear(year)) 6 else 5
                val day = splits[0].toIntOrNull() ?: 0
                require(day in 1..monthLength)
                return FrenchRCDate(year, month, day)
            }
            else if (splits.size in 4..5) {
                val yearStr = splits[splits.lastIndex]
                val year = yearStr.toIntOrNull() ?: 0
                require(year > 0)
                val scDay = frcDate.trim().dropLast(yearStr.length + 1)
                val day = intercal.indexOf(scDay) + 1
                val maxDay = if (isLeapYear(year)) 6 else 5
                require (day in 1..maxDay)
                return FrenchRCDate(year, 13, day)
            }
            else throw IllegalArgumentException("Invalid French Republican date")
        }

        /* for convenience we treat 'Sansculottide' as an extra month with 5 or 6 days */
        val months = arrayOf(
            "Vendémiaire", "Brumaire", "Frimaire", "Nivôse", "Pluviôse", "Ventôse", "Germinal",
            "Floréal", "Prairial", "Messidor", "Thermidor", "Fructidor", "Sansculottide"
        )

        val intercal = arrayOf(
            "Fête de la vertu", "Fête du génie", "Fête du travail",
            "Fête de l'opinion", "Fête des récompenses", "Fête de la Révolution"
        )

        val introductionDate = LocalDate.of(1792, 9, 22)
    }
}

fun LocalDate.toFrenchRCDate(): FrenchRCDate {
    val daysDiff  = DAYS.between(FrenchRCDate.introductionDate, this).toInt() + 1
    if (daysDiff <= 0) throw IllegalArgumentException("Date can't be before 22 September 1792")
    var year = 1
    var startDay = 1
    while (true) {
        val endDay = startDay + if (FrenchRCDate.isLeapYear(year)) 365 else 364
        if (daysDiff in startDay..endDay) break
        year++
        startDay = endDay + 1
    }
    val remDays = daysDiff - startDay
    val month  = remDays / 30
    val day = remDays - month * 30
    return FrenchRCDate(year, month + 1, day + 1)
}

fun main(args: Array<String>) {
    val formatter = DateTimeFormatter.ofPattern("d MMMM yyyy")
    val dates = arrayOf("22 September 1792", "20 May 1795", "15 July 1799", "23 September 1803",
                        "31 December 1805", "18 March 1871", "25 August 1944", "19 September 2016",
                        "22 September 2017", "28 September 2017")
    val frcDates = Array<String>(dates.size) { "" }
    for ((i, date) in dates.withIndex()) {
        val thisDate = LocalDate.parse(date, formatter)
        val frcd = thisDate.toFrenchRCDate()
        frcDates[i] = frcd.toString()
        println("${date.padEnd(25)} => $frcd")
    }

    // now process the other way around
    println()
    for (frcDate in frcDates) {
        val thisDate = FrenchRCDate.parse(frcDate)
        val lds = formatter.format(thisDate.toLocalDate())
        println("${frcDate.padEnd(25)} => $lds")
    }
}
```


```txt

22 September 1792         => 1 Vendémiaire 1
20 May 1795               => 1 Prairial 3
15 July 1799              => 27 Messidor 7
23 September 1803         => Fête de la Révolution 11
31 December 1805          => 10 Nivôse 14
18 March 1871             => 27 Ventôse 79
25 August 1944            => 7 Fructidor 152
19 September 2016         => Fête du travail 224
22 September 2017         => 1 Vendémiaire 226
28 September 2017         => 7 Vendémiaire 226

1 Vendémiaire 1           => 22 September 1792
1 Prairial 3              => 20 May 1795
27 Messidor 7             => 15 July 1799
Fête de la Révolution 11  => 23 September 1803
10 Nivôse 14              => 31 December 1805
27 Ventôse 79             => 18 March 1871
7 Fructidor 152           => 25 August 1944
Fête du travail 224       => 19 September 2016
1 Vendémiaire 226         => 22 September 2017
7 Vendémiaire 226         => 28 September 2017

```



## Perl


```perl
use feature 'state';
use DateTime;
my @month_names = qw{
    Vendémiaire Brumaire Frimaire  Nivôse   Pluviôse  Ventôse
    Germinal    Floréal  Prairial  Messidor Thermidor Fructidor
};
my @intercalary = (
    'Fête de la vertu',  'Fête du génie',        'Fête du travail',
    "Fête de l'opinion", 'Fête des récompenses', 'Fête de la Révolution',
);
my %month_nums  = map { $month_names[$_] => $_+1 } 0 .. $#month_names;
my %i_cal_nums  = map { $intercalary[$_] => $_+1 } 0 .. $#intercalary;
my $i_cal_month = 13;
my $epoch       = DateTime->new( year => 1792, month => 9, day => 22 );

sub is_republican_leap_year {
    my $y = $_[0] + 1;
    return !!( ($y % 4)==0 and (($y % 100)!=0 or ($y % 400)==0) );
}

sub Republican_to_Gregorian {
    my ($rep_date) = @_;
    state $months   = join '|', map { quotemeta } @month_names;
    state $intercal = join '|', map { quotemeta } @intercalary;
    state $re = qr{
      \A
        \s* (?:
                (?<ic> $intercal)
              | (?<day> \d+) \s+ (?<month> $months)
            )
        \s+ (?<year> \d+)
        \s*
      \z
    }msx;

    $rep_date =~ /$re/
        or die "Republican date not recognized: '$rep_date'";

    my $day1   = $+{ic}    ? $i_cal_nums{$+{ic}}    : $+{day};
    my $month1 = $+{month} ? $month_nums{$+{month}} : $i_cal_month;
    my $year1  = $+{year};

    my $days_since_epoch = ($year1-1) * 365 + ($month1-1) * 30 + ($day1-1);

    my $leap_days = grep { is_republican_leap_year($_) } 1 .. $year1-1;
    return $epoch->clone->add( days => ($days_since_epoch + $leap_days) );
}

sub Gregorian_to_Republican {
    my ($greg_date) = @_;

    my $days_since_epoch = $epoch->delta_days($greg_date)->in_units('days');
    die if $days_since_epoch < 0;
    my ( $year, $days ) = ( 1, $days_since_epoch );
    while (1) {
        my $year_length = 365 + ( is_republican_leap_year($year) ? 1 : 0 );
        last if $days < $year_length;
        $days -= $year_length;
        $year += 1;
    }
    my $day0   = $days % 30;
    my $month0 = ($days - $day0) / 30;

    my ( $day1, $month1 ) = ( $day0 + 1, $month0 + 1 );

    return $month1 == $i_cal_month
        ?       "$intercalary[$day0  ] $year"
        : "$day1 $month_names[$month0] $year";
}

while (<DATA>) {
    s{\s*\#.+\n?\z}{};
    /^(\d{4})-(\d{2})-(\d{2})\s+(\S.+?\S)\s*$/ or die;

    my $g = DateTime->new( year => $1, month => $2, day => $3 );
    my $r = $4;

    die if Republican_to_Gregorian($r) != $g
        or Gregorian_to_Republican($g) ne $r;

    die if Gregorian_to_Republican(Republican_to_Gregorian($r)) ne $r
        or Republican_to_Gregorian(Gregorian_to_Republican($g)) != $g;
}
say 'All tests successful.';

__DATA__
1792-09-22  1 Vendémiaire 1
1795-05-20  1 Prairial 3
1799-07-15  27 Messidor 7
1803-09-23  Fête de la Révolution 11
1805-12-31  10 Nivôse 14
1871-03-18  27 Ventôse 79
1944-08-25  7 Fructidor 152
2016-09-19  Fête du travail 224
1871-05-06  16 Floréal 79   # Paris Commune begins
1871-05-23  3 Prairial 79   # Paris Commune ends
1799-11-09  18 Brumaire 8   # Revolution ends by Napoléon coup
1804-12-02  11 Frimaire 13  # Republic   ends by Napoléon coronation
1794-10-30  9 Brumaire 3    # École Normale Supérieure established
1794-07-27  9 Thermidor 2   # Robespierre falls
1799-05-27  8 Prairial 7    # Fromental Halévy born
1792-09-22  1 Vendémiaire 1
1793-09-22  1 Vendémiaire 2
1794-09-22  1 Vendémiaire 3
1795-09-23  1 Vendémiaire 4
1796-09-22  1 Vendémiaire 5
1797-09-22  1 Vendémiaire 6
1798-09-22  1 Vendémiaire 7
1799-09-23  1 Vendémiaire 8
1800-09-23  1 Vendémiaire 9
1801-09-23  1 Vendémiaire 10
1802-09-23  1 Vendémiaire 11
1803-09-24  1 Vendémiaire 12
1804-09-23  1 Vendémiaire 13
1805-09-23  1 Vendémiaire 14
1806-09-23  1 Vendémiaire 15
1807-09-24  1 Vendémiaire 16
1808-09-23  1 Vendémiaire 17
1809-09-23  1 Vendémiaire 18
1810-09-23  1 Vendémiaire 19
1811-09-24  1 Vendémiaire 20
2015-09-23  1 Vendémiaire 224
2016-09-22  1 Vendémiaire 225
2017-09-22  1 Vendémiaire 226

```

```txt
All tests successful.
```



## Perl 6


```perl6
use v6;
constant @month_names = <
    Vendémiaire Brumaire Frimaire  Nivôse   Pluviôse  Ventôse
    Germinal    Floréal  Prairial  Messidor Thermidor Fructidor
>;
constant @intercalary =
    'Fête de la vertu',  'Fête du génie',        'Fête du travail',
    "Fête de l'opinion", 'Fête des récompenses', 'Fête de la Révolution',
;
constant %month_nums  = %( @month_names Z=> 1..12 );
constant %i_cal_nums  = %( @intercalary Z=> 1.. 6 );
constant $i_cal_month = 13;
constant $epoch       = Date.new: '1792-09-22';

sub is_republican_leap_year ( Int:D $year --> Bool ) {
    my $y := $year + 1;
    return ?( $y %% 4 and ($y !%% 100 or $y %% 400) );
}

sub Republican_to_Gregorian ( Str:D $rep_date --> Date ) {
    grammar Republican_date_text {
        token day   { \d+ }
        token year  { \d+ }
        token ic    { @intercalary }
        token month { @month_names }
        rule TOP    { ^ [ <ic> | <day> <month> ] <year> $ }
    }
    
    Republican_date_text.parse($rep_date)
        orelse die "Republican date not recognized: '$rep_date'";

    my $day1   := $/<ic>    ?? %i_cal_nums{~$/<ic>}    !! +$/<day>;
    my $month1 := $/<month> ?? %month_nums{~$/<month>} !! $i_cal_month;

    my @ymd0 := ($/<year>, $month1, $day1) »-» 1;
    my $days_since_epoch := [+] @ymd0 Z* (365, 30, 1);

    my $leap_days := +grep &is_republican_leap_year, 1 ..^ $/<year>;
    return $epoch + $days_since_epoch + $leap_days;
}

sub Gregorian_to_Republican ( Date:D $greg_date --> Str ) {
    my $days_since_epoch := $greg_date - $epoch;
    die if $days_since_epoch < 0;

    my ( $year, $days ) = 1, $days_since_epoch;
    loop {
        my $year_length = 365 + (1 if $year.&is_republican_leap_year);
        last if $days < $year_length;
        $days -= $year_length;
        $year += 1;
    }

    my ( $day0, $month0 ) = $days.polymod( 30 );
    my ( $day1, $month1 ) = ($day0, $month0) X+ 1;

    return $month1 == $i_cal_month
        ??       "@intercalary[$day0  ] $year"
        !! "$day1 @month_names[$month0] $year";
}

my @test_data =
    ( '1792-09-22', '1 Vendémiaire 1' ),
    ( '1795-05-20', '1 Prairial 3' ),
    ( '1799-07-15', '27 Messidor 7' ),
    ( '1803-09-23', 'Fête de la Révolution 11' ),
    ( '1805-12-31', '10 Nivôse 14' ),

    ( '1871-03-18', '27 Ventôse 79' ),
    ( '1944-08-25', '7 Fructidor 152' ),
    ( '2016-09-19', 'Fête du travail 224' ),

    ( '1871-05-06', '16 Floréal 79'  ), # Paris Commune begins
    ( '1871-05-23', '3 Prairial 79'  ), # Paris Commune ends
    ( '1799-11-09', '18 Brumaire 8'  ), # Revolution ends by Napoléon coup
    ( '1804-12-02', '11 Frimaire 13' ), # Republic   ends by Napoléon coronation
    ( '1794-10-30', '9 Brumaire 3'   ), # École Normale Supérieure established
    ( '1794-07-27', '9 Thermidor 2'  ), # Robespierre falls
    ( '1799-05-27', '8 Prairial 7'   ), # Fromental Halévy born

    ( '1792-09-22', '1 Vendémiaire 1'   ),
    ( '1793-09-22', '1 Vendémiaire 2'   ),
    ( '1794-09-22', '1 Vendémiaire 3'   ),
    ( '1795-09-23', '1 Vendémiaire 4'   ),
    ( '1796-09-22', '1 Vendémiaire 5'   ),
    ( '1797-09-22', '1 Vendémiaire 6'   ),
    ( '1798-09-22', '1 Vendémiaire 7'   ),
    ( '1799-09-23', '1 Vendémiaire 8'   ),
    ( '1800-09-23', '1 Vendémiaire 9'   ),
    ( '1801-09-23', '1 Vendémiaire 10'  ),
    ( '1802-09-23', '1 Vendémiaire 11'  ),
    ( '1803-09-24', '1 Vendémiaire 12'  ),
    ( '1804-09-23', '1 Vendémiaire 13'  ),
    ( '1805-09-23', '1 Vendémiaire 14'  ),
    ( '1806-09-23', '1 Vendémiaire 15'  ),
    ( '1807-09-24', '1 Vendémiaire 16'  ),
    ( '1808-09-23', '1 Vendémiaire 17'  ),
    ( '1809-09-23', '1 Vendémiaire 18'  ),
    ( '1810-09-23', '1 Vendémiaire 19'  ),
    ( '1811-09-24', '1 Vendémiaire 20'  ),
    ( '2015-09-23', '1 Vendémiaire 224' ),
    ( '2016-09-22', '1 Vendémiaire 225' ),
    ( '2017-09-22', '1 Vendémiaire 226' ),
;

for @test_data -> ( $g_text, $r ) {
    my $g = Date.new: $g_text;

    die if Republican_to_Gregorian($r) != $g
        or Gregorian_to_Republican($g) ne $r;

    die if Gregorian_to_Republican(Republican_to_Gregorian($r)) ne $r
        or Republican_to_Gregorian(Gregorian_to_Republican($g)) != $g;
}
say 'All tests successful.';
```

```txt
All tests successful.
```



## Phix


```Phix
constant gregorians = {"January", "February", "March", "April", "May", "June", "July", 
                       "August", "September", "October", "November", "December"},
         gregorian = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
         republicans = {"Vendémiaire", "Brumaire", "Frimaire", "Nivôse", "Pluviôse", 
                        "Ventôse", "Germinal", "Floréal", "Prairial", "Messidor", 
                        "Thermidor", "Fructidor"},
         sansculottides = {"Fête de la vertu", "Fête du génie", "Fête du travail",
                           "Fête de l'opinion", "Fête des récompenses",
                           "Fête de la Révolution"}
 
function rep_leap(integer year)
    return mod(year+1,4)==0 and (mod(year+1,100)!=0 or mod(year+1,400)==0)
end function

function rep_to_day(sequence dmy)
    integer {d, m, y} = dmy
    if m == 13 then
        m -= 1
        d += 30
    end if
    if rep_leap(y) then
        d -= 1
    end if
    integer res = 365*(y-1) + floor((y+1)/4) - floor((y+1)/100) + floor((y+1)/400) + 30*(m-1) + d
    return res
end function

function gre_leap(integer year)
    return mod(year,4)==0 and (mod(year,100)!=0 or mod(year,400)==0)
end function

function gre_to_day(sequence dmy)
    integer {d, m, y} = dmy
    if m < 3 then
        y -= 1
        m += 12
    end if
    integer res =  floor(y*365.25) - floor(y/100) + floor(y/400)
                 + floor(30.6*(m+1)) + d - 654842
    return res
end function

function day_to_rep(integer day)
    integer y = floor((day-1)/365.25)
    if not rep_leap(y) then
        y += 1
    end if
    integer d = day - floor(y*365.25) + 365 + floor(y/100) - floor(y/400),
            m = 1,
            sansculottide = 5+rep_leap(y)
    while d>30 do
        d -= 30
        m += 1
        if m == 13 then
            if d > sansculottide then
                d -= sansculottide
                m = 1
                y += 1
                sansculottide = 5 + rep_leap(y)
            end if
        end if
    end while
    return {d,m,y}
end function

function day_to_gre(integer day)
    integer y = floor(day/365.25),
            d = day - floor(y*365.25) + 21,
            m = 9
    y += 1792
    d += floor(y/100) - floor(y/400) - 13
    sequence gregoriam = gregorian  -- (modifiable copy)
    while d>gregoriam[m] do
        d -= gregoriam[m]
        m += 1
        if m == 13 then
            m = 1
            y += 1
            gregoriam[2] = 28 + gre_leap(y)
        end if
    end while
    return {d,m,y}
end function

function greg_to_frep(string greg)
    {{integer day, string months, integer year}} = scanf(greg,"%d %s %d")
    integer month = find(months,gregorians)
    {day,month,year} = day_to_rep(gre_to_day({day,month,year}))
    string frep = iff(month=13?sprintf("%s %d",{sansculottides[day], year})
                              :sprintf("%d %s %d",{day, republicans[month], year}))
    return frep
end function    

function frep_to_greg(string frep)
    integer day, month, year
    if frep[1]<='9' then
        {{day, string months, year}} = scanf(frep,"%d %s %d")
        month = find(months,republicans)
    else
        {{string days, year}} = scanf(frep,"%s %d")
        day = find(days,sansculottides)
        month = 13
    end if
    {day,month,year} = day_to_gre(rep_to_day({day, month, year}))
    string greg = sprintf("%02d %s %d",{day, gregorians[month], year})
    return greg
end function

constant test_data = {
                      { "22 September 1792", "1 Vendémiaire 1" },
                      { "20 May 1795", "1 Prairial 3" },
                      { "15 July 1799", "27 Messidor 7" },
                      { "23 September 1803", "Fête de la Révolution 11" },
                      { "31 December 1805", "10 Nivôse 14" },
 
                      { "18 March 1871", "27 Ventôse 79" },
                      { "25 August 1944", "7 Fructidor 152" },
                      { "19 September 2016", "Fête du travail 224" },

                      { "06 May 1871", "16 Floréal 79" },       -- Paris Commune begins
                      { "23 May 1871", "3 Prairial 79" },       -- Paris Commune ends
                      { "09 November 1799", "18 Brumaire 8" },  -- Revolution ends by Napoléon coup
                      { "02 December 1804", "11 Frimaire 13" }, -- Republic ends by Napoléon coronation
                      { "30 October 1794", "9 Brumaire 3" },    -- École Normale Supérieure established
                      { "27 July 1794", "9 Thermidor 2" },      -- Robespierre falls
                      { "27 May 1799", "8 Prairial 7" },        -- Fromental Halévy born
 
                      { "22 September 1792", "1 Vendémiaire 1" },
                      { "22 September 1793", "1 Vendémiaire 2" },
                      { "22 September 1794", "1 Vendémiaire 3" },
                      { "23 September 1795", "1 Vendémiaire 4" },
                      { "22 September 1796", "1 Vendémiaire 5" },
                      { "22 September 1797", "1 Vendémiaire 6" },
                      { "22 September 1798", "1 Vendémiaire 7" },
                      { "23 September 1799", "1 Vendémiaire 8" },
                      { "23 September 1800", "1 Vendémiaire 9" },
                      { "23 September 1801", "1 Vendémiaire 10" },
                      { "23 September 1802", "1 Vendémiaire 11" },
                      { "24 September 1803", "1 Vendémiaire 12" },
                      { "23 September 1804", "1 Vendémiaire 13" },
                      { "23 September 1805", "1 Vendémiaire 14" },
                      { "23 September 1806", "1 Vendémiaire 15" },
                      { "24 September 1807", "1 Vendémiaire 16" },
                      { "23 September 1808", "1 Vendémiaire 17" },
                      { "23 September 1809", "1 Vendémiaire 18" },
                      { "23 September 1810", "1 Vendémiaire 19" },
                      { "24 September 1811", "1 Vendémiaire 20" },
                      { "23 September 2015", "1 Vendémiaire 224" },
                      { "21 September 2016", "Fête des récompenses 224" },
                      { "22 September 2016", "1 Vendémiaire 225" },
                      { "23 September 2016", "2 Vendémiaire 225" },
                      { "22 September 2017", "1 Vendémiaire 226" },
                      { "28 September 2017", "7 Vendémiaire 226" } }

for i=1 to length(test_data) do
    string {greg, frep} = test_data[i],
           frep2 = greg_to_frep(greg),
           greg2 = frep_to_greg(frep),
           ok = iff(frep=frep2 and greg=greg2?"ok":"**** ERROR ****")
    if platform()=WINDOWS then
        -- the windows console does not handle 
        -- non-basic-latin-ascii characters well...
        frep = substitute_all(frep,"éêéô","eeeo")
    end if
    printf(1,"%18s <==> %-25s %s\n",{greg,frep,ok})
end for

--sanity test:
for i=1 to 150000 do -- (years 1792..~2203)
    if rep_to_day(day_to_rep(i))!=i then ?9/0 end if
    if gre_to_day(day_to_gre(i))!=i then ?9/0 end if
end for
```

```txt

 22 September 1792 <==> 1 Vendemiaire 1           ok
       20 May 1795 <==> 1 Prairial 3              ok
      15 July 1799 <==> 27 Messidor 7             ok
 23 September 1803 <==> Fete de la Revolution 11  ok
  31 December 1805 <==> 10 Nivose 14              ok
     18 March 1871 <==> 27 Ventose 79             ok
    25 August 1944 <==> 7 Fructidor 152           ok
 19 September 2016 <==> Fete du travail 224       ok
       06 May 1871 <==> 16 Floreal 79             ok
       23 May 1871 <==> 3 Prairial 79             ok
  09 November 1799 <==> 18 Brumaire 8             ok
  02 December 1804 <==> 11 Frimaire 13            ok
   30 October 1794 <==> 9 Brumaire 3              ok
      27 July 1794 <==> 9 Thermidor 2             ok
       27 May 1799 <==> 8 Prairial 7              ok
 22 September 1792 <==> 1 Vendemiaire 1           ok
 22 September 1793 <==> 1 Vendemiaire 2           ok
 22 September 1794 <==> 1 Vendemiaire 3           ok
 23 September 1795 <==> 1 Vendemiaire 4           ok
 22 September 1796 <==> 1 Vendemiaire 5           ok
 22 September 1797 <==> 1 Vendemiaire 6           ok
 22 September 1798 <==> 1 Vendemiaire 7           ok
 23 September 1799 <==> 1 Vendemiaire 8           ok
 23 September 1800 <==> 1 Vendemiaire 9           ok
 23 September 1801 <==> 1 Vendemiaire 10          ok
 23 September 1802 <==> 1 Vendemiaire 11          ok
 24 September 1803 <==> 1 Vendemiaire 12          ok
 23 September 1804 <==> 1 Vendemiaire 13          ok
 23 September 1805 <==> 1 Vendemiaire 14          ok
 23 September 1806 <==> 1 Vendemiaire 15          ok
 24 September 1807 <==> 1 Vendemiaire 16          ok
 23 September 1808 <==> 1 Vendemiaire 17          ok
 23 September 1809 <==> 1 Vendemiaire 18          ok
 23 September 1810 <==> 1 Vendemiaire 19          ok
 24 September 1811 <==> 1 Vendemiaire 20          ok
 23 September 2015 <==> 1 Vendemiaire 224         ok
 21 September 2016 <==> Fete des recompenses 224  ok
 22 September 2016 <==> 1 Vendemiaire 225         ok
 23 September 2016 <==> 2 Vendemiaire 225         ok
 22 September 2017 <==> 1 Vendemiaire 226         ok
 28 September 2017 <==> 7 Vendemiaire 226         ok

```



## Sidef

```ruby
require('DateTime')

var month_names = %w(
  Vendémiaire Brumaire Frimaire  Nivôse   Pluviôse  Ventôse
  Germinal    Floréal  Prairial  Messidor Thermidor Fructidor
)

var intercalary = [
                   'Fête de la vertu',
                   'Fête du génie',
                   'Fête du travail',
                   "Fête de l'opinion",
                   'Fête des récompenses',
                   'Fête de la Révolution',
                  ]

var i_cal_month = 13
var epoch = %O<DateTime>.new(year => 1792, month => 9, day => 22)

var month_nums = Hash(month_names.kv.map {|p| [p[1], p[0]+1] }.flat...)
var i_cal_nums = Hash(intercalary.kv.map {|p| [p[1], p[0]+1] }.flat...)

func is_republican_leap_year(Number year) -> Bool {
    var y = (year + 1)
    !!(4.divides(y) && (!100.divides(y) || 400.divides(y)))
}

func Republican_to_Gregorian(String rep_date) -> String {
    static months   = month_names.map { .escape }.join('|')
    static intercal = intercalary.map { .escape }.join('|')
    static re       = Regex("^
        \\s* (?:
                (?<ic> #{intercal})
              | (?<day> \\d+) \\s+ (?<month> #{months})
            )
        \\s+ (?<year> \\d+)
        \\s*
      \\z", 'x')

    var m = (rep_date =~ re)
    m || die "Republican date not recognized: '#{rep_date}'"

    var ncap = m.named_captures

    var day1   = Number(ncap{:ic}    ? i_cal_nums{ncap{:ic}}    : ncap{:day})
    var month1 = Number(ncap{:month} ? month_nums{ncap{:month}} : i_cal_month)
    var year1  = Number(ncap{:year})

    var days_since_epoch = (365*(year1 - 1) + 30*(month1 - 1) + (day1 - 1))

    var leap_days = (1 ..^ year1 -> grep { is_republican_leap_year(_) })
    epoch.clone.add(days => (days_since_epoch + leap_days)).strftime("%Y-%m-%d")
}

func Gregorian_to_Republican(String greg_date) -> String {
    var m = (greg_date =~ /^(\d{4})-(\d{2})-(\d{2})\z/)
    m || die "Gregorian date not recognized: '#{greg_date}'"

    var g = %O<DateTime>.new(year => m[0], month => m[1], day => m[2])
    var days_since_epoch = epoch.delta_days(g).in_units('days')
    days_since_epoch < 0 && die "unexpected error"
    var (year, days) = (1, days_since_epoch)

    loop {
        var year_length = (365 + (is_republican_leap_year(year) ? 1 : 0))
        days < year_length && break
        days -= year_length
        year += 1;
    }

    var day0   = (days % 30)
    var month0 = (days - day0)/30

    var (day1, month1) = (day0 + 1, month0 + 1)

    (month1 == i_cal_month
        ? "#{intercalary[day0]} #{year}"
        : "#{day1} #{month_names[month0]} #{year}")
}

for line in DATA {

    line.sub!(/\s*\#.+\R?\z/, '')

    var m = (line =~ /^(\d{4})-(\d{2})-(\d{2})\s+(\S.+?\S)\s*$/)
    m || die "error for: #{line.dump}"

    var g = "#{m[0]}-#{m[1]}-#{m[2]}"
    var r = m[3]

    var r2g = Republican_to_Gregorian(r)
    var g2r = Gregorian_to_Republican(g)

    #say "#{r} -> #{r2g}"
    #say "#{g} -> #{g2r}"

    if ((g2r != r) || (r2g != g)) {
        die "1-way error"
    }

    if ((Gregorian_to_Republican(r2g) != r) ||
        (Republican_to_Gregorian(g2r) != g)
    ) {
        die "2-way error"
    }
}

say 'All tests successful.'

__DATA__
1792-09-22  1 Vendémiaire 1
1795-05-20  1 Prairial 3
1799-07-15  27 Messidor 7
1803-09-23  Fête de la Révolution 11
1805-12-31  10 Nivôse 14
1871-03-18  27 Ventôse 79
1944-08-25  7 Fructidor 152
2016-09-19  Fête du travail 224
1871-05-06  16 Floréal 79   # Paris Commune begins
1871-05-23  3 Prairial 79   # Paris Commune ends
1799-11-09  18 Brumaire 8   # Revolution ends by Napoléon coup
1804-12-02  11 Frimaire 13  # Republic   ends by Napoléon coronation
1794-10-30  9 Brumaire 3    # École Normale Supérieure established
1794-07-27  9 Thermidor 2   # Robespierre falls
1799-05-27  8 Prairial 7    # Fromental Halévy born
1792-09-22  1 Vendémiaire 1
1793-09-22  1 Vendémiaire 2
1794-09-22  1 Vendémiaire 3
1795-09-23  1 Vendémiaire 4
1796-09-22  1 Vendémiaire 5
1797-09-22  1 Vendémiaire 6
1798-09-22  1 Vendémiaire 7
1799-09-23  1 Vendémiaire 8
1800-09-23  1 Vendémiaire 9
1801-09-23  1 Vendémiaire 10
1802-09-23  1 Vendémiaire 11
1803-09-24  1 Vendémiaire 12
1804-09-23  1 Vendémiaire 13
1805-09-23  1 Vendémiaire 14
1806-09-23  1 Vendémiaire 15
1807-09-24  1 Vendémiaire 16
1808-09-23  1 Vendémiaire 17
1809-09-23  1 Vendémiaire 18
1810-09-23  1 Vendémiaire 19
1811-09-24  1 Vendémiaire 20
2015-09-23  1 Vendémiaire 224
2016-09-22  1 Vendémiaire 225
2017-09-22  1 Vendémiaire 226
```

```txt

All tests successful.

```

