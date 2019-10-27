+++
title = "Dinesman's multiple-dwelling problem/AutoHotkey"
description = ""
date = 2014-03-17T22:12:15Z
aliases = []
[extra]
id = 17265
[taxonomies]
categories = []
tags = []
+++

{{collection|Dinesman's multiple-dwelling problem}}
{{collection|Zebra puzzle}}
[[Category:AutoHotkey]]
Combined solution for [[Dinesman's multiple-dwelling problem]] and [[Zebra puzzle]].

```autohotkey
#NoEnv
SetBatchLines, -1
#SingleInstance, Force
Backtrack:=0 ; Set to -1 to turn off backtracking.

; Solve this the same way a human would - logic grids.
; To animate solution, uncomment Display(size) lines.
; Parse problem statement intuitively.
demo1 = Four people: Abe Ben Charlie David. Abe not second or top. not adjacent Ben Charlie. David Abe adjacent. David adjacent Ben. Abe?
demo2 = Four: A B C D. A not adjacent D. not B adjacent higher C. C lower D. A ; assumed nothigher = lower
problem1 = Five adjacent floors of an apartment house contain these different tradesmen: Baker, Cooper, Fletcher, Miller, Smith. Baker does not live on the top floor. Cooper does not live on the bottom floor. Fletcher does not live on either the top or the bottom floor. Miller lives on a higher floor than does Cooper. Smith does not live on a floor adjacent to Fletcher's. Fletcher does not live on a floor adjacent to Cooper's. Where does Fletcher live?
problem2 = Seven adjacent floors of an apartment house contain these different people: Baker, Cooper, Fletcher, Miller, Guinan, and Smith. Guinan does not live on either the top or the third or the fourth floor. Baker does not live on the top floor. Cooper does not live on the bottom floor. Fletcher does not live on either the top or the bottom floor. Miller lives on a higher floor than does Cooper. Smith does not live on a floor adjacent to Fletcher's. Fletcher does not live on a floor adjacent to Cooper's. Where does Fletcher live? ; from the Python version
problem3 = Five adjacent houses contain different men of different nationalities and different tastes, one from each of these groups: Dane, Englishman, German, Norwegian, Swede, Blend, Blue_Master, Dunhill, Pall_Mall, Prince, blue, green, red, white, yellow, beer, coffee, milk, water, tea, birds, cats, dog, horse, zebra. The Englishman lives in the red house. The Swede has a dog. The Dane drinks tea. The green house is immediately to the left of the white house. They drink coffee in the green house. The man who smokes Pall_Mall has birds. In the yellow house they smoke Dunhill. In the middle house they drink milk. The Norwegian lives in the first house. The man who smokes Blend lives in the house next to the house with cats. In a house next to the house where they have a horse, they smoke Dunhill. The man who smokes Blue_Master drinks beer. The German smokes Prince. The Norwegian lives next to the blue house. They drink water in a house next to the house where they smoke Blend. Who owns the zebra?
problem4 = Ten adjacent houses contain different men of different nationalities and different tastes, one from each of these groups: Renault, Ferrari, Mercedes, Ford, Volvo, VW, BMW, Porsche, Rolls_Royce, Toyota, White, Red, Magenta, Yellow, Green, Brown, Gray, Pink, Blue, Black, Water, Ice_tea, Beer, Coffee, Tea, Wine, Espresso, Soda, Milk, Lemonade, Orchids, Crocuses, Roses, Cactuses, Tulips, Dahlias, Geraniums, Hyacinth, Daffodils, Lilies, Waffles, Potatoes, Cheese, Chocolate, Cookies, Ice_cream, Eggs, Spaghetti, Pancakes, Steaks, Norseman, Swiss, German, Spaniard, Brit, Italian, Irishman, Greek, Dane, Swede, Dogs, Mice, Tortoises, Butterflies, Snakes, Birds, Cats, Fish, Horses, Turtles, Chesterfields, Pipe, Cubans, Kools, Pall_Mall, Blend, Dunhill, Bluemaster, Marlboro, Prince, Soccer, Badminton, Lacrosse, Tennis, Rugby, Basketball, Football, Baseball, Volleyball, Ice_hockey, Redwoods, Eucalyptus, Oaks, Pines, Birch, Nuts, Firs, Maples, Palms, Willows. The Ford driver lives directly next to the person drinking ice_tea. The person with the cats drinks tea. The fish live directly to the right of the person drinking milk. The Ford driver lives directly to the right of tennis player. The Swede lives directly to the right of the birch trees. The ice_hockey player does not like redwoods. The Renault driver drinks soda. The green house does not have a Ferrari parked in front of it. The second house reeks of Pall_Mall. The blue house reeks of Prince. The house with nuts does not reek of Bluemaster. The driver of the Ford is not Greek. The driver of the Porsche likes steaks. You will be served spaghetti in the first house. Cookies and birch trees do not go together. The person driving the Volvo plays baseball. The tortoises live directly to the right of the horses. The Brit lives directly to the left of the potatoes-eating person. The firs grow directly next to the basketball player. The person eating cheese lives at a number higher than that of the red house. The person in house nine plays soccer. The eucalyptus trees grow directly to the left of the person drinking coffee. The Dane lives directly to the right of the badminton player. The tenth house is gray. The Norseman lives in house nine. The Brit eats pancakes. The Italian lives directly to the right of the Marlboro-smoker. The person with palms lives directly to the right of the person driving the Rolls_Royce. The person liking the geraniums has dogs. The volleyball player lives at a number higher that of the person eating eggs. The Mercedes is parked in front of house nine. The orchids grow at a number higher that of the blue house. The volleyball player lives directly to the left of the person smoking Kools. The first house is magenta. The person in house four smokes Chesterfields. The football player lives directly to the left of the person smoking Dunhill. The oaks grow directly to the left of the cactuses. The dogs live directly to the left of the baseball player. The person eating pancakes lives in the brown house. The Ferrari is parked in front of house four. The person driving the Porsche lives directly to the right of the white house. The turtles live in house seven. The hyacinth grow in front of house eight. The crocuses grow in front of house six. The VW is parked in front of house two. The palms grow directly to the right of the person drinking milk. The Italian grows lilies. The person driving the Renault lives in the red house. The person in house nine drinks tea. The willows grow directly next to the daffodils. The birds live in house five. The person with the turtles drinks wine. The person drinking beer lives at a number higher than that of the person drinking espresso. The orchids grow directly next to the horses. The nuts grow in front of house ten. The person in house one does not play basketball. The person smoking pipe lives directly to the right of the black house. The Irishman lives directly to the left of the Blend-smoking person. The dahlias grow directly to the left of the snakes. The maples grow in front of house seven. The Toyota driver lives directly to the left of the Ford driver. The mice live directly to the right of the person eating chocolate. The person in house one drinks water. The pines grow directly to the right of the person smoking Pall_Mall. The person smoking Dunhill lives directly next to the person drinking tea. The person playing rugby lives directly to the right of the yellow house. The willows grow in front of house four. The Swiss eats waffles. The baseball player lives directly next to the Lacrosse player. The German lives in house one. The tulips grow directly next to the red house. The daffodils grow directly to the right of the badminton player. The person drinking beer lives directly next to the person drinking milk. Who eats ice_cream?

Loop, Parse, problem1, . ; select name of problem here; do not edit below
{
	If (A_Index = 1)
	{
		; Get number of houses/floors.
		numerals:="One|Two|Three|Four|Five|Six|Seven|Eight|Nine|Ten|Eleven|Twelve|Thirteen|Fourteen|Fifteen|Sixteen|Seventeen|Eighteen|Nineteen|Twenty"
		ordinals:="First|Second|Third|Fourth|Fifth|Sixth|Seventh|Eighth|Ninth|Tenth|Eleventh|Twelfth|Thirteenth|Fourteenth|Fifteenth|Sixteenth|Seventeenth|Eighteenth|Nineteenth|Twentieth"
		Loop, Parse, numerals, |
			%A_LoopField%:=A_Index
		StringSplit, ordinal, ordinals, |
		n:=SubStr(A_LoopField, 1, RegExMatch(A_LoopField, "\W")-1)
		n:=%n%
		
		; Get list of all properties, and assign each to an index.
		Loop, % size:=n+RegExMatchAll(SubStr(A_LoopField, InStr(A_LoopField, ":")+1), "\b\w+\b", "property" )
		{
			If (A_Index <= n)
				i:=A_Index
				, name:=ordinal%i%
			Else
				i:=A_Index-n
				, name:=property%i%
			all.=name "|"
			%name%:=A_Index
		}
		all:=SubStr(all, 1, -1)
		Middle:=(n+1)//2, Last:=n, Bottom:=1, Top:=n ; permissible shortcut properties
		re_properties:="i)(\bBottom|Middle|Top|Last|" numerals "|" all "\b)"
		re_properties:=RegExReplace(re_properties, "\|", "\b|\b")
		re_keywords:="i)(\bhigher\b|\blower\b|\badjacent\b|\bnext\b|\babove\b|\bbelow\b|\bleft\b|\bright\b)"
		
		; Initialize values, with 1's on the diagonal.
		Loop, % size
		{
			j:=A_Index
			Loop, % size
			{
				i:=A_Index
				%i%_%j%:=(i = j) ? 1 : -1
			}
		}
	}
	Else
	{
		RegExMatchAll(A_LoopField, re_properties, "properties")
		RegExMatchAll(A_LoopField, re_keywords, "keywords")
		note:=RegExMatch(A_LoopField, "i)\bnot\b") ? "not" : ""
		If (properties0 = 1)
		{
			find:=properties1 ; at end of loop will contain name in last line
		}
		Else If (properties0 = 2)
		{
			properties1:=%properties1%
			properties2:=%properties2%
			If (keywords0 = 0)
			{
				%properties1%_%properties2%:=(note ? 0 : 1)
			}
			Else Loop, % keywords0
			{
				keyword:=keywords%A_Index%
				%note%%keyword% .= properties1 "," properties2 "`n"
			}
		}
		Else Loop, % properties0
		{
			If (A_Index = 1)
			{
				properties1:=%properties1%
			}
			Else
			{
				properties:=properties%A_Index%
				properties:=%properties%
				%properties1%_%properties%:=(note ? 0 : 1)
			}
		}
	}
}

; Create a grid of these attributes, and GUI to show it.
Gui +Resize +MinSize +OwnDialogs +LastFound
Gui, Font, s8, Courier New
Gui, Add, Text, w90 h90 x10 y10, Current`n%find%`nState
x:=80, y:=80
Loop, Parse, all, |
{
	x+=21+(mod(A_Index,n)=1 ? 5 : 0)
	y+=21+(mod(A_Index,n)=1 ? 5 : 0)
	Gui, Add, Tab2, w21 h100 x%x% y10 Left Buttons, %A_LoopField%
	Gui, Add, Edit, w90 h15 x10 y%y% Right Disabled, %A_LoopField%
}
y:=81, s:=0
Loop, % size
{
	j:=A_Index
	x:=83
	y+=21+(mod(j,n)=1 ? 5 : 0)
	Loop, % size
	{
		i:=A_Index
		x+=21+(mod(i,n)=1 ? 5 : 0)
		If (++s < 10000-3*size) ; workaround as AHK only supports 11000 controls per gui
			Gui, Add, Checkbox, x%x% y%y% w13 h13 v%i%_%j% Check3 CheckedGray ; w13 h13 tricks AHK to not display label
	}
}
Gui, Add, Button, xp-55 yp+20 w70, Close
Gui, Add, Button, xp-85 yp w70, Reset
Gui, Add, Button, xp-85 yp w70 Default, Continue
Gui, Add, Text, xm yp w400 r2 vNotify
Gui, Show, NA

; Internal cleanup.
lower.=nothigher ; only implemented for floor puzzles
higher.=notlower ; only implemented for floor puzzles
right.=above ; synonymns for house vs. floor puzzles
left.=below
adjacent.=next
notadjacent.=notnext
adjacent:=SubStr(adjacent, 1, -1)
notadjacent:=SubStr(notadjacent, 1, -1)
right:=SubStr(right, 1, -1)
Loop, Parse, right, `n
{
	StringSplit, a, A_LoopField, `,
	left.=a2 "," a1 "`n"
}
left:=SubStr(left, 1, -1)
higher:=SubStr(higher, 1, -1)
Loop, Parse, higher, `n
{
	StringSplit, a, A_LoopField, `,
	lower.=a2 "," a1 "`n"
}
lower:=SubStr(lower, 1, -1)

; Allow user to add constraint mid-process
Display(size) ; required for Gui Submit below
ButtonContinue:
GuiControl, , Notify, Solving...
Gui, Submit, NoHide
Backtrack:

; Five steps:
s:=true
While, s ; loop until solved
{
; 1. Check adjacent properties.
Loop, Parse, adjacent, `n
{
	StringSplit, a, A_LoopField, `,
	
	Loop, % n
	{
		k:=A_Index-1
		l:=A_Index+1
		If (%A_Index%_%a1% = 1)
			If (A_Index = 1) ; there is only one house adjacent the first house
			{
				m:=1+1
				If (%m%_%a2% != 0)
					%m%_%a2%:=1
				Else	Goto Error
			}
			Else If (A_Index = n) ; there is only one house adjacent the last house
			{
				m:=n-1
				If (%m%_%a2% != 0)
					%m%_%a2%:=1
				Else	Goto Error
			}
			Else If (%l%_%a2%=0) ; there are two adjacent house but one has already been eliminated
				If (%k%_%a2% != 0)
					%k%_%a2%:=1
				Else	Goto Error
			Else If (%k%_%a2%=0) ; there are two adjacent house but one has already been eliminated
				If (%l%_%a2% != 0)
					%l%_%a2%:=1
				Else	Goto Error
		Else If (%A_Index%_%a1% = 0)
			If (A_Index = 1+1) ; there is only one house adjacent the first house
				If (%1%_%a2% != 1)
					%1%_%a2%:=0
				Else	Goto Error
			Else If (A_Index = n-1) ; there is only one house adjacent the last house
				If (%n%_%a2% != 1)
					%n%_%a2%:=0
				Else	Goto Error
		If (%A_Index%_%a2% = 1) ; repeat with houses switched
			If (A_Index = 1)
			{
				m:=1+1
				If (%m%_%a1% != 0)
					%m%_%a1%:=1
				Else	Goto Error
			}
			Else If (A_Index = n)
			{
				m:=n-1
				If (%m%_%a1% != 0)
					%m%_%a1%:=1
				Else	Goto Error
			}
			Else If (%l%_%a1%=0)
				If (%k%_%a1% != 0)
					%k%_%a1%:=1
				Else	Goto Error
			Else If (%k%_%a1%=0)
				If (%l%_%a1% != 0)
					%l%_%a1%:=1
				Else	Goto Error
		Else If (%A_Index%_%a2% = 0)
			If (A_Index = 1+1)
				If (%1%_%a1% != 1)
					%1%_%a1%:=0
				Else	Goto Error
			Else If (A_Index = n-1)
				If (%n%_%a1% != 1)
					%n%_%a1%:=0
				Else	Goto Error
	}
}
Loop, Parse, notadjacent, `n
{
	StringSplit, a, A_LoopField, `,
	
	Loop, % n
	{
		k:=A_Index-1
		l:=A_Index+1
		If (%A_Index%_%a1% = 1) ; if you know where this house is, you know the other is not adjacent it
			If (A_Index = 1)
				If (%l%_%a2% != 1)
					%l%_%a2%:=0
				Else	Goto Error
			Else If (A_Index = n)
				If (%k%_%a2% != 1)
					%k%_%a2%:=0
				Else	Goto Error
			Else
				If (%k%_%a2% != 1 and %l%_%a2% != 1)
					%k%_%a2%:=0
					, %l%_%a2%:=0
				Else	Goto Error
		If (%A_Index%_%a2% = 1) ; repeat with houses switched
			If (A_Index = 1)
				If (%l%_%a1% != 1)
					%l%_%a1%:=0
				Else	Goto Error
			Else If (A_Index = n)
				If (%k%_%a1% != 1)
					%k%_%a1%:=0
				Else	Goto Error
			Else
				If (%k%_%a1% != 1 and %l%_%a1% != 1)
					%k%_%a1%:=0
					, %l%_%a1%:=0
				Else	Goto Error
	}
	
	m:=0
	Loop, % n
	{
		j:=A_Index
		If (%j%_%a2% = -1) ; find available a2
		{
			Loop, % n
			{
				i:=A_Index
				If (abs(j-i) > 1 and %i%_%a1% = -1) ; find available a1
					k:=i, l:=j, m++
			}
		}
	}
	If (m = 1)
		If (%k%_%a1% != 0 and %l%_%a2% != 0)
			%k%_%a1%:=1
			, %l%_%a2%:=1
		Else	Goto Error
}
Loop, Parse, left, `n
{
	StringSplit, a, A_LoopField, `,
	
	If (1_%a2% != 1)
		1_%a2%:=0 ; because otw a1 cannot be below it
	Else	Goto Error
	If (%n%_%a1% != 1)
		%n%_%a1%:=0 ; because otw a2 cannot be above it
	Else	Goto Error
	Loop, % n
	{
		k:=A_Index-1
		l:=A_Index+1
		If (%A_Index%_%a1% = 1) ; a2 is same value as a1 one house to left
			If (A_Index < n and %l%_%a2% != 0)
				%l%_%a2%:=1
			Else	Goto Error
		Else If (%A_Index%_%a1% = 0)
			If (A_Index < n)
				If (%l%_%a2% != 1)
					%l%_%a2%:=0
				Else	Goto Error
		If (%A_Index%_%a2% = 1) ; and vice versa
			If (A_Index > 1 and %k%_%a1% != 0)
				%k%_%a1%:=1
			Else	Goto Error
		Else If (%A_Index%_%a2% = 0)
			If (A_Index > 1)
				If (%k%_%a1% != 1)
					%k%_%a1%:=0
				Else	Goto Error
	}
}
Loop, Parse, lower, `n
{
	StringSplit, a, A_LoopField, `,
	
	Loop, % n ; find highest available a2
	{
		If (%A_Index%_%a2% != 0)
		{
			l:=A_Index
		}
	}
	Loop, % n-(l-1)
	{
		m:=(l-1)+A_Index
		If (%m%_%a1% != 1)
			%m%_%a1%:=0 ; because otw a2 cannot be above it
		Else	Goto Error
	}
	
	Loop, % n ; find lowest available a1
	{
		If (%A_Index%_%a1% != 0)
		{
			k:=A_Index
			Break
		}
	}
	Loop, % k
	{
		If (%A_Index%_%a2% != 1)
			%A_Index%_%a2%:=0 ; because otw a1 cannot be below it
		Else	Goto Error
	}
}
;Display(size)

; 2. Mirror both directions.
Loop, % size
{
	j:=A_Index
	Loop, % size
	{
		i:=A_Index
		If (%i%_%j% != -1)
			If (%j%_%i% = -1)
				%j%_%i%:=%i%_%j%
			Else If (%j%_%i% != %i%_%j%)
				Goto Error
	}
}
;Display(size)

; 3. Add 0s around 1s.
Loop, % size
{
	j:=A_Index
	Loop, % size
	{
		i:=A_Index
		If (%i%_%j% = 1)
		{
			k:=i-(i//n=i/n ? n : 0)
			Loop, % n-1
			{
				
				k+=1-(k//n=k/n && k>i ? n : 0)
				If (%k%_%j% = -1)
					%k%_%j%:=0
				Else If (%k%_%j% != 0)
					Goto Error
			}
		}
	}
}
;Display(size)

; 4. Flip 0s around 1s.
Loop, % size
{
	j:=A_Index
	Loop, % size
	{
		i:=A_Index
		If (%i%_%j% = 1)
		{
			Loop, % size
			{
				k:=A_Index
				If (%k%_%j% = 0)
					If (%i%_%k% = -1)
						%i%_%k%:=0
					Else If (%i%_%k% != 0)
						Goto Error
			}
		}
	}
}
;Display(size)

; 5. Add 1s when only one spot remaining.
Loop, % size
{
	j:=A_Index
	Loop, % size
	{
		i:=A_Index
		If (mod(i,n) = 1)
			k:=0, l:=i-1
		If (%i%_%j% = 0)
			k++
		Else
			l:=i
		If (k = n-1)
		{
			If (mod(l,n) = 0 and i-l = n-1)
				l+=n
			If (%l%_%j% != 0)
				%l%_%j%:=1
			Else	Goto Error
			k:=0
		}
	}
	l:=0
}
;Display(size)

; Catch infinite loop.
olds:=s
s:=Check(size)
If (s=olds)
{
	If (Backtrack = -1)
	{
		Display(size)
		Msgbox, System error! Infinite loop.
		ExitApp
	}
	Backtrack++
	Loop, % size
	{
		j:=A_Index
		Loop, % size
		{
			i:=A_Index
			%Backtrack%_%i%_%j%:=%i%_%j%
			If (%i%_%j% = -1)
				k:=i, l:=j
		}
	}
	%k%_%l%:=1
}
}

; Show results.
Display(size)
m=
StringSplit, a, all, |
Loop, % n
{
	j:=A_Index
	m.=a%j% " house:`t"
	Loop, % size
	{
		i:=A_Index
		If (%i%_%j% and i!=j)
			m.=a%i% "`t"
		If (%i%_%j% and a%i% = find)
			found := a%j%
			, m:="The " found " house contains the " find ".`n`n" m
	}
	m.="`n"
}
If (Backtrack <= 0)
	m.="`nThis solution is unique."
Else
	m.="`nBacktracking was required; this solution may not be unique."
GuiControl, , Notify, Solved. The %found% house contains the %find%.
MsgBox % m

Return ;--------------------------------------------------------------

; Function to display grid on screen.
Display(size) {
s:=0
Loop, % size
{
	j:=A_Index
	Loop, % size
	{
		i:=A_Index
		If (++s < 10000-3*size) ; workaround as AHK only supports 11000 controls per gui
			GuiControl, , %i%_%j%, % %i%_%j%
	}
}
Gui, Show, NA
Sleep 0.5*1000
Return
}

; Function to check completion.
Check(size) {
s:=0
Loop, % size
{
	j:=A_Index
	Loop, % size
	{
		i:=A_Index
		If (%i%_%j% = -1)
			s++
	}
}
Return s
}

; modified from http://www.autohotkey.com/board/topic/58737-stdlib-regexmatchall/
RegExMatchAll( Haystack, NeedleRegEx , QuotedOutputVar, StartingPosition = 1 )
{
	Local RegExPosition := StartingPosition-1, MatchCount := 0, temp
	While RegExPosition := RegExMatch( Haystack, NeedleRegEx, %QuotedOutputVar%, RegExPosition+1 )
	{
		%QuotedOutputVar%0 := ++MatchCount
		%QuotedOutputVar%%MatchCount% := %QuotedOutputVar%
		If MatchCount = 1
			temp := %QuotedOutputVar%
	}
	%QuotedOutputVar%0 := MatchCount
	%QuotedOutputVar%1 := temp
	Return MatchCount
}

; Consistent error check.
Error:
	If (Backtrack = -1)
	{
		Display(size)
		Msgbox, System error! Problem unsolvable.
		ExitApp
	}
	Loop, % size
	{
		j:=A_Index
		Loop, % size
		{
			i:=A_Index
			%i%_%j%:=%Backtrack%_%i%_%j%
			If (%i%_%j% = -1)
				k:=i, l:=j
		}
	}
	%k%_%l%:=0
	Backtrack--
	Goto Backtrack

ButtonReset:
;~F5::
	Reload

; Pause key.
;~^c::
~Pause::
	Pause
Return

; Break key.
ButtonClose:
GuiClose:
GuiEscape:
~^Pause::
	ExitApp
```


Demo1:

```txt
The Third house contains the Abe.

First house:	Ben	
Second house:	David	
Third house:	Abe	
Fourth house:	Charlie	

Backtracking was required; this solution may not be unique.
```

Demo2:

```txt
The Second house contains the A.

First house:	B	
Second house:	A	
Third house:	C	
Fourth house:	D	

This solution is unique.
```

Problem1 (Dinesman's multiple-dwelling problem):

```txt
The Fourth house contains the Fletcher.

First house:	Smith	
Second house:	Cooper	
Third house:	Baker	
Fourth house:	Fletcher	
Fifth house:	Miller	

This solution is unique.
```

Problem2:

```txt
The Fourth house contains the Fletcher.

First house:	Baker	
Second house:	Cooper	
Third house:	Miller	
Fourth house:	Fletcher	
Fifth house:	Guinan	
Sixth house:	and	
Seventh house:	Smith	

Backtracking was required; this solution may not be unique.
```

Problem3 (Zebra puzzle):

```txt
The Fourth house contains the zebra.

First house:	Norwegian	Dunhill	yellow	water	cats	
Second house:	Dane	Blend	blue	tea	horse	
Third house:	Englishman	Pall_Mall	red	milk	birds	
Fourth house:	German	Prince	green	coffee	zebra	
Fifth house:	Swede	Blue_Master	white	beer	dog	

This solution is unique.
```

Problem4:

```txt
The Fifth house contains the ice_cream.

First house:	BMW	Magenta	Water	Roses	Spaghetti	German	Butterflies	Bluemaster	Ice_hockey	Eucalyptus	
Second house:	VW	Brown	Coffee	Geraniums	Pancakes	Brit	Dogs	Pall_Mall	Lacrosse	Oaks	
Third house:	Volvo	Blue	Espresso	Cactuses	Potatoes	Greek	Horses	Prince	Baseball	Pines	
Fourth house:	Ferrari	Pink	Beer	Orchids	Eggs	Irishman	Tortoises	Chesterfields	Badminton	Willows	
Fifth house:	Rolls_Royce	Green	Milk	Daffodils	Ice_cream	Dane	Birds	Blend	Volleyball	Birch	
Sixth house:	Toyota	Black	Ice_tea	Crocuses	Cookies	Swede	Fish	Kools	Tennis	Palms	
Seventh house:	Ford	Yellow	Wine	Tulips	Chocolate	Spaniard	Turtles	Pipe	Football	Maples	
Eighth house:	Renault	Red	Soda	Hyacinth	Waffles	Swiss	Mice	Dunhill	Rugby	Redwoods	
Ninth house:	Mercedes	White	Tea	Dahlias	Cheese	Norseman	Cats	Marlboro	Soccer	Firs	
Tenth house:	Porsche	Gray	Lemonade	Lilies	Steaks	Italian	Snakes	Cubans	Basketball	Nuts	

This solution is unique.
```

