+++
title = "Knapsack Problem/Visual Basic"
description = ""
date = 2010-01-08T17:22:13Z
aliases = []
[extra]
id = 4915
[taxonomies]
categories = []
tags = []
+++

{{collection|Knapsack Problem}}
{{works with|Visual Basic|6.0}}

```vb
Option Explicit

Type TreasureType
    Name As String
    Units As String
    Value As Currency
    weight As Single
    Volume As Single
End Type

Type SolutionType
    Desc As String
    Value As Currency
End Type

Type KnapsackType
    Contents() As Integer
    CapacityWeight As Single
    CapacityVolume As Single
End Type

Dim Treasures() As TreasureType

Public Sub Main()
    
    SetupTreasureShangriLa
    Debug.Print CalcKnapsack(25, 0.25)
    
End Sub

Public Sub SetupTreasureShangriLa()

    ReDim Treasures(3) As TreasureType
    With Treasures(1)
        .Name = "panacea"
        .Units = "vials"
        .Value = 3000
        .weight = 0.3
        .Volume = 0.025
    End With
    With Treasures(2)
        .Name = "ichor"
        .Units = "ampules"
        .Value = 1800
        .weight = 0.2
        .Volume = 0.015
    End With
    With Treasures(3)
        .Name = "gold"
        .Units = "bars"
        .Value = 2500
        .weight = 2
        .Volume = 0.002
    End With
    
End Sub

Public Function CalcKnapsack(ByVal sCapacityWeight As Single, ByVal sCapacityVolume As Single) As String
Dim Knapsack As KnapsackType
Dim Solution As SolutionType

    Knapsack.CapacityVolume = sCapacityVolume
    Knapsack.CapacityWeight = sCapacityWeight
    ReDim Knapsack.Contents(UBound(Treasures)) As Integer
    Call Stuff(Knapsack, Solution, 1)
    Debug.Print "Maximum value: " & Solution.Value
    Debug.Print "Ideal Packing(s): " & vbCrLf & Solution.Desc
    
End Function

Private Sub Stuff(ByRef Knapsack As KnapsackType, ByRef Solution As SolutionType, ByVal nDepth As Integer)
Dim nI As Integer
Dim curVal As Currency
Dim sWeightRemaining As Single
Dim sVolumeRemaining As Single
Dim nJ As Integer

    sWeightRemaining = CalcWeightRemaining(Knapsack)
    sVolumeRemaining = CalcvolumeRemaining(Knapsack)

    With Treasures(nDepth)
        If nDepth = UBound(Treasures) Then
            Knapsack.Contents(nDepth) = Min(Fix(sWeightRemaining / .weight), Fix(sVolumeRemaining / .Volume))
            curVal = CalcValue(Knapsack)
            If curVal > Solution.Value Then
                Solution.Value = curVal
                Solution.Desc = BuildDesc(Knapsack)
            ElseIf curVal = Solution.Value Then
                Solution.Desc = Solution.Desc & vbCrLf & "or" & vbCrLf & vbCrLf & BuildDesc(Knapsack)
            End If
        Else
            For nI = 0 To Min(Fix(sWeightRemaining / .weight), Fix(sVolumeRemaining / .Volume))
                Knapsack.Contents(nDepth) = nI
                For nJ = nDepth + 1 To UBound(Treasures)
                    Knapsack.Contents(nJ) = 0
                Next nJ
                Call Stuff(Knapsack, Solution, nDepth + 1)
            Next nI
        End If
    End With

End Sub

Private Function CalcValue(ByRef Knapsack As KnapsackType) As Currency
Dim curTmp As Currency
Dim nI As Integer

    For nI = 1 To UBound(Treasures)
        curTmp = curTmp + (Treasures(nI).Value * Knapsack.Contents(nI))
    Next nI
    
    CalcValue = curTmp
    
End Function

Private Function Min(ByVal vA As Variant, ByVal vB As Variant) As Variant

    If vA < vB Then
        Min = vA
    Else
        Min = vB
    End If

End Function

Private Function CalcWeightRemaining(ByRef Knapsack As KnapsackType) As Single
Dim sTmp As Single
Dim nI As Integer

    For nI = 1 To UBound(Treasures)
        sTmp = sTmp + (Treasures(nI).weight * Knapsack.Contents(nI))
    Next nI
    
    CalcWeightRemaining = Knapsack.CapacityWeight - sTmp
    
End Function

Private Function CalcvolumeRemaining(ByRef Knapsack As KnapsackType) As Single
Dim sTmp As Single
Dim nI As Integer

    For nI = 1 To UBound(Treasures)
        sTmp = sTmp + (Treasures(nI).Volume * Knapsack.Contents(nI))
    Next nI
    
    CalcvolumeRemaining = Knapsack.CapacityVolume - sTmp
    
End Function

Private Function BuildDesc(ByRef Knapsack As KnapsackType) As String
Dim cTmp As String
Dim nI As Integer

    For nI = 1 To UBound(Treasures)
        cTmp = cTmp & Knapsack.Contents(nI) & " " & Treasures(nI).Units & " of " & Treasures(nI).Name & vbCrLf
    Next nI
    BuildDesc = cTmp

End Function
```


Output:

```txt
Maximum value: 54500
Ideal Packing(s): 
0 vials of panacea
15 ampules of ichor
11 bars of gold

or

3 vials of panacea
10 ampules of ichor
11 bars of gold

or

6 vials of panacea
5 ampules of ichor
11 bars of gold

or

9 vials of panacea
0 ampules of ichor
11 bars of gold
```

