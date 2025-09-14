+++
title = "Go Fish"
description = ""
date = 2019-07-21T21:33:04Z
aliases = []
[extra]
id = 4996
[taxonomies]
categories = ["task", "Games"]
tags = []
languages = [
  "aime",
  "autohotkey",
  "c",
  "cpp",
  "d",
  "erlang",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "julia",
  "kotlin",
  "locomotive_basic",
  "ocaml",
  "perl_6",
  "phix",
  "picolisp",
  "purebasic",
  "python",
  "racket",
  "ruby",
  "tcl",
]
+++

## Task

Write a program to let the user play [[wp:Go Fish|Go Fish]] against a computer opponent. Use the following rules:

* Each player is dealt nine cards to start with.
* On their turn, a player asks their opponent for a given rank (such as threes or kings). A player must already have at least one card of a given rank to ask for more.
** If the opponent has any cards of the named rank, they must hand over all such cards, and the requester can ask again.
** If the opponent has no cards of the named rank, the requester draws a card and ends their turn.
* A ''book'' is a collection of every card of a given rank. Whenever a player completes a book, they may remove it from their hand.
* If at any time a player's hand is empty, they may immediately draw a new card, so long as any new cards remain in the deck.
* The game ends when every book is complete. The player with the most books wins.

The game's AI need not be terribly smart, but it should use at least some strategy. That is, it shouldn't choose legal moves entirely at random.

You may want to use code from [[Playing Cards]].


## Aime

See [[Go Fish/Aime]]


## AutoHotkey

See [[Go Fish/AutoHotkey]]


## C

See [[Go Fish/C]]


## C++

See [[Go Fish/C++]]


## D

See [[Go Fish/D]]


## Erlang

See [[Go Fish/Erlang]]


## FreeBASIC


```freebasic

' Go Fish ~ ¡Pesca!

Const cartas = "A234567890JQK"

Declare Sub Reparto_Cartas
Declare Sub Pescar_Carta_Jug
Declare Sub Pescar_Carta_CPU
Declare Sub Comprobar_Libro_Jug
Declare Sub Comprobar_Libro_CPU
Declare Sub Comprobar_Fin_Partida
Declare Sub Intro

Dim Shared As Integer play(13), compu(13), deck(13), guess(13), poss(13), asked(13)
Dim Shared As String nombre, Snombre, CartaPedida
Dim Shared puntos(2) As Byte = {0,0}
Dim Shared As Integer remca = 4*Len(cartas)
Dim Shared As Integer i, k, j, cn
For i = 1 To 13
    deck(i) = 4
Next i
For i = 1 To 9
    Reparto_Cartas
    deck(k) -= 1
    compu(k) += 1
    Reparto_Cartas
    deck(k) -= 1
    play(k) += 1
Next i
Dim As Integer v, po

Sub Reparto_Cartas
    remca -= 1
    Dim As Integer sc = remca * Rnd + 1
    For k = 1 To 13
        sc -= deck(k)
        If sc <= 0 Then Return
    Next k
End Sub

Sub Pescar_Carta_Jug
    Reparto_Cartas
    Print " " &Mid(cartas,k,1) &"."
    deck(k) -= 1
    play(k) += 1
End Sub

Sub Pescar_Carta_CPU
    Reparto_Cartas
    Print "a carta."
    deck(k) -= 1
    compu(k) += 1
End Sub

Sub Comprobar_Libro_Jug
    For i = 1 To 13
        If play(i) <> 4 Then
        Else
            Color 11: Print Snombre &" completa el libro de " &Mid(cartas,i,1) &"'s.": Color 7
            play(i) = 0
            puntos(0) += 1
        End If
    Next i
End Sub

Sub Comprobar_Libro_CPU
    For i = 1 To 13
        If compu(i) <> 4 Then
        Else
            Color 11: Print Snombre &" completa el libro de " &Mid(cartas,i,1) &"'s.": Color 7
            compu(i) = 0
            puntos(1) += 1
        End If
    Next i
End Sub

Sub Comprobar_Fin_Partida
    Dim As Integer np = 0, nc = 0
    For i = 1 To 13
        np += play(i)
        nc += compu(i)
    Next i
    If remca = 0 Or np = 0 Or nc = 0 Then         
        Color 15: Print
        Print "*** ­FIN de la partida! ***"
        Print
        If puntos(0) < puntos(1) Then 
            Print "La CPU ha ganado."
        Elseif puntos(0) > puntos(1) Then 
            Print nombre &" ha ganado." 
        Else 
            Print "­Es un empate!"
        End If
        Sleep: End
    End If
End Sub

Sub Intro
    Color 15 
    Print "                __ _     _      "
    Print "  __ _  ___    / _(_)___| |__   "
    Print " /  ` |/ _ \  | |_| / __| '_ \  "
    Print "| (_) | (_) | |  _| \__ \ | | | "
    Print " \__, |\___/  |_| |_|___/_| |_| "
    Print " |___/                          "
    Print "                                "
    Color 14: Locate 10, 2: Input "Como te llamas: ", nombre
End Sub

'--- Programa Principal ---
Cls
Randomize Timer
Intro
Do
    Dim As boolean MuestraMano = false
    While MuestraMano = false
        Color 15: Print Chr(10) &"Puntos >> " &nombre &": "; puntos(0); "  CPU: "; puntos(1)
        Color 13: Print Chr(10) &space(10) &remca &" cartas restantes"
        Color 14: Print Chr(10) &"Tu mano: ";
        For i = 1 To 13
            If Not play(i) Then 
                For j = 1 To play(i)
                    Print Mid(cartas,i,1); " ";
                Next j
            End If
        Next i
        Print
        
        Dim As boolean PideCarta = false
        While PideCarta = false
            Comprobar_Fin_Partida
            Snombre = nombre
            Color 7: Print
            Input "¨Que carta pides... "; CartaPedida
            Print
            If CartaPedida <> "" Then cn = Instr(cartas, Ucase(CartaPedida)): PideCarta = true
            If cn = 0 Then 
                Print "Lo siento, no es una opción valida.": PideCarta = false
                Elseif play(cn) = 0 Then Color 12: Print "­No tienes esa carta!": Color 7: PideCarta = false
            End If
        Wend
        
        guess(cn) = 1
        If compu(cn) = 0 Then 
            Print Snombre &", ";
            Color 15: Print "­ve a pescar!"
            Color 7: Print Snombre &" pesca un";: Pescar_Carta_Jug
            Comprobar_Libro_Jug
            MuestraMano = true
        Else
            v = compu(cn)
            compu(cn) = 0
            play(cn) += v
            Print Snombre &" consigue " &v &" carta(s) mas."
            Comprobar_Libro_Jug
            MuestraMano = false
        End If
    Wend

    Snombre = "CPU"
    For i = 1 To 13
        asked(i) = 0
    Next i
    Dim As boolean Turno_CPU_2 = false
    While Turno_CPU_2 = false
        Comprobar_Fin_Partida
        po = 0
        For i = 1 To 13
            If (compu(i) > 0) And (guess(i) > 0) Then poss(i) = 1: po += 1
        Next i
        If po = 0 Then 
            Do
                k = (Rnd*12)+1
            Loop While compu(k) = 0 Or asked(k)
        Else
            Do
                k = (Rnd*12)+1
            Loop While poss(k) = 0
            guess(k) = 0
            asked(k) = 1
        End If

        Print: Print Snombre &" quiere tus " &Mid(cartas,k,1) &"'s."
        asked(k) = 1
        If play(k) = 0 Then 
            Print Snombre &", ";
            Color 15: Print "­ve a pescar!"
            Color 7:Print Snombre &" pesca un";: Pescar_Carta_CPU
            Comprobar_Libro_CPU
            Turno_CPU_2 = true
        Else
            v = play(k)
            play(k) = 0
            compu(k) += v
            Print Snombre &" consigue " &v &" carta(s) mas."
            Comprobar_Libro_CPU
            Turno_CPU_2 = false
        End If
    Wend 
Loop
End

```



## Go

See [[Go Fish/Go]]


## Haskell

See [[Go Fish/Haskell]]

=={{header|Icon}} and {{header|Unicon}}==
See [[Go Fish/Unicon]]


## J

See [[Go Fish/J]]


## Java

See [[Go Fish/Java]]




## Julia

see [[Go Fish/Julia]]


## Kotlin

See [[Go Fish/Kotlin]]


## Locomotive Basic

See [[Go Fish/Locomotive Basic]]

=={{header|Mathematica}} / {{header|Wolfram Language}}==
See [[Go Fish/Mathematica]]


## OCaml

See [[Go Fish/OCaml]]


## Perl 6

See [[Go Fish/Perl_6]]


## Phix

See [[Go Fish/Phix]]


## PicoLisp

See [[Go Fish/PicoLisp]]


## PureBasic

See [[Go Fish/PureBasic]]


## Python

See [[Go Fish/Python]]


## Racket

See [https://github.com/plt/racket/blob/master/pkgs/games/gofish/gofish.rkt gofish.rkt].
Documentation: [http://docs.racket-lang.org/games/gofish.html?q=fish Go Fish].
[[File:go-fish-racket.png|thumb|right]]


## Ruby

See [[Go Fish/Ruby]]


## Tcl

See [[Go Fish/Tcl]]

