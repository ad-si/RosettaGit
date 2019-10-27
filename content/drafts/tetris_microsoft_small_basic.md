+++
title = "Tetris/Microsoft Small Basic"
description = ""
date = 2016-04-16T00:33:55Z
aliases = []
[extra]
id = 20662
[taxonomies]
categories = []
tags = []
+++

{{collection|Tetris}}
==Code==
This game was coded by Small Basic coder Kenneth Kasajian.

```smallbasic
GraphicsWindow.KeyDown = HandleKey
GraphicsWindow.BackgroundColor = GraphicsWindow.GetColorFromRGB( 253, 252, 251 )

While "True"
  BOXES = 4      ' number of boxes per piece
  BWIDTH = 25    ' box width in pixels
  XOFFSET = 40   ' Screen X offset in pixels of where the board starts
  YOFFSET = 40   ' Screen Y offset in pixels of where the board starts
  CWIDTH = 10    ' Canvas Width, in number of boxes
  CHEIGHT = 20   ' Canvas Height, in number of boxes.
  STARTDELAY = 800
  ENDDELAY = 175
  PREVIEW_xpos = 13
  PREVIEW_ypos = 2

  GraphicsWindow.Clear()
  GraphicsWindow.Title = "Small Basic Tetris"
  GraphicsWindow.Height = 580
  GraphicsWindow.Width = 700
  GraphicsWindow.Show()

  SetupTemplates()
  SetupCanvas()
  MainLoop()

  GraphicsWindow.ShowMessage( "Game Over", "Small Basic Tetris" )
EndWhile

Sub MainLoop
  template = Text.Append("template", Math.GetRandomNumber(7))

  CreatePiece() ' in: template ret: h
  nextPiece = h

  end = 0
  sessionDelay = STARTDELAY
  While end = 0
    If sessionDelay > ENDDELAY Then
      sessionDelay = sessionDelay - 1
    EndIf

    delay = sessionDelay
    thisPiece = nextPiece
    template = Text.Append("template", Math.GetRandomNumber(7))

    CreatePiece() ' in: template ret: h
    nextPiece = h
    DrawPreviewPiece()

    h = thisPiece

    ypos = 0
    done = 0
    xpos = 3 ' always drop from column 3
    CheckStop() ' in: ypos, xpos, h ret: done
    If done = 1 Then
      ypos = ypos - 1
      MovePiece()  'in: ypos, xpos, h
      end = 1
    EndIf

    yposdelta = 0
    While done = 0 Or yposdelta > 0
      MovePiece()  'in: ypos, xpos, h

      ' Delay, but break if the delay get set to 0 if the piece gets dropped
      delayIndex = delay
      While delayIndex > 0 And delay > 0
        Program.Delay(10)
        delayIndex = delayIndex - 10
      EndWhile

      If yposdelta > 0 Then
        yposdelta = yposdelta - 1  ' used to create freespin, when the piece is rotated
      Else
        ypos = ypos + 1            ' otherwise, move the piece down.
      EndIf

      ' Check if the piece should stop.
      CheckStop() ' in: ypos, xpos, h ret: done 
    EndWhile
  EndWhile
EndSub

Sub HandleKey
  ' Stop game
  If GraphicsWindow.LastKey = "Escape" Then
    Program.End()
  EndIf

  ' Move piece left
  If GraphicsWindow.LastKey = "Left" Then
    moveDirection = -1
    ValidateMove()  ' in: ypos, xpos, h, moveDirection ret: invalidMove = 1 or -1 or 2 if move is invalid, otherwise 0
    If invalidMove = 0 Then
      xpos = xpos + moveDirection
    EndIf
    MovePiece()  'in: ypos, xpos, h
  EndIf

  ' Move piece right
  If GraphicsWindow.LastKey = "Right" Then
    moveDirection = 1
    ValidateMove()  ' in: ypos, xpos, h, moveDirection ret: invalidMove = 1 or -1 or 2 if move is invalid, otherwise 0
    If invalidMove = 0 Then
      xpos = xpos + moveDirection
    EndIf
    MovePiece()  'in: ypos, xpos, h
  EndIf

  ' Move piece down
  If GraphicsWindow.LastKey = "Down" or GraphicsWindow.LastKey = "Space" Then
    delay = 0
  EndIf

  ' Rotate piece
  If GraphicsWindow.LastKey = "Up" Then
    basetemplate = Array.GetValue(h, -1)  ' Array.GetValue(h, -1) = the template name
    template = "temptemplate"
    rotation = "CW"
    CopyPiece()  'in basetemplate, template, rotation

    Array.SetValue(h, -1, template) ' Array.GetValue(h, -1) = the template name
    moveDirection = 0
    ValidateMove()  ' in: ypos, xpos, h, moveDirection ret: invalidMove = 1 or -1 or 2 if move is invalid, otherwise 0

    ' See if it can be moved so that it will rotate.
    xposbk = xpos
    yposdelta = 0
    While yposdelta = 0 And Math.Abs(xposbk - xpos) < 3 ' move up to 3 times only
      ' if the rotation move worked, copy the temp to "rotatedtemplate" and use that from now on
      If invalidMove = 0 Then
        basetemplate = template
        template = "rotatedtemplate"
        Array.SetValue(h, -1, template) ' Array.GetValue(h, -1) = the template name
        rotation = "COPY"
        CopyPiece()  'in basetemplate, template, rotation
        yposdelta = 1 ' Don't move down if we rotate
        MovePiece()  'in: ypos, xpos, h
      ElseIf invalidMove = 2 Then
        ' Don't support shifting piece when hitting another piece to the right or left.
        xpos = 99 ' exit the loop
      Else
        ' if the rotated piece can't be placed, move it left or right and try again.
        xpos = xpos - invalidMove
        ValidateMove()  ' in: ypos, xpos, h, moveDirection ret: invalidMove = 1 or -1 or 2 if move is invalid, otherwise 0
      EndIf
    EndWhile

    If invalidMove <> 0 Then
      xpos = xposbk
      Array.SetValue(h, -1, basetemplate) ' Array.GetValue(h, -1) = the template name
      template = ""
    EndIf
  EndIf
EndSub


Sub DrawPreviewPiece
  xpos = PREVIEW_xpos
  ypos = PREVIEW_ypos
  h = nextPiece

  XOFFSETBK = XOFFSET
  YOFFSETBK = YOFFSET
  XOFFSET = XOFFSET + Array.GetValue(Array.GetValue(h, -1), "pviewx") ' Array.GetValue(h, -1) = the template name
  YOFFSET = YOFFSET + Array.GetValue(Array.GetValue(h, -1), "pviewy") ' Array.GetValue(h, -1) = the template name
  MovePiece()  'in: ypos, xpos, h

  XOFFSET = XOFFSETBK
  YOFFSET = YOFFSETBK
EndSub

' creates template that's a rotated basetemplate
Sub CopyPiece  'in basetemplate, template, rotation 
  L = Array.GetValue(basetemplate, "dim")

  If rotation = "CW" Then
    For i = 0 to BOXES - 1 ' x' = y y' = L - 1 - x
      v = Array.GetValue(basetemplate, i)

      'x = Math.Floor(v/10)
      'y = Math.Remainder(v, 10)

      ' new x and y
      x = (Math.Remainder(v, 10))
      y = (L - 1 - Math.Floor(v/10))
      Array.SetValue(template, i, x * 10 + y)
    EndFor
  ' Count-Cockwise is not currently used
  ElseIf rotation = "CCW" Then
    For i = 0 to BOXES - 1 ' x' = L - 1 - y y' = x
      v = Array.GetValue(basetemplate, i)
      'x = Math.Floor(v/10)
      'y = Math.Remainder(v, 10)

      ' new x and y
      x = (L - 1 - Math.Remainder(v, 10))
      y = Math.Floor(v/10)
      Array.SetValue(template, i, x * 10 + y)
    EndFor
  ElseIf rotation = "COPY" Then
    For i = 0 to BOXES - 1
      Array.SetValue(template, i, Array.GetValue(basetemplate, i))
    EndFor
  Else
    GraphicsWindow.ShowMessage("invalid parameter", "Error")
    Program.End()
  EndIf

  ' Copy the remain properties from basetemplate to template.
  Array.SetValue(template, "color", Array.GetValue(basetemplate, "color"))
  Array.SetValue(template, "dim", Array.GetValue(basetemplate, "dim"))
  Array.SetValue(template, "pviewx", Array.GetValue(basetemplate, "pviewx"))
  Array.SetValue(template, "pviewy", Array.GetValue(basetemplate, "pviewy"))
EndSub

Sub CreatePiece ' in: template ret: h
  ' Create a new handle, representing an arrayName, that will represent the piece
  hcount = hcount + 1
  h = Text.Append("piece", hcount)

  Array.SetValue(h, -1, template) ' Array.GetValue(h, -1) = the template name

  GraphicsWindow.PenWidth = 1
  GraphicsWindow.PenColor = "Black"
  GraphicsWindow.BrushColor = Array.GetValue(template, "color")

  For i = 0 to BOXES - 1
    s = Shapes.AddRectangle(BWIDTH, BWIDTH)
    Shapes.Move(s, -BWIDTH, -BWIDTH) ' move off screen
    Array.SetValue(h, i, s)
  EndFor
EndSub

Sub MovePiece 'in: ypos, xpos, h. ypos/xpos is 0-19, representing the top/left box coordinate of the piece on the canvas. h returned by CreatePiece
  For i = 0 to BOXES - 1
    v = Array.GetValue(Array.GetValue(h, -1), i)  ' Array.GetValue(h, -1) = the template name
    x = Math.Floor(v/10)
    y = Math.Remainder(v, 10)

    ' Array.GetValue(h, i) = box for piece h.
    ' xpos/ypos = are topleft of shape. x/y is the box offset within the shape.
    Shapes.Move(Array.GetValue(h, i), XOFFSET + xpos * BWIDTH + x * BWIDTH, YOFFSET + ypos * BWIDTH + y * BWIDTH)
  EndFor
EndSub

Sub ValidateMove ' in: ypos, xpos, h, moveDirection ret: invalidMove = 1 or -1 or 2 if move is invalid, otherwise 0
  i = 0
  invalidMove = 0
  While i < BOXES
    v = Array.GetValue(Array.GetValue(h, -1), i)  ' Array.GetValue(h, -1) = the template name

    'x/y is the box offset within the shape.
    x = Math.Floor(v/10)
    y = Math.Remainder(v, 10)

    If (x + xpos + moveDirection) < 0 Then
      invalidMove = -1
      i = BOXES ' force getting out of the loop
    EndIf

    If (x + xpos + moveDirection) >= CWIDTH Then
      invalidMove = 1
      i = BOXES ' force getting out of the loop
    EndIf

    If Array.GetValue("c", (x + xpos + moveDirection) + (y + ypos) * CWIDTH) <> "." Then
      invalidMove = 2
      i = BOXES ' force getting out of the loop
    EndIf

    i = i + 1
  EndWhile
EndSub


Sub CheckStop ' in: ypos, xpos, h ret: done
  done = 0
  i = 0
  While i < BOXES
    v = Array.GetValue(Array.GetValue(h, -1), i)  ' Array.GetValue(h, -1) = the template name

    'x/y is the box offset within the shape.
    x = Math.Floor(v/10)
    y = Math.Remainder(v, 10)

    If y + ypos > CHEIGHT Or Array.GetValue("c", (x + xpos) + (y + ypos) * CWIDTH) <> "." Then
      done = 1
      i = BOXES ' force getting out of the loop
    EndIf

    i = i + 1
  EndWhile

  ' If we need to stop the piece, move the box handles to the canvas
  If done = 1 Then
    For i = 0 to BOXES - 1
      v = Array.GetValue(Array.GetValue(h, -1), i) ' Array.GetValue(h, -1) = the template name
      'x = Math.Floor(v/10)
      'y = Math.Remainder(v, 10) 
      Array.SetValue("c", (Math.Floor(v/10) + xpos) + (Math.Remainder(v, 10) + ypos - 1) * CWIDTH, Array.GetValue(h, i))
    EndFor

    ' 1 points for every piece successfully dropped
    score = score + 1
    PrintScore()

    ' Delete clared lines
    DeleteLines()
  EndIf
EndSub


Sub DeleteLines
  linesCleared = 0

  ' Iterate over each row, starting from the bottom
  For y = CHEIGHT - 1 to 0 Step -1

    ' Check to see if the whole row is filled
    x = CWIDTH
    While x = CWIDTH
      x = 0
      While x < CWIDTH
        piece = Array.GetValue("c", x + y * CWIDTH)
        If piece = "." then
          x = CWIDTH
        EndIf
        x = x + 1
      EndWhile

      ' if non of them were empty (i.e "."), then remove the line.
      If x = CWIDTH Then

        ' Delete the line
        For x1 = 0 to CWIDTH - 1
          Shapes.Remove(Array.GetValue("c", x1 + y * CWIDTH))
        EndFor
        linesCleared = linesCleared + 1

        ' Move everything else down one.
        For y1 = y To 1 Step -1
          For x1 = 0 to CWIDTH - 1
            piece = Array.GetValue("c", x1 + (y1 - 1) * CWIDTH)
            Array.SetValue("c", x1 + y1 * CWIDTH, piece)
            Shapes.Move(piece, Shapes.GetLeft(piece), Shapes.GetTop(piece) + BWIDTH)
          EndFor
        EndFor
      EndIf
    EndWhile
  EndFor

  If linesCleared > 0 Then
    score = score + 100 * Math.Round(linesCleared * 2.15 - 1)
    PrintScore()
  EndIf
EndSub

Sub SetupCanvas
' GraphicsWindow.DrawResizedImage( Flickr.GetRandomPicture( "bricks" ), 0, 0, GraphicsWindow.Width, GraphicsWindow.Height)


  GraphicsWindow.BrushColor = GraphicsWindow.BackgroundColor
  GraphicsWindow.FillRectangle(XOFFSET, YOFFSET, CWIDTH*BWIDTH, CHEIGHT*BWIDTH)

  Program.Delay(200)
  GraphicsWindow.PenWidth = 1
  GraphicsWindow.PenColor = "Pink"
  For x = 0 To CWIDTH-1
    For y = 0 To CHEIGHT-1
      Array.SetValue("c", x + y * CWIDTH, ".") ' "." indicates spot is free
      GraphicsWindow.DrawRectangle(XOFFSET + x * BWIDTH, YOFFSET + y * BWIDTH, BWIDTH, BWIDTH)
    EndFor
  EndFor

  GraphicsWindow.PenWidth = 4
  GraphicsWindow.PenColor = "Black"
  GraphicsWindow.DrawLine(XOFFSET, YOFFSET, XOFFSET, YOFFSET + CHEIGHT*BWIDTH)
  GraphicsWindow.DrawLine(XOFFSET + CWIDTH*BWIDTH, YOFFSET, XOFFSET + CWIDTH*BWIDTH, YOFFSET + CHEIGHT*BWIDTH)
  GraphicsWindow.DrawLine(XOFFSET, YOFFSET + CHEIGHT*BWIDTH, XOFFSET + CWIDTH*BWIDTH, YOFFSET + CHEIGHT*BWIDTH)

  GraphicsWindow.PenColor = "Lime"
  GraphicsWindow.DrawLine(XOFFSET - 4, YOFFSET, XOFFSET - 4, YOFFSET + CHEIGHT*BWIDTH + 6)
  GraphicsWindow.DrawLine(XOFFSET + CWIDTH*BWIDTH + 4, YOFFSET, XOFFSET + CWIDTH*BWIDTH + 4, YOFFSET + CHEIGHT*BWIDTH + 6)
  GraphicsWindow.DrawLine(XOFFSET - 4, YOFFSET + CHEIGHT*BWIDTH + 4, XOFFSET + CWIDTH*BWIDTH + 4, YOFFSET + CHEIGHT*BWIDTH + 4)

  GraphicsWindow.PenColor = "Black"
  GraphicsWindow.BrushColor = "Pink"
  x = XOFFSET + PREVIEW_xpos * BWIDTH - BWIDTH
  y = YOFFSET + PREVIEW_ypos * BWIDTH - BWIDTH
  GraphicsWindow.FillRectangle(x, y, BWIDTH * 5, BWIDTH * 6)
  GraphicsWindow.DrawRectangle(x, y, BWIDTH * 5, BWIDTH * 6)

  GraphicsWindow.FillRectangle(x - 20, y + 190, 310, 170)
  GraphicsWindow.DrawRectangle(x - 20, y + 190, 310, 170)

  GraphicsWindow.BrushColor = "Black"
  GraphicsWindow.FontItalic = "False"
  GraphicsWindow.FontName = "Comic Sans MS"
  GraphicsWindow.FontSize = 16
  GraphicsWindow.DrawText(x, y + 200, "Game control keys:")
  GraphicsWindow.DrawText(x + 25, y + 220, "Left Arrow = Move piece left")
  GraphicsWindow.DrawText(x + 25, y + 240, "Right Arrow = Move piece right")
  GraphicsWindow.DrawText(x + 25, y + 260, "Up Arrow = Rotate piece")
  GraphicsWindow.DrawText(x + 25, y + 280, "Down Arrow = Drop piece")
  GraphicsWindow.DrawText(x, y + 320, "Press to stop game")

  Program.Delay(200) ' without this delay, the above text will use the fontsize of the score 

  GraphicsWindow.BrushColor = "Black"
  GraphicsWindow.FontName = "Georgia"
  GraphicsWindow.FontItalic = "True"
  GraphicsWindow.FontSize = 36
  GraphicsWindow.DrawText(x - 20, y + 400, "Small Basic Tetris")
  Program.Delay(200) ' without this delay, the above text will use the fontsize of the score 
  GraphicsWindow.FontSize = 16
  GraphicsWindow.DrawText(x - 20, y + 440, "ver.0.1")

  Program.Delay(200) ' without this delay, the above text will use the fontsize of the score 
  score = 0
  PrintScore()
EndSub


Sub PrintScore
  GraphicsWindow.PenWidth = 4
  GraphicsWindow.BrushColor = "Pink"
  GraphicsWindow.FillRectangle(500, 65, 153, 50)
  GraphicsWindow.BrushColor = "Black"
  GraphicsWindow.DrawRectangle(500, 65, 153, 50)
  GraphicsWindow.FontItalic = "False"
  GraphicsWindow.FontSize = 32
  GraphicsWindow.FontName = "Impact"
  GraphicsWindow.BrushColor = "Black"
  GraphicsWindow.DrawText(505, 70, Text.Append(Text.GetSubText( "00000000", 0, 8 - Text.GetLength( score ) ), score))
EndSub


Sub SetupTemplates
  ' each piece has 4 boxes.
  ' the index of each entry within a piece represents the box number (1-4)
  ' the value of each entry represents to box zero-based box coordinate within the piece: tens place is x, ones place y

  '_X_
  '_X_
  '_XX

  Array.SetValue("template1", 0, 10)
  Array.SetValue("template1", 1, 11)
  Array.SetValue("template1", 2, 12)
  Array.SetValue("template1", 3, 22)
  Array.SetValue("template1", "color", "Yellow")
  Array.SetValue("template1", "dim", 3)
  Array.SetValue("template1", "pviewx", -12)
  Array.SetValue("template1", "pviewy", 12)


  '_X_
  '_X_
  'XX_
  Array.SetValue("template2", 0, 10)
  Array.SetValue("template2", 1, 11)
  Array.SetValue("template2", 2, 12)
  Array.SetValue("template2", 3, 02)
  Array.SetValue("template2", "color", "Magenta")
  Array.SetValue("template2", "dim", 3)
  Array.SetValue("template2", "pviewx", 12)
  Array.SetValue("template2", "pviewy", 12)


  '_X_
  'XXX
  '_
  Array.SetValue("template3", 0, 10)
  Array.SetValue("template3", 1, 01)
  Array.SetValue("template3", 2, 11)
  Array.SetValue("template3", 3, 21)
  Array.SetValue("template3", "color", "Gray")
  Array.SetValue("template3", "dim", 3)
  Array.SetValue("template3", "pviewx", 0)
  Array.SetValue("template3", "pviewy", 25)


  'XX_
  'XX_
  '_
  Array.SetValue("template4", 0, 00)
  Array.SetValue("template4", 1, 10)
  Array.SetValue("template4", 2, 01)
  Array.SetValue("template4", 3, 11)
  Array.SetValue("template4", "color", "Cyan")
  Array.SetValue("template4", "dim", 2)
  Array.SetValue("template4", "pviewx", 12)
  Array.SetValue("template4", "pviewy", 25)


  'XX_
  '_XX
  '_
  Array.SetValue("template5", 0, 00)
  Array.SetValue("template5", 1, 10)
  Array.SetValue("template5", 2, 11)
  Array.SetValue("template5", 3, 21)
  Array.SetValue("template5", "color", "Green")
  Array.SetValue("template5", "dim", 3)
  Array.SetValue("template5", "pviewx", 0)
  Array.SetValue("template5", "pviewy", 25)


  '_XX
  'XX_
  '_
  Array.SetValue("template6", 0, 10)
  Array.SetValue("template6", 1, 20)
  Array.SetValue("template6", 2, 01)
  Array.SetValue("template6", 3, 11)
  Array.SetValue("template6", "color", "Blue")
  Array.SetValue("template6", "dim", 3)
  Array.SetValue("template6", "pviewx", 0)
  Array.SetValue("template6", "pviewy", 25)


  '_X
  '_X
  '_X
  '_X
  Array.SetValue("template7", 0, 10)
  Array.SetValue("template7", 1, 11)
  Array.SetValue("template7", 2, 12)
  Array.SetValue("template7", 3, 13)
  Array.SetValue("template7", "color", "Red")
  Array.SetValue("template7", "dim", 4)
  Array.SetValue("template7", "pviewx", 0)
  Array.SetValue("template7", "pviewy", 0)
EndSub
```

