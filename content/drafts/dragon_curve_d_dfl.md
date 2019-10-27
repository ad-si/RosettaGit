+++
title = "Dragon curve/D/DFL"
description = ""
date = 2013-09-23T11:34:55Z
aliases = []
[extra]
id = 8858
[taxonomies]
categories = []
tags = []
+++

{{libheader|DFL}}
Translation of Java

```d
//  dfl dragoncurve -gui -release -O

private import dfl.all;
import std.math, std.range;

class DrawForm: Form {

    private int[] turns;
    private double startingAngle;
    private double side;
    private immutable iter = 14; 

    this() {
        width = 800;
        height = 600;
        text = "DragonCurve";
        backColor = Color(0xFF, 0xFF, 0xFF);
        startPosition = FormStartPosition.CENTER_SCREEN;
        formBorderStyle = FormBorderStyle.FIXED_DIALOG;
        maximizeBox = false;

        turns = getSequence(iter);
        startingAngle = -iter * (PI / 4);
        side = 400 / pow(2, iter / 2.0);
    }

    private int[] getSequence(in int iterations) {
        int[] turnSequence;
        foreach (_; 0 .. iterations) {
            auto copy = turnSequence.retro;
            turnSequence ~= 1;
            foreach (turn; copy) {
                turnSequence ~= -turn;
            }
        }
        return turnSequence;
    }
    
    protected override void onPaint(PaintEventArgs ea) {
        super.onPaint(ea);
        
        Pen p = new Pen(Color(0, 0, 0));

        double angle = startingAngle;
        int x1 = 230, y1 = 350;
        int x2 = x1 + cast(int) (cos(angle) * side);
        int y2 = y1 + cast(int) (sin(angle) * side);
        ea.graphics.drawLine(p, x1, y1, x2, y2);
        x1 = x2;
        y1 = y2;
        foreach (turn; turns) {
            angle += turn * (PI / 2);
            x2 = x1 + cast(int) (cos(angle) * side);
            y2 = y1 + cast(int) (sin(angle) * side);
            ea.graphics.drawLine(p, x1, y1, x2, y2);
            x1 = x2;
            y1 = y2;
        }
    }
}

int main() {
    int result = 0; 
    try {
        Application.run(new DrawForm);
    }
    catch(Exception e) {
        msgBox(e.msg, "Fatal Error", MsgBoxButtons.OK, MsgBoxIcon.ERROR);        
        result = 1;
    }   
    return result;
}
```


Screenshot: [http://img534.imageshack.us/img534/2324/dfldragoncurve.png]
