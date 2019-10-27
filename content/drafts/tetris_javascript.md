+++
title = "Tetris/JavaScript"
description = ""
date = 2017-09-29T12:35:06Z
aliases = []
[extra]
id = 21620
[taxonomies]
categories = []
tags = []
+++

{{collection|Tetris}}
WASD or arrow keys.

==Code==
{{trans|Java}}

```javascript
<!DOCTYPE html>
<html lang='en'>

<head>
    <meta charset='UTF-8'>
    <style>
        canvas {
            position: absolute;
            top: 45%;
            left: 50%;
            width: 640px;
            height: 640px;
            margin: -320px 0 0 -320px;
        }
    </style>
</head>

<body>
    <canvas></canvas>
    <script>
        'use strict';
        var canvas = document.querySelector('canvas');
        canvas.width = 640;
        canvas.height = 640;

        var g = canvas.getContext('2d');

        var right = { x: 1, y: 0 };
        var down = { x: 0, y: 1 };
        var left = { x: -1, y: 0 };

        var EMPTY = -1;
        var BORDER = -2;

        var fallingShape;
        var nextShape;
        var dim = 640;
        var nRows = 18;
        var nCols = 12;
        var blockSize = 30;
        var topMargin = 50;
        var leftMargin = 20;
        var scoreX = 400;
        var scoreY = 330;
        var titleX = 130;
        var titleY = 160;
        var clickX = 120;
        var clickY = 400;
        var previewCenterX = 467;
        var previewCenterY = 97;
        var mainFont = 'bold 48px monospace';
        var smallFont = 'bold 18px monospace';
        var colors = ['green', 'red', 'blue', 'purple', 'orange', 'blueviolet', 'magenta'];
        var gridRect = { x: 46, y: 47, w: 308, h: 517 };
        var previewRect = { x: 387, y: 47, w: 200, h: 200 };
        var titleRect = { x: 100, y: 95, w: 252, h: 100 };
        var clickRect = { x: 50, y: 375, w: 252, h: 40 };
        var outerRect = { x: 5, y: 5, w: 630, h: 630 };
        var squareBorder = 'white';
        var titlebgColor = 'white';
        var textColor = 'black';
        var bgColor = '#DDEEFF';
        var gridColor = '#BECFEA';
        var gridBorderColor = '#7788AA';
        var largeStroke = 5;
        var smallStroke = 2;

        // position of falling shape
        var fallingShapeRow;
        var fallingShapeCol;

        var keyDown = false;
        var fastDown = false;

        var grid = [];
        var scoreboard = new Scoreboard();

        addEventListener('keydown', function (event) {
            if (!keyDown) {
                keyDown = true;

                if (scoreboard.isGameOver())
                    return;

                switch (event.key) {

                    case 'w':
                    case 'ArrowUp':
                        if (canRotate(fallingShape))
                            rotate(fallingShape);
                        break;

                    case 'a':
                    case 'ArrowLeft':
                        if (canMove(fallingShape, left))
                            move(left);
                        break;

                    case 'd':
                    case 'ArrowRight':
                        if (canMove(fallingShape, right))
                            move(right);
                        break;

                    case 's':
                    case 'ArrowDown':
                        if (!fastDown) {
                            fastDown = true;
                            while (canMove(fallingShape, down)) {
                                move(down);
                                draw();
                            }
                            shapeHasLanded();
                        }
                }
                draw();
            }
        });

        addEventListener('click', function () {
            startNewGame();
        });

        addEventListener('keyup', function () {
            keyDown = false;
            fastDown = false;
        });

        function canRotate(s) {
            if (s === Shapes.Square)
                return false;

            var pos = new Array(4);
            for (var i = 0; i < pos.length; i++) {
                pos[i] = s.pos[i].slice();
            }

            pos.forEach(function (row) {
                var tmp = row[0];
                row[0] = row[1];
                row[1] = -tmp;
            });

            return pos.every(function (p) {
                var newCol = fallingShapeCol + p[0];
                var newRow = fallingShapeRow + p[1];
                return grid[newRow][newCol] === EMPTY;
            });
        }

        function rotate(s) {
            if (s === Shapes.Square)
                return;

            s.pos.forEach(function (row) {
                var tmp = row[0];
                row[0] = row[1];
                row[1] = -tmp;
            });
        }

        function move(dir) {
            fallingShapeRow += dir.y;
            fallingShapeCol += dir.x;
        }

        function canMove(s, dir) {
            return s.pos.every(function (p) {
                var newCol = fallingShapeCol + dir.x + p[0];
                var newRow = fallingShapeRow + dir.y + p[1];
                return grid[newRow][newCol] === EMPTY;
            });
        }

        function shapeHasLanded() {
            addShape(fallingShape);
            if (fallingShapeRow < 2) {
                scoreboard.setGameOver();
                scoreboard.setTopscore();
            } else {
                scoreboard.addLines(removeLines());
            }
            selectShape();
        }

        function removeLines() {
            var count = 0;
            for (var r = 0; r < nRows - 1; r++) {
                for (var c = 1; c < nCols - 1; c++) {
                    if (grid[r][c] === EMPTY)
                        break;
                    if (c === nCols - 2) {
                        count++;
                        removeLine(r);
                    }
                }
            }
            return count;
        }

        function removeLine(line) {
            for (var c = 0; c < nCols; c++)
                grid[line][c] = EMPTY;

            for (var c = 0; c < nCols; c++) {
                for (var r = line; r > 0; r--)
                    grid[r][c] = grid[r - 1][c];
            }
        }

        function addShape(s) {
            s.pos.forEach(function (p) {
                grid[fallingShapeRow + p[1]][fallingShapeCol + p[0]] = s.ordinal;
            });
        }

        function Shape(shape, o) {
            this.shape = shape;
            this.pos = this.reset();
            this.ordinal = o;
        }

        var Shapes = {
            ZShape: [[0, -1], [0, 0], [-1, 0], [-1, 1]],
            SShape: [[0, -1], [0, 0], [1, 0], [1, 1]],
            IShape: [[0, -1], [0, 0], [0, 1], [0, 2]],
            TShape: [[-1, 0], [0, 0], [1, 0], [0, 1]],
            Square: [[0, 0], [1, 0], [0, 1], [1, 1]],
            LShape: [[-1, -1], [0, -1], [0, 0], [0, 1]],
            JShape: [[1, -1], [0, -1], [0, 0], [0, 1]]
        };

        function getRandomShape() {
            var keys = Object.keys(Shapes);
            var ord = Math.floor(Math.random() * keys.length);
            var shape = Shapes[keys[ord]];
            return new Shape(shape, ord);
        }

        Shape.prototype.reset = function () {
            this.pos = new Array(4);
            for (var i = 0; i < this.pos.length; i++) {
                this.pos[i] = this.shape[i].slice();
            }
            return this.pos;
        }

        function selectShape() {
            fallingShapeRow = 1;
            fallingShapeCol = 5;
            fallingShape = nextShape;
            nextShape = getRandomShape();
            if (fallingShape != null) {
                fallingShape.reset();
            }
        }

        function Scoreboard() {
            this.MAXLEVEL = 9;

            var level = 0;
            var lines = 0;
            var score = 0;
            var topscore = 0;
            var gameOver = true;

            this.reset = function () {
                this.setTopscore();
                level = lines = score = 0;
                gameOver = false;
            }

            this.setGameOver = function () {
                gameOver = true;
            }

            this.isGameOver = function () {
                return gameOver;
            }

            this.setTopscore = function () {
                if (score > topscore) {
                    topscore = score;
                }
            }

            this.getTopscore = function () {
                return topscore;
            }

            this.getSpeed = function () {

                switch (level) {
                    case 0: return 700;
                    case 1: return 600;
                    case 2: return 500;
                    case 3: return 400;
                    case 4: return 350;
                    case 5: return 300;
                    case 6: return 250;
                    case 7: return 200;
                    case 8: return 150;
                    case 9: return 100;
                    default: return 100;
                }
            }

            this.addScore = function (sc) {
                score += sc;
            }

            this.addLines = function (line) {

                switch (line) {
                    case 1:
                        this.addScore(10);
                        break;
                    case 2:
                        this.addScore(20);
                        break;
                    case 3:
                        this.addScore(30);
                        break;
                    case 4:
                        this.addScore(40);
                        break;
                    default:
                        return;
                }

                lines += line;
                if (lines > 10) {
                    this.addLevel();
                }
            }

            this.addLevel = function () {
                lines %= 10;
                if (level < this.MAXLEVEL) {
                    level++;
                }
            }

            this.getLevel = function () {
                return level;
            }

            this.getLines = function () {
                return lines;
            }

            this.getScore = function () {
                return score;
            }
        }

        function draw() {
            g.clearRect(0, 0, canvas.width, canvas.height);

            drawUI();

            if (scoreboard.isGameOver()) {
                drawStartScreen();
            } else {
                drawFallingShape();
            }
        }

        function drawStartScreen() {
            g.font = mainFont;

            fillRect(titleRect, titlebgColor);
            fillRect(clickRect, titlebgColor);

            g.fillStyle = textColor;
            g.fillText('Tetris', titleX, titleY);

            g.font = smallFont;
            g.fillText('click to start', clickX, clickY);
        }

        function fillRect(r, color) {
            g.fillStyle = color;
            g.fillRect(r.x, r.y, r.w, r.h);
        }

        function drawRect(r, color) {
            g.strokeStyle = color;
            g.strokeRect(r.x, r.y, r.w, r.h);
        }

        function drawSquare(colorIndex, r, c) {
            var bs = blockSize;
            g.fillStyle = colors[colorIndex];
            g.fillRect(leftMargin + c * bs, topMargin + r * bs, bs, bs);

            g.lineWidth = smallStroke;
            g.strokeStyle = squareBorder;
            g.strokeRect(leftMargin + c * bs, topMargin + r * bs, bs, bs);
        }

        function drawUI() {

            // background
            fillRect(outerRect, bgColor);
            fillRect(gridRect, gridColor);

            // the blocks dropped in the grid
            for (var r = 0; r < nRows; r++) {
                for (var c = 0; c < nCols; c++) {
                    var idx = grid[r][c];
                    if (idx > EMPTY)
                        drawSquare(idx, r, c);
                }
            }

            // the borders of grid and preview panel
            g.lineWidth = largeStroke;
            drawRect(gridRect, gridBorderColor);
            drawRect(previewRect, gridBorderColor);
            drawRect(outerRect, gridBorderColor);

            // scoreboard
            g.fillStyle = textColor;
            g.font = smallFont;
            g.fillText('hiscore    ' + scoreboard.getTopscore(), scoreX, scoreY);
            g.fillText('level      ' + scoreboard.getLevel(), scoreX, scoreY + 30);
            g.fillText('lines      ' + scoreboard.getLines(), scoreX, scoreY + 60);
            g.fillText('score      ' + scoreboard.getScore(), scoreX, scoreY + 90);

            // preview
            var minX = 5, minY = 5, maxX = 0, maxY = 0;
            nextShape.pos.forEach(function (p) {
                minX = Math.min(minX, p[0]);
                minY = Math.min(minY, p[1]);
                maxX = Math.max(maxX, p[0]);
                maxY = Math.max(maxY, p[1]);
            });
            var cx = previewCenterX - ((minX + maxX + 1) / 2.0 * blockSize);
            var cy = previewCenterY - ((minY + maxY + 1) / 2.0 * blockSize);

            g.translate(cx, cy);
            nextShape.shape.forEach(function (p) {
                drawSquare(nextShape.ordinal, p[1], p[0]);
            });
            g.translate(-cx, -cy);
        }

        function drawFallingShape() {
            var idx = fallingShape.ordinal;
            fallingShape.pos.forEach(function (p) {
                drawSquare(idx, fallingShapeRow + p[1], fallingShapeCol + p[0]);
            });
        }

       function animate(lastFrameTime) {
            var requestId = requestAnimationFrame(function () {
                animate(lastFrameTime);
            });
                        
            var time = new Date().getTime();
            var delay = scoreboard.getSpeed();

            if (lastFrameTime + delay < time) {

                if (!scoreboard.isGameOver()) {

                    if (canMove(fallingShape, down)) {
                        move(down);
                    } else {
                        shapeHasLanded();
                    }
                    draw();
                    lastFrameTime = time;

                } else {
                    cancelAnimationFrame(requestId);
                }
            }
        }

        function startNewGame() {
            initGrid();
            selectShape();
            scoreboard.reset();
            animate(-1);
        }

        function initGrid() {
            function fill(arr, value) {
                for (var i = 0; i < arr.length; i++) {
                    arr[i] = value;
                }
            }
            for (var r = 0; r < nRows; r++) {
                grid[r] = new Array(nCols);
                fill(grid[r], EMPTY);
                for (var c = 0; c < nCols; c++) {
                    if (c === 0 || c === nCols - 1 || r === nRows - 1)
                        grid[r][c] = BORDER;
                }
            }
        }

        function init() {
            initGrid();
            selectShape();
            draw();
        }

        init();
    </script>

</body>

</html>
```

