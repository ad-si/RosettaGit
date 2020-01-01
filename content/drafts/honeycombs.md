+++
title = "Honeycombs"
description = ""
date = 2019-09-05T10:22:22Z
aliases = []
[extra]
id = 9782
[taxonomies]
categories = []
tags = []
+++

{{task}}[[Category:GUI]]

The task is to produce a matrix of 20 hexagon shaped widgets in a honeycomb arrangement. The matrix should be arranged in such a manner that there are five
columns of four hexagons. The hexagons in columns one, three and five are aligned horizontally, whereas the hexagons in columns two and four occupy a lower position within the arrangement. Each hexagon should be the same colour, and should
display a unique randomly selected single capital letter on the front. The application should now wait for the user to select a hexagon, either by using a pointing device, or by pressing a key that carries a corresponding letter on a hexagon. For platforms that support pointing devices and keyboards, the application should support both methods of selection. A record of the chosen letters should be maintained and the code should be suitably commented, at the point where the the selected letter has been determined. The selected hexagon should now change colour on the display. The cycle repeats until the user has chosen all of the letters. Note that each letter can only be selected once and previously selected hexagons retain their colour after selection. The program terminates when all letters have been chosen.

Optionally: output the list of selected letters and show the last selected letter, cater for a different number of columns or a different number of hexagons in each column, cater for two players, (turns alternate and the hexagons change a different colour depending on whether they were selected by player one or player two and records of both players selections are maintained.)

[[image:honeycomb.gif]]


## ActionScript

{{works with|Flash Player|Flash Player|10 or higher}}
{{works with|Adobe AIR|AIR|1.5 or higher}}

Honeycomb class:

```ActionScript3

package  {

    import flash.display.GraphicsPathCommand;
    import flash.display.Sprite;
    import flash.text.TextField;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFormat;

    /**
     * A honeycomb.
     */
    public class Honeycomb extends Sprite {

        /**
         * The sine of 60 degrees.
         *
         * @private
         */
        private static const SIN_60:Number = Math.sin(Math.PI / 3);

        /**
         * The cosine of 60 degrees.
         *
         * @private
         */
        private static const COS_60:Number = Math.cos(Math.PI / 3);

        /**
         * The drawing commands to be passed to Graphics.drawPath()
         *
         * @private
         */
        private static var _gCommands:Vector.<int> = new Vector.<int>(7, true);

        /**
         * The coordinates to be passed to Graphics.drawPath()
         *
         * @private
         */
        private static var _gCoords:Vector.<Number> = new Vector.<Number>(14, true);

        /**
         * The side length of the last honeycomb created.
         */
        private static var _lastSide:Number;

        /**
         * The horizontal space difference (between the leftmost and topmost vertex) of the last hexagon drawn.
         *
         * @private
         */
        private static var _lastHSpace:Number;

        /**
         * The heightof the last hexagon drawn.
         *
         * @private
         */
        private static var _lastHeight:Number;

        {
            _staticInit();
        }

        /**
         * Initialises the Honeycomb class.
         *
         * @private
         */
        private static function _staticInit():void {
            _gCommands[0] = GraphicsPathCommand.MOVE_TO;
            _gCommands[1] = _gCommands[2] = _gCommands[3] = _gCommands[4] = _gCommands[5] = _gCommands[6] = GraphicsPathCommand.LINE_TO;
        }

        /**
         * Calculates the points of the hexagon for a given side length.
         *
         * @param side The length of the side.
         */
        private static function _calculatePoints(side:Number):void {

            var height:Number = side * SIN_60 * 2;
            var hSpace:Number = side * COS_60;

            _gCoords[0] = _gCoords[12] = 0;
            _gCoords[2] = _gCoords[10] = hSpace;
            _gCoords[4] = _gCoords[8] = hSpace + side;
            _gCoords[6] = side + hSpace * 2;

            _gCoords[1] = _gCoords[7] = _gCoords[13] = height / 2;
            _gCoords[3] = _gCoords[5] = height;
            _gCoords[9] = _gCoords[11] = 0;

            _lastSide = side;
            _lastHSpace = hSpace;
            _lastHeight = height;

        }

        /**
         * The side length of the honeycomb.
         *
         * @private
         */
        private var _side:Number;

        /**
         * The text field displaying the character in the honeycomb.
         *
         * @private
         */
        private var _text:TextField;

        /**
         * The character code of the character in the honeycomb.
         *
         * @private
         */
        private var _charCode:uint;

        /**
         * Whether the honeycomb has been activated (i.e. the activate() method has been called).
         *
         * @private
         */
        private var _activated:Boolean = false;

        /**
         * Creates a new Honeycomb object.
         *
         * @param side The length of the side of the honeycomb.
         * @param fill The honeycomb's fill colour.
         * @param letter The character code of the letter to be displayed in the honeycomb.
         * @param textColour The colour of the letter displayed inside the honeycomb.
         */
        public function Honeycomb(side:Number, fill:uint, letter:uint, textColour:uint) {
            _init(side, fill, letter, textColour);
        }

        /**
         * Initialises the Honeycomb object.
         *
         * @param side The length of the side of the honeycomb.
         * @param fill The honeycomb's fill colour.
         * @param letter The character code of the letter to be displayed in the honeycomb.
         * @param textColour The colour of the letter displayed inside the honeycomb.
         */
        private function _init(side:Number, fill:uint, letter:uint, textColour:uint):void {

            mouseChildren = false;
            buttonMode = true;
            useHandCursor = false;

            graphics.beginFill(fill);
            graphics.lineStyle(3, 0x000000);

            if ( side != _lastSide )
                _calculatePoints(side);

            _side = side;
            _charCode = letter;

            graphics.drawPath(_gCommands, _gCoords);

            _text = new TextField();
            _text.autoSize = TextFieldAutoSize.CENTER;
            _text.defaultTextFormat = new TextFormat('_sans', side * 1.2, textColour, true);
            _text.text = String.fromCharCode(letter);
            _text.x = (side + _lastHSpace * 2 - _text.width) / 2;
            _text.y = (_lastHeight - _text.height) / 2;

            addChild(_text);

        }

        /**
         * The character code of the character in the honeycomb.
         */
        public function get charCode():uint {
            return _charCode;
        }

        /**
         * Whether the honeycomb has been activated (i.e. the activate() method has been called).
         */
        public function get activated():Boolean {
            return _activated;
        }

        /**
         * Activates the honeycomb and changes its colour.
         *
         * @param backColour The new fill colour of the honeycomb.
         * @param textColour The new text colour of the honeycomb.
         */
        public function activate(backColour:uint, textColour:uint):void {

            if ( _side != _lastSide )
                _calculatePoints(_side);

            graphics.beginFill(backColour);
            graphics.drawPath(_gCommands, _gCoords);

            var textFormat:TextFormat = _text.getTextFormat();
            textFormat.color = textColour;
            _text.setTextFormat(textFormat);

            _activated = true;

        }

    }

}

```


Document (main) class:

```ActionScript3

package  {

    import flash.display.Sprite;
    import flash.events.Event;
    import flash.events.KeyboardEvent;
    import flash.events.MouseEvent;
    import flash.text.TextField;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFormat;
    import flash.utils.Dictionary;

    /**
     * The Honeycomb game.
     */
    public class Main extends Sprite {

        /**
         * The fill colour for unselected honeycombs.
         *
         * @private
         */
        private static const FILL_COLOUR1:uint = 0xFFFF00;

        /**
         * The text colour for unselected honeycombs.
         *
         * @private
         */
        private static const FILL_COLOUR2:uint = 0xFF00FF;

        /**
         * The fill colour for selected honeycombs.
         *
         * @private
         */
        private static const TEXT_COLOUR1:uint = 0xFF0000;

        /**
         * The text colour for selected honeycombs.
         *
         * @private
         */
        private static const TEXT_COLOUR2:uint = 0x000000;

        /**
         * The honeycombs being displayed. They can be accesses using their character code as the key.
         *
         * @private
         */
        private var _honeycombs:Dictionary = new Dictionary();

        /**
         * The text field showing the selected letters.
         *
         * @private
         */
        private var _selectedLettersOutputField:TextField;

        /**
         * Entry point of the application.
         */
        public function Main() {
            if ( stage ) init();
            else addEventListener(Event.ADDED_TO_STAGE, init)
        }

        /**
         * Initialises the Main object when it is added to the stage.
         *
         * @private
         */
        private function init(e:Event = null):void {

            removeEventListener(Event.ADDED_TO_STAGE, init);

            var side:uint = 35;
            var hSpace:Number = side * Math.cos(Math.PI / 3);
            var height:Number = side * Math.sin(Math.PI / 3) * 2;

            var i:uint, j:uint, comb:Honeycomb, colX:Number = 0, rowY:Number = 0, char:uint;
            var usedCodes:Vector.<uint> = new Vector.<uint>();

            for ( i = 0; i < 5; i++ ) {
                for ( j = 0; j < 4; j++ ) {

                    // Select a character to display. If it is already used, repeat this process until an unused
                    // character is found.

                    do
                        char = uint(Math.random() * 26) + 0x41;
                    while ( usedCodes.indexOf(char) != -1 );

                    usedCodes[usedCodes.length] = char;

                    comb = new Honeycomb(side, FILL_COLOUR1, char, TEXT_COLOUR1);
                    comb.x = colX + 30;
                    comb.y = rowY + 30 + height * j;
                    addChild(comb);

                    _honeycombs[char] = comb;

                }

                if ( rowY == 0 )
                    rowY = height / 2;
                else
                    rowY = 0;

                colX += side + hSpace;
            }

            _selectedLettersOutputField = new TextField();
            _selectedLettersOutputField.x = 30;
            _selectedLettersOutputField.y = 30 + height * 5;
            _selectedLettersOutputField.defaultTextFormat = new TextFormat(null, 18);
            _selectedLettersOutputField.multiline = true;
            _selectedLettersOutputField.autoSize = TextFieldAutoSize.LEFT;
            _selectedLettersOutputField.text = "Selected letters:\n";

            addChild(_selectedLettersOutputField);

            // Since the MouseEvent.CLICK event bubbles, it is sufficient to add the listener to the Main object
            // itself rather than to each honeycomb individually, and the event's target property will always
            // be the clicked honeycomb.

            addEventListener(MouseEvent.CLICK, _onMouseClick);
            stage.addEventListener(KeyboardEvent.KEY_DOWN, _onKeyDown);

        }

        /**
         * Function called when a honeycomb is clicked.
         *
         * @private
         */
        private function _onMouseClick(e:MouseEvent):void {
            var comb:Honeycomb = e.target as Honeycomb;

            if ( comb && ! comb.activated ) {
                comb.activate(FILL_COLOUR2, TEXT_COLOUR2);
                _selectedLettersOutputField.appendText(String.fromCharCode(comb.charCode));
            }
        }

        /**
         * Function called when a keyboard key is pressed.
         *
         * @private
         */
        private function _onKeyDown(e:KeyboardEvent):void {
            var char:uint = e.charCode;
            if ( char > 0x60 )
                // Convert lowercase to uppercase
                char -= 0x20;

            var comb:Honeycomb = _honeycombs[char] as Honeycomb;

            if ( comb && ! comb.activated ) {
                comb.activate(FILL_COLOUR2, TEXT_COLOUR2);
                _selectedLettersOutputField.appendText(String.fromCharCode(char));
            }
        }

    }

}

```




## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      ALTERNATE = 1
      VDU 23,22,252;252;8,16,16,128
      *FONT Arial,24,B

      Letters$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      Letters% = !^Letters$
      FOR i% = 0 TO 24
        SWAP Letters%?i%, Letters%?(i%+RND(26-i%)-1)
      NEXT

      DIM xpos%(20), ypos%(20), hrgn%(20)
      C% = 1
      FOR Y% = 36 TO 192 STEP 52
        FOR X% = 35 TO 215 STEP 90
          xpos%(C%) = X%
          ypos%(C%) = Y%
          C% += 1
        NEXT
        FOR X% = 80 TO 170 STEP 90
          xpos%(C%) = X%
          ypos%(C%) = Y%+26
          C% += 1
        NEXT
      NEXT

      REM Plot the hexagons:
      FOR C% = 1 TO 20
        hrgn%(C%) = FNplothexagon(xpos%(C%), ypos%(C%), &00FFFF, \
        \           MID$(Letters$,C%,1), &0000FF)
      NEXT
      SYS "InvalidateRect", @hwnd%, 0, 0

      REM Initialise word selected:
      Word$ = ""

      REM Monitor mouse clicks:
      ON MOUSE PROCmouse(@wparam%,@lparam%) : RETURN

      REM Monitor keypresses:
      REPEAT
        key$ = INKEY$(1)
        IF key$ >= "a" key$ = CHR$(ASCkey$-32)
        C% = INSTR(Letters$, key$)
        IF C% IF C%<21 IF hrgn%(C%) PROCselect(C%)
      UNTIL FALSE
      END

      REM Select a hexagon with the keyboard or mouse:
      DEF PROCselect(C%)
      hrgn%(C%) = 0 * FNplothexagon(xpos%(C%), ypos%(C%), &FF00FF, \
      \               MID$(Letters$,C%,1), &000000)
      SYS "InvalidateRect", @hwnd%, 0, 0
      Word$ += MID$(Letters$, C%, 1)
      SYS "SetWindowText", @hwnd%, Word$
      ENDPROC

      DEF PROCmouse(W%, L%)
      LOCAL C%, R%
      IF W%<>1 ENDPROC
      FOR C% = 1 TO 20
        SYS "PtInRegion", hrgn%(C%), L% AND &FFFF, L% >>> 16 TO R%
        IF R% PROCselect(C%)
      NEXT
      ENDPROC

      DEF FNplothexagon(x%, y%, hcol%, text$, tcol%)
      LOCAL brush%, pen%, hrgn%, pt%(), size{}
      DIM pt%(5,1), size{dx%,dy%}
      pt%() = x%-30,y%,x%-15,y%+26,x%+15,y%+26,x%+30,y%,x%+15,y%-26,x%-15,y%-26
      SYS "CreatePen", 0, 3, 0 TO pen%
      SYS "CreateSolidBrush", hcol% TO brush%
      SYS "SelectObject", @memhdc%, pen% TO pen%
      SYS "SelectObject", @memhdc%, brush% TO brush%
      SYS "Polygon", @memhdc%, ^pt%(0,0), 6
      SYS "SelectObject", @memhdc%, pen% TO pen%
      SYS "SelectObject", @memhdc%, brush% TO brush%
      SYS "DeleteObject", pen%
      SYS "DeleteObject", brush%
      SYS "GetTextExtentPoint32", @memhdc%, text$, LEN(text$), size{}
      SYS "SetTextColor", @memhdc%, tcol%
      SYS "SetBkColor", @memhdc%, hcol%
      SYS "TextOut", @memhdc%, x%-size.dx%/2, y%-size.dy%/2, text$, LEN(text$)
      SYS "CreatePolygonRgn", ^pt%(0,0), 6, ALTERNATE TO hrgn%
      = hrgn%
```

[[File:honeycombs_bbc.gif]]


## C



```C


/* Program for gtk3 */
/* discovery: essential to use consistent documentation */
/* compilation on linux: */
/* $ a=./hexagon && make -k "CFLAGS=$( pkg-config --cflags gtk+-3.0 )" "LOADLIBES=$( pkg-config --libs gtk+-3.0 )" $a && $a --gtk-debug=all */
/* search for  to do */
/* The keyboard and mouse callbacks increment the "selected" status, */
/* of the matching hexagon, */
/* then invalidate the drawing window which triggers a draw event. */
/* The draw callback redraws the screen and tests for completion, */
/* upon which the program spits back the characters selected and exits */

#include<math.h>
#include<string.h>
#include<stdlib.h>
#include<gtk/gtk.h>

static GdkPixbuf*create_pixbuf(const gchar*filename) {
  GdkPixbuf*pixbuf;
  GError*error = NULL;
  pixbuf = gdk_pixbuf_new_from_file(filename, &error);
  if(!pixbuf) {
    fprintf(stderr,"\n%s\n", error->message);
    g_error_free(error);
  }
  return pixbuf;
}

#define NGON struct ngon
NGON {
  double Cx,Cy, r;
  int sides, selected;
  char c;
};

GRand*random_numbers = NULL;

#define R 20
#define TAU (2*M_PI)	   /* http://laughingsquid.com/pi-is-wrong/ */
#define OFFSET_X (1+sin(TAU/12))
#define OFFSET_Y (cos(TAU/12))
#define ODD(A) ((A)&1)

static void initialize_hexagons(NGON*hs,size_t n) {
  NGON*h;
  gint i,broken;
  GQueue*shuffler = g_queue_new();
  if (NULL == shuffler) {
    fputs("\ncannot allocate shuffling queue.  quitting!\n",stderr);
    exit(EXIT_FAILURE);
  }
  /* randomize characters by stuffing them onto a double end queue
     and popping them off from random positions */
  if ((broken = (NULL == random_numbers)))
    random_numbers = g_rand_new();
  for (i = 'A'; i <= 'Z'; ++i)
    g_queue_push_head(shuffler,GINT_TO_POINTER(i));
  memset(hs,0,n*(sizeof(NGON)));
  hs[n-1].sides = -1;		/* assign the sentinel */
  for (h = hs; !h->sides; ++h) {
    int div = (h-hs)/4, mod = (h-hs)%4;
    h->sides = 6;
    h->c = GPOINTER_TO_INT(
	     g_queue_pop_nth(
	       shuffler,
	       g_rand_int_range(
		 random_numbers,
		 (gint32)0,
		 (gint32)g_queue_get_length(shuffler))));
    fputc(h->c,stderr);
    h->r = R;
    h->Cx = R*(2+div*OFFSET_X), h->Cy = R*(2*(1+mod*OFFSET_Y)+ODD(div)*OFFSET_Y);
    fprintf(stderr,"(%g,%g)\n",h->Cx,h->Cy);
  }
  fputc('\n',stderr);
  g_queue_free(shuffler);
  if (broken)
    g_rand_free(random_numbers);
}

static void add_loop(cairo_t*cr,NGON*hs,int select) {
  NGON*h;
  double r,Cx,Cy,x,y;
  int i, sides;
  for (h = hs; 0 < (sides = h->sides); ++h)
    if ((select && h->selected) || (select == h->selected)) {
      r = h->r, Cx = h->Cx, Cy = h->Cy;
      i = 0;
      x = Cx+r*cos(TAU*i/sides), y = Cy+r*sin(TAU*i/sides), cairo_move_to(cr,x,y);
      for (i = 1; i < sides; ++i)
        x = Cx+r*cos(TAU*i/sides), y = Cy+r*sin(TAU*i/sides), cairo_line_to(cr,x,y);
      cairo_close_path(cr);
    }
}

static int make_labels(cairo_t*cr,NGON*hs,int select) {
  NGON*h;
  int i = 0;
  char text[2];
  text[1] = 0;
  for (h = hs; 0 < h->sides; ++h)
    if ((select && h->selected) || (select == h->selected))
      /* yuck, need to measure the font.  Better to use pango_cairo */
      *text = h->c, cairo_move_to(cr,h->Cx,h->Cy), cairo_show_text(cr,text), ++i;
  return i;
}

static int archive(int a) {
  static GQueue*q = NULL;
  if ((NULL == q) && (NULL == (q = g_queue_new()))) {
    fputs("\ncannot allocate archival queue.  quitting!\n",stderr);
    exit(EXIT_FAILURE);
  }
  if (a < -1)			/* reset */
    return g_queue_free(q), q = NULL, 0;
  if (-1 == a)			/* pop off tail */
    return g_queue_is_empty(q) ? 0 : GPOINTER_TO_INT(g_queue_pop_tail(q));
  if (!a)			/* peek most recent entry */
    return g_queue_is_empty(q) ? 0 : GPOINTER_TO_INT(g_queue_peek_head(q));
  g_queue_push_head(q,GINT_TO_POINTER(a)); /* store */
  return a;
}

/* to do: use appropriate sizing, use the cairo transformation matrix */
static gboolean draw(GtkWidget*widget,cairo_t*cr,gpointer data) {

  /* unselected fill in yellow */
  cairo_set_source_rgba(cr,0.8,0.8,0,1),
  add_loop(cr,(NGON*)data,0);
  cairo_fill(cr);

  /* selected fill, purple */
  cairo_set_source_rgba(cr,0.8,0,0.8,1);
  add_loop(cr,(NGON*)data,1);
  cairo_fill_preserve(cr);

  /* all outlines gray, background shows through, fun fun! */
  cairo_set_line_width (cr, 3.0);
  cairo_set_source_rgba(cr,0.7,0.7,0.7,0.7);
  add_loop(cr,(NGON*)data,0);
  cairo_stroke(cr);

  /* select labels */
  cairo_set_source_rgba(cr,0,1,0,1);
  make_labels(cr,(NGON*)data,1);
  cairo_stroke(cr);

  /* unselected labels */
  cairo_set_source_rgba(cr,1,0,0,1);
  /* to do: clean up this exit code */
  if (!make_labels(cr,(NGON*)data,0)) {
    int c;
    putchar('\n');
    while ((c = archive(-1)))
      putchar(c);
    puts("\nfinished");
    archive(-2);
    exit(EXIT_SUCCESS);
  }
  cairo_stroke(cr);

  return TRUE;
}

/*the widget is a GtkDrawingArea*/
static gboolean button_press_event(GtkWidget*widget,const GdkEvent*event,gpointer data) {
  NGON*h,*hs = (NGON*)data;
  gdouble x_win, y_win;
  if (!gdk_event_get_coords(event,&x_win,&y_win))
    fputs("\nBUTTON, gdk_event_get_coords(event,&x_win,&y_win)) failed\n",stderr);
  else {
    fprintf(stderr,"x_win=%g y_win=%g\n",(double)x_win,(double)y_win);
    for (h = hs; 0 < h->sides; ++h) /* detection algorithm: */
      /* if mouse click within inner radius of hexagon */
      /* Much easier than all in-order cross products have same sign test! */
      if ((pow((x_win-h->Cx),2)+pow((y_win-h->Cy),2)) < pow((h->r*cos(TAU/(180/h->sides))),2)) {
	++h->selected;
	archive(h->c);
	/* discovery: gdk_window_invalidate_region with NULL second argument does not work */
	gdk_window_invalidate_rect(gtk_widget_get_window(widget),(const GdkRectangle*)NULL,TRUE);
	break;
      }
  }
  return TRUE;
}

static gboolean key_press_event(GtkWidget*widget,const GdkEvent*event,gpointer data) {
  NGON*h,*hs = (NGON*)data;
  guint keyval;
  int unicode;
  if (!gdk_event_get_keyval(event,&keyval))
    fputs("\nKEY!  gdk_event_get_keyval(event,&keyval)) failed.\n",stderr);
  else {
    unicode = (int)gdk_keyval_to_unicode(gdk_keyval_to_upper(keyval));
    fprintf(stderr,"key with unicode value: %d\n",unicode);
    for (h = hs; 0 < h->sides; ++h) /* look for a matching character associated with a hexagon */
      if (h->c == unicode) {
	++(h->selected);
	archive(h->c);
	/* discovery: gdk_window_invalidate_region with NULL second argument does not work */
	gdk_window_invalidate_rect(gtk_widget_get_window(widget),(const GdkRectangle*)NULL,TRUE);
	break;
      }
  }
  return TRUE;
}

int main(int argc,char*argv[]) {
  GtkWidget *window, *vbox, /* *label, */ *drawing_area;
  NGON ngons[21];	   /* sentinal has negative number of sides */

  /* discovery: gtk_init removes gtk debug flags, such as --gtk-debug=all */
  /*   also calls gdk_init which handles --display and --screen or other X11 communication issues */
  gtk_init(&argc, &argv);

  /* GTK VERSION 3.2.0 */
  fprintf(stderr,"GTK VERSION %d.%d.%d\n",GTK_MAJOR_VERSION,GTK_MINOR_VERSION,GTK_MICRO_VERSION);

  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  /* discovery: to make window transparent I have to use the alpha channel correctly */

  /* discovery: GTK_WINDOW(GtkWidget*) casts the widget to window */
  /* discovery: window in the function name?  use GTK_WINDOW.  g_ in function name?  use G_OBJECT */
  gtk_window_set_title(GTK_WINDOW(window), "Rosetta Code Honeycomb, C with GTK");
  gtk_window_set_default_size(GTK_WINDOW(window), 308, 308+12+8); /* XxY */
  /* discovery: making the window vanish does not stop the program */
  /* discovery: NULL is placeholder for extra data sent to the callback */
  g_signal_connect_swapped(G_OBJECT(window),"destroy",G_CALLBACK(gtk_main_quit),NULL);

  /* I created /tmp/favicon.ico from http://rosettacode.org/favicon.ico */
  /* Your window manager could use the icon, if it exists, and you fix the file name */
  gtk_window_set_icon(GTK_WINDOW(window),create_pixbuf("/tmp/favicon.ico"));

  vbox = gtk_vbox_new(TRUE,1);
  gtk_container_add(GTK_CONTAINER(window),vbox);

  /* to do: fix the label widget */
  /* I did not learn to control multiple box packing, and I was */
  /* too lazy to make the label widget accessible.  Plan was to */
  /* insert the most recent character using "peek" option of the archive */
#if 0
  label = gtk_label_new("None Selected");
  gtk_widget_set_size_request(label,308,20);
  gtk_box_pack_end(GTK_BOX(vbox),label,FALSE,TRUE,4);
#endif

  drawing_area = gtk_drawing_area_new();
  gtk_widget_set_events(drawing_area,GDK_BUTTON_PRESS_MASK|GDK_KEY_PRESS_MASK|GDK_EXPOSURE_MASK);

  random_numbers = g_rand_new();
  initialize_hexagons(ngons,G_N_ELEMENTS(ngons));
  /* Discovery: expose_event changed to draw signal.  We no longer need configure-event */
  g_signal_connect(G_OBJECT(drawing_area),"draw",G_CALLBACK(draw),(gpointer)ngons);

  g_signal_connect(G_OBJECT(drawing_area),"button-press-event",G_CALLBACK(button_press_event),(gpointer)ngons);
  g_signal_connect(G_OBJECT(drawing_area),"key-press-event",G_CALLBACK(key_press_event),(gpointer)ngons);
  gtk_widget_set_size_request(drawing_area, 308, 308); /* XxY */
  gtk_box_pack_start(GTK_BOX(vbox),drawing_area,TRUE,TRUE,4);

  /* Discovery: must allow focus to receive keyboard events */
  gtk_widget_set_can_focus(drawing_area,TRUE);

  /* Discovery: can set show for individual widgets or use show_all */
  gtk_widget_show_all(window);
  gtk_main();
  g_rand_free(random_numbers);
  return EXIT_SUCCESS;
}

```



## C#


```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;

////////////////////////////////////////////////////////////////////////////////////////////////////
// namespace: Honeycombs
//
// summary:	WPF implementation of Rosetta Code Honeycombs task.  Uses Polygon shapes as hexes.
////////////////////////////////////////////////////////////////////////////////////////////////////

namespace Honeycombs
{
    public partial class MainWindow
    {
        private const int RowCount = 4;
        private const int ColCount = 5;
        private const int LabelSize = 20;
        private readonly char[] _permutedChars;

        public MainWindow()
        {
            if (RowCount * ColCount > 26)
#pragma warning disable 162
            {
                throw new ArgumentException("Too many cells");
            }
#pragma warning restore 162
            _permutedChars = GetPermutedChars(RowCount * ColCount);

            // VS Generated code not included
            InitializeComponent();
        }

        private static char[] GetPermutedChars(int characterCount)
        {
            const string allChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            var rnd = new Random();
            var chars = new char[allChars.Length];

            for (int i = 0; i < allChars.Length; i++)
            {
                chars[i] = allChars[i];
            }

            for (int i = 0; i < characterCount; i++)
            {
                int swapIndex = rnd.Next() % (allChars.Length - i);
                char tmp = chars[swapIndex + i];
                chars[swapIndex + i] = chars[i];
                chars[i] = tmp;
            }
            return chars;
        }

        private void SetHexProperties(UIElementCollection hexes, double cellSize)
        {
            int charIndex = 0;
            List<Polygon> hexList = hexes.Cast<Polygon>().ToList();

            foreach (Polygon element in hexList)
            {
                SetHexProperties(element, _permutedChars[charIndex++], cellSize);
            }
        }

        private void SetHexProperties(Polygon hex, char charToSet, double cellSize)
        {
            var tag = (Tuple<int, int, double, double>) hex.Tag;
            double cellX = tag.Item3;
            double cellY = tag.Item4;

            // We place the text in a grid centered on the hex.
            // The grid will then center the text within itself.

            var centeringGrid = new Grid();
            centeringGrid.Width = centeringGrid.Height = 2 * cellSize;
            centeringGrid.SetValue(Canvas.LeftProperty, cellX - cellSize);
            centeringGrid.SetValue(Canvas.TopProperty, cellY - cellSize);
            centeringGrid.IsHitTestVisible = false;
            HoneycombCanvas.Children.Add(centeringGrid);

            var label = new TextBlock
                {
                    Text = new string(charToSet, 1),
                    FontFamily = new FontFamily("Segoe"),
                    FontSize = LabelSize
                };
            label.HorizontalAlignment = HorizontalAlignment.Center;
            label.VerticalAlignment = VerticalAlignment.Center;
            label.IsHitTestVisible = false;
            centeringGrid.Children.Add(label);

            // Reset the tag to keep track of the character in the hex
            hex.Tag = charToSet;
            hex.Fill = new SolidColorBrush(Colors.Yellow);
            hex.Stroke = new SolidColorBrush(Colors.Black);
            hex.StrokeThickness = cellSize / 10;

            // Mouse down event handler for the hex
            hex.MouseDown += hex_MouseDown;
        }

        private void hex_MouseDown(object sender, MouseButtonEventArgs e)
        {
            var hex = sender as Shape;
            if (hex == null)
            {
                throw new InvalidCastException("Non-shape in Honeycomb");
            }

            // Get the letter for this hex
            var ch = (char) hex.Tag;

            // Add it to our Letters TextBlock
            Letters.Text = Letters.Text + ch;

            // Color the hex magenta
            hex.Fill = new SolidColorBrush(Colors.Magenta);

            // Remove the mouse down event handler so we won't hit on this hex again
            hex.MouseDown -= hex_MouseDown;
        }

        private static void GetCombSize(double actualHeight, double actualWidth, int columns, int rows,
                                        out double cellSize, out double combHeight, out double combWidth)
        {
            double columnFactor = (3 * columns + 1) / 2.0;
            double rowFactor = (Math.Sqrt(3) * (2 * rows + 1)) / 2.0;
            double cellFromWidth = actualWidth / columnFactor;
            double cellFromHeight = actualHeight / rowFactor;
            cellSize = Math.Min(cellFromWidth, cellFromHeight);
            combWidth = cellSize * columnFactor;
            combHeight = cellSize * rowFactor;
        }

        private static void AddCells(Canvas canvas, double cellSize, int columns, int rows)
        {
            double rowHeight = cellSize * Math.Sqrt(3) / 2;

            for (int row = 0; row < rows; row++)
            {
                AddRow(rowHeight, canvas, cellSize, columns, row);
                rowHeight += cellSize * Math.Sqrt(3);
            }
        }

        private static void AddRow(double rowHeight, Canvas canvas, double cellSize, int columnCount, int row)
        {
            double cellX = cellSize;
            double cellHeight = cellSize * Math.Sqrt(3);

            for (int col = 0; col < columnCount; col++)
            {
                double cellY = rowHeight + ((col & 1) == 1 ? cellHeight / 2 : 0);
                Polygon hex = GetCenteredHex(cellSize, cellX, cellY, cellHeight);
                hex.Tag = Tuple.Create(col, row, cellX, cellY);
                canvas.Children.Add(hex);
                cellX += 3 * cellSize / 2;
            }
        }

        private static Polygon GetCenteredHex(double cellSize, double cellX, double cellY, double cellHeight)
        {
            var hex = new Polygon();
            hex.Points.Add(new Point(cellX - cellSize, cellY));
            hex.Points.Add(new Point(cellX - cellSize / 2, cellY + cellHeight / 2));
            hex.Points.Add(new Point(cellX + cellSize / 2, cellY + cellHeight / 2));
            hex.Points.Add(new Point(cellX + cellSize, cellY));
            hex.Points.Add(new Point(cellX + cellSize / 2, cellY - cellHeight / 2));
            hex.Points.Add(new Point(cellX - cellSize / 2, cellY - cellHeight / 2));
            return hex;
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            double combHeight, combWidth, cellSize;

            // Get sizes that will fit within our window
            GetCombSize(Main.ActualHeight, Main.ActualWidth, ColCount, RowCount, out cellSize, out combHeight,
                        out combWidth);

            // Set the canvas size appropriately
            HoneycombCanvas.Width = combWidth;
            HoneycombCanvas.Height = combHeight;

            // Add the cells to the canvas
            AddCells(HoneycombCanvas, cellSize, ColCount, RowCount);

            // Set the cells to look like we want them
            SetHexProperties(HoneycombCanvas.Children, cellSize);
        }
    }
}
```

XAML:

```xml
<Window x:Class="Honeycombs.MainWindow"
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Title="Honeycomb" Height="400" Width="300" Loaded="Window_Loaded" ResizeMode="NoResize">
    <Grid x:Name="Main" Margin="5,5,5,0">
        <Grid.RowDefinitions>
            <RowDefinition/>
            <RowDefinition Height="69.6"/>
        </Grid.RowDefinitions>
        <TextBlock x:Name="Letters" HorizontalAlignment="Center" TextWrapping="Wrap" Grid.Row="1" VerticalAlignment="Center" FontSize="20"/>
        <Canvas x:Name="HoneycombCanvas" HorizontalAlignment="Center" VerticalAlignment="Center"/>
    </Grid>
</Window>
```

[[File:CSharpHoneycomb.jpg]]


## Go

{{libheader|raylib-go}}

```go
package main

import (
    rl "github.com/gen2brain/raylib-go/raylib"
    "math"
    "strings"
)

type hexagon struct {
    x, y     float32
    letter   rune
    selected bool
}

func (h hexagon) points(r float32) []rl.Vector2 {
    res := make([]rl.Vector2, 7)
    for i := 0; i < 7; i++ {
        fi := float64(i)
        res[i].X = h.x + r*float32(math.Cos(math.Pi*fi/3))
        res[i].Y = h.y + r*float32(math.Sin(math.Pi*fi/3))
    }
    return res
}

func inHexagon(pts []rl.Vector2, pt rl.Vector2) bool {
    rec := rl.NewRectangle(pts[4].X, pts[4].Y, pts[5].X-pts[4].X, pts[2].Y-pts[4].Y)
    if rl.CheckCollisionPointRec(pt, rec) {
        return true
    }
    if rl.CheckCollisionPointTriangle(pt, pts[2], pts[3], pts[4]) {
        return true
    }
    if rl.CheckCollisionPointTriangle(pt, pts[0], pts[1], pts[5]) {
        return true
    }
    return false
}

func main() {
    screenWidth := int32(600)
    screenHeight := int32(600)
    rl.InitWindow(screenWidth, screenHeight, "Honeycombs")
    rl.SetTargetFPS(60)

    letters := "LRDGITPFBVOKANUYCESM"
    runes := []rune(letters)
    var combs [20]hexagon
    var pts [20][]rl.Vector2

    x1, y1 := 150, 100
    x2, y2 := 225, 143
    w, h := 150, 87
    r := float32(w / 3)
    for i := 0; i < 20; i++ {
        var x, y int
        if i < 12 {
            x = x1 + (i%3)*w
            y = y1 + (i/3)*h
        } else {
            x = x2 + (i%2)*w
            y = y2 + (i-12)/2*h
        }
        combs[i] = hexagon{float32(x), float32(y), runes[i], false}
        pts[i] = combs[i].points(r)
    }

    nChosen := 0
    sChosen := "Chosen: "
    lChosen := "Last chosen: "

    for !rl.WindowShouldClose() {
        rl.BeginDrawing()
        rl.ClearBackground(rl.RayWhite)
        for i, c := range combs {
            ctr := pts[i][0]
            ctr.X -= r
            index := -1
            if key := rl.GetKeyPressed(); key != -1 {
                if key >= 97 && key <= 122 {
                    key -= 32
                }
                index = strings.IndexRune(letters, key)
            } else if rl.IsMouseButtonPressed(rl.MouseLeftButton) {
                pt := rl.Vector2{float32(rl.GetMouseX()), float32(rl.GetMouseY())}
                for i := 0; i < 20; i++ {
                    if inHexagon(pts[i], pt) {
                        index = i
                        break
                    }
                }
            }
            if index >= 0 {
                if !combs[index].selected {
                    combs[index].selected = true
                    nChosen++
                    s := string(combs[index].letter)
                    sChosen += s
                    lChosen = "Last chosen: " + s
                    if nChosen == 20 {
                        lChosen += " (All 20 Chosen!)"
                    }
                }
            }
            if !c.selected {
                rl.DrawPoly(ctr, 6, r-1, 30, rl.Yellow)
            } else {
                rl.DrawPoly(ctr, 6, r-1, 30, rl.Magenta)
            }
            rl.DrawText(string(c.letter), int32(c.x)-5, int32(c.y)-10, 32, rl.Black)
            rl.DrawPolyExLines(pts[i], 7, rl.Black)
            rl.DrawText(sChosen, 100, 525, 24, rl.Black)
            rl.DrawText(lChosen, 100, 565, 24, rl.Black)
        }
        rl.EndDrawing()
    }

    rl.CloseWindow()
}
```



## Haskell

{{libheader|gloss}}
{{libheader|random-shuffle}}

```Haskell
import Data.Char (toUpper)
import Data.Function (on)
import Data.List (zipWith4)
import System.Exit
import System.Random

-- External libraries.
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry
import Graphics.Gloss.Interface.IO.Game
import System.Random.Shuffle

-- A record of a hexagon-letter.
data Hex =
  Hex
  { hLetter   :: Char  -- The letter it holds.
  , hSelected :: Bool  -- The flag for if the hexagon has been selected.
  , hPath     :: Path  -- The hexagon's vertices.
  , hCenter   :: Point -- The center of the hexagon.
  }

-- A record of the world state.
data World =
  World
  { wHexes  :: [Hex]  -- The hexagons to interact with.
  , wString :: String -- An ordering of picked letters.
  }

-- Assorted helper functions.
addV, subV :: Vector -> Vector -> Vector
addV (a, b) (x, y) = (a + x, b + y)
subV (a, b) (x, y) = (a - x, b - y)

translateP :: Vector -> Path -> Path
translateP v = map (addV v)

translateV :: Vector -> Picture -> Picture
translateV (x, y) p = translate x y p

lightblue, darkblue :: Color
lightblue = makeColor 0.5 0.5 1.0 1.0
darkblue  = makeColor 0.0 0.0 0.5 1.0

-- Create vertices for an n-gon with the given radius to a vertex
ngon :: Int -> Float -> Path
ngon n radius =
  let angle = 2 * pi / fromIntegral n
  in map (mulSV radius . unitVectorAtAngle . (* angle) . fromIntegral)
     [0..(n - 1)]

-- Determine if a point lies on or within a polygon.
inPolygon :: Point -> Path -> Bool
inPolygon point path =
  all (>= 0) $ zipWith detV vas vbs
  where
    vas = zipWith subV (drop 1 $ cycle path) path
    vbs = map (subV point) path

-- Construct all of the hexagons transformed to their screen coordinates
-- to make mouse picking easier to solve.
mkHexes :: RandomGen g => g -> Float -> World
mkHexes gen radius = World hexes ""
  where
    letters = take 20 $ shuffle' ['A'..'Z'] 26 gen
    xs      = concatMap (replicate 4) [-2..2]
    ys      = cycle [-2..1]
    inRad   = radius * (cos $ degToRad 30)
    yOff x  = if ((floor x) :: Int) `mod` 2 == 0 then inRad else 0
    yStep   = inRad * 2
    xStep   = radius * 1.5
    centers = zipWith (\x y -> (x * xStep, yOff x + y * yStep)) xs ys
    paths   = map (flip translateP $ ngon 6 radius) centers
    hexes   = zipWith4 Hex letters (repeat False) paths centers

-- Draw a single hexagon-letter.
drawHex :: Hex -> Picture
drawHex (Hex letter selected path center) =
  pictures [hex, outline, letterPic]
  where
    hex              = color hcolor $ polygon path
    outline          = color blue $ lineLoop path
    letterPic        = color lcolor
                       $ translateV (addV (-10, -10) center)
                       $ scale 0.25 0.25
                       $ text [letter]
    (hcolor, lcolor) = if selected
                       then (darkblue, white)
                       else (lightblue, black)

-- Draw the whole scene.
drawWorld :: World -> Picture
drawWorld (World hexes string) =
  pictures [pictures $ map drawHex hexes
           ,pictures $ map drawHighHex hexes
           ,color (light lightblue) $ textPic
           ,scale 1.05 1.05 $ textPic]
  where
    drawHighHex hex = color black $ scale 1.05 1.05 $ lineLoop $ hPath hex
    textPic = translateV (-130, -175) $ scale 0.15 0.15 $ text string

-- Handle keyboard and mouse events and update the hexagons
-- accordingly. This function checks the hexagon states and
-- invokes a system exit when all are marked selected.
handleInput :: Event -> World -> IO World
handleInput event world@(World hexes string) =
  case event of
    EventKey key Down _ point ->
      case key of
        SpecialKey KeyEsc -> exitSuccess
        Char char         -> hCond (\hex -> hLetter hex == toUpper char)
        MouseButton _     -> hCond (\hex -> inPolygon point $ hPath hex)
        _                 -> return world
    _                         ->
      return world
  where
    checkWorld w = if all hSelected $ wHexes w then exitSuccess else return w
    hCond cond   = checkWorld $ World newHexes newString
      where
        newHexes  = flip map hexes
                   (\hex -> if cond hex
                            then hex {hSelected = True}
                            else hex)
        diff      = map fst
                    $ filter (uncurry ((/=) `on` hSelected))
                    $ zip hexes newHexes
        newString = case diff of
          []      -> string
          (hex:_) -> string ++ [hLetter hex]

main :: IO ()
main = do
  stdGen <- getStdGen
  playIO
    (InWindow "Honeycombs" (500, 500) (100, 100))
    white
    60
    (mkHexes stdGen 30)
    (return . drawWorld)
    handleInput
    (\_ x -> return x)
```


=={{header|Icon}} and {{header|Unicon}}==
[[File:HoneyComb unicon 4x5.PNG|thumb|right|4x5 Honeycomb in progress]]
The configuration of the honeycomb can be changed from the command line (height and width in cells as well as the length of the side of one cell).  Window dimensions are calculated.  The character set used to label cells expands for larger grids.  The completed grid is saved as a GIF.

There is no hexagonal widget in the Icon Graphics library so a custom widget was developed.  No attempt was made to make this widget like the VIB or VIB2 widgets.

The HexWidgetData record carries around alot of data about each widget including drawing coordinates, label, a routine to know if it's been selected and helper data, and coordinates for drawing neighboring cells (down and upper/lower right).

Label selection is straight forward.  Mouse selection first determines if x,y is within the widgets rectangular outer bounds.  The x,y point is then reflected into the north west quadrant of the cell and the helper data is used to calculate an abbreviated cross-product (x and y will always be 0).  The direction of the resultant z indicates if the point is inside or outside of the widgets inner bounds.

```Icon
link printf

procedure main(A)
   h := (0 < integer(\A[1])) | 4             # cells high
   w := (0 < integer(\A[2])) | 5             # cells wide
   u := (10 < integer(\A[3])) | 30           # length of cell side
   HoneyComb(h,w,u)
end

$define INACTIVE "light yellow"
$define ACTIVE   "light purple"

procedure HoneyComb(h,w,u)                   #: run HoneyComb demo

   wb := u/2                                 # window border
   wmsg := 10                                # . message space
   ww := 2*wb + u*(3*w+1)/2                  # . width
   wh := 2*wb+wmsg+integer((h+1)*u*sqrt(3.)) # . height

   chosen := sprintf("black,%d",wb)
   fine   := sprintf("black,%d",wmsg)

   wparms := [ title := sprintf("HoneyComb-%dx%d",h,w),
               "g","bg=white","fg=black",
               sprintf("size=%d,%d",ww,wh) ]
   &window := open!wparms | stop("Unable to open window")

   alpha := &ucase                           # per original spec
   if h*w > *alpha then alpha ++:= &lcase    # more
   if h*w > *alpha then alpha ++:= &digits   # more again
   if h*w > *alpha then
      stop("More than ",*alpha," cells.")    # choke

   every put(letters := [],!string(alpha))
   every !letters :=: ?letters               # randomize

   Widgets := []                             # prepare widgets
   every c := 1 to w do {                    # layout grid of cells
      if /top then                           # start at top left
         x := y := wb
      else {                                 # continue right from top
         x := top.rx
         y := if c % 2 = 0 then top.ry0 else top.ry1
         }
      put(Widgets,W := top := HexWidget(x,y,u,get(letters)))
      every 2 to h do                        # fill in rest of column
         put(Widgets,W := HexWidget(x := W.dx,y := W.dy,u,get(letters)))
      }

   activated := ""
   until *activated = *Widgets do {          # process widgets
      e := Event()
      every W := !Widgets do                          # select widget by
         if ((e == &lpress) & W.inside(W,&x,&y)) |    # mouse (left press) or
            (e == W.s) then                           # label character
               if not find(W.s,activated) then        # activate if new
                  break activated ||:= ( DrawCell(W,ACTIVE), W.s)
      Font(chosen)
      DrawString(wb,wh-wb-wmsg,"Chosen: "||activated) # update selected list
      }
   WriteImage(sprintf("%s-%d.gif",title,&now))        # save file
   Font(fine)                                         # tell how to quit
   DrawString(wb,wh-wmsg,"Right click to exit")
   until Event() == &rpress
   close(&window)
end

record HexWidgetData(s,u,w,h,ax,ay,cx,cy,poly,xx,xy,dx,dy,rx,ry0,ry1,inside)

procedure HexWidget(ax,ay,u,s)               #: create widget s @ x,y & side u
/u := 20.                                    # side
x := integer(0 <= ax) | runerr(205,ax)       # ensure whole numbers
y := integer(0 <= ay) | runerr(205,ay)
u := integer(1 <= u)  | runerr(205,u)        # 1 is minimal if ridiculous
h := integer(sqrt(3./4) * (w := 2 * u))      # h,w
W := HexWidgetData(s,u,w,h,                  # string, side, width and height
                  ax,ay,                     # absolute x,y
                  ax+w/2,ay+h/2,             # center x,y
                  [ax+u/2,ay, ax+(3*u)/2,ay, ax+2*u,ay+h/2,
                   ax+(3*u)/2,ay+h, ax+u/2,ay+h, ax,ay+h/2],  # to draw polygon
                   -u/2,h/2,                 # const for z of cross product
                   x,ay+h,                   # next cell down
                   ax+(3*u)/2,ay+h/2,ay-h/2, # next cells right up/down
                   InHexWidget)              # is it activated proc
return DrawCell(W,INACTIVE)
end

procedure DrawCell(W,colour)                 #: Draw the (general) Widget
   Fg(colour)
   FillPolygon!W.poly                        # can draw any polygon
   Fg("black")
   DrawPolygon!W.poly
   Font(sprintf("Helvetica,%d",integer(W.h/2.)))
   DrawString(W.cx - TextWidth(W.s)/2,
               W.cy + (WAttrib("ascent") - WAttrib("descent"))/2 + 1,W.s)
   return W
end

procedure InHexWidget(W,x,y)                 #: return W if x,y are inside W
   if W.w < 0 then W.ax -:= (W.w := -W.w)                       # fix if -w
   if W.h < 0 then W.ay -:= (W.h := -W.h)                       # fix if -h
   if (0 < x - W.ax < W.w) & (0 < y - W.ay < W.h) then {        # disallow edge
      if x > W.cx then x := W.cx - (x - W.cx)                   # reflect x->NW
      if y > W.cy then y := W.cy - (y - W.cy)                   # reflect y->NW
      if 0 > real(W.xx)*(y-W.poly[2]) - W.xy*(x-W.poly[1]) then # z from cross
         return W
      }
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]


## Java


```java
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class Honeycombs extends JFrame {

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new Honeycombs();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setVisible(true);
        });
    }

    public Honeycombs() {
        add(new HoneycombsPanel(), BorderLayout.CENTER);
        setTitle("Honeycombs");
        setResizable(false);
        pack();
        setLocationRelativeTo(null);
    }
}

class HoneycombsPanel extends JPanel {

    Hexagon[] comb;

    public HoneycombsPanel() {
        setPreferredSize(new Dimension(600, 500));
        setBackground(Color.white);
        setFocusable(true);

        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent e) {
                for (Hexagon hex : comb)
                    if (hex.contains(e.getX(), e.getY())) {
                        hex.setSelected();
                        break;
                    }
                repaint();
            }
        });

        addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                for (Hexagon hex : comb)
                    if (hex.letter == Character.toUpperCase(e.getKeyChar())) {
                        hex.setSelected();
                        break;
                    }
                repaint();
            }
        });

        char[] letters = "LRDGITPFBVOKANUYCESM".toCharArray();
        comb = new Hexagon[20];

        int x1 = 150, y1 = 100, x2 = 225, y2 = 143, w = 150, h = 87;
        for (int i = 0; i < comb.length; i++) {
            int x, y;
            if (i < 12) {
                x = x1 + (i % 3) * w;
                y = y1 + (i / 3) * h;
            } else {
                x = x2 + (i % 2) * w;
                y = y2 + ((i - 12) / 2) * h;
            }
            comb[i] = new Hexagon(x, y, w / 3, letters[i]);
        }

        requestFocus();
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        g.setFont(new Font("SansSerif", Font.BOLD, 30));
        g.setStroke(new BasicStroke(3));

        for (Hexagon hex : comb)
            hex.draw(g);
    }
}

class Hexagon extends Polygon {
    final Color baseColor = Color.yellow;
    final Color selectedColor = Color.magenta;
    final char letter;

    private boolean hasBeenSelected;

    Hexagon(int x, int y, int halfWidth, char c) {
        letter = c;
        for (int i = 0; i < 6; i++)
            addPoint((int) (x + halfWidth * Math.cos(i * Math.PI / 3)),
                     (int) (y + halfWidth * Math.sin(i * Math.PI / 3)));
        getBounds();
    }

    void setSelected() {
        hasBeenSelected = true;
    }

    void draw(Graphics2D g) {
        g.setColor(hasBeenSelected ? selectedColor : baseColor);
        g.fillPolygon(this);

        g.setColor(Color.black);
        g.drawPolygon(this);

        g.setColor(hasBeenSelected ? Color.black : Color.red);
        drawCenteredString(g, String.valueOf(letter));
    }

    void drawCenteredString(Graphics2D g, String s) {
        FontMetrics fm = g.getFontMetrics();
        int asc = fm.getAscent();
        int dec = fm.getDescent();

        int x = bounds.x + (bounds.width - fm.stringWidth(s)) / 2;
        int y = bounds.y + (asc + (bounds.height - (asc + dec)) / 2);

        g.drawString(s, x, y);
    }
}
```


## Julia

Uses Cairo and Gtk for graphics. Tasks done include the optional one of recording and then displaying letters as chosen with mouse or keyboard on exit once all letters are chosen.

```julia
using Gtk.ShortNames, GtkReactive, Graphics, Cairo, Colors

mutable struct Hexagon
    center::Point
    radius::Int
    letter::String
    color::Colorant
end

const offset = 50
const hgt = 450
const wid = 400
const hcombdim = (rows = 5, cols = 4)
const randletters = reshape(string.(Char.(shuffle(UInt8('A'):UInt8('Z'))))[1:20], Tuple(hcombdim))
const win = Window("Honeycombs", wid, hgt)
const can = Canvas()
const honeycomb = Dict{Point, Hexagon}()
const chosen = Vector{String}()

function hexmat(p, rad)
    shor = rad * 0.5
    long = rad * sqrt(3.0) / 2.0
    mat = reshape([shor, long, -shor, long, Float64(-rad), 0.0, -shor, -long, shor, -long, Float64(rad), 0.0], 2, 6)
    [Point(mat[1, n] + p.x, mat[2, n] + p.y) for n in 1:6]
end

function whichclicked(clickpos)
    centers = [c for c in keys(honeycomb)]
    (maybeclicked, idx) = findmin(map(c -> sqrt((clickpos.x - c.x)^2 + (clickpos.y - c.y)^2), centers))
    return maybeclicked < offset * sqrt(3) / 2.0 ? centers[idx] : nothing
end

whichtyped(ch) = (for (k, v) in honeycomb if v.letter == ch return k end end; nothing)

function hexagon(ctx, pos, rad, ltr, colr = colorant"yellow")
    set_source(ctx, colr)
    points = hexmat(pos, rad)
    set_line_width(ctx, 4)
    polygon(ctx, points)
    close_path(ctx)
    fill(ctx)
    set_source(ctx, colorant"black")
    polygon(ctx, points)
    close_path(ctx)
    stroke(ctx)
    move_to(ctx, pos.x - (ltr == "I" ? 7 : 18), pos.y + 15)
    set_source(ctx, colr == colorant"yellow" ? colorant"red" : colorant"black")
    set_font_size(ctx, offset)
    show_text(ctx, ltr)
    Hexagon(pos, rad, ltr, colr)
end

hexagon(ctx, h::Hexagon) = hexagon(ctx, h.center, h.radius, h.letter, h.color)

function makehoneycomb(ctx)
    centers = fill(Point(0, 0), hcombdim.rows, hcombdim.cols)
    xdelta = 75.0
    ydelta = 90.0
    for i in 1:hcombdim.rows, j in 1:hcombdim.cols
        center = Point((i - 1) * xdelta + offset, (j - 1) * ydelta + ((i - 1 ) % 2 + 1) * offset)
        centers[i, j] = center
        honeycomb[center] = hexagon(ctx, center, offset, randletters[i, j])
    end
    centers
end

@guarded draw(can) do widget
    ctx = getgc(can)
    if length(honeycomb) == 0
        makehoneycomb(ctx)
    else
        map(c -> hexagon(ctx, honeycomb[c]), collect(keys(honeycomb)))
    end
end

""" At entry to this function we have just found out what letter was chosen."""
function changecolor(colr)
    h = honeycomb[colr]
    h.color = colorant"violet"
    hexagon(getgc(can), h)
    reveal(win, true)
    push!(chosen, h.letter)
    if all(map(k -> honeycomb[k].color == colorant"violet", collect(keys(honeycomb))))
        println("All hexagons ($chosen, and the last letter was $(chosen[end])) have been chosen. Exiting.")
        exit(0)
    end
end

signal_connect(win, "key-press-event") do widget, event
    if (whichhexgon = whichtyped(string(uppercase(Char(event.keyval))))) != nothing
        changecolor(whichhexgon)
    end
end

can.mouse.button1press = @guarded (widget, event) -> begin
    if (whichhexgon = whichclicked(Point(event.x, event.y))) != nothing
        changecolor(whichhexgon)
    end
end

push!(win, can)
show(can)
condition = Condition()
endit(w) = notify(condition)
signal_connect(endit, win, :destroy)
show(win)
wait(condition)
exit()


```



## Kotlin

This is a translation of the Java entry except that code has been added to end the program automatically when all the hexagons have been selected.

```scala
// version 1.1.4

import java.awt.BasicStroke
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Polygon
import java.awt.RenderingHints
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.event.WindowEvent
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.SwingUtilities

class Honeycombs : JPanel() {
    private val comb: Array<Hexagon?> = arrayOfNulls(20)

    init {
        preferredSize = Dimension(600, 500)
        background = Color.white
        isFocusable = true

        addMouseListener(object : MouseAdapter() {
            override fun mousePressed(e: MouseEvent) {
                for (hex in comb)
                    if (hex!!.contains(e.x, e.y)) {
                        hex.setSelected()
                        checkForClosure()
                        break
                    }
                repaint()
            }
        })

        addKeyListener(object : KeyAdapter() {
            override fun keyPressed(e: KeyEvent) {
                for (hex in comb)
                    if (hex!!.letter == e.keyChar.toUpperCase()) {
                        hex.setSelected()
                        checkForClosure()
                        break
                    }
                repaint()
            }
        })

        val letters = "LRDGITPFBVOKANUYCESM".toCharArray()
        val x1 = 150
        val y1 = 100
        val x2 = 225
        val y2 = 143
        val w = 150
        val h = 87

        for (i in 0 until comb.size) {
            var x: Int
            var y: Int
            if (i < 12) {
                x = x1 + (i % 3) * w
                y = y1 + (i / 3) * h
            }
            else {
                x = x2 + (i % 2) * w
                y = y2 + ((i - 12) / 2) * h
            }
            comb[i] = Hexagon(x, y, w / 3, letters[i])
        }

        requestFocus()
    }

    override fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON)
        g.font = Font("SansSerif", Font.BOLD, 30)
        g.stroke = BasicStroke(3.0f)
        for (hex in comb) hex!!.draw(g)
    }

    private fun checkForClosure() {
        if (comb.all { it!!.hasBeenSelected } ) {
            val f = SwingUtilities.getWindowAncestor(this) as JFrame
            f.dispatchEvent(WindowEvent(f, WindowEvent.WINDOW_CLOSING))
        }
    }
}

class Hexagon(x: Int, y: Int, halfWidth: Int, c: Char) : Polygon() {
    private val baseColor = Color.yellow
    private val selectedColor = Color.magenta
    var hasBeenSelected = false
    val letter = c

    init {
        for (i in 0..5)
            addPoint((x + halfWidth * Math.cos(i * Math.PI / 3.0)).toInt(),
                     (y + halfWidth * Math.sin(i * Math.PI / 3.0)).toInt())
        getBounds()
    }

    fun setSelected() {
        hasBeenSelected = true
    }

    fun draw(g: Graphics2D) {
        with(g) {
            color = if (hasBeenSelected) selectedColor else baseColor
            fillPolygon(this@Hexagon)
            color = Color.black
            drawPolygon(this@Hexagon)
            color = if (hasBeenSelected) Color.black else Color.red
            drawCenteredString(g, letter.toString())
        }
    }

    private fun drawCenteredString(g: Graphics2D, s: String) {
        val fm = g.fontMetrics
        val asc = fm.ascent
        val dec = fm.descent
        val x = bounds.x + (bounds.width - fm.stringWidth(s)) / 2
        val y = bounds.y + (asc + (bounds.height - (asc + dec)) / 2)
        g.drawString(s, x, y)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        with(f) {
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            add(Honeycombs(), BorderLayout.CENTER)
            title = "Honeycombs"
            isResizable = false
            pack()
            setLocationRelativeTo(null)
            isVisible = true
        }
    }
}
```



## Liberty BASIC

By Andy Amaya, Sept. 24, 2015 -- with thanks from the Liberty BASIC Community.

```lb

NoMainWin
Dim hxc(20,2), ltr(26)
Global sw, sh, radius, radChk, mx, my, h$, last
h$="#g": radius = 40: radChk = 35 * 35: last = 0
sw = 400: sh = 380: WindowWidth = sw+6: WindowHeight= sh+32
Open "Liberty BASIC - Honeycombs" For graphics_nsb_nf As #g
#g "Down; Cls; TrapClose xit"

Call shuffle
Call grid 75, 15, "0 0 0", "255 215 32", "0 0 0"

#g "SetFocus; when characterInput getKey; when leftButtonDown chkClick"
Wait

Sub xit h$
    Close #h$:End
End Sub

'Assign ASCII values of A thru Z to ltr() array and randomize order of letters
Sub shuffle
    For i = 1 To 26
        ltr(i) = i+64
    Next
    For i = 1 To 77
        r1 = Int(Rnd(1)*26)+1
        r2 = Int(Rnd(1)*26)+1
        temp = ltr(r1): ltr(r1) = ltr(r2): ltr(r2) = temp
    Next
End Sub

'Draw the hex cells and fill with 20 out of 26 random letters
Sub grid ox, oy, fc$, bc$, tc$
    cx = ox: cy = oy
    For i = 1 To 5
        If (i And 1)=0 Then cy = oy + 76 Else cy = oy + 42
        For j = 1 To 4
            count = count + 1: letter$ = Chr$(ltr(count))
            Call cell, cx, cy, fc$, bc$, tc$, letter$
            hxc(count,0)=cx: hxc(count,1)=cy: cy = cy + 70
        Next
        cx = cx + 61
    Next
End Sub

'Draw a filled hex cell and printed the letter associated with cell
Sub cell cx, cy, fc$, bc$, tc$, lt$
    lastx = cx + radius: lasty = cy
    For f = 1.04719755 To 6.2831853 Step 1.0471955
        nx = Cos(f)*radius+cx: ny = Sin(f)*radius+cy
        #g "Size 2; Color ";bc$;";BackColor ";bc$
        Call triFill cx, cy, lastx, lasty, nx, ny
        #g "Size 5; Color ";fc$
        #g "Line ";lastx;" ";lasty;" ";nx;" ";ny;";Size 1"
        lastx = nx: lasty = ny
    Next
    #g "Font Courier_New 36 Bold"
    #g "Color ";tc$;";BackColor ";bc$
    #g "Place ";cx-15;" ";cy+15;";\";lt$
End Sub

'Check for a mouse click in a hex cell
Sub chkClick h$, x, y
    mx = MouseX
    my = MouseY
    For i = 1 To 20
        If pnc(mx,my,hxc(i,0),hxc(i,1)) = 1 Then 'selected hex cell found
            If hxc(i,2)=0 Then
                hxc(i,2)=1 'when set to 1, hex cell & letter no longer selectable
                key$ = Chr$(ltr(i))
                Call cell hxc(i,0),hxc(i,1),"0 0 0","80 0 128","255 255 255",key$
                Call showLetter key$
                Exit For
            End If
        End If
    Next
End Sub

'Allow letter selection via keyboard
Sub getKey h$, char$
    key$ = Upper$(Inkey$)
    'Poll ESC key to exit at any time
    If key$=Chr$(27) Then Call xit h$
    idx = Instr("ABCDEFGHIJKLMNOPQRSTUVWXYZ",key$)
    If idx <> 0 Then
        For i = 1 To 20
            If idx+64 = ltr(i) Then 'letter matching key press found
                If hxc(i,2)=0 Then
                    hxc(i,2)=1 'when set to 1, hex cell & letter no longer selectable
                    Call cell hxc(i,0),hxc(i,1),"0 0 0","80 0 128","255 255 255",key$
                    Call showLetter key$
                    Exit For
                End If
            End If
        Next
    End If
End Sub

'Print letters selected at bottom of screen
Sub showLetter key$
    #g "Font Courier_New 18 Bold"
    #g "Color Black;BackColor white"
    #g "Place ";last*18+20;" 365;\"; key$
    last = last + 1
    'When 20th letter selected; exit
    If last > 19 Then Call xit h$
End Sub

'Draw a filled triangle
Sub triFill x1,y1, x2,y2, x3,y3
    If x2<x1 Then x=x2: y=y2: x2=x1: y2=y1: x1=x: y1=y
    If x3<x1 Then x=x3: y=y3: x3=x1: y3=y1: x1=x: y1=y
    If x3<x2 Then x=x3: y=y3: x3=x2: y3=y2: x2=x: y2=y
    If x1<>x3 Then slope1=(y3-y1)/(x3-x1)
    length=x2-x1
    If length<>0 Then
        slope2=(y2-y1)/(x2-x1)
        For x = 0 To length
            #g "Line ";Int(x+x1);" ";Int(x*slope1+y1);" ";Int(x+x1);" ";Int(x*slope2+y1)
        Next
    End If
    y = length*slope1+y1 :length=x3-x2
    If length<>0 Then
        slope3=(y3-y2)/(x3-x2)
        For x = 0 To length
            #g "Line ";Int(x+x2);" ";Int(x*slope1+y);" ";Int(x+x2);" ";Int(x*slope3+y2)
        Next
    End If
End Sub

'Point in Circle function
Function pnc(ax, ay, bx, by)
    If (bx-ax)*(bx-ax)+(by-ay)*(by-ay) <= radChk Then
        pnc=1
    Else
        pnc=0
    End If
End Function

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Two players, 5 by 4.

```Mathematica
hexagon[{x_, y_}] :=
  Polygon[Transpose[{{1/2, 1/4, -1/4, -1/2, -1/4, 1/4} +
      x, {0, Sqrt[3]/4, Sqrt[3]/4, 0, -Sqrt[3]/4, -Sqrt[3]/4} + y}]];
off = Transpose[{ConstantArray[0, 20], {0, 0, 0, 0, Sqrt[3]/4,
     Sqrt[3]/4, Sqrt[3]/4, Sqrt[3]/4, 0, 0, 0, 0, Sqrt[3]/4,
     Sqrt[3]/4, Sqrt[3]/4, Sqrt[3]/4, 0, 0, 0, 0}}];
DynamicModule[{letters = RandomSample[CharacterRange["A", "Z"], 20],
  blue = False, cols = {},
  locs = Tuples[{Range[1, 4, 3/4],
      Range[1, 1 + (3 Sqrt[3])/2, Sqrt[3]/2]}] - off},
 EventHandler[
  Dynamic[Graphics[{EdgeForm[{Thick, Black}], LightGray,
     hexagon /@ locs, {#[[1]], hexagon[#[[2]]]} & /@ cols, Black,
     MapThread[
      Text, {Style[#, FontSize -> Large] & /@ letters, locs}], Red,
     Text[Style[
       StringJoin[
        letters[[FirstPosition[locs, #[[2]]][[1]]]] & /@
         Cases[cols, {Red, _}][[All, 2]]],
       FontSize -> 40], {5/2, -1/2}, {Right, Center}], Blue,
     Text[Style[
       StringJoin[
        letters[[FirstPosition[locs, #[[2]]][[1]]]] & /@
         Cases[cols, {Blue, _}][[All, 2]]],
       FontSize -> 40], {5/2, -1/2}, {Left, Center}]},
    PlotRange -> {{-1, 6}, Automatic},
    ImageSize -> Large]], {"MouseClicked" :>
    If[! MemberQ[cols[[All, 2]],
       Nearest[locs, MousePosition["Graphics"]][[1]]],
     AppendTo[
      cols, {If[blue, Blue, Red],
       Nearest[locs, MousePosition["Graphics"]][[1]]}];
     blue = ! blue]}]]
```



## MATLAB

Simple version
*Click-play, no entering letters
*One-player
*Prints choices list to figure title, no output
*Easy to change number of rows or columns
*No support for different numbers of hexagons in different columns
*Works for up to 26 hexagons before using lowercase labels, 52 hexagons before repeating letters
*Works off of mouse-click events, so program doesn't need to "quit" as it is not really running constantly

```MATLAB
function Honeycombs
    nRows = 4;                      % Number of rows
    nCols = 5;                      % Number of columns
    nHexs = nRows*nCols;            % Number of hexagons
    rOuter = 1;                     % Circumradius
    startX = 0;                     % x-coordinate of upper left hexagon
    startY = 0;                     % y-coordinate of upper left hexagon
    delX = rOuter*1.5;              % Horizontal distance between hexagons
    delY = rOuter*sqrt(3);          % Vertical distance between hexagons
    offY = delY/2;                  % Vertical offset between columns
    genHexX = rOuter.*cos(2.*pi.*(0:5).'./6);   % x-coords of general hexagon
    genHexY = rOuter.*sin(2.*pi.*(0:5).'./6);   % y-coords of general hexagon
    centX = zeros(1, nHexs);        % x-coords of hexagon centers
    centY = zeros(1, nHexs);        % y-coords of hexagon centers
    for c = 1:nCols
        idxs = (c-1)*nRows+1:c*nRows;   % Indeces of hexagons in that column
        if mod(c, 2)                % Odd numbered column - higher y-values
            centY(idxs) = startY:-delY:startY-delY*(nRows-1);
        else                        % Even numbered column - lower y-values
            centY(idxs) = startY-offY:-delY:startY-offY-delY*(nRows-1);
        end
        centX(idxs) = (startX+(c-1)*delX).*ones(1, nRows);
    end
    [MCentX, MGenHexX] = meshgrid(centX, genHexX);
    [MCentY, MGenHexY] = meshgrid(centY, genHexY);
    HexX = MCentX+MGenHexX;         % x-coords of hexagon vertices
    HexY = MCentY+MGenHexY;         % y-coords of hexagon vertices
    figure
    hold on
    letters = char([65:90 97:122]);
    randIdxs = randperm(26);
    letters = [letters(randIdxs) letters(26+randIdxs)];
    hexH = zeros(1, nHexs);
    for k = 1:nHexs                 % Create patches individually
        hexH(k) = patch(HexX(:, k), HexY(:, k), [1 1 0]);
        textH = text(centX(k), centY(k), letters(mod(k, length(letters))), ...
            'HorizontalAlignment', 'center', 'FontSize', 14, ...
            'FontWeight', 'bold', 'Color', [1 0 0], 'HitTest', 'off');
        set(hexH(k), 'UserData', textH) % Save to object for easy access
    end
    axis equal
    axis off
    set(gca, 'UserData', '')        % List of clicked patch labels
    set(hexH, 'ButtonDownFcn', @onClick)
end

function onClick(obj, event)
    axesH = get(obj, 'Parent');
    textH = get(obj, 'UserData');
    set(obj, 'FaceColor', [1 0 1])                      % Change color
    set(textH, 'Color', [0 0 0])                        % Change label color
    set(obj, 'HitTest', 'off')                          % Ignore future clicks
    currList = get(axesH, 'UserData');                  % Hexs already clicked
    newList = [currList get(textH, 'String')];          % Update list
    set(axesH, 'UserData', newList)
    title(newList)
end
```



## Perl


The programme uses the ''Tk'' GUI toolkit.


```perl
#!/usr/bin/perl
use warnings;
use strict;

use Tk;
use List::Util qw(shuffle);


sub altitude {
    sqrt(3/4) * shift;
}


sub polygon_coordinates {
    my ($x, $y, $size) = @_;
    my $alt = altitude($size);
    return ($x - $size,       $y,
            $x - ($size / 2), $y - $alt,
            $x + ($size / 2), $y - $alt,
            $x + $size,       $y,
            $x + ($size / 2), $y + $alt,
            $x - ($size / 2), $y + $alt,
           );
}


{   my %changed;
    sub change {
        my ($canvas, $id, $letter_id) = @_;
        return sub {
            $canvas->itemconfigure($id,        -fill => 'magenta');
            $canvas->itemconfigure($letter_id, -fill => 'black');
            undef $changed{$id};

            if (20 == keys %changed) {
                print "All letters pressed.\n";
                # Simple exit causes a "Font still in cache" segfault
                # when the last letter is changed with a mouse button.
                $canvas->MainWindow->after(10, sub { exit });
            }
        }
    }
}


{   my @letters = (shuffle('A' .. 'Z'))[1 .. 20];
    sub comb {
        my ($canvas, $fromx, $fromy, $size, $count) = @_;
        for (my $x = $fromx; $x < 3 * $count * $size; $x += 3 * $size) {
            for (my $y = $fromy; $y < 7.5 * $size; $y += 2 * altitude($size)) {
                my $id = $canvas->createPolygon(
                                      polygon_coordinates($x, $y, $size),
                                      -outline => 'black',
                                      -fill    => 'yellow',
                                      -width   => 2,
                                  );
                my $letter = shift @letters;
                my $letter_id = $canvas->createText($x, $y,
                                         -fill => 'red',
                                         -text => $letter,
                                         -font => "{sans} " . ($size * 0.9),
                                     );
                $canvas->MainWindow->bind('all', lc $letter,
                                          change($canvas, $id, $letter_id));
                $canvas->bind($_, '<Button-1>',
                              change($canvas, $id, $letter_id))
                    for $id, $letter_id;
            }
        }
    }
}


my $size = 36;

my $mw     = 'MainWindow'->new(-title => "Honeycombs");
my $canvas = $mw->Canvas(-width  => 8 * $size,
                         -height => 8 * $size,
                        )->pack;

comb($canvas, $size,       $size,                   $size, 3);
comb($canvas, $size * 2.5, $size + altitude($size), $size, 2);


my $btn = $mw->Button(-text      => 'Quit',
                      -underline => 0,
                      -command   => sub { exit },
                     )->pack;
$mw->bind('<Alt-q>', sub { $btn->invoke });
MainLoop();
```



## Phix

Resizable, with automatic font and line width scaling. Selection by mouse or keyboard. Constants for easy modification.

In the 2 player game, the string chosen contains the selections of both players: odd chars = player 1, even chars = player 2.

Included in the distribution as demo\rosetta\honeycomb.exw

```Phix
include ..\arwen\arwen.ew
include ..\arwen\axtra.ew

constant N = 5, -- columns
         M = 4, -- rows
         cLetter = Yellow,  -- initial colour(!)
         cChosen = Purple,
         cPlayr2 = Purple,  -- (2 player if!=c_Chosen)
         cHover  = White,
         cLines  = Black,
         cSelect = Black,   -- (text/list of selected letters)
         cBackgnd = #EFF8FA,
         letters = shuffle("ABCDEFGHIJKLMNOPQRSTUVWXYZ")[1..N*M]
sequence fonts  -- list of {width,handle}, see set_font()
string chosen = ""

constant main = create(Window, "honeycomb", 0, 0, 20, 20, 520, 540, 0),
         mainDC = getPrivateDC(main),
         viewDC = c_func(xCreateCompatibleDC, {NULL}),
         pSize = allocate_Point()

integer ls,         -- length of a single side
        dx, dy,     -- bounding rectangle of a sloping side
        ox, oy,     -- offsets needed to center things
        lw          -- line width (10% of ls, tweaked)

-- The total bounding rectange of a completed N by M honeycomb is N*(ls+dx)+dx by (2*M+1)*dy.
--  However, as space for the chosen letters, pretend there is an extra row at the bottom.
-- Use that to determine the best ls, and hence dx and dy, as the window is resized.

constant cos60 = cos(2*PI*60/360),      -- dx = ls*cos60 (cos60=0.5)
         sin60 = sin(2*PI*60/360)       -- dy = ls*sin60

function font_info(integer size)
atom hFont = createFontForDC(viewDC, "Calibri", size, Bold)
    {} = selectObject(viewDC,hFont)
    {} = c_func(xGetTextExtentPoint32,{viewDC,"W",1,pSize})
    return {peek4u(pSize),hFont}
end function
fonts = {font_info(1)}

procedure set_font(atom ls)
    while length(fonts)<=200    -- (arbitrary limit)
      and fonts[$][1]<ls do     -- until slightly too big
        fonts = append(fonts,font_info(length(fonts)+1))
    end while
    for i=length(fonts) to 1 by -1 do
        if fonts[i][1]<=ls then
            {} = selectObject(viewDC,fonts[i][2])
            exit
        end if
    end for
end procedure

integer mx=0, my=0  -- mouse hover cell

procedure drawHexagon(integer x, integer y)
integer k = (y-1)*N+x
integer ch = letters[k]
string s = letters[k..k]
sequence points
    -- calculate position of left mid:
    atom x0 = (x-1)*(ls+dx) + ox + 5
    atom y0 = (2*y-mod(x,2))*dy + oy + 10
    -- and 3 more x co-ords, and above/below y0:
    atom x1 = x0+dx, x2 = x1+ls, x3 = x2+dx,
         ya = y0-dy, yb = y0+dy
    -- points are {{left,top},top,right,btm,btm,home(left)}
    points = {{x0,y0,x1,ya},{x2,ya},{x3,y0},{x2,yb},{x1,yb},{x0,y0}}
    k = find(ch,chosen)
    setPenColor(iff(k?iff(mod(k,2)?cChosen:cPlayr2):iff(x=mx and y=my?cHover:cLetter)))
    drawPolygonh(viewDC,points)
    setPenColor(cLines)
    drawLinesh(viewDC,points)
    {} = c_func(xGetTextExtentPoint32,{viewDC,s,1,pSize})
    x0 += dx+ls/2-peek4u(pSize)/2 -- centre-width/2
    y0 -= peek4u(pSize+4)/2       -- (centre)-height/2
    wPuts2(viewDC,x0,y0,s)
end procedure

function xy_to_hex(sequence xy)
integer x, y, gx, gy, rx, ry, hx=0, hy=0

    if dx!=0 and dy!=0 then -- (avoid divide by 0)
        x = xy[1]-ox-5
        y = xy[2]-oy-10+2*dy

        -- Credit: Matt Lewis (hexes.exw)
        -- (but I basically tilted it on its head
        --  and tweaked it using trial and error;
        --  see commented out loop in WM_PAINT.)
        gx = floor(x/dx)
        gy = floor((y-dy)/dy)
        rx = remainder(x,dx)
        ry = remainder(y,dy)

        hx = floor(gx/3+0.7)
        if remainder(gx,3)<1 then
            atom mxb = (dx/dy)*ry
            -- need to check the slope
            if remainder(hx,2)!=remainder(gy,2) then
                mxb = dx-mxb
            end if
            hx += (rx>mxb)
        end if
        hy = floor((gy+remainder(hx,2))/2)
    end if
    return {hx,hy}
end function

integer dw = 0, dh = 0      -- client area width and height
atom bmView
integer vwX = 0, vwY = 0    -- actual size of the view bitmap

function mainHandler(integer id, integer msg, atom wParam, object lParam)
integer ch

    if msg=WM_SIZE then
        {{},{},dw,dh} = getClientRect(main)
        if dw>vwX or dh>vwY then
            -- we need a bigger bitmap
            bmView = c_func(xCreateCompatibleBitmap, {mainDC, dw, dh})
            {} = deleteObject(selectObject(viewDC,bmView))
            {vwX,vwY} = {dw,dh}
        end if
        --  width = N*(ls+dx)+dx = ls*(N*(1+cos60)+cos60),
        --  height = (2*M+3)*dy = ls*(2*M+3)*sin60, pick whichever fits:
        ls = min(floor((dw-10)/(N*(1+cos60)+cos60)),
                 floor((dh-20)/((2*M+3)*sin60)))
        dx = floor(ls*cos60)    -- (same as ls/2)
        dy = floor(ls*sin60)
        ox = floor((dw-((N*(ls+dx))+dx))/2)
        oy = floor((dh-((2*M+3)*dy))/2)
        lw = floor((ls-10)/10)+1
        setPenWidth(lw)
        set_font(ls)
    elsif msg=WM_PAINT then
        setPenColor(cBackgnd)
        drawRectangleh(viewDC, True, 0, 0, dw, dh)
        for x=1 to N do
            for y=1 to M do
                drawHexagon(x,y)
            end for
        end for
        -- text/list of selected letters goes where (M+2)th row would:
        setPenColor(cSelect)
        wPuts2(viewDC,ox+dx+5,oy+(2*M+1)*dy+10,chosen)
        -- I needed this to get xy_to_hex() working:
--      for i=1 to 400 do
--          for j=1 to 400 do
--              if xy_to_hex({i,j})={1,1} then
--                  drawRectangleh(viewDC, True, i, j, i+1, j+1)
--              end if
--          end for
--      end for
        void = c_func(xBitBlt,{mainDC,0,0,dw,dh,viewDC,0,0,SRCCOPY})
    elsif msg=WM_CHAR then
        if wParam=VK_ESCAPE then
            closeWindow(main)
            if id then end if -- suppress warnings
--      elsif wParam='!' then
--          ?9/0
        else
            ch = upper(wParam)
            if find(ch,letters) and not find(ch,chosen) then
                chosen &= ch
                repaintWindow(main)
            end if
        end if
    elsif msg=WM_MOUSEMOVE then
        {mx,my} = xy_to_hex(lParam)
        repaintWindow(main)
    elsif msg = WM_LBUTTONDOWN then
        {mx,my} = xy_to_hex(lParam)
        if mx>=1 and mx<=N
        and my>=1 and my<=M then
            ch = letters[(my-1)*N+mx]
            if find(ch,letters) and not find(ch,chosen) then
                chosen &= ch
                repaintWindow(main)
            end if
        end if
    elsif msg=WM_GETMINMAXINFO then
        -- below this, things stop working...
        poke4(lParam+MINMAXINFO_ptMinTrackSize,{188,250})
    end if
    return 0
end function
setHandler(main,routine_id("mainHandler"))

WinMain(main, SW_NORMAL)
```



## Prolog

Works with SWI-Prolog and XPCE.

```Prolog
honeycomb :-
	new(W, window('Honeycomb')),
	new(Counter, counter(20)),
        new(Ph, phrase(W, point(50,500))),
	send(W, recogniser, new(KB, key_binding(@nil, argument))),
	numlist(0, 19, NL),
	create_letters(20, [], LL),
	maplist(build_list(150,100), NL, LP),
	new(ChCell, chain),
	maplist(create_cell(W, Counter, Ph, KB, ChCell), LP, LL),
	send(W, size, size(600, 600)),
	% we must free the resources
	send(W, done_message, and(message(ChCell, for_all, message(@arg1, free)),
				  message(ChCell, free),
				  message(Counter, free),
				  message(Ph, free),
				  message(W, destroy))),
	send(W, open).


% create the link between the keyboard and the cell
init_key_binding(KB, Cell-UpperCase) :-
	downcase_atom(UpperCase, LowerCase),
	send(KB, function, UpperCase, message(Cell, click)),
	send(KB, function, LowerCase, message(Cell, click)).

create_letters(0, LL, LL) :- !.

create_letters(N, L1, LL) :-
	C is random(26) + 65,
	(   \+member(C, L1) ->
	    N1 is N-1, create_letters(N1, [C|L1], LL)
	;   create_letters(N, L1, LL)).

% creation of the cells
create_cell(W, Counter,Phrase, KB, ChCell, Point,  Code) :-
	char_code(Letter, Code),
	new(H, cell(W, Counter, Phrase, Letter, Point)),
	send(H, my_draw),
	send(ChCell, append, H),
	% create the link between the keyboard and the cell
	init_key_binding(KB, H-Letter).


% build the list of the centers of the cells
build_list(X0,Y0, N, point(X,Y)) :-
	C is N mod  5,
	L is N // 5,
	C0 is C mod 2,
	X is C * 75 + X0,
	Y is L * round(50 * sqrt(3)) + C0 * round(25 * sqrt(3)) + Y0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pce_begin_class(phrase, string, "spelled string").
variable(str, string, both, "displayed string").
variable(window, object, both, "the display" ).
variable(pt, point, both, "where to display strings").
variable(lbl1, label, both, "label to display the letters").
variable(lbl2, label, both, "label to display the last letter").

initialise(P, Window : object, Point : point) :->
	send(P, slot, window, Window),
	send(P, slot, str, new(_, string(''))),
	send(P, slot, pt, Point),
	new(Lbl1, label),
	send(Lbl1, font,  @times_bold_24),
	send(P, slot, lbl1, Lbl1),
	new(Lbl2, label),
	send(Lbl2, font,  @times_bold_24),
	send(P, slot, lbl2, Lbl2).

unlink(P) :->
	get(P, slot, lbl1, Lbl1),
	send(Lbl1, free),
	get(P, slot, lbl2, Lbl2),
	send(Lbl2, free),
	send(P, send_super, unlink).

% display the list of the letters
% and the last letter on the screen
new_letter(P, Letter) :->
	get(P, slot, str, Str),
	send(Str, append, Letter),
	send(P, slot, str, Str),
	new(S1, string('Chosen : %s', Str)),
	get(P, slot, lbl1, Lbl1),
	send(Lbl1, selection, S1),
	get(P, slot, window, W),
	get(P, slot, pt, Pt),
	send(W, display,  Lbl1, Pt),
	new(S2, string('The user choose letter %c.', Letter)),
	get(P, slot, lbl2, Lbl2),
	send(Lbl2, selection, S2),
	get(Pt, x, X),
	get(Pt, y, Y),
	Y1 is Y + 30,
	send(W, display, Lbl2, point(X, Y1)).

:- pce_end_class(phrase).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pce_begin_class(counter, object, "count the unclicked cells").
variable(nb, number, both, "number of unclicked cells").

initialise(P, N : number) :->
	send(P, slot, nb, N).

decrement(P) :->
	get(P, slot, nb, N),
	send(N, minus, 1),
	send(P, slot, nb, N),
	(   send(N, equal, 0) ->
	    send(@display, inform, 'The game is over !')
	;   true).
:- pce_end_class(counter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pce_begin_class(cell, path, "The honneycomb cell").
variable(p, path, both, "the cell itself" ).
variable(window, object, both, "the display" ).
variable(letter, name, both, "Upcase letter displayed in the cell" ).
variable(center, point, both, "coordinates of the center of the cell").
variable(color, colour, both, "colour of the cell").
variable(count, counter, both, "counter of unclicked cells").
variable(status, object, both, "clicked/unclicked").
variable(phr, phrase, both, "to display the new letter").

initialise(P, Window : object, Counter : counter,
	   Phrase: phrase, Letter:name, Center:point) :->
	send_super(P, initialise),
	send(P, slot, letter, Letter),
	send(P, slot, center, Center),
	send(P, slot, window, Window),
	send(P, slot, count, Counter),
	send(P, slot, status, unclicked),
	send(P, slot, phr, Phrase),
	new(Pa, path),
        (
	   get(Center, x, X0),
	   get(Center, y, Y0),
	   X is X0 - 25, Y is Y0 -  round(25 * sqrt(3)),
   	   send(Pa, append, point(X, Y)),
	   X1 is X + 50,
  	   send(Pa, append, point(X1, Y)),
	   X2 is X1 + 25,
	   send(Pa, append, point(X2, Y0)),
 	   Y3 is  Y0 + round(25 * sqrt(3)),
	   send(Pa, append, point(X1, Y3)),
	   send(Pa, append, point(X, Y3)),
	   X4 is X - 25,
	   send(Pa, append, point(X4, Y0)),
	   send(Pa, closed, @on)
	),
	send(P, p, Pa),
	send(P, slot, color, colour(@default, 65535, 65535, 0)),
	% create the link between the mouse and the cell
	send(Pa, recogniser,
	     click_gesture(left, '', single, message(P, click))).


unlink(P) :->
	get(P, slot, p, Pa),
	send(Pa, free),
	send(P, send_super, unlink).


% message processed when the cell is clicked
% or when the letter is pressed on the keyboard
click(P) :->
	% test if the cell has already been clicked
	% succeed when the the status is 'unclicked'
	get(P, slot, status, unclicked),
	% change the status
	send(P, slot, status, clicked),
	% change the color
	send(P, slot, color, colour(@default, 65535, 0, 65535)),
	send(P, my_draw),
	get(P, slot, letter, Letter),
	% inform the object "phrase" that a new letter is clicked
	get(P, slot, phr, Phrase),
	send(Phrase, new_letter, Letter),
	% inform the object "counter" that a new letter is clicked
	get(P, count, Counter),
	send(Counter, decrement).

my_draw(P) :->
	% display the path and fill it with the current colour
	get(P, slot, window, W),
	get(P, slot, p, Pa),
        send(W, display, Pa),
        get(P, slot, color, Col),
	send(Pa, fill_pattern, Col),

	% display the letter centered
	get(P, slot, letter, C),
   	new(Str, string(C)),
	new(Tx, text(Str?value)),
	send(Tx, font, font(times, bold, 24)),

	% compute the size of the message to center it
	get(P, slot, center, point(X0,Y0)),
	get(font(times, bold, 24), width(Str), M),
	XT is X0 - M/2,
	get(font(times, bold, 24), height, H),
	YT is Y0 - H/2,
	send(W, display, Tx, point(XT, YT)).


:- pce_end_class(cell).


```

[[File:Prolog_honeycombs.png|400px]]


## PureBasic

Requires PureBasic v4.60.  Screen controls in PureBasic are referred to as 'gadgets'.

```PureBasic
Structure hexGadget
  text.s
  Status.i     ;nonselected = 0, selected = 1
  center.POINT ;location of hex's center
  List shape.POINT()
EndStructure

Structure honeycomb
  gadgetID.i
  margins.POINT
  unusedLetters.s
  chosen.s
  maxLength.i
  Array hexGadgets.hexGadget(0)
  textY.i
EndStructure

Prototype hexEvent_prt(*h.honeycomb, hexID)

Procedure inpoly(*p.POINT, List poly.POINT())
  ;returns 1 if point is inside the polygon defined by poly(), otherwise returns 0
  Protected new.POINT, old.POINT, lp.POINT, rp.POINT, i, inside, *poly
  If ListSize(poly()) < 3: ProcedureReturn 0: EndIf
  LastElement(poly()): old = poly()
  ForEach poly()
    ;find leftmost endpoint 'lp' and the rightmost endpoint 'rp' based on x value
    If poly()\x > old\x
      lp = old
      rp = poly()
    Else
      lp = poly()
      rp = old
    EndIf
    If lp\x < *p\x And *p\x <= rp\x And (*p\y - lp\y) * (rp\x - lp\x) < (rp\y - lp\y) * (*p\x - lp\x)
      inside = ~inside
    EndIf
    old = poly()
  Next
  ProcedureReturn inside & 1
EndProcedure

;draw a hex Gadget by number
Procedure drawhex(*h.honeycomb, hexID)
  With *h\hexGadgets(hexID)
    Protected p.POINT
    If LastElement(\shape())
      p = \shape()
    EndIf
    ForEach \shape()
      LineXY(p\x, p\y, \shape()\x, \shape()\y, RGB(0, 0, 0)) ;black
      p = \shape()
    Next
    DrawingMode(#PB_2DDrawing_Transparent)
    DrawingFont(FontID(0))
    If \Status
      FillArea(\center\x + 1, \center\y + 1, RGB(0, 0, 0), RGB($FF, 0, $FF))    ;magenta
      DrawText(\center\x - TextWidth(\text) / 2, \center\y - TextHeight(\text) / 2, \text, RGB(0, 0, 1)) ;black, almost
    Else
      FillArea(\center\x + 1, \center\y + 1, RGB(0, 0, 0), RGB($FF, $FF, 0)) ;yellow
      DrawText(\center\x - TextWidth(\text) / 2, \center\y - TextHeight(\text) / 2, \text, RGB($FF, 0, 0)) ;red
    EndIf
  EndWith
EndProcedure

Procedure selectHex(*h.honeycomb, hexID)
  If Not *h\hexGadgets(hexID)\Status
    *h\chosen + *h\hexGadgets(hexID)\text
    *h\hexGadgets(hexID)\Status = 1
    StartDrawing(CanvasOutput(*h\gadgetID))
      drawhex(*h, hexID)
      DrawingMode(#PB_2DDrawing_Default)
      DrawingFont(#PB_Default)
      DrawText(0, *h\textY, "Chosen: " + *h\chosen)
      DrawText(0, *h\textY + 20, "The user chose letter " + *h\hexGadgets(hexID)\text + ".  ")
    StopDrawing()
    ProcedureReturn 1
  EndIf
EndProcedure

Procedure hexKey(*h.honeycomb, hexID)
  If UCase(Chr(GetGadgetAttribute(*h\gadgetID, #PB_Canvas_Input))) = *h\hexGadgets(hexID)\text
    ProcedureReturn selectHex(*h, hexID)
  EndIf
EndProcedure

Procedure hexMouse(*h.honeycomb, hexID)
  Protected mPos.POINT
  mPos\x = GetGadgetAttribute(*h\gadgetID, #PB_Canvas_MouseX)
  mPos\y = GetGadgetAttribute(*h\gadgetID, #PB_Canvas_MouseY)
  If inpoly(mPos,*h\hexGadgets(hexID)\shape())
    ProcedureReturn selectHex(*h, hexID)
  EndIf
EndProcedure

Procedure honeycombEvents(*h.honeycomb)
  If Len(*h\chosen) >= *h\maxLength: ProcedureReturn: EndIf

  Protected event = EventType(), *eventFunction.hexEvent_prt
  Select event
    Case #PB_EventType_Input
      *eventFunction = @hexKey()
    Case #PB_EventType_LeftButtonUp
      *eventFunction = @hexMouse()
    Case #PB_EventType_LostFocus
      SetActiveGadget(*h\gadgetID)
  EndSelect

  If *eventFunction
    For hexID = 0 To ArraySize(*h\hexGadgets())
      If *eventFunction(*h, hexID)
        Break ;event successfully handled
      EndIf
    Next
  EndIf
EndProcedure

Procedure createHexGadget(*h.honeycomb, hexID, x, y, dx, dy)
  With *h\hexGadgets(hexID)
    If *h\unusedLetters
      Protected letterNum = Random(Len(*h\unusedLetters) - 1) + 1
      \text = Mid(*h\unusedLetters, letterNum, 1)
      *h\unusedLetters = ReplaceString(*h\unusedLetters, \text, "")
    EndIf
    \center\x = x: \center\y = y
    AddElement(\shape()): \shape()\x = x - dx:     \shape()\y = y
    AddElement(\shape()): \shape()\x = x - dx / 2: \shape()\y = y + dy
    AddElement(\shape()): \shape()\x = x + dx / 2: \shape()\y = y + dy
    AddElement(\shape()): \shape()\x = x + dx:     \shape()\y = y
    AddElement(\shape()): \shape()\x = x + dx / 2: \shape()\y = y - dy
    AddElement(\shape()): \shape()\x = x - dx / 2: \shape()\y = y - dy
  EndWith
EndProcedure

Procedure initHoneycomb(*h.honeycomb, posX, posY, dx = 30, dy = 25, marginX = 10, marginY = 5)
  Protected i, sx, sy, hCols = 5, hRows = 4, hexGadgetCount = hCols * hRows - 1
  If Not *h: ProcedureReturn 0: EndIf

  *h\unusedLetters.s = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  *h\chosen = ""
  *h\maxLength = 20

  Dim *h\hexGadgets(hexGadgetCount)
  ;calculate size width, height and create honeycomb with margins
  sx = Round(dx * (0.5 + hCols * 1.5), #PB_Round_Nearest) + 1 + 2 * marginX
  sy = dy * (2 * hRows + 1) + 1 + 2 * marginY + 2 * 20 ;includes room for hex, margins, and text
  *h\textY = sy - 2 * 20

  ;create hexes
  Protected hexID, column, row, x, y, baseX, baseY, majorOffsetY = dy
  baseX = dx + marginX
  For column = 0 To hCols - 1
    baseY = dy + marginY
    majorOffsetY ! dy
    For row = 0 To hRows - 1
      x = baseX
      y = baseY + majorOffsetY
      createHexGadget(*h, hexID, x, y, dx, dy)
      baseY + dy * 2
      hexID + 1
    Next
    baseX + dx * 1.5
  Next

  ;draw honeycomb
  *h\gadgetID = CanvasGadget(#PB_Any, posX, posY, sx, sy, #PB_Canvas_Keyboard | #PB_Canvas_ClipMouse)
  If *h\gadgetID = 0: ProcedureReturn 0: EndIf ;failed to created honeycomb

  LoadFont(0, "Arial", 24, #PB_Font_Bold)
  StartDrawing(CanvasOutput(*h\gadgetID))
    For i = 0 To ArraySize(*h\hexGadgets())
      drawhex(*h, i)
    Next
    Box(0, *h\textY, sx, 40, RGB(0, 0, 0)) ;draw black text box
  StopDrawing()
  ProcedureReturn 1
EndProcedure

If OpenWindow(0, 0, 0, 400, 400, "PureBasic - Honeycombs", #PB_Window_SystemMenu)
  Define honeycomb.honeycomb, quit
  If Not initHoneycomb(honeycomb, 0, 0): End: EndIf
  ResizeWindow(0, #PB_Ignore, #PB_Ignore, GadgetWidth(honeycomb\gadgetID), GadgetHeight(honeycomb\gadgetID))
  SetActiveGadget(honeycomb\gadgetID)

  Repeat
    event = WaitWindowEvent()

    Select event
      Case #PB_Event_Gadget
        If EventGadget() = honeycomb\gadgetID
          honeycombEvents(honeycomb)
          If Len(honeycomb\chosen) = honeycomb\maxLength
            MessageRequester("Exit", "You chose: " + honeycomb\chosen + ".")
            quit = 1
          EndIf
        EndIf
      Case #PB_Event_CloseWindow
        quit = 1
    EndSelect

  Until quit = 1
  FreeGadget(honeycomb\gadgetID)
  CloseWindow(0)
EndIf
```

[[File:PureBasic_Honeycomb.png]]


## Python

[[Honeycombs/Python|Rambling python3 code with tkinter]]


## Racket


```racket
#lang racket

(struct Hex (x y letter clicked?) #:mutable #:transparent)

(define hexes
  (let* ([A (char->integer #\A)]
         [letters (take (shuffle (map (compose string integer->char)
                                      (range A (+ A 26))))
                        20)])
    (for*/list ([row 4] [column 5])
      (Hex (* 3/2 column) (* 2 (+ row (if (odd? column) 1/2 0)))
           (list-ref letters (+ (* 5 row) column))
           false))))

(require 2htdp/image)
(define (blank width height) (rectangle width height 'outline (color 0 0 0 0)))

(define (hexagon mode color) (regular-polygon 1 6 mode color))
(define aspect-ratio (sin (/ pi 3)))

(define (board _)
  (scale 100
         (for/fold ([the-board (blank 8 (* aspect-ratio 9))])
           ([hex hexes])
           (define-values (letter-color hex-color)
             (if (Hex-clicked? hex) (values 'black 'purple) (values 'red 'yellow)))
           (underlay/align/offset
            'left 'top the-board
            (Hex-x hex) (* aspect-ratio (Hex-y hex))
            (overlay (scale 1/10 (text (Hex-letter hex) 10 letter-color))
                     (hexagon 'outline 'black)
                     (hexagon 'solid hex-color))))))

#| Closest hex in hexes to x y, as one with minimum distance to its center. |#
(define (hex-at x y)
  (argmin (λ (hex) (+ (sqr (- x (* 100 (add1 (Hex-x hex)))))
                      (sqr (- y (* aspect-ratio 100 (add1 (Hex-y hex)))))))
          hexes))

(define letters-chosen '())
(define (choose hex)
  (set-Hex-clicked?! hex true)
  (define letter (Hex-letter hex))
  (when (not (member letter letters-chosen))
    (set! letters-chosen (list* (Hex-letter hex) letters-chosen))))

(require 2htdp/universe)
(void (big-bang
       (void)
       [to-draw board]
       [stop-when (λ (_) (andmap Hex-clicked? hexes)) board]
       [on-key (λ (_ k)
                 (define hex (findf (λ (hex) (key=? k (string-downcase (Hex-letter hex))))
                                    hexes))
                 (when hex (choose hex)))]
       [on-mouse (λ (_ x y event-type)
                   (when (equal? "button-down" event-type)
                     (choose (hex-at x y))))]))

(displayln "The letters were chosen in the order:")
(for-each display (add-between (reverse letters-chosen) " "))
```



## Ruby

{{libheader|Shoes}}

```ruby
Shoes.app(title: "Honeycombs", height: 700, width: 700) do
  C = Math::cos(Math::PI/3)
  S = Math::sin(Math::PI/3)
  Radius = 60.0
  letters = [
    %w[L A R N D 1 2],
    %w[G U I Y T 3 4],
    %w[P C F E B 5 6],
    %w[V S O M K 7 8],
    %w[Q X J Z H 9 0],
  ]

  def highlight(hexagon)
    hexagon.style(fill: magenta)
  end

  def unhighlight(hexagon)
    hexagon.style(fill: yellow)
  end

  def choose(hexagon)
    hexagon.choose
    highlight hexagon
    chosen = @hexagons.find_all {|h| h.chosen?}.map {|h| h.letter}
    if chosen.size == @hexagons.size
      @chosen.text = 'Every hexagon has been chosen.'
    else
      @chosen.text = "Chosen: #{chosen.sort.join(',')}\n" +
                     "Last Chosen: #{hexagon.letter}"
    end
  end

  width = 20 + (Radius*(7*letters[0].size - 3)/4.0).ceil
  height = 60 + (Radius*(1 + 2*S*letters.size)).ceil
  @hexagons = []
  letter_to_hex = {}

  # create the GUI
  stack(height: height, width: width) do
    @chosen = para("Chosen:\nLast chosen:")

    # draw the hexagrams
    letters.each_index do |row|
      letters[0].each_index do |column|
        x = 60 + column * Radius * 0.75 + (1-S) * Radius
        y = 80 + row * S * Radius + (column.odd? ? S * Radius * 0.5 : 0)
        h = shape(x-Radius, y-S*Radius) do
          strokewidth 3
          move_to(x-C*Radius, y-S*Radius)
          line_to(x+C*Radius, y-S*Radius)
          line_to(x+Radius,   y)
          line_to(x+C*Radius, y+S*Radius)
          line_to(x-C*Radius, y+S*Radius)
          line_to(x-Radius,   y)
          line_to(x-C*Radius, y-S*Radius)
        end

        # add some attributes and methods to the shape
        class << h
          attr_accessor :x, :y, :state, :letter
          def chosen?
            not @state.nil?
          end
          def choose
            @state = :chosen
          end
          def contains?(px, py)
            if @x-Radius < px and px <= @x-C*Radius
              ratio = (px - @x + Radius)/(Radius*(1-C))
              @y - ratio*S*Radius < py and py <= @y + ratio*S*Radius
            elsif @x-C*Radius < px and px <= @x+C*Radius
              @y - S*Radius < py and py < @y + S*Radius
            elsif @x+C*Radius < px and px <= @x+Radius
              ratio = (@x + Radius - px)/(Radius*(1-C))
              @y - ratio*S*Radius < py and py <= @y + ratio*S*Radius
            else
              false
            end
          end
          def inspect
            %q(<%s,"%s",%s,%d@%d>) % [self.class, letter, chosen?, x, y]
          end
        end

        h.x = x + x - Radius
        h.y = y + y - S*Radius
        h.letter = letters[row][column]
        unhighlight h

        @hexagons << h
        letter_to_hex[h.letter.downcase] = h
        letter_to_hex[h.letter.upcase] = h

        # add the letter to the hexagon
        para(h.letter).style(size:56, stroke:red) \
                      .move(h.x - C*Radius, h.y - S*Radius)
      end
    end

    # highlight the hexagon under the mouse
    hex_over = nil
    motion do |x, y|
      hex = @hexagons.find {|h| h.contains?(x,y)}
      unless hex.nil? or hex.chosen?
        highlight hex
      end
      unless hex_over == hex or hex_over.nil? or hex_over.chosen?
        unhighlight hex_over
      end
      hex_over = hex
    end

    # handle mouse clicks
    click do |button, x, y|
      info("button #{button} clicked at (#{x}, #{y})")
      hexagon = @hexagons.find {|h| h.contains?(x,y)}
      if hexagon
        info("clicked hexagon #{hexagon}")
        choose hexagon
      end
    end

    # handle keystrokes
    keypress do |key|
      if key == "\x11"  # control-Q
        exit
      elsif key == "?"
        info @hexagons.collect {|h| h.inspect}.join("\n")
      elsif letter_to_hex.has_key?(key)
        info("pressed key #{key} -> #{letter_to_hex[key]}")
        choose letter_to_hex[key]
      end
    end
  end
end
```



## Scala


### Java Swing Interoperability

{{libheader|Scala Java Swing interoperability}}
{{works with|Scala|2.13}}

```Scala
import java.awt.{BasicStroke, BorderLayout, Color, Dimension,
  Font, FontMetrics, Graphics, Graphics2D, Point, Polygon, RenderingHints}
import java.awt.event.{KeyAdapter, KeyEvent, MouseAdapter, MouseEvent}

import javax.swing.{JFrame, JPanel}

import scala.math.{Pi, cos, sin}

object Honeycombs extends App {
  private val (letters, x1, y1, x2, y2, h, w) = ("LRDGITPFBVOKANUYCESM", 150, 100, 225, 143, 87, 150)

  private class HoneycombsPanel() extends JPanel {
    private val comb: IndexedSeq[Hexagon] =
      for {i <- 0 until 20
           (x: Int, y: Int) =
           if (i < 12) (x1 + (i % 3) * w, y1 + (i / 3) * h)
           else (x2 + (i % 2) * w, y2 + ((i - 12) / 2) * h)
           } yield Hexagon(x, y, w / 3, letters(i))

    override def paintComponent(gg: Graphics): Unit = {
      super.paintComponent(gg)
      val g = gg.asInstanceOf[Graphics2D]
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.setFont(new Font("SansSerif", Font.BOLD, 30))
      g.setStroke(new BasicStroke(3))
      comb.foreach(_.draw(g))
    }

    case class Hexagon(x: Int, y: Int, halfWidth: Int, letter: Char,
                       var hasBeenSelected: Boolean = false) extends Polygon {
      private val (baseColor, selectedColor) = (Color.yellow, Color.magenta)

      def setSelected(): Unit = hasBeenSelected = true

      def draw(g: Graphics2D): Unit = {
        val fm: FontMetrics = g.getFontMetrics
        val (asc, dec) = (fm.getAscent, fm.getDescent)

        def drawCenteredString(g: Graphics2D, s: String): Unit = {
          val x: Int = bounds.x + (bounds.width - fm.stringWidth(s)) / 2
          val y: Int = bounds.y + (asc + (bounds.height - (asc + dec)) / 2)
          g.drawString(s, x, y)
        }

        g.setColor(if (hasBeenSelected) selectedColor else baseColor)
        g.fillPolygon(this)
        g.setColor(Color.black)
        g.drawPolygon(this)
        g.setColor(if (hasBeenSelected) Color.black else Color.red)
        drawCenteredString(g, letter.toString)
      }

      for (i <- 0 until 6)
        addPoint((x + halfWidth * cos(i * Pi / 3)).toInt, (y + halfWidth * sin(i * Pi / 3)).toInt)

      getBounds
    }

    addKeyListener(new KeyAdapter() {
      override def keyPressed(e: KeyEvent): Unit = {
        val key = e.getKeyChar.toUpper
        comb.find(_.letter == key).foreach(_.setSelected())
        repaint()
      }
    })

    addMouseListener(new MouseAdapter() {
      override def mousePressed(e: MouseEvent): Unit = {
        val mousePos: Point = e.getPoint

        comb.find(h => h.contains(mousePos)).foreach(_.setSelected())
        repaint()
      }
    })

    setBackground(Color.white)
    setPreferredSize(new Dimension(600, 500))
    setFocusable(true)
    requestFocus()
  }

  new JFrame("Honeycombs") {
    add(new HoneycombsPanel(), BorderLayout.CENTER)
    pack()
    setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    setLocationRelativeTo(null)
    setResizable(false)
    setVisible(true)
  }

}
```



## Sidef

{{trans|Perl}}
[[File:Honeycombs_sidef.png|250px|thumb|right]]

```ruby
require('Tk')

class Honeycombs(
    Number size   = 36,
    Array letters = @('A' .. 'Z').shuffle.first(20),
) {

    define tk   = %S<Tk>
    has changed = Hash()

    func altitude(n) {
        sqrt(3/4) * n
    }

    method polygon_coordinates(x, y, size) {
        var alt = altitude(size)
        return (x - size,       y,
                x - size/2, y - alt,
                x + size/2, y - alt,
                x + size,       y,
                x + size/2, y + alt,
                x - size/2, y + alt,
               );
    }

    method change(canvas, id, letter_id) {
        return {
            canvas.itemconfigure(id,        '-fill' => 'magenta')
            canvas.itemconfigure(letter_id, '-fill' => 'black')
            changed{id} = true

            if (20 == changed.len) {
                say "All letters pressed."
                canvas.MainWindow.after(10, { tk.exit })
            }
        }
    }

    method comb(canvas, fromx, fromy, size, count) {
        for x,y in (
            RangeNum(fromx, 3*count*size - 1,          3*size) ~X
            RangeNum(fromy,     7.5*size - 1, 2*altitude(size))
        ) {
            var id = canvas.createPolygon(
                                  self.polygon_coordinates(x, y, size),
                                  '-outline' => 'black',
                                  '-fill'    => 'yellow',
                                  '-width'   => 2,
                                )
            var letter = letters.shift
            var letter_id = canvas.createText(x, y,
                                     '-fill' => 'red',
                                     '-text' => letter,
                                     '-font' => "{sans} #{size * 0.9}",
                                )
            canvas.MainWindow.bind('all', letter.lc,
                                      self.change(canvas, id, letter_id))
            [id, letter_id].each { |b|
                canvas.bind(b, '<Button-1>',
                                      self.change(canvas, id, letter_id))
            }
        }
    }

    method display(title) {
        {
            var mw     = %s'MainWindow'.new('-title' => title)
            var canvas = mw.Canvas('-width'  => 8*size,
                                   '-height' => 8*size).pack

            self.comb(canvas, size,       size,                  size, 3)
            self.comb(canvas, size * 2.5, size + altitude(size), size, 2)

            var btn = mw.Button('-text'      => 'Quit',
                                '-underline' => 0,
                                '-command'   => { tk.exit },
                               ).pack
            mw.bind('<Alt-q>', { btn.invoke })
            tk.MainLoop()
        }.fork
    }
}

Honeycombs().display(title: 'Honeycombs')
```



## Tcl

{{libheader|Tk}}

```tcl
package require Tcl 8.5
package require Tk

# How to make a honeycomb
proc honeycomb {w letterpattern} {
    canvas $w -width 500 -height 470
    set basey 10
    foreach row $letterpattern {
	set basex 10
	set majoroffsety 0
	foreach letter $row {
	    set x [expr {$basex + 60}]
	    set y [expr {$basey + 50 + $majoroffsety}]
	    drawhex $w $x $y $letter 30 50
	    set majoroffsety [expr {50 - $majoroffsety}]
	    incr basex 90
	}
	incr basey 100
    }
    return $w
}

namespace import tcl::mathop::?   ;# For convenience
# How to draw a single hexagon, centered at a particular point.
proc drawhex {w x y ch dx dy} {
    if {$ch eq ""} return          ;# Allow elision of cells (not used here)
    $w create polygon \
	[- $x $dx] [- $y $dy] [+ $x $dx] [- $y $dy] [+ $x $dx $dx] $y \
	[+ $x $dx] [+ $y $dy] [- $x $dx] [+ $y $dy] [- $x $dx $dx] $y \
	-fill yellow -outline black -tags [list hex$ch hull$ch] -width 3
    $w create text $x $y -text $ch -fill red -tags [list hex$ch txt$ch] \
	-font {Arial 72 bold}
    # Install bindings on items
    $w bind hex$ch <Enter> [list enterhex $w $ch]
    $w bind hex$ch <Leave> [list leavehex $w $ch]
    $w bind hex$ch <Button-1> [list dohex $w $ch]
    # Handle keyboard activity through canvas-level bindings
    bind $w [string toupper $ch] [list dokey $w $ch]
    bind $w [string tolower $ch] [list dokey $w $ch]
}

# Callbacks for various bindings
proc enterhex {w ch} {
    global chosen
    if {$ch ni $chosen} {
	$w itemconfigure hull$ch -fill magenta
	$w itemconfigure txt$ch -fill black
    }
}
proc leavehex {w ch} {
    global chosen
    if {$ch ni $chosen} {
	$w itemconfigure hull$ch -fill yellow
	$w itemconfigure txt$ch -fill red
    }
}
proc dohex {w ch} {
    global chosen
    if {$ch ni $chosen} {
	lappend chosen $ch
	puts "chosen $ch"
    }
    if {[llength $chosen] >= 5} {
	destroy $w
    }
}
proc dokey {w ch} {
    enterhex $w $ch
    dohex $w $ch
}

# Initial declarations of state variables
set chosen {}
set letterpattern {
    {L A R N D}
    {G U I Y T}
    {P C F E B}
    {V S O M K}
}

# Build the GUI
pack [honeycomb .c $letterpattern]
focus .c
# Usually don't use this, but it's ideal for this interaction pattern
tkwait window .c
puts "overall list of characters: $chosen"
exit
```



## XPL0

[[File:HoneyXPL0.gif|right]]

```XPL0
include c:\cxpl\stdlib;                 \(color definitions, etc.)

proc DrawHexagon(X0, Y0, Side, Color);  \Draw a filled hexagon centered at X0,Y0
int  X0, Y0, Side, Color;
int  X, Y;
for Y:= -Side*19/22 to +Side*19/22 do           \19/22 aprox = sqrt(3.0)/2.0
    for X:= -Side to +Side do
        if abs(X) + abs(Y)*23/38 <= Side then   \23/38 aprox = 1.0/sqrt(3.0)
            Point(X+X0, Y+Y0, Color);

def Cols=5, Rows=4, X0=23, Y0=20;               \matrix shape, offset on screen
int C, R, X, Y, Letter, Counter, SaveX(26), SaveY(26);
[SetVid($101);                                  \set video to 640x480x8 graphics
for Letter:= 0 to 26-1 do SaveX(Letter):= 0;
Attrib(LMagenta<<8);                            \light magenta letter background
for R:= 0 to Rows-1 do
    for C:= 0 to Cols-1 do
        [X:= C*35 + X0;  Y:= R*40 + (C&1)*20 + Y0;
        DrawHexagon(X, Y, 22, LMagenta);
        repeat Letter:= Ran(26) until SaveX(Letter) = 0;
        Move(X-4, Y-7);  ChOut(6, Letter+^A);
        SaveX(Letter):= X;  SaveY(Letter):= Y;  \save coordinates for letter
        ];
Counter:= 0;
repeat  Letter:= ChIn(1);                       \get letter from keyboard
        if Letter>=^a & Letter<=^z then Letter:= Letter-$20;    \make uppercase
        if Letter>=^A & Letter<=^Z then
           if SaveX(Letter-^A) # 0 then         \letter is available
                [X:= SaveX(Letter-^A);  Y:= SaveY(Letter-^A);
                DrawHexagon(X, Y, 22, Yellow);  \change color
                Move(X-4, Y-7);
                Attrib(Yellow<<8);  ChOut(6, Letter);
                Move(Counter*8+8, Rows*40+30);  \show record of chosen letters
                Attrib(Yellow);  ChOut(6, Letter);
                Counter:= Counter+1;
                SaveX(Letter-^A):= 0;           \letter is no longer available
                ]
           else ChOut(0, Bel)                   \letter is not available
        else ChOut(0, Bel);                     \not a letter (A..Z)
until   Counter >= Cols*Rows;
SetVid($03);                                    \restore normal text display
]
```


{{omit from|ACL2}}
{{omit from|AWK}}
{{omit from|Blast}}
{{omit from|GUISS|An application would need to be installed for this}}
{{omit from|Lilypond}}
{{omit from|Lotus 123 Macro Scripting}}
{{omit from|MIRC Scripting Language}}
{{omit from|TI-83 BASIC|Does not have a color display.}}
{{omit from|UNIX Shell}}
