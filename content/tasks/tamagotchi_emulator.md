+++
title = "Tamagotchi emulator"
description = ""
date = 2019-04-30T04:03:02Z
aliases = []
[extra]
id = 19799
[taxonomies]
categories = ["task"]
tags = []
+++

If you don't know what Tamagotchi is, take a look at the [[wp:Tamagotchi|Wikipedia]] page about it.

This task is about creating a Tamagotchi emulator, a virtual pet that you must take care of.

Your virtual pet must, like real pets, at least: get hungry, get bored, age and poop!<br />
Against hunger, you must create a way to feed it. Against boredom, you must play with or pet it. The poop must be cleaned, otherwise the pet might get sick and if it is not cured, it might die from its disease. Finally, the pet should grow older and eventually die.

On screen, your program must display the virtual pet status data - age, hunger and happiness levels, when the pet poops, its poop must also be displayed. Ah, well, an avatar of the pet must be there too, but I guess that's obvious!

What else? Well, use your creativity…<br />
Every pet needs a name. What kind of games, or ‘mini games’ one can play with his pet? And so on!

But, above of all, have fun!


## EchoLisp

The tamagotchi status is saved in permanent storage. The tamagotchi cycles can be started manually, or in the '''preferences''' function, or at predefined intervals : '''every''' function. The following code may be loaded from the EchoLisp library : (load 'tamagotchi). This tamagotchi does not play, but gets bored, and needs to talk. It must be feed two times between each cycle. It will die at age around 42 (42 cycles).

```scheme


(define-constant CYCLE_TIME 30000) ;; 30 sec for tests, may be 4 hours, 1 day ...
(string-delimiter "")
(struct tamagotchi (name age food poop bored))

;; utility : display tamagotchi thoughts : transitive verb + complement
(define (tama-talk tama)
(writeln (string-append
   "😮 : "
   (word-random #:any '( verbe trans inf -vintran)))
   " les " 
   (word-random #:any '(nom pluriel))))

;; load tamagotchi from persistent storage into *tama* global
(define (run-tamagotchi)
	(if (null? (local-get '*tama*))
	    (writeln "Please (make-tamagotchi <name>)")
	(begin 
	    (make-tamagotchi (tamagotchi-name *tama*) *tama*)
	    (tama-cycle *tama*)
	    (writeln (tama-health *tama*)))))
		
;; make a new tamagotchi
;; or instantiate an existing
;; tama : instance ot tamagotchi structure
(define (make-tamagotchi name (tama null))
(when (null? tama) 
		(set! tama (tamagotchi name 0 2 0 0))
		(define-global '*tama* tama)
		(local-put '*tama*))
		
;; define the <name> procedure :
;;  perform user action / save tamagotchi / display status

(define-global name
(lambda (action)
	(define tama (local-get '*tama*))
	[case action
	((feed) (set-tamagotchi-food! tama  (1+  (tamagotchi-food tama))))
	((talk)  (tama-talk tama) (set-tamagotchi-bored! tama (max 0 (1- (tamagotchi-bored tama)))))
	((clean) (set-tamagotchi-poop! tama (max 0 (1- (tamagotchi-poop tama)))))
	((look) #t)
;; debug actions
	((_cycle) (tama-cycle tama))
	((_reset) (set! *tama* null) (local-put '*tama*))
	((_kill) (set-tamagotchi-age! tama 44))
	((_self) (writeln tama))
	(else (writeln "actions: feed/talk/clean/look"))]
	
	(local-put '*tama*)
	(tama-health tama))))
	
;; every n msec : get older / eat food / get bored / poop
(define (tama-cycle tama)
		(when (tama-alive tama)
		(set-tamagotchi-age! tama (1+ (tamagotchi-age tama)))
		(set-tamagotchi-bored! tama (+   (tamagotchi-bored tama) (random 2)))
		(set-tamagotchi-food! tama  (max 0 (-  (tamagotchi-food tama) 2)))
		(set-tamagotchi-poop! tama  (+   (tamagotchi-poop tama) (random 2))))
		(local-put '*tama*))
		
;; compute sickness (too much poop, too much food, too much bored)
(define (tama-sick tama) 	
	 (+ (tamagotchi-poop tama) 
	    (tamagotchi-bored tama) 
	    (max 0 (- (tamagotchi-age tama) 32)) ;; die at 42
	    (abs (-  (tamagotchi-food tama) 2))))
	    
;; alive if sickness <= 10
(define (tama-alive tama)
		(<= (tama-sick tama) 10))
	
;; display num icons from a list
(define (icons list num)
		(for/fold (str "  ") ((i [in-range 0 num] )) 
		(string-append str (list-ref list (random (length list))))))
	
;; display boredom/food/poops icons
(define (tama-status tama)
	(if (tama-alive tama)
		(string-append 
		" [ "
		(icons '(💤  💭 ❓ ) (tamagotchi-bored tama))  
		(icons  '(🍼 🍔 🍟 🍰  🍜 ) (tamagotchi-food tama))
		(icons '(💩) (tamagotchi-poop tama))
		" ]")
	    " R.I.P" ))
	
;; display health status = f(sickness)
(define (tama-health tama)
	(define sick (tama-sick tama))
	;;(writeln 'health:sick°= sick)
	(string-append 
	(format "%a (🎂 %d)  " (tamagotchi-name tama)(tamagotchi-age tama))
	(cond 
	([<= sick 2]   (icons '(😄  😃  😀 😊  😎️  👍 ) 1 ))  ;; ok <= 2
	([<= sick 4]   (icons '(😪  😥  😰  😓  )  1))
	([<= sick 6]   (icons '(😩  😫 )  1))
	([<= sick 10]  (icons '(😡  😱 ) 1)) ;; very bad
	(else  (icons '(❌  💀  👽  😇 ) 1))) ;; dead
    (tama-status tama)))
    
;; timer operations
;; run tama-proc = cycle every CYCLE_TIME msec

(define (tama-proc n)
	(define tama (local-get '*tama*))
	(when (!null? tama)
		(tama-cycle tama)
		(writeln (tama-health tama))))
		
;; boot
;; manual boot or use (preferences) function
(every CYCLE_TIME tama-proc)
(run-tamagotchi)


```

User commands are function calls : (albert 'clean). The rest is automatic display : one status line / cycle.

```txt

(lib 'struct)
(lib 'sql) ;; for words
(lib 'words)
(lib 'timer)
(lib 'dico.fr);; will talk in french
(load 'tamagotchi)
Please (make-tamagotchi <name>)   
    
    (make-tamagotchi 'albert)

    albert (🎂 1) 😓 [ 💭 ]    
    albert (🎂 2) 😰 [ 💭❓ ]    ;; needs to talk
    (albert 'talk)
    😮 : déléaturer     les      fidèles    
    albert (🎂 2) 😓 [ ❓ ]
    (albert 'talk)
    😮 : facetter     les      décorations    
    albert (🎂 2) 😃 [ ]
    albert (🎂 3) 😥 [ 💤 💩 ]    
    (albert 'feed)
    albert (🎂 3) 😪 [ 💤 🍔 💩 ] ;; needs cleaning
    (albert 'clean)
    albert (🎂 3) 😊 [ 💭 🍟 ]
    (albert 'talk)
    😮 : manifester     les      canyons    
    albert (🎂 3) 😃 [ 🍰 ] ;; all is ok
    albert (🎂 4) 👍 [ ]    
    albert (🎂 5) 😃 [ ]    
    albert (🎂 6) 😥 [ ❓ 💩 ]    
    (albert 'clean)
    albert (🎂 6) 😪 [ 💤 ]
    albert (🎂 7) 😰 [ 💭 💩 ]    
    albert (🎂 8) 😓 [ 💭 💩 ]    
    (for ((i 10)) (albert 'feed))
    albert (🎂 8) 😱 [ 💭 🍔🍼🍔🍼🍜🍼🍟🍔🍔🍟 💩 ] ;; very sick
    albert (🎂 9) 😱 [ ❓ 🍰🍰🍟🍟🍼🍟🍜🍜 💩💩 ]    
    (albert 'talk)
    😮 : assortir     les      déchiffrages    
    albert (🎂 9) 😱 [ 🍼🍼🍟🍟🍜🍔🍼🍰 💩💩 ]
    albert (🎂 10) 😡 [ 💭 🍰🍟🍟🍟🍟🍜 💩💩💩 ]  

    (for ((i 20)) (albert 'talk)) ;; can talk without getting tired
    😮 : réaliser     les      délassements    
    😮 : ratiboiser     les      étatistes    
    😮 : commenter     les      diphtongues    
    😮 : jurer     les      samouraïs    
    😮 : bousculer     les      méchages    
    😮 : épépiner     les      dénicotiniseurs    
    😮 : témoigner     les      péniches    
    😮 : pateliner     les      maquereaux    
    😮 : conseiller     les      diminutifs    
    😮 : gratiner     les      perdreaux    
    😮 : klaxonner     les      élues    
    😮 : ganser     les      dévoltages    
    😮 : réconcilier     les      pixels    ;; reconciliate the pixels
    😮 : rocher     les      écrasés    
    😮 : guêtrer     les      transgressions    
    😮 : lanterner     les      pisseurs    
    😮 : opérer     les      rasades    
    😮 : actionner     les      loukoums    
    😮 : dégarnir     les      artichauts    
    😮 : chanfreiner     les      rajeunissements    
 
    (for ((i 14)) (albert 'feed)) ;; don't feed it too much .. it will die
    albert (🎂 10) 😇 R.I.P ;; died at age 10
    albert (🎂 10) ❌ R.I.P    
    albert (🎂 10) 👽 R.I.P    
    albert (🎂 10) 😇 R.I.P    
    albert (🎂 10) 😇 R.I.P  
  
    (make-tamagotchi 'simon)
    simon
    simon (🎂 1) 😓 [ 💤 💩 ]    

```



## Forth


```forth
( current object )
0 value o
' o >body constant 'o
: >o ( o -- ) postpone o postpone >r postpone 'o postpone ! ; immediate
: o> ( -- )   postpone r> postpone 'o postpone ! ; immediate

( chibi: classes with a current object and no formal methods )
0 constant object
: subclass ( class "name" -- a )  create here swap , does> @ ;
: class ( "name" -- a )  object subclass ;
: end-class ( a -- )  drop ;
: var ( a size "name" -- a )  over dup @ >r +!
  : postpone o r> postpone literal postpone + postpone ; ;

( tamagotchi )
class tama
  cell var hunger
  cell var boredom
  cell var age
  cell var hygiene
  cell var digestion
  cell var pooped
end-class

: offset ( -- )  \ go to column #13 of current line
  s\" \e[13G" type ;

: show ( "field" -- )
  ' POSTPONE literal POSTPONE dup
  POSTPONE cr POSTPONE id. POSTPONE offset
  POSTPONE execute POSTPONE ? ;  immediate
: dump ( -- )
  show hunger  show boredom  show age  show hygiene
  cr ." pooped" offset pooped @ if ." yes" else ." no" then ;

\ these words both exit their caller on success
: -poop ( -- )
  digestion @ 1 <> ?exit  digestion off  pooped on
  cr ." tama poops!"  r> drop ;
: -hunger ( -- )
  digestion @ 0 <> ?exit  hunger ++
  cr ." tama's stomach growls"  r> drop ;

: died-from ( 'reason' f -- )
  if cr ." tama died from " type cr bye then 2drop ;
: by-boredom ( -- )  "boredom"  boredom @ 5 > died-from ;
: by-sickness ( -- ) "sickness" hygiene @ 1 < died-from ;
: by-hunger ( -- )   "hunger"   hunger @  5 > died-from ;
: by-oldness ( -- )  "age"      age @    30 > died-from ;

: sicken ( -- )  pooped @ if hygiene -- then ;
: digest ( -- )  -poop -hunger  digestion -- ;
: die ( -- )     by-boredom  by-sickness  by-hunger  by-oldness ;

( tamagotchi ops )
: spawn ( -- )
  cr ." tama is born!"
  hunger off  boredom off  age off  pooped off
  5 hygiene !  5 digestion ! ;

: wait ( -- )
  cr ." ** time passes **"
  boredom ++  age ++
  digest sicken die ;

: look ( -- )  0
  boredom @ 2 > if 1+ cr ." tama looks bored" then
  hygiene @ 5 < if 1+ cr ." tama could use a wash" then
  hunger @  0 > if 1+ cr ." tama's stomach is grumbling" then
  age @    20 > if 1+ cr ." tama is getting long in the tooth" then
  pooped @      if 1+ cr ." tama is disgusted by its own waste" then
  0= if cr ." tama looks fine" then ;

: feed ( -- )
  hunger @ 0= if cr ." tama bats the offered food away" exit then
  cr ." tama happily devours the offered food"
  hunger off  5 digestion ! ;

: clean ( -- )
  pooped @ 0= if cr ." tama is clean enough already." exit then
  cr ." You dispose of the mess."  pooped off  5 hygiene ! ;

: play ( -- )
  boredom @ 0= if cr ." tama ignores you." exit then
  cr ." tama plays with you for a while."  boredom off ;

( game mode )
\ this just permanently sets the current object
\ a more complex game would use >o ... o> to set it
create pet  tama allot
pet to o

cr .( You have a pet tamagotchi!)
cr
cr .( commands:  WAIT LOOK FEED CLEAN PLAY)
cr  ( secret commands: SPAWN DUMP )
spawn look
cr

```


Boredom kills tama faster than anything else.


## Objeck

GUI based Tamagotchi that sleeps at night. Not cleaning up poop makes Tamagotchi sicker faster. Implementation has associated sound effects.


```objeck
use Game.SDL2;
use Collection;
use Game.Framework;

class Game {
  @gotchi : Tamagotchi;

  enum Faces {
    BORED,
    POOP,
    HUNGRY,
    HAPPY,
    OK,
    SAD,
    SLEEP
  }

  enum DayTime {
    MORNING,
    EVENING,
    NIGHT,
    DAY
  }

  @framework : GameFramework;
  @quit : Bool;

  @faces : AnimatedImageSprite;
  @time_of_day : AnimatedImageSprite;

  @action_chunk : MixChunk;
  @sleep_chunk : MixChunk;
  @eat_chunk : MixChunk;
  @play_chunk : MixChunk;
  @clean_chunk : MixChunk;

  @age_text : TextSprite;
  @age : Int;

  @wait_mins : Int;

  New(wait : Int) {
    @framework := GameFramework->New(Meta->SCREEN_WIDTH, Meta->SCREEN_HEIGHT, "Tamagotchi");
    @framework->SetClearColor(Color->New(240, 248, 255));
    @wait_mins := wait * @framework->GetFps() * 60; # minutes
        
    @faces := @framework->AddAnimatedImageSprite("media/faces.png");
    @faces->AddClip(Rect->New(0, 0, 240, 160));
    @faces->AddClip(Rect->New(240, 0, 240, 160));
    @faces->AddClip(Rect->New(480, 0, 240, 160));
    @faces->AddClip(Rect->New(720, 0, 240, 160));
    @faces->AddClip(Rect->New(960, 0, 240, 160));
    @faces->AddClip(Rect->New(1200, 0, 240, 160));
    @faces->AddClip(Rect->New(1440, 0, 240, 160));

    @time_of_day := @framework->AddAnimatedImageSprite("media/tod.png");
    @time_of_day->AddClip(Rect->New(0, 0, 48, 48));
    @time_of_day->AddClip(Rect->New(48, 0, 48, 48));
    @time_of_day->AddClip(Rect->New(96, 0, 48, 48));
    @time_of_day->AddClip(Rect->New(144, 0, 48, 48));
    @time_of_day->SetScale(0.5);

    @action_chunk := @framework->AddMixChunk("media/action.wav");
    @sleep_chunk := @framework->AddMixChunk("media/sleep.wav");
    @eat_chunk := @framework->AddMixChunk("media/eat.wav");
    @play_chunk := @framework->AddMixChunk("media/play.wav");
    @clean_chunk := @framework->AddMixChunk("media/clean.wav");

    @age_text := @framework->AddTextSprite();
    @age_text->RenderedText("Age: 0");
  }

  function : Main(args : String[]) ~ Nil {
    wait : Int;
    if(args->Size() = 1) {
      wait := args[0]->ToInt();
      if(wait = 0) {
        wait := Meta->WAIT;
      };
    }
    else {
      wait := Meta->WAIT;
    };

    game := Game->New(wait);
    game->Run();
  }

  method : Run() ~ Nil {
    leaving {
      @framework->Quit();
    };

    if(@framework->IsOk()) {
      @gotchi := Tamagotchi->New(@self);
            
      e := @framework->GetEvent();
      count := 0;
      while(<>@quit & @gotchi->IsAlive()) {
        Start();
      
        Input(e);
        if(count = @gotchi->GetWait()) {
          @gotchi->Update();
          count := 0;
        };
        Draw();
        count += 1;

        End();
      };
    }
    else {
      "--- Error Initializing Game Environment ---"->ErrorLine();
      return;
    };
  }

  method : public : GetWait() ~ Int {
    return @wait_mins;
  }

  method : public : ActionSound() ~ Nil {
    @action_chunk->PlayChannel(-1, 0);
  }

  method : public : SleepSound() ~ Nil {
    @sleep_chunk->PlayChannel(-1, 0);
  }

  method : public : EatSound() ~ Nil {
    @eat_chunk->PlayChannel(-1, 0);
  }

  method : public : PlaySound() ~ Nil {
    @play_chunk->PlayChannel(-1, 0);
  }

  method : public : CleanSound() ~ Nil {
    @clean_chunk->PlayChannel(-1, 0);
  }

  method : Input(e : Event) ~ Nil {
    # process input
    while(e->Poll() <> 0) {
      if(e->GetType() = EventType->SDL_QUIT) {
        @quit := true;
      }
      else if(e->GetType() = EventType->SDL_KEYDOWN & e->GetKey()->GetRepeat() = 0) {
        select(e->GetKey()->GetKeysym()->GetScancode()) {
          label Scancode->SDL_SCANCODE_F: {
            @gotchi->Input('f');
          }

          label Scancode->SDL_SCANCODE_P: {
            @gotchi->Input('p');
          }

          label Scancode->SDL_SCANCODE_C: {
            @gotchi->Input('c');
          }
        };
      };
    };
  }

  method : public : Draw() ~ Nil {
    action := @gotchi->GetAction();
    neglect := @gotchi->GetNeglect();
    
    if(@gotchi->GetState() = Tamagotchi->States->SLEEP) {
      @faces->Render(0, 40, Faces->SLEEP);
    }  
    else if(action) {
      select(@gotchi->GetState()) {
        label Tamagotchi->States->HUNGRY: {
          @faces->Render(0, 40, Faces->HUNGRY);
        }

        label Tamagotchi->States->BORED: {
          @faces->Render(0, 40, Faces->BORED);
        }

        label Tamagotchi->States->POOP: {
          @faces->Render(0, 40, Faces->POOP);
        }
      };
    }
    else if(neglect < 1.0) {
      @faces->Render(0, 40, Faces->HAPPY);
    }
    else if(neglect < 2.0) {
      @faces->Render(0, 40, Faces->OK);
    }
    else {
      @faces->Render(0, 40, Faces->SAD);
    };

    age := @gotchi->GetAge();
    buffer := "Age: ";
    buffer += age;
    @age_text->RenderedText(buffer);
    @age_text->Render(10, 10);

    hour := @gotchi->GetHour();
    if(hour >= 6 & hour <= 10) {
      @time_of_day->Render(208, 10, DayTime->MORNING);
    }
    else if(hour >= 10 & hour <= 18) {
      @time_of_day->Render(208, 10, DayTime->DAY);
    }
    else if(hour >= 18 & hour <= 20) {
      @time_of_day->Render(208, 10, DayTime->EVENING);
    }
    else {
      @time_of_day->Render(208, 10, DayTime->NIGHT);
    };
  }

  method : Start() ~ Nil {
    @framework->FrameStart();
    @framework->Clear();
  }

  method : End() ~ Nil {
    @framework->Show();
    @framework->FrameEnd();
  }
}

class Tamagotchi  {
  @state : Int;
  @age : Int;
  @hour : Int;
  @neglect : Float;
  @game : Game;
  @action : Bool;
  @wait_mins : Int;

  enum States {
    HUNGRY, 
    BORED,
    POOP,
    SLEEP
  }

  New(game : Game) {
    @game := game;
    @hour := Int->Random(24);
    Update();
  }

  method : public : GetHour() ~ Int {
    return @hour;
  }

  method : public : GetWait() ~ Int {
    return @wait_mins;
  }

  method : public : GetAge() ~ Int {
    return @age;
  }

  method : public : GetAction() ~ Bool {
    return @action;
  }

  method : public : GetState() ~ Int {  
    return @state;
  }

  method : public : GetNeglect() ~ Float {  
    return @neglect;
  }

  method : public : Update() ~ Nil {
    NextState();
    NextHour();  
  }

  method : public : IsAlive() ~ Bool {
    return @age < 4 & @neglect < 3.0;
  }

  method : public : Input(action : Char) ~ Nil {
    select(action) {
      label 'f': {
        if(@state = States->HUNGRY) {
          @neglect -= .6;
          @game->EatSound();
        };
        @action := false;
      }

      label 'p': {
        if(@state = States->BORED) {
          @neglect -= .35;
          @game->PlaySound();
        };
        @action := false;
      }

      label 'c': {
        if(@state = States->POOP) {
          @neglect -= .85;
          @game->CleanSound();
        };
        @action := false;
      }
    };
  }

  method : NextState() ~ Nil {
    @state := Int->Random(States->SLEEP);
    if(<>IsAwake() | @state = States->SLEEP) {
      @state := States->SLEEP;
      @neglect -= .1;
      @action := false;

      @game->SleepSound();
    }
    else {
      select(@state) {
        label States->HUNGRY: {
          @neglect += .5;
          @action := true;
        }

        label States->BORED: {
          @neglect += .25;
          @action := true;
        }

        label States->POOP: {
          @neglect += .75;
          @action := true;
        }
      };

      @game->ActionSound();
    };

    if(@neglect < 0.0) {
      @neglect := 0.0;
    };
    # "hour={$@hour}, neglect={$@neglect}"->PrintLine();
  }

  method : IsAwake() ~ Bool {
    return @hour > 7 & @hour < 23;
  }

  method : NextHour() ~ Nil {
    @hour += 1;
    if(@hour = 24) {
      @hour := 0;
      @age += 1;
    };
    wait := @game->GetWait();
    @wait_mins := Int->Random(wait - wait / 3, wait + wait / 3);
  }
}

consts Meta {
  SCREEN_WIDTH := 240,
  SCREEN_HEIGHT := 200,
  WAIT := 5
}

```

