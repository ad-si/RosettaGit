+++
title = "Go Fish/Perl 6"
description = ""
date = 2019-05-17T03:28:48Z
aliases = []
[extra]
id = 5229
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}

{{works with|Rakudo|2019.03.1}}


```perl6
constant BOOKSIZE = 4;
constant HANDSIZE = 9;
constant @pips = <two three four five six seven eight nine ten jack queen king ace>;
  # The elements of @pips are only names. Pips are represented internally
  # as indices of this array.
constant @piparticles = <a a a a a a an a a a a a an>;
constant @ppips = <deuces threes fours fives sixes sevens eights nines tens jacks queens kings aces>;
constant @shortpips = <2 3 4 5 6 7 8 9 T J Q K A>;
constant $foe_nominative_pronoun = pick 1, <he she it e xe>;

sub count ($x, *@a) {
    my $n = 0;
    $_ eqv $x and ++$n for @a;
    return $n;
}

sub find ($x, *@a) {
    for @a.kv -> $k, $v {
        $v eq $x and return $k;
    }
    fail 'Not found';
}

sub maxes (&f, *@a) {
    my $x = max map &f, @a;
    return @a.grep: { f($^e) eqv $x };
}

sub ncard ($n, $pip) {
    $n > 1 ?? "$n {@ppips[$pip]}" !! "{@piparticles[$pip]} {@pips[$pip]}"
}

sub readpip (@user_hand) {
    my @choices = grep { @user_hand[$^p] }, ^@pips;
    if @choices == 1 {
        say "You're obliged to ask for { @ppips[@choices[0]] }.";
        return @choices[0];
    }
    loop {
        print 'For what do you ask? (', join(', ', @shortpips[@choices]), '): ';
        my $in = substr ($*IN.get.uc or next), 0, 1;

        my $pip = find $in, @shortpips;
        if defined $pip {
            @user_hand[$pip] and return $pip;
            say "You don't have any { @ppips[$pip] }.";
        }
        else {
            say 'No such rank.';
        }
    }
}

enum Maybe <No Yes Dunno>;

class Knowledge {
# The computer player has an instance of this class for each pip.
# Each instance tracks whether the computer thinks the user has at
# least one card of the corresponding pip.
    has Maybe $.maybe = Dunno;
      # Yes if the user definitely has this pip, No if they didn't
      # have it the last time we checked, Dunno if we haven't yet
      # checked.
    has Int $.n = 0;
      # If $.maybe is No, $.n counts how many cards the user
      # has drawn since we last checked.

    method set (Maybe $!maybe) { $!n = 0 }

    method incr { $.maybe == No and ++$!n }
}

class Player {
    has @.h;
      # @h[$n] is number of cards of pip $n in this player's hand.
    has $.deck;
      # A reference to whatever deck the player's playing with.
    has Int $.books = 0;
    has Bool $.cpu;
    has Knowledge @.know;

    method new ( $cpu, @deck ) {
        my @h = 0 xx @pips;
        ++@h[$_] for @deck[^HANDSIZE];
        @deck = @deck[HANDSIZE ..^ @deck];
        Player.bless(
            h => @h, cpu => $cpu,
            deck => @deck,
            know => ($cpu ?? map { Knowledge.new() }, @pips !! ())
        );
    }

    method showhand {
        say
            ($.cpu ?? 'The dealer has   ' !! 'You have   '),
            join('   ',
                map { join ' ', @shortpips[.key] xx .value },
                grep { .value },
                pairs @.h),
            '.';
    }

    method draw () {
        my $new = shift $.deck;
        $.cpu or print "You got { ncard 1, $new }. ";
        say "({ $.deck.elems or 'No' } card{ $.deck.elems == 1 ?? '' !! 's' } left.)";
        self.getcards(1, $new);
    }

    method getcards (Int $quantity, $pip) {
        @!h[$pip] += $quantity;
        @.h[$pip] == BOOKSIZE or return;
        ++$!books;
        say
            ($.cpu
              ?? "The dealer puts down a book of { @ppips[$pip] }"
              !! "That's a book"),
            " (for a total of $.books book{ $.books == 1 ?? '' !! 's' }).";
        self.losecards($pip);
    }

    method losecards ($pip) {
       @.h[$pip] = 0;
       while none @.h and $.deck.elems {
           say $.cpu
            ?? "The dealer's hand is empty, so $foe_nominative_pronoun draws a new card."
            !! "Your hand's empty, so you draw a new card.";
           self.draw;
       }
    }

    method learn ($pip, Maybe $m) { @.know[$pip].set($m) }

    method notice_draw () { .incr for @.know }

    method choose_request ()  {
        #self.showhand;
        #say 'Know: ', join ', ', map
        #   { .maybe ~~ Yes ?? 'Yes' !! .maybe ~~ Dunno ?? 'Dunno' !! .n },
        #   @.know;
        my @ps = map { .key }, grep { .value }, pairs @.h;

        return ( maxes { @.h[$^p] },
            # Most of all we should ask for cards we know the
            # user has.
            @ps.grep({ @.know[$^p].maybe ~~ Yes }).flat ||
            # Then try asking for one we haven't requested
            # before.
            @ps.grep({ @.know[$^p].maybe ~~ Dunno }).flat ||
            # Then try asking for one we least recently
            # asked about.
            maxes { @.know[$^p].n }, @ps ).roll;
    }
}

sub play () {

    my @deck;
    # Shuffle the deck until the first two hands contain no books.
    # (If BOOKSIZE is greater than 2 and HANDSIZE is reasonably
    # small, this'll probably take only one shuffle.)
    repeat { @deck = (flat ^@pips xx BOOKSIZE).pick(*) }
    until none(map { count $^x, @deck[^HANDSIZE] }, ^@pips) >= BOOKSIZE and
          none(map { count $^x, @deck[HANDSIZE ..^ 2*HANDSIZE] }, ^@pips) >= BOOKSIZE;

    my Player $user .= new(False, @deck);
    my Player $foe .= new(True, @deck);

    while any |$user.h or any |$foe.h {

        # The user goes first.
        while any |$user.h {
            say '';
            $user.showhand;
            my $request = readpip $user.h;
            $foe.learn($request, Yes);
            if $foe.h[$request] -> $quantity is copy {
                say 'The dealer reluctantly hands over ',
                    ncard($quantity, $request),
                    '.';
                $foe.losecards($request);
                $user.getcards($quantity, $request);
            }
            else {
                say '"Go fish!"';
                $user.draw;
                $foe.notice_draw;
                last;
            }
        }

        while any |$foe.h {
            my $request = $foe.choose_request;
            say "\n\"Got any ", @ppips[$request], '?"';
            $foe.learn($request, No);
            if $user.h[$request] -> $quantity is copy {
                say '"Thanks!"';
                $foe.getcards($quantity, $request);
                $user.losecards($request);
            }
            else {
                say 'The dealer goes fishing.';
                $foe.draw;
                last;
            }
        }

    }

    say "\nGame over!";
    say 'Your books: ', $user.books;
    say "The dealer's books: ", $foe.books;
    say do
        $user.books > $foe.books
     ?? 'A winner is you!'
     !! $user.books < $foe.books
     ?? 'Alas, you have lost.'
     # A draw is possible if @pips !% 2.
     !! "It's a draw.";

}

sub MAIN () { play }
```

