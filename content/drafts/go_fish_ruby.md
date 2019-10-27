+++
title = "Go Fish/Ruby"
description = ""
date = 2010-01-26T06:56:55Z
aliases = []
[extra]
id = 5231
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}


```ruby
class Card
  RANKS = %w(2 3 4 5 6 7 8 9 10 J Q K A)
  SUITS = %w(C D H S)

  def initialize(rank, suit)
    @rank = rank
    @suit = suit
  end
  attr_reader :rank, :suit

  def <=>(other)
    # this ordering sorts first by rank, then by suit
    (RANKS.find_index(self.rank) <=> RANKS.find_index(other.rank)).nonzero? ||
    (SUITS.find_index(self.suit) <=> SUITS.find_index(other.suit))
  end

  def to_s
    @rank + @suit
  end
end

#######################################################################
class Deck
  def initialize
    @deck = []
    Card::SUITS.each do |suit|
      Card::RANKS.each do |rank|
        @deck << Card.new(rank, suit)
      end
    end
    @deck.shuffle!
  end
  attr_reader :deck

  # returns an array of cards, even for dealing just 1 card
  def deal(n=1)
    @deck.pop(n)
  end

  def empty?
    @deck.empty?
  end

  def cards_remaining
    @deck.length
  end
end

#######################################################################
class Player
  def initialize(game)
    @hand = {}
    @books = []
    @game = game
    @opponents_hand = {
      :known_to_have => [],
      :known_not_to_have => [],
    }
  end
  attr_reader :name

  def take_cards(cards)
    my_cards = @hand.values.flatten.concat(cards)
    @hand = my_cards.group_by {|card| card.rank}

    # look for, and remove, any books
    @hand.each do |rank, cards|
      if cards.length == 4
        puts "#@name made a book of #{rank}"
        @books << rank
        @hand.delete(rank)
      end
    end
    if @hand.empty? and not @game.deck.empty?
      @game.deal(self, 1)
    end
  end

  def num_books
    @books.length
  end

  # return true if the next turn is still mine
  # return false if the next turn is my opponent's
  def query(opponent)
    wanted = wanted_card
    puts "#@name: Do you have a #{wanted}?"
    received = opponent.answer(wanted)
    @opponents_hand[:known_to_have].delete(wanted)
    if received.empty?
      @game.deal(self, 1)
      # by my next turn, opponent will have been dealt a card
      # so I cannot know what he does not have.
      @opponents_hand[:known_not_to_have] = []
      false
    else
      take_cards(received)
      @opponents_hand[:known_not_to_have].push(wanted).uniq!
      true
    end
  end

  def answer(rank)
    cards = []
    @opponents_hand[:known_to_have].push(rank).uniq!
    if not @hand[rank]
      puts "#@name: Go Fish!"
    else
      cards = @hand[rank]
      @hand.delete(rank)
      puts "#@name: Here you go -- #{cards.join(', ')}"
      @game.deal(self, 1) if @hand.empty?
    end
    cards
  end

  def print_hand
    puts "hand for #@name:"
    puts "  hand: "+ @hand.values.flatten.sort.join(', ')
    puts "  books: "+ @books.join(', ')
    puts "opponent is known to have: " + @opponents_hand[:known_to_have].sort.join(', ')
  end
end

#######################################################################
class ComputerPlayer < Player
  def initialize(game)
    super
    @name = 'Computer'
  end

  def wanted_card
    known = @hand.keys & @opponents_hand[:known_to_have]
    if not known.empty?
      sort_cards_by_most(known).first
    else
      possibilities = @hand.keys - @opponents_hand[:known_not_to_have]
      if not possibilities.empty?
        possibilities.shuffle.first
      else
        #sort_cards_by_most(@hand.keys).first
        @hand.keys.shuffle.first
      end
    end
  end

  # sort ranks by ones with most cards in my hand.  better chance to make a book
  def sort_cards_by_most(array_of_ranks)
    array_of_ranks.sort_by {|rank| -@hand[rank].length}
  end
end

#######################################################################
class HumanPlayer < Player
  def initialize(game)
    super
    @name = 'Human'
  end

  def take_cards(cards)
    puts "#@name received: #{cards.join(', ')}"
    super
  end

  def wanted_card
    print_hand
    wanted = nil
    loop do
      print "\nWhat rank to ask for? "
      wanted = $stdin.gets
      wanted.strip!.upcase!
      if not Card::RANKS.include?(wanted)
        puts "not a valid rank: #{wanted} -- try again."
      elsif not @hand.has_key?(wanted)
        puts "you don't have a #{wanted} -- try again"
      else
        break
      end
    end
    wanted
  end
end

#######################################################################
class GoFishGame
  def initialize
    @deck = Deck.new
    @players = [HumanPlayer.new(self), ComputerPlayer.new(self)]
    rotate_players if rand(2) == 1
    @players.each {|p| deal(p, 9)}
  end
  attr_reader :deck

  def start
    loop do
      p1, p2 = @players
      # p1.query(p2) method returns true if p1 keeps his turn
      # and returns false otherwise
      p1.query(p2) or rotate_players
      break if p1.num_books + p2.num_books == 13
    end
    puts "
### ========================
" # add a separator between turns
    puts "Game over"
    @players.each {|p| puts "#{p.name} has #{p.num_books} books"}
    nil
  end

  def rotate_players
    @players.push(@players.shift)
    puts "------------------------------" # add a separator between turns
  end

  def deal(player, n=1)
    n = [n, @deck.cards_remaining].min
    puts "Dealer: #{n} card(s) to #{player.name}"
    player.take_cards(@deck.deal(n))
  end
end

#######################################################################
# main

srand
GoFishGame.new.start
```

