+++
title = "Go Fish/Java"
description = ""
date = 2011-11-24T05:33:12Z
aliases = []
[extra]
id = 9983
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}

{{works with|JDK|1.5+}}

I read everything on the [[Go Fish]] page before starting this implementation -- except for the part about [[Playing Cards]]. So no code from that was reused here, although there may be strong similarities. The class architecture and names , as you might have noticed, were stolen from the [[Go Fish/Ruby|Ruby version]] (GoFish was Deck until my storing all the classes in one GoFish file stopped that). Although I didn't look at the Java [[Playing Cards]] until after most of my coding was complete, as I said, I have a feeling my using enum was not an original thought, though I don't know where it came from. 

As I say in the comments, this implementation was made possible by the JCF and Java's enums. I don't know how I could have done it without them.


```Java
import java.util.ArrayList; //Java's Collection Framework is really what is
import java.util.Scanner;  //powering the whole game: don't underestimate it.
import java.util.Random;

public class GoFish
{
    static final Random rng = new Random();
    static private ArrayList<Card> cards;
    static public Player[] Players;

    public static Card draw()
	{
		return cards.remove(rng.nextInt(cards.size()));
	}

	public static int deckSize()
	{
		return cards.size();
    }

    public static void main(String[] args)
    {

        cards = new ArrayList<Card>();
        for(int i=0;i<4;i++)
            for(Card c: Card.values())
                cards.add(c);
        Player h = new HumanPlayer();
        Player ai = new AIPlayer();
        Players = new Player[] {h, ai};

        while(Players[0].getNumBooks() + Players[1].getNumBooks() < 13)
        {
            Players[0].haveTurn();
            System.out.println("----------");
            Players[1].haveTurn();
            System.out.println("----------");
        }

        int yScore = Players[0].getNumBooks(); int aiScore = Players[1].getNumBooks();
        if (yScore > aiScore)
            System.out.println("Congratulations, you win "+ yScore + " to "+ aiScore +"!");
        else if (aiScore > yScore)
            System.out.println("The terrible AI beat you "+ yScore + " to "+ aiScore +"...");
        else
            System.out.println("It's a tie at "+yScore+" each!");
    }
}

enum Card //Java's enums made this possible!
{
    ACE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, JACK, QUEEN, KING;
}

abstract class Player
{
    protected ArrayList<Card> hand = new ArrayList<Card>();
    private int numBooks;

    public Player()
    {
        for(int i=0;i<8;i++)
            fish();
    }

    public boolean hasGiven(Card cType)
    {
        return hand.contains(cType);
    }

    public ArrayList<Card> giveAll(Card cType)
    {
        ArrayList<Card> x = new ArrayList<Card>(); //Complicated because simply taking the cards as they
        for(int i=0;i<hand.size();i++)            //are found would mess up the traversing of the hand
            if (hand.get(i) == cType)
              x.add(hand.get(i));
        for(int c=0;c<x.size();c++)
            hand.remove(cType);
        return x;
    }

    protected boolean askFor(Card cType)
    {
        int tmp = 0;
        if (this instanceof HumanPlayer)
            tmp = 1;
        Player other = GoFish.Players[tmp];

        //Used for the computer's strategy//
        if (tmp==1)
            ((AIPlayer) other).queries.add(cType);
        //                               //

        if (other.hasGiven(cType))
        {
            for(Card c: other.giveAll(cType))
                hand.add(c);
            return true;
        }
        else
        {
            return false;
        }
    }

    protected void fish()
	    {
	        if (GoFish.deckSize() > 0)
	        	hand.add(GoFish.draw());
	        else
	        	System.out.println("But that's impossible since the deck is empty.");
    }

    public int getNumBooks()
    {
        return numBooks;
    }

    protected Card checkForBooks()
    {
        for(Card c: hand) //Not very elegant!
        {
            int num = 0;
            for(Card d: hand)
              if (c == d)
                  num++;
            if (num == 4)
            {
                for(int i=0;i<4;i++)
                    hand.remove(c);
                numBooks++;
                return c;
            }
        }
        return null;


    }

    public abstract void haveTurn();

}

class HumanPlayer extends Player
{
    public void haveTurn()
    {
        Scanner scn = new Scanner(System.in);
        boolean playing = true;
        do{
            Card book = checkForBooks();
            if(book != null)
                System.out.println("You got a book of " + book + "s!");

            if (hand.size() == 0)
            {
                System.out.print("Your hand is empty, you must "); //"Go fish!"
                break;
            }
            else
            {
                System.out.print("Your hand:");
                for(Card c: hand)
                    System.out.print(c + " ");
                System.out.println();
            }

            System.out.println("Ask opponent for what card?");

            Card req;
            try{
                req = Card.valueOf(scn.next().toUpperCase());
            }
            catch(IllegalArgumentException e){ //If what you said is not in Card
                System.out.println("Card not present in this deck. Try again:");
                continue;
            }

            if(!hand.contains(req))
            {
                System.out.println("You may not ask for a card you have none of. Try again:");
                continue;
            }

            System.out.println("You ask for a " + req);
            playing = askFor(req); //If you get card(s), askFor returns true and loops
        } while(playing);
        System.out.println("Go fish!");
        fish();
    }
}

class AIPlayer extends Player
{
    public ArrayList<Card> queries = new ArrayList<Card>();
    private int age = 0;

    public void haveTurn()
    {
        boolean playing;
        do{
            Card book = checkForBooks();
            if(book != null)
                System.out.println("Your opponent got a book of " + book + "s...");
            if (hand.size() == 0)
            {
                System.out.print("Your opponent's hand is empty.");
                break;
            }
            Card req = aiMagic();
            System.out.println("Your opponent asks for cards by the name of " + req);
            playing = askFor(req);
            age++;
        } while(playing);
        System.out.println("Your opponent goes fishing.");
        fish();
    }

	//The AI's strategy is to first ask for things you asked for, since you have those things.
	//Failing that, it chooses at random.
    private Card aiMagic()
    {
        if (age>2)
        {
            queries.remove(queries.size()-1); //gets rid of oldest entry so it won't 
            age=0;                           //get stuck asking for the same thing
        }
        for(int i=queries.size()-1; i>-1; i--) //(maybe a queue would have been better?)
            if (hand.contains(queries.get(i)))
            {
                return queries.remove(i); //once it extracts a card from you, it will stop
            }                            //asking for that card, until YOU ask for it again.
        return hand.get(GoFish.rng.nextInt(hand.size()));
    }
}
```

