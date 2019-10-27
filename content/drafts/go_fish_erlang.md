+++
title = "Go Fish/Erlang"
description = ""
date = 2013-12-16T20:42:39Z
aliases = []
[extra]
id = 16916
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}
The computers strategy is to ask for the things the human player has asked for previously. Otherwise it asks for the pips that it has not asked for before. Finally it takes any legal choice.

I have not seen any other language include a sample of game play so I will not do that either. A pity, since I spent more time making the printouts usable then on any other thing in the code.

```Erlang

-module( go_fish ).

-export( [task/0] ).

-record( computer, {cards, books, asked_pips=[], player_asked_pips=[]} ).

task() ->
    {[Computers, Players], Deck} = playing_cards:deal( 2, 9, playing_cards:shuffle(playing_cards:deck()) ),
    {Computer_cards, Computer_books} = books( Computers, [] ),
    {Player_cards, Player_books} = books( Players, [] ),
    Starter = drawer_start( random:uniform() ),
    Result = loop( Starter, #computer{cards=Computer_cards, books=Computer_books}, {Player_cards, Player_books}, Deck ),
    a_result( Result ).



a_result( quit ) -> io:fwrite( "player quits.~n" );
a_result( {Computer, Player} ) when erlang:length(Computer) > erlang:length(Player) -> io:fwrite( "computer wins with ~p.~n", [pips(Computer)] );
a_result( {_Computer, Player} ) -> io:fwrite( "player wins with ~p.~n", [pips(Player)] ).

books( Cards, Books ) ->
    New_books = [Books_per_pip || {_Pip, Books_per_pip} <- lists:foldl( fun books_pips/2, [], Cards ), erlang:length(Books_per_pip) =:= 4],
    books_update( New_books, Cards, Books ).

books_pips( Card, Acc ) ->
	Pip = playing_cards:pip( Card ),
	Existing = proplists:get_value( Pip, Acc, [] ),
	[{Pip, [Card | Existing]} | proplists:delete(Pip, Acc)].

books_update( [], Cards, Books ) -> {Cards, Books};
books_update( [Book], Cards, Books ) -> {Cards -- Book, [Book | Books]}.

computer_pip_to_draw( #computer{cards=Computers_cards, asked_pips=Asked_pips, player_asked_pips=Player_pips} ) ->
	Computer_pips = pips( Computers_cards ),
	Pips = computer_pip_to_draw_select( [X || X <- Player_pips, lists:member(X, Computer_pips)] -- Asked_pips, Computer_pips -- Asked_pips, Computer_pips ),
	lists:nth( random:uniform(erlang:length(Pips)), Pips ).

computer_pip_to_draw_select( [], [], Pips ) -> Pips;
computer_pip_to_draw_select( [], Pips, _Computer_pips ) -> Pips;
computer_pip_to_draw_select( Pips, _Unused_pips, _Computer_pips ) -> Pips.

draw( _Drawer, quit, _Drawer_cards, _Drawee_cards, _Deck ) -> {[], [], [], quit};
draw( Drawer, Pip, Drawer_cards, Drawee_cards, Deck ) ->
    io:fwrite( "~p asks for ~p from ~p.~n", [Drawer, Pip, drawer_change(Drawer)] ),
    Draw_froms_with_correct_pip = [X || X <- Drawee_cards, playing_cards:pip(X) =:= Pip],
    draw_pips( Draw_froms_with_correct_pip, Drawer, Drawer_cards, Drawee_cards, Deck ).

draw_pips( [], Drawer, Drawer_cards, Drawee_cards, [Card | Deck] ) ->
    draw_pips_go_fish( Drawer, Card ),
    {drawer_change(Drawer), [Card | Drawer_cards], Drawee_cards, Deck};
draw_pips( Cards, Drawer, Drawer_cards, Drawee_cards, Deck ) ->
    io:fwrite( "~p got", [Drawer] ),
    playing_cards:print( Cards ),
    {Drawer, Drawer_cards ++ Cards, Drawee_cards -- Cards, Deck}.

draw_pips_go_fish( player, Card ) -> io:fwrite( "player go fish, got ~p.~n", [Card] );
draw_pips_go_fish( computer, _Card ) -> io:fwrite( "computer go fish~n" ).

drawer_change( computer ) -> player;
drawer_change( player ) -> computer.

drawer_start( R ) when R < 0.5 -> computer;
drawer_start( _R  ) -> player.


loop( _, _, _, quit ) -> quit;
loop( _Who, #computer{cards=[], books=Computers_books}, {[], Players_books}, _Deck ) -> {Computers_books, Players_books};
loop( Who, #computer{cards=[]}=Computer, Players, [Card | Deck] ) ->
    io:fwrite( "computer has no cards, got extra card.~n" ),
    loop( Who, Computer#computer{cards=[Card]}, Players, Deck );
loop( Who, Computer, {[], Players_books}, [Card | Deck] ) ->
    io:fwrite( "player has no cards, got extra card: ~p.~n", [Card] ),
    loop( Who, Computer, {[Card], Players_books}, Deck );
loop( computer, #computer{cards=Computers_cards, books=Computers_books, asked_pips=Pips}=Computer, {Players_cards, Players_books}, Deck ) ->
    io:fwrite( "~ncomputer books: ~p.~n", [pips(Computers_books)] ),
    Pip = computer_pip_to_draw( Computer ),
    {Who, New_computers, New_players, New_deck} = draw( computer, Pip, Computers_cards, Players_cards, Deck ),
    {Final_computers_cards, Final_computers_books} = books( New_computers, Computers_books ),
    loop( Who, Computer#computer{cards=Final_computers_cards, books=Final_computers_books, asked_pips=[Pip | Pips]}, {New_players, Players_books}, New_deck );
loop( player, #computer{cards=Computers_cards, books=Computers_books, player_asked_pips=Pips}, {Players_cards, Players_books}, Deck ) ->
    io:fwrite( "~nplayer books: ~p.~n", [pips(Players_books)] ),
    io:fwrite( "player cards: " ),
    playing_cards:print( playing_cards:sort_pips(Players_cards) ),
    Pip = player_pip_to_draw( false, ignore_this, pips(Players_cards) ),
    {Who, New_players, New_computers, New_deck} = draw( player, Pip, Players_cards, Computers_cards, Deck ),
    loop( Who, #computer{cards=New_computers, books=Computers_books, player_asked_pips=[Pip | lists:delete(Pip, Pips)]}, books(New_players, Players_books), New_deck ).

pips( Cards ) -> lists:usort( [playing_cards:pip(X) || X <- lists:flatten(Cards)] ).

player_pip_to_draw( _, "Quit", _Pips ) -> quit;
player_pip_to_draw( true, Pip, _Pips ) -> Pip;
player_pip_to_draw( false, _Pip, Pips ) ->
    {ok, [[C | T]]} = io:fread( "player should draw what pip from computer? ", "~s" ),
    Pip = string:to_upper( [C] ) ++ T,
    player_pip_to_draw( lists:member(Pip, Pips), Pip, Pips ).

```

