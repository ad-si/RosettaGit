+++
title = "Go Fish/Phix"
description = ""
date = 2018-09-21T01:15:12Z
aliases = []
[extra]
id = 22001
[taxonomies]
categories = []
tags = []
+++

{{trans|Ruby}}
{{collection|Go Fish}}

```Phix
constant show_non_human_hands = false

sequence players

enum NAME, HAND, BOOKS, KNOWN, SINCE, PLAYER=$

function new_player(string name)
    if name="" then
        name = prompt_string("What is your name?:")
--      puts(1,"What is your name?:pete\n")
--      name = "pete"
--      name = "computer"   -- plays itself
        if name!="" then
            name = lower(name)
            name[1] = upper(name[1])
        else
            name = "Computer"
        end if
    end if
    sequence player = repeat(0,PLAYER)
    player[NAME] = name
    player[HAND] = {}
    player[BOOKS] = ""
    player[KNOWN] = repeat(' ',13)
    player[SINCE] = repeat('2',13) -- (cards picked up since you last asked)
    -- aside: arguably, SINCE should be "999..99", but it would not make any
    --        difference, however 9 messes up debug info almost immediately,
    --        whereas 2s remain visually meaningful for 7 or so more rounds.
    return player
end function

constant ranks = "123456789EJQX",   -- ("1EX"->"ATK" in output)
         fish  = "A23456789TJQK",   -- (input version of above)
         suits = "CDHS",
         cards = {"Ace","Two","Three","Four","Five","Six","Seven",
                  "Eight","Nine","Ten","Jack","Queen","King"}

function map_card(integer card)
-- (input -> internal)
    integer k = find(card,"ATK")
    if k then card = "1EX"[k] end if
    return card
end function

function unmap_cards(string cards)
-- (internal -> display)
    return substitute_all(cards,"1EX","ATK")
end function

procedure show_cards(sequence cards)
    printf(1,"%s\n",{unmap_cards(join(cards))})
end procedure

sequence deck

function deal()
    string res = deck[$]
    deck = deck[1..-2]
    return res
end function

procedure check_empty_hand(integer player)
    if players[player][HAND]={}
    and deck!={} then
        sequence one = {deal()}
        string name = players[player][NAME]
        if show_non_human_hands
        or name!="Computer" then
            printf(1,"%s's hand is empty. Picks up the ",{name})
            show_cards(one)
        end if
        players[player][HAND] = one
    end if
end procedure

procedure check_for_books(integer player)
    sequence hand = sort(players[player][HAND])
    string books = players[player][BOOKS]
    integer i = length(hand)-3
    while i>=1 do
        if hand[i][1]=hand[i+3][1] then
            books &= hand[i][1]
            hand[i..i+3] = {}
            i -= 4
        else
            i -= 1
        end if
    end while
    players[player][HAND] = hand
    players[player][BOOKS] = books
end procedure

procedure take_cards(integer player, sequence cards, bool bDeal=true)
    players[player][HAND] &= cards
    check_for_books(player)
    if bDeal then
        check_empty_hand(player)
    end if
end procedure

function has_card(integer player, rank)
    sequence hand = players[player][HAND]
    for i=1 to length(hand) do
        if hand[i][1]=rank then return true end if
    end for
    return false
end function

function hand_over(integer player, rank)
    sequence cards = players[player][HAND]
    integer l = length(cards)
    for i=1 to l do
        if cards[i][1]=rank then
            for j=i+1 to l+1 do
                if j=l+1 or cards[j][1]!=rank then
                    cards = cards[i..j-1]
                    players[player][HAND][i..j-1] = {}
                    printf(1,"%s hands over ",{players[player][NAME]})
                    show_cards(cards)
                    check_empty_hand(player)
                    return cards
                end if
            end for
        end if
    end for
    return 9/0  -- should never trigger
end function

constant ESC=#1B

function wanted_card(integer player)
integer wanted,
        opponent = 3-player
string name = players[player][NAME],
       books = players[player][BOOKS],
       known = players[player][KNOWN],
       since = players[player][SINCE]
sequence hand = players[player][HAND]

    if show_non_human_hands
    or name!="Computer" then
        printf(1,"%s's hand: ",{name})
        show_cards(hand)
--      printf(1,"  books: %s\n",{unmap_cards(books)})
        printf(1,"  opponent is known to have: %s\n",{unmap_cards(known)})
        -- aside: this is not very clear: should probably space out any
        --  entries for known!=' ', and do better than ..789:;<=>?@..
        printf(1,"         and not have since: %s\n",{since})
    end if

    if name="Computer" then
        -- strategy: if known then take, 
        -- otherwise pick largest since
        -- (or try something else here)
        integer hs = 0, hw -- highest; hs==since[hw]
        for i=1 to length(hand) do
            integer wdx = find(hand[i][1],ranks)
            wanted = known[wdx]
            if wanted!=' ' then exit end if
            integer sw = since[wdx]
            if sw>hs then {hs,hw} = {sw,wdx} end if
        end for
        if wanted=' ' then wanted = ranks[hw] end if
    else
        printf(1,"\n")
        while 1 do
            printf(1,"What rank to ask for?:")
            while 1 do
                wanted = wait_key()
                if wanted=ESC then
                    abort(0)
                elsif wanted<#FF then -- ignore ctrl/shift etc
                    wanted = upper(wanted)
                    printf(1,"%c",wanted)
                    exit
                end if
            end while
            if not find(wanted,fish) then
                printf(1," not a valid rank -- try again.\n")
            else
                wanted = map_card(wanted)
                if has_card(player,wanted) then exit end if
                printf(1," you don't have any of those -- try again\n")
            end if
        end while
        printf(1,"\n")
    end if
    return wanted
end function

function query(integer player)
    integer wanted = wanted_card(player),
            wdx = find(wanted,ranks),
            opponent = 3-player
    string name = players[player][NAME],
           card = substitute(cards[wdx],"x","xe") -- (Sixs -> Sixes)

    -- we now (or soon will) know opponent no longer has that card
    players[player][SINCE][wdx] = '0'
    players[player][KNOWN][wdx] = ' '

    -- opponent now knows we have one or more of that card:
    players[opponent][KNOWN][wdx] = wanted
    players[opponent][SINCE][wdx] = '0'

    printf(1,"%s: Do you have any %ss?\n",{name,card})
    if not has_card(opponent,wanted) then
        printf(1,"%s: Go fish!\n",{players[opponent][NAME]})
        players[player][SINCE] = sq_add(players[player][SINCE],1)
        if deck!={} then
            sequence one = {deal()}
            if show_non_human_hands
            or name!="Computer" then
                printf(1,"%s picks up ",{name})
                show_cards(one)
            end if
            take_cards(player,one)
        end if
        return false
    end if
    sequence cards = hand_over(opponent,wanted)
    take_cards(player,cards)
    return true -- another go
end function

function gameover()
    return length(players[1][BOOKS])+length(players[2][BOOKS])=13
end function

procedure GoFishGame()
    deck = {}
    for rank=1 to length(ranks) do
        for suit=1 to length(suits) do
            string card = ""&ranks[rank]&suits[suit]
            deck = append(deck,card)
        end for
    end for
    deck = shuffle(deck)
    players = {new_player("Computer"),
               new_player("")}
    for i=1 to 9 do
        for p=1 to 2 do
            take_cards(p,{deal()},false)
        end for
    end for
    integer player = rand(2)
    while not gameover() do
        if not query(player) then
            player = 3-player
            puts(1,"--------------------------------------\n")
        end if
    end while
    puts(1,"Game over\n")
    for p=1 to 2 do
        integer l = length(players[p][BOOKS])
        string name = players[p][NAME],
                s = iff(l=1?"":"s")
        printf(1,"%s has %d book%s\n",{name,l,s})
    end for
end procedure
GoFishGame()
```

