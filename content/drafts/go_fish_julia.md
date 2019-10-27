+++
title = "Go Fish/Julia"
description = ""
date = 2019-02-04T02:33:12Z
aliases = []
[extra]
id = 22164
[taxonomies]
categories = []
tags = []
+++


```julia
import Base.show

const sorteddeck = reshape([string(r == 'T' ? "10" : r) * s for r in "23456789TJQKA", s in "♣♦♥♠"], 52)

isvaliddeck(a) = (s = Vector{UInt8}(unique(a)); (a == s && all(x -> 0 < x <= 52, s)))

mutable struct Deck
    cards::Vector{UInt8}
    Deck(a) = if isvaliddeck(a) new(a) else throw("bad deck constructor $a") end
end

suitrank(crd) = divrem((crd - 1), 13) .+ 1
cardsuit(crd) = suitrank(crd)[1]
cardrank(crd) = suitrank(crd)[2]
shuffledeck!(d) = shuffle!(d.cards)

ascards(d::Deck) = [sorteddeck[i] for i in d.cards]
Base.show(io::IO, d::Deck) = show(io, join(ascards(d), " "))

byrank(d::Deck) = filter(!isempty, [filter(x -> x in n:13:52, d.cards) for n in 1:13])
printbyrank(d) = foreach(x -> println(map(y -> sorteddeck[y], x)), byrank(d))

function deal!(d::Deck, hlen)
    if hlen < length(d.cards)
        hand = Deck(d.cards[1:hlen])
        d.cards = d.cards[hlen+1:end]
    else
        hand = Deck(d.cards)
        empty!(d.cards)
    end
    return hand
end

function queryprompt(query, choices, choicetxt="")
    carr = map(x -> uppercase(strip(string(x))), collect(choices))
    while true
        print(query, " ", choicetxt == "" ? carr : choicetxt, ": ")
        choice = uppercase(strip(readline(stdin)))
        if choice in carr
            return choice
        end
        println()
    end
end

function gofish()
    ranks = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]
    displayplayer(d, l, cl) = (println("Player hand is"), printbyrank(d),
        println(".\nYour score is $(length(l)) books. Computer score is $(length(cl)) books."))
    discardfrom!(a, d) = (dis = Deck(a); d.cards = filter(x -> !(x in a), d.cards); dis)
    books!(d, l) = (b = [discardfrom!(a, d) for a in byrank(d) if length(a) == 4]; append!(l, b); b)

    function cardswithrank(rankstring, d)
        r = indexin([rankstring], ranks)[1]
        if r == nothing
            return []::Vector{Int}
        else
            arr = findall(x -> cardrank(x) == r, d.cards)
        end
        d.cards[arr]
    end

    function moverank!(rnk, srcdeck, dstdeck)
        arr = cardswithrank(rnk, srcdeck)
        srcdeck.cards = sort(filter(x -> !(x in arr), srcdeck.cards))
        dstdeck.cards = sort(unique(append!(dstdeck.cards, arr)))
    end

    function drawacard!(d, maindeck, display=true)
        if isempty(maindeck.cards)
            println("Draw pile is empty.")
            return
        end
        crd = popfirst!(maindeck.cards)
        d.cards = sort(push!(d.cards, crd))
        if display
            println("You drew a ", Deck([crd]))
        end
    end

    function playerasks!(d, plaid, claid, cdeck, maindeck)
        choice = ""
        while true
            if isempty(cdeck.cards)
                drawacard!(cdeck, maindeck, false)
            end
            if isempty(d.cards)
                drawacard!(d, maindeck)
            end
            if isempty(d.cards)
                return
            end
            displayplayer(d, plaid, claid)
            choice = queryprompt("Your turn. Rank you request? ", ranks)
            if isempty(cardswithrank(choice, d))
                println("You cannot ask for \"$choice\" cards since you have none.")
                continue
            end
            println("You want the \"$choice\" cards.")
            if (n = length(cardswithrank(choice, cdeck))) > 0
                moverank!(choice, cdeck, d)
                println("Computer gives you $n of the \"$choice\" cards.")
                choice = queryprompt("Ask computer for cards again? ", ["Y", "N"])
                if choice == "N"
                    break
                end
            else
                println("Sorry, computer does not have any of those cards.")
                break
            end
        end
    end

    function playerdiscards!(pd, plaid)
        choice = queryprompt("Discard cards? ", ["Y","N"])
        if choice == "Y"
            bks = books!(pd, plaid)
            println(length(bks) > 0 ? "Books: $(join(map(ascards, bks), ", ")))" : "No new books.")
        end
    end

    function computerasks!(cdeck, claid, pdeck, maindeck, memory)
        choice = ""
        while true
            if isempty(pdeck.cards)
                drawacard!(pdeck, maindeck)
            end
            if isempty(cdeck.cards)
                drawacard!(pdeck, maindeck, false)
            end
            if isempty(cdeck.cards)
                return
            end
            rankpick = 0
            while true
                r = rand(cdeck.cards)
                rankpick = cardrank(r)
                if haskey(memory, rankpick) && memory[rankpick] > 0
                    memory[rankpick] -= 1
                    continue
                else
                    memory[rankpick] = 3
                    break
                end
            end
            choice = ranks[rankpick]
            println("Computer asks you for \"$choice\" cards.")
            if (n = length(cardswithrank(choice, pdeck))) > 0
                moverank!(choice, pdeck, cdeck)
                println("Computer accepts $n \"$choice\" cards from you.")
            else
                println("Computer cannot get those cards from you.")
                return
            end
        end
    end

    function computerdiscards!(cdec, claid)
        bks = books!(cdec, claid)
        if length(bks) > 0
            println("Computer new books: ", join(map(ascards, bks), ", "))
        end
    end

    fishd = Deck(shuffle(collect(1:52)))
    println("------------GO FISH GAME------------\n\nDealing:")
    pdeck = deal!(fishd, 9)
    cdeck = deal!(fishd, 9)
    println("You are dealt $pdeck.")

    plaid = Vector{Deck}()
    claid = Vector{Deck}()
    memory= Dict{Int,Int}()

    while sum(length, [pdeck.cards, cdeck.cards, fishd.cards]) > 0
        playerasks!(pdeck, plaid, claid, cdeck, fishd)
        playerdiscards!(pdeck, plaid)
        drawacard!(pdeck, fishd)

        computerasks!(cdeck, claid, pdeck, fishd, memory)
        computerdiscards!(cdeck, claid)
        drawacard!(cdeck, fishd, false)
    end

    pscore = length(plaid)
    cscore = length(claid)
    println("Player has taken out $pscore books, and computer has taken out $cscore books.")
    println(pscore > cscore ? "Player wins." : pscore < cscore ? "Computer wins." : "Tied game.")
end

gofish()

```

