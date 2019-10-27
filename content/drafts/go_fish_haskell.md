+++
title = "Go Fish/Haskell"
description = ""
date = 2010-01-07T12:57:26Z
aliases = []
[extra]
id = 5227
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}
If possible, the AI will randomly select a rank known to be in the human's hand (a card in the AI's hand that the human has asked for before and the AI hasn't asked for before). If there are no known ranks, a rank is randomly selected from the AI's hand.


```haskell
import Char
import IO
import Data.Map (Map)
import qualified Data.Map as M 
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Monad
import Random

data Player = Player String (GameState -> IO Rank)

type PlayerState = (Hand, Set Rank, Set Rank)

data GameState = GS [(Suit, Rank)] PlayerState PlayerState

type Hand = Map Rank (Set Suit)

data Suit = Diamond | Club | Heart | Spade
            deriving (Bounded, Enum, Eq, Ord, Show)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
            Jack | Queen | King | Ace
            deriving (Bounded, Enum, Eq, Ord, Show)

main =
    hSetBuffering stdout NoBuffering >>
    putStrLn "GO FISH\n\nDealing Cards" >>
    initialGameState >>=
    play (cycle [player, computer])

play ((Player name next):ps) state =
    putStrLn ('\n' : name ++ "'S TURN") >>
    runPlayer next state >>= \nextState ->
    (if isDone nextState
     then let (winnerName, ws, ls) = scoreGame name (head ps) nextState
          in putStrLn (winnerName ++ " WINS " ++ show ws ++ " TO " ++ show ls)
     else play ps $ swapPlayers nextState
    )

scoreGame name (Player opp _) (GS _ (_,books,_) (_,oppBooks,_)) =
    if score > oppScore then (name,score,oppScore) else (opp,oppScore,score)
    where (score, oppScore) = (S.size books, S.size oppBooks)

player = Player "PLAYER" playerBrain
playerBrain (GS _ (hand,_,_) _) =
    putStr "Your cards: " >>
    putStrLn (intercalate " " . map showCard $ handToCards hand) >>
    untilSuccess (liftM readRank rankPrompt) (putStrLn "Bad rank")
    where rankPrompt = putStr "Ask opponent for what rank? " >> getLine

computer = Player "COMPUTER" computerBrain
computerBrain (GS _ (hand,_,history) (_, _, oppHistory)) =
    liftM selectRank newStdGen >>= \rank ->
    putStrLn ("Do you have any " ++ show rank ++ "s?") >>
    return rank
    where knowns = S.difference (S.intersection guesses oppHistory) history
          guesses = M.keysSet hand
          ranks = S.toList $ if S.null knowns then guesses else knowns
          selectRank = (ranks !!) . fst . randomR (0, length ranks - 1)

runPlayer askRank state@(GS deck (hand, b, hi) o) =
    if M.null hand
    then if null deck
         then return $! state
         else putStrLn "Empty hand, forced draw" >>
              let (newHand, newDeck) = draw hand deck
              in normalizeBooks $ GS newDeck (newHand, b, hi) o
    else getValidRank askRank state >>= \rank ->
         exchangeCards state rank >>= \(newState, done) ->
         normalizeBooks newState >>=
         (if done then return else runPlayer askRank)

exchangeCards (GS deck (hand, b, hist) (opponentHand, ob, ohi)) rank =
    putStrLn m >> return (GS nd (nh, b, nhi) (noh, ob, ohi), done)
    where (m, nh, noh, nd, done) = worker $ M.lookup rank opponentHand
          nhi = S.insert rank hist
          worker Nothing = ("Go fish", newHand, opponentHand, newDeck, True)
              where (newHand, newDeck) = draw hand deck
          worker (Just suits) = (message, newHand, newOppHand, deck, False)
              where message = show (S.size suits) ++ " " ++ show rank ++ "(s)"
                    newHand = M.adjust (S.union suits) rank hand
                    newOppHand = M.delete rank opponentHand

getValidRank askRank state@(GS _ (hand,_,_) _) = untilSuccess
    (liftM (check hand) $ askRank state) $ putStrLn "Rank not in hand"
    where check m v = M.lookup v m >>= Just . const v

normalizeBooks (GS d (hand, books, hi) o) =
    mapM_ printRank (M.keys newBookRanks) >>
    (return $! GS d (newHand, newBooks, hi) o)
    where (newBookRanks, newHand) = M.partition ((==4) . S.size) hand
          newBooks = S.union books $ M.keysSet newBookRanks
          printRank r = putStrLn ("Rank " ++ show r ++ " was booked")

swapPlayers (GS d p1 p2) = GS d p2 p1

isDone (GS deck (hand,_,_) (oppHand,_,_)) =
    and [M.null hand, M.null oppHand, null deck]

initialGameState = liftM worker newStdGen
    where worker gen =
              GS deck (hand1, S.empty, S.empty) (hand2, S.empty, S.empty)
              where (startDeck, _) = shuffle initialDeck gen
                    (hand1, deckMinusPlayerHand) = drawN 9 M.empty startDeck
                    (hand2, deck) = drawN 9 M.empty deckMinusPlayerHand

untilSuccess action onFailure = worker
    where worker = action >>= \result -> case result of
              Nothing -> onFailure >> worker
              Just value -> return $! value

readRank [x] | isDigit x && x > '1' = Just $ toEnum (fromEnum x - 50)
readRank x@[_,_] | x == "10" = Just Ten
readRank [x] = case toLower x of
    'j' -> Just Jack
    'q' -> Just Queen
    'k' -> Just King
    'a' -> Just Ace
    _   -> Nothing
readRank _ = Nothing

showCard (suit, rank) = r ++ front suit
    where r = if rank > Ten then front rank else show (fromEnum rank + 2)
          front v = [head $ show v]

initialDeck = liftM2 (,) [Diamond .. Spade] [Two .. Ace]

shuffle deck gen = worker gen (length deck) [] deck
    where worker g _ xs [] = (xs, g)
          worker g l xs ys = worker newGen (l-1) (card : xs) (delete card ys)
              where (index, newGen) = randomR (0,l-1) g
                    card = ys !! index

draw hand ((s,r):deck) = (M.insertWith S.union r (S.singleton s) hand, deck)

drawN n hand deck = iterate (uncurry draw) (hand, deck) !! (n-1)

handToCards = concatMap (\(r,ss) -> map (flip (,) r) $ S.toList ss) . M.assocs
```

