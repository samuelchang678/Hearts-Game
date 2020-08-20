{- My players first check if its leading the trick or not, if it is leading the trick it checks wether this is the first trick of the game . If it is the first trick then he will find the clubs of two and play it.
 If my player is leading a trick but not the first if checks if hearts is broken if it is it will pick the lowest card among all 4 suits if hearts is not broke it will get the lowest of the 3 suits(excluding hearts)
 if it can follows a suit then :            
    it will try to pick the largest card that is smaller than the  largest card in the trick             
    if not it will pick the smallest card that is larger than the largest card in the trick
if it cant follow a suit then :
    1)if it can play the Queen of Spades then it will pick the Queen of spades
    2)if it can play Heart it will pick the largest card of Heart
    3)otherwise if will pick the largest card 
The main tatic is to get rid of the point card as soon as possible and try not to lead a trick in hopes of lowering the risk of getting point card towards the end of the game 
-}
module Player (
    playCard,
    makeBid
)
where

import Cards
import Data.Maybe
import Data.List
import Hearts.Types
-- import Data.List(sortBy)
-- import Data.Function(on)    

-- decides weather the player is leading the trick and if so whether if it is the first trick of the game or not 
playCard :: PlayFunc
playCard _ hand [] previous
    -- if this is the first trick of the game the lead
    | previous == Nothing =lead hand 
    -- if this is not the first trick then go to lead4
    | otherwise = lead4 hand previous
-- if the player is to follow a trick
playCard _ hand trick previous 
    -- if this is the first trick then(previous == Nothing) then u cant play point cards
    | previous == Nothing = firsttrick (checkforlargest (suit (fst (last trick))) trick) hand
    |otherwise = renege ( checkforlargest (suit(fst  (last trick))) trick ) hand

-- function to see of player can follow the current trick if not then pick point cards
renege :: Card -> [Card] -> (Card, String)
renege leader hand
    | filter ((suit leader ==) . suit) hand /= [] = ( highestpossible leader (filter ((suit leader ==) . suit) hand),"")
    | otherwise = pickqueenorhearts hand

--  chooses to lead the first trick of the game , find it card Club Two  is in deck , if is it play it 
lead :: [Card]  ->(Card,String)
lead hand = select (find (== (Card Club Two)) hand) where

    select :: Maybe Card -> (Card,String)
    select Nothing = filterbysuit Heart hand 
    select card = (fromJust card,"")

--  function that choose the highest possible rank card of that lead suit but still smaller than the largest card
highestpossible ::Card-> [Card] -> Card
highestpossible leader hand 
    | filter ((< rank leader) . rank) hand == [] = getlowestrank hand
    | otherwise  = gethighestrank (filter ((< rank leader) . rank) hand)

--  if player is leading the second trick or so forth, it check if heart is broken if it is then it will pick the lowest
--  of all 4 suits if not then just 3 suits
lead4 :: [Card]->  Maybe ([(Card, PlayerId)], String)-> (Card,String)
lead4 hand previous
    | snd (fromJust previous) == "H" = (getlowestrank hand,"H")
    | filter ((Heart /=) . suit) hand== [] =( getlowestrank hand ,"H")
    | otherwise =(getlowestrank (filter ((Heart /=) . suit) hand) ,"")

-- if the player cannot follow the first trick he will choose the highest card tht is not a point Card
firsttrick :: Card -> [Card] -> (Card,String)
firsttrick card hand
    | filter ((suit card ==) . suit) hand == [] = (gethighestrank (filter (Card Spade Queen /=) (filter ((Heart /=) . suit) hand)), "")
    | otherwise                                =  (highestpossible card (filter ((suit card ==) . suit) hand),"")

--  given a list of card played in the current trick it will return the largest card 
checkforlargest:: Suit-> [(Card, PlayerId)] -> Card
checkforlargest leadingsuit trick = gethighestfromtrick (filter ((leadingsuit ==) . suit . fst) trick)

-- fucntion in which it check the deck for the Queen of Hearts , if present then play it . If not then choose highest card of Heart
--  if both conditions are not met it will just pick the largest card in the deck 
pickqueenorhearts:: [Card]->(Card,String)
pickqueenorhearts deck
    | (Card Spade Queen) `elem` deck =( head (filter (Card Spade Queen ==) deck),"")
    | filter(\x->suit x /= Heart) deck ==[] = (gethighestrank (filter ((Heart ==) . suit) deck) , "H")
    | otherwise =(gethighestrank deck,"")

-- filter a deck of cards by the suit 
filterbysuit :: Suit ->[Card] ->(Card,String)
filterbysuit suits hand     
    | filter ((suits /=) . suit) hand == [] = (gethighestrank $ filter ((suits ==) . suit) hand,"")
    | otherwise                               =  (getlowestrank $ filter ((suits /=) . suit) hand,"")



-- get the largest card with the leading suit 
gethighestfromtrick :: [(Card, PlayerId)] -> Card
gethighestfromtrick hand =maxtrick (head hand) (tail hand)


maxtrick:: (Card, PlayerId)->  [(Card, PlayerId)] -> Card
maxtrick current [] =  fst current
maxtrick current (c:cs)
    |  rank(fst current) > rank(fst c) = maxtrick current cs
    |  otherwise = maxtrick c cs

-- get the largest card of of a deck in terms of rank 
gethighestrank :: [Card]-> Card
gethighestrank hand = maxcard (head hand) (tail hand)

maxcard :: Card ->[Card] -> Card
maxcard current [] = current
maxcard current (c:cs)
    | rank current > rank c = maxcard current cs
    | otherwise = maxcard c cs

-- get the smallest card of of a deck in terms of rank 
getlowestrank :: [Card]-> Card
getlowestrank hand = mincard (head hand) (tail hand)

mincard :: Card ->[Card] -> Card
mincard current [] = current
mincard current (c:cs)
    | rank current < rank c = mincard current cs
    | otherwise = mincard c cs
-- returns suit of a Card
suit :: Card -> Suit
suit (Card s _) = s

--  returns rank of a Card
rank :: Card -> Rank
rank (Card _ r) = r


-- | Not used, do not remove.filter(\x -> suit x /=suits) hand 
makeBid :: BidFunc
makeBid = undefined
