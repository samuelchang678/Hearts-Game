-- | Write a report describing your design and strategy here.
module NaivePlayer (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Cards
import Data.Maybe
import Data.List
import Hearts.Types
import Data.List(sortBy)
import Data.Function(on)    
playCard :: PlayFunc
playCard _ hand [] _ = (lead hand, "")
-- playCard _ hand trick Nothing = (head $ firsttrick (suit $ fst $ last trick) hand, "" )
playCard _ hand trick previous 
    |previous == Nothing = ( firsttrick ( fst $ last trick) hand, "")
    |otherwise =( renege ( fst $ last trick) hand, "")

renege :: Card -> [Card] -> Card
renege leader hand 
    | filter (\x -> suit x == suit leader) hand /= [] = highestpossible  (leader)  (filter (\x -> suit x == suit leader ) hand)
    | otherwise = pickqueenorhearts hand
    -- | filter ((suit leader ==) . suit) hand ==[] =  pickqueenorhearts hand
highestpossible ::Card-> [Card] -> Card
highestpossible leader hand 
    | filter(\x-> rank x < rank leader) hand== [] =  head hand
    | otherwise  = gethighestrank $  filter(\x-> rank x < rank leader) hand 
pickqueenorhearts:: [Card]->Card
pickqueenorhearts deck
    | (Card Spade Queen) `elem` deck = head $ filter(\x-> x == Card Spade Queen) deck 
    | filter(\x->suit x /= Heart) deck ==[] = gethighestrank $ filter(\x-> suit x == Heart) deck 
    | otherwise =gethighestrank deck
filterbysuit :: Suit ->[Card] -> Card
filterbysuit suits hand     
    | filter ((suits /=) . suit) hand == [] = gethighestrank $ filter ((suits ==) . suit) hand
    | otherwise                               =  getlowestrank $ filter ((suits /=) . suit) hand
firsttrick :: Card -> [Card] -> Card
firsttrick card hand
    | filter (\x -> suit x == suit card) hand == [] = getlowestrank $ filter ((Heart /=) . suit) hand
    | otherwise                                = getlowestrank $ filter ((suit card ==) . suit) hand
lead :: [Card] -> Card
lead hand = select (find (== (Card Club Two)) hand) where

    select :: Maybe Card -> Card
    select Nothing = filterbysuit Heart hand 
    select card = fromJust card


gethighestrank :: [Card]-> Card
gethighestrank hand = maxcard  (head(hand)) (tail(hand))

maxcard :: Card ->[Card] -> Card
maxcard current [] = current
maxcard current (c:cs)
    | rank current > rank c = maxcard current cs
    | otherwise = maxcard c cs

getlowestrank :: [Card]-> Card
getlowestrank hand = mincard  (head(hand)) (tail(hand))

mincard :: Card ->[Card] -> Card
mincard current [] = current
mincard current (c:cs)
    | rank current < rank c = mincard current cs
    | otherwise = mincard c cs

suit :: Card -> Suit
suit (Card s _) = s

rank :: Card -> Rank
rank (Card _ r) = r


-- | Not used, do not remove.filter(\x -> suit x /=suits) hand 
makeBid :: BidFunc
makeBid = undefined
-- head $ filter ((leader ==) . suit) hand
-- *BetterPlayer> updateMemo (Just([((Card Spade Queen),"
-- "SQ "
-- *BetterPlayer> updateMemo (Just([((Card Spade Queen),"
-- "SQH2 "
-- *BetterPlayer> updateMemo Nothing
-- ""
-- *BetterPlayer> memo = updateMemo (Just([((Card Spade Q
-- "H4SAC3"))
-- *BetterPlayer> memo
-- "SQH4SAC3"