import System.Random
import Data.List

-- a blackjack simulator to measure effectiveness of tactics

data Card = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King deriving (Show, Eq, Enum)
type Hand = [Card]
type Deck = [Card]

fullDeck :: Deck
fullDeck = [Ace .. King] ++ [Ace .. King] ++ [Ace .. King] ++ [Ace .. King]

shuffleCards :: Deck -> Deck -> IO Deck
shuffleCards shuffled [] = return shuffled
shuffleCards shuffled unshuffled = do
  randomCardIndex <- randomRIO (0, length unshuffled - 1)
  let randomCard = unshuffled !! randomCardIndex
      unshuffledBefore = take randomCardIndex unshuffled
      unshuffledAfter = drop (randomCardIndex + 1) unshuffled
  
  shuffleCards (randomCard:shuffled) (unshuffledBefore ++ unshuffledAfter)

shuffleDeck :: IO Deck
shuffleDeck = shuffleCards [] fullDeck

cardValues :: Card -> [Int]
cardValues Ace = [1, 11]
cardValues Two = [2]
cardValues Three = [3]
cardValues Four = [4]
cardValues Five = [5]
cardValues Six = [6]
cardValues Seven = [7]
cardValues Eight = [8]
cardValues Nine = [9]
cardValues _ = [10]

-- blackjack is a hand of two cards, an ace and a ten/picture card
handIsBlackjack :: Hand -> Bool
handIsBlackjack [card1, card2] =
  ((card1 == Ace) && (elem card2 [Ten, Jack, Queen, King])) ||
  ((card2 == Ace) && (elem card1 [Ten, Jack, Queen, King]))
handIsBlackjack _ = False

handIsSoft :: Hand -> Bool
handIsSoft hand = Ace `elem` hand

-- work out the possible scores this hand could have. No concerns have
-- been given to efficiency here.
possibleHandTotals :: Hand -> [Int] -> [Int]
possibleHandTotals [] totals = sort $ nub totals
possibleHandTotals (card:cards) runningTotals =
  possibleHandTotals cards newTotals
  where newTotals = [total + value | total <- runningTotals, value <- cardValues card]

data Score x = Value Int | Blackjack | Bust deriving (Show, Ord, Eq)

handScore :: Hand -> Score Int
handScore hand =
  if notBustTotals == [] then Bust else
    if handIsBlackjack hand then Blackjack else Value (last notBustTotals)
  where notBustTotals = filter (<= 21) $ possibleHandTotals hand [0]

-- todo: DoubleDown, Split
data Move = Hit | Stand deriving Show

-- in Las Vegas, dealer hits on soft 17
dealerNextMove :: Hand -> Move
dealerNextMove hand
  | score < Value 17 = Hit
  | score == Value 17 = if handIsSoft hand then Hit else Stand
  | otherwise = Stand
  where score = handScore hand

-- very simple player for the time being
playerNextMove = dealerNextMove
