import System.Random
import Data.List

data Card = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King deriving (Show, Eq)
type Hand = [Card]

dealOneCard :: [Card] -> IO Card
dealOneCard [] = error "No cards left in deck"
dealOneCard deck = do
  randomCardIndex <- randomRIO (0, length deck - 1)
  return (deck !! randomCardIndex)
  
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

-- work out the possible scores this hand could have. No concerns have
-- been given to efficiency here.
possibleHandTotals :: Hand -> [Int] -> [Int]
possibleHandTotals [] totals = sort $ nub totals
possibleHandTotals (card:cards) runningTotals =
  possibleHandTotals cards newTotals
  where newTotals = [total + value | total <- runningTotals, value <- cardValues card]
