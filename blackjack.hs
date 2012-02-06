import System.Random

data Card = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King deriving Show

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

