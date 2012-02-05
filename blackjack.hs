import System.Random

data Card = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King deriving Show

dealOneCard [] = error "No cards left in deck"
dealOneCard deck = do
  randomCardIndex <- randomRIO (0, length deck - 1)
  return (deck !! randomCardIndex)