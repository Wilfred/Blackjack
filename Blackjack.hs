import System.Random
import Data.List
import Text.Printf

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
cardValues Ace   = [1, 11]
cardValues Two   = [2]
cardValues Three = [3]
cardValues Four  = [4]
cardValues Five  = [5]
cardValues Six   = [6]
cardValues Seven = [7]
cardValues Eight = [8]
cardValues Nine  = [9]
cardValues _     = [10]

-- separate the first N cards in the deck from the rest of the deck
dealCards :: Int -> Deck -> (Hand, Deck)
dealCards number deck = (take number deck, drop number deck)

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

-- todo: Split
data Move = Hit | Stand | DoubleDown deriving (Show, Eq)

-- in Las Vegas, dealer hits on soft 17
dealerNextMove :: Hand -> Move
dealerNextMove hand
  | score < Value 17 = Hit
  | score == Value 17 = if handIsSoft hand then Hit else Stand
  | otherwise = Stand
  where score = handScore hand

-- very simple player for the time being
playerNextMove :: Hand -> Card -> Move
playerNextMove playerHand dealerVisibleCard
  | playerScore > Value 16 = Stand
  | playerScore > Value 12 && dealerScore < Value 7 = Stand
  | playerScore == Value 11 || playerScore == Value 10 = DoubleDown
  | otherwise = Hit
  where playerScore = handScore playerHand
        dealerScore = handScore [dealerVisibleCard]
        
-- since the money gained from winning with a blackjack hand is
-- different, we use two wins
data Outcome = Loss | Push | Win | BlackjackWin deriving (Show, Eq)

-- a Las Vegas casino generally only deals with whole numbers of dollars
type Money = Integer

-- calculate the money made in this hand
moneyMade :: Money -> Outcome -> Money
moneyMade bet Loss         = -1 * bet
moneyMade _   Push         = 0
moneyMade bet Win          = bet
moneyMade bet BlackjackWin = ceiling $ (1.5 :: Double) * fromIntegral bet

findOutcome :: Score Int -> Score Int -> Outcome
findOutcome Bust _ = Loss
findOutcome Blackjack _ = BlackjackWin
findOutcome _ Bust = Win
findOutcome playerScore dealerScore
  | playerScore > dealerScore = Win
  | playerScore == dealerScore = Push
  | otherwise = Loss
                
data GameState = PlayerPlaying | DealerPlaying

-- we pass the bet during the round too, since the bet can change
roundOutcome :: Money -> GameState -> Hand -> Hand -> Deck -> (Outcome, Money)
roundOutcome _ _ _ _ [] = error "Deck is empty!"
roundOutcome bet PlayerPlaying playerHand dealerHand (card:cards)
  | playerScore == Bust      = roundOutcome bet DealerPlaying playerHand dealerHand (card:cards)
  | playerMove == Stand      = roundOutcome bet DealerPlaying playerHand dealerHand (card:cards)
  | playerMove == Hit        = roundOutcome bet PlayerPlaying (card:playerHand) dealerHand cards
  | playerMove == DoubleDown = roundOutcome (2 * bet) DealerPlaying (card:playerHand) dealerHand cards
  where playerScore = handScore playerHand
        playerMove = playerNextMove playerHand (head dealerHand)
                           
roundOutcome bet DealerPlaying playerHand dealerHand (card:cards)
  | dealerScore == Bust = (findOutcome playerScore dealerScore, bet)
  | dealerMove == Hit   = roundOutcome bet DealerPlaying playerHand (card:dealerHand) cards
  | dealerMove == Stand = (findOutcome playerScore dealerScore, bet)
  where playerScore = handScore playerHand
        dealerScore = handScore dealerHand
        dealerMove = dealerNextMove dealerHand
        
roundTakings :: Money -> Hand -> Hand -> Deck -> Money
roundTakings bet playerHand dealerHand remainingDeck = moneyMade finalBet outcome
  where (outcome, finalBet) = roundOutcome bet PlayerPlaying playerHand dealerHand remainingDeck
      
-- play a game with the current strategy
playRound :: Money -> IO Money
playRound bet = do
  shuffledDeck <- shuffleDeck
  -- we don't deal cards in an alternating order, but it makes no difference
  let (playerHand, remainingDeck) = dealCards 2 shuffledDeck
      (dealerHand, remainingDeck') = dealCards 2 remainingDeck
      takings = roundTakings bet playerHand dealerHand remainingDeck'
  return $ takings

-- play a game N times and work out the overall takings/losses
play :: Integer -> Money -> IO Money
play 0 _ = return 0
play count bet = do
  -- get total for this round
  takings <- playRound bet
      
  -- recursively add up other rounds
  remainingTakings <- play (count - 1) bet
  
  return $ takings + remainingTakings
  
main = do
  let iterations = 10000 :: Integer
      bet = 10 :: Money
  takings <- play iterations bet :: IO Money
  let houseEdge = fromInteger (-1 * takings) / fromInteger (bet * iterations)
      housePercentage = 100 * houseEdge :: Double
  printf "After %d $%d hands, total money made was $%d (house made %.2f%%).\n"
    iterations bet takings housePercentage
