# Blackjack

A blackjack simulator in Haskell. The `playerNextMove` function
implements the player's strategy, then the simulator runs many hands
to measure the strategy effectiveness.

GPLv2 license.

### Blackjack variation

This simulator plays one deck blackjack. The dealer always hits on
soft 17. Blackjack pays out 1.5x the original bet. Players may hit,
stand or double down (insurance isn't supported, but perfect strategy
shouldn't use it).

## Usage

    $ ghc Blackjack.hs
    $ ./Blackjack
    After 1000 $10 hands, total money made was $180 (house made -1.7999999999999998%).



