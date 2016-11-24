# HMarkov
Flexible Markov chain generation, Haskelly.

## How

    ghci> let p = buildProc (V.fromList [0,1,2,3,0,1,2,0,1]) 0 (mkStdGen 1234)
    ghci> runUntil (\m -> (V.last m) == 1) p
    ghci> [0,1]
    ghci> runUntil (\m -> (V.length m) == 4) p
    ghci> [0,1,2,3]

`buildProc` builds a `MarkovProcess` with a vector of training elements,
a starting element, and a `StdGen`. `runUntil` runs a `MarkovProcess` until a
termination condition is met.
Termination conditions are `m a -> Bool`, where `m a` is a `MonadPlus` of elements.


## Prior Art
[Data.MarkovChain](https://hackage.haskell.org/package/markov-chain-0.0.3.4/docs/Data-MarkovChain.html) is less involved but only lets you terminate on sequence length.


## &c.
Look, there aren't any tests, yet. I'm honestly intimidated by writing Haskell tests,
what with the QuickCheck and the properties and whatnot. I would like to get this
lil' guy up on Hackage, so tests are definitely coming. Eventually.


(c) 2016 Sam Raker, sam dot raker at gmail dot com. Please tell me how this is bad,
and how bad this is.

