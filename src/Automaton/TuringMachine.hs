{-|
    Module      : Automaton.TuringMachine
    Description : Turing Machines
    Copyright   : (c) Jose Antonio Riaza Valverde, 2016-2021
    License     : MIT license
    Maintainer  : riazavalverde@gmail.com
    Stability   : experimental
    Portability : portable
    Website     : https://jariaza.es
    GitHub      : https://github.com/jariazavalverde/hackage-turing-machines

    A Turing machine is an abstract machine that manipulates symbols
    on a strip of tape according to a table of rules.
-}

module Automaton.TuringMachine (
    TuringMachine(..),
    Transition,
    Head(..),
    Tape,
    mkTuringMachine,
    (>>>)
) where

import Data.Maybe(isNothing)

-- | Head
data Head s = ShiftLeft | ShiftRight | Write s deriving (Show, Read, Ord, Eq)

-- | Tape
type Tape s = ([s], [s])

-- | Transition function
--   (state, symbol) => (state', head)
type Transition q s = (q, s) -> Maybe (q, Head s)

-- | Data structure for Turing machines
data TuringMachine q s = TuringMachine {
    state :: q,
    tape :: Tape s,
    blankSymbol :: s,
    finalStates :: [q],
    transition :: Transition q s
}

-- | Makes a Turing machine
mkTuringMachine :: q -> [q] -> s -> Transition q s -> TuringMachine q s
mkTuringMachine q qs b delta = TuringMachine{
    state = q,
    finalStates = qs,
    blankSymbol = b,
    transition = delta,
    tape = ([], [])
}

-- | Reads the cell of the tape
readTape :: TuringMachine q s -> s
readTape TuringMachine{tape = (_, []), blankSymbol = b} = b
readTape TuringMachine{tape = (_, s:_)} = s

-- | Writes the cell of the tape
writeTape :: s -> TuringMachine q s -> TuringMachine q s
writeTape s m@TuringMachine{tape = (ls, [])} = m{tape = (ls, [s])}
writeTape s m@TuringMachine{tape = (ls, _:rs)} = m{tape = (ls, s:rs)}

-- | Moves the tape to the left
shiftLeftTape :: TuringMachine q s -> TuringMachine q s
shiftLeftTape m@TuringMachine{tape = ([], rs)} = m{tape = ([], blankSymbol m:rs)}
shiftLeftTape m@TuringMachine{tape = (l:ls, rs)} = m{tape = (ls, l:rs)}

-- | Moves the tape to the right
shiftRightTape :: TuringMachine q s -> TuringMachine q s
shiftRightTape m@TuringMachine{tape = (ls, [])} = m{tape = (blankSymbol m:ls, [])}
shiftRightTape m@TuringMachine{tape = (ls, r:rs)} = m{tape = (r:ls, rs)}

updateTape :: Head s -> TuringMachine q s -> TuringMachine q s
updateTape ShiftLeft m = shiftLeftTape m
updateTape ShiftRight m = shiftRightTape m
updateTape (Write s) m = writeTape s m

-- | Performs a computational step in a Turing machine
step :: (Eq q, Eq s) => TuringMachine q s -> TuringMachine q s
step m
    -- do nothing (halt)
    | isNothing delta = m
    -- perform action on tape and go to state
    | otherwise = updateTape action m{state = q'}
    where
        s = readTape m
        delta = transition m (state m, s)
        Just (q', action) = delta

-- | Runs a Turing machine
run :: (Eq q, Eq s) => TuringMachine q s -> TuringMachine q s
run m
    | state m `elem` finalStates m = m
    | otherwise = run (step m)

-- | Initializes and executes a Turing machine
(>>>) :: (Eq q, Eq s) => [s] -> TuringMachine q s -> TuringMachine q s
input >>> m = run m{tape = ([], input)}