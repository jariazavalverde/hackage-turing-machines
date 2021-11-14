import Automaton.TuringMachine

-- | Add two numbers in base-1
--   Input: two numbers in base-1 separated by a blank (0)
--   Example: [1,1,0,1,1,1] >>> addTM = [1,1,1,1,1] = 2 + 3 = 5
addTM :: TuringMachine Int Int
addTM = mkTuringMachine 0 [4] 0 delta
    where
        delta (0,1) = Just (0, ShiftRight)
        delta (0,0) = Just (1, Write 1)
        delta (1,1) = Just (1, ShiftRight)
        delta (1,0) = Just (2, ShiftLeft)
        delta (2,1) = Just (2, Write 0)
        delta (2,0) = Just (3, ShiftLeft)
        delta (3,1) = Just (3, ShiftLeft)
        delta (3,0) = Just (4, ShiftRight)
        delta _ = Nothing