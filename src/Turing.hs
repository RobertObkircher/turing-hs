{-# LANGUAGE LambdaCase #-}

{-
 - q = machine state
 - s = sigma = input alphabet
 - g = gamma = tape alphabet
 -}
module Turing where

import Control.Monad(filterM)
import Data.Bool(bool)
import Data.List(permutations)

data Tape g = Tape [g] g [g] deriving (Show)

data Machine g q = Machine
    { tape :: Tape g
    , blankSymbol :: g
    , currentState :: q
    , finalStates :: [q]
    , transitionFun :: q -> g -> (q, g, Movement)
    }

data Movement = L | R | N deriving (Show)


--
-- Tape
--

moveTape :: Movement -> Tape g -> Tape g
moveTape L (Tape (x:xs) current right) = Tape xs x (current:right)
moveTape R (Tape left current (x:xs)) = Tape (current:left) x xs
moveTape N t = t

writeTape :: g -> Tape g -> Tape g
writeTape symbol (Tape left _ right) = Tape left symbol right

readTape :: Tape g -> g
readTape (Tape _ current _) = current

insert :: [g] -> Tape g -> Tape g
insert [] t = t
insert (x:xs) (Tape left current right) = Tape left x $ xs ++ current : right

-- print until the first blank
printTape :: Eq g => g -> (Bool -> g -> String) -> Tape g -> String
printTape blank printSymbol (Tape left current right) =
    printList leftSymbols ++ printSymbol True current ++ printList rightSymbols
  where
    untilBlank = takeWhile (/= blank)
    leftSymbols = reverse $ untilBlank left
    rightSymbols = untilBlank right
    printList = concatMap (printSymbol False)

--
-- Machine
--

emptyMachine :: g -> q -> [q] -> (q -> g -> (q, g, Movement)) -> Machine g q
emptyMachine blank start final transition = Machine 
    { tape = Tape (repeat blank) blank (repeat blank)
    , blankSymbol = blank
    , currentState = start
    , finalStates = final
    , transitionFun = transition
    }

withInput :: [g] -> Machine g q -> Machine g q
withInput input m = m { tape = insert input (tape m) }

runMachine :: Machine g q -> [Machine g q]
runMachine = iterate step

step :: Machine g q -> Machine g q
step m = applyTransition (doTransition m) m

applyTransition :: (q, g, Movement) -> Machine g q -> Machine g q
applyTransition (state, symbol, direction) m = m
    { tape = moveTape direction $ writeTape symbol $ tape m
    , currentState = state
    }

doTransition :: Machine g q -> (q, g, Movement)
doTransition m = transitionFun m (currentState m) (readTape $ tape m)

halted :: Eq q => Machine g q -> Bool
halted m = currentState m `elem` finalStates m

runMachineHalted :: Eq q => Machine g q -> Machine g q
runMachineHalted = head . dropWhile (not . halted) . runMachine

printMachine :: Eq g => (Bool -> g -> String) -> ((q, g, Movement) -> String) -> Machine g q -> String
printMachine printSymbol printTransition m = tapeString ++ " || " ++ transitionString
  where
    tapeString = printTape (blankSymbol m) printSymbol (tape m)
    transitionString = printTransition $ doTransition m


--
-- Problem specific
--

-- This type represents the symbols a_1 to a_n
type A = Char

data Sigma
    = Heart
    | A A
    deriving(Eq, Show)

data Gamma
    = Blank
    | Ignore
    | Sigma Sigma
    deriving(Eq, Show)

data Q
    = Initial
    | Start
    | Accept
    | Reject
    | Reset
    | Heartless
    | Move A
    | Check A
    deriving(Eq, Show)

sigma :: Char -> Sigma
sigma = \case
  '❤' -> Heart
  c   -> A c

sigmaToChar :: Sigma -> Char
sigmaToChar = \case
    Heart -> '❤'
    A c -> c

transition :: Q -> Gamma -> (Q, Gamma, Movement)

transition Initial = \case
  Sigma Heart -> (Reject, Blank, N)    -- + not *
  a           -> (Start, a, N)

transition Start = \case
  Sigma Heart -> (Heartless, Blank, R) -- we have checked all symbols including the first Heart
  Sigma (A a) -> (Move a, Blank, R)    -- begin to check the next symbol
  Blank       -> (Reject, Blank, N)    -- no input

transition Accept = \case
  Blank       -> (Accept, Blank, N)    -- transitions to Accept always write a Blank

transition Reject = \case
  Blank       -> (Reject, Blank, N)    -- transitions to Reject always write a Blank

transition Reset = \case
  Blank       -> (Start, Blank, R)     -- begin to check the next Sigma symbol
  a           -> (Reset, a, L)         -- move all the way to the left until the first Blank

transition Heartless = \case
  Blank       -> (Accept, Blank, N)
  Ignore      -> (Heartless, Blank, R) -- skip
  Sigma _     -> (Reject, Blank, N)    -- we didn't consume everything on the right side of the first heart

transition (Move s) = \case
  Sigma (A a) -> (Move s, Sigma (A a), R)  -- move to the first Heart
  Sigma Heart -> (Check s, Sigma Heart, R) --
  Blank       -> (Reject, Blank, N)        -- so sad (that I forgot about this case)

transition (Check s) = \case
  Ignore      -> (Check s, Ignore, R)    -- skip
  Sigma (A a) -> bool (Reject, Blank, N) (Reset, Ignore, L) (a == s) -- we found the symbol
  Sigma Heart -> (Reject, Blank, N)      -- too much love
  Blank       -> (Reject, Blank, N)      -- the right hand side was too short

machine :: Machine Gamma Q
machine = emptyMachine Blank Initial [Accept, Reject] transition

-- "abc❤abc"
machineWithInput :: [A] -> Machine Gamma Q
machineWithInput input = withInput tapeSymbols machine
  where
    tapeSymbols = Sigma . sigma <$> input


run :: String -> [Machine Gamma Q]
run = takeWhile (not.halted) . runMachine . machineWithInput

printMachine' :: Machine Gamma Q -> String
printMachine' = printMachine printSymbol printTransition

printSymbol :: Bool -> Gamma -> String
printSymbol = bool printOtherSymbol printCurrentSymbol

printCurrentSymbol :: Gamma -> String
printCurrentSymbol (Sigma s) = "(" ++ sigmaToChar s : ")"
printCurrentSymbol symbol = "(" ++ show symbol ++ ")"

printOtherSymbol :: Gamma -> String
printOtherSymbol (Sigma s) = " " ++ sigmaToChar s : " "
printOtherSymbol symbol = " " ++ show symbol ++ " "

printTransition :: (Q, Gamma, Movement) -> String
printTransition (q,g,direction) = show q ++ printOtherSymbol g ++ show direction

-- "abc❤abc"
runAndPrint :: String -> String
runAndPrint = unlines . map printMachine' . run

--
-- Tests
--

testAlphabet :: String
testAlphabet = sigmaToChar Heart : "abb"

reference :: String -> Bool
reference [] = False
reference input = case span (/= sigmaToChar Heart) input of
    (first, _:second) -> not (null first) && first == second
    _ -> False

testInstances :: [String]
testInstances = filterM (const [True, False]) symbols >>= permutations
  where
    symbols = testAlphabet ++ testAlphabet

runTests :: String
runTests = show (length failures) ++ "/" ++ show (length testInstances) ++ " FAILED\n" ++ unlines (take 10 failures)
  where
    state = currentState . runMachineHalted . machineWithInput
    check i = reference i == (state i == Accept)
    failures = filter (not . check) testInstances


