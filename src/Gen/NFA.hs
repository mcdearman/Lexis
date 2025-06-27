{-# LANGUAGE TupleSections #-}

module Gen.NFA where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A state in the automaton
type StateID = Int

-- | Labels: Just c for character transitions, Nothing for epsilon
type Label = Maybe Char

-- | NFA represented as:
--   - A set of states
--   - A transition function: Map from (state, label) to set of target states
--   - A start state
--   - A set of accept states
data NFA = NFA
  { states :: Set.Set StateID,
    transitions :: Map.Map (StateID, Label) (Set.Set StateID),
    startState :: StateID,
    acceptStates :: Set.Set StateID
  }
  deriving (Show)

-- | Simple regex AST
data Regex
  = Empty -- matches empty string
  | Literal Char -- single character
  | Concat Regex Regex -- concatenation
  | Alt Regex Regex -- alternation (R1 | R2)
  | Star Regex -- Kleene star (R*)
  deriving (Show)

-- | Monad to generate fresh state IDs
type NFAState a = State StateID a

newState :: NFAState StateID
newState = do
  n <- get
  put (n + 1)
  return n

-- | Combine two NFA fragments
unionNFA :: NFA -> NFA -> NFA
unionNFA n1 n2 =
  NFA
    { states = states n1 `Set.union` states n2,
      transitions = Map.unionWith Set.union (transitions n1) (transitions n2),
      startState = startState n1,
      acceptStates = acceptStates n1 `Set.union` acceptStates n2
    }

-- | Thompson's construction: Regex -> NFA
regexToNFA :: Regex -> NFAState NFA
regexToNFA Empty = do
  s <- newState
  f <- newState
  let st = Set.fromList [s, f]
      trans = Map.singleton (s, Nothing) (Set.singleton f)
  return $ NFA st trans s (Set.singleton f)
regexToNFA (Literal c) = do
  s <- newState
  f <- newState
  let st = Set.fromList [s, f]
      trans = Map.singleton (s, Just c) (Set.singleton f)
  return $ NFA st trans s (Set.singleton f)
regexToNFA (Concat r1 r2) = do
  n1 <- regexToNFA r1
  n2 <- regexToNFA r2
  -- connect each accept of n1 to start of n2 with epsilon
  let epsTrans =
        Map.fromList
          [ ((a, Nothing), Set.singleton (startState n2))
          | a <- Set.toList (acceptStates n1)
          ]
  return $
    NFA
      { states = states n1 `Set.union` states n2,
        transitions =
          Map.unionsWith
            Set.union
            [transitions n1, transitions n2, epsTrans],
        startState = startState n1,
        acceptStates = acceptStates n2
      }
regexToNFA (Alt r1 r2) = do
  n1 <- regexToNFA r1
  n2 <- regexToNFA r2
  s <- newState
  f <- newState
  let st = Set.insert s (Set.insert f (states n1 `Set.union` states n2))
      -- epsilon from new start to both old starts
      startEps =
        Map.fromList
          [((s, Nothing), Set.fromList [startState n1, startState n2])]
      -- epsilon from both old accepts to new accept
      acceptEps =
        Map.fromList
          [ ((a, Nothing), Set.singleton f)
          | a <- Set.toList (acceptStates n1 `Set.union` acceptStates n2)
          ]
  return $
    NFA
      { states = st,
        transitions =
          Map.unionsWith
            Set.union
            [ transitions n1,
              transitions n2,
              startEps,
              acceptEps
            ],
        startState = s,
        acceptStates = Set.singleton f
      }
regexToNFA (Star r) = do
  n <- regexToNFA r
  s <- newState
  f <- newState
  let st = Set.insert s (Set.insert f (states n))
      -- epsilon: new start to old start, and new start to new accept
      startEps =
        Map.singleton
          (s, Nothing)
          (Set.fromList [startState n, f])
      -- epsilon: old accepts to old start, and old accepts to new accept
      loopEps =
        Map.fromList
          [ ((a, Nothing), Set.fromList [startState n, f])
          | a <- Set.toList (acceptStates n)
          ]
  return $
    NFA
      { states = st,
        transitions =
          Map.unionsWith
            Set.union
            [ transitions n,
              startEps,
              loopEps
            ],
        startState = s,
        acceptStates = Set.singleton f
      }

-- | Top‐level helper: run the State monad starting at 0
buildNFA :: Regex -> NFA
buildNFA r = evalState (regexToNFA r) 0
