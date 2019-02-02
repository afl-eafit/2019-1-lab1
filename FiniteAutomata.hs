{- | This module is used to represent Finite Automata. -}
{-# LANGUAGE ScopedTypeVariables #-}
module FiniteAutomata where

import Data.Set as Set
import Data.Map as Map

{- | Represents the Transition Function (delta function) of a FiniteAutomata.
     The outer map stores the transitions from a orgin state,
     its value (the inner map) stores the symbols that -}
type TransitionFunction state symbol = Map state (Map symbol (Set state))

{- | Represents a Finite Automata.
     The type can represent DFA and NFA, with
     states of type @state@ and input symbols
     of type @symbol@. -}
data FiniteAutomata state symbol = FA
  {
      -- | The set of states.
      states       :: Set state
      -- | The set of input symbols.
    , alphabet     :: Set symbol
      -- | The Transition Function.
    , delta        :: TransitionFunction state symbol
      -- | The initial state.
    , initialState :: state
      -- | The set of final or accepting states.
    , acceptState  :: Set state
  }

{- | Adds a transition to the given TransitionFunction.
     @(state, symbol, state)@ is a tuple whose elements
     represent the origin state, the symbol that executes
     the transition and the end state, respectively.-}
addTransition :: forall state symbol. (Ord state, Ord symbol) =>
  (state, symbol, state) ->
    TransitionFunction state symbol -> TransitionFunction state symbol
addTransition (q, s, q') =
  Map.insertWith (Map.unionWith Set.union) q (Map.singleton s q'')
  where q'' :: Set state
        q''  = Set.singleton q'

-- | Formats how to display instances of FiniteAutomata.
instance (Show state, Show symbol) => Show (FiniteAutomata state symbol) where
  show (FA st sy tf is ac) =
    "States:         "   ++ show st ++
    "\nAlphabet:       " ++ show sy ++
    "\nDelta:          " ++ show tf ++
    "\nInitial States: " ++ show is ++
    "\nAccept States:  " ++ show ac

-- ** Example NFAs **

-- | Example 2.10 of the course's textbook.
example0 :: FiniteAutomata String Int
example0  = FA {
      states       = Set.fromList ["q0", "q1", "q2"]
    , alphabet     = Set.fromList [0, 1]
    , delta        = delta'
    , initialState = "q0"
    , acceptState  = Set.fromList ["q2"]
  } where delta' :: TransitionFunction String Int
          delta'  =
            addTransition ("q0", 0, "q0") $
            addTransition ("q0", 0, "q1") $
            addTransition ("q0", 1, "q0") $
            addTransition ("q1", 1, "q2")
            Map.empty

example1 :: FiniteAutomata Int Char
example1  = FA {
      states       = Set.fromList [1, 2, 3]
    , alphabet     = Set.fromList ['a', 'b']
    , delta        = delta'
    , initialState = 1
    , acceptState  = Set.fromList [3]
  } where delta' :: TransitionFunction Int Char
          delta'  =
            addTransition (1, 'a', 3) $
            addTransition (2, 'b', 1) $
            addTransition (2, 'b', 3) $
            addTransition (3, 'a', 1) $
            addTransition (3, 'a', 2) $
            addTransition (3, 'b', 1)
            Map.empty

example2 :: FiniteAutomata Char Int
example2  = FA {
      states       = Set.fromList ['A', 'B', 'C', 'D', 'E']
    , alphabet     = Set.fromList [0, 1]
    , delta        = delta'
    , initialState = 'A'
    , acceptState  = Set.fromList ['E']
  } where delta' :: TransitionFunction Char Int
          delta'  =
            addTransition ('A', 0, 'A') $
            addTransition ('A', 0, 'B') $
            addTransition ('A', 0, 'C') $
            addTransition ('A', 0, 'D') $
            addTransition ('A', 0, 'E') $
            addTransition ('A', 1, 'D') $
            addTransition ('A', 1, 'E') $
            addTransition ('B', 0, 'C') $
            addTransition ('B', 1, 'E') $
            addTransition ('C', 1, 'B') $
            addTransition ('D', 0, 'E')
            Map.empty
