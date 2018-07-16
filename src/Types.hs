{-# OPTIONS_GHC -Wall #-}

module Types where

import qualified Data.Set as S

-- | An 'Error' is just a type containing a string.
newtype Error = E String

-- | 'Name' is a String alias.
type Name = String

-- | 'Variable' is a String alias.
type Variable = String

-- | 'Parameter' is a String alias.
type Parameter = String

-- | An 'Objects' is a list of 'Variable's
type Objects = [Variable]

-- | Typealiases for specific 'State' types
type PreState = State
type PostState = State
type InitialState = State
type GoalState = State

-- | An 'Action' consists of a 'Name', a list of 'Parameter's, a
-- 'Precondition' and an 'Effect'.
data Action = Action Name [Parameter] PreState PostState

-- | A 'GroundAction' is an 'Action' that has been ground to a particular
-- fixed 'Parameter' assignment.
data GroundAction = GroundAction Name PreState PostState

-- | A 'Domain' has a 'Name', a list of 'Predicate's, and a list of
--'Action's.
data Domain = Domain Name [Predicate] [Action]

-- | The 'World' has a 'Domain', some 'Objects' an 'InitialState' and a
-- 'GoalState'.
data World = World Domain Objects InitialState GoalState

-- | 'Predicate' is a name, and a list of arguments
type Predicate = (String, [String])

-- | A 'TrueSet' is the set of all true literals. The 'Add' list.
type TrueSet = S.Set Predicate

-- | A 'FalseSet' is the set of all false literals. The 'Delete' list.
type FalseSet = S.Set Predicate

-- | A 'State' is a possible ('State', 'GroundAction') pair that 
-- represents the parent state, and the action performed to construct the
-- current state. A 'State' is also defined by the tuple of literals that are
-- True, and literals that are False.
data State = 
  St (Maybe (State, GroundAction)) (TrueSet, FalseSet)

-- | A 'RelaxedState' is particular 'State' and an associated cost with solving
-- the relaxed plan from that 'State'.
data RelaxedState = Rs State Int

----------Show instances allow a type to be converted to a String-------------
instance Show Error where
  show (E s) = s

instance Show Domain where
  show (Domain name ps as) = show name ++ " " ++ show ps ++ " " ++ show as

instance Show State where
  show (St ps st) = show "State: " ++ show ps ++ " " ++ show st

instance Show World where
  show (World d os is gs) = show d ++ " " ++ show os ++ " " ++ show is ++ " "
    ++ show gs

instance Show Action where
  show (Action name param pre post) = show name ++ " " ++ show param ++ " "
    ++ show pre ++ " " ++ show post

instance Show GroundAction where
  show (GroundAction name pre post) = show name ++ " " ++ show pre ++ " " 
    ++ show post

-- | A 'GroundAction' is equal when its components are all equal.
instance Eq GroundAction where
  GroundAction n1 p1 e1 == GroundAction n2 p2 e2 = (n1 == n2) && (p1 == p2) && (e1 == e2)

-- | A 'State' is equal when its components are all equal.
instance Eq State where
  St p s == St p2 s2  = p == p2 && s == s2

-- | A 'RelaxedState' is equal when its components are all equal.
instance Eq RelaxedState where
  Rs s1 i1 == Rs s2 i2 = s1 == s1

-- | A 'RelaxedState' is ordered by its respective relaxed plan cost.
instance Ord RelaxedState where
  Rs s1 i1 <= Rs s2 i2 = i1 <= i2

