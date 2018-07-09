
module Types where

-- | An 'Error' is just a type containing a string.
data Error = E String

-- | 'Name' is a String alias.
type Name = String

-- | 'Variable' is a String alias.
type Variable = String

-- | 'Parameter' is a String alias.
type Parameter = String

-- | A 'Precondition' is a list of 'Predicate's.
type PreCondition = [Predicate]

-- | An 'Effect' is a list of 'Predicate's
type Effect = [Predicate]

-- | An 'Obects' is a list of 'Variable's
type Objects = [Variable]

-- | The 'InitialState' is the list of 'Predicate's that are True at problem
-- initialisation.
type InitialState = [Predicate]

-- | The 'GoalState' is the list of 'Predicate's that must be True to satisfy
-- the problem.
type GoalState = [Predicate]

-- | A 'Predicate' consists of a 'Name' a list of 'Variable's and a Boolean
-- value.
data Predicate = Predicate Name [Variable] Bool

-- | An 'Action' consists of a 'Name', a list of 'Parameter's, a
-- 'Precondition'
-- and an 'Effect'.
data Action = Action Name [Parameter] PreCondition Effect

-- | A 'Domain' has a 'Name', a list of 'Predicate's, and a list of
--'Action's.
data Domain = Domain Name [Predicate] [Action]

-- | The 'World' has a 'Domain', some 'Objects' an 'InitialState' and a
-- 'GoalState'.
data World = World Domain Objects InitialState GoalState

instance Show Predicate where
  show (Predicate s vs b) = "Predicate " ++ show s ++ " " ++ show vs
    ++ " " ++ show b

instance Show Action where
  show (Action s ps pc e) = "\n\t\tAction " ++ show s 
    ++ "\n\t\t\tPredicates:\n\t\t\t\t" ++ show ps 
    ++ "\n\t\t\tPrecondition:\n\t\t\t\t" ++ show pc 
    ++ "\n\t\t\tEffects:\n\t\t\t\t" ++ show e

instance Show Domain where
  show (Domain s ps as) = "Domain: " ++ show s ++ "\n\tPredicates:\n\t\t" 
    ++ show ps ++ "\n\tActions:\n\t\t" ++ show as

instance Show World where
  show (World d os is gs) = "World: " ++ show d ++ "\n\tObjects:\n\t\t" 
    ++ show os ++ "\n\tInitialState:\n\t\t" ++ show is 
    ++ "\n\tGoalState:\n\t\t" ++ show gs

instance Show Error where
  show (E s) = s

instance Eq Predicate where
  (Predicate n vs b) == (Predicate n2 vs2 b2) = (n == n2) && b == b2
