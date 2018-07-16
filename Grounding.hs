{-# OPTIONS_GHC -Wall #-}

module Grounding where

import Types
import Data.List (permutations)
import Data.Compact 
import qualified Data.Set as S

-- | replace transforms all elements in a 'list' using 'f', if the element 
-- satisfies 'p'. Otherwise the element is unchanged. See 'bindName'.
replace :: (a -> a) -> (a -> Bool) -> [a] -> [a]
replace f p = map (\x -> if p x then f x else x) 

-- | bindName replaces all occurances of 'old' inside the given 'State' 
-- with 'new'. See 'bindAll'
bindName :: State -> String -> String -> State
bindName (St ps (trues, falses)) old new =
  let r :: [String] -> [String]
      r = replace (const new) (== old)
      m :: [Predicate] -> [Predicate]
      m = map (\(n, l) -> (n,r l))
      xs :: [Predicate]
      xs = S.toList trues
      ys :: [Predicate]
      ys = S.toList falses
  in St ps (S.fromList (m xs), S.fromList (m ys))

-- | bindAll binds all 'Paramters' in a 'State' with object values, given a tuple 
-- of the binding relation. 
bindAll :: [(Parameter, Variable)] -> State -> State
bindAll [] state = state
bindAll ((x, y) : xs) state = bindAll xs (bindName state x y)

-- | zipAll takes a list of paramters and a list of object permutations and 
-- creates a list of the param/object pairs for each permutation.
zipAll :: [Parameter] -> [[Variable]] -> [[(Parameter, Variable)]]
zipAll xs = map (zip xs)

-- | groundAll returns a list of 'GroundAction' given a list of possible actions
-- and objects. This generates all possible combinations of ground variable
-- to the action.
groundAll :: [Action] -> Objects -> [GroundAction]
groundAll as os = concat m
  where
  m :: [[GroundAction]] 
  m = map (`ground` os) as

-- | ground takes an 'Action' and some 'Objects' over which to parameterise
-- that action. ground then generates all possible ground actions that could
-- be generate from the list of objects. 
ground :: Action -> Objects -> [GroundAction]
ground (Action name params precond postcond) objects = groundActions where 
  n :: Int
  n = length params

  os :: [[Variable]]
  os = choose n objects

  boundPairs :: [[(Parameter, Variable)]]
  boundPairs = zipAll params os

  boundPrecond :: [State]
  boundPrecond = map (`bindAll` precond) boundPairs
 
  boundPostcond :: [State]
  boundPostcond = map (`bindAll` postcond) boundPairs

  statePairs :: [(State, State)]
  statePairs = zip boundPrecond boundPostcond

  groundActions :: [GroundAction]
  groundActions = map (uncurry (GroundAction name)) statePairs

-- | getActs is a helper function to retrieve the actions from a world.
getActs :: World -> [Action]
getActs (World (Domain _ _ as) _ _ _) = as

-- | getObjs is a helper function to retreive the objects from a world.
getObjs :: World -> Objects
getObjs (World _ os _ _ ) = os

-- | getIS is a helper function to retreive the initial state from a world.
getIS :: World -> InitialState
getIS (World _ _ is _) = is

-- | getGS is a helper function to retreive the goal state from a world.
getGS :: World -> GoalState
getGS (World _ _ _ gs) = gs

-- | canPerform takes a 'State' and a 'GroundAction' and returns True if the
-- action's precondition is satisfied by the state. Otherwise canPerform
-- returns False.
canPerform :: State -> GroundAction -> Bool
canPerform (St _ (ts, fs)) (GroundAction _ (St _ (pts, pfs)) _) 
  = trueTest && falseTest 
  where
  trueTest :: Bool 
  trueTest = S.null (S.difference pts ts)
  
  falseTest :: Bool
  falseTest = S.null (S.difference pfs fs)

-- | validActs returns a list of 'GroundAction' that are able to be
-- performed given a 'State' and a list of all 'GroundAction's.
validActs :: State -> [GroundAction] -> [GroundAction]
validActs s = filter (canPerform s) 

-- | act generates a new 'State' through the application of a 'GroundAction'
-- to a given 'State'. 
act :: State -> GroundAction -> State
act s@(St _ (ts, fs)) a@(GroundAction _ _ (St _ (ts', fs'))) 
  = St (Just (s,a)) (newTrue, newFalse)
  where
  removeFalse :: S.Set Predicate
  removeFalse = S.difference ts fs'

  removeTrue :: S.Set Predicate
  removeTrue = S.difference fs ts'

  newTrue :: S.Set Predicate
  newTrue = S.union removeFalse ts'

  newFalse :: S.Set Predicate
  newFalse = S.union removeTrue fs'

-- | getName is a helper function for retreiving the name from a 'GroundAction'
getName :: GroundAction -> String
getName (GroundAction name _ _) = name

-- | getParams is a helper function for retreiving the paramaters from a
-- 'GroundAction'. NOTE: This loses the parameter ording and is currently not
-- suitable for pretty printing the results. TODO: Find a way to preserve
-- parameter ordering.
getParams :: GroundAction -> [String]
getParams (GroundAction name (St _ (ts, fs)) _) = S.toList $ S.fromList (g ++ f)
  where
  g = concat $ map (\(x, ys) -> ys) (S.toList ts)
  f = concat $ map (\(x, ys) -> ys) (S.toList fs)

-- | printParents is a pretty printing function to print the list of actions
-- that were required to execute from the initial state in order to reach the
-- current state. When passed the goalState, this function should give a
-- successful plan. NOTE: due to the reliance on 'getParams' This function is
-- not showing the action parameters in the correct order! TODO: fix!
printParents :: State -> String
printParents (St (Just (s, a)) _) = printParents s ++ " " ++ getName a ++ " " ++ show (getParams a)
printParents (St Nothing _) = ""

-- | expandStates returns a list of 'State's that are arrived at upon applying
-- each action in a list of 'GroundAction's to a given 'State'.
expandStates :: State -> [GroundAction] -> [State]
expandStates s as = act s <$> as

-- | check returns True if the two given 'State's have the same 'TrueSet' and
-- 'FalseSet' (or Add and Delete list). check uses set difference and a test for
-- the empty set to achieve this.
check :: State -> State -> Bool
check (St _ (ts, fs)) (St _ (ts', fs'))
  = checkTrue && checkFalse
  where
  checkTrue :: Bool
  checkTrue =  null (S.difference ts ts')

  checkFalse :: Bool
  checkFalse = null (S.difference fs fs')

-- | ignoreDeleteList is the equality test for the FF heuristic. This returns
-- only if the set difference between the two 'TrueSet's  
ignoreDeleteList :: State -> State -> Bool
ignoreDeleteList (St _ (ts, _)) (St _ (ts', _)) = checkTrue
  where
  checkTrue :: Bool
  checkTrue = null (S.difference ts ts')

-- choose generates all 'n' length permutations from a given 'list'.
choose :: Int -> [a] -> [[a]]
choose n list = concatMap permutations $ choose' list [] where
  choose' [] r     = [r | length r == n]
  choose' (x:xs) r | length r == n = [r]
                   | otherwise = choose' xs (x:r) ++ choose' xs r

getLength :: State -> Int -> Int
getLength (St Nothing _) n = n
getLength (St (Just (s,a)) _) n = getLength s (succ n)

 
