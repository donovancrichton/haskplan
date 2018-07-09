{-# OPTIONS_GHC -Wall #-}

module Grounding where

import Types
import Data.List (permutations)
import qualified Data.Set as S

-- replace transforms all elements in a 'list' using 'f', if the element 
-- satisfies 'p'. Otherwise the element is unchanged. See 'bindName'.
replace :: (a -> a) -> (a -> Bool) -> [a] -> [a]
replace f p = map (\x -> if p x then f x else x) 

-- bindName replaces all occurances of 'old' inside the given 'State' 
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

-- bindAll binds all 'Paramters' in a 'State' with object values, given a tuple 
-- of the binding relation. 
bindAll :: [(Parameter, Variable)] -> State -> State
bindAll [] state = state
bindAll ((x, y) : xs) state = bindAll xs (bindName state x y)

-- zipAll takes a list of paramters and a list of object permutations and 
-- creates a list of the param/object pairs for each permutation.
zipAll :: [Parameter] -> [[Variable]] -> [[(Parameter, Variable)]]
zipAll xs = map (zip xs)

-- groundAll returns a list of 'GroundAction' given a list of possible actions
-- and objects. This generates all possible combinations of ground variable
-- to the action.
groundAll :: [Action] -> Objects -> [GroundAction]
groundAll as os = concat m
  where
  m :: [[GroundAction]] 
  m = map (`ground` os) as

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

--getVars (GroundAction n pre post) = 
--  foldl (\(x, xs) -> (:) xs) [] (S.toList pre)

getActs :: World -> [Action]
getActs (World (Domain _ _ as) _ _ _) = as

getObjs :: World -> Objects
getObjs (World _ os _ _ ) = os

getIS :: World -> InitialState
getIS (World _ _ is _) = is

getGS :: World -> GoalState
getGS (World _ _ _ gs) = gs

canPerform :: State -> GroundAction -> Bool
canPerform (St _ (ts, fs)) (GroundAction _ (St _ (pts, pfs)) _) 
  = trueTest && falseTest 
  where
  trueTest :: Bool 
  trueTest = S.null (S.difference pts ts)
  
  falseTest :: Bool
  falseTest = S.null (S.difference pfs fs)

validActs :: State -> [GroundAction] -> [GroundAction]
validActs s = filter (canPerform s) 

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

printParents :: State -> String
printParents (St (Just (s, a)) _) = show a ++ " " ++ printParents s
printParents (St Nothing _) = "EndActions"

expandStates :: State -> [GroundAction] -> [State]
expandStates s as = act s <$> as

check :: State -> State -> Bool
check (St _ (ts, fs)) (St _ (ts', fs'))
  = checkTrue && checkFalse
  where
  checkTrue :: Bool
  checkTrue =  null (S.difference ts ts')

  checkFalse :: Bool
  checkFalse = null (S.difference fs fs')

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
