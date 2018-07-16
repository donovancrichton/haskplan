{-# OPTIONS_GHC -Wall #-}

module Search where

import qualified Data.Set as S
import qualified Data.PQueue.Min as P

import Types
import Grounding

type Actions = [GroundAction]
type Frontier = [State]
type PriorityQueue = P.MinQueue RelaxedState
type ClosedList = S.Set (TrueSet, FalseSet)

-- | dfs returns a Pair of a possible state, and a pair of 'Integer's. The
-- first Integer gives the number of expanded nodes, the second integer gives
-- the number of recursive calls. dfs works by adding the expanded nodes to the
-- front of the Frontier, ensuring that the left-most state will always be
-- expanded to the furthers possible before backtracking. The function
-- 'null' returns True if the list is empty.
dfs :: GoalState -> (Int, Int) -> Actions -> Frontier -> ClosedList
  -> (Maybe State, (Int, Int))
dfs gs (i, j) as fs cl | check gs this = (Just this,(i, j))
                       | null fs = (Nothing, (i, j))
                       | otherwise = case this of
               (St _ ss) -> if S.member ss cl || null vacts 
                            then dfs gs (i, succ j) as those cl
                            else dfs gs (succ i, succ j) as (new ++ those) (S.insert ss cl)
  where
  -- this is the current state in the frontier
  this :: State
  this = head fs
  
  -- those are the remaining states in the frontier
  those :: [State]
  those = tail fs

  -- vacts are the valid actions from this state.
  vacts :: [GroundAction]
  vacts = validActs this as

  -- new are the list of new expanded states generated through the
  -- application of valid actions.
  new :: [State]
  new = expandStates this vacts


-- | bfs is almost identical to dfs except that the new states are added to the
-- back of the frontier. Ensuring that each node is selected in turn for each
-- level of the tree. See 'dfs' for an explaination of sub-functions and other
-- parameters.
bfs :: GoalState -> (Int, Int) -> Actions -> Frontier -> ClosedList 
  -> (Maybe State, (Int,Int))
bfs gs (i,j) as fs cl | check gs this = (Just this, (i,j))
                      | null fs = (Nothing, (i,j))
                      | otherwise = case this of
                   (St _ ss) -> if S.member ss cl || null vacts
                                then bfs gs (i,succ j) as those cl
                                else bfs gs (succ i,succ j) as (those ++ new) (S.insert ss cl)
  where
  this :: State
  this = head fs

  those :: [State]
  those = tail fs

  vacts :: [GroundAction]
  vacts = validActs this as

  new :: [State]
  new = expandStates this vacts

-- | getCost returns an integer that represents the cost of solving a relaxed
-- plan. Note: The relaxed plan is actually solved using a BFS approach and the
-- ignore delete list heuristic. More experiments could be done to determine
-- other combinations of heuristic and search strategies for solving the
-- relaxed plan (always satisfy precondition and DFS, for example). 
getCost :: GoalState -> Int -> Actions -> Frontier -> ClosedList -> Int
getCost gs n as fs cl | ignoreDeleteList gs this = n 
                      | otherwise = case this of
                 (St _ ss) -> if S.member ss cl || null vacts
                              then getCost gs (succ n) as those cl
                              else getCost gs (succ n) as (new ++ those) (S.insert ss cl)
  where
  this :: State
  this = head fs

  those :: [State]
  those = tail fs
  
  vacts :: [GroundAction]
  vacts = validActs this as
  
  new :: [State]
  new = expandStates this vacts

-- | astar returns a pair containing the goal state and the cost to reach the
-- state. TODO: there is a small bug here, astar is increasing the cost for
-- each recursive call. Instead the path cost to the root should be used.
-- and a small function given to calculate such a cost from the final node.
-- astar uses the relaxed plan and heuristics from the 'getCost' function.
astar :: GoalState -> Int -> Actions -> PriorityQueue -> ClosedList -> (Maybe State, Int)
astar gs cost as pq cl | check gs ts = (Just ts, cost)
                       | P.null pq = (Nothing, cost)
                       | otherwise = case ts of
                  (St _ ss) -> if S.member ss cl || null vacts
                               then astar gs cost' as pq cl
                               else astar gs cost' as pq' (S.insert ss cl)
  where
  -- a relaxed state has no delete list, and also has an attached cost.
  this :: RelaxedState
  this = P.findMin pq

  ts :: State
  ts = case this of
    (Rs s i) -> s

  ps :: P.MinQueue RelaxedState
  ps = P.drop 1 pq

  pq' :: P.MinQueue RelaxedState
  pq' = P.union (P.drop 1 pq) (P.fromList new)

  vacts :: [GroundAction]
  vacts = case this of
          (Rs s i) -> validActs s as 

  new :: [RelaxedState]
  new = map (\x -> Rs x (cost + (getCost gs 0 as [ts] S.empty))) (expandStates ts vacts)

  -- | cost' calculates the path cost to the current node
  cost' :: Int
  cost' = getLength ts 0

  
  

  
  
  
