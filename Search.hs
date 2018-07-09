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

dfs :: GoalState -> (Int, Int) -> Actions -> Frontier -> ClosedList
  -> (Maybe State, (Int, Int))
dfs gs (i, j) as fs cl | check gs this = (Just this,(i, j))
                       | null fs = (Nothing, (i, j))
                       | otherwise = case this of
               (St _ ss) -> if S.member ss cl || null vacts
                            then dfs gs (i, succ j) as those cl
                            else dfs gs (succ i, succ j) as (new ++ those) (S.insert ss cl)
  where
  this :: State
  this = head fs

  those :: [State]
  those = tail fs

  vacts :: [GroundAction]
  vacts = validActs this as

  new :: [State]
  new = expandStates this vacts


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


getCost :: GoalState -> Int -> Actions -> Frontier -> ClosedList -> Int
getCost gs n as fs cl | ignoreDeleteList gs this = n
                      | otherwise = case this of
                 (St _ ss) -> if S.member ss cl || null vacts
                              then getCost gs (succ n) as those cl
                              else getCost gs (succ n) as (those ++ new) (S.insert ss cl)
  where
  this :: State
  this = head fs

  those :: [State]
  those = tail fs
  
  vacts :: [GroundAction]
  vacts = validActs this as
  
  new :: [State]
  new = expandStates this vacts


astar :: GoalState -> Int -> Actions -> PriorityQueue -> ClosedList -> (Maybe State, Int)
astar gs cost as pq cl | check gs ts = (Just ts, cost)
                       | P.null pq = (Nothing, cost)
                       | otherwise = case ts of
                  (St _ ss) -> if S.member ss cl || null vacts
                               then astar gs (succ cost) as pq cl
                               else astar gs (succ cost) as pq' (S.insert ss cl)
  where
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



  
  

  
  
  
