{-# OPTIONS_GHC -Wall #-}

module Search where

import System.IO
--import Control.Monad.ST
--import Control.Monad
--import qualified Data.Vector.Unboxed.Mutable as M
--import qualified Data.Vector.Unboxed as V
import Data.Either

import Combinators
import Parser
import Types

-- | The 'main' function reads the input files, and parses them.
main :: IO ()
main = do
  -- args <- getArgs
  let args = ["../Gripper/domain.pddl", "../Gripper/GENERATOR/gw03.pddl"]
  --let args = ["../Blocks/domain.pddl", "../Blocks/GENERATOR/bw03.pddl"]
  domainHandle <- openFile (head args) ReadMode
  worldHandle <- openFile (head (tail args)) ReadMode
  domainStream <- hGetContents domainHandle
  worldStream <- hGetContents worldHandle
  --print domainStream
  --print worldStream
  let x  = run domain domainStream
  let s  = "Domain failed to parse! World was not built!"
  let x' = case x of
            (Right d) -> run (world d) worldStream
            (Left _) -> Left (E s)
  --print x'
  let w = fromRight blank x'
  let as = map pc (actions w)
  let is = initialstate w
  print (actions w)
  putStr "\n\n"
  print is
  putStr "\n\n"
  print $ map (\xs -> check is xs) as
  hClose domainHandle
  hClose worldHandle

blank :: World
blank = World (Domain "" [] []) [] [] []

check :: (Eq a) => [a] -> [a] -> Bool
check = \xs ys -> foldr (&&) True (map (\x -> elem x ys) xs)

pc :: Action -> PreCondition
pc (Action _ _ p _) = p

actions :: World -> [Action]
actions (World (Domain _ _ as) _ _ _) = as

initialstate :: World -> InitialState
initialstate (World _ _ is _ ) = is







