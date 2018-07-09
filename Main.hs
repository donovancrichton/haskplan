module Main where

import System.IO
import qualified Data.Set as S
import qualified Data.PQueue.Min as P

import Types
import Combinators
import Parser
import Grounding
import Search



-- | The 'main' function reads the input files, and parses them.
main :: IO ()
main = do
  -- args <- getArgs
  --let args = ["../Gripper/domain.pddl", "../Gripper/GENERATOR/gw03.pddl"]
  let args = ["../Blocks/domain.pddl", "../Blocks/GENERATOR/bw04.pddl"]
  domainHandle <- openFile (head args) ReadMode
  worldHandle <- openFile (head (tail args)) ReadMode
  domainStream <- hGetContents domainHandle
  worldStream <- hGetContents worldHandle
  --print domainStream
  --print worldStream
  let x = run domain domainStream
  -- print x
  let s  = "Domain failed to parse! World was not built!"
  let x' = case x of
            (Right d) -> run (world d) worldStream
            (Left _) -> Left (E s)
  let as = getActs <$> x'
  let os = getObjs <$> x'
  let gs = getGS <$> x'
  let is = getIS <$> x'
  let rs = is >>= \i -> return (P.singleton (Rs i 0))
  let xs = groundAll <$> as <*> os
  let as = validActs <$> is <*> xs
  let ss = expandStates <$> is <*> as
  let cl = Right S.empty
  --print $ (length <$> xs)
  --print $ xs
  --print $ os
  --print $ length <$> xs
  --print is
  --print ""
  --print ss
  --print gs
  --print test
  let search = bfs <$> gs <*> Right (0,0) <*> xs <*> (is >>= \i -> return [i]) <*> cl
  let search' = case search of
                Right (Just s, _) -> printParents s
                _  -> "fail"
  print search'
  --let astr = astar <$> gs <*> Right 0 <*> xs <*> rs <*> cl
  --print astr
  hClose domainHandle
  hClose worldHandle
