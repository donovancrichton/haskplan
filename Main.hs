module Main where

import System.IO
import System.Environment (getArgs)
import qualified Data.Set as S
import qualified Data.PQueue.Min as P

import Types
import Combinators
import Parser
import Grounding
import Search



-- | The 'main' function reads the input files, parses them, and prints a plan.
main :: IO ()
main = do
  -- Get the arguments from the command line
  args <- getArgs
  -- The first argument is the domain file
  domainHandle <- openFile (head args) ReadMode
  -- The second argument is the world (problem) file
  worldHandle <- openFile (head (tail args)) ReadMode
  -- Parse the domain file
  domainStream <- hGetContents domainHandle
  -- Parse the problem file
  worldStream <- hGetContents worldHandle
  -- run the domain parser
  let x = run domain domainStream
  let s  = "Domain failed to parse! World was not built!"
  let x' = case x of
            -- if a successful domain was parse, run the world parser
            (Right d) -> run (world d) worldStream
            (Left _) -> Left (E s)
  -- get specific information from the world.
  let as = getActs <$> x'
  let os = getObjs <$> x'
  let gs = getGS <$> x'
  let is = getIS <$> x'
  let rs = is >>= \i -> return (P.singleton (Rs i 0))
  -- ground all the actions
  let xs = groundAll <$> as <*> os
  -- list all possible valid actions from the initial state
  let as = validActs <$> is <*> xs
  -- generate a list of expended states for the valid actions
  let ss = expandStates <$> is <*> as
  -- start with an empty closed list
  let cl = Right S.empty
  -- This section exploits lazy evaluation. We can ask Haskell to
  -- calculate all of the possible searches, but only print one of them.
  -- Only the printed search will actually get run!
  let astr = astar <$> gs <*> Right 0 <*> xs <*> rs <*> cl
  let sbfs = bfs <$> gs <*> Right(0, 0) <*> xs <*> (is >>= \i -> return [i]) <*> cl
  let sdfs = dfs <$> gs <*> Right(0, 0) <*> xs <*> (is >>= \i -> return [i]) <*> cl
  let search' = case (head (tail (tail args))) of
               "dfs" -> case sdfs of 
                        Right (Just s, _) -> printParents s ++ " " ++ show (getLength s 0)
                        _  -> "fail"
               "bfs" -> case sbfs of
                        Right (Just s, _) -> printParents s ++ " " ++ show (getLength s 0)
                        _  -> "fail"
               "astar" -> case astr of
                        Right (Just s, _) -> printParents s ++ " " ++ show (getLength s 0)
                        _  -> "fail"
  print search'
  -- close file handles
  hClose domainHandle
  hClose worldHandle
  -- exit
