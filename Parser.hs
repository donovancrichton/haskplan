{-# OPTIONS_GHC -Wall #-}

module Parser where

import System.IO
--import System.Environment (getArgs)
import Control.Applicative (Alternative(..))

import Types 
import Combinators

-- | The 'action' combinator is a PDDL parser for valid actions.
action :: Parser Action
action = do
  _ <- whitespace >> string "(:action " <|> string "(:action "
  n <- name
  _ <- whitespace
  ps <- parameters
  p <- precondition
  e <- effect
  _ <- string ")"
  return $ Action n ps p e

-- | The 'effect' combinator is a PDDL parser for valid effects.
effect :: Parser Effect
effect = do
  _ <- whitespace >> string ":effect (and" <|> string ":effect (and"
  p <- some (notPredicate <|> predicate)
  _ <- string ")"
  return p

-- | The 'precondition' combinator is a PDDL Parser forlid
-- preconditions.
precondition :: Parser PreCondition
precondition = do
  _ <- string ":precondition" <|> (whitespace >> string ":precondition")
  _ <- whitespace
  _ <- string "("
  _ <- string "and"
  p <- some (notPredicate <|> predicate)
  _ <- string ")"
  return p
  
-- | The 'predicates' combinator is a PDDL parser for zero or more valid 
-- predicates.
predicates :: Parser [Predicate]
predicates = do 
  _ <- whitespace
  between (string "(:predicates") (string ")") (some predicate)

-- | The 'notPredicate' combinator is a PDDL parser for a predicate thats
-- prefixed by "not".
notPredicate :: Parser Predicate
notPredicate = do
  _ <- whitespace >> char '(' <|> char '('
  _ <- string "not"
  _ <- whitespace
  p <- predicate
  _ <- string ")"
  let x = case p of
            (Predicate n ps True) -> Predicate n ps False
            (Predicate n ps False) -> Predicate n ps True
  return x

-- | The 'predicate' combinator is a PDDL Parser for a single predicate.
predicate :: Parser Predicate
predicate = do
  _ <- whitespace >> string "(" <|> string "("
  n <- name
  params <- some parameter
  _ <- string ")"
  return $ Predicate n (map (\x -> x) params) True

-- | The 'parameters' combinator is a PDDL parser for at zero or more
-- parameters.
parameters :: Parser [Parameter]
parameters = do
  let s = string ":parameters"
  let open = string "("
  let close = string ")"
  _ <- whitespace <|> empty
  _ <- s <|> s >> whitespace
  between open close (some parameter)

-- | The 'p1' combinator is for predicates without the preceeding '?'.
p1 :: Parser Predicate
p1 = do
  _ <- whitespace >> string "("  <|> string "("
  n <- name
  vars <- some name
  _ <- whitespace >> string ")" <|> string ")"  
  return (Predicate n vars True)

goal :: Parser GoalState
goal = do
  _ <- whitespace >> string "(:goal" <|> string "(:goal"
  _ <- whitespace >> string "(and" <|> string "(and"
  ps <- some p1
  _ <- whitespace >> string ")" <|> string ")"
  _ <- whitespace >> string ")" <|> string ")"
  return ps
  

-- | The 'parameter' combinator is a PDDL parser for a single parameter.
parameter :: Parser Parameter
parameter = do
  _ <- whitespace >> char '?' <|> char '?'
  name
  
-- | The 'name' combinator is a PDDL parser for one or more alphanumeric 
-- characters.
name :: Parser String
name = whitespace >> (some alphaNum) <|> (some alphaNum)

-- | The 'state' combinator is a PDDL parser for one or more states.
state :: Parser [String]
state = between (string "(") (string ")") (some name)

-- | The 'domain' combinator is a PDDL parser for the domain description.
domain :: Parser Domain
domain = do
  _ <- string "(define"
  _ <- whitespace
  _ <- string "(domain"
  _ <- whitespace
  n <- name
  _ <- string ")"
  ps <- predicates
  _ <- whitespace
  as <- some action
  _ <- string ")"
  _ <- whitespace
  return $ Domain n ps as

-- | The 'world' combinator is a PDDL parser for the problem description.
world :: Domain -> Parser World
world d = do
  _ <- whitespace
  _ <- string "(define"
  _ <- problem
  _ <- worldDomain
  os <- objects
  is <- initial
  gs <- goal
  _ <- whitespace >> string ")" <|> string ")" >> whitespace
  return (World d os is gs)

  

-- | The 'problem' combinator is a PDDL parser for the problem name.
problem :: Parser Name
problem = do
  let f = between (string "(problem") (string ")") name
  whitespace >> f <|> f

-- | The 'worldDomain' combinator is a PDDL parser for the domain name.
worldDomain :: Parser Name
worldDomain = do
  let f = between (string "(:domain") (string ")") name
  whitespace >> f <|> f

-- | The 'objects' combinator is a PDDL parser for one or more objects.
objects :: Parser Objects
objects = do
  let close = string ")" <|> (whitespace >> string ")")
  let f = between (string "(:objects") close (some name)
  (whitespace >> f) <|> f

-- | The 'initial' combinator is a PDDL parser for the initial state.
initial :: Parser InitialState
initial = do
  let end = string ")"
      f   = between (string "(:init") (end <|> whitespace >> end) (some p1)
  whitespace >> f <|> f

{-
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
  print x'
  hClose domainHandle
  hClose worldHandle
-}

