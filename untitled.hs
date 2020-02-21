module Untitled where

-- State of program is the value of all of the variables
-- Semantic domain (?): [Var] -> [Var] = State -> State
-- Stretch goal: pointers - reference variable by location in State
type State = [Var] 
type Prog  = [Cmd]
type Name  = String

data Cmd = Declare Var
         | Add Name Name Name
      -- | Sub Name Name
      -- | Call Function [Var] OR Call Function [Name] (value or reference?)
      -- | TODO: More cmds
    deriving (Eq,Show)

-- Var = Name + Value
data Var = Int Name Int
      -- | Bool Name Bool
      -- | String Name String
      -- | TODO: More types
    deriving (Eq,Show)

-- TODO: Function = String (name of function) -> [String OR Var] (variables) -> Prog (code) -> State -> State
-- Based on other parts of the code, it seems like we only want to pass by reference. Function scope may be hard
-- Have to decide if we want to allow function calls with no arguments
-- If we do this early we can probably implement all of our cmds as functions

-- Stretch goal: classes

s0 :: State
s0 = []

run :: Prog -> State -> State
run (c:cs) s = run cs (cmd c s)
run [] s = s 

cmd :: Cmd -> State -> State
cmd c s = case c of
    Declare v -> declare v s
    Add r v1 v2 -> add r v1 v2 s

-- Goes through stack and gets specific variable's value by reference
-- TODO: Make better so we don't have to pattern match against each var type
pullVar :: Name -> State -> Int
pullVar id ((Int idv i):vs) = if id == idv then i else pullVar id vs 
   -- TODO: Wrong var name case
   -- pullVar _ [] = undefined

-- Changes the value of a variable on the stack
pushVar :: Name -> Int -> State -> State
pushVar id i ((Int idv iv):vs) =
    if id == idv 
    then [Int id i]   ++ pushVar id i vs
    else [Int idv iv] ++ pushVar id i vs
   
--Adds variable to stack
--This currently doesn't allow for uninitialized values (which feels like a good thing?)
--TODO: No repeat variable names?
declare :: Var -> State -> State
declare v s = s ++ [v]

add :: Name -> Name -> Name -> State -> State
add result a1 a2 s = declare (Int result ((pullVar a1 s) + (pullVar a2 s))) s

-- run prog s0
prog = [Declare (Int "num1" 1), Declare (Int "num2" 4), Add "sum" "num1" "num2"]





