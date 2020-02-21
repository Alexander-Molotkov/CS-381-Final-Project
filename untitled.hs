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
         | Bool Name Bool
      -- | String Name String
      -- | TODO: More types
    deriving (Eq,Show)

-- TODO: Function = String (name of function) -> [String OR Var] (variables) -> Prog (code) -> State -> State
-- Based on other parts of the code, it seems like we only want to pass by reference. Function scope may be hard
-- Have to decide if we want to allow function calls with no arguments
-- If we do this early we can probably implement all of our cmds as functions

-- Stretch goal: classes

run :: Prog -> State -> State
run (c:cs) s = run cs (cmd c s)
run [] s = s 

cmd :: Cmd -> State -> State
cmd c s = case c of
    Declare v   -> declare v s
    Add r v1 v2 -> add r v1 v2 s
    -- Other commands

-- Goes through stack and gets specific variable's value by reference
pullVar :: Name -> State -> Var 
pullVar ref (v:vs) = case v of
   Int  name val -> if name == ref then v else pullVar ref vs
   Bool name val -> if name == ref then v else pullVar ref vs
-- TODO: Variable does not exist case
pullVar _ [] = undefined

-- Changes the value of a variable on the stack
-- Right now you can change types into other types dynamically - Bools can become Ints, etc, this feels bad.
-- Names can also change - double bad?
-- Have to think about what restrictions we want on variable manipulation - do we want variables to
   -- be autodeclared like python if they don't already exist? Or more C-like strict typing?
pushVar :: Name -> Var -> State -> State
pushVar ref i (v:vs) = case v of
    Int name val -> 
        if name == ref 
        then [i] ++ pushVar ref i vs
        else  v   : pushVar ref i vs
    Bool name val -> 
        if name == ref 
        then [i] ++ pushVar ref i vs
        else  v   : pushVar ref i vs                 
pushVar ref i [] = [i]

-- TODO: 

-- Adds variable to stack
-- This currently doesn't allow for uninitialized values (which feels like a good thing?)
-- TODO: Change value of variables explicitly with declare **(declare and pushVar can be merged)**
declare :: Var -> State -> State
declare v s = s ++ [v]

add :: Name -> Name -> Name -> State -> State
add result a1 a2 s = pushVar result (Int result (x+y)) s
    where 
    x = case pullVar a1 s of
        (Int name v) -> v    
    y = case pullVar a2 s of
        (Int name v) -> v

-- Testing 
-- TODO: Doctests
s0 :: State
s0 = []

-- run prog s0
prog = [Declare (Int "num1" 1), Declare (Int "num2" 3), Add "sum" "num1" "num2"]





