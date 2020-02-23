module Crypt0 where

-- State of program is the value of all of the variables
-- Semantic domain (?): [Var] -> [Var] = State -> State
-- Stretch goal: pointers - reference variable by location in State

-- TODO (Feb 27th)
--    - CMDs: Sub, Call, If, While/For
--    - VARs: Int, Bool, String, (Maybe float)
--    - Features: Start on static typing, 

type State = [Var] 
type Prog  = [Cmd]
type Name  = String

data Cmd = Declare Name Var
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
    Declare ref v   -> set ref v s
    Add r v1 v2 -> add r v1 v2 s
    -- Other commands

-- Goes through stack and gets specific variable's value by reference
get :: Name -> State -> Var 
get ref (v:vs) = case v of
   Int  name val -> if name == ref then v else get ref vs
   Bool name val -> if name == ref then v else get ref vs
-- TODO: Variable does not exist case
get _ [] = undefined

-- Changes the value of a variable on the stack
-- Right now you can change types into other types dynamically - Bools can become Ints, etc, this feels bad.
-- Names can also change - double bad?
-- Have to think about what restrictions we want on variable manipulation - do we want variables to-
   -- be autodeclared like python if they don't already exist? Or more C-like strict typing?
set :: Name -> Var -> State -> State
set ref i s = (declareVar ref i (removeVar i s))

declareVar :: Name -> Var -> State -> State
declareVar ref i (v:vs) = 
  case v of
    Int name val -> 
        if name == ref 
        then [i] ++ set ref i (removeVar i vs)
        else  v   : set ref i (removeVar i vs)
    Bool name val -> 
        if name == ref 
        then [i] ++ set ref i (removeVar i vs)
        else  v   : set ref i (removeVar i vs)               
declareVar ref i [] = [i]

-- Removes a variable from scope
removeVar :: Var -> State -> State
removeVar i (v:vs) =
  if i == v 
  then []  ++ removeVar i vs
  else [v] ++ removeVar i vs
removeVar i [] = []

-- Adds together two variables
add :: Name -> Name -> Name -> State -> State
add result a1 a2 s = declareVar result (Int result (x+y)) s
    where 
    x = case get a1 s of
        (Int name v) -> v    
    y = case get a2 s of
        (Int name v) -> v

-- Testing 
-- TODO: Doctests
s0 :: State
s0 = []

-- run prog s0
prog = [Declare "num1" (Int "num1" 1), Declare "num2" (Int "num2" 3), Add "sum" "num1" "num2"]
