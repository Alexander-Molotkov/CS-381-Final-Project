module Untitled where

-- State of program is the value of all of the variables
-- Semantic domain (?): [Var] -> [Var] = State -> State
-- Stretch goal: pointers - reference variable by location in State
type State = [Var] 
type Prog  = [Cmd]
type Name  = String

data Cmd = Declare Var
         | Add Name Name
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

run :: Prog -> State -> State
run (c:cs) s = undefined

cmd :: Cmd -> State -> State
cmd c s = case c of
    Declare v -> declare v s
    Add v1 v2 -> add v1 v2 s

--Goes through stack and gets specific variable by reference
pullVar :: Name -> State -> Var
pullVar ref (v:vs) = case v of
   -- TODO: Make better so we don't have to pattern match against each var type
   Int name val -> if name == ref then v else pullVar name vs
   -- TODO: Wrong var name case
   -- pullVar _ [] = undefined

pushVar :: Name -> State -> State
pushVar = undefined

--This currently doesn't allow for uninitialized values (which feels like a good thing?)
declare :: Var -> State -> State
declare = undefined

add :: Name -> Name -> State -> State
add = undefined

--prog = [Declare Int "num1" 1, Declare Int "num2" 2, Add "num1" "num2"]





