module Crypt0 where

import Data.Map 

-- State of program is the value of all of the variables
-- Semantic domain = State -> State

-- TODO (Feb 27th)
--    - CMDs: Sub, Call, If, While/For
--    - VARs: Int, Bool, String, (Maybe float)
--    - Features: Start on static typing, 

type State = Map Name Var
type Prog  = [Cmd]
type Name  = String

data Cmd = Declare Name Var
         | Add Name Name Name
      --  Sub Name Name
         | If Name Cmd Cmd
      --  Call Function [Var] OR Call Function [Name] (value or reference?)
      --  TODO: More cmds
    deriving (Eq,Show)

-- Var = Name + Value
data Var = Int Int
         | Double Double
         | Bool Bool
         | String String
      --  TODO: More types
    deriving (Eq,Show)

-- TODO: Function = String (name of function) -> [String OR Var] (variables) -> Prog (code) -> State -> State
-- Based on other parts of the code, it seems like we only want to pass by reference. Function scope may be hard

-- Stretch goal: classes

run :: Prog -> State -> State
run (c:cs) s = run cs (cmd c s)
run [] s = s 

cmd :: Cmd -> State -> State
cmd c s = case c of
    Declare ref v -> set ref v s
    Add r v1 v2   -> 
        if (typeOf v1 s) == (typeOf v2 s)
        then case typeOf v1 s of 
            "Int"  -> addNum r v1 v2 s 
            "Dbl"  -> addNum r v1 v2 s
            "Str"  -> addStr r v1 v2 s
    If b c1 c2    -> 
        if (case get b s of
              Bool v -> v)
        then cmd c1 s
        else cmd c2 s
                   
    -- Other commands

-- | Goes through stack and gets specific variable's value by reference
--
-- >>> s1 = set "var" (Int 3) s0
-- >>> get "var" s1
-- Int 3
--
get :: Name -> State -> Var 
get key s = s ! key
-- TODO: Variable does not exist case

-- Changes the value of a variable on the stack
-- Have to think about what restrictions we want on variable manipulation - do we want variables to-
   -- be autodeclared like python if they don't already exist? Or more C-like strict typing?
set :: Name -> Var -> State -> State
set key v s = (insert key v s)

-- Returns type of a variable
typeOf :: Name -> State -> String
typeOf key s = case get key s of 
    (Int _)    -> "Int"
    (Double _) -> "Dbl"
    (Bool _)   -> "Bool"
    (String _) -> "Str"

-- Removes a variable from scope
removeVar :: Name -> State -> State
removeVar key s = delete key s

-- | Adds together two numbers
--
-- >>> prog = [Declare "num1" (Int 5), Declare "num2" (Int 15), Add "sum" "num1" "num2"]
-- >>> s1 = run prog s0 
-- >>> get "sum" s1
-- Int 20
--
-- >>> prog = [Declare "num1" (Double 8.2), Declare "num2" (Double 3.6), Add "sum" "num1" "num2"]
-- >>> s1 = run prog s0
-- >>> get "sum" s1
-- Double 11.8
--
addNum :: Name -> Name -> Name -> State -> State
addNum result a1 a2 s = set result (Int (x + y)) s
    where
      x = case get a1 s of
        Int v    -> v
      y = case get a2 s of
        Int v    -> v

-- | Adds together two strings
-- 
-- >>> prog = [Declare "str1" (String "asd"), Declare "str2" (String "123"), Add "str3" "str1" "str2"]
-- >>> s1 = run prog s0
-- >>> get "str3" s1
-- String "asd123"
--
addStr :: Name -> Name -> Name -> State -> State
addStr result a1 a2 s = set result (String (x ++ y)) s
    where 
      x = case get a1 s of
        String s -> s
      y = case get a2 s of
        String s -> s


--TODO:

--addNum' :: Name -> Name -> Name -> State -> State
--addNum' = add Int (+)

--add :: (a -> Var) -> (b -> b -> b) -> (Name -> Name -> Name -> State -> State)
--add type_ op = set (type_ (f `op` f))
    

-- Testing 
-- TODO: Doctests
s0 :: State
s0 = empty

--run prog s0
prog = [Declare "num1" (Double 8.2), Declare "num2" (Double 3.6), Add "sum" "num1" "num2"]
prog1 = [Declare "bool1" (Bool True), If "bool1" (Declare "true" (Int 1)) (Declare "false" (Int 0)]
