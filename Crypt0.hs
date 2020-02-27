module Crypt0 where

import Data.Map 

-- State of program is the value of all of the variables
-- Semantic domain = State -> State = (Map Name Var) -> (Map Name Var)

-- TODO (Feb 27th)
--    - CMDs: Call, While/For
--    - Features: Start on static typing, 

--
-- TYPE DECLARATIONS
--

type State = Map Name Var
type Prog  = [Cmd]
type Name  = String

data Cmd = Declare Name Expr
         | If Name Cmd Cmd
         | Return Name 
    deriving (Eq,Show)

data Expr = Add Name Name
          | Sub Name Name
          | Mul Name Name
          | Div Name Name
          | Call Name [Name]
          -- LessThan name name
          -- GreaterThan name name
          -- Equals name name
          | Var
    deriving (Eq,Show)

data Var = Int Int
         | Double Double
         | Bool Bool
         | String String
         | Function Type [(Type, Name)] Prog
    deriving (Eq,Show)

data Op = Plus | Minus | Mult | Divi
    deriving (Eq,Show)

--            Int  | Double |  Bool  | String | Function
data Type = Int_ty | Dbl_ty | Bul_ty | Str_ty | Fun_ty
    deriving (Eq,Show)

-- Stretch goal: classes

--
-- SEMANTICS
--

run :: Prog -> State -> State
run (c:cs) s = run cs (cmd c s)
run [] s = s 

cmd :: Cmd -> State -> State
cmd c s = case c of
    -- Variable declaration
    Declare ref e -> set ref (expr e s) s
    -- If statement
    If b c1 c2    -> 
        if (valBool b s)
        then cmd c1 s
        else cmd c2 s

expr :: Expr -> State -> Var
expr e s = case e of
    Add v1 v2   -> 
    -- Addition
      case typeOf v1 s of
        Int_ty    -> Int (performOpInt Plus (valInt v1 s) (valInt v2 s)) 
        Dbl_ty    -> Double (performOpDbl Plus (valDbl v1 s) (valDbl v2 s)) 
        Str_ty    -> String (performOpStr Plus (valStr v1 s) (valStr v2 s)) 
        otherwise -> error ("Invalid variable type passed to 'Add': " ++ show (typeOf v1 s))
    -- Subtraction
    Sub v1 v2   ->
      case typeOf v1 s of
        Int_ty    -> Int (performOpInt Minus (valInt v1 s) (valInt v2 s)) 
        Dbl_ty    -> Double (performOpDbl Minus (valDbl v1 s) (valDbl v2 s)) 
        -- TODO: "Str"     -> set r (String (performOpStr (valStr v1 s) (valStr v2 s) Minus)) s
        otherwise -> error ("Invalid variable type passed to 'Sub': " ++ show (typeOf v1 s)) 
    -- Multiplication
    Mul v1 v2   ->
      case typeOf v1 s of
        Int_ty    -> Int (performOpInt Mult (valInt v1 s) (valInt v2 s)) 
        Dbl_ty    -> Double (performOpDbl Mult (valDbl v1 s) (valDbl v2 s)) 
        otherwise -> error ("Invalid variable type passed to 'Add': " ++ show (typeOf v1 s))    
    -- Division
    Div v1 v2   ->
      case typeOf v1 s of
        Int_ty    -> Int (performOpInt Divi (valInt v1 s) (valInt v2 s)) 
        Dbl_ty    -> Double (performOpDbl Divi (valDbl v1 s) (valDbl v2 s)) 
        otherwise -> error ("Invalid variable type passed to 'Add': " ++ show (typeOf v1 s))    

--
-- EXPRESSIONS
--

performOpInt :: Op -> Int -> Int -> Int
performOpInt o x y = 
    case o of
      Plus  -> x + y
      Minus -> x - y
      Mult  -> x * y
      Divi  -> x `div` y

valInt :: Name -> State -> Int
valInt v s = 
    case get v s of
      Int x -> x

performOpDbl :: Op -> Double -> Double -> Double
performOpDbl o x y = 
    case o of
      Plus  -> x + y
      Minus -> x - y
      Mult  -> x * y
      Divi  -> x / y

valDbl :: Name -> State -> Double
valDbl v s = 
    case get v s of
      Double x -> x

performOpStr :: Op -> String -> String -> String
performOpStr o x y = 
    case o of
      Plus  -> x ++ y
      --TODO: Minus -> x - y

valStr :: Name -> State -> String
valStr v s = 
    case get v s of
      String x -> x

valBool :: Name -> State -> Bool
valBool v s = 
    case get v s of
      Bool x -> x

--
-- VARIABLE MANIPULATION
--


get :: Name -> State -> Var 
get key s = s ! key
-- TODO: Variable does not exist case

-- Changes the value of a variable on the stack
set :: Name -> Var -> State -> State
set key v s = (insert key v s)

-- Returns type of a variable
typeOf :: Name -> State -> Type
typeOf key s = case get key s of 
    (Int _)    -> Int_ty
    (Double _) -> Dbl_ty
    (Bool _)   -> Bul_ty
    (String _) -> Str_ty

-- Removes a variable from scope
removeVar :: Name -> State -> State
removeVar key s = delete key s

-- TESTING

-- | Add together two vars
--
-- NOTE: We deal with the same inherent issues with floating point arithmetic as languages like Python
--
-- | Subtract two vars
--
-- | Multiplies two vars
--
-- | Divide two vars
--
-- | If statement


--
-- FUNCTIONS
--

-- Function [(Type, Name)] Prog
-- data Op = Plus | Minus | Mult | Divi

--call :: Var -> State -> Var
--call t  s = case t of
--    Int_ty -> undefined
--    Dbl_ty -> undefined
--    Bul_ty -> undefined
--   Str_ty -> undefined
--    Fun_ty -> undefined

-- Int | Double | Bool | String | Function
-- data Type = Int_ty | Dbl_ty | Bul_ty | Str_ty | Fun_ty

s0 :: State
s0 = empty

--run prog s0
prog = [Declare "v1" (Int 23), Declare "v2" (Int 56)]
