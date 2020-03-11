module Crypt0 where

import Data.Map 

-- State of program is the value of all of the variables
-- Semantic domain = State -> State = (Map Name Var) -> (Map Name Var)

-- TODO: Type checking

--
-- TYPE DECLARATIONS
--

type State  = Map Name Var
type Prog   = [Cmd]
type Name   = String
type Params = [Expr]

-- Commands change the program's state
data Cmd = Declare Name Expr
         | If Expr Prog Prog
         | While Expr Prog
         | Return Expr

    deriving (Eq,Show)

--TODO: Express iteration and for loop as syntactic sugar - will ease type checking

-- Expressions perform operations and return variables
data Expr = BinOp Op Expr Expr
          | Lit Var
          | Get Name
          | Lt Expr Expr 
          | Gt Expr Expr 
          | Eq Expr Expr
          | Call Name [Expr]
    deriving (Eq,Show)

data Var = Int Int
         | Dbl Double
         | Bul Bool
         | Str String
         -- Function
         | Fun Type [(Type, Name)] Prog
    deriving (Eq,Show)

-- The arithmetic operators that we support
data Op = Add | Sub | Mul | Div
    deriving (Eq,Show)

--            Int  | Double |  Bool  | String | Function
data Type = Int_ty | Dbl_ty | Bul_ty | Str_ty | Fun_ty
    deriving (Eq,Show)

--
-- SEMANTICS
--

run :: Prog -> State -> (Either String State)
run p s = case (typeCheck p []) of
    (Just s)  -> Left s
    (Nothing) -> Right (driver p s)

driver :: Prog -> State -> State
driver (c:cs) s = driver cs (cmd c s)
driver [] s = s 

cmd :: Cmd -> State -> State
cmd command s = case command of
    -- Variable declaration
    Declare ref e   -> set ref (expr e s) s
    -- If statement
    If e c1 c2      -> case expr e s of
        (Bul b)           -> if b then driver c1 s else driver c2 s
    -- While loop 
    While e c       -> while e c s

expr :: Expr -> State -> Var
expr e s = case e of
    -- Addition
    BinOp o e1 e2 -> case (expr e1 s, expr e2 s) of
        (v1, v2) -> op o v1 v2
    -- Less than check
    Lt e1 e2  -> case (expr e1 s, expr e2 s) of
        (Int v1, Int v2) -> Bul (v1 < v2)
        (Dbl v1, Dbl v2) -> Bul (v1 < v2)
    -- Greater than check
    Gt e1 e2  -> case (expr e1 s, expr e2 s) of
        (Int v1, Int v2) -> Bul (v1 > v2)
        (Dbl v1, Dbl v2) -> Bul (v1 > v2)
    -- Equality check
    Eq e1 e2  -> case (expr e1 s, expr e2 s) of
        (Int v1, Int v2) -> Bul (v1 == v2)
        (Dbl v1, Dbl v2) -> Bul (v1 == v2)
        (Bul v1, Bul v2) -> Bul (v1 == v2)
        (Str v1, Str v2) -> Bul (v1 == v2)
    -- Call function
    Call ref es -> call ref es s
    -- Lit
    Lit v   -> v
    -- Get existing variable
    Get ref   -> get ref s

--
-- TYPE CHECKING
--

-- Static type checking for each prog
-- QUESTION: Check repeated declares?
typeCheck :: Prog -> [(Name, Type)] -> Maybe String
typeCheck [] s = Nothing
typeCheck (c:cs) s = case c of
    (Declare ref e) -> undefined
    (If e p1 p2)    -> undefined
    (While e p)     -> undefined
    (Return e)      -> undefined

-- Finds the eventual type of an expression, returns nothing if there is a type error
typeExpr :: Expr -> [(Name, Type)] -> maybe Type
typeExpr (Lit v)           s = undefined
typeExpr (Get ref)         s = undefined
typeExpr (Lt e1 e2)        s = undefined 
typeExpr (Gt e1 e2)        s = undefined
typeExpr (Eq e1 e2)        s = undefined
typeExpr (BinOp op e1 e2)  s = undefined
typeExpr (Call ref (e:es)) s = undefined

-- Gets type of a variable
typeOf :: Var -> Type
typeOf (Int _)     = Int_ty
typeOf (Dbl _)     = Dbl_ty
typeOf (Bul _)     = Bul_ty
typeOf (Str _)     = Str_ty
typeOf (Fun _ _ _) = Fun_ty

--
-- BINARY OPERATORS
--

op :: Op -> Var -> Var -> Var
-- Addition
op Add (Int s1) (Int s2) = Int (s1 + s2)
op Add (Dbl s1) (Dbl s2) = Dbl (s1 + s2)
op Add (Str s1) (Str s2) = Str (s1 ++ s2)
-- Subtraction
op Sub (Int s1) (Int s2) = Int (s1 - s2)
op Sub (Dbl s1) (Dbl s2) = Dbl (s1 - s2)
-- TODO: op Sub (String s1) (String s2) = String (s1 -- s2)
-- Multiplication
op Mul (Int s1) (Int s2) = Int (s1 * s2)
op Mul (Dbl s1) (Dbl s2) = Dbl (s1 * s2)
-- Division
op Div (Int s1) (Int s2) = if s2 /= 0
    then Int (s1 `div` s2) else error "ERROR: attempt to divide by 0"
op Div (Dbl s1) (Dbl s2) = if s2 /= 0 
    then Dbl (s1 / s2) else error "ERROR: attempt to divide by 0"
--
-- VARIABLE MANIPULATION
--

-- Returns a variable by name
get :: Name -> State -> Var 
get ref s = case Data.Map.lookup ref s of
    (Just v) -> v
    Nothing  -> error ("ERROR: variable not in scope: " ++ ref) 

-- Changes the value of a variable on the stack
set :: Name -> Var -> State -> State
set key v s = (insert key v s)

-- Removes a variable from scope
removeVar :: Name -> State -> State
removeVar key s = delete key s

-- Increment and Decrement
iter ::  Name -> Op -> State -> Prog
iter ref o s = case get ref s of 
    (Int i) -> [Declare ref (BinOp o (Lit (Int i)) (Lit (Int 1)))]
    (Dbl d) -> [Declare ref (BinOp o (Lit (Dbl d)) (Lit (Dbl 1)))]

-- Pulls the value out of a bool variable
valBool :: Var -> Bool
valBool (Bul b) = b

varExists :: Name -> State -> Bool
varExists ref s = case Data.Map.lookup ref s of
    (Just v) -> True
    Nothing  -> False

--
-- LOOPS
--

-- NOTE: loops are part of the same function scope they are run in

-- A while loop of the form `while (condition) {prog}`
while :: Expr -> Prog -> State -> State
while e p s = if valBool (expr e s) then while e p (driver p s) else s

-- TODO:
-- A for loop of the form `for (declaration; condition; iterator) {prog}`

--
-- FUNCTIONS
--

-- First create a state with the function's parameters and then run the function
-- Our functions have static scoping and are pass by value
call :: Name -> Params -> State -> Var
call ref prms s = case get ref s of
    (Fun _ fVars body) ->
        let s' = getParams fVars prms s
        in doFunc body s' 

-- Actually run the function body - similar to the prog function
doFunc :: Prog -> State -> Var
doFunc (c:cs) s' = case c of 
    Return e  -> expr e s'
    otherwise -> doFunc cs (cmd c s')  
doFunc [] s'= error "ERROR: No return statement in function body"

-- Binds the parameters passed to the function to the function's expected variables
-- This creates a sub-state with only the function's passed-in variables
getParams :: [(Type, Name)] -> Params -> State -> State
getParams (fv:fvs) (p:ps) s = case fv of
    (_, ref) -> set ref (expr p s) empty `union` getParams fvs ps s
getParams [] [] s       = s
getParams [] (p:ps) s   = error "ERROR: Too many parameters passed to function"
getParams (fv:fvs) [] s = error "ERROR: Too few parameters passed to function" 

-- Function Type [(Type, Name)] Prog

--
-- SYNTACTIC SUGAR
--

-- S0 is the empty state
s0 :: State
s0 = empty

--
-- DOCTESTS
--

-- TODO testing for:
-- | Add together two vars
-- | Subtract two vars
-- | Muliplies two vars
-- | Divide two vars
-- | If statement

--
-- MANUAL TESTING
--

--run <program> s0
addProg = run [Declare "num" (Lit (Int 23)), 
               Declare "num2" (Lit (Int 24)), 
               Declare "Result" (BinOp Add (Get "num") (Get "num2"))] s0
         
subProg = run [Declare "num" (Lit (Int 23)), 
               Declare "num2" (Lit (Int 24)), 
               Declare "Result" (BinOp Sub (Get "num") (Get "num2"))] s0        
           
mulProg = run [Declare "num" (Lit (Int 23)), 
               Declare "num2" (Lit (Int 24)), 
               Declare "Result" (BinOp Mul (Get "num") (Get "num2"))] s0      

divProg = run [Declare "num" (Lit (Int 23)), 
               Declare "num2" (Lit (Int 24)), 
               Declare "Result" (BinOp Div (Get "num") (Get "num2"))] s0        

divZeroProg = run [Declare "num" (Lit (Int 23)), 
                   Declare "num2" (Lit (Int 0)), 
                   Declare "Result" (BinOp Div (Get "num") (Get "num2"))] s0        

ifProg = run [Declare "true" (Lit (Bul True)), 
              If (Get "true") 
                  [(Declare "True" (Lit (Int 1)))]
                  [(Declare "False" (Lit (Int 0)))]] s0

ltProg = run [Declare "lt" (Lit (Int 5)), 
              Declare "gt" (Lit (Int 10)),
              If (Lt (Get "lt") (Get "gt"))
                 [Declare "True" (Lit (Bul True))] 
                 [Declare "False" (Lit (Bul False))]] s0

gtProg = run [Declare "lt" (Lit (Int 5)), 
              Declare "gt" (Lit (Int 10)), 
              If (Gt (Get "gt") (Get "lt"))
                 [Declare "True" (Lit (Bul True))] 
                 [Declare "False" (Lit (Bul False))]] s0

eqProg = run [Declare "1" (Lit (Int 10)), 
              Declare "2" (Lit (Int 10)), 
              If (Eq (Get "1") (Get "2"))
                 [Declare "True" (Lit (Bul True))] 
                 [Declare "False" (Lit (Bul False))]] s0

nullVarProg = run [Declare "i" (Get "null")] s0

--iterProg = run [Declare "i" (Lit (Dbl 0)),
--             Iterate Inc "i",
--             Iterate Inc "i", 
--             Iterate Dec "i"] s0

--whileProg = run [Declare "i" (Lit (Int 0)),
--                 While (Lt (Get "i") (Lit (Int 5))) 
                     --[Iterate Inc "i"]] s0

--forProg = run [Declare "y" (Lit (Int 10)),
--               For ("i", Lit (Int 0)) (Lt (Get "i") (Lit (Int 7))) Inc
                   --[Iterate Dec "y"]] s0

-- Function Type [(Type, Name)] Prog
--funProg = run [Declare "fun" (Lit (Fun Int_ty [(Int_ty, "x")]
--                   [Return (BinOp Add (Get "x") (Lit (Int 3)))])),
--               Declare "result" (Call "fun" [(Lit (Int 5))])] s0
