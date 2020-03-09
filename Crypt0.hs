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
         | Iterate CmdIter Name
         | While Expr Prog
         | For (Name, Expr) Expr CmdIter Prog

         | Return Expr

    deriving (Eq,Show)

-- Subset of commands that iterate variables
data CmdIter = Inc
             | Dec
        deriving (Eq,Show)

-- Expressions perform operations and return variables
data Expr = BinOp Op Expr Expr
          | Lit Var
          | Get Name
          | Lt Expr Expr 
          | Gt Expr Expr 
          | Eq Expr Expr
          | Call Name [Expr]
    deriving (Eq,Show)

data Var = I Int
         | D Double
         | B Bool
         | S String
         -- Function
         | F Type [(Type, Name)] Prog
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

run :: Prog -> State -> State
run p s = 
    let check = typeCheck p 
    in if check then driver p s else s

driver :: Prog -> State -> State
driver (c:cs) s = driver cs (cmd c s)
driver [] s = s 

cmd :: Cmd -> State -> State
cmd command s = case command of
    -- Variable declaration
    Declare ref e   -> set ref (expr e s) s
    -- If statement
    If e c1 c2      -> case expr e s of
        (B b)        -> if b then run c1 s else run c2 s
    -- While loop 
    While e c       -> while e c s
    -- For loop
    For d c it p    -> for d c it p s
    -- Increment
    Iterate Inc ref -> run (iter ref Add s) s
    -- Decrement
    Iterate Dec ref -> run (iter ref Sub s) s

expr :: Expr -> State -> Var
expr e s = case e of
    -- Addition
    BinOp o e1 e2 -> case (expr e1 s, expr e2 s) of
        (v1, v2) -> op o v1 v2
    -- Less than check
    Lt e1 e2  -> case (expr e1 s, expr e2 s) of
        (I v1, I v2) -> B (v1 < v2)
        (D v1, D v2) -> B (v1 < v2)
    -- Greater than check
    Gt e1 e2  -> case (expr e1 s, expr e2 s) of
        (I v1, I v2) -> B (v1 > v2)
        (D v1, D v2) -> B (v1 > v2)
    -- Equality check
    Eq e1 e2  -> case (expr e1 s, expr e2 s) of
        (I v1, I v2) -> B (v1 == v2)
        (D v1, D v2) -> B (v1 == v2)
        (B v1, B v2) -> B (v1 == v2)
        (S v1, S v2) -> B (v1 == v2)
    -- Call function
    Call ref es -> call ref es s
    -- Lit
    Lit v   -> v
    -- Get existing variable
    Get ref   -> get ref s

--
-- TYPE CHECKING
--

typeCheck :: Prog -> Bool
typeCheck _ = True

--
-- BINARY OPERATORS
--

op :: Op -> Var -> Var -> Var
-- Addition
op Add (I s1) (I s2) = I (s1 + s2)
op Add (D s1) (D s2) = D (s1 + s2)
op Add (S s1) (S s2) = S (s1 ++ s2)
-- Subtraction
op Sub (I s1) (I s2) = I (s1 - s2)
op Sub (D s1) (D s2) = D (s1 - s2)
-- TODO: op Sub (String s1) (String s2) = String (s1 -- s2)
-- Multiplication
op Mul (I s1) (I s2) = I (s1 * s2)
op Mul (D s1) (D s2) = D (s1 * s2)
-- Division
op Div (I s1) (I s2) = if s2 /= 0
    then I (s1 `div` s2) else error "ERROR: attempt to divide by 0"
op Div (D s1) (D s2) = if s2 /= 0 
    then D (s1 / s2) else error "ERROR: attempt to divide by 0"
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
    (I i) -> [Declare ref (BinOp o (Lit (I i)) (Lit (I 1)))]
    (D d) -> [Declare ref (BinOp o (Lit (D d)) (Lit (D 1)))]

-- Pulls the value out of a bool variable
valBool :: Var -> Bool
valBool (B b) = b

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
while e p s = if valBool (expr e s) then while e p (run p s) else s

-- A for loop of the form `for (declaration; condition; iterator) {prog}`
for :: (Name, Expr) -> Expr -> CmdIter -> Prog -> State -> State
for (ref, e) cmp iter p s = 
    let s' = set ref (expr e s) s
        p' = (Iterate iter ref) : p
    in while cmp p' s' 

--
-- FUNCTIONS
--

-- First create a state with the function's parameters and then run the function
-- Our functions have static scoping and are pass by value
call :: Name -> Params -> State -> Var
call ref prms s = case get ref s of
    (F _ fVars body) ->
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
addProg = run [Declare "num" (Lit (I 23)), 
               Declare "num2" (Lit (I 24)), 
               Declare "Result" (BinOp Add (Get "num") (Get "num2"))] s0
         
subProg = run [Declare "num" (Lit (I 23)), 
               Declare "num2" (Lit (I 24)), 
               Declare "Result" (BinOp Sub (Get "num") (Get "num2"))] s0        
           
mulProg = run [Declare "num" (Lit (I 23)), 
               Declare "num2" (Lit (I 24)), 
               Declare "Result" (BinOp Mul (Get "num") (Get "num2"))] s0      

divProg = run [Declare "num" (Lit (I 23)), 
               Declare "num2" (Lit (I 24)), 
               Declare "Result" (BinOp Div (Get "num") (Get "num2"))] s0        

divZeroProg = run [Declare "num" (Lit (I 23)), 
                   Declare "num2" (Lit (I 0)), 
                   Declare "Result" (BinOp Div (Get "num") (Get "num2"))] s0        

ifProg = run [Declare "true" (Lit (B True)), 
              If (Get "true") 
                  [(Declare "True" (Lit (I 1)))]
                  [(Declare "False" (Lit (I 0)))]] s0

ltProg = run [Declare "lt" (Lit (I 5)), 
              Declare "gt" (Lit (I 10)),
              If (Lt (Get "lt") (Get "gt"))
                 [Declare "True" (Lit (B True))] 
                 [Declare "False" (Lit (B False))]] s0

gtProg = run [Declare "lt" (Lit (I 5)), 
              Declare "gt" (Lit (I 10)), 
              If (Gt (Get "gt") (Get "lt"))
                 [Declare "True" (Lit (B True))] 
                 [Declare "False" (Lit (B False))]] s0

eqProg = run [Declare "1" (Lit (I 10)), 
              Declare "2" (Lit (I 10)), 
              If (Eq (Get "1") (Get "2"))
                 [Declare "True" (Lit (B True))] 
                 [Declare "False" (Lit (B False))]] s0

nullVarProg = run [Declare "i" (Get "null")] s0

iterProg = run [Declare "i" (Lit (D 0)),
               Iterate Inc "i",
               Iterate Inc "i", 
               Iterate Dec "i"] s0

whileProg = run [Declare "i" (Lit (I 0)),
                 While (Lt (Get "i") (Lit (I 5))) 
                     [Iterate Inc "i"]] s0

forProg = run [Declare "y" (Lit (I 10)),
               For ("i", Lit (I 0)) (Lt (Get "i") (Lit (I 7))) Inc
                   [Iterate Dec "y"]] s0

-- Function Type [(Type, Name)] Prog
funProg = run [Declare "fun" (Lit (F Int_ty [(Int_ty, "x")]
                   [Return (BinOp Add (Get "x") (Lit (I 3)))])),
               Declare "result" (Call "fun" [(Lit (I 5))])] s0
