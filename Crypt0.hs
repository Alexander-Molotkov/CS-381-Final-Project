module Crypt0 where

import Data.Map 

-- State of program is the value of all of the variables
-- Semantic domain = State -> State = (Map Name Var) -> (Map Name Var)


-- TODO: Type checking
-- TODO: Divide by zero

--
-- TYPE DECLARATIONS
--

type State = Map Name Var
type Prog  = [Cmd]
type Name  = String

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

data Var = Int Int
         | Double Double
         | Bool Bool
         | String String
         | Function Type [(Type, Name)] Prog
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
run (c:cs) s = run cs (cmd c s)
run [] s = s 

cmd :: Cmd -> State -> State
cmd command s = case command of
    -- Variable declaration
    Declare ref e   -> set ref (expr e s) s
    -- If statement
    If e c1 c2      -> case expr e s of
        (Bool b)  -> if b then run c1 s else run c2 s
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
        (Int v1, Int v2)       -> Bool (v1 < v2)
        (Double v1, Double v2) -> Bool (v1 < v2)
    -- Greater than check
    Gt e1 e2  -> case (expr e1 s, expr e2 s) of
        (Int v1, Int v2)       -> Bool (v1 > v2)
        (Double v1, Double v2) -> Bool (v1 > v2)
    -- Equality check
    Eq e1 e2  -> case (expr e1 s, expr e2 s) of
        (Int v1, Int v2)       -> Bool (v1 == v2)
        (Double v1, Double v2) -> Bool (v1 == v2)
        (Bool v1, Bool v2)     -> Bool (v1 == v2)
        (String v1, String v2) -> Bool (v1 == v2)
    -- Call function
    Call ref es -> undefined
    -- Lit
    Lit v   -> v
    -- Get existing variable
    Get ref   -> get ref s

--
-- Binary Operators
--

op :: Op -> Var -> Var -> Var
op Add (Int s1) (Int s2)       = Int    (s1 + s2)
op Add (Double s1) (Double s2) = Double (s1 + s2)
op Add (String s1) (String s2) = String (s1 ++ s2)
op Sub (Int s1) (Int s2)       = Int    (s1 - s2)
op Sub (Double s1) (Double s2) = Double (s1 - s2)
-- TODO: op Sub (String s1) (String s2) = String (s1 -- s2)
op Mul (Int s1) (Int s2)       = Int    (s1 * s2)
op Mul (Double s1) (Double s2) = Double (s1 * s2)
op Div (Int s1) (Int s2)       = if s2 /= 0
    then Int (s1 `div` s2) else error "ERROR: attempt to divide by 0"
op Div (Double s1) (Double s2) = if s2 /= 0 
    then Double (s1 / s2)  else error "ERROR: attempt to divide by 0"
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
    (Int i)    -> [Declare ref (BinOp o (Lit (Int i)) (Lit (Int 1)))]
    (Double d) -> [Declare ref (BinOp o (Lit (Double d)) (Lit (Double 1)))]

-- Pulls the value out of a bool variable
valBool :: Var -> Bool
valBool (Bool b) = b

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

ifProg = run [Declare "true" (Lit (Bool True)), 
              If (Get "true") 
                  [(Declare "True" (Lit (Int 1)))]
                  [(Declare "False" (Lit (Int 0)))]] s0

ltProg = run [Declare "lt" (Lit (Int 5)), 
              Declare "gt" (Lit (Int 10)),
              If (Lt (Get "lt") (Get "gt"))
                 [Declare "True" (Lit (Bool True))] 
                 [Declare "False" (Lit (Bool False))]] s0

gtProg = run [Declare "lt" (Lit (Int 5)), 
              Declare "gt" (Lit (Int 10)), 
              If (Gt (Get "gt") (Get "lt"))
                 [Declare "True" (Lit (Bool True))] 
                 [Declare "False" (Lit (Bool False))]] s0

eqProg = run [Declare "1" (Lit (Int 10)), 
              Declare "2" (Lit (Int 10)), 
              If (Eq (Get "1") (Get "2"))
                 [Declare "True" (Lit (Bool True))] 
                 [Declare "False" (Lit (Bool False))]] s0

nullVarProg = run [Declare "i" (Get "null")] s0

iterProg = run [Declare "i" (Lit (Double 0)),
               Iterate Inc "i",
               Iterate Inc "i", 
               Iterate Dec "i"] s0

whileProg = run [Declare "i" (Lit (Int 0)),
                 While (Lt (Get "i") (Lit (Int 5))) 
                     [Iterate Inc "i"]] s0

forProg = run [Declare "y" (Lit (Int 10)),
               For ("i", Lit (Int 0)) (Lt (Get "i") (Lit (Int 7))) Inc
                   [Iterate Dec "y"]] s0

funProg = undefined
