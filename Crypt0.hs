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
data Cmd =
           Declare Name Expr
         | If Expr Prog Prog
         | While Expr Prog
         | For Cmd Expr Expr Prog
         | Return Expr
    deriving (Eq,Show)

-- Expressions perform operations and return variables
data Expr = 
            BinOp Op Expr Expr
          | Call Name [Expr]
          | Lt Expr Expr
          | Gt Expr Expr 
          | Eq Expr Expr
          | Inc Expr
          | Dec Expr
          | Const Var
          | Get Name
    deriving (Eq,Show)

data Var = Int Int
         | Double Double
         | Bool Bool
         | String String
         | Function Type [(Type, Name)] Prog
    deriving (Eq,Show)

-- The arithmetic operators that we support
data Op = Add | Sub | Mul | Div -- TODO: Divde by 0 case
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
    Declare ref e -> set ref (expr e s) s
    -- If statement
    If e c1 c2    -> ifStatement e c1 c2 s
    -- While loop 
    While e c     -> whileLoop e c s
    -- For loop
    For d c it p  -> forLoop d c it p s

expr :: Expr -> State -> Var
expr e s = case e of
    -- Addition
    BinOp o e1 e2 -> case (expr e1 s, expr e2 s) of
        (v1, v2)             -> op o v1 v2

    -- Less than check
    Lt e1 e2  -> if isConst e1 && isConst e2
        then case e1 of
            Const (Int _)    -> 
               if valInt e1 < valInt e2 then Bool True else Bool False
            Const (Double _) -> 
               if valDbl e1 < valDbl e2 then Bool True else Bool False
        else expr (Lt (Const (expr e1 s)) (Const (expr e2 s))) s
    -- Greater than check
    Gt e1 e2  -> if isConst e1 && isConst e2
        then case e1 of
            Const (Int _)    -> 
               if valInt e1 > valInt e2 then Bool True else Bool False
            Const (Double _) -> 
               if valDbl e1 > valDbl e2 then Bool True else Bool False
        else expr (Gt (Const (expr e1 s)) (Const (expr e2 s))) s
    -- Equality check
    Eq e1 e2  -> if isConst e1 && isConst e2
        then case e1 of
            Const (Int _)    -> 
               if valInt e1 == valInt e2 then Bool True else Bool False
            Const (Double _) -> 
               if valDbl e1 == valDbl e2 then Bool True else Bool False
        else expr (Eq (Const (expr e1 s)) (Const (expr e2 s))) s
    -- Increment
    Inc e     -> if isConst e 
        then case e of
            Const (Int _)    -> (Int ((valInt e) + 1))
            Const (Double _) -> (Double ((valDbl e) + 1))
        else expr (Inc (Const (expr e s))) s
    -- Decrement
    Dec e     -> if isConst e 
        then case e of
            Const (Int _)    -> (Int ((valInt e) - 1))
            Const (Double _) -> (Double ((valDbl e) - 1))
        else expr (Dec (Const (expr e s))) s 

    -- Call function
    Call ref es -> call (get ref s) es s
    -- Constant
    Const v   -> v
    -- Get existing variable
    Get ref   -> get ref s

--
-- EXPRESSION HELPER FUNCTIONS
--

isConst :: Expr -> Bool
isConst (Const _) = True
isConst _         = False

op :: Op -> Var -> Var -> Var
op Add (Int s1) (Int s2)       = Int    (s1 + s2)
op Add (Double s1) (Double s2) = Double (s1 + s2)
op Add (String s1) (String s2) = String (s1 ++ s2)
op Sub (Int s1) (Int s2)       = Int    (s1 - s2)
op Sub (Double s1) (Double s2) = Double (s1 - s2)
-- TODO: op Sub (String s1) (String s2) = String (s1 -- s2)
op Mul (Int s1) (Int s2)       = Int    (s1 * s2)
op Mul (Double s1) (Double s2) = Double (s1 * s2)
op Div (Int s1) (Int s2)       = Int    (s1 `div` s2)
op Div (Double s1) (Double s2) = Double (s1 / s2)
   
 
valInt :: Expr -> Int
valInt (Const (Int x)) = x

valDbl :: Expr -> Double
valDbl (Const (Double x)) = x

valStr :: Expr -> String
valStr (Const (String x)) = x

valBool :: Expr -> Bool
valBool (Const (Bool x)) = x

--
-- VARIABLE MANIPULATION
--

-- Returns a variable by name
get :: Name -> State -> Var 
get key s = s ! key
-- TODO: Variable does not exist case

-- Changes the value of a variable on the stack
set :: Name -> Var -> State -> State
set key v s = (insert key v s)

-- Returns type of a variable
typeOf :: Var -> State -> Type
typeOf (Int _) s    = Int_ty
typeOf (Bool _) s   = Bul_ty
typeOf (Double _) s = Dbl_ty
typeOf (String _) s = Str_ty

-- Removes a variable from scope
removeVar :: Name -> State -> State
removeVar key s = delete key s

--
-- CONDITIONAL STATEMENTS
--

-- If statement
ifStatement :: Expr -> Prog -> Prog -> State -> State
ifStatement e c1 c2 s = case expr e s of
    (Bool b)  -> if b then run c1 s else run c2 s
    otherwise -> ifStatement (Const (expr e s)) c1 c2 s

--
-- FUNCTIONS
--

-- Call a function
call :: Var -> [Expr] -> State -> Var
call (Function typ params body) passing s = 
    doFunc body (fetchParams params passing s)

-- Gets the parameters that are passed by reference in a function call
-- Returns a state with just those parameters - the 'scope' of the function
fetchParams :: [(Type, Name)] -> [Expr] -> State -> State
fetchParams ((typ, ref):ps) (e:es) s =
    (set ref (expr e s) empty) `union` (fetchParams ps es s)
fetchParams [] [] s = s
fetchParams _ [] _  = error "Error: Too many parameters passed to function."
fetchParams [] _ _  = error "Error: Too few parameters passed to function."

-- Actually carries out the function body
doFunc :: Prog -> State -> Var
doFunc (c:cs) s = case c of
    Return e  -> (expr e s) 
    otherwise -> doFunc cs (cmd c s)
doFunc [] _     = error "Error: No Return Statement from function"

--
-- SYNTACTIC SUGAR
--

-- While Loop
-- NOTE: While loops do not create their own scope -> TODO?
whileLoop :: Expr -> Prog -> State -> State
whileLoop e c s = case expr e s of
    (Bool b)  -> if b then whileLoop e c (run c s) else s
    otherwise -> whileLoop (Const (expr e s)) c s

-- For Loop
-- NOTE: For loops do not create their own scope -> TODO?
-- For (declaration; condition; iterator) {prog}
forLoop :: Cmd -> Expr -> Expr -> Prog -> State -> State
forLoop decCmd condEx iterEx p s = 
    let s' = cmd decCmd s
        p' = case decCmd of 
            (Declare ref ex) -> p ++ [Declare ref iterEx]
    in whileLoop condEx p' s'
  

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
addProg = [Declare "num" (Const (Int 23)), 
           Declare "num2" (Const (Int 24)), 
           Declare "Result" (BinOp Add (Get "num") (Get "num2"))]
         
subProg = [Declare "num" (Const (Int 23)), 
             Declare "num2" (Const (Int 24)), 
             Declare "Result" (BinOp Sub (Get "num") (Get "num2"))]         
           
mulProg = [Declare "num" (Const (Int 23)), 
           Declare "num2" (Const (Int 24)), 
           Declare "Result" (BinOp Mul (Get "num") (Get "num2"))]         

divProg = [Declare "num" (Const (Int 23)), 
           Declare "num2" (Const (Int 24)), 
           Declare "Result" (BinOp Div (Get "num") (Get "num2"))]         



ifProg = run [Declare "true" (Const (Bool True)), 
              If (Get "true") 
                  [(Declare "True" (Const (Int 1)))]
                  [(Declare "False" (Const (Int 0)))]] s0

ltProg = run [Declare "lt" (Const (Int 5)), 
              Declare "gt" (Const (Int 10)),
              If ( Lt (Get "lt") (Get "gt"))
                  [Declare "True" (Const (Bool True))] 
                  [Declare "False" (Const (Bool False))]] s0

gtProg = run [Declare "lt" (Const (Int 5)), 
              Declare "gt" (Const (Int 10)), 
              If ( Gt (Get "gt") (Get "lt"))
                  [Declare "True" (Const (Bool True))] 
                  [Declare "False" (Const (Bool False))]] s0

eqProg = run [Declare "1" (Const (Int 10)), 
              Declare "2" (Const (Int 10)), 
              If ( Eq (Get "1") (Get "2"))
                  [Declare "True" (Const (Bool True))] 
                  [Declare "False" (Const (Bool False))]] s0

funProg = undefined

whileProg = undefined

forProg = run [Declare "x" (Const (Int 20)),
               For (Declare "i" (Const (Int 0))) (Lt (Get "i") (Const (Int 15))) (Inc (Get "i"))
                   [Declare "x" (Dec (Get "x"))]] s0
