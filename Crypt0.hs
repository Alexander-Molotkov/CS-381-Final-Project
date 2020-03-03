module Crypt0 where

import Data.Map 

-- State of program is the value of all of the variables
-- Semantic domain = State -> State = (Map Name Var) -> (Map Name Var)


-- TODO: Type checking

--
-- TYPE DECLARATIONS
--

type State = Map Name Var
type Prog  = [Cmd]
type Name  = String

-- Commands change the program's state
data Cmd = Declare Name Expr
         | If Expr Prog Prog
         | While Expr Prog
         | For Cmd Expr Expr Prog
         | Return Expr
    deriving (Eq,Show)

-- Expressions perform operations and return variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
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
data Op = Plus | Minus | Mult | Divi -- TODO: Divide by 0 case
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
    Add e1 e2 -> if isConst e1 && isConst e2 
        then case e1 of 
            Const (Int _)    -> Int (performOpInt Plus (valInt e1) (valInt e2))
            Const (Double _) -> Double (performOpDbl Plus (valDbl e1) (valDbl e2))
            Const (String _) -> String (performOpStr Plus (valStr e1) (valStr e2))
        else expr (Add (Const (expr e1 s)) (Const (expr e2 s))) s
    -- Subtraction
    Sub e1 e2 -> if isConst e1 && isConst e2 
        then case e1 of 
            Const (Int _)    -> Int (performOpInt Minus (valInt e1) (valInt e2))
            Const (Double _) -> Double (performOpDbl Minus (valDbl e1) (valDbl e2))
         -- TODO: "Str"      -> set r (String (performOpStr (valStr v1 s) (valStr v2 s) Minus)) s
        else expr (Sub (Const (expr e1 s)) (Const (expr e2 s))) s
    -- Multiplication
    Mul e1 e2 -> if isConst e1 && isConst e2 
        then case e1 of 
            Const (Int _)    -> Int (performOpInt Mult (valInt e1) (valInt e2))
            Const (Double _) -> Double (performOpDbl Mult (valDbl e1) (valDbl e2))
        else expr (Mul (Const (expr e1 s)) (Const (expr e2 s))) s
    -- Division
    Div e1 e2 -> if isConst e1 && isConst e2 
        then case e1 of 
            Const (Int _)    -> Int (performOpInt Divi (valInt e1) (valInt e2))
            Const (Double _) -> Double (performOpDbl Divi (valDbl e1) (valDbl e2))
        else expr (Div (Const (expr e1 s)) (Const (expr e2 s))) s
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

performOpInt :: Op -> Int -> Int -> Int
performOpInt o x y = 
    case o of
      Plus  -> x + y
      Minus -> x - y
      Mult  -> x * y
      Divi  -> x `div` y

valInt :: Expr -> Int
valInt (Const (Int x)) = x

performOpDbl :: Op -> Double -> Double -> Double
performOpDbl o x y = 
    case o of
      Plus  -> x + y
      Minus -> x - y
      Mult  -> x * y
      Divi  -> x / y

valDbl :: Expr -> Double
valDbl (Const (Double x)) = x

performOpStr :: Op -> String -> String -> String
performOpStr o x y = 
    case o of
      Plus  -> x ++ y
      --TODO: Minus -> x - y

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
-- For (declaration expression; condition expression; iterator expression) {prog}
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
-- | Multiplies two vars
-- | Divide two vars
-- | If statement

--
-- MANUAL TESTING
--

--run <program> s0
prog = run [Declare "v1" (Const (Int 23)), 
            Declare "v2" (Const (Int 56)), 
            Declare "sub" (Sub (Get "v1") (Get "v2")),
            Declare "sumsum" (Add (Get "sub") (Mul (Get "v1") (Get "v2")))] s0

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

function = [Declare "f" (Const (Function Int_ty [(Int_ty, "num1"), (Int_ty, "num2")] 
               [Return (Add (Get "num1") (Get "num2"))]))]
functionCall = [Declare "result" (Call "f" [Const (Int 31), Const (Int 12)])]
funProg = run functionCall (run function s0)

whileProg = run [Declare "i" (Const (Int 5)), 
                 While (Lt (Const (Int 0)) (Get "i")) 
                     [Declare "i" (Sub (Get "i") (Const (Int 1))) ]] s0

--For (Name, Expr) Expr Expr Prog
forProg = run [Declare "x" (Const (Int 20)),
               For (Declare "i" (Const (Int 0))) (Lt (Get "i") (Const (Int 10))) (Inc (Get "i"))
                   [Declare "x" (Dec (Get "x"))]] s0
