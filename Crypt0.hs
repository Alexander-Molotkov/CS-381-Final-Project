module Crypt0 where

import Data.Map 

-- State of program is the value of all of the variables
-- Semantic domain = State -> State = (Map Name (Type, Expr)) -> (Map Name (Type, Expr))


-- What are the programs that could be written in the language
-- Think about having declarations all up front

-- TODO: Type checking

--
-- TYPE DECLARATIONS
--

-- Map Name Expr -> get rid of CONST
-- Tuple of expressions and variables
type State = Map Name (Type, Expr)
type Prog  = [Cmd]
type Name  = String

-- Commands change the program's state
data Cmd = Declare Name (Type, Expr)
         | If Expr Prog Prog
         | While Expr Prog
         | For Cmd Expr Expr Prog -- For (Return Expr) case
         | Return Expr -- Move over to expressions
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
          | Get Name

          -- Const Var
          | I Int
          | D Double
          | B Bool
          | S String
          -- | F Function 
    deriving (Eq,Show)

-- Keep this in the state, don't actually have this at all
data Var = Int Int
         | Double Double
         | Bool Bool
         | String String
          -- Want a return statement inside of that data type
          -- Other way to keep track of functions?
         | Function Type [(Type, Name)] Prog
    deriving (Eq,Show)

data Op = Plus | Minus | Mult | Divi -- TODO: Divide by 0 case
    deriving (Eq,Show)

--            Int  | Double |  Bool  | String | Function
data Type = TInt | TDbl | TBool | TStr | TFunc | TErr
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
    Declare ref e -> set ref e s
    -- If statement
    If e c1 c2    -> ifStatement e c1 c2 s
    -- While loop 
    While e c     -> whileLoop e c s
    -- For loop
    For d c it p  -> forLoop d c it p s

expr :: Expr -> State -> Var
expr e s = case e of
    -- Addition
    Add e1 e2 -> undefined
    -- Subtraction
    Sub e1 e2 -> undefined
    -- Multiplication 
    Mul e1 e2 -> undefined
    -- Division 
    Div e1 e2 -> undefined
    -- Less than
    Lt e1 e2  -> undefined
    -- Greater than
    Gt e1 e2  -> undefined
    -- Equality check
    Eq e1 e2  -> undefined
    -- Call function
    Call ref es -> undefined
    -- Get existing variable
    Get ref   -> undefined

--
-- EXPRESSION HELPER FUNCTIONS
--

performOpInt :: Op -> Int -> Int -> Int
performOpInt o x y = 
    case o of
      Plus  -> x + y
      Minus -> x - y
      Mult  -> x * y
      Divi  -> x `div` y

performOpDbl :: Op -> Double -> Double -> Double
performOpDbl o x y = 
    case o of
      Plus  -> x + y
      Minus -> x - y
      Mult  -> x * y
      Divi  -> x / y

performOpStr :: Op -> String -> String -> String
performOpStr o x y = 
    case o of
      Plus  -> x ++ y
      --TODO: Minus -> x - y

--
-- VARIABLE MANIPULATION
--

-- Returns a variable by name
get :: Name -> State -> (Type, Expr)
get ref s = case Data.Map.lookup ref s of
    Just v    -> v
    otherwise -> error ("ERROR: Variable not in scope: " ++ ref)

-- Changes the value of a variable on the stack
set :: Name -> (Type, Expr) -> State -> State
set ref v s = insert ref v s

-- Returns type of an expression
typeOf :: Expr -> State -> Type
typeOf = undefined

-- Removes a variable from scope
removeVar :: Name -> State -> State
removeVar key s = delete key s

--
-- CONDITIONAL STATEMENTS
--

-- If statement
ifStatement :: Expr -> Prog -> Prog -> State -> State
ifStatement e c1 c2 s = undefined

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
fetchParams ((typ, ref):ps) (e:es) s = undefined -- TODO
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
whileLoop = undefined

-- For Loop
-- NOTE: For loops do not create their own scope -> TODO?
-- For (declaration expression; condition expression; iterator expression) {prog}
forLoop :: Cmd -> Expr -> Expr -> Prog -> State -> State
forLoop = undefined
  

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
prog = undefined

ifProg = undefined

ltProg = undefined

gtProg = undefined

eqProg = undefined

funProg = undefined

whileProg = undefined

--For (Name, Expr) Expr Expr Prog
forProg = undefined
