module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int


{- Function which creates a new state from the old state, and the required input and output. -}
extend :: State -> String -> Int -> State
extend state var val = newState where newState x | x == var  = val
                                                 | otherwise = state x

{- The empty state is a function which takes a string and always returns 0. -}
empty :: State
empty = (\var -> 0)


{- Takes the required state, and uses it to evaluate the expression. -}
evalE :: State -> Expression -> Int
evalE state (Var x)      = state x
evalE state (Val x)      = x
evalE state (Op x bop y) = (operation bop) (evalE state x) (evalE state y)

{- Takes an operation in SImPL and translates it into an operation in Haskell. -}
operation :: Bop -> (Int -> Int -> Int)
operation bop 
    | bop == Plus    = (+)
    | bop == Minus   = (-)
    | bop == Times   = (*)
    | bop == Divide  = div
    | bop == Gt      = (\x y -> if x > y then 1 else 0)
    | bop == Ge      = (\x y -> if x >= y then 1 else 0)
    | bop == Lt      = (\x y -> if x < y then 1 else 0)
    | bop == Le      = (\x y -> if x >= y then 1 else 0)
    | bop == Eql     = (\x y -> if x == y then 1 else 0)


{- The original statement data type has increments and for loops, which are just syntactic sugar 
   for assignments and while loops respectively. DietStatement is a statement which is desugared. -}
data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

{- Desugars a statement into a DietStatement. -}
desugar :: Statement -> DietStatement
desugar (Assign var val)                    = DAssign var val
desugar (Incr var)                          = DAssign var (Op (Var var) Plus (Val 1))
desugar (If expr if_then if_else)           = DIf expr (desugar if_then) (desugar if_else)
desugar (While expr while_do)               = DWhile expr (desugar while_do)
desugar (For init_var expr for_incr for_do) = DWhile expr (desugar (Sequence for_incr for_do))
desugar (Sequence first second)             = DSequence (desugar first) (desugar second)
desugar (Skip)                              = DSkip



evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign var val) = extend state var (evalE state val)
  
evalSimple state (DIf condition if_then if_else)  
    | evalE state condition == 1 = evalSimple state if_then
    | evalE state condition == 0 = evalSimple state if_else  

evalSimple state (DWhile condition while_do)  
    | evalE state condition == 1 = evalSimple state while_do
    | evalE state condition == 0 = state 

evalSimple state (DSequence first second) = evalSimple (evalSimple state first) second
evalSimple state (DSkip)                  = empty

run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)
