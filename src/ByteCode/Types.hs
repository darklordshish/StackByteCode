module ByteCode.Types where

{- 
ByteCode Instructions : first part with base arithmetic instructions
second - with While loops
-}
data ByteInstr a 
  = LOAD_VAL a 
  | WRITE_VAR String
  | READ_VAR String
  | ADD 
  | MULTIPLY
  | SUBSTRACTION
  | RETURN_VALUE
  | WHILE  -- Instructions  
  | DO     -- needed to the
  | END    -- while loop  
  | GTE    -- Instructions
  | LTE    -- needed for the formulation 
  | EQU    -- of  conditions
  | NOT    -- main 
  | AND    -- logic
  | OR     -- operators
  deriving (Show,Eq)
  
type ByteCode a = [ByteInstr a] 

{- Type for extract loops -}
data ByteLoop a 
  = While 
  { conditions :: ByteCode a
  , body :: ByteCode a
  } deriving (Eq,Show)

{- Error Type -}   
data EvalError 
  = UndefinedVariable String 
  | BadValue String 
  | BadStack String
  | BadInstr String
  | BadWhile String
  deriving (Show,Eq)

{- Program's Stack
with nameless constants and named variabels

BStack extended with posible Bool values 
here Either not as Monad but as coproduct
-}  
type Stack a = [(String, a)]

type BStack a = Stack (Either Bool a)