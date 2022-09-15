module ByteCode.Run where 

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Data.Either (isRight, fromRight)
import Data.List (lookup, span) 

import ByteCode.Types

{- run ByteCode  with loops-}                                                                                
runByteCode :: (Num a, Eq a, Ord a) => ByteCode a -> Either EvalError a
runByteCode [] = Left $ BadInstr "empty instructions's Stack"
runByteCode bcs = let seqInstr = extractLoops bcs 
                   in case runExcept seqInstr of
                        Left err    -> Left err
                        Right instr -> let errSt = foldM exec [] instr
                                       in case runExcept errSt of
                                            Left err' -> Left err'
                                            Right st -> Right $ (snd . head) st
  where 
    exec ::(Num a, Eq a, Ord a) =>  
         Stack a  
      -> Either (ByteLoop a) (ByteCode a) 
      -> Except EvalError (Stack a)
    exec st (Right bcd) = bStackToStack <$> (foldM (flip tickLoop) (stackToBStack st) bcd)
    exec st (Left bcl) = execWhile st bcl 

                
{- execute monadic action until predicate is failed-}                
iterateUntilM :: (Monad m) =>(a -> Bool) -> (a -> m a) -> a -> m a  
iterateUntilM p f v = do
  v' <- f v 
  if p v' 
  then 
    f v >>= iterateUntilM p f
  else 
    return v       
     
{- convertion Stac a <-> BStack a -}           
stackToBStack ::(Num a) => Stack a -> BStack a
stackToBStack  = ((Right  <$> ) <$>)

bStackToStack :: (Num a) => BStack a -> Stack a
bStackToStack bst = (\(st,y) -> (st,fromRight 0 y)) <$> [x | x <- bst
                                                           , isRight (snd x)]

{- execute While loop with Stack -}      
execWhile :: (Num a, Eq a, Ord a) =>  
     Stack a 
  -> ByteLoop a 
  -> Except EvalError (Stack a) 
execWhile st btl = bStackToStack <$> endBState
  where 
    condInst = conditions btl
    bodyInst = body btl
    startBState = stackToBStack st
    endBState = iterateUntilM checkCond execBody startBState
    --execCond :: Num a => BStack a -> Except EvalError (BStack a)
    execCond bst = foldM (flip tickLoop) bst condInst
    --execBody :: Num a => BStack a -> Except EvalError (BStack a)
    execBody bst = foldM (flip tickLoop) bst bodyInst
    -- checkCond :: Num a => BStack a -> Bool
    checkCond bst = case runExcept $ execCond bst of
                      Right ((str, Left True):xs)  -> True
                      Right ((str, Left False):xs) -> False
                      _                            -> False

{-It seporate ByteCode by fragments with loops extracted -}
extractLoops :: (Num a, Eq a, Ord a) =>  
     ByteCode a 
  -> Except EvalError [Either (ByteLoop a) (ByteCode a)]
extractLoops []  = liftEither $ Right []
extractLoops bcs = 
  let (xs,ys)  = span (/= WHILE) bcs
  in case ys of
      [] -> liftEither $ Right [Right bcs]
      (WHILE:ys') -> let (ys'',zs) = span (/= DO) ys' 
        in case zs of
             [] -> throwError $ BadWhile "incorrect Loop,missed DO"
             (DO:zs')  -> let (zs'', rs) = span (/= END) zs' 
               in case rs of
                    [] -> throwError $ BadWhile "incorrect Loop,missed END"
                    (END:rs') -> do 
                      rest <- extractLoops rs'
                      return ([Right xs,Left $ While ys'' zs'']++rest)
                                      
-- step by arithmetic instructions with loops
tickLoop :: (Num a, Eq a, Ord a) => 
     ByteInstr  a 
  -> BStack a 
  -> Except EvalError (BStack a)
tickLoop (LOAD_VAL x) xs           = liftEither $ Right $ ("",  Right x) : xs
tickLoop (WRITE_VAR str) ys    = case ys of
  ((_,Right x):xs) -> liftEither $ Right $ (str, Right x) : xs
  _                -> throwError $ BadValue "var type should be Numeric"
tickLoop (READ_VAR str) xs         = readVar str xs  
tickLoop RETURN_VALUE (x:_)        = liftEither $ Right $ return ("", snd x )
tickLoop ADD ys          = case ys of 
  ((st1,Right x):(st2,Right y):xs) -> liftEither $ Right $ ("",Right (y + x)):xs
  _                                -> throwError $ BadInstr "ADD"                  
tickLoop MULTIPLY ys     = case ys of 
  ((st1,Right x):(st2,Right y):xs) -> liftEither $ Right $ ("",Right (y * x)):xs
  _                                -> throwError $ BadInstr "MULTIPLY"
tickLoop SUBSTRACTION ys = case ys of 
  ((st1,Right x):(st2,Right y):xs) -> liftEither $ Right $ ("",Right (y - x)):xs
  _                                -> throwError $ BadInstr "SUBSTRACTION"
tickLoop GTE ys    = case ys of 
  (("",Right x):("",Right y):xs) -> liftEither $ Right $ ("",Left (y >= x)):xs
  (x'@(str1,Right x):("",Right y):xs) -> liftEither $ Right $ ("",Left (y >= x)):x':xs
  (("",Right x):y'@(st2,Right y):xs) -> liftEither $ Right $ ("",Left (y >= x)):y':xs
  (x'@(st1,Right x):y'@(st2,Right y):xs) -> liftEither $ Right $ ("",Left (y >= x)):x':y':xs
  _                                -> throwError $ BadInstr "GTE"
tickLoop LTE ys    = case ys of 
  (("",Right x):("",Right y):xs) -> liftEither $ Right $ ("",Left (y <= x)):xs
  (x'@(str1,Right x):("",Right y):xs) -> liftEither $ Right $ ("",Left (y <= x)):x':xs
  (("",Right x):y'@(st2,Right y):xs) -> liftEither $ Right $ ("",Left (y <= x)):y':xs
  (x'@(st1,Right x):y'@(st2,Right y):xs) -> liftEither $ Right $ ("",Left (y <= x)):x':y':xs
  _                                -> throwError $ BadInstr "LTE"
tickLoop EQU ys    = case ys of 
  (("",Right x):("",Right y):xs) -> liftEither $ Right $ ("",Left (y == x)):xs
  (x'@(str1,Right x):("",Right y):xs) -> liftEither $ Right $ ("",Left (y == x)):x':xs
  (("",Right x):y'@(st2,Right y):xs) -> liftEither $ Right $ ("",Left (y == x)):y':xs
  (x'@(st1,Right x):y'@(st2,Right y):xs) -> liftEither $ Right $ ("",Left (y == x)):x':y':xs
  _                                -> throwError $ BadInstr "EQU"
tickLoop WHILE ys  = throwError $ BadWhile "incorrect Loop instruction WHILE"
tickLoop DO ys     = throwError $ BadWhile "incorrect Loop instruction DO"
tickLoop END ys    = throwError $ BadWhile "incorrect Loop instruction END"
tickLoop NOT  ys   = case ys of 
  ((str,Left x):xs) -> liftEither $ Right $ (str,Left (not x)):xs
  _                 -> throwError $ BadInstr "NOT"
tickLoop OR ys     =  case ys of 
  ((st1,Left x):(st2,Left y):xs) -> liftEither $ Right $ ("",Left (y || x)):xs
  _                              -> throwError $ BadInstr "OR"
tickLoop AND ys    =  case ys of 
  ((st1,Left x):(st2,Left y):xs) -> liftEither $ Right $ ("",Left (y && x)):xs
  _                              -> throwError $ BadInstr "AND"
tickLoop _ []      = throwError $ BadStack "empty BStack"
--tickLoop _ _       = throwError $ BadStack "bad stack or instructions"

{- lift up first finded directive with the variable   -}    
readVar :: String -> Stack a  -> Except EvalError (Stack a)
readVar str xs = case lookup str xs of
                   Nothing  ->  throwError $ UndefinedVariable str
                   Just _   -> let (ys,(x:zs)) = span (( /= str) . fst)  xs
                               in liftEither $ Right $ (x:ys) ++ zs
                            
