import Test.Hspec
--import Test.QuickCheck

import ByteCode.Types
import ByteCode.Run

{- first example, like in questionnaire -}
testByteCode1 :: (Num a, Eq a, Ord a)  => ByteCode a
testByteCode1 = [ LOAD_VAL 1 , WRITE_VAR "x"
                , LOAD_VAL 2 , WRITE_VAR "y"
                ,  READ_VAR "x", LOAD_VAL 1 , ADD
                , READ_VAR "y",  MULTIPLY
                , RETURN_VALUE]
                
{- second example, with undefined variable -}
testByteCode2 :: (Num a, Eq a, Ord a)  => ByteCode a
testByteCode2 = [ LOAD_VAL 2,LOAD_VAL 1, SUBSTRACTION
                , READ_VAR "x", ADD
                , RETURN_VALUE]

{- 
third example, with while loop :
function f() {
    
    i = 3
    
    while  (i < 7):
      
      i = i + 1
      
    return i   
}
-}
testByteCode3 :: (Num a, Eq a, Ord a)  => ByteCode a 
testByteCode3 = [ LOAD_VAL 1, WRITE_VAR "i"
                , WHILE
                , READ_VAR "i", LOAD_VAL 7, GTE, NOT
                , DO
                , READ_VAR "i", LOAD_VAL 1, ADD, WRITE_VAR "i"
                , END
                , RETURN_VALUE] 


main :: IO ()
main = hspec $ do
  describe "Bytecode.runByteCode" $ do
    it "empty stack " $ do
      runByteCode [] `shouldBe` (Left $ BadInstr "empty instructions's Stack")
    it "first example, like in questionnaire" $ do 
      runByteCode testByteCode1 `shouldBe` (Right 4)
    it "second example, with undefined variable" $ do 
      runByteCode testByteCode2 `shouldBe` (Left $ UndefinedVariable "x" )
    it "third example, with while loop " $ do 
      runByteCode testByteCode3 `shouldBe` (Right 6)