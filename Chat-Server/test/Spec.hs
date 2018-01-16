--import Test.QuickCheck
import Test.HUnit
import Test.QuickCheck

import Data.Text (Text)
import qualified Data.Text as T

import ChatServer

testHate1 = TestCase $ assertEqual "test if text contains hate speech" (False) (checkIfContainsRage $ T.pack "We are in lovin' mode")
testHate2 = TestCase $ assertEqual "test if text contains hate speech" (True) (checkIfContainsRage $ T.pack "I hate you")
testHate3 = TestCase $ assertEqual "test if text contains hate speech" (True) (checkIfContainsRage $ T.pack "I cant love you, I hate you")
testHate4 = TestCase $ assertEqual "test if text contains hate speech" (False) (checkIfContainsRage $ T.pack "love for stupid")

testCats1 = TestCase $ assertEqual "test if text contains loving dogs" (True) (checkIfNotLovingCats $ T.pack "I love dog")
testCats2 = TestCase $ assertEqual "test if text contains loving dogs" (True) (checkIfNotLovingCats $ T.pack "I love dogs")
testCats3 = TestCase $ assertEqual "test if text contains loving dogs" (False) (checkIfNotLovingCats $ T.pack "I hate dogs")
testCats4 = TestCase $ assertEqual "test if text contains loving dogs" (False) (checkIfNotLovingCats $ T.pack "I love cats")

testNewServer1 = TestCase $ assertEqual "test creating newServer"  (True) ((length newServerState) == 0)
testNewServer2 = TestCase $ assertEqual "test creating newServer"  (True) ((length newServerState) == 4)

testList = TestList [testHate1, testHate2, testHate3, testHate4, testCats1, testCats2, testCats3, testCats4, testNewServer1 ]
main :: IO ()
main = do
    runTestTT testList

    quickCheck $ checkIfNotLovingCats $ T.pack "I love dog"
    quickCheck $ checkIfContainsRage $ T.pack "I cant love you, I hate you"
    quickCheck $ (length newServerState) == 0
    return ()
