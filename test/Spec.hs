{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Book Store Tests" [
    testCase "Empty input" $
        Lib2.parseQuery "" @?= Left "Invalid command",
    
    testCase "Invalid command" $
        Lib2.parseQuery "hello" @?= Left "Invalid command",
    
    testCase "Add book - valid input" $
        Lib2.parseQuery "add \"The Stranger\", Albert Camus, Fiction, 1942" @?= 
        Right (Lib2.AddQuery "The Stranger, Albert Camus, Fiction, 1942"),

    testCase "Add book - invalid title" $
        Lib2.parseQuery "add The Stranger, Albert Camus, Fiction, 1942" @?= 
        Left "Invalid Title syntax",

    testCase "Add book - invalid author" $   
        Lib2.parseQuery "add \"The Stranger\", Albert 111, Fiction, 1942" @?= 
        Left "Invalid Author",
    
    testCase "Add book - invalid year" $
        Lib2.parseQuery "add \"The Stranger\", Albert Camus, Fiction, 1" @?= 
        Left "Invalid Year",
    
    testCase "Add book - invalid genre" $
        Lib2.parseQuery "add \"The Stranger\", Albert Camus, aaaaaa, 1942" @?= 
        Left "Invalid Genre",
    
    testCase "Remove - valid id" $
        Lib2.parseQuery "remove 1" @?= Right (Lib2.RemoveQuery "1"),
    
    testCase "Remove - invalid id format" $
        Lib2.parseQuery "remove abc" @?= Left "Invalid ID",
    
    testCase "List command" $
        Lib2.parseQuery "list" @?= Right Lib2.ListQuery
    ]