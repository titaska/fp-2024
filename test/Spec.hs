{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified


instance Arbitrary Lib2.BookGenre where
    arbitrary :: Gen Lib2.BookGenre
    arbitrary = elements [Lib2.Fantasy, Lib2.Scientific, Lib2.Detective, Lib2.Dictionary, Lib2.Fiction]

instance Arbitrary Lib2.BookInfo where
    arbitrary :: Gen Lib2.BookInfo
    arbitrary = Lib2.BookInfo 
        <$> (listOf1 $ elements ['A'..'Z'])
        <*> (listOf1 $ elements ['A'..'Z']) 
        <*> arbitrary                                               

instance Arbitrary Lib2.Query where
    arbitrary :: Gen Lib2.Query
    arbitrary = 
        Lib2.AddBookQuery <$> arbitrary  


instance Arbitrary Lib3.Statements where
    arbitrary :: Gen Lib3.Statements
    arbitrary = oneof [Lib3.Single <$> arbitrary, Lib3.Batch <$> listOf arbitrary]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,

    testCase "Parsing empty query" $
      Lib2.parseQuery "" @?= Left "No match",

    testCase "Parsing AddBookQuery" $ 
      Lib2.parseQuery "add TheStranger AlbertCamus Fiction" @?= 
        Right (Lib2.AddBookQuery 
          (Lib2.BookInfo "TheStranger" "AlbertCamus" Lib2.Fiction)
        ),

    testCase "Parsing RemoveBookQuery" $ 
      Lib2.parseQuery "remove TheStranger AlbertCamus Fiction" @?= 
        Right (Lib2.RemoveBookQuery 
          (Lib2.BookInfo "TheStranger" "AlbertCamus" Lib2.Fiction)
        ),
        
    testCase "Parsing invalid command" $
      Lib2.parseQuery "invalidCommand" @?= Left "No match"
  ]

propertyTests :: TestTree
propertyTests = testGroup "Lib3 Property Tests"
  [ testCase "Test single" $
        let s = Lib3.Single (Lib2.AddBookQuery (Lib2.BookInfo "TheStranger" "AlbertCamus" Lib2.Fiction)) 
         in Lib3.parseStatements (Lib3.renderStatements s) @?= Right (s, ""),

      testCase "Test batch" $
        let s1 = Lib2.AddBookQuery (Lib2.BookInfo "TheStranger" "AlbertCamus" Lib2.Fiction)
            s2 = Lib2.AddBookQuery (Lib2.BookInfo "Hobbit" "Tolkien" Lib2.Fiction)
            b = Lib3.Batch [s1, s2]
         in Lib3.parseStatements (Lib3.renderStatements b) @?= Right (b, ""),

      QC.testProperty "rendered and parsed" $
        \s -> Lib3.parseStatements (Lib3.renderStatements s) == Right (s, "")
       
  ]