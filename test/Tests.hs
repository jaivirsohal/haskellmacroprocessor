import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion, HasCallStack)

import MP

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "macro"
  [ testGroup "lookUp" (numberedTests lookUpTests)
  , testGroup "splitText" (numberedTests splitTextTests)
  , testGroup "combine" (numberedTests combineTests)
  , testGroup "getKeywordDefs" (numberedTests getKeywordDefsTests)
  , testGroup "expand" (numberedTests expandTests)
  ]

lookUpTests :: [Assertion]
lookUpTests = [ lookUp "A" [("A", 8), ("B",9), ("C",5), ("A",7)] --> [8,7]
              , lookUp "a" ([] :: [(String, Int)])               --> []
              , lookUp "a" [("a", 9)]                            --> [9]
              , lookUp "a" [("b", 9)]                            --> []
              ]

splitTextTests :: [Assertion]
splitTextTests = [ splitText " .," "A comma, then some words." --> (" ,   .", ["A","comma","","then","some","words",""])
                 , splitText "" ""                             --> ("", [""])
                 , splitText "." "A.B"                         --> (".", ["A","B"])
                 , splitText " " " A"                          --> (" ", ["", "A"])
                 ]

combineTests :: [Assertion]
combineTests = [ combine " ,   ." ["A","comma","","then","some","words",""] --> ["A"," ","comma",",",""," ","then"," ","some"," ","words",".",""]
               , combine "" [""]                                            --> [""]
               , combine "." ["A","B"]                                      --> ["A",".","B"]
               , combine " " ["", "A"]                                      --> [""," ","A"]
               ]

getKeywordDefsTests :: [Assertion]
getKeywordDefsTests = [ getKeywordDefs ["$rule Reproduce this precisely -- or else!!"] --> [("$rule","Reproduce this precisely -- or else!!")]
                      , getKeywordDefs ["$x Define x", "$y 55"]                        --> [("$x","Define x"),("$y","55")]
                      , getKeywordDefs ["$a A", "$b B", "$c C"]                        --> [("$a","A"),("$b","B"),("$c","C")]
                      , getKeywordDefs []                                              --> []
                      , getKeywordDefs ["$x-y-z $$$"]                                  --> [("$x-y-z","$$$")]
                      , getKeywordDefs ["$$ something to think about"]                 --> [("$$","something to think about")]
                      , getKeywordDefs ["$ meanie!"]                                   --> [("$","meanie!")]
                      , getKeywordDefs ["$var  Tristan Allwood"]                       --> [("$var", " Tristan Allwood")]
                      ]

expandTests :: [Assertion]
expandTests = [ expand "The capital of $1 is $2" "$1 Peru\n$2 Lima." --> "The capital of Peru is Lima."
              , expand "The time is $a"  "$a now."                   --> "The time is now."
              , expand "Keywords (e.g. $x, $y, $z...) may appear anwhere, e.g. <$here>."
                       "$x $a\n$y $b\n$z $c\n$here $this-is-one"
                   --> "Keywords (e.g. $a, $b, $c...) may appear anwhere, e.g. <$this-is-one>."
              ]

-------------------------------------------------------------------------------
-- HELPERS

{-|
This function ensures that its first argument is the same as the second one
-}
infix 1 -->
(-->) :: (Show a, Eq a, HasCallStack)
      => a -- ^ the actual value
      -> a -- ^ the expected value
      -> Assertion
(-->) = (@?=)

{-|
This function just matches up a bunch of assertions to a numerical naming system, allowing us to distinguish them.

If we wanted, we could provide descriptions to them instead...
-}
numberedTests :: [Assertion] -> [TestTree]
numberedTests = zipWith (\n -> testCase ("#" ++ show n)) ([1..] :: [Integer])
