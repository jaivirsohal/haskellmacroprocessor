import Test.Tasty (defaultMain, TestTree, testGroup)

import Test.Tasty.QuickCheck

import MP

main :: IO ()
main = defaultMain props

props :: TestTree
props = testGroup "macro" [ testProperty "combine with concat is the inverse of splitText" combineConcatInverse]

{-
From the spec, splitting up a string with some separators then combining them and concatenating
should do nothing. Note that this property could be more general and try any arbitrary separators
too, but that'll likely make for some confusing counter-examples, so this is specialised to the
separators we are given.
-}
newtype SeparatedString = SeparatedString String
combineConcatInverse :: SeparatedString -> Bool
combineConcatInverse (SeparatedString str) =
  concat (uncurry combine (splitText separators str)) == str

separator :: Gen Char
separator = elements separators

nonSeparator :: Gen Char
nonSeparator = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-'])

instance Arbitrary SeparatedString where
  arbitrary = fmap SeparatedString (listOf (frequency [(1, separator), (3, nonSeparator)]))
  shrink (SeparatedString msg) = map SeparatedString (shrinkList (const []) msg)
instance Show SeparatedString where
  show (SeparatedString str) = show str
