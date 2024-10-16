module MP (separators, lookUp, splitText, combine, getKeywordDefs, expand) where

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

{-|
This function will look up a key in a list of key-value pairs,
returning all the values that match with that key.

> lookUp "A" [("A", 8), ("B", 9), ("C", 5), `("A", 7)] == [8, 7]
-}
lookUp :: String -> [(String, a)] -> [a]
lookUp key xs = [v | (k, v) <- xs, k == key]
    
{-|
This function will break up a string with some iven separator
characters, returning both the list of separators found between
each "word" and the words themselves.
-}
splitText :: [Char] -- ^ the separators to split on
          -> String -- ^ the string to split
          -> ([Char], [String])
splitText k "" = ([], [""])
splitText k (c:cs)
  | c `elem` k = (c : seps, [] : words)
  | otherwise = (seps, (c : head words) : tail words)
  where (seps, words) = splitText k cs

{-|
This function interleaves the characters from the first argument
list with the strings in the second argument. The second list must
be non-empty.
-}
combine :: [Char] -> [String] -> [String]
combine "" words = words
combine (sep:seps) (word:words) = word : [sep] : combine seps words

{-|
This function takes a list of lines and splits each line to
extract a list of keyword-definition pairs.

> getKeywordDefs ["$x Define x", "$y 55"] == [("$x", "Define x"), ("$y", "55")]
-}
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (x:xs) = (key, value) : getKeywordDefs xs
   where key = head splits
         value = concat (combine (tail seps) (tail splits))
         (seps, splits) = splitText " " x

{-|
This function takes the contents of two files, one containing
a template and the other the definitions for each keyword
found within the template. It extracts the keyword-definition
information from the info file and uses it to expand the occurrences
of these keywords in the template file, producing new file contents
as a result.

> expand "The capital $1 is $2" "$1 Peru\n$2 Lima." == "The capital of Peru is Lima"
-}
expand :: FileContents -- ^ the template file contents
       -> FileContents -- ^ the info file contents
       -> FileContents
expand template info
   | null template = template
   | null info = template
   | otherwise = concat (combine seps replacedWords)
   where
      (seps, words) = splitText separators template
      (_ , splitInfo) = splitText "\n" info
      keys = getKeywordDefs splitInfo
      replacedWords = [if not (null w) && head w == '$' then replaceWord w keys else w | w <- words]

--Helper Function to replace a word with a keyword
replaceWord :: String -> KeywordDefs -> String
replaceWord word a 
     | null check = word
     | otherwise = head check
     where check = lookUp word a
     