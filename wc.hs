import Data.Char
import Data.List
import Data.Ord (comparing)
import System.Environment   
import qualified Data.Map.Strict as Map

stopWords :: [String]
stopWords =["i", "me", "my", "myself", "we", "our", "ours",
            "ourselves", "you", "your", "yours", "yourself", "yourselves",
             "he", "him", "his", "himself", "she", "her", "hers", "herself",
             "it", "its", "itself", "they", "them", "their", "theirs",
             "themselves", "what", "which", "who", "whom", "this", "that",
             "these", "those", "am", "is", "are", "was", "were", "be", "been",
             "being", "have", "has", "had", "having", "do", "does", "did", "doing",
             "a", "an", "the", "and", "but", "if", "or", "because", "as",
             "until", "while", "of", "at", "by", "for", "with", "about",
             "against", "between", "into", "through", "during", "before", "after",
             "above", "below", "to", "from", "up", "down", "in", "out",
             "on", "off", "over", "under", "again", "further", "then",
             "once", "here", "there", "when", "where", "why", "how",
             "all", "any", "both", "each", "few", "more", "most",
             "other", "some", "such", "no", "nor", "not",
             "only", "own", "same", "so", "than", "too", "very",
             "s", "t", "can", "will", "just", "don", "should", "now"]

filterCharsAndNormalize :: String -> String
filterCharsAndNormalize [] = []
filterCharsAndNormalize (c:cs) =
  let rest = filterCharsAndNormalize cs in 
    if isAlphaNum c then (toLower c):rest else ' ':rest

scan :: String -> [String] 
scan = words

removeStopWords :: [String] -> [String]
removeStopWords words = filter (not . (flip elem) stopWords) words

wordFrequency :: [String] -> Map.Map String Int
wordFrequency xs = foldr (\word acc -> Map.insertWith (+) word 1 acc) Map.empty xs

sortFreq :: Map.Map String Int -> [(String,Int)]
sortFreq  freqs = sortBy (comparing  (negate . snd)) $ Map.toList freqs

printAll :: [(String, Int)] -> String
printAll freqs = intercalate "\n" $  map(\(s, i) -> s ++ " - " ++ (show i) )  freqs

main = do
  args <- getArgs
  txt <- readFile $ head args
  putStrLn . printAll . sortFreq  . wordFrequency . removeStopWords . scan . filterCharsAndNormalize $ txt

