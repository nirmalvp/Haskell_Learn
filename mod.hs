import Data.Char
import Data.List

numOfWords :: String -> [(String,Int)]
numOfWords = map (\wordList -> (head wordList, length wordList)).group.sort.words

digSum :: Int -> Int 
digSum = sum.map digitToInt.show

firstTo :: Int->Maybe Int
firstTo k = find (\num -> digSum num == k) [1..]

isIn :: Eq a => [a] -> [a] -> Bool
isIn subList = any(isPrefixOf subList).tails 

encode :: Int->String -> String
encode offset = map (chr.(offset+).ord) 

decode :: Int->String -> String
decode offset = encode (negate offset)

getInDict :: Eq a => a->[(a,b)]->Maybe b
getInDict _ [] = Nothing
getInDict key ((k,v):xs) 
 | key == k = Just v
 | otherwise = getInDict key xs

getInDict' :: Eq a => a->[(a,b)]->Maybe b
getInDict' key = foldl (\acc (k,v) -> if k == key then Just v else acc) Nothing