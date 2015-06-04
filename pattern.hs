fact :: (Integral a) => a->a
fact 0 = 1
fact n = n * fact(n-1)
addVectNorm :: (Num a)  => (a, a) -> (a, a) -> (a, a)
addVectNorm a b = (fst a + fst b , snd a + snd b)
addVectPattern :: (Num a ) => (a,a) -> (a,a) -> (a,a)
addVectPattern (a,b) (c,d) = (a+c , b+d)
first :: (Num a) => (a,a,a) -> a
second ::(Num a) => (a,a,a) -> a
third :: (Num a) => (a,a,a) -> a

first (x,_,_) = x
second (_,x,_) = x
third (_,_,x) = x
 
head' :: [a]->a
head' (x:_) = x

tail' :: [a]->[a]
tail' (_:x) = x

listLength ::(Show a) => [a]->String
listLength [] = "Zero Elements"
listLength (x:[]) = "One element" ++ show x
listLength (x:y:[]) = "2 elements" ++ show x ++ "And " ++ show y
listLength xs = "So many elements"

length' :: (Num b) => [a]-> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a]->a
sum' [] = 0
sum' (x:xs) = x + sum' xs

firstLetter :: String -> String 
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  
