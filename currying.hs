divTen :: (Floating a) => a-> a
divTen = (100/)

applyTwice :: (a->a)->a -> a
applyTwice f x = f (f x)

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (t1 -> t2 -> t) -> t2 -> t1 -> t
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a->Bool)->[a]->[a]
filter' _ [] = []
filter' predicate (x:xs)
	| predicate x == True = x:(filter' predicate xs)
    | otherwise = filter' predicate xs

takeWhile' _ [] = []
takeWhile' predicate (x:xs)
    | predicate x == True = x:(takeWhile' predicate xs)
    | otherwise = takeWhile' predicate []

collatz :: Integer -> [Integer]
collatz n
	| n == 1 = [1]
	| even n = n: collatz (div n 2)
	| otherwise = n : collatz (n*3 + 1)

longChainNums :: Int
longChainNums = length (filter isLong (map collatz [1..100]))
					where isLong xs = length xs > 15

longChainNums' :: Int
longChainNums' = length (filter (\xs -> length xs > 15) (map collatz [1..100]))
					
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' fn acc [] = acc 
foldl' fn acc (x:xs) = foldl' fn (fn acc x) xs

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  

map'' :: (a->b)->[a]->[b]
map'' fn xs= foldr (\x acc -> fn x : acc ) [] xs

maximum' :: (Ord a)=> [a]->a
maximum' = foldl1 max

reverse' = foldl (flip (:)) [] 

product' ::(Num a) => [a] -> a
product' = foldl1 (*)

filter'' :: (a->Bool)->[a]->[a]
filter'' predicate = foldr (\x acc-> if predicate x == True then x:acc else acc) [] 

last' = foldl1 (\_ x ->x)

scanl' _ acc [] = [acc]
scanl' f acc (x:xs) = acc:(scanl' f (f acc x) xs)  