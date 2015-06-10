getInitials :: String -> String -> String
getInitials (x:xs) (y:ys) = [x] ++ "." ++ [y]

getInitials' :: String -> String -> String
getInitials' fname lname = [f] ++ "." ++ [l]
	where (f:_) = fname
	      (l:_) = lname

getSums :: (Num a) => [(a,a)] -> [a]
getSums xs = [sum' pair | pair <- xs]
	where sum' (a,b) = a+b

cylinder :: (Floating a) => a -> a ->a
cylinder r h = 
	let topArea = pi * r * r
	    sideArea = 2 * pi * r * h
	in 2 * topArea + sideArea

head' :: [a]->a
head' xs = case xs of [] -> error "Empty"
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list " ++ case xs of [] -> " is Empty"
                                            (_:[]) -> "Has one eleme"
                                            (_:_:[]) -> "Has 2 elems"
                                            xs -> "More than 2 elems"
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
	where what [] = "is Empty"
	      what (_:[]) = "has one elme"
	      what (_:_:[]) = "has 2 elems"
	      what _ = "More than 2"

maxRec :: (Ord a) => [a] -> a
maxRec (x:[]) = x
maxRec (x:xs) = max x (maxRec xs)


replicate' elem 0 = []
replicate' elem num = elem:(replicate' elem (num-1))

replicate'' elem n
    | n <= 0 = []
    | otherwise = elem:(replicate'' elem (n-1)) 

take' :: Int-> [a] -> [a]
take' n _ 
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:[]) = [x]
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a->[a]
repeat' num = num : repeat' num

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' ::(Eq a) => a-> [a] -> Bool
elem' num [] = False
elem' num (x:xs)
	| num == x = True
	| otherwise = elem' num xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
	let smaller = [elem | elem <- xs, elem < x]
	    larger = [elem| elem <-xs, elem >= x]
	in quickSort(smaller) ++ [x] ++ quickSort(larger)

