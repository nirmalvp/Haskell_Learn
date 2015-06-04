bmiTell :: (RealFloat a) => a -> a->  String

bmiTell weight height
	| weight / height ^ 2 <= 18.5 = "UnderWeight"
	| weight / height ^ 2 <= 25.0 = "Normal"
	| weight / height ^ 2 <= 30.0 = "Fat"
	| otherwise = "Whale"  

bmiTellGuards :: (RealFloat a) => a -> a -> String
bmiTellGuards weight height
	| bmi <= 18.5 ="UnderWeight"
	| bmi <= 25.0 = "Normal"
	| bmi <= 30.0 = "Fat"
	| otherwise = "Whale"
	where bmi = weight / height ^ 2

bmiTellGuardsFn :: (RealFloat a) => [(a,a)]-> [a]
bmiTellGuardsFn wh = [bmi w h | (w,h) <- wh]
	where bmi weight height = weight / height ^ 2

max' :: (Ord a) => a-> a -> a
max' a b 
	| a<b = b
	| otherwise = a

initials :: String -> String -> String
initials firstname lastname = [f] ++ " . " ++ [l]
	where (f:_) = firstname
	      (l:_) = lastname 





