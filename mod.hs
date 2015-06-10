import Data.Char
import Data.List
digSum = sum.map digitToInt.show
firstTo k = find (\num -> digSum num == k) [1..]