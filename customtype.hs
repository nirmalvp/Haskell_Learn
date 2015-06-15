data EmpInfo = Emp Int Int String deriving (Show)

--Alternate name to existing types
type Bookname = String
type Author = String
data BookReview = BookReview Author Bookname deriving (Show) 