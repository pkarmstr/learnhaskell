import qualified Data.List as L

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mean :: (Integral a, Fractional b) => [a] -> b
mean xs = s / l where
	s = fromIntegral (sum xs)
	l = fromIntegral (length xs)

makePalindrome xs = xs ++ reverse xs

isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = x == last xs && (isPalindrome $ take (length xs - 1) xs)

sortSublists :: [[a]] -> [[a]]
sortSublists = L.sortBy (\xs ys -> compare (length xs) (length ys))

intersperse :: [a] -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse sep (x:xs) = x ++ sep ++ intersperse sep xs

data Tree a = Node a (Tree a) (Tree a)
	| Empty
	deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

main = do
	print $ myLength "foo" == length "foo"
	print $ mean [1..10]
