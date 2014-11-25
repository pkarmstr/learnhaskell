sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


penultimate :: [a] -> a
--penultimate [] = []
--penultimate (x:[]) = []
penultimate (x:y:[]) = x
penultimate xs = penultimate (tail xs)

main = do
	print $ (penultimate [1..10])
	print $ (penultimate [1])
