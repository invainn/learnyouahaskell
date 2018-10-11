compareWithHundred :: (Ord a, Num a) => a -> Ordering
compareWithHundred = compare 100

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] ->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x


--map' :: (a -> b) -> [a] -> [b]
--map' _ [] = []
--map' f (x:xs) = (f x) : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n = n : chain (3 * n + 1)

--numLongChains :: Int
--numLongChains = length (filter isLong(map chain [1..100]))
    --where isLong xs = length xs > 15

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

reverseList' :: [a] -> [a]
reverseList' xs = foldl (\acc x -> x : acc) [] xs 

numLongChains :: Int
numLongChains = length $ filter isLong $ map chain [1..100]
    where isLong xs = length xs > 15

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [ a | a <- xs, a <= x]
        biggerSorted = quicksort [ a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted