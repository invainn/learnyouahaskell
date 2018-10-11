import Data.List

-- "10 4 3 + 2 *"
solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words 
    where foldingFunction (x:y:xs) "*" = (x * y) : xs
          foldingFunction (x:y:xs) "+" = (x + y) : xs
          foldingFunction (x:y:xs) "-" = (y - x) : xs
          foldingFunction xs numString = (read numString) : xs