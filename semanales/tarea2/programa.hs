import Data.List

isPermutation :: (Eq a) => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs ys 
    | (length xs /= length ys) = False
    | elem (head xs) ys = isPermutation (tail xs) (delete (head xs) ys)
    | otherwise = False