compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress xs =
    if (head xs) == (head (tail xs)) then compress (drop 1 xs)
    else (head xs) : compress (tail xs)