-- Creates a list of [x, x, x, ...]
infiniteVal :: Num a => a -> [a]
infiniteVal x = x : (infiniteVal x)

-- Infinitely appends l to itself
infiniteList :: [t] -> [t]
infiniteList l = l ++ (infiniteList l)

-- Creates an infinite list of [x, y, x, y, ...]
(+++) :: t -> t -> [t]
x +++ y = x : y : (x +++ y)

-- Infinitely appends x and y to each other
(++++) :: [t] -> [t] -> [t]
x ++++ y = x ++ y ++ (x ++++ y)

-- EXAMPLES:
--take 10 (infiniteVal 99)
--take 20 (infiniteList [1, 2, 3, 4, 5])
--take 10 (1 +++ 0)
--take 10 ([1, 2] ++++ [3, 4])
