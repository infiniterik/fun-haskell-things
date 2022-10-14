-- Sieve of Eratosthenes
-- Returns a list with all factors of the first number removed
-- Ex: [2, 3, 4, 6, 8, 9] would remove all factors of 2 returning: [3, 9]
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:l) = [y | y <- l, not ((mod y x) == 0)]

-- Helper function for nprimes
-- Applies the sieve function to the list (x:l) k times
-- Returns a list of all the factors sieved out of the list
-- Ex: nprimes' 1 [2, 3, 4, 5] returns [2]
-- Ex: nprimes' 2 [2, 3, 4, 5] returns [2, 3]
-- Ex: nprimes' 3 [2, 3, 4, 5] returns [2, 3, 5]
-- NOTE: k /= 0 is intentional; it's a surprise tool that will help us later :)
nprimes' :: (Num a, Ord a) => a -> [Integer] -> [Integer]
nprimes' _ [] = []
nprimes' k (x:l) = if k /= 0 then x : nprimes' (k - 1) (sieve (x:l)) else []

-- Makes calling the nprimes' function easier
-- Uses the infinite list [2, 3, 4, ...] by default
-- Returns the first k prime numbers in the infinite list [2, 3, 4, ...]
nprimes :: Integer -> [Integer]
nprimes k = nprimes' k [2..]

-- A list of all the prime numbers
-- Exploits k /= 0 in nprimes' because subtracting from a negative number
--   will never make the k argument in nprimes' a positive number
all_primes :: [Integer]
all_primes = nprimes (-1)
