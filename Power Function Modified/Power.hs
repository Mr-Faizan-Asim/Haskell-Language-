-- used to export whole functions
module Power where

-- already given 
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)


-- Task 1 -------------------------

-- calculate power without recursive
stepsPower :: Integer -> Integer -> Integer
stepsPower n k
  | k == 0 = 1
  | otherwise = k + 1



-- Task 2 -------------------------

-- calculate power by list replication
power1 :: Integer -> Integer -> Integer
power1 n k
  -- check is k is greater then zero
  | k < 0 = error "k must be non-negative"
  | otherwise = product (replicate (fromInteger k) n)



-- Task 3 -------------------------

-- calculate using recursive function
power2 :: Integral a => a -> a -> a
power2 n k
  | k < 0 = error "k must be non-negative"
  | k == 0 = 1
  | even k = (n * n) ^ (k `div` 2)
  | odd k = n * power2 n (k - 1)



-- Task 4 -------------------------

-- compare with power power and task 2
comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = power n k == power1 n k

-- compare with power power and task 3
comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = power n k == power2 n k


-- Task 5 -------------------------
comparisonList :: [Integer] -> [Integer] -> [(Integer, Integer, Bool, Bool)]
-- using whole task 4 and all tasks
comparisonList ns ks =
  [(n, k, comparePower1 n k, comparePower2 n k) | n <- ns, k <- ks]