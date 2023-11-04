-- Code for the Power to the People Coursework.
-- Write all your code in this file.
-- *DO NOT CHANGE THE FUNCTION NAMES OR TYPE SIGNATURES*

---------------------------------------------
-- An example implementation of the power function.
-- *DO NOT EDIT THIS FUNCTION OR USE IT IN YOUR CODE*

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- Task 1 -------------------------

stepsPower :: Integer -> Integer -> Integer
stepsPower n k
  | k == 0 = 1
  | otherwise = k + 1



-- Task 2 -------------------------

power1 :: Integer -> Integer -> Integer
power1 n k
  | k < 0 = error "k must be non-negative"
  | otherwise = product (replicate (fromInteger k) n)



-- Task 3 -------------------------

    
power2 :: Integral a => a -> a -> a
power2 n k
  | k < 0 = error "k must be non-negative"
  | k == 0 = 1
  | even k = (n * n) ^ (k `div` 2)
  | odd k = n * power2 n (k - 1)


-- Task 4 -------------------------

comparePower1 :: Integer -> Integer -> Bool
comparePower1 n k = power n k == power1 n k

comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = power n k == power2 n k



-- Task 5: comparisonList
comparisonList :: [Integer] -> [Integer] -> [(Integer, Integer, Bool, Bool)]
comparisonList ns ks =
  [(n, k, comparePower1 n k, comparePower2 n k) | n <- ns, k <- ks]
  
main :: IO ()
main = do
  putStrLn "Enter n:"
  n <- readLn
  putStrLn "Enter k:"
  k <- readLn
  let result = power1 n k
  let res = stepsPower n k
  let pow2 = power2 n k
  putStrLn $ "power1 " ++ show n ++ " " ++ show k ++ " = " ++ show result
  putStrLn $ "Steps: " ++ show res
  putStrLn $ "power2 " ++ show n ++ " " ++ show k ++ " = " ++ show pow2

  let result1 = comparePower1 n k
  let result2 = comparePower2 n k
  putStrLn $ "comparePower1 for " ++ show n ++ " " ++ show k ++ " = " ++ show result1
  putStrLn $ "comparePower2 for " ++ show n ++ " " ++ show k ++ " = " ++ show result2

  let ns = [2, 3]
  let ks = [0, 1]
  let results = comparisonList ns ks
  putStrLn "Comparison Results:"
  mapM_ (\(n, k, result1, result2) ->
    putStrLn $ "(" ++ show n ++ ", " ++ show k ++ ", " ++ show result1 ++ ", " ++ show result2 ++ ")") results
