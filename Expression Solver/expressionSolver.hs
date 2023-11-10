-- Simple Calculator in Haskell

-- Define a data type for the calculator's expressions
data Expr = Val Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

-- Evaluate an expression
eval :: Expr -> Double
eval (Val x)   = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x / eval y

-- Parse an expression from a string
parse :: String -> Maybe Expr
parse s = case reads s of
    [(x, "")] -> Just (Val x)
    _         -> case words s of
        [a, "+", b] -> liftA2 Add (parse a) (parse b)
        [a, "-", b] -> liftA2 Sub (parse a) (parse b)
        [a, "*", b] -> liftA2 Mul (parse a) (parse b)
        [a, "/", b] -> liftA2 Div (parse a) (parse b)
        _           -> Nothing

-- Main function
main :: IO ()
main = do
    putStrLn "Enter an expression (e.g., 2 + 3 * 4):"
    input <- getLine
    case parse input of
        Just expr -> putStrLn ("Result: " ++ show (eval expr))
        Nothing   -> putStrLn "Invalid expression"

-- liftA2 function to apply a binary function to two parsed expressions
liftA2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftA2 f (Just x) (Just y) = Just (f x y)
liftA2 _ _        _        = Nothing
