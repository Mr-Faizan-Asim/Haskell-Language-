module Simplify where


import Expr
import Poly

--------------------------------------------------------------------------------
-- * Task 1
-- Define add, which adds 2 expressions together without introducing
-- any 'junk'.
add :: Expr -> Expr -> Expr
add (NumLit x) (NumLit y) = NumLit (x + y)
add (Op op1 e1 e2) expr2  = Op op1 (add e1 expr2) (add e2 expr2)
add expr1 (Op op2 e3 e4)  = Op op2 (add expr1 e3) (add expr1 e4)
add expr1 expr2           = Op AddOp expr1 expr2


--------------------------------------------------------------------------------
-- * Task 2
-- Define mul, which multiplies 2 expressions together without introducing
-- any 'junk'.
mul :: Expr -> Expr -> Expr
mul (NumLit x) (NumLit y) = NumLit (x * y)
mul (ExpX n) (ExpX m)     = ExpX (n + m)
mul (Op op1 e1 e2) expr2  = Op op1 (mul e1 expr2) (mul e2 expr2)
mul expr1 (Op op2 e3 e4)  = Op op2 (mul expr1 e3) (mul expr1 e4)
mul expr1 expr2           = Op MulOp expr1 expr2

--------------------------------------------------------------------------------
-- * Task 3
-- Define addAll, which adds a list of expressions together into
-- a single expression without introducing any 'junk'.
addAll :: [Expr] -> Expr
addAll = foldl add (NumLit 0)

--------------------------------------------------------------------------------
-- * Task 4
-- Define mulAll, which multiplies a list of expressions together into
-- a single expression without introducing any 'junk'.

mulAll :: [Expr] -> Expr
mulAll = foldl mul (NumLit 1)


--------------------------------------------------------------------------------
-- * Task 5
-- Define exprToPoly, which converts an expression into a polynomial.

exprToPoly :: Expr -> Poly
exprToPoly (NumLit x)    = listToPoly [x]
exprToPoly (ExpX n)      = listToPoly (replicate n 0 ++ [1])
exprToPoly (Op AddOp e1 e2) = addPoly (exprToPoly e1) (exprToPoly e2)
exprToPoly (Op MulOp e1 e2) = mulPoly (exprToPoly e1) (exprToPoly e2)

--------------------------------------------------------------------------------
-- * Task 6
-- Define polyToExpr, which converts a polynomial into an expression.

polyToExpr :: Poly -> Expr
polyToExpr (Poly [])  = NumLit 0
polyToExpr (Poly [c]) = NumLit c
polyToExpr poly       = foldr1 (Op MulOp) (map (\(c, e) -> Op AddOp (NumLit c) (ExpX e)) (zip (polyToList poly) [0..]))

--------------------------------------------------------------------------------
-- * Task 7
-- Define a function which simplifies an expression by converting it to a
-- polynomial and back again.

simplify :: Expr -> Expr
simplify = polyToExpr . exprToPoly

