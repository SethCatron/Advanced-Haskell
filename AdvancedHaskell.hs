{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- Seth Johnston -}
{- UIN: 228009976 -}
import Data.Char -- give isDigit

{- Data type E -}
data E = IntLit Int
       | BoolLit Bool
       | Plus E E
       | Minus E E
       | Multiplies E E
       | Exponentiate E E
       | Equals E E
         deriving (Eq, Show)

{- operator helper -}
operator op (IntLit a) (IntLit b) = IntLit $ a `op` b  -- a and b are arguments of op, where op is the operation you are wanting to perform; Haskell works like op x y or x `op' y
operator _ _ _ = error "Non-exhaustive patterns in function insideInt"

{- comparison helper  -}
equals (BoolLit x) (BoolLit y) = BoolLit (x == y)
equals (IntLit x) (IntLit y) = BoolLit (x == y)
equals _ _ = error "Non-exhaustive patterns in function insideInt"


{- eval -}
eval :: E -> E
eval x = case x of
    Plus x1 x2 -> operator (+) (eval x1) (eval x2)
    Minus x1 x2 -> operator (-) (eval x1) (eval x2)
    Multiplies x1 x2 -> operator (*) (eval x1) (eval x2)
    Exponentiate x1 x2 -> operator (^) (eval x1) (eval x2)
    Equals x1 x2 -> equals (eval x1) (eval x2)


--  takes logs to base 2 if it is possible. If it isn't possible, the program may abort (e.g., with a Non-exhaustive pattern exception).
-- log2Sim x = case (eval x) of  -- eval used here so that it can use the proper returned value for this function
--     IntLit e -> IntLit (logBase 2 e)  -- logBase is built in function to Haskell; returns the logarithm of the second argument in the base of the first one; 2 is always going to be the first parameter since the goal is to get the log base 2 of the value e (exponent)
--     e -> error $ "type error: can't take the log of a non-IntLit-valued expression: " ++ show e

{- log helper function returns an int which is comatible to turn into data type E-}
logger :: Int -> Int
logger a = if (a `mod` 2 == 0) then floor (logBase 2 (fromIntegral a)) else (error "Non-exhaustive patterns in function insideInt")

{- Log rules apply here!!! -}
log2Sim :: E -> E
log2Sim v = case v of
    (IntLit x) -> IntLit (logger x)
    (BoolLit x) -> BoolLit x
    (Exponentiate (IntLit x) (IntLit y)) -> Multiplies (IntLit x) (log2Sim (IntLit y))
    (Multiplies (IntLit x) (IntLit y)) -> Plus (log2Sim (IntLit x)) (log2Sim (IntLit y))
    (Multiplies e1 e2) -> Plus (log2Sim e1) (log2Sim e2)
    (Equals e1 e2) -> Equals (log2Sim e1) (log2Sim e2)
    _ -> v
