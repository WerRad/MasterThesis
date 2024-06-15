data IntPair = IntPair Int Int deriving Show

instance Num IntPair where
    (IntPair a b) + (IntPair c d) = IntPair (a + c) (b + d)
    (IntPair a b) - (IntPair c d) = IntPair (a - c) (b - d)
    (IntPair a b) * (IntPair c d) = IntPair (a * c) (b * d)
    negate (IntPair a b) = IntPair (negate a) (negate b)
    abs (IntPair a b) = IntPair (abs a) (abs b)
    signum (IntPair a b) = IntPair (signum a) (signum b)
    fromInteger n = IntPair (fromInteger n) (fromInteger n)

main = do
    let a = IntPair 1 2
    let b = IntPair 3 4
    putStrLn(show(a + b))
    putStrLn(show(a - b))