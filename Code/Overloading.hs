
data Square = Square { length :: Integer } deriving Show
data Rectangle = Rectangle { width :: Integer, height :: Integer } deriving Show

getArea :: Square -> Integer
getArea (Square l) = l*l

getArea :: Rectangle -> Integer
getArea (Rectangle w h) = w*h

main = do
    let s = Square 5
    let r = Rectangle 3 4
    putStrLn(show(getArea s))
    putStrLn(show(getArea r))