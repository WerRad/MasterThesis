

data Vector = Vec Float Float

class Movable a where
    move :: Vector -> a -> a
    reflectX :: a -> a
    reflectY :: a -> a
    rotate180 :: a -> a
    rotate180 = reflectX . reflectY

data Point = Point Float Float
    deriving Show

instance Movable Point where
    move (Vec v1 v2) (Point c1 c2) = Point (c1+v1) (c2+v2)
    reflectX (Point c1 c2) = Point c1 (-c2)
    reflectY (Point c1 c2) = Point (-c1) c2
    rotate180 (Point c1 c2) = Point (-c1) (-c2)

data Shape = Circle Point Float | Line Point Point deriving Show

instance Movable Shape where
    move v (Circle p r) = Circle (move v p) r
    move v (Line p1 p2) = Line (move v p1) (move v p2)
    reflectX (Circle p r) = Circle (reflectX p) r
    reflectX (Line p1 p2) = Line (reflectX p1) (reflectX p2)
    reflectY (Circle p r) = Circle (reflectY p) r
    reflectY (Line p1 p2) = Line (reflectY p1) (reflectY p2)
    rotate180 (Circle p r) = Circle (rotate180 p) r
    rotate180 (Line p1 p2) = Line (rotate180 p1) (rotate180 p2)

class Named a where
    lookName :: a -> String
    giveName :: String -> a -> a

data Name a = Pair a String deriving Show

instance Named (Name a) where
    lookName (Pair obj nm) = nm
    giveName nm (Pair obj _) = (Pair obj nm)

mapName :: (a -> b) -> Name a -> Name b
mapName f (Pair obj nm) = Pair (f obj) nm

instance Movable a => Movable (Name a) where
    move v = mapName (move v)
    reflectX = mapName reflectX 
    reflectY = mapName reflectY

class (Movable b, Named b) => NamedMovable b

instance Movable a => NamedMovable (Name a)

instance Movable b => Movable (b,c) where
    move v (x,y) = (move v x, y)
    reflectX (x,y) = (reflectX x, y)
    reflectY (x,y) = (reflectY x, y)

instance Named c => Named (b,c) where
    lookName (_,nm) = lookName nm
    giveName nm (x, y) = (x,giveName nm y)

instance (Movable b, Named c) => NamedMovable (b,c)

main :: IO ()
main = do
    let c = Circle (Point 1 2) 3
    let l = Line (Point 1 2) (Point 3 4)
    putStrLn $ show c
    putStrLn $ show l
    putStrLn $ show $ move (Vec 1 1) c
    putStrLn $ show $ move (Vec 1 1) l
    putStrLn $ show $ reflectX c
    putStrLn $ show $ reflectX l
    putStrLn $ show $ reflectY c
    putStrLn $ show $ reflectY l
    putStrLn $ show $ rotate180 c
    putStrLn $ show $ rotate180 l
    putStrLn $ show $ rotate180 $ rotate180 c
    putStrLn $ show $ rotate180 $ rotate180 l
    let c2 = giveName "Circle" $ Pair c "Circle"
    let l2 = giveName "Line" $ Pair l "Line"
    putStrLn $ show $ lookName c2
    putStrLn $ show $ lookName l2
    putStrLn $ show $ move (Vec 1 1) c2
    putStrLn $ show $ move (Vec 1 1) l2


