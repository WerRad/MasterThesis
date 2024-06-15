{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

data Point = Point { x :: Float, y :: Float } deriving Show

data Vector = Vector { dx :: Float, dy :: Float } deriving Show

data Circle = Circle { circ_center :: Point, radius :: Float } deriving Show
data Line = Line { start :: Point, end :: Point } deriving Show
data Triangle = Triangle { a :: Point, b :: Point, c :: Point } deriving Show
data Rectangle = Rectangle { topLeft :: Point, bottomRight :: Point } deriving Show
data Square = Square { sq_topLeft :: Point, sq_bottomRight :: Point } deriving Show
data Ellipse = Ellipse { ellip_center :: Point, radiusX :: Float, radiusY :: Float } deriving Show

class Shape a where
    getCenter :: a -> Point
    move :: Vector -> a -> a
    getArea :: a -> Float

instance Shape Point where
    getCenter = id
    move v (Point x y) = Point (x + dx v) (y + dy v)
    getArea _ = 0

instance Shape Circle where
    getCenter = circ_center
    move v (Circle p r) = Circle (Point (x p + dx v) (y p + dy v)) r
    getArea (Circle _ r) = pi * r * r

instance Shape Line where
    getCenter (Line p1 p2) = Point ((x p1 + x p2) / 2) ((y p1 + y p2) / 2)
    move v (Line p1 p2) = Line (Point (x p1 + dx v) (y p1 + dy v)) (Point (x p2 + dx v) (y p2 + dy v))
    getArea _ = 0

instance Shape Triangle where
    getCenter (Triangle p1 p2 p3) = Point ((x p1 + x p2 + x p3) / 3) ((y p1 + y p2 + y p3) / 3)
    move v (Triangle p1 p2 p3) = Triangle (Point (x p1 + dx v) (y p1 + dy v)) (Point (x p2 + dx v) (y p2 + dy v)) (Point (x p3 + dx v) (y p3 + dy v))
    getArea (Triangle p1 p2 p3) = abs ((x p1 * (y p2 - y p3) + x p2 * (y p3 - y p1) + x p3 * (y p1 - y p2)) / 2)

instance Shape Rectangle where
    getCenter (Rectangle tl br) = Point ((x tl + x br) / 2) ((y tl + y br) / 2)
    move v (Rectangle tl br) = Rectangle (Point (x tl + dx v) (y tl + dy v)) (Point (x br + dx v) (y br + dy v))
    getArea (Rectangle tl br) = abs ((x tl - x br) * (y tl - y br))

instance Shape Square where
    getCenter (Square tl br) = Point ((x tl + x br) / 2) ((y tl + y br) / 2)
    move v (Square tl br) = Square (Point (x tl + dx v) (y tl + dy v)) (Point (x br + dx v) (y br + dy v))
    getArea (Square tl br) = abs ((x tl - x br) * (y tl - y br))

instance Shape Ellipse where
    getCenter = ellip_center
    move v (Ellipse p rx ry) = Ellipse (Point (x p + dx v) (y p + dy v)) rx ry
    getArea (Ellipse _ rx ry) = pi * rx * ry


class Shape a => EllipseLike a where
    centerToString :: a -> String
    centerToString a = "Ellipse center: " ++ show (getCenter a)
    getRadiusX :: a -> Float
    getRadiusY :: a -> Float

class Shape a => RectangleLike a where
    getWidth :: a -> Float
    getHeight :: a -> Float

instance EllipseLike Ellipse where
    getRadiusX = radiusX
    getRadiusY = radiusY

instance EllipseLike Circle where
    getRadiusX = radius
    getRadiusY = radius
    centerToString (Circle p _) = "Circle center: " ++ show p

instance EllipseLike Point where
    getRadiusX _ = 0
    getRadiusY _ = 0

instance RectangleLike Rectangle where
    getWidth (Rectangle tl br) = abs (x tl - x br)
    getHeight (Rectangle tl br) = abs (y tl - y br)

instance RectangleLike Square where
    getWidth (Square tl br) = abs (x tl - x br)
    getHeight (Square tl br) = abs (y tl - y br)

instance RectangleLike Point where
    getWidth _ = 0
    getHeight _ = 0

class (RectangleLike a, EllipseLike a) => PointLike a where
    pointToString :: a -> String
    pointToString a = 
        "Point: " ++ show (getCenter a) ++ "\n" ++
        "RadiusX: " ++ show (getRadiusX a) ++ "\n" ++
        "RadiusY: " ++ show (getRadiusY a) ++ "\n" ++
        "Width: " ++ show (getWidth a) ++ "\n" ++
        "Height: " ++ show (getHeight a)

instance PointLike Point

printRectangle :: (Shape a, RectangleLike a) => a -> IO ()
printRectangle a = do
    putStrLn $ "Width: " ++ show (getWidth a)
    putStrLn $ "Height: " ++ show (getHeight a)
    putStrLn $ "Area: " ++ show (getArea a)

main :: IO ()

main = do
    let p1 = Point 1 2
    let p2 = Point 3 4
    let p3 = Point 5 2
    let p4 = Point 7 8
    let p5 = Point 9 10
    let c = Circle p1 3
    let l = Line p1  p2
    let t = Triangle p1 p2 p3
    let r = Rectangle p1 p2
    let s = Square p1 p2
    let e = Ellipse p1 3 4
    putStrLn $ pointToString p1
    putStrLn $ show $ getArea t
    printRectangle r
    putStrLn $ centerToString e
    putStrLn $ centerToString c

