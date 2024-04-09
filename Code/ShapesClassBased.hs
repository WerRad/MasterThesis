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
data Polygon = Polygon { vertices :: [Point] } deriving Show

class Shape a where
    getCenter :: a -> Point

class EllipseLike a where
    getECenter :: a -> Point
    getRadiusX :: a -> Float
    getRadiusY :: a -> Float
    moveE :: Vector -> a -> a

class RectangleLike a where
    getRCenter :: a -> Point
    getWidth :: a -> Float
    getHeight :: a -> Float

instance EllipseLike Ellipse where
    getECenter = ellip_center
    getRadiusX = radiusX
    getRadiusY = radiusY
    moveE v (Ellipse p rx ry) = Ellipse (Point (x p + dx v) (y p + dy v)) rx ry

instance EllipseLike Circle where
    getECenter = circ_center
    getRadiusX = radius
    getRadiusY = radius
    moveE v (Circle p r) = Circle (Point (x p + dx v) (y p + dy v)) r

instance EllipseLike Point where
    getECenter = id
    getRadiusX _ = 0
    getRadiusY _ = 0
    moveE v p = Point (x p + dx v) (y p + dy v)

instance RectangleLike Rectangle where
    getRCenter (Rectangle tl br) = Point ((x tl + x br) / 2) ((y tl + y br) / 2)
    getWidth (Rectangle tl br) = abs (x tl - x br)
    getHeight (Rectangle tl br) = abs (y tl - y br)

instance RectangleLike Square where
    getRCenter (Square tl br) = Point ((x tl + x br) / 2) ((y tl + y br) / 2)
    getWidth (Square tl br) = abs (x tl - x br)
    getHeight (Square tl br) = abs (y tl - y br)

instance RectangleLike Point where
    getRCenter = id
    getWidth _ = 0
    getHeight _ = 0


instance (EllipseLike a, RectangleLike a) => Shape a where
    getCenter a = getECenter a


main :: IO ()

main = do
    let p1 = Point 1 2
    let p2 = Point 3 4
    let p3 = Point 5 6
    let p4 = Point 7 8
    let p5 = Point 9 10
    let c = Circle p1 3
    let l = Line p1  p2
    let t = Triangle p1 p2 p3
    let r = Rectangle p1 p2
    let s = Square p1 p2
    let e = Ellipse p1 3 4
    let p = Polygon [p1, p2, p3, p4, p5]
    putStrLn $ show $ getCenter p1

