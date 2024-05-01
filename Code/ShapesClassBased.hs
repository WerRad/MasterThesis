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

instance Shape Point where
    getCenter = id

instance Shape Circle where
    getCenter = circ_center

instance Shape Line where
    getCenter (Line p1 p2) = Point ((x p1 + x p2) / 2) ((y p1 + y p2) / 2)

instance Shape Triangle where
    getCenter (Triangle p1 p2 p3) = Point ((x p1 + x p2 + x p3) / 3) ((y p1 + y p2 + y p3) / 3)

instance Shape Rectangle where
    getCenter (Rectangle tl br) = Point ((x tl + x br) / 2) ((y tl + y br) / 2)

instance Shape Square where
    getCenter (Square tl br) = Point ((x tl + x br) / 2) ((y tl + y br) / 2)

instance Shape Ellipse where
    getCenter = ellip_center

instance Shape Polygon where
    getCenter (Polygon ps) = Point (sum (map x ps) / fromIntegral (length ps)) (sum (map y ps) / fromIntegral (length ps))

class Shape a => EllipseLike a where
    getECenter :: a -> Point
    getECenter a = getCenter a
    getRadiusX :: a -> Float
    getRadiusY :: a -> Float
    moveE :: Vector -> a -> a


class RectangleLike a where
    getWidth :: a -> Float
    getHeight :: a -> Float

instance EllipseLike Ellipse where
    getRadiusX = radiusX
    getRadiusY = radiusY
    moveE v (Ellipse p rx ry) = Ellipse (Point (x p + dx v) (y p + dy v)) rx ry

instance EllipseLike Circle where
    getRadiusX = radius
    getRadiusY = radius
    moveE v (Circle p r) = Circle (Point (x p + dx v) (y p + dy v)) r

instance EllipseLike Point where
    getRadiusX _ = 0
    getRadiusY _ = 0
    moveE v p = Point (x p + dx v) (y p + dy v)

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
    moveP :: Vector -> a -> a
    moveP v a = moveE v a

instance PointLike Point

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
    putStrLn $ show $ getCenter $ (moveP (Vector 1 2) p1)

