module Vectors where

eye :: Vector3
eye = Vector3 0 0 0

sphere :: Sphere3
sphere = Sphere3 (Vector3 0 10 0) 2

data Vector3 = Vector3 
    { x :: Float
    , y :: Float
    , z :: Float
    } deriving (Show)

data Line3 = Line3
    { origin :: Vector3
    , unit :: Vector3
    } deriving (Show)

data Sphere3 = Sphere3
    { center :: Vector3
    , radius :: Float 
    } deriving (Show)

addV3 :: Vector3 -> Vector3 -> Vector3
addV3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = 
    Vector3 (x1 + x2) (y1 + y2) (z1 + z2) 

subV3 :: Vector3 -> Vector3 -> Vector3
subV3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = 
    Vector3 (x1 - x2) (y1 - y2) (z1 - z2) 

mulV3 :: Vector3 -> Float -> Vector3
mulV3 (Vector3 x1 y1 z1) f = 
    Vector3 (x1 * f) (y1 *f) (z1 * f)

dotV3 :: Vector3 -> Vector3 -> Float
dotV3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = 
    x1*x2 + y1*y2 + z1*z2

lenV3 :: Vector3 -> Float
lenV3 v = 
    sqrt ( dotV3 v v)

lineFrom :: Vector3 -> Vector3 -> Line3
lineFrom o t = 
    Line3 o u where
        v = subV3 t o
        u = mulV3 v (1.0/lenV3 v)

intersectDistance :: Sphere3 -> Line3 -> Maybe Float
intersectDistance (Sphere3 c r) (Line3 o u) = 
    if f < 0 then Nothing 
    else Just ((-b - sqrt f)/2.0)
    where
        oc = subV3 o c
        b = 2*dotV3 u oc
        f = b**2 - 4*(dotV3 oc oc - r**2)

linePoint :: Line3 -> Float -> Vector3
linePoint (Line3 o u) distance = 
    addV3 o (mulV3 u distance)

reflectV3 :: Sphere3 -> Line3 -> Maybe Line3
reflectV3 sphere line = 
    fmap reflected distance where
        distance = intersectDistance sphere line
        reflected :: Float -> Line3
        reflected d =  Line3 t v where
            t = linePoint line d
            u = unit line
            n = mulV3 (subV3 t (center sphere)) (1.0/(radius sphere))
            v = addV3 u (mulV3 n (-2*(dotV3 u n)))

reflectedLine :: Sphere3 -> Line3 -> Line3
reflectedLine sphere line = 
    case (reflectV3 sphere line) of 
        Just reflection -> reflection
        Nothing -> line