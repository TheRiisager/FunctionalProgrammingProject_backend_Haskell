module Color where

data Color = Red |
 Yellow |
 Blue |
 Green |
 Purple |
 Orange |
 Brown deriving(Eq, Show)
 
instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b = if a == b
                then a
                else Brown

mix :: Color -> Color -> Color
mix color1 color2 = 
    color1 <> color2

--guard

{--
instance Semigroup Color where
 (<>) a b | a == b = a
          | all (`elem` [Red,Blue,Purple]) [a,b] = Purple
          | all (`elem` [Blue,Yellow,Green]) [a,b] = Green
          | all (`elem` [Red,Yellow,Orange]) [a,b] = Orange
          | otherwise = Brown
--}