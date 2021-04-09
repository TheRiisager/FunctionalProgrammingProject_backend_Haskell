-- | An example module.
module Example (main) where

-- | An example function.
main :: IO ()
main = do
    print(add 22 5)
    print(greet "Noobs")
    print(multi 5 22)

greet :: String -> String
greet string = "Hello " ++ string

add :: Int -> Int -> Int
add x y =  x + y

multi :: Int -> Int -> Int
multi x y = x * y