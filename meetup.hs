import qualified Data.Map as Map

myLast :: [a] -> a
myLast [] = error "empty!"
myLast [x] = x
myLast (x:xs) = myLast xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = foldl ()
-- myReverse (x:xs) = myReverse xs ++ [x]

main :: IO ()
-- main = putStrLn $ show $ myLast "aaaabccaadeeee"
-- main = putStrLn $ show $ myLength "aaaabccaadeeee"
-- main = putStrLn $ show $ myReverse "abcde"
-- main = putStrLn $ show $ allButLast [1,2,3]
-- main = putStrLn $ show $ compress "aaaabccaadeeee"
-- main = putStrLn $ show $ pack "aaaabccaadeeee"
