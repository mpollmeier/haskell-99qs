import qualified Data.Map as Map

myLast :: [a] -> a
myLast [] = error "empty list does not have a last element!"
myLast [x] = x
myLast (x : xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "empty list not supported"
myButLast [x] = error "length one list not supported"
myButLast [x, _] = x
myButLast (x : xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list not supported"
elementAt (x : xs) 1 = x
elementAt (x : xs) i = elementAt xs (i-1)

myLength :: [a] -> Int
myLength as = foldl (\acc _ -> acc + 1) 0 as
-- myLength [] = 0
-- myLength [_] = 1
-- myLength (_ : xs) = 1 + (myLength xs)

myReverse :: [a] -> [a]
-- myReverse [] = []
-- myReverse (x : xs) = (myReverse xs) ++ [x]
myReverse as = foldl (\acc a -> a : acc) [] as

allButLast :: [a] -> [a]
allButLast [] = []
allButLast [_] = []
allButLast (x : xs) = x : (allButLast xs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x : xs) = (x == myLast xs) && (isPalindrome (allButLast xs))

compress :: Eq a => [a] -> [a]
-- compress [] = []
-- compress [x] = [x]
-- compress (x1 : (x2 : xs))
--   | x1 == x2  = compress (x2 : xs)
--   | otherwise = x1 : (compress (x2 : xs))
--
-- compress as = foldr appendMaybe [] as
--   where
--     appendMaybe n [] = [n]
--     appendMaybe n (x : xs) = if n == x then (x : xs) else (n : (x : xs))

compress as = foldr appendMaybe [head as] as
  where appendMaybe n acc = if n == head acc then acc else n : acc

pack :: Ord a => [a] -> [[a]]
pack as = Map.elems m
  where m = foldr appendToMap Map.empty as
        appendToMap n acc = Map.insertWith (++) n [n] acc

main :: IO ()
-- main = putStrLn $ show $ myLength "aaaabccaadeeee"
-- main = putStrLn $ show $ myReverse "abcde"
-- main = putStrLn $ show $ allButLast [1,2,3]
-- main = putStrLn $ show $ compress "aaaabccaadeeee"
main = putStrLn $ show $ pack "aaaabccaadeeee"
