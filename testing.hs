main = do
    putStrLn "Enter hex"
    line <- getLine
    let number = (read line :: [Char])
    let x = digits number
    print(x)
    --let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2]
    --print(triangles)
    --print(primes)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerSort = quickSort [a | a <- xs, a <= x]
        biggerSort = quickSort [a | a <- xs, a > x]
    in smallerSort ++ [x] ++ biggerSort


isPrime n = go 2
     where
         go d
             | d*d > n  = True
             | n `rem` d == 0 = False
             | otherwise = go (d+1)
            
primes = filter isPrime [2 .. 10000]

--verify :: String -> String

digits :: [Char] -> [Int]
digits = map (read . return) . show

toDigt :: Char -> Maybe Int