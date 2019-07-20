import Data.List
import Data.Bool
import Data.Complex (Complex((:+)), magnitude)

pythag :: (Floating a) => a -> a -> a
pythag x y = sqrt (x**2 + y**2)

rights = [(a,b,c) | c <- [1..100], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]


test x = "This number is " ++ show x

length' :: (Num p) => [a] -> p
length' [] = 0
length' (_:xs) = 1 + length' xs

divideBy :: (Floating a) => a -> a -> a
divideBy x = (/x) -- divideBy 5 10 -> 10/5

divides x y = x / y

weirdNumber = sum (takeWhile(<10000) (filter odd (map (^2) [1..])))

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n:collatz (n `div` 2)
    | odd n = n:collatz (n*3 + 1)

--numIter :: (Integral a) => a -> (a, [a], a)
--numIter n = (n, (collatz n), length (collatz n))

numIter :: Int -> (String, [Int])
numIter n = ("Number " ++ show n ++ " takes " ++ show (length (collatz n)) ++ " iterations", collatz n) 

primeSieve :: (Eq p, Num p, Enum p) => p -> [p]
primeSieve z = sieve [2..z]
    where
        sieve (x:xs) = x : sieve (xs \\ [x,x+x..z])
        sieve [] = []

ugcd :: (Integral a) => a -> a -> a
ugcd a 0 = a
ugcd a b = ugcd b (a `mod` b)

egcd :: Integer -> Integer -> (Integer, Integer)
egcd a 0 = (1, 0)
egcd a b = (t, s-q*t)
    where 
        (q, r) = a `quotRem` b
        (s, t) = egcd b r

lcm :: (Integral a) => a -> a -> a
lcm _ 0 = 0
lcm 0 _ = 0
lcm x y = abs ((x `quot` (gcd x y )) * y)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)        


mandelbrot :: RealFloat a => Complex a -> Complex a
mandelbrot a = iterate ((a+) . (^ 2)) 0 !! 50

run :: IO ()
run = 
    mapM_
        putStrLn
        [ [ bool ' ' '*' (2 > magnitude (mandelbrot (x :+ y)))
          | x <- [-2,-1.9685 .. 0.5] ]
        | y <- [1, 0.95 .. -1] ]
