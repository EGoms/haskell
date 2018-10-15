import Data.Char (intToDigit, digitToInt)

main = do
    line <- promptLine "Enter radix"
    let radix = (read line :: Int)
    radixHandler radix

radixHandler :: Int -> IO String
radixHandler number
    | number < 10 = sub10 "Enter a number in base <" number
    | number == 16 = radix16 "Enter a hexadecimal number in quotes"
    | number == 10 = radix10 "Enter a number"
    | otherwise = unhandled

unhandled :: IO String
unhandled = do
    putStrLn "Unhandled radix"
    main
    
radix10 :: String -> IO String
radix10 str = do
    line <- promptLine str
    let number = (read line :: Int)
    line <- promptLine "Enter a new base"
    let base = (read line :: Int)
    print $ decToAny number base
    main

radix16 :: String -> IO String
radix16 str = do
    line <- promptLine str
    let number = (read line :: [Char])
    print $ hexToDec number
    main

sub10 :: String -> Int -> IO String
sub10 str num = do
    line <- promptLine (str ++ show num)
    let number = (read line :: Int)
    let radixList = [num^n | n <- [0..10]]
    print (sum (zipWith (*) (reverse (verify (digits (number)) num)) radixList))
    main
    

promptLine :: String -> IO String
promptLine prompt = do
    putStrLn prompt
    getLine
    
--to decimal, takes a character list, base to convert to, mapping function, returns an int
toNum :: [Char] -> Int -> (Char -> Int) -> Int
toNum [] base map = 0
toNum s base map = base * toNum (init (s)) base map + map(last(s))

--from decimal, takes number in decimal, base to convert to, and mapping function
--if x > base split it into digits map the function to each one and combine recursively
toBase :: Int -> Int -> (Int -> Char) -> [Char]
toBase x base map
     | x < base = [map x]
     | otherwise = toBase (x `div` base) base map ++ [map (x `mod` base)]

--function to convert hexadecimal to decimal
mapHexDec :: Char -> Int
mapHexDec x
    | x == 'A' = 10
    | x == 'B' = 11
    | x == 'C' = 12
    | x == 'D' = 13
    | x == 'E' = 14
    | x == 'F' = 15
    | otherwise = digitToInt(x) :: Int

--function to convert decimal to hexadecimal
mapDecHex :: Int -> Char
mapDecHex x
    | x < 10 = intToDigit(x)
    | x == 10 = 'A'
    | x == 11 = 'B'
    | x == 12 = 'C'
    | x == 13 = 'D'
    | x == 14 = 'E'
    | x == 15 = 'F'

--takes [char] so uses toNum, base 16 and hexadecimal mapping
hexToDec :: [Char] -> Int
hexToDec [] = 0
hexToDec x = toNum x 16 mapHexDec

--takes and int and maps to hexadecimal using toBase, 16 and mapping function
decToHex :: Int -> String
decToHex x = toBase x 16 mapDecHex

--verify the digits are correct given the base, takes list of digits to check and base
--if its valid 0 <= x < base confirm, otherwise turn it into a 0
verify :: [Int] -> Int -> [Int]
verify [] n = []
verify (x:xs) n
    | x>=0 && x<n = x: (verify xs n)
    | otherwise = 0: (verify xs n)

--takes an int and splits into digits
digits :: Int -> [Int]
digits = map (read . return) . show

--use lambda to provide mapping function use intToDigit
decToAny :: Int -> Int -> String
decToAny x n = toBase x n (\x -> intToDigit(x))