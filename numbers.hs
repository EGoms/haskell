import Data.Char (intToDigit, digitToInt)

main = do
    line <- promptLine "Enter radix"
    let radix = (read line :: Int)
    if radix == 16
        then do
            line <- promptLine "Enter hexadecimal number"
            let number = (read line :: [Char])
            let x = hexToDec number
            print x
            main
        else do
            line <- promptLine "Enter number"
            let number = (read line :: Int)
            let x = decToHex number
            print x
            main
    

promptLine :: String -> IO String
promptLine prompt = do
    putStrLn prompt
    getLine
    
--to decimal, takes a string representing a number in a different base, and the base it is in, and mapping
toNum :: [Char] -> Int -> (Char -> Int) -> Int
toNum [] base map = 0
toNum s base map = base * toNum (init (s)) base map + map(last(s))

--from decimal, takes number in decimal, base to conver too, and mapping function
toBase :: Int -> Int -> (Int -> Char) -> [Char]
toBase x base map
     | x < base = [map x]
     | otherwise = toBase (x `div` base) base map ++ [map (x `mod` base)]

mapHexDec :: Char -> Int
mapHexDec x
    | x == 'A' = 10
    | x == 'B' = 11
    | x == 'C' = 12
    | x == 'D' = 13
    | x == 'E' = 14
    | x == 'F' = 15
    | otherwise = digitToInt(x) :: Int

mapDecHex :: Int -> Char
mapDecHex x
    | x < 10 = intToDigit(x)
    | x == 10 = 'A'
    | x == 11 = 'B'
    | x == 12 = 'C'
    | x == 13 = 'D'
    | x == 14 = 'E'
    | x == 15 = 'F'

hexToDec :: [Char] -> Int
hexToDec [] = 0
hexToDec x = toNum x 16 mapHexDec

decToHex :: Int -> String
decToHex x = toBase x 16 mapDecHex

