import Data.Char (intToDigit, digitToInt)

main = do
    line <- promptLine "Enter radix"
    let radix = (read line :: Int)
    let radixList = [radix^n | n <- [0..10]]
    if radix == 10
        then do
            line <- promptLine "Enter a number"
            let number = (read line :: Int)
            line <- promptLine "Enter new base"
            let base = (read line :: Int)
            print $ decToAny number base
            main
        else do
            if radix == 16
                then do
                    line <- promptLine "Enter a hexadecimal number in quotes"
                    let number = (read line :: [Char])
                    print $ hexToDec number
                    main
                else do
                    if radix < 10
                        then do
                            line <- promptLine "Enter a number in base <10"
                            let number = (read line :: Int)
                            print (sum (zipWith (*) (reverse (verify (digits (number)) radix)) radixList))
                            main
                        else do
                            putStrLn "unhandled radix"
                            return ()

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

verify :: [Int] -> Int -> [Int]
verify [] n = []
verify (x:xs) n
    | x>=0 && x<n = x: (verify xs n)
    | otherwise = 0: (verify xs n)

digits :: Int -> [Int]
digits = map (read . return) . show

decToAny :: Int -> Int -> String
decToAny x n = toBase x n (\x -> intToDigit(x))


