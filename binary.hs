import Data.Char (ord, chr, digitToInt, intToDigit, toLower)
import Text.Read
import Data.List
import Data.Maybe
import Data.Functor
import Control.Applicative
import Numeric (readHex)

main = do
    line <- promptLine "Enter radix"
    let radix = (read line :: Int)
    let list = [radix^n | n <- [0..10]]
    if radix == 16
        then do
            line <- promptHex "Enter hex"
            let number = (read line :: [Char])
            let hex = parseHex number
            print (hex)
            main
        else do
            line <- promptLine "Enter number"
            let number = (read line :: Int)
            print (sum (zipWith (*) (reverse (verify (digits (number)) radix)) list))
            main
    
promptLine :: String -> IO String
promptLine prompt = do
    putStrLn prompt
    getLine
    

promptHex :: String -> IO String
promptHex prompt = do
    putStrLn prompt
    getLine
    
hexChar ch = fromMaybe (error $ "illegal char" ++ [ch]) $ 
    elemIndex ch "0123456789ABCDEF"
parseHex :: String -> Int
parseHex hex = foldl f 0 hex where
    f n c = 16*n + hexChar c

verify :: [Int] -> Int -> [Int]
verify [] n = []
verify (x:xs) n
    | x>=0 && x<n = x: (verify xs n)
    | otherwise = 0: (verify xs n)

digits :: Int -> [Int]
digits = map (read . return) . show
--digits x = map (map read . words) $ digitToInt x
--translate :: [Int] -> [Int]
--translate [] = []
--translate (x:xs) = if x == 1
--   then x: (translate xs)
--    else
--        if x == 0
--            then x: (translate xs)
--            else 0:(translate xs) --currently turns any number ne 0/1 to 0

--verifyB :: [Int] -> [Int]
--verifyB [] = []
--verifyB (x:xs)
--    | x==0 || x==1 = x: (verifyB xs)
--    | x==1 = x: (verifyB xs)
--    | otherwise = (verifyB xs)
--binary :: IO ()
--binary = do
--    line <- promptLine "Enter radix"
--    let radix = (read line :: Int)
--    line <- promptLine "Enter binary number"
--    let number = (read line :: Int)
--    let list = [radix^n | n <- [0..10]]
    --let c = translate (digits $ number)
    --print (c)
    --print (sum (zipWith (*) (reverse . digits $ number) binaryList)) --digits applied to number, reverse to that, zipwith with binary list then sum it
    --let d = sum (zipWith (*) (reverse . verify $ digits $ number) binaryList)
--    print (sum (zipWith(*) (reverse (verify (digits (number)) radix)) list))
    --print (d)
--    main
    
--octal :: IO ()
--octal = do
--    let octalList = [8^n | n <- [0..10]]
--    line <- promptLine "Enter octal number"
--    let number = (read line :: Int)
    --let c =  reverse (digits number)
--    print (sum (zipWith (*) (reverse (verify (digits (number)) 8)) octalList)) 
    --print (d)
--    main
    
--hex :: IO ()
--hex = do
--    let hexList = [16^n | n<- [0..10]]
 --   line <- promptLine "Enter hexadecimal number"
--    let number = (read line :: Int)
--    let d = sum (zipWith (*) (reverse . digits $ number) hexList)
--    print(d)
--    main