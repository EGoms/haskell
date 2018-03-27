main = do
    mode <- promptLine "Enter Mode"
    let x = (read mode :: Int)
    if x == 2
        then do binary
        else if x == 8
            then do octal
            else if x == 16
                then do hex
                else do return ()
    
    
promptLine :: String -> IO String
promptLine prompt = do
    putStrLn prompt
    getLine
    
digits :: Int -> [Int]
digits = map (read . return) . show

--translate :: [Int] -> [Int]
--translate [] = []
--translate (x:xs) = if x == 1
--   then x: (translate xs)
--    else
--        if x == 0
--            then x: (translate xs)
--            else 0:(translate xs) --currently turns any number ne 0/1 to 0

verifyB :: [Int] -> [Int]
verifyB [] = []
verifyB (x:xs)
    | x==0 || x==1 = x: (verifyB xs)
--    | x==1 = x: (verifyB xs)
    | otherwise = (verifyB xs)

verifyO :: [Int] -> [Int]
verifyO [] = []
verifyO (x:xs)
    | x>=0 && x<8 = x: (verifyO xs)
    | otherwise = 0: (verifyO xs)

binary :: IO ()
binary = do
    let binaryList = [2^n | n <- [0..10]]
    line <- promptLine "Enter binary number"
    let number = (read line :: Int)
    --let c = translate (digits $ number)
    --print (c)
    --print (sum (zipWith (*) (reverse . digits $ number) binaryList)) --digits applied to number, reverse to that, zipwith with binary list then sum it
    --let d = sum (zipWith (*) (reverse . verify $ digits $ number) binaryList)
    print (sum (zipWith(*) (reverse (verifyB (digits (number)))) binaryList))
    --print (d)
    main
    
octal :: IO ()
octal = do
    let octalList = [8^n | n <- [0..10]]
    line <- promptLine "Enter octal number"
    let number = (read line :: Int)
    --let c =  reverse (digits number)
    print (sum (zipWith (*) (reverse . verifyO $ digits $ number) octalList)) 
    --print (d)
    main
    
hex :: IO ()
hex = do
    let hexList = [16^n | n<- [0..10]]
    line <- promptLine "Enter hexadecimal number"
    let number = (read line :: Int)
    let d = sum (zipWith (*) (reverse . digits $ number) hexList)
    print(d)
    main