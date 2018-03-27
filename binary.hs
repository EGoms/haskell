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

binary :: IO ()
binary = do
    let binaryList = [2^n | n <- [0..10]]
    line <- promptLine "Enter binary number"
    let number = (read line :: Int)
    --let c = reverse (digits number)
    let d = sum (zipWith (*) (reverse . digits $ number) binaryList) --digits applied to number, reverse to that, zipwith with binary list then sum it
    print (d)
    main
    
octal :: IO ()
octal = do
    let octalList = [8^n | n <- [0..10]]
    line <- promptLine "Enter octal number"
    let number = (read line :: Int)
    --let c =  reverse (digits number)
    print (sum (zipWith (*) (reverse . digits $ number) octalList)) 
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