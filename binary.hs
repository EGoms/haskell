main = do
    mode <- promptLine "Enter Mode"
    let x = (read mode :: Int)
    if x == 2
        then do binary
        else if x == 8
            then do octal
            else if x == 16
                then do putStrLn "Hex not yet supported"
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
    let c = reverse (digits number)
    let d = zipWith (*) c binaryList
    print (sum(d))
    main
    
octal :: IO ()
octal = do
    let octalList = [8^n | n <- [0..10]]
    line <- promptLine "Enter octal number"
    let number = (read line :: Int)
    let c =  reverse (digits number)
    let d = zipWith (*) c octalList
    print (sum(d))
    main