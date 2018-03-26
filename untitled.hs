main = do
    let binaryList = [2^n | n <- [0..10]]
    let octalList = [8^n | n <- [0..10]]
    let hexList = [16^n | n <- [0..10]]

    putStrLn "Enter mode"
    line <- getLine
    let x = (read line :: Int)
    
    putStrLn "Enter Number"
    line <- getLine
    let y = (read line :: Int)
    
    processIt x
    let z = reverseInt y
    let a = digits y
    let b = reverse a
    let c = binary b binaryList
    print (sum(c))
    --let x = reverseWord line
    

processIt :: Int -> (Int -> Int)
processIt s = do
    if s == 2
            
reverseInt :: Int -> Int
reverseInt x
    | x < 0     = 0 - (read . reverse . tail . show $ x)
    | otherwise = read . reverse . show $ x
                
binary :: [Int] -> [Int] -> [Int]
binary x y = do
    zipWith (*) x y
max' :: (Ord a) => a -> a -> a --a is ordinal type and its applied to the 2 input whigh give output
max' a b
    | a > b     = a
    | otherwise = b
    
digs :: (Integral x) => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]
--alt
digits :: Int -> [Int]
digits = map (read . return) . show

