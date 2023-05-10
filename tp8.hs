nand :: Bool -> Bool -> Bool
nand True True = False
nand a b = True

maj :: Bool -> Bool -> Bool -> Bool
maj True True _ = True
maj True _ True = True
maj _ True True = True
maj _ _ _ = False

paraTodo :: [Int] -> [Bool] -> (Int -> [Bool] -> Bool)-> Bool
paraTodo ns xs p = and [(xs!!n) |n <- ns,p n xs]

existe :: [Int] -> [Bool] -> (Int -> [Bool] -> Bool)-> Bool
existe ns xs p = or [(xs!!n) |n <- ns,p n xs]

sumatoria :: [Int] -> [Integer] -> Integer
sumatoria ns xs = sum [(xs!!n) | n <- ns]

productoria :: [Int] -> [Integer] -> Integer
productoria ns xs = product [(xs!!n) | n <- ns]

contatoria :: [Int] -> [Int] -> Int
contatoria ns xs = length [(xs!!n) | n <- ns]