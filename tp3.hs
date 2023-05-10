merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
   | (x <= y) = [x] ++ merge xs (y:ys)
   | otherwise = [y] ++ merge (x:xs) ys

mn :: [Int] -> Int
mn [x] = x
mn (x:xs) 
   | x <= mn xs = x
   | otherwise = mn xs

selection :: [Int] -> [Int]
selection [] = []
selection [x] = [x] 
selection (x:xs)
   | x <= mn xs = [x] ++ selection (xs)
   | otherwise = selection (xs ++ [x])

pot :: Int -> Int
pot 0 = 1
pot 1 = 2
pot n = 2 * pot (n-1)

bin :: Int -> [Int]
bin 0 = [0]
bin 1 = [1]
bin n = bin (div n 2) ++ [mod n 2]

nat :: [Int] -> Int
nat [] = 0
nat (x:xs) = x * (2^(length xs)) + nat xs

binpar :: [Int] -> Bool
binpar [0] = True
binpar [1] = False
binpar x = (mod (nat x) 2) == 0

distanciaH :: (Eq a) => [a] -> [a] -> Int
distanciaH [] y = 0
distanciaH x [] = 0
distanciaH (x:xs) (y:ys)
   | [x] /= [y] = 1 + (distanciaH xs ys)
   | otherwise = (distanciaH xs ys)


cuadrado :: Int -> Int -> Bool
cuadrado 0 m = True
cuadrado 1 m = True
cuadrado n m
   | n > m^2 = cuadrado n (m+1)
   | n == m^2 = True
   | otherwise = False

znveces :: a -> Int -> [a]
znveces a 0 = []
znveces a n
  | (n > 0) = [a] ++ znveces a (n-1)
  | otherwise = znveces a (n+1)

nelem :: (Eq a) => [a] -> a -> Int
nelem [] n = 0
nelem (x:xs) n
  | (x == n) = length xs
  | otherwise = nelem xs n

posicionesC :: (Eq a) => [a] -> a -> [Int]
posicionesC [] m = []
posicionesC (x:xs) m
  | (x == m) = posicionesC xs m ++ [(length (x:xs)) +1]
  | otherwise = posicionesC xs m

compact :: [Int] -> [Int]
compact [x] = [x]
compact (x:y:xs)
  | (x == y) = compact (y:xs)
  | otherwise = [x] ++ compact (y:xs)