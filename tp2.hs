--Lista por extension = [3,6,9,12,15,18,21,24,27,30,..]
--Lista por intension = [n|n <- [1..],mod n 3 == 0]

hd :: [a] -> a
hd [x] = x
hd (x:xs) = x

lst :: [a] -> a
lst [x] = x
lst (x:xs) = lst xs

tl :: [a] -> [a]
tl [x] = []
tl (x:xs) = xs

iit :: [a] -> [a]
iit [x] = []
iit (x:xs) = [x] ++ (iit xs)

ppi :: [a] -> [a] -> [a]
ppi x y = x ++ y 

ppf :: [a] -> [a] -> [a]
ppf x y = y ++ x

ep :: [a] -> [a]
ep [x] = []
ep (x:xs) = xs

ef :: [a] -> [a]
ef [x] = []
ef (x:xs) = [x] ++ ef xs

ej8 :: Int -> [Int]
ej8 x = if x < 10 then [x] else ej8 (div x 10) ++ [mod x 10]

ej4a :: [a] -> [a] -> [a]
ej4a [] [] = []
ej4a [] ys = ys
ej4a xs [] = xs
ej4a (x:xs) ys = x : ej4a xs ys

ej4ab :: [a] -> a ->[a]
ej4ab [] x = [x]
ej4ab (x:xs) y = x : ej4ab xs y

ej4ac :: a -> [a] -> [a]
ej4ac x [] = [x]
ej4ac x y = x : y 

ej4b :: [a] -> Int -> [a]
ej4b [] n = []
ej4b xs 0 = []
ej4b (x:xs) n = x : (ej4b xs (n-1))

ej4c :: [a] -> Int -> [a]
ej4c [] n = []
ej4c xs 0 = xs
ej4c (x:xs) n = ej4c xs (n-1)

mx3 :: Integer -> Integer -> Integer -> Integer
mx3 x y z = if x > y && x > z then x else if y > x && y > z then y else z

mn3 :: Integer -> Integer -> Integer -> Integer
mn3 x y z = if x < y && x < z then x else if y < x && y < z then y else z

absoluto :: Integer -> Integer
absoluto x = if (x >= 0) then x else (-1 * x)

cd :: Int -> Int -> Int
cd n 1 = 1
cd n m
 | mod n m == 0 = 1 + cd n (m-1)
 | otherwise = cd n (m-1)

prim :: Int -> Bool
prim 1 = False
prim n = cd n n == 2

ej9 :: Int -> [Int]
ej9 n
 | n == 0 = []
 | prim n == True = n : ej9 (n-1)
 | prim n == False = ej9 (n-1)


edad :: (Integer,Integer,Integer) -> (Integer,Integer,Integer) -> Integer
edad (x,y,z) (j,k,l)
     | z > l = (z - l) * 365
     | l > z = (l - z) * 365


xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor True False = True
xor False True = True

xor2 :: Bool -> Bool -> Bool
xor2 True True = False
xor2 False False = False
xor2 p q = True

ej11 :: (Eq a) => [a] -> [a] -> Bool
ej11 [] [] = True
ej11 x [] = False
ej11 [] y = False
ej11 (x:xs) (y:ys)
    | (x == y) = True && ej11 xs ys
    | otherwise = False

rvs :: [a] -> [a]
rvs [x] = [x]
rvs (x:xs) = rvs xs ++ [x]

ej12 :: (Eq a) => [a] -> [a] -> Bool
ej12 xs ys
 | ej11 xs (rvs ys) == True = True
 | otherwise = False

ej13 :: Int -> Int -> Int -> Int
ej13 a b c
 | ((b*b) - 4 * a * c) > 0 = 2
 | ((b*b) - 4 * a * c) == 0 = 1
 | otherwise = 0

fll :: (b->a->b) -> b -> [a] -> b
fll f z [] = z
fll f z (x:xs) = fll f (f z x) xs

flr :: (a->b->b) -> b -> [a] -> b
flr f z [] = z
flr f z (x:xs) = f x (flr f z xs)