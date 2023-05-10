unoinf :: Int -> [Int]
unoinf n = [1] ++ unoinf n

natinf :: Int -> [Int]
natinf n 
    | n >= 0 = [n] ++ natinf (n+1)
    | otherwise = error "Ingrese un num mayor o igual a 0"

primnat :: Int  -> [Int]
primnat n = take n (natinf 0)

tk5 :: Int -> [Int]
tk5 n
   | n >= 1 = take 5 (natinf n)
   | otherwise = error "Ingrese un num mayor o igual a 1"

cuadlista :: [Int] -> [Int]
cuadlista x = map (*2) x

sds :: Int -> Int -> Bool
sds x 0 = False
sds x y = mod x y == 0

divs :: Int -> [Int]
divs x = filter (sds x) [-x..x]

cd :: Int -> Int -> Int
cd n 1 = 1
cd n m
 | mod n m == 0 = 1 + cd n (m-1)
 | otherwise = cd n (m-1)

prim :: Int -> Bool
prim 1 = False
prim n = cd n n == 2

listaNatp :: [Int] -> [Int]
listaNatp xs = filter (prim) xs

listaNatsumcuad :: [Int] -> Int
listaNatsumcuad [] = 0
listaNatsumcuad xs = foldr (+) 0 (map (*2) xs)

listaNatsucc :: [Int] -> [Int]
listaNatsucc [] = []
listaNatsucc xs = map (+1) xs

listaIntsuma :: [Int] -> Int
listaIntsuma [] = 0
listaIntsuma xs = foldr (+) 0 xs

factorial :: Int -> Int
factorial n = foldr (*) 1 [1..n]

--fun :: Int -> Int
--fun x = 2*x

--dos :: (Int -> Int) -> Int -> Int
--dos f x = f (f x)

and2 :: [Bool] -> Bool
and2 [] = error "lista vacia"
and2 [x] = x
and2 (x:xs) 
   | x == True = x && (and2 xs)
   | otherwise = False

tam :: [Int] -> Int
tam [] = 0
tam xs = foldr (+) 0 xs

compsussint :: [Int] -> [Int]
compsussint xs = [(x+1) | x <- xs]

compcuadnat :: [Int] -> [Int]
compcuadnat xs = [(x*x) | x <- xs]

compparmay10int :: [Int] -> [Int]
compparmay10int xs = [x | x <- xs , mod x 2 == 0 , x > 10]

divsint :: Int -> [Int]
divsint n = [x | x <- [1..n] , mod n x == 0]

list :: (Eq a) => a -> [a] -> Bool
list x [] = False
list x (y:ys) 
   | x == y = True
   | otherwise = list x ys

todosOcurrenEn :: (Eq a) => [a] -> [a] -> [Bool]
todosOcurrenEn xs ys = [list x ys |x <- xs]

primentreny2 :: Int -> [Int]
primentreny2 n = [x | x <- [2..n],prim x]

prodcart :: [Int] -> [Int] -> [Int]
prodcart xs ys = [x * y | x <- xs,y <- ys]

--prodcart :: [Int] -> [Int] -> Int
--prodcart xs ys = foldr (+) 0 (prodcartsinsum xs ys)

ocurr :: (Eq a) => [a] -> a -> Int
ocurr ys x = length [n | n <- ys, x==n]



split2 :: [a] -> [([a],[a])]
split2 xs = [(take i xs,drop i xs)|i <- [0..length xs]]

segint :: [Int] -> Int
segint [] = 0
segint xs = foldr (+) 0 [(take i xs)| i <- [0..length xs]]


--[x | x <- [0..], mod x 2 == 0]