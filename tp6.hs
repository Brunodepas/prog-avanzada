data Nat = Cero | Suc Nat

instance Show Nat where
    show Cero = "Cero"
    show (Suc n) = "(Suc " ++ show n ++ ")"

natToInt :: Nat -> Int
natToInt Cero = 0
natToInt (Suc n) = 1+ natToInt n

intToNat :: Int -> Nat
intToNat 0 = Cero
intToNat n = Suc (intToNat (n-1))

sumaNat :: Nat -> Nat -> Nat
sumaNat Cero Cero = Cero
sumaNat Cero n = n
sumaNat n Cero = n
sumaNat (Suc n) (Suc m) = Suc (sumaNat n (Suc m))

data Arbol a = Nil | Nodo (Arbol a) a (Arbol a)


size :: Arbol a -> Int
size Nil = 0
size (Nodo hi n hd) = 1 + size hi + size hd

height :: Arbol a -> Int
height Nil = 0
height (Nodo hi n hd) = 1 + max (height hi) (height hd)

rpreorder :: Arbol a -> [a]
rpreorder Nil = []
rpreorder (Nodo hi n hd) = n : rpreorder hi ++ rpreorder hd
--let arbol=(Nodo (Nodo Nil 2 (Nodo 3 7 Nil)) 5 Nil) 