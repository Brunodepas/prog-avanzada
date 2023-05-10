--1-
--Orden Aplicativo
--2 * cuadrado (head [2,4,5,6,7,8])		head (x:xs) = x
--2 * cuadrado (2)		cuadrado x = x * x
--2 * (2 * 2)	aritmetica
--2 * 4		aritmetica
--8
--Orden Normal
--2 * cuadrado (head [2,4,5,6,7,8])		
--2 * ( head [2,4,5,6,7,8] * head [2,4,5,6,7,8] )		cuadrado x = x * x
--2 * (2 * head [2,4,5,6,7,8])		head (x:xs) = x
--4 * head [2,4,5,6,7,8]	aritmetica
--4 * 2		head (x:xs) = x
--8		aritmetica


--2-
--Orden Aplicativo
--head linf		linf = 1
--head 1:linf		linf = 1
--head 1:1:linf		no termina
--XXXXXX
--Orden Normal
--head linf			linf = 1
--head 1:linf			head (x:xs) = x
--1


--3-
--f : : Int −> Int −> Int
--f x 0 = x
--f x ( n+1) = cuadrado ( f x n )
--Orden Aplicativo
--f 2 3
--cuadrado (f 2 2)		f x (n+1) = cuadrado (f x n)
--cuadrado ( cuadrado (f 2 1) ) 		f x (n+1) = cuadrado (f x n)
--cuadrado ( cuadrado ( cuadrado (f 2 0) ) )  	f x (n+1) = cuadrado (f x n)
--cuadrado ( cuadrado ( cuadrado (2) ) )		f x 0 = x
--cuadrado ( cuadrado ( 2 * 2 ) )		cuadrado x = x * x
--cuadrado ( cuadrado ( 4 ) )		aritmetica
--cuadrado ( 4 * 4 )		cuadrado x = x * x
--cuadrado ( 16 )		aritmetica
-- 16 * 16		cuadrado x = x * x
-- 256		aritmetica
--Orden Normal
--f 2 3
--cuadrado (f 2 2)		f x (n+1) = cuadrado (f x n)
--(f 2 2) * (f 2 2)		cuadrado x = x * x
--(cuadrado (f 2 1)) * (f 2 2) 		f x (n+1) = cuadrado (f x n)
--((f 2 1) * (f 2 1)) * (f 2 2)		cuadrado x = x * x
--((cuadrado (f 2 0)) * (f 2 1)) * (f 2 2)		f x (n+1) = cuadrado (f x n)
--(((f 2 0) * (f 2 0)) * (f 2 1)) * (f 2 2)		cuadrado x = x * x
--((2 * (f 2 0)) * (f 2 1)) * (f 2 2)			f x 0 = x
--((2 * 2) * (f 2 1)) * (f 2 2)			f x 0 = x
--(4 * (f 2 1)) * (f 2 2)			aritmetica
--(4 * (cuadrado (f 2 0))) * (f 2 2)		f x (n+1) = cuadrado (f x n)
--(4 * ((f 2 0) * (f 2 0))) * (f 2 2)		cuadrado x = x * x
--(4 * ((2) * (f 2 0))) * (f 2 2)			f x 0 = x
--(8 * (f 2 0)) * (f 2 2)		aritmetica
--(8 * 2) * (f 2 2)		f x 0 = x
--16 * (f 2 2)			aritmetica
--16 * (cuadrado (f 2 1))		f x (n+1) = cuadrado (f x n)
--16 * ((f 2 1) * (f 2 1))		cuadrado x = x * x
--16 * ((cuadrado (f 2 0)) * (f 2 1)) 		f x (n+1) = cuadrado (f x n)
--16 * ((f 2 0) * (f 2 0)) * (f 2 1)) 		cuadrado x = x * x
--16 * ((2) * (f 2 0)) * (f 2 1)) 			f x 0 = x
--32 * (f 2 0) * (f 2 1))			aritmetica
--32 * (2) * (f 2 1))			f x 0 = x
--64 * (f 2 1)			aritmetica
--64 * (cuadrado (f 2 0))		f x (n+1) = cuadrado (f x n)
--64 * ((f 2 0) * (f 2 0))		cuadrado x = x * x
--64 * ((2) * (f 2 0))			f x 0 = x
--128 * (f 2 0)			aritmetica
--128 * 2 			f x 0 = x
--256 		aritmetica


--4-
--square : : Int −> Int
--square x = x ∗ x
--inf : : Int
--inf = inf + 1
--Orden Aplicativo
--square (inf)
--square (inf + 1) 		inf = inf + 1
--square ((inf + 1) + 1)		inf = inf + 1
--XXXXXXX			No termina
--Orden Normal
--square (inf)
--inf * inf  			square x = x ∗ x
--(inf + 1) * inf 		inf = inf + 1
--((inf + 1) + 1) * inf 	inf = inf + 1
--(((inf + 1) + 1) + 1) * inf  	inf = inf + 1
--XXXXXXXXX 	No termina


--5-
--f : : Int −> Int −> Int
--f x 0 = x
--f x ( n+1) = cuadrado ( f x n )
--Orden lazy
-- f 2 3
--cuadrado ( f 2 2)		f x ( n+1) = cuadrado ( f x n )
--X * X 				cuadrado x = x * x 	[X = f 2 2]
--X * X 				f x ( n+1) = cuadrado ( f x n ) 	[X = cuadrado (f 2 1)]
--X * X 				cuadrado x = x * x 	[X = Y * Y , Y = f 2 1]
--X * X 				f x ( n+1) = cuadrado ( f x n ) 	[X = Y * Y , Y = cuadrado (f 2 0)]
--X * X 				cuadrado x = x * x 	[X = Y * Y , Y = Z * Z , Z = f 2 0]
--X * X 				cuadrado x = x * x 	[X = Y * Y , Y = Z * Z , Z = 2]		f x 0 = x
--X * X 				cuadrado x = x * x 	[X = Y * Y , Y = 2 * 2]		sustitucion
--X * X 				cuadrado x = x * x 	[X = Y * Y , Y = 4]		aritmetica
--X * X 				cuadrado x = x * x 	[X = 4 * 4]		sustitucion
--X * X 				cuadrado x = x * x 	[X = 16]		aritmetica
--16 * 16				sustitucion
--256					aritmetica