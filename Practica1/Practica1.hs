{-
1. Definir las siguientes funciones y determinar su tipo:
-}
-- b) apply, que toma una función y un valor, y devuelve el resultado de aplicar la funciónal valor dado
apply :: (a -> a) -> a -> a
apply f a = (f a)

-- f) f ) sign, la función signo. Nota: como signum o debia devolver char?
sign :: (Num a) => a -> a
sign a | a < 0 = (-1)
       | a > 0 = 1
       | otherwise = 0
--h) pot, que toma un entero y un número, y devuelve el resultado de elevar el segundo a la potencia dada por el primero
pot :: (Num a) => Int -> a -> a
pot x y =  y^x


{-
2. Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
a) (Int → Int) → Int
b) Int → (Int → Int)
c) (Int → Int) → (Int → Int)
d) Int → Bool
e) Bool → (Bool → Bool)
f) (Int,Char) → Bool
g) (Int,Int) → Int
h) Int → (Int,Int)
i) a → Bool
j) a → a
-}

--a)
ejA :: (Int -> Int) -> Int
ejA f = f 5

ejA2 :: (Int -> Int) -> Int
ejA2 f = 4

--b)
enteroPorDos :: Int -> (Int -> Int)
enteroPorDos x = g
			where g x = (x*2)

enteroMasDos :: Int -> (Int -> Int)
enteroMasDos x = g
			where g x = (x+2)			
			
--c)
aplicarDosVeces :: (Int -> Int) -> (Int -> Int)
aplicarDosVeces f = g 
		where g x = f (f x)

ejC :: (Int -> Int) -> (Int -> Int)
ejC f = g 
		where g x = (f x) + 5
--d)

ejD :: Int -> Bool
ejD x = x < 5

ejD2 :: Int -> Bool
ejD x = x == 0

--e)
negador :: Bool -> (Bool -> Bool)
negador x = g 
    where g x = if x then False else True

siempreTrue :: Bool -> (Bool -> Bool)
siempreTrue x = g
            where g x = True
--f)
esCero :: (Int,Char) -> Bool
esCero (x,y) = x == 0 && y == '0'

primeroPositivo :: (Int,Char) -> Bool
primeroPositivo (x, y) = x > 0

--g)
sumadorTupla :: (Int,Int) -> Int
sumadorTupla (x, y) = x + y

primerComponente :: (Int,Int) -> Int
primerComponente (x, y) = x

--h)
duplicador :: Int -> (Int,Int)
duplicador x = (x, x)

duplicador2 :: Int -> (Int,Int)
duplicador2 x = (x, x+x)

--i)
esCero :: a -> Bool
esCero 0 = True
esCero _ = False

--j) 
siempreCuarentayDos :: a -> a
siempreCuarentayDos _ = 42

{-
3. Reescribir cada una de las siguientes definiciones sin usar let, where o if:
a) f x = let (y,z) = (x,x) in y
b) greater (x,y) = if x > y then True else False
c) f (x,y) = let z = x + y in g (z,y) where g (a,b) = a − b
-}

f x = x
-- En el a), let se utiliza para definir una tupla (y, z) donde ambos valores son x
-- "in y" indica que dado lo definido en el let, esto es lo que se ejecuta. y, entonces, es solamente x

greater (x, y) | x > y = True
               | otherwise = False

greater2 (x, y) = x > y
--Esta version es mas sencilla ya que la expresion a > b ya devuelve un valor booleano 
               
--f1 (x,y) = let z = x + y in g (z,y) where g (a,b) = a − b
f1 (x, y) = x
--Esta funcion también termina devolviendo solamente x.
-- el let define z como x+y, por lo que podemos reescribir el in como g (x+y, y)
-- pero el where define g como una funcion que toma una tupla (a, b) y devuelve a-b
-- quedando entonces (x + y - y) = x 

{-
6. Sin usar funciones predefinidas, defina recursivamente las siguientes funciones y determine su
tipo más general:
-}

-- a) suma, que suma todos los elementos de una lista de números
suma [] = 0
suma x:[] = x
suma x:xs = x + (suma xs)
--d) codes, que dada una lista de caracteres, devuelve la lista de sus ordinales
-- codes (x:xs) = [] 
 
{-
8. Definir las siguientes funciones usando listas por comprensi´on:
a) divisors, que dado un entero positivo x devuelve la lista de los divisores de x (y la lista vac´ıa si el entero no es positivo).
b) matches, que dados un entero x y una lista de enteros descarta de la lista los elementos distintos a x.
c) unique, que dada una lista xs de enteros, devuelve la lista con los elementos no repetidos de xs.
d) cuadrupla, que dados cuatro enteros a, b, c y d tales que 0 < a, b, c, d, ≤ 100, devuelve las
cuadruplas (a, b, c, d) que cumplen a2 + b2 = c2 + d2 (al cuadrado)
-}
cuadruplas = [(a, b, c, d) | a <- [1.. 100], b <- [1..100], c <- [1..100], d <- [1..100], a*a + b*b == c*c + d*d]
unique xs = [x | x <- xs]

{-
9. Definir el tipo de datos Direction cuyos valores describan los puntos cardinales. Definir la
funci´on move que dado un punto en el plano (representado como un par de enteros) y una direcci´on
devuelva el punto que se obtiene el desplazarse una unidad en dicha direcci´on.
-}
data Direccion = N | S | E | O
type Punto = (Int, Int)
move :: Punto -> Direccion -> Punto
move (x, y) N = (x, y+1)
move (x, y) S = (x, y-1)
move (x, y) E = (x+1, y)
move (x, y) O = (x-1, y)
