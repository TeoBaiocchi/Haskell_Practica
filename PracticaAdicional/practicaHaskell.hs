{-



{-
1) Definir las siguientes funciones en forma recursiva:

a) borrarUltimo que dada una lista borra el ´ultimo elemento de la lista. 
 No utilizar reverse, ni tail
-}

borrarUltimo :: [a] -> [a]
borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = (x:borrarUltimo xs) 
-- "x ++ xs" Erroneo, ++ une listas del mismo tipo, x es solo un dato. Podria resolverse si x lo vuelvo un elemento con corchetes


{-
c) serie que se comporta de la siguiente manera: serie [1, 2, 3] = [[ ], [1], [1, 2], [1, 2, 3]] Dar su
tipo m´as general.
-}

serie :: [a] -> [[a]]
serie [] = [[]]
serie xs = serie borrarUltimo xs ++ [xs]  

{-
d) paresIguales :: Int → Int → Int → Int → Bool toma 4 n´umeros enteros y retorna True si de
dos en dos son iguales (en cualquier orden), en los dem´as casos retorna False. Por ejemplo:
paresIguales 3 1 1 2 = False paresIguales 3 1 3 1 = True paresIguales 3 3 1 1 = True
paresIguales 3 1 1 3 = True
-}


paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d | a == b = c == d  
                     | a == c = b == d 
                     | a == d = if b == c then True else False 
                     | otherwise = False
                     -- El if no es necesario, porque "c == d" ya devuelve un tipo bool
                     -- Dejo uno a caracter ilustrativo
					 
{-
e) isosceles :: Int → Int → Int → Bool que dadas la longitud de los lados de un tri´angulo nos
dice si es un tri´angulo is´osceles.
-}

isoceles :: Int -> Int -> Int -> Bool
isoceles a b c | a == b = True
			   | b == c = True
			   | a == c = True
               | otherwise = False

			   
{-
f) ror que dada una lista xs y un entero n, tal que n 6 lenght xs, rota los primeros n elementos
de xs a la derecha: ror 3 [1, 2, 3, 4, 5] = [4, 5, 1, 2, 3]. Definir una versi´on recursiva de ror ,
sin usar drop, take ni tail.
-}

obtenerUltimo :: [a] -> a
obtenerUltimo [x] = x
obtenerUltimo (x:xs) = obtenerUltimo xs

ror :: [a] -> Int -> [a]
ror xs 0 = xs
ror xs n = ror ((obtenerUltimo xs) : (borrarUltimo xs)) (n-1)

--ror xs 0 = xs
--ror (x:xs) n = ror xs (n-1) ++ [x]


{-
g) upto :: Int → Int → [Int] que dado dos n´umeros enteros n y m devuelve la lista 
[n, n + 1, n + 2, ..., m ] en caso que n menor o igual m y la lista[ ] en otro caso. No usar listas por comprensi´on
-}

upto :: Int -> Int -> [Int]
upto n m | n <= m = [n] ++ (upto n+1 m)
		 | otherwise = []
		 
{-
h) eco que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas
veces como indica su posici´on. No usar listas por comprensi´on.
Por ejemplo: eco "hola" = "hoolllaaaa"
-}		 

eco :: [Char] -> [Char]
eco [] = []
eco xs = ecoFin 1 xs    

ecoAux :: Int -> Char -> [Char]
ecoAux 0 _ = []
ecoAux x c = c:(ecoAux (x-1) c)

ecoFin :: Int -> [Char] -> [Char]
ecoFin _ [] = []
ecoFin n (x:xs) = (ecoAux n x) ++ (ecoFin (n+1) xs) 


ecoLet :: [Char] -> [Char]
ecoLet x = let ecoAux 0 _ = []
               ecoAux x c = c:(ecoAux (x-1) c)
               ecoFin _ [] = []
               ecoFin n (x:xs) = (ecoAux n x) ++ (ecoFin (n+1) xs)
            in ecoFin 1 x


-}
{-
2. Definir usando listas por comprensión las funciones:

a) cambios : [a ] → [Int]
que dada una lista, devuelve la lista de los ı́ndices en que la lista
cambia. Es decir, dada la lista s retorna la lista con los i tal que 
si 6= si+1 cambios [1, 1, 1, 3, 3, 1, 1] = [2, 4]
-}

cambios :: (Eq a) => [a] -> [Int]
cambios (x:xs) = [x | (x,(y,z)) <-(zip [0..] (zip (x:xs) xs)), z /= y]


{-
b) oblongoNumber :: [Int] que genera la lista de los números oblongos. Un número es oblongo
si es el producto de dos naturales consecutivos. Por ejemplo, los números [2, 6, 12, 20, ...]
-}

oblongoNumber :: Int -> [Int]
oblongoNumber x = [(y*(y+1)) | y <- [0..x]]

{-
c) abundantes :: [Integer] que es la lista de todos los números abundantes. Un número natural
n se denomina abundante si es menor que la suma de sus divisores propios. Por ejemplo, 12
y 30 son abundantes pero 5 y 28 no lo son. Por ejemplo abundates = [12, 18, 20, 24, 30, 36, ...
-}

abundantes :: [Int]
abundantes = [x | x <- [1..], (sum (divisoresDeUnEnteroDado x)) > x]

divisoresDeUnEnteroDado :: Int -> [Int]             
divisoresDeUnEnteroDado x = [y | y <- [1..x-1], (mod x y) == 0]

abundantesConTope :: Int -> [Int]
abundantesConTope y = [x | x <- [1..y], (sum (divisoresDeUnEnteroDado x)) > x]

{-
e) euler :: Int → Int tal que euler n es la suma de todos los múltiplos de 3 ó 5 menores que n.
Por ejemplo, euler 10 = 23. Puedes usar sin definir la función sum que suma los elementos
de una lista.
-}

euler :: Int -> Int
euler x = (sum (g x 3)) + (sum (g x 5))
        where g x y = [z| z <- [1..x-1], (mod z y) == 0]
--euler x = (sum [y| y <- [1..x], (mod y 3) == 0]) + ([z| z <- [1..x], (mod z 5) == 0])   


-- Eco, de nuevo. Dato, existe repeat...
ecoComp :: [Char] -> [Char]
ecoComp xs = [z | (x, y) <- (zip xs [1..]), (z, w) <- (zip (repeat x) [1..y])]




{-
3) Dar dos ejemplos de funciones que tengan los siguientes tipos:
-}
--a) (Int → Int) → (Bool → Bool)
ejA :: (Int -> Int) -> (Bool -> Bool)
ejA f = let funcion a = if a then ((f 1) == 0) else ((f 0) == 1)
         in funcion

ejA1 f = let usarEnCero x = if x then f 0 == 0 else False  
         in usarEnCero 

--b) Bool → (Int → Bool)
ejB :: Bool -> (Int -> Bool)
ejB b = g 
        where g x = (x < 0)

ejB2 :: Bool -> (Int -> Bool)
ejB2 bul = let g x = if x == 0 then bul else False in g   

--c) Char → Char
ejC :: Char -> Char
ejC a = a

ejC2 :: Char -> Char
ejC2 a = head (a : "Hola mundo!")

ejC3 :: Char -> Char
ejC3 'h' = 'o'
ejC3 'l' = 'a'
ejC3 'm' = 'u'
ejC3 'n' = 'd'
ejC3 'o' = '!'
ejC3 a = a

--d) Int → (Int → Bool) → [Int]
--e) [a ] → (a → [b ]) → [b ]

ejE :: [a] -> (a -> [b]) -> [b]
ejE ::  -> 
--f ) [[a ]] → (a → Bool) → [a ]
--g) (a, b, c) → Bool
--h) (a, b, c) → Int → c
--i) (a, a, a) → Int → a

{-
5. Definir las siguientes funciones usando foldr:
Nota: foldr toma una operacion y un elemento caso inicial y una lista, y de izquierda a derecha (por el "l") va aplicando en ese orden la operacion
ej: foldr (/) 2 [4, 8, 12, 10]
haria primero 10/2, y con ese resultado operaria 12/5... y asi

a) map :: (a → b) → [a ] → [b ] que dada una funci´on y una lista, aplica la funci´on a cada
elemento de la lista.
-}

--map1 :: (a -> b) -> [a] -> [b]
--map1 f [] = []
--map1 f (x:xs) = (foldr (f) x []) ++ (map1 f xs)  

map2 f xs = let funcion z [] = [(f z)]
                funcion z zs = (f z): zs
            in foldr funcion [] xs
