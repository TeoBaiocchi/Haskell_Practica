{-
-}

{-
 borrarUltimo que dada una lista borra el ´ultimo elemento de la lista. 
 No utilizar reverse, ni tail
-}

borrarUltimo :: [a] -> [a]
borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = (x:borrarUltimo xs) 
-- x ++ xs Erroneo, ++ une listas del mismo tipo, x es solo un dato. Podria resolverse si x lo vuelvo un elemento con corchetes


{-
 serie que se comporta de la siguiente manera: serie [1, 2, 3] = [[ ], [1], [1, 2], [1, 2, 3]] Dar su
tipo m´as general.
-}

serie :: [a] -> [[a]]
serie [] = [[]]
serie xs = serie borrarUltimo xs ++ [xs]  

{-
paresIguales :: Int → Int → Int → Int → Bool toma 4 n´umeros enteros y retorna True si de
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
isosceles :: Int → Int → Int → Bool que dadas la longitud de los lados de un tri´angulo nos
dice si es un tri´angulo is´osceles.
-}

isoceles a b c | a == b = true
			   | b == c =
			   |
			   
			   {-
 ror que dada una lista xs y un entero n, tal que n 6 lenght xs, rota los primeros n elementos
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
 upto :: Int → Int → [Int] que dado dos n´umeros enteros n y m devuelve la lista 
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
ecoAux 0 _ =[]
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