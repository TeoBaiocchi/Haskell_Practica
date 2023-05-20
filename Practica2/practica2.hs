{-
2. Consideremos un editor de lı́neas simple. Supongamos que una Lı́nea es una secuencia de
caracteres c1 , c2 , . . . , cn junto con una posición p, siendo 0 6 p 6 n, llamada cursor (consideraremos
al cursor a la izquierda de un caracter que será borrado o insertado, es decir como el cursor de la
mayorı́a de los editores). Se requieren las siguientes operaciones sobre lı́neas:
La descripción informal es la siguiente: (1) la constante vacı́a denota la lı́nea vacı́a, (2) la ope-
ración moverIzq mueve el cursor una posición a la izquierda (siempre que ellos sea posible), (3)
análogamente para moverDer , (4) moverIni mueve el cursor al comienzo de la lı́nea, (5) moverFin
mueve el cursor al final de la lı́nea, (6) la operación borrar elimina el caracterer que se encuentra
a la izquierda del cursor, (7) insertar agrega un caracter en el lugar donde se encontraba el cursor
y lo mueve una posición a la derecha.
Definir un tipo de datos Lı́nea e implementar las operaciones dadas.
-}

data Linea = L [Char] Int deriving Show

vacia :: Linea
vacia = (L [] 0)

moverIzq :: Linea -> Linea
moverIzq (L xs 0) = (L xs 0)
moverIzq (L xs pos) = (L xs (pos-1))
 
moverDer :: Linea -> Linea
moverDer (L xs pos) | pos < (length xs) = (L xs pos)
                    | otherwise = (L xs (pos+1)) 

moverIni :: Linea -> Linea
moverIni (L xs pos) = (L xs 0) 

moverFin :: Linea -> Linea
moverFin (L xs pos) = (L xs (length xs))

insertar :: Char -> Linea -> Linea
insertar a (L xs pos) = (L (auxInsertar a xs pos) (pos+1))

auxInsertar :: Char -> [Char] -> Int -> [Char]
auxInsertar a xs 0 = (a:xs)
auxInsertar a (x:xs) pos = (x:(auxInsertar a xs (pos-1)))

borrar :: Linea -> Linea
borrar (L xs pos) = (L (auxEliminar xs pos) (pos-1))  

auxEliminar :: [Char] -> Int -> [Char]
auxEliminar (x:xs) 1 = xs
auxEliminar (x:xs) pos = (x:(auxEliminar xs (pos-1))) 
