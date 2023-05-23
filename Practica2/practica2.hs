{-
1. El modelo de color RGB es un modelo aditivo que tiene al rojo, verde y azul como colores
primarios. Cualquier otro color se expresa en t´erminos de las proporciones de estos tres colores que
es necesario combinar en forma aditiva para obtenerlo. Dichas proporciones caracterizan a cada
color de manera biun´ıvoca, por lo que usualmente se utilizan estos valores como representaci´on de
un color.
Definir un tipo Color en este modelo y una funci´on mezclar que permita obtener el promedio
componente a componente entre dos colores.
-}
type Rojo = Int
type Verde = Int
type Azul = Int
type Color = (Rojo, Verde, Azul)

promedioColor :: Color -> Int
promedioColor (r, g, b) = (r + g + b) `div` 3

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

{-
3. Dado el tipo de datos

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a

a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:
Las funciones de acceso son headCL, tailCL, isEmptyCL, isCUnit.

headCL y tailCL no est´an definidos para una lista vac´ıa.

headCL toma una CList y devuelve el primer elemento de la misma (el de m´as a la
izquierda).

tailCL toma una CList y devuelve la misma sin el primer elemento.

isEmptyCL aplicado a una CList devuelve True si la CList es vac´ıa (EmptyCL) o False
en caso contrario.

isCUnit aplicado a una CList devuelve True sii la CList tiene un solo elemento (CUnit a)
o False en caso contrario.

b) Definir una funci´on reverseCL que toma una CList y devuelve su inversa.

c) Definir una funci´on inits que toma una CList y devuelve una CList con todos los posibles
inicios de la CList.

d) Definir una funci´on lasts que toma una CList y devuelve una CList con todas las posibles
terminaciones de la CList.

e) Definir una funci´on concatCL que toma una CList de CList y devuelve la CList con todas ellas
concatenadas
-}

--1 2 3 4
--consnoc 1 (consnoc 2 empty 3 ) 4
--1 2 3 4 5
--consnoc 1 (consnoc 2 CUnit 3 4) 5

-- 1 2 3 4 1 2 3 4 5

-- (consnoc 1 (consnoc 2 (consnoc 3 (consnoc 4 (CUnit 1) 2) 3) 4) 5)
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a

HeadCL :: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x _ _) = x 

tailCL :: CList a -> CList a
tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x list y) = (Consnoc (headCL list) (tailCL list) y)

isEmptyCL :: CList -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit :: CList -> Bool
isCUnit :: CList -> Bool
isCUnit CUnit _ = True
isCUnit _ = False

--b)
reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = (CUnit x)
reverseCL (Consnoc x list y) = (Consnoc y (reverseCL list) x)


--e)
concatCL :: CList (Clist a) -> CList a
concatCL EmptyCL = EmptyCL 
concatCL (CUnit x) = x
concatCL (Consnoc clist_izq centro clist_der) = (Consnoc 
												(headCL (headCL clist_izq)) --asi accederia al valor en cuestion de la lista dentro de la lista 
												(Consnoc (concatCL (tailCL clist_izq)) (concatCL centro) (headCL clist_der))  
												(ConcatCL (tailCL clist_der))		
												)
{-
4. Dado el siguiente tipo algebraico:
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp
a) Defina un evaluador eval :: Aexp → Int. ¿C´omo maneja los errores de divisi´on por 0?
b) Defina un evaluador seval :: Aexp → Maybe Int.
-}

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

eval :: Aexp -> Int
eval Num x = x
eval (Prod x y) = (eval x) * (eval y)
eval (Div x y) = (eval x) `div` (eval y)
--Nota, x ´div´ y == (div x y)

seval :: Aexp -> Maybe Int
seval (Num x) = (Just x)
seval (Prod x y) = (multiplicar (seval x) (seval y)
seval (Div x y) = (dividir x y)

dividir (Just x) (Just y) = (Just (div (seval x) (seval y)))
dividir _ (Just 0) = Nothing
dividir _ _ = Nothing

multiplicar Num x = Just x
multiplicar (Just x) (Just y) =  (Just (x*y))
multiplicar _ _ = Nothing

{-7. La definicion de member dada en teor´ıa (la cual determina si un elemento esta en un bst)
realiza en el peor caso 2 * d comparaciones, donde d es la altura del arbol. Dar una definicion
de menber que realice a lo sumo d + 1 comparaciones. Para ello definir member en terminos de
una funcion auxiliar que tenga como parametro el elemento candidato, el cual puede ser igual al
elemento que se desea buscar (por ejemplo, el ´ultimo elemento para el cual la comparacion de
a menor igual b retorno True) y que chequee que los elementos son iguales solo cuando llega a una hoja del arbol
-}

member :: Eq a => a -> Bin a -> Bool
member a Hoja = False
member a (Nodo l b r ) = a == b ∨ member a l ∨ member a r

memberRedux :: Eq a => a -> Bin a -> Bool
memberRedux a Hoja = False
memberRedux a (Nodo l b r) | a < b = (member a l)
						   | a > b = (member a r)
						   | a == b = True
						  
memberRedux2 ::Eq a => a -> Bin a -> Bool
memberRedux2 _ Hoja = False
memberRedux2 a (Nodo l b r) = memberAux a (Nodo l b r)
						   
memberAux original (Nodo l nuevo r) = 						   