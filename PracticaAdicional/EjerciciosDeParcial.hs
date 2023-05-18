{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Practica0 where
import Data.List

{- 1 -------------------------------------------------------------)
Utilizando ZIP y listas por comprension. Escriba una función que realice el producto escalar de
dos listas. Donde producto escalar estaría definido como la suma de los productos uno a uno,
componente a componente de cada lista. Si una lista tuviera más elementos que la otra, al agotarse
uno de los operandos se detiene la suma.
a1*b1 + a2*b2+...+ai*bi+...+ an*bn.
-}

productoEscalar :: Num a => [a] -> [a] -> a
productoEscalar xs ys = sum [x*y | (x, y) <- zip xs ys]





{- 2) -----------------------------------------------------------
Escribir una función que inserta elementos en una lista de manera de mantenerla ordenada de
menor a mayor.
De esta forma cada operación Head sobre la lista devuelve el elemento más chico almacenado en
ella.
Inserta:: (Ord a) => a->[a]->[a]
-}

funcion :: (Ord a) => a -> [a] -> [a]
funcion a [] = [a]
funcion a [x] = if a > x then x:a:[] else a:[x] 
funcion a (x:xs) = if x > a then a:x:xs else x:(funcion a xs)




{- 3) ------------------------------------------------------
Defina un tipo de dato árbol binario de búsqueda (ArbolBin) yes
--En los bst la rama izquierda es mas chica que el nodo padre y la rama derecha mas grande
Escriba el método addTree e inOrderTree.
addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a
Inserta un elemento del tipo a en un arbol binario.
inOrderTree :: (Ord a)=> ArbolBin a -> [a]
Produce un listado “En Orden” del árbol binario.
El listado en orden del árbol se define de la siguiente manera, primero se lista en orden el árbol
izquierdo, luego la raíz y finalmente se lista en orden el árbol derecho.
-}

data ArbolBin a = Vacio | Nodo (ArbolBin a) a (ArbolBin a) deriving (Show)

addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a
addTree a Vacio = Nodo Vacio a Vacio
addTree a (Nodo izq n der)  | a == n = (Nodo izq n der) 
                            | a < n = (Nodo (addTree a izq) n der)
                            | a > n = (Nodo izq n (addTree a der))

inOrderTree :: (Ord a) => ArbolBin a -> [a]
inOrderTree Vacio = []
inOrderTree (Nodo izq n der) = inOrderTree izq ++ [n] ++ inOrderTree der




{- ------------------------------------------------------------------
 Un numero perfecto es aquel que es igual a la suma de sus divisores menores que el, como 6: 3, 2, 1
 Utilizando lista por comprension escribir la funcion perfectosn que de como resultado la lista de 
 numeros perfectos comprendidos por el intervalo [1,n]
-}

perfectosN :: (Integral n) => n -> [n] 
perfectosN n = [x | x <- [1..n], sum([y | y <- [1..x-1], x `mod` y == 0]) == x]



{-
Escribir una funcion que recibe como argumento dos listas ordenadas y devuleve una 
lista ordenada fusión de las listas argumentos 
(No se debe usar ningun metodo de clasificacion)
juntar :: (Ord a) => [a] -> [a] ->  [a]
-}

juntarTres :: (Ord a) => [a] -> [a] -> [a] 
juntarTres [] [] = []
juntarTres (x:xs) [] = [x] ++ juntarTres xs []
juntarTres [] (x:xs) = [x] ++ juntarTres xs []
juntarTres (x:xs) (y:ys) | x < y = [x] ++ juntarTres xs (y:ys)
                         | x > y = [y] ++ juntarTres (x:xs)  ys
                         | x == y = [x] ++ juntarTres xs (y:ys)

--Arriba, mi version de la funcion
--Abajo, la que estaba en el drive

juntarDrive a [] = a
juntarDrive [] b = b 
juntarDrive (x:xs) (y:ys) = if x < y then x : ((juntarDrive xs (y:ys))) else y : ((juntarDrive (x:xs) ys))


--En estas dos versiones entendi mal el enunciado
juntar xs [] = xs
juntar [] ys = ys
juntar xs ys = [x + y | (x, y) <- zip xs ys]
juntarDos [] [] = []
juntarDos [x] [] = [x]
juntarDos (x:xs) [] = [x] ++ juntarDos xs []
juntarDos (x:xs) [y]  = [x + y] ++ juntarDos xs []
juntarDos (x:xs) (y:ys) = [x + y] ++ juntarDos xs ys 
--


{-
 Escriba una funcion Qsort :: (Ord a) => [a] -> [a] sin utilizar listas por comprension
 Nota: Escriba una funcion particion que reciba como argumento un valor de referencia o pivot
 y una lista de valores del mismo tipo que el pivot
 Esta funcion da como resultado una tupla con dos listas (l1, l2) de modo que en l1 estan todos los valores
 que son menores o iguales que el pivot y en l2 todos los mayores que el pivot
 particion :: Ord a => a -> [a] -> ([a], [a])      

 (Qsort: toma un elemento de la lista y en funcion de ese elemento (pivot) separa el resto en dos listas
 determinadas por elementos mas grandes o mas chicos que el pivot y aplica el mismo procedimiento a las restantes
 cuando queda una de un solo elemento o vacia, se considera que ya esta "ordenada" y empieza a ordenar)
-}

particion :: Ord a => a -> [a] -> ([a], [a])
particion x [] = ([], [])
particion x (y:ys) | x <= y = (fst(particion x ys), snd(particion x ys) ++ [y]) 
                   | x > y =  (fst(particion x ys) ++ [y], snd(particion x ys)) 

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) =  qsort(fst(particion x xs)) ++ [x] ++ qsort(snd(particion x xs))


qsortConWhere :: (Ord a) => [a] -> [a]
qsortConWhere [] = []
qsortConWhere [x] = [x]
qsortConWhere (x:xs) = qsortConWhere(primer) ++ [x] ++ qsortConWhere(segundo)
                       where 
                       (primer, segundo) = (particion x xs)



particionAlMedio :: Ord a => [a] -> ([a], [a])
particionAlMedio [] = ([], [])
particionAlMedio [x] = ([x], [])
particionAlMedio (x:y:xs) = (x:primer, y:segundo) 
                where (primer, segundo) = (particionAlMedio xs)

fusionarListas :: Ord a => [a] -> [a] -> [a]  
fusionarListas [] [] = []
fusionarListas xs [] = xs
fusionarListas [] ys = ys
fusionarListas (x:xs) (y:ys) | x < y = [x] ++ fusionarListas xs (y:ys)
                             | x >= y = [y] ++ fusionarListas (x:xs) ys
                             
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = fusionarListas ys zs  
               where 
                (primer, segundo) = particionAlMedio xs
                ys = msort primer
                zs = msort segundo



findMin :: (Ord a) => [a] -> a
findMin [] = error "La lista no puede estar vacia"
findMin [x] = x
findMin (x:xs) = if x > findMin xs 
                 then findMin xs
                 else x 

removeOne :: (Ord a) => [a] -> a -> [a]
removeOne [] v = error "Elemento no encontrado"
removeOne (x:xs) v = if x == v then xs 
                     else x:removeOne xs v

--selectionSort :: (Ord a) => [a] -> [a]
--selectionSort [] = []
--selectionSort [x] = [x]
--selectionSort xs = ((findMin xs):selectionSort(removeOne(xs findMin xs)))

selectionSort2 :: (Ord a) => [a] -> [a]
selectionSort2 [] = []
selectionSort2 [x] = [x]
selectionSort2 xs = minimo:selectionSort2(removeOne xs minimo)
                   where minimo = findMin xs





{-
Recordemos que la funcion de biblioteca ZIP recibe como argumento dos listas x:xs e y:ys
y produce una lista de tuplas (i,j) donde los i provienen de la primera lista y los j de 
la segunda.
Cuando una lista es mas larga que la otra, el resultado contempla solo los pares hasta donde pudieron formarse
a) Escriba una version personal de la funcion zip llamada miZip
miZip :: [a] -> [b] -> [(a,b)]
b) Utilizando miZip y listas por comprension escriba una funcion que realice el producto
escalar de dos listas...
c) Utilizando solo la funcion miZip escriba la funcion "indexado"
dada una lista produce una lista de pares donde cada elemento de la lista tiene
su posicion dentro de la misma comenzando en 1
-}

miZip :: [a] -> [b] -> [(a,b)]
miZip _ [] = []
miZip [] _ = [] 
miZip (x:xs) (y:ys) = (x, y) : (miZip xs ys)

--escalar :: (Num a|) => [a] -> [a] -> a
--escalar xs ys = [n*m]  
 

--Un ej de la practica
unique :: (Num a, Eq a) => [a] -> [a]
unique xs = [y| (x, y) <- zip [0..] xs, not(elem y (take x xs))]


-- not(x `elem` xs)
{-
    Consideremos la siguiente funcion 
    split :: (Ord a) => a -> [a] -> ([a], [a])
    split x l = ([y | y <- l, y <= x], [y | y <- l, y > x])
    defina una version de esta funcion que trabaje en exactamente una sola pasada
    a la lista l
-}

particionWhere :: Ord a => a -> [a] -> ([a], [a])
particionWhere x [] = ([], [])
particionWhere x (y:ys) | x <= y = (primer, segundo ++ [y]) 
                        | x > y =  (primer ++ [y], segundo) 
                        where (primer, segundo) = particionWhere x ys



{-
Una cola de prioridad es una estructura de datos que almacena elementos "Clasificables"
Con la particularidad de que cuando se saca uno de ella siempre se extrae el elemento con menor clave
de ahi su nombre pues clasifica los elementos en funcion de su prioridad mas baja primero
Las funciones que manipulan a la cola de prioridad son:
mkqpr: instancia una nueva cola de prioridad vacia
addqpr: agrega un nuevo elemento
nextqpr: devuelve el elemento con clave mas baja
popqpr: devuelve una cola de prioridad donde se quito el nextqpr
defina el tad ColaPrioridad e implemente el mismo utilizando un arbol binario de busqueda
como estructura de almacenamiento
Escribir todas las funciones necesarias para la manipulacion de la estructura subyacente es decir para
manipular el arbol
sugerencia: recordar como extraer el elemento con clave mas pequeña de un arbol
-}


--newtype ColaPrioridad a = ColaPrioridad [a] deriving (Ord, Show)

data ArbolBin2 a = Vacio | Nodo (ArbolBin2 a) a (ArbolBin2 a) deriving Show

mkNewTree :: (Ord a) => ArbolBin2 a
mkNewTree = Vacio

addTree :: (Ord a) => a -> ArbolBin2 a -> ArbolBin2 a
addTree a Vacio = Nodo Vacio a Vacio
addTree a (Nodo izq n der) | a == n = (Nodo izq a der) --si es igual no se repite
                           | a > n = (Nodo izq a addTree(der))
                           | a < n = (Nodo addTree(izq) a der)

minTree :: (Ord a) => ArbolBin2 a -> a
minTree a Vacio = Vacio
minTree a (Nodo Vacio raiz _) = raiz 
minTree a (Nodo izq raiz der) = minTree izq

 newtype CP a = CP (ArbolBin2 a) deriving Show






{-
 Un conjunto o Set es una coleccion de items del mismo tipo distinguibles entre
 si por su clave o valor en el cual un item puede ser testeado si es 
 miembro, insertado o borrado de la coleccion 
 La cantidad de elementos distintos es lo que se denomina tamaño del conjunto
 Module set....
 emptySet :: Set a
 setEmpty :: Set a -> Bool
 inSet :: (Eq a) => a -> Set a -> Bool
 AddSet :: (Eq a) => a -> Set a -> Set a
 delSet :: (Eq a) => a -> Set a -> Set a
 unionSet :: (Eq a) => Set a -> Set a -> Set a
 
 Defina el tipo de dato e implemente los metodos del nuevo tipo de dato
 utilizando listas no ordenadas y sin duplicados.
 El metodo unionSet (union de los dos conjuntos) se escribira haciendo uso 
 de los metodos ya definidos, es decir, sin operar directamente la lista sino
 que directamente se operara el SET


module Set(Set, emptySet, setEmpty, addSet, inSet) where
newtype Set a = Set [a] deriving Show


 emptySet :: Set a
 setEmpty :: Set a -> Bool
 inSet :: (Eq a) => a -> Set a -> Bool
 AddSet :: (Eq a) => a -> Set a -> Set a
 delSet :: (Eq a) => a -> Set a -> Set a
 unionSet :: (Eq a) => Set a -> Set a -> Set a

emptySet = Set []

setEmpty (Set []) = True
setEmpty _ = False

inSet _ (Set []) = False
inSet a (Set (x:xs)) | a == x = True
                     | a /= x = inSet(a Set xs)

addSet a (Set xs) | inSet(a (Set xs)) = (Set xs)
                  | not(inSet(a (Set xs))) = (Set (xs ++ [a]))     

{-
delSet _ (Set []) = (Set []) 
delSet a (Set (x:xs)) = if x /= a 
                        then Set((Set [x]) ++ delSet(a (Set xs)))
                        else Set(delSet(a Set(xs)))
-}

delSet _ (Set []) = (Set []) 
delSet a (Set (x:xs)) = if x /= a 
                        then addSet(x delSet(a (Set xs)))
                        else (Set xs)   --delSet(a Set(xs))

unionSet (Set xs) (Set []) = (Set xs)
unionSet (Set xs) (Set y:ys) = unionSet (addSet y xs) ys
                             -}