{-
1. Dado el siguiente tipo de dato que representa los n´umeros naturales:
data Nat = Cero | Succ Nat

a) ¿Qu´e tipo tiene Succ?
b) Definir la funci´on int2Nat :: Int → Nat que dado un entero retorne su representaci´n en Nat
Ejemplo int2Nat 4 = Succ (Succ (Succ (Succ Cero)))
c) Definir la funci´on suma :: Nat → Nat → Nat
NO convertir los Nat a enteros para poder sumarlos.
d) Definir la funci´on nat2Int :: Nat → Int que dado un Nat retorne a que entero representa.
-}

data Nat = Cero | Succ Nat deriving Show
--a) tipo Succ :: Nat -> Nat

int2Nat :: Int -> Nat 
int2Nat 0 = Cero
int2Nat x = Succ (int2Nat (x-1)) 

suma :: Nat -> Nat -> Nat
suma Cero x = x
suma x Cero = x
suma (Succ x) (Succ y) = Succ (suma x y)

nat2Int :: Nat -> Int
nat2Int Cero = 0
nat2Int (Succ x) = 1 + (nat2int x)


{- 
----------------------------------------
2. Dado el tipo de datos de ´arboles binarios:
data Arb = E | H Int | N Arb Arb
y el tipo de datos de comandos, para navegar el ´arbol:
data Cmd = L | R
a) ¿Qu´e tipo tiene N?
b) Definir la funci´on parcial selec::[Cmd] → Arb → Arb, que selecciona el sub´arbol correspondiente.
Por ej:          
				/\
select [L,R] ( /  \ ) = Hoja 4
   			  /\   5
			 /  \
		  	3   4
			
			(N (N (H 3) (H 4)) (H 5))
			
La funci´on selec es parcial, ya que no est´a definida para listas de comandos err´oneas (como por
ejemplo, ir a la izquierda cuando nos encontramos en una hoja).
-- Nota: El ejercicio da un ejemplo pesimo, porque si bien ejemplifica con la devolucion de una Hoja
-- lo que hace es devolver el SUBARBOL dado por esas direcciones. Es decir, si el 4 siguiese ramificando tendria que mostrarlo.

c) Definir una funci´on enum :: Arb → [[Cmd]] que devuelva todas las secuencias de comandos
v´alidas para ir desde la ra´ız hasta una hoja.
-}

data Arb = E | H1 Int | N1 Arb Arb deriving Show
data Cmd = L | R
-- a) N :: Arb -> Arb -> Arb

selec :: [Cmd] -> Arb -> Arb
selec [] x = x
selec (L:xs) (N i d) = selec xs i
selec (R:xs) (N i d) = selec xs d 

enum :: Arb -> [[Cmd]]
enum (H1 x) = [[]]
enum E = [[]]
enum (N1 i d) = (agregarInicio (enum i) L) ++ (agregarInicio (enum d) R) 

agregarInicio :: [[a]] -> a -> [[a]]
agregarInicio [] a = []
agregarInicio (x:xs) a = (a:x):(agregarInicio xs a)


{-
------------------------------------------------
3. Un lenguaje imperativo simple solo permites variables de un unico tipo, 
para esto se mantiene un estado con el nombre de las variables y sus valores. 
Un Estado es una estructura secuencial formada por un nombre de variable y el valor correspondiente. 
Se requiere las siguientes operaciones sobre Estado :

inicial : Estado a
update : Nombre → A → Estado a → Estado a
lookfor : Nombre → Estado a → Maybe a
free : Nombre → Estado a → Estado a

donde

- inicial representa el estado inicial de un programa donde no sean definido ninguna variable
- update permite actualizar el valor de una variable existente y si la variable no existe la agrega
al estado con el valor dado.
- lookfor dado el nombre de una variable permite obtener el valor de esta si es que existe en
el estado.
- free dado el nombre de una variable la elimina del estado.

Definir los tipo de dato para Nombre y Estado e implementar las operaciones dadas.
->
-}

data Nombre = N [Char] deriving (Eq, Show)
data Estado a = H | V Nombre a Estado deriving Show

--data Maybe a = Nothing | Just a deriving Show

inicial :: Estado a
inicial = H

update :: Nombre -> a -> Estado a -> Estado a
update nom1 x H = (V nom1 x H)
update nom1 x (V nom2 y e) = if nom1 == nom2 then (V nom2 y e) else (V nom2 y (update nom1 x e))

lookfor :: Nombre -> Estado a -> Maybe a
lookfor nom1 H = Nothing
lookfor nom1 (V nom2 y e) = if nom1 == nom2 then (Just y) else (lookfor (N nom1) e)

free :: Nombre -> Estado a -> Estado a
free _ H = H
free nom1 (V nom2 y e) = if nom1 == nom2 then e else (V nom2 y (free nom1 e))
-- Se asume que el elemento aparece una unica vez




{-
-------------------------------------------
4. Implementar una funcion que:

a) calcule el numero de nodos en un nivel especıfico de un arbol binario

b) reciba un arbol binario de b´usqueda y verifique si es un arbol balanceado, es decir, que la
diferencia de alturas entre su sub´arbol izquierdo y derecho no sea mayor que 1 para todos
los nodos

c) encuentren el sucesor y el predecesor de un valor dado en un arbol binario de busqueda. El
sucesor es el valor mas pequeño mayor que el valor dado, y el predecesor es el valor m´as
grande menor que el valor dado

d) dado un Leftist Heaps, retorne una lista con sus elementos ordenados de mayor a menor

e) verifique si un arbol cumple con la propiedad de Leftist Heap

f) elimine todos los elementos duplicados en un Leftist Heap y devuelva el nuevo heap resultante

g) verifique si un arbol cumple con la propiedad de Red − Black − Tree
-}

-- BST
data Bin a = Hoja | Nodo (Bin a) a (Bin a)

delete z (Nodo Hoja b Hoja) | z == b = Hoja
delete z (Nodo Hoja b r) | z == b = r
delete z (Nodo l b Hoja) | z == b = l
delete z (Nodo l b r)
    | z > b = Nodo (delete z l) b r 
    | z < b = Nodo l b  (delete z r)
    | z == b = let y = getMin r in Nodo l y (delete y r) 

getMin (Nodo Hoja x _) = x
getMin (Nodo l _ _) = getMin l


-- a)
calcular :: Bin -> Int -> Int
calcular (N H) _ = 0
calcular (N i c H) 0 = 1
calcular (N H c d) 0 = 1
calcular (N i c d) x = calcular i (x-1) + calcular d (x-1) 
-- Nota, lo dejo para claridad, pero los dos casos que devuelven 1
-- pueden simplificarse con _ 0 = 1 ya que no importa donde tengan la hoja.


--b)
getHeight (Nodo Hoja _ Hoja) = 0
getHeight (Nodo l _ Hoja) = getHeight l + 1
getHeight (Nodo Hoja _ r) = getHeight r + 1
getHeight (Nodo l _ r) = if lHeight <= rHeight then rHeight + 1 else lHeight + 1
							where 
							lHeight = getHeight l
							rHeight = getHeight r
  
balanced Hoja = True
balanced (Nodo l c r) = abs(getHeight l - getHeight r) <= 1 && (balanced l) && (balanced r)
-- -------------------------------


--RBT
data Color = R | B
data RBT a = E | T Color (RBT a) a (RBT a)

memberRBT :: Ord a ⇒ a → RBT a → Bool
memberRBT a E = False
memberRBT a (T l b r) | a ≡ b = True
				      | a < b = memberRBT a l
                      | a > b = memberRBT a r

insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
			 where ins x E = T R E x E
			
ins x (T c l y r) | x < y = balance c (ins x l) y r
				  | x > y = balance c l y (ins x r)
                  | otherwise = T c l y r
makeBlack E = E
makeBlack (T l x r) = T B l x r

balance :: Color → RBT a → a → RBT a → RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

{-
-}



