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
 


