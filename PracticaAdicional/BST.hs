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
