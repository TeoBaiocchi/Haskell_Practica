{-data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp
-- data Maybe a = Nothing | Just a

eval :: Aexp -> Int
eval (Num a) = a
eval (Prod x y) = (eval x) * (eval y)
eval (Div x y) = (eval x) `div` (eval y)

por :: Maybe Int -> Maybe Int -> Maybe Int
(Just x) `por` (Just y) =  Just (x*y)
Nothing `por` _ = Nothing
_ `por` Nothing = Nothing

dividir :: Maybe Int -> Maybe Int -> Maybe Int
_ `dividir` Nothing = Nothing
Nothing `dividir` _ = Nothing
_ `dividir` (Just 0) = Nothing
(Just x) `dividir` (Just y) = Just (x `div` y)

seval :: Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod x y) = (seval x) `por` (seval y)
seval (Div x y) = (seval x) `dividir` (seval y)

-}

data Tree a = Leaf  | Node  (Tree a) a (Tree a) deriving (Show)

completo :: a -> Int -> Tree a
completo _ 0 = Leaf 
completo x d = let  l = completo x (d-1) 
                    in Node l x l 

balanceado :: a -> Int -> Tree a
balanceado _ 0 = Leaf
balanceado x d 
      | even (d-1) = let l =  balanceado x ((d-1) `div` 2)
                            in Node l x l
      | otherwise = let m = (div (d-2) 2)
                        balanceado2 x m = (balanceado x m, balanceado x (m+1))
                        (l,r) = balanceado2 x m
                        in Node l x r

{-
data BST a = Leaf  | Nodo (BST a) a (BST a) deriving (Show)

maximum :: BST a -> a
maximum (Nodo _ x Leaf) = x
maximum (Nodo _ _ x ) = maximum x

minimum :: BST a -> a
minimum (Nodo Leaf x _) = x
minimum (Nodo x _ _) = minimum x

checkBST :: (Ord a) => BST a -> Bool
checkBST Leaf = True
checkBST (Nodo Leaf _ Leaf) = True
checkBST (Nodo Leaf x y) = checkBST y && x <= minimum y
checkBST (Nodo x y Leaf) = checkBST x && y >= maximum x
checkBST (Nodo x y z) = checkBST x && y >= maximum x && y <= minimum z && checkBST z
-}

data BST a = Hoja  | Nodo (BST a) a (BST a) deriving (Show)

member :: Ord a => a -> BST a -> Bool
member _ Hoja = False
member a (Nodo l b r) | a == b = True
                      | a < b = member a l
                      | otherwise = member a r

