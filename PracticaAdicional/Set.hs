{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Set(Set, emptySet, setEmpty, addSet, inSet) where
newtype Set a = Set [a] deriving Show


emptySet :: Set a
setEmpty :: Set a -> Bool
inSet :: (Eq a) => a -> Set a -> Bool
AddSet :: (Eq a) => a -> Set a -> Set a
delSet :: (Eq a) => a -> Set a -> Set a
unionSet :: (Eq a) => Set a -> Set a -> Set a

emptySet = (Set [])

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