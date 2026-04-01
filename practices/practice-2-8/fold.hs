f1 xs = foldr (*) 1 xs
--map
f2 f xs = foldr (\x ys-> f x : ys) [] xs
--otra manera con where
f22 f xs = foldr aplicar [] xs
  where
    aplicar x ys = f x : ys
--filter
f3 f xs = foldr (\x ys -> if f x then x:ys else ys) [] xs
--length
f4 xs = foldr (\x y -> 1 + y) 0 xs
--(++)
f5 xs ys = foldr (:) ys xs
--reverse
f6 xs = foldr (\x ys -> ys ++ [x]) [] xs 
--otra manera  con foldl
rev xs  = foldl (\ys x -> x:ys) []
--concat
f7  xss = foldr (\xs ys-> xs ++ ys) [] xss
--takeWhile  
f8 f xs = foldr (\x ys -> if f x then x:ys else []) [] xs
f9 xs = foldl (\y x -> y*10 + x) 0 xs
--el min de una lista
f10 xs = foldr (\x y-> if x < y then x else y) (may xs) xs 
    where 
        may xs = foldr (\x y -> if x > y then x else y) 0 xs
--otra forma
f101 xs = foldr (\x y-> if x < y then x else y) (head xs) xs 
--otra forma xs toma el ultimo valor *ojo examen*
minimo :: (Ord a) => [a] -> a
minimo = foldr1 (\x xs -> if x < xs then x else xs)
f11 xs = let -- ojo *exmane  interesante* 
    m xs = minimo xs 
    in foldr (\x ys -> m xs: ys) [] xs
--verificar  si  esta  ordenado
f12 xs = snd(foldr f (last xs, True) xs)  
    where 
        f sig (ant, cond) = if sig <= ant && cond then (sig, True) else (sig, False)



--II. Sea la definición de la función foldl:
