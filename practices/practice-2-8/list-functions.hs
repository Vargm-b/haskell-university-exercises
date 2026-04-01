-- II. Utilizando las funciones estándar sobre listas, definir las siguientes funciones:

f1 xs = xs !! 2
f2 xs = xs !! 1
f3 xss x = (head.head) xss x 
f4 xss = (xss !! 2) !! 4
f5 xsss = xsss !! 1 !! 3 !! 2 
f6 xs f = and(zipWith f xs (tail xs))  
f7 xs ys = if (length xs == length ys) && and(zipWith (==) xs (ys)) then True else False
f8 xss = and(map ((== (length (head xss))).length) xss)
f9 xs n = xs !! n -1
length1 xs = if xs == [] then 0 else 1 + length1 (tail xs)
-- *ojo examen*
filterr f (x:xs) 
    | xs == [] = []
    | f x = x : filterr f xs
    | otherwise = filterr f xs

--transpuesta
f11mat xss = map getc [0..(length (head xss) -1)]
    where 
        getc n = map (!! n) xss
--multi entre matrices
--error
f12 xss yss = map f xss
    where 
        long = (length xss)- 1
        f xs = map mult (zip xs [0..long])
        mult (n, i) = sum (map (*n) (map (!! i) yss))
--mejorado y correcto*examne *
fmat xss yss = map f xss
    where 
        tyss = f11mat yss  
        f xs = map (mult xs) tyss
        mult xs ys = sum(zipWith (*) xs ys); 
--mult de  3 matrices 
f13mat xss yss zss = fmat (fmat xss yss) zss
--mult de 4 matrices
f14mat wss xss yss zss = fmat (fmat (fmat wss xss) yss) zss
--ordenada mat *examne*
f15 xss f = ord (map (!! 0) xss) && and(map ord xss)
    where
        ord xs =  and(zipWith f xs (tail xs))

f16 xs = filter (even) xs
f17 xss = map length xss
f18 xss = filter (even.length) xss
f19 xss = map (filter odd) xss
f20 xss = map (last.init) xss
{-\n -> n `rem` x == 0-}
f21 n = filter (mult) [1..n]
    where 
        mult x = n `rem` x == 0
        
f22 xs ys f = map (\(x, y) -> f x y) (zip xs ys)


{-Sea:
(map f).(map g) xs = map p xs
De una definición apropiada para p
entonces p =  f . g 

Sea:
(filter q) * (filter p) = filter (p  q)
de una definición apropiada para el operador -
es operador  es un operador  binario que espera dos argumentos para  evaluar
-}









