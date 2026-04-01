
--simulacro 1--------------------
tup xs ys = map tripla (filter iguales dif)
    where 
        long = length xs 
        dif = zip(zip xs ys) [0..long -1]
        iguales ((x,y), n) = if x /= y then True else False
        tripla ((x,y),n) = (x,y,n)
-- *ojo examen*

procesa xs = foldr f ([],"") xs
    where 
        f x (as,bs) 
            | x == '-' || x == ' ' || x == '.' = (as ++ [bs], "")
            | otherwise = (as, x :bs)

fun f g c = if f c == 1 && g este == 'c' && c == 'A' then 1 else 0
    where 
        este bo = if bo then 1 else 0

--simulacro2---------------------------

fun1 f g b = if f (2,'c') && b then (g (b, 2,'c') 7, not b) else (0, b)
 

