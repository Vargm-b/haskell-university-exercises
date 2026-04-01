{-pert xs y = or[x == y | x <- xs]
inter xs  ys = [x | x <- [x| x <-]]-}
filte xs f = [x | x <- xs, f x]
mapi xs f = [f x | x <- xs]
concate xss = [[x | x<- xs]| xs <- xss] 
longit xs = sum[1 | x <- xs]
encrip xs = [f x| x<- xs]
    where
        f x = case x of
            'a' -> 1
            'e' -> 2
            'i' -> 3
            'o' -> 4
            'u' -> 5

f6 xs ys = [(x, y) | x<- xs , y <- ys]
--si x existe en mi conj
f7 xs y = or [x == y| x <- xs]
noExiste xs y = and [x /= y| x <- xs]
--interseccion de dos conj
f8 xs ys = [x | x<- xs, y <- ys, x == y]
-- elimina x del conj
f9 xs y = [x | x <- xs, x /= y]
f10 xs ys = xs ++[ y| y <- ys, y `notElem` xs]
-- sacar  la diagonal de matriz
f11 xss = [xs !! i | (xs, i) <-(zip xss [0..])]
--sacar diagonal secundaria
f12 xss = [xs !! i | (xs, i) <- zip xss [long-1, long-2..0]]
    where 
        long = length xss

--si una matriz es  un  cuadrado
f13 xss = length xss == length (head xss)

--multiplica una matriz 
transpo yss = [ys |ys <- map g [0..((length (head yss) )-1)]]
    where
        g c = map (!! c) yss

--no correcto x <- [xs | xs <- xss]
f14 xss yss = [[suma xs ys | ys <- transp]| xs <- xss]
    where 
        suma xs ys = sum(zipWith (*) xs ys)
        transp = [ys |ys <- map g [0..((length (head yss) )-1)]]
        g c = map (!! c) yss

--si  es primo
f15 x =  x > 1 && and [(x `rem` n) /= 0 | n <- [2..isqrt x]]
    where 
        isqrt = floor.sqrt.fromIntegral

--zipWith *examen*
f16 f xs ys = [f x y | (x, y) <- zip xs ys]

--zip3 ojo *examen* 
f17 xs ys zs = [(xs !! i, ys !! i, zs !! i) | i <- [0.. min((length xs) -1) (min((length xs) -1) ((length ys) - 1))]]

--zip4 
f18 ws xs ys zs = [(ws !! i, xs !! i, ys !! i, zs !! i)| i <- [0..mini ws xs ys zs]]
    where 
        mini ws xs ys zs = min (min ((length ws) -1) ((length xs) -1)) (min ((length ys) -1) ((length zs) -1))

--ver si esta ordenada  de acuerdo a la  funcion  de ordenada
f19 xs f = and[f (xs !! i) (xs !! (i + 1)) | i <- [0.. ((length xs) - 2)]]
