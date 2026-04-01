
iguales [] [] = True
iguales (x:xs) (y:ys) = x == y && iguales xs ys
iguales _ _ = False

--2----------------------------
fusionar [] ys = ys
fusionar xs [] = xs
fusionar (x:xs) (y:ys)
    | x <= y    = x : fusionar xs (y:ys)
    | otherwise = y : fusionar (x:xs) ys

--3-------------
esMatriz [] = True
esMatriz [_] = True
esMatriz (x:y:xss) = length x == length y && esMatriz (y:xss)
--4-------------------------
ordenar [_] f = True
ordenar (x:y:xs) f = f x y && ordenar (y:xs) f

filasOrd [] f = True 
filasOrd (x:xss) f =  ordenar x f && filasOrd (xss) f

matrizOrd [_] f = True
matrizOrd (x:y:xss) f = f (last x) (head y) && matrizOrd (y:xss) f

ordMat xss f = filasOrd xss f && matrizOrd xss f
--5----------------
pares [] = []
pares (x:xs)
    | rem x 2 == 0 = x:pares (xs)
    | otherwise = pares (xs)
--6-----------------------------
lispar [] = []
lispar (x:xss) 
    | rem (length x) 2 == 0 = x: lispar (xss)
    | otherwise = lispar xss 
--7--------------------
modi [] = []
modi (x:xs) 
    | rem x 2 /= 0 = x : modi xs
    | otherwise = modi xs

borrarpar [] = []
borrarpar (x:xss) = (modi x) : borrarpar xss
--8-----------------------------

penultimos [] = []
penultimos (x:xss) = drop ((length x) - 2) x : penultimos xss 
--9--------------
posiblesdiv x = (divisores 1)
    where 
        divisores i 
            | i * i > x = []
            | mod x i == 0 = 
                if div x i == i then [i] 
                else i : (div x i) : divisores (i + 1)
            | otherwise = divisores (i + 1)

--10------------
busqSecuencial [] n = -1
busqSecuencial (x:xs) n
    | x == n = 0
    | esta /= - 1 = 1 + esta
    | otherwise = esta
        where 
            esta = busqSecuencial(xs) n


--11-------------
binarySearch xs n = buscar (-1) (length xs)
    where 
        buscar l r 
            | r - l <= 1 = -1
            | xs!! mid == n = mid
            | xs !! mid > n = buscar l (mid)
            | otherwise = buscar (mid) r
            where 
                mid = div  (l + r) 2


--12--------------
{-
swap n menor (x:xs) 
    | x == menor = n:xs
    | otherwise = x: swap n menor xs

sorti (x:xs) = menor (xs): sorti(swap x menor xs)
    where
        esMenor (n) (x:xs)
            | xs == [] = True
            | n <= x =  esMenor (n) (xs)
            | otherwise = False
        menor (z:zs) 
            | esMenor z zs = z
            | otherwise = menor zs

-}
-----13-----
ordenaInser [] = []
ordenaInser (x:xs) = insertar x (ordenaInser xs)
    where 
        insertar x [] = [x]    
        insertar x (y:ys) 
            | x <= y = x:y:ys
            | otherwise = y:(insertar x ys)

----14----
{-
buble [] = []
buble (x:xs)  = cambiar x (ordena xs)
    where
        cambiar x (y:ys)
            | x <= y = x:y:ys
            | otherwise = y:cambiar x

-}

-----16----transpuesta
trans xss = form 0 xss
form i xss 
    | i == length (head xss) = []
    | otherwise = (hallar i xss):(form (i+1) xss)
hallar _ [] = []
hallar i (x: xss) = x !! i: (hallar i xss)

----17 --2matrices y multiplicar 
{-
suma [] _ = 0
suma _ [] = 0
suma (x:xs) (y:ys) = (x * y) + suma xs ys  
multiplic [] _ = []
multiplic (fila) (col:ms) = (suma fila col) : multiplic fila ms
princ [] _ = []
princ (fila:ns) ms = multiplc fila (trans ms): princ ns ms
-}

---21-----
funci xs ys = if (length xs) > (length ys) then estaInc xs ys else estaInc ys xs  
estaInc [] [] = True
estaInc _ [] = True
estaInc [] _  = False
estaInc (m:ms) (p:ps) 
    | p == m = estaInc ms ps
    | otherwise = estaInc (ms) (p:ps)


posi xs ys = encontrar xs ys 0 

encontrar _ [] _ = []
encontrar xs (y:ys) i 
    | (esta xs (y:ys)) = i : encontrar xs ys (i+1)   
    | otherwise = encontrar xs ys (i+1)


esta [] _ = True
esta (x:xs) (y:ys) 
    | x == y = esta xs ys 
    | otherwise  = False





