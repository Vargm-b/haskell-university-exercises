data Lista a = Sinelementos | Aumentar a (Lista a)
data Natural = Nada | Unomas Natural
    deriving Show
type Cadena = Lista Char
type PosicionInicial = Natural
type PosicionFinal = Natural
    
list = Aumentar 'a' (Aumentar 'b' (Aumentar 'c' (Aumentar 'd' (Aumentar 'e' (Aumentar 'f' (Aumentar 'g' Sinelementos))))))
--------- por rangos
num = Nada    -- Representa el número 2
num1 = Unomas (Unomas (Unomas (Unomas (Unomas (Unomas Nada))))) 

subcadena (Sinelementos) _ _  = []
subcadena (Aumentar x xs) (Unomas a) (Unomas b) = subcadena xs a b
subcadena (Aumentar x xs) (Nada) (Unomas b) = x:subcadena xs Nada b
subcadena (Aumentar x xs) Nada Nada = [x]
-------------------

ordenaInser [] = []
ordenaInser (x:xs) = insertar x (ordenaInser xs)
    where 
        insertar x [] = [x]    
        insertar x (y:ys) 
            | x <= y = x:y:ys
            | otherwise = y:(insertar x ys)
---------


convertir Vacia _ = ""
convertir (Add x xs) Vacia = []
convertir (Add x xs) (Add y ys) = duplicar x y ++ convertir xs ys
duplicar x Cero = ""
duplicar x (Sgte a) = x ++ (duplicar x a)
