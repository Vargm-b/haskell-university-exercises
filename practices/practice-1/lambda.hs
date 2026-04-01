---------------------------------- Ejercicio 1

-- Definir una función que reciba un número y devuelva su cuadrado.
cuadrado = \x -> x*x

-- Definir una función que reciba dos números y devuelva
-- el área y el perímetro de un rectángulo.
rectangulo = \x y -> (x*x, 2*x + 2*y)

-- Definir una función que reciba dos números y devuelva el mayor.
mayor :: (Ord a) => a -> a -> a
mayor = \x y -> if (x > y) then x else y

-- Definir una función que reciba dos números y devuelva True
-- si el primero es mayor que el segundo, y False en otro caso.
may :: (Ord a) => a -> a -> Bool
may = \x y -> if(x > y) then True else False

-- Definir una función que compare dos números y devuelva
-- True si el primero es mayor que el segundo.
mayi :: (Ord a) => a -> a -> Bool
mayi = (>)

-- Definir una función que reciba un número y determine si es múltiplo de 2.
multiplo = \x -> (x `rem`2 == 0)

-- Definir una función que reciba un número y determine si es múltiplo de 2 y de 3.
multiplo3 = \x -> (x `rem`2 == 0 && x `rem`3 == 0)

-- Definir una función que reciba un número y devuelva su cubo.
potencia = \x -> (x ^3)



---------------------------------- Ejercicio 2

-- Definir una función que reciba dos números y devuelva el mayor.
may1 :: (Ord a) => a -> a -> a 
may1 =  \x y -> if x > y then x else y

-- Definir una función que reciba tres números y devuelva el mayor.
may3 :: (Ord a) => a -> a -> a -> a
may3 = \x y z -> (if x > y && x > z then x else if y > x && y > z then y else z)

-- Definir una función que reciba dos fechas y devuelva la fecha menor.
fecha :: (Int, Int, Int) -> (Int,Int,Int) -> (Int,Int,Int) 
fecha = \(d1, m1, a1) (d2, m2, a2) -> 
    if (a1 < a2) then (d1, m1, a1)
    else if(a2 < a1) then (d2, m2, a2)
    else if(m1 > m2) then (d1, m1, a1)
    else if(m2 > m1) then (d2, m2, a2)
    else if(d1 > d2) then (d1, m1, a1)
    else (d2, m2, a2)



---------------------------------- Ejercicio 3

-- Definir una función que reciba una nota y devuelva
-- "Excelente" si está entre 90 y 100 y "Mal" en otro caso.
nota :: Int -> String
nota = \x -> case () of 
    _ | x >= 90 && x <= 100 -> "Excelente"
      | otherwise -> "Mal"