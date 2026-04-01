--1. Definir una función que reciba 6 números y devuelva el menor

menor a b c d e f = case (menor a b, menor c d, menor e f) of
    (x, y, z) -> menor x (menor y z)
    where 
        menor x y = if x > y then y else x 
--otra manera
menorL a b c d e f = let 
    mina = if  a < b then a else b
    minb = if c > d then d else c 
    minc = if e < f then e else f
    minr = if mina < minb then mina else minb 
    mint = if minr < minc then minr else minc
    in mint

--2. Definir una función que reciba 3 números y devuelva el mensaje “Sumatoria mayor” si la sumatoria de los números es menor que 20, el mensaje “Sumatoria menor” si la sumatoria es menor que 10 y el mensaje “Vacio” en otro caso.
suma x y z = let 
    sumat = x + y z
    resp = if sumat < 10 then "sumatoria menor"
           else if sumat < 20 then "sumatoria mayor"
           else "vacio"
    in resp
--otra manera
sumai x y z = let 
    sumat = x + y + z
    resp = case sumat of 
        s | s < 10 -> "sumatoria menor"
        s | s < 20 -> "sumatoria mayor"
        _ -> "vacio"
    in resp
--3. Definir una función que reciba 3 notas que devuelva el mensaje “Excelente“ si el promedio esta entre 90-100, “Bien” si esta entre 70-89, “Regular” si esta entre 51-69 y mal si esta entre 0-50.
notas a b c = let
    promedio = (a +b +c) / 3 
    res = case promedio of
        p | p >= 0 && p <= 50 -> "mal"
        p | p >= 51 && p <= 69 -> "regular"
        p | p >= 70 && p <= 89 -> "bien"
        p | p >= 90 && p <= 100 -> "excelente"

    in res