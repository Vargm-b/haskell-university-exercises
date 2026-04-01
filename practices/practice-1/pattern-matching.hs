-- 1. Definir una función que reciba una fecha y devuelva el día.
fecha (d, _, _) = d

-- 2. Definir una función que reciba una fecha y devuelva el mes.
fecham (_, m, _) = m

-- 3. Definir una función que reciba una fecha y devuelva el año.
año (_, _, a) = a

-- 4. Definir una función que reciba 2 quebrados y devuelva el mayor.
quebrados (a,b) (c,d) =
    if (a*d > b*c) then (a,b) else (c,d)

-- 5. Definir una función que reciba 1 quebrado y lo devuelva reducido.
quebrado (a,b) = (div a multiplo, div b multiplo)
    where
        multiplo = gcd a b

-- 6. Definir una función que reciba 1 quebrado y devuelva su signo como carácter.
quebrad (a,b) = case (a,b) of
    (x,y) | x < 0 && y < 0 -> '+'
    (x,y) | x < 0 || y < 0 -> '-'
    _ -> '+'

-- 7. Definir una función que reciba 3 fechas y devuelva la fecha menor.
fechas f1 f2 f3
    | f1 <= f2 && f1 <= f3 = f1
    | f2 <= f3 && f2 <= f1 = f2
    | otherwise = f3