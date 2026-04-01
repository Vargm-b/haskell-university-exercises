-- Definir una función que reciba una vocal y devuelva la siguiente vocal en orden
-- (a→e→i→o→u→a) considerando también mayúsculas.
sigVocal c = case c of
    'a' -> 'e'
    'e' -> 'i'
    'i' -> 'o'
    'o' -> 'u'
    'u' -> 'a'
    'A' -> 'E'
    'E' -> 'I'
    'I' -> 'O'
    'O' -> 'U'
    'U' -> 'A'
    _ -> error "No es un a vocal"


-- Definir una función que reciba un número del 1 al 9 y devuelva su nombre en texto.
literal x = case x of
    1 -> "uno"
    2 -> "dos"
    3 -> "tres"
    4 -> "cuatro"
    5 -> "cinco"
    6 -> "seis"
    7 -> "siete"
    8 -> "ocho"
    9 -> "nueve"


-- Definir la operación lógica AND para dos valores binarios (0 y 1).
and x y = case (x, y) of
    (1, 1) -> 1
    _ -> 0 


-- Definir la operación lógica OR para dos valores binarios (0 y 1).
or x y = case (x, y) of
    (0, 0) -> 0
    _ -> 1


-- Definir la operación lógica XOR para dos valores binarios (0 y 1).
xor x y = case (x, y) of
    (1, 0) -> 1
    (0, 1) -> 1
    _ -> 0


-- Definir una función que reciba dos números y devuelva el menor.
menor x y  = case (x < y) of
    True -> x
    False -> y


-- 13. Definir una función que reciba 6 números y devuelva el menor.
men a b c d e f = case (menor a b, menor c d, menor e f) of
    (x, y, z) -> menor x (menor y z)
    where 
        menor x y = case (x < y) of
            True -> x
            False -> y


-- 14. Definir una función que reciba 3 números y devuelva el mensaje
-- “Sumatoria mayor” si la sumatoria de los números es menor que 20,
-- el mensaje “Sumatoria menor” si la sumatoria es menor que 10
-- y el mensaje “Vacio” en otro caso.
sumatoria x y z = case suma of 
    a | a < 10 -> "sumatoria menor"
    a | a < 20 -> "sumatoria mayor"
    _ -> error "no valido"
    where
        suma = x + y + z


-- 15. Definir una función que reciba 3 notas que devuelva el mensaje
-- “Excelente” si el promedio está entre 90-100,
-- “Bien” si está entre 70-89,
-- “Regular” si está entre 51-69
-- y “Mal” si está entre 0-50.
notas a b c = case promedio of 
    x | x >= 0 && x <= 50 -> "mal"
    x | x >= 51 && x <= 69 -> "regular"
    x | x >= 70 && x <= 89 -> "bien"
    x | x >= 90 && x <= 100 -> "excelnte"
    where 
        promedio = (a + b + c) / 3