-- 1. Definir una función que reciba 4 números y devuelva el mayor.
-- Por combinación.
may w x y z = max (max w x) (max y z)

-- 1. Definir una función que reciba 4 números y devuelva el mayor.
-- Por distinción de casos.
mayo w x y z  
    | w >= x && w >= y && w >= z = w
    | x >= w && x >= y && x >= z = x 
    | y >= w && y >= x && y >= z = y
    | otherwise = z

-- 2. Definir una función que reciba una nota y devuelva el mensaje
-- “Aprobado” o “Reprobado”.
aprobar nota 
    | nota >= 51 = "aprobado"
    | otherwise = "reprobado"

-- 3. Definir una función que reciba una nota y devuelva el mensaje
-- “Excelente” si la nota está entre 90-100, “Bien” si está entre 70-89,
-- “Regular” si está entre 51-69 y “Mal” si está entre 0-50.
nota not1 
    | not1 >= 90 && not1 <= 100 = "Excelente"
    | not1 >= 70 && not1 <= 89 = "Bien"
    | not1 >= 51 && not1 <= 69 = "Regular"
    | not1 >= 0 && not1 <= 50 = "Mal"

-- 4. Definir una función que reciba como argumentos las notas de
-- primer parcial, segundo parcial, final y segunda instancia
-- y retorne el mensaje aprobado o reprobado, según el caso.
aproba pp sp f si
    | pp >= 51 || sp >= 51 || f >= 51 && si >= 51 = "aprobado"
    | otherwise = "reprobado"

-- 6. Definir una función que reciba un quebrado y devuelva verdad
-- si este es mayor que 1 y falso en otro caso.
quebrado x  
    | x > 1 = True
    | otherwise = False

-- 7. Definir una función que reciba 2 fechas y devuelva la fecha mayor.
fechas (d1, m1, a1) (d2, m2, a2) =
    if a1 > a2
        then (d1, m1, a1)
    else if a1 < a2
        then (d2, m2, a2)
    else
        if m1 > m2
            then (d1, m1, a1)
        else if m1 < m2
            then (d2, m2, a2)
        else
            if d1 > d2
                then (d1, m1, a1)
            else
                (d2, m2, a2)

-- Extra: combinación de fechas para obtener la mayor entre 4 fechas.
fechasi (d1, m1, a1) (d2, m2, a2) (d3, m3, a3) (d4, m4, a4) = 
    fechas (fechas (d1, m1, a1) (d2, m2, a2)) (fechas (d3, m3, a3) (d4, m4, a4))

-- 8. Definir una función que reciba 2 fechas y devuelva los años transcurridos.
año (d1, m1, a1) (d2, m2, a2)  
    | m1 == m2 && d1 <= d2 = a2 - a1
    | otherwise = a2 - a1 - 1

-- 9. Definir una función que reciba 2 fechas y devuelva los meses transcurridos.
mes (d1, m1, a1) (d2, m2, a2)  
    | d1 <= d2 && a1 == a2 = m2 - m1
    | otherwise = m2 - m1 - 1

-- 10. Definir una función que reciba 2 fechas y devuelva los días transcurridos.
dia (d1, m1, a1) (d2, m2, a2)  
    | m1 == m2 && a1 == a2 = d2 - d1
    | otherwise = año (d1, m1, a1) (d2, m2, a2) * 12 + mes (d1, m1, a1) (d2, m2, a2) * 30 + (d2 - d1)

