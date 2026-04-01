module Program where

-- nombre :: tipo
-- nombre parametro = qué hace la función

-- Calcula el doble de un número entero
doble :: Int -> Int --Recibe un número entero y devuelve su doble.
doble n = n + n

-- Otra forma de calcular el doble usando multiplicación
doble' :: Int -> Int
doble' n = 2 * n

-- Calcula el cuádruple de un número usando la función doble
cuad :: Int -> Int 
cuad n = doble (doble n)

-- Calcula el cuádruple de un número directamente
cuad' :: Int -> Int
cuad' n = 4 * n

-- Calcula el triple de un número
triple :: Int -> Int
triple x = x * 3

-- Calcula el factorial de un número natural
factorial :: Int -> Int
factorial n = product [1..n]

-- Calcula la media aritmética de una lista de enteros
media :: [Int] -> Int
media ns = sum ns `div` length ns

-- Ordena una lista de menor a mayor usando el algoritmo quicksort
ordena :: Ord a => [a] -> [a] --Recibe una lista de elementos comparables y devuelve la lista ordenada de menor a mayor.
ordena [] = []
ordena (x:xs) =
    ordena menores ++ [x] ++ ordena mayores
  where
    menores = [a | a <- xs, a <= x]
    mayores = [b | b <- xs, b > x]