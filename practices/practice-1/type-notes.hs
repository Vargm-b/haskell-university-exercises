{-
   Práctica de definición de tipos y aplicación de funciones en Haskell.

   Idea general:
   Aquí se trabaja con funciones que pueden recibir valores normales
   o incluso otras funciones como argumentos.

   El objetivo es entender:
   1. cómo se leen los tipos,
   2. cómo se aplican las funciones,
   3. cómo funciona el principio de currying,
   4. y cómo influye la asociación en Haskell.

   ---------------------------------------------------------
   PRINCIPIO DE CURRYING
   ---------------------------------------------------------

   En Haskell, una función de varios parámetros en realidad
   se interpreta como una cadena de funciones de un solo parámetro.

   Por ejemplo:

       Int -> Int -> Int

   realmente significa:

       Int -> (Int -> Int)

   O sea:
   - recibe un Int
   - y devuelve una función que recibe otro Int
   - y finalmente devuelve un Int

   Lo mismo pasa con:

       Bool -> Int -> Int -> Int

   que en realidad significa:

       Bool -> (Int -> (Int -> Int))

   Por eso esos paréntesis no se pueden borrar al azar.
   No es adorno: muestran cómo se agrupan los tipos.

   ---------------------------------------------------------
   APLICACIÓN DE FUNCIONES
   ---------------------------------------------------------

   En Haskell, la aplicación de funciones se asocia a la izquierda.

   Entonces:

       x y z

   significa:

       (x y) z

   y no:

       x (y z)

   Si quieres que se aplique primero (y z),
   tienes que escribirlo explícitamente con paréntesis.

   ---------------------------------------------------------
   IDEA IMPORTANTE
   ---------------------------------------------------------

   Si una función espera, por ejemplo, un Int,
   no puedes darle un Char o una función.
   Si una función espera una función,
   no puedes darle un número.
-}


-- =========================================================
-- PARTE I: ejemplos de lectura de tipos
-- =========================================================

-- f1 recibe:
-- 1) un Bool
-- 2) un Int
-- 3) un Int
-- y devuelve un Int.
--
-- Su tipo:
-- Bool -> Int -> Int -> Int
--
-- Por currying, también se entiende como:
-- Bool -> (Int -> (Int -> Int))
--
-- Si se fija solo el primer argumento, por ejemplo:
-- f1 True
-- entonces queda una función de tipo:
-- Int -> Int -> Int
--
-- La función dice:
-- si el primer argumento es True, devuelve y + 10
-- si no, devuelve z
f1 :: Bool -> Int -> Int -> Int
f1 x y z = if x then y + 10 else z


-- f2 recibe:
-- 1) un Int
-- 2) un Int
-- 3) un Char
-- y devuelve un Int.
--
-- Su tipo:
-- Int -> Int -> Char -> Int
--
-- También puede escribirse como:
-- Int -> (Int -> (Char -> Int))
--
-- Importante:
-- si escribieras solo f2 'a', sería error,
-- porque f2 espera primero un Int, no un Char.
--
-- La función dice:
-- si el carácter recibido es 's', devuelve 2*x
-- en otro caso devuelve y
f2 :: Int -> Int -> Char -> Int
f2 x y z = if z == 's' then 2 * x else y


-- f3 recibe tres funciones:
--
-- x :: Int -> Bool
-- y :: Char -> Bool
-- z :: Bool -> Bool
--
-- y devuelve un Bool.
--
-- Entonces su tipo completo es:
-- (Int -> Bool) -> (Char -> Bool) -> (Bool -> Bool) -> Bool
--
-- La función aplica:
-- x al número 2
-- y al carácter 'a'
-- z al valor True
--
-- y luego junta los tres resultados con &&
--
-- Importante:
-- f3 5 sería error,
-- porque f3 no espera un número como primer argumento,
-- espera una función de tipo Int -> Bool
f3 :: (Int -> Bool) -> (Char -> Bool) -> (Bool -> Bool) -> Bool
f3 x y z = (x 2) && (y 'a') && (z True)


-- =========================================================
-- PARTE II: función con lambdas anidadas
-- =========================================================

-- f4 está escrita con funciones lambda.
--
-- Recibe cuatro argumentos:
-- x debe ser una función que pueda aplicarse a 2 y devolver Bool
-- y no se usa
-- z es el valor que se devuelve si x 2 es True
-- w es un número al que se le suma 10 si x 2 es False
--
-- Explicación:
-- si x aplicado a 2 da True, devuelve z;
-- en caso contrario, devuelve w + 10.
f4 = \x -> \y -> \z -> \w -> if x 2 then z else w + 10


-- =========================================================
-- EJEMPLOS APARTE
-- =========================================================

-- h1 recibe:
-- 3 enteros, luego un Bool, luego otro entero, luego otro Bool
-- y devuelve un entero.
--
-- Si ambos booleanos son True, suma los cuatro enteros.
-- En otro caso, devuelve 5.
h1 :: Int -> Int -> Int -> Bool -> Int -> Bool -> Int
h1 n1 n2 n3 b1 n4 b2 =
    if b1 && b2 then n1 + n2 + n3 + n4 else 5


-- h2 recibe:
-- 1) una función de Int a Int
-- 2) un Int
-- y devuelve una función de tipo:
-- Bool -> Int -> Bool -> Int
--
-- O sea, después de darle los dos primeros argumentos,
-- todavía queda una función esperando tres más.
--
-- Ese es justamente un ejemplo claro de currying.
--
-- Aquí lo escribimos completo para que tenga sentido:
-- si ambos booleanos son True, aplica f a la suma de n1 y n2
-- en caso contrario devuelve n2
h2 :: (Int -> Int) -> Int -> Bool -> Int -> Bool -> Int
h2 f n1 b1 n2 b2 =
    if b1 && b2 then f (n1 + n2) else n2


-- =========================================================
-- PARTE III: práctica de definición de tipos y aplicación
-- =========================================================

-- f5 trabaja con dos pares:
-- (x, y) donde x es una función e y es su argumento
-- (z, w) donde z es otra función y w es su argumento
--
-- Aplica x a y, aplica z a w, y suma ambos resultados.
f5 = \(x, y) -> \(z, w) -> (x y) + (z w)


-- f7 recibe una función x y un argumento y.
-- Devuelve el resultado de aplicar x a y.
--
-- Es el caso más simple de aplicación.
f7 x y = x y


-- f8 primero aplica y a z,
-- y luego aplica x al resultado.
--
-- Se lee así:
-- x (y z)
--
-- Aquí sí se fuerza que primero ocurra (y z).
f8 x y z = x (y z)


-- f9 se lee distinto:
-- primero x se aplica a y
-- y el resultado de eso se aplica a z
--
-- O sea:
-- (x y) z
--
-- Esto muestra la asociación a la izquierda.
f9 x y z = (x y) z


-- f10 aplica funciones de adentro hacia afuera:
-- primero z a w
-- luego y al resultado
-- luego x al resultado final
f10 x y z w = x (y (z w))


-- f11 muestra claramente la asociación a la izquierda:
-- primero (x y)
-- luego ((x y) z)
-- luego (((x y) z) w)
f11 x y z w = ((x y) z) w


-- f12 combina dos aplicaciones:
-- primero (x y)
-- luego (z w)
-- y después aplica el primer resultado al segundo
f12 x y z w = ((x y) (z w))