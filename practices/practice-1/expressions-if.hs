
--Ejercicio1

--Definir una función que reciba el lado de un cuadrado y devuelva su área.
cuadrado x = 4 * x;

--Definir una función que reciba la base y la altura de un rectángulo y devuelva su área y su perímetro.
rectangulo base altura = (base * altura, 2 * (base + altura))

--Definir una función que reciba 2 números y devuelva verdad si el primero es mayor que el segundo.
maximo x y = x > y

--Definir una función que reciba un número y retorne verdad si este es múltiplo de 2.
par x = (x `rem` 2 )== 0

--Definir una función que reciba un número y devuelva verdad si este es múltiplo de 2 y de 3 al mismo tiempo.
multiplo x = (x `rem` 2) == 0 && (x `rem` 3) == 0

--Definir una función que reciba un número y lo devuelva elevado a la potencia 3.
potencia x = x ^ 3

-- Definir funciones que reciban un número y lo devuelvan elevado a la potencia 4,8,10,32.
potencia4 x = x ^ 4
potencia8 x = x ^ 8
potencia10 x = x ^ 10
potencia32 x = x ^ 32

--Definir una función que reciba dos números y una función de orden y devuelva verdad
si los números obedecen a la función de orden, falso en otro caso.
fOrden f x y  = f x y


--Ejercicio2

--Definir una función que devuelva el mayor de 2 números
maximo2 x y = if x > y then x else y

--Definir una función que reciba 3 números y devuelva el mayor
maximo3 x y z = maximo2 x (maximo2 y z)

--Definir una función que reciba 4 números y devuelva el mayor
maximo4 w x y z = maximo2 w (maximo3 x y z)

--Definir una función que reciba dos exámenes parciales, un final y una segunda instancia y devuelva el mensaje “Aprobado”, “Reprobado” o “Abandono” según el caso.
aprobar examf segInst = if (examf >= 51) || (segInst >= 51) then "aprobado" else "reprobado"

--Definir una función que reciba 2 fechas y devuelva la fecha mayor
aprobacion parcial1 parcial2 examenf segundaInst = if (parcial1 + parcial2) `div` 2 >= 51 then "aprobado" else aprobar examenf segundaInst
dia (d1, m1, a1) (d2, m2, a2) = 
    if d1 < d2 then (d1, m1, a1)
    else (d2, m2, a2)
mes (d1, m1, a1) (d2, m2, a2) = 
    if(m1 > m2) then (d2, m2, a2)
    else dia (d1, m1, a1) (d2, m2, a2)
igual (d1, m1, a1) (d2, m2, a2) = 
    if(m1 < m2) then (d1, m1, a1)
    else mes (d1, m1, a1) (d2, m2, a2)
menor (d1, m1, a1) (d2, m2, a2)= 
    if(a1 > a2) then (a2, m2, a2)
    else igual (d1, m1, a1) (d2, m2, a2)
mayor(d1, m1, a1) (d2, m2, a2) =  
    if(a1 < a2) then (d1, m1 ,a1)
    else menor (d1, m1, a1) (d2, m2, a2)