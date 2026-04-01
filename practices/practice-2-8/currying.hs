-- I Escribir la definición de tipos de las siguientes funciones.

--1
f1 :: Bool -> Int -> Int -> Int
f1 x y z = if x then y+10 else z
--2
f2 :: Int -> Int -> Char -> Int
f2 x y z = if z == 's' then 2*x else y
--3
f3 :: (Int -> Bool) -> (Char -> Bool) -> (Bool -> Bool) -> Bool
f3 x y z = (x 2) && (y 'a') && (z True)
--4
f4 :: (Int -> Bool) -> a -> b -> Int -> b
f4 = (\x -> \y -> \z -> \w -> if x 2 then z else w+10)
--5
f5 :: ( (a -> Int, a) -> (b -> Int, b) -> Int )
f5 = (\(x,y) -> \(z ,w) -> (x y )+ (z w))
--6
f6 :: a -> a --es un tipo polimorfico
f6 x = x
--7
f7 :: (a -> b) -> a -> b
f7 x y = x y
--8
f8 :: (b -> c) -> (a -> b) -> a -> c
f8 x y z = x (y z)
--9
f9 :: (a -> b -> c) -> a -> b -> c
f9 x y z = (x y) z
--10
f10 :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d
f10 x y z w= x (y (z w))
--11
f11 :: (a -> b -> c -> d) -> a -> b -> c -> d
f11 x y z w= ((x y) z) w
--12
f12 :: (a -> b -> c) -> a -> (d -> b) -> d -> c
f12 x y z w= (x y) (z w)
--16
f16 :: (a -> b -> c) -> a -> b -> c
f16 x y z = x y z
--17
f17 :: Bool -> a -> a -> a
f17 x y z 
    | x = y
    | y = z
--18
f18 :: (a -> a -> b) -> (c -> a) -> c -> b
f18 x y z = x (y z)(y z)
--19
c :: (a -> b -> c) -> (a,b) -> c
c f (x, y) = f x y
--20
u :: ((a,b) -> c) -> a -> b -> c
u f x y = f(x, y)
--21
f21 :: Num a => b -> a -> (a -> a) -> a
f21 x y z = s + y
    where s = z y
--22
f22 :: Num d => (Int -> a -> d) -> a -> (Int -> d) -> d
f22 x y z = r1 + r2
    where r1 = x 5 y
          r2 = x y z
--23
c1 :: ((a,b) -> c) -> a -> b -> c
c1 f = g
    where g x y = f (x, y)
--24
u1 :: (a -> b -> c) -> (a,b) -> c
u1 f = g
    where g (x,y) = f x y
--25
c2 :: ((a,b,c) -> d) -> a -> b -> c -> d
c2 f = g
    where g x y z = f(x, y, z)
--26
f26 :: a -> Bool -> (Bool -> Bool) -> a -> (a -> a) -> a
f26 e x y z w = if x&&(y x) then z else w z
    where w a |a = e
              |otherwise = z

--II Sean las siguientes funciones 

--curry1::((tx, ty) -> trf) -> tx -> ty -> trf  
curry1 f x y = f(x,y)
--curry2::((tx, ty) -> trf) -> tx -> ty -> trf  
curry2 f = g
    where g x y = f(x,y)
--uncurry1::(tx -> ty -> trf) -> (tx, ty) -> trf 
uncurry1 f (x,y) = f x y
--uncurry2::(tx -> ty -> trf) -> (tx, ty) -> trf  
uncurry2 f = g
    where g(x,y)= f x y
--suma1:: Int -> Int -> Int  
suma1 x y = x + y
--suma2 :: (Int, Int) -> Int
suma2 (x,y) = x + y

--Escribir los tipos de las siguientes expresiones:
{-

5. curry1 suma1:: 
6. curry1 suma2::
7. uncurry1 suma1::
aqui quedamos 
8. curry1 suma2 3 5::
9. curry1 suma2 7::
10. curry1 uncurry1 suma1 2 3::
11. uncurry1 (curry1 suma2) (2,3)
12. uncurry1 (curry1 suma2 (2,3))::
-}

