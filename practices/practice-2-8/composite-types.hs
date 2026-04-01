data Rpta = Entero Int | Mensaje String
    deriving (Show)
mayor x y 
    | x > y = Entero x
    | y > x = Entero y
    | otherwise = Mensaje "Iguales" 

elMayor (Entero a) (Entero b) = mayor a b 
mayorDeMay (Entero x) (Entero y) (Entero z) (Entero w) = elMayor (mayor x y) (mayor w z)

{-
data Enteros = Positivos Int | Negativos Int
    deriving Show
data Multi x y = x * y
    deriving Show
data Resta x y = x - y
    deriving Show
data Suma x y = x + y
    deriving Show
data Div = div x y
    deriving Show

-}
data Lista a = Vacia | Add a (Lista a)
    deriving Show


ocurrencias ss cs = contar ss cs
    where 
        contar Vacia Vacia = 1
        contar _ Vacia = 0
        contar Vacia (Add y ys) = 1 + contar ss (Add y ys)
        contar (Add x xs) (Add y ys)
            | x == y = contar xs ys
            | otherwise = contar ss ys
            

data Arboln a = Hoja a |Rama (Arboln a) (Arboln a) 
    deriving Show

totalHojas (Hoja _) = 1
totalHojas (Rama izq der) = (totalHojas izq) + (totalHojas der)

sumaHojas (Hoja x) = x
sumaHojas (Rama izq der) = (sumaHojas izq) + (sumaHojas der)

nodosTerm (Hoja _) = 0
nodosTerm (Rama izq der) = 1 + (nodosTerm izq) + (nodosTerm der)

comparar2Arbol (Hoja x) (Hoja x1) = x == x1
comparar2Arbol (Rama izq der) (Rama izq1 der1) = comparar2Arbol izq izq1 && comparar2Arbol der der1 

--(Rama (Rama (Hoja 2) (Rama (Hoja 5) (Hoja 6))) (Hoja 100)) (Rama (Rama (Hoja 2) (Rama (Hoja 5) (Hoja 6))) (Hoja 100))

arb1::Arboln Integer
arb1 = Rama (Rama (Hoja 2) (Rama (Hoja 5) (Hoja 6))) (Hoja 100)

compararListaArb (y:[]) = True
compararListaArb (x:y:xs)
    | comparar2Arbol x y = compararListaArb (y:xs)
    | otherwise = False

compararListaArb1 (Add y Vacia) = True
compararListaArb1 (Add x (Add y xs))
    | comparar2Arbol x y = compararListaArb1 (Add y xs)
    | otherwise = False
--compararListaArb [(Rama (Rama (Hoja 2) (Rama (Hoja 5) (Hoja 6))) (Hoja 100)), (Rama (Rama (Hoja 2) (Rama (Hoja 5) (Hoja 6))) (Hoja 100)), (Rama (Rama (Hoja 2) (Rama (Hoja 5) (Hoja 6))) (Hoja 100)), (Rama (Rama (Hoja 2) (Rama (Hoja 5) (Hoja 6))) (Hoja 100))]   
----foldr lista y Arbol
mifoldr (h,r) (Hoja x) = h x 
mifoldr (h,r) (Rama izq der) = r (mifoldr (h,r) izq) (mifoldr (h,r) der) 

mifold (v,a) Vacia = v
mifold p@(v,a) (Add x xs) = a x (mifold p xs) 

----Arbol de tree 

data ArbolT a = HojaT a | RamaT a (ArbolT a) (ArbolT a) (ArbolT a) | RamaB a (ArbolT a) (ArbolT a)
    deriving (Show, Eq) 

arb3 :: ArbolT Integer
arb3 = RamaT 4 
       (RamaT 1 
          (HojaT 2) 
          (RamaB 3 
             (HojaT 5) 
             (HojaT 4)) 
          (HojaT 4)
       )
       (HojaT 4)
       (RamaB 4 
          (HojaT 6) 
          (HojaT 8) 
       )

arb4 :: ArbolT Integer
arb4 = RamaT 4 
       (RamaT 1 
          (HojaT 2) 
          (RamaB 3 
             (HojaT 5) 
             (HojaT 4)) 
          (HojaT 4)
       )
       (HojaT 4)
       (RamaB 4 
          (HojaT 9) 
          (HojaT 8) 
       )


totalHoj (HojaT _) = 1
totalHoj (RamaB _ izq der) = (totalHoj izq) + (totalHoj der)
totalHoj (RamaT _ izq med der) = totalHoj izq + totalHoj med + totalHoj der

nodTerm (HojaT _) = 0
nodTerm (RamaB _ izq der) = 1 + (nodTerm izq) + (nodTerm der)
nodTerm (RamaT _ izq med der) = 1 + (nodTerm izq) + (nodTerm med) + (nodTerm der)

mifol (h,r) (HojaT x) = h x
mifol p@(h,r) (RamaT x izq med der) = r (r (mifol (h,r) izq) (mifol (h,r) med)) ((mifol (h,r) der))
mifol p@(h,r) (RamaB x izq der) =  r (mifol p izq) (mifol p der)

mifolT (h,r) (HojaT x) = h x
mifolT p@(h,r) (RamaT _ izq med der) =1 + r (r (mifolT (h,r) izq) (mifolT (h,r) med)) ((mifolT (h,r) der))
mifolT p@(h,r) (RamaB _ izq der) = 1 + r (mifolT p izq) (mifol p der)

mifols (h,r) (HojaT x) = h x
mifols p@(h,r) (RamaT x izq med der) = r x (r (r (mifols p izq) (mifols p med)) ((mifols p der)))
mifols p@(h,r) (RamaB x izq der) =  r x (r (mifols p izq) (mifols p der))


mifoldc (h,r) (HojaT x) (HojaT y) = h x y
mifoldc p@(h,r) (RamaT x izq med der) (RamaT y izq1 med1 der1) = r x y && mifoldc p izq izq1 && mifoldc p med med1 && mifoldc p der der1
mifoldc p@(h,r) (RamaB x izq der) (RamaB y izq1 der1) = r x y && mifoldc p izq izq1 && mifoldc p der der1
mifoldc _ _ _ = False


instance Eq a 