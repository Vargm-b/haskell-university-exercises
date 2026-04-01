data Zona = Valle | Llano | Altiplano
    deriving (Show, Eq)
data Departamento = Cochabamba | Tarija | Beni | SantaCruz | 
                    Chuquisaca | Pando | LaPaz | Oruro | Potosi
    deriving (Show)


esValle d = case d of 
    Cochabamba -> True
    Tarija -> True
    Chuquisaca -> True
    _ -> False

zonas d = case d of 
    Cochabamba -> Valle
    Tarija -> Valle
    Chuquisaca -> Valle
    SantaCruz -> Llano
    Pando -> Llano
    Beni -> Llano
    LaPaz -> Altiplano
    Oruro -> Altiplano
    Potosi -> Altiplano

llanVall [] = []
llanVall (x:xs)
    | zonas x == Valle || zonas x == Llano = x:llanVall xs 
    | otherwise = llanVall xs 

type Dia = Int
type Mes = Int
type Año = Int
type Fecha = (Dia, Mes, Año)
type Periodo = (Fecha, Fecha)
type Nombre = String
type Presidente = (Nombre, Periodo)
--Una función que reciba un Periodo y devuelva el tiempo transcurrido en años.
tiempotransc ((d1,m1,a1),(d2,m2,a2))
    | (m2 >= m1) || (m2 == m1 && d2 > d1)= a2 -a1
    | otherwise = a2 -a1 -1

--2.Una función que reciba un Presidente y devuelva el tiempo total en años que
--gobernó.

añosGovernacion (_, p) = tiempotransc p
--3. Definir una función que reciba dos presidentes y devuelva aquel que gobernó más
--tiempo.
masAños x y
    | añosGovernacion x < añosGovernacion y = y 
    | otherwise = x
--4. Una función que reciba una lista de presidentes y devuelva el nombre del
--presidente que menos tiempo gobernó.

menostiempo (x:xs) = buscar x xs
    where 
       buscar (nom,_) [] = nom 
       buscar x (y:ys)
            | añosGovernacion x < añosGovernacion y =  buscar x ys
            | otherwise = buscar y ys 

menostimP [] = []
menostimP ((nom,(_,(_,_,a2))):xs)
    | a2 < 1990 = nom: menostimP xs
    | otherwise = menostimP xs

menos4años [] = []
menos4años (x:xs)
    | añosGovernacion x < 4 = x:menos4años xs
    | otherwise = menos4años xs

data Empleado = Docente Nombre Horas SueldoHra Materia | Administrativo Nombre Salario Cargo
    deriving (Show)
type Horas = Int 
type SueldoHra = Float
type Salario = Float
type Materia = [String]
type Cargo = String

ingreso (Docente _ h sh m) (Administrativo _ s _) = (fromIntegral h*sh* fromIntegral (length m)) * 3 == s
ingreso (Administrativo _ s _) (Docente _ h sh m) = (fromIntegral h*sh* fromIntegral (length m)) * 3 == s
ingreso _ _ = False


masMater (x:xs) = mas x xs
masMater [] = "No hay docentes"

mas x [] = nombre x
mas x  ( y:xs)
    | cantMat x  > cantMat y = mas x xs
    | otherwise = mas y xs
    
nombre (Docente n _ _ _ ) = n
cantMat (Docente _ _ _ lista) = length lista


