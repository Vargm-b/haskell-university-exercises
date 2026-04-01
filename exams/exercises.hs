type Dia=Int
type Mes=Int
type Anio=Int
type Fecha= (Dia, Mes, Anio)

type Ci=Int
type FecNac=Fecha
type Nombre=String
type Sexo = Char
type Paciente=(Ci, Nombre, FecNac, Sexo)

type Fecinternacion=Fecha
type FecSalida=Fecha
type MotivoSalida=Char
-- R=recuperado
--F=fallecido
type Tratamiento= String

type Internacion=(Paciente, Fecinternacion, FecSalida, Tratamiento, MotivoSalida)
type Listalnternaciones = [Internacion]

data Lista a = Vacia | Add a (Lista a)
    deriving Show

-- Datos de prueba
paciente1 = (123, "Juan Pérez", (15, 6, 1935), 'M')  -- Cumple
paciente2 = (456, "María Gómez", (20, 3, 1945), 'F')  -- No cumple (mujer)
paciente3 = (789, "Carlos López", (10, 12, 1930), 'M') -- Cumple
paciente4 = (321, "Pedro Martínez", (5, 8, 1938), 'M') -- Cumple (pero veremos motivo)

-- Internaciones
internacion1 = (paciente1, (1, 1, 2020), (15, 1, 2020), "Antibióticos", 'F')  -- Cumple
internacion2 = (paciente2, (1, 2, 2020), (20, 2, 2020), "Analgésicos", 'R')    -- No cumple
internacion3 = (paciente3, (1, 3, 2020), (10, 3, 2020), "Quimioterapia", 'F')   -- Cumple
internacion4 = (paciente4, (1, 4, 2020), (5, 4, 2020), "Cirugía", 'R')         -- No cumple (no falleció)

-- Crear lista personalizada
listaInternaciones = Add internacion1 
                   (Add internacion2 
                   (Add internacion3 
                   (Add internacion4 Vacia)))

procesa Vacia = 0
procesa (Add x xs)
    | cumple x = 1 + procesa xs
    | otherwise = procesa xs

cumple (p,_,_,m) = esMasc p && m == 'F'
esMasc (_,_, f, s) = antes f && s == 'M'
    where 
        antes (_,_,a) = a < 1940


type Codigo = Int
type Valor = Int
data ArbEsp = Terminal | Bifurcacion (Codigo,Valor) ArbEsp ArbEsp

a1 = Bifurcacion (200,1) 
     (Bifurcacion (100,20)
        (Bifurcacion (50,8) Terminal Terminal)
        (Bifurcacion (150,400)
            (Bifurcacion (120,5) Terminal Terminal)
            Terminal
        )
     )
     (Bifurcacion (500,30)
        (Bifurcacion (400, 2) Terminal Terminal)
        (Bifurcacion (600,8) Terminal Terminal)
     )

busca Terminal _ = ("Fracaso", 0)
busca (Bifurcacion (c,v) izq der) x
    | x > c = case busca der x of
              (res, suma) -> (res, v + suma)
    | x < c = case busca izq x of 
              (res, suma) -> (res, v + suma)
    | x == c = ("Exito", v)

type Carnet=Int
type Promedio=Int
type Nombre=String
type Alumo =(Nombre, Cantidad De Faltas, Promedio)
type GraficaDeFaltas=[Char]
type AlumnoResp = (nombre, GraficaDeFaltas)

faltas xs = [tupla x | x <- xs]
    where 
        contar c = [x |'*' <- c]
        tupla (nom,c,_)= (nom,contar c) 

data Lista a = Vacia | Add a (Lista a)
data Natural = Cero | Sucesor Natural

procesaFila Vacia Vacia = Vacia 
procesaFila (Add x xs) (Add y ys) 
    | x == y = Add x (procesaFila xs ys)
    | otherwise = Add '*' (procesaFila xs ys)
procesaFila (Add x xs) Vacia = Add '*' (procesaFila xs Vacia)
procesaFila Vacia (Add y ys) = Add '*' (procesaFila Vacia ys)

data Lista a = Sinelementos | Aumentar a (Lista a)
data Natural = Nada | Unomas Natural
type Cadena = Lista Char
type PosicionInicial = Natural
type PosicionFinal = Natural

funi Cadena PosicionFinal PosicionInicial = buscarP 0 Cadena PosicionFinal PosicionInicial
buscarP i (Aumentar x Cadena) pos