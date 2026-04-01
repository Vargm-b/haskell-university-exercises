{-type Carnet = Int 
type Nombre = String
type ApellidoP = String
type ApellidoM = String
type Sexo = Char 
type FechaNacim = (Int, Int, Int)
type Perseona = (Carnet, Nombre, ApellidoP, ApellidoM, Sexo, FechaNacim)
devuelvaSex :: Persona -> Sexo
devuelvaSex (_, _, _, _, sex, _) = sex

devuelvaNom :: Persona -> Nombre
devuelvaNom (_, nom, _, _, _, _) = nom

edadAprox :: Persona -> Nombre
edadAprox añoActual (_, _, _, _, _, (_,_,año)) = añoActual - año
-}
--enumerdos  : constructores de tipo = 
data Logico = Verdad | Falso 
    deriving Show
miAnd:: Logico -> Logico -> Logico
miAnd x y = case (x,y) of 
    (Verdad, Verdad) -> Verdad
    _ -> Falso

miOr x y  = case (x, y) of 
    (Falso, Falso) -> Falso
    _ -> Verdad

nota :: Int -> Logico
nota n
    | n >= 51 = Verdad
    | otherwise = Falso
data literal = A
notal literal
    | 