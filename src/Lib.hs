module Lib where
import Text.Show.Functions
type Nombre = String
type Edad = Float
type Peso = Float
type Enfermedad = String

laVerdad = True

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

data Raton = UnRaton {
nombre :: Nombre,
edad :: Edad,
peso :: Peso,
enfermedades :: [Enfermedad]
} deriving (Show, Eq)


--punto 1

cerebro :: Raton
cerebro = UnRaton "Cerebro" 9 0.2 ["brucelosis","sarampion","tuberculosis"]

bicenterrata :: Raton
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []

huesudo :: Raton
huesudo = UnRaton "Huesudo" 4 10 ["alta obesidad","sinusitis"]

--punto 2

type Hierba = Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena raton = raton{edad = (sqrt.edad) raton}

hierbaVerde :: String -> Hierba
hierbaVerde tipo raton = raton {enfermedades = filter (terminaEn tipo) (enfermedades raton) }

terminaEn :: String -> Enfermedad -> Bool
terminaEn tipo raton =  drop (length raton - length tipo) raton == tipo

alcachofa :: Hierba
alcachofa raton = raton {peso=(adelgazar.peso) raton}

adelgazar :: Peso -> Peso
adelgazar peso | (>2) peso  = peso - peso*0.10
               | otherwise =  peso - peso*0.05

hierbaZort :: Hierba
hierbaZort raton = (pinky.bandAid.pocionJuventud) raton

pocionJuventud :: Hierba
pocionJuventud raton = raton {edad=0}

bandAid :: Hierba
bandAid raton = raton{enfermedades = []}

pinky :: Hierba
pinky raton = raton{nombre = "Pinky"}

hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = (perdePesoFlaco.borrarEnfermedades) raton

borrarEnfermedades :: Hierba
borrarEnfermedades raton = raton{enfermedades = filter ((<10).length) (enfermedades raton)}

perdePesoFlaco :: Hierba
perdePesoFlaco raton | ((== 0).peso) raton  = raton{peso=peso raton}
                     | otherwise = raton{peso=peso raton - 0.1}

-- punto 3
type Medicamento = Raton -> Raton

pondsAntiAge :: Medicamento
pondsAntiAge raton = componer [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa] raton

componer :: [Hierba] -> Medicamento
componer [] = id
componer (x:xs) = x . componer xs

reduceFatFast ::Int -> Medicamento
reduceFatFast = componer . listaDeHierbas

listaDeHierbas :: Int -> [Hierba]
listaDeHierbas potencia = replicate potencia alcachofa ++ [hierbaVerde "Obesidad"]

pdepCilina :: Medicamento
pdepCilina raton = componer (map hierbaVerde sufijosInfecciosas) raton

--punto 4
type Comunidad = [Raton]

cantidadIdeal :: (Int->Bool) -> Int
cantidadIdeal funcion = head (filter funcion [1..])

lograEstabilizar :: Comunidad->Medicamento->Bool
lograEstabilizar raton medicamento = all tresEnfermedades (map medicamento raton) && all sinSobrepeso (map medicamento raton) 

sinSobrepeso :: Raton -> Bool
sinSobrepeso raton = (not.(>1).peso) raton

tresEnfermedades :: Raton -> Bool
tresEnfermedades raton = ((<3).length.enfermedades) raton

--experimento :: Comunidad -> Int
--experimento comunidad = cantidadIdeal (\potencia -> lograEstabilizar (reduceFatFast potencia) comunidad) 

--punto 5

{- A: En este caso no se podria determinar si se puede estabilizar a la comunidad, ya que el programa no podria completar de estabilizar una
lista infinita.
B: En este caso si se podria determinar, debido a la lazy evaluation que realiza haskell a la hora de procesar los comandos. Al iterar
en la lista infinita, haskell eventualmente encontraria un raton que cumpla con las condiciones 2kg y 4 enfermedades, por lo tanto saldria
de la lista y devolveria un valor verdadero
-}

{- Punto 6

A: no se deberia hacer ningun cambio al codigo, ademas de escribir la nueva hierba y el nuevo medicamento.
B: se utiliza el concepto de abstraccion y TADs. En este caso se utiliza para poder agregar nuevos cambios al codigo sin tener que alterar
el codigo ya existente.
C: deberia modificar alcachofa, perdePesoFlaco y adelgazar

-}