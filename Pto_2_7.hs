module Pto_2_7 where
import Pista
import Pto_2 -- autoParamas
import Pto_2_6 -- superPista y peganLaVuelta
import PdePreludat

-- 7a
data Carrera = UnaCarrera {
  pistaCarrera :: Pista,
  vueltasCarrera :: Number
} deriving Show

primerAuto :: [Auto] -> Auto
primerAuto autos =  head autos
-- Obtiene el primer auto

resto_de_Autos :: [Auto] -> [Auto]
resto_de_Autos autos = tail autos
-- Obtiene el resto de autos menos el primero

ultimaVuelta :: Carrera -> [Auto] -> [Auto]
ultimaVuelta carrera autos = last (filter (not . null) (jugarCarrera carrera autos))
-- Devuelve la última vuelta de la carrera, filtrando las que no tienen autos

--7b
tourBuenosAires :: Carrera
tourBuenosAires = UnaCarrera vueltaALaManzana 20 -- Debería ser superPista

--7c
jugarCarrera :: Carrera -> [Auto] -> [[Auto]]
jugarCarrera carrera autos = jugarVueltas (vueltasCarrera carrera) autos (pistaCarrera carrera)
-- Devuelve los resultados parciales de cada vuelta (lista de listas de autos)


jugarVueltas :: Number -> [Auto] -> Pista -> [[Auto]]
jugarVueltas 0 _ _ = []
jugarVueltas _ [] _ = []
jugarVueltas n autos pista =
    filter autoParamas (peganLaVuelta pista autos) : jugarVueltas (n-1) (filter autoParamas (peganLaVuelta pista autos)) pista
    

autoGanador :: Carrera -> [Auto] -> Auto
autoGanador carrera autos = primerAuto (ultimaVuelta carrera autos)
-- Devuelve el primer auto de la última vuelta, que es el ganador

tiempoSegundo :: Carrera -> [Auto] -> Number
tiempoSegundo carrera autos = tiempodecarrera ((ultimaVuelta carrera autos) !! 1)
-- Devuelve el tiempo del segundo auto de la última vuelta

tiempoParcial2Vueltas :: Carrera -> [Auto] -> Number
tiempoParcial2Vueltas carrera autos = tiempodecarrera (primerAuto ((jugarCarrera carrera autos) !! 1))
-- Devuelve el tiempo del primer auto de la segunda vuelta

cantidadAutosQueTerminaron :: Carrera -> [Auto] -> Number
cantidadAutosQueTerminaron carrera autos = length (ultimaVuelta carrera autos)
-- Devuelve la cantidad de autos que terminaron la carrera, es decir, los que llegaron a la última vuelta


-- Mejorar nombres de funciones y variables
-- Mejorar la logica de no repetir codigo
-- Poder hacer lo de los tramos con los modificadores (o todo data o solo funciones)
-- 