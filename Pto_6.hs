module Pto_6 where
import PdePreludat
import Pista
import Pto_2


--E2.1a
costodeinscripcion::Auto->Number
costodeinscripcion auto=(velocidadmax auto) *1000

adiciondeAuto :: Auto->(String,[Auto],Number)->(String,[Auto],Number)
adiciondeAuto autoagregado (nombre,autos,presupuesto)
    | (presupuesto >= costodeinscripcion autoagregado) = (nombre,(autoagregado:autos),(presupuesto - costodeinscripcion autoagregado))
    | otherwise = (nombre,autos,presupuesto)


