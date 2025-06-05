module Pto_2_2 where
import Pista
import Pto_2_1 --costoRepacion
import PdePreludat

-- 2.2

costoPorPunto :: Number
costoPorPunto = 500

costoTotalReparacion :: Equipo -> Number
costoTotalReparacion equipo = sum (map costoRepacion (autos equipo))

