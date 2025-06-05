module Pto_2_6 where
import Pista
import Pto_4 -- tramos
import Pto_2_4 
import Pto_2_5 (pasarPorTramo)
import Pto_2 -- autoParamas
import PdePreludat

--6a
vueltaALaManzana = UnaPista "La manzana" "Italia" 30 [(Recta  130 []),(Curva 90 13 []),(Recta 130 []),(Curva 90 13 []),(Recta 130 []),(Curva 90 13 []),(Recta 130 []),(Curva 90 13 [])]

--6 b
superPista = UnaPista "Super Pista" "Argentina" 300 [(tramoRectoClassic),(curvaTranca),(tramoConTurbo tramito),(tramoMojado tramito), (Rulo 10 []), (tramoConObstruccion 2 (Curva 80 400 [])), (Curva 115 650 []), (Recta 970 []), (curvaPeligrosa), (tramoConRipio tramito), (tramoConBoxes(Recta 800 [])), (tramoConObstruccion 5 (casiCurva)),(Zigzag 2 []),(tramoConRipio(tramoMojado(deseoDeMuerte))),(ruloClasico),(zigZagLoco)]

--6c
peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta pista = map (darVuelta (tramos pista))

darVuelta :: [Tramo] -> Auto -> Auto
darVuelta [] auto = auto
darVuelta (tramo:tramos) auto
  | autoParamas auto = darVuelta tramos (pasarPorTramo tramo auto)
  | otherwise  = auto
