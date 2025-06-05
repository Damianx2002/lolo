module Pto_2_5 where
import Pista
import Pto_2 --autoParamas
import Pto_4 --desgasteTramo

import PdePreludat

--5
pasarPorTramo :: Tramo -> Auto -> Auto
pasarPorTramo tramo auto 
 |autoParamas auto = desgasteTramo auto tramo
 |otherwise = auto
