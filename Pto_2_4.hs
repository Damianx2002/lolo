module Pto_2_4 where
import Pista
import Pto_3
import PdePreludat
import Pto_4
import Pto_2 

{-
tramoConBoxes :: Auto -> Tramo -> Auto
tramoConBoxes  auto tramo
    | buenEstadoAuto auto = desgasteTramo auto tramo
    | otherwise = auto {desgaste = desgaste(desgasteTramo (reparoAuto auto) tramo), tiempodecarrera= tiempodecarrera(desgasteTramo auto tramo)+10}

tramoMojado :: Auto -> Tramo-> Auto
tramoMojado auto tramo = auto {desgaste = desgaste (desgasteTramo auto tramo) ,tiempodecarrera= tiempodecarrera auto + 1.5*(tiempoTramo auto tramo)}

tramoConRipio :: Auto -> Tramo -> Auto
tramoConRipio auto tramo = auto {desgaste = UnDesgaste {chasis= chasis(desgaste(desgasteTramo auto tramo))*2 ,ruedas=ruedas(desgaste(desgasteTramo auto tramo))*2},tiempodecarrera=tiempodecarrera auto + (tiempoTramo auto tramo)*2}

tramoConObstruccion :: Auto -> Tramo -> Number -> Auto
tramoConObstruccion auto tramo metrosafectados = auto {desgaste= UnDesgaste {ruedas= ruedas(desgaste(desgasteTramo auto tramo))+ 2*metrosafectados, chasis=chasis(desgaste(desgasteTramo auto tramo))}, tiempodecarrera= tiempodecarrera auto + (tiempoTramo auto tramo)}

tramoConTurbo :: Auto -> Tramo -> Auto
tramoConTurbo auto tramo = auto {desgaste=desgaste(desgasteTramo autoconNitro tramo),velocidadmax= velocidadmax auto , tiempodecarrera= tiempodecarrera auto + tiempodecarrera(desgasteTramo autoconNitro tramo)}
 where
 autoconNitro= auto{velocidadmax=(velocidadmax auto)*2}
-}

--Modificando las listas de modificadores de tramos
--4a
tramoConBoxes :: Tramo -> Tramo
tramoConBoxes tramo = tramo{modificadores="Boxes":modificadores tramo}
--4b
tramoMojado :: Tramo -> Tramo
tramoMojado tramo = tramo{modificadores="Mojado":modificadores tramo}
--4c
tramoConRipio :: Tramo -> Tramo
tramoConRipio tramo = tramo{modificadores="Ripio":modificadores tramo}
--4d
tramoConObstruccion :: Number -> Tramo -> Tramo
tramoConObstruccion metrosafectados tramo= tramo{modificadores= "Obstrucciones":show (metrosafectados):modificadores tramo}
--4e
tramoConTurbo :: Tramo -> Tramo
tramoConTurbo tramo = tramo{modificadores="Turbo":modificadores tramo}



