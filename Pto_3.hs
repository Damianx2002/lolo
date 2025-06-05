module Pto_3 where
import Pista 
import PdePreludat 

--3a
reparar :: Number -> Number
reparar nmo = nmo *0.15

reparoAuto :: Auto -> Auto
reparoAuto auto = auto {desgaste= UnDesgaste {chasis= reparar (chasis (desgaste auto)), ruedas=0}}

--3b
penalidadAuto :: Auto -> Number -> Auto
penalidadAuto auto tiempoextra = auto{tiempodecarrera= tiempodecarrera auto + tiempoextra}

--3c
nitro :: Number -> Number
nitro nmo = nmo *1.2

ponerNitro  :: Auto -> Auto
ponerNitro  auto= auto{velocidadmax= nitro (velocidadmax auto)}

--3d
bautizoAuto :: Auto -> String -> Auto
bautizoAuto auto nombrenuevo = auto{apodos=nombrenuevo : apodos auto}

--3e
desarmaderoAuto :: Auto -> String -> String -> Auto
desarmaderoAuto auto nuevamarca nuevomodelo = auto{marca=nuevamarca, modelo=nuevomodelo, apodos="Nunca Taxi":filter null (apodos auto)}
