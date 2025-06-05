module Pto_5 where
import Pista
import Pto_2  -- Funciones  buenEstadoAuto esunaJoya
import PdePreludat 

--5a
tiempojoya :: Auto -> Number
tiempojoya (UnAuto _ _ (UnDesgaste _ _) _ tiempodecarrera _)
    |tiempodecarrera<50 =1
    |tiempodecarrera>=50 =2
    |otherwise = 0

sonJoya :: [Auto] -> [Auto]
sonJoya autos = filter esunaJoya autos

niveldeJoyez :: [Auto]->Number
niveldeJoyez autos = sum(map tiempojoya (sonJoya autos)) 

--5b
tiempoEntendido :: Auto -> Bool
tiempoEntendido auto =tiempodecarrera auto <= 200 

buenoyEntendido :: Auto -> Bool
buenoyEntendido auto = buenEstadoAuto auto && tiempoEntendido auto

buenosyEntendidos ::[Auto] -> Bool
buenosyEntendidos autos = all buenoyEntendido autos

