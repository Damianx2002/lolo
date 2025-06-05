module Pto_2 where
import Pista 
import PdePreludat 


--2a
buenEstadoAuto :: Auto -> Bool
buenEstadoAuto (UnAuto marca _ (UnDesgaste chasis ruedas) _ tiempodecarrera _ ) 
            | marca=="Peugeot" = False  
            | (tiempodecarrera<100 && chasis<20 ||  tiempodecarrera>=100 && chasis<40 && ruedas<60) = True  
            | otherwise = False

--2b
autoParamas:: Auto -> Bool
autoParamas (UnAuto _ _ (UnDesgaste chasis ruedas) _ _ apodos) 
            |(take 2 (head apodos)  == "La" && chasis>80 || ruedas>80 ) = False
            |otherwise = True

--2c
-- Devuele la cantidad de apodos no nulos
cantApodos :: [String] -> Number
cantApodos apodos =  length (filter (not . null) apodos )

esunChicle :: Auto -> Bool
esunChicle (UnAuto _ _ (UnDesgaste chasis ruedas) _ _ apodos) = 
    (even (cantApodos apodos )&& chasis < 20 ) || (odd (cantApodos apodos) && chasis < 50 )

-- 2d
esunaJoya :: Auto -> Bool
esunaJoya (UnAuto _ _ (UnDesgaste chasis ruedas) _ _ apodos) =  
    chasis == 0  && cantApodos apodos <= 1

-- 2e
niveldeChetez :: Auto -> Number
niveldeChetez (UnAuto _ modelo (UnDesgaste _ _) _ _ apodos) =
    20 * (cantApodos apodos) * (length modelo)

--2f
capacidadSupercalifragilisticaespialidosa :: Auto -> Number
capacidadSupercalifragilisticaespialidosa (UnAuto _ _ _ _ _ apodos) = length (head apodos) 

--2g
multiplicadorEstado :: Bool -> Number
multiplicadorEstado estado 
    | estado == True = 1
    | otherwise = 2


riesgoAuto :: Auto -> Number
riesgoAuto auto = 
   (velocidadmax auto) * (ruedas (desgaste auto)  / 10)  * multiplicadorEstado(buenEstadoAuto(auto))
