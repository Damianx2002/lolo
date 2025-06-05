module Pto_2_3 where
import Pista
import PdePreludat


--3a
autoInfinito :: Number -> Auto
--autoInfinito x = UnAuto("Ferrari" "F50" (UnDesgaste 1 0) ( 65 * x ) _ _ )
autoInfinito x = UnAuto{marca = "Ferrari",modelo ="F50",desgaste = UnDesgaste{chasis = 1,ruedas=0},velocidadmax = 65 * x, tiempodecarrera = 0 ,apodos = []}

equipoInfinia = UnEquipo "Infinia" (map autoInfinito [1..]) 5000

{-b 
    - Cuando se realiza repararEquipo a "Infinia" la función nunca terminará debido a que se trata de un bucle infinito,
    la manera de probar si la función es útil sería con la ayuda de una función take 
    - Cuando se realiza optimizarEquipo, no se podrá optimizar ningún auto debido a que supera el presupuesto inicial de 5000
    siendo necesarios 6500 para optimizar un auto ferrari
    - No tiene sentido ferrarizar los autos ya que todos son autos ferrari, el presupuesto no se actualiza
    - Ocurre algo parecido a repararEquipo cuando se quiere utilizar costoTotalReparacion,
    la suma del costo total es infinita por lo que nunca terminará
-}