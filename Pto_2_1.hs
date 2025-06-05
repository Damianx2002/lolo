module Pto_2_1 where
import Pista 
import Pto_3 --nitroAuto reparoAuto
import Pto_2_7 -- primerAuto resto_de_Autos
import PdePreludat 

costoAuto :: Auto -> Number
costoAuto auto =  (velocidadmax auto) * 1000

-- --1a
agregaraEquipo :: Equipo -> Auto -> Equipo
agregaraEquipo equipo auto
     | (presupuesto equipo)  >  (costoAuto auto) = 
         equipo{autos  =auto :(autos equipo),presupuesto = (presupuesto equipo) - (costoAuto auto)}  
     | otherwise = equipo


modificarEquipoMientrasAlcancePresupuesto :: Equipo -> [Auto] ->  (Auto -> Auto) -> (Auto -> Number) -> Equipo
modificarEquipoMientrasAlcancePresupuesto equipo [] _ _ = equipo
modificarEquipoMientrasAlcancePresupuesto equipo autos_1 aplicarTarea aplicarCosto
    | (presupuesto equipo) >= (aplicarCosto (primerAuto autos_1)) = modificarEquipoMientrasAlcancePresupuesto equipo{autos = (aplicarTarea((primerAuto autos_1)):(autos equipo)),presupuesto =(presupuesto equipo - aplicarCosto (primerAuto autos_1))}  (resto_de_Autos autos_1)  aplicarTarea aplicarCosto
    | otherwise = equipo
-- Modifica los coches del equipo hasta que no de más el presupuesto

-- --1b
costoRepacion :: Auto -> Number
costoRepacion auto = chasis (desgaste auto) * 500  * 0.85

repararEquipo :: Equipo -> Equipo
repararEquipo equipo = modificarEquipoMientrasAlcancePresupuesto equipo (autos equipo) reparoAuto costoRepacion

 --1c
costoNitro :: Auto -> Number
costoNitro auto = velocidadmax auto * 100

optimizarEquipo :: Equipo -> Equipo
optimizarEquipo equipo = modificarEquipoMientrasAlcancePresupuesto equipo (autos equipo) ponerNitro costoNitro
 

--1d
costoFerrarizar :: Auto -> Number
costoFerrarizar auto
     | marca auto == "Ferrari" = 0
     | otherwise = 3500

ferrarizarAuto :: Auto -> Auto 
ferrarizarAuto auto
     | marca auto == "Ferrari" = auto
     | otherwise = auto {marca = "Ferrari", modelo = "F50", apodos = ["Nunca Taxi"]}

ferrarizarEquipo :: Equipo -> Equipo
ferrarizarEquipo equipo = modificarEquipoMientrasAlcancePresupuesto equipo (autos equipo) ferrarizarAuto costoFerrarizar
--Ferrariza todos los autos del equipo, si es que el presupuesto alcanza. Devuelve el equipo con los autos modificados y el presupuesto actualizado.



-- -- ferrarizarAutos :: [Auto] -> Number -> [Auto]
-- -- ferrarizarAutos [] _ = []
-- -- ferrarizarAutos (auto:resto) presupuesto
-- --     | presupuesto >= costoFerrarizar auto = ferrarizarAuto auto : ferrarizarAutos resto (presupuesto - costoFerrarizar auto)
-- --     | otherwise = auto : resto

-- -- nuevoPresupuestoFerrari :: [Auto] -> Number -> Number
-- -- nuevoPresupuestoFerrari [] presupuesto = presupuesto
-- -- nuevoPresupuestoFerrari (auto:resto) presupuesto
-- --     | presupuesto >= costoFerrarizar auto = nuevoPresupuestoFerrari resto (presupuesto - costoFerrarizar auto)
-- --     | otherwise = presupuesto
-- --Se podria agrupar en una solo funcion (nuevoPresupuestoGeneral)


-- -- optimizarAutos :: [Auto] -> Number -> [Auto]
-- -- optimizarAutos [] _ = []
-- -- optimizarAutos (auto:resto) presupuesto
-- --     | presupuesto >= costoNitro auto = ponerNitro auto : optimizarAutos resto (presupuesto - costoNitro auto)
-- --     | otherwise = auto : resto
-- --Se podria agrupar en una solo funcion ()
-- -- Recorre la lista de autos y pone nitro a cada uno mientras haya presupuesto suficiente.

-- -- nuevoPresupuesto :: [Auto] -> Number -> Number
-- -- nuevoPresupuesto [] presupuesto = presupuesto
-- -- nuevoPresupuesto (auto:resto) presupuesto
-- --     | presupuesto >= costoNitro auto = nuevoPresupuesto resto (presupuesto - costoNitro auto)
-- --     | otherwise = presupuesto
-- -- Calcula el presupuesto restante después de optimizar los autos.
-- --Se podria agrupar en una solo funcion (nuevoPresupuestoGeneral)



-- Esta función toma un equipo, una acción a aplicar a cada auto y una función de costo, y devuelve un nuevo equipo con los autos modificados y el presupuesto actualizado.
-- modificarEquipoMientrasAlcancePresupuesto :: Equipo -> (Auto -> Auto) -> (Auto -> Number) -> Equipo
-- modificarEquipoMientrasAlcancePresupuesto equipo aplicarTarea aplicarCosto = 
--       modificacionAutoYNuevoPresupuesto (autos equipo) (presupuesto equipo) aplicarTarea aplicarCosto (nombre_eq equipo) -- Llama a la función auxiliar con la lista de autos, el presupuesto y el nombre del equipo
  

-- Función auxiliar recursiva que procesa la lista de autos y el presupuesto
-- modificacionAutoYNuevoPresupuesto :: [Auto] -> Number -> (Auto -> Auto) -> (Auto -> Number) -> String -> Equipo
-- modificacionAutoYNuevoPresupuesto [] presupuesto _ _ nombre = UnEquipo nombre [] presupuesto -- Si no hay más autos, devuelve un equipo vacío con el presupuesto actual
-- modificacionAutoYNuevoPresupuesto (auto:resto) presupuesto aplicarTarea aplicarCosto nombre -- Caso recursivo: hay al menos un auto en la lista
--     -- Si el presupuesto alcanza para modificar este auto:
--     | presupuesto >= costo auto = -- Aplica la acción al auto, descuenta el costo del presupuesto, y sigue procesando el resto de los autos con el presupuesto actualizado.
--              UnEquipo nombre 
--             (aplicarTarea auto : autos (modificacionAutoYNuevoPresupuesto resto (presupuesto - costoAuto(auto)) aplicarTarea aplicarCosto nombre))
--             (presupuesto (modificacionAutoYNuevoPresupuesto resto (presupuesto - costoAuto(auto)) aplicarTarea aplicarCosto nombre))
--     -- Si el presupuesto NO alcanza para modificar este auto:
--     | otherwise =  -- Deja el auto como está, no descuenta nada, y sigue con el resto.
--         UnEquipo nombre 
--             (auto : autos (modificacionAutoYNuevoPresupuesto resto presupuesto aplicarTarea aplicarCosto nombre))
--             (presupuesto (modificacionAutoYNuevoPresupuesto resto presupuesto aplicarTarea aplicarCosto nombre))


 --nuevoPresupuestoGeneral :: [Auto] -> Number -> (Auto -> Number) -> Number
 --nuevoPresupuestoGeneral [] presupuesto _ = presupuesto
 --nuevoPresupuestoGeneral (auto:resto) presupuesto costoAuto
 --    | presupuesto >= costoAuto auto = nuevoPresupuestoGeneral resto (presupuesto - costoAuto(auto)) costoAuto 
 --    | otherwise = presupuesto
 -- Calcula el presupuesto restante después de aplicar una funcion.


 --modificarAutosMientrasAlcancePresupuesto :: Equipo -> (Auto -> Auto) -> (Auto -> Number) -> Equipo
 --modificarAutosMientrasAlcancePresupuesto [] _ _ _= []
 --modificarAutosMientrasAlcancePresupuesto equipo aplicarTarea aplicarCosto
 --    | presupuesto equipo >= funcionCosto auto =  aplicarTarea(auto) : modificarAutosMientrasAlcancePresupuesto resto (nuevoPresupuestoGeneral) aplicarTarea aplicarCosto
 --    | otherwise = auto : resto