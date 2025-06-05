module Pto_4 where
import Pista 
import PdePreludat 
import Library 
import Pto_2
import Pto_3
--4
tiempoTramo :: Auto -> Tramo -> Number
tiempoTramo auto (Curva _ longitud _) = longitud / (velocidadmax auto / 2)
tiempoTramo auto (Recta longitud _) = longitud / velocidadmax auto
tiempoTramo _ (Zigzag cambios _) = cambios * 3
tiempoTramo auto (Rulo diametro _) = 5 * diametro / velocidadmax auto

desgasteChasisTramo :: Auto -> Tramo -> Number
desgasteChasisTramo auto (Recta longitud _) = longitud / 100
desgasteChasisTramo auto (Zigzag _ _) = 5
desgasteChasisTramo auto _ = 0

desgasteRuedasTramo :: Auto -> Tramo -> Number
desgasteRuedasTramo _ (Curva angulo longitud _) = 3 * longitud / angulo
desgasteRuedasTramo auto (Zigzag cambios _) = velocidadmax auto * cambios / 10
desgasteRuedasTramo _ (Recta _ _) = 0
desgasteRuedasTramo _ (Rulo diametro _) = diametro * 1.5

desgasteTramo :: Auto -> Tramo -> Auto
desgasteTramo auto tramo
    |modificadores tramo==[] =auto {desgaste = UnDesgaste {chasis= desgasteChasisTramo auto tramo, ruedas=desgasteRuedasTramo auto tramo}, velocidadmax= velocidadmax auto, tiempodecarrera= tiempodecarrera auto + tiempoTramo auto tramo}
    |head(modificadores tramo) =="Boxes" && buenEstadoAuto auto = auto {desgaste = UnDesgaste {chasis= desgasteChasisTramo auto tramo, ruedas=desgasteRuedasTramo auto tramo}, tiempodecarrera= tiempodecarrera auto + tiempoTramo auto tramo}
    |head(modificadores tramo) =="Boxes" && (buenEstadoAuto auto == False) = auto {desgaste = desgaste (reparoAuto auto), tiempodecarrera= tiempodecarrera auto + tiempoTramo auto tramo + 10}
    |head(modificadores tramo) =="Mojado" = auto {desgaste = UnDesgaste {chasis= desgasteChasisTramo auto tramo, ruedas=desgasteRuedasTramo auto tramo} , tiempodecarrera= tiempodecarrera auto + 1.5*(tiempoTramo auto tramo)}
    |head(modificadores tramo) =="Ripio" = auto {desgaste = UnDesgaste {chasis= (desgasteChasisTramo auto tramo)*2 ,ruedas=(desgasteRuedasTramo auto tramo)*2}, tiempodecarrera=tiempodecarrera auto + (tiempoTramo auto tramo)*2}
--    |head(modificadores tramo) =="Obstrucciones" = auto {desgaste = UnDesgaste {chasis= desgasteChasisTramo auto tramo, ruedas=desgasteRuedasTramo auto tramo}, tiempodecarrera= tiempoTramo auto tramo}
    |head(modificadores tramo) =="Turbo" = auto {desgaste= UnDesgaste{chasis= desgasteChasisTramo autoconNitro tramo, ruedas=desgasteRuedasTramo autoconNitro tramo} ,velocidadmax= velocidadmax auto , tiempodecarrera= tiempodecarrera auto + tiempoTramo autoconNitro tramo}
        where
        autoconNitro = auto{velocidadmax=(velocidadmax auto)*2}

--4a
curvaPeligrosa = Curva 60 300 []
curvaTranca = Curva 110 550 []
--4b
tramoRectoClassic = Recta 715 []
tramito = Recta 260 ["Mojado"]
--4c
zigZagLoco = Zigzag 5 []
casiCurva = Zigzag 1 []
--4d
ruloClasico = Rulo 13 []
deseoDeMuerte = Rulo 26 []

