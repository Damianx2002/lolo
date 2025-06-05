module Pista where
import PdePreludat 
{-
Punto 1 
declaro los datas que se utilizarán y defino
los autos para el resto de puntos
-}

data Desgaste = UnDesgaste 
    {
    chasis :: Number,
    ruedas :: Number
    } deriving(Show)

data Auto = UnAuto
    {
    marca::String,
    modelo::String,
    desgaste::Desgaste,
    velocidadmax::Number,
    tiempodecarrera::Number, 
    apodos :: [String]
    } deriving (Show)

data Tramo = Curva
    {
    angulo :: Number,
    longitud :: Number,
    modificadores :: [String]
    }
    | 
    Recta
    {
    longitud :: Number,
    modificadores :: [String]
    }
    | 
    Zigzag
    {
    cambios :: Number,
    modificadores :: [String]
    }
    | 
    Rulo
    {
    diametro :: Number,
    modificadores :: [String]
    }     
    deriving (Show)


data Equipo = UnEquipo
    {
    nombre_eq :: String,
    autos :: [Auto],
    presupuesto :: Number
} deriving (Show)


data Pista = UnaPista
    {
    nombre :: String,
    pais :: String,
    precio :: Number,
    tramos :: [Tramo]
    } deriving (Show)

--Auto Ferrari, modelo F50, sin desgaste en su chasis y ruedas, con velocidad máxima de 65 m/s. Los apodos son “La nave”, “El fierro”, “Ferrucho”
autoFe = UnAuto "Ferrari" "F50" (UnDesgaste 0 0) 65 0 ["La nave" ,"El fierro", "Ferrucho"]

--Auto Lamborghini, modelo Diablo, con desgaste 7 de chasis y 4 de ruedas, con velocidad máxima de 73 m/s. Los apodos son “Lambo” y “La bestia”.
autoLa = UnAuto "Lamborghini" "Diablo" (UnDesgaste 7 4) 73 0 ["Lambo", "La bestia"]

--Auto Fiat, modelo 600, con desgaste 33 de chasis y 27 de ruedas, con velocidad máxima de 44 m/s. Los apodos son “La Bocha”, “La bolita” y “Fitito”
autoFi = UnAuto "Fiat" "600" (UnDesgaste 33 27) 44 0 ["La Bocha", "La bolita", "Fitito"]

--Auto Peugeot, modelo 504, no se encuentra desgaste de chasis ni de ruedas y tiene una velocidad máxima de 40 m/s. Tiene un solo apodo: “El rey del desierto”
autoPe = UnAuto "Peugeot" "504" (UnDesgaste 0 0) 40 0 ["El rey del desierto"]

-- Un auto marca Lamborghini, con tiempo en pista de 99 segundos y desgaste de chasis 7
autoLa2 = UnAuto "Lamborghini" "Diablo" (UnDesgaste 7 4) 73 99 ["Lambo", "La bestia"]
-- Un auto marca Fiat, con tiempo en pista de 99 segundos y desgaste de chasis 33
autofi3 = UnAuto "Fiat" "600" (UnDesgaste 33 27) 44 99 ["La Bocha" ,"La bolita" ,"Fitito"]
-- Un auto marca Ferrari que tiene 130 segundos de tiempo en pista con desgaste de chasis 30 y ruedas 50 
unAutoFe4 = UnAuto "Ferrari" "F50" (UnDesgaste 30 50) 65 130 ["La nave" ,"El fierro" ,"Ferrucho"]
-- Un auto marca Ferrari que tiene 15 segundos de tiempo en pista con desgaste de chasis 45 y ruedas 50  
unAutoFe5 = UnAuto "Ferrari" "F50" (UnDesgaste 45 50) 65 15 ["La nave" ,"El fierro" ,"Ferrucho"]
-- Un auto marca Ferrari que tiene 150 segundos de tiempo en pista con desgaste de  chasis 30 y ruedas 70
unAutoFe6 = UnAuto "Ferrari" "F50" (UnDesgaste 30 70) 65 150 ["La nave", "El fierro", "Ferrucho"]

-- 2-b
-- Un auto de marca Ferrari con desgaste de  chasis 90 y ruedas 20  
unAutoFe7  = UnAuto "Ferrari" "F50" (UnDesgaste 90 20) 65 150 ["La nave", "El fierro", "Ferrucho"]
-- Un auto de marca Ferrari con desgaste de chasis 20
unAutoFe8  = UnAuto "Ferrari" "F50" (UnDesgaste 20 20) 65 150 ["La nave", "El fierro", "Ferrucho"]
--Un auto de marca Lamborghini con desgaste de chasis 20 y ruedas 90
autoLa3 = UnAuto "Lamborghini" "Diablo" (UnDesgaste 20 90) 73 99 ["Lambo", "La bestia"]

-- 2-c
--Un auto de marca Ferrari con desgaste de chasis 90 y ruedas 20 
unAutoFe9  = UnAuto "Ferrari" "F50" (UnDesgaste 90 20) 65 150 ["La nave", "El fierro", "Ferrucho"]

--3-b
-- Aplicar penalidad de 20 segundos un auto de marca ferrari con tiempo 10 segundos en pista
autoFe10 = UnAuto "Ferrari" "F50" (UnDesgaste 0 0) 65 10 ["La nave" ,"El fierro", "Ferrucho"]
-- Aplicar penalidad de 0 segundos a un auto de marca ferrari con tiempo 10 segundos en pista
autoFi1 = UnAuto "Ferrari" "F50" (UnDesgaste 0 0) 0 10 ["La nave" ,"El fierro", "Ferrucho"]
-- Bautizar “El diablo” a un auto marca Lamborghini sin apodos
autoLa4 = UnAuto "Lamborghini" "Diablo" (UnDesgaste 7 4) 73 0 []

--5 a
--Un auto Ferrari, un auto Peugeot con un tiempo de carrera de 49
autoFe5a1 = UnAuto "Ferrari" "F50" (UnDesgaste 0 0) 65 49 ["La nave" ,"", ""]
--Un auto Peugeot con un tiempo de 50
autoPe5a2 = UnAuto "Peugeot" "504" (UnDesgaste 0 0) 40 50 ["El rey del desierto" ,"", ""]
grupo5a1= [autoFe5a1,autoPe5a2]


--5 b
--Un auto Ferrari con un tiempo de carrera de 201
autoFe5b1 = UnAuto "Ferrari" "F50" (UnDesgaste 0 0) 65 201 ["La nave" ,"", ""]
--Un auto Ferrari con un tiempo de carrera de 200
autoFe5b2 = UnAuto "Ferrari" "F50" (UnDesgaste 0 0) 65 200 ["La nave" ,"", ""]
-- Lamborghini con tiempo también de 200
autoLa5b3 = UnAuto "Lamborghini" "Diablo" (UnDesgaste 7 4) 73 200 ["Lambo", "", ""]

lista1 = [autoPe, unAutoFe4,unAutoFe5]
grupo5b1= [autoFe5b1,autoFe5b2]
grupo5b2= [autoFe5b2,autoPe]
grupo5b3 = [autoFe5b2,autoLa5b3]

--2 ENTREGA
--con Ferrari (chasis 10)
unAutoFe_2_1  = UnAuto "Ferrari" "F50" (UnDesgaste 10 0) 65 150 ["La nave", "El fierro", "Ferrucho"]
--con Ferrari (chasis 50)
unAutoFi_2_2  = UnAuto "Fiat" "600" (UnDesgaste 50 0) 44 0 ["La Bocha", "La bolita", "Fitito"]


equipoA = (UnEquipo "Roberto" [autoFe,autoLa] 100000)
equipoB = (UnEquipo "Martinez" [autoFe,autoFi] 120000)
equipoC = (UnEquipo "Aníbal" [autoFe,autoFi,autoPe] 120000)
equipoD = (UnEquipo "Escipión" [autoFe,autoFi,autoFe,autoFi] 11110)

--1 
--a
equipo_1_a_1 = (UnEquipo "Testeo" []  70000)
equipo_1_a_2 = (UnEquipo "Testeo" [autoPe]  50000 )
equipo_1_a_3 = (UnEquipo "Testeo" []  70000)

--b
equipo_1_b_1 = (UnEquipo "Testeo" [unAutoFe_2_1,autoLa3]  20000)
equipo_1_b_2 = (UnEquipo "Testeo" [unAutoFi_2_2]  10000)

--c
equipo_1_c_1 = (UnEquipo "Testeo" [autoFe,autoLa]  20000)
equipo_1_c_2 = (UnEquipo "Testeo" [autoFe,autoLa]  10000)

--d
equipo_1_d_1 = (UnEquipo "Testeo" [autoPe,autoLa]   20000)
equipo_1_d_2 = (UnEquipo "Testeo" [autoPe,autoLa]    4000)
equipo_1_d_3 = (UnEquipo "Testeo" [autoPe,autoFe,autoLa]   20000)

--2
equipo_2_1 = (UnEquipo "Testeo" [unAutoFe_2_1,autoLa3]  0)
equipo_2_2 = (UnEquipo "Testeo" [unAutoFi_2_2,autoPe]  0)

--6
autoPe_6_c = UnAuto "Peugeot" "504" (UnDesgaste 0 79) 40 0 ["El rey del desierto"]

grupo_6_c = [autoFe,autoPe_6_c]