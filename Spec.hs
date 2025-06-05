module Spec where
import PdePreludat
import Pista
import Pto_2
import Pto_3
import Pto_4
import Pto_5
import Pto_2_1
import Pto_2_2
import Pto_2_3
import Pto_2_4
import Pto_2_5
import Pto_2_6

import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
--2
  describe "Estado de salud del auto" $ do
    it "1) Un auto marca Peugeot" $ do
      buenEstadoAuto autoPe `shouldBe` False
    it "2) Un auto marca Lamborghini, con tiempo en pista de 99 segundos y desgaste de chasis 7" $ do
      buenEstadoAuto autoLa2 `shouldBe` True
    it "3) Un auto marca Fiat, con tiempo en pista de 99 segundos y desgaste de chasis 33" $ do
      buenEstadoAuto autofi3 `shouldBe` False
    it "4) Un auto marca Ferrari que tiene 130 segundos de tiempo en pista con desgaste de ruedas 50 y chasis 30" $ do
      buenEstadoAuto unAutoFe4 `shouldBe` True
    it "5) Un auto marca Ferrari que tiene 15 segundos de tiempo en pista con desgaste de ruedas 50 y chasis 45" $ do 
      buenEstadoAuto unAutoFe5 `shouldBe` False
    it "6) Un auto marca Ferrari que tiene 150 segundos de tiempo en pista con desgaste de ruedas 70 y chasis 30" $ do 
      buenEstadoAuto unAutoFe6 `shouldBe` False

  describe "Estado de salud del auto - No da más" $ do
    it "1) Un auto de marca Ferrari con desgaste de ruedas 20 y chasis 90" $ do
      autoParamas unAutoFe7 `shouldBe` False
    it "2)Un auto de marca Ferrari con desgaste de chasis 20" $ do
      autoParamas unAutoFe8 `shouldBe` True
    it "3)Un auto de marca Lamborghini con desgaste de chasis 20 y ruedas 90" $ do
      autoParamas autoLa3 `shouldBe` False
    it "4)Un auto de marca Lamborghini " $ do
      autoParamas autoLa `shouldBe` True

  describe "Si el auto es un chiche" $ do
    it "1)Un auto de marca Lamborghini" $ do
      esunChicle autoLa `shouldBe` True
    it "2)Un auto de marca Lamborghini con desgaste de ruedas 90 y chasis 20" $ do
      esunChicle autoLa3  `shouldBe` False
    it "3)Un auto de marca Ferrari con desgaste de ruedas 20 y chasis 90" $ do
      esunChicle unAutoFe9 `shouldBe` False
    it "4)Un auto de marca Ferrari" $ do
      esunChicle autoFe `shouldBe` True

  describe "Si es una joya" $ do
    it "1) Un auto de marca Peugeot" $ do
      esunaJoya autoPe `shouldBe` True
    it "2) Una auto de marca Ferrari" $ do
      esunaJoya autoFe `shouldBe` False

  describe "Capacidad supercalifragilisticaespialidosa" $ do
    it "Un auto de marca Ferrari" $ do
      capacidadSupercalifragilisticaespialidosa autoFe `shouldBe` 7 
  
  describe "Que tan riesgoso es un auto" $ do
    it "1)Un auto de marca Lamborghini" $ do
      riesgoAuto autoLa `shouldBe` 29.2  
    it "2)Un auto de marca Fiat" $ do
      riesgoAuto autoFi `shouldBe` 237.6
--3   
  describe "Reparar un auto" $ do
    it "1)Reparar un auto de marca Fiat - Chasis" $ do
       chasis(desgaste(reparoAuto autoFi)) `shouldBe` 4.95
    it ".Reparar un auto de marca Fiat - Ruedas" $ do
       ruedas(desgaste(reparoAuto autoFi)) `shouldBe` 0
    it "2)Reparar un auto de marca Ferrari - Chasis" $ do
       chasis(desgaste(reparoAuto autoFe)) `shouldBe` 0
    it ".Reparar un auto de marca Ferrari - Ruedas" $ do
       ruedas(desgaste(reparoAuto autoFe)) `shouldBe` 0

  describe "Aplicar una penalidad" $ do
    it "1)Penalidad de 20 segundos un auto de marca ferrari con tiempo 10 segundos en pista - Tiempo" $ do
          tiempodecarrera (penalidadAuto autoFe10 20) `shouldBe` 30
    it "2)Penalidad de 0 segundos a un auto de marca ferrari con tiempo 10 segundos en pista - Tiempo" $ do
          tiempodecarrera (penalidadAuto autoFe10 0) `shouldBe` 10

  describe "Ponerle nitro a un auto" $ do
    it "1)Ponerle nitro a un fiat - Velocidad máxima" $ do
        velocidadmax(ponerNitro autoFi)  `shouldBe` 52.8
    it "2)Ponerle nitro a un fiat con velocidad máxima 0 m/s - Velocidad máxima" $ do
        velocidadmax(ponerNitro autoFi1) `shouldBe` 0

  describe "Bautizar un auto" $ do
    it "1)Bautizar 'El diablo' a un auto marca Lamborghini - Apodos" $ do
        apodos (bautizoAuto autoLa "El diablo") `shouldBe` ["El diablo","Lambo", "La bestia"]
    it "2)Bautizar 'El diablo' a un auto marca Lamborghini sin apodos - Apodos" $ do
        apodos (bautizoAuto autoLa4 "El diablo") `shouldBe` ["El diablo"]

  describe "Llevar un auto a un desarmadero" $ do
    it "1)Llevar a un desarmadero a un auto marca Fiat para cambiar por marca 'Tesla' modelo 'X' - Marca" $ do
       marca (desarmaderoAuto autoFi "Tesla" "X") `shouldBe` "Tesla"
    it ".Llevar a un desarmadero a un auto marca Fiat para cambiar por marca 'Tesla' modelo 'X' - Modelo" $ do
       modelo (desarmaderoAuto autoFi "Tesla" "X") `shouldBe` "X"
    it ".Llevar a un desarmadero a un auto marca Fiat para cambiar por marca 'Tesla' modelo 'X' - Apodos" $ do
       apodos (desarmaderoAuto autoFi "Tesla" "X") `shouldBe` ["Nunca Taxi"]
--4
  describe "Transitar un tramo curva" $ do
    it "1)Transitar una curva peligrosa  con un auto marca Ferrari" $ do
         desgasteRuedasTramo autoFe curvaPeligrosa  `shouldBe` 15
    it "2)Transitar una curva peligrosa  con un auto marca Ferrari" $ do
          desgasteChasisTramo autoFe curvaPeligrosa  `shouldBe` 0
    it "3)Transitar una curva peligrosa  con un auto marca Peugeot" $ do
          tiempoTramo autoPe curvaPeligrosa `shouldBe` 15
    it "4)Transitar una curva tranca  con un auto marca Ferrari " $ do
        desgasteRuedasTramo autoFe curvaTranca `shouldBe` 15
    it "5)Transitar una curva tranca  con un auto marca Ferrari " $ do
          desgasteChasisTramo autoFe curvaTranca `shouldBe` 0
    it "6)Transitar una curva tranca  con un auto marca Peugeot" $ do
          tiempoTramo autoPe curvaTranca `shouldBe` 27.5

  describe "Transitar un tramo recto" $ do
    it "1)Transitar un tramo retro classic con un auto marca Ferrari " $ do
         desgasteChasisTramo autoFe tramoRectoClassic  `shouldBe` 7.15
    it "2)Transitar un tramo retro classic con un auto marca Ferrari " $ do
          tiempoTramo autoFe tramoRectoClassic `shouldBe` 11
    it "3)Transitar un tramito con un auto marca Ferrari " $ do
          desgasteChasisTramo autoFe tramito `shouldBe` 2.6
    it "4)Transitar un tramito con un auto marca Ferrari" $ do
          tiempoTramo autoFe tramito `shouldBe` 4

--5
  describe "Calcular el nivel de joyez" $ do
    it "1)Un auto Ferrari, un auto Peugeot con un tiempo de carrera de 49 y un auto Peugeot con un tiempo de 50" $ do
        niveldeJoyez grupo5a1 `shouldBe` 3

  describe "Grupo de autos para entendidos" $ do
    it "1)Un auto Ferrari con un tiempo de carrera de 201 y un otro igual pero con un tiempo de 200" $ do
        buenosyEntendidos grupo5b1 `shouldBe` False
    it "2)Un auto Ferrari con un tiempo de carrera de 200 y un otro Peugeot" $ do
        buenosyEntendidos grupo5b2 `shouldBe` False
    it "3)Un auto Ferrari con un tiempo de carrera de 200 y otro Lamborghini con tiempo también de 200" $ do
        buenosyEntendidos lista1 `shouldBe` False

-- SEGUNDA ENTREGA
--1
  describe "Agregar un auto a un equipo" $ do
    it "1)Agregar un Ferrari (costo 65000) a un equipo con presupuesto 70000" $ do
      presupuesto (agregaraEquipo equipo_1_a_1 autoFe)  `shouldBe` 5000
    it "2)Agregar un Fiat (costo 44000) a un equipo con presupuesto 50000 que ya tiene un Peugeot" $ do
      presupuesto (agregaraEquipo equipo_1_a_2 autoFi)  `shouldBe` 6000
    it "3)Agregar un Lamborghini (costo 73000) a un equipo con presupuesto 70000" $ do
        presupuesto (agregaraEquipo equipo_1_a_3 autoLa) `shouldBe` 70000

  describe "Realizar una reparación en equipo" $ do
    it "1)Reparar un equipo con un Ferrari (chasis 10), un Lamborghini (chasis 20) y presupuesto 20000" $ do
      presupuesto(repararEquipo equipo_1_b_1) `shouldBe` 7250
    it "2)Reparar un equipo con un Fiat (chasis 50) y presupuesto 10000" $ do
        presupuesto(repararEquipo equipo_1_b_2) `shouldBe` 10000

  describe "Optimizar autos en equipo" $ do
    it "1)Optimizar un equipo con un Ferrari, un Lamborghini y presupuesto 20000" $ do
      presupuesto(optimizarEquipo equipo_1_c_1) `shouldBe` 6200
    it "2)Optimizar un equipo con un Ferrari, un Lamborghini y presupuesto 10000" $ do
        presupuesto(optimizarEquipo equipo_1_c_2) `shouldBe` 3500

  describe "Ferrarizar" $ do
    it "1)Ferrarizar un equipo con un Peugeot, un Lamborghini y presupuesto 20000" $ do
      presupuesto(ferrarizarEquipo equipo_1_d_1 ) `shouldBe` 13000
    it "2)Ferrarizar un equipo con un Peugeot, un Lamborghini y presupuesto 4000" $ do
        presupuesto(ferrarizarEquipo equipo_1_d_2) `shouldBe` 500
    it "3)Ferrarizar un equipo con un Peugeot, una Ferrari, un Lamborghini y presupuesto 20000" $ do
      presupuesto(ferrarizarEquipo equipo_1_d_3) `shouldBe` 13000
-- 2
  describe "costo total de reparación" $ do
    it "1)Calcular el costo de reparación de un equipo con Ferrari (chasis 10), Lamborghini (chasis 20)" $ do
        costoTotalReparacion equipo_2_1 `shouldBe` 12750
    it "2)Calcular el costo de reparación de un equipo con Fiat (chasis 50), Peugeot (chasis 0)" $ do
        costoTotalReparacion equipo_2_2  `shouldBe` 21250

--7