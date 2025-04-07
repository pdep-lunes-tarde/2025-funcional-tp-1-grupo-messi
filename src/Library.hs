module Library where
import PdePreludat

-- 1. Numeros
siguiente :: Number -> Number
siguiente nro = nro + 1

esPositivo :: Number -> Bool
esPositivo nro = nro > 0

inversa :: Number -> Number 
inversa nro = 1 / nro

-- 1.5 Bonus
perimetroCirculo :: Number -> Number
perimetroCirculo radio = 2 * radio * pi

perimetroCuadrado :: Number -> Number
perimetroCuadrado lado = lado * 4

superficieCuadrado :: Number -> Number
superficieCuadrado lado = lado * lado

superficieCubo :: Number -> Number
superficieCubo lado = 6 *lado * lado

superficieCilindro :: Number -> Number -> Number
superficieCilindro radio altura = pi * radio * radio * altura 

-- 2. Temperaturas
celsiusAFahrenheit :: Number -> Number
celsiusAFahrenheit celsius = (celsius * 1.8) + 32

fahrenheitACelsius :: Number -> Number
fahrenheitACelsius fahrenheit = (fahrenheit - 32) / 1.8

haceFrioCelsius :: Number -> Bool
haceFrioCelsius grados = grados <= 8

haceFrioFahrenheit :: Number -> Bool
haceFrioFahrenheit grados = grados <= (celsiusAFahrenheit 8)