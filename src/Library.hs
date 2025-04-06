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

-- 2. Temperaturas
celsiusAFahrenheit :: Number -> Number
celsiusAFahrenheit celsius = (celsius * 1.8) + 32

fahrenheitACelsius :: Number -> Number
fahrenheitACelsius fahrenheit = (fahrenheit - 32) / 1.8

haceFrioCelsius :: Number -> Bool
haceFrioCelsius grados = grados <= 8

haceFrioFahrenheit :: Number -> Bool
haceFrioFahrenheit grados = grados <= (celsiusAFahrenheit 8)
{-
-- 2.5 Bonus OPCIONAL
perimetroCirculo :: Number -> Number
perimetroCirculo radio = implementame

perimetroCuadrado :: Number -> Number
perimetroCuadrado lado = implementame

superficieCuadrado :: Number -> Number
superficieCuadrado lado = implementame

superficieCubo :: Number -> Number
superficieCubo lado = implementame

superficieCilindro :: Number -> Number -> Number
superficieCilindro radio altura = implementame
-}