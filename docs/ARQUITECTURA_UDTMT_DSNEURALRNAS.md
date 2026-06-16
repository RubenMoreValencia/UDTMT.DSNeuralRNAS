# Arquitectura UDT-MT DSNeuralRNAS

UDT-MT significa Unidad Dinamica Textual con Memoria Trazable.

La arquitectura parte de una idea central: el lenguaje no se procesa solo como secuencia opaca, sino como sistema textual con estados, flujos, evidencia y memoria interpretable.

## Principio

La unidad no es una LSTM. La memoria no se define como celda recurrente con compuertas, sino como stock informativo. Los flujos textuales modifican, conectan o activan esos stocks.

## Componentes

- Texto resuelto.
- Pregunta interpretada.
- Stocks informativos.
- Flujos textuales.
- Memoria textual.
- Memoria de pregunta.
- Memoria de evidencia.
- Trayectoria interpretativa.
- Salida preparada para ADTR.

## Uso esperado

Esta libreria debe usarse despues de `Lexico.DSNeuralRNAS` y `PLN.DSNeuralRNAS`.
