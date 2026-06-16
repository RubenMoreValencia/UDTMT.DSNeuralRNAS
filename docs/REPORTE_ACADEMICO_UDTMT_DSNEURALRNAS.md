# Reporte academico UDT-MT DSNeuralRNAS

Este documento describe el sentido de las funciones de reporte incorporadas en `UDTMT.DSNeuralRNAS v0.1.1`.

## Proposito

El reporte academico permite documentar una unidad UDT-MT como estructura dinamica textual. Su objetivo no es entrenar un modelo, sino organizar de manera interpretable los resultados producidos por la unidad:

```text
texto resuelto
-> pregunta interpretada
-> stocks informativos
-> flujos textuales
-> memoria textual
-> memoria de pregunta
-> memoria de evidencia
-> trayectoria interpretativa
-> respuesta trazable
-> salida para ADTR
```

## Funciones

- `interpretar_memoria_udtmt_ds()` genera una lectura compacta de la memoria.
- `generar_reporte_udtmt_ds()` produce un reporte Markdown con resumen, trayectoria, respuesta, ruta y tablas compactas.

## Criterio conceptual

UDT-MT no es una LSTM. Su memoria no se basa en compuertas recurrentes, sino en stocks informativos, flujos, evidencia y trazabilidad.

## Validacion esperada

Antes de cerrar la version se debe ejecutar:

```r
devtools::document()
devtools::load_all()
devtools::test()
```
