# Validacion estatica UDTMT.DSNeuralRNAS v0.1.5

Version orientada a relaciones semanticas trazables.

## Cambios principales

- Nueva funcion `extraer_relaciones_textuales_udtmt_ds()`.
- Nueva funcion `clasificar_relaciones_semanticas_udtmt_ds()`.
- Nueva funcion `construir_matriz_relacional_udtmt_ds()`.
- Nueva funcion `resumir_relaciones_udtmt_ds()`.
- Nueva funcion `responder_por_relaciones_udtmt_ds()`.
- `crear_udtmt_ds()` ahora incorpora `relaciones_semanticas`.
- `trazar_udtmt_ds()` incluye relaciones y prepara salida ADTR con relaciones semanticas.

## Validacion esperada

```r
devtools::document()
devtools::load_all()
devtools::test()
```
