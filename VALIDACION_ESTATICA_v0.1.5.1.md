# Validacion estatica UDTMT.DSNeuralRNAS v0.1.5.1

## Correccion aplicada

Se corrigio el error detectado en `extraer_relaciones_textuales_udtmt_ds()`, causado por usar `next` como nombre de variable. En R, `next` es una palabra reservada para saltar a la siguiente iteracion dentro de un bucle; por ello no debe utilizarse como objeto asignable.

Cambio aplicado:

```r
siguiente_idx <- contenido_idx[contenido_idx > i]
destino_i <- if (length(siguiente_idx)) head(siguiente_idx, 1) else NA_integer_
```

## Limpieza roxygen

Se retiro el enlace interno entre corchetes hacia `extraer_relaciones_textuales_udtmt_ds()` en el parametro de `clasificar_relaciones_semanticas_udtmt_ds()`, para evitar advertencias durante `devtools::document()`.

## Validacion esperada en RStudio

Ejecutar:

```r
devtools::document()
devtools::load_all()
devtools::test()
```

Resultado esperado:

```r
[ FAIL 0 | WARN 0 | SKIP 0 | PASS ... ]
```

## Prueba relacional esperada

```r
library(Lexico.DSNeuralRNAS)
library(PLN.DSNeuralRNAS)
library(UDTMT.DSNeuralRNAS)

dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()

texto <- "La perdida disminuye despues del ajuste. El sistema mejora con la revision. La evidencia muestra una trayectoria estable."
pregunta <- "Que trayectoria presenta el texto?"

u <- crear_udtmt_ds(texto, pregunta, diccionarios = dic)
resumir_relaciones_udtmt_ds(u)
responder_por_relaciones_udtmt_ds(u)
```
