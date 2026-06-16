# UDTMT.DSNeuralRNAS

**Unidad Dinamica Textual con Memoria Trazable para DSNeuralRNAS**

`UDTMT.DSNeuralRNAS` es la libreria del ecosistema DSNeuralRNAS que implementa la arquitectura textual propia **UDT-MT**, orientada a construir memoria trazable a partir de textos, preguntas, evidencias, stocks informativos, flujos textuales y trayectorias interpretativas.

Esta libreria **no implementa una LSTM**. Aunque puede responder funcionalmente a necesidades de memoria secuencial, su arquitectura no usa compuertas recurrentes. La memoria se organiza mediante stocks informativos, flujos de entrada/salida, evidencia textual y trazabilidad dinamica.

## Relacion con el ecosistema

```text
Lexico.DSNeuralRNAS
  -> resolucion lexica, variantes, emergencia semantica
PLN.DSNeuralRNAS
  -> texto, pregunta, evidencia, respuesta trazable
UDTMT.DSNeuralRNAS
  -> stocks, flujos, memoria, trayectoria, reporte y salida para ADTR
```

## Flujo conceptual

```text
texto + pregunta
-> PLN.DSNeuralRNAS
-> matriz lexico-semantica
-> intencion de pregunta
-> evidencia textual
-> stocks informativos
-> flujos textuales
-> memoria textual/pregunta/evidencia
-> trayectoria UDT-MT
-> reporte academico
-> salida preparada para ADTR
```

## Funciones principales

```r
catalogo_udtmt_ds()
verificar_udtmt_ds()
crear_udtmt_ds()
crear_stock_textual_ds()
crear_flujo_textual_ds()
actualizar_stock_textual_ds()
registrar_memoria_textual_ds()
registrar_memoria_pregunta_ds()
registrar_memoria_evidencia_ds()
construir_trayectoria_udtmt_ds()
resumir_udtmt_ds()
trazar_udtmt_ds()
interpretar_memoria_udtmt_ds()
generar_reporte_udtmt_ds()
```

## Ejemplo minimo

```r
library(Lexico.DSNeuralRNAS)
library(PLN.DSNeuralRNAS)
library(UDTMT.DSNeuralRNAS)

dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()

texto <- "La calma llega despues de las lluvias y la perdida disminuye"
pregunta <- "Que cambia en el texto?"

u <- crear_udtmt_ds(texto, pregunta, dic)

resumir_udtmt_ds(u)
trazar_udtmt_ds(u)
interpretar_memoria_udtmt_ds(u)
```

## Reporte academico

La version `0.1.1` agrega una funcion para documentar la memoria textual generada:

```r
reporte <- generar_reporte_udtmt_ds(u)
cat(paste(reporte, collapse = "\n"))

generar_reporte_udtmt_ds(
  unidad = u,
  archivo = "outputs/reporte_udtmt.md"
)
```

El reporte organiza: arquitectura, conteos dinamicos, lectura de memoria, trayectoria interpretativa, respuesta trazable, ruta UDT-MT y tablas compactas de stocks y flujos.

## Validacion

Antes de cerrar una version se debe ejecutar:

```r
devtools::document()
devtools::load_all()
devtools::test()
```

La version `0.1.0` fue validada en RStudio con:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 29 ]
```

La version `0.1.1` debe validarse de nuevo tras documentar las nuevas funciones de reporte.


## Ejemplos de comportamiento funcional

Desde la version 0.1.2 se incluye una funcion para observar tres comportamientos basicos de la UDT-MT:

```r
dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
ejecutar_ejemplos_udtmt_ds(dic)
```

Tambien se acepta el alias `diccionarios` en `crear_udtmt_ds()`:

```r
u <- crear_udtmt_ds(
  texto = "El sosiego brota en la pradera y la memoria textual se activa",
  pregunta = "Que palabras funcionan como memoria?",
  diccionarios = dic
)
```


## Mejora v0.1.3: componentes funcionales refinados

La version 0.1.3 separa la trayectoria en contexto limpio, senales dinamicas, operadores textuales y flujos de transformacion. Los ejemplos funcionales se ejecutan desde consola con `ejecutar_ejemplos_udtmt_ds()` y no requieren archivos externos de ejemplo.

```r
dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
ejecutar_ejemplos_udtmt_ds(dic)
```


## Version 0.1.4

Esta version agrega respuesta trazable refinada, de modo que la salida textual use contexto limpio, senales dinamicas y flujos de transformacion calculados por UDT-MT. Los ejemplos se ejecutan por consola con `ejecutar_ejemplos_udtmt_ds()`.


## Nota v0.1.5.6

Esta version mejora la semantica relacional con coordinacion verbal, verbos intransitivos, origenes emergentes y control de relaciones reflexivas artificiales.


## Nota v0.1.5.6.9

Correccion defensiva de modulacion posnominal inmediata para evitar condiciones logicas con NA en ejemplos internos.
