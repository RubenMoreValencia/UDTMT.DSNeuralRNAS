# Plan ejecutable de integracion semantica

Este directorio contiene insumos materializados para integrar candidatos semanticos de forma controlada.

## Archivos generados

- `lexico_propuesto.csv`: terminos candidatos o debilmente conectados.
- `relaciones_validadas_localmente.csv`: relaciones candidatas con validacion local o revision.
- `reglas_validadas_localmente.csv`: reglas candidatas asociadas a operadores detectados.
- `tests_sugeridos.csv`: pruebas positivas y negativas sugeridas.
- `script_revision_integracion.R`: script para cargar y revisar los insumos.
- `tests/testthat/test-expansion-semantica-sugerida.R`: plantilla de pruebas para adaptar al paquete destino.

## Criterio de integracion

La integracion no se considera consolidada hasta ejecutar pruebas regresivas en los paquetes afectados.
Los candidatos deben mantenerse como revisables hasta pasar `devtools::document()`, `devtools::load_all()` y `devtools::test()` en las librerias destino.
