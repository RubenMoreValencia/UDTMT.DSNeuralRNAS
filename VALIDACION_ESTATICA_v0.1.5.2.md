# Validacion estatica UDTMT.DSNeuralRNAS v0.1.5.2

Esta version corrige la seleccion de destino semantico en la matriz relacional.

Criterios aplicados:

- No usar palabras reservadas de R como nombres de objetos.
- No tomar conectores, articulos o preposiciones como destino semantico.
- Saltar conectores hasta encontrar un token de contenido.
- Conservar conectores como evidencia relacional cuando aportan temporalidad, causa o contraste.
- Evitar que conectores sean tratados como moduladores.

Validacion recomendada en RStudio:

```r
devtools::document()
devtools::load_all()
devtools::test()
devtools::install(upgrade = FALSE, dependencies = FALSE)
```
