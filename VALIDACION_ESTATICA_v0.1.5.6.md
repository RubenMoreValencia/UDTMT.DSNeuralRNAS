# Validacion estatica UDTMT.DSNeuralRNAS v0.1.5.6

Esta version corrige fronteras detectadas en pruebas con textos variados:

- No crear relaciones reflexivas artificiales como `control --falla--> control`.
- Representar verbos intransitivos como estados trazables, por ejemplo `control --falla--> estado_falla`.
- Reutilizar el sujeto en coordinacion verbal: `ajuste --reduce--> error` y `ajuste --mejora--> sistema`.
- Mantener origenes provisionales no registrados cuando funcionan como sujeto, por ejemplo `metacauce --organiza--> memoria`.
- Recuperar flujos afectivos y transformacionales contextuales como `sosiego --calma--> memoria` y `esperanza --transforma--> recuerdo`.

Validar en R con:

```r
devtools::document()
devtools::load_all()
devtools::test()
```
