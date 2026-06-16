# Validacion estatica UDTMT.DSNeuralRNAS v0.1.5.3

## Proposito

Esta version mejora la capa relacional semantica de UDT-MT incorporando una regla prudente de limite de oracion.

## Mejora funcional

La extraccion relacional ahora puede recibir el texto original y derivar un identificador de oracion para cada token. Con ello, las relaciones se construyen solo entre tokens de contenido pertenecientes a la misma oracion, salvo futuras reglas discursivas explicitas.

## Resultado esperado

Para el texto:

```text
La perdida disminuye despues del ajuste.
El sistema mejora con la revision.
La evidencia muestra una trayectoria estable.
```

La lectura relacional esperada es:

```text
perdida --disminuye--> ajuste
sistema --mejora--> revision
evidencia --muestra--> trayectoria
trayectoria --modula--> estable
```

Se evita la relacion espuria:

```text
revision --evidencia--> muestra
```

## Validacion recomendada

```r
devtools::document()
devtools::load_all()
devtools::test()
devtools::install(upgrade = FALSE, dependencies = FALSE)
```
