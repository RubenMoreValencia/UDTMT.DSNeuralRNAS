# UDTMT.DSNeuralRNAS 0.1.5.6.10

- Refuerza rescate local de moduladores posnominales posteriores para conservar relaciones como `transformacion --modula--> estable`.
- Protege filtros vectoriales contra `NA` en reglas relacionales finales.


# UDTMT.DSNeuralRNAS 0.1.5.6.9

* Correccion defensiva final en la regla de modulacion posnominal inmediata.
* Se protege la evaluacion de `es_flujo[mod_i]` usando `isTRUE()` para evitar fallos cuando el indicador contiene `NA`.
* Mantiene el refinamiento: `transformacion --modula--> estable` sin reintroducir relaciones incompletas.

# UDTMT.DSNeuralRNAS 0.1.5.6.8

* Refuerzo defensivo de moduladores posnominales finales.
* Recupera relaciones como `transformacion --modula--> estable` cuando el sustantivo fue destino de una relacion principal.
* Mantiene protecciones contra comparaciones con `NA` en reglas relacionales.

# UDTMT.DSNeuralRNAS 0.1.5.6.7

- Corrección defensiva en la regla de modificadores posnominales para evitar comparaciones lógicas con NA en `flujo_posterior`.
- Mantiene el refinamiento de `transformacion --modula--> estable` sin romper los ejemplos internos de consola.

# UDTMT.DSNeuralRNAS 0.1.5.6.6

- Recupera moduladores posnominales cuando el sustantivo ya fue destino de una relacion principal.
- Conserva casos como `transformacion --modula--> estable` sin reintroducir relaciones con origen `NA`.
- Mantiene la depuracion de relaciones redundantes hacia modificadores prenominales.

# UDTMT.DSNeuralRNAS 0.1.5.6.4

- Depura relaciones SVO redundantes cuando un modificador prenominal fue tomado como destino principal.
- Conserva relaciones nominales como `revision --activa--> estrategia` y deja el modificador como `estrategia --modula--> nueva`.
- Refuerza moduladores posnominales como `transformacion --modula--> estable`.

# UDTMT.DSNeuralRNAS 0.1.5.6.3

- Depura relaciones completas en textos integrados.
- Evita modulaciones sin origen, como NA --modula--> estable.
- Evita duplicados por coordinacion, conservando el sujeto correcto.
- Asocia modificadores prenominales con su sustantivo posterior, por ejemplo estrategia --modula--> nueva.
- Evita tratar sustantivos de tipo transformacion como flujos verbales.

# UDTMT.DSNeuralRNAS 0.1.5.6.2

- Corrige la deteccion de flujos para no confundir `stock_estado` con flujo relacional.
- Refuerza un fallback SVO contextual para conservar relaciones afectivas, transformacionales y emergentes.
- Mantiene la correccion tecnica de `integer(0)` en destinos relacionales.


# UDTMT.DSNeuralRNAS 0.1.5.6.1

- Corrige la construcción de relaciones cuando el destino calculado queda como `integer(0)`.
- Refuerza `agregar_relacion()` para manejar índices vacíos o no escalares de forma segura.
- Mantiene las mejoras de v0.1.5.6 sobre flujos afectivos, orígenes emergentes, coordinación verbal e intransitivos.

# UDTMT.DSNeuralRNAS 0.1.5.6

- Mejora la extraccion relacional con reglas para verbos intransitivos sin relaciones reflexivas artificiales.
- Agrega coordinacion verbal con `y` para conservar el sujeto compartido.
- Permite origenes provisionales no registrados cuando funcionan como sujeto semantico.
- Refuerza flujos contextuales como `calma`, `transforma`, `organiza`, `falla` y `persiste`.
- Agrega pruebas para relaciones intransitivas, coordinacion verbal, flujos afectivos y origenes emergentes.

# UDTMT.DSNeuralRNAS 0.1.5.5

- Amplia catalogo proactivo de flujos relacionales.
- Evita que verbos ya usados como relacion se reutilicen como origen artificial.
- Trata formas ambiguas como nominales si aparecen con determinante: una mejora, la calma.
- Respeta clausulas separadas por coma, punto y coma o dos puntos.
- Mejora origen semantico cuando un adjetivo precede al flujo: señal intermedia activa -> señal activa frontera.

