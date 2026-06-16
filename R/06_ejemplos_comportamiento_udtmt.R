#' Ejecutar ejemplos de comportamiento funcional UDT-MT
#'
#' Ejecuta tres casos breves para observar el comportamiento funcional de
#' `UDTMT.DSNeuralRNAS` frente a preguntas de memoria, naturaleza y
#' transformacion. La funcion devuelve un resumen compacto por caso.
#'
#' @param dic Diccionarios PLN. Si es `NULL`, se cargan mediante
#'   `PLN.DSNeuralRNAS::cargar_diccionarios_pln()`.
#'
#' @details
#' Esta funcion no entrena modelos. Usa la salida de `PLN.DSNeuralRNAS` y la
#' organiza como unidad dinamica textual con memoria trazable. Su proposito es
#' observar si el flujo texto-pregunta-evidencia genera stocks informativos,
#' flujos textuales y respuesta trazable coherente con la intencion de la
#' pregunta.
#'
#' Los tres casos incluidos son memoria, naturaleza y transformacion. Estos
#' permiten observar el comportamiento funcional minimo antes de integrar
#' capas posteriores con `ADTR.DSNeuralRNAS`.
#'
#' @return Un `data.frame` con caso, intencion, numero de tokens, stocks,
#'   flujos, evidencias y respuesta trazable.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   ejecutar_ejemplos_udtmt_ds()
#' }
#'
#' @export
ejecutar_ejemplos_udtmt_ds <- function(dic = NULL) {
  if (!requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
    stop("Se requiere PLN.DSNeuralRNAS para ejecutar ejemplos UDT-MT.", call. = FALSE)
  }
  if (is.null(dic)) dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()

  casos <- list(
    list(
      caso = "memoria_textual",
      texto = "El sosiego brota en la pradera y la memoria textual se activa",
      pregunta = "Que palabras funcionan como memoria?"
    ),
    list(
      caso = "naturaleza_contexto",
      texto = "La lluvia toca el rio y el viento mueve la pradera",
      pregunta = "Que elementos de naturaleza aparecen?"
    ),
    list(
      caso = "transformacion_textual",
      texto = "La perdida disminuye despues del ajuste y el sistema mejora",
      pregunta = "Que cambia en el texto?"
    )
  )

  resultados <- lapply(casos, function(caso) {
    unidad <- crear_udtmt_ds(
      texto = caso$texto,
      pregunta = caso$pregunta,
      dic = dic
    )
    resumen <- resumir_udtmt_ds(unidad)
    componentes <- clasificar_componentes_udtmt_ds(unidad)
    data.frame(
      caso = caso$caso,
      intencion = resumen$intencion,
      n_tokens = resumen$n_tokens,
      n_stocks = resumen$n_stocks,
      n_flujos = resumen$n_flujos,
      n_evidencias = resumen$n_evidencias,
      contexto_limpio = paste(componentes$contexto_limpio, collapse = ", "),
      senales_dinamicas = paste(componentes$senales_dinamicas, collapse = ", "),
      flujos_transformacion = paste(componentes$flujos_transformacion, collapse = ", "),
      respuesta = resumen$respuesta,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, resultados)
}
