#' Resumir una unidad UDT-MT DSNeuralRNAS
#'
#' Produce un resumen compacto de una unidad dinamica textual con memoria
#' trazable.
#'
#' @param unidad Objeto generado por `crear_udtmt_ds()`.
#'
#' @details
#' El resumen permite verificar si la unidad contiene texto resuelto,
#' pregunta interpretada, stocks, flujos, memoria y trayectoria. Es una
#' funcion de diagnostico y validacion antes de conectar la unidad con
#' componentes futuros como `ADTR.DSNeuralRNAS`.
#'
#' @return Una lista con conteos, intencion, respuesta y estado de la unidad.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   u <- crear_udtmt_ds("La calma llega despues de la lluvia", "Que cambia?", dic)
#'   resumir_udtmt_ds(u)
#' }
#'
#' @export
resumir_udtmt_ds <- function(unidad) {
  if (!inherits(unidad, "udtmt_ds") && !is.list(unidad)) {
    stop("unidad debe ser un objeto creado por crear_udtmt_ds().", call. = FALSE)
  }
  list(
    arquitectura = if (!is.null(unidad$arquitectura)) unidad$arquitectura else "UDT-MT DSNeuralRNAS",
    es_lstm = if (!is.null(unidad$es_lstm)) unidad$es_lstm else FALSE,
    n_tokens = if (!is.null(unidad$analisis_texto)) nrow(unidad$analisis_texto) else NA_integer_,
    n_stocks = if (!is.null(unidad$stocks)) nrow(unidad$stocks) else NA_integer_,
    n_flujos = if (!is.null(unidad$flujos)) nrow(unidad$flujos) else NA_integer_,
    intencion = if (!is.null(unidad$memoria$pregunta$intencion)) unidad$memoria$pregunta$intencion else NA_character_,
    n_evidencias = if (!is.null(unidad$memoria$evidencia$n)) unidad$memoria$evidencia$n else NA_integer_,
    n_contexto_limpio = if (!is.null(unidad$trayectoria$contexto_limpio)) length(unidad$trayectoria$contexto_limpio) else NA_integer_,
    n_senales_dinamicas = if (!is.null(unidad$trayectoria$senales_dinamicas)) length(unidad$trayectoria$senales_dinamicas) else NA_integer_,
    n_relaciones = if (!is.null(unidad$relaciones_semanticas)) nrow(unidad$relaciones_semanticas) else NA_integer_,
    lectura = if (!is.null(unidad$trayectoria$lectura)) unidad$trayectoria$lectura else NA_character_,
    respuesta = if (!is.null(unidad$respuesta$respuesta)) unidad$respuesta$respuesta else NA_character_
  )
}

#' Trazar una unidad UDT-MT DSNeuralRNAS
#'
#' Genera una ruta trazable desde el texto crudo hasta la salida preparada
#' para ADTR.
#'
#' @param unidad Objeto generado por `crear_udtmt_ds()`.
#'
#' @details
#' La trazabilidad de UDT-MT permite observar el paso desde resolucion
#' lexica, interpretacion de pregunta y evidencia hasta stocks, flujos,
#' memoria y trayectoria. Esta funcion prepara la lectura conceptual que
#' sera usada en futuras integraciones con `ADTR.DSNeuralRNAS`.
#'
#' @return Una lista con ruta conceptual, stocks, flujos, memoria y resumen.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   u <- crear_udtmt_ds("La calma llega despues de la lluvia", "Que cambia?", dic)
#'   trazar_udtmt_ds(u)
#' }
#'
#' @export
trazar_udtmt_ds <- function(unidad) {
  if (!inherits(unidad, "udtmt_ds") && !is.list(unidad)) {
    stop("unidad debe ser un objeto creado por crear_udtmt_ds().", call. = FALSE)
  }
  list(
    ruta_udtmt = unidad$ruta_udtmt,
    resumen = resumir_udtmt_ds(unidad),
    stocks = unidad$stocks,
    flujos = unidad$flujos,
    memoria = unidad$memoria,
    trayectoria = unidad$trayectoria,
    componentes_funcionales = clasificar_componentes_udtmt_ds(unidad),
    relaciones_semanticas = if (!is.null(unidad$relaciones_semanticas)) unidad$relaciones_semanticas else extraer_relaciones_textuales_udtmt_ds(unidad),
    salida_adtr_preparada = list(
      puntos = c("texto", "pregunta", "stocks", "flujos", "memoria", "trayectoria", "relaciones_semanticas", "respuesta"),
      tipo = "pre_adtr_udtmt",
      observacion = "Salida preparada para lectura transformacional-referencial posterior."
    )
  )
}
