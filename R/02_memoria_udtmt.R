#' Registrar memoria textual UDT-MT
#'
#' Construye una memoria textual resumida a partir del analisis, stocks,
#' flujos y trayectoria interpretativa.
#'
#' @param analisis_texto Matriz lexico-semantica del texto.
#' @param stocks Tabla de stocks informativos.
#' @param flujos Tabla de flujos textuales.
#' @param trayectoria Lista de trayectoria interpretativa generada por PLN.
#'
#' @details
#' La memoria textual conserva el estado del texto como informacion
#' acumulada. Incluye numero de tokens, tokens registrados, stocks,
#' flujos, evidencia y una lectura interpretativa si existe.
#'
#' @return Una lista con resumen y componentes de memoria textual.
#'
#' @examples
#' a <- data.frame(token = c("calma", "llega"), registrado = c(TRUE, TRUE))
#' s <- data.frame(token = "calma")
#' f <- data.frame(token = "llega")
#' registrar_memoria_textual_ds(a, s, f)
#'
#' @export
registrar_memoria_textual_ds <- function(analisis_texto, stocks, flujos, trayectoria = NULL) {
  if (!is.data.frame(analisis_texto)) stop("analisis_texto debe ser un data.frame.", call. = FALSE)
  evidencia <- if ("evidencia" %in% names(analisis_texto)) analisis_texto$token[isTRUE(length(analisis_texto$evidencia) > 0) & analisis_texto$evidencia %in% TRUE] else character(0)
  list(
    n_tokens = nrow(analisis_texto),
    n_registrados = if ("registrado" %in% names(analisis_texto)) sum(analisis_texto$registrado %in% TRUE, na.rm = TRUE) else NA_integer_,
    n_stocks = nrow(stocks),
    n_flujos = nrow(flujos),
    evidencia = as.character(evidencia),
    lectura = if (!is.null(trayectoria) && !is.null(trayectoria$lectura)) trayectoria$lectura else NA_character_,
    tipo_memoria = "memoria_textual_trazable"
  )
}

#' Registrar memoria de pregunta UDT-MT
#'
#' Conserva la intencion, modulo y tipo de respuesta asociados con la
#' pregunta procesada por `PLN.DSNeuralRNAS`.
#'
#' @param analisis_pregunta `data.frame` generado por `analizar_pregunta_pln()`.
#'
#' @details
#' La memoria de pregunta es necesaria para que la unidad no solo recuerde
#' el texto, sino tambien el tipo de consulta que guia la interpretacion.
#'
#' @return Una lista con la pregunta normalizada, intencion, modulo y dificultad.
#'
#' @examples
#' p <- data.frame(pregunta_normalizada = "que cambia", intencion = "transformacion",
#'                 modulo_activado = "modulo_dinamico_textual", nivel_dificultad = "medio")
#' registrar_memoria_pregunta_ds(p)
#'
#' @export
registrar_memoria_pregunta_ds <- function(analisis_pregunta) {
  if (!is.data.frame(analisis_pregunta) || nrow(analisis_pregunta) == 0) {
    return(list(
      pregunta_normalizada = NA_character_, intencion = "sin_intencion",
      modulo_activado = "sin_modulo", tipo_respuesta = NA_character_,
      nivel_dificultad = NA_character_, tipo_memoria = "memoria_pregunta_vacia"
    ))
  }
  fila <- analisis_pregunta[1, , drop = FALSE]
  list(
    pregunta_normalizada = if ("pregunta_normalizada" %in% names(fila)) as.character(fila$pregunta_normalizada[1]) else rownames(fila)[1],
    intencion = if ("intencion" %in% names(fila)) as.character(fila$intencion[1]) else NA_character_,
    modulo_activado = if ("modulo_activado" %in% names(fila)) as.character(fila$modulo_activado[1]) else NA_character_,
    tipo_respuesta = if ("tipo_respuesta" %in% names(fila)) as.character(fila$tipo_respuesta[1]) else NA_character_,
    nivel_dificultad = if ("nivel_dificultad" %in% names(fila)) as.character(fila$nivel_dificultad[1]) else NA_character_,
    tipo_memoria = "memoria_pregunta_trazable"
  )
}

#' Registrar memoria de evidencia UDT-MT
#'
#' Conserva los tokens y roles que sustentan una respuesta trazable.
#'
#' @param evidencia `data.frame` con evidencia textual extraida por PLN.
#'
#' @details
#' La memoria de evidencia evita que la respuesta quede separada del texto.
#' Cada salida puede rastrearse hacia tokens, lemas y roles funcionales.
#'
#' @return Una lista con tokens, lemas, roles y conteos de evidencia.
#'
#' @examples
#' e <- data.frame(token = c("llega", "disminuye"), lema = c("llegar", "disminuir"),
#'                 rol_funcional = c("flujo_accion", "flujo_transformacion"))
#' registrar_memoria_evidencia_ds(e)
#'
#' @export
registrar_memoria_evidencia_ds <- function(evidencia) {
  if (!is.data.frame(evidencia) || nrow(evidencia) == 0) {
    return(list(n = 0, tokens = character(0), lemas = character(0), roles = character(0), tipo_memoria = "memoria_evidencia_vacia"))
  }
  list(
    n = nrow(evidencia),
    tokens = if ("token" %in% names(evidencia)) as.character(evidencia$token) else character(0),
    lemas = if ("lema" %in% names(evidencia)) as.character(evidencia$lema) else character(0),
    roles = if ("rol_funcional" %in% names(evidencia)) as.character(evidencia$rol_funcional) else character(0),
    tipo_memoria = "memoria_evidencia_trazable"
  )
}
