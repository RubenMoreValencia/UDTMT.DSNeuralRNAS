#' Crear stocks textuales desde un analisis PLN
#'
#' Construye una tabla de stocks informativos a partir de la matriz
#' lexico-semantica generada por `PLN.DSNeuralRNAS`.
#'
#' @param analisis_texto `data.frame` generado por `analizar_texto_pln()` o por una
#'   funcion equivalente que contenga al menos `token`, `lema`, `rol_funcional`,
#'   `tipo_dinamico`, `peso_semantico` y `evidencia`.
#' @param trayectoria Lista opcional generada por `construir_trayectoria_pln()`.
#'
#' @details
#' En UDT-MT, un stock textual es una acumulacion informativa. Puede surgir
#' de estados afectivos, memoria, evidencia, contexto o componentes que
#' permanecen como referencia durante la interpretacion. No equivale a una
#' celda LSTM; es una memoria trazable y visible.
#'
#' @return Un `data.frame` con stocks textuales, tipo, contenido, origen y peso.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   a <- PLN.DSNeuralRNAS::analizar_texto_pln("La calma llega despues de la lluvia", dic)
#'   crear_stock_textual_ds(a)
#' }
#'
#' @export
crear_stock_textual_ds <- function(analisis_texto, trayectoria = NULL) {
  if (!is.data.frame(analisis_texto)) {
    stop("analisis_texto debe ser un data.frame.", call. = FALSE)
  }
  if (!"rol_funcional" %in% names(analisis_texto)) {
    stop("analisis_texto debe contener la columna rol_funcional.", call. = FALSE)
  }

  roles <- as.character(analisis_texto$rol_funcional)
  tipos <- if ("tipo_dinamico" %in% names(analisis_texto)) as.character(analisis_texto$tipo_dinamico) else rep(NA_character_, nrow(analisis_texto))
  evidencia <- if ("evidencia" %in% names(analisis_texto)) as.logical(analisis_texto$evidencia) else rep(FALSE, nrow(analisis_texto))

  idx <- grepl("^stock_", roles) |
    roles %in% c("contexto", "evidencia") |
    tipos %in% c("stock_informativo", "componente_textual") |
    evidencia

  base <- analisis_texto[idx & !is.na(roles), , drop = FALSE]
  if (nrow(base) == 0) {
    return(data.frame(
      id_stock = character(0), tipo_stock = character(0), token = character(0),
      lema = character(0), rol_funcional = character(0), origen = character(0),
      peso = character(0), persistencia = character(0), stringsAsFactors = FALSE
    ))
  }

  peso <- if ("peso_semantico" %in% names(base)) as.character(base$peso_semantico) else rep("medio", nrow(base))
  data.frame(
    id_stock = paste0("S", seq_len(nrow(base))),
    tipo_stock = ifelse(grepl("^stock_", base$rol_funcional), base$rol_funcional, paste0("stock_", base$rol_funcional)),
    token = as.character(base$token),
    lema = as.character(base$lema),
    rol_funcional = as.character(base$rol_funcional),
    origen = "analisis_pln",
    peso = peso,
    persistencia = ifelse(peso == "alto", "alta", ifelse(peso == "medio", "media", "baja")),
    stringsAsFactors = FALSE
  )
}

#' Crear flujos textuales desde un analisis PLN
#'
#' Construye una tabla de flujos textuales a partir de verbos, transiciones,
#' operadores y componentes dinamicos identificados en el texto.
#'
#' @param analisis_texto `data.frame` generado por `analizar_texto_pln()` o una
#'   matriz lexico-semantica equivalente.
#'
#' @details
#' Un flujo textual representa accion, cambio, transicion, adicion, contraste,
#' direccion, control o transformacion. En UDT-MT los flujos actualizan o
#' conectan stocks informativos, permitiendo que la memoria textual tenga
#' dinamica visible y trazable.
#'
#' @return Un `data.frame` con flujos textuales, tipo, token, lema, regla y peso.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   a <- PLN.DSNeuralRNAS::analizar_texto_pln("La calma llega despues de la lluvia", dic)
#'   crear_flujo_textual_ds(a)
#' }
#'
#' @export
crear_flujo_textual_ds <- function(analisis_texto) {
  if (!is.data.frame(analisis_texto)) {
    stop("analisis_texto debe ser un data.frame.", call. = FALSE)
  }
  roles <- if ("rol_funcional" %in% names(analisis_texto)) as.character(analisis_texto$rol_funcional) else rep(NA_character_, nrow(analisis_texto))
  tipos <- if ("tipo_dinamico" %in% names(analisis_texto)) as.character(analisis_texto$tipo_dinamico) else rep(NA_character_, nrow(analisis_texto))
  categoria <- if ("categoria_gramatical" %in% names(analisis_texto)) as.character(analisis_texto$categoria_gramatical) else rep(NA_character_, nrow(analisis_texto))

  idx <- grepl("^flujo_", roles) |
    grepl("^transicion_", roles) |
    grepl("^operador_", roles) |
    roles %in% c("control", "perturbacion", "marcador_referencial") |
    tipos %in% c("flujo_textual", "operador_textual") |
    categoria %in% c("verbo", "conector", "adverbio", "preposicion")

  base <- analisis_texto[idx & !is.na(roles), , drop = FALSE]
  if (nrow(base) == 0) {
    return(data.frame(
      id_flujo = character(0), tipo_flujo = character(0), token = character(0),
      lema = character(0), rol_funcional = character(0), regla = character(0),
      peso = character(0), stringsAsFactors = FALSE
    ))
  }

  peso <- if ("peso_funcional" %in% names(base)) as.character(base$peso_funcional) else rep("medio", nrow(base))
  regla <- if ("regla_flexion" %in% names(base)) as.character(base$regla_flexion) else rep("sin_regla", nrow(base))
  data.frame(
    id_flujo = paste0("F", seq_len(nrow(base))),
    tipo_flujo = as.character(base$rol_funcional),
    token = as.character(base$token),
    lema = as.character(base$lema),
    rol_funcional = as.character(base$rol_funcional),
    regla = regla,
    peso = peso,
    stringsAsFactors = FALSE
  )
}

#' Actualizar stocks textuales mediante flujos
#'
#' Agrega una lectura simple de actualizacion dinamica a los stocks
#' informativos usando los flujos textuales identificados.
#'
#' @param stocks `data.frame` generado por `crear_stock_textual_ds()`.
#' @param flujos `data.frame` generado por `crear_flujo_textual_ds()`.
#'
#' @details
#' Esta funcion no entrena un modelo. Solo prepara una lectura dinamica
#' inicial: los stocks conservan informacion y los flujos indican acciones
#' o transiciones que pueden activar, reforzar o conectar esos stocks.
#'
#' @return Un `data.frame` de stocks con columnas de activacion y lectura.
#'
#' @examples
#' s <- data.frame(id_stock = "S1", token = "calma", peso = "alto")
#' f <- data.frame(id_flujo = "F1", token = "llega", tipo_flujo = "flujo_accion")
#' actualizar_stock_textual_ds(s, f)
#'
#' @export
actualizar_stock_textual_ds <- function(stocks, flujos) {
  if (!is.data.frame(stocks)) stop("stocks debe ser un data.frame.", call. = FALSE)
  if (!is.data.frame(flujos)) stop("flujos debe ser un data.frame.", call. = FALSE)
  if (nrow(stocks) == 0) return(stocks)

  n_flujos <- nrow(flujos)
  intensidad_base <- ifelse(stocks$peso == "alto", 1.0, ifelse(stocks$peso == "medio", 0.6, 0.3))
  factor_flujo <- if (n_flujos == 0) 0 else min(1, n_flujos / 5)
  stocks$activacion_inicial <- intensidad_base
  stocks$activacion_actualizada <- pmin(1, intensidad_base + 0.2 * factor_flujo)
  stocks$lectura_dinamica <- ifelse(
    stocks$activacion_actualizada >= 0.8,
    "stock_relevante",
    ifelse(stocks$activacion_actualizada >= 0.5, "stock_medio", "stock_debil")
  )
  stocks
}
