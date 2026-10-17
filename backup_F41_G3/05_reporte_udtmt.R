#' Interpretar la memoria de una unidad UDT-MT DSNeuralRNAS
#'
#' Construye una lectura academica compacta de la memoria textual, memoria de
#' pregunta y memoria de evidencia contenidas en una unidad UDT-MT.
#'
#' @param unidad Objeto generado por [crear_udtmt_ds()].
#'
#' @details
#' Esta funcion no entrena un modelo neuronal ni implementa una LSTM. Su proposito
#' es interpretar la memoria textual trazable construida por la arquitectura
#' UDT-MT DSNeuralRNAS. La lectura se apoya en los stocks informativos, flujos
#' textuales, intencion de pregunta, evidencia y trayectoria interpretativa que
#' provienen del flujo `Lexico.DSNeuralRNAS -> PLN.DSNeuralRNAS -> UDTMT.DSNeuralRNAS`.
#'
#' Conceptualmente, la funcion permite observar si la unidad textual conserva
#' una memoria suficiente para ser usada luego por `ADTR.DSNeuralRNAS` como punto
#' de lectura transformacional-referencial.
#'
#' @return Una lista con resumen, lectura de stocks, lectura de flujos, lectura
#'   de memoria, lectura de trayectoria y conclusion interpretativa.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   u <- crear_udtmt_ds(
#'     texto = "La calma llega despues de la lluvia",
#'     pregunta = "Que cambia en el texto?",
#'     diccionarios = dic
#'   )
#'   interpretar_memoria_udtmt_ds(u)
#' }
#'
#' @export
interpretar_memoria_udtmt_ds <- function(unidad) {
  if (!inherits(unidad, "udtmt_ds") && !is.list(unidad)) {
    stop("unidad debe ser un objeto creado por crear_udtmt_ds().", call. = FALSE)
  }

  resumen <- resumir_udtmt_ds(unidad)

  stocks <- if (!is.null(unidad$stocks) && nrow(unidad$stocks) > 0) unidad$stocks else data.frame()
  flujos <- if (!is.null(unidad$flujos) && nrow(unidad$flujos) > 0) unidad$flujos else data.frame()
  memoria <- if (!is.null(unidad$memoria)) unidad$memoria else list()
  trayectoria <- if (!is.null(unidad$trayectoria)) unidad$trayectoria else list()
  componentes <- clasificar_componentes_udtmt_ds(unidad)

  lectura_stocks <- if (nrow(stocks) > 0) {
    paste0(
      "La unidad conserva ", nrow(stocks),
      " stocks informativos. Los tokens principales son: ",
      paste(stocks$token, collapse = ", "), "."
    )
  } else {
    "La unidad no registra stocks informativos."
  }

  lectura_flujos <- if (nrow(flujos) > 0) {
    paste0(
      "La unidad registra ", nrow(flujos),
      " flujos textuales u operadores. Los flujos observados son: ",
      paste(flujos$token, collapse = ", "), "."
    )
  } else {
    "La unidad no registra flujos textuales."
  }

  lectura_memoria <- paste0(
    "La memoria textual conserva ", if (!is.null(memoria$textual$n_tokens)) memoria$textual$n_tokens else NA_integer_,
    " tokens, ", if (!is.null(memoria$textual$n_stocks)) memoria$textual$n_stocks else NA_integer_,
    " stocks y ", if (!is.null(memoria$textual$n_flujos)) memoria$textual$n_flujos else NA_integer_,
    " flujos. La memoria de pregunta identifica la intencion como '",
    if (!is.null(memoria$pregunta$intencion)) memoria$pregunta$intencion else NA_character_,
    "'."
  )

  lectura_trayectoria <- if (!is.null(trayectoria$lectura)) {
    trayectoria$lectura
  } else {
    "No se registra lectura de trayectoria."
  }

  lectura_componentes <- paste0(
    "La trayectoria refinada separa contexto limpio (",
    if (length(componentes$contexto_limpio) > 0) paste(componentes$contexto_limpio, collapse = ", ") else "sin elementos",
    "), senales dinamicas (",
    if (length(componentes$senales_dinamicas) > 0) paste(componentes$senales_dinamicas, collapse = ", ") else "sin elementos",
    ") y flujos de transformacion (",
    if (length(componentes$flujos_transformacion) > 0) paste(componentes$flujos_transformacion, collapse = ", ") else "sin elementos",
    ")."
  )

  conclusion <- paste0(
    "La unidad UDT-MT se interpreta como una memoria textual trazable de tipo '",
    resumen$arquitectura,
    "'. No corresponde a una LSTM; organiza memoria mediante stocks, flujos y evidencia."
  )

  list(
    resumen = resumen,
    lectura_stocks = lectura_stocks,
    lectura_flujos = lectura_flujos,
    lectura_memoria = lectura_memoria,
    lectura_trayectoria = lectura_trayectoria,
    lectura_componentes = lectura_componentes,
    componentes_funcionales = componentes,
    conclusion = conclusion
  )
}

#' Generar reporte academico de una unidad UDT-MT DSNeuralRNAS
#'
#' Produce un reporte textual, en formato Markdown simple, para documentar la
#' estructura dinamica textual de una unidad UDT-MT.
#'
#' @param unidad Objeto generado por [crear_udtmt_ds()].
#' @param archivo Ruta opcional de archivo `.md` donde se guardara el reporte.
#'   Si es `NULL`, el reporte se devuelve como vector de caracteres.
#' @param incluir_tablas Valor logico. Si es `TRUE`, incluye tablas Markdown
#'   compactas de stocks y flujos.
#'
#' @details
#' El reporte esta pensado para uso academico y documental dentro del ecosistema
#' DSNeuralRNAS. Resume la ruta `texto -> resolucion lexica -> pregunta -> stocks
#' -> flujos -> memoria -> trayectoria -> salida ADTR`, permitiendo dejar evidencia
#' de ejecucion antes de avanzar a integraciones con `ADTR.DSNeuralRNAS` o
#' extensiones de aprendizaje dinamico.
#'
#' Esta funcion no realiza entrenamiento, no implementa LSTM y no modifica la
#' unidad. Solo organiza los resultados ya generados por la arquitectura UDT-MT.
#'
#' @return Un vector de caracteres con el reporte. Si `archivo` no es `NULL`,
#'   tambien escribe el reporte en disco y devuelve el mismo contenido de forma
#'   invisible.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   u <- crear_udtmt_ds(
#'     texto = "La calma llega despues de la lluvia",
#'     pregunta = "Que cambia en el texto?",
#'     diccionarios = dic
#'   )
#'   generar_reporte_udtmt_ds(u)
#' }
#'
#' @export
generar_reporte_udtmt_ds <- function(unidad, archivo = NULL, incluir_tablas = TRUE) {
  if (!inherits(unidad, "udtmt_ds") && !is.list(unidad)) {
    stop("unidad debe ser un objeto creado por crear_udtmt_ds().", call. = FALSE)
  }
  if (!is.null(archivo) && (!is.character(archivo) || length(archivo) != 1)) {
    stop("archivo debe ser NULL o una ruta de archivo de longitud 1.", call. = FALSE)
  }

  interpretacion <- interpretar_memoria_udtmt_ds(unidad)
  resumen <- interpretacion$resumen
  traza <- trazar_udtmt_ds(unidad)

  lineas <- c(
    "# Reporte UDT-MT DSNeuralRNAS",
    "",
    "## 1. Identificacion de la arquitectura",
    "",
    paste0("- Arquitectura: ", resumen$arquitectura),
    paste0("- Es LSTM: ", resumen$es_lstm),
    paste0("- Intencion interpretada: ", resumen$intencion),
    "",
    "## 2. Resumen dinamico textual",
    "",
    paste0("- Numero de tokens: ", resumen$n_tokens),
    paste0("- Numero de stocks informativos: ", resumen$n_stocks),
    paste0("- Numero de flujos textuales: ", resumen$n_flujos),
    paste0("- Numero de evidencias: ", resumen$n_evidencias),
    "",
    "## 3. Lectura de memoria",
    "",
    interpretacion$lectura_stocks,
    "",
    interpretacion$lectura_flujos,
    "",
    interpretacion$lectura_memoria,
    "",
    "## 4. Trayectoria interpretativa",
    "",
    interpretacion$lectura_trayectoria,
    "",
    "## 5. Componentes funcionales refinados",
    "",
    interpretacion$lectura_componentes,
    "",
    "## 6. Respuesta trazable",
    "",
    if (!is.null(resumen$respuesta)) resumen$respuesta else "No se registra respuesta trazable.",
    "",
    "## 7. Ruta UDT-MT",
    "",
    paste0("`", paste(traza$ruta_udtmt, collapse = " -> "), "`"),
    "",
    "## 8. Conclusión",
    "",
    interpretacion$conclusion
  )

  if (isTRUE(incluir_tablas)) {
    lineas <- c(lineas, "", "## 9. Tabla compacta de stocks", "")
    if (!is.null(unidad$stocks) && nrow(unidad$stocks) > 0) {
      lineas <- c(lineas, markdown_tabla_compacta_udtmt(unidad$stocks, c("id_stock", "tipo_stock", "token", "rol_funcional", "peso")))
    } else {
      lineas <- c(lineas, "No se registran stocks.")
    }
    lineas <- c(lineas, "", "## 10. Tabla compacta de flujos", "")
    if (!is.null(unidad$flujos) && nrow(unidad$flujos) > 0) {
      lineas <- c(lineas, markdown_tabla_compacta_udtmt(unidad$flujos, c("id_flujo", "tipo_flujo", "token", "rol_funcional", "peso")))
    } else {
      lineas <- c(lineas, "No se registran flujos.")
    }
  }

  if (!is.null(archivo)) {
    dir.create(dirname(archivo), recursive = TRUE, showWarnings = FALSE)
    writeLines(lineas, archivo, useBytes = TRUE)
    return(invisible(lineas))
  }

  lineas
}

markdown_tabla_compacta_udtmt <- function(x, columnas) {
  columnas <- intersect(columnas, names(x))
  if (length(columnas) == 0 || nrow(x) == 0) {
    return("No hay columnas disponibles para la tabla.")
  }
  x <- x[, columnas, drop = FALSE]
  x[] <- lapply(x, function(v) as.character(v))
  encabezado <- paste0("| ", paste(columnas, collapse = " | "), " |")
  separador <- paste0("| ", paste(rep("---", length(columnas)), collapse = " | "), " |")
  filas <- apply(x, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  c(encabezado, separador, filas)
}
