#' Refinar la trayectoria interpretativa UDT-MT
#'
#' Separa los componentes funcionales de una trayectoria textual para distinguir
#' contexto limpio, señales dinamicas, operadores textuales y flujos de
#' transformacion. Esta separacion evita que palabras asociadas al aprendizaje,
#' al cambio o a la evidencia sean tratadas como contexto simple.
#'
#' @param analisis_texto Tabla generada por `PLN.DSNeuralRNAS::analizar_texto_pln()`.
#' @param trayectoria Lista generada por `PLN.DSNeuralRNAS::construir_trayectoria_pln()`.
#'
#' @details
#' En la arquitectura UDT-MT, el contexto no debe mezclarse con señales
#' dinamicas. Por ejemplo, en una frase como "la perdida disminuye despues del
#' ajuste", `perdida`, `ajuste` y `mejora` funcionan mejor como señales del
#' proceso que como contexto ambiental. Esta funcion mantiene la trayectoria
#' original de PLN, pero agrega una clasificacion mas fina para memoria textual
#' trazable.
#'
#' La funcion no modifica diccionarios ni entrena modelos. Su objetivo es
#' mejorar la lectura funcional de la unidad antes de generar memoria, reporte o
#' salida futura hacia `ADTR.DSNeuralRNAS`.
#'
#' @return Una lista de trayectoria enriquecida con campos `contexto_limpio`,
#'   `senales_dinamicas`, `operadores_textuales`, `flujos_transformacion` y
#'   `lectura_refinada`.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   a <- PLN.DSNeuralRNAS::analizar_texto_pln("La perdida disminuye despues del ajuste", dic)
#' t <- PLN.DSNeuralRNAS::construir_trayectoria_pln(
#'   "La perdida disminuye despues del ajuste",
#'   "Que cambia?",
#'   dic
#' )
#'   refinar_trayectoria_udtmt_ds(a, t)
#' }
#'
#' @export
refinar_trayectoria_udtmt_ds <- function(analisis_texto, trayectoria) {
  if (!is.data.frame(analisis_texto)) {
    stop("analisis_texto debe ser un data.frame.", call. = FALSE)
  }
  if (!is.list(trayectoria)) {
    stop("trayectoria debe ser una lista.", call. = FALSE)
  }

  if (nrow(analisis_texto) == 0) {
    trayectoria$contexto_limpio <- character(0)
    trayectoria$senales_dinamicas <- character(0)
    trayectoria$operadores_textuales <- character(0)
    trayectoria$flujos_transformacion <- character(0)
    trayectoria$lectura_refinada <- "No se registran componentes para refinar."
    return(trayectoria)
  }

  col <- function(nombre) {
    if (nombre %in% names(analisis_texto)) as.character(analisis_texto[[nombre]]) else rep(NA_character_, nrow(analisis_texto))
  }

  token <- col("token")
  rol <- col("rol_funcional")
  tipo <- col("tipo_semantico")
  dominio <- col("dominio")
  categoria <- col("categoria_gramatical")
  evidencia <- if ("evidencia" %in% names(analisis_texto)) as.logical(analisis_texto$evidencia) else rep(FALSE, nrow(analisis_texto))
  peso <- col("peso_semantico")

  es_operador <- rol %in% c(
    "marcador_referencial", "operador_adicion", "operador_causal",
    "operador_contraste", "inversor_semantico", "transicion_temporal"
  ) | categoria %in% c("articulo", "preposicion", "conector", "pronombre", "determinante")

  es_flujo <- grepl("^flujo", rol) | rol %in% c("transicion_temporal") | tipo %in% c("accion", "transformacion")

  es_senal_dinamica <- tipo %in% c(
    "aprendizaje", "regimen", "control", "transformacion", "accion"
  ) | dominio %in% c("aprendizaje", "dinamico") | rol %in% c(
    "evidencia", "stock_evidencia", "flujo_transformacion", "flujo_accion",
    "control", "regimen", "stock_contexto"
  )

  # Evita clasificar como senal dinamica operadores puros de bajo contenido.
  es_senal_dinamica <- es_senal_dinamica & !es_operador

  es_contexto_limpio <- rol %in% c("contexto", "stock_contexto") |
    tipo %in% c("naturaleza", "espacio", "relacion_social", "memoria", "estado", "emocion")
  es_contexto_limpio <- es_contexto_limpio & !es_operador &
    !(tipo %in% c("aprendizaje", "transformacion", "accion", "control", "regimen"))

  contexto_limpio <- unique(token[es_contexto_limpio & !is.na(token)])
  senales_dinamicas <- unique(token[es_senal_dinamica & !is.na(token)])
  operadores_textuales <- unique(token[es_operador & !is.na(token)])
  flujos_transformacion <- unique(token[es_flujo & !es_operador & !is.na(token)])
  evidencia_refinada <- unique(token[evidencia & !is.na(token) & !es_operador])

  estado_inicial <- if (!is.null(trayectoria$estado_inicial)) trayectoria$estado_inicial else character(0)
  cambio_final <- if (length(flujos_transformacion) > 0) tail(flujos_transformacion, 2) else if (!is.null(trayectoria$cambio_final)) trayectoria$cambio_final else character(0)

  parte_estado <- if (length(estado_inicial) > 0) paste0("estado o condicion inicial: ", paste(estado_inicial, collapse = ", ")) else "estado o condicion inicial no identificado"
  parte_flujos <- if (length(flujos_transformacion) > 0) paste0("flujos de transformacion: ", paste(flujos_transformacion, collapse = ", ")) else "flujos de transformacion no identificados"
  parte_contexto <- if (length(contexto_limpio) > 0) paste0("contexto limpio: ", paste(contexto_limpio, collapse = ", ")) else "contexto limpio no identificado"
  parte_senales <- if (length(senales_dinamicas) > 0) paste0("senales dinamicas: ", paste(senales_dinamicas, collapse = ", ")) else "senales dinamicas no identificadas"
  parte_cambio <- if (length(cambio_final) > 0) paste0("cambio final sugerido: ", paste(cambio_final, collapse = ", ")) else "cambio final no identificado"

  trayectoria$contexto_original <- trayectoria$contexto
  trayectoria$contexto_limpio <- contexto_limpio
  trayectoria$senales_dinamicas <- senales_dinamicas
  trayectoria$operadores_textuales <- operadores_textuales
  trayectoria$flujos_transformacion <- flujos_transformacion
  trayectoria$evidencia_refinada <- evidencia_refinada
  trayectoria$cambio_final <- cambio_final
  trayectoria$lectura_refinada <- paste(parte_estado, parte_flujos, parte_contexto, parte_senales, parte_cambio, sep = " | ")
  trayectoria$lectura <- trayectoria$lectura_refinada
  trayectoria
}

#' Clasificar componentes funcionales de una unidad UDT-MT
#'
#' Devuelve un resumen estructurado de los componentes refinados de una unidad
#' UDT-MT: contexto limpio, señales dinamicas, operadores textuales, flujos de
#' transformacion y evidencia refinada.
#'
#' @param unidad Objeto creado por [crear_udtmt_ds()].
#'
#' @return Una lista con componentes funcionales refinados.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   u <- crear_udtmt_ds("La perdida disminuye despues del ajuste", "Que cambia?", dic)
#'   clasificar_componentes_udtmt_ds(u)
#' }
#'
#' @export
clasificar_componentes_udtmt_ds <- function(unidad) {
  if (!inherits(unidad, "udtmt_ds") && !is.list(unidad)) {
    stop("unidad debe ser un objeto creado por crear_udtmt_ds().", call. = FALSE)
  }
  trayectoria <- if (!is.null(unidad$trayectoria)) unidad$trayectoria else list()
  list(
    contexto_limpio = if (!is.null(trayectoria$contexto_limpio)) trayectoria$contexto_limpio else character(0),
    senales_dinamicas = if (!is.null(trayectoria$senales_dinamicas)) trayectoria$senales_dinamicas else character(0),
    operadores_textuales = if (!is.null(trayectoria$operadores_textuales)) trayectoria$operadores_textuales else character(0),
    flujos_transformacion = if (!is.null(trayectoria$flujos_transformacion)) trayectoria$flujos_transformacion else character(0),
    evidencia_refinada = if (!is.null(trayectoria$evidencia_refinada)) trayectoria$evidencia_refinada else character(0),
    lectura_refinada = if (!is.null(trayectoria$lectura_refinada)) trayectoria$lectura_refinada else if (!is.null(trayectoria$lectura)) trayectoria$lectura else NA_character_
  )
}
