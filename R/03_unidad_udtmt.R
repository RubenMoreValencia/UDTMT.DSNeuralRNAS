#' Construir trayectoria UDT-MT desde texto y pregunta
#'
#' Genera una trayectoria interpretativa enriquecida a partir de las salidas
#' de `PLN.DSNeuralRNAS`.
#'
#' @param texto Cadena de texto en espanol general.
#' @param pregunta Cadena con la pregunta o consulta del usuario.
#' @param dic Diccionarios cargados por `PLN.DSNeuralRNAS::cargar_diccionarios_pln()`.
#'   Si es `NULL`, se intentan cargar automaticamente.
#' @param diccionarios Alias opcional de `dic`. Se incorpora para mantener
#'   compatibilidad con llamadas provenientes de flujos PLN que nombran el
#'   objeto como diccionarios.
#'
#' @details
#' Esta funcion no entrena modelos. Organiza la lectura del texto como
#' trayectoria: estado inicial, transiciones, contexto, evidencia y cambio
#' final. La salida queda preparada para ser usada como base de memoria en
#' `crear_udtmt_ds()`.
#'
#' @return Una lista con la trayectoria PLN y una ruta conceptual UDT-MT.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   construir_trayectoria_udtmt_ds("La calma llega despues de la lluvia", "Que cambia?", dic)
#' }
#'
#' @export
construir_trayectoria_udtmt_ds <- function(texto, pregunta, dic = NULL, diccionarios = NULL) {
  if (!is.null(diccionarios)) {
    if (!is.null(dic)) {
      warning("Se proporcionaron `dic` y `diccionarios`; se usara `dic`.", call. = FALSE)
    } else {
      dic <- diccionarios
    }
  }
  if (!requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
    stop("Se requiere PLN.DSNeuralRNAS para construir la trayectoria UDT-MT.", call. = FALSE)
  }
  if (is.null(dic)) dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  analisis_texto <- PLN.DSNeuralRNAS::analizar_texto_pln(texto, dic)
  trayectoria <- PLN.DSNeuralRNAS::construir_trayectoria_pln(texto, pregunta, dic)
  trayectoria <- refinar_trayectoria_udtmt_ds(analisis_texto, trayectoria)
  list(
    trayectoria_pln = trayectoria,
    ruta_udtmt = c(
      "texto_resuelto",
      "pregunta_interpretada",
      "stocks_informativos",
      "flujos_textuales",
      "memoria_trazable",
      "trayectoria_udtmt"
    )
  )
}

#' Crear una unidad UDT-MT DSNeuralRNAS
#'
#' Construye una Unidad Dinamica Textual con Memoria Trazable a partir de
#' un texto y una pregunta procesados por `PLN.DSNeuralRNAS`.
#'
#' @param texto Cadena de texto en espanol general.
#' @param pregunta Cadena con pregunta o consulta.
#' @param dic Diccionarios de PLN. Si es `NULL`, se cargan desde
#'   `PLN.DSNeuralRNAS`.
#' @param diccionarios Alias opcional de `dic`. Se agrega para que el usuario
#'   pueda llamar `crear_udtmt_ds(texto, pregunta, diccionarios = dic)` sin
#'   romper la interfaz conceptual del ecosistema.
#'
#' @details
#' La UDT-MT DSNeuralRNAS no es una LSTM. Su memoria se construye mediante
#' stocks informativos y flujos textuales. El texto y la pregunta se resuelven
#' primero con `Lexico.DSNeuralRNAS` a traves de `PLN.DSNeuralRNAS`; luego se
#' organizan como memoria textual, memoria de pregunta y memoria de evidencia.
#'
#' La funcion devuelve una estructura `list` con clase `udtmt_ds`, preparada
#' para resumen, trazabilidad y futura conexion con `ML.DSNeuralRNAS` y
#' `ADTR.DSNeuralRNAS`.
#'
#' Desde la version 0.1.2, la funcion acepta el alias `diccionarios` para
#' facilitar el uso desde scripts integrados. Si se proporcionan `dic` y
#' `diccionarios`, se prioriza `dic`.
#'
#' @return Una lista de clase `udtmt_ds` con texto, pregunta, stocks, flujos,
#'   memoria, trayectoria, respuesta y ruta conceptual.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   u <- crear_udtmt_ds(
#'     "La calma llega despues de las lluvias y la perdida disminuye",
#'     "Que cambia en el texto?",
#'     dic
#'   )
#'   resumir_udtmt_ds(u)
#' }
#'
#' @export
crear_udtmt_ds <- function(texto, pregunta, dic = NULL, diccionarios = NULL) {
  if (!is.null(diccionarios)) {
    if (!is.null(dic)) {
      warning("Se proporcionaron `dic` y `diccionarios`; se usara `dic`.", call. = FALSE)
    } else {
      dic <- diccionarios
    }
  }
  if (!requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
    stop("Se requiere PLN.DSNeuralRNAS para crear UDT-MT.", call. = FALSE)
  }
  if (is.null(dic)) dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()

  analisis_texto <- PLN.DSNeuralRNAS::analizar_texto_pln(texto, dic)
  analisis_pregunta <- PLN.DSNeuralRNAS::analizar_pregunta_pln(pregunta, dic)
  evidencia <- PLN.DSNeuralRNAS::extraer_evidencia_pln(texto, pregunta, dic)
  respuesta_pln <- PLN.DSNeuralRNAS::responder_trazable_pln(texto, pregunta, dic)
  trayectoria <- PLN.DSNeuralRNAS::construir_trayectoria_pln(texto, pregunta, dic)
  trayectoria <- refinar_trayectoria_udtmt_ds(analisis_texto, trayectoria)
  relaciones_semanticas <- construir_matriz_relacional_udtmt_ds(analisis_texto, trayectoria, texto = texto)

  respuesta <- construir_respuesta_refinada_udtmt_ds(
    trayectoria = trayectoria,
    respuesta_pln = respuesta_pln,
    analisis_pregunta = analisis_pregunta
  )

  stocks <- crear_stock_textual_ds(analisis_texto, trayectoria)
  flujos <- crear_flujo_textual_ds(analisis_texto)
  stocks_actualizados <- actualizar_stock_textual_ds(stocks, flujos)

  memoria <- list(
    textual = registrar_memoria_textual_ds(analisis_texto, stocks_actualizados, flujos, trayectoria),
    pregunta = registrar_memoria_pregunta_ds(analisis_pregunta),
    evidencia = registrar_memoria_evidencia_ds(evidencia)
  )

  unidad <- list(
    texto_original = texto,
    pregunta_original = pregunta,
    analisis_texto = analisis_texto,
    analisis_pregunta = analisis_pregunta,
    stocks = stocks_actualizados,
    flujos = flujos,
    memoria = memoria,
    trayectoria = trayectoria,
    relaciones_semanticas = relaciones_semanticas,
    respuesta = respuesta,
    ruta_udtmt = c(
      "texto_crudo",
      "resolucion_lexica_pln",
      "pregunta_interpretada",
      "stocks_informativos",
      "flujos_textuales",
      "memoria_textual_trazable",
      "trayectoria_interpretativa",
      "salida_para_adtr"
    ),
    arquitectura = "UDT-MT DSNeuralRNAS",
    es_lstm = FALSE
  )
  class(unidad) <- c("udtmt_ds", class(unidad))
  unidad
}
