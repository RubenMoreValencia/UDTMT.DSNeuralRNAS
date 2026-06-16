#' Catalogo conceptual de UDT-MT DSNeuralRNAS
#'
#' Devuelve los componentes conceptuales usados por la Unidad Dinamica
#' Textual con Memoria Trazable. Este catalogo funciona como referencia
#' minima para interpretar la arquitectura propia del proyecto.
#'
#' @details
#' UDT-MT DSNeuralRNAS no implementa una LSTM. Su memoria no se basa en
#' compuertas internas, sino en stocks informativos, flujos textuales,
#' evidencia y trayectorias interpretativas. Esta funcion documenta los
#' componentes centrales para mantener trazabilidad conceptual entre codigo,
#' salida computacional y arquitectura DSNeuralRNAS.
#'
#' @return Un `data.frame` con los componentes, su tipo y su descripcion.
#'
#' @examples
#' catalogo_udtmt_ds()
#'
#' @export
catalogo_udtmt_ds <- function() {
  data.frame(
    componente = c(
      "texto_resuelto",
      "pregunta_interpretada",
      "stock_informativo",
      "flujo_textual",
      "memoria_textual",
      "memoria_pregunta",
      "memoria_evidencia",
      "trayectoria_interpretativa",
      "respuesta_trazable"
    ),
    tipo = c(
      "entrada_resuelta",
      "entrada_resuelta",
      "memoria",
      "dinamica",
      "memoria",
      "memoria",
      "memoria",
      "trayectoria",
      "salida"
    ),
    descripcion = c(
      "Matriz lexico-semantica generada por PLN.DSNeuralRNAS y Lexico.DSNeuralRNAS.",
      "Pregunta clasificada por intencion, modulo y tipo de respuesta esperada.",
      "Acumulacion textual asociada a estados, afectos, memoria, contexto o evidencia.",
      "Componente dinamico asociado a acciones, transformaciones, transiciones u operadores.",
      "Resumen acumulado del texto resuelto como base de memoria trazable.",
      "Resumen de intencion y alcance de la pregunta procesada.",
      "Evidencia textual conservada para sostener una respuesta.",
      "Lectura ordenada del texto como estado, transicion, contexto, evidencia y cambio.",
      "Respuesta generada con soporte en evidencia y trayectoria interpretativa."
    ),
    stringsAsFactors = FALSE
  )
}

#' Verificar dependencias de UDT-MT DSNeuralRNAS
#'
#' Comprueba que el entorno tenga disponible la libreria base
#' `PLN.DSNeuralRNAS`, necesaria para construir la unidad dinamica textual.
#'
#' @details
#' La arquitectura UDT-MT utiliza como entrada las salidas de
#' `PLN.DSNeuralRNAS`, que a su vez se apoya en `Lexico.DSNeuralRNAS`.
#' Esta funcion permite validar rapidamente si el proyecto puede ejecutar
#' el flujo minimo de texto, pregunta, evidencia y respuesta trazable.
#'
#' @return Una lista con indicadores logicos y mensajes de diagnostico.
#'
#' @examples
#' verificar_udtmt_ds()
#'
#' @export
verificar_udtmt_ds <- function() {
  ok_pln <- requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)
  list(
    ok = ok_pln,
    PLN.DSNeuralRNAS = ok_pln,
    mensaje = if (ok_pln) {
      "PLN.DSNeuralRNAS disponible. UDT-MT puede construir unidades textuales."
    } else {
      "PLN.DSNeuralRNAS no esta disponible. Instale o cargue primero la libreria marco de PLN."
    }
  )
}
