#' Construir respuesta trazable refinada UDT-MT
#'
#' Genera una respuesta textual que usa la trayectoria refinada de UDT-MT,
#' separando contexto limpio, senales dinamicas, operadores textuales y flujos
#' de transformacion.
#'
#' @param trayectoria Lista de trayectoria generada o refinada por
#'   [refinar_trayectoria_udtmt_ds()].
#' @param respuesta_pln Respuesta original generada por
#'   `PLN.DSNeuralRNAS::responder_trazable_pln()`. Se conserva como referencia
#'   y respaldo.
#' @param analisis_pregunta Tabla generada por
#'   `PLN.DSNeuralRNAS::analizar_pregunta_pln()`.
#'
#' @details
#' Esta funcion corrige la lectura final de UDT-MT cuando la respuesta heredada
#' de PLN conserva etiquetas generales de contexto. La respuesta refinada usa
#' directamente los componentes funcionales de UDT-MT: stocks, senales dinamicas,
#' flujos de transformacion, evidencia refinada y contexto limpio.
#'
#' La funcion no entrena modelos, no genera lenguaje libre no trazado y no
#' implementa una LSTM. Su finalidad es alinear la respuesta textual con la
#' memoria dinamica trazable ya construida por la unidad UDT-MT.
#'
#' @return Una lista con pregunta, intencion, modulo, evidencia refinada,
#'   respuesta refinada, respuesta PLN original y ruta de respuesta.
#'
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   a <- PLN.DSNeuralRNAS::analizar_texto_pln("La perdida disminuye despues del ajuste", dic)
#'   t <- PLN.DSNeuralRNAS::construir_trayectoria_pln(
#'     "La perdida disminuye despues del ajuste", "Que cambia?", dic
#'   )
#'   t <- refinar_trayectoria_udtmt_ds(a, t)
#'   construir_respuesta_refinada_udtmt_ds(t)
#' }
#'
#' @export
construir_respuesta_refinada_udtmt_ds <- function(trayectoria, respuesta_pln = NULL, analisis_pregunta = NULL) {
  if (!is.list(trayectoria)) {
    stop("trayectoria debe ser una lista.", call. = FALSE)
  }

  primer_valor <- function(x, defecto = NA_character_) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return(defecto)
    as.character(x[[1]])
  }

  tomar <- function(nombre) {
    if (!is.null(trayectoria[[nombre]])) unique(as.character(trayectoria[[nombre]][!is.na(trayectoria[[nombre]])])) else character(0)
  }

  intencion <- primer_valor(trayectoria$intencion, "sin_intencion")
  modulo <- primer_valor(trayectoria$modulo_activado, "sin_modulo")
  pregunta <- if (!is.null(respuesta_pln$pregunta)) {
    primer_valor(respuesta_pln$pregunta, NA_character_)
  } else if (!is.null(analisis_pregunta) && is.data.frame(analisis_pregunta) && "pregunta_normalizada" %in% names(analisis_pregunta)) {
    primer_valor(analisis_pregunta$pregunta_normalizada, NA_character_)
  } else {
    NA_character_
  }

  estado <- tomar("estado_inicial")
  contexto <- tomar("contexto_limpio")
  senales <- tomar("senales_dinamicas")
  flujos <- tomar("flujos_transformacion")
  evidencia <- tomar("evidencia_refinada")
  cambio <- tomar("cambio_final")

  if (length(evidencia) == 0) evidencia <- tomar("evidencia")

  texto_lista <- function(x) {
    if (length(x) == 0) "no identificado" else paste(x, collapse = ", ")
  }

  respuesta <- switch(
    intencion,
    "transformacion" = {
      parte_flujo <- if (length(flujos) > 0) {
        paste0("El flujo principal de cambio se expresa en: ", texto_lista(flujos), ".")
      } else {
        "No se identifica un flujo principal de cambio."
      }
      parte_senales <- if (length(senales) > 0) {
        paste0("Las senales dinamicas que sostienen la lectura son: ", texto_lista(senales), ".")
      } else {
        "No se identifican senales dinamicas suficientes."
      }
      parte_contexto <- if (length(contexto) > 0) {
        paste0("El contexto limpio aparece en: ", texto_lista(contexto), ".")
      } else {
        "No se identifica un contexto externo dominante; la lectura se concentra en el proceso dinamico interno."
      }
      parte_cambio <- if (length(cambio) > 0) {
        paste0("El cambio final se refuerza con: ", texto_lista(cambio), ".")
      } else {
        "El cambio final queda pendiente de interpretacion."
      }
      paste("El texto puede leerse como una trayectoria de transformacion.", parte_flujo, parte_senales, parte_contexto, parte_cambio)
    },
    "naturaleza" = {
      base <- if (length(contexto) > 0) contexto else evidencia
      paste0("Los elementos de naturaleza o contexto limpio relevantes son: ", texto_lista(base), ".")
    },
    "emocion" = {
      base <- if (length(estado) > 0) estado else evidencia
      paste0("La lectura afectiva se sostiene en: ", texto_lista(base), ".")
    },
    "contexto" = {
      base <- if (length(contexto) > 0) contexto else evidencia
      paste0("El contexto textual limpio se sostiene en: ", texto_lista(base), ".")
    },
    "senales_dinamicas" = {
      base <- if (length(senales) > 0) senales else evidencia
      paste0("Las senales dinamicas principales son: ", texto_lista(base), ".")
    },
    "evidencia" = {
      paste0("La evidencia textual refinada principal es: ", texto_lista(evidencia), ".")
    },
    "trayectoria" = {
      lectura <- if (!is.null(trayectoria$lectura_refinada)) trayectoria$lectura_refinada else if (!is.null(trayectoria$lectura)) trayectoria$lectura else texto_lista(evidencia)
      paste0("La trayectoria textual se interpreta asi: ", lectura, ".")
    },
    "causalidad" = {
      base <- if (length(senales) > 0) senales else evidencia
      paste0("La posible relacion causal o de apoyo se revisa desde: ", texto_lista(base), ".")
    },
    "contraste" = {
      base <- if (length(evidencia) > 0) evidencia else c(senales, contexto)
      paste0("El posible contraste textual se sostiene en: ", texto_lista(base), ".")
    },
    "memoria" = {
      base <- if (length(estado) > 0) estado else evidencia
      paste0("La memoria textual se sostiene en: ", texto_lista(base), ".")
    },
    "dsneuralrnas" = {
      base <- if (length(estado) > 0) estado else evidencia
      parte_base <- paste0("La memoria textual se sostiene en: ", texto_lista(base), ".")
      parte_flujos <- if (length(flujos) > 0) paste0("Los flujos que activan o transforman la trayectoria son: ", texto_lista(flujos), ".") else "No se registran flujos de transformacion dominantes."
      paste(parte_base, parte_flujos)
    },
    {
      paste0("La evidencia textual refinada principal es: ", texto_lista(evidencia), ".")
    }
  )

  list(
    pregunta = pregunta,
    intencion = intencion,
    modulo_activado = modulo,
    evidencia_refinada = evidencia,
    componentes_funcionales = list(
      contexto_limpio = contexto,
      senales_dinamicas = senales,
      flujos_transformacion = flujos,
      estado_inicial = estado,
      cambio_final = cambio
    ),
    respuesta = respuesta,
    respuesta_pln_original = if (!is.null(respuesta_pln$respuesta)) respuesta_pln$respuesta else NA_character_,
    ruta_respuesta = c(
      "pregunta",
      "intencion",
      "componentes_funcionales_refinados",
      "respuesta_trazable_refinada",
      "base_para_adtr"
    )
  )
}
