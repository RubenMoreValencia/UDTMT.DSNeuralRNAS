.udtmt_col <- function(x, nombre, defecto = NA_character_) {
  if (!is.data.frame(x) || !(nombre %in% names(x))) return(rep(defecto, ifelse(is.data.frame(x), nrow(x), 0L)))
  as.character(x[[nombre]])
}

.udtmt_es_operador_rel <- function(a) {
  rol <- .udtmt_col(a, "rol_funcional")
  cat <- .udtmt_col(a, "categoria_gramatical")
  rol %in% c("marcador_referencial", "operador_adicion", "operador_causal", "operador_contraste", "inversor_semantico") |
    cat %in% c("articulo", "preposicion", "conector", "pronombre", "determinante")
}

.udtmt_conectores_relacionales <- function() {
  c(
    "despues", "despu\u00e9s", "antes", "durante", "luego", "posterior", "previamente",
    "con", "sin", "por", "para", "hacia", "desde", "en", "entre", "sobre", "bajo",
    "de", "del", "la", "el", "las", "los", "un", "una", "unos", "unas",
    "y", "o", "pero", "aunque", "porque", "debido", "mediante", "segun", "seg\u00fan",
    "que", "se", "al", "coma", "ademas", "adem\u00e1s", "por", "ello", "tanto", "sin", "embargo"
  )
}

.udtmt_es_conector_relacional <- function(token, rol = NA_character_, categoria = NA_character_) {
  tk <- tolower(as.character(token))
  rl <- tolower(as.character(rol))
  ct <- tolower(as.character(categoria))
  tk %in% .udtmt_conectores_relacionales() |
    rl %in% c("marcador_referencial", "operador_adicion", "operador_causal", "operador_contraste", "inversor_semantico") |
    ct %in% c("articulo", "preposicion", "conector", "pronombre", "determinante")
}

.udtmt_es_flujo_por_lexico <- function(token) {
  tk <- tolower(as.character(token))
  tk %in% c(
    "disminuye", "disminuir", "baja", "reduce", "mejora", "mejorar", "cambia", "transforma",
    "ajusta", "activa", "brota", "nace", "conserva", "guarda", "muestra", "sostiene",
    "respalda", "aprende", "fluye", "aumenta", "crece", "emerge",
    "alimenta", "mueve", "organiza", "altera", "persiste", "confirma", "deriva",
    "calma", "falla", "era", "sostiene", "transforma", "indica", "explica"
  )
}

.udtmt_es_determinante_rel <- function(token) {
  tolower(as.character(token)) %in% c("el", "la", "los", "las", "un", "una", "unos", "unas")
}

.udtmt_flujo_ambiguo_nominal <- function(token) {
  tolower(as.character(token)) %in% c("mejora", "calma", "control", "muestra")
}

.udtmt_es_modificador_rel <- function(token, categoria = NA_character_, tipo = NA_character_, rol = NA_character_) {
  tk <- tolower(as.character(token))
  ct <- tolower(as.character(categoria))
  tp <- tolower(as.character(tipo))
  rl <- tolower(as.character(rol))
  ct %in% c("adjetivo", "adverbio") |
    tp %in% c("intensidad", "estado", "cualidad") |
    rl %in% c("modulador", "contexto", "estado_final") |
    tk %in% c("estable", "nueva", "nuevo", "emergente", "intermedia", "intermedio",
              "progresiva", "progresivo", "aprendida", "aprendido", "alta", "alto")
}

.udtmt_tokenizar_oracion <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^[:alnum:]\u00e1\u00e9\u00ed\u00f3\u00fa\u00f1\u00fc\u00c1\u00c9\u00cd\u00d3\u00da\u00d1\u00dc]+", " ", x, perl = TRUE)
  out <- unlist(strsplit(trimws(x), "\\s+"), use.names = FALSE)
  out[!is.na(out) & nzchar(out)]
}

.udtmt_id_oracion_desde_texto <- function(tokens, texto = NULL) {
  tokens <- tolower(as.character(tokens))
  n <- length(tokens)
  if (!n) return(integer(0))
  if (is.null(texto) || length(texto) == 0L || is.na(texto) || !nzchar(as.character(texto))) {
    return(rep(1L, n))
  }
  # G5-C12: internal dots in identifiers are not sentence boundaries.
  # Insert a space (not a sentence stop), preserving token alignment.
  bruto <- gsub("([[:alnum:]])\\.([[:alnum:]])", "\\1 \\2", as.character(texto), perl = TRUE)
  oraciones <- unlist(strsplit(bruto, "[.!?;]+\\s*", perl = TRUE), use.names = FALSE)
  oraciones <- oraciones[!is.na(oraciones) & nzchar(trimws(oraciones))]
  if (!length(oraciones)) return(rep(1L, n))

  sec_tokens <- character(0)
  sec_oracion <- integer(0)
  for (j in seq_along(oraciones)) {
    tk <- .udtmt_tokenizar_oracion(oraciones[j])
    if (length(tk)) {
      sec_tokens <- c(sec_tokens, tk)
      sec_oracion <- c(sec_oracion, rep.int(j, length(tk)))
    }
  }
  if (!length(sec_tokens)) return(rep(1L, n))

  # Alineacion prudente: los analizadores PLN suelen conservar el orden de tokens.
  # Si algun token no coincide por normalizacion, se asigna la oracion secuencial disponible.
  out <- rep(1L, n)
  k <- 1L
  for (i in seq_len(n)) {
    if (k > length(sec_tokens)) {
      out[i] <- tail(sec_oracion, 1)
      next
    }
    actual <- tokens[i]
    if (!is.na(actual) && nzchar(actual)) {
      busc <- which(sec_tokens[k:length(sec_tokens)] == actual)
      if (length(busc)) {
        k2 <- k + busc[1] - 1L
        out[i] <- sec_oracion[k2]
        k <- k2 + 1L
      } else {
        out[i] <- sec_oracion[k]
        k <- k + 1L
      }
    } else {
      out[i] <- sec_oracion[k]
      k <- k + 1L
    }
  }
  out
}

.udtmt_tokens_contenido <- function(a) {
  if (!nrow(a)) return(a[FALSE, , drop = FALSE])
  op <- .udtmt_es_operador_rel(a)
  a[!op & !is.na(a$token) & nzchar(as.character(a$token)), , drop = FALSE]
}

.udtmt_pegar_evidencia <- function(origen, relacion, conector, destino) {
  x <- c(origen, relacion, conector, destino)
  x <- x[!is.na(x) & nzchar(x)]
  paste(unique(x), collapse = " ")
}

.udtmt_tipo_relacion <- function(relacion, conector = NA_character_, tipo = NA_character_, rol = NA_character_) {
  r <- tolower(paste(relacion, tipo, rol, conector, collapse = " "))
  if (grepl("despues|antes|durante|luego|era", r)) return("secuencia_temporal")
  if (grepl("porque|causa|debido|por", r)) return("causa_apoya")
  if (grepl("pero|aunque|sin embargo|contraste", r)) return("contrasta")
  if (grepl("muestra|evidencia|sostiene|respalda|conserva|guarda|confirma|indica|explica", r)) return("sostiene")
  if (grepl("activa|brota|nace|enciende|alimenta|mueve", r)) return("activa")
  if (grepl("disminuye|mejora|cambia|transforma|ajusta|reduce|aumenta|altera|organiza|deriva|calma|flujo_transformacion|transformacion", r)) return("transforma")
  if (grepl("contexto|naturaleza|espacio", r)) return("contextualiza")
  "relacion_semantica"
}

#' Extraer relaciones textuales UDT-MT
#'
#' Construye relaciones semanticas trazables desde una matriz PLN o desde una
#' unidad UDT-MT. La relacion se expresa como origen, relacion, destino,
#' conector, tipo de relacion y evidencia textual minima.
#'
#' @details
#' Esta funcion inicia la capa relacional de UDT-MT. No usa atencion profunda ni
#' entrenamiento masivo. Las relaciones se obtienen por posicion, rol funcional,
#' tipo semantico, conectores y flujos textuales. La finalidad es pasar de una
#' lectura basada en tokens a una lectura basada en vinculos interpretables.
#'
#' En esta version se incorporan reglas prudentes para coordinacion verbal,
#' verbos intransitivos y origenes provisionales no registrados. Esto evita
#' relaciones reflexivas artificiales y permite relaciones como
#' ajuste--mejora-->sistema, control--falla-->estado_falla y
#' metacauce--organiza-->memoria.
#'
#' @param x Unidad UDT-MT o data frame de analisis textual PLN.
#' @param trayectoria Trayectoria refinada. Se usa cuando `x` es data frame.
#' @param texto Texto original opcional. Permite inferir limites de oracion y evita relaciones espurias entre oraciones independientes.
#' @return Data frame de relaciones textuales trazables.
#' @examples
#' if (requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE)) {
#'   dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
#'   u <- crear_udtmt_ds("La perdida disminuye despues del ajuste", "Que cambia?", dic)
#'   extraer_relaciones_textuales_udtmt_ds(u)
#' }
#' @export
extraer_relaciones_textuales_udtmt_ds <- function(x, trayectoria = NULL, texto = NULL) {
  if (inherits(x, "udtmt_ds") || (is.list(x) && !is.null(x$analisis_texto))) {
    a <- x$analisis_texto
    trayectoria <- x$trayectoria
    if (is.null(texto) && !is.null(x$texto_original)) texto <- x$texto_original
  } else {
    a <- x
  }
  vacio <- function() {
    data.frame(
      id_relacion = character(0), origen = character(0), relacion = character(0), destino = character(0),
      conector = character(0), tipo_relacion = character(0), evidencia = character(0),
      posicion_origen = integer(0), posicion_relacion = integer(0), posicion_destino = integer(0),
      confianza = character(0), stringsAsFactors = FALSE
    )
  }
  if (!is.data.frame(a) || !nrow(a)) return(vacio())

  token <- .udtmt_col(a, "token")
  lema <- .udtmt_col(a, "lema")
  rol <- .udtmt_col(a, "rol_funcional")
  tipo <- .udtmt_col(a, "tipo_semantico")
  cat <- .udtmt_col(a, "categoria_gramatical")
  pos <- if ("posicion" %in% names(a)) as.integer(a$posicion) else seq_len(nrow(a))
  id_oracion <- if ("id_oracion" %in% names(a)) as.integer(a$id_oracion) else .udtmt_id_oracion_desde_texto(token, texto)
  op <- .udtmt_es_operador_rel(a)
  conector_rel <- .udtmt_es_conector_relacional(token, rol, cat)

  # Un flujo relacional no debe confundirse con un stock de estado.
  # Version 0.1.5.6.2: se evita que roles como stock_estado sean tratados
  # como verbos/relaciones por contener la palabra "estado".
  rol_low <- tolower(rol)
  tipo_low <- tolower(tipo)
  cat_low <- tolower(cat)
  es_flujo <- grepl("^flujo", rol_low) |
    rol_low %in% c("accion", "flujo_transformacion", "flujo_textual", "flujo_estado",
                   "flujo_afectivo", "flujo_evidencial") |
    tipo_low %in% c("accion") | cat_low == "verbo" |
    .udtmt_es_flujo_por_lexico(token)

  previo_inmediato <- c(NA_character_, token[-length(token)])
  es_flujo <- es_flujo & !( .udtmt_flujo_ambiguo_nominal(token) & .udtmt_es_determinante_rel(previo_inmediato) )

    # F4.1-B: comparative verbs are relational predicates, not discourse connectors.
  comparativo_verbal <- cat_low == "verbo" & rol_low == "operador_contraste"
  comparativo_verbal[is.na(comparativo_verbal)] <- FALSE
  op[comparativo_verbal] <- FALSE
  conector_rel[comparativo_verbal] <- FALSE
  idx_flujo <- which(es_flujo & !op & !conector_rel & !is.na(token) & nzchar(token))
  contenido_idx <- which(!op & !is.na(token) & nzchar(token))
  contenido_semantico_idx <- contenido_idx[!conector_rel[contenido_idx]]
  relaciones <- list()
  ultimo_origen_por_oracion <- list()
  ultima_rel_pos_por_oracion <- list()

  agregar_relacion <- function(origen_i, rel_i, destino_i = NA_integer_, destino_sintetico = NA_character_, conector = NA_character_) {
    origen_i <- if (length(origen_i) && !is.na(origen_i)) as.integer(origen_i[1]) else NA_integer_
    rel_i <- if (length(rel_i) && !is.na(rel_i)) as.integer(rel_i[1]) else NA_integer_
    destino_i <- if (length(destino_i) && !is.na(destino_i)) as.integer(destino_i[1]) else NA_integer_
    destino_sintetico <- if (length(destino_sintetico) && !is.na(destino_sintetico) && nzchar(destino_sintetico)) as.character(destino_sintetico[1]) else NA_character_
    conector <- if (length(conector) && !is.na(conector) && nzchar(conector)) as.character(conector[1]) else NA_character_
    if (is.na(rel_i)) return(FALSE)
    origen <- if (!is.na(origen_i)) token[origen_i] else NA_character_
    destino <- if (!is.na(destino_i)) token[destino_i] else destino_sintetico
    rel <- token[rel_i]
    if (is.na(origen) || !nzchar(origen) || is.na(destino) || !nzchar(destino)) return(FALSE)
    if (identical(tolower(origen), tolower(destino))) return(FALSE)
    ya <- vapply(relaciones, function(z) {
      identical(as.character(z$origen), origen) && identical(as.character(z$relacion), rel) && identical(as.character(z$destino), destino)
    }, logical(1))
    if (length(ya) && any(ya)) return(FALSE)
    tipo_rel <- .udtmt_tipo_relacion(rel, conector, tipo[rel_i], rol[rel_i])
    relaciones[[length(relaciones) + 1L]] <<- data.frame(
      id_relacion = paste0("R", length(relaciones) + 1L),
      origen = origen,
      relacion = rel,
      destino = destino,
      conector = conector,
      tipo_relacion = tipo_rel,
      evidencia = .udtmt_pegar_evidencia(origen, rel, conector, destino),
      posicion_origen = if (!is.na(origen_i)) pos[origen_i] else NA_integer_,
      posicion_relacion = pos[rel_i],
      posicion_destino = if (!is.na(destino_i)) pos[destino_i] else NA_integer_,
      confianza = if (is.na(destino_i)) "media_intransitiva" else "media",
      stringsAsFactors = FALSE
    )
    key <- as.character(id_oracion[rel_i])
    ultimo_origen_por_oracion[[key]] <<- origen_i
    ultima_rel_pos_por_oracion[[key]] <<- rel_i
    TRUE
  }

  for (i in idx_flujo) {
    misma_oracion_idx <- contenido_semantico_idx[id_oracion[contenido_semantico_idx] == id_oracion[i]]
    prev <- misma_oracion_idx[misma_oracion_idx < i]
    siguiente_idx <- misma_oracion_idx[misma_oracion_idx > i]

    prev_origen <- prev[!es_flujo[prev] | cat_low[prev] %in% c("sustantivo", "nombre")]
    origen_i <- if (length(prev_origen)) tail(prev_origen, 1) else NA_integer_
    origen_por_coordinacion <- FALSE
    key <- as.character(id_oracion[i])
    if (!is.null(ultimo_origen_por_oracion[[key]]) && !is.null(ultima_rel_pos_por_oracion[[key]])) {
      inter_coord <- seq(ultima_rel_pos_por_oracion[[key]] + 1L, i - 1L)
      inter_coord <- inter_coord[inter_coord >= 1L & inter_coord <= length(token)]
      if (length(inter_coord) && any(tolower(token[inter_coord]) %in% c("y", "e"), na.rm = TRUE)) {
        origen_i <- ultimo_origen_por_oracion[[key]]
        origen_por_coordinacion <- TRUE
      }
    }
    if (!origen_por_coordinacion && !is.na(origen_i) && tolower(cat[origen_i]) %in% c("adjetivo", "adverbio") && length(prev_origen) >= 2L) {
      origen_i <- tail(prev_origen[-length(prev_origen)], 1)
    }

    siguiente_destino <- siguiente_idx[!es_flujo[siguiente_idx] & !tolower(cat[siguiente_idx]) %in% c("adjetivo", "adverbio")]
    destino_i <- if (length(siguiente_destino)) head(siguiente_destino, 1) else NA_integer_
    destino_sintetico <- NA_character_
    if (is.na(destino_i) && !is.na(origen_i) && tolower(token[i]) %in% c("falla", "persiste", "emerge", "mejora", "aumenta", "disminuye", "calma")) {
      destino_sintetico <- paste0("estado_", tolower(token[i]))
    }

    inter <- if (!is.na(destino_i) && destino_i > i + 1L) seq(i + 1L, destino_i - 1L) else integer(0)
    conector <- NA_character_
    if (length(inter)) {
      cand <- token[inter]
      cand <- cand[!is.na(cand) & nzchar(cand)]
      if (length(cand)) conector <- paste(cand, collapse = " ")
    }

    invisible(agregar_relacion(origen_i, i, destino_i, destino_sintetico, conector))
  }

  # Fallback prudente: si algun flujo contextual no produjo relacion, se intenta
  # una lectura sujeto--flujo-->objeto dentro de la misma oracion. Esto ayuda a
  # no perder origenes provisionales no registrados como metacauce.
  pos_rel_usadas <- if (length(relaciones)) vapply(relaciones, function(z) as.integer(z$posicion_relacion), integer(1)) else integer(0)
  for (i in idx_flujo[!(pos[i] %in% pos_rel_usadas)]) {
    misma_oracion_idx <- contenido_semantico_idx[id_oracion[contenido_semantico_idx] == id_oracion[i]]
    prev <- misma_oracion_idx[misma_oracion_idx < i]
    nexts <- misma_oracion_idx[misma_oracion_idx > i]
    origen_i <- if (length(prev)) {
      cand_origen <- prev[!es_flujo[prev] | cat_low[prev] %in% c("sustantivo", "nombre")]
      if (length(cand_origen)) tail(cand_origen, 1) else NA_integer_
    } else NA_integer_
    nexts_destino <- nexts[!es_flujo[nexts]]
    destino_i <- if (length(nexts_destino)) head(nexts_destino, 1) else NA_integer_
    if (length(origen_i) == 1L && !is.na(origen_i) &&
        origen_i >= 1L && origen_i <= length(cat) &&
        !is.na(cat[origen_i]) &&
        tolower(cat[origen_i]) %in% c("adjetivo", "adverbio")) {
      pp <- prev[prev < origen_i & !es_flujo[prev]]
      if (length(pp)) origen_i <- tail(pp, 1)
    }
    destino_sintetico <- NA_character_
    if (is.na(destino_i) && !is.na(origen_i) && tolower(token[i]) %in% c("falla", "persiste", "emerge", "mejora")) {
      destino_sintetico <- paste0("estado_", tolower(token[i]))
    }
    invisible(agregar_relacion(origen_i, i, destino_i, destino_sintetico, NA_character_))
  }

  # Refuerzo SVO contextual: conserva relaciones sujeto--flujo-->objeto
  # cuando el analisis lexical deja el sujeto como stock, frontera o termino
  # emergente. Esto evita perder casos como sosiego--calma-->memoria,
  # esperanza--transforma-->recuerdo y metacauce--organiza-->memoria.
  pos_rel_usadas2 <- if (length(relaciones)) vapply(relaciones, function(z) as.integer(z$posicion_relacion), integer(1)) else integer(0)
  flujos_contextuales <- c("calma", "transforma", "organiza", "activa", "deriva",
                           "reduce", "mejora", "alimenta", "mueve", "altera",
                           "conserva", "sostiene", "muestra", "confirma")
  for (i in which(tolower(token) %in% flujos_contextuales)) {
    if (pos[i] %in% pos_rel_usadas2) next
    misma_oracion_idx <- contenido_semantico_idx[id_oracion[contenido_semantico_idx] == id_oracion[i]]
    prev <- misma_oracion_idx[misma_oracion_idx < i]
    nexts <- misma_oracion_idx[misma_oracion_idx > i]
    if (!length(prev) || !length(nexts)) next
    origen_cand <- prev[!conector_rel[prev] & !.udtmt_es_determinante_rel(token[prev])]
    origen_cand <- origen_cand[!tolower(cat[origen_cand]) %in% c("adjetivo", "adverbio")]
    destino_cand <- nexts[!conector_rel[nexts] & !.udtmt_es_determinante_rel(token[nexts])]
    destino_cand <- destino_cand[!tolower(cat[destino_cand]) %in% c("adjetivo", "adverbio")]
    if (!length(origen_cand) || !length(destino_cand)) next
    origen_i <- tail(origen_cand, 1)
    destino_i <- head(destino_cand, 1)
    invisible(agregar_relacion(origen_i, i, destino_i, NA_character_, NA_character_))
  }

  idx_mod <- which((cat_low %in% c("adjetivo", "adverbio") |
                     tolower(rol) %in% c("modulador", "modulador_dinamico", "modulador_intensidad", "modulador_afectivo")) &
                     !op & !conector_rel & !es_flujo & !is.na(token) & nzchar(token))
  for (i in idx_mod) {
    misma_oracion_idx <- contenido_semantico_idx[id_oracion[contenido_semantico_idx] == id_oracion[i]]
    prev <- misma_oracion_idx[misma_oracion_idx < i]
    nexts <- misma_oracion_idx[misma_oracion_idx > i]

    # Si el modificador antecede inmediatamente a un sustantivo o contenido no flujo
    # antes de cualquier nuevo flujo, se asocia con ese contenido posterior:
    # "nueva estrategia" -> estrategia --modula--> nueva.
    next_contenido <- nexts[!es_flujo[nexts] & !conector_rel[nexts] &
                             !.udtmt_es_determinante_rel(token[nexts]) &
                             !tolower(cat[nexts]) %in% c("adjetivo", "adverbio")]
    next_contenido <- next_contenido[!is.na(next_contenido)]
    next_flujo <- nexts[es_flujo[nexts]]
    next_flujo <- next_flujo[!is.na(next_flujo)]
    origen_i <- NA_integer_
    usar_siguiente <- length(next_contenido) > 0L &&
      (length(next_flujo) == 0L || isTRUE(head(next_contenido, 1) < head(next_flujo, 1)))
    # G5-C12: in "datos sinteticos", the preceding adjacent noun is the head.
    prev_noun <- prev[tolower(cat[prev]) %in% c("sustantivo", "nombre")]
    prev_adjacent <- length(prev_noun) && tail(prev_noun, 1L) == i - 1L
    if (isTRUE(prev_adjacent)) {
      origen_i <- tail(prev_noun, 1L)
    } else if (usar_siguiente) {
      origen_i <- head(next_contenido, 1)
    } else if (length(prev)) {
      # Permite que un sustantivo de contenido actue como origen del modificador
      # aunque haya sido marcado por reglas dinamicas. Evita perder casos como
      # transformacion --modula--> estable.
      prev_origen <- prev[((!es_flujo[prev]) | tolower(cat[prev]) %in% c("sustantivo", "nombre")) &
                            !conector_rel[prev] & !.udtmt_es_determinante_rel(token[prev])]
      if (length(prev_origen)) origen_i <- tail(prev_origen, 1)
    }
    if (is.na(origen_i)) next
    if (identical(tolower(token[origen_i]), tolower(token[i]))) next
    relaciones[[length(relaciones) + 1L]] <- data.frame(
      id_relacion = paste0("R", length(relaciones) + 1L),
      origen = token[origen_i],
      relacion = "modula",
      destino = token[i],
      conector = NA_character_,
      tipo_relacion = "modula",
      evidencia = .udtmt_pegar_evidencia(token[origen_i], "modula", NA_character_, token[i]),
      posicion_origen = pos[origen_i],
      posicion_relacion = pos[i],
      posicion_destino = pos[i],
      confianza = "media",
      stringsAsFactors = FALSE
    )
  }


  # Refuerzo v0.1.5.6.5: si un sustantivo fue destino de una relacion
  # principal y el token siguiente funciona como modificador posnominal, se
  # conserva la modulacion: transformacion --modula--> estable.
  if (length(relaciones)) {
    destinos_rel <- vapply(relaciones, function(z) {
      if (is.null(z$posicion_destino) || is.na(z$posicion_destino)) NA_integer_ else as.integer(z$posicion_destino)
    }, integer(1))
    for (pd in destinos_rel[!is.na(destinos_rel)]) {
      origen_i <- which(pos == pd)[1]
      if (is.na(origen_i) || !length(origen_i)) next
      if (!(tolower(cat[origen_i]) %in% c("sustantivo", "nombre") ||
            tolower(tipo[origen_i]) %in% c("transformacion", "estado", "concepto", "proceso", "naturaleza"))) next
      misma_oracion_idx <- contenido_semantico_idx[id_oracion[contenido_semantico_idx] == id_oracion[origen_i]]
      cand <- misma_oracion_idx[misma_oracion_idx > origen_i]
      cand <- cand[tolower(cat[cand]) %in% c("adjetivo", "adverbio") |
                   tolower(rol[cand]) %in% c("modulador", "modulador_dinamico", "modulador_intensidad", "modulador_afectivo")]
      cand <- cand[!es_flujo[cand] & !conector_rel[cand]]
      if (!length(cand)) next
      mod_i <- head(cand[!is.na(cand)], 1)
      if (!length(mod_i) || is.na(mod_i)) next
      # No cruzar hacia otra relacion verbal posterior. La comparacion se
      # protege contra NA para que los ejemplos internos no fallen cuando
      # existan tokens sin posicion relacional v\u00e1lida.
      flujo_posterior <- misma_oracion_idx[misma_oracion_idx > origen_i & es_flujo[misma_oracion_idx]]
      flujo_posterior <- flujo_posterior[!is.na(flujo_posterior)]
      if (length(flujo_posterior) > 0L && isTRUE(head(flujo_posterior, 1) < mod_i)) next
      existe <- any(vapply(relaciones, function(z) {
        identical(as.character(z$origen), as.character(token[origen_i])) &&
          identical(as.character(z$relacion), "modula") &&
          identical(as.character(z$destino), as.character(token[mod_i]))
      }, logical(1)))
      if (existe) next
      relaciones[[length(relaciones) + 1L]] <- data.frame(
        id_relacion = paste0("R", length(relaciones) + 1L),
        origen = token[origen_i],
        relacion = "modula",
        destino = token[mod_i],
        conector = NA_character_,
        tipo_relacion = "modula",
        evidencia = .udtmt_pegar_evidencia(token[origen_i], "modula", NA_character_, token[mod_i]),
        posicion_origen = pos[origen_i],
        posicion_relacion = pos[mod_i],
        posicion_destino = pos[mod_i],
        confianza = "media",
        stringsAsFactors = FALSE
      )
    }
  }

  # G5-C12 candidate comparator: sentence-local verbal comparisons.
  # Does not compute or verify amounts. Existing public columns are retained.
  compare_verbs <- c("supera", "superan", "excede", "exceden",
                     "sobrepasa", "sobrepasan", "iguala", "igualan")
  compare_idx <- which(tolower(token) %in% compare_verbs &
                       tolower(cat) %in% c("verbo", "verbal"))
  for (ci in compare_idx) {
    misma <- which(!is.na(id_oracion) & id_oracion == id_oracion[ci])
    prev <- misma[misma < ci]
    post <- misma[misma > ci]
    if (!length(prev) || !length(post)) next
    # A comparison object follows the preposition "a" when available.
    a_post <- post[tolower(token[post]) == "a"]
    if (length(a_post)) post <- post[post > a_post[1L]]
    nouns_before <- prev[tolower(cat[prev]) %in% c("sustantivo", "nombre")]
    nouns_after <- post[tolower(cat[post]) %in% c("sustantivo", "nombre")]
    # Never mistake currency units immediately following a number for object.
    nouns_after <- nouns_after[!vapply(nouns_after, function(j) {
      j > 1L && grepl("^[0-9]+([.,][0-9]+)?$", token[j - 1L])
    }, logical(1))]
    if (!length(nouns_before) || !length(nouns_after)) next
    oi <- tail(nouns_before, 1L)
    di <- nouns_after[1L]
    # Skip same-surface reflexives only when token positions are actually equal.
    if (oi == di) next
    # Optional qualifier: "recaudacion de PAB3" / "recaudacion de PAB2".
    qualifier <- function(j, end) {
      if (j + 2L <= end && tolower(token[j + 1L]) == "de" &&
          !is.na(token[j + 2L]) && nzchar(token[j + 2L])) {
        return(paste0(token[j], "@", toupper(token[j + 2L])))
      }
      token[j]
    }
    origen_c <- qualifier(oi, ci - 1L)
    destino_c <- qualifier(di, max(post))
    # Record the mentioned comparison magnitude as a connector, not as money truth.
    inter <- seq.int(ci + 1L, di - 1L)
    inter <- inter[inter > ci & inter < di]
    evidencia_comp <- paste(token[seq.int(oi, max(di, di +
      if (grepl("@", destino_c, fixed=TRUE)) 2L else 0L))], collapse=" ")
    conector_comp <- if (length(inter)) paste(token[inter], collapse=" ") else NA_character_
    relaciones[[length(relaciones) + 1L]] <- data.frame(
      id_relacion = paste0("R", length(relaciones) + 1L),
      origen = origen_c, relacion = token[ci], destino = destino_c,
      conector = conector_comp, tipo_relacion = "contrasta",
      evidencia = evidencia_comp, posicion_origen = pos[oi],
      posicion_relacion = pos[ci], posicion_destino = pos[di],
      confianza = "media", stringsAsFactors = FALSE
    )
  }
  if (!length(relaciones)) return(vacio())
  out <- do.call(rbind, relaciones)

  # Depuracion final: elimina relaciones incompletas y duplicados de la misma
  # relacion/destino. Si dos relaciones comparten relacion y destino, se conserva
  # la de origen mas temprano, que suele representar el sujeto coordinado correcto
  # (ajuste --mejora--> trayectoria antes que error --mejora--> trayectoria).
  out <- out[!is.na(out$origen) & nzchar(out$origen) & !is.na(out$destino) & nzchar(out$destino), , drop = FALSE]
  if (nrow(out)) {
    ord <- order(out$posicion_relacion, out$destino, out$posicion_origen, na.last = TRUE)
    out <- out[ord, , drop = FALSE]
    clave <- paste(out$posicion_relacion, out$relacion, out$destino, sep = "||")
    out <- out[!duplicated(clave), , drop = FALSE]

    # Si una relacion SVO ya encontro un destino nominal, se elimina la version
    # redundante cuyo destino es un modificador prenominal. Ejemplo:
    # revision --activa--> estrategia se conserva y revision --activa--> nueva
    # se descarta, dejando estrategia --modula--> nueva.
    if (nrow(out)) {
      eliminar <- rep(FALSE, nrow(out))
      for (rr in seq_len(nrow(out))) {
        pd <- out$posicion_destino[rr]
        if (is.na(pd)) next
        idx_dest <- which(pos == pd)
        if (!length(idx_dest)) next
        destino_es_mod <- tolower(cat[idx_dest[1]]) %in% c("adjetivo", "adverbio")
        if (!destino_es_mod) next
        pares <- which(out$origen == out$origen[rr] &
                         out$relacion == out$relacion[rr] &
                         out$posicion_relacion == out$posicion_relacion[rr] &
                         seq_len(nrow(out)) != rr)
        if (!length(pares)) next
        pares_nominales <- pares[vapply(out$posicion_destino[pares], function(pp) {
          if (is.na(pp)) return(FALSE)
          ii <- which(pos == pp)
          length(ii) && !(tolower(cat[ii[1]]) %in% c("adjetivo", "adverbio"))
        }, logical(1))]
        if (length(pares_nominales)) eliminar[rr] <- TRUE
      }
      out <- out[!eliminar, , drop = FALSE]
    }

    # Refuerzo final v0.1.5.6.8: despues de depurar duplicados, se recupera
    # la modulacion posnominal inmediata cuando el destino de una relacion
    # principal tiene un modificador a continuacion. Esto evita perder casos
    # como transformacion --modula--> estable en "evidencia confirma una
    # transformacion estable", incluso si el modificador no quedo dentro de
    # contenido_semantico_idx por alguna decision previa del analizador.
    if (nrow(out)) {
      extras <- list()
      for (rr in seq_len(nrow(out))) {
        pd <- out$posicion_destino[rr]
        if (is.na(pd)) next
        origen_idx <- which(pos == pd)
        if (!length(origen_idx)) next
        origen_idx <- origen_idx[1]
        if (!(tolower(cat[origen_idx]) %in% c("sustantivo", "nombre") ||
              tolower(tipo[origen_idx]) %in% c("transformacion", "estado", "concepto", "proceso", "naturaleza"))) next

        misma <- which(id_oracion == id_oracion[origen_idx])
        cand <- misma[pos[misma] > pos[origen_idx]]
        cand <- cand[!is.na(cand)]
        if (!length(cand)) next

        # Tomar el primer token posterior no determinante ni conector. Si es
        # modificador, se crea la relacion de modulacion. Se evita cruzar otro
        # flujo verbal antes del modificador.
        cand <- cand[!.udtmt_es_determinante_rel(token[cand]) & !conector_rel[cand]]
        cand <- cand[!is.na(cand)]
        if (!length(cand)) next
        mod_i <- cand[1]
        if (!length(mod_i) || is.na(mod_i)) next
        if (isTRUE(es_flujo[mod_i])) next
        if (!(tolower(cat[mod_i]) %in% c("adjetivo", "adverbio") |
              tolower(rol[mod_i]) %in% c("modulador", "modulador_dinamico", "modulador_intensidad", "modulador_afectivo"))) next

        flujo_intermedio <- misma[pos[misma] > pos[origen_idx] & pos[misma] < pos[mod_i] & es_flujo[misma]]
        flujo_intermedio <- flujo_intermedio[!is.na(flujo_intermedio)]
        if (length(flujo_intermedio)) next

        existe <- any(out$origen == token[origen_idx] &
                       out$relacion == "modula" &
                       out$destino == token[mod_i], na.rm = TRUE)
        if (existe) next

        extras[[length(extras) + 1L]] <- data.frame(
          id_relacion = paste0("R", nrow(out) + length(extras) + 1L),
          origen = token[origen_idx],
          relacion = "modula",
          destino = token[mod_i],
          conector = NA_character_,
          tipo_relacion = "modula",
          evidencia = .udtmt_pegar_evidencia(token[origen_idx], "modula", NA_character_, token[mod_i]),
          posicion_origen = pos[origen_idx],
          posicion_relacion = pos[mod_i],
          posicion_destino = pos[mod_i],
          confianza = "media",
          stringsAsFactors = FALSE
        )
      }
      if (length(extras)) {
        out <- rbind(out, do.call(rbind, extras))
      }
    }


    # Rescate final v0.1.5.6.10: si un destino nominal/conceptual queda
    # inmediatamente seguido por un modificador posterior, se agrega la
    # modulacion aunque el tipo/categoria del sustantivo no haya sido
    # reconocido como nombre por el analizador. Esta regla es deliberadamente
    # local: no cruza conectores, determinantes ni flujos verbales.
    if (nrow(out)) {
      extras_rescate <- list()
      destinos_pos <- unique(out$posicion_destino[!is.na(out$posicion_destino)])
      for (pd in destinos_pos) {
        origen_idx <- which(pos == pd)
        origen_idx <- origen_idx[!is.na(origen_idx)]
        if (!length(origen_idx)) next
        origen_idx <- origen_idx[1]
        if (is.na(origen_idx) || .udtmt_es_determinante_rel(token[origen_idx])) next
        if (isTRUE(conector_rel[origen_idx])) next

        misma <- which(id_oracion == id_oracion[origen_idx])
        misma <- misma[!is.na(misma) & !is.na(pos[misma])]
        cand <- misma[pos[misma] > pos[origen_idx]]
        cand <- cand[!is.na(cand)]
        if (!length(cand)) next

        cand <- cand[!(.udtmt_es_determinante_rel(token[cand]))]
        cand <- cand[!(conector_rel[cand] %in% TRUE)]
        cand <- cand[!(es_flujo[cand] %in% TRUE)]
        cand <- cand[!is.na(cand)]
        if (!length(cand)) next

        mod_i <- cand[1]
        if (!length(mod_i) || is.na(mod_i)) next
        if (!(tolower(cat[mod_i]) %in% c("adjetivo", "adverbio") |
              tolower(rol[mod_i]) %in% c("modulador", "modulador_dinamico", "modulador_intensidad", "modulador_afectivo"))) next

        flujo_intermedio <- misma[pos[misma] > pos[origen_idx] & pos[misma] < pos[mod_i] & (es_flujo[misma] %in% TRUE)]
        flujo_intermedio <- flujo_intermedio[!is.na(flujo_intermedio)]
        if (length(flujo_intermedio)) next

        existe <- any(out$origen == token[origen_idx] &
                       out$relacion == "modula" &
                       out$destino == token[mod_i], na.rm = TRUE)
        existe_extra <- any(vapply(extras_rescate, function(z) {
          identical(as.character(z$origen), as.character(token[origen_idx])) &&
            identical(as.character(z$relacion), "modula") &&
            identical(as.character(z$destino), as.character(token[mod_i]))
        }, logical(1)))
        if (existe || existe_extra) next

        extras_rescate[[length(extras_rescate) + 1L]] <- data.frame(
          id_relacion = paste0("R", nrow(out) + length(extras_rescate) + 1L),
          origen = token[origen_idx],
          relacion = "modula",
          destino = token[mod_i],
          conector = NA_character_,
          tipo_relacion = "modula",
          evidencia = .udtmt_pegar_evidencia(token[origen_idx], "modula", NA_character_, token[mod_i]),
          posicion_origen = pos[origen_idx],
          posicion_relacion = pos[mod_i],
          posicion_destino = pos[mod_i],
          confianza = "media",
          stringsAsFactors = FALSE
        )
      }
      if (length(extras_rescate)) {
        out <- rbind(out, do.call(rbind, extras_rescate))
      }
    }

    out$id_relacion <- paste0("R", seq_len(nrow(out)))
  }
  rownames(out) <- NULL
  out
}

#' Clasificar relaciones semanticas UDT-MT
#'
#' Resume una matriz de relaciones por tipo relacional.
#'
#' @param relaciones Data frame producido previamente por la funcion extraer_relaciones_textuales_udtmt_ds.
#' @return Lista con conteos y relaciones.
#' @export
clasificar_relaciones_semanticas_udtmt_ds <- function(relaciones) {
  if (inherits(relaciones, "udtmt_ds") || (is.list(relaciones) && !is.null(relaciones$analisis_texto))) {
    relaciones <- extraer_relaciones_textuales_udtmt_ds(relaciones)
  }
  if (!is.data.frame(relaciones) || !nrow(relaciones)) {
    return(list(n = 0L, por_tipo = data.frame(), relaciones = data.frame()))
  }
  por_tipo <- as.data.frame(table(relaciones$tipo_relacion), stringsAsFactors = FALSE)
  names(por_tipo) <- c("tipo_relacion", "n")
  list(n = nrow(relaciones), por_tipo = por_tipo, relaciones = relaciones)
}

#' Construir matriz relacional UDT-MT
#'
#' Devuelve la matriz de relaciones semanticas trazables de una unidad o de una
#' matriz PLN.
#'
#' @param x Unidad UDT-MT o data frame de analisis textual PLN.
#' @param trayectoria Trayectoria opcional.
#' @param texto Texto original opcional para respetar limites de oracion.
#' @return Data frame de relaciones.
#' @export
construir_matriz_relacional_udtmt_ds <- function(x, trayectoria = NULL, texto = NULL) {
  extraer_relaciones_textuales_udtmt_ds(x, trayectoria, texto = texto)
}

#' Resumir relaciones UDT-MT
#'
#' Produce un resumen textual y tabular de relaciones semanticas trazables.
#'
#' @param unidad Unidad UDT-MT.
#' @return Lista con conteo, tipos y lectura.
#' @export
resumir_relaciones_udtmt_ds <- function(unidad) {
  relaciones <- if (!is.null(unidad$relaciones_semanticas)) unidad$relaciones_semanticas else extraer_relaciones_textuales_udtmt_ds(unidad)
  clas <- clasificar_relaciones_semanticas_udtmt_ds(relaciones)
  lectura <- if (!nrow(relaciones)) {
    "No se identificaron relaciones semanticas suficientes."
  } else {
    pares <- paste0(relaciones$origen, " --", relaciones$relacion, "--> ", relaciones$destino)
    paste(unique(pares), collapse = " | ")
  }
  list(n_relaciones = nrow(relaciones), por_tipo = clas$por_tipo, lectura = lectura, relaciones = relaciones)
}

#' Responder por relaciones UDT-MT
#'
#' Genera una respuesta breve usando la matriz relacional semantica de una unidad.
#'
#' @param unidad Unidad UDT-MT.
#' @return Cadena de respuesta basada en relaciones.
#' @export
responder_por_relaciones_udtmt_ds <- function(unidad) {
  r <- resumir_relaciones_udtmt_ds(unidad)
  if (!r$n_relaciones) return("No se identificaron relaciones semanticas suficientes para responder desde vinculos textuales.")
  tipos <- if (nrow(r$por_tipo)) paste(r$por_tipo$tipo_relacion, collapse = ", ") else "relaciones semanticas"
  paste0("La lectura relacional identifica ", r$n_relaciones, " relaciones trazables de tipo ", tipos, ". Relaciones principales: ", r$lectura, ".")
}
