# Candidate source patch for UDTMT.DSNeuralRNAS 0.1.5.6.12
# Run from package root on a development copy. No Git operations or installation.
stopifnot(file.exists('DESCRIPTION'), file.exists('R/09_relaciones_semanticas_udtmt.R'))
ver <- trimws(unname(as.character(read.dcf('DESCRIPTION')[1, 'Version'])))
if (length(ver) != 1L || is.na(ver) || ver != '0.1.5.6.11') stop('Expected source version 0.1.5.6.11; found ', ver)
source_file <- 'R/09_relaciones_semanticas_udtmt.R'
x <- readLines(source_file, warn=FALSE, encoding='UTF-8')
needle <- function(pattern, label) {
  idx <- grep(pattern, x, fixed=TRUE)
  if (length(idx)!=1L) stop('Source anchor missing or ambiguous: ',label,'; matches=',length(idx))
  idx
}
if (any(grepl('G5-C12 candidate comparator', x, fixed=TRUE))) stop('Patch already applied')
# Fail before making any modifications if source does not match expected shape.
needle('  bruto <- as.character(texto)', 'sentence input')
needle('  if (!length(relaciones)) return(vacio())', 'empty relation return')
# No modifier declaration anchor required: the patch edits its decision block only.
needle('    if (usar_siguiente) {', 'modifier prioritization')
needle('    if (is.na(origen_i)) next', 'modifier origin protection')
backup <- paste0(source_file, '.pre_G5_C12.bak')
if (file.exists(backup)) stop('Backup already exists: ', backup)
stopifnot(file.copy(source_file, backup, overwrite=FALSE))
replace_once <- function(old, new) {
  j <- grep(old,x,fixed=TRUE)
  if(length(j)!=1L) stop('Cannot replace anchor uniquely: ',old)
  x <<- append(x[-j], new, after=j-1L)
}
replace_once('  bruto <- as.character(texto)', c(
 '  # G5-C12: internal dots in identifiers are not sentence boundaries.',
 '  # Insert a space (not a sentence stop), preserving token alignment.',
 '  bruto <- gsub("([[:alnum:]])\\\\.([[:alnum:]])", "\\\\1 \\\\2", as.character(texto), perl = TRUE)'))
# Critical: preserve prior modifier-before-noun behavior, but prefer adjacent noun before adjective.
replace_once('    if (usar_siguiente) {', c(
 '    # G5-C12: in "datos sinteticos", the preceding adjacent noun is the head.',
 '    prev_noun <- prev[tolower(cat[prev]) %in% c("sustantivo", "nombre")]',
 '    prev_adjacent <- length(prev_noun) && tail(prev_noun, 1L) == i - 1L',
 '    if (isTRUE(prev_adjacent)) {',
 '      origen_i <- tail(prev_noun, 1L)',
 '    } else if (usar_siguiente) {'))
# Insert a comparator pass ahead of existing empty short-circuit.
comparison <- c(
 '  # G5-C12 candidate comparator: sentence-local verbal comparisons.',
 '  # Does not compute or verify amounts. Existing public columns are retained.',
 '  compare_verbs <- c("supera", "superan", "excede", "exceden",',
 '                     "sobrepasa", "sobrepasan", "iguala", "igualan")',
 '  compare_idx <- which(tolower(token) %in% compare_verbs &',
 '                       tolower(cat) %in% c("verbo", "verbal"))',
 '  for (ci in compare_idx) {',
 '    misma <- which(!is.na(id_oracion) & id_oracion == id_oracion[ci])',
 '    prev <- misma[misma < ci]',
 '    post <- misma[misma > ci]',
 '    if (!length(prev) || !length(post)) next',
 '    # A comparison object follows the preposition "a" when available.',
 '    a_post <- post[tolower(token[post]) == "a"]',
 '    if (length(a_post)) post <- post[post > a_post[1L]]',
 '    nouns_before <- prev[tolower(cat[prev]) %in% c("sustantivo", "nombre")]',
 '    nouns_after <- post[tolower(cat[post]) %in% c("sustantivo", "nombre")]',
 '    # Never mistake currency units immediately following a number for object.',
 '    nouns_after <- nouns_after[!vapply(nouns_after, function(j) {',
 '      j > 1L && grepl("^[0-9]+([.,][0-9]+)?$", token[j - 1L])',
 '    }, logical(1))]',
 '    if (!length(nouns_before) || !length(nouns_after)) next',
 '    oi <- tail(nouns_before, 1L)',
 '    di <- nouns_after[1L]',
 '    # Skip same-surface reflexives only when token positions are actually equal.',
 '    if (oi == di) next',
 '    # Optional qualifier: "recaudacion de PAB3" / "recaudacion de PAB2".',
 '    qualifier <- function(j, end) {',
 '      if (j + 2L <= end && tolower(token[j + 1L]) == "de" &&',
 '          !is.na(token[j + 2L]) && nzchar(token[j + 2L])) {',
 '        return(paste0(token[j], "@", toupper(token[j + 2L])))',
 '      }',
 '      token[j]',
 '    }',
 '    origen_c <- qualifier(oi, ci - 1L)',
 '    destino_c <- qualifier(di, max(post))',
 '    # Record the mentioned comparison magnitude as a connector, not as money truth.',
 '    inter <- seq.int(ci + 1L, di - 1L)',
 '    inter <- inter[inter > ci & inter < di]',
 '    evidencia_comp <- paste(token[seq.int(oi, max(di, di +',
 '      if (grepl("@", destino_c, fixed=TRUE)) 2L else 0L))], collapse=" ")',
 '    conector_comp <- if (length(inter)) paste(token[inter], collapse=" ") else NA_character_',
 '    relaciones[[length(relaciones) + 1L]] <- data.frame(',
 '      id_relacion = paste0("R", length(relaciones) + 1L),',
 '      origen = origen_c, relacion = token[ci], destino = destino_c,',
 '      conector = conector_comp, tipo_relacion = "contrasta",',
 '      evidencia = evidencia_comp, posicion_origen = pos[oi],',
 '      posicion_relacion = pos[ci], posicion_destino = pos[di],',
 '      confianza = "media", stringsAsFactors = FALSE',
 '    )',
 '  }'
)
replace_once('  if (!length(relaciones)) return(vacio())', c(comparison, '  if (!length(relaciones)) return(vacio())'))
# Do not edit DESCRIPTION until tests confirm the candidate.
invisible(parse(text=x))
writeLines(x, source_file, useBytes=TRUE)
cat('G5-C12 PATCH APPLIED; backed up:',backup,'\n')
cat('Source version remains:',read.dcf('DESCRIPTION')[1,'Version'],'\n')
