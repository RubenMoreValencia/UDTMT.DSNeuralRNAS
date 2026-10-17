# UDTMT F4.1-B patch R2 - run ONLY from package source root.
# Does not modify installed libraries or external ERP project.
stopifnot(file.exists("DESCRIPTION"), dir.exists("R"))
d <- read.dcf("DESCRIPTION", fields = c("Package", "Version"))
stopifnot(identical(unname(d[1,"Package"]), "UDTMT.DSNeuralRNAS"))
if (!identical(unname(d[1,"Version"]), "0.1.5.6.10")) {
  stop("Expected source version 0.1.5.6.10; refusing to patch.")
}
file <- "R/09_relaciones_semanticas_udtmt.R"
stopifnot(file.exists(file))
x <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
# Exact-match preflight: the local source must contain the audited constructs.
changes <- list(
  c('idx_flujo <- which(es_flujo & !op & !conector_rel & !is.na(token) & nzchar(token))',
    paste0('  # F4.1-B: comparative verbs are relational predicates, not discourse connectors.\n',
           '  comparativo_verbal <- cat_low == "verbo" & rol_low == "operador_contraste"\n',
           '  comparativo_verbal[is.na(comparativo_verbal)] <- FALSE\n',
           '  op[comparativo_verbal] <- FALSE\n',
           '  conector_rel[comparativo_verbal] <- FALSE\n',
           '  idx_flujo <- which(es_flujo & !op & !conector_rel & !is.na(token) & nzchar(token))')),
  c('prev_origen <- prev[!es_flujo[prev]]',
    'prev_origen <- prev[!es_flujo[prev] | cat_low[prev] %in% c("sustantivo", "nombre")]'),
  c('origen_i <- if (length(prev)) tail(prev[!es_flujo[prev]], 1) else NA_integer_',
    'origen_i <- if (length(prev)) {\n      cand_origen <- prev[!es_flujo[prev] | cat_low[prev] %in% c("sustantivo", "nombre")]\n      if (length(cand_origen)) tail(cand_origen, 1) else NA_integer_\n    } else NA_integer_'),
  c('if (!is.na(origen_i) && tolower(cat[origen_i]) %in% c("adjetivo", "adverbio")) {',
    'if (length(origen_i) == 1L && !is.na(origen_i) &&\n        origen_i >= 1L && origen_i <= length(cat) &&\n        !is.na(cat[origen_i]) &&\n        tolower(cat[origen_i]) %in% c("adjetivo", "adverbio")) {'),
  c('idx_mod <- which(.udtmt_es_modificador_rel(token, cat, tipo, rol) &',
    'idx_mod <- which((cat_low %in% c("adjetivo", "adverbio") |\n                     tolower(rol) %in% c("modulador", "modulador_dinamico", "modulador_intensidad", "modulador_afectivo")) &'),
  c('cand <- cand[.udtmt_es_modificador_rel(token[cand], cat[cand], tipo[cand], rol[cand])]',
    'cand <- cand[tolower(cat[cand]) %in% c("adjetivo", "adverbio") |\n                   tolower(rol[cand]) %in% c("modulador", "modulador_dinamico", "modulador_intensidad", "modulador_afectivo")]'),
  c('if (!.udtmt_es_modificador_rel(token[mod_i], cat[mod_i], tipo[mod_i], rol[mod_i])) next',
    'if (!(tolower(cat[mod_i]) %in% c("adjetivo", "adverbio") |\n              tolower(rol[mod_i]) %in% c("modulador", "modulador_dinamico", "modulador_intensidad", "modulador_afectivo"))) next'),
  c('if (!( .udtmt_es_modificador_rel(token[mod_i], cat[mod_i], tipo[mod_i], rol[mod_i]) %in% TRUE)) next',
    'if (!(tolower(cat[mod_i]) %in% c("adjetivo", "adverbio") |\n              tolower(rol[mod_i]) %in% c("modulador", "modulador_dinamico", "modulador_intensidad", "modulador_afectivo"))) next')
)
# Accommodate the original spacing of the final rescue condition.
last <- length(changes)
variants <- c(
  'if (!( .udtmt_es_modificador_rel(token[mod_i], cat[mod_i], tipo[mod_i], rol[mod_i]) %in% TRUE)) next',
  'if (!(.udtmt_es_modificador_rel(token[mod_i], cat[mod_i], tipo[mod_i], rol[mod_i]) %in% TRUE)) next'
)
found <- variants[vapply(variants, function(z) grepl(z, x, fixed = TRUE), logical(1))]
if (length(found) == 1L) changes[[last]][1] <- found
counts <- vapply(changes, function(p) {
  hits <- gregexpr(p[1], x, fixed = TRUE)[[1]]
  if (identical(hits[1], -1L)) 0L else length(hits)
}, integer(1))
# The local copy may differ: abort *before* writing anything.
if (any(counts != 1L)) {
  print(data.frame(change = seq_along(changes), occurrences = counts))
  stop("Source differs from audited file; no changes applied. Provide the source file for an adapted patch.")
}
backup <- paste0(file, ".f41_prepatch_0.1.5.6.10.bak")
if (file.exists(backup)) stop("Backup already exists; refusing to overwrite it.")
stopifnot(file.copy(file, backup, overwrite = FALSE))
for (p in changes) x <- sub(p[1], p[2], x, fixed = TRUE)
writeLines(x, con = file, useBytes = TRUE)
tryCatch(parse(file = file), error = function(e) {
  file.copy(backup, file, overwrite = TRUE)
  stop("R syntax failed; original file restored: ", conditionMessage(e))
})
cat("PATCH APPLIED to local SOURCE only. Backup:", backup, "\n")
cat("Version unchanged pending tests and check.\n")
