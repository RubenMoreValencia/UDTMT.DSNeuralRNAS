# UDTMT F4.1-B, candidate patch 0.1.5.6.11
# Run from the ROOT of a clean, disposable UDTMT.DSNeuralRNAS source checkout.
# This script deliberately refuses to modify an installed package or an unexpected baseline.
stopifnot(file.exists('DESCRIPTION'), dir.exists('R'))
d <- read.dcf('DESCRIPTION')
stopifnot(identical(unname(d[1,'Package']), 'UDTMT.DSNeuralRNAS'))
if (!identical(unname(d[1,'Version']), '0.1.5.6.10')) stop('Baseline version is not 0.1.5.6.10; inspect before patching.')
if (dir.exists('.git') && system2('git', c('diff','--quiet'), stdout=FALSE, stderr=FALSE) != 0L) {
  stop('Working tree has uncommitted changes. Use a clean branch or clean copy.')
}
files <- list.files('R', pattern='\\.[Rr]$', recursive=TRUE, full.names=TRUE)
containing <- files[vapply(files, function(f) any(grepl('extraer_relaciones_textuales_udtmt_ds[[:space:]]*<-[[:space:]]*function', readLines(f, warn=FALSE))), logical(1))]
if (length(containing) != 1L) stop('Expected exactly one original extractor definition in R source; found: ', length(containing))
file <- containing[[1]]
x <- paste(readLines(file,warn=FALSE,encoding='UTF-8'), collapse='\n')
# All substitutions are local to original source and require exactly ONE occurrence.
replace_one <- function(x,old,new,label) {
  where <- gregexpr(old,x,fixed=TRUE)[[1]]
  if (length(where)!=1L || where[1] < 0L) stop('Source anchor not unique: ',label,'. No file has been written.')
  sub(old,new,x,fixed=TRUE)
}
# Protect empty/non-scalar candidate indices in both extraction routes.
old_guard <- 'if (!is.na(origen_i) && tolower(cat[origen_i]) %in% c("adjetivo", "adverbio"))'
new_guard <- 'if (length(origen_i) == 1L && !is.na(origen_i) && origen_i >= 1L && origen_i <= length(cat) && !is.na(cat[origen_i]) && tolower(cat[origen_i]) %in% c("adjetivo", "adverbio"))'
x <- replace_one(x,old_guard,new_guard,'scalar-origin guard')
# Candidate nouns are not removed merely because their semantic class is action.
old_prev <- 'prev_origen <- prev[!es_flujo[prev]]'
new_prev <- 'prev_origen <- prev[!es_flujo[prev] | tolower(cat[prev]) %in% c("sustantivo", "nombre")]'
x <- replace_one(x,old_prev,new_prev,'nominal-origin selection')
# Comparative VERBS are still operators but must be allowed through the verbal relation route.
old_idx <- 'idx_flujo <- which(es_flujo & !op & !conector_rel & !is.na(token) & nzchar(token))'
new_idx <- 'comparativo_verbal <- tolower(cat) == "verbo" & tolower(rol) == "operador_contraste"\n  comparativo_verbal[is.na(comparativo_verbal)] <- FALSE\n  idx_flujo <- which(es_flujo & (!op | comparativo_verbal) & (!conector_rel | comparativo_verbal) & !is.na(token) & nzchar(token))'
x <- replace_one(x,old_idx,new_idx,'verbal-comparison route')
# Existing false-positive: an ordinary substantive is not a modifier just because of its semantic role.
old_mod <- 'idx_mod <- which(.udtmt_es_modificador_rel(token, cat, tipo, rol) & !op & !conector_rel & !es_flujo & !is.na(token) & nzchar(token))'
new_mod <- 'idx_mod <- which(.udtmt_es_modificador_rel(token, cat, tipo, rol) & tolower(cat) %in% c("adjetivo", "adverbio") & !op & !conector_rel & !es_flujo & !is.na(token) & nzchar(token))'
x <- replace_one(x,old_mod,new_mod,'modifier qualification')
# Defensive check: parse candidate before mutating any files.
parse(text=x,keep.source=FALSE)
# Existing package may contain other source occurrences: these are intentional, narrowly targeted anchors.
writeLines(x,file,useBytes=TRUE)
# DESCRIPTION bump, preserving package metadata.
desc <- readLines('DESCRIPTION',warn=FALSE)
desc <- sub('^Version: 0\\.1\\.5\\.6\\.10$', 'Version: 0.1.5.6.11',desc)
if (!any(desc=='Version: 0.1.5.6.11')) stop('Could not update DESCRIPTION')
writeLines(desc,'DESCRIPTION',useBytes=TRUE)
cat('SOURCE PATCH APPLIED:',file,'\nCANDIDATE VERSION: 0.1.5.6.11\nValidation has NOT been run.\n')
