# Copy to tests/testthat/test-f41-relations.R in UDTMT source tree.
# Test dictionary is constructed reproducibly using the upstream public API.
f41_dictionary <- function() {
  d <- Lexico.DSNeuralRNAS::cargar_diccionarios_ds()
  lote <- Lexico.DSNeuralRNAS::plantilla_lote_lexico_ds(c('pago','recaudacion','saldo','supera'))
  # Reuse source schema, avoid loading EIGM-specific RDS.
  rows <- which(lote$palabra %in% c('pago','recaudacion','saldo','supera'))
  lote$categoria_gramatical[rows] <- c('sustantivo','sustantivo','sustantivo','verbo')[match(lote$palabra[rows],c('pago','recaudacion','saldo','supera'))]
  lote$tipo_semantico[rows] <- c('accion','evidencia','estado','contraste')[match(lote$palabra[rows],c('pago','recaudacion','saldo','supera'))]
  lote$rol_funcional[rows] <- c('flujo_accion','evidencia','stock_estado','operador_contraste')[match(lote$palabra[rows],c('pago','recaudacion','saldo','supera'))]
  lote$tipo_dinamico[rows] <- c('flujo_textual','componente_textual','stock_informativo','operador_textual')[match(lote$palabra[rows],c('pago','recaudacion','saldo','supera'))]
  lote$flujo_asociado[lote$palabra=='supera'] <- 'operador_contraste'
  lote$observacion <- 'udtmt_f41_regression_fixture'
  lote <- lote[!lote$palabra %in% d$lexico$palabra,,drop=FALSE]
  if (!nrow(lote)) return(d)
  v <- Lexico.DSNeuralRNAS::validar_lote_lexico_ds(lote,diccionarios=d)
  testthat::expect_true(isTRUE(v$ok))
  out <- Lexico.DSNeuralRNAS::integrar_lote_lexico_ds(diccionarios=d,lote=lote)
  testthat::expect_true(isTRUE(out$validacion$ok_global))
  out$diccionarios
}
testthat::test_that('action nominal subjects do not crash and give correct relation', {
  d <- f41_dictionary()
  a <- PLN.DSNeuralRNAS::analizar_texto_pln('El pago transforma el estado.',diccionarios=d)
  r <- extraer_relaciones_textuales_udtmt_ds(a)
  testthat::expect_true(is.data.frame(r))
  testthat::expect_true(any(r$origen=='pago' & r$relacion=='transforma' & r$destino=='estado',na.rm=TRUE))
})
testthat::test_that('comparative verb is extracted, not just a nominal modulation', {
  d <- f41_dictionary()
  a <- PLN.DSNeuralRNAS::analizar_texto_pln('La recaudacion supera el saldo.',diccionarios=d)
  r <- extraer_relaciones_textuales_udtmt_ds(a)
  testthat::expect_true(any(r$relacion=='supera',na.rm=TRUE))
  testthat::expect_false(any(r$relacion=='modula' & r$origen=='recaudacion' & r$destino=='saldo',na.rm=TRUE))
})
testthat::test_that('verbal change is retained without unsupported nominal modulation', {
  d <- f41_dictionary()
  a <- PLN.DSNeuralRNAS::analizar_texto_pln('La recaudacion aumenta el saldo.',diccionarios=d)
  r <- extraer_relaciones_textuales_udtmt_ds(a)
  testthat::expect_true(any(r$origen=='recaudacion' & r$relacion=='aumenta' & r$destino=='saldo',na.rm=TRUE))
  testthat::expect_false(any(r$relacion=='modula' & r$origen=='recaudacion' & r$destino=='saldo',na.rm=TRUE))
})
