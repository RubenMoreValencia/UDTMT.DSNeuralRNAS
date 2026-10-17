testthat::test_that('G5-C12 comparative entities preserve identity and positions', {
  tokens <- c('la','recaudacion','de','pab3','supera','en','20','soles',
              'a','la','recaudacion','de','pab2')
  cat <- c('articulo','sustantivo','preposicion',NA,'verbo','conector',NA,
           'sustantivo','preposicion','articulo','sustantivo','preposicion',NA)
  rol <- c('marcador_referencial','evidencia','marcador_referencial',NA,
           'operador_contraste','operador_adicion',NA,'contexto',
           'marcador_referencial','marcador_referencial','evidencia',
           'marcador_referencial',NA)
  a <- data.frame(token=tokens,posicion=seq_along(tokens),
                  categoria_gramatical=cat,rol_funcional=rol,
                  tipo_semantico=c(rep('evidencia',4),'contraste',rep('evidencia',8)),
                  id_oracion=rep(1L,length(tokens)),stringsAsFactors=FALSE)
  r <- UDTMT.DSNeuralRNAS::extraer_relaciones_textuales_udtmt_ds(a)
  q <- r[tolower(r$relacion)=='supera',,drop=FALSE]
  testthat::expect_equal(nrow(q),1L)
  testthat::expect_equal(q$origen,'recaudacion@PAB3')
  testthat::expect_equal(q$destino,'recaudacion@PAB2')
  testthat::expect_equal(q$tipo_relacion,'contrasta')
  testthat::expect_true(grepl('20 soles',q$conector,fixed=TRUE))
})

testthat::test_that('G5-C12 internal punctuation does not split identifier', {
  f <- get('.udtmt_id_oracion_desde_texto',envir=asNamespace('UDTMT.DSNeuralRNAS'))
  tok <- c('en','datos','sinteticos','de','eigm','piura','el','pabellon',
           'registro','ventas','la','recaudacion','supera','saldo')
  text <- paste('En datos sinteticos de EIGM.PIURA, el pabellon registro ventas.',
                'La recaudacion supera saldo.')
  ids <- f(tok,text)
  testthat::expect_equal(length(unique(ids)),2L)
  testthat::expect_equal(ids[5],ids[6])
  testthat::expect_equal(ids[11],ids[14])
})

testthat::test_that('G5-C12 postnominal modifier binds nearest noun', {
  a <- data.frame(token=c('datos','sinteticos','de','eigm','piura'),
                  posicion=1:5,id_oracion=rep(1L,5),
                  categoria_gramatical=c('sustantivo','adjetivo','preposicion',NA,'sustantivo'),
                  rol_funcional=c('evidencia','evidencia','marcador_referencial',NA,'contexto'),
                  tipo_semantico=c('cognicion','evidencia','direccion',NA,'espacio'))
  r <- UDTMT.DSNeuralRNAS::extraer_relaciones_textuales_udtmt_ds(a)
  q <- r[r$relacion=='modula' & r$destino=='sinteticos',,drop=FALSE]
  testthat::expect_true(nrow(q)>0L)
  testthat::expect_true(all(q$origen=='datos'))
})
