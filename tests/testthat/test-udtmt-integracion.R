test_that("se crea una unidad UDT-MT usando PLN.DSNeuralRNAS", {
  testthat::skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  texto <- "La calma llega despues de las lluvias y la perdida disminuye"
  pregunta <- "Que cambia en el texto?"
  u <- crear_udtmt_ds(texto, pregunta, dic)

  expect_true(inherits(u, "udtmt_ds"))
  expect_false(u$es_lstm)
  expect_true(nrow(u$stocks) >= 1)
  expect_true(nrow(u$flujos) >= 1)
  expect_true(length(u$ruta_udtmt) >= 5)

  r <- resumir_udtmt_ds(u)
  expect_equal(r$arquitectura, "UDT-MT DSNeuralRNAS")
  expect_true(r$n_tokens >= 1)
  expect_true(r$n_stocks >= 1)
  expect_true(r$n_flujos >= 1)

  tr <- trazar_udtmt_ds(u)
  expect_true(is.list(tr))
  expect_true("salida_adtr_preparada" %in% names(tr))
})

test_that("trayectoria UDT-MT conserva ruta conceptual", {
  testthat::skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  tr <- construir_trayectoria_udtmt_ds("La calma llega despues de la lluvia", "Que cambia?", dic)
  expect_true(is.list(tr))
  expect_true("trayectoria_pln" %in% names(tr))
  expect_true("ruta_udtmt" %in% names(tr))
  expect_true("stocks_informativos" %in% tr$ruta_udtmt)
})
