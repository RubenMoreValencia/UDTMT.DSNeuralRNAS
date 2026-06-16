test_that("crear_udtmt_ds acepta alias diccionarios", {
  skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  u <- crear_udtmt_ds(
    texto = "La calma llega despues de la lluvia",
    pregunta = "Que cambia en el texto?",
    diccionarios = dic
  )
  expect_s3_class(u, "udtmt_ds")
  expect_false(u$es_lstm)
  expect_equal(u$arquitectura, "UDT-MT DSNeuralRNAS")
})

test_that("ejemplos funcionales devuelven tres casos", {
  skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  res <- ejecutar_ejemplos_udtmt_ds(dic)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_true(all(c("memoria_textual", "naturaleza_contexto", "transformacion_textual") %in% res$caso))
  expect_true(all(res$n_tokens > 0))
  expect_true(all(res$n_stocks > 0))
  expect_true(all(nchar(res$respuesta) > 0))
})
