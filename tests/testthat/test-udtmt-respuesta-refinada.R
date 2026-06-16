test_that("la respuesta refinada usa senales dinamicas y no contexto heredado", {
  skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  u <- crear_udtmt_ds(
    "La perdida disminuye despues del ajuste y el sistema mejora",
    "Que cambia en el texto?",
    diccionarios = dic
  )
  resumen <- resumir_udtmt_ds(u)
  expect_true(grepl("senales dinamicas", resumen$respuesta))
  expect_false(grepl("contexto aparece en perdida", resumen$respuesta))
  expect_true(grepl("proceso dinamico interno|contexto limpio", resumen$respuesta))
})

test_that("los ejemplos por consola devuelven respuesta refinada", {
  skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  res <- ejecutar_ejemplos_udtmt_ds(dic)
  fila <- res[res$caso == "transformacion_textual", , drop = FALSE]
  expect_equal(nrow(fila), 1)
  expect_true(grepl("senales dinamicas", fila$respuesta))
  expect_false(grepl("contexto aparece en perdida", fila$respuesta))
})
