test_that("la trayectoria refinada separa contexto y senales", {
  skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  u <- crear_udtmt_ds(
    "La perdida disminuye despues del ajuste y el sistema mejora",
    "Que cambia en el texto?",
    dic
  )
  comp <- clasificar_componentes_udtmt_ds(u)
  expect_true(is.list(comp))
  expect_true("senales_dinamicas" %in% names(comp))
  expect_true("contexto_limpio" %in% names(comp))
  expect_true("flujos_transformacion" %in% names(comp))
  expect_true(any(comp$flujos_transformacion %in% c("disminuye", "mejora")))
  expect_false(any(comp$contexto_limpio %in% c("perdida", "mejora")))
})

test_that("los ejemplos de consola incluyen componentes refinados", {
  skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  res <- ejecutar_ejemplos_udtmt_ds(dic)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("contexto_limpio", "senales_dinamicas", "flujos_transformacion") %in% names(res)))
  expect_equal(nrow(res), 3)
})
