test_that("relaciones semanticas respetan limite de oracion", {
  skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  texto <- "La perdida disminuye despues del ajuste. El sistema mejora con la revision. La evidencia muestra una trayectoria estable."
  u <- crear_udtmt_ds(texto, "Que trayectoria presenta el texto?", diccionarios = dic)
  rel <- resumir_relaciones_udtmt_ds(u)$relaciones
  lectura <- paste0(rel$origen, " --", rel$relacion, "--> ", rel$destino, collapse = " | ")
  expect_true(any(rel$origen == "perdida" & rel$relacion == "disminuye" & rel$destino == "ajuste"))
  expect_true(any(rel$origen == "sistema" & rel$relacion == "mejora" & rel$destino == "revision"))
  expect_true(any(rel$origen == "evidencia" & rel$relacion == "muestra" & rel$destino == "trayectoria"))
  expect_false(grepl("revision --evidencia--> muestra", lectura, fixed = TRUE))
})

test_that("relaciones semanticas no generan vinculos sin origen", {
  skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  texto <- "La perdida disminuye despues del ajuste. El sistema mejora con la revision. La evidencia muestra una trayectoria estable."
  u <- crear_udtmt_ds(texto, "Que trayectoria presenta el texto?", diccionarios = dic)
  rel <- resumir_relaciones_udtmt_ds(u)$relaciones
  expect_false(any(is.na(rel$origen)))
  expect_false(any(is.na(rel$destino)))
  expect_false(any(rel$relacion == "evidencia", na.rm = TRUE))
  expect_true(any(rel$origen == "evidencia" & rel$relacion == "muestra" & rel$destino == "trayectoria"))
})
