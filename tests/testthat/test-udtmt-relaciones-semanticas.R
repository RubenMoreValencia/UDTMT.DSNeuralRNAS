test_that("UDT-MT construye relaciones semanticas trazables", {
  skip_if_not(requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE), "PLN.DSNeuralRNAS no disponible")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  texto <- "La perdida disminuye despues del ajuste. El sistema mejora con la revision."
  u <- crear_udtmt_ds(texto, "Que cambia en el texto?", diccionarios = dic)
  rel <- extraer_relaciones_textuales_udtmt_ds(u)
  expect_true(is.data.frame(rel))
  expect_true(nrow(rel) >= 1)
  expect_true(all(c("origen", "relacion", "destino", "tipo_relacion", "evidencia") %in% names(rel)))
})

test_that("UDT-MT resume y responde desde relaciones", {
  skip_if_not(requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE), "PLN.DSNeuralRNAS no disponible")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  u <- crear_udtmt_ds("La evidencia muestra una trayectoria estable", "Que trayectoria presenta el texto?", diccionarios = dic)
  r <- resumir_relaciones_udtmt_ds(u)
  expect_true(is.list(r))
  expect_true("n_relaciones" %in% names(r))
  resp <- responder_por_relaciones_udtmt_ds(u)
  expect_true(is.character(resp))
  expect_true(nchar(resp) > 0)
})


test_that("UDT-MT no usa conectores como destino semantico", {
  skip_if_not(requireNamespace("PLN.DSNeuralRNAS", quietly = TRUE), "PLN.DSNeuralRNAS no disponible")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  texto <- "La perdida disminuye despues del ajuste. El sistema mejora con la revision. La evidencia muestra una trayectoria estable."
  u <- crear_udtmt_ds(texto, "Que trayectoria presenta el texto?", diccionarios = dic)
  rel <- extraer_relaciones_textuales_udtmt_ds(u)
  expect_true(is.data.frame(rel))
  expect_false(any(rel$destino %in% c("despues", "después", "con", "del", "la", "el"), na.rm = TRUE))
  expect_true(any(rel$destino %in% c("ajuste", "revision", "trayectoria", "estable"), na.rm = TRUE))
})
