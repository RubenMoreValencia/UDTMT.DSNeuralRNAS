test_that("UDT-MT v0.1.5.6 evita relaciones reflexivas artificiales", {
  skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  u <- crear_udtmt_ds("El control falla. La frontera persiste.", "Que relaciones aparecen?", dic)
  rel <- resumir_relaciones_udtmt_ds(u)$relaciones
  expect_false(any(rel$origen == rel$destino))
  expect_true(any(rel$origen == "control" & rel$relacion == "falla" & rel$destino == "estado_falla"))
  expect_true(any(rel$origen == "frontera" & rel$relacion == "persiste" & rel$destino == "estado_persiste"))
})

test_that("UDT-MT v0.1.5.6 reutiliza sujeto en coordinacion verbal", {
  skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  u <- crear_udtmt_ds("El ajuste reduce el error y mejora el sistema.", "Que cambia?", dic)
  rel <- resumir_relaciones_udtmt_ds(u)$relaciones
  expect_true(any(rel$origen == "ajuste" & rel$relacion == "reduce" & rel$destino == "error"))
  expect_true(any(rel$origen == "ajuste" & rel$relacion == "mejora" & rel$destino == "sistema"))
  expect_false(any(rel$origen == "error" & rel$relacion == "mejora" & rel$destino == "sistema"))
})

test_that("UDT-MT v0.1.5.6 conserva flujos afectivos y origenes emergentes", {
  skip_if_not_installed("PLN.DSNeuralRNAS")
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  u1 <- crear_udtmt_ds("El sosiego calma la memoria. La esperanza transforma el recuerdo.", "Que cambia?", dic)
  rel1 <- resumir_relaciones_udtmt_ds(u1)$relaciones
  expect_true(any(rel1$origen == "sosiego" & rel1$relacion == "calma" & rel1$destino == "memoria"))
  expect_true(any(rel1$origen == "esperanza" & rel1$relacion == "transforma" & rel1$destino == "recuerdo"))

  u2 <- crear_udtmt_ds("El metacauce organiza la memoria del sistema.", "Que relaciones aparecen?", dic)
  rel2 <- resumir_relaciones_udtmt_ds(u2)$relaciones
  expect_true(any(rel2$origen == "metacauce" & rel2$relacion == "organiza" & rel2$destino == "memoria"))
})
