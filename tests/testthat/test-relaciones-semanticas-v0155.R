test_that("UDT-MT v0.1.5.5 detecta flujos relacionales ampliados", {
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  u <- crear_udtmt_ds("La lluvia alimenta el rio. El viento mueve la pradera.", "Que relaciones aparecen?", dic)
  r <- resumir_relaciones_udtmt_ds(u)$lectura
  expect_match(r, "lluvia --alimenta--> rio")
  expect_match(r, "viento --mueve--> pradera")
})

test_that("UDT-MT evita origen artificial desde verbos", {
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  u <- crear_udtmt_ds("El indicador muestra una mejora progresiva.", "Que evidencia textual sostiene la respuesta?", dic)
  rel <- resumir_relaciones_udtmt_ds(u)$relaciones
  expect_false(any(rel$origen == "muestra" & rel$relacion == "mejora"))
  expect_true(any(rel$origen == "indicador" & rel$relacion == "muestra" & rel$destino == "mejora"))
})

test_that("UDT-MT usa sustantivo como origen cuando hay modificador previo", {
  dic <- PLN.DSNeuralRNAS::cargar_diccionarios_pln()
  u <- crear_udtmt_ds("La senal intermedia activa una frontera nueva.", "Que cambia?", dic)
  r <- resumir_relaciones_udtmt_ds(u)$lectura
  expect_match(r, "senal --activa--> frontera")
  expect_match(r, "senal --modula--> intermedia")
})
