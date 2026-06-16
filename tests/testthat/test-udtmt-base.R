test_that("catalogo y verificacion UDT-MT funcionan", {
  cat <- catalogo_udtmt_ds()
  expect_true(is.data.frame(cat))
  expect_true(nrow(cat) >= 5)
  expect_true("stock_informativo" %in% cat$componente)

  ver <- verificar_udtmt_ds()
  expect_true(is.list(ver))
  expect_true("ok" %in% names(ver))
})

test_that("se crean stocks y flujos desde una matriz minima", {
  a <- data.frame(
    token = c("calma", "llega", "despues"),
    lema = c("calma", "llegar", "despues"),
    rol_funcional = c("stock_afectivo", "flujo_accion", "transicion_temporal"),
    tipo_dinamico = c("stock_informativo", "flujo_textual", "operador_textual"),
    peso_semantico = c("alto", "alto", "medio"),
    peso_funcional = c("medio", "alto", "alto"),
    evidencia = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  s <- crear_stock_textual_ds(a)
  f <- crear_flujo_textual_ds(a)
  expect_true(nrow(s) >= 1)
  expect_true(nrow(f) >= 2)
  expect_true(any(s$token == "calma"))
  expect_true(any(f$token == "llega"))

  s2 <- actualizar_stock_textual_ds(s, f)
  expect_true("activacion_actualizada" %in% names(s2))
})

test_that("memorias UDT-MT funcionan con datos minimos", {
  a <- data.frame(token = c("calma", "llega"), registrado = c(TRUE, TRUE), evidencia = c(TRUE, TRUE))
  s <- data.frame(token = "calma")
  f <- data.frame(token = "llega")
  mt <- registrar_memoria_textual_ds(a, s, f)
  expect_equal(mt$n_tokens, 2)
  expect_equal(mt$n_stocks, 1)

  p <- data.frame(
    pregunta_normalizada = "que cambia",
    intencion = "transformacion",
    modulo_activado = "modulo_dinamico_textual",
    tipo_respuesta = "trayectoria",
    nivel_dificultad = "medio",
    stringsAsFactors = FALSE
  )
  mp <- registrar_memoria_pregunta_ds(p)
  expect_equal(mp$intencion, "transformacion")

  e <- data.frame(token = c("llega", "disminuye"), lema = c("llegar", "disminuir"), rol_funcional = c("flujo_accion", "flujo_transformacion"))
  me <- registrar_memoria_evidencia_ds(e)
  expect_equal(me$n, 2)
})
