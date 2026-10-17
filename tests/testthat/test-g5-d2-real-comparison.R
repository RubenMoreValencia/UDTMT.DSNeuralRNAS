# G5-D2 regression: preserve distinct participants in a real PLN-derived fixture.
# Fixture is created in EIGM.PIURA and copied to tests/testthat/fixtures/.

testthat::test_that(
  "comparative relations preserve distinct nominal participants",
  {
    fixture_path <- testthat::test_path(
      "fixtures", "G5_D2_fixture_comparativo.rds"
    )

    testthat::expect_true(file.exists(fixture_path))
    if (!file.exists(fixture_path)) {
      testthat::skip("G5-D2 fixture has not been copied from EIGM.PIURA")
    }

    fixture <- readRDS(fixture_path)
    a3 <- fixture$analisis
    texto3 <- fixture$texto

    testthat::expect_equal(nrow(a3), 13L)
    testthat::expect_equal(as.character(a3$token[5]), "supera")
    testthat::expect_equal(as.integer(a3$posicion[5]), 29L)

    r <- UDTMT.DSNeuralRNAS::extraer_relaciones_textuales_udtmt_ds(
      a3, texto = texto3
    )

    compara <- r[
      !is.na(r$relacion) & tolower(r$relacion) == "supera",
      , drop = FALSE
    ]

    testthat::expect_equal(nrow(compara), 1L)
    testthat::expect_equal(compara$origen, "recaudacion@PAB3")
    testthat::expect_equal(compara$destino, "recaudacion@PAB2")
    testthat::expect_equal(as.integer(compara$posicion_relacion), 29L)
  }
)
