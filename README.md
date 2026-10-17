# UDTMT 0.1.5.6.12 — G5-C12 candidate

Experimental R-only patch to apply on a **copy/branch** of local UDTMT 0.1.5.6.11. Preserve the installed stable version. The patch does **not** modify DESCRIPTION and does **not** install anything.

1. Open the local package source root in RStudio. Create a Git branch or a full package backup first.
2. Run `source("/path/to/UDTMT_G5_C12/scripts/01_apply_G5_C12.R", encoding="UTF-8")`.
3. Copy `tests/testthat/test-g5-c12-comparison.R` to the package `tests/testthat` folder.
4. Run `devtools::load_all(".")`, `testthat::test_file("tests/testthat/test-g5-c12-comparison.R")`, `devtools::test(".")`.
5. If all pass, update DESCRIPTION to 0.1.5.6.12 in development copy, add NEWS, run `devtools::document(".")` and `devtools::check(".", args="--no-manual")`.
6. Repeat EIGM G5 A/B with the same corpus. Validate both the comparison and the modifier.

Important: regression tests are **not run in the assistant environment** (no R executable available); they are acceptance tests for your RStudio runtime. This version does not verify monetary arithmetic. The comparison connector stores the *textually mentioned* phrase such as 'en 20 soles a la'.

Restore `R/09_relaciones_semanticas_udtmt.R.pre_G5_C12.bak` if any test fails. The script stops when expected source anchors do not match.

Revision R2: version check normalizes the named DCF scalar with unname(as.character()).

R3: removed an overly strict, unused anchor for idx_mod. The patch still checks all actual mutation points before writing and protects the original with a backup.
