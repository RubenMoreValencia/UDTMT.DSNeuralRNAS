# UDTMT F4.1-B — local patch candidate R2

This is a **source patch candidate**, not a validated package release. Run from the ROOT of your existing local R package. It requires DESCRIPTION Version 0.1.5.6.10 and exact original source blocks. If it does not match, it stops without changing anything. No GitHub download is required.

1. Keep an independent backup or commit your source branch. Place `01_apply_patch_local.R` in any convenient folder.
2. In RStudio, set working directory to the package root, then run `source("/full/path/01_apply_patch_local.R", encoding="UTF-8")`.
3. The source file is backed up alongside the original as `.f41_prepatch_0.1.5.6.10.bak`.
4. Put the existing F4.1 tests under `tests/testthat/test-f41-relations.R` (NOT directly in `tests/`).
5. Run `devtools::load_all("."); testthat::test_file("tests/testthat/test-f41-relations.R"); devtools::test(".")`.
6. Only after all functional and regression tests pass, update the version **candidate** to `0.1.5.6.11`, regenerate roxygen documentation, and run `devtools::check(".")`. Separate pre-existing check hygiene issues (hello.Rd, imports, LICENSE, filenames, non-ASCII) from semantic fixes.

The changes target: nominal action-origin selection; `supera` as a comparative verbal predicate; NA scalar condition; restricting unsubstantiated `modula` relations. The semantic scope and regression impact require testing against the local source.

If the script refuses to apply because the local source differs, do not replace it with code downloaded elsewhere: return the occurrence counts and relevant original file for a tailored patch.
