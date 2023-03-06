test_that("basic example work", {
  res <- splot.colorcontrast(c("#ffffff", "#666666", "#444444"))
  expect_identical(as.logical(res$AA), c(FALSE, TRUE, TRUE))
  expect_identical(as.logical(res$AAA), c(FALSE, FALSE, TRUE))
})
