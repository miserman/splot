test_that("basic example work", {
  res <- splot.bench(Sys.sleep(.01), Sys.sleep(.1), runsize = 1)
  expect_true(res$summary[3, 1] < res$summary[3, 2])
})
