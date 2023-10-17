base <- c(
  "#82C473", "#A378C0", "#616161", "#9F5C61", "#D3D280", "#6970B2", "#78C4C2", "#454744", "#D98C82"
)

test_that("base cases work", {
  expect_identical(splot.color(), base)
  expect_true(!all(splot.color(rep(1, 5), method = "related") == splot.color(rep(1, 5))))
  expect_identical(splot.color(c(1, 1), by = 1:2), base[1:2])
  expect_identical(splot.color(c(1, 1), by = 1:2, flat = FALSE), list(`1` = base[1], `2` = base[2]))
  expect_identical(
    unname(splot.color(list(c(1, 1), c(1, 1)), method = "none")),
    rep(base[1:2], each = 2)
  )
})

test_that("scale works", {
  initial <- splot.color(1:5)
  expect_identical(order(-colMeans(col2rgb(initial))), seq_len(5))
  expect_identical(
    order(-colMeans(col2rgb(initial))),
    order(colMeans(col2rgb(splot.color(1:5, decreasing = TRUE))))
  )
  expect_true(!all(
    initial == vapply(1:10, function(i) splot.color(1:5, shuffle = TRUE), character(5))
  ))
  expect_true(all(initial %in% splot.color(1:5, shuffle = TRUE)))
})
