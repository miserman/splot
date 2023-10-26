n <- 2000
d <- data.frame(sapply(c("c1", "c2", "c3"), function(c) sample(0:1, n, TRUE)))
d$v1 <- with(
  d,
  rnorm(n) + c1 * -.4 + c1 * c2 * -.3 + c1 * c3 *
    .3 + c2 * c3 * .9 - .8 + rnorm(n, 0, c1)
)
d$v2 <- with(
  d,
  v1 * .2 + c1 * .3 + c3 * -.6 + c2 * c3 * .8 + v1 *
    c1 * c2 * -.5 + v1 * c1 * c2 * c3 * -.5
    + rnorm(n, 5) + rnorm(n, -1, .1 * v1^2)
)

test_that("density works", {
  expect_warning(splot(d$v1, type = "scatter"), "x must be included")
  expect_true(splot(v1 ~ c1, d, type = "d")$ck$t == 2)

  res <- splot(d$v1)
  expect_true(res$ck$t == 2)
  expect_identical(res$cdat$`.^^.`$y, d$v1)
  expect_identical(res$dat, splot(v1, d)$dat)

  res <- splot(v1, d, by = c1)
  expect_true(res$ck$t == 2)
  expect_true(res$lega$title == "c1")

  res <- splot(v1, d, by = c1, between = c2)
  expect_true(res$ck$t == 2)
  expect_identical(as.numeric(res$seg$n), as.numeric(table(d$c2)))

  res <- splot(v1, d, by = c1, between = c(c2, c3))
  expect_true(res$ck$t == 2)
  expect_identical(as.numeric(res$seg$n), as.numeric(table(d$c2, d$c3)))
})

test_that("line/bar works", {
  expect_true(splot(v1 ~ c1, d, type = "d")$ck$t == 2)

  res <- splot(d$v1 ~ d$v2, type = "l")
  expect_true(res$ck$t == 1)
  expect_identical(unname(res$ptxt$l.x), c("Under Median", "Over Median"))
  expect_identical(
    unname(splot(d$v1 ~ d$v2, type = "b", split = "mean")$ptxt$l.x),
    c("Below Average", "Above Average")
  )

  res <- splot(d$v1 ~ d$c1)
  expect_true(res$ck$t == 1)
  expect_identical(res$cdat$`.^^.`, data.frame(y = d$v1, x = as.factor(d$c1)))
  expect_identical(res$dat, splot(v1 ~ c1, d)$dat)

  res <- splot(v1 ~ c1, d, by = c2, model = TRUE)
  expect_identical(names(res$fmod$coefficients), c("(Intercept)", "c1", "c2", "c1:c2"))
  expect_true(res$ck$t == 1)
  expect_true(res$lega$title == "c2")
  expect_identical(res$dat, splot(v1 ~ c1 * c2, d)$dat)

  res <- splot(v1 ~ c1, d, by = c2, between = c3)
  expect_true(res$ck$t == 1)
  expect_identical(as.numeric(res$seg$n), as.numeric(table(d$c3)))
  expect_identical(res$dat, splot(v1 ~ c1 * c2 * c3, d)$dat)

  res <- splot(v1 ~ v2, d, by = "c1", between = c("c2", "c3"), type = "b")
  expect_true(res$ck$t == 1)
  expect_identical(as.numeric(res$seg$n), as.numeric(table(d$c2, d$c3)))
  expect_identical(res$dat, splot(v1 ~ v2 * c1 * c2 * c3, d, type = "l")$dat)
})

test_that("scatter works", {
  expect_true(splot(v1 ~ c1, d, type = "s")$ck$t == 3)

  res <- splot(d$v1 ~ d$v2)

  res <- splot(v1 ~ v2, d, by = c1, model = TRUE)
  expect_identical(names(res$fmod$coefficients), c("(Intercept)", "v2", "c1", "v2:c1"))
  expect_true(res$ck$t == 3)
  expect_true(res$lega$title == "c1")
  expect_true(res$ck$t == 3)
  expect_identical(
    unname(splot(d$v1 ~ d$v2 * d$v1, split = "mean")$ptxt$l.by),
    c("Below Average", "Above Average")
  )
  expect_identical(res$dat, splot(v1 ~ v2 * c1, d)$dat)

  res <- splot(v1 ~ v2, d, by = "c1", between = "c2")
  expect_true(res$ck$t == 3)
  expect_identical(as.numeric(res$seg$n), as.numeric(table(d$c2)))
  expect_identical(res$dat, splot(v1 ~ v2 * c1 * c2, d)$dat)

  res <- splot(v1 ~ v2, d, by = c1, between = c(c2, c3))
  expect_true(res$ck$t == 3)
  expect_identical(as.numeric(res$seg$n), as.numeric(table(d$c2, d$c3)))
  expect_identical(res$dat, splot(v1 ~ v2 * c1 * c2 * c3, d)$dat)
})

test_that("colorby works", {
  res <- splot(d$v1 ~ d$v2, colorby = .y)
  expect_true(res$ptxt$cbo == ".y")
  expect_identical(res$ptxt$leg, splot(d$v1 ~ d$v2, colorby = d$v1)$ptxt$leg)
  expect_identical(
    sort(splot(as.character(c1), d, colorby = v1)$lega$legend),
    sort(unname(formatC(tapply(d$v1, d$c1, mean), 2, format = "f")))
  )
})

skip_if_not(capabilities("cairo"), "Cairo devices not supported")
test_that("saving works", {
  file <- paste0(tempfile(), ".svg")
  splot(1:10, file.name = file, format = "svg")
  expect_true(file.exists(file))
})
