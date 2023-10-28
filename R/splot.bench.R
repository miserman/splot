#' splot benchmarker
#'
#' Time one or more expressions over several iteration, then plot the distributions of their times.
#' @param ... accepts any number of expressions to be timed. See examples.
#' @param runs the number of overall iterations. Increase to stabilize estimates.
#' @param runsize the number of times each expression is evaluated within each run. Increase to
#'   differentiate estimates (particularly for very fast operations).
#' @param cleanup logical; if \code{TRUE}, garbage collection will be performed before each run.
#'   Garbage collection greatly increases run time, but may result in more stable timings.
#' @param print.names logical; if \code{FALSE}, the entered expressions will be included in the plot
#'   as legend names. Otherwise, (and if the number of expressions is over 5 or the length of any
#'   expression is over 50 characters) expressions are replaced with numbers corresponding to their
#'   entered position.
#' @param limit.outliers logical; if \code{TRUE} (default), times over an upper bound for the given
#'   expression will be set to that upper bound, removing aberrant extremes.
#' @param check_output logical; if \code{TRUE}, the output of each expression is checked with
#'   \code{\link[base]{all.equal}} against that of the first. A warning indicates if any are not
#'   equal, and results are invisibly returned.
#' @param check_args a list of arguments to be passed to \code{\link[base]{all.equal}}, if
#'   \code{check_output} is \code{TRUE}.
#' @param options a list of options to pass on to splot.
#' @return A list:
#' \tabular{ll}{
#'   plot \tab splot output\cr
#'   checks \tab a list of result from all.equal, if \code{check_output} was \code{TRUE}\cr
#'   expressions \tab a list of the entered expressions \cr
#'   summary \tab a matrix of the printed results \cr
#' }
#' @examples
#' # increase the number of runs for more stable estimates
#'
#' # compare ways of looping through a vector
#' splot.bench(
#'   sapply(1:100, "*", 10),
#'   mapply("*", 1:100, 10),
#'   vapply(1:100, "*", 0, 10),
#'   unlist(lapply(1:100, "*", 10)),
#'   runs = 20, runsize = 200
#' )
#'
#' # compare ways of setting all but the maximum value of each row in a matrix to 0
#' if (FALSE) {
#'
#' mat <- matrix(c(rep(1, 4), rep(0, 8)), 4, 3)
#' splot.bench(
#'   t(vapply(seq_len(4), function(r) {
#'     mat[r, mat[r, ] < max(mat[r, ])] <- 0
#'     mat[r, ]
#'   }, numeric(ncol(mat)))),
#'   do.call(rbind, lapply(seq_len(4), function(r) {
#'     mat[r, mat[r, ] < max(mat[r, ])] <- 0
#'     mat[r, ]
#'   })),
#'   do.call(rbind, lapply(seq_len(4), function(r) {
#'     nr <- mat[r, ]
#'     nr[nr < max(nr)] <- 0
#'     nr
#'   })),
#'   {
#'     nm <- mat
#'     for (r in seq_len(4)) {
#'       nr <- nm[r, ]
#'       nm[r, nr < max(nr)] <- 0
#'     }
#'     nm
#'   },
#'   {
#'     nm <- mat
#'     for (r in seq_len(4)) nm[r, nm[r, ] < max(nm[r, ])] <- 0
#'     nm
#'   },
#'   {
#'     nm <- matrix(0, dim(mat)[1], dim(mat)[2])
#'     for (r in seq_len(4)) {
#'       m <- which.max(mat[r, ])
#'       nm[r, m] <- mat[r, m]
#'     }
#'     nm
#'   },
#'   {
#'     ck <- do.call(rbind, lapply(seq_len(4), function(r) {
#'       nr <- mat[r, ]
#'       nr < max(nr)
#'     }))
#'     nm <- mat
#'     nm[ck] <- 0
#'     nm
#'   },
#'   t(apply(mat, 1, function(r) {
#'     r[r < max(r)] <- 0
#'     r
#'   })),
#'   runs = 50,
#'   runsize = 200
#' )
#'
#' }
#' @export

splot.bench <- function(
    ..., runs = 20, runsize = 200, cleanup = FALSE, print.names = FALSE,
    limit.outliers = TRUE, check_output = TRUE, check_args = list(), options = list()) {
  e <- sapply(as.character(substitute(list(...)))[-1], function(t) parse(text = t))
  e <- e[!duplicated(names(e))]
  es <- length(e)
  if (!es) stop("no expressions found", call. = FALSE)
  ne <- names(e)
  seconds <- matrix(NA, runs, es, dimnames = list(NULL, ne))
  rs <- seq_len(runsize)
  ops <- tryCatch(
    lapply(e, eval, parent.frame(3)),
    error = function(e) stop("one of your expressions breaks:\n", e, call. = FALSE)
  )
  checks <- if (check_output && length(e) != 1) {
    if (!"check.attributes" %in% names(check_args)) check_args$check.attributes <- FALSE
    if (!"check.names" %in% names(check_args)) check_args$check.names <- FALSE
    lapply(ops[-1], function(r) {
      tryCatch(
        do.call(all.equal, c(list(r), list(ops[[1]]), check_args)),
        error = function(e) FALSE
      )
    })
  } else {
    NULL
  }
  if (!is.null(checks) && !all(vapply(checks, isTRUE, TRUE))) {
    warning(
      "some of your expressions do not seem to have similar results as the first;",
      " see the `checks` output.",
      call. = FALSE
    )
  }
  ost <- proc.time()[3]
  cat("benchmarking", es, "expression(s) in chunks of", runsize, "per run... \nrun 0 of", runs)
  fun <- function(e) {
    eval(e, .GlobalEnv)
    NULL
  }
  for (r in seq_len(runs)) {
    for (f in sample(seq_len(es))) {
      if (cleanup) gc(FALSE)
      st <- proc.time()[[3]]
      for (i in rs) fun(e[[f]])
      seconds[r, f] <- proc.time()[[3]] - st
    }
    cat("\rrun", r, "of", runs)
  }
  cat("\rfinished", runs, "runs in", round(proc.time()[3] - ost, 2), "seconds       \n\n")
  cat("expressions:\n\n")
  icn <- seq_len(es)
  ne <- gsub("\n", "\n   ", ne, fixed = TRUE)
  for (i in icn) cat(i, ". ", ne[i], "\n", sep = "")
  cat("\n")
  res <- rbind(colSums(seconds), colMeans(seconds))
  res <- rbind(res, if (min(res[1, ], na.rm = TRUE) == 0) res[1, ] + 1 else res[1, ] / min(res[1, ], na.rm = TRUE))
  dimnames(res) <- list(c("total time (seconds)", "mean time per run", "times the minimum"), icn)
  print(round(res, 4))
  if (!print.names) {
    if (!missing(print.names) || es > 5 || any(nchar(names(e)) > 50)) {
      colnames(seconds) <- icn
    }
  }
  if (limit.outliers) {
    for (f in seq_len(es)) {
      qr <- quantile(seconds[, f], c(.25, .75), TRUE)
      qrc <- qr[2] + (qr[2] - qr[1]) * 1.5
      seconds[seconds[, f] > qrc, f] <- qrc
    }
  }
  if (es == 1 && runs == 1) {
    return(list(plot = NULL, summary = res))
  }
  title <- paste("timing of", runs, "runs of", runsize, "calls each")
  if (nrow(seconds) == 1) {
    options$x <- colnames(seconds)
    seconds <- seconds[1, ]
  }
  invisible(list(
    plot = splot(seconds, title = title, labels.filter = FALSE, labels.trim = FALSE, options = options),
    checks = checks,
    expressions = as.list(unname(e)),
    summary = res
  ))
}
