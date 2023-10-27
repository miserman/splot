#' splot color average
#'
#' Calculates the average of a set of colors, returning its Hex code.
#' @param ... color codes or names as characters.
#' @return The calculated color code.
#' @examples
#' # average of red and blue
#' plot(
#'   1:3, numeric(3),
#'   pch = 15, cex = 20, xlim = c(0, 4),
#'   col = c("red", splot.colormean("red", "blue"), "blue")
#' )
#'
#' # average of a set
#' x <- rnorm(100)
#' set <- splot.color(x, method = "related")
#' splot(
#'   x ~ rnorm(100),
#'   colors = set,
#'   add = points(0, 0, pch = 15, cex = 10, col = splot.colormean(set))
#' )
#' @export

splot.colormean <- function(...) {
  hdc <- c(0:9, LETTERS[1:6])
  hdc <- outer(hdc, hdc, paste0)
  s <- seq_len(16)
  ccs <- adjustcolor(unlist(list(...), use.names = FALSE))
  paste(c("#", apply(Reduce("+", lapply(ccs, function(cc) {
    cc <- strsplit(cc, "")[[1]][2:7]
    cc <- paste0(cc[c(TRUE, FALSE)], cc[c(FALSE, TRUE)])
    vapply(cc, function(c) which(hdc == c, TRUE), numeric(2))
  })) / length(ccs), 2, function(cc) {
    hdc[which.min(abs(s - cc[1])), which.min(abs(s - cc[2]))]
  })), collapse = "")
}
