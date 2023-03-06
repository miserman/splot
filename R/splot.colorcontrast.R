#' splot color contrast ratio
#'
#' Calculates the color contrast ratio between two sets of colors, as defined by the
#' \href{https://www.w3.org/TR/WCAG20/#contrast-ratiodef}{World Wide Web Consortium}.
#' @param color,background A character vector of colors, or a matrix with RGB values across rows.
#' @param plot Logical; if \code{FALSE}, will not plot the results.
#' @return A list with entries for \code{ratio} (contrast ratio),
#' \code{AA} (ratios of at least 4.5), and \code{AAA} (ratios of at least 7).
#' Each entry contains a matrix with colors in rows and backgrounds in columns.
#' @examples
#' # check colors against dark and light backgrounds
#' splot.colorcontrast(c("#FF0000", "#00FF00", "#0000FF"), c("black", "white"))
#'
#' # check contrast between colors
#' splot.colorcontrast(c("red", "green", "blue"), c("red", "green", "blue"))
#'
#' # see when shades of a color cross thresholds on a given background
#' splot.colorcontrast(splot.color(1:10, seed = "#a388b5"), "#101010")
#' @export

splot.colorcontrast <- function(color, background = "#ffffff", plot = TRUE) {
  oc <- color
  ob <- background
  adj <- c(0.2126, 0.7152, 0.0722)
  if (is.character(color)) color <- col2rgb(color)
  if (is.null(dim(color))) color <- matrix(color, 3)
  if (is.character(background)) background <- col2rgb(background)
  if (is.null(dim(background))) background <- matrix(background, 3)
  color <- color / 255
  su <- color <= .03928
  if (any(su)) color[su] <- color[su] / 12.92
  color[!su] <- ((color[!su] + .055) / 1.055)^2.4
  color <- colSums(color * adj)
  background <- background / 255
  su <- background <= .03928
  if (any(su)) background[su] <- background[su] / 12.92
  background[!su] <- ((background[!su] + .055) / 1.055)^2.4
  background <- colSums(background * adj)
  r <- vapply(background, function(bg) {
    su <- bg > color
    color[su] <- (bg + .05) / (color[su] + .05)
    color[!su] <- (color[!su] + .05) / (bg + .05)
    color
  }, color)
  if (is.null(dimnames(r))) r <- matrix(r, length(color))
  rownames(r) <- if (is.character(oc)) oc else paste0("color_", seq_along(color))
  colnames(r) <- if (is.character(ob)) ob else paste0("background_", seq_along(background))
  if (plot) {
    data <- data.frame(
      Contrast = as.numeric(r),
      Background = rep(colnames(r), each = nrow(r)),
      Color = rep(rownames(r), ncol(r))
    )
    splot(
      Contrast ~ Color, data,
      between = "Background",
      type = "bar", title = FALSE, colors = data$Color, ndisp = FALSE, sort = FALSE,
      add = {
        abline(h = 4.5, col = "#a52600", xpd = FALSE)
        abline(h = 7, col = "#0050a5", xpd = FALSE, lty = 2)
      },
      note = "The solid red line is the AA threshold, and the dashed blue line is the AAA threshold."
    )
  }
  list(ratio = r, AA = r >= 4.5, AAA = r >= 7)
}
