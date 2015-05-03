#' Draw a two-dimensional pie chart
#'
#' @param x  A vector of non-negative numerical quantities. The values in x are displayed as the areas of pie slices.
#' @param labels One or more expressions or character strings giving names for the slices. 
#' @param edges The circular outline of the pie is approximated by a polygon with this many edges.
#' @param radius The pie is drawn centered in a square box whose sides range from -1 to 1.
#' @param clockwise Logical indicating if slices are drawn clockwise or counter clockwise, the latter is default.
#' @param init.angle Number specifying the starting angle (in degrees) for the slices. 
#' @param density The density of shading lines, in lines per inch. The default value of NULL means that no shading lines are drawn.
#' @param angle The slope of shading lines, given as an angle in degrees (counter-clockwise).
#' @param col A vector of colors to be used in filling or shading the slices. 
#' @param border,lty (Possibly vectors) arguments passed to polygon which draws each slice.
#' @param main An overall title for the plot.
#' @param ... Graphical parameters can be given as arguments to pie. They will affect the main title and labels only.
#' 
#' @return A colored 2-dim pie chart 
#'
#' @examples
#' piechart(rep(1, 24), col = rainbow(24), radius = 0.9)
#' 
#' pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
#' names(pie.sales) <- c("Blueberry", "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")
#' piechart(pie.sales) # default colours
#' piechart(pie.sales, col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))
#'
#' @export
#' 
#' @author Sijing Li


piechart <- function (x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE, 
          init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
          col = NULL, border = NULL, lty = NULL, main = NULL, ...) 
{
  if (!is.numeric(x) || any(is.na(x) | x < 0)) 
    stop("'x' values must be positive.")
  if (is.null(labels)) 
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) 
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col)) 
    col <- if (is.null(density)) 
      c("white", "lightblue", "mistyrose", "lightcyan", 
        "lavender", "cornsilk")
  else par("fg")
  if (!is.null(col)) 
    col <- rep_len(col, nx)
  if (!is.null(border)) 
    border <- rep_len(border, nx)
  if (!is.null(lty)) 
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density)) 
    density <- rep_len(density, nx)
  twopi <- if (clockwise) 
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i])
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
      text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, 
           adj = ifelse(P$x < 0, 1, 0), ...)
    }
  }
  title(main = main, ...)
  invisible(NULL)
}
