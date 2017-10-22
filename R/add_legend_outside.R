#'@description add a legend outside the plot. taken from:
#'http://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
#'@name add_legend_outside
#'@aliases add_legend_outside
#'@title add_legend_outside
#'@export
#'@examples
#'par(mar = c(5, 4, 1.4, 0.2))
#'plot(rnorm(50), rnorm(50), col=c("steelblue", "indianred"), pch=20)
#'add_legend_outside("topright", legend=c("Foo", "Bar"), pch=20, 
#'                   col=c("steelblue", "indianred"),
#'                   horiz=TRUE, bty='n', cex=0.8)


add_legend_outside <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

