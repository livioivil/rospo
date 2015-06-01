\name{webplot}
\alias{webplot}
\alias{webplot.multi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
webplot
}
\description{
webplot
}
\usage{
webplot(data, data.row = NULL, y.cols = NULL, main = NULL, add = F, col = "red", scale = T, lwd = 3, lty = 1, cex = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{data.row}{
%%     ~~Describe \code{data.row} here~~
}
  \item{y.cols}{
%%     ~~Describe \code{y.cols} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, data.row = NULL, y.cols = NULL, main = NULL, 
    add = F, col = "red", scale = T, lwd = 3, lty = 1, cex = 1, 
    ...) 
{
    if (!is.matrix(data) & !is.data.frame(data)) 
        stop("Requires matrix or data.frame")
    if (is.null(y.cols)) 
        y.cols = colnames(data)
    if (sum(!sapply(data[, y.cols], is.numeric)) > 0) {
        out = paste0("\"", colnames(data)[!sapply(data, is.numeric)], 
            "\"", collapse = ", ")
        stop(paste0("All y.cols must be numeric\n", out, " are not numeric"))
    }
    if (is.null(data.row)) 
        data.row = 1
    if (is.character(data.row)) 
        if (data.row \%in\% rownames(data)) {
            data.row = which(rownames(data) == data.row)
        }
        else {
            stop("Invalid value for data.row:\nMust be a valid rownames(data) or row-index value")
        }
    if (is.null(main)) 
        main = rownames(data)[data.row]
    if (scale == T) {
        data = apply(data, 2, function(x) x/max(abs(x)))
    }
    data = as.data.frame(data)
    n.y = length(y.cols)
    min.rad = 360/n.y
    polar.vals = (90 + seq(0, 360, length.out = n.y + 1)) * pi/180
    if (add == F) {
        plot(0, xlim = c(-2.2, 2.2), ylim = c(-2.2, 2.2), type = "n", 
            axes = F, xlab = "", ylab = "")
        title(main, ...)
        lapply(polar.vals, function(x) lines(c(0, 2 * cos(x)), 
            c(0, 2 * sin(x))))
        lapply(1:n.y, function(x) text(2.15 * cos(polar.vals[x]), 
            2.15 * sin(polar.vals[x]), y.cols[x], cex = cex))
        lapply(seq(0.5, 2, 0.5), function(x) lines(x * cos(seq(0, 
            2 * pi, length.out = 100)), x * sin(seq(0, 2 * pi, 
            length.out = 100)), lwd = 0.5, lty = 2, col = "gray10"))
        lines(cos(seq(0, 2 * pi, length.out = 100)), sin(seq(0, 
            2 * pi, length.out = 100)), lwd = 1.2, col = "gray50")
    }
    r = 2 * data[data.row, y.cols]
    xs = -r * cos(polar.vals)
    ys = r * sin(polar.vals)
    xs = c(xs, xs[1])
    ys = c(ys, ys[1])
    lines(xs, ys, col = col, lwd = lwd, lty = lty, cex = cex, 
        ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
