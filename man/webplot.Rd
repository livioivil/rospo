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
if \code{TRUE}:
    data = apply(data, 2, function(x) ((x - min(x))/(max(x)-min(x))+.1)/1.1)

}
  \item{col}{
as in \code{plot()}
}
  \item{lwd}{
as in \code{plot()}
}
  \item{lty}{
as in \code{plot()}
}
  \item{cex}{
as in \code{plot()}
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
data(pal.unipd)

X <- as.data.frame(matrix(rpois(30,100),3,10))
webplot.multi(X[1,])
webplot.multi(X,col=pal.unipd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
