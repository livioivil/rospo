\name{cc.biplot}
\alias{cc.biplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cc.biplot
}
\description{
cc.biplot
}
\usage{
cc.biplot(SV, x = 1, y = 2, title = "Biplot", obs.names = NULL, obs.opt = list(cex = 2), obs.label.size = obs.size, obs.col.palette = NULL, var.names.x = NULL, var.names.y = NULL, var.color.x = NULL, var.color.y = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SV}{
result of cc and rcc of library(CCA)}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{obs.names}{
%%     ~~Describe \code{obs.names} here~~
}
  \item{obs.opt}{
%%     ~~Describe \code{obs.opt} here~~
}
  \item{obs.label.size}{
%%     ~~Describe \code{obs.label.size} here~~
}
  \item{obs.col.palette}{
%%     ~~Describe \code{obs.col.palette} here~~
}
  \item{var.names.x}{
%%     ~~Describe \code{var.names.x} here~~
}
  \item{var.names.y}{
%%     ~~Describe \code{var.names.y} here~~
}
  \item{var.color.x}{
%%     ~~Describe \code{var.color.x} here~~
}
  \item{var.color.y}{
%%     ~~Describe \code{var.color.y} here~~
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
function (SV, x = 1, y = 2, title = "Biplot", obs.names = NULL, 
    obs.opt = list(cex = 2), obs.label.size = obs.size, obs.col.palette = NULL, 
    var.names.x = NULL, var.names.y = NULL, var.color.x = NULL, 
    var.color.y = NULL, ...) 
{
    nvar.x = nrow(SV$scores$corr.X.xscores)
    nvar.y = nrow(SV$scores$corr.Y.xscores)
    SV = list(x = SV$scores$xscores, rotation = rbind(SV$scores$corr.X.xscores, 
        SV$scores$corr.Y.xscores), sdev = SV$cor)
    colnames(SV$rotation) = paste("CC", 1:ncol(SV$rotation), 
        sep = "")
    colnames(SV$x) = paste("CC", 1:ncol(SV$x), sep = "")
    var.names = c(var.names.x, var.names.y)
    if (is.null(var.color.x)) 
        var.color.x = "#444F51"
    if (is.null(var.color.y)) 
        var.color.y = "#9b0014"
    var.color = c(rep(var.color.x, length.out = nvar.x), rep(var.color.y, 
        length.out = nvar.y))
    pc.biplot(SV, x = x, y = y, title = title, obs.names = obs.names, 
        obs.opt = obs.opt, obs.label.size = obs.label.size, obs.col.palette = obs.col.palette, 
        var.names = var.names, var.opt = list(col = var.color), 
        ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
