\name{pc.biplot}
\alias{pc.biplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
pc.biplot}
\description{
SV e' il risultato di prcomp, di princomp o di svd.
 assi calcolati come in 
 http://en.wikipedia.org/wiki/Biplot
 si veda anche
 http://www.multivariatestatistics.org/biplots.html
}
\usage{
pc.biplot(SV, x = 1, y = 2, title = "Biplot", obs.opt = list(cex = 2, pch = 20), obs.names = FALSE, obs.col.palette = NULL, var.opt = NULL, var.names = NULL, var.col.palette = NULL, lwd = 2, filename = NULL, addPercEV = TRUE, obs.color.title = NULL, obs.pch.title = NULL, var.suppl = NULL, var.suppl.color = NULL, var.text.size = 3, var.arrow.size = 0.2, main = "Biplot", asp = NULL, alpha = 1/2, xlim = NULL, ylim = NULL, legend = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SV}{
%%     ~~Describe \code{SV} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{obs.opt}{
%%     ~~Describe \code{obs.opt} here~~
}
  \item{obs.names}{
%%     ~~Describe \code{obs.names} here~~
}
  \item{obs.col.palette}{
%%     ~~Describe \code{obs.col.palette} here~~
}
  \item{var.opt}{
%%     ~~Describe \code{var.opt} here~~
}
  \item{var.names}{
%%     ~~Describe \code{var.names} here~~
}
  \item{var.col.palette}{
%%     ~~Describe \code{var.col.palette} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{addPercEV}{
%%     ~~Describe \code{addPercEV} here~~
}
  \item{obs.color.title}{
%%     ~~Describe \code{obs.color.title} here~~
}
  \item{obs.pch.title}{
%%     ~~Describe \code{obs.pch.title} here~~
}
  \item{var.suppl}{
%%     ~~Describe \code{var.suppl} here~~
}
  \item{var.suppl.color}{
%%     ~~Describe \code{var.suppl.color} here~~
}
  \item{var.text.size}{
%%     ~~Describe \code{var.text.size} here~~
}
  \item{var.arrow.size}{
%%     ~~Describe \code{var.arrow.size} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{asp}{
%%     ~~Describe \code{asp} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
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


data(pal.uno)

par(mar=c(1,1,1,1))
Y=matrix(rnorm(30),10,3)
rownames(Y)=paste("obs",1:nrow(Y))
sv=svd(Y)

pc.biplot(sv)

#i nomi vengono persi con svd
pc.biplot(sv,obs.names = TRUE)
# soluzione a mano
rownames(sv$u)=rownames(Y)
pc.biplot(sv,obs.names = TRUE)


###########
sv=svd(scale(Y,center=TRUE,scale=FALSE))
pc.biplot(sv,obs.names = TRUE)

pc=princomp(Y)
pc.biplot(pc,obs.names = TRUE)

pc=prcomp(Y)
pc.biplot(pc,obs.names = TRUE)

pc.biplot(sv,asp=1,obs.col.palette = pal.uno)



pc.biplot(sv,obs.opt = list(col=rep(1:2,5)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
