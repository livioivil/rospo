\name{rospo.colors}
\alias{col.grey.unipd}
\alias{col.red.unipd}
\alias{pal.uno}
\alias{pal.due}
\alias{pal.tre}
\alias{pal.unipd}
\alias{pal.unipd.76}
\docType{data}
\title{
colors and palette
}
\description{
colors and palette
}
\usage{data("colors")}
\format{
  The format is:
 chr [1:5] "#FF0000" "#00A08A" "#F2AD00" "#F98400" "#5BBCD6"
}
\details{
\code{col.grey.unipd}
\code{col.red.unipd}
\code{pal.uno}
\code{pal.due}
\code{pal.tre}
\code{pal.unipd}
\code{pal.unipd.76}
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(pal.uno)
data(pal.due)
data(pal.tre)
data(pal.unipd)
data(pal.unipd76)

X=t(matrix(1:10,10,4))
matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.uno,main="pal.uno")
matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.unipd,main="pal.unipd")
matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.due,main="pal.due")
matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.tre,main="pal.tre")
}
\keyword{datasets}
