% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/t_test_light.R
\name{t.test.light}
\alias{t.test.light}
\title{t.test.light}
\usage{
\method{t}{test.light}(Y, tail = 1, group = NULL)
}
\arguments{
\item{Y}{n X p matrix or data.frame}

\item{tail}{+1,-1 or 0}

\item{group}{if \code{NULL}, it performs a one sample t-test}
}
\value{
a list with the following vectors:\code{p,t,df,means,sds}
}
\description{
t.test.light
}
\examples{
Y=matrix(rnorm(30),10,3)
x=rep(0:1,5)
t.test.light(Y,group=x)
summary(lm(Y[,1]~x))
}
