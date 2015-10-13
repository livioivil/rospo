\name{rospo-package}
\alias{rospo-package}
\alias{rospo}
\docType{package}
\title{
\packageTitle{rospo}
}
\description{
\packageDescription{rospo}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{rospo}
\packageIndices{rospo}
Utilities from associazionerospo.org
}
\author{
\packageAuthor{rospo}

Maintainer: \packageMaintainer{rospo}
}


\keyword{ package }


\examples{

data(pal.unipd76)

####### webplot
X <- as.data.frame(matrix(rpois(30,3),3,10))
webplot.multi(X,col=pal.unipd)

####### biplot
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
