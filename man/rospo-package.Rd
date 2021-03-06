% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rospo-package.R
\docType{package}
\name{rospo-package}
\alias{rospo-package}
\alias{rospo}
\title{c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_title(\"#1\")}",
"rospo")\Sexpr{tools:::Rd_package_title("rospo")}}
\description{
c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_description(\"#1\")}",
"rospo")\Sexpr{tools:::Rd_package_description("rospo")}
}
\details{
The DESCRIPTION file:
c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_DESCRIPTION(\"#1\")}",
"rospo")\Sexpr{tools:::Rd_package_DESCRIPTION("rospo")}
c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_indices(\"#1\")}",
"rospo")\Sexpr{tools:::Rd_package_indices("rospo")} Utilities from
associazionerospo.org
}
\examples{


data(pal.unipd.76)

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
\author{
c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_author(\"#1\")}",
"rospo")\Sexpr{tools:::Rd_package_author("rospo")}

Maintainer:
c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_maintainer(\"#1\")}",
"rospo")\Sexpr{tools:::Rd_package_maintainer("rospo")}
}
\keyword{package}
