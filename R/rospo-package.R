

#' colors and palette
#' 
#' colors and palette
#' 
#' \code{col.grey.unipd} \code{col.red.unipd} \code{pal.uno} \code{pal.due}
#' \code{pal.tre} \code{pal.unipd} \code{pal.unipd.76}
#' 
#' @name rospo.colors
#' @aliases col.grey.unipd col.red.unipd pal.uno pal.due pal.tre pal.unipd
#' pal.unipd.76
#' @docType data
#' @format The format is: chr [1:5] "#FF0000" "#00A08A" "#F2AD00" "#F98400"
#' "#5BBCD6"
#' @references %% ~~ possibly secondary sources and usages ~~
#' @source %% ~~ reference to a publication or URL from which the data were
#' obtained ~~
#' @keywords datasets
#' @examples
#' 
#' data(pal.uno)
#' data(pal.due)
#' data(pal.tre)
#' data(pal.unipd)
#' data(pal.unipd76)
#' 
#' X=t(matrix(1:10,10,4))
#' matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.uno,main="pal.uno")
#' matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.unipd,main="pal.unipd")
#' matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.due,main="pal.due")
#' matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.tre,main="pal.tre")
#' 
NULL





#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_title(\"#1\")}",
#' "rospo")\Sexpr{tools:::Rd_package_title("rospo")}
#' 
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_description(\"#1\")}",
#' "rospo")\Sexpr{tools:::Rd_package_description("rospo")}
#' 
#' 
#' The DESCRIPTION file:
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_DESCRIPTION(\"#1\")}",
#' "rospo")\Sexpr{tools:::Rd_package_DESCRIPTION("rospo")}
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_indices(\"#1\")}",
#' "rospo")\Sexpr{tools:::Rd_package_indices("rospo")} Utilities from
#' associazionerospo.org
#' 
#' @name rospo-package
#' @aliases rospo-package rospo
#' @docType package
#' @author
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_author(\"#1\")}",
#' "rospo")\Sexpr{tools:::Rd_package_author("rospo")}
#' 
#' Maintainer:
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_maintainer(\"#1\")}",
#' "rospo")\Sexpr{tools:::Rd_package_maintainer("rospo")}
#' @keywords package
#' @examples
#' 
#' 
#' data(pal.unipd.76)
#' 
#' ####### webplot
#' X <- as.data.frame(matrix(rpois(30,3),3,10))
#' webplot.multi(X,col=pal.unipd)
#' 
#' ####### biplot
#' par(mar=c(1,1,1,1))
#' Y=matrix(rnorm(30),10,3)
#' rownames(Y)=paste("obs",1:nrow(Y))
#' sv=svd(Y)
#' 
#' pc.biplot(sv)
#' 
#' #i nomi vengono persi con svd
#' pc.biplot(sv,obs.names = TRUE)
#' # soluzione a mano
#' rownames(sv$u)=rownames(Y)
#' pc.biplot(sv,obs.names = TRUE)
#' 
#' 
#' ###########
#' sv=svd(scale(Y,center=TRUE,scale=FALSE))
#' pc.biplot(sv,obs.names = TRUE)
#' 
#' pc=princomp(Y)
#' pc.biplot(pc,obs.names = TRUE)
#' 
#' pc=prcomp(Y)
#' pc.biplot(pc,obs.names = TRUE)
#' 
#' pc.biplot(sv,asp=1,obs.col.palette = pal.uno)
#' 
#' 
#' 
#' pc.biplot(sv,obs.opt = list(col=rep(1:2,5)))
#' 
NULL



