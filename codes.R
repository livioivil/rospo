setwd("/home/livio/github/rospo")
rm(list=ls())

sapply(dir("./functions/",full.names = TRUE),source)

X=t(matrix(1:10,10,4))
matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.uno,main="pal.uno")
matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.unipd,main="pal.unipd")
matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.due,main="pal.due")
matplot(X, type = "b", pch = c(15,17,19), lwd=5,lty=1,col=pal.tre,main="pal.tre")



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

pc.biplot(sv,asp=1,obs.col.palette = pal.unipd.76)


pc.biplot(pc,obs.opt = list(col=rep(1:2,5)))
