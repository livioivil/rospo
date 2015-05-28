####### biplot per Canonical correlation come da library(CCA) cc()
#### vedi prima PCbiplot qui sotto
cc.biplot<-  function(PC, x=1, y=2, title="Biplot",
                     obs.names=NULL, obs.opt=list(cex=2), obs.label.size=obs.size,
                     obs.col.palette=NULL,  
                     var.names.x=NULL, var.names.y=NULL, 
                     var.color.x=NULL, var.color.y=NULL, ...) {
  
  nvar.x=nrow(PC$scores$corr.X.xscores)
  nvar.y=nrow(PC$scores$corr.Y.xscores)
  PC=list(x=PC$scores$xscores,
          rotation=rbind(PC$scores$corr.X.xscores,PC$scores$corr.Y.xscores),
          sdev=PC$cor)
  colnames(PC$rotation)=paste("CC",1:ncol(PC$rotation),sep="")
  colnames(PC$x)=paste("CC",1:ncol(PC$x),sep="")
  
  var.names=c(var.names.x, var.names.y)
  
  if(is.null(var.color.x))
    var.color.x="#444F51"
  if(is.null(var.color.y))
    var.color.y="#9b0014"
  
  var.color=c(rep(var.color.x,length.out=nvar.x),
              rep(var.color.y,length.out=nvar.y))
  
  pc.biplot(PC, x=x, y=y,  title=title,
             obs.names=obs.names, obs.opt=obs.opt,
            obs.label.size=obs.label.size,
           obs.col.palette=obs.col.palette,  
           var.names=var.names, var.opt=list(col=var.color), 
           ...)
  
}

##################################
####### main function  
# print("PC e' il risultato di una prcomp. se e' un princomp rinominare loadings come rotation e scores come x")
# cat("	PC is a list with 
# 1) x. the score matrix: nxp matrix with colnames (es 'PC1' 'PC2' etc.)
# 2) rotation. the matrix of loadings original variables as rows and PCs as columns.(required row/col-names)
# ")
pc.biplot <- function(PC, x=1, y=2, title="Biplot", 
                     obs.opt=list(cex=2,pch=20),
obs.col.palette=NULL,  
var.names=NULL, var.opt=NULL,
var.col.palette=NULL, lwd=2,
filename=NULL,
# loadingsTextJitter= position_jitter(w = 0.2, h = 0.2),
# scoresJitter=  position_jitter(w = .2, h = .2),
addPercEV=TRUE,
obs.color.title=NULL,obs.pch.title=NULL,
var.suppl=NULL,var.suppl.color=NULL,var.text.size=3,
var.arrow.size=.2,main="Biplot",
asp=NULL,...) {
	#modificata da http://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
	
	##PC is a list with 1) x. the score matrix: nxp matrix with colnames (es "PC1" "PC2" etc.)
	##					2) rotation. the matrix of loadings original variables as rows and PCs as columns.(required row/col-names)
	
  old.par=par()
  n=nrow(PC$x)
  
  obs.opt=.get.obs.opt(obs.opt,obs.col.palette,PC=PC)
  
  
  
  x.name=colnames(PC$x)[x]
  y.name=colnames(PC$x)[y]
  idx=which(colnames(PC$x)==x.name)
  idy=which(colnames(PC$x)==y.name)
  
  PC$x=PC$x[,c(idx,idy)]/PC$sdev[c(idx,idy)]      
  datapc=PC$rotation[,c(idx,idy)]*sqrt(nrow(PC$rotation))*1.3
  
  xLabel=ifelse(addPercEV,paste("PC",x," (",round(PC$sdev[idx]^2/sum(PC$sdev^2)*100,0),"%)",sep=""),x)  
  yLabel=ifelse(addPercEV,paste("PC",y," (",round(PC$sdev[idy]^2/sum(PC$sdev^2)*100,0),"%)",sep=""),y)  
  
  plot(PC$x, # xlim=range(c(PC$x[,1],datapc[,1])), ylim=range(c(PC$x[,2],datapc[,2])),
       xlab=xLabel,
       ylab=yLabel,
       col=obs.opt$col,
       bg=obs.opt$bg,
       pch=obs.opt$pch,
       cex=obs.opt$cex,
       main=main, asp=asp,...)
  
  var.opt=.get.var.opt(var.opt,var.col.palette,PC=PC)
  arrows(0,0,datapc[,1],datapc[,2],col=var.opt$col,
         lwd=lwd,angle=15,length=.1)
  text(datapc[,1],datapc[,2],labels=rownames(datapc),col="gray30")
  
  
  legend.pars=.get.legend.pars (obs.opt)
if(!is.null(legend.pars))
  legend(legend=legend.pars$legend,col=legend.pars$col,
       pch=legend.pars$pch,...)


}
