#SV e' il risultato di prcomp, di princomp o di svd.

pc.biplot <- function(SV, x=1, y=2, title="Biplot", 
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
asp=NULL,
alpha=1/2,
xlim=NULL,ylim=NULL,...) {

  #######################
  SV=.convertAny2SVD(SV)
  
  old.par=par()
  n.obs=nrow(SV$u)
  n.vars=nrow(SV$v)
  obs.opt=.get.obs.opt(obs.opt,obs.col.palette,SV=SV)
  var.opt=.get.var.opt(var.opt,var.col.palette,SV=SV)
  
  
  
  if(!is.numeric(x)){
     x.name=colnames(SV$x)[x]
     idx=which(colnames(SV$x)==x.name)
     } else idx=x
  if(!is.numeric(y)){
    y.name=colnames(SV$x)[y]
    idy=which(colnames(SV$x)==y.name)
  } else idy=y
  
  X=SV$u[,c(idx,idy)]%*%diag(SV$d[c(idx,idy)]^alpha)*sqrt(n.obs) 
  ARROWS=SV$v[,c(idx,idy)]%*%diag(SV$d[c(idx,idy)]^(1-alpha))*sqrt(n.vars)
  rescale.coefs=(SV$d[c(idx,idy)]^alpha)*(SV$d[c(idx,idy)]^(1-alpha))/sqrt(n.obs)*sqrt(n.vars)
  
  
  
  xLabel=ifelse(addPercEV,paste("Pc",x," (",round(SV$d[idx]^2/sum(SV$d^2)*100,0),"%)",sep=""),x)  
  yLabel=ifelse(addPercEV,paste("Pc",y," (",round(SV$d[idy]^2/sum(SV$d^2)*100,0),"%)",sep=""),y)  
  
  if(is.null(xlim)){
    temp=range(c(X[,1],ARROWS[,1])) 
    temp=max(abs(temp))
    xlim=c(-temp,temp)
  }
  if(is.null(ylim)){
    temp=range(c(X[,2],ARROWS[,2])) 
    temp=max(abs(temp))
    ylim=c(-temp,temp)
  }
  
  plot(X,  xlim=xlim, ylim=ylim, axes=FALSE,
       xlab=xLabel,
       ylab=yLabel,
       col=obs.opt$col,
       bg=obs.opt$bg,
       pch=obs.opt$pch,
       cex=obs.opt$cex,
       main=main, asp=asp,...)
  
  temp=axis(1)
  axis(3,at=temp,labels=round(temp*rescale.coefs[1],2),col =var.opt$col[1])
  temp=axis(2)
  axis(4,at=temp,labels=round(temp*rescale.coefs[2],2),col =var.opt$col[1])

  arrows(0,0,ARROWS[,1],ARROWS[,2],col=var.opt$col,
         lwd=lwd,angle=15,length=.1)
  text(ARROWS[,1],ARROWS[,2],labels=rownames(ARROWS),col="gray30")
  
  abline(v=0,col="gray90")
  abline(h=0,col="gray90")
  
  legend.pars=.get.legend.pars (obs.opt)
if(!is.null(legend.pars))
  legend(legend=legend.pars$legend,col=legend.pars$col,
       pch=legend.pars$pch,...)

}
