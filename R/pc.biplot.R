#' pc.biplot
#' 
#' SV e' il risultato di prcomp, di princomp o di svd.  assi calcolati come in
#' http://en.wikipedia.org/wiki/Biplot si veda anche
#' http://www.multivariatestatistics.org/biplots.html
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param SV the result of a call to princomp, prcomp, svd function. It can also be a metaMDS object (from vegan library).
#' @param x dimension on the x axis (number or name)
#' @param y dimension on the y axis (number or name)
#' @param title a string. "Biplot" by default.
#' @param obs.opt %% ~~Describe \code{obs.opt} here~~
#' @param obs.names %% ~~Describe \code{obs.names} here~~
#' @param obs.col.palette %% ~~Describe \code{obs.col.palette} here~~
#' @param var.opt %% ~~Describe \code{var.opt} here~~
#' @param var.names %% ~~Describe \code{var.names} here~~
#' @param var.col.palette %% ~~Describe \code{var.col.palette} here~~
#' @param lwd %% ~~Describe \code{lwd} here~~
#' @param filename %% ~~Describe \code{filename} here~~
#' @param addPercEV %% ~~Describe \code{addPercEV} here~~
#' @param obs.color.title %% ~~Describe \code{obs.color.title} here~~
#' @param obs.pch.title %% ~~Describe \code{obs.pch.title} here~~
#' @param var.suppl %% ~~Describe \code{var.suppl} here~~
#' @param var.suppl.color %% ~~Describe \code{var.suppl.color} here~~
#' @param var.text.size %% ~~Describe \code{var.text.size} here~~
#' @param var.arrow.size %% ~~Describe \code{var.arrow.size} here~~
#' @param main %% ~~Describe \code{main} here~~
#' @param asp %% ~~Describe \code{asp} here~~
#' @param alpha %% ~~Describe \code{alpha} here~~
#' @param xlim %% ~~Describe \code{xlim} here~~
#' @param ylim %% ~~Describe \code{ylim} here~~
#' @param legend %% ~~Describe \code{legend} here~~
#' @param xylabs string or vector of labels. \code{"PC"} by default.
#' @param percToAdd usually \code{NULL}
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' 
#' 
#' data(pal.uno)
#' 
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
#' @export pc.biplot
pc.biplot <-
function(SV, x=1, y=2,  
                     obs.opt=list(cex=2,pch=20),
                     obs.names=FALSE,
                     obs.col.palette=NULL,  
                     obs.pch.palette=NULL,  
                     var.opt=NULL,
                     var.names=NULL, 
                     var.col.palette=NULL, 
                     lwd=2,
                     filename=NULL,
                     addPercEV=TRUE, 
                     var.suppl=NULL,var.suppl.color=NULL,
                     var.text.size=3,
                     var.text.col="gray30",
                     var.text.relpos=NULL,
                     var.text.cex=1,
                     var.arrow.size=.2,
                     main="Biplot",
                     asp=1,
                     alpha=.5,
                     xlim=NULL,ylim=NULL,
                     legend=NULL,
                     xylabs="PC",
         percToAdd=NULL,...) {

  #######################
  SV=.convertAny2SVD(SV)
  
  old.par=par()
  n.obs=nrow(SV$u)
  n.vars=nrow(SV$v)
  obs.opt=.get.obs.opt(obs.opt,obs.col.palette,SV=SV,obs.pch.palette=obs.pch.palette)
  var.opt=.get.var.opt(var.opt,var.col.palette,SV=SV)
  
  
  
  if(!is.numeric(x)){
     idx=which(colnames(SV$v)==x.name)
     } else idx=x
  if(!is.numeric(y)){
    idy=which(colnames(SV$v)==y.name)
  } else idy=y
  x.name=colnames(SV$v)[idx]
  y.name=colnames(SV$v)[idy]
  
  X=SV$u[,c(idx,idy)]%*%diag(SV$d[c(idx,idy)]^alpha)*sqrt(n.obs) 
  ARROWS=SV$v[,c(idx,idy)]%*%diag(SV$d[c(idx,idy)]^(1-alpha))*sqrt(n.vars)
  rescale.coefs=(SV$d[c(idx,idy)]^alpha)*(SV$d[c(idx,idy)]^(1-alpha))/sqrt(n.obs)*sqrt(n.vars)
  
  xLabel=paste(xylabs,x,sep="")
  yLabel=paste(xylabs,y,sep="")
  if(addPercEV){
    if(is.null(percToAdd))  percToAdd=paste(" (",round(SV$d[c(idx,idy)]^2/sum(SV$d^2)*100,0),"%)",sep="")
    xLabel=paste(xLabel,percToAdd[1],sep="")  
    yLabel=paste(yLabel,percToAdd[2],sep="")
  }
  
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
  
  #gestione dei margins perchè il titolo di non si sovrapponga alle labels dell'asse 3
  old.par.mar=par()$mar
  par(mar=old.par.mar+c(0,0,2,0))
  
  plot(X,  xlim=xlim, ylim=ylim, axes=FALSE,
       xlab=xLabel,
       ylab=yLabel,
       col=obs.opt$col,
       bg=obs.opt$bg,
       pch=obs.opt$pch,
       cex=obs.opt$cex,
       main=main, asp=asp,...)
  if((obs.names!=FALSE)||(length(obs.names)>1)){
    if(length(obs.names)==1 && (obs.names==TRUE)) 
      obs.names=rownames(X)
    if(is.null(obs.names)) obs.names=1:nrow(X)
    text(X[,1]+.15,X[,2]+.15,labels = obs.names)
  }
  temp=axis(1)
  axis(3,at=temp,labels=round(temp*rescale.coefs[1],2),col =var.opt$col[1])
  temp=axis(2)
  axis(4,at=temp,labels=round(temp*rescale.coefs[2],2),col =var.opt$col[1])
  
  #add horiz and vert 0-lines
  abline(v=0,col="gray90")
  abline(h=0,col="gray90")
  
  arrows(0,0,ARROWS[,1],ARROWS[,2],col=var.opt$col,
         lwd=var.opt$lwd,angle=15,length=.1)
  if(!is.null(var.text.relpos)) 
    var.text.xy=ARROWS*var.text.relpos else 
      var.text.xy=ARROWS
  if(is.null(var.names))  var.names=rownames(ARROWS)
  text(var.text.xy[,1],var.text.xy[,2],labels=var.names,
       col=var.text.col,cex=var.text.cex)
  
  
  if((legend==TRUE)||is.null(legend)){
    legend.opt=.get.legend.opt(legend,obs.opt)
    if(!is.null(legend.opt))
      do.call('legend',legend.opt)
  }
par(mar=old.par.mar)
}
