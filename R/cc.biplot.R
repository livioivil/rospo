cc.biplot <-
function(SV, x=1, y=2, title="Biplot",
                      obs.names=NULL, obs.opt=list(cex=2), obs.label.size=obs.size,
                      obs.col.palette=NULL,  
                      var.names.x=NULL, var.names.y=NULL, 
                      var.color.x=NULL, var.color.y=NULL, ...) {
  
  nvar.x=nrow(SV$scores$corr.X.xscores)
  nvar.y=nrow(SV$scores$corr.Y.xscores)
  SV=list(x=SV$scores$xscores,
          rotation=rbind(SV$scores$corr.X.xscores,SV$scores$corr.Y.xscores),
          sdev=SV$cor)
  colnames(SV$rotation)=paste("CC",1:ncol(SV$rotation),sep="")
  colnames(SV$x)=paste("CC",1:ncol(SV$x),sep="")
  
  var.names=c(var.names.x, var.names.y)
  
  if(is.null(var.color.x))
    var.color.x="#444F51"
  if(is.null(var.color.y))
    var.color.y="#9b0014"
  
  var.color=c(rep(var.color.x,length.out=nvar.x),
              rep(var.color.y,length.out=nvar.y))
  
  pc.biplot(SV, x=x, y=y,  title=title,
            obs.names=obs.names, obs.opt=obs.opt,
            obs.label.size=obs.label.size,
            obs.col.palette=obs.col.palette,  
            var.names=var.names, var.opt=list(col=var.color), 
            ...)
  
}
