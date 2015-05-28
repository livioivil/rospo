###################### .get.legend.pars
.get.legend.pars <- function(obs.opt){
 leg.elements= apply(cbind(col=obs.opt$col,bg=obs.opt$bg,pch=obs.opt$pch),
        1, paste,collapse="__")
 if(length(leg.elements)) 
   leg.pars=NULL else {
     leg.el.col=cbind(legend=unique(obs.opt$col),pch=1,col=unique(obs.opt$col))
     leg.el.pch=cbind(legend=unique(obs.opt$col),col=1,pch=unique(obs.opt$pch))
     #da modificare:
     leg.el.col$bg=leg.el.col$col
     leg.el.pch$bg=leg.el.pch$col
     leg.pars=rbind(leg.el.col,leg.el.pch)
     if(is.null(legend.opt$bty)) legend.opt$bty="n"
   }
 
 leg.pars
}
####################### .get.obs.opt
.get.obs.opt <- function(obs.opt,obs.col.palette=NULL,PC){
  
  #   if(is.null(var.suppl.opt$col)) var.suppl.color=greyUnipd
  if(is.null(obs.opt$names)) obs.names=FALSE
  if(is.logical(obs.opt$names) && (obs.opt$names==TRUE)) 
    obs.opt$obs.names=if(is.null(row.names(PC$x))) 
      as.character(1:n) else row.names(PC$x)
  
  if(is.null(obs.opt$col)) obs.opt$col=1
  if(obs.opt$col[1]=="each.obs")
    obs.opt$col=1:n

  if(is.null(obs.opt$pch)) obs.opt$pch=21
  
  if(is.null(obs.opt$bg)) obs.opt$bg=1
  if(obs.opt$bg[1]=="each.obs")
    obs.opt$bg=1:n
  
  if(!is.null(obs.col.palette))
      palette(obs.col.palette) 
  obs.opt
}


#################.get.var.opt
.get.var.opt <- function(var.opt,var.col.palette,PC){
  if(is.null(var.opt$names)) var.names=rownames(PC$rotation)
  if(is.null(var.opt$col)) var.opt$col=2
  if(!is.null(var.col.palette))
    palette(var.col.palette) 
  var.opt
}