###################### .get.legend.opt
.get.legend.opt <- function(legend.opt,obs.opt){
  identificatori.gruppi=c("col","bg","pch")

  leg.elements=unique(as.data.frame(obs.opt[identificatori.gruppi]),MARGIN = 1)
  if((nrow(leg.elements)<=1)&&(is.null(legend.opt)))
    legend.opt=NULL else 
      { 
        #legent.opt default
        if(is.null(legend.opt)){
           legend.opt=as.list(rep(NA,length(identificatori.gruppi)+1))
           names(legend.opt)=c(identificatori.gruppi,"x")
           legend.opt$x="bottomleft"
           }
        elementi.variabili=names(which(apply(leg.elements,2,function(x)length(unique(x))>1)))
        legend.opt$legend=apply(leg.elements[,elementi.variabili,drop=FALSE],1,paste,collapse="-")

        for (i in setdiff(identificatori.gruppi,names(which(!is.na(legend.opt)))))
          legend.opt[[i]]=leg.elements[,i]
#         names(legend.opt)[which(names(legend.opt)=="bg")]="fill"
        if(is.null(legend.opt$bty)) legend.opt$bty="n"
   }
  legend.opt
}
####################### .get.obs.opt
.get.obs.opt <- function(obs.opt,obs.col.palette=NULL,SV){
  
  #   if(is.null(var.suppl.opt$col)) var.suppl.color=greyUnipd
  if(is.null(obs.opt$names)) obs.names=FALSE
  if(is.logical(obs.opt$names) && (obs.opt$names==TRUE)) 
    obs.opt$obs.names=if(is.null(row.names(SV$x))) 
      as.character(1:n) else row.names(SV$x)
  
  if(is.null(obs.opt$col)) obs.opt$col=1
  if(obs.opt$col[1]=="each.obs")
    obs.opt$col=1:n

  if(is.null(obs.opt$pch)) obs.opt$pch=20
  
  if(is.null(obs.opt$bg)) obs.opt$bg=1
  if(obs.opt$bg[1]=="each.obs")
    obs.opt$bg=1:n
  
  if(!is.null(obs.col.palette))
      palette(obs.col.palette) 
  obs.opt
}


#################.get.var.opt
.get.var.opt <- function(var.opt,var.col.palette,SV){
  if(is.null(var.opt$names)) var.names=rownames(SV$rotation)
  if(is.null(var.opt$col)) var.opt$col=2
  if(!is.null(var.col.palette))
    palette(var.col.palette) 
  var.opt
}