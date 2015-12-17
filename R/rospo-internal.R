.convertAny2SVD <-
function(SV){
#   if(is.svd(SV)){
#     #rinomina e riscala per generare gli score
#     names(SV)[which.hasname(SV,"u")]="x"
#     SV$x=SV$x%*%diag(SV$d)*sqrt(nrow(SV$x))
#     SV$x=.fix.col.pc.names(SV$x)
#     #dev std delle SV
#     names(SV)[which.hasname(SV,"d")]="sdev"
#     #matrice loadings
#     names(SV)[which.hasname(SV,"v")]="rotation"    
#     SV$rotation=.fix.col.pc.names(SV$rotation)
#   }
  
  if(is(SV,"princomp")){
    #rinomina e riscala per generare gli score
    names(SV)[which.hasname(SV,"scores")]="u"
    SV$u=SV$u%*%diag(1/SV$sdev)/sqrt(nrow(SV$u))
    #dev std delle SV
    names(SV)[which.hasname(SV,"sdev")]="d"
    #matrice loadings
    names(SV)[which.hasname(SV,"loadings")]="v"    
  }  
  if(is(SV,"prcomp")){
    #rinomina e riscala per generare gli score
    names(SV)[which.hasname(SV,"x")]="u"
    SV$u=SV$u%*%diag(1/SV$sdev)/sqrt(nrow(SV$u))
    #dev std delle SV
    names(SV)[which.hasname(SV,"sdev")]="d"
    #matrice loadings
    names(SV)[which.hasname(SV,"rotation")]="v"    
  }  
  
  SV$u=.fix.names(SV$u,1,"")
  SV$v=.fix.names(SV$v,2,"Pc")
  SV$d=.fix.names(SV$d,1,"Sd")
  SV
}
.fix.col.pc.names <-
function(mat){
  if(is.null(colnames(mat))) 
    colnames(mat)=paste("PC",1:ncol(mat),sep="")
  mat
}
.fix.names <-
function(mat,ndim=1,prefix="PC"){
  if(is.vector(mat)){
    if(is.null(names(mat)))
      names(mat)=paste(prefix,1:length(mat),sep="") 
    } else if(is.null(dimnames(mat)[[ndim]]))
      { 
      dimnames(mat)[[ndim]]=paste(prefix,1:dim(mat)[ndim],sep="")
    }
  mat
}
.get.legend.opt <-
function(legend.opt,obs.opt){

  if(!is.null(obs.opt$bg))
    if(is.factor(obs.opt$bg))
      obs.opt$bg=as.numeric(obs.opt$bg)
  identificatori.gruppi=c("col","bg","pch")
  identificatori.gruppi.name=c("col","bg","pch.name")
  if(!is.null(names(obs.opt$pch))) 
    obs.opt$pch.name=names(obs.opt$pch) else
      obs.opt$pch.name=obs.opt$pch
  
  leg.elements=unique(as.data.frame(obs.opt[identificatori.gruppi]),MARGIN = 1)
  if((nrow(leg.elements)<=1)&&(is.null(legend.opt)))
    legend.opt=NULL else 
      { 
        #legent.opt default
        if(is.null(legend.opt)){
           legend.opt = list(x="bottomleft")
           }
        elementi.variabili=names(which(apply(leg.elements,2,function(x)length(unique(x))>1)))
        leg.elements.leg=unique(as.data.frame(obs.opt[identificatori.gruppi.name]),MARGIN = 1)
        names(leg.elements.leg)=gsub("pch.name","pch",names(leg.elements.leg))
        
        elementi.variabili.leg=elementi.variabili
        if(!obs.opt$legend_bg){ 
          leg.elements.leg$bg=NULL
          elementi.variabili.leg=setdiff(elementi.variabili.leg,"bg")
        }
        
        legend.opt$legend=apply(leg.elements.leg[,elementi.variabili.leg,drop=FALSE],1,paste,collapse="-")
        
        legend.opt=c(legend.opt,sapply(setdiff(colnames(leg.elements),elementi.variabili),
          function(tag) leg.elements[1,tag]))
          
        
        for (i in setdiff(identificatori.gruppi,names(which(!is.na(legend.opt)))))
          legend.opt[[i]]=leg.elements[,i]
#         names(legend.opt)[which(names(legend.opt)=="bg")]="fill"
        if(is.null(legend.opt$bty)) legend.opt$bty="n"
   }


  if(!is.null(names(legend.opt)))names(legend.opt)=gsub("bg","pt.bg",names(legend.opt))
  legend.opt
}
.get.obs.opt <-
function(obs.opt,obs.col.palette=NULL,SV,obs.pch.palette=NULL){
  
  #   if(is.null(var.suppl.opt$col)) var.suppl.color=greyUnipd
  if(is.null(obs.opt$names)) obs.names=FALSE
  if(is.logical(obs.opt$names) && (obs.opt$names==TRUE)) 
    obs.opt$obs.names=if(is.null(row.names(SV$u))) 
      as.character(1:n) else row.names(SV$u)
  
  if(is.null(obs.opt$col)) obs.opt$col=1
  if(obs.opt$col[1]=="each.obs")
    obs.opt$col=1:n

  if(is.null(obs.opt$bg)) {
    obs.opt$bg=obs.opt$col
    obs.opt$legend_bg=FALSE
  } else obs.opt$legend_bg=TRUE
  if(obs.opt$bg[1]=="each.obs")
    obs.opt$bg=1:n
  
  if(!is.null(obs.col.palette))
      palette(obs.col.palette) 
  
  
  if(is.null(obs.pch.palette))
    obs.pch.palette=21:22 
  
  if(is.null(obs.opt$pch)) obs.opt$pch=20
  
  if(!is.numeric(obs.opt$pch)){    
    obs.opt$pch=factor(obs.opt$pch)
    labs=levels(obs.opt$pch)
    obs.pch.palette=rep(obs.pch.palette,
                        length.out = nlevels(obs.opt$pch))
    obs.opt$pch=as.numeric(obs.opt$pch)
    names_pch=labs[obs.opt$pch]
    obs.opt$pch= obs.pch.palette[obs.opt$pch]
    names(obs.opt$pch)=names_pch
  }
   
  obs.opt
}
.get.var.opt <-
function(var.opt,var.col.palette,SV){
  if(is.null(var.opt$names)) var.names=rownames(SV$rotation)
  if(is.null(var.opt$col)) var.opt$col=2
  if(!is.null(var.col.palette))
    palette(var.col.palette) 
  var.opt
}
