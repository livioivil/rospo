
# is.svd <- function(SV){
#   setequal(names(SV),c("d","u","v"))
# }

which.hasname <- function(X,name){
  which(names(X)==name)
}

.fix.col.pc.names <- function(mat){
  if(is.null(colnames(mat))) 
    colnames(mat)=paste("PC",1:ncol(mat),sep="")
  mat
}

.fix.names <- function(mat,ndim=1,prefix="PC"){
  if(is.null(dimnames(mat)[[ndim]])) 
    dimnames(mat)[[ndim]]=paste(prefix,1:dim(mat)[ndim],sep="")
  mat
}
.convertAny2SVD <- function(SV){
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
  SV$v=.fix.names(SV$v,1,"Var")
  SV
}