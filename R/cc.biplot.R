#' cc.biplot
#' 
#' cc.biplot
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param SV result of cc and rcc of library(CCA)
#' @param x %% ~~Describe \code{x} here~~
#' @param y %% ~~Describe \code{y} here~~
#' @param title %% ~~Describe \code{title} here~~
#' @param obs.names %% ~~Describe \code{obs.names} here~~
#' @param obs.opt %% ~~Describe \code{obs.opt} here~~
#' @param obs.label.size %% ~~Describe \code{obs.label.size} here~~
#' @param obs.col.palette %% ~~Describe \code{obs.col.palette} here~~
#' @param var.names.x %% ~~Describe \code{var.names.x} here~~
#' @param var.names.y %% ~~Describe \code{var.names.y} here~~
#' @param var.color.x %% ~~Describe \code{var.color.x} here~~
#' @param var.color.y %% ~~Describe \code{var.color.y} here~~
#' @param xylabs string or vector of labels. \code{"CC1"} by default.
#' @param usually NULL
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @export cc.biplot
cc.biplot <-
function(SV, x=1, y=2, main="Biplot",
         obs.col.palette=NULL,  
         obs.label.size =2,
                      obs.names=FALSE, obs.opt=list(cex=2), 
                      var.names.x=NULL, var.names.y=NULL, 
                      var.color.x=NULL, var.color.y=NULL, xylabs="CC",percToAdd=NULL,...) {
  
  nvar.x=nrow(SV$scores$corr.X.xscores)
  nvar.y=nrow(SV$scores$corr.Y.xscores)
  SV=list(u=SV$scores$xscores,
          v=rbind(SV$scores$corr.X.xscores,SV$scores$corr.Y.xscores),
          d=SV$cor)
  colnames(SV$v)=paste("CCx",1:ncol(SV$v),sep="")
  colnames(SV$u)=paste("CCy",1:ncol(SV$u),sep="")
  names(SV$d)=paste("cor",1:length(SV$d),sep="")
  
  var.names=c(var.names.x, var.names.y)
  
  if(is.null(percToAdd))  
    percToAdd=paste(" (",round(SV$d[c(x,y)]*100,0),"%)",sep="")
  
  
  if(is.null(var.color.x))
    var.color.x="#444F51"
  if(is.null(var.color.y))
    var.color.y="#9b0014"
  
  var.color=c(rep(var.color.x,length.out=nvar.x),
              rep(var.color.y,length.out=nvar.y))
  
  pc.biplot(SV, x=x, y=y,  main=main,
            obs.names=obs.names, obs.opt=obs.opt,
            obs.label.size=obs.label.size,
            obs.col.palette=obs.col.palette,  
            var.names=var.names, var.opt=list(col=var.color),
            xylabs=xylabs,percToAdd=percToAdd,
            ...)
}