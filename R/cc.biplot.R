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
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (SV, x = 1, y = 2, title = "Biplot", obs.names = NULL, 
#'     obs.opt = list(cex = 2), obs.label.size = obs.size, obs.col.palette = NULL, 
#'     var.names.x = NULL, var.names.y = NULL, var.color.x = NULL, 
#'     var.color.y = NULL, ...) 
#' {
#'     nvar.x = nrow(SV$scores$corr.X.xscores)
#'     nvar.y = nrow(SV$scores$corr.Y.xscores)
#'     SV = list(x = SV$scores$xscores, rotation = rbind(SV$scores$corr.X.xscores, 
#'         SV$scores$corr.Y.xscores), sdev = SV$cor)
#'     colnames(SV$rotation) = paste("CC", 1:ncol(SV$rotation), 
#'         sep = "")
#'     colnames(SV$x) = paste("CC", 1:ncol(SV$x), sep = "")
#'     var.names = c(var.names.x, var.names.y)
#'     if (is.null(var.color.x)) 
#'         var.color.x = "#444F51"
#'     if (is.null(var.color.y)) 
#'         var.color.y = "#9b0014"
#'     var.color = c(rep(var.color.x, length.out = nvar.x), rep(var.color.y, 
#'         length.out = nvar.y))
#'     pc.biplot(SV, x = x, y = y, title = title, obs.names = obs.names, 
#'         obs.opt = obs.opt, obs.label.size = obs.label.size, obs.col.palette = obs.col.palette, 
#'         var.names = var.names, var.opt = list(col = var.color), 
#'         ...)
#'   }
#' 
#' @export cc.biplot
cc.biplot <-
function(SV, x=1, y=2, main="Biplot",
         obs.col.palette=NULL,  
         obs.label.size =2,
                      obs.names=NULL, obs.opt=list(cex=2), 
                      var.names.x=NULL, var.names.y=NULL, 
                      var.color.x=NULL, var.color.y=NULL, ...) {
  
  nvar.x=nrow(SV$scores$corr.X.xscores)
  nvar.y=nrow(SV$scores$corr.Y.xscores)
  SV=list(u=SV$scores$xscores,
          v=rbind(SV$scores$corr.X.xscores,SV$scores$corr.Y.xscores),
          d=SV$cor)
  colnames(SV$v)=paste("CC",1:ncol(SV$v),sep="")
  colnames(SV$u)=paste("CC",1:ncol(SV$u),sep="")
  names(SV$d)=paste("cor",1:length(SV$d),sep="")
  
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
