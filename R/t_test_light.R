#' @name t.test.light
#' @title t.test.light
#' @export t.test.light
#' @aliases t.test.light
#' @title t.test.light
#' @description t.test.light
#' @param Y n X p matrix or data.frame
#' @param tail +1,-1 or 0
#' @param group if \code{NULL}, it performs a one sample t-test
#' @return a list with the following vectors:\code{p,t,df,means,sds}
#' @examples 
#' Y=matrix(rnorm(30),10,3)
#' x=rep(0:1,5)
#' t.test.light(Y,group=x)
#' summary(lm(Y[,1]~x))
t.test.light <- function(Y,tail=1,group=NULL){
  if (is.null(group)){
    means=colMeans(Y,na.rm=TRUE)
    sds=plyr::aaply(Y,2,sd,na.rm=TRUE)
    n=colSums(!is.na(Y))
    ts= means/sds*sqrt(n)
    if(tail==0)  return(list(p=2*pt(-abs(ts),df = n-1),t=ts,df=n-1))
    return(list(p=pt(-sign(tail)*ts,df = n-1),t=ts,df=n-1,means=means,sds=sds))
  } else {
    grps_labls=unique(group)
    n=colSums(!is.na(Y[group==grps_labls[1],]))
    n2=colSums(!is.na(Y[group==grps_labls[2],]))
    means1=colSums(Y[group==grps_labls[1],],na.rm=TRUE)/n
    means2=colSums(Y[group==grps_labls[2],],na.rm=TRUE)/n2
    
    sds=colSums(Y[group==grps_labls[1],]^2,na.rm=TRUE)-(means1^2)*n
    devres2=colSums(Y[group==grps_labls[2],]^2,na.rm=TRUE)-(means2^2)*n2
    sds=sds+devres2
    rm(devres2)
    

    meansdiff=means2-means1
    # n=n+n2
    sds=sqrt(sds/(n+n2-2)*(1/n+1/n2))
    rm(means1,means2)
    
    ts= meansdiff/sds
    if(tail==0)  return(list(p=2*pt(-abs(ts),df = n+n2-2),t=ts,df=n+n2-2))
    return(list(p=pt(-sign(tail)*ts,df = n+n2-2),t=ts,df=n+n2-2,
                meansdiff=meansdiff,sderr=sds))
  }
}
