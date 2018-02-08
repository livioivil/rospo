#' @name plot_effects_individual
#' @title plot_effects_individual
#' @author livio finos
#' @param data
#' @param pred_name
#' @param predict_funct a function that makes predictions
#' @param npoints 10
#' @export
#' @examples
#' n=50
#' x=matrix(rnorm(n*3),n,3)
#' x[,2]=sign(x[,2])
#' y=rnorm(n,X[,1]+X[,1]*X[,2])
#' x=data.frame(x)
#' x$y=y
#' mod=lm(y~X1*X2+X3,data=x)
#' summary(mod)
#' 
#' predict_funct=function(newdata) predict(mod,newdata=newdata)
#' plot_effects_individual(x,"X1","y",predict_funct=predict_funct)
#' plot_effects_individual(x,"X1","y",predict_funct=predict_funct,col.by = x$X2)
#' plot_effects_individual(x,"X3","y",predict_funct=predict_funct,center_effs = TRUE)
#' plot_effects_individual(x,"X3","y",predict_funct=predict_funct,center_effs = FALSE)
#' 

plot_effects_individual <- function(data,pred_name,resp_name,predict_funct,
                         col.by=NULL,npoints=10,center_effs=TRUE){
  rownames(data)=NULL
  rng=range(data[,pred_name])
  pred_values=seq(from=rng[1],to=rng[2],length.out = npoints)

  pred_id=grep(pred_name,colnames(data))

  mat_lines=plyr::llply(1:nrow(data),make_line_pred,
                        pred_id=pred_id,data=data,
                        pred_values=pred_values,
                        pred_name=pred_name,
                        center_effs=center_effs,
                        predict_funct=predict_funct)
  names(data)=gsub(pred_name,"pred_var",names(data))
  names(data)=gsub(resp_name,"resp_var",names(data))
  if(is.null(col.by)) data$col.by=factor(1) else
    data$col.by=factor(col.by)
  
  if(center_effs) {
    centers=plyr::laply(mat_lines,function(x){attr(x,"scaled:center")})
  } else {
      centers=rep(0,nrow(data))
    }
  names(mat_lines)=1:length(mat_lines)
  mat_lines=as.data.frame(mat_lines)
  mat_lines=t(mat_lines)

  #ggplot needs a dataframe
  mat_lines <- as.data.frame(mat_lines)
  #id variable for position in matrix 
  mat_lines$id <- 1:nrow(mat_lines) 
  #reshape to long format
  plot_data <- reshape2::melt(mat_lines,id.var="id")
  plot_data$col.by=data$col.by
  plot_data$pred_var=rep(pred_values,each=nrow(data))
  pp=ggplot2::ggplot()+ geom_line(data=plot_data, 
                     aes(x=pred_var,y=value,group=id,colour=col.by))
  pp=pp+ theme(legend.position="none")+labs(x=pred_name,y=resp_name)
  pp=pp+geom_point(data=data, aes(x=pred_var,y=resp_var,colour=col.by))
   pp
}

############
make_line_pred <- function(i,pred_id,data,pred_values,pred_name,center_effs,predict_funct){
  newdata=data.frame(pred_values,data[i,-pred_id],row.names=NULL)
  names(newdata)[1]=pred_name
  pred_vals=predict_funct(newdata)
  pred_vals=scale(pred_vals,scale = FALSE,center = center_effs)
  pred_vals
}

