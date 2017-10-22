#'@name freq_xtable_totale
#'@title freq_xtable_totale
#'@description as xtable, but put a line between befor the total. Also substitute `Sum` with `Totale`
#'@param tab a freq table
#'@param digits as in xtable
#'@export

freq_xtable_totale <-function(tab,digits = NULL,...){
  if(length(dim(tab))==1)
    tab=cbind(f=tab)
  rownames(tab)=gsub("Sum","Totale",rownames(tab))
  if(is.null(digits)) digits=c(0,rep(c(0,2),length.out = ncol(tab)))
  if(any(colnames(tab)=="Sum")){colnames(tab)=gsub("Sum","Totale",colnames(tab))
   align=c("l",rep('r',ncol(tab)-1),'|l')} 
  else align=c("l",rep('r',ncol(tab)))
  xtab=  xtable::xtable(tab,digits = digits,align =align)
  print(xtab,hline.after = getOption("xtable.hline.after", c(-1,0,nrow(xtab)-1,nrow(xtab))),...)
}
