#'@name freq_xtable_totale
#'@title freq_xtable_totale
#'@description as xtable, but put a line between befor the total. Also substitute `Sum` with `Totale`
#'@param tab a freq table
#'@param digits as in xtable
#'@export

freq_xtable_totale <-function(tab,digits = NULL,...){
  rownames(tab)=gsub("Sum","Totale",rownames(tab))
  if(is.null(digits)) digits=c(0,rep(c(0,2),length.out = ncol(tab)))
  xtab=  xtable(tab,digits = digits,align =c("l",rep('r',ncol(tab))))
  print(xtab,hline.after = getOption("xtable.hline.after", c(-1,0,nrow(xtab)-1,nrow(xtab))),...)
}
