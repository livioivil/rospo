#'@name freq_xtable_totale
#'@title freq_xtable_totale
#'@description as xtable, but put a line between befor the total. Also substitute `Sum` with `Totale`
#'@param tab a freq table
#'@param digits as in xtable

freq_xtable_totale <-function(tab,digits = c(0,0,2)){
  rownames(tab)=gsub("Sum","Totale",rownames(tab))
  xtab=  xtable(tab,digits = digits)
  print(xtab,hline.after = getOption("xtable.hline.after", c(-1,0,nrow(xtab)-1,nrow(xtab))))
}
