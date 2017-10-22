#'@name print_xtable_with_dimnames
#'@title print_xtable_with_dimnames
#'@param x as in xtable
#'@description print_xtable_with_dimnames
#'@export print_xtable_with_dimnames

print_xtable_with_dimnames <- function(x,names_rows_cols=c("Rows","Cols"),
                                       hline.after=getOption("xtable.hline.after", c(-1,0,nrow(x))),
                                       ...){
  hline0=(any(hline.after==-1))
  hline.after=hline.after[hline.after!=-1]
  xtable::print.xtable(x,hline.after=hline.after,
               add.to.row=list(
                 pos=list(-1),
                 command=paste(sep="",
                 ifelse(hline0," \\hline ",""),
                 "& \\multicolumn{4}{c}{",names_rows_cols[2],"} \\\\ ",
                 names_rows_cols[1])))
}
