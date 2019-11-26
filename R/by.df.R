#' by.df
#' 
#' Same as df, but Value is a data.frame, not a list.
#' 
#' Come df, ma value e' un data.frame e non una list
#' 
#' 
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (...) 
#' {
#'     rr = by(...)
#'     attr(rr, "class") = "list"
#'     rr = data.frame(rr)
#'     rr = t(rr)
#'     if (!is.null(list(...)$INDICES)) {
#'         rownames(rr) = unique(list(...)$INDICES)
#'     }
#'     else rownames(rr) = unique(list(...)[[2]])
#'     rr
#'   }
#' 
#' @export by.df
by.df <-
function(...){
  rr=by(...)
  attr(rr,"class")="list"
  rr=data.frame(rr)
  rr=t(rr)
  if(!is.null(list(...)$INDICES)){
    rownames(rr)=unique(list(...)$INDICES)
  } else rownames(rr)=unique(list(...)[[2]])
  rr
}
