#' by.df
#' 
#' Same as df, but Value is a data.frame, not a list.
#' 
#' Come df, ma value e' un data.frame e non una list
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
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
