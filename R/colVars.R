#' same as col/rowMeans but for variance
#' 
#' same as col/rowMeans but for variance
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases colVars rowVars
#' @param X %% ~~Describe \code{X} here~~
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
#' function (X, ...) 
#' apply(X, 2, var, ...)
#' 
#' @export colVars
colVars <-
function(X,...) apply(X,2,var,...)
