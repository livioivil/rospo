#' which.hasname
#' 
#' which.hasname
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param X %% ~~Describe \code{X} here~~
#' @param name %% ~~Describe \code{name} here~~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (X, name) 
#' {
#'     which(names(X) == name)
#'   }
#' 
#' @export which.hasname
which.hasname <-
function(X,name){
  which(names(X)==name)
}
