#'@name index_gini
#'@alias index_entropy
#'@param frequency table
#'@export index_gini index_entropy
#'@title Entropy Indices
#'@description Entropy Indices

index_gini <- function(fr){
  fr=fr[!is.na(fr)]
  fr=prop.table(fr)
  1-sum(fr^2)
}

index_entropy <- function(fr){
  fr=fr[!is.na(fr)]
  fr=prop.table(fr)
  fr=fr[fr>0]
  -sum(fr*log(fr))
}