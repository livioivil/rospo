setwd("/home/livio/github/rospo")
rm(list=ls())

sapply(dir("./functions/",full.names = TRUE),source)

Y=matrix(rnorm(30),10,3)
sv=svd(Y)

pc.biplot(sv)
