## AUROC2
## Area under ROC type 2 curve
## translated from matlab script type2roc.m by Fleming & Lau (2014)
## taken from 
## https://github.com/smfleming/meta_dots/blob/master/type2roc.m
## authored: Steve Fleming 2012

calculate_auroc2 <- function(corrects, subjectiveRatings, numRatings) {
  ratings <- rev(c(1:numRatings))
  
  hits <- function(x) {subjectiveRatings==x & corrects}
  falsealarms <- function(x) {subjectiveRatings==x & abs(corrects-1)}
  
  type2hits_list <- lapply(ratings,hits)
  type2falsealarms_list <- lapply(ratings,falsealarms)
  
  type2hits_listsums <- 0.5 + sapply(type2hits_list,sum)
  type2falsealarms_listsums <- 0.5 + sapply(type2falsealarms_list,sum)
  
  H2s <- type2hits_listsums / sum(type2hits_listsums)
  FA2s <- type2falsealarms_listsums / sum(type2falsealarms_listsums)
  
  cumulative_H2s <- c(0,cumsum(H2s))
  cumulative_FA2s <- c(0, cumsum(FA2s))
  
  k <- function(i) {(cumulative_H2s[i+1] - cumulative_FA2s[i])^2 - (cumulative_H2s[i] - cumulative_FA2s[i+1])^2}
  ## k(i) = (cum_H2(c+1) - cum_FA2(c))^2 - (cum_H2(c) - cum_FA2(c+1))^2;
  
  ks <- sapply(rev(ratings), k)
  
  aoc <- 0.5 + 0.25*sum(ks)
  return(aoc)
}
