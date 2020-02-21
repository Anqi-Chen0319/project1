#' @title frequently used words in the paper title
#' @description use function to find the number of frequently used words in the paper title among the year
#' @param x wording string
#' @return y number of times uesd among years

count_A <- function(x) {
  x_countA <- rep(0,length(unique(data$yearsa)))
  for (i in 1:length(unique(data$yearsa))) {
    tmp <- filter(data,yearsa==unique_yearA[i])
    x_countA[i] <- sum(str_count(tmp$titlea,x),na.rm = TRUE) 
  }
  print(x_countA)
}



#' @title frequently used words in the paper title
#' @description use function to find the number of frequently used words in the paper title among the year
#' @param x wording string
#' @return y number of times uesd among years

count_B <- function(x) {
  x_countB <- rep(0,length(unique(datb$yearsb)))
  for (i in 1:length(unique(datb$yearsb))) {
    tmb <- filter(datb,yearsb==unique_yearB[i])
    x_countB[i] <- sum(str_count(tmb$titleb,x),na.rm = TRUE) 
  }
  print(x_countB)
}