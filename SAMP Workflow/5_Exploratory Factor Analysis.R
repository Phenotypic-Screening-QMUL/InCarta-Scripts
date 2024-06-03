#Import the data
library("dplyr")

#Load Files

Ind <- read.csv ("OIS Ind Scale.csv")

#Remove NA
Ind <- Ind %>% replace(is.na(.), 0)

#Build extract factor loadings functions 

getS3method("print","loadings") #get the hidden method and modify it
printLoadings <- function (x, digits = 3, cutoff = 0.5, sort = TRUE, ...) 
{
  Lambda <- unclass(x)
  p <- nrow(Lambda)
  factors <- ncol(Lambda)
  if (sort) {
    mx <- max.col(abs(Lambda))
    ind <- cbind(1L:p, mx)
    mx[abs(Lambda[ind]) < 0.5] <- factors + 1
    Lambda <- Lambda[order(mx, 1L:p), ]
  }
  cat("\nLoadings:\n")
  fx <- format(round(Lambda, digits))
  names(fx) <- NULL
  nc <- nchar(fx[1L], type = "c")
  fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
  newx <- print(fx, quote = FALSE, ...) # I assigned this to a variable
  vx <- colSums(x^2)
  varex <- rbind(`SS loadings` = vx)
  if (is.null(attr(x, "covariance"))) {
    varex <- rbind(varex, `Proportion Var` = vx/p)
    if (factors > 1) 
      varex <- rbind(varex, `Cumulative Var` = cumsum(vx/p))
  }
  cat("\n")
  print(round(varex, digits))
  invisible(newx) #previously returned x
}

library(psych)
library("REdaS")
library("GPArotation")
library(dplyr)
library("corrplot")

test <- (Ind[,-c(1,2)])

#Generate FA
FA <- factanal(test, factors=8, lower = 0.5, scores = "regression", rotation = "oblimin")

#Extract Scores
score <-(FA$scores)
Ind_Name <- as.data.frame(Ind[,2])
score_test <- cbind.data.frame(Ind_Name,score)
write.csv(score_test, "score_Ind.csv")


#Extract Loadings
mmm <- printLoadings(FA$loadings)
write.csv(as.matrix(mmm), "loadings_Ind.csv")


#Extract Loadings for polar
mmm <- printLoadings(FA$loadings)
write.csv(FA$loadings, "Loadings Ind for Polar.csv")