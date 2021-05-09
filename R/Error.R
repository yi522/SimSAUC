#' Error
#'
#' This function calculate error rate of a logistic model in the given data.
#'
#'
#' @param l a matrix containing outcome and markers. Please put outcome in the
#' first column of the matrix, and put markers in the rest columns
#' @param prop_case threshold for decsion rule
#' @param beta coefficients of logistic model
#' @return it returns the error rate which is the proportion of falsely
#' classified subjects in the given data.
#' @export


Error <- function(l, prop_case, beta){
  ind <- l[, 1]
  z <- l[, 2:ncol(l)]
  samplesize <- length(ind)
  mu <- as.matrix(z)%*%as.matrix(as.numeric(beta))
  p <- exp(mu)/(1+exp(mu))
  n_risk <- as.integer(samplesize*prop_case)
  idd <- order(p)[(samplesize-n_risk+1):samplesize]
  ind_est <- rep(0, samplesize)
  ind_est[idd] <- 1
  res <- 1-length(which(ind==ind_est))/samplesize
  return(res)
}
