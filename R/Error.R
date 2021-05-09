#' Error
#' @param l matrix containing outcome and covariates from test set
#' @param prop_case proportion of cases in the training set
#' @param beta coefficients of logistic model
#' @return
#' @export


Error <- function(l, prop_case, beta){
  ind <- l[, 1]
  z <- l[, 2:ncol(l)]
  samplesize <- length(ind)
  mu <- as.matrix(z)%*%as.matrix(as.numeric(beta))
  p <- 1/(1-exp(mu))
  n_risk <- as.integer(samplesize*prop_case)
  idd <- order(p)[(samplesize-n_risk+1):samplesize]
  ind_est <- rep(0, samplesize)
  ind_est[idd] <- 1
  res <- 1-length(which(ind==ind_est))/samplesize
  return(res)
}
