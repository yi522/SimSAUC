#' Logistic
#' @param l matrix containing outcome and covariates
#' @return
#' @export

Logistic <- function(l){
  x1 <- l[, 2]
  ll <- l[, -2]
  lr <- glm(ind_train~.-1, data=ll, offset = x1,family = binomial(link = "logit"))
  return(c(1, lr$coefficients))
}
