#' Logistic
#'
#' @param l matrix containing outcome and covariates
#'
#' @return
#'
#' @export
#'



Logistic <- function(l){
  lr <- glm(ind_train~.-1, data=l, family = binomial(link = "logit"))
  return(lr$coefficients)
}
