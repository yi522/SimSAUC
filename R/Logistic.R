#' Logistic
#'
#' This a function to get MLE of a logitic model while fixing the first component
#' of coefficents to be 1.
#'
#'
#' @param l a matrix containing outcome and markers. Please put outcome in the
#' first column of the matrix, and put markers in the rest columns
#' @return it returns the MLE of coefficients while fixing the first component
#' of coefficents to be 1.
#' @export

Logistic <- function(l){
  lr <- glm(ind_train~.-1, data=l, family = binomial(link = "logit"))
  return(lr$coefficients)
}
