#' Predict Method for mylm Fits
#'
#' Computes predicted values from a fitted `mylm` model.
#'
#' @param object An object of class `"mylm"`.
#' @param newdata An optional data frame in which to look for variables
#'   used for prediction. If omitted, the fitted values are returned.
#' @param ... Further arguments (currently ignored).
#'
#' @return A named numeric vector of predicted values.
#'
#' @examples
#' fit <- mylm(mpg ~ wt + hp, data = mtcars)
#' predict(fit, newdata = data.frame(wt = c(2.5, 3), hp = c(100, 150)))
#'
#' @export
predict.mylm = function(object, newdata, ...){
  # If no new data, return the fitted values
  if (missing(newdata)) {
    return(object$fitted.values)
  }

  # Remove the response from terms, since newdata has no y
  tt = delete.response(object$terms)

  # Build the design matrix for newdata using the same formula and factor levels
  mf_new = model.frame(tt, newdata, xlev = object$xlevels)
  X_new = model.matrix(tt, mf_new)

  # Compute predicted values
  predictions = as.vector(X_new %*% object$coefficients)

  # Preserve row names
  names(predictions) = rownames(mf_new)

  return(predictions)
}
