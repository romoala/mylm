#' Fit a Linear Regression Model
#'
#' Fits a linear regression model by solving the normal equations
#' \eqn{X^T X \beta = X^T y}. Results are comparable to [stats::lm()].
#'
#' @param formula A formula describing the model, e.g. `y ~ x1 + x2`.
#' @param data A data frame containing the variables in the formula.
#'
#' @return An object of class `"mylm"`, a list containing:
#' \describe{
#'   \item{call}{The matched call.}
#'   \item{coefficients}{Named vector of estimated coefficients.}
#'   \item{standard.errors}{Standard errors of the coefficients.}
#'   \item{t.values}{t statistics.}
#'   \item{p.values}{Two-sided p-values.}
#'   \item{fitted.values}{Fitted values.}
#'   \item{residuals}{Residuals.}
#'   \item{R.squared}{Coefficient of determination.}
#'   \item{df.residual}{Residual degrees of freedom.}
#' }
#'
#' @examples
#' fit <- mylm(mpg ~ wt + hp, data = mtcars)
#' fit
#'
#' @export
mylm = function(formula, data){
  ...
}

#' Print a mylm Object
#'
#' Prints the call and estimated coefficients of a fitted `mylm` model.
#'
#' @param x An object of class `"mylm"`.
#' @param ... Further arguments (currently ignored).
#'
#' @return The input object `x`, invisibly.
#'
#' @export
print.mylm = function(x,...){
  ...
}

mylm = function(formula, data){
  call = match.call()

  mf = model.frame(formula, data)
  X = model.matrix(attr(mf, "terms"), mf)
  y = model.response(mf)

  XtX = t(X) %*% X
  Xty = t(X) %*% y
  beta = solve(XtX, Xty) # \hat{\beta}

  fitted_y = X %*% beta # \hat{y}
  residuals_e = y - fitted_y

  n = nrow(X)
  p = ncol(X)
  df = n - p

  RSS = sum(residuals_e^2)
  sigma_squared = RSS/df

  var_beta = sigma_squared * solve(XtX) # \hat{Var(\hat{\beta})}
  se_beta = sqrt(diag(var_beta))

  t_values = drop(beta) / se_beta
  p_values = 2 * pt(-abs(t_values), df = df)

  TSS = sum((y - mean(y))^2)
  R_squared = 1 - RSS/TSS

  result = list(
    call = call,
    coefficients = drop(beta),
    standard.errors = se_beta,
    t.values = t_values,
    p.values = p_values,
    fitted.values = as.vector(fitted_y),
    residuals = as.vector(residuals_e),
    R.squared = R_squared,
    df.residual = df
  )
  class(result) = "mylm"
  return(result)
}

print.mylm = function(x,...){
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(signif(x$coefficients,5))
  cat("\n")
  invisible(x)
}
