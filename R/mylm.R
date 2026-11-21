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
#'   \item{terms}{The terms object of the model.}
#'   \item{xlevels}{Levels of factor predictors, used by [predict.mylm()].}
#' }
#'
#' @examples
#' fit = mylm(mpg ~ wt + hp, data = mtcars)
#' fit
#' @export
mylm = function(formula, data){
  call = match.call()

  # Build the design matrix and response from the formula
  mf = model.frame(formula, data)
  X = model.matrix(attr(mf, "terms"), mf)
  y = model.response(mf)

  # Solve the normal equation
  XtX = t(X) %*% X
  Xty = t(X) %*% y
  beta = solve(XtX, Xty) # \hat{\beta}

  # Fitted value and residual
  fitted_y = X %*% beta # \hat{y}
  residuals_e = y - fitted_y

  # Sample size, number of coefficients, residual df
  n = nrow(X)
  p = ncol(X)
  df = n - p

  # Estimate residual variance
  RSS = sum(residuals_e^2)
  sigma_squared = RSS/df

  # Standard errors of coefficients
  var_beta = sigma_squared * solve(XtX) # \hat{Var(\hat{\beta})}
  se_beta = sqrt(diag(var_beta))

  # t-test and p-value
  t_values = drop(beta) / se_beta
  p_values = 2 * pt(-abs(t_values), df = df)

  # R-squared (TSS consider no-intercept case)
  intercept = attr(attr(mf, "terms"), "intercept")
  if (intercept == 1){
    TSS = sum((y-mean(y))^2)
  }else{
    TSS = sum(y^2)
  }
  R_squared = 1 - RSS/TSS

  # Store the results in a list
  result = list(
    call = call,
    coefficients = drop(beta),
    standard.errors = se_beta,
    t.values = t_values,
    p.values = p_values,
    fitted.values = drop(fitted_y),
    residuals = drop(residuals_e),
    R.squared = R_squared,
    df.residual = df,
    terms = attr(mf, "terms"),
    xlevels = .getXlevels(attr(mf, "terms"), mf)
  )

  # Assign an S3 class for print(), summary(), and predict()
  class(result) = "mylm"
  return(result)
}

#' Print a mylm Object
#'
#' Prints the call and estimated coefficients of a fitted `mylm` model.
#'
#' @param x An object of class `"mylm"`.
#' @param ... Further arguments (currently ignored).
#'
#' @return The input object `x`, invisibly.
#' @export
print.mylm = function(x,...){
  cat("\nCall:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(signif(x$coefficients,5))
  cat("\n")
  invisible(x)
}
