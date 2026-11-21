
#' Summarize a mylm Object
#'
#' Computes the coefficient table, residual standard error, R-squared,
#' adjusted R-squared, and F-statistic for a fitted `mylm` model.
#'
#' @param object An object of class `"mylm"`.
#' @param ... Further arguments (currently ignored).
#'
#' @return An object of class `"summary.mylm"`.
#'
#' @examples
#' fit = mylm(mpg ~ wt + hp, data = mtcars)
#' summary(fit)
#'
#' @export

summary.mylm = function(object, ...){

  # Coefficients table
  coef_table = cbind(
    Estimate = object$coefficients,
    "Std. Error" = object$standard.errors,
    "t value" = object$t.values,
    "Pr(>|t|)" = object$p.values)

  # Summary of residuals
  residual_summary = quantile(
    object$residuals,
    probs = c(0,0.25,0.5,0.75,1)
  )
  names(residual_summary) = c(
    "Min", "1Q", "Median", "3Q", "Max"
  )

  # Residual Standard Error
  RSS = sum(object$residuals^2)
  sigma = sqrt(RSS/object$df.residual)

  n = length(object$residuals)
  p = length(object$coefficients)
  intercept = attr(object$terms, "intercept")

  # Adjusted R-squared and F-statistics
  adj_r_squared = 1 - (1 - object$R.squared)*(n-intercept)/(n-p)

  df1 = p-intercept
  df2 = object$df.residual

  F_value = (object$R.squared /df1)/((1-object$R.squared)/df2)
  F_p_value = pf(F_value, df1, df2, lower.tail = FALSE)

  result = list(
    call = object$call,
    residuals = residual_summary,
    coefficients = coef_table,
    sigma = sigma,
    df.residual = object$df.residual,
    r.squared = object$R.squared,
    adj.r.squared = adj_r_squared,
    fstatistic = c(
      value = F_value,
      numdf = df1,
      dendf = df2),
    f.p.value = F_p_value)

  class(result)="summary.mylm"
  return(result)
}


#' Print a summary.mylm Object
#'
#' @param x An object of class `"summary.mylm"`.
#' @param ... Further arguments (currently ignored).
#'
#' @return The input object `x`, invisibly.
#'
#' @examples
#' fit = mylm(mpg ~ wt + hp, data = mtcars)
#' model_summary <- summary(fit)
#' print(model_summary)
#'
#' @export
print.summary.mylm = function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  cat("\nResiduals:\n")
  print(x$residuals)
  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients, P.values = TRUE)
  cat("\nResidual standard error:",
      format(x$sigma, digits = 4),
      "on", x$df.residual,
      "degrees of freedom\n")
  cat("Multiple R-squared:",
      paste0(format(x$r.squared, digits = 4),","),
      "Adjusted R-squared:",
      format(x$adj.r.squared, digits = 4),"\n")
  cat("F-statistic:",
      format(x$fstatistic["value"], digits = 4),
      "on",
      x$fstatistic["numdf"],
      "and",
      x$fstatistic["dendf"],
      "DF, p-value:",
      format.pval(x$f.p.value, digits=4),
      "\n")
  invisible(x)
}
