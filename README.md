# mylm

`mylm` is a simple R package for ordinary least squares (OLS) linear regression. It re-implements the main features of `stats::lm()` by solving the normal equations:

$$
X^T X \hat{\beta} = X^T y
$$

The package provides functions for fitting linear models, viewing model summaries, and making predictions.

This package was developed for Biostat 625 at the University of Michigan.

## Installation

You can install `mylm` from GitHub:

```r
# install.packages("remotes")
remotes::install_github("romoala/mylm", build_vignettes = TRUE)
```

## Features

The package provides the following functions:

- `mylm()`: Fits a linear regression model.
- `print()`: Displays the model call and estimated coefficients.
- `summary()`: Shows coefficient estimates, standard errors, t-statistics, p-values, R-squared, adjusted R-squared, and the F-statistic.
- `predict()`: Predicts values using a fitted model.

The package supports multiple predictors, categorical variables, and interaction terms.

## Example

We use the built-in `mtcars` dataset to predict fuel efficiency (`mpg`) using vehicle weight (`wt`) and horsepower (`hp`).
```r
library(mylm)

fit = mylm(mpg ~ wt + hp, data = mtcars)

# View fitted model
fit

# View model summary
summary(fit)
```

The model gives an R-squared of approximately 0.827, meaning that weight and horsepower explain about 82.7% of the variation in fuel efficiency in this dataset.

We can also predict fuel efficiency for new cars:
```r
newdata = data.frame(
  wt = c(2.5, 3.0),
  hp = c(100, 150)
)

predict(fit, newdata = newdata)
```

Output:
```
       1        2
24.35540 20.82784
```

## Comparison with `lm()`

We compare `mylm()` with the original `stats::lm()` to check whether the results are correct.
```
fit_my = mylm(mpg ~ wt + hp, data = mtcars)
fit_lm = lm(mpg ~ wt + hp, data = mtcars)

all.equal(
  unname(fit_my$coefficients),
  unname(coef(fit_lm))
)
#> [1] TRUE

all.equal(
  summary(fit_my)$coefficients,
  summary(fit_lm)$coefficients
)
#> [1] TRUE
```

The comparisons return `TRUE`, showing that the results agree within numerical tolerance.

We also tested models with categorical variables, interaction terms, and simulated data. All comparisons in these examples returned `TRUE`.

## Performance

We used `bench::mark()` to compare the speed and memory usage of `mylm()` and `lm()`.

| Dataset                                    | Function | Median time | Memory  |
| ------------------------------------------ | -------- | ----------- | ------- |
| Small (`mtcars`)                           | `mylm()` | 182 µs      | 8.16 KB |
| Small (`mtcars`)                           | `lm()`   | 184 µs      | 6.45 KB |
| Large (10,000 observations, 20 predictors) | `mylm()` | 3.48 ms     | 12.2 MB |
| Large (10,000 observations, 20 predictors) | `lm()`   | 3.64 ms     | 10.4 MB |

In these tests, `mylm()` was slightly faster than `lm()`, but used more memory. The differences in execution time were small.

The results may vary between computers and runs.

For more examples and detailed comparisons, see the package vignette:
```
vignette("mylm", package = "mylm")
```

## Limitations

- Models with perfectly collinear predictors are not supported.
- Weights, offsets, and the `subset` argument are not supported.
- The normal-equation method can be less numerically stable than the QR method used by `lm()`, especially when predictors are highly correlated.
- The package does not support all features of `stats::lm()`.
- Intercept-only models (y ~ 1) are not supported.

## License

MIT
