# Defining custom simulation workflows with \`simulate_custom()\`

## Motivation

The
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md)
function allows users to define their own data generation, model
fitting, and performance metric functions. This provides flexibility to
calculate sample sizes for any prediction model, data type or
performance metric. The `pmsims` built-in sample size functions—such as
[`simulate_binary()`](https://pmsims-package.github.io/pmsims/reference/simulate_binary.md)—use
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md)
under the hood.

For this vignette, we’ll use five packages:

1.  `pmsims`
2.  `mlbench` package, which will provide the `PimaIndiansDiabetes`
    dataset
3.  `synthpop` package, which will allow us to generate a large
    synthetic dataset based on the `PimaIndiansDiabetes` dataset.
4.  `glmnet` for the elastic net modelling function.
5.  `DescTools`, which provides the Brier score function used to assess
    model performance.

We will try to estimate the minimum sample size required for a
prediction model to predict `diabetes`.

``` r
library(pmsims)
library(mlbench)
library(synthpop)
library(DescTools)
library(glmnet)
```

## Using `simulate_custom()`

The function requires three key functions:

1.  **Data function**: Generates synthetic datasets based on specified
    parameters.
2.  **Model function**: Function that fits a prediction model to the
    generated data.
3.  **Metric function**: Calculates the performance metric of interest.

In this example we will define a data function from a pre-existing
dataset, use elastic net regression, and assess performance using the
Brier Score, a performance metric for binary outcomes similar to mean
squared error.

### Defining the data generator

For this example we will use the `PimaIndiansDiabetes` data set from the
`mlbench` package. The dataset consists of diabetes test results. For
more details, see the [help
page](https://search.r-project.org/CRAN/refmans/mlbench/html/PimaIndiansDiabetes.html).
We will use the `synthpop` package to generate a very large synthetic
dataset. We will then sample from this dataset to obtain a dataset of
any required size.

``` r
set.seed(1234)
data("PimaIndiansDiabetes", package = "mlbench")
real_data <- PimaIndiansDiabetes
real_data$diabetes <- ifelse(real_data$diabetes == "pos", 1, 0)

synthetic_data <- synthpop::syn(
  real_data,
  k = 1000000,
  print.flag = FALSE,
  minnumlevels = 2
)
```

    ## 
    ## Variable(s): diabetes numeric but with only 2 or fewer distinct values turned into factor(s) for synthesis.

``` r
my_data_generator <- function(n, data = synthetic_data$syn) {
  data[sample(seq_len(nrow(data)), n, replace = FALSE), ]
}

example_data <- my_data_generator(n = 10)
print(example_data)
```

    ##        pregnant glucose pressure triceps insulin mass pedigree age diabetes
    ## 979690       10     125       70      37     122 33.1    0.647  43        1
    ## 501265        2      92       64      42     207 39.4    0.395  24        0
    ## 99101        13     106       70       0       0 34.2    0.297  63        0
    ## 840015        2     100       68      20      54 18.2    0.832  27        0
    ## 72928         1     144       58      20      83 26.2    0.529  33        0
    ## 625267        0     127       80      31       0 35.8    0.218  23        0
    ## 817900       12     140       88      33       0 32.0    0.244  51        0
    ## 829506        1      95       80      31      18 39.5    0.236  21        0
    ## 826615        2     100       68      15      84 24.6    0.154  28        0
    ## 665137        6      87       66       0       0 23.5    0.342  31        0

### Defining the model function

Next, we need to define a model function. The model function needs to
take only the data returned by the data function as an argument. It must
return a fitted model object that can be used with our metric function.

We will use the `glmnet` package to fit an elastic net regression model,
setting the elastic net mixing parameter to 0.5. For this function, the
data must be in the form of a matrix. We aim to predict `diabetes` using
the remaining columns in the dataset.

``` r
my_model_function <- function(data) {
  data_matrix <- as.matrix(data)
  outcome <- "diabetes"
  x <- data_matrix[, colnames(data_matrix) != outcome, drop = FALSE]
  y <- data_matrix[, outcome]

  glmnet::cv.glmnet(
    x,
    y,
    family = "binomial",
    alpha = 0.5,
    nfolds = 10
  )
}

example_data <- my_data_generator(n = 100)
example_fitted_model <- my_model_function(example_data)
fitted_model <- my_model_function(example_data)
```

### Metric function

Next, we define a metric function. For this example we use the Brier
score.

The metric function must take three positional arguments:

- `test_data`: The data used to evaluate model performance.
- `fitted_model`: The fitted model object returned by the model
  function.
- `model_name`: The model name string. This argument is required even if
  unused.

The metric function must return a single numeric value representing
model performance on the test data. Here we return the negative Brier
score, because by default `pmsims` assumes that higher values indicate
better performance. If your custom metric may fail for some datasets,
you can optionally set `attr(my_metric, "value_on_error")` to define the
fallback value returned for failed simulation runs.

``` r
my_metric <- function(test_data, fitted_model, model_name) {
  test_data_matrix <- as.matrix(test_data)
  y <- which(names(test_data) == "diabetes")
  x_test <- test_data_matrix[, -y]
  y_test <- test_data_matrix[, y]
  predictions <- predict(
    fitted_model,
    newx = x_test,
    s = "lambda.min",
    type = "response"
  )

  brier_score <- DescTools::BrierScore(y_test, pred = predictions)
  return(-brier_score)
}

test_data <- my_data_generator(n = 1000)
my_metric(test_data, example_fitted_model, "elastic net regression")
```

    ## [1] -0.1664593

### What is the maximum achievable performance?

Before running `pmsims`, it is important to estimate the *maximum
achievable performance* of your modelling strategy. When using the
wrapper functions (e.g.
[`simulate_binary()`](https://pmsims-package.github.io/pmsims/reference/simulate_binary.md)),
this is provided directly by the user through arguments such as
`maximum_achievable_cstatistic`. In
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md),
you must estimate the maximum achievable performance yourself using a
very large training sample. You can then use this estimate to choose
your `target_performance`.

When calculating the minimum sample size, we are looking for the
smallest sample size at which model performance meets a minimum
acceptable threshold. This threshold is passed through
`target_performance` and should be set below the maximum achievable
performance. The gap between these two quantities will influence the
minimum sample size returned.

In this example, we use a training sample of 10,000 to approximate the
maximum achievable performance. For some machine learning models,
particularly XGBoost, this may be insufficient, and larger samples may
be needed.

``` r
set.seed(1234)
maximum_achievable_data <- my_data_generator(n = 10000)
test_data <- my_data_generator(n = 30000)
test_model <- my_model_function(maximum_achievable_data)
maximum_achievable_performance <- my_metric(
  test_data,
  test_model,
  "elastic net regression"
)
print(maximum_achievable_performance)
```

    ## [1] -0.1529583

We will also look at small-sample performance, which reflects what
happens when we have limited data. We run this a few times because
small-sample performance can be highly variable.

``` r
set.seed(1234)
small_sample_performance <- rep(NA, 10)
for(i in 1:10) {
  small_sample_data <- my_data_generator(n = 50)
  test_data <- my_data_generator(n = 30000)
  test_model <- my_model_function(small_sample_data)
  small_sample_performance[i] <- my_metric(
    test_data,
    test_model,
    "elastic net regression"
  )
}

print(small_sample_performance)
```

    ##  [1] -0.2266960 -0.1725650 -0.1743998 -0.1844464 -0.1731943 -0.1978301
    ##  [7] -0.1789102 -0.1864845 -0.1856546 -0.1898357

``` r
mean(small_sample_performance)
```

    ## [1] -0.1870017

### Running `simulate_custom()`

We are now ready to run
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md).
We decide that a Brier score of -0.165 is the minimum acceptable
performance, allowing some degradation from the estimated maximum
achievable performance. This target is approximately halfway between
`maximum_achievable_performance` and the small-sample performance. For
the Brier score there are no established criteria for choosing the
target, so it may be useful to explore how the estimated minimum sample
size changes across different values of `target_performance`. For speed,
we will set the total number of replications to 1000. We use default
arguments for all other parameters.

``` r
set.seed(1234)
result <- simulate_custom(
  data_function = my_data_generator,
  model_function = my_model_function,
  metric_function = my_metric,
  target_performance = -0.165,
  n_reps_total = 1000,
  progress = FALSE
)
```

    ## Estimating first stage... (Adaptive starting value search algorithm)
    ## Starting values determined: min sample size = 160 max sample size = 320 
    ## Estimating second stage... (Gaussian process algorithm)

``` r
print(result)
```

    ##                     ┌────────────────────────────────────────┐
    ##                     │ pmsims: Sample size simulation summary │
    ##                     └────────────────────────────────────────┘
    ## ──────────────────────────────────── Inputs ────────────────────────────────────
    ##   Target for chosen performance metric :  = -0.165
    ##                        Simulation reps : <NA>
    ## ──────────────────────────────────── Results ───────────────────────────────────
    ##              Final minimum sample size : 213
    ##             Estimated performance at N : -0.165 ( = -0.165)
    ##            Estimated other metric at N : <NA> ()
    ##                                   Mode : Assurance
    ##                           Running time : 3 minutes 18 seconds
    ##     Assurance mode ensures the target metric is met with high probability across repeated datasets.

### Interpretation

The results show a minimum sample size of 213. This is calculated using
the assurance criterion, which means that we would expect 80% of models
developed on samples of this size to have a negative Brier score of
-0.165 or better.

Note that this is the minimum sample size required for the Brier score;
other performance metrics may require larger sample sizes to achieve
adequate performance.
