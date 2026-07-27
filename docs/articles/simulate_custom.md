# Defining custom simulation workflows

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
2.  `mlbench`, which provides the `BreastCancer` dataset
3.  `synthpop` package, which will allow us to generate a large
    synthetic dataset based on the `BreastCancer` dataset.
4.  `glmnet` for the elastic net modelling function.
5.  `DescTools`, which provides the Brier score function used to assess
    model performance.

We will estimate the minimum sample size required for a prediction model
to distinguish malignant from benign samples.

[`library`](https://rdrr.io/r/base/library.html)`(`[`pmsims`](https://pmsims-package.github.io/pmsims/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(``mlbench``)`

    ## Warning: package 'mlbench' was built under R version 4.6.1

[`library`](https://rdrr.io/r/base/library.html)`(``synthpop``)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`DescTools`](https://andrisignorell.github.io/DescTools/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`glmnet`](https://glmnet.stanford.edu)`)`

## Defining your custom components

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

For this example we use the `BreastCancer` dataset from the `mlbench`
package. It contains cytological characteristics of benign and malignant
samples. For more details, see the [help
page](https://search.r-project.org/CRAN/refmans/mlbench/html/BreastCancer.html).
We use `synthpop` to create a synthetic population and then sample
datasets of the required size from it.

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1234``)`` `[`data`](https://rdrr.io/r/utils/data.html)`(``"BreastCancer"``, package ``=`` ``"mlbench"``)`` ``real_data`` ``<-`` ``stats``::`[`na.omit`](https://rdrr.io/r/stats/na.fail.html)`(``BreastCancer``)`` ``real_data``$``Id`` ``<-`` ``NULL`` ``real_data``$``Class`` ``<-`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(``real_data``$``Class`` ``==`` ``"malignant"``)`` ``real_data``[``]`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`` `` ``real_data``,`` `` ``function``(``x``)`` ``if`` ``(`[`is.factor`](https://rdrr.io/r/base/factor.html)`(``x``)``)`` `[`as.numeric`](https://rdrr.io/r/base/numeric.html)`(`[`as.character`](https://rdrr.io/r/base/character.html)`(``x``)``)`` ``else`` ``x`` ``)`` `` ``synthetic_data`` ``<-`` ``synthpop``::`[`syn`](https://rdrr.io/pkg/synthpop/man/syn.html)`(`` `` ``real_data``,`` `` k ``=`` ``5000``,`` `` print.flag ``=`` ``FALSE``,`` `` minnumlevels ``=`` ``2`` ``)`

    ## 
    ## Variable(s): Class numeric but with only 2 or fewer distinct values turned into factor(s) for synthesis.

`synthetic_data``$``syn``$``Class`` ``<-`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(`[`as.character`](https://rdrr.io/r/base/character.html)`(``synthetic_data``$``syn``$``Class``)``)`` `` ``my_data_generator`` ``<-`` ``function``(`` `` ``n``,`` `` ``n_signal_parameters`` ``=`` ``9``,`` `` ``noise_parameters`` ``=`` ``0``,`` `` ``data`` ``=`` ``synthetic_data``$``syn`` ``)`` ``{`` `` ``data``[`[`sample`](https://rdrr.io/r/base/sample.html)`(`[`seq_len`](https://rdrr.io/r/base/seq.html)`(`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``data``)``)``, ``n``, replace ``=`` ``TRUE``)``, ``]`` ``}`` `` ``example_data`` ``<-`` ``my_data_generator``(``n ``=`` ``10``)`` `[`print`](https://rdrr.io/r/base/print.html)`(``example_data``)`

    ##      Cl.thickness Cell.size Cell.shape Marg.adhesion Epith.c.size Bare.nuclei
    ## 4886            8         6          3             2            5          10
    ## 2481            5         1          2             1            2           1
    ## 4038            1         1          1             3            1           1
    ## 3096           10         7          8            10            6          10
    ## 1199           10         8          7             3            4          10
    ## 2347            2         3          4             5            3           5
    ## 2764            3         1          1             1            2           1
    ## 3485            9         5          6             2            2          10
    ## 3207            1         1          1             1            2           1
    ## 3991            3         1          1             1            2           1
    ##      Bl.cromatin Normal.nucleoli Mitoses Class
    ## 4886           4               5       1     1
    ## 2481           1               1       1     0
    ## 4038           2               1       1     0
    ## 3096           7               6       1     1
    ## 1199           4               5       1     1
    ## 2347           5               3       3     0
    ## 2764           2               1       1     0
    ## 3485           4               1       2     1
    ## 3207           2               1       1     0
    ## 3991           3               1       1     0

### Defining the model function

Next, we need to define a model function. The model function needs to
take only the data returned by the data function as an argument. It must
return a fitted model object that can be used with our metric function.

We will use the `glmnet` package to fit an elastic net regression model,
setting the elastic net mixing parameter to 0.5. For this function, the
data must be in the form of a matrix. We aim to predict `Class` using
the remaining columns in the dataset.

`my_model_function`` ``<-`` ``function``(``data``)`` ``{`` `` ``data_matrix`` ``<-`` `[`as.matrix`](https://rdrr.io/r/base/matrix.html)`(``data``)`` `` ``outcome`` ``<-`` ``"Class"`` `` ``x`` ``<-`` ``data_matrix``[``, `[`colnames`](https://rdrr.io/r/base/colnames.html)`(``data_matrix``)`` ``!=`` ``outcome``, drop ``=`` ``FALSE``]`` `` ``y`` ``<-`` ``data_matrix``[``, ``outcome``]`` `` `` ``glmnet``::`[`cv.glmnet`](https://glmnet.stanford.edu/reference/cv.glmnet.html)`(`` `` ``x``,`` `` ``y``,`` `` family ``=`` ``"binomial"``,`` `` alpha ``=`` ``0.5``,`` `` nfolds ``=`` ``5`` `` ``)`` ``}`` `` ``example_data`` ``<-`` ``my_data_generator``(``n ``=`` ``100``)`` ``example_fitted_model`` ``<-`` ``my_model_function``(``example_data``)`` ``fitted_model`` ``<-`` ``my_model_function``(``example_data``)`

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

`my_metric`` ``<-`` ``function``(``test_data``, ``fitted_model``, ``model_name``)`` ``{`` `` ``test_data_matrix`` ``<-`` `[`as.matrix`](https://rdrr.io/r/base/matrix.html)`(``test_data``)`` `` ``y`` ``<-`` `[`which`](https://rdrr.io/r/base/which.html)`(`[`names`](https://rdrr.io/r/base/names.html)`(``test_data``)`` ``==`` ``"Class"``)`` `` ``x_test`` ``<-`` ``test_data_matrix``[``, ``-``y``]`` `` ``y_test`` ``<-`` ``test_data_matrix``[``, ``y``]`` `` ``predictions`` ``<-`` `[`predict`](https://rdrr.io/r/stats/predict.html)`(`` `` ``fitted_model``,`` `` newx ``=`` ``x_test``,`` `` s ``=`` ``"lambda.min"``,`` `` type ``=`` ``"response"`` `` ``)`` `` `` ``brier_score`` ``<-`` ``DescTools``::`[`BrierScore`](https://andrisignorell.github.io/DescTools/reference/BrierScore.html)`(``y_test``, pred ``=`` ``predictions``)`` `` `[`return`](https://rdrr.io/r/base/function.html)`(``-``brier_score``)`` ``}`` `[`attr`](https://rdrr.io/r/base/attr.html)`(``my_metric``, ``"metric"``)`` ``<-`` ``"brier_score"`` `[`attr`](https://rdrr.io/r/base/attr.html)`(``my_metric``, ``"value_on_error"``)`` ``<-`` ``-``1`` `` ``test_data`` ``<-`` ``my_data_generator``(``n ``=`` ``500``)`` ``my_metric``(``test_data``, ``example_fitted_model``, ``"elastic net regression"``)`

    ## [1] -0.04277218

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

In this example, we use a training sample of 3,000 to approximate the
maximum achievable performance. For some machine learning models,
particularly XGBoost, this may be insufficient, and larger samples may
be needed.

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1234``)`` ``maximum_achievable_data`` ``<-`` ``my_data_generator``(``n ``=`` ``3000``)`` ``test_data`` ``<-`` ``my_data_generator``(``n ``=`` ``1000``)`` ``test_model`` ``<-`` ``my_model_function``(``maximum_achievable_data``)`` ``maximum_achievable_performance`` ``<-`` ``my_metric``(`` `` ``test_data``,`` `` ``test_model``,`` `` ``"elastic net regression"`` ``)`` `[`print`](https://rdrr.io/r/base/print.html)`(``maximum_achievable_performance``)`

    ## [1] -0.03029851

We will also look at small-sample performance, which reflects what
happens when we have limited data. We run this a few times because
small-sample performance can be highly variable.

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1234``)`` ``small_sample_performance`` ``<-`` `[`rep`](https://rdrr.io/r/base/rep.html)`(``NA``, ``5``)`` ``for`` ``(``i`` ``in`` `[`seq_along`](https://rdrr.io/r/base/seq.html)`(``small_sample_performance``)``)`` ``{`` `` ``small_sample_data`` ``<-`` ``my_data_generator``(``n ``=`` ``50``)`` `` ``test_data`` ``<-`` ``my_data_generator``(``n ``=`` ``1000``)`` `` ``test_model`` ``<-`` ``my_model_function``(``small_sample_data``)`` `` ``small_sample_performance``[``i``]`` ``<-`` ``my_metric``(`` `` ``test_data``,`` `` ``test_model``,`` `` ``"elastic net regression"`` `` ``)`` ``}`` `` `[`print`](https://rdrr.io/r/base/print.html)`(``small_sample_performance``)`

    ## [1] -0.07020165 -0.05314556 -0.03743018 -0.04255960 -0.07313804

[`mean`](https://rdrr.io/r/base/mean.html)`(``small_sample_performance``)`

    ## [1] -0.05529501

## Running `simulate_custom()`

We are now ready to run
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md).
For illustration, we set the minimum acceptable performance slightly
below the estimated maximum. There are no universal criteria for
choosing a Brier-score target, so it is useful to explore how the
estimated minimum sample size changes across target values. The small
simulation budget below keeps the vignette quick; use larger values for
an analysis.

[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1234``)`` ``result`` ``<-`` `[`simulate_custom`](https://pmsims-package.github.io/pmsims/reference/simulate_custom.md)`(`` `` data_function ``=`` ``my_data_generator``,`` `` model_function ``=`` ``my_model_function``,`` `` metric_function ``=`` ``my_metric``,`` `` target_performance ``=`` ``maximum_achievable_performance`` ``-`` ``0.02``,`` `` mean_or_assurance ``=`` ``"mean"``,`` `` test_n ``=`` ``500``,`` `` min_sample_size ``=`` ``50``,`` `` max_sample_size ``=`` ``300``,`` `` n_reps_total ``=`` ``20``,`` `` n_reps_per ``=`` ``5``,`` `` method ``=`` ``"bisection"``,`` `` progress ``=`` ``FALSE`` ``)`

    ## Using user-specified min_sample_size and max_sample_size. Adaptive starting values will not be used.

[`print`](https://rdrr.io/r/base/print.html)`(``result``)`

    ## $outcome
    ## NULL
    ## 
    ## $min_n
    ## [1] 96
    ## 
    ## $perf_n
    ## logical(0)
    ## 
    ## $target_performance
    ## [1] -0.05029851
    ## 
    ## $summaries
    ## NULL
    ## 
    ## $data
    ## NULL
    ## 
    ## $data_function
    ## function (n, n_signal_parameters = 9, noise_parameters = 0, data = synthetic_data$syn) 
    ## {
    ##     data[sample(seq_len(nrow(data)), n, replace = TRUE), ]
    ## }
    ## <bytecode: 0xc416b2e40>
    ## 
    ## $model_function
    ## function (data) 
    ## {
    ##     data_matrix <- as.matrix(data)
    ##     outcome <- "Class"
    ##     x <- data_matrix[, colnames(data_matrix) != outcome, drop = FALSE]
    ##     y <- data_matrix[, outcome]
    ##     glmnet::cv.glmnet(x, y, family = "binomial", alpha = 0.5, 
    ##         nfolds = 5)
    ## }
    ## <bytecode: 0xc3f9313b8>
    ## 
    ## $metric_function
    ## function (test_data, fitted_model, model_name) 
    ## {
    ##     test_data_matrix <- as.matrix(test_data)
    ##     y <- which(names(test_data) == "Class")
    ##     x_test <- test_data_matrix[, -y]
    ##     y_test <- test_data_matrix[, y]
    ##     predictions <- predict(fitted_model, newx = x_test, s = "lambda.min", 
    ##         type = "response")
    ##     brier_score <- DescTools::BrierScore(y_test, pred = predictions)
    ##     return(-brier_score)
    ## }
    ## <bytecode: 0xc443902e0>
    ## attr(,"metric")
    ## [1] "brier_score"
    ## attr(,"value_on_error")
    ## [1] -1
    ## 
    ## $model
    ## NULL
    ## 
    ## $metric
    ## [1] "brier_score"
    ## 
    ## $c_statistic
    ## NULL
    ## 
    ## $test_n
    ## [1] 500
    ## 
    ## $min_sample_size
    ## [1] 50
    ## 
    ## $max_sample_size
    ## [1] 300
    ## 
    ## $n_reps_total
    ## [1] 20
    ## 
    ## $n_reps_per
    ## [1] 5
    ## 
    ## $method
    ## [1] "bisection"
    ## 
    ## $progress
    ## [1] FALSE
    ## 
    ## $verbose
    ## [1] FALSE
    ## 
    ## $simulation_time
    ## Time difference of 0.5285511 secs
    ## 
    ## $mean_or_assurance
    ## [1] "mean"

### Interpretation

The results show a minimum sample size of 96. This is calculated using
the mean criterion: the average negative Brier score across repeated
training samples meets the selected target.

Note that this is the minimum sample size required for the Brier score;
other performance metrics may require larger sample sizes to achieve
adequate performance.
