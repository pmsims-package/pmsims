# Parse and validate input specifications

This function validates the provided data, model, and metric
specifications, and returns corresponding generator functions for each.
It ensures that all required inputs are provided and correctly
configured.

## Usage

``` r
parse_inputs(data_spec, metric, model)
```

## Arguments

- data_spec:

  A list containing two elements:

  `type`

  :   A character string indicating the outcome type.

  `args`

  :   A list of arguments to be passed to the data-generating function.

- metric:

  A character vector specifying one or more metrics to be used.
  Currently, only the first element is used.

- model:

  A character string specifying the model to be used.

## Value

A list containing three elements:

- `data_function`:

  The data-generating function.

- `model_function`:

  The model-generating function.

- `metric_function`:

  The metric function corresponding to the chosen metric.

## Details

This function calls
[`default_data_generators()`](https://pmsims-package.github.io/pmsims/reference/default_data_generators.md),
`default_model_generators()`, and `default_metric_generator()` to
construct the appropriate functions based on the supplied inputs.
