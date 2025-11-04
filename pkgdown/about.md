---
title: "How pmsims works"
---

# Overview

**pmsims** estimates the minimum sample size needed for a prediction model to achieve adequate performance with high probability (“assurance”).
It does this by simulating datasets, fitting models, evaluating performance, and tracing how performance improves as the sample size grows — a *learning curve*.

# 1. Conceptual background

Prediction models must be trained on enough data to generalise beyond the development sample.
Inadequate sample sizes lead to **overfitting**, unstable estimates, and poor calibration.
Traditional heuristics (e.g., *10 events per variable*) ignore factors such as outcome prevalence, predictor strength, and model complexity.
Analytic formulae (e.g., *pmsampsize*) rely on assumptions that often fail for modern models.

**Simulation-based approaches**, like pmsims, overcome these limitations by explicitly generating data and empirically assessing model behaviour across different training sizes.

# 2. The pmsims workflow

The package operationalises the simulation-based framework in four modular steps:

### (1) Define the simulation scenario
Specify:
- **Data generator** — how predictors and outcomes are simulated
  (continuous, binary, or time-to-event)
- **Model function** — the model to be fitted (e.g. logistic regression, penalised regression, ML)
- **Metric function** — one or more performance measures (AUC, calibration slope, etc.)

You also set the **target performance** (e.g. AUC ≥ 0.75)
and an **assurance level** (e.g. 80% probability that performance ≥ target).

### (2) Tune the data generator
The data-generating process is adjusted so that, for large simulated datasets,
the chosen model reaches the specified target performance.
This ensures simulations reflect realistic population characteristics.

### (3) Estimate the learning curve
Synthetic datasets of increasing size are generated.
For each sample size:
- Fit the model on training data.
- Evaluate performance on independent test data.
- Repeat across many simulated datasets.

To reduce computation, pmsims uses **Gaussian Process (GP) surrogate modelling** (via `mlpwr`)
to interpolate smooth learning curves from a limited number of simulation points,
focusing computational effort where the solution is likely to lie.

### (4) Determine the minimum sample size
The learning curve is used to identify the smallest `n` where the chosen quantile
(e.g. 20th percentile) of performance exceeds the target value.
This gives the required training size for which the model is expected
to achieve adequate performance in at least 80% of cases.

# 3. Interpretation

Two complementary criteria can be applied:

- **Mean-based** — target achieved *on average* across replications
- **Assurance-based** — target achieved in *most* replications (e.g., 80%)

Assurance is typically preferred for study planning,
since it accounts for model instability and data variability.

---

# 4. Why simulations?

Simulations capture multiple sources of randomness:

| Source | Description | Mitigated by increasing *n*? |
|--------|--------------|------------------------------|
| Irreducible outcome error | Intrinsic variability in outcomes | ✖ |
| Training data variability | Different samples yield different models | ✔ |
| Model-fitting randomness | Stochastic optimisation paths (e.g. ML) | Partial |
| Validation data variability | Finite validation sample | Related problem |

---

# 5. Flexibility and extensibility

Because **pmsims** separates *data generation*, *model fitting*, and *performance assessment*,
it can be adapted to:
- regression or ML models
- binary, continuous, or survival outcomes
- user-defined metrics
- synthetic or pilot-based data generation

Planned extensions include:
- hierarchical or longitudinal data structures
- missing data mechanisms
- fairness and subgroup stability analyses

---

# References

For a detailed methodological background, see:

Shamsutdinova D et al. (2025). *Sample Size Calculations for Clinical Prediction Modelling: Conceptualisation and Simulation-Based Approach.* King’s College London, Department of Biostatistics & Health Informatics.
