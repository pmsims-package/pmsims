# pmsims <img src="man/figures/logo.png" align="right" height="110" />

**pmsims** is an R package for estimating how much data are needed to develop reliable and generalisable prediction models. It uses flexible, simulation-based methods to examine how performance changes with sample size via repeated data generation, model fitting, and validation.

The aim of **pmsims** is to support study planning and feasibility assessment by quantifying the sample sizes required to achieve adequate discrimination, calibration, and parameter precision. The current version focuses on regression-based prediction models with continuous, binary, and time-to-event outcomes.

**Authors & contributors.** The package is developed at [King’s College London](https://www.kcl.ac.uk/) (Department of Biostatistics & Health Informatics) with input from researchers, clinicians, and patient partners.

To learn more about the wider project, visit the **[pmsims project website](https://pmsims-package.github.io/pmsims-website/)**.

---

## Getting started

### Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("pmsims-package/pmsims")
```

### Minimal example

```r
library(pmsims)

# Explore performance across sample sizes for a binary outcome using logistic regression
set.seed(123)

print(res)
plot(res)
```

---

## Get in touch

We welcome questions, suggestions, and collaboration enquiries.

<p align="center">
  <a href="mailto:pmsims@kcl.ac.uk?subject=pmsims%20enquiry">
    <img src="https://img.shields.io/badge/Email%20the%20team-pmsims@kcl.ac.uk-003366?style=for-the-badge" alt="Email the pmsims team" />
  </a>
</p>

---

## Funding

This work is supported by the **National Institute for Health and Care Research (NIHR)** under the **Research for Patient Benefit (RfPB)** Programme.

*The views expressed are those of the authors and not necessarily those of the NIHR or the Department of Health and Social Care.*
