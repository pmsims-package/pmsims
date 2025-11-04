---
title: "pmsims"
---

<img src="man/figures/logo.png" align="right" height="110" />

**pmsims** is an R package for estimating how much data are needed to develop
reliable and generalisable prediction models. It uses a **simulation-based
learning curve** approach to quantify how model performance improves with
increasing sample size, supporting principled study planning and feasibility
assessment.

The package is fully model-agnostic: users can define how data are generated,
how models are fitted, and how predictive performance is measured. It currently
supports regression-based prediction models with continuous, binary, and
time-to-event outcomes.

**Developed at** [King’s College London](https://www.kcl.ac.uk/) (Department of
Biostatistics & Health Informatics)  with input from researchers, clinicians,
and patient partners.  See also the [pmsims project
site](https://pmsims-package.github.io/pmsims-website/).

---


## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("pmsims-package/pmsims")
```

## Minimal example

```r
library(pmsims)

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
