# Defining custom simulation workflows

## Motivation

The
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_custom.md)
function allows users to define their own data generation, model
fitting, and performance metric functions. This provides flexibility to
calculate sample sizes for any prediction model, data type or
performance metric. The `pmsims` built-in sample size functions—such as
[`simulate_binary()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_binary.md)—use
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_custom.md)
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
[`simulate_binary()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_binary.md)),
this is provided directly by the user through arguments such as
`maximum_achievable_cstatistic`. In
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_custom.md),
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

## Running `simulate_custom()`

We are now ready to run
[`simulate_custom()`](https://pmsims-package.github.io/pmsims/dev/reference/simulate_custom.md).
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

    ## Starting values determined: min sample size = 80 max sample size = 160 
    ## Estimating second stage... (Gaussian process algorithm)

``` r

print(result)
```

    ## $outcome
    ## NULL
    ## 
    ## $min_n
    ## [1] 160
    ## 
    ## $perf_n
    ## [1] -0.1683316
    ## 
    ## $target_performance
    ## [1] -0.165
    ## 
    ## $summaries
    ## $summaries$mean_performance
    ##         80        100        120        157        160 
    ## -0.1757883 -0.1658685 -0.1675615 -0.1655770 -0.1643640 
    ## 
    ## $summaries$median_performance
    ##         80        100        120        157        160 
    ## -0.1729307 -0.1649489 -0.1671078 -0.1646063 -0.1635168 
    ## 
    ## $summaries$quant20_performance
    ##         80        100        120        157        160 
    ## -0.1849510 -0.1724087 -0.1711760 -0.1705800 -0.1683034 
    ## 
    ## $summaries$quant5_performance
    ##         80        100        120        157        160 
    ## -0.1918734 -0.1744730 -0.1814717 -0.1758953 -0.1740579 
    ## 
    ## $summaries$quant95_performance
    ##         80        100        120        157        160 
    ## -0.1636877 -0.1580924 -0.1588048 -0.1567179 -0.1575743 
    ## 
    ## 
    ## $data
    ## $data[[1]]
    ## $data[[1]]$x
    ##  n 
    ## 80 
    ## 
    ## $data[[1]]$y
    ##  [1] -0.1746989 -0.1915031 -0.1790800 -0.1816064 -0.1773924 -0.1847415
    ##  [7] -0.1857888 -0.1686836 -0.1691788 -0.1673933 -0.1708242 -0.1989081
    ## [13] -0.1626372 -0.1860213 -0.1782606 -0.1673964 -0.1711625 -0.1682090
    ## [19] -0.1685373 -0.1637430
    ## 
    ## 
    ## $data[[2]]
    ## $data[[2]]$x
    ##   n 
    ## 100 
    ## 
    ## $data[[2]]$y
    ##  [1] -0.1586377 -0.1669267 -0.1700087 -0.1581235 -0.1634470 -0.1725119
    ##  [7] -0.1759373 -0.1622515 -0.1621641 -0.1664508 -0.1605567 -0.1687299
    ## [13] -0.1628211 -0.1723829 -0.1743960 -0.1605861 -0.1575019 -0.1624994
    ## [19] -0.1725175 -0.1689199
    ## 
    ## 
    ## $data[[3]]
    ## $data[[3]]$x
    ##   n 
    ## 120 
    ## 
    ## $data[[3]]$y
    ##  [1] -0.1588551 -0.1685841 -0.1769639 -0.1831036 -0.1629972 -0.1813858
    ##  [7] -0.1662528 -0.1626839 -0.1630504 -0.1578488 -0.1689875 -0.1595639
    ## [13] -0.1690430 -0.1675963 -0.1740841 -0.1666193 -0.1686608 -0.1629020
    ## [19] -0.1704489 -0.1615978
    ## 
    ## 
    ## $data[[4]]
    ## $data[[4]]$x
    ## [1] 157
    ## 
    ## $data[[4]]$y
    ##  [1] -0.1727363 -0.1700410 -0.1758001 -0.1585055 -0.1610564 -0.1675709
    ##  [7] -0.1690670 -0.1621405 -0.1615839 -0.1648514 -0.1643613 -0.1569081
    ## [13] -0.1655090 -0.1759801 -0.1531044 -0.1639373 -0.1643459 -0.1613655
    ## [19] -0.1667841 -0.1758909
    ## 
    ## 
    ## $data[[5]]
    ## $data[[5]]$x
    ##   n 
    ## 160 
    ## 
    ## $data[[5]]$y
    ##   [1] -0.1569898 -0.1561873 -0.1590246 -0.1691213 -0.1573674 -0.1654238
    ##   [7] -0.1652673 -0.1677501 -0.1581986 -0.1596122 -0.1598439 -0.1642123
    ##  [13] -0.1693042 -0.1615880 -0.1625475 -0.1636899 -0.1656384 -0.1584617
    ##  [19] -0.1637352 -0.1588898 -0.1653286 -0.1574575 -0.1651047 -0.1615496
    ##  [25] -0.1601266 -0.1611985 -0.1618618 -0.1614127 -0.1578215 -0.1642901
    ##  [31] -0.1619329 -0.1703987 -0.1665790 -0.1630558 -0.1670033 -0.1700866
    ##  [37] -0.1703093 -0.1608535 -0.1605047 -0.1663385 -0.1775812 -0.1619951
    ##  [43] -0.1594991 -0.1619085 -0.1662803 -0.1647865 -0.1609146 -0.1607048
    ##  [49] -0.1779177 -0.1595021 -0.1576179 -0.1798751 -0.1652691 -0.1638852
    ##  [55] -0.1555077 -0.1644829 -0.1672254 -0.1697353 -0.1666188 -0.1722243
    ##  [61] -0.1700038 -0.1633855 -0.1608465 -0.1639328 -0.1600320 -0.1687946
    ##  [67] -0.1636024 -0.1672779 -0.1611403 -0.1629789 -0.1633387 -0.1611742
    ##  [73] -0.1589683 -0.1646928 -0.1684095 -0.1649737 -0.1622729 -0.1678734
    ##  [79] -0.1710951 -0.1617287 -0.1613346 -0.1729506 -0.1643289 -0.1747596
    ##  [85] -0.1613908 -0.1610798 -0.1643501 -0.1579094 -0.1597380 -0.1622023
    ##  [91] -0.1613469 -0.1617042 -0.1627329 -0.1673660 -0.1579375 -0.1622066
    ##  [97] -0.1724254 -0.1695565 -0.1667367 -0.1635490 -0.1709157 -0.1648363
    ## [103] -0.1662045 -0.1622139 -0.1694082 -0.1575772 -0.1611019 -0.1838311
    ## [109] -0.1595816 -0.1655736 -0.1659322 -0.1787999 -0.1701229 -0.1570212
    ## [115] -0.1649615 -0.1687611 -0.1636684 -0.1659492 -0.1668451 -0.1599705
    ## [121] -0.1604834 -0.1608116 -0.1749155 -0.1612566 -0.1628366 -0.1679690
    ## [127] -0.1603620 -0.1529488 -0.1633715 -0.1651877 -0.1583639 -0.1711522
    ## [133] -0.1665635 -0.1617230 -0.1593414 -0.1580473 -0.1668938 -0.1672577
    ## [139] -0.1698984 -0.1600304 -0.1587367 -0.1671826 -0.1663300 -0.1574150
    ## [145] -0.1666625 -0.1685871 -0.1708276 -0.1697604 -0.1644528 -0.1639322
    ## [151] -0.1683335 -0.1601211 -0.1637381 -0.1665684 -0.1767633 -0.1597445
    ## [157] -0.1742463 -0.1585641 -0.1629152 -0.1663908 -0.1659637 -0.1594431
    ## [163] -0.1713756 -0.1612211 -0.1714590 -0.1695996 -0.1675398 -0.1626926
    ## [169] -0.1609962 -0.1577944 -0.1751143 -0.1618135 -0.1587640 -0.1642636
    ## [175] -0.1599139 -0.1597546 -0.1645122 -0.1650274 -0.1602257 -0.1659615
    ## [181] -0.1613956 -0.1617640 -0.1629317 -0.1561589 -0.1592436 -0.1618792
    ## [187] -0.1673505 -0.1670192 -0.1678962 -0.1593095 -0.1651685 -0.1678042
    ## [193] -0.1687900 -0.1745247 -0.1625840 -0.1641373 -0.1636927 -0.1607307
    ## [199] -0.1669302 -0.1628074 -0.1646193 -0.1592497 -0.1631595 -0.1627116
    ## [205] -0.1610640 -0.1658624 -0.1652053 -0.1633243 -0.1704255 -0.1615253
    ## [211] -0.1640213 -0.1689633 -0.1630276 -0.1809382 -0.1569901 -0.1629355
    ## [217] -0.1613407 -0.1629737 -0.1630368 -0.1740326 -0.1618662 -0.1601501
    ## [223] -0.1669797 -0.1772354 -0.1728522 -0.1578734 -0.1586439 -0.1689672
    ## [229] -0.1651793 -0.1646341 -0.1630454 -0.1709851 -0.1602851 -0.1628342
    ## [235] -0.1674107 -0.1748277 -0.1618105 -0.1671758 -0.1579407 -0.1647172
    ## [241] -0.1586048 -0.1634898 -0.1635863 -0.1625552 -0.1656433 -0.1671495
    ## [247] -0.1623189 -0.1783502 -0.1623032 -0.1665581 -0.1728933 -0.1745728
    ## [253] -0.1622942 -0.1570019 -0.1643849 -0.1665972 -0.1608155 -0.1623797
    ## [259] -0.1601232 -0.1631148 -0.1630951 -0.1585402 -0.1630445 -0.1643770
    ## [265] -0.1667247 -0.1602282 -0.1620216 -0.1592809 -0.1640728 -0.1687448
    ## [271] -0.1661235 -0.1631175 -0.1639352 -0.1726687 -0.1686894 -0.1658970
    ## [277] -0.1647634 -0.1585725 -0.1595883 -0.1640317 -0.1584855 -0.1586475
    ## [283] -0.1631850 -0.1740875 -0.1628565 -0.1627012 -0.1629519 -0.1669497
    ## [289] -0.1577800 -0.1655022 -0.1695882 -0.1587637 -0.1636874 -0.1603021
    ## [295] -0.1644878 -0.1635476 -0.1604845 -0.1623119 -0.1631045 -0.1649219
    ## [301] -0.1702249 -0.1614611 -0.1597017 -0.1678028 -0.1757256 -0.1609203
    ## [307] -0.1670737 -0.1612519 -0.1639096 -0.1621930 -0.1706558 -0.1613604
    ## [313] -0.1606616 -0.1578539 -0.1588548 -0.1721493 -0.1646898 -0.1622536
    ## [319] -0.1631792 -0.1649099 -0.1741475 -0.1768872 -0.1672581 -0.1592456
    ## [325] -0.1614652 -0.1546799 -0.1643272 -0.1670150 -0.1597378 -0.1632108
    ## [331] -0.1591710 -0.1685185 -0.1597015 -0.1604099 -0.1630235 -0.1595441
    ## [337] -0.1656225 -0.1703067 -0.1614438 -0.1629850 -0.1777475 -0.1682243
    ## [343] -0.1613167 -0.1637837 -0.1631721 -0.1620815 -0.1581887 -0.1772154
    ## [349] -0.1593070 -0.1631568 -0.1637739 -0.1612348 -0.1686667 -0.1661385
    ## [355] -0.1664277 -0.1638616 -0.1642674 -0.1676359 -0.1592917 -0.1629643
    ## [361] -0.1710245 -0.1699370 -0.1615627 -0.1613868 -0.1624125 -0.1659448
    ## [367] -0.1609207 -0.1669209 -0.1704354 -0.1649552 -0.1581461 -0.1615865
    ## [373] -0.1602089 -0.1633216 -0.1624946 -0.1694610 -0.1672762 -0.1685570
    ## [379] -0.1719328 -0.1662035 -0.1667988 -0.1688449 -0.1620252 -0.1615805
    ## [385] -0.1594160 -0.1726905 -0.1631091 -0.1590529 -0.1580608 -0.1728457
    ## [391] -0.1635930 -0.1627505 -0.1646514 -0.1597283 -0.1558983 -0.1700562
    ## [397] -0.1666797 -0.1659961 -0.1633962 -0.1652261 -0.1685371 -0.1653190
    ## [403] -0.1648911 -0.1701314 -0.1731976 -0.1697420 -0.1618842 -0.1584191
    ## [409] -0.1538398 -0.1756468 -0.1632090 -0.1626162 -0.1598444 -0.1544593
    ## [415] -0.1595907 -0.1687128 -0.1724551 -0.1612109 -0.1664527 -0.1685109
    ## [421] -0.1763808 -0.1674575 -0.1584054 -0.1593640 -0.1663901 -0.1654623
    ## [427] -0.1661163 -0.1581498 -0.1620581 -0.1610278 -0.1685201 -0.1625292
    ## [433] -0.1726173 -0.1580588 -0.1580655 -0.1607601 -0.1681306 -0.1597753
    ## [439] -0.1575058 -0.1658488 -0.1612100 -0.1605348 -0.1710036 -0.1597009
    ## [445] -0.1613720 -0.1674850 -0.1657233 -0.1702344 -0.1676189 -0.1586186
    ## [451] -0.1740519 -0.1741307 -0.1574322 -0.1635672 -0.1698859 -0.1649493
    ## [457] -0.1570308 -0.1646138 -0.1602689 -0.1675173 -0.1669845 -0.1703853
    ## [463] -0.1634364 -0.1783094 -0.1673620 -0.1646599 -0.1602783 -0.1612868
    ## [469] -0.1644407 -0.1602853 -0.1612431 -0.1575895 -0.1634717 -0.1611798
    ## [475] -0.1683227 -0.1598957 -0.1612608 -0.1639285 -0.1677974 -0.1732667
    ## [481] -0.1662047 -0.1633090 -0.1688050 -0.1610913 -0.1610603 -0.1661673
    ## [487] -0.1737125 -0.1680456 -0.1712294 -0.1666261 -0.1676199 -0.1736603
    ## [493] -0.1592499 -0.1629222 -0.1672835 -0.1783187 -0.1613505 -0.1622610
    ## [499] -0.1654053 -0.1640458 -0.1686737 -0.1665837 -0.1594049 -0.1618631
    ## [505] -0.1647586 -0.1664124 -0.1665063 -0.1630435 -0.1677566 -0.1669026
    ## [511] -0.1645304 -0.1608056 -0.1648051 -0.1693932 -0.1640679 -0.1646905
    ## [517] -0.1606505 -0.1780878 -0.1626487 -0.1632722 -0.1606814 -0.1623426
    ## [523] -0.1682986 -0.1634662 -0.1615138 -0.1573763 -0.1714037 -0.1659614
    ## [529] -0.1670309 -0.1627420 -0.1635771 -0.1564145 -0.1590600 -0.1589470
    ## [535] -0.1608851 -0.1655126 -0.1622083 -0.1634535 -0.1624492 -0.1599006
    ## [541] -0.1594629 -0.1563851 -0.1615777 -0.1660143 -0.1715691 -0.1763407
    ## [547] -0.1720630 -0.1782644 -0.1622416 -0.1588973 -0.1607167 -0.1628601
    ## [553] -0.1576522 -0.1666068 -0.1620127 -0.1580301 -0.1728135 -0.1570430
    ## [559] -0.1626936 -0.1679960 -0.1669632 -0.1726352 -0.1717485 -0.1752793
    ## [565] -0.1699625 -0.1602582 -0.1661881 -0.1627286 -0.1601741 -0.1614062
    ## [571] -0.1750313 -0.1598222 -0.1613025 -0.1619718 -0.1626189 -0.1576303
    ## [577] -0.1688660 -0.1673790 -0.1645576 -0.1622668 -0.1663663 -0.1580411
    ## [583] -0.1625772 -0.1559043 -0.1649080 -0.1617780 -0.1751402 -0.1564691
    ## [589] -0.1640683 -0.1600397 -0.1585271 -0.1579822 -0.1574101 -0.1737104
    ## [595] -0.1590628 -0.1594297 -0.1609713 -0.1640418 -0.1684284 -0.1595085
    ## [601] -0.1586302 -0.1598085 -0.1589770 -0.1604830 -0.1731192 -0.1647546
    ## [607] -0.1727378 -0.1703084 -0.1598090 -0.1651568 -0.1609130 -0.1666989
    ## [613] -0.1636513 -0.1584800 -0.1622897 -0.1648028 -0.1644018 -0.1780612
    ## [619] -0.1626045 -0.1644646 -0.1730081 -0.1601921 -0.1605198 -0.1677724
    ## [625] -0.1635779 -0.1622999 -0.1687358 -0.1701145 -0.1652937 -0.1710531
    ## [631] -0.1559573 -0.1715238 -0.1624851 -0.1601656 -0.1636731 -0.1602503
    ## [637] -0.1790167 -0.1654532 -0.1586458 -0.1633126 -0.1672446 -0.1567060
    ## [643] -0.1634963 -0.1721491 -0.1666923 -0.1610956 -0.1604197 -0.1682974
    ## [649] -0.1585011 -0.1797492 -0.1598512 -0.1616720 -0.1563673 -0.1709200
    ## [655] -0.1723085 -0.1664077 -0.1572872 -0.1609825 -0.1644872 -0.1672774
    ## [661] -0.1685978 -0.1598006 -0.1644669 -0.1725293 -0.1730519 -0.1617585
    ## [667] -0.1639097 -0.1632898 -0.1634004 -0.1697287 -0.1607636 -0.1560354
    ## [673] -0.1693454 -0.1616586 -0.1657419 -0.1717072 -0.1726432 -0.1649648
    ## [679] -0.1627192 -0.1661475 -0.1591407 -0.1585240 -0.1599183 -0.1664798
    ## [685] -0.1647565 -0.1725599 -0.1626196 -0.1632609 -0.1781428 -0.1634535
    ## [691] -0.1598008 -0.1623894 -0.1615322 -0.1609060 -0.1646607 -0.1551201
    ## [697] -0.1569319 -0.1565954 -0.1543147 -0.1725252 -0.1702371 -0.1584508
    ## [703] -0.1627309 -0.1562200 -0.1622659 -0.1607382 -0.1671729 -0.1697811
    ## [709] -0.1739395 -0.1609584 -0.1643933 -0.1634917 -0.1581966 -0.1744508
    ## [715] -0.1589886 -0.1668751 -0.1632484 -0.1651441 -0.1688708 -0.1617356
    ## [721] -0.1637763 -0.1611930 -0.1678331 -0.1615345 -0.1582634 -0.1612729
    ## [727] -0.1584834 -0.1641753 -0.1619610 -0.1642107 -0.1756353 -0.1692022
    ## [733] -0.1635372 -0.1597175 -0.1619111 -0.1708682 -0.1882017 -0.1645431
    ## [739] -0.1613671 -0.1774480 -0.1587702 -0.1626106 -0.1588655 -0.1629028
    ## [745] -0.1575195 -0.1657090 -0.1603545 -0.1652500 -0.1658397 -0.1647260
    ## [751] -0.1601976 -0.1612027 -0.1605160 -0.1556303 -0.1622995 -0.1641118
    ## [757] -0.1746611 -0.1636186 -0.1623099 -0.1628116 -0.1670995 -0.1611027
    ## [763] -0.1606749 -0.1649243 -0.1711858 -0.1644122 -0.1573053 -0.1679549
    ## [769] -0.1598727 -0.1673756 -0.1657454 -0.1663886 -0.1596792 -0.1608610
    ## [775] -0.1676574 -0.1689547 -0.1605298 -0.1724946 -0.1588036 -0.1657906
    ## [781] -0.1585618 -0.1572220 -0.1588326 -0.1639748 -0.1578058 -0.1592169
    ## [787] -0.1743746 -0.1664716 -0.1679198 -0.1613416 -0.1656413 -0.1642671
    ## [793] -0.1639913 -0.1628511 -0.1644966 -0.1606425 -0.1591581 -0.1564711
    ## [799] -0.1618379 -0.1633184 -0.1628450 -0.1654089 -0.1595254 -0.1734379
    ## [805] -0.1703753 -0.1643516 -0.1637071 -0.1683392 -0.1576685 -0.1585435
    ## [811] -0.1583711 -0.1604604 -0.1691164 -0.1587333 -0.1628328 -0.1675976
    ## [817] -0.1639900 -0.1648680 -0.1665872 -0.1627297 -0.1616093 -0.1651918
    ## [823] -0.1632380 -0.1611460 -0.1751828 -0.1719342 -0.1657405 -0.1650963
    ## [829] -0.1606902 -0.1562563 -0.1705173 -0.1608361 -0.1666180 -0.1678348
    ## [835] -0.1712963 -0.1679802 -0.1639242 -0.1703920 -0.1766520 -0.1653032
    ## [841] -0.1648306 -0.1650838 -0.1701649 -0.1724725 -0.1658067 -0.1644934
    ## [847] -0.1711244 -0.1649743 -0.1713318 -0.1675743 -0.1655867 -0.1645595
    ## [853] -0.1637371 -0.1562289 -0.1694038 -0.1619212 -0.1740563 -0.1692149
    ## [859] -0.1652699 -0.1591925 -0.1615343 -0.1633655 -0.1562392 -0.1598876
    ## [865] -0.1612541 -0.1634596 -0.1646952 -0.1689997 -0.1686873 -0.1575077
    ## [871] -0.1663738 -0.1615801 -0.1605722 -0.1669174 -0.1616481 -0.1594022
    ## [877] -0.1552481 -0.1628449 -0.1592056 -0.1711951 -0.1634692 -0.1631565
    ## [883] -0.1670667 -0.1624359 -0.1607658 -0.1644825 -0.1628166 -0.1687005
    ## [889] -0.1596567 -0.1590523 -0.1640503 -0.1625341 -0.1608277 -0.1572006
    ## [895] -0.1664756 -0.1614422 -0.1623203 -0.1593128 -0.1614239 -0.1627786
    ## [901] -0.1579758 -0.1589249 -0.1589118 -0.1690161 -0.1631657 -0.1579732
    ## [907] -0.1618378 -0.1712687 -0.1637564 -0.1620064 -0.1596586 -0.1611242
    ## [913] -0.1701250 -0.1664440 -0.1678676 -0.1649466 -0.1666680 -0.1613441
    ## [919] -0.1767452 -0.1652189
    ## 
    ## 
    ## 
    ## $data_function
    ## function (n, data = synthetic_data$syn) 
    ## {
    ##     data[sample(seq_len(nrow(data)), n, replace = FALSE), ]
    ## }
    ## <bytecode: 0x557a5b1872f8>
    ## 
    ## $model_function
    ## function (data) 
    ## {
    ##     data_matrix <- as.matrix(data)
    ##     outcome <- "diabetes"
    ##     x <- data_matrix[, colnames(data_matrix) != outcome, drop = FALSE]
    ##     y <- data_matrix[, outcome]
    ##     glmnet::cv.glmnet(x, y, family = "binomial", alpha = 0.5, 
    ##         nfolds = 10)
    ## }
    ## <bytecode: 0x557a28c69e48>
    ## 
    ## $metric_function
    ## function (test_data, fitted_model, model_name) 
    ## {
    ##     test_data_matrix <- as.matrix(test_data)
    ##     y <- which(names(test_data) == "diabetes")
    ##     x_test <- test_data_matrix[, -y]
    ##     y_test <- test_data_matrix[, y]
    ##     predictions <- predict(fitted_model, newx = x_test, s = "lambda.min", 
    ##         type = "response")
    ##     brier_score <- DescTools::BrierScore(y_test, pred = predictions)
    ##     return(-brier_score)
    ## }
    ## <bytecode: 0x557a4cd5ce18>
    ## 
    ## $model
    ## NULL
    ## 
    ## $metric
    ## NULL
    ## 
    ## $c_statistic
    ## NULL
    ## 
    ## $test_n
    ## [1] 30000
    ## 
    ## $min_sample_size
    ## NULL
    ## 
    ## $max_sample_size
    ## NULL
    ## 
    ## $n_reps_total
    ## [1] 1000
    ## 
    ## $n_reps_per
    ## [1] 20
    ## 
    ## $method
    ## [1] "mlpwr"
    ## 
    ## $progress
    ## [1] FALSE
    ## 
    ## $verbose
    ## [1] FALSE
    ## 
    ## $simulation_time
    ## Time difference of 102.7293 secs
    ## 
    ## $mean_or_assurance
    ## [1] "assurance"

### Interpretation

The results show a minimum sample size of 160. This is calculated using
the assurance criterion, which means that we would expect 80% of models
developed on samples of this size to have a negative Brier score of
-0.165 or better.

Note that this is the minimum sample size required for the Brier score;
other performance metrics may require larger sample sizes to achieve
adequate performance.
