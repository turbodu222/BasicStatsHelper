# BasicStatsHelper

<!-- badges: start -->
<!-- badges: end -->

BasicStatsHelper provides easy-to-use functions for common statistical analyses including descriptive statistics, confidence intervals, hypothesis testing, and data visualization helpers. Designed for students and researchers who need quick and reliable statistical computations with clear interpretations.

## Purpose

This is a final project package for UW BIOST 561 (Spring 2025), tutored by Kevin Lin.

## Installation

You can install the development version of BasicStatsHelper from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("turbodu222/BasicStatsHelper")
```

## Quick Start

```r
library(BasicStatsHelper)

# Generate synthetic data for testing
data <- generate_synthetic_data(n = 100, type = "normal", seed = 123)

# Calculate descriptive statistics
stats <- descriptive_stats(data$data)
print(stats)

# Calculate confidence interval
ci <- confidence_interval(data$data)
cat("95% CI: [", round(ci$lower, 2), ",", round(ci$upper, 2), "]\n")

# Perform t-test
ttest <- simple_ttest(data$data, mu = 50)
cat(ttest$interpretation)

# Create publication-ready histogram
plot_histogram(data$data, title = "Sample Distribution", add_stats = TRUE)
```

## Main Functions

- **`descriptive_stats()`**: Calculate comprehensive descriptive statistics including mean, median, standard deviation, quartiles, and more for any numeric vector.

- **`confidence_interval()`**: Compute confidence intervals for population means using appropriate t-distribution or normal distribution methods.

- **`simple_ttest()`**: Perform one-sample or two-sample t-tests with clear, easy-to-understand interpretations in plain English.

- **`plot_histogram()`**: Create publication-ready histograms with optional normal curve overlays and statistical summaries.

- **`generate_synthetic_data()`**: Generate synthetic datasets with various distributions (normal, uniform, skewed, bimodal) for testing and educational purposes.

## Features

✅ **Beginner-friendly**: Functions designed with clear parameter names and helpful error messages  
✅ **Educational focus**: Results include plain-English interpretations of statistical tests  
✅ **Publication-ready plots**: Clean, professional visualizations suitable for reports and presentations  
✅ **Flexible data generation**: Multiple distribution types for simulation and testing  
✅ **Comprehensive documentation**: Detailed help files with practical examples for every function

## Example Workflow

```r
library(BasicStatsHelper)

# Step 1: Generate or load your data
data <- generate_synthetic_data(n = 200, type = "normal", seed = 123)

# Step 2: Explore your data
stats <- descriptive_stats(data$data)
print(stats)

# Step 3: Calculate confidence intervals
ci <- confidence_interval(data$data, conf.level = 0.95)
cat("95% CI:", round(ci$lower, 2), "to", round(ci$upper, 2))

# Step 4: Perform hypothesis testing
result <- simple_ttest(data$data, mu = 50)
cat(result$interpretation)

# Step 5: Visualize your results
plot_histogram(data$data, title = "My Data Distribution", add_stats = TRUE)
```

## Dependencies

The package depends on the following packages: `ggplot2` and `stats` (base R).

## Session Info

This package was developed in the following environment:

```r
R version 4.4.0 (2024-04-24)
Platform: x86_64-apple-darwin20
Running under: macOS

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRblas.0.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRlapack.dylib

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] ggplot2_3.5.1   devtools_2.4.5   usethis_2.2.2    pkgdown_2.0.7
```

## Links

- **GitHub Repository**: https://github.com/turbodu222/BasicStatsHelper
- **Documentation Website**: https://turbodu222.github.io/BasicStatsHelper/
- **Report Issues**: https://github.com/turbodu222/BasicStatsHelper/issues
