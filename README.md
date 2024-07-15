# Calculation of Sample Size for Diagnostic Studies

(Based on the Paper by Buderer - Pubmed ID 8870764)

## Overview

This repository provides tools for calculating the sample size required for diagnostic studies, based on the methodology described in the paper by Buderer. The paper incorporates the prevalence of disease into the sample size calculation for sensitivity and specificity.

### Link to Paper

[Statistical Methodology: I. Incorporating the Prevalence of Disease into the Sample Size Calculation for Sensitivity and Specificity](https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1553-2712.1996.tb03538.x)

## Installation

To use the `SampleSizeDiagnostics` package, you can install it from GitHub using the `devtools` package:

```r
library(devtools)
install_github("statpharm/SampleSizeDiagnostics")
library(SampleSizeDiagnostics)
```
## Usage

To understand how to use the package and its functions, you can access the help documentation:

```r
?calculate_sample_size
```
## Example

Here is an example of how to calculate the sample size with given parameters:
This function call will calculate the required sample size for a diagnostic study with:

    Sensitivity (sn) of 0.9
    Specificity (sp) of 0.85
    Prevalence (p) of 0.2
    Width of the confidence interval (w) of 0.1
    Confidence level (CI) of 0.95

```r
calculate_sample_size(sn = 0.9,
                      sp = 0.85,
                      p = 0.2,
                      w = 0.1,
                      CI = 0.95)
```
