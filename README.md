Calculation of Sample Size for Diagnostic Studies. (Based on the Paper by Buderer - Pubmed ID 8870764)

Link to paper "Statistical Methodology: I. Incorporating the Prevalence of Disease into the Sample Size Calculation for Sensitivity and Specificity"

onlinelibrary.wiley.com/doi/epdf/10.1111/j.1553-2712.1996.tb03538.x

library(devtools)
install_github("statpharm/SampleSizeDiagnostics")
library(SampleSizeDiagnostics)
?calculate_sample_size()

Example:
  
calculate_sample_size(sn =0.9,
                      sp = 0.85, p = 0.2,
                      w = 0.1,CI = 0.95)
